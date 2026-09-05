"""
Bexar County Criminal Court Data — Merge, Clean, and Export
Run from the directory containing the CSV files.
Outputs: bexar_cleaned.parquet, bexar_panel_1990_2021.parquet, bexar_cleaned.csv
"""

import re
import glob
import pandas as pd
import numpy as np
from pathlib import Path

DATA_DIR = Path(".")  # adjust if CSVs are in a subdirectory


# ── 1. Load and merge ─────────────────────────────────────────────────────────

def load_csvs(data_dir: Path) -> pd.DataFrame:
    files = sorted(glob.glob(str(data_dir / "DC_cjjorad_*.csv")))
    if not files:
        raise FileNotFoundError(f"No DC_cjjorad_*.csv files found in {data_dir}")
    print(f"Found {len(files)} CSV files: {[Path(f).name for f in files]}")

    frames = []
    reference_cols = None
    for f in files:
        df = pd.read_csv(f, dtype=str, low_memory=False)
        df = df.drop(columns=[c for c in df.columns if c.startswith("Unnamed:")], errors="ignore")
        if reference_cols is None:
            reference_cols = list(df.columns)
        else:
            if list(df.columns) != reference_cols:
                extra = set(df.columns) - set(reference_cols)
                missing = set(reference_cols) - set(df.columns)
                print(f"  WARNING schema drift in {Path(f).name}: extra={extra}, missing={missing}")
        frames.append(df)
        print(f"  Loaded {Path(f).name}: {len(df):,} rows")

    merged = pd.concat(frames, ignore_index=True)
    print(f"\nMerged total: {len(merged):,} rows, {len(merged.columns)} columns")
    return merged


def check_duplicates(df: pd.DataFrame) -> pd.DataFrame:
    key = "CASE-CAUSE-NBR"
    dups = df[df.duplicated(key, keep=False)]
    if len(dups):
        print(f"WARNING: {len(dups):,} rows share duplicate {key}. Keeping first occurrence.")
        df = df.drop_duplicates(key, keep="first")
    else:
        print(f"No duplicate {key} values — clean primary key.")
    return df


# ── 2. Strip whitespace ───────────────────────────────────────────────────────

def strip_strings(df: pd.DataFrame) -> pd.DataFrame:
    str_cols = df.select_dtypes("object").columns
    df[str_cols] = df[str_cols].apply(lambda s: s.str.strip())
    return df


# ── 3. Parse dates ────────────────────────────────────────────────────────────

DATE_COLS = ["CASE-DATE", "OFFENSE-DATE", "DISPOSITION-DATE",
             "CUSTODY-DATE", "SENTENCE-START-DATE", "SENTENCE-END-DATE", "BIRTHDATE"]

def parse_dates(df: pd.DataFrame) -> pd.DataFrame:
    for col in DATE_COLS:
        if col in df.columns:
            df[col] = pd.to_datetime(df[col], errors="coerce", infer_datetime_format=True)
    df["CASE-YEAR"] = df["CASE-DATE"].dt.year
    df["CASE-DECADE"] = (df["CASE-YEAR"] // 10 * 10).astype("Int64")
    df["DAYS-TO-CASE"] = (df["CASE-DATE"] - df["OFFENSE-DATE"]).dt.days
    return df


# ── 4. Recode categorical variables ──────────────────────────────────────────

def recode_race(df: pd.DataFrame) -> pd.DataFrame:
    mapping = {"B": "Black", "W": "White", "L": "Latino"}
    df["RACE-LABEL"] = df["RACE"].map(mapping).fillna("Other/Unknown")
    return df


def recode_sex(df: pd.DataFrame) -> pd.DataFrame:
    df["SEX-LABEL"] = df["SEX"].map({"M": "M", "F": "F"}).fillna("Unknown")
    return df


def recode_attorney(df: pd.DataFrame) -> pd.DataFrame:
    col = "ATTORNEY-APPOINTED-RETAINED"
    if col not in df.columns:
        return df
    df["ATTORNEY-TYPE"] = df[col].map({
        "A": "Appointed",
        "H": "Hired",
        "S": "ProSe",
    }).fillna(pd.NA)  # blank → missing
    return df


def recode_offense_type(df: pd.DataFrame) -> pd.DataFrame:
    col = "OFFENSE-TYPE"
    if col not in df.columns:
        return df

    felony_map = {"F1": "F1", "F2": "F2", "F3": "F3", "FS": "FS"}
    misdemeanor = {"MA", "MB", "MC", "M "}

    def classify(v):
        if pd.isna(v) or v == "":
            return "Unknown"
        if v in felony_map:
            return felony_map[v]
        if v in misdemeanor:
            return "Misdemeanor"
        return "Other Felony"

    df["OFFENSE-CLASS"] = df[col].apply(classify)
    # ordinal severity for felonies (lower = more serious)
    severity = {"F1": 1, "F2": 2, "F3": 3, "FS": 4,
                "Other Felony": 5, "Misdemeanor": 6, "Unknown": np.nan}
    df["OFFENSE-SEVERITY"] = df["OFFENSE-CLASS"].map(severity)
    return df


# ── 5. Parse ORIGINAL-SENTENCE ────────────────────────────────────────────────

SENT_RE = re.compile(
    r"(\d+)\s*YR.*?(\d+)\s*MTH.*?(\d+)\s*DYS",
    re.IGNORECASE,
)

def parse_sentence(s: str) -> float | None:
    if pd.isna(s) or s.strip() == "":
        return np.nan
    m = SENT_RE.search(s)
    if not m:
        return np.nan
    yrs, mths, days = int(m.group(1)), int(m.group(2)), int(m.group(3))
    total = yrs * 365.25 + mths * 30.44 + days
    return total


def parse_sentences(df: pd.DataFrame) -> pd.DataFrame:
    col = "ORIGINAL-SENTENCE"
    if col not in df.columns:
        return df
    df["SENTENCE-DAYS"] = df[col].apply(parse_sentence)
    df["SENTENCE-ZERO"] = df["SENTENCE-DAYS"] == 0  # probation-only / deferred
    return df


# ── 6. Outcome indicator variables ───────────────────────────────────────────

def build_outcomes(df: pd.DataFrame) -> pd.DataFrame:
    disp = df.get("DISPOSITION-DESC", pd.Series("", index=df.index)).fillna("")
    judg = df.get("JUDGEMENT-DESC", pd.Series("", index=df.index)).fillna("")

    df["DEFERRED"]    = disp.str.contains("DEFR", case=False, na=False).astype(int)
    df["DISMISSED"]   = disp.str.contains("DSMD", case=False, na=False).astype(int)
    df["GUILTY-PLEA"] = disp.str.contains("CT-GUILTY", case=False, na=False).astype(int)
    df["JURY-TRIAL"]  = (
        disp.str.contains("PNG JRY", case=False, na=False) |
        judg.str.contains("JURY",    case=False, na=False)
    ).astype(int)
    df["PROBATION"]   = disp.str.contains("PROB", case=False, na=False).astype(int)
    df["CONVICTED"]   = (
        (df["GUILTY-PLEA"] == 1) |
        (df["JURY-TRIAL"]  == 1) |
        (~df["SENTENCE-DAYS"].isna() & (df["SENTENCE-DAYS"] > 0))
    ).astype(int)
    return df


# ── 7. Bond amount ────────────────────────────────────────────────────────────

def clean_bond(df: pd.DataFrame) -> pd.DataFrame:
    col = "BOND-AMOUNT"
    if col not in df.columns:
        return df
    df[col] = pd.to_numeric(df[col], errors="coerce")
    df["BOND-MISSING"] = (df[col].isna() | (df[col] == 0)).astype(int)
    df["BOND-OUTLIER"] = (df[col] > 1_000_000).astype(int)
    return df


# ── 8. Prosecutor names ───────────────────────────────────────────────────────

def clean_prosecutors(df: pd.DataFrame) -> pd.DataFrame:
    for col in ["INTAKE-PROSECUTOR", "OUTTAKE-PROSECUTOR"]:
        if col in df.columns:
            df[col] = df[col].str.upper().str.strip()
    # case counts per intake prosecutor
    if "INTAKE-PROSECUTOR" in df.columns:
        counts = df["INTAKE-PROSECUTOR"].value_counts()
        df["PROSECUTOR-CASE-COUNT"] = df["INTAKE-PROSECUTOR"].map(counts)
    return df


# ── 9. Main ───────────────────────────────────────────────────────────────────

def main():
    df = load_csvs(DATA_DIR)
    df = check_duplicates(df)
    df = strip_strings(df)
    df = parse_dates(df)
    df = recode_race(df)
    df = recode_sex(df)
    df = recode_attorney(df)
    df = recode_offense_type(df)
    df = parse_sentences(df)
    df = build_outcomes(df)
    df = clean_bond(df)
    df = clean_prosecutors(df)

    # Historical frame (pre-1990)
    df_historical = df[df["CASE-YEAR"] < 1990].copy()
    print(f"\nHistorical records (pre-1990): {len(df_historical):,}")

    # Main panel
    panel = df[(df["CASE-YEAR"] >= 1990) & (df["CASE-YEAR"] <= 2021)].copy()
    print(f"Main panel (1990–2021): {len(panel):,}")

    # Save full cleaned dataset
    df.to_parquet("bexar_cleaned.parquet", index=False)
    df.to_csv("bexar_cleaned.csv", index=False)
    print("\nSaved: bexar_cleaned.parquet, bexar_cleaned.csv")

    # Save main panel
    panel.to_parquet("bexar_panel_1990_2021.parquet", index=False)
    print("Saved: bexar_panel_1990_2021.parquet")

    # Quick sanity checks
    print("\n── Race distribution (panel) ──")
    print(panel["RACE-LABEL"].value_counts())
    print("\n── Attorney type (panel) ──")
    print(panel["ATTORNEY-TYPE"].value_counts(dropna=False))
    print("\n── Offense class (panel) ──")
    print(panel["OFFENSE-CLASS"].value_counts())
    print("\n── Outcome rates (panel, primary races) ──")
    primary = panel[panel["RACE-LABEL"].isin(["Black", "White", "Latino"])]
    for outcome in ["DEFERRED", "DISMISSED", "GUILTY-PLEA", "CONVICTED"]:
        rates = primary.groupby("RACE-LABEL")[outcome].mean().round(4)
        print(f"\n{outcome}:\n{rates}")


if __name__ == "__main__":
    main()
