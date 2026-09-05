"""
Bexar County — Regression Models
Requires: bexar_panel_1990_2021.parquet (produced by bexar_clean.py)

Models:
  1. Baseline logistic: DEFERRED ~ RACE + OFFENSE_CLASS + SEX + CASE_YEAR
  2. Add attorney type
  3. Add prosecutor fixed effects (>= 50 cases)
  4. DiD: DEFERRED ~ RACE * POST2019 + controls
  5. Robustness: GUILTY_PLEA as outcome

Output: bexar_model_results.csv (coefficient tables), printed summaries
"""

import warnings
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from statsmodels.tools.sm_exceptions import PerfectSeparationWarning

warnings.filterwarnings("ignore", category=PerfectSeparationWarning)

panel = pd.read_parquet("bexar_panel_1990_2021.parquet")

# ── Prep analysis frame ───────────────────────────────────────────────────────

primary = panel[panel["RACE-LABEL"].isin(["Black", "White", "Latino"])].copy()
primary = primary[primary["SEX-LABEL"].isin(["M", "F"])].copy()

# Reference category: White, F3, Hired counsel
primary["RACE_CAT"]     = pd.Categorical(primary["RACE-LABEL"],
                                          categories=["White", "Black", "Latino"])
primary["OFFENSE_CAT"]  = pd.Categorical(
    primary["OFFENSE-CLASS"],
    categories=["F3", "F1", "F2", "FS", "Other Felony", "Misdemeanor"])
primary["SEX_CAT"]      = pd.Categorical(primary["SEX-LABEL"],
                                          categories=["M", "F"])
primary["ATTY_CAT"]     = pd.Categorical(
    primary.loc[primary["ATTORNEY-TYPE"].isin(["Appointed", "Hired"]), "ATTORNEY-TYPE"],
    categories=["Hired", "Appointed"])
primary["POST2019"]     = (primary["CASE-YEAR"] >= 2019).astype(int)
primary["CASE_YEAR_CTR"] = primary["CASE-YEAR"] - 2005  # center for interpretability

# Restrict to known felonies for primary models
felony_mask = primary["OFFENSE-CLASS"].isin(["F1", "F2", "F3", "FS", "Other Felony"])


def run_logit(formula: str, data: pd.DataFrame, label: str):
    print(f"\n{'='*60}")
    print(f"Model: {label}")
    print(f"N = {len(data):,}")
    print(f"{'='*60}")
    try:
        model = smf.logit(formula, data=data).fit(
            method="bfgs", maxiter=500, disp=False)
        print(model.summary2())
        return model
    except Exception as e:
        print(f"  ERROR: {e}")
        return None


# ── Model 1: Baseline ─────────────────────────────────────────────────────────

d1 = primary[felony_mask].dropna(subset=["DEFERRED", "CASE_YEAR_CTR"])
m1 = run_logit(
    "DEFERRED ~ C(RACE_CAT) + C(OFFENSE_CAT) + C(SEX_CAT) + CASE_YEAR_CTR",
    d1, "1 — Baseline (deferred ~ race + offense + sex + year)")

# ── Model 2: Add attorney type ────────────────────────────────────────────────

d2 = primary[felony_mask & primary["ATTORNEY-TYPE"].isin(["Appointed", "Hired"])].copy()
d2 = d2.dropna(subset=["DEFERRED", "CASE_YEAR_CTR"])
m2 = run_logit(
    "DEFERRED ~ C(RACE_CAT) + C(OFFENSE_CAT) + C(SEX_CAT) + C(ATTY_CAT) + CASE_YEAR_CTR",
    d2, "2 — Add attorney type")

# ── Model 3: Prosecutor fixed effects ────────────────────────────────────────

high_vol = primary["INTAKE-PROSECUTOR"].value_counts()
high_vol = high_vol[high_vol >= 50].index
d3 = d2[d2["INTAKE-PROSECUTOR"].isin(high_vol)].copy()
d3["PROSECUTOR_FE"] = d3["INTAKE-PROSECUTOR"].astype("category")
m3 = run_logit(
    "DEFERRED ~ C(RACE_CAT) + C(OFFENSE_CAT) + C(SEX_CAT) + C(ATTY_CAT)"
    " + CASE_YEAR_CTR + C(PROSECUTOR_FE)",
    d3, f"3 — Prosecutor FE (prosecutors with ≥50 cases, N prosecutors = {len(high_vol)})")

# ── Model 4: DiD (race × post-2019) ──────────────────────────────────────────

d4 = d2.copy()
m4 = run_logit(
    "DEFERRED ~ C(RACE_CAT) * POST2019 + C(OFFENSE_CAT) + C(SEX_CAT) + C(ATTY_CAT)",
    d4, "4 — DiD: race × POST2019")

# ── Model 5: Guilty plea (robustness) ────────────────────────────────────────

m5 = run_logit(
    "Q('GUILTY-PLEA') ~ C(RACE_CAT) + C(OFFENSE_CAT) + C(SEX_CAT) + C(ATTY_CAT) + CASE_YEAR_CTR",
    d2, "5 — Robustness: guilty plea as outcome")

# ── Export coefficient table ──────────────────────────────────────────────────

def coef_table(model, label):
    if model is None:
        return pd.DataFrame()
    t = model.summary2().tables[1].copy()
    t.columns = ["Coef", "Std_Err", "z", "P>|z|", "CI_Low", "CI_High"]
    t["OR"] = np.exp(t["Coef"]).round(4)
    t["Model"] = label
    t.index.name = "Variable"
    return t.reset_index()

tables = pd.concat([
    coef_table(m1, "M1_Baseline"),
    coef_table(m2, "M2_AttorneyType"),
    coef_table(m3, "M3_ProsecutorFE"),
    coef_table(m4, "M4_DiD"),
    coef_table(m5, "M5_GuiltyPlea"),
], ignore_index=True)

tables.to_csv("bexar_model_results.csv", index=False)
print("\n\nSaved coefficient tables to: bexar_model_results.csv")

# ── Sentence length OLS (conditional on conviction) ──────────────────────────

convicted = primary[
    felony_mask &
    (primary["CONVICTED"] == 1) &
    (primary["SENTENCE-DAYS"] > 0) &
    primary["ATTORNEY-TYPE"].isin(["Appointed", "Hired"])
].copy()
convicted["LOG_SENTENCE"] = np.log(convicted["SENTENCE-DAYS"])

print(f"\n{'='*60}")
print(f"OLS — Log sentence days | convicted (N = {len(convicted):,})")
print(f"{'='*60}")
try:
    ols = smf.ols(
        "LOG_SENTENCE ~ C(RACE_CAT) + C(OFFENSE_CAT) + C(SEX_CAT) + C(ATTY_CAT) + CASE_YEAR_CTR",
        data=convicted).fit()
    print(ols.summary2())
    ols_tbl = ols.summary2().tables[1].copy()
    ols_tbl["Model"] = "OLS_LogSentence"
    ols_tbl.index.name = "Variable"
    ols_tbl.reset_index().to_csv("bexar_ols_sentence.csv", index=False)
    print("Saved: bexar_ols_sentence.csv")
except Exception as e:
    print(f"  ERROR: {e}")
