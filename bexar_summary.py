"""
Bexar County — Descriptive Summary Table for Law Review Paper
Requires: bexar_panel_1990_2021.parquet (produced by bexar_clean.py)
Output: bexar_summary_table.csv
"""

import pandas as pd
import numpy as np

panel = pd.read_parquet("bexar_panel_1990_2021.parquet")
primary = panel[panel["RACE-LABEL"].isin(["Black", "White", "Latino"])].copy()


def pct(s):
    return s.mean().round(4) * 100


# ── By race × decade ──────────────────────────────────────────────────────────

def race_decade_table(df):
    rows = []
    for race in ["Black", "White", "Latino"]:
        sub = df[df["RACE-LABEL"] == race]
        for decade in sorted(sub["CASE-DECADE"].dropna().unique()):
            d = sub[sub["CASE-DECADE"] == decade]
            rows.append({
                "Race": race,
                "Decade": int(decade),
                "N": len(d),
                "Appointed_Counsel_Pct": round(
                    (d["ATTORNEY-TYPE"] == "Appointed").mean() * 100, 1),
                "Deferred_Adjudication_Pct": round(d["DEFERRED"].mean() * 100, 1),
                "Dismissed_Pct":   round(d["DISMISSED"].mean() * 100, 1),
                "Guilty_Plea_Pct": round(d["GUILTY-PLEA"].mean() * 100, 1),
                "Convicted_Pct":   round(d["CONVICTED"].mean() * 100, 1),
                "Mean_Bond_Amount": round(
                    d.loc[d["BOND-MISSING"] == 0, "BOND-AMOUNT"].mean(), 0),
            })
    return pd.DataFrame(rows)


table = race_decade_table(primary)
table.to_csv("bexar_summary_table.csv", index=False)
print("Saved: bexar_summary_table.csv")
print(table.to_string(index=False))

# ── Overall totals by race ────────────────────────────────────────────────────

print("\n── Overall by race ──")
overall = primary.groupby("RACE-LABEL").agg(
    N=("CASE-CAUSE-NBR", "count"),
    Appointed_Pct=("ATTORNEY-TYPE", lambda x: round((x == "Appointed").mean() * 100, 1)),
    Deferred_Pct=("DEFERRED", lambda x: round(x.mean() * 100, 1)),
    Dismissed_Pct=("DISMISSED", lambda x: round(x.mean() * 100, 1)),
    Guilty_Plea_Pct=("GUILTY-PLEA", lambda x: round(x.mean() * 100, 1)),
    Convicted_Pct=("CONVICTED", lambda x: round(x.mean() * 100, 1)),
)
print(overall)
