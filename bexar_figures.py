"""
Bexar County — Figures for Law Review Paper
Requires: bexar_panel_1990_2021.parquet
Output: bexar_figures/ directory with PNG files
"""

import matplotlib
matplotlib.use("Agg")

import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import pandas as pd
import numpy as np
from pathlib import Path

OUT = Path("bexar_figures")
OUT.mkdir(exist_ok=True)

COLORS = {"Black": "#1f4e79", "Latino": "#c55a11", "White": "#538135"}
RACE_ORDER = ["Black", "Latino", "White"]

panel = pd.read_parquet("bexar_panel_1990_2021.parquet")
primary = panel[panel["RACE-LABEL"].isin(["Black", "White", "Latino"])].copy()


def save(fig, name):
    path = OUT / name
    fig.savefig(path, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved: {path}")


# ── Figure 1: Deferred adjudication rate by race × year ──────────────────────

def fig1_deferred_trend():
    yearly = (primary.groupby(["CASE-YEAR", "RACE-LABEL"])["DEFERRED"]
              .mean().reset_index())
    # smooth with 3-year rolling average
    yearly = yearly.sort_values(["RACE-LABEL", "CASE-YEAR"])
    yearly["DEFERRED-SMOOTH"] = (
        yearly.groupby("RACE-LABEL")["DEFERRED"]
        .transform(lambda s: s.rolling(3, center=True, min_periods=1).mean()))

    fig, ax = plt.subplots(figsize=(9, 5))
    for race in RACE_ORDER:
        d = yearly[yearly["RACE-LABEL"] == race]
        ax.plot(d["CASE-YEAR"], d["DEFERRED-SMOOTH"] * 100,
                color=COLORS[race], label=race, linewidth=2)
    ax.axvline(2019, color="gray", linestyle="--", linewidth=1, alpha=0.7,
               label="2019 DA transition")
    ax.yaxis.set_major_formatter(mtick.PercentFormatter())
    ax.set_xlabel("Case Year")
    ax.set_ylabel("Deferred Adjudication Rate")
    ax.set_title("Deferred Adjudication Rate by Race, 1990–2021\n"
                 "(3-year rolling average)", fontsize=11)
    ax.legend(title="Race/Ethnicity")
    ax.set_xlim(1990, 2021)
    fig.tight_layout()
    save(fig, "fig1_deferred_trend.png")


# ── Figure 2: Appointed counsel rate by race × year ──────────────────────────

def fig2_appointed_trend():
    appt = primary[primary["ATTORNEY-TYPE"].isin(["Appointed", "Hired"])].copy()
    appt["IS-APPOINTED"] = (appt["ATTORNEY-TYPE"] == "Appointed").astype(int)
    yearly = (appt.groupby(["CASE-YEAR", "RACE-LABEL"])["IS-APPOINTED"]
              .mean().reset_index())
    yearly = yearly.sort_values(["RACE-LABEL", "CASE-YEAR"])
    yearly["SMOOTH"] = (
        yearly.groupby("RACE-LABEL")["IS-APPOINTED"]
        .transform(lambda s: s.rolling(3, center=True, min_periods=1).mean()))

    fig, ax = plt.subplots(figsize=(9, 5))
    for race in RACE_ORDER:
        d = yearly[yearly["RACE-LABEL"] == race]
        ax.plot(d["CASE-YEAR"], d["SMOOTH"] * 100,
                color=COLORS[race], label=race, linewidth=2)
    ax.yaxis.set_major_formatter(mtick.PercentFormatter())
    ax.set_xlabel("Case Year")
    ax.set_ylabel("Appointed Counsel Rate")
    ax.set_title("Rate of Appointed Counsel by Race, 1990–2021\n"
                 "(3-year rolling average)", fontsize=11)
    ax.legend(title="Race/Ethnicity")
    ax.set_xlim(1990, 2021)
    fig.tight_layout()
    save(fig, "fig2_appointed_trend.png")


# ── Figure 3: Deferred rate by race × attorney type (bar) ────────────────────

def fig3_deferred_by_attorney():
    d = primary[primary["ATTORNEY-TYPE"].isin(["Appointed", "Hired"])].copy()
    grouped = (d.groupby(["RACE-LABEL", "ATTORNEY-TYPE"])["DEFERRED"]
               .mean().reset_index())
    grouped["DEFERRED-PCT"] = grouped["DEFERRED"] * 100

    fig, ax = plt.subplots(figsize=(7, 5))
    x = np.arange(len(RACE_ORDER))
    width = 0.35
    for i, atty in enumerate(["Appointed", "Hired"]):
        vals = [grouped.loc[(grouped["RACE-LABEL"] == r) &
                            (grouped["ATTORNEY-TYPE"] == atty), "DEFERRED-PCT"].values
                for r in RACE_ORDER]
        vals = [v[0] if len(v) else 0 for v in vals]
        bars = ax.bar(x + i * width - width / 2, vals, width,
                      label=f"{atty} Counsel",
                      color=[COLORS[r] for r in RACE_ORDER],
                      alpha=0.7 if atty == "Appointed" else 1.0,
                      edgecolor="white")
    ax.set_xticks(x)
    ax.set_xticklabels(RACE_ORDER)
    ax.yaxis.set_major_formatter(mtick.PercentFormatter())
    ax.set_ylabel("Deferred Adjudication Rate")
    ax.set_title("Deferred Adjudication Rate\nby Race and Attorney Type", fontsize=11)
    ax.legend()
    fig.tight_layout()
    save(fig, "fig3_deferred_by_attorney.png")


# ── Figure 4: Pre/Post 2019 comparison (DiD visual) ──────────────────────────

def fig4_did_visual():
    d = primary.copy()
    d["PERIOD"] = d["CASE-YEAR"].apply(
        lambda y: "Pre-2019" if y < 2019 else "2019–2021")
    grouped = (d.groupby(["PERIOD", "RACE-LABEL"])["DEFERRED"]
               .mean().reset_index())
    grouped["DEFERRED-PCT"] = grouped["DEFERRED"] * 100

    fig, axes = plt.subplots(1, 2, figsize=(10, 5), sharey=True)
    for ax, period in zip(axes, ["Pre-2019", "2019–2021"]):
        sub = grouped[grouped["PERIOD"] == period]
        bars = ax.bar(
            RACE_ORDER,
            [sub.loc[sub["RACE-LABEL"] == r, "DEFERRED-PCT"].values[0]
             if len(sub.loc[sub["RACE-LABEL"] == r]) else 0
             for r in RACE_ORDER],
            color=[COLORS[r] for r in RACE_ORDER])
        ax.yaxis.set_major_formatter(mtick.PercentFormatter())
        ax.set_title(f"{period} DA\n(Nico LaHood → Joe Gonzales)" if period == "2019–2021"
                     else f"{period}", fontsize=10)
        ax.set_ylabel("Deferred Adjudication Rate" if period == "Pre-2019" else "")
        for bar in bars:
            h = bar.get_height()
            ax.text(bar.get_x() + bar.get_width() / 2, h + 0.1,
                    f"{h:.1f}%", ha="center", va="bottom", fontsize=9)
    fig.suptitle("Deferred Adjudication Rate Before and After 2019 DA Transition",
                 fontsize=11)
    fig.tight_layout()
    save(fig, "fig4_did_visual.png")


# ── Figure 5: Outcome rates overall (horizontal bar) ─────────────────────────

def fig5_outcome_overview():
    outcomes = {
        "Deferred\nAdjudication": "DEFERRED",
        "Dismissed": "DISMISSED",
        "Guilty Plea": "GUILTY-PLEA",
        "Convicted": "CONVICTED",
    }
    fig, ax = plt.subplots(figsize=(9, 5))
    y = np.arange(len(outcomes))
    height = 0.25

    for i, race in enumerate(RACE_ORDER):
        d = primary[primary["RACE-LABEL"] == race]
        vals = [d[col].mean() * 100 for col in outcomes.values()]
        ax.barh(y + i * height - height, vals, height,
                label=race, color=COLORS[race])

    ax.set_yticks(y)
    ax.set_yticklabels(list(outcomes.keys()))
    ax.xaxis.set_major_formatter(mtick.PercentFormatter())
    ax.set_xlabel("Rate")
    ax.set_title("Case Outcome Rates by Race, 1990–2021", fontsize=11)
    ax.legend(title="Race/Ethnicity")
    fig.tight_layout()
    save(fig, "fig5_outcome_overview.png")


if __name__ == "__main__":
    fig1_deferred_trend()
    fig2_appointed_trend()
    fig3_deferred_by_attorney()
    fig4_did_visual()
    fig5_outcome_overview()
    print(f"\nAll figures saved to: {OUT}/")
