# Bexar County — Descriptive Summary Table for Law Review Paper
# Requires: bexar_panel_1990_2021.parquet (from bexar_clean.R)
# Output: bexar_summary_table.csv

library(tidyverse)
library(arrow)

panel   <- read_parquet("bexar_panel_1990_2021.parquet")
primary <- panel |> filter(`RACE-LABEL` %in% c("Black", "White", "Latino"))

# ── Race × decade table ───────────────────────────────────────────────────────

summary_table <- primary |>
  group_by(Race = `RACE-LABEL`, Decade = `CASE-DECADE`) |>
  summarise(
    N = n(),
    Appointed_Counsel_Pct = round(
      mean(`ATTORNEY-TYPE` == "Appointed", na.rm = TRUE) * 100, 1),
    Deferred_Adjudication_Pct = round(mean(DEFERRED,      na.rm = TRUE) * 100, 1),
    Dismissed_Pct             = round(mean(DISMISSED,     na.rm = TRUE) * 100, 1),
    Guilty_Plea_Pct           = round(mean(`GUILTY-PLEA`, na.rm = TRUE) * 100, 1),
    Convicted_Pct             = round(mean(CONVICTED,     na.rm = TRUE) * 100, 1),
    Mean_Bond_Amount          = round(
      mean(`BOND-AMOUNT`[`BOND-MISSING` == 0], na.rm = TRUE), 0),
    .groups = "drop"
  ) |>
  arrange(Decade, Race)

write_csv(summary_table, "bexar_summary_table.csv")
message("Saved: bexar_summary_table.csv")
print(summary_table, n = Inf)

# ── Overall totals by race ────────────────────────────────────────────────────

message("\n── Overall by race ──")
primary |>
  group_by(Race = `RACE-LABEL`) |>
  summarise(
    N = n(),
    Appointed_Pct  = round(mean(`ATTORNEY-TYPE` == "Appointed", na.rm = TRUE) * 100, 1),
    Deferred_Pct   = round(mean(DEFERRED,      na.rm = TRUE) * 100, 1),
    Dismissed_Pct  = round(mean(DISMISSED,     na.rm = TRUE) * 100, 1),
    GuiltyPlea_Pct = round(mean(`GUILTY-PLEA`, na.rm = TRUE) * 100, 1),
    Convicted_Pct  = round(mean(CONVICTED,     na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) |>
  print()
