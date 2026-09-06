# Bexar County — Descriptive Outputs
# Requires: bexar_prosecutor_panel_1990_2015.parquet (from bexar_prosecutor_panel.R)
# Outputs: bexar_summary_table.csv, printed diagnostics

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"

library(tidyverse)
library(arrow)

dp    <- read_parquet(file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))
panel <- read_parquet(file.path(DATA_DIR, "bexar_panel_1990_2021.parquet"))

primary <- dp |> filter(`RACE-LABEL` %in% c("Black", "White", "Latino"))

# ── 1. Overall descriptives by race ──────────────────────────────────────────

message("── Overall by race (prosecutor panel 1990-2015) ──")
overall <- primary |>
  group_by(Race = `RACE-LABEL`) |>
  summarise(
    N = n(),
    Appointed_pct  = round(mean(`ATTORNEY-TYPE` == "Appointed", na.rm = TRUE) * 100, 1),
    Deferred_pct   = round(mean(DEFERRED,      na.rm = TRUE) * 100, 1),
    Dismissed_pct  = round(mean(DISMISSED,     na.rm = TRUE) * 100, 1),
    GuiltyPlea_pct = round(mean(`GUILTY-PLEA`, na.rm = TRUE) * 100, 1),
    Convicted_pct  = round(mean(CONVICTED,     na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(overall)

# ── 2. Race × decade table (for general paper data section) ──────────────────

message("\n── Race × decade (full 1990-2021 panel) ──")
summary_table <- panel |>
  filter(`RACE-LABEL` %in% c("Black", "White", "Latino")) |>
  group_by(Race = `RACE-LABEL`, Decade = `CASE-DECADE`) |>
  summarise(
    N = n(),
    Appointed_pct  = round(mean(`ATTORNEY-TYPE` == "Appointed", na.rm = TRUE) * 100, 1),
    Deferred_pct   = round(mean(DEFERRED,      na.rm = TRUE) * 100, 1),
    Dismissed_pct  = round(mean(DISMISSED,     na.rm = TRUE) * 100, 1),
    GuiltyPlea_pct = round(mean(`GUILTY-PLEA`, na.rm = TRUE) * 100, 1),
    Convicted_pct  = round(mean(CONVICTED,     na.rm = TRUE) * 100, 1),
    Mean_Bond      = round(mean(`BOND-AMOUNT`[`BOND-MISSING` == 0], na.rm = TRUE), 0),
    .groups = "drop"
  ) |>
  arrange(Decade, Race)

write_csv(summary_table, file.path(DATA_DIR, "bexar_summary_table.csv"))
message("Saved: bexar_summary_table.csv")
print(summary_table, n = Inf)

# ── 3. Deferred rate by race × experience quintile ───────────────────────────

message("\n── Deferred rate by race × experience quintile ──")
primary |>
  group_by(Race = `RACE-LABEL`, Q = EXP_QUINTILE) |>
  summarise(
    N = n(),
    Deferred_pct = round(mean(DEFERRED, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = Race, values_from = c(N, Deferred_pct)) |>
  mutate(WB_gap_pp = round(Deferred_pct_White - Deferred_pct_Black, 2)) |>
  print()

# ── 4. Offense composition by experience quintile ────────────────────────────

message("\n── Offense composition by experience quintile ──")
dp |>
  filter(`OFFENSE-CLASS` %in% c("F1", "F2", "F3", "FS")) |>
  count(Q = EXP_QUINTILE, `OFFENSE-CLASS`) |>
  group_by(Q) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  select(-n) |>
  pivot_wider(names_from = `OFFENSE-CLASS`, values_from = pct) |>
  print()
