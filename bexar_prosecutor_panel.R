# Bexar County — Prosecutor Learning Curve Panel
# Requires: bexar_panel_1990_2021.parquet (from bexar_clean.R)
# Output: bexar_prosecutor_panel_1990_2015.parquet, bexar_codebook.md
#
# Builds per-case career experience variables for each line prosecutor:
#   PROSECUTOR_CASE_N   — cumulative case number within prosecutor career
#   PROSECUTOR_CAREER_YEAR — case year minus first case year for that prosecutor
#   EXP_QUINTILE        — quintile of PROSECUTOR_CASE_N within each prosecutor

DATA_DIR <- "C:/Users/carolineferguson/Box/Bigelow/Bexar/Data"  # all inputs and outputs here

library(tidyverse)
library(arrow)

panel <- read_parquet(file.path(DATA_DIR, "bexar_panel_1990_2021.parquet"))

# ── 1. Prosecutor data window: 1990–2015 ──────────────────────────────────────
# INTAKE-PROSECUTOR is blank ~100% pre-1990 and 63–76% from 2016 onward.
# The 1990–2015 window has 7–15% blank rates per year.

dp <- panel |>
  filter(
    `CASE-YEAR` >= 1990,
    `CASE-YEAR` <= 2015,
    !is.na(`INTAKE-PROSECUTOR`),
    `INTAKE-PROSECUTOR` != ""
  )

message("Cases 1990-2015 with non-blank prosecutor: ", nrow(dp), " of ",
        nrow(panel |> filter(`CASE-YEAR` >= 1990, `CASE-YEAR` <= 2015)),
        " (", round(nrow(dp) / nrow(panel |> filter(`CASE-YEAR` >= 1990, `CASE-YEAR` <= 2015)) * 100, 1), "%)")

# ── 2. Blank rate by year (diagnostic) ───────────────────────────────────────

blank_by_year <- panel |>
  filter(`CASE-YEAR` >= 1988, `CASE-YEAR` <= 2021) |>
  group_by(Year = `CASE-YEAR`) |>
  summarise(
    N_total = n(),
    N_blank  = sum(is.na(`INTAKE-PROSECUTOR`) | `INTAKE-PROSECUTOR` == ""),
    Blank_pct = round(N_blank / N_total * 100, 1),
    .groups = "drop"
  )

write_csv(blank_by_year, file.path(DATA_DIR, "bexar_prosecutor_blank_by_year.csv"))
message("Saved: bexar_prosecutor_blank_by_year.csv")
print(blank_by_year, n = Inf)

# ── 3. Restrict to prosecutors with 50+ cases ─────────────────────────────────

vol <- dp |>
  count(`INTAKE-PROSECUTOR`, name = "total_cases") |>
  filter(total_cases >= 50)

message("\nHigh-volume prosecutors (50+ cases, 1990-2015): ", nrow(vol))

dp <- dp |> semi_join(vol, by = "INTAKE-PROSECUTOR")
message("Cases from high-volume prosecutors: ", nrow(dp))

# ── 4. Career sequence variables ─────────────────────────────────────────────

dp <- dp |>
  arrange(`INTAKE-PROSECUTOR`, `CASE-DATE`) |>
  group_by(`INTAKE-PROSECUTOR`) |>
  mutate(
    PROSECUTOR_CASE_N    = row_number(),
    FIRST_CASE_YEAR      = min(`CASE-YEAR`, na.rm = TRUE),
    LAST_CASE_YEAR       = max(`CASE-YEAR`, na.rm = TRUE),
    CAREER_SPAN          = LAST_CASE_YEAR - FIRST_CASE_YEAR,
    PROSECUTOR_CAREER_YEAR = `CASE-YEAR` - FIRST_CASE_YEAR,
    EXP_QUINTILE         = ntile(PROSECUTOR_CASE_N, 5)
  ) |>
  ungroup()

# ── 5. DA administration cohort ───────────────────────────────────────────────
# Approximate DA tenures in Bexar County (for hire-year coding):
#   Canales:  ~1986–1990
#   Millard:  1991–1998
#   Reed:     1999–2014
#   LaHood:   2015–2018
#   Gonzales: 2019–present
# We tag each prosecutor by the DA in office when they filed their FIRST case.

da_era <- function(year) {
  case_when(
    year <= 1990 ~ "Canales",
    year <= 1998 ~ "Millard",
    year <= 2014 ~ "Reed",
    year <= 2018 ~ "LaHood",
    TRUE         ~ "Gonzales"
  )
}

dp <- dp |>
  mutate(DA_AT_HIRE = da_era(FIRST_CASE_YEAR))

# Flag prosecutors whose career spans a DA transition
da_transitions <- c(1991, 1999, 2015, 2019)

dp <- dp |>
  group_by(`INTAKE-PROSECUTOR`) |>
  mutate(
    CROSSES_DA_TRANSITION = any(
      purrr::map_lgl(da_transitions, ~ any(`CASE-YEAR` < .x) & any(`CASE-YEAR` >= .x))
    )
  ) |>
  ungroup()

# ── 6. Offense category (collapsed from OFFENSE-DESC) ────────────────────────
# Collapse OFFENSE-DESC into eight groups for fixed effects.
# Adjust regex patterns after reviewing full merged distribution.

dp <- dp |>
  mutate(
    OFFENSE_CATEGORY = case_when(
      str_detect(`OFFENSE-DESC`, regex("drug|controlled|marijuana|cocaine|heroin|meth|narcot", TRUE)) ~ "Drug",
      str_detect(`OFFENSE-DESC`, regex("theft|robbery|fraud|forgery|burglary of vehicle|stolen", TRUE)) ~ "Property",
      str_detect(`OFFENSE-DESC`, regex("assault|agg assault|family|bodily", TRUE)) ~ "Assault",
      str_detect(`OFFENSE-DESC`, regex("burglary of hab|burglary of build", TRUE)) ~ "Burglary",
      str_detect(`OFFENSE-DESC`, regex("weapon|firearm|deadly conduct|prohibited weapon", TRUE)) ~ "Weapon",
      str_detect(`OFFENSE-DESC`, regex("dwi|dui|intoxicat", TRUE)) ~ "DWI",
      str_detect(`OFFENSE-DESC`, regex("sex|rape|indecency|sexual|prostit", TRUE)) ~ "Sex",
      TRUE ~ "Other"
    )
  )

message("\n── Offense category distribution ──")
print(count(dp, OFFENSE_CATEGORY, sort = TRUE))

# ── 7. Prosecutor volume table ────────────────────────────────────────────────

prosecutor_table <- dp |>
  group_by(Prosecutor = `INTAKE-PROSECUTOR`) |>
  summarise(
    N_cases     = n(),
    First_year  = min(`CASE-YEAR`),
    Last_year   = max(`CASE-YEAR`),
    Career_span = Last_year - First_year,
    DA_at_hire  = first(DA_AT_HIRE),
    .groups = "drop"
  ) |>
  arrange(desc(N_cases))

write_csv(prosecutor_table, file.path(DATA_DIR, "bexar_prosecutor_table.csv"))
message("Saved: bexar_prosecutor_table.csv  (", nrow(prosecutor_table), " prosecutors)")
print(prosecutor_table, n = 20)

# ── 8. Cell size table: race × offense × quintile ────────────────────────────

cell_table <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White", "Latino")) |>
  count(`RACE-LABEL`, `OFFENSE-CLASS`, EXP_QUINTILE, name = "N") |>
  mutate(FLAG_SMALL = N < 30)

write_csv(cell_table, file.path(DATA_DIR, "bexar_cell_size_table.csv"))
n_small <- sum(cell_table$FLAG_SMALL)
message("Cell size table saved. Small cells (<30): ", n_small)
if (n_small > 0) {
  message("  Small cells:")
  print(filter(cell_table, FLAG_SMALL))
}

# ── 9. Export ─────────────────────────────────────────────────────────────────

write_parquet(dp, file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))
message("\nSaved: bexar_prosecutor_panel_1990_2015.parquet  (", nrow(dp), " cases)")

# ── 10. Preliminary findings check (Section 3 of handoff) ─────────────────────

message("\n", strrep("=", 60))
message("PRELIMINARY FINDINGS CHECK — full dataset")
message(strrep("=", 60))

bw <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White")) |>
  group_by(`RACE-LABEL`, EXP_QUINTILE) |>
  summarise(
    N        = n(),
    def_rate = mean(DEFERRED, na.rm = TRUE),
    gp_rate  = mean(`GUILTY-PLEA`, na.rm = TRUE),
    .groups  = "drop"
  )

message("\nDeferred adjudication rate by race × experience quintile:")
print(bw |> select(`RACE-LABEL`, EXP_QUINTILE, N, def_rate) |>
        pivot_wider(names_from = `RACE-LABEL`, values_from = c(N, def_rate)) |>
        mutate(gap_WB = round((def_rate_White - def_rate_Black) * 100, 1)))

message("\nGuilty plea rate by race × experience quintile:")
print(bw |> select(`RACE-LABEL`, EXP_QUINTILE, gp_rate) |>
        pivot_wider(names_from = `RACE-LABEL`, values_from = gp_rate) |>
        mutate(gap_WB = round((White - Black) * 100, 1)))

message("\nOffense composition by experience quintile:")
print(dp |>
  group_by(EXP_QUINTILE, `OFFENSE-CLASS`) |>
  summarise(N = n(), .groups = "drop") |>
  group_by(EXP_QUINTILE) |>
  mutate(pct = round(N / sum(N) * 100, 1)) |>
  filter(`OFFENSE-CLASS` %in% c("F1", "F2", "F3", "FS")) |>
  select(EXP_QUINTILE, `OFFENSE-CLASS`, pct) |>
  pivot_wider(names_from = `OFFENSE-CLASS`, values_from = pct))

message("\nAppointed counsel only — gap by quintile:")
dp_appt <- dp |>
  filter(`ATTORNEY-TYPE` == "Appointed", `RACE-LABEL` %in% c("Black", "White"))
print(dp_appt |>
  group_by(`RACE-LABEL`, EXP_QUINTILE) |>
  summarise(def_rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = `RACE-LABEL`, values_from = def_rate) |>
  mutate(gap_WB = round((White - Black) * 100, 1)))
