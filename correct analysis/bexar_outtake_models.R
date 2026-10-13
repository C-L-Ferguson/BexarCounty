# Bexar County — Outtake Prosecutor Regression Models (Correct Analysis)
# Input:  bexar_panel_1990_2021.parquet  (from bexar_clean.R)
# Output: table_gap_controls.tex, table_experience.tex
#
# Outcome:  DEFERRED_GRANT  (from JUDGEMENT-DESC "DEFR ADJUD")
# Exposure: BLACK, LATINO; BLACK/LATINO × OUTTAKE_CASE_N100 interactions
# Prosecutor: OUTTAKE-PROSECUTOR (disposition-stage)
# Experience: OUTTAKE_CASE_N ordered by G-JURY-DATE (not CASE-DATE)
# Sample:  fresh_outtake (first GJURY_YEAR >= 1991), GJURY_YEAR 1997–2019

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"

library(tidyverse)
library(arrow)
library(lubridate)
library(fixest)

# ── 1. Load raw panel ─────────────────────────────────────────────────────────

dp_raw <- read_parquet(file.path(DATA_DIR, "bexar_panel_1990_2021.parquet")) |>
  mutate(
    GJURY_DATE = mdy(`G-JURY-DATE`),
    GJURY_YEAR = year(GJURY_DATE)
  )

# ── 2. fresh_outtake: prosecutors first seen 1991+ (excludes left-censored) ──
# Uses GJURY_YEAR (not CASE-YEAR) because CASE-DATE is contaminated for
# deferred cases (set to supervision end date, not filing date).

fresh_outtake <- dp_raw |>
  filter(!is.na(`OUTTAKE-PROSECUTOR`), `OUTTAKE-PROSECUTOR` != "",
         !is.na(GJURY_DATE), GJURY_YEAR >= 1950, GJURY_YEAR <= 2021) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  summarise(first_gjury_year = min(GJURY_YEAR, na.rm = TRUE), .groups = "drop") |>
  filter(first_gjury_year >= 1991) |>
  pull(`OUTTAKE-PROSECUTOR`)

message("Fresh outtake prosecutors: ", length(fresh_outtake))

# ── 3. Experience sequence (ALL offense classes, ALL races) ───────────────────
# Must count all cases toward career experience, not just the analytic subsample.
# One row per case (keep most serious charge when multiple rows per case).

offense_order <- c("F1", "F2", "F3", "FS")

pros_case_seq <- dp_raw |>
  filter(`OUTTAKE-PROSECUTOR` %in% fresh_outtake,
         !is.na(`OUTTAKE-PROSECUTOR`), !is.na(GJURY_DATE)) |>
  mutate(OFFENSE_RANK = coalesce(match(`OFFENSE-CLASS`, offense_order), 99L)) |>
  arrange(`CASE-CAUSE-NBR`, OFFENSE_RANK) |>
  distinct(`CASE-CAUSE-NBR`, .keep_all = TRUE) |>
  arrange(`OUTTAKE-PROSECUTOR`, GJURY_DATE) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  mutate(OUTTAKE_CASE_N = row_number()) |>
  ungroup() |>
  select(`CASE-CAUSE-NBR`, OUTTAKE_CASE_N)

message("Cases in experience sequence: ", nrow(pros_case_seq))

# ── 4. First GJURY_YEAR per prosecutor (for TENURE_YEARS) ────────────────────

first_year_tbl <- dp_raw |>
  filter(`OUTTAKE-PROSECUTOR` %in% fresh_outtake,
         !is.na(GJURY_DATE), GJURY_YEAR >= 1950) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  summarise(first_gjury_year = min(GJURY_YEAR, na.rm = TRUE), .groups = "drop")

# ── 5. Analytic case frame ────────────────────────────────────────────────────
# Restrict to fresh outtake, non-missing race (Black/White/Latino),
# felony charges (F1/F2/F3/FS), valid GJURY_DATE.

dp_out_case <- dp_raw |>
  filter(`OUTTAKE-PROSECUTOR` %in% fresh_outtake,
         !is.na(`OUTTAKE-PROSECUTOR`),
         `RACE-LABEL` %in% c("Black", "White", "Latino"),
         `OFFENSE-CLASS` %in% offense_order,
         !is.na(GJURY_DATE), GJURY_YEAR >= 1950, GJURY_YEAR <= 2021) |>
  mutate(OFFENSE_RANK = match(`OFFENSE-CLASS`, offense_order)) |>
  arrange(`CASE-CAUSE-NBR`, OFFENSE_RANK) |>
  distinct(`CASE-CAUSE-NBR`, .keep_all = TRUE) |>
  select(-OFFENSE_RANK) |>
  left_join(pros_case_seq, by = "CASE-CAUSE-NBR") |>
  left_join(first_year_tbl, by = "OUTTAKE-PROSECUTOR") |>
  mutate(
    # Outcome: grant of deferred adjudication (from JUDGEMENT-DESC)
    DEFERRED_GRANT    = as.integer(str_detect(replace_na(`JUDGEMENT-DESC`, ""),
                                              regex("DEFR ADJUD", ignore_case = TRUE))),
    BLACK             = as.integer(`RACE-LABEL` == "Black"),
    LATINO            = as.integer(`RACE-LABEL` == "Latino"),
    OFFENSE_TYPE      = fct_relevel(factor(`OFFENSE-CLASS`), "F3"),
    APPOINTED         = as.integer(`ATTORNEY-TYPE` == "Appointed"),
    GJURY_YEAR_FE     = factor(GJURY_YEAR),
    COURT_F           = factor(COURT),
    PROSECUTOR        = factor(`OUTTAKE-PROSECUTOR`),
    OUTTAKE_CASE_N100 = OUTTAKE_CASE_N / 100,
    TENURE_YEARS      = GJURY_YEAR - first_gjury_year,
    OFFENSE_CAT = case_when(
      str_detect(`OFFENSE-DESC`, regex("homicide|murder|manslaughter", TRUE))        ~ "Homicide",
      str_detect(`OFFENSE-DESC`, regex("sex|rape|indecency|sexual|prostit", TRUE))   ~ "Sex",
      str_detect(`OFFENSE-DESC`, regex("dwi|dui|intoxicat", TRUE))                   ~ "DWI",
      str_detect(`OFFENSE-DESC`, regex("weapon|firearm|deadly conduct|prohibited weapon", TRUE)) ~ "Weapon",
      str_detect(`OFFENSE-DESC`, regex("drug|controlled|marijuana|cocaine|heroin|meth|narcot", TRUE)) ~ "Drug",
      str_detect(`OFFENSE-DESC`, regex("burglary of hab|burglary of build", TRUE))   ~ "Burglary",
      str_detect(`OFFENSE-DESC`, regex("theft|robbery|fraud|forgery|burglary of vehicle|stolen", TRUE)) ~ "Property",
      str_detect(`OFFENSE-DESC`, regex("evad", TRUE))                                ~ "Evading",
      TRUE ~ "Other"
    )
  )

message("dp_out_case: ", nrow(dp_out_case), " cases, ",
        n_distinct(dp_out_case$`OUTTAKE-PROSECUTOR`), " prosecutors")

# ── 6. Prior record ───────────────────────────────────────────────────────────
# Count prior cases, convictions, and deferrals per defendant (by SID)
# prior to each case's GJURY_DATE.

prior2 <- dp_raw |>
  filter(!is.na(SID), SID != "") |>
  mutate(GJURY_DATE = mdy(`G-JURY-DATE`),
         DEFERRED_GRANT = as.integer(str_detect(replace_na(`JUDGEMENT-DESC`, ""),
                                                regex("DEFR ADJUD", ignore_case = TRUE)))) |>
  filter(!is.na(GJURY_DATE)) |>
  arrange(SID, GJURY_DATE) |>
  group_by(SID) |>
  mutate(
    PRIOR_CASES = row_number() - 1L,
    PRIOR_CONV  = cumsum(lag(as.integer(CONVICTED == 1L), default = 0L)),
    PRIOR_DEF   = cumsum(lag(DEFERRED_GRANT, default = 0L))
  ) |>
  ungroup() |>
  select(`CASE-CAUSE-NBR`, PRIOR_CASES, PRIOR_CONV, PRIOR_DEF)

dp_out_case <- dp_out_case |> left_join(prior2, by = "CASE-CAUSE-NBR")

# ── 7. Main analytic sample: 1997–2019 ───────────────────────────────────────
# Drop pre-1997 (thin N) and 2020+ (COVID collapse).

dp_main <- dp_out_case |>
  filter(GJURY_YEAR >= 1997, GJURY_YEAR <= 2019) |>
  mutate(
    AGE           = as.numeric(difftime(GJURY_DATE, BIRTHDATE, units = "days")) / 365.25,
    AGE           = if_else(AGE >= 14 & AGE <= 90, AGE, NA_real_),
    FEMALE        = as.integer(`SEX-LABEL` == "F"),
    PRIOR_BIN     = factor(pmin(PRIOR_CASES, 10)),
    PRIOR_CONV_BIN = factor(pmin(PRIOR_CONV, 5)),
    PRIOR_DEF_BIN  = factor(pmin(PRIOR_DEF, 5))
  )

message("dp_main: ", nrow(dp_main), " cases, ",
        n_distinct(dp_main$`OUTTAKE-PROSECUTOR`), " prosecutors")

# Raw grant rates by race
dp_main |>
  group_by(`RACE-LABEL`) |>
  summarise(rate = mean(DEFERRED_GRANT, na.rm = TRUE), n = n(), .groups = "drop") |>
  print()

# ── 8. Models ─────────────────────────────────────────────────────────────────

# m3: raw gap (prosecutor + year + court FE, offense controls, no prior record)
m3 <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED
            | PROSECUTOR + GJURY_YEAR + COURT_F,
            data = dp_main, cluster = ~PROSECUTOR)

# m4: + linear prior cases
m4 <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED +
              PRIOR_CASES
            | PROSECUTOR + GJURY_YEAR + COURT_F,
            data = dp_main, cluster = ~PROSECUTOR)

# m5: + linear prior convictions and deferrals
m5 <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED +
              pmin(PRIOR_CONV, 5) + pmin(PRIOR_DEF, 5)
            | PROSECUTOR + GJURY_YEAR + COURT_F,
            data = dp_main, cluster = ~PROSECUTOR)

# m6: flexible prior cases bins
m6 <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED +
              PRIOR_BIN
            | PROSECUTOR + GJURY_YEAR + COURT_F,
            data = dp_main, cluster = ~PROSECUTOR)

# m6b: flexible cases bins + sex/age
m6b <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED +
               PRIOR_BIN + FEMALE + AGE + I(AGE^2)
             | PROSECUTOR + GJURY_YEAR + COURT_F,
             data = dp_main, cluster = ~PROSECUTOR)

# m7a: flexible conv/def bins (no sex/age)
m7a <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED +
               PRIOR_CONV_BIN + PRIOR_DEF_BIN
             | PROSECUTOR + GJURY_YEAR + COURT_F,
             data = dp_main, cluster = ~PROSECUTOR)

# m7: flexible conv/def bins + sex/age
m7 <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED +
              PRIOR_CONV_BIN + PRIOR_DEF_BIN + FEMALE + AGE + I(AGE^2)
            | PROSECUTOR + GJURY_YEAR + COURT_F,
            data = dp_main, cluster = ~PROSECUTOR)

# m8: all flexible bins + sex/age (kitchen sink prior record)
m8 <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED +
              PRIOR_BIN + PRIOR_CONV_BIN + PRIOR_DEF_BIN + FEMALE + AGE + I(AGE^2)
            | PROSECUTOR + GJURY_YEAR + COURT_F,
            data = dp_main, cluster = ~PROSECUTOR)

# m8x: m8 + experience interactions (main experience-gap model)
m8x <- feols(DEFERRED_GRANT ~ BLACK + LATINO +
               BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
               OUTTAKE_CASE_N100 + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED +
               PRIOR_BIN + PRIOR_CONV_BIN + PRIOR_DEF_BIN + FEMALE + AGE + I(AGE^2)
             | PROSECUTOR + GJURY_YEAR + COURT_F,
             data = dp_main, cluster = ~PROSECUTOR)

# ── 9. Summary tables ─────────────────────────────────────────────────────────

message("\n── Gap across prior-record specifications ──")
etable(m3, m4, m5, m6, m7, m8, keep = "BLACK|LATINO",
       headers = c("No prior rec", "Linear cases", "Linear conv/def",
                   "Flexible cases", "Conv/def + sex/age", "All bins + sex/age"))

message("\n── Experience interaction model (m8x) ──")
summary(m8x)

# ── 10. Export LaTeX tables ───────────────────────────────────────────────────

etable(m3, m4, m5, m6, m7, m8, tex = TRUE,
       file = file.path(DATA_DIR, "table_gap_controls.tex"))
message("Saved: table_gap_controls.tex")

etable(m8x, tex = TRUE,
       file = file.path(DATA_DIR, "table_experience.tex"))
message("Saved: table_experience.tex")
