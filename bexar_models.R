# Bexar County — Prosecutor Learning Curve Regression Models
# Requires: bexar_prosecutor_panel_1990_2015.parquet (from bexar_prosecutor_panel.R)
# install.packages(c("tidyverse", "arrow", "broom", "fixest"))
#
# Key outcome: DEFERRED
# Key exposure: PROSECUTOR_CASE_N × RACE interactions
# The interaction coefficient tests whether the White-Black gap widens with experience.
#
# Models M1–M9 per handoff v6 Section 5.
# Output: bexar_model_results.csv, bexar_ols_sentence.csv

library(tidyverse)
library(arrow)
library(broom)

has_fixest <- requireNamespace("fixest", quietly = TRUE)
if (!has_fixest) message("NOTE: install fixest for M3/M5/M7/M8/M9. Falling back to glm.")

dp <- read_parquet("bexar_prosecutor_panel_1990_2015.parquet")

# ── Analysis frame ────────────────────────────────────────────────────────────

df <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White", "Latino")) |>
  mutate(
    BLACK  = as.integer(`RACE-LABEL` == "Black"),
    LATINO = as.integer(`RACE-LABEL` == "Latino"),
    RACE   = fct_relevel(`RACE-LABEL`, "White"),     # reference: White
    OFFENSE_TYPE = fct_relevel(`OFFENSE-CLASS`, "F3"), # reference: F3
    SEX    = factor(`SEX-LABEL`),
    APPOINTED = as.integer(`ATTORNEY-TYPE` == "Appointed"),
    COURT  = factor(COURT),
    CASE_YEAR_FE = factor(`CASE-YEAR`),
    PROSECUTOR   = factor(`INTAKE-PROSECUTOR`),
    DA_HIRE = factor(DA_AT_HIRE),
    BOND_LOG = log(`BOND-AMOUNT` + 1),   # NA where bond missing/sentinel
    # Normalize experience within prosecutor (0–1) for interpretable coefficients
    CASE_N_NORM = PROSECUTOR_CASE_N / max(PROSECUTOR_CASE_N, na.rm = TRUE)
  )

df_atty <- df |> filter(`ATTORNEY-TYPE` %in% c("Appointed", "Hired"))
df_felony <- df |> filter(`OFFENSE-CLASS` %in% c("F1", "F2", "F3", "FS"))

# Helper: run logit, return tidy table
run_logit <- function(formula, data, label) {
  message("\n", strrep("=", 60))
  message("Model: ", label, "  (N = ", nrow(data), ")")
  fit <- tryCatch(
    glm(formula, data = data, family = binomial),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  if (is.null(fit)) return(tibble(model = label))
  print(summary(fit))
  tidy(fit, conf.int = TRUE) |> mutate(OR = exp(estimate), model = label)
}

run_feglm <- function(formula, data, fe, label) {
  message("\n", strrep("=", 60))
  message("Model: ", label, "  (N = ", nrow(data), ")")
  if (!has_fixest) {
    message("  fixest not installed — skipping")
    return(tibble(model = label))
  }
  fit <- tryCatch(
    fixest::feglm(formula, data = data, fixef = fe, family = "logit"),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  if (is.null(fit)) return(tibble(model = label))
  print(summary(fit))
  tidy(fit, conf.int = TRUE) |> mutate(OR = exp(estimate), model = label)
}

# ── M1: Baseline — no controls ────────────────────────────────────────────────
# Key: interaction BLACK*PROSECUTOR_CASE_N — positive = gap widens with experience

m1 <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N,
  df, "M1_Baseline")

# ── M2: Add offense type and category FE ──────────────────────────────────────

m2 <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY,
  df |> filter(!is.na(OFFENSE_CATEGORY)),
  "M2_OffenseFE")

# ── M3: Add court FE and case year FE ─────────────────────────────────────────

if (has_fixest) {
  m3 <- run_feglm(
    DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
      BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
      OFFENSE_TYPE + OFFENSE_CATEGORY,
    df |> filter(!is.na(OFFENSE_CATEGORY), !is.na(COURT), !is.na(CASE_YEAR_FE)),
    fe = c("COURT", "CASE_YEAR_FE"),
    "M3_CourtYearFE")
} else {
  m3 <- run_logit(
    DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
      BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
      OFFENSE_TYPE + OFFENSE_CATEGORY + COURT + CASE_YEAR_FE,
    df |> filter(!is.na(OFFENSE_CATEGORY), !is.na(COURT)),
    "M3_CourtYearFE")
}

# ── M4: Full primary model ────────────────────────────────────────────────────
# Add attorney type. Bond: run with and without to check stability.

m4_no_bond <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED + COURT + CASE_YEAR_FE,
  df |> filter(!is.na(OFFENSE_CATEGORY), !is.na(COURT)),
  "M4_Full_NoBond")

m4_bond <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED + COURT + CASE_YEAR_FE + BOND_LOG,
  df |> filter(!is.na(OFFENSE_CATEGORY), !is.na(COURT), !is.na(BOND_LOG)),
  "M4_Full_WithBond")

# ── M5: Prosecutor fixed effects ──────────────────────────────────────────────
# NOTE: PROSECUTOR_FE and PROSECUTOR_CASE_N are collinear by construction.
# fixest handles this via within-group demeaning; interpret carefully.

if (has_fixest) {
  m5 <- run_feglm(
    DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
      BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
      OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED,
    df |> filter(!is.na(OFFENSE_CATEGORY)),
    fe = "PROSECUTOR",
    "M5_ProsecutorFE")
} else {
  message("\nM5 requires fixest — skipping")
  m5 <- tibble(model = "M5_ProsecutorFE_SKIPPED")
}

# ── M6: Appointed counsel only (key robustness) ───────────────────────────────

m6 <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + COURT + CASE_YEAR_FE,
  df |> filter(`ATTORNEY-TYPE` == "Appointed", !is.na(OFFENSE_CATEGORY), !is.na(COURT)),
  "M6_AppointedOnly")

# ── M7: Within offense type ───────────────────────────────────────────────────

m7_list <- map(c("F1", "F2", "F3", "FS"), function(ot) {
  sub <- df |> filter(`OFFENSE-CLASS` == ot, !is.na(OFFENSE_CATEGORY), !is.na(COURT))
  run_logit(
    DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
      BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
      OFFENSE_CATEGORY + APPOINTED + COURT + CASE_YEAR_FE,
    sub, paste0("M7_", ot, "_only"))
})
m7 <- bind_rows(m7_list)

# ── M8: Guilty plea robustness ────────────────────────────────────────────────

m8 <- run_logit(
  `GUILTY-PLEA` ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED + COURT + CASE_YEAR_FE,
  df |> filter(!is.na(OFFENSE_CATEGORY), !is.na(COURT)),
  "M8_GuiltyPlea")

# ── M9: DA cohort interaction ─────────────────────────────────────────────────

m9 <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    PROSECUTOR_CASE_N:DA_HIRE +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED + COURT + CASE_YEAR_FE,
  df |> filter(!is.na(OFFENSE_CATEGORY), !is.na(COURT)),
  "M9_DAcohort")

# ── Export key coefficient table ──────────────────────────────────────────────
# Primary table: interaction coefficients across M1–M5

all_results <- bind_rows(m1, m2, m3, m4_no_bond, m4_bond, m5, m6, m7, m8, m9) |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high, OR)

write_csv(all_results, "bexar_model_results.csv")
message("\nSaved: bexar_model_results.csv")

# Focal coefficients for progression table (M1 through M5)
message("\n── KEY COEFFICIENTS: BLACK × PROSECUTOR_CASE_N ──")
all_results |>
  filter(str_detect(term, "BLACK.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*BLACK")) |>
  select(model, term, estimate, std.error, p.value, OR) |>
  print(n = Inf)

message("\n── KEY COEFFICIENTS: LATINO × PROSECUTOR_CASE_N ──")
all_results |>
  filter(str_detect(term, "LATINO.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*LATINO")) |>
  select(model, term, estimate, std.error, p.value, OR) |>
  print(n = Inf)
