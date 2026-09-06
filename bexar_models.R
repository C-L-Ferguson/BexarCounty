# Bexar County — Prosecutor Learning Curve Regression Models
# Requires: bexar_prosecutor_panel_1990_2015.parquet (from bexar_prosecutor_panel.R)
# install.packages(c("tidyverse", "arrow", "broom", "fixest"))
#
# Key outcome: DEFERRED
# Key exposure: PROSECUTOR_CASE_N × RACE interactions
# The interaction coefficient tests whether the White-Black gap widens with experience.
#
# Main table specs: SpecA (offense controls), SpecB (+ year FE), SpecC (prosecutor FE)
# Robustness: M1 (baseline), M2 (offense FE), M6 (appointed only),
#             M7 (within offense type), M8 (plea-conditional), M9 (straight conviction)
# Output: bexar_model_results.csv

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"

library(tidyverse)
library(arrow)
library(broom)
library(fixest)

dp <- read_parquet(file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))

# ── Analysis frame ────────────────────────────────────────────────────────────

df <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White", "Latino")) |>
  mutate(
    BLACK        = as.integer(`RACE-LABEL` == "Black"),
    LATINO       = as.integer(`RACE-LABEL` == "Latino"),
    RACE         = fct_relevel(`RACE-LABEL`, "White"),
    OFFENSE_TYPE = fct_relevel(`OFFENSE-CLASS`, "F3"),
    SEX          = factor(`SEX-LABEL`),
    APPOINTED    = as.integer(`ATTORNEY-TYPE` == "Appointed"),
    COURT        = factor(COURT),
    CASE_YEAR_FE = factor(`CASE-YEAR`),
    PROSECUTOR   = factor(`INTAKE-PROSECUTOR`),
    BOND_LOG     = log(`BOND-AMOUNT` + 1)
  )

# Drop prosecutors first observed in 1990 (left-censored career counts)
fresh_prosecutors <- df |>
  group_by(`INTAKE-PROSECUTOR`) |>
  summarise(first_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop") |>
  filter(first_year >= 1991) |>
  pull(`INTAKE-PROSECUTOR`)

df <- df |> filter(`INTAKE-PROSECUTOR` %in% fresh_prosecutors)

message("Analysis sample: ", nrow(df), " cases, ",
        n_distinct(df$`INTAKE-PROSECUTOR`), " prosecutors (1991+ only)")

# Helper: run logit, return tidy table
run_logit <- function(formula, data, label) {
  message("\n", strrep("=", 60))
  message("Model: ", label, "  (N = ", nrow(data), ")")
  fit <- tryCatch(
    glm(formula, data = data, family = binomial),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  if (is.null(fit)) return(tibble(model = label))
  tidy(fit, conf.int = TRUE) |> mutate(OR = exp(estimate), model = label)
}

run_feglm <- function(formula, data, fe_vars, label) {
  message("\n", strrep("=", 60))
  message("Model: ", label, "  (N = ", nrow(data), ")")
  fit <- tryCatch(
    feglm(formula, data = data, fixef = fe_vars, family = binomial(),
          fixef.tol = 1e-4, fixef.iter = 50, iter = 50),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  if (is.null(fit)) return(tibble(model = label))
  print(summary(fit))
  tidy(fit, conf.int = TRUE) |> mutate(OR = exp(estimate), model = label)
}

# ── M1: Baseline — no controls ────────────────────────────────────────────────

m1 <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N,
  df, "M1_Baseline")

# ── M2: Offense type and category controls ────────────────────────────────────

m2 <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY,
  df |> filter(!is.na(OFFENSE_CATEGORY)),
  "M2_OffenseFE")

# ── Spec A: Clean baseline — offense + attorney controls (main table col 1) ───

specA <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED,
  df |> filter(!is.na(OFFENSE_CATEGORY)),
  "SpecA_OffenseOnly")

# ── Spec B: Add case year FE (main table col 2) ───────────────────────────────
# Note: CASE_YEAR_FE is collinear with PROSECUTOR_CASE_N by construction;
# interaction surviving here is a conservative test.
# Uses feglm for speed (glm + tidy hangs on 24 year FE dummies).

specB <- run_feglm(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED,
  df |> filter(!is.na(OFFENSE_CATEGORY)),
  fe_vars = "CASE_YEAR_FE",
  "SpecB_YearFE")

# ── Spec C: Prosecutor fixed effects (main table col 3) ───────────────────────
# Within-prosecutor identification; answers selection critique.

specC <- run_feglm(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED,
  df |> filter(!is.na(OFFENSE_CATEGORY)),
  fe_vars = "PROSECUTOR",
  "SpecC_ProsecutorFE")

# ── M6: Appointed counsel only (robustness) ───────────────────────────────────

m6 <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY,
  df |> filter(`ATTORNEY-TYPE` == "Appointed", !is.na(OFFENSE_CATEGORY)),
  "M6_AppointedOnly")

# ── M7: Within offense type (robustness) ──────────────────────────────────────

m7_list <- map(c("F1", "F2", "F3", "FS"), function(ot) {
  sub <- df |> filter(`OFFENSE-CLASS` == ot, !is.na(OFFENSE_CATEGORY))
  tryCatch(
    run_logit(
      DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
        BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
        OFFENSE_CATEGORY + APPOINTED + CASE_YEAR_FE,
      sub, paste0("M7_", ot, "_only")),
    error = function(e) {
      message("  M7_", ot, " skipped: ", e$message)
      tibble(model = paste0("M7_", ot, "_only_SKIPPED"))
    }
  )
})
m7 <- bind_rows(m7_list)

# ── M8: Deferred conditional on any plea (robustness) ────────────────────────

df_plea <- df |> filter(`GUILTY-PLEA` == 1 | DEFERRED == 1, !is.na(OFFENSE_CATEGORY))

m8 <- run_logit(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED + CASE_YEAR_FE,
  df_plea,
  "M8_DeferredConditionalOnPlea")

# ── M9: Straight conviction via plea (robustness) ─────────────────────────────

m9 <- run_logit(
  STRAIGHT_CONVICTION ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED + CASE_YEAR_FE,
  df_plea |> mutate(STRAIGHT_CONVICTION = as.integer(`GUILTY-PLEA` == 1 & DEFERRED == 0)),
  "M9_StraightConviction")

# ── Export ────────────────────────────────────────────────────────────────────

all_results <- bind_rows(m1, m2, specA, specB, specC, m6, m7, m8, m9) |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high, OR)

write_csv(all_results, file.path(DATA_DIR, "bexar_model_results.csv"))
message("\nSaved: bexar_model_results.csv")

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
