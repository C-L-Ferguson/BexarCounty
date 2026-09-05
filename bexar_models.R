# Bexar County — Regression Models
# Requires: bexar_panel_1990_2021.parquet (from bexar_clean.R)
# install.packages(c("tidyverse", "arrow", "broom", "fixest"))
#
# Models:
#   1. Baseline logit: DEFERRED ~ RACE + OFFENSE_CLASS + SEX + CASE_YEAR
#   2. Add attorney type
#   3. Prosecutor fixed effects (>= 50 cases)  [uses fixest::feglm]
#   4. DiD: DEFERRED ~ RACE * POST2019 + controls
#   5. Robustness: GUILTY-PLEA as outcome
#   OLS: log sentence days | convicted
#
# Output: bexar_model_results.csv, bexar_ols_sentence.csv

library(tidyverse)
library(arrow)
library(broom)

# fixest gives fast FE logit; fall back to stats::glm if not installed
has_fixest <- requireNamespace("fixest", quietly = TRUE)
if (!has_fixest) {
  message("NOTE: install fixest for Model 3 (prosecutor FE). ",
          "Running without it for now.")
}

panel <- read_parquet("bexar_panel_1990_2021.parquet")

# ── Analysis frame ────────────────────────────────────────────────────────────

df <- panel |>
  filter(
    `RACE-LABEL`    %in% c("Black", "White", "Latino"),
    `SEX-LABEL`     %in% c("M", "F"),
    `OFFENSE-CLASS` %in% c("F1", "F2", "F3", "FS", "Other Felony")
  ) |>
  mutate(
    # Reference categories: White, F3, Hired
    RACE     = fct_relevel(`RACE-LABEL`,    "White"),
    OFFENSE  = fct_relevel(`OFFENSE-CLASS`, "F3"),
    SEX      = factor(`SEX-LABEL`),
    ATTY     = fct_relevel(`ATTORNEY-TYPE`, "Hired"),
    POST2019 = as.integer(`CASE-YEAR` >= 2019),
    YEAR_CTR = `CASE-YEAR` - 2005  # center for interpretability
  )

d_all  <- df |> drop_na(DEFERRED, YEAR_CTR)
d_atty <- df |>
  filter(`ATTORNEY-TYPE` %in% c("Appointed", "Hired")) |>
  drop_na(DEFERRED, YEAR_CTR)

# ── Helper: run logit, tidy, attach model label ───────────────────────────────

run_logit <- function(formula, data, label) {
  message("\n", strrep("=", 60))
  message("Model: ", label, "  (N = ", nrow(data), ")")
  fit <- glm(formula, data = data, family = binomial(link = "logit"))
  print(summary(fit))
  tidy(fit, conf.int = TRUE) |>
    mutate(OR = exp(estimate), model = label)
}

# ── Model 1: Baseline ─────────────────────────────────────────────────────────

m1 <- run_logit(
  DEFERRED ~ RACE + OFFENSE + SEX + YEAR_CTR,
  d_all,
  "M1_Baseline")

# ── Model 2: Add attorney type ────────────────────────────────────────────────

m2 <- run_logit(
  DEFERRED ~ RACE + OFFENSE + SEX + ATTY + YEAR_CTR,
  d_atty,
  "M2_AttorneyType")

# ── Model 3: Prosecutor fixed effects ─────────────────────────────────────────

high_vol <- d_atty |>
  count(`INTAKE-PROSECUTOR`) |>
  filter(n >= 50) |>
  pull(`INTAKE-PROSECUTOR`)

d_fe <- d_atty |>
  filter(`INTAKE-PROSECUTOR` %in% high_vol) |>
  mutate(PROSECUTOR = factor(`INTAKE-PROSECUTOR`))

message("\n", strrep("=", 60))
message("Model 3 — Prosecutor FE  (", length(high_vol), " prosecutors, N = ", nrow(d_fe), ")")

if (has_fixest) {
  m3_fit <- fixest::feglm(
    DEFERRED ~ RACE + OFFENSE + SEX + ATTY + YEAR_CTR | PROSECUTOR,
    data = d_fe, family = "logit")
  print(summary(m3_fit))
  m3 <- tidy(m3_fit, conf.int = TRUE) |>
    mutate(OR = exp(estimate), model = "M3_ProsecutorFE")
} else {
  m3_fit <- glm(
    DEFERRED ~ RACE + OFFENSE + SEX + ATTY + YEAR_CTR + PROSECUTOR,
    data = d_fe, family = binomial)
  print(summary(m3_fit))
  m3 <- tidy(m3_fit, conf.int = TRUE) |>
    filter(!str_starts(term, "PROSECUTOR")) |>  # suppress ~hundreds of FE rows
    mutate(OR = exp(estimate), model = "M3_ProsecutorFE")
}

# ── Model 4: DiD (race × POST2019) ────────────────────────────────────────────

m4 <- run_logit(
  DEFERRED ~ RACE * POST2019 + OFFENSE + SEX + ATTY,
  d_atty,
  "M4_DiD_RaceXPost2019")

# ── Model 5: Guilty plea (robustness) ────────────────────────────────────────

m5 <- run_logit(
  `GUILTY-PLEA` ~ RACE + OFFENSE + SEX + ATTY + YEAR_CTR,
  d_atty,
  "M5_GuiltyPlea")

# ── Export coefficient table ──────────────────────────────────────────────────

results <- bind_rows(m1, m2, m3, m4, m5) |>
  select(model, term, estimate, std.error, statistic, p.value,
         conf.low, conf.high, OR)

write_csv(results, "bexar_model_results.csv")
message("\nSaved: bexar_model_results.csv")

# ── OLS: log sentence days | convicted ────────────────────────────────────────

message("\n", strrep("=", 60))
convicted <- d_atty |>
  filter(CONVICTED == 1, !is.na(`SENTENCE-DAYS`), `SENTENCE-DAYS` > 0) |>
  mutate(LOG_SENTENCE = log(`SENTENCE-DAYS`))

message("OLS — log sentence days | convicted  (N = ", nrow(convicted), ")")
ols <- lm(LOG_SENTENCE ~ RACE + OFFENSE + SEX + ATTY + YEAR_CTR, data = convicted)
print(summary(ols))

tidy(ols, conf.int = TRUE) |>
  mutate(model = "OLS_LogSentence") |>
  write_csv("bexar_ols_sentence.csv")
message("Saved: bexar_ols_sentence.csv")
