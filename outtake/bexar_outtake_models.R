# Bexar County — Outtake Prosecutor Regression Models
# Requires: bexar_prosecutor_panel_1990_2015.parquet
# install.packages(c("tidyverse", "arrow", "broom", "fixest"))
#
# Key outcome: DEFERRED
# Key exposure: OUTTAKE_CASE_N100 × RACE interactions
# Prosecutor = outtake (disposition-stage) prosecutor
#
# Output: bexar_outtake_model_results.csv

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"

library(tidyverse)
library(arrow)
library(broom)
library(fixest)

dp_raw <- read_parquet(file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))

# ── Build case-level outtake frame ────────────────────────────────────────────
# One row per case (keep most serious charge when multiple rows per case)
# Restrict to intake prosecutors first observed 1991+ (left-censoring)

fresh_prosecutors <- dp_raw |>
  group_by(`INTAKE-PROSECUTOR`) |>
  summarise(first_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop") |>
  filter(first_year >= 1991) |>
  pull(`INTAKE-PROSECUTOR`)

offense_order <- c("F1", "F2", "F3", "FS")

dp_out_case <- dp_raw |>
  filter(`INTAKE-PROSECUTOR` %in% fresh_prosecutors,
         !is.na(`OUTTAKE-PROSECUTOR`),
         `RACE-LABEL` %in% c("Black", "White", "Latino"),
         `OFFENSE-CLASS` %in% offense_order) |>
  mutate(OFFENSE_RANK = match(`OFFENSE-CLASS`, offense_order)) |>
  arrange(`CASE-CAUSE-NBR`, OFFENSE_RANK) |>
  distinct(`CASE-CAUSE-NBR`, .keep_all = TRUE) |>
  select(-OFFENSE_RANK)

# ── Derive variables ──────────────────────────────────────────────────────────

dp_out_case <- dp_out_case |>
  mutate(
    BLACK    = as.integer(`RACE-LABEL` == "Black"),
    LATINO   = as.integer(`RACE-LABEL` == "Latino"),
    RACE     = fct_relevel(`RACE-LABEL`, "White"),
    OFFENSE_TYPE = fct_relevel(`OFFENSE-CLASS`, "F3"),
    OFFENSE_CATEGORY2 = case_when(
      str_detect(`OFFENSE-DESC`, "POSS CS|POSS W/I DEL CS|POSS W/INT DEL CS|MAN/DEL CS|DEL CS|POSS MARIJ") ~ "Drug",
      str_detect(`OFFENSE-DESC`, "BURGLARY|BURG HAB|BURG VEHICLE") ~ "Burglary",
      str_detect(`OFFENSE-DESC`, "EVADING ARREST") ~ "Evading",
      str_detect(`OFFENSE-DESC`, "MURDER|HOMICIDE|MANSLAUGHTER") ~ "Homicide",
      str_detect(`OFFENSE-DESC`, "AGG ASSLT|ASSLT|INJURY TO CHILD|RETALIATION") ~ "Assault",
      str_detect(`OFFENSE-DESC`, "FORG|CREDIT/DEBIT|FRAUD|THEFT|UNAUTH USE VEH|CRIM MISCH") ~ "Property",
      str_detect(`OFFENSE-DESC`, "DWI|DRIV WHILE INTOX") ~ "DWI",
      str_detect(`OFFENSE-DESC`, "SEX|RAPE|INDECENCY|SEXUAL") ~ "Sex",
      str_detect(`OFFENSE-DESC`, "WEAPON|WPN|CARRY") ~ "Weapon",
      TRUE ~ "Other"
    ),
    OFFENSE_CATEGORY2 = fct_relevel(OFFENSE_CATEGORY2, "Other"),
    APPOINTED    = as.integer(`ATTORNEY-TYPE` == "Appointed"),
    CASE_YEAR_FE = factor(`CASE-YEAR`),
    PROSECUTOR   = factor(`OUTTAKE-PROSECUTOR`)
  )

# Outtake experience: running case count per outtake prosecutor ordered by CASE-DATE
dp_out_case <- dp_out_case |>
  arrange(`OUTTAKE-PROSECUTOR`, `CASE-DATE`) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  mutate(OUTTAKE_CASE_N = row_number()) |>
  ungroup() |>
  mutate(OUTTAKE_CASE_N100 = OUTTAKE_CASE_N / 100)

# Defendant career sequence (proxy for prior record)
def_seq <- dp_raw |>
  arrange(SID, `CASE-DATE`) |>
  group_by(SID) |>
  mutate(DEFENDANT_CASE_N = row_number()) |>
  ungroup() |>
  select(`CASE-CAUSE-NBR`, DEFENDANT_CASE_N)

dp_out_case <- dp_out_case |>
  left_join(def_seq, by = "CASE-CAUSE-NBR")

message("Outtake sample: ", nrow(dp_out_case), " cases, ",
        n_distinct(dp_out_case$`OUTTAKE-PROSECUTOR`), " outtake prosecutors")

# ── Helpers ───────────────────────────────────────────────────────────────────

run_feglm <- function(formula, data, fe_vars, label) {
  message("\n", strrep("=", 60))
  message("Model: ", label, "  (N = ", nrow(data), ")")
  fit <- tryCatch(
    feglm(formula, data = data, fixef = fe_vars, family = binomial(),
          cluster = fe_vars[1],
          fixef.tol = 1e-4, fixef.iter = 50, iter = 50),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  if (is.null(fit)) return(tibble(model = label))
  print(summary(fit))
  tidy(fit, conf.int = TRUE) |> mutate(OR = exp(estimate), model = label)
}

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

df <- dp_out_case |> filter(!is.na(OFFENSE_CATEGORY2))

# ── Spec A: Offense + attorney controls (col 1) ───────────────────────────────

specA <- run_logit(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
  df, "SpecA_OffenseOnly")

# ── Spec B: + case year FE (col 2) ───────────────────────────────────────────

specB <- run_feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
  df, fe_vars = "CASE_YEAR_FE", "SpecB_YearFE")

# ── Spec C: Prosecutor FE (col 3) ────────────────────────────────────────────

specC <- run_feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
  df, fe_vars = "PROSECUTOR", "SpecC_ProsecutorFE")

# ── Spec D: Prosecutor FE + prior record proxy (col 4) ───────────────────────

specD <- run_feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED + DEFENDANT_CASE_N,
  df, fe_vars = "PROSECUTOR", "SpecD_WithPriors")

# ── Within-offense-type (robustness) ─────────────────────────────────────────

m7_list <- map(c("F1", "F2", "F3", "FS"), function(ot) {
  sub <- df |> filter(`OFFENSE-CLASS` == ot)
  tryCatch(
    run_logit(
      DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
        BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
        OFFENSE_CATEGORY2 + APPOINTED,
      sub, paste0("M7_", ot, "_only")),
    error = function(e) tibble(model = paste0("M7_", ot, "_SKIPPED"))
  )
})
m7 <- bind_rows(m7_list)

# ── DA era split (robustness) ─────────────────────────────────────────────────

outtake_start <- dp_out_case |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  summarise(start_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop")

dp_out_case <- dp_out_case |>
  left_join(outtake_start, by = "OUTTAKE-PROSECUTOR") |>
  mutate(DA_ERA = ifelse(start_year <= 1998, "Hilbig", "Reed"))

fit_era <- function(era_label) {
  sub <- dp_out_case |>
    filter(DA_ERA == era_label, !is.na(OFFENSE_CATEGORY2))
  run_feglm(
    DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
      BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
      OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
    sub, fe_vars = "PROSECUTOR", paste0("Era_", era_label))
}

era_hilbig <- fit_era("Hilbig")
era_reed   <- fit_era("Reed")

# ── Defendant age robustness ──────────────────────────────────────────────────

df_age <- dp_out_case |>
  filter(!is.na(BIRTHDATE), !is.na(OFFENSE_CATEGORY2)) |>
  mutate(DEF_AGE = as.numeric(difftime(`CASE-DATE`, BIRTHDATE, units = "days")) / 365.25) |>
  filter(DEF_AGE >= 16, DEF_AGE <= 80)

specC_age <- run_feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED + DEF_AGE,
  df_age, fe_vars = "PROSECUTOR", "SpecC_DefAge")

# ── Saturated offense FE (robustness) ────────────────────────────────────────
# Replace 9-category OFFENSE_CATEGORY2 with all unique charge descriptions

dp_out_case <- dp_out_case |>
  mutate(OFFENSE_DESC_FE = factor(`OFFENSE-DESC`))

specC_satFE <- run_feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + APPOINTED,
  df |> mutate(OFFENSE_DESC_FE = factor(`OFFENSE-DESC`)),
  fe_vars = c("PROSECUTOR", "OFFENSE_DESC_FE"), "SpecC_SaturatedOffenseFE")

# ── M8: Deferred conditional on any plea ─────────────────────────────────────

df_plea <- dp_out_case |>
  filter(`GUILTY-PLEA` == 1 | DEFERRED == 1, !is.na(OFFENSE_CATEGORY2))

m8 <- run_feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
  df_plea, fe_vars = "PROSECUTOR", "M8_DeferredConditionalOnPlea")

# ── M9: Straight conviction via plea (mirror image) ──────────────────────────

m9 <- run_feglm(
  STRAIGHT_CONVICTION ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
  df_plea |> mutate(STRAIGHT_CONVICTION = as.integer(`GUILTY-PLEA` == 1 & DEFERRED == 0)),
  fe_vars = "PROSECUTOR", "M9_StraightConviction")

# ── Export ────────────────────────────────────────────────────────────────────

all_results <- bind_rows(specA, specB, specC, specD, m7,
                         era_hilbig, era_reed, specC_age,
                         specC_satFE, m8, m9) |>
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high, OR)

write_csv(all_results, file.path(DATA_DIR, "bexar_outtake_model_results.csv"))
message("\nSaved: bexar_outtake_model_results.csv")

message("\n── KEY COEFFICIENTS: BLACK × OUTTAKE_CASE_N100 ──")
all_results |>
  filter(str_detect(term, "BLACK.*OUTTAKE_CASE_N100|OUTTAKE_CASE_N100.*BLACK")) |>
  select(model, term, estimate, std.error, p.value, OR) |>
  print(n = Inf)

message("\n── KEY COEFFICIENTS: LATINO × OUTTAKE_CASE_N100 ──")
all_results |>
  filter(str_detect(term, "LATINO.*OUTTAKE_CASE_N100|OUTTAKE_CASE_N100.*LATINO")) |>
  select(model, term, estimate, std.error, p.value, OR) |>
  print(n = Inf)
