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
# Restrict to outtake prosecutors first observed 1991+ (left-censoring fix)
# Experience variable (OUTTAKE_CASE_N) is outtake-prosecutor-specific, so
# left-censoring must be applied to the outtake prosecutor, not intake.

fresh_outtake <- dp_raw |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  summarise(first_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop") |>
  filter(first_year >= 1991) |>
  pull(`OUTTAKE-PROSECUTOR`)

offense_order <- c("F1", "F2", "F3", "FS")

dp_out_case <- dp_raw |>
  filter(`OUTTAKE-PROSECUTOR` %in% fresh_outtake,
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
      str_detect(`OFFENSE-DESC`, "POSS CS|POSS W/I DEL CS|POSS W/INT DEL CS|MAN/DEL CS|DEL CS|POSS MARIJ|POSS COCAINE|POSS HEROIN|DEL COCAINE|DEL HEROIN|POSS METH|DEL METH") ~ "Drug",
      str_detect(`OFFENSE-DESC`, "BURGLARY|BURG HAB|BURG VEHICLE") ~ "Burglary",
      str_detect(`OFFENSE-DESC`, "ROBBERY|AGG ROB") ~ "Robbery",
      str_detect(`OFFENSE-DESC`, "EVADING ARREST|EVAD ARR") ~ "Evading",
      str_detect(`OFFENSE-DESC`, "MURDER|HOMICIDE|MANSLAUGHTER") ~ "Homicide",
      str_detect(`OFFENSE-DESC`, "AGG ASSLT|ASSLT|ASSAULT|INJURY TO CHILD|RETALIATION|DEADLY CONDUCT|ENDANGERING CHILD") ~ "Assault",
      str_detect(`OFFENSE-DESC`, "FORG|CREDIT/DEBIT|FRAUD|THEFT|UNAUTH USE VEH|UNAUTH USE OF VEH|CRIM MISCH|ARSON") ~ "Property",
      str_detect(`OFFENSE-DESC`, "DWI|DRIV WHILE INTOX|DRIVING WHILE INTOX|INTOXICATION") ~ "DWI",
      str_detect(`OFFENSE-DESC`, "SEX|RAPE|INDECENCY|SEXUAL|PROSTITUTION") ~ "Sex",
      str_detect(`OFFENSE-DESC`, "WEAPON|WPN|CARRY|FELON POSS FIREARM|FIREARM") ~ "Weapon",
      TRUE ~ "Other"
    ),
    OFFENSE_CATEGORY2 = fct_relevel(OFFENSE_CATEGORY2, "Other"),
    APPOINTED    = as.integer(`ATTORNEY-TYPE` == "Appointed"),
    CASE_YEAR_FE = factor(`CASE-YEAR`),
    PROSECUTOR   = factor(`OUTTAKE-PROSECUTOR`)
  )

# Outtake experience: running case count per outtake prosecutor from full caseload
# (all races, before race filter) so the count reflects true experience at the
# time of each case, not just experience with Black/Latino/White defendants.
pros_case_seq <- dp_raw |>
  filter(`OUTTAKE-PROSECUTOR` %in% fresh_outtake, !is.na(`OUTTAKE-PROSECUTOR`),
         !is.na(`CASE-DATE`)) |>
  mutate(OFFENSE_RANK = match(`OFFENSE-CLASS`, offense_order),
         OFFENSE_RANK = ifelse(is.na(OFFENSE_RANK), 99L, OFFENSE_RANK)) |>
  arrange(`CASE-CAUSE-NBR`, OFFENSE_RANK) |>
  distinct(`CASE-CAUSE-NBR`, .keep_all = TRUE) |>
  select(`CASE-CAUSE-NBR`, `OUTTAKE-PROSECUTOR`, `CASE-DATE`) |>
  arrange(`OUTTAKE-PROSECUTOR`, `CASE-DATE`) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  mutate(OUTTAKE_CASE_N = row_number()) |>
  ungroup() |>
  select(`CASE-CAUSE-NBR`, OUTTAKE_CASE_N)

dp_out_case <- dp_out_case |>
  left_join(pros_case_seq, by = "CASE-CAUSE-NBR") |>
  mutate(OUTTAKE_CASE_N100 = OUTTAKE_CASE_N / 100)

# Defendant career sequence (proxy for prior record)
# Use full raw CSV history (all available years) so cumulative case count is
# not left-censored at 1990. A defendant with cases before 1990 will have
# a higher DEFENDANT_CASE_N than if we only counted from dp_raw.
raw_files <- list.files(DATA_DIR, pattern = "^DC_cjjorad_", full.names = TRUE)
message("Building defendant case sequence from ", length(raw_files), " raw files...")

def_seq <- map_dfr(raw_files, function(f) {
  read_csv(f, col_types = cols(.default = "c"), show_col_types = FALSE) |>
    select(any_of(c("SID", "CASE-CAUSE-NBR", "CASE-DATE")))
}) |>
  filter(!is.na(SID), SID != "", !is.na(`CASE-CAUSE-NBR`)) |>
  mutate(`CASE-DATE` = as.Date(`CASE-DATE`)) |>
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

# ── Appointed counsel only (robustness) ──────────────────────────────────────

specC_appointed <- run_feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2,
  df |> filter(APPOINTED == 1),
  fe_vars = "PROSECUTOR", "SpecC_AppointedOnly")

# ── Completed careers only (right-censoring robustness) ──────────────────────
# Restrict to prosecutors whose last observed case is before 2015, i.e. those
# who left the office before the sample ends and have fully observed careers.

completed_pros <- dp_out_case |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  summarise(last_year = max(`CASE-YEAR`, na.rm = TRUE), .groups = "drop") |>
  filter(last_year < 2015) |>
  pull(`OUTTAKE-PROSECUTOR`)

specD_completed <- run_feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED + DEFENDANT_CASE_N,
  df |> filter(`OUTTAKE-PROSECUTOR` %in% completed_pros),
  fe_vars = "PROSECUTOR", "SpecD_CompletedCareers")

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
                         specC_appointed, specD_completed, m8, m9) |>
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
