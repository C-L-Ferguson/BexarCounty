############################################################
# BEXAR COUNTY: end-to-end script that reproduces the numbers
#
# Built from the code you already ran, in the order you ran it.
# Inputs (made by your cleaning script):
#   bexar_panel_1990_2021.parquet   and   bexar_cleaned.parquet (all years)
# Run it top to bottom. "EXPECT" comments give the numbers you got.
############################################################
library(tidyverse); library(arrow); library(fixest); library(lubridate)

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"    # <-- CHANGE if needed (carol vs carolineferguson)
OUT_DIR  <- file.path(DATA_DIR, "final_output"); dir.create(OUT_DIR, showWarnings = FALSE)

# Your numbers were produced with the first-observed year taken from the 1990-2021 PANEL.
# Set TRUE to take it from ALL years instead (stricter left-censoring rule; counts shift slightly).
FIRST_FROM_ALL_YEARS <- FALSE

# ---------- 1. LOAD ----------
dp_raw  <- read_parquet(file.path(DATA_DIR, "bexar_panel_1990_2021.parquet"))
all_raw <- read_parquet(file.path(DATA_DIR, "bexar_cleaned.parquet"))
cat("Panel rows:", nrow(dp_raw), "(EXPECT 330,778) | All-years rows:", nrow(all_raw), "(EXPECT 385,097)\n")

dp_raw <- dp_raw |> mutate(GJURY_DATE = mdy(`G-JURY-DATE`), GJURY_YEAR = year(GJURY_DATE))

# ---------- 2. PROSECUTORS FIRST OBSERVED IN 1991 OR LATER (by grand-jury date) ----------
first_src <- if (FIRST_FROM_ALL_YEARS) {
  all_raw |> mutate(GJURY_DATE = mdy(`G-JURY-DATE`), GJURY_YEAR = year(GJURY_DATE))
} else dp_raw

fresh_outtake <- first_src |>
  filter(!is.na(`OUTTAKE-PROSECUTOR`), `OUTTAKE-PROSECUTOR` != "",
         !is.na(GJURY_DATE), GJURY_YEAR >= 1950, GJURY_YEAR <= 2021) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  summarise(first_gjury_year = min(GJURY_YEAR, na.rm = TRUE), .groups = "drop") |>
  filter(first_gjury_year >= 1991) |>
  pull(`OUTTAKE-PROSECUTOR`)
cat("Prosecutors starting 1991+:", length(fresh_outtake), "(EXPECT 614 with FIRST_FROM_ALL_YEARS = FALSE)\n")

# ---------- 3. EXPERIENCE: prior cases handled, ordered by grand-jury date ----------
offense_order <- c("F1", "F2", "F3", "FS")

pros_case_seq <- dp_raw |>
  filter(`OUTTAKE-PROSECUTOR` %in% fresh_outtake, !is.na(`OUTTAKE-PROSECUTOR`), !is.na(GJURY_DATE)) |>
  mutate(OFFENSE_RANK = coalesce(match(`OFFENSE-CLASS`, offense_order), 99L)) |>
  arrange(`CASE-CAUSE-NBR`, OFFENSE_RANK) |>
  distinct(`CASE-CAUSE-NBR`, .keep_all = TRUE) |>
  arrange(`OUTTAKE-PROSECUTOR`, GJURY_DATE) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  mutate(OUTTAKE_CASE_N = row_number()) |>
  ungroup() |>
  select(`CASE-CAUSE-NBR`, OUTTAKE_CASE_N)
cat("Cases in experience sequence:", nrow(pros_case_seq), "(EXPECT 128,235)\n")

# ---------- 4. ANALYTIC SAMPLE ----------
dp_out_case <- dp_raw |>
  filter(`OUTTAKE-PROSECUTOR` %in% fresh_outtake, !is.na(`OUTTAKE-PROSECUTOR`),
         `RACE-LABEL` %in% c("Black", "White", "Latino"),
         `OFFENSE-CLASS` %in% offense_order,
         !is.na(GJURY_DATE), GJURY_YEAR >= 1950, GJURY_YEAR <= 2021) |>
  mutate(OFFENSE_RANK = match(`OFFENSE-CLASS`, offense_order)) |>
  arrange(`CASE-CAUSE-NBR`, OFFENSE_RANK) |>
  distinct(`CASE-CAUSE-NBR`, .keep_all = TRUE) |>
  select(-OFFENSE_RANK) |>
  mutate(
    # PRIMARY OUTCOME: deferred-adjudication GRANT at the plea (judgment-level)
    DEFERRED_GRANT = as.integer(str_detect(replace_na(`JUDGEMENT-DESC`, ""),
                                           regex("DEFR ADJUD", ignore_case = TRUE))),
    BLACK        = as.integer(`RACE-LABEL` == "Black"),
    LATINO       = as.integer(`RACE-LABEL` == "Latino"),
    OFFENSE_TYPE = fct_relevel(factor(`OFFENSE-CLASS`), "F3"),
    APPOINTED    = as.integer(`ATTORNEY-TYPE` == "Appointed"),
    GJURY_YEAR_FE = factor(GJURY_YEAR),
    PROSECUTOR   = factor(`OUTTAKE-PROSECUTOR`)) |>
  left_join(pros_case_seq, by = "CASE-CAUSE-NBR") |>
  mutate(OUTTAKE_CASE_N100 = OUTTAKE_CASE_N / 100)
cat("Analytic sample:", nrow(dp_out_case), "(EXPECT 126,582) | prosecutors:", n_distinct(dp_out_case$PROSECUTOR), "(EXPECT 604)\n")

# Diagnostic: grant rate by grand-jury year should be flat (about 22-30%) with no late collapse
dp_out_case |> group_by(GJURY_YEAR) |>
  summarise(grant_rate = mean(DEFERRED_GRANT, na.rm = TRUE), n = n(), .groups = "drop") |>
  filter(GJURY_YEAR >= 1995) |> print(n = 30)

# ---------- 5. MAIN WINDOW 1997-2019 ----------
dp_main <- dp_out_case |> filter(GJURY_YEAR >= 1997, GJURY_YEAR <= 2019)
cat("\nMain sample:", nrow(dp_main), "(EXPECT 122,539) | prosecutors:", n_distinct(dp_main$PROSECUTOR), "(EXPECT 557)\n")
dp_main |> group_by(`RACE-LABEL`) |>
  summarise(grant_rate = mean(DEFERRED_GRANT), n = n(), .groups = "drop") |> print()   # EXPECT Black .243, Latino .231, White .361

# ---------- 6. CONTROLS: offense category, court, sex, age ----------
dp_main <- dp_main |>
  mutate(
    OFFENSE_CAT = case_when(
      str_detect(`OFFENSE-DESC`, "POSS CS|POSS W/I DEL CS|POSS W/INT DEL CS|MAN/DEL CS|DEL CS|POSS MARIJ") ~ "Drug",
      str_detect(`OFFENSE-DESC`, "BURGLARY|BURG HAB|BURG VEHICLE") ~ "Burglary",
      str_detect(`OFFENSE-DESC`, "EVADING ARREST") ~ "Evading",
      str_detect(`OFFENSE-DESC`, "MURDER|HOMICIDE|MANSLAUGHTER") ~ "Homicide",
      str_detect(`OFFENSE-DESC`, "AGG ASSLT|ASSLT|INJURY TO CHILD|RETALIATION") ~ "Assault",
      str_detect(`OFFENSE-DESC`, "FORG|CREDIT/DEBIT|FRAUD|THEFT|UNAUTH USE VEH|CRIM MISCH") ~ "Property",
      str_detect(`OFFENSE-DESC`, "DWI|DRIV WHILE INTOX|DRIVING WHILE INTOX") ~ "DWI",
      str_detect(`OFFENSE-DESC`, "SEX|RAPE|INDECENCY|SEXUAL") ~ "Sex",
      str_detect(`OFFENSE-DESC`, "WEAPON|WPN|CARRY|FELON POSS FIREARM") ~ "Weapon",
      TRUE ~ "Other"),
    COURT_F = factor(COURT),
    AGE = as.numeric(difftime(GJURY_DATE, BIRTHDATE, units = "days")) / 365.25,
    AGE = if_else(AGE >= 14 & AGE <= 90, AGE, NA_real_),
    FEMALE = as.integer(`SEX-LABEL` == "F"))

# ---------- 7. PRIOR RECORD (needs the full all-years file) ----------
# PRIOR_CASES : number of the defendant's earlier cases (same SID, earlier grand-jury date), any outcome
# PRIOR_CONV  : earlier cases ending in a conviction judgment
# PRIOR_DEF   : earlier cases that received a deferred-adjudication grant
conv_codes <- c("SENTENCE-FINE+TERM", "SENTENCE-TERM", "PROB-TERM + FINE",
                "PROBATION TERM", "SENTENCE-FINE", "SENTENCE-LIFE")

prior_all <- all_raw |>
  filter(!is.na(SID), SID != "", !is.na(`G-JURY-DATE`)) |>
  mutate(gj = mdy(`G-JURY-DATE`),
         is_conv  = `JUDGEMENT-DESC` %in% conv_codes,
         is_grant = str_detect(replace_na(`JUDGEMENT-DESC`, ""), "DEFR ADJUD")) |>
  filter(!is.na(gj)) |>
  arrange(SID, gj, `CASE-CAUSE-NBR`) |>
  group_by(SID) |>
  mutate(PRIOR_CASES = row_number() - 1L,
         PRIOR_CONV  = cumsum(is_conv)  - is_conv,
         PRIOR_DEF   = cumsum(is_grant) - is_grant) |>
  ungroup() |>
  select(`CASE-CAUSE-NBR`, PRIOR_CASES, PRIOR_CONV, PRIOR_DEF)

dp_main <- dp_main |>
  left_join(prior_all, by = "CASE-CAUSE-NBR") |>
  mutate(PRIOR_BIN      = factor(pmin(PRIOR_CASES, 10)),
         PRIOR_CONV_BIN = factor(pmin(PRIOR_CONV, 5)),
         PRIOR_DEF_BIN  = factor(pmin(PRIOR_DEF, 5)))
cat("missing PRIOR_CASES:", sum(is.na(dp_main$PRIOR_CASES)), "(EXPECT 0)\n")

# ---------- 8. MAIN TABLE: progression of controls (gap vs. White defendants) ----------
fe3 <- "| PROSECUTOR + GJURY_YEAR + COURT_F"
f_ <- function(rhs, fe = fe3) as.formula(paste("DEFERRED_GRANT ~", rhs, fe))
cl <- ~PROSECUTOR
base <- "BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED"

m1 <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + APPOINTED | PROSECUTOR + GJURY_YEAR,
            data = dp_main, cluster = cl)
m2 <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED | PROSECUTOR + GJURY_YEAR,
            data = dp_main, cluster = cl)
m3 <- feols(f_(base), data = dp_main, cluster = cl)
m4 <- feols(f_(paste(base, "+ pmin(PRIOR_CASES, 10)")), data = dp_main, cluster = cl)
m5 <- feols(f_(paste(base, "+ pmin(PRIOR_CONV, 5) + pmin(PRIOR_DEF, 5)")), data = dp_main, cluster = cl)
m6 <- feols(f_(paste(base, "+ PRIOR_BIN")), data = dp_main, cluster = cl)
m6b <- feols(f_(paste(base, "+ PRIOR_BIN + FEMALE + AGE + I(AGE^2)")), data = dp_main, cluster = cl)
m7a <- feols(f_(paste(base, "+ PRIOR_CONV_BIN + PRIOR_DEF_BIN")), data = dp_main, cluster = cl)
m7 <- feols(f_(paste(base, "+ PRIOR_CONV_BIN + PRIOR_DEF_BIN + FEMALE + AGE + I(AGE^2)")), data = dp_main, cluster = cl)
m8 <- feols(f_(paste(base, "+ PRIOR_BIN + PRIOR_CONV_BIN + PRIOR_DEF_BIN + FEMALE + AGE + I(AGE^2)")),
            data = dp_main, cluster = cl)

etable(m1, m2, m3, m4, m5, keep = "BLACK|LATINO",
       headers = c("Class", "+ Category", "+ Court", "+ Prior cases (linear)", "+ Conv/def (linear)"))
# EXPECT Black/Latino: m1 -.110/-.128 | m2 -.119/-.119 | m3 -.119/-.119 | m4 -.053/-.054 | m5 -.053/-.044
etable(m6, m6b, m7a, m7, m8, keep = "BLACK|LATINO",
       headers = c("Cases bins", "+ sex/age", "Conv/def bins", "Conv/def + sex/age", "All bins + sex/age"))
# EXPECT Black/Latino: m6 -.036/-.028 | m6b -.034/-.022 | m7a -.041/-.026 | m7 -.039/-.021 | m8 -.033/-.017

# share of the gap accounted for by observed characteristics (m3 -> m8)
b3 <- coef(m3)[c("BLACK", "LATINO")]; b8 <- coef(m8)[c("BLACK", "LATINO")]
print(round(1 - b8 / b3, 2))     # EXPECT about 0.73 (Black), 0.86 (Latino)

# ---------- 9. FIRST-TIME DEFENDANTS vs. DEFENDANTS WITH PRIORS ----------
m_first <- feols(f_(base), data = filter(dp_main, PRIOR_CASES == 0), cluster = cl)
m_prior <- feols(f_(base), data = filter(dp_main, PRIOR_CASES >= 1), cluster = cl)
etable(m_first, m_prior, keep = "BLACK|LATINO", headers = c("No prior case", "1+ prior cases"))
# EXPECT Black/Latino: first-time -.038/-.014 | with priors -.065/-.062

# ---------- 10. EXPERIENCE: does the gap change with prosecutor experience? ----------
m8x <- feols(f_(paste("BLACK + LATINO + BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 + OUTTAKE_CASE_N100 +",
                      "OFFENSE_TYPE + OFFENSE_CAT + APPOINTED + PRIOR_BIN + PRIOR_CONV_BIN + PRIOR_DEF_BIN +",
                      "FEMALE + AGE + I(AGE^2)")), data = dp_main, cluster = cl)
print(coeftable(m8x)[c("BLACK", "LATINO", "OUTTAKE_CASE_N100",
                       "BLACK:OUTTAKE_CASE_N100", "LATINO:OUTTAKE_CASE_N100"), ])
# EXPECT: BLACK -.028 | LATINO -.015 | experience -.0004 (p=.70) | Black x exp -.0009 (p=.21) | Latino x exp -.0003 (p=.54)

# ---------- 11. EARLIER MODELS (logit and tenure) ----------
specC <- feglm(DEFERRED_GRANT ~ BLACK + LATINO + BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
                 OUTTAKE_CASE_N100 + OFFENSE_TYPE + APPOINTED + GJURY_YEAR_FE | PROSECUTOR,
               data = dp_main, family = binomial(link = "logit"), cluster = ~PROSECUTOR)
specC_lpm <- feols(DEFERRED_GRANT ~ BLACK + LATINO + BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
                     OUTTAKE_CASE_N100 + OFFENSE_TYPE + APPOINTED + GJURY_YEAR_FE | PROSECUTOR,
                   data = dp_main, cluster = ~PROSECUTOR)

first_year_tbl <- dp_raw |>
  filter(`OUTTAKE-PROSECUTOR` %in% fresh_outtake, !is.na(GJURY_DATE), GJURY_YEAR >= 1950) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  summarise(first_gjury_year = min(GJURY_YEAR, na.rm = TRUE), .groups = "drop")
dp_main_t <- dp_main |>
  left_join(first_year_tbl, by = "OUTTAKE-PROSECUTOR") |>
  mutate(TENURE_YEARS = GJURY_YEAR - first_gjury_year)
specC_tenure_lpm <- feols(DEFERRED_GRANT ~ BLACK + LATINO + BLACK:TENURE_YEARS + LATINO:TENURE_YEARS +
                            TENURE_YEARS + OFFENSE_TYPE + APPOINTED | PROSECUTOR + GJURY_YEAR,
                          data = dp_main_t, cluster = ~PROSECUTOR)   # year FE as an fixed effect, so tenure main effect is not absorbed incorrectly
etable(specC, specC_lpm, specC_tenure_lpm,
       keep = c("BLACK", "LATINO", "OUTTAKE_CASE_N100", "TENURE"))
# EXPECT logit: Black -.492, Latino -.637, Black x exp -.0101 (p=.024); LPM: Black -.101, Latino -.126, Black x exp -.0020

# ---------- 12. EXPORT ----------
etable(m1, m2, m3, m4, m5, m6, m7, m8, keep = "BLACK|LATINO", tex = TRUE,
       file = file.path(OUT_DIR, "table_gap_progression.tex"))
etable(m_first, m_prior, keep = "BLACK|LATINO", tex = TRUE, file = file.path(OUT_DIR, "table_by_prior_record.tex"))
etable(m8x, keep = "BLACK|LATINO|OUTTAKE", tex = TRUE, file = file.path(OUT_DIR, "table_experience.tex"))
message("Done. Tables saved in: ", OUT_DIR)
