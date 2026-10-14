############################################################
# BEXAR COUNTY: ALL-IN-ONE SCRIPT (every result in one file)
#
# Inputs (from your cleaning script):
#   bexar_panel_1990_2021.parquet   and   bexar_cleaned.parquet (all years)
# Run top to bottom. "EXPECT" comments give the numbers you got in R.
#
# PARTS
#   1-5  Sample construction (grand-jury window, prosecutors, experience)
#   6-7  Controls and prior record
#   8    Stage 1: gap in receiving deferred adjudication (progression table)
#   9    First-time defendants vs. defendants with priors
#   10   Experience: does the gap change with prosecutor experience?
#   11   Stage 2: among grant recipients, revocation proceeding / conviction
#   12   Cascade: grant -> completed dismissal (descriptive, controlled, figure)
#   13   Earlier logit / tenure models
#   14   Export
############################################################
library(tidyverse); library(arrow); library(fixest); library(lubridate)

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"    # <-- CHANGE if needed (carol vs carolineferguson)
OUT_DIR  <- file.path(DATA_DIR, "final_output"); dir.create(OUT_DIR, showWarnings = FALSE)

# Your numbers used the first-observed year from the 1990-2021 PANEL.
# TRUE = use ALL years instead (stricter left-censoring rule; counts shift slightly).
FIRST_FROM_ALL_YEARS <- FALSE

# ---------- 1. LOAD ----------
dp_raw  <- read_parquet(file.path(DATA_DIR, "bexar_panel_1990_2021.parquet"))
all_raw <- read_parquet(file.path(DATA_DIR, "bexar_cleaned.parquet"))
cat("Panel rows:", nrow(dp_raw), "(EXPECT 330,778) | All-years rows:", nrow(all_raw), "(EXPECT 385,097)\n")
dp_raw <- dp_raw |> mutate(GJURY_DATE = mdy(`G-JURY-DATE`), GJURY_YEAR = year(GJURY_DATE))

# ---------- 2. PROSECUTORS FIRST OBSERVED 1991 OR LATER (grand-jury date) ----------
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
cat("Prosecutors starting 1991+:", length(fresh_outtake), "(EXPECT 614 when FIRST_FROM_ALL_YEARS = FALSE)\n")

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
cat("Analytic sample:", nrow(dp_out_case), "(EXPECT 126,582) | prosecutors:",
    n_distinct(dp_out_case$PROSECUTOR), "(EXPECT 604)\n")

# Diagnostic: grant rate by grand-jury year should be flat (about 22-30%), no late collapse
dp_out_case |> group_by(GJURY_YEAR) |>
  summarise(grant_rate = mean(DEFERRED_GRANT, na.rm = TRUE), n = n(), .groups = "drop") |>
  filter(GJURY_YEAR >= 1995) |> print(n = 30)

# ---------- 5. MAIN WINDOW 1997-2019 ----------
dp_main <- dp_out_case |> filter(GJURY_YEAR >= 1997, GJURY_YEAR <= 2019)
cat("\nMain sample:", nrow(dp_main), "(EXPECT 122,539) | prosecutors:", n_distinct(dp_main$PROSECUTOR), "(EXPECT 557)\n")
dp_main |> group_by(`RACE-LABEL`) |>
  summarise(grant_rate = mean(DEFERRED_GRANT), n = n(), .groups = "drop") |> print()
# EXPECT grant rate: Black .243 (19,272) | Latino .231 (64,871) | White .361 (38,396)

# ---------- 6. CONTROLS: offense category (original regex, matches the real strings), court, sex, age ----------
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
cat("Share in 'Other' offense category (should be about 25%):",
    round(mean(dp_main$OFFENSE_CAT == "Other"), 3), "\n")

# ---------- 7. PRIOR RECORD (needs the full all-years file) ----------
# PRIOR_CASES : earlier cases for the same defendant (SID, earlier grand-jury date), any outcome
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

# ---------- 8. STAGE 1: gap in RECEIVING deferred adjudication (progression of controls) ----------
cl  <- ~PROSECUTOR
fe3 <- "| PROSECUTOR + GJURY_YEAR + COURT_F"
f_  <- function(rhs, y = "DEFERRED_GRANT", fe = fe3) as.formula(paste(y, "~", rhs, fe))
base <- "BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED"

m1  <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + APPOINTED | PROSECUTOR + GJURY_YEAR,
             data = dp_main, cluster = cl)
m2  <- feols(DEFERRED_GRANT ~ BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED | PROSECUTOR + GJURY_YEAR,
             data = dp_main, cluster = cl)
m3  <- feols(f_(base), data = dp_main, cluster = cl)
m4  <- feols(f_(paste(base, "+ pmin(PRIOR_CASES, 10)")), data = dp_main, cluster = cl)
m5  <- feols(f_(paste(base, "+ pmin(PRIOR_CONV, 5) + pmin(PRIOR_DEF, 5)")), data = dp_main, cluster = cl)
m6  <- feols(f_(paste(base, "+ PRIOR_BIN")), data = dp_main, cluster = cl)
m6b <- feols(f_(paste(base, "+ PRIOR_BIN + FEMALE + AGE + I(AGE^2)")), data = dp_main, cluster = cl)
m7a <- feols(f_(paste(base, "+ PRIOR_CONV_BIN + PRIOR_DEF_BIN")), data = dp_main, cluster = cl)
m7  <- feols(f_(paste(base, "+ PRIOR_CONV_BIN + PRIOR_DEF_BIN + FEMALE + AGE + I(AGE^2)")), data = dp_main, cluster = cl)
m8  <- feols(f_(paste(base, "+ PRIOR_BIN + PRIOR_CONV_BIN + PRIOR_DEF_BIN + FEMALE + AGE + I(AGE^2)")),
             data = dp_main, cluster = cl)

etable(m1, m2, m3, m4, m5, keep = "BLACK|LATINO",
       headers = c("Class", "+ Category", "+ Court", "+ Prior cases (linear)", "+ Conv/def (linear)"))
# EXPECT Black/Latino: m1 -.110/-.128 | m2 -.119/-.119 | m3 -.119/-.119 | m4 -.053/-.054 | m5 -.053/-.044
etable(m6, m6b, m7a, m7, m8, keep = "BLACK|LATINO",
       headers = c("Cases bins", "+ sex/age", "Conv/def bins", "Conv/def + sex/age", "All bins + sex/age"))
# EXPECT Black/Latino: m6 -.036/-.028 | m6b -.034/-.022 | m7a -.041/-.026 | m7 -.039/-.021 | m8 -.033/-.017

b3 <- coef(m3)[c("BLACK", "LATINO")]; b8 <- coef(m8)[c("BLACK", "LATINO")]
cat("Share of the gap accounted for by observed characteristics (m3 -> m8):\n"); print(round(1 - b8 / b3, 2))
# EXPECT about 0.73 (Black), 0.86 (Latino)

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

# ---------- 11. STAGE 2: among defendants who RECEIVED a grant ----------
# REVOKED          : a prosecutor is assigned to a revocation proceeding (a motion to revoke/adjudicate;
#                    it does not mean the revocation was granted)
# CONV_AFTER_GRANT : the case ends in a guilty-plea disposition (PG / NOLO CT-GUILTY)
dp_deferred <- dp_main |>
  filter(DEFERRED_GRANT == 1) |>
  mutate(REVOKED = as.integer(!is.na(`REVOKATION-PROSECUTOR`) & `REVOKATION-PROSECUTOR` != ""),
         CONV_AFTER_GRANT = as.integer(`DISPOSITION-DESC` %in% c("PG CT-GUILTY", "NOLO CT-GUILTY")))

dp_deferred |> filter(GJURY_YEAR <= 2014) |> group_by(`RACE-LABEL`) |>
  summarise(n = n(), revoked = mean(REVOKED), conv_after_grant = mean(CONV_AFTER_GRANT), .groups = "drop") |> print()
dp_deferred |> group_by(GJURY_YEAR) |>
  summarise(n = n(), revoked = round(mean(REVOKED), 3), .groups = "drop") |> print(n = 30)  # falls in recent years = censoring

ctrl2 <- "BLACK + LATINO + OFFENSE_TYPE + OFFENSE_CAT + APPOINTED + PRIOR_BIN + PRIOR_CONV_BIN + PRIOR_DEF_BIN + FEMALE + AGE + I(AGE^2)"
fit_g <- function(y, d) feols(as.formula(paste(y, "~", ctrl2, fe3)), data = d, cluster = cl)
d19 <- dp_deferred
d14 <- filter(dp_deferred, GJURY_YEAR <= 2014)
d12 <- filter(dp_deferred, GJURY_YEAR <= 2012)

etable(fit_g("REVOKED", d19), fit_g("REVOKED", d14), fit_g("REVOKED", d12),
       fit_g("CONV_AFTER_GRANT", d19), fit_g("CONV_AFTER_GRANT", d14), fit_g("CONV_AFTER_GRANT", d12),
       keep = "BLACK|LATINO",
       headers = c("Revoc <=2019", "Revoc <=2014", "Revoc <=2012", "Conv <=2019", "Conv <=2014", "Conv <=2012"))
# EXPECT Black/Latino: Revoc <=2014 +.105/+.082 | Revoc <=2012 +.113/+.085
#                      Conv  <=2014 +.078/+.084 | Conv  <=2012 +.094/+.086   (all p < .001)
# (<=2019 first run for revocation was +.098/+.080 on 33,528 grants, model without APPOINTED)

# ---------- 12. CASCADE: receiving a grant -> ending in a completed dismissal (grand-jury years <= 2012) ----------
dp12 <- dp_main |> filter(GJURY_YEAR <= 2012)

# 12a. Descriptive rates by race
casc <- dp12 |> group_by(`RACE-LABEL`) |>
  summarise(n = n(), received_grant = mean(DEFERRED_GRANT), completed_dismissal = mean(DEFERRED), .groups = "drop")
print(casc)
# EXPECT: Black n=10,503 .241 .0895 | Latino n=36,342 .246 .0995 | White n=16,862 .349 .190

# 12b. Controlled gaps at each stage, same years
m_grant12 <- feols(as.formula(paste("DEFERRED_GRANT ~", ctrl2, fe3)), data = dp12, cluster = cl)
m_comp12  <- feols(as.formula(paste("DEFERRED ~",       ctrl2, fe3)), data = dp12, cluster = cl)
etable(m_grant12, m_comp12, keep = "BLACK|LATINO",
       headers = c("Received a grant", "Ended in completed dismissal"))
# EXPECT completed dismissal: BLACK -.056 | LATINO -.046 (n = 62,710)

# 12c. Figure
fig <- casc |>
  transmute(`RACE-LABEL`, `Received deferred adjudication` = received_grant,
            `Ended in completed dismissal` = completed_dismissal) |>
  pivot_longer(-`RACE-LABEL`, names_to = "stage", values_to = "rate")
p <- ggplot(fig, aes(`RACE-LABEL`, 100 * rate, fill = stage)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Percent of defendants", fill = NULL) + theme_minimal()
ggsave(file.path(OUT_DIR, "figure_two_stages.png"), p, width = 7, height = 4.5, dpi = 200)

# ---------- 13. EARLIER MODELS (logit and tenure) ----------
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
                          data = dp_main_t, cluster = ~PROSECUTOR)
etable(specC, specC_lpm, specC_tenure_lpm, keep = c("BLACK", "LATINO", "OUTTAKE_CASE_N100", "TENURE"))
# EXPECT logit: Black -.492, Latino -.637, Black x exp -.0101 (p=.024); LPM: Black -.101, Latino -.126, Black x exp -.0020

# ---------- 14. EXPORT ----------
etable(m1, m2, m3, m4, m5, m6, m7, m8, keep = "BLACK|LATINO", tex = TRUE,
       file = file.path(OUT_DIR, "table_gap_progression.tex"))
etable(m_first, m_prior, keep = "BLACK|LATINO", tex = TRUE, file = file.path(OUT_DIR, "table_by_prior_record.tex"))
etable(m8x, keep = "BLACK|LATINO|OUTTAKE", tex = TRUE, file = file.path(OUT_DIR, "table_experience.tex"))
etable(fit_g("REVOKED", d19), fit_g("REVOKED", d14), fit_g("REVOKED", d12),
       fit_g("CONV_AFTER_GRANT", d19), fit_g("CONV_AFTER_GRANT", d14), fit_g("CONV_AFTER_GRANT", d12),
       keep = "BLACK|LATINO", tex = TRUE, file = file.path(OUT_DIR, "table_stage2.tex"))
etable(m_grant12, m_comp12, keep = "BLACK|LATINO", tex = TRUE, file = file.path(OUT_DIR, "table_cascade.tex"))
message("Done. Tables and figure saved in: ", OUT_DIR)
