# Bexar County — Outtake Prosecutor Publication Tables (LaTeX output)
# Requires: bexar_outtake_model_results.csv (from bexar_outtake_models.R)
#           bexar_prosecutor_panel_1990_2015.parquet
#
# All tables label the prosecutor as "Prosecutor" (not "Outtake Prosecutor").
# Experience variable: prosecutor cumulative case count / 100.
#
# Outputs:
#   bexar_outtake_table1.tex — main interaction coefficients
#   bexar_outtake_table2.tex — within-offense-type
#   bexar_outtake_table3.tex — predicted probabilities
#   bexar_outtake_table4.tex — DA-era robustness
#   bexar_outtake_table5.tex — career start vs peak gap

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"

library(tidyverse)
library(arrow)
library(fixest)

res <- read_csv(file.path(DATA_DIR, "bexar_outtake_model_results.csv"), show_col_types = FALSE)

dp_raw <- read_parquet(file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))

# ── Helpers ───────────────────────────────────────────────────────────────────

stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )
}

fmt_est <- function(est, p) paste0(sprintf("%.6f", est), stars(p))
fmt_se  <- function(se)      paste0("(", sprintf("%.6f", se), ")")

write_tex <- function(lines, path) {
  writeLines(lines, con = path)
  message("Saved: ", path)
}

# ── Build outtake data frame for predicted probabilities ─────────────────────

fresh_prosecutors <- dp_raw |>
  group_by(`INTAKE-PROSECUTOR`) |>
  summarise(first_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop") |>
  filter(first_year >= 1991) |>
  pull(`INTAKE-PROSECUTOR`)

offense_order <- c("F1", "F2", "F3", "FS")

dp_out <- dp_raw |>
  filter(`INTAKE-PROSECUTOR` %in% fresh_prosecutors,
         !is.na(`OUTTAKE-PROSECUTOR`),
         `RACE-LABEL` %in% c("Black", "White", "Latino"),
         `OFFENSE-CLASS` %in% offense_order) |>
  mutate(OFFENSE_RANK = match(`OFFENSE-CLASS`, offense_order)) |>
  arrange(`CASE-CAUSE-NBR`, OFFENSE_RANK) |>
  distinct(`CASE-CAUSE-NBR`, .keep_all = TRUE) |>
  select(-OFFENSE_RANK) |>
  arrange(`OUTTAKE-PROSECUTOR`, `CASE-DATE`) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  mutate(OUTTAKE_CASE_N = row_number()) |>
  ungroup() |>
  mutate(
    BLACK    = as.integer(`RACE-LABEL` == "Black"),
    LATINO   = as.integer(`RACE-LABEL` == "Latino"),
    APPOINTED = as.integer(`ATTORNEY-TYPE` == "Appointed"),
    OFFENSE_TYPE = fct_relevel(`OFFENSE-CLASS`, "F3"),
    OUTTAKE_CASE_N100 = OUTTAKE_CASE_N / 100,
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
    OFFENSE_CATEGORY2 = fct_relevel(OFFENSE_CATEGORY2, "Other")
  )

# ── Table 1: Main interaction coefficients ────────────────────────────────────

model_order <- c("SpecA_OffenseOnly", "SpecB_YearFE", "SpecC_ProsecutorFE", "SpecD_WithPriors")
focal_rows  <- c("Black $\\times$ Career Case $N$ (per 100)",
                 "Latino $\\times$ Career Case $N$ (per 100)",
                 "Black", "Latino", "Career Case $N$ (per 100)")

table_data <- res |>
  filter(model %in% model_order) |>
  mutate(
    term_clean = case_when(
      term == "BLACK:OUTTAKE_CASE_N100" | term == "OUTTAKE_CASE_N100:BLACK" ~
        "Black $\\times$ Career Case $N$ (per 100)",
      term == "LATINO:OUTTAKE_CASE_N100" | term == "OUTTAKE_CASE_N100:LATINO" ~
        "Latino $\\times$ Career Case $N$ (per 100)",
      term == "BLACK"              ~ "Black",
      term == "LATINO"             ~ "Latino",
      term == "OUTTAKE_CASE_N100"  ~ "Career Case $N$ (per 100)",
      TRUE ~ NA_character_
    ),
    est_cell = fmt_est(estimate, p.value),
    se_cell  = fmt_se(std.error),
    model    = factor(model, levels = model_order)
  ) |>
  filter(!is.na(term_clean))

t1_est <- table_data |>
  filter(term_clean %in% focal_rows) |>
  select(term_clean, model, est_cell) |>
  pivot_wider(names_from = model, values_from = est_cell) |>
  mutate(term_clean = factor(term_clean, levels = focal_rows)) |>
  arrange(term_clean) |>
  mutate(across(any_of(c("SpecA_OffenseOnly","SpecB_YearFE","SpecC_ProsecutorFE","SpecD_WithPriors")),
                ~ replace_na(., "---")))

t1_se <- table_data |>
  filter(term_clean %in% focal_rows) |>
  select(term_clean, model, se_cell) |>
  pivot_wider(names_from = model, values_from = se_cell) |>
  mutate(term_clean = factor(term_clean, levels = focal_rows)) |>
  arrange(term_clean) |>
  mutate(across(any_of(c("SpecA_OffenseOnly","SpecB_YearFE","SpecC_ProsecutorFE","SpecD_WithPriors")),
                ~ replace_na(., "")))

n_specA <- res |> filter(model == "SpecA_OffenseOnly") |> slice(1) |> pull(model)  # placeholder
# Pull N from model output printed during bexar_outtake_models.R run
# These are populated after running that script; update manually if needed
n_specA_val  <- "146,202"
n_specB_val  <- "146,202"
n_specC_val  <- "146,202"
n_specD_val  <- "146,202"

tex1 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Deferred Adjudication and Prosecutor Experience: Focal Interaction Coefficients}",
  "\\label{tab:main_out}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccc}",
  "\\hline\\hline",
  " & (1) & (2) & (3) & (4) \\\\",
  " & Offense + Attorney & $+$ Year FE & Prosecutor FE & Prosecutor FE + Priors \\\\",
  "\\hline"
)

for (i in seq_len(nrow(t1_est))) {
  e <- t1_est[i, ]; s <- t1_se[i, ]
  tex1 <- c(tex1,
    paste0(e$term_clean, " & ", e$SpecA_OffenseOnly,
           " & ", e$SpecB_YearFE, " & ", e$SpecC_ProsecutorFE,
           " & ", e$SpecD_WithPriors, " \\\\"),
    paste0(" & ", s$SpecA_OffenseOnly,
           " & ", s$SpecB_YearFE, " & ", s$SpecC_ProsecutorFE,
           " & ", s$SpecD_WithPriors, " \\\\"),
    "& & & & \\\\"
  )
}

tex1 <- c(tex1,
  "\\hline",
  "Offense type \\& category FE & Yes & Yes & Yes & Yes \\\\",
  "Attorney type & Yes & Yes & Yes & Yes \\\\",
  "Case year FE & No & Yes & No & No \\\\",
  "Prosecutor FE & No & No & Yes & Yes \\\\",
  "Defendant case $N$ & No & No & No & Yes \\\\",
  "Sample & 1991+ & 1991+ & 1991+ & 1991+ \\\\",
  paste0("$N$ & $", n_specA_val, "$ & $", n_specB_val,
         "$ & $", n_specC_val, "$ & $", n_specD_val, "$ \\\\"),
  "\\hline\\hline",
  "\\multicolumn{5}{l}{\\footnotesize \\textit{Notes:} Logistic regression coefficients (log-odds). Outcome: deferred adjudication.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Standard errors in parentheses, clustered by prosecutor. Sample: felony cases, prosecutors first observed 1991+.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Prosecutor = disposition-stage (outtake) prosecutor. Career case $N$ = cumulative case count per prosecutor.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Col.~(4) adds defendant cumulative case count as proxy for prior record.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize $^{***}p<0.01$\\quad $^{**}p<0.05$\\quad $^{*}p<0.10$} \\\\",
  "\\end{tabular}}",
  "\\end{table}"
)

write_tex(tex1, file.path(DATA_DIR, "bexar_outtake_table1.tex"))

# ── Table 2: Within-offense-type ──────────────────────────────────────────────

m7_data <- res |>
  filter(str_starts(model, "M7_")) |>
  mutate(
    offense = str_extract(model, "F[123S]"),
    term_clean = case_when(
      term == "BLACK:OUTTAKE_CASE_N100" | term == "OUTTAKE_CASE_N100:BLACK" ~
        "Black $\\times$ Career Case $N$ (per 100)",
      term == "LATINO:OUTTAKE_CASE_N100" | term == "OUTTAKE_CASE_N100:LATINO" ~
        "Latino $\\times$ Career Case $N$ (per 100)",
      term == "BLACK"  ~ "Black",
      term == "LATINO" ~ "Latino",
      TRUE ~ NA_character_
    ),
    est_cell = fmt_est(estimate, p.value),
    se_cell  = fmt_se(std.error)
  ) |>
  filter(!is.na(term_clean))

m7_est <- m7_data |>
  select(term_clean, offense, est_cell) |>
  pivot_wider(names_from = offense, values_from = est_cell)

m7_se <- m7_data |>
  select(term_clean, offense, se_cell) |>
  pivot_wider(names_from = offense, values_from = se_cell)

if (nrow(m7_est) > 0) {
  offense_cols <- setdiff(colnames(m7_est), "term_clean")
  n_cols <- length(offense_cols)
  col_spec <- paste0("l", paste(rep("c", n_cols), collapse = ""))

  tex2 <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Within-Offense-Type Estimates}",
    "\\label{tab:offense_out}",
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\hline\\hline",
    paste0(" & ", paste(offense_cols, collapse = " & "), " \\\\"),
    "\\hline"
  )

  for (i in seq_len(nrow(m7_est))) {
    e <- m7_est[i, ]; s <- m7_se[i, ]
    tex2 <- c(tex2,
      paste0(e$term_clean, " & ", paste(unlist(e[offense_cols]), collapse = " & "), " \\\\"),
      paste0(" & ", paste(unlist(s[offense_cols]), collapse = " & "), " \\\\"),
      paste0(paste(rep("&", n_cols), collapse = " "), " \\\\")
    )
  }

  tex2 <- c(tex2,
    "\\hline\\hline",
    paste0("\\multicolumn{", n_cols + 1, "}{l}{\\footnotesize \\textit{Notes:} Each column estimates the same model within a single offense class.} \\\\"),
    paste0("\\multicolumn{", n_cols + 1, "}{l}{\\footnotesize Controls: offense category and attorney type. Log-odds. SEs in parentheses.} \\\\"),
    paste0("\\multicolumn{", n_cols + 1, "}{l}{\\footnotesize $^{***}p<0.01$\\quad $^{**}p<0.05$\\quad $^{*}p<0.10$} \\\\"),
    "\\end{tabular}",
    "\\end{table}"
  )

  write_tex(tex2, file.path(DATA_DIR, "bexar_outtake_table2.tex"))
}

# ── Table 3: Predicted probabilities ──────────────────────────────────────────
# Re-fit SpecA as plain glm for predict()

fit_pred <- glm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
  data   = dp_out |> filter(!is.na(OFFENSE_CATEGORY2)),
  family = binomial()
)

q_breaks <- quantile(dp_out$OUTTAKE_CASE_N100, probs = seq(0, 1, 0.2), na.rm = TRUE)
q1_mid   <- mean(c(q_breaks[1], q_breaks[2]))
q5_mid   <- mean(c(q_breaks[5], q_breaks[6]))

ref_dist <- dp_out |>
  filter(!is.na(OFFENSE_CATEGORY2), APPOINTED == 1) |>
  count(OFFENSE_TYPE, OFFENSE_CATEGORY2) |>
  mutate(wt = n / sum(n))

pred_cell <- function(black, latino, case_n100) {
  g <- ref_dist |>
    mutate(BLACK = black, LATINO = latino,
           OUTTAKE_CASE_N100 = case_n100, APPOINTED = 1)
  weighted.mean(predict(fit_pred, newdata = g, type = "response"), g$wt)
}

white_q1  <- pred_cell(0, 0, q1_mid)
black_q1  <- pred_cell(1, 0, q1_mid)
latino_q1 <- pred_cell(0, 1, q1_mid)
white_q5  <- pred_cell(0, 0, q5_mid)
black_q5  <- pred_cell(1, 0, q5_mid)
latino_q5 <- pred_cell(0, 1, q5_mid)

message(sprintf("White Q1: %.1f%%  Black Q1: %.1f%%  W-B Gap: %.1f pp",
                white_q1*100, black_q1*100, (white_q1-black_q1)*100))
message(sprintf("White Q5: %.1f%%  Black Q5: %.1f%%  W-B Gap: %.1f pp",
                white_q5*100, black_q5*100, (white_q5-black_q5)*100))
message(sprintf("White Q1: %.1f%%  Latino Q1: %.1f%%  W-L Gap: %.1f pp",
                white_q1*100, latino_q1*100, (white_q1-latino_q1)*100))
message(sprintf("White Q5: %.1f%%  Latino Q5: %.1f%%  W-L Gap: %.1f pp",
                white_q5*100, latino_q5*100, (white_q5-latino_q5)*100))

# By-offense breakdown
offense_types  <- c("F1", "F2", "F3", "FS")
offense_labels <- c("F1 (first degree)", "F2 (second degree)",
                    "F3 (third degree)", "FS (state jail)")

wb_rows <- list(); wl_rows <- list()
for (j in seq_along(offense_types)) {
  ot <- offense_types[j]
  ref_ot <- dp_out |>
    filter(!is.na(OFFENSE_CATEGORY2), APPOINTED == 1, OFFENSE_TYPE == ot) |>
    count(OFFENSE_TYPE, OFFENSE_CATEGORY2) |>
    mutate(wt = n / sum(n))
  if (nrow(ref_ot) == 0) next

  pred_ot <- function(black, latino, case_n100) {
    g <- ref_ot |> mutate(BLACK = black, LATINO = latino,
                           OUTTAKE_CASE_N100 = case_n100, APPOINTED = 1)
    weighted.mean(predict(fit_pred, newdata = g, type = "response"), g$wt)
  }

  wq1 <- pred_ot(0, 0, q1_mid); bq1 <- pred_ot(1, 0, q1_mid); lq1 <- pred_ot(0, 1, q1_mid)
  wq5 <- pred_ot(0, 0, q5_mid); bq5 <- pred_ot(1, 0, q5_mid); lq5 <- pred_ot(0, 1, q5_mid)

  wb_rows[[j]] <- list(label = offense_labels[j],
                        gap_q1 = (wq1 - bq1) * 100,
                        gap_q5 = (wq5 - bq5) * 100,
                        change = ((wq5 - bq5) - (wq1 - bq1)) * 100)
  wl_rows[[j]] <- list(label = offense_labels[j],
                        gap_q1 = (wq1 - lq1) * 100,
                        gap_q5 = (wq5 - lq5) * 100,
                        change = ((wq5 - lq5) - (wq1 - lq1)) * 100)
}

tex3 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Predicted Probability of Deferred Adjudication by Race and Prosecutor Experience}",
  "\\label{tab:pred_out}",
  "\\begin{tabular}{lccc}",
  "\\hline\\hline",
  " & Q1 Gap (pp) & Q5 Gap (pp) & Change (pp) \\\\",
  "\\hline",
  "\\textit{Panel A: White--Black gap, all felony offenses} & & & \\\\",
  paste0("\\quad All offenses & ",
         sprintf("%.1f", (white_q1 - black_q1) * 100), " & ",
         sprintf("%.1f", (white_q5 - black_q5) * 100), " & ",
         sprintf("%.1f", ((white_q5 - black_q5) - (white_q1 - black_q1)) * 100), " \\\\")
)

for (r in wb_rows) {
  tex3 <- c(tex3,
    paste0("\\quad ", r$label, " & ",
           sprintf("%.1f", r$gap_q1), " & ",
           sprintf("%.1f", r$gap_q5), " & ",
           sprintf("%.1f", r$change), " \\\\"))
}

tex3 <- c(tex3,
  "\\hline",
  "\\textit{Panel B: White--Latino gap, all felony offenses} & & & \\\\",
  paste0("\\quad All offenses & ",
         sprintf("%.1f", (white_q1 - latino_q1) * 100), " & ",
         sprintf("%.1f", (white_q5 - latino_q5) * 100), " & ",
         sprintf("%.1f", ((white_q5 - latino_q5) - (white_q1 - latino_q1)) * 100), " \\\\")
)

for (r in wl_rows) {
  tex3 <- c(tex3,
    paste0("\\quad ", r$label, " & ",
           sprintf("%.1f", r$gap_q1), " & ",
           sprintf("%.1f", r$gap_q5), " & ",
           sprintf("%.1f", r$change), " \\\\"))
}

tex3 <- c(tex3,
  "\\hline\\hline",
  "\\multicolumn{4}{l}{\\footnotesize \\textit{Notes:} Gap in predicted probability of deferred adjudication (percentage points).} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize Predicted from Specification (1). Averaged across appointed-counsel cases within each offense class.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize Q1 = bottom experience quintile; Q5 = top experience quintile.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(tex3, file.path(DATA_DIR, "bexar_outtake_table3.tex"))

# ── Table 4: DA-era robustness ────────────────────────────────────────────────

focal_era <- c("Black $\\times$ Career Case $N$ (per 100)",
               "Latino $\\times$ Career Case $N$ (per 100)",
               "Black", "Latino")

era_data <- res |>
  filter(model %in% c("Era_Hilbig", "Era_Reed")) |>
  mutate(
    term_clean = case_when(
      term == "BLACK:OUTTAKE_CASE_N100" | term == "OUTTAKE_CASE_N100:BLACK" ~
        "Black $\\times$ Career Case $N$ (per 100)",
      term == "LATINO:OUTTAKE_CASE_N100" | term == "OUTTAKE_CASE_N100:LATINO" ~
        "Latino $\\times$ Career Case $N$ (per 100)",
      term == "BLACK"  ~ "Black",
      term == "LATINO" ~ "Latino",
      TRUE ~ NA_character_
    ),
    est_cell = fmt_est(estimate, p.value),
    se_cell  = fmt_se(std.error)
  ) |>
  filter(!is.na(term_clean))

era_est <- era_data |>
  filter(model == "Era_Hilbig") |>
  select(term_clean, est_cell) |> rename(Hilbig = est_cell) |>
  left_join(
    era_data |> filter(model == "Era_Reed") |>
      select(term_clean, est_cell) |> rename(Reed = est_cell),
    by = "term_clean"
  ) |>
  mutate(term_clean = factor(term_clean, levels = focal_era)) |>
  arrange(term_clean)

era_se <- era_data |>
  filter(model == "Era_Hilbig") |>
  select(term_clean, se_cell) |> rename(Hilbig = se_cell) |>
  left_join(
    era_data |> filter(model == "Era_Reed") |>
      select(term_clean, se_cell) |> rename(Reed = se_cell),
    by = "term_clean"
  ) |>
  mutate(term_clean = factor(term_clean, levels = focal_era)) |>
  arrange(term_clean)

tex4 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Learning Curve by DA Era: Hilbig vs.\\ Reed Hires}",
  "\\label{tab:era_out}",
  "\\begin{tabular}{lcc}",
  "\\hline\\hline",
  " & Hilbig-era hires & Reed-era hires \\\\",
  " & (DA 1991--1998) & (DA 1999--2015) \\\\",
  "\\hline"
)

for (i in seq_len(nrow(era_est))) {
  e <- era_est[i, ]; s <- era_se[i, ]
  tex4 <- c(tex4,
    paste0(e$term_clean, " & ", e$Hilbig, " & ", e$Reed, " \\\\"),
    paste0(" & ", s$Hilbig, " & ", s$Reed, " \\\\"),
    "& & \\\\"
  )
}

tex4 <- c(tex4,
  "\\hline\\hline",
  "\\multicolumn{3}{l}{\\footnotesize \\textit{Notes:} Both columns: prosecutor fixed-effects logistic regression. Subsamples} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize split by DA in office when the outtake prosecutor was first observed. Controls: felony} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize severity, offense category, attorney type. SEs clustered by prosecutor.} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize $^{***}p<0.01$\\quad $^{**}p<0.05$\\quad $^{*}p<0.10$} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(tex4, file.path(DATA_DIR, "bexar_outtake_table4.tex"))

# ── Table 5: Gap at career start vs peak ─────────────────────────────────────

max_case_n5 <- max(dp_out$OUTTAKE_CASE_N100, na.rm = TRUE)
modal_oc2   <- names(sort(table(dp_out$OFFENSE_CATEGORY2), decreasing = TRUE))[1]

t5_wb <- list(); t5_wl <- list()
for (j in seq_along(offense_types)) {
  oc <- offense_types[j]

  nd_start <- tibble(BLACK = c(0,1,0), LATINO = c(0,0,1),
                     OUTTAKE_CASE_N100 = 0.01,
                     OFFENSE_TYPE = oc, OFFENSE_CATEGORY2 = modal_oc2, APPOINTED = 1L)
  nd_peak  <- tibble(BLACK = c(0,1,0), LATINO = c(0,0,1),
                     OUTTAKE_CASE_N100 = max_case_n5,
                     OFFENSE_TYPE = oc, OFFENSE_CATEGORY2 = modal_oc2, APPOINTED = 1L)

  nd_start$pred <- predict(fit_pred, newdata = nd_start, type = "response")
  nd_peak$pred  <- predict(fit_pred, newdata = nd_peak,  type = "response")

  # White=row1, Black=row2, Latino=row3
  t5_wb[[j]] <- list(
    label     = offense_labels[j],
    gap_start = (nd_start$pred[1] - nd_start$pred[2]) * 100,
    gap_peak  = (nd_peak$pred[1]  - nd_peak$pred[2])  * 100,
    change    = ((nd_peak$pred[1] - nd_peak$pred[2]) -
                 (nd_start$pred[1] - nd_start$pred[2])) * 100
  )
  t5_wl[[j]] <- list(
    label     = offense_labels[j],
    gap_start = (nd_start$pred[1] - nd_start$pred[3]) * 100,
    gap_peak  = (nd_peak$pred[1]  - nd_peak$pred[3])  * 100,
    change    = ((nd_peak$pred[1] - nd_peak$pred[3]) -
                 (nd_start$pred[1] - nd_start$pred[3])) * 100
  )
}

tex5 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Racial Gap in Deferred Adjudication at Career Start vs.\\ Peak, by Offense Class}",
  "\\label{tab:gap_by_offense_out}",
  "\\begin{tabular}{lccc}",
  "\\hline\\hline",
  " & Career Start & Career Peak & Change \\\\",
  " & (case 1) & (case $N_{\\max}$) & (pp) \\\\",
  "\\hline",
  "\\textit{Panel A: White--Black gap} & & & \\\\"
)

for (r in t5_wb) {
  tex5 <- c(tex5,
    paste0("\\quad ", r$label, " & ",
           sprintf("%.1f", r$gap_start), " & ",
           sprintf("%.1f", r$gap_peak), " & ",
           sprintf("+%.1f", r$change), " \\\\"))
}

tex5 <- c(tex5,
  "\\hline",
  "\\textit{Panel B: White--Latino gap} & & & \\\\"
)

for (r in t5_wl) {
  tex5 <- c(tex5,
    paste0("\\quad ", r$label, " & ",
           sprintf("%.1f", r$gap_start), " & ",
           sprintf("%.1f", r$gap_peak), " & ",
           sprintf("+%.1f", r$change), " \\\\"))
}

tex5 <- c(tex5,
  "\\hline\\hline",
  "\\multicolumn{4}{l}{\\footnotesize \\textit{Notes:} Gap in predicted probability of deferred adjudication (percentage points).} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize Predicted from Specification (1) at career case 1 (start) and maximum career case $N$ (peak),} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize holding offense category and attorney type at modal values (appointed counsel).} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(tex5, file.path(DATA_DIR, "bexar_outtake_table5.tex"))

# ── Table 6: Prosecutor race split ───────────────────────────────────────────
# Requires: wru package; builds race predictions for outtake prosecutors

library(wru)

outtake_prosecutor_names <- dp_out |>
  distinct(`OUTTAKE-PROSECUTOR`) |>
  rename(name = `OUTTAKE-PROSECUTOR`) |>
  mutate(surname = str_extract(name, "^[^,]+") |> str_trim())

outtake_race_pred <- predict_race(
  voter.file = outtake_prosecutor_names,
  surname.only = TRUE
)

outtake_prosecutor_race <- outtake_race_pred |>
  mutate(pred_race = case_when(
    pred.his >= 0.5 ~ "Hispanic",
    pred.whi >= 0.5 ~ "White",
    pred.bla >= 0.5 ~ "Black",
    pred.asi >= 0.5 ~ "Asian",
    TRUE ~ "Other"
  )) |>
  select(`OUTTAKE-PROSECUTOR` = name, pros_pred_race = pred_race)

dp_out_pros <- dp_out |>
  left_join(outtake_prosecutor_race, by = "OUTTAKE-PROSECUTOR") |>
  mutate(PROSECUTOR = factor(`OUTTAKE-PROSECUTOR`))

df_pros <- dp_out_pros |> filter(!is.na(OFFENSE_CATEGORY2), !is.na(pros_pred_race))

fit_white_pros <- feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
  data = df_pros |> filter(pros_pred_race == "White"),
  fixef = "PROSECUTOR", family = binomial(), cluster = "PROSECUTOR")

fit_hisp_pros <- feglm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
  data = df_pros |> filter(pros_pred_race == "Hispanic"),
  fixef = "PROSECUTOR", family = binomial(), cluster = "PROSECUTOR")

pros_race_results <- bind_rows(
  broom::tidy(fit_white_pros, conf.int = TRUE) |> mutate(model = "WhitePros"),
  broom::tidy(fit_hisp_pros,  conf.int = TRUE) |> mutate(model = "HispanicPros")
) |>
  mutate(
    term_clean = case_when(
      term == "BLACK:OUTTAKE_CASE_N100" | term == "OUTTAKE_CASE_N100:BLACK" ~
        "Black $\\times$ Career Case $N$ (per 100)",
      term == "LATINO:OUTTAKE_CASE_N100" | term == "OUTTAKE_CASE_N100:LATINO" ~
        "Latino $\\times$ Career Case $N$ (per 100)",
      term == "BLACK"             ~ "Black",
      term == "LATINO"            ~ "Latino",
      term == "OUTTAKE_CASE_N100" ~ "Career Case $N$ (per 100)",
      TRUE ~ NA_character_
    ),
    est_cell = fmt_est(estimate, p.value),
    se_cell  = fmt_se(std.error)
  ) |>
  filter(!is.na(term_clean))

focal6 <- c("Black $\\times$ Career Case $N$ (per 100)",
            "Latino $\\times$ Career Case $N$ (per 100)",
            "Black", "Latino", "Career Case $N$ (per 100)")

t6_est <- pros_race_results |>
  select(term_clean, model, est_cell) |>
  pivot_wider(names_from = model, values_from = est_cell) |>
  mutate(term_clean = factor(term_clean, levels = focal6)) |>
  arrange(term_clean) |>
  mutate(across(any_of(c("WhitePros", "HispanicPros")), ~ replace_na(., "---")))

t6_se <- pros_race_results |>
  select(term_clean, model, se_cell) |>
  pivot_wider(names_from = model, values_from = se_cell) |>
  mutate(term_clean = factor(term_clean, levels = focal6)) |>
  arrange(term_clean) |>
  mutate(across(any_of(c("WhitePros", "HispanicPros")), ~ replace_na(., "")))

n_white_pros <- nrow(df_pros |> filter(pros_pred_race == "White"))
n_hisp_pros  <- nrow(df_pros |> filter(pros_pred_race == "Hispanic"))

tex6 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Racial Gap Growth by Prosecutor Race: White vs.\\ Hispanic Prosecutors}",
  "\\label{tab:pros_race_out}",
  "\\begin{tabular}{lcc}",
  "\\hline\\hline",
  " & White Prosecutors & Hispanic Prosecutors \\\\",
  "\\hline"
)

for (i in seq_len(nrow(t6_est))) {
  e <- t6_est[i, ]; s <- t6_se[i, ]
  tex6 <- c(tex6,
    paste0(e$term_clean, " & ", e$WhitePros, " & ", e$HispanicPros, " \\\\"),
    paste0(" & ", s$WhitePros, " & ", s$HispanicPros, " \\\\"),
    "& & \\\\"
  )
}

tex6 <- c(tex6,
  "\\hline",
  "Prosecutor FE & Yes & Yes \\\\",
  "Offense type \\& category FE & Yes & Yes \\\\",
  "Attorney type & Yes & Yes \\\\",
  paste0("$N$ & $", formatC(n_white_pros, big.mark = ","), "$ & $",
         formatC(n_hisp_pros, big.mark = ","), "$ \\\\"),
  "\\hline\\hline",
  "\\multicolumn{3}{l}{\\footnotesize \\textit{Notes:} Prosecutor fixed-effects logistic regression, estimated separately} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize by predicted prosecutor race (surname-based, \\texttt{wru} package). Outcome:} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize deferred adjudication. SEs clustered by prosecutor. Sample: 1991--2015.} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize $^{***}p<0.01$\\quad $^{**}p<0.05$\\quad $^{*}p<0.10$} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(tex6, file.path(DATA_DIR, "bexar_outtake_table6.tex"))

message("\nAll outtake tables saved to ", DATA_DIR)
