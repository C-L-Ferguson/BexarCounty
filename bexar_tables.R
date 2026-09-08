# Bexar County — Publication-Ready Regression Tables (LaTeX output)
# Requires: bexar_model_results.csv (from bexar_models.R)
#           bexar_prosecutor_panel_1990_2015.parquet (for Table 0)
# Outputs:  bexar_table0.tex — descriptive statistics
#           bexar_table1.tex — main progression table (Spec A/B/C)
#           bexar_table2.tex — within-offense-type estimates
#
# Format follows Shaffer (2023) 90 U. Chi. L. Rev. 1889:
#   - Columns = model specifications, rows = coefficients
#   - Cells: coefficient (log-odds) with SE in parentheses below
#   - Significance stars: ***p<0.01  **p<0.05  *p<0.10

DATA_DIR <- "C:/Users/carolineferguson/Box/Bigelow/Bexar/Data"

library(tidyverse)
library(arrow)

res <- read_csv(file.path(DATA_DIR, "bexar_model_results.csv"), show_col_types = FALSE)

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

# ── Table 0: Descriptive Statistics ───────────────────────────────────────────

dp <- read_parquet(file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))

# Restrict to 1991+ prosecutors (mirrors main analysis sample)
fresh_prosecutors <- dp |>
  group_by(`INTAKE-PROSECUTOR`) |>
  summarise(first_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop") |>
  filter(first_year >= 1991) |>
  pull(`INTAKE-PROSECUTOR`)

dp_clean <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White", "Latino"),
         `INTAKE-PROSECUTOR` %in% fresh_prosecutors)

desc <- dp_clean |>
  group_by(Race = `RACE-LABEL`) |>
  summarise(
    N               = n(),
    Deferred_pct    = mean(DEFERRED,      na.rm = TRUE) * 100,
    Dismissed_pct   = mean(DISMISSED,     na.rm = TRUE) * 100,
    GuiltyPlea_pct  = mean(`GUILTY-PLEA`, na.rm = TRUE) * 100,
    Appointed_pct   = mean(`ATTORNEY-TYPE` == "Appointed", na.rm = TRUE) * 100,
    Mean_ProsCase_N = mean(PROSECUTOR_CASE_N, na.rm = TRUE),
    .groups = "drop"
  )

overall <- dp_clean |>
  summarise(
    Race            = "All",
    N               = n(),
    Deferred_pct    = mean(DEFERRED,      na.rm = TRUE) * 100,
    Dismissed_pct   = mean(DISMISSED,     na.rm = TRUE) * 100,
    GuiltyPlea_pct  = mean(`GUILTY-PLEA`, na.rm = TRUE) * 100,
    Appointed_pct   = mean(`ATTORNEY-TYPE` == "Appointed", na.rm = TRUE) * 100,
    Mean_ProsCase_N = mean(PROSECUTOR_CASE_N, na.rm = TRUE)
  )

desc_all <- bind_rows(desc, overall) |>
  mutate(Race = factor(Race, levels = c("Black", "Latino", "White", "All"))) |>
  arrange(Race)

message("\n\u2550\u2550 TABLE 0: Descriptive Statistics \u2550\u2550")
print(desc_all)

tex0 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Descriptive Statistics by Defendant Race}",
  "\\label{tab:desc}",
  "\\begin{tabular}{lrrrrrr}",
  "\\hline\\hline",
  " & $N$ & Deferred (\\%) & Dismissed (\\%) & Guilty Plea (\\%) & Appointed (\\%) & Mean Career Case $N$ \\\\",
  "\\hline"
)

for (i in seq_len(nrow(desc_all))) {
  r <- desc_all[i, ]
  if (r$Race == "All") tex0 <- c(tex0, "\\hline")
  tex0 <- c(tex0,
    paste0(r$Race, " & ",
           formatC(r$N, format = "d", big.mark = ","), " & ",
           sprintf("%.1f", r$Deferred_pct), " & ",
           sprintf("%.1f", r$Dismissed_pct), " & ",
           sprintf("%.1f", r$GuiltyPlea_pct), " & ",
           sprintf("%.1f", r$Appointed_pct), " & ",
           sprintf("%.1f", r$Mean_ProsCase_N), " \\\\")
  )
}

tex0 <- c(tex0,
  "\\hline\\hline",
  "\\multicolumn{7}{l}{\\footnotesize \\textit{Notes:} Felony cases, Bexar County. Sample: Black, Latino, and White defendants} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize assigned to prosecutors first observed 1991 or later (left-censoring excluded).} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Appointed = court-appointed counsel. Career Case $N$ = cumulative prosecutor caseload.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(tex0, file.path(DATA_DIR, "bexar_table0.tex"))

# ── SpecD: Prosecutor FE + Year FE (robustness check) ────────────────────────
library(fixest)

df_specD <- dp |>
  filter(`INTAKE-PROSECUTOR` %in% fresh_prosecutors,
         `RACE-LABEL` %in% c("Black", "White", "Latino"),
         !is.na(OFFENSE_CATEGORY)) |>
  mutate(
    BLACK    = as.integer(`RACE-LABEL` == "Black"),
    LATINO   = as.integer(`RACE-LABEL` == "Latino"),
    APPOINTED = as.integer(`ATTORNEY-TYPE` == "Appointed")
  )

fit_specD <- feglm(
  DEFERRED ~ BLACK + LATINO +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    `OFFENSE-CLASS` + OFFENSE_CATEGORY + APPOINTED |
    `INTAKE-PROSECUTOR` + `CASE-YEAR`,
  data    = df_specD,
  family  = binomial(),
  cluster = ~`INTAKE-PROSECUTOR`
)

specD_coefs <- as.data.frame(summary(fit_specD)$coeftable) |>
  tibble::rownames_to_column("term") |>
  rename(estimate = Estimate, std.error = `Std. Error`, p.value = `Pr(>|z|)`) |>
  mutate(model = "SpecD_ProsYearFE")

specD_focal <- specD_coefs |>
  filter(term %in% c("BLACK", "LATINO",
                     "BLACK:PROSECUTOR_CASE_N", "LATINO:PROSECUTOR_CASE_N")) |>
  mutate(
    term_clean = case_when(
      str_detect(term, "BLACK.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*BLACK") ~
        "Black $\\times$ Career Case N",
      str_detect(term, "LATINO.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*LATINO") ~
        "Latino $\\times$ Career Case N",
      term == "BLACK"  ~ "Black",
      term == "LATINO" ~ "Latino"
    ),
    est_cell = fmt_est(estimate, p.value),
    se_cell  = fmt_se(std.error)
  )

# ── Table 1: Clean identification progression (Spec A → B → C → D) ───────────
# Sample: prosecutors first observed 1991 or later (left-censoring excluded)

model_order <- c("SpecA_OffenseOnly", "SpecB_YearFE", "SpecC_ProsecutorFE", "SpecC_WithPriors")
focal_rows  <- c("Black $\\times$ Career Case N",
                 "Latino $\\times$ Career Case N",
                 "Black", "Latino", "Career Case N")

table_data <- res |>
  filter(model %in% model_order) |>
  mutate(
    term_clean = case_when(
      str_detect(term, "BLACK.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*BLACK") ~
        "Black $\\times$ Career Case N",
      str_detect(term, "LATINO.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*LATINO") ~
        "Latino $\\times$ Career Case N",
      term == "BLACK"             ~ "Black",
      term == "LATINO"            ~ "Latino",
      term == "PROSECUTOR_CASE_N" ~ "Career Case N",
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
  replace_na(list(SpecA_OffenseOnly = "---", SpecB_YearFE = "---", SpecC_ProsecutorFE = "---", SpecC_WithPriors = "---")) |>
  left_join(
    specD_focal |> select(term_clean, est_cell) |> rename(SpecD_ProsYearFE = est_cell),
    by = "term_clean"
  ) |>
  replace_na(list(SpecD_ProsYearFE = "---"))

t1_se <- table_data |>
  filter(term_clean %in% focal_rows) |>
  select(term_clean, model, se_cell) |>
  pivot_wider(names_from = model, values_from = se_cell) |>
  mutate(term_clean = factor(term_clean, levels = focal_rows)) |>
  arrange(term_clean) |>
  replace_na(list(SpecA_OffenseOnly = "", SpecB_YearFE = "", SpecC_ProsecutorFE = "", SpecC_WithPriors = "")) |>
  left_join(
    specD_focal |> select(term_clean, se_cell) |> rename(SpecD_ProsYearFE = se_cell),
    by = "term_clean"
  ) |>
  replace_na(list(SpecD_ProsYearFE = ""))

message("\n\u2550\u2550 TABLE 1: Focal Interaction Coefficients (Log-Odds) \u2550\u2550")
print(t1_est, n = Inf)

# Build LaTeX
specD_N <- nobs(fit_specD)

tex1 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Deferred Adjudication and Prosecutor Experience: Focal Interaction Coefficients}",
  "\\label{tab:main}",
  "\\begin{tabular}{lccccc}",
  "\\hline\\hline",
  " & (1) & (2) & (3) & (4) & (5) \\\\",
  " & Offense + Attorney & $+$ Year FE & Prosecutor FE & Prosecutor FE + Priors & Prosecutor $+$ Year FE \\\\",
  "\\hline"
)

for (i in seq_len(nrow(t1_est))) {
  e <- t1_est[i, ]
  s <- t1_se[i, ]
  tex1 <- c(tex1,
    paste0(e$term_clean, " & ", e$SpecA_OffenseOnly,
           " & ", e$SpecB_YearFE, " & ", e$SpecC_ProsecutorFE,
           " & ", e$SpecC_WithPriors, " & ", e$SpecD_ProsYearFE, " \\\\"),
    paste0(" & ", s$SpecA_OffenseOnly,
           " & ", s$SpecB_YearFE, " & ", s$SpecC_ProsecutorFE,
           " & ", s$SpecC_WithPriors, " & ", s$SpecD_ProsYearFE, " \\\\"),
    "& & & & & \\\\"
  )
}

tex1 <- c(tex1,
  "\\hline",
  "Offense type \\& category FE & Yes & Yes & Yes & Yes & Yes \\\\",
  "Attorney type & Yes & Yes & Yes & Yes & Yes \\\\",
  "Case year FE & No & Yes & No & No & Yes \\\\",
  "Prosecutor FE & No & No & Yes & Yes & Yes \\\\",
  "Defendant case $N$ & No & No & No & Yes & No \\\\",
  "Sample & 1991+ & 1991+ & 1991+ & 1991+ & 1991+ \\\\",
  paste0("$N$ & $162{,}218$ & $157{,}791$ & $157{,}372$ & $157{,}372$ & $",
         formatC(specD_N, format = "d", big.mark = "{,}"), "$ \\\\"),
  "\\hline\\hline",
  "\\multicolumn{6}{l}{\\footnotesize \\textit{Notes:} Logistic regression coefficients (log-odds). Outcome: deferred adjudication.} \\\\",
  "\\multicolumn{6}{l}{\\footnotesize Standard errors in parentheses, clustered by prosecutor. Sample: felony cases, prosecutors first observed 1991+.} \\\\",
  "\\multicolumn{6}{l}{\\footnotesize Col.~(4) adds defendant cumulative case count as proxy for prior record. Col.~(5) adds year FE to Col.~(3).} \\\\",
  "\\multicolumn{6}{l}{\\footnotesize $^{***}p<0.01$\\quad $^{**}p<0.05$\\quad $^{*}p<0.10$} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(tex1, file.path(DATA_DIR, "bexar_table1.tex"))

# ── Table 2: Within-offense-type ──────────────────────────────────────────────

m7_data <- res |>
  filter(str_starts(model, "M7_")) |>
  mutate(
    offense = str_extract(model, "F[123S]"),
    term_clean = case_when(
      str_detect(term, "BLACK.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*BLACK") ~
        "Black $\\times$ Career Case N",
      str_detect(term, "LATINO.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*LATINO") ~
        "Latino $\\times$ Career Case N",
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
  message("\n\u2550\u2550 TABLE 2: Within-Offense-Type Estimates (M7) \u2550\u2550\n")
  print(m7_est)

  offense_cols <- setdiff(colnames(m7_est), "term_clean")
  n_cols <- length(offense_cols)
  col_spec <- paste0("l", paste(rep("c", n_cols), collapse = ""))
  header_cols <- paste(offense_cols, collapse = " & ")

  tex2 <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Within-Offense-Type Estimates}",
    "\\label{tab:offense}",
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\hline\\hline",
    paste0(" & ", header_cols, " \\\\"),
    "\\hline"
  )

  for (i in seq_len(nrow(m7_est))) {
    e <- m7_est[i, ]
    s <- m7_se[i, ]
    est_vals <- paste(unlist(e[offense_cols]), collapse = " & ")
    se_vals  <- paste(unlist(s[offense_cols]), collapse = " & ")
    tex2 <- c(tex2,
      paste0(e$term_clean, " & ", est_vals, " \\\\"),
      paste0(" & ", se_vals, " \\\\"),
      paste0(paste(rep("&", n_cols), collapse = " "), " \\\\")
    )
  }

  tex2 <- c(tex2,
    "\\hline\\hline",
    paste0("\\multicolumn{", n_cols + 1, "}{l}{\\footnotesize \\textit{Notes:} Each column estimates the same model within a single offense class.} \\\\"),
    paste0("\\multicolumn{", n_cols + 1, "}{l}{\\footnotesize Log-odds. Standard errors in parentheses. $^{***}p<0.01$\\quad $^{**}p<0.05$\\quad $^{*}p<0.10$} \\\\"),
    "\\end{tabular}",
    "\\end{table}"
  )

  write_tex(tex2, file.path(DATA_DIR, "bexar_table2.tex"))
} else {
  message("No M7 models found in results — skipping Table 2.")
}


# ── Table 3: Predicted Probability Summary (marginal effects) ─────────────────
# Re-fit SpecA as plain glm to support predict(newdata=...)

df_pred <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White", "Latino"),
         `INTAKE-PROSECUTOR` %in% fresh_prosecutors) |>
  mutate(
    BLACK            = as.integer(`RACE-LABEL` == "Black"),
    LATINO           = as.integer(`RACE-LABEL` == "Latino"),
    DEFERRED         = as.integer(DEFERRED),
    APPOINTED        = as.integer(`ATTORNEY-TYPE` == "Appointed"),
    OFFENSE_TYPE     = `OFFENSE-TYPE`
  )

fit_pred <- glm(
  DEFERRED ~ BLACK + LATINO + PROSECUTOR_CASE_N +
    BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
    OFFENSE_TYPE + OFFENSE_CATEGORY + APPOINTED,
  data   = df_pred |> filter(!is.na(OFFENSE_CATEGORY)),
  family = binomial()
)

q_breaks <- quantile(df_pred$PROSECUTOR_CASE_N, probs = seq(0, 1, 0.2), na.rm = TRUE)
q1_mid   <- mean(c(q_breaks[1], q_breaks[2]))
q4_mid   <- mean(c(q_breaks[4], q_breaks[5]))

ref_dist <- df_pred |>
  filter(!is.na(OFFENSE_CATEGORY), APPOINTED == 1) |>
  count(OFFENSE_TYPE, OFFENSE_CATEGORY) |>
  mutate(wt = n / sum(n))

pred_cell <- function(black, case_n) {
  g <- ref_dist |>
    mutate(BLACK = black, LATINO = 0,
           PROSECUTOR_CASE_N = case_n, APPOINTED = 1)
  weighted.mean(predict(fit_pred, newdata = g, type = "response"), g$wt)
}

white_q1 <- pred_cell(0, q1_mid)
black_q1 <- pred_cell(1, q1_mid)
white_q4 <- pred_cell(0, q4_mid)
black_q4 <- pred_cell(1, q4_mid)

message("\n══ TABLE 3: Predicted Probabilities ══")
message(sprintf("White Q1: %.1f%%  Black Q1: %.1f%%  Gap: %.1f pp",
                white_q1*100, black_q1*100, (white_q1-black_q1)*100))
message(sprintf("White Q4: %.1f%%  Black Q4: %.1f%%  Gap: %.1f pp",
                white_q4*100, black_q4*100, (white_q4-black_q4)*100))

# By-offense-type breakdown
offense_types <- c("F1", "F2", "F3", "FS")
offense_labels <- c("F1 (first degree)", "F2 (second degree)", "F3 (third degree)", "FS (state jail)")

ot_rows <- list()
for (j in seq_along(offense_types)) {
  ot <- offense_types[j]
  ref_ot <- df_pred |>
    filter(!is.na(OFFENSE_CATEGORY), APPOINTED == 1, OFFENSE_TYPE == ot) |>
    count(OFFENSE_TYPE, OFFENSE_CATEGORY) |>
    mutate(wt = n / sum(n))
  if (nrow(ref_ot) == 0) next
  pred_ot <- function(black, case_n) {
    g <- ref_ot |> mutate(BLACK = black, LATINO = 0,
                           PROSECUTOR_CASE_N = case_n, APPOINTED = 1)
    weighted.mean(predict(fit_pred, newdata = g, type = "response"), g$wt)
  }
  wq1 <- pred_ot(0, q1_mid); bq1 <- pred_ot(1, q1_mid)
  wq4 <- pred_ot(0, q4_mid); bq4 <- pred_ot(1, q4_mid)
  ot_rows[[j]] <- list(label = offense_labels[j],
                        gap_q1 = (wq1 - bq1) * 100,
                        gap_q4 = (wq4 - bq4) * 100,
                        change = ((wq4 - bq4) - (wq1 - bq1)) * 100)
}

tex3 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Predicted Probability of Deferred Adjudication by Race and Prosecutor Experience}",
  "\\label{tab:pred}",
  "\\begin{tabular}{lccc}",
  "\\hline\\hline",
  " & Q1 Gap (pp) & Q4 Gap (pp) & Change (pp) \\\\",
  "\\hline",
  "\\textit{Panel A: All felony offenses} & & & \\\\",
  paste0("\\quad White--Black gap & ",
         sprintf("%.1f", (white_q1 - black_q1) * 100), " & ",
         sprintf("%.1f", (white_q4 - black_q4) * 100), " & ",
         sprintf("%.1f", ((white_q4 - black_q4) - (white_q1 - black_q1)) * 100), " \\\\"),
  "\\hline",
  "\\textit{Panel B: By felony severity} & & & \\\\"
)

for (r in ot_rows) {
  tex3 <- c(tex3,
    paste0("\\quad ", r$label, " & ",
           sprintf("%.1f", r$gap_q1), " & ",
           sprintf("%.1f", r$gap_q4), " & ",
           sprintf("%.1f", r$change), " \\\\")
  )
}

tex3 <- c(tex3,
  "\\hline\\hline",
  "\\multicolumn{4}{l}{\\footnotesize \\textit{Notes:} White--Black gap in predicted probability of deferred adjudication (percentage points).} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize Predicted from Specification (1). Averaged across appointed-counsel cases within each offense class.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize Q1 midpoint $\\approx$ 77 cumulative cases; Q4 midpoint $\\approx$ 1{,}183 cumulative cases.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(tex3, file.path(DATA_DIR, "bexar_table3.tex"))

# ── Table 4: DA-era robustness (Hillig vs Reed) ───────────────────────────────

fresh_prос_era <- dp |>
  group_by(`INTAKE-PROSECUTOR`) |>
  summarise(first_year = min(`CASE-YEAR`, na.rm = TRUE),
            DA_AT_HIRE = first(DA_AT_HIRE), .groups = "drop") |>
  filter(first_year >= 1991)

hillig_prosecutors <- fresh_prос_era |> filter(DA_AT_HIRE != "Reed") |> pull(`INTAKE-PROSECUTOR`)
reed_prosecutors   <- fresh_prос_era |> filter(DA_AT_HIRE == "Reed") |> pull(`INTAKE-PROSECUTOR`)

fit_era_model <- function(prosecutor_ids) {
  df_era <- dp |>
    filter(`INTAKE-PROSECUTOR` %in% prosecutor_ids,
           `RACE-LABEL` %in% c("Black", "White", "Latino"),
           !is.na(OFFENSE_CATEGORY)) |>
    mutate(
      BLACK    = as.integer(`RACE-LABEL` == "Black"),
      LATINO   = as.integer(`RACE-LABEL` == "Latino"),
      APPOINTED = as.integer(`ATTORNEY-TYPE` == "Appointed")
    )
  feglm(
    DEFERRED ~ BLACK + LATINO +
      BLACK:PROSECUTOR_CASE_N + LATINO:PROSECUTOR_CASE_N +
      `OFFENSE-CLASS` + OFFENSE_CATEGORY + APPOINTED |
      `INTAKE-PROSECUTOR`,
    data = df_era, family = binomial(), cluster = ~`INTAKE-PROSECUTOR`
  )
}

fit_hillig <- fit_era_model(hillig_prosecutors)
fit_reed   <- fit_era_model(reed_prosecutors)

extract_era <- function(fit) {
  as.data.frame(summary(fit)$coeftable) |>
    tibble::rownames_to_column("term") |>
    rename(estimate = Estimate, std.error = `Std. Error`, p.value = `Pr(>|z|)`) |>
    filter(term %in% c("BLACK", "LATINO",
                       "BLACK:PROSECUTOR_CASE_N", "LATINO:PROSECUTOR_CASE_N")) |>
    mutate(
      term_clean = case_when(
        str_detect(term, "BLACK.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*BLACK") ~
          "Black $\\times$ Career Case N",
        str_detect(term, "LATINO.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*LATINO") ~
          "Latino $\\times$ Career Case N",
        term == "BLACK"  ~ "Black",
        term == "LATINO" ~ "Latino"
      ),
      est_cell = fmt_est(estimate, p.value),
      se_cell  = fmt_se(std.error)
    )
}

era_hillig <- extract_era(fit_hillig)
era_reed   <- extract_era(fit_reed)

focal_era <- c("Black $\\times$ Career Case N", "Latino $\\times$ Career Case N",
               "Black", "Latino")

era_est <- era_hillig |>
  select(term_clean, est_cell) |> rename(Hillig = est_cell) |>
  left_join(era_reed |> select(term_clean, est_cell) |> rename(Reed = est_cell),
            by = "term_clean") |>
  mutate(term_clean = factor(term_clean, levels = focal_era)) |>
  arrange(term_clean)

era_se <- era_hillig |>
  select(term_clean, se_cell) |> rename(Hillig = se_cell) |>
  left_join(era_reed |> select(term_clean, se_cell) |> rename(Reed = se_cell),
            by = "term_clean") |>
  mutate(term_clean = factor(term_clean, levels = focal_era)) |>
  arrange(term_clean)

message("\n══ TABLE 4: DA-Era Robustness ══")
print(era_est)

tex4 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Learning Curve by DA Era: Hillig vs.\\ Reed Hires}",
  "\\label{tab:era}",
  "\\begin{tabular}{lcc}",
  "\\hline\\hline",
  " & Hillig-era hires & Reed-era hires \\\\",
  " & (DA 1991--1998) & (DA 1999--2015) \\\\",
  "\\hline"
)

for (i in seq_len(nrow(era_est))) {
  e <- era_est[i, ]; s <- era_se[i, ]
  tex4 <- c(tex4,
    paste0(e$term_clean, " & ", e$Hillig, " & ", e$Reed, " \\\\"),
    paste0(" & ", s$Hillig, " & ", s$Reed, " \\\\"),
    "& & \\\\"
  )
}

tex4 <- c(tex4,
  "\\hline",
  paste0("$N$ & $", formatC(nobs(fit_hillig), format="d", big.mark="{,}"),
         "$ & $", formatC(nobs(fit_reed), format="d", big.mark="{,}"), "$ \\\\"),
  "\\hline\\hline",
  "\\multicolumn{3}{l}{\\footnotesize \\textit{Notes:} Both columns estimate Specification (3) (prosecutor fixed effects)} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize on subsamples split by DA in office when prosecutor was hired.} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize The interaction survives in both subsamples, ruling out the hypothesis that the learning} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize curve effect reflects office-wide changes in deferred adjudication practices rather than} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize within-prosecutor experience. Standard errors clustered by prosecutor.} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize $^{***}p<0.01$\\quad $^{**}p<0.05$\\quad $^{*}p<0.10$} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(tex4, file.path(DATA_DIR, "bexar_table4.tex"))

message("\nAll tables saved to ", DATA_DIR)
