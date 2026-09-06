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

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"

library(tidyverse)
library(arrow)

res <- read_csv(file.path(DATA_DIR, "bexar_model_results.csv"), show_col_types = FALSE)

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

# ── Helpers ───────────────────────────────────────────────────────────────────

stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )
}

fmt_coef <- function(est, se, p) {
  paste0(
    sprintf("%.4f", est), stars(p),
    " \\\\\\ \n& (", sprintf("%.4f", se), ")"
  )
}

write_tex <- function(lines, path) {
  writeLines(lines, con = path)
  message("Saved: ", path)
}

# ── Table 1: Clean identification progression (Spec A → B → C) ───────────────
# Sample: prosecutors first observed 1991 or later (left-censoring excluded)

model_order <- c("SpecA_OffenseOnly", "SpecB_YearFE", "SpecC_ProsecutorFE")
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
    cell  = fmt_coef(estimate, std.error, p.value),
    model = factor(model, levels = model_order)
  ) |>
  filter(!is.na(term_clean))

t1_wide <- table_data |>
  filter(term_clean %in% focal_rows) |>
  select(term_clean, model, cell) |>
  pivot_wider(names_from = model, values_from = cell) |>
  mutate(term_clean = factor(term_clean, levels = focal_rows)) |>
  arrange(term_clean) |>
  replace_na(list(SpecA_OffenseOnly  = "---",
                  SpecB_YearFE       = "---",
                  SpecC_ProsecutorFE = "---"))

message("\n\u2550\u2550 TABLE 1: Focal Interaction Coefficients (Log-Odds) \u2550\u2550")
print(t1_wide, n = Inf)

# Build LaTeX
tex1 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Deferred Adjudication and Prosecutor Experience: Focal Interaction Coefficients}",
  "\\label{tab:main}",
  "\\begin{tabular}{lccc}",
  "\\hline\\hline",
  " & (1) & (2) & (3) \\\\",
  " & Offense + Attorney & $+$ Year FE & Prosecutor FE \\\\",
  "\\hline"
)

for (i in seq_len(nrow(t1_wide))) {
  r <- t1_wide[i, ]
  tex1 <- c(tex1,
    paste0(r$term_clean, " & ", r$SpecA_OffenseOnly,
           " & ", r$SpecB_YearFE,
           " & ", r$SpecC_ProsecutorFE, " \\\\"),
    "& & & \\\\"
  )
}

tex1 <- c(tex1,
  "\\hline",
  "Offense type \\& category FE & Yes & Yes & Yes \\\\",
  "Attorney type & Yes & Yes & Yes \\\\",
  "Case year FE & No & Yes & No \\\\",
  "Prosecutor FE & No & No & Yes \\\\",
  "Sample & 1991+ & 1991+ & 1991+ \\\\",
  "\\hline\\hline",
  "\\multicolumn{4}{l}{\\footnotesize \\textit{Notes:} Logistic regression coefficients (log-odds). Outcome: deferred adjudication.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize Standard errors in parentheses. Sample: felony cases, prosecutors first observed 1991+.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize $^{***}p<0.01$\\quad $^{**}p<0.05$\\quad $^{*}p<0.10$} \\\\",
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
    cell = fmt_coef(estimate, std.error, p.value)
  ) |>
  filter(!is.na(term_clean)) |>
  select(term_clean, offense, cell) |>
  pivot_wider(names_from = offense, values_from = cell)

if (nrow(m7_data) > 0) {
  message("\n\u2550\u2550 TABLE 2: Within-Offense-Type Estimates (M7) \u2550\u2550\n")
  print(m7_data)

  offense_cols <- setdiff(colnames(m7_data), "term_clean")
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

  for (i in seq_len(nrow(m7_data))) {
    r <- m7_data[i, ]
    vals <- paste(unlist(r[offense_cols]), collapse = " & ")
    tex2 <- c(tex2,
      paste0(r$term_clean, " & ", vals, " \\\\"),
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

message("\nAll tables saved to ", DATA_DIR)
