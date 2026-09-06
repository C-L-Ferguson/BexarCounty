# Bexar County — Publication-Ready Regression Tables
# Requires: bexar_model_results.csv (from bexar_models.R)
# Outputs:  bexar_table1.csv  — progression table (M1–M5), focal coefficients
#           bexar_table1.docx — Word-ready version (if officer package available)
#
# Format follows Shaffer (2023) 90 U. Chi. L. Rev. 1889:
#   - Columns = model specifications, rows = coefficients
#   - Cells: coefficient (log-odds) with SE in parentheses below
#   - Significance stars: ***p<0.01  **p<0.05  *p<0.10
#   - Bottom rows: N, controls included (Y/N)

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"

library(tidyverse)

res <- read_csv(file.path(DATA_DIR, "bexar_model_results.csv"), show_col_types = FALSE)

# ── Stars helper ──────────────────────────────────────────────────────────────

stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )
}

fmt_coef <- function(est, se, p) {
  paste0(round(est, 3), stars(p), "\n(", round(se, 3), ")")
}

# ── Table 1: Progression of focal interactions (M1–M5) ───────────────────────
# Rows: BLACK × PROSECUTOR_CASE_N, LATINO × PROSECUTOR_CASE_N
# Columns: M1_Baseline, M2_OffenseFE, M3_CourtYearFE, M4_Full_NoBond, M5_ProsecutorFE

focal_terms <- c("BLACK:PROSECUTOR_CASE_N", "PROSECUTOR_CASE_N:BLACK",
                 "LATINO:PROSECUTOR_CASE_N", "PROSECUTOR_CASE_N:LATINO",
                 "BLACK", "LATINO")

model_order <- c("M1_Baseline", "M2_OffenseFE", "M3_CourtYearFE",
                 "M4_Full_NoBond", "M5_ProsecutorFE")

model_labels <- c(
  M1_Baseline      = "(1)\nBaseline",
  M2_OffenseFE     = "(2)\n+ Offense FE",
  M3_CourtYearFE   = "(3)\n+ Court/Year FE",
  M4_Full_NoBond   = "(4)\nFull Model",
  M5_ProsecutorFE  = "(5)\n+ Prosecutor FE"
)

table_data <- res |>
  filter(model %in% model_order) |>
  mutate(
    # Normalize interaction term name
    term_clean = case_when(
      str_detect(term, "BLACK.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*BLACK") ~ "Black × Career Case N",
      str_detect(term, "LATINO.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*LATINO") ~ "Latino × Career Case N",
      term == "BLACK"  ~ "Black",
      term == "LATINO" ~ "Latino",
      term == "PROSECUTOR_CASE_N" ~ "Career Case N",
      TRUE ~ term
    ),
    cell = fmt_coef(estimate, std.error, p.value),
    model = factor(model, levels = model_order)
  )

# Focal rows to show
focal_rows <- c("Black × Career Case N", "Latino × Career Case N",
                "Black", "Latino", "Career Case N")

t1_wide <- table_data |>
  filter(term_clean %in% focal_rows) |>
  select(term_clean, model, cell) |>
  pivot_wider(names_from = model, values_from = cell, values_fill = "—") |>
  # Order rows
  mutate(term_clean = factor(term_clean, levels = focal_rows)) |>
  arrange(term_clean)

# Add control rows
controls <- tribble(
  ~term_clean,            ~M1_Baseline, ~M2_OffenseFE, ~M3_CourtYearFE, ~M4_Full_NoBond, ~M5_ProsecutorFE,
  "── Controls ──",       "",           "",             "",              "",               "",
  "Offense type FE",      "No",         "Yes",          "Yes",           "Yes",            "Yes",
  "Offense category FE",  "No",         "Yes",          "Yes",           "Yes",            "Yes",
  "Court FE",             "No",         "No",           "Yes",           "Yes",            "No",
  "Case year FE",         "No",         "No",           "Yes",           "Yes",            "No",
  "Attorney type",        "No",         "No",           "No",            "Yes",            "Yes",
  "Prosecutor FE",        "No",         "No",           "No",            "No",             "Yes"
)

# N per model
n_row <- res |>
  filter(model %in% model_order, term == "(Intercept)" | row_number() == 1) |>
  group_by(model) |>
  slice(1) |>
  ungroup()

# If nobs is not stored, note that N is in the model output
# Build final table
colnames(t1_wide) <- c("Coefficient", model_labels[levels(table_data$model)])

message("\n══ TABLE 1: Focal Interaction Coefficients (Log-Odds) ══")
message("Key test: Black × Career Case N — positive = gap widens with experience\n")
print(t1_wide, n = Inf)

write_csv(t1_wide, file.path(DATA_DIR, "bexar_table1.csv"))
message("\nSaved: bexar_table1.csv")
message("Note: cells show log-odds coefficient with SE in parentheses.")
message("Stars: ***p<0.01  **p<0.05  *p<0.10")

# ── Table 2: Within-offense-type (M7) ─────────────────────────────────────────
# One column per offense type, same focal rows

m7_models <- res |>
  filter(str_starts(model, "M7_")) |>
  mutate(
    offense = str_remove(model, "M7_(.*)_only$") |> str_extract("F[123S]"),
    term_clean = case_when(
      str_detect(term, "BLACK.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*BLACK") ~ "Black × Career Case N",
      str_detect(term, "LATINO.*PROSECUTOR_CASE_N|PROSECUTOR_CASE_N.*LATINO") ~ "Latino × Career Case N",
      term == "BLACK"  ~ "Black",
      term == "LATINO" ~ "Latino",
      TRUE ~ term
    ),
    cell = fmt_coef(estimate, std.error, p.value)
  ) |>
  filter(term_clean %in% c("Black × Career Case N", "Latino × Career Case N",
                            "Black", "Latino")) |>
  select(term_clean, offense, cell) |>
  pivot_wider(names_from = offense, values_from = cell, values_fill = "—")

message("\n══ TABLE 2: Within-Offense-Type Estimates (M7) ══\n")
print(m7_models)
write_csv(m7_models, file.path(DATA_DIR, "bexar_table2_offense_type.csv"))
message("Saved: bexar_table2_offense_type.csv")

# ── Optional: Word export via officer ────────────────────────────────────────
if (requireNamespace("officer", quietly = TRUE) &&
    requireNamespace("flextable", quietly = TRUE)) {

  library(officer)
  library(flextable)

  make_ft <- function(df, title) {
    ft <- flextable(df) |>
      set_caption(caption = title) |>
      theme_booktabs() |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Times New Roman", part = "all") |>
      bold(part = "header") |>
      align(align = "center", part = "header") |>
      align(j = 1, align = "left", part = "body") |>
      autofit()
    ft
  }

  doc <- read_docx() |>
    body_add_par("Table 1. Deferred Adjudication and Prosecutor Experience: Focal Interaction Coefficients",
                 style = "heading 1") |>
    body_add_par(paste0(
      "Logistic regression coefficients (log-odds). Outcome: deferred adjudication. ",
      "Sample: felony cases with identified prosecutors handling 50+ cases, 1990–2015 (N varies by model). ",
      "Standard errors in parentheses. ***p<0.01  **p<0.05  *p<0.10."
    ), style = "Normal") |>
    body_add_flextable(make_ft(t1_wide, "")) |>
    body_add_break() |>
    body_add_par("Table 2. Within-Offense-Type Estimates (M7)",
                 style = "heading 1") |>
    body_add_par(paste0(
      "Each column estimates the same model within a single offense class. ",
      "Logistic regression, log-odds. Standard errors in parentheses. ",
      "***p<0.01  **p<0.05  *p<0.10."
    ), style = "Normal") |>
    body_add_flextable(make_ft(m7_models, ""))

  print(doc, target = file.path(DATA_DIR, "bexar_tables.docx"))
  message("Saved: bexar_tables.docx  (requires officer + flextable)")

} else {
  message("\nInstall 'officer' and 'flextable' for Word export:")
  message("  install.packages(c('officer', 'flextable'))")
}
