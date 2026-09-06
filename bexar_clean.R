# Bexar County Criminal Court Data — Merge, Clean, Export
# Run from the directory containing the CSV files.
# Outputs: bexar_cleaned.parquet, bexar_panel_1990_2021.parquet, bexar_cleaned.csv
#
# install.packages(c("tidyverse", "arrow", "lubridate"))

library(tidyverse)
library(arrow)
library(lubridate)

DATA_DIR  <- "C:/Users/carol/Box/Bigelow/Bexar/Data"
OUTPUT_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"  # where .parquet/.csv outputs go

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── 1. Load and merge ─────────────────────────────────────────────────────────

files <- list.files(DATA_DIR, pattern = "^DC_cjjorad_", full.names = TRUE)
if (length(files) == 0) stop("No DC_cjjorad_*.csv files found in ", DATA_DIR)
message("Found ", length(files), " CSV files: ", paste(basename(files), collapse = ", "))

frames <- map(files, function(f) {
  df <- read_csv(f, col_types = cols(.default = "c"), show_col_types = FALSE)
  df <- select(df, -starts_with("Unnamed"))
  message("  Loaded ", basename(f), ": ", nrow(df), " rows, ", ncol(df), " cols")
  df
})

# Check schema consistency
col_sets <- map(frames, colnames)
ref_cols  <- col_sets[[1]]
walk2(files, col_sets, function(f, cols) {
  extra   <- setdiff(cols, ref_cols)
  missing <- setdiff(ref_cols, cols)
  if (length(extra) | length(missing))
    message("  WARNING schema drift in ", basename(f),
            " extra=", paste(extra, collapse=","),
            " missing=", paste(missing, collapse=","))
})

df <- bind_rows(frames)
message("\nMerged total: ", nrow(df), " rows, ", ncol(df), " columns")

# ── 2. Duplicate check ────────────────────────────────────────────────────────

key <- "CASE-CAUSE-NBR"
n_dups <- sum(duplicated(df[[key]]))
if (n_dups > 0) {
  message("WARNING: ", n_dups, " duplicate ", key, " rows. Keeping first.")
  df <- df[!duplicated(df[[key]]), ]
} else {
  message("No duplicate ", key, " values — clean primary key.")
}

# ── 3. Strip whitespace from all character columns ───────────────────────────

df <- df |> mutate(across(where(is.character), str_trim))

# ── 4. Parse dates ────────────────────────────────────────────────────────────

date_cols <- c("CASE-DATE", "OFFENSE-DATE", "DISPOSITION-DATE",
               "CUSTODY-DATE", "SENTENCE-START-DATE", "SENTENCE-END-DATE", "BIRTHDATE")

for (col in intersect(date_cols, colnames(df))) {
  df[[col]] <- parse_date_time(df[[col]], orders = c("mdy", "ymd", "dmy"), quiet = TRUE)
}

df <- df |>
  mutate(
    `CASE-YEAR`   = year(`CASE-DATE`),
    `CASE-DECADE` = floor(`CASE-YEAR` / 10) * 10L,
    `DAYS-TO-CASE` = as.numeric(difftime(`CASE-DATE`, `OFFENSE-DATE`, units = "days"))
  )

# ── 5. Recode RACE ────────────────────────────────────────────────────────────

df <- df |>
  mutate(`RACE-LABEL` = case_match(
    RACE,
    "B" ~ "Black",
    "W" ~ "White",
    "L" ~ "Latino",
    .default = "Other/Unknown"
  ))

# ── 6. Recode SEX ─────────────────────────────────────────────────────────────

df <- df |>
  mutate(`SEX-LABEL` = case_match(
    SEX,
    "M" ~ "M",
    "F" ~ "F",
    .default = "Unknown"
  ))

# ── 7. Recode ATTORNEY-APPOINTED-RETAINED ────────────────────────────────────

df <- df |>
  mutate(`ATTORNEY-TYPE` = case_match(
    `ATTORNEY-APPOINTED-RETAINED`,
    "A" ~ "Appointed",
    "H" ~ "Hired",
    "S" ~ "ProSe",
    .default = NA_character_
  ))

# ── 8. Recode OFFENSE-TYPE ───────────────────────────────────────────────────

df <- df |>
  mutate(
    `OFFENSE-CLASS` = case_match(
      `OFFENSE-TYPE`,
      "F1" ~ "F1",
      "F2" ~ "F2",
      "F3" ~ "F3",
      "FS" ~ "FS",
      c("MA", "MB", "MC", "M ") ~ "Misdemeanor",
      .default = "Other Felony"
    ),
    `OFFENSE-SEVERITY` = case_match(
      `OFFENSE-CLASS`,
      "F1" ~ 1L, "F2" ~ 2L, "F3" ~ 3L, "FS" ~ 4L,
      "Other Felony" ~ 5L, "Misdemeanor" ~ 6L,
      .default = NA_integer_
    )
  )

# ── 9. Parse ORIGINAL-SENTENCE → total days ──────────────────────────────────

parse_sentence <- function(s) {
  # Format: NNNYRnnMTHnnDYSnnnHR  e.g. "002YR00MTH00DYS000HR"
  yr  <- as.numeric(str_extract(s, "(\\d+)(?=\\s*YR)",  group = 1))
  mth <- as.numeric(str_extract(s, "(\\d+)(?=\\s*MTH)", group = 1))
  dys <- as.numeric(str_extract(s, "(\\d+)(?=\\s*DYS)", group = 1))
  total <- replace_na(yr, 0) * 365.25 +
           replace_na(mth, 0) * 30.44 +
           replace_na(dys, 0)
  if_else(is.na(yr) & is.na(mth) & is.na(dys), NA_real_, total)
}

if ("ORIGINAL-SENTENCE" %in% colnames(df)) {
  df <- df |>
    mutate(
      `SENTENCE-DAYS` = parse_sentence(`ORIGINAL-SENTENCE`),
      `SENTENCE-ZERO` = `SENTENCE-DAYS` == 0 & !is.na(`SENTENCE-DAYS`)
    )
}

# ── 10. Outcome indicator variables ──────────────────────────────────────────

# Detect actual column names (case-insensitive, handle spaces vs dashes)
all_cols <- colnames(df)
disp_col <- all_cols[str_detect(str_to_upper(all_cols), "DISPOSITION.*DESC")][1]
judg_col <- all_cols[str_detect(str_to_upper(all_cols), "JUDG.*DESC")][1]

if (is.na(disp_col)) {
  message("WARNING: No DISPOSITION-DESC column found. DEFERRED/DISMISSED/GUILTY-PLEA will be NA.")
  df$`DISPOSITION-DESC` <- NA_character_
  disp_col <- "DISPOSITION-DESC"
}
if (is.na(judg_col)) {
  df$`JUDGEMENT-DESC` <- NA_character_
  judg_col <- "JUDGEMENT-DESC"
}

# Standardize to expected names
if (disp_col != "DISPOSITION-DESC") {
  df <- df |> rename(`DISPOSITION-DESC` = all_of(disp_col))
}
if (judg_col != "JUDGEMENT-DESC") {
  df <- df |> rename(`JUDGEMENT-DESC` = all_of(judg_col))
}

df <- df |>
  mutate(
    disp_ = replace_na(`DISPOSITION-DESC`, ""),
    judg_ = replace_na(`JUDGEMENT-DESC`,   ""),
    DEFERRED    = as.integer(str_detect(disp_, regex("DEFR",      ignore_case = TRUE))),
    DISMISSED   = as.integer(str_detect(disp_, regex("DSMD",      ignore_case = TRUE))),
    `GUILTY-PLEA` = as.integer(str_detect(disp_, regex("CT-GUILTY", ignore_case = TRUE))),
    `JURY-TRIAL`  = as.integer(
      str_detect(disp_, regex("PNG JRY", ignore_case = TRUE)) |
      str_detect(judg_, regex("JURY",    ignore_case = TRUE))
    ),
    PROBATION   = as.integer(str_detect(disp_, regex("PROB",      ignore_case = TRUE))),
    CONVICTED   = as.integer(
      `GUILTY-PLEA` == 1L | `JURY-TRIAL` == 1L |
      (!is.na(`SENTENCE-DAYS`) & `SENTENCE-DAYS` > 0)
    )
  ) |>
  select(-disp_, -judg_)

# ── 11. Bond amount ───────────────────────────────────────────────────────────

if ("BOND-AMOUNT" %in% colnames(df)) {
  df <- df |>
    mutate(
      `BOND-AMOUNT` = as.numeric(`BOND-AMOUNT`),
      # Sentinel value >= 9,999,990 means "no bond set / missing" — treat as NA
      `BOND-AMOUNT` = if_else(`BOND-AMOUNT` >= 9999990, NA_real_, `BOND-AMOUNT`),
      `BOND-MISSING` = as.integer(is.na(`BOND-AMOUNT`) | `BOND-AMOUNT` == 0),
      `BOND-OUTLIER` = as.integer(!is.na(`BOND-AMOUNT`) & `BOND-AMOUNT` > 1e6)
    )
}

# ── 12. Prosecutor names ──────────────────────────────────────────────────────

for (col in intersect(c("INTAKE-PROSECUTOR", "OUTTAKE-PROSECUTOR"), colnames(df))) {
  df[[col]] <- str_to_upper(str_trim(df[[col]]))
}

if ("INTAKE-PROSECUTOR" %in% colnames(df)) {
  prosecutor_counts <- df |>
    count(`INTAKE-PROSECUTOR`, name = "PROSECUTOR-CASE-COUNT")
  df <- left_join(df, prosecutor_counts, by = "INTAKE-PROSECUTOR")
}

# ── 13. Split and export ──────────────────────────────────────────────────────

df_historical <- df |> filter(`CASE-YEAR` < 1990)
message("Historical records (pre-1990): ", nrow(df_historical))

panel <- df |> filter(`CASE-YEAR` >= 1990, `CASE-YEAR` <= 2021)
message("Main panel (1990-2021): ", nrow(panel))

write_csv(df,    file.path(OUTPUT_DIR, "bexar_cleaned.csv"))
write_parquet(df,    file.path(OUTPUT_DIR, "bexar_cleaned.parquet"))
write_parquet(panel, file.path(OUTPUT_DIR, "bexar_panel_1990_2021.parquet"))
message("Saved: bexar_cleaned.csv, bexar_cleaned.parquet, bexar_panel_1990_2021.parquet")

# ── 14. Quick sanity checks ───────────────────────────────────────────────────

message("\n── Race distribution (panel) ──")
print(count(panel, `RACE-LABEL`, sort = TRUE))

message("\n── Attorney type (panel) ──")
print(count(panel, `ATTORNEY-TYPE`, sort = TRUE))

message("\n── Offense class (panel) ──")
print(count(panel, `OFFENSE-CLASS`, sort = TRUE))

message("\n── Outcome rates by race (primary races, panel) ──")
panel |>
  filter(`RACE-LABEL` %in% c("Black", "White", "Latino")) |>
  group_by(`RACE-LABEL`) |>
  summarise(
    N              = n(),
    Deferred_pct   = mean(DEFERRED,      na.rm = TRUE) * 100,
    Dismissed_pct  = mean(DISMISSED,     na.rm = TRUE) * 100,
    GuiltyPlea_pct = mean(`GUILTY-PLEA`, na.rm = TRUE) * 100,
    Convicted_pct  = mean(CONVICTED,     na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  print()
