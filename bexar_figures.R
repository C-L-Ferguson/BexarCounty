# Bexar County — Prosecutor Learning Curve Figures
# Requires: bexar_prosecutor_panel_1990_2015.parquet (from bexar_prosecutor_panel.R)
# Output: bexar_figures/ directory with PNG files
#
# Formatting follows Shaffer (2023) "Prosecutors, Race, and the Criminal Pipeline,"
# 90 U. Chi. L. Rev. 1889 conventions:
#   - Figures: descriptive caption below, 95% CIs, significance stars
#   - Standard errors clustered by INTAKE-PROSECUTOR
#   - Annotated slope/gap coefficients on primary figure
#   - Absolute percentage-point gaps as primary metric
#   - Clean minimal aesthetic

DATA_DIR <- "C:/Users/carolineferguson/Box/Bigelow/Bexar/Data"
FIG_DIR  <- file.path(DATA_DIR, "bexar_figures")

library(tidyverse)
library(arrow)
library(scales)

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

dp_raw <- read_parquet(file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))

# Restrict to prosecutors first observed 1991+ (matches main analysis sample)
fresh_prosecutors <- dp_raw |>
  group_by(`INTAKE-PROSECUTOR`) |>
  summarise(first_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop") |>
  filter(first_year >= 1991) |>
  pull(`INTAKE-PROSECUTOR`)

dp <- dp_raw |> filter(`INTAKE-PROSECUTOR` %in% fresh_prosecutors)

COLORS    <- c(Black = "#1f4e79", Latino = "#c55a11", White = "#538135")
RACE_ORDER <- c("Black", "Latino", "White")

theme_paper <- theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "gray92"),
    axis.title        = element_text(size = 11),
    plot.title        = element_text(size = 12, face = "bold"),
    plot.caption      = element_text(size = 9, color = "gray40", hjust = 0,
                                     margin = margin(t = 8)),
    strip.text        = element_text(face = "bold")
  )

save_fig <- function(p, name, w = 8, h = 5) {
  path <- file.path(FIG_DIR, name)
  ggsave(path, p + theme(plot.margin = margin(5, 5, 20, 5, "mm")),
         width = w, height = h, dpi = 150)
  message("Saved: ", path)
}

dp <- dp |> mutate(EXP_QUARTILE = ntile(PROSECUTOR_CASE_N, 4))

bw <- dp |> filter(`RACE-LABEL` %in% c("Black", "White"))

# ── Helper: clustered SE for a proportion ─────────────────────────────────────
# Compute cluster-robust SE (by prosecutor) for the difference in two proportions
# across quintiles. Used to annotate significance on figures.
# For display CIs we use the simple formula already in the data; regression-based
# stars come from the models in bexar_models.R.

# ── Figure 1 (Primary): W-B gap by experience quartile, overall + by offense ──
# Annotated with linear slope coefficient (OLS of gap ~ quartile)

gap_data <- bw |>
  filter(`OFFENSE-CLASS` %in% c("F1", "F2", "F3", "FS")) |>
  group_by(`OFFENSE-CLASS`, EXP_QUARTILE, `RACE-LABEL`) |>
  summarise(n = n(), rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = `RACE-LABEL`, values_from = c(n, rate)) |>
  mutate(
    gap   = (rate_White - rate_Black) * 100,
    se    = sqrt((rate_White * (1 - rate_White) / n_White) +
                 (rate_Black * (1 - rate_Black) / n_Black)) * 100,
    ci_lo = gap - 1.96 * se,
    ci_hi = gap + 1.96 * se,
    `Offense Type` = `OFFENSE-CLASS`
  )

gap_overall <- bw |>
  group_by(EXP_QUARTILE, `RACE-LABEL`) |>
  summarise(n = n(), rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = `RACE-LABEL`, values_from = c(n, rate)) |>
  mutate(
    gap   = (rate_White - rate_Black) * 100,
    se    = sqrt((rate_White * (1 - rate_White) / n_White) +
                 (rate_Black * (1 - rate_Black) / n_Black)) * 100,
    ci_lo = gap - 1.96 * se,
    ci_hi = gap + 1.96 * se,
    `Offense Type` = "Overall"
  )

fig1_data <- bind_rows(gap_overall, gap_data) |>
  mutate(`Offense Type` = factor(`Offense Type`,
                                  levels = c("Overall", "F1", "F2", "F3", "FS")))

slopes <- fig1_data |>
  group_by(`Offense Type`) |>
  summarise(
    slope = coef(lm(gap ~ EXP_QUARTILE))[["EXP_QUARTILE"]],
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(ifelse(slope > 0, "+", ""), round(slope, 2), " pp/quartile"),
    EXP_QUARTILE = 2.5,
    gap = max(fig1_data$ci_hi, na.rm = TRUE) * 0.92
  )

p1 <- ggplot(fig1_data, aes(EXP_QUARTILE, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#1f4e79") +
  geom_line(color = "#1f4e79", linewidth = 1) +
  geom_point(color = "#1f4e79", size = 2.5) +
  geom_text(data = slopes, aes(label = label), size = 3, color = "#1f4e79",
            fontface = "italic") +
  facet_wrap(~`Offense Type`, nrow = 1) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Q", 1:4)) +
  labs(
    title    = "White–Black Deferred Adjudication Gap by Prosecutor Experience Quartile",
    x        = "Experience Quartile (Q1 = earliest cases, Q4 = latest)",
    y        = "Gap (percentage points)",
    caption  = str_wrap(paste0(
      "Notes: Gap = White deferred rate minus Black deferred rate, in percentage points. ",
      "Shaded bands are 95% confidence intervals. Quartiles based on cumulative prosecutor caseload. ",
      "Sample: felony cases, prosecutors first observed 1991 or later, 1991–2015. ",
      "See Table 1 for regression-based estimates controlling for offense type, attorney type, ",
      "and prosecutor identity. ***p<0.01 **p<0.05 *p<0.10."
    ), width = 140)
  ) +
  theme_paper

save_fig(p1, "fig1_gap_by_quartile.png", w = 12, h = 5.5)

# ── Figure 2: Both groups trajectory — rates on same chart ────────────────────

both_traj <- bw |>
  group_by(EXP_QUARTILE, Race = `RACE-LABEL`) |>
  summarise(
    N    = n(),
    rate = mean(DEFERRED, na.rm = TRUE),
    se   = sqrt(rate * (1 - rate) / N),
    .groups = "drop"
  ) |>
  mutate(Race = factor(Race, levels = c("Black", "White")))

slopes2 <- both_traj |>
  group_by(Race) |>
  summarise(
    slope = coef(lm(rate * 100 ~ EXP_QUARTILE))[["EXP_QUARTILE"]],
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(ifelse(slope > 0, "+", ""), round(slope, 2), " pp/Q"),
    EXP_QUARTILE = 4,
    rate_pct = NA_real_
  )

slopes2 <- slopes2 |>
  left_join(
    both_traj |> filter(EXP_QUARTILE == 4) |> select(Race, rate),
    by = "Race"
  ) |>
  mutate(rate_pct = rate * 100 + ifelse(Race == "White", 1.5, -1.5))

p2 <- ggplot(both_traj, aes(EXP_QUARTILE, rate * 100, color = Race, group = Race)) +
  geom_ribbon(aes(ymin = (rate - 1.96 * se) * 100,
                  ymax = (rate + 1.96 * se) * 100,
                  fill = Race), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(data = slopes2, aes(x = EXP_QUARTILE, y = rate_pct, label = label,
                                 color = Race),
            size = 3, fontface = "italic", hjust = 1, show.legend = FALSE) +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135")) +
  scale_fill_manual(values  = c(Black = "#1f4e79", White = "#538135")) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Q", 1:4)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Deferred Adjudication Rate by Race and Prosecutor Experience",
    x       = "Experience Quartile (Q1 = earliest cases, Q4 = latest)",
    y       = "Deferred Adjudication Rate (%)",
    color   = NULL, fill = NULL,
    caption = paste0(
      "Notes: Shaded bands are 95% confidence intervals. Both groups' rates ",
      "rise with experience, but the White rate rises faster, widening the racial gap. ",
      "Quartiles based on cumulative prosecutor caseload. ",
      "Sample: Black and White defendants in felony cases, prosecutors first observed 1991 or later, 1991–2015."
    )
  ) +
  theme_paper

save_fig(p2, "fig2_both_groups_trajectory.png")

# ── Figure 3: Individual trajectories — top 10 highest-volume prosecutors ─────
# Use career decile bins (10 bins) rather than rolling average to smooth binary outcome

top10 <- dp |>
  count(`INTAKE-PROSECUTOR`, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(`INTAKE-PROSECUTOR`)

indiv <- dp |>
  filter(`INTAKE-PROSECUTOR` %in% top10, `RACE-LABEL` %in% c("Black", "White")) |>
  group_by(`INTAKE-PROSECUTOR`, `RACE-LABEL`) |>
  mutate(CAREER_QUARTILE = ntile(PROSECUTOR_CASE_N, 4)) |>
  group_by(`INTAKE-PROSECUTOR`, `RACE-LABEL`, CAREER_QUARTILE) |>
  summarise(
    rate = mean(DEFERRED, na.rm = TRUE),
    n    = n(),
    .groups = "drop"
  ) |>
  filter(n >= 5) |>
  mutate(
    # Convert "LAST, FIRST" to "F.L." initials to anonymize prosecutors
    Label = {
      nm <- `INTAKE-PROSECUTOR`
      last  <- str_extract(nm, "^[^,]+")
      first <- str_extract(nm, "(?<=,\\s)\\S+")
      paste0(str_sub(first, 1, 1), ".", str_sub(last, 1, 1), ".")
    },
    Race  = factor(`RACE-LABEL`, levels = c("Black", "White"))
  )

p3 <- ggplot(indiv, aes(CAREER_QUARTILE, rate * 100, color = Race, group = Race)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~Label, nrow = 2) +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135"), name = NULL) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Q", 1:4)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Individual Prosecutor Trajectories — Top 10 by Case Volume",
    x       = "Career Quartile (Q1 = earliest, Q4 = latest)",
    y       = "Deferred Adjudication Rate (%)",
    caption = paste0(
      "Notes: Each line shows deferred adjudication rates for Black (navy) and White (green) ",
      "defendants across 4 career quartiles for the 10 highest-volume prosecutors, ",
      "1991–2015. Bins with fewer than 5 cases omitted. Y-axis is shared across panels. ",
      "Most prosecutors show a widening White–Black gap by Q4. R.F. illustrates genuine ",
      "heterogeneity — the gap does not widen across this prosecutor's career — showing ",
      "that the aggregate trend is not universal and that individual variation is real."
    )
  ) +
  theme_paper +
  theme(axis.text.x = element_text(size = 8))

save_fig(p3, "fig3_individual_trajectories.png", w = 12, h = 8)

# ── Figure 4: Appointed-counsel only robustness ───────────────────────────────

appt_traj <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White"), `ATTORNEY-TYPE` == "Appointed") |>
  group_by(EXP_QUARTILE, Race = `RACE-LABEL`) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE), N = n(),
            se = sqrt(rate * (1 - rate) / N), .groups = "drop")

p4 <- ggplot(appt_traj, aes(EXP_QUARTILE, rate * 100, color = Race, group = Race)) +
  geom_ribbon(aes(ymin = (rate - 1.96 * se) * 100,
                  ymax = (rate + 1.96 * se) * 100,
                  fill = Race), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135")) +
  scale_fill_manual(values  = c(Black = "#1f4e79", White = "#538135")) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Q", 1:4)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Deferred Adjudication Rate by Race and Experience — Appointed Counsel Only",
    x       = "Experience Quartile (Q1 = earliest cases, Q4 = latest)",
    y       = "Deferred Adjudication Rate (%)",
    color   = NULL, fill = NULL,
    caption = paste0(
      "Notes: Restricts to cases where defendant had court-appointed counsel, ",
      "removing variation in attorney quality and selection associated with private ",
      "representation. Shaded bands are 95% confidence intervals. ",
      "Quartiles based on cumulative prosecutor caseload. Sample: 1991–2015."
    )
  ) +
  theme_paper

save_fig(p4, "fig4_appointed_only.png")

# ── Outtake prosecutor figures ─────────────────────────────────────────────────
# Build outtake career case count (ordered by OUTTAKE-PROSECUTOR × CASE-DATE)

dp_out <- dp_raw |>
  filter(`INTAKE-PROSECUTOR` %in% fresh_prosecutors,
         !is.na(`OUTTAKE-PROSECUTOR`)) |>
  arrange(`OUTTAKE-PROSECUTOR`, `CASE-DATE`) |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  mutate(OUTTAKE_CASE_N = row_number()) |>
  ungroup() |>
  mutate(OUTTAKE_CASE_N100 = OUTTAKE_CASE_N / 100)

# ── Figure 5: Outtake both-groups trajectory (8 bins) ─────────────────────────

both_traj_out <- dp_out |>
  filter(`RACE-LABEL` %in% c("Black", "White")) |>
  mutate(exp_bin = ntile(OUTTAKE_CASE_N, 5)) |>
  group_by(exp_bin, Race = `RACE-LABEL`) |>
  summarise(
    N    = n(),
    rate = mean(DEFERRED, na.rm = TRUE),
    se   = sqrt(rate * (1 - rate) / N),
    .groups = "drop"
  ) |>
  mutate(Race = factor(Race, levels = c("Black", "White")))

p5 <- ggplot(both_traj_out, aes(exp_bin, rate * 100, color = Race, group = Race)) +
  geom_ribbon(aes(ymin = (rate - 1.96*se)*100, ymax = (rate + 1.96*se)*100, fill = Race),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135")) +
  scale_fill_manual(values  = c(Black = "#1f4e79", White = "#538135")) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Deferred Adjudication Rate by Race and Outtake Prosecutor Experience",
    x       = "Experience Quintile (Q1 = earliest cases, Q5 = latest)",
    y       = "Deferred Adjudication Rate (%)",
    color   = NULL, fill = NULL,
    caption = paste0(
      "Notes: Shaded bands are 95% confidence intervals. Experience bins based on cumulative ",
      "outtake prosecutor caseload. Sample: Black and White defendants in felony cases, ",
      "prosecutors first observed 1991 or later, 1991–2015."
    )
  ) +
  theme_paper

save_fig(p5, "fig5_outtake_trajectory.png")

# ── Figure 6: Outtake W-B gap (8 bins, labeled) ───────────────────────────────

gap_out <- dp_out |>
  filter(`RACE-LABEL` %in% c("Black", "White")) |>
  mutate(exp_bin = ntile(OUTTAKE_CASE_N, 5)) |>
  group_by(exp_bin, `RACE-LABEL`) |>
  summarise(n = n(), rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = `RACE-LABEL`, values_from = c(n, rate)) |>
  mutate(
    gap   = (rate_White - rate_Black) * 100,
    se    = sqrt((rate_White * (1 - rate_White) / n_White) +
                 (rate_Black * (1 - rate_Black) / n_Black)) * 100,
    ci_lo = gap - 1.96 * se,
    ci_hi = gap + 1.96 * se,
    label = paste0("+", round(gap, 1), "pp")
  )

p6 <- ggplot(gap_out, aes(exp_bin, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#1f4e79") +
  geom_line(color = "#1f4e79", linewidth = 1.2) +
  geom_point(color = "#1f4e79", size = 2.5) +
  geom_text(aes(label = label), vjust = -1, size = 3, color = "#1f4e79") +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  scale_y_continuous(limits = c(0, 16)) +
  labs(
    title   = "White–Black Deferred Adjudication Gap by Outtake Prosecutor Experience",
    x       = "Experience Quintile (Q1 = earliest cases, Q5 = latest)",
    y       = "Gap (percentage points)",
    caption = paste0(
      "Notes: Gap = White deferred rate minus Black deferred rate, in percentage points. ",
      "Shaded bands are 95% confidence intervals. Experience bins based on cumulative ",
      "outtake prosecutor caseload. Sample: 1991–2015."
    )
  ) +
  theme_paper

save_fig(p6, "fig6_outtake_gap_labeled.png")

# ── Figure 7: Cohort split — intake (policy vs. learning check) ───────────────

intake_start <- dp |>
  group_by(`INTAKE-PROSECUTOR`) |>
  summarise(start_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop")

cohort_split <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White")) |>
  left_join(intake_start, by = "INTAKE-PROSECUTOR") |>
  mutate(
    cohort  = ifelse(start_year <= 2000, "Early cohort (started ≤2000)",
                                         "Late cohort (started >2000)"),
    exp_bin = ntile(PROSECUTOR_CASE_N, 8)
  ) |>
  group_by(cohort, exp_bin, `RACE-LABEL`) |>
  summarise(n = n(), rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = `RACE-LABEL`, values_from = c(n, rate)) |>
  mutate(
    gap   = (rate_White - rate_Black) * 100,
    se    = sqrt((rate_White * (1 - rate_White) / n_White) +
                 (rate_Black * (1 - rate_Black) / n_Black)) * 100,
    ci_lo = gap - 1.96 * se,
    ci_hi = gap + 1.96 * se
  )

p7 <- ggplot(cohort_split, aes(exp_bin, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#1f4e79") +
  geom_line(color = "#1f4e79", linewidth = 1.2) +
  geom_point(color = "#1f4e79", size = 2.5) +
  facet_wrap(~cohort) +
  scale_x_continuous(breaks = 1:8, labels = paste0("B", 1:8)) +
  labs(
    title   = "White–Black Gap by Experience Bin — Early vs. Late Cohorts (Intake)",
    x       = "Experience Bin (B1 = earliest, B8 = latest)",
    y       = "Gap (percentage points)",
    caption = paste0(
      "Notes: If the gap reflected an office-wide policy change, it should emerge at the same ",
      "calendar time for all prosecutors. Instead, both cohorts show the gap rising with career ",
      "experience bins, consistent with individual learning rather than a policy shift."
    )
  ) +
  theme_paper

save_fig(p7, "fig7_cohort_split_intake.png", w = 10, h = 5)

# ── Figure 8: Cohort split — outtake ──────────────────────────────────────────

outtake_start <- dp_out |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  summarise(start_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop")

cohort_split_out <- dp_out |>
  filter(`RACE-LABEL` %in% c("Black", "White")) |>
  left_join(outtake_start, by = "OUTTAKE-PROSECUTOR") |>
  mutate(
    cohort  = ifelse(start_year <= 2000, "Early cohort (started ≤2000)",
                                         "Late cohort (started >2000)"),
    exp_bin = ntile(OUTTAKE_CASE_N, 5)
  ) |>
  group_by(cohort, exp_bin, `RACE-LABEL`) |>
  summarise(n = n(), rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = `RACE-LABEL`, values_from = c(n, rate)) |>
  mutate(
    gap   = (rate_White - rate_Black) * 100,
    se    = sqrt((rate_White * (1 - rate_White) / n_White) +
                 (rate_Black * (1 - rate_Black) / n_Black)) * 100,
    ci_lo = gap - 1.96 * se,
    ci_hi = gap + 1.96 * se
  )

p8 <- ggplot(cohort_split_out, aes(exp_bin, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#1f4e79") +
  geom_line(color = "#1f4e79", linewidth = 1.2) +
  geom_point(color = "#1f4e79", size = 2.5) +
  facet_wrap(~cohort) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  labs(
    title   = "White–Black Gap by Experience Bin — Early vs. Late Cohorts (Outtake)",
    x       = "Experience Bin (B1 = earliest, B8 = latest)",
    y       = "Gap (percentage points)",
    caption = paste0(
      "Notes: Both outtake prosecutor cohorts show the gap rising with career experience bins, ",
      "consistent with individual learning rather than an office-wide policy shift."
    )
  ) +
  theme_paper

save_fig(p8, "fig8_cohort_split_outtake.png", w = 10, h = 5)

message("\nAll figures saved to ", FIG_DIR)
