# Bexar County — Outtake Prosecutor Figures
# Requires: bexar_prosecutor_panel_1990_2015.parquet
# Output: bexar_outtake_figures/ directory with PNG files
#
# Figures show deferred adjudication rates and racial gaps by prosecutor
# experience quintile (Q1–Q5). All labels say "Prosecutor" (not "Outtake").
# Three racial groups: Black, Latino, White.

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"
FIG_DIR  <- file.path(DATA_DIR, "bexar_outtake_figures")

library(tidyverse)
library(arrow)
library(scales)

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

dp_raw <- read_parquet(file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))

# ── Build case-level outtake frame ────────────────────────────────────────────

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
    EXP_QUINTILE = ntile(OUTTAKE_CASE_N, 5),
    Race = factor(`RACE-LABEL`, levels = c("Black", "Latino", "White"))
  )

message("Outtake figures sample: ", nrow(dp_out), " cases")

# ── Theme and colors ──────────────────────────────────────────────────────────

COLORS <- c(Black = "#1f4e79", Latino = "#c55a11", White = "#538135")

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

x_scale <- scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5))
x_lab   <- "Prosecutor Experience Quintile (Q1 = least experienced, Q5 = most experienced)"

# ── Figure 1: Deferred adjudication rate — all three groups over Q1–Q5 ────────

traj_all <- dp_out |>
  group_by(EXP_QUINTILE, Race) |>
  summarise(
    N    = n(),
    rate = mean(DEFERRED, na.rm = TRUE),
    se   = sqrt(rate * (1 - rate) / N),
    .groups = "drop"
  )

p1 <- ggplot(traj_all, aes(EXP_QUINTILE, rate * 100, color = Race, group = Race)) +
  geom_ribbon(aes(ymin = (rate - 1.96 * se) * 100,
                  ymax = (rate + 1.96 * se) * 100,
                  fill = Race), alpha = 0.10, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = COLORS) +
  scale_fill_manual(values  = COLORS) +
  x_scale +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Deferred Adjudication Rate by Race and Prosecutor Experience",
    x       = x_lab,
    y       = "Deferred Adjudication Rate (%)",
    color   = NULL, fill = NULL,
    caption = str_wrap(paste0(
      "Notes: Shaded bands are 95% confidence intervals. Experience quintiles based on cumulative ",
      "prosecutor caseload. Sample: Black, Latino, and White defendants in felony cases, ",
      "prosecutors first observed 1991 or later, 1991\u20132015. N = ", formatC(nrow(dp_out), big.mark = ","), "."
    ), width = 130)
  ) +
  theme_paper

save_fig(p1, "fig1_three_groups_trajectory.png")

# ── Figure 2: White–Black gap over Q1–Q5 ─────────────────────────────────────

gap_wb <- dp_out |>
  filter(Race %in% c("Black", "White")) |>
  group_by(EXP_QUINTILE, Race) |>
  summarise(n = n(), rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Race, values_from = c(n, rate)) |>
  mutate(
    gap   = (rate_White - rate_Black) * 100,
    se    = sqrt((rate_White * (1 - rate_White) / n_White) +
                 (rate_Black * (1 - rate_Black) / n_Black)) * 100,
    ci_lo = gap - 1.96 * se,
    ci_hi = gap + 1.96 * se,
    label = sprintf("%.1fpp", gap)
  )

p2 <- ggplot(gap_wb, aes(EXP_QUINTILE, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#1f4e79") +
  geom_line(color = "#1f4e79", linewidth = 1.2) +
  geom_point(color = "#1f4e79", size = 3) +
  geom_text(aes(label = label), vjust = -1, size = 3, color = "#1f4e79") +
  x_scale +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title   = "White\u2013Black Deferred Adjudication Gap by Prosecutor Experience",
    x       = x_lab,
    y       = "Gap (percentage points)",
    caption = str_wrap(paste0(
      "Notes: Gap = White deferred rate minus Black deferred rate (percentage points). ",
      "Shaded band is 95% confidence interval. Experience quintiles based on cumulative ",
      "prosecutor caseload. Sample: 1991\u20132015."
    ), width = 130)
  ) +
  theme_paper

save_fig(p2, "fig2_white_black_gap.png")

# ── Figure 3: White–Latino gap over Q1–Q5 ────────────────────────────────────

gap_wl <- dp_out |>
  filter(Race %in% c("Latino", "White")) |>
  group_by(EXP_QUINTILE, Race) |>
  summarise(n = n(), rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Race, values_from = c(n, rate)) |>
  mutate(
    gap   = (rate_White - rate_Latino) * 100,
    se    = sqrt((rate_White * (1 - rate_White) / n_White) +
                 (rate_Latino * (1 - rate_Latino) / n_Latino)) * 100,
    ci_lo = gap - 1.96 * se,
    ci_hi = gap + 1.96 * se,
    label = sprintf("%.1fpp", gap)
  )

p3 <- ggplot(gap_wl, aes(EXP_QUINTILE, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#c55a11") +
  geom_line(color = "#c55a11", linewidth = 1.2) +
  geom_point(color = "#c55a11", size = 3) +
  geom_text(aes(label = label), vjust = -1, size = 3, color = "#c55a11") +
  x_scale +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title   = "White\u2013Latino Deferred Adjudication Gap by Prosecutor Experience",
    x       = x_lab,
    y       = "Gap (percentage points)",
    caption = str_wrap(paste0(
      "Notes: Gap = White deferred rate minus Latino deferred rate (percentage points). ",
      "Shaded band is 95% confidence interval. Experience quintiles based on cumulative ",
      "prosecutor caseload. Sample: 1991\u20132015."
    ), width = 130)
  ) +
  theme_paper

save_fig(p3, "fig3_white_latino_gap.png")

# ── Figure 4: Combined gap figure (W-B and W-L on same axes) ─────────────────

gaps_combined <- bind_rows(
  gap_wb |> mutate(Comparison = "White\u2013Black"),
  gap_wl |> mutate(Comparison = "White\u2013Latino")
) |>
  mutate(Comparison = factor(Comparison, levels = c("White\u2013Black", "White\u2013Latino")))

gap_colors <- c("White\u2013Black" = "#1f4e79", "White\u2013Latino" = "#c55a11")

p4 <- ggplot(gaps_combined, aes(EXP_QUINTILE, gap, color = Comparison, group = Comparison)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = Comparison),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = gap_colors) +
  scale_fill_manual(values  = gap_colors) +
  x_scale +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title   = "Racial Gaps in Deferred Adjudication by Prosecutor Experience",
    x       = x_lab,
    y       = "Gap (percentage points)",
    color   = NULL, fill = NULL,
    caption = str_wrap(paste0(
      "Notes: Gap = White deferred rate minus Black or Latino deferred rate (percentage points). ",
      "Shaded bands are 95% confidence intervals. Experience quintiles based on cumulative ",
      "prosecutor caseload. Sample: 1991\u20132015."
    ), width = 130)
  ) +
  theme_paper

save_fig(p4, "fig4_combined_gaps.png")

# ── Figure 5: By felony severity — W-B gap ───────────────────────────────────

gap_by_offense <- dp_out |>
  filter(Race %in% c("Black", "White")) |>
  mutate(`Offense Type` = factor(`OFFENSE-CLASS`, levels = offense_order)) |>
  group_by(`Offense Type`, EXP_QUINTILE, Race) |>
  summarise(n = n(), rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Race, values_from = c(n, rate)) |>
  mutate(
    gap   = (rate_White - rate_Black) * 100,
    se    = sqrt((rate_White * (1 - rate_White) / n_White) +
                 (rate_Black * (1 - rate_Black) / n_Black)) * 100,
    ci_lo = gap - 1.96 * se,
    ci_hi = gap + 1.96 * se
  )

p5 <- ggplot(gap_by_offense, aes(EXP_QUINTILE, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#1f4e79") +
  geom_line(color = "#1f4e79", linewidth = 1) +
  geom_point(color = "#1f4e79", size = 2.5) +
  facet_wrap(~`Offense Type`, nrow = 1) +
  x_scale +
  labs(
    title   = "White\u2013Black Gap by Prosecutor Experience and Offense Severity",
    x       = x_lab,
    y       = "Gap (percentage points)",
    caption = str_wrap(paste0(
      "Notes: Shaded bands are 95% confidence intervals. Each panel restricts to cases of that ",
      "felony severity. Experience quintiles based on cumulative prosecutor caseload. Sample: 1991\u20132015."
    ), width = 130)
  ) +
  theme_paper

save_fig(p5, "fig5_gap_by_offense.png", w = 12, h = 5)

# ── Figure 6: Individual prosecutor trajectories (top 10, 3 groups) ──────────

top10 <- dp_out |>
  count(`OUTTAKE-PROSECUTOR`, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(`OUTTAKE-PROSECUTOR`)

indiv <- dp_out |>
  filter(`OUTTAKE-PROSECUTOR` %in% top10) |>
  group_by(`OUTTAKE-PROSECUTOR`, Race) |>
  mutate(CAREER_QUINTILE = ntile(OUTTAKE_CASE_N, 5)) |>
  group_by(`OUTTAKE-PROSECUTOR`, Race, CAREER_QUINTILE) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE), n = n(), .groups = "drop") |>
  filter(n >= 5) |>
  mutate(
    Label = {
      nm    <- `OUTTAKE-PROSECUTOR`
      last  <- str_extract(nm, "^[^,]+")
      first <- str_extract(nm, "(?<=,\\s)\\S+")
      paste0(str_sub(first, 1, 1), ".", str_sub(last, 1, 1), ".")
    }
  )

p6 <- ggplot(indiv, aes(CAREER_QUINTILE, rate * 100, color = Race, group = Race)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~Label, nrow = 2) +
  scale_color_manual(values = COLORS, name = NULL) +
  x_scale +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Individual Prosecutor Trajectories \u2014 Top 10 by Case Volume",
    x       = "Career Quintile (Q1 = least experienced, Q5 = most experienced)",
    y       = "Deferred Adjudication Rate (%)",
    caption = str_wrap(paste0(
      "Notes: Each panel shows deferred adjudication rates for Black (navy), Latino (orange), ",
      "and White (green) defendants across career quintiles for the 10 highest-volume prosecutors, ",
      "1991\u20132015. Bins with fewer than 5 cases omitted."
    ), width = 130)
  ) +
  theme_paper +
  theme(axis.text.x = element_text(size = 8))

save_fig(p6, "fig6_individual_trajectories.png", w = 12, h = 8)

message("\nAll figures saved to ", FIG_DIR)
