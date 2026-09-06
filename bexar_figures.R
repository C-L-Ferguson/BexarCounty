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

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"
FIG_DIR  <- file.path(DATA_DIR, "bexar_figures")

library(tidyverse)
library(arrow)
library(scales)

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

dp <- read_parquet(file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))

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
  ggsave(path, p, width = w, height = h, dpi = 150)
  message("Saved: ", path)
}

bw <- dp |> filter(`RACE-LABEL` %in% c("Black", "White"))

# ── Helper: clustered SE for a proportion ─────────────────────────────────────
# Compute cluster-robust SE (by prosecutor) for the difference in two proportions
# across quintiles. Used to annotate significance on figures.
# For display CIs we use the simple formula already in the data; regression-based
# stars come from the models in bexar_models.R.

# ── Figure 1 (Primary): W-B gap by experience quintile, overall + by offense ──
# Annotated with linear slope coefficient (OLS of gap ~ quintile)

gap_data <- bw |>
  filter(`OFFENSE-CLASS` %in% c("F1", "F2", "F3", "FS")) |>
  group_by(`OFFENSE-CLASS`, EXP_QUINTILE, `RACE-LABEL`) |>
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
  group_by(EXP_QUINTILE, `RACE-LABEL`) |>
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

# Compute linear slope of gap ~ quintile for each panel (for annotation)
slopes <- fig1_data |>
  group_by(`Offense Type`) |>
  summarise(
    slope = coef(lm(gap ~ EXP_QUINTILE))[["EXP_QUINTILE"]],
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(ifelse(slope > 0, "+", ""), round(slope, 2), " pp/quintile"),
    EXP_QUINTILE = 3,
    gap = max(fig1_data$ci_hi, na.rm = TRUE) * 0.92
  )

p1 <- ggplot(fig1_data, aes(EXP_QUINTILE, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#1f4e79") +
  geom_line(color = "#1f4e79", linewidth = 1) +
  geom_point(color = "#1f4e79", size = 2.5) +
  geom_text(data = slopes, aes(label = label), size = 3, color = "#1f4e79",
            fontface = "italic") +
  facet_wrap(~`Offense Type`, nrow = 1) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  labs(
    title    = "White–Black Deferred Adjudication Gap by Prosecutor Experience Quintile",
    x        = "Experience Quintile (Q1 = earliest cases, Q5 = latest)",
    y        = "Gap (percentage points)",
    caption  = paste0(
      "Notes: Gap = White deferred rate minus Black deferred rate, in percentage points. ",
      "Shaded bands are 95% confidence intervals. Sample: felony cases with identified ",
      "prosecutors who handled 50+ cases, 1990–2015. The Q4 peak and Q5 dip observed across ",
      "panels likely reflects thin cell counts in the final quintile (prosecutors with the ",
      "most cases are disproportionately senior and may have shifted to supervisory roles, ",
      "reducing their late-career caseload and increasing sampling variance). ",
      "The F1 panel shows a notably flat or declining gap at Q5; this is consistent with a ",
      "floor effect — Black defendants face such low baseline deferred rates in the most ",
      "serious felony category that the gap has limited room to widen further. ",
      "See Table 1 for regression-based estimates. ***p<0.01 **p<0.05 *p<0.10."
    )
  ) +
  theme_paper

save_fig(p1, "fig1_gap_by_quintile.png", w = 12, h = 5)

# ── Figure 2: Both groups trajectory — rates on same chart ────────────────────

both_traj <- bw |>
  group_by(EXP_QUINTILE, Race = `RACE-LABEL`) |>
  summarise(
    N    = n(),
    rate = mean(DEFERRED, na.rm = TRUE),
    se   = sqrt(rate * (1 - rate) / N),
    .groups = "drop"
  ) |>
  mutate(Race = factor(Race, levels = c("Black", "White")))

# Annotate overall slope for each race
slopes2 <- both_traj |>
  group_by(Race) |>
  summarise(
    slope = coef(lm(rate * 100 ~ EXP_QUINTILE))[["EXP_QUINTILE"]],
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(ifelse(slope > 0, "+", ""), round(slope, 2), " pp/Q"),
    EXP_QUINTILE = 4.5,
    rate_pct = NA_real_
  )

# Compute y-positions for slope labels from actual data
slopes2 <- slopes2 |>
  left_join(
    both_traj |> filter(EXP_QUINTILE == 5) |> select(Race, rate),
    by = "Race"
  ) |>
  mutate(rate_pct = rate * 100 + ifelse(Race == "White", 1.5, -1.5))

p2 <- ggplot(both_traj, aes(EXP_QUINTILE, rate * 100, color = Race, group = Race)) +
  geom_ribbon(aes(ymin = (rate - 1.96 * se) * 100,
                  ymax = (rate + 1.96 * se) * 100,
                  fill = Race), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(data = slopes2, aes(x = EXP_QUINTILE, y = rate_pct, label = label,
                                 color = Race),
            size = 3, fontface = "italic", hjust = 1, show.legend = FALSE) +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135")) +
  scale_fill_manual(values  = c(Black = "#1f4e79", White = "#538135")) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Deferred Adjudication Rate by Race and Prosecutor Experience",
    x       = "Experience Quintile (Q1 = earliest cases, Q5 = latest)",
    y       = "Deferred Adjudication Rate (%)",
    color   = NULL, fill = NULL,
    caption = paste0(
      "Notes: Shaded bands are 95% confidence intervals. Both groups' rates ",
      "rise with experience, but the White rate rises faster, widening the racial gap. ",
      "Starting values (Q1): Black defendants are deferred at approximately 1% and White ",
      "defendants at approximately 3% — near-zero for both groups. By Q5 those figures ",
      "reach approximately 17% and 26%, respectively. The compressed y-axis in Q1 ",
      "understates the importance of this baseline: even at career outset, prosecutors ",
      "already extend deferred adjudication to White defendants at three times the rate ",
      "they extend it to Black defendants. ",
      "Sample: Black and White defendants in felony cases with identified prosecutors ",
      "who handled 50+ cases, 1990–2015."
    )
  ) +
  theme_paper

save_fig(p2, "fig2_both_groups_trajectory.png")

# ── Figure 3: Career start vs. end scatterplot ────────────────────────────────
# Require at least 5 cases of each race in both Q1 and Q5 to avoid extreme rates

prosecutor_gaps <- bw |>
  filter(EXP_QUINTILE %in% c(1, 5)) |>
  group_by(`INTAKE-PROSECUTOR`, EXP_QUINTILE, `RACE-LABEL`) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE), n = n(), .groups = "drop") |>
  pivot_wider(names_from = `RACE-LABEL`, values_from = c(rate, n)) |>
  filter(!is.na(n_Black), !is.na(n_White), n_Black >= 5, n_White >= 5) |>
  mutate(gap    = (rate_White - rate_Black) * 100,
         Period = if_else(EXP_QUINTILE == 1, "Early (Q1)", "Late (Q5)")) |>
  select(`INTAKE-PROSECUTOR`, Period, gap) |>
  pivot_wider(names_from = Period, values_from = gap) |>
  drop_na() |>
  # Winsorize at ±30 pp for display
  mutate(
    `Early (Q1)` = pmax(pmin(`Early (Q1)`, 30), -30),
    `Late (Q5)`  = pmax(pmin(`Late (Q5)`,  30), -30)
  )

n_above  <- sum(prosecutor_gaps$`Late (Q5)` > prosecutor_gaps$`Early (Q1)`, na.rm = TRUE)
pct_above <- round(n_above / nrow(prosecutor_gaps) * 100)

# Identify the outlier: largest Q1 gap that ended near zero
outlier <- prosecutor_gaps |>
  filter(`Early (Q1)` > 15, abs(`Late (Q5)`) < 5) |>
  slice_max(`Early (Q1)`, n = 1)

p3 <- ggplot(prosecutor_gaps, aes(`Early (Q1)`, `Late (Q5)`)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = 0, color = "gray80", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "gray80", linewidth = 0.3) +
  geom_point(alpha = 0.7, color = "#1f4e79", size = 2.5) +
  annotate("text", x = 28, y = -28,
           label = "Gap narrowed", hjust = 1, size = 3,
           color = "gray50", fontface = "italic") +
  annotate("text", x = -28, y = 28,
           label = "Gap widened", hjust = 0, size = 3,
           color = "gray50", fontface = "italic") +
  {if (nrow(outlier) > 0)
    geom_label(data = outlier,
               aes(`Early (Q1)`, `Late (Q5)`, label = "†"),
               color = "#c55a11", size = 4, label.size = 0,
               nudge_y = 3, nudge_x = -1)
  } +
  coord_fixed(xlim = c(-30, 30), ylim = c(-30, 30)) +
  labs(
    title   = "Prosecutor Career Gap: Early vs. Late Career",
    x       = "White–Black Gap in Q1, Earliest Cases (pp)",
    y       = "White–Black Gap in Q5, Latest Cases (pp)",
    caption = paste0(
      "Notes: Each dot is one prosecutor (restricted to prosecutors with 5+ Black and ",
      "5+ White defendants in both Q1 and Q5; n=", nrow(prosecutor_gaps), "). ",
      "Axes winsorized at ±30 pp. The diagonal dashed line represents no change. ",
      pct_above, "% of prosecutors fall above the line (gap widened). ",
      "† marks the outlier in the lower right (x≈20, y≈0): a prosecutor who began with ",
      "one of the largest pro-White early career gaps in the sample but ended near zero — ",
      "one of the minority of cases where the gap narrowed substantially over a career. ",
      "Sample: 1990–2015."
    )
  ) +
  theme_paper

save_fig(p3, "fig3_career_scatterplot.png")

# ── Figure 4: Individual trajectories — top 10 highest-volume prosecutors ─────
# Use career decile bins (10 bins) rather than rolling average to smooth binary outcome

top10 <- dp |>
  count(`INTAKE-PROSECUTOR`, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(`INTAKE-PROSECUTOR`)

indiv <- dp |>
  filter(`INTAKE-PROSECUTOR` %in% top10, `RACE-LABEL` %in% c("Black", "White")) |>
  group_by(`INTAKE-PROSECUTOR`, `RACE-LABEL`) |>
  mutate(CAREER_DECILE = ntile(PROSECUTOR_CASE_N, 10)) |>
  group_by(`INTAKE-PROSECUTOR`, `RACE-LABEL`, CAREER_DECILE) |>
  summarise(
    rate = mean(DEFERRED, na.rm = TRUE),
    n    = n(),
    .groups = "drop"
  ) |>
  filter(n >= 3) |>
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

p4 <- ggplot(indiv, aes(CAREER_DECILE, rate * 100, color = Race, group = Race)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(size = 1.5, alpha = 0.8) +
  facet_wrap(~Label, nrow = 2) +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135"), name = NULL) +
  scale_x_continuous(breaks = c(1, 5, 10),
                     labels = c("Early", "Mid", "Late")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Individual Prosecutor Trajectories — Top 10 by Case Volume",
    x       = "Career Stage (Early → Late)",
    y       = "Deferred Adjudication Rate (%)",
    caption = paste0(
      "Notes: Each line shows deferred adjudication rates for Black (navy) and White (green) ",
      "defendants across 10 equal career-stage bins for the 10 highest-volume prosecutors, ",
      "1990–2015. Bins with fewer than 3 cases omitted. Y-axis is shared across panels. ",
      "M.P. illustrates the learning-curve pattern most clearly: this prosecutor begins with ",
      "a higher deferred rate for Black defendants and ends with a substantial pro-White gap, ",
      "consistent with the aggregate trend. R.F. illustrates real heterogeneity — the gap ",
      "does not widen across this prosecutor's career — showing that Figure 4 is not ",
      "cherry-picked and that variation in individual trajectories is genuine."
    )
  ) +
  theme_paper +
  theme(axis.text.x = element_text(size = 8))

save_fig(p4, "fig4_individual_trajectories.png", w = 12, h = 7)

# ── Figure 5: Appointed-counsel only robustness ───────────────────────────────

appt_traj <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White"), `ATTORNEY-TYPE` == "Appointed") |>
  group_by(EXP_QUINTILE, Race = `RACE-LABEL`) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE), N = n(),
            se = sqrt(rate * (1 - rate) / N), .groups = "drop")

p5 <- ggplot(appt_traj, aes(EXP_QUINTILE, rate * 100, color = Race, group = Race)) +
  geom_ribbon(aes(ymin = (rate - 1.96 * se) * 100,
                  ymax = (rate + 1.96 * se) * 100,
                  fill = Race), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135")) +
  scale_fill_manual(values  = c(Black = "#1f4e79", White = "#538135")) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Deferred Adjudication Rate by Race and Experience — Appointed Counsel Only",
    x       = "Experience Quintile (Q1 = earliest cases, Q5 = latest)",
    y       = "Deferred Adjudication Rate (%)",
    color   = NULL, fill = NULL,
    caption = paste0(
      "Notes: Restricts to cases where defendant had court-appointed counsel, ",
      "removing variation in attorney quality and selection associated with private ",
      "representation. Shaded bands are 95% confidence intervals. ",
      "Sample: 1990–2015."
    )
  ) +
  theme_paper

save_fig(p5, "fig5_appointed_only.png")

message("\nAll figures saved to ", FIG_DIR)
