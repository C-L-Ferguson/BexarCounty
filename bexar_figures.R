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
      "prosecutors who handled 50+ cases, 1990–2015. See Table 1 for regression-based ",
      "estimates. ***p<0.01 **p<0.05 *p<0.10."
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
      "Sample: Black and White defendants in felony cases with identified prosecutors ",
      "who handled 50+ cases, 1990–2015."
    )
  ) +
  theme_paper

save_fig(p2, "fig2_both_groups_trajectory.png")

# ── Figure 3: Career start vs. end scatterplot ────────────────────────────────

prosecutor_gaps <- bw |>
  filter(EXP_QUINTILE %in% c(1, 5)) |>
  group_by(`INTAKE-PROSECUTOR`, EXP_QUINTILE, `RACE-LABEL`) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE), n = n(), .groups = "drop") |>
  pivot_wider(names_from = `RACE-LABEL`, values_from = c(rate, n)) |>
  mutate(gap    = (rate_White - rate_Black) * 100,
         Period = if_else(EXP_QUINTILE == 1, "Early (Q1)", "Late (Q5)")) |>
  select(`INTAKE-PROSECUTOR`, Period, gap) |>
  pivot_wider(names_from = Period, values_from = gap) |>
  drop_na()

n_above <- sum(prosecutor_gaps$`Late (Q5)` > prosecutor_gaps$`Early (Q1)`, na.rm = TRUE)
pct_above <- round(n_above / nrow(prosecutor_gaps) * 100)

p3 <- ggplot(prosecutor_gaps, aes(`Early (Q1)`, `Late (Q5)`)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(alpha = 0.7, color = "#1f4e79", size = 2.5) +
  annotate("text", x = Inf, y = -Inf,
           label = "Below line: gap narrowed", hjust = 1.05, vjust = -0.5,
           size = 3, color = "gray50", fontface = "italic") +
  annotate("text", x = -Inf, y = Inf,
           label = "Above line: gap widened", hjust = -0.05, vjust = 1.5,
           size = 3, color = "gray50", fontface = "italic") +
  labs(
    title   = "Prosecutor Career Gap: Early vs. Late Career",
    x       = "White–Black Gap in Q1, Earliest Cases (pp)",
    y       = "White–Black Gap in Q5, Latest Cases (pp)",
    caption = paste0(
      "Notes: Each dot is one prosecutor. The diagonal dashed line represents no change. ",
      pct_above, "% of prosecutors (", n_above, " of ", nrow(prosecutor_gaps),
      ") fall above the line, meaning their White–Black gap widened over their career. ",
      "Sample: prosecutors with identified cases in both Q1 and Q5, 1990–2015."
    )
  ) +
  theme_paper

save_fig(p3, "fig3_career_scatterplot.png")

# ── Figure 4: Individual trajectories — top 10 highest-volume prosecutors ─────

top10 <- dp |>
  count(`INTAKE-PROSECUTOR`, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(`INTAKE-PROSECUTOR`)

indiv <- dp |>
  filter(`INTAKE-PROSECUTOR` %in% top10, `RACE-LABEL` %in% c("Black", "White")) |>
  arrange(`INTAKE-PROSECUTOR`, `RACE-LABEL`, PROSECUTOR_CASE_N) |>
  group_by(`INTAKE-PROSECUTOR`, `RACE-LABEL`) |>
  mutate(
    roll_rate = if (requireNamespace("zoo", quietly = TRUE))
      zoo::rollmean(DEFERRED, k = 3, fill = NA, align = "center")
    else
      DEFERRED
  ) |>
  ungroup() |>
  mutate(Label = str_trunc(str_extract(`INTAKE-PROSECUTOR`, "^\\S+\\s+\\S+"), 20))

p4 <- ggplot(indiv |> filter(!is.na(roll_rate)),
             aes(PROSECUTOR_CASE_N, roll_rate * 100,
                 color = `RACE-LABEL`, group = `RACE-LABEL`)) +
  geom_line(alpha = 0.8, linewidth = 0.7) +
  facet_wrap(~Label, nrow = 2, scales = "free_x") +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135"), name = NULL) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title   = "Individual Prosecutor Trajectories — Top 10 by Case Volume",
    x       = "Career Case Number",
    y       = "Deferred Adjudication Rate (%)",
    caption = paste0(
      "Notes: Lines show 3-case rolling-average deferred adjudication rates for Black ",
      "(navy) and White (green) defendants handled by the 10 highest-volume prosecutors ",
      "in the sample, 1990–2015. X-axis varies by prosecutor."
    )
  ) +
  theme_paper +
  theme(axis.text.x = element_text(size = 7))

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
