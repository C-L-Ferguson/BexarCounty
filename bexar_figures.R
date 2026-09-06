# Bexar County — Prosecutor Learning Curve Figures
# Requires: bexar_prosecutor_panel_1990_2015.parquet (from bexar_prosecutor_panel.R)
# Output: bexar_figures/ directory with PNG files

DATA_DIR <- "C:/Users/carol/Box/Bigelow/Bexar/Data"
FIG_DIR  <- file.path(DATA_DIR, "bexar_figures")

library(tidyverse)
library(arrow)

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

dp <- read_parquet(file.path(DATA_DIR, "bexar_prosecutor_panel_1990_2015.parquet"))

COLORS <- c(Black = "#1f4e79", Latino = "#c55a11", White = "#538135")
RACE_ORDER <- c("Black", "Latino", "White")

theme_paper <- theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    panel.grid.minor  = element_blank(),
    axis.title        = element_text(size = 11),
    plot.title        = element_text(size = 12, face = "bold"),
    plot.subtitle     = element_text(size = 10, color = "gray40"),
    strip.text        = element_text(face = "bold")
  )

save_fig <- function(p, name, w = 8, h = 5) {
  path <- file.path(FIG_DIR, name)
  ggsave(path, p, width = w, height = h, dpi = 150)
  message("Saved: ", path)
}

bw <- dp |> filter(`RACE-LABEL` %in% c("Black", "White"))

# ── Figure 1 (Primary): W-B gap by experience quintile, overall + by offense ──

gap_data <- bw |>
  filter(`OFFENSE-CLASS` %in% c("F1", "F2", "F3", "FS")) |>
  group_by(`OFFENSE-CLASS`, EXP_QUINTILE, `RACE-LABEL`) |>
  summarise(
    n    = n(),
    rate = mean(DEFERRED, na.rm = TRUE),
    .groups = "drop"
  ) |>
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

p1 <- ggplot(fig1_data, aes(EXP_QUINTILE, gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#1f4e79") +
  geom_line(color = "#1f4e79", linewidth = 1) +
  geom_point(color = "#1f4e79", size = 2.5) +
  facet_wrap(~`Offense Type`, nrow = 1) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  labs(
    title    = "White–Black Deferred Adjudication Gap by Prosecutor Experience Quintile",
    subtitle = "Percentage-point gap (White rate minus Black rate); 95% confidence intervals; 1990–2015",
    x = "Experience Quintile (Q1 = earliest cases, Q5 = latest)",
    y = "Gap (percentage points)"
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

p2 <- ggplot(both_traj, aes(EXP_QUINTILE, rate * 100, color = Race, group = Race)) +
  geom_ribbon(aes(ymin = (rate - 1.96 * se) * 100,
                  ymax = (rate + 1.96 * se) * 100,
                  fill = Race), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135")) +
  scale_fill_manual(values  = c(Black = "#1f4e79", White = "#538135")) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Deferred Adjudication Rate by Race and Prosecutor Experience",
    subtitle = "Both groups rise; White defendants rise faster — 1990–2015",
    x = "Experience Quintile (Q1 = earliest cases, Q5 = latest)",
    y = "Deferred Adjudication Rate",
    color = NULL, fill = NULL
  ) +
  theme_paper

save_fig(p2, "fig2_both_groups_trajectory.png")

# ── Figure 3: Career start vs. end scatterplot ────────────────────────────────
# One dot per prosecutor: x = Q1 gap, y = Q5 gap

prosecutor_gaps <- bw |>
  filter(EXP_QUINTILE %in% c(1, 5)) |>
  group_by(`INTAKE-PROSECUTOR`, EXP_QUINTILE, `RACE-LABEL`) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE), n = n(), .groups = "drop") |>
  pivot_wider(names_from = `RACE-LABEL`, values_from = c(rate, n)) |>
  mutate(gap = (rate_White - rate_Black) * 100,
         Period = if_else(EXP_QUINTILE == 1, "Early (Q1)", "Late (Q5)")) |>
  select(`INTAKE-PROSECUTOR`, Period, gap) |>
  pivot_wider(names_from = Period, values_from = gap) |>
  drop_na()

p3 <- ggplot(prosecutor_gaps,
             aes(`Early (Q1)`, `Late (Q5)`)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(alpha = 0.7, color = "#1f4e79", size = 2.5) +
  annotate("text", x = Inf, y = -Inf, label = "Below line = gap narrowed",
           hjust = 1.1, vjust = -0.5, size = 3, color = "gray50") +
  annotate("text", x = -Inf, y = Inf, label = "Above line = gap widened",
           hjust = -0.1, vjust = 1.5, size = 3, color = "gray50") +
  labs(
    title    = "Prosecutor Career Gap: Early vs. Late Career",
    subtitle = paste0("One dot per prosecutor (n=", nrow(prosecutor_gaps),
                      "). Diagonal = no change. Most dots above line."),
    x = "White–Black Gap in Q1 (percentage points)",
    y = "White–Black Gap in Q5 (percentage points)"
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
    roll_rate = zoo::rollmean(DEFERRED, k = 3, fill = NA, align = "center")
  ) |>
  ungroup()

if (!requireNamespace("zoo", quietly = TRUE)) {
  indiv <- indiv |> mutate(roll_rate = DEFERRED)
  message("NOTE: install 'zoo' for smooth individual trajectories")
}

# Shorten name labels for facet strips
indiv <- indiv |>
  mutate(Label = str_extract(`INTAKE-PROSECUTOR`, "^\\S+\\s+\\S+") |>
           str_trunc(20))

p4 <- ggplot(indiv |> filter(!is.na(roll_rate)),
             aes(PROSECUTOR_CASE_N, roll_rate * 100,
                 color = `RACE-LABEL`, group = `RACE-LABEL`)) +
  geom_line(alpha = 0.8, linewidth = 0.7) +
  facet_wrap(~Label, nrow = 2, scales = "free_x") +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135"),
                     name = NULL) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Individual Prosecutor Trajectories — Top 10 by Volume",
    subtitle = "3-case rolling average deferred adjudication rate by race",
    x = "Career Case Number",
    y = "Deferred Adjudication Rate"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(size = 7))

save_fig(p4, "fig4_individual_trajectories.png", w = 12, h = 7)

# ── Figure 5: Appointed-counsel only robustness ───────────────────────────────

appt_traj <- dp |>
  filter(`RACE-LABEL` %in% c("Black", "White"), `ATTORNEY-TYPE` == "Appointed") |>
  group_by(EXP_QUINTILE, Race = `RACE-LABEL`) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE), N = n(), .groups = "drop")

p5 <- ggplot(appt_traj, aes(EXP_QUINTILE, rate * 100, color = Race, group = Race)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(Black = "#1f4e79", White = "#538135")) +
  scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Deferred Adjudication Rate — Appointed Counsel Cases Only",
    subtitle = "Controls for private attorney effect; pattern still holds",
    x = "Experience Quintile",
    y = "Deferred Adjudication Rate",
    color = NULL
  ) +
  theme_paper

save_fig(p5, "fig5_appointed_only.png")

message("\nAll figures saved to bexar_figures/")
