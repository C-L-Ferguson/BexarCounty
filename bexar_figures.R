# Bexar County — Figures for Law Review Paper
# Requires: bexar_panel_1990_2021.parquet (from bexar_clean.R)
# Output: bexar_figures/ directory with PNG files

library(tidyverse)
library(arrow)

dir.create("bexar_figures", showWarnings = FALSE)

panel   <- read_parquet("bexar_panel_1990_2021.parquet")
primary <- panel |> filter(`RACE-LABEL` %in% c("Black", "White", "Latino"))

COLORS <- c(Black = "#1f4e79", Latino = "#c55a11", White = "#538135")
RACE_ORDER <- c("Black", "Latino", "White")

theme_paper <- theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    panel.grid.minor  = element_blank(),
    axis.title        = element_text(size = 11),
    plot.title        = element_text(size = 12, face = "bold"),
    plot.subtitle     = element_text(size = 10, color = "gray40")
  )

save_fig <- function(p, name, w = 8, h = 5) {
  path <- file.path("bexar_figures", name)
  ggsave(path, p, width = w, height = h, dpi = 150)
  message("Saved: ", path)
}

# ── Figure 1: Deferred adjudication rate by race × year ──────────────────────

yearly_def <- primary |>
  group_by(`CASE-YEAR`, Race = `RACE-LABEL`) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE), .groups = "drop") |>
  arrange(Race, `CASE-YEAR`) |>
  group_by(Race) |>
  mutate(rate_smooth = zoo::rollmean(rate, k = 3, fill = NA, align = "center")) |>
  ungroup()

# zoo may not be installed — fall back to simple mean if needed
if (!requireNamespace("zoo", quietly = TRUE)) {
  yearly_def <- yearly_def |> mutate(rate_smooth = rate)
  message("NOTE: install 'zoo' for smoothed trend lines")
}

p1 <- ggplot(yearly_def |> filter(!is.na(rate_smooth)),
             aes(`CASE-YEAR`, rate_smooth * 100, color = Race)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2019, linetype = "dashed",
             color = "gray50", linewidth = 0.7) +
  annotate("text", x = 2019.3, y = Inf, label = "2019 DA\ntransition",
           hjust = 0, vjust = 1.3, size = 3, color = "gray40") +
  scale_color_manual(values = COLORS, breaks = RACE_ORDER) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  labs(title = "Deferred Adjudication Rate by Race, 1990–2021",
       subtitle = "3-year rolling average",
       x = "Case Year", y = "Deferred Adjudication Rate",
       color = "Race/Ethnicity") +
  theme_paper

save_fig(p1, "fig1_deferred_trend.png")

# ── Figure 2: Appointed counsel rate by race × year ──────────────────────────

yearly_appt <- primary |>
  filter(`ATTORNEY-TYPE` %in% c("Appointed", "Hired")) |>
  mutate(is_appt = as.integer(`ATTORNEY-TYPE` == "Appointed")) |>
  group_by(`CASE-YEAR`, Race = `RACE-LABEL`) |>
  summarise(rate = mean(is_appt, na.rm = TRUE), .groups = "drop") |>
  arrange(Race, `CASE-YEAR`) |>
  group_by(Race) |>
  mutate(rate_smooth = if (requireNamespace("zoo", quietly = TRUE))
           zoo::rollmean(rate, k = 3, fill = NA, align = "center")
         else rate) |>
  ungroup()

p2 <- ggplot(yearly_appt |> filter(!is.na(rate_smooth)),
             aes(`CASE-YEAR`, rate_smooth * 100, color = Race)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = COLORS, breaks = RACE_ORDER) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  labs(title = "Rate of Appointed Counsel by Race, 1990–2021",
       subtitle = "3-year rolling average",
       x = "Case Year", y = "Appointed Counsel Rate",
       color = "Race/Ethnicity") +
  theme_paper

save_fig(p2, "fig2_appointed_trend.png")

# ── Figure 3: Deferred rate by race × attorney type ──────────────────────────

d3 <- primary |>
  filter(`ATTORNEY-TYPE` %in% c("Appointed", "Hired")) |>
  group_by(Race = `RACE-LABEL`, `Attorney Type` = `ATTORNEY-TYPE`) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE) * 100, .groups = "drop") |>
  mutate(Race = factor(Race, levels = RACE_ORDER))

p3 <- ggplot(d3, aes(Race, rate, fill = Race, alpha = `Attorney Type`)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  scale_fill_manual(values = COLORS, guide = "none") +
  scale_alpha_manual(values = c(Appointed = 0.6, Hired = 1.0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Deferred Adjudication Rate by Race and Attorney Type",
       x = NULL, y = "Deferred Adjudication Rate",
       alpha = "Counsel Type") +
  theme_paper

save_fig(p3, "fig3_deferred_by_attorney.png")

# ── Figure 4: Pre/Post 2019 DiD visual ───────────────────────────────────────

d4 <- primary |>
  mutate(Period = if_else(`CASE-YEAR` >= 2019, "2019–2021\n(Gonzales)", "Pre-2019\n(LaHood)")) |>
  group_by(Period, Race = `RACE-LABEL`) |>
  summarise(rate = mean(DEFERRED, na.rm = TRUE) * 100, .groups = "drop") |>
  mutate(Race = factor(Race, levels = RACE_ORDER),
         Period = factor(Period, levels = c("Pre-2019\n(LaHood)", "2019–2021\n(Gonzales)")))

p4 <- ggplot(d4, aes(Race, rate, fill = Race)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", rate)),
            vjust = -0.4, size = 3.2) +
  facet_wrap(~Period) +
  scale_fill_manual(values = COLORS, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Deferred Adjudication Rate Before and After 2019 DA Transition",
       x = NULL, y = "Deferred Adjudication Rate") +
  theme_paper +
  theme(strip.text = element_text(size = 10, face = "bold"))

save_fig(p4, "fig4_did_visual.png")

# ── Figure 5: Outcome rates overview ─────────────────────────────────────────

d5 <- primary |>
  group_by(Race = `RACE-LABEL`) |>
  summarise(
    `Deferred Adjudication` = mean(DEFERRED,      na.rm = TRUE) * 100,
    Dismissed               = mean(DISMISSED,     na.rm = TRUE) * 100,
    `Guilty Plea`           = mean(`GUILTY-PLEA`, na.rm = TRUE) * 100,
    Convicted               = mean(CONVICTED,     na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(-Race, names_to = "Outcome", values_to = "Rate") |>
  mutate(
    Race    = factor(Race, levels = RACE_ORDER),
    Outcome = factor(Outcome,
                     levels = c("Deferred Adjudication", "Dismissed",
                                "Guilty Plea", "Convicted"))
  )

p5 <- ggplot(d5, aes(Rate, Outcome, fill = Race)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  scale_fill_manual(values = COLORS, breaks = RACE_ORDER) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Case Outcome Rates by Race, 1990–2021",
       x = "Rate", y = NULL, fill = "Race/Ethnicity") +
  theme_paper

save_fig(p5, "fig5_outcome_overview.png", w = 9, h = 5)

message("\nAll figures saved to bexar_figures/")
