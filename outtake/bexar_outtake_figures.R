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

fresh_outtake <- dp_raw |>
  group_by(`OUTTAKE-PROSECUTOR`) |>
  summarise(first_year = min(`CASE-YEAR`, na.rm = TRUE), .groups = "drop") |>
  filter(first_year >= 1991) |>
  pull(`OUTTAKE-PROSECUTOR`)

offense_order <- c("F1", "F2", "F3", "FS")

dp_out <- dp_raw |>
  filter(`OUTTAKE-PROSECUTOR` %in% fresh_outtake,
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

# ── Figure 7: Dot plot — predicted probability gap, career start vs peak ──────
# Model-based counterpart to Table 5 (career start vs peak by offense class).
# Two panels (W-B, W-L), offense class on y-axis, two dots per row connected
# by a segment showing the change.

library(fixest)

dp_out_pred <- dp_out |>
  mutate(
    BLACK     = as.integer(`RACE-LABEL` == "Black"),
    LATINO    = as.integer(`RACE-LABEL` == "Latino"),
    APPOINTED = as.integer(`ATTORNEY-TYPE` == "Appointed"),
    OFFENSE_TYPE = fct_relevel(`OFFENSE-CLASS`, "F3"),
    OUTTAKE_CASE_N100 = OUTTAKE_CASE_N / 100,
    OFFENSE_CATEGORY2 = case_when(
      str_detect(`OFFENSE-DESC`, "POSS CS|POSS W/I DEL CS|POSS W/INT DEL CS|MAN/DEL CS|DEL CS|POSS MARIJ") ~ "Drug",
      str_detect(`OFFENSE-DESC`, "BURGLARY|BURG HAB|BURG VEHICLE") ~ "Burglary",
      str_detect(`OFFENSE-DESC`, "EVADING ARREST") ~ "Evading",
      str_detect(`OFFENSE-DESC`, "MURDER|HOMICIDE|MANSLAUGHTER") ~ "Homicide",
      str_detect(`OFFENSE-DESC`, "AGG ASSLT|ASSLT|INJURY TO CHILD|RETALIATION") ~ "Assault",
      str_detect(`OFFENSE-DESC`, "FORG|CREDIT/DEBIT|FRAUD|THEFT|UNAUTH USE VEH|CRIM MISCH") ~ "Property",
      str_detect(`OFFENSE-DESC`, "DWI|DRIV WHILE INTOX") ~ "DWI",
      str_detect(`OFFENSE-DESC`, "SEX|RAPE|INDECENCY|SEXUAL") ~ "Sex",
      str_detect(`OFFENSE-DESC`, "WEAPON|WPN|CARRY") ~ "Weapon",
      TRUE ~ "Other"
    ),
    OFFENSE_CATEGORY2 = fct_relevel(OFFENSE_CATEGORY2, "Other")
  ) |>
  filter(!is.na(OFFENSE_CATEGORY2))

fit_pred7 <- glm(
  DEFERRED ~ BLACK + LATINO + OUTTAKE_CASE_N100 +
    BLACK:OUTTAKE_CASE_N100 + LATINO:OUTTAKE_CASE_N100 +
    OFFENSE_TYPE + OFFENSE_CATEGORY2 + APPOINTED,
  data = dp_out_pred, family = binomial()
)

max_n100 <- max(dp_out_pred$OUTTAKE_CASE_N100, na.rm = TRUE)
modal_oc2 <- names(sort(table(dp_out_pred$OFFENSE_CATEGORY2), decreasing = TRUE))[1]

offense_types  <- c("F1", "F2", "F3", "FS")
offense_labels <- c("F1 (first degree)", "F2 (second degree)",
                    "F3 (third degree)", "FS (state jail)")

dot_rows <- purrr::map_dfr(seq_along(offense_types), function(j) {
  oc <- offense_types[j]
  nd <- function(b, l, n100) {
    tibble(BLACK = b, LATINO = l, OUTTAKE_CASE_N100 = n100,
           OFFENSE_TYPE = oc, OFFENSE_CATEGORY2 = modal_oc2, APPOINTED = 1L)
  }
  w_start <- predict(fit_pred7, nd(0, 0, 0.01),   type = "response")
  b_start <- predict(fit_pred7, nd(1, 0, 0.01),   type = "response")
  l_start <- predict(fit_pred7, nd(0, 1, 0.01),   type = "response")
  w_peak  <- predict(fit_pred7, nd(0, 0, max_n100), type = "response")
  b_peak  <- predict(fit_pred7, nd(1, 0, max_n100), type = "response")
  l_peak  <- predict(fit_pred7, nd(0, 1, max_n100), type = "response")

  bind_rows(
    tibble(panel = "Panel A: White–Black gap",
           offense = offense_labels[j],
           xmin = (w_start - b_start) * 100,
           xmax = (w_peak  - b_peak)  * 100),
    tibble(panel = "Panel B: White–Latino gap",
           offense = offense_labels[j],
           xmin = (w_start - l_start) * 100,
           xmax = (w_peak  - l_peak)  * 100)
  )
}) |>
  mutate(
    offense = factor(offense, levels = rev(offense_labels)),
    panel   = factor(panel, levels = c("Panel A: White–Black gap",
                                        "Panel B: White–Latino gap"))
  )

# Average change labels for each panel
avg_change <- dot_rows |>
  group_by(panel) |>
  summarise(avg = mean(xmax - xmin), .groups = "drop") |>
  mutate(label = sprintf("avg. change: +%.1f pp", avg),
         x = 7, offense = factor(offense_labels[1], levels = levels(dot_rows$offense)))

p7 <- ggplot(dot_rows, aes(y = offense)) +
  geom_segment(aes(x = xmin, xend = xmax, yend = offense),
               color = "gray50", linewidth = 0.8,
               arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  geom_point(aes(x = xmin), color = "#2166ac", size = 3.5, shape = 16) +
  geom_point(aes(x = xmax), color = "#d6604d", size = 3.5, shape = 16) +
  geom_text(aes(x = xmin, label = sprintf("%.1f", xmin)),
            hjust = 1.3, size = 3, color = "#2166ac") +
  geom_text(aes(x = xmax, label = sprintf("%.1f", xmax)),
            hjust = -0.3, size = 3, color = "#d6604d") +
  geom_text(data = avg_change, aes(x = x, y = offense, label = label),
            vjust = -1.8, size = 2.9, color = "gray30", fontface = "italic") +
  facet_wrap(~panel, nrow = 1) +
  scale_x_continuous(limits = c(2, 13),
                     breaks = seq(2, 12, 2),
                     labels = function(x) paste0(x, " pp")) +
  labs(
    title   = "Predicted Racial Gap in Deferred Adjudication: Career Start vs. Peak",
    x       = "Predicted gap (percentage points)",
    y       = NULL,
    caption = str_wrap(paste0(
      "Notes: Blue dot = career start (case 1); red dot = career peak (maximum observed case N). ",
      "Predicted from logistic regression at modal offense category and attorney type = appointed counsel. ",
      "Sample: felony cases, prosecutors first observed 1991 or later, 1991–2015."
    ), width = 110)
  ) +
  theme_paper +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    axis.text.y        = element_text(size = 10)
  )

save_fig(p7, "fig7_dotplot_start_vs_peak.png", w = 11, h = 5)

# ── Figure 8: Robustness coefficient plot — interaction terms only ─────────────
# Requires: bexar_outtake_model_results.csv loaded as `res`

res <- read_csv(file.path(DATA_DIR, "bexar_outtake_model_results.csv"),
                show_col_types = FALSE)

rob_models <- c("SpecD_WithPriors", "SpecC_DefAge", "SpecC_AppointedOnly")
rob_labels <- c("Benchmark", "+ Age control", "Appointed only")

coef_plot_data <- res |>
  filter(model %in% rob_models) |>
  mutate(
    term_clean = case_when(
      term %in% c("BLACK:OUTTAKE_CASE_N100", "OUTTAKE_CASE_N100:BLACK") ~
        "Black × Experience",
      term %in% c("LATINO:OUTTAKE_CASE_N100", "OUTTAKE_CASE_N100:LATINO") ~
        "Latino × Experience",
      TRUE ~ NA_character_
    ),
    ci_lo = estimate - 1.96 * std.error,
    ci_hi = estimate + 1.96 * std.error,
    model = factor(model, levels = rob_models, labels = rob_labels)
  ) |>
  filter(!is.na(term_clean)) |>
  mutate(term_clean = factor(term_clean,
    levels = c("Black × Experience", "Latino × Experience")))

p8 <- ggplot(coef_plot_data, aes(x = estimate, y = model, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.2, linewidth = 0.7) +
  geom_point(size = 3) +
  facet_wrap(~term_clean, nrow = 1) +
  scale_x_continuous(limits = c(0, 0.038),
                     breaks = seq(0, 0.035, 0.01),
                     labels = function(x) sprintf("%.2f", x)) +
  scale_color_manual(values = c("#1f4e79", "#c55a11", "#538135"), guide = "none") +
  labs(
    title   = "Robustness of Interaction Coefficients Across Specifications",
    x       = "Log-odds coefficient (95% CI)",
    y       = NULL,
    caption = paste0(
      "Notes: Horizontal bars are 95% confidence intervals, SEs clustered by prosecutor.\n",
      "Benchmark = Spec D (prosecutor FE + defendant case N). All felony cases 1991–2015."
    )
  ) +
  theme_paper +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    axis.text.y        = element_text(size = 10),
    plot.caption       = element_text(size = 8, color = "gray40", hjust = 0)
  )

save_fig(p8, "fig8_robustness_coefplot.png", w = 10, h = 4)

message("\nAll figures saved to ", FIG_DIR)
