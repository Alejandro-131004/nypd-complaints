# =====================================================================
# HYPOTHESIS 1, reformulated after the checkpoint 1 feedback (Matilde)
# The first version and its tests (H1a-H1d) are in R/Hypothesis_matilde.R.
# =====================================================================
library(dplyr)
library(ggplot2)

dir.create("fig/02_exploration/02_matilde", showWarnings = FALSE, recursive = TRUE)

# Load clean data (created by R/01_clean.R)
nypd <- readRDS("data/nypd_clean.rds")
nypd_ocorr <- nypd |> filter(!old_occurrence)

source_caption <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"

# Feedback (checkpoint 1): "Reported crime in New York City does not distribute
# randomly" is true in any city and any year, so no figure can contradict it.
# A verifiable hypothesis must name the borough, the time of day and the type of
# offense, with a value that the data can contradict.
#
# H1 (reformulated):
#   In the Bronx, more than 50% of felony assault complaints, and more than 50% of
#   grand larceny complaints, occur at night (21:00-04:59), in at least 5 of the
#   6 years from 2020 to 2025.
#
# Criterion, fixed before looking at the result: each offense is tested on its own;
#   it supports the hypothesis if its night share is above 50% in at least 5 of the 6 years.
# Night: hours 21, 22, 23, 0, 1, 2, 3 and 4 (8 of 24 hours), the window where felonies
#   stay above misdemeanors and violations in H1b. If complaints were spread evenly
#   over the day, these 8 hours would hold 33.3% of them.
# Note: complaints recorded at exactly 00:00 or 12:00 are excluded, as in H1b,
#   because they may stand for an unknown time.
# ---------------------------------------------------------------------

h1_years    <- 2020:2025
h1_offenses <- c("FELONY ASSAULT", "GRAND LARCENY")
night_hours <- c(21:23, 0:4)

bronx_h1 <- nypd_ocorr |>
  filter(boro_nm == "BRONX",
         report_year %in% h1_years,
         !is.na(hour),
         !cmplnt_fr_tm %in% c("00:00:00", "12:00:00"),
         ofns_desc %in% h1_offenses)

# Check: number of complaints kept per offense (should be in the thousands per year)
bronx_h1 |> count(ofns_desc, report_year) |> print()

# Night share per offense and year
night_share_year <- bronx_h1 |>
  mutate(night = hour %in% night_hours) |>
  group_by(ofns_desc, report_year) |>
  summarise(n_total  = n(),
            n_night  = sum(night),
            pct_night = 100 * n_night / n_total,
            .groups = "drop") |>
  mutate(above_50 = pct_night > 50)

print(night_share_year, n = Inf)

# Verdict per offense: night share above 50% in at least 5 of the 6 years?
night_share_year |>
  group_by(ofns_desc) |>
  summarise(years_above_50 = sum(above_50),
            supported      = years_above_50 >= 5,
            .groups = "drop") |>
  print()

# Data for the figure (to be drawn by the group): share of each hour of the day,
# per offense, Bronx, 2020-2025. Hour 12 is set to NA as in H1b.
bronx_hourly_share <- bronx_h1 |>
  count(ofns_desc, hour) |>
  group_by(ofns_desc) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  mutate(pct   = if_else(hour == 12, NA_real_, pct),
         night = hour %in% night_hours)

# One colour per offense (Okabe-Ito, colorblind-friendly)
offense_colors <- c(
  "FELONY ASSAULT" = "#D55E00",
  "GRAND LARCENY"  = "#0072B2"
)

h1_bronx_night_share <- ggplot(bronx_hourly_share, aes(x = hour, y = pct, color = ofns_desc)) +
  annotate("rect", xmin = 20.5, xmax = 23.5, ymin = -Inf, ymax = Inf, alpha = 0.1) +
  annotate("rect", xmin = -0.5, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.1) +
  geom_hline(yintercept = 100 / 24, colour = "grey50", linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = offense_colors) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Felony Assault and Grand Larceny by Hour in the Bronx",
    subtitle = "2020-2025. Shaded: night (21:00-04:59). Hour 12 is not shown.\nDashed line: 4.2%, the share of each hour if complaints were spread evenly.\nComplaints at exactly 00:00 or 12:00 are excluded (possible unknown times)",
    x = "Hour of the Day (0 - 23)",
    y = "% of the Offense's Complaints",
    color = "Offense",
    caption = source_caption
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

print(h1_bronx_night_share)
ggsave("fig/02_exploration/02_matilde/h1_bronx_night_share.png",
       plot = h1_bronx_night_share, width = 7, height = 4.5, dpi = 300)

# Observed (figure h1_bronx_night_share.png):
#   * Grand larceny peaks in the afternoon (15:00-17:00); at night, its highest hour is 21:00.
#   * Felony assault peaks at 20:00, just before the night window; at night, its highest hour is 21:00.

# Observed (H1 reformulated, refuted):
#   * The hypothesis is refuted for both offenses: although many complaints happen at night,
#     the night share never reaches 50% (0 of the 6 years, for both offenses).
#   * Felony assault: the night share ranges from 33.5% to 38.5%, slightly above the 33.3%
#     expected if complaints were spread evenly over the day.
#   * Grand larceny: the night share ranges from 22.7% to 25.8%, below the 33.3% in every year.

# Sensitivity check: keep the complaints at exactly 00:00 (most favourable case for the hypothesis)
night_share_with_midnight <- nypd_ocorr |>
  filter(boro_nm == "BRONX",
         report_year %in% h1_years,
         !is.na(hour),
         cmplnt_fr_tm != "12:00:00",
         ofns_desc %in% h1_offenses) |>
  mutate(night = hour %in% night_hours) |>
  group_by(ofns_desc, report_year) |>
  summarise(n_total   = n(),
            n_night   = sum(night),
            pct_night = 100 * n_night / n_total,
            .groups = "drop")

print(night_share_with_midnight, n = Inf)

# Observed (sensitivity check, complaints at 00:00 kept):
#   * Even keeping the complaints at exactly 00:00, the night share never reaches 50%:
#     felony assault ranges from 34.1% to 39.0% and grand larceny from 24.6% to 27.7%.
#     The verdict does not change.
#   * In both versions, felony assault stays above the 33.3% expected if complaints were
#     spread evenly over the day, in every year; grand larceny stays below it.

h1_night_by_year <- ggplot(night_share_year, aes(x = report_year, y = pct_night, color = ofns_desc, group = ofns_desc)) +
  geom_hline(yintercept = 50, colour = "grey30", linetype = "dashed") +
  geom_hline(yintercept = 100 * 8 / 24, colour = "grey60", linetype = "dotted") +
  annotate("text", x = 2020, y = 53, label = "50% (criterion)", hjust = 0, size = 3, colour = "grey30") +
  annotate("text", x = 2020, y = 31.8, label = "33.3% (evenly spread)", hjust = 0, size = 3, colour = "grey50") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = offense_colors) +
  scale_x_continuous(breaks = h1_years, expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(0, 60), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Share of Complaints at Night in the Bronx, by Year",
    subtitle = "Felony assault and grand larceny. Night: 21:00-04:59.\nDashed line: 50%, the criterion. Dotted line: 33.3%, if complaints were spread evenly.\nComplaints at exactly 00:00 or 12:00 are excluded (possible unknown times)",
    x = "Report Year",
    y = "Night share (21:00-04:59)",
    color = "Offense",
    caption = source_caption
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")

print(h1_night_by_year)
ggsave("fig/02_exploration/02_matilde/h1_bronx_night_share_by_year.png",
       plot = h1_night_by_year, width = 7, height = 4.5, dpi = 300)

# Observed (figure h1_bronx_night_share_by_year.png):
#   * Grand larceny stays below the 33.3% expected at night in every year.
#   * Felony assault stays above 33.3% in every year but never reaches the 50% criterion.
#   * The night share of felony assault decreased slightly over the years, from 38.5% in 2020
#     to 33.6% in 2025 (about 5 percentage points).

# =====================================================================
# HYPOTHESIS 1, second reformulation: afternoon peak of petit larceny
# =====================================================================
# Origin: this hypothesis comes from the exploration, not from a criterion fixed
# before looking at the data. After the night hypothesis was refuted, the hourly
# figures (hourly_crime_by_severity.png, all boroughs together, and
# h1_bronx_night_share.png) showed that complaints peak in the afternoon, and
# misdemeanors stay at their highest level from 15:00 to 18:59.
#
# H1 (second reformulation):
#   In Manhattan, and separately in the Bronx, more than 33.3% of petit larceny
#   complaints occur in the afternoon (15:00-18:59), in at least 5 of the 6 years
#   from 2020 to 2025.
#
# Criterion: each borough is tested on its own; it supports the hypothesis if its
#   afternoon share is above 33.3% in at least 5 of the 6 years.
# Afternoon: hours 15, 16, 17 and 18 (4 of 24 hours). If complaints were spread evenly
#   over the day, these 4 hours would hold 16.7% of them; the threshold (33.3%) is
#   twice that value.
# Boroughs: Manhattan has the highest misdemeanor rate per resident and the Bronx
#   the second highest (h1a_borough_rates.png). The afternoon share is computed
#   within each borough, so it does not depend on population.
# Note: complaints recorded at exactly 00:00 or 12:00 are excluded, as before,
#   because they may stand for an unknown time.
# ---------------------------------------------------------------------

pm_boroughs       <- c("MANHATTAN", "BRONX")
pm_offense        <- "PETIT LARCENY"
afternoon_hours   <- 15:18
afternoon_even    <- 100 * length(afternoon_hours) / 24   # 16.7%
afternoon_thresh  <- 2 * afternoon_even  # 33.3%

petit_pm <- nypd_ocorr |>
  filter(boro_nm %in% pm_boroughs,
         report_year %in% h1_years,
         !is.na(hour),
         !cmplnt_fr_tm %in% c("00:00:00", "12:00:00"),
         ofns_desc == pm_offense)

# Check: number of complaints kept per borough and year
petit_pm |> count(boro_nm, report_year) |> print()

# Afternoon share per borough and year
afternoon_share_year <- petit_pm |>
  mutate(afternoon = hour %in% afternoon_hours) |>
  group_by(boro_nm, report_year) |>
  summarise(n_total       = n(),
            n_afternoon   = sum(afternoon),
            pct_afternoon = 100 * n_afternoon / n_total,
            .groups = "drop") |>
  mutate(above_thresh = pct_afternoon > afternoon_thresh)

print(afternoon_share_year, n = Inf)

# Verdict per borough: afternoon share above 33.3% in at least 5 of the 6 years?
afternoon_share_year |>
  group_by(boro_nm) |>
  summarise(years_above = sum(above_thresh),
            supported   = years_above >= 5,
            .groups = "drop") |>
  print()

# Observed (H1 second reformulation, refuted):
#   * Manhattan has about twice as many petit larceny complaints as the Bronx in every year
#     (counts, not rates per resident).
#   * The hypothesis is refuted for both boroughs: the afternoon share never exceeds 33.3%
#     (0 of the 6 years, for both boroughs).
#   * Bronx: the afternoon share ranges from 26.7% to 29.0%; Manhattan: from 28.2% to 29.0%.
#     Both stay well above the 16.7% expected if complaints were spread evenly over the day.


# Data for the figures
# 1) Share of each hour of the day, per borough, 2020-2025 (hour 12 set to NA)
petit_hourly_share <- petit_pm |>
  count(boro_nm, hour) |>
  group_by(boro_nm) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  mutate(pct= if_else(hour == 12, NA_real_, pct),
         afternoon = hour %in% afternoon_hours)

# 2) Afternoon share per year: afternoon_share_year, created above for the verdict
# One colour per borough 
borough_colors <- c(
  "BRONX"         = "#D55E00",
  "MANHATTAN"     = "#0072B2",
  "BROOKLYN"      = "#009E73",
  "QUEENS"        = "#CC79A7",
  "STATEN ISLAND" = "#E69F00"
)

h1_petit_afternoon_share <- ggplot(petit_hourly_share, aes(x = hour, y = pct, color = boro_nm)) +
  annotate("rect", xmin = 14.5, xmax = 18.5, ymin = -Inf, ymax = Inf, alpha = 0.1) +
  geom_hline(yintercept = 100 / 24, colour = "grey50", linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = borough_colors) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Share of Petit Larceny Complaints by Hour, Manhattan and the Bronx",
    subtitle = "2020-2025. Shaded: afternoon (15:00-18:59). Hour 12 is not shown.\nDashed line: 4.2%, the share of each hour if complaints were spread evenly.\nComplaints at exactly 00:00 or 12:00 are excluded (possible unknown times)",
    x = "Hour of the Day (0 - 23)",
    y = "% of the Borough's Petit Larceny Complaints",
    color = "Borough",
    caption = source_caption
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

print(h1_petit_afternoon_share)
ggsave("fig/02_exploration/02_matilde/h1_petit_afternoon_share.png",
       plot = h1_petit_afternoon_share, width = 7, height = 4.5, dpi = 300)

# Observed:
#   * Both boroughs peak in the afternoon: Manhattan at 15:00 and the Bronx at 17:00-18:00.
#   * The window was chosen from all misdemeanors in the city, but it also fits petit larceny:
#     the peaks of both boroughs fall inside it. In Manhattan, however, the shares at 13:00 and
#     14:00 are almost as high as the peak, so part of its high period starts before the window.
#   * In both boroughs the shares are very low at night, rise above 4.2% (the even share) around
#     08:00 and fall below it again at 21:00 in Manhattan and at 22:00 in the Bronx.


h1_petit_afternoon_by_year <- ggplot(afternoon_share_year, aes(x = report_year, y = pct_afternoon, color = boro_nm, group = boro_nm)) +
  geom_hline(yintercept = afternoon_thresh, colour = "grey30", linetype = "dashed") +
  geom_hline(yintercept = afternoon_even, colour = "grey60", linetype = "dotted") +
  annotate("text", x = 2020, y = 34.8, label = "33.3% (criterion)", hjust = 0, size = 3, colour = "grey30") +
  annotate("text", x = 2020, y = 15.2, label = "16.7% (evenly spread)", hjust = 0, size = 3, colour = "grey50") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = borough_colors) +
  scale_x_continuous(breaks = h1_years, expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(0, 40), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Share of Petit Larceny Complaints in the Afternoon, by Year",
    subtitle = "Manhattan and the Bronx. Afternoon: 15:00-18:59.\nDashed line: 33.3%, the criterion. Dotted line: 16.7%, if complaints were spread evenly.\nComplaints at exactly 00:00 or 12:00 are excluded (possible unknown times)",
    x = "Report Year",
    y = "Afternoon share (15:00-18:59)",
    color = "Borough",
    caption = source_caption
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")

print(h1_petit_afternoon_by_year)
ggsave("fig/02_exploration/02_matilde/h1_petit_afternoon_by_year.png",
       plot = h1_petit_afternoon_by_year, width = 7, height = 4.5, dpi = 300)

# Observed:
#   * Both lines stay close to 30% and never reach the 33.3% criterion.
#   * There is little variation over the years, and both boroughs stay well above the 16.7%
#     expected if complaints were spread evenly.
