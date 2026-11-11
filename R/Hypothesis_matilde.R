# =====================================================================
# Hypothesis Analysis: Matilde
# Objective: Tactical police resource allocation based on spatial, 
#            temporal, and severity patterns of NYPD crimes.
# =====================================================================

library(dplyr)
library(ggplot2)

dir.create("fig/02_exploration/02_matilde", showWarnings = FALSE, recursive = TRUE)

# Load clean data (created by R/01_clean.R)
nypd <- readRDS("data/nypd_clean.rds")
nypd_ocorr <- nypd |> filter(!old_occurrence)

# ---------------------------------------------------------------------
# RESEARCH HYPOTHESIS 1:
# Reported crime in New York City does not distribute randomly; it concentrates
# in specific boroughs and time windows, and the pattern differs by severity.

# Split into three testable parts:
#   H1a: some boroughs have more serious crime than their population would suggest
#        -> confirmed: the Bronx and Manhattan have the highest felony rates per resident
#   H1b: the afternoon peak (15:00-18:00) holds once round-hour times are removed
#        -> confirmed: felonies also keep a larger share at night
#   H1c: the most frequent offenses changed differently since 2020
#        -> confirmed: five of the six rose, only criminal mischief fell

# Dropped from the first version: holidays and victim profiles. Holidays are
# covered by hypothesis 2 (assaults on officers) and victims by hypothesis 3.

# Decision it supports: when and where to reinforce NYPD patrols.

# Structure of this script:
#   Exploration (figures 1 to 3): the figures that led to the hypothesis
#   Tests (H1a, H1b, H1c): one figure per part of the hypothesis
# ---------------------------------------------------------------------

# Plot styling options
dv_blue <- "#2E75B6"
source_caption <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"

# colorblind-friendly palette
tactical_colors <- c(
  "FELONY"      = "#D55E00", 
  "MISDEMEANOR" = "#0072B2", 
  "VIOLATION"   = "#009E73" 
)

# =====================================================================
# EXPLORATION
# =====================================================================
# ---------------------------------------------------------------------
# 1: Summary of most frequent crimes by severity category
# ---------------------------------------------------------------------

tactical_crimes <- nypd_ocorr |>
  count(ofns_desc, law_cat_cd, sort = TRUE) |>
  head(10)

print(tactical_crimes)

g_matilde_1 <- ggplot(tactical_crimes, aes(x = reorder(ofns_desc, n), y = n, fill = law_cat_cd)) +
  geom_col() +
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 3, color = "black") +
  coord_flip() +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Top Crimes for NYPD Tactical Intervention",
    subtitle = "Volume by offense type and severity category",
    x = "Offense Description",
    y = "Number of Complaints",
    fill = "Law Category",
    caption = source_caption
  ) +
  scale_fill_manual(values = tactical_colors) +
  theme_minimal()
ggsave("fig/02_exploration/02_matilde/top_tactical_crimes.png", plot = g_matilde_1, width = 10, height = 6, dpi = 300)

# Observed: two low-severity offenses lead by far, petit larceny (misdemeanor, 656,833)
# and harassment (violation, 522,367). The most frequent felonies (grand larceny,
# felony assault, robbery) are far less common. Counts are by offense and law category,
# so an offense with two categories (e.g. criminal mischief) appears only in its larger part.

# ---------------------------------------------------------------------
# 2: Crime severity distribution by Borough
# ---------------------------------------------------------------------

spatial_severity <- nypd_ocorr |>
  filter(!is.na(boro_nm) & boro_nm != "") |>
  count(boro_nm, law_cat_cd)

g_matilde_2 <- ggplot(spatial_severity, aes(x = reorder(boro_nm, n), y = n, fill = law_cat_cd)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::comma(n)), position = position_dodge(width = 0.9), hjust = -0.1, size = 2.8, color = "black") +
  coord_flip() +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Crime Severity Distribution by Borough",
    subtitle = "Comparison of Felonies, Misdemeanors, and Violations across NYC",
    x = "Borough",
    y = "Number of Complaints",
    fill = "Law Category",
    caption = source_caption
  ) +
  scale_fill_manual(values = tactical_colors) +
  # coord_flip puts FELONY at the bottom of each group, so reverse the legend to match the bars
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal()

print(g_matilde_2)
ggsave("fig/02_exploration/02_matilde/crime_severity_by_borough.png", plot = g_matilde_2, width = 10, height = 6, dpi = 300)

# Observed: in absolute counts Brooklyn and Manhattan lead and Staten Island is far behind,
# and misdemeanors are the largest category in every borough. These are raw counts, so
# they mostly follow population size: this led to H1a (rates per resident).

# ---------------------------------------------------------------------
# 3: Crime concentration by hour of the day and severity
# ---------------------------------------------------------------------
hourly_crime <- nypd_ocorr |>
  filter(!is.na(hour)) |>
  count(hour, law_cat_cd)

g_matilde_3 <- ggplot(hourly_crime, aes(x = hour, y = n, color = law_cat_cd)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Hourly Trend of Crime Volume by Severity Category",
    subtitle = "Tactical distribution of complaints across 24-hour windows\nSpikes at 00h and 12h are partly unknown times recorded as 00:00 / 12:00",
    x = "Hour of the Day (0 - 23)",
    y = "Number of Complaints",
    color = "Law Category",
    caption = source_caption
  ) +
  scale_color_manual(values = tactical_colors) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma)

print(g_matilde_3)
ggsave("fig/02_exploration/02_matilde/hourly_crime_by_severity.png", plot = g_matilde_3, width = 10, height = 6, dpi = 300)

# Observed: all three categories follow the same daily cycle, with a minimum at 04:00-06:00
# and a plateau at 15:00-18:00. The spikes at 00:00 and 12:00 are suspicious: the data
# quality checks show many more complaints at exactly 00:00 and 12:00 than in the
# neighbouring minutes. This led to H1b (the same cycle without round-hour times).

# =====================================================================
# TESTS OF HYPOTHESIS 1
# =====================================================================
# ---------------------------------------------------------------------
# H1a: do some boroughs have more crime than their population would suggest?
# Complaints per 100,000 residents per year, average of report years 2020-2025.
# Population: U.S. Census Bureau, 2020 Census (official counts per borough).
# Manhattan has many commuters and tourists who are not residents, so its rate
# per resident overstates the risk per person present.
# ---------------------------------------------------------------------
rate_years <- 2020:2025

borough_pop <- data.frame(
  boro_nm    = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"),
  population = c(1472654, 2736074, 1694251, 2405464, 495747)
)

borough_rates <- nypd_ocorr |>
  filter(report_year %in% rate_years, !is.na(boro_nm)) |>
  count(boro_nm, law_cat_cd) |>
  inner_join(borough_pop, by = "boro_nm") |>
  mutate(rate = n / length(rate_years) / population * 1e5)

print(borough_rates)

# Order the boroughs by their felony rate
felony_order <- borough_rates |>
  filter(law_cat_cd == "FELONY") |>
  arrange(rate) |>
  pull(boro_nm)

borough_rates <- borough_rates |>
  mutate(boro_nm = factor(boro_nm, levels = felony_order))

g_matilde_h1a <- ggplot(borough_rates, aes(x = rate, y = boro_nm, fill = law_cat_cd)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  geom_text(aes(label = scales::comma(round(rate))), position = position_dodge(width = 0.8),
            hjust = -0.1, size = 2.8, color = "black") +
  scale_fill_manual(values = tactical_colors) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Complaints per 100,000 Residents per Year, by Borough",
    subtitle = "Average of report years 2020-2025. Boroughs ordered by felony rate",
    x = "Complaints per 100,000 residents per year",
    y = NULL,
    fill = "Law Category",
    caption = paste0(source_caption, "\nPopulation: U.S. Census Bureau, 2020 Census")
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

print(g_matilde_h1a)
ggsave("fig/02_exploration/02_matilde/h1a_borough_rates.png",
       plot = g_matilde_h1a, width = 10, height = 6, dpi = 300)

# Observed (H1a confirmed):
#   * Adjusted for population, the Bronx has the highest felony rate (2,567 per 100,000
#     residents per year), about 1.5 times Brooklyn (1,748) and 2.2 times Staten Island
#     (1,183). It also has the highest violation rate (1,255).
#   * Manhattan is second in felonies (2,436) and first in misdemeanors (3,938), but its
#     rate is overstated because many people in Manhattan during the day are not residents.
#   * Brooklyn, first in absolute counts (figure 2), drops to third once population is
#     taken into account.
#   * Staten Island keeps the lowest felony rate, but its misdemeanor rate (2,329) is
#     almost the same as Queens (2,383).
# Limitation: one population count (2020) is used for all six years.


# ---------------------------------------------------------------------
# H1b: does the afternoon peak hold without the round-hour times?
# Complaints recorded at exactly 00:00 or 12:00 are removed, because they may stand
# for an unknown time. Each line is the share of its own law category, so the three
# categories can be compared by the shape of their daily cycle, not by their volume.
# ---------------------------------------------------------------------
hourly_share <- nypd_ocorr |>
  filter(!is.na(hour), !cmplnt_fr_tm %in% c("00:00:00", "12:00:00")) |>
  count(law_cat_cd, hour) |>
  group_by(law_cat_cd) |>
  mutate(pct = 100 * n / sum(n)) |>
  # Hour 12 lost most of its complaints with the 12:00 filter, so its share is not comparable
  mutate(pct = if_else(hour == 12, NA_real_, pct)) |>
  ungroup()

g_matilde_h1b <- ggplot(hourly_share, aes(x = hour, y = pct, color = law_cat_cd)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = tactical_colors) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Share of Complaints by Hour, Within Each Severity Category",
    subtitle = "Complaints recorded at exactly 00:00 or 12:00 are excluded (possible unknown times); hour 12 is not shown",
    x = "Hour of the Day (0 - 23)",
    y = "% of the Category's Complaints",
    color = "Law Category",
    caption = source_caption
  ) +
  theme_minimal()

print(g_matilde_h1b)
ggsave("fig/02_exploration/02_matilde/h1b_hourly_share_no_round_hours.png",
       plot = g_matilde_h1b, width = 10, height = 6, dpi = 300)

# Observed (H1b confirmed):
#   * Without the round-hour times, all three categories still peak at 15:00-18:00,
#     with about 6% of their complaints in each of those hours. The afternoon peak is real.
#   * Felonies follow a flatter cycle: they keep a larger share at night. At midnight
#     there are about 4.1% of felonies, against 3.1% of misdemeanors and 2.6% of
#     violations, and felonies stay above the other two from 21:00 to 04:00.
#   * Violations weigh more in the morning (08:00-11:00) than the other two categories.
# Limitation: hour 0 may still be slightly inflated, because the data quality checks also
# show an excess of complaints at 00:01 (possibly another way of recording an unknown time).


# ---------------------------------------------------------------------
# H1c: did the most frequent offenses change differently since 2020?
# Index 2020 = 100 by report year. 2026 is left out because it only runs until June.
# Offense names are cut to 30 characters, because some were truncated until 2023
# and written in full from 2024 (e.g. "CRIMINAL MISCHIEF & RELATED OF").
# ---------------------------------------------------------------------
years <- 2020:2025

offense_year <- nypd_ocorr |>
  filter(report_year %in% years, !is.na(ofns_desc)) |>
  mutate(offense = substr(ofns_desc, 1, 30)) |>
  count(offense, report_year)

top_offenses <- offense_year |>
  group_by(offense) |>
  summarise(total = sum(n), .groups = "drop") |>
  slice_max(total, n = 6) |>
  pull(offense)

offense_index <- offense_year |>
  filter(offense %in% top_offenses) |>
  group_by(offense) |>
  arrange(report_year, .by_group = TRUE) |>
  mutate(index = 100 * n / first(n)) |>
  ungroup()

print(offense_index, n = Inf)

# Law category of each offense (the most common one, since a few offenses have more than one)
offense_level <- nypd_ocorr |>
  filter(!is.na(ofns_desc)) |>
  mutate(offense = substr(ofns_desc, 1, 30)) |>
  count(offense, law_cat_cd) |>
  group_by(offense) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(offense, law_cat_cd)

offense_index <- offense_index |> left_join(offense_level, by = "offense")

# Spread the end labels so they do not overlap: each label sits at least `gap` index points above the previous one
spread_labels <- function(y, gap) {
  for (i in seq_along(y)[-1]) y[i] <- max(y[i], y[i - 1] + gap)
  y
}
index_ends <- offense_index |>
  filter(report_year == max(years)) |>
  arrange(index) |>
  mutate(label_y = spread_labels(index, gap = 8))

g_matilde_h1c <- ggplot(offense_index, aes(x = report_year, y = index, color = law_cat_cd, group = offense)) +
  geom_hline(yintercept = 100, colour = "grey50", linetype = "dashed") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  geom_segment(data = index_ends, aes(x = report_year, xend = report_year + 0.1, y = index, yend = label_y),
               linewidth = 0.3, show.legend = FALSE) +
  geom_text(data = index_ends, aes(y = label_y, label = offense), x = max(years) + 0.12,
            hjust = 0, size = 3, show.legend = FALSE) +
  scale_color_manual(values = tactical_colors) +
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.5))) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Change in the Six Most Frequent Offenses Since 2020",
    subtitle = "Index by report year, 2020 = 100",
    x = "Report Year",
    y = "Index (2020 = 100)",
    caption = source_caption,
    color = "Law Category"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

print(g_matilde_h1c)
ggsave("fig/02_exploration/02_matilde/h1c_offense_index.png",
       plot = g_matilde_h1c, width = 10, height = 6, dpi = 300)

# Observed (H1c confirmed):
#   * Five of the six most frequent offenses rose between 2020 and 2025: felony assault,
#     grand larceny, assault 3, petit larceny and harassment.
#   * Both felonies are among the largest rises. Felony assault rose almost every year,
#     while grand larceny peaked in 2022 and then eased.
#   * Criminal mischief is the only one that fell, and it fell steadily every year.
# Limitation: 2020 was the COVID year, with fewer people in the streets, so part of
# the rise is a return to normal activity, not only an increase in crime.
