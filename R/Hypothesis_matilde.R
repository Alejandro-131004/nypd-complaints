# =====================================================================
# Hypothesis Analysis: Matilde
# Objective: Tactical police resource allocation based on spatial, temporal, and severity patterns of NYPD crimes.
# =====================================================================
#install.packages("tidyr")
library(dplyr)
library(ggplot2)

dir.create("fig/02_exploration/02_matilde", showWarnings = FALSE, recursive = TRUE)

# Load clean data (created by R/01_clean.R)
nypd <- readRDS("data/nypd_clean.rds")
nypd_ocorr <- nypd |> filter(!old_occurrence)

# ---------------------------------------------------------------------
# RESEARCH HYPOTHESIS 1:
# Reported crime in New York City does not distribute randomly; it concentrates in specific boroughs, time windows and contexts, and the pattern differs by severity.

# Split into four testable parts:
#   H1a: some boroughs have more serious crime than their population would suggest
#        -> confirmed: the Bronx and Manhattan have the highest felony rates per resident in every year; the Bronx leads since 2023, Manhattan led from 2020 to 2022
#   H1b: the afternoon peak (15:00-18:00) holds once round-hour times are removed
#        -> confirmed: felonies also keep a larger share at night
#   H1c: the most frequent offenses changed differently since 2020
#        -> confirmed: five of the six rose, only criminal mischief fell
#   H1d: complaints concentrate in streets and apartment buildings, and the mix of law categories differs by premises type
#        -> confirmed: felonies and misdemeanors occur mostly in the street (36.5% of all felonies and 29.8% of all misdemeanors),
#           while violations occur mostly in apartment buildings (35.7% of all violations). The street and commercial buildings
#           have the highest share of felonies among their own complaints (40.2% and 35.2%)

# Dropped from the first version: holidays and victim profiles. Holidays are covered by hypothesis 2 (assaults on officers) and victims by hypothesis 3.
# Decision it supports: when and where to reinforce NYPD patrols.
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

# Spread the end labels so they do not overlap: each label sits at least `gap` units above the previous one
spread_labels <- function(y, gap) {
  for (i in seq_along(y)[-1]) y[i] <- max(y[i], y[i - 1] + gap)
  y
}

# One colour per borough
borough_colors <- c(
  "BRONX"         = "#D55E00",
  "MANHATTAN"     = "#0072B2",
  "BROOKLYN"      = "#009E73",
  "QUEENS"        = "#CC79A7",
  "STATEN ISLAND" = "#E69F00"
)

# =====================================================================
# EXPLORATION
# =====================================================================
# ---------------------------------------------------------------------
# 1: Summary of most frequent crimes by severity category

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

# Observed: two low-severity offenses lead by far, petit larceny (misdemeanor, 656,833) and harassment (violation, 522,367). 
# The most frequent felonies (grand larceny, felony assault, robbery) are far less common. Counts are by offense and law category,
# so an offense with two categories (e.g. criminal mischief) appears only in its larger part.

# ---------------------------------------------------------------------
# 2: Crime severity distribution by Borough
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

# Observed: in absolute counts Brooklyn and Manhattan lead and Staten Island is far behind, and misdemeanors are the largest category in every borough. 
# These are raw counts, so they mostly follow population size: this led to H1a (rates per resident).

# ---------------------------------------------------------------------
# 3: Crime concentration by hour of the day and severity
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

# Observed: all three categories follow the same daily cycle, with a minimum at 04:00-06:00 and a plateau at 15:00-18:00. 
# The spikes at 00:00 and 12:00 are suspicious: the data quality checks show many more complaints at exactly 00:00 and 12:00 than in the
# neighbouring minutes. This led to H1b (the same cycle without round-hour times).

# =====================================================================
# TESTS OF HYPOTHESIS 1
# =====================================================================
# ---------------------------------------------------------------------
# H1a: do some boroughs have more crime than their population would suggest?
# Complaints per 100,000 residents per year, average of report years 2020-2025.
# Population: U.S. Census Bureau, 2020 Census (official counts per borough).
# Manhattan has many commuters and tourists who are not residents, so its rate per resident overstates the risk per person present.
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
#   * Adjusted for population, the Bronx has the highest felony rate (2,567 per 100,000 residents per year), about 1.5 times Brooklyn (1,748) 
#     and 2.2 times Staten Island (1,183). It also has the highest violation rate (1,255).
#     This is the 2020-2025 average: year by year, the Bronx is first only since 2023 (see robustness H1a).
#   * Manhattan is second in felonies (2,436) and first in misdemeanors (3,938), but its rate is overstated because many people in Manhattan during the day are not residents.
#   * Brooklyn, first in absolute counts (figure 2), drops to third once population is taken into account.
#   * Staten Island keeps the lowest felony rate, but its misdemeanor rate (2,329) is almost the same as Queens (2,383).
# Limitation: one population count (2020) is used for all six years.

# ---------------------------------------------------------------------
# Robustness H1a: does the borough ranking hold year by year?
# Same rate as H1a, but per report year instead of the 2020-2025 average.
# ---------------------------------------------------------------------
borough_rates_year <- nypd_ocorr |>
  filter(report_year %in% rate_years, !is.na(boro_nm),
         law_cat_cd %in% c("FELONY", "MISDEMEANOR", "VIOLATION")) |>
  count(report_year, boro_nm, law_cat_cd) |>
  inner_join(borough_pop, by = "boro_nm") |>
  mutate(rate = n / population * 1e5) |>
  group_by(report_year, law_cat_cd) |>
  mutate(rank = min_rank(desc(rate))) |>   # 1 = highest rate that year
  ungroup()

# Felony ranking per year, one column per year
borough_rates_year |>
  filter(law_cat_cd == "FELONY") |>
  select(boro_nm, report_year, rank) |>
  tidyr::pivot_wider(names_from = report_year, values_from = rank) |>
  arrange(`2025`) |>
  print()
# Observed:
#   * Brooklyn (3rd), Queens (4th) and Staten Island (5th) keep the same position in every year from 2020 to 2025.
#   * The Bronx currently has the highest felony rate, but only since 2023: from 2020 to 2022 Manhattan was first. 
#     The H1a figure, which averages 2020-2025, shows the Bronx first and hides this change.

# Felony rate per year, one line per borough
felony_rate_year <- borough_rates_year |>
  filter(law_cat_cd == "FELONY")

# End labels (2025), spread so they do not overlap
felony_ends <- felony_rate_year |>
  filter(report_year == max(rate_years)) |>
  arrange(rate) |>
  mutate(label_y = spread_labels(rate, gap = 80))

g_matilde_h1a_robust <- ggplot(felony_rate_year, aes(x = report_year, y = rate, color = boro_nm, group = boro_nm)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  geom_segment(data = felony_ends, aes(x = report_year, xend = report_year + 0.1, y = rate, yend = label_y),
               linewidth = 0.3, show.legend = FALSE) +
  geom_text(data = felony_ends, aes(y = label_y, label = boro_nm), x = max(rate_years) + 0.12,
            hjust = 0, size = 3, show.legend = FALSE) +
  scale_color_manual(values = borough_colors) +
  scale_x_continuous(breaks = rate_years, expand = expansion(mult = c(0.02, 0.25))) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  labs(
    title = "The Bronx Currently Has the Highest Felony Rate",
    subtitle = "Only since 2023: from 2020 to 2022 Manhattan was first. Felonies per 100,000 residents, by report year",
    x = "Report Year",
    y = "Felonies per 100,000 residents",
    caption = paste0(source_caption, "\nPopulation: U.S. Census Bureau, 2020 Census")
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")

print(g_matilde_h1a_robust)
ggsave("fig/02_exploration/02_matilde/h1a_felony_rate_by_year.png",
       plot = g_matilde_h1a_robust, width = 10, height = 6, dpi = 300)

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
# Robustness H1b: weekdays vs weekends
# weekday is an ordered factor from Monday to Sunday (data dictionary),
# so levels 6 and 7 are Saturday and Sunday, whatever the labels' language.
# Same filters as H1b (no 00:00 / 12:00, hour 12 set to NA).
# ---------------------------------------------------------------------
hourly_share_daytype <- nypd_ocorr |>
  filter(!is.na(hour), !is.na(wday),
         !cmplnt_fr_tm %in% c("00:00:00", "12:00:00"),
         law_cat_cd %in% c("FELONY", "MISDEMEANOR", "VIOLATION")) |>
  mutate(day_type = if_else(as.integer(wday) >= 6, "Weekend", "Weekday")) |>
  count(day_type, law_cat_cd, hour) |>
  group_by(day_type, law_cat_cd) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  mutate(pct = if_else(hour == 12, NA_real_, pct))

# Quick check: hours with the highest share, per day type and law category
hourly_share_daytype |>
  filter(!is.na(pct)) |>
  group_by(day_type, law_cat_cd) |>
  slice_max(pct, n = 3) |>
  print(n = Inf)
# Observed:
#   * The afternoon peak appears both on weekdays and at weekends.
#   * At weekends the peak happens slightly later, for all three law categories (e.g. felonies at 16:00-18:00 on weekdays and 18:00-20:00 at weekends).
#   * Violations peak slightly earlier than felonies and misdemeanors, on both weekdays and weekends.
#   * The share of complaints at the peak hours is higher on weekdays (about 6.2%-6.7%) than at weekends (about 5.5%-6.1%).
# Limitation: the table shows only the three top hours of each group.

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

# ---------------------------------------------------------------------
# Robustness H1c: 2021 as the base year instead of 2020 (COVID year)
# Same top offenses as H1c; index 2021 = 100.
# ---------------------------------------------------------------------
offense_index_2021 <- offense_year |>
  filter(offense %in% top_offenses, report_year >= 2021) |>
  group_by(offense) |>
  mutate(index = 100 * n / n[report_year == 2021]) |>
  ungroup()

# Change 2020->2025 vs 2021->2025, side by side
offense_year |>
  filter(offense %in% top_offenses, report_year %in% c(2020, 2021, 2025)) |>
  tidyr::pivot_wider(names_from = report_year, values_from = n, names_prefix = "y") |>
  mutate(change_from_2020 = round(100 * (y2025 / y2020 - 1), 1),
         change_from_2021 = round(100 * (y2025 / y2021 - 1), 1)) |>
  print()
# Observed:
#   * With 2021 as the base, the change up to 2025 is smaller for all six offenses, as expected, since 2020 was the COVID year
#     (probably the reason why counts rose from 2020 to 2021).
#   * Criminal mischief is the only offense that fell by 2025, with either base year (-22.9% from 2020, -19.5% from 2021).
#   * No offense changes direction (from a rise to a fall, or the reverse) when the base year changes from 2020 to 2021.

#---------------------------------------------------------------------
# H1d: do complaints concentrate in a few types of premises, and does this differ by law category?
# ---------------------------------------------------------------------
premises_level <- nypd_ocorr |>
  filter(!is.na(prem_typ_desc),
         law_cat_cd %in% c("FELONY", "MISDEMEANOR", "VIOLATION")) |>
  count(prem_typ_desc, law_cat_cd)

# How many complaints have no premises (to report as a dataset gap)
sum(is.na(nypd_ocorr$prem_typ_desc))

# Share of each premises within its law category (e.g. "STREET holds X% of all felonies")
premises_share_in_level <- premises_level |>
  group_by(law_cat_cd) |>
  mutate(pct_of_level = 100 * n / sum(n)) |>
  ungroup()

# Share of each law category within each premises
# (e.g. "X% of complaints in the subway are felonies")
premises_level_mix <- premises_level |>
  group_by(prem_typ_desc) |>
  mutate(total_premises = sum(n),
         pct_of_premises = 100 * n / total_premises) |>
  ungroup()

# Keep the premises with the most complaints (change 10 if needed)
top_premises <- premises_level |>
  group_by(prem_typ_desc) |>
  summarise(total = sum(n), .groups = "drop") |>
  slice_max(total, n = 10) |>
  pull(prem_typ_desc)

premises_share_top <- premises_share_in_level |> filter(prem_typ_desc %in% top_premises)
premises_mix_top   <- premises_level_mix     |> filter(prem_typ_desc %in% top_premises)

print(premises_share_top, n = Inf)
# Observed (H1d, where each law category happens):
#   * The street and apartment buildings (RESIDENCE - APT. HOUSE) hold a large part of all complaints.
#   * Felonies happen mostly in the street (36.5% of all felonies), and so do misdemeanors (29.8%).
#   * Violations happen mostly in apartment buildings (35.7% of all violations).

print(premises_mix_top, n = Inf)
# Observed (H1d, law category mix within each premises):
#   * The street (40.2% felonies) and commercial buildings (35.2%) have a larger share of felonies than the other premises, which range from 10% to 31%.
#   * Chain stores (79.3%), department stores (79.1%), drug stores (87.5%), the subway (63.0%), the street (51.3%), commercial buildings (51.2%) and OTHER
#     (51.5%) are mostly misdemeanors (more than 50% of their complaints).
#   * The street is in both groups: it has the largest share of felonies, but more than half of its complaints are still misdemeanors.
# In my view, premises with few complaints but a high share of felonies should have different, specialized patrols to deal with those cases.
# Limitation: only the 10 premises with the most complaints are shown, so smaller
# premises with a high share of felonies are not in this table.

