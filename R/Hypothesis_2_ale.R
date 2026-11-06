#==================================================================================================================
# Setup
#==================================================================================================================

library(dplyr)
library(ggplot2)

dv_blue <- "#2E75B6"
dv_orange <- "#E8792F"
dv_dark <- "#1B2A4A"

source <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"

nypd <- readRDS("data/nypd_clean.rds")


#==================================================================================================================
# Hypothesis 2: assaults on police and peace officers
#==================================================================================================================

# While checking the victim sex codes, I found M (male), F (female), U (unknown),
# D (business or organisation), E (People of the State of New York) and L, which is not
# in the data dictionary. L is mostly linked to assaults on officers, but it only marks
# these assaults from 2022, so the analysis uses the "ASSAULT POLICE/PEACE OFFICER"
# description from 2020 to 2025. 2026 is left out because it only runs until June.
#
# Hypothesis: recorded assaults on police and peace officers are concentrated in a small
# group of precincts, beyond what their total complaints would suggest, and this pattern
# persists over time.

police_fig_dir <- "fig/02_exploration/02_ale/02_police_victims"
dir.create(police_fig_dir, showWarnings = FALSE, recursive = TRUE)

# Minimum assaults for a precinct to appear in the ratio figure
min_assaults <- 50

# Mark the assaults and group the premises into three simple types
police <- nypd |>
  filter(report_year %in% 2020:2025) |>
  mutate(
    is_assault = pd_desc %in% "ASSAULT POLICE/PEACE OFFICER",
    place = case_when(
      prem_typ_desc %in% "STREET" ~ "Street",
      is.na(prem_typ_desc) | prem_typ_desc %in% "OTHER" ~ "Other or unknown",
      TRUE ~ "Other known place"
    )
  )

assaults <- police |>
  filter(is_assault)

# Number of assaults from 2020 to 2025
nrow(assaults)


# ---- Check 1. In which years does the L code mark these assaults? ----

assaults |>
  count(report_year, is_l = vic_sex %in% "L") |>
  group_by(report_year) |>
  mutate(pct = round(100 * n / sum(n))) |>
  ungroup() |>
  as_tibble() |>
  print(n = Inf)


# ---- Check 2. Do assaults change differently from all complaints? ----

# Index: 2020 = 100, so both lines can be compared on the same scale
yearly_totals <- police |>
  group_by(report_year) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  mutate(
    complaints_index = round(100 * complaints / first(complaints), 1),
    assaults_index = round(100 * assaults / first(assaults), 1)
  )

print(yearly_totals)

trend_plt <- ggplot(yearly_totals, aes(x = report_year)) +
  geom_line(aes(y = complaints_index, colour = "All complaints"), linewidth = 1.2) +
  geom_line(aes(y = assaults_index, colour = "Assaults on officers"), linewidth = 1.2) +
  scale_colour_manual(values = c("All complaints" = "grey60", "Assaults on officers" = dv_blue)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Assaults on Officers Compared with All Complaints",
    subtitle = "Index: 2020 = 100 (200 means twice the 2020 value)",
    x = "Report Year",
    y = "Index",
    colour = NULL,
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_trend.png"), plot = trend_plt, width = 9, height = 5, dpi = 300)


# ---- Check 3. Are assaults concentrated beyond complaints in general? ----

# A ratio above 1 means the area has more assaults than its total complaints would suggest
add_shares <- function(data) {
  data |>
    mutate(
      pct_assaults = round(100 * assaults / sum(assaults), 2),
      pct_all = round(100 * complaints / sum(complaints), 2),
      ratio = round(pct_assaults / pct_all, 2)
    ) |>
    arrange(desc(assaults))
}

boroughs <- police |>
  filter(!is.na(boro_nm)) |>
  group_by(boro_nm) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  add_shares()

print(boroughs)

precincts <- police |>
  filter(!is.na(addr_pct_cd)) |>
  group_by(addr_pct_cd) |>
  summarise(
    complaints = n(),
    assaults = sum(is_assault),
    correction = sum(is_assault & juris_desc %in% "DEPT OF CORRECTIONS")
  ) |>
  add_shares()

print(precincts, n = Inf)

# Share of assaults and share of all complaints in the top 10 precincts
top10_precincts <- head(precincts, 10)
sum(top10_precincts$pct_assaults)
sum(top10_precincts$pct_all)

# Precincts with the highest ratio, among those with enough assaults
# Precincts where most assaults fall under the Department of Correction are shown in grey,
# because they mostly happen in jails and not on NYPD patrol
top_ratio <- precincts |>
  filter(assaults >= min_assaults) |>
  slice_max(ratio, n = 15) |>
  mutate(
    pct_correction = round(100 * correction / assaults),
    mostly_correction = pct_correction > 50
  )

grey_precincts <- filter(top_ratio, mostly_correction)
grey_note <- paste0("Precinct ", grey_precincts$addr_pct_cd, " (", grey_precincts$pct_correction, "%)", collapse = ", ")

ratio_plt <- ggplot(top_ratio, aes(x = ratio, y = reorder(addr_pct_cd, ratio), fill = mostly_correction)) +
  geom_col() +
  geom_vline(xintercept = 1, colour = dv_orange, linewidth = 1, linetype = "dashed") +
  geom_text(aes(label = paste(assaults, "assaults")), hjust = -0.1, size = 3.2) +
  scale_fill_manual(values = c("FALSE" = dv_blue, "TRUE" = "grey60"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Precincts Where Assaults on Officers Are Most Over-Represented",
    subtitle = paste0("Ratio = share of assaults / share of all complaints. Dashed line = 1. At least ",
                      min_assaults, " assaults.\nGrey = most assaults fall under the Department of Correction: ", grey_note),
    x = "Ratio",
    y = "Precinct",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_ratio.png"), plot = ratio_plt, width = 10, height = 6, dpi = 300)


# ---- Check 4. Do the same precincts stay on top each year? ----

yearly <- assaults |>
  filter(!is.na(addr_pct_cd)) |>
  count(report_year, addr_pct_cd)

yearly |>
  group_by(report_year) |>
  slice_max(n, n = 10) |>
  ungroup() |>
  count(addr_pct_cd, name = "years_in_top10", sort = TRUE) |>
  print(n = Inf)

yearly_plt <- yearly |>
  filter(addr_pct_cd %in% top10_precincts$addr_pct_cd) |>
  ggplot(aes(x = factor(report_year), y = reorder(addr_pct_cd, n, FUN = sum), fill = n)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = n, colour = n > max(n) / 2), size = 3.2) +
  scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white"), guide = "none") +
  scale_fill_gradient(low = "#DCE6F0", high = dv_dark) +
  labs(
    title = "Assaults on Officers per Year in the Top 10 Precincts",
    subtitle = "Precincts sorted by total assaults from 2020 to 2025",
    x = "Report Year",
    y = "Precinct",
    fill = "Assaults",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_by_year.png"), plot = yearly_plt, width = 10, height = 6, dpi = 300)


# ---- Check 5. Where do the assaults happen in the precincts that stand out? ----

count(assaults, place, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))
count(assaults, juris_desc, sort = TRUE) |> head(10)

# % of assaults outside NYPD jurisdiction
round(100 * mean(!assaults$juris_desc %in% "N.Y. POLICE DEPT"), 1)

# Exact premises in the same precincts
assaults |>
  filter(addr_pct_cd %in% top_ratio$addr_pct_cd) |>
  count(addr_pct_cd, prem_typ_desc, sort = TRUE) |>
  group_by(addr_pct_cd) |>
  slice_head(n = 3) |>
  print(n = Inf)

# All premises across the city, most common first
assaults |>
  mutate(premise = ifelse(is.na(prem_typ_desc), "MISSING", prem_typ_desc)) |>
  count(premise, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  as_tibble() |>
  print(n = Inf)

# All premises in each top ratio precinct
premises_by_precinct <- assaults |>
  filter(addr_pct_cd %in% top_ratio$addr_pct_cd) |>
  mutate(premise = ifelse(is.na(prem_typ_desc), "MISSING", prem_typ_desc)) |>
  count(addr_pct_cd, premise, sort = TRUE) |>
  group_by(addr_pct_cd) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  ungroup() |>
  arrange(addr_pct_cd, desc(n)) |>
  as_tibble()

print(premises_by_precinct, n = Inf)

# Figure: place of assaults, with the percentage written inside each section
place_data <- assaults |>
  filter(addr_pct_cd %in% top_ratio$addr_pct_cd) |>
  count(addr_pct_cd, place) |>
  group_by(addr_pct_cd) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup()

place_plt <- ggplot(place_data, aes(x = pct, y = factor(addr_pct_cd, levels = rev(top_ratio$addr_pct_cd)), fill = place)) +
  geom_col() +
  geom_text(aes(label = ifelse(pct >= 5, paste0(round(pct), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.2) +
  scale_fill_manual(values = c("Street" = "#9DC3E6", "Other known place" = "#F4B183", "Other or unknown" = "grey85")) +
  labs(
    title = "Where Assaults on Officers Happen in the Precincts with the Highest Ratios",
    subtitle = paste0(nrow(top_ratio), " precincts with the highest ratio, among those with at least ",
                      min_assaults, " assaults. Each row adds up to 100%"),
    x = "% of the Precinct's Assaults",
    y = "Precinct",
    fill = "Place",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_place.png"), plot = place_plt, width = 10, height = 6, dpi = 300)

# Precincts that stand out: a ratio of at least 1.5, with enough assaults
standout_precincts <- precincts |>
  filter(assaults >= min_assaults, ratio >= 1.5) |>
  arrange(desc(ratio)) |>
  mutate(panel = paste0("Precinct ", addr_pct_cd, " (ratio ", ratio, ")"))

# The 3 most common premises in each of those precincts
top3_premises <- assaults |>
  filter(addr_pct_cd %in% standout_precincts$addr_pct_cd) |>
  mutate(premise = ifelse(is.na(prem_typ_desc), "MISSING", prem_typ_desc)) |>
  count(addr_pct_cd, premise) |>
  group_by(addr_pct_cd) |>
  mutate(pct = 100 * n / sum(n)) |>
  slice_max(n, n = 3, with_ties = FALSE) |>
  ungroup() |>
  left_join(select(standout_precincts, addr_pct_cd, panel), by = "addr_pct_cd") |>
  mutate(
    panel = factor(panel, levels = standout_precincts$panel),
    bar = paste(addr_pct_cd, premise)
  )

print(top3_premises, n = Inf)

# Figure: each bar gets its own name (precinct and premise) so it is sorted inside its panel,
# and the axis only shows the premise
premises_plt <- ggplot(top3_premises, aes(x = pct, y = reorder(bar, pct))) +
  geom_col(fill = dv_blue) +
  geom_text(aes(label = paste0(round(pct), "% (", n, ")")), hjust = -0.1, size = 3) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_y_discrete(labels = setNames(top3_premises$premise, top3_premises$bar)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(
    title = "Top 3 Premises in the Precincts Where Assaults on Officers Stand Out",
    subtitle = "Precincts with a ratio of at least 1.5. % of the precinct's assaults, with the number of assaults in brackets",
    x = "% of the Precinct's Assaults",
    y = NULL,
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_top3_premises.png"), plot = premises_plt, width = 10, height = 8, dpi = 300)


# ---- Check 6. Do assaults happen at different hours from complaints in general? ----

# Exact 00:00 and 12:00 are removed because they may stand for an unknown time
timed <- police |>
  filter(!old_occurrence, !is.na(hour), !is.na(wday),
         !cmplnt_fr_tm %in% c("00:00:00", "12:00:00"))

hourly <- timed |>
  group_by(hour) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  mutate(
    pct_assaults = round(100 * assaults / sum(assaults), 2),
    pct_all = round(100 * complaints / sum(complaints), 2)
  )

print(hourly, n = Inf)

# % of assaults and of all complaints between 7 pm and 5 am
timed |>
  mutate(night = hour >= 19 | hour < 5) |>
  summarise(
    pct_assaults_night = round(100 * sum(is_assault & night) / sum(is_assault), 1),
    pct_all_night = round(100 * mean(night), 1)
  )

hourly_plt <- ggplot(hourly, aes(x = hour)) +
  geom_line(aes(y = pct_all, colour = "All complaints"), linewidth = 1.2) +
  geom_line(aes(y = pct_assaults, colour = "Assaults on officers"), linewidth = 1.2) +
  scale_colour_manual(values = c("All complaints" = "grey60", "Assaults on officers" = dv_blue)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Hour of Assaults on Officers Compared with All Complaints",
    subtitle = "Complaints recorded at exactly 00:00 or 12:00 are excluded, which lowers hours 0 and 12. That dip is not a real pattern",
    x = "Hour of Occurrence",
    y = "% of Complaints in Each Group",
    colour = NULL,
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_by_hour.png"), plot = hourly_plt, width = 10, height = 5, dpi = 300)

# Day names from the date itself (1 = Monday), so they are in English on any computer
time_plt <- timed |>
  filter(is_assault) |>
  mutate(wday = factor(format(occurred_on, "%u"), levels = 1:7,
                       labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) |>
  count(wday, hour) |>
  ggplot(aes(x = hour, y = wday, fill = n)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "#DCE6F0", high = dv_dark) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Assaults on Officers by Hour and Day of the Week",
    subtitle = "Complaints recorded at exactly 00:00 or 12:00 are excluded, which lowers hours 0 and 12. That dip is not a real pattern",
    x = "Hour of Occurrence",
    y = "Day of the Week",
    fill = "Assaults",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_hour_weekday.png"), plot = time_plt, width = 10, height = 5, dpi = 300)


# ---- Check 7. Which boroughs stand out? ----

borough_plt <- ggplot(boroughs, aes(x = ratio, y = reorder(boro_nm, ratio))) +
  geom_col(fill = dv_blue) +
  geom_vline(xintercept = 1, colour = dv_orange, linewidth = 1, linetype = "dashed") +
  geom_text(aes(label = paste(assaults, "assaults")), hjust = -0.1, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Assaults on Officers Compared with All Complaints, by Borough",
    subtitle = "Ratio = share of assaults / share of all complaints. Dashed line = 1, the same share of both",
    x = "Ratio",
    y = "Borough",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_ratio_borough.png"), plot = borough_plt, width = 9, height = 5, dpi = 300)


# ---- Check 8. Do holidays stand out? ----

# Dates that are the same every year. Holidays that move, such as Thanksgiving, are not included
holidays <- tibble(
  day = c("01-01", "07-04", "10-31", "12-24", "12-25", "12-31"),
  holiday = c("New Year's Day", "Independence Day", "Halloween", "Christmas Eve", "Christmas Day", "New Year's Eve")
)

# Complaints that occurred from 2020 to 2025, whatever the report year, so that late December 2025
# reported in 2026 is not left out
occurred <- nypd |>
  filter(occurred_on >= as.Date("2020-01-01"), occurred_on <= as.Date("2025-12-31")) |>
  mutate(is_assault = pd_desc %in% "ASSAULT POLICE/PEACE OFFICER")

# Unknown dates may be recorded as the 1st of the month, like unknown hours as 00:00.
# If day 01 is much higher than the others for all complaints, January 1 is not a fair holiday.
# Days 29 to 31 are lower because some months do not have them
occurred |>
  group_by(day_of_month = format(occurred_on, "%d")) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  mutate(
    complaints_vs_avg = round(complaints / mean(complaints), 2),
    assaults_vs_avg = round(assaults / mean(assaults), 2)
  ) |>
  print(n = Inf)

# Totals for each calendar day. 1.5 means 50% more than an average calendar day
calendar_days <- occurred |>
  mutate(day = format(occurred_on, "%m-%d")) |>
  group_by(day) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  mutate(
    assaults_vs_avg = round(assaults / mean(assaults), 2),
    complaints_vs_avg = round(complaints / mean(complaints), 2)
  )

# The holidays
calendar_days |>
  inner_join(holidays, by = "day") |>
  print(n = Inf)

# The 10 calendar days with the most assaults
calendar_days |>
  slice_max(assaults, n = 10)

# Figure: holidays compared with an average calendar day
holiday_plt <- calendar_days |>
  inner_join(holidays, by = "day") |>
  tidyr::pivot_longer(c(assaults_vs_avg, complaints_vs_avg), names_to = "group", values_to = "value") |>
  ggplot(aes(x = value, y = reorder(holiday, value), fill = group)) +
  geom_col(position = "dodge") +
  geom_vline(xintercept = 1, colour = dv_orange, linewidth = 1, linetype = "dashed") +
  scale_fill_manual(values = c("assaults_vs_avg" = dv_blue, "complaints_vs_avg" = "grey70"),
                    labels = c("Assaults on officers", "All complaints")) +
  labs(
    title = "Holidays Compared with an Average Calendar Day",
    subtitle = "1 = the same as an average day (dashed line). 2 = twice as many",
    x = "Compared with an Average Calendar Day",
    y = "Holiday",
    fill = NULL,
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_holidays.png"), plot = holiday_plt, width = 10, height = 5, dpi = 300)


# ---- Check 9. Is the Independence Day peak a repeated pattern? ----

july4 <- occurred |>
  filter(is_assault, format(occurred_on, "%m-%d") == "07-04")

count(july4, year = format(occurred_on, "%Y"))
count(july4, addr_pct_cd, sort = TRUE) |> head(10)
count(july4, hour, sort = TRUE) |> head(5)

# Figure: Independence Day assaults by hour, all years added together
july4_plt <- july4 |>
  count(hour) |>
  ggplot(aes(x = hour, y = n)) +
  geom_col(width = 0.9, fill = dv_blue) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.2) +
  scale_x_continuous(breaks = 0:23, limits = c(-0.5, 23.5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Assaults on Officers on Independence Day, by Hour",
    subtitle = paste0(nrow(july4), " assaults in total: every July 4 from 2020 to 2025 added together"),
    x = "Hour of Occurrence",
    y = "Number of Assaults",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_july4_hour.png"), plot = july4_plt, width = 10, height = 5, dpi = 300)


# ---- Check 10. Which agencies are involved in the 3 precincts with the highest ratios? ----

assaults |>
  filter(addr_pct_cd %in% head(top_ratio$addr_pct_cd, 3)) |>
  count(addr_pct_cd, juris_desc, sort = TRUE) |>
  group_by(addr_pct_cd) |>
  slice_head(n = 3) |>
  print(n = Inf)


# ---- Check 11. Does the concentration remain under NYPD jurisdiction only? ----

nypd_precincts <- police |>
  filter(juris_desc %in% "N.Y. POLICE DEPT", !is.na(addr_pct_cd)) |>
  group_by(addr_pct_cd) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  add_shares()

# Share of assaults and of all complaints in the top 10 NYPD precincts
sum(head(nypd_precincts, 10)$pct_assaults)
sum(head(nypd_precincts, 10)$pct_all)

# Highest ratios, among precincts with enough assaults
nypd_precincts |>
  filter(assaults >= min_assaults) |>
  slice_max(ratio, n = 10)