# Hypothesis 4: The rise of complaints in the subway

library(dplyr)
library(ggplot2)

# Colors
dv_blue <- "#2E75B6"
dv_orange <- "#E8792F"
dv_dark <- "#1B2A4A"

source <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"

nypd <- readRDS("data/nypd_clean.rds")

title_case <- function(x) gsub("\\b([a-z])", "\\U\\1", tolower(x), perl = TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# The exploration that led to this hypothesis is in R/exploration_bea.R 

# Hypothesis: More than half of the rise of complaints in the NYC subway since 2020 comes from two offence classes:
# theft of services (fare evasion) and N.Y.C. Transit Auth. R&R (transit rules), and not from felonies.
# A ranking of stations by total complaints therefore leaves out stations where felonies are high.

# Decision it supports: where and when to place patrols against violent crime, using felonies and not total complaints.

# Things to keep in mind:
# - Fare evasion is only recorded when an officer sees it, so a rise can mean more evasion or more police checks. The data cannot tell the two apart.
# - Subway ridership collapsed in 2020 (COVID). The dataset has no ridership, so every growth from 2020 is also partly a return of passengers. 
# - 2026 is left out because it only runs until June.

# --------------------------------------------------------------------------------------------------------------------------- #

fig_dir <- "fig/02_exploration/hypothesis_4/hypothesis"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

years <- 2020:2025


# The two offence classes of the hypothesis (no victim reports them, they are recorded by the Transit Police)
fare_rule_pd <- c("THEFT OF SERVICES, UNCLASSIFIE", # fare evasion
                  "N.Y.C. TRANSIT AUTH. R&R")        # transit rules and regulations

subway <- nypd |>
  filter(report_year %in% years, prem_typ_desc %in% "TRANSIT - NYC SUBWAY") |>
  mutate(group = case_when(
    pd_desc %in% fare_rule_pd ~ "Fare evasion and transit rules",
    law_cat_cd %in% "FELONY"  ~ "Felonies",
    TRUE                      ~ "Other misdemeanors and violations"
  ))

# Check: these offences are recorded by the Transit Police and are not felonies
subway |> filter(pd_desc %in% fare_rule_pd) |> count(pd_desc, juris_desc, law_cat_cd, sort = TRUE)

# --------------------------------------------------------------------------------------------------------------------------- #
# Part 1. What is growing in the subway?
# --------------------------------------------------------------------------------------------------------------------------- #

by_group <- subway |>
  count(report_year, group)

print(tidyr::pivot_wider(by_group, names_from = group, values_from = n))

# Share of the 2020-2025 rise that comes from fare evasion and transit rules (hypothesis: more than 50%)
totals <- by_group |>
  group_by(report_year) |>
  summarise(total = sum(n), fare_rule = sum(n[group == "Fare evasion and transit rules"]), .groups = "drop")

rise_total <- totals$total[totals$report_year == 2025] - totals$total[totals$report_year == 2020]
rise_fare_rule <- totals$fare_rule[totals$report_year == 2025] - totals$fare_rule[totals$report_year == 2020]
pct_rise_fare_rule <- round(100 * rise_fare_rule / rise_total)

print(totals)
cat("Share of the rise that comes from fare evasion and transit rules:", pct_rise_fare_rule, "%\n")

group_colours <- c("Fare evasion and transit rules"    = dv_orange,
                   "Felonies"                          = dv_dark,
                   "Other misdemeanors and violations" = "grey60")

ends <- by_group |> filter(report_year == max(years))

group_plt <- ggplot(by_group, aes(x = report_year, y = n, colour = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text(data = ends, aes(label = group), hjust = 0, nudge_x = 0.12, fontface = "bold", size = 3.6) +
  scale_colour_manual(values = group_colours) +
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.45))) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  labs(
    title = paste0(pct_rise_fare_rule, "% of the rise in subway complaints since 2020 comes from fare evasion and transit-rule offences"),
    subtitle = "Complaints in the NYC subway per report year",
    x = "Report Year",
    y = "Complaints",
    caption = source
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title.position = "plot", panel.grid.minor = element_blank(), plot.subtitle = element_text(size = 10, colour = "grey35"))

ggsave(file.path(fig_dir, "h4_test_1_subway_by_group.png"), plot = group_plt, width = 10, height = 5.5, dpi = 300, bg = "white")



# --------------------------------------------------------------------------------------------------------------------------- #
# Part 2. Did serious crime in the subway grow?
# --------------------------------------------------------------------------------------------------------------------------- #

# The four most common felony offences in the subway
n_top_felonies <- 4

top_felonies <- subway |>
  filter(group == "Felonies", !is.na(ofns_desc)) |>
  count(ofns_desc, sort = TRUE) |>
  slice_head(n = n_top_felonies) |>
  pull(ofns_desc)

# Index: 2020 = 100, as in hypothesis nº2
felony_index <- subway |>
  filter(ofns_desc %in% top_felonies, group == "Felonies") |>
  mutate(ofns_desc = sub(" & Related Of$", "", title_case(ofns_desc))) |>
  count(ofns_desc, report_year) |>
  group_by(ofns_desc) |>
  arrange(report_year, .by_group = TRUE) |>
  mutate(index = 100 * n / first(n)) |>
  ungroup()

# Fare evasion and transit rules are left out of the index: they start from almost zero in 2020, so their index would be in the tens of thousands and flatten every other line.
# Their counts are quoted in the subtitle instead
fare_rule_2020 <- totals$fare_rule[totals$report_year == 2020]
fare_rule_2025 <- totals$fare_rule[totals$report_year == 2025]

print(felony_index, n = Inf)

felony_ends <- felony_index |> filter(report_year == max(years))

felony_plt <- ggplot(felony_index, aes(x = report_year, y = index, colour = ofns_desc)) +
  geom_hline(yintercept = 100, colour = "grey50", linetype = "dashed") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  geom_text(data = felony_ends, aes(label = ofns_desc), hjust = 0, nudge_x = 0.12, fontface = "bold", size = 3.4) +
  scale_colour_manual(values = c("#56B4E9", "#CC79A7", "#009E73", "#000000")) +
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.02, 0.5))) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Serious crime in the subway changed far less than the total",
    subtitle = paste0("Index of the ", n_top_felonies, " most common felonies in the subway, 2020 = 100.\n",
                      "For comparison, fare evasion and transit rules went from ", scales::comma(fare_rule_2020),
                      " to ", scales::comma(fare_rule_2025), " complaints"),
    x = "Report Year",
    y = "Index (2020 = 100)",
    caption = source
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title.position = "plot", panel.grid.minor = element_blank(), plot.subtitle = element_text(size = 10, colour = "grey35"))


ggsave(file.path(fig_dir, "h4_test_2_felonies_index.png"), plot = felony_plt, width = 10, height = 5.5, dpi = 300, bg = "white")


# --------------------------------------------------------------------------------------------------------------------------- #
# Part 3. Does the station ranking change when only felonies are counted?
# --------------------------------------------------------------------------------------------------------------------------- #

# Only 2024 and 2025: the years when fare evasion and transit rules are large
recent_years <- 2024:2025
n_top_stations <- 15

stations <- subway |>
  filter(report_year %in% recent_years, !is.na(station_name)) |>
  group_by(station_name) |>
  summarise(
    total = n(),
    fare_rule = sum(group == "Fare evasion and transit rules"),
    felonies = sum(group == "Felonies"),
    .groups = "drop"
  ) |>
  mutate(
    pct_fare_rule = round(100 * fare_rule / total),
    rank_total = rank(-total, ties.method = "first"),
    rank_felonies = rank(-felonies, ties.method = "first")
  )

# Stations in the top of at least one of the two rankings
top_stations <- stations |>
  filter(rank_total <= n_top_stations | rank_felonies <= n_top_stations) |>
  arrange(rank_felonies)

print(top_stations, n = Inf, width = Inf)

n_overlap <- sum(top_stations$rank_total <= n_top_stations & top_stations$rank_felonies <= n_top_stations)
cat("Stations in both top", n_top_stations, ":", n_overlap, "\n")

# Compare the 2024 and 2025 rankings to see if the felony stations, and the ones left out, are the same in both years
by_year <- subway |>
  filter(report_year %in% recent_years, !is.na(station_name)) |>
  group_by(report_year, station_name) |>
  summarise(total = n(), felonies = sum(group == "Felonies"), .groups = "drop") |>
  group_by(report_year) |>
  mutate(rank_total = rank(-total, ties.method = "first"),
         rank_felonies = rank(-felonies, ties.method = "first")) |>
  ungroup()

missed_by_year <- by_year |>
  filter(rank_felonies <= n_top_stations, rank_total > n_top_stations) |>
  arrange(report_year, rank_felonies)

print(missed_by_year, n = Inf)
print(count(missed_by_year, report_year))

top_2024 <- by_year$station_name[by_year$report_year == 2024 & by_year$rank_felonies <= n_top_stations]
top_2025 <- by_year$station_name[by_year$report_year == 2025 & by_year$rank_felonies <= n_top_stations]
cat("Felony top", n_top_stations, "stations that are the same in 2024 and 2025:", length(intersect(top_2024, top_2025)), "\n")
cat("Missed stations that are the same in 2024 and 2025:", length(intersect(missed_by_year$station_name[missed_by_year$report_year == 2024],
                                                                            missed_by_year$station_name[missed_by_year$report_year == 2025])), "\n")

# Focus on the stations with the most felonies, as these are the most relevant for planning patrols against serious crime. 
# Stations in blue would be left out of the top by a ranking of all complaints
felony_top <- stations |>
  filter(rank_felonies <= n_top_stations) |>
  mutate(
    missed = rank_total > n_top_stations,
    station = title_case(station_name),
    label = ifelse(missed,
                   paste0(felonies, "  (#", rank_total, " by all complaints)"),
                   as.character(felonies))
  )

n_missed <- sum(felony_top$missed)

ranking_plt <- ggplot(felony_top, aes(x = felonies, y = reorder(station, felonies), fill = missed)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label, colour = missed), hjust = 0, nudge_x = 2, size = 3.6) +
  scale_fill_manual(values = c(`FALSE` = "grey75", `TRUE` = dv_blue), guide = "none") +
  scale_colour_manual(values = c(`FALSE` = "grey30", `TRUE` = dv_blue), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
  labs(
    title = paste0("Ranking stations by all complaints misses ", n_missed, " of the ", n_top_stations, " subway stations with the most felonies"),
    subtitle = paste0("The ", n_top_stations, " stations with the most felonies, ", min(recent_years), "-", max(recent_years), 
                      ".\nBlue = not in the top ", n_top_stations, " when all complaints are counted (its rank there in brackets)"),
    x = paste0("Felonies, ", min(recent_years), "-", max(recent_years)),
    y = NULL,
    caption = source
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), plot.title.position = "plot", plot.subtitle = element_text(size = 10, colour = "grey35"))

ggsave(file.path(fig_dir, "h4_test_3_station_ranking.png"), plot = ranking_plt, width = 11, height = 6.5, dpi = 300, bg = "white")



# --------------------------------------------------------------------------------------------------------------------------- #
# Part 4. When do fare evasion / transit rules and felonies happen?
# --------------------------------------------------------------------------------------------------------------------------- #

# Exact 00:00 and 12:00 are removed because they may stand for an unknown time (as in hypothesis nº2)
timed <- subway |>
  filter(!old_occurrence, !is.na(hour), !is.na(wday),
         !cmplnt_fr_tm %in% c("00:00:00", "12:00:00"),
         group %in% c("Fare evasion and transit rules", "Felonies")) |>
  count(group, wday, hour) |>
  group_by(group) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  # The wday labels in the file are in Portuguese (seg, ter, ...). The factor runs Monday to Sunday,
  # so the English names are given in the same order, then reversed to put Monday on top
  mutate(wday = factor(as.integer(wday), levels = 7:1, labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))))

# Numbers quoted in the title: share of each group at 14:00-17:59, at 00:00-05:59 and the weekend
time_shares <- timed |>
  group_by(group) |>
  summarise(afternoon = round(sum(pct[hour %in% 14:17])),
            night = round(sum(pct[hour %in% 0:5])),
            weekend = round(sum(pct[wday %in% c("Sat", "Sun")])),
            .groups = "drop")

print(time_shares)


fare_rule_shares <- time_shares[time_shares$group == "Fare evasion and transit rules", ]
fel_shares <- time_shares[time_shares$group == "Felonies", ]

time_plt <- ggplot(timed, aes(x = hour, y = wday, fill = pct)) +
  geom_tile(colour = "white") +
  facet_wrap(~group, ncol = 1) +
  scale_fill_viridis_c(name = "% of the\ngroup", labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = 0:23, expand = c(0,0)) +
  labs(
    title = "Fare evasion and transit rules peak on weekday afternoons, felonies spread into nights and weekends",
    subtitle = "Share of each group by hour and day of occurrence (exact 00:00 and 12:00 excluded)",
    x = "Hour of Occurrence",
    y = NULL,
    caption = source
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), plot.title.position = "plot", plot.subtitle = element_text(size = 10, colour = "grey35"))


ggsave(file.path(fig_dir, "h4_test_4_hour_weekday.png"), plot = time_plt, width = 10, height = 7, dpi = 300, bg = "white")



# --------------------------------------------------------------------------------------------------------------------------- #
# Part 5. When did fare evasion and transit rules change? Month by month
# --------------------------------------------------------------------------------------------------------------------------- #

# Months from 2022, when these offences were still almost zero. Report month (date the complaint was reported)
monthly <- subway |>
  filter(report_year >= 2022, group %in% c("Fare evasion and transit rules", "Felonies")) |>
  mutate(month = as.Date(format(reported_on, "%Y-%m-01"))) |>
  count(month, group)

# The month with the largest jump in fare evasion and transit rules compared with the month before 
# The jump is measured in complaints, not in %, so that a jump from 7 to 33 in 2022 does not count more than one from 111 to 521.
jumps <- monthly |>
  filter(group == "Fare evasion and transit rules") |>
  arrange(month) |>
  mutate(before = lag(n), rise = n - before) |>
  slice_max(rise, n = 1)
print(jumps)

# Range of felonies per month, quoted in the title
felony_range <- range(monthly$n[monthly$group == "Felonies"])

monthly_plt <- ggplot(monthly, aes(x = month, y = n, colour = group)) +
  geom_vline(xintercept = jumps$month, colour = "grey60", linetype = "dashed") +
  geom_line(linewidth = 1.1) +
  annotate("text", x = jumps$month, y = max(monthly$n), hjust = 1.05, vjust = 1, size = 3.4, colour = "grey30",
           label = paste0(format(jumps$month, "%B %Y"), ":\n", jumps$before, " to ", jumps$n, " in one month")) +
  geom_text(data = filter(monthly, month == max(month)), aes(label = group), hjust = 0, nudge_x = 20, fontface = "bold", size = 3.6) +
  scale_colour_manual(values = c("Fare evasion and transit rules" = dv_orange, "Felonies" = dv_dark)) +
  scale_x_date(breaks = as.Date(paste0(2022:2025, "-01-01")), date_labels = "%Y", expand = expansion(mult = c(0.02, 0.32))) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  labs(
    title = paste0("Fare evasion and transit rules jumped in ", format(jumps$month, "%B %Y"), ", felonies stayed at ", felony_range[1], "-", felony_range[2], " a month"),
    subtitle = "Subway complaints per month of report, 2022-2025",
    x = NULL,
    y = "Complaints per month",
    caption = source
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title.position = "plot", panel.grid.minor = element_blank(), plot.subtitle = element_text(size = 10, colour = "grey35"))

ggsave(file.path(fig_dir, "h4_test_5_monthly.png"), plot = monthly_plt, width = 10, height = 5.5, dpi = 300, bg = "white")



# --------------------------------------------------------------------------------------------------------------------------- #
# Part 6. Where do fare evasion and transit rules happen, compared with felonies? By borough
# --------------------------------------------------------------------------------------------------------------------------- #

# One number per borough: fare evasion and transit-rule complaints for each felony in its subway, 2024-2025
# If these offences were similar across boroughs relative to serious crime, the values would be close to the city average (dashed line)
# Staten Island is not included because it has no NYC subway stations
borough_ratio <- subway |>
  filter(report_year %in% recent_years, !is.na(boro_nm)) |>
  group_by(boro_nm) |>
  summarise(fare_rule = sum(group == "Fare evasion and transit rules"),
            felonies = sum(group == "Felonies"), .groups = "drop") |>
  mutate(ratio = fare_rule / felonies,
         borough = title_case(boro_nm))

city_ratio <- sum(borough_ratio$fare_rule) / sum(borough_ratio$felonies)
print(borough_ratio)

cat("City:", round(city_ratio, 1), "fare evasion and transit-rule complaints per felony\n")

top_b <- slice_max(borough_ratio, ratio, n = 1)
low_b <- slice_min(borough_ratio, ratio, n = 1)

borough_plt <- ggplot(borough_ratio, aes(x = ratio, y = reorder(borough, ratio))) +
  geom_col(fill = dv_orange, width = 0.6) +
  geom_vline(xintercept = city_ratio, colour = "grey30", linetype = "dashed") +
  annotate("text", x = city_ratio, y = Inf, hjust = -0.08, vjust = 1.5, size = 3.6, colour = "grey30",
           label = paste0("City: ", format(round(city_ratio, 1), nsmall = 1))) +
  geom_text(aes(label = format(round(ratio, 1), nsmall = 1)), hjust = -0.3, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.9))) +
  labs(
    title = paste0("Fare evasion and transit-rule complaints per felony in the subway: ",
                   top_b$borough, " ", format(round(top_b$ratio, 1), nsmall = 1), ", ",
                   low_b$borough, " ", format(round(low_b$ratio, 1), nsmall = 1)),
    subtitle = paste0("Fare evasion and transit-rule complaints divided by felonies, subway, ", min(recent_years), "-", max(recent_years), ". Staten Island has no subway stations"),
    x = "Fare evasion and transit-rule complaints per felony",
    y = NULL,
    caption = source
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), plot.subtitle = element_text(size = 10, colour = "grey35"))


ggsave(file.path(fig_dir, "h4_test_6_borough.png"), plot = borough_plt, width = 10, height = 4.5, dpi = 300, bg = "white")
