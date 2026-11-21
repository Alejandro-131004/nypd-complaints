#install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

dv_blue <- "#2E75B6"
orange <- "orange2"
dark_orange <- "darkorange3"
gold <- "gold"
light_blue <- "lightblue"
dark_blue <- "darkblue"
nice_green <- "green4"
grey <- "grey85"
white <- "white"
pink <- "violet"
black <- "black"
red <- "firebrick"

source <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"
source_cal <- paste0(source, "\nSchool calendar: NYC Public Schools, 2019-20 to 2025-26 (schools.nyc.gov/calendar)")

# Loading dataset and removing old occurrences
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- subset(nypd, report_year >= 2020 & report_year <= 2025)
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)

dir.create("fig/02_exploration/02_rafa/hypothesis_revisited", showWarnings = FALSE, recursive = TRUE)

# --------------------------------------------------------------- #
# ----------------------- Disclaimers ----------------------- #
# --------------------------------------------------------------- #

# If year interval is not specified, consider 2020-2025
# Public transport only. The residence part of the old hypothesis is a
# documented discard (02_rafa_hypothesis.R, Figure 8)
# Every percentage comes with its counts: n_minor (under-18 victims) and
# n_all (all victims with a valid age group)
# "Commute" is an interpretation, not a variable: it stays out of the
# hypothesis and only shows up in the discussion

# --------------------------------------------------------------- #
# --------------------- School calendar --------------------- #
# --------------------------------------------------------------- #

# docs/nyc_school_calendar.csv, built by hand from the NYC Public
# Schools calendar of each school year (source URL in every row)
# Each day of 2020-2025 falls into one group:
#   Classes:        regular weekday with classes
#   Holiday/Recess: weekday without classes during the school year
#                   (holidays, recesses, Chancellor's Conference Days, remote days)
#   Summer Break:   weekday between the last and the first day of school
#   COVID:          weekday between 16 Mar 2020 and 12 Sep 2021 (closed, then hybrid)
#   Weekend:        Saturday or Sunday outside COVID
#   Left out:       COVID weekends and partial days (only some grades off)
# COVID dates: Chalkbeat (2021), CBS New York (2020), AP (2021), links in
# h3_hypothesis_revisited.md

# --------------------------------------------------------------- #
# ----------------------- Hypothesis ------------------------ #
# --------------------------------------------------------------- #

# Before (checkpoint 2):
# Victims under 18 (underage/minor) are concentrated in times of
# home-school-home commute, as well as unsupervised after-school time.

# Now (H3, professor's wording):
# On school weekdays, the share of under-18 victims on public transport
# rises at 7h and 15h.

# Extension, tested on its own: the rise disappears on days without classes,
# whatever the reason (Holiday/Recess, Summer Break, COVID, Weekend)

# --------------------------------------------------------------- #
# ------------------ Refutation criterion ------------------- #
# --------------------------------------------------------------- #

# ratio = share of under-18 victims at 7h and 15h on regular weekdays with classes /
#         same share on weekdays without classes (Holiday/Recess + Summer Break)
# If school made no difference, the ratio would be about 1.
# H3 is refuted if the ratio is not above 1.5 in at least 5 of the 6 school years.
# Thresholds taken unchanged from the professor's H2 example, applied after
# the first run (stated in the article), so they weren't tuned to these results.

peak_hours      <- c(7, 15)
criterion_ratio <- 1.5
criterion_years <- 5

# --------------------------------------------------------------- #
# -------------------------- Plots -------------------------- #
# --------------------------------------------------------------- #

# 0. Public transport victims with a valid age, and the school calendar

valid_ages <- c("<18", "18-24", "25-44", "45-64", "65+")

young_tr <- nypd |>
  filter(grepl("TRANSIT|^BUS ", prem_typ_desc), !is.na(hour), !is.na(occurred_on),
         vic_age_group %in% valid_ages)

young_tr$minor       <- young_tr$vic_age_group == "<18"
young_tr$occurred_on <- as.Date(young_tr$occurred_on)
young_tr             <- young_tr |> filter(occurred_on >= as.Date("2020-01-01"))

cal <- read.csv("docs/nyc_school_calendar.csv")
cal$start <- as.Date(cal$start)
cal$end   <- as.Date(cal$end)

# TRUE for each date inside any period of the given type
in_type <- function(dates, which_type) {
  p   <- cal[cal$type == which_type, ]
  hit <- rep(FALSE, length(dates))
  for (i in seq_len(nrow(p))) {
    hit <- hit | (dates >= p$start[i] & dates <= p$end[i])
  }
  hit
}

# Every day of 2020-2025, classified once
days <- data.frame(occurred_on = seq(as.Date("2020-01-01"), as.Date("2025-12-31"), by = "day"))
weekend <- format(days$occurred_on, "%u") %in% c("6", "7")

days$school_day <- case_when(
  in_type(days$occurred_on, "covid") & weekend ~ "COVID weekend",
  in_type(days$occurred_on, "covid")           ~ "COVID",
  weekend                                      ~ "Weekend",
  in_type(days$occurred_on, "partial")         ~ "Partial day",
  in_type(days$occurred_on, "break")           ~ "Holiday/Recess",
  in_type(days$occurred_on, "remote_day")      ~ "Holiday/Recess",
  in_type(days$occurred_on, "school_year")     ~ "Classes",
  TRUE                                         ~ "Summer Break"
)

# School year label (September to August)
start_year <- as.integer(format(days$occurred_on, "%Y")) -
  (as.integer(format(days$occurred_on, "%m")) < 9)
days$school_year <- paste0(start_year, "-", substr(start_year + 1, 3, 4))

young_tr <- left_join(young_tr, days, by = "occurred_on")

n_days <- days |> count(school_day, name = "n_days")

# Confirming the calendar (a full school year has about 175-180 class days)
days |> filter(school_day == "Classes") |> count(school_year)
n_days


# ----------------------------------------------------------------------------

# 1. Hourly share of underage victims: regular weekdays with classes vs days without classes

reasons <- c("Holiday/Recess", "Summer Break", "COVID", "Weekend")

minor_pct_tr <- young_tr |>
  group_by(school_day, hour) |>
  summarise(n_all = n(), n_minor = sum(minor), percentage = n_minor / n_all, .groups = "drop")

minor_pct_tr <- left_join(minor_pct_tr, n_days, by = "school_day")
minor_pct_tr$per_day <- minor_pct_tr$n_minor / minor_pct_tr$n_days

# Orange line: without the school_day column, ggplot draws it in every panel
minor_pct_classes_tr <- minor_pct_tr |> filter(school_day == "Classes") |> select(-school_day)

# Yellow line: one panel per reason for no classes
minor_pct_no_classes_tr <- minor_pct_tr |> filter(school_day %in% reasons)
minor_pct_no_classes_tr$school_day <- factor(minor_pct_no_classes_tr$school_day, levels = reasons)

# Counts written inside each panel
n_reason <- minor_pct_no_classes_tr |>
  group_by(school_day) |>
  summarise(label = paste0("n = ", scales::comma(sum(n_minor)), " underage of ",
                           scales::comma(sum(n_all)), " victims"),
            .groups = "drop")

n_classes <- paste0("Regular weekdays with classes: n = ", scales::comma(sum(minor_pct_classes_tr$n_minor)),
                    " underage of ", scales::comma(sum(minor_pct_classes_tr$n_all)), " victims")

minor_pct_cal_tr_plt <- ggplot(minor_pct_no_classes_tr, aes(x = hour, y = percentage)) +
  annotate("rect", xmin = 6.5,  xmax = 7.5,  ymin = -Inf, ymax = Inf, fill = grey, alpha = 0.5) +
  annotate("rect", xmin = 14.5, xmax = 15.5, ymin = -Inf, ymax = Inf, fill = grey, alpha = 0.5) +
  geom_line(data = minor_pct_classes_tr, aes(colour = "Regular weekdays with classes"), linewidth = 1.2) +
  geom_point(data = minor_pct_classes_tr, aes(colour = "Regular weekdays with classes"), size = 2) +
  geom_line(aes(colour = "No classes (reason in panel title)"), linewidth = 1.2) +
  geom_point(aes(colour = "No classes (reason in panel title)"), size = 2) +
  geom_text(data = n_reason, aes(x = 0, y = Inf, label = label),
            hjust = 0, vjust = 1.5, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ school_day, ncol = 2) +
  scale_colour_manual(values = c("Regular weekdays with classes" = orange,
                                 "No classes (reason in panel title)" = gold),
                      breaks = c("Regular weekdays with classes", "No classes (reason in panel title)")) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Hourly Underage Victim Percentage in Public Transportation: Days with vs without Classes",
       subtitle = paste0(n_classes, ". Shaded: 7h and 15h"),
       x = "Hour of occurrence", y = "Percentage of Underage Victims", colour = NULL,
       caption = source_cal) +
  theme_minimal() +
  theme(legend.position = "bottom")

minor_pct_cal_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis_revisited/class_vs_no_classes_percentage_public_transport.png",
       plot = minor_pct_cal_tr_plt, width = 14, height = 8, bg = white)


# ----------------------------------------------------------------------------

# 1.1 Same comparison in counts: underage victims per day, at each hour
#     (each group has a different number of days, so counts are divided by them)

n_reason_days <- data.frame(school_day = factor(reasons, levels = reasons),
                            label = paste0(scales::comma(n_days$n_days[match(reasons, n_days$school_day)]), " days"))

minor_day_cal_tr_plt <- ggplot(minor_pct_no_classes_tr, aes(x = hour, y = per_day)) +
  annotate("rect", xmin = 6.5,  xmax = 7.5,  ymin = -Inf, ymax = Inf, fill = grey, alpha = 0.5) +
  annotate("rect", xmin = 14.5, xmax = 15.5, ymin = -Inf, ymax = Inf, fill = grey, alpha = 0.5) +
  geom_line(data = minor_pct_classes_tr, aes(colour = "Regular weekdays with classes"), linewidth = 1.2) +
  geom_point(data = minor_pct_classes_tr, aes(colour = "Regular weekdays with classes"), size = 2) +
  geom_line(aes(colour = "No classes (reason in panel title)"), linewidth = 1.2) +
  geom_point(aes(colour = "No classes (reason in panel title)"), size = 2) +
  geom_text(data = n_reason_days, aes(x = 0, y = Inf, label = label),
            hjust = 0, vjust = 1.5, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ school_day, ncol = 2) +
  scale_colour_manual(values = c("Regular weekdays with classes" = orange,
                                 "No classes (reason in panel title)" = gold),
                      breaks = c("Regular weekdays with classes", "No classes (reason in panel title)")) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Hourly Underage Victims per Day in Public Transportation: Days with vs without Classes",
       subtitle = paste0("Regular weekdays with classes: ",
                         scales::comma(n_days$n_days[n_days$school_day == "Classes"]),
                         " days. Shaded: 7h and 15h"),
       x = "Hour of occurrence", y = "Underage victims per day (average)", colour = NULL,
       caption = source_cal) +
  theme_minimal() +
  theme(legend.position = "bottom")

minor_day_cal_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis_revisited/class_vs_no_classes_per_day_public_transport.png",
       plot = minor_day_cal_tr_plt, width = 14, height = 8, bg = white)



# 2. Underage victims at 7h and 15h, per school year
#    Weekdays with classes vs weekdays without classes (Holiday/Recess + Summer Break,
#    COVID left out). Criterion: share with classes above 1.75x the share without,
#    in at least 5 of the 6 school years.
#    2019-20 only has Jan-Mar 2020 and 2025-26 only Sep-Dec 2025.

ratio_line <- 1.75

minor_peak_tr <- young_tr |>
  filter(hour %in% peak_hours, school_day %in% c("Classes", "Holiday/Recess", "Summer Break")) |>
  mutate(classes = ifelse(school_day == "Classes", "Weekdays with classes", "Weekdays without classes")) |>
  group_by(school_year, classes) |>
  summarise(n_all = n(), n_minor = sum(minor), percentage = n_minor / n_all, .groups = "drop")

# Line over each orange bar at 1.75x the yellow bar next to it (the orange bar has to clear it)
threshold <- minor_peak_tr |>
  filter(classes == "Weekdays without classes") |>
  mutate(x = as.numeric(factor(school_year)),   # orange bar sits at x - 0.2
         y = ratio_line * percentage)

minor_peak_tr_plt <- ggplot(minor_peak_tr, aes(x = school_year, y = percentage, fill = classes)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = paste0("n = ", n_minor)), position = position_dodge(width = 0.8),
            vjust = 1.5, size = 3.5) +
  geom_segment(data = threshold,
               aes(x = x - 0.39, xend = x - 0.01, y = y, yend = y,
                   linetype = paste0(ratio_line, "x the share without classes")),
               inherit.aes = FALSE, colour = red, linewidth = 1) +
  scale_fill_manual(values = c("Weekdays with classes" = orange, "Weekdays without classes" = gold)) +
  scale_linetype_manual(values = "dashed", name = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Underage Victim Percentage at 7h and 15h in Public Transportation, per School Year",
       subtitle = "Without classes: Holiday/Recess and Summer Break weekdays (COVID left out)",
       x = "School year (September to August)", y = "Percentage of Underage Victims", fill = NULL,
       caption = source_cal) +
  theme_minimal() +
  theme(legend.position = "bottom")

minor_peak_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis_revisited/criterion_per_school_year.png",
       plot = minor_peak_tr_plt, width = 12, height = 6, bg = white)


# 3. Where: boroughs, precincts and subway stations
#    For each place: its share of all public transport victims (x) vs its share of
#    underage victims at 7h and 15h on class days (y).
#    Above the dashed line = more than its share of underage victims at those hours.

young_tr$peak <- young_tr$minor & young_tr$school_day == "Classes" & young_tr$hour %in% peak_hours

# 3.1 Boroughs

minor_boro_tr <- young_tr |>
  filter(!is.na(boro_nm), boro_nm != "(null)") |>
  group_by(boro_nm) |>
  summarise(n_all = n(), n_peak = sum(peak), .groups = "drop")

minor_boro_tr$pct_all  <- minor_boro_tr$n_all / sum(minor_boro_tr$n_all)
minor_boro_tr$pct_peak <- minor_boro_tr$n_peak / sum(minor_boro_tr$n_peak)
top <- max(minor_boro_tr$pct_all, minor_boro_tr$pct_peak)

minor_boro_tr_plt <- ggplot(minor_boro_tr, aes(x = pct_all, y = pct_peak)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = black) +
  geom_point(colour = orange, size = 3) +
  geom_text(aes(label = paste0(boro_nm, " (n = ", n_peak, ")")), vjust = -1, size = 3.5) +
  scale_x_continuous(labels = scales::percent, limits = c(0, top * 1.1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, top * 1.1)) +
  labs(title = "Underage Victims at 7h and 15h on Class Days per Borough (Public Transportation)",
       subtitle = "Above the dashed line: more than its share of underage victims at 7h and 15h",
       x = "Share of all public transport victims", y = "Share of underage victims at 7h and 15h",
       caption = source_cal) +
  theme_minimal()

minor_boro_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis_revisited/where_borough.png",
       plot = minor_boro_tr_plt, width = 9, height = 7, bg = white)

# 3.2 Precincts (top 10 by number of underage victims are labelled)

minor_precinct_tr <- young_tr |>
  filter(!is.na(addr_pct_cd)) |>
  group_by(addr_pct_cd) |>
  summarise(n_all = n(), n_peak = sum(peak), .groups = "drop")

minor_precinct_tr$pct_all  <- minor_precinct_tr$n_all / sum(minor_precinct_tr$n_all)
minor_precinct_tr$pct_peak <- minor_precinct_tr$n_peak / sum(minor_precinct_tr$n_peak)
top <- max(minor_precinct_tr$pct_all, minor_precinct_tr$pct_peak)

minor_precinct_tr_plt <- ggplot(minor_precinct_tr, aes(x = pct_all, y = pct_peak)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = black) +
  geom_point(colour = orange, size = 2.5) +
  geom_text(data = slice_max(minor_precinct_tr, n_peak, n = 10),
            aes(label = paste0(addr_pct_cd, " (n = ", n_peak, ")")), vjust = -1, size = 3.2, check_overlap = TRUE) +
  scale_x_continuous(labels = scales::percent, limits = c(0, top * 1.1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, top * 1.1)) +
  labs(title = "Underage Victims at 7h and 15h on Class Days per Precinct (Public Transportation)",
       subtitle = "Above the dashed line: more than its share of underage victims at 7h and 15h",
       x = "Share of all public transport victims", y = "Share of underage victims at 7h and 15h",
       caption = source_cal) +
  theme_minimal()

minor_precinct_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis_revisited/where_precinct.png",
       plot = minor_precinct_tr_plt, width = 9, height = 7, bg = white)

# 3.3 Subway stations (the precinct goes next to the name, as names repeat across lines)

minor_station_tr <- young_tr |>
  filter(!is.na(station_name), station_name != "(null)") |>
  mutate(station = paste0(station_name, " (", addr_pct_cd, ")")) |>
  group_by(station) |>
  summarise(n_all = n(), n_peak = sum(peak), .groups = "drop")

minor_station_tr$pct_all  <- minor_station_tr$n_all / sum(minor_station_tr$n_all)
minor_station_tr$pct_peak <- minor_station_tr$n_peak / sum(minor_station_tr$n_peak)
top <- max(minor_station_tr$pct_all, minor_station_tr$pct_peak)

minor_station_tr_plt <- ggplot(minor_station_tr, aes(x = pct_all, y = pct_peak)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = black) +
  geom_point(colour = orange, size = 2, alpha = 0.7) +
  geom_text(data = slice_max(minor_station_tr, n_peak, n = 10, with_ties = FALSE),
            aes(label = paste0(station, ", n = ", n_peak)), vjust = -1, size = 3, check_overlap = TRUE) +
  scale_x_continuous(labels = scales::percent, limits = c(0, top * 1.1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, top * 1.1)) +
  labs(title = "Underage Victims at 7h and 15h on Class Days per Subway Station",
       subtitle = "Above the dashed line: more than its share of underage victims at 7h and 15h",
       x = "Share of all subway victims", y = "Share of underage victims at 7h and 15h",
       caption = source_cal) +
  theme_minimal()

minor_station_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis_revisited/where_station.png",
       plot = minor_station_tr_plt, width = 9, height = 7, bg = white)

# Conclusion: