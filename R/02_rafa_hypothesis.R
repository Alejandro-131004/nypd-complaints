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


# Loading dataset and removing old occurrences
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- subset(nypd, report_year >= 2020 & report_year <= 2025)
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)

# --------------------------------------------------------------- #
  # ----------------------- Disclaimers ----------------------- #
# --------------------------------------------------------------- #

# If year interval is not specified, consider 2020-2025

# --------------------------------------------------------------- #
 # ----------------------- Future Work ------------------------ #
# --------------------------------------------------------------- #

# Check offence level per (apartment, house) vs (public_housing)
# Check if sex crimes, rape and harrassment happen more at public housing
# Maybe check gender splits? Would be interesting to devise specific strategies
# even if my solution remains global

# --------------------------------------------------------------- #
  # ----------------------- Hypothesis ------------------------ #
# --------------------------------------------------------------- #

# Victims under 18 (underage/minor) are concentrated in times of
# home-school-home commute, as well as unsupervised after-school time.

# The plots below are dedicated to supporting or refuting this hypothesis.

# ------------------------------------------------------------------------

# 1. Raw counts of underage victims in both residences and public transportation
valid_ages <- c("<18", "18-24", "25-44", "45-64", "65+")

dir.create("fig/02_exploration/02_rafa/hypothesis", showWarnings = FALSE, recursive = TRUE)

# Setting: any residence, or any bus / subway / transit premises
nypd$setting <- ifelse(grepl("RESIDENCE", nypd$prem_typ_desc), "Residence",
                       ifelse(grepl("TRANSIT|^BUS ", nypd$prem_typ_desc), "Public transport", NA))
young <- nypd |>
  filter(!is.na(setting), !is.na(hour), !is.na(occurred_on),
         vic_age_group %in% valid_ages)

young$minor    <- young$vic_age_group == "<18"
young$day_type <- ifelse(format(young$occurred_on, "%u") %in% c("6", "7"), "Weekend", "Weekday")
young$month    <- format(young$occurred_on, "%m")

# All victims vs underage victims per premises: residences
premises_res <- bind_rows(
  young |> filter(setting == "Residence") |> count(prem_typ_desc) |> mutate(group = "All victims"),
  young |> filter(minor, setting == "Residence") |> count(prem_typ_desc) |> mutate(group = "Underage victims")
)

premises_res_plt <- ggplot(premises_res, aes(x = n, y = reorder(prem_typ_desc, n), fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("All victims" = grey, "Underage victims" = dark_blue)) +
  scale_x_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "All and Underage Victims in Residences (2020-2025)",
       x = "Number of complaints", y = NULL, fill = NULL) +
  theme_minimal()

premises_res_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/underage_victims_residence.png",
       plot = premises_res_plt, width = 9, height = 4, bg = white)

# All victims vs underage victims per premises: public transport
premises_tr <- bind_rows(
  young |> filter(setting == "Public transport") |> count(prem_typ_desc) |> mutate(group = "All victims"),
  young |> filter(minor, setting == "Public transport") |> count(prem_typ_desc) |> mutate(group = "Underage victims")
)

premises_tr_plt <- ggplot(premises_tr, aes(x = n, y = reorder(prem_typ_desc, n), fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("All victims" = grey, "Underage victims" = orange)) +
  scale_x_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "All and Underage Victims in Public Transportation (2020-2025)",
       x = "Number of complaints", y = NULL, fill = NULL) +
  theme_minimal()

premises_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/underage_victims_transport.png",
       plot = premises_tr_plt, width = 9, height = 4, bg = white)

# ----------------------------------------------------------------------------

# 1.1 Percentage of underage victims for the aforementioned locations
minor_setting <- young |>
  group_by(setting) |>
  summarise(percentage = mean(minor), total = n(), .groups = "drop")

all_valid <- nypd |>
  filter(vic_age_group %in% valid_ages)

minor_setting <- bind_rows(minor_setting,
                 data.frame(setting = "All premises",
                 percentage = mean(all_valid$vic_age_group == "<18"),
                 total = nrow(all_valid)))

minor_setting$label <- paste0(minor_setting$setting, "\n(n = ", scales::comma(minor_setting$total), ")")

minor_setting_plt <- ggplot(minor_setting, aes(x = reorder(label, -percentage), y = percentage, fill = setting)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percentage, accuracy = 0.1)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Residence" = dark_blue, "Public transport" = orange,
                               "All premises" = grey),
                    guide = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Percentage of Underage Victims (2020-2025)",
       x = NULL, y = "Percentage of Underage Victims") +
  theme_minimal()

minor_setting_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/underage_victims_percentage_global.png",
       plot = minor_setting_plt, width = 8, height = 5, bg = white)



# This means that children account for ~8% of all public transportation 
# victims, as well as 4.6% of all residence victims

# Underage share is not overwhelmingly large, but values remain concerning
# as it represents a group the city has a specific responsibility to protect.

# That protection depends on both adults and institutions.

# ----------------------------------------------------------------------------

# This brings us back to the hours and weekdays rationale that indicates
# lack of supervision might be a strong cause for the spike 

# 2. Underage victims on weekdays, by hour: residences (counts)
minor_counts_res <- young |>
  filter(minor, day_type == "Weekday", setting == "Residence") |>
  count(hour)

minor_counts_res_plt <- ggplot(minor_counts_res, aes(x = hour, y = n)) +
  geom_line(linewidth = 1.2, colour = dark_blue) +
  geom_point(size = 2, colour = dark_blue) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Hourly Underage Victim Counts on Weekdays, in Residences",
       x = "Hour of occurrence", y = "Number of complaints") +
  theme_minimal()

minor_counts_res_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/weekday_hour_residence.png",
       plot = minor_counts_res_plt, width = 10, height = 5, bg = white)



# 2.1 Underage victims on weekdays, by hour: public transport (counts)
minor_counts_tr <- young |>
  filter(minor, day_type == "Weekday", setting == "Public transport") |>
  count(hour)

minor_counts_tr_plt <- ggplot(minor_counts_tr, aes(x = hour, y = n)) +
  geom_line(linewidth = 1.2, colour = orange) +
  geom_point(size = 2, colour = orange) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Hourly Underage Victim Counts on Weekdays, in Public Transportation",
       x = "Hour of occurrence", y = "Number of complaints") +
  theme_minimal()

minor_counts_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/weekday_hour_public_transportation.png",
       plot = minor_counts_tr_plt, width = 10, height = 5, bg = white)

# Again, the timing matches that of a regular school day: 
# first, they catch a transport peak on the way to school (7-8h)
# followed by another, larger one on the way back (14-16h).
# Home arrival time is more broad likely due to distances in residences (15-20h)

# In other words, this might identify a security issue in trips between
# institutions and residences as well as possibly unsupervised, alone
# time at their residences.

# ----------------------------------------------------------------------------

# This can be better confirmed if weekends contradict the spikes in previous
# plots: it would confirm these crimes happen at school-residence commutes

# 3. Weekday vs weekend: residences
minor_pct_day_res <- young |>
  filter(setting == "Residence") |>
  group_by(day_type, hour) |>
  summarise(percentage = mean(minor), .groups = "drop")

minor_pct_day_res_plt <- ggplot(minor_pct_day_res, aes(x = hour, y = percentage, colour = day_type, group = day_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Weekday" = dark_blue, "Weekend" = dv_blue)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Hourly Underage Victim Percentage (Residence, Weekday vs Weekend)",
       x = "Hour of occurrence", y = "Percentage of Underage Victims", colour = NULL) +
  theme_minimal()

minor_pct_day_res_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/weekday_vs_weekend_residence.png",
       plot = minor_pct_day_res_plt, width = 10, height = 5, bg = white)


# 3.1 Weekday vs weekend: public transport
minor_pct_day_tr <- young |>
  filter(setting == "Public transport") |>
  group_by(day_type, hour) |>
  summarise(percentage = mean(minor), .groups = "drop")

minor_pct_day_tr_plt <- ggplot(minor_pct_day_tr, aes(x = hour, y = percentage, colour = day_type, group = day_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Weekday" = orange, "Weekend" = gold)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Hourly Underage Victim Percentage (Public Transportation, Weekday vs Weekend)",
       x = "Hour of occurrence", y = "Percentage of Underage Victims", colour = NULL) +
  theme_minimal()

minor_pct_day_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/weekday_vs_weekend_public_transportation.png",
       plot = minor_pct_day_tr_plt, width = 10, height = 5, bg = white)

# As expected, the strong spike is lost on weekends, especially in public transportation. 
# As further confirmation that this is not simply due to transportation frequency
# or another weekend specific variable, let's look at months where educational
# institutions are no longer brimming with commuting students

# ----------------------------------------------------------------------------

# 4. School months vs summer (July-August), weekdays only: residences
minor_season <- young |>
  filter(day_type == "Weekday")

minor_season$season <- ifelse(minor_season$month %in% c("07", "08"),
                              "Summer (Jul-Aug)", "School months")

minor_pct_season_res <- minor_season |>
  filter(setting == "Residence") |>
  group_by(season, hour) |>
  summarise(percentage = mean(minor), .groups = "drop")

minor_pct_season_res_plt <- ggplot(minor_pct_season_res, aes(x = hour, y = percentage, colour = season, group = season)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("School months" = dark_blue, "Summer (Jul-Aug)" = dv_blue)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Weekday Percentage of Underage Victims (Residence, School Months vs Summer)",
       x = "Hour of occurrence", y = "Percentage of Underage Victims", colour = NULL) +
  theme_minimal()

minor_pct_season_res_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/school_vs_summer_residence.png",
       plot = minor_pct_season_res_plt, width = 10, height = 5, bg = white)


# 4.1 School months vs summer (July-August), weekdays only: public transport
minor_pct_season_tr <- minor_season |>
  filter(setting == "Public transport") |>
  group_by(season, hour) |>
  summarise(percentage = mean(minor), .groups = "drop")

minor_pct_season_tr_plt <- ggplot(minor_pct_season_tr, aes(x = hour, y = percentage, colour = season, group = season)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("School months" = orange, "Summer (Jul-Aug)" = gold)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Weekday Percentage of Underage Victims 
       (Public Transportation, School Months vs Summer)",
       x = "Hour of occurrence", y = "Percentage of Underage Victims", colour = NULL) +
  theme_minimal()

minor_pct_season_tr_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/school_vs_summer_public_transport.png",
       plot = minor_pct_season_tr_plt, width = 10, height = 5, bg = white)

# Once again, confirms the hypothesis. Residences still increase in report counts
# but lose the hour-specific, drastic spike associated with unsupervised arrival
# at a residence.

# ---------------------------------------------------------------------------

# --------------------------------------------------------------- #
  # ----------------------- Specifying ------------------------ #
# --------------------------------------------------------------- #

# 5. Underage Victims: Offense Levels (residences vs public transport)
#    (percentages within each premise, so each premise adds up to 100%)

minor_sev <- young |>
  filter(minor, !is.na(law_cat_cd)) |>
  count(setting, law_cat_cd) |>
  group_by(setting) |>
  mutate(percentage = n / sum(n)) |>
  ungroup()

minor_sev_plt <- ggplot(minor_sev, aes(x = law_cat_cd, y = percentage, fill = setting)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Residence" = dark_blue, "Public transport" = orange)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Underage Victims: Offence Levels (residences vs public transport)",
       x = "Offence level", y = "Percentage of Underage Victims", fill = "Setting") +
  theme_minimal()

minor_sev_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/offence_levels.png",
       plot = minor_sev_plt, width = 10, height = 5, bg = white)

# Offence levels follow the global rationale, nothing to see here

# 6. Top 10 offences against minors: residences vs public transport
# Add the offence level to the name only when an offence appears under
# more than one level in the same setting (like sex crimes)

minor_off <- young |>
  filter(minor, !is.na(ofns_desc), !is.na(law_cat_cd)) |>
  count(setting, ofns_desc, law_cat_cd) |>
  group_by(setting) |>
  mutate(percentage = n / sum(n)) |>
  slice_max(n, n = 10) |>
  ungroup()

minor_off <- minor_off |>
  group_by(setting, ofns_desc) |>
  mutate(n_levels = n()) |>
  ungroup()

minor_off$offence_name <- ifelse(minor_off$n_levels > 1,
                                 paste0(minor_off$ofns_desc, " (", tolower(minor_off$law_cat_cd), ")"),
                                 minor_off$ofns_desc)

# Name made unique per setting, so each panel is sorted on its own
minor_off$offence_key <- paste(minor_off$offence_name, minor_off$setting, sep = "___")

# Colour key: setting + offence level, so each panel keeps its own colours
minor_off$colour_key <- paste(minor_off$setting, minor_off$law_cat_cd, sep = " - ")

minor_off_plt <- ggplot(minor_off, aes(x = percentage, y = reorder(offence_key, percentage),
                                       fill = colour_key)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percentage, accuracy = 0.1)), hjust = -0.1, size = 3.2) +
  facet_wrap(~ setting, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("___.*", "", x)) +
  scale_fill_manual(values = c("Residence - FELONY"             = dark_blue,
                               "Residence - MISDEMEANOR"        = dv_blue,
                               "Residence - VIOLATION"          = light_blue,
                               "Public transport - FELONY"      = dark_orange,
                               "Public transport - MISDEMEANOR" = orange,
                               "Public transport - VIOLATION"   = gold),
                    breaks = c("Residence - FELONY", "Residence - MISDEMEANOR", "Residence - VIOLATION",
                               "Public transport - FELONY", "Public transport - MISDEMEANOR",
                               "Public transport - VIOLATION")) +
  scale_x_continuous(labels = scales::percent, limits = c(0, NA), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "10 most Common Offences against Underage Victims (Residence & Pub. Trans.)",
       x = "Percentage of each Premise/Location's Underage Victims per Crime", y = NULL, fill = "Offence level") +
  theme_minimal()

minor_off_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/underage_top_10_crimes.png",
       plot = minor_off_plt, width = 14, height = 6, bg = white)

# Clearly, our most important result yet. Our responsibility increases when
# these crimes are as heinous as those displayed. It's no longer a matter of simple
# robberies or fixing structures in misbehaviour: it's about children and teenagers'
# lives, integrity and health.