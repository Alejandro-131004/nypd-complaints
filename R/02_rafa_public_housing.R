#install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

dv_blue <- "#2E75B6"
orange <- "orange2"
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


# Loading dataset and removing old occurrences
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- subset(nypd, report_year >= 2020 & report_year <= 2025)
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)

# Sources

# https://catalog.data.gov/dataset/nycha-resident-data-book-summary


# Pertinent list variables
residences <- c("RESIDENCE - PUBLIC HOUSING",
                "RESIDENCE - APT. HOUSE",
                "RESIDENCE-HOUSE")

# --------------------------------------------------------------- #
   # -------------------- Public Housing --------------------- #
# --------------------------------------------------------------- #



# After looking at severity_sex_premises.png, female victims have consistently 
# outnumbered male victims for all offense levels inside public housing

# The NYCHA Resident Data Book Summary only separates per head of the family
# sex, which is mostly female (roughly 70, 75% from what I could see), but these
# values can be largely female-inflated due to omitting all other house members

# Nonetheless, there is no way for me to verify this information purely,
# so I'll only be looking at victim counts discrepancy.

# Disclaimer: there is a chance that these values can be explained by larger
# female population, but I have no way of fully confirming this.

# --------------------------------------------------------------- #
  # ------------------- Evolution (20-25) --------------------- #
# --------------------------------------------------------------- #


# Let's look at an evolution of male vs female victims in public housing, from 
# 2020 to 2025

# 1. Yearly counts of female and male victims in public housing
ph_counts <- nypd |>
  filter(vic_sex %in% c("F", "M"), prem_typ_desc == "RESIDENCE - PUBLIC HOUSING") |>
  count(report_year, vic_sex)
ph_total <- ph_counts |>
  group_by(report_year) |>
  summarise(n = sum(n)) |>
  mutate(vic_sex = "Total")

ph_counts <- bind_rows(ph_counts, ph_total)

# Male vs Female vs Total Counts
ph_counts_plt <- ggplot(ph_counts, aes(x = report_year, y = n,
                                       colour = vic_sex, group = vic_sex)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Total" = "grey60", "F" = pink, "M" = dv_blue),
                      labels = c("Total" = "Total", "F" = "Female", "M" = "Male"),
                      breaks = c("Total", "F", "M")) +
  scale_x_continuous(breaks = 2020:2025) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title  = "Victims in Public Housing by Sex (2020-2025)",
       x      = "Report year",
       y      = "Number of complaints",
       colour = "Victim sex",
       caption = source) +
  theme_minimal()

ph_counts_plt

ggsave("fig/02_exploration/02_rafa/public_housing_female/public_housing_victims_sex_count.png",
       plot = ph_counts_plt, width = 9, height = 5, bg = white)

# From 2020-2023, there was a clear increase in both male and female victim
# complaints. From 2023 onward, these values decreased but are now either 
# stabilizing or hinting at another growth period.

# 1.1 Percentages
ph_share <- nypd |>
  filter(vic_sex %in% c("F", "M"), prem_typ_desc == "RESIDENCE - PUBLIC HOUSING") |>
  count(report_year, vic_sex) |>
  group_by(report_year) |>
  mutate(share = n / sum(n)) |>
  ungroup()

View(ph_share)

ph_share_plt <- ggplot(ph_share, aes(x = report_year, y = share,
                                     colour = vic_sex, group = vic_sex)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("F" = pink, "M" = dv_blue),
                      labels = c("F" = "Female", "M" = "Male")) +
  scale_x_continuous(breaks = 2020:2025) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title  = "Percentage of Victims in Public Housing by Sex (2020-2025)",
       x      = "Report year",
       y      = "Sex percentage",
       colour = "Victim sex",
       caption = source) +
  theme_minimal()

ph_share_plt

ggsave("fig/02_exploration/02_rafa/public_housing_female/public_housing_victims_sex_percent.png",
       plot = ph_share_plt, width = 9, height = 5, bg = white)

# Upon considering percentages, 2025 values hint at a recession of female victim
# percentage back to near 75% values. Unfortunately, I can't derive any real
# conclusions without a population baseline, but it's clear that the issue has
# seen no significant measures to mitigate or solve


# --------------------------------------------------------------- #
    # ------------------- Concentration --------------------- #
# --------------------------------------------------------------- #

# Now, I'd like to see if these occurrences are spread evenly throughout
# NYCHA residences, or if there is a higher density in a specific part 

# 2.1 Top 10 NYCHA residence female victim 
ph <- nypd |>
  filter(prem_typ_desc == "RESIDENCE - PUBLIC HOUSING", vic_sex %in% c("F", "M"))

# counting missing name
missing_vals <- c("", "(NULL)", "NULL", "NA", "N/A", "NAN", "UNKNOWN")
ph$dev_missing <- is.na(ph$hadevelopt) | toupper(trimws(ph$hadevelopt)) %in% missing_vals

ph_missing <- subset(ph, dev_missing)
print("Missing development names, by form:")
print(count(ph_missing, hadevelopt))
print(paste0("Total missing: ", nrow(ph_missing), " of ", nrow(ph), " complaints (",
             round(100 * nrow(ph_missing) / nrow(ph), 1), "%)"))

# percent female victims for NaN names
print(paste0("Female share of victims among missing: ",
             round(100 * mean(ph_missing$vic_sex == "F"), 1), "%"))

# Top 10 NYCHA housings by female percentage of victims
dev_share <- ph |>
  filter(!dev_missing) |>
  count(hadevelopt, vic_sex) |>
  group_by(hadevelopt) |>
  mutate(total = sum(n), fem_share = n / total) |>
  ungroup() |>
  filter(vic_sex == "F", total >= 50) |>
  slice_max(fem_share, n = 10)

# Name with the total number of victims underneath
dev_share$label <- paste0(dev_share$hadevelopt, "\n(n = ", scales::comma(dev_share$total), ")")

dev_share_plt <- ggplot(dev_share, aes(x = reorder(label, -fem_share), y = fem_share)) +
  geom_col(fill = pink) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "NYCHA Developments with the Highest Female Share of Victims, 2020-2025",
       x = NULL,
       y = "Female percentage of victims (M/F only)",
       caption = paste0("Developments with at least 50 complaints.\n", source)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8))

dev_share_plt

ggsave("fig/02_exploration/02_rafa/public_housing_female/nycha_top10_female_share.png",
       plot = dev_share_plt, width = 12, height = 5, bg = white)

# 2.2 Bottom 10 NYCHA residence female victim 


dev_all <- ph |>
  filter(!dev_missing) |>
  count(hadevelopt, vic_sex) |>
  group_by(hadevelopt) |>
  mutate(total = sum(n), fem_share = n / total) |>
  ungroup() |>
  filter(vic_sex == "F", total >= 50)


dev_bottom <- dev_all |>
  slice_min(fem_share, n = 10)

dev_bottom$label <- paste0(dev_bottom$hadevelopt, "\n(n = ", scales::comma(dev_bottom$total), ")")

dev_bottom_plt <- ggplot(dev_bottom, aes(x = reorder(label, fem_share), y = fem_share)) +
  geom_col(fill = pink) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Top 10 Lowest NYCHA Female Victim Percentage (> 50 victims)",
       x = NULL,
       y = "Female share of victims (M/F only)",
       caption = source) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8))

dev_bottom_plt

ggsave("fig/02_exploration/02_rafa/public_housing_female/nycha_bottom10_female_share.png",
       plot = dev_bottom_plt, width = 12, height = 5, bg = white)

# Here, we confirm once again that there is never a case where men are more affected
# than women for this issue, independently of public housing residence, fluctuating
# between values around 63-78%


# --------------------------------------------------------------- #
  # ------------------- Age Vulnerability --------------------- #
# --------------------------------------------------------------- #


# 3. Age Vulnerability for crimes against Women in Public Housing
valid_ages <- c("<18", "18-24", "25-44", "45-64", "65+")

ph_age <- nypd |>
  filter(vic_sex == "F", prem_typ_desc == "RESIDENCE - PUBLIC HOUSING",
         vic_age_group %in% valid_ages) |>
  count(vic_age_group) |>
  mutate(share = n / sum(n))

# Ages in increasing order (otherwise "<18" is sorted last)
ph_age$vic_age_group <- factor(ph_age$vic_age_group, levels = valid_ages)

View(ph_age)

# Bars add up to 100%; darker blue = older age group
ph_age_plt <- ggplot(ph_age, aes(x = vic_age_group, y = share)) +
  geom_col(fill = pink) +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Age of Female Victims in Public Housing, 2020-2025",
       x = "Victim age group",
       y = "Share of female victims in public housing",
       caption = source) +
  theme_minimal()

ph_age_plt

ggsave("fig/02_exploration/02_rafa/public_housing_female/public_housing_female_victims_age.png",
       plot = ph_age_plt, width = 8, height = 5, bg = white)

# Majority of victims are in the 25-44 years old range

# --------------------------------------------------------------- #
    # ------------------- Time of Day --------------------- #
# --------------------------------------------------------------- #

ph_age_hour <- nypd |>
  filter(vic_sex == "F", prem_typ_desc == "RESIDENCE - PUBLIC HOUSING",
         vic_age_group %in% valid_ages, !is.na(hour)) |>
  count(vic_age_group, hour) |>
  group_by(vic_age_group) |>
  mutate(share = n / sum(n)) |>
  ungroup()

ph_age_hour$vic_age_group <- factor(ph_age_hour$vic_age_group, levels = valid_ages)

# Each age group's 24 hours add up to 100%: compares the SHAPE of the day
# between ages, regardless of how many victims each age group has
ph_age_hour_plt <- ggplot(ph_age_hour, aes(x = hour, y = share,
                                           colour = vic_age_group, group = vic_age_group)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c("<18"   = "#C6DBEF",
                                 "18-24" = "#9ECAE1",
                                 "25-44" = "#4292C6",
                                 "45-64" = "#2171B5",
                                 "65+"   = dark_blue)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Time of Day of Female Victims in Public Housing, by Age, 2020-2025",
       x = "Hour of occurrence (start of the offence)",
       y = "Share of the age group's complaints",
       colour = "Victim age",
       caption = source) +
  theme_minimal()

ph_age_hour_plt

ggsave("fig/02_exploration/02_rafa/public_housing_female/public_housing_female_age_hour.png",
       plot = ph_age_hour_plt, width = 10, height = 5, bg = white)



# --------------------------


# ---- Cell 2: female victims by age and hour, APARTMENT HOUSES (comparison) ----

apt_age_hour <- nypd |>
  filter(vic_sex == "F", prem_typ_desc == "RESIDENCE - APT. HOUSE",
         vic_age_group %in% valid_ages, !is.na(hour)) |>
  count(vic_age_group, hour) |>
  group_by(vic_age_group) |>
  mutate(share = n / sum(n)) |>
  ungroup()

apt_age_hour$vic_age_group <- factor(apt_age_hour$vic_age_group, levels = valid_ages)

apt_age_hour_plt <- ggplot(apt_age_hour, aes(x = hour, y = share,
                                             colour = vic_age_group, group = vic_age_group)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c("<18" = "#9ECAE1", "18-24" = "#6BAED6", "25-44" = "#4292C6",
                                 "45-64" = "#2171B5", "65+" = dark_blue)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.10)) +
  labs(title = "Time of Day of Female Victims in Apartment Houses, by Age, 2020-2025",
       x = "Hour of occurrence (start of the offence)",
       y = "Share of the age group's complaints",
       colour = "Victim age",
       caption = source) +
  theme_minimal()

apt_age_hour_plt

ggsave("fig/02_exploration/02_rafa/public_housing_female/apartment_house_female_age_hour.png",
       plot = apt_age_hour_plt, width = 10, height = 5, bg = white)

# --------------------------------

# ---- Cell 3: girls under 18 in public housing, weekday vs weekend ----

# Day of the week from the occurrence date: %u gives 1 = Monday ... 7 = Sunday
# (a number, so it does not depend on the computer's language)
ph_u18 <- nypd |>
  filter(vic_sex == "F", prem_typ_desc == "RESIDENCE - PUBLIC HOUSING",
         vic_age_group == "<18", !is.na(hour), !is.na(occurred_on))

ph_u18$day_type <- ifelse(format(ph_u18$occurred_on, "%u") %in% c("6", "7"),
                          "Weekend", "Weekday")

u18_daytype <- ph_u18 |>
  count(day_type, hour) |>
  group_by(day_type) |>
  mutate(share = n / sum(n)) |>
  ungroup()

u18_daytype_plt <- ggplot(u18_daytype, aes(x = hour, y = share,
                                           colour = day_type, group = day_type)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c("Weekday" = dark_blue, "Weekend" = "#9ECAE1")) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Time of Day of Female Victims under 18 in Public Housing, Weekday vs Weekend",
       x = "Hour of occurrence (start of the offence)",
       y = "Share of the day type's complaints",
       colour = NULL,
       caption = source) +
  theme_minimal()

u18_daytype_plt

ggsave("fig/02_exploration/02_rafa/public_housing_female/public_housing_female_u18_weekday_weekend.png",
       plot = u18_daytype_plt, width = 10, height = 5, bg = white)