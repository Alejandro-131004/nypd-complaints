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
  # ----------------------- Hypothesis ------------------------ #
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
  # -------------------- Dedicated Plots ---------------------- #
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
       colour = "Victim sex") +
  theme_minimal()

ph_counts_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/public_housing_victims_sex_count.png",
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
       colour = "Victim sex") +
  theme_minimal()

ph_share_plt

ggsave("fig/02_exploration/02_rafa/hypothesis/public_housing_victims_sex_percent.png",
       plot = ph_share_plt, width = 9, height = 5, bg = white)

# Upon considering percentages, 2025 values hint at a divergent evolution of 
# values, where female victims increase while male victims decrease simultaneously