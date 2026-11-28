# Shneiderman's Statement: "Overview first, zoom and filter, then details on demand"

library(dplyr)
library(ggplot2)
library(plotly)


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


# Had to swap these out, they were disappearing in plotly
#source <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"
#source_cal <- paste0(source, "\nSchool calendar: NYC Public Schools, 2019-20 to 2025-26 (schools.nyc.gov/calendar)")

# HTML version
cite <- paste0("<br><sup>Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)",
               "<br>School calendar: NYC Public Schools, 2019-20 to 2025-26 (schools.nyc.gov/calendar)</sup>")

# Loading dataset and removing old occurrences
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- subset(nypd, report_year >= 2020 & report_year <= 2025)
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)

dir.create("fig/03_interaction/underage_public_transport_interactive", showWarnings = FALSE, recursive = TRUE)


# As suggested in this week's powerpoint, I'm going to start by re-running
# my most relevant images (from ppa and after feedback) and applying ggplotly(p)

# IMPORTANT: forgot to mention in earlier scripts but my pct naming convention
# is NOT precinct, but percentage!!


# ----------------- Important Variables ---------------- 

# First, I'll organize important variables to avoid cluttering down the line

# 1. Misc.
valid_ages <- c("<18", "18-24", "25-44", "45-64", "65+")
peak_hours <- c(7, 15)
reasons    <- c("Holiday/Recess", "Summer Break", "COVID", "Weekend")


# 2. Public transport victims with a valid age, from 2020
valid_age_tr <- nypd |>
  filter(grepl("TRANSIT|^BUS ", prem_typ_desc), !is.na(hour), !is.na(occurred_on),
         vic_age_group %in% valid_ages)

valid_age_tr$minor       <- valid_age_tr$vic_age_group == "<18"
valid_age_tr$occurred_on <- as.Date(valid_age_tr$occurred_on)
valid_age_tr             <- valid_age_tr |> filter(occurred_on >= as.Date("2020-01-01"))


# 3. School calendar setup (claude helped do this setup)
cal <- read.csv("docs/nyc_school_calendar.csv")
cal$start <- as.Date(cal$start)
cal$end   <- as.Date(cal$end)

in_type <- function(dates, which_type) {
  p   <- cal[cal$type == which_type, ]
  hit <- rep(FALSE, length(dates))
  for (i in seq_len(nrow(p))) {
    hit <- hit | (dates >= p$start[i] & dates <= p$end[i])
  }
  hit
}

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

start_year <- as.integer(format(days$occurred_on, "%Y")) - (as.integer(format(days$occurred_on, "%m")) < 9)
days$school_year <- paste0(start_year, "-", substr(start_year + 1, 3, 4))

valid_age_tr$day_type <- ifelse(format(valid_age_tr$occurred_on, "%u") %in% c("6", "7"), "Weekend", "Weekday")
valid_age_tr$month    <- format(valid_age_tr$occurred_on, "%m")
n_days <- days |> count(school_day, name = "n_days")




# ----------------- Experimenting with Plotly ----------------------


# 1. I'll start by applying it to the weekday share of underage victims per hour 
minor_counts_tr <- valid_age_tr |> filter(minor, day_type == "Weekday") |> count(hour)

minor_counts_tr_plt <- ggplot(minor_counts_tr, aes(x = hour, y = n)) +
  geom_line(linewidth = 1.2, colour = orange) +
  geom_point(size = 2, colour = orange) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Hourly Underage Victim Counts on Weekdays, in Public Transportation",
       x = "Hour of occurrence", y = "Number of complaints", caption = source) +
  theme_minimal()

ggplotly(minor_counts_tr_plt)   

# It's really interesting to see the count by hovering over a specific point.
# I think this'll be especially useful in percentage/share plots, as we'll
# get to have context of both percentage and raw values at once, something
# I'd been struggling with


# 2. Adding onto the previous plot's complexity, let's see school vs summer
minor_season_tr <- valid_age_tr |> filter(day_type == "Weekday")
minor_season_tr$season <- ifelse(minor_season_tr$month %in% c("07", "08"), "Summer (Jul-Aug)", "School months")

minor_pct_season_tr <- minor_season_tr |>
  group_by(season, hour) |>
  summarise(percentage = mean(minor), .groups = "drop")

school_vs_summer <- ggplot(minor_pct_season_tr, aes(x = hour, y = percentage, colour = season, group = season)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("School months" = orange, "Summer (Jul-Aug)" = gold)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Weekday Percentage of Underage Victims (Public Transportation, School Months vs Summer)",
       x = "Hour of occurrence", y = "Percentage of Underage Victims", colour = NULL) +
  theme_minimal()

ggplotly(school_vs_summer)


# I can also add every other line plot for non-school instances and click on
# their legend to eliminate them or compare more than one at once 
# (new colors added to be able to compare them properly)


valid_age_tr <- left_join(valid_age_tr, days, by = "occurred_on")
# names(valid_age_tr)

minor_pct_all_seasons <- valid_age_tr |> 
      # Filter out Claude's fetched COVID weekends and partial days
      filter(school_day %in% c("Classes", reasons)) |>
      group_by(school_day, hour) |>
      summarise(n_all = n(), n_minor = sum(minor), 
                percentage = n_minor / n_all, .groups = "drop")


all_seasons_pct_plt <- ggplot(minor_pct_all_seasons, aes(x=hour, y=percentage ,
                                                         colour=school_day,
                                                         group=school_day)) +
          geom_line(linewidth=1.2) + geom_point(size=2)  +
          
          # Specific color choices for easier differencing
          scale_colour_manual(values = c("Classes"= gold, "Weekend"=orange,
                                         "COVID"=red, "Holiday/Recess"=dark_blue,
                                         "Summer Break"= nice_green)) +
          # Adding all hour ticks
          scale_x_continuous(breaks = 0:23) + 
  
          # Switching from 0.num to percentages
          scale_y_continuous(labels = scales::percent) + 
          
          labs

all_seasons_pct_plt
                      
                         
  



