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

source <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"
source_cal <- paste0(source, "\nSchool calendar: NYC Public Schools, 2019-20 to 2025-26 (schools.nyc.gov/calendar)")

# Loading dataset and removing old occurrences
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- subset(nypd, report_year >= 2020 & report_year <= 2025)
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)

dir.create("fig/03_interaction/underage_public_transport_interactive", showWarnings = FALSE, recursive = TRUE)


# As suggested in this week's powerpoint, I'm going to start by re-running
# my most relevant images (from ppa and after feedback) and applying ggplotly(p)

# First, I'll organize important variables to avoid cluttering down the line

# 1. I'll start with


# I'll start with the 4-panel visualisation of increase in crime at 7-9am
# and 2-6pm