# =====================================================================
# Hypothesis Analysis: Matilde
# Objective: Tactical police resource allocation based on spatial, 
#            temporal, and severity patterns of NYPD crimes.
# =====================================================================

library(dplyr)
library(ggplot2)

dir.create("fig/02_exploration/02_matilde", showWarnings = FALSE, recursive = TRUE)

# Load clean data
nypd <- readRDS("data/nypd_clean.rds")
nypd_ocorr <- nypd |> filter(!old_occurrence)

# ---------------------------------------------------------------------
# RESEARCH HYPOTHESIS:
# "Reported crime in New York City does not distribute randomly; 
#  it systematically concentrates in specific boroughs, time windows, 
#  and holidays, disproportionately affecting victims based on the 
#  urban context. By auditing data quality and analyzing both high-frequency 
#  crimes and high-severity incidents, we can provide 
#  actionable intelligence to optimize NYPD tactical deployment and prevention."
# ---------------------------------------------------------------------

# Plot styling options
dv_blue <- "#2E75B6"
source_caption <- "Data: NYPD Complaint Data, NYC Open Data (2020-2026)"