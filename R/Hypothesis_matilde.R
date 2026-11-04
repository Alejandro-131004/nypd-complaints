# =====================================================================
# Hypothesis Analysis: Matilde
# Objective: Tactical police resource allocation based on spatial, 
#            temporal, and severity patterns of NYPD crimes.
# =====================================================================

library(dplyr)
library(ggplot2)

dir.create("fig/02_exploration/02_matilde", showWarnings = FALSE, recursive = TRUE)

# Load clean data
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd_ocorr <- nypd |> filter(!old_occurrence)

# ---------------------------------------------------------------------
# RESEARCH HYPOTHESIS:
# Reported crime in New York City does not distribute randomly; 
#  it systematically concentrates in specific boroughs, time windows, 
#  and holidays, disproportionately affecting victims based on the 
#  urban context. By auditing data quality and analyzing both high-frequency 
#  crimes and high-severity incidents, we can provide 
#  actionable intelligence to optimize NYPD tactical deployment and prevention.
# ---------------------------------------------------------------------

# Plot styling options
dv_blue <- "#2E75B6"
source_caption <- "Data: NYPD Complaint Data, NYC Open Data (2020-2026)"

# ---------------------------------------------------------------------
# 1: Summary of most frequent crimes by severity category
# ---------------------------------------------------------------------

tactical_crimes <- nypd_ocorr |>
  count(ofns_desc, law_cat_cd, sort = TRUE) |>
  head(10)

print(tactical_crimes)

g_matilde_1 <- ggplot(tactical_crimes, aes(x = reorder(ofns_desc, n), y = n, fill = law_cat_cd)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.1, size = 3, color = "black") +
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
  theme_minimal()
ggsave("fig/02_exploration/02_matilde/top_tactical_crimes.png", plot = g_matilde_1, width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------
# 2: Crime severity distribution by Borough
# ---------------------------------------------------------------------

spatial_severity <- nypd_ocorr |>
  filter(!is.na(boro_nm) & boro_nm != "") |>
  count(boro_nm, law_cat_cd)

g_matilde_2 <- ggplot(spatial_severity, aes(x = reorder(boro_nm, n), y = n, fill = law_cat_cd)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.1, size = 2.8, color = "black") +
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
  theme_minimal()

print(g_matilde_2)
ggsave("fig/02_exploration/02_matilde/crime_severity_by_borough.png", plot = g_matilde_2, width = 10, height = 6, dpi = 300)


# ---------------------------------------------------------------------
# Step 3: Crime concentration by hour of the day and severity
# ---------------------------------------------------------------------
hourly_crime <- nypd_ocorr |>
  filter(!is.na(hour)) |>
  count(hour, law_cat_cd)

g_matilde_3 <- ggplot(hourly_crime, aes(x = hour, y = n, color = law_cat_cd)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Hourly Trend of Crime Volume by Severity Category",
    subtitle = "Tactical distribution of complaints across 24-hour windows",
    x = "Hour of the Day (0 - 23)",
    y = "Number of Complaints",
    color = "Law Category",
    caption = source_caption
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23)

print(g_matilde_3)
ggsave("fig/02_exploration/02_matilde/hourly_crime_by_severity.png", plot = g_matilde_3, width = 10, height = 6, dpi = 300)
