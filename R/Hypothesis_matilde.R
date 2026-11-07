# =====================================================================
# Hypothesis Analysis: Matilde
# Objective: Tactical police resource allocation based on spatial, 
#            temporal, and severity patterns of NYPD crimes.
# =====================================================================

library(dplyr)
library(ggplot2)

dir.create("fig/02_exploration/02_matilde", showWarnings = FALSE, recursive = TRUE)

# Load clean data (created by R/01_clean.R)
nypd <- readRDS("data/nypd_clean.rds")
nypd_ocorr <- nypd |> filter(!old_occurrence)

# ---------------------------------------------------------------------
# RESEARCH HYPOTHESIS 1:
# Reported crime in New York City does not distribute randomly; it concentrates
# in specific boroughs and time windows, and the pattern differs by severity.
#
# Split into three testable parts:
#   H1a: some boroughs have more serious crime than their population would suggest
#        (needs borough population; to do)
#   H1b: the afternoon peak (15:00-18:00) holds once round-hour times are removed
#        (tested below: confirmed; felonies keep a larger share at night)
#   H1c: the most frequent offenses changed differently since 2020 (to do)
#
# Dropped from the first version: holidays and victim profiles. Victims are
# covered by hypotheses 2 and 3.
#
# Decision it supports: when and where to reinforce NYPD patrols.
# ---------------------------------------------------------------------

# Plot styling options
dv_blue <- "#2E75B6"
source_caption <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"

# colorblind-friendly palette
tactical_colors <- c(
  "FELONY"      = "#D55E00", 
  "MISDEMEANOR" = "#0072B2", 
  "VIOLATION"   = "#009E73" 
)

# ---------------------------------------------------------------------
# 1: Summary of most frequent crimes by severity category
# ---------------------------------------------------------------------

tactical_crimes <- nypd_ocorr |>
  count(ofns_desc, law_cat_cd, sort = TRUE) |>
  head(10)

print(tactical_crimes)

g_matilde_1 <- ggplot(tactical_crimes, aes(x = reorder(ofns_desc, n), y = n, fill = law_cat_cd)) +
  geom_col() +
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 3, color = "black") +
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
  scale_fill_manual(values = tactical_colors) +
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
  geom_text(aes(label = scales::comma(n)), position = position_dodge(width = 0.9), hjust = -0.1, size = 2.8, color = "black") +
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
  scale_fill_manual(values = tactical_colors) +
  # coord_flip puts FELONY at the bottom of each group, so reverse the legend to match the bars
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal()

print(g_matilde_2)
ggsave("fig/02_exploration/02_matilde/crime_severity_by_borough.png", plot = g_matilde_2, width = 10, height = 6, dpi = 300)


# ---------------------------------------------------------------------
# 3: Crime concentration by hour of the day and severity
# ---------------------------------------------------------------------
hourly_crime <- nypd_ocorr |>
  filter(!is.na(hour)) |>
  count(hour, law_cat_cd)

g_matilde_3 <- ggplot(hourly_crime, aes(x = hour, y = n, color = law_cat_cd)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Hourly Trend of Crime Volume by Severity Category",
    subtitle = "Tactical distribution of complaints across 24-hour windows\nSpikes at 00h and 12h are partly unknown times recorded as 00:00 / 12:00",
    x = "Hour of the Day (0 - 23)",
    y = "Number of Complaints",
    color = "Law Category",
    caption = source_caption
  ) +
  scale_color_manual(values = tactical_colors) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma)

print(g_matilde_3)
ggsave("fig/02_exploration/02_matilde/hourly_crime_by_severity.png", plot = g_matilde_3, width = 10, height = 6, dpi = 300)



# ---------------------------------------------------------------------
# H1b: does the afternoon peak hold without the round-hour times?
# Complaints recorded at exactly 00:00 or 12:00 are removed, because they may stand
# for an unknown time. Each line is the share of its own law category, so the three
# categories can be compared by the shape of their daily cycle, not by their volume.
# ---------------------------------------------------------------------
hourly_share <- nypd_ocorr |>
  filter(!is.na(hour), !cmplnt_fr_tm %in% c("00:00:00", "12:00:00")) |>
  count(law_cat_cd, hour) |>
  group_by(law_cat_cd) |>
  mutate(pct = 100 * n / sum(n)) |>
  # Hour 12 lost most of its complaints with the 12:00 filter, so its share is not comparable
  mutate(pct = if_else(hour == 12, NA_real_, pct)) |>
  ungroup()

g_matilde_h1b <- ggplot(hourly_share, aes(x = hour, y = pct, color = law_cat_cd)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = tactical_colors) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Share of Complaints by Hour, Within Each Severity Category",
    subtitle = "Complaints recorded at exactly 00:00 or 12:00 are excluded (possible unknown times); hour 12 is not shown",
    x = "Hour of the Day (0 - 23)",
    y = "% of the Category's Complaints",
    color = "Law Category",
    caption = source_caption
  ) +
  theme_minimal()

print(g_matilde_h1b)
ggsave("fig/02_exploration/02_matilde/h1b_hourly_share_no_round_hours.png",
       plot = g_matilde_h1b, width = 10, height = 6, dpi = 300)
