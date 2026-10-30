library(dplyr)
library(ggplot2)

dv_blue <- "#2E75B6"
orange <- "orange2"
light_blue <- "lightblue"
dark_blue <- "darkblue"
nice_green <- "green4"

source <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"
dir.create("fig/01_exploration", showWarnings = FALSE, recursive = TRUE)

nypd <- readRDS("data/nypd_clean.rds")

nypd_ocorr <- nypd |> filter(!old_occurrence)

# 10 and 15 most frequent premise types
top10 <- nypd |> filter(!is.na(prem_typ_desc))|> count(prem_typ_desc, sort = TRUE) |> slice_head(n=10) |> pull(prem_typ_desc)
top15 <- nypd |> filter(!is.na(prem_typ_desc)) |> count(prem_typ_desc, sort = TRUE) |> slice_head(n=15) |> pull(prem_typ_desc)

# Check
#nrow(nypd)
#top10



# We begin our experiment by creating simple visualizations
# The goal here is to identify potentially significant yet straightforward
# Frequency differences between boros, years, times of day, etc


# 1. Borough Crime Frequency
boro_crimes <- nypd |> filter(!is.na(boro_nm)) |> count(boro_nm)
boro_crimes_plt <- ggplot(boro_crimes, aes(x = reorder(boro_nm, n), y = n)) +
  geom_col(fill = dv_blue) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Boro Crime Frequency", x = "Boro", y = "Number of Complaints", caption = source) + 
  theme_minimal()

ggsave("fig/01_exploration/boro_crime_frequency.png", plot = boro_crimes_plt, width = 8, height = 5, dpi = 300)


# 2. Yearly report frequency
crimes_per_year <- nypd |> filter(report_year <= 2025) |> count(report_year)
yearly_crimes_plt <- ggplot(crimes_per_year, aes(x = factor(report_year), y = n)) +
  geom_col(fill = dv_blue) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Yearly Report Frequency (2020-2025)", subtitle = "2026 omitted: only January-June available", x = "Year", y = "Number of Complaints", caption = source) +
  theme_minimal()

ggsave("fig/01_exploration/crimes_per_year.png", plot = yearly_crimes_plt, width = 8, height = 5, dpi = 300)


# 3. Top 20 most common crime types
crime_types <- count(nypd, ofns_desc, sort = TRUE) |> slice_head(n = 20)
crime_types_plt <- ggplot(crime_types, aes(x = reorder(ofns_desc, n), y = n)) +
  geom_col(fill = dv_blue) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 20 Most Reported Crimes", x = "Offence Description", y = "Number of Complaints", caption = source) +
  theme_minimal()

ggsave("fig/01_exploration/crime_types.png", plot = crime_types_plt, width = 10, height = 6, dpi = 300)


# 4. Crime frequency per hour

# Check exact times that may be placeholders for "unknown time"
# Result: 12:00:00 = 74630 complaints ; 00:00:00 = 37899 complaints

# Simple cleanup (NaN removal) & count
crimes_per_hour <- nypd_ocorr |> 
  filter(!is.na(hour)) |> 
  count(hour)

# Important to mention that this considers the approximate hour the crime
# was committed, not the time of the report!

crimes_per_hour_plt <- ggplot(crimes_per_hour, aes(x = hour, y = n)) +
  geom_line(colour = dv_blue, linewidth = 1.2) +
  geom_point(colour = dv_blue, size = 2) +
  scale_x_continuous(breaks = 0:23) + 
  
  # Small fix that forces y=0 as starting point instead of 
  # around the lowest value
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) + 
  labs(title = "NY City Crime Report Frequency Per Hour", 
       subtitle = "Unknown times may be recorded as 00:00 or 12:00",
       x = "Time of Day", y = "Number of Complaints", caption = source) +
  theme_minimal()

ggsave("fig/01_exploration/hourly_crime_frequency.png", plot = crimes_per_hour_plt, width = 9, height = 5, dpi = 300)



# ---------------------------------------------------------------- #


# This section explores more complex data relationships
# Our goal here is to start experimenting with variable connections
# and how they affect report ratios and frequency 

# Idea: We think we should use a same-colour heatmap here, 
# different colours might give off the wrong idea 


# 5. Age Group Vulnerability & premise of Crime
age_vulnerability <- nypd |> 
  filter(vic_age_group != "UNKNOWN", !is.na(vic_age_group),
         prem_typ_desc %in% top10) |>
  count(prem_typ_desc, vic_age_group)

age_vulnerability$vic_age_group <- factor(age_vulnerability$vic_age_group, levels = c("<18", "18-24", "25-44", "45-64", "65+", "UNKNOWN"))

age_vulnerability_plt <- ggplot(age_vulnerability, aes(x = reorder(prem_typ_desc, n), y = n, fill = vic_age_group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  
  # Sort of manual heatmap to help understand age increase better with no particular scale
  scale_fill_manual(values = c("<18"   = "#DCE6F0",
                               "18-24" = light_blue,
                               "25-44" = "#6E9BCB",
                               "45-64" = dv_blue,
                               "65+"   = dark_blue)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Age Group Vulnerability per Premise",
       x = "premise", y = "Number of Complaints", fill = "Victim Age", caption = source) +
  theme_minimal()

ggsave("fig/01_exploration/age_vulnerability.png", plot = age_vulnerability_plt, width = 10, height = 6, dpi = 300)


# 6. Attempted vs completed crimes per premise
att_vs_comp <- nypd |>
  
  # Get attempted-completed counts from the crm_atpt_cptd_cd column
  filter(crm_atpt_cptd_cd %in% c("ATTEMPTED", "COMPLETED"),
         prem_typ_desc %in% top15) |>
  count(prem_typ_desc, crm_atpt_cptd_cd)

att_vs_comp_plt <- ggplot(att_vs_comp, aes(n, reorder(prem_typ_desc, n),fill = crm_atpt_cptd_cd)) +
  geom_col(position = "fill", width = 0.7) +
  scale_fill_manual(values = c("COMPLETED" = orange, "ATTEMPTED" = dv_blue)) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Attempted vs Completed Crimes per Premise",
       x = "% of Complaints", y = "premise", fill = "Crime Status", caption = source) +
  theme_minimal()

ggsave("fig/01_exploration/att_vs_comp.png", plot = att_vs_comp_plt, width = 10, height = 6, dpi = 300)

# 7. Yearly crime evolution per premise (2020-2025)

# This plot has evolved a bit, it started off as top 10
# so we decided to maintain it for consistency
top_locations <- nypd |>
  filter(report_year %in% 2020:2025) |>
  count(prem_typ_desc, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(prem_typ_desc)

# Number of Complaints per year for top10 locations only
evolution <- nypd |>
  filter(report_year %in% 2020:2025, prem_typ_desc %in% top_locations) |>
  count(report_year, prem_typ_desc)
evolution$report_year <- as.factor(evolution$report_year)

# Middle years in grey (class rationale for line plots' clutter avoidance)
evolution_plt <- ggplot(evolution, aes(x = reorder(prem_typ_desc, n), y = n, fill = report_year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  
  # 2020 is green, 2025 is blue
  scale_fill_manual(values = c("2020" = nice_green, "2021" = "grey70", "2022" = "grey70",
                               "2023" = "grey70", "2024" = "grey70", "2025" = dark_blue),
                    breaks = c("2020", "2025")) +
  
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Crimes per Premise, 2020 to 2025",
       x = "Premise", y = "Number of Complaints", fill = "Year", caption = source) +
  theme_minimal()

ggsave("fig/01_exploration/premise_yearly_evolution.png", plot = evolution_plt, width = 10, height = 7, dpi = 300)


#--------------------------------------------------------------------------------------------------------------
# 8. Heatmap (Hour x Weekday) 
crimes_heatmap <- nypd_ocorr |> 
  filter(!is.na(hour), !is.na(wday)) |> 
  count(wday, hour)
heatmap_plt <- ggplot(crimes_heatmap, aes(x = hour, y = wday, fill = n)) +
  geom_tile(colour = "white") +
  coord_flip() +
  scale_fill_gradient(low = light_blue, high = dark_blue, labels = scales::comma) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Heatmap: Crime Concentration by Hour and Day of the Week",
    x = "Hour of Day",
    y = "Day of the Week",
    fill = "Complaints",
    caption = source
  ) +
  theme_minimal()
dir.create("fig/01_exploration", showWarnings = FALSE, recursive = TRUE)
ggsave("fig/01_exploration/heatmap_hour_weekday.png", plot = heatmap_plt, width = 10, height = 6, dpi = 300)


#--------------------------------------------------------------------------------------------------------
# 9. Offence level by borough
crimes_severity <- nypd |> 
  filter(!is.na(boro_nm), !is.na(law_cat_cd)) |> 
  count(boro_nm, law_cat_cd)
crimes_severity$law_cat_cd

severity_plt <- ggplot(crimes_severity, aes(x = reorder(boro_nm, n, sum), y = n, fill = law_cat_cd)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) + # Y axis must start at 0
  scale_fill_manual(values = c(
    "FELONY"      = orange, 
    "MISDEMEANOR" = dv_blue, 
    "VIOLATION"   = nice_green  
  )) +
  labs(
    title = "Crime Volume by Offence Level and Borough",
    subtitle = "Absolute comparison of Felonies, Misdemeanors and Violations",
    x = "Borough",
    y = "Number of Complaints",
    fill = "Offence Level", 
    caption = source
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )
ggsave("fig/01_exploration/severity_by_borough.png", plot = severity_plt, width = 10, height = 6)

#----------------------------------------------------------------------------------------------------------------

# 10. Seasonality 
crimes_mes <- nypd_ocorr |> 
  filter(occurred_on >= as.Date("2020-01-01"), occurred_on <= as.Date("2025-12-31")) |>
  count(mes = as.integer(format(occurred_on, "%m")))
    
grafico_mes <- ggplot(crimes_mes, aes(x = mes, y = n)) +
  geom_line(colour = dv_blue, linewidth = 1.2) +
  geom_point(colour = dv_blue, size = 2.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  labs(
    title = "Complaints by Month of Occurrence",
    x = NULL,
    y = "Complaints",
    caption = source
  ) +
  theme_minimal()
ggsave("fig/01_exploration/monthly_seasonality.png", plot = grafico_mes, width = 9, height = 5, dpi = 300)

