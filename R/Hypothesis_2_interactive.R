#==================================================================================================================
# Hypothesis 2: interactive figure (checkpoint 3)
#==================================================================================================================

# The ratio per year under NYPD jurisdiction (Check 12 of Hypothesis_2_ale.R), made interactive with plotly.
# Hovering over a cell shows the precinct, the year, the number of assaults, the number of complaints and
# the ratio, so the reader can see whether a high ratio comes from many cases or from very few.

library(dplyr)
library(ggplot2)
library(plotly)

dv_blue <- "#2E75B6"

nypd <- readRDS("data/nypd_clean.rds")

police_fig_dir <- "fig/02_exploration/02_ale/02_police_victims"
dir.create(police_fig_dir, showWarnings = FALSE, recursive = TRUE)


# Same settings as Check 12
min_assaults <- 50
ratio_limit <- 1.5

# Complaints under NYPD jurisdiction, 2020 to 2025
nypd_police <- nypd |>
  filter(report_year %in% 2020:2025, juris_desc %in% "N.Y. POLICE DEPT", !is.na(addr_pct_cd)) |>
  mutate(is_assault = pd_desc %in% "ASSAULT POLICE/PEACE OFFICER")


# A ratio above 1 means the area has more assaults than its total complaints would suggest
add_shares <- function(data) {
  data |>
    mutate(
      pct_assaults = round(100 * assaults / sum(assaults), 2),
      pct_all = round(100 * complaints / sum(complaints), 2),
      ratio = round(pct_assaults / pct_all, 2)
    ) |>
    arrange(desc(assaults))
}

# Ratio over the whole period, to choose the 15 precincts shown
nypd_precincts <- nypd_police |>
  group_by(addr_pct_cd) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  add_shares()

nypd_top_ratio <- nypd_precincts |>
  filter(assaults >= min_assaults) |>
  slice_max(ratio, n = 15)

# Ratio per precinct and year
nypd_yearly <- nypd_police |>
  group_by(report_year, addr_pct_cd) |>
  summarise(complaints = n(), assaults = sum(is_assault), .groups = "drop") |>
  group_by(report_year) |>
  add_shares() |>
  ungroup()

n_years <- n_distinct(nypd_yearly$report_year)

# Years with a ratio of at least 1.5 for each precinct
years_above <- nypd_yearly |>
  group_by(addr_pct_cd) |>
  summarise(years_above = sum(ratio >= ratio_limit))

# Data for the figure, with the text shown when hovering over a cell
ratio_year_data <- nypd_yearly |>
  filter(addr_pct_cd %in% nypd_top_ratio$addr_pct_cd) |>
  left_join(years_above, by = "addr_pct_cd") |>
  mutate(
    precinct_label = paste0(addr_pct_cd, " (", years_above, " of ", n_years, ")"),
    hover = paste0(
      "Precinct ", addr_pct_cd, ", ", report_year,
      "<br>Assaults on officers: ", assaults,
      "<br>All complaints: ", format(complaints, big.mark = ","),
      "<br>Ratio: ", ratio
    )
  )

# The same heatmap as Check 12, with the hover text added to aes()
ratio_year_plt <- ggplot(ratio_year_data,
                         aes(x = factor(report_year), y = reorder(precinct_label, ratio, FUN = mean),
                             fill = ratio, text = hover)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(ratio, 1)), size = 3.2) +
  scale_fill_gradient2(low = dv_blue, mid = "white", high = "#C0392B", midpoint = 1) +
  labs(
    title = "Ratio of Assaults on Officers to All Complaints per Year, NYPD Jurisdiction Only",
    x = "Report Year",
    y = "Precint",
    fill = "Ratio"
  ) +
  theme_minimal()

# Interactive version: only the hover text is shown in the tooltip
ratio_year_interactive <- ggplotly(ratio_year_plt, tooltip = "text")

ratio_year_interactive

# Save as an HTML file that opens in any browser
htmlwidgets::saveWidget(ratio_year_interactive,
                        file.path(police_fig_dir, "assaults_ratio_by_year_nypd_interactive.html"))



# The HTML already contains everything, so the support folder is not needed
unlink(file.path(police_fig_dir, "assaults_ratio_by_year_nypd_interactive_files"), recursive = TRUE)







