#==================================================================================================================
# Setup shared by all hypotheses
#==================================================================================================================

library(dplyr)
library(ggplot2)

# Course colours (labs 2 to 4)
dv_blue   <- "#2E75B6"
dv_orange <- "#E8792F"
dv_dark   <- "#1B2A4A"

source <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"

# Clean data from R/01_clean.R ("(null)" already replaced by NA)
nypd <- readRDS("data/nypd_clean.rds")


#==================================================================================================================
# Exploration that led to hypothesis nº4
#==================================================================================================================

# =================================================================================================================
# Three figures, in the order we looked at them. Each one raised the question that the next one answers.
#   E1. Which premises changed the most from 2020 to 2025?            -> the subway, by far
#   E2. Which offences are behind the rise in the subway?             -> one offence class
#   E3. What is inside that offence class, and who records it?       -> fare evasion, by the Transit Police
# The hypothesis is written at the end of this file and tested in R/Hypothesis_subway.R.
# 2026 is left out because it only runs until June.
# =================================================================================================================

fig_dir <- "fig/03_hypothesis/hypothesis_4/exploration"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

years <- 2020:2025


# ---------------------------------------------------------------- #
# Data fix: offence classes with two spellings
# ---------------------------------------------------------------- #

# Until 2023 some offence classes were cut at 30 characters ("OTHER OFFENSES RELATED TO THEF");
# from 2024 they are written in full ("OTHER OFFENSES RELATED TO THEFT"). Counted by name, the
# class seems to start from zero in 2024. We give each class one spelling, the longest one.
ofns_names <- unique(na.omit(nypd$ofns_desc))
truncated <- ofns_names[nchar(ofns_names) == 30]

spelling_fix <- do.call(rbind, lapply(truncated, function(short) {
  long <- ofns_names[startsWith(ofns_names, short) & ofns_names != short]
  if (length(long) == 1) data.frame(short = short, long = long)
}))
print(spelling_fix)

nypd <- nypd |>
  mutate(ofns_desc = ifelse(ofns_desc %in% spelling_fix$short,
                            spelling_fix$long[match(ofns_desc, spelling_fix$short)],
                            ofns_desc))


# ---------------------------------------------------------------- #
# E1. Which premises changed the most?
# ---------------------------------------------------------------- #

# Premises with at least this many complaints in 2025 (small premises give unstable ratios)
min_premise_2025 <- 5000

premise_change <- nypd |>
  filter(report_year %in% c(2020, 2025), !is.na(prem_typ_desc)) |>
  count(prem_typ_desc, report_year) |>
  tidyr::pivot_wider(names_from = report_year, values_from = n, names_prefix = "y", values_fill = 0) |>
  filter(y2025 >= min_premise_2025) |>
  mutate(ratio = y2025 / y2020,
         highlight = prem_typ_desc == "TRANSIT - NYC SUBWAY")

print(arrange(premise_change, desc(ratio)))

subway_ratio <- premise_change$ratio[premise_change$highlight]

premise_plt <- ggplot(premise_change, aes(x = ratio, y = reorder(prem_typ_desc, ratio), fill = highlight)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 1, colour = "grey40", linetype = "dashed") +
  geom_text(aes(label = paste0("x", round(ratio, 1))), hjust = -0.15, size = 3.4) +
  scale_fill_manual(values = c(`FALSE` = "grey75", `TRUE` = dv_orange), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = paste0("Subway complaints grew x", round(subway_ratio, 1),
                   " from 2020 to 2025, more than any other premises"),
    subtitle = paste0("Complaints in 2025 divided by complaints in 2020. Premises with at least ",
                      scales::comma(min_premise_2025), " complaints in 2025. Dashed line = no change"),
    x = "Complaints in 2025 / complaints in 2020",
    y = NULL,
    caption = source
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), plot.title.position = "plot")

ggsave(file.path(fig_dir, "h4_explore_1_premise_change.png"), plot = premise_plt, width = 10, height = 6, dpi = 300, bg = "white")

# What we saw: the subway grows much more than any other premises. Question for E2: why?


# ---------------------------------------------------------------- #
# E2. Which offences are behind the rise in the subway?
# ---------------------------------------------------------------- #

# Offence classes shown: the most common in the subway in 2025
n_top_offences <- 12

subway_offences <- nypd |>
  filter(report_year %in% c(2020, 2025), prem_typ_desc %in% "TRANSIT - NYC SUBWAY", !is.na(ofns_desc)) |>
  count(ofns_desc, report_year) |>
  tidyr::pivot_wider(names_from = report_year, values_from = n, names_prefix = "y", values_fill = 0) |>
  mutate(rise = y2025 - y2020) |>
  slice_max(y2025, n = n_top_offences)

print(subway_offences)

# The offence with the largest rise, and its share of the total rise, for the title
biggest <- slice_max(subway_offences, rise, n = 1)
total_rise <- nypd |>
  filter(report_year %in% c(2020, 2025), prem_typ_desc %in% "TRANSIT - NYC SUBWAY") |>
  count(report_year) |>
  summarise(rise = n[report_year == 2025] - n[report_year == 2020]) |>
  pull(rise)
pct_biggest <- round(100 * biggest$rise / total_rise)

# One row per offence and year, for a dumbbell: two points joined by a line
dumbbell <- subway_offences |>
  tidyr::pivot_longer(c(y2020, y2025), names_to = "year", values_to = "n") |>
  mutate(year = sub("y", "", year))

offences_plt <- ggplot(dumbbell, aes(x = n, y = reorder(ofns_desc, n, max))) +
  geom_line(aes(group = ofns_desc), colour = "grey80", linewidth = 1.5) +
  geom_point(aes(colour = year), size = 3) +
  scale_colour_manual(values = c("2020" = "grey60", "2025" = dv_orange)) +
  scale_x_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(
    title = paste0("One offence class explains ", pct_biggest, "% of the rise in subway complaints: ",
                   tolower(biggest$ofns_desc)),
    subtitle = paste0("Complaints in the subway in 2020 and 2025. The ", n_top_offences,
                      " most common offence classes in 2025"),
    x = "Complaints",
    y = NULL,
    colour = NULL,
    caption = source
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top", plot.title.position = "plot")

ggsave(file.path(fig_dir, "h4_explore_2_subway_offences.png"), plot = offences_plt, width = 10, height = 6, dpi = 300, bg = "white")

# What we saw: one class ("other offenses related to theft") goes from almost nothing to the most common
# offence in the subway. Question for E3: what is inside that class?


# ---------------------------------------------------------------- #
# E3. What is inside that offence class, and who records it?
# ---------------------------------------------------------------- #

theft_class <- nypd |>
  filter(report_year %in% years, prem_typ_desc %in% "TRANSIT - NYC SUBWAY", ofns_desc %in% biggest$ofns_desc)

# The detailed descriptions inside the class, and the agency that records them
count(theft_class, pd_desc, sort = TRUE)
count(theft_class, juris_desc, sort = TRUE)

fare_pd <- "THEFT OF SERVICES, UNCLASSIFIE"
pct_fare <- round(100 * mean(theft_class$pd_desc %in% fare_pd))
pct_transit_police <- round(100 * mean(theft_class$juris_desc %in% "N.Y. TRANSIT POLICE"))

inside <- theft_class |>
  mutate(detail = ifelse(pd_desc %in% fare_pd, "Theft of services (fare evasion)", "Other descriptions")) |>
  count(report_year, detail)

inside_plt <- ggplot(inside, aes(x = factor(report_year), y = n, fill = detail)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Theft of services (fare evasion)" = dv_orange, "Other descriptions" = "grey75")) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = paste0(pct_fare, "% of this class in the subway is fare evasion, and ",
                   pct_transit_police, "% is recorded by the Transit Police"),
    subtitle = paste0("Subway complaints in the class '", tolower(biggest$ofns_desc), "', by report year"),
    x = "Report Year",
    y = "Complaints",
    fill = NULL,
    caption = source
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(), legend.position = "top", plot.title.position = "plot")

ggsave(file.path(fig_dir, "h4_explore_3_theft_class.png"), plot = inside_plt, width = 10, height = 5.5, dpi = 300, bg = "white")

# What we saw: the class is almost only fare evasion, recorded by the Transit Police. Fare evasion has no
# victim who calls the police: the complaint exists because an officer stopped someone. So the rise in the
# subway may measure how much the police enforce fares, not how much crime there is.


# =================================================================================================================
# Hypothesis nº4 (tested in R/Hypothesis_subway.R)
#
# The rise of complaints in the NYC subway since 2020 comes mostly from fare and transit-rule enforcement,
# not from serious crime. A ranking of stations by total complaints therefore points to some stations
# because of enforcement, and hides others where serious crime is high.
#
# Decision it supports: where and when to place transit patrols against violent crime, separately from
# fare enforcement.
# =================================================================================================================
