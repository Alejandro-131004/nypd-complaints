#==================================================================================================================
# Setup shared by all hypotheses
#==================================================================================================================

library(dplyr)
library(ggplot2)

dv_blue <- "#2E75B6"
orange <- "orange2"

source <- "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)"

nypd <- readRDS("data/nypd_clean.rds")

# Values that do not represent a real category
not_valid <- c("", "(null)")


#==================================================================================================================
# Hypothesis nº1
#==================================================================================================================

# ====================================================================================
# My first idea was to check if adults in some boroughs use children to commit
# crimes, for example in drug possession or trafficking. The dataset does not record
# whether an adult recruited or directed a suspect, so we cannot test this. Instead,
# I looked at the share of drug complaints with a suspect recorded as under 18 in
# each borough and precinct.
#
# This did not give us enough cases. Only 481 of the 73,125 drug complaints with a
# known suspect age (0.66%) have a suspect under 18. The boroughs go from 10 to 198
# cases, so we should be careful with their differences, and the precinct ranking is
# not reliable. Precinct 111, for example, ranks second with just 1 case out of 53.
#
# Since there are so few drug cases, my next idea is to repeat the analysis with
# all crimes. I want to see if the share of complaints with a suspect recorded
# as under 18 changes between boroughs and precincts.
#
# We need to be careful when reading these results. A borough may have a higher share
# just because it has more of certain types of crime, so I also want to compare the
# most common offences separately. Suspect age may also be unknown more often across
# all crimes, so I should check how many ages are missing before comparing areas.
#
# These numbers only tell us about reported complaints. They do not show that adults
# directed these suspects, and some of the differences may come from the way police
# work or record cases in each area.

# The problem is that there are no police stations for young people (underage),
# so the solution for the Mayor or the NYPD would essentially be the same
# as for people aged 18 or over. I think it's better to change hypothesis,
# unless I search for external sources that could help the Mayor put police stations
# that handle problems with young people in those specific boroughs that have a higher
# share of complaints with underage suspects.
# ====================================================================================

fig_dir <- "fig/02_exploration/02_ale/02_youth_complaints"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Minimum complaints with a known suspect age for a precinct to be ranked
min_known_age_drugs <- 50
min_known_age_all <- 500

# Number of precincts shown in the plots
n_top_precincts <- 15

# Number of offence types compared across boroughs
n_top_offences <- 6

# Known suspect age groups (UNKNOWN and invalid values are excluded)
known_ages <- c("<18", "18-24", "25-44", "45-64", "65+")

all_complaints <- nypd |>
  mutate(age_known = susp_age_group %in% known_ages,
         under_18 = susp_age_group %in% "<18")

drug_complaints <- all_complaints |>
  filter(ofns_desc == "DANGEROUS DRUGS")


# Summarise complaints, age coverage and the under-18 share for each group
summarise_youth <- function(data) {
  data |>
    summarise(
      complaints = n(),
      known_age = sum(age_known),
      under_18 = sum(under_18),
      .groups = "drop"
    ) |>
    mutate(
      pct_age_unknown = round(100 * (1 - known_age / complaints), 1),
      pct_under_18 = round(100 * under_18 / known_age, 2)
    )
}

# Under-18 share for each borough
summarise_boroughs <- function(data) {
  data |>
    filter(!is.na(boro_nm), !boro_nm %in% not_valid) |>
    group_by(boro_nm) |>
    summarise_youth() |>
    arrange(desc(pct_under_18))
}

# Under-18 share for each precinct, labelled with its most frequent borough
summarise_precincts <- function(data) {
  precinct_boro <- data |>
    filter(!is.na(addr_pct_cd), !is.na(boro_nm), !boro_nm %in% not_valid) |>
    count(addr_pct_cd, boro_nm) |>
    group_by(addr_pct_cd) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(addr_pct_cd, boro_nm)
  
  data |>
    filter(!is.na(addr_pct_cd)) |>
    group_by(addr_pct_cd) |>
    summarise_youth() |>
    left_join(precinct_boro, by = "addr_pct_cd") |>
    mutate(precinct_label = paste0("Precinct ", addr_pct_cd, " (", boro_nm, ")")) |>
    arrange(desc(pct_under_18))
}

# Print how many precincts have enough complaints, then keep only those
filter_precincts <- function(by_precinct, min_known_age) {
  cat("Precincts in total:", nrow(by_precinct), "\n")
  cat("Precincts with at least", min_known_age, "known ages:",
      sum(by_precinct$known_age >= min_known_age), "\n")
  print(summary(by_precinct$known_age))
  
  by_precinct |> filter(known_age >= min_known_age)
}

# Horizontal bars of the under-18 share, labelled with the counts behind each bar
plot_share <- function(data, group_var, title, subtitle, y_title, city_share = NULL) {
  plt <- ggplot(data, aes(x = pct_under_18, y = reorder({{ group_var }}, pct_under_18))) +
    geom_col(fill = dv_blue)
  
  if (!is.null(city_share)) {
    plt <- plt + geom_vline(xintercept = city_share, colour = orange, linewidth = 1, linetype = "dashed")
  }
  
  plt +
    geom_text(aes(label = paste0(scales::comma(under_18), " of ", scales::comma(known_age))),
              hjust = -0.1, size = 3.2) +
    scale_x_continuous(labels = scales::label_percent(scale = 1), expand = expansion(mult = c(0, 0.25))) +
    labs(title = title, subtitle = subtitle, x = "% of Suspects Recorded as Under 18",
         y = y_title, caption = source) +
    theme_minimal()
}

# Subtitle text explaining the dashed line
city_note <- function(city_share) {
  paste0("Suspects with a known age only. Dashed line = % of suspects under 18 in all of New York City (", city_share, "%)")
}


# ---------------------------------------------------------------- #
# Part 1. Drug complaints only (too few cases, kept as a record)
# ---------------------------------------------------------------- #

drug_city <- summarise_youth(drug_complaints)
print(drug_city)

drug_boroughs <- summarise_boroughs(drug_complaints)
print(drug_boroughs)

drug_borough_plt <- plot_share(drug_boroughs, boro_nm,
                               "Drug Complaints with a Suspect Under 18, by Borough",
                               city_note(drug_city$pct_under_18), "Borough",
                               city_share = drug_city$pct_under_18)

ggsave(file.path(fig_dir, "drugs_by_borough.png"), plot = drug_borough_plt, width = 10, height = 5, dpi = 300)

drug_precincts <- summarise_precincts(drug_complaints) |>
  filter_precincts(min_known_age_drugs)
print(drug_precincts)

# The dashed line is left out because every precinct shown is above the average
drug_precinct_plt <- plot_share(slice_head(drug_precincts, n = n_top_precincts), precinct_label,
                                "Precincts with the Highest Share of Drug Complaints with a Suspect Under 18",
                                paste0("Precincts with at least ", min_known_age_drugs, " known suspect ages"),
                                "Precinct")

ggsave(file.path(fig_dir, "drugs_by_precinct.png"), plot = drug_precinct_plt, width = 10, height = 7, dpi = 300)


# ---------------------------------------------------------------- #
# Part 2. All crimes
# ---------------------------------------------------------------- #

all_city <- summarise_youth(all_complaints)
print(all_city)

# Check pct_age_unknown here before comparing the boroughs
all_boroughs <- summarise_boroughs(all_complaints)
print(all_boroughs)

all_borough_plt <- plot_share(all_boroughs, boro_nm,
                              "Complaints with a Suspect Under 18, by Borough (All Crimes)",
                              city_note(all_city$pct_under_18), "Borough",
                              city_share = all_city$pct_under_18)

ggsave(file.path(fig_dir, "all_crimes_by_borough.png"), plot = all_borough_plt, width = 10, height = 5, dpi = 300)

all_precincts <- summarise_precincts(all_complaints) |>
  filter_precincts(min_known_age_all)
print(all_precincts)

all_precinct_plt <- plot_share(slice_head(all_precincts, n = n_top_precincts), precinct_label,
                               "Precincts with the Highest Share of Complaints with a Suspect Under 18 (All Crimes)",
                               paste0("Precincts with at least ", min_known_age_all, " known suspect ages"),
                               "Precinct")

ggsave(file.path(fig_dir, "all_crimes_by_precinct.png"), plot = all_precinct_plt, width = 10, height = 7, dpi = 300)


# ---------------------------------------------------------------- #
# Part 3. Most common offences compared across boroughs
# ---------------------------------------------------------------- #

# Comparing each offence separately shows if the borough differences
# in Part 2 could come from the offence mix
top_offences <- all_complaints |>
  filter(!is.na(ofns_desc), !ofns_desc %in% not_valid) |>
  count(ofns_desc, sort = TRUE) |>
  slice_head(n = n_top_offences) |>
  pull(ofns_desc)

offence_boroughs <- all_complaints |>
  filter(ofns_desc %in% top_offences, !is.na(boro_nm), !boro_nm %in% not_valid) |>
  group_by(ofns_desc, boro_nm) |>
  summarise_youth() |>
  mutate(
    ofns_desc = factor(ofns_desc, levels = top_offences),
    boro_nm = factor(boro_nm, levels = rev(all_boroughs$boro_nm))
  ) |>
  arrange(ofns_desc, desc(pct_under_18))

print(offence_boroughs, n = Inf)

offence_plt <- ggplot(offence_boroughs, aes(x = pct_under_18, y = boro_nm)) +
  geom_col(fill = dv_blue) +
  geom_text(aes(label = paste0(scales::comma(under_18), " of ", scales::comma(known_age))),
            hjust = -0.1, size = 2.8) +
  facet_wrap(~ ofns_desc, ncol = 2, labeller = label_wrap_gen(40)) +
  scale_x_continuous(labels = scales::label_percent(scale = 1), expand = expansion(mult = c(0, 0.45))) +
  labs(
    title = "Complaints with a Suspect Under 18, by Borough and Offence",
    subtitle = paste0("The ", n_top_offences, " most common offences. Suspects with a known age only"),
    x = "% of Suspects Recorded as Under 18",
    y = "Borough",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(fig_dir, "top_offences_by_borough.png"), plot = offence_plt, width = 11, height = 8, dpi = 300)


# ---------------------------------------------------------------- #
# Small checks for hypothesis nº1
# ---------------------------------------------------------------- #

# Difference and ratio between each area's share and the citywide share
compare_to_city <- function(data, city_share) {
  data |>
    mutate(
      diff_vs_city = round(pct_under_18 - city_share, 2),
      ratio_vs_city = round(pct_under_18 / city_share, 2)
    )
}

print(compare_to_city(all_boroughs, all_city$pct_under_18))
print(compare_to_city(all_precincts, all_city$pct_under_18), n = Inf, width = Inf)

# Text to search for in the columns below
youth_pattern <- "YOUTH|JUVENILE|SCHOOL|EDUCATION"

# All agencies with jurisdiction
nypd |>
  count(juris_desc, sort = TRUE) |>
  as_tibble() |>
  print(n = Inf)

# Agencies whose name matches the search
nypd |>
  filter(grepl(youth_pattern, juris_desc, ignore.case = TRUE)) |>
  count(juris_desc, sort = TRUE)

# Premise types whose name matches the search
nypd |>
  filter(grepl(youth_pattern, prem_typ_desc, ignore.case = TRUE)) |>
  count(prem_typ_desc, sort = TRUE)


#==================================================================================================================
# Hypothesis nº2
#==================================================================================================================

# =================================================================================================================
# My second idea involved the gender category. They're divided into M (Masculine), 
# F (Feminine), U (Unknown), D (Business or Organisation), E (The People of the State of New York),
# so neither D and E are a person. 
# I also found an interesting one in data_quality_checks.R, which is L, and that stands for complaints that appear
# to involve police officers as victims.
# So the initial hypothesis for now is Complaints that appear to involve police officers as victims 
# are concentrated in certain boroughs and precincts
# =================================================================================================================

# L only starts in 2022 and never covers all assaults on officers, so the analysis
# below uses the "ASSAULT POLICE/PEACE OFFICER" description from 2020 to 2025.
# 2026 is left out because it only runs until June.

police_fig_dir <- "fig/02_exploration/02_ale/02_police_victims"
dir.create(police_fig_dir, showWarnings = FALSE, recursive = TRUE)

# Minimum assaults for a precinct to appear in the ratio figure
min_assaults <- 50

# Mark the assaults and group the premises into three simple types
police <- nypd |>
  filter(report_year %in% 2020:2025) |>
  mutate(
    is_assault = pd_desc %in% "ASSAULT POLICE/PEACE OFFICER",
    place = case_when(
      prem_typ_desc %in% "STREET" ~ "Street",
      is.na(prem_typ_desc) | prem_typ_desc %in% "OTHER" ~ "Other or unknown",
      TRUE ~ "Other known place"
    )
  )

assaults <- police |>
  filter(is_assault)


# ---- Check 1. Is the L code the reason to avoid it? ----

assaults |>
  count(report_year, is_l = vic_sex %in% "L") |>
  as_tibble() |>
  print(n = Inf)


# ---- Check 2. Do assaults change differently from all complaints? ----

# Index: 2020 = 100, so both lines can be compared on the same scale
yearly_totals <- police |>
  group_by(report_year) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  mutate(
    complaints_index = round(100 * complaints / first(complaints), 1),
    assaults_index = round(100 * assaults / first(assaults), 1)
  )

print(yearly_totals)

trend_plt <- ggplot(yearly_totals, aes(x = report_year)) +
  geom_line(aes(y = complaints_index, colour = "All complaints"), linewidth = 1.2) +
  geom_line(aes(y = assaults_index, colour = "Assaults on officers"), linewidth = 1.2) +
  scale_colour_manual(values = c("All complaints" = "grey60", "Assaults on officers" = dv_blue)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Assaults on Officers Compared with All Complaints",
    subtitle = "Index: 2020 = 100",
    x = "Report Year",
    y = "Index",
    colour = NULL,
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_trend.png"), plot = trend_plt, width = 9, height = 5, dpi = 300)


# ---- Check 3. Are assaults concentrated beyond complaints in general? ----

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

boroughs <- police |>
  filter(!is.na(boro_nm)) |>
  group_by(boro_nm) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  add_shares()

print(boroughs)

precincts <- police |>
  filter(!is.na(addr_pct_cd)) |>
  group_by(addr_pct_cd) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  add_shares()

print(precincts, n = Inf)

# Share of assaults and share of all complaints in the top 10 precincts
top10_precincts <- head(precincts, 10)
sum(top10_precincts$pct_assaults)
sum(top10_precincts$pct_all)

# Precincts with the highest ratio, among those with enough assaults
top_ratio <- precincts |>
  filter(assaults >= min_assaults) |>
  slice_max(ratio, n = 15)

ratio_plt <- ggplot(top_ratio, aes(x = ratio, y = reorder(addr_pct_cd, ratio))) +
  geom_col(fill = dv_blue) +
  geom_vline(xintercept = 1, colour = orange, linewidth = 1, linetype = "dashed") +
  geom_text(aes(label = paste(assaults, "assaults")), hjust = -0.1, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Precincts Where Assaults on Officers Are Most Over-Represented",
    subtitle = paste0("Ratio = share of assaults / share of all complaints. Dashed line = 1, the same share of both. At least ",
                      min_assaults, " assaults"),
    x = "Ratio",
    y = "Precinct",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_ratio.png"), plot = ratio_plt, width = 10, height = 6, dpi = 300)


# ---- Check 4. Do the same precincts stay on top each year? ----

yearly <- assaults |>
  filter(!is.na(addr_pct_cd)) |>
  count(report_year, addr_pct_cd)

yearly |>
  group_by(report_year) |>
  slice_max(n, n = 10) |>
  ungroup() |>
  count(addr_pct_cd, name = "years_in_top10", sort = TRUE) |>
  print(n = Inf)

yearly_plt <- yearly |>
  filter(addr_pct_cd %in% top10_precincts$addr_pct_cd) |>
  ggplot(aes(x = factor(report_year), y = addr_pct_cd, fill = n)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = n), colour = "white", size = 3.2) +
  scale_fill_gradient(low = "#9DC3E6", high = "darkblue") +
  labs(
    title = "Assaults on Officers per Year in the Top 10 Precincts",
    x = "Report Year",
    y = "Precinct",
    fill = "Assaults",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_by_year.png"), plot = yearly_plt, width = 10, height = 6, dpi = 300)


# ---- Check 5. Where do the assaults happen in the precincts that stand out? ----

count(assaults, place, sort = TRUE)
count(assaults, juris_desc, sort = TRUE) |> head(10)

# Exact premises in the same precincts
assaults |>
  filter(addr_pct_cd %in% top_ratio$addr_pct_cd) |>
  count(addr_pct_cd, prem_typ_desc, sort = TRUE) |>
  group_by(addr_pct_cd) |>
  slice_head(n = 3) |>
  print(n = Inf)

place_plt <- assaults |>
  filter(addr_pct_cd %in% top_ratio$addr_pct_cd) |>
  ggplot(aes(y = factor(addr_pct_cd, levels = rev(top_ratio$addr_pct_cd)), fill = place)) +
  geom_bar(position = "fill") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Street" = dv_blue, "Other known place" = orange, "Other or unknown" = "grey70")) +
  labs(
    title = "Where Assaults on Officers Happen in the Top Ratio Precincts",
    subtitle = "Precincts in the same order as the ratio figure",
    x = "% of the Precinct's Assaults",
    y = "Precinct",
    fill = "Place",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_place.png"), plot = place_plt, width = 10, height = 6, dpi = 300)


# ---- Check 6. Do assaults happen at different hours from complaints in general? ----

# Exact 00:00 and 12:00 are removed because they may stand for an unknown time
timed <- police |>
  filter(!old_occurrence, !is.na(hour), !is.na(wday),
         !cmplnt_fr_tm %in% c("00:00:00", "12:00:00"))

hourly <- timed |>
  group_by(hour) |>
  summarise(complaints = n(), assaults = sum(is_assault)) |>
  mutate(
    pct_assaults = round(100 * assaults / sum(assaults), 2),
    pct_all = round(100 * complaints / sum(complaints), 2)
  )

print(hourly, n = Inf)

hourly_plt <- ggplot(hourly, aes(x = hour)) +
  geom_line(aes(y = pct_all, colour = "All complaints"), linewidth = 1.2) +
  geom_line(aes(y = pct_assaults, colour = "Assaults on officers"), linewidth = 1.2) +
  scale_colour_manual(values = c("All complaints" = "grey60", "Assaults on officers" = dv_blue)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(limits = c(0, NA), labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Hour of Assaults on Officers Compared with All Complaints",
    subtitle = "Complaints recorded at exactly 00:00 or 12:00 are excluded",
    x = "Hour of Occurrence",
    y = "% of Complaints in Each Group",
    colour = NULL,
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_by_hour.png"), plot = hourly_plt, width = 10, height = 5, dpi = 300)

time_plt <- timed |>
  filter(is_assault) |>
  count(wday, hour) |>
  ggplot(aes(x = hour, y = wday, fill = n)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "#DCE6F0", high = "darkblue") +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Assaults on Officers by Hour and Day of the Week",
    subtitle = "Complaints recorded at exactly 00:00 or 12:00 are excluded",
    x = "Hour of Occurrence",
    y = "Day of the Week",
    fill = "Assaults",
    caption = source
  ) +
  theme_minimal()

ggsave(file.path(police_fig_dir, "assaults_hour_weekday.png"), plot = time_plt, width = 10, height = 5, dpi = 300)