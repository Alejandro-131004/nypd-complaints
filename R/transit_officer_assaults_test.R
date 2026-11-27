# =====================================================================
# Test: are there enough assaults on officers in public transport for the
# H2 criterion (ratio >= 1.5 in at least 5 of the 6 years) to hold there?
# Standalone: same definitions as R/Hypothesis_2_ale.R and the H3 transit rule.
# =====================================================================
library(dplyr)

years       <- 2020:2025

nypd <- readRDS("data/nypd_clean.rds") |>
  filter(!old_occurrence, report_year %in% years)

is_transit <- function(prem) grepl("TRANSIT|^BUS ", prem)   # same rule as H3

# =====================================================================
# Assaults on officers in public transport
# =====================================================================
assault_pd <- "ASSAULT POLICE/PEACE OFFICER"

#1. How many assaults on officers happen in transit, and under which jurisdiction?
#     (H2 uses NYPD jurisdiction only; subway incidents may be recorded under another one)
nypd |>
  filter(pd_desc %in% assault_pd) |>
  mutate(place = if_else(is_transit(prem_typ_desc), "Transit", "Elsewhere")) |>
  count(place, juris_desc, sort = TRUE) |>
  print()   # data.frame (01_clean.R)
# Observed:
#   * 897 assaults on officers happen in public transport in 2020-2025, about 7% of all of them.
#   * 649 of the 897 (72%) are recorded under N.Y. TRANSIT POLICE, 104 under Port Authority,
#     99 under MTA Police and only 29 under N.Y. POLICE DEPT. With the H2 filter (N.Y. POLICE DEPT
#     only), transit keeps 29 assaults and practically disappears.

#2. Assaults on officers in transit per year (all jurisdictions)
nypd |>
  filter(pd_desc %in% assault_pd, is_transit(prem_typ_desc)) |>
  count(report_year) |>
  print()
# Observed: 67 (2020), 84, 127, 144, 247 and 228 (2025): more than three times more in 2025 than in
# 2020 (assaults on officers in general about doubled). Caution: in the same years fare evasion and
# transit-rule complaints in the subway went from 17 to 16,934 (H4), so there were many more police
# stops in the subway; the rise may partly follow that, and the data cannot separate the two.

#3. The H2 criterion, inside transit only (from R/Hypothesis_2_ale.R, Check 12):
#     per precinct and year, ratio = share of transit assaults / share of all transit complaints;
#     concentrated = ratio >= 1.5 in at least 5 of the 6 years; only precincts with >= 50 assaults.
#     All jurisdictions are kept here, because transit may not be under "N.Y. POLICE DEPT".
transit <- nypd |>
  filter(is_transit(prem_typ_desc), !is.na(addr_pct_cd)) |>
  mutate(is_assault = pd_desc %in% assault_pd)

transit_year <- transit |>
  group_by(report_year, addr_pct_cd) |>
  summarise(complaints = n(), assaults = sum(is_assault), .groups = "drop") |>
  group_by(report_year) |>
  mutate(ratio = (assaults / sum(assaults)) / (complaints / sum(complaints))) |>
  ungroup()

transit_total <- transit_year |>
  group_by(addr_pct_cd) |>
  summarise(assaults_6y   = sum(assaults),
            years_ratio_15 = sum(ratio >= 1.5, na.rm = TRUE),
            .groups = "drop")

# How many precincts have at least 50 transit assaults in six years?
sum(transit_total$assaults_6y >= 50)

# Precincts that meet the H2 criterion inside transit
transit_total |>
  filter(assaults_6y >= 50) |>
  arrange(desc(years_ratio_15), desc(assaults_6y)) |>
  print(n = 20)
# Observed: only 1 precinct has at least 50 transit assaults in six years (the minimum used in H2):
# precinct 14, with 187, which reaches a ratio of 1.5 in 5 of the 6 years. With a single precinct
# that can be evaluated, there is no "small group of precincts" to compare.

# =====================================================================
# Opinion (Matilde): assaults on officers in transit are only about 7% of the total (897) and only
# one precinct can be evaluated, so H2 does not fit well in an "everything in public transport" link.
# The rise in transit (more than three times since 2020) is a relevant point, so the option holds
# only in part, and little for H2: the rise can be context, not the H2 criterion.