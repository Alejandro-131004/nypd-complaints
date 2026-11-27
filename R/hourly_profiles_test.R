# =====================================================================
# Test: does each problem (H2, H3, H4) peak at different hours from the total of complaints?
# Standalone: same definitions as each member's script (source named in each part).
# =====================================================================
library(dplyr)

years       <- 2020:2025
round_times <- c("00:00:00", "00:01:00", "12:00:00")   # possible unknown times
# 00:01 is also excluded here: with it, subway felonies peaked at midnight (6.5% at 0h),
# and the peak disappears without it (see the H1 limitation on the excess at 00:01).

nypd <- readRDS("data/nypd_clean.rds") |>
  filter(!old_occurrence, report_year %in% years)

is_transit <- function(prem) grepl("TRANSIT|^BUS ", prem)   # same rule as H3
assault_pd <- "ASSAULT POLICE/PEACE OFFICER"   # same offence as H2

# =====================================================================
# Hourly profiles (share of each group's complaints in each hour)
# =====================================================================
# Each profile excludes the complaints at exactly 00:00 or 12:00.
nypd_h <- nypd |> filter(!is.na(hour), !cmplnt_fr_tm %in% round_times)

hour_share <- function(df, label) {
  df |>
    count(hour) |>
    mutate(group = label, pct = 100 * n / sum(n))
}

profiles <- bind_rows(
  # Total: all complaints
  hour_share(nypd_h, "Total"),
  # H2: assaults on officers, NYPD jurisdiction (as in the revised H2)
  hour_share(filter(nypd_h, pd_desc %in% assault_pd, juris_desc %in% "N.Y. POLICE DEPT"),
             "H2 assaults on officers"),
  # H4: subway complaints by group (from R/Hypothesis_4.R)
  hour_share(filter(nypd_h, prem_typ_desc %in% "TRANSIT - NYC SUBWAY"), "H4 subway, all"),
  hour_share(filter(nypd_h, prem_typ_desc %in% "TRANSIT - NYC SUBWAY", law_cat_cd %in% "FELONY"),
             "H4 subway felonies"),
  hour_share(filter(nypd_h, prem_typ_desc %in% "TRANSIT - NYC SUBWAY",
                    pd_desc %in% c("THEFT OF SERVICES, UNCLASSIFIE", "N.Y.C. TRANSIT AUTH. R&R")),
             "H4 fare evasion and rules")
)

# H3: underage victims in public transport on class days (from R/02_rafa_hypothesis_revisited.R)
nypd_raw <- readRDS("data/nypd_2020_2026.rds") |> filter(!old_occurrence)
young_tr <- nypd_raw |>
  filter(is_transit(prem_typ_desc), !is.na(hour), !is.na(occurred_on),
         !cmplnt_fr_tm %in% round_times,
         vic_age_group %in% c("<18", "18-24", "25-44", "45-64", "65+")) |>
  mutate(occurred_on = as.Date(occurred_on)) |>
  filter(occurred_on >= as.Date("2020-01-01"), occurred_on <= as.Date("2025-12-31"))
rm(nypd_raw); gc()

cal <- read.csv("docs/nyc_school_calendar.csv")
cal$start <- as.Date(cal$start); cal$end <- as.Date(cal$end)
in_type <- function(dates, which_type) {
  p <- cal[cal$type == which_type, ]; hit <- rep(FALSE, length(dates))
  for (i in seq_len(nrow(p))) hit <- hit | (dates >= p$start[i] & dates <= p$end[i])
  hit
}
weekend <- format(young_tr$occurred_on, "%u") %in% c("6", "7")
young_tr$class_day <- in_type(young_tr$occurred_on, "school_year") & !weekend &
  !in_type(young_tr$occurred_on, "covid") & !in_type(young_tr$occurred_on, "partial") &
  !in_type(young_tr$occurred_on, "break") & !in_type(young_tr$occurred_on, "remote_day")

profiles <- bind_rows(
  profiles,
  hour_share(filter(young_tr, class_day), "H3 transit victims, class days (all ages)"),
  hour_share(filter(young_tr, class_day, vic_age_group == "<18"), "H3 underage victims, class days")
)

#1. Peak hour and the three busiest hours of each group
profiles |>
  group_by(group) |>
  arrange(desc(pct), .by_group = TRUE) |>
  summarise(peak_hour  = first(hour),
            top3_hours = paste(head(hour, 3), collapse = ", "),
            peak_pct   = round(first(pct), 1),
            n          = sum(n),
            .groups = "drop") |>
  print(n = Inf)
# Observed:
#   * Total: peak at 17h (6.3%); top hours 17h, 15h, 16h (afternoon).
#   * H2 assaults on officers: peak at 22h (6.8%); top hours 22h, 19h, 21h (night).
#   * H3 underage victims on class days: peak at 15h with 21.9% of their complaints in one hour,
#     and a second, morning peak at 7h-8h (about 10% and 9%, wide table) that no other group has.
#     Transit victims of all ages peak at 15h too, but with only 8.9%.
#   * H4: fare evasion and rules peak at 15h (10%); subway felonies now peak at 15h (6.6%), with the
#     same top hours as the total (15h, 16h, 17h). Their midnight peak disappeared once 00:01 was excluded.

#2. Share of each group in the windows used by the hypotheses
profiles |>
  group_by(group) |>
  summarise(night_19_04   = round(sum(pct[hour >= 19 | hour <= 4]), 1),  # H2 window (19:00-04:59)
            school_7_15   = round(sum(pct[hour %in% c(7, 15)]), 1),       # H3 hours
            afternoon_15_18 = round(sum(pct[hour %in% 15:18]), 1),         # H1.2 window
            .groups = "drop") |>
  print(n = Inf)
# Observed:
#   * Night (19:00-04:59): H2 48.6% against 38.0% for the total, about 10 points more.
#     Subway felonies 40.0%, close to the total and to an even spread (41.7%).
#   * 7h + 15h: underage victims 31.9% against 8.8% for the total, almost four times more (transit victims of all ages: 13.9%).
#   * 15:00-18:59: underage victims 45.9%, fare evasion 31.7%, total 25.2%; H2 22.8%, below the total.
# For reference: if complaints were spread evenly, 19:00-04:59 = 41.7%, 7h + 15h = 8.3%, 15-18 = 16.7%

#3 How similar is each profile to the total? (correlation of the 24 hourly shares; 1 = same shape)
total_profile <- profiles |> filter(group == "Total") |> select(hour, total = pct)
profiles |>
  filter(group != "Total") |>
  left_join(total_profile, by = "hour") |>
  group_by(group) |>
  summarise(cor_with_total = round(cor(pct, total), 2), .groups = "drop") |>
  print()
# Observed (first run, before excluding 00:01):
#   * Underage victims 0.51: the profile most different from the total.
#   * Subway felonies 0.74, transit victims of all ages 0.76, H2 0.79, fare evasion 0.82.
#   * All subway complaints 0.86: the closest to the total, probably because it mixes every
#     offence, as the total does; fare evasion is also close to the total.

#4. Wide table: the hourly shares side by side
profiles_wide <- profiles |>
  select(hour, group, pct) |>
  tidyr::pivot_wider(names_from = group, values_from = pct) |>
  arrange(hour)
print(profiles_wide, n = Inf, width = Inf)

# =====================================================================
# Opinion (Matilde): H2 and H3 have their own "when": assaults on officers peak at night and
# underage victims at 7h-8h and 15h on class days, both clearly apart from the afternoon peak of
# the total. Subway felonies do not: once 00:01 is excluded, their peak moves to 15h, like the total.
# Linking by the "when" holds for H2 and H3; H4 joins better by the "where" (the stations that the
# total hides) than by the hour.
# =====================================================================