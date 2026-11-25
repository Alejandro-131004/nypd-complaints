# =====================================================================
# Test: can H2, H3 and H4 be compared at the same unit (precinct)?
# Standalone: rebuilds the three precinct tables with the SAME definitions
# as each member's script (copied from them, source named in each part),
# so the full scripts do not need to be run.
#
# For each hypothesis, two shares per precinct:
#   share_total   = the precinct's share of ALL the complaints of that setting
#   share_measure = the precinct's share of the specific problem
#
#   H2: all complaints               vs assaults on officers
#   H3: all public transport victims vs underage victims at 7h/15h on class days
#   H4: all subway complaints        vs subway felonies
# =====================================================================
library(dplyr)

shares <- function(df, h) {
  df |>
    mutate(hypothesis    = h,
           share_total   = n_total / sum(n_total),
           share_measure = n_measure / sum(n_measure),
           ratio         = share_measure / share_total,
           rank_total    = min_rank(desc(share_total)),
           rank_measure  = min_rank(desc(share_measure)))
}

nypd_clean <- readRDS("data/nypd_clean.rds")

# ---------------------------------------------------------------------
# H2 (from R/Hypothesis_2_ale.R, Check 12): all complaints vs assaults on officers, 2020-2025,
# under NYPD jurisdiction only, as in the revised H2
h2 <- nypd_clean |>
  filter(report_year %in% 2020:2025, !is.na(addr_pct_cd), juris_desc %in% "N.Y. POLICE DEPT") |>
  group_by(addr_pct_cd) |>
  summarise(n_total   = n(),
            n_measure = sum(pd_desc %in% "ASSAULT POLICE/PEACE OFFICER"),
            .groups = "drop") |>
  shares("H2")

# ---------------------------------------------------------------------
# H4 (from R/Hypothesis_4.R): subway complaints vs subway felonies, 2024-2025(the years of the station ranking)
h4 <- nypd_clean |>
  filter(report_year %in% 2024:2025, prem_typ_desc %in% "TRANSIT - NYC SUBWAY",
         !is.na(addr_pct_cd)) |>
  group_by(addr_pct_cd) |>
  summarise(n_total   = n(),
            n_measure = sum(law_cat_cd %in% "FELONY"),
            .groups = "drop") |>
  shares("H4")

rm(nypd_clean); gc()

# ---------------------------------------------------------------------
# H3 (from R/02_rafa_hypothesis_revisited.R, parts 0 and 3.2):
# public transport victims with a valid age vs underage victims at 7h/15h on class days
nypd_raw <- readRDS("data/nypd_2020_2026.rds") |> filter(!old_occurrence)

valid_ages <- c("<18", "18-24", "25-44", "45-64", "65+")
young_tr <- nypd_raw |>
  filter(grepl("TRANSIT|^BUS ", prem_typ_desc), !is.na(hour), !is.na(occurred_on),
         vic_age_group %in% valid_ages) |>
  mutate(minor = vic_age_group == "<18", occurred_on = as.Date(occurred_on)) |>
  filter(occurred_on >= as.Date("2020-01-01"))
rm(nypd_raw); gc()

cal <- read.csv("docs/nyc_school_calendar.csv")
cal$start <- as.Date(cal$start)
cal$end   <- as.Date(cal$end)
in_type <- function(dates, which_type) {
  p   <- cal[cal$type == which_type, ]
  hit <- rep(FALSE, length(dates))
  for (i in seq_len(nrow(p))) hit <- hit | (dates >= p$start[i] & dates <= p$end[i])
  hit
}
days <- data.frame(occurred_on = seq(as.Date("2020-01-01"), as.Date("2025-12-31"), by = "day"))
weekend <- format(days$occurred_on, "%u") %in% c("6", "7")
days$school_day <- case_when(
  in_type(days$occurred_on, "covid") & weekend ~ "COVID weekend",
  in_type(days$occurred_on, "covid")           ~ "COVID",
  weekend                                      ~ "Weekend",
  in_type(days$occurred_on, "partial")         ~ "Partial day",
  in_type(days$occurred_on, "break")           ~ "Holiday/Recess",
  in_type(days$occurred_on, "remote_day")      ~ "Holiday/Recess",
  in_type(days$occurred_on, "school_year")     ~ "Classes",
  TRUE                                         ~ "Summer Break"
)

h3 <- young_tr |>
  left_join(days, by = "occurred_on") |>
  mutate(peak = minor & school_day %in% "Classes" & hour %in% c(7, 15)) |>
  filter(!is.na(addr_pct_cd)) |>
  group_by(addr_pct_cd) |>
  summarise(n_total = n(), n_measure = sum(peak), .groups = "drop") |>
  shares("H3")

# ---------------------------------------------------------------------
# The comparison
all_h <- bind_rows(h2, h3, h4)
top_n_prec <- 10

# Check: totals per hypothesis (compare with each member's own numbers)
#   H2 under NYPD jurisdiction only; H3 the n of underage victims at 7h/15h on class days
all_h |> group_by(hypothesis) |>
  summarise(total = sum(n_total), measure = sum(n_measure), .groups = "drop") |> print()
# Observed: H2 has 9,026 assaults on officers under NYPD jurisdiction (11,735 with all agencies);
# H3 has 840 underage victims at 7h/15h on class days (matches the borough totals of H3);
# H4 has 10,106 subway felonies in 2024-2025.

# 1) How small are the numbers? (few cases per precinct = fragile ratio)
all_h |>
  group_by(hypothesis) |>
  summarise(precincts           = n(),
            median_per_precinct = median(n_measure),
            precincts_under_10  = sum(n_measure < 10),
            .groups = "drop") |>
  print()
# Observed: H2 and H4 have about 90-115 cases per precinct (median) and only 1 precinct under 10.
# H3 has a median of 9 cases per precinct and 41 of 78 precincts under 10: one victim more or less
# changes a precinct's ratio by more than 10%, so the H3 ratios per precinct are fragile.

# 2) Top 10 by total vs top 10 by measure: how many precincts are in both?
all_h |>
  group_by(hypothesis) |>
  summarise(in_both_top10 = sum(rank_total <= top_n_prec & rank_measure <= top_n_prec),
            .groups = "drop") |>
  print()
# Observed: 7 (H2), 5 (H3) and 7 (H4) of the 10 precincts with the most cases are also in the top 10
# by total complaints. The total gets more than half right; the "wrong place" is in the rest.

# 3) "Hidden" precincts: in the measure's top 10 but not in the total's top 10
all_h |>
  filter(rank_measure <= top_n_prec, rank_total > top_n_prec) |>
  arrange(hypothesis, rank_measure) |>
  select(hypothesis, addr_pct_cd, n_measure, rank_total, rank_measure, ratio) |>
  print(n = Inf)
# Observed (hidden precincts):
#   * H2: 84, 5 and 48. Under NYPD jurisdiction only, precinct 41 (mostly Department of Correction)
#     is no longer hidden. The highest H2 ratios are 5 (3.01), 84 (2.93) and 6 (2.08), the three
#     precincts that meet the H2 criterion; 6 is not hidden because it is also in the total's top 10.
#   * H3: 60, 107, 75, 110, 52, 77 and 78, each with only 19 to 25 cases in six years.
#   * H4: 52, 44 and 115 (44 has a ratio below 1, 0.975: it enters by its share of felonies).
#   * Only precinct 52 is hidden in two hypotheses (H3 and H4).

# 4) Precincts in the measure's top 10 of at least two hypotheses
all_h |>
  filter(rank_measure <= top_n_prec) |>
  group_by(addr_pct_cd) |>
  summarise(n_hypotheses = n(), which = paste(hypothesis, collapse = ", "), .groups = "drop") |>
  filter(n_hypotheses >= 2) |>
  print()
# Observed: 14, 44 and 84 are in the measure's top 10 of all three hypotheses, and 40, 52, 73 and 75
# of two. But 14 and 44 are also at the top by total complaints, so being in several top 10s does not
# mean being hidden: table 3 is the one that tests the idea "the total points to the wrong place".

# 5) Wide table: the three ratios side by side (NA = no case in that hypothesis)
all_h |>
  select(addr_pct_cd, hypothesis, ratio) |>
  tidyr::pivot_wider(names_from = hypothesis, values_from = ratio, names_prefix = "ratio_") |>
  arrange(addr_pct_cd) |>
  print(n = Inf)
# Observed:
#   * NA in H4 = precincts without subway (Staten Island and parts of Queens: 105, 111, 113, 116,
#     120-123, 63). Precinct 22 (Central Park) has 0 in H3 and H4.

# =====================================================================
# Opinion (Matilde): linking the three hypotheses by the same precincts gives a weak link, because
# the hidden places are almost all different (only precinct 52 repeats) and H3 rests on few cases
# per precinct (median of 9). Linking them by the common research question, with each hypothesis
# at its own unit (H2 by precinct, H3 by borough and hour, H4 by station), holds up better.
# =====================================================================