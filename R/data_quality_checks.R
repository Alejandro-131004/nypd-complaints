# =====================================================================
# Data quality checks: missing values, placeholder values and
# values that cannot be right. Each block prints a small table.
# All output is written to a text file (and also shown in the console);
# the data are not modified.
# Data: NYPD Complaint Data, NYC Open Data
# (complaints reported from January 2020 to June 2026)
# =====================================================================

library(dplyr)
library(lubridate)
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- as.data.frame(nypd)

# Output file (overwritten every time the script runs)
out_file <- "data_quality.txt"

# Wide console so tables do not wrap in the text file
options(width = 200)

# Helper: print a section title and a short description
section <- function(title, description) {
  cat("\n\n---- ", title, " ----\n", description, "\n\n", sep = "")
}

# Start writing everything printed to the file (split = TRUE keeps console output)
sink(out_file, split = TRUE)


# ---- 1. File size ----
section("1. File size",
        "Number of rows and columns. Expected: 3 386 974 rows and 39 columns (data dictionary).")
print(dim(nypd))


# ---- 2. Duplicated complaint identifiers ----
section("2. Duplicated complaint identifiers",
        "Number of repeated cmplnt_num values. Expected: 0 (one row per complaint).")
print(sum(duplicated(nypd$cmplnt_num)))


# ---- 3. Missing values per column ----
# Three ways a value can be missing: NA, an empty string, or a placeholder text
text_cols  <- names(nypd)[sapply(nypd, is.character)]
total_rows <- nrow(nypd)

# Numeric summary (kept numeric so it can be reused later)
missing_summary <- data.frame(
  column      = names(nypd),
  type        = sapply(nypd, function(x) class(x)[1]),
  n_na        = sapply(nypd, function(x) sum(is.na(x))),
  n_empty     = sapply(nypd, function(x) if (is.character(x)) sum(x == "", na.rm = TRUE) else NA),
  n_null_text = sapply(nypd, function(x) if (is.character(x)) sum(x == "(null)", na.rm = TRUE) else NA),
  n_unknown   = sapply(nypd, function(x) if (is.character(x)) sum(x == "UNKNOWN", na.rm = TRUE) else NA),
  row.names   = NULL
) |>
  mutate(pct_any = 100 * (n_na + coalesce(n_empty, 0L) +
                            coalesce(n_null_text, 0L)) / total_rows) |>
  arrange(desc(pct_any))

# Readable version for printing:
# - "n/a" means the check does not apply (column is not text)
# - counts use thousands separators
# - very small percentages show as "<0.1" instead of "0.0"
missing_readable <- missing_summary |>
  mutate(
    across(c(n_na, n_empty, n_null_text, n_unknown),
           ~ ifelse(is.na(.x), "n/a", formatC(.x, format = "d", big.mark = ","))),
    pct_any = case_when(
      pct_any == 0  ~ "0",
      pct_any < 0.1 ~ "<0.1",
      TRUE          ~ sprintf("%.1f", pct_any)
    )
  )

section("3. Missing values per column",
        paste0("Three ways a value can be missing: NA, an empty string, or a placeholder text.\n",
               "For each column: type, NA values, empty strings, the literal text \"(null)\", the value UNKNOWN,\n",
               "and the percentage of rows with any of the first three.\n",
               "\"n/a\" means the check does not apply (column is not text)."))
print(missing_readable)

cat("\nExample rows where transit_district is a real NA:\n\n")
print(nypd |>
        filter(is.na(transit_district)) |>
        select(cmplnt_num, boro_nm, station_name, transit_district) |>
        head(10))

cat("\nExample rows where hadevelopt holds the placeholder text \"(null)\":\n\n")
print(nypd |>
        filter(hadevelopt == "(null)") |>
        select(cmplnt_num, boro_nm, hadevelopt) |>
        head(10))


# ---- 4. Dates: occurrences before 2020 ----
section("4. Dates: occurrences before 2020",
        paste0("Year of occurrence for the rows with old_occurrence = TRUE\n",
               "(mix of late reports and impossible dates)."))
print(nypd |>
        filter(old_occurrence) |>
        mutate(occurred_year = as.integer(format(occurred_on, "%Y"))) |>
        count(occurred_year, sort = FALSE))


# ---- Date column types (used by points 5 and 6) ----
section("Date column types",
        "Class of the date columns. Date = day level, no time component.")
print(c(reported_on  = class(nypd$reported_on)[1],
        occurred_on  = class(nypd$occurred_on)[1],
        cmplnt_to_dt = class(nypd$cmplnt_to_dt)[1]))

# Parse the end date (stored as text) into a Date
nypd_dates <- nypd |>
  mutate(ended_on = as_date(parse_date_time(cmplnt_to_dt,
                                            orders = c("mdy", "ymd", "ymd HMS"),
                                            quiet = TRUE)))


# ---- 5. Reported before it occurred ----
report_before <- nypd_dates |>
  filter(reported_on < occurred_on) |>
  mutate(gap_days = as.numeric(occurred_on - reported_on)) |>
  select(cmplnt_num, ofns_desc, occurred_on, reported_on, gap_days) |>
  arrange(desc(gap_days))

section("5. Dates: reported before it occurred",
        "Number of complaints whose report date is earlier than the occurrence date.")
print(nrow(report_before))
cat("\nGap in days (occurred_on - reported_on):\n")
print(summary(report_before$gap_days))
cat("\nAll cases:\n\n")
print(report_before)


# ---- 6. End before start ----
end_before <- nypd_dates |>
  filter(ended_on < occurred_on) |>
  mutate(gap_days = as.numeric(occurred_on - ended_on)) |>
  select(cmplnt_num, ofns_desc, occurred_on, ended_on, gap_days) |>
  arrange(desc(gap_days))

section("6. Dates: end before start",
        "Number of complaints whose end date (cmplnt_to_dt) is earlier than the occurrence date.")
print(nrow(end_before))
cat("\nGap in days (occurred_on - ended_on):\n")
print(summary(end_before$gap_days))
cat("\nAll cases:\n\n")
print(end_before)


# ---- 7. Times: round hours used as "unknown" ----
section("7. Times: round hours used as \"unknown\"",
        "Complaints at each exact minute around midnight, noon and 3 pm.")
print(nypd |>
        filter(cmplnt_fr_tm %in% c("23:59:00", "00:00:00", "00:01:00",
                                   "11:59:00", "12:00:00", "12:01:00",
                                   "14:59:00", "15:00:00", "15:01:00")) |>
        count(cmplnt_fr_tm))


# ---- 8. Age groups: values outside the valid list ----
valid_ages <- c("<18", "18-24", "25-44", "45-64", "65+", "UNKNOWN")

section("8. Age groups: values outside the valid list",
        "Valid values: <18, 18-24, 25-44, 45-64, 65+, UNKNOWN.")
cat("Suspect age group:\n\n")
print(nypd |>
        filter(!susp_age_group %in% valid_ages) |>
        count(susp_age_group, sort = TRUE))
cat("\nVictim age group:\n\n")
print(nypd |>
        filter(!vic_age_group %in% valid_ages) |>
        count(vic_age_group, sort = TRUE))

# ---- 9. Victim sex: values that are not people ----
section("9. Victim sex: values that are not people",
        "D = business or organisation, E = People of the State of New York.")
print(count(nypd, vic_sex, sort = TRUE))


# ---- Check what each vic_sex code really means ----
# Top 5 offense types for each victim sex code
nypd |>
  count(vic_sex, ofns_desc) |>
  group_by(vic_sex) |>
  slice_max(n, n = 5) |>
  ungroup() |>
  print(n = Inf)

# Share of UNKNOWN age and race for each victim sex code
# (organisations or the State should have almost 100% UNKNOWN)
nypd |>
  group_by(vic_sex) |>
  summarise(
    n            = n(),
    pct_age_unkn = round(100 * mean(vic_age_group == "UNKNOWN"), 1),
    pct_race_unkn = round(100 * mean(vic_race == "UNKNOWN"), 1)
  ) |>
  arrange(desc(n))

# ---- Check the detailed offense description for vic_sex = L ----

# Top 10 detailed offense descriptions when the victim sex code is L
nypd |>
  filter(vic_sex == "L") |>
  count(pd_desc, sort = TRUE) |>
  head(10)


# ---- 10. Coordinates: repeated points ----
section("10. Coordinates: repeated points",
        "The 10 most frequent exact coordinates; a huge pile on one point deserves a look.")
print(nypd |>
        filter(!is.na(latitude), !is.na(longitude)) |>
        count(latitude, longitude, sort = TRUE) |>
        head(10))

# ---- Profile the 10 most repeated coordinates ----

# Helper: most frequent value of a vector
top_value <- function(x) names(sort(table(x), decreasing = TRUE))[1]

# The 10 most repeated exact points
top_points <- nypd |>
  filter(!is.na(latitude), !is.na(longitude), latitude != 0) |>
  count(latitude, longitude, sort = TRUE) |>
  head(10)

# What happens at each point: borough, precinct, main offense, main premise,
# share of business victims, and a Google Maps link to see the place
point_profile <- nypd |>
  semi_join(top_points, by = c("latitude", "longitude")) |>
  group_by(latitude, longitude) |>
  summarise(
    n                = n(),
    boro             = top_value(boro_nm),
    precinct         = top_value(addr_pct_cd),
    top_offense      = top_value(ofns_desc),
    pct_top_offense  = round(100 * mean(ofns_desc == top_value(ofns_desc)), 1),
    top_premise      = top_value(prem_typ_desc),
    pct_top_premise  = round(100 * mean(prem_typ_desc == top_value(prem_typ_desc)), 1),
    pct_vic_business = round(100 * mean(vic_sex == "D"), 1),
    .groups = "drop"
  ) |>
  mutate(maps_link = sprintf("https://www.google.com/maps?q=%.5f,%.5f", latitude, longitude)) |>
  arrange(desc(n))

print(point_profile, width = Inf)

# ---- 11. Coordinates: outside New York City ----
section("11. Coordinates: outside New York City",
        "Number of complaints outside an approximate bounding box of the five boroughs.")
print(nypd |>
        filter(!is.na(latitude), !is.na(longitude)) |>
        filter(latitude < 40.49 | latitude > 40.92 |
                 longitude < -74.27 | longitude > -73.68) |>
        nrow())

# ---- Inspect the complaints outside the NYC bounding box ----

nypd |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  filter(latitude < 40.49 | latitude > 40.92 |
           longitude < -74.27 | longitude > -73.68) |>
  select(cmplnt_num, boro_nm, addr_pct_cd, ofns_desc, latitude, longitude) |>
  arrange(latitude)


# ---- 12. Borough and patrol borough agreement ----
section("12. Borough and patrol borough agreement",
        "Each patrol borough should belong to a single borough.")
print(nypd |>
        count(patrol_boro, boro_nm) |>
        arrange(patrol_boro, desc(n)))



# Stop writing to the file
sink()