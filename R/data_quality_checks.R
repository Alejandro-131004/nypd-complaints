# =====================================================================
# In this file we will be looking for missing values, placeholder values 
# and values that cannot be right. Each block prints a small table.
# All output is written to a text file so that we can analyze the outputs 
# better and take some notes.
# =====================================================================

library(dplyr)
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- as.data.frame(nypd)

# Output file (overwritten every time the script runs)
out_file <- "data_quality.txt"

# Wide console so tables do not wrap in the text file
options(width = 200)

# This is a helper fucntion. It print a section title and a short description
section <- function(title, description) {
  cat("\n\n---- ", title, " ----\n", description, "\n\n", sep = "")
}

# Start writing everything printed to the file (split = TRUE keeps console output)
sink(out_file, split = TRUE)


# ---- 1. File size ----
section("1. File size",
        "Number of rows and columns. Expected: 3 386 974 rows and 39 columns (data dictionary).")
print(dim(nypd))


# ---- Column types ----
section("Column types",
        "Type of each column and its first values.")
glimpse(nypd)


# ---- 2. Duplicated complaint identifiers ----
section("2. Duplicated complaint identifiers",
        "Number of repeated cmplnt_num values. Expected: 0 (one row per complaint).")
print(sum(duplicated(nypd$cmplnt_num)))


# ---- 3. Missing values per column ----
# Three ways a value can be missing: NA, an empty string, or a placeholder text
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

section("3. Missing values per column",
        "For each column: NA values, empty strings, \"(null)\", UNKNOWN, and % of rows with any of the first three.")
print(missing_summary)

# ---- 4. Dates: occurrences before 2020 ----
section("4. Dates: occurrences before 2020",
        paste0("Year of occurrence for the rows with old_occurrence = TRUE\n",
               "(mix of late reports and impossible dates)."))
print(nypd |>
        filter(old_occurrence) |>
        mutate(occurred_year = as.integer(format(occurred_on, "%Y"))) |>
        count(occurred_year, sort = FALSE))

# Convert the end date (stored as text) into a Date
nypd_dates <- nypd |>
  mutate(ended_on = as.Date(cmplnt_to_dt, format = "%Y-%m-%d"))


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
print(nypd |>
        group_by(vic_sex) |>
        summarise(
          n             = n(),
          pct_age_unkn  = round(100 * mean(vic_age_group == "UNKNOWN"), 1),
          pct_race_unkn = round(100 * mean(vic_race == "UNKNOWN"), 1)
        ) |>
        arrange(desc(n)))

# ---- Check the detailed offense description for vic_sex = L ----

# Top 10 detailed offense descriptions when the victim sex code is L
print(nypd |>
        filter(vic_sex == "L") |>
        count(pd_desc, sort = TRUE) |>
        head(10))


# ---- 10. Coordinates: repeated points ----
section("10. Coordinates: repeated points",
        "The 10 most frequent exact coordinates.")
print(nypd |>
        filter(!is.na(latitude), !is.na(longitude)) |>
        count(latitude, longitude, sort = TRUE) |>
        head(10))


# ---- 11. Coordinates: outside New York City ----
section("11. Coordinates: outside New York City",
        "Number of complaints outside an approximate bounding box of the five boroughs.")
print(nypd |>
        filter(!is.na(latitude), !is.na(longitude)) |>
        filter(latitude < 40.49 | latitude > 40.92 |
                 longitude < -74.27 | longitude > -73.68) |>
        nrow())

# ---- Inspect the complaints outside the NYC bounding box ----

print(nypd |>
        filter(!is.na(latitude), !is.na(longitude)) |>
        filter(latitude < 40.49 | latitude > 40.92 |
                 longitude < -74.27 | longitude > -73.68) |>
        select(cmplnt_num, boro_nm, addr_pct_cd, ofns_desc, latitude, longitude) |>
        arrange(latitude))


# ---- 12. Borough and patrol borough agreement ----
section("12. Borough and patrol borough agreement",
        "Each patrol borough should belong to a single borough.")
print(nypd |>
        count(patrol_boro, boro_nm) |>
        arrange(patrol_boro, desc(n)))



# Stop writing to the file
sink()