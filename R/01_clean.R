library(dplyr)

# 1. Read the dataset
nypd <- readRDS("data/nypd_2020_2026.rds")

nypd <- as.data.frame(nypd)

nrow(nypd)

## Quick look
str(nypd)
head(nypd)
tail(nypd)
names(nypd)

nypd_clean <- nypd


# 2. Replace "(null)" by NA
nypd_clean <- nypd_clean |> mutate(across(where(is.character), ~na_if(.x, "(null)")))

## Check
nypd_clean |>
  select(where(is.character)) |> 
  sapply(function(x) sum(x == "(null)", na.rm = TRUE))


# 3. Check age values
table(nypd_clean$vic_age_group, useNA = "ifany")
table(nypd_clean$susp_age_group, useNA = "ifany")

idades_validas <- c("<18", "18-24", "25-44", "45-64", "65+", "UNKNOWN")

# Replace invalid categories with NA
nypd_clean <- nypd_clean |> mutate(
  vic_age_group = if_else(vic_age_group %in% idades_validas |
                            is.na(vic_age_group), vic_age_group, NA_character_), 
  susp_age_group = if_else(susp_age_group %in% idades_validas |
                             is.na(susp_age_group), susp_age_group, NA_character_)
)


# Check
nrow(nypd)
nrow(nypd_clean)

table(nypd_clean$vic_age_group, useNA = "ifany")
table(nypd_clean$susp_age_group, useNA = "ifany")


# 4. Save
dir.create("data", showWarnings = FALSE)
saveRDS(nypd_clean, "data/nypd_clean.rds")
