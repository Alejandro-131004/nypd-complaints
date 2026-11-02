#install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

dv_blue <- "#2E75B6"
orange <- "orange2"
gold <- "gold"
light_blue <- "lightblue"
dark_blue <- "darkblue"
nice_green <- "green4"
grey <- "grey85"
white <- "white"


# Loading dataset and removing old occurrences
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)



# --------------------------------------------------------------- #
  # --------------------------- Idea -------------------------- #
# --------------------------------------------------------------- #



# I want to assess crime severity vulnerability and targeting
# mostly focused on racial tendencies or discrimination

# The idea is to then identify which regions of boroughs are more
# predominantly a specific race, and how we can mitigate targeting
# by allocating specialized resources to the most affected areas

# Since we lack specific borough segments, I'll be using police station
# locations as an approximation of north-south-east-west


# --------------------------------------------------------------- #
  # ------------------- Thought Process ----------------------- #
# --------------------------------------------------------------- #

# I'll start by looking at crime severity counts per borough

# 1. Crime severity per borough
boro <- subset(nypd, !is.na(boro_nm))
ggplot(boro, aes(x = boro_nm, fill = law_cat_cd)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
  labs(
    title   = "Crime Severity by Borough",
    x       = "Borough",
    y       = "Number of Complaints (Thousands)",
    fill    = "Crime Severity") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Reds", direction = -1)

# Let's see if the trend of misdemeanor-felony-violation is also present in races

# 2. Crime severity per victim race
vic <- subset(nypd, !is.na(vic_race) & !(vic_race %in% c("(null)", "UNKNOWN")))

ggplot(vic, aes(x = vic_race, fill = law_cat_cd)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
  labs(
    title   = "Crime Severity by Victim Race",
    x       = "Victim Race (as recorded by police)",
    y       = "Number of Complaints (Thousands)",
    fill    = "Crime Severity") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_brewer(palette = "Reds", direction = -1)

# In both cases, we can identify clear magnitude issues. Let's try a 
# regularization by instance counts (Black-White >> much larger than Alaskan)

# 3. Crime severity per borough accounting for population size
n_boro      <- table(boro$boro_nm)
boro_labels <- paste0(names(n_boro), "\n(n = ", scales::comma(as.numeric(n_boro)), ")")
names(boro_labels) <- names(n_boro)

ggplot(boro, aes(x = boro_nm, fill = law_cat_cd)) +
  geom_bar(aes(y = after_stat(count / ave(count, x, FUN = sum))),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = boro_labels) +
  labs(
    title   = "Crime Severity by Borough",
    x       = "Borough",
    y       = "Share of the Borough's Complaints",
    fill    = "Crime Severity") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Reds", direction = -1)


# 4. Crime severity per victim race accounting for population size
n_race      <- table(vic$vic_race)
race_labels <- paste0(names(n_race), "\n(n = ", scales::comma(as.numeric(n_race)), ")")
names(race_labels) <- names(n_race)

ggplot(vic, aes(x = vic_race, fill = law_cat_cd)) +
  geom_bar(aes(y = after_stat(count / ave(count, x, FUN = sum))),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = race_labels) +
  labs(
    title   = "Population-Normalized Crime Severity by Victim Race",
    x       = "Victim Race (as recorded by police)",
    y       = "Complaints %",
    fill    = "Crime Severity") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_brewer(palette = "Reds", direction = -1)

# This tells us that the relationship between severity is stable
# across boroughs and race. Let's look at racial density per borough


# Top3 for readability, rest grayed out
boro_race <- subset(nypd, !is.na(boro_nm) &
        !is.na(vic_race) & !(vic_race %in% c("(null)", "UNKNOWN")))
top3 <- names(sort(table(boro_race$vic_race), decreasing = TRUE))[1:3]
boro_race$vic_race_top <- ifelse(boro_race$vic_race %in% top3, 
                                 boro_race$vic_race, "OTHER")
boro_race$vic_race_top <- factor(boro_race$vic_race_top, levels = c(top3, "OTHER"))

ggplot(boro_race, aes(x = boro_nm, fill = vic_race_top)) +
  geom_bar(aes(y = after_stat(count / ave(count, x, FUN = sum))),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = boro_labels) +
  labs(
    title   = "Victim Race by Borough (Top 3)",
    x       = "Borough",
    y       = "Percentage of Borough's Complaints",
    fill    = "Victim Race (as recorded by police)") +
  theme_minimal() + 
  scale_fill_manual(values = c("BLACK"          = orange,
                               "WHITE"          = gold,
                               "WHITE HISPANIC" = dark_blue,
                               "OTHER"          = grey))

# An interesting takeaway is that crime severity percentages are consistent 
# throughout both race and boroughs, despite race distribution per borough
# being completely different for each case


library(dplyr)

# 3. Top 3 crimes per victim race
vic <- subset(nypd, !is.na(vic_race) & !(vic_race %in% c("(null)", "UNKNOWN")) &
                !is.na(ofns_desc))

top_crimes <- vic |>
  count(vic_race, ofns_desc) |>          # complaints per race and crime
  group_by(vic_race) |>
  mutate(pct = n / sum(n)) |>            # share of that race's complaints
  slice_max(n, n = 3) |>                 # keep the 3 most common per race
  ungroup()

# Total complaints per race, used in the axis labels
n_race      <- table(vic$vic_race)
race_labels <- paste0(names(n_race), "\n(n = ", scales::comma(as.numeric(n_race)), ")")
names(race_labels) <- names(n_race)

ggplot(top_crimes, aes(x = vic_race, y = pct, fill = ofns_desc)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = race_labels) +
  labs(
    title   = "Top 3 Crimes by Victim Race",
    x       = "Victim Race (as recorded by police)",
    y       = "Share of the Race's Complaints",
    fill    = "Offence") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")




# 4. Top 3 felonies per victim race
vic_fel <- subset(nypd, !is.na(vic_race) & !(vic_race %in% c("(null)", "UNKNOWN")) &
                    !is.na(ofns_desc) & law_cat_cd == "FELONY")

top_felonies <- vic_fel |>
  count(vic_race, ofns_desc) |>
  group_by(vic_race) |>
  mutate(pct = n / sum(n)) |>
  slice_max(n, n = 3) |>
  ungroup()

# Total felony complaints per race, used in the axis labels
n_race      <- table(vic_fel$vic_race)
race_labels <- paste0(names(n_race), "\n(n = ", scales::comma(as.numeric(n_race)), ")")
names(race_labels) <- names(n_race)

ggplot(top_felonies, aes(x = vic_race, y = pct, fill = ofns_desc)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = race_labels) +
  labs(
    title   = "Top 3 Felonies by Victim Race",
    x       = "Victim Race (as recorded by police)",
    y       = "Share of the Race's Felony Complaints",
    fill    = "Felony") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")