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
nypd <- subset(nypd, report_year >= 2020 & report_year <= 2025)
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

ggsave("fig/02_exploration/02_rafa/severity_count_borough.png")


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

ggsave("fig/02_exploration/02_rafa/severity_count_victim.png")


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

ggsave("fig/02_exploration/02_rafa/severity_percent_borough.png")


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

ggsave("fig/02_exploration/02_rafa/severity_percent_victim.png")


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

ggsave("fig/02_exploration/02_rafa/race_percent_borough.png")


# An interesting takeaway is that crime severity percentages are consistent 
# throughout both race and boroughs, despite race distribution per borough
# being completely different for each case


# Data: one row per complaint with a known borough, victim race and offence
vic_bc <- subset(nypd, !is.na(boro_nm) & !is.na(ofns_desc) &
                   !is.na(vic_race) & !(vic_race %in% c("(null)", "UNKNOWN")))

# Share of each offence within each borough x race group
tab_race   <- table(vic_bc$boro_nm, vic_bc$vic_race, vic_bc$ofns_desc)
share_race <- prop.table(tab_race, margin = c(1, 2))

# Share of each offence within each borough (the borough average)
share_boro <- prop.table(table(vic_bc$boro_nm, vic_bc$ofns_desc), margin = 1)

# Ratio: how much more (or less) common the offence is for that race than the borough average
ratio <- sweep(share_race, c(1, 3), share_boro, "/")

# Into a data frame for ggplot
mix <- as.data.frame(as.table(ratio))
names(mix) <- c("boro_nm", "vic_race", "ofns_desc", "ratio")

# Size of each borough x race group, to drop tiny noisy groups
group_n     <- apply(tab_race, c(1, 2), sum)
mix$group_n <- group_n[cbind(as.character(mix$boro_nm), as.character(mix$vic_race))]

# Keep the 8 most common offences and groups with at least 1,000 complaints
top_offences <- names(sort(table(vic_bc$ofns_desc), decreasing = TRUE))[1:8]
mix <- subset(mix, ofns_desc %in% top_offences & group_n >= 1000)

# Short labels
race_short <- c("AMERICAN INDIAN/ALASKAN NATIVE" = "Am. Indian/Alaskan",
                "ASIAN / PACIFIC ISLANDER"       = "Asian/PI",
                "BLACK"                          = "Black",
                "BLACK HISPANIC"                 = "Black Hisp.",
                "WHITE"                          = "White",
                "WHITE HISPANIC"                 = "White Hisp.")
boro_short <- c("BRONX" = "Bronx", "BROOKLYN" = "Brooklyn", "MANHATTAN" = "Manhattan",
                "QUEENS" = "Queens", "STATEN ISLAND" = "Staten Is.")

ggplot(mix, aes(x = boro_nm, y = vic_race, fill = ratio)) +
  geom_tile(colour = "white") +
  facet_wrap(~ ofns_desc, nrow = 2, labeller = label_wrap_gen(width = 22)) +
  scale_x_discrete(labels = boro_short) +
  scale_y_discrete(labels = race_short) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                       midpoint = 1, limits = c(0, 2), oob = scales::squish) +
  labs(
    title = "Over- and Under-Represented Offences by Victim Race and Borough",
    x     = NULL,
    y     = NULL,
    fill  = "Ratio to\nborough average") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid  = element_blank())
ggsave("fig/02_exploration/02_rafa/victims_full.png")
