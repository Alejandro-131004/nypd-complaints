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

ggsave("fig/02_exploration/02_rafa/race/severity_count_borough.png")


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

ggsave("fig/02_exploration/02_rafa/race/severity_count_victim.png")


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

ggsave("fig/02_exploration/02_rafa/race/severity_percent_borough.png")


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

ggsave("fig/02_exploration/02_rafa/race/severity_percent_victim.png")


# This tells us that the relationship between severity is stable
# across boroughs and race. Let's look at racial density per borough


# 5. Victim race share per borough (top 3 races, rest grouped as OTHER)
boro_race <- subset(nypd, !is.na(boro_nm) &
                      !is.na(vic_race) & !(vic_race %in% c("(null)", "UNKNOWN")))

top3 <- names(sort(table(boro_race$vic_race), decreasing = TRUE))[1:3]
boro_race$vic_race_top <- ifelse(boro_race$vic_race %in% top3,
                                 boro_race$vic_race, "OTHER")
boro_race$vic_race_top <- factor(boro_race$vic_race_top, levels = c(top3, "OTHER"))

# n per borough, counted on THIS subset (complaints with a known victim race)
n_boro_race      <- table(boro_race$boro_nm)
boro_race_labels <- paste0(names(n_boro_race), "\n(n = ",
                           scales::comma(as.numeric(n_boro_race)), ")")
names(boro_race_labels) <- names(n_boro_race)

ggplot(boro_race, aes(x = boro_nm, fill = vic_race_top)) +
  geom_bar(aes(y = after_stat(count / ave(count, x, FUN = sum))),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = boro_race_labels) +
  labs(
    title   = "Victim Race by Borough (Top 3, rest grouped as Other)",
    x       = "Borough",
    y       = "Share of the Borough's Complaints",
    fill    = "Victim Race (as recorded by police)",
    caption = "Complaints with unknown victim race excluded.") +
  theme_minimal() +
  scale_fill_manual(values = c("BLACK"          = orange,
                               "WHITE"          = gold,
                               "WHITE HISPANIC" = dark_blue,
                               "OTHER"          = grey))

ggsave("fig/02_exploration/02_rafa/race/race_percent_borough.png",
       width = 10, height = 6, dpi = 300, bg = white)


# An interesting takeaway is that crime severity percentages are consistent 
# throughout both race and boroughs, despite race distribution per borough
# being completely different for each case

# Globally, there is no clear race or borough vulnerability to crime severity
# However, results above indicate that racial density between boroughs is
# highly heterogeneous. 

# This means that there might be certain races accounting for large portions
# of different crime severities in certain boroughs. In other words, the ratio
# of felony victims in a specific borough, for example, can be compliant to 
# average levels of global ny felony victims, but intra-borough it might be strongly
# associated to a victim of a certain race, which would expose race vulnerability
# in boroughs.

# 7. Which race is most over-represented in each severity, per borough,
#    and by how much compared with all other races in the same borough
tab <- table(boro_race$boro_nm, boro_race$vic_race, boro_race$law_cat_cd)  # borough x race x severity

n_BR <- apply(tab, c(1, 2), sum)   # complaints per borough x race
n_BS <- apply(tab, c(1, 3), sum)   # complaints per borough x severity
n_B  <- apply(tab, 1, sum)         # complaints per borough

sev_vs <- as.data.frame(as.table(tab))
names(sev_vs) <- c("boro_nm", "vic_race", "law_cat_cd", "n")
b <- as.character(sev_vs$boro_nm)
r <- as.character(sev_vs$vic_race)
s <- as.character(sev_vs$law_cat_cd)

sev_vs$group_n      <- n_BR[cbind(b, r)]
sev_vs$share_race   <- sev_vs$n / sev_vs$group_n                                  # this race
sev_vs$share_others <- (n_BS[cbind(b, s)] - sev_vs$n) / (n_B[b] - sev_vs$group_n)  # all other races
sev_vs$ratio        <- sev_vs$share_race / sev_vs$share_others

# Drop small groups, then flag the highest ratio per borough x severity
sev_vs <- subset(sev_vs, group_n >= 1000)
sev_vs$is_top <- sev_vs$ratio == ave(sev_vs$ratio, sev_vs$boro_nm, sev_vs$law_cat_cd, FUN = max)

boro_short <- c("BRONX" = "Bronx", "BROOKLYN" = "Brooklyn", "MANHATTAN" = "Manhattan",
                "QUEENS" = "Queens", "STATEN ISLAND" = "Staten Is.")

ggplot(sev_vs, aes(x = boro_nm, y = vic_race, fill = ratio)) +
  geom_tile(colour = white) +
  geom_tile(data = subset(sev_vs, is_top), fill = NA, colour = "black", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", ratio)), size = 3) +
  facet_wrap(~ law_cat_cd, nrow = 1) +
  scale_x_discrete(labels = boro_short) +
  scale_y_discrete(labels = race_short) +
  scale_fill_gradient2(low = dv_blue, mid = white, high = "firebrick",
                       midpoint = 1, limits = c(0.5, 1.5), oob = scales::squish) +
  labs(
    title   = "Over-Representation by Crime Severity: Each Race vs All Other Races in the Borough",
    x       = NULL,
    y       = "Victim Race (as recorded by police)",
    fill    = "Ratio to other\nraces in borough",
    caption = "1.25 = this severity is a 25% larger share of the race's complaints than of other races' complaints in the borough.\nBlack outline: highest ratio in that borough and severity. Groups with fewer than 1,000 complaints excluded.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid  = element_blank())

ggsave("fig/02_exploration/02_rafa/race/severity_ratio_race_vs_others_borough.png",
       width = 13, height = 6, dpi = 300, bg = white)


# ------------- #


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
ggsave("fig/02_exploration/02_rafa/race/victims_full.png")

# ----------- #

# 8. Offence counts by victim race, one image per offence level
race_off <- nypd |>
  filter(!is.na(vic_race), !(vic_race %in% c("(null)", "UNKNOWN")),
         !is.na(ofns_desc), !is.na(law_cat_cd)) |>
  count(law_cat_cd, ofns_desc, vic_race) |>
  group_by(law_cat_cd, ofns_desc) |>
  
  # total, used for ordering
  mutate(total = sum(n)) |>
  group_by(law_cat_cd) |>
  
  # top 7 chosen manually (magnitude)
  filter(dense_rank(desc(total)) <= 7) |>
  ungroup()

for (level in c("FELONY", "MISDEMEANOR", "VIOLATION")) {
  
  p <- ggplot(filter(race_off, law_cat_cd == level),
              aes(x = vic_race, y = reorder(ofns_desc, total), fill = n)) +
    geom_tile(colour = white) +
    geom_text(aes(label = scales::comma(n)), size = 3) +
    scale_x_discrete(labels = race_short, position = "top") +
    scale_fill_gradient(low = white, high = dv_blue, labels = scales::comma) +
    labs(
      title = paste("Offence Counts by Victim Race:", tools::toTitleCase(tolower(level))),
      x     = NULL,
      y     = NULL,
      fill  = "Complaints") +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  print(p)
  
  ggsave(paste0("fig/02_exploration/02_rafa/race/offence_counts_race_", tolower(level), ".png"),
         plot = p, width = 12, height = 5, bg = white)
}

