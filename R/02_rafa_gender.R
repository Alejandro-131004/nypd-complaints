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
pink <- "violet"
black <- "black"


# Loading dataset and removing old occurrences
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- subset(nypd, report_year >= 2020 & report_year <= 2025)
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)



# --------------------------------------------------------------- #
  # --------------------------- Idea -------------------------- #
# --------------------------------------------------------------- #



# I want to assess crime severity vulnerability and 
# targeting mostly focused on gender and age 

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

ggsave("fig/02_exploration/02_rafa/gender_age/severity_count_borough.png")


# Let's see if the trend of misdemeanor-felony-violation is also present in genders

# 2. Crime severity per victim sex
vic_sex <- subset(nypd, vic_sex %in% c("M", "F"))

ggplot(vic_sex, aes(x = vic_sex, fill = law_cat_cd)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
  scale_x_discrete(labels = c("F" = "Female", "M" = "Male")) +
  labs(
    title   = "Crime Severity by Victim Sex",
    x       = "Victim Sex",
    y       = "Number of Complaints (Thousands)",
    fill    = "Crime Severity") +
  theme_minimal() +
  scale_fill_brewer(palette = "Reds", direction = -1)

ggsave("fig/02_exploration/02_rafa/gender_age/severity_count_victim_sex.png")


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
    y       = "Percentage of the Borough's Complaints",
    fill    = "Crime Severity") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Reds", direction = -1)

ggsave("fig/02_exploration/02_rafa/gender_age/severity_percent_borough.png")


# 4. Crime severity per victim sex (share of each sex's complaints)
n_sex      <- table(vic_sex$vic_sex)

# Filtering only male and female, there are genders which equal to state, company, etc
sex_names  <- c("F" = "Female", "M" = "Male")
sex_labels <- paste0(sex_names[names(n_sex)], "\n(n = ", scales::comma(as.numeric(n_sex)), ")")
names(sex_labels) <- names(n_sex)

ggplot(vic_sex, aes(x = vic_sex, fill = law_cat_cd)) +
  geom_bar(aes(y = after_stat(count / ave(count, x, FUN = sum))),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = sex_labels) +
  labs(
    title   = "Crime Severity by Victim Sex",
    x       = "Victim Sex",
    y       = "Percentage of the Sex's Complaints",
    fill    = "Crime Severity") +
  theme_minimal() +
  scale_fill_brewer(palette = "Reds", direction = -1)

ggsave("fig/02_exploration/02_rafa/gender_age/severity_percent_victim_sex.png")


# This tells us that the relationship between severity is stable
# across boroughs, but with non-insignificant differences in both
# percentage and count between sex, especially in the felonies and violations

# 5. Victim sex share per borough
boro_sex <- subset(nypd, !is.na(boro_nm) & vic_sex %in% c("M", "F"))

# n per boro on only male or female victim
n_boro_sex      <- table(boro_sex$boro_nm)
boro_sex_labels <- paste0(names(n_boro_sex), "\n(n = ",
                          scales::comma(as.numeric(n_boro_sex)), ")")
names(boro_sex_labels) <- names(n_boro_sex)

ggplot(boro_sex, aes(x = boro_nm, fill = vic_sex)) +
  geom_bar(aes(y = after_stat(count / ave(count, x, FUN = sum))),
           position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = boro_sex_labels) +
  labs(
    title   = "Victim Sex by Borough",
    x       = "Borough",
    y       = "Percentage of the Borough's Complaints",
    fill    = "Victim Sex")
  theme_minimal() +
  scale_fill_manual(values = c("F" = pink, "M" = dv_blue),
                    labels = c("F" = "Female", "M" = "Male"))

ggsave("fig/02_exploration/02_rafa/gender_age/sex_percent_borough.png", bg = white)


# An interesting takeaway is that crime severity percentages tend to be larger in
# more serious crimes for men rather than women, despite representing a slightly
# smaller (yet still balanced) global and borough-wise representation

# Globally, there is no clear gender or borough vulnerability to specific 
# severity, so we must look into gender vulnerability inside boroughs

# We have 2 ways of looking into this:
# 1. Looking inside of each boro, what percentages of crimes against 
#    a specific sex are a specific severity?
# 2. Looking inside of each boro, what percentages of each severity
#    are committed against each sex?


# 6.1 Gender Severity Vulnerability, inside boros

sex_sev <- count(boro_sex, boro_nm, vic_sex, law_cat_cd)

# Severity likelyhood per gender, inside boros
# of all SEX complaints, how many are SEVERITY?
sex_sev <- sex_sev |>
  group_by(boro_nm, vic_sex) |>
  mutate(share = n / sum(n)) |>
  ungroup()

ggplot(sex_sev, aes(x = law_cat_cd, y = share, fill = vic_sex)) +
  geom_col(position = "dodge") +
  facet_wrap(~ boro_nm, nrow = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Crime Severity by Victim Sex, per Borough",
    x     = "Crime Severity",
    y     = "Percentage of the Sex's Complaints in the Borough",
    fill  = "Victim Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("F" = pink, "M" = dv_blue),
                    labels = c("F" = "Female", "M" = "Male"))

ggsave("fig/02_exploration/02_rafa/gender_age/severity_percent_sex_borough.png", bg = white)



# 6.2 Gender likelyhood by severity, inside boros
# of all SEVERITY complaints, how many are SEX?
sex_in_sev <- boro_sex |>
  count(boro_nm, law_cat_cd, vic_sex) |>
  group_by(boro_nm, law_cat_cd) |>
  mutate(share = n / sum(n)) |>
  ungroup()

ggplot(sex_in_sev, aes(x = law_cat_cd, y = share, fill = vic_sex)) +
  geom_col() +
  geom_text(aes(label = scales::percent(share, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            colour = white, size = 3) +
  facet_wrap(~ boro_nm, nrow = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("F" = pink, "M" = dv_blue),
                    labels = c("F" = "Female", "M" = "Male")) +
  labs(
    title = "Victim Sex within Each Crime Severity, per Borough",
    x     = "Crime Severity",
    y     = "Percentage of the Severity's Complaints",
    fill  = "Victim Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fig/02_exploration/02_rafa/gender_age/sex_percent_severity_borough.png", bg = white)

# Across the board, even though there are always at least as many women as men:
#  1. in every borough, the more serious the offense, the higher 
#     the likelihood that the victim is a man
#  2. in every borough, men are more likely to be victims of misdemeanors 
#     and felonies (with the small exception of Bronx Misdemeanors)


# 7. Offence counts by victim sex, one table per offence level
sex_off <- nypd |>
  filter(vic_sex %in% c("M", "F"), !is.na(ofns_desc), !is.na(law_cat_cd)) |>
  count(law_cat_cd, ofns_desc, vic_sex) |>
  group_by(law_cat_cd, ofns_desc) |>
  mutate(total = sum(n)) |>                    # total for both sexes, used for ordering
  group_by(law_cat_cd) |>
  filter(dense_rank(desc(total)) <= 10) |>     # top 10 offences per level
  ungroup()

ggplot(sex_off, aes(x = vic_sex, y = reorder(ofns_desc, total), fill = n)) +
  geom_tile(colour = white) +
  geom_text(aes(label = scales::comma(n)), size = 3) +
  facet_wrap(~ law_cat_cd, scales = "free_y", ncol = 3) +
  scale_x_discrete(labels = c("F" = "Female", "M" = "Male"), position = "top") +
  scale_fill_gradient(low = white, high = "firebrick", labels = scales::comma) +
  labs(
    title = "Offence Counts by Victim Sex, per Offence Level",
    x     = NULL,
    y     = NULL,
    fill  = "Complaints") +
  theme_minimal() +
  theme(panel.grid = element_blank())

ggsave("fig/02_exploration/02_rafa/gender_age/offence_count_sex_level.png",
       width = 16, height = 6, bg = white)