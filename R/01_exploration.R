#install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

dv_blue <- "#2E75B6"
orange <- "orange2"
light_blue <- "lightblue"
dark_blue <- "darkblue"
nice_green <- "green4"

dir.create("fig/01_exploration", showWarnings = FALSE, recursive = TRUE)

# Loading dataset and removing old occurrences
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)


# ---------------------------------------------------------------- #

 # We begin our experiment by creating simple visualizations
 # The goal here is to identify potentially significant yet straightforward
 # fequency differences between boros, years, times of day, etc

# Boro Crime Frequency
boro_crimes <- count(nypd, boro_nm, sort = TRUE)
boro_crimes_plt <- ggplot(boro_crimes, aes(x = reorder(boro_nm, n), y = n)) +
  geom_col(fill = dv_blue) +
  coord_flip() +
  labs(title = "Boro Crime Frequency", x = "Boro", y = "# Reports") + 
  theme_minimal()

ggsave("fig/01_exploration/boro_crime_frequency.png", plot = boro_crimes_plt, width = 8, height = 5)


# Yearly report frequency
crimes_per_year <- count(nypd, report_year)
yearly_crimes_plt <- ggplot(crimes_per_year, aes(x = as.factor(report_year), y = n)) +
  geom_col(fill = dv_blue) +
  labs(title = "Yearly Report Frequency (2020-2026)", x = "Year", y = "# Reports") +
  theme_minimal()

ggsave("fig/01_exploration/crimes_per_year.png", plot = yearly_crimes_plt, width = 8, height = 5)


# Top 20 most common rime types
crime_types <- count(nypd, ofns_desc, sort = TRUE) |> slice_head(n = 20)
crime_types_plt <- ggplot(crime_types, aes(x = reorder(ofns_desc, n), y = n)) +
  geom_col(fill = dv_blue) +
  coord_flip() +
  labs(title = "Top 20 most Reported Crimes", x = "Description", y = "# Reports") +
  theme_minimal()

ggsave("fig/01_exploration/crime_types.png", plot = crime_types_plt, width = 10, height = 6)


# Crime frequency per hour

# Simple cleanup (NaN removal) & count
crimes_per_hour <- nypd |> 
  filter(!is.na(hour)) |> 
  count(hour)


# Important to mention that this considers the approximate hour the crime
# was commited, not the time of the report!

crimes_per_hour_plt <- ggplot(crimes_per_hour, aes(x = hour, y = n)) +
  geom_line(color = dv_blue, linewidth = 1.2) +
  geom_point(color = dv_blue, size = 2) +
  scale_x_continuous(breaks = 0:23) + 
  
  # Small fix that forces y=0 as starting point instead of 
  # around the lowest value
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) + 
  labs(title = "NY City Crime Report Frequency Per Hour", 
       subtitle = "Unknown timestamps are also marked 00:00",
       x = "Time of Day", y = "Number of Reports") +
  theme_minimal()

ggsave("fig/01_exploration/hourly_crime_frequency.png", plot = crimes_per_hour_plt, width = 9, height = 5)



# ---------------------------------------------------------------- #

 
 # This section explores more complex data relationships
 # Our goal here is to start experimenting with variable connections
 # and how they affect report ratios and frequency 
 
# Idea: I think we should use a same-color heatmap here, 
# different colors might give off the wrong idea 
 
# Age Group Vulnerability & premise of Crime
age_vulnerability <- nypd |> 
  filter(vic_age_group != "UNKNOWN", !is.na(vic_age_group)) |>
  count(vic_age_group, prem_typ_desc, sort = TRUE) |>
  slice_head(n = 15)

age_vulnerability_plt <- ggplot(age_vulnerability, aes(x = reorder(prem_typ_desc, n), y = n, fill = vic_age_group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Age Group Vulnerability per Premise",
       x = "premise", y = "# Reports", fill = "Victim Age") +
  theme_minimal()

ggsave("fig/01_exploration/age_vulnerability.png", plot = age_vulnerability_plt, width = 10, height = 6)


# Attempted vs completed crimes per premise
att_vs_comp <- nypd |>
  
  # Get attempted-completed counts from the crm_atpt_cptd_cd column
  filter(crm_atpt_cptd_cd %in% c("ATTEMPTED", "COMPLETED")) |>
  count(prem_typ_desc, crm_atpt_cptd_cd) |>
  
  # Consider premise-wise
  group_by(prem_typ_desc) |>
  mutate(premise_total = sum(n)) |>
  ungroup() |>
  
  # 15 premises * 2 states = 30 rows (for readability)
  slice_max(premise_total, n = 30) 

att_vs_comp_plt <- ggplot(att_vs_comp, aes(x = reorder(prem_typ_desc, n), y = n, fill = crm_atpt_cptd_cd)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("COMPLETED" = orange, "ATTEMPTED" = dv_blue)) +
  labs(title = "Attempted vs Completed Crimes per Premise",
       x = "premise", y = "# Reports", fill = "Crime Status") +
  theme_minimal()

ggsave("fig/01_exploration/att_vs_comp.png", plot = att_vs_comp_plt, width = 10, height = 6)


# EIXO 3: Evolução Pós-2020 (Quando e Onde)
evolucao <- nypd |>
  count(report_year, prem_typ_desc, sort = TRUE) |>
  filter(report_year %in% c(2020, 2025)) |>
  slice_head(n = 20)
evolucao$report_year <- as.factor(evolucao$report_year)
grafico_evolucao <- ggplot(evolucao, aes(x = reorder(prem_typ_desc, n), y = n, fill = report_year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("2020" = light_blue, "2025" = dark_blue)) +
  labs(title = "Evolução dos Crimes por Local (2020 vs 2025)", x = "Tipo de Local", y = "Ocorrências", fill = "Ano") +
  theme_minimal()
ggsave("fig/01_exploration/evolucao_locais.png", plot = grafico_evolucao, width = 10, height = 6)



#--------------------------------------------------------------------------------------------------------------------------------------
# ANÁLISE DE VALORES NULOS, ERROS E ANOMALIAS
# 1. Resumo de NAs por coluna
auditoria_nas <- data.frame(
  coluna = names(nypd),
  tipo = sapply(nypd, function(x) class(x)[1]),
  n_nas = sapply(nypd, function(x) sum(is.na(x))),
  pct_nas = sapply(nypd, function(x) round(sum(is.na(x)) / nrow(nypd) * 100, 2))
)
# 2. Contar valores literais "(null)" nas colunas de texto
nulos_texto <- sapply(nypd, function(x) {
  if(is.character(x)) {
    sum(x == "(null)", na.rm = TRUE)
  } else {
    0
  }
})
auditoria_nas$n_null_literal <- nulos_texto
# Imprimir a tabela completa com todas as colunas
library(tidyr)
auditoria_long <- auditoria_nas |> 
  mutate(
    # Calcular total de falhas (NAs + nulos literais) em percentagem
    total_falhas_pct = ((n_nas + n_null_literal) / nrow(nypd)) * 100
  ) |> 
  filter(total_falhas_pct > 0) # Mostrar apenas colunas com problemas
grafico_auditoria <- ggplot(auditoria_long, aes(x = reorder(coluna, total_falhas_pct), y = total_falhas_pct)) +
  geom_col(fill = dv_blue) +
  coord_flip() +
  labs(
    title = "Auditoria de Qualidade: Percentagem de Dados em Falta por Coluna",
    subtitle = "Inclui valores NA e strings literais '(null)'",
    x = "Nome da Coluna",
    y = "Percentagem de Valores Omissos (%)"
  ) +
  theme_minimal()
ggsave("fig/01_exploration/auditoria_dados_falta.png", plot = grafico_auditoria, width = 10, height = 7)

#--------------------------------------------------------------------------------------------------------------
# MAPA DE CALOR (HORA VS DIA DA SEMANA) 
crimes_heatmap <- nypd |> 
  filter(!is.na(hour), !is.na(wday)) |> 
  count(wday, hour)
grafico_heatmap <- ggplot(crimes_heatmap, aes(x = hour, y = wday, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = light_blue, high = dark_blue, labels = scales::comma) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Mapa de Calor: Concentração de Crimes por Hora e Dia da Semana",
    x = "Hora do Dia",
    y = "Dia da Semana",
    fill = "Ocorrências"
  ) +
  theme_minimal()
dir.create("fig/01_exploration", showWarnings = FALSE, recursive = TRUE)
ggsave("fig/01_exploration/heatmap_hora_dia.png", plot = grafico_heatmap, width = 10, height = 6)

#--------------------------------------------------------------------------------------------------------
# GRAVIDADE DO CRIME POR BAIRRO 
crimes_gravidade <- nypd |> 
  filter(!is.na(boro_nm), !is.na(law_cat_cd)) |> 
  count(boro_nm, law_cat_cd)
grafico_gravidade <- ggplot(crimes_gravidade, aes(x = reorder(boro_nm, n, sum), y = n, fill = law_cat_cd)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) + # Eixo Y começa obrigatoriamente no 0
  scale_fill_manual(values = c(
    "FELONY"      = orange, 
    "MISDEMEANOR" = dv_blue, 
    "VIOLATION"   = nice_green  
  )) +
  labs(
    title = "Volume de Crimes por Nível de Gravidade e Bairro",
    subtitle = "Comparação absoluta entre Felonies, Misdemeanors e Violations",
    x = "Bairro",
    y = "Número de Ocorrências",
    fill = "Nível de Ofensa"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )
ggsave("fig/01_exploration/gravidade_por_bairro.png", plot = grafico_gravidade, width = 10, height = 6)

#----------------------------------------------------------------------------------------------------------------
# TENDÊNCIA MENSAL / SAZONALIDADE 
crimes_mes <- nypd |> 
  filter(!is.na(occurred_on), !old_occurrence) |> 
  mutate(mes = lubridate::month(occurred_on, label = TRUE, abbr = TRUE)) |> 
  count(mes)
grafico_mes <- ggplot(crimes_mes, aes(x = mes, y = n, group = 1)) +
  geom_line(color = dv_blue, linewidth = 1.2) +
  geom_point(color = dv_blue, size = 2.5) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  labs(
    title = "Sazonalidade: Distribuição de Crimes por Mês do Ano",
    x = "Mês",
    y = "Ocorrências Totais"
  ) +
  theme_minimal()
dir.create("fig/01_exploration", showWarnings = FALSE, recursive = TRUE)
ggsave("fig/01_exploration/sazonalidade_mensal.png", plot = grafico_mes, width = 9, height = 5)
