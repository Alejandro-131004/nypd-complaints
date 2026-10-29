#install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

# 1. Carregar e limpar dados
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)

# 2. Contar crimes por bairro
crimes_bairro <- count(nypd, boro_nm, sort = TRUE)

# 3. Criar o gráfico
grafico_bairros <- ggplot(crimes_bairro, aes(x = reorder(boro_nm, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Crimes por Bairro (2020-2026)", x = "Bairro", y = "Ocorrências") +
  theme_minimal()

# 4. Guardar o gráfico na pasta fig/
ggsave("fig/crimes_por_bairro.png", plot = grafico_bairros, width = 8, height = 5)


# 5 Exploração dos Dados
# Crimes por ano de reporte
crimes_por_ano <- count(nypd, report_year)
grafico_ano <- ggplot(crimes_por_ano, aes(x = as.factor(report_year), y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Crimes Reportados por Ano (2020-2026)", x = "Ano", y = "Ocorrências") +
  theme_minimal()
ggsave("fig/crimes_por_ano.png", plot = grafico_ano, width = 8, height = 5)

# Nível de gravidade dos crimes (Felony, Misdemeanor, Violation)
crimes_gravidade <- count(nypd, law_cat_cd, sort = TRUE)
grafico_gravidade <- ggplot(crimes_gravidade, aes(x = reorder(law_cat_cd, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Nível de Gravidade dos Crimes", x = "Gravidade", y = "Ocorrências") +
  theme_minimal()
ggsave("fig/crimes_gravidade.png", plot = grafico_gravidade, width = 8, height = 5)

# Top 20 tipos de crimes mais comuns
top_crimes <- count(nypd, ofns_desc, sort = TRUE) |> slice_head(n = 20)
grafico_tipos <- ggplot(top_crimes, aes(x = reorder(ofns_desc, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Tipos de Crimes mais Reportados", x = "Descrição do Crime", y = "Ocorrências") +
  theme_minimal()
ggsave("fig/top_crimes.png", plot = grafico_tipos, width = 10, height = 6)

# Crimes por hora do dia
crimes_hora <- count(nypd, hour)
grafico_hora <- ggplot(crimes_hora, aes(x = hour, y = n)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  scale_x_continuous(breaks = 0:23) + 
  labs(title = "Ciclo Diário de Crimes na Cidade de Nova Iorque", 
       subtitle = "O pico na hora 0 pode representar horas desconhecidas (00:00)",
       x = "Hora do Dia", y = "Ocorrências") +
  theme_minimal()
ggsave("fig/crimes_por_hora.png", plot = grafico_hora, width = 9, height = 5)

# Crimes por dia da semana
crimes_dia_semana <- count(nypd, wday)
grafico_dia <- ggplot(crimes_dia_semana, aes(x = wday, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Crimes por Dia da Semana", x = "Dia da Semana", y = "Ocorrências") +
  theme_minimal()
ggsave("fig/crimes_dia_semana.png", plot = grafico_dia, width = 8, height = 5)


# 5. Exploração de cruzamento de variáveis
# EIXO 1: Vulnerabilidade (Quem e Onde)
vulnerabilidade <- nypd |> 
  filter(vic_age_group != "UNKNOWN", !is.na(vic_age_group)) |>
  count(vic_age_group, prem_typ_desc, sort = TRUE) |>
  slice_head(n = 15)
grafico_vulnerabilidade <- ggplot(vulnerabilidade, aes(x = reorder(prem_typ_desc, n), y = n, fill = vic_age_group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Vítimas por Local e Grupo Etário", x = "Tipo de Local", y = "Ocorrências", fill = "Idade") +
  theme_minimal()
ggsave("fig/vulnerabilidade.png", plot = grafico_vulnerabilidade, width = 10, height = 6)

# EIXO 2: Eficácia da Prevenção (O que e Onde)
# Step 1: pick the 15 premises with most complaints (attempted + completed together)
top_premises <- nypd |>
  filter(crm_atpt_cptd_cd %in% c("ATTEMPTED", "COMPLETED"),
         !is.na(prem_typ_desc), prem_typ_desc != "(null)") |>
  count(prem_typ_desc, sort = TRUE) |>
  slice_head(n = 15) |>
  pull(prem_typ_desc)

# Step 2: count attempted vs completed only within those premises
eficacia <- nypd |>
  filter(crm_atpt_cptd_cd %in% c("ATTEMPTED", "COMPLETED"),
         prem_typ_desc %in% top_premises) |>
  count(prem_typ_desc, crm_atpt_cptd_cd) |>
  # Order premises by total volume (largest at the top after coord_flip)
  mutate(prem_typ_desc = factor(prem_typ_desc, levels = rev(top_premises)))

grafico_eficacia <- ggplot(eficacia, aes(x = prem_typ_desc, y = n, fill = crm_atpt_cptd_cd)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Crimes Tentados vs. Concluídos por Local", x = "Tipo de Local", y = "Ocorrências", fill = "Estado") +
  theme_minimal()
grafico_eficacia
ggsave("fig/exploracao_01/eficacia.png", plot = grafico_eficacia, width = 10, height = 6)


# EIXO 3: Evolução Pós-2020 (Quando e Onde)
evolucao <- nypd |>
  count(report_year, prem_typ_desc, sort = TRUE) |>
  filter(report_year %in% c(2020, 2025)) |>
  slice_head(n = 20)
evolucao$report_year <- as.factor(evolucao$report_year)
grafico_evolucao <- ggplot(evolucao, aes(x = reorder(prem_typ_desc, n), y = n, fill = report_year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("2020" = "lightblue", "2025" = "darkblue")) +
  labs(title = "Evolução dos Crimes por Local (2020 vs 2025)", x = "Tipo de Local", y = "Ocorrências", fill = "Ano") +
  theme_minimal()
ggsave("fig/evolucao_locais.png", plot = grafico_evolucao, width = 10, height = 6)



# Offence level (felony / misdemeanor / violation) for each of the top 20 offence types

gravidade_crimes <- nypd |>
  filter(ofns_desc %in% top_crimes$ofns_desc,
         law_cat_cd %in% c("FELONY", "MISDEMEANOR", "VIOLATION")) |>
  count(ofns_desc, law_cat_cd)

grafico_gravidade_crimes <- ggplot(
  gravidade_crimes,
  aes(x = reorder(ofns_desc, n), y = n, fill = law_cat_cd)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Crime Types by Severity Level",
    subtitle = "Top 20 crime types",
    x = "Crime Type",
    y = "Number of Occurrences",
    fill = "Severity Level"
  ) +  
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal()

# Display the plot
grafico_gravidade_crimes

# Save the plot
ggsave(
  "fig/exploracao_01/crimes_por_gravidade.png",
  plot = grafico_gravidade_crimes,
  width = 12,
  height = 9
)



