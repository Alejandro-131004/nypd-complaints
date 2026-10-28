library(dplyr)
library(ggplot2)

dir.create("fig/01_exploracao", showWarnings = FALSE, recursive = TRUE)

#Carregar e limpar dados
nypd <- readRDS("data/nypd_2020_2026.rds")
nypd <- as.data.frame(nypd)
nypd <- nypd |> filter(!old_occurrence)

# Contar crimes por bairro
crimes_bairro <- count(nypd, boro_nm, sort = TRUE)
grafico_bairros <- ggplot(crimes_bairro, aes(x = reorder(boro_nm, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Crimes por Bairro (2020-2026)", x = "Bairro", y = "Ocorrências") +
  theme_minimal()
ggsave("fig/01_exploracao/crimes_por_bairro.png", plot = grafico_bairros, width = 8, height = 5)


# Crimes por ano de reporte
crimes_por_ano <- count(nypd, report_year)
grafico_ano <- ggplot(crimes_por_ano, aes(x = as.factor(report_year), y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Crimes Reportados por Ano (2020-2026)", x = "Ano", y = "Ocorrências") +
  theme_minimal()
ggsave("fig/01_exploracao/crimes_por_ano.png", plot = grafico_ano, width = 8, height = 5)

# Nível de gravidade dos crimes (Felony, Misdemeanor, Violation)
crimes_gravidade <- count(nypd, law_cat_cd, sort = TRUE)
grafico_gravidade <- ggplot(crimes_gravidade, aes(x = reorder(law_cat_cd, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Nível de Gravidade dos Crimes", x = "Gravidade", y = "Ocorrências") +
  theme_minimal()
ggsave("fig/01_exploracao/crimes_gravidade.png", plot = grafico_gravidade, width = 8, height = 5)

# Top 20 tipos de crimes mais comuns
top_crimes <- count(nypd, ofns_desc, sort = TRUE) |> slice_head(n = 20)
grafico_tipos <- ggplot(top_crimes, aes(x = reorder(ofns_desc, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Tipos de Crimes mais Reportados", x = "Descrição do Crime", y = "Ocorrências") +
  theme_minimal()
ggsave("fig/01_exploracao/top_crimes.png", plot = grafico_tipos, width = 10, height = 6)

# Crimes por hora do dia
crimes_hora <- nypd |> 
  filter(!is.na(hour)) |> 
  count(hour)
grafico_hora <- ggplot(crimes_hora, aes(x = hour, y = n)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  scale_x_continuous(breaks = 0:23) + 
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) + # Força o eixo Y a começar no 0
  labs(title = "Ciclo Diário de Crimes na Cidade de Nova Iorque", 
       subtitle = "O pico na hora 0 pode representar horas desconhecidas (00:00)",
       x = "Hora do Dia", y = "Ocorrências") +
  theme_minimal()
dir.create("fig/01_exploracao", showWarnings = FALSE, recursive = TRUE)
ggsave("fig/01_exploracao/crimes_por_hora.png", plot = grafico_hora, width = 9, height = 5)



#------------------------------------------------------------------------------------------------------------------

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
ggsave("fig/01_exploracao/vulnerabilidade.png", plot = grafico_vulnerabilidade, width = 10, height = 6)

# EIXO 2: Eficácia da Prevenção (O que e Onde)
eficacia <- nypd |>
  filter(crm_atpt_cptd_cd %in% c("ATTEMPTED", "COMPLETED")) |>
  count(prem_typ_desc, crm_atpt_cptd_cd) |>
  # Para filtrar os top 15 locais com maior volume total
  group_by(prem_typ_desc) |>
  mutate(total_local = sum(n)) |>
  ungroup() |>
  slice_max(total_local, n = 30) # 15 locais * 2 estados = 30 linhas

grafico_eficacia <- ggplot(eficacia, aes(x = reorder(prem_typ_desc, n), y = n, fill = crm_atpt_cptd_cd)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("COMPLETED" = "orange2", "ATTEMPTED" = "steelblue")) +
  labs(
    title = "Crimes Tentados vs. Concluídos por Local",
    x = "Tipo de Local",
    y = "Ocorrências",
    fill = "Estado"
  ) +
  theme_minimal()
ggsave("fig/01_exploracao/eficacia.png", plot = grafico_eficacia, width = 10, height = 6)

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
ggsave("fig/01_exploracao/evolucao_locais.png", plot = grafico_evolucao, width = 10, height = 6)

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
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Auditoria de Qualidade: Percentagem de Dados em Falta por Coluna",
    subtitle = "Inclui valores NA e strings literais '(null)'",
    x = "Nome da Coluna",
    y = "Percentagem de Valores Omissos (%)"
  ) +
  theme_minimal()
ggsave("fig/01_exploracao/auditoria_dados_falta.png", plot = grafico_auditoria, width = 10, height = 7)

#--------------------------------------------------------------------------------------------------------------
# MAPA DE CALOR (HORA VS DIA DA SEMANA) 
crimes_heatmap <- nypd |> 
  filter(!is.na(hour), !is.na(wday)) |> 
  count(wday, hour)
grafico_heatmap <- ggplot(crimes_heatmap, aes(x = hour, y = wday, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = scales::comma) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Mapa de Calor: Concentração de Crimes por Hora e Dia da Semana",
    x = "Hora do Dia",
    y = "Dia da Semana",
    fill = "Ocorrências"
  ) +
  theme_minimal()
dir.create("fig/01_exploracao", showWarnings = FALSE, recursive = TRUE)
ggsave("fig/01_exploracao/heatmap_hora_dia.png", plot = grafico_heatmap, width = 10, height = 6)

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
    "FELONY"      = "orange2", 
    "MISDEMEANOR" = "steelblue", 
    "VIOLATION"   = "green4"  
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
ggsave("fig/01_exploracao/gravidade_por_bairro.png", plot = grafico_gravidade, width = 10, height = 6)

#----------------------------------------------------------------------------------------------------------------
# TENDÊNCIA MENSAL / SAZONALIDADE 
crimes_mes <- nypd |> 
  filter(!is.na(occurred_on), !old_occurrence) |> 
  mutate(mes = lubridate::month(occurred_on, label = TRUE, abbr = TRUE)) |> 
  count(mes)
grafico_mes <- ggplot(crimes_mes, aes(x = mes, y = n, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2.5) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  labs(
    title = "Sazonalidade: Distribuição de Crimes por Mês do Ano",
    x = "Mês",
    y = "Ocorrências Totais"
  ) +
  theme_minimal()
dir.create("fig/01_exploracao", showWarnings = FALSE, recursive = TRUE)
ggsave("fig/01_exploracao/sazonalidade_mensal.png", plot = grafico_mes, width = 9, height = 5)
