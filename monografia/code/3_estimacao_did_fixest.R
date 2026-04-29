################################################################################
# Projeto: Monografia - Juventude e Emprego: Choque e Recuperação Heterogênos
# Pós-Pandemia
# Script: Criação de Dummies e Estimação do DiD
# Autor: Alexandre Bezerra dos Santos - Economia - UFPE
# Orientador: Cristiano da Costa da Silva
# Data: 29/04/2026
# Versão: 1.0
#
# Descrição:
# Este script cria dummies utilizada na estimação do DiD. Devido os resultados,
# é feita uma segunda estimação com Tendência Linear Prévia.
#
# Dados:
# Painel de dados juvenis tratado.
#
# Requisitos:
# - R versão >= 4.0
# - Pacotes: dplyr, fixest, stringr e modelsummary
#
# Licença:
# Este código está licenciado sob os termos da licença MIT.
# Você pode reutilizá-lo, modificá-lo e distribuí-lo, com os devidos créditos.
################################################################################

# 1. Carregamento das bibliotecas necessárias
library(dplyr)
library(fixest)
library(stringr)
library(modelsummary)


# 2. Carregar base
dados_reg <- readRDS("../data/base_tcc_pronta_reg.rds")


# 3. Tratamento variáveis explicativas e de período
dados_reg <- dados_reg %>% 
  mutate(
    # definição das fases da pandemia em tempo absoluto
    # assumido choque a partir de 2020Q2 (8082), ate 2021Q4 (8088)
    periodo = case_when(
      tempo_absoluto < 8082 ~ "1_Pre_Pandemia",
      tempo_absoluto >= 8082 & tempo_absoluto <= 8088 ~ "2_Durante_Pandemia",
      tempo_absoluto > 8088 ~ "3_Pos_Pandemia"
    ),
    periodo = as.factor(periodo),
    
    # criacao de dummies
    mulher = ifelse(str_detect(V2007, "Mulher"), 1, 0),
    
    raca = case_when(
      str_detect(V2010, "Branca") ~ "Branca",
      str_detect(V2010, "Preta") ~ "Preta",
      str_detect(V2010, "Parda") ~ "Parda",
      TRUE ~ "Outras"
    ),
    raca = factor(raca, levels = c("Branca","Preta","Parda","Outras")),
    
    escolaridade = case_when(
      str_detect(str_to_lower(V3009A),
                 "médio|2º grau|científico|clássico") ~ "2_Medio",
      str_detect(str_to_lower(V3009A),
                 "superior|mestrado|doutorado|especialização") ~ "3_Superior",
      is.na(V3009A)  ~ "4_Ainda_Estuda_ou_Sem_Info",
      TRUE ~ "1_Fundamental"
    ),
    escolaridade = factor(escolaridade),
    
    localidade = ifelse(str_detect(str_to_lower(V1022), "urbana"),
                        "Urbana", "Rural"),
    localidade = factor(localidade, levels=c("Rural","Urbana")),
    
    experiencia = case_when(
      V2009 >= 14 & V2009 <= 17 ~ "1_Adolescente",
      V2009 >= 18 & V2009 <= 24 ~ "2_Jovem",
      V2009 >= 25 & V2009 <= 29 ~ "3_Jovem_Adulto"
    ),
    experiencia = factor(experiencia)
  )


# 4. Estimação do Modelo de Probabilidade Linear (LPM - DiD)
# rodando modelo
modelo_did_lpm <- feols(
  transicao_emprego ~ periodo * (mulher + raca + escolaridade + localidade +
                                   experiencia) | Trimestre_Ano + UF,
  data = dados_reg,
  weights = ~V1028,
  cluster = ~UF
)

# visualizando o sumário
etable(modelo_did_lpm, fitstat = c("n", "r2", "wr2"))


# 6. Event Studies
# Eixo de Tempo Relativo e dummies
dados_reg <- dados_reg %>% 
  mutate(
    tempo_relativo = tempo_absoluto - 8082,
    is_pardo = ifelse(raca == "Parda", 1, 0),
    is_preto = ifelse(raca == "Preta", 1, 0),
    is_superior = ifelse(escolaridade == "3_Superior", 1, 0),
    is_jovem_adulto = ifelse(experiencia == "3_Jovem_Adulto", 1, 0)
  )


# 7. Event Study

dados_grafico <- dados_reg %>%
  filter(tempo_relativo >= -12 & tempo_relativo <= 12)

# Modelo Event Study (Jovens Pardos) Base
modelo_event_study <- feols(
  transicao_emprego ~ i(tempo_relativo, is_pardo, ref = -1) + 
    mulher + escolaridade + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico,
  weights = ~V1028,
  cluster = ~UF 
)

png(filename = "../graphics/Event_Study_Pardos_Base_AltaRes.png", 
    width = 16, height = 8, units = "in", res = 600)

iplot(modelo_event_study, 
      main = "Efeito Dinâmico da Pandemia na Transição para o Emprego (Jovens Pardos)",
      xlab = "Trimestres em relação ao início da Pandemia (0 = 2020Q2)",
      ylab = "Efeito na Prob. de Transição (Pontos Percentuais)",
      pt.pch = 16, col = "#2c3e50", ci.col = "#2980b9", ci.width = 0.2, 
      grid = TRUE, ref.line = 0) 

dev.off()


# Modelo Event Study (Jovens Pardos) Ajustado por Tendência Linear Prévia
modelo_event_study_ajustado <- feols(
  transicao_emprego ~ i(tempo_relativo, is_pardo, ref = -1) + 
    is_pardo:tempo_absoluto + 
    mulher + escolaridade + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico,
  weights = ~V1028,
  cluster = ~UF 
)

png(filename = "../graphics/Event_Study_Pardos_Ajustado_AltaRes.png", 
    width = 16, height = 8, units = "in", res = 600)

iplot(modelo_event_study_ajustado, 
      main = "Efeito Dinâmico: Jovens Pardos",
      xlab = "Trimestres em relação ao início da Pandemia (0 = 2020Q2)",
      ylab = "Efeito na Prob. de Transição (Pontos Percentuais)",
      pt.pch = 16, col = "#2c3e50", ci.col = "#2980b9", ci.width = 0.2, 
      grid = TRUE, ref.line = 0)

dev.off()

# Modelo Event Study (Jovens Pretos)
modelo_es_pretos <- feols(
  transicao_emprego ~ i(tempo_relativo, is_preto, ref = -1) + 
    is_preto:tempo_absoluto +
    mulher + escolaridade + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico, weights = ~V1028, cluster = ~UF 
)

png(filename = "../graphics/Event_Study_Pretos_Ajustado_AltaRes.png", 
    width = 16, height = 8, units = "in", res = 600)

iplot(modelo_es_pretos, 
      main = "Efeito Dinâmico: Jovens Pretos",
      xlab = "Trimestres em relação ao início da Pandemia (0 = 2020Q2)",
      ylab = "Efeito na Prob. de Transição (Pontos Percentuais)",
      pt.pch = 16, col = "#8e44ad", ci.col = "#9b59b6", ci.width = 0.2, 
      grid = TRUE, ref.line = 0)

dev.off()

# Modelo Event Study (Jovens Mulheres)
modelo_es_mulheres <- feols(
  transicao_emprego ~ i(tempo_relativo, mulher, ref = -1) + 
    mulher:tempo_absoluto + 
    raca + escolaridade + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico, weights = ~V1028, cluster = ~UF 
)

png(filename = "../graphics/Event_Study_Mulheres_Ajustado_AltaRes.png", 
    width = 16, height = 8, units = "in", res = 600)

iplot(modelo_es_mulheres, 
      main = "Efeito Dinâmico: Mulheres",
      xlab = "Trimestres em relação ao início da Pandemia (0 = 2020Q2)",
      ylab = "Efeito na Prob. de Transição (Pontos Percentuais)",
      pt.pch = 16, col = "#c0392b", ci.col = "#d35400", ci.width = 0.2, 
      grid = TRUE, ref.line = 0)

dev.off()

# Modelo Event Study (Jovens Superior)
modelo_es_superior <- feols(
  transicao_emprego ~ i(tempo_relativo, is_superior, ref = -1) + 
    is_superior:tempo_absoluto + 
    mulher + raca + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico, weights = ~V1028, cluster = ~UF 
)

png(filename = "../graphics/Event_Study_Superior_Ajustado_AltaRes.png", 
    width = 16, height = 8, units = "in", res = 600)

iplot(modelo_es_superior, 
      main = "Efeito Dinâmico: Ensino Superior",
      xlab = "Trimestres em relação ao início da Pandemia (0 = 2020Q2)",
      ylab = "Efeito na Prob. de Transição (Pontos Percentuais)",
      pt.pch = 16, col = "#27ae60", ci.col = "#2ecc71", ci.width = 0.2, 
      grid = TRUE, ref.line = 0)

dev.off()

# Modelo Event Study (Jovens Adultos)
modelo_es_jovens_adultos <- feols(
  transicao_emprego ~ i(tempo_relativo, is_jovem_adulto, ref = -1) + 
    is_jovem_adulto:tempo_absoluto + 
    mulher + raca + escolaridade + localidade 
  | Trimestre_Ano + UF, 
  data = dados_grafico, weights = ~V1028, cluster = ~UF 
)

png(filename = "../graphics/Event_Study_Adultos_Ajustado_AltaRes.png", 
    width = 16, height = 8, units = "in", res = 600)

iplot(modelo_es_jovens_adultos, 
      main = "Efeito Dinâmico: Jovens Adultos (25-29 anos)",
      xlab = "Trimestres em relação ao início da Pandemia (0 = 2020Q2)",
      ylab = "Efeito na Prob. de Transição (Pontos Percentuais)",
      pt.pch = 16, col = "#f39c12", ci.col = "#e67e22", ci.width = 0.2, 
      grid = TRUE, ref.line = 0)

dev.off()


# Gerar Gráficos e Tabelas para Impressão
mapa_coeficientes <- c(
  "mulher" = "Mulher",
  "racaPreta" = "Preto",
  "racaParda" = "Pardo",
  "escolaridade2_Medio" = "Ensino Médio",
  "escolaridade3_Superior" = "Ensino Superior",
  "experiencia2_Jovem" = "Jovem (18-24 anos)",
  "experiencia3_Jovem_Adulto" = "Jovem Adulto (25-29 anos)",
  "periodo2_Durante_Pandemia:racaParda" = "Durante Pandemia × Pardo",
  "periodo3_Pos_Pandemia:racaParda" = "Pós-Pandemia × Pardo",
  "periodo2_Durante_Pandemia:racaPreta" = "Durante Pandemia × Preto",
  "periodo3_Pos_Pandemia:racaPreta" = "Pós-Pandemia × Preto",
  "periodo2_Durante_Pandemia:escolaridade3_Superior" = "Durante Pandemia × Ens. Superior",
  "periodo2_Durante_Pandemia:experiencia3_Jovem_Adulto" = "Durante Pandemia × Jovem Adulto"
)

estatisticas_rodape <- list(
  list("raw" = "nobs", "clean" = "Observações", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R²", "fmt" = 3)
)

modelos_tabela <- list(
  "DiD Principal" = modelo_did_lpm,
  "Event Study (Pardos)" = modelo_event_study_ajustado,
  "Event Study (Pretos)" = modelo_es_pretos,
  "Event Study (Mulheres)" = modelo_es_mulheres,
  "Event Study (Superior)" = modelo_es_superior,
  "Event Study (Jovens Adultos)" = modelo_es_jovens_adultos
)


modelsummary(
  modelos_tabela,
  coef_map = mapa_coeficientes,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = estatisticas_rodape,
  title = "Tabela 1 - Modelos de Probabilidade Linear (DiD) para Transição ao Emprego",
  notes = c(
    "Fonte: Elaborado pelo autor (2026) com base nos microdados da PNAD Contínua.",
    "Nota: Erros-padrão clusterizados ao nível da UF entre parênteses. Todos os modelos incluem efeitos fixos de Trimestre e Estado (UF)."
  ),
  output = "../graphics/Tabela_Resultados_TCC.html" 
)


# 8. Modelo DiD Robusto
modelo_did_robusto <- feols(
  transicao_emprego ~ periodo * (mulher + raca + escolaridade + localidade +
                                   experiencia) +
   raca:tempo_relativo + 
    mulher:tempo_relativo + 
    escolaridade:tempo_relativo + 
    experiencia:tempo_relativo + 
    localidade:tempo_relativo
  | Trimestre_Ano + UF, 
  data = dados_reg, weights = ~V1028, cluster = ~UF 
)

# 2. Atualizando a Lista de Modelos para a Tabela
modelos_tabela_final <- list(
  "(1) DiD Base (Sem Tendência)" = modelo_did_lpm,
  "(2) DiD Robusto (Com Tendência)" = modelo_did_robusto
)

# 3. Gerando a Tabela Definitiva
modelsummary(
  modelos_tabela_final,
  coef_map = mapa_coeficientes, # Usando o mesmo mapa elegante que criamos antes
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = estatisticas_rodape,
  title = "Tabela 1 - Efeito da Pandemia na Transição para o Emprego (Modelos DiD)",
  notes = c(
    "Fonte: Elaborado pelo autor (2026) com base nos microdados da PNAD Contínua.",
    "Nota: Erros-padrão clusterizados ao nível da UF entre parênteses. O Modelo (2) inclui tendências temporais lineares específicas para cada característica sociodemográfica."
  ),
  output = "../graphics/Tabela_Resultados_Definitiva_TCC.html" 
)
