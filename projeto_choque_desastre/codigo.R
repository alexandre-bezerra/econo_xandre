################################################################################
# Pesquisa - O EFEITO DO FPM NA MITIGAÇÃO DE DANOS POR DESASTRE NATURAIS
#
# Autor: Alexandre Bezerra dos Santos - Economia - UFPE
# 
# Data de última edição: 28/05/2026
# Versão: 1.0
# ETAPAS 1 e 2: Coleta e Tratamento de Dados
################################################################################

# Carregando pacotes
library(tidyverse)
library(basedosdados)
library(janitor)

# ------------------------------------------------------------------------------
# 1. COLETA DOS DADOS
# ------------------------------------------------------------------------------

# A. População do IBGE via Base dos Dados
# Substitua 'seu-project-id' pelo ID do seu projeto no Google Cloud
set_billing_id("didipdf") 

query_ibge <- "
SELECT 
    ano, 
    id_municipio, 
    populacao
FROM `basedosdados.br_ibge_populacao.municipio`
WHERE ano BETWEEN 2013 AND 2023
"
df_populacao <- read_sql(query_ibge)

# B. Microdados de Desastres do S2iD (Defesa Civil)
# Lendo direto da URL ou do arquivo local baixado do Gov.br
# Usamos read_csv2 pois o padrão brasileiro costuma usar ';' como separador
url_s2id <- "https://dados.mdr.gov.br/dataset/c4b2b1a1-3700-4813-81b3-76472dfcc156/resource/1/download/fide.csv"

# O S2iD pode ter problemas de encoding (latin1), ajustamos isso na leitura
df_s2id_raw <- read_csv2(url_s2id, 
                         locale = locale(encoding = "latin1"),
                         show_col_types = FALSE) |> 
  clean_names() # Padroniza nomes das colunas para minúsculas e sem espaços

# ------------------------------------------------------------------------------
# 2. TRATAMENTO E AGREGAÇÃO
# ------------------------------------------------------------------------------

# Agregando os danos do S2iD por município e ano
df_s2id_agg <- df_s2id_raw |> 
  # Selecionando colunas de interesse (nomes limpos pelo janitor)
  select(ano, codigo_ibge, cobrade, dh_desabrigados, dh_desalojados, 
         dm_obras_infraestrutura_publica_reais) |> 
  # Transformando colunas financeiras em numéricas (removendo possíveis vírgulas)
  mutate(dano_infra = as.numeric(str_replace_all(dm_obras_infraestrutura_publica_reais, ",", "."))) |> 
  group_by(ano, codigo_ibge) |> 
  summarise(
    total_desabrigados = sum(dh_desabrigados, na.rm = TRUE),
    total_desalojados  = sum(dh_desalojados, na.rm = TRUE),
    dano_infra_total   = sum(dano_infra, na.rm = TRUE),
    qtd_desastres      = n(),
    .groups = "drop"
  )

# Ajustando a chave do município para o Merge (IBGE 7 dígitos)
# Convertendo para character para garantir que o join funcione perfeitamente
df_populacao <- df_populacao |> mutate(id_municipio = as.character(id_municipio))
df_s2id_agg  <- df_s2id_agg |> mutate(codigo_ibge = as.character(codigo_ibge))

# Realizando o Left Join
# Municípios que não sofreram desastres ficarão com NA nos danos. 
# Precisamos substituir esses NAs por 0.
df_painel <- df_populacao |> 
  left_join(df_s2id_agg, by = c("ano" = "ano", "id_municipio" = "codigo_ibge")) |> 
  mutate(
    across(c(total_desabrigados, total_desalojados, dano_infra_total, qtd_desastres), 
           ~replace_na(., 0))
  )

# ------------------------------------------------------------------------------
# CRIANDO AS VARIÁVEIS PARA O RDD (Corte FPM = 10.188 hab)
# ------------------------------------------------------------------------------
corte_fpm <- 10188

df_painel <- df_painel |> 
  mutate(
    # Variável indicadora de Tratamento
    tratamento = ifelse(populacao >= corte_fpm, 1, 0),
    
    # Running Variable centralizada
    distancia_corte = populacao - corte_fpm,
    
    # Transformação em log para os danos financeiros (lidando com zeros)
    log_dano_infra = log(dano_infra_total + 1)
  ) |> 
  # Filtrando apenas municípios próximos ao corte para evitar outliers extremos
  # Mantendo apenas cidades com até 20 mil habitantes para essa análise
  filter(populacao <= 20000)

# Checando a estrutura final do painel
glimpse(df_painel)