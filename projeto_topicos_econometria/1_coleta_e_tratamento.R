################################################################################
# Projeto de Pesquisa - IMPACTO DA TARIFA ZERO NO TRANSPORTE PÚBLICO SOBRE O
#                       EMPREGO FORMAL DO COMÉRCIO LOCAL
#
# Autor: Alexandre Bezerra dos Santos - Economia - UFPE
# 
# Data de última edição: 26/05/2026
# Versão: 1.0
################################################################################

# Carregamento das bibliotecas necessárias
library(basedosdados)
library(tidyverse)
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)

# ==========================================
# 1. COLETA DE DADOS
# ==========================================

# Importação de Tabela Tarifa Zero
df_tarifa_zero <- read_csv("dados/tarifa_zero.csv") 

# Coleta de Dados do CAGED
set_billing_id("didipdf")
query_caged <- "
  SELECT 
    ano, 
    mes, 
    id_municipio,
    SUM(saldo_movimentacao) AS saldo_empregos
  FROM `basedosdados.br_me_caged.microdados_movimentacao`
  WHERE ano >= 2020
    AND cnae_2_secao IN ('G', 'I') -- G: Comércio; I: Alojamento e Alimentação
  GROUP BY ano, mes, id_municipio
  ORDER BY id_municipio, ano, mes
"
dados_caged_mensal <- read_sql(query_caged)

# Coleta de Dados de População
query_populacao <- "
  SELECT 
    id_municipio, 
    populacao
  FROM `basedosdados.br_ibge_populacao.municipio`
  WHERE ano = 2019
"

df_populacao <- read_sql(query_populacao) %>%
  mutate(
    codigo_ibge = as.numeric(as.character(id_municipio)),
    populacao = as.numeric(as.character(populacao))
    ) %>%
  select(codigo_ibge, populacao)

saveRDS(dados_caged_mensal, "dados/dados_caged_mensal.rds")
saveRDS(df_populacao, "dados/df_populacao.rds")

# ==========================================
# 2. TRATAMENTO
# ==========================================

# CAGED
df_caged_final <- dados_caged_mensal %>%
  filter(!is.na(id_municipio)) %>%
  mutate(
    codigo_ibge = as.numeric(as.character(id_municipio)),
    periodo = as.numeric(as.character((ano * 100) + mes)),
    saldo_empregos = as.numeric(as.character(saldo_empregos))
  ) %>%
  select(codigo_ibge, periodo, saldo_empregos)

# balanceamento
df_caged_final <- df_caged_final %>%
  tidyr::complete(
    codigo_ibge, 
    periodo, 
    fill = list(saldo_empregos = 0)
  )

# Tarifa Zero
df_tratamento <- df_tarifa_zero %>%
  mutate(
    codigo_ibge = as.numeric(as.character(codigo_ibge)),
    coorte_tratamento = (as.numeric(as.character(ano_inicio)) * 100) + as.numeric(as.character(mes_inicio))
  ) %>%
  select(codigo_ibge, coorte_tratamento)

# Merge
painel_did <- df_caged_final %>%
  left_join(df_tratamento, by = "codigo_ibge") %>%
  left_join(df_populacao, by = "codigo_ibge") %>% 
  mutate(
    coorte_tratamento = ifelse(is.na(coorte_tratamento), 0, coorte_tratamento),
    populacao = as.numeric(as.character(populacao))
  ) %>% 
  filter(!is.na(populacao)) %>%
  filter(populacao >= 30000 & populacao <= 400000)

glimpse(painel_did)

# Correção do Painel
painel_corrigido <- painel_did %>%
  mutate(
    ano_atual = floor(periodo / 100),
    mes_atual = periodo %% 100,
    tempo_seq = (ano_atual - 2020) * 12 + mes_atual,
    ano_trat = floor(coorte_tratamento / 100),
    mes_trat = coorte_tratamento %% 100,
    coorte_seq = ifelse(coorte_tratamento == 0, 0, (ano_trat - 2020) * 12 + mes_trat),
    saldo_norm = (saldo_empregos / populacao) * 1000
  ) %>%
  filter(!is.na(saldo_norm) & !is.na(tempo_seq))

# ==========================================
# 3. ESTIMAÇÃO DE CALLAWAY & SANT'ANNA (2021)
# ==========================================
# Estimação I
out_att <- att_gt(
  yname = "saldo_empregos",       # Sua variável dependente
  tname = "periodo",              # O tempo numérico (ex: 202301)
  idname = "codigo_ibge",         # O ID do município
  gname = "coorte_tratamento",    # O tempo de adoção (0 para controle)
  data = painel_did,     # Nossa base filtrada e limpa!
  control_group = "nevertreated", # O controle são as cidades que nunca adotaram
  bstrap = TRUE,                  # Usa Bootstrap para erros-padrão robustos
  anticipation = 0,
  cband = FALSE,
  panel = FALSE,
  allow_unbalanced_panel = TRUE
)

# Estimação II (Normalizada)
out_att_corrigido <- att_gt(
  yname = "saldo_norm",           # USAMOS A NOVA VARIÁVEL NORMALIZADA
  tname = "tempo_seq",            # USAMOS O TEMPO SEQUENCIAL (1, 2, 3...)
  idname = "codigo_ibge",
  gname = "coorte_seq",           # USAMOS A COORTE SEQUENCIAL
  data = painel_corrigido,
  control_group = "nevertreated",
  bstrap = TRUE,
  cband = FALSE,                  # Mantemos FALSE para estabilidade
  panel = FALSE,
  allow_unbalanced_panel = TRUE
)


# ==========================================
# 4. ESTUDO DE EVENTOS (AGREGAÇÃO DINÂMICA)
# ==========================================

# Agregação I
#out_es <- aggte(out_att, type = "dynamic")

# Agregação II
out_es_corrigido <- aggte(out_att_corrigido, type = "dynamic")

# Resumo estatístico no console
summary(out_es_corrigido)

# Plotar o gráfico com intervalos pontuais
ggdid(out_es_corrigido) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Efeito da Tarifa Zero no Emprego Local (Comércio e Serviços)",
    subtitle = "Painel balanceado e corrigido para inflação demográfica",
    x = "Meses antes e depois da adoção da Tarifa Zero (0 = Mês da Lei)",
    y = "Saldo de Empregos (por 1.000 habitantes)"
  )


grafico_final <- ggdid(out_es_corrigido)
grafico_final +
  theme_minimal(base_size = 14) + # Aumenta ligeiramente a letra para o ecrã
  coord_cartesian(xlim = c(-24, 24)) + 
  scale_x_continuous(breaks = seq(-24, 24, by = 6)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  labs(
    title = "Impacto da Tarifa Zero no Emprego (Comércio e Serviços)",
    subtitle = "Estudo de Eventos: Janela restrita de 24 meses em torno da implementação",
    x = "Meses relativos à implementação (0 = Início da Tarifa Zero)",
    y = "Saldo de Empregos (por 1.000 habitantes)",
    caption = "Fonte: Elaboração própria com dados do Novo CAGED, IBGE e Santini (2024)."
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey30"),
    panel.grid.minor = element_blank() # Remove as linhas de grelha secundárias
  )
