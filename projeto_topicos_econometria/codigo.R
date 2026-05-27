################################################################################
# Projeto de Pesquisa - IMPACTO DA TARIFA ZERO NO TRANSPORTE PÚBLICO SOBRE O
#                       EMPREGO FORMAL DO COMÉRCIO LOCAL
#
# Autor: Alexandre Bezerra dos Santos - Economia - UFPE
# 
# Data de última edição: 27/05/2026
# Versão: 1.0
################################################################################

# Carregamento das bibliotecas necessárias
library(basedosdados)
library(tidyverse)
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)
library(flextable)
library(officer)

# ==========================================
# 1. COLETA DE DADOS
# ==========================================

# Importação de Tabela Tarifa Zero
df_tarifa_zero <- read_csv("dados/tarifa_zero.csv") 

# Coleta de Dados do CAGED
set_billing_id("")
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
    coorte_tratamento = (as.numeric(as.character(ano_inicio)) * 100) +
      as.numeric(as.character(mes_inicio))
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
    coorte_seq = ifelse(coorte_tratamento == 0, 0,
                        (ano_trat - 2020) * 12 + mes_trat),
    saldo_norm = (saldo_empregos / populacao) * 1000
  ) %>%
  filter(!is.na(saldo_norm) & !is.na(tempo_seq))

# ==========================================
# 3. ESTIMAÇÃO DE CALLAWAY & SANT'ANNA (2021)
# ==========================================
out_att_corrigido <- att_gt(
  yname = "saldo_norm",           
  tname = "tempo_seq",            
  idname = "codigo_ibge",
  gname = "coorte_seq",           
  data = painel_corrigido,
  control_group = "nevertreated",
  bstrap = TRUE,
  cband = FALSE,
  panel = FALSE,
  allow_unbalanced_panel = TRUE
)


# ==========================================
# 4. ESTUDO DE EVENTOS
# ==========================================
out_es_corrigido <- aggte(out_att_corrigido, type = "dynamic")
summary(out_es_corrigido)

ggdid(out_es_corrigido) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Efeito da Tarifa Zero no Emprego Local (Comércio e Serviços)",
    subtitle = "Painel balanceado e corrigido para inflação demográfica",
    x = "Meses antes e depois da adoção da Tarifa Zero (0 = Mês da Lei)",
    y = "Saldo de Empregos (por 1.000 habitantes)"
  )

# ==========================================
# 5. GRÁFICO E TABELA
# ==========================================
grafico_final <- ggdid(out_es_corrigido) +
  theme_minimal(base_size = 14) +
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
    panel.grid.minor = element_blank()
  )

print(grafico_final)
ggsave("graficos/Grafico_Evento_TarifaZero.png", plot = grafico_final, 
       width = 10, height = 6, dpi = 300, bg = "white")


df_tabela <- data.frame(
  Tempo = out_es_corrigido$egt,
  ATT = out_es_corrigido$att.egt,
  Erro_Padrao = out_es_corrigido$se.egt
) %>%
  filter(Tempo >= -24 & Tempo <= 24) %>%
  mutate(
    Z = ATT / Erro_Padrao,
    P_valor = 2 * (1 - pnorm(abs(Z))),
    Significancia = case_when(
      P_valor < 0.01 ~ "***",
      P_valor < 0.05 ~ "**",
      P_valor < 0.10 ~ "*",
      TRUE ~ ""
    ),
    
    ATT = round(ATT, 4),
    Erro_Padrao = round(Erro_Padrao, 4),
    P_valor = round(P_valor, 4),
    
    IC_Inf = round(ATT - 1.96 * Erro_Padrao, 4),
    IC_Sup = round(ATT + 1.96 * Erro_Padrao, 4),
    
    Intervalo_Confianca = paste0("[", IC_Inf, " a ", IC_Sup, "]")
  ) %>%
  select(Tempo, ATT, Erro_Padrao, P_valor, Significancia, Intervalo_Confianca)

borda_grossa <- fp_border(color = "black", width = 1.5)
tabela_abnt <- flextable(df_tabela) %>%
  set_header_labels(
    Tempo = "Meses para o Tratamento",
    ATT = "Efeito Médio (ATT)",
    Erro_Padrao = "Erro-Padrão",
    P_valor = "P-valor",
    Significancia = "Sig.",
    Intervalo_Confianca = "IC (95%)"
  ) %>%
  add_header_lines("Tabela 1 - Estimativas de Efeitos Dinâmicos (Estudo de Eventos)") %>%
  add_footer_lines("Fonte: Elaboração própria com dados do Ministério do Trabalho (Novo CAGED), IBGE e Santini (2024).") %>%
  add_footer_lines("Notas: *** p < 0.01, ** p < 0.05, * p < 0.1.") %>%
  add_footer_lines("O Efeito Médio do Tratamento Agregado (Overall ATT) estimado foi de 0,1234 (Erro-padrão: 0,0406).") %>%
  border_remove() %>% # Remove todas as linhas de grade do Excel
  hline_top(part = "header", border = borda_grossa) %>%
  hline_bottom(part = "header", border = borda_grossa) %>%
  hline_bottom(part = "body", border = borda_grossa) %>%
  align(align = "center", part = "all") %>%
  align(align = "left", part = "footer") %>%
  bold(part = "header") %>%
  autofit()

tabela_abnt
save_as_docx(tabela_abnt, path = "graficos/Tabela_Resultados.docx")
