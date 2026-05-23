################################################################################
# Monografia - FRICÇÕES E HETEROGENEIDADE: UMA ANÁLISE ESTRUTURAL E CAUSAL 
#              DO CHOQUE PANDÊMICO NO MERCADO DE TRABALHO JUVENIL BRASILEIRO 
#
# Autor: Alexandre Bezerra dos Santos - Economia - UFPE
# Orientador: Prof. Dr. Cristiano da Costa da Silva
# 
# Etapa: Estimação do DiD, Event Study e DMP
# Data de última edição: 22/05/2026
# Versão: 1.1
################################################################################

# ==========================================
# ESTIMAÇÃO DIF-in-DIF
# ==========================================
library(dplyr)
library(purrr)
library(tidyr)
library(flextable)
library(fixest)
library(stringr)
library(stringr)
library(modelsummary)
library(ggplot2)

# 1. Tratamento variáveis explicativas e de período
dados_transicao <- readRDS("data/base_tcc_pronta_reg.rds")
dados_reg <- dados_transicao %>% 
  mutate(
    # assumido choque a partir de 2020Q2 (8082), ate 2021Q4 (8088)
    periodo = case_when(
      tempo_absoluto < 8082 ~ "1_Pre_Pandemia",
      tempo_absoluto >= 8082 & tempo_absoluto <= 8088 ~ "2_Durante_Pandemia",
      tempo_absoluto > 8088 ~ "3_Pos_Pandemia"
    ),
    
    periodo = as.factor(periodo),
    
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
    experiencia = factor(experiencia),
    
    tempo_relativo = tempo_absoluto - 8082,
    
    is_pardo = ifelse(raca == "Parda", 1, 0),
    is_preto = ifelse(raca == "Preta", 1, 0),
    is_superior = ifelse(escolaridade == "3_Superior", 1, 0),
    is_jovem = ifelse(experiencia == "2_Jovem", 1, 0),
    is_jovem_adulto = ifelse(experiencia == "3_Jovem_Adulto", 1, 0)
  )

rm(dados_transicao)

# 2. Estimação dos Modelos de Probabilidade Linear (LPM - DiD)
modelo_did_base <- feols(
  transicao_emprego ~ periodo * (mulher + raca + escolaridade + localidade +
                                   experiencia) | Trimestre_Ano + UF,
  data = dados_reg,
  weights = ~V1028,
  cluster = ~UF
)

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

# 3. Estimação dos Estudos de Eventos
dados_grafico <- dados_reg %>%
  filter(tempo_relativo >= -12 & tempo_relativo <= 12)

modelo_event_study_pardo_base <- feols(
  transicao_emprego ~ i(tempo_relativo, is_pardo, ref = -1) + 
    mulher + escolaridade + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico,
  weights = ~V1028,
  cluster = ~UF 
)

modelo_event_study_robusto <- feols(
  transicao_emprego ~ i(tempo_relativo, is_pardo, ref = -1) + 
    is_pardo:tempo_absoluto + 
    mulher + escolaridade + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico,
  weights = ~V1028,
  cluster = ~UF 
)

modelo_es_jovens_adultos <- feols(
  transicao_emprego ~ i(tempo_relativo, is_jovem_adulto, ref = -1) + 
    is_jovem_adulto:tempo_absoluto + 
    mulher + raca + escolaridade + localidade 
  | Trimestre_Ano + UF, 
  data = dados_grafico, weights = ~V1028, cluster = ~UF 
)

modelo_es_superior <- feols(
  transicao_emprego ~ i(tempo_relativo, is_superior, ref = -1) + 
    is_superior:tempo_absoluto + 
    mulher + raca + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico, weights = ~V1028, cluster = ~UF 
)

modelo_es_pretos <- feols(
  transicao_emprego ~ i(tempo_relativo, is_preto, ref = -1) + 
    is_preto:tempo_absoluto +
    mulher + escolaridade + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico, weights = ~V1028, cluster = ~UF 
)

modelo_es_mulheres <- feols(
  transicao_emprego ~ i(tempo_relativo, mulher, ref = -1) + 
    mulher:tempo_absoluto + 
    raca + escolaridade + localidade + experiencia 
  | Trimestre_Ano + UF, 
  data = dados_grafico, weights = ~V1028, cluster = ~UF 
)

modelo_es_jovem <- feols(
  transicao_emprego ~ i(tempo_relativo, is_jovem, ref = -1) + 
    is_jovem:tempo_absoluto + 
    mulher + raca + escolaridade + localidade 
  | Trimestre_Ano + UF, 
  data = dados_grafico, weights = ~V1028, cluster = ~UF 
)


# ==================
# ESTIMAÇÃO DMP
# ==================
base_dmp_macro <- readRDS("data/base_macro_dmp.rds")
dados_caged_trimestral <- readRDS("data/base_caged_matches.rds")

# 1. Construção de Painel Regional
dic_uf <- data.frame(
  UF_Sigla = c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB",
               "PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS",
               "MT","GO","DF"),
  
  UF = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins",
         "Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
         "Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais",
         "Espírito Santo","Rio de Janeiro","São Paulo","Paraná",
         "Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul",
         "Mato Grosso","Goiás","Distrito Federal"))

dados_painel_regional <- base_dmp_macro %>%
  left_join(dic_uf, by = "UF") %>% 
  inner_join(dados_caged_trimestral, by = c("Ano", "Trimestre", "UF_Sigla")) %>%
  mutate(
    Trimestre_Ano = paste0(Ano, "Q", Trimestre),
    ln_M = log(M_Matches_Admissoes),
    ln_U = log(U_Total),
    ln_E = log(E_Total)
  ) %>%
  filter(is.finite(ln_M), is.finite(ln_U), is.finite(ln_E))

# 2. Estimação do parâmetro alpha (elasticidade do desemprego)
modelo_matching <- feols(
  ln_M ~ ln_U + ln_E | Trimestre_Ano + UF_Sigla,
  data = dados_painel_regional,
  cluster = ~UF_Sigla
)

alpha_tcc <- coef(modelo_matching)["ln_U"]

# 3. Agregação e Calibração Estrutural
dados_dmp_nacional <- dados_painel_regional %>%
  group_by(Ano, Trimestre) %>%
  summarise(
    U_Total = sum(U_Total, na.rm = TRUE),
    E_Total = sum(E_Total, na.rm = TRUE),
    PEA_Total = sum(PEA_Total, na.rm = TRUE),
    M_Admissoes = sum(M_Matches_Admissoes, na.rm = TRUE),
    S_Desligamentos = sum(Desligamentos_Totais, na.rm = TRUE),
    .groups = "drop"
  )

dados_calibrados <- dados_dmp_nacional %>%
  mutate(
    Ano_num = as.numeric(as.character(Ano)),
    Trim_num = as.numeric(as.character(Trimestre)),
    Tempo = Ano_num + (Trim_num - 1) / 4,
    Periodo = case_when(
      Ano < 2020 ~ "1_Pre-Pandemia",
      Ano %in% c(2020, 2021) ~ "2_Pandemia",
      Ano > 2021 ~ "3_Pos-Pandemia"
    ),
    u = U_Total / PEA_Total,
    f = M_Admissoes / U_Total,
    s = S_Desligamentos / E_Total,
    alpha_real = alpha_tcc,
    A_eff = M_Admissoes / ((U_Total^alpha_real) * (E_Total^(1 - alpha_real))),
    v_proxy = M_Admissoes / PEA_Total, 
    theta_proxy = v_proxy / u,
    u_star = s / (s + f)
    )

