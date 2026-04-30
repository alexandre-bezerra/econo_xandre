################################################################################
# Projeto: Monografia - Juventude e Emprego: Choque e Recuperação Heterogênos 
# Pós-Pandemia
# Autor: Alexandre Bezerra dos Santos - Economia - UFPE
# Orientador: Cristiano da Costa da Silva
# Data: 29/04/2026
# Versão: 1.0
################################################################################

# Carregamento das bibliotecas necessárias
#install.packages(c("dplyr", 'purrr', 'PNADcIBGE', 'basedosdados', 'tidyr', 'fixest', 'stringr', 'modelsummary', 'ggplot2'))
library(dplyr)
library(purrr)
library(PNADcIBGE)
library(basedosdados)
library(tidyr)
library(fixest)
library(stringr)
library(modelsummary)
library(ggplot2)

# ==========================================
# 1. COLETA DE DADOS
# ==========================================

# Preparação de pasta de checkpoints para segurança da coleta
dir.create("../data/checkpoints_tcc", showWarnings = FALSE)

#Definição de variáveis importante
variaveis_alvo <- c(
  "Ano", "Trimestre", "UF", 
  "UPA", "Estrato", "V1028",      # Desenho Amostral (Pesos)
  "V1008", "V1014", "V2003",      # Variáveis de Domicílio
  "V2007", "V2009", "V2010",      # Demográficas (Sexo, Idade, Cor)
  "V3009A", "V1022",              # Escolaridade e Localização
  "VD4001", "VD4002"              # Força de trabalho e Ocupação
)

# Função de Coleta, Filtro e Backup
f_cfb <- function(ano, trimestre) {
  
  # Define o nome do arquivo de checkpoint para cada trimestre coletado
  arquivo_checkpoint <- file.path("../data/checkpoints_tcc",
                                  sprintf("dados_%d_Q%d.rds",
                                          ano, trimestre))
  
  # Verificação da existência do arquivo
  if (file.exists(arquivo_checkpoint)) {
    cat(sprintf("-> Checkpoint encontrado para %d Q%d. Pulando download...\n",
                ano, trimestre))
    return(readRDS(arquivo_checkpoint))
  }
  
  cat(sprintf("Baixando Ano: %d | Trimestre: %d...\n", ano, trimestre))
  
  resultado <- tryCatch({
    
    # Baixa a base bruta e filtra as variáveis
    base_bruta <- get_pnadc(year = ano, quarter = trimestre,
                            vars = variaveis_alvo, design = FALSE)
    
    base_bruta <- base_bruta %>% 
      select("Ano", "Trimestre", "UF", 
             "UPA", "Estrato", "V1028",      # Desenho Amostral (Pesos)
             "V1008", "V1014", "V2003",      # Variáveis de Domicílio
             "V2007", "V2009", "V2010",      # Demográficas (Sexo, Idade, Cor)
             "V3009A", "V1022",              # Escolaridade e Localização
             "VD4001", "VD4002")
    
    # Expansão para a população real via pesos amostrais (v1028), para
    # agregados do DMP
    macro_trimestre <- base_bruta %>%
      filter(V2009 >= 14 & V2009 <= 29) %>% # Filtro vital para o TCC
      group_by(Ano = ano, Trimestre = trimestre, UF) %>% # Agrupamento Estadual
      summarise(
        U_Total = sum(V1028[VD4002 == "Pessoas desocupadas" 
                            & VD4001 == "Pessoas na força de trabalho"],
                      na.rm = TRUE),
        E_Total = sum(V1028[VD4002 == "Pessoas ocupadas" 
                            & VD4001 == "Pessoas na força de trabalho"],
                      na.rm = TRUE),
        PEA_Total = sum(V1028[VD4001 == "Pessoas na força de trabalho"],
                        na.rm = TRUE),
        .groups = "drop"
      )
    
    # Filtro da base para indivíduos jovens (14-29 anos) para estimações
    base_jovens <- base_bruta %>%
      filter(V2009 >= 14 & V2009 <= 29,
             VD4001 == "Pessoas na força de trabalho")
    
    # Agrupamento do resultado e armazenamento do progresso
    resultado_lista <- list(macro = macro_trimestre, micro = base_jovens)
    saveRDS(resultado_lista, arquivo_checkpoint)
    
    # Limpeza de dados
    rm(base_bruta)
    gc()
    dir_temp <- tempdir()
    arquivos_lixo <- list.files(dir_temp, full.names = TRUE,
                                pattern = "\\.zip$|\\.txt$|PNADC")
    unlink(arquivos_lixo, recursive = TRUE, force = TRUE)
    
    return(resultado_lista)
    
  }, error = function(e) {
    cat(sprintf("Erro no trimestre %d Q%d: %s\n", ano, trimestre, e$message))
    return(NULL)
  })
  
  return(resultado)
}

# Criação de Grade de Tempo e Execução
grade_tempo <- expand.grid(trimestre = 1:4, ano = 2012:2025) %>%
  arrange(ano, trimestre)
resultados_completos <- map2(grade_tempo$ano, grade_tempo$trimestre, f_cfb)
resultados_completos <- compact(resultados_completos)


# Separação e armazenamento
base_dmp_macro <- map_dfr(resultados_completos, "macro")
base_tcc_jovens <- map_dfr(resultados_completos, "micro")
saveRDS(base_dmp_macro, "../data/base_macro_dmp.rds")
saveRDS(base_tcc_jovens, "../data/base_tcc_jovens_bruta.rds")

# Coleta de Dados do CAGED
set_billing_id("didipdf")
query_caged_jovens <- "
WITH caged_completo AS (
  -- Dados do CAGED Antigo (Até 2019)
  SELECT ano, mes, sigla_uf,
         (CASE WHEN saldo_movimentacao = 1 THEN 1 ELSE 0 END) as admissoes,
         (CASE WHEN saldo_movimentacao = -1 THEN 1 ELSE 0 END) as desligamentos
  FROM `basedosdados.br_me_caged.microdados_antigos`
  WHERE ano >= 2012 AND idade BETWEEN 14 AND 29
  
  UNION ALL
  
  -- Dados do Novo CAGED (De 2020 em diante)
  SELECT ano, mes, sigla_uf,
         (CASE WHEN saldo_movimentacao = 1 THEN 1 ELSE 0 END) as admissoes,
         (CASE WHEN saldo_movimentacao = -1 THEN 1 ELSE 0 END) as desligamentos
  FROM `basedosdados.br_me_caged.microdados_movimentacao`
  WHERE ano >= 2020 AND idade BETWEEN 14 AND 29
)
SELECT ano, mes, sigla_uf, 
       SUM(admissoes) as admissoes_jovens, 
       SUM(desligamentos) as desligamentos_jovens
FROM caged_completo
GROUP BY ano, mes, sigla_uf
ORDER BY ano, mes, sigla_uf
"

dados_caged_mensal <- read_sql(query_caged_jovens)

# Painel Trimestral
dados_caged_trimestral <- dados_caged_mensal %>%
  mutate(
    Trimestre = case_when(
      mes %in% 1:3 ~ 1,
      mes %in% 4:6 ~ 2,
      mes %in% 7:9 ~ 3,
      mes %in% 10:12 ~ 4
    )
  ) %>%
  group_by(ano, Trimestre, sigla_uf) %>%
  summarise(
    M_Matches_Admissoes = sum(admissoes_jovens, na.rm = TRUE),
    Desligamentos_Totais = sum(desligamentos_jovens, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Ano = ano, UF_Sigla = sigla_uf)

saveRDS(dados_caged_trimestral, "../data/base_caged_matches.rds")


# Limpeza
rm(list = setdiff(ls(), c("base_dmp_macro", "base_tcc_jovens",
                          "dados_caged_trimestral")))

# ==========================================
# 2. EMPARELHAMENTO
# ==========================================

# Formatação dos Dados e criação de identificadores
base_tcc_jovens <- base_tcc_jovens %>% 
  mutate(
    Ano = as.numeric(as.character(Ano)),
    Trimestre = as.numeric(as.character(Trimestre)),
    V2009 = as.numeric(as.character(V2009)),
    V1028 = as.numeric(as.character(V1028)),
    UPA = as.character(UPA),
    V1008 = as.character(V1008),
    V1014 = as.character(V1014),
    V2003 = as.character(V2003),
    V2007 = as.character(V2007),
    VD4002 = as.character(VD4002),
    VD4001 = as.character(VD4001),
    Trimestre_Ano = as.factor(paste0(Ano, "Q", Trimestre)),
    UF = as.factor(UF),
    id_dom = paste(UPA, V1008, V1014, sep = "_"),
    id_pessoa = paste(id_dom, V2003, sep = '_'),
    tempo_absoluto = Ano * 4 + Trimestre
  ) %>% 
  arrange(id_pessoa, tempo_absoluto)

# Filtro de Ribas & Soares (2008) e variável de transição
dados_transicao <- base_tcc_jovens %>% 
  group_by(id_pessoa) %>% 
  mutate(
    lag_tempo = lag(tempo_absoluto),
    lag_sexo = lag(V2007),
    lag_raca = lag(V2010),
    lag_idade = lag(V2009),
    lag_estado = lag(VD4002),
    
    # Checagem de mesmo indivíduo
    mesma_pessoa = case_when(
      # se é a primeira vez
      is.na(lag_tempo) ~ FALSE,
      # se há buracos entre entrevistas
      tempo_absoluto - lag_tempo != 1 ~ FALSE,
      # se mudou de sexo
      V2007 != lag_sexo ~ FALSE,
      # se mudou de raça
      V2010 != lag_raca ~ FALSE,
      # se a diferença de idade é menor ou maior que 1
      (V2009 - lag_idade) < 0 | (V2009 - lag_idade) > 1 ~ FALSE,
      TRUE ~ TRUE
    )
  ) %>%
  # filtrando os que se mantêm
  filter(mesma_pessoa == TRUE) %>% 
  
  # criação de variável dependente do DiD
  mutate(
    # se transitou do desemprego para o emprego = 1
    transicao_emprego = ifelse(
      lag_estado == "Pessoas desocupadas" & VD4002 == "Pessoas ocupadas", 1, 0
    )
  ) %>% 
  ungroup()

saveRDS(dados_transicao, "../data/base_tcc_pronta_reg.rds")
cat("Tamanho final do painel validado:", nrow(dados_transicao), "transicoes.")

# Limpeza
rm(list = setdiff(ls(), c("dados_transicao","base_dmp_macro", "dados_caged_trimestral")))

# ==========================================
# 3. ESTIMAÇÃO DiD
# ==========================================

# Tratamento variáveis explicativas e de período
dados_reg <- dados_transicao %>% 
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
    experiencia = factor(experiencia),
    tempo_relativo = tempo_absoluto - 8082,
    is_pardo = ifelse(raca == "Parda", 1, 0),
    is_preto = ifelse(raca == "Preta", 1, 0),
    is_superior = ifelse(escolaridade == "3_Superior", 1, 0),
    is_jovem_adulto = ifelse(experiencia == "3_Jovem_Adulto", 1, 0)
  )

rm(dados_transicao)

# Estimação do Modelo de Probabilidade Linear (LPM - DiD)
modelo_did_lpm <- feols(
  transicao_emprego ~ periodo * (mulher + raca + escolaridade + localidade +
                                   experiencia) | Trimestre_Ano + UF,
  data = dados_reg,
  weights = ~V1028,
  cluster = ~UF
)

# Preparação para Event Study
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


# Modelo DiD Robusto
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

# Atualizando a Lista de Modelos para a Tabela
modelos_tabela_final <- list(
  "(1) DiD Base (Sem Tendência)" = modelo_did_lpm,
  "(2) DiD Robusto (Com Tendência)" = modelo_did_robusto
)

# Gerando a Tabela Definitiva
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

# Limpeza
rm(list = setdiff(ls(), c("base_dmp_macro", "dados_caged_trimestral")))


# ==========================================
# 4. ESTIMAÇÃO DA FUNÇÃO DE MATCHING E CALIBRAÇÃO DMP
# ==========================================

# 4.1 Dicionário para cruzar UF (Extenso) com UF (Sigla)
dic_uf <- data.frame(
  UF_Sigla = c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF"),
  UF = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins","Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo","Paraná","Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal")
)

# 4.2 Construindo o Painel Regional (UF x Trimestre)
dados_painel_regional <- base_dmp_macro %>%
  # Trazendo a Sigla para a base da PNAD
  left_join(dic_uf, by = "UF") %>% 
  # Cruzando PNAD com CAGED através da Sigla, Ano e Trimestre
  inner_join(dados_caged_trimestral, by = c("Ano", "Trimestre", "UF_Sigla")) %>%
  mutate(
    Trimestre_Ano = paste0(Ano, "Q", Trimestre),
    # Transformação Logarítmica para a Regressão de Cobb-Douglas
    ln_M = log(M_Matches_Admissoes),
    ln_U = log(U_Total),
    ln_E = log(E_Total) # Proxy para escala de Vagas (V) e Ciclo Econômico
  ) %>%
  filter(is.finite(ln_M), is.finite(ln_U), is.finite(ln_E))

# 4.3 ESTIMAÇÃO INÉDITA DO ALPHA (Elasticidade do Desemprego)
cat("\nEstimando a Função de Matching Juvenil via TWFE...\n")
modelo_matching <- feols(
  ln_M ~ ln_U + ln_E | Trimestre_Ano + UF_Sigla,
  data = dados_painel_regional,
  cluster = ~UF_Sigla
)

# Exportando a tabela para o seu TCC
modelsummary(
  list("Matching Cobb-Douglas (Jovens)" = modelo_matching),
  estimate = "{estimate}{stars}", statistic = "({std.error})",
  title = "Tabela 2 - Estimação da Função de Matching Juvenil (2012-2024)",
  output = "../graphics/Tabela_Matching_TCC.html"
)

# O pulo do gato: Extraindo o seu Alpha estimado pelo modelo!
alpha_tcc <- coef(modelo_matching)["ln_U"]
cat(sprintf("-> O parâmetro Alpha (elasticidade) estimado pelo modelo é: %.3f\n", alpha_tcc))

# 4.4 Agregação Macro Nacional (O Brasil como um todo para o DMP)
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

# 4.5 Calibração Estrutural com o SEU Alpha
dados_calibrados <- dados_dmp_nacional %>%
  mutate(
    Tempo = Ano + (Trimestre - 1) / 4,
    Periodo = case_when(
      Ano < 2020 ~ "1_Pre-Pandemia",
      Ano %in% c(2020, 2021) ~ "2_Pandemia",
      Ano > 2021 ~ "3_Pos-Pandemia"
    ),
    u = U_Total / PEA_Total,
    f = M_Admissoes / U_Total,
    s = S_Desligamentos / E_Total,
    
    # Usando o Alpha gerado no passo 4.3
    alpha_real = alpha_tcc,
    
    # Inversão da função para achar a eficiência (A) e Tensionamento (theta)
    theta_proxy = (f)^(1 / (1 - alpha_real)),
    v_proxy = theta_proxy * u,
    A_eff = M_Admissoes / ((U_Total^alpha_real) * ((v_proxy * PEA_Total)^(1 - alpha_real))),
    u_star = s / (s + f)
  )

# 4.6 Paradoxo de Shimer (Simulação de Rigidez)
dados_simulacao <- dados_calibrados %>%
  mutate(
    # Hosios (Salário Flexível = Ajuste no Preço)
    u_star_flexivel = ifelse(Periodo == "2_Pandemia", s / (s + (f * 1.3)), u_star),
    # Shimer (Salário Rígido = Destruição Massiva de Vagas)
    u_star_rigido = ifelse(Periodo == "2_Pandemia", (s * 1.3) / ((s * 1.3) + (f * 0.7)), u_star)
  )

# 4.7 Exportação dos Gráficos Macroeconômicos
grafico_beveridge <- ggplot(dados_calibrados, aes(x = u, y = v_proxy, color = Periodo)) +
  geom_path(aes(group = 1), color = "gray80", size = 0.5, arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(values = c("#27ae60", "#c0392b", "#2980b9")) +
  theme_minimal(base_size = 14) +
  labs(title = "Curva de Beveridge do Mercado Juvenil (2012-2024)",
       subtitle = "Deslocamento da Eficiência de Matching (A)",
       x = "Taxa de Desemprego (u)", y = "Taxa de Vagas (Proxy v)", color = "Fase")

dados_plot_beta <- dados_simulacao %>%
  select(Tempo, u_star, u_star_flexivel, u_star_rigido) %>%
  pivot_longer(cols = starts_with("u_star"), names_to = "Modelo", values_to = "Desemprego_Equilibrio") %>%
  mutate(Modelo = case_when(
    Modelo == "u_star" ~ "Observado (Baseline)",
    Modelo == "u_star_flexivel" ~ "Simulado: Salário Flexível (Hosios)",
    Modelo == "u_star_rigido" ~ "Simulado: Salário Rígido (Shimer)"
  ))

grafico_shimer <- ggplot(dados_plot_beta, aes(x = Tempo, y = Desemprego_Equilibrio, color = Modelo, linetype = Modelo)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2020.0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("black", "#2980b9", "#c0392b")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.direction = "vertical") +
  labs(title = "Desemprego u* sob Diferentes Poderes de Barganha (\U03B2)",
       x = "Tempo", y = "Desemprego u* (%)")

ggsave("../graphics/Curva_Beveridge_Estrutural.png", plot = grafico_beveridge, width = 10, height = 7, dpi = 600)
ggsave("../graphics/Simulacao_Shimer_Beta.png", plot = grafico_shimer, width = 10, height = 7, dpi = 600)

cat("\nPipeline completamente executado com sucesso!\n")