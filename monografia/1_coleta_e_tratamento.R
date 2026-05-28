################################################################################
# Monografia - FRICÇÕES E HETEROGENEIDADE: UMA ANÁLISE ESTRUTURAL E CAUSAL 
#              DO CHOQUE PANDÊMICO NO MERCADO DE TRABALHO JUVENIL BRASILEIRO 
#
# Autor: Alexandre Bezerra dos Santos - Economia - UFPE
# Orientador: Prof. Dr. Cristiano da Costa da Silva
# 
# Etapa: Coleta de Dados da PNAD e do CAGED, filtro e emparelhamento
# Data de última edição: 22/05/2026
# Versão: 1.1
################################################################################

# Carregamento das bibliotecas necessárias
#install.packages(c("dplyr", 'purrr', 'PNADcIBGE', 'basedosdados', 'tidyr',
#'fixest', 'stringr', 'modelsummary', 'ggplot2'))
library(dplyr)
library(purrr)
library(PNADcIBGE)
library(basedosdados)
library(tidyr)
library(flextable)
library(fixest)
library(stringr)
library(modelsummary)
library(ggplot2)

# ==========================================
# 1. COLETA DE DADOS
# ==========================================

# Preparação de pasta de checkpoints para segurança da coleta
dir.create("data/checkpoints_tcc", showWarnings = FALSE)

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
  arquivo_checkpoint <- file.path("data/checkpoints_tcc",
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
      filter(V2009 >= 14 & V2009 <= 29) %>%
      group_by(Ano = ano, Trimestre = trimestre, UF) %>%
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
saveRDS(base_dmp_macro, "data/base_macro_dmp.rds")
saveRDS(base_tcc_jovens, "data/base_tcc_jovens_bruta.rds")

# Coleta de Dados do CAGED
set_billing_id("")
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

saveRDS(dados_caged_trimestral, "data/base_caged_matches.rds")


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

saveRDS(dados_transicao, "data/base_tcc_pronta_reg.rds")
cat("Tamanho final do painel validado:", nrow(dados_transicao), "transicoes.")

# Limpeza
rm(list = setdiff(ls(), c("dados_transicao","base_dmp_macro", "dados_caged_trimestral")))