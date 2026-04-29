################################################################################
# Projeto: Monografia - Juventude e Emprego: Choque e Recuperação Heterogênos 
# Pós-Pandemia
# Script: Coleta e Filtro de Microdados da PNAD Contínua (2012–2025), e do CAGED
# Autor: Alexandre Bezerra dos Santos - Economia - UFPE
# Orientador: Cristiano da Costa da Silva
# Data: 28/04/2026
# Versão: 1.0
#
# Descrição:
# Este script faz o download dos microdados da PNAD Contínua 
# Trimestral de 2012 a 2025, estima dados agregados de emprego e filtra 
# por idade, e baixa dados do CAGED.
#
# Uso dos Dados:
# Dados públicos do IBGE, obtidos via PNAD Contínua Trimestral (2012–2025),
# e do CAGED.
#
# Requisitos:
# - R versão >= 4.0
# - Pacotes: dplyr, purr, PNADcIBGE e basedosdados
#
# Licença:
# Este código está licenciado sob os termos da licença MIT.
# Você pode reutilizá-lo, modificá-lo e distribuí-lo, com os devidos créditos.
################################################################################

# 1. Carregamento das bibliotecas necessárias
#install.packages(c("dplyr", 'purrr', 'PNADcIBGE', 'basedosdados'))
library(dplyr)
library(purrr)
library(PNADcIBGE)
library(basedosdados)


# 2. Preparação de pasta de checkpoints para segurança da coleta
dir.create("../data/checkpoints_tcc", showWarnings = FALSE)


# 3. Definição de variáveis importante
variaveis_alvo <- c(
  "Ano", "Trimestre", "UF", 
  "UPA", "Estrato", "V1028",      # Desenho Amostral (Pesos)
  "V1008", "V1014", "V2003",      # Variáveis de Domicílio
  "V2007", "V2009", "V2010",      # Demográficas (Sexo, Idade, Cor)
  "V3009A", "V1022",              # Escolaridade e Localização
  "VD4001", "VD4002"              # Força de trabalho e Ocupação
)


# 4. Função de Coleta, Filtro e Backup
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
      summarise(
        Ano = ano,
        Trimestre = trimestre,
        # Total de Desempregados
        U_Total = sum(V1028[VD4002 == "Pessoas desocupadas" 
                            & VD4001 == "Pessoas na força de trabalho"],
                      na.rm = TRUE),
        # Total de Empregados
        E_Total = sum(V1028[VD4002 == "Pessoas ocupadas" 
                            & VD4001 == "Pessoas na força de trabalho"],
                      na.rm = TRUE),
        # Força de Trabalho Total
        PEA_Total = sum(V1028[VD4001 == "Pessoas na força de trabalho"],
                        na.rm = TRUE)
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


# 5. Criação de Grade de Tempo e Execução
grade_tempo <- expand.grid(trimestre = 1:4, ano = 2012:2025) %>%
  arrange(ano, trimestre)
resultados_completos <- map2(grade_tempo$ano, grade_tempo$trimestre, f_cfb)
resultados_completos <- compact(resultados_completos)


# 4. Separação e armazenamento
base_dmp_macro <- map_dfr(resultados_completos, "macro")
base_tcc_jovens <- map_dfr(resultados_completos, "micro")
saveRDS(base_dmp_macro, "../data/base_macro_dmp.rds")
saveRDS(base_tcc_jovens, "../data/base_tcc_jovens_bruta.rds")

# 5. Coleta de Dados do CAGED
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
