################################################################################
# Projeto: Monografia - Juventude e Emprego: Choque e Recuperação Heterogênos
# Pós-Pandemia
# Script: Emparelhamento Logitudinal dos dados juvenis coletados
# Autor: Alexandre Bezerra dos Santos - Economia - UFPE
# Orientador: Cristiano da Costa da Silva
# Data: 29/04/2026
# Versão: 1.0
#
# Descrição:
# Este script cria identificadores únicos para os indivíduos entrevistados,
# filtra pelo método desenvolvido em Ribas & Soares (2008) os que são visitados
# mais de uma vez em períodos subsequentes, e cria variável de transição
# ocupacional.
#
# Dados:
# Dados da PNAD Contínua Trimestral (2012–2025) já filtrados
# pelas variáveis relevantes e idade (jovens de 14 a 29 anos).
#
# Requisitos:
# - R versão >= 4.0
# - Pacotes: dplyr, tidyr
#
# Licença:
# Este código está licenciado sob os termos da licença MIT.
# Você pode reutilizá-lo, modificá-lo e distribuí-lo, com os devidos créditos.
################################################################################

# 1. Carregamento das bibliotecas necessárias
library(dplyr)
library(tidyr)


# 2. Carregamento da Base de Dados Juvenis
dados <- readRDS("../data/base_tcc_jovens_bruta.rds")
cat("Dados brutos carregados!")


# 3. Formatação dos Dados
dados <- dados %>% 
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
    UF = as.factor(UF)
    )


# 4. Criação de Identificadores Únicos e ordenação temporal
dados_painel <- dados %>% 
  mutate(
    # Identificador do Domicílio: UPA + N. Domicílio + Grupo de Rotação
    id_dom = paste(UPA, V1008, V1014, sep = "_"),
    # Identificador do Indivíduo: Domicílio + N. Ordem da Família
    id_pessoa = paste(id_dom, V2003, sep = '_'),
    # Linha do tempo trimestral
    tempo_absoluto = Ano * 4 + Trimestre
  ) %>% 
  # Ordenação por pessoa e tempo
  arrange(id_pessoa, tempo_absoluto)


# 5. Filtro de Ribas & Soares (2008) e variável de transição
dados_transicao <- dados_painel %>% 
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


# 6. Armazenando base pronta
saveRDS(dados_transicao, "../data/base_tcc_pronta_reg.rds")


cat("Tamanho final do painel validado:", nrow(dados_transicao), "transicoes.")