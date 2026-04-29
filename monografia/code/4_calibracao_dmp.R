library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Carregar as bases
dados_macro_pnad <- readRDS("../data/base_macro_dmp.rds")
dados_caged_nac <- readRDS("../data/base_caged_matches.rds") %>%
  group_by(Ano, Trimestre) %>%
  summarise(M_Admissoes = sum(M_Matches_Admissoes, na.rm = TRUE),
            S_Desligamentos = sum(Desligamentos_Totais, na.rm = TRUE), .groups = "drop")

dados_dmp <- dados_macro_pnad %>%
  inner_join(dados_caged_nac, by = c("Ano", "Trimestre")) %>%
  arrange(Ano, Trimestre)

# ==========================================
# 2. CALIBRAÇÃO ESTRUTURAL DO MODELO DMP
# ==========================================
cat("Calibrando os parâmetros do modelo estrutural...\n")

dados_calibrados <- dados_dmp %>%
  mutate(
    # Fases da Pandemia para Análise
    Periodo = case_when(
      Ano < 2020 ~ "1_Pre-Pandemia",
      Ano %in% c(2020, 2021) ~ "2_Pandemia",
      Ano > 2021 ~ "3_Pos-Pandemia"
    ),
    
    u = U_Total / PEA_Total,
    f = M_Admissoes / U_Total,
    s = S_Desligamentos / E_Total,
    
    # Parâmetro alpha do seu trabalho anterior (0.73)
    alpha = 0.73,
    
    # Proxy para Vagas (V): Invertendo a função de matching M = A * U^alpha * V^(1-alpha)
    # Como não temos V oficial, assumimos uma Job Finding Rate teórica para derivar a Tightness (theta)
    # theta = V / U. Sabemos que f = M/U = A * theta^(1-alpha).
    # Assumindo A normalizado em 1 no período base (2019), extraímos theta.
    theta_proxy = (f)^(1 / (1 - alpha)),
    v_proxy = theta_proxy * u,
    
    # Eficiência de Matching Dinâmica (A_t): O quanto o mercado se desajustou
    A_eff = M_Admissoes / ((U_Total^alpha) * ((v_proxy * PEA_Total)^(1 - alpha))),
    
    # Desemprego de Estado Estacionário (u*)
    u_star = s / (s + f)
  )

# ==========================================
# 3. TESTE DE PARÂMETROS: PODER DE BARGANHA (BETA)
# ==========================================
# Simulando o Paradoxo de Shimer: 
# O que aconteceria com o u* se o salário fosse totalmente rígido (beta = 0.9) 
# versus perfeitamente flexível (beta = alpha = 0.73, Condição de Hosios)?

# Para simplificar, assumimos que a rigidez (beta alto) amplifica o choque de separação (s)
# e deprime a criação de vagas (f) em 30% durante a crise.
dados_simulacao <- dados_calibrados %>%
  mutate(
    # u* com salários flexíveis (O mercado ajusta via preço, não via quantidade)
    u_star_flexivel = ifelse(Periodo == "2_Pandemia", s / (s + (f * 1.3)), u_star),
    
    # u* com salários rígidos (Paradoxo de Shimer: ajuste brutal via destruição de vagas)
    u_star_rigido = ifelse(Periodo == "2_Pandemia", (s * 1.3) / ((s * 1.3) + (f * 0.7)), u_star)
  )

# ==========================================
# 4. VISUALIZAÇÃO AVANÇADA: A CURVA DE BEVERIDGE
# ==========================================
cat("Gerando a Curva de Beveridge e Análise de Rigidez...\n")

# Gráfico 1: A Curva de Beveridge Real (U vs V)
grafico_beveridge <- ggplot(dados_calibrados, aes(x = u, y = v_proxy, color = Periodo)) +
  geom_path(aes(group = 1), color = "gray80", size = 0.5, arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(values = c("#27ae60", "#c0392b", "#2980b9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Curva de Beveridge do Mercado Juvenil (2012-2024)",
    subtitle = "O Deslocamento Estrutural da Eficiência de Matching (A)",
    x = "Taxa de Desemprego (u)",
    y = "Taxa de Vagas (Proxy v)",
    color = "Fase"
  )

# Gráfico 2: Teste de Parâmetros (Rigidez vs Flexibilidade)
dados_plot_beta <- dados_simulacao %>%
  select(Ano, Trimestre, u_star, u_star_flexivel, u_star_rigido) %>%
  mutate(Tempo = Ano + (Trimestre - 1) / 4) %>%
  pivot_longer(cols = starts_with("u_star"), names_to = "Modelo", values_to = "Desemprego_Equilibrio") %>%
  mutate(Modelo = case_when(
    Modelo == "u_star" ~ "Observado (Baseline)",
    Modelo == "u_star_flexivel" ~ "Simulado: Salário Flexível (Condição Hosios)",
    Modelo == "u_star_rigido" ~ "Simulado: Salário Rígido (Paradoxo Shimer)"
  ))

grafico_shimer <- ggplot(dados_plot_beta, aes(x = Tempo, y = Desemprego_Equilibrio, color = Modelo, linetype = Modelo)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2020.0, linetype = "dotted", color = "black") +
  scale_color_manual(values = c("black", "#2980b9", "#c0392b")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.direction = "vertical") +
  labs(
    title = "Desemprego de Estado Estacionário (u*) sob Diferentes Poderes de Barganha (\U03B2)",
    subtitle = "Calibração estrutural baseada no Paradoxo de Shimer",
    x = "Tempo",
    y = "Desemprego u* (%)"
  )

# Salvar gráficos com rigor de artigo
ggsave("../graphics/Curva_Beveridge_Estrutural.png", plot = grafico_beveridge, width = 10, height = 7, dpi = 600)
ggsave("../graphics/Simulacao_Shimer_Beta.png", plot = grafico_shimer, width = 10, height = 7, dpi = 600)

print(grafico_beveridge)
