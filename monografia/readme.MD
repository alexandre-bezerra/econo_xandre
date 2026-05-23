# Fricções e Heterogeneidade: Uma Análise Estrutural e Causal do Choque Pandêmico no Mercado de Trabalho Juvenil Brasileiro

**Autor:** Alexandre Bezerra dos Santos  
**Orientador:** Prof. Dr. Cristiano da Costa da Silva  
**Instituição:** Universidade Federal de Pernambuco (UFPE) - Departamento de Economia  

---

## 1. Visão Geral da Pesquisa

Este repositório contém os scripts em `R`, bases de dados tratadas e as saídas gráficas do Trabalho de Conclusão de Curso (TCC). A pesquisa investiga o impacto estrutural e causal da pandemia de COVID-19 sobre a probabilidade de inserção de jovens no mercado de trabalho formal, unindo duas abordagens complementares:
1. **Microeconometria (Causal):** Estimação de Modelos de Diferenças-em-Diferenças (DiD) com Efeitos Fixos Bidirecionais (TWFE) e Estudos de Evento (*Event Studies*) utilizando painéis rotativos da PNAD Contínua emparelhados pelo método de Ribas e Soares (2008).
2. **Macroeconomia (Estrutural):** Calibração empírica da Função de *Matching* e do modelo Diamond-Mortensen-Pissarides (DMP), simulando o Paradoxo de Shimer e analisando o deslocamento da Curva de Beveridge com dados do (Novo) CAGED.

Este `README` serve como um **Guia Metodológico Suplementar**, detalhando e justificando as escolhas econométricas, as parametrizações visuais e as limitações inerentes aos dados utilizados.

---

## 2. Justificativas Metodológicas (Fundamentação Teórica)

Para garantir a robustez da identificação causal e a coerência teórica do modelo de busca, as seguintes decisões metodológicas foram adotadas no código:

### 2.1. Uso do Modelo Linear de Probabilidade (MLP) em vez de Logit/Probit
Embora a variável dependente seja binária (*dummy* de Transição para o Emprego), optou-se pela estimação via **Modelo Linear de Probabilidade (OLS/MLP)** dentro do arcabouço *Two-Way Fixed Effects* (TWFE). 
* **Justificativa:** A introdução de milhares de variáveis *dummy* para controlar os Efeitos Fixos não-observáveis de UF e Trimestre em modelos não-lineares (Logit/Probit) gera o viés de **Problema dos Parâmetros Incidentais** (*Incidental Parameters Problem*) em painéis curtos. Ademais, a literatura moderna de DiD (Wooldridge, 2010; Callaway & Sant'Anna, 2021) padronizou o uso do MLP, pois permite interpretar os coeficientes diretamente como Efeitos Marginais em pontos percentuais (p.p.), facilitando a extração do $\delta$ causal médio sem as distorções das razões de chance (*odds ratios*).

### 2.2. Introdução de Tendências Lineares Específicas ("Modelo Robusto")
No Modelo Base, grupos historicamente vulneráveis (menor escolaridade e não-brancos) pareciam apresentar um "prêmio" contraintuitivo de transição pré-2020. Isso configurava uma violação severa da premissa de **Tendências Paralelas**.
* **Justificativa:** Conforme demonstrado por Angrist e Pischke (2009, *Mostly Harmless Econometrics*), quando os grupos de tratamento e controle possuem trajetórias de crescimento distintas antes do evento, deve-se incluir **tendências temporais específicas por grupo** (*group-specific linear trends*). Economicamente, isso limpa o modelo da **Inércia Precária** (Pastore, 2019) — a alta rotatividade estrutural de jovens periféricos em subempregos informais. Ao saturar o modelo com essas tendências, o estimador isola estritamente a "quebra" provocada pelo choque sanitário.

### 2.3. A *Proxy* para Vagas e o Ruído do "Novo CAGED"
O Brasil não dispõe de um relatório oficial de vagas abertas e não preenchidas comparável ao JOLTS norte-americano. 
* **Justificativa:** Seguindo a literatura empírica nacional do modelo DMP, utilizou-se o fluxo bruto de Admissões Formais do CAGED como a melhor *proxy* observável para a demanda das firmas ($V$).
* **O Salto da Eficiência ($A$):** O modelo capta um aumento abrupto na eficiência de *matching* após 2020. O TCC fundamenta que este salto é majoritariamente um **ruído institucional contabilístico** gerado pela obrigatoriedade do Novo CAGED (integração ao e-Social), que passou a captar vínculos antes subnotificados. Além disso, reflete o que denominamos "Eficiência da Exclusão": as firmas passaram a triar algoritmicamente apenas a "elite" escolarizada, tornando o processo seletivo mais rápido para a base amostral que restou.

---

## 3. Guia de Leitura dos Gráficos e Atualizações Visuais

Os gráficos gerados pelo script `3_graficos_e_tabelas.R` foram desenhados com rigor editorial ABNT. A leitura de cada visualização obedece às seguintes lógicas:

### 3.1. Estudos de Evento (*Event Studies* - Gráficos 2 a 6)
* **Como ler:** As barras (intervalos de confiança) que cruzam a linha do zero a partir de 2022 **não indicam recuperação plena**. A nota de rodapé das imagens elucida que o eixo Y representa a *evolução da desigualdade de transição* em relação ao grupo base (ex: Homens, Brancos) e ao período pré-choque ($t=-1$).
* **Significado:** Quando o efeito passa a não ter significância estatística no longo prazo, indica-se uma **convergência por baixo**. O mercado de trabalho estagnou em um novo equilíbrio excludente, evidenciando o Efeito Cicatriz (*Scarring Effect*), onde o distanciamento intra-grupos persiste pelo desalento e depreciação do capital humano.

### 3.2. Curva de Beveridge (Gráfico 8)
* **Ajuste de Escala:** Para evitar o achatamento visual causado pela distância percentual entre os eixos (Desemprego $\approx 25\%$ vs. Vagas $\approx 2\%$), os limites foram calibrados para focar estritamente na nuvem de dados, mantendo a escala natural percentual (`scales::percent_format`).
* **Interpretação:** A seta tracejada desenhada entre as medianas intertemporais corrobora empiricamente a expansão do *skill mismatch* (desalinhamento de competências), onde o mercado sustenta altas taxas de vacância e de desemprego simultaneamente.

### 3.3. Gráficos Macroeconômicos DMP (Variável Tempo Contínuo)
* **Periodicidade:** Todos os gráficos estruturais (Eficiência, Pissarides, Shimer) foram convertidos de anuais para **Trimestrais Contínuos** (ex: 2020.25 para 2020Q2).
* **Justificativa:** A agregação anual ocultaria o choque instantâneo do 2º trimestre de 2020, gerando saltos em formato de "degrau" (*step-functions*). A variável contínua preserva as curvas orgânicas de colapso de admissões ($f$) e pico de demissões ($s$).

---

## 4. Limitações do Estudo

Ao relatar os achados, a pesquisa reconhece formalmente as seguintes restrições:
1. **Quebra de Série Institucional:** O advento do Novo CAGED em 2020Q1 gera um viés de medição irremovível para o lado da firma. Tentou-se contornar o viés via Efeitos Fixos Trimestrais, mas a série de vagas pós-pandemia está intrinsecamente superestimada frente ao histórico.
2. **Alta Variância nas Caudas:** No modelo de *Event Study*, a ampliação dos intervalos de confiança a partir do 8º trimestre pós-choque limita a capacidade de afirmar que a exclusão continua a se agravar.

---

## 5. Síntese dos Resultados

1. **A Fuga para a Qualidade (Micro):** Sob incerteza, as firmas elevaram a seletividade. Jovens adultos (25-29 anos) e com Ensino Superior foram utilizados como "escudos de produtividade", penalizando entrantes e adolescentes.
2. **Cicatriz Racial:** Pardos sofreram uma penalização persistente e adicional nas suas chances de transição, validando hipóteses de discriminação estatística acentuada pela triagem remota/digital.
3. **Rigidez Institucional (Shimer Paradox):** A simulação estrutural provou que, impossibilitadas de reduzir salários devido ao piso institucional brasileiro, as firmas absorveram o choque de produtividade unicamente via quantidades, interrompendo a contratação e elevando as separações ($s$).

---
*Repositório mantido como anexo técnico à monografia apresentada ao curso de Ciências Econômicas da UFPE.*