### CÓDIGO DA ANÁLISE/ESTUDO ACERCA DO GRAU DE TRANSVERSALIDADE MENSURADO (VIs) OCASIONANDO A EFICIÊNCIA DE POLÍTICAS PÚBLICAS (VD)
### PESQUISA QUANTITATIVA: A PARTIR DE UMA ANÁLISE DE REGRESSÃO LINEAR MÚLTIPLA 
### CPRI/CFCH/UFPE 2024.2 
### DOCENTE DRª. Mª DO CARMO (MQ1)
### CODADO ORIGINALMENTE EM R VERSÃO 4.4.1
### INÍCIO 06-01-25


### TITULO: ENSAIO SOBRE O LEGADO DO COMBATE À FOME E À EXTREMA POBREZA EM PERNAMBUCO (2004-2024): UM ESTUDO A PARTIR DO GRAU DE TRANSVERSALIDADE
# ===================================================================================
# DISCENTES 
# ===================================================================================

# JEFFERSON GABRIEL
# JULIA EVELYN
# LARISSA KARLA
# RYAN ALMEIDA

# ===================================================================================
# CARREGANDO FUNÇÕES NECESSÁRIAS
# ===================================================================================

#PARA A REPRODUTIBILIDADE DO ESTUDO
set.seed(123) 

install.packages("tidyverse")  
library(tidyverse)  

#PARA LER ARQUIVOS EM EXCEL 
install.packages("readxl")  
library(readxl)

install.packages("readr")  
library(readr)

install.packages("dplyr") 
library(dplyr)

#PARA A PRODUÇÃO DE GRÁFICOS MAIS SOFISTICADOS 
install.packages("ggplot2") 
library(ggplot2)

# ===================================================================================
# CARREGANDO DATABASE 
# ===================================================================================

taxadeanalfabetismo.municipios <- read.csv("C://Users//Notebook//Desktop//R//TRABALHOFINAL//dadosbrutos//taxadeanalfabetismo//ipeadata(13-02-2025-04-05)csvvirgula.csv",
                                skip = 1,
                                header = FALSE,
                                sep = ";", 
                                stringsAsFactors = FALSE,
                                fill = TRUE)

colnames(taxadeanalfabetismo.municipios) <- c("estado","ibge.cod","municipio","2000", "2010", "2022")

# ----------------------------------------

taxadeanalfabetismo.municipios <- taxadeanalfabetismo.municipios %>%
  pivot_longer(cols = starts_with())
  
  dados_long <- dados %>%
  pivot_longer(cols = starts_with("Taxa_"),   # Seleciona colunas com "Taxa_"
               names_to = "Ano",              # Nova coluna para armazenar anos
               values_to = "Taxa_Analfabetismo") %>%  # Nova coluna para os valores
  mutate(Ano = as.numeric(gsub("Taxa_", "", Ano))) 

dadosEppTranversalidade.grau <- 
# ===================================================================================
# RASPAGEM 
# ===================================================================================


# ===================================================================================
# MENSURANDO TRANSVERSALIDADE, FONTE: IPEADATA; DADOS ABERTOS GOV
# ===================================================================================

# 1. ANÁLISE DE DOCUMENTOS E PLANILHAS PARA VER QUANTO QUANTAS ÁREAS E SETORES SÃO MENCIONADOS, VER BASE DE DADOS DO PLANO PLURI ANUAL DE 2004-2024

# VIs -> PENSAR GASTO PÚBLICO, URBAN, DENSIDADE POPULACIONAL, INDICE DE POBREZA, GINI, CRIMINALIDADE, TAXA DE DESEMPREGO, NÍVEL DE ESCOLARIDADE 

#VD -> INDICE DE POBREZA E PENSAR POBREZA OCMO ALGO QUE MEDE A TRANSVERSALIDADE 

#PRIMEIRO MODELO: TAXA DE POBREZA COMO VD FAZER COM AS VIS COMO AS SUPRAS CITADAS 

#SEGUNDO MODELO: TAXA DE FOME (VD) PEGAR AS VIS SUPRACITADAS 

#PERIODOS E BOLETINS ESTADUAIS SOBRE POBREZA, CONSIGAMOS MAIS VIs

#VER LITERATURA RECENTES TRABALHOS ACADÊMICOS 
# 2. 

# ===================================================================================
# ANÁLISE DESCRITIVA 
# ===================================================================================

# ===================================================================================
# MODELOS DE REGRESSÃO MÚLTIPLA
# ===================================================================================

#Epp devo buscar dados de combate a fome e a extrema pobreza do ipea 
# Epp (Programa Fome Zero) = Beta0 + Beta1 * Orçamento compartilhado + Beta2 * Analise de documento e política s

# ===================================================================================
# CÓDIGO MODELO PARA ALGUMA COISA
# ===================================================================================

#EXEMPLO DE CÓDIGO PARA A CRIAÇÃO DE UM INDICE
# Criando um conjunto de dados fictício
dados <- data.frame(
  instituicao = paste("Org", 1:10),
  reunioes = sample(5:20, 10, replace = TRUE),  # Número de reuniões
  diversidade = sample(1:10, 10, replace = TRUE),  # Diversidade de setores
  participacao = runif(10, 0.5, 1)  # Frequência de participação (0 a 1)
)

# Normalizando os dados (Min-Max Scaling)
normalizar <- function(x) (x - min(x)) / (max(x) - min(x))
dados_norm <- as.data.frame(lapply(dados[,-1], normalizar))

# 1. Índice de Média Ponderada (pesos arbitrários)
pesos <- c(0.4, 0.3, 0.3)  # Ajuste conforme necessário
dados$indice_media_ponderada <- rowSums(dados_norm * pesos)

# 2. Índice Multiplicativo (Geométrico)
dados$indice_multiplicativo <- apply(dados_norm, 1, function(x) prod(x)^(1/length(x)))

# Exibir resultados
print(dados[, c("instituicao", "indice_media_ponderada", "indice_multiplicativo")])

# ===================================================================================
# REFERÊNCIAS 
# ===================================================================================

### R DEVELOPMENT CORE TEAM. R: Uma linguagem e ambiente para computação estatística . Viena: R Foundation for Statistical Computing, 2011. ISBN 3-900051-07-0. Disponível em: http://www.R-project.org/ . Acessado em : 6 de jan. de 2025.

# ===================================================================================