# ===================================================================================
# SCRIPT::PRINCIPAL COMPONENTES ANALYSIS 
# ===================================================================================

# ---------------------------------------------
# CARREGANDO PACOTES
# ---------------------------------------------

if(!require(pacman))
  install.packages("pacman")  
library(pacman)

if(!require(openxlsx))
  install.packages("openxlsx")  
library(openxlsx)

if(!require(tidyverse))
  install.packages("tidyverse")  
library(tidyverse)

if(!require(readxl))
  install.packages("readxl")  
library(readxl)

if(!require(corrgram))
  install.packages("corrgram")  
library(corrgram)

# ---------------------------------------------

pacman::p_load(openxlsx, readxl, tidyverse, corrgram)

# ---------------------------------------------

# ===================================================================================
# CARREGANDO DADOS
# ===================================================================================

# NOTA -----------------------------------------------------------------------------
# IMPORTANTE RESSALTAR QUE NESSA PARTE DO SCRIPT JÁ ESTAMOS TRABALHANDO COM OS DADOS PADRONIZADOS DO ZSCORE 

# ---------------------------------------------
# ANO: 2022
# ---------------------------------------------

Zscore_2022 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito/Zscore_2022.xlsx")

summary(Zscore_2022)

# VARIAVEL DEPENDENTE: INDICE DE EFETIVADE DA GESTÃO MUNICIPAL

IEGM_2021 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_PE_2021.xlsx")

summary(IEGM_2021)

# ===================================================================================
# ANÁLISE DE CORRELAÇÃO
# ===================================================================================

corrplot::corrplot(Zscore_2022)

# ALTERNATIVA AO CORRPLOT, DEVIDO OS DADOS DO ZSCORE NÃO ESTAREM ENTRE -1 E 1 
library(corrgram)
corrgram(Zscore_2022)
# HÁ UM GRAU DE CORRELAÇÃO ENTRE ALGUMAS VARIÁVEIS
# RESOLVEMOS ISSO A PARTIR DA TECNICA DE PRINCIPAL COMPONENT ANALYSIS 

# ---------------------------------------------------
pca_2022 <- prcomp(Zscore_2022, center = TRUE, scale. = TRUE)
summary(pca_2022)

# O SUMMARY DE PCA GERA TRÊS RESULTADOS 
#(i)   DESVIO PADRÃO DOS COMPONENTES PRINCIPAIS 
#(ii)  PROPORÇÃO DA VARIÂNCIA EXPLICADA POR CADA COMPONENTE 
#(iii) VARIÂNCIA ACUMULADA, OU SEJA, A SOMA DAS PROPORÇÕES ATÉ CADA COMPONENE

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2022)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

# ---------------------------------------------------

pca_2022$rotation[1]

# ===================================================================================
# CONSTRUÇÃO DO MODELO 
# ===================================================================================

#OBTER COMPONENTES PRINCIPAIS 
X_pca_2022 <- pca_2022$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
X_pca_df_2022 <- as.data.frame(X_pca_2022)

# CONSTRUIR O MODELO DE REGRESSÃO USANDO OS COMPENTENTES PRINCIPAIS
model_pca_2022 <- lm(Zscore_2022$IEGM_taxa_2021 ~ -1 + PC1 + PC2 + PC3 + PC4, data = X_pca_df_2022)

summary(model_pca_2022)

# POR ALGUM MOTIVO O MODELO COM ESSA FUNÇÃO, O R^2 DEU 1, E COMO SABEMOS ISSO NAS CIÊNCIAS SOCIAIS APLICADAS É PRATICAMENTE IMPOSSÍVEL 

#  --------------------------------------------------
#  --------------------------------------------------
