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
# PACOTES 
# ===================================================================================

if(!require(corrplot))
  install.packages("corrplot") 
library(corrplot)

# INSTALANDO/CARREGANDO PACOTES
if(!require(readxl)) 
  install.packages("readxl")
library(readxl)

if(!require(tidyverse)) 
  install.packages("tidyverse")
library(tidyverse)

if(!require(openxlsx)) 
  install.packages("openxlsx")
library(openxlsx)

if(!require(ggplot2)) 
  install.packages("ggplot2")

if(!require(car)) 
  install.packages("car")

if(!require(rstatix)) 
  install.packages("rstatix")

if(!require(lmtest)) 
  install.packages("lmtest")

if(!require(ggpubr)) 
  install.packages("ggpubr")

if(!require(QuantPsyc)) 
  install.packages("QuantPsyc")

if(!require(psych)) 
  install.packages("psych")

if(!require(QuantPsyc)) 
  install.packages("scatterplot3d")

# -----------------------------------------

# CARREGAR TODAS AS BIBLIOTECAS DE UMA VEZ
if(!require(pacman))
  install.packages("pacman")  
library(pacman)

pacman::p_load(corrplot, readxl, tidyverse, corrgram, GGally, openxlsx)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, scatterplot3d)

# -----------------------------------------

# ===================================================================================
# UPLOADIND DATABASE, ANALISE DESCRITIVA E TRATAMENTO DE DADOS
# ===================================================================================

# CARREGANDO... 
Dados_Zscore_2023 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2023.xlsx")

glimpse(Dados_Zscore_2023)

# TRANSFORMAÇÃO DE CHR PARA INTEGER E DBL
Dados_Zscore_2023$OBITOS_INFANTIS_2010 <- as.integer(Dados_Zscore_2023$OBITOS_INFANTIS_2010)
Dados_Zscore_2023$TAXA_MORTALIDADE_INFANTIL_2010 <- as.double(Dados_Zscore_2023$TAXA_MORTALIDADE_INFANTIL_2010)
#TUDO OK...
summary(Dados_Zscore_2023)

# -----------------------------------------
# REMOVENDO COLUNAS NÃO IMPORTANTES PARA A PADRONIZAÇÃO
view(Dados_Zscore_2023)
Dados_Zscore_2023 <- Dados_Zscore_2023[,-1]
print(Dados_Zscore_2023)
# TUDO OK...

# -----------------------------------------
# TRANTANDO NAs INTRODUZIDOS POR COERÇÃO VIA IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2023 <- Dados_Zscore_2023 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
summary(Dados_Zscore_2023)

# ===================================================================================
# PADRONIZAÇÃO DE DADOS VIA ZSCORE
# ===================================================================================

Dados_Zscore_2023 <- Dados_Zscore_2023 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

# EXPORTANDO PADRONIZAÇÃO TABULADA PARA MINHA MAQUINA 
write.xlsx(Dados_Zscore_2023, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2023.xlsx")

# ===================================================================================
# ANALISE DE DIAGNOSTICO: CORRELAÇÃO E MULTICOLINEARIDADE
# ===================================================================================
# UPLOAD DOS DADOS PADRONIZADOS DE 2023

Zscore_2023 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2023.xlsx")

# DADOS PRÉ-PADRONIZAÇÃO
corrplot(Dados_Zscore_2023)
# NÃO ESTÁ LENDO ESSE CODIGO, TENTAMOS UM CODIGO ALTERNATIVO

corrgram::corrgram(Dados_Zscore_2023)
# ESTÁ IDENTIFICADA A CORRELAÇÃO

# DADOS PÓS-PADRONIZAÇÃO
corrgram::corrgram(Zscore_2023)
# ESTÁ  IDENTIFICADA A CORRELAÇÃO
# -----------------------------------------

# ===================================================================================
# TRATAMENTO DA MULTICOLINEARIDADE VIA PRINCIPAL COMPONENTS ANALYSIS (PCA)
# ===================================================================================

pca_2023 <- prcomp(Zscore_2023, center = TRUE, scale. = TRUE)
summary(pca_2023)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2023)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

# ---------------------------------------------------

pca_2023$rotation[1]
plot(pca_2023)

# ===================================================================================
# CONSTRUÇÃO DO MODELO 2023
# ===================================================================================

# UPLOAD DA VARIAVEL DEPENDENTE: INDICE DE EFETIVADE DA GESTÃO MUNICIPAL EM 2019
VD_IEGM_2023 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_PE_2023.xlsx")

#OBTER COMPONENTES PRINCIPAIS 
X_pca_2023 <- pca_2023$x[, 1:num_components]
print(X_pca_2023)

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
X_pca_df_2023 <- as.data.frame(X_pca_2023)
print(X_pca_df_2023)

# ---------------------------------------------------
# CONSTRUIR O MODELO DE REGRESSÃO USANDO OS COMPENTENTES PRINCIPAIS
View(Zscore_2023$IEGM_taxa_2023)
model1_pca_2023 <- lm(Zscore_2023$IEGM_taxa_2023 ~ ., 
                      data = X_pca_df_2023)

summary(model1_pca_2023)
# SOLICITA-SE A REMOÇÃO DO INTERCEPTO
AIC(model1_pca_2023)
BIC(model1_pca_2023)

model2_pca_2023 <- lm(Zscore_2023$IEGM_taxa_2023 ~ -1 + PC1 + PC2 +  PC3, 
                      data = X_pca_df_2023)

summary(model2_pca_2023)
AIC(model2_pca_2023)
BIC(model2_pca_2023)
# SEGUNDO AS TESTAGENS ESTATISTICAS ACIMA PODE-SE INFERIR QUE O MELHOR MODELO REALMENTE É O MODELO SEM INTERCEPTO

# OUTPUT DO MODELO (model2_pca_2023)
# ---------------------------------------------------
# Multiple R-squared:  0.9455,	Adjusted R-squared:  0.9446 
# ---------------------------------------------------

# ===================================================================================
# ANALISE DE DIAGNOSTICO::PRESSUPOSTOS DA REGRESSÃO LINEAR MULTIPLA
# ===================================================================================

par(mfrow=c(2,2))
plot(model2_pca_2023)
par(mfrow=c(1,1))

#  --------------------------------------------------
# REALIZAÇÃO DE TESTES ESTATÍSTICO, IDENTIFICAÇÃO E TRATAMENTO 

# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSÃO PROPRIAMENTE DITA
# ===================================================================================

summary(model2_pca_2023)$coefficients

#  --------------------------------------------------
#        Estimate Std. Error    t value      Pr(>|t|)
# PC1 -0.05344229 0.00843066  -6.339041  1.771197e-09
# PC2 -0.57247351 0.01038585 -55.120535 1.706301e-115
# PC3  0.11012301 0.01227541   8.971028  3.561779e-16
#  --------------------------------------------------

# CODIGO ABAIXO PARA A CONSTRUÇÃO DA MATRIZ, PARA A ANALISE FINAL
pca_2023_1 <- prcomp(Zscore_2023, center = TRUE, scale. = TRUE)
print(pca_2023_1)

# COPIANDO DA AREA DE TRANSFERENCIA O QUE EU ISOLEI
pesos_pca_colada_2023 <- read.table("clipboard", header = T)
print(pesos_pca_colada_2023)
View(pesos_pca_colada_2023)

pesos_pca_colada_2017 <- pesos_pca_colada_2017[,-5]
# NÃO ESTOU CONSEGUINDO EXCLUIR PCA4, TENHO QU FORÇAR A AÇÃO

colnames(pesos_pca_colada_2023)
pesos_pca_colada_2023 <- pesos_pca_colada_2023[, !colnames(pesos_pca_colada_2023) %in% "PC4"]

print(pesos_pca_colada_2023)

matriz_pesos_pca_2023 <- as.matrix(pesos_pca_colada_2023)
print(matriz_pesos_pca_2023)

#  --------------------------------------------------
#  --------------------------------------------------

# BETAS 1, 2 E 3, RESPECTIVAMENTE 
betas_2023 <- c(-0.05344229, -0.57247351, 0.11012301)
resultado <- matriz_pesos_pca_2023[, 1:3] %*% betas_2023

modelo2023_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2023), Resultado = round(resultado, 4))
#  --------------------------------------------------
print(modelo2023_resultado)
#  --------------------------------------------------

#                                                      Variavel Resultado
# IEGM_taxa_2023                                 IEGM_taxa_2023    0.3427
# i-Plan_taxa_2023                             i-Plan_taxa_2023    0.2241
# i-Saúde_taxa_2023                           i-Saúde_taxa_2023    0.0732
# i-GovTI_taxa_2023                           i-GovTI_taxa_2023    0.2016
# i-Fiscal_taxa_2023                         i-Fiscal_taxa_2023    0.1896
# i-Educ_taxa_2023                             i-Educ_taxa_2023    0.1797
# i-Cidade_taxa_2023                         i-Cidade_taxa_2023    0.1241
# i-Amb_taxa_2023                               i-Amb_taxa_2023    0.1422
# GASTO_PUBLICO_2023                         GASTO_PUBLICO_2023    0.0653
# TAXA_CRIMINALIDADE_2023               TAXA_CRIMINALIDADE_2023   -0.1039
# N_VITIMAS_CVLI_2023                       N_VITIMAS_CVLI_2023   -0.0699
# DENSIDADE_DEMOGRAFICA_2010         DENSIDADE_DEMOGRAFICA_2010   -0.0108
# IDHM_PE_2010                                     IDHM_PE_2010    0.0060
# GINI_2010                                           GINI_2010    0.0385
# TAXA_ANALFABETISMO_2010               TAXA_ANALFABETISMO_2010   -0.0110
# TAXA_DESEMPREGO_2010                     TAXA_DESEMPREGO_2010    0.0345
# NASCIDOS_VIVOS_PE_2010                 NASCIDOS_VIVOS_PE_2010   -0.0111
# OBITOS_INFANTIS_2010                     OBITOS_INFANTIS_2010   -0.0119
# TAXA_MORTALIDADE_INFANTIL_2010 TAXA_MORTALIDADE_INFANTIL_2010   -0.0023
# TAXA_URBANIZAÇÃO_2010                   TAXA_URBANIZAÇÃO_2010    0.0451
