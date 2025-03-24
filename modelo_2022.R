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

# -----------------------------------------

# CARREGAR TODAS AS BIBLIOTECAS DE UMA VEZ
if(!require(pacman))
  install.packages("pacman")  
library(pacman)

pacman::p_load(corrplot, readxl, tidyverse, corrgram, GGally, openxlsx)

# -----------------------------------------

# ===================================================================================
# UPLOADIND DATABASE, ANALISE DESCRITIVA E TRATAMENTO DE DADOS
# ===================================================================================

# CARREGANDO... 
Dados_Zscore_2022 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2022.xlsx")

glimpse(Dados_Zscore_2022)
view(Dados_Zscore_2022)
# DEU ERRO DE LEITURA RESULTANDO EM CHR APENAS EM NUMEROS DE OBITOS INFANTIS, ASSIM COMO EM TAXA DE MORTALIDADE INFANTIL

Dados_Zscore_2022$OBITOS_INFANTIS_2010 <- as.integer(Dados_Zscore_2022$OBITOS_INFANTIS_2010)
Dados_Zscore_2022$Numero_Vitimas_CVLI_2022) <- as.integer(Dados_Zscore_2022$Numero_Vitimas_CVLI_202)
Dados_Zscore_2022$Taxa_Mortalidade_Infantil_2010 <- as.double(Dados_Zscore_2022$Taxa_Mortalidade_Infantil_2010)
Dados_Zscore_2022$Taxa_Criminalidade_2022) <- as.double(Dados_Zscore_2022$Taxa_Criminalidade_2022)


Taxa_Mortalidade_Infantil_2022

TAXA_MORTALIDADE_INFANTIL_2010 #remover

Dados_Zscore_2022$Taxa_Criminalidade_2022 <- as.double(Dados_Zscore_2022$Taxa_Criminalidade_2022)
Dados_Zscore_2022$Numero_Vitimas_CVLI_2022 <- as.double(Dados_Zscore_2022$Numero_Vitimas_CVLI_2022)
Dados_Zscore_2022$Taxa_Mortalidade_Infantil_2022 <- as.double(Dados_Zscore_2022$Taxa_Mortalidade_Infantil_2022)


summary(Dados_Zscore_2017)
#OK...

# REMOVENDO INUTILIDADES (COLUNAS) PARA A PADRONIZAÇÃO  DO ZSCORE
Dados_Zscore_2022 <- Dados_Zscore_2022[,-23]
Dados_Zscore_2022 <- Dados_Zscore_2022[,-19]
Dados_Zscore_2022 <- Dados_Zscore_2022[,-2]
Dados_Zscore_2022 <- Dados_Zscore_2022[,-1]

summary(Dados_Zscore_2022)
view(Dados_Zscore_2022)

# TRATANDO NAs POR IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2022 <- Dados_Zscore_2022 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

summary(Dados_Zscore_2022)

# ===================================================================================
# PADRONIZANDO EM ZSCORE
# ===================================================================================

Dados_Zscore_2022 <- Dados_Zscore_2022 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

# EXPORTANDO PADRONIZAÇÃO TABULADA PARA MINHA MAQUINA 

write.xlsx(Dados_Zscore_2022, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2022_novo.xlsx")

# ===================================================================================
# ANALISE DE CORRELAÇÃO
# ===================================================================================

# FIZ O UPLOAD NOVAMENTE DO DATABASE ORIGINAL PARA CONFERIR SE HÁ CORRELAÇÃO ANTES E DEPOIS DA PADRONIZAÇÃO DOS DADOS

Zscore_2022 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2022_novo.xlsx")

# ANTES DA PADRONIZAÇÃO...
corrgram(Zscore_2022)
# HÁ ALTOS GRAUS DE CORRELAÇÃO EM APROXIMADAMENTE 2/3 DAS VARIAVEIS

# PÓS-PADRONIZAÇÃO 
corrgram(Zscore_2022)
# A CORRELAÇÃO MANTEM-SE IGUAL

# ---------------------------------------------
# ANO::2022
# ---------------------------------------------

# UPLOAD DE DADOS PADRONIZADO
Zscore_2022 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito/Zscore_2022.xlsx")

summary(Zscore_2022)

# UPLOAD DA VARIAVEL DEPENDENTE: INDICE DE EFETIVADE DA GESTÃO MUNICIPAL
VD_IEGM_2022 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_PE_2021.xlsx")

summary(IEGM_2022)

# ===================================================================================
# TRATAMENTO DA CORRELAÇÃO::PRINCIPAL_COMPONENT_ANALYSIS
# ===================================================================================

pca_2022 <- prcomp(Zscore_2022, center = TRUE, scale. = TRUE)
summary(pca_2022)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2022)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

# ---------------------------------------------------

pca_2022$rotation[1]
plot(pca_2022)

# ===================================================================================
# CONSTRUÇÃO DO MODELO 2022
# ===================================================================================

#OBTER COMPONENTES PRINCIPAIS 
X_pca_2022 <- pca_2022$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
X_pca_df_2022 <- as.data.frame(X_pca_2022)

# CONSTRUIR O MODELO DE REGRESSÃO USANDO OS COMPENTENTES PRINCIPAIS
model1_pca_2022 <- lm(Zscore_2022$IEGM_taxa_2021 ~ ., 
                     data = X_pca_df_2022)

#  --------------------------------------------------

summary(model1_pca_2022)
#  --------------------------------------------------

# REMOVER INTERCEPTO NO MODELO 2

model2_pca_2022 <- lm(Zscore_2022$IEGM_taxa_2021 ~ -1 + PC1 + PC2 + PC3, 
                      data = X_pca_df_2022)

summary(model2_pca_2022)
plot(model_pca_2017)
#  --------------------------------------------------

# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSAO PROPRIAMENTE DITA
# ===================================================================================

#  --------------------------------------------------
# OBTENÇÃO DOS BETAS 

summary(model2_pca_2022)$coefficients

# OUTPUT --------------------------------------------
#        Estimate  Std. Error   t value      Pr(>|t|)
# PC1 -0.01816788 0.007398421 -2.455643  1.500222e-02
# PC2  0.58918533 0.008840157 66.648743 8.498367e-130
# PC3  0.07142699 0.010674987  6.691061  2.646305e-10
#  --------------------------------------------------

# NOTAS -----------------------------------------------------------------------------
# HOUVE A NECESSIDADE DE CRIAR UMA MATRIZ PARA CALCULAR DE FORMA AUTOMATIZADA
# O QUANTO A VARIAVEL AFETA A VARIÁVEL DEPENDENTE  
# IDEIA PRINCIPAL: 4 COLUNAS REPRESENTANDO OS COMPONENTES PRINCIPAIS E 17 OBS. REPRESENTANDO AS 19 VARIÁVEIS

# CODIGO ABAIXO PARA A CONSTRUÇÃO DA MATRIZ, PARA A ANALISE FINAL
pca_2022_1 <- prcomp(Zscore_2022, center = TRUE, scale. = TRUE)
print(pca_2022_1)

# COPIANDO DA AREA DE TRANSFERENCIA O QUE EU ISOLEI
pesos_pca_colada_2022 <- read.table("clipboard", header = T)
print(pesos_pca_colada_2022)
View(pesos_pca_colada_2017)

pesos_pca_colada_2017 <- pesos_pca_colada_2017[,-5]
# NÃO ESTOU CONSEGUINDO EXCLUIR PCA4, TENHO QU FORÇAR A AÇÃO

colnames(pesos_pca_colada_2022)
pesos_pca_colada_2022 <- pesos_pca_colada_2022[, !colnames(pesos_pca_colada_2022) %in% "PC4"]

# RETOMANDO O CODIGO...
matriz_pesos_pca_2022 <- as.matrix(pesos_pca_colada_2022)
print(matriz_pesos_pca_2022)

#  --------------------------------------------------
#  --------------------------------------------------

# BETAS 1, 2, 3 (RESPECTIVAMENTE)
 
betas_2022 <- c(-0.01816788, 0.58918533, 0.07142699)  


# MULTIPLICANDO OS BETAS PELOS PESOS DOS COMPONENTES PRINCIPAIS
# OU SEJA, CALCULA A COMBINAÇÃO LINEAR PARA CADA VARIÁVEL

resultado <- matriz_pesos_pca_2022[, 1:3] %*% betas_2022  

#  --------------------------------------------------
# DATAFRAME PARA O RESULTADO
modelo2022_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2022), Resultado = round(resultado, 4))

#  --------------------------------------------------

print(modelo2022_resultado)

#  --------------------------------------------------

# ===================================================================================
# OUTPUT::RESULTADO_MODELO_2017
# ===================================================================================

#Variavel Resultado
#Taxa_Criminalidade_2022               Taxa_Criminalidade_2022    0.0234
#Densidade_Demografica_2022         Densidade_Demografica_2022   -0.0104
#IDH_Municipal_2022                         IDH_Municipal_2022   -0.0045
#Taxa_Analfabetismo_2022               Taxa_Analfabetismo_2022   -0.0591
#IDEB_Municipal_2022                       IDEB_Municipal_2022   -0.0019
#Numero_Vitimas_CVLI_2022             Numero_Vitimas_CVLI_2022   -0.0051
#Taxa_Mortalidade_Infantil_2022 Taxa_Mortalidade_Infantil_2022   -0.0368
#Gasto_Publico_2022                         Gasto_Publico_2022   -0.0176
#IEGM_taxa_2021                                 IEGM_taxa_2021    0.3526
#i-Plan_taxa_2021                             i-Plan_taxa_2021    0.1751
#i-Saúde_taxa_2021                           i-Saúde_taxa_2021    0.1638
#i-GovTI_taxa_2021                           i-GovTI_taxa_2021    0.1613
#i-Fiscal_taxa_2021                         i-Fiscal_taxa_2021    0.1871
#i-Educ_taxa_2021                             i-Educ_taxa_2021    0.2185
#i-Cidade_taxa_2021                         i-Cidade_taxa_2021    0.1194
#i-Amb_taxa_2021                               i-Amb_taxa_2021    0.2016
#TAXA_DESEMPREGO_2010                     TAXA_DESEMPREGO_2010   -0.0184
#NASCIDOS_VIVOS_PE_2010                 NASCIDOS_VIVOS_PE_2010   -0.0139
#OBITOS_INFANTIS_2010                     OBITOS_INFANTIS_2010   -0.0192
#TAXA_URBANIZAÇÃO_2010                   TAXA_URBANIZAÇÃO_2010   -0.0184