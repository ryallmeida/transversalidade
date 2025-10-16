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
# CONSTRUÇÃO DO MODELO 2022
# ===================================================================================

#OBTER COMPONENTES PRINCIPAIS 
X_pca_2022 <- pca_2022$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
X_pca_df_2022 <- as.data.frame(X_pca_2022)

# CONSTRUIR O MODELO DE REGRESSÃO USANDO OS COMPENTENTES PRINCIPAIS
model_pca_2022 <- lm(Zscore_2022$IEGM_taxa_2021 ~ -1 + PC1 + PC2 + PC3 + PC4, data = X_pca_df_2022)

#  --------------------------------------------------
summary(model_pca_2022)
#  --------------------------------------------------
# POR ALGUM MOTIVO O MODELO COM ESSA FUNÇÃO, O R^2 DEU 1, E COMO SABEMOS ISSO NAS CIÊNCIAS SOCIAIS APLICADAS É PRATICAMENTE IMPOSSÍVEL 

# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSAO PROPRIAMENTE DITA
# ===================================================================================
 
# NOTAS -----------------------------------------------------------------------------
# HOUVE A NECESSIDADE DE CRIAR UMA MATRIZ PARA CALCULAR DE FORMA AUTOMATIZADA
# O QUANTO A VARIAVEL AFETA A VARIÁVEL DEPENDENTE  
# IDEIA PRINCIPAL: 4 COLUNAS REPRESENTANDO OS COMPONENTES PRINCIPAIS E 17 OBS. REPRESENTANDO AS 17 VARIÁVEIS

# CODIGO ABAIXO PARA A CONSTRUÇÃO DA MATRIZ, PARA A ANALISE FINAL
pca_2022_1 <- prcomp(Zscore_2022, center = TRUE, scale. = TRUE)
print(pca_2022_1)

# COPIANDO DA AREA DE TRANSFERENCIA O QUE EU ISOLEI
pesos_pca_colada <- read.table("clipboard", header = T)
print(pesos_pca_colada)

matriz_pesos_pca_2022 <- as.matrix(pesos_pca_colada)
print(matriz_pesos_pca_2022)

#  --------------------------------------------------
#  --------------------------------------------------

# BETAS 1, 2, 3 E 4 (RESPECTIVAMENTE)
betas <- c(0.49, -0.32, 0.05, -0.07)  

# MULTIPLICANDO OS BETAS PELOS PESOS DOS COMPONENTES PRINCIPAIS
# OU SEJA, CALCULA A COMBINAÇÃO LINEAR PARA CADA VARIÁVEL

resultado <- matriz_pesos_pca_2022[, 1:4] %*% betas  

#  --------------------------------------------------
# DATAFRAME PARA O RESULTADO
modelo2022_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2022), Resultado = round(resultado, 4))

#  --------------------------------------------------
print(modelo2022_resultado)
#  --------------------------------------------------

# ===================================================================================
# OOUTPUT::ANALISE
# ===================================================================================


# Taxa_Criminalidade_2022               Taxa_Criminalidade_2022    0.0351
# Densidade_Demografica_2022         Densidade_Demografica_2022   -0.0039
# IDH_Municipal_2022                         IDH_Municipal_2022   -0.0262
# Taxa_Analfabetismo_2022               Taxa_Analfabetismo_2022   -0.0617
# IDEB_Municipal_2022                       IDEB_Municipal_2022    0.0099
# Numero_Vitimas_CVLI_2022             Numero_Vitimas_CVLI_2022   -0.0244
# Taxa_Mortalidade_Infantil_2022 Taxa_Mortalidade_Infantil_2022   -0.0338
# Gasto_Publico_2022                         Gasto_Publico_2022   -0.0345
# IEGM_taxa_2021                                 IEGM_taxa_2021    0.3557
# i-Plan_taxa_2021                             i-Plan_taxa_2021    0.1810
# i-Saúde_taxa_2021                           i-Saúde_taxa_2021    0.2094
# i-GovTI_taxa_2021                           i-GovTI_taxa_2021    0.1374
# i-Fiscal_taxa_2021                         i-Fiscal_taxa_2021    0.1638
# i-Educ_taxa_2021                             i-Educ_taxa_2021    0.2392
# i-Cidade_taxa_2021                         i-Cidade_taxa_2021    0.0708
# i-Amb_taxa_2021                               i-Amb_taxa_2021    0.1730
# Domicilios_cRede_Esgoto_2022     Domicilios_cRede_Esgoto_2022   -0.0089
