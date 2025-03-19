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
Dados_Zscore_2019 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2019.xlsx")

glimpse(Dados_Zscore_2019)
# FORAM IDENTIFICADO VARIAVEIS SENDO LIDAS COMO CHR 

Dados_Zscore_2019$IEGM_Taxa_2019 <- as.double(Dados_Zscore_2019$IEGM_Taxa_2019)
Dados_Zscore_2019$i.Plan_taxa_2019 <- as.double(Dados_Zscore_2019$i.Plan_taxa_2019)   
Dados_Zscore_2019$i.Saúde_taxa_2019 <- as.double(Dados_Zscore_2019$i.Saúde_taxa_2019) 
Dados_Zscore_2019$i.Fiscal_taxa_2019 <- as.double(Dados_Zscore_2019$i.Fiscal_taxa_2019)
Dados_Zscore_2019$i.Educ_taxa_2019 <- as.double(Dados_Zscore_2019$i.Educ_taxa_2019 )
Dados_Zscore_2019$i.Cidade_taxa_2019 <- as.double(Dados_Zscore_2019$i.Cidade_taxa_2019  )
Dados_Zscore_2019$i.Amb_taxa_2019 <- as.double(Dados_Zscore_2019$i.Amb_taxa_2019)
Dados_Zscore_2019$TAXA_CRIMINALIDADE_2019 <- as.double(Dados_Zscore_2019$TAXA_CRIMINALIDADE_2019)
Dados_Zscore_2019$TAXA_MORTALIDADE_INFANTIL_2010 <- as.double(Dados_Zscore_2019$TAXA_MORTALIDADE_INFANTIL_2010)
Dados_Zscore_2019$`i.GovTI_taxa_2019'` <- as.double(Dados_Zscore_2019$`i.GovTI_taxa_2019'`)


Dados_Zscore_2019$N_VITIMAS_CVLI_2019 <- as.integer(Dados_Zscore_2019$N_VITIMAS_CVLI_2019)
Dados_Zscore_2019$OBITOS_INFANTIS_2010 <- as.integer(Dados_Zscore_2019$OBITOS_INFANTIS_2010)

summary(Dados_Zscore_2019)
glimpse(Dados_Zscore_2019)
# TUDO OK...

# -----------------------------------------
# REMOVENDO COLUNAS NÃO IMPORTANTES PARA A PADRONIZAÇÃO
view(Dados_Zscore_2019)
Dados_Zscore_2019 <- Dados_Zscore_2019[,-1]
print(Dados_Zscore_2019)
# TUDO OK...

# -----------------------------------------
# TRANTANDO NAs INTRODUZIDOS POR COERÇÃO VIA IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2019 <- Dados_Zscore_2019 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
summary(Dados_Zscore_2019)

# ===================================================================================
# PADRONIZAÇÃO DE DADOS VIA ZSCORE
# ===================================================================================

Dados_Zscore_2019 <- Dados_Zscore_2019 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

# EXPORTANDO PADRONIZAÇÃO TABULADA PARA MINHA MAQUINA 
write.xlsx(Dados_Zscore_2019, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2019.xlsx")

# ===================================================================================
# ANALISE DE DIAGNOSTICO: CORRELAÇÃO E MULTICOLINEARIDADE
# ===================================================================================

# UPLOAD DOS DADOS PADRONIZADOS DE 2019

Zscore_2019 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2019.xlsx")

# -----------------------------------------

# DADOS PRÉ-PADRONIZAÇÃO
corrplot(Dados_Zscore_2019)
# NÃO ESTÁ LENDO ESSE CODIGO, TENTAMOS UM CODIGO ALTERNATIVO

corrgram::corrgram(Dados_Zscore_2019)
# ESTÁ IDENTIFICADA A CORRELAÇÃO

# DADOS PÓS-PADRONIZAÇÃO
corrgram::corrgram(Zscore_2019)
# ESTÁ  IDENTIFICADA A CORRELAÇÃO

# ===================================================================================
# TRATAMENTO DA MULTICOLINEARIDADE VIA PRINCIPAL COMPONENTS ANALYSIS (PCA)
# ===================================================================================

pca_2019 <- prcomp(Zscore_2019, center = TRUE, scale. = TRUE)
summary(pca_2019)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2019)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

# ---------------------------------------------------

pca_2019$rotation[1]
plot(pca_2019)

# ===================================================================================
# CONSTRUÇÃO DO MODELO 2017
# ===================================================================================

# UPLOAD DA VARIAVEL DEPENDENTE: INDICE DE EFETIVADE DA GESTÃO MUNICIPAL EM 2019
VD_IEGM_2019 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_Pernambuco_2019_tratado.xlsx")

#OBTER COMPONENTES PRINCIPAIS 
X_pca_2019 <- pca_2019$x[, 1:num_components]
print(X_pca_2019)

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
X_pca_df_2019 <- as.data.frame(X_pca_2019)
print(X_pca_df_2019)

# CONSTRUIR O MODELO DE REGRESSÃO USANDO OS COMPENTENTES PRINCIPAIS
model1_pca_2019 <- lm(Zscore_2019$IEGM_Taxa_2019 ~ PC1 + PC2 + PC3, 
                     data = X_pca_df_2019)

model2_pca_2019 <- lm(Zscore_2019$IEGM_Taxa_2019 ~ -1 + PC1 + PC2, 
                      data = X_pca_df_2019)

#  --------------------------------------------------
summary(model1_pca_2019)
# REMOVER INTERCEPTO DEVIDO INSIGNIFICANCIA
# REMOVER PC3 DEVIDO INSIGNIFICANCIA
#  --------------------------------------------------

summary(model2_pca_2019)
# Multiple R-squared:  0.9485,	Adjusted R-squared:  0.9479

# ===================================================================================
# ANALISE DE DIAGNOSTICO::PRESSUPOSTOS DA REGRESSÃO LINEAR MULTIPLA
# ===================================================================================

par(mfrow=c(2,2))
plot(model2_pca_2019)
par(mfrow=c(1,1))

#  --------------------------------------------------
# REALIZAÇÃO DE TESTES ESTATÍSTICO, IDENTIFICAÇÃO E TRATAMENTO 

# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSÃO PROPRIAMENTE DITA
# ===================================================================================

summary(model2_pca_2019)$coefficients

#  --------------------------------------------------
#      Estimate  Std. Error   t value      Pr(>|t|)
#PC1  0.1986097 0.008296393  23.93928  2.846988e-58
#PC2 -0.5436000 0.010281335 -52.87251 8.456659e-113
#  --------------------------------------------------

# CODIGO ABAIXO PARA A CONSTRUÇÃO DA MATRIZ, PARA A ANALISE FINAL
pca_2019_1 <- prcomp(Zscore_2019, center = TRUE, scale. = TRUE)
print(pca_2019_1)

# COPIANDO DA AREA DE TRANSFERENCIA O QUE EU ISOLEI
pesos_pca_colada_2019 <- read.table("clipboard", header = T)
print(pesos_pca_colada_2019)
View(pesos_pca_colada_2017)

pesos_pca_colada_2017 <- pesos_pca_colada_2017[,-5]
# NÃO ESTOU CONSEGUINDO EXCLUIR PCA4, TENHO QU FORÇAR A AÇÃO

colnames(pesos_pca_colada_2019)
pesos_pca_colada_2019 <- pesos_pca_colada_2019[, !colnames(pesos_pca_colada_2019) %in% "PC4"]

pesos_pca_colada_2019 <- pesos_pca_colada_2019[, !colnames(pesos_pca_colada_2019) %in% "PC3"]

print(pesos_pca_colada_2019)

# RETOMANDO O CODIGO...
matriz_pesos_pca_2019 <- as.matrix(pesos_pca_colada_2019)
print(matriz_pesos_pca_2019)

#  --------------------------------------------------
#  --------------------------------------------------

# BETAS 1 E 2, RESPECTIVAMENTE 
betas_2019 <- c(0.1986097, -0.5436000)
resultado <- matriz_pesos_pca_2019[, 1:2] %*% betas_2019

modelo2019_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2019), Resultado = round(resultado, 4))
#  --------------------------------------------------
print(modelo2019_resultado)
#  --------------------------------------------------
# OUTPUT --------------------------------------------

#                                                     Variavel Resultado

#IEGM_Taxa_2019                                 IEGM_Taxa_2019    0.3349
#i.Plan_taxa_2019                             i.Plan_taxa_2019    0.1720
#i.Saúde_taxa_2019                           i.Saúde_taxa_2019    0.1623
#i.GovTI_taxa_2019'                         i.GovTI_taxa_2019'    0.1564
#i.Fiscal_taxa_2019                         i.Fiscal_taxa_2019    0.1609
#i.Educ_taxa_2019                             i.Educ_taxa_2019    0.2029
#i.Cidade_taxa_2019                         i.Cidade_taxa_2019    0.1079
#i.Amb_taxa_2019                               i.Amb_taxa_2019    0.2130
#GASTO_PUBLICO_2019                         GASTO_PUBLICO_2019   -0.0392
#TAXA_CRIMINALIDADE_2019               TAXA_CRIMINALIDADE_2019    0.0000
#N_VITIMAS_CVLI_2019                       N_VITIMAS_CVLI_2019    0.0909
#DENSIDADE_DEMOGRAFICA_2010         DENSIDADE_DEMOGRAFICA_2010   -0.0062
#IDHM_PE_2010                                     IDHM_PE_2010   -0.0274
#GINI_2010                                           GINI_2010   -0.0262
#TAXA_ANALFABETISMO_2010               TAXA_ANALFABETISMO_2010    0.0242
#TAXA_DESEMPREGO_2010                     TAXA_DESEMPREGO_2010    0.0781
#NASCIDOS_VIVOS_PE_2010                 NASCIDOS_VIVOS_PE_2010   -0.0057
#OBITOS_INFANTIS_2010                     OBITOS_INFANTIS_2010   -0.0032
#TAXA_MORTALIDADE_INFANTIL_2010 TAXA_MORTALIDADE_INFANTIL_2010    0.0023
#TAXA_URBANIZAÇÃO_2010                   TAXA_URBANIZAÇÃO_2010    0.0178




