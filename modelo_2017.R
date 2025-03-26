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
Dados_Zscore_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2017.xlsx")

glimpse(Dados_Zscore_2017)
view(Dados_Zscore_2017)
# DEU ERRO DE LEITURA RESULTANDO EM CHR APENAS EM NUMEROS DE OBITOS INFANTIS, ASSIM COMO EM TAXA DE MORTALIDADE INFANTIL

Dados_Zscore_2017$Obitos_Infantis_2010 <- as.integer(Dados_Zscore_2017$Obitos_Infantis_2010)

Dados_Zscore_2017$Taxa_Mortalidade_Infantil_2010 <- as.double(Dados_Zscore_2017$Taxa_Mortalidade_Infantil_2010)

summary(Dados_Zscore_2017)
#OK...

# REMOVENDO INUTILIDADES (COLUNAS) PARA A PADRONIZAÇÃO  DO ZSCORE
Dados_Zscore_2017 <- Dados_Zscore_2017[,-3]
Dados_Zscore_2017 <- Dados_Zscore_2017[,-2]
Dados_Zscore_2017 <- Dados_Zscore_2017[,-1]

# TRATANDO NAs POR IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2017 <- Dados_Zscore_2017 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

summary(Dados_Zscore_2017)

# ===================================================================================
# PADRONIZANDO EM ZSCORE
# ===================================================================================

Dados_Zscore_2017 <- Dados_Zscore_2017 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

# EXPORTANDO PADRONIZAÇÃO TABULADA PARA MINHA MAQUINA 

write.xlsx(Dados_Zscore_2017, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2017_1.xlsx")

# ===================================================================================
# ANALISE DE CORRELAÇÃO
# ===================================================================================

# FIZ O UPLOAD NOVAMENTE DO DATABASE ORIGINAL PARA CONFERIR SE HÁ CORRELAÇÃO ANTES E DEPOIS DA PADRONIZAÇÃO DOS DADOS

Dados_Zscore_2017_1 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2017.xlsx")

# ANTES DA PADRONIZAÇÃO...
corrgram(Dados_Zscore_2017_1)
# HÁ ALTOS GRAUS DE CORRELAÇÃO EM APROXIMADAMENTE 2/3 DAS VARIAVEIS

# PÓS-PADRONIZAÇÃO 
corrgram(Zscore_2017)
# A CORRELAÇÃO MANTEM-SE IGUAL

# ---------------------------------------------
# ANO::2017
# ---------------------------------------------

# UPLOAD DE DADOS PADRONIZADO
Zscore_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito/Zscore_2017_1.xlsx")

summary(Zscore_2022)

# UPLOAD DA VARIAVEL DEPENDENTE: INDICE DE EFETIVADE DA GESTÃO MUNICIPAL
VD_IEGM_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_Pernambuco_2017_tratado.xlsx")

summary(IEGM_2021)

# ===================================================================================
# TRATAMENTO DA CORRELAÇÃO::PRINCIPAL_COMPONENT_ANALYSIS
# ===================================================================================

pca_2017 <- prcomp(Zscore_2017, center = TRUE, scale. = TRUE)
summary(pca_2017)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2017)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

# ---------------------------------------------------

pca_2022$rotation[1]
plot(pca_2017)

# ===================================================================================
# CONSTRUÇÃO DO MODELO 2017
# ===================================================================================

#OBTER COMPONENTES PRINCIPAIS 
X_pca_2017 <- pca_2017$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
X_pca_df_2017 <- as.data.frame(X_pca_2017)

# CONSTRUIR O MODELO DE REGRESSÃO USANDO OS COMPENTENTES PRINCIPAIS

model1_pca_2017 <- lm(Zscore_2017$IEGM_taxa ~ ., 
                      data = X_pca_df_2017)
summary(model1_pca_2017)

# MELHOR MODELO É O MODEL1
#  --------------------------------------------------
#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.074202 -0.009495  0.000714  0.010295  0.051428 

#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  0.5260703  0.0011465 458.857   <2e-16 ***
#  PC1          0.0052011  0.0005204   9.995   <2e-16 ***
#  PC2         -0.0329648  0.0007132 -46.219   <2e-16 ***
#  PC3         -0.0080346  0.0008401  -9.564   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  --------------------------------------------------

model2_pca_2017 <- lm(Zscore_2017$IEGM_taxa ~ -1 + PC1 + PC2 + PC3, 
                     data = X_pca_df_2017)


summary(model_pca_2017)

plot(model_pca_2017)
#  --------------------------------------------------

# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSAO PROPRIAMENTE DITA
# ===================================================================================

#  --------------------------------------------------
# OBTENÇÃO DOS BETAS (MODELO ANTES DOS GASTOS PUBLICOS) 

summary(model_pca_2017)$coefficients

# OUTPUT --------------------------------------------
#       Estimate  Std. Error    t value      Pr(>|t|)
# PC1  0.08722669 0.008241122  10.584322  1.047680e-20
# PC2 -0.58545379 0.011347771 -51.591966 1.406539e-110
# PC3 -0.11390072 0.013319954  -8.551135  4.891236e-15
#  --------------------------------------------------

#OBTENÇÃO DOS BETAS PÓS GASTOS PUBLICOS

summary(model1_pca_2017)$coefficients
#  --------------------------------------------------
# Estimate   Std. Error    t value      Pr(>|t|)
# (Intercept)  0.526070270 0.0011464804 458.856761 1.973613e-279
# PC1          0.005201111 0.0005203739   9.994949  5.234982e-19
# PC2         -0.032964844 0.0007132257 -46.219370 3.803312e-102
# PC3         -0.008034567 0.0008400644  -9.564228  8.469622e-18
#  --------------------------------------------------

# NOTAS -----------------------------------------------------------------------------
# HOUVE A NECESSIDADE DE CRIAR UMA MATRIZ PARA CALCULAR DE FORMA AUTOMATIZADA
# O QUANTO A VARIAVEL AFETA A VARIÁVEL DEPENDENTE  
# IDEIA PRINCIPAL: 4 COLUNAS REPRESENTANDO OS COMPONENTES PRINCIPAIS E 17 OBS. REPRESENTANDO AS 19 VARIÁVEIS

# CODIGO ABAIXO PARA A CONSTRUÇÃO DA MATRIZ, PARA A ANALISE FINAL
pca_2017_1 <- prcomp(Zscore_2017, center = TRUE, scale. = TRUE)
print(pca_2017_1)

# COPIANDO DA AREA DE TRANSFERENCIA O QUE EU ISOLEI
pesos_pca_colada_2017 <- read.table("clipboard", header = T)
print(pesos_pca_colada_2017)
View(pesos_pca_colada_2017)

pesos_pca_colada_2017 <- pesos_pca_colada_2017[,-5]
# NÃO ESTOU CONSEGUINDO EXCLUIR PCA4, TENHO QU FORÇAR A AÇÃO

colnames(pesos_pca_colada_2017)
pesos_pca_colada_2017 <- pesos_pca_colada_2017[, !colnames(pesos_pca_colada_2017) %in% "PC4"]

# RETOMANDO O CODIGO...
matriz_pesos_pca_2017 <- as.matrix(pesos_pca_colada_2017)
print(matriz_pesos_pca_2017)

#  --------------------------------------------------
#  --------------------------------------------------

# BETAS 1, 2, 3 (RESPECTIVAMENTE), DADOS ABAIXO ANTES DE GASTO PUBLICO
betas_2017 <- c(0.08722669, -0.58545379, -0.11390072)  

# BETAS 0, 1, 2, 3 (RESPECTIVAMENTE), DADOS ABAIXO ANTES DE GASTO PUBLICO

betas_2017 <- c(0.526070270, 0.005201111, -0.032964844, -0.008034567)  

# MULTIPLICANDO OS BETAS PELOS PESOS DOS COMPONENTES PRINCIPAIS
# OU SEJA, CALCULA A COMBINAÇÃO LINEAR PARA CADA VARIÁVEL

resultado <- matriz_pesos_pca_2017[, 1:3] %*% betas_2017  

#  --------------------------------------------------
# DATAFRAME PARA O RESULTADO
modelo2017_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2017), Resultado = round(resultado, 4))

#  --------------------------------------------------

print(modelo2017_resultado)

#  --------------------------------------------------

# DEVIDO O INTERCEPTO TER DADO SIGNIFICATIVO NESSE MODELO TIVE QUE ALTERAR A ESTRUTURA DO CODIGO DESSE MODELO DE REGRESSAÕ EM ESPECIFICO 

# Vetor de betas (incluindo o intercepto beta_0)
betas_2017 <- c(0.526070270, 0.005201111, -0.032964844, -0.008034567)  

# Multiplicação da matriz de pesos dos três primeiros componentes principais pelos betas correspondentes
y_predito <- matriz_pesos_pca_2017[, 1:3] %*% betas_2017[2:4]  

# Adiciona o intercepto (beta_0) a todos os valores previstos
y_predito <- y_predito + betas_2017[1]

# Exibir o resultado
y_predito

#  --------------------------------------------------
# ===================================================================================
# OUTPUT::RESULTADO_MODELO_2017_COM_BETA-0
# ===================================================================================
#BETA-0 = 0.526070270

#[,1]
#i.Amb                          1.063634
#i.Cidade                       1.062994
#i.Educ                         1.062958
#i.Fiscal                       1.056544
#i.GovTI                        1.063952
#i.Saúde                        1.061185
#i.Plan                         1.062235
#IEGM_taxa                      1.072605
#Taxa_Criminalidade_2017        1.051194
#Numero_CVLI_2017               1.051854
#Densidade_Demografica_2010     1.051853
#IDHM_2010                      1.051603
#GINI_2010                      1.055367
#Taxa_Analfabetismo_2010        1.052831
#Taxa_Desemprego_2010           1.053265
#Nascidos_Vivos_2010            1.051907
#Obitos_Infantis_2010           1.051750
#Taxa_Mortalidade_Infantil_2010 1.048458
#Taxa_Urbanização_2010          1.054288
#GASTO_PUBLICO_2017             1.056393

# ===================================================================================
# OUTPUT::RESULTADO_MODELO_2017
# ===================================================================================


# i.Amb                                                   i.Amb    0.2051
# i.Cidade                                             i.Cidade    0.1930
# i.Educ                                                 i.Educ    0.1864
# i.Fiscal                                             i.Fiscal    0.0801
# i.GovTI                                               i.GovTI    0.2067
# i.Saúde                                               i.Saúde    0.1600
# i.Plan                                                 i.Plan    0.1819
# IEGM_taxa                                           IEGM_taxa    0.3633
# Taxa_Criminalidade_2017               Taxa_Criminalidade_2017   -0.0104
# Numero_CVLI_2017                             Numero_CVLI_2017   -0.0052
# Densidade_Demografica_2010         Densidade_Demografica_2010   -0.0062
# IDHM_2010                                           IDHM_2010   -0.0008
# GINI_2010                                           GINI_2010    0.0506
# Taxa_Analfabetismo_2010               Taxa_Analfabetismo_2010    0.0027
# Taxa_Desemprego_2010                     Taxa_Desemprego_2010    0.0248
# Nascidos_Vivos_2010                       Nascidos_Vivos_2010   -0.0052
# Obitos_Infantis_2010                     Obitos_Infantis_2010   -0.0080
# Taxa_Mortalidade_Infantil_2010 Taxa_Mortalidade_Infantil_2010   -0.0653
# Taxa_Urbanização_2010                   Taxa_Urbanização_2010    0.0416

