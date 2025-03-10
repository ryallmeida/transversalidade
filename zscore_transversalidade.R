# ===================================================================================
# SCRIPT::PADRONIZANDO OS DADOS VIA ZSCORE 
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

# -----------------------------------------

# CARREGAR TODAS AS BIBLIOTECAS DE UMA VEZ
if(!require(pacman))
  install.packages("pacman")  
library(pacman)

pacman::p_load(corrplot, readxl, tidyverse, corrgram, GGally)

# -----------------------------------------

# BATABASE_2022
# CARREGANDO... 
Dados_Zscore_2022_3 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2022.xlsx")

glimpse(Dados_Zscore_2022_3)
view(Dados_Zscore_2022_3)
# NA ANALISE DESCRITIVA OBSERVA-SE VARIAVEIS QUE NÃO DEVERIAM ESTAR COMO CHR

# TRANSFORMANDO VARIAVEIS EM NUMERIC
informaçõesSeparadas_Zscr_2022_3 <- c("Taxa_Criminalidade_2022", "Numero_Vitimas_CVLI_2022", "Taxa_Mortalidade_Infantil_2022")

Dados_Zscore_2022_3 <- Dados_Zscore_2022_3 %>%
  mutate(across(all_of(informaçõesSeparadas_Zscr_2022_3), ~ as.numeric(gsub(",", ".", .))))

# ------------------------------------------
# ANÁLISE DESCRITIVA DOS DADOS
summary(Dados_Zscore_2022_3)
# OBSERVA-SE A INTRODUÇÃO DE VARIOS NAs POR COERÇÃO


# PROBLEMAS NAS VARIAVEIS: TAXA DE MORTALIDADE, TAXA DE CRIMINALIDADE E NUMEROS DE VITIMAS DE CRIME VIOLNETO [...]
# PROBLEMAS TRATATOS
Dados_Zscore_2022_3$Numero_Vitimas_CVLI_2022 <- as.integer(Dados_Zscore_2022_3$Numero_Vitimas_CVLI_2022)

Dados_Zscore_2022_3$Taxa_Mortalidade_Infantil_2022 <- as.double(Dados_Zscore_2022_3$Taxa_Mortalidade_Infantil_2022)

Dados_Zscore_2022_3$Taxa_Criminalidade_2022 <- as.double(Dados_Zscore_2022_3$Taxa_Criminalidade_2022)

# REMOVENDO INUTILIDADES
Dados_Zscore_2022_3 <- Dados_Zscore_2022_3[,-8]
Dados_Zscore_2022_3 <- Dados_Zscore_2022_3[,-7]
Dados_Zscore_2022_3 <- Dados_Zscore_2022_3[,-2]
Dados_Zscore_2022_3 <- Dados_Zscore_2022_3[,-1]

# TRATANDO NAs POR IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2022_3 <- Dados_Zscore_2022_3 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# ===================================================================================
# ANALISANDO MATRIZ DE CORRELAÇÃO 
# ===================================================================================

# ALTERNATIVA AO CORRPLOT
# CODO NA VERSÃO 4.4.1. E POR ALGUM MOTIVO O CORRPLOT NÃO ESTÁ RODANDO NA MINHA MÁQUINA

# Error in corrplot(dados) : The matrix is not in [-1, 1]!
# -----------------------------------------

install.packages("GGally")
library(GGally)

install.packages("corrgram")
library(corrgram)

# -----------------------------------------

# PARA A ANÁLISE REMOVE-SE A VARIAVEL DEPENDENTE E TODAS AS OUTRAS QUE SE MANIFESTEM COMO CHR 
dados <-  Dados_Zscore_2022_3[,-9]
view(dados)
cor(dados)

# ATRAVES DA MATRIZ DE CORRELAÇÃO ANALISA-SE ALTO GRAU DE CORRELAÇÃO POSITIVO ENTRE DENSIDADE DEMOGRÁFICA E TAXA DE ANALFABETISMO, E UM ALTO GRAU DE CORRELAÇÃO NEGATIVA ENTRE IDH MUNICIPAL E NUMEROS DE VITIMAS DE CRIME VIOLENTO 

# A FIM DE TRATAR ESSA CORRELAÇÃO UTILIZAREI A TÉCNICA DE PRINCIPAL COMPONENT ANALYSIS (PCA)

corrplot(dados)
ggcorr(dados)
corrplot(dados)
corrgram(dados)

# ===================================================================================
# PADRONIZANDO OS DADOS (PROPRIAMENTE DITOS E JÁ TRATATOS) VIA ZSCORE PARA TODAS AS VARIAVEIS NUMERICAS 
# ===================================================================================

# A ESCOLHA DE UTILIZAR ZSCORE PARA A PADRONIZAÇÃO, SE DEU PORQUÊ MUITOS DADOS PARA COMPLETAR A TABULAÇÃO DOS DADOS (VIA VARÍAVEIS), E QUE O DADO DO ANO PROPRIAMENTE DITO NÃO EXISTIA; LOGO SÓ FOI POSSÍVEL COLHER DE DADOS DE OUTROS ANOS E REUTIZÁ-LOS EM OUTRAS TABULAÇÕES 

Dados_Zscore_2022_3 <- Dados_Zscore_2022_ %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

# -----------------------------------------


