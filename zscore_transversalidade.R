# ===================================================================================
# PADRONIZANDO OS DADOS VIA ZSCORE 
# ===================================================================================

# CARREGAR TODAS AS BIBLIOTECAS DE UMA VEZ
if(!require(pacman))
  install.packages("pacman")  
library(pacman)

# INSTALANDO/CARREGANDO PACOTES
if(!require(readxl)) install.packages("readxl")
library(readxl)

# BATABASE_2022
# CARREGANDO... 
Dados_Zscore_2022 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2022.xlsx")

glimpse(Dados_Zscore_2022)
view(Dados_Zscore_2022)

# TRANSFORMANDO VARIAVEIS EM NUMERIC
informaçõesSeparadas_Zscr_2022 <- c("Taxa_Criminalidade_2022", "Numero_Vitimas_CVLI_2022", "Taxa_Mortalidade_Infantil_2022")

Dados_Zscore_2022 <- Dados_Zscore_2022 %>%
  mutate(across(all_of(informaçõesSeparadas_Zscr_2022), ~ as.numeric(gsub(",", ".", .))))

summary(Dados_Zscore_2022)

# REMOVENDO INUTILIDADES
Dados_Zscore_2022 <- Dados_Zscore_2022[,-8]
Dados_Zscore_2022 <- Dados_Zscore_2022[,-7]
Dados_Zscore_2022 <- Dados_Zscore_2022[,-2]
Dados_Zscore_2022 <- Dados_Zscore_2022[,-1]


# TRATANDO N, INF E NAN POR IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2022 <- Dados_Zscore_2022 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Aplicar Z-score para todas as variáveis numéricas
Dados_Zscore_2022 <- Dados_Zscore_2022 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

view(Dados_Zscore_2022)
# DEU SUPER CERTO
