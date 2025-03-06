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
# MIGUEL 
# RYAN ALMEIDA

# CONTATO: ryallmeida@gmail.com

# ===================================================================================
# CARREGANDO PACKAGES DO PROJETO 
# ===================================================================================


# CARREGAR TODAS AS BIBLIOTECAS DE UMA VEZ
if(!require(pacman))
  install.packages("pacman")  
library(pacman)

# PACOTES PARA A TENTATIVA DE TENTAR TRANSFORMAR OS DADOS DO FORMADO WIDE PARA LONG
# SOLICITAM TAMBEM O PACOTE XLSX

if(!require(reshape2))
  install.packages("reshape2")  
library(reshape2)

# PARA LER ARQUIVOS EM EXCEL 
if(!require(readxl))
  install.packages("readxl")  
library(readxl)

if(!require(readr))
  install.packages("readr")  
library(readr)

if(!require(tidyverse))
  install.packages("tidyverse")  
library(tidyverse)  

# PARA A ESTATISTICA DESCRITIVA
if(!require(psych))
  install.packages("psych")  
library(psych)  

if(!require(dplyr))
  install.packages("dplyr") 
library(dplyr)

#PARA A PRODUÇÃO DE GRÁFICOS MAIS SOFISTICADOS 
if(!require(ggplot2))
  install.packages("ggplot2") 
library(ggplot2)


if(!require(magrittr))
  install.packages("magrittr")  
library(magrittr)  

if(!require(units))
  install.packages("units")  
library(units)  

if(!require(corrplot))
  install.packages("corrplot")  
library(corrplot)  

if(!require(tidyr))
  install.packages("tidyr")  
library(tidyr)

# UPANDO PARA O COMPUTADOR BANCO DE DADOS MANIPULADOS AQUI
if(!require(openxlsx))
  install.packages("openxlsx")  
library(openxlsx)

# SE NA SUA BIBLIOTECA JA TIVER USE ESSE CODIDIGO
pacman::p_load(tidyverse, readxl, readr, dplyr, ggplot2, psych, reshape2, magrittr, units, corrplot, tidyr, stringr, openxlsx)