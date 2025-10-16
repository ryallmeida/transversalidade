<<<<<<< HEAD
# ===================================================================================
# SCRIPT::TRATAMENTO DA VARIAVEL DEPENDENTE - IEGM
# ===================================================================================

if(!require(tidyverse)) 
  install.packages("tidyverse")
library(tidyverse)


if(!require(dplyr)) 
  install.packages("dplyr")
library(dplyr)

if(!require(pacman))
  install.packages("pacman")  
library(pacman)

pacman::p_load(openxlsx, tidyverse, dplyr)

# ===================================================================================
# DADOS 2017
# ===================================================================================

IEGM_Pernambuco_2017 <- read.csv("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Dados_IEGM//IEGM_Brasil_2017.csv")

glimpse(IEGM_Pernambuco_2017)
View(IEGM_Pernambuco_2017)

# UNICA MANIFESTAÇÃO DE PROBLEMA ESTA NA ULTIMA VARIAVEL, A TRATAREMOS EXTRAINDO A LETRA DA TAXA E A COLOCANDO EM OUTRA COLUNA 

separador_2017 <- c("IEGM")

IEGM_Pernambuco_2017 <- IEGM_Pernambuco_2017 %>%
  mutate(across(all_of(separador_2017), 
                .fns = list(taxa = ~ str_extract(., "\\d+\\.?\\d*"),    
                            classificacao = ~ str_extract(., "[A-Za-z]")), 
                .names = "{.col}_{.fn}"))

glimpse(IEGM_Pernambuco_2017)
View(IEGM_Pernambuco_2017)

# REMOVENDO INUTILIDADES
IEGM_Pernambuco_2017 <- IEGM_Pernambuco_2017[, -13]
IEGM_Pernambuco_2017 <- IEGM_Pernambuco_2017[, -11]

# TRANSFORMANDO A VARIÁVEL DEPENDENTE EM DBL
IEGM_Pernambuco_2017$IEGM_taxa <- as.double(IEGM_Pernambuco_2017$IEGM_taxa)

# -----------------------------------------
# EXPORTANDO DADOS PARA UMA TABELA EM EXCEL A FIM DE COLOCAR MAIS VARIAVEIS

if(!require(openxlsx)) 
  install.packages("openxlsx")
library(openxlsx)

write.xlsx(IEGM_Pernambuco_2017, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_Pernambuco_2017_tratado.xlsx")

# ===================================================================================
# DADOS 2019
# ===================================================================================

IEGM_Pernambuco_2019 <- read.csv("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Dados_IEGM//IEGM_Brasil_2019.csv")

glimpse(IEGM_Pernambuco_2019)
View(IEGM_Pernambuco_2019)

# TODAS AS COLUNAS NECESSITAM DE TRATAMENTO

separador_2019 <- c("i.Amb", "i.Cidade", "i.Educ", "i.Fiscal", "i.GovTI", "i.Saúde", "i.Plan", "IEGM")

IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019 %>%
  mutate(across(all_of(separador_2019), 
                .fns = list(taxa = ~ str_extract(., "\\d+\\.?\\d*"),    
                            classificacao = ~ str_extract(., "[A-Za-z]")), 
                .names = "{.col}_{.fn}"))

# REMOVENDO AS LETRAS EXTRAIDAS

IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -11]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -10]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -9]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -8]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -7]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -6]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -5]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -4]

# NOVA ANÁLISE - TUDO OK!
glimpse(IEGM_Pernambuco_2019)
View(IEGM_Pernambuco_2019)
# -----------------------------------------
# EXPORTANDO DADOS PARA UMA TABELA EM EXCEL A FIM DE COLOCAR MAIS VARIAVEIS

write.xlsx(IEGM_Pernambuco_2019, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_Pernambuco_2019_tratado.xlsx")

# ===================================================================================
# DADOS 2021
# ===================================================================================

IEGM_Pernambuco_2021 <- read.csv("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_PE_2021.csv",
                                          skip = 2,
                                          header = FALSE,
                                          sep = ",", 
                                          stringsAsFactors = FALSE,
                                          fill = TRUE,
                                          dec = ",")

# -------------------------------------------
         
colnames(IEGM_Pernambuco_2021) <- c("Município",	"Tribuntal",	"Exercício",	"i-Amb",	"i-Cidade",	"i-Educ",	"i-Fiscal",	"i-GovTI",	"i-Saúde",	"i-Plan",	"IEGM")
         
separar_classOutNumb <- c("i-Amb",	"i-Cidade",	"i-Educ",	"i-Fiscal",	"i-GovTI",	"i-Saúde",	"i-Plan",	"IEGM")
         
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021 %>%
           mutate(across(all_of(separar_classOutNumb), 
                         .fns = list(taxa = ~ str_extract(., "\\d+\\.?\\d*"),    
                                     classificacao = ~ str_extract(., "[A-Za-z]")), 
                         .names = "{.col}_{.fn}"))
         
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -11]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -10]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -9]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -8]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -7]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -6]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -5]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -4]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -2]

# TRANSFORMANDO DE CHARACTER PARA NUMERIC 
         
taxa_IEGM_Sep <- c("i-Amb_taxa", "i-Cidade_taxa", "i-Educ_taxa", "i-Fiscal_taxa", "i-GovTI_taxa", "i-Saúde_taxa", "i-Plan_taxa", "IEGM_taxa")
         
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021 %>%
           mutate(across(all_of(taxa_IEGM_Sep), ~ as.numeric(gsub(",", ".", .))))
         
# TRANFORMANDO COLUNAS CATÉGORICAS EM VARIÁVEIS CATEGÓRICAS ORENÁVEIS 
         
Sep_categoricas_IEGM <- c("i-Amb_classificacao", "i-Cidade_classificacao", "i-Educ_classificacao", "i-Fiscal_classificacao", "i-GovTI_classificacao", "i-Saúde_classificacao", "i-Plan_classificacao", "IEGM_classificacao")
         
# -------------------------------------------

write.xlsx(IEGM_Pernambuco_2021, file = "IEGM_Pernambuco_2021.xlsx")

 # ===================================================================================
 # DADOS 2023
 # ===================================================================================
 
IEGM_Pernambuco <- read.csv("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_CSV.csv",
                            skip = 2,
                            header = FALSE,
                            sep = ";", 
                            stringsAsFactors = FALSE,
                            fill = TRUE,
                            dec = ",")

# -------------------------------------------

colnames(IEGM_Pernambuco) <- c("Município",	"Tribuntal",	"Exercício",	"i-Amb",	"i-Cidade",	"i-Educ",	"i-Fiscal",	"i-GovTI",	"i-Saúde",	"i-Plan",	"IEGM")


# SEPARANDO O NOME DA CLASSE 
separar_classOutNumb <- c("i-Amb",	"i-Cidade",	"i-Educ",	"i-Fiscal",	"i-GovTI",	"i-Saúde",	"i-Plan",	"IEGM")

IEGM_Pernambuco <- IEGM_Pernambuco %>%
  mutate(across(all_of(separar_classOutNumb), 
                .fns = list(taxa = ~ str_extract(., "\\d+\\.?\\d*"),    
                            classificacao = ~ str_extract(., "[A-Za-z]")), 
                .names = "{.col}_{.fn}"))

# REMOVENDO COLUNAS ANTIGAS POS-SEPARAÇÃO 
IEGM_Pernambuco <- IEGM_Pernambuco[, -11]
IEGM_Pernambuco <- IEGM_Pernambuco[, -10]
IEGM_Pernambuco <- IEGM_Pernambuco[, -9]
IEGM_Pernambuco <- IEGM_Pernambuco[, -8]
IEGM_Pernambuco <- IEGM_Pernambuco[, -7]
IEGM_Pernambuco <- IEGM_Pernambuco[, -6]
IEGM_Pernambuco <- IEGM_Pernambuco[, -5]
IEGM_Pernambuco <- IEGM_Pernambuco[, -4]
IEGM_Pernambuco <- IEGM_Pernambuco[, -2]

# TRANSFORMANDO DE CHARACTER PARA NUMERIC 

taxa_IEGM_Sep <- c("i-Amb_taxa", "i-Cidade_taxa", "i-Educ_taxa", "i-Fiscal_taxa", "i-GovTI_taxa", "i-Saúde_taxa", "i-Plan_taxa", "IEGM_taxa")

IEGM_Pernambuco <- IEGM_Pernambuco %>%
  mutate(across(all_of(taxa_IEGM_Sep), ~ as.numeric(gsub(",", ".", .))))

# TRANFORMANDO COLUNAS CATÉGORICAS EM VARIÁVEIS CATEGÓRICAS ORENÁVEIS 

Sep_categoricas_IEGM <- c("i-Amb_classificacao", "i-Cidade_classificacao", "i-Educ_classificacao", "i-Fiscal_classificacao", "i-GovTI_classificacao", "i-Saúde_classificacao", "i-Plan_classificacao", "IEGM_classificacao")

# -------------------------------------------

write.xlsx(IEGM_Pernambuco, file = "IEGM_Pernambuco_2023.xlsx")

# ===================================================================================
# REFERÊNCIAS 
# ===================================================================================

### R DEVELOPMENT CORE TEAM. R: Uma linguagem e ambiente para computação estatística . Viena: R Foundation for Statistical Computing, 2011. ISBN 3-900051-07-0. Disponível em: http://www.R-project.org/ . Acessado em : 6 de jan. de 2025.

### DADOS: https://iegm.irbcontas.org.br/index.php
### FONTE: TRIBUNAL DE CONTAS DO ESTADO DE PERNAMBUCO/REDE INDICON/INSTITUTO RUI BARBOSA
=======
# ===================================================================================
# SCRIPT::TRATAMENTO DA VARIAVEL DEPENDENTE - IEGM
# ===================================================================================

if(!require(tidyverse)) 
  install.packages("tidyverse")
library(tidyverse)


if(!require(dplyr)) 
  install.packages("dplyr")
library(dplyr)

if(!require(pacman))
  install.packages("pacman")  
library(pacman)

pacman::p_load(openxlsx, tidyverse, dplyr)

# ===================================================================================
# DADOS 2017
# ===================================================================================

IEGM_Pernambuco_2017 <- read.csv("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Dados_IEGM//IEGM_Brasil_2017.csv")

glimpse(IEGM_Pernambuco_2017)
View(IEGM_Pernambuco_2017)

# UNICA MANIFESTAÇÃO DE PROBLEMA ESTA NA ULTIMA VARIAVEL, A TRATAREMOS EXTRAINDO A LETRA DA TAXA E A COLOCANDO EM OUTRA COLUNA 

separador_2017 <- c("IEGM")

IEGM_Pernambuco_2017 <- IEGM_Pernambuco_2017 %>%
  mutate(across(all_of(separador_2017), 
                .fns = list(taxa = ~ str_extract(., "\\d+\\.?\\d*"),    
                            classificacao = ~ str_extract(., "[A-Za-z]")), 
                .names = "{.col}_{.fn}"))

glimpse(IEGM_Pernambuco_2017)
View(IEGM_Pernambuco_2017)

# REMOVENDO INUTILIDADES
IEGM_Pernambuco_2017 <- IEGM_Pernambuco_2017[, -13]
IEGM_Pernambuco_2017 <- IEGM_Pernambuco_2017[, -11]

# TRANSFORMANDO A VARIÁVEL DEPENDENTE EM DBL
IEGM_Pernambuco_2017$IEGM_taxa <- as.double(IEGM_Pernambuco_2017$IEGM_taxa)

# -----------------------------------------
# EXPORTANDO DADOS PARA UMA TABELA EM EXCEL A FIM DE COLOCAR MAIS VARIAVEIS

if(!require(openxlsx)) 
  install.packages("openxlsx")
library(openxlsx)

write.xlsx(IEGM_Pernambuco_2017, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_Pernambuco_2017_tratado.xlsx")

# ===================================================================================
# DADOS 2019
# ===================================================================================

IEGM_Pernambuco_2019 <- read.csv("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Dados_IEGM//IEGM_Brasil_2019.csv")

glimpse(IEGM_Pernambuco_2019)
View(IEGM_Pernambuco_2019)

# TODAS AS COLUNAS NECESSITAM DE TRATAMENTO

separador_2019 <- c("i.Amb", "i.Cidade", "i.Educ", "i.Fiscal", "i.GovTI", "i.Saúde", "i.Plan", "IEGM")

IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019 %>%
  mutate(across(all_of(separador_2019), 
                .fns = list(taxa = ~ str_extract(., "\\d+\\.?\\d*"),    
                            classificacao = ~ str_extract(., "[A-Za-z]")), 
                .names = "{.col}_{.fn}"))

# REMOVENDO AS LETRAS EXTRAIDAS

IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -11]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -10]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -9]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -8]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -7]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -6]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -5]
IEGM_Pernambuco_2019 <- IEGM_Pernambuco_2019[, -4]

# NOVA ANÁLISE - TUDO OK!
glimpse(IEGM_Pernambuco_2019)
View(IEGM_Pernambuco_2019)
# -----------------------------------------
# EXPORTANDO DADOS PARA UMA TABELA EM EXCEL A FIM DE COLOCAR MAIS VARIAVEIS

write.xlsx(IEGM_Pernambuco_2019, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_Pernambuco_2019_tratado.xlsx")

# ===================================================================================
# DADOS 2021
# ===================================================================================

IEGM_Pernambuco_2021 <- read.csv("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_PE_2021.csv",
                                          skip = 2,
                                          header = FALSE,
                                          sep = ",", 
                                          stringsAsFactors = FALSE,
                                          fill = TRUE,
                                          dec = ",")

# -------------------------------------------
         
colnames(IEGM_Pernambuco_2021) <- c("Município",	"Tribuntal",	"Exercício",	"i-Amb",	"i-Cidade",	"i-Educ",	"i-Fiscal",	"i-GovTI",	"i-Saúde",	"i-Plan",	"IEGM")
         
separar_classOutNumb <- c("i-Amb",	"i-Cidade",	"i-Educ",	"i-Fiscal",	"i-GovTI",	"i-Saúde",	"i-Plan",	"IEGM")
         
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021 %>%
           mutate(across(all_of(separar_classOutNumb), 
                         .fns = list(taxa = ~ str_extract(., "\\d+\\.?\\d*"),    
                                     classificacao = ~ str_extract(., "[A-Za-z]")), 
                         .names = "{.col}_{.fn}"))
         
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -11]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -10]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -9]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -8]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -7]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -6]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -5]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -4]
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021[, -2]

# TRANSFORMANDO DE CHARACTER PARA NUMERIC 
         
taxa_IEGM_Sep <- c("i-Amb_taxa", "i-Cidade_taxa", "i-Educ_taxa", "i-Fiscal_taxa", "i-GovTI_taxa", "i-Saúde_taxa", "i-Plan_taxa", "IEGM_taxa")
         
IEGM_Pernambuco_2021 <- IEGM_Pernambuco_2021 %>%
           mutate(across(all_of(taxa_IEGM_Sep), ~ as.numeric(gsub(",", ".", .))))
         
# TRANFORMANDO COLUNAS CATÉGORICAS EM VARIÁVEIS CATEGÓRICAS ORENÁVEIS 
         
Sep_categoricas_IEGM <- c("i-Amb_classificacao", "i-Cidade_classificacao", "i-Educ_classificacao", "i-Fiscal_classificacao", "i-GovTI_classificacao", "i-Saúde_classificacao", "i-Plan_classificacao", "IEGM_classificacao")
         
# -------------------------------------------

write.xlsx(IEGM_Pernambuco_2021, file = "IEGM_Pernambuco_2021.xlsx")

 # ===================================================================================
 # DADOS 2023
 # ===================================================================================
 
IEGM_Pernambuco <- read.csv("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_CSV.csv",
                            skip = 2,
                            header = FALSE,
                            sep = ";", 
                            stringsAsFactors = FALSE,
                            fill = TRUE,
                            dec = ",")

# -------------------------------------------

colnames(IEGM_Pernambuco) <- c("Município",	"Tribuntal",	"Exercício",	"i-Amb",	"i-Cidade",	"i-Educ",	"i-Fiscal",	"i-GovTI",	"i-Saúde",	"i-Plan",	"IEGM")


# SEPARANDO O NOME DA CLASSE 
separar_classOutNumb <- c("i-Amb",	"i-Cidade",	"i-Educ",	"i-Fiscal",	"i-GovTI",	"i-Saúde",	"i-Plan",	"IEGM")

IEGM_Pernambuco <- IEGM_Pernambuco %>%
  mutate(across(all_of(separar_classOutNumb), 
                .fns = list(taxa = ~ str_extract(., "\\d+\\.?\\d*"),    
                            classificacao = ~ str_extract(., "[A-Za-z]")), 
                .names = "{.col}_{.fn}"))

# REMOVENDO COLUNAS ANTIGAS POS-SEPARAÇÃO 
IEGM_Pernambuco <- IEGM_Pernambuco[, -11]
IEGM_Pernambuco <- IEGM_Pernambuco[, -10]
IEGM_Pernambuco <- IEGM_Pernambuco[, -9]
IEGM_Pernambuco <- IEGM_Pernambuco[, -8]
IEGM_Pernambuco <- IEGM_Pernambuco[, -7]
IEGM_Pernambuco <- IEGM_Pernambuco[, -6]
IEGM_Pernambuco <- IEGM_Pernambuco[, -5]
IEGM_Pernambuco <- IEGM_Pernambuco[, -4]
IEGM_Pernambuco <- IEGM_Pernambuco[, -2]

# TRANSFORMANDO DE CHARACTER PARA NUMERIC 

taxa_IEGM_Sep <- c("i-Amb_taxa", "i-Cidade_taxa", "i-Educ_taxa", "i-Fiscal_taxa", "i-GovTI_taxa", "i-Saúde_taxa", "i-Plan_taxa", "IEGM_taxa")

IEGM_Pernambuco <- IEGM_Pernambuco %>%
  mutate(across(all_of(taxa_IEGM_Sep), ~ as.numeric(gsub(",", ".", .))))

# TRANFORMANDO COLUNAS CATÉGORICAS EM VARIÁVEIS CATEGÓRICAS ORENÁVEIS 

Sep_categoricas_IEGM <- c("i-Amb_classificacao", "i-Cidade_classificacao", "i-Educ_classificacao", "i-Fiscal_classificacao", "i-GovTI_classificacao", "i-Saúde_classificacao", "i-Plan_classificacao", "IEGM_classificacao")

# -------------------------------------------

write.xlsx(IEGM_Pernambuco, file = "IEGM_Pernambuco_2023.xlsx")

# ===================================================================================
# REFERÊNCIAS 
# ===================================================================================

### R DEVELOPMENT CORE TEAM. R: Uma linguagem e ambiente para computação estatística . Viena: R Foundation for Statistical Computing, 2011. ISBN 3-900051-07-0. Disponível em: http://www.R-project.org/ . Acessado em : 6 de jan. de 2025.

### DADOS: https://iegm.irbcontas.org.br/index.php
### FONTE: TRIBUNAL DE CONTAS DO ESTADO DE PERNAMBUCO/REDE INDICON/INSTITUTO RUI BARBOSA
>>>>>>> 7e219bf4c70a21386428cce5bb91352c5e640d0f
