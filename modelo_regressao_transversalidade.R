<<<<<<< HEAD
### CÓDIGO DA ANÁLISE/ESTUDO ACERCA DO GRAU DE TRANSVERSALIDADE MENSURADO (VIs) OCASIONANDO A EFICIÊNCIA DE POLÍTICAS PÚBLICAS (VD)
### PESQUISA QUANTITATIVA: A PARTIR DE UMA ANÁLISE DE REGRESSÃO LINEAR MÚLTIPLA 
### CPRI/CFCH/UFPE 2024.2 
### DOCENTE DRª. Mª DO CARMO (MQ1)
### CODADO ORIGINALMENTE EM R VERSÃO 4.4.1
### INÍCIO 11-02-25

# ===================================================================================
# MODELO DE REGRESSÃO LINEAR
# ===================================================================================

modelo <- lm(formula, data, subset, weights, na.action,
             method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
             singular.ok = TRUE, contrasts = NULL, offset, ...)

# R-quadrado QUANTI MAIOR MELHOR
# VER AS SIGNIFICANCIAS DO MODELO APRESENTADO POR ***

# ===================================================================================
# CRITÉRIO AIC - CRITÉRIO BIC
# ===================================================================================
# MEDIDA QUE QUANTO MENOR MELHOR 


# ===================================================================================
# ANÁLISE DE CORRELAÇÃO 
# ===================================================================================
#IDENTIFICANTO OUTLIERS

bloxplot.stats(bancodedados$variavelindependente)$out

# ===================================================================================
# UPANDO BANDO DE DADOS E RASPAGEM 
# ===================================================================================

library(MASS)
data(cats)

library(dplyr)
cats <- cats %>%
  mutate(ifelse(Sex == "M", 0, 1))

print(cats)

cats$Sex <- cats$`ifelse(Sex == "M", 0, 1)`
cats <- cats[,-4]

print(cats)
#R2 MENOR QUE 50% DEVE-SE PROCURAR OUTRAS VARIAVEIS
library(corrplot)

# ===================================================================================
# MANUPULANDO 
# ===================================================================================
data("UScrime")

y <- UScrime$y
x <- UScrime[,-16]

# VERIFICANDO CORRELAÇÃO 
# ANALISE DE COMPONENTE PRINCIPAL (PCA) COMBINAÇÃO DE NOVAS VARIÁVEIS QUE VÃO EVITAR CORRELAÇÃO ENTRE VIs FUNÇÃO = REDUZIR CORRELAÇÃO ENTRE VARIAVEIS EXPLICATIVAS E E A REDUÇÃO DE DIMENSIONALIDADE DOS DADOS 

pca <- prcomp(X, center = T, scale. = T)

model <- lm(y ~ .)
#variavel respota tamnaho do corpo variavel explicativa sexo e tamanho do coração 
#analis descritiva sumarry, plo blox plot
#analise de correlação 

=======
### CÓDIGO DA ANÁLISE/ESTUDO ACERCA DO GRAU DE TRANSVERSALIDADE MENSURADO (VIs) OCASIONANDO A EFICIÊNCIA DE POLÍTICAS PÚBLICAS (VD)
### PESQUISA QUANTITATIVA: A PARTIR DE UMA ANÁLISE DE REGRESSÃO LINEAR MÚLTIPLA 
### CPRI/CFCH/UFPE 2024.2 
### DOCENTE DRª. Mª DO CARMO (MQ1)
### CODADO ORIGINALMENTE EM R VERSÃO 4.4.1
### INÍCIO 11-02-25

# ===================================================================================
# MODELO DE REGRESSÃO LINEAR
# ===================================================================================

modelo <- lm(formula, data, subset, weights, na.action,
             method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
             singular.ok = TRUE, contrasts = NULL, offset, ...)

# R-quadrado QUANTI MAIOR MELHOR
# VER AS SIGNIFICANCIAS DO MODELO APRESENTADO POR ***

# ===================================================================================
# CRITÉRIO AIC - CRITÉRIO BIC
# ===================================================================================
# MEDIDA QUE QUANTO MENOR MELHOR 


# ===================================================================================
# ANÁLISE DE CORRELAÇÃO 
# ===================================================================================
#IDENTIFICANTO OUTLIERS

bloxplot.stats(bancodedados$variavelindependente)$out

# ===================================================================================
# UPANDO BANDO DE DADOS E RASPAGEM 
# ===================================================================================

library(MASS)
data(cats)

library(dplyr)
cats <- cats %>%
  mutate(ifelse(Sex == "M", 0, 1))

print(cats)

cats$Sex <- cats$`ifelse(Sex == "M", 0, 1)`
cats <- cats[,-4]

print(cats)
#R2 MENOR QUE 50% DEVE-SE PROCURAR OUTRAS VARIAVEIS
library(corrplot)

# ===================================================================================
# MANUPULANDO 
# ===================================================================================
data("UScrime")

y <- UScrime$y
x <- UScrime[,-16]

# VERIFICANDO CORRELAÇÃO 
# ANALISE DE COMPONENTE PRINCIPAL (PCA) COMBINAÇÃO DE NOVAS VARIÁVEIS QUE VÃO EVITAR CORRELAÇÃO ENTRE VIs FUNÇÃO = REDUZIR CORRELAÇÃO ENTRE VARIAVEIS EXPLICATIVAS E E A REDUÇÃO DE DIMENSIONALIDADE DOS DADOS 

pca <- prcomp(X, center = T, scale. = T)

model <- lm(y ~ .)
#variavel respota tamnaho do corpo variavel explicativa sexo e tamanho do coração 
#analis descritiva sumarry, plo blox plot
#analise de correlação 

>>>>>>> 7a04d81af68795e11250b23ba25039fb27d6743c
