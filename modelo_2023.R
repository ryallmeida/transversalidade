# ===================================================================================
# SCRIPT::MODELO DE 2023
# ===================================================================================

# ENTRE INDICADORES E TRANSVERSALIDADE: UMA ANÁLISE EM PERSPECTIVA DA EFETIVIDADE DA GESTÃO MUNICIPAL COMO EXPRESSÃO DE INDICADORES SOCIOECONÔMICOS E ADMINISTRAAATIVOS EM PERNAMBUCO

### PESQUISA QUANTITATIVA REALIZADA NO DEPARTAMENTO DE CIÊNCIA POLÍTICA (CPRI) A PARTIR DE UMA ANÁLISE DE REGRESSÃO LINEAR MÚLTIPLA 

### DOCENTE DRª. Mª DO CARMO (MQ1)
### CODADO ORIGINALMENTE EM R VERSÃO 4.4.1
### INÍCIO 06-01-25

set.seed(123)
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

dplyr::glimpse(Dados_Zscore_2023)

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
# GRÁFICO::MATRIZ DE CALOR/CORRELAÇÃO (SLIDES)
# ===================================================================================
# ----------------------------------------
# CARREGANDO DADOS 2023
# ----------------------------------------
# AQUI EU LIMPEI A MEMÓRIA DO R NOVAMENTE PARA MELHOR MANUSEIO DOS DADOS E PARA TAMBEM EVITAR ERROS

# UPLOAD DATABASE
Zscore_2023 <- readxl::read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito/Zscore_2023.xlsx")

# CALCULo DE CORRELAÇÃO 
cor_matrix4 <- cor(Zscore_2023, use = "complete.obs")
cor_melted4 <- melt(cor_matrix4)

# MATRIZ DE CALOR
ggplot(cor_melted4, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  
  scale_fill_gradient2(low = "#D5EAE3", mid = "#61988E", high = "#1C544A", midpoint = 0) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

# ENFASE NAS CORRELÇÕES MAIS ALTAS

# Filtrar apenas correlações altas (acima de 0.5 ou abaixo de -0.5)
cor_melted_filt23 <- cor_melted4[abs(cor_melted4$value) > 0.5, ]  

# MATRIZ DE CALOR
ggplot(cor_melted_filt23, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#D5EAE3", mid = "#61988E", high = "#1C544A", midpoint = 0) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

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

model2_pca_2023 <- lm(Zscore_2023$IEGM_taxa_2023 ~ -1 + PC1 + PC2 +  PC3, 
                      data = X_pca_df_2023)

# ---------------------------------------------------
summary(model2_pca_2023)
# ---------------------------------------------------


# SEGUNDO AS TESTAGENS ESTATISTICAS ACIMA PODE-SE INFERIR QUE O MELHOR MODELO REALMENTE É O MODELO SEM INTERCEPTO

# OUTPUT DO MODELO (model2_pca_2023)
# ---------------------------------------------------
# Multiple R-squared:  0.9455,	Adjusted R-squared:  0.9446 
# ---------------------------------------------------

# ===================================================================================
# GRÁFICO::IMPACTO DOS COEFICIENTES DE REGREESSÃO (SLIDES)
# ===================================================================================
# ----------------------------------------
# DADOS 2023
# ----------------------------------------

pacman::p_load(broom)

coeficientes <- tidy(model2_pca_2023)
print(coeficientes)


ggplot2::ggplot(coeficientes, 
                aes(x = term,
                    y = estimate)) +
  geom_col(fill = "#61988E",
           alpha = 0.7) +
  geom_errorbar(aes(ymin = estimate - std.error,
                    ymax = estimate + std.error),
                width = 0.2) +
  coord_flip() +
  labs(title = "",
       x = "Variáveis",
       y = "Estimativa do Coeficiente")

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
#       Estimate  Std. Error   t value      Pr(>|t|)
#PC1 -0.04892997 0.008657055 -5.652034  6.034193e-08
#PC2  0.57286222 0.010672723 53.675356 1.625603e-113
#PC3 -0.09660764 0.012578337 -7.680478  9.430703e-13
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
betas_2023 <- c(-0.04892997, 0.57286222, -0.09660764)
resultado <- matriz_pesos_pca_2023[, 1:3] %*% betas_2023

modelo2023_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2023), Resultado = round(resultado, 4))
#  --------------------------------------------------
print(modelo2023_resultado)
#  --------------------------------------------------

# ===================================================================================
# OUTPUT::RESULTADO_MODELO_2023
# ===================================================================================

#Variavel Resultado
#IEGM_taxa_2023                                 IEGM_taxa_2023    0.3399
#i-Plan_taxa_2023                             i-Plan_taxa_2023    0.2290
#i-Saúde_taxa_2023                           i-Saúde_taxa_2023    0.0676
#i-GovTI_taxa_2023                           i-GovTI_taxa_2023    0.2030
#i-Fiscal_taxa_2023                         i-Fiscal_taxa_2023    0.1874
#i-Educ_taxa_2023                             i-Educ_taxa_2023    0.1741
#i-Cidade_taxa_2023                         i-Cidade_taxa_2023    0.1205
#i-Amb_taxa_2023                               i-Amb_taxa_2023    0.1388
#GASTO_PUBLICO_2023                         GASTO_PUBLICO_2023    0.0675
#TAXA_CRIMINALIDADE_2023               TAXA_CRIMINALIDADE_2023   -0.1003
#N_VITIMAS_CVLI_2023                       N_VITIMAS_CVLI_2023   -0.0682
#DENSIDADE_DEMOGRAFICA_2010         DENSIDADE_DEMOGRAFICA_2010   -0.0088
#IDHM_PE_2010                                     IDHM_PE_2010    0.0081
#GINI_2010                                           GINI_2010    0.0330
#TAXA_ANALFABETISMO_2010               TAXA_ANALFABETISMO_2010   -0.0119
#TAXA_DESEMPREGO_2010                     TAXA_DESEMPREGO_2010    0.0379
#NASCIDOS_VIVOS_PE_2010                 NASCIDOS_VIVOS_PE_2010   -0.0093
#OBITOS_INFANTIS_2010                     OBITOS_INFANTIS_2010   -0.0108
#TAXA_MORTALIDADE_INFANTIL_2010 TAXA_MORTALIDADE_INFANTIL_2010   -0.0079
#TAXA_URBANIZAÇÃO_2010                   TAXA_URBANIZAÇÃO_2010    0.0461
#IDEB_Municipal_2022                       IDEB_Municipal_2022    0.0494


# ===================================================================================
# GRÁFICO::PREDIÇÃO VS OBSERVADO (AVALIAÇÃO DO MODELO)
# ===================================================================================
# ----------------------------------------
# DADOS 2017
# ----------------------------------------

colnames(Zscore_2023)

# OBTENDOS OS VALORES PREDITOS VIA PCA
Zscore_2023$preditos <- predict(model2_pca_2023, newdata = X_pca_df_2023)

# PRIMEIRO PLOT
ggplot(Zscore_2023, aes(x = IEGM_taxa_2023, y = preditos)) +
  geom_point(color = "#61989E", 
             shape = 1,
             size = 2)  +
  geom_abline(slope = 1, intercept = 0, color = "#1C544A", linetype = "dashed") +
  labs(title = "",
       x = "Variáveis socioeconômicas e administrativas (PE) (Observado)",
       y = "Índice de Efetividade da Gestão Municipal (PE) 
       (Predito)") +
  theme_minimal()

# --------------------------------------
# COM A EQUAÇÃO DA RETA E O R^2AJUSTADO

pacman::p_load(ggplot2)

# AJUSTAR OS VALORES OBSERVADOS PELOS VALORES PREDITOS
modelo_ajuste <- lm(preditos ~ IEGM_taxa_2023, data = Zscore_2023)

# OBTER OS COEFICIENTES DA RETA
coeficientes <- coef(modelo_ajuste)
intercepto <- coeficientes[1]
inclinação <- coeficientes[2]

# OOBTENÇÃO DO R^2 AJUSTADO DO MODELO
r2_ajustado <- summary(modelo_ajuste)$adj.r.squared

# BOTAR EQUAÇÃO DA RETA
equacao_texto <- paste0("y = ", round(inclinação, 3), "x + ", round(intercepto, 3))
r2_texto <- paste0("R² Ajustado = ", round(r2_ajustado, 3))

# PLOTANDO OBV X PREDITC EM EXPLICAÇÃO DA VARIAVEL IEGM
ggplot(Zscore_2023, aes(x = IEGM_taxa_2023, y = preditos)) +
  geom_point(size = 2, shape = 1, color = "#1C544A") + 
  geom_smooth(method = "lm", color = "#61988E", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + 
  annotate("text", 
           x = min(Zscore_2023$IEGM_taxa_2023), 
           y = max(Zscore_2023$preditos), 
           label = equacao_texto, 
           hjust = 0, size = 4, color = "black") +  
  annotate("text", 
           x = min(Zscore_2023$IEGM_taxa_2023), 
           y = max(Zscore_2023$preditos) * 0.95, 
           label = r2_texto, 
           hjust = 0, size = 4, color = "black") +
  labs(title = "",
       x = "Variáveis socioeconômicas e administrativas (PE) (Observado)",
       y = "Índice de Efetividade da Gestão Municipal (PE) 
       (Predito)") +
  theme_minimal()