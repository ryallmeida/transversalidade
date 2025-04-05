# ===================================================================================
# SCRIPT::MODELO DE 2022
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
# MIGUEL

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

Dados_Zscore_2022$TAXA_CRIMINALIDADE_2022 <- as.double(Dados_Zscore_2022$TAXA_CRIMINALIDADE_2022)

Dados_Zscore_2022$DENSIDADE_DEMOGRÁFICA_2022 <- as.integer(Dados_Zscore_2022$DENSIDADE_DEMOGRÁFICA_2022)

Dados_Zscore_2022$N_CVLI_2022 <- as.integer(Dados_Zscore_2022$N_CVLI_2022)

Dados_Zscore_2022$TAXA_MORTALIDADE_INFANTIL_2022 <- as.double(Dados_Zscore_2022$TAXA_MORTALIDADE_INFANTIL_2022)
#REMOVER A COLUNA DE TAXA DE MORTALIDADE INFANTIL DE 2010

summary(Dados_Zscore_2022)
#OK...

# REMOVENDO INUTILIDADES (COLUNAS) PARA A PADRONIZAÇÃO  DO ZSCORE
Dados_Zscore_2022 <- Dados_Zscore_2022[,-22]
Dados_Zscore_2022 <- Dados_Zscore_2022[,-2]
Dados_Zscore_2022 <- Dados_Zscore_2022[,-1]

colnames(Dados_Zscore_2022)

Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "NUMEROS"  ]
Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "MUNICIPIOS"]
Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "...19"]

Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "TAXA_MORTALIDADE_INFANTIL_2010"]

dplyr::glimpse(Dados_Zscore_2022)
Dados_Zscore_2022$
  
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

write.xlsx(Dados_Zscore_2022, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2022.xlsx")


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

# ===================================================================================
# GRÁFICO::MATRIZ DE CALOR/CORRELAÇÃO (SLIDES)
# ===================================================================================
# ----------------------------------------
# CARREGANDO DADOS 2022
# ----------------------------------------

# AQUI EU LIMPEI A MEMÓRIA DO R NOVAMENTE PARA MELHOR MANUSEIO DOS DADOS E PARA TAMBEM EVITAR ERROS

# UPLOAD DATABASE
Zscore_2022 <- readxl::read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito/Zscore_2022.xlsx")

# CALCULo DE CORRELAÇÃO 
cor_matrix3 <- cor(Zscore_2022, use = "complete.obs")
cor_melted3 <- melt(cor_matrix3)

# MATRIZ DE CALOR
ggplot(cor_melted3, aes(x = Var1, y = Var2, fill = value)) +
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
cor_melted_filt22 <- cor_melted3[abs(cor_melted3$value) > 0.5, ]  

# MATRIZ DE CALOR
ggplot(cor_melted_filt22, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#D5EAE3", mid = "#61988E", high = "#1C544A", midpoint = 0) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

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

model_pca_2022 <- lm(Zscore_2022$IEGM_taxa_2021 ~ ., 
                     data = X_pca_df_2022)

summary(model_pca_2022)

model2_pca_2022 <- lm(Zscore_2022$IEGM_taxa_2021 ~ -1 + PC1 + PC2 + PC3 + PC4, 
                      data = X_pca_df_2022)

#  --------------------------------------------------
summary(model2_pca_2022)
#  --------------------------------------------------

# ===================================================================================
# GRÁFICO::IMPACTO DOS COEFICIENTES DE REGREESSÃO (SLIDES)
# ===================================================================================
# ----------------------------------------
# DADOS 2022
# ----------------------------------------

pacman::p_load(broom)

coeficientes <- tidy(model2_pca_2022)
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
# SCRIPT::ANALISE DO MODELO DE REGRESSAO PROPRIAMENTE DITA
# ===================================================================================

#  --------------------------------------------------
# OBTENÇÃO DOS BETAS 

summary(model2_pca_2022)$coefficients

# OUTPUT --------------------------------------------
#       Estimate  Std. Error   t value      Pr(>|t|)
#PC1 -0.01564622 0.006453700 -2.424379  1.631794e-02
#PC2  0.58872948 0.007793621 75.539917 8.353028e-139
#PC3 -0.07264423 0.009126200 -7.959964  1.828477e-13
#PC4 -0.07524914 0.010038460 -7.496084  2.827199e-12
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

#NESSE CODIGO NÃO FOI PRECISO REMOVER O PC4

# RETOMANDO O CODIGO...
matriz_pesos_pca_2022 <- as.matrix(pesos_pca_colada_2022)
print(matriz_pesos_pca_2022)

#  --------------------------------------------------
#  --------------------------------------------------

# BETAS 1, 2, 3 E 4 (RESPECTIVAMENTE)

betas_2022 <- c(-0.01564622, 0.58872948, -0.07264423, -0.07524914)  


# MULTIPLICANDO OS BETAS PELOS PESOS DOS COMPONENTES PRINCIPAIS
# OU SEJA, CALCULA A COMBINAÇÃO LINEAR PARA CADA VARIÁVEL

resultado <- matriz_pesos_pca_2022[, 1:4] %*% betas_2022  

#  --------------------------------------------------
# DATAFRAME PARA O RESULTADO
modelo2022_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2022), Resultado = round(resultado, 4))

#  --------------------------------------------------

print(modelo2022_resultado)

#  --------------------------------------------------
# ===================================================================================
# OUTPUT::RESULTADO_MODELO_2022
# ===================================================================================
#Variavel Resultado
#TAXA_CRIMINALIDADE_2022               TAXA_CRIMINALIDADE_2022    0.0261
#DENSIDADE_DEMOGRÁFICA_2022         DENSIDADE_DEMOGRÁFICA_2022    0.0007
#IDHM_2022                                           IDHM_2022   -0.0398
#TAXA_ANALFABETISMO_2022               TAXA_ANALFABETISMO_2022   -0.0782
#IDEB_Municipal_2022                       IDEB_Municipal_2022   -0.0042
#N_CVLI_2022                                       N_CVLI_2022   -0.0380
#TAXA_MORTALIDADE_INFANTIL_2022 TAXA_MORTALIDADE_INFANTIL_2022   -0.0446
#GASTO_PUBLICO_2022                         GASTO_PUBLICO_2022   -0.0193
#IEGM_taxa_2021                                 IEGM_taxa_2021    0.3578
#i-Plan_taxa_2021                             i-Plan_taxa_2021    0.1896
#i-Saúde_taxa_2021                           i-Saúde_taxa_2021    0.1874
#i-GovTI_taxa_2021                           i-GovTI_taxa_2021    0.1437
#i-Fiscal_taxa_2021                         i-Fiscal_taxa_2021    0.1766
#i-Educ_taxa_2021                             i-Educ_taxa_2021    0.2296
#i-Cidade_taxa_2021                         i-Cidade_taxa_2021    0.0909
#i-Amb_taxa_2021                               i-Amb_taxa_2021    0.1825
#TAXA_DESEMPREGO_2010                     TAXA_DESEMPREGO_2010    0.0005
#NASCIDOS_VIVOS_PE_2010                 NASCIDOS_VIVOS_PE_2010   -0.0027
#OBITOS_INFANTIS_2010                     OBITOS_INFANTIS_2010   -0.0085
#TAXA_URBANIZAÇÃO_2010                   TAXA_URBANIZAÇÃO_2010    0.0039
#GINI_2010                                           GINI_2010   -0.0172


# ===================================================================================
# GRÁFICO::PREDIÇÃO VS OBSERVADO (AVALIAÇÃO DO MODELO)
# ===================================================================================
# ----------------------------------------
# DADOS 2022
# ----------------------------------------

colnames(Zscore_2022)

# OBTENDOS OS VALORES PREDITOS VIA PCA
Zscore_2022$preditos <- predict(model2_pca_2022, newdata = X_pca_df_2022)

# PRIMEIRO PLOT
ggplot(Zscore_2022, aes(x = IEGM_taxa_2022, y = preditos)) +
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
modelo_ajuste <- lm(preditos ~ IEGM_taxa_2021, data = Zscore_2022)

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
ggplot(Zscore_2022, aes(x = IEGM_taxa_2021, y = preditos)) +
  geom_point(size = 2, shape = 1, color = "#1C544A") + 
  geom_smooth(method = "lm", color = "#61988E", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + 
  annotate("text", 
           x = min(Zscore_2022$IEGM_taxa_2021), 
           y = max(Zscore_2022$preditos), 
           label = equacao_texto, 
           hjust = 0, size = 4, color = "black") +  
  annotate("text", 
           x = min(Zscore_2022$IEGM_taxa_2021), 
           y = max(Zscore_2022$preditos) * 0.95, 
           label = r2_texto, 
           hjust = 0, size = 4, color = "black") +
  labs(title = "",
       x = "Variáveis socioeconômicas e administrativas (PE) (Observado)",
       y = "Índice de Efetividade da Gestão Municipal (PE) 
       (Predito)") +
  theme_minimal()