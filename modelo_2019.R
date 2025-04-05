# ===================================================================================
# SCRIPT::MODELO DE 2019
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



dplyr::glimpse(Dados_Zscore_2019)
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
dplyr::glimpse(Dados_Zscore_2019)
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
# GRÁFICO::MATRIZ DE CALOR/CORRELAÇÃO (SLIDES)
# ===================================================================================
# ----------------------------------------
# DADOS 2019
# ----------------------------------------

cor_matrix <- cor(Zscore_2019, use = "complete.obs")
cor_melted <- melt(cor_matrix)

# MATRIZ DE CALOR
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  
  scale_fill_gradient2(low = "#D5EAE3", mid = "#61988E", high = "#1C544A", midpoint = 0) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

# ENFASE NAS CORRELÇÕES MAIS ALTAS
cor_matrix19 <- cor(Zscore_2019, use = "complete.obs")
cor_melted19 <- melt(cor_matrix)

# Filtrar apenas correlações altas (acima de 0.5 ou abaixo de -0.5)
cor_melted_filt19 <- cor_melted19[abs(cor_melted$value) > 0.5, ]  

# MATRIZ DE CALOR
ggplot(cor_melted_filt19, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#D5EAE3", mid = "#61988E", high = "#1C544A", midpoint = 0) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

# ===================================================================================
# GRÁFICO::MATRIZ DE CALOR/CORRELAÇÃO (SLIDES)
# ===================================================================================
# ----------------------------------------
# CARREGANDO DADOS 2019
# ----------------------------------------
# AQUI EU LIMPEI A MEMÓRIA DO R PARA MELHOR MANUSEIO DOS DADOS E PARA TAMBEM EVITAR ERROS

# UPLOAD DATABASE
Zscore_2019 <- readxl::read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito/Zscore_2019.xlsx")

# CALCULO DE CORRELAÇÃO 
cor_matrix2 <- cor(Zscore_2019, use = "complete.obs")
cor_melted2 <- melt(cor_matrix2)

# MATRIZ DE CALOR
ggplot(cor_melted2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  
  scale_fill_gradient2(low = "#D5EAE3", mid = "#61988E", high = "#1C544A", midpoint = 0) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

# ENFASE NAS CORRELÇÕES MAIS ALTAS

cor_melted_filt19 <- cor_melted2[abs(cor_melted2$value) > 0.5, ]  

# MATRIZ DE CALOR
ggplot(cor_melted_filt19, aes(x = Var1, y = Var2, fill = value)) +
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

model_pca_2019 <- lm(Zscore_2019$IEGM_Taxa_2019 ~ ., 
                     data = X_pca_df_2019)
summary(model_pca_2019)


model2_pca_2019 <- lm(Zscore_2019$IEGM_Taxa_2019 ~ -1 + PC1 + PC2 + PC4, 
                      data = X_pca_df_2019)


summary(model2_pca_2019)
# Multiple R-squared:  0.9555,	Adjusted R-squared:  0.9547


# ===================================================================================
# GRÁFICO::IMPACTO DOS COEFICIENTES DE REGREESSÃO (SLIDES)
# ===================================================================================
# ----------------------------------------
# DADOS 2019
# ----------------------------------------

pacman::p_load(broom)

coeficientes <- tidy(model2_pca_2019)
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
plot(model2_pca_2019)
par(mfrow=c(1,1))

#  --------------------------------------------------
# REALIZAÇÃO DE TESTES ESTATÍSTICO, IDENTIFICAÇÃO E TRATAMENTO 

# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSÃO PROPRIAMENTE DITA
# ===================================================================================

summary(model2_pca_2019)$coefficients

#  --------------------------------------------------
#Estimate  Std. Error    t value      Pr(>|t|)
#PC1  0.19893533 0.007718066  25.775283  1.207746e-62
#PC2 -0.54077822 0.009580400 -56.446310 2.862140e-117
#PC4  0.09450452 0.012924060   7.312293  8.052771e-12
#  --------------------------------------------------

# CODIGO ABAIXO PARA A CONSTRUÇÃO DA MATRIZ, PARA A ANALISE FINAL
pca_2019_1 <- prcomp(Zscore_2019, center = TRUE, scale. = TRUE)
print(pca_2019_1)

# COPIANDO DA AREA DE TRANSFERENCIA O QUE EU ISOLEI
pesos_pca_colada_2019 <- read.table("clipboard", header = T)
print(pesos_pca_colada_2019)
View(pesos_pca_colada_2019)

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
betas_2019 <- c(0.19893533, -0.54077822,  0.09450452)
resultado <- matriz_pesos_pca_2019[, 1:3] %*% betas_2019

modelo2019_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2019), Resultado = round(resultado, 4))
#  --------------------------------------------------
print(modelo2019_resultado)
#  --------------------------------------------------
# OUTPUT --------------------------------------------

# ===================================================================================
# OUTPUT::RESULTADO_MODELO_2019
# ===================================================================================

#Variavel Resultado
#IEGM_Taxa_2019                                 IEGM_Taxa_2019    0.3409
#i.Plan_taxa_2019                             i.Plan_taxa_2019    0.1831
#i.Saúde_taxa_2019                           i.Saúde_taxa_2019    0.1458
#i.GovTI_taxa_2019                           i.GovTI_taxa_2019    0.1424
#i.Fiscal_taxa_2019                         i.Fiscal_taxa_2019    0.1934
#i.Educ_taxa_2019                             i.Educ_taxa_2019    0.1942
#i.Cidade_taxa_2019                         i.Cidade_taxa_2019    0.0972
#i.Amb_taxa_2019                               i.Amb_taxa_2019    0.2061
#GASTO_PUBLICO_2019                         GASTO_PUBLICO_2019   -0.0835
#TAXA_CRIMINALIDADE_2019               TAXA_CRIMINALIDADE_2019   -0.0373
#N_VITIMAS_CVLI_2019                       N_VITIMAS_CVLI_2019    0.0836
#DENSIDADE_DEMOGRAFICA_2010         DENSIDADE_DEMOGRAFICA_2010   -0.0114
#IDHM_PE_2010                                     IDHM_PE_2010   -0.0006
#GINI_2010                                           GINI_2010   -0.0399
#TAXA_ANALFABETISMO_2010               TAXA_ANALFABETISMO_2010   -0.0084
#TAXA_DESEMPREGO_2010                     TAXA_DESEMPREGO_2010    0.0673
#NASCIDOS_VIVOS_PE_2010                 NASCIDOS_VIVOS_PE_2010   -0.0104
#OBITOS_INFANTIS_2010                     OBITOS_INFANTIS_2010   -0.0057
#TAXA_MORTALIDADE_INFANTIL_2010 TAXA_MORTALIDADE_INFANTIL_2010    0.0224
#TAXA_URBANIZAÇÃO_2010                   TAXA_URBANIZAÇÃO_2010   -0.0005
#IDEB_PE_2010                                     IDEB_PE_2010    0.0178

# ===================================================================================
# GRÁFICO::PREDIÇÃO VS OBSERVADO (AVALIAÇÃO DO MODELO)
# ===================================================================================
# ----------------------------------------
# DADOS 2019
# ----------------------------------------

colnames(Zscore_2019)

# OBTENDOS OS VALORES PREDITOS VIA PCA
Zscore_2019$preditos <- predict(model2_pca_2019, newdata = X_pca_df_2019)

# PRIMEIRO PLOT
ggplot(Zscore_2019, aes(x = IEGM_Taxa_2019, y = preditos)) +
  geom_point(color = "#61989E", 
             shape = 1,
             size = 2)  +
  geom_abline(slope = 1, intercept = 0, color = "#1C544A", linetype = "dashed") +
  labs(title = "",
       x = "Variáveis sociodemográficas e administrativas (PE) (Observado)",
       y = "Índice de Efetividade da Gestão Municipal (PE) 
       (Predito)") +
  theme_minimal()

# --------------------------------------
# COM A EQUAÇÃO DA RETA E O R^2AJUSTADO

pacman::p_load(ggplot2)

# AJUSTAR OS VALORES OBSERVADOS PELOS VALORES PREDITOS
modelo_ajuste <- lm(preditos ~ IEGM_Taxa_2019, data = Zscore_2019)

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
ggplot(Zscore_2019, aes(x = IEGM_Taxa_2019, y = preditos)) +
  geom_point(size = 2, shape = 1, color = "#1C544A") + 
  geom_smooth(method = "lm", color = "#61988E", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + 
  annotate("text", 
           x = min(Zscore_2019$IEGM_Taxa_2019), 
           y = max(Zscore_2019$preditos), 
           label = equacao_texto, 
           hjust = 0, size = 4, color = "black") +  
  annotate("text", 
           x = min(Zscore_2019$IEGM_Taxa_2019), 
           y = max(Zscore_2019$preditos) * 0.95, 
           label = r2_texto, 
           hjust = 0, size = 4, color = "black") +
  labs(title = "",
       x = "Variáveis socioeconômicas e administrativas (PE) (Observado)",
       y = "Índice de Efetividade da Gestão Municipal (PE) 
       (Predito)") +
  theme_minimal()
