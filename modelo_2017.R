# ===================================================================================
# SCRIPT::MODELO DE 2017
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
# MIGUEL SIQUEIRA

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

pacman::p_load(corrplot, readxl, tidyverse, corrgram, GGally, openxlsx, tidyr)

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

Dados_Zscore_2017$GASTO_PUBLICO_2017 <- as.double(Dados_Zscore_2017$GASTO_PUBLICO_2017)

dplyr::glimpse(Dados_Zscore_2017)
summary(Dados_Zscore_2017)
#OK...

# REMOVENDO INUTILIDADES (COLUNAS) PARA A PADRONIZAÇÃO  DO ZSCORE
Dados_Zscore_2017 <- Dados_Zscore_2017[,-3]
Dados_Zscore_2017 <- Dados_Zscore_2017[,-2]
Dados_Zscore_2017 <- Dados_Zscore_2017[,-1]

# TRATANDO NAs POR IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2017 <- Dados_Zscore_2017 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# EXPORTANTO DADOS TRATADOS, SEM A PADRONIZAÇÃO POR ESCORE Z

write.xlsx(Dados_Zscore_2017,"C://Users//Notebook//Desktop//R//TRABALHOFINAL//Dados_Tratados//Tratados_2017.xlsx")

summary(Dados_Zscore_2017)

# ===================================================================================
# ANÁLISE EXPLORATÓRIA DOS DADOS::
# ===================================================================================
# COMPREENDENDO RESUMOS ESTATÍSTICAS A PARTIR DE MEDIDAS DE TENDÊNCIA CENTRAL E BOXPLOT

# TRAFORMANDO DE DADOS DE WIDE PARA LONG
# PRE-PADRONIZAÇÃO
Dados_Zscore_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Dados_Tratados//Tratados_2017.xlsx")

dados_longos_2017 <- Dados_Zscore_2017 %>%
  pivot_longer(
    cols = c(i.Amb, i.Cidade, i.Educ, i.Fiscal, i.GovTI, i.Saúde, i.Plan, IEGM_taxa, Taxa_Criminalidade_2017, Numero_CVLI_2017, Densidade_Demografica_2010, IDHM_2010, GINI_2010, Taxa_Analfabetismo_2010, Taxa_Desemprego_2010, Nascidos_Vivos_2010, Obitos_Infantis_2010, Taxa_Mortalidade_Infantil_2010, Taxa_Urbanização_2010, GASTO_PUBLICO_2017, IDEB_PE_2010),  
    names_to = "variavel",
    values_to = "valor"
  )

ggplot(dados_longos_2017, aes(x = variavel, y = valor)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "",
    x = "Variável",
    y = "Valor"
  ) +
  geom_boxplot(
    fill = "#61988E",                 
    outlier.colour = "#1C544A",           
    outlier.shape = 16,               
    outlier.size = 3                 
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# NÃO FICOU INTELIGIVEL, VOU TER QUE SEPARAR EM TRÊS BLOCOS: VARIAVEIS ADMINSTRATIVAS, AS SOCIAIS QUE CAPTURAM A DESIGUALDADE, E AS ECONÔMICAS 

# ----------------------------------------
# APENAS VARIAVEIS ADMINISTRATIVAS
dados_longos_2017 <- Dados_Zscore_2017 %>%
  pivot_longer(
    cols = c(i.Amb, i.Cidade, i.Educ, i.Fiscal, i.GovTI, i.Saúde, i.Plan),  
    names_to = "variavel",
    values_to = "valor"
  )

ggplot(dados_longos_2017, aes(x = variavel, y = valor)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "",
    x = "Variáveis administrativas",
    y = "Valor"
  ) +
  geom_boxplot(
    fill = "#61988E",                 
    outlier.colour = "#1C544A",           
    outlier.shape = 1,               
    outlier.size = 1                 
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# FILTRANDO QUEM SÃO OS OUTLIERS NAS VARIAVEIS ADMINISTRATIVAS
# CARREGANDO DADOS ANTES DA REMOÇÃO DE COLUNAS DO MUNICIPIO
Dados_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2017.xlsx")

#QUEM SABE UM DIA SEJA POSSÍVEL FAZER UMA ANALISE QUALITATIVA EM CIMA DESSES OUTLIERS
which(Dados_Zscore_2017$i.Amb %in% boxplot.stats(Dados_Zscore_2017$i.Amb)$out)
#[1]   2  75  95 102 114

Dados_2017[2,]   #AFOGADOS DA INGAZEIRA
Dados_2017[75,]  #JABOATÃO DOS GUARARAPES
Dados_2017[95,]  #OROBO
Dados_2017[102,] #PARNAMIRIM
Dados_2017[114,] #RECIFE

which(Dados_Zscore_2017$i.Fiscal %in% boxplot.stats(Dados_Zscore_2017$i.Fiscal)$out)
# [1]  39  58  64  75  81 100 123 128 144

Dados_2017[39,] #CARUARU
Dados_2017[58,] #GOIANA
Dados_2017[64,] #IGUARACI
Dados_2017[75,] #JABOATÃO DOS GUARARPES
Dados_2017[81,] #JUPI
Dados_2017[100,] #PANELAS
Dados_2017[123,] #SANTA FILOMENA
Dados_2017[128,] #SERRITA
Dados_2017[144,] #TERRA NOVA

# AUTOMATIZANDO ISSO

linhas_outliers_fical <- which(Dados_Zscore_2017$i.Fiscal %in% boxplot.stats(Dados_Zscore_2017$i.Fiscal)$out)

# Passo 2: acessar os municípios correspondentes no banco Dados_2017
municipios_outliers <- Dados_2017[linhas_outliers_fical, "Município"]

# Passo 3: visualizar as linhas e os nomes dos municípios
data.frame(Linha = linhas_outliers_fical, Municipio = municipios_outliers)
# ----------------------------------------
#  Linha               Município
#1    39                 CARUARU
#2    58                  GOIANA
#3    64                IGUARACI
#4    75 JABOATÃO DOS GUARARAPES
#5    81                    JUPI
#6   100                 PANELAS
#7   123          SANTA FILOMENA
#8   128                 SERRITA
#9   144              TERRA NOVA
# ----------------------------------------
# ----------------------------------------
# VARIAVEIS SOCIAIS

#PREPADRONIZAÇÃO
# APENAS VARIAVEIS ADMINISTRATIVAS
dados_longos_2017 <- Dados_Zscore_2017 %>%
  pivot_longer(
    cols = c(Taxa_Criminalidade_2017, IDHM_2010, GINI_2010, Taxa_Analfabetismo_2010, Taxa_Desemprego_2010, Taxa_Urbanização_2010, IDEB_PE_2010),  
    names_to = "variavel",
    values_to = "valor"
  )

ggplot(dados_longos_2017, aes(x = variavel, y = valor)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "",
    x = "Variáveis que capturam as desigualdades sociais",
    y = "Valor"
  ) +
  geom_boxplot(
    fill = "#61988E",                 
    outlier.colour = "#1C544A",           
    outlier.shape = 1,               
    outlier.size = 1                 
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dados_longos_2017 <- Dados_Zscore_2017 %>%
  pivot_longer(
    cols = c(Taxa_Criminalidade_2017, IDHM_2010, GINI_2010, Taxa_Analfabetismo_2010, Taxa_Desemprego_2010, Taxa_Urbanização_2010, IDEB_PE_2010),  
    names_to = "variavel",
    values_to = "valor"
  )

#POS-PADRONIZAÇÃO
# UPLOAD DE DADOS PADRONIZADO
Zscore_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito/Zscore_2017.xlsx")

dados_longos_2017_padronizados <- Zscore_2017 %>%
  pivot_longer(
    cols = c(Taxa_Criminalidade_2017, IDHM_2010, GINI_2010, Taxa_Analfabetismo_2010, Taxa_Desemprego_2010, Taxa_Urbanização_2010, IDEB_PE_2010),  
    names_to = "variavel",
    values_to = "valor")

ggplot(dados_longos_2017_padronizados, aes(x = variavel, y = valor)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "",
    x = "Variáveis que capturam as desigualdades sociais",
    y = "Valor"
  ) +
  geom_boxplot(
    fill = "#61988E",                 
    outlier.colour = "#1C544A",           
    outlier.shape = 1,               
    outlier.size = 1                 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ----------------------------------------
# ----------------------------------------
# VARIAVEIS QUE CAPTURAM BIOPODER
# NAO-PADRONIZADAS
  
dados_longos_2017 <- Dados_Zscore_2017 %>%
    pivot_longer(
      cols = c(Nascidos_Vivos_2010, Obitos_Infantis_2010, Numero_CVLI_2017),  
      names_to = "variavel",
      values_to = "valor")
  
  ggplot(dados_longos_2017, aes(x = variavel, y = valor)) +
    geom_boxplot(fill = "lightblue") +
    labs(
      title = "",
      x = "Variáveis que capturam dimensões do biopoder",
      y = "Valor"
    ) +
    geom_boxplot(
      fill = "#61988E",                 
      outlier.colour = "#1C544A",           
      outlier.shape = 1,               
      outlier.size = 1                 
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

# ===================================================================================
# PADRONIZANDO EM ZSCORE
# ===================================================================================

Dados_Zscore_2017 <- Dados_Zscore_2017 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

# EXPORTANDO PADRONIZAÇÃO TABULADA PARA MINHA MAQUINA 

write.xlsx(Dados_Zscore_2017, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2017_1.xlsx")


write.xlsx(Dados_Zscore_2017, "C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2017.xlsx")

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


# ===================================================================================
# GRÁFICO::MATRIZ DE CALOR/CORRELAÇÃO (SLIDES)
# ===================================================================================
# ----------------------------------------
# DADOS 2017
# ----------------------------------------

cor_matrix <- cor(Zscore_2017, use = "complete.obs")
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
cor_matrix17 <- cor(Zscore_2017, use = "complete.obs")
cor_melted17 <- melt(cor_matrix)

# Filtrar apenas correlações altas (acima de 0.5 ou abaixo de -0.5)
cor_melted_filt17 <- cor_melted17[abs(cor_melted$value) > 0.5, ]  

# MATRIZ DE CALOR
ggplot(cor_melted_filt17, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#D5EAE3", mid = "#61988E", high = "#1C544A", midpoint = 0) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

# ---------------------------------------------
# ANO::2017
# ---------------------------------------------

# UPLOAD DE DADOS PADRONIZADO
Zscore_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito/Zscore_2017.xlsx")

summary(Zscore_2017)

# UPLOAD DA VARIAVEL DEPENDENTE: INDICE DE EFETIVADE DA GESTÃO MUNICIPAL
VD_IEGM_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Transversalidade//IEGM_Pernambuco_Manipulado//IEGM_Pernambuco_2017_tratado.xlsx")

summary(VD_IEGM_2017)

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

pca_2017$rotation[1]
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
#Call:
#  lm(formula = Zscore_2017$IEGM_taxa ~ ., data = X_pca_df_2017)

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -1.28357 -0.17176  0.01397  0.18431  0.89064 

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -7.956e-16  1.986e-02    0.00        1    
#  PC1          9.162e-02  9.001e-03   10.18   <2e-16 ***
#  PC2         -5.702e-01  1.234e-02  -46.21   <2e-16 ***
#  PC3         -1.463e-01  1.449e-02  -10.10   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.2701 on 181 degrees of freedom
#Multiple R-squared:  0.9282,	Adjusted R-squared:  0.927 
#F-statistic: 780.4 on 3 and 181 DF,  p-value: < 2.2e-16
#  --------------------------------------------------

model2_pca_2017 <- lm(Zscore_2017$IEGM_taxa ~ -1 + PC1 + PC2 + PC3, 
                     data = X_pca_df_2017)


summary(model2_pca_2017)

plot(model2_pca_2017)
#  --------------------------------------------------

# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSAO PROPRIAMENTE DITA
# ===================================================================================

#  --------------------------------------------------
# OBTENÇÃO DOS BETAS (MODELO ANTES DOS GASTOS PUBLICOS) 

summary(model2_pca_2017)$coefficients

# OUTPUT --------------------------------------------
#       Estimate  Std. Error   t value      Pr(>|t|)
#PC1  0.09162191 0.008976277  10.20712  1.253989e-19
#PC2 -0.57020072 0.012304616 -46.34039 1.085216e-102
#PC3 -0.14626399 0.014446501 -10.12453  2.153049e-19
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
betas_2017 <- c(0.09162191, -0.57020072, -0.14626399)  

# MULTIPLICANDO OS BETAS PELOS PESOS DOS COMPONENTES PRINCIPAIS
# OU SEJA, CALCULA A COMBINAÇÃO LINEAR PARA CADA VARIÁVEL

resultado <- matriz_pesos_pca_2017[, 1:3] %*% betas_2017  

#  --------------------------------------------------
# DATAFRAME PARA O RESULTADO
modelo2017_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2017), Resultado = round(resultado, 4))

#  --------------------------------------------------

print(modelo2017_resultado)

#  --------------------------------------------------

# ===================================================================================
# GRÁFICO::IMPACTO DOS COEFICIENTES DE REGREESSÃO (SLIDES)
# ===================================================================================
# ----------------------------------------
# DADOS 2017
# ----------------------------------------

pacman::p_load(broom)

coeficientes <- tidy(model1_pca_2017)
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

######################################### AO ADICIONAR OUTRA VARIAVEL O INTERCEPTO DEUXOU DE SER SIGNIFICANTE, LOGO A PARTE ABAIXO DEVE SER IGNORADA
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
# OUTPUT::RESULTADO_MODELO_2017
# ===================================================================================

#Variavel Resultado
#i.Amb                                                   i.Amb    0.1989
#i.Cidade                                             i.Cidade    0.1878
#i.Educ                                                 i.Educ    0.1878
#i.Fiscal                                             i.Fiscal    0.0759
#i.GovTI                                               i.GovTI    0.2050
#i.Saúde                                               i.Saúde    0.1575
#i.Plan                                                 i.Plan    0.1753
#IEGM_taxa                                           IEGM_taxa    0.3549
#Taxa_Criminalidade_2017               Taxa_Criminalidade_2017   -0.0152
#Numero_CVLI_2017                             Numero_CVLI_2017   -0.0052
#Densidade_Demografica_2010         Densidade_Demografica_2010   -0.0054
#IDHM_2010                                           IDHM_2010   -0.0098
#GINI_2010                                           GINI_2010    0.0553
#Taxa_Analfabetismo_2010               Taxa_Analfabetismo_2010    0.0123
#Taxa_Desemprego_2010                     Taxa_Desemprego_2010    0.0194
#Nascidos_Vivos_2010                       Nascidos_Vivos_2010   -0.0045
#Obitos_Infantis_2010                     Obitos_Infantis_2010   -0.0072
#Taxa_Mortalidade_Infantil_2010 Taxa_Mortalidade_Infantil_2010   -0.0643
#Taxa_Urbanização_2010                   Taxa_Urbanização_2010    0.0380
#GASTO_PUBLICO_2017                         GASTO_PUBLICO_2017    0.0740
#IDEB_PE_2010                                     IDEB_PE_2010   -0.0208

# ===================================================================================
# GRÁFICO::PREDIÇÃO VS OBSERVADO (AVALIAÇÃO DO MODELO)
# ===================================================================================
# ----------------------------------------
# DADOS 2017
# ----------------------------------------

# OBTENDOS OS VALORES PREDITOS VIA PCA
Zscore_2017$preditos <- predict(model1_pca_2017, newdata = X_pca_df_2017)

# PRIMEIRO PLOT
ggplot(Zscore_2017, aes(x = IEGM_taxa, y = preditos)) +
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
modelo_ajuste <- lm(preditos ~ IEGM_taxa, data = Zscore_2017)

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
ggplot(Zscore_2017, aes(x = IEGM_taxa, y = preditos)) +
  geom_point(size = 2, shape = 1, color = "#1C544A") + 
  geom_smooth(method = "lm", color = "#61988E", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + 
  annotate("text", 
           x = min(Zscore_2017$IEGM_taxa), 
           y = max(Zscore_2017$preditos), 
           label = equacao_texto, 
           hjust = 0, size = 4, color = "black") +  
  annotate("text", 
           x = min(Zscore_2017$IEGM_taxa), 
           y = max(Zscore_2017$preditos) * 0.95, 
           label = r2_texto, 
           hjust = 0, size = 4, color = "black") +
  labs(title = "",
       x = "Variáveis socioeconômicas e administrativas (PE) (Observado)",
       y = "Índice de Efetividade da Gestão Municipal (PE) 
       (Predito)") +
  theme_minimal()

