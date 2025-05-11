# ===================================================================================
# SCRIPT::ANALISE DE DIAGNOSTICO DO MODELO DE 2022
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

colnames(Dados_Zscore_2022)

Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "NUMEROS"  ]
Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "MUNICIPIOS"]
Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "...19"]

Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "NASCIDOS_VIVOS_PE_2010"]

Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "DENSIDADE_DEMOGRÁFICA_2022"]

Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "N_CVLI_2022"]

Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "GASTO_PUBLICO_2022"]

Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "OBITOS_INFANTIS_2010"]

Dados_Zscore_2022 <- Dados_Zscore_2022[, !colnames(Dados_Zscore_2022) %in% "TAXA_MORTALIDADE_INFANTIL_2010"]

dplyr::glimpse(Dados_Zscore_2022)

# TRATANDO NAs POR IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2022 <- Dados_Zscore_2022 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))


# ===================================================================================
# PADRONIZANDO EM ZSCORE
# ===================================================================================

Zscore_2022 <- Dados_Zscore_2022 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))
glimpse(Zscore_2022)

# ===================================================================================
# ANALISE DE CORRELAÇÃO ENTRE VAR. INDEP.
# ===================================================================================
colnames(Zscore_2022)
SEM_VD <- Zscore_2022[, !colnames(Zscore_2022) %in% "IEGM_taxa_2021"]
corrgram::corrgram(SEM_VD)

# ===================================================================================
# TRATAMENTO DA CORRELAÇÃO POR PCA
# ===================================================================================

pca_2022 <- prcomp(SEM_VD, center = TRUE, scale. = TRUE)
summary(pca_2022)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2022)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

#  --------------------------------------------------
#  --------------------------------------------------
# ISOLANDO VARIAVEL DEPENDENTE
y <- Zscore_2022$IEGM_taxa_2021

# ISOLANDOS VARIAVEIS INDEPENDENTES (X) COM PCA
X_pca_2022 <- pca_2022$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
modelo_data2022 <- as.data.frame(cbind(y, X_pca_2022))

#  --------------------------------------------------
model1_pca_2022 <- lm(modelo_data2022$y ~ -1 + PC1 + PC2 + PC3 + PC4, data = modelo_data2022)
#  --------------------------------------------------
# Adjusted R-squared:  0.8822, PC3 NÃO SIGNIFICANTE 
summary(model1_pca_2022)

# Call:
# lm(formula = modelo_data2022$y ~ -1 + PC1 + PC2 + PC3 + PC4, 
#     data = modelo_data2022)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.82317 -0.20975 -0.00041  0.22238  0.92112 

# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# PC1 -0.27739    0.01718  -16.15   <2e-16 ***
# PC2 -0.56249    0.01757  -32.01   <2e-16 ***
# PC3  0.28717    0.01982   14.49   <2e-16 ***
# PC4 -0.21388    0.02122  -10.08   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.3217 on 181 degrees of freedom
# Multiple R-squared:  0.8982,	Adjusted R-squared:  0.8959 
# F-statistic: 399.2 on 4 and 181 DF,  p-value: < 2.2e-16


# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE TESTES ESTATISTICOS
# ===================================================================================

# ANALISE DOS RESIDUOS 
# ---------------------------------------------

residuos_padronizados <- residuals(model1_pca_2022, type = "pearson")
print(residuos_padronizados)

# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE GRÁFICOS
# ===================================================================================

modelo <- model1_pca_2022

# Obter resíduos, valores ajustados e resíduos padronizados
residuos <- residuals(modelo)
fitted_values <- fitted(modelo)
std_residuos <- rstandard(modelo)
cooks_dist <- cooks.distance(modelo)

# Criar base de dados para os gráficos
dados_grafico <- data.frame(
  Fitted = fitted_values,
  Residuos = residuos,
  StdResiduos = std_residuos,
  CooksDist = cooks_dist,
  Index = 1:length(cooks_dist)
)

# ===================================================================================
# GRÁFICO 1: Resíduos vs. Valores Ajustados
# ===================================================================================
library(ggplot2)
p1 <- ggplot(dados_grafico, aes(x = Fitted, y = Residuos)) +
  geom_point(alpha = 0.7, color = "#FBC02D", shape = 1,
             size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Resíduos vs. Valores Ajustados", x = "Valores Ajustados", y = "Resíduos") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ===================================================================================
# GRÁFICO 2: QQ-Plot dos resíduos
# ===================================================================================
p2 <- ggplot(dados_grafico, aes(sample = Residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Gráfico QQ dos Resíduos", x = "Quantis Teóricos", y = "Quantis Amostrais") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10)) 

# ===================================================================================
# GRÁFICO 3: Histograma dos resíduos padronizados
# ===================================================================================
p3 <- ggplot(dados_grafico, aes(x = StdResiduos)) +
  geom_histogram(bins = 20, fill = "#FBC02D", color = "black", alpha = 0.7) +
  labs(title = "Hist. Resíduos Padronizados", x = "Resíduos Padronizados", y = "Frequência") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ===================================================================================
# GRÁFICO 4: Distância de Cook
# ===================================================================================
limite_influencia <- 4 / length(fitted(modelo))
pontos_influentes <- which(cooks_dist > limite_influencia)

dados_diag <- data.frame(
  Index = 1:length(cooks_dist),
  CooksDist = cooks_dist
)
dados_acima <- dados_diag[pontos_influentes, ]

p4 <- ggplot(dados_diag, aes(x = Index, y = CooksDist)) +
  geom_point(alpha = 0.7, color = "#FBC02D", shape = 1,
             size = 2) +
  geom_point(data = dados_acima, aes(x = Index, y = CooksDist), color = "red", size = 2, shape = 1, size = 2) +
  geom_text_repel(data = dados_acima, aes(label = Index), 
                  size = 3, box.padding = 0.5, point.padding = 0.5, segment.color = 'grey50') +
  labs(title = "Distância de Cook", x = "Observações", y = "Distância de Cook") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ===================================================================================

# Plotar todos os gráficos em uma única página
# Definindo a disposição dos gráficos em uma matriz 2x2
# Combinar os gráficos em uma única figura

library(patchwork)
combined_plot <- p1 + p2 + p3 + p4 + plot_layout(ncol = 2)
print(combined_plot)

ggplot2::ggsave("residuos_modelo_2022.png", combined_plot, width = 10, height = 8)

par(mfrow=c(2,2))
plot(model1_pca_2019)
par(mfrow=c(1,1))

plot(model1_pca_2017)

# ===================================================================================
# PLOTANDO OS PREDICTOS PELOS OBSERVADOS
# ===================================================================================

# Gerar os valores preditos
X_pca_df_2022 <- as.data.frame(X_pca_2022)
Zscore_2022$preditos <- predict(model1_pca_2022, newdata = X_pca_df_2022)

# Ajustar reta entre observados e preditos
modelo_ajuste_2022 <- lm(preditos ~ IEGM_taxa_2021, data = Zscore_2022)
coeficientes <- coef(modelo_ajuste_2022)
intercepto <- coeficientes[1]
inclinacao <- coeficientes[2]
r2_ajustado <- summary(modelo_ajuste_2022)$adj.r.squared

# Criar textos para equação da reta e R² ajustado
equacao_texto <- paste0("y = ", round(inclinacao, 3), "x + ", round(intercepto, 3))
r2_texto <- paste0("R² Ajustado = ", round(r2_ajustado, 3))

# Plotar gráfico com equação e R²
plot.pred_obs_2022 <- ggplot(Zscore_2022, aes(x = IEGM_taxa_2021, y = preditos)) +
  geom_smooth(method = "lm", color = "#FBC02D", se = FALSE) +
  geom_point(size = 2, shape = 1, color = "#FBC02D") +
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
  labs(title = "Modelo II (2022)",
       x = "IEGM (Observado)",
       y = "IEGM (Predito)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot2::ggsave("plot.pred_obs_2022.png", plot.pred_obs_2022, width = 10, height = 8)

print(plot.pred_obs_2022)

#  --------------------------------------------------
# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSÃO PROPRIAMENTE DITA
# ===================================================================================

#  --------------------------------------------------
# OUTPUT --------------------------------------------

summary(model1_pca_2022)$coefficients

#  --------------------------------------------------
#      Estimate Std. Error   t value     Pr(>|t|)
#PC1 -0.2773879 0.01717750 -16.14832 6.533405e-37
#PC2 -0.5624944 0.01757299 -32.00903 1.901748e-76
#PC3  0.2871662 0.01982365  14.48604 4.456561e-32
#PC4 -0.2138834 0.02121614 -10.08116 2.986419e-19
#  --------------------------------------------------

Zscore_2022_1 <- Zscore_2022[, !colnames(Zscore_2022) %in% "preditos"]
Zscore_2022_2 <- Zscore_2022_1[, !colnames(Zscore_2022_1) %in% "IEGM_taxa_2021"]

# CODIGO ABAIXO PARA A CONSTRUÇÃO DA MATRIZ, PARA A ANALISE FINAL
pca_2022_1 <- prcomp(Zscore_2022_2, center = TRUE, scale. = TRUE)
print(pca_2022_1)

# COPIANDO DA AREA DE TRANSFERENCIA O QUE EU ISOLEI
pesos_pca_colada_2022 <- read.table("clipboard", header = T)
print(pesos_pca_colada_2022)

# NÃO REMOVER NEMNHUM PC VISTO QUE OS 4 PRIMEROS DERAM SIGNIFICANTES 

print(pesos_pca_colada_2022)

matriz_pesos_pca_2022 <- as.matrix(pesos_pca_colada_2022)
print(matriz_pesos_pca_2022)

#  --------------------------------------------------
#  --------------------------------------------------

# BETAS 1, 2, 3 E 4, RESPECTIVAMENTE 
betas_2022 <- c(-0.2773879, -0.5624944, 0.2871662, -0.2138834)
resultado <- matriz_pesos_pca_2022[, 1:4] %*% betas_2022

modelo2022_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2022), Resultado = round(resultado, 4))

#  --------------------------------------------------
print(modelo2022_resultado)
#  --------------------------------------------------

#Variavel Resultado
#TAXA_CRIMINALIDADE_2022               TAXA_CRIMINALIDADE_2022    0.0324
#IDHM_2022                                           IDHM_2022   -0.0471
#TAXA_ANALFABETISMO_2022               TAXA_ANALFABETISMO_2022   -0.1448
#IDEB_Municipal_2022                       IDEB_Municipal_2022   -0.0081
#TAXA_MORTALIDADE_INFANTIL_2022 TAXA_MORTALIDADE_INFANTIL_2022   -0.0660
#i-Plan_taxa_2021                             i-Plan_taxa_2021    0.2564
#i-Saúde_taxa_2021                           i-Saúde_taxa_2021    0.3355
#i-GovTI_taxa_2021                           i-GovTI_taxa_2021    0.2266
#i-Fiscal_taxa_2021                         i-Fiscal_taxa_2021    0.2059
#i-Educ_taxa_2021                             i-Educ_taxa_2021    0.3803
#i-Cidade_taxa_2021                         i-Cidade_taxa_2021    0.0727
#i-Amb_taxa_2021                               i-Amb_taxa_2021    0.2397
#TAXA_DESEMPREGO_2010                     TAXA_DESEMPREGO_2010   -0.0841
#TAXA_URBANIZAÇÃO_2010                   TAXA_URBANIZAÇÃO_2010   -0.0772
#GINI_2010                                           GINI_2010    0.0205

# Rodar o teste de Durbin-Watson
dwtest(model1_pca_2022)





