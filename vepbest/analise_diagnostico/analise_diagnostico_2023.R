# ===================================================================================
# SCRIPT::ANALISE DE DIAGNOSTICO DO MODELO DE 2023
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
colnames(Dados_Zscore_2023)

Dados_Zscore_2023 <- Dados_Zscore_2023[, !colnames(Dados_Zscore_2023) %in% "MUNICIPIO"]

Dados_Zscore_2023 <- Dados_Zscore_2023[, !colnames(Dados_Zscore_2023) %in% "DENSIDADE_DEMOGRAFICA_2010"]

Dados_Zscore_2023 <- Dados_Zscore_2023[, !colnames(Dados_Zscore_2023) %in% "OBITOS_INFANTIS_2010"]

Dados_Zscore_2023 <- Dados_Zscore_2023[, !colnames(Dados_Zscore_2023) %in% "GASTO_PUBLICO_2023"]

Dados_Zscore_2023 <- Dados_Zscore_2023[, !colnames(Dados_Zscore_2023) %in% "N_VITIMAS_CVLI_2023"]

Dados_Zscore_2023 <- Dados_Zscore_2023[, !colnames(Dados_Zscore_2023) %in% "NASCIDOS_VIVOS_PE_2010"]

# -----------------------------------------
# TRANTANDO NAs INTRODUZIDOS POR COERÇÃO VIA IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2023 <- Dados_Zscore_2023 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
summary(Dados_Zscore_2023)

# ===================================================================================
# PADRONIZAÇÃO DE DADOS VIA ZSCORE
# ===================================================================================

Zscore_2023 <- Dados_Zscore_2023 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

# ===================================================================================
# ANALISE DE CORRELAÇÃO ENTRE VAR. INDEP.
# ===================================================================================
colnames(Zscore_2023)
SEM_VD <- Zscore_2023[, !colnames(Zscore_2023) %in% "IEGM_taxa_2023"]
corrgram::corrgram(SEM_VD)

# ===================================================================================
# TRATAMENTO DA CORRELAÇÃO POR PCA
# ===================================================================================

pca_2023 <- prcomp(SEM_VD, center = TRUE, scale. = TRUE)
summary(pca_2023)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2023)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

#  --------------------------------------------------
#  --------------------------------------------------
# ISOLANDO VARIAVEL DEPENDENTE
y <- Zscore_2023$IEGM_taxa_2023

# ISOLANDOS VARIAVEIS INDEPENDENTES (X) COM PCA
X_pca_2023 <- pca_2023$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
modelo_data2023 <- as.data.frame(cbind(y, X_pca_2023))

#  --------------------------------------------------
model1_pca_2023 <- lm(modelo_data2023$y ~ -1 + PC1 + PC2 + PC3 + PC4, data = modelo_data2023)
#  --------------------------------------------------
# Adjusted R-squared:  0.8822, PC3 NÃO SIGNIFICANTE 
summary(model1_pca_2023)

# Call:
# lm(formula = modelo_data2023$y ~ -1 + PC1 + PC2 + PC3 + PC4, 
#     data = modelo_data2023)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.81560 -0.21931 -0.02295  0.20210  1.04762 

# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# PC1 -0.17294    0.01696 -10.200   <2e-16 ***
# PC2 -0.63783    0.01957 -32.596   <2e-16 ***
# PC3 -0.05059    0.02125  -2.381   0.0183 *  
# PC4 -0.24675    0.02280 -10.824   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.3537 on 181 degrees of freedom
# Multiple R-squared:  0.8769,	Adjusted R-squared:  0.8742 
# F-statistic: 322.3 on 4 and 181 DF,  p-value: < 2.2e-16

# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE TESTES ESTATISTICOS
# ===================================================================================

# ANALISE DOS RESIDUOS 
# ---------------------------------------------

residuos_padronizados <- residuals(model1_pca_2023, type = "pearson")
print(residuos_padronizados)

# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE GRÁFICOS
# ===================================================================================

modelo <- model1_pca_2023
library(ggrepel)

modelo <- model1_pca_2023

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
  geom_point(alpha = 0.7, color = "#B22222", shape = 1,
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
  geom_histogram(bins = 20, fill = "#B22222", color = "black", alpha = 0.7) +
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
  geom_point(alpha = 0.7, color = "#B22222", shape = 1,
             size = 2) +
  geom_point(data = dados_acima, aes(x = Index, y = CooksDist), color = "red", size = 2, shape = 11, size = 2) +
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

ggplot2::ggsave("residuos_modelo_2019.png", combined_plot, width = 10, height = 8)

par(mfrow=c(2,2))
plot(model1_pca_2019)
par(mfrow=c(1,1))

plot(model1_pca_2017)

# ===================================================================================
# PLOTANDO OS PREDICTOS PELOS OBSERVADOS
# ===================================================================================

# Gerar os valores preditos
X_pca_df_2023 <- as.data.frame(X_pca_2023)
Zscore_2023$preditos <- predict(model1_pca_2023, newdata = X_pca_df_2023)

# Ajustar reta entre observados e preditos
modelo_ajuste_2023 <- lm(preditos ~ IEGM_taxa_2023, data = Zscore_2023)
coeficientes <- coef(modelo_ajuste_2023)
intercepto <- coeficientes[1]
inclinacao <- coeficientes[2]
r2_ajustado <- summary(modelo_ajuste_2023)$adj.r.squared

# Criar textos para equação da reta e R² ajustado
equacao_texto <- paste0("y = ", round(inclinacao, 3), "x + ", round(intercepto, 3))
r2_texto <- paste0("R² Ajustado = ", round(r2_ajustado, 3))

# Plotar gráfico com equação e R²
plot.pred_obs_2023 <- ggplot(Zscore_2023, aes(x = IEGM_taxa_2023, y = preditos)) +
  geom_smooth(method = "lm", color = "#B22222", se = FALSE) +
  geom_point(size = 2, shape = 1, color = "#B22222") +
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
  labs(title = "Modelo IV (2023)",
       x = "IEGM (Observado)",
       y = "IEGM (Predito)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot2::ggsave("plot.pred_obs_2023(1).png", plot.pred_obs_2019, width = 10, height = 8)

print(plot.pred_obs_2023)


library(patchwork)

combined_plot4 <- plot.pred_obs_2017+ plot.pred_obs_2019 + plot.pred_obs_2022 + plot.pred_obs_2023 + plot_layout(ncol = 2)
print(combined_plot4)

# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSÃO PROPRIAMENTE DITA
# ===================================================================================

#  --------------------------------------------------
# OUTPUT --------------------------------------------

summary(model1_pca_2023)$coefficients

#  --------------------------------------------------
#      Estimate Std. Error    t value     Pr(>|t|)
#PC1 -0.17294326 0.01695535 -10.199923 1.375537e-19
#PC2 -0.63783444 0.01956782 -32.596087 1.149660e-77
#PC3 -0.05059349 0.02124703  -2.381203 1.829408e-02
#PC4 -0.24675189 0.02279697 -10.823889 2.261735e-21
#  --------------------------------------------------

Zscore_2023_1 <- Zscore_2023[, !colnames(Zscore_2023) %in% "preditos"]
Zscore_2023_2 <- Zscore_2023_1[, !colnames(Zscore_2023_1) %in% "IEGM_taxa_2023"]

# CODIGO ABAIXO PARA A CONSTRUÇÃO DA MATRIZ, PARA A ANALISE FINAL
pca_2023_1 <- prcomp(Zscore_2023_2, center = TRUE, scale. = TRUE)
print(pca_2023_1)

# COPIANDO DA AREA DE TRANSFERENCIA O QUE EU ISOLEI
pesos_pca_colada_2023 <- read.table("clipboard", header = T)
print(pesos_pca_colada_2023)

# NÃO REMOVER NEMNHUM PC VISTO QUE OS 4 PRIMEROS DERAM SIGNIFICANTES 

print(pesos_pca_colada_2023)

matriz_pesos_pca_2023 <- as.matrix(pesos_pca_colada_2023)
print(matriz_pesos_pca_2023)

#  --------------------------------------------------
#  --------------------------------------------------

# BETAS 1, 2, 3 E 4, RESPECTIVAMENTE 
betas_2023 <- c(-0.17294326, -0.63783444, -0.05059349, -0.24675189)
resultado <- matriz_pesos_pca_2023[, 1:4] %*% betas_2023

modelo2023_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2023), Resultado = round(resultado, 4))

#  --------------------------------------------------
print(modelo2023_resultado)
#  --------------------------------------------------

#Variavel Resultado
#i-Plan_taxa_2023                             i-Plan_taxa_2023    0.2726
#i-Saúde_taxa_2023                           i-Saúde_taxa_2023    0.1221
#i-GovTI_taxa_2023                           i-GovTI_taxa_2023    0.2897
#i-Fiscal_taxa_2023                         i-Fiscal_taxa_2023    0.3095
#i-Educ_taxa_2023                             i-Educ_taxa_2023    0.2616
#i-Cidade_taxa_2023                         i-Cidade_taxa_2023    0.2363
#i-Amb_taxa_2023                               i-Amb_taxa_2023    0.2248
#TAXA_CRIMINALIDADE_2023               TAXA_CRIMINALIDADE_2023   -0.1652
#IDHM_PE_2010                                     IDHM_PE_2010   -0.0218
#GINI_2010                                           GINI_2010    0.0664
#TAXA_ANALFABETISMO_2010               TAXA_ANALFABETISMO_2010    0.0161
#TAXA_DESEMPREGO_2010                     TAXA_DESEMPREGO_2010    0.1067
#TAXA_MORTALIDADE_INFANTIL_2010 TAXA_MORTALIDADE_INFANTIL_2010    0.0320
#TAXA_URBANIZAÇÃO_2010                   TAXA_URBANIZAÇÃO_2010    0.1074
#IDEB_Municipal_2022                       IDEB_Municipal_2022    0.0053


# Rodar o teste de Durbin-Watson
dwtest(model1_pca_2023)

