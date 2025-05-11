# ===================================================================================
# SCRIPT::MODELO DE 2019
# ===================================================================================

# ENTRE INDICADORES E TRANSVERSALIDADE: UMA ANÁLISE EM PERSPECTIVA DA EFETIVIDADE DA GESTÃO MUNICIPAL COMO EXPRESSÃO DE INDICADORES SOCIOECONÔMICOS E ADMINISTRAAATIVOS EM PERNAMBUCO

### PESQUISA QUANTITATIVA REALIZADA NO DEPARTAMENTO DE CIÊNCIA POLÍTICA (CPRI) DA UNIVERSIDADE FEDERAL DE PERNAMBUCO A PARTIR DE UMA ANÁLISE DE REGRESSÃO LINEAR MÚLTIPLA 

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

# CARREGANDO... 
Dados_Zscore_2019 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2019.xlsx")

# CARREGAR TODAS AS BIBLIOTECAS DE UMA VEZ
if(!require(pacman))
  install.packages("pacman")  
library(pacman)

pacman::p_load(corrplot, readxl, tidyverse, corrgram, GGally, openxlsx)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, scatterplot3d)

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

# REMOVENDO INUTILIDADES (COLUNAS) PARA A PADRONIZAÇÃO  DO ZSCORE
colnames(Dados_Zscore_2019)

Dados_Zscore_2019 <- Dados_Zscore_2019[, !colnames(Dados_Zscore_2019) %in% "MUNICIPIO"]
Dados_Zscore_2019 <- Dados_Zscore_2019[, !colnames(Dados_Zscore_2019) %in% "DENSIDADE_DEMOGRAFICA_2010"]
Dados_Zscore_2019 <- Dados_Zscore_2019[, !colnames(Dados_Zscore_2019) %in% "OBITOS_INFANTIS_2010"]
Dados_Zscore_2019 <- Dados_Zscore_2019[, !colnames(Dados_Zscore_2019) %in% "GASTO_PUBLICO_2019"]
Dados_Zscore_2019 <- Dados_Zscore_2019[, !colnames(Dados_Zscore_2019) %in% "NASCIDOS_VIVOS_PE_2010"]
Dados_Zscore_2019 <- Dados_Zscore_2019[, !colnames(Dados_Zscore_2019) %in% "N_VITIMAS_CVLI_2019"]

# TRATANDO NAs POR IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2019 <- Dados_Zscore_2019 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# ===================================================================================
# PADRONIZANDO EM ZSCORE
# ===================================================================================

Zscore_2019 <- Dados_Zscore_2019 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))
glimpse(Zscore_2019)

# ===================================================================================
# ANALISE DE CORRELAÇÃO ENTRE VAR. INDEP.
# ===================================================================================
colnames(Zscore_2019)
SEM_VD <- Zscore_2019[, !colnames(Zscore_2019) %in% "IEGM_Taxa_2019"]
corrgram::corrgram(SEM_VD)

# ===================================================================================
# TRATAMENTO DA CORRELAÇÃO POR PCA
# ===================================================================================

pca_2019 <- prcomp(SEM_VD, center = TRUE, scale. = TRUE)
summary(pca_2019)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2019)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

#  --------------------------------------------------
#  --------------------------------------------------
# ISOLANDO VARIAVEL DEPENDENTE
y <- Zscore_2019$IEGM_Taxa_2019

# ISOLANDOS VARIAVEIS INDEPENDENTES (X) COM PCA
X_pca_2019 <- pca_2019$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
modelo_data2019 <- as.data.frame(cbind(y, X_pca_2019))

#  --------------------------------------------------
model1_pca_2019 <- lm(modelo_data2019$y ~ -1 + PC1 + PC2 + PC4, data = modelo_data2019)
#  --------------------------------------------------
# Adjusted R-squared:  0.8822, PC3 NÃO SIGNIFICANTE 
summary(model1_pca_2019)

# Call:
# lm(formula = modelo_data2019$y ~ -1 + PC1 + PC2 + PC4, data = modelo_data2019)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.99129 -0.18407  0.01523  0.21982  1.03602 

# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# PC1 -0.25676    0.01626  -15.79   <2e-16 ***
# PC2  0.56325    0.01871   30.11   <2e-16 ***
# PC4 -0.34974    0.02272  -15.39   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.3418 on 182 degrees of freedom
# Multiple R-squared:  0.8844,	Adjusted R-squared:  0.8825 
# F-statistic: 464.3 on 3 and 182 DF,  p-value: < 2.2e-16


# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE TESTES ESTATISTICOS
# ===================================================================================

# ANALISE DOS RESIDUOS 
# ---------------------------------------------

residuos_padronizados <- residuals(model1_pca_2019, type = "pearson")
print(residuos_padronizados)

# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE GRÁFICOS
# ===================================================================================

modelo2 <- model1_pca_2019
library(ggrepel)

# Obter resíduos, valores ajustados e resíduos padronizados
residuos2 <- residuals(modelo2)
fitted_values2 <- fitted(modelo2)
std_residuos2 <- rstandard(modelo2)
cooks_dist2 <- cooks.distance(modelo2)

# Criar base de dados para os gráficos
dados_grafico2 <- data.frame(
  Fitted = fitted_values2,
  Residuos = residuos2,
  StdResiduos = std_residuos2,
  CooksDist = cooks_dist2,
  Index = 1:length(cooks_dist2)
)

# AGORA DEU CERTO
# ===================================================================================
# GRÁFICO 1: Resíduos vs. Valores Ajustados
# ===================================================================================
library(ggplot2)
p1.2 <- ggplot(dados_grafico2, aes(x = Fitted, y = Residuos)) +
  geom_point(alpha = 0.7, color = "blue", shape = 1,
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
p2.2 <- ggplot(dados_grafico, aes(sample = Residuos)) +
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
p3.2 <- ggplot(dados_grafico, aes(x = StdResiduos)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Hist. Resíduos Padronizados", x = "Resíduos Padronizados", y = "Frequência") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ===================================================================================
# GRÁFICO 4: Distância de Cook
# ===================================================================================
limite_influencia2 <- 4 / length(fitted(modelo2))
pontos_influentes2 <- which(cooks_dist2 > limite_influencia2)

dados_diag2 <- data.frame(
  Index = 1:length(cooks_dist2),
  CooksDist = cooks_dist2
)
dados_acima2 <- dados_diag2[pontos_influentes2, ]

p4.2 <- ggplot(dados_diag2, aes(x = Index, y = CooksDist)) +
  geom_point(alpha = 0.7, color = "blue", shape = 1,
             size = 2) +
  geom_point(data = dados_acima2, aes(x = Index, y = CooksDist), color = "red", size = 2, shape = 1, size = 2) +
  geom_text_repel(data = dados_acima2, aes(label = Index), 
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
combined_plot.2 <- p1.2 + p2.2 + p3.2 + p4.2 + plot_layout(ncol = 2)
print(combined_plot.2)

ggplot2::ggsave("residuos_modelo_2019.png", combined_plot, width = 10, height = 8)

par(mfrow=c(2,2))
plot(model1_pca_2019)
par(mfrow=c(1,1))

plot(model1_pca_2017)

# ===================================================================================
# PLOTANDO OS PREDICTOS PELOS OBSERVADOS
# ===================================================================================

# Gerar os valores preditos
X_pca_df_2019 <- as.data.frame(X_pca_2019)
Zscore_2019$preditos <- predict(model1_pca_2019, newdata = X_pca_df_2019)

# Ajustar reta entre observados e preditos
modelo_ajuste_2019 <- lm(preditos ~ IEGM_Taxa_2019, data = Zscore_2019)
coeficientes <- coef(modelo_ajuste_2019)
intercepto <- coeficientes[1]
inclinacao <- coeficientes[2]
r2_ajustado <- summary(modelo_ajuste_2019)$adj.r.squared

# Criar textos para equação da reta e R² ajustado
equacao_texto <- paste0("y = ", round(inclinacao, 3), "x + ", round(intercepto, 3))
r2_texto <- paste0("R² Ajustado = ", round(r2_ajustado, 3))

# Plotar gráfico com equação e R²
plot.pred_obs_2019 <- ggplot(Zscore_2019, aes(x = IEGM_Taxa_2019, y = preditos)) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  geom_point(size = 2, shape = 1, color = "blue") +
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
  labs(title = "Modelo II (2019)",
       x = "IEGM (Observado)",
       y = "IEGM (Predito)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot2::ggsave("plot.pred_obs_2019(1).png", plot.pred_obs_2019, width = 10, height = 8)

print(plot.pred_obs_2019)

#  --------------------------------------------------
# ===================================================================================
# SCRIPT::ANALISE DO MODELO DE REGRESSÃO PROPRIAMENTE DITA
# ===================================================================================

#  --------------------------------------------------
# OUTPUT --------------------------------------------

summary(model1_pca_2019)$coefficients

#  --------------------------------------------------
#       Estimate Std. Error   t value     Pr(>|t|)
# PC1 -0.2567638 0.01626141 -15.78977 6.146969e-36
# PC2  0.5632547 0.01870593  30.11102 1.316332e-72
# PC4 -0.3497427 0.02271931 -15.39407 8.717731e-35
#  --------------------------------------------------

Zscore_2019_1 <- Zscore_2019[, !colnames(Zscore_2019) %in% "preditos"]
Zscore_2019_2 <- Zscore_2019_1[, !colnames(Zscore_2019_1) %in% "IEGM_Taxa_2019"]

# CODIGO ABAIXO PARA A CONSTRUÇÃO DA MATRIZ, PARA A ANALISE FINAL
pca_2019_1 <- prcomp(Zscore_2019_2, center = TRUE, scale. = TRUE)
print(pca_2019_1)

# COPIANDO DA AREA DE TRANSFERENCIA O QUE EU ISOLEI
pesos_pca_colada_2019 <- read.table("clipboard", header = T)
print(pesos_pca_colada_2019)


colnames(pesos_pca_colada_2019)
pesos_pca_colada_2019 <- pesos_pca_colada_2019[, !colnames(pesos_pca_colada_2019) %in% "PC3"]

print(pesos_pca_colada_2019)

matriz_pesos_pca_2019 <- as.matrix(pesos_pca_colada_2019)
print(matriz_pesos_pca_2019)

#  --------------------------------------------------
#  --------------------------------------------------

# BETAS 1, 2 E 4, RESPECTIVAMENTE 
betas_2019 <- c(-0.2567638, 0.5632547, -0.3497427)
resultado <- matriz_pesos_pca_2019[, 1:3] %*% betas_2019

modelo2019_resultado <- data.frame(Variavel = rownames(matriz_pesos_pca_2019), Resultado = round(resultado, 4))

#  --------------------------------------------------
print(modelo2019_resultado)
#  --------------------------------------------------

#Variavel Resultado
#i.Plan_taxa_2019                             i.Plan_taxa_2019    0.3131
#i.Saúde_taxa_2019                           i.Saúde_taxa_2019    0.2254
#i.GovTI_taxa_2019'                         i.GovTI_taxa_2019'    0.2103
#i.Fiscal_taxa_2019                         i.Fiscal_taxa_2019    0.2782
#i.Educ_taxa_2019                             i.Educ_taxa_2019    0.3314
#i.Cidade_taxa_2019                         i.Cidade_taxa_2019    0.0399
#i.Amb_taxa_2019                               i.Amb_taxa_2019    0.2639
#TAXA_CRIMINALIDADE_2019               TAXA_CRIMINALIDADE_2019   -0.1325
#IDHM_PE_2010                                     IDHM_PE_2010   -0.0239
#GINI_2010                                           GINI_2010   -0.1173
#TAXA_ANALFABETISMO_2010               TAXA_ANALFABETISMO_2010    0.0054
#TAXA_DESEMPREGO_2010                     TAXA_DESEMPREGO_2010    0.1041
#TAXA_MORTALIDADE_INFANTIL_2010 TAXA_MORTALIDADE_INFANTIL_2010    0.0886
#TAXA_URBANIZAÇÃO_2010                   TAXA_URBANIZAÇÃO_2010    0.0098
#IDEB_PE_2010                                     IDEB_PE_2010    0.0575

# Suponha que seu modelo se chama modelo_ajustado
# Instale o pacote, se necessário
install.packages("lmtest")  # só precisa rodar uma vez
library(lmtest)

# Rodar o teste de Durbin-Watson
dwtest(model1_pca_2019)
