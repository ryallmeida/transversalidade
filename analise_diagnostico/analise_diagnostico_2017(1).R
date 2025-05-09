# ===================================================================================
# SCRIPT::MODELO DE 2017
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


pacman::p_load(corrplot, readxl, tidyverse, corrgram, GGally, openxlsx)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, scatterplot3d)


# CARREGANDO... 
Dados_Zscore_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2017.xlsx")

Dados_Zscore_2017$Obitos_Infantis_2010 <- as.integer(Dados_Zscore_2017$Obitos_Infantis_2010)

Dados_Zscore_2017$Taxa_Mortalidade_Infantil_2010 <- as.double(Dados_Zscore_2017$Taxa_Mortalidade_Infantil_2010)

Dados_Zscore_2017$GASTO_PUBLICO_2017 <- as.double(Dados_Zscore_2017$GASTO_PUBLICO_2017)

# TRATANDO NAs POR IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2017 <- Dados_Zscore_2017 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

colnames(Dados_Zscore_2017)
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Município" ]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Exercício" ]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Tribuntal" ]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Numero_CVLI_2017"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Nascidos_Vivos_2010"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Nascidos_Vivos_2010"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Tribuntal"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Densidade_Demografica_2010"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Obitos_Infantis_2010"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "GASTO_PUBLICO_2017"]

# -----------------------------------------
# TRANTANDO NAs INTRODUZIDOS POR COERÇÃO VIA IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2017 <- Dados_Zscore_2017 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
summary(Dados_Zscore_2023)

# ===================================================================================
# PADRONIZAÇÃO DE DADOS VIA ZSCORE
# ===================================================================================

Zscore_2017 <- Dados_Zscore_2017 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

# ===================================================================================
# ANALISE DE CORRELAÇÃO ENTRE VAR. INDEP.
# ===================================================================================
colnames(Zscore_2017)
SEM_VD1 <- Zscore_2017[, !colnames(Zscore_2017) %in% "IEGM_taxa"]
corrgram::corrgram(SEM_VD1)

# ===================================================================================
# TRATAMENTO DA CORRELAÇÃO POR PCA
# ===================================================================================

pca_2017 <- prcomp(SEM_VD1, center = TRUE, scale. = TRUE)
summary(pca_2017)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance1 <- summary(pca_2017)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components1 <- which(cumsum(explained_variance1) >= 0.95)[1]
print(num_components)

#  --------------------------------------------------
#  --------------------------------------------------
# ISOLANDO VARIAVEL DEPENDENTE
y1 <- Zscore_2017$IEGM_taxa

# ISOLANDOS VARIAVEIS INDEPENDENTES (X) COM PCA
X_pca_2017 <- pca_2017$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
modelo_data2017 <- as.data.frame(cbind(y1, X_pca_2017))

#  --------------------------------------------------
model1_pca_2017 <- lm(modelo_data2017$y1 ~ -1 + PC1 + PC2 + PC3 , data = modelo_data2017)
#  --------------------------------------------------
# Adjusted R-squared:  Adjusted R-squared:  0.6882 , PC4 NÃO SIGNIFICANTE 
summary(model1_pca_2017)

# Call:
# lm(formula = modelo_data2017$y1 ~ -1 + PC1 + PC2 + PC3, data = modelo_data2017)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.72487 -0.37046  0.00494  0.34495  2.42108 

# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# PC1 -0.22503    0.02647  -8.502 6.64e-15 ***
# PC2  0.58249    0.03184  18.297  < 2e-16 ***
# PC3  0.06976    0.03366   2.073   0.0396 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.5569 on 182 degrees of freedom
# Multiple R-squared:  0.6933,	Adjusted R-squared:  0.6882 
# F-statistic: 137.1 on 3 and 182 DF,  p-value: < 2.2e-16



# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE TESTES ESTATISTICOS
# ===================================================================================

# ANALISE DOS RESIDUOS 
# ---------------------------------------------

residuos_padronizados1 <- residuals(model1_pca_2017, type = "pearson")
print(residuos_padronizados1)

# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE GRÁFICOS
# ===================================================================================

modelo1 <- model1_pca_2017
library(ggrepel)

# Obter resíduos, valores ajustados e resíduos padronizados
residuos1 <- residuals(modelo1)
fitted_values1 <- fitted(modelo1)
std_residuos1 <- rstandard(modelo1)
cooks_dist1 <- cooks.distance(modelo1)

# Criar base de dados para os gráficos
dados_grafico1 <- data.frame(
  Fitted = fitted_values1,
  Residuos = residuos1,
  StdResiduos = std_residuos1,
  CooksDist = cooks_dist1,
  Index = 1:length(cooks_dist1)
)

# ===================================================================================
# GRÁFICO 1: Resíduos vs. Valores Ajustados
# ===================================================================================
library(ggplot2)
p1.1 <- ggplot(dados_grafico1, aes(x = Fitted, y = Residuos)) +
  geom_point(alpha = 0.7, color = "mediumpurple", shape = 1, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Resíduos vs. Valores Ajustados", x = "Valores Ajustados", y = "Resíduos") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ===================================================================================
# GRÁFICO 2: QQ-Plot dos resíduos
# ===================================================================================
p2.1 <- ggplot(dados_grafico1, aes(sample = Residuos)) +
  stat_qq(color = "#228B22") +
  stat_qq_line(color = "red") +
  labs(title = "Gráfico QQ dos Resíduos", x = "Quantis Teóricos", y = "Quantis Amostrais") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10)) 

# ===================================================================================
# GRÁFICO 3: Histograma dos resíduos padronizados
# ===================================================================================
p3.1 <- ggplot(dados_grafico1, aes(x = StdResiduos)) +
  geom_histogram(bins = 20, fill = "mediumpurple", color = "black", alpha = 0.7) +
  labs(title = "Hist. Resíduos Padronizados", x = "Resíduos Padronizados", y = "Frequência") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ===================================================================================
# GRÁFICO 4: Distância de Cook
# ===================================================================================
limite_influencia1 <- 4 / length(fitted_values1)
pontos_influentes1 <- which(cooks_dist1 > limite_influencia)

dados_diag1 <- data.frame(
  Index = 1:length(cooks_dist1),
  CooksDist = cooks_dist1
)

dados_acima1 <- dados_diag1[pontos_influentes, ]

p4.1 <- ggplot(dados_diag1, aes(x = Index, y = CooksDist)) +
  geom_point(alpha = 0.7, color = "mediumpurple", shape = 1, size = 2) +
  geom_point(data = dados_acima, aes(x = Index, y = CooksDist), color = "red", shape = 8, size = 2) +
  geom_text_repel(data = dados_acima, aes(label = Index), 
                  size = 3, box.padding = 0.5, point.padding = 0.5, segment.color = 'grey50') +
  labs(title = "Distância de Cook", x = "Observações", y = "Distância de Cook") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ===================================================================================
# PLOTAGEM DOS GRÁFICOS DIAGNÓSTICOS
# ===================================================================================
library(patchwork)
combined_plot1 <- p1.1 + p2.1 + p3.1 + p4.1 + plot_layout(ncol = 2)
print(combined_plot1)

ggplot2::ggsave("residuos_modelo_2017.png", combined_plot, width = 10, height = 8)

# ===================================================================================
# PLOTAGEM DOS GRÁFICOS DIAGNÓSTICOS
# ===================================================================================

# Gerar os valores preditos para o modelo de 2017
X_pca_df_2017 <- as.data.frame(X_pca_2017)
Zscore_2017$preditos <- predict(model1_pca_2017, newdata = X_pca_df_2017)

# Ajustar reta entre observados e preditos
modelo_ajuste_2017 <- lm(preditos ~ IEGM_taxa, data = Zscore_2017)
coeficientes_2017 <- coef(modelo_ajuste_2017)
intercepto_2017 <- coeficientes_2017[1]
inclinacao_2017 <- coeficientes_2017[2]
r2_ajustado_2017 <- summary(modelo_ajuste_2017)$adj.r.squared

# Criar textos para equação da reta e R² ajustado
equacao_texto_2017 <- paste0("y = ", round(inclinacao_2017, 3), "x + ", round(intercepto_2017, 3))
r2_texto_2017 <- paste0("R² Ajustado = ", round(r2_ajustado_2017, 3))

# Plotar gráfico com equação e R²
plot.pred_obs_2017 <- ggplot(Zscore_2017, aes(x = IEGM_taxa, y = preditos)) +
  geom_smooth(method = "lm", color = "mediumpurple", se = FALSE) +
  geom_point(size = 2, shape = 1, color = "mediumpurple") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  annotate("text",
           x = min(Zscore_2017$IEGM_taxa),
           y = max(Zscore_2017$preditos),
           label = equacao_texto_2017,
           hjust = 0, size = 4, color = "black") +
  annotate("text",
           x = min(Zscore_2017$IEGM_taxa),
           y = max(Zscore_2017$preditos) * 0.95,
           label = r2_texto_2017,
           hjust = 0, size = 4, color = "black") +
  labs(title = "Modelo I (2017)",
       x = "IEGM (Observado)",
       y = "IEGM (Predito)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Salvar e exibir o gráfico
ggplot2::ggsave("plot.pred_obs_2017.png", plot.pred_obs_2017, width = 10, height = 8)

print(plot.pred_obs_2017)










