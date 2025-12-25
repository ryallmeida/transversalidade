# ===================================================================================
# SCRIPT::PARA CRIAÇÃO DOS GRAFICOS
# ===================================================================================

# ANALISE DOS RESIDUOS 
# ---------------------------------------------

residuos_padronizados <- residuals(model1_pca_2017, type = "pearson")
print(residuos_padronizados)

# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE GRÁFICOS
# ===================================================================================

modelo <- model1_pca_2017
library(ggrepel)

#OBTER RESIUDOS E VALORES AJUSTADOS
residuos <- residuals(modelo)
fitted_values <- fitted(modelo)
std_residuos <- residuos_padronizados
cooks_dist <- cooks.distance(modelo)

# CRIA UMA BASE DE DADOS PARA OS GRÁFICOS, COM AS INFORMAÇÕES OBTIDAS A PARTIR DO MODELO ESTIMADO

dados_grafico <- data.frame(
  Fitted = fitted_values,
  Residuos = residuos,
  StdResiduos = std_residuos,
  CooksDist = cooks_dist,
  Index = 1:length(cooks_dist)
)

# TÁ DANDO ERRO O CODIGO ACIMA

modelo <- model2_pca_2017

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

# AGORA DEU CERTO
# ===================================================================================
# GRÁFICO 1: Resíduos vs. Valores Ajustados
# ===================================================================================
library(ggplot2)
p1 <- ggplot(dados_grafico, aes(x = Fitted, y = Residuos)) +
  geom_point(alpha = 0.7, color = "blue") +
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
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
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
  geom_point(alpha = 0.7, color = "blue") +
  geom_point(data = dados_acima, aes(x = Index, y = CooksDist), color = "red", size = 2) +
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

par(mfrow=c(2,2))
plot(model1_pca_2017)
par(mfrow=c(1,1))

plot(model1_pca_2017)
