# Modelo 2022
modelo_2022 <- model1_pca_2022
residuos_2022 <- residuals(modelo_2022)
fitted_values_2022 <- fitted(modelo_2022)
std_residuos_2022 <- rstandard(modelo_2022)
cooks_dist_2022 <- cooks.distance(modelo_2022)

dados_grafico_2022 <- data.frame(
  Fitted = fitted_values_2022,
  Residuos = residuos_2022,
  StdResiduos = std_residuos_2022,
  CooksDist = cooks_dist_2022,
  Index = 1:length(cooks_dist_2022),
  Ano = "2022"
)

# Modelo 2023
modelo_2023 <- model1_pca_2023
residuos_2023 <- residuals(modelo_2023)
fitted_values_2023 <- fitted(modelo_2023)
std_residuos_2023 <- rstandard(modelo_2023)
cooks_dist_2023 <- cooks.distance(modelo_2023)

dados_grafico_2023 <- data.frame(
  Fitted = fitted_values_2023,
  Residuos = residuos_2023,
  StdResiduos = std_residuos_2023,
  CooksDist = cooks_dist_2023,
  Index = 1:length(cooks_dist_2023),
  Ano = "2023"
)

# Unir os dois data.frames
dados_grafico_todos <- rbind(dados_grafico_2022, dados_grafico_2023)


# Gráfico unificado para os dois anos
library(ggplot2)

p1_unificado <- ggplot(dados_grafico_todos, aes(x = Fitted, y = Residuos, color = Ano)) +
  geom_point(alpha = 0.7, shape = 1, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_color_manual(values = c("2022" = "#FBC02D", "2023" = "#B22222")) +
  labs(title = "Resíduos vs. Valores Ajustados (2022 x 2023)",
       x = "Valores Ajustados",
       y = "Resíduos",
       color = "Ano") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

p1_unificado

library(ggplot2)

p2_unificado <- ggplot(dados_grafico_todos, aes(sample = Residuos, color = Ano)) +
  stat_qq(shape = 1, alpha = 0.7) +
  stat_qq_line(aes(group = Ano), color = "red") +
  scale_color_manual(values = c("2022" = "#FBC02D", "2023" = "#B22222")) +
  labs(title = "Gráfico QQ dos Resíduos (2022 x 2023)",
       x = "Quantis Teóricos",
       y = "Quantis Amostrais",
       color = "Ano") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

p2_unificado

library(ggplot2)

p3_unificado <- ggplot(dados_grafico_todos, aes(x = StdResiduos, fill = Ano)) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity", color = "black") +
  scale_fill_manual(values = c("2022" = "#FBC02D", "2023" = "#B22222")) +
  labs(title = "Histograma dos Resíduos Padronizados (2022 x 2023)",
       x = "Resíduos Padronizados",
       y = "Frequência",
       fill = "Ano") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

p3_unificado

library(ggplot2)
library(ggrepel)
library(dplyr)

# Modelo 2022
modelo_2022 <- model1_pca_2022
cooks_2022 <- cooks.distance(modelo_2022)
fitted_2022 <- fitted(modelo_2022)
limite_2022 <- 4 / length(fitted_2022)
infl_2022 <- which(cooks_2022 > limite_2022)

dados_diag_2022 <- data.frame(
  Index = 1:length(cooks_2022),
  CooksDist = cooks_2022,
  Ano = "2022"
)
dados_acima_2022 <- dados_diag_2022[infl_2022, ]

# Modelo 2023
modelo_2023 <- model1_pca_2023
cooks_2023 <- cooks.distance(modelo_2023)
fitted_2023 <- fitted(modelo_2023)
limite_2023 <- 4 / length(fitted_2023)
infl_2023 <- which(cooks_2023 > limite_2023)

dados_diag_2023 <- data.frame(
  Index = 1:length(cooks_2023),
  CooksDist = cooks_2023,
  Ano = "2023"
)
dados_acima_2023 <- dados_diag_2023[infl_2023, ]

# Unir os dados
dados_diag_todos <- bind_rows(dados_diag_2022, dados_diag_2023)
dados_acima_todos <- bind_rows(dados_acima_2022, dados_acima_2023)

# Gráfico unificado
p4_unificado <- ggplot(dados_diag_todos, aes(x = Index, y = CooksDist, color = Ano)) +
  geom_point(alpha = 0.7, shape = 1, size = 2) +
  geom_point(data = dados_acima_todos, aes(x = Index, y = CooksDist), 
             shape = 8, size = 2) +
  geom_text_repel(data = dados_acima_todos, aes(label = Index), 
                  size = 3, box.padding = 0.5, point.padding = 0.5, 
                  segment.color = 'grey50') +
  labs(title = "Distância de Cook (2022 x 2023)",
       x = "Observações", y = "Distância de Cook", color = "Ano") +
  scale_color_manual(values = c("2022" = "#FBC02D", "2023" = "#B22222")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

p4_unificado



library(patchwork)
combined_plot2 <- p1_unificado + p2_unificado + p3_unificado + p4_unificado + plot_layout(ncol = 2)
print(combined_plot2)

ggplot2::ggsave("COMPARACAO_22x23(1).png", combined_plot2, width = 10, height = 8)

library(ggplot2)

# 1. Gerar os valores preditos
X_pca_df_2022 <- as.data.frame(X_pca_2022)
Zscore_2022$preditos <- predict(model1_pca_2022, newdata = X_pca_df_2022)
Zscore_2022$Ano <- "2022"
Zscore_2022$IEGM_Obs <- Zscore_2022$IEGM_taxa_2021

X_pca_df_2023 <- as.data.frame(X_pca_2023)
Zscore_2023$preditos <- predict(model1_pca_2023, newdata = X_pca_df_2023)
Zscore_2023$Ano <- "2023"
Zscore_2023$IEGM_Obs <- Zscore_2023$IEGM_taxa_2023

# 2. Unir os dataframes
dados_comparativos_2 <- rbind(Zscore_2022[, c("IEGM_Obs", "preditos", "Ano")],
                              Zscore_2023[, c("IEGM_Obs", "preditos", "Ano")])

# 3. Ajustar modelos
modelo_ajuste_2022 <- lm(preditos ~ IEGM_Obs, data = subset(dados_comparativos_2, Ano == "2022"))
modelo_ajuste_2023 <- lm(preditos ~ IEGM_Obs, data = subset(dados_comparativos_2, Ano == "2023"))

# 4. Equações e R²
coef_2022 <- coef(modelo_ajuste_2022)
r2_2022 <- summary(modelo_ajuste_2022)$adj.r.squared
texto_eq_2022 <- paste0("2022: y = ", round(coef_2022[2], 3), "x + ", round(coef_2022[1], 3),
                        "\nR² = ", round(r2_2022, 3))

coef_2023 <- coef(modelo_ajuste_2023)
r2_2023 <- summary(modelo_ajuste_2023)$adj.r.squared
texto_eq_2023 <- paste0("2023: y = ", round(coef_2023[2], 3), "x + ", round(coef_2023[1], 3),
                        "\nR² = ", round(r2_2023, 3))

# 5. Gráfico sobreposto
grafico_sobreposto_2 <- ggplot(dados_comparativos_2, aes(x = IEGM_Obs, y = preditos)) +
  geom_point(data = subset(dados_comparativos_2, Ano == "2022"),
             color = "#FBC02D", size = 2, shape = 1) +
  geom_smooth(data = subset(dados_comparativos_2, Ano == "2022"),
              method = "lm", se = FALSE, color = "#FBC02D") +
  geom_point(data = subset(dados_comparativos_2, Ano == "2023"),
             color = "#B22222", size = 2, shape = 1) +
  geom_smooth(data = subset(dados_comparativos_2, Ano == "2023"),
              method = "lm", se = FALSE, color = "#B22222") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  annotate("text",
           x = min(dados_comparativos_2$IEGM_Obs),
           y = max(dados_comparativos_2$preditos),
           label = texto_eq_2022,
           hjust = 0, size = 4, color = "#FBC02D") +
  annotate("text",
           x = min(dados_comparativos_2$IEGM_Obs),
           y = max(dados_comparativos_2$preditos) * 0.85,
           label = texto_eq_2023,
           hjust = 0, size = 4, color = "#B22222") +
  labs(title = "Modelos 2022 e 2023 sobrepostos",
       x = "IEGM (Observado)",
       y = "IEGM (Predito)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 6. Exibir e salvar
print(grafico_sobreposto_2)
ggsave("grafico_sobreposto_modelos_2022_2023.png", grafico_sobreposto_2, width = 10, height = 8)

library(patchwork)
combined_plot5 <- grafico_sobreposto1 + grafico_sobreposto_2 + plot_layout(ncol = 2)
print(combined_plot5)