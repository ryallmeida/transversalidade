# ====================================================================================
# GRAFICO 1
# ====================================================================================


library(ggplot2)
library(dplyr)

# Supondo que 'dados_grafico1' e 'dados_grafico2' tenham os mesmos nomes de colunas e correspondam ao formato correto.

# Adicionar uma coluna 'Ano' para diferenciar os dois conjuntos de dados
dados_grafico1 <- dados_grafico1 %>%
  mutate(Ano = "2017")

dados_grafico2 <- dados_grafico2 %>%
  mutate(Ano = "2019")

# Combinar os dois data.frames
dados_combinados <- bind_rows(dados_grafico1, dados_grafico2)

# Criar o gráfico sobreposto
p1.1 <- ggplot(dados_combinados, aes(x = Fitted, y = Residuos, color = Ano)) +
  geom_point(alpha = 0.7, shape = 1, size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Resíduos vs. Valores Ajustados: Comparação 2017 x 2019",
       x = "Valores Ajustados", y = "Resíduos") +
  scale_color_manual(values = c("2017" = "#228B22", "2019" = "blue")) +  # Definindo cores
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ====================================================================================
# GRAFICO 2
# ====================================================================================

library(ggplot2)
library(dplyr)

# Adicionar identificadores de grupo
dados_grafico1 <- dados_grafico1 %>%
  mutate(Ano = "2017")

dados_grafico2 <- dados_grafico %>%
  mutate(Ano = "2019")

# Combinar os dados
dados_qq_combinados <- bind_rows(dados_grafico1, dados_grafico2)

# Gráfico QQ sobreposto
p2.1 <- ggplot(dados_qq_combinados, aes(sample = Residuos, color = Ano)) +
  stat_qq(alpha = 0.7) +
  stat_qq_line(aes(color = Ano)) +
  labs(title = "Gráfico QQ dos Resíduos: Comparação 2017 x 2019",
       x = "Quantis Teóricos", y = "Quantis Amostrais") +
  scale_color_manual(values = c("2017" = "#228B22", "2019" = "#1E90FF")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ====================================================================================
# GRAFICO 3
# ====================================================================================

# Adicionar identificadores de grupo
dados_grafico1 <- dados_grafico1 %>%
  mutate(Ano = "2017")

dados_grafico2 <- dados_grafico %>%
  mutate(Ano = "2019")

# Combinar os dados
dados_hist_combinados <- bind_rows(dados_grafico1, dados_grafico2)

# Histograma sobreposto
p3.1 <- ggplot(dados_hist_combinados, aes(x = StdResiduos, fill = Ano)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  scale_fill_manual(values = c("2017" = "#228B22", "2019" = "lightblue")) +
  labs(title = "Histograma dos Resíduos Padronizados: Comparação 2017 x 2019",
       x = "Resíduos Padronizados", y = "Frequência") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))


# ====================================================================================
# GRAFICO 4
# ====================================================================================

# Adicionar ano aos dados
dados_diag1$Ano <- "2017"
dados_diag2$Ano <- "2019"
dados_acima1$Ano <- "2017"
dados_acima2$Ano <- "2019"

# Unir dados
dados_diag_combinado <- bind_rows(dados_diag1, dados_diag2)
dados_acima_combinado <- bind_rows(dados_acima1, dados_acima2)

# Gráfico combinado da Distância de Cook
p4.1 <- ggplot(dados_diag_combinado, aes(x = Index, y = CooksDist, color = Ano)) +
  geom_point(shape = 1, size = 2, alpha = 0.7) +
  geom_point(data = dados_acima_combinado, aes(x = Index, y = CooksDist), 
             shape = 8, color = "red", size = 2) +
  geom_text_repel(data = dados_acima_combinado, aes(label = Index),
                  size = 3, box.padding = 0.5, point.padding = 0.5, segment.color = 'grey50') +
  labs(title = "Distância de Cook: Comparação 2017 x 2019",
       x = "Observações", y = "Distância de Cook") +
  scale_color_manual(values = c("2017" = "mediumpurple", "2019" = "blue")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ====================================================================================
# GRAFICO 4
# ====================================================================================


# Calcular distância de Cook para modelo 1 (2017)
cooks_dist1 <- cooks.distance(modelo1)
limite_influencia1 <- 4 / length(fitted(modelo1))
pontos_influentes1 <- which(cooks_dist1 > limite_influencia1)

dados_diag1 <- data.frame(
  Index = 1:length(cooks_dist1),
  CooksDist = cooks_dist1,
  Ano = "2017"
)

dados_acima1 <- dados_diag1[pontos_influentes1, ]

# Calcular distância de Cook para modelo 2 (2019)
cooks_dist2 <- cooks.distance(modelo2)
limite_influencia2 <- 4 / length(fitted(modelo2))
pontos_influentes2 <- which(cooks_dist2 > limite_influencia2)

dados_diag2 <- data.frame(
  Index = 1:length(cooks_dist2),
  CooksDist = cooks_dist2,
  Ano = "2019"
)

dados_acima2 <- dados_diag2[pontos_influentes2, ]

# Unir os dados
dados_diag_combinado <- bind_rows(dados_diag1, dados_diag2)
dados_acima_combinado <- bind_rows(dados_acima1, dados_acima2)

# Gráfico final combinado
p4.1 <- ggplot(dados_diag_combinado, aes(x = Index, y = CooksDist, color = Ano)) +
  geom_point(shape = 1, size = 2, alpha = 0.7) +
  geom_point(data = dados_acima_combinado, aes(x = Index, y = CooksDist), 
             shape = 8, color = "red", size = 2) +
  geom_text_repel(data = dados_acima_combinado, aes(label = Index),
                  size = 3, box.padding = 0.5, point.padding = 0.5, segment.color = 'grey50') +
  labs(title = "Distância de Cook: Comparação 2017 x 2019",
       x = "Observações", y = "Distância de Cook") +
  scale_color_manual(values = c("2017" = "#228B22", "2019" = "blue")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ====================================================================================
# GRAFICO 4
# ====================================================================================

library(patchwork)
combined_plot1 <- p1.1 + p2.1 + p3.1 + p4.1 + plot_layout(ncol = 2)
print(combined_plot1)

ggplot2::ggsave("COMPARACAO_17x19(1).png", combined_plot1, width = 10, height = 8)

# ====================================================================================
# PREDICTO X OBS
# ====================================================================================

library(ggplot2)

# Adicionar coluna 'Ano' aos dataframes
Zscore_2017$Ano <- "2017"
Zscore_2017$IEGM_Obs <- Zscore_2017$IEGM_taxa

Zscore_2019$Ano <- "2019"
Zscore_2019$IEGM_Obs <- Zscore_2019$IEGM_Taxa_2019

# Unir os dois dataframes
dados_comparativos <- rbind(Zscore_2017[, c("IEGM_Obs", "preditos", "Ano")],
                            Zscore_2019[, c("IEGM_Obs", "preditos", "Ano")])

# Modelos de regressão por ano
modelo_ajuste_2017 <- lm(preditos ~ IEGM_Obs, data = subset(dados_comparativos, Ano == "2017"))
modelo_ajuste_2019 <- lm(preditos ~ IEGM_Obs, data = subset(dados_comparativos, Ano == "2019"))

# Equações e R²
coef_2017 <- coef(modelo_ajuste_2017)
r2_2017 <- summary(modelo_ajuste_2017)$adj.r.squared
texto_eq_2017 <- paste0("2017: y = ", round(coef_2017[2], 3), "x + ", round(coef_2017[1], 3),
                        "\nR² = ", round(r2_2017, 3))

coef_2019 <- coef(modelo_ajuste_2019)
r2_2019 <- summary(modelo_ajuste_2019)$adj.r.squared
texto_eq_2019 <- paste0("2019: y = ", round(coef_2019[2], 3), "x + ", round(coef_2019[1], 3),
                        "\nR² = ", round(r2_2019, 3))

# Gráfico com cores manuais
grafico_sobreposto1 <- ggplot(dados_comparativos, aes(x = IEGM_Obs, y = preditos)) +
  geom_point(data = subset(dados_comparativos, Ano == "2017"),
             color = "#228B22", size = 2, shape = 1) +
  geom_smooth(data = subset(dados_comparativos, Ano == "2017"),
              method = "lm", se = FALSE, color = "#228B22") +
  geom_point(data = subset(dados_comparativos, Ano == "2019"),
             color = "#1E3A8A", size = 2, shape = 1) +
  geom_smooth(data = subset(dados_comparativos, Ano == "2019"),
              method = "lm", se = FALSE, color = "#1E3A8A") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  annotate("text",
           x = min(dados_comparativos$IEGM_Obs),
           y = max(dados_comparativos$preditos),
           label = texto_eq_2017,
           hjust = 0, size = 4, color = "#228B22") +
  annotate("text",
           x = min(dados_comparativos$IEGM_Obs),
           y = max(dados_comparativos$preditos) * 0.85,
           label = texto_eq_2019,
           hjust = 0, size = 4, color = "#1E3A8A") +
  labs(title = "Modelos 2017 e 2019 sobrepostos",
       x = "IEGM (Observado)",
       y = "IEGM (Predito)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Exibir gráfico
print(grafico_sobreposto1)
