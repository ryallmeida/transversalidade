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

