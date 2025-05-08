# ===================================================================================
# SCRIPT::FREQUENCIA DE ALIMENTAÇÃO DO SITE 'ALICE BEE NO VALE DAS NINFAS'
# ===================================================================================

# PACKAGES
library(ggplot2)
library(dplyr)

# UPANDO DADOS
dados <- data.frame(
  Mes = c("Maio", "Junho", "Julho", "Agosto", "Setembro", 
          "Outubro", "Novembro", "Dezembro", "Fevereiro", 
          "Março", "Abril"),
  Frequencia = c(
    9,                        # Maio
    sum(c(7, 9, 6, 10)),      # Junho = 32
    sum(c(11, 5, 7, 11, 11)), # Julho = 45
    sum(c(8, 7, 7, 9)),       # Agosto = 31
    sum(c(10, 8, 10, 10)),    # Setembro = 38
    sum(c(8, 9, 9, 9, 7)),    # Outubro = 42
    sum(c(11, 9, 13, 10)),    # Novembro = 43
    sum(c(8, 0, 9)),          # Dezembro (faltou 1 valor, tratei como 0) = 17
    sum(c(7, 5)),             # Fevereiro = 12
    sum(c(8, 10, 7, 8)),      # Março = 33
    sum(c(9, 5, 7))           # Abril = 21
  )
)

# ORDENANDO OS MESES
dados$Mes <- factor(dados$Mes, 
                    levels = c("Maio", "Junho", "Julho", "Agosto", 
                               "Setembro", "Outubro", "Novembro", 
                               "Dezembro", "Fevereiro", "Março", "Abril"))

# MÉDIA
media_frequencia <- mean(dados$Frequencia)

# GRÁFICO
ggplot(dados, aes(x = Mes, y = Frequencia)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Frequencia), 
            vjust = -0.5) +
  geom_hline(yintercept = media_frequencia, 
             color = "black", 
             linetype = "dashed", 
             size = 1) +
  annotate("text", x = 1, 
           y = media_frequencia + 1, 
           label = paste("", round(media_frequencia, 2)), 
           color = "black", 
           hjust = 0) +
  labs(title = "",
       x = "Mês",
       y = "Alimentação") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# INFO ADD MEDIA APROXIMADA FOI DE 30 UND. POR MES