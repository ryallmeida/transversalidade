# PLOTTAGEM DAS PROPORÇÕES DAS RESPECTIVAS CATEGORIAS 
# ----------------------------------------
# TITULO DO TRABALHO 
# EVIDENCIAS DO USO DE ALGORITMOS DE PROCESSAMENTO DE LINGUAGEM NATURAL E MACHINE LEARNING APLICADOS COMO FERRAMENTA DIAGOSTICA à TRANSVERSALIDADE EM PERNAMBUCO

# ----------------------------------------
# AUTORIA DE RYAN ALMEIDA, MARIANA BATISTA, JULIA EVELYN, LARISSA SILVA, 
# ALEXANDRE MENDES, ANA CASTELO BRANCO, CICERA VITÓRIA E A PROFA. DRA. MARIA DO CARMO SOARES DE LIMA

# ----------------------------------------
# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# DEPARTAMENTO DE CIÊNCIA POLÍTICA 
# CODADO ORIGINALMENTE EM R 4.4.3

# ==============================================================================
set.seed(123)

if(!require(pacman)){
  install.packages("pacman") 
}

pacman::p_load(tidyverse,
               viridis,
               scales,
               showtext)

# ==============================================================================

proporcao_ldo <- readr::read_csv(
  "C:/Users/ryall/Downloads/PROPORCOES/proporcao_ldo.csv",
  show_col_types = FALSE
)

proporcao_ppa <- readr::read_csv(
  "C:/Users/ryall/Downloads/PROPORCOES/proporcao_ppa.csv",
  show_col_types = FALSE
)

proporcao_loa <- readr::read_csv(
  "C:/Users/ryall/Downloads/PROPORCOES/proporcao_loa.csv",
  show_col_types = FALSE
)

dados_proporcoes <- dplyr::bind_rows(proporcao_ldo, proporcao_ppa, proporcao_loa)

readr::write_csv(
  dados_proporcoes,
  "C:/Users/ryall/Desktop/R/transversalidade/wanda/index/plots/dados_proporcoes.csv",
  na = "",
  quote = "needed"
)

# ==============================================================================
# USE O QUE SEGUE

dados_proporcoes <- readr::read_csv(
  "https://raw.githubusercontent.com/ryallmeida/transversalidade/refs/heads/main/wanda/index/plots/dados_proporcoes.csv",
  show_col_types = FALSE
)

# ==============================================================================

dados_proporcoes <- dados_proporcoes %>%
  mutate(
    espectro = case_when(
      
      # ======================
      # TIPO 1 (tricotômico)
      # ======================
      tipo == "tipo1" & classe == "NEGATIVO" ~ -1,
      tipo == "tipo1" & classe == "NEUTRO"   ~  0,
      tipo == "tipo1" & classe == "POSITIVO"~  1,
      
      # ======================
      # ESPECTRO SENSÍVEL
      # ======================
      classe == "HYPERNEGATIVO" ~ -6,
      classe == "NEGATIVO_4"    ~ -5,
      classe == "NEGATIVO_3"    ~ -4,
      classe == "NEGATIVO_2"    ~ -3,
      classe == "NEGATIVO_1"    ~ -2,
      classe == "NEUTRO"        ~  0,
      classe == "POSITIVO_1"    ~  2,
      classe == "POSITIVO_2"    ~  3,
      classe == "POSITIVO_3"    ~  4,
      classe == "POSITIVO_4"    ~  5,
      classe == "HYPERPOSITIVO" ~  6,
      
      TRUE ~ NA_real_
    )
  )

# ==============================================================================

plot_espectro_sentimento <- function(
    df,
    nivel_sel,
    titulo = NULL
) {
  
  dados_plot <- df %>%
    filter(nivel_corpus == nivel_sel) %>%
    mutate(
      espectro_f = factor(espectro, levels = sort(unique(espectro)))
    ) %>%
    arrange(espectro)
  
  ggplot(dados_plot, aes(
    x = proporcao * 100,
    y = tipo,
    fill = espectro_f
  )) +
    geom_bar(
      stat = "identity",
      width = 0.65
    ) +
    geom_text(
      aes(label = paste0(round(proporcao * 100, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 3,
      fontface = "bold"
    ) +
    facet_wrap(~ documento, nrow = 1) +
    scale_fill_viridis_d(
      option = "cividis",
      direction = -1,
      name = "Espectro"
    ) +
    scale_x_continuous(
      limits = c(0, 100),
      expand = c(0, 0)
    ) +
    labs(
      x = "Proporção (%)",
      y = NULL,
      title = titulo
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

p_paragrafo <- plot_espectro_sentimento(
  dados_proporcoes,
  nivel_sel = "paragrafo",
  titulo = ""
)

print(p_paragrafo)

p_palavra <- plot_espectro_sentimento(
  dados_proporcoes,
  nivel_sel = "palavra",
  titulo = ""
)

print(p_palavra)

# ==============================================================================
# USAR A OPÇÃO DE FUNÇÃO ABAIXO POIS REMOVE A POLUIÇÃO VISUAL
# ==============================================================================

plot_espectro_sentimento2 <- function(
    df,
    nivel_sel,
    titulo = NULL
) {
  
  dados_plot <- df %>%
    filter(nivel_corpus == nivel_sel) %>%
    mutate(
      espectro_f = factor(espectro, levels = sort(unique(espectro)))
    ) %>%
    arrange(espectro)
  
  ggplot(dados_plot, aes(
    x = proporcao * 100,
    y = tipo,
    fill = espectro_f
  )) +
    geom_bar(
      stat = "identity",
      width = 0.65
    ) +
    geom_text(
      aes(
        label = ifelse(proporcao * 100 >= 5,
                       paste0(round(proporcao * 100, 1), "%"),
                       "")
      ),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 3,
      fontface = "bold"
    ) +
    facet_wrap(~ documento, nrow = 1) +
    scale_fill_viridis_d(
      option = "cividis",
      direction = -1,
      name = "Espectro"
    ) +
    scale_x_continuous(
      limits = c(0, 100),
      expand = c(0, 0)
    ) +
    labs(
      x = "Proporção (%)",
      y = NULL,
      title = titulo
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

p_paragrafo2 <- plot_espectro_sentimento2(
  dados_proporcoes,
  nivel_sel = "paragrafo",
  titulo = ""
)

print(p_paragrafo2)

espectro <- p_paragrafo2 +
  theme(text = element_text(family = "Helvetica"))
print(espectro)

ggsave(
  filename = "C:/Users/ryall/Desktop/R/transversalidade/wanda/index/plots/espectro.png",
  plot = espectro,
  dpi = 800,
  width = 18,
  height = 10,
  units = "cm",
  type = "cairo"
)


# ==============================================================================


# --- 12. Visualização ---
p1 <- ggplot(serie_temporal, aes(x = ano, y = sentimento_diario_mean)) +
  geom_line(color = "blue") +
  geom_point(color = "black") +
  labs(title = "Sentiment Index (Average Intensity)",
       x = "Date", y = "Average sentiment") +
  theme_minimal() #+
#scale_x_date(date_labels = "%d-%b", date_breaks = "1 week")

p2 <- ggplot(serie_temporal, aes(x = ano, y = sentimento_diario_total)) +
  geom_col(fill = "darkred") +
  labs(title = "Sentiment Index (Gross Magnitude)",
       x = "Date", y = "Sum of the sentiment") +
  theme_minimal() #+
#scale_x_date(date_labels = "%d-%b", date_breaks = "1 week")

p3 <- ggplot(serie_temporal, aes(x = ano)) +
  # Linha do índice normalizado
  geom_line(aes(y = sent_norm, color = "Normalized Index"), linewidth = 0.9, alpha = 0.8) +
  
  # Linha suavizada
  geom_line(aes(y = sent_smooth, color = "Smoothed Index"), linewidth = 1.3) +
  
  # Escala de cores personalizada
  scale_color_manual(values = c("Normalized Index" = "#6a5acd",  # roxo suave
                                "Smoothed Index"   = "#ffa500")) +  # laranja
  
  # Rótulos e título
  labs(
    title = "Normalized and Smoothed Sentiment Index",
    subtitle = "Sentiment toward the Magnitsky Law on X (Twitter)",
    x = "Year", 
    y = "Sentiment (−1 to 1)",
    color = "Series"
  ) +
  
  # Eixo de datas formatado
  #scale_x_date(
  #  date_labels = "%d-%b", 
  #  date_breaks = "1 week"
  #) +
  
  # Tema visual aprimorado
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Mostrar gráficos
p1; p2; p3


##############RODAR ATÉ ANTES DESSA LINHA#######################

###############################################
#outra opção - NSS

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    positivos = sum(intensidade > 0, na.rm = TRUE),
    negativos = sum(intensidade < 0, na.rm = TRUE),
    total     = n(),
    net_sent  = (positivos - negativos) / total
  )

#O NSS indica claramente se o discurso está “puxado” para o positivo ou negativo.
ggplot(serie_temporal, aes(x = ano, y = net_sent)) +
  geom_line(linewidth = 1.2, color = "#1f77b4") +
  geom_point(size = 2, color = "#1f77b4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    title = "Net Sentiment Score (NSS) por Ano",
    subtitle = "Índice de polaridade líquida: (positivos − negativos) / total",
    x = "Ano",
    y = "NSS (−1 a +1)"
  ) +
  
  scale_y_continuous(limits = c(-1, 1)) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )


############################################################
#Índice Ponderado por Intensidade (Weighted Sentiment Index)
#Dá mais peso a termos muito positivos ou muito negativos.

serie_temporal <- serie_temporal %>%
  mutate(
    weighted_sent = ifelse(
      matched_tokens == 0, 
      NA, 
      sum_polarity / matched_tokens
    )
  )

ggplot(serie_temporal, aes(x = ano, y = weighted_sent, group = 1)) +
  geom_line(linewidth = 1.2, color = "#e66101") +
  geom_point(size = 2.5, color = "#e66101") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Weighted Sentiment por Ano",
    x = "Ano",
    y = "Índice ponderado"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#############################################################
#Índice de Polarização (Polarization Index)

#Mede o quão “extremos” são os sentimentos — mesmo com média neutra.

serie_temporal <- dados_sentimento%>%
  group_by(ano) %>%
  summarise(
    polarization = mean(abs(intensidade), na.rm = TRUE)
  )

ggplot(serie_temporal, aes(x = ano, y = polarization, group = 1)) +
  geom_line(linewidth = 1.2, color = "#e66101") +
  geom_point(size = 2.5, color = "#e66101") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Weighted Sentiment por Ano",
    x = "Ano",
    y = "Índice ponderado"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

###########################################################
#Índice de Entropia Semântica (Semantic Entropy Index)

# Mede quão diversa é a distribuição de sentimentos (confusão, variabilidade).

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    H = {
      probs <- prop.table(table(cut(intensidade, breaks = c(-1, -0.1, 0.1, 1))))
      entropy <- -sum(probs * log(probs), na.rm = TRUE)
      entropy
    }
  )

ggplot(serie_temporal, aes(x = ano, y = H, group = 1)) +
  geom_line(linewidth = 1.2, color = "#e41a1c") +
  geom_point(size = 2.5, color = "#e41a1c") +
  labs(
    title = "Índice de Entropia do Sentimento ao Longo do Tempo",
    subtitle = "Maior entropia indica distribuição mais dispersa de sentimentos",
    x = "Ano",
    y = "Entropia (H)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )


###########################################################
#Muito usado em monitoramento de redes sociais.

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    positivos = sum(intensidade > 0, na.rm = TRUE),
    negativos = sum(intensidade < 0, na.rm = TRUE),
    pri = positivos / (positivos + negativos)
  )

library(ggplot2)

ggplot(serie_temporal, aes(x = ano, y = pri, group = 1)) +
  geom_line(linewidth = 1.3, color = "#1f78b4") +
  geom_point(size = 3, color = "#1f78b4") +
  labs(
    title = "Proportion of Positive Relative to Polarized Sentiment (PRI)",
    subtitle = "PRI = positivos / (positivos + negativos)",
    x = "Ano",
    y = "PRI (0 = NEG, 1 = POS)"
  ) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

##############################################################
#Índice de Valência Média Ajustada (Adjusted Valence Index)

#Desconta a proporção de palavras sem sentimento, suavizando ruídos.

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    avi = sum(intensidade, na.rm = TRUE) /
      sum(!is.na(intensidade))
  )

ggplot(serie_temporal, aes(x = ano, y = avi, group = 1)) +
  geom_line(linewidth = 1.2, color = "#4daf4a") +
  geom_point(size = 2.8, color = "#4daf4a") +
  labs(
    title = "Average Valence Intensity (AVI) ao Longo do Tempo",
    subtitle = "Média da intensidade dos termos identificados no léxico",
    x = "Ano",
    y = "AVI (média da polaridade)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )
#############################################################
#Índice de Sentimento Diferenciado (Separated Positive/Negative Indexes)

#Útil para estudos políticos: positivo e negativo caminham diferente.

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    positive_intensity = mean(intensidade[intensidade > 0], na.rm = TRUE),
    negative_intensity = mean(intensidade[intensidade < 0], na.rm = TRUE)
  )

ggplot(serie_temporal, aes(x = ano)) +
  
  # Linha da intensidade positiva
  geom_line(aes(y = positive_intensity, color = "Positive Intensity"),
            linewidth = 1.2) +
  geom_point(aes(y = positive_intensity, color = "Positive Intensity"),
             size = 3) +
  
  # Linha da intensidade negativa
  geom_line(aes(y = negative_intensity, color = "Negative Intensity"),
            linewidth = 1.2) +
  geom_point(aes(y = negative_intensity, color = "Negative Intensity"),
             size = 3) +
  
  # Paleta de cores
  scale_color_manual(
    values = c(
      "Positive Intensity" = "#1b9e77",  # verde
      "Negative Intensity" = "#d95f02"   # laranja
    )
  ) +
  
  labs(
    title = "Positive vs Negative Intensity ao Longo do Tempo",
    subtitle = "Força média dos sentimentos positivos e negativos",
    x = "Ano",
    y = "Intensidade Média",
    color = "Série"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

