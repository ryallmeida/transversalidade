# PLOTTAGEM DAS SERIES TEMPORAIS DA TRANSVERSALIDADE NOS DOCUMENTOS OFICIAS DE PLANEJAMENTO DO ESTADO

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
               showtext,
               zoo)

# ==============================================================================
# CRIANDO RESUMO POR ANO
# ==============================================================================

dados_sentimentos_ldo <- readr::read_csv(
  "C:/Users/ryall/Downloads/CORPUS/ldo_corpus_palavra.csv",
  show_col_types = FALSE
)

dados_sentimentos_ppa <- readr::read_csv(
  "C:/Users/ryall/Downloads/CORPUS/ppa_corpus_palavra.csv",
  show_col_types = FALSE
)

dados_sentimentos_loa <- readr::read_csv(
  "C:/Users/ryall/Downloads/CORPUS/loa_corpus_palavra.csv",
  show_col_types = FALSE
)


dados_sentimento_resumo_ldo <- dados_sentimentos_ldo %>%
  group_by(ano) %>%
  summarise(
    matched_tokens = sum(!is.na(intensidades)),
    sum_polarity   = sum(intensidades, na.rm = TRUE),
    mean_matched   = ifelse(matched_tokens == 0, 0, sum_polarity / matched_tokens),
    mean_all       = mean(replace_na(intensidades, 0)),
    .groups = "drop"
  )

dados_sentimento_resumo_ldo$documento <- "LDO"


dados_sentimento_resumo_ppa <- dados_sentimentos_ppa %>%
  group_by(ano) %>%
  summarise(
    matched_tokens = sum(!is.na(intensidades)),
    sum_polarity   = sum(intensidades, na.rm = TRUE),
    mean_matched   = ifelse(matched_tokens == 0, 0, sum_polarity / matched_tokens),
    mean_all       = mean(replace_na(intensidades, 0)),
    .groups = "drop"
  )

dados_sentimento_resumo_ppa$documento <- "PPA"


dados_sentimento_resumo_loa <- dados_sentimentos_loa %>%
  group_by(ano) %>%
  summarise(
    matched_tokens = sum(!is.na(intensidades)),
    sum_polarity   = sum(intensidades, na.rm = TRUE),
    mean_matched   = ifelse(matched_tokens == 0, 0, sum_polarity / matched_tokens),
    mean_all       = mean(replace_na(intensidades, 0)),
    .groups = "drop"
  )

dados_sentimento_resumo_loa$documento <- "LOA"

resumo <- dplyr::bind_rows(dados_sentimento_resumo_ldo, dados_sentimento_resumo_ppa, dados_sentimento_resumo_loa)

readr::write_csv(
  resumo,
  "C:/Users/ryall/Desktop/R/transversalidade/wanda/index/plots/resumo_serie_temporal.csv",
  na = "",
  quote = "needed"
)

rm(resumo); gc()

# ==============================================================================
# USE O QUE SEGUE
# ==============================================================================

serie_temporal <- readr::read_csv(
  "https://raw.githubusercontent.com/ryallmeida/transversalidade/refs/heads/main/wanda/index/plots/resumo_serie_temporal.csv",
  show_col_types = FALSE
)


df_final <- serie_temporal %>%
  group_by(documento, ano) %>%
  summarise(
    sentimento_diario_mean  = mean(mean_matched, na.rm = TRUE),
    sentimento_diario_total = sum(sum_polarity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(documento) %>% 
  mutate(
    max_abs = max(abs(sentimento_diario_total), na.rm = TRUE),
    sent_norm = ifelse(max_abs == 0, 0, sentimento_diario_total / max_abs),
    sent_smooth = zoo::rollmean(
      sent_norm,
      k = 3,
      fill = NA,
      align = "right"
    )
  ) %>%
  ungroup() %>%
  select(-max_abs)


# ==============================================================================
# PLOTTAGEM 
# ==============================================================================

vars_keep <- c(
  "texto_limpo",
  "ano",
  "label_final",
  "espectro_tipo2",
  "espectro_tipo3",
  "documento"
)

corpus_unificado <- bind_rows(
  corpus_ldo %>% select(all_of(vars_keep)),
  corpus_ppa %>% select(all_of(vars_keep)),
  corpus_loa %>% select(all_of(vars_keep))
)

resumo <- corpus_unificado %>%
  mutate(
    soma_vars  = label_final + espectro_tipo2 + espectro_tipo3,
    media_vars = (label_final + espectro_tipo2 + espectro_tipo3) / 3,
    mediana_vars = pmax(
      pmin(label_final + espectro_tipo2 + espectro_tipo3),
      pmin(pmax(label_final, espectro_tipo2), pmax(label_final, espectro_tipo3), pmax(espectro_tipo2, espectro_tipo3))
    ),
    desvio_vars = sqrt(
      ((label_final - media_vars)^2 +
         (espectro_tipo2 - media_vars)^2 +
         (espectro_tipo3 - media_vars)^2) / 2
    )
  )

resumo_agrupado <- resumo %>%
  group_by(ano, documento) %>%
  summarise(
    media_soma_vars    = mean(soma_vars, na.rm = TRUE),
    media_media_vars   = mean(media_vars, na.rm = TRUE),
    media_mediana_vars = mean(mediana_vars, na.rm = TRUE),
    media_desvio_vars  = mean(desvio_vars, na.rm = TRUE),
    .groups = "drop"
  )

resumo_agrupado2 <- resumo %>%
  group_by(ano) %>%
  summarise(
    media_media_vars = mean(mediana_vars, na.rm = TRUE),
    .groups = "drop"
  )

p0 <- ggplot(
  resumo_agrupado,
  aes(
    x = ano,
    y = media_media_vars,
    color = media_media_vars
  )
) +
  
  geom_line(
    linewidth = 1.2,
    alpha = 0.9
  ) +
  
  geom_point(
    size = 2.8
  ) +
  
  # escala viridis
  scale_color_viridis_c(
    option = "cividis",
    name = "Nível"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(resumo_agrupado$ano), max(resumo_agrupado$ano), by = 1),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  
  labs(
    title = "Média anual da variação (agregação geral)",
    x = "Ano",
    y = "Variação"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92"),
    legend.position = "bottom",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

p1 <- ggplot(
  resumo_agrupado,
  aes(
    x = ano,
    y = media_media_vars,
    group = documento,
    color = media_media_vars
  )
) +
  
  geom_line(
    linewidth = 1.2,
    alpha = 0.9
  ) +
  
  geom_point(
    size = 2.8
  ) +
  
  # rótulos diretos no último ano
  geom_text(
    data = resumo_agrupado %>%
      dplyr::group_by(documento) %>%
      dplyr::slice_max(ano),
    aes(label = documento),
    hjust = -0.1,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # escala viridis magma
  scale_color_viridis_c(
    option = "cividis",
    name = "Nível"
  ) +
  
  # eixo X com todos os anos
  scale_x_continuous(
    breaks = seq(min(resumo_agrupado$ano), max(resumo_agrupado$ano), by = 1),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  
  labs(
    title = "Agrupamento pela média da variação anual",
    x = "Ano",
    y = "Variação",
    color = "Documento"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


p2 <- ggplot(
  resumo_agrupado,
  aes(
    x = ano,
    y = media_mediana_vars,
    group = documento,
    color = media_mediana_vars
  )
) +
  
  geom_line(
    linewidth = 1.2,
    alpha = 0.9
  ) +
  
  geom_point(
    size = 2.8
  ) +
  
  # rótulos diretos no último ano
  geom_text(
    data = resumo_agrupado %>%
      dplyr::group_by(documento) %>%
      dplyr::slice_max(ano),
    aes(label = documento),
    hjust = -0.1,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # escala viridis magma
  scale_color_viridis_c(
    option = "cividis",
    name = "Nível"
  ) +
  
  # eixo X com todos os anos
  scale_x_continuous(
    breaks = seq(min(resumo_agrupado$ano), max(resumo_agrupado$ano), by = 1),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  
  labs(
    title = "Agrupamento pela mediana da variação anual",
    x = "Ano",
    y = "Variação",
    color = "Documento"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

p3 <- ggplot(
  resumo_agrupado,
  aes(
    x = ano,
    y = media_desvio_vars,
    group = documento,
    color = media_desvio_vars
  )
) +
  
  geom_line(
    linewidth = 1.2,
    alpha = 0.9
  ) +
  
  geom_point(
    size = 2.8
  ) +
  
  # rótulos diretos no último ano
  geom_text(
    data = resumo_agrupado %>%
      dplyr::group_by(documento) %>%
      dplyr::slice_max(ano),
    aes(label = documento),
    hjust = -0.1,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # escala viridis magma
  scale_color_viridis_c(
    option = "cividis",
    name = "Nível"
  ) +
  
  # eixo X com todos os anos
  scale_x_continuous(
    breaks = seq(min(resumo_agrupado$ano), max(resumo_agrupado$ano), by = 1),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  
  labs(
    title = "Desvio padrão médio da variação",
    x = "Ano",
    y = "Variação",
    color = "Documento"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(color = "grey85"),
        panel.grid.minor = element_line(color = "grey92"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

library(patchwork)
p0 + p2 + p1 + p3 + plot_layout(nrow = 2, ncol =  2)

print(p0)
# write.csv(dados_sentimento_resumo, "C:/Users/ryall/Documents/transversalidade/DATASETS/resumo_ldo.csv")