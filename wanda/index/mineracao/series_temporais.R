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

p1 <- ggplot(
  df_final,
  aes(
    x = ano,
    y = sentimento_diario_mean,
    color = documento,
    group = documento
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_viridis_d(option = "magma") +
  scale_x_continuous(
    breaks = seq(
      min(df_final$ano, na.rm = TRUE),
      max(df_final$ano, na.rm = TRUE),
      by = 1
    )
  ) +
  labs(
    title = "",
    x = "Ano",
    y = "Variação",
    color = "Documento"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(p1)

p2 <- ggplot(
  df_final,
  aes(
    x = ano,
    y = sent_smooth,
    group = documento,
    color = sent_smooth
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
    data = df_final %>%
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
    breaks = seq(min(df_final$ano), max(df_final$ano), by = 1),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  
  labs(
    x = "Ano",
    y = "Variação"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title = element_text(face = "bold")
  )

print(p2)

# write.csv(dados_sentimento_resumo, "C:/Users/ryall/Documents/transversalidade/DATASETS/resumo_ldo.csv")