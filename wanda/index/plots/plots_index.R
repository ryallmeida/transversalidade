# TRATAMENTO DO CORPUS: ANÁLISE DAS LEIS DIRETRIZES ORCAMENTÁRIAS DE PERNAMBUCO (2008-2025)
# ----------------------------------------
# TITULO DO TRABALHO "TRANSVERSALIDADE EM PERPECTIVA: ALGORITMOS DE 
# MACHINE LEARNING APLICADOS À CIÊNCIA POLÍTICA NA INVESTIGAÇÃO DE DOCUMENTOS 
# OFICAIS"
# ----------------------------------------
# AUTORIA DE RYAN ALMEIDA, MARIANA BATISTA, JULIA EVELYN, LARISSA SILVA, 
# ALEXANDRE MENDES, ANA CASTELO BRANCO, CICERA VITÓRIA E DRA. MARIA DO CARMO SOARES DE LIMA
# ----------------------------------------
# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# DEPARTAMENTO DE CIÊNCIA POLÍTICA 
# CODADO ORIGINALMENTE EM R 4.4.3

# PLOTAGGEM DO INDEX

# =============================================================================

# ----------------------------------------
#FAZENDO O UPLOAD DE BIBLIOTECAS 
# ----------------------------------------
#CARREGANDO TODOS OS PACOTES AO MESMO TEMPO

if(!require(pacman)){
  install.packages("pacman") 
}

# -------------------------------------------------
# INSTALAÇÃO DOS PACKAGES 
# -------------------------------------------------

pacman::p_load(tidyverse,
               viridis,
               patchwork,
               tibble)

# =============================================================================

resumo_ldo <- readr::read_csv(
  "C:/Users/ryall/Documents/transversalidade/DATASETS/resumo_ldo.csv",
  show_col_types = FALSE
)

resumo_ppa <- readr::read_csv(
  "C:/Users/ryall/Documents/transversalidade/DATASETS/resumo_ppa.csv",
  show_col_types = FALSE
)

resumo_loa <- readr::read_csv(
  "C:/Users/ryall/Documents/transversalidade/DATASETS/resumo_loa.csv",
  show_col_types = FALSE
)
# =============================================================================

resumo_ldo <- resumo_ldo %>% select(-"...1")
resumo_ppa <- resumo_ppa %>% select(-"...1")
# resumo_loa não tem "...1", então não precisa mexer

resumo_ldo <- resumo_ldo %>%
  mutate(documento = "LDO")

resumo_loa <- resumo_loa %>%
  mutate(documento = "LOA")

resumo_ppa <- resumo_ppa %>%
  mutate(documento = "PPA")

index_transversalidade <- bind_rows(
  resumo_ldo,
  resumo_loa,
  resumo_ppa
)

# =============================================================================
# SALVANDO OS DADOS

readr::write_csv(
  index_transversalidade,
  "C:/Users/ryall/Desktop/R/transversalidade/wanda/index/index_transversalidade.csv",
  na = "",
  quote = "needed"
)

index_transversalidade  <- readr::read_csv(
  "C:/Users/ryall/Desktop/R/transversalidade/wanda/index/index_transversalidade.csv",
  show_col_types = FALSE
)

# =============================================================================
# PLOTAGEM DO INDEX 1 - OS DOCUMENTOS TEM TAMANHOS COMPLETAMENTE DIFERENTES
# =============================================================================

labels_documento <- index_transversalidade %>%
  group_by(documento) %>%
  filter(ano == max(ano, na.rm = TRUE)) %>%
  ungroup()

index <- ggplot(
  index_transversalidade,
  aes(
    x = ano,
    y = mean_matched,
    group = documento,
    color = mean_matched  # cor pelo nível do índice
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  # Texto flutuante acompanhando cada série (no último ano)
  geom_text(
    data = labels_documento,
    aes(label = documento),
    hjust = -0.1,          # empurra levemente para a direita
    vjust = 0.5,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # Gradiente semântico
  scale_color_viridis_c(
    option = "magma",
    name = "Nível do índice"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      min(index_transversalidade$ano, na.rm = TRUE),
      max(index_transversalidade$ano, na.rm = TRUE),
      by = 1
    ),
    expand = expansion(mult = c(0, 0.08)) # espaço para o texto à direita
  ) +
  
  labs(
    title = "",
    x = "Ano",
    y = "Variação"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

print(index)

ggsave(
  filename = "C:/Users/ryall/Desktop/R/transversalidade/wanda/index/serie_temporal.png",
  plot     = index,
  width    = 14,
  height   = 5,
  units    = "in",
  dpi      = 600,
  bg       = "white"
)

# =============================================================================
# PLOTAGEM DA PROPORÇÃO DAS CATEGORIAS 
# =============================================================================

sentimento_prop <- tribble(
  ~documento, ~nivel, ~sentimento, ~proporcao,
  
  # =====================
  # LDO
  # =====================
  
  # LDO - tipo1
  "LDO", "tipo1", "NEGATIVO", 0.1302791,
  "LDO", "tipo1", "NEUTRO",   0.5443917,
  "LDO", "tipo1", "POSITIVO", 0.3253291,
  
  # LDO - tipo2
  "LDO", "tipo2", "HYPERPOSITIVO", 0.23059326,
  "LDO", "tipo2", "NEGATIVO_1",     0.13119511,
  "LDO", "tipo2", "NEGATIVO_2",     0.25112080,
  "LDO", "tipo2", "NEGATIVO_3",     0.09252748,
  "LDO", "tipo2", "NEUTRO",         0.01005650,
  "LDO", "tipo2", "POSITIVO_2",     0.12208285,
  "LDO", "tipo2", "POSITIVO_4",     0.16242400,
  
  # LDO - tipo3
  "LDO", "tipo3", "HYPERNEGATIVO", 0.04106754,
  "LDO", "tipo3", "HYPERPOSITIVO", 0.16193908,
  "LDO", "tipo3", "NEGATIVO_1",    0.17944006,
  "LDO", "tipo3", "NEUTRO",        0.40858815,
  "LDO", "tipo3", "POSITIVO_1",    0.02764001,
  "LDO", "tipo3", "POSITIVO_2",    0.05563683,
  "LDO", "tipo3", "POSITIVO_3",    0.03617940,
  "LDO", "tipo3", "POSITIVO_4",    0.08950893,
  
  # =====================
  # PPA
  # =====================
  
  # PPA - tipo1
  "PPA", "tipo1", "NEGATIVO", 0.03735643,
  "PPA", "tipo1", "NEUTRO",   0.60523160,
  "PPA", "tipo1", "POSITIVO", 0.35741197,
  
  # PPA - tipo2
  "PPA", "tipo2", "HYPERPOSITIVO", 0.055805914,
  "PPA", "tipo2", "NEGATIVO_1",     0.083518386,
  "PPA", "tipo2", "NEGATIVO_4",     0.006100085,
  "PPA", "tipo2", "NEUTRO",         0.409795388,
  "PPA", "tipo2", "POSITIVO_1",     0.069849421,
  "PPA", "tipo2", "POSITIVO_2",     0.030507771,
  "PPA", "tipo2", "POSITIVO_3",     0.043182546,
  "PPA", "tipo2", "POSITIVO_4",     0.301240488,
  
  # PPA - tipo3
  "PPA", "tipo3", "HYPERNEGATIVO", 0.01052280,
  "PPA", "tipo3", "HYPERPOSITIVO", 0.17259417,
  "PPA", "tipo3", "NEGATIVO_1",    0.06377980,
  "PPA", "tipo3", "NEUTRO",        0.46255481,
  "PPA", "tipo3", "POSITIVO_1",    0.12715750,
  "PPA", "tipo3", "POSITIVO_2",    0.05224901,
  "PPA", "tipo3", "POSITIVO_3",    0.03352189,
  "PPA", "tipo3", "POSITIVO_4",    0.07762001,
  
  # =====================
  # LOA
  # =====================
  
  # LOA - tipo1
  "LOA", "tipo1", "NEGATIVO", 0.1208906,
  "LOA", "tipo1", "NEUTRO",   0.5176700,
  "LOA", "tipo1", "POSITIVO", 0.3614394,
  
  # LOA - tipo2
  "LOA", "tipo2", "HYPERNEGATIVO", 0.0002133187,
  "LOA", "tipo2", "HYPERPOSITIVO", 0.1055269417,
  "LOA", "tipo2", "NEGATIVO_1",    0.1448333139,
  "LOA", "tipo2", "NEGATIVO_2",    0.0519701689,
  "LOA", "tipo2", "NEGATIVO_3",    0.0775271070,
  "LOA", "tipo2", "NEGATIVO_4",    0.0716588935,
  "LOA", "tipo2", "NEUTRO",        0.1545226018,
  "LOA", "tipo2", "POSITIVO_1",    0.0915175171,
  "LOA", "tipo2", "POSITIVO_2",    0.0928792882,
  "LOA", "tipo2", "POSITIVO_3",    0.0604323980,
  "LOA", "tipo2", "POSITIVO_4",    0.1489184512,
  
  # LOA - tipo3
  "LOA", "tipo3", "HYPERNEGATIVO", 0.002204318,
  "LOA", "tipo3", "HYPERPOSITIVO", 0.130036919,
  "LOA", "tipo3", "NEGATIVO_1",    0.184956156,
  "LOA", "tipo3", "NEUTRO",        0.398708223,
  "LOA", "tipo3", "POSITIVO_1",    0.103976108,
  "LOA", "tipo3", "POSITIVO_2",    0.031254151,
  "LOA", "tipo3", "POSITIVO_3",    0.023245947,
  "LOA", "tipo3", "POSITIVO_4",    0.125618179
)

# níveis
neg_levels <- c(
  "HYPERNEGATIVO",
  "NEGATIVO_1",
  "NEGATIVO_2",
  "NEGATIVO_3",
  "NEGATIVO_4",
  "NEGATIVO"
)

pos_levels <- c(
  "POSITIVO",
  "POSITIVO_1",
  "POSITIVO_2",
  "POSITIVO_3",
  "POSITIVO_4",
  "HYPERPOSITIVO"
)

# gradientes cividis
cores_neg <- viridis(
  n = length(neg_levels),
  option = "cividis",
  begin = 0.05,
  end = 0.45
)

cores_pos <- viridis(
  n = length(pos_levels),
  option = "cividis",
  begin = 0.55,
  end = 0.95
)

# neutro fixo (cividis médio)
cor_neutro <- viridis(
  n = 1,
  option = "cividis",
  begin = 0.50,
  end = 0.50
)

# vetor final
cores_sentimento <- c(
  setNames(cores_neg, neg_levels),
  "NEUTRO" = cor_neutro,
  setNames(cores_pos, pos_levels)
)


ordem_sentimento <- c(
  "HYPERNEGATIVO",
  "NEGATIVO_1",
  "NEGATIVO_2",
  "NEGATIVO_3",
  "NEGATIVO_4",
  "NEGATIVO",
  "NEUTRO",
  "POSITIVO",
  "POSITIVO_1",
  "POSITIVO_2",
  "POSITIVO_3",
  "POSITIVO_4",
  "HYPERPOSITIVO"
)


plot_proporcao_tipos <- function(df) {
  
  ordem_sentimento <- c(
    "HYPERNEGATIVO",
    "NEGATIVO_1",
    "NEGATIVO_2",
    "NEGATIVO_3",
    "NEGATIVO_4",
    "NEGATIVO",
    "NEUTRO",
    "POSITIVO",
    "POSITIVO_1",
    "POSITIVO_2",
    "POSITIVO_3",
    "POSITIVO_4",
    "HYPERPOSITIVO"
  )
  
  cores_sentimento <- c(
    "HYPERNEGATIVO" = "#08306B",
    "NEGATIVO_1"    = "#08519C",
    "NEGATIVO_2"    = "#2171B5",
    "NEGATIVO_3"    = "#4292C6",
    "NEGATIVO_4"    = "#6BAED6",
    "NEGATIVO"      = "#9ECAE1",
    "NEUTRO"        = "#BDBDBD",
    "POSITIVO"      = "#FEE391",
    "POSITIVO_1"    = "#FEC44F",
    "POSITIVO_2"    = "#FE9929",
    "POSITIVO_3"    = "#EC7014",
    "POSITIVO_4"    = "#CC4C02",
    "HYPERPOSITIVO" = "#993404"
  )
  
  dados_plot <- df %>%
    mutate(
      prop = proporcao * 100,
      sentimento = factor(sentimento, levels = ordem_sentimento),
      nivel = factor(nivel, levels = c("tipo1", "tipo2", "tipo3")),
      documento = factor(documento, levels = c("LDO", "PPA", "LOA"))
    )
  
  ggplot(dados_plot, aes(x = prop, y = nivel, fill = sentimento)) +
    geom_col(width = 0.75) +
    geom_text(
      aes(label = paste0(round(prop, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 3
    ) +
    facet_wrap(~ documento, nrow = 1) +
    scale_fill_manual(
      values = cores_sentimento,
      name = "Categoria de sentimento",
      drop = FALSE
    ) +
    scale_x_continuous(
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      x = "Proporção (%)",
      y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 14)
    )
}

proportions <- plot_proporcao_tipos(sentimento_prop)

ggsave(
  filename = "C:/Users/ryall/Desktop/R/transversalidade/wanda/index/distribuicao_sentimentos.png",
  plot     = proportions,
  width    = 14,
  height   = 5,
  units    = "in",
  dpi      = 600,
  bg       = "white"
)

