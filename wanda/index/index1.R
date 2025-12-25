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

# =============================================================================

# ----------------------------------------
#FAZENDO O UPLOAD DE BIBLIOTECAS 
# ----------------------------------------
#CARREGANDO TODOS OS PACOTES AO MESMO TEMPO

if(!require(pacman)){
  install.packages("pacman") 
}

pacman::p_load(tidyverse)

# =============================================================================
# LEITURA DOS ARQUIVOS E CONSTRUÇÃO DO CORPUS
# =============================================================================
# lendo o caminho considerando apenas arquivos em pdf

arquivos_ldo <- list.files(
  path       = "C:/Users/ryall/Documents/transversalidade/LDO",
  pattern    = "\\.pdf$", 
  full.names = TRUE
)

textos_unificados_ldo <- sapply(
  arquivos_ldo,
  function(arq) {
    paginas <- pdf_text(arq)
    paste(paginas, collapse = " ")
  },
  USE.NAMES = FALSE
)

df_corpus_ldo <- data.frame(
  doc_id = basename(arquivos_ldo),
  text   = textos_unificados_ldo,
  stringsAsFactors = FALSE
)

corpus_ldo <- quanteda::corpus(
  x             = df_corpus_ldo,
  text_field    = "text",
  docid_field   = "doc_id"
)

corpus_ldo <- quanteda::corpus(
  x = setNames(df_corpus_ldo$text, df_corpus_ldo$doc_id)
)

summary(corpus_ldo)
class(corpus_ldo)

# =============================================================================
# PREPROCESSAMENTO DOS DADOS
# =============================================================================

tokens_ldo <- corpus_ldo|>
  quanteda::tokens(
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE,
    remove_separators = TRUE,
    split_hyphens = TRUE
  ) |>
  quanteda::tokens_tolower() |>   # deixa tudo minúsculo
  quanteda::tokens_remove(pattern = stopwords("portuguese")) |>  # remove stopwords
  quanteda::tokens_remove(pattern = c(
    "nº", "1º", "2º", "3º", "4º",
    "lei", "outubro", "inciso", "parágrafo",
    "art", "artigo", "caput", "único",
    "i","ii","iii","iv","v","vi","vii","viii",
    letters   # remove letras sozinhas (a, b, c, d...)
  )) |>
  # quanteda::tokens_wordstem(language = "portuguese") |>  # stemming opcional
  quanteda::tokens_trim()     # remove tokens vazios


# =============================================================================
# CRIAÇÃO DO PRIMEIRO INDEX ROBUSTO
# =============================================================================

caminho_lexico <- "https://raw.githubusercontent.com/ryallmeida/transversalidade/refs/heads/main/wanda/preprocessamento/dataframes/matriz_td-idf_ldo.csv"
lexico <- read.csv(caminho_lexico, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

lexico_unique <- lexico %>% distinct(word, .keep_all = TRUE)

dados_sentimento <- tokens_ldo %>%
  left_join(lexico_unique, by = c("word" = "word"))
