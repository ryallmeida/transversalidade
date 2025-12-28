# TRATAMENTO DO CORPUS: CONTROLE DE VIÉS AMOSTRAL 

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

if(!require(pacman)){
  install.packages("pacman") 
}

pacman::p_load(tidyverse)

# ==============================================================================

corpus_ldo <- readr::read_csv(
  "C:/Users/ryall/Downloads/CORPUS/ldo_corpus_paragrafo.csv",
  show_col_types = FALSE
)

prop.table(table(corpus_ldo$tipo1))
prop.table(table(corpus_ldo$tipo2))
prop.table(table(corpus_ldo$tipo3))

# ==============================================================================
# CORPUS PPA
# ==============================================================================
corpus_ppa <- readr::read_csv(
  "C:/Users/ryall/Downloads/CORPUS/ppa_corpus_paragrafo.csv",
  show_col_types = FALSE
)

prop.table(table(corpus_ppa$tipo1))

prop.table(table(corpus_ppa$tipo2))
# SOMA DO ESPECTRO POSITIVO

corpus_sem_na <- corpus_ppa %>%
  filter(!is.na(tipo2))

pos_ppa <- corpus_sem_na %>%
  summarise(
    proporcao = sum(tipo2 %in% c(
      "HYPERPOSITIVO",
      "POSITIVO_1", "POSITIVO_2", "POSITIVO_3", "POSITIVO_4"
    )) / n()
  )


pos_ppa <- corpus_ppa %>%
  dplyr::summarise(
    proporcao = sum(tipo2 %in% c("HYPERPOSITIVO", 
                                 "POSITIVO_1", 
                                 "POSITIVO_2", 
                                 "POSITIVO_3", 
                                 "POSITIVO_4")) / n()
  )
print(pos_ppa)

neg_ppa <- corpus_ppa %>%
  dplyr::summarise(
    proporcao = sum(tipo2 %in% c("HYPERNEGATIVO", 
                                 "NEGATIVO_1", 
                                 "NEGATIVO_2", 
                                 "NEGATIVO_3", 
                                 "NEGATIVO_4")) / n()
  )
print(neg_ppa)

neu_ppa <- corpus_ppa %>%
  dplyr::summarise(
    proporcao = sum(tipo2 %in% c("NEUTRO")) / n()
  )
print(neu_ppa)

pos_ppa + neg_ppa + neu_ppa

mean(is.na(corpus_ppa$tipo1))
mean(is.na(corpus_ppa$tipo2))
mean(is.na(corpus_ppa$tipo3))
# IDENTIFICO A PRESENÇA DE 5% DE NAS, NO BALANCEAMENTO 

# ==============================================================================
# ESTRATÉGIA DE CONTROLE DE VIÉS
# CORPUS COM AMOSTRAGEM ALEATÓRIA SEM REPOSIÇÃO LIMITADA PELA MENOR CLASSE
# ==============================================================================

corpus_limpo <- corpus_ppa %>%
  filter(!is.na(tipo2))

n_min <- corpus_limpo %>%
  count(tipo2) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

set.seed(123) 

corpus_balanceado <- corpus_limpo %>%
  group_by(tipo2) %>%
  slice_sample(n = n_min, replace = FALSE) %>%
  ungroup()

prop.table(table(corpus_balanceado$tipo2))

# ==============================================================================
# ESTRÁTÉGIA DE CONTROLE DE VIÉS [AGRUPAMENTO DE ESPECTRO]
# CONSTRUÇÃO DO CORPUS DADA A SOMA DE DETERMINADAS CLASSES, AMOSTRAGEM RANDOMIZADA SEM REPOSIÇÃO
# ==============================================================================

corpus_espectro <- corpus_ppa %>%
  filter(!is.na(tipo2)) %>%
  mutate(
    espectro = case_when(
      grepl("NEGATIVO", tipo2) ~ "NEGATIVO",
      grepl("POSITIVO", tipo2) ~ "POSITIVO",
      tipo2 == "NEUTRO"        ~ "NEUTRO",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(espectro))

n_limite <- corpus_espectro %>%
  count(espectro) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

set.seed(123)  # reprodutibilidade

corpus_balanceado <- corpus_espectro %>%
  group_by(espectro) %>%
  slice_sample(n = n_limite, replace = FALSE) %>%
  ungroup()

table(corpus_balanceado$espectro)


prop.table(table(corpus_ppa$tipo3))

# ==============================================================================

corpus_loa <- readr::read_csv(
  "C:/Users/ryall/Downloads/CORPUS/loa_corpus_paragrafo.csv",
  show_col_types = FALSE
)


