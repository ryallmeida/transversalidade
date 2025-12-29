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

corpus_ldo$documento <- "LDO"

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

corpus_ppa$documento <- "PPA"

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
# CORPUS COM AMOSTRAGEM ALEATÓRIA SEM REPOSIÇÃO LIMITADA PELA MENOR CLASSE EM CADA TIPO
# ==============================================================================

n_min <- corpus_unificado %>%
  count(espectro_tipo2) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

set.seed(123) 

corpus_balanceado_tipo2 <- corpus_unificado %>%
  group_by(espectro_tipo2) %>%
  slice_sample(n = n_min, replace = FALSE) %>%
  ungroup()

prop.table(table(corpus_balanceado$tipo2))

# ==============================================================================
# ESTRÁTÉGIA DE CONTROLE DE VIÉS [AGRUPAMENTO DE ESPECTRO]
# CONSTRUÇÃO DO CORPUS DADA A SOMA DE DETERMINADAS CLASSES, AMOSTRAGEM RANDOMIZADA SEM REPOSIÇÃO
# ==============================================================================

n_limite <- corpus_unificado %>%
  count(label_final) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

set.seed(123) 

corpus_balanceado_tp1 <- corpus_unificado %>%
  group_by(label_final) %>%
  slice_sample(n = n_limite, replace = FALSE) %>%
  ungroup()

prop.table(table(corpus_balanceado_tp1$label_final))

prop.table(table(corpus_balanceado_tp1$documento))

readr::write_csv(
  corpus_balanceado_tp1,
  "C:/Users/ryall/Downloads/corpus_balanceado_tp1.csv",
  na = "",
  quote = "needed"
)

n_limite <- corpus_unificado %>%
  count(espectro_tipo2) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

set.seed(123) 

corpus_balanceado_tp2 <- corpus_unificado %>%
  group_by(espectro_tipo2) %>%
  slice_sample(n = n_limite, replace = FALSE) %>%
  ungroup()

prop.table(table(corpus_balanceado_tp2$espectro_tipo2))

prop.table(table(corpus_balanceado_tp2$documento))


readr::write_csv(
  corpus_balanceado_tp2,
  "C:/Users/ryall/Downloads/corpus_balanceado_tp2.csv",
  na = "",
  quote = "needed"
)

n_limite <- corpus_unificado %>%
  count(espectro_tipo3) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

set.seed(123) 

corpus_balanceado_tp3 <- corpus_unificado %>%
  group_by(espectro_tipo3) %>%
  slice_sample(n = n_limite, replace = FALSE) %>%
  ungroup()

prop.table(table(corpus_balanceado_tp3$espectro_tipo3))

prop.table(table(corpus_balanceado_tp3$documento))

readr::write_csv(
  corpus_balanceado_tp3,
  "C:/Users/ryall/Downloads/corpus_balanceado_tp3.csv",
  na = "",
  quote = "needed"
)


# ==============================================================================

corpus_loa <- readr::read_csv(
  "C:/Users/ryall/Downloads/CORPUS/loa_corpus_paragrafo.csv",
  show_col_types = FALSE
)

corpus_loa$documento <- "LOA"

# ==============================================================================
# UNIFICANDO BANCO DE DADOS

vars_keep <- c(
  "texto_limpo",
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

rm(vars_keep); gc()

corpus_unificado <- corpus_unificado %>%
  tidyr::drop_na()

corpus_unificado <- corpus_unificado %>%
  mutate(
    zero_triplo = espectro_tipo2 == 0 & espectro_tipo3 == 0 & label_final == 0
  )

prop.table(table(corpus_unificado$zero_duplo))

prop.table(table(is.na(corpus_unificado)))

prop.table(table(corpus_unificado$label_final))
prop.table(table(corpus_unificado$espectro_tipo2))
prop.table(table(corpus_unificado$espectro_tipo3))

prop.table(table(corpus_unificado$documento))

(0.6437210 + 0.38406222 + 0.419221890 )/3

resumo <- corpus_unificado %>%
  mutate(
    soma_vars    = label_final + espectro_tipo2 + espectro_tipo3,
    media_vars   = rowMeans(across(c(label_final + espectro_tipo2 + espectro_tipo3)), na.rm = TRUE),
    mediana_vars = apply(select(., label_final + espectro_tipo2 + espectro_tipo3), 1, median, na.rm = TRUE),
    desvio_vars  = apply(select(., label_final + espectro_tipo2 + espectro_tipo3), 1, sd, na.rm = TRUE)
  )

corpus_unificado$label_final

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

n_min <- resumo %>%
  count(mediana_vars) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

set.seed(123) 

corpus_balanceado <- resumo %>%
  group_by(mediana_vars) %>%
  slice_sample(n = n_min, replace = FALSE) %>%
  ungroup()

dados <- corpus_balanceado %>%
  dplyr::select(texto_limpo, label_final, espectro_tipo2, espectro_tipo3)


readr::write_csv(
  dados,
  "C:/Users/ryall/Downloads/corpus_balanceado_mediana.csv",
  na = "",
  quote = "needed"
)

# ==============================================================================


corpus_desbalanceado <- corpus_unificado %>%
  dplyr::select(texto_limpo, label_final, espectro_tipo2, espectro_tipo3)

readr::write_csv(
  corpus_desbalanceado,
  "C:/Users/ryall/Downloads/corpus_desbalanceado.csv",
  na = "",
  quote = "needed"
)

