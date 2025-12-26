# TRATAMENTO DO CORPUS: ANÁLISE DAS LEIS ORÇAMENTÁRIAS ANUAIS DE PERNAMBUCO (2008-2025)
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

# NOTA ESSE PROJETO EM ESPEIFICO VAI LHE MOSTRAR COMO CONSEGUIR O CORPUS NO NIVEL DO PARAGRAFO, MAS O AUTOR TEM DADOS EM NIVEL DE PALAVRA TAMBÉM. CONTATE-O PARA SABER MAIS.

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

pacman::p_load(pdftools, 
               tidyverse,
               stringr,
               tidytext,
               purrr,
               scales,
               stopwords,
               viridis, 
               tibble, 
               quanteda,
               quanteda.textstats)

# FUNÇÃO PARA DIAGNOSTICAR QUAIS ELEMENTOS DEVO IR REMOVENDO PARA ALIVIAR OS SLOTS DA RAM
# ESSA PROBLEMATICA NASCE EM FUNÇÃO DE A COMPOSIÇÃO DO CORPUS DAS LEIS ORÇAMENTÁRIAS ANUAIS SEREM ALTAMENTE DENSOS, E DEMANDA DE UMA ALTA CAPACIDADE DE PROCESSAMENTO
# PARA ESSE PROJETO EM ESPECIFICO COMO REQUISITOS MINIMOS RECOMENDA-SE 16 GM [RAM] E i5

objetos_memoria <- ls()

tamanho_objetos <- sapply(objetos_memoria, function(x) {
  object.size(get(x))
})

diagnostico_memoria <- data.frame(
  objeto = objetos_memoria,
  classe = sapply(objetos_memoria, function(x) class(get(x))[1]),
  tamanho_MB = round(tamanho_objetos / 1024^2, 2)
) |> 
  arrange(desc(tamanho_MB))

diagnostico_memoria

#-----------------------------------------------------------
# 1. Caminho para a pasta com os PDFs
#-----------------------------------------------------------

pasta <- "C://Users//ryall//Documents//transversalidade//LOA"
arquivos <- list.files(pasta, pattern = "\\.pdf$", full.names = TRUE)


#-----------------------------------------------------------
# 2. Função para extrair ano do nome do PDF
#-----------------------------------------------------------
extrair_ano <- function(nome) {
  str_extract(nome, "\\d{4}")
}

#-----------------------------------------------------------
# 3. Ler PDFs e transformar em parágrafos
#-----------------------------------------------------------

ler_pdf_seguro <- possibly(
  function(arq) {
    texto_bruto <- pdf_text(arq)
    paste(texto_bruto, collapse = "\n")
  },
  otherwise = NA_character_
)

resultado <- map_df(arquivos, function(arq) {
  
  nome_arquivo <- basename(arq)
  ano <- extrair_ano(nome_arquivo)
  
  texto_completo <- ler_pdf_seguro(arq)
  
  # Se falhou, retorna linha informando erro
  if (is.na(texto_completo)) {
    return(tibble(
      arquivo = nome_arquivo,
      ano = ano,
      paragrafo_id = NA_integer_,
      texto = NA_character_
    ))
  }
  
  paragrafos <- unlist(str_split(texto_completo, "\\n\\s*\\n"))
  
  tibble(
    arquivo = nome_arquivo,
    ano = ano,
    paragrafo_id = seq_along(paragrafos),
    texto = str_squish(paragrafos)
  )
})

readr::write_csv(
  resultado,
  "C:/Users/ryall/Downloads/loa_preprocessado.csv",
  na = "",
  quote = "needed"
)

# NESSE SCRIPT VOU FAZENDO UNS CHECKPOINTS, PORQUE ACABA QUE TEM-SE MUITO CALCULOS MATRICIAIS E AO EXECUTAR O COMANDO MAIS DE UMA VEZ PODE CONTAMINAR O O OBJETO EM MANIPULAÇÃO, E MUITO DOS COMANDOS DEMORAM A SER EXECUTADOS. A VOCÊ USUÁRIO DESSE SCRIPT SE ATENTE A QUESTÕES COMO ESSA

resultado <- readr::read_csv(
  "C:/Users/ryall/Downloads/dados em paragrafos/loa_preprocessado.csv",
  show_col_types = FALSE
)


#-----------------------------------------------------------
# 4. Stopwords em português
#-----------------------------------------------------------

stopwords_pt <- stopwords::stopwords("pt", source = "snowball")
stopwords_pt <- as.data.frame(stopwords_pt)

#-----------------------------------------------------------
# 5. Limpeza dos parágrafos
#-----------------------------------------------------------

resultado_limpo <- resultado %>%
  mutate(
    texto_limpo = str_to_lower(texto),
    texto_limpo = str_replace_all(texto_limpo, "[[:punct:]]", " "),
    texto_limpo = str_replace_all(texto_limpo, "[0-9]", " "),
    texto_limpo = str_replace_all(texto_limpo, "[^[:alnum:]\\s]", " "),
    texto_limpo = str_squish(texto_limpo)
  ) %>%
  mutate(
    texto_limpo = map_chr(texto_limpo, function(t) {
      palavras <- str_split(t, "\\s+")[[1]]
      palavras <- palavras[!(palavras %in% stopwords_pt$stopwords_pt)]
      palavras <- palavras[nchar(palavras) >= 4]
      str_squish(paste(palavras, collapse = " "))
    })
  )

# resultado_limpo <- resultado_limpo[, -4]

# RECOMENDO EXECUTAR O CODIGO ACIMA PARA REMOVER A COLUNA DE TEXTOS NAO PROCESSADOS, PORQUE ISSO DIMINUI DRASTICAMENTE O PESO DO OBJETO NA MEMÓRIA, E A LIBERAÇÃO DE CADA SLOT DE MEMORIA É IMPORTANTE NESSE PROJETO


#-----------------------------------------------------------
# 6. Ler léxico
#-----------------------------------------------------------
# NOTAS METODOLÓGICAS: A ELABORAÇÃO DO SEGUINTE LEXICO DEU-SE DA SEGUINTE FORMA: FORAM SELECIONADAS PAGINAS ALEATORIAS USANDO SAMPLE AQUI NO R, A FIM DE COLHER 30 AMOSTRAS DE CADA DOCUMENTO OFICAL A PARTIR DA LEITURA QUALITATIVA, POSTERIORMENTE MANUALMENTE FORA-SE DANDO VALORES AO PESO DE CASA PALAVRA DADO O NOSSO O OBJETO DE ANÁLISE

lexico <- read.csv("https://raw.githubusercontent.com/ryallmeida/transversalidade/refs/heads/main/wanda/preprocessamento/dataframes/lexico_dic.csv.csv", encoding = "UTF-8")

# trocando virgula para ponto como separador decimal, e fazendo conversão de variavel
lexico$intensidade <- lapply( lexico$intensidade, 
                              function(x) { as.numeric(gsub(",", ".", x)) })
lexico_unique <- lexico %>% distinct(termo, 
                                     .keep_all = TRUE)
lexico_unique$intensidade <- as.double(lexico_unique$intensidade)

#-----------------------------------------------------------
# 7. Tokenização: transformar parágrafos em palavras
#-----------------------------------------------------------

dados_tokens <- resultado_limpo %>%
  unnest_tokens(word, texto_limpo)

# PARA A FORMUALAÇÃO DO CORPUS EM NIVEL DE PARAGRAFO RECOMENDA-SE USAR O OBJETO dados_tokens

dados_tokens <- dados_tokens %>%
  mutate(
    texto_limpo = str_to_lower(texto),
    texto_limpo = str_replace_all(texto_limpo, "[[:punct:]]", " "),
    texto_limpo = str_replace_all(texto_limpo, "[0-9]", " "),
    texto_limpo = str_replace_all(texto_limpo, "[^[:alnum:]\\s]", " "),
    texto_limpo = str_squish(texto_limpo)
  ) %>%
  mutate(
    texto_limpo = map_chr(texto_limpo, function(t) {
      palavras <- str_split(t, "\\s+")[[1]]
      palavras <- palavras[!(palavras %in% stopwords_pt$stopwords_pt)]
      palavras <- palavras[nchar(palavras) >= 4]
      str_squish(paste(palavras, collapse = " "))
    })
  )


# A PARTIR DAQUI O USUÁRIO DEVE TORMAR UMA DECISÃO SE QUER O CORPUS EM NIVEL DE PALAVRA OU EM NÍVEL DE PARAGRAFO

# =============================================================================
# CORPUS EM NIVEL DA PALAVRA
# =============================================================================

#-----------------------------------------------------------
# 8. Juntar tokens com o léxico anteriormente carregado
#-----------------------------------------------------------
# Tokenização: transformar parágrafos em palavras


dados_sentimento <- dados_tokens %>%
  left_join(lexico_unique, by = c("word" = "termo"))

# converter intensidade para numérica
dados_sentimento <- dados_sentimento %>%
  mutate(intensidade = as.numeric(intensidade),
         ano = as.numeric(ano))

# como estamos lidando com um banco de dados para analises mais complexas achei de bom tom incluir para termos de análise/curiosidade variáveis estatisticas como TF e TF-IDF

# ------------------------------------------------------------------------------
# EXTRAINDO UM DATAFRAME DO TF
# ------------------------------------------------------------------------------

corp_sent <- quanteda::corpus(
  dados_sentimento,
  text_field = "texto_limpo"
)

dfm_tf <- corp_sent |>
  tokens(
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_separators = TRUE
  ) |>
  tokens_remove(stopwords("portuguese")) |>
  dfm()

tokens_loa <- dados_sentimento$texto_limpo |>
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) |>
  tokens_remove(stopwords("portuguese"))

dfm_tf <- dfm(tokens_loa)

ranking_tf <- quanteda.textstats::textstat_frequency(dfm_tf) |>
  as.data.frame() |>
  arrange(desc(frequency)) |>
  mutate(rank_tf = row_number())


dados_sentimento <- dados_sentimento %>%
  left_join(
    ranking_tf %>% select(feature, frequency),
    by = c("word" = "feature")
  )

dados_sentimento <- dados_sentimento %>%
  rename(DFM = frequency)

# TRATANDO NAS COM ZERO

dados_sentimento$DFM <- dados_sentimento$DFM %>%
  mutate(replace_na(dados_sentimento$DFM, 0))

# SALVO ATE AQUI, PORQUE PAR EXTRAIR O TF-IDF VAI SER BABADO
# write.csv(dados_sentimento, "C:/Users/ryall/Downloads/loa_preprocessado(2).csv")


# ------------------------------------------------------------------------------
# EXTRAINDO A SOMA DAS INTENSIDADES DADOS O LEXICO PRE-ELABORADO
# ------------------------------------------------------------------------------

dados_tokens <- dados_tokens %>% 
  dplyr::select(-texto)

corp <- corpus(dados_tokens, 
               text_field = "texto_limpo")

dfm_texto <- corp %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE
  ) %>%
  tokens_tolower() %>%
  dfm()

lexico_tratado <- lexico_unique %>% group_by(termo) %>% summarise( intensidade = sum(intensidade, na.rm = TRUE), .groups = "drop" )

lexico_vec <- lexico_tratado %>%
  deframe()

dfm_lexico <- dfm_select(dfm_texto, pattern = names(lexico_vec))

sentimento_total <- as.numeric(dfm_lexico %*% lexico_vec[colnames(dfm_lexico)])

dados_sentimento$sentimento_total <- sentimento_total
dados_tokens$intensidades <- sentimento_total

# TRANSFORMANDO EM UM BANCO DE DADOS SEM OBSERVAÇÕES DUPLICADAS A FIM DE DIMINUIR O PESO DO BANCO DE DADOS NAS MEMORIAS

dados_tokens_unico <- dados_tokens %>%
  group_by(paragrafo_id) %>%
  summarise(
    intensidade_media = mean(intensidades, na.rm = TRUE),
    .groups = "drop"
  )

resultado_limpo <- resultado_limpo %>%
  left_join(
    dados_tokens_unico,
    by = "paragrafo_id"
  )

resultado_limpo <- resultado_limpo %>%
  mutate(
    intensidade_media = replace_na(intensidade_media, 0)
  )

resultado_limpo <- resultado_limpo %>%
  mutate(
    intensidades = label_final * intensidade_media
  )

# ------------------------------------------------------------------------------
# AQUI COM O AUXILIO DO CHATGPT E DO GEMINI TENTO CONSTRUIR UM ALGORITMO QUE NAO ESTOURE MINHA RAM

dfm_tfidf <- dfm_tfidf(dfm_tf)

lexico_vec <- lexico_tratado %>%
  select(termo, intensidade) %>%
  deframe()

dfm_tfidf_lexico <- dfm_select(
  dfm_tfidf,
  pattern = names(lexico_vec)
)

sentimento_tfidf <- as.numeric(
  dfm_tfidf_lexico %*% lexico_vec[colnames(dfm_tfidf_lexico)]
)

tfidf_total <- rowSums(dfm_tfidf)
tfidf_medio <- rowMeans(dfm_tfidf)

sentimento_tfidf <- as.numeric(
  dfm_select(dfm_tfidf, pattern = names(lexico_vec)) %*%
    lexico_vec[colnames(dfm_select(dfm_tfidf, pattern = names(lexico_vec)))]
)

# --------------------------------------
# JA FIZ ESSES PROCESSOS, MAS TO FAZENDO DE NOVO POR ALGUM MOTIVO AS MATRIZES NAO BATEM ENTÃO O CALCULO MATRICIAL NAO FUNCIONA

dados_tokens <- dados_tokens %>%
  distinct(paragrafo_id, texto_limpo, ano, arquivo)

corp_sent <- corpus(
  dados_tokens,
  text_field = "texto_limpo"
)

tokens_sent <- tokens(
  corp_sent,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE
) |>
  tokens_tolower() |>
  tokens_remove(stopwords("portuguese"))

dfm_tf <- dfm(tokens_sent)
dfm_tfidf <- dfm_tfidf(dfm_tf)
tfidf_total <- rowSums(dfm_tfidf)
tfidf_medio <- rowMeans(dfm_tfidf)

lexico_vec <- lexico_tratado %>%
  select(termo, intensidade) %>%
  deframe()

dfm_lexico <- dfm_select(dfm_tf, pattern = names(lexico_vec))

sentimento_total2 <- as.numeric(
  dfm_lexico %*% lexico_vec[colnames(dfm_lexico)]
)

dfm_tfidf_lex <- dfm_select(dfm_tfidf, pattern = names(lexico_vec))

sentimento_tfidf <- as.numeric(
  dfm_tfidf_lex %*% lexico_vec[colnames(dfm_tfidf_lex)]
)

# SE VC DESCER MAIS UM POUCOQUINHO VC ACHA O RESTO DESSE CODIGO
dados_doc <- dados_documento %>%
  distinct(paragrafo_id, .keep_all = TRUE) %>%
  mutate(
    tfidf_total = tfidf_total,
    tfidf_medio = tfidf_medio,
    sentimento_total2 = sentimento_total2,
    sentimento_tfidf = sentimento_tfidf
  )

dados_sentimento <- dados_sentimento %>%
  left_join(
    dados_doc %>%
      select(
        paragrafo_id,
        tfidf_total,
        tfidf_medio,
        sentimento_total2,
        sentimento_tfidf
      ),
    by = "paragrafo_id"
  )

dados_sentimento <- dados_sentimento %>%
  rename(TF_IDF = tfidf_total,
         intensidades = sentimento_total,
         soma_intensidades = sentimento_total2)

dados_sentimento <- dados_sentimento %>%
  mutate(
    tfidf_intensidade1 = TF_IDF * intensidades,
    tfidf_intensidade2 = TF_IDF * soma_intensidades
  )


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -----------------------------
# 1. Base única de documentos
# -----------------------------
dados_documento <- dados_tokens %>%
  distinct(paragrafo_id, texto_limpo, ano, arquivo)

# -----------------------------
# 2. Corpus ÚNICO
# -----------------------------
corp_sent <- corpus(
  dados_documento,
  text_field = "texto"
)

# -----------------------------
# 3. Tokens ÚNICOS
# -----------------------------
tokens_sent <- tokens(
  corp_sent,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE
) |>
  tokens_tolower() |>
  tokens_remove(stopwords("portuguese"))

# -----------------------------
# 4. DFM TF (VERDADE ÚNICA)
# -----------------------------
dfm_tf <- dfm(tokens_sent)
rm(tokens_sent); gc()

# -----------------------------
# 5. Léxico
# -----------------------------
lexico_vec <- lexico_tratado %>%
  select(termo, intensidade) %>%
  deframe()

# -----------------------------
# 6. TF-IDF (UMA VEZ)
# -----------------------------
dfm_tfidf <- dfm_tfidf(dfm_tf)

tfidf_total <- rowSums(dfm_tfidf)
tfidf_medio <- rowMeans(dfm_tfidf)

# -----------------------------
# 7. Sentimento TF
# -----------------------------
dfm_tf_lex <- dfm_select(dfm_tf, pattern = names(lexico_vec))

sentimento_total2 <- as.numeric(
  dfm_tf_lex %*% lexico_vec[colnames(dfm_tf_lex)]
)

# -----------------------------
# 8. Sentimento TF-IDF
# -----------------------------
dfm_tfidf_lex <- dfm_select(dfm_tfidf, pattern = names(lexico_vec))

sentimento_tfidf <- as.numeric(
  dfm_tfidf_lex %*% lexico_vec[colnames(dfm_tfidf_lex)]
)

rm(dfm_tf, dfm_tfidf, dfm_tf_lex, dfm_tfidf_lex); gc()

# PARA CONSEGUIR UNIR AO dados_doc AO BANCO DE DADOS resultado_limpo DEVO GARANTIR QUE A VARIAVEL DE ID SEJA UNICA

dados_doc_unico <- dados_doc %>%
  group_by(paragrafo_id) %>%
  summarise(
    tfidf_total = mean(tfidf_total, na.rm = TRUE),
    tfidf_medio = mean(tfidf_medio, na.rm = TRUE),
    sentimento_total2 = mean(sentimento_total2, na.rm = TRUE),
    sentimento_tfidf = mean(sentimento_tfidf, na.rm = TRUE),
    .groups = "drop"
  )

resultado_limpo <- resultado_limpo %>%
  left_join(
    dados_doc_unico,
    by = "paragrafo_id"
  )



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# -----------------------------------------------------------
# CLASSIFICANDO OS DADOS, SEM MANIPULAR PELA SOMA DAS INTENSIDADES
# -----------------------------------------------------------

dados_sentimento <- dados_sentimento %>%
  mutate(
    tipo1 = case_when(
      label_final > 0  ~ "POSITIVO",
      label_final == 0 ~ "NEUTRO",
      label_final < 0  ~ "NEGATIVO",
      TRUE ~ tipo  # mantém o valor antigo se houver NA em label_final
    )
  )

prop.table(table(dados_sentimento$tipo1))
# NEGATIVO    NEUTRO  POSITIVO 
#0.4265754 0.2143105 0.3591140 

# SEGUNDA TENTATIVA

prop.table(table(dados_sentimento$tipo1))

#  NEGATIVO    NEUTRO  POSITIVO 
# 0.1208906 0.5176700 0.3614394

# -----------------------------------------------------------
# CLASSIFICANDO OS DADOS, MANIPULANDO PELA SOMA DAS INTENSIDADES
# -----------------------------------------------------------

dados_sentimento <- dados_sentimento %>%
  rename(intensidades = sentimento_total)

bp <- boxplot.stats(dados_sentimento$intensidades)

lim_inf <- bp$stats[1]
q1      <- bp$stats[2]
med     <- bp$stats[3]
q3      <- bp$stats[4]
lim_sup <- bp$stats[5]

# divisões internas (proporcionais)
neg_q1_med <- seq(q1, med, length.out = 4)  # 3 partes
pos_med_q3 <- seq(med, q3, length.out = 4)

dados_sentimento <- dados_sentimento %>%
  mutate(
    tipo2= case_when(
      
      intensidades < lim_inf ~ "HYPERNEGATIVO",
      intensidades > lim_sup ~ "HYPERPOSITIVO",
      
      intensidades == 0 ~ "NEUTRO",
      
      intensidades >= lim_inf & intensidades < q1 ~ "NEGATIVO_1",
      
      intensidades >= neg_q1_med[1] & intensidades < neg_q1_med[2] ~ "NEGATIVO_2",
      intensidades >= neg_q1_med[2] & intensidades < neg_q1_med[3] ~ "NEGATIVO_3",
      intensidades >= neg_q1_med[3] & intensidades < med           ~ "NEGATIVO_4",
      
      intensidades > med & intensidades < pos_med_q3[2] ~ "POSITIVO_1",
      intensidades >= pos_med_q3[2] & intensidades < pos_med_q3[3] ~ "POSITIVO_2",
      intensidades >= pos_med_q3[3] & intensidades < q3            ~ "POSITIVO_3",
      
      intensidades >= q3 & intensidades <= lim_sup ~ "POSITIVO_4",
      
      TRUE ~ NA_character_
    )
  )

prop.table(table(dados_sentimento$tipo2))

# HYPERNEGATIVO HYPERPOSITIVO    NEGATIVO_1        NEUTRO    POSITIVO_1 
#   0.007035925   0.110946817   0.103931563   0.499808893   0.018013792 
#    POSITIVO_2    POSITIVO_3    POSITIVO_4 
#   0.079951704   0.034479747   0.145831558 

# SEGUNDA TENTATIVA
(table(dados_sentimento$tipo2))
# HYPERNEGATIVO HYPERPOSITIVO    NEGATIVO_1    NEGATIVO_2    NEGATIVO_3 
#  0.0002133187  0.1055269417  0.1448333139  0.0519701689  0.0775271070 
#    NEGATIVO_4        NEUTRO    POSITIVO_1    POSITIVO_2    POSITIVO_3 
#  0.0716588935  0.1545226018  0.0915175171  0.0928792882  0.0604323980 
#    POSITIVO_4 
# 0.1489184512 

# -----------------------------------------------------------
# CLASSIFICANDO OS DADOS, ESTRESSANDO A VARIAVEL PELO TF-IDF
# -----------------------------------------------------------

dados_sentimento <- dados_sentimento %>%
  rename(tfidf_intensidade1  = sentimento_tfidf)

bp2 <- boxplot.stats(dados_sentimento$tfidf_intensidade1)

lim_inf <- bp2$stats[1]
q1      <- bp2$stats[2]
med     <- bp2$stats[3]
q3      <- bp2$stats[4]
lim_sup <- bp2$stats[5]

neg_q1_med <- seq(q1, med, length.out = 4)  # 3 partes
pos_med_q3 <- seq(med, q3, length.out = 4)

dados_sentimento <- dados_sentimento %>%
  mutate(
    tipo3 = case_when(
      
      tfidf_intensidade1 < lim_inf ~ "HYPERNEGATIVO",
      tfidf_intensidade1 > lim_sup ~ "HYPERPOSITIVO",
      
      tfidf_intensidade1 == 0 ~ "NEUTRO",
      
      tfidf_intensidade1 >= lim_inf & tfidf_intensidade1 < q1 ~ "NEGATIVO_1",
      
      tfidf_intensidade1 >= neg_q1_med[1] & tfidf_intensidade1 < neg_q1_med[2] ~ "NEGATIVO_2",
      tfidf_intensidade1 >= neg_q1_med[2] & tfidf_intensidade1 < neg_q1_med[3] ~ "NEGATIVO_3",
      tfidf_intensidade1 >= neg_q1_med[3] & tfidf_intensidade1 < med           ~ "NEGATIVO_4",
      
      tfidf_intensidade1 > med & tfidf_intensidade1 < pos_med_q3[2] ~ "POSITIVO_1",
      tfidf_intensidade1 >= pos_med_q3[2] & tfidf_intensidade1 < pos_med_q3[3] ~ "POSITIVO_2",
      tfidf_intensidade1 >= pos_med_q3[3] & tfidf_intensidade1 < q3            ~ "POSITIVO_3",
      
      tfidf_intensidade1 >= q3 & tfidf_intensidade1 <= lim_sup ~ "POSITIVO_4",
      
      TRUE ~ NA_character_
    )
  )

prop.table(table(dados_sentimento$tipo3))


dados_sentimento <- dados_sentimento %>%
  rename(sentimento_tfidf = tfidf_intensidade1)


# HYPERNEGATIVO HYPERPOSITIVO    NEGATIVO_1        NEUTRO    POSITIVO_1 
#    0.01590487    0.16531556    0.09424461    0.50440979    0.05303648 
#    POSITIVO_2    POSITIVO_3    POSITIVO_4 
#    0.04864905    0.03373684    0.08470279 

# SEGUNDA TENTATIVA
prop.table(table(dados_sentimento$tipo3))

# HYPERNEGATIVO HYPERPOSITIVO    NEGATIVO_1        NEUTRO    POSITIVO_1 
#   0.002204318   0.130036919   0.184956156   0.398708223   0.103976108 
#    POSITIVO_2    POSITIVO_3    POSITIVO_4 
#   0.031254151   0.023245947   0.125618179 

readr::write_csv(
  dados_sentimento,
  "C:/Users/ryall/Downloads/loa_preprocessado(3).csv",
  na = "",
  quote = "needed"
)

#-----------------------------------------------------------
# 9. Resumo por ano
#-----------------------------------------------------------

# ACHO QUE NAO DELIMITEI BEM AS LINHAS E A VARIAVEL ANO TA CONTAMINDA COM TEXTO
# TUDO INDICA QUE VOU TER QUE RODAR DENOVO ESSE CODIGO AMANHÃ

# MINERANDO ESSES DADOS PARA RODAR O SCRIPT QUE SEGUE, MAS MAS TODO MODO VOU TER QUE RODAR TUDO DE NOVO, MAS MEIO CAMINHO JA FOI ANDADO

dados_sentimento <- dados_sentimento %>%
  mutate(
    ano = suppressWarnings(as.integer(ano))
  )

teste <- dados_sentimento %>%
  filter(!is.na(ano))


# -----------------------------------

dados_sentimento_resumo <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    matched_tokens = sum(!is.na(intensidades)),
    sum_polarity   = sum(intensidades, na.rm = TRUE),
    mean_matched   = ifelse(matched_tokens == 0, 0, sum_polarity / matched_tokens),
    mean_all       = mean(replace_na(intensidades, 0)),
    .groups = "drop"
  )

view(dados_sentimento_resumo)


# --- 10. Série temporal anual---
serie_temporal <- dados_sentimento_resumo %>%
  group_by(ano) %>%
  summarise(
    sentimento_diario_mean   = mean(mean_matched, na.rm = TRUE),   # intensidade média
    sentimento_diario_total  = sum(sum_polarity, na.rm = TRUE),    # magnitude bruta
    .groups = "drop"
  )

# --- 11. Normalização e suavização ---
max_abs <- max(abs(serie_temporal$sentimento_diario_total), na.rm = TRUE)

serie_temporal <- serie_temporal %>%
  mutate(
    sent_norm   = ifelse(max_abs == 0, 0, sentimento_diario_total / max_abs),
    sent_smooth = zoo::rollmean(sent_norm, k = 3, fill = NA, align = "right") # média móvel 3 dias
  )


dados_sentimento_resumo <- dados_sentimento_resumo %>%
  left_join(
    serie_temporal,
    by = "ano"
  )


readr::write_csv(
  dados_sentimento_resumo,
  "C:/Users/ryall/Documents/transversalidade/DATASETS/resumo_loa.csv",
  na = "",
  quote = "needed"
)

