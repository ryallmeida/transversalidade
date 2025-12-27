# TRATAMENTO DO CORPUS: ANÁLISE DAS LEIS ORCAMENTÁRIAS ANUAIS [LOA] DE PERNAMBUCO 
# PERÍODO (2008-2025)
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

# ----------------------------------------
#FAZENDO O UPLOAD DE BIBLIOTECAS 
# ----------------------------------------
set.seed(123)

if(!require(pacman)){
  install.packages("pacman") 
}

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

# DIAGNOZE DA RAM
# ==============================================================================

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

# ==============================================================================

pasta <- "C://Users//ryall//Documents//transversalidade//LOA"
arquivos <- list.files(pasta, pattern = "\\.pdf$", full.names = TRUE)

extrair_ano <- function(nome) {
  str_extract(nome, "\\d{4}")
}

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

stopwords_pt <- stopwords::stopwords("pt", source = "snowball")
stopwords_pt <- as.data.frame(stopwords_pt)

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


dic2_transversalidade <- resultado_limpo %>%
  mutate(label=ifelse(grepl("integrar | integração | intersetorial | articulação | transversal | multidisciplinar | interinstitucional | cooperação | coordenação | consórcio | parceria | colaborativo | convergência | municípios | estados | união | entes federados | instâncias | interfederativo | descentralização | governança | colegiado | comitê gestor | conselhos | pactuação | corresponsabilidade | plano integrado | orçamento integrado | matriz intersetorial | planejamento conjunto | agenda comum | metas compartilhadas | indicadores integrados | programas estruturantes | iniciativas conjuntas | financiamento integrado | participação social | controle social | audiências públicas | consulta pública | escuta ativa | transparência | sociedade civil | deliberação | interseccionalidade | equidade | inclusão | grupos vulneráveis | promoção da igualdade | combate às desigualdades | ações afirmativas | direitos humanos | diversidade | justiça social | promover | executar | garantir | implantar | manutencao | fortalecer | desenvolver | ampliacao | crescimento | elevar | transferencia | atender | beneficios | realizar | ampliar | assegurar | execucao | fornecer | melhorar | promocao | recursos | acesso | atendimento | criacao | exercer | implantacao | manter | melhoria | modernizar | aperfeicoar | colaboracao | concessao | consolidacao | coordenar | desenvolvimento | eficiencia | estimular | executados | fortalecimento | implementar | incrementar | integracao | integrada | integrar | metas | otimizar | pagamento | participacao | participar | recuperacao | transparencia | viabilizar | abranger | abrangera | acao | acessibilidade | acompanhar | adaptavel realidade | agregar | alcancadas | alcance | alocacao | ampliacao cidadania ativa grupos discriminados | ampliacao integracao | ampliar melhoria | aperfeicoamento | aportar | aquisicao | arrecadacao | articulacao | atraente | aumento | avaliacao | capacitar | compensacao | compromisso | conscientizacao | conscientizar | conservar | consolidar | construcao | construir | contribuir | controle social | convenio | convergencia | cooperacao | creditos | custeio | defender | descentralizar | desenvolvidas | desigualdade infraestrutura | determinacao | direcionador | disponibilizar | disponibilizara | divulgar | efetivar | elaboracao", texto, ignore.case = TRUE), 1, -1))


dic2_transversalidade2 <- dic2_transversalidade %>%
  mutate(label2=ifelse(grepl("alienacao | apoio | despesas | limite | reducao | assentamentos | atingida | ausencia | comprometimento | concentrados | contempla | contingenciamento | custo | deficit | densidade | desaceleracao | desemprego | desindustrializacao | devolucao | diminuicao | escolar | especificidades | estagnado | exclusivamente | fragmentacao | impedimento | julgar | limitacao | limitacoes | limites | mediante | morte | nenhuma | possibilita | programa | racionalizar | rever | subregistro | terras | tragedias | vedada | vulneraveis | setor específico | por secretaria | por pasta | atuação isolada | atuação setorial | responsabilidade única | sem articulação | fragmentado | fragmentação | divisão de competências | compartimentalização | autônomo | setorializado | ausência de coordenação | falta de integração | falta de diálogo | não articulado | sem parceria | sem cooperação | desarticulado | sem comunicação | sem sinergia | ações separadas | decisão centralizada | competência exclusiva | centralização | comando único | sem descentralização | autoridade exclusiva | gestão isolada | nível único | sem consulta | unilateral | responsabilidade do município | sem apoio da união | ausência de corresponsabilidade | não pactuado | sem pactuação | sem integração federativa | delegação sem recursos", texto, ignore.case = TRUE), -1, 1))

dic2_final_loa <- dic2_transversalidade2 %>% mutate(label_final = case_when(
  label == 1 & label2 == 1 ~ 1,
  label == 1 & label2 == -1 ~ 0,
  label == -1 & label2 == 1 ~ 0,
  label == -1 & label2 == -1 ~ -1
))

resultado_limpo <- dic2_final_loa

rm(dic2_transversalidade); gc()
rm(dic2_transversalidade2); gc()


# resultado_limpo <- resultado_limpo[, -4]

#-----------------------------------------------------------
# PRIMEIRO CHECKPOINT
#-----------------------------------------------------------

readr::write_csv(
  resultado_limpo,
  "C:/Users/ryall/Downloads/loa_preprocessado(1).csv",
  na = "",
  quote = "needed"
)

resultado_limpo <- readr::read_csv(
  "C:/Users/ryall/Downloads/loa_preprocessado(1).csv",
  show_col_types = FALSE
)

# ==============================================================================
# LEXICO
# ==============================================================================

lexico <- read.csv("https://raw.githubusercontent.com/ryallmeida/transversalidade/refs/heads/main/wanda/preprocessamento/dataframes/lexico_dic.csv.csv", 
                   encoding = "UTF-8")
lexico$intensidade <- lapply( lexico$intensidade, 
                              function(x) { as.numeric(gsub(",", 
                                                            ".", 
                                                            x)) })
lexico_unique <- lexico %>% distinct(termo, 
                                     .keep_all = TRUE)
lexico_unique$intensidade <- as.double(lexico_unique$intensidade)

rm(lexico); gc()

# ==============================================================================
# CORPUS/DATAFRAME EM NIVEL DE PALAVRA
# ==============================================================================

dados_tokens <- resultado_limpo %>%
  unnest_tokens(
    word,
    texto_limpo,
    drop = FALSE
  )

dados_sentimento <- dados_tokens %>%
  left_join(lexico_unique, by = c("word" = "termo"))

dados_sentimento <- dados_sentimento %>%
  mutate(intensidade = as.numeric(intensidade),
         ano = as.numeric(ano))

dados_sentimento <- dados_sentimento %>% 
  dplyr::select(-texto)


# ==============================================================================
# SOMA DAS INTENSIDADES
# ==============================================================================

lexico_tratado <- lexico_unique %>%
  group_by(termo) %>%
  summarise(
    intensidade = sum(intensidade, na.rm = TRUE),
    .groups = "drop"
  )

sentimento_texto <- dados_sentimento %>%
  mutate(id_texto = row_number()) %>%
  
  unnest_tokens(palavra, texto_limpo) %>%
  mutate(palavra = str_to_lower(palavra)) %>%
  
  inner_join(
    lexico_unique %>% rename(soma_intensidades = intensidade),
    by = c("palavra" = "termo")
  ) %>%
  
  group_by(id_texto) %>%
  summarise(
    sentimento_total = sum(.data$soma_intensidades, na.rm = TRUE),
    .groups = "drop"
  )

dados_sentimento <- dados_sentimento %>%
  left_join(
    sentimento_texto %>%
      select(
        id_texto,
        intensidades = sentimento_total
      ),
    by = c("paragrafo_id" = "id_texto")
  )

dados_sentimento <- dados_sentimento %>%
  dplyr::mutate(intensidades = tidyr::replace_na(intensidades, 0))

rm(lexico_unique); gc()


# ==============================================================================
# TF
# ==============================================================================

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

dados_sentimento <- dados_sentimento %>%
  dplyr::mutate(DFM = tidyr::replace_na(DFM, 0))

# ==============================================================================
# TF-IDF EM NIVEL DE PALAVRA
# ==============================================================================

dfm_tf_ano <- dfm_tf |> 
  dfm_group(groups = corp_sent$ano)

dfm_tfidf_ano <- dfm_tf_ano |> dfm_tfidf()

ranking_tfidf <- dfm_tfidf_ano |>
  convert(to = "data.frame")

ranking_tfidf <- ranking_tfidf |>
  select(-doc_id) |>
  summarise(across(everything(), sum)) |>
  pivot_longer(
    cols = everything(),
    names_to = "feature",
    values_to = "tfidf"
  ) |>
  arrange(desc(tfidf)) |>
  mutate(rank_tfidf = row_number())

dados_sentimento <- dados_sentimento %>%
  left_join(
    ranking_tfidf %>% select(feature, tfidf),
    by = c("word" = "feature")
  )

dados_sentimento <- dados_sentimento %>%
  rename(TF_IDF = tfidf)

dados_sentimento <- dados_sentimento %>%
  dplyr::mutate(TF_IDF = tidyr::replace_na(TF_IDF, 0))

# ==============================================================================
# SEGUNDO CHECKPOINT

readr::write_csv(
  dados_sentimento,
  "C:/Users/ryall/Downloads/loa_nivel_palavra.csv",
  na = "",
  quote = "needed"
)

dados_sentimento <- readr::read_csv(
  "C:/Users/ryall/Downloads/CORPUS/loa_processado_npalavra.csv",
  show_col_types = FALSE
)

# ==============================================================================
# AGRUPADOR DE DADOS VIA VARIAVEL ID
# ==============================================================================

loa_sumarizado <- dados_sentimento %>%
  group_by(paragrafo_id) %>%
  summarise(
    DFM_medio = mean(DFM, na.rm = TRUE),
    tfidf_medio = mean(TF_IDF, na.rm = TRUE),
    soma_media_intensidades = mean(intensidades, na.rm = TRUE),
    .groups = "drop"
  )

loa_nivel_paragrafo <- resultado_limpo %>%
  left_join(
    loa_sumarizado,
    by = "paragrafo_id"
  )

readr::write_csv(
  loa_nivel_paragrafo,
  "C:/Users/ryall/Downloads/loa_nivel_paragrafo.csv",
  na = "",
  quote = "needed"
)

loa_nivel_paragrafo <- readr::read_csv(
  "C:/Users/ryall/Downloads/CORPUS/loa_processado_nparagrafo.csv",
  show_col_types = FALSE
)

# ==============================================================================
# TRATAMENTOS
# ==============================================================================


dados_sentimento <- dados_sentimento %>%
  mutate(
    tfidf_intensidade = TF_IDF * intensidades
  )

loa_nivel_paragrafo <- loa_nivel_paragrafo %>%
  mutate(
    tfidf_intensidade = tfidf_medio * soma_media_intensidades
  )

# ==============================================================================
# CLASSIFICANDO OS DADOS, SEM MANIPULAR PELA SOMA DAS INTENSIDADES
# ==============================================================================

dados_sentimento <- dados_sentimento %>%
  mutate(
    tipo1 = case_when(
      label_final > 0  ~ "POSITIVO",
      label_final == 0 ~ "NEUTRO",
      label_final < 0  ~ "NEGATIVO",
      TRUE ~ NA_character_
    )
  )

prop.table(table(dados_sentimento$tipo1))
#  NEGATIVO    NEUTRO  POSITIVO 
# 0.1229632 0.5257967 0.3512401 


loa_nivel_paragrafo <- loa_nivel_paragrafo %>%
  dplyr::mutate(
    tipo1 = dplyr::case_when(
      label_final > 0  ~ "POSITIVO",
      label_final == 0 ~ "NEUTRO",
      label_final < 0  ~ "NEGATIVO",
      TRUE ~ NA_character_
    )
  )

prop.table(table(loa_nivel_paragrafo$tipo1))

#  NEGATIVO    NEUTRO  POSITIVO 
# 0.1785235 0.6125247 0.2089518  

# ==============================================================================
# CLASSIFICANDO OS DADOS, MANIPULANDO A SOMA DAS INTENSIDADES
# ==============================================================================

bp <- boxplot.stats(dados_sentimento$intensidades)

lim_inf <- bp$stats[1]
q1      <- bp$stats[2]
med     <- bp$stats[3]
q3      <- bp$stats[4]
lim_sup <- bp$stats[5]

neg_q1_med <- seq(q1, med, length.out = 4)  
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
#  HYPERPOSITIVO    NEGATIVO_1    NEGATIVO_2    NEGATIVO_3    NEGATIVO_4 
#    0.187835019   0.050116123   0.007593635   0.032612308   0.029640928 
#         NEUTRO    POSITIVO_1    POSITIVO_2    POSITIVO_3    POSITIVO_4 
#    0.447905830   0.030597580   0.020531323   0.069507145   0.123660110 

bp <- boxplot.stats(loa_nivel_paragrafo$soma_media_intensidades)

lim_inf <- bp$stats[1]
q1      <- bp$stats[2]
med     <- bp$stats[3]
q3      <- bp$stats[4]
lim_sup <- bp$stats[5]


neg_q1_med <- seq(q1, med, length.out = 4)  
pos_med_q3 <- seq(med, q3, length.out = 4)

loa_nivel_paragrafo <- loa_nivel_paragrafo %>%
  mutate(
    tipo2= case_when(
      
      soma_media_intensidades < lim_inf ~ "HYPERNEGATIVO",
      soma_media_intensidades > lim_sup ~ "HYPERPOSITIVO",
      
      soma_media_intensidades == 0 ~ "NEUTRO",
      
      soma_media_intensidades >= lim_inf & soma_media_intensidades < q1 ~ "NEGATIVO_1",
      
      soma_media_intensidades >= neg_q1_med[1] & soma_media_intensidades < neg_q1_med[2] ~ "NEGATIVO_2",
      soma_media_intensidades >= neg_q1_med[2] & soma_media_intensidades< neg_q1_med[3] ~ "NEGATIVO_3",
      soma_media_intensidades >= neg_q1_med[3] & soma_media_intensidades < med           ~ "NEGATIVO_4",
      
      soma_media_intensidades > med & soma_media_intensidades < pos_med_q3[2] ~ "POSITIVO_1",
      soma_media_intensidades >= pos_med_q3[2] & soma_media_intensidades < pos_med_q3[3] ~ "POSITIVO_2",
      soma_media_intensidades >= pos_med_q3[3] & soma_media_intensidades < q3            ~ "POSITIVO_3",
      
      soma_media_intensidades >= q3 & soma_media_intensidades <= lim_sup ~ "POSITIVO_4",
      
      TRUE ~ NA_character_
    )
  )

prop.table(table(loa_nivel_paragrafo$tipo2))

# HYPERPOSITIVO    NEGATIVO_1    NEGATIVO_2    NEGATIVO_3    NEGATIVO_4 
#   0.231575402   0.046347155   0.007216685   0.031272302   0.018202083 
#        NEUTRO    POSITIVO_1    POSITIVO_2    POSITIVO_3    POSITIVO_4 
#   0.409490743   0.166432793   0.009253394   0.008499651   0.071709793 

# ==============================================================================
# CLASSIFICANDO OS DADOS, ESTRESSANDO A VARIAVEL PELO TF-IDF
# ==============================================================================

bp2 <- boxplot.stats(dados_sentimento$tfidf_intensidade)

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
      
      tfidf_intensidade < lim_inf ~ "HYPERNEGATIVO",
      tfidf_intensidade > lim_sup ~ "HYPERPOSITIVO",
      
      tfidf_intensidade == 0 ~ "NEUTRO",
      
      tfidf_intensidade >= lim_inf & tfidf_intensidade < q1 ~ "NEGATIVO_1",
      
      tfidf_intensidade >= neg_q1_med[1] & tfidf_intensidade < neg_q1_med[2] ~ "NEGATIVO_2",
      tfidf_intensidade >= neg_q1_med[2] & tfidf_intensidade < neg_q1_med[3] ~ "NEGATIVO_3",
      tfidf_intensidade >= neg_q1_med[3] & tfidf_intensidade < med           ~ "NEGATIVO_4",
      
      tfidf_intensidade > med & tfidf_intensidade < pos_med_q3[2] ~ "POSITIVO_1",
      tfidf_intensidade >= pos_med_q3[2] & tfidf_intensidade < pos_med_q3[3] ~ "POSITIVO_2",
      tfidf_intensidade >= pos_med_q3[3] & tfidf_intensidade < q3            ~ "POSITIVO_3",
      
      tfidf_intensidade >= q3 & tfidf_intensidade <= lim_sup ~ "POSITIVO_4",
      
      TRUE ~ NA_character_
    )
  )

prop.table(table(dados_sentimento$tipo3))

# HYPERNEGATIVO HYPERPOSITIVO        NEUTRO 
#   0.003012313   0.077400090   0.919587597 

bp2 <- boxplot.stats(loa_nivel_paragrafo$tfidf_intensidade)

lim_inf <- bp2$stats[1]
q1      <- bp2$stats[2]
med     <- bp2$stats[3]
q3      <- bp2$stats[4]
lim_sup <- bp2$stats[5]

neg_q1_med <- seq(q1, med, length.out = 4)  # 3 partes
pos_med_q3 <- seq(med, q3, length.out = 4)

loa_nivel_paragrafo <- loa_nivel_paragrafo %>%
  mutate(
    tipo3 = case_when(
      
      tfidf_intensidade < lim_inf ~ "HYPERNEGATIVO",
      tfidf_intensidade > lim_sup ~ "HYPERPOSITIVO",
      
      tfidf_intensidade == 0 ~ "NEUTRO",
      
      tfidf_intensidade >= lim_inf & tfidf_intensidade < q1 ~ "NEGATIVO_1",
      
      tfidf_intensidade >= neg_q1_med[1] & tfidf_intensidade < neg_q1_med[2] ~ "NEGATIVO_2",
      tfidf_intensidade >= neg_q1_med[2] & tfidf_intensidade < neg_q1_med[3] ~ "NEGATIVO_3",
      tfidf_intensidade >= neg_q1_med[3] & tfidf_intensidade < med           ~ "NEGATIVO_4",
      
      tfidf_intensidade > med & tfidf_intensidade < pos_med_q3[2] ~ "POSITIVO_1",
      tfidf_intensidade >= pos_med_q3[2] & tfidf_intensidade < pos_med_q3[3] ~ "POSITIVO_2",
      tfidf_intensidade >= pos_med_q3[3] & tfidf_intensidade < q3            ~ "POSITIVO_3",
      
      tfidf_intensidade >= q3 & tfidf_intensidade <= lim_sup ~ "POSITIVO_4",
      
      TRUE ~ NA_character_
    )
  )

prop.table(table(loa_nivel_paragrafo$tipo3))

# HYPERNEGATIVO HYPERPOSITIVO    NEGATIVO_1    NEGATIVO_2    NEGATIVO_3 
#  0.0001442908  0.1978515104  0.0415557431  0.0076401965  0.0150134551 
#    NEGATIVO_4        NEUTRO    POSITIVO_1    POSITIVO_2    POSITIVO_3 
#  0.0125677265  0.4230749807  0.1899660195  0.0347307895  0.0252581001 
#    POSITIVO_4 
#  0.0521971878 

# ==============================================================================
# PROPORÇÃO DAS CATEGORIAS
# ==============================================================================

resumo_prop <- function(df, var, nivel) {
  prop.table(table(df[[var]])) %>%
    as.data.frame() %>%
    rename(
      classe = Var1,
      proporcao = Freq
    ) %>%
    mutate(
      tipo = var,
      nivel_corpus = nivel
    )
}

resultado_final <- bind_rows(
  resumo_prop(dados_sentimento, "tipo1", "palavra"),
  resumo_prop(dados_sentimento, "tipo2", "palavra"),
  resumo_prop(dados_sentimento, "tipo3", "palavra"),
  resumo_prop(loa_nivel_paragrafo, "tipo1", "paragrafo"),
  resumo_prop(loa_nivel_paragrafo, "tipo2", "paragrafo"),
  resumo_prop(loa_nivel_paragrafo, "tipo3", "paragrafo")
)

resultado_final$documento <- "LOA"

readr::write_csv(
  resultado_final,
  "C:/Users/ryall/Downloads/proporcao_loa.csv",
  na = "",
  quote = "needed"
)



# ==============================================================================
# QUANTIFICANDO EM ESPECTRO
# ==============================================================================

loa_nivel_paragrafo <- loa_nivel_paragrafo %>%
  mutate(
    espectro_tipo2 = dplyr::recode(
      tipo2,
      "HYPERNEGATIVO" = -6,
      "NEGATIVO_4"    = -5,
      "NEGATIVO_3"    = -4,
      "NEGATIVO_2"    = -3,
      "NEGATIVO_1"    = -2,
      "NEUTRO"        =  0,
      "POSITIVO_1"    =  2,
      "POSITIVO_2"    =  3,
      "POSITIVO_3"    =  4,
      "POSITIVO_4"    =  5,
      "HYPERPOSITIVO" =  6,
      .default = NA_real_
    )
  )

loa_nivel_paragrafo <- loa_nivel_paragrafo %>%
  mutate(
    espectro_tipo3 = dplyr::recode(
      tipo3,
      "HYPERNEGATIVO" = -6,
      "NEGATIVO_4"    = -5,
      "NEGATIVO_3"    = -4,
      "NEGATIVO_2"    = -3,
      "NEGATIVO_1"    = -2,
      "NEUTRO"        =  0,
      "POSITIVO_1"    =  2,
      "POSITIVO_2"    =  3,
      "POSITIVO_3"    =  4,
      "POSITIVO_4"    =  5,
      "HYPERPOSITIVO" =  6,
      .default = NA_real_
    )
  )

dados_sentimento <- dados_sentimento %>%
  mutate(
    espectro_tipo2 = dplyr::recode(
      tipo2,
      "HYPERNEGATIVO" = -6,
      "NEGATIVO_4"    = -5,
      "NEGATIVO_3"    = -4,
      "NEGATIVO_2"    = -3,
      "NEGATIVO_1"    = -2,
      "NEUTRO"        =  0,
      "POSITIVO_1"    =  2,
      "POSITIVO_2"    =  3,
      "POSITIVO_3"    =  4,
      "POSITIVO_4"    =  5,
      "HYPERPOSITIVO" =  6,
      .default = NA_real_
    )
  )

dados_sentimento <- dados_sentimento %>%
  mutate(
    espectro_tipo3 = dplyr::recode(
      tipo3,
      "HYPERNEGATIVO" = -6,
      "NEGATIVO_4"    = -5,
      "NEGATIVO_3"    = -4,
      "NEGATIVO_2"    = -3,
      "NEGATIVO_1"    = -2,
      "NEUTRO"        =  0,
      "POSITIVO_1"    =  2,
      "POSITIVO_2"    =  3,
      "POSITIVO_3"    =  4,
      "POSITIVO_4"    =  5,
      "HYPERPOSITIVO" =  6,
      .default = NA_real_
    )
  )

table(dados_sentimento$tipo2, dados_sentimento$espectro_tipo2)
table(dados_sentimento$tipo3, dados_sentimento$espectro_tipo3)
table(loa_nivel_paragrafo$tipo2, loa_nivel_paragrafo$espectro_tipo2)
table(loa_nivel_paragrafo$tipo3, loa_nivel_paragrafo$espectro_tipo3)

# ==============================================================================
# CHECKPOINT FINAL 
# ==============================================================================

readr::write_csv(
  loa_nivel_paragrafo,
  "C:/Users/ryall/Downloads/loa_corpus_paragrafo.csv",
  na = "",
  quote = "needed"
)

readr::write_csv(
  dados_sentimento,
  "C:/Users/ryall/Downloads/loa_corpus_palavra.csv",
  na = "",
  quote = "needed"
)

