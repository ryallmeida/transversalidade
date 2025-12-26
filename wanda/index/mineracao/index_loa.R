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


# ==============================================================================
# CORPUS EM NÍVEL PARAGRAFO
# ==============================================================================
# NOTAS METODOLÓGICAS: É MUITO CUSTOSO PRODUZIR UM CORPUS EM NIVEL DE PALAVRA

#-----------------------------------------------------------
# 6. Tokenização: transformar parágrafos em palavras
#-----------------------------------------------------------

dados_tokens <- resultado_limpo %>%
  unnest_tokens(word, texto_limpo)

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



# ==============================================================================
# CORPUS EM NÍVEL PALAVRA
# ==============================================================================

dados_sentimento <- dados_tokens %>%
  left_join(lexico_unique, by = c("word" = "termo"))

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

tokens_loa <- dados_sentimento$texto |>
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) |>
  tokens_remove(stopwords("portuguese"))

# ------------------------------------
# EXTRAINDO UM DATAFRAME DO TF
# ------------------------------------

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

dados_sentimento$DFM <- dados_sentimento$DFM %>%
  mutate(replace_na(dados_sentimento$DFM, 0))
