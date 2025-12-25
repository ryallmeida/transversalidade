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


# -------------------------------------------------
# INSTALAÇÃO DOS PACKAGES 
# -------------------------------------------------

pacman::p_load(pdftools, 
               tidyverse, 
               readr, 
               writexl, 
               tm, 
               quanteda, 
               quanteda.textplots, 
               quanteda.textstats, 
               readtext, 
               stopwords,
               textreuse,
               topicmodels,
               wesanderson,
               tidytext)

pacman::p_load(stringr,
               tidyverse,
               readtext,
               quanteda)

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
# COLOCANDO O ID DA OBSERVAÇÃO
# =============================================================================

df_raw <- data.frame(
  doc_id = quanteda::docnames(corpus_ldo),
  text   = as.character(corpus_ldo),
  stringsAsFactors = FALSE
)

df_tidy_ldo <- df_raw %>%
  dplyr::mutate(paragrafo = stringr::str_split(text, pattern = "\n\\s*\n")) %>%
  tidyr::unnest(paragrafo) %>%
  dplyr::mutate(paragrafo = stringr::str_squish(paragrafo)) %>%
  dplyr::filter(paragrafo != "") %>%
  dplyr::select(doc_id, paragrafo)


# =============================================================================
# MINERANDO O DADO: ANO
# =============================================================================

df_tidy_ldo$ano <- stringr::str_extract(df_tidy_ldo$doc_id, "\\d{4}")

# =============================================================================
# ROTULANDO OS DADOS A PARTIR DE UM DICIONÁRIO TEMÁTICO
# =============================================================================

dic2_transversalidade <- df_tidy_ldo %>%
  mutate(label=ifelse(grepl("integrar | integração | intersetorial | articulação | transversal | multidisciplinar | interinstitucional | cooperação | coordenação | consórcio | parceria | colaborativo | convergência | municípios | estados | união | entes federados | instâncias | interfederativo | descentralização | governança | colegiado | comitê gestor | conselhos | pactuação | corresponsabilidade | plano integrado | orçamento integrado | matriz intersetorial | planejamento conjunto | agenda comum | metas compartilhadas | indicadores integrados | programas estruturantes | iniciativas conjuntas | financiamento integrado | participação social | controle social | audiências públicas | consulta pública | escuta ativa | transparência | sociedade civil | deliberação | interseccionalidade | equidade | inclusão | grupos vulneráveis | promoção da igualdade | combate às desigualdades | ações afirmativas | direitos humanos | diversidade | justiça social | promover | executar | garantir | implantar | manutencao | fortalecer | desenvolver | ampliacao | crescimento | elevar | transferencia | atender | beneficios | realizar | ampliar | assegurar | execucao | fornecer | melhorar | promocao | recursos | acesso | atendimento | criacao | exercer | implantacao | manter | melhoria | modernizar | aperfeicoar | colaboracao | concessao | consolidacao | coordenar | desenvolvimento | eficiencia | estimular | executados | fortalecimento | implementar | incrementar | integracao | integrada | integrar | metas | otimizar | pagamento | participacao | participar | recuperacao | transparencia | viabilizar | abranger | abrangera | acao | acessibilidade | acompanhar | adaptavel realidade | agregar | alcancadas | alcance | alocacao | ampliacao cidadania ativa grupos discriminados | ampliacao integracao | ampliar melhoria | aperfeicoamento | aportar | aquisicao | arrecadacao | articulacao | atraente | aumento | avaliacao | capacitar | compensacao | compromisso | conscientizacao | conscientizar | conservar | consolidar | construcao | construir | contribuir | controle social | convenio | convergencia | cooperacao | creditos | custeio | defender | descentralizar | desenvolvidas | desigualdade infraestrutura | determinacao | direcionador | disponibilizar | disponibilizara | divulgar | efetivar | elaboracao", paragrafo, ignore.case = TRUE), 1, -1))


dic2_transversalidade2 <- dic2_transversalidade %>%
  mutate(label2=ifelse(grepl("alienacao | apoio | despesas | limite | reducao | assentamentos | atingida | ausencia | comprometimento | concentrados | contempla | contingenciamento | custo | deficit | densidade | desaceleracao | desemprego | desindustrializacao | devolucao | diminuicao | escolar | especificidades | estagnado | exclusivamente | fragmentacao | impedimento | julgar | limitacao | limitacoes | limites | mediante | morte | nenhuma | possibilita | programa | racionalizar | rever | subregistro | terras | tragedias | vedada | vulneraveis | setor específico | por secretaria | por pasta | atuação isolada | atuação setorial | responsabilidade única | sem articulação | fragmentado | fragmentação | divisão de competências | compartimentalização | autônomo | setorializado | ausência de coordenação | falta de integração | falta de diálogo | não articulado | sem parceria | sem cooperação | desarticulado | sem comunicação | sem sinergia | ações separadas | decisão centralizada | competência exclusiva | centralização | comando único | sem descentralização | autoridade exclusiva | gestão isolada | nível único | sem consulta | unilateral | responsabilidade do município | sem apoio da união | ausência de corresponsabilidade | não pactuado | sem pactuação | sem integração federativa | delegação sem recursos", paragrafo, ignore.case = TRUE), -1, 1))

dic2_final_ldo <- dic2_transversalidade2 %>% mutate(label_final = case_when(
  label == 1 & label2 == 1 ~ 1,
  label == 1 & label2 == -1 ~ 0,
  label == -1 & label2 == 1 ~ 0,
  label == -1 & label2 == -1 ~ -1
))

# =============================================================================
# ANALISE EXPLORATÓRIA INICIAL DO CORPUS DO LOA
# =============================================================================

contagem_ldo <- table(dic2_final_ldo$label_final)
print(contagem_ldo)

# ------------------------------------
#   -1    0    1 
# 1093 7557 2662 
# ------------------------------------

# TEM MUITOS ZEROS E ISSO PODE SER UM PROBLEMA NO MACHINE LEARNING
# A PARTIR DAQUI FAREMOS OS PRIMEIROS TRATAMENTOS DO BANCO DE DADOS

# =============================================================================
# MATRIZ DE FREQUÊNCIA
# =============================================================================

# dir.create("C:/Users/ryall/Documents/transversalidade/LDO/LDO_txt", showWarnings = FALSE)

# for (i in seq_along(df_corpus_ldo$text)) { caminho <- file.path("C:/Users/ryall/Documents/transversalidade/LDO/LDO_txt",paste0(tools::file_path_sans_ext(df_corpus_ldo$doc_id[i]), ".txt")) writeLines(df_corpus_ldo$text[i], con = caminho)}

textos_ldo <- readtext("C:/Users/ryall/Documents/transversalidade/LDO/LDO_txt/*.txt", text_field = "text")

# Transforma em corpus do quanteda
corpus_ldo2 <- corpus(textos_ldo)

dfmat_ldo <- quanteda::corpus_subset(corpus_ldo2) |>
  quanteda::tokens(
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_separators = TRUE
  ) |>
  quanteda::tokens_remove(pattern = stopwords("portuguese")) |>
  quanteda::tokens_remove(pattern = c("nº", "1º", "2º", "3º", "4º", "lei", "outubro", "i", "ii", "iii", "iv", "v", "inciso", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")) |>  
  quanteda::dfm() |>
  quanteda::dfm_trim(min_termfreq = 100, 
                     verbose = FALSE)

dfmat_tfidf <- quanteda::dfm_tfidf(dfmat_ldo)

# Visualizar os termos com maior TF-IDF por documento
m_frq <- as.data.frame(topfeatures(dfmat_tfidf, n = 1000))

m_frq$word <- rownames(m_frq)

#write.csv(m_frq, "C:/Users/ryall/Desktop/R/transversalidade/wanda/preprocessamento/dataframes/matriz_td-idf_ldo.csv", row.names = FALSE)

# =============================================================================
# LIMPEZA E PREPROCESSAMENTO
# =============================================================================
# dados <- read.csv("https://raw.githubusercontent.com/ryallmeida/transversalidade/refs/heads/main/wanda/preprocessamento/dataframes/corpus_ldo.csv")

dados <- dic2_final_ldo

dados$paragrafo <- dados$paragrafo %>%
  # Tudo para minúsculo (antes de remover)
  tolower() %>%
  
  # Remove números
  str_replace_all("\\d+", " ") %>%
  
  # Remove URLs
  str_replace_all("http[^\\s]+", " ") %>%
  
  # Remove pontuações internas e externas
  str_replace_all("[[:punct:]]+", " ") %>%
  
  # Remove letras soltas (a, b, c...)
  str_replace_all("\\b[a-z]\\b", " ") %>%
  
  # Remove palavras jurídicas repetitivas
  str_replace_all("\\b(lei|i|ii|iii|iv|v|vi|vii|viii|ix|x|xi|xii|xiii|º|inciso|parágrafo|artigo|art|caput|único|outubro|nº)\\b", " ") %>%
  
  # Remove stopwords portuguesas (convertendo lista para regex)
  { str_replace_all(.,
                    paste0("\\b(", paste(stopwords("portuguese"), collapse="|"), ")\\b"),
                    " "
  ) } %>%
  
  # Remover múltiplos espaços
  str_squish()


# write.csv(dic2_final_ldo, "C:/Users/ryall/Desktop/R/transversalidade/wanda/preprocessamento/dataframes/corpus_ldo.csv", row.names = FALSE)
