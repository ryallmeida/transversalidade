# TRATAMENTO DO CORPUS: ANÁLISE DOS PLANOS PLURIANUAIS DE PERNAMBUCO (2008-2027)
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

if(!require(pacman))
  install.packages("pacman")  
library(pacman)

# -------------------------------------------------
# INSTALAÇÃO DOS PACKAGES 
# -------------------------------------------------


pacman::p_load(pdftools, 
               dplyr, 
               readr, 
               writexl,
               quanteda, 
               quanteda.textplots, 
               quanteda.textstats, 
               readtext, 
               stopwords,
               textreuse,
               tidyverse, topicmodels,
               wesanderson)

# -------------------------------------------------
# VERSÕES DOS PACOTES NESSA MÁQUINA
# -------------------------------------------------

#  > cat("-------------------\n")
#-------------------
#  > cat("pdftools:", as.character(packageVersion("pdftools")), "\n")
#pdftools: 3.5.0 
#> cat("dplyr:", as.character(packageVersion("dplyr")), "\n")
#dplyr: 1.1.4 
#> cat("readr:", as.character(packageVersion("readr")), "\n")
#readr: 2.1.5 
#> cat("writexl:", as.character(packageVersion("writexl")), "\n")
#writexl: 1.5.4 
#> cat("tm:", as.character(packageVersion("tm")), "\n")
#tm: 0.7.16 
#> cat("quanteda:", as.character(packageVersion("quanteda")), "\n")
#quanteda: 4.3.1 
#> cat("quanteda.textplots:", as.character(packageVersion("quanteda.textplots")), "\n")
#quanteda.textplots: 0.95 
#> cat("quanteda.textstats:", as.character(packageVersion("quanteda.textstats")), "\n")
#quanteda.textstats: 0.97.2 
#> cat("readtext:", as.character(packageVersion("readtext")), "\n")
#readtext: 0.91 
#> cat("stopwords:", as.character(packageVersion("stopwords")), "\n")
#stopwords: 2.3 
#> cat("textreuse:", as.character(packageVersion("textreuse")), "\n")
#textreuse: 0.1.5 
#> cat("tidyverse:", as.character(packageVersion("tidyverse")), "\n")
#tidyverse: 2.0.0 
#> cat("topicmodels:", as.character(packageVersion("topicmodels")), "\n")
#topicmodels: 0.2.17 
#> cat("wesanderson:", as.character(packageVersion("wesanderson")), "\n")
#wesanderson: 0.3.7 

# =============================================================================

# -------------------------------------------------
# LEITURA DOS ARQUIVOS E CONSTRUÇÃO DO CORPUS
# -------------------------------------------------
arquivos_ppa <- list.files(
  path       = "C://Users//cicer//OneDrive//Documentos//METODOS//PPA",
  pattern    = "\\.pdf$", 
  full.names = TRUE
)

textos_unificados_ppa <- sapply(
  arquivos_ppa,
  function(arq) {
    paginas <- pdf_text(arq)
    paste(paginas, collapse = " ")
  },
  USE.NAMES = FALSE
)

df_corpus_ppa <- data.frame(
  doc_id = basename(arquivos_ppa),
  text   = textos_unificados_ppa,
  stringsAsFactors = FALSE
)

corpus_ppa <- corpus(
  x             = df_corpus_ppa,
  text_field    = "text",
  docid_field   = "doc_id"
)

# print(class(corpus_ldo))   
# [1] "corpus"    "character"
summary(corpus_ppa)  

# OUTPUT ----------------------------------------------------------------------
# -----------------------------------------------------------------------------
#Corpus consisting of 18 documents, showing 18 documents:
#         Text Types Tokens Sentences
#PPA_08-11.pdf  5805  29845       632 Pt_1
#PPA_08-11.pdf   641   1724        15 Pt_2
#PPA_08-11.pdf   383    846         2 Pt_3
#PPA_12-15.pdf  3883  17148       392 Pt_1
#PPA_12-15.pdf  9654  80011       351 Pt_2
#PPA_16-19.pdf 12365 201912      1234
#PPA_20-23.pdf 11855 186639       918
#PPA_24-27.pdf 11712 184432      1445

# =============================================================================
# MONTANDO O DATRAFRAME PARA APLICAR OS DICIONÁRIO E ELABORAR O LABEL FINAL
# =============================================================================

texto_completo <- paste(textos_unificados_ppa, 
                        collapse = "\n")

paragrafos <- unlist(strsplit
                     (texto_completo, 
                       split = "\n\\s*\n", 
                       perl = TRUE))

paragrafos_limpos <- trimws(paragrafos)

paragrafos_filtrados <- paragrafos_limpos[paragrafos_limpos != ""]

df_paragrafos_ppa <- data.frame(paragrafo = paragrafos_filtrados, 
                                stringsAsFactors = FALSE)

# Visualiza os primeiros parágrafos
head(df_paragrafos_loa)

# Salva como CSV (pra usar depois no Machine Learning!)
#write.csv(df_paragrafos, "paragrafos_extraidos.csv", row.names = FALSE)

# =============================================================================
# COMPOSIÇÃO DOS DICIONÁRIOS PENSADOS AS LEI DE DIRETRIZES ORÇAMENTÁRIAS
# =============================================================================

dic2_transversalidade <- df_paragrafos_ppa %>%
  mutate(label=ifelse(grepl("integrar | integração | intersetorial | articulação | transversal | multidisciplinar | interinstitucional | cooperação | coordenação | consórcio | parceria | colaborativo | convergência | municípios | estados | união | entes federados | instâncias | interfederativo | descentralização | governança | colegiado | comitê gestor | conselhos | pactuação | corresponsabilidade | plano integrado | orçamento integrado | matriz intersetorial | planejamento conjunto | agenda comum | metas compartilhadas | indicadores integrados | programas estruturantes | iniciativas conjuntas | financiamento integrado | participação social | controle social | audiências públicas | consulta pública | escuta ativa | transparência | sociedade civil | deliberação | interseccionalidade | equidade | inclusão | grupos vulneráveis | promoção da igualdade | combate às desigualdades | ações afirmativas | direitos humanos | diversidade | justiça social | promover | executar | garantir | implantar | manutencao | fortalecer | desenvolver | ampliacao | crescimento | elevar | transferencia | atender | beneficios | realizar | ampliar | assegurar | execucao | fornecer | melhorar | promocao | recursos | acesso | atendimento | criacao | exercer | implantacao | manter | melhoria | modernizar | aperfeicoar | colaboracao | concessao | consolidacao | coordenar | desenvolvimento | eficiencia | estimular | executados | fortalecimento | implementar | incrementar | integracao | integrada | integrar | metas | otimizar | pagamento | participacao | participar | recuperacao | transparencia | viabilizar | abranger | abrangera | acao | acessibilidade | acompanhar | adaptavel realidade | agregar | alcancadas | alcance | alocacao | ampliacao cidadania ativa grupos discriminados | ampliacao integracao | ampliar melhoria | aperfeicoamento | aportar | aquisicao | arrecadacao | articulacao | atraente | aumento | avaliacao | capacitar | compensacao | compromisso | conscientizacao | conscientizar | conservar | consolidar | construcao | construir | contribuir | controle social | convenio | convergencia | cooperacao | creditos | custeio | defender | descentralizar | desenvolvidas | desigualdade infraestrutura | determinacao | direcionador | disponibilizar | disponibilizara | divulgar | efetivar | elaboracao", paragrafo, ignore.case = TRUE), 1, -1))


dic2_transversalidade2 <- dic2_transversalidade %>%
  mutate(label2=ifelse(grepl("alienacao | apoio | despesas | limite | reducao | assentamentos | atingida | ausencia | comprometimento | concentrados | contempla | contingenciamento | custo | deficit | densidade | desaceleracao | desemprego | desindustrializacao | devolucao | diminuicao | escolar | especificidades | estagnado | exclusivamente | fragmentacao | impedimento | julgar | limitacao | limitacoes | limites | mediante | morte | nenhuma | possibilita | programa | racionalizar | rever | subregistro | terras | tragedias | vedada | vulneraveis | setor específico | por secretaria | por pasta | atuação isolada | atuação setorial | responsabilidade única | sem articulação | fragmentado | fragmentação | divisão de competências | compartimentalização | autônomo | setorializado | ausência de coordenação | falta de integração | falta de diálogo | não articulado | sem parceria | sem cooperação | desarticulado | sem comunicação | sem sinergia | ações separadas | decisão centralizada | competência exclusiva | centralização | comando único | sem descentralização | autoridade exclusiva | gestão isolada | nível único | sem consulta | unilateral | responsabilidade do município | sem apoio da união | ausência de corresponsabilidade | não pactuado | sem pactuação | sem integração federativa | delegação sem recursos", paragrafo, ignore.case = TRUE), -1, 1))

dic2_final_ppa <- dic2_transversalidade2 %>% mutate(label_final = case_when(
  label == 1 & label2 == 1 ~ 1,
  label == 1 & label2 == -1 ~ 0,
  label == -1 & label2 == 1 ~ 0,
  label == -1 & label2 == -1 ~ -1
))

# =======================================
# ANALISE EXPLORATÓRIA DO CORPUS DO LOA
# =======================================

contagem_ppa <- table(dic2_final_ldo$label_final)
print(contagem_ldo)
#CONTAGEM COM DICIONÁRIO ANTIGO

# -------------------------------------------------
#   -1    0    1 
# 1046 7268 2456 
# -------------------------------------------------

#CONTAGEM COM DICIONÁRIO NOVO
contagem_ppa <- table(dic2_final_ppa$label_final)
print(contagem_ppa)

# -------------------------------------------------
#  -1     0     1 
# 960 20382  5107 
# -------------------------------------------------
# =======================================
# VALIDAÇÃO CRUZADA DOS DADOS CLASSIFICADOS COMO NEUTROS
# =======================================
dic3_transversalidade3 <- df_paragrafos_ppa %>%
  mutate(label3 = ifelse(grepl("apoiar | ampliar | ampliacao | coordenar | nenhuma | apoio | fortalecer | promover | adotar | consolidar | estimular | implementar | monitorar | avaliacao | desenvolver | divulgar | garantir | melhoria | adequacao | adequar | amortizacao | assegurar | atender | avaliar | gerir | manter | propoe | reduzir | acompanhamento | agregar | ajuda | alvo | amparar | apoiar ampliar | apresentar | aquisicao | articulado | articular | articularse | assumir | centralizar | conscientizar | conservar | controle | coordenara | democratizar | descentralizar | dinamicidade | disciplinando | divilgar | dotar | eficientizar | elaboracao | estima | estrategia | estruturar | facilitar | fomentar | fomentara | formulacao | fortalecimento | gerenciar | hipotese | implantar | implementacao | incentivar | incentivo | integrado | interiorizacao | intervencao | melhorar oferecer | mobilizar | modernizar | modificar | monitoramento | oferecer | participar | potencializar | preparar inserir | preservar | prevencao | publicar | qualificacao | recomposicao | regular | requalificar | revisar | revitalizar | subsidiar | superar | supervisionar | universalizacao | viabilizar", paragrafo, ignore.case = TRUE), 1, -1))

contagem_neutro_ppa <- table(dic3_transversalidade3$label3)
print(contagem_neutro_ppa)

# valores conferem
# -------------------------------------------------
#    -1     1 
# 22374  4075 
# -------------------------------------------------

bar3 <- barplot(contagem_ldo)

#dic_final$label_final

colnames(dic2_final_ppa)
dic2_final_ppa <- dic2_final_ppa[, !colnames(dic2_final_ppa) %in% "label"]
dic2_final_ppa <- dic2_final_ppa[, !colnames(dic2_final_ppa) %in% "label2"]

# write.csv(dic2_final_ppa, "C://Users//cicer//OneDrive//Documentos//METODOS//CORPUS//dic2_corpus_ppa.csv", row.names = FALSE)

# =============================================================================
# ANÁLISE DA FREQUENCIA DOS TOKKENS
# =============================================================================

# Cria uma pasta para os .txt se ainda não existir
# dir.create("C://Users//cicer//OneDrive//Documentos//METODOS//PPA_txt", showWarnings = FALSE)

# Salva cada texto como .txt
for (i in seq_along(df_corpus_ppa$text)) {
  caminho <- file.path("C://Users//cicer//OneDrive//Documentos//METODOS//PPA_txt",paste0(tools::file_path_sans_ext(df_corpus_ppa$doc_id[i]), ".txt"))
  writeLines(df_corpus_ppa$text[i], con = caminho)
}

library(readtext)
library(quanteda)

# Lê todos os .txt como um corpus novamente
textos_ppa_readtext <- readtext("C://Users//cicer//OneDrive//Documentos//METODOS//PPA_txt//", text_field = "text")

# Transforma em corpus do quanteda
corpus_ppa2 <- corpus(textos_ppa_readtext)

tokens_ppa <- quanteda::corpus_subset(corpus_ppa2) |>
  quanteda::tokens(
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_separators = TRUE
  ) |>
  quanteda::tokens_remove(pattern = stopwords("portuguese")) |>
  quanteda::tokens_remove(pattern = c("nº", "1º", "2º", "3º", "4º", "lei", "outubro", "i", "ii", "iii", "iv", "v", "inciso", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"))


dfmat_ldo <- corpus_subset(corpus_ldo2) |>
  tokens(
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_separators = TRUE
  ) |>
  tokens_remove(pattern = stopwords("portuguese")) |>
  tokens_remove(pattern = c("nº", "1º", "2º", "3º", "4º", "lei", "outubro", "i", "ii", "iii", "iv", "v", "inciso", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")) |>  
  dfm() |>
  dfm_trim(min_termfreq = 100, verbose = FALSE)

###
textplot_xray(
  kwic(tokens_ppa, pattern = "integrar"),
  kwic(tokens_ppa, pattern = "transversalidade"),
  kwic(tokens_ppa, pattern = "setor"),
  scale = "absolute"
)

###
library(quanteda.textstats)
freq_grouped_ppa <- textstat_frequency(dfm
                                   (tokens_ppa), 
                                   groups = )

# Filter the term "american"
freq_transversalidade_ppa <- subset(freq_grouped_ppa, freq_grouped_ppa$feature %in% "transversalidade")  

library(ggplot2)
plot2_transv <- ggplot(freq_transversalidade_ppa, aes(x = frequency, y = group)) +
  geom_point() + 
  scale_x_continuous(limits = c(0, 14), breaks = c(seq(0, 14, 2))) +
  labs(x = "Frequência", y = NULL,
       title = 'PPA') +
  theme(plot.title = element_text(hjust = 0.5))
print(plot2_transv)


