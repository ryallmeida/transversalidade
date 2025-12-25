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
               tm, 
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
arquivos_ldo <- list.files(
  path       = "C://Users//cicer//OneDrive//Documentos//METODOS//LDO",
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

corpus_ldo <- corpus(
  x             = df_corpus_ldo,
  text_field    = "text",
  docid_field   = "doc_id"
)

# print(class(corpus_ldo))   
# [1] "corpus"    "character"
# summary(corpus_ldo)  

# OUTPUT ----------------------------------------------------------------------
# -----------------------------------------------------------------------------
#Corpus consisting of 18 documents, showing 18 documents:
#        Text Types Tokens Sentences
#LDO_2008.pdf  3704  17426       257
#LDO_2009.pdf  3714  17710       265
#LDO_2010.pdf  3819  19026       283
#LDO_2011.pdf  3578  16441       270
#LDO_2012.pdf  3928  18549       276
#LDO_2013.pdf  3849  18632       284
#LDO_2014.pdf  3945  19463       317
#LDO_2015.pdf  3955  19404       311
#LDO_2016.pdf  4201  21948       409
#LDO_2017.pdf  4654  24609       440
#LDO_2018.pdf     0      0         0
#LDO_2019.pdf  4665  23720       423
#LDO_2020.pdf  5811  30717       480
#LDO_2021.pdf  5851  30013       515
#LDO_2022.pdf  6373  33832       573
#LDO_2023.pdf  6492  34459       508
#LDO_2024.pdf  6489  31239       474
#LDO_2025.pdf  7083  33346       484

# =============================================================================
# MONTANDO O DATRAFRAME PARA APLICAR OS DICIONÁRIO E ELABORAR O LABEL FINAL
# =============================================================================

texto_completo <- paste(textos_unificados_ldo, 
                        collapse = "\n")

paragrafos <- unlist(strsplit
                     (texto_completo, 
                       split = "\n\\s*\n", 
                       perl = TRUE))

paragrafos_limpos <- trimws(paragrafos)

paragrafos_filtrados <- paragrafos_limpos[paragrafos_limpos != ""]

df_paragrafos_ldo <- data.frame(paragrafo = paragrafos_filtrados, 
                                stringsAsFactors = FALSE)

# Visualiza os primeiros parágrafos
head(df_paragrafos_loa)

# Salva como CSV (pra usar depois no Machine Learning!)
#write.csv(df_paragrafos, "paragrafos_extraidos.csv", row.names = FALSE)

# =============================================================================
# COMPOSIÇÃO DOS DICIONÁRIOS PENSADOS AS LEI DE DIRETRIZES ORÇAMENTÁRIAS
# =============================================================================

dic2_transversalidade <- df_paragrafos_ldo %>%
  mutate(label=ifelse(grepl("integrar | integração | intersetorial | articulação | transversal | multidisciplinar | interinstitucional | cooperação | coordenação | consórcio | parceria | colaborativo | convergência | municípios | estados | união | entes federados | instâncias | interfederativo | descentralização | governança | colegiado | comitê gestor | conselhos | pactuação | corresponsabilidade | plano integrado | orçamento integrado | matriz intersetorial | planejamento conjunto | agenda comum | metas compartilhadas | indicadores integrados | programas estruturantes | iniciativas conjuntas | financiamento integrado | participação social | controle social | audiências públicas | consulta pública | escuta ativa | transparência | sociedade civil | deliberação | interseccionalidade | equidade | inclusão | grupos vulneráveis | promoção da igualdade | combate às desigualdades | ações afirmativas | direitos humanos | diversidade | justiça social | promover | executar | garantir | implantar | manutencao | fortalecer | desenvolver | ampliacao | crescimento | elevar | transferencia | atender | beneficios | realizar | ampliar | assegurar | execucao | fornecer | melhorar | promocao | recursos | acesso | atendimento | criacao | exercer | implantacao | manter | melhoria | modernizar | aperfeicoar | colaboracao | concessao | consolidacao | coordenar | desenvolvimento | eficiencia | estimular | executados | fortalecimento | implementar | incrementar | integracao | integrada | integrar | metas | otimizar | pagamento | participacao | participar | recuperacao | transparencia | viabilizar | abranger | abrangera | acao | acessibilidade | acompanhar | adaptavel realidade | agregar | alcancadas | alcance | alocacao | ampliacao cidadania ativa grupos discriminados | ampliacao integracao | ampliar melhoria | aperfeicoamento | aportar | aquisicao | arrecadacao | articulacao | atraente | aumento | avaliacao | capacitar | compensacao | compromisso | conscientizacao | conscientizar | conservar | consolidar | construcao | construir | contribuir | controle social | convenio | convergencia | cooperacao | creditos | custeio | defender | descentralizar | desenvolvidas | desigualdade infraestrutura | determinacao | direcionador | disponibilizar | disponibilizara | divulgar | efetivar | elaboracao", paragrafo, ignore.case = TRUE), 1, -1))


dic2_transversalidade2 <- dic2_transversalidade %>%
  mutate(label2=ifelse(grepl("alienacao | apoio | despesas | limite | reducao | assentamentos | atingida | ausencia | comprometimento | concentrados | contempla | contingenciamento | custo | deficit | densidade | desaceleracao | desemprego | desindustrializacao | devolucao | diminuicao | escolar | especificidades | estagnado | exclusivamente | fragmentacao | impedimento | julgar | limitacao | limitacoes | limites | mediante | morte | nenhuma | possibilita | programa | racionalizar | rever | subregistro | terras | tragedias | vedada | vulneraveis | setor específico | por secretaria | por pasta | atuação isolada | atuação setorial | responsabilidade única | sem articulação | fragmentado | fragmentação | divisão de competências | compartimentalização | autônomo | setorializado | ausência de coordenação | falta de integração | falta de diálogo | não articulado | sem parceria | sem cooperação | desarticulado | sem comunicação | sem sinergia | ações separadas | decisão centralizada | competência exclusiva | centralização | comando único | sem descentralização | autoridade exclusiva | gestão isolada | nível único | sem consulta | unilateral | responsabilidade do município | sem apoio da união | ausência de corresponsabilidade | não pactuado | sem pactuação | sem integração federativa | delegação sem recursos", paragrafo, ignore.case = TRUE), -1, 1))

dic2_final_ldo <- dic2_transversalidade2 %>% mutate(label_final = case_when(
  label == 1 & label2 == 1 ~ 1,
  label == 1 & label2 == -1 ~ 0,
  label == -1 & label2 == 1 ~ 0,
  label == -1 & label2 == -1 ~ -1
))

# =======================================
# ANALISE EXPLORATÓRIA DO CORPUS DO LOA
# =======================================

contagem_ldo <- table(dic2_final_ldo$label_final)
print(contagem_ldo)
#  -1    0    1 
#1046 7268 2456 

bar3 <- barplot(contagem_ldo)

#dic_final$label_final

# =============================================================================
colnames(dic2_final_ldo)
dic2_final_ldo <- dic2_final_ldo[, !colnames(dic2_final_ldo) %in% "label"]
dic2_final_ldo <- dic2_final_ldo[, !colnames(dic2_final_ldo) %in% "label2"]
# =============================================================================

#write.csv(dic2_final_ldo, "C://Users//cicer//OneDrive//Documentos//METODOS//CORPUS//dic2_corpus_ldo.csv", row.names = FALSE)

#write.csv(dic2_final, "C://Users//cicer//OneDrive//Documentos//METODOS//CORPUS//dic2_corpus_loa.csv", row.names = FALSE)

# =============================================================================
# GRAFICOS QUE EXPOEM O RESULTADO DA ANÁLISE EXPLORATÓRIA
# =============================================================================

if(!require(ggplot2))
  install.packages("ggplot2")  
library(ggplot2)

# VETORES COM DADOS EXTRAÍDOS ANTERIORMENTE
ppa <- c(`-1` = 1057, `0` = 20814, `1` = 4578)
ldo <- c(`-1` = 1046, `0` = 7268, `1` = 2456)
loa <- c(`-1` = 24273, `0` = 70295, `1` = 26707)

# FT TYPE WIDE
dados <- data.frame(
  valor = factor(rep(c(-1, 0, 1), times = 3), 
                 levels = c(-1, 0, 1)),
  frequencia = c(ppa, 
                 ldo, 
                 loa),
  tipo = rep(c("PPA", "LDO", "LOA"), each = 3)
)

ggplot(dados, aes(x = valor, 
                  y = frequencia, 
                  fill = tipo)) +
  geom_bar(stat = "identity", 
           position = "stack", 
           color = "black", 
           width = 0.7) +
  scale_fill_manual(values = c("#252525", 
                               "#636363", 
                               "#969696")) +
  labs(title = "", 
       x = "Valor", 
       y = "Frequência") +
  theme_gray(base_size = 14)


cores_cinza <- c(
  "PPA" = "#252525",
  "LDO" = "#636363",
  "LOA" = "#969696"
)
# =============================================================================
# Calculando a proporçãoo para tabelar
# =============================================================================
# PPA

valor1 <- 4578
valor2 <- 20814
valor3 <- 1057
total_ppa <- 4578 + 20814 + 1057

proporção1 <- valor1/total_ppa*100
proporção2 <- valor2/total_ppa*100
proporção3 <- valor3/total_ppa*100

print(proporção1)
print(proporção2)
print(proporção3)

valor4 <- 2456
valor5 <- 7268
valor6 <- 1046
total_ldo <- 2456 + 7268 + 1046

proporção4 <- valor4/total_ldo*100
proporção5 <- valor5/total_ldo*100
proporção6 <- valor6/total_ldo*100

print(proporção4)
print(proporção5)
print(proporção6)

valor7 <- 26707
valor8 <- 70295
valor9 <- 24273
total_loa <- 26707 + 70295 + 24273

proporção7 <- valor7/total_loa*100
proporção8 <- valor8/total_loa*100
proporção9 <- valor9/total_loa*100

print(proporção7)
print(proporção8)
print(proporção9)

# =============================================================================
# ANÁLISE DA FREQUENCIA DOS TOKKENS
# =============================================================================

teste_ppa <- read.csv2("C:/Users/cicer/OneDrive/Documentos/METODOS/PREPROCESSADOS/corpus_preprocessado_ppa.csv", sep = ",")

library(tm)
corpus_tm <- tm::VCorpus(VectorSource(teste_ppa$Texto))

corpus_tm <- tm::tm_map(corpus_tm, content_transformer(tolower))       
corpus_tm <- tm::tm_map(corpus_tm, removePunctuation)                 
corpus_tm <- tm::tm_map(corpus_tm, removeNumbers)                     
corpus_tm <- tm::tm_map(corpus_tm, removeWords, stopwords("portuguese"))  
corpus_tm <- tm::tm_map(corpus_tm, stripWhitespace)     

dtm <- tm::DocumentTermMatrix(corpus_tm)
inspect(dtm[1:5, 1:10])

quanteda.textstats::textstat_frequency(dtm)
# PAREI AQUI!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!DEVO CONTINUAR TENTANDO FAZER UMA FORMA DE ACHAR A MATRIZ DE FREQUENCIA DIALOGAR COM O TM (IMPOSSIBLE)

# =============================================================================
# ANÁLISE DA FREQUENCIA DOS TOKKENS
# =============================================================================

# Cria uma pasta para os .txt se ainda não existir
# dir.create("C://Users//cicer//OneDrive//Documentos//METODOS//PPA_txt", showWarnings = FALSE)

# Cria uma pasta para os .txt se ainda não existir
dir.create("C://Users//cicer//OneDrive//Documentos//METODOS//LDO_txt", showWarnings = FALSE)

# Salva cada texto como .txt
for (i in seq_along(df_corpus_ldo$text)) {
  caminho <- file.path("C://Users//cicer//OneDrive//Documentos//METODOS//LDO_txt",paste0(tools::file_path_sans_ext(df_corpus_ldo$doc_id[i]), ".txt"))
  writeLines(df_corpus_ldo$text[i], con = caminho)
}

library(readtext)
library(quanteda)

# Lê todos os .txt como um corpus novamente
textos_ldo_readtext <- readtext("C://Users//cicer//OneDrive//Documentos//METODOS//LDO_txt/*.txt", text_field = "text")

# Transforma em corpus do quanteda
corpus_ldo2 <- corpus(textos_ldo_readtext)

tokens_ldo <- corpus_subset(corpus_ldo2) |>
  tokens(
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_separators = TRUE
  ) |>
  tokens_remove(pattern = stopwords("portuguese")) |>
  tokens_remove(pattern = c("nº", "1º", "2º", "3º", "4º", "lei", "outubro", "i", "ii", "iii", "iv", "v", "inciso", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"))

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
  kwic(tokens_ldo, pattern = "integrar"),
  kwic(tokens_ldo, pattern = "transversalidade"),
  kwic(tokens_ldo, pattern = "setor"),
  scale = "absolute"
)

###
library(quanteda.textstats)
freq_grouped <- textstat_frequency(dfm(tokens_ldo), groups = )

# Filter the term "american"
freq_transversalidade <- subset(freq_grouped, freq_grouped$feature %in% "transversalidade")  

library(ggplot2)
plot1_transv <- ggplot(freq_transversalidade, aes(x = frequency, y = group)) +
  geom_point() + 
  scale_x_continuous(limits = c(0, 14), breaks = c(seq(0, 14, 2))) +
  labs(x = "Frequência", y = NULL,
       title = 'LDO') +
  theme(plot.title = element_text(hjust = 0.5))
print(plot1_transv)

set.seed(100)

dfm(dfmat_ldo) 

library("quanteda")
library("quanteda.textplots")
textplot_wordcloud(dfmat_ldo)
textplot_network(dfmat_ldo, min_freq = 10, max_terms = 25)

# =============================================================================
# REFERÊNCIAS 
# =============================================================================

### R DEVELOPMENT CORE TEAM. R: Uma linguagem e ambiente para computação estatística . Viena: R Foundation for Statistical Computing, 2011. ISBN 3-900051-07-0. Disponível em: http://www.R-project.org/ . Acessado em : 6 de jan. de 2025.



