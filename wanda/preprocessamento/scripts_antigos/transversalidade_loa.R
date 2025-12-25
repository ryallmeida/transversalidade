# TRATAMENTO DO CORPUS: ANÁLISE DAS LEIS ORÇAMENTARIAS ANUAIS DE PERNAMBUCO (2008-2025)
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
arquivos_loa <- list.files(
  path       = "C://Users//cicer//OneDrive//Documentos//METODOS//LOA",
  pattern    = "\\.pdf$", 
  full.names = TRUE
)

textos_unificados_loa <- sapply(
  arquivos_loa,
  function(arq) {
    paginas <- pdf_text(arq)
    paste(paginas, collapse = " ")
  },
  USE.NAMES = FALSE
)

df_corpus_loa <- data.frame(
  doc_id = basename(arquivos_loa),
  text   = textos_unificados_loa,
  stringsAsFactors = FALSE
)

corpus_loa <- corpus(
  x             = df_corpus_loa,
  text_field    = "text",
  docid_field   = "doc_id"
)

#print(class(corpus_loa))   
#summary(corpus_loa)         
# OUTPUT ----------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Corpus consisting of 19 documents, showing 19 documents:
  
#        Text Types Tokens Sentences
#LOA_2008.pdf 19796 274536      3182
#LOA_2009.pdf 21186 264071      2785
#LOA_2010.pdf 22400 312043      3996
#LOA_2011.pdf 22679 344708      4558
#LOA_2012.pdf 15370 215800      1254 vol1
#LOA_2012.pdf 10409 346806      6308 vol2
#LOA_2013.pdf 16446 241015      1577
#LOA_2014.pdf 16735 249147      1378
#LOA_2015.pdf 17118 255870      1181
#LOA_2016.pdf 15852 266672      1424
#LOA_2017.pdf 16222 262069      1371
#LOA_2018.pdf 17218 273169      1433
#LOA_2019.pdf 16374 261118      1132
#LOA_2020.pdf 17270 255982      1465
#LOA_2021.pdf 16580 247409      1260
#LOA_2022.pdf 17247 263012      1442
#LOA_2023.pdf 16728 276169      1403
#LOA_2024.pdf 17336 271978      1847
#LOA_2025.pdf 16983 250362      1667

# =============================================================================
# MONTANDO O DATRAFRAME PARA APLICAR OS DICIONÁRIO E ELABORAR O LABEL FINAL
# =============================================================================

texto_completo <- paste(textos_unificados_loa, 
                        collapse = "\n")

paragrafos <- unlist(strsplit
                     (texto_completo, 
                       split = "\n\\s*\n", 
                       perl = TRUE))

paragrafos_limpos <- trimws(paragrafos)

paragrafos_filtrados <- paragrafos_limpos[paragrafos_limpos != ""]

df_paragrafos_loa <- data.frame(paragrafo = paragrafos_filtrados, 
                            stringsAsFactors = FALSE)

# Visualiza os primeiros parágrafos
head(df_paragrafos_loa)

# Salva como CSV (pra usar depois no Machine Learning!)
#write.csv(df_paragrafos, "paragrafos_extraidos.csv", row.names = FALSE)

# =============================================================================
# COMPOSIÇÃO DOS DICIONÁRIOS PENSADOS AS LEI DE DIRETRIZES ORÇAMENTÁRIAS
# =============================================================================

dic2_transversalidade <- df_paragrafos_loa %>%
  mutate(label=ifelse(grepl("integrar | integração | intersetorial | articulação | transversal | multidisciplinar | interinstitucional | cooperação | coordenação | consórcio | parceria | colaborativo | convergência | municípios | estados | união | entes federados | instâncias | interfederativo | descentralização | governança | colegiado | comitê gestor | conselhos | pactuação | corresponsabilidade | plano integrado | orçamento integrado | matriz intersetorial | planejamento conjunto | agenda comum | metas compartilhadas | indicadores integrados | programas estruturantes | iniciativas conjuntas | financiamento integrado | participação social | controle social | audiências públicas | consulta pública | escuta ativa | transparência | sociedade civil | deliberação | interseccionalidade | equidade | inclusão | grupos vulneráveis | promoção da igualdade | combate às desigualdades | ações afirmativas | direitos humanos | diversidade | justiça social | promover | executar | garantir | implantar | manutencao | fortalecer | desenvolver | ampliacao | crescimento | elevar | transferencia | atender | beneficios | realizar | ampliar | assegurar | execucao | fornecer | melhorar | promocao | recursos | acesso | atendimento | criacao | exercer | implantacao | manter | melhoria | modernizar | aperfeicoar | colaboracao | concessao | consolidacao | coordenar | desenvolvimento | eficiencia | estimular | executados | fortalecimento | implementar | incrementar | integracao | integrada | integrar | metas | otimizar | pagamento | participacao | participar | recuperacao | transparencia | viabilizar | abranger | abrangera | acao | acessibilidade | acompanhar | adaptavel realidade | agregar | alcancadas | alcance | alocacao | ampliacao cidadania ativa grupos discriminados | ampliacao integracao | ampliar melhoria | aperfeicoamento | aportar | aquisicao | arrecadacao | articulacao | atraente | aumento | avaliacao | capacitar | compensacao | compromisso | conscientizacao | conscientizar | conservar | consolidar | construcao | construir | contribuir | controle social | convenio | convergencia | cooperacao | creditos | custeio | defender | descentralizar | desenvolvidas | desigualdade infraestrutura | determinacao | direcionador | disponibilizar | disponibilizara | divulgar | efetivar | elaboracao", paragrafo, ignore.case = TRUE), 1, -1))


dic2_transversalidade2 <- dic2_transversalidade %>%
  mutate(label2=ifelse(grepl("alienacao | apoio | despesas | limite | reducao | assentamentos | atingida | ausencia | comprometimento | concentrados | contempla | contingenciamento | custo | deficit | densidade | desaceleracao | desemprego | desindustrializacao | devolucao | diminuicao | escolar | especificidades | estagnado | exclusivamente | fragmentacao | impedimento | julgar | limitacao | limitacoes | limites | mediante | morte | nenhuma | possibilita | programa | racionalizar | rever | subregistro | terras | tragedias | vedada | vulneraveis | setor específico | por secretaria | por pasta | atuação isolada | atuação setorial | responsabilidade única | sem articulação | fragmentado | fragmentação | divisão de competências | compartimentalização | autônomo | setorializado | ausência de coordenação | falta de integração | falta de diálogo | não articulado | sem parceria | sem cooperação | desarticulado | sem comunicação | sem sinergia | ações separadas | decisão centralizada | competência exclusiva | centralização | comando único | sem descentralização | autoridade exclusiva | gestão isolada | nível único | sem consulta | unilateral | responsabilidade do município | sem apoio da união | ausência de corresponsabilidade | não pactuado | sem pactuação | sem integração federativa | delegação sem recursos", paragrafo, ignore.case = TRUE), -1, 1))

dic2_final <- dic2_transversalidade2 %>% mutate(label_final = case_when(
  label == 1 & label2 == 1 ~ 1,
  label == 1 & label2 == -1 ~ 0,
  label == -1 & label2 == 1 ~ 0,
  label == -1 & label2 == -1 ~ -1
))

# =======================================
# ANALISE EXPLORATÓRIA DO CORPUS DO LOA
# =======================================

contagem <- table(dic2_final$label_final)
print(contagem)

# -------------------------------------------------
#    -1     0     1 
# 24273 70295 26707 
# -------------------------------------------------
bar2 <- barplot(contagem)

#dic_final$label_final

colnames(dic2_final)
dic2_final <- dic2_final[, !colnames(dic2_final) %in% "label"]
dic2_final <- dic2_final[, !colnames(dic2_final) %in% "label2"]

#write.csv(dic2_final, "C://Users//cicer//OneDrive//Documentos//METODOS//CORPUS//dic2_corpus_loa.csv", row.names = FALSE)

print(class(corpus_loa))

# =============================================================================
# ANÁLISE DA FREQUENCIA DOS TOKKENS
# =============================================================================

# Cria uma pasta para os .txt se ainda não existir
# dir.create("C://Users//cicer//OneDrive//Documentos//METODOS//LOA_txt", showWarnings = FALSE)

# Salva cada texto como .txt
for (i in seq_along(df_corpus_loa$text)) {
  caminho <- file.path("C://Users//cicer//OneDrive//Documentos//METODOS//LOA_txt",paste0(tools::file_path_sans_ext(df_corpus_loa$doc_id[i]), ".txt"))
  writeLines(df_corpus_loa$text[i], con = caminho)
}

library(readtext)
library(quanteda)

# Lê todos os .txt como um corpus novamente
textos_loa_readtext <- readtext("C://Users//cicer//OneDrive//Documentos//METODOS//LOA_txt/*.txt", text_field = "text")

# Transforma em corpus do quanteda
corpus_loa2 <- corpus(textos_loa_readtext)
view(textos_loa_readtext)

tokens_loa <- quanteda::corpus_subset(corpus_loa2) |>
  quanteda::tokens(
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_separators = TRUE
  ) |>
  quanteda::tokens_remove(pattern = stopwords("portuguese")) |>
  quanteda::tokens_remove(pattern = c("nº", "1º", "2º", "3º", "4º", "lei", "outubro", "i", "ii", "iii", "iv", "v", "inciso", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"))

# QUEBRANDO O CORPUS EM UM DATAFRAME E ROTULANDO ELE

texto_completo2 <- paste(corpus_loa2, 
                        collapse = "\n")

paragrafos_loa2 <- unlist(strsplit
                     (texto_completo2, 
                       split = "\n\\s*\n", 
                       perl = TRUE))

paragrafos_limpos_loa2 <- trimws(paragrafos_loa2)

paragrafos_filtrados_loa2 <- paragrafos_limpos_loa2[paragrafos_limpos_loa2 != ""]

df_paragrafos_loa2 <- data.frame(paragrafo = paragrafos_filtrados_loa2, 
                                stringsAsFactors = FALSE)


dic2_transversalidade_loa2 <- df_paragrafos_loa2 %>%
  mutate(label=ifelse(grepl("integrar | integração | intersetorial | articulação | transversal | multidisciplinar | interinstitucional | cooperação | coordenação | consórcio | parceria | colaborativo | convergência | municípios | estados | união | entes federados | instâncias | interfederativo | descentralização | governança | colegiado | comitê gestor | conselhos | pactuação | corresponsabilidade | plano integrado | orçamento integrado | matriz intersetorial | planejamento conjunto | agenda comum | metas compartilhadas | indicadores integrados | programas estruturantes | iniciativas conjuntas | financiamento integrado | participação social | controle social | audiências públicas | consulta pública | escuta ativa | transparência | sociedade civil | deliberação | interseccionalidade | equidade | inclusão | grupos vulneráveis | promoção da igualdade | combate às desigualdades | ações afirmativas | direitos humanos | diversidade | justiça social | promover | executar | garantir | implantar | manutencao | fortalecer | desenvolver | ampliacao | crescimento | elevar | transferencia | atender | beneficios | realizar | ampliar | assegurar | execucao | fornecer | melhorar | promocao | recursos | acesso | atendimento | criacao | exercer | implantacao | manter | melhoria | modernizar | aperfeicoar | colaboracao | concessao | consolidacao | coordenar | desenvolvimento | eficiencia | estimular | executados | fortalecimento | implementar | incrementar | integracao | integrada | integrar | metas | otimizar | pagamento | participacao | participar | recuperacao | transparencia | viabilizar | abranger | abrangera | acao | acessibilidade | acompanhar | adaptavel realidade | agregar | alcancadas | alcance | alocacao | ampliacao cidadania ativa grupos discriminados | ampliacao integracao | ampliar melhoria | aperfeicoamento | aportar | aquisicao | arrecadacao | articulacao | atraente | aumento | avaliacao | capacitar | compensacao | compromisso | conscientizacao | conscientizar | conservar | consolidar | construcao | construir | contribuir | controle social | convenio | convergencia | cooperacao | creditos | custeio | defender | descentralizar | desenvolvidas | desigualdade infraestrutura | determinacao | direcionador | disponibilizar | disponibilizara | divulgar | efetivar | elaboracao", paragrafo, ignore.case = TRUE), 1, -1))


dic2_transversalidade2_loa2 <- dic2_transversalidade_loa2  %>%
  mutate(label2=ifelse(grepl("alienacao | apoio | despesas | limite | reducao | assentamentos | atingida | ausencia | comprometimento | concentrados | contempla | contingenciamento | custo | deficit | densidade | desaceleracao | desemprego | desindustrializacao | devolucao | diminuicao | escolar | especificidades | estagnado | exclusivamente | fragmentacao | impedimento | julgar | limitacao | limitacoes | limites | mediante | morte | nenhuma | possibilita | programa | racionalizar | rever | subregistro | terras | tragedias | vedada | vulneraveis | setor específico | por secretaria | por pasta | atuação isolada | atuação setorial | responsabilidade única | sem articulação | fragmentado | fragmentação | divisão de competências | compartimentalização | autônomo | setorializado | ausência de coordenação | falta de integração | falta de diálogo | não articulado | sem parceria | sem cooperação | desarticulado | sem comunicação | sem sinergia | ações separadas | decisão centralizada | competência exclusiva | centralização | comando único | sem descentralização | autoridade exclusiva | gestão isolada | nível único | sem consulta | unilateral | responsabilidade do município | sem apoio da união | ausência de corresponsabilidade | não pactuado | sem pactuação | sem integração federativa | delegação sem recursos", paragrafo, ignore.case = TRUE), -1, 1))

dic2_final_loa2 <- dic2_transversalidade2_loa2 %>% mutate(label_final = case_when(
  label == 1 & label2 == 1 ~ 1,
  label == 1 & label2 == -1 ~ 0,
  label == -1 & label2 == 1 ~ 0,
  label == -1 & label2 == -1 ~ -1
))

dic2_final_loa2$label_final = as.factor(dic2_final_loa2$label_final)
plot(dic2_final_loa2$label_final)

colnames(dic2_final_loa2)
dic2_final_loa2 <- dic2_final_loa2[, !colnames(dic2_final_loa2) %in% "label"]
dic2_final_loa2 <- dic2_final_loa2[, !colnames(dic2_final_loa2) %in% "label2"]

#write.csv(dic2_final_loa2, "C://Users//cicer//OneDrive//Documentos//METODOS//CORPUS//corpus_preprocessado_loa.csv", row.names = FALSE)

# Se quiser salvar em CSV:
# write.csv(amostras, "amostras_aleatorias.csv", row.names = FALSE)

# =============================================================================
# BALANCEANDO AMOSTRAS [balanceamento por undersampling]

# Número-alvo de amostras por classe
n_amostras <- 30000

# Verifique a estrutura da base
str(dic2_final_loa2)

# Verifique os valores únicos em sua variável de classe
table(dic2_final_loa2$label_final)

# Realiza a amostragem estratificada (balanceada)
set.seed(42)  # Para reprodutibilidade
dic2_balanceado <- dic2_final_loa2 %>%
  group_by(label_final) %>%
  sample_n(size = min(n_amostras, n()), replace = FALSE) %>%
  ungroup()

# Verifica a nova distribuição das classes
table(dic2_balanceado$label_final)

# Salva o resultado (opcional)
#write.csv(dic2_balanceado, "C://Users//cicer//OneDrive//Documentos//METODOS//PREPROCESSADOS//loa2_balanceado_preprocessado.csv", row.names = FALSE)


# =============================================================================


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

# =============================================================================

###
textplot_xray(
  kwic(tokens_loa, pattern = "integrar"),
  kwic(tokens_loa, pattern = "transversalidade"),
  kwic(tokens_loa, pattern = "setor"),
  scale = "absolute"
)

###
library(quanteda.textstats)
freq_grouped_loa <- textstat_frequency(dfm(tokens_loa), groups = )

# Filter the term "american"
freq_transversalidade_loa <- subset(freq_grouped_loa, freq_grouped_loa$feature %in% "transversalidade")  

library(ggplot2)

plot1_transv_loa <- ggplot(freq_transversalidade_loa, aes(x = frequency, y = group)) +
  geom_point() + 
  geom_text(aes(label = frequency), vjust = 1.5) +
  scale_x_continuous(
    limits = c(0, 200), 
    breaks = c(seq(0, 100, 40), 143, 200)
  ) +
  labs(x = "Frequência", y = NULL, title = 'LOA') +
  theme(plot.title = element_text(hjust = 0.5))

print(plot1_transv_loa)


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



corpus_transv = read.csv("C:/Users/cicer/OneDrive/Documentos/METODOS/CORPUS/CORPUS_TRANSVERSALIDADE.csv",
                            sep = ",")
glimpse(corpus_transv)

library(dplyr)
set.seed(42)  # Para reprodutibilidade

corpus_transv$CLASSIFICAÇÃO. = as.factor(corpus_transv$CLASSIFICAÇÃO.)

# Sorteia 10 amostras aleatórias da categoria neutra (0)
amostras_neutras <- corpus_transv %>%
  filter(CLASSIFICAÇÃO. == "NEUTRO") %>%
  slice_sample(n = 1) %>%
  select(TRECHO)
print(amostras_neutras)

# Sorteia 10 amostras aleatórias da categoria positiva (1)
amostras_pos <- corpus_transv %>%
  filter(CLASSIFICAÇÃO. == "POSITIVO") %>%
  slice_sample(n = 10) %>%
  select(TRECHO)
print(amostras_pos)

# Sorteia 10 amostras aleatórias da categoria negativa (-1)
amostras_neg <- corpus_transv %>%
  filter(CLASSIFICAÇÃO. == "NEGATIVO") %>%
  slice_sample(n = 1) %>%
  select(corpus_transv$TRECHO)
print(amostras_neg)

