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


dfm_trim(min_freq = 300, max_terms = 500, verbose = FALSE)



