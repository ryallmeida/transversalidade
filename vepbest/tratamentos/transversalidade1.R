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

# -------------------------------------------------
# LEITURA DOS ARQUIVOS E CONSTRUÇÃO DO CORPUS
# -------------------------------------------------
arquivos_pdf <- list.files(
  path       = "C://Users//cicer//OneDrive//Documentos//METODOS//PPA",
  pattern    = "\\.pdf$", 
  full.names = TRUE
)

# -------------------------------------------------
# 2. Lê o texto de cada PDF e colapsa todas as páginas
# -------------------------------------------------

textos_unificados <- sapply(
  arquivos_pdf,
  function(arq) {
    paginas <- pdf_text(arq)
    paste(paginas, collapse = " ")
  },
  USE.NAMES = FALSE
)

# -------------------------------------------------
# 3. Cria um data.frame para alimentar o corpus
# -------------------------------------------------

df_corpus <- data.frame(
  doc_id = basename(arquivos_pdf),
  text   = textos_unificados,
  stringsAsFactors = FALSE
)

# -------------------------------------------------
# 4. Constrói o corpus com quanteda (classe pura)
# -------------------------------------------------

corpus_pdf <- corpus(
  x             = df_corpus,
  text_field    = "text",
  docid_field   = "doc_id"
)

# -------------------------------------------------
# 5. Inspeção rápida
# -------------------------------------------------

print(class(corpus_pdf))   
summary(corpus_pdf)         

# write.csv(corpus_pdf, "C://Users//cicer//OneDrive//Documentos//METODOS//CORPUS//PPAs_08-27.csv", row.names = FALSE)


# =================================================
#MONTANDO O DATRAFRAME PARA APLICAR OS DICIONÁRIO E ELABORAR O LABEL FINAL

# Junta todo o conteúdo em uma única string, com quebra de linha entre páginas
texto_completo <- paste(textos_unificados, 
                        collapse = "\n")

# Divide o texto em parágrafos com base em quebras de linha duplas
paragrafos <- unlist(strsplit
                     (texto_completo, 
                              split = "\n\\s*\n", 
                              perl = TRUE))

# Limpa espaços em branco antes e depois dos parágrafos
paragrafos_limpos <- trimws(paragrafos)

# Remove parágrafos vazios (caso existam)
paragrafos_filtrados <- paragrafos_limpos[paragrafos_limpos != ""]

# Cria um data frame com os parágrafos
df_paragrafos <- data.frame(paragrafo = paragrafos_filtrados, 
                            stringsAsFactors = FALSE)

# (Opcional) Visualiza os primeiros parágrafos
head(df_paragrafos)

# Salva como CSV (pra usar depois no Machine Learning!)
#write.csv(df_paragrafos, "paragrafos_extraidos.csv", row.names = FALSE)

# ==================================

# Lista todos os arquivos PDF na pasta
arquivos_pdf <- list.files(path = "C://Users//cicer//OneDrive//Documentos//METODOS//PPA", 
                           pattern = "\\.pdf$", 
                           full.names = TRUE)

# Usa pdf_text em cada arquivo
textos_pdf <- lapply(arquivos_pdf, pdf_text)

# Colapsa as páginas de cada documento em um único texto
textos_unificados <- lapply(textos_pdf, 
                            function(paginas) paste(paginas, 
                                                    collapse = " "))
# Cria o corpus a partir dos textos unificados
corpus_pdf <- Corpus(textos_unificados, 
                     docnames = basename(arquivos_pdf))  

textos_unificados <- sapply(arquivos_pdf, 
                            function(arq) {
                              paste(pdf_text(arq), 
                                                 collapse = " ")})

# 3. Cria o corpus com quanteda (agora os textos são character)
corpus_pdf <- corpus(textos_unificados, docnames = basename(arquivos_pdf))
# summary(corpus_pdf)

# ==================================
# MONTANDO O DICINÁRIO A PARTIR DO  FORMS

data_forms <- read.csv("C://Users//cicer//OneDrive//Documentos//METODOS//FORMULARIO//CORPUS_TRANSVERSALIDADE_FORMS.csv",
                       header = TRUE,
                       sep = ",")

data_forms <- as.data.frame(data_forms)
#glimpse(data_forms)

library(dplyr)

data_forms$CLASSIFICAÇÃO. <- as.factor(data_forms$CLASSIFICAÇÃO.)  
plot(data_forms$CLASSIFICAÇÃO.)

library(dplyr)

# POSITIVO
palavras_positivas <- data_forms$PALAVRA.CHAVE_ACIONADA[data_forms$CLASSIFICAÇÃO == "POSITIVO"]

# NEGATIVO
palavras_negativas <- data_forms$PALAVRA.CHAVE_ACIONADA[data_forms$CLASSIFICAÇÃO == "NEGATIVO"]

# NEUTRO
palavras_neutras <- data_forms$PALAVRA.CHAVE_ACIONADA[data_forms$CLASSIFICAÇÃO == "NEUTRO"]

# ===================================

# Carregar pacotes necessários
pacman::p_load(stringr, stringi)

# Padronização de função
padronizar_palavras <- function(vetor) {
  vetor %>%
    tolower() %>%                          
    stri_trans_general("Latin-ASCII") %>% 
    str_replace_all("[[:punct:]]", "") %>%
    str_squish()                         
}

# Aplicar a função de padronização
palavras_positivas <- padronizar_palavras(data_forms$PALAVRA.CHAVE_ACIONADA[data_forms$CLASSIFICAÇÃO == "POSITIVO"])
palavras_negativas <- padronizar_palavras(data_forms$PALAVRA.CHAVE_ACIONADA[data_forms$CLASSIFICAÇÃO == "NEGATIVO"])
palavras_neutras   <- padronizar_palavras(data_forms$PALAVRA.CHAVE_ACIONADA[data_forms$CLASSIFICAÇÃO == "NEUTRO"])

# Contar frequência com dplyr + tibble
frequencia_positivas <- as_tibble(palavras_positivas) %>%
  count(value, sort = TRUE)
print(frequencia_positivas)

frequencia_negativas <- as_tibble(palavras_negativas) %>%
  count(value, sort = TRUE)
print(frequencia_negativas)

frequencia_neutras <- as_tibble(palavras_neutras) %>%
  count(value, sort = TRUE)
print(frequencia_neutras)

# ===================================
# CONSTRUÇÃO DO DICIONÁRIO PROPRIAMENTE DITA
# ===================================


dic_transversalidade1 <- df_paragrafos %>%
  mutate(label=ifelse(grepl("promover | executar | garantir | implantar | manutencao | fortalecer | desenvolver | ampliacao | crescimento | elevar | transferencia | atender | beneficios | realizar | ampliar | assegurar | execucao | fornecer | melhorar | promocao | recursos | acesso | atendimento | criacao | exercer | implantacao | manter | melhoria | modernizar | aperfeicoar | colaboracao | concessao | consolidacao | coordenar | desenvolvimento | eficiencia | estimular | executados | fortalecimento | implementar | incrementar | integracao | integrada | integrar | metas | otimizar | pagamento | participacao | participar | recuperacao | transparencia | viabilizar | abranger | abrangera | acao | acessibilidade | acompanhar | adaptavel realidade | agregar | alcancadas | alcance | alocacao | ampliacao cidadania ativa grupos discriminados | ampliacao integracao | ampliar melhoria | aperfeicoamento | aportar | aquisicao | arrecadacao | articulacao | atraente | aumento | avaliacao | capacitar | compensacao | compromisso | conscientizacao | conscientizar | conservar | consolidar | construcao | construir | contribuir | controle social | convenio | convergencia | cooperacao | creditos | custeio | defender | descentralizar | desenvolvidas | desigualdade infraestrutura | determinacao | direcionador | disponibilizar | disponibilizara | divulgar | efetivar | elaboracao"
, paragrafo, ignore.case = TRUE), 1, -1))


dic_transversalidade2 <- df_paragrafos %>%
  mutate(label2=ifelse(grepl("alienacao | apoio | despesas | limite | reducao | assentamentos | atingida | ausencia | comprometimento | concentrados | contempla | contingenciamento | custo | deficit | densidade | desaceleracao | desemprego | desindustrializacao | devolucao | diminuicao | escolar | especificidades | estagnado | exclusivamente | fragmentacao | impedimento | julgar | limitacao | limitacoes | limites | mediante | morte | nenhuma | possibilita | programa | racionalizar | rever | subregistro | terras | tragedias | vedada | vulneraveis", paragrafo, ignore.case = TRUE), -1, 1))

dic_final <- dic_transversalidade2 %>% mutate(label_final = case_when(
      label == 1 & label2 == 1 ~ 1,
      label == 1 & label2 == -1 ~ 0,
      label == -1 & label2 == 1 ~ 0,
      label == -1 & label2 == -1 ~ -1
    )
  )


dic_final$label_final

# =======================================

dic_transversalidade1 <- df_paragrafos %>%
  mutate(label = ifelse(grepl("promover | executar | garantir | implantar | manutencao | fortalecer | desenvolver | ampliacao | crescimento | elevar | transferencia | atender | beneficios | realizar | ampliar | assegurar | execucao | fornecer | melhorar | promocao | recursos | acesso | atendimento | criacao | exercer | implantacao | manter | melhoria | modernizar | aperfeicoar | colaboracao | concessao | consolidacao | coordenar | desenvolvimento | eficiencia | estimular | executados | fortalecimento | implementar | incrementar | integracao | integrada | integrar | metas | otimizar | pagamento | participacao | participar | recuperacao | transparencia | viabilizar | abranger | abrangera | acao | acessibilidade | acompanhar | adaptavel realidade | agregar | alcancadas | alcance | alocacao | ampliacao cidadania ativa grupos discriminados | ampliacao integracao | ampliar melhoria | aperfeicoamento | aportar | aquisicao | arrecadacao | articulacao | atraente | aumento | avaliacao | capacitar | compensacao | compromisso | conscientizacao | conscientizar | conservar | consolidar | construcao | construir | contribuir | controle social | convenio | convergencia | cooperacao | creditos | custeio | defender | descentralizar | desenvolvidas | desigualdade infraestrutura | determinacao | direcionador | disponibilizar | disponibilizara | divulgar | efetivar | elaboracao", paragrafo, ignore.case = TRUE), 1, -1))

dic_transversalidade2 <- dic_transversalidade1 %>%
  mutate(label2 = ifelse(grepl("alienacao | apoio | despesas | limite | reducao | assentamentos | atingida | ausencia | comprometimento | concentrados | contempla | contingenciamento | custo | deficit | densidade | desaceleracao | desemprego | desindustrializacao | devolucao | diminuicao | escolar | especificidades | estagnado | exclusivamente | fragmentacao | impedimento | julgar | limitacao | limitacoes | limites | mediante | morte | nenhuma | possibilita | programa | racionalizar | rever | subregistro | terras | tragedias | vedada | vulneraveis", paragrafo, ignore.case = TRUE), -1, 1))

dic_final <- dic_transversalidade2 %>%
  mutate(label_final = case_when(
    label == 1  & label2 == 1  ~ 1,
    label == 1  & label2 == -1 ~ 0,
    label == -1 & label2 == 1  ~ 0,
    label == -1 & label2 == -1 ~ -1
  ))

#write.csv(dic_final, "C:/Users/cicer/OneDrive/Documentos/METODOS/PPA/CLASSIFICACAO/corpus_ppa_transversalidade.csv", row.names = FALSE)

# =======================================
# ANALISE EXPLORATÓRIA DO CORPUS DO PPA
# =======================================

contagem <- table(dic_final$label_final)
print(contagem)
#-1     0     1 
#1057 20814  4578

bar1 <- barplot(contagem)

# CONFERINDO SE A MÉDIA DE PALAVRAS NEUTRAS A PARTIR DO DICIONARIO DE PALAVRAS NEUTRAS, CONFEREM

dic_transversalidade3 <- df_paragrafos %>%
  mutate(label3 = ifelse(grepl("apoiar | ampliar | ampliacao | coordenar | nenhuma | apoio | fortalecer | promover | adotar | consolidar | estimular | implementar | monitorar | avaliacao | desenvolver | divulgar | garantir | melhoria | adequacao | adequar | amortizacao | assegurar | atender | avaliar | gerir | manter | propoe | reduzir | acompanhamento | agregar | ajuda | alvo | amparar | apoiar ampliar | apresentar | aquisicao | articulado | articular | articularse | assumir | centralizar | conscientizar | conservar | controle | coordenara | democratizar | descentralizar | dinamicidade | disciplinando | divilgar | dotar | eficientizar | elaboracao | estima | estrategia | estruturar | facilitar | fomentar | fomentara | formulacao | fortalecimento | gerenciar | hipotese | implantar | implementacao | incentivar | incentivo | integrado | interiorizacao | intervencao | melhorar oferecer | mobilizar | modernizar | modificar | monitoramento | oferecer | participar | potencializar | preparar inserir | preservar | prevencao | publicar | qualificacao | recomposicao | regular | requalificar | revisar | revitalizar | subsidiar | superar | supervisionar | universalizacao | viabilizar", paragrafo, ignore.case = TRUE), 1, -1))

contagem2 <- table(dic_transversalidade3$label3)
print(contagem2)
barplot(contagem2)

#ONDE -1 INDICAM AS AMOSTRAS NEUTRAS, E 1 INDICAM AMOSTRAS POSITIVAS 
#-1     1 
#22374  4075 
#ESTA CORRETO REALMENTE A QUANTIDADE DE AMOSTRAS BATEM 

# ===================================================================================
# REFERÊNCIAS 
# ===================================================================================

### R DEVELOPMENT CORE TEAM. R: Uma linguagem e ambiente para computação estatística . Viena: R Foundation for Statistical Computing, 2011. ISBN 3-900051-07-0. Disponível em: http://www.R-project.org/ . Acessado em : 6 de jan. de 2025.

