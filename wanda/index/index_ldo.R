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
               stringr,
               tidytext,
               purrr,
               scales,
               stopwords,
               viridis)


#-----------------------------------------------------------
# 1. Caminho para a pasta com os PDFs
#-----------------------------------------------------------
pasta <- "C://Users//ryall//Documents//transversalidade//LDO"
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
resultado <- map_df(arquivos, function(arq) {
  
  nome_arquivo <- basename(arq)
  ano <- extrair_ano(nome_arquivo)
  
  texto_bruto <- pdf_text(arq)
  texto_completo <- paste(texto_bruto, collapse = "\n")
  
  paragrafos <- unlist(str_split(texto_completo, "\\n\\s*\\n"))
  
  tibble(
    arquivo = nome_arquivo,
    ano = ano,
    paragrafo_id = seq_along(paragrafos),
    texto = str_squish(paragrafos)
  )
})

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

#-----------------------------------------------------------
# 6. Tokenização: transformar parágrafos em palavras
#-----------------------------------------------------------

dados_tokens <- resultado_limpo %>%
  unnest_tokens(word, texto_limpo)

#-----------------------------------------------------------
# 7. Ler léxico
#-----------------------------------------------------------
lexico <- read.csv("C:/Users/ryall/Desktop/R/transversalidade/wanda/preprocessamento/dataframes/lexico_dic.csv.csv", encoding = "UTF-8")

# trocando virgula para ponto como separador decimal, e fazendo conversão de variavel

lexico$intensidade <- lapply( lexico$intensidade, function(x) { as.numeric(gsub(",", ".", x)) })

lexico_unique <- lexico %>% distinct(termo, .keep_all = TRUE)

#-----------------------------------------------------------
# 8. Juntar tokens com o léxico
#-----------------------------------------------------------
dados_sentimento <- dados_tokens %>%
  left_join(lexico_unique, by = c("word" = "termo"))

# converter intensidade para numérica
dados_sentimento <- dados_sentimento %>%
  mutate(intensidade = as.numeric(intensidade),
         ano = as.numeric(ano))

#-----------------------------------------------------------
# 8.1 Colocar classificação literal pelo dicionário de termos
#-----------------------------------------------------------

dic2_transversalidade <- dados_sentimento %>%
  mutate(label=ifelse(grepl("integrar | integração | intersetorial | articulação | transversal | multidisciplinar | interinstitucional | cooperação | coordenação | consórcio | parceria | colaborativo | convergência | municípios | estados | união | entes federados | instâncias | interfederativo | descentralização | governança | colegiado | comitê gestor | conselhos | pactuação | corresponsabilidade | plano integrado | orçamento integrado | matriz intersetorial | planejamento conjunto | agenda comum | metas compartilhadas | indicadores integrados | programas estruturantes | iniciativas conjuntas | financiamento integrado | participação social | controle social | audiências públicas | consulta pública | escuta ativa | transparência | sociedade civil | deliberação | interseccionalidade | equidade | inclusão | grupos vulneráveis | promoção da igualdade | combate às desigualdades | ações afirmativas | direitos humanos | diversidade | justiça social | promover | executar | garantir | implantar | manutencao | fortalecer | desenvolver | ampliacao | crescimento | elevar | transferencia | atender | beneficios | realizar | ampliar | assegurar | execucao | fornecer | melhorar | promocao | recursos | acesso | atendimento | criacao | exercer | implantacao | manter | melhoria | modernizar | aperfeicoar | colaboracao | concessao | consolidacao | coordenar | desenvolvimento | eficiencia | estimular | executados | fortalecimento | implementar | incrementar | integracao | integrada | integrar | metas | otimizar | pagamento | participacao | participar | recuperacao | transparencia | viabilizar | abranger | abrangera | acao | acessibilidade | acompanhar | adaptavel realidade | agregar | alcancadas | alcance | alocacao | ampliacao cidadania ativa grupos discriminados | ampliacao integracao | ampliar melhoria | aperfeicoamento | aportar | aquisicao | arrecadacao | articulacao | atraente | aumento | avaliacao | capacitar | compensacao | compromisso | conscientizacao | conscientizar | conservar | consolidar | construcao | construir | contribuir | controle social | convenio | convergencia | cooperacao | creditos | custeio | defender | descentralizar | desenvolvidas | desigualdade infraestrutura | determinacao | direcionador | disponibilizar | disponibilizara | divulgar | efetivar | elaboracao", texto, ignore.case = TRUE), 1, -1))


dic2_transversalidade2 <- dic2_transversalidade %>%
  mutate(label2=ifelse(grepl("alienacao | apoio | despesas | limite | reducao | assentamentos | atingida | ausencia | comprometimento | concentrados | contempla | contingenciamento | custo | deficit | densidade | desaceleracao | desemprego | desindustrializacao | devolucao | diminuicao | escolar | especificidades | estagnado | exclusivamente | fragmentacao | impedimento | julgar | limitacao | limitacoes | limites | mediante | morte | nenhuma | possibilita | programa | racionalizar | rever | subregistro | terras | tragedias | vedada | vulneraveis | setor específico | por secretaria | por pasta | atuação isolada | atuação setorial | responsabilidade única | sem articulação | fragmentado | fragmentação | divisão de competências | compartimentalização | autônomo | setorializado | ausência de coordenação | falta de integração | falta de diálogo | não articulado | sem parceria | sem cooperação | desarticulado | sem comunicação | sem sinergia | ações separadas | decisão centralizada | competência exclusiva | centralização | comando único | sem descentralização | autoridade exclusiva | gestão isolada | nível único | sem consulta | unilateral | responsabilidade do município | sem apoio da união | ausência de corresponsabilidade | não pactuado | sem pactuação | sem integração federativa | delegação sem recursos", texto, ignore.case = TRUE), -1, 1))

dic2_final_ldo <- dic2_transversalidade2 %>% mutate(label_final = case_when(
  label == 1 & label2 == 1 ~ 1,
  label == 1 & label2 == -1 ~ 0,
  label == -1 & label2 == 1 ~ 0,
  label == -1 & label2 == -1 ~ -1
))

dados_sentimento <- dic2_final_ldo

#-----------------------------------------------------------
# 8.2 Adicionar matriz de TF-IDF no banco de dados
#-----------------------------------------------------------

# FREQUENICA RELATIVA DAS 1000 PALAVRAS MAIS FREQUENTS NO CORPUS DO LDO

df_tf_idf_ldo <- read.csv("C:/Users/ryall/Desktop/R/transversalidade/wanda/preprocessamento/dataframes/matriz_td-idf_ldo.csv")


dados_sentimento <- dados_sentimento %>%
  left_join(
    df_tf_idf_ldo %>%
      select(word, `topfeatures.dfmat_tfidf..n...1000.`),
    by = "word"
  )

dados_sentimento <- dados_sentimento %>%
  rename(TF_IDF = `topfeatures.dfmat_tfidf..n...1000.`)

#-----------------------------------------------------------
# 8.3 somando as intensidades de cada frase dada as palavras que aparecem
#-----------------------------------------------------------

lexico_tratado <- lexico %>%
  group_by(termo) %>%
  summarise(
    intensidade = sum(intensidade, na.rm = TRUE),
    .groups = "drop"
  )

sentimento_texto <- dados_sentimento %>%
  mutate(id_texto = row_number()) %>%
  
  unnest_tokens(palavra, texto) %>%
  mutate(palavra = str_to_lower(palavra)) %>%
  
  inner_join(
    lexico %>% rename(intensidades = intensidade),
    by = c("palavra" = "termo")
  ) %>%
  
  group_by(id_texto) %>%
  summarise(
    sentimento_total = sum(.data$intensidades, na.rm = TRUE),
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

# Quais palavras são, ao mesmo tempo, semanticamente relevantes 
# e emocionalmente carregadas de transversalidade dentro das ferramentas de planejamento do estado?
#

# NOTO POSTERIORMENTE QUE CALCULEI ESSA VARIAVEL SEM TRATAR OS NAS, FAÇO ISSO LOGO MAIS A FRENTE E CRIO UMA NOVA VARIAVEL tfidf_intensidade2

dados_sentimento <- dados_sentimento %>%
  mutate(
    tfidf_intensidade = TF_IDF * intensidades
  )

# -----------------------------------------------------------
# CLASSIFICANDO OS DADOS, SEM MANIPULAR PELA SOMA DAS INTENSIDADES
# -----------------------------------------------------------

dados_sentimento <- dados_sentimento %>%
  mutate(
    tipo = case_when(
      label_final > 0  ~ "POSITIVO",
      label_final == 0 ~ "NEUTRO",
      label_final < 0  ~ "NEGATIVO",
      TRUE ~ tipo  # mantém o valor antigo se houver NA em label_final
    )
  )


dados_sentimento <- readr::read_csv(
  "C:/Users/ryall/Documents/transversalidade/DATASETS/corpus_ldo.csv",
  show_col_types = FALSE
)

prop.table(table(dados_sentimento$tipo))


# -----------------------------------------------------------

dados_sentimento$intensidades[is.na(dados_sentimento$intensidades)] <- 0

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
#HYPERPOSITIVO    NEGATIVO_1    NEGATIVO_2 
#   0.23059326    0.13119511    0.25112080 
#NEGATIVO_3        NEUTRO    POSITIVO_2 
#0.09252748    0.01005650    0.12208285 
#POSITIVO_4 
#0.16242400 

# -----------------------------------------------------------
# CLASSIFICANDO OS DADOS, ESTRESSANDO A VARIAVEL PELO TF-IDF
# -----------------------------------------------------------

dados_sentimento$tfidf_intensidade[is.na(dados_sentimento$tfidf_intensidade)] <- 0

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

#HYPERNEGATIVO HYPERPOSITIVO    NEGATIVO_1 
#   0.04106754    0.16193908    0.17944006 
#       NEUTRO    POSITIVO_1    POSITIVO_2 
#   0.40858815    0.02764001    0.05563683 
#   POSITIVO_3    POSITIVO_4 
#   0.03617940    0.08950893 

# -----------------------------------------------------------
# CLASSIFICANDO OS DADOS, ESTRESSANDO A VARIAVEL PELO TF-IDF E DEPOIS DE TRATAR OS NAS COM ZERO
# -----------------------------------------------------------
# ANÁLISE DA CLASIFICAÇÃO TIPO4 PELA VARIAVEL tfidf_intensidade2

dados_sentimento$TF_IDF[is.na(dados_sentimento$TF_IDF)] <- 0

dados_sentimento <- dados_sentimento %>%
  mutate(
    tfidf_intensidade2 = TF_IDF * intensidades
  )

bp3 <- boxplot.stats(dados_sentimento$tfidf_intensidade2)

lim_inf <- bp3$stats[1]
q1      <- bp3$stats[2]
med     <- bp3$stats[3]
q3      <- bp3$stats[4]
lim_sup <- bp3$stats[5]

neg_q1_med <- seq(q1, med, length.out = 4)  # 3 partes
pos_med_q3 <- seq(med, q3, length.out = 4)

dados_sentimento <- dados_sentimento %>%
  mutate(
    tipo4 = case_when(
      
      tfidf_intensidade2 < lim_inf ~ "HYPERNEGATIVO",
      tfidf_intensidade2 > lim_sup ~ "HYPERPOSITIVO",
      
      tfidf_intensidade2 == 0 ~ "NEUTRO",
      
      tfidf_intensidade2 >= lim_inf & tfidf_intensidade2 < q1 ~ "NEGATIVO_1",
      
      tfidf_intensidade2 >= neg_q1_med[1] & tfidf_intensidade2 < neg_q1_med[2] ~ "NEGATIVO_2",
      tfidf_intensidade2 >= neg_q1_med[2] & tfidf_intensidade2 < neg_q1_med[3] ~ "NEGATIVO_3",
      tfidf_intensidade2 >= neg_q1_med[3] & tfidf_intensidade2 < med           ~ "NEGATIVO_4",
      
      tfidf_intensidade2 > med & tfidf_intensidade2 < pos_med_q3[2] ~ "POSITIVO_1",
      tfidf_intensidade2 >= pos_med_q3[2] & tfidf_intensidade2 < pos_med_q3[3] ~ "POSITIVO_2",
      tfidf_intensidade2 >= pos_med_q3[3] & tfidf_intensidade2 < q3            ~ "POSITIVO_3",
      
      tfidf_intensidade2 >= q3 & tfidf_intensidade2 <= lim_sup ~ "POSITIVO_4",
      
      TRUE ~ NA_character_
    )
  )

prop.table(table(dados_sentimento$tipo4))

# HYPERNEGATIVO HYPERPOSITIVO    NEGATIVO_1 
#    0.04106754    0.16193908    0.17944006 
#        NEUTRO    POSITIVO_1    POSITIVO_2 
#    0.40858815    0.02764001    0.05563683 
#    POSITIVO_3    POSITIVO_4 
#    0.03617940    0.08950893 

# CONCLUSÃO, DEPOOIS DE IMPUTAR OS ZEROS NAO MUDOU-SE NADA, OS RESULTADOS SÃO EQUIVALENTES 


plot_proporcao_tipo <- function(df, var_cat, titulo = NULL) {
  
  dados_prop <- df %>%
    filter(!is.na(.data[[var_cat]])) %>%
    count(.data[[var_cat]]) %>%
    mutate(prop = 100 * n / sum(n))
  
  ggplot(dados_prop, aes(
    x = prop,
    y = "",
    fill = .data[[var_cat]]
  )) +
    geom_bar(
      stat = "identity",
      position = "stack",
      width = 0.6,
      color = NA
    ) +
    geom_text(
      aes(label = paste0(round(prop, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 3.3,
      fontface = "bold"
    ) +
    scale_fill_viridis_d(
      option = "rocket",
      direction = -1,
      begin = 0.15,
      end = 0.85,
      name = "Categoria de sentimento"
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, 100)
    ) +
    labs(
      x = "Proporção (%)",
      y = NULL,
      title = titulo
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

p_tipo  <- plot_proporcao_tipo(
  dados_sentimento,
  "tipo",
  titulo = "Distribuição das categorias — tipo"
)

p_tipo2 <- plot_proporcao_tipo(
  dados_sentimento,
  "tipo2",
  titulo = "Distribuição das categorias — tipo2"
)

p_tipo3 <- plot_proporcao_tipo(
  dados_sentimento,
  "tipo3",
  titulo = "Distribuição das categorias — tipo3"
)

print(p_tipo3)


# ==============================================================================

#-----------------------------------------------------------
# 9. Resumo por ano
#-----------------------------------------------------------
dados_sentimento_resumo <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    matched_tokens = sum(!is.na(intensidades)),
    sum_polarity   = sum(intensidades, na.rm = TRUE),
    mean_matched   = ifelse(matched_tokens == 0, 0, sum_polarity / matched_tokens),
    mean_all       = mean(replace_na(intensidades, 0)),
    .groups = "drop"
  )

print(dados_sentimento_resumo)


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

# write.csv(dados_sentimento_resumo, "C:/Users/ryall/Documents/transversalidade/DATASETS/resumo_ldo.csv")

# --- 12. Visualização ---
p1 <- ggplot(serie_temporal, aes(x = ano, y = sentimento_diario_mean)) +
  geom_line(color = "blue") +
  geom_point(color = "black") +
  labs(title = "Sentiment Index (Average Intensity)",
       x = "Date", y = "Average sentiment") +
  theme_minimal() #+
#scale_x_date(date_labels = "%d-%b", date_breaks = "1 week")

p2 <- ggplot(serie_temporal, aes(x = ano, y = sentimento_diario_total)) +
  geom_col(fill = "darkred") +
  labs(title = "Sentiment Index (Gross Magnitude)",
       x = "Date", y = "Sum of the sentiment") +
  theme_minimal() #+
#scale_x_date(date_labels = "%d-%b", date_breaks = "1 week")

p3 <- ggplot(serie_temporal, aes(x = ano)) +
  # Linha do índice normalizado
  geom_line(aes(y = sent_norm, color = "Normalized Index"), linewidth = 0.9, alpha = 0.8) +
  
  # Linha suavizada
  geom_line(aes(y = sent_smooth, color = "Smoothed Index"), linewidth = 1.3) +
  
  # Escala de cores personalizada
  scale_color_manual(values = c("Normalized Index" = "#6a5acd",  # roxo suave
                                "Smoothed Index"   = "#ffa500")) +  # laranja
  
  # Rótulos e título
  labs(
    title = "Normalized and Smoothed Sentiment Index",
    subtitle = "Sentiment toward the Magnitsky Law on X (Twitter)",
    x = "Year", 
    y = "Sentiment (−1 to 1)",
    color = "Series"
  ) +
  
  # Eixo de datas formatado
  #scale_x_date(
  #  date_labels = "%d-%b", 
  #  date_breaks = "1 week"
  #) +
  
  # Tema visual aprimorado
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Mostrar gráficos
p1; p2; p3


##############RODAR ATÉ ANTES DESSA LINHA#######################

###############################################
#outra opção - NSS

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    positivos = sum(intensidade > 0, na.rm = TRUE),
    negativos = sum(intensidade < 0, na.rm = TRUE),
    total     = n(),
    net_sent  = (positivos - negativos) / total
  )

#O NSS indica claramente se o discurso está “puxado” para o positivo ou negativo.
ggplot(serie_temporal, aes(x = ano, y = net_sent)) +
  geom_line(linewidth = 1.2, color = "#1f77b4") +
  geom_point(size = 2, color = "#1f77b4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  labs(
    title = "Net Sentiment Score (NSS) por Ano",
    subtitle = "Índice de polaridade líquida: (positivos − negativos) / total",
    x = "Ano",
    y = "NSS (−1 a +1)"
  ) +
  
  scale_y_continuous(limits = c(-1, 1)) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )


############################################################
#Índice Ponderado por Intensidade (Weighted Sentiment Index)
#Dá mais peso a termos muito positivos ou muito negativos.

serie_temporal <- dados_sentimento_resumo %>%
  mutate(
    weighted_sent = ifelse(
      matched_tokens == 0, 
      NA, 
      sum_polarity / matched_tokens
    )
  )


ggplot(serie_temporal, aes(x = ano, y = weighted_sent, group = 1)) +
  geom_line(linewidth = 1.2, color = "#e66101") +
  geom_point(size = 2.5, color = "#e66101") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Weighted Sentiment por Ano",
    x = "Ano",
    y = "Índice ponderado"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#############################################################
#Índice de Polarização (Polarization Index)

#Mede o quão “extremos” são os sentimentos — mesmo com média neutra.

serie_temporal <- dados_sentimento%>%
  group_by(ano) %>%
  summarise(
    polarization = mean(abs(intensidade), na.rm = TRUE)
  )

ggplot(serie_temporal, aes(x = ano, y = polarization, group = 1)) +
  geom_line(linewidth = 1.2, color = "#e66101") +
  geom_point(size = 2.5, color = "#e66101") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Weighted Sentiment por Ano",
    x = "Ano",
    y = "Índice ponderado"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

###########################################################
#Índice de Entropia Semântica (Semantic Entropy Index)

# Mede quão diversa é a distribuição de sentimentos (confusão, variabilidade).

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    H = {
      probs <- prop.table(table(cut(intensidade, breaks = c(-1, -0.1, 0.1, 1))))
      entropy <- -sum(probs * log(probs), na.rm = TRUE)
      entropy
    }
  )

ggplot(serie_temporal, aes(x = ano, y = H, group = 1)) +
  geom_line(linewidth = 1.2, color = "#e41a1c") +
  geom_point(size = 2.5, color = "#e41a1c") +
  labs(
    title = "Índice de Entropia do Sentimento ao Longo do Tempo",
    subtitle = "Maior entropia indica distribuição mais dispersa de sentimentos",
    x = "Ano",
    y = "Entropia (H)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )


###########################################################
#Muito usado em monitoramento de redes sociais.

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    positivos = sum(intensidade > 0, na.rm = TRUE),
    negativos = sum(intensidade < 0, na.rm = TRUE),
    pri = positivos / (positivos + negativos)
  )

library(ggplot2)

ggplot(serie_temporal, aes(x = ano, y = pri, group = 1)) +
  geom_line(linewidth = 1.3, color = "#1f78b4") +
  geom_point(size = 3, color = "#1f78b4") +
  labs(
    title = "Proportion of Positive Relative to Polarized Sentiment (PRI)",
    subtitle = "PRI = positivos / (positivos + negativos)",
    x = "Ano",
    y = "PRI (0 = NEG, 1 = POS)"
  ) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

##############################################################
#Índice de Valência Média Ajustada (Adjusted Valence Index)

#Desconta a proporção de palavras sem sentimento, suavizando ruídos.

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    avi = sum(intensidade, na.rm = TRUE) /
      sum(!is.na(intensidade))
  )

ggplot(serie_temporal, aes(x = ano, y = avi, group = 1)) +
  geom_line(linewidth = 1.2, color = "#4daf4a") +
  geom_point(size = 2.8, color = "#4daf4a") +
  labs(
    title = "Average Valence Intensity (AVI) ao Longo do Tempo",
    subtitle = "Média da intensidade dos termos identificados no léxico",
    x = "Ano",
    y = "AVI (média da polaridade)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )
#############################################################
#Índice de Sentimento Diferenciado (Separated Positive/Negative Indexes)

#Útil para estudos políticos: positivo e negativo caminham diferente.

serie_temporal <- dados_sentimento %>%
  group_by(ano) %>%
  summarise(
    positive_intensity = mean(intensidade[intensidade > 0], na.rm = TRUE),
    negative_intensity = mean(intensidade[intensidade < 0], na.rm = TRUE)
  )

ggplot(serie_temporal, aes(x = ano)) +
  
  # Linha da intensidade positiva
  geom_line(aes(y = positive_intensity, color = "Positive Intensity"),
            linewidth = 1.2) +
  geom_point(aes(y = positive_intensity, color = "Positive Intensity"),
             size = 3) +
  
  # Linha da intensidade negativa
  geom_line(aes(y = negative_intensity, color = "Negative Intensity"),
            linewidth = 1.2) +
  geom_point(aes(y = negative_intensity, color = "Negative Intensity"),
             size = 3) +
  
  # Paleta de cores
  scale_color_manual(
    values = c(
      "Positive Intensity" = "#1b9e77",  # verde
      "Negative Intensity" = "#d95f02"   # laranja
    )
  ) +
  
  labs(
    title = "Positive vs Negative Intensity ao Longo do Tempo",
    subtitle = "Força média dos sentimentos positivos e negativos",
    x = "Ano",
    y = "Intensidade Média",
    color = "Série"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

