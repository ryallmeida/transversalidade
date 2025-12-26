
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