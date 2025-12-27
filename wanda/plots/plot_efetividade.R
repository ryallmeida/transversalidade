# ==============================================================================

# #GRAFICO: MÉDIA DA EFETIVIDADE DA GESTÃO MUNICIPAL EM PERNAMBUCO (2017-2023)
# DADOS A PARTIR DA GEOLOCALIZAÇÃO
# FONTE INSTITUTO RUY BARBOSA/TCE-PE
# ----------------------------------------

# TITULO DO TRABALHO "TRANSVERSALIDADE EM PERPECTIVA: ALGORITMOS DE 
# MACHINE LEARNING APLICADOS À CIÊNCIA POLÍTICA NA INVESTIGAÇÃO DE DOCUMENTOS 
# OFICAIS"
# ----------------------------------------

# AUTORIA DE RYAN ALMEIDA, MARIANA BATISTA, JULIA EVELYN, LARISSA SILVA, 
# ALEXANDRE MENDES, ANA CASTELO BRANCO, CICERA VITÓRIA E PROFA. DRA. MARIA DO CARMO SOARES DE LIMA
# ----------------------------------------

# UNIVERSIDADE FEDERAL DE PERNAMBUCO
# DEPARTAMENTO DE CIÊNCIA POLÍTICA 
# CODADO ORIGINALMENTE EM R 4.4.3

# ==============================================================================

if(!require(pacman)) {
  install.packages("pacman")  
}

pacman::p_load(openxlsx,
               stringr,
               tidyverse,
               geobr,
               sf)

# ==============================================================================
# CRIANDO OBJETO COM TODOS OS MUNICIPIOS DE PERNAMBUCO PARA DEPOIS INCORPORAR OS DADOS DO TCE-PE
# ==============================================================================

# FONTE: https://www.ibge.gov.br/explica/codigos-dos-municipios.php
# MUNICIPIOS_IBGE <- readxl::read_excel("C:/Users/Admin/Downloads/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls")

# FILTRANDO APENAS AS OBS. DE PERNAMBUCO

MUNICIPIOS_PE <- MUNICIPIOS_IBGE %>% filter(...2 == "Pernambuco")

MUNICIPIOS_PE <- MUNICIPIOS_PE[, !colnames(MUNICIPIOS_PE) %in% c("TÍTULO : BET - BANCO DE ESTRUTURAS TERRITORIAIS", "...3", "...4", "...5", "...6", "...7", "...10")]

MUNICIPIOS_PE <- MUNICIPIOS_PE %>% 
  rename(UF = ...2,
         COD_IBGE = ...8,
         Municipios = ...9)

# write.csv(MUNICIPIOS_PE, "C:/Users/Admin/Documents/RYAN/transversalidade/wanda/mapa/municipios_ibge_pe.csv")

# ==============================================================================
# CARREGANDO BASES DE DADOS DO TCE-PE
# ==============================================================================


# IEGM_PE_2017 <- read.xlsx("https://github.com/ryallmeida/transversalidade/raw/refs/heads/main/wanda/dataset/IEGM_PE_2017.xlsx")
glimpse(IEGM_PE_2017)

# IEGM_PE_2019 <- read.xlsx("https://github.com/ryallmeida/transversalidade/raw/refs/heads/main/wanda/dataset/IEGM_PE_2019.xlsx")
glimpse(IEGM_PE_2019)

# IEGM_PE_2021 <- read.xlsx("https://github.com/ryallmeida/transversalidade/raw/refs/heads/main/wanda/dataset/IEGM_PE_2021.xlsx")
glimpse(IEGM_PE_2021)

# IEGM_PE_2023 <- read.xlsx("https://github.com/ryallmeida/transversalidade/raw/refs/heads/main/wanda/dataset/IEGM_PE_2023.xlsx")
glimpse(IEGM_PE_2023)

# ==============================================================================
# UNININDO A BASE DE DADOS E FAZENDO A VARIAVEL MÉDIA
# ==============================================================================

# Padronizar os nomes de município pra evitar erro por maiúscula/minúscula ou acento
padronizar_nome <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[ç]", "c") %>%
    str_replace_all("[áàãâä]", "a") %>%
    str_replace_all("[éèêë]", "e") %>%
    str_replace_all("[íìîï]", "i") %>%
    str_replace_all("[óòõôö]", "o") %>%
    str_replace_all("[úùûü]", "u")
}

MUNICIPIOS_PE <- MUNICIPIOS_PE %>%
  mutate(nome_municipio = padronizar_nome(Municipios))

IEGM_PE_2017 <- IEGM_PE_2017 %>%
  mutate(nome_municipio = padronizar_nome(Município))
colnames(IEGM_PE_2017)[which(colnames(IEGM_PE_2017) == "IEGM_taxa")] <- "IEGM"

IEGM_PE_2019 <- IEGM_PE_2019 %>%
  mutate(nome_municipio = padronizar_nome(Município))
colnames(IEGM_PE_2019)[which(colnames(IEGM_PE_2019) == "IEGM_taxa")] <- "IEGM"

IEGM_PE_2021 <- IEGM_PE_2021 %>%
  mutate(nome_municipio = padronizar_nome(Município))
colnames(IEGM_PE_2021)[which(colnames(IEGM_PE_2021) == "IEGM_taxa")] <- "IEGM"

IEGM_PE_2023 <- IEGM_PE_2023 %>%
  mutate(nome_municipio = padronizar_nome(Município))
colnames(IEGM_PE_2023)[which(colnames(IEGM_PE_2023) == "IEGM_taxa")] <- "IEGM"

# Renomeando as colunas IEGM em cada base auxiliar
IEGM_PE_2017 <- IEGM_PE_2017 |> rename(IEGM_2017 = IEGM)
IEGM_PE_2019 <- IEGM_PE_2019 |> rename(IEGM_2019 = IEGM)
IEGM_PE_2021 <- IEGM_PE_2021 |> rename(IEGM_2021 = IEGM)
IEGM_PE_2023 <- IEGM_PE_2023 |> rename(IEGM_2023 = IEGM)

# Realizando os joins no banco principal
MUNICIPIOS_PE <- MUNICIPIOS_PE |>
  left_join(IEGM_PE_2017[, 
                         c("nome_municipio", "IEGM_2017")], 
            by = "nome_municipio") |>
  left_join(IEGM_PE_2019[, 
                         c("nome_municipio", 
                           "IEGM_2019")], 
            by = "nome_municipio") |>
  left_join(IEGM_PE_2021[, 
                         c("nome_municipio", 
                           "IEGM_2021")], 
            by = "nome_municipio") |>
  left_join(IEGM_PE_2023[, 
                         c("nome_municipio", 
                           "IEGM_2023")], 
            by = "nome_municipio")

MUNICIPIOS_PE$IEGM_2017 = as.double(MUNICIPIOS_PE$IEGM_2017)
MUNICIPIOS_PE$IEGM_2019 = as.double(MUNICIPIOS_PE$IEGM_2019)
MUNICIPIOS_PE$IEGM_2021 = as.double(MUNICIPIOS_PE$IEGM_2021)
MUNICIPIOS_PE$IEGM_2023 = as.double(MUNICIPIOS_PE$IEGM_2023)

MUNICIPIOS_PE <- MUNICIPIOS_PE |> 
  rowwise() |> 
  mutate(IEGM_media = mean(c(IEGM_2017, 
                             IEGM_2019, 
                             IEGM_2021, 
                             IEGM_2023), 
                           na.rm = TRUE)) |> 
  ungroup()

# summary(MUNICIPIOS_PE)

# -----------------------------------------
# EXPORTANDO DADOS PARA UMA TABELA EM EXCEL A FIM DE COLOCAR MAIS VARIAVEIS

#write.csv(MUNICIPIOS_PE, "C:/Users/Admin/Documents/RYAN/transversalidade/wanda/mapa/iegm_mean_final.csv")
# -----------------------------------------

MUNICIPIOS_PE <- read.csv("https://github.com/ryallmeida/transversalidade/raw/refs/heads/main/wanda/mapa/iegm_mean_final.csv")

# ==============================================================================
# ORGAZANIZANDO O DATAFAME PROPRIAMENTE DITO
# ==============================================================================

mapa_pernambuco = geobr::read_municipality(
  code_muni = "all",
  year = 2010,
  simplified = TRUE,
  showProgress = TRUE,
  cache = TRUE,
  keep_areas_operacionais = FALSE
)

mapa_pernambuco = mapa_pernambuco |>
  filter(abbrev_state == "PE")

colnames(MUNICIPIOS_PE)[which(colnames(MUNICIPIOS_PE) == "COD_IBGE")] <- "code_muni"

dados_IEGM <- MUNICIPIOS_PE |>
  select(code_muni, IEGM_2017, IEGM_2019, IEGM_2021, IEGM_2023, IEGM_media)

dados_IEGM$code_muni <- as.double(dados_IEGM$code_muni)
mapa_pernambuco <- mapa_pernambuco |>
  left_join(dados_IEGM, by = "code_muni")

# -----------------------------------------
# EXPORTANDO DADOS PARA UMA TABELA XLSX PARA SALVAR O DAATFRAME
# openxlsx::write.xlsx(mapa_pernambuco, "C:/Users/cicer/OneDrive/Documentos/METODOS/GEOLOCALIZADOS/MEAN_IEGM_PE.xlsx")
# -----------------------------------------

# IMPORTANDO BASES DE DADOS DO REPOSITÓRIO GIT


# ==============================================================================
# PLOTANDO O GRAFICO PROPRIAMENTE DITO
# ==============================================================================

if(!require(viridis)) {
  install.packages("viridis")  
  }

# VERSÃO CORRETA: EM ESCALA ABSOLUTA NO IEGM 
MEAN_IEGM_PE3 <- ggplot() +
  geom_sf(data = mapa_pernambuco, aes(fill = IEGM_media), color = "white", size = 0.2) +
  viridis::scale_fill_viridis(
    option = "B",
    limits = c(0, 1),
    name = "Média (IEGM)",
    breaks = seq(0, 1, by = 0.2),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  coord_sf(
    xlim = c(-41.36, -32.39),
    ylim = c(-9.5, -3.83),
    expand = FALSE
  ) +
  ggtitle("") +
  theme_void(base_size = 16) +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",  
    legend.key.width = unit(2, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )
print(MEAN_IEGM_PE3)

# Exportar com espaço equilibrado
# ggsave("C:/Users/Admin/Documents/RYAN/transversalidade/wanda/mapa/escala_absoluta.png", plot = MEAN_IEGM_PE3, width = 10, height = 6, dpi = 300)

# VERSÃO CORRETA: EM ESCALA COMPARADA NOS MUNICIPIOS

MEAN_IEGM_PE4 <- ggplot() +
  geom_sf(data = mapa_pernambuco,
          color = "white",
          aes(fill = mapa_pernambuco$IEGM_media)) +
  viridis::scale_fill_viridis(option = "B",
                              name = "Média (IEGM)")  +
  coord_sf(
    xlim = c(-41.36, -32.39),
    ylim = c(-9.5, -3.83),
    expand = FALSE
  ) +
  ggtitle("") +
  theme_void(base_size = 16) +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",  
    legend.key.width = unit(2, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )
print(MEAN_IEGM_PE4)

# ggsave("C:/Users/Admin/Documents/RYAN/transversalidade/wanda/mapa/escala_comparada1.png", plot = MEAN_IEGM_PE4, width = 10, height = 6, dpi = 300)

# =============================================================================================
# SCRIPTS DE RASCUNHO PARA CODIGO ACIMA
# =============================================================================================

MEAN_IEGM_PE1 <- ggplot() +
  geom_sf(data = mapa_pernambuco,
          color = "white",
          aes(fill = mapa_pernambuco$IEGM_media)) +
  viridis::scale_fill_viridis(option = "B",
                              name = "Média (IEGM)") +
  coord_sf(
    xlim = c(-41.36, -32.39),
    ylim = c(-9.5, -3.83),
    expand = FALSE
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.margin = margin(50, 30, 30, 30)
  )
print(MEAN_IEGM_PE1)

MEAN_IEGM_PE2 <- ggplot() +
  geom_sf(data = mapa_pernambuco, 
          aes(fill = IEGM_media), 
          color = "white") +
  viridis::scale_fill_viridis(
    option = "B",
    limits = c(0, 1),                  
    name = "Média (IEGM)",            
    breaks = seq(0, 1, by = 0.2),      
    labels = scales::number_format(accuracy = 0.1)
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",         
    plot.title = element_text(hjust = 0.5, size = 16),  
    plot.margin = margin(20, 20, 20, 20) 
  ) +
  coord_sf(
    xlim = c(-41.5, -33.5),   
    ylim = c(-10.0, -3.5),    
  ) +
  ggtitle("")

print(MEAN_IEGM_PE2)

# =============================================================================================
# PARA O WANDA
# =============================================================================================

MEAN_IEGM_PE3 <- ggplot() +
  geom_sf(
    data = mapa_pernambuco,
    aes(fill = IEGM_media),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option = "cividis",   
    limits = c(0, 1),
    name = "Média (IEGM)",
    breaks = seq(0, 1, by = 0.2),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  coord_sf(
    xlim = c(-41.36, -32.39),
    ylim = c(-9.5, -3.83),
    expand = FALSE
  ) +
  theme_void(base_size = 16) +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )

print(MEAN_IEGM_PE3)


MEAN_IEGM_PE4 <- ggplot() +
  geom_sf(data = mapa_pernambuco,
          color = "white",
          aes(fill = mapa_pernambuco$IEGM_media)) +
  viridis::scale_fill_viridis(option = "cividis",
                              name = "Média (IEGM)")  +
  coord_sf(
    xlim = c(-41.36, -32.39),
    ylim = c(-9.5, -3.83),
    expand = FALSE
  ) +
  ggtitle("") +
  theme_void(base_size = 16) +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",  
    legend.key.width = unit(2, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )
print(MEAN_IEGM_PE4)
