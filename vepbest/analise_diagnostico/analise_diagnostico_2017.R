# ===================================================================================
# SCRIPT::MODELO DE 2017
# ===================================================================================

# ENTRE INDICADORES E TRANSVERSALIDADE: UMA ANÁLISE EM PERSPECTIVA DA EFETIVIDADE DA GESTÃO MUNICIPAL COMO EXPRESSÃO DE INDICADORES SOCIOECONÔMICOS E ADMINISTRAAATIVOS EM PERNAMBUCO

### PESQUISA QUANTITATIVA REALIZADA NO DEPARTAMENTO DE CIÊNCIA POLÍTICA (CPRI) DA UNIVERSIDADE FEDERAL DE PERNAMBUCO A PARTIR DE UMA ANÁLISE DE REGRESSÃO LINEAR MÚLTIPLA 

### DOCENTE DRª. Mª DO CARMO (MQ1)
### CODADO ORIGINALMENTE EM R VERSÃO 4.4.1
### INÍCIO 06-01-25

set.seed(123)

# ===================================================================================
# DISCENTES 
# ===================================================================================

# JEFFERSON GABRIEL
# JULIA EVELYN
# LARISSA KARLA
# RYAN ALMEIDA
# MIGUEL

# CARREGAR TODAS AS BIBLIOTECAS DE UMA VEZ
if(!require(pacman))
  install.packages("pacman")  
library(pacman)

pacman::p_load(corrplot, dplyr, readxl, tidyverse, corrgram, GGally, openxlsx, ggrepel, tidyr, ggplot2)

# CARREGANDO... 
Dados_Zscore_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//DadosZscore//Dados_Zscore_2017.xlsx")

glimpse(Dados_Zscore_2017)
view(Dados_Zscore_2017)
# DEU ERRO DE LEITURA RESULTANDO EM CHR APENAS EM NUMEROS DE OBITOS INFANTIS, ASSIM COMO EM TAXA DE MORTALIDADE INFANTIL

# ===================================================================================
# TRATAMENTO INICIAL 
# ===================================================================================

Dados_Zscore_2017$Obitos_Infantis_2010 <- as.integer(Dados_Zscore_2017$Obitos_Infantis_2010)

Dados_Zscore_2017$Taxa_Mortalidade_Infantil_2010 <- as.double(Dados_Zscore_2017$Taxa_Mortalidade_Infantil_2010)

Dados_Zscore_2017$GASTO_PUBLICO_2017 <- as.double(Dados_Zscore_2017$GASTO_PUBLICO_2017)

dplyr::glimpse(Dados_Zscore_2017)
summary(Dados_Zscore_2017)
#OK...

# REMOVENDO INUTILIDADES (COLUNAS) PARA A PADRONIZAÇÃO  DO ZSCORE
colnames(Dados_Zscore_2017)

Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Município "]

Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Tribuntal "]

Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Exercício "]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Numero_CVLI_2017"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Nascidos_Vivos_2010"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Obitos_Infantis_2010"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "GASTO_PUBLICO_2017"]
Dados_Zscore_2017 <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "Densidade_Demografica_2010"]

# REMOVENDO INUTILIDADES (COLUNAS) PARA A PADRONIZAÇÃO  DO ZSCORE
Dados_Zscore_2017 <- Dados_Zscore_2017[,-3]
Dados_Zscore_2017 <- Dados_Zscore_2017[,-2]
Dados_Zscore_2017 <- Dados_Zscore_2017[,-1]

# TRATANDO NAs POR IMPUTAÇÃO DE MEDIANA 
Dados_Zscore_2017 <- Dados_Zscore_2017 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

print(Dados_Zscore_2017)


# ===================================================================================
# PADRONIZANDO EM ZSCORE
# ===================================================================================

Dados_Zscore_2017 <- Dados_Zscore_2017 %>%
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.)))

# ===================================================================================
# ANALISE DE CORRELAÇÃO ENTRE VAR. INDEP.
# ===================================================================================

SEM_VD <- Dados_Zscore_2017[, !colnames(Dados_Zscore_2017) %in% "IEGM_taxa"]
corrgram::corrgram(SEM_VD)
# DIAGNOSTICADO A CORRELAÇÃO MODERADA ENTRE PELO MENOS 3 VARIAVEIS E 1 FORTE ENTRE DUAS 
# ===================================================================================
# TRATAMENTO DA CORRELAÇÃO POR PCA
# ===================================================================================

pca_2017 <- prcomp(SEM_VD, center = TRUE, scale. = TRUE)
summary(pca_2017)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2017)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

#  --------------------------------------------------
#  --------------------------------------------------
# ISOLANDO VARIAVEL DEPENDENTE
y <- Dados_Zscore_2017$IEGM_taxa

# ISOLANDOS VARIAVEIS INDEPENDENTES (X) COM PCA
X_pca_2017 <- pca_2017$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
modelo_data2017 <- as.data.frame(cbind(y, X_pca_2017))

# CONSTRUIR O MODELO DE REGRESSÃO USANDO OS COMPENTENTES PRINCIPAIS

model1_pca_2017 <- lm(modelo_data2017$y ~ 1 + PC1 + PC2 + PC3, data = modelo_data2017)

summary(model1_pca_2017)

#  --------------------------------------------------
#  --------------------------------------------------
# Call:
# lm(formula = modelo_data2017$y ~ 1 + PC1 + PC2 + PC3, data = modelo_data2017)

# Residuals:
#       Min        1Q    Median        3Q       Max 
# -0.099315 -0.021331  0.000284  0.019862  0.139402 

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.526070   0.002364 222.548  < 2e-16 ***
# PC1         -0.012957   0.001528  -8.478 7.86e-15 ***
# PC2          0.033539   0.001838  18.247  < 2e-16 ***
# PC3          0.004017   0.001943   2.067   0.0401 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.03215 on 181 degrees of freedom
# Multiple R-squared:  0.6933,	Adjusted R-squared:  0.6882 
# F-statistic: 136.4 on 3 and 181 DF,  p-value: < 2.2e-16
#  --------------------------------------------------
#  --------------------------------------------------


# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE TESTES ESTATISTICOS
# ===================================================================================

# ANALISE DOS RESIDUOS 
# ---------------------------------------------

residuos_padronizados <- residuals(model1_pca_2017, type = "pearson")
print(residuos_padronizados)

# ===================================================================================
# ANALISE DE DIAGNOSTICOS, OS RESIDUOS A PARTIR DE GRÁFICOS
# ===================================================================================

modelo <- model1_pca_2017
library(ggrepel)

#OBTER RESIUDOS E VALORES AJUSTADOS
residuos <- residuals(modelo)
fitted_values <- fitted(modelo)
std_residuos <- residuos_padronizados
cooks_dist <- cooks.distance(modelo)

# CRIA UMA BASE DE DADOS PARA OS GRÁFICOS, COM AS INFORMAÇÕES OBTIDAS A PARTIR DO MODELO ESTIMADO

dados_grafico <- data.frame(
  Fitted = fitted_values,
  Residuos = residuos,
  StdResiduos = std_residuos,
  CooksDist = cooks_dist,
  Index = 1:length(cooks_dist)
)

# TÁ DANDO ERRO O CODIGO ACIMA

modelo <- model2_pca_2017

# Obter resíduos, valores ajustados e resíduos padronizados
residuos <- residuals(modelo)
fitted_values <- fitted(modelo)
std_residuos <- rstandard(modelo)
cooks_dist <- cooks.distance(modelo)

# Criar base de dados para os gráficos
dados_grafico <- data.frame(
  Fitted = fitted_values,
  Residuos = residuos,
  StdResiduos = std_residuos,
  CooksDist = cooks_dist,
  Index = 1:length(cooks_dist)
)

# AGORA DEU CERTO
# ===================================================================================
# GRÁFICO 1: Resíduos vs. Valores Ajustados
# ===================================================================================
library(ggplot2)
p1 <- ggplot(dados_grafico, aes(x = Fitted, y = Residuos)) +
  geom_point(alpha = 0.7, color = "blue", shape = 1,
             size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Resíduos vs. Valores Ajustados", x = "Valores Ajustados", y = "Resíduos") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ===================================================================================
# GRÁFICO 2: QQ-Plot dos resíduos
# ===================================================================================
p2 <- ggplot(dados_grafico, aes(sample = Residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Gráfico QQ dos Resíduos", x = "Quantis Teóricos", y = "Quantis Amostrais") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10)) 

# ===================================================================================
# GRÁFICO 3: Histograma dos resíduos padronizados
# ===================================================================================
p3 <- ggplot(dados_grafico, aes(x = StdResiduos)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Hist. Resíduos Padronizados", x = "Resíduos Padronizados", y = "Frequência") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# ===================================================================================
# GRÁFICO 4: Distância de Cook
# ===================================================================================
limite_influencia <- 4 / length(fitted(modelo))
pontos_influentes <- which(cooks_dist > limite_influencia)

dados_diag <- data.frame(
  Index = 1:length(cooks_dist),
  CooksDist = cooks_dist
)
dados_acima <- dados_diag[pontos_influentes, ]

p4 <- ggplot(dados_diag, aes(x = Index, y = CooksDist)) +
  geom_point(alpha = 0.7, color = "blue", shape = 1,
             size = 2) +
  geom_point(data = dados_acima, aes(x = Index, y = CooksDist), color = "red", size = 2, shape = 1, size = 2) +
  geom_text_repel(data = dados_acima, aes(label = Index), 
                  size = 3, box.padding = 0.5, point.padding = 0.5, segment.color = 'grey50') +
  labs(title = "Distância de Cook", x = "Observações", y = "Distância de Cook") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))



# ===================================================================================

# Plotar todos os gráficos em uma única página
# Definindo a disposição dos gráficos em uma matriz 2x2
# Combinar os gráficos em uma única figura

library(patchwork)
combined_plot <- p1 + p2 + p3 + p4 + plot_layout(ncol = 2)
print(combined_plot)

par(mfrow=c(2,2))
plot(model1_pca_2017)
par(mfrow=c(1,1))

plot(model1_pca_2017)


# ===================================================================================
# ANALISE DE DIAGNOSTICOS, PONTOS DE ALAVANCA A PARTIR DE TESTES ESTATISTICOS
# ===================================================================================

# ANALISE DOS PONTOS DE ALAVANCAGEM
# ---------------------------------------------
# FUNÇÃO PARA CALCULAR A ALAVANCA 

calcular_alavanca <- function(modelo) {
  X <- model.matrix(modelo)
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  alavanca <- diag(H)
  return(alavanca)
}

alavanca <- calcular_alavanca(modelo)

# IDENTIFICANDO OS PONTOS DE ALAVANCA, USANDO O CRÍTÉRIO DE 2 VEZES A MÉDIA DA ALAVANCA
pontos_alavanca <- which(alavanca > 2 * mean(alavanca))  
# EXIBINDO...
print("Índices dos Pontos de Alavanca:")
print(pontos_alavanca)
# BUSCANDO QUEM SÃO...
observacao <- c(1,  31,  37,  45,  73, 105, 115, 119, 140 )
observacoes_selecionadas <- Dados_Zscore_2017[observacao, ]
print(observacoes_selecionadas)
# CONSTRUINOD MODELO SEM OS PONTOS DE ALAVANCA 

# ===================================================================================
# TRATANDO A INFLUÊNCIA DE PONTOS INFLUENTES (DISTÂNCIA DE COOK)
# ===================================================================================
# AJUSTAR O MODELO SEM PONTOS INFLUENTES 

dados_sem_influentes <- Zscore_2017[-pontos_influentes, ]

#DESCOBRINDO QUEM SÃO OS PONTOS INFLUENTES...
observacao <- c(1, 8, 13, 31, 45, 74, 88, 89, 105, 137, 140)
observacoes_selecionadas <- Dados_Zscore_2017[observacao, ]
print(observacoes_selecionadas)

# ===================================================================================
# DESCOBRINDO INTERSECÇÃO ENTRE PONTOS INFLUENTES E DE ALAVANCA
# ===================================================================================

observação <- c(1, 31, 45, 105, 140) 
observacoes_selecionadas <- Dados_Zscore_2017[observacao, ]
print(observacoes_selecionadas)

#PONTOS DE ALAVACAGEM E INFLUENCIA SÃO AS CIDADES: ABREU E LIMA (RMR), CALUMBI, CARNAIBA, CUPIRA, ITAQUITINGA, PAULISTA (RMR), RIACHO DAS ALMAS, SALGUEIRO E TABIRA

# ===================================================================================
# CONSTRUINDO MODELO SEM ALAVANCA E PONTOS DE INFLUENCIA
# ===================================================================================

# UPLOAD DE DADOS PADRONIZADO
Zscore_2017 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito/Zscore_2017.xlsx")
glimpse(Zscore_2017)

Zscore_2017 <- Zscore_2017[, !colnames(Zscore_2017) %in% "Numero_CVLI_2017"]
Zscore_2017 <- Zscore_2017[, !colnames(Zscore_2017) %in% "Nascidos_Vivos_2010"]
Zscore_2017 <- Zscore_2017[, !colnames(Zscore_2017) %in% "Obitos_Infantis_2010"]
Zscore_2017 <- Zscore_2017[, !colnames(Zscore_2017) %in% "GASTO_PUBLICO_2017"]
Zscore_2017 <- Zscore_2017[, !colnames(Zscore_2017) %in% "Densidade_Demografica_2010"]

dados <- Zscore_2017[-observação, ]

# ===================================================================================
# CONSTRUINDO O PCA
# ===================================================================================

SEM_VD <- dados[, !colnames(dados) %in% "IEGM_taxa"]

pca_2017 <- prcomp(SEM_VD, center = TRUE, scale. = TRUE)
summary(pca_2017)

# EXTRAI APENAS A VARIÂNCIA ACUMULADA
explained_variance <- summary(pca_2017)$importance[3, ] 
print(explained_variance)

# NUMERO MINIMO DE COMPONENTES PRINCIPAIS NECESSÁRIOS PARA EXPLICAR 95% DA VARIÂNCIA DOS DADOS 
num_components <- which(cumsum(explained_variance) >= 0.95)[1]
print(num_components)

#  --------------------------------------------------
#  --------------------------------------------------

# ISOLANDO VARIAVEL DEPENDENTE
y <- dados$IEGM_taxa

# ISOLANDOS VARIAVEIS INDEPENDENTES (X) COM PCA
X_pca_2017 <- pca_2017$x[, 1:num_components]

# CRIAR UM DATAFRAME COM OS COMPOMPONENTES PRINCIPAIS PARA USAR NO MODELO 
# OU SEJA, DEVE-SE ISOLÁ-LOS
modelo_data2017 <- as.data.frame(cbind(y, X_pca_2017))

# CONSTRUIR O MODELO DE REGRESSÃO USANDO OS COMPENTENTES PRINCIPAIS

model1_pca_2017 <- lm(modelo_data2017$y ~ -1 + PC1 + PC2 + PC3, data = modelo_data2017)
summary(model1_pca_2017)

# ===================================================================================
# CONSTRUIR O MODELO DE REGRESSÃO USANDO OS COMPENTENTES PRINCIPAIS
# ===================================================================================
model1_pca_2017 <- lm(Zscore_2017$IEGM_taxa ~ ., data = X_pca_df_2017)
summary(model1_pca_2017)

par(mfrow=c(2,2))
plot(model1_pca_2017)
par(mfrow=c(1,1))

