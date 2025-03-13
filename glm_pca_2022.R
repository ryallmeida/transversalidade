# ===================================================================================
# SCRIPT::PRINCIPAL COMPONENTES ANALYSIS 
# ===================================================================================

Zscore_2022 <- read_excel("C://Users//Notebook//Desktop//R//TRABALHOFINAL//Zscore_PropriamenteDito//Zscore_2022.xlsx")

# ---------------------------------------------

model_2022 <- lm(IEGM_taxa_2021~., data=Zscore_2022)
summary(model_2022)



# GENERALIZED LINEAR MODEL
model_2022_gaussian <- glm(IEGM_taxa_2021~., data=Zscore_2022, family =  gaussian())
summary(model_2022_gaussian)

library(corrgram)
corrgram(Zscore_2022)


pca_2022 <- prcomp(Zscore_2022, center = TRUE, scale. = TRUE)
summary(pca_2022)

explained_variance <- summary(pca_2022)$importance[3, ]
num_components <- which(cumsum(explained_variance) >= 0.95)[1]

# ---------------------------------------------------


# Obter os componentes principais
X_pca <- pca_2022$x[, 1:num_components]

# Criar um data frame com os componentes principais para usar no modelo
X_pca_df <- as.data.frame(X_pca)

# Construir o modelo de regressão usando os componentes principais
model_pca <- lm(Zscore_2022$IEGM_taxa_2021 ~ -1 + PC1 + PC2 + PC3 + PC4, data = X_pca_df)
summary(model_pca)


pca_2022$rotation[1]
