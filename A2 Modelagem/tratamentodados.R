# Carregar pacote dplyr para manipulação de dados
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)




# Carregar os dados
dados <- read.csv("caminho_para_seu_arquivo.csv")

# Visualizar as primeiras linhas para verificar os dados
head(dados)

# 1. Tratar valores faltantes
# Opção 1: Remover linhas com qualquer valor faltante
dados_limpos <- na.omit(dados)

# Opção 2: Substituir valores faltantes na coluna X e Y por médias
dados$X <- ifelse(is.na(dados$X), mean(dados$X, na.rm = TRUE), dados$X)
dados$Y <- ifelse(is.na(dados$Y), mean(dados$Y, na.rm = TRUE), dados$Y)

# 2. Filtrar dados
# Filtrar linhas onde X é maior que um valor específico, por exemplo, 0
dados_filtrados <- filter(dados, X > 0)

# 3. Criar uma nova coluna
# Por exemplo, uma coluna que é o dobro do valor de X
dados <- mutate(dados, X_dobro = 2 * X)

# 4. Agrupar dados e sumarizar
# Agrupar por uma nova variável categórica e calcular médias de X e Y
dados$Categoria <- ifelse(dados$X > median(dados$X), "Alto", "Baixo")
dados_agrupados <- dados %>%
  group_by(Categoria) %>%
  summarise(
    Media_X = mean(X, na.rm = TRUE),
    Media_Y = mean(Y, na.rm = TRUE)
  )

# Ver resumo dos dados tratados
summary(dados_agrupados)

# 5. Ordenar dados
# Ordenar dados por Y em ordem decrescente
dados_ordenados <- arrange(dados, desc(Y))
