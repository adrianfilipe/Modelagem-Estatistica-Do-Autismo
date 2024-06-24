# Carregar pacotes necessários
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(ggplot2)
library(dplyr)
library(effects)
install.packages(c("readr","mfx","caret","pRoc","ResourceSelection","modEvA","foreign","stargazer"))
require(readr)
require(stargazer)
install.packages("readxl")
library(readxl)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
library(caret)
library(stats)


# Carregar dados do arquivo CSV
dados <- read.csv("C:/Users/Adrian/Desktop/A2 Modelagem/train.csv")

# Renomeando colunas diretamente
colnames(dados) <- c("ID","pontuacao1", "pontuacao2", 
                     "pontuacao3", "pontuacao4", 
                     "pontuacao5", "pontuacao6", 
                     "pontuacao7", "pontuacao8", 
                     "pontuacao9", "pontuacao10",
                     "idade", "genero",
                     "etnia", "icteria",
                     "familia", "país",
                     "triagem_antes", "pontuação_teste",
                     "idade_desc", "relação",
                     "autismo_resultado")

#Remover Colunas Desnecessarias (Pra mim né kakaka)
dados$ID <- NULL
dados$idade_desc <- NULL

# Se Male é 0 caso contrário é 1 
dados$genero <- ifelse(dados$genero == "m", 0, 1)
dados$icteria <- ifelse(dados$icteria == "no", 0, 1)
dados$familia <- ifelse(dados$familia == "no", 0, 1)
dados$triagem_antes <- ifelse(dados$triagem_antes == "no", 0, 1)

#Converte categoricas com mais de 2 opções em fatores
# dados$etnia<- as.factor(dados$etnia)
dados$país <- as.integer(factor(dados$país, levels = unique(dados$país)))
dados$relação <- as.integer(factor(dados$relação, levels = unique(dados$relação)))
dados$etnia <- as.integer(factor(dados$etnia, levels = unique(dados$etnia)))

# Dividindo os dados em conjuntos de treino e teste
set.seed(123) # Para reprodutibilidade do exemplo
particao <- createDataPartition(dados$autismo_resultado, p = 0.70, list = FALSE)
dados_treino <- dados[particao, ]
dados_teste <- dados[-particao, ]


# Modelo Regressão Logistica Multipla
modelo <- glm(autismo_resultado ~ pontuacao1 + pontuacao2 + pontuacao3 + pontuacao4 + 
                pontuacao5 + pontuacao6 + pontuacao7 + pontuacao8
              + pontuacao9 + pontuacao10 + idade + genero +etnia + icteria + 
                familia + triagem_antes +país + pontuação_teste + relação , data=dados, family = binomial(link="logit"))

#Resultados
# P-Valores está no summary
#AIC está no summary
summary(modelo)
#IC
confint(modelo)
stargazer(modelo, title="Resultados",type = "text")

# Fazer previsões no conjunto de teste
probabilidades <- predict(modelo,dados_teste, type = "response")
#predicoes <- factor(ifelse(probabilidades > 0.5, '1', '0'), levels = c('1', '0'))
predicoes <- as.factor(ifelse(probabilidades > 0.5, '1', '0'))

# Dados reais (ajuste conforme o nome da sua coluna de resposta no dados_teste)
valores_reais <-as.factor(dados_teste$autismo_resultado)

# Matriz de confusão e cálculo de métricas
confusao <- confusionMatrix(predicoes, valores_reais)
acuracia <- confusao$overall['Accuracy']
recall <- confusao$byClass['Sensitivity']
precisao <- confusao$byClass['Precision']
f1_score <- 2 * (precisao * recall) / (precisao + recall)

# Exibindo os resultados
print(paste("Acurácia: ", acuracia))
print(paste("Recall: ", recall))
print(paste("Precisão: ", precisao))
print(paste("F1-Score: ", f1_score))

#Plotar Matriz de Confusão:
d <- as.data.frame(confusao$table)
ggplot(data = d, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Matriz de Confusão", x = "Valor Real", y = "Predição") +
  theme_minimal()




#ODDS ratio Calcular 
require(mfx)
logitor(autismo_resultado ~ pontuacao1 + pontuacao2 + pontuacao3 + pontuacao4 + 
          pontuacao5 + pontuacao6 + pontuacao7 + pontuacao8
        + pontuacao9 + pontuacao10 + idade + genero +etnia + icteria + 
          familia + triagem_antes +país + pontuação_teste + relação, data=dados)


exp(coef(modelo))

# Intervalo de COnfiança para OR
exp(cbind(OR=coef(modelo), confint(modelo)))





#Metodo Stepwise (AIC) - não sei é necessario no nosso modelo
#step(modelo, direction = 'both')

#Calculo do VIF (Não deve ficar abaixo de 10, se não apresenta problema de multicolinearidade)
require(faraway)
vif(modelo)

#ROC-AUC
library(pROC)
# Calcular a curva ROC
roc_obj <- roc(valores_reais, probabilidades, levels = c("1", "0"))

# Calcular a AUC
auc_val <- auc(roc_obj)

# Plotar a curva ROC
plot(roc_obj, main = "Curva ROC", col = "#1c61b6")

# Mostrar o valor da AUC
print(paste("AUC: ", auc_val))

# Teste de DeLong para a AUC
roc_test <- roc.test(roc_obj, auc = 0.5)

# Imprimir o p-valor
print(paste("P-valor: ", roc_test$p.value))


# Gráfico de resíduos vs. valores ajustados
plot(modelo$fitted.values, residuals(modelo), main = "Gráfico de Resíduos",
     xlab = "Valores Ajustados", ylab = "Resíduos", pch = 20)
abline(h = 0, col = "red")  # Adiciona uma linha horizontal em y=0

#Gráficos De Diagnostico de Modelo
par(mfrow = c(2, 2))
plot(modelo)
