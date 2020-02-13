data("anscombe")

#checando os dados
dim(anscombe) # dimensao dos dados, N de linhas e N de colunas
head(anscombe) # seis primeiras linhas dos dados
class(anscombe) # classe do objeto
str(anscombe) # estrutura do objeto

# media de cada coluna
mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

## media de todos os vetores x
apply(anscombe[,1:4], 2, mean) #aplica uma funcao a todas as linhas de um objeto
## media de todos os vetores y
apply(anscombe[,5:8], 2, mean)

apply(anscombe, 2, var) # aplica a funcao var a todas as linhas do objeto

#Ententendo a correlação e coeficiente de regressão dos conjuntos x e y.
#correlação

cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

# coeficiente de regressão
## primeiro criamos objetos com as regressoes dos quatro conjuntos

m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)

## vamos criar agora uma lista com todos os modelos para facilitar o trabalho

mlist <- list(m1, m2, m3, m4)

## agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao

lapply(mlist, coef)

# funcao par para definir as configuracoes da janela grafica entre em ?par

par(mfrow=c(2, 2), #abre uma janela gráfica com 2 linhas  e 2 colunas
    las=1, # deixa as legendas dos eixos na vertical
    bty="l") # tipo do box do grafico em L

plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

par(mfrow=c(1, 1))

head(iris)
summary(iris)

table(iris$Species)

#media do comprimento da petala por especie
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
# a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
# ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data = iris, mean)

#desvio padrao de cada variavel

tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol = 3, nrow = 4)
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]
for (i in 1:4){medias[i,] <- tapply(iris[,i], iris$Species, mean)}

medias

#Estatísticas descritivas
##media
vars <- iris[, -5]
apply(vars, 2, mean)

#mediana
apply(vars, 2, var)

#desvio padrao
sd01 <- apply(vars, 2, sd)

#Coeficiente de variação: medida relativa de desvio padrão
## criei uma função para isso
cv <- function(x){sd(x)/mean(x)*100}
apply(vars, 2, cv)

#quantis ou percentis
apply(vars, 2, quantile)
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))

#intervalo
apply(vars, 2, range)
my_range <- function(x){diff(range(x))}
apply(vars, 2, my_range)

#intervalo interquartil
apply(vars, 2, IQR)
#correlação
cor(vars)

#crindo gráficos de barras
barplot(table(iris$Species))

#criando o histograma
par(mfrow=c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
par(mfrow=c(1, 1))
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)
par(mfrow=c(1, 1))
plot(density(iris$Sepal.Width))
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue")
par(mfrow=c(1, 1))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)
boxplot(iris$Sepal.Width)

my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot
outliers <- my_boxplot$out
which(iris$Sepal.Width %in% outliers)

iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

boxplot(Sepal.Width ~ Species, data = iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
outliers2 <- my_boxplot2$out

iris[iris$Sepal.Width %in% outliers2 & iris$Species == "setosa", c("Sepal.Width", "Species")]

par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"],main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"], main ="versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"], main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])

par(mfrow=c(1,1))
pairs(vars)
