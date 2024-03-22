# Encontro 2: Manipulação de dados: pacotes e funções essenciais

# Transformações de classes
# Transforme essa string em fator
escolaridade <- c("Médio", "Superior", "Fundamental", "Fundamental", "Médio")
fator <- as.factor(escolaridade)
fator

class(fator)

# Transforme a matriz em um data frame
numero_frutas <- matrix(1:10,nrow=5, ncol=5)
dataframe <- as.data.frame(numero_frutas)
dataframe

# Operadores relacionais
# Os operadores relacionais trabalham como comparações, igualdades e desigualdades. Eles verificam os valores dos operandos, que ficam cada um de um lado da operação, retornando VERDADEIRO ou FALSO

10 == 10 
11 == 10 
5 > 6
10 <= 12
10 != 10
8 > 10
10 != 20

# Operadores lógicos
# Os operadores lógicos são aqueles que analisam condições, são operadores que analisaram operandos com valores lógicos 1 ou 0, ou então, Verdadeiro ou Falso. Uma situação pode ou não ser verdadeira, ou falsa, dependendo da condição em que se encontra. 

y <- 2

y >= 3 | y <=7
y >= 3 | y == 0
y >= 1 & y <=7
y >= 3 & y <= 7
(!y < 4)

# R base

# Carregando os dados
letters <- letters
mtcars <- mtcars

# Transformar o objeto letters em dataframe
class(letters)
letters <- as.data.frame(letters)
str(letters)

# Usar a função head para ver o banco de dados 
head(letters)

# Escolha uma variável numérica e faça um histograma
hist(mtcars$mpg, col="red")

mtcars$mpg

help(hist)


