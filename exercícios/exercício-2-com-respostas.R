#Exercício 2: Funções essenciais do R base

#Carregue o banco de dados mtcars
mtcars <- mtcars

#1. Quantas variáveis tem no banco de dados? 
#Use o help para saber mais sobre o banco de dados e as variáveis incluídas nele
#RESPOSTA: Existem 11 variáveis no banco de dados e 32 carros (observações).
help(mtcars)

#2. Quais são os nomes das variáveis do banco de dados?
#RESPOSTA: As variáveis se chamam "mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear" e "carb"
names(mtcars)

#3. As variáveis são binárias, ordinais ou numéricas?
#RESPOSTA: Todas as variáveis são numéricas. 
str(mtcars)

#4. Qual é a classe do objeto mtcars?
#RESPOSTA: É um objeto dataframe.
class(mtcars)

#5. Você notou que a variável de nomes de carros não é uma variável? 
# Isso acontece porque essas informações foram imputadas como rownames e não como valores de uma coluna
# Como são valores referentes às observações, vamos seguir os princípios do tidy data e transformá-los em uma coluna?
# Semana que vem vamos falar mais sobre o universo tidyverse, mas por enquanto, basta instalá-lo e carregá-lo em sua biblioteca para usarmos a função rownames_to_column
# Nesta função, primeiro é indicado o banco de dados (mt cars) e então é indicado o nome da variável desejada de acordo com o argumento "var ="
library(tidyverse)
mtcars <- tibble::rownames_to_column(mtcars, var = "cars")

#6. Qual é o tipo da variável cars?
#RESPOSTA: É uma variável do tipo character.
str(mtcars$cars)

#7. Usando a função grep ou grepl, quantos Toyotas existem no banco de dados?
#RESPOSTA: Existem dois carros da Toyota
grep("Toyota", mtcars$cars) # 20 e 21
grepl("Toyota", mtcars$cars) # TRUE nas linhas 20 e 21, FALSE nas linhas restantes

#8. Usando a função grep ou grepl, quantos Mercedes existem no banco de dados?
#Dica: os carros Mercedes estão com nome Merc no banco de dados
#RESPOSTA: Existem dois carros da Toyota
grep("Merc", mtcars$cars) # 8, 9, 10, 11, 12, 13 e 14
grepl("Merc", mtcars$cars) # TRUE nas linhas 8, 9, 10, 11, 12, 13 e 14, FALSE nas linhas restantes

#9. Usando a função View(), qual carro consome menos gasolina por galão? E qual carro consome mais gasolina?
#RESPOSTA: Os carros que consomem mais são o  Cadillac Fleetwood e o Lincoln Continental que percorrem 10.4milhas por galão. O carro que consome menos gasolina é o Toyota Corolla que faz 33.9 milhas por galão.
View(mtcars)

#10. Vamos visualizar os dados? Faça um histograma da variável correspondente a milhas por galão. Em média, os carros percorrem quantas milhas por galão? Pode indicar intervalo de números, se preferir.
#RESPOSTA: Em média, os carros desse banco de dados parecem percorrer entre 15 e 25 milhas por galão.
hist(mtcars$mpg)

#11. Agora faça um boxplot com a variável de milhas por galão. Qual é a mediana de milhas por galão nesse banco de dados?
#RESPOSTA: A mediana (ponto central da distribuição) é 19,2 milhas por galão.
boxplot(mtcars$mpg)
median(mtcars$mpg)

#12. Por fim, se hoje você quisesse comprar o carro mais potente, em termos de potência (horsepower), qual carro você comparia de acordo com esse banco de dados?
#RESPOSTA: O carro mais potente é o Honda Civic com uma potência de 52 cavalos.
View(mtcars)

# Até a próxima!
print('Até mais!')

