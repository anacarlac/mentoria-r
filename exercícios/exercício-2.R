#Exercício 2: Funções essenciais do R base

#Carregue o banco de dados mtcars
mtcars <- mtcars

#1. Quantas variáveis tem no banco de dados? 
#Use o help para saber mais sobre o banco de dados e as variáveis incluídas nele
#RESPOSTA: 

#2. Quais são os nomes das variáveis do banco de dados?
#RESPOSTA: 

#3. As variáveis são binárias, ordinais ou numéricas?
#RESPOSTA: 

#4. Qual é a classe do objeto mtcars?
#RESPOSTA: 

#5. Você notou que a variável de nomes de carros não é uma variável? 
# Isso acontece porque essas informações foram imputadas como rownames e não como valores de uma coluna
# Como são valores referentes às observações, vamos seguir os princípios do tidy data e transformá-los em uma coluna?
# Semana que vem vamos falar mais sobre o universo tidyverse, mas por enquanto, basta instalá-lo e carregá-lo em sua biblioteca para usarmos a função rownames_to_column
# Nesta função, primeiro é indicado o banco de dados (mt cars) e então é indicado o nome da variável desejada de acordo com o argumento "var ="
library(tidyverse)
mtcars <- tibble::rownames_to_column(mtcars, var = "cars")

#6. Qual é o tipo da variável cars?
#RESPOSTA: 

#7. Usando a função grep ou grepl, quantos Toyotas existem no banco de dados?
#RESPOSTA: 

#8. Usando a função grep ou grepl, quantos Mercedes existem no banco de dados?
#Dica: os carros Mercedes estão com nome Merc no banco de dados
#RESPOSTA: 

#9. Usando a função View(), qual carro consome menos gasolina por galão? E qual carro consome mais gasolina?
#RESPOSTA: 

#10. Vamos visualizar os dados? Faça um histograma da variável correspondente a milhas por galão. Em média, os carros percorrem quantas milhas por galão? Pode indicar intervalo de números, se preferir.
#RESPOSTA: 

#11. Agora faça um boxplot com a variável de milhas por galão. Qual é a mediana de milhas por galão nesse banco de dados?
#RESPOSTA: 

#12. Por fim, se hoje você quisesse comprar o carro mais potente, em termos de potência (horsepower), qual carro você comparia de acordo com esse banco de dados?
#RESPOSTA: 

# Até a próxima!
print('Até mais!')

