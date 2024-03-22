.# Carregando pacotes
library(tidyverse)

# Aquecimento
## Resposta Média: 217.5 pessoas
## Resposta DP: 99.1 pessoas

## Forma 1: Média
media_1 <- (150+150+210+360)/4

## Forma 1: SD
dados <- c(150,150,210,360)
n_media <- 4 
n_media <- length(dados)
sd_1 <- sqrt(sum((dados - mean(dados))^2) / (n_media - 1))

## Forma 2: Média
dados <- c(150,150,210,360)
media_2 <- mean(dados)
sd_2 <- sd(dados)

## Forma 3: Média e DP
dados <- c(150,150,210,360)
dados <- as.data.frame(dados)

dados %>% 
  dplyr::summarise(
    media_3 = mean(dados),
    sd_3 = sd(dados)
  )

## Extra - A relação entre variância e DP
var(dados$dados) #9825
sqrt(9825) #DP = 99.12 
99.12^2 #var = 9825

## Extra - Quantis
dados_ment <- rnorm(n=10, mean=5, sd=3)
quantile(dados_ment)

## Atalho <- : ALT + -
## Atalho pipe: ctrl+shift+M


# DPLYR
## Banco de dados sobre carros e suas características
dados_dplyr <- mtcars

## Nome das variáveis
names(dados_dplyr)
head(dados_dplyr)
glimpse(dados_dplyr)

## FILTER
## Filtrando dados
dados_dplyr %>% 
  filter(cyl >= 6)

## Crie um banco de dados com esse critério
carros_cyl_6oumais <- dados_dplyr %>% 
  filter(cyl >= 6)

## Exercício 1:
## Selecione carros com transmissão automática
dados_dplyr %>% 
  filter(am == 0)

## Novo banco de dados
carros_am_aut <- dados_dplyr %>% 
  filter(am == 0)

## SELECT
## Selecionando variáveis e atribuindo para um objeto
dados_dplyr_select <- dados_dplyr %>% 
  select(mpg, cyl, am)

## Exercício 2:
## Selecione milhagem, cilindros, potência (gross horsepower) e peso
dados_dplyr_select_plus<- dados_dplyr %>% 
  select(mpg, cyl, hp, wt)



## Filtrando dados
dados_dplyr %>% 
  filter(cyl >= 6)

2+2

## Crie um banco de dados com esse critério
carros_cyl_6oumais <- dados_dplyr %>% 
  filter(cyl >= 6)

x<-2+2
