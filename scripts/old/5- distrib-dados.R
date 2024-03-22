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
.# Carregando pacotes
library(tidyverse)

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