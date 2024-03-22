# Carregando os pacotes
library(beepr)
library(tidyverse)
library(psych)
library(psychTools)
library(janitor)

# O que é R? 

## Cumprimentando o R
print('Olá!')

## Vamos fazer o R cantar?
library(beepr)
beep(sound=8)

# R express: Objetos e classes 

# Converta o vetor escolaridade para fator
escolaridade <- c("Médio", "Superior", "Fundamental", "Fundamental", "Médio")
fator <- as.factor(escolaridade)
class(fator)

# Converta a matriz de dados numero_frutas para data.frame 
numero_frutas <- matrix(1:10,nrow=5, ncol=5)
dataframe <- as.data.frame(numero_frutas)
str(dataframe)

# Pacotes e funções essenciais
# Checando a integridade dos dados
# Objetivo: Corrigir erros estruturais do banco de dados e verificar a integridade das respostas no objeto data_raw

## Abrir o banco de dados
bfi_fake <- read_excel("data_raw/bfi_data.xlsx")

## Visualizar o banco de dados
View(bfi_fake)

# Verificar premissas do tidy data
View(bfi_fake)
glimpse(bfi_fake)

# Identificar o nome das variáveis
names(bfi_fake)

# Número de itens do BFI
bfi_fake %>% 
  select(A1:O5)

# Ponto de atenção 1: nomes de variáveis com caixa alta ou espaço entre elas
bfi_fake <- bfi_fake %>% 
  clean_names() 

names(bfi_fake)

# Ponto de atenção 2: As características pessoais estão fora de ordem no banco de dados
bfi_fake <- relocate(bfi_fake, c("genero","education","idade", "duracao_minutos"), .before = "a1")

# Ponto de atenção 3: A variável ID está com o nome de identidade
bfi_fake <- bfi_fake %>% 
  rename("id" = "identidade")

names(bfi_fake)

# Checagem de dados duplicados sem a variável id 
get_dupes(bfi_fake, -id)

# Checagem de dados duplicados sem a variável id
get_dupes(bfi_fake, id_estudo, idade, education, genero)

# Removendo os participantes com linhas duplicadas
bfi_fake %>% 
  filter(id_estudo == "2801" | id_estudo == "2804") %>% 
  glimpse()

bfi_fake <- bfi_fake %>% 
  filter(!id %in% c("2805", "2806"))

# Checagem de respostas descuidadas

# Estatística descritiva
df_bfi <- psych::bfi

# Média

# Mediana

# Moda

# Desvio padrão

# Variância

# Quartis
quantile(df_bfi$C2, na.rm=TRUE)

# Percentis
percentile(df_bfi$C2, na.rm=TRUE)

# Amplitude interquartílica
IQR(df_bfi$C2, na.rm=TRUE)

# Intervalo de confiança
## Fonte: https://biostatistics-uem.github.io/Bio/IC.html
mean_bfi_C2 <- mean(df_bfi$C2, na.rm=TRUE)
sd_bfi_C2 <- sd(df_bfi$C2, na.rm=TRUE)

se_bfi_C2 <- sd_bfi_C2/sqrt(2800)
IC_bfi_C2 <-mean_bfi_C2 + qnorm(c(0.025, 0.975)) * se_bfi_C2
IC_bfi_C2

IC <- function(x, conf = 0.95) {
  n <- length(x)
  media <- mean(x, na.rm=TRUE)
  variancia <- var(x, na.rm=TRUE)
  se <-sqrt(variancia/n)
  quantis <- qt(c((1 - conf)/2, 1 - (1 - conf)/2), df = n - 1)
  ic <- media + quantis * se
  return(ic)
}

IC(df_bfi$C2)
