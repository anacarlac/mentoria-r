# Carregando pacotes
library(tidyverse)
library(psychTools)
library(sjmisc)

# Aquecimento
# Big Five
data(bfi)
data(bfi.dictionary)

# Identifique quais variáveis tem NAs no banco de dados
frq(bfi)

# E se quiséssemos filtrar os dados para ter um banco de dados com informações de pessoas que tem respostas nas variáveis SES?
bfi_na_edu <- bfi %>% 
  filter(!is.na(education))

# Vamos criar um banco de dados sem missing nas variáveis?
bfi_na_clean <- bfi %>% 
  filter(if_all(everything(), ~ !is.na(.x)))

# MUTATE
## Alterando a métrica de uma variável
## A1
bfi_na_clean <- bfi_na_clean %>% 
  mutate(A1_z = (A1 - mean(A1))/sd(A1))

psych::describe(bfi_na_clean$A1)
psych::describe(bfi_na_clean$A1_z)

## A2
bfi_na_clean <- bfi_na_clean %>% 
  mutate(A2_z = (A2 - mean(A2))/sd(A2))

## A3
bfi_na_clean <- bfi_na_clean %>% 
  mutate(A3_z = (A3 - mean(A3))/sd(A3))

## A4
bfi_na_clean <- bfi_na_clean %>% 
  mutate(A4_z = (A4 - mean(A4))/sd(A4))

## A5
bfi_na_clean <- bfi_na_clean %>% 
  mutate(A5_z = (A5 - mean(A5))/sd(A5))

# SUMMARISE/SUMMARIZE
# Criando uma tabela com valores médios dos escores de Amabilidade
bfi_na_clean %>% 
  summarize(A1_mean = mean(A1, na.rm=TRUE),
            A2_mean = mean(A2, na.rm=TRUE),
            A3_mean = mean(A3, na.rm=TRUE),
            A4_mean = mean(A4, na.rm=TRUE),
            A5_mean = mean(A5, na.rm=TRUE),
            n = n()
            )

# Criando uma tabela com valores médios dos escores de Abertura ao novo
bfi_na_clean %>% 
  summarize(O1z_mean = mean(O1, na.rm=TRUE),
            O2z_mean = mean(O2, na.rm=TRUE),
            O3z_mean = mean(O3, na.rm=TRUE),
            O4z_mean = mean(O4, na.rm=TRUE),
            O5z_mean = mean(O5, na.rm=TRUE),
            n = n()
  )

# DPLYR::SUMMARISE + FILTER
O_table_women <- bfi_na_clean %>% 
  filter(gender==2) %>% 
  summarize(O1_mean = mean(O1, na.rm=TRUE),
            O2_mean = mean(O2, na.rm=TRUE),
            O3_mean = mean(O3, na.rm=TRUE),
            O4_mean = mean(O4, na.rm=TRUE),
            O5_mean = mean(O5, na.rm=TRUE),
            n = n()
  ) 

frq(bfi_na_clean$gender)            

# GROUP_BY
# Criando uma tabela com valores médios dos escores de Amabilidade por genero
A_gender_table <- bfi_na_clean %>% 
  group_by(gender) %>% 
  summarize(A1_mean = mean(A1, na.rm=TRUE),
            A2_mean = mean(A2, na.rm=TRUE),
            A3_mean = mean(A3, na.rm=TRUE),
            A4_mean = mean(A4, na.rm=TRUE),
            A5_mean = mean(A5, na.rm=TRUE),
            n = n()
  )

# Criando uma tabela com valores médios dos escores de Amabilidade por educação
A_edu_table <- bfi_na_clean %>% 
  group_by(education) %>% 
  summarize(A1z_mean = mean(A1, na.rm=TRUE),
            A2z_mean = mean(A2, na.rm=TRUE),
            A3z_mean = mean(A3, na.rm=TRUE),
            A4z_mean = mean(A4, na.rm=TRUE),
            A5z_mean = mean(A5, na.rm=TRUE),
            n = n()
  )
