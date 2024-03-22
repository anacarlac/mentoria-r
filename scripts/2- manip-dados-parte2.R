# Manipulação de dados: pacotes e funções essenciais

# Carregando pacotes
library(tidyverse)
library(psychTools)
library(readxl)
library(sjmisc)
library(readxl)

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
filter(dados_dplyr, cyl >= 6)

dados_dplyr %>% 
  filter(cyl >= 6 &
         mpg > 200, #apaguei porque eu quis
         #cyl == 0)
  )

dados_dplyr %>% 
  filter(cyl >= 6)

## Crie um banco de dados com esse critério
carros_cyl_6oumais <- dados_dplyr %>% 
  filter(cyl >= 6)

## Exercício 1:
## Selecione carros com transmissão automática
dados_dplyr %>% 
  filter(am == 0)

## Novo banco de dados apenas com esse critério de am == 0
carros_am_aut <- dados_dplyr %>% 
  filter(am == 0)

# Agora abra um novo banco de dados
data(bfi)

# Cria um objeto e atribui a ele as linhas com mulheres.
mulheres <- bfi %>% filter(gender == 2)

# Mulheres com menos de 30 anos
mulheres_menor30 <- bfi %>% filter(gender == 2 & age < 30)

# Filtre pessoas que estão na graduação OU finalizando-a.
grads <- bfi %>% filter(education == 3 | education == 4)

# ALTERNATIVA: Filtre pessoas que estão na graduação OU finalizando-a.
grads <- bfi %>% filter(education %in% c(3, 4))

# Existe alguém sem informação de escolaridade? 
# is.na() - função que retorna TRUE se o valor for NA e FALSE se não for.
edu_na<- bfi %>% filter(!is.na(Education))

# Carros que são da marca Mercedes (Merc) em alguma posição da variável cars.
# str_detect - função que retorna TRUE se detectou o valor dado e 
# FALSE, caso não tenha encontrado.
data(mtcars)
mtcars <- tibble::rownames_to_column(mtcars, "cars")
merc <- mtcars %>% filter(str_detect(cars, "Merc"))

## SELECT
## Selecionando variáveis e atribuindo para um objeto
dados_dplyr_select <- dados_dplyr %>% 
  select(mpg, cyl, am)

## Exercício 2:
## Selecione milhagem, cilindros, potência (gross horsepower) e peso
dados_dplyr_select_plus<- dados_dplyr %>% 
  select(mpg, cyl, hp, wt)

# Seleciona as colunas indicadas: SES + itens de Amabilidade.
bfi %>% select(age, gender, education, A1, A2, A3, A4, A5)

# Seleciona as colunas que COMEÇAM com E (Extroversão).
bfi %>% select(starts_with("E"))

# Seleciona as colunas que TERMINAM com “5".
bfi %>% select(ends_with("5"))

# Seleciona as colunas que contém com “C“ (em qualquer posição da palavra).
bfi %>% select(contains("C"))

# Retire as variáveis idade e gênero 
bfi%>% select(-age,  -gender)

# Retire as variáveis idade e os itens de Amabilidade
bfi %>% select(-starts_with("A"), -age)

# A prática de FILTER + SELECT
# Big Five
data(bfi.dictionary)

# Quantos itens você encontrou nesse instrumento?
# 25 variáveis
## Na coluna IPIP100 do bfi.dictionary, você irá ver que os itens do Big Five começam com "B5"
## Dica: busque no google "filter with string in r"
itens_b5<- bfi.dictionary %>% 
  filter(str_detect(IPIP100, 'B5'))

# Onde que os autores encontraram os itens desse instrumento?
#International Personality Item Pool
?psychTools::bfi 

# Quantas pessoas responderam esse questionário?
# 2800

# Monte um banco de dados com variáveis BFI apenas
bfi.dictionary <- tibble::rownames_to_column(bfi.dictionary, "coditem")

v_big5 <- bfi.dictionary %>% 
  filter(str_detect(IPIP100, 'B5')) %>% 
  pull(coditem) %>% 
  as.vector()

banco_big5 <- bfi %>% 
  select(v_big5)

### ALTERNATIVA
banco_big5 <- bfi %>% 
  select(starts_with("O") | starts_with("C") | starts_with("E") |starts_with("A") |starts_with("N"), -education, -age)

banco_big5 <- bfi %>% 
  select(A1:O5)

# Identifique quais variáveis tem NAs no banco de dados
frq(bfi)

# E se quiséssemos filtrar os dados para ter um banco de dados com informações de pessoas que tem respostas nas variáveis SES?
bfi_na_edu <- bfi %>% 
  filter(!is.na(education))

# Vamos criar um banco de dados sem missing nas variáveis?
bfi_na_clean <- bfi %>% 
  filter(if_all(everything(), ~ !is.na(.x)))

# ARRANGE
# Ordene o banco de acordo com a variável gender
bfi_gender_order <- bfi %>% 
  arrange(gender)

View(bfi_gender_order)

# Ordena por ordem decrescente de idade e por ordem crescente gênero.
bfi_gender_age_order <- bfi %>% 
  arrange(desc(age), gender)

View(bfi_gender_age_order)

# Ordenando pelas variáveis que tem muitos campos vazios ou NA 
bfi%>% 
  arrange(desc(is.na(education)))%>% 
  glimpse()

head(bfi)
View(bfi)
glimpse(bfi)

# Filtre as mulheres de 30 anos e ordena por escolaridade.
bfi %>% 
  filter(gender == 2 & age ==30) %>% 
  arrange(education)

# MUTATE
## Alterando a métrica de uma variável
## A1
bfi_na_clean <- bfi_na_clean %>% 
  mutate(A1_z = (A1 - mean(A1))/sd(A1))

bfi_na_clean <- bfi_na_clean %>% 
  mutate(A1_2 = A1)

psych::describe(bfi_na_clean$A1)
psych::describe(bfi_na_clean$A1_2)
psych::describe(bfi_na_clean$A1_z)

bfi_na_clean <- bfi_na_clean %>% 
  select(-A1_2)

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
  summarize(A1z_mean = round(mean(A1_z, na.rm=TRUE), 3),
            A2z_mean = round(mean(A2_z, na.rm=TRUE), 3),
            A3z_mean = round(mean(A3_z, na.rm=TRUE), 3),
            A4z_mean = round(mean(A4_z, na.rm=TRUE), 3),
            A5z_mean = round(mean(A5_z, na.rm=TRUE), 3),
            n = n()
  )

bfi_na_clean %>% 
  summarize(A1_mean = round(mean(A1, na.rm=TRUE), 3),
            A2_mean = round(mean(A2, na.rm=TRUE), 3),
            A3_mean = round(mean(A3, na.rm=TRUE), 3),
            A4_mean = round(mean(A4, na.rm=TRUE), 3),
            A5_mean = round(mean(A5, na.rm=TRUE), 3),
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




# SELECT
# Seleciona as colunas indicadas: SES + itens de Amabilidade.
bfi %>% 
  select(age, gender, education, A1, A2, A3, A4, A5)

# Seleciona as colunas que COMEÇAM com E (Extroversão).
bfi %>% 
  select(starts_with("E"))

bfi %>% 
  select(starts_with("E"), -education)

# Seleciona as colunas que TERMINAM com 5.
bfi %>% 
  select(ends_with("5"))

# Seleciona as colunas que contém com C (em qualquer posição da palavra).
bfi %>% 
  select(contains("C"))

# Retire as variáveis idade e gênero 
bfi%>% 
  select(-age,  -gender)

# Retire as variáveis idade e os itens de Amabilidade
bfi %>% 
  select(-starts_with("A"), -age)

# EXTRA: Como abrir banco em Excel?
banco_exemplo_excel <- read_excel("data_raw/banco_exemplo_excel.xlsx", sheet ="Planilha2")
View(banco_exemplo_excel)
