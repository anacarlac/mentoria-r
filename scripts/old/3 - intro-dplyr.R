# Carregando pacotes
library(tidyverse)
library(psychTools)
library(readxl)

# Como abrir banco em Excel?
banco_exemplo_excel <- read_excel("data_raw/banco_exemplo_excel.xlsx", sheet ="Planilha2")
View(banco_exemplo_excel)

# Aquecimento
# Big Five
data(bfi)
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

### ALTERNATIVE
banco_big5 <- bfi %>% 
  select(starts_with("O") | starts_with("C") | starts_with("E") |starts_with("A") |starts_with("N"), -education, -age)

banco_big5 <- bfi %>% 
  select(A1:O5)

# Transformações de classes
# Transforme essa string em factor
escolaridade <- c("Médio", "Superior", "Fundamental", "Fundamental", "Médio")
fator <- as.factor(escolaridade)
fator

# Transforme a matriz em um data frame
numero_frutas <- matrix(1:10,nrow=5, ncol=5)
dataframe <- as.data.frame(numero_frutas)

# FILTER
# Cria um objeto e atribui a ele as linhas com mulheres.
mulheres <- bfi %>% 
  filter(gender == 2)

# Mulheres com menos de 30 anos
mulheres_menor30 <- bfi %>% 
  filter(gender == 2 & age < 30)

# Filtre pessoas que estão na graduação ou finalizando-a.
grads <- bfi %>% 
  filter(education == 3 | education == 4)

# Alternativa: Filtre pessoas que estão na graduação OU finalizando-a.
grads <- bfi %>% 
  filter(education %in% c(3, 4))

# Existe alguém sem informação de escolaridade? 
# is.na() - função que retorna TRUE se o valor for NA e FALSE se não for.
edu_na<- bfi %>% 
  filter(is.na(education))

# Carros que são da marca Mercedes (Merc) em alguma posição da variável cars.
# str_detect - função que retorna TRUE se detectou o valor dado e 
# FALSE, caso não tenha encontrado.
data(mtcars)
mtcars <- tibble::rownames_to_column(mtcars, "cars")
merc <- mtcars %>% 
  filter(str_detect(cars, "Merc"))

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

# Filtra as mulheres de 30 anos e ordena por escolaridade.
bfi %>% 
  filter(gender == 2 & age ==30) %>% 
  arrange(education)

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

