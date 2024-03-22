# Carregando os pacotes
library(beepr)
library(tidyverse)
library(datasauRus)
library(psych)
library(janitor)
library(sjmisc)
library(careless)
library(readxl)
library(ggthemes)

# O que é R? 

## Cumprimentando o R
print('Olá!')

## Vamos fazer o R cantar?
beep(sound=8)

## O R analisando dados
ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol = 3)

## Abrir o banco de dados
bd_mh_fake <- read_excel("data_raw/data_raw_ias.xlsx")

## Visualizar o banco de dados
View(bd_mh_fake)

# Verificar premissas do tidy data
View(bd_mh_fake)
glimpse(bd_mh_fake)

# Identificar o nome das variáveis
names(bd_mh_fake)

# Ponto de atenção 1: nomes de variáveis com caixa alta ou espaço entre elas
bd_mh_fake <- bd_mh_fake %>% 
  clean_names(case = "none", 
              replace = c("é" = "e"," " = "_"))

names(bd_mh_fake)

# Ponto de atenção 2: As características pessoais estão fora de ordem no banco de dados
bd_mh_fake <- relocate(bd_mh_fake, c("sexo"), .before = "Mtl_Hth_01")

names(bd_mh_fake)

# Tipos de variáveis
glimpse(bd_mh_fake)

# Checagem de dados duplicados sem a variável id 
get_dupes(bd_mh_fake)

get_dupes(bd_mh_fake, -id)

# Removendo os participantes com linhas duplicadas

bd_dupes <- get_dupes(bd_mh_fake, -id)

v_dupes <- bd_dupes$id

bd_mh_fake <- bd_mh_fake %>% 
  filter(!id %in% c(v_dupes))

get_dupes(bd_mh_fake, -id)

# Checagem de respostas descuidadas
# Separe as medidas de autorrelato para isso

bd_mh_fake_careless <- bd_mh_fake %>% 
  select(id, Mtl_Hth_01:Mtl_Hth_12)

bd_mh_fake_careless$careless <- longstring(bd_mh_fake_careless, avg = FALSE)

frq(bd_mh_fake_careless$careless)

# Vamos gerar um histograma para essa distribuição?

bd_mh_fake_careless %>% 
  ggplot(aes(x=careless)) + 
  geom_histogram(binwidth=1, 
                 color="black", fill="#00528F") + 
  theme_hc() +
  geom_vline(aes(xintercept=mean(careless)),
             color="orange", linetype="dashed", size=1)

# Lidando com as respostas descuidadas
bd_mh_fake <- merge(x = bd_mh_fake, y = bd_mh_fake_careless[ , c("id", "careless")], by = "id", all.x=TRUE)

bd_mh_fake <- bd_mh_fake %>% 
  filter(!careless >= 10) # x

# Características pessoais e socioeconômicas
frq(bd_mh_fake$sexo)

frq(bd_mh_fake$serie)

# Salvando o banco de dados em /data
saveRDS(bd_mh_fake, "data/bd_mh_fake.RDS")