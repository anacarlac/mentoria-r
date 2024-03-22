#Carregando pacotes
library(tidyverse)
library(lsr)
library(psych)

# T-TEST

#Comparando médias: dois grupos
#Dados sobre consumo diário de calorias para crianças e adultos
crianças <- c(200,150,150,100)
adultos <- c(300,400,500,500)

#teste 1 - qual média é necessária para que as diferenças não sejam significativas?
crianças <- c(200,150,150,100)
adultos <- c(200,400,300,300)

#teste 2 - qual média é necessária para que as diferenças não sejam significativas?
crianças <- c(200,150,150,100)
adultos <- c(200,300,300,300)

#teste 3 - qual média é necessária para que as diferenças não sejam significativas?
crianças <- c(200,150,150,100)
adultos <- c(200,100,100,300)

#Vamos descobrir se existe uma diferença significativa entre o consumo diário de calorias?
t.test(crianças,adultos)

#teste 5 - porque o t score é negativo? Seria por conta da ordem dos grupos?
t.test(adultos,crianças)

#Tamanho do efeito
cohensD(crianças,adultos)

# ANOVA
data(bfi)

# Construindo os escores de A
scores <- scoreItems(bfi.keys,bfi,min=1,max=6)  #using a keys matrix
scores_bfi <- as.data.frame(scores$scores)
bfi <- cbind(bfi, scores_bfi)
bfi$education <- as.factor(bfi$education)

# Construindo a tabela de médias
table_gender <- bfi %>% 
  group_by(education) %>%
  summarise(
    count = n(),
    mean = mean(agree, na.rm = TRUE),
    sd = sd(agree, na.rm = TRUE)
  ) %>% 
  drop_na()

# Análise ANOVA
anova_bfi_gender <- aov(agree ~ education, data = bfi)

# Resultados ANOVA
summary(anova_bfi_gender)

# Post-hoc
TukeyHSD(anova_bfi_gender)


# Gráfico para visualizar as médias
library("ggpubr")
ggboxplot(bfi, x = "education", y = "agree",
          color = "education", palette = c("#00AFBB", "#E7B800", "#FC4E07", "black", "#00AFBB"),
          #order = c("ctrl", "trt1", "trt2"),
          ylab = "Escore A", xlab = "Escolaridade")

# Chi-square
data(housetasks)
