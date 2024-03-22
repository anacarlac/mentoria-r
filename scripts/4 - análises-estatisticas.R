# Carregando os pacotes
library(tidyverse)
library(psych)
library(janitor)
library(sjmisc)
library(careless)
library(formattable)
library(qgraph)
library(corrplot)
library(lavaan)
library(PROscorerTools)
library(sjPlot)
library(readxl)
library(DT)
library(ggthemes)
library(stats)

# Carregar o banco de dados limpo e dicionário
bd_mh_fake <- readRDS("data/bd_mh_fake.RDS")
dic_mh <- read_excel("dic/dic_mtl.xlsx")
dic_senna <- read_excel("dic/nomes_facetas_socioemocional.xlsx")

# Análises descritivas

# Exemplos práticos

# Gênero
table_gender <- frq(bd_mh_fake$sexo)

table_gender <- table_gender %>% 
  as.data.frame() %>% 
  dplyr::select(-variable, -label) %>% 
  dplyr::filter(val %in% c("F","M")) %>% 
  dplyr::rename(
    "Gênero" = "val",
    "Número de estudantes" = "frq",
    "Porcentagem absoluta" = "raw.prc",
    "Porcentagem cumulativa" = "cum.prc"
  )

formattable(table_gender, list(
            'Porcentagem absoluta' = color_bar("lightblue"),
            'Número de estudantes' = color_bar("#FFDE55")
            )) %>% 
  as.datatable()

# Ano escolar
table_anoescolar <- frq(bd_mh_fake$serie)
table_anoescolar <- table_anoescolar %>% 
  as.data.frame() %>% 
  dplyr::select(-variable, -label) %>% 
  dplyr::filter(val %in% c("9","12")) %>% 
  dplyr::rename(
    "Ano escolar" = "val",
    "Número de estudantes" = "frq",
    "Porcentagem absoluta" = "raw.prc",
    "Porcentagem cumulativa" = "cum.prc"
  )

formattable(table_anoescolar, list(
  'Porcentagem absoluta' = color_bar("lightblue"),
  'Número de estudantes' = color_bar("#FFDE55")
)) %>%
  as.datatable()

# Respostas de autorrelato

# Saúde mental
bd_mh_fake %>% 
  select(Mtl_Hth_01:Mtl_Hth_12) %>% 
  skimr::skim()

# Competências socioemocionais
bd_mh_fake %>% 
  select(A_9:IntCur_9) %>% 
  skimr::skim()

# Como está a saúde mental dos estudantes dos estudantes após o retorno às aulas?

# Quantos alunos tem tido dificuldade em nível muito alto para conseguir se concentrar naquilo que fazem?
frq(bd_mh_fake$Mtl_Hth_01) # Mtl_Hth_01	Você tem conseguido se concentrar naquilo que faz? #4.95%
   
# Quantos estudantes se sentem totalmente esgotados e sob pressão?
frq(bd_mh_fake$Mtl_Hth_05) # Você tem se sentido esgotado e sob pressão? #23.8%

# Quantos alunos costumam perder totalmente o sono por conta das suas preocupações?
frq(bd_mh_fake$Mtl_Hth_02) # Você tem perdido o sono frequente por causa de suas preocupações? #19.82%

# Quantos alunos relataram que perderam totalmente a confiança em si?
frq(bd_mh_fake$Mtl_Hth_10) # Mtl_Hth_10	Você tem perdido a confiança em si mesmo?* #13.5%

# Quantos alunos se sentem totalmente infelizes e deprimidos?
frq(bd_mh_fake$Mtl_Hth_09) # Mtl_Hth_09	Você tem se sentido infeliz e deprimido?* #11.60%

# E para a frequência de todos os itens ao mesmo tempo?

# Primeiro, crie um vetor com o nome das variáveis a serem usadas
v_mh_frq <- bd_mh_fake %>% 
  dplyr::select(Mtl_Hth_01:Mtl_Hth_12) %>% 
  colnames()

# Segundo, monte um tabela com as frequências e porcentagens usando verbos do dplyr + pivotwider
tabela_itens_mh <- bd_mh_fake %>% 
  select(v_mh_frq) %>% 
  gather(key, value) %>%
  na.omit() %>% 
  count(key, value) %>% 
  group_by(key) %>% 
  mutate(perc = round((n/sum(n)*100),2)) %>% 
  pivot_wider(id_cols = c("key"),
              names_from = value,
              values_from = c(n, perc)) %>% 
  dplyr::rename(
    "coditem" = "key",
    "N categoria 1" = "n_1",
    "N categoria 2" = "n_2",
    "N categoria 3" = "n_3",
    "N categoria 4" = "n_4",
    "N categoria 5" = "n_5",
    "Porcentagem categoria 1" = "perc_1",
    "Porcentagem categoria 2" = "perc_2",
    "Porcentagem categoria 3" = "perc_3",
    "Porcentagem categoria 4" = "perc_4",
    "Porcentagem categoria 5" = "perc_5",
  )

# Terceiro, use a tabela de dicionário para combinar com a tabela de frequência e adicionar o conteúdo das afirmativas por meio do coditem
tabela_itens_mh <- merge(x = tabela_itens_mh, y = dic_mh[ , c("coditem", "item_text_pt")], by = "coditem", all.x=TRUE)

# Quarto, ajuste a ordem das variáveis, renomeie o que ainda for necessário e retire variáveis extras
tabela_itens_mh <- tabela_itens_mh %>% 
  relocate(c("item_text_pt"), .before = "N categoria 1") %>% 
  dplyr::rename(
    "Código" = "coditem",
    "Afirmativa" = "item_text_pt"
  ) %>% 
  dplyr::select(-'N categoria 1', -'N categoria 2',-'N categoria 3',-'N categoria 4',-'N categoria 5', -'Código')

# Quinto, use o formattable para ter uma visualização 
formattable(tabela_itens_mh, list(
  'Porcentagem categoria 1' = color_tile("white", "pink"),
  'Porcentagem categoria 5' = color_tile("white", "pink")
)) %>%
  as.datatable()

# Análises diagnósticas

# Correlações

# Criar o vetor com nome das variáveis de Senna
v_socioemo <- bd_mh_fake %>% 
  dplyr::select(A_9:IntCur_9) %>% 
  colnames()

# Calcular um objeto com as correlações do Senna por linha e Saúde mental na coluna
corr_senna_mh <- corr.test(x = bd_mh_fake[ ,v_socioemo], y = bd_mh_fake$mh_pompscore)

# Visualizar as correlações
print(corr_senna_mh)

# Visualizar correlação com gráficos
bd_mh_fake %>% 
  ggplot(aes(x=mh_pompscore, y=LDep_9)) + 
    geom_point(color = "steelblue", alpha = 0.2) +
    xlim(0,85) +
    ylim(0, 8) +
    theme_hc()

bd_mh_fake %>% 
  ggplot(aes(x=mh_pompscore, y=Tru_9)) + 
  geom_point(color = "#FC4E07", alpha = 0.2) +
  xlim(0,85) +
  ylim(0, 10) +
  theme_hc()

# Regressão linear múltipla
# Criar um modelo de regressão com saúde mental como DV (Y) e conjunto de competências socioemocionais como IV (X)
fit_mh <- lm(mh_pompscore ~ Act_9 + Tru_9 + LDep_9 + LAnx_9 + Conc_9 + Achv_9, data=bd_mh_fake)

# Visualizar os resultados do modelo
summary(fit_mh)

sjPlot::tab_model(fit_mh,
          show.std = TRUE,
          show.se =TRUE,
          show.ci = 0.95,
          digits.p = 3)

# Gráficos

# By wb CSE cats
table_domains_mtl_act <- bd_mh_fake %>% 
  group_by(Act_cat) %>% 
  summarize(
    mtl_mean = mean(mh_pompscore, na.rm=TRUE)
  ) %>% 
  pivot_longer(
    cols = "Act_cat",
    names_to = "domain",
    values_to = c("nivel")
  )

table_domains_mtl_tru <- bd_mh_fake %>% 
  group_by(Tru_cat) %>% 
  summarize(
    mtl_mean = mean(mh_pompscore, na.rm=TRUE)
  ) %>% 
  pivot_longer(
    cols = "Tru_cat",
    names_to = "domain",
    values_to = c("nivel")
  )

table_domains_mtl_ldep <- bd_mh_fake %>% 
  group_by(LDep_cat) %>% 
  summarize(
    mtl_mean = mean(mh_pompscore, na.rm=TRUE)
  ) %>% 
  pivot_longer(
    cols = "LDep_cat",
    names_to = "domain",
    values_to = c("nivel")
  )

table_domains_mtl_lanx <- bd_mh_fake %>% 
  group_by(LAnx_cat) %>% 
  summarize(
    mtl_mean = mean(mh_pompscore, na.rm=TRUE)
  ) %>% 
  pivot_longer(
    cols = "LAnx_cat",
    names_to = "domain",
    values_to = c("nivel")
  )

table_domains_mtl_conc <- bd_mh_fake %>% 
  group_by(Conc_cat) %>% 
  summarize(
    mtl_mean = mean(mh_pompscore, na.rm=TRUE)
  ) %>% 
  pivot_longer(
    cols = "Conc_cat",
    names_to = "domain",
    values_to = c("nivel")
  )

table_domains_mtl_achv <- bd_mh_fake %>% 
  group_by(Achv_cat) %>% 
  summarize(
    mtl_mean = mean(mh_pompscore, na.rm=TRUE)
  ) %>% 
  pivot_longer(
    cols = "Achv_cat",
    names_to = "domain",
    values_to = c("nivel")
  )

table_mtl <- rbind(
  table_domains_mtl_act, 
  table_domains_mtl_tru, 
  table_domains_mtl_ldep, 
  table_domains_mtl_lanx, 
  table_domains_mtl_conc, 
  table_domains_mtl_achv
)

table_mtl$nivel <- as.factor(table_mtl$nivel)

table_mtl$nivel <- dplyr::recode(table_mtl$nivel,
                            'baixo' = "Pouco \ndesenvolvido",
                            'emerg' = "Medianamente \ndesenvolvido",
                            'capaz' = "Bastante \ndesenvolvido",
                            'muit/cpz' = "Muito \ndesenvolvido"
)

table_mtl$domain <- dplyr::recode(table_mtl$domain,
                            'Act_cat' = "Entusiasmo",
                            'Tru_cat' = "Confiança",
                            'LDep_cat' = "Autoconfiança",
                            'LAnx_cat' = "Tolerância ao estresse",
                            'Conc_cat' = "Foco",
                            'Achv_cat' = "Determinação"
)

table_mtl %>%
  mutate(
    nivel = 
      fct_relevel(nivel, "Pouco \ndesenvolvido", 
                  "Medianamente \ndesenvolvido", 
                  "Bastante \ndesenvolvido", 
                  "Muito \ndesenvolvido")) %>%
  na.omit(nivel) %>%
  ggplot(aes(x = nivel, y =  mtl_mean, fill=domain)) + 
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9)) +
  geom_text(aes(label=round(mtl_mean, 1)), 
            position=position_dodge(width=0.9), 
            vjust=-0.5,
            size = 4,
            fontface = "bold") +
  scale_fill_manual(values=c("#00A8E9", "#0092D2", "#FFDE55", "#00528F", "#003C78", "#FFCD00")) +
  ylim(0, 80) +
  theme_hc() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_blank()
  )

ggsave(
  "Gráfico - Banco Fake - Saúde Mental.png", 
  height = 7, 
  width = 10, 
  dpi = 300
)
