#Exercício 3: Aplicando os verbos do dplyr
#Vamos usar os verbos do dplyr para manipular as variáveis da BFI!

#Carregue os pacotes tidyverse, psychTools e psych
#Lembre-se que você precisa carregar os pacotes toda vez que abrir uma sessão R.
library(tidyverse)
library(psychTools)
library(psych)

#Carregue o banco de dados e o dicionário da BFI que está no pacote psychTools
data(bfi)
data(bfi.dictionary)

#R in a hurry!
#Vamos treinar o uso dos verbos do dplyr, um por vez!

#dplyr::filter
#1. Crie um banco de dados com todos os participantes homens com 40 anos ou mais
#RESPOSTA: Banco com 137 observações e 28 variáveis
bfi_homens_40oumais <- bfi %>% 
  filter(gender==1 & age >=40)

#2. Crie um banco de dados sem missings na variável de educação
#RESPOSTA: Banco com 2577 observações e 28 variáveis
bfi_sem_missings_education <- bfi %>% 
  filter(!is.na(education))

#3. Crie um banco de dados sem missings em qualquer variável
#RESPOSTA: Banco com 2236 observações e 28 variáveis
bfi_na_clean <- bfi %>% 
  filter(if_all(everything(), ~ !is.na(.x)))

#dplyr::select
#1. Crie um banco de dados com variáveis sociodemográficas apenas
#RESPOSTA: Banco com 2800 observações e 3 variáveis
bfi_ses_only <- bfi %>% 
  select(age, gender, education)

#2. Crie um banco de dados com variáveis de Amabilidade e SES apenas
#RESPOSTA: Banco com 2800 observações e 8 variáveis
bfi_A_ses <- bfi %>% 
  select(age, gender, education, starts_with("A"))

#3. Crie um banco de dados com variáveis de Conscienciosidade, SES e depois filtre para o banco para ter mulheres apenas
#RESPOSTA: Banco com 1881 observações e 8 variáveis
bfi_C_ses_mulheres <- bfi %>% 
  select(age, gender, education, starts_with("C")) %>% 
  filter(gender==2)

#dplyr::arrange
#1. Crie um banco de dados com variáveis de Amabilidade e SES apenas e ordene-o crescentemente por idade
#RESPOSTA: Banco com 2800 observações e 8 variáveis e o participante na linha 1 tem 3 anos de idade
bfi_A_ses <- bfi %>% 
  select(age, gender, education, starts_with("A")) %>% 
  arrange(age)

#2. Crie um banco de dados com variáveis sociodemográficas apenas e ordene-o decrescentemente por idade
#RESPOSTA: Banco com 2800 observações e 3 variáveis e o participante na linha 1 tem 86 anos de idade
bfi_ses_only <- bfi %>% 
  select(age, gender, education) %>% 
  arrange(-age)

#3. Crie um banco de dados com variáveis sociodemográficas apenas e ordene-o decrescentemente por idade e crescentemente pela variável gender
#RESPOSTA: Banco com 2800 observações e 3 variáveis. O participante na linha 1 tem 86 anos de idade e é uma mulher (gender==2)
bfi_ses_only <- bfi %>% 
  select(age, gender, education) %>% 
  arrange(-age, gender)

#dplyr::mutate
#1.Crie um banco de dados que tenha o escore Z de cada variável de Amabilidade computado para cada participante
#RESPOSTA: Banco com 2800 observações e 33 variáveis 
bfi_A_escorez <- bfi %>% 
  mutate(A1_z = (A1 - mean(A1,na.rm=TRUE))/sd(A1,na.rm=TRUE),
         A2_z = (A2 - mean(A2,na.rm=TRUE))/sd(A2,na.rm=TRUE),
         A3_z = (A3 - mean(A3,na.rm=TRUE))/sd(A3,na.rm=TRUE),
         A4_z = (A4 - mean(A4,na.rm=TRUE))/sd(A4,na.rm=TRUE),
         A5_z = (A5 - mean(A5,na.rm=TRUE))/sd(A5,na.rm=TRUE)
         )

#1.Repita o procedimento acima de escore Z para as variáveis de Conscienciosdade
#RESPOSTA: Banco com 2800 observações e 33 variáveis 
bfi_C_escorez <- bfi %>% 
  mutate(C1_z = (C1 - mean(C1,na.rm=TRUE))/sd(C1,na.rm=TRUE),
         C2_z = (C2 - mean(C2,na.rm=TRUE))/sd(C2,na.rm=TRUE),
         C3_z = (C3 - mean(C3,na.rm=TRUE))/sd(C3,na.rm=TRUE),
         C4_z = (C4 - mean(C4,na.rm=TRUE))/sd(C4,na.rm=TRUE),
         C5_z = (C5 - mean(C5,na.rm=TRUE))/sd(C5,na.rm=TRUE)
  )

#dplyr::group_by+summarise
#1.Use o objeto criado de escore Z para variáveis de A, selecione apenas variáveis de A e gênero, e tire a média de cada variável por genero
bfi_A_escorez %>% 
  select(gender, A1_z:A5_z) %>% 
  group_by(gender) %>% 
  summarise(mean_A1 = mean(A1_z, na.rm=TRUE),
            mean_A2 = mean(A2_z, na.rm=TRUE),
            mean_A3 = mean(A3_z, na.rm=TRUE),
            mean_A4 = mean(A4_z, na.rm=TRUE),
            mean_A5 = mean(A5_z, na.rm=TRUE),
            )

#Em geral, homens tem escores maiores ou menos do que mulheres nas variáveis de A?
#RESPOSTA: Os homens apresentaram médias menores do que mulheres em 4 das 5 perguntas. Portanto, no geral, eles têm escores menores do que mulheres em Amabilidade.

#1.Use o objeto criado de escore Z para variáveis de C, selecione apenas variáveis de A e gênero, e tire a média de cada variável por genero
bfi_C_escorez %>% 
  select(gender, C1_z:C5_z) %>% 
  group_by(gender) %>% 
  summarise(mean_C1 = mean(C1_z, na.rm=TRUE),
            mean_C2 = mean(C2_z, na.rm=TRUE),
            mean_C3 = mean(C3_z, na.rm=TRUE),
            mean_C4 = mean(C4_z, na.rm=TRUE),
            mean_C5 = mean(C5_z, na.rm=TRUE),
  )

#Em geral, homens tem escores maiores ou menos do que mulheres nas variáveis de C?
#RESPOSTA: Os homens apresentaram médias menores do que mulheres em 3 das 5 perguntas. Portanto, no geral, eles têm escores menores do que mulheres em Conscienciosidade.

#Agora vamos fazer um exercício um pouco mais avançado!
#Vamos criar escores para cada dimensão do Big Five?
#Use o ?psychTools::bfi e vá na seção de exemplos do Help
#Veja que ele dá orientações para saber a distribuição das variáveis por meio do psych::describe.
#E você também encontra um código com scoreItems que serve para calcular os escores por dimensões

#Use o psych::describe primeiro e veja a distribuição das variáveis
#Se tiver dúvida sobre o que cada coluna significa, use ?psych::describe
psych::describe(bfi)

#Agora vamos calcular os escores!
#No objeto bfi.dicitonary você encontra uma coluna chamada Keying
#Nessa coluna é indicado quais itens são invertidos, ou seja, itens com valor -1 nessa coluna.
#A função scoreItems vai calcular os escores e inverter os escores necessários.
#Use a função scoreItems e atribua essa função para um objeto chamado bfi_scores.
#Os valores mínimos e máximos de cada item da BFI precisam ser identificados com argumentos nessa função, como você pode ver em min e max
bfi_scores <- psych::scoreItems(bfi.keys,bfi,min=1,max=6) 

#Faça um view(bfi_scores).
#Esse é um objeto do tipo lista e temos resultados de confiabilidade, correlação, entre outros para os escores calculados.
View(bfi_scores)

#Para usar os verbos do dplyr, vamos precisar do objeto scores que está dentro do objeto bfi_scores
#Atribua esse objeto para um objeto novo
#Veja que esse novo objeto terá estrutura de matrix e não de dataframe
bfi_scores_only <- bfi_scores$scores

#Transforme bfi_scores_only em data.frame e atribua o resultado novamente para o objeto bfi_scores_only
bfi_scores_only <- as.data.frame(bfi_scores_only)

#Como esses dados estão sem as variáveis de gênero e idade, precisamos unir o banco bfi com o banco com escores calculados, certo?
#Use o comando a seguir para fazer essa operação
#Note que os códigos de identificação de cada estudante estão indicados como rownames em ambos os bancos bfi e bfi_scores_only]
#E especificamente nesse caso, o banco bfi_scores_only manteve a mesma ordem de linhas que o banco bfi
#Por isso, podemos usar o comando cbind sem precisar indicar uma variável que reflita o código de identificação dos participantes em ambos bancos para fazer o "match"
bfi_allvars <- cbind(bfi, bfi_scores_only)

#Pronto, agora temos um banco com todas as variáveis
#Esse banco deve ter 2800 observações e 33 variáveis: 28 vars do bfi e 5 vars do bfi_scores_only
names(bfi_allvars)

#Qual é a pontuação média e o DP de homens e mulheres nos domínios da BFI? 
#Selecione as variáveis agree, conscientious, extraversion, neuroticism, openness e gender
#E então use o group_by()+summarise() para saber essa informação
bfi_allvars %>% 
  select(gender, agree, conscientious, extraversion, neuroticism, openness) %>% 
  group_by(gender) %>% 
  summarise(
    mean_O = mean(openness, na.rm=TRUE),    
    dp_O = sd(openness, na.rm=TRUE),
    mean_C = mean(conscientious, na.rm=TRUE),
    dp_C = sd(conscientious, na.rm=TRUE),    
    mean_E = mean(extraversion, na.rm=TRUE),
    dp_E = sd(extraversion, na.rm=TRUE),
    mean_A = mean(agree, na.rm=TRUE),
    dp_A = sd(agree, na.rm=TRUE),
    mean_N = mean(neuroticism, na.rm=TRUE),
    dp_N = sd(neuroticism, na.rm=TRUE)
  )

#Em quais domínios que as mulheres apresentaram uma pontuação média maior? Indique as médias e DPs.
#Resposta: As mulheres apresentaram maiores médias em Conscienciosidade (M = 4.33, SD=0.93), Extroversão (M = 4.22, SD=1.02), Amabilidade (M = 4.78, SD=0.85) e Neuroticismo (M = 3.27, SD=1.2)

#Em quais domínios que os homens apresentaram uma pontuação média maior? Indique as médias e DPs.
#Resposta: Os homens apresentaram maiores médias em Openness apenas (M = 4.66, SD=0.81)

#EXTRA
#Pense sobre o que isso significa e veja o artigo SOTO et al (2011): Age Differences in Personality Traits From 10 to 65: Big Five Domains and Facets in a Large Cross-Sectional Sample
#Os resultados dessa amostra estão similares aos resultados do artigo?
#Vamos falar um pouquinho sobre isso na mentoria :)