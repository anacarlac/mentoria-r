# Exercício 3: Aplicando os verbos do dplyr
# Vamos usar os verbos do dplyr para manipular as variáveis da BFI!

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
#RESPOSTA: 

#2. Crie um banco de dados sem missings na variável de educação
#RESPOSTA: 

#3. Crie um banco de dados sem missings em qualquer variável
#RESPOSTA: 

#dplyr::select
#1. Crie um banco de dados com variáveis sociodemográficas apenas
#RESPOSTA: 

#2. Crie um banco de dados com variáveis de Amabilidade e SES apenas
#RESPOSTA: 

#3. Crie um banco de dados com variáveis de Conscienciosidade, SES e depois filtre para o banco para ter mulheres apenas
#RESPOSTA: 

#dplyr::arrange
#1. Crie um banco de dados com variáveis de Amabilidade e SES apenas e ordene-o crescentemente por idade
#RESPOSTA: 

#2. Crie um banco de dados com variáveis sociodemográficas apenas e ordene-o decrescentemente por idade
#RESPOSTA: 

#3. Crie um banco de dados com variáveis sociodemográficas apenas e ordene-o decrescentemente por idade e crescentemente pela variável gender
#RESPOSTA: 

#dplyr::mutate
#1.Crie um banco de dados que tenha o escore Z de cada variável de Amabilidade computado para cada participante
#RESPOSTA: 

#1.Repita o procedimento acima de escore Z para as variáveis de Conscienciosdade
#RESPOSTA: 

#dplyr::group_by+summarize
#1.Use o objeto criado de escore Z para variáveis de A, selecione apenas variáveis de A e gênero, e tire a média de cada variável por genero


#Em geral, homens tem escores maiores ou menos do que mulheres nas variáveis de A?
#RESPOSTA: 

#1.Use o objeto criado de escore Z para variáveis de C, selecione apenas variáveis de A e gênero, e tire a média de cada variável por genero

#Em geral, homens tem escores maiores ou menos do que mulheres nas variáveis de C?
#RESPOSTA: 

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


#Em quais domínios que as mulheres apresentaram uma pontuação média maior? Indique as médias e DPs.
#RESPOSTA: 

#Em quais domínios que os homens apresentaram uma pontuação média maior? Indique as médias e DPs.
#RESPOSTA:

#EXTRA
#Pense sobre o que isso significa e veja o artigo SOTO et al (2011): Age Differences in Personality Traits From 10 to 65: Big Five Domains and Facets in a Large Cross-Sectional Sample
#Os resultados dessa amostra estão similares aos resultados do artigo?
#Vamos falar um pouquinho sobre isso na mentoria :)