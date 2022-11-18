# Despesas feitas por candidatos no estado de SP no pleito municipal 2020 
### Este script foi criado por [Jonas Arjona](https://www.linkedin.com/in/jonas-arjona-639497190/) como parte de um projeto de capacitação em linguagem R promovido pelo Núcleo de Estudos Integrados em Democracia, Comunicação e Sociedade (DECOS), liderado pela professora Dra. [Gleidylucy Oliveira da Silva](http://lattes.cnpq.br/1871892858720738). 

Neste script desenvolvemos uma breve análise dos dados de despesas contratadas pelos candidatos do estado de São Paulo no pleito municipal de 2020. A escolha desse recorte se dá por conta da menor relevância dada à disputa municipal se comparada à nacional. Além disso, São Paulo foi escolhido por ser o estado com o maior colégio eleitoral do país. As conclusões aqui elaboradas não irão necessariamente refletir a dinâmica nacional, objetivamos apenas treinar etapas próprias de um projeto em análise de dados.

## Primeiro passo: a pergunta de pesquisa
Antes de mais nada, é preciso deixar bem evidente qual o objetivo do projeto de análise a ser desenvolvido, em especial qual é a pergunta de pesquisa e/ou problema de negócio (para análises voltadas à iniciativa privada) que o norteia. Isso porque os esforços subsequentes partirão da premissa que nossos dados coletados e métodos escolhidos são condizentes com nossas expectativas e pergunta.
Logo, se por exemplo eu pretendo compreender os efeitos de determinadas variáveis sociodemográficas num fenômeno social, eu preciso saber:
* Quais são as melhores e mais confiáveis fontes tanto para minha variável dependente quanto as independentes;
* Eventuais dilemas que as escolhas feitas terão nos resultados (por exemplo, os efeitos de maneiras distintas de registrar um aspecto sociodemográfico);
* Lacunas na análise a serem reconhecidas e/ou exploradas posteriormente.

Assim, precisamos definir de maneira objetiva uma pergunta de pesquisa principal e auxiliares, estas últimas atuando como etapas para responder o questionamento principal. Para nós aqui, nossa pergunta principal foi:

**Há algum evidente padrão nos dados de despesas eleitorais contratadas pelas candidaturas do estado de SP em 2020?**

E como perguntas auxiliares:
* Há quantas despesas declaradas no geral? E por sexo/cargo/partido?
* Quanto foi gasto por cada um dos possíveis valores dessas variáveis?
* Quantas e quais são os totais gastos por tipo de despesa? É possível melhorar essa visualização?
* Há padrões entre as variáveis citadas acima e gastos?

## Segundo passo: os dados e suas fontes
Como estaremos usando de dados eleitorais, podemos coletá-los do site do Tribunal Superior Eleitoral(TSE) [neste link](https://dadosabertos.tse.jus.br/dataset/?q=2020), que corresponde à página para o pleito de 2020.
Precisávamos simultaneamente de dados de despesas e sociodemografia das candidaturas, dois grupos de variáveis separados. Por isso, empreenderemos numa união das fontes num só arquivo em formato ".csv".
Baixamos os arquivos originais no site do TSE: "Candidatos - 2020" para dados sociodemográficos; "Prestação de Contas Eleitorais - 2020" para dados das despesas. Extraímos os arquivos ".rar" para obtermos os dados em ".csv" para cada estado. Por fim, separamos os arquivos do estado de SP. Junto dos arquivos compactados, também temos o arquivo "LEIAME.pdf", com instruções sobre a estrutura dos dados.

Partimos então para o RStudio, onde selecionamos nosso "working directory" e carregamos os pacotes úteis. Além disso, como lidamos com cifras bastante elevadas, rodamos a função **options** com o argumento **scipen = 999** para desabilitar a notação científica automática do RStudio.

```r
#WORKING DIRECTORY 
setwd([DIRETÓRIO DE SUA ESCOLHA])
getwd()

#PACOTES
library(tidyverse)
library(ggplot2)

#OPTIONS
options(scipen = 999)
```

Abrimos os dois arquivos **.csv** no RStudio e então os associamos a dois objetos: **desp_2020** e **cand_2020**.

```r
#DADOS
cand_2020 <- read.csv(file = "consulta_cand_2020_SP.csv",header = T, sep = ";", 
                      fileEncoding = "latin1",dec = ",")

desp_2020 <- read.csv(file = "despesas_contratadas_candidatos_2020_SP.csv",header = T, sep = ";", 
                      fileEncoding = "latin1",dec = ",")
```
Em seguida, selecionamos apenas candidaturas deferidas de fato e as variáveis relevantes de **cand_2020**. Abaixo temos o nome das variáveis e a distribuição das observações na variável **DS_DETALHE_SITUACAO_CAND** que registra deferência.

```r
colnames(cand_2020) 
table(desp_2020$DS_DETALHE_SITUACAO_CAND)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/colnames-table-cand.png "imagem 1")

Muitas variáveis redundantes e muitas formas de atestar indeferência. Nas próximas linhas, reduzimos as variáveis e filtramos pela deferência.

```r
#LIMPEZA
var_rem <- c(1:14,17:42,45,56:71)
sel_sit <- c("DEFERIDO","DEFERIDO COM RECURSO")

cand_2020<- cand_2020 %>% 
  filter(DS_DETALHE_SITUACAO_CAND %in% sel_sit) %>% 
  select(-all_of(var_rem))
```

Por fim, executamos um "inner_join" entre os dois DFs usando como primary key **SQ_CANDIDATO**. Além disso, removemos mais variáveis irrelevantes e redundantes. Nosso **data.frame** final será chamado de **desp_cand_2020**.

```r
#CRIAÇÃO DO DF FINAL
desp_cand_2020 <- inner_join(desp_2020,cand_2020,by = "SQ_CANDIDATO") %>% 
  select(1:21,55:66,22:53)
```

Temos abaixo o **head** do nosso novo **data.frame** e algumas de suas variáveis.

```r
head(desp_cand_2020)
```
![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/head.png "imagem 2")

Antes de finalizarmos esse passo, checamos uma das implicações do nosso join. O **inner_join** resultou num **data.frame** que só possui as observações que estão presentes nos dois objetos unidos. Logo, se tivéssemos um candidato sem despesas ou uma despesa sem candidato (algo impossível porque a despesa registrada é sempre feita por uma candidatura), ambos estariam excluídos de **desp_cand_2020**. Isso por si só já pode ser útil para uma análise já que podemos encontrar padrões de não-gasto, isto é, de menor investimento e/ou até desamparo por parte dos partidos com algumas candidaturas específicas.

Então distinguimos os **SQ_CANDIDATO** de **cand_2020** que não se encontram em **desp_2020** dos que se encontram, ou seja, candidaturas registradas e deferidas, mas que não possuem gastos. Em seguida criamos um novo **data.frame** com uma variável que distingue candidaturas com gastos daquelas sem gastos.

```r
n_gasto <- cand_2020 %>% 
  mutate(FEZ_GASTO = ifelse(SQ_CANDIDATO %in% desp_2020$SQ_CANDIDATO,
                            "SIM","NÃO"))

n_gasto %>% 
  group_by(DS_CARGO,FEZ_GASTO) %>% 
  summarise(contagem = n())
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/n_gasto.png "imagem 3")

Muito mais candidaturas à vereador não fizeram gastos do que aquelas para prefeito, tanto em números absolutos quanto em proporções. Vice-prefeitos não são votados mas precisam ser registrados e ficam sob a campanha principal do prefeito, o que explica suas discrepâncias. Outros cruzamentos, como sexo, partido, idade e cor/raça poderiam eventualmente mostrar outros achados. Não tomamos este rumo aqui.

## Terceiro passo: um mergulho preliminar
Após essas etapas, demos propriamente início à nossa análise. Este tópico em específico foi usado para conhecermos os nossos dados e eventuais relações que eles podem demonstrar. Para isso, usamos funções em R que geram medidas-resumo, descrição da estrutura dos dados e **pipes** de agrupamento e **summarise**. Além disso, plotamos alguns gráficos, mesmo que pouco intuitivos e/ou visualmente objetivos. 
Evidemente os resultados aqui descritos não se encontrariam num relatório final de análise de dados a ser entregue às partes interessadas, já que eles possuem como utilidade informar o analista da forma mais simples e rápida possível. É na produção dos gráficos ao final da análise que despendemos mais tempo em questões estéticas.

Inicialmente, executamos a função **str**, que descreve o tipo de dados que cada variável tem e alguns exemplos de valores. O mais importante é constatar que a variável de valor da despesa estava em formato numérico e que não existam erros de reconhecimento de tipo de variável. Ambos esses tópicos não apresentaram problemas nos nossos dados.

```r
str(desp_cand_2020)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/str.png "imagem 4")

Em seguida, solicitamos medidas-resumo da nossa variável de interesse, **VR_DESPESA_CONTRATADA**, o valor em reais da despesa contratada.

```r
summary(desp_cand_2020$VR_DESPESA_CONTRATADA)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/summary.png "imagem 5")

Vemos como são díspares as despesas registradas, alguns na casa dos centavos, outras na dos milhares e algumas poucas na casa dos milhões.

Plotamos então essas medidas, distinguindo as observações por partido, sexo e cargo. Outras variáveis poderiam com toda certeza serem usadas aqui, apenas demos preferência para esses citadas. Também plotamos alguns gráficos-esboço, apenas para mostrar a relação estabelecida e ignorando etapas de renomeação de títulos, subtítulos e eixos.

#### Partido

```r
#MEDIDAS-RESUMO
desp_cand_2020 %>% 
  group_by(SG_PARTIDO) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA),
            min = min(VR_DESPESA_CONTRATADA),
            max = max(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma)) %>% 
  print(n = 32)

```
![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_part.png "imagem 6")


```r
#GRÁFICO
desp_cand_2020 %>% 
  group_by(SG_PARTIDO) %>% 
  summarise(contagem = n()) %>% 
  ggplot()+
  geom_col(aes(x=reorder(SG_PARTIDO,contagem,sum),y=contagem))+
  coord_flip()
```
![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_contagem_part.png "imagem 7")

Como era de se esperar, os partidos mais bem estabelecidos (mais antigos, consistentes, presentes na cena pública, com mais parlamentares eleitos) são aqueles que demonstram as maiores cifras totais bem como as maiores quantidades de despesas. Curiosamente, eles nem sempre apresentam as maiores médias, perdendo em alguns casos para outros partidos menores como o PSOL e PSL. Isso porque o cálculo de média é altamente influenciável pelo número total de observações de cada partido e por despesas **outliers**. Os valores mínimos em todos esses casos são encargos financeiros, como juros ou taxas bancários.

#### Sexo
```r
#MEDIDAS-RESUMO
desp_cand_2020 %>% 
  group_by(DS_GENERO) %>% 
  summarise(contagem = n()) %>% 
  ggplot()+
  geom_col(aes(x=reorder(DS_GENERO,contagem,sum),y=contagem))

```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_gen.png "imagem 8")

```r
#GRÁFICO
desp_cand_2020 %>% 
  group_by(DS_GENERO) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA),
            min = min(VR_DESPESA_CONTRATADA),
            max = max(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_contagem_gen.png "imagem 9")

Nada de novo sob o sol. Candidaturas femininas gastam menos em número de despesa e valor total acumulado. Por outro lado, a história que as médias nos contam é a de equidade de gênero entre candidaturas, já que elas estão muito próximas. Porém isso é falacioso, novamente, porque o valor final da média é altamente influenciado pelo número de observações. Se observarmos inclusive as proporções entre quantidade de despesas entre os sexos e seus valores totais, encontraremos um padrão: essas cifras para os casos femininos gravitam na casa dos 30%.

Esse valor não é por acaso. Segundo o artigo 10 da Lei das Eleições ([9.504/1997](http://www.planalto.gov.br/ccivil_03/leis/l9504.htm)), fazendo referência às cotas, cada partido deverá apresentar no mínimo 30% e no máximo 70% das suas candidaturas para cada um dos sexos. Logo, a escolha dos partidos pela inequidade aparece também nos nossos dados de despesas.

#### Cor/raça
```r
#MEDIDAS RESUMO
desp_cand_2020 %>% 
  group_by(DS_COR_RACA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA),
            min = min(VR_DESPESA_CONTRATADA),
            max = max(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_raça.png "imagem 10")

```r
#GRÁFICO
desp_cand_2020 %>% 
  group_by(DS_COR_RACA) %>% 
  summarise(contagem = n()) %>% 
  ggplot()+
  geom_col(aes(x=reorder(DS_COR_RACA,contagem,sum),y=contagem))
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_contagem_raça.png "imagem 11")

Candidaturas brancas são as que mais fazem despesas, justamente porque são as mais presentes nos nossos dados. É importante ressaltar que esses são dados de um estado da região sudeste. Estados do norte ou nordeste apresentarão outros padrões. Abaixo temos a quantidade de candidatos para cada uma das categorias de cor/raça.

```r
desp_cand_2020 %>%
  select(SQ_CANDIDATO,DS_COR_RACA) %>% 
  unique() %>% 
  group_by(DS_COR_RACA) %>% 
  summarise(contagem = n())
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_raça_unique.png "imagem 12")

#### Cargo

Por conta do **inner_join** usado anteriormente, a variável **DS_CARGO** sofreu uma leve alteração no nome. Ela precisou ser renomeada antes de prosseguirmos. Além disso, removemos do gráfico as candidaturas para vice-prefeito, já que seu porte é tão pequeno que geram visualiações pouco intuitivas e distorcidas.

```r
desp_cand_2020 <- rename(desp_cand_2020, DS_CARGO = DS_CARGO.x)

#MEDIDAS-RESUMO
desp_cand_2020 %>% 
  group_by(DS_CARGO) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA),
            min = min(VR_DESPESA_CONTRATADA),
            max = max(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_cargo.png "imagem 13")

```r
#GRÁFICO
desp_cand_2020 %>% 
  filter(DS_CARGO != "Vice-prefeito") %>%
  group_by(DS_CARGO) %>% 
  summarise(contagem = n()) %>% 
  arrange(desc(contagem)) %>% 
  ggplot()+
  geom_col(aes(x=DS_CARGO,y=contagem))
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_contagem_cargo.png "imagem 14")

Aqui encontramos relações interessantes. Por um lado, há mais despesas declaradas por vereadores, pelo simples fato de que há mais candidaturas para vereador do que para prefeito.

```r
desp_cand_2020 %>%
  select(SQ_CANDIDATO,DS_CARGO) %>% 
  unique() %>% 
  group_by(DS_CARGO) %>% 
  summarise(contagem = n())
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_cargo_unique.png "imagem 15")

Por outro lado, as somas totais são inegáveis: candidatos para prefeito gastam bem mais que vereadores. Isso fica evidente pelas médias de cada cargo. Há também valores máximos muito altos e próximos para esses dois cargos, algo particularmente curioso dada as médias diferentes. Quem são os candidatos com as despesas únicas de maior valor?

```r
desp_cand_2020 %>%
  filter(VR_DESPESA_CONTRATADA >= 1000000) %>% 
  select(DS_CARGO,NM_CANDIDATO,NM_UE,
         SG_PARTIDO,DS_ORIGEM_DESPESA,
         VR_DESPESA_CONTRATADA)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/desp_1kk+.png "imagem 16")

Na imagem vemos figuras que já esperávamos: Tatto e Bruno Covas, candidatos para prefeito da cidade de São Paulo. Além deles, o candidato Dario Jorge (Campinas) também aparece, além daquele vereador com a despesa de 1 milhão e 700 mil reais: Vitor Abou Anni (São Paulo), do PSL, que declarou tal despesa se tratar de uma doação ao partido. Um caso tão discrepante num partido que não aqueles mais estabelecidos (como o PSDB) mereceria esforços analíticos mais incisivos.

Poderíamos prosseguir testando mais e mais relações, nosso banco possui outras variáveis também. Uma outra análise interessante seria plotar mapas com os estabelecimentos contratados nas despesas, com círculo mais ou menores indicando a quantidade e/ou a soma total gasta nesses lugares. Isso nos daria insights sobre a movimentação financeira que uma campanha gera no município e/ou estado. Também não seguimos esse caminho aqui.

## Quarto passo: preparação à análise

> Daqui em diante iremos gerar alguns gráficos como antes, mas agora zelando pelo aspecto estético. Logo, os códigos ficarão maiores. Além disso, optamos por criar alguns objetos que serão usados para armazenar configurações de estética. Abaixo temos um deles, o **tema**.
```r
tema <- theme(title = element_text(size = 20),
              axis.title = element_text(size = 17.5),
              axis.text = element_text(size = 13),
              legend.title = element_text(size = 17.5),
              legend.text = element_text(size = 13))
```

Optamos por destrinchar a variável **DS_ORIGEM_DESPESA**, usada pelos declarantes para informar o objetivo da despesa. Trata-se de uma variável com uma quantidade fixa de possível valores, como um **factor** em linguagem R. As candidaturas precisam avaliar os gastos e escolher a melhor categoria para eles. Abaixo temos uma lista de todas as possíveis respostas e a quantidade de cada uma.

```r
desp_cand_2020 %>% 
  group_by(DS_ORIGEM_DESPESA) %>% 
  summarise(contagem = n()) %>% 
  arrange(desc(contagem)) %>% 
  print(n = 40)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/ds_origem_despesa.png "imagem 17")

Temos também a soma total por categoria e o valor médio de cada uma delas.

```r
desp_cand_2020 %>% 
  group_by(DS_ORIGEM_DESPESA) %>% 
  summarise(soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/ds_origem_despesa_resumo.png "imagem 18")

O que ambas as imagens mostram é que despesas associadas à campanha de feição mais tradicional, com material impresso, programas de rádio e televisão, campanha na rua e afins, são tanto as mais presentes nas declarações como as que somam as maiores cifras. A média, por outro lado, mostra certa disparidade desse padrão, novamente por conta dos efeitos que o número de observações e **outliers** têm sobre o cálculo. 

Assim, é possível afirmar que há despesas que lideram o ranking da imagem acima pela quantidade e soma total (mesmo com baixas médias), enquanto outras lideram pela média (mesmo com baixas quantidades e soma total). Para exemplificar, comparemos "Publicidade por material impresso" (linha 1) e "Despesas com impulsionamento de conteúdo" (linha 6). A relação as somas totais é inversa aquela entre as médias.

Apesar dos insights, a dispersão dos valores (mínimos e máximos), a quantidade de despesas e a variedade de possíveis valores dificulta a criação de boas visualizações. Abaixo temos um gráfico de **boxplot** para cada categoria de despesa, com eixo X em escala de log 10 (para reduzir a dispersão dos valores) e os valores em mil reais.

```r
desp_cand_2020 %>% 
  group_by(SQ_DESPESA,DS_ORIGEM_DESPESA) %>%
  summarise(gasto = VR_DESPESA_CONTRATADA/1000) %>% 
  ggplot()+
  geom_boxplot(aes(x=reorder(DS_ORIGEM_DESPESA,
                             gasto,
                             sum),
                   y=gasto))+
  scale_y_log10()+
  labs(title = "Valor gasto por tipo de despesa original",
       y = "Valor(em mil reais)",
       x = "Tipo de despesa original")+
  coord_flip()+
  tema
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/boxplot_ds_origem_despesa.png "imagem 19")

Perceba como mesmo assim ainda temos muito **outliers**(pontos pretos) e longas sombras(linhas horizontais partindo das extremidades das caixas). Além disso, ficou evidente a dispersão do valor dos tipos de despesa.

Para facilitar todo esse processo, optamos por recategorizar as despesas em grupos comuns com base num esforço empírico desenvolvido anteriormente ([CERVI; VASCONCELLOS; CAVASSANA, 2021](https://www.researchgate.net/publication/348481707_2021_cap_campanhasdigitaisINCTdd)). O estudo foi feito para os candidatos a deputado federal em 2018, mas sua tipologia ainda nos é útil. Apenas há três alterações a serem feitas com base em mudanças feitas pelo TSE na descrição das despesas:

* Foi criado pelo órgão a categoria "Despesa com geradores de energia" e nós colocamos ela dentro de "Infraestrutura";
* Foram criadas pelo TSE as categoriais "Serviços advocatícios" e "Serviços contábeis" e nós colocamos ambas em "Despesas administrativas";
* Modificou-se a categorização de "Despesas com Hospedagem", erroneamente colocada pelos autores em duas categorias, confundindo hospedagem em hotel com hospedagem de websites.

As despesas foram aglutinadas com base no papel que cada uma exerce no esforço de campanha: mão-de-obra, infraestrutura, administração, comunicação tradicional, comunicação digital, diversas não especificadas. Logo, as 40 categorias dos nossos dados foram reduzidas para apenas 6. Abaixo a relação entre categoria original e categoria nova.

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/categorias_gastos.png  "imagem 20")

Fizemos a recategorização usando de um **case_when** dentro de um **mutate**. Para tanto, primeiro criamos uma série de objetos para conter os possíveis valores de **DS_ORIGEM_DESPESA**. Em seguida, rodamos o **mutate** com o **case_when** dentro que fará a criação da nossa nova variável **TIPO_DESPESA**. Solicitamos então um **table**

```r
com_dig<-c("Criação e inclusão de páginas na internet",
           "Despesa com Impulsionamento de Conteúdos",
           "Despesas com Hospedagem",
           "Taxa de Administração de Financiamento Coletivo")

com_tradicional<-c("Atividades de militância e mobilização de rua",
                   "Comícios",
                   "Produção de jingles, vinhetas e slogans",
                   "Produção de programas de rádio, televisão ou vídeo",
                   "Publicidade por adesivos",
                   "Publicidade por carros de som",
                   "Publicidade por jornais e revistas",
                   "Publicidade por materiais impressos")

pessoal<-c("Despesas com pessoal",
           "Serviços prestados por terceiros",
           "Serviços próprios prestados por terceiros")

administrativa<-c("Alimentação",
                  "Combustíveis e lubrificantes",
                  "Correspondências e despesas postais",
                  "Despesas com transporte ou deslocamento",
                  "Doações financeiras a outros candidatos/partidos",
                  "Encargos financeiros, taxas bancárias e/ou op. cartão de crédito",
                  "Encargos sociais",
                  "Eventos de promoção da candidatura",
                  "Impostos, contribuições e taxas",
                  "Materiais de expediente",
                  "Multas eleitorais",
                  "Passagem Aérea",
                  "Pesquisas ou testes eleitorais",
                  "Reembolsos de gastos realizados por eleitores",
                  "Serviços advocatícios",
                  "Serviços contábeis")

infra<-c("Telefone",
         "Água",
         "Aquisição/Doação de bens móveis ou imóveis",
         "Cessão ou locação de veículos",
         "Energia elétrica",
         "Locação/cessão de bens imóveis",
         "Locação/cessão de bens móveis (exceto veículos)",
         "Pré-instalação física de comitê de campanha",
         "Despesa com geradores de energia")

div<-"Diversas a especificar"

desp_cand_2020 <- desp_cand_2020 %>% 
  mutate(TIPO_DESPESA=case_when(
    DS_ORIGEM_DESPESA %in% com_dig~"Comunicação digital",
    DS_ORIGEM_DESPESA %in% com_tradicional~"Comunicação tradicional",
    DS_ORIGEM_DESPESA %in% pessoal~"Despesas com pessoal",
    DS_ORIGEM_DESPESA %in% administrativa~"Despesas administrativas",
    DS_ORIGEM_DESPESA %in% infra~"Infraestrutura",
    DS_ORIGEM_DESPESA %in% div~"Diversas a especificar",
    TRUE~"CHECAR"))

table(desp_cand_2020$TIPO_DESPESA)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/table_tipo_despesa.png "imagem 21")

Temos também a soma total das despeas por **TIPO_DESPESA**. Vemos a total predominância das despesas com comunicação tradicional, com pessoal e administrativas. Na retaguarda, temos gastos com comunicação digital, infraestrutura e diversas a especificar.

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/soma_tipo_despesa.png "imagem 22")

Seguem alguns gráficos semelhantes aos gerados antes, plotando quantidade de despesas e soma total para cada uma das categorias de **TIPO_DESPESA**.

```r
desp_cand_2020 %>% 
  group_by(TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>%
  ggplot()+
  geom_col(aes(x = reorder(TIPO_DESPESA,contagem,sum),y = contagem))+
  coord_flip()+
  labs(title = "Número de despesas declaradas por tipo",
       y = "Quantidade de despesas",
       x = "Tipo de despesa")+
  tema
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_contagem_tipo_despesa.png "imagem 23")

```r
desp_cand_2020 %>% 
  group_by(TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>%
  ggplot()+
  geom_col(aes(x = TIPO_DESPESA, y= soma))+
  coord_flip()+
  labs(title = "Valor total de despesas declaradas por tipo",
       y = "Valor total",
       x = "Tipo de despesa")+
  tema
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_soma_tipo_despesa.png "imagem 24")

Ainda preparando os dados, usamos mais dois **mutates**. O primeiro criou faixas-etárias, gerando sete grupos etários. O segundo aglutinou os valores da variável **DS_COR_RACA** em dois grupos: brancos e não brancos. Isso porque outros valores que não "branco" apresentaram frequências muito baixas. Seguem os dois códigos e os dois **summarise**.

```r
desp_cand_2020 <- desp_cand_2020 %>% 
  mutate(FAIXA_ETARIA = case_when(
    NR_IDADE_DATA_POSSE %in% 18:25 ~ "18-25",
    NR_IDADE_DATA_POSSE %in% 26:35 ~ "26-35",
    NR_IDADE_DATA_POSSE %in% 36:45 ~ "36-45",
    NR_IDADE_DATA_POSSE %in% 46:55 ~ "46-55",
    NR_IDADE_DATA_POSSE %in% 56:65 ~ "56-65",
    NR_IDADE_DATA_POSSE %in% 66:75 ~"66-75",
    NR_IDADE_DATA_POSSE >=76 ~ "76+",
    TRUE ~ "CHECAR"
  ))

table(desp_cand_2020$FAIXA_ETARIA)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_faixa.png "imagem 25")

Observamos uma predominância no número de despesas declaradas pelas candidaturas entre 36 anos e 65. Abaixo temos a relação de quantidades candidaturas temos no nosso **data.frame** por faixa. De fato observamos que é nesse intervalo de idade onde estão a maioria das candidaturas do nosso banco.

```r
desp_cand_2020 %>% 
  select(SQ_CANDIDATO,FAIXA_ETARIA) %>% 
  unique() %>% 
  group_by(FAIXA_ETARIA) %>% 
  summarise(contagem = n()) %>% 
  arrange(desc(contagem))
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_faixa_unique.png "imagem 26")

```r
branca <- "BRANCA"
nao_branca <- c("AMARELA","INDÍGENA","PARDA","PRETA","NÃO INFORMADO")

desp_cand_2020 <- desp_cand_2020 %>% 
  mutate(DS_COR_RACA2 = case_when(
    DS_COR_RACA %in% branca ~ "BRANCA",
    DS_COR_RACA %in% nao_branca ~ "NÃO-BRANCA"
  ))

table(desp_cand_2020$DS_COR_RACA2)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_raça2.png "imagem 27")

Observamos a predominância de despesas declaradas por candidaturas brancas, mesmo após nosso **mutate** unir várias categorias. Abaixo temos a relação de quantidades candidaturas temos no nosso **data.frame** por **cor/raça2**. De fato essa é a categoria mais presente no nosso banco.

```r
desp_cand_2020 %>% 
  select(SQ_CANDIDATO,DS_COR_RACA2) %>% 
  unique() %>% 
  group_by(DS_COR_RACA2) %>% 
  summarise(contagem = n()) %>% 
  arrange(desc(contagem))
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/contagem_raça2_unique.png "imagem 28")

## Quinto passo: cruzamentos e análise

Feito todos esses processos de preparação, seguimos nossos esforços a fim de responde a pergunta de pesquisa. Já tínhamos encontrado algumas possíveis respostas só no mergulho exploratório e na preparação das variáveis, mas daqui em diante nos aprofundamos nos cruzamentos e na elaboração de interpretações aos achados.

> Da mesma forma que fizemos com o objeto **tema** criado antes, criamos outro objeto para configurações estéticas que armazenará a escala personalizada de cores para **TIPO_DESPESA**.  
```r
cores <- scale_fill_manual(values = c("#f6b395","#f6e095",
                                      "#b9f695","#acf5ed",
                                      "#acbff5","#eaacf5"))
```

#### Sexo
O primeiro cruzamento é entre **TIPO_DESPESA** e **DS_GENERO**. Já tínhamos visto que a disparidade entre candidaturas femininas e masculinas persiste na quantidade e soma total das despesas, numa proporção de 1:3.

```r
desp_cand_2020 %>% 
  group_by(DS_GENERO,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(TIPO_DESPESA,soma)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/tipo_gen.png "imagem 29")

```r
desp_cand_2020 %>% 
  group_by(DS_GENERO,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>%
  ggplot()+
  geom_col(aes(x=DS_GENERO,
               y=soma,
               fill=TIPO_DESPESA,soma,sum),
           position = "dodge")+
  labs(title = "Total gasto por sexo e tipo de despesa",
       x = "Sexo",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_tipo_gen.png "imagem 30")

Observamos uma replicação da disparidade em todos os tipos de despesa: candidaturas femininas estão sempre atrás nos gastos, independente de que tipo de gastos estamos falando. Encontramos uma reprodução do padrão mais geral da predominância da comunicação tradicional sobre as outras, com as disparidades sendo mais atenuadas nas candidaturas femininas, muito provavelmente pela própria limitação numérica delas.

#### Cargo (sem vice-prefeitos)

Anteriormente descobrimos que apesar de os vereadores declararem mais despesas, a soma total é superior para prefeitos e a média indica ser a alta quantidade de candidaturas para aquelas responsável por isso. Removemos os vice-prefeitos, como mencionado anteriormente.

```r
desp_cand_2020 %>% 
  filter(DS_CARGO != "Vice-prefeito") %>% 
  group_by(DS_CARGO,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(TIPO_DESPESA,soma)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/tipo_cargo.png "imagem 31")

```r
desp_cand_2020 %>% 
  filter(DS_CARGO != "Vice-prefeito") %>% 
  group_by(DS_CARGO,TIPO_DESPESA) %>% 
  summarise(soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  ggplot()+
  geom_col(aes(x=DS_CARGO,
               y=soma,
               fill = TIPO_DESPESA),
           position = "dodge")+
  labs(title = "Total gasto por cargo e tipo de despesa",
       subtitle = "Excluídos vice-prefeitos",
       x = "Cargo",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_tipo_cargo.png "imagem 32")

A disparidade entre quantidade e soma total persiste. Para alguns dos tipos de despesa, a diferente na quantidade é menor ou quase igual (como despesas com pessoal, tradicional e infraestrutura). Em outras (comunicação digital e administrativas) ela é muito acentuada. Na soma total, porém, a relação se inverte em alguns casos (comunicação tradicional e administrativa) e em outras as cifras se aproximam muito (comunicação digital e com pessoal). Vereadores parecem optar por despesas mais baratas em grande quantidade, enquanto prefeitos preferem investir pesado em grandes serviços.

#### Partido

Esperávamos encontrar a saliência dos grandes partidos, com grandes cifras e alta quantidade de despesas. Sabendo da diferença entre os cargos, optamos por criar três gráficos: todos os cargos, apenas prefeitos e apenas vereadores.
Além disso, como temos muitos partidos, optamos por expor os dados de uma forma diferente. Primeiro geramos os valores com um **summarise** para em seguida converter a visualização de um formato longo (várias linhas e poucas colunas) para formato largo (poucas linhas e várias colunas). Fazemos isso usando a função **pivot_wider**. A desvantagem disso é que perdemos a possibilidade de comparar quantidade e soma numa só visualização. Os gráficos não sofrem alterações. 

```r
#TODOS OS CARGOS
desp_cand_2020 %>% 
  group_by(SG_PARTIDO,TIPO_DESPESA) %>% 
  summarise(soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  pivot_wider(id_cols = 1,
              names_from = TIPO_DESPESA,
              values_from = soma) %>%
              print(n = 32)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/soma_tipo_part.png "imagem 33")

Temos dois **NA**, referentes aos gastos da UP em diversas a especificar e do PSTU com infraestrutura. Eles nos indicam ausência de gastos para esses partidos nesses categorias.

```r
#TODOS OS CARGOS
desp_cand_2020 %>% 
  group_by(SG_PARTIDO,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  ggplot()+
  geom_col(aes(x=reorder(SG_PARTIDO,soma,sum),
               y=soma,
               fill = TIPO_DESPESA))+
  coord_flip()+
  labs(title = "Total gasto por partido e tipo de despesa",
       x = "Partido",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_tipo_part_tds.png "imagem 34")

```r
#PREFEITOS
desp_cand_2020 %>% 
  filter(DS_CARGO == "Prefeito") %>% 
  group_by(SG_PARTIDO,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  ggplot()+
  geom_col(aes(x=reorder(SG_PARTIDO,soma,sum),
               y=soma,
               fill = TIPO_DESPESA))+
  coord_flip()+
  labs(title = "Total gasto por partido e tipo de despesa",
       subtitle = "Apenas candidatos a prefeito",
       x = "Partido",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_tipo_part_pref.png "imagem 35")

```r
desp_cand_2020 %>%
  filter(DS_CARGO == "Vereador") %>% 
  group_by(SG_PARTIDO,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  ggplot()+
  geom_col(aes(x=reorder(SG_PARTIDO,soma,sum),
               y=soma,
               fill = TIPO_DESPESA))+
  coord_flip()+
  labs(title = "Total gasto por partido e tipo de despesa",
       subtitle = "Apenas candidatos a vereador",
       x = "Partido",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_tipo_part_ver.png "imagem 35")

É quando selecionamos apenas vereadores que encontramos novas relações. No geral, persiste o predomínio das grandes legendas e da comunicação tradicional, mas as distâncias entre os partidos e entre os tipos de despesa é reduzida, bem como o ranking dos que mais gastam. Se antes tínhamos o PT abaixo do PSDB e uma ampla margem de vantagem para este, aqui são os partidos do centrão que seguem o líder, quase equiparando as somas totais. 
Logo, podemos reafirmar que, comparando com os dados de despesa para prefeito, a disputa para vereador é relativamente mais equitativa. Tanto nas somas totais quanto pelos tipos de despesa.

#### Faixa etária

Esperávamos encontrar novamente aqueles grupos etários centrais como predominantes. Poderíamos encontrar diferenças entre os tipos de despesa caso aceitássemos que as candidaturas possuem algum lastro entre idade e grupos representados. Em outras palavras, candidaturas mais jovens poderiam representar contingentes da sociedade correspondentes, algo que eventualmente afetaria preferências por estratégias de campanha mais digitais. Nada disso, porém, é certo mas sim hipóteses. 

```r
desp_cand_2020 %>% 
  group_by(FAIXA_ETARIA,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(TIPO_DESPESA,soma) %>% 
  pivot_wider(id_cols = 1,
              names_from = TIPO_DESPESA,
              values_from = soma)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/soma_tipo_faixa.png "imagem 36")

```r
desp_cand_2020 %>% 
  group_by(FAIXA_ETARIA,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>%
  ggplot()+
  geom_col(aes(x=FAIXA_ETARIA,
               y=soma,
               fill=TIPO_DESPESA),
           position = "dodge")+
  labs(title = "Total gasto por faixa etária e tipo de despesa",
       x = "Faixa etária",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_tipo_faixa.png "imagem 37")

O que achamos é mais do mesmo padrão da hegemonia da comunicação tradicional e uma rejeição da hipótese das diferenças entre gupos etários quanto às suas estratégia.

#### Cor/raça binária

Esperávamos que as candidaturas brancas dominassem as próximas visualizações, já que são as mais presentes, que fazem mais gastos e totalizam maiores valores. Da mesma forma que com cargo e faixa etária, poderíamos questionar se há diferenças entre estratégias e escolhas de tipos de despesa.

```r
desp_cand_2020 %>% 
  group_by(DS_COR_RACA2,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(TIPO_DESPESA,soma)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/tipo_raça2.png "imagem 38")

```r
desp_cand_2020 %>% 
  group_by(DS_COR_RACA2,TIPO_DESPESA) %>% 
  summarise(soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  ggplot()+
  geom_col(aes(x=DS_COR_RACA2,
               y=soma,
               fill = TIPO_DESPESA),position = "dodge")+
  labs(title = "Total gasto por cor/raça e tipo de despesa",
       x = "Cor/raça",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/plot_tipo_raça2.png "imagem 39")

Repete-se o mesmo padrão de maior das despesas com comunicação tradicional, seguidas das despesas com pessoal e das despesas administrativas. Mesmo quando aglutinamos candidatos não-brancos, não encontramos o menor indício de que o padrão mais geral seria diferente.

## Sexto passo: conclusões, interpretações e encaminhamentos

Há claros padrões nos dados das despesas. O primeiro deles é a inegável presença de grupos privilegiados nas despesas com campanha. Apesar de não ser novidade, esses dados enfatizam que a disputa não atuou nunca de maneira a reparar inequidades. Se a ausência de minorias já era constatável pelo reduzido número de candidatos e menor ainda de eleitos, a dinâmica financeira só reafirma isso. São os homens brancos, mais velhos e dos grandes partidos aqueles que mais gastam, tanto em cifras quanto em número de declarações. Reproduzir essa análise em outros contextos poderia nos gerar outros resultados (principalmente quanto às relações envolvendo cor/raça).

O segundo padrão é o da escolha das estratégias. Não há diferenças muito significativas entre as proporções gastas nas categorias de **TIPO_DESPESA**. Isso pode nos indicar um lugar-comum dos esforços dos candidatos e/ou uma rigidez dos assessores de campanha dos partidos em fazer diferente. Não necessariamente podemos culpar estes ou os pleiteantes, já que nenhuma estratégia se provou 100% eficaz, o que favorece "o que sempre foi feito". Uma próxima etapa com certeza seria focar nas categorias internas a cada valor de **TIPO_DESPESA**. Lá poderíamos encontrar diferenças interessantes.

Dissertemos um pouco sobre esse segundo ponto. Um desdobramento interessante dele é a ausência de despesas com estratégias de campanha que usam das Tecnologias da Informação e Comunicação (TICs), a comunicação digital. Havendo pequiníssimas flutuações nas cifras conforme controlamos pelas variáveis que testamos, no geral não há diferenças maiores. Num outro momento poderíamos extrapolar e testar outras variáveis: porte do município, colégio eleitoral, IDH/PIB, composição etária, taxa de alfabetização, acesso às TICS, etc. Os cenários podem ser muitos.

Por conseguinte, com base nos achados obtidos, questionamos então onde estariam as TICs numa sociedade que de maneira recorrente repete a importância delas à comunicação política. Pelo visto até agora, elas não estão nos gastos declarados. E é justamente esse o problema. Temos três possíveis explicações para isso.

* O custo-benefício desses gastos pode ser mais vantajoso do que para outros tipos de despesa: isto é, gerar o mesmo "retorno" em votos porém custando menos. Logo, as baixas cifras poderiam indicar alta eficácia das TICs. Para atestar isso, entretanto, precisaríamos de métricas robustas para tal, que fizessem a conversão real gasto e votos para cada tipo de estratégia;
* As vantagens das TICs podem ser informais e/ou indeclaráveis: Isso significa que há um benefício obtido que é inapreensível nas declarações oficiais. Um exemplo seriam as redes de relações sociais (contatos) de grupos específicos, como por profissão, religião, localidade, hobbies, etc. O diálogo desenvolvido nessas redes, bem como o efeito dele na campanha eleitoral é de difícil compreensão;
* Caixa dois: como prática ilegal, o caixa dois não é declarável, mas bem pode ter influência no processo eleitoral. Grupos simpatizantes de um candidato podem pagar impulsionamentos ilegais e/ou publicações de caráter político-partidário em favorecimento ao pleiteante, deixando este isento de declarar tais despesas.

De qualquer forma, mesmo com dados públicos, é difícil avaliar o uso das TICs em campanhas políticas, seja de maneira formal ou informal, direta ou indireta. Falar da importância delas é muito diferente de elaborar métricas para apreensão dos seus efeitos.

Em casos como esse, enquanto os métodos quatitativos pensam seus passos para este dilema, uma boa dose de pesquisa qualitativa pode jogar luz na temática.
