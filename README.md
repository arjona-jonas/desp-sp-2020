# Despesas feitas por candidatos no estado de SP no pleito municipal 2020 #
### Este script foi criado por [Jonas Arjona](https://www.linkedin.com/in/jonas-arjona-639497190/) como parte de um projeto de capacitação em linguagem R promovido pelo Núcleo de Estudos Integrados em Democracia, Comunicação e Sociedade (DECOS), da professora Dra. [Gleidylucy Oliveira da Silva](http://lattes.cnpq.br/1871892858720738). ###

Neste script desenvolveremos uma breve análise dos dados de despesas contratadas pelos candidatos do estado de São Paulo no pleito municipal de 2020. A escolha dessa recorte se dá por conta da menor relevância dada à disputa municipal se comparada à nacional. Além disso, São Paulo foi escolhido por ser o estado com o maior colégio eleitoral do país. As conclusões aqui elaboradas não irão necessariamente refletir a dinâmica nacional, objetivamos apenas treinar etapas próprias de um projeto em análise de dados.

## Primeiro passo: a pergunta de pesquisa
Antes de mais nada, é preciso deixar bem evidente qual o objetivo do projeto de análise a ser desenvolvido, em especial qual é a pergunta de pesquisa e/ou problema de negócio (para análises voltadas à iniciativa privada) que o norteia. Isso porque os esforços subsequentes partirão da premissa que nossos dados coletados e métodos escolhidos são condizentes com nossas expectativas.
Logo, se por exemplo eu pretendo compreender os efeitos de determinadas variáveis sociodemográficas num fenômeno social, eu preciso saber:
* Quais são as melhores e mais confiáveis fontes tanto para minha variável dependente quanto as independentes;
* Eventuais dilemas que as escolhas terão nos resultados (por exemplo, os efeitos de maneiras distintas de registrar um aspecto sociodemográfico);
* Lacunas na análise a serem reconhecidas e/ou exploradas posteriormente.

Assim, precisamos definir de maneira objetiva uma pergunta de pesquisa principal e auxiliares, estas últimas atuando como etapas para responder o questionamento principal. Para nossa pergunta principal temos:

**Há algum evidente padrão nos dados de despesas eleitorais contratadas pelas candidaturas do estado de SP em 2020?**

E como perguntas auxiliares:
* Há quantas despesas declaradas no geral? E por sexo/cargo/partido?
* Quanto foi gasto por cada um dos possíveis valores dessas variáveis?
* Quantas e quais são os totais gastos por tipo de despesa? É possível melhorar essa visualização?
* Há padrões entre as variáveis citadas acima e gastos?

## Segundo passo: os dados e suas fontes
Como estaremos usando de dados eleitorais, podemos coletá-los do site do Tribunal Superior Eleitoral(TSE) [neste link](https://dadosabertos.tse.jus.br/dataset/?q=2020), que corresponde à página para o pleito de 2020.
Precisamos simultaneamente de dados de despesas e sociodemografia das candidaturas, dois grupos de variáveis separados. Por isso, empreenderemos numa união das fontes num só arquivo em formato ".csv".
Baixaremos então os arquivos originais no site do TSE: "Candidatos - 2020" para dados sociodemográficos; "Prestação de Contas Eleitorais - 2020" para dados das despesas. Extrairemos os arquivos ".rar" para obtermos os dados em ".csv" para cada estado. Por fim, separaremos os arquivos do estado de SP. Junto dos arquivos compactados, também temos o arquivo "LEIAME.pdf", com instruções sobre a estrutura dos dados.

Partimos então para o RStudio, onde iremos selecionar nosso "working directory" e carregar os pacotes úteis.

```r
#WORKING DIRECTORY 
setwd([DIRETÓRIO DE SUA ESCOLHA])
getwd()

#PACOTES
library(tidyverse)
library(ggplot2)
```

Iremos abrir os dois arquivos **.csv** no RStudio e associá-los a dois objetos: **desp_2020** e **cand_2020**.

```r
#DADOS
desp_2020<- read.csv(file = "desp_sp_2020.csv",header = T, sep = ";", 
                 fileEncoding = "latin1",dec = ",")
cand_2020<- read.csv(file = "cand_sp_2020.csv",header = T, sep = ";", 
                 fileEncoding = "latin1",dec = ",")
```
Em seguida, iremos selecionar apenas candidaturas deferidas de fato e as variáveis relevantes de **cand_2020**. Abaixo temos o nome das variáveis e a distribuição das observações na variável **DS_DETALHE_SITUACAO_CAND** que registra deferência.

```r
colnames(cand_2020) 
table(desp_2020$DS_DETALHE_SITUACAO_CAND)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/colnames-table-cand.png "imagem 1")


Muitas variáveis redundantes e muitas formas de atestar indeferência. Nas próximas linhas, reduzidos as variáveis e filtramos pela deferência.

```r
#LIMPEZA
var_rem <- c(1:14,17:42,45,56:71)
sel_sit <- c("DEFERIDO","DEFERIDO COM RECURSO")

cand_2020<- cand_2020 %>% 
  filter(DS_DETALHE_SITUACAO_CAND %in% sel_sit) %>% 
  select(-all_of(var_rem))
```

Por fim, faremos um "inner_join" entre os dois DFs usando como primary key **SQ_CANDIDATO**. Além disso, removeremos mais variáveis irrelevantes e redundantes. Nosso **data.frame** final será chamado de **desp_cand_2020**.

```r
#CRIAÇÃO DO DF FINAL
desp_cand_2020 <- inner_join(desp_2020,cand_2020,by = "SQ_CANDIDATO") %>% 
  select(1:21,55:66,22:53)
```

Temos abaixo o **head** do nosso data.frame

```r
head(desp_cand_2020)
```
![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/head.png "imagem 2")

Antes de finalizarmos esse passo, vamos checar um dar implicações do nosso join. O **inner_join** resultado num **data.frame** que só possui as observações que estão presentes nos dois objetos unidos. Logo, se tivéssemos um candidato sem despesas ou uma despesa sem candidato (algo muito improvável), ambos estariam excluídos de **desp_cand_2020**. Isso por si só já pode ser algo útil para uma análise já que podemos encontrar padrões de não-gasto, isto é, de menor investimento e/ou até desamparo por parte dos partidos dessas candidaturas.
Então coletaremos os **SQ_CANDIDATO** de **cand_2020** que não se encontram em **desp_2020**, ou seja, candidaturas registradas e deferidas, mas que não possuem gastos.

```r
n_gasto <- cand_2020 %>% 
  mutate(FEZ_GASTO = ifelse(SQ_CANDIDATO %in% desp_2020$SQ_CANDIDATO,
                            "SIM","NÃO"))

n_gasto %>% 
  group_by(DS_CARGO,FEZ_GASTO) %>% 
  summarise(contagem = n())
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/n_gasto.png "imagem 3")

Muito mais candidaturas à vereador não fizeram gastos do que aquelas para prefeito, tanto em números absolutos quanto em proporções. Vice-prefeitos não são votados mas precisam ser registrados e ficam sob a campanha principal do prefeito, o que explica a discrepâncias. Outros cruzamentos, como sexo, partido, idade e cor/raça podem eventualmente mostrar outros achados. 

## Terceiro passo: um mergulho preliminar
Agora iremos dar início propriamente à nossa análise. Este tópico em específico será usado para conhecermos os nossos dados e eventuais relações que eles podem demonstrar. Para isso, usaremos funções em R que geram medidas-resumo, descrição da estrutura dos dados e **pipes** de agrupamento e **summarise**. Além disso, plotaremos alguns gráficos, mesmo que pouco intuitivos e/ou visualmente objetivos. 
Evidemente os resultados aqui descritos não se encontrariam num relatório final de análise de dados a ser entregue às partes interessadas, já que eles possuem como utilidade informar o analista da forma mais simples e rápida possível. É na produção dos gráficos ao final da análise que despenderemos mais tempo em questões estéticas.

Inicialmente, executaremos a função **str**, que descreve o tipo de dados que cada variável tem e alguns exemplos de valores. O mais importante é constatar que a variável de valor da despesa esteja em formato numérico e que não existam erros de reconhecimento de tipo de variável. Ambos esses tópicos não aprensetaram problemas nos nossos dados.

```r
str(desp_cand_2020)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/str.png "imagem 4")

Em seguida, medidas-resumo da nossa variável de interesse, **VR_DESPESA_CONTRATADA**, o valor em reais da despesa contratada.

```r
summary(desp_cand_2020$VR_DESPESA_CONTRATADA)
```

![imagem](https://raw.githubusercontent.com/arjona-jonas/desp-sp-2020/main/imagens/summary.png "imagem 5")

