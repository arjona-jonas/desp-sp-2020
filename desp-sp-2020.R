###Jonas Arjona
##26/10/2022
##PROJETO DE ANÁLISE DE DADOS DE DESPESAS ELEITORAIS

#WORKING DIRECTORY 
setwd("[INSIRA SEU DIRETÓRIO DE ESCOLHA]")
getwd()

#PACOTES
library(tidyverse)
library(ggplot2)

#DESABILITAR NOTAÇÃO CIENTÍFICA
options(scipen = 999)

#DADOS
cand_2020 <- read.csv(file = "consulta_cand_2020_SP.csv",header = T, sep = ";", 
                      fileEncoding = "latin1",dec = ",")

desp_2020 <- read.csv(file = "despesas_contratadas_candidatos_2020_SP.csv",header = T, sep = ";", 
                      fileEncoding = "latin1",dec = ",")

#SELEÇÃO DAS OBSERVAÇÕES EM cand_2020 (APENAS DEFERIDOS DE FATO) E DE VARIÁVEIS
#RELEVANTES

colnames(cand_2020)
table(cand_2020$DS_DETALHE_SITUACAO_CAND)

var_rem <- c(1:14,17:42,45,56:71)
sel_sit <- c("DEFERIDO","DEFERIDO COM RECURSO")

cand_2020<- cand_2020 %>% 
  filter(DS_DETALHE_SITUACAO_CAND %in% sel_sit) %>% 
  select(-all_of(var_rem))

#CRIAÇÃO DO DF FINAL
desp_cand_2020 <- inner_join(desp_2020,cand_2020,by = "SQ_CANDIDATO") %>% 
  select(1:21,55:66,22:53)

#CHECANDO NÃO-GASTO
n_gasto <- cand_2020 %>% 
  mutate(FEZ_GASTO = ifelse(SQ_CANDIDATO %in% desp_2020$SQ_CANDIDATO,
                            "SIM","NÃO"))

n_gasto %>% 
  group_by(DS_CARGO,FEZ_GASTO) %>% 
  summarise(contagem = n())

#PODEMOS OBSERVAR A QUANTIDADE DE CONTAS PRESTADAS, OU SEJA,
#A QUANTIDADE DE DESPESAS CONTRATADAS POR PARTIDO,SEXO,CARGO,ETC.
#PODEMOS AINDA RETIRAR A MÉDIA, MIN E MAX

#PARTIDO
desp_cand_2020 %>% 
  group_by(SG_PARTIDO) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA),
            min = min(VR_DESPESA_CONTRATADA),
            max = max(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma)) %>% 
  print(n = 32)

desp_cand_2020 %>% 
  group_by(SG_PARTIDO) %>% 
  summarise(contagem = n()) %>% 
  ggplot()+
  geom_col(aes(x=reorder(SG_PARTIDO,contagem,sum),y=contagem))+
  coord_flip()

#SEXO
desp_cand_2020 %>% 
  group_by(DS_GENERO) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA),
            min = min(VR_DESPESA_CONTRATADA),
            max = max(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))

desp_cand_2020 %>% 
  group_by(DS_GENERO) %>% 
  summarise(contagem = n()) %>% 
  ggplot()+
  geom_col(aes(x=reorder(DS_GENERO,contagem,sum),y=contagem))

#COR/RAÇA
desp_cand_2020 %>% 
  group_by(DS_COR_RACA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA),
            min = min(VR_DESPESA_CONTRATADA),
            max = max(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))

desp_cand_2020 %>%
  select(SQ_CANDIDATO,DS_COR_RACA) %>% 
  unique() %>% 
  group_by(DS_COR_RACA) %>% 
  summarise(contagem = n())

desp_cand_2020 %>% 
  group_by(DS_COR_RACA) %>% 
  summarise(contagem = n()) %>% 
  ggplot()+
  geom_col(aes(x=reorder(DS_COR_RACA,contagem,sum),y=contagem))

#CARGO

desp_cand_2020 <- rename(desp_cand_2020, DS_CARGO = DS_CARGO.x)

desp_cand_2020 %>% 
  group_by(DS_CARGO) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA),
            min = min(VR_DESPESA_CONTRATADA),
            max = max(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))

desp_cand_2020 %>% 
  filter(DS_CARGO != "Vice-prefeito") %>%
  group_by(DS_CARGO) %>% 
  summarise(contagem = n()) %>% 
  arrange(desc(contagem)) %>% 
  ggplot()+
  geom_col(aes(x=DS_CARGO,y=contagem))

desp_cand_2020 %>%
  select(SQ_CANDIDATO,DS_CARGO) %>% 
  unique() %>% 
  group_by(DS_CARGO) %>% 
  summarise(contagem = n())

desp_cand_2020 %>%
  filter(VR_DESPESA_CONTRATADA >= 1000000) %>% 
  select(DS_CARGO,NM_CANDIDATO,NM_UE,
         SG_PARTIDO,DS_ORIGEM_DESPESA,
         VR_DESPESA_CONTRATADA)


#A VARIÁVEL DS_ORIGEM_DESPESA DESCREVE A DESPESA NO FORMATO PRÓPRIO DO TSE

desp_cand_2020 %>% 
  group_by(DS_ORIGEM_DESPESA) %>% 
  summarise(contagem = n()) %>% 
  arrange(desc(contagem)) %>% 
  print(n = 40)

desp_cand_2020 %>% 
  group_by(DS_ORIGEM_DESPESA) %>% 
  summarise(soma = sum(VR_DESPESA_CONTRATADA),
            media = mean(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma)) %>% 
  print(n=40)

#TEMA
tema <- theme(title = element_text(size = 20),
              axis.title = element_text(size = 17.5),
              axis.text = element_text(size = 13),
              legend.title = element_text(size = 17.5),
              legend.text = element_text(size = 13))

#BOXPLOT

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


#A SOLUÇÃO ESCOLHIDA FOI RECATEGORIZAR AS DESPESAS COM BASE EM OUTROS
#ESFORÇOS DESENVOLVIDOS POR PARES.

#O EFEITO FINAL SERIA GRANDES GRUPOS DE DESPESAS QUE, APESAR DE SEREM
#DIFERENTES, SERIAM MENOS DIFERENTES ENTRE SI DO QUE AS DESPESAS LIVRES.

#PARA CHECAR OS PORMENORES DA RECATEGORIZAÇÃO, VER CERVI, VASCONCELLOS E 
#CAVASSANA (2021).

#ABAIXO CRIAMOS 6 OBJETOS DISTINTOS QUE SERÃO USADOS PARA AGLUTINAR DESPESAS E
#SEREM REFERENCIADOS NUMA COMBINAÇÃO DE mutate E case_when

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

desp_cand_2020 %>% 
  group_by(TIPO_DESPESA) %>% 
  summarise(soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))

#QUANTIDADE DE DESPESAS
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

#SOMA TOTAL 
desp_cand_2020 %>% 
  group_by(TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>%
  ggplot()+
  geom_col(aes(x = reorder(TIPO_DESPESA,soma,sum), y = soma))+
  coord_flip()+
  labs(title = "Valor total de despesas declaradas por tipo",
       y = "Valor total",
       x = "Tipo de despesa")+
  tema


#FAIXA ETÁRIA
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

desp_cand_2020 %>% 
  group_by(FAIXA_ETARIA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))

desp_cand_2020 %>% 
  select(SQ_CANDIDATO,FAIXA_ETARIA) %>% 
  unique() %>% 
  group_by(FAIXA_ETARIA) %>% 
  summarise(contagem = n()) %>% 
  arrange(desc(contagem))

#COR/RAÇA2
branca <- "BRANCA"
nao_branca <- c("AMARELA","INDÍGENA","PARDA","PRETA","NÃO INFORMADO")

desp_cand_2020 <- desp_cand_2020 %>% 
  mutate(DS_COR_RACA2 = case_when(
    DS_COR_RACA %in% branca ~ "BRANCA",
    DS_COR_RACA %in% nao_branca ~ "NÃO-BRANCA"
  ))

desp_cand_2020 %>% 
  group_by(DS_COR_RACA2) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(desc(soma))

desp_cand_2020 %>% 
  select(SQ_CANDIDATO,DS_COR_RACA2) %>% 
  unique() %>% 
  group_by(DS_COR_RACA2) %>% 
  summarise(contagem = n()) %>% 
  arrange(desc(contagem))

#CORES
cores <- scale_fill_manual(values = c("#f6b395","#f6e095",
                                      "#b9f695","#acf5ed",
                                      "#acbff5","#eaacf5"))

#TIPO_DESPESA E SEXO
desp_cand_2020 %>% 
  group_by(DS_GENERO,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(TIPO_DESPESA,soma)

desp_cand_2020 %>% 
  group_by(DS_GENERO,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>%
  ggplot()+
  geom_col(aes(x=DS_GENERO,
               y=soma,
               fill=TIPO_DESPESA),
           position = "dodge")+
  labs(title = "Total gasto por sexo e tipo de despesa",
       x = "Sexo",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema

#TIPO_DESPESA E CARGO (REMOVENDO VICE-PREFEITO)
desp_cand_2020 %>% 
  filter(DS_CARGO != "Vice-prefeito") %>% 
  group_by(DS_CARGO,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(TIPO_DESPESA,soma)

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

#TIPO_DESPESA E PARTIDO (GRÁFICO ORGANIZADO POR TOTAL GASTO)
desp_cand_2020 %>% 
  group_by(SG_PARTIDO,TIPO_DESPESA) %>% 
  summarise(soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  pivot_wider(id_cols = 1,
              names_from = TIPO_DESPESA,
              values_from = soma) %>% 
  print(n = 32)

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

#PREFEITO
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

#VEREADOR
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

#TIPO_DESPESA E FAIXA ETARIA
desp_cand_2020 %>% 
  group_by(FAIXA_ETARIA,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(TIPO_DESPESA,soma) %>% 
  pivot_wider(id_cols = 1,
              names_from = TIPO_DESPESA,
              values_from = soma)

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

#TIPO_DESPESA E COR/RAÇA BINÁRIA
desp_cand_2020 %>% 
  group_by(DS_COR_RACA2,TIPO_DESPESA) %>% 
  summarise(contagem = n(),
            soma = sum(VR_DESPESA_CONTRATADA)) %>% 
  arrange(TIPO_DESPESA,soma)

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

####RESULTADOS E CONCLUSÕES####
#OLHANDO PARA NOSSOS GRÁFICOS PODEMOS CONCLUIR QUE AS DESPESAS COM COMUN.
#TRAD., COM PESSOAL E ADMINISTRATIVAS SÃO AS MAIS RECORRENTES EM TODOS OS
#CASOS (IDADE, SEXO, CARGO, E PARTIDO). HÁ ALGUMAS PEQUENAS NUANCES NAS
#NAS PROPORÇÕES, MAS O CENÁRIO SE MANTÉM O MESMO SEMPRE

#ALÉM DISSO, AS DESPESAS SÃO EM MAIOR QUANT. E TOTALIZAM MAIOR VALOR NAQUELAS
#CANDIDATURAS SABIDAMENTE MAIS COMUNS: MASCULINAS, BRANCAS, ENTRE 36 E 56
#ANOS.

#QUANTO A PARTIDOS, AQUELES QUE MAIS LANÇAM CANDIDATURAS (OU SEJA,
#OS MAIORES PARTIDOS) POSSUEM AS MAIORES SOMAS E QUANTIDADES DE DESPESAS


####E A CAMPANHA DIGITAL?####

#APESAR DO AMPLO DISCURSO DO USO DAS TICS NA COMUNICAÇÃO POLÍTICA, 
#ELE NÃO APARECE NAS DECLARAÇÕES OFICIAIS DE CONTAS.
#HÁ ALGUMA VARIAÇÃO ENTRE GENERO, CARGO, PARTIDO E IDADE MAS ELA É PEQUENA.

#VARIAÇÕES INTERESSANTES DE TESTAR SERIAM AQUELAS RELACIONADAS À GEOGRAFIA:
#PORTE DO MUNICÍPIO/ESTADO/REGIÃO,IDH,COMPOSIÇÃO ETÁRIA,ACESSO AOS TICS, ETC.

#POSSÍVEIS EXPLICAÇÕES (APENAS PARA ESTADO DE SP):

#1-QUANDO COMPARADAS COM OUTRAS DESPESAS, O CUSTO-BENEFÍCIO DAS TICS PARECE
#SER BEM MAIS VANTAJOSO PARA ELAS DO QUE PARA OUTRAS DESPESAS. OU SEJA, PODE
#SER MAIS BARATO FAZER O MESMO USANDO TICS DO QUE USANDO DE MÉTODOS DE 
#CAMPANHA MAIS TRADICIONAIS. PORÉM, SERIA PRECISO PROPOR MÉTRICAS PARA TAL
#COMPARAÇÃO (COMO UMA TAXA DE CONVERSÃO ENTRE REAL GASTO-VOTO PARA AMBOS OS
#TIPOS DE ESTRATÉGIA)

#2-MUITAS DAS VANTAGENS DE USAR TICS EM CAMPANHA ELEITORAL PODE SE DAR POR 
#USOS INFORMAIS, NÃO DECLARÁVEIS, COMO ATRAVÉS DE REDES DE RELAÇÕES SOCIAIS 
#DE GRUPOS ESPECÍFICOS (OCUPAÇÃO PROFISSIONAL, RELIGIÃO, HOBBIES, ETC.).

#3-CAIXA DOIS


#DE QUALQUER FORMA, PERSISTE A NOÇÃO DE QUE HÁ BASTANTE
#DIFICULDADE EM AVALIARMOS, COM MÉTRICAS, O USO DAS TICS EM CAMPANHAS,
#MESMO QUANDO FALAMOS A TODO MOMENTO QUE ELAS SÃO IMPORTANTES.

#ABORDAGENS QUANTITATIVAS PARECEM SER MAIS FALHAS NESSA TEMÁTICA





