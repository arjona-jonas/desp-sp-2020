###Jonas Arjona
##26/10/2022
##PROJETO DE AN�LISE DE DADOS DE DESPESAS ELEITORAIS

#WORKING DIRECTORY 
setwd("E:/ANTES DA MUDAN�A/Jonas/quanti R/github/desp-sp-2020")
getwd()

#PACOTES
library(tidyverse)
library(ggplot2)

#DESABILITAR NOTA��O CIENT�FICA
options(scipen = 999)

#DADOS
cand_2020 <- read.csv(file = "consulta_cand_2020_SP.csv",header = T, sep = ";", 
                      fileEncoding = "latin1",dec = ",")

desp_2020 <- read.csv(file = "despesas_contratadas_candidatos_2020_SP.csv",header = T, sep = ";", 
                      fileEncoding = "latin1",dec = ",")

#SELE��O DAS OBSERVA��ES EM cand_2020 (APENAS DEFERIDOS DE FATO) E DE VARI�VEIS
#RELEVANTES

colnames(cand_2020)
table(cand_2020$DS_DETALHE_SITUACAO_CAND)

var_rem <- c(1:14,17:42,45,56:71)
sel_sit <- c("DEFERIDO","DEFERIDO COM RECURSO")

cand_2020<- cand_2020 %>% 
  filter(DS_DETALHE_SITUACAO_CAND %in% sel_sit) %>% 
  select(-all_of(var_rem))

#CRIA��O DO DF FINAL
desp_cand_2020 <- inner_join(desp_2020,cand_2020,by = "SQ_CANDIDATO") %>% 
  select(1:21,55:66,22:53)

#CHECANDO N�O-GASTO
n_gasto <- cand_2020 %>% 
  mutate(FEZ_GASTO = ifelse(SQ_CANDIDATO %in% desp_2020$SQ_CANDIDATO,
                            "SIM","N�O"))

n_gasto %>% 
  group_by(DS_CARGO,FEZ_GASTO) %>% 
  summarise(contagem = n())

#PODEMOS OBSERVAR A QUANTIDADE DE CONTAS PRESTADAS, OU SEJA,
#A QUANTIDADE DE DESPESAS CONTRATADAS POR PARTIDO,SEXO,CARGO,ETC.
#PODEMOS AINDA RETIRAR A M�DIA, MIN E MAX

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

#COR/RA�A
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


#A VARI�VEL DS_ORIGEM_DESPESA DESCREVE A DESPESA NO FORMATO PR�PRIO DO TSE

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


#A SOLU��O ESCOLHIDA FOI RECATEGORIZAR AS DESPESAS COM BASE EM OUTROS
#ESFOR�OS DESENVOLVIDOS POR PARES.

#O EFEITO FINAL SERIA GRANDES GRUPOS DE DESPESAS QUE, APESAR DE SEREM
#DIFERENTES, SERIAM MENOS DIFERENTES ENTRE SI DO QUE AS DESPESAS LIVRES.

#PARA CHECAR OS PORMENORES DA RECATEGORIZA��O, VER CERVI, VASCONCELLOS E 
#CAVASSANA (2021).

#ABAIXO CRIAMOS 6 OBJETOS DISTINTOS QUE SER�O USADOS PARA AGLUTINAR DESPESAS E
#SEREM REFERENCIADOS NUMA COMBINA��O DE mutate E case_when

com_dig<-c("Cria��o e inclus�o de p�ginas na internet",
           "Despesa com Impulsionamento de Conte�dos",
           "Despesas com Hospedagem",
           "Taxa de Administra��o de Financiamento Coletivo")

com_tradicional<-c("Atividades de milit�ncia e mobiliza��o de rua",
                   "Com�cios",
                   "Produ��o de jingles, vinhetas e slogans",
                   "Produ��o de programas de r�dio, televis�o ou v�deo",
                   "Publicidade por adesivos",
                   "Publicidade por carros de som",
                   "Publicidade por jornais e revistas",
                   "Publicidade por materiais impressos")

pessoal<-c("Despesas com pessoal",
           "Servi�os prestados por terceiros",
           "Servi�os pr�prios prestados por terceiros")

administrativa<-c("Alimenta��o",
                  "Combust�veis e lubrificantes",
                  "Correspond�ncias e despesas postais",
                  "Despesas com transporte ou deslocamento",
                  "Doa��es financeiras a outros candidatos/partidos",
                  "Encargos financeiros, taxas banc�rias e/ou op. cart�o de cr�dito",
                  "Encargos sociais",
                  "Eventos de promo��o da candidatura",
                  "Impostos, contribui��es e taxas",
                  "Materiais de expediente",
                  "Multas eleitorais",
                  "Passagem A�rea",
                  "Pesquisas ou testes eleitorais",
                  "Reembolsos de gastos realizados por eleitores",
                  "Servi�os advocat�cios",
                  "Servi�os cont�beis")

infra<-c("Telefone",
         "�gua",
         "Aquisi��o/Doa��o de bens m�veis ou im�veis",
         "Cess�o ou loca��o de ve�culos",
         "Energia el�trica",
         "Loca��o/cess�o de bens im�veis",
         "Loca��o/cess�o de bens m�veis (exceto ve�culos)",
         "Pr�-instala��o f�sica de comit� de campanha",
         "Despesa com geradores de energia")

div<-"Diversas a especificar"

desp_cand_2020 <- desp_cand_2020 %>% 
  mutate(TIPO_DESPESA=case_when(
    DS_ORIGEM_DESPESA %in% com_dig~"Comunica��o digital",
    DS_ORIGEM_DESPESA %in% com_tradicional~"Comunica��o tradicional",
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
  labs(title = "N�mero de despesas declaradas por tipo",
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


#FAIXA ET�RIA
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

#COR/RA�A2
branca <- "BRANCA"
nao_branca <- c("AMARELA","IND�GENA","PARDA","PRETA","N�O INFORMADO")

desp_cand_2020 <- desp_cand_2020 %>% 
  mutate(DS_COR_RACA2 = case_when(
    DS_COR_RACA %in% branca ~ "BRANCA",
    DS_COR_RACA %in% nao_branca ~ "N�O-BRANCA"
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
       subtitle = "Exclu�dos vice-prefeitos",
       x = "Cargo",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema

#TIPO_DESPESA E PARTIDO (GR�FICO ORGANIZADO POR TOTAL GASTO)
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
  labs(title = "Total gasto por faixa et�ria e tipo de despesa",
       x = "Faixa et�ria",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema

#TIPO_DESPESA E COR/RA�A BIN�RIA
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
  labs(title = "Total gasto por cor/ra�a e tipo de despesa",
       x = "Cor/ra�a",
       y = "Total gasto (reais)",
       fill = "Tipo de despesa")+
  cores+
  tema

####RESULTADOS E CONCLUS�ES####
#OLHANDO PARA NOSSOS GR�FICOS PODEMOS CONCLUIR QUE AS DESPESAS COM COMUN.
#TRAD., COM PESSOAL E ADMINISTRATIVAS S�O AS MAIS RECORRENTES EM TODOS OS
#CASOS (IDADE, SEXO, CARGO, E PARTIDO). H� ALGUMAS PEQUENAS NUANCES NAS
#NAS PROPOR��ES, MAS O CEN�RIO SE MANT�M O MESMO SEMPRE

#AL�M DISSO, AS DESPESAS S�O EM MAIOR QUANT. E TOTALIZAM MAIOR VALOR NAQUELAS
#CANDIDATURAS SABIDAMENTE MAIS COMUNS: MASCULINAS, BRANCAS, ENTRE 36 E 56
#ANOS.

#QUANTO A PARTIDOS, AQUELES QUE MAIS LAN�AM CANDIDATURAS (OU SEJA,
#OS MAIORES PARTIDOS) POSSUEM AS MAIORES SOMAS E QUANTIDADES DE DESPESAS


####E A CAMPANHA DIGITAL?####

#APESAR DO AMPLO DISCURSO DO USO DAS TICS NA COMUNICA��O POL�TICA, 
#ELE N�O APARECE NAS DECLARA��ES OFICIAIS DE CONTAS.
#H� ALGUMA VARIA��O ENTRE GENERO, CARGO, PARTIDO E IDADE MAS ELA � PEQUENA.

#VARIA��ES INTERESSANTES DE TESTAR SERIAM AQUELAS RELACIONADAS � GEOGRAFIA:
#PORTE DO MUNIC�PIO/ESTADO/REGI�O,IDH,COMPOSI��O ET�RIA,ACESSO AOS TICS, ETC.

#POSS�VEIS EXPLICA��ES (APENAS PARA ESTADO DE SP):

#1-QUANDO COMPARADAS COM OUTRAS DESPESAS, O CUSTO-BENEF�CIO DAS TICS PARECE
#SER BEM MAIS VANTAJOSO PARA ELAS DO QUE PARA OUTRAS DESPESAS. OU SEJA, PODE
#SER MAIS BARATO FAZER O MESMO USANDO TICS DO QUE USANDO DE M�TODOS DE 
#CAMPANHA MAIS TRADICIONAIS. POR�M, SERIA PRECISO PROPOR M�TRICAS PARA TAL
#COMPARA��O (COMO UMA TAXA DE CONVERS�O ENTRE REAL GASTO-VOTO PARA AMBOS OS
#TIPOS DE ESTRAT�GIA)

#2-MUITAS DAS VANTAGENS DE USAR TICS EM CAMPANHA ELEITORAL PODE SE DAR POR 
#USOS INFORMAIS, N�O DECLAR�VEIS, COMO ATRAV�S DE REDES DE RELA��ES SOCIAIS 
#DE GRUPOS ESPEC�FICOS (OCUPA��O PROFISSIONAL, RELIGI�O, HOBBIES, ETC.).

#3-CAIXA DOIS


#DE QUALQUER FORMA, PERSISTE A NO��O DE QUE H� BASTANTE
#DIFICULDADE EM AVALIARMOS, COM M�TRICAS, O USO DAS TICS EM CAMPANHAS,
#MESMO QUANDO FALAMOS A TODO MOMENTO QUE ELAS S�O IMPORTANTES.

#ABORDAGENS QUANTITATIVAS PARECEM SER MAIS FALHAS NESSA TEM�TICA




