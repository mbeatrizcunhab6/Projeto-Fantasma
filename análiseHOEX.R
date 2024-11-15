#abrindo diretório, dados e bibliotecas
dir()
getwd()
library(readxl)
ano00 <- readxl::read_xlsx("/Users/mariabeatrizcunhabarros/Downloads/houseofexc/Olimpiadas 2000 - 2016.xlsx",
                           sheet = 4)
ano04 <- readxl::read_xlsx("/Users/mariabeatrizcunhabarros/Downloads/houseofexc/Olimpiadas 2000 - 2016.xlsx",
                           sheet = 1)
ano08 <- readxl::read_xlsx("/Users/mariabeatrizcunhabarros/Downloads/houseofexc/Olimpiadas 2000 - 2016.xlsx",
                           sheet = 5)
ano12 <- readxl::read_xlsx("/Users/mariabeatrizcunhabarros/Downloads/houseofexc/Olimpiadas 2000 - 2016.xlsx",
                           sheet = 2)
ano16 <- readxl::read_xlsx("/Users/mariabeatrizcunhabarros/Downloads/houseofexc/Olimpiadas 2000 - 2016.xlsx",
                           sheet = 3)

ano00$Year <- 2000
ano04$Year <- 2004
ano08$Year <- 2008
ano12$Year <- 2012
ano16$Year <- 2016

names(ano00)[1:9] <- c('Name','Sex','Age','Height','Weight','Team','Sport','Event','Medal')
names(ano04)[1:9] <- c('Name','Sex','Age','Height','Weight','Team','Sport','Event','Medal')
names(ano08)[1:9] <- c('Name','Sex','Age','Height','Weight','Team','Sport','Event','Medal')
names(ano12)[1:9] <- c('Name','Sex','Age','Height','Weight','Team','Sport','Event','Medal')
names(ano16)[1:9] <- c('Name','Sex','Age','Height','Weight','Team','Sport','Event','Medal')
dados = rbind(ano00, ano04, ano08, ano12, ano16)
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)

install.packages("car")
library(car)
library(reshape2)
install.packages('nortest')
library(nortest)

#separando pessoas medalistas
dadosf <- dados %>% filter(!is.na(dados$Medal))
dadosf
#mulheres medalistas
mulheresm <- filter(dadosf,Sex=="F")
mulheresm <- distinct(mulheresm, Name, .keep_all = TRUE)

#pegando as frequencias de cada pais
freqpaises <- table(mulheresm$Team)
freqpaises
sum(freqpaises)
paisescresc <- sort(freqpaises, decreasing = TRUE) #maiorpmenor
paisescresc
prop.table((paisescresc))



#montando top 5 :EUA (34,45%), RUSSIA (19,30%), CHINA (17,90%), AUSTRALIA (15,85%), GERMANY (12,49%)
top5paises <- head(paisescresc, 5)
top5paises
top5 <- barplot(top5paises, col = c("#901238","#BC0046",'#CA687C','#DEA9BE','#ECD2DF'), xlab = "Países", ylab = "Frequência absoluta")
pie(top5paises, col = c("#901238","#BC0046",'#CA687C','#DEA9BE','#ECD2DF'))

#separando os esportes pedidos

dadosesp <- dadosf %>%
  filter(Sport %in% c("Gymnastics","Football","Judo","Athletics","Badminton"))

dadosesp$Height <- dadosesp$Height / 100
dadosesp$Weight <- dadosesp$Weight * 0.453592
dadosesp$IMC <- dadosesp$Weight / (dadosesp$Height^2)
dadosesp$IMCcat <- ifelse(dadosesp$IMC < 18.5,"Abaixo",
                          ifelse(dadosesp$IMC >= 18.5 & dadosesp$IMC < 25, "Normal",
                                 ifelse(dadosesp$IMC >= 25 & dadosesp$IMC < 30, "Sobrepeso",
                                        ifelse(dadosesp$IMC >=20,"Obesidade", NA))))

categorias <- table(dadosesp$IMCcat, dadosesp$Sport)
categorias

categorias <- as.data.frame(categorias)

dadosesp %>%
  group_by(Sport) %>%
  summarise(
    mediaIMC = mean(IMC, na.rm = TRUE),
    medianaIMC = median(IMC, na.rm = TRUE),
    dpIMC = sd(IMC, na.rm = TRUE),
    minIMC = min(IMC, na.rm = TRUE),
    maxIMC = max(IMC, na.rm = TRUE)
  )


a <- shapiro.test(dadosesp$IMC)
a
# dados nao-paramétricos
kruskalesp <- kruskal.test(IMC~Sport, data=dadosesp)
summary(kruskalesp)

library(FSA)
dunnTest(IMC~Sport, data = dadosesp, method = "bonferroni")

anovaesp <- aov(IMC ~ Sport, data = dadosesp)
summary(anovaesp)

tukeyesp <- TukeyHSD(anovaesp)
tukeyesp

#analisando ginastica

ginastica <-dadosesp %>%
  filter(Sport %in% c("Gymnastics"))

ginastica %>%
  group_by(Sport) %>%
  summarise(
    mediaIMC = mean(IMC, na.rm = TRUE),
    medianaIMC = median(IMC, na.rm = TRUE),
    dpIMC = sd(IMC, na.rm = TRUE),
    minIMC = min(IMC, na.rm = TRUE),
    maxIMC = max(IMC, na.rm = TRUE)
  )

#analisando fut

fut <-dadosesp %>%
  filter(Sport %in% c("Football"))

fut %>%
  group_by(Sport) %>%
  summarise(
    mediaIMC = mean(IMC, na.rm = TRUE),
    medianaIMC = median(IMC, na.rm = TRUE),
    dpIMC = sd(IMC, na.rm = TRUE),
    minIMC = min(IMC, na.rm = TRUE),
    maxIMC = max(IMC, na.rm = TRUE)
  )

#analisando judo

judo <-dadosesp %>%
  filter(Sport %in% c("Judo"))

judo %>%
  group_by(Sport) %>%
  summarise(
    mediaIMC = mean(IMC, na.rm = TRUE),
    medianaIMC = median(IMC, na.rm = TRUE),
    dpIMC = sd(IMC, na.rm = TRUE),
    minIMC = min(IMC, na.rm = TRUE),
    maxIMC = max(IMC, na.rm = TRUE)
  )

#analisando atletismo

atletismo <-dadosesp %>%
  filter(Sport %in% c("Athletics"))

atletismo %>%
  group_by(Sport) %>%
  summarise(
    mediaIMC = mean(IMC, na.rm = TRUE),
    medianaIMC = median(IMC, na.rm = TRUE),
    dpIMC = sd(IMC, na.rm = TRUE),
    minIMC = min(IMC, na.rm = TRUE),
    maxIMC = max(IMC, na.rm = TRUE)
  )

#analisando Badminton

bad <-dadosesp %>%
  filter(Sport %in% c("Badminton"))

bad %>%
  group_by(Sport) %>%
  summarise(
    mediaIMC = mean(IMC, na.rm = TRUE),
    medianaIMC = median(IMC, na.rm = TRUE),
    dpIMC = sd(IMC, na.rm = TRUE),
    minIMC = min(IMC, na.rm = TRUE),
    maxIMC = max(IMC, na.rm = TRUE)
  )


#quais são os 3 medalhistas com maior número de medalhas no total, e dentre eles

medalistas <- dadosf %>%
  group_by(Name) %>%
  summarise(TotalMedalhas = n()) %>%
  arrange(desc(TotalMedalhas))

medalistas

#Michael Fred Phelps, II (28), Natalie Anne Coughlin (-Hall) (12), Ryan Steven Lochte (12)
top3med <- medalistas %>%
  slice_max(TotalMedalhas, n = 3)
top3med

top3isolado <- dadosf %>%
  filter(Name %in% c("Michael Fred Phelps, II","Natalie Anne Coughlin (-Hall)", "Ryan Steven Lochte"))

tabelamed <- table(top3isolado$Name, top3isolado$Medal)
tabelamed
medalhas <- as.data.frame(tabelamed)

top3isolado <- top3isolado %>%
  mutate(Name = case_when(Name == "Michael Fred Phelps, II" ~ "Michael Phelps",
                       Name == "Natalie Anne Coughlin (-Hall)"~"Natalie Anne",
                       Name == "Ryan Steven Lochte"~"Ryan Lochte"))

chisq.test(tabelamed)


#vendo se as variáveis são paramétricas (teste anderson-darling)
altura <- ks.test(dadosf$Height,"pnorm",mean(dadosf$Height),sd = sd(dadosf$Height))
altura2 <- ad.test(dadosf$Height)
peso <- ad.test(dadosf$Weight)

#ambos rejeitam a hipotese-nula, ou seja, são não-paramétricos
#fazendo teste de correlação

cor.test(dadosf$Height,dadosf$Weight,method = "spearman")
cor.test(dadosf$Height,dadosf$Weight,method = "kendall")

#p-valor menor que 0.05, ou seja, significativo e tau 0.665, ou seja correlação positiva
#coeficiente positivo = diretamente proporcional
dadosespp <- na.omit(dadosesp)
