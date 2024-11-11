#filtrando por esporte
ginastica <- filter(dados,Sport=='Gymnastics')
judo <- filter(dados,Sport=='Judo')
atletismo <- filter(dados,Sport=='Athletics')
badminton <- filter(dados,Sport=='Badminton')
#conversao e calculando IMC
ginastica$IMC <- (ginastica$Weight*0.453592) / ((ginastica$Height*0.01)^2)
judo$IMC <- (judo$Weight*0.453592) / ((judo$Height*0.01)^2)
atletismo$IMC <- (atletismo$Weight*0.453592) / ((atletismo$Height*0.01)^2)
badminton$IMC <- (badminton$Weight*0.453592) / ((badminton$Height*0.01)^2)
#categorias de imc
dadosimc <- rbind(ginastica,judo,atletismo,badminton)
dadosimc <- dadosimc %>%
  mutate(IMCcat = case_when(
    IMC < 18.5 ~ "Abaixo",
    IMC >= 18.5 & IMC < 24.9 ~ "Normal",
    IMC >= 25 ~ "Sobrepeso"
  ))
dadosimc


top5paisesp <- as.data.frame(top5paises)
top5paisesp

classes <- top5paisesp %>%
  filter(!is.na(Var1)) %>%
  count(Var1) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )
ggplot(top5paisesp) +
  aes(x = fct_reorder(class, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Países", y = "Frequência") +
  theme_estat()
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm"
)




#total do top 5 em relação ao total : 46,97% do total
totaltop5 <- sum(top5paises)
totaltudo <- sum(paisescresc)
porctop5 <- (totaltop5/totaltudo)*100
porctop5
