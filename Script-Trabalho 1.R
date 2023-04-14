install.packages("tidyverse")
library(tidyverse)

#com o codigo seguinte, eu abri o meu banco de dados "NYcrimes":
NYcrimes <- read.csv(choose.files())

#para tirar os NA's, eu utilizei este codigo:
NYcrimes <- na.omit(NYcrimes)

#depois,eu instalei um pacote para que fosse possivel mudar minhas variaveis para o portugues, para ficar mais facil de interpretrar, por meio deste codigo:
install.packages("data.table")
library(data.table)
setnames(NYcrimes, old = c("County", "Agency", "Year", "Months.Reported", "Violent.Total", "Murder", "Rape", "Robbery", "Aggravated.Assault", "Burglary", "Larceny", "Motor.Vehicle.Theft", "Region"), new = c("Condado", "Agencia", "Ano", "Mes.da.Reportagem", "Violencia.Total", "Assassinato", "Estupro", "Assalto", "Agressao.Agravada", "Roubo", "Furto", "Roubo.de.Veiculo", "Regiao"))

#A partir desse codigo eu fixei o nuumero de index.total por ano; por meio destes é possivel saber qual o ano em que ocorre mais crime e qual crime tem  maior incidencia
index.por.ano <- NYcrimes %>%
  group_by(Ano) %>% 
  summarise(total = sum(Index.Total))
ggplot(data = index.por.ano) +
  geom_bar(aes(x = Ano, y = total), colour = "black", fill = "#A11D21", stat = "identity") +
  theme_bw() +
  coord_flip() +
  ggtitle("Total de Crimes por Ano")

#A partir desse codigo eu fixei o numero de index.total por condado. Mas antes instalei um pacote.
install.packages("ggrepel")
library(ggrepel)
 index.por.condado <- NYcrimes %>%
  group_by(Condado) %>% 
  summarise(total = sum(Index.Total))
 ggplot(data = NYcrimes) +
   geom_bar(aes(x = reorder(Condado, Index.Total, FUN = sum), y = Index.Total), fill = "#A11D21", stat = "identity") +
   theme_bw() +
   coord_flip() +
   labs(x = "Condado", y = "Total de Crimes(Index.Total)", title = "Total de Crimes por Condado") +
   ggtitle("Total de Crimes por Condado") +
   geom_text_repel(data = filter(index.por.condado, total > 300000), aes(x = Condado, y = total, label = Condado), box.padding = 1.5, color = "darkblue", size = 5)
 
ggplot(data = NYcrimes) +
  geom_bar(aes(x = reorder(Condado, Index.Total, FUN = sum), y = Index.Total), fill = "#A11D21", stat = "identity") +
  theme_bw() +
  coord_flip() +
  labs(x = "Condado", y = "Total de Crimes(Index.Total)", title = "Total de Crimes por Condado") +
  ggtitle("Total de Crimes por Condado") +
  geom_text_repel(data = filter(index.por.condado, total < 6000), aes(x = Condado, y = total, label = Condado), box.padding = 3, color = "darkblue", size = 4, hjust = -0.5, vjust = -3)
 
  
 
 #A partir desse codigo eu fixei a violencia total por condado
 violencia.por.condado <- NYcrimes %>%
   group_by(Condado) %>% 
   summarise(total = sum(Violencia.Total))
 ggplot(data = NYcrimes) +
   geom_bar(aes(x = reorder(Condado, Violencia.Total, FUN = sum), y = Violencia.Total), fill = "#A11D21", stat = "identity") +
   theme_bw() +
   coord_flip() +
   labs(x = "Condado", y = "Total de Viol?ncia(Violencia.Total)", title = "Total de Viol?ncia por Condado") +
   ggtitle("Total de Viol?ncia por Condado") +
   geom_text_repel(data = filter(violencia.por.condado, total > 70000), aes(x = Condado, y = total, label = Condado), box.padding = 1.5, color = "darkblue", size = 5)

 ggplot(data = NYcrimes) +
   geom_bar(aes(x = reorder(Condado, Violencia.Total, FUN = sum), y = Violencia.Total), fill = "#A11D21", stat = "identity") +
   theme_bw() +
   coord_flip() +
   labs(x = "Condado", y = "Total de Viol?ncia(Violencia.Total)", title = "Total de Viol?ncia por Condado") +
   ggtitle("Total de Viol?ncia por Condado") +
   geom_text_repel(data = filter(violencia.por.condado, total < 400), aes(x = Condado, y = total, label = Condado), box.padding = 1.5, color = "darkblue", size = 5) 
 
 #A partir desse codigo eu fixei a violencia total por ano
 violencia.por.ano <- NYcrimes %>%
   group_by(Ano) %>% 
   summarise(total = sum(Violencia.Total))
 ggplot(data = violencia.por.ano) +
   geom_bar(aes(x = Ano , y = total), colour = "black", fill = "#A11D21", stat = "identity") +
   theme_bw() +
   coord_flip() +
   ggtitle("Total de Viol?ncia por Ano")

 #evolucao temporal de cada crime
 crime <- melt(data = NYcrimes, id = c("Condado", "Agencia", "Ano", "Mes.da.Reportagem", "Regiao", "Index.Total", "Violencia.Total", "Property.Total"), value.name = "Num.Crimes", variable.name = "crime")
 ggplot(data = filter(crime, Ano > 2009)) +
   geom_bar(aes(x = reorder(crime, Num.Crimes, FUN = sum),y = Num.Crimes, fill = crime), stat = "identity") +
   facet_wrap(~Ano) +
   labs(x = "Crime", y = "Frequ?ncia", title = "Quantidade de cada crime por Ano") +
   theme(axis.text.x = element_blank())
 
 ggplot(data = filter(crime, Ano > 2009)) +
   geom_bar(aes(x = Ano,y = Num.Crimes, fill = crime), stat = "identity") +
   facet_wrap(~crime) +
   labs(x = "Ano", y = "Numero de Crimes", title = "evolucao Temporal de cada crime") +
   theme(axis.text.x = element_blank())
 
 ggplot(data = filter(crime, Ano > 2009), aes( x = Ano, y = Num.Crimes, group = 1)) +
   facet_wrap(~crime) +
   geom_point(stat = "summary", fun.y = sum) +
   stat_summary(fun.y = sum, geom = "line")
 
 filter(crime, crime == "Assassinato")
 ASS <- filter(crime, crime == "Assassinato" | crime == "Estupro")
 ggplot(data = filter(ASS, Ano > 2010)) +
   geom_bar(aes(x = reorder(crime, Num.Crimes, FUN = sum),y = Num.Crimes, fill = crime), stat = "identity") +
   facet_wrap(~Ano) +
   labs(x = "Crimes", y = "Frequ?ncia", title = "Total de Crimes por Ano") +
   theme(axis.text.x = element_blank())
 
 #analise da evolucao temporal
 ss3 <- NYcrimes %>% select(Ano, Index.Total) %>% 
   group_by(Ano) %>% 
   mutate(total = sum(Index.Total)) %>% 
   select(-Index.Total) %>% 
   unique()
 ggplot(data = ss3) +
   geom_point(aes(x = Ano, y = total, group = 1)) +
   geom_line(aes(x = Ano, y = total, group = 1), size = 1) +
   labs(x = "Ano", y = "Total de crime", title = "evolucao Temporal do Index.Total") 
 
 ss3.1 <- NYcrimes %>% select(Ano, Violencia.Total) %>% 
   group_by(Ano) %>% 
   mutate(total = sum(Violencia.Total)) %>% 
   select(-Violencia.Total) %>% 
   unique()
 ggplot(data = ss3.1) +
   geom_point(aes(x = Ano, y = total, group = 1)) +
   geom_line(aes(x = Ano, y = total, group = 1), size = 1) +
   labs(x = "Ano", y = "Total de crime", title = "evolucao Temporal do Violencia.Total") 
 
 ss3.2 <- NYcrimes %>% select(Ano, Property.Total) %>% 
   group_by(Ano) %>% 
   mutate(total = sum(Property.Total)) %>% 
   select(-Property.Total) %>% 
   unique()
 ggplot(data = ss3.2) +
   geom_point(aes(x = Ano, y = total, group = 1)) +
   geom_line(aes(x = Ano, y = total, group = 1), size = 1) +
   labs(x = "Ano", y = "Total de crime", title = "evolucao Temporal do Property.Total") 
 
 #as frequencias de cada crime
 ggplot(data = crime) +
   geom_bar(aes(x = reorder(crime, Num.Crimes, FUN = sum), y = Num.Crimes, fill = crime), stat = "identity") +
   labs(x = "Crimes", y = "Numero de Crimes", title = "Total de Crimes") +
   theme(axis.text.x = element_blank())
 
 
 #porcentagem de crimes por condado
 ss <- NYcrimes %>% select(1,5, 7:10, 12:14)
 crimes2 <-melt(data = ss, id = c("Condado", "Index.Total"))
 crimes2 <- crimes2 %>% 
   group_by(Condado) %>% 
   mutate( total = sum(value)) %>%
   group_by(Condado, variable) %>% 
   mutate(totalcrimes = sum(value)) %>% 
   select(- value, -Index.Total)
 crimes2 <- unique(crimes2) 
 crimes2 <-  crimes2 %>%
   group_by(Condado, variable) %>% 
   mutate(porcentagemcrimes = (totalcrimes/total)*100) 
 
 ggplot(data = filter(crimes2, Condado == "Erie")) +
   geom_bar(aes(x = variable, y = porcentagemcrimes), stat = "identity", fill = "#A11D21") +
   theme(axis.text.x = element_text(size = 7.8)) +
   labs(x = "Crime", y = "Porcentagem", title = "Porcentagem de cada crime em Erie")
 
 ggplot(data = filter(crimes2, Condado == "Kings")) +
   geom_bar(aes(x = variable, y = porcentagemcrimes), stat = "identity", fill = "#A11D21") +
   theme(axis.text.x = element_text(size = 7.8)) +
   labs(x = "Crime", y = "Porcentagem", title = "Porcentagem de cada crime em Kings")
 
 ggplot(data = filter(crimes2, Condado == "Bronx")) +
   geom_bar(aes(x = variable, y = porcentagemcrimes), stat = "identity", fill = "#A11D21") +
   theme(axis.text.x = element_text(size = 7.8)) +
   labs(x = "Crime", y = "Porcentagem", title = "Porcentagem de cada crime em Bronx")
 
 ggplot(data = filter(crimes2, Condado == "Suffolk")) +
   geom_bar(aes(x = variable, y = porcentagemcrimes), stat = "identity", fill = "#A11D21") +
   theme(axis.text.x = element_text(size = 7.8)) +
   labs(x = "Crime", y = "Porcentagem", title = "Porcentagem de cada crime em Suffolk")
 
 ggplot(data = filter(crimes2, Condado == "Yates")) +
   geom_bar(aes(x = variable, y = porcentagemcrimes), stat = "identity", fill = "#A11D21") +
   theme(axis.text.x = element_text(size = 7.8)) +
   labs(x = "Crime", y = "Porcentagem", title = "Porcentagem de cada crime em Yates")
 
 ggplot(data = filter(crimes2, Condado == "Lewis")) +
   geom_bar(aes(x = variable, y = porcentagemcrimes), stat = "identity", fill = "#A11D21") +
   theme(axis.text.x = element_text(size = 7.8)) +
   labs(x = "Crime", y = "Porcentagem", title = "Porcentagem de cada crime em Lewis")
 
 #media total de cada crime, do index.total -> participacao de cada crime
 ss2 <- NYcrimes %>% select(5, 7:10, 12:14)
 crimes4 <-melt(data = ss2, id = c("Index.Total"))
 crimes4 <- crimes4 %>%
   mutate(totalcrimes = sum(value)) %>% 
   group_by(variable) %>% 
   mutate( total = sum(value)) %>%
   select(- value, -Index.Total)
 crimes4 <- unique(crimes4) 
 crimes4 <-  crimes4 %>%
   group_by( variable) %>% 
   mutate(porcentagemcrimes = (total/totalcrimes)*100) 
 
 ggplot(data = crimes4) +
   geom_bar(aes(x = variable, y = porcentagemcrimes ), stat = "identity", fill = "#A11D21" ) +
   labs(x = "Crime", y = "Porcentagem", title = "Participa??o de cada crime") +
   theme(axis.text.x = element_text(size = 7.8))
 
setwd("C:/Users/Wesley Araruna/Downloads/new-york-state-index-crimes")

