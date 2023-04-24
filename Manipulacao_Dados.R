---
  title: "Manipulação de Dados para Análise"
subtitle: "Aluno: Gustavo Lopes Braga Dias"
---

### Instalando bibliotecas e definindo preferências: ###
  
install.packages("tidyverse")
install.packages("readr")
install.packages("devtools")
install.packages("ggplot2")
library(tidyverse)
library(conflicted)
library(readr)
library(dplyr)
library(ggplot2)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)


### Carregando bases de dados da web: ###

life_exp <- read_csv("https://raw.githubusercontent.com/gugalbdias/Intro-R/main/5c90d02a-52fb-433f-a9d2-04e475e5a37f_Data.csv")
## expectativa de vida ao nascer, independente do sexo (métrica de saúde) ##

school_att <- read_csv("https://raw.githubusercontent.com/gugalbdias/Intro-R/main/2b51efca-816f-4c85-b6d5-cc759432f493_Data.csv")
## proporção, de mulheres sobre homens, das crianças de 7 a 12 anos  que estão na escola (métrica de educação) ##


### Transformando e limpando os dados: ###
## manteremos o horizonte de análise entre 2001 e 2016 por conta da disponibilidade dos dados ##

life_exp <- life_exp %>%
  select("Country Name", "2001 [YR2001]", "2002 [YR2002]", "2003 [YR2003]", "2004 [YR2004]", "2005 [YR2005]", "2006 [YR2006]", "2007 [YR2007]", "2008 [YR2008]", "2009 [YR2009]", "2010 [YR2010]", "2011 [YR2011]", "2012 [YR2012]", "2013 [YR2013]", "2014 [YR2014]", "2015 [YR2015]", "2016 [YR2016]") %>%
  rename(Country = "Country Name", "2001" = "2001 [YR2001]", "2002" = "2002 [YR2002]", "2003" = "2003 [YR2003]", "2004" = "2004 [YR2004]", "2005"="2005 [YR2005]", "2006"="2006 [YR2006]", "2007"="2007 [YR2007]", "2008"="2008 [YR2008]", "2009"="2009 [YR2009]","2010"= "2010 [YR2010]", "2011"="2011 [YR2011]", "2012"="2012 [YR2012]", "2013"="2013 [YR2013]", "2014"="2014 [YR2014]", "2015"="2015 [YR2015]", "2016"="2016 [YR2016]") %>%
  drop_na()
## selecionando as colunas-alvo e renomeando-as ##

life_exp <- life_exp %>%
  pivot_longer(cols=c("2001":"2016"), names_to = "Year", values_to = "Life Expectancy")
## pivotando as colunas de ano para otimização dos dados ##

life_exp$'Life Expectancy' <- ifelse(life_exp$'Life Expectancy'=='..',NA,life_exp$'Life Expectancy')
## retirando valores não numéricos (falta de dados) da base ##


## Mesmo processo para o indicador de educação: ##

school_att <- school_att %>%
  select("Country Name", "2001 [YR2001]", "2002 [YR2002]", "2003 [YR2003]", "2004 [YR2004]", "2005 [YR2005]", "2006 [YR2006]", "2007 [YR2007]", "2008 [YR2008]", "2009 [YR2009]", "2010 [YR2010]", "2011 [YR2011]", "2012 [YR2012]", "2013 [YR2013]", "2014 [YR2014]", "2015 [YR2015]", "2016 [YR2016]") %>%
  rename(Country = "Country Name", "2001" = "2001 [YR2001]", "2002" = "2002 [YR2002]", "2003" = "2003 [YR2003]", "2004" = "2004 [YR2004]", "2005"="2005 [YR2005]", "2006"="2006 [YR2006]", "2007"="2007 [YR2007]", "2008"="2008 [YR2008]", "2009"="2009 [YR2009]","2010"= "2010 [YR2010]", "2011"="2011 [YR2011]", "2012"="2012 [YR2012]", "2013"="2013 [YR2013]", "2014"="2014 [YR2014]", "2015"="2015 [YR2015]", "2016"="2016 [YR2016]") %>%
  drop_na()

school_att <- school_att %>%
  pivot_longer(cols=c("2001":"2016"), names_to = "Year", values_to = "GPIA")

school_att$GPIA <- ifelse(school_att$GPIA=='..',NA,school_att$GPIA)


### Unificando as bases de dados, ja retirando as linhas sem dados e definindo tipos das strings: ###

base <- left_join(life_exp, school_att, by=c("Country","Year")) %>%
  drop_na()

base$'Life Expectancy' <- as.numeric(base$'Life Expectancy')
base$GPIA <- as.numeric(base$GPIA)
base$Date <- as.Date(paste0(base$Year, "-01-01"), format = "%Y-%m-%d")
base$Year <- year(base$Date)


### Aplicando filtro para manter apenas países com dados em todos os períodos de análise ###

base <- base %>%
  select('Country','Year','GPIA','Life Expectancy') %>%
  group_by(Country) %>%
  filter(n()==16)%>%
  ungroup

## Temos apenas dois países com a base de dados completa ##


### O valor ideal para o GPIA é de 1, ou seja, mesma proporção de homens e mulheres na escola primária ###
### Assim, calcularemos um indicador de edução dado pelo quadrado da diferença entre o GPIA de cada país e um  ###
### Ou seja, quanto menor for o quadrado da diferença, melhor ###

base$'Var_GPIA' <- (base$GPIA - 1)^2


### Criando bases individuais para cada país: ###

dom_rep <- subset(base, Country=='Dominican Republic')
peru <- subset(base, Country=='Peru')


### Calculando o crescimento da expectativa de vida como indicador de saúde: ###

dom_rep$'LE_Growth' <- c(NA, ((dom_rep$'Life Expectancy'[-1]/dom_rep$'Life Expectancy'[-nrow(dom_rep)])-1)*100)
peru$'LE_Growth' <- c(NA, ((peru$'Life Expectancy'[-1]/peru$'Life Expectancy'[-nrow(peru)])-1)*100)


### Unindo as bases novamente: ###

base <- bind_rows(dom_rep,peru)


# Fazendo gráficos comparando os resultados: #

base_agrupada <- base %>%
  group_by(Country, Year)

ggplot(base_agrupada, aes(x= 'Var_GPIA', y= 'LE_Growth' , color=Country)) +
  geom_point()+
  labs(x='Variância em relação à um', y='Expectativa de vida')+
  theme_classic()                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             

