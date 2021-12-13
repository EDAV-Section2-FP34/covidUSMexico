library(tidyverse)


covid_MX <- read.csv("C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/COVID_mex.csv", stringsAsFactors = F)
covid_US <- read.csv("C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/COVID_usa.csv", stringsAsFactors = F)

inflation_MX <- read.csv("C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/Inflation_MX_2019_2021.csv", stringsAsFactors = F)
inflation_US <- read.csv("C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/Inflation_US_2012_2021.csv", stringsAsFactors = F)

GDP_MX <- read.csv("C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/GDP_MX_Clean.csv", stringsAsFactors = F)
GDP_US <- read.csv("C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/GDP_US_Clean.csv", stringsAsFactors = F)

financials<- read_csv('C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/Financial data.csv')


# Health data ratios
vars=c("date","confirmed","deaths","people_fully_vaccinated")
covid_MX<-covid_MX%>%
  select(vars)

covid_US<-covid_US%>%
  select(vars)

covid<- covid_MX%>%
  left_join(covid_US,by="date",suffix=c("_MX","_USA"))%>%
  mutate(date=as.Date(date))

# Mexico/USA

cases_ratio<-covid%>%
  mutate(value=confirmed_MX/confirmed_USA)%>%
  select(date,value)
  
deaths_ratio<-covid%>%
  mutate(value=deaths_MX/deaths_USA)%>%
  select(date,value)

vaccines_ratio<-covid%>%
  mutate(value= people_fully_vaccinated_MX/people_fully_vaccinated_USA)%>%
  select(date,value)

# vaccines ratio
ggplot(cases_ratio)+
  geom_line(aes(x=date,y=value))


##############
covid_MX <- read.csv("C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/COVID_mex.csv", stringsAsFactors = F)
covid_US <- read.csv("C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/COVID_usa.csv", stringsAsFactors = F)

aux <- covid_MX %>% inner_join(dplyr::select(covid_US, date, confirmed, population),
                               by=c("date"="date"))

aux$Daily_Cases_MX <- c(aux$confirmed.x[1], (aux$confirmed.x[-1]-aux$confirmed.x[-nrow(aux)])) / aux$population.x
aux$Daily_Cases_US <- c(aux$confirmed.y[1], (aux$confirmed.y[-1]-aux$confirmed.y[-nrow(aux)])) / aux$population.y

# Simple exponential smoothing function
ES <- function(x, l){
  s = rep(0, length(x))
  for(k in 1:length(x)){
    if(k==1){
      s[k] = x[k]
    }else{
      s[k] = l*x[k] + (1-l)*s[k-1]
    }
  }
  return(s)
}

aux[is.na(aux)] <- 0
#aux <- aux %>% 
#  mutate(Cases_MX = zoo::rollmean(Daily_Cases_MX, k = 7, fill = NA),
#         Cases_US = zoo::rollmean(Daily_Cases_US, k = 7, fill = NA) ) %>% 
#  filter(!is.na(Cases_US))

ggplot(aux)+
  geom_line(aes(x=as.Date(date),Cases_MX*1e5))+
  geom_line(aes(x=as.Date(date),Cases_US*1e5))
###################
aux2 <- covid_MX %>% inner_join(dplyr::select(covid_US, date, deaths, population),
                               by=c("date"="date"))
aux2$Daily_Cases_MX <- c(aux2$deaths.x[1], (aux2$deaths.x[-1]-aux2$deaths.x[-nrow(aux2)])) / aux2$population.x
aux2$Daily_Cases_US <- c(aux2$deaths.y[1], (aux2$deaths.y[-1]-aux2$deaths.y[-nrow(aux2)])) / aux2$population.y


aux2[is.na(aux2)] <- 0
#aux2$Cases_MX <- ES(aux2$Daily_Cases_MX, 0.1)
#aux2$Cases_US <- ES(aux2$Daily_Cases_US, 0.1)
aux2 <- aux2 %>% filter(!is.na(Cases_US))

ggplot(aux2)+
  geom_line(aes(x=as.Date(date),Cases_MX*1e7,color="Mex"))+
  geom_line(aes(x=as.Date(date),Cases_US*1e7,color="US"))

###################
aux3 <- covid_MX %>% inner_join(dplyr::select(covid_US, date,people_fully_vaccinated , population),
                                by=c("date"="date"))
aux3$Daily_Cases_MX <- c(aux3$people_fully_vaccinated.x[1], (aux3$people_fully_vaccinated.x[-1]-aux3$people_fully_vaccinated.x[-nrow(aux3)])) / aux3$population.x
aux3$Daily_Cases_US <- c(aux3$people_fully_vaccinated.y[1], (aux3$people_fully_vaccinated.y[-1]-aux3$people_fully_vaccinated.y[-nrow(aux3)])) / aux3$population.y


aux3[is.na(aux3)] <- 0
#aux3$Cases_MX <- ES(aux3$Daily_Cases_MX, 0.1)
#aux3$Cases_US <- ES(aux3$Daily_Cases_US, 0.1)
aux3 <- aux3 %>% filter(!is.na(Cases_US))

ggplot(aux3)+
  geom_line(aes(x=as.Date(date),Cases_MX*1e5,color="Mexico"))+
  geom_line(aes(x=as.Date(date),Cases_US*1e5,color="US"))


############# datasets

# confirmed cases
confirmed<-aux%>%
  mutate(value=Daily_Cases_MX/Daily_Cases_US)%>%
  select(date,value)

# deaths cases
deaths<-aux2%>%
  mutate(value=Daily_Cases_MX/Daily_Cases_US)%>%
  select(date,value)

# vaccinated
vaccinated<-aux3%>%
  mutate(value=Daily_Cases_MX/Daily_Cases_US)%>%
  select(date,value)

ggplot(confirmed)+
  geom_line(aes(x=as.Date(date),y=value))

ggplot(deaths)+
  geom_line(aes(x=as.Date(date),y=value))

ggplot(vaccinated)+
  geom_line(aes(x=as.Date(date),y=value))

# save files
write.csv(confirmed,"C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/confirmed_index.csv",row.names = F)
write.csv(deaths,"C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/deaths_index.csv",row.names = F)
write.csv(vaccinated,"C:/Users/Ulises/Desktop/Columbia University/Cursos/Fall 21/EDAV/Project/covidUSMexico/Data/vaccinated_index.csv",row.names = F)
