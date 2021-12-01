# Contains COVID19 data sets from many countries
library(COVID19)
library(tidyverse)

# Load data sets of Mexico and US
mex<- covid19(country="MX")
usa<- covid19(country="US")

# percentage of NA
mex_nas<-lapply(setNames(names(mex),names(mex)),function(x){
  sum(!is.na(mex[,x]))
})

mex_nas<-data.frame(var=names(mex_nas),Non_missing=unlist(mex_nas),row.names = NULL)


usa_nas<-lapply(setNames(names(usa),names(usa)),function(x){
  sum(!is.na(usa[,x]))
})

usa_nas<-data.frame(var=names(usa_nas),Non_missing=unlist(usa_nas),row.names = NULL)

compara<-usa_nas%>%
  left_join(mex_nas,by = "var",suffix = c("_usa","_mx"))

# Variables of interest:
variables<-read.csv("Data/COVID_vars_description.csv")

# Filter data
mex<- mex%>%
  select(variables$Variable[variables$Usar==1])

usa<- usa%>%
  select(variables$Variable[variables$Usar==1])

# Save data
write.csv(mex,"Data/COVID_mex.csv",row.names = F)
write.csv(usa,"Data/COVID_usa.csv",row.names = F)

