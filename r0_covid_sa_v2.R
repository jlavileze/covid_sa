library(utils)
library(httr)
library(tidyr)
library(dplyr)
library("jsonlite")
library(splitstackshape)
library(maditr) ##for the "dcast()" function
library(R0)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into ?R?. The dataset will be called "data".
ecdc <- read.csv(tf)

#the country Nambia is coded as NA, hence we need to recode it different
#First make the variable a character (in factor I would've need to add another level)

ecdc=ecdc %>% 
  mutate(
    geo.id=as.character(geoId),
    dateRep=as.Date(dateRep,format="%d/%m/%Y"),
    countries=countriesAndTerritories)%>%
  replace_na(list(geo.id="Nam"))%>%
  select(-countriesAndTerritories,-geoId)

#load the data to assign continents



json_file <- 'https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
    print(data)
  }
} 

#make arrangement to join the databases
data=data %>% 
  mutate(cont_id=as.character(Continent_Code),
         geo.id=as.character(Two_Letter_Country_Code))%>%
  replace_na(list(cont_id="NrA", geo.id="Nam"))%>%
  select(Continent_Name,cont_id,geo.id)

#Join both dataset with the two-letter codes of the countries

cd=plyr::join(data, ecdc, by=c("geo.id"))
cd=na.omit(cd)

#need to detach the "plyr" package to use the summarize function of dplyr
detach("package:plyr")
#



cd=cd%>%
  arrange(Continent_Name, countries, dateRep)

# Eliminate databases to save memory space
rm(ecdc,data) 

# Get the cumulative sum for each country and remove unobserved cases

cd=cd%>%
  group_by(geo.id)%>%
  mutate(cumsu.c=cumsum(cases),
         cumsu.d=cumsum(deaths))%>%
  filter(cumsu.c!=0)


# Assing an id that would be a proxy for number of days since the outbreak
cd=getanID(cd,id.vars=c("countries"))

## Extract the cases for South America with at least 14 days of from the pt zero


sa.cs=cd%>%
  filter(cont_id=="SA")%>%
  filter(countries!="Suriname",countries!="Guyana")%>%
  mutate(
    countries.f=countries,
    countries.f=factor(countries.f,levels=c("Brazil","Chile","Ecuador","Argentina",
                                            "Peru","Colombia","Paraguay","Bolivia","Uruguay","Venezuela")))
View(sa.cs)
names(sa.cs)
sa.log=sa.cs%>%
  mutate(log.c=log10(cumsu.c))%>%
  select(countries.f,.id,log.c,cumsu.c)
View(sa.log)


############ R0 estimation functions ##############

filter.country <- function(df, country.name) {
  df2<- df%>%
    filter(countries==country.name)
  return(df2)
}

covid.estimate.r0 <- function (epid.country, CDC.GT = FALSE, end.date = 13) {
  # Use as inition conditions GT ~ CDC(GT)
  # End date is a parameter only if CDC.GT = FALSE
  gen <- generation.time(type=c("gamma"), c(mean=3.94,3.75))
  if (CDC.GT) {
    res <- estimate.R(epid.country,GT=gen,methods=c("ML"), end=13)
  }
  else {
    res <- estimate.R(epid.country,GT=gen,methods=c("ML"), end=end.date, unknown.GT=TRUE)
  }
  return(res)
}

covid.estimate.SA <- function(df, CDC.GT = FALSE) {
  countries <- unique(df$countries)
  output <- vector(mode="list", length=length(countries))
  for (country in countries) {
    print(country)
    country.epid <- filter.country(df, country)
    output[[country]] <- covid.estimate.r0(country.epid$cases, CDC.GT)
  }
  return(output)
}

SA.r0 <- covid.estimate.SA(sa.cs, TRUE)

## To-do: sensitivity analysis to R0 estimation period

