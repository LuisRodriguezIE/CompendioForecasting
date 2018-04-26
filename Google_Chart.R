rm(list=ls()) 

#Si es necesario
#install.packages("googleVis")

library(googleVis) 

#getWorldBankData <- function(id='SP.POP.TOTL', date='1960:2010',
getWorldBankData <- function(id='SP.POP.TOTL', date='1960:2015',
                             value="value", per.page=15000){     #per.page=14000
  require(RJSONIO)
  url <- paste("http://api.worldbank.org/countries/all/indicators/", id,
               "?date=", date, "&format=json&per_page=", per.page,
               sep="")
  
  wbData <- fromJSON(url)[[2]]
  
  wbData = data.frame(
    year = as.numeric(sapply(wbData, "[[", "date")), 
    value = as.numeric(sapply(wbData, function(x)
      ifelse(is.null(x[["value"]]),NA, x[["value"]]))),  
    country.name = sapply(wbData, function(x) x[["country"]]['value']),
    country.id = sapply(wbData, function(x) x[["country"]]['id'])    
  )
  
  names(wbData)[2] <- value
  
  return(wbData)
}



## OK - that above is the function that calls DATA (JSON format) from the 
## world bank APIs that have been exposed to make info available - 
## when called, you'll see the variables populate

getWorldBankCountries <- function(){
  require(RJSONIO)
  wbCountries <-
    fromJSON("http://api.worldbank.org/countries?per_page=15000&format=json") # per_page=14000&format=json
  wbCountries <- data.frame(t(sapply(wbCountries[[2]], unlist)))
  wbCountries$longitude <- as.numeric(wbCountries$longitude)
  wbCountries$latitude <- as.numeric(wbCountries$latitude)
  levels(wbCountries$region.value) <- gsub(" \\(all income levels\\)",
                                           "", levels(wbCountries$region.value))
  return(wbCountries)
}
### Obtener los paises, su clasificacion y su localizacion. 

## Create a string 1960:this year, e.g. 1960:2011
#years <- paste("1960:", format(Sys.Date(), "%Y"), sep="")
#Ano hasta el que se desea analizar
years <- paste("1960:", "2015", sep="")
## this just makes a string that says "1960:2014" - that's it
years

## Fertility rate, Indice de fertilidad
fertility.rate <- getWorldBankData(id='SP.DYN.TFRT.IN',
                                   date=years, value="Tasa de fertilidad")
## calls the function iwth instructions to pull fertility data

## Life Expectancy
life.exp <- getWorldBankData(id='SP.DYN.LE00.IN',  date=years,
                             value="Esperanza de vida") 
##calls function to get hte life expectancy (same function, different id to API)


## Population
population <- getWorldBankData(id='SP.POP.TOTL',  date=years,
                               value="Poblacion")
### and population - again, same funciton, different ID, different query, different data returns (12k-15k obs)


## GDP per capita (current US$)
GDP.per.capita <- getWorldBankData(id='NY.GDP.PCAP.CD',
                                   date=years,
                                   value="PIB per capita en USD") 
## and one more trip to the API to get the GDP data

## Merge data sets
wbData <- merge(life.exp, fertility.rate)
wbData <- merge(wbData, population)
wbData <- merge(wbData, GDP.per.capita)
## RA > I'm not sure if merge requres the left hand column to be same across all sets 
## like a KEY - and if needs to be in same order, but suggest checking/testing/researching
## if you are hacking your own data in here
head(wbData)
dim(wbData)

## Get country mappings
wbCountries <- getWorldBankCountries()
head(wbCountries)
dim(wbCountries)
## returns a BEAUTIFUL key:
## header id  iso2Code    name    region.id    region.value    adminregion.id    adminregion.value    incomeLevel.id    incomeLevel.value    lendingType.id    lendingType.value    capitalCity    longitude    latitude
## 1st row 1    ABW    AW    Aruba    LCN    Latin America & Caribbean              NOC    High income: nonOECD    LNX    Not classified    Oranjestad    46    57

## Add regional information
wbData <- merge(wbData, wbCountries[c("iso2Code", "region.value", #region.value
                                      "incomeLevel.value")],      #incomeLevel.value
                by.x="country.id", by.y="iso2Code")
## here is magic of merge

head(wbData)
dim(wbData)


## Filter out the aggregates and country id column
subData <- subset(wbData, !region.value %in% "Aggregates" , select=-country.id) 
## SUBDATA is only 9.9k long, rather than 12k (filtered Aggregates) // 12k may not be enougn anymore

## Create a motion chart!!!!!!!!!!!!!! (make sure you save first!)
M <- gvisMotionChart(subData, idvar="country.name", timevar="year",options=list(width=700, height=600))
## using SubData that looks like this:
# id  iso2Code    name    region.id    region.value    adminregion.id    adminregion.value    incomeLevel.id    incomeLevel.value    lendingType.id    lendingType.value    capitalCity    longitude    latitude
# 1    ABW    AW    Aruba    LCN    Latin America & Caribbean              NOC    High income: nonOECD    LNX    Not classified    Oranjestad    46    57
# 2    AFG    AF    Afghanistan    SAS    South Asia    SAS    South Asia    LIC    Low income    IDX    IDA    Kabul    193    120
# 3    AFR    A9    Africa    NA    Aggregates              NA    Aggregates         Aggregates         1    1

## Wont "do" anything except prepare the data in "M" for the plot

## Display the chart in your browser
plot(M)

## awesome!

## SOURCE: http://lamages.blogspot.co.uk/2011/09/accessing-and-plotting-world-bank-data.html
## ORIGiNAL SOURCE: Posted by Markus Gesmann 
## comments and a few tweaks by Ryan Anderson www.dreamtolearn.com 