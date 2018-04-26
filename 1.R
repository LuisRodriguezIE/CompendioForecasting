#QuantBros.com Introduction to R Programming for Financial Timeseries
#https://www.youtube.com/watch?v=92zCRV3eQxw&t=19s


#Programming finance, Quantomod.
#Basics
#DataFrame
#Quantum package
#Reading finance data.
#Vector in R
#Ploting
#CSV FILES

rm(list=ls())
library(xts)
library(zoo)
library(quantmod)
library(tidyr)

##################################################
df<-data.frame(getSymbols("AAPL",src="yahoo",auto.assign = F))
View(df)
getwd()
setwd("C:/Users/lr_29/Desktop/Big Data Goal/R/Proyectos R Studio")
getwd()
write.csv(df, file = "APPL_stock.csv")
head(df)
tail(df)




apple_df<-read.csv(file="APPL_stock1.csv",header=TRUE)
View(apple_df)
#comprobar si las columnas estan en  formato fecha 
sapply(apple_df, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))











