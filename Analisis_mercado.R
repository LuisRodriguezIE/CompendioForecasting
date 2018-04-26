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
setwd("C:/Users/lr_29/Desktop/Big Data Goal/R/Proyectos R Studio/Stock Prices")
head(df)
tail(df)
summary(df)
dim(df)
names(df)
sapply(df, class)
write.csv(df, file = "APPL_stock.csv")
sapply(df, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))

df1<-read.csv(file="APPL_stock1.csv",header=TRUE)
View(df1)
#comprobar si las columnas estan en  formato fecha 
sapply(df1, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))

APPL<-tail(df)
APPL
class(APPL)

#APPL[,c(1,4)]
colnames(APPL)<-c("Open","High","Low","Close","Volume","Adjusted")
#APPL
#APPL$Open
#APPL[,1]

APPL<-df
head(df)

colnames(APPL)<-c("Open","High","Low","Close","Volume","Adjusted")
colnames(APPL)
write.csv(APPL, file = "APPL-Data.csv")
data<-APPL
Cl(data)
plot(Cl(data),type = 'l')
plot(Ad(data),type = 'l',col="red")

length(Ad(data))

#Simple Return = (Current Price-Purchase Price) / Purchase Price
simret<-((Ad(data)[2816]-Ad(data)[1])/Ad(data)[1])*100
simret

#Calcular el retorno dimple diario.
dRet<-dailyReturn(Ad(as.xts(data)),type='arithmetic')
plot(dRet,type="l",col="green")

#Cumulative Return
cumProduct<-cumprod(1+dRet)
plot(cumProduct,type="l",col="blue")

#logarithmic Returns Adjusted data
LogRet<-diff(log(Ad(data)))
plot(LogRet,type = "l",col="gold")

#logarithmic Returns of Close data
plot(diff(log(Cl(data))),type = "l",col="navy")

#Conversion a escala logaritmica. 
#Log Returns. 
#Continuous Return=(1+Simple Returns)
#Continuous Return=diff(log(data))
logAdCl<-apply(apply(data[,c("Close","Adjusted")],2,log),2,diff)
compareAdtoCl<-data.frame(logAdCl)
View(compareAdtoCl)

plot(compareAdtoCl$Close,type = "l",col="red")
lines(compareAdtoCl$Adjusted,type = "l",col="blue")

#Cumulative logartihmic returns.
compAdtoClCumSum<-data.frame(apply(compareAdtoCl,2,cumsum))
View(compAdtoClCumSum)
plot(compAdtoClCumSum$Close,type = "l",col="gold")
lines(compAdtoClCumSum$Adjusted,type="l",col="green")

#Cumulative arithmetic return
plot(cumprod(1+dailyReturn(Ad(as.xts(data)),type="arithmetic")))
lines(cumsum(dailyReturn(Ad(as.xts(data)),type="log")),col="blue")

###############################################################
library(PerformanceAnalytics)

data<-as.xts(compareAdtoCl)
#Simple return
data<-exp(data)-1
View(data)
charts.PerformanceSummary(data,main="Close vs Adjusted Return")
