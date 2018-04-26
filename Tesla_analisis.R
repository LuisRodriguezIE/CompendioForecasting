rm(list=ls())
library(xts)
library(zoo)
library(quantmod)
library(tidyr)
library(PerformanceAnalytics)


TSLA<-getSymbols("TSLA",src="yahoo",auto.assign = F)
TSLA<-dailyReturn(Ad(TSLA))
View(TSLA)
charts.PerformanceSummary(TSLA)

AZTK<-getSymbols("AZTECACPO.MX",src="yahoo",auto.assign = F)
AZTK<-dailyReturn(Ad(AZTK))
charts.PerformanceSummary(AZTK)

TELE<-getSymbols("TLEVISACPO.MX",src="yahoo",auto.assign = F)
TELE<-dailyReturn(Ad(TELE))
charts.PerformanceSummary(TELE)

colnames(AZTK)<-c("Aztk_d")
colnames(TELE)<-c("Tele_d")

head(AZTK)
head(TELE)

dim(AZTK)
dim(TELE)

m_info<-merge(AZTK,TELE,all = F)
dim(m_info)
head(m_info)
charts.PerformanceSummary(m_info,main = "Comparacion")


#Sharpe Ratio Medida de riesgo-ajuste Mayor es mejor.
#Sharpe Ratio=(Expected Return-Risk Free Rate)/Standar Dev of returns
#Sharpe=(E[R]-Rf)/Sigma

#Numero de días comerciales en un año
table.AnnualizedReturns(m_info,scale = 252,Rf=0.005/252)
