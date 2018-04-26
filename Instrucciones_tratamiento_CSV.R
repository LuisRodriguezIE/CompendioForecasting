#Cargar la informacion.

setwd("C:/Users/lr_29/Desktop/Big Data Goal/R/Proyectos R Studio/Stock Prices")
data<-read.csv("EURUSD60.csv",header = FALSE)
DATA<-data
str(DATA)
sapply(DATA, class)
DATA$V8 <- paste(DATA$V1,DATA$V2)
DATA$V1<-NULL
DATA$V2<-NULL
DATA<-DATA[c(6,1,2,3,4,5)]
colnames(DATA)<-c("Date","Open","High","Low","Close","Volume")
write.csv(DATA, file = "EURUSD60_5.csv")