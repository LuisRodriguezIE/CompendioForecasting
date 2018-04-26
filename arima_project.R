#ARIMA and R: Stock Price Forecasting
#https://www.youtube.com/watch?v=N_XKJqr-VT4

# Modelo autorregresivo integrado de media movil.
# Función de autocorrelacion y autocorrelacion parcial
# Prueba de Dickey-Fuller
# Prueba de Ljung-Box

#Prediccion de valores financieros. 


rm(list=ls()) 

library(MASS)
library(tseries)
library(forecast)


#Definir el directorio de trabajo
setwd("C:/Users/lr_29/Desktop/Big Data Goal/R/Proyectos R Studio/Arima_stock")
#Informacion de la compañia Johnson & Johnson (JNJ) 2006-2016.
mydata<-read.csv(file="jnj.csv",header=TRUE)
View(mydata)
attach(mydata)

plot(Date,type="l",col="red")
plot(price,type="l",col="blue")

#Informacion de entrenamiento, transformacion logaritmica. 
lnstock<-log(price[1:96])
lnstock

#Grafica de correlacion
prcorr<-acf(lnstock, lag.max = 20)
prcorr

#Grafica de autocorrelacion
pracor<-pacf(lnstock, lag.max = 20)
pracor

#Obtener las diferencias entre cada muestra
Dlnstock<-diff(lnstock,1)
Dlnstock

#Prueba de Dickey Fuller para determinar si la funcion es estatica o no estatica.
adf.test(lnstock)
adf.test(Dlnstock)

#Definicion de la serie de tiempo.
precioArim<-ts(lnstock, start = c(2006,09), frequency =12)
precioArim
Ajustesto<-auto.arima(precioArim)
Ajustesto
plot(precioArim,type="l",col="blue",main="Precio JNJ")
AjusExp<-exp(lnstock)
AjusExp
plot(AjusExp)

#Prediciones empleando ARIMA
valorespredecidos<-forecast(Ajustesto,h=26)
valorespredecidos
plot(valorespredecidos,type = "l",col="gold",main="Prediccion ARIMA")

prediccionextraida<-as.numeric(valorespredecidos$mean)
prediccionextraida
valorespredecidosfinales<-exp(prediccionextraida)
valorespredecidosfinales

#Evaluar el porcentaje de error usando el conjunto de prueba. 
dfprecio<-data.frame(price[96:121],valorespredecidosfinales)
titulos<-c("Precio real","Precio estimado")
#Asignar los encabezados
names(dfprecio)<-titulos
View(dfprecio)
attach(dfprecio)

error_perce=((`Precio real`- `Precio estimado`)/(`Precio real`))
error_perce
mean(error_perce)

Box.test(Ajustesto$residuals, lag=5,type = "Ljung-Box")
Box.test(Ajustesto$residuals, lag=10,type = "Ljung-Box")
Box.test(Ajustesto$residuals, lag=15,type = "Ljung-Box")


