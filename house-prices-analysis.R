#R-Programming | Analyzing House Prices in King County, USA
#https://www.youtube.com/watch?v=Yz_Tzxkl-mI

rm(list=ls()) #Borrar todas las variables cargadas.

#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("dplyr")

library("ggplot2") #Create Elegant Data Visualisations Using the Grammar of Graphics.
library("ggmap") #Spatial Visualization with ggplot2.
library("dplyr") #A Grammar of Data Manipulation.

#House Price In King County, USA
house.data<-read.csv('C:/Users/lr_29/Desktop/Big Data Goal/R/Proyectos R Studio/PrecioCasas/kc_house_data.csv',header=TRUE) #Load dataset.
View(house.data) #Visualizar dataset.
summary(house.data) #Resumen de los datos en el dataset. 

dim(house.data)
nrow(house.data)
ncol(house.data)
head(house.data)

house.data<-house.data[,c(1,3:21)] #Elimina la variable "date" del dataset.

dim(house.data)
head(house.data)


house.data<-house.data[1:5000,] #Solo se toman los primeros 5000 datos del dataset. 
dim(house.data)

#Hacer disponible el nombre de las variables para el script
attach(house.data)

#1. Exploración de la informacion

# Ver la estructura de la informacion
glimpse(house.data)

#Ver un resumen de la informacion
summary(house.data)

#Hacer los precios en 100k
pricesin100k<-house.data$price/100000

#Distribución de los precios.
hist(pricesin100k,
     breaks = 10,
     main='Distribucion de los precios',
     xlab='Precios en 100k',
     ylab='Frecuencia',
     col='blue'
     )

# Distribucion numero de habitaciones.
hist(house.data$bedrooms,
     main='Distribucion de las habitaciones',
     xlab='Numero de habitaciones',
     ylab='Frecuencia',
     col='green'
)

# Distribucion condiciones de la casa.
hist(house.data$condition,
     main='Distribucion de las condiciones',
     xlab='Condicion de la casa',
     ylab='Frecuencia',
     col='yellow'
)

#Area del lote en 2015
squareFt<-house.data$sqft_lot15/100

#Precio por pie cuadrado
plot(y=pricesin100k,x=squareFt,main='Precio por pie cuadrado',
     xlab="Pies cuadrados",
     ylab="Precio en 100k",
     col='red')

plot(pricesin100k,bedrooms, main='Precio por habitacion',
     xlab='Precio en 100k',
     ylab='Numero de habitaciones',
     col='purple'
     )  


#Multiple Linear Regression (MLR) Model

#Creacion de un modelo base
house.model<-lm(price~.,data=house.data)

#Resumen del modelo
house.model
summary(house.model)
plot(house.model)

#Redondeo de los coeficientes de la tabla
coeffs<-summary(house.model)$coefficients
coeffs
coeffs<-round(coeffs,digits = 4)
coeffs

#Realizar una grafica de dispersion relacione año y precio casas.
plot(price~yr_built,
     data=house.data,
     cex=0.5,
     main='Precio por año',
     xlab='Año',
     ylab='Precio de la casa en 100k',
     col='red'
)

# 3. Anova Análisis de la Varianza
# Se quiere conocer si los precios promedios de las casas varian cada 25 años.
# Transformar el precio y año para convertirlo en decadas

priceByDecade<-data.frame(Price=house.data$price,Decade=house.data$yr_built) #Crear un subconjunto de datos.

min(priceByDecade$Decade)
max(priceByDecade$Decade)

hist(priceByDecade$Decade,
     breaks =15,
     main='Distribucion de casas por año',
     xlab='Decada de construccion',
     ylab='Cantidad',
     col=c('orange')
)

#Crear una cuenta cada 25 años
for(i in 1:5000){
  
  if(priceByDecade$Decade[i]<1925){
    priceByDecade$Decade[i]<-'1900-1925'
  }
  
  else if(priceByDecade$Decade[i]>1925 && priceByDecade$Decade[i]<1950){
    priceByDecade$Decade[i]<-'1925-1950'
  }
  
  else if(priceByDecade$Decade[i]>1950 && priceByDecade$Decade[i]<1975){
    priceByDecade$Decade[i]<-'1950-1975'
  }
  
  else if(priceByDecade$Decade[i]>1975 && priceByDecade$Decade[i]<2000){
    priceByDecade$Decade[i]<-'1975-2000'
  }
  
  else{
    priceByDecade$Decade[i]<-'2000 - actualidad'
  }
}

#Convert a column into a factor column, represent categorical data.
priceByDecade$Decade<-as.factor(priceByDecade$Decade)

#Con los años agrupado en factores de 25 años, se puede correr anova.
anova<-aov(Price ~ Decade, data=priceByDecade)

# Resumen de las estadisticas en Anova
summary(anova)

#Correr el test de Tukey en anova
TukeyHSD(anova)

#Grafica de las relaciones en ANOVA
plot(pricesin100k ~ Decade,
     data=priceByDecade,
     main='ANOVA precio ~ Cuarto de siglo',
     xlab='1900 - Presente',
     ylab='Precio en 100k',
     col=c('orange','blue','green','yellow','pink')
)


# 4. GGMaps
# Determinar el rango para CARO

#Eliminar en el tercer cuarto y superior
summary(house.data$price)

#Nueva tabla para las casas más caras 
mostExpensiveHouses<-house.data[house.data[,2]>650000,]
head(mostExpensiveHouses)

#Seleccion de las columnas necesarias
#Precio, Longuitudes y Latitudes
mostExpensiveHouses<-mostExpensiveHouses[,c(2,17,18)]
head(mostExpensiveHouses)

#Mapa de la zona King County
map<-get_map("King County, Washington",
             zoom=10,
             maptype='hybrid',
             source='google',
             color='bw'
             )

m<-ggmap(map) #ggmap plots the raster object produced by get_map.

#The point geom is used to create scatterplots.
m+geom_point(data=mostExpensiveHouses,
             aes(x=mostExpensiveHouses$long,
                 y=mostExpensiveHouses$lat),
                 color="green",
                 size=2,
                 alpha=0.2
                 )+labs(title='Casas mas caras en King County, Washington')
