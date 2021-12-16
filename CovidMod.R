
#Modelos covid

#Librerias

library(readxl)
library(stats)
library(ISLR)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(prophet)
library(TSdata)
library(mosaic)
library(ZeBook)
library(tstools)
library(RSNNS)
library(neuralnet)
library(caret)
library(zoo)
library(tidyverse)
library(seasonal)
library(fpp2) #datos


#Datos

CovidData <- read_excel("C:/Users/98pjo/OneDrive/Escritorio/Covid/CovidData.xlsx")

summary(CovidData)

ggplot(data = CovidData) +
  geom_line(mapping = aes(x = dateRep, y = cases), color = "blue")

#Outliers

maha.F <- mahalanobis(CovidData[,c(-1,-3,-5,-6)], 
                      center = colMeans(CovidData[,c(-1,-3,-5,-6)]),
                      cov = cov(CovidData[,c(-1,-3,-5,-6)])) #colmeans es la media por columnas (variables)

plot(maha.F, pch=22, col="blue",main="Outliers")
text(x=1:length(CovidData$dateRep),y=maha.F, 1:length(CovidData$dateRep), pos=3, col="black")
abline(h=7*mean(maha.F),col="red")
quitar<-which(maha.F>7*mean(maha.F))
quitar
CovidData<-CovidData[-quitar,]
CovidData<-CovidData[-quitar,]

ggplot(data = CovidData) +
  geom_line(mapping = aes(x = dateRep, y = cases), color = "blue")+
  geom_smooth(mapping = aes(x = dateRep, y = cases), color = "blue")

#Regresion polinomica#

t<-seq(from=57,to=130,by=1)
t2<-t*t
CovidData2<-data.frame(CovidData[57:130,],t,t2) #Primera ola

model <- lm(CovidData$cases[57:130] ~ t+t2) 
summary(model)
confint(model, level=0.95) #intervalos de confianza
plot(fitted(model),residuals(model),col="blue",xlab="Estimacion",ylab="Error",main="Estimacion vs error")
estimados<-fitted(model)
estimados<-ifelse(estimados<0,yes=0,no=estimados)
plot(estimados,residuals(model),col="blue",xlab="Estimacion",ylab="Error",main="Estimacion vs error")
predicciones<-predict(model)

#grafico con los valores estimados

ggplot(data = CovidData[57:130,]) +
  geom_line(mapping = aes(x = dateRep, y = cases), color = "blue")+
  geom_smooth(mapping = aes(x = dateRep, y = cases), color = "blue")+
  geom_line(aes(y=estimados,x=dateRep), color = "red")+
  labs(x = "Fecha" , y = "Casos diarios" )

acumulado<-cumsum(estimados)

datos2<-as.data.frame(cbind(estimados,acumulado))

plot(CovidData$dateRep[57:130],CovidData$casesA[57:130],xlab="Fecha",
     ylab="Casos",main="Casos acumulados de Covid")
lines(CovidData$dateRep[57:130],datos2$acumulado,col='red',lwd=2) #grafico valores estimados

#Prophet

#Transformamos en fecha(Date)
CovidData <- CovidData %>% 
  mutate(dateRep = ymd(dateRep))

#Estimacion del modelo

# Renombramos la tabla de datos y sus variables
train_daily <- CovidData[57:130,] %>% 
  rename(
    ds = "dateRep",
    y = "casesA",
  )

train_daily$cap <- 250000 #Capacidad de carga
train_daily$floor<-0

model_prophet <- prophet(growth = "logistic",yearly.seasonality=TRUE) %>%
  fit.prophet(train_daily)

#Prediccion

model_prophet$cap <- 250000 #Capacidad de carga
model_prophet$floor <- 0

future <- make_future_dataframe(model_prophet, periods = 3)

future$cap <- 250000
future$floor<- 0

forecast_prophet <- predict(model_prophet, future)

prophet_plot_components(model_prophet, forecast_prophet)

plot(model_prophet,forecast_prophet)
plot(model_prophet,predict(model_prophet))

#Validacion

model_prophet2 <- prophet(growth = "logistic",yearly.seasonality=TRUE) %>%
  fit.prophet(train_daily[1:30,])


future2 <- make_future_dataframe(model_prophet2, periods = 50)

future2$cap <- 250000
future2$floor<- 0
forecast_prophet2 <- predict(model_prophet2, future2)
plot(model_prophet2,forecast_prophet2)


#Logistica con nls()

logical <- nls(casesA ~ SSlogis(t, phi1, phi2, phi3), data = CovidData2)
summary(logical)
predlol<-predict(logical)

ggplot(data = CovidData2) +
  geom_line(mapping = aes(x = dateRep, y = casesA), color = "blue")+
  geom_line(aes(y=predlol,x=dateRep), color = "red")+
  labs(x = "Fecha" , y = "Casos" )

#Validacion 
logical2 <- nls(casesA ~ SSlogis(t, phi1, phi2, phi3), data = CovidData2[1:30])
predlol2<-predict(logical2,h=50)

ggplot(data = CovidData2) +
  geom_line(mapping = aes(x = dateRep, y = casesA), color = "blue")+
  geom_line(aes(y=predlol2,x=dateRep), color = "red")+
  labs(x = "Fecha" , y = "Casos" )

#Red Neuronal 

fitnn <- nnetar(CovidData2$casesA,repeats=100)
plot(forecast(fitnn,h=10))
accuracy(fitnn)

plot(CovidData2$dateRep,CovidData2$casesA,xlab="Fechas",ylab="Casos Acumulados")
lines(CovidData2$dateRep,predict(fitnn)$fitted,col="red")

#Validacion

fitnn2 <- nnetar(CovidData2$casesA[1:30],repeats=100)
plot(forecast(fitnn2,h=50))
accuracy(fitnn2)

#Segunda Ola

#Datos

CovidDataS2<-data.frame(CovidData$dateRep[178:333],CovidData$cases[178:333])
names(CovidDataS2)<-c("dateRep","cases")

casesA<-cumsum(CovidDataS2$cases)

CovidDataS2<-data.frame(CovidDataS2,casesA)

ggplot(data = CovidDataS2) +
  geom_line(mapping = aes(x = dateRep, y = casesA), color = "blue")

ggplot(data = CovidDataS2) +
  geom_line(mapping = aes(x = dateRep, y = cases), color = "blue")+
  geom_smooth(mapping = aes(x = dateRep, y = cases), color = "blue")

#Outliers

maha.F2 <- mahalanobis(CovidDataS2[,-1], 
                      center = colMeans(CovidDataS2[,-1]),
                      cov = cov(CovidDataS2[,-1])) #colmeans es la media por columnas (variables)

plot(maha.F2, pch=22, col="blue",main="Outliers")
text(x=1:length(CovidDataS2$dateRep),y=maha.F2, 1:length(CovidDataS2$dateRep), pos=3, col="black")
abline(h=7*mean(maha.F2),col="red")
quitar2<-c(50,57,64,71,78,85,92,106,99)
quitar2
CovidDataS2<-CovidDataS2[-quitar2,]

ggplot(data = CovidDataS2) +
  geom_line(mapping = aes(x = dateRep, y = casesA), color = "blue")

#Regresion polinomica#

t<-seq(from=1,to=147,by=1)
t2<-t*t

model2 <- lm(CovidDataS2$cases ~ t+t2) 
summary(model2)
confint(model2, level=0.95) #intervalos de confianza
plot(fitted(model2),residuals(model2),col="blue",xlab="Estimacion",ylab="Error",main="Estimacion vs error")
estimados2<-fitted(model2)
estimados2<-ifelse(estimados2<0,yes=0,no=estimados2)
plot(estimados2,residuals(model2),col="blue",xlab="Estimacion",ylab="Error",main="Estimacion vs error")
predicciones2<-predict(model2)

#grafico con los valores estimados

ggplot(data = CovidDataS2) +
  geom_line(mapping = aes(x = dateRep, y = cases), color = "blue")+
  geom_smooth(mapping = aes(x = dateRep, y = cases), color = "blue")+
  geom_line(aes(y=estimados2,x=dateRep), color = "red")+
  labs(x = "Fecha" , y = "Casos diarios" )

acumulado2<-cumsum(estimados2)

datos22<-as.data.frame(cbind(estimados2,acumulado2))

plot(CovidDataS2$dateRep,CovidDataS2$casesA,xlab="Fecha",
     ylab="Casos",main="Casos acumulados de Covid")
lines(CovidDataS2$dateRep,datos22$acumulado2,col='red',lwd=2) #grafico valores estimados

#Pico

CovidDataS2<-data.frame(CovidDataS2,estimados2)
CovidDataS2[which.max(estimados2),1]

#Prophet

#Transformamos en fecha(Date)
CovidDataS2 <- CovidDataS2 %>% 
  mutate(dateRep = ymd(dateRep))

#Estimacion del modelo

# Renombramos la tabla de datos y sus variables
train_daily2 <- CovidDataS2 %>% 
  rename(
    ds = "dateRep",
    y = "casesA",
  )

train_daily2$cap <- 1500000 #Capacidad de carga
train_daily2$floor<-0

model_prophet2 <- prophet(growth = "logistic",yearly.seasonality=TRUE) %>%
  fit.prophet(train_daily2)

#Prediccion

model_prophet2$cap <- 1500000 #Capacidad de carga
model_prophet2$floor <- 0

future2 <- make_future_dataframe(model_prophet2, periods = 60)

future2$cap <- 1500000
future2$floor<- 0

forecast_prophet2 <- predict(model_prophet2, future2)

prophet_plot_components(model_prophet2, forecast_prophet2)

plot(model_prophet2,forecast_prophet2)
plot(model_prophet2,predict(model_prophet2))

#Pico

#Casos diarios
CD2<-forecast_prophet2$trend-lag(forecast_prophet2$trend,1)
max2<-which.max(CD2)
forecast_prophet2[max2,1]

#Logistica con nls()

logicalS2 <- nls(casesA ~ SSlogis(t, phi1, phi2, phi3), data = CovidDataS2)
summary(logicalS2)
predlolS2<-predict(logicalS2)

ggplot(data = CovidDataS2) +
  geom_line(mapping = aes(x = dateRep, y = casesA), color = "blue")+
  geom_line(aes(y=predlolS2,x=dateRep), color = "red")+
  labs(x = "Fecha" , y = "Casos" )

#Pico

#Casos diarios
CD1<-predlolS2-lag(predlolS2,1)
max1<-which.max(CD1)
CovidDataS2<-data.frame(CovidDataS2,CD1)
CovidDataS2[max1,1]
