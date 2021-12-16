
#####Modelos para ajuste de evolucion del Covid#####

#####Obtencion de datos#####

colors <- c("Real" = "blue", "Estimado" = "red")

#Seleccionamos datos de un pais (Usaremos datos de defunciones)

ItalyCovid <- Covid %>% 
  filter(countriesAndTerritories=="Italy") %>% 
  arrange(month,day,year) %>% mutate(dateRep=as.Date(dateRep,"%d/%m/%Y"))

ItalyCovid %>% 
  filter(year == '2020') %>% 
  ggplot() + 
  geom_line(aes(dateRep,deaths)) +
  ggtitle("Defunciones por Covid-19 en Italia (2020)")

#Seleccionamos datos de primera ola

ItalyCovid_Ola1 <- ItalyCovid %>% filter(month<=5) %>% filter(month>=3)

ItalyCovid_Ola1 %>% 
  filter(year == '2020') %>% 
  ggplot() + 
  geom_line(aes(dateRep,deaths)) +
  geom_point(aes(dateRep,deaths))+
  ggtitle("Defunciones por Covid-19 en Italia en la primera ola (2020)") +
  labs(x = "Fecha", y = "Defunciones", color = "Legend") +
  scale_colour_manual(name="Leyenda",values=colors)

#####Seleccion de modelo en base a resultados en primera ola#####

#####Regresion polinomica#####

t<-seq(from=1,to=dim(ItalyCovid_Ola1)[1],by=1)
t2<-t*t
ItalyCovid_Ola1<-tibble(ItalyCovid_Ola1,t,t2) #Primera ola

model <- lm(data=ItalyCovid_Ola1,deaths ~ t+t2) 
summary(model)
confint(model, level=0.95) #intervalos de confianza
plot(fitted(model),residuals(model),col="blue",xlab="Estimacion",ylab="Error",main="Estimacion vs error")
estimados<-predict(model)
ItalyCovid_Ola1 <- tibble(ItalyCovid_Ola1,estimados_parabola=estimados)

#grafico con los valores estimados

ggplot(data = ItalyCovid_Ola1) +
  geom_line(mapping = aes(x = dateRep, y = deaths),col='blue')+
  geom_line(aes(y=estimados_parabola,x=dateRep),col='red',linetype='dashed')+
  labs(x = "Fecha" , y = "Casos diarios" ) +
  ggtitle("Estimacion lineal defunciones por Covid-19 en Italia",subtitle='Primera ola') +
  labs(x = "Fecha", y = "Defunciones", color = "Legend") 
  

acumulado<-cumsum(estimados)

ItalyCovid_Ola1 <- ItalyCovid_Ola1 %>% 
  mutate(Acumulados=cumsum(deaths)) %>%
  mutate(Acumulados_est_P=cumsum(estimados_parabola))

#Casos acumulados y su estimacion parabolica

ItalyCovid_Ola1 %>% 
  ggplot() +
  geom_line(aes(dateRep,Acumulados),col='blue') +
  geom_line(aes(dateRep,Acumulados_est_P),col='red',linetype='dashed') +
  ggtitle("Estimacion parabolica de casos acumulados",subtitle="Italia primera ola") +
  labs(x = "Fecha", y = "Defunciones", color = "Legend")

#Error

ItalyCovid_Ola1=ItalyCovid_Ola1 %>% mutate(resid=Acumulados-Acumulados_est_P)

#MAPE

MAPE(ItalyCovid_Ola1$Acumulados,ItalyCovid_Ola1$Acumulados_est_P)

#####Prophet#####

#Estimacion del modelo

#Renombramos la tabla de datos y sus variables
train_daily <- ItalyCovid_Ola1 %>% 
  rename(
    ds = "dateRep",
    y = "Acumulados",
  )

train_daily$cap <- 35000 #Capacidad de carga
train_daily$floor<-0

model_prophet <- prophet(growth = "logistic",yearly.seasonality=TRUE) %>%
  fit.prophet(train_daily)

#Prediccion

model_prophet$cap <- 35000 #Capacidad de carga
model_prophet$floor <- 0

future <- make_future_dataframe(model_prophet, periods = 3)

future$cap <- 35000
future$floor<- 0

forecast_prophet <- predict(model_prophet, future)

prophet_plot_components(model_prophet, forecast_prophet)

plot(model_prophet,forecast_prophet)

plot(model_prophet,predict(model_prophet))

#Validacion

model_prophet2 <- prophet(growth = "logistic",yearly.seasonality=TRUE) %>%
  fit.prophet(train_daily[1:60,])


future2 <- make_future_dataframe(model_prophet2, periods = 30)

future2$cap <- 35000
future2$floor<- 0
forecast_prophet2 <- predict(model_prophet2, future2)
plot(model_prophet2,forecast_prophet2)

#####Logistica con nls()#####

logical <- nls(Acumulados ~ SSlogis(t, phi1, phi2, phi3), data = ItalyCovid_Ola1)
summary(logical)
predlol<-predict(logical)

ggplot(data = ItalyCovid_Ola1) +
  geom_line(mapping = aes(x = dateRep, y = Acumulados),col="blue")+
  geom_line(aes(y=predlol,x=dateRep),col="red",linetype='dashed')+
  ggtitle('Estimacion logistica de casos acumulados',subtitle = 'Italia primera ola')+
  labs(x = "Fecha" , y = "Casos" ) +
  labs(x = "Fecha", y = "Defunciones", color = "Legend")

#Validacion 
logical2 <- nls(Acumulados ~ SSlogis(t, phi1, phi2, phi3), data = ItalyCovid_Ola1[1:60,])
predlol2<-tibble(pred=predict(logical2,h=30),ItalyCovid_Ola1[1:60,]%>%select(dateRep))

ggplot() +
  geom_line(data = ItalyCovid_Ola1,mapping = aes(x = dateRep, y = Acumulados), color = "blue")+
  geom_line(data=predlol2,aes(y=pred,x=dateRep), color = "red",linetype='dashed')+
  labs(x = "Fecha" , y = "Casos" )

#MAPE

MAPE(ItalyCovid_Ola1$Acumulados,predlol)

#####Red Neuronal##### 

fitnn <- nnetar(ItalyCovid_Ola1$Acumulados,repeats=100)
plot(forecast(fitnn,h=10))
accuracy(fitnn)

ggplot(data = ItalyCovid_Ola1) +
  geom_line(mapping = aes(x = dateRep, y = Acumulados), color = "blue",alpha=0.6)+
  geom_line(aes(y=predict(fitnn)$fitted,x=dateRep), color = "red",linetype='dashed')+
  ggtitle('Estimacion red neuronal de casos acumulados',subtitle = 'Italia primera ola')+
  labs(x = "Fecha" , y = "Casos" )

#Validacion

fitnn2 <- nnetar(ItalyCovid_Ola1$Acumulados[1:60],repeats=100)
autoplot(forecast(fitnn2,h=32))
accuracy(fitnn2)
prednn<-tibble(pred=predict(fitnn2,h=32)$fitted,ItalyCovid_Ola1[1:60,]%>%select(dateRep))

ggplot() +
  geom_line(data = ItalyCovid_Ola1,mapping = aes(x = dateRep, y = Acumulados), color = "blue")+
  geom_line(data=prednn,aes(y=pred,x=dateRep), color = "red",linetype='dashed')+
  labs(x = "Fecha" , y = "Casos" )

plot(forecast(fitnn2,h=50))

#####Comparacion MAPE#####

#Regresion Polinomica

MAPE(ItalyCovid_Ola1$Acumulados,ItalyCovid_Ola1$Acumulados_est_P)

#Regresion logistica

MAPE(ItalyCovid_Ola1$Acumulados,predlol)

#Red Neuronal

accuracy(fitnn)

