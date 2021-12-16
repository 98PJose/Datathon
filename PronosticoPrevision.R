
#####PRONOSTICO Y PREVISION#####

##Obtencion de datos

#Precio tomate agricultor
 
PI_DF <- PreciosSemanalesCovid %>% 
  filter(PRODUCTO=="TOMATE") %>% 
  filter(POSICION=="Agricultor") %>%
  mutate(dateRep=as.Date(dateRep,"%d/%m/%Y")) %>% #Tomate 
  group_by(dateRep,DAY,MONTH,YEAR) %>% 
  summarise(price_mean=mean(PRECIO),deaths=mean(deaths)) %>% 
  arrange(YEAR,MONTH,DAY) %>% 
  mutate(deaths = replace(deaths, deaths < 0, 0)) #Quitamos muertes negativas

#Grafico

plot1<-PI_DF %>% 
  ggplot() + 
  geom_line(aes(dateRep,deaths)) +
  geom_point(aes(dateRep,deaths))+
  ggtitle("Defunciones por Covid-19 (2020)") +
  labs(x = "Fecha", y = "Precio", color = "Legend") +
  scale_colour_manual(name="Leyenda",values=colors)

plot2<-PI_DF %>% 
  ggplot() + 
  geom_line(aes(dateRep,price_mean)) +
  geom_smooth(aes(dateRep,price_mean),se=FALSE)+
  ggtitle("Precio Tomate") +
  labs(x = "Fecha", y = "Precio", color = "Legend") +
  scale_colour_manual(name="Leyenda",values=colors)

grid.arrange(plot1, plot2, ncol=1)

#Desestacionalizamos

PI_SEM<-PreciosSemanales %>% 
  filter(PRODUCTO=="TOMATE") %>% 
  filter(POSICION=="Agricultor") %>%
  group_by(YEAR,MONTH,DAY) %>% 
  summarise(price_mean=mean(PRECIO)) %>% 
  arrange(YEAR,MONTH,DAY)

PI_DF_SE<- PI_SEM[,4]%>%
  ts(start = c(2018,1), frequency = 12) 

STL_PI<-PI_DF_SE %>% mstl(robust=TRUE) 

autoplot(STL_PI) +
  ggtitle("Descomposicion STL precio tomate")

#Analisis de la estacionalidad

STL_PI[,3] %>% autoplot() #Estacionalidad

freq_PI <- periodogram(STL_PI[,3]) 

STL_PI_SE <- STL_PI[,2] + STL_PI[,4] #Datos sin estacionalidad trend + remainer

PI_SE_CE <- tibble(FILTRO(ComercioExterior,"Italy","53"),STL_PI_SE,est=STL_PI[,3]) %>% 
  inner_join(Covid_Media,by = c("REPORTER"="countriesAndTerritories","MONTH"="month","YEAR"="year")) %>%
  select(MONTH:REPORTER,STL_PI_SE,deaths,est)

####1.Modelos causales####

####1.1.Modelo lineal#####

mod_lineal <- lm(price_mean~deaths,data=PI_DF)

PI_DF<-tibble(PI_DF,est_lineal=predict(mod_lineal)) %>% 
  mutate(resid_lineal=(price_mean-est_lineal))

#grafico con los valores estimados

ggplot(data = PI_DF) +
  geom_line(mapping = aes(x = dateRep, y = price_mean),col='blue')+
  geom_line(aes(y=est_lineal,x=dateRep),col='red',linetype='dashed')+
  labs(x = "Fecha" , y = "Casos diarios" ) +
  ggtitle("Estimacion lineal precio tomate") +
  labs(x = "Fecha", y = "Precio", color = "Legend")

#Grafico error

plot(PI_DF$resid_lineal) #no es ruido aleatorio

#MAPE

MAPE(PI_DF$price_mean,predict(mod_lineal))

#####1.2.Modelo GAM####

mod_gam=gam(price_mean~s(deaths ,4) ,data=PI_DF)
#s() indica que queremos un suavizado con k grados de libertad

summary(mod_gam)


PI_DF<-tibble(PI_DF,est_gam=predict(mod_gam)) %>% 
  mutate(resid_gam=(price_mean-est_gam))

#grafico con los valores estimados

ggplot(data = PI_DF) +
  geom_line(mapping = aes(x = dateRep, y = price_mean),col='blue')+
  geom_line(aes(y=est_gam,x=dateRep),col='red',linetype='dashed')+
  labs(x = "Fecha" , y = "Casos diarios" ) +
  ggtitle("Estimacion GA precio tomate") +
  labs(x = "Fecha", y = "Precio", color = "Legend")

#MAPE

MAPE(PI_DF$price_mean,PI_DF$est_gam)

#Grafico error

plot(PI_DF$resid_gam)

#####1.3.Arbol de regresion####

mod_tree = tree(price_mean ~deaths,PI_DF )

summary (mod_tree)

plot(mod_tree)
text(mod_tree ,pretty =1)

#Podado

cv.tree =cv.tree(mod_tree)
plot(cv.tree$size ,cv.tree$dev ,type="b")

mod_tree_pruned =prune.tree(mod_tree ,best =4)
plot(mod_tree_pruned)
text(mod_tree_pruned ,pretty =0) #Sigue siendo el mismo

#Prediccion

est_tree = predict (mod_tree)

PI_DF <- tibble(PI_DF,est_tree) %>% mutate(resid_tree=price_mean-est_tree)
 
ggplot(data = PI_DF) +
  geom_line(mapping = aes(x = dateRep, y = price_mean),col='blue')+
  geom_line(aes(y=est_tree,x=dateRep),col='red',linetype='dashed')+
  labs(x = "Fecha" , y = "Casos diarios" ) +
  ggtitle("Estimacion arbol precio tomate") +
  labs(x = "Fecha", y = "Precio", color = "Legend")

#MAPE

MAPE(PI_DF$price_mean,PI_DF$est_tree)

#Grafico error

plot(PI_DF$resid_tree)

#####Modelos no causales####

#####2.1.ARIMA####

#Datos

PI_SEM<-PreciosSemanales %>% 
  filter(PRODUCTO=="TOMATE") %>% 
  filter(POSICION=="Agricultor") %>%
  group_by(YEAR,MONTH,DAY) %>% 
  summarise(price_mean=mean(PRECIO)) %>% 
  arrange(YEAR,MONTH,DAY) 

PI_SEM<-PI_SEM %>% 
  unite('dateRep',c(DAY,MONTH,YEAR)) %>% 
  mutate(dateRep =  str_replace(dateRep,"_","/")) %>%
  mutate(dateRep =  str_replace(dateRep,"_","/")) %>%
  mutate(dateRep=as.Date(dateRep,"%d/%m/%Y"))

#Estimacion

mod_arima <- stlf(PI_DF_SE, method='arima') #ARIMA

autoplot(mod_arima)

arima_df <- tibble(PI_SEM,est_arima=mod_arima$fitted) %>% 
  mutate(resid_arima=price_mean-est_arima)

#Grafico estimacion

ggplot(data = arima_df) +
  geom_line(mapping = aes(x = dateRep, y = price_mean),col='blue')+
  geom_line(aes(y=est_arima,x=dateRep),col='red',linetype='dashed')+
  labs(x = "Fecha" , y = "Casos diarios" ) +
  ggtitle("Estimacion ARIMA precio tomate") +
  labs(x = "Fecha", y = "Precio", color = "Legend")

#MAPE

MAPE(arima_df$price_mean,arima_df$est_arima)

#####2.2.Suavizado exponencial####

sefit<-ets(PI_DF_SE, model="ZZZ", damped=TRUE, alpha=NULL, beta=NULL,
           gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
           additive.only=FALSE, restrict=TRUE,
           allow.multiplicative.trend=TRUE)

#Descomposicion
autoplot(sefit) +
  ggtitle("ETS(ZZZ)")

#Grafico

se_df <- tibble(PI_SEM,est_se=sefit$fitted) %>% 
  mutate(resid_se=price_mean-est_se)

#Grafico estimacion

ggplot(data = se_df) +
  geom_line(mapping = aes(x = dateRep, y = price_mean),col='blue')+
  geom_line(aes(y=est_se,x=dateRep),col='red',linetype='dashed')+
  labs(x = "Fecha" , y = "Casos diarios" ) +
  ggtitle("Estimacion Suavizado Exponencial precio tomate") +
  labs(x = "Fecha", y = "Precio", color = "Legend")

#MAPE

MAPE(se_df$price_mean,se_df$est_se)

#####4.Comparacion modelos####

#Modelo lineal

MAPE(PI_DF$price_mean,predict(mod_lineal))

#Modelo GAM

MAPE(PI_DF$price_mean,PI_DF$est_gam)

#Arbol de regresion

MAPE(PI_DF$price_mean,PI_DF$est_tree)

#ARIMA

MAPE(arima_df$price_mean,arima_df$est_arima)

#Suavizado exponencial

MAPE(se_df$price_mean,se_df$est_se)

#Tabla comparativa

(COMPARACION_MAPE=tibble(LINEAL=MAPE(PI_DF$price_mean,predict(mod_lineal)),
       GAM=MAPE(PI_DF$price_mean,PI_DF$est_gam),
       TREE=MAPE(PI_DF$price_mean,PI_DF$est_tree),
       ARIMA=MAPE(arima_df$price_mean,arima_df$est_arima),
       SE=MAPE(se_df$price_mean,se_df$est_se)))
