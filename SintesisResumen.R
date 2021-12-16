
####SINTESIS####

#Aqui exponemos los principales resultados del proyecto.

#Seguimos la misma estructura que en el documento word.

#####1.Obtencion de datos#####

#Cargamos el archivo DatosSintesis. Incluye los datos requeridos aqui.

#####2.Analisis y Resultados####

#####2.1.Volatilidad#####

CEcv #Volatilidad para base de datos ComercioExterior

CCAAcv #Volatilidad para base de datos ConsumoCCAA

#####2.2.Grafico distribucion del comercio por paises#####

CE_INDEX %>% 
  filter(INDEX < 250) %>%
  ggplot(mapping = aes(x = REPORTER, y = INDEX)) +
  geom_boxplot(aes(color = REPORTER)) +
  ggtitle("Distribucion de las exportaciones por paises (2020)")

CCAA_INDEX %>% 
  ggplot(mapping = aes(x = REPORTER, y = INDEX)) +
  geom_boxplot(aes(color = REPORTER)) +
  ggtitle("Distribucion del consumo por CCAA (2020)")

#####2.3.Tasa de variacion del total####

CE_TV_ANUAL %>% 
  filter(YEAR==2020) %>%
  filter(TV<50) %>%
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = REPORTER, y = TV,
                                         fill = REPORTER),show.legend = FALSE,position = "dodge") +
  ggtitle("TV Exportaciones (2019-2020)")

CCAA_TV_ANUAL %>% 
  filter(YEAR==2020) %>%
  filter(TV<50) %>%
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = REPORTER, y = TV,
                                         fill = REPORTER),show.legend = FALSE,position = "dodge") +
  ggtitle("TV Consumo (2019-2020)")

#####2.4.Evolucion interanual mensual de exportaciones en la UE#####

CE_TV_TOTAL

#####2.5.Evolucion del indice de exportaciones por producto y comparacion####

CE_INDEX %>% filter(MONTH!=12) %>% #Faltan datos de mes 12
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE EXPORTACIONES POR PAIS (ENERO 2020 = 100)")

#Grafico comparativo; Acumulados e Indice de exportaciones

plot1 <- CE_INDEX %>% filter(MONTH!=12) %>% #Faltan datos de mes 12
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE EXPORTACIONES POR PAIS (ENERO 2020 = 100)")

plot2 <- CE_INDEX %>% filter(MONTH!=12) %>% #Faltan datos de mes 12
  filter(INDEX_D < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX_D,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX_D,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX_D),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE COVID POR PAIS")

grid.arrange(plot1, plot2, ncol=1)

#####2.6.Analisis especifico tomate Alemania####

#Descomposicion temporal

autoplot(STL_TA) +
  ggtitle("Descomposicion STL Tomate Alemania")
#Grafico

plot1 <- TA_SE_CE %>%
  ggplot() +
  geom_line(aes(MONTH,STL_TA_SE),lwd=1.25) +
  geom_smooth(aes(MONTH,STL_TA_SE),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  labs(x = "Fecha" , y = "Exportaciones" )+
  ggtitle("EVOLUCION DESESTACIONALIZADA EXPORTACIONES DE TOMATES ALEMANIA")

plot2 <- TA_SE_CE %>%
  ggplot() +
  geom_line(aes(MONTH,INDEX_D),lwd=1.25) +
  geom_smooth(aes(MONTH,INDEX_D),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  labs(x = "Fecha" , y = "Defunciones" )+
  ggtitle("EVOLUCION COVID ALEMANIA")

grid.arrange(plot1, plot2, ncol=1)

#Tasa de variacion interanual por mes Sin Estacionalidad(SE)

TVAM_ALEMANIA_PATATA_SE_CE <- tibble(FILTRO(ComercioExterior,"German","85"),STL_TA_SE) %>% 
  group_by(MONTH,YEAR,REPORTER)%>%
  filter(YEAR!=2018) %>%
  arrange(YEAR) %>%
  group_by(REPORTER) %>%
  mutate(Exportaciones_19 = lag(STL_TA_SE,12)) %>%
  filter(YEAR == 2020) %>%
  mutate(TVAM = (STL_TA_SE-Exportaciones_19)/STL_TA_SE) %>%
  mutate(Exportaciones_20 = STL_TA_SE ) %>%
  select(MONTH,YEAR,REPORTER,Exportaciones_20,TVAM,Exportaciones_19)

#####2.7.Total de consumo por CCAA y producto####

ConsumoCCAA %>% filter(REPORTER != 'Total Nacional') %>% #TotalNacional opaca al resto
  filter(Code != '44') %>% #Total de hortalizas y frutas opacan al resto
  filter(Code != '45') %>%
  group_by(REPORTER,Code) %>% 
  summarise(Consumo_Total=sum(Valor)) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = REPORTER, y = Consumo_Total,
                                         fill = Code),show.legend = FALSE,position = "dodge") +
  ggtitle("Consumo de productos por CCAA")

#####2.7.Red Neuronal para Covid####

fitnn

ggplot(data = ItalyCovid_Ola1) +
  geom_line(mapping = aes(x = dateRep, y = Acumulados), color = "blue",alpha=0.6)+
  geom_line(aes(y=predict(fitnn)$fitted,x=dateRep), color = "red",linetype='dashed')+
  ggtitle('Estimacion red neuronal de casos acumulados',subtitle = 'Italia primera ola')+
  labs(x = "Fecha" , y = "Casos" )

#Comparacion de modelos

#Regresion Polinomica

MAPE(ItalyCovid_Ola1$Acumulados,ItalyCovid_Ola1$Acumulados_est_P)

#Regresion logistica

MAPE(ItalyCovid_Ola1$Acumulados,predlol)

#Red Neuronal

accuracy(fitnn)

#####2.8.ARIMA para precios tomate####

autoplot(mod_arima)

ggplot(data = arima_df) +
  geom_line(mapping = aes(x = dateRep, y = price_mean),col='blue')+
  geom_line(aes(y=est_arima,x=dateRep),col='red',linetype='dashed')+
  labs(x = "Fecha" , y = "Casos diarios" ) +
  ggtitle("Estimacion ARIMA precio tomate") +
  labs(x = "Fecha", y = "Precio", color = "Legend")