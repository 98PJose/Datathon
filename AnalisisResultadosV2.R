
#### ANALISIS Y RESULTADOS ####

####1.Base Comercio Exterior####

#EXPORT_VALUE_IN_EUROS SON EXPORTACIONES DE PAISES EUROPEOS A SPAIN
#PARA ANALIZAR EXPORTACIONES DE SPAIN A ELLOS USAMOS IMPORT_VALUE_IN_EUROS

#####1.1.Estadistica descriptiva#####

#Sumarios

#General
summary(ComercioExterior)

#Por paises
Sumario_CE_Paises<-by(ComercioExterior, ComercioExterior$REPORTER, summary)

#Por producto
Sumario_CE_Producto<-by(ComercioExterior, ComercioExterior$Code, summary)

#Recuento
ComercioExterior %>% 
  count(PRODUCT) 

ComercioExterior %>% 
  count(REPORTER)

#Medias por region y producto

CEmean<-ComercioExterior %>%
  group_by(REPORTER,Code) %>% #Agrupamos
  select(REPORTER,Code,
         IMPORT_VALUE_IN_EUROS:EXPORT_PRICE) %>% #Seleccionamos variables 
  summarise_all(mean) #Media 

#Desviacion tipica por region y producto

CEsd<-ComercioExterior %>%
  group_by(REPORTER,Code) %>% #Agrupamos
  select(REPORTER,Code,
         IMPORT_VALUE_IN_EUROS:EXPORT_PRICE) %>% #Seleccionamos variables 
  summarise_all(sd) #Desviacion tipica

#Coerficiente de variacion #Cuando no hay operaciones con el producto surgen NA
#Permite comparar mejor que la desviacion tipica

CEcv<-ComercioExterior %>%
  group_by(REPORTER,Code) %>% #Agrupamos
  select(REPORTER,Code,
         IMPORT_VALUE_IN_EUROS:EXPORT_PRICE) %>% #Seleccionamos variables 
  summarise_all(CV) %>% #Coeficiente de variacion
  delete.na(n=0) %>% #Eliminamos filas con NA (Funcion propia)
  arrange(desc(IMPORT_VALUE_IN_EUROS)) %>% #Ordenamos de mayor a menor volatilidad 
  select(REPORTER:IMPORT_VALUE_IN_EUROS)

#Estructura de las exportaciones

#1.pipe line para filtrar ComercioExterior y media por paises
#2.ggplot para grafico de barras

#Importancia de cada pais en las exportaciones
ComercioExterior %>% filter(REPORTER != 'Europe') %>% #Europa opaca al resto
  group_by(REPORTER,Code) %>% 
  summarise(mean(IMPORT_VALUE_IN_EUROS)) %>% 
  setNames(c('Pais','Product','Exportaciones')) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = Pais, y = Exportaciones,
                                         fill = Pais),show.legend = FALSE,position = "dodge") +
  ggtitle("Exportaciones totales por pais")

#1.pipe line para filtrar ComercioExterior y media por paises
#2.ggplot para grafico de barras
#Exportaciones medias de producto por cada pais en el periodo
ComercioExterior %>% filter(REPORTER != 'Europe') %>% #Europa opaca al resto
  filter(Code != '82') %>% #La patata es el mayor y opaca al resto
  group_by(REPORTER,PRODUCT) %>% 
  summarise(mean(IMPORT_VALUE_IN_EUROS)) %>% 
  setNames(c('Pais','Product','Exportaciones')) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = Pais, y = Exportaciones,
                                         fill = Product),show.legend = FALSE,position = "dodge")+
  ggtitle("Exportaciones totales de cada producto por pais")

#Repetimos para extraer la leyenda #Quitamos show.legend = FALSE

legend <-ComercioExterior %>% filter(REPORTER != 'Europ') %>% #Europa opaca al resto
  filter(Code != '82') %>% #La patata es el mayor y opaca al resto
  group_by(REPORTER,Code) %>% 
  summarise(mean(IMPORT_VALUE_IN_EUROS)) %>% 
  setNames(c('Pais','Product','Exportaciones')) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = Pais, y = Exportaciones,
                                         fill = Product),position = "dodge")

legend <- get_legend(legend)

grid.newpage()
grid.draw(legend)
rm(legend)


#####1.2.Analisis general##### 

#Tratamos de ver la evolucion por paises y por productos

#Indice de productos por pais

#Peso de cada producto sobre el total de exportaciones

#Exportaciones promedio por pais y peso relativo 
CE_EXP_SUM <- ComercioExterior %>% 
  group_by(REPORTER,Code) %>% 
  summarise(Exportaciones=sum(IMPORT_VALUE_IN_EUROS)) %>% 
  setNames(c('Pais','Product','Exportaciones')) %>% 
  mutate(Peso=(Exportaciones/sum(Exportaciones)))

#Comprobacion #Vemos si los pesos suman 1

CE_EXP_SUM %>% group_by(Pais)%>% summarise(sum(Peso))%>%setNames(c('Pais','SumaPesos'))

#Añadimos pesos a base de datos

ComercioExterior <- ComercioExterior %>% 
  inner_join(CE_EXP_SUM,by = c("REPORTER"="Pais","Code"="Product"))

ComercioExteriorCovid <- ComercioExteriorCovid %>% 
  inner_join(CE_EXP_SUM,by = c("REPORTER"="Pais","Code"="Product"))

ComercioCovidMedia <- ComercioCovidMedia %>% 
  inner_join(CE_EXP_SUM,by = c("REPORTER"="Pais","Code"="Product"))

#Creacion del indice de productos (Indice de media ponderada de exportaciones)

#Media ponderada y vector boceto para indice ordenado por fecha y pais
#TV es tasa de variacion de la media ponderada (MEAN)
CE_INDEX <- ComercioExterior %>% 
  filter(YEAR ==2020) %>% 
  group_by(MONTH,YEAR,REPORTER)%>%
  summarise(MEAN = sum(IMPORT_VALUE_IN_EUROS*Peso)) %>% 
  arrange(REPORTER,YEAR)%>% 
  mutate(INDEX=100) %>% 
  group_by(REPORTER) %>% 
  mutate(MEAN2=first(MEAN[MEAN>0])) %>% 
  mutate(INDEX = (MEAN*100)/MEAN2) %>% 
  select(MONTH:INDEX) %>%
  mutate(X0 = lag(MEAN)) %>%
  mutate(TV = (MEAN-X0)/X0) %>%
  select(-X0) 
#Los que tienen 0 como primer valor generan INF por tanto se coge primer no nulo

#Grafico evolucion del Indice de productos

CE_INDEX %>% filter(MONTH!=12) %>% #Faltan datos de mes 12
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE EXPORTACIONES POR PAIS (ENERO 2020 = 100)")

#Grafico tasas de variacion intermensuales de exportaciones por pais

CE_INDEX  %>% filter(MONTH!=1) %>% #Faltan datos de mes 1
  filter(MONTH!=12) %>%
  filter(TV < 2) %>%
  ggplot() + 
  geom_point(aes(MONTH,TV,color=REPORTER)) +
  geom_line(aes(MONTH,TV,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TV),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  geom_hline(yintercept=0, linetype="dashed" ) +
  ggtitle("TASAS DE VARIACION INTERMENSUALES DE LA MEDIA DE EXPORTACIONES")


#Grafico de caja y bigotes
CE_INDEX %>% 
  filter(INDEX < 250) %>%
  ggplot(mapping = aes(x = REPORTER, y = INDEX)) +
  geom_boxplot(aes(color = REPORTER)) +
  ggtitle("Distribucion de las exportaciones por paises (2020)")

#Tasas de variacion interanuales por mes

CE_TV <- ComercioExterior %>% 
  group_by(MONTH,YEAR,REPORTER)%>%
  summarise(MEAN = sum(IMPORT_VALUE_IN_EUROS*Peso)) %>% 
  arrange(REPORTER,YEAR) %>%
  group_by(REPORTER) %>%
  mutate(MEAN0 = lag(MEAN,12)) %>%
  filter(YEAR ==2020) %>%
  mutate(MEAN = replace(MEAN,MEAN<1,1)) %>%
  mutate(MEAN0 = replace(MEAN0,MEAN0<1,1)) %>%
  mutate(TVAM = (MEAN-MEAN0)/MEAN0) %>%
  select(-MEAN0) #Remplazamos 0 con 1 para evitar Inf

#Grafico tasas de variacion interanuales por mes

CE_TV %>%
  filter(TVAM < 2) %>%
  filter(MONTH!=12)%>%
  ggplot() + 
  geom_point(aes(MONTH,TVAM,color=REPORTER)) +
  geom_line(aes(MONTH,TVAM,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TVAM),linetype="dashed",se=FALSE,color="black")+
  geom_hline(yintercept=0, linetype="dashed" ) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed")+ #Octubre
  ggtitle("TASAS DE VARIACION DE LA MEDIA DE EXPORTACIONES INTERANUAL POR MES (2020)")

#Los siguientes indices se haran con PESOS() e INDEX() para simplificar codigo

#Tasas de variacion interanuales por mes total exportaciones

CE_TV_TOTAL <- ComercioExterior %>% 
  group_by(MONTH,YEAR,REPORTER)%>%
  summarise(MEAN = sum(IMPORT_VALUE_IN_EUROS)) %>% 
  arrange(REPORTER,YEAR) %>%
  group_by(REPORTER) %>%
  mutate(MEAN0 = lag(MEAN,12)) %>%
  filter(YEAR ==2020) %>%
  mutate(MEAN = replace(MEAN,MEAN<1,1)) %>%
  mutate(MEAN0 = replace(MEAN0,MEAN0<1,1)) %>%
  mutate(TVAM = (MEAN-MEAN0)/MEAN0) %>%
  select(-MEAN0,-MEAN) #Remplazamos 0 con 1 para evitar Inf

#En orden descendente

caca = CE_TV_TOTAL %>% arrange(TVAM) %>% filter(MONTH!=12)

#Indice de valor de producto y tasa de variacion intermensual 

CE_PRODUCT_INDEX <- ComercioExterior %>% 
  filter(YEAR ==2020)%>% 
  group_by(MONTH,YEAR,Code) %>% 
  summarise(XTotal = sum(IMPORT_VALUE_IN_EUROS)) %>% 
  arrange(Code,YEAR,MONTH) %>%
  group_by(Code) %>% 
  mutate(X1 = first(XTotal[XTotal>0])) %>% 
  mutate(INDEX = (XTotal*100)/X1) %>% 
  select(-X1) %>% 
  mutate(X0=lag(XTotal)) %>%
  mutate(TV=(XTotal-X0)/X0) %>% 
  select(-X0)

#Grafico evolucion de exportaciones de cada producto

CE_PRODUCT_INDEX %>% filter(YEAR==2020) %>% 
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=Code)) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_line(aes(MONTH,INDEX,color=Code),lwd=2,alpha=0.5) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION EXPORTACIONES TOTALES POR PRODUCTO (2020)")

#Grafico tasa de variacion intermensual

CE_PRODUCT_INDEX %>% filter(YEAR==2020) %>% 
  filter(TV < 2) %>%
  ggplot() + 
  geom_point(aes(MONTH,TV,color=PRODUCT)) +
  geom_line(aes(MONTH,TV,color=PRODUCT),lwd=2,alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed" ) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed")+ #Octubre
  ggtitle("TV intermensual por producto (2020)")

#Tasa de variacion interanual por meses

CE_TV_PRODUCT <-ComercioExterior %>% 
  group_by(MONTH,YEAR,Code) %>% 
  summarise(XTotal = sum(IMPORT_VALUE_IN_EUROS)) %>% 
  arrange(Code,YEAR,MONTH) %>%
  group_by(Code) %>%
  mutate(X0 = lag(XTotal,12)) %>%
  filter(YEAR ==2020) %>%
  mutate(TVAM = (XTotal-X0)/X0) %>%
  select(-X0)

#Grafico TVAM

CE_TV_PRODUCT  %>% filter(YEAR==2020) %>% 
  filter(TVAM < 2) %>%
  ggplot() + 
  geom_point(aes(MONTH,TVAM,color=Code)) +
  geom_line(aes(MONTH,TVAM,color=Code),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TVAM),linetype="dashed",se=FALSE,color="black")+
  geom_hline(yintercept=0, linetype="dashed" ) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed")+ #Octubre
  ggtitle("TV Interanual por producto (2020)")

#Tasa de variacion exportaciones 2020 - 2019 por pais

CE_TV_ANUAL <- ComercioExterior %>% 
  group_by(YEAR,REPORTER) %>% 
  summarise(EXPORT = sum(IMPORT_VALUE_IN_EUROS)) %>%
  filter(YEAR != 2018) %>%
  arrange(REPORTER) %>% 
  group_by(REPORTER) %>%
  mutate(origen = first(EXPORT)) %>%
  mutate(final = last(EXPORT)) %>% 
  mutate(TV = (final-origen)/origen * 100) %>%
  select(-origen,-final)

#Grafica

CE_TV_ANUAL %>% 
  filter(YEAR==2020) %>%
  filter(TV<50) %>%
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = REPORTER, y = TV,
                                         fill = REPORTER),show.legend = FALSE,position = "dodge") +
  ggtitle("TV Exportaciones (2019-2020)")

#Importancia de producto en las exportaciones totales

(CE_PRODUCT_RELEVANCE <- ComercioExterior %>% 
  group_by(Code) %>% 
  summarise(EXPORTACIONES = sum(IMPORT_VALUE_IN_EUROS)) %>% 
  arrange(desc(EXPORTACIONES)))

#Importancia del pais en las exportaciones

(CE_COUNTRY_RELEVANCE <- ComercioExterior %>% 
  group_by(REPORTER) %>% 
  summarise(EXPORTACIONES = sum(IMPORT_VALUE_IN_EUROS)) %>%
  arrange(desc(EXPORTACIONES)))


#####1.3.Analisis con Covid#####

#Analisis descriptivo

summary(ComercioExteriorCovid)

#Correlaciones lineales

Cor_CE_Covid <- ComercioExteriorCovid %>% 
  select(IMPORT_VALUE_IN_EUROS:popData2019,Acumulados) %>%
  cor()

#Unimos Indice y Covid #Creamos Indice de Covid

Covid_Media <- Covid_Media %>% 
  arrange(countriesAndTerritories,year,month) %>%
  filter(year==2020) %>% 
  group_by(countriesAndTerritories) %>% 
  mutate(deaths2=first(deaths[deaths>0])) %>% 
  mutate(INDEX_D = (deaths*100)/deaths2) %>% 
  select(-deaths2) 

CE_INDEX <- CE_INDEX %>% 
  inner_join(Covid_Media, by = c("MONTH"="month","YEAR"="year",
                                 "REPORTER"="countriesAndTerritories")) 

CE_INDEX<-CE_INDEX %>% relocate(MONTH,YEAR,REPORTER,INDEX,INDEX_D)

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


#####1.4. Analisis especifico##### 

#Con FILTRO() seleccionamos region y producto de nuestras bases de datos

FILTRO(ComercioExteriorCovid,"German","85") 

summary(FILTRO(ComercioExteriorCovid,"German","85"))

#Media 

FILTRO(ComercioExteriorCovid,"German","85") %>% 
  select(IMPORT_VALUE_IN_EUROS) %>% 
  summarise(MEDIA=mean(IMPORT_VALUE_IN_EUROS))

#Desviacion tipica

FILTRO(ComercioExteriorCovid,"German","85") %>% 
  select(IMPORT_VALUE_IN_EUROS) %>% 
  summarise(DT=sd(IMPORT_VALUE_IN_EUROS))

#Coeficiente de variacion

FILTRO(ComercioExteriorCovid,"German","85") %>% 
  select(IMPORT_VALUE_IN_EUROS) %>% 
  summarise(CV=CV(IMPORT_VALUE_IN_EUROS))

#Comparacion evolucion covid y producto

plot1 <- FILTRO(ComercioExteriorCovid,"German","85") %>%
  ggplot() +
  geom_line(aes(MONTH,IMPORT_VALUE_IN_EUROS),lwd=1.25) +
  geom_smooth(aes(MONTH,IMPORT_VALUE_IN_EUROS),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  labs(x = "Fecha" , y = "Exportaciones" )+
  ggtitle("EVOLUCION EXPORTACIONES DE TOMATES ALEMANIA")

plot2 <- FILTRO(ComercioExteriorCovid,"German","85") %>%
  group_by(MONTH) %>%
  summarise(deaths=sum(deaths)) %>%
  ggplot() +
  geom_line(aes(MONTH,deaths),lwd=1.25) +
  geom_smooth(aes(MONTH,deaths),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  labs(x = "Fecha" , y = "Defunciones" )+
  ggtitle("EVOLUCION COVID ALEMANIA")

grid.arrange(plot1, plot2, ncol=1)

#Tasa de variacion intermensual

TV_ALEMANIA_PATATA_CE <- FILTRO(ComercioExterior,"German","85") %>%
  filter(YEAR==2020) %>%
  mutate(origen = lag(IMPORT_VALUE_IN_EUROS)) %>%
  mutate(TV = (IMPORT_VALUE_IN_EUROS-origen)/origen) %>%
  mutate(Exportaciones = IMPORT_VALUE_IN_EUROS ) %>%
  select(MONTH,YEAR,REPORTER,Exportaciones,TV)

#Tasa de variacion interanual por mes

TVAM_ALEMANIA_PATATA_CE <- FILTRO(ComercioExterior,"German","85") %>% 
  group_by(MONTH,YEAR,REPORTER)%>%
  filter(YEAR!=2018) %>%
  arrange(YEAR) %>%
  group_by(REPORTER) %>%
  mutate(Exportaciones_19 = lag(IMPORT_VALUE_IN_EUROS,12)) %>%
  filter(YEAR == 2020) %>%
  mutate(TVAM = (IMPORT_VALUE_IN_EUROS-Exportaciones_19)/Exportaciones_19) %>%
  mutate(Exportaciones_20 = IMPORT_VALUE_IN_EUROS ) %>%
  select(MONTH,YEAR,REPORTER,Exportaciones_20,TVAM,Exportaciones_19)

#Grafico

TVAM_ALEMANIA_PATATA_CE %>%
  ggplot() +
  geom_line(aes(MONTH,TVAM),lwd=1.25) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  geom_hline(yintercept = 0) +
  ggtitle("TV Interanual por meses de exportaciones de tomate a Alemania (2019-2020)")

#Descomposicion temporal #Todo el espacio temporal (2018-2020)
#Estacionalidad por clima y caracteristicas biologicas del producto altera analisis
#Tomate Alemania (TA)

TomateAlemania_TS_CE <- FILTRO(ComercioExterior,"German","85") %>% 
  select(IMPORT_VALUE_IN_EUROS) %>%
  ts(start = c(2018,1), frequency = 12) 

STL_TA<-TomateAlemania_TS_CE[,1] %>% mstl(robust=TRUE) 

autoplot(STL_TA) +
  ggtitle("Descomposicion STL Tomate Alemania")

#Analisis de la estacionalidad

STL_TA[,3] %>% autoplot() #Estacionalidad

freq_TA <- periodogram(STL_TA[,3]) 

freq_TA[["freq"]]

freq_TA[["spec"]]

#La frecuencia de mayor amplitud es 0.8333 

#Convertimos frecuencia en tiempo 0.8333=1/12

0.83333333333333 * 12

#Cada diez meses se produce el ciclo

#Analizamos solo tendencia y remanente

#Creamos base de datos

STL_TA_SE <- STL_TA[,2] + STL_TA[,4] #Datos sin estacionalidad trend + remainer

TA_SE_CE <- tibble(FILTRO(ComercioExterior,"German","85"),STL_TA_SE) %>% 
  inner_join(Covid_Media,by = c("REPORTER"="countriesAndTerritories","MONTH"="month","YEAR"="year")) %>%
  select(MONTH:REPORTER,STL_TA_SE,INDEX_D,deaths)

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

#Grafico

TVAM_ALEMANIA_PATATA_SE_CE %>%
  ggplot() +
  geom_line(aes(MONTH,TVAM),lwd=1.25) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  geom_hline(yintercept = 0) +
  ggtitle("TV Interanual por meses de exportaciones de tomate a Alemania SE(2019-2020)")


#####2.Base ConsumoCCAA#####

#Damos un codigo a los productos

PRODUCT_CCAA_NOM= ConsumoCCAA %>% 
  group_by(PRODUCT) %>%
  summarise(sum(Consumopc)) %>%
  select(PRODUCT) %>%
  mutate(Code=as.factor(seq(1,51,1)))

#Unimos codigo a las bases de datos

ConsumoCCAA=ConsumoCCAA %>% inner_join(PRODUCT_CCAA_NOM,by=c("PRODUCT"="PRODUCT"))

CCAACovid = CCAACovid  %>% inner_join(PRODUCT_CCAA_NOM,by=c("PRODUCT"="PRODUCT"))

CCAACovid_Media =  CCAACovid_Media %>% inner_join(PRODUCT_CCAA_NOM,by=c("PRODUCT"="PRODUCT"))


#####2.1.Estadistica descriptiva#####

#Sumarios

#General
summary(ConsumoCCAA)

#Por paises
Sumario_CCAA<-by(ConsumoCCAA, ConsumoCCAA$REPORTER, summary)

#Por producto
Sumario_CCAA_Producto<-by(ConsumoCCAA, ConsumoCCAA$PRODUCT, summary)

#Recuento
ConsumoCCAA %>% 
  count(PRODUCT) 

ConsumoCCAA %>% 
  count(REPORTER)

#Medias por region y producto

CCAAmean<-ConsumoCCAA %>%
  group_by(REPORTER,PRODUCT) %>% #Agrupamos
  select(REPORTER,PRODUCT,
         Volumen:Gastopc) %>% #Seleccionamos variables 
  summarise_all(mean) #Media 

#Desviacion tipica por region y producto

CCAAsd<-ConsumoCCAA %>%
  group_by(REPORTER,PRODUCT) %>% #Agrupamos
  select(REPORTER,PRODUCT,Volumen:Gastopc) %>% #Seleccionamos variables  
  summarise_all(sd) #Desviacion tipica

#Coerficiente de variacion #Cuando no hay operaciones con el producto surgen NA
#Permite comparar mejor que la desviacion tipica

CCAAcv<-ConsumoCCAA %>%
  group_by(REPORTER,PRODUCT) %>% #Agrupamos
  select(REPORTER,PRODUCT, Volumen:Gastopc) %>% #Seleccionamos variables  
  summarise_all(CV) %>% #Coeficiente de variacion
  delete.na(n=0) %>% #Eliminamos filas con NA (Funcion propia)
  arrange(desc(Consumopc)) #Ordenamos de mayor a menor volatilidad 

#Estructura del consumo

#1.pipe line para filtrar ComercioExterior y media por paises
#2.ggplot para grafico de barras

#Importancia de cada CCAA en el consumo

ConsumoCCAA %>% filter(REPORTER != 'Total Nacional') %>% #TotalNacional opaca al resto
  group_by(REPORTER) %>% 
  summarise(Consumo_Total=sum(Valor)) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = REPORTER, y = Consumo_Total,
                                         fill = REPORTER),show.legend = FALSE,position = "dodge") +
  ggtitle("Consumo por CCAA")

#Consumo pc para comparar mejor

ConsumoCCAA %>% filter(REPORTER != 'Total Nacional') %>% #TotalNacional opaca al resto
  group_by(REPORTER) %>% 
  summarise(Consumo_Total=sum(Consumopc)) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = REPORTER, y = Consumo_Total,
                                         fill = REPORTER),show.legend = FALSE,position = "dodge") +
  ggtitle("Consumo pc por CCAA")

#Importancia de los productos en el consumo

ConsumoCCAA %>% filter(REPORTER != 'Total Nacional') %>% #TotalNacional opaca al resto
  filter(Code != '44') %>% #Total de hortalizas y frutas opacan al resto
  filter(Code != '45') %>%
  group_by(Code) %>% 
  summarise(Consumo_Total=sum(Valor)) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = Code, y = Consumo_Total,
                                         fill = Code),show.legend = FALSE,position = "dodge") +
  ggtitle("Consumo por producto")

#Consumo por CCAA y Producto

ConsumoCCAA %>% filter(REPORTER != 'Total Nacional') %>% #TotalNacional opaca al resto
  filter(Code != '44') %>% #Total de hortalizas y frutas opacan al resto
  filter(Code != '45') %>%
  group_by(REPORTER,Code) %>% 
  summarise(Consumo_Total=sum(Valor)) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = REPORTER, y = Consumo_Total,
                                         fill = Code),show.legend = FALSE,position = "dodge") +
  ggtitle("Consumo de productos por CCAA")

#Repetimos para extraer la leyenda #Quitamos show.legend = FALSE

legend <-ConsumoCCAA %>% filter(REPORTER != 'Total Nacional') %>% #TotalNacional opaca al resto
  filter(Code != '44') %>% #Total de hortalizas y frutas opacan al resto
  filter(Code != '45') %>%
  group_by(REPORTER,Code) %>% 
  summarise(Consumo_Total=sum(Valor)) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = REPORTER, y = Consumo_Total,
                                         fill = Code),position = "dodge") +
  ggtitle("Consumo de productos por CCAA")

legend <- get_legend(legend)

grid.newpage()
grid.draw(legend)
rm(legend)



#####2.2.Analisis general#####

#Tratamos de ver la evolucion por paises y por productos

#Indice de productos por pais

#Peso de cada producto sobre el total de exportaciones

#Exportaciones promedio por pais y peso relativo 
CCAA_EXP_SUM <- ConsumoCCAA%>% 
  group_by(REPORTER,Code) %>% 
  summarise(ConsumoTotal=sum(Valor)) %>% 
  mutate(Peso=(ConsumoTotal/sum(ConsumoTotal)))

#Comprobacion
CCAA_EXP_SUM %>% group_by(REPORTER)%>% summarise(sum(Peso))%>%setNames(c('Pais','SumaPesos'))

#Añadimos pesos a base de datos

ConsumoCCAA <- ConsumoCCAA %>% 
  inner_join(CCAA_EXP_SUM,by = c("REPORTER"="REPORTER","Code"="Code"))

CCAACovid <- CCAACovid %>% 
  inner_join(CCAA_EXP_SUM,by = c("REPORTER"="REPORTER","Code"="Code"))

CCAACovid_Media <- CCAACovid_Media %>% 
  inner_join(CCAA_EXP_SUM,by = c("REPORTER"="REPORTER","Code"="Code"))

#Creacion del indice de productos (Indice de media ponderada de valor de consumo)

#Media ponderada y vector boceto para indice ordenado por fecha y CCAA
#TV es tasa de variacion de la media ponderada (MEAN)
CCAA_INDEX <- ConsumoCCAA %>% 
  filter(YEAR ==2020) %>% 
  group_by(MONTH,YEAR,REPORTER)%>%
  summarise(MEAN = sum(Valor*Peso)) %>% 
  arrange(REPORTER,YEAR)%>% 
  mutate(INDEX=100) %>% 
  group_by(REPORTER) %>% 
  mutate(MEAN2=first(MEAN[MEAN>0])) %>% 
  mutate(INDEX = (MEAN*100)/MEAN2) %>% 
  select(MONTH:INDEX) %>%
  mutate(X0 = lag(MEAN)) %>%
  mutate(TV = (MEAN-X0)/X0) %>%
  select(-X0) 
#Los que tienen 0 como primer valor generan INF por tanto se coge primer no nulo

#Grafico evolucion del Indice de productos

CCAA_INDEX %>% 
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE CONSUMO POR CCAA (ENERO 2020 = 100)")

#Grafico tasas de variacion intermensuales de consumo por CCAA

CCAA_INDEX  %>% 
  ggplot() + 
  geom_point(aes(MONTH,TV,color=REPORTER)) +
  geom_line(aes(MONTH,TV,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TV),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  geom_hline(yintercept=0, linetype="dashed" ) +
  ggtitle("TASAS DE VARIACION INTERMENSUALES DE LA MEDIA DE CONSUMO")


#Grafico de caja y bigotes
CCAA_INDEX %>% 
  ggplot(mapping = aes(x = REPORTER, y = INDEX)) +
  geom_boxplot(aes(color = REPORTER)) +
  ggtitle("Distribucion del consumo por CCAA (2020)")


#Tasas de variacion interanuales por mes

CCAA_TV <- ConsumoCCAA %>% 
  group_by(MONTH,YEAR,REPORTER)%>%
  summarise(MEAN = sum(Valor*Peso)) %>% 
  arrange(REPORTER,YEAR) %>%
  group_by(REPORTER) %>%
  mutate(MEAN0 = lag(MEAN,12)) %>%
  filter(YEAR ==2020) %>%
  mutate(MEAN = replace(MEAN,MEAN<1,1)) %>%
  mutate(MEAN0 = replace(MEAN0,MEAN0<1,1)) %>%
  mutate(TVAM = (MEAN-MEAN0)/MEAN0) %>%
  select(-MEAN0) #Remplazamos 0 con 1 para evitar Inf

#Grafico tasas de variacion interanuales por mes

CCAA_TV %>%
  ggplot() + 
  geom_point(aes(MONTH,TVAM,color=REPORTER)) +
  geom_line(aes(MONTH,TVAM,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TVAM),linetype="dashed",se=FALSE,color="black")+
  geom_hline(yintercept=0, linetype="dashed" ) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed")+ #Octubre
  ggtitle("TASAS DE VARIACION DE LA MEDIA DE CONSUMO INTERANUAL POR MES (2020)")

#Analisis por productos

#Indice de valor de producto y tasa de variacion intermensual 

CCAA_PRODUCT_INDEX <- ConsumoCCAA %>% 
  filter(YEAR ==2020)%>% 
  group_by(MONTH,YEAR,Code) %>% 
  summarise(XTotal = sum(Valor)) %>% 
  arrange(Code,YEAR,MONTH) %>%
  group_by(Code) %>% 
  mutate(X1 = first(XTotal[XTotal>0])) %>% 
  mutate(INDEX = (XTotal*100)/X1) %>% 
  select(-X1) %>% 
  mutate(X0=lag(XTotal)) %>%
  mutate(TV=(XTotal-X0)/X0) %>% 
  select(-X0)

#Grafico evolucion de consumo por producto

CCAA_PRODUCT_INDEX %>% filter(YEAR==2020) %>% 
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=Code)) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_line(aes(MONTH,INDEX,color=Code),lwd=2,alpha=0.5) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION CONSUMO POR PRODUCTO (2020)")

#Tasa de variacion interanual por meses

CCAA_TV_PRODUCT <-ConsumoCCAA %>% 
  group_by(MONTH,YEAR,Code) %>% 
  summarise(XTotal = sum(Valor)) %>% 
  arrange(Code,YEAR,MONTH) %>%
  group_by(Code) %>%
  mutate(X0 = lag(XTotal,12)) %>%
  filter(YEAR ==2020) %>%
  mutate(TVAM = (XTotal-X0)/X0) %>%
  select(-X0)

#Grafico TVAM

CCAA_TV_PRODUCT  %>% filter(YEAR==2020) %>% 
  filter(TVAM < 2) %>%
  ggplot() + 
  geom_point(aes(MONTH,TVAM,color=Code)) +
  geom_line(aes(MONTH,TVAM,color=Code),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TVAM),linetype="dashed",se=FALSE,color="black")+
  geom_hline(yintercept=0, linetype="dashed" ) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed")+ #Octubre
  ggtitle("TV Interanual por producto (2020)")


#Tasa de variacion exportaciones 2020 - 2019 por CCAA

CCAA_TV_ANUAL <- ConsumoCCAA %>% 
  group_by(YEAR,REPORTER) %>% 
  summarise(Consumo = sum(Valor)) %>%
  filter(YEAR != 2018) %>%
  arrange(REPORTER) %>% 
  group_by(REPORTER) %>%
  mutate(origen = first(Consumo)) %>%
  mutate(final = last(Consumo)) %>% 
  mutate(TV = (final-origen)/origen * 100) %>%
  select(-origen,-final)


#Grafica

CCAA_TV_ANUAL %>% 
  filter(YEAR==2020) %>%
  filter(TV<50) %>%
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = REPORTER, y = TV,
                                         fill = REPORTER),show.legend = FALSE,position = "dodge") +
  ggtitle("TV Consumo (2019-2020)")

#Importancia de producto en el consumo

(CCAA_PRODUCT_RELEVANCE <- ConsumoCCAA %>% 
    group_by(Code) %>% 
    summarise(Consumo = sum(Valor)) %>% 
    arrange(desc(Consumo)))

#Importancia de CCAA en consumo

(CCAA_RELEVANCE <- ConsumoCCAA %>% 
    group_by(REPORTER) %>% 
    summarise(Consumo = sum(Valor)) %>%
    arrange(desc(Consumo)))


#####2.3. Analisis con Covid#####

#Analisis descriptivo

summary(CCAACovid)

#Correlaciones lineales

Cor_CCAA_Covid <- CCAACovid %>% 
  select(Valor:Acumulados,-countriesAndTerritories,-continentExp,-dateRep,-popData2019) %>%
  cor()

#Unimos Indice y Covid #Creamos Indice de Covid

Covid_Media_Spain <- Covid_Media_Spain %>% 
  arrange(countriesAndTerritories,year,month) %>%
  filter(year==2020) %>% 
  group_by(countriesAndTerritories) %>% 
  mutate(deaths2=first(deaths[deaths>0])) %>% 
  mutate(INDEX_D = (deaths*100)/deaths2) %>% 
  select(-deaths2) 

CCAA_INDEX <- CCAA_INDEX %>% 
  inner_join(Covid_Media_Spain, by = c("MONTH"="month","YEAR"="year")) 

CCAA_INDEX<-CCAA_INDEX %>% relocate(MONTH,YEAR,REPORTER,INDEX,INDEX_D)

#Grafico comparativo; Acumulados e Indice de consumo

plot1 <- CCAA_INDEX %>%
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE CONSUMO POR CCAA (ENERO 2020 = 100)")

plot2 <- CCAA_INDEX %>% 
  ggplot() + 
  geom_point(aes(MONTH,deaths)) +
  geom_line(aes(MONTH,deaths),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,deaths),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL COVID")

grid.arrange(plot1, plot2, ncol=1)

#Proporcion del consumo sobre el total nacional

TotalNacional=CCAA_RELEVANCE %>% filter(REPORTER=="Total Nacional") 

CCAA_RELEVANCE = CCAA_RELEVANCE %>% 
  full_join(TotalNacional,by = c("REPORTER"="REPORTER")) %>%
  setNames(c('REPORTER','ConsumoTotal','TotalNacional')) %>%
  mutate(TotalNacional = 	first(TotalNacional)) %>%
  mutate(PesoNacional = ConsumoTotal/TotalNacional) %>%
  select(-TotalNacional)


#####2.4. Analisis especifico##### 

#Con FILTRO() seleccionamos region y producto de nuestras bases de datos

FILTRO(CCAACovid_Media,"Cataluña","44") #Cataluña total de frutas

summary(FILTRO(CCAACovid_Media,"Cataluña","44"))

#Media 

FILTRO(CCAACovid_Media,"Cataluña","44") %>% 
  select(Valor) %>% 
  summarise(MEDIA=mean(Valor))

#Desviacion tipica

FILTRO(CCAACovid_Media,"Cataluña","44") %>% 
  select(Valor) %>% 
  summarise(DT=sd(Valor))

#Coeficiente de variacion

FILTRO(CCAACovid_Media,"Cataluña","44") %>% 
  select(Valor) %>% 
  summarise(CV=CV(Valor))

#Comparacion evolucion covid y producto

plot1 <- FILTRO(CCAACovid_Media,"Cataluña","44") %>%
  ggplot() +
  geom_line(aes(MONTH,Valor),lwd=1.25) +
  geom_smooth(aes(MONTH,Valor),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  ggtitle("EVOLUCION CONSUMO DE FRUTAS CATALUÑA")

plot2 <- FILTRO(CCAACovid_Media,"Cataluña","44") %>%
  group_by(MONTH) %>%
  summarise(deaths=sum(deaths)) %>%
  ggplot() +
  geom_line(aes(MONTH,deaths),lwd=1.25) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  ggtitle("EVOLUCION COVID")

grid.arrange(plot1, plot2, ncol=1)

#Tasa de variacion intermensual

TV_CATA_FRUTA_CCAA <- FILTRO(CCAACovid_Media,"Cataluña","44") %>%
  filter(YEAR==2020) %>%
  mutate(origen = lag(Valor)) %>%
  mutate(TV = (Valor-origen)/origen) %>%
  select(MONTH,YEAR,REPORTER,Valor,TV)

#Tasa de variacion interanual por mes

TVAM_CATA_FRUTA_CCAA <- FILTRO(ConsumoCCAA,"Cataluña","44") %>% 
  group_by(MONTH,YEAR,REPORTER)%>%
  filter(YEAR!=2018) %>%
  arrange(YEAR) %>%
  group_by(REPORTER) %>%
  mutate(Consumo_19 = lag(Valor,12)) %>%
  filter(YEAR == 2020) %>%
  mutate(TVAM = (Valor-Consumo_19)/Consumo_19) %>%
  mutate(Consumo_20 = Valor ) %>%
  select(MONTH,YEAR,REPORTER,Consumo_20,TVAM,Consumo_19)

#Grafico

TVAM_CATA_FRUTA_CCAA %>%
  ggplot() +
  geom_line(aes(MONTH,TVAM),lwd=1.25) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  geom_hline(yintercept = 0) +
  ggtitle("TV Interanual por meses del consumo de fruta en Cataluña (2019-2020)")


#Descomposicion temporal #Todo el espacio temporal (2018-2020)
#Estacionalidad por clima y caracteristicas biologicas del producto altera analisis
#Fruta Cataluña (FC)

FrutaCata_TS_CE <- FILTRO(ConsumoCCAA,"Cataluña","44") %>% 
  select(Valor) %>%
  ts(start = c(2018,1), frequency = 12) 

STL_FC<-FrutaCata_TS_CE[,1] %>% mstl(robust=TRUE) 

autoplot(STL_FC) +
  ggtitle("Descomposicion STL Fruta Catalana")

#Analisis de la esFCcionalidad

STL_FC[,3] %>% autoplot() #EsFCcionalidad

freq_FC <- periodogram(STL_FC[,3]) 

freq_FC[["freq"]]

freq_FC[["spec"]]

#Creamos base de datos

STL_FC_SE <- STL_FC[,2] + STL_FC[,4] #Datos sin estacionalidad trend + remainer

FC_SE_CE <- tibble(FILTRO(ConsumoCCAA,"Cataluña","44"),STL_FC_SE) %>% 
  inner_join(Covid_Media_Spain,by = c("MONTH"="month","YEAR"="year")) %>%
  select(MONTH:REPORTER,STL_FC_SE,INDEX_D,deaths)

#Grafico

plot1 <- FC_SE_CE %>%
  ggplot() +
  geom_line(aes(MONTH,STL_FC_SE),lwd=1.25) +
  geom_smooth(aes(MONTH,STL_FC_SE),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  ggtitle("EVOLUCION DESESTACIONALIZADA CONSUMO FRUTA CATALAN")

plot2 <- FC_SE_CE %>%
  ggplot() +
  geom_line(aes(MONTH,INDEX_D),lwd=1.25) +
  geom_smooth(aes(MONTH,INDEX_D),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  ggtitle("EVOLUCION COVID")

grid.arrange(plot1, plot2, ncol=1)


#####3.Base MercaBarna#####

#####3.1.Estadistica descriptiva#####

#Damos un codigo a los productos

PRODUCT_MB_NOM= MercaBarna %>% 
  group_by(PRODUCT) %>%
  summarise(sum(EXPORT_VALUE_IN_EUROS)) %>%
  select(PRODUCT) %>%
  mutate(Code=as.factor(seq(1,126,1)))

#Unimos codigo a las bases de datos

MercaBarna=MercaBarna %>% inner_join(PRODUCT_MB_NOM,by=c("PRODUCT"="PRODUCT"))

MercaBarnaCovid = MercaBarnaCovid  %>% inner_join(PRODUCT_MB_NOM,by=c("PRODUCT"="PRODUCT"))

MercaBarnaCovid_Media =  MercaBarnaCovid_Media %>% inner_join(PRODUCT_MB_NOM,by=c("PRODUCT"="PRODUCT"))

#Damos un codigo a las regiones

ORIGEN_MB_NOM = MercaBarna %>% 
  group_by(REPORTER) %>%
  summarise(sum(EXPORT_VALUE_IN_EUROS)) %>%
  select(REPORTER) %>%
  mutate(OrigenCode=as.factor(seq(1,96,1)))

#Unimos codigo a las bases de datos

MercaBarna=MercaBarna %>% inner_join(ORIGEN_MB_NOM,by=c("REPORTER"="REPORTER"))

#Sumarios

#General
summary(MercaBarna)

#Por paises
Sumario_MB_Paises<-by(MercaBarna, MercaBarna$REPORTER, summary)

#Por producto
Sumario_MB_Producto<-by(MercaBarna, MercaBarna$Code, summary)

#Recuento
MercaBarna %>% 
  count(PRODUCT) 

MercaBarna %>% 
  count(REPORTER)

#Medias por region y producto

MBmean<-MercaBarna %>%
  group_by(REPORTER,Code) %>% #Agrupamos
  select(REPORTER,Code,
         EXPORT_VALUE_IN_EUROS,price_mean) %>% #Seleccionamos variables 
  summarise_all(mean) #Media 

#Desviacion tipica por region y producto

MBsd<-MercaBarna %>%
  group_by(REPORTER,Code) %>% #Agrupamos
  select(REPORTER,Code,
         EXPORT_VALUE_IN_EUROS,price_mean) %>% #Seleccionamos variables 
  summarise_all(sd) #Desviacion tipica

#Coerficiente de variacion #Cuando no hay operaciones con el producto surgen NA
#Permite comparar mejor que la desviacion tipica

MBcv<-MercaBarna %>%
  group_by(REPORTER,Code) %>% #Agrupamos
  select(REPORTER,Code,
         EXPORT_VALUE_IN_EUROS,price_mean) %>% #Seleccionamos variables 
  summarise_all(CV) %>% #Coeficiente de variacion
  delete.na(n=0) %>% #Eliminamos filas con NA (Funcion propia)
  arrange(desc(EXPORT_VALUE_IN_EUROS)) #Ordenamos de mayor a menor volatilidad 

#Estructura de las exportaciones

#1.pipe line para filtrar ComercioExterior y media por paises
#2.ggplot para grafico de barras

#Importancia de cada pais en las exportaciones
MercaBarna %>% 
  group_by(OrigenCode,Code) %>% 
  summarise(sum(EXPORT_VALUE_IN_EUROS)) %>% 
  setNames(c('Pais','Product','Exportaciones')) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = Pais, y = Exportaciones,
                                         fill = Pais),show.legend = FALSE,position = "dodge") +
  ggtitle("Valor total por region")

#1.pipe line para filtrar ComercioExterior y media por paises
#2.ggplot para grafico de barras
#Exportaciones medias de producto por cada pais en el periodo
MercaBarna %>% 
  group_by(OrigenCode,Code) %>% 
  summarise(mean(EXPORT_VALUE_IN_EUROS)) %>% 
  setNames(c('Region','Product','Exportaciones')) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = Region, y = Exportaciones,
                                         fill = Product),show.legend = FALSE,position = "dodge")+
  ggtitle("Exportaciones totales de cada producto por pais")

#Repetimos para extraer la leyenda #Quitamos show.legend = FALSE

legend <-MercaBarna %>% 
  group_by(OrigenCode,Code) %>% 
  summarise(mean(EXPORT_VALUE_IN_EUROS)) %>% 
  setNames(c('Region','Product','Exportaciones')) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = Region, y = Exportaciones,
                                         fill = Product),position = "dodge")

legend <- get_legend(legend)

grid.newpage()
grid.draw(legend)
rm(legend)


#####3.2.Analisis general##### 

#Tratamos de ver la evolucion por paises y por productos

#Indice de productos por pais

#Peso de cada producto sobre el total de exportaciones

#Exportaciones promedio por pais y peso relativo 
MB_EXP_SUM <- PESOS(MercaBarna)

#Comprobacion #Vemos si los pesos suman 1

MB_EXP_SUM %>% group_by(REPORTER)%>% summarise(sum(Peso))%>%setNames(c('Origen','SumaPesos'))

#Añadimos pesos a base de datos

MercaBarna <- MercaBarna %>% 
  inner_join(MB_EXP_SUM,by = c("REPORTER"="REPORTER","Code"="Code"))

MercaBarnaCovid <- MercaBarnaCovid %>% 
  inner_join(PESOS(MercaBarnaCovid),by = c("REPORTER"="REPORTER","Code"="Code"))

MercaBarnaCovid_Media <- MercaBarnaCovid_Media %>% 
  inner_join(PESOS(MercaBarnaCovid_Media),by = c("REPORTER"="REPORTER","Code"="Code"))

#Creacion del indice de productos (Indice de media ponderada de exportaciones)

#Media ponderada y vector boceto para indice ordenado por fecha y pais
#TV es tasa de variacion de la media ponderada (MEAN)

MB_INDEX <- INDEX(MercaBarna%>%filter(YEAR==2020))

#Unimos con codigo de paises

MB_INDEX = MB_INDEX %>% 
  inner_join(ORIGEN_MB_NOM,by=c("REPORTER"="REPORTER")) %>%
  relocate(OrigenCode)

#Los que tienen 0 como primer valor generan INF por tanto se coge primer no nulo

#Grafico evolucion del Indice de productos

MB_INDEX %>% filter(MONTH!=12) %>% #Faltan datos de mes 12
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE COMERCIO POR PAIS (ENERO 2020 = 100)")

#Grafico tasas de variacion intermensuales de exportaciones por pais

MB_INDEX  %>% filter(MONTH!=1) %>% #Faltan datos de mes 1
  filter(TV < 2) %>%
  ggplot() + 
  geom_point(aes(MONTH,TV,color=REPORTER)) +
  geom_line(aes(MONTH,TV,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TV),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  geom_hline(yintercept=0, linetype="dashed" ) +
  ggtitle("TASAS DE VARIACION INTERMENSUALES DE LA MEDIA DE COMERCIO")


#Grafico de caja y bigotes
MB_INDEX %>% 
  filter(INDEX < 250) %>%
  ggplot(mapping = aes(x = OrigenCode, y = INDEX)) +
  geom_boxplot(aes(color = OrigenCode),show.legend = FALSE) +
  ggtitle("Distribucion del comercio por paises (2020)")

#Tasas de variacion interanuales por mes

MB_TV <- MercaBarna %>% 
  group_by(MONTH,YEAR,REPORTER)%>%
  summarise(MEAN = sum(EXPORT_VALUE_IN_EUROS*Peso)) %>% 
  arrange(REPORTER,YEAR) %>%
  group_by(REPORTER) %>%
  mutate(MEAN0 = lag(MEAN,12)) %>%
  filter(YEAR ==2020) %>%
  mutate(MEAN = replace(MEAN,MEAN<1,1)) %>%
  mutate(MEAN0 = replace(MEAN0,MEAN0<1,1)) %>%
  mutate(TVAM = (MEAN-MEAN0)/MEAN0) %>%
  select(-MEAN0) #Remplazamos 0 con 1 para evitar Inf

#Grafico tasas de variacion interanuales por mes

MB_TV %>%
  filter(TVAM < 2) %>%
  filter(MONTH!=12)%>%
  ggplot() + 
  geom_point(aes(MONTH,TVAM,color=REPORTER)) +
  geom_line(aes(MONTH,TVAM,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TVAM),linetype="dashed",se=FALSE,color="black")+
  geom_hline(yintercept=0, linetype="dashed" ) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed")+ #Octubre
  ggtitle("TASAS DE VARIACION DE LA MEDIA DE COMERCIO, INTERANUAL POR MES (2020)")

#Indice de valor de producto y tasa de variacion intermensual 

MB_PRODUCT_INDEX <- MercaBarna %>% 
  filter(YEAR ==2020)%>% 
  group_by(MONTH,YEAR,Code) %>% 
  summarise(XTotal = sum(EXPORT_VALUE_IN_EUROS)) %>% 
  arrange(Code,YEAR,MONTH) %>%
  group_by(Code) %>% 
  mutate(X1 = first(XTotal[XTotal>0])) %>% 
  mutate(INDEX = (XTotal*100)/X1) %>% 
  select(-X1) %>% 
  mutate(X0=lag(XTotal)) %>%
  mutate(TV=(XTotal-X0)/X0) %>% 
  select(-X0) %>%
  arrange(Code,MONTH)

#Grafico evolucion de exportaciones de cada producto

MB_PRODUCT_INDEX %>% filter(YEAR==2020) %>% 
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=Code)) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_line(aes(MONTH,INDEX,color=Code),lwd=2,alpha=0.5) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION COMERCIO POR PRODUCTO (2020)")

#Grafico tasa de variacion intermensual

MB_PRODUCT_INDEX %>% filter(YEAR==2020) %>% 
  filter(TV < 2) %>%
  ggplot() + 
  geom_point(aes(MONTH,TV,color=Code)) +
  geom_line(aes(MONTH,TV,color=Code),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TV),linetype="dashed",se=FALSE,color="black")+
  geom_hline(yintercept=0, linetype="dashed" ) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed")+ #Octubre
  ggtitle("TV intermensual por producto (2020)")

#Tasa de variacion interanual por meses

MB_TV_PRODUCT <-MercaBarna %>% 
  group_by(MONTH,YEAR,Code) %>% 
  summarise(XTotal = sum(EXPORT_VALUE_IN_EUROS)) %>% 
  arrange(Code,YEAR,MONTH) %>%
  group_by(Code) %>%
  mutate(X0 = lag(XTotal,12)) %>%
  filter(YEAR ==2020) %>%
  mutate(TVAM = (XTotal-X0)/X0) %>%
  select(-X0)

#Grafico TVAM

MB_TV_PRODUCT  %>% filter(YEAR==2020) %>% 
  filter(TVAM < 2) %>%
  ggplot() + 
  geom_point(aes(MONTH,TVAM,color=Code)) +
  geom_line(aes(MONTH,TVAM,color=Code),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TVAM),linetype="dashed",se=FALSE,color="black")+
  geom_hline(yintercept=0, linetype="dashed" ) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed")+ #Octubre
  ggtitle("TV Interanual por producto (2020)")

#Tasa de variacion exportaciones 2020 - 2019 por Origen

MB_TV_ANUAL <- MercaBarna %>% 
  group_by(YEAR,OrigenCode) %>% 
  summarise(EXPORT = sum(EXPORT_VALUE_IN_EUROS)) %>%
  filter(YEAR != 2018) %>%
  arrange(OrigenCode) %>% 
  group_by(OrigenCode) %>%
  mutate(origen = first(EXPORT)) %>%
  mutate(final = last(EXPORT)) %>% 
  mutate(TV = (final-origen)/origen * 100) %>%
  select(-origen,-final)

#Grafica

MB_TV_ANUAL %>% 
  filter(YEAR==2020) %>%
  filter(TV<50) %>%
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = OrigenCode, y = TV,
                                         fill = OrigenCode),show.legend = FALSE,position = "dodge") +
  ggtitle("TV comercio (2019-2020)")

#Importancia de producto en las exportaciones totales

(MB_PRODUCT_RELEVANCE <- MercaBarna %>% 
    group_by(Code) %>% 
    summarise(EXPORTACIONES = sum(EXPORT_VALUE_IN_EUROS)) %>% 
    arrange(desc(EXPORTACIONES)))

#Importancia del pais en las exportaciones

(MB_COUNTRY_RELEVANCE <- MercaBarna %>% 
    group_by(REPORTER) %>% 
    summarise(EXPORTACIONES = sum(EXPORT_VALUE_IN_EUROS)) %>%
    arrange(desc(EXPORTACIONES)))


#####3.3.Analisis con Covid#####

#Analisis descriptivo

summary(MercaBarnaCovid)

#Correlaciones lineales

Cor_MB_Covid <- MercaBarnaCovid %>% 
  select(price_mean:popData2019,Acumulados) %>%
  cor()

#Unimos Indice y Covid #Creamos Indice de Covid

#Hay que cambiar nombres de paises #Usamos script NombresPaises#

MB_INDEX <- MB_INDEX %>% 
  inner_join(Covid_Media, by = c("MONTH"="month","YEAR"="year",
                                 "REPORTER"="countriesAndTerritories")) 

MB_INDEX<-MB_INDEX %>% relocate(MONTH,YEAR,REPORTER,INDEX,INDEX_D)

#Grafico comparativo; Acumulados e Indice de exportaciones

plot1 <- MB_INDEX %>% 
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL COMERCIO POR ORIGEN  (ENERO 2020 = 100)")

plot2 <- MB_INDEX %>% 
  filter(INDEX_D < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX_D,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX_D,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX_D),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE COVID POR ORIGEN")

grid.arrange(plot1, plot2, ncol=1)


#####3.4. Analisis especifico#####

#Con FILTRO() seleccionamos region y producto de nuestras bases de datos

FILTRO(MercaBarnaCovid,"Belgiu","122") 

summary(FILTRO(MercaBarnaCovid,"Belgiu","122"))

#Media 

FILTRO(MercaBarnaCovid,"Belgiu","122") %>% 
  select(EXPORT_VALUE_IN_EUROS) %>% 
  summarise(MEDIA=mean(EXPORT_VALUE_IN_EUROS))

#Desviacion tipica

FILTRO(MercaBarnaCovid,"Belgiu","122") %>% 
  select(EXPORT_VALUE_IN_EUROS) %>% 
  summarise(DT=sd(EXPORT_VALUE_IN_EUROS))

#Coeficiente de variacion

FILTRO(MercaBarnaCovid,"Belgiu","122") %>% 
  select(EXPORT_VALUE_IN_EUROS) %>% 
  summarise(CV=CV(EXPORT_VALUE_IN_EUROS))

#Comparacion evolucion covid y producto

plot1 <- FILTRO(MercaBarnaCovid,"Belgiu","122") %>%
  ggplot() +
  geom_line(aes(MONTH,EXPORT_VALUE_IN_EUROS),lwd=1.25) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION EXPORTACIONES DE TOMATES")

plot2 <- FILTRO(MercaBarnaCovid,"Belgiu","122") %>%
  group_by(MONTH) %>%
  summarise(deaths=sum(deaths)) %>%
  ggplot() +
  geom_line(aes(MONTH,deaths),lwd=1.25) +
  geom_smooth(aes(MONTH,deaths),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION COVID")

grid.arrange(plot1, plot2, ncol=1)

#Tasa de variacion intermensual

TV_BELGICA_PATATA_CE <- FILTRO(MercaBarnaCovid,"Belgiu","122") %>%
  filter(YEAR==2020) %>%
  mutate(origen = lag(EXPORT_VALUE_IN_EUROS)) %>%
  mutate(TV = (EXPORT_VALUE_IN_EUROS-origen)/origen) %>%
  mutate(Exportaciones = EXPORT_VALUE_IN_EUROS ) %>%
  select(MONTH,YEAR,REPORTER,Exportaciones,TV)

#Descomposicion temporal #Todo el espacio temporal (2018-2020)
#Estacionalidad por clima y caracteristicas biologicas del producto altera analisis
#Tomate BELGICA (TB)

TomateBELGICA_TS_CE <- FILTRO(MercaBarna,"BELGICA","122") %>% 
  select(EXPORT_VALUE_IN_EUROS) %>%
  ts(start = c(2018,1), frequency = 12) 

STL_TB<-TomateBELGICA_TS_CE[,1] %>% mstl(robust=TRUE) 

autoplot(STL_TB) +
  ggtitle("Descomposicion STL Tomate BELGICA")

#Analisis de la estacionalidad

STL_TB[,3] %>% autoplot() #Estacionalidad

freq_TB <- periodogram(STL_TB[,3]) 

freq_TB[["freq"]]

freq_TB[["spec"]]

#Analizamos solo tendencia y remanente

#Creamos base de datos

STL_TB_SE <- STL_TB[,2] + STL_TB[,4] #Datos sin estacionalidad trend + remainer

TB_SE_CE <- tibble(FILTRO(MercaBarna2,"Belgiu","122"),STL_TB_SE) %>% 
  inner_join(Covid_Media,by = c("REPORTER"="countriesAndTerritories","MONTH"="month","YEAR"="year")) %>%
  select(MONTH:REPORTER,STL_TB_SE,INDEX_D,deaths)

#Grafico

plot1 <- TB_SE_CE %>%
  ggplot() +
  geom_line(aes(MONTH,STL_TB_SE),lwd=1.25) +
  geom_smooth(aes(MONTH,STL_TB_SE),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION DESESTACIONALIZADA EXPORTACIONES DE TOMATES BELGICA")

plot2 <- TB_SE_CE %>%
  ggplot() +
  geom_line(aes(MONTH,INDEX_D),lwd=1.25) +
  geom_smooth(aes(MONTH,INDEX_D),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION COVID BELGICA")

grid.arrange(plot1, plot2, ncol=1)


#####4.Base MercaMadrid#####

#####4.1.Estadistica descriptiva#####

#Damos un codigo a los productos

PRODUCT_MM_NOM= MercaMadrid %>% 
  group_by(PRODUCT) %>%
  summarise(sum(EXPORT_VALUE_IN_EUROS)) %>%
  select(PRODUCT) %>%
  mutate(Code=as.factor(seq(1,125,1)))

#Unimos codigo a las bases de datos

MercaMadrid=MercaMadrid %>% inner_join(PRODUCT_MM_NOM,by=c("PRODUCT"="PRODUCT"))

MercaMadridCovid = MercaMadridCovid  %>% inner_join(PRODUCT_MM_NOM,by=c("PRODUCT"="PRODUCT"))

MercaMadridCovid_Media =  MercaMadridCovid_Media %>% inner_join(PRODUCT_MM_NOM,by=c("PRODUCT"="PRODUCT"))

#Damos un codigo a las regiones

ORIGEN_MM_NOM = MercaMadrid %>% 
  group_by(REPORTER) %>%
  summarise(sum(EXPORT_VALUE_IN_EUROS)) %>%
  select(REPORTER) %>%
  mutate(OrigenCode=as.factor(seq(1,104,1)))

#Unimos codigo a las bases de datos

MercaMadrid=MercaMadrid %>% inner_join(ORIGEN_MM_NOM,by=c("REPORTER"="REPORTER"))

#Sumarios

#General
summary(MercaMadrid)

#Por paises
Sumario_MB_Paises<-by(MercaMadrid, MercaMadrid$REPORTER, summary)

#Por producto
Sumario_MB_Producto<-by(MercaMadrid, MercaMadrid$Code, summary)

#Recuento
MercaMadrid %>% 
  count(PRODUCT) 

MercaMadrid %>% 
  count(REPORTER)

#Medias por region y producto

MMmean<-MercaMadrid %>%
  group_by(REPORTER,Code) %>% #Agrupamos
  select(REPORTER,Code,
         EXPORT_VALUE_IN_EUROS,price_mean) %>% #Seleccionamos variables 
  summarise_all(mean) #Media 

#Desviacion tipica por region y producto

MMsd<-MercaMadrid %>%
  group_by(REPORTER,Code) %>% #Agrupamos
  select(REPORTER,Code,
         EXPORT_VALUE_IN_EUROS,price_mean) %>% #Seleccionamos variables 
  summarise_all(sd) #Desviacion tipica

#Coerficiente de variacion #Cuando no hay operaciones con el producto surgen NA
#Permite comparar mejor que la desviacion tipica

MMcv<-MercaMadrid %>%
  group_by(REPORTER,Code) %>% #Agrupamos
  select(REPORTER,Code,
         EXPORT_VALUE_IN_EUROS,price_mean) %>% #Seleccionamos variables 
  summarise_all(CV) %>% #Coeficiente de variacion
  delete.na(n=0) %>% #Eliminamos filas con NA (Funcion propia)
  arrange(desc(EXPORT_VALUE_IN_EUROS)) #Ordenamos de mayor a menor volatilidad 

#Estructura de las exportaciones

#1.pipe line para filtrar ComercioExterior y media por paises
#2.ggplot para grafico de barras

#Importancia de cada region en el comercio
MercaMadrid %>% 
  group_by(OrigenCode,Code) %>% 
  summarise(sum(EXPORT_VALUE_IN_EUROS)) %>% 
  setNames(c('Pais','Product','Exportaciones')) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = Pais, y = Exportaciones,
                                         fill = Pais),show.legend = FALSE,position = "dodge") +
  ggtitle("Valor total por region")

#Importancia productos

MercaMadrid %>% 
  group_by(OrigenCode,Code) %>% 
  summarise(sum(EXPORT_VALUE_IN_EUROS)) %>% 
  setNames(c('Pais','Product','Exportaciones')) %>% 
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = Product, y = Exportaciones,
                                         fill = Product),show.legend = FALSE,position = "dodge") +
  ggtitle("Valor total por producto")
#####4.2.Analisis general##### 

#Tratamos de ver la evolucion por paises y por productos

#Indice de productos por pais

#Peso de cada producto sobre el total de exportaciones

#Exportaciones promedio por pais y peso relativo 
MM_EXP_SUM <- PESOS(MercaMadrid)

#Comprobacion #Vemos si los pesos suman 1

MM_EXP_SUM %>% group_by(REPORTER)%>% summarise(sum(Peso))%>%setNames(c('Origen','SumaPesos'))

#Añadimos pesos a base de datos

MercaMadrid <- MercaMadrid %>% 
  inner_join(MM_EXP_SUM,by = c("REPORTER"="REPORTER","Code"="Code"))

MercaMadridCovid <- MercaMadridCovid %>% 
  inner_join(PESOS(MercaMadridCovid),by = c("REPORTER"="REPORTER","Code"="Code"))

MercaMadridCovid_Media <- MercaMadridCovid_Media %>% 
  inner_join(PESOS(MercaMadridCovid_Media),by = c("REPORTER"="REPORTER","Code"="Code"))

#Creacion del indice de productos (Indice de media ponderada de exportaciones)

#Media ponderada y vector boceto para indice ordenado por fecha y pais
#TV es tasa de variacion de la media ponderada (MEAN)

MM_INDEX <- INDEX(MercaMadrid%>%filter(YEAR==2020))

#Unimos con codigo de paises

MM_INDEX = MM_INDEX %>% 
  inner_join(ORIGEN_MM_NOM,by=c("REPORTER"="REPORTER")) %>%
  relocate(OrigenCode)

#Los que tienen 0 como primer valor generan INF por tanto se coge primer no nulo

#Grafico evolucion del Indice de productos

MM_INDEX %>% filter(MONTH!=12) %>% #Faltan datos de mes 12
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE COMERCIO POR PAIS (ENERO 2020 = 100)")

#Grafico tasas de variacion intermensuales de exportaciones por pais

MM_INDEX  %>% filter(MONTH!=1) %>% #Faltan datos de mes 1
  filter(TV < 2) %>%
  ggplot() + 
  geom_point(aes(MONTH,TV,color=REPORTER)) +
  geom_line(aes(MONTH,TV,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TV),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  geom_hline(yintercept=0, linetype="dashed" ) +
  ggtitle("TASAS DE VARIACION INTERMENSUALES DE LA MEDIA DE COMERCIO")


#Grafico de caja y bigotes
MM_INDEX %>% 
  filter(INDEX < 250) %>%
  ggplot(mapping = aes(x = OrigenCode, y = INDEX)) +
  geom_boxplot(aes(color = OrigenCode),show.legend = FALSE) +
  ggtitle("Distribucion del comercio por paises (2020)")

#Indice de valor de producto y tasa de variacion intermensual 

MM_PRODUCT_INDEX <- MercaMadrid %>% 
  filter(YEAR ==2020)%>% 
  group_by(MONTH,YEAR,Code) %>% 
  summarise(XTotal = sum(EXPORT_VALUE_IN_EUROS)) %>% 
  arrange(Code,YEAR,MONTH) %>%
  group_by(Code) %>% 
  mutate(X1 = first(XTotal[XTotal>0])) %>% 
  mutate(INDEX = (XTotal*100)/X1) %>% 
  select(-X1) %>% 
  mutate(X0=lag(XTotal)) %>%
  mutate(TV=(XTotal-X0)/X0) %>% 
  select(-X0) %>%
  arrange(Code,MONTH)

#Grafico evolucion de exportaciones de cada producto

MM_PRODUCT_INDEX %>% filter(YEAR==2020) %>% 
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=Code)) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_line(aes(MONTH,INDEX,color=Code),lwd=2,alpha=0.5) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION COMERCIO POR PRODUCTO (2020)")

#Grafico tasa de variacion intermensual

MM_PRODUCT_INDEX %>% filter(YEAR==2020) %>% 
  filter(TV < 2) %>%
  ggplot() + 
  geom_point(aes(MONTH,TV,color=Code)) +
  geom_line(aes(MONTH,TV,color=Code),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,TV),linetype="dashed",se=FALSE,color="black")+
  geom_hline(yintercept=0, linetype="dashed" ) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed")+ #Octubre
  ggtitle("TV intermensual por producto (2020)")

#Tasa de variacion exportaciones 2020 - 2019 por Origen

MM_TV_ANUAL <- MercaMadrid %>% 
  group_by(YEAR,OrigenCode) %>% 
  summarise(EXPORT = sum(EXPORT_VALUE_IN_EUROS)) %>%
  filter(YEAR != 2018) %>%
  arrange(OrigenCode) %>% 
  group_by(OrigenCode) %>%
  mutate(origen = first(EXPORT)) %>%
  mutate(final = last(EXPORT)) %>% 
  mutate(TV = (final-origen)/origen * 100) %>%
  select(-origen,-final)

#Grafica

MM_TV_ANUAL %>% 
  filter(YEAR==2020) %>%
  filter(TV<50) %>%
  ggplot() +
  geom_bar(stat="identity",mapping = aes(x = OrigenCode, y = TV,
                                         fill = OrigenCode),show.legend = FALSE,position = "dodge") +
  ggtitle("TV Comercio (2019-2020)")

#Importancia de producto en las exportaciones totales

(MM_PRODUCT_RELEVANCE <- MercaMadrid %>% 
    group_by(Code) %>% 
    summarise(EXPORTACIONES = sum(EXPORT_VALUE_IN_EUROS)) %>% 
    arrange(desc(EXPORTACIONES)))

#Importancia del pais en las exportaciones

(MM_COUNTRY_RELEVANCE <- MercaMadrid %>% 
    group_by(REPORTER) %>% 
    summarise(EXPORTACIONES = sum(EXPORT_VALUE_IN_EUROS)) %>%
    arrange(desc(EXPORTACIONES)))

#####4.3.Analisis con Covid#####

#Analisis descriptivo

summary(MercaMadridCovid)

#Correlaciones lineales

Cor_MM_Covid <- MercaMadridCovid %>% 
  select(price_mean:popData2019,Acumulados) %>%
  cor()

#Unimos Indice y Covid #Creamos Indice de Covid

#Hay que cambiar nombres de paises #Usamos script NombresPaises#

MM_INDEX <- MM_INDEX %>% 
  inner_join(Covid_Media, by = c("MONTH"="month","YEAR"="year",
                                 "REPORTER"="countriesAndTerritories")) 

MM_INDEX<-MM_INDEX %>% relocate(MONTH,YEAR,REPORTER,INDEX,INDEX_D)

#Grafico comparativo; Acumulados e Indice de comercio

plot1 <- MM_INDEX %>% 
  filter(INDEX < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL COMERCIO POR ORIGEN  (ENERO 2020 = 100)")

plot2 <- MM_INDEX %>% 
  filter(INDEX_D < 250) %>%
  ggplot() + 
  geom_point(aes(MONTH,INDEX_D,color=REPORTER)) +
  geom_line(aes(MONTH,INDEX_D,color=REPORTER),lwd=2,alpha=0.5) +
  geom_smooth(aes(MONTH,INDEX_D),linetype="dashed",se=FALSE,color="black")+
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") +  #Octubre
  ggtitle("EVOLUCION DEL INDICE DE COVID POR ORIGEN")

grid.arrange(plot1, plot2, ncol=1)



#####4.4. Analisis especifico#####

#Con FILTRO() seleccionamos region y producto de nuestras bases de datos

FILTRO(MercaMadridCovid,"Morocc","96") 

summary(FILTRO(MercaMadridCovid,"Morocc","96"))

#Media 

FILTRO(MercaMadridCovid,"Morocc","96") %>% 
  select(EXPORT_VALUE_IN_EUROS) %>% 
  summarise(MEDIA=mean(EXPORT_VALUE_IN_EUROS))

#Desviacion tipica

FILTRO(MercaMadridCovid,"Morocc","96") %>% 
  select(EXPORT_VALUE_IN_EUROS) %>% 
  summarise(DT=sd(EXPORT_VALUE_IN_EUROS))

#Coeficiente de variacion

FILTRO(MercaMadridCovid,"Morocc","96") %>% 
  select(EXPORT_VALUE_IN_EUROS) %>% 
  summarise(CV=CV(EXPORT_VALUE_IN_EUROS))

#Comparacion evolucion covid y producto

plot1 <- FILTRO(MercaMadridCovid,"Morocc","96") %>%
  group_by(MONTH) %>%
  summarise(EXPORT_VALUE_IN_EUROS=sum(EXPORT_VALUE_IN_EUROS))%>%
  ggplot() +
  geom_line(aes(MONTH,EXPORT_VALUE_IN_EUROS),lwd=1.25) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  ggtitle("EVOLUCION EXPORTACIONES DE TOMATES")

plot2 <- FILTRO(MercaMadridCovid,"Morocc","96") %>%
  group_by(MONTH) %>%
  summarise(deaths=sum(deaths)) %>%
  ggplot() +
  geom_line(aes(MONTH,deaths),lwd=1.25) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  ggtitle("EVOLUCION COVID")

grid.arrange(plot1, plot2, ncol=1)

#Tasa de variacion intermensual

TV_MARRUECOS_PATATA_CE <- FILTRO(MercaMadridCovid,"Morocc","96") %>%
  filter(YEAR==2020) %>%
  group_by(MONTH) %>%
  summarise(EXPORT_VALUE_IN_EUROS=sum(EXPORT_VALUE_IN_EUROS)) %>%
  mutate(origen = lag(EXPORT_VALUE_IN_EUROS)) %>%
  mutate(TV = (EXPORT_VALUE_IN_EUROS-origen)/origen) %>%
  mutate(Exportaciones = EXPORT_VALUE_IN_EUROS ) %>%
  select(MONTH,Exportaciones,TV)