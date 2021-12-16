#####1.4. Analisis especifico##### 

#Con FILTRO() seleccionamos region y producto de nuestras bases de datos

FILTRO(ComercioExteriorCovid,"Italy","53") 

summary(FILTRO(ComercioExteriorCovid,"Italy","53"))

#Media 

FILTRO(ComercioExteriorCovid,"Italy","53") %>% 
  select(IMPORT_VALUE_IN_EUROS) %>% 
  summarise(MEDIA=mean(IMPORT_VALUE_IN_EUROS))

#Desviacion tipica

FILTRO(ComercioExteriorCovid,"Italy","53") %>% 
  select(IMPORT_VALUE_IN_EUROS) %>% 
  summarise(DT=sd(IMPORT_VALUE_IN_EUROS))

#Coeficiente de variacion

FILTRO(ComercioExteriorCovid,"Italy","53") %>% 
  select(IMPORT_VALUE_IN_EUROS) %>% 
  summarise(CV=CV(IMPORT_VALUE_IN_EUROS))

#Comparacion evolucion covid y producto

plot1 <- FILTRO(ComercioExteriorCovid,"Italy","53") %>%
  ggplot() +
  geom_line(aes(MONTH,IMPORT_VALUE_IN_EUROS),lwd=1.25) +
  geom_smooth(aes(MONTH,IMPORT_VALUE_IN_EUROS),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION EXPORTACIONES DE PIMIENTO ITALIA")

plot2 <- FILTRO(ComercioExteriorCovid,"Italy","53") %>%
  group_by(MONTH) %>%
  summarise(deaths=sum(deaths)) %>%
  ggplot() +
  geom_line(aes(MONTH,deaths),lwd=1.25) +
  geom_smooth(aes(MONTH,deaths),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION COVID ITALIA")

grid.arrange(plot1, plot2, ncol=1)

#Tasa de variacion intermensual

TV_ITALIA_PEPPER_CE <- FILTRO(ComercioExterior,"Italy","53") %>%
  filter(YEAR==2020) %>%
  mutate(origen = lag(IMPORT_VALUE_IN_EUROS)) %>%
  mutate(TV = (IMPORT_VALUE_IN_EUROS-origen)/origen) %>%
  mutate(Exportaciones = IMPORT_VALUE_IN_EUROS ) %>%
  select(MONTH,YEAR,REPORTER,Exportaciones,TV)

#Tasa de variacion interanual por mes

TVAM_ITALIA_PEPPER_CE <- FILTRO(ComercioExterior,"Italy","53") %>% 
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

TVAM_ITALIA_PEPPER_CE %>%
  ggplot() +
  geom_line(aes(MONTH,TVAM),lwd=1.25) +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  geom_hline(yintercept = 0) +
  ggtitle("TV Interanual por meses de exportaciones de pimiento a Italia (2019-2020)")

#Descomposicion temporal #Todo el espacio temporal (2018-2020)
#Estacionalidad por clima y caracteristicas biologicas del producto altera analisis
#Tomate Alemania (TA)

PepperItalia_TS_CE <- FILTRO(ComercioExterior,"Italy","53") %>% 
  select(IMPORT_VALUE_IN_EUROS) %>%
  ts(start = c(2018,1), frequency = 12) 

STL_PI<-PepperItalia_TS_CE[,1] %>% mstl(robust=TRUE) 

autoplot(STL_PI) +
  ggtitle("Descomposicion STL Pimiento Italia")

#Analisis de la estacionalidad

STL_PI[,3] %>% autoplot() #Estacionalidad

freq_PI <- periodogram(STL_PI[,3]) 

freq_PI[["freq"]]

freq_PI[["spec"]]


STL_PI_SE <- STL_PI[,2] + STL_PI[,4] #Datos sin estacionalidad trend + remainer

PI_SE_CE <- tibble(FILTRO(ComercioExterior,"Italy","53"),STL_PI_SE) %>% 
  inner_join(Covid_Media,by = c("REPORTER"="countriesAndTerritories","MONTH"="month","YEAR"="year")) %>%
  select(MONTH:REPORTER,STL_PI_SE,INDEX_D,deaths)

#Grafico

plot1 <- PI_SE_CE %>%
  ggplot() +
  geom_line(aes(MONTH,STL_PI_SE),lwd=1.25) +
  geom_smooth(aes(MONTH,STL_PI_SE),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION DESESTACIONALIZADA EXPORTACIONES DE TOMATES ALEMANIA")

plot2 <- PI_SE_CE %>%
  ggplot() +
  geom_line(aes(MONTH,INDEX_D),lwd=1.25) +
  geom_smooth(aes(MONTH,INDEX_D),lwd=1,color="blue",se=FALSE,linetype="dashed") +
  geom_vline(xintercept = 3,linetype="dashed") + #Marzo
  geom_vline(xintercept = 10,linetype="dashed") + #Octubre
  ggtitle("EVOLUCION COVID ALEMANIA")

grid.arrange(plot1, plot2, ncol=1)

#Tasa de variacion interanual por mes Sin Estacionalidad(SE)

TVAM_ALEMANIA_PATATA_SE_CE <- tibble(FILTRO(ComercioExterior,"Italy","53"),STL_TA_SE) %>% 
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