###Obtención de datos y tratamiento inicial###

##Obtención de datos

####1.Consumo por CCA####

ConsumoCCAA <- read_delim("Dataset1.- DatosConsumoAlimentarioMAPAporCCAA.txt", 
                          "|", escape_double = FALSE, trim_ws = TRUE)

#Simplificamos nombres de variables
names(ConsumoCCAA) <- c("Año","Mes","CCAA","Producto","Volumen",
                        "Valor","Precio","Penetración","Consumopc","Gastopc","X11","X12")

#Hay variables que sobran
ConsumoCCAA <- ConsumoCCAA %>% select(Año:Gastopc)

#Algunas variables son character por tener (,)
#Con parse_double() las convertimos en numeros con decimales.

ConsumoCCAA = ConsumoCCAA %>%
  mutate(Volumen = parse_double(Volumen,locale = locale(decimal_mark = ","))) %>%
  mutate(Precio = parse_double(Precio,locale = locale(decimal_mark = ","))) %>%
  mutate(Penetración = parse_double(Penetración,locale = locale(decimal_mark = ","))) %>%
  mutate(Consumopc = parse_double(Consumopc,locale = locale(decimal_mark = ","))) %>%
  mutate(Gastopc = parse_double(Gastopc,locale = locale(decimal_mark = ",")))

#Valores ausentes (NA)

ConsumoCCAA[!complete.cases(ConsumoCCAA),]

#Imputamos los valores ausentes mediante algoritmo KNN

ConsumoCCAA<-kNN(ConsumoCCAA, k=15)

ConsumoCCAA <- tibble(ConsumoCCAA %>% select(Año:Gastopc))

#Valores atípicos

#Usamos distancia de Mahalanobis para detectarlos

maha.CCAA <- mahalanobis(ConsumoCCAA[,c(5:10)], 
                         center = colMeans(ConsumoCCAA[,c(5:10)]),
                         cov = cov(ConsumoCCAA[,c(5:10)])) #colmeans es la media por columnas (variables)

plot(maha.CCAA, pch=22, col="blue",main="Outliers")
abline(h=200,col="red")

#Eliminamos los que tienen una distancia superior a 200 en base al grafico
quitar.CCAA<-which(maha.CCAA>200)
ConsumoCCAA<-ConsumoCCAA[-quitar.CCAA,]

#Limpiamos enviroment
rm(maha.CCAA,quitar.CCAA)

#Convertimos en factor los caracteres correspondientes

ConsumoCCAA = ConsumoCCAA %>% mutate(Producto=as.factor(Producto),
                                     CCAA=as.factor(CCAA))

#Convertimos chr a dbl

ConsumoCCAA = ConsumoCCAA %>% 
  mutate(Mes =  str_replace(Mes,"Enero","1")) %>%
  mutate(Mes =  str_replace(Mes,"Febrero","2")) %>%
  mutate(Mes =  str_replace(Mes,"Marzo","3")) %>%
  mutate(Mes =  str_replace(Mes,"Abril","4")) %>%
  mutate(Mes =  str_replace(Mes,"Mayo","5")) %>%
  mutate(Mes =  str_replace(Mes,"Junio","6")) %>%
  mutate(Mes =  str_replace(Mes,"Julio","7")) %>%
  mutate(Mes =  str_replace(Mes,"Agosto","8")) %>%
  mutate(Mes =  str_replace(Mes,"Septiembre","9")) %>%
  mutate(Mes =  str_replace(Mes,"Octubre","10"))%>%
  mutate(Mes =  str_replace(Mes,"Noviembre","11"))%>%
  mutate(Mes =  str_replace(Mes,"Diciembre","12"))

ConsumoCCAA = ConsumoCCAA %>% mutate(Mes = as.double(Mes))

####2.Precios semanales####

PreciosSemanales <- read_delim("Dataset2.- Precios Semanales Observatorio de Precios Junta de Andalucia.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE)

#Cambiamos separador decimal. parse_double()

PreciosSemanales = PreciosSemanales %>%
  mutate(PRECIO = parse_double(PRECIO,locale = locale(decimal_mark = ",")))

#Valores ausentes (NA)

PreciosSemanales[!complete.cases(PreciosSemanales),]

rowSums(is.na(PreciosSemanales)) #Cuantos NAs tiene cada observacion
colSums(is.na(PreciosSemanales)) #Cuantos NAs tiene cada variable

#Quitamos variables con NA

PreciosSemanales = PreciosSemanales %>% select(INICIO:PRODUCTO,POSICION,PRECIO)

#Valores atipicos

#Usamos distancia de Mahalanobis para detectarlos

maha.semana <- mahalanobis(PreciosSemanales[,8], 
                           center = colMeans(PreciosSemanales[,8]),
                           cov = cov(PreciosSemanales[,8])) #colmeans es la media por columnas (variables)

plot(maha.semana, pch=22, col="blue",main="Outliers")
abline(h=2*mean(maha.semana),col="red")

#Eliminamos los que tienen una distancia superior a 2 veces la media.
quitar.semana<-which(maha.semana>2*mean(maha.semana))
length(quitar.semana)
PreciosSemanales<-PreciosSemanales[-quitar.semana,]

#Limpiamos enviroment
rm(maha.semana,quitar.semana)

#Convertimos en factor

PreciosSemanales = PreciosSemanales %>% mutate(POSICION=as.factor(POSICION),
                                               PRODUCTO=as.factor(PRODUCTO),
                                               SUBSECTOR=as.factor(SUBSECTOR),
                                               SECTOR=as.factor(SECTOR),
                                               GRUPO=as.factor(GRUPO))

####3.Merca Madrid####

MercaMadrid <- read_delim("Dataset3a_Datos_MercaMadrid.txt", 
                          "|", escape_double = FALSE, trim_ws = TRUE)
#Solucionamos separador decimal

MercaMadrid = MercaMadrid %>%
  mutate(price_mean = parse_double(price_mean,locale = locale(decimal_mark = ","))) %>%
  mutate(price_min = parse_double(price_min,locale = locale(decimal_mark = ","))) %>%
  mutate(price_max = parse_double(price_max,locale = locale(decimal_mark = ","))) 

#Valores Ausentes

rowSums(is.na(MercaMadrid)) #Cuantos NAs tiene cada observacion
colSums(is.na(MercaMadrid)) #Cuantos NAs tiene cada variable

#Valores atipicos

#Usamos distancia de Mahalanobis para detectarlos

maha.madrid <- mahalanobis(MercaMadrid[,c(8:11)], 
                           center = colMeans(MercaMadrid[,c(8:11)]),
                           cov = cov(MercaMadrid[,c(8:11)])) #colmeans es la media por columnas (variables)

plot(maha.madrid, pch=22, col="blue",main="Outliers")
abline(h=15000,col="red")

#Eliminamos los que tienen una distancia superior a 15000
#Las observaciones entorno a 1K, 2K y 4K muestran un patron que puede ser informacion relevante
quitar.madrid<-which(maha.madrid>15000)
length(quitar.madrid)
MercaMadrid<-MercaMadrid[-quitar.madrid,]

#Limpiamos posibles outliers en dicho patron repitiendo el procedimiento

maha.madrid <- mahalanobis(MercaMadrid[,c(8:11)], 
                           center = colMeans(MercaMadrid[,c(8:11)]),
                           cov = cov(MercaMadrid[,c(8:11)])) #colmeans es la media por columnas (variables)

plot(maha.madrid, pch=22, col="blue",main="Outliers")
abline(h=500,col="red")

#Eliminamos los que tienen una distancia superior a 500 en base al grafico
quitar.madrid<-which(maha.madrid>500)
length(quitar.madrid)
MercaMadrid<-MercaMadrid[-quitar.madrid,]

#Limpiamos enviroment
rm(maha.madrid,quitar.madrid)

#Convertimos en factor

MercaMadrid = MercaMadrid %>% mutate(familia=as.factor(familia),
                                     Unidad=as.factor(Unidad),
                                     origen=as.factor(origen),
                                     variedad=as.factor(variedad),
                                     product=as.factor(product))

####4.Merca Barna####

MercaBarna <- read_delim("Dataset3b_Datos_MercaBarna.txt", 
                         "|", escape_double = FALSE, trim_ws = TRUE)

#Valores Ausentes

rowSums(is.na(MercaBarna)) #Cuantos NAs tiene cada observacion
colSums(is.na(MercaBarna)) #Cuantos NAs tiene cada variable

#Cambiamos separador decimal del precio medio

MercaBarna = MercaBarna %>%
  mutate(price_mean = parse_double(price_mean,locale = locale(decimal_mark = ",")))

#Valores atipicos

#Usamos distancia de Mahalanobis para detectarlos

maha.barna <- mahalanobis(MercaBarna[,c(8,7)], 
                          center = colMeans(MercaBarna[,c(8,7)]),
                          cov = cov(MercaBarna[,c(8,7)])) #colmeans es la media por columnas (variables)

plot(maha.barna, pch=22, col="blue",main="Outliers")
abline(h=200,col="red")

#Eliminamos los que tienen una distancia superior a 200 en base al grafico
#Hay cierto patron entorno a 2K y 3K que puede ser informacion relevante

quitar.barna<-which(maha.barna>2*mean(maha.barna))
length(quitar.barna)
MercaBarna<-MercaBarna[-quitar.barna,]

#Repetimos el procedimiento para quitar outliers dentro del patron

#Usamos distancia de Mahalanobis para detectarlos

maha.barna <- mahalanobis(MercaBarna[,c(8,7)], 
                          center = colMeans(MercaBarna[,c(8,7)]),
                          cov = cov(MercaBarna[,c(8,7)])) #colmeans es la media por columnas (variables)

plot(maha.barna, pch=22, col="blue",main="Outliers")
abline(h=30,col="red")

#Por encima de 30 parecen estar los outliers del patron

quitar.barna<-which(maha.barna>30)
length(quitar.barna)
MercaBarna<-MercaBarna[-quitar.barna,]

#Limpiamos enviroment
rm(maha.barna,quitar.barna)

#Convertimos en factor

MercaBarna = MercaBarna %>% mutate(familia=as.factor(familia),
                                   Unidad=as.factor(Unidad),
                                   origen=as.factor(origen),
                                   product=as.factor(product))

#Ordenamos fecha al principio

MercaBarna = MercaBarna %>% relocate(YEAR,MONTH)

####4.Comercio Exterior####

ComercioExterior <- read_delim("Dataset4.- Comercio Exterior de Espana.txt", 
                               "|", escape_double = FALSE, trim_ws = TRUE)
#Los 0 son : lo que crea vector de caracteres. Solucionamos reemplazando en .txt

#Hay que solucionar NA´s

ComercioExterior[6433,]

ComercioExterior<-ComercioExterior%>%mutate_all(funs(replace_na(., 0)))

#Reordenamos datos (Hay 4 filas por observacion)
ComercioExterior = ComercioExterior %>% 
  pivot_wider(names_from = c("FLOW", "INDICATORS"), values_from = "Value") %>%
  select(PERIOD:EXPORT_QUANTITY_IN_100KG)

#Solucionamos listas columna
ComercioExterior <- ComercioExterior %>% unnest(cols = c(IMPORT_VALUE_IN_EUROS, 
                                                         IMPORT_QUANTITY_IN_100KG, 
                                                         EXPORT_VALUE_IN_EUROS, 
                                                         EXPORT_QUANTITY_IN_100KG))
#Creamos chuleta con los nombres y codigo

PRODUCT_CE_NAME= ComercioExterior %>% 
  group_by(PRODUCT) %>%
  summarise(sum(IMPORT_VALUE_IN_EUROS)) %>%
  select(PRODUCT) %>%
  mutate(Code=as.factor(seq(1,86,1)))

#Unimos codigo a la base de datos
ComercioExterior=ComercioExterior %>% 
  inner_join(PRODUCT_CE_NAME,by=c("PRODUCT"="PRODUCT")) %>% 
  relocate(PERIOD,REPORTER,PARTNER,PRODUCT,Code)

#Simplificamos nombre de paises

ComercioExterior <- ComercioExterior %>% mutate(REPORTER = str_sub(REPORTER, 1, 6))

ComercioExterior <- ComercioExterior %>% 
  mutate(REPORTER=str_replace(REPORTER,fixed(" "),"")) %>% 
  mutate(REPORTER=str_trim(REPORTER,side = c("both", "left", "right")))

#Creamos variable precios 

ComercioExterior = ComercioExterior %>% 
  mutate(IMPORT_PRICE = IMPORT_VALUE_IN_EUROS / (IMPORT_QUANTITY_IN_100KG * 100),
         EXPORT_PRICE = EXPORT_VALUE_IN_EUROS / (EXPORT_QUANTITY_IN_100KG * 100))

#Quitamos Nan
ComercioExterior<-ComercioExterior%>%mutate_all(funs(replace_na(., 0)))

#Quitamos infinitos #Cuando la cantidad es 0 surgen

ComercioExterior<-ComercioExterior %>% 
  mutate(EXPORT_PRICE = ifelse(EXPORT_PRICE == Inf,0, EXPORT_PRICE),
         IMPORT_PRICE = ifelse(IMPORT_PRICE == Inf,0, IMPORT_PRICE))  

#Separar columna de fechas en mes y año

ComercioExterior<-ComercioExterior%>%separate(PERIOD,c("MONTH","YEAR"),sep = ". ")

#Quitamos filas resumen (Datos de enero a diciembre)

ComercioExterior<-ComercioExterior%>%filter(!MONTH=="Jan.-Dec")

#Valores atipicos

#Usamos distancia de Mahalanobis para detectarlos

maha.comercio <- mahalanobis(ComercioExterior[,c(6:11)], 
                             center = colMeans(ComercioExterior[,c(6:11)]),
                             cov = cov(ComercioExterior[,c(6:11)])) #colmeans es la media por columnas (variables)

plot(maha.comercio, pch=22, col="blue",main="Outliers")
abline(h=1000,col="red")

#Eliminamos los que tienen una distancia superior a 1000 en base al grafico.
quitar.comercio<-which(maha.comercio>1000)
length(quitar.comercio)
Precioscomercioles<-Precioscomercioles[-quitar.comercio,]

#Limpiamos enviroment
rm(maha.comercio,quitar.comercio)

#Convertimos en factor los caracteres correspondientes

ComercioExterior= ComercioExterior %>% mutate(PRODUCT=as.factor(PRODUCT),
                                              PARTNER=as.factor(PARTNER),
                                              REPORTER=as.factor(REPORTER))

#Convertimos mes en numero #Para poder unir con Covid

ComercioExterior = ComercioExterior %>% 
  mutate(MONTH =  str_replace(MONTH,"Jan","1")) %>%
  mutate(MONTH =  str_replace(MONTH,"Feb","2")) %>%
  mutate(MONTH =  str_replace(MONTH,"Mar","3")) %>%
  mutate(MONTH =  str_replace(MONTH,"Apr","4")) %>%
  mutate(MONTH =  str_replace(MONTH,"May","5")) %>%
  mutate(MONTH =  str_replace(MONTH,"Jun","6")) %>%
  mutate(MONTH =  str_replace(MONTH,"Jul","7")) %>%
  mutate(MONTH =  str_replace(MONTH,"Aug","8")) %>%
  mutate(MONTH =  str_replace(MONTH,"Sep","9")) %>%
  mutate(MONTH =  str_replace(MONTH,"Oct","10"))%>%
  mutate(MONTH =  str_replace(MONTH,"Nov","11"))%>%
  mutate(MONTH =  str_replace(MONTH,"Dec","12"))

ComercioExterior = ComercioExterior %>% mutate(MONTH = as.double(MONTH))

ComercioExterior = ComercioExterior %>% mutate(YEAR = as.double(YEAR))

####5.Covid####

Covid <- read_delim("Dataset5_Coronavirus_cases.txt", 
                    "|", escape_double = FALSE, trim_ws = TRUE)

#Simplificamos nombre
names(Covid)[12] = "Acumulados"

#Solucionamos separador decimal

Covid = Covid %>%
  mutate(Acumulados = parse_double(Acumulados,locale = locale(decimal_mark = ",")))

#Valores ausentes (NA)

Covid[!complete.cases(Covid),]

rowSums(is.na(Covid)) #Cuantos NAs tiene cada observacion
colSums(is.na(Covid)) #Cuantos NAs tiene cada variable

#Quitamos variables con outliers

Covid <- Covid %>% select(dateRep:countriesAndTerritories,popData2019:Acumulados)

#Imputamos el resto de ausentes mediante KNN

Covid<-kNN(Covid, k=5)

Covid <- tibble(Covid %>% select(dateRep:Acumulados))

#Valores atipicos

#Usamos distancia de Mahalanobis para detectarlos

maha.covid <- mahalanobis(Covid[,c(5,6)], 
                          center = colMeans(Covid[,c(5,6)]),
                          cov = cov(Covid[,c(5,6)])) #colmeans es la media por columnas (variables)

plot(maha.covid, pch=22, col="blue",main="Outliers")
abline(h=500,col="red")

#Eliminamos los que tienen una distancia superior a 500 en base al grafico.
quitar.covid<-which(maha.covid>500)
length(quitar.covid)
Covid<-Covid[-quitar.covid,]

#Limpiamos enviroment
rm(maha.covid,quitar.covid)

#Convertimos en factor

Covid = Covid %>% mutate(countriesAndTerritories=as.factor(countriesAndTerritories),
                         continentExp=as.factor(continentExp))

#Chuleta paises #Para comprobar si se pierden paises

Covid_Paises = Covid %>% group_by(countriesAndTerritories) %>% 
  summarise(n()) %>% select(countriesAndTerritories)

CE_Paises = ComercioExterior %>% group_by(REPORTER) %>% 
  summarise(n()) %>% select(REPORTER)

#Simplificamos nombre paises

Covid <- Covid %>% mutate(countriesAndTerritories = str_sub(countriesAndTerritories, 1, 6))

Covid_Paises = Covid %>% group_by(countriesAndTerritories) %>% 
  summarise(n()) %>% select(countriesAndTerritories)

#Medias mensuales

Covid_Media <- Covid %>% group_by(year,month,countriesAndTerritories) %>% 
  summarise(cases = mean(cases),deaths=mean(deaths),Acumulados=mean(Acumulados))

Covid_Media_Spain <- Covid_Media %>% filter(countriesAndTerritories=="Spain")

#Pais como factor

Covid = Covid %>% mutate(countriesAndTerritories=as.factor(countriesAndTerritories))

#Covid Spain

CovidSpain <- Covid %>% filter(countriesAndTerritories=="Spain")

###Union de bases de datos

#Comercio exterior y covid

#Base de datos completa

ComercioExteriorCovid = ComercioExterior %>%
  inner_join(Covid, by = c("MONTH"="month","YEAR"="year","REPORTER"="countriesAndTerritories"))

CEC_Paises = ComercioExteriorCovid %>% group_by(REPORTER) %>% 
  summarise(n()) %>% select(REPORTER) #CEC != CE ; Europa conjunta no esta en Covid

ComercioExteriorCovid = ComercioExteriorCovid %>% relocate(dateRep,day)

#Base de datos con Covid promedio mensual #inner solo guarda coincidencias

ComercioCovidMedia = ComercioExterior %>%
  inner_join(Covid_Media, by = c("MONTH"="month","YEAR"="year","REPORTER"="countriesAndTerritories"))

CECM_Paises = ComercioCovidMedia %>% group_by(REPORTER) %>% 
  summarise(n()) %>% select(REPORTER) #CEC != CE ; Europa conjunta no esta en Covid

#Convertimos pais en factor
ComercioCovidMedia = ComercioCovidMedia %>% mutate(REPORTER=as.factor(REPORTER))

#Consumo CCAA y Covid

#Base de datos completa

CCAACovid = ConsumoCCAA %>%
  inner_join(CovidSpain, by = c("Mes"="month","Año"="year"))

#Base de datos con promedio mensual de covid

CCAACovid_Media = ConsumoCCAA %>%
  inner_join(Covid_Media_Spain, by = c("Mes"="month","Año"="year"))

#Para poder usar funciones propias
MercaBarna <- MercaBarna %>% rename(PRODUCT = product) %>% 
  rename(REPORTER = origen) %>% rename(EXPORT_VALUE_IN_EUROS=Volumen)

MercaMadrid<-MercaMadrid %>% rename(PRODUCT = product) %>% 
  rename(REPORTER = origen) %>% rename(EXPORT_VALUE_IN_EUROS=Volumen)

###########################################################################
#Para poder juntar MercaBarna/MercaMadrid con Covid hay que traducir paises
#Aqui hay que ejecutar el script NombresPaises
###########################################################################

#Merca Barna y Covid

#Base de datos completa

MercaBarnaCovid = MercaBarna2 %>% 
  inner_join(Covid, by = c("MONTH"="month","YEAR"="year","REPORTER"="countriesAndTerritories")) %>% 
  relocate(day) %>%
  arrange(day,MONTH,YEAR,REPORTER,PRODUCT)

MercaBarnaCovid = MercaBarnaCovid %>% relocate(dateRep)

#Base de datos con promedio mensual de covid

MercaBarnaCovid_Media = MercaBarna2 %>% 
  inner_join(Covid_Media, by = c("MONTH"="month","YEAR"="year","REPORTER"="countriesAndTerritories")) %>%
  arrange(MONTH,YEAR,REPORTER,PRODUCT)

#Merca Madrid

#Base de datos completa

MercaMadridCovid = MercaMadrid2 %>% relocate(MONTH,YEAR) %>%
  inner_join(Covid, by = c("MONTH"="month","YEAR"="year","REPORTER"="countriesAndTerritories")) %>% 
  relocate(day) %>%
  arrange(day,MONTH,YEAR,REPORTER,PRODUCT)

MercaMadridCovid = MercaMadridCovid %>% relocate(dateRep,YEAR,MONTH)

#Base de datos con promedio mensual de covid

MercaMadridCovid_Media = MercaMadrid2 %>%
  inner_join(Covid_Media, by = c("MONTH"="month","YEAR"="year","REPORTER"="countriesAndTerritories")) %>%
  arrange(MONTH,YEAR,REPORTER,PRODUCT) %>% relocate(MONTH,YEAR)

MercaMadrid_origen <- MercaMadrid %>% group_by(REPORTER) %>% summarise(n()) %>% select(REPORTER)

#Precios semanales y covid

#Separamos en dia mes y ano #Convertimos chr a double

PreciosSemanales<-PreciosSemanales%>% separate(INICIO,c("DAY","MONTH","YEAR"),sep = "/")

PreciosSemanales<-PreciosSemanales %>% mutate(DAY = str_replace(DAY,"0",""))%>%
  mutate(MONTH = str_replace(MONTH,"0",""))

PreciosSemanales<-PreciosSemanales %>% mutate(DAY = as.double(DAY)) %>%
  mutate(MONTH = as.double(MONTH)) %>% mutate(YEAR = as.double(YEAR))

#Unimos

PreciosSemanalesCovid = PreciosSemanales %>% 
  inner_join(CovidSpain, by = c("DAY"="day","MONTH"="month","YEAR"="year"))

PreciosSemanalesCovid = PreciosSemanalesCovid %>% relocate(dateRep)

#Cambiamos nombres de variables para usar funciones propias en AnalisisExploratorio

ConsumoCCAA<-ConsumoCCAA %>% rename(PRODUCT = Producto) %>% 
  rename(REPORTER = CCAA) %>% rename(MONTH=Mes) %>% rename(YEAR=Año)

CCAACovid <- CCAACovid %>% rename(PRODUCT = Producto) %>% 
  rename(REPORTER = CCAA)%>%rename(MONTH=Mes) %>% rename(YEAR=Año)

CCAACovid_Media <- CCAACovid_Media %>% rename(PRODUCT = Producto) %>% 
  rename(REPORTER = CCAA) %>% rename(MONTH=Mes) %>% rename(YEAR=Año)

rm(MercaBarna2,MercaMadrid2,MercaMadrid_origen)

#CLM es un nombre muy largo

ConsumoCCAA = ConsumoCCAA %>% 
  mutate(REPORTER =  str_replace(REPORTER,"Castilla La Mancha","CLM"))


