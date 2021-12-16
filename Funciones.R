
###Funciones###

##Coeficiente de Variacion (CV)

CV <- function(x, na.rm = FALSE) {
  sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}

#Borrar filas con NA

delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}

#Funcion para filtrar base segun region y producto #Para simplificar codigo

#region y producto son chr van entre ""

FILTRO <- function(base,region,producto,nom_reg,nom_prod){
  base %>% 
    filter(REPORTER==region)%>%
    filter(Code==producto)%>% 
    arrange(YEAR,MONTH)
}

#Ejemplos

ComercioExteriorCovid %>% 
  filter(REPORTER=="France") %>% 
  filter(Code=="20") %>% 
  arrange(YEAR,MONTH,day)

FILTRO(ComercioExteriorCovid,
       region="France",
       producto="20")

FILTRO(ComercioExteriorCovid,"France","20")

#Funcion para la obtencion de pesos
#D es la base de datos

PESOS <- function(D){
  
  P <- D %>% 
    group_by(REPORTER,Code) %>% 
    summarise(Exportaciones=sum(EXPORT_VALUE_IN_EUROS)) %>% 
    mutate(Peso=(Exportaciones/sum(Exportaciones)))
  
  return(P)
  
} 

#Ejemplos

PESOS(ComercioExterior)

PESOS(MercaBarna)

#Comprobacion

PESOS(MercaBarna) %>% group_by(REPORTER) %>% 
  summarise(sum(Peso))%>%setNames(c('REPORTER','SumaPesos'))

#Indice (Se usa para resumir evolucion de muchos productos para vision general)
#Requiere base de datos con pesos
#D = Base de datos

INDEX <- function(D){
  
  IND <- D %>% group_by(MONTH,YEAR,REPORTER)%>%
    summarise(MEAN = sum(EXPORT_VALUE_IN_EUROS*Peso)) %>% 
    arrange(REPORTER,YEAR)%>% 
    mutate(INDEX=100) %>% 
    group_by(REPORTER) %>% 
    mutate(MEAN2=first(MEAN[MEAN>0])) %>% 
    mutate(INDEX = (MEAN*100)/MEAN2) %>% 
    select(-MEAN2) %>%
    mutate(X0 = lag(MEAN)) %>%
    mutate(TV = (MEAN-X0)/X0) %>%
    select(-X0)
  
  return(IND)
}

#MAPE

MAPE <- function(real,estimado){
  MAPE_v=sum(abs((real-estimado)/real))*(100/length(real)) #Vector
  MAPE_num = sum(MAPE_v)/length(real) #Resultado
  result=list(vector=MAPE_v,valor=MAPE_num)
  print("MAPE")
  return(MAPE_v)
}

