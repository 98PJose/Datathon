# Datathon
Project for Datathon competition 2021. Agricultural trade data analysis

El script SintesisResumen incluye una versión simplificada 
del proyecto con los principales resultados.

En primer lugar hay que ejecutar los scripts Librerias y Funciones en este orden, nos
proporcionan las herramientas para realizar el resto del proyecto. 
La libreria principal es tidyverse.

Para obtener los datos en la sesion de R lo mas sencillo es ejecutar el archivo DatosR 
o DatosRSintesis. Es su defecto se puede ejecutar ObtencionTratamientoV2 que es el script
con el que se creó los datos para este archivo.

ObtencionTratamientoV2 es el script en el que se transforman y obtienen las bases de datos
en el formato adecuado, para ejecutarlo hay que prestar atencion a la linea que requiere 
usar el script NombresPaises para poder unificar MercaBarna y MercaMadrid con Covid.

AnalisisResultadosV2 es la version amplia del analisis en el proyecto e incluye todos
los procedimientos, graficos y resultados.

ModelosCovid es el script donde se desarrollan comparan y seleccionan los modelos 
para ajustar el Covid.

PronosticoPrevision es un script donde se desarrollan modelos para hacer regresiones sobre
el precio de un bien agrícola.

Todas las bases de datos se encuentran en Datasets.zip
