# sen2agri_analisis

Estructura de carpetas

data: contiene los datos brutos que requieron un procesamiento para ser utilizados
  - biomasa: registro de la muestra de terreno, el peso indicado incluye el peso del sobre.
  - estacion: datos descargafos de la plataforma zentra
  - shp: vector de los puntos de muestreo y predios
 
output: contiene los datos procesados para ser utilizados en el reporte
  - rds: objetos sf, stars y tibble de las variables utilizadas en el reporte
  - report: archivos rmd para generar el reporte
  
scrips: codigos .R utilizados para procesar los datos brutos  y generar los archivos rds

scrips archivados:  codigos .R antiguos que actualmente no son utilizados
