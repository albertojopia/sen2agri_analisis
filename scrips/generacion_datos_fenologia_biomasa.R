library(tidyverse)
library(dplyr)
library(lubridate)
library(raster)
library(sf)
library(stars)
library(dplyr)


in.biomasa<-"data/biomasa"
in.vector<-"data/shp"
in.raster2<-"E:/ALBERTO/HEMERA/PROYECTO/sen2agri"
#in.vector<-"E:/ALBERTO/HEMERA/GITHUB/sen2agri_analisis/data/shp"
out.rds<-"output/rds"


#Leer datos ----------------------------------------------------------------------
#raster ndvi y fenologia
list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "metric_estimation.tif$") %>%
  read_stars()->pheno2

list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "all_ndvis.tif$") %>%
  read_stars()->ndvi2


#raster lai
list.files(in.raster2, full.names = T, pattern = "l3b_lai_05") %>%
  list.files(., full.names = T, pattern = "LAI_img.tif$") %>%
  read_stars()->lai
names(lai)<-substr(names(lai), start = 12, stop=19)


#vectores
list.files(in.vector, full.names = T, pattern="coberturas2.shp$") %>%
  read_sf()->coberturas
 
list.files(in.vector, full.names = T, pattern="pts_muestreo.shp$") %>%
  read_sf()-> puntos



#biomasa
dir(in.biomasa, full.names = T, pattern = "trigo") %>%
  read.csv(., sep=";") %>% as_tibble() %>%
  mutate(cultivo="trigo") %>%
  mutate(peso=if_else(estructura %in% c("tallo","hoja", "espiga"), peso-19.3, peso)) ->datos.trigo

dir(in.biomasa, full.names = T,  pattern = "maiz") %>%
  read.csv(., sep=";") %>% as_tibble() %>%
  mutate(cultivo="maiz") %>%
  mutate(peso=peso-19.3)->datos.maiz



#Biomasa    ---------------------------------------------------------------------------------
names(datos.trigo)<-names(datos.maiz)
rbind(datos.trigo,datos.maiz) %>%
  mutate(fecha=dmy(fecha)) %>%
  inner_join(puntos) %>%
  st_as_sf() %>%
  select_at(., vars(-id))->datos.muestreo



#Fenologia   --------------------------------------------------------------------------------
#fechas ndvi
list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "all_dates") %>%
  read.csv(header=FALSE)->fechas

fechas.n18<-fechas$V1 %>% ymd()

#extrarer fenologia
st_extract(pheno2,puntos) %>%
  as_tibble() %>% 
  spread(band,metric_estimation.tif )%>%
  inner_join(puntos)%>%
  mutate(cultivo=rep(c("trigo", "maiz"), c(5,5)), variable="feno") %>%
  gather(band, valor, -muestra, -cultivo,-geometry, -variable) %>%
  mutate(metric_fecha=as.Date(valor-1, origin="2020-01-01")) %>%
  rename(fecha=metric_fecha)%>%
  st_as_sf()->tabla.f

#NDVI -------------------------------------------------------------------------------------
#extrarer ndvi
ndvi2 %>%
  st_extract(.,puntos) %>%
  as_tibble() %>% 
  spread(band,all_ndvis.tif) %>%
  inner_join( puntos)->ndvi3

names(ndvi3)<-c("geometry", as.character(fechas.n18), "muestra", "cultivo")
ndvi3[c(-11,-15)]->ndvi4 #porque hay dos fechas repetidas

ndvi4 %>%
  gather(fecha, ndvi, -muestra, -cultivo, -geometry) %>%
  mutate(fecha=ymd(fecha)) %>%
  filter(ndvi>=0) %>%
  group_by(cultivo, muestra) %>%
  mutate(cndvi=cumsum(ndvi)) %>%
  gather(variable, valor, -geometry, -muestra, -cultivo, -fecha) ->tabla.n

#LAI   --------------------------------------------------------------------------------
#extraer lai
st_extract(lai,puntos) %>%
  as_tibble() %>%
  inner_join( puntos) %>%
  gather(fecha, lai, -geometry, -muestra, -cultivo) %>%
  mutate(fecha=ymd(fecha)) %>%
  filter( lai>=0) %>%
  group_by(cultivo, muestra) %>%
  mutate(clai=cumsum(lai)) %>%
  gather(variable, valor, -geometry, -muestra, -cultivo, -fecha) ->tabla.lai


#Compilacion NDVI + LAI ----------------------------------------------------------------

tabla.vege<-bind_rows(tabla.lai, tabla.n) %>% st_as_sf()

#Exportar datos   ----------------------------------------------------------------------
setwd(out.rds)
write_rds(datos.muestreo, "sf_datos_muestreo.rds")
write_rds(tabla.f, "sf_datos_feno.rds" )
write_rds(tabla.vege, "sf_datos_vege.rds")


