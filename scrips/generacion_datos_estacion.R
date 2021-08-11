

library(tidyverse)
library(dplyr)
library(lubridate)
library(raster)
library(sf)
library(stars)
library(dplyr)
library(SPEI)
library(sirad)
library(naniar)

#Datos estacion   ---------------------------------------------------------------------------------
in.csv<-"data/estacion"
out.csv<-"output/rds"


dir(in.csv, full.names = T, pattern = "trigo") %>%
  read.csv(., sep=";", dec=".", skip = 2)%>%
  as_tibble() %>%
  mutate(., na_if(., "#N/D")) %>%
  mutate_at(-1, as.numeric) %>%
  mutate(cultivo="trigo")->datos.est.trigo

dir(in.csv, full.names = T, pattern = "maiz") %>%
  read.csv(., sep=";", dec=".", skip = 2)%>%
  as_tibble() %>%
  mutate(., na_if(., "#N/D")) %>%
  mutate_at(-1, as.numeric) %>%
  mutate(cultivo="maiz")->datos.est.maiz

bind_rows(datos.est.trigo,datos.est.maiz) %>% 
  mutate(Timestamps=dmy_hm(Timestamps)) %>%
  drop_na(1)->datos.estaciones 
clima<-datos.estaciones[, c(1:3, 6:11, 15:16, c(17,20,23),  30)] 



#Composicion diaria ---------------------------------------------------------------------------------
#pp y soil moisture (suma)
clima %>% 
  group_by(cultivo,Timestamps=as_date(floor_date(Timestamps, "1 day")))  %>%
  summarise_at(vars(c(2,11:13)), sum, na.rm = T)->pp

#temperatura min y max (promedio)
clima %>% 
  dplyr::select(1,7, 15) %>%
  group_by(cultivo,Timestamps=as_date(floor_date(Timestamps, "1 day")))  %>%
  summarize(tmin=min(X.C.Air.Temperature), tmax=max(X.C.Air.Temperature))->tem

#clima (promedio)
clima %>%
  group_by(cultivo, Timestamps=as_date(floor_date(Timestamps, "1 day"))) %>%
  summarize_at(vars(c(4,1,7,9, 8)), mean)->c1 #wind speed, solar radiation, vapor pressure, rh, atm pressure

inner_join(c1, tem) %>%
  inner_join(pp)->clima2
names(clima2)<-c("cultivo","fecha", "ws", "r", "vp", "rh", "atm", "tmin", "tmax", "pp", "wc15", "wc30", "wc45")



#Calculo Evapotranspiracion ---------------------------------------------------------------------------------
clima2%>% filter(cultivo=="maiz")->clima2.m
clima2%>% filter(cultivo=="trigo")->clima2.t

ref <- clima2.m$r/0.0864 #http://www.fao.org/3/x0490s/x0490s04.pdf
i <- as.Date(clima2.m$fecha) # evaluar con los dias del año
latr <- radians(-36.3870)
tal.m<-cst(ref,i,latr)

ref.t <- clima2.t$r/0.0864 
i.t <- as.Date(clima2.t$fecha)
tal.t<-cst(ref.t,i.t,latr)

# et0<-penman(Tmin=c1$degree_C.Air.Temperature, Tmax=c1$degree_C.Air.Temperature+5, U2= c1$m.s.Wind.Speed, Ra = NA, lat=-36.3870, Rs=c1$W.m2.Solar.Radiation, tsun = NA, 
#        ed=c1$kPa.Vapor.Pressure, P=c1$kPa.Atmospheric.Pressure, z=600 )#external radiation?, horas de sol

etr.t= et0(clima2.t$tmax, clima2.t$tmin, clima2.t$vp, clima2.t$r/0.0864, tal=tal.t, z=600,
           clima2.t$ws, extraT=NA, days=i.t, lat=-36.3870 )

etr.m= et0(clima2.m$tmax, clima2.m$tmin, clima2.m$vp, clima2.m$r/0.0864, tal=tal.m, z=600,
           clima2.m$ws, extraT=NA, days=i, lat=-36.3870 )

clima2.m$et0<-etr.m
clima2.t$et0<-etr.t

bind_rows(clima2.m, clima2.t) %>%
  gather(variable, valor, -fecha, -cultivo)->clima3



#Separar variables de flujo (se suman) y de estado (se promedian) -------------------------------------------
clima3 %>% 
  filter(variable %in% c("tmin", "tmax", "vp", "rh", "atm", "ws","wc15", "wc30", "wc45"))->clima3.estado
clima3 %>% 
  filter(variable %in% c("pp","et0"))->clima3.flujo



#Exportar ---------------------------------------------------------------------------------
setwd(out.csv)
write_rds(clima3.estado, "tibble_clima_estado.rds")
write_rds(clima3.flujo, "tibble_clima_flujo.rds")
