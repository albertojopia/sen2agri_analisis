library(tidyverse)
library(dplyr)
library(lubridate)
library(raster)
library(sf)
library(stars)
library(dplyr)


in.biomasa<-"data/biomasa"
in.vector<-"data/shp"
#in.vector<-"E:/ALBERTO/HEMERA/GITHUB/sen2agri_analisis/data/shp"
in.raster2<-"E:/ALBERTO/HEMERA/PROYECTO/sen2agri"
out.r<-"output/csv"
out.rds<-"output/rds"

#Leer datos ----------------------------------------------------------------------
#raster ndvi
list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "all_ndvis.tif$") %>%
  read_stars() ->ndvi

#fechas ndvi
list.files(in.raster2, full.names = T, pattern = "l3b_ndvi_05") %>%
  list.files(., full.names = T, pattern = "all_dates") %>%
  read.csv(header=FALSE)->fechas
fechas.ndvi<-fechas$V1 %>% ymd()
ndvi %>%   st_set_dimensions(3, values = fechas.ndvi)->ndvi2 


#raster lai
list.files(in.raster2, full.names = T, pattern = "l3b_lai_05") %>%
  list.files(., full.names = T, pattern = "LAI_img.tif$") %>%
  read_stars()->lai

#fechas lai
fechas.lai<-ymd(substr(names(lai), start = 12, stop=19))
names(lai)<-as.character(fechas.lai)


#vector
list.files(in.vector, full.names = T, pattern="coberturas2.shp$") %>%
  read_sf()->coberturas



#NDVI   --------------------------------------------------------------------------------
#extrarer ndvi
coberturas %>%
  filter(LC=="cultivo") %>%
  st_geometry() ->cultivos.sf

st_crop(ndvi2,cultivos.sf) %>%
  st_as_stars()->ndvi.cultivos

ndvi.cultivos[[1]][ndvi.cultivos[[1]] < 0]=NA

#LAI   --------------------------------------------------------------------------------
#extrarer lai
st_crop(lai,cultivos.sf) %>%
  st_as_stars() %>%
  merge() %>%
  st_set_dimensions(names = c("x", "y", "band")) %>%
  st_set_dimensions(3, values = fechas.lai)->lai.cultivos

lai.cultivos[[1]][lai.cultivos[[1]] < 0]=NA
lai.cultivos[[1]][lai.cultivos[[1]] < 0]=NA


color2<-colorRampPalette(c("#C2543C", "#D97529","#EDA813", "#F7D707", "#C6F700","#35E300", "#0EC441", "#1E9E84", 
                           "#166D8A", "#0C2F7A"))
plot(ndvi.cultivos,col = color2(10), breaks=seq(0,1, by=0.1), box_col = grey(1))
plot(lai.cultivos,col = color2(10))


#exportar
setwd(out.rds)
write_rds(ndvi.cultivos, "stars_ndvi.rds")
write_rds(lai.cultivos, "stars_lai.rds")


