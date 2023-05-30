##===============================================================#
## Process shapefiles to harmonize data for Bogotá
## Author: Guido España
## Date: 2020/09/08
##===============================================================#
## Setup-------------
##===============================================================#
library(dplyr)
library(tidyverse)
library(rjson)
library(sf)
library(raster)
library(rgdal) #Please note that rgdal will be retired during 2023
library(maptools)
library(osmdata)
library(RColorBrewer)
library(osmplotr)
library(RCurl)
library(data.table)
options(digits = 20,scipen = 999)


##===============================================================#
## Shapefiles-------------
##===============================================================#
# upz_shp = rgdal::readOGR('data/raw_data/geodata/UPZ_Bogota/UPla.shp')
upz_shp = sf::st_read('data/raw_data/geodata/UPZ_Bogota/UPla.shp')
# localities_shp = rgdal::readOGR('data/raw_data/geodata/localidades_bogota/poligonos-localidades.shp')
localities_shp = sf::st_read('data/raw_data/geodata/localidades_bogota/poligonos-localidades.shp')
# esc_shp = rgdal::readOGR('data/raw_data/geodata/scat_shp/scat_shp.shp')
esc_shp = sf::st_read('data/raw_data/geodata/scat_shp/scat_shp.shp')
# esc_data = esc_shp@data[,c('SCACODIGO', 'SCANOMBRE')]
esc_data = esc_shp[,c('SCACODIGO', 'SCANOMBRE')]
esc_data$Localidad = 0

#esc_coor = coordinates(esc_shp)
esc_coor = sf::st_coordinates(esc_shp)
esc_coor <- as_tibble(esc_coor) %>% group_by(L3) %>% 
  summarise(X=mean(X), Y=mean(Y))
esc_coor <- esc_coor[,2:3]
colnames(esc_coor) = c("LON", "LAT")
esc_coor = as.data.frame(esc_coor)
#coordinates(esc_coor) = ~ LAT + LON
esc_coor <- st_as_sf(esc_coor, coords = c("LON", "LAT"))
#proj4string(esc_coor) = proj4string(localities_shp)
st_crs(esc_coor) <- st_crs(localities_shp)

for(ll in 1:nrow(localities_shp)){
    print(ll)
    #esc_locality = sp::over(esc_coor, localities_shp[ll,])
    esc_locality = sf::st_join(esc_coor, localities_shp[ll,])
    esc_data$Localidad[which(!is.na(esc_locality$Nombre_de_l))] = localities_shp$Identificad[ll]
}

#Continue the migration to sf (there is a bug here)
esc_data$Localidad[esc_data$SCACODIGO == '108108'] = 1
esc_shp@data$Localidad = esc_data$Localidad
esc_list = unique(as.character(esc_data$SCACODIGO))
esc_shp@data$ZONE = as.character(esc_shp@data$SCACODIGO)

write_csv(esc_data, 'data/processed_data/geodata/Localidad_Unidad_Catastral.csv')

block_shp = rgdal::readOGR('data/processed_data/geodata/manzanas_bogota/manzanas_bogota.shp')
block_coor = coordinates(block_shp)
colnames(block_coor) = c("LAT", "LON")
block_coor = as.data.frame(block_coor)
coordinates(block_coor) = ~ LAT + LON 
proj4string(block_coor) = proj4string(esc_shp)

block_data = block_shp@data
block_data$SCACODIGO = "0"
for(ee in 1:nrow(esc_shp)){
    block_esc = sp::over(block_coor, esc_shp[ee,])
    block_data$SCACODIGO[which(!is.na(block_esc[,1]))] = as.character(esc_shp@data$SCACODIGO[ee])
}

block_shp@data$SCACODIGO = block_data$SCACODIGO

write_csv(block_data, 'data/processed_data/geodata/Manzana_Unidad_Catastral.csv')


##===============================================================#
## UPZ Shapefiles-------------
##===============================================================#
upz_shp = rgdal::readOGR('data/raw_data/geodata/UPZ_Bogota/UPla.shp')
esc_shp = rgdal::readOGR('data/raw_data/geodata/scat_shp/scat_shp.shp')
esc_data = esc_shp@data[,c('SCACODIGO', 'SCANOMBRE')]
esc_data$upz = ""
esc_coor = coordinates(esc_shp)
colnames(esc_coor) = c("LAT", "LON")
esc_coor = as.data.frame(esc_coor)
coordinates(esc_coor) = ~ LAT + LON
proj4string(esc_coor) = proj4string(upz_shp)

for(uu in 1:nrow(upz_shp)){
    print(uu)
    esc_upz = sp::over(esc_coor, upz_shp[uu,])
    esc_data$upz[which(!is.na(esc_upz[,1]))] = as.character(upz_shp@data$UPlCodigo[uu])
}

if(length(which(esc_data$upz == "")) > 0){
    esc_data$upz[which(esc_data$upz == "")] = as.character(upz_shp@data$UPlCodigo[as.numeric(apply(rgeos::gDistance(esc_coor[which(esc_data$upz == "")], upz_shp, byid = T), 2,which.min))])
}


write_csv(esc_data, 'data/processed_data/geodata/UPZ_Unidad_Catastral.csv')

