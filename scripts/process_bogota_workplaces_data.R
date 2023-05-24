##=======================================#
## Author: Guido España
## Process demographic data for Colombia
## Date: 2018/10/03
##=======================================#
## Libraries and data files--------
##=======================================#
library(dplyr)
library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(maptools)
library(osmdata)
library(RColorBrewer)
library(osmplotr)
library(RCurl)
library(data.table)
options(digits = 20,scipen = 999)

datadir = '../data/raw_data/popdata'
workplaces_file = '../data/raw_data/workplacedata/08092020 Unidad Economica ID.xlsx'
mov_file = '../data/raw_data/workplacedata/03_Anexo D_Movilidad.xlsx'

workers_data =  readxl::read_xlsx(workplaces_file, skip = 0, sheet = 1)
## 1. Find a location for workplaces missing location
workers_nodata = workers_data[is.na(workers_data$Latitud),]
workers_data = workers_data[!is.na(workers_data$Latitud),]

localidad_shp = rgdal::readOGR('../data/raw_data/geodata/localidades_bogota/poligonos-localidades.shp')
block_shp = rgdal::readOGR('../data/processed_data/geodata/manzanas_bogota/manzanas_bogota.shp')
block_esc = read_csv('../data/processed_data/geodata/Manzana_Unidad_Catastral.csv')
esc_localidad = read_csv('../data/processed_data/geodata/Localidad_Unidad_Catastral.csv') 
block_localidad = left_join(esc_localidad, block_esc, by = "SCACODIGO")    

block_tmp = filter(block_localidad, Localidad != 20)
zone_blocks = block_shp[which(as.numeric(as.character(block_shp@data$COD_DANE)) %in% block_tmp$COD_DANE),]    
workplaces_points = sp::spsample(zone_blocks, n = nrow(workers_nodata), "random",iter = 50)
    
workers_nodata$Latitud = coordinates(workplaces_points)[,'y']
workers_nodata$Longitud = coordinates(workplaces_points)[,'x']

workers_df = bind_rows(workers_data, workers_nodata) %>%
    rename(latitude = Latitud, longitude = Longitud,
           TotalWorkers = "Número total de empleados")
workers_df$workplace_id = sprintf("11001%06d",1:nrow(workers_df))

workers_df = workers_df %>%
    dplyr::select(workplace_id, TotalWorkers,latitude, longitude)

##=======================================#
## Mobility data--------
##=======================================#
workers_df$Localidad = 0
workers_coor = coordinates(workers_df[,c('latitude','longitude')])
colnames(workers_coor) = c("latitude", "longitude")
workers_coor = as.data.frame(workers_coor)
coordinates(workers_coor) = ~   longitude + latitude
proj4string(workers_coor) = proj4string(localidad_shp)

for(ll in 1:nrow(localidad_shp)){
    workers_locality = sp::over(workers_coor, localidad_shp[ll,])
    workers_df$Localidad[which(!is.na(workers_locality[,1]))] = as.numeric(localidad_shp@data$Identificad[ll])
}

if(length(which(workers_df$Localidad == 0)) > 0){
    workers_df$Localidad[workers_df$Localidad == 0] =       as.numeric(localidad_shp@data$Identificad[as.numeric(apply(rgeos::gDistance(workers_coor[which(workers_df$Localidad == 0)],localidad_shp, byid = T), 2,which.min))])
}

localidad_data = localidad_shp@data %>%
    mutate(NameLocalidad = tolower(iconv(Nombre_de_l,from="UTF-8",to="ASCII//TRANSLIT")),
           Localidad = as.numeric(Identificad)) %>%
    dplyr::select(NameLocalidad, Localidad)

## Read mobility matrix
mov_df =  readxl::read_xlsx(mov_file, skip = 9, sheet = 'IND_191', n_max = 428)   %>%
    rename(Trips = "Cantidad de viajes autocontenidos*") %>%
    filter(!(Origen == "Upr" | Destino == "Upr")) %>%
    mutate(Origen = tolower(Origen),
           Destino = tolower(Destino)) %>%
    mutate(Origen = iconv(Origen,from="UTF-8",to="ASCII//TRANSLIT")) %>%
    mutate(Destino = iconv(Destino,from="UTF-8",to="ASCII//TRANSLIT")) %>%    
    left_join(localidad_data %>% rename(OrigenLocalidad = Localidad), by = c("Origen" = "NameLocalidad")) %>%
    left_join(localidad_data %>% rename(DestinoLocalidad = Localidad), by = c("Destino" = "NameLocalidad"))  %>%
    arrange(OrigenLocalidad, DestinoLocalidad) %>%
    drop_na() %>%
    right_join(expand.grid(OrigenLocalidad = 1:20,DestinoLocalidad = 1:20), by = c("OrigenLocalidad","DestinoLocalidad")) %>%
    replace_na(list(Trips = 0)) %>%
    group_by(OrigenLocalidad) %>% mutate(Trips = Trips / sum(Trips))
    

write_csv(workers_df, '../data/processed_data/workplacedata/workplace_bogota_data.csv')
write_csv(mov_df, '../data/processed_data/workplacedata/mobility_matrix_bogota_data.csv')
    
           
