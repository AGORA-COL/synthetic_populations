##=======================================#
## Author: Guido España
## Process demographic data for Colombia
## Date: 2018/10/03
##=======================================#
## Libraries and data files--------
##=======================================#
library(dplyr)
library(tidyverse)
library(rjson)

library(sf)
library(raster)
library(rgdal)
library(maptools)
library(osmdata)
library(RColorBrewer)
library(osmplotr)
library(RCurl)
library(data.table)
options(digits = 22,scipen = 999)


data_dir = '../data/raw_data/microdata/11Bogota'

country_name = "colombia"
city_code = 11001
formatted_dir = file.path("..", "output", "formatted_populations",
                          sprintf("%s_%d", country_name, city_code))

##=======================================#
## Geo data-----------
##=======================================#
## Read nursing homes data
## https://www.minsalud.gov.co/proteccionsocial/lists/oferta%20institucional%20personas%20mayores/todas%20las%20ofertas.aspx?Paged=TRUE&p_ID=540&PageFirstRow=621&&View=%7BC84CB794-1D1E-4A3B-9973-FE97E04FBD27%7D#InplviewHashc84cb794-1d1e-4a3b-9973-fe97e04fbd27=Paged%3DTRUE-p_ID%3D700-PageFirstRow%3D781
esc_shp = rgdal::readOGR('../data/raw_data/geodata/scat_shp/scat_shp.shp')
localidad_shp = rgdal::readOGR('../data/raw_data/geodata/localidades_bogota/poligonos-localidades.shp')

localidad_data = localidad_shp@data %>%
    mutate(NameLocalidad = tolower(iconv(Nombre_de_l,from="UTF-8",to="ASCII//TRANSLIT")),
           LocalidadID = as.numeric(Identificad)) %>%
    dplyr::select(NameLocalidad, LocalidadID)

lea_df = read_csv('../data/raw_data/popdata/LEA_Bogota.csv')    

lea_coor = coordinates(lea_df %>% dplyr::select(Lat,Lon))
colnames(lea_coor) = c("LAT", "LON")
lea_coor = as.data.frame(lea_coor)
coordinates(lea_coor) = ~ LON + LAT
proj4string(lea_coor) = proj4string(localidad_shp)


lea_df$LocalidadID = -1
lea_df$LocalidadName = ""
lea_df$ESC = ""
## Find locality for each place
for(ll in 1:nrow(localidad_shp)){
    lea_localidad = sp::over(lea_coor, localidad_shp[ll,])
    lea_df$LocalidadID[which(!is.na(lea_localidad[,1]))] = as.numeric(as.character(localidad_shp@data$Identificad[ll]))
    lea_df$LocalidadName[which(!is.na(lea_localidad[,1]))] = as.character(localidad_shp@data$Nombre_de_l[ll])
}

proj4string(lea_coor) = proj4string(esc_shp)
proj4string(esc_shp) = proj4string(lea_coor) ## Something weird about the projections and comments

for(ee in 1:nrow(esc_shp)){
    lea_esc = sp::over(lea_coor, esc_shp[ee,])
    lea_df$ESC[which(!is.na(lea_esc[,1]))] = as.character(esc_shp@data$SCACODIGO[ee])
}

plot(localidad_shp[localidad_shp@data$Identificad != 20,])
points(lea_coor, pch = 15, col = "#00800050")
text(x = coordinates(localidad_shp), labels = localidad_shp@data$Nombre_de_l, cex = 0.5)

##=======================================#
## Read updated list of residents--------
##=======================================#
hogares_geriatricos = readxl::read_xlsx('../data/raw_data/popdata/BASE_IPIPM_15042021_AJUSTADO.xlsx') %>%
    rename('80_105' = "NÚMERO DE RESIDENTES CON 80 AÑOS O MÁS",
           '60_79' = "NÚMERO DE RESIDENTES DE 60 A 79 AÑOS",
           '40_59' = "NÚMERO DE RESIDENTES MENORES DE 60 AÑOS",
           'TOTAL' = "NUMERO TOTAL DE RESIDENTES DE LA IPIPM") %>%
    dplyr::select('40_59', '60_79', '80_105', 'LOCALIDAD', 'UPZ') %>%
    drop_na() %>%
    mutate(VIV_ID = sprintf('7511001%03d', row_number())) %>%
    mutate(LOCALIDAD = tolower(LOCALIDAD)) %>%
    left_join(localidad_data, by = c("LOCALIDAD" = "NameLocalidad"))

## For each house, sample residents based on their age distribution

sum_hh_df = hogares_geriatricos %>%
    gather(key = AgeGroup, value = Residents, -c('LOCALIDAD', 'UPZ','VIV_ID', 'LocalidadID') )

sum_hh_df = sum_hh_df[rep(row.names(sum_hh_df), sum_hh_df$Residents),] %>%
    group_by(VIV_ID) %>%
    mutate(sp_id = sprintf('%s%04d', VIV_ID, row_number()),
           sporder = row_number()) %>%
    ungroup() %>%
    separate(AgeGroup, into = c('MinAge', 'MaxAge'), remove = F)

sum_hh_df$Age = round(runif(rep(1,nrow(sum_hh_df)),
                      min = as.numeric(sum_hh_df$MinAge),
                      max = as.numeric(sum_hh_df$MaxAge)))

hogares_geriatricos$Lat = 0
hogares_geriatricos$Lon = 0
hogares_geriatricos$ESC = ""

## Then, assign each household an address based on the locality
for(ll in unique(hogares_geriatricos$LocalidadID)){
    indx_ll = which(hogares_geriatricos$LocalidadID == ll)
    tmp_lea = dplyr::filter(lea_df,LocalidadID == ll)
    tmp_indx = sample(1:nrow(tmp_lea), size = length(indx_ll), replace = T)
    hogares_geriatricos[indx_ll,'Lat'] = tmp_lea$Lat[tmp_indx]
    hogares_geriatricos[indx_ll,'Lon'] = tmp_lea$Lon[tmp_indx]
    hogares_geriatricos[indx_ll,'ESC'] = tmp_lea$ESC[tmp_indx]
}

hogares_geriatricos$Residents = rowSums(hogares_geriatricos[,c('40_59','60_79','80_105')])

##=======================================#
## Draw LEA places and assign-----------
##=======================================#
## sp_id,sp_gq_id,sporder,age,sex
## Sex in FRED: 1 M, 0 F
## Sex in Census: 1 M, 2 F; for now just uniformly sampling
people_out_df = sum_hh_df %>%
    mutate(sp_gq_id = VIV_ID,
           age = Age,
           sex = sample(0:1,size = nrow(sum_hh_df), replace = T)) %>%
    dplyr::select(sp_id, sp_gq_id, sporder, age, sex) %>%
    arrange(sp_gq_id, sporder)

## "sp_id","gq_type","persons","stcotrbg","latitude","longitude"
gq_houses = hogares_geriatricos %>%
    dplyr::mutate(longitude = Lon, latitude = Lat, sp_id = VIV_ID, gq_type = "N",
                  stcotrbg = sprintf("11001%s", ESC), persons = Residents) %>%
    dplyr::select(sp_id, gq_type, persons, stcotrbg, latitude, longitude)



plot(localidad_shp[localidad_shp@data$Identificad != 20,])
points(gq_houses$longitude, gq_houses$latitude, pch = 15, col = "#00800050")
text(x = coordinates(localidad_shp), labels = localidad_shp@data$Nombre_de_l, cex = 0.5)

## write.csv(
##     gq_houses,
##     file.path(formatted_dir,              
##               sprintf('%s_%d_synth_gq.txt',country_name,city_code)
##               ),
##     na = "", row.names = F, quote = F
## )


## write.csv(
##     people_out_df,
##     file.path(formatted_dir,              
##               sprintf('%s_%d_synth_gq_people.txt',country_name,city_code)
##               ),
##     na = "", row.names = F, quote = F
## )
