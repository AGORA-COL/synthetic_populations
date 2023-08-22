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
## process L.E.A-----------
##=======================================#
## http://microdatos.dane.gov.co/index.php/catalog/643/datafile/F11
## LEA_Tipo de institución o establecimiento 1 Centro penitenciario 2 Institucion de proteccion e internado preventivo para niños, niñas y adolescentes 3 Centro de proteccion y atencion al adulto mayor 4 Convento, seminario, monasterio u otras instituciones similiares 5 Sede educativa con Poblacion interna 6 Cuartel, guarnicion militar (Ejercito, Armada y Fuerza AErea) 7 Comando de policIa, estacion de policIa 8 Campamento de trabajo 9 Casa de lenocinio o prostIbulo 10 Albergue de desplazados 11 Hogar de paz 12 Centrode rehabilitacion funcional 13 Casa de paso indigena No Aplica
## L_TIPO_INST == "3"
geo_df = read_csv('../data/raw_data/microdata/11Bogota/CNPV2018_MGN_A2_11.CSV',
                  col_types = cols(.default = "c")) %>%
    mutate(VIVCODIGO = sprintf("%09d%03d", as.numeric(COD_ENCUESTAS), as.numeric(U_VIVIENDA)))


viv_df = read_csv('../data/raw_data/microdata/11Bogota/CNPV2018_1VIV_A2_11.CSV', col_types = cols(.default = "c")) %>%
    mutate(VIVCODIGO = sprintf("%09d%03d", as.numeric(COD_ENCUESTAS), as.numeric(U_VIVIENDA)))%>%
    filter(UVA_USO_UNIDAD == "4", L_TIPO_INST %in% c("3")) %>%
    left_join(geo_df %>% dplyr::select(UA1_LOCALIDAD, U_MZA, VIVCODIGO), by = "VIVCODIGO")

house_df = read_csv('../data/raw_data/microdata/11Bogota/CNPV2018_2HOG_A2_11.CSV',
                    col_types = cols(.default = "c")) %>%
    mutate(VIVCODIGO = sprintf("%09d%03d", as.numeric(COD_ENCUESTAS), as.numeric(U_VIVIENDA))) %>%
    mutate(HOGCODIGO = sprintf("%09d%03d%02d", as.numeric(COD_ENCUESTAS), as.numeric(U_VIVIENDA), as.numeric(H_NROHOG))) %>%
    filter(VIVCODIGO %in% unique(viv_df$VIVCODIGO))


age_brks = seq(from = 0, by = 5, to = 105)
age_data = read_csv('../data/processed_data/popdata/bogota_population_data_sec.csv')
pop_data = age_data %>% group_by(AgeGroup, Gender) %>%
    summarize(Pop = sum(Pop)) %>%
    ungroup() %>%
    spread(key = Gender, value = Pop) %>%
    separate(AgeGroup, into = c('MinAge','MaxAge'), remove = F) %>%
    mutate(MinAge = as.numeric(MinAge)) %>%
    arrange(MinAge)

## Read synth population
brks = c(sort(pop_data$MinAge), 200)
lbls = sprintf("%d-%d",brks[1:(length(brks) - 1)], brks[2:length(brks)] - 1)
lbls[length(lbls)] = sprintf("%d-above", brks[length(brks) -1])

people_df = read_csv('../data/raw_data/microdata/11Bogota/CNPV2018_5PER_A2_11.CSV',
                     col_types = cols(.default = "c")) %>%
    mutate(VIVCODIGO = sprintf("%09d%03d", as.numeric(COD_ENCUESTAS), as.numeric(U_VIVIENDA))) %>%
    mutate(HOGCODIGO = sprintf("%09d%03d%02d", as.numeric(COD_ENCUESTAS), as.numeric(U_VIVIENDA), as.numeric(P_NROHOG))) %>%
    filter(VIVCODIGO %in% unique(viv_df$VIVCODIGO)) %>%
    left_join(viv_df %>% dplyr::select(VIVCODIGO, L_TIPO_INST, UA1_LOCALIDAD, U_MZA), by = "VIVCODIGO")

people_df$AGE = runif(rep(1,nrow(people_df)),
      min = age_brks[as.numeric(people_df$P_EDADR)],
      max = age_brks[as.numeric(people_df$P_EDADR) + 1])


people_df = read_csv('../data/raw_data/microdata/11Bogota/CNPV2018_5PER_A2_11.CSV',
                     col_types = cols(.default = "c"))

workers_df = people_df %>%
    dplyr::select(P_EDADR, P_TRABAJO) %>%
    mutate(AGE = round(runif(rep(1,nrow(people_df)),
      min = age_brks[as.numeric(P_EDADR)],
      max = age_brks[as.numeric(P_EDADR) + 1]))) %>%
    mutate(AgeGroup = cut(as.numeric(AGE), breaks =brks, labels = lbls, include.lowest = T,right = F)) %>%
    mutate(WORKER = ifelse(as.numeric(P_TRABAJO) %in% c(1,2,3), 1, 0))

a = workers_df %>% group_by(AgeGroup) %>% summarize(WORKERS = sum(WORKER), Pop = n()) %>% ungroup() %>% mutate(PropWorkers = 100 * WORKERS / Pop)

##=======================================#
## Draw LEA places and assign-----------
##=======================================#
## sp_id,sp_gq_id,sporder,age,sex
## Sex in FRED: 1 M, 0 F
## Sex in Census: 1 M, 2 F
people_out_df = people_df %>%
    mutate(sp_id = sprintf("%s%04d",VIVCODIGO, as.numeric(P_NRO_PER)),
           sp_gq_id = VIVCODIGO,
           sporder = as.numeric(P_NRO_PER),
           age = AGE,
           sex = ifelse(P_SEXO == "1", 1, 0)) %>%
    dplyr::select(sp_id, sp_gq_id, sporder, age, sex) %>%
    arrange(sp_gq_id, sporder)


## Sample locality based on number of places and population
lea_trim  = lea_df %>% dplyr::select(Lat,Lon,LocalidadID, LocalidadName, ESC)
localidad_lea =  lea_trim %>%
    group_by(LocalidadID) %>% summarize(N = n()) %>%ungroup() %>% arrange(N)

viv_df$LEA_INDX = sample(1:nrow(lea_trim), nrow(viv_df), replace = F)
viv_df$Localidad = lea_trim$LocalidadName[viv_df$LEA_INDX]
viv_df$ESC = lea_trim$ESC[viv_df$LEA_INDX]
viv_df$Lat = lea_trim$Lat[viv_df$LEA_INDX]
viv_df$Lon = lea_trim$Lon[viv_df$LEA_INDX]


## "sp_id","gq_type","persons","stcotrbg","latitude","longitude"
gq_houses = viv_df %>%
    dplyr::mutate(longitude = Lon, latitude = Lat, sp_id = VIVCODIGO, gq_type = "N",
                  stcotrbg = sprintf("11001%s", ESC), persons = L_TOT_PERL) %>%
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
