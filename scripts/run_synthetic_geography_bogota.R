##===============================================================#
## Generate synthetic populations from start to finish
## Author: Guido España
## Date: 2019/07/17
##===============================================================#
## Read Input-------------
##===============================================================#
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
options(digits = 20,scipen = 999)

setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
##===============================================================#
## Read Input-------------
##===============================================================#
country_name = "colombia"
city_name = "Bogota"
metadata_file = file.path("..", "data", "param_files", "countries_latam_metadata.json")
country_shp_file = "../data/raw_data/geodata/Colombia_shp/Municipios.shp"

args = (commandArgs(TRUE))
if(length(args) == 3){
    country_name = args[1]
    city_name = args[2]
    metadata_file = args[3]
}
print(sprintf("Running synthetic population generator for %s:%s from %s",country_name,city_name, metadata_file))

##===============================================================#
## Set up-------------
##===============================================================#
## Localidades: https://bogota-laburbano.opendatasoft.com/explore/dataset/poligonos-localidades/export/
## Barrios: https://bogota-laburbano.opendatasoft.com/explore/?sort=modified&refine.keyword=Bogot%C3%A1
## Poblacion barrios: https://www.datos.gov.co/Funci-n-p-blica/Listado-de-barrios-y-n-mero-de-habitantes/fcsx-656w
## Colegios: https://www.datos.gov.co/Educaci-n/Directorio-nico-de-establecimiento-Educativo-Bogot/5fy4-qx2f/data

source('./synthetic_geographic_locations_functions_bogota.R')

processed_data_dir = file.path("..","data", "processed_data")
outputdir_microdata = file.path('..','output','synthesized_microdata')
raw_geodata_path = file.path('..','data','raw_data','geodata')
report_dir = file.path("..","output","reports")
osm_retag_file = file.path('..','data','processed_data','geodata','retag_OSM_buildings.csv')

##===============================================================#
## 0. Process inputs--------
##===============================================================#
countries_datalist = rjson::fromJSON(
                                file = metadata_file,
                                simplify = F)

country_data = countries_datalist[[country_name]]
city_data = country_data[[city_name]]
city_code = unlist(countries_datalist[[country_name]][[city_name]]$city_code)
country_code = unlist(countries_datalist[[country_name]][[city_name]]$country_code)

city_levels = unlist(city_data$city_levels)
country_name_gdam = unlist(city_data$country_name_gdam)
raster_file = unlist(city_data$raster_file)

formatted_dir = file.path("..", "output", "formatted_populations",
                          sprintf("%s_%d", country_name, city_code))


if(!dir.exists(formatted_dir)){dir.create(formatted_dir)}
if(!dir.exists(outputdir_microdata)){dir.create(outputdir_microdata)}

##===============================================================#
## 1. Calculate schools capacities and students--------
## TODO!!: What to do when there is no info in students?
##===============================================================#
people_file = file.path(
    outputdir_microdata,
    sprintf('synthetic_microdata_people_%s_%d.csv',country_name,city_code)
)


##===============================================================#
## 1.Get geo data for localities / nb--------
##===============================================================#
## vulnr_shp = rgdal::readOGR('../data/raw_data/geodata/vulnrb_data/VULNRB_IPMxMZ.shp')
## vulnr2 = vulnr_shp[vulnr_shp@data$COD_DPTO == 11,]
## zones_names = tolower(iconv(localities_shp@data$Nombre_de_l,from="UTF-8",to="ASCII//TRANSLIT"))
## zones_id = as.numeric(localities_shp@data$Identificad)
## agegroup_df = read_csv('../data/processed_data/popdata/bogota_population_data.csv')
## zones_list = unique(agegroup_df$Zone)

## zones_id_df = data.frame(Localidad = zones_names, Localidad_ID = zones_id, stringsAsFactors = F)
## write_csv(zones_id_df, file.path(formatted_dir,'zones_list.csv'))


barrios_shp = rgdal::readOGR('../data/raw_data/geodata/barrios_bogota/barrios_prueba.shp')
localities_shp = rgdal::readOGR('../data/raw_data/geodata/localidades_bogota/poligonos-localidades.shp')
esc_shp = rgdal::readOGR('../data/raw_data/geodata/scat_shp/scat_shp.shp')
esc_data = esc_shp@data[,c('SCACODIGO', 'SCANOMBRE')]
esc_data$Localidad = 0

esc_coor = coordinates(esc_shp)
colnames(esc_coor) = c("LAT", "LON")
esc_coor = as.data.frame(esc_coor)
coordinates(esc_coor) = ~ LAT + LON
proj4string(esc_coor) = proj4string(localities_shp)

for(ll in 1:nrow(localities_shp)){
    print(ll)
    esc_locality = sp::over(esc_coor, localities_shp[ll,])
    esc_data$Localidad[which(!is.na(esc_locality[,1]))] = localities_shp@data$Identificad[ll]
}

esc_data$Localidad[esc_data$SCACODIGO == '108108'] = 1
esc_shp@data$Localidad = esc_data$Localidad
esc_list = unique(as.character(esc_data$SCACODIGO))
esc_shp@data$ZONE = as.character(esc_shp@data$SCACODIGO)
write_csv(esc_data, '../data/processed_data/geodata/Localidad_Unidad_Catastral.csv')

block_shp = rgdal::readOGR('../data/processed_data/geodata/manzanas_bogota/manzanas_bogota.shp')
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

write_csv(block_data, '../data/processed_data/geodata/Manzana_Unidad_Catastral.csv')

##===============================================================#
## Schools pop-----------
##===============================================================#
## Read universities and assign capacity
## https://www.mineducacion.gov.co/sistemasinfo/Informacion-a-la-mano/212400:Estadisticas
students_df = read_csv(people_file) %>%
    filter(SCHOOLSTATUS != "ns") %>%
    mutate(GRADE = ifelse(AGE < 5, "prek",
                   ifelse(AGE == 5, "kinder",
                   ifelse(AGE <= 18, "gr01_gr12","ungraded")))) %>%
    group_by(GRADE) %>% summarize(ENROLLED = n()) %>% ungroup() %>%
    spread(key = GRADE, value = ENROLLED)     
students_df$total = rowSums(students_df)

universities_file = "../data/raw_data/schooldata/colombia_matriculados_educacion_superior_2018.xlsx"
universities_df = readxl::read_xlsx(universities_file, skip = 5) %>%
    filter(Semestre == 1) %>%
    rename(mun_code = "Código del \r\nMunicipio\r\n(Programa)",
           name = "Institución de Educación Superior (IES)",
           capacity = "Matriculados 2018") %>%
    dplyr::select(mun_code, name, capacity) %>%
    mutate(mun_code = sprintf("%05d",mun_code),
           name = gsub(",","",name)) %>%
    filter(mun_code == sprintf("%05d",as.numeric(city_code)))%>%
    group_by(mun_code, name) %>%
    summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
    filter(capacity > 10)

universities_df$total = sum(universities_df$capacity)

universities_df$ungraded = floor((universities_df$capacity / universities_df$total) * students_df$ungraded)
if(students_df$ungraded > sum(universities_df$ungraded)){
    universities_df$ungraded[1] = universities_df$ungraded[1] + (students_df$ungraded - sum(universities_df$ungraded))
}
universities_df = universities_df %>%
    dplyr::mutate(school_id =  sprintf('%03d%06d%07d', country_code,city_code,row_number()),
                  mun_code = as.character(mun_code),
                  prek = 0, gr01_gr12 = 0, kinder = 0) %>%
    dplyr::select(mun_code, school_id, name, prek, kinder, gr01_gr12,ungraded)

schools_combined = universities_df

## TODO:
## - [x] Since schools already have LAT, LON, locate them in the trajectories script, not here
## - [x] Given that workplaces have LAT, LON, locate them in the trajectories script, not here
##===============================================================#
## 3. Use the Geo-Locations generator--------
##===============================================================#
house_file = file.path(
    outputdir_microdata,
    sprintf('synthetic_microdata_%s_%d.csv',country_name,city_code)
)

synth_locations = synthesize_locations_zones(
    house_file = house_file,
    country_name_gdam = country_name_gdam,
    country_code = country_code,
    city_name = city_name,
    city_levels = city_levels,
    country_shp_file = country_shp_file,
    city_code = city_code,
    raw_data_path = raw_geodata_path,
    zones_shp = esc_shp,
    block_shp = block_shp)


write.csv(
    synth_locations$city_houses,
    file.path(formatted_dir,              
              sprintf('%s_%d_synth_households.txt',country_name,city_code)
              ),
    na = "",row.names = F,quote = F
)

## write.csv(
##     synth_locations$city_schools,
##     file.path(formatted_dir,              
##               sprintf('%s_%d_schools.txt',country_name,city_code)
##               ),
##     na = "", row.names = F,quote =F
## )

## write.csv(
##     synth_locations$city_workplaces,
##     file.path(formatted_dir,              
##               sprintf('%s_%d_workplaces.txt',country_name,city_code)
##               ),
##     na = "", row.names = F, quote = F
## )


