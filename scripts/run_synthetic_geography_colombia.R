setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
Sys.setenv(PROJ_LIB = "/mnt/disco_aux/trace/apps/mambaforge/share/proj")
##===============================================================#
## Generate synthetic populations from start to finish
## Author: Guido España - Date: 2019/07/17
## Mod: Diego Veloza - Date: 2023/09/20
##===============================================================#
## Read Input-------------
##===============================================================#
library(dplyr)
library(ggplot2)
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
country_name        = "colombia"
departament_name    = "SANTANDER"
metadata_file       = file.path("..", "data", "param_files", "colombia_municipios_metadata.json")
country_shp_file    = "../data/raw_data/geodata/Colombia_shp/Municipios.shp"

args = (commandArgs(TRUE))
if(length(args) == 3){
    country_name = args[1]
    departament_name = args[2]
    metadata_file = args[3]
}
print(sprintf("Running synthetic population generator for %s:%s from %s",country_name,departament_name, metadata_file))

##===============================================================#
## Set up-------------
##===============================================================#
## Localidades: https://bogota-laburbano.opendatasoft.com/explore/dataset/poligonos-localidades/export/
## Barrios: https://bogota-laburbano.opendatasoft.com/explore/?sort=modified&refine.keyword=Bogot%C3%A1
## Poblacion barrios: https://www.datos.gov.co/Funci-n-p-blica/Listado-de-barrios-y-n-mero-de-habitantes/fcsx-656w
## Colegios: https://www.datos.gov.co/Educaci-n/Directorio-nico-de-establecimiento-Educativo-Bogot/5fy4-qx2f/data

source('./synthetic_geographic_locations_functions_bogota.R')

processed_data_dir  = file.path("..","data", "processed_data")
outputdir_microdata = file.path('..','output','synthesized_microdata')
raw_geodata_path    = file.path('..','data', 'raw_data','geodata')
report_dir          = file.path("..","output","reports")
osm_retag_file      = file.path('..','data', 'processed_data','geodata','retag_OSM_buildings.csv')

##===============================================================#
## 0. Process inputs--------
##===============================================================#
countries_datalist = rjson::fromJSON(
                                file = metadata_file,
                                simplify = F)

country_data = countries_datalist[[country_name]]

# Extract data for the given departamento
departamento_data <- country_data[[departament_name]]

# Get municipio names and divipola codes
municipios <- names(departamento_data)
divipola_codes <- sapply(departamento_data, function(municipio_data) {
    municipio_data$divipola_code[[1]]
})
municipio_data <- data.frame(municipio = municipios, divipola_code = divipola_codes)

# update departmento info
departamento_data$country_name_gdam <- departamento_data[[1]]$country_name_gdam[[1]]
departamento_data$country_code <- departamento_data[[1]]$country_code[[1]]
departamento_data$departamento_code <- departamento_data[[1]]$department_code[[1]]
departamento_data$year_pop <- departamento_data[[1]]$year_pop[[1]]

# city_code = unlist(countries_datalist[[country_name]][[city_name]]$city_code)
# country_code = unlist(countries_datalist[[country_name]][[city_name]]$country_code)
# city_levels = unlist(city_data$city_levels)

formatted_dir = file.path("..", "output", "formatted_populations",
                          sprintf("%s_%d", country_name, departamento_data$departamento_code))


if(!dir.exists(formatted_dir)){dir.create(formatted_dir)}
if(!dir.exists(outputdir_microdata)){dir.create(outputdir_microdata)}
##===============================================================#
## 1. Calculate schools capacities and students--------
## TODO!!: What to do when there is no info in students?
##===============================================================#
people_file = file.path(
    outputdir_microdata,
    sprintf('synthetic_microdata_people_%s_%d.csv', country_name, departamento_data$departamento_code)
)

##===============================================================#
## 1.Get geo data for localities / nb--------
##===============================================================#
vulnr_shp               <- rgdal::readOGR('../data/raw_data/geodata/VULNRB/VULNRB_IPMxMZ.shp')
municipio_UE_gdb_file   <- "../data/raw_data/geodata/Manzanas_CNUE_Marco_Pos_DEST.gdb"
municipio_shp_file      <- "../data/raw_data/geodata/Colombia_shp/Municipios.shp"
# manzana_data <- "../data/raw_data/geodata/SHP_MGN2018_INTGRD_MANZ/MGN_ANM_MANZANA.shp"

# Read the feature class
municipio_UE_gdb        <- readOGR(dsn=municipio_UE_gdb_file, layer="Manzanas_CNUE_Marco_DEST_290722")
municipio_UE_data_gdb   <- municipio_UE_gdb@data %>% mutate(ROW = row_number())

municipio_shp       <- rgdal::readOGR(municipio_shp_file)
municipio_data_shp  <- municipio_shp@data %>% mutate(ROW = row_number())



vulnr2 = vulnr_shp[vulnr_shp@data$COD_DPTO == departamento_data$country_code,]

# manzana_shp = rgdal::readOGR(manzana_data)
# manzana_data_shp = manzana_shp@data %>% mutate(ROW = row_number())

# ## We're looking for municipalities    
# manzana_indx = manzana_data_shp %>%
#     filter(MPIO_CDPMP == municipio_code) %>%
#     pull(ROW)

# manzana_shp_ <- spTransform(manzana_shp[manzana_indx,], 
#                                  CRS("+proj=longlat +datum=WGS84"))


# ggplot() +
#     geom_sf(data = st_as_sf(municipio_UE_shp_), aes(fill = "#51ff00"), color = "#ff0000", alpha = 0.9) + 
#     geom_sf(data = st_as_sf(municipio_shp_), aes(fill = "#0000ff"), color = "#0000ff", alpha = 0.3) + 
#     geom_sf(data = st_as_sf(vulnr2), aes(fill = "#0000ff"), color = "#0000ff", alpha = 0.2) + 
#   theme_minimal()


# depart_shp = rgdal::readOGR('../data/raw_data/geodata/Poligonos espaciales/MGN_DPTO_POLITICO.shp')
# depart2 = depart_shp[depart_shp@data$DPTO_CCDGO == "68",]

# barrios_shp     = rgdal::readOGR('../data/raw_data_/geodata/barrios_bogota/barrios_prueba.shp')
# localities_shp  = rgdal::readOGR('../data/raw_data_/geodata/localidades_bogota/poligonos-localidades.shp')
# esc_shp         = rgdal::readOGR('../data/raw_data_/geodata/scat_shp/scat_shp.shp')
# esc_data        = esc_shp@data[,c('SCACODIGO', 'SCANOMBRE')]
# esc_data$Localidad = 0

# esc_coor = coordinates(esc_shp)
# colnames(esc_coor) = c("LAT", "LON")
# esc_coor = as.data.frame(esc_coor)
# coordinates(esc_coor) = ~ LAT + LON
# proj4string(esc_coor) = proj4string(localities_shp)

# for(ll in 1:nrow(localities_shp)){
#     print(ll)
#     esc_locality = sp::over(esc_coor, localities_shp[ll,])
#     esc_data$Localidad[which(!is.na(esc_locality[,1]))] = localities_shp@data$Identificad[ll]
# }

# esc_data$Localidad[esc_data$SCACODIGO == '108108'] = 1
# esc_shp@data$Localidad = esc_data$Localidad
# esc_list = unique(as.character(esc_data$SCACODIGO))
# esc_shp@data$ZONE = as.character(esc_shp@data$SCACODIGO)
# write_csv(esc_data, '../data/processed_data/geodata/Localidad_Unidad_Catastral.csv')

# block_shp = rgdal::readOGR('../data/processed_data/geodata/manzanas_bogota/manzanas_bogota.shp')
# block_coor = coordinates(block_shp)
# colnames(block_coor) = c("LAT", "LON")
# block_coor = as.data.frame(block_coor)
# coordinates(block_coor) = ~ LAT + LON 
# proj4string(block_coor) = proj4string(esc_shp)

# block_data = block_shp@data
# block_data$SCACODIGO = "0"
# for(ee in 1:nrow(esc_shp)){
#     block_esc = sp::over(block_coor, esc_shp[ee,])
#     block_data$SCACODIGO[which(!is.na(block_esc[,1]))] = as.character(esc_shp@data$SCACODIGO[ee])
# }

# block_shp@data$SCACODIGO = block_data$SCACODIGO

# write_csv(block_data, '../data/processed_data/geodata/Manzana_Unidad_Catastral.csv')

##===============================================================#
## Schools pop-----------
##===============================================================#
## Read universities and assign capacity
## https://www.mineducacion.gov.co/sistemasinfo/Informacion-a-la-mano/212400:Estadisticas
# students_df = read_csv(people_file) %>%
#     filter(SCHOOLSTATUS != "ns") %>%
#     mutate(GRADE = ifelse(AGE < 5, "prek",
#                    ifelse(AGE == 5, "kinder",
#                    ifelse(AGE <= 18, "gr01_gr12","ungraded")))) %>%
#     group_by(GRADE) %>% summarize(ENROLLED = n()) %>% ungroup() %>%
#     spread(key = GRADE, value = ENROLLED)     
# students_df$total = rowSums(students_df)

# universities_file = "../data/raw_data/schooldata/IES_Estudiantes_matriculados_2019.xlsx"
# universities_df = readxl::read_xlsx(universities_file, skip = 7) %>%
#     filter(SEMESTRE == 1) %>%
#     rename(mun_code = "CÓDIGO DEL MUNICIPIO (PROGRAMA)",
#             name = "INSTITUCIÓN DE EDUCACIÓN SUPERIOR (IES)",
#             capacity = "MATRICULADOS") %>%
#     dplyr::select(mun_code, name, capacity) %>%
#     mutate(mun_code = sprintf("%05d",mun_code),
#            name = gsub(",","",name)) %>%
#     filter(mun_code == sprintf("%05d",as.numeric(city_code)))%>%
#     group_by(mun_code, name) %>%
#     summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
#     filter(capacity > 10)

# universities_df$total = sum(universities_df$capacity)

# universities_df$ungraded = floor((universities_df$capacity / universities_df$total) * students_df$ungraded)
# if(students_df$ungraded > sum(universities_df$ungraded)){
#     universities_df$ungraded[1] = universities_df$ungraded[1] + (students_df$ungraded - sum(universities_df$ungraded))
# }
# universities_df = universities_df %>%
#     dplyr::mutate(school_id =  sprintf('%03d%06d%07d', country_code,city_code,row_number()),
#                   mun_code = as.character(mun_code),
#                   prek = 0, gr01_gr12 = 0, kinder = 0) %>%
#     dplyr::select(mun_code, school_id, name, prek, kinder, gr01_gr12,ungraded)

# schools_combined = universities_df

## TODO:
## - [x] Since schools already have LAT, LON, locate them in the trajectories script, not here
## - [x] Given that workplaces have LAT, LON, locate them in the trajectories script, not here
##===============================================================#
## 3. Use the Geo-Locations generator--------
##===============================================================#
house_file = file.path(
    outputdir_microdata,
    sprintf('synthetic_microdata_%s_%d.csv', country_name, departamento_data$departamento_code)
)

synth_locations         = synthesize_locations_zones_mun(
    house_file          = house_file,
    country_name        = country_name,
    country_name_gdam   = departamento_data$country_name_gdam,
    country_code        = departamento_data$country_code,
    departamento_name   = departament_name,
    departamento_levels = divipola_codes,
    country_shp_file    = country_shp_file,
    departamento_code   = departamento_data$departamento_code,
    raw_data_path       = raw_geodata_path)


municipio_indx = municipio_data_shp %>%
    filter(as.numeric(COD_DEPTO) == departamento_data$departamento_code) %>%
    pull(ROW)

departamento_shp <- spTransform(municipio_shp[municipio_indx,], CRS("+proj=longlat +datum=WGS84"))


## We're looking for municipalities    
municipio_UE_indx = municipio_UE_data_gdb %>%
    filter(as.numeric(COD_DPTO) == departamento_data$departamento_code) %>%
    pull(ROW)

departamento_UE_shp <- spTransform(municipio_UE_gdb[municipio_UE_indx, ], CRS("+proj=longlat +datum=WGS84"))

ggplot() +
    geom_point(data = synth_locations$city_houses, aes(x = longitude, y = latitude), color = "#1900ff", size = 1, alpha = 0.7) +
    geom_sf(data = st_as_sf(departamento_UE_shp), aes(fill = "#51ff00"), color = "#ff0000", alpha = 0.9) + 
    geom_sf(data = st_as_sf(departamento_shp), aes(fill = "#0000ff"), color = "#0000ff", alpha = 0.3) + 
  theme_minimal()


write.csv(
    synth_locations$city_houses,
    file.path(formatted_dir,              
              sprintf('%s_%d_synth_households.txt',country_name, departamento_data$departamento_code)
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


