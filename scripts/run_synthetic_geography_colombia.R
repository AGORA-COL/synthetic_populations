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

##===============================================================#
## Read Input-------------
##===============================================================#
country_name        = "colombia"
departament_name    = "ANTIOQUIA"
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
source('./synthetic_geographic_locations_functions_colombia.R')

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
municipio_UE_gdb_file   <- "../data/raw_data/geodata/Manzanas_CNUE_Marco_Pos_DEST.gdb"
municipio_shp_file      <- "../data/raw_data/geodata/Colombia_shp/Municipios.shp"
# vulnr_shp               <- rgdal::readOGR('../data/raw_data/geodata/VULNRB/VULNRB_IPMxMZ.shp')
# manzana_data <- "../data/raw_data/geodata/SHP_MGN2018_INTGRD_MANZ/MGN_ANM_MANZANA.shp"

# Read the feature class
municipio_UE_gdb        <- readOGR(dsn=municipio_UE_gdb_file, layer="Manzanas_CNUE_Marco_DEST_290722")
municipio_UE_data_gdb   <- municipio_UE_gdb@data %>% mutate(ROW = row_number())

municipio_shp       <- rgdal::readOGR(municipio_shp_file)
municipio_data_shp  <- municipio_shp@data %>% mutate(ROW = row_number())

# manzana_shp = rgdal::readOGR(manzana_data)
# manzana_data_shp = manzana_shp@data %>% mutate(ROW = row_number())

# vulnr2 = vulnr_shp[vulnr_shp@data$COD_DPTO == departamento_data$country_code,]

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

write.csv(
    synth_locations$city_houses,
    file.path(formatted_dir,              
              sprintf('%s_%d_synth_households.txt',country_name, departamento_data$departamento_code)
              ),
    na = "",row.names = F,quote = F
)


ggplot() +
    geom_point(data = synth_locations$city_houses, aes(x = longitude, y = latitude), color = "#0B113A", size = 1, alpha = 0.7) +
    # geom_sf(data = st_as_sf(departamento_UE_shp), aes(fill = "#51ff00"), alpha = 0.4) + 
    geom_sf(data = st_as_sf(departamento_shp), aes(fill = "#0000ff"), color = "#000000", alpha = 0.3) + 
  theme_minimal()


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