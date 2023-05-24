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

options(digits = 22, scipen = 999)
source('./synthetic_trajectories_functions.R')
##===============================================================#
## Read Input-------------
##===============================================================#
country_name = "colombia"
city_name = "Cali"
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
## 0. Process inputs--------
##===============================================================#
processed_data_dir = file.path("..","data", "processed_data")
outputdir_microdata = file.path('..','output','synthesized_microdata')
report_dir = file.path("..","output","reports")

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
## 1. Read synth population----------------
##===============================================================#
people_file = file.path(
    outputdir_microdata,
    sprintf('synthetic_microdata_people_%s_%d.csv',country_name,city_code)
)

synth_houses = file.path(
    formatted_dir,              
    sprintf('%s_%d_synth_households.txt',country_name,city_code)
)

synth_schools = file.path(
    formatted_dir,              
    sprintf('%s_%d_schools.txt',country_name,city_code)
)

synth_workplaces = file.path(
    formatted_dir,              
    sprintf('%s_%d_workplaces.txt',country_name,city_code)
)

schools_df = read_csv(synth_schools)
workplaces_df = read_csv(synth_workplaces)


##===============================================================#
## 2. Assign schools and workplaces----------------
##===============================================================#
people_out_df = assign_schools_workplaces(people_file, synth_houses,synth_schools, synth_workplaces)

write.csv(
    people_out_df,
    file.path(formatted_dir,              
              sprintf('%s_%d_synth_people.txt',country_name,city_code)
              ),
    na = "", row.names = F, quote = F
)
