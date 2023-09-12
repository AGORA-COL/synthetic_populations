##===============================================================#
## Generate synthetic transportation
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
print(sprintf("Running synthetic transportation for %s:%s from %s",country_name,city_name, metadata_file))
                            

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

synth_houses_file = sprintf(
    '../output/formatted_populations/%s_%d/%s_%d_synth_households.txt',
    country_name, city_code, country_name, city_code)

transport_stations_file = sprintf('../data/processed_data/transportdata/%s_publictransport_%d.csv',
                             country_name, city_code)

##===============================================================#
## Read datasets-------------
##===============================================================#
houses_df = read_csv(synth_houses_file) %>%
    mutate(station_id = -1)
stations_df = read_csv(transport_stations_file)

## For each house, find the closest station
for(hh in 1:nrow(houses_df)){
    house_tmp = houses_df[hh,]
    stations_dist = sqrt((house_tmp$latitude - stations_df$station_latitude)^2 + (house_tmp$longitude - stations_df$station_longitude)^2)

    houses_df$station_id[hh] = stations_df$station_ID_number[which.min(stations_dist)]
    
    ## plot(stations_df$station_longitude, stations_df$station_latitude, col = "gray")
    ## points(house_tmp$longitude, house_tmp$latitude, col = "navy", pch = 9)
    ## points(stations_df$station_longitude[which.min(stations_dist)], stations_df$station_latitude[which.min(stations_dist)], col = "red")
    ## Sys.sleep(0.5)
    if(hh %% 100 == 0){
        cat("\r",hh)
    }
}



house_transport_file = sprintf('../data/processed_data/transportdata/%s_%d_house_transport.csv',
                             country_name, city_code)

write_csv(dplyr::select(houses_df, sp_id, station_id, latitude, longitude), house_transport_file)


