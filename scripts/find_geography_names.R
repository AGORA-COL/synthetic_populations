##===============================================================#
## Find the city name in shape files
## Author: Guido España
## Date: 2019/09/26
## Please run from the scripts directory 
##===============================================================#
## Set up--------
##===============================================================#
library(dplyr)
library(sf)
library(tidyverse)
library(raster)
library(rgdal)
library(maptools)
library(osmdata)
library(RColorBrewer)
library(osmplotr)
library(rjson)
require(RCurl)
require(lubridate)

source('./synthetic_mosquitoes_functions.R')
source('./synthetic_geographic_locations_functions.R')

##===============================================================#
## Functions--------------
##===============================================================#
synth_locations_shpdata <- function(country_name_gdam,
                                   country_code,
                                   city_name,
                                   city_code,
                                   raw_data_path = '../data/raw_data/geodata')
{
    country_shp = getData('GADM', country = country_name_gdam, level = length(city_levels), path = raw_data_path)
    country_data_shp = country_shp@data %>% mutate(ROW = row_number())    
    ## for(ll in 1:length(city_levels)){
    ##     country_data_shp  = country_data_shp %>%
    ##         mutate(CITYNAME = UQ(as.name(sprintf("NAME_%d",ll))))
    ##     country_data_shp[,sprintf("NAME_%d",ll)] = gsub("['`^~\"]","",iconv(country_data_shp[,sprintf("NAME_%d",ll)],to="ASCII//TRANSLIT//IGNORE"))
    ##     country_data_shp[,sprintf("NAME_%d",ll)] = gsub(" ","", country_data_shp[,sprintf("NAME_%d",ll)])
    ## }

    ## ##  Up to three levels!!!
    ## if(length(city_levels) == 1){        
    ##     city_indx = country_data_shp %>%
    ##         filter(NAME_1 == city_levels) %>% pull(ROW)
    ## }else if(length(city_levels) == 2){
    ##     city_indx = country_data_shp %>%
    ##         filter(NAME_1 == city_levels[1], NAME_2 == city_levels[2]) %>%
    ##         pull(ROW)
    ## }else if (length(city_levels) == 3){
    ##     city_indx = country_data_shp %>%
    ##         filter(NAME_1 == city_levels[1], NAME_2 == city_levels[2], NAME_3 == city_levels[3]) %>%
    ##         pull(ROW)
    ## }
    
    ## city_shp = country_shp[city_indx,]
    ## city_bbx = bbox(city_shp)
    return(country_data_shp)
}
##===============================================================#
## Load params and metadata--------------
##===============================================================#
countries_datalist = rjson::fromJSON(
                                file = '../data/param_files/countries_latam_metadata.json',
                                simplify = F)
country_name    = "brazil"
city_name       = "Goiana"

##===============================================================#        
## Select a country and run the generator--------------
##===============================================================#
## This part requires an internet connection
city_config = countries_datalist[[country_name]][[city_name]]
city_levels = unlist(city_config$city_levels)
city_code = city_config$city_code[[1]]
country_code = city_config$country_code[[1]]
country_name_gdam = as.character(city_config$country_name_gdam)[[1]]
raster_file = city_config$raster_file[[1]]

house_file = sprintf(
    '../output/synthesized_microdata/synthetic_microdata_%s_%d.csv',
    country_name,city_code)

raw_data_path = '../data/raw_data/geodata/'

shp_data = synth_locations_shpdata(
    country_name_gdam = country_name_gdam,
    country_code = country_code,
    city_name = city_name,
    city_code = city_code,
    raw_data_path = raw_data_path)

country_data_shp = shp_data
for(ll in 1:length(city_levels)){
    country_data_shp  = country_data_shp %>%
        mutate(CITYNAME = UQ(as.name(sprintf("NAME_%d",ll))))
    country_data_shp[,sprintf("NAME_%d",ll)] = gsub("['`^~\"]","",iconv(country_data_shp[,sprintf("NAME_%d",ll)],to="ASCII//TRANSLIT//IGNORE"))
    country_data_shp[,sprintf("NAME_%d",ll)] = gsub(" ","", country_data_shp[,sprintf("NAME_%d",ll)])
}
