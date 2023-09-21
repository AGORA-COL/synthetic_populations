##===============================================================#
## Create report of Synthetic Population for Bogotá
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
library(rgdal)
library(maptools)
library(osmdata)
library(RColorBrewer)
library(osmplotr)
library(RCurl)
library(data.table)
options(digits = 22,scipen = 999)

setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
##===============================================================#
## 0. Data--------------
##===============================================================#
## Data
unidad_catastral    = read_csv('../data/processed_data/geodata/Localidad_Unidad_Catastral.csv')
synth_pop           = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_synth_people.txt', col_types = cols(.default = "c"))
synth_houses        = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_synth_households.txt', col_types = cols(.default = "c"))
schools_df          = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_schools.txt')
workplaces_df       = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_workplaces.txt')

for(localidad_filter in 1:19){
    loc_esc = filter(unidad_catastral, Localidad == localidad_filter)
    houses_loc = synth_houses %>% mutate(ESC = substr(stcotrbg, 6, 13)) %>%
        filter(ESC %in% loc_esc$SCACODIGO)
    people_loc = synth_pop %>% filter(sp_hh_id %in% houses_loc$sp_id)
    output_dir = sprintf('../output/formatted_populations/colombia_11001%02d', localidad_filter)
    if(!dir.exists(output_dir)){
        dir.create(output_dir)
    }
    write_csv(houses_loc, file.path(output_dir, sprintf('colombia_11001%02d_synth_households.txt', localidad_filter)))
    write_csv(people_loc, file.path(output_dir, sprintf('colombia_11001%02d_synth_people.txt', localidad_filter)))
    write_csv(schools_df, file.path(output_dir, sprintf('colombia_11001%02d_schools.txt', localidad_filter)))
    write_csv(workplaces_df, file.path(output_dir, sprintf('colombia_11001%02d_workplaces.txt', localidad_filter)))
}
