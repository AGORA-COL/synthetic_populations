##===============================================================#
## Generate synthetic transportation
## Author: Guido España
## Date: 2019/07/17
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
options(digits = 20,scipen = 999)

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

synth_people_file = sprintf(
    '../output/formatted_populations/%s_%d/%s_%d_synth_people.txt',
    country_name, city_code, country_name, city_code)

transport_stations_file = sprintf('../data/processed_data/transportdata/%s_publictransport_%d.csv',
                             country_name, city_code)

house_transport_file = sprintf('../data/processed_data/transportdata/%s_%d_house_transport.csv',
                             country_name, city_code)


stations_df = read_csv(transport_stations_file)

people_df = read_csv(synth_people_file) %>%
    dplyr::select(sp_id, sp_hh_id, age, sp_work_id, sp_school_id) %>%
    left_join(read_csv(house_transport_file), by = c("sp_hh_id" = "sp_id"))

##===============================================================#
## Microsimulation-------------
## 1. Early morning: 5,6,7 -> People go to school or work
##===============================================================#
## Things to improve:
## 1. Choose only school and work people for early morning?
station_capacity = people_df %>% group_by(station_id) %>%
    summarize(people = n())%>%
    ungroup() %>%
    left_join(stations_df, by = c("station_id" = "station_ID_number"))

minutes_split = 1
hrs_to_simulate = seq(from=5,length.out = 19)
total_hrs = length(hrs_to_simulate)
new_people_cols = sprintf("hr_%02d", hrs_to_simulate)
new_cols_mat = matrix(rep(-1, nrow(people_df)*length(new_people_cols)), ncol=length(new_people_cols))
colnames(new_cols_mat) = new_people_cols

people_df[,new_people_cols] = new_cols_mat

## From home to work/school, find home station, so these indices are for many hours
ss = 17
station_id_in = station_capacity$station_id[ss]
station_name_in = station_capacity$station_name[ss]
tmp_indx = which(people_df$station_id == station_id_in)

## Load dataframe with the expected people to enter a station
binom_tm = readRDS('../data/processed_data/transportdata/coefs_neg_binomial.rds') %>%
    mutate(no_estacion = as.integer(str_sub(nombreestacion, start = 2, end = 6)),
           estacion = str_sub(nombreestacion, start = 9, end = 20))

## For each station, and a specific hour frame, go through each minute and sample people
for(hh in c(1,2,3)){
    hr_sim = hrs_to_simulate[hh]
    ## Load Orig_Dest matrices for this hour
    Orig_Dest = readRDS(sprintf("../data/processed_data/transportdata/results/Matriz_%d.rds", hr_sim))
    tmp_station_prob = Orig_Dest[as.character(station_id_in),]
    
    ## Choose the number of people who depart from the station id every X minutes
    ## maybe change 1 for 60/X minutes? so that there's a value for each minute timeframe?
    num_people_in = rpois(1, exp(binom_tm %>% filter(no_estacion == station_id_in, c_hora == sprintf("b_h%02d",hr_sim)) %>% pull(Estimate)))
    print(sprintf("Station ID %d HR %d people in: %d", station_id_in, hr_sim, num_people_in))
    ## Finally, assign each person a destination station
    for(mm in 1:(60/minutes_split)){
        if(length(tmp_indx) > 0){
            if(length(tmp_indx) >= num_people_in){
                sampled_indx = sample(tmp_indx, num_people_in, replace = F)
            }else{
                sample_indx = tmp_indx
            }
            tmp_indx = tmp_indx[!(tmp_indx %in% sampled_indx)]
            station_id_out = sample(as.numeric(names(tmp_station_prob)), size = length(sampled_indx), prob = tmp_station_prob, replace = F)        
            people_df[sampled_indx, sprintf("hr_%02d",hr_sim)] = station_id_out    
        }else{
            print(sprintf("Reached limit for station %s-%d!!!", station_name_in, station_id_in))    
        }
    }
}

##===============================================================#
## Plot station capacity-------------
##===============================================================#
jpeg(sprintf('../output/reports/figure_synthetic_transport_%s_%d.jpeg', country_name, city_code), width=7,height=7, units="in", res = 300)
plot(station_capacity$station_longitude, station_capacity$station_latitude, cex = 6 * station_capacity$people / max(station_capacity$people), col = "#3182bd50", pch = 16,
     main = "Personas asignadas a cada estación", ylab = "latitud", xlab = "longitud")

points(station_capacity$station_longitude, station_capacity$station_latitude, cex = 6 * station_capacity$people / max(station_capacity$people), col = "#3182bd", pch = 1)


dev.off()

##===============================================================#
## Plot routs-------------
##===============================================================#
