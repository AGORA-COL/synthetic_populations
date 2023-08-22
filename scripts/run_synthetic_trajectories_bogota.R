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
source('./synthetic_trajectories_functions_bogota.R')

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

unidad_catastral = read_csv('../data/processed_data/geodata/Localidad_Unidad_Catastral.csv') %>%
    filter(Localidad != 20)
manzana_catastro = read_csv('../data/processed_data/geodata/Manzana_Unidad_Catastral.csv')
##geodata_info = left_join(manzana_catastro, unidad_catastral, by = "SCACODIGO")
geodata_info = unidad_catastral

##===============================================================#
## 2. Read schools and workplaces----------------
##===============================================================#
students_file = '../data/processed_data/schooldata/Students_by_localidad.csv'
priv_students_file = '../data/processed_data/schooldata/Students_by_localidad_private.csv'

synth_schools = '../data/processed_data/schooldata/Schools_processed_capacity_bogota.csv'
synth_priv_schools = '../data/processed_data/schooldata/Schools_processed_capacity_bogota_private.csv'

synth_colleges = '../data/processed_data/schooldata/IES_Bogota_Geo_ESC.csv'
synth_workplaces = '../data/processed_data/workplacedata/workplace_bogota_data.csv'
workers_mobility_file = '../data/processed_data/workplacedata/mobility_matrix_bogota_data.csv'
workers_mobility = read_csv(workers_mobility_file)

students_mobility = read_csv(students_file) %>%
    mutate(SchoolType = "Public")
priv_students_mobility = read_csv(priv_students_file) %>%
    mutate(SchoolType = "Private")

students_mobility_df = bind_rows(students_mobility, priv_students_mobility) %>%
    dplyr::select(-NameLocalidad)
esc_shp = rgdal::readOGR('../data/raw_data/geodata/scat_shp/scat_shp.shp')

## Actually read data
schools_df = read_csv(synth_schools)
priv_schools_df = read_csv(synth_priv_schools)
college_df = read_csv(synth_colleges)
workplaces_df = read_csv(synth_workplaces)
houses_df = read_csv(synth_houses)


people_df =  read_csv(people_file) %>%
    dplyr::select(PERNUM, SERIAL, GENDER, SEX, AGE, HHID,RELATE, WORKSTATUS, SCHOOLSTATUS) %>%
    mutate(SCHOOL_ID = NA, WORKPLACE_ID = NA) %>%
    right_join(houses_df %>% dplyr::select(sp_id, stcotrbg, latitude,longitude),by = c("HHID" = "sp_id"))
people_df$Zone = substr(people_df$stcotrbg, 6, 13)
people_df$SCHOOL_ID = as.character(people_df$SCHOOL_ID)
people_df$WORKPLACE_ID = as.character(people_df$WORKPLACE_ID)

people_df$SCHOOLSTATUS[(which(people_df$SCHOOLSTATUS == 'bs' & people_df$AGE > 17))] = 'cs'

##=============================================#
## Adjust workers-------------
##=============================================#
brks = c(0,15,25,35,45,55,65,120)
lbls = sprintf("%d-%d",brks[1:(length(brks) - 1)], brks[2:length(brks)] - 1)
lbls[length(lbls)] = sprintf("%d-above", brks[length(brks) -1])

census_workers = read_csv('../data/raw_data/microdata/11Bogota/CNPV2018_5PER_A2_11.CSV') %>%
    mutate(AgeGroup = as.character(cut(as.numeric(P_EDADR)*5 - 1, breaks = brks, labels = lbls, include.lowest = T, right = F))) %>%
    group_by(AgeGroup, P_TRABAJO) %>% summarize(N = n()) %>%
    ungroup() %>%
    filter(P_TRABAJO == 1 | P_TRABAJO == 2) %>%
    group_by(AgeGroup) %>%
    summarize(N = sum(N))%>%
    ungroup()

##===============================================================#
## 2. Assign schools and workplaces----------------
##===============================================================#
## list_synth_people = assign_schools_workplaces(people_file, synth_houses,synth_schools,synth_priv_schools,
##                                               students_mobility_df, workers_mobility,
##                                               synth_colleges, synth_workplaces, geodata_info, esc_shp)
college_list = assign_colleges(people_df, houses_df, college_df, geodata_info, esc_shp)
college_list$schools_out$sp_id = as.character(college_list$schools_out$sp_id)
work_list = assign_workplaces(college_list$people_out, houses_df, workers_mobility, workplaces_df, geodata_info, esc_shp)
school_list = assign_schools(work_list$people_out, houses_df, schools_df, priv_schools_df, students_mobility_df,geodata_info, esc_shp)

people_format = format_people_out(school_list$people_out)
schools_out_df = bind_rows(school_list$schools_out, college_list$schools_out)

##===============================================================#
## 4. Write output----------------
##===============================================================#
write.csv(
    people_format$people_out,
    file.path(formatted_dir,              
              sprintf('%s_%d_synth_people.txt',country_name,city_code)
              ),
    na = "", row.names = F, quote = F
)


write.csv(
    schools_out_df %>%
    dplyr::select(sp_id,name, stabbr, address, city, county, zipcode, zip4, nces_id, total, prek, kinder, gr01_gr12, ungraded, latitude, longitude, source, stco,  income, stcotrbg, schooltype),
    file.path(formatted_dir,              
              sprintf('%s_%d_schools.txt',country_name,city_code)
              ),
    na = "", row.names = F, quote = F
)

write.csv(
    work_list$workplaces_out,
    file.path(formatted_dir,              
              sprintf('%s_%d_workplaces.txt',country_name,city_code)
              ),
    na = "", row.names = F, quote = F
)
