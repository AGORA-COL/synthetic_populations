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
library(mipfp)
library(simPop)
library(surveysd)
library(RColorBrewer)
require(RCurl)
require(lubridate)
library(data.table)
library(crayon)
options(digits = 20,scipen = 999)

##===============================================================#
## Read Input-------------
##===============================================================#
country_name = "colombia"
city_name = "Bogota"
metadata_file = file.path("data", "param_files", "countries_latam_metadata.json")
population_cap = -1
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
source('scripts/synthetic_demography_functions.R')

processed_data_dir = file.path("data", "processed_data")
outputdir_microdata = file.path('output','synthesized_microdata')
report_dir = file.path("output","reports")

school_file = 'data/raw_data/schooldata/ESTADISTICAS_EN_EDUCACION_BASICA_POR_MUNICIPIO.csv'
universities_file = "data/raw_data/schooldata/colombia_matriculados_educacion_superior_2018.xlsx"
##workplace_file = "../data/raw_data/workplacedata/CENSUS_WORKPLACESv2.csv"
workplace_file = 'data/processed_data/workplacedata/workplace_bogota_data.csv'

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
#raster_file = unlist(city_data$raster_file)
raster_file<-"data/raw_data/geodata/Colombia/COL_ppp_v2b_2015_UNadj.tif"

formatted_dir = file.path("output", "formatted_populations",
                          sprintf("%s_%d", country_name, city_code))

if(!dir.exists(formatted_dir)){dir.create(formatted_dir,recursive = T)}
if(!dir.exists(outputdir_microdata)){dir.create(outputdir_microdata, recursive = T)}


##===============================================================#
## 1.2. Read zones age group file--------
##===============================================================#
unidad_catastral = read_csv('data/processed_data/geodata/Localidad_Unidad_Catastral.csv') %>%
    filter(Localidad != 20)
manzana_catastro = read_csv('data/processed_data/geodata/Manzana_Unidad_Catastral.csv')
geodata_info = left_join(manzana_catastro, unidad_catastral, by = "SCACODIGO")

agegroup_df = read_csv('data/processed_data/popdata/bogota_population_data.csv')
zones_list = unique(agegroup_df$Zone)

##total_city_pop = sum(agegroup_df[agegroup_df$Gender != "Total","Pop"])

age_sec_df = read_csv('data/processed_data/popdata/bogota_population_data_sec.csv') %>%
    dplyr::filter(Zone %in% unidad_catastral$SCACODIGO)
household_comp = read_csv('data/processed_data/popdata/bogota_household_composition_sec.csv') %>%
    dplyr::filter(Zone %in% unidad_catastral$SCACODIGO)

total_city_pop = sum(age_sec_df$Pop)
zones_list = unique(age_sec_df$Zone)

##===============================================================#
## 2. Use the population sinthesizer--------
##===============================================================#
city_data$microdata_file = file.path(
    processed_data_dir,  "microdata", 
    sprintf('%s_microdata_%03d%06d.csv',
            country_name,
            unlist(city_data$country_code),
            unlist(city_data$city_code))
)

city_data$agepop_file = file.path(
    processed_data_dir, "popdata",
    sprintf('%s_population_data.csv', country_name)
)

print(sprintf("creating synthetic population for country %s code %d",
              country_name, unlist(city_data$city_code)))

pop_counter = 0
house_counter = 0
## TODO:
## [ ] What's going on with household ocmpositions?
## [ ] Get the number of workers for each zone from official file
for(zz in 1:length(zones_list)){
    cat(crayon::red(sprintf("\n%s current_pop %d\n",zones_list[zz], pop_counter)))
    tmp_age_file = "tmp_age_file.csv"
    write_csv(x = age_sec_df %>% filter(Zone == zones_list[zz]) %>%dplyr::select(-Zone),
              path = tmp_age_file)
    tmp_house =  household_comp %>% filter(Zone == zones_list[zz])
    tmp_pop = synthesize_population_bog(
        microdata_file = unlist(city_data$microdata_file),
        agepop_file = tmp_age_file,
        country_code = unlist(city_data$country_code),
        adm_code = unlist(city_data$city_ipums_code),
        adm_census_code = unlist(city_data$city_code),
        year_pop = unlist(city_data$year_pop),
        school_file = school_file,
        university_file = university_file,
        workplace_file = workplace_file,
        people_file = sprintf("%s/synthetic_microdata_people_%s_%d.csv",
                              outputdir_microdata,
                              country_name,
                              unlist(city_data$city_code)),
        house_file = sprintf("%s/synthetic_microdata_%s_%d.csv",
                             outputdir_microdata,
                         country_name,
                         unlist(city_data$city_code)),
        fitHCons = T,
        HCons_df = tmp_house,
        capPop = population_cap,
        subcity = TRUE,
        subcity_counter = pop_counter,
        subcity_zone = zones_list[zz],
        subcity_total_pop = total_city_pop,
        house_counter)
    unlink(tmp_age_file)
    pop_counter = pop_counter + tmp_pop$total_pop
    house_counter = tmp_pop$total_houses
}
