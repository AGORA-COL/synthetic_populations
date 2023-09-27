setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
##===============================================================#
## Generate synthetic populations from start to finish
## Author: Diego Veloza
## Date: 2023/09/21
##===============================================================#
## Read Input-------------
##===============================================================#
library(tidyverse)
library(dplyr)
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


source('./synthetic_trajectories_functions_colombia.R')

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
## 0. Process inputs--------
##===============================================================#
processed_data_dir = file.path("..","data", "processed_data")
outputdir_microdata = file.path('..','output','synthesized_microdata')
report_dir = file.path("..","output","reports")

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
## 1. Read synth population----------------
##===============================================================#
people_file = file.path(
    outputdir_microdata,
    sprintf('synthetic_microdata_people_%s_%d.csv', country_name, departamento_data$departamento_code)
)

synth_houses = file.path(
    formatted_dir,              
    sprintf('%s_%d_synth_households.txt', country_name, departamento_data$departamento_code)
)

esc_shp = rgdal::readOGR('../data/raw_data/geodata/Colombia_shp/Municipios.shp')


geoinfo_file <- '../data/processed_data/geodata/geoinfo_municipios_colombia.csv'
geodata_info <- read_csv(geoinfo_file)


##===============================================================#
## 2. Read schools and workplaces----------------
##===============================================================#
students_file = '../data/processed_data/schooldata/Students_by_municipio.csv'
synth_schools = '../data/processed_data/schooldata/Schools_processed_capacity_colombia.csv'

synth_colleges      = '../data/processed_data/schooldata/IES_colombia_ESC.csv'
synth_workplaces    = '../data/processed_data/workplacedata/workplace_colombia_data.csv'
workers_mobility_file = '../data/processed_data/workplacedata/mobility_matrix_colombia_data.csv'

workers_mobility    = read_csv(workers_mobility_file)

students_mobility_df = read_csv(students_file) %>%
    mutate(SchoolType = "Public")

# students_mobility_df = bind_rows(students_mobility, priv_students_mobility) %>%
#     dplyr::select(-NameLocalidad)


# Actually read data
students_mobility = read_csv(students_file) #%>% mutate(SchoolType = "Public")
schools_df = read_csv(synth_schools)
college_df = read_csv(synth_colleges)
workplaces_df = read_csv(synth_workplaces)
houses_df = read_csv(synth_houses)


people_df =  read_csv(people_file) %>%
    dplyr::select(PERNUM, SERIAL, GENDER, SEX, AGE, HHID, RELATE, WORKSTATUS, SCHOOLSTATUS) %>%
    mutate(SCHOOL_ID = NA, WORKPLACE_ID = NA) %>%
    right_join(houses_df %>% dplyr::select(sp_id, stcotrbg, latitude, longitude), by = c("HHID" = "sp_id"))
people_df$Zone = substr(people_df$stcotrbg, 6, 13)
people_df$SCHOOL_ID = as.character(people_df$SCHOOL_ID)
people_df$WORKPLACE_ID = as.character(people_df$WORKPLACE_ID)

people_df$SCHOOLSTATUS[(which(people_df$SCHOOLSTATUS == 'bs' & people_df$AGE > 17))] = 'cs'

# Remove the first two digits
if(departament_name == 'ANTIOQUIA'){
    length_str = 2
} else {
    length_str = 3
}
people_df <- people_df %>% mutate(stcotrbg = as.numeric(substr(as.character(stcotrbg), length_str, nchar(as.character(stcotrbg)))))

##=============================================#
## Adjust workers-------------
##=============================================#
# brks = c(0,15,25,35,45,55,65,120)
# lbls = sprintf("%d-%d",brks[1:(length(brks) - 1)], brks[2:length(brks)] - 1)
# lbls[length(lbls)] = sprintf("%d-above", brks[length(brks) -1])

# census_workers = read_csv('../data/raw_data/microdata/11Bogota/CNPV2018_5PER_A2_11.CSV') %>%
#     mutate(AgeGroup = as.character(cut(as.numeric(P_EDADR)*5 - 1, breaks = brks, labels = lbls, include.lowest = T, right = F))) %>%
#     group_by(AgeGroup, P_TRABAJO) %>% summarize(N = n()) %>%
#     ungroup() %>%
#     filter(P_TRABAJO == 1 | P_TRABAJO == 2) %>%
#     group_by(AgeGroup) %>%
#     summarize(N = sum(N))%>%
#     ungroup()

##===============================================================#
## 2. Assign schools and workplaces----------------
##===============================================================#
# Assign colleges
college_list = assign_colleges(people_df, houses_df, college_df, geodata_info, departamento_data$departamento_code)
college_list$schools_out$sp_id = as.character(college_list$schools_out$sp_id)

# Assign workplaces
work_list = assign_workplaces(college_list$people_out, houses_df, workers_mobility, workplaces_df, geodata_info, departamento_data$departamento_code)

# Assign public schools #work_list$people_out
school_list = assign_schools(work_list$people_out, houses_df, schools_df, students_mobility, geodata_info, departamento_data$departamento_code, esc_shp)

# Formatting the output
people_format = format_people_out(school_list$people_out)
schools_out_df = bind_rows(school_list$schools_out, college_list$schools_out)


##===============================================================#
## 4. Write output----------------
##===============================================================#
write.csv(
    people_format$people_out,
    file.path(formatted_dir,              
              sprintf('%s_%d_synth_people.txt',country_name, departamento_data$departamento_code)
              ),
    na = "", row.names = F, quote = F
)


write.csv(
    schools_out_df %>%
    dplyr::select(sp_id,name, stabbr, address, city, county, zipcode, zip4, nces_id, total, prek, kinder, gr01_gr12, ungraded, latitude, longitude, source, stco,  income, stcotrbg, schooltype),
    file.path(formatted_dir,              
              sprintf('%s_%d_schools.txt',country_name, departamento_data$departamento_code)
              ),
    na = "", row.names = F, quote = F
)

write.csv(
    work_list$workplaces_out,
    file.path(formatted_dir,              
              sprintf('%s_%d_workplaces.txt',country_name, departamento_data$departamento_code)
              ),
    na = "", row.names = F, quote = F
)
