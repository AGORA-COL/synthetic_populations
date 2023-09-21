setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
##===============================================================#
## Generate synthetic populations from start to finish
## Author: Guido España - Date: 2019/07/17
## Mod: Diego Veloza - Date: 2023/09/20
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

contains_grade <- function(grade_str, grade_target){
    grade_df = tibble(grade = as.numeric(unlist(str_split(grade_str, ",")))) %>%
        mutate(grade_name = ifelse(grade < 0, "prek",
                            ifelse(grade == 0, "kinder",
                            ifelse(grade < 12, "gr01_gr12","ungraded"))))%>%
        replace_na(list(grade_name = "ungraded"))
    if(grade_target %in% grade_df$grade_name){
        return(1)
    }else{
        return(0)
    }
}
##===============================================================#
## Read Input-------------
##===============================================================#
country_name        = "colombia"
departament_name    = "CÓRDOBA"
metadata_file       = file.path("..", "data", "param_files", "colombia_municipios_metadata.json")
country_shp_file    = "../data/raw_data/geodata/Colombia_shp/Municipios.shp"

args = (commandArgs(TRUE))
if(length(args) == 3){
    country_name = args[1]
    city_name = args[2]
    metadata_file = args[3]
}
print(sprintf("Running synthetic population generator for %s:%s from %s",country_name, departament_name, metadata_file))

##===============================================================#
## Set up-------------
##===============================================================#
source('./synthetic_geographic_locations_functions.R')

processed_data_dir  = file.path("..","data", "processed_data")
outputdir_microdata = file.path('..','output','synthesized_microdata')
raw_geodata_path    = file.path('..','data','raw_data','geodata')
report_dir          = file.path("..","output","reports")
osm_retag_file      = file.path('..','data','processed_data','geodata','retag_OSM_buildings.csv')

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
students_df = read_csv(people_file) %>%
    filter(SCHOOLSTATUS != "ns") %>%
    mutate(GRADE = ifelse(AGE < 5, "prek",
                   ifelse(AGE == 5, "kinder",
                   ifelse(AGE <= 18, "gr01_gr12","ungraded")))) %>%
    group_by(GRADE) %>% summarize(ENROLLED = n()) %>% ungroup() %>%
    spread(key = GRADE, value = ENROLLED)     
students_df$total = rowSums(students_df)

## TODO!!!!###############################
## Change this file to the updated one?
schools_file = "../data/raw_data/schooldata/Sedes_colegios_2013.csv"
schools_list = read_delim(schools_file, delim = ";", locale = locale(encoding = "latin1"), trim_ws = T) %>%
    rename(mun_code = "Código municipio", grades = "Grados", name = "Nombre Establecimiento") %>%
    filter(mun_code == sprintf("%05d", city_code)) %>%
    dplyr::select(mun_code, grades, name) %>%
    replace_na(list(name = "UNNAMED")) %>%
    mutate(prek = 0, kinder = 0, gr01_gr12 = 0, ungraded = 0) %>%
    mutate(school_id =  sprintf('%03d%06d%07d', country_code, departamento_data$departamento_code, row_number()),
           name = gsub(",","",name))

schools_list$prek = sapply(1:nrow(schools_list), function(x){contains_grade(schools_list$grades[x],"prek")})
schools_list$kinder = sapply(1:nrow(schools_list), function(x){contains_grade(schools_list$grades[x],"kinder")})
schools_list$gr01_gr12 = sapply(1:nrow(schools_list), function(x){contains_grade(schools_list$grades[x],"gr01_gr12")})
schools_list$ungraded = sapply(1:nrow(schools_list), function(x){contains_grade(schools_list$grades[x],"ungraded")})

## Assign capacity to schools based on the students by grade
schools_list = schools_list %>% dplyr::select(-grades) %>%
    gather(key = grade, value = availability, -c(mun_code, name,school_id)) %>%
    filter(availability == 1, grade != "ungraded")

## ENSURE THAT ALL SCHOOLS HAVE AT LEAST SOME STUDENTS
school_df = tibble()
for(grade_name in c("prek","kinder","gr01_gr12")){
    tmp_school = filter(schools_list, grade == grade_name)
    tmp_ind = rep(1:nrow(tmp_school),floor( as.numeric(students_df[1,grade_name] / nrow(tmp_school))))
    tmp_df = tmp_school[tmp_ind,]
    tmp_df2 = sample_n(tmp_school, size = as.numeric(students_df[1,grade_name]) - length(tmp_ind), replace = T) %>%
        bind_rows(tmp_df) %>%
        group_by(mun_code, grade, school_id, name) %>%
        summarize(capacity = sum(availability))
    school_df = bind_rows(school_df,tmp_df2)
}

school_df = spread(school_df, key = grade, value = capacity, fill = 0) %>%
    mutate(ungraded = 0)

## Read universities and assign capacity
## https://www.mineducacion.gov.co/sistemasinfo/Informacion-a-la-mano/212400:Estadisticas
universities_file = "../data/raw_data/schooldata/colombia_matriculados_educacion_superior_2018.xlsx"
universities_df = readxl::read_xlsx(universities_file, skip = 5) %>%
    filter(Semestre == 1) %>%
    rename(mun_code = "Código del \r\nMunicipio\r\n(Programa)",
           name = "Institución de Educación Superior (IES)",
           capacity = "Matriculados 2018") %>%
    dplyr::select(mun_code, name, capacity) %>%
    mutate(mun_code = sprintf("%05d",mun_code),
           name = gsub(",","",name)) %>%
    filter(mun_code == sprintf("%05d",as.numeric(departamento_data$departamento_code)))%>%
    group_by(mun_code, name) %>%
    summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
    filter(capacity > 10)

universities_df$total = sum(universities_df$capacity)

universities_df$ungraded = floor((universities_df$capacity / universities_df$total) * students_df$ungraded)
if(students_df$ungraded > sum(universities_df$ungraded)){
    universities_df$ungraded[1] = universities_df$ungraded[1] + (students_df$ungraded - sum(universities_df$ungraded))
}
universities_df = universities_df %>%
    dplyr::mutate(school_id =  sprintf('%03d%06d%07d', country_code, departamento_data$departamento_code, row_number() + nrow(school_df)),
                  mun_code = as.character(mun_code),
                  prek = 0, gr01_gr12 = 0, kinder = 0) %>%
    dplyr::select(mun_code, school_id, name, prek, kinder, gr01_gr12,ungraded)

schools_combined = bind_rows(school_df, universities_df)

##===============================================================#
## 2. Format workplaces--------
## TODO: What to do when there is no info on workplaces?
##===============================================================#
## Get pop for this year
workers_df = read_csv(people_file)%>%
    filter(EMPSTAT == 1)

total_synth_workers = nrow(workers_df)

workplace_file = "../data/raw_data/workplacedata/CENSUS_WORKPLACESv2.csv"
workplace_stats = read_csv(workplace_file) %>%
    mutate(mun_code = sprintf("%05d",codigo)) %>%
    rename(Pop = poblacion, total_workplaces = "Unidades economicas total",
           total_workers = "Personas que trabajaron (Prom.) total",
           workplaces_1 = "Unidades economicas(1 per)",
           workers_1 = "Personas que trabajaron (Prom.) 1 per",
           workplaces_5 = "Unidades economicas (2-5)",
           workers_5 = "Personas que trabajaron (Prom.) (2-5)",
           workplaces_9 = "Unidades economicas(6-9)",
           workers_9 = "Personas que trabajaron (Prom.)(6-9)",
           workplaces_10 = "Unidades economicas (10)",
           workers_10 = "Personas que trabajaron (Prom.)(10)",
           workplaces_20 = "Unidades economicas(11-20)",
           workers_20 = "Personas que trabajaron (Prom.)11-20",
           workplaces_50 = "Unidades economicas(21-50)",
           workers_50 = "Personas que trabajaron (Prom.)(21-50)",
           workplaces_200 = "Unidades economicas (51-200)",
           workers_200 = "Personas que trabajaron (Prom.)(51-200)",
           workplaces_500 = "Unidades economicas(201-500)",
           workers_500 = "Personas que trabajaron (Prom.)(201-500)",
           workplaces_900 = "Unidades economicas >500",
           workers_900 = "Personas que trabajaron (Prom.) >500") %>%
    filter(mun_code == sprintf("%05d",as.numeric(departamento_data$departamento_code))) %>%
    dplyr::select(Pop, mun_code, starts_with('work')) %>%
    mutate(total_workers = sum(workers_1, workers_5, workers_9, workers_10,workers_20,workers_50,workers_200,workers_500,workers_900,na.rm = T))

tmp_workplace_stats = workplace_stats %>% dplyr::select( mun_code, starts_with('workplaces')) %>% gather(key = workplace_size, value = N_workplaces, -c( mun_code)) %>% separate(workplace_size, into = c("type", "worksize"), sep = "_")
tmp_workers_stats = workplace_stats %>% dplyr::select( mun_code, starts_with('workers')) %>% gather(key = workers_size, value = N_workers, -c(mun_code)) %>% separate(workers_size, into = c("type", "worksize"), sep = "_")

## grouping by workplace capacity and calculating new number of workplaces based on total workers
workplace_capacity = left_join(tmp_workplace_stats, tmp_workers_stats,
                               by = c("mun_code", "worksize")) %>% dplyr::select(mun_code, worksize, N_workplaces, N_workers)

workplace_capacity$max_workers = c(1,as.numeric(workplace_capacity$worksize)[2:(nrow(workplace_capacity))])
workplace_capacity$min_workers = c(1,1+as.numeric(workplace_capacity$worksize)[1:(nrow(workplace_capacity) - 1)])

workplace_capacity = workplace_capacity %>%
    mutate(Mean_workers = N_workers / N_workplaces) %>%
    mutate(N_workplaces = floor(N_workplaces * total_synth_workers / sum(N_workers))) %>%
    mutate(N_workers = floor(Mean_workers * N_workplaces))

if(total_synth_workers > sum(workplace_capacity$N_workers)){
    workplace_capacity$N_workers[workplace_capacity$worksize == 900] = workplace_capacity$N_workers[workplace_capacity$worksize == 900] + (total_synth_workers - sum(workplace_capacity$N_workers))
}
workplace_capacity = dplyr::select(workplace_capacity,c(mun_code, worksize, N_workplaces,N_workers, min_workers, max_workers))

## Go through each column and sample N_workplaces with average capacity of mean workers
## TODO: Improve this assignment!!!!
workplace_df = tibble()
for(nn in 1:nrow(workplace_capacity)){
    if(workplace_capacity$min_workers[nn] == 1){
        tmp_df = tibble(capacity = rep(1, workplace_capacity$N_workplaces[nn]))
    }else{
        ## 1. Sample minimum capacity for all workplaces
        tmp_capacity = workplace_capacity[nn,]
        if(tmp_capacity$max_workers == 900){
            tmp_capacity$max_workers = 2000
        }
        tmp_df = tibble(capacity = rep(tmp_capacity$min_workers, tmp_capacity$N_workplaces))

        extra_capacity = tmp_capacity$N_workers - sum(tmp_df$capacity)
        if(extra_capacity > 0){
            ## 2. Sample extra workers
            ## 2.1. repeat the list (max - min) times
            ind_tmp_work_rep = rep(1:nrow(tmp_df), tmp_capacity$max_workers - tmp_capacity$min_workers)
            
            ## 2.2. sample extra_capacity from repeated_list
            selected_workplaces = as.data.frame(table(sample(ind_tmp_work_rep, size = extra_capacity, replace = F)))
            tmp_df$capacity[selected_workplaces$Var1] = tmp_df$capacity[selected_workplaces$Var1] + selected_workplaces$Freq
        }
    }
    workplace_df = bind_rows(workplace_df, tmp_df)
}
workplace_df = workplace_df %>%
    mutate(mun_code = departamento_data$departamento_code,
           workplace_id =  sprintf('%03d%06d%07d', country_code,departamento_data$departamento_code,row_number()))

## Adjust final capacities
if(total_synth_workers != sum(workplace_df$capacity)){
    stop(sprintf("Stopping because the total expected workers (%.0f) differ from workplace capacity (%.0f)", total_synth_workers, sum(workplace_df$capacity)))
}

##===============================================================#
## 3. Use the Geo-Locations generator--------
##===============================================================#
house_file = file.path(
    outputdir_microdata,
    sprintf('synthetic_microdata_%s_%d.csv', country_name, departamento_data$departamento_code)
)

synth_locations = synthesize_locations(
    house_file = house_file,
    country_name_gdam = country_name_gdam,
    country_code = country_code,
    city_name = city_name,
    city_levels = divipola_codes,
    country_shp_file = country_shp_file,
    city_code = departamento_data$departamento_code,
    school_df = schools_combined,
    workplace_df = workplace_df,
    urban_limit_in = 1,
    raw_data_path = raw_geodata_path,
    osm_retag_file = osm_retag_file,
    report_locations = TRUE,
    prop_houses_other_buildings = 0.5,
    report_dir = report_dir)


write.csv(
    synth_locations$city_houses,
    file.path(formatted_dir,              
              sprintf('%s_%d_synth_households.txt',country_name, departamento_data$departamento_code)
              ),
    na = "",row.names = F,quote = F
)

write.csv(
    synth_locations$city_schools,
    file.path(formatted_dir,              
              sprintf('%s_%d_schools.txt',country_name, departamento_data$departamento_code)
              ),
    na = "", row.names = F,quote =F
)

write.csv(
    synth_locations$city_workplaces,
    file.path(formatted_dir,              
              sprintf('%s_%d_workplaces.txt',country_name, departamento_data$departamento_code)
              ),
    na = "", row.names = F, quote = F
)