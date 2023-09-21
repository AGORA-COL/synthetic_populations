setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
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
country_name        = "colombia"
departament_name    = "SANTANDER"
metadata_file       = file.path("..", "data", "param_files", "colombia_municipios_metadata.json")
population_cap = -1

args = (commandArgs(TRUE))
if(length(args) == 3){
    country_name = args[1]
    departament_name = args[2]
    metadata_file = args[3]
}
print(sprintf("Running synthetic population generator for %s:%s from %s",country_name, departament_name, metadata_file))

##===============================================================#
## Set up-------------
##===============================================================#
source('./synthetic_demography_functions.R')

processed_data_dir  = file.path("..","data", "processed_data")
outputdir_microdata = file.path('..','output','synthesized_microdata')
report_dir = file.path("..","output","reports")

school_file         = '../data/raw_data/schooldata/ESTADISTICAS_EN_EDUCACION_BASICA_POR_MUNICIPIO.csv'
universities_file   = "../data/raw_data/schooldata/IES_Estudiantes_matriculados_2019.xlsx"
##workplace_file    = "../data/raw_data/workplacedata/CENSUS_WORKPLACESv2.csv"
workplace_file      = '../data/processed_data/workplacedata/workplace_colombia_data.csv'

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
departamento_data$country_code <- departamento_data[[1]]$country_code[[1]]
departamento_data$departamento_code <- departamento_data[[1]]$department_code[[1]]
departamento_data$year_pop <- departamento_data[[1]]$year_pop[[1]]

formatted_dir = file.path("..", "output", "formatted_populations",
                          sprintf("%s_%d", country_name, departamento_data$departamento_code))

if(!dir.exists(formatted_dir)){dir.create(formatted_dir, recursive = T)}
if(!dir.exists(outputdir_microdata)){dir.create(outputdir_microdata, recursive = T)}

##===============================================================#
## 1.2. Read zones age group file--------
##===============================================================#
age_sec_df      = read_csv('../data/processed_data/popdata/colombia_population_data_municp.csv') %>% dplyr::filter(as.integer(Zone) %in% as.numeric(municipio_data$divipola_code))
household_comp  = read_csv('../data/processed_data/popdata/colombia_household_composition_municp.csv') %>% dplyr::filter(as.integer(Zone) %in% as.numeric(municipio_data$divipola_code))

zones_list = as.integer(unique(age_sec_df$Zone))

age_sec_df <- age_sec_df %>%
  group_by(Zone, Gender, Year, Code) %>%
  mutate(AgeGroup = ifelse(AgeGroup %in% c("80-84", "85-89", "90-94", "95-99", "100-above"), "80-above", AgeGroup)) %>%
  group_by(AgeGroup, add = TRUE) %>%
  summarise(Pop = sum(Pop)) %>% filter(Zone != '00')

##===============================================================#
## 2. Use the population sinthesizer--------
##===============================================================#
departamento_data$microdata_file = file.path(
    processed_data_dir,  "microdata", 
    sprintf('%s_microdata_%03d%03d.csv',
            country_name,
            unlist(departamento_data$country_code),
            unlist(departamento_data$departamento_code))
)

departamento_data$agepop_file = file.path(
    processed_data_dir, "popdata",
    sprintf('%s_population_data.csv', country_name)
)

print(sprintf("creating synthetic population for country %s departamento %d",
              country_name, unlist(departamento_data$departamento_code)))

pop_counter = 0
house_counter = 0

for(zz in 5:length(zones_list)){
    mun_code_ = zones_list[zz]
    total_city_pop = sum((age_sec_df %>% filter(as.numeric(Zone) == mun_code_, Year == unique(departamento_data$year_pop)))$Pop)

    cat(crayon::red(sprintf("\n%s current_pop %d\n",zones_list[zz], pop_counter)))
    tmp_age_file = "tmp_age_file.csv"
    write_csv(x = age_sec_df %>% filter(as.numeric(Zone) == zones_list[zz]) %>%dplyr::select(-Zone),
              path = tmp_age_file)
    tmp_house =  household_comp %>% filter(as.numeric(Zone) == zones_list[zz], NumHouses > 0)
    tmp_pop = synthesize_population_col(
        microdata_file = unlist(departamento_data$microdata_file),
        agepop_file = tmp_age_file,
        country_code = unlist(departamento_data$country_code),
        adm_code = unlist(departamento_data$departamento_code),
        adm_census_code = mun_code_,
        year_pop = unlist(departamento_data$year_pop),
        school_file = school_file,
        university_file = university_file,
        workplace_file = workplace_file,
        people_file = sprintf("%s/synthetic_microdata_people_%s_%d.csv",
                              outputdir_microdata,
                              country_name,
                              unlist(departamento_data$departamento_code)),
        house_file = sprintf("%s/synthetic_microdata_%s_%d.csv",
                             outputdir_microdata,
                         country_name,
                         unlist(departamento_data$departamento_code)),
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

# universities_file_old   = "../data/raw_data_/schooldata/colombia_matriculados_educacion_superior_2018.xlsx"
# university_coverage_old = readxl::read_xlsx(universities_file_old, skip = 5)


# university_coverage = readxl::read_xlsx(universities_file, skip = 7)

# zz = 1
# mun_code_ = zones_list[zz]

# t = university_coverage %>%
#     filter(SEMESTRE == 1) %>%
#     rename(mun_code = "CÓDIGO DEL MUNICIPIO (PROGRAMA)",
#             name = "INSTITUCIÓN DE EDUCACIÓN SUPERIOR (IES)",
#             capacity = "MATRICULADOS") %>%
#     dplyr::select(mun_code, name, capacity) %>%
#     filter(as.numeric(mun_code) == mun_code_)

# unique(t$mun_code)



# university_coverage = readxl::read_xlsx(universities_file, skip = 7) %>%
#     filter(SEMESTRE == 1) %>%
#     rename(mun_code = "CÓDIGO DEL MUNICIPIO (PROGRAMA)",
#             name = "INSTITUCIÓN DE EDUCACIÓN SUPERIOR (IES)",
#             capacity = "MATRICULADOS") %>%
#     dplyr::select(mun_code, name, capacity) %>%
#     filter(as.numeric(mun_code) == mun_code_) %>%
#     group_by(mun_code, name) %>%
#     summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
#     filter(capacity > 10) %>% group_by(mun_code) %>%
#     summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
#     mutate(year = 2019, mun_code = as.numeric(mun_code))