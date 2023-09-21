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

setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')

##===============================================================#
## Read Input-------------
##===============================================================#
country_name = "colombia"
city_name = "Bogota"
metadata_file = file.path("..", "data", "param_files", "countries_latam_metadata.json")
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
source('./synthetic_demography_functions.R')

processed_data_dir = file.path("..","data", "processed_data_")
outputdir_microdata = file.path('..','output','synthesized_microdata')
report_dir = file.path("..","output","reports")

school_file         = '../data/raw_data_/schooldata/ESTADISTICAS_EN_EDUCACION_BASICA_POR_MUNICIPIO.csv'
universities_file   = "../data/raw_data_/schooldata/colombia_matriculados_educacion_superior_2018.xlsx"
##workplace_file    = "../data/raw_data/workplacedata/CENSUS_WORKPLACESv2.csv"
workplace_file      = '../data/processed_data_/workplacedata/workplace_bogota_data.csv'

##===============================================================#
## 0. Process inputs--------
##===============================================================#
countries_datalist = rjson::fromJSON(
                                file = metadata_file,
                                simplify = F)

country_data    = countries_datalist[[country_name]]
city_data       = country_data[[city_name]]
city_code       = unlist(countries_datalist[[country_name]][[city_name]]$city_code)
country_code    = unlist(countries_datalist[[country_name]][[city_name]]$country_code)

city_levels         = unlist(city_data$city_levels)
country_name_gdam   = unlist(city_data$country_name_gdam)
raster_file         = unlist(city_data$raster_file)


formatted_dir = file.path("..", "output", "formatted_populations",
                          sprintf("%s_%d", country_name, city_code))

if(!dir.exists(formatted_dir)){dir.create(formatted_dir,recursive = T)}
if(!dir.exists(outputdir_microdata)){dir.create(outputdir_microdata, recursive = T)}


##===============================================================#
## 1.2. Read zones age group file--------
##===============================================================#
unidad_catastral    = read_csv('../data/processed_data_/geodata/Localidad_Unidad_Catastral.csv') %>% filter(Localidad != 20)
manzana_catastro    = read_csv('../data/processed_data_/geodata/Manzana_Unidad_Catastral.csv')
geodata_info        = left_join(manzana_catastro, unidad_catastral, by = "SCACODIGO")

agegroup_df = read_csv('../data/processed_data_/popdata/bogota_population_data.csv')
zones_list  = unique(agegroup_df$Zone)

##total_city_pop = sum(agegroup_df[agegroup_df$Gender != "Total","Pop"])

age_sec_df      = read_csv('../data/processed_data_/popdata/bogota_population_data_sec.csv') %>% dplyr::filter(Zone %in% unidad_catastral$SCACODIGO)
household_comp  = read_csv('../data/processed_data_/popdata/bogota_household_composition_sec.csv') %>% dplyr::filter(Zone %in% unidad_catastral$SCACODIGO)

total_city_pop = sum(age_sec_df$Pop)
zones_list = unique(age_sec_df$Zone)

age_sec_df <- age_sec_df %>%
  group_by(Zone, Gender, Year, Code) %>%
  mutate(AgeGroup = ifelse(AgeGroup %in% c("80-84", "85-89", "90-94", "95-99", "100-above"), "80-above", AgeGroup)) %>%
  group_by(AgeGroup, add = TRUE) %>%
  summarise(Pop = sum(Pop))

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
    tmp_house =  household_comp %>% filter(Zone == zones_list[zz], NumHouses > 0)
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

# zz = 3
# cat(crayon::red(sprintf("\n%s current_pop %d\n",zones_list[zz], pop_counter)))
# tmp_age_file = "tmp_age_file.csv"
# write_csv(x = age_sec_df %>% filter(Zone == zones_list[zz]) %>%dplyr::select(-Zone),
#             path = tmp_age_file)
# tmp_house =  household_comp %>% filter(Zone == zones_list[zz], NumHouses > 0)
# tmp_pop = synthesize_population_bog(
#     microdata_file = unlist(city_data$microdata_file),
#     agepop_file = tmp_age_file,
#     country_code = unlist(city_data$country_code),
#     adm_code = unlist(city_data$city_ipums_code),
#     adm_census_code = unlist(city_data$city_code),
#     year_pop = unlist(city_data$year_pop),
#     school_file = school_file,
#     university_file = university_file,
#     workplace_file = workplace_file,
#     people_file = sprintf("%s/synthetic_microdata_people_%s_%d.csv",
#                             outputdir_microdata,
#                             country_name,
#                             unlist(city_data$city_code)),
#     house_file = sprintf("%s/synthetic_microdata_%s_%d.csv",
#                             outputdir_microdata,
#                             country_name,
#                             unlist(city_data$city_code)),
#     fitHCons = TRUE,
#     HCons_df = tmp_house,
#     capPop = population_cap,
#     subcity = TRUE,
#     subcity_counter = pop_counter,
#     subcity_zone = zones_list[zz],
#     subcity_total_pop = total_city_pop,
#     house_counter)
# unlink(tmp_age_file)
# pop_counter = pop_counter + tmp_pop$total_pop
# house_counter = tmp_pop$total_houses


# zz = 1
# cat(crayon::red(sprintf("\n%s current_pop %d\n",zones_list[zz], pop_counter)))
# tmp_age_file = "tmp_age_file.csv"
# write_csv(x = age_sec_df %>% filter(Zone == zones_list[zz]) %>%dplyr::select(-Zone),
#             path = tmp_age_file)

# microdata_file = unlist(city_data$microdata_file)
# agepop_file = tmp_age_file
# country_code = unlist(city_data$country_code)
# adm_code = unlist(city_data$city_ipums_code)
# adm_census_code = unlist(city_data$city_code)
# year_pop = unlist(city_data$year_pop)
# # school_file = school_file
# # university_file = university_file
# # workplace_file = workplace_file
# people_file = sprintf("%s/synthetic_microdata_people_%s_%d.csv",
#                         outputdir_microdata,
#                         country_name,
#                         unlist(city_data$city_code))
# house_file = sprintf("%s/synthetic_microdata_%s_%d.csv",
#                         outputdir_microdata,
#                         country_name,
#                         unlist(city_data$city_code))
# fitHCons = TRUE
# HCons_df = tmp_house
# capPop = population_cap
# subcity = TRUE
# subcity_counter = pop_counter
# subcity_zone = zones_list[zz]
# subcity_total_pop = total_city_pop


# if(subcity == FALSE || subcity_counter == 0){
#     unlink(people_file)
#     unlink(house_file)
# }
# micro_data = read_csv(microdata_file) %>% mutate(HHWT = as.double(HHWT)) %>%
#     drop_na()
# micro_data$AGE[micro_data$AGE > 99] = 99

# propCap = 1.0
# if(capPop > 100000){
#     tmp_n = read_csv(agepop_file) %>%
#         filter(Code == sprintf("%05d",adm_census_code), Year == year_pop, 
#                 AgeGroup != 'Total', Gender != 'Total') %>%
#         tally(Pop) %>% pull(n)
#     if(tmp_n > capPop){
#         propCap = capPop / tmp_n
#     }
# }
# ## Read marginal distributions
# agepop_data = read_csv(agepop_file) %>% 
#     filter(Code == sprintf("%05d",adm_census_code), Year == year_pop, 
#             AgeGroup != 'Total', Gender != 'Total') %>%
#     mutate(Gender = ifelse(Gender == "Male","m","f")) %>%
#     unite(AGEGENDER,Gender,AgeGroup, sep = "") %>% 
#     dplyr::select(AGEGENDER, Pop) %>%
#     mutate(AGEGENDER = str_replace_all(AGEGENDER,"-","_")) %>%
#     mutate(Pop = Pop * propCap)  

# breaks_df = filter(agepop_data, str_detect(AGEGENDER ,'f')) %>%
#     transmute(AgeRange = substring(AGEGENDER,2),
#                 Labels = substring(AGEGENDER,2)) %>%
#     mutate(AgeRange = str_replace_all(AgeRange, "above","200")) %>%
#     separate(AgeRange, into = c("MinAge","MaxAge"), convert = TRUE)

# ## Prepare constraints for the IPF algorithm
# ## Sex: Male = 1, Female = 2    
# brks = c(as.integer(breaks_df$MinAge),200)
# lbls = breaks_df$Labels
# cat(sprintf("breaks and labels created for %d-%d\n", country_code, adm_code))
# microdata_hhsize = micro_data %>% 
#     group_by(SERIAL) %>% summarize(HHSIZE = n()) %>% ungroup()

# micro_data = micro_data %>%
#     mutate(GENDER = ifelse(SEX == 1, "m","f")) %>%
#     mutate(AGEGROUP = cut(micro_data$AGE,breaks = brks, labels = lbls, right = FALSE)) %>%
#     unite(AGEGENDER, GENDER, AGEGROUP, sep = "", remove= FALSE) %>%
#     left_join(microdata_hhsize, by = "SERIAL") %>%
#     mutate(WORKSTATUS = ifelse(EMPSTAT == 1, "w", "nw"),
#             SCHOOLSTATUS = ifelse(SCHOOL != 1, "ns",
#                                     ifelse(AGE < 19, "bs", "cs")))

# cat(sprintf("crosstabulating constraints for %d-%d\n",country_code,adm_code))
# # agepop_data <- agepop_data #%>% filter(Pop > 20)
# consP = xtabs(Pop ~ AGEGENDER, data = agepop_data)        

# if(is.null(nrow(HCons_df))){
#     stop("Please specify Houses per zone")
# }

# ## Workers CONSTRAINTS
# workers_coverage = read_csv(workplace_file)
# total_workers_coverage = sum(workers_coverage$TotalWorkers) / subcity_total_pop

# if(total_workers_coverage == 0){
#     ## at least 1% employed
#     total_workers_coverage = 0.01
# }

# workers_data = data.frame(w = floor(total_workers_coverage * sum(agepop_data$Pop)),stringsAsFactors = F) %>%
#     mutate(nw = sum(agepop_data$Pop) - w) %>%
#     gather(key = WORKSTATUS, value = Pop)

# consW = xtabs(Pop ~ WORKSTATUS, data = workers_data)

# ## SCHOOL CONSTRAINTS
# ## 1. Assume people only go to school from 4 to 18
# school_coverage = read_csv(school_file) %>%
#     rename(year = "AÑO", mun_code = "CÓDIGO_MUNICIPIO", coverage = "TASA_MATRICULACIÓN_5_16") %>%
#     dplyr::select(year, mun_code, coverage) %>% filter(year == 2018, mun_code == city_code) %>%
#     mutate(coverage = coverage / 100)

# university_coverage = readxl::read_xlsx(universities_file, skip = 5) %>%
#     filter(Semestre == 1) %>%
#     rename(mun_code = "Código del \r\nMunicipio\r\n(Programa)",
#             name = "Institución de Educación Superior (IES)",
#             capacity = "Matriculados 2018") %>%
#     dplyr::select(mun_code, name, capacity) %>%
#     mutate(mun_code = sprintf("%05d",mun_code)) %>%
#     filter(mun_code == sprintf("%05d",as.numeric(city_code)))%>%
#     group_by(mun_code, name) %>%
#     summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
#     filter(capacity > 10) %>% group_by(mun_code) %>%
#     summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
#     mutate(year = 2018, mun_code = as.numeric(mun_code))


# if(subcity == TRUE){
#     if(year_pop == 2020){
#         university_coverage$year = 2018
#         school_coverage$year = 2020
#     }
# }

# print(school_coverage)
# print(university_coverage)

# school_year_pop = read_csv(agepop_file) %>%
#     filter(Code == sprintf("%05d",adm_census_code), Year == school_coverage$year, 
#             AgeGroup != 'Total', Gender != 'Total') %>%
#     tally(Pop) %>% pull(n)

# school_pop = filter(agepop_data, AGEGENDER %in% c("m5_9","f5_9","m10_14","f10_14")) %>%
#     summarize(Pop = sum(Pop)) %>%
#     bind_rows(filter(agepop_data,AGEGENDER %in% c("m0_4","f0_4")) %>% summarize(Pop = sum(Pop)/5)) %>%
#     bind_rows(filter(agepop_data, AGEGENDER %in% c("m15_19","f15_19")) %>% summarize(Pop = sum(Pop)*4/5)) %>%
#     summarize(Pop = sum(Pop)) %>% pull(Pop)

# school_coverage$coverage = floor(school_coverage$coverage * school_pop)
# if(subcity == TRUE){
#     school_year_pop = subcity_total_pop
# }
# university_coverage$coverage = floor((university_coverage$capacity / school_year_pop)  * sum(agepop_data$Pop))

# students_data = data.frame(bs = school_coverage$coverage, cs = university_coverage$coverage, ns = sum(agepop_data$Pop) - school_coverage$coverage - university_coverage$coverage, stringsAsFactors = F) %>%
#     gather(key = SCHOOLSTATUS, value = Pop)
# print(students_data)
# consS = xtabs(Pop ~ SCHOOLSTATUS, data = students_data)


# HCons_df <- HCons_df %>% filter(NumHouses > 0)

# ## IPF Algorithm
# setDT(micro_data)
# if(fitHCons == TRUE){
#     HCons_df$HHSIZE = HCons_df$PersonsHousehold
#     consH = list(xtabs(NumHouses ~ HHSIZE, data = HCons_df))
# }else{
#     consH = NULL
# }

# cat(sprintf("calibrating weights for %d-%d\n",country_code,adm_code))
# calibrated_weights = surveysd::ipf(micro_data, hid = NULL, 
#                             conP = list(consP, consW, consS),
#                             conH = consH,
#                             epsP = 1e-7,
#                             epsH = 1e-3,
#                             w = "HHWT",
#                             bound = NULL, 
#                             verbose = TRUE, 
#                             maxIter = 2000)
