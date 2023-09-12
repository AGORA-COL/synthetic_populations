##===============================================================#
## Generate json metadata file for Colombia
## Author: Diego Veloza
## Date: 2023/09/12
##===============================================================#
## Read Input-------------
##===============================================================#
library(jsonlite)
library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)

##===============================================================#
## Read Input-------------
##===============================================================#
ipums_processed_file    <- '../data/processed_data_/microdata/ipums_relation_processed.csv'
colombia_geographic_labels = unique(read_csv(ipums_processed_file) %>% dplyr::select(-c('GEO2_CO170005003')))


##===============================================================#
## Setup info-------------
##===============================================================#
departamentos       <- unique(colombia_geographic_labels$DPTO)
departamento_codes  <- unique(colombia_geographic_labels$DPTO_CODE)

municipios <- lapply(departamentos, function(d) {
  return(colombia_geographic_labels[colombia_geographic_labels$DPTO == d,]$MUN)
})

municipio_codes <- lapply(departamentos, function(d) {
  return(colombia_geographic_labels[colombia_geographic_labels$DPTO == d,]$divipola)
})

ipums_codes <- lapply(departamentos, function(d) {
  return(colombia_geographic_labels[colombia_geographic_labels$DPTO == d,]$ipums)
})

names(municipios)       <- departamentos
names(municipio_codes)  <- departamentos
names(ipums_codes)      <- departamentos


##===============================================================#
## Organize data-------------
##===============================================================#
data_list <- list()
for (departamento in departamentos) {
    data_list[[departamento]] <- list()
    departamento_code <- as.numeric(unique(colombia_geographic_labels$DPTO_CODE[colombia_geographic_labels$DPTO == departamento]))
    
    for (idx in seq_along(municipios[[departamento]])) {
        
        municipio <- municipios[[departamento]][idx]
        municipio_code <- as.numeric(municipio_codes[[departamento]][idx])
        ipums_code <- as.numeric(ipums_codes[[departamento]][idx])
        
        data_list[[departamento]][[municipio]] <- list(
            department_code     = departamento_code,
            divipola_code       = municipio_code,
            ipums_code          = ipums_code,
            country_code        = 170,
            country_name_gdam   = "COL",
            city_levels         = list(departamento, municipio),
            raster_file         = "../data/raw_data/geodata/Colombia/COL_ppp_v2b_2015_UNadj.tif",
            year_pop            = 2018
        )
    }
}

##===============================================================#
## Write JSON-------------
##===============================================================#
json_content <- toJSON(list(colombia = data_list), pretty = TRUE)
write(json_content, "../data/param_files/colombia_municipios_metadata.json")