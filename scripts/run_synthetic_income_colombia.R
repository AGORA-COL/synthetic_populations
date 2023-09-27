setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
##===============================================================#
## Create report of Synthetic Population for Bogotá
## Author: Guido España
## Mod: Diego Veloza Diaz
## Date: 2023/09/25
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


country_name        = "colombia"
departament_name    = "TOLIMA"
metadata_file       = file.path("..", "data", "param_files", "colombia_municipios_metadata.json")

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

##===============================================================#
## 0. Data--------------
##===============================================================#
## Data
synth_houses_file <- sprintf('../output/formatted_populations/colombia_%s/colombia_%s_synth_households.txt', departamento_data$departamento_code, departamento_data$departamento_code)
schools_df_file <- sprintf('../output/formatted_populations/colombia_%s/colombia_%s_schools.txt', departamento_data$departamento_code, departamento_data$departamento_code)

synth_houses = read_csv(synth_houses_file, col_types = cols(.default = "c"))
schools_df = read_csv(schools_df_file, col_types = cols(.default = "c")) %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude))

esc_shp = rgdal::readOGR('../data/raw_data/geodata/Colombia_shp/Municipios.shpp')
income_shp = rgdal::readOGR('../data/raw_data/geodata/SHP_MGN2018_INTGRD_MANZ/MGN_ANM_MANZANA.shp')

## We're looking for municipalities
municipio_data_shp  <- income_shp@data %>% mutate(ROW = row_number())
municipio_indx = municipio_data_shp %>%
    filter(as.numeric(DPTO_CCDGO) == departamento_data$departamento_code) %>%
    pull(ROW)

municipio_shp <- spTransform(income_shp[municipio_indx, ], 
                                CRS("+proj=longlat +datum=WGS84"))

municipio_shp@data$TOTAL_HOUSES = municipio_shp@data$TP19_EE_E1 + municipio_shp@data$TP19_EE_E2 +
                                  municipio_shp@data$TP19_EE_E3 + municipio_shp@data$TP19_EE_E5 +
                                  municipio_shp@data$TP19_EE_E5 + municipio_shp@data$TP19_EE_E6

municipio_shp@data$INCOME = round((municipio_shp@data$TP19_EE_E1 + 2*municipio_shp@data$TP19_EE_E2 +
                            3*municipio_shp@data$TP19_EE_E3 + 4*municipio_shp@data$TP19_EE_E5 +
                            5*municipio_shp@data$TP19_EE_E5 + 6*municipio_shp@data$TP19_EE_E6)/municipio_shp@data$TOTAL_HOUSES)

##===============================================================#
## 0. Plot income--------------
##===============================================================#
my_colors = brewer.pal(6,"YlGnBu")
my_colors = colorRampPalette(my_colors)(6)
income_brk <- cut(municipio_shp@data$INCOME, breaks = 1:6)
my_colors = my_colors[as.numeric(income_brk)]

plot(municipio_shp, lwd = 0.2, col = my_colors)
points(school_coor, col = "red")

##===============================================================#
## 1. Process schools--------------
##===============================================================#
# Calculate the mean of the existing coordinates
mean_latitude <- mean(schools_df$latitude, na.rm = TRUE)
mean_longitude <- mean(schools_df$longitude, na.rm = TRUE)

# Replace NA values with the mean
schools_df$latitude[is.na(schools_df$latitude)] <- mean_latitude
schools_df$longitude[is.na(schools_df$longitude)] <- mean_longitude


schools_df$income = 0
school_coor = coordinates(schools_df %>% dplyr::select(latitude, longitude))
colnames(school_coor) = c("LAT", "LON")
school_coor = as.data.frame(school_coor)
coordinates(school_coor) = ~  LON + LAT
proj4string(school_coor) = proj4string(municipio_shp)

for(ss in 1:nrow(municipio_shp)){
    school_income = sp::over(school_coor, municipio_shp[ss,])
    if(length(which(!is.na(school_income[,1]))) > 0){
        schools_df$income[which(!is.na(school_income[,1]))] = municipio_shp@data$INCOME[ss]
    }
}
schools_income_indx = which(schools_df$income != 0)
schools_income_df = schools_df[schools_df$income != 0, ]
schools_income_coor = school_coor[schools_income_indx]

if(length(which(schools_df$income == 0)) > 0){
    schools_df$income[which(schools_df$income == 0)] = schools_income_df$income[as.numeric(apply(rgeos::gDistance(school_coor[which(schools_df$income == 0)], schools_income_coor, byid = T), 2,which.min))]
}

write_csv(schools_df, schools_df_file)

##===============================================================#
## 2. Process households--------------
##===============================================================#
## [ ] Maybe do this in the household geo functions!!
