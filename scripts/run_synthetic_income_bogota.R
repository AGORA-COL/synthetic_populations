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


##===============================================================#
## 0. Data--------------
##===============================================================#
## Data
synth_houses = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_synth_households.txt', col_types = cols(.default = "c"))
schools_df = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_schools.txt',col_types = cols(.default = "c")) %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude))

esc_shp = rgdal::readOGR('../data/raw_data/geodata/scat_shp/scat_shp.shp')
income_shp = rgdal::readOGR('../data/raw_data/geodata/manzana_estratificacion/ManzanaEstratificacion.shp')
income_shp@data$INCOME = as.numeric(as.character(income_shp@data$ESTRATO))

##===============================================================#
## 0. Plot income--------------
##===============================================================#
my_colors = brewer.pal(6,"YlGnBu")
my_colors = colorRampPalette(my_colors)(6)
income_brk <- cut(income_shp@data$INCOME, breaks = 1:6)
my_colors = my_colors[as.numeric(income_brk)]

plot(income_shp, lwd = 0.2, col = my_colors)
points(school_coor, col = "red")

##===============================================================#
## 1. Process schools--------------
##===============================================================#
schools_df$income = 0
school_coor = coordinates(schools_df %>% dplyr::select(latitude, longitude))
colnames(school_coor) = c("LAT", "LON")
school_coor = as.data.frame(school_coor)
coordinates(school_coor) = ~  LON + LAT
proj4string(school_coor) = proj4string(income_shp)

for(ss in 1:nrow(income_shp)){
    school_income = sp::over(school_coor, income_shp[ss,])
    if(length(which(!is.na(school_income[,1]))) > 0){
        schools_df$income[which(!is.na(school_income[,1]))] = income_shp@data$INCOME[ss]
    }
}
schools_income_indx = which(schools_df$income != 0)
schools_income_df = schools_df[schools_df$income != 0, ]
schools_income_coor = school_coor[schools_income_indx]

if(length(which(schools_df$income == 0)) > 0){
    schools_df$income[which(schools_df$income == 0)] = schools_income_df$income[as.numeric(apply(rgeos::gDistance(school_coor[which(schools_df$income == 0)], schools_income_coor, byid = T), 2,which.min))]
}

write_csv(schools_df, '../output/formatted_populations/colombia_11001/colombia_11001_schools.txt')

##===============================================================#
## 2. Process households--------------
##===============================================================#
## [ ] Maybe do this in the household geo functions!!
