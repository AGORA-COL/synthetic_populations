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

age_data = read_csv('../data/processed_data/popdata/bogota_population_data_sec.csv')
household_comp = read_csv('../data/processed_data/popdata/bogota_household_composition_sec.csv')
synth_pop = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_synth_people.txt', col_types = cols(.default = "c"))
synth_houses = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_synth_households.txt', col_types = cols(.default = "c"))
schools_df = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_schools.txt')
workplaces_df = read_csv('../output/formatted_populations/colombia_11001/colombia_11001_workplaces.txt')

esc_shp = rgdal::readOGR('../data/raw_data/geodata/scat_shp/scat_shp.shp')
localidad_shp = rgdal::readOGR('../data/raw_data/geodata/localidades_bogota/poligonos-localidades.shp')
block_shp = rgdal::readOGR('../data/raw_data/geodata/vulnrb_data/VULNRB_IPMxMZ.shp')
upz_shp = rgdal::readOGR('../data/raw_data/geodata/UPZ_Bogota/UPla.shp')
    
##===============================================================#
## 1. Population pyramid and household composition--------------
##===============================================================#
pop_data = age_data %>% group_by(AgeGroup, Gender) %>%
    summarize(Pop = sum(Pop)) %>%
    ungroup() %>%
    spread(key = Gender, value = Pop) %>%
    separate(AgeGroup, into = c('MinAge','MaxAge'), remove = F) %>%
    mutate(MinAge = as.numeric(MinAge)) %>%
    arrange(MinAge)

## Read synth population
brks = c(sort(pop_data$MinAge), 200)
lbls = sprintf("%d-%d",brks[1:(length(brks) - 1)], brks[2:length(brks)] - 1)
lbls[length(lbls)] = sprintf("%d-above", brks[length(brks) -1])

synth_pop_df = synth_pop %>%
    mutate(AgeGroup = cut(as.numeric(age), breaks =brks, labels = lbls, include.lowest = T,right = F)) %>%
    group_by(AgeGroup, sex) %>%
    summarize(SynthPop = n()) %>%
    ungroup() %>%
    mutate(AgeGroup = str_replace_all(AgeGroup, "_","-"),
           sex = ifelse(sex == "2", "SynthFemale", "SynthMale")) %>%
    spread(key = sex, value = SynthPop)
    
pop_data = pop_data %>% left_join(synth_pop_df, by = c("AgeGroup" = "AgeGroup"))


jpeg('../figures/report_figure_validation_demography.jpeg', width=7,height=5, units="in", res = 300)
layout(
    matrix(1:3,1,3, byrow = T), widths = c(rep(4,2),6), heights = rep(1,1)
)
par(mar = c(0.5,0.0,1.1,0.0), oma = c(3,3.0,5.5,2.5))

bb = barplot(height = -pop_data$Female, horiz = T, col = "red", xlim = c(-max(pop_data$Female)*1.4,0), xaxt = "n")
axis(1, at = seq(from= 0, by = -100000, length.out = 5), labels =  seq(from= 0, by = -100000, length.out = 5), cex.axis = 1.0)
points( -pop_data$SynthFemale, bb, pch = 18)

text(cex=1.0, x=-max(pop_data$Female)*1.1, y=bb,pop_data$AgeGroup , xpd=TRUE)

barplot(height = pop_data$Male, horiz = T, col = "blue", xlim = c(0,max(pop_data$Male)*1.4), xaxt = "n")
axis(1, at = seq(from= 0, by = 100000, length.out = 5), labels =  str_replace(as.character(seq(from= 0, by = 100000, length.out = 5)), "^0", ""))
points(pop_data$SynthMale, bb, pch = 18)
mtext(sprintf("Population Bogotá"),
      side = 3, line = 0, outer = T, cex = 1.7)
##plot(0,0, lwd = 0,axes = F)
legend('top',legend=c("F","M"), col = c("red", "blue"), pch = 15, cex = 1)

## Household composition
##par(mar = c(4.5,0.0,1.1,1.0), oma = c(3,3.0,5.5,2.5))
house_df = household_comp %>% group_by(PersonsHousehold) %>% summarize(NumHouses = sum(NumHouses)) %>%
    full_join(data.frame(PersonsHousehold = 1:max(household_comp$PersonsHousehold, NumHouses = 0)), by = 'PersonsHousehold') %>%
    replace_na(list(NumHouses = 0)) %>%
    arrange(PersonsHousehold)

par(mar = c(0.5,1.5,1.1,1.5))
bb = barplot(height = house_df$NumHouses, horiz = F, col = "gray")
axis(1, at = bb, labels = house_df$PersonsHousehold )

synth_houses_df = synth_houses %>% group_by(hh_size) %>%
    summarize(SynthHouses = n()) %>%
    ungroup() %>%
    mutate(PersonsHousehold = as.numeric(hh_size)) %>%
    full_join(data.frame(PersonsHousehold = 1:max(household_comp$PersonsHousehold, NumHouses = 0)), by = 'PersonsHousehold') %>%
    replace_na(list(NumHouses = 0)) %>%
    arrange(PersonsHousehold)

points(bb, synth_houses_df$SynthHouses, pch = 18)

mtext(text = "Number of houses", side = 2, line = 2)
mtext(text = "Persons per house", side = 1, line = 2)

dev.off()

##===============================================================#
## 2. Population by catastro-----------
##===============================================================#
synth_pop$Zone = substr(synth_pop$stcotrbg, 6, 13)

jpeg('../figures/report_figure_esc_popualtion.jpeg', width=7,height=6, units="in", res = 300)
layout(
    matrix(1:2,1,2, byrow = T), widths = c(1,1), heights = rep(1,1)
)

## Total population
zone_pop = synth_pop %>% group_by(Zone) %>% summarize(Pop = n()) %>%ungroup()
pop_shp = esc_shp
pop_shp@data = left_join(pop_shp@data, zone_pop, by = c("SCACODIGO"= "Zone"))

my_colors = brewer.pal(6,"YlGnBu")
my_colors = colorRampPalette(my_colors)(6)
pop_brk <- cut(pop_shp@data$Pop, breaks = c(0,1000,2000,5000,10000,50000,10000000))
my_colors = my_colors[as.numeric(pop_brk)]

plot(upz_shp[upz_shp@data$UPlTipo == 1,], lwd = 0.001)
plot(pop_shp, col = my_colors, bg = "gray", lwd = 0.1, add = T)
plot(localidad_shp, add = T, lwd = 0.5)
##plot(block_shp, add = T, lwd = 0.1)
mtext("Synth. population by Unidad catastral", side = 3)

## Population > 60
zone_pop_old = synth_pop %>% filter(as.numeric(age) >= 60) %>%
    group_by(Zone) %>% summarize(Pop60 = n()) %>%
    ungroup() %>%
    right_join(zone_pop) %>%
    mutate(Prop60 = Pop60 / Pop * 100)

pop_shp = esc_shp
pop_shp@data = left_join(pop_shp@data, zone_pop_old, by = c("SCACODIGO"= "Zone"))

my_colors = brewer.pal(5,"YlGnBu")
##my_colors = colorRampPalette(my_colors)(6)
pop_brk <- cut(pop_shp@data$Prop60, breaks = c(0,10,15,20,25,100))
my_colors = my_colors[as.numeric(pop_brk)]

plot(upz_shp[upz_shp@data$UPlTipo == 1,], lwd = 0.001)
plot(pop_shp, col = my_colors, bg = "gray", lwd = 0.1, add = T)
plot(localidad_shp, add = T, lwd = 0.5)
##plot(block_shp, add = T, lwd = 0.1)

mtext("Synth. population proportion 60+", side = 3)
dev.off()

##===============================================================#
## 3. Houses exact location-----------
##===============================================================#
house_coor = coordinates(synth_houses %>%
                         dplyr::select(latitude, longitude) %>%
                         mutate(latitude = as.numeric(latitude),longitude = as.numeric(longitude))) 
colnames(house_coor) = c("latitude", "longitude")
house_coor = as.data.frame(house_coor)
coordinates(house_coor) = ~  longitude + latitude
proj4string(house_coor) = proj4string(upz_shp)

schools_coor = coordinates(schools_df %>%
                         dplyr::select(latitude, longitude) %>%
                         mutate(latitude = as.numeric(latitude),longitude = as.numeric(longitude))) 
colnames(schools_coor) = c("latitude", "longitude")
schools_coor = as.data.frame(schools_coor)
coordinates(schools_coor) = ~  longitude + latitude
proj4string(schools_coor) = proj4string(upz_shp) 

workplace_coor = coordinates(workplaces_df %>%
                         dplyr::select(latitude, longitude) %>%
                         mutate(latitude = as.numeric(latitude),longitude = as.numeric(longitude))) 
colnames(workplace_coor) = c("latitude", "longitude")
workplace_coor = as.data.frame(workplace_coor)
coordinates(workplace_coor) = ~  longitude + latitude
proj4string(workplace_coor) = proj4string(upz_shp)

jpeg('../figures/report_figure_house_location.jpeg', width=7,height=6, units="in", res = 300)
layout(
    matrix(1:2,1,2, byrow = T), widths = c(1,1), heights = rep(1,1)
)

plot(upz_shp[upz_shp@data$UPlTipo == 1,], lwd = 0.001)

plot(house_coor, col = "#bdbdbd10", pch = 15, cex = 0.05, add = T)
plot(schools_coor, col = "#1b9e7780", pch = 18, cex = 0.3, add = T)
plot(localidad_shp, add = T, lwd = 0.5)

mtext("Houses and schools", side = 3)

plot(upz_shp[upz_shp@data$UPlTipo == 1,], lwd = 0.001)

plot(house_coor, col = "#bdbdbd10", pch = 15, cex = 0.05, add = T)
plot(workplace_coor, col = "#7570b320", pch = 16, cex = 0.1, add = T)
plot(localidad_shp, add = T, lwd = 0.5)

mtext("Houses  and workplaces", side = 3)
dev.off()

##===============================================================#
## 4. School mobility-----------
##===============================================================#
unidad_catastral = read_csv('../data/processed_data/geodata/Localidad_Unidad_Catastral.csv')
localidad_names = data.frame(Localidad = as.numeric(localidad_shp@data$Identificad),
                             LocalidadName = tolower(as.character(localidad_shp@data$Nombre_de_l)),
                             stringsAsFactors = F) %>%
    arrange(Localidad)
school_mov = left_join(schools_df, unidad_catastral, by = c("zipcode" = "SCACODIGO")) %>%
    dplyr::select(sp_id, Localidad, zipcode) %>%
    rename(SchoolLocalidad = Localidad)

students_df = synth_pop %>% filter(!is.na(sp_school_id)) %>%
    dplyr::left_join(unidad_catastral, by = c("Zone" = "SCACODIGO")) %>%
    dplyr::select(sp_id, Zone, sp_school_id, Localidad)  %>%
    mutate(sp_school_id = as.numeric(sp_school_id)) %>%
    group_by(sp_school_id, Localidad) %>% summarize(Students = n()) %>% ungroup() %>%
    left_join(school_mov, by = c("sp_school_id" = "sp_id")) %>%
    group_by(Localidad, SchoolLocalidad) %>%
    summarize(Students = sum(Students)) %>%
    ungroup() %>%
    drop_na() %>%
    arrange(Localidad, SchoolLocalidad) %>%
    group_by(Localidad) %>% mutate(StudentsProp = Students / sum(Students)) %>%
    ungroup() %>%
    right_join(expand.grid(Localidad = 1:19,SchoolLocalidad = 1:19, stringsAsFactors = F)) %>%
    replace_na(list(Students = 0, StudentsProp = 0))


jpeg('../figures/report_figure_school_commute.jpeg', width=5,height=5, units="in", res = 300)
my_colors = brewer.pal(5,"Greys")
my_breaks = c(0,0.05,0.1,0.2,0.5,1.0)


mov_matrix = matrix(data = students_df$StudentsProp, nrow = 19, ncol = 19, byrow = T)
image(x = 1:19, y = 1:19, z = mov_matrix, breaks = my_breaks, col = my_colors,
      ylab = "", xlab = "",
      xaxt = 'n', yaxt = "n")
axis(1, at = 1:19, labels = localidad_names$LocalidadName[1:19], las = 2, cex.axis = 0.5)
axis(2, at = 1:19, labels = localidad_names$LocalidadName[1:19], las = 2, cex.axis = 0.5)
mtext("School commute", side = 3)
dev.off()

##===============================================================#
## 4. workplace mobility-----------
##===============================================================#
workplace_geo = read_csv('../data/processed_data/workplacedata/workplace_bogota_data.csv') %>%
    rename(WorkLocalidad = Localidad) 

workers_df = synth_pop %>% filter(!is.na(sp_work_id)) %>%
    dplyr::left_join(unidad_catastral, by = c("Zone" = "SCACODIGO")) %>%
    dplyr::select(sp_id, Zone, sp_work_id, Localidad)  %>%
    mutate(sp_work_id = as.numeric(sp_work_id)) %>%
    group_by(sp_work_id, Localidad) %>% summarize(Workers = n()) %>% ungroup() %>%
    left_join(workplace_geo, by = c("sp_work_id" = "workplace_id")) %>%
    group_by(Localidad, WorkLocalidad) %>%
    summarize(Workers = sum(Workers)) %>%
    ungroup() %>%
    drop_na() %>%
    group_by(Localidad) %>% mutate(WorkersProp = Workers / sum(Workers)) %>%
    ungroup() %>%
    right_join(expand.grid(Localidad = 1:19,WorkLocalidad = 1:19, stringsAsFactors = F)) %>%
    replace_na(list(Workers = 0, WorkersProp = 0)) %>%
    arrange(Localidad, WorkLocalidad) 


jpeg('../figures/report_figure_work_commute.jpeg', width=5,height=5, units="in", res = 300)
my_colors = brewer.pal(6,"Greys")
my_breaks = c(0,0.05,0.1,0.2,0.5,0.8,1.0)


mov_matrix = matrix(data = workers_df$WorkersProp, nrow = 19, ncol = 19, byrow = T)
image(x = 1:19, y = 1:19, z = mov_matrix, breaks = my_breaks, col = my_colors,
      ylab = "", xlab = "",
      xaxt = 'n', yaxt = "n")
axis(1, at = 1:19, labels = localidad_names$LocalidadName[1:19], las = 2, cex.axis = 0.5)
axis(2, at = 1:19, labels = localidad_names$LocalidadName[1:19], las = 2, cex.axis = 0.5)
mtext(text = "Work commute", side = 3)
dev.off()

##===============================================================#
## 4.1 Bogota input mobility-----------
##===============================================================#
mov_df = read_csv('../data/processed_data/workplacedata/mobility_matrix_bogota_data.csv')  %>%
    filter(OrigenLocalidad != 20, DestinoLocalidad != 20) %>%
        arrange(OrigenLocalidad, DestinoLocalidad)
 

jpeg('../figures/report_figure_bogota_commute_input_mobility_matrix.jpeg', width=5,height=5, units="in", res = 300)
my_colors = brewer.pal(6,"Greys")
my_breaks = c(0,0.01,0.02,0.03,0.04,0.05,0.1)


mov_orig_matrix = matrix(data = mov_df$Trips, nrow = 19, ncol = 19, byrow = T)
image(x = 1:19, y = 1:19, z = mov_orig_matrix, breaks = my_breaks, col = my_colors,
      ylab = "", xlab = "",
      xaxt = 'n', yaxt = "n")
axis(1, at = 1:19, labels = localidad_names$LocalidadName[1:19], las = 2, cex.axis = 0.5)
axis(2, at = 1:19, labels = localidad_names$LocalidadName[1:19], las = 2, cex.axis = 0.5)
mtext(text = "Mobility matrix for Bogota", side = 3)
dev.off()

##===============================================================#
## 2. Demographics of workers-----------
##===============================================================#
brks = c(0,15,25,35,45,55,65,120)
lbls = sprintf("%d-%d",brks[1:(length(brks) - 1)], brks[2:length(brks)] - 1)
lbls[length(lbls)] = sprintf("%d-above", brks[length(brks) -1])

synth_pop_df = synth_pop %>%
    mutate(AgeGroup = as.character(cut(as.numeric(age), breaks =brks, labels = lbls, include.lowest = T,right = F))) %>%
    group_by(AgeGroup) %>%
    summarize(SynthPop = n()) %>%
    ungroup()

synth_workers = synth_pop %>% dplyr::filter(!is.na(sp_work_id))  %>%
    mutate(AgeGroup = as.character(cut(as.numeric(age), breaks = brks, labels = lbls, include.lowest = T, right = F))) %>%
    group_by(AgeGroup) %>%
    summarize(TotalWorkers = n()) %>%
    ungroup() %>%
    left_join(synth_pop_df, by = "AgeGroup") %>%
    mutate(PropWorkers = (TotalWorkers / sum(TotalWorkers) * 100))

census_workers = read_csv('../data/raw_data/microdata/11Bogota/CNPV2018_5PER_A2_11.CSV') %>%
    mutate(AgeGroup = as.character(cut(as.numeric(P_EDADR)*5 - 1, breaks = brks, labels = lbls, include.lowest = T, right = F))) %>%
    group_by(AgeGroup, P_TRABAJO) %>% summarize(N = n()) %>%
    ungroup() %>%
    filter(P_TRABAJO == 1 | P_TRABAJO == 2) %>%
    group_by(AgeGroup) %>%
    summarize(N = sum(N))%>%
    ungroup() %>%
    mutate(PropWorkers = (N / sum(N)) * 100)

jpeg('../figures/report_figure_validation_workers_proportion.jpeg', width=7,height=5, units="in", res = 300)
tmp_bar = barplot(census_workers$PropWorkers, names.arg = census_workers$AgeGroup, ylim = c(0,40),
                  ylab = "Proportion of total workers")
points(tmp_bar, synth_workers$PropWorkers, col = "blue", pch = 18)
legend('topright', legend = c('Synthetic pop.', 'Census'), col = c("blue", 'gray'), lwd = c(0,2), pch = c(18,-1))
dev.off()


a = synth_pop %>% dplyr::select(sp_id, sp_hh_id, age) %>%
    left_join(
        synth_houses %>% dplyr::select(sp_id, hh_size), by = c('sp_hh_id' = 'sp_id')) %>%
    mutate(AgeGroup = as.character(cut(as.numeric(age), breaks = brks, labels = lbls, include.lowest = T, right = F))) %>%
    group_by(AgeGroup) %>%
    summarize(hh_size = mean(as.numeric(hh_size))) %>%
    ungroup()


