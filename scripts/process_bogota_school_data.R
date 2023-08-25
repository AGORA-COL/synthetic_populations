##=======================================#
## Author: Guido España
## Process demographic data for Colombia
## Date: 2018/10/03
##=======================================#
## Libraries and data files--------
##=======================================#
library(dplyr)
library(tidyverse)
library(sf)
library(raster)
#library(rgdal)
#library(maptools)
library(osmdata)
library(RColorBrewer)
library(osmplotr)
library(RCurl)
library(data.table)
options(digits = 20,scipen = 999)

setwd("scripts")

datadir = '../data/raw_data/popdata'
schools_sed = '../data/raw_data/schooldata/20200908 Información Modelo Epidemiologico.xlsx'                         
school_sedes_file = '../data/raw_data/schooldata/MATRÍCULA OFICIAL POR SEDE 31 DE AGOSTO 2020.xlsx'

#localities_shp = rgdal::readOGR('../data/raw_data/geodata/localidades_bogota/poligonos-localidades.shp')
localities_shp = sf::st_read('../data/raw_data/geodata/localidades_bogota/poligonos-localidades.shp')
#esc_shp = rgdal::readOGR('../data/raw_data/geodata/scat_shp/scat_shp.shp')
esc_shp = sf::st_read('../data/raw_data/geodata/scat_shp/scat_shp.shp')
esc_shp <- st_make_valid(esc_shp)
unidad_catastral = read_csv('../data/processed_data/geodata/Localidad_Unidad_Catastral.csv')
unidad_catastral$Localidad<-as.numeric(unidad_catastral$Localidad)
localidad_list_df = read_csv('../data/raw_data/geodata/Bogota_localidades_ID.csv')

##=======================================#
## Read IPM data--------
##=======================================#
## ipm_data = readxl::read_xlsx('../data/raw_data/schooldata/IPM_EB_InstitucionEducativa.xlsx', sheet = 'IPM_Institución', skip = 1) %>%
##     dplyr::select(CODIGO_DANE_Texto, TOTAL_ALUMNOS, INCIDENCIA_AJUSTADA) %>%
##     mutate(Income = findInterval(INCIDENCIA_AJUSTADA, as.numeric(quantile(INCIDENCIA_AJUSTADA, c(0,0.25,0.5,0.75,1))))) %>%
##     mutate(Income = ifelse(Income > 4,  4, Income),
##            SchoolCode = as.numeric(CODIGO_DANE_Texto)) %>%
##     mutate(Income = 5 - Income)

ipm_data_base = readxl::read_xlsx('../data/raw_data/schooldata/IPM_EB_InstitucionEducativa.xlsx', sheet = 'IPM_Institución', skip = 1) %>%
    dplyr::select(CODIGO_DANE_Texto, TOTAL_ALUMNOS, INCIDENCIA_AJUSTADA)
ipm_data = readxl::read_xlsx('../data/raw_data/schooldata/IPM_EB_InstitucionEducativa.xlsx', sheet = 'IPM_Institución', skip = 1) %>%
    dplyr::select(CODIGO_DANE_Texto, TOTAL_ALUMNOS, INCIDENCIA_AJUSTADA) %>%
    mutate(Income = findInterval(INCIDENCIA_AJUSTADA,
                                 as.numeric(quantile(rep(ipm_data_base$INCIDENCIA_AJUSTADA, ipm_data_base$TOTAL_ALUMNOS), c(0,0.25,0.5,0.75,1)))))%>%
    mutate(Income = ifelse(Income > 4,  4, Income),
           SchoolCode = as.numeric(CODIGO_DANE_Texto)) %>%
    mutate(Income = 5 - Income)
    
##=======================================#
## Process school data--------
##=======================================#
grades_age = read_csv('../data/raw_data/schooldata/conversion_grados_edad.csv') %>%
    mutate(Grade = tolower(iconv(Grade,from="UTF-8",to="ASCII//TRANSLIT")))

school_data = readxl::read_xlsx(schools_sed, skip = 1, sheet = 'P1.1', n_max = 422) %>%
    dplyr::select(-TOTAL)
    
school_location = readxl::read_xlsx(schools_sed, skip = 5, sheet = 'P2', n_max = 755)  %>%
    rename(SchoolGroupCode = "Código DANE del establecimiento",
           SchoolGroupName = "Nombre Establecimiento Educativo",
           SchoolCode = "Código DANE de la Sede",
           SchoolName = "Nombre Sede Educativa",
           NumberLocalidad = "Número de la localidad",
           NameLocalidad = "Localidad",
           SchoolType = "Clase de Colegio",
           SchoolAddress = "Dirección de la sede") %>%
    mutate(SchoolName = str_replace_all(SchoolName, ",", ""),
           SchoolGroupName = str_replace_all(SchoolGroupName, ",", "")) %>%
    left_join(ipm_data, by = c('SchoolGroupCode' = 'SchoolCode')) %>%
    mutate(Income = ifelse(is.na(Income), median(Income, na.rm = T), Income))

## IT SEEMS LIKE THESE SPREADSHEETS ARE SORTED!!
school_location_df = readxl::read_xlsx(school_sedes_file, skip = 1, sheet = 'Hoja1', n_max = 755) %>%
        rename(SchoolGroupName = "NOMBRE ESTABLECIMIENTO EDUCATIVO",
           SchoolName = "NOMBRE SEDE EDUCATIVA",
           NumberLocalidad = "NUMERO LOCALIDAD",
           NameLocalidad = "NOMBRE LOCALIDAD") %>%
    mutate(SchoolName = str_replace_all(SchoolName, ",", "")) %>%
    dplyr::select(-NumberLocalidad, -NameLocalidad, -SchoolGroupName, -SchoolName, -Total) %>%
    bind_cols(school_location[,'SchoolCode']) %>%
    gather(key = Grade, value = Students, -SchoolCode) %>%
    drop_na() %>%
    mutate(Grade = str_replace_all(Grade,'( -)','')) %>%
    mutate(Grade = tolower(iconv(Grade,from="UTF-8",to="ASCII//TRANSLIT"))) %>%
    left_join(school_location, by = 'SchoolCode') %>%
    mutate(Grade = trimws(str_replace_all(Grade, "(\\(.*\\))", ""))) %>%
    left_join(grades_age, by = "Grade") %>%
    mutate(NameLocalidad = tolower(NameLocalidad)) %>%
    filter(NumberLocalidad != 20, Students > 0) %>%
    rename(latitude = Latitud, longitude = Longitud) %>%
    mutate(Zone = "")

##=======================================#
## Write school data--------
##=======================================#
localidad_list = unique(school_location_df$NumberLocalidad)
school_location_loc = tibble()
#for(ll in 1:length(localidad_list)){
#    ind_coors = which(school_location_df$NumberLocalidad == localidad_list[ll])
#    loc_unidad_catastral = filter(unidad_catastral, Localidad == localidad_list[ll])
#    esc_loc_shp = esc_shp[esc_shp@data$SCACODIGO %in% loc_unidad_catastral$SCACODIGO,]
#    tmp_school_location = filter(school_location_df, NumberLocalidad == localidad_list[ll])
#    school_coor = coordinates(tmp_school_location %>% dplyr::select(latitude, longitude)) 
#    colnames(school_coor) = c("LAT", "LON")
#    school_coor = as.data.frame(school_coor)
#    coordinates(school_coor) = ~  LON + LAT
#    proj4string(school_coor) = proj4string(esc_shp)
    
#    for(ss in 1:nrow(esc_loc_shp)){
#        school_esc = sp::over(school_coor, esc_loc_shp[ss,])
#        if(length(which(!is.na(school_esc[,1]))) > 0){
#            tmp_school_location$Zone[which(!is.na(school_esc[,1]))] = as.character(esc_loc_shp@data$SCACODIGO[ss])
#        }
#    }
#    if(length(which(tmp_school_location$Zone == "")) > 0){
#        tmp_school_location$Zone[which(tmp_school_location$Zone == "")] = as.character(esc_loc_shp@data$SCACODIGO[as.numeric(apply(rgeos::gDistance(school_coor[which(tmp_school_location$Zone == "")], esc_loc_shp, byid = T), 2,which.min))])
#    }
#    school_location_loc = bind_rows(school_location_loc, tmp_school_location)
#}
for(ll in 1:length(localidad_list)){
  ind_coors = which(school_location_df$NumberLocalidad == localidad_list[ll])
  loc_unidad_catastral = filter(unidad_catastral, Localidad == localidad_list[ll])
  esc_loc_shp = esc_shp[esc_shp$SCACODIGO %in% loc_unidad_catastral$SCACODIGO,]
  tmp_school_location = filter(school_location_df, NumberLocalidad == localidad_list[ll])
  school_coor = st_as_sf(tmp_school_location, coords = c("longitude", "latitude"),crs = st_crs(esc_shp)) 
  
  for(ss in 1:nrow(esc_loc_shp)){
    school_esc = sf::st_join(school_coor, esc_loc_shp[ss,])
    if(length(which(!is.na(school_esc$SCACODIGO))) > 0){
      tmp_school_location$Zone[which(!is.na(school_esc$SCACODIGO))] = as.character(esc_loc_shp$SCACODIGO[ss])
    }
  }
  if(length(which(tmp_school_location$Zone == "")) > 0){
    tmp_school_location$Zone[which(tmp_school_location$Zone == "")] = as.character(esc_loc_shp$SCACODIGO[as.numeric(apply(sf::st_distance(school_coor[which(tmp_school_location$Zone == ""),], esc_loc_shp), 1,which.min))])
  }
  school_location_loc = bind_rows(school_location_loc, tmp_school_location)
}

#block_data$SCACODIGO[which(!is.na(block_esc[,1])[,1])] = as.character(esc_shp$SCACODIGO[ee])
#esc_data$upz[which(esc_data$upz == "")] = as.character(upz_shp$UPlCodigo[as.numeric(apply(sf::st_distance(esc_coor[which(esc_data$upz == ""),], upz_shp), 1,which.min))])
school_location_loc$SchoolType = "Public"
write_csv(school_location_loc, '../data/processed_data/schooldata/Schools_processed_capacity_bogota.csv')

##=======================================#
## Process students by locality--------
##=======================================#
## localidad_students =  readxl::read_xlsx(schools_sed, skip = 1, sheet = 'P1', n_max = 20)  %>%
##     dplyr::select(-TOTAL) %>%
##     gather(key = Grade, value =Students, -c("NUMERO LOCALIDAD", "NOMBRE LOCALIDAD")) %>%
##     replace_na(list(Students = 0)) %>%
##     mutate(Grade = trimws(str_replace_all(Grade, "(\\(.*\\))", ""))) %>%
##     mutate(Grade = tolower(iconv(Grade,from="UTF-8",to="ASCII//TRANSLIT"))) %>%
##     left_join(grades_age, by = "Grade") %>%
##     rename(NumberLocalidad = "NUMERO LOCALIDAD",
##            NameLocalidad = "NOMBRE LOCALIDAD") %>%
##     mutate(NameLocalidad = tolower(NameLocalidad))

localidad_students =  school_location_df %>%
    group_by(NumberLocalidad, NameLocalidad, Grade, MinAge, MaxAge) %>%
    summarize(Students = sum(Students)) %>%
    ungroup()

residence_students = readxl::read_xlsx(schools_sed, skip = 1, sheet = 'P3', n_max = 20) %>%
    dplyr::select(-TOTAL, -`SIN RESULTADO`) %>%
    gather(key = Residence, value =Students, -c("NUMERO LOCALIDAD", "NOMBRE LOCALIDAD")) %>%
    replace_na(list(Students = 0))  %>%
    rename(NumberLocalidad = "NUMERO LOCALIDAD",
           NameLocalidad = "NOMBRE LOCALIDAD") %>%
    mutate(NameLocalidad = tolower(NameLocalidad))

total_students = residence_students %>% group_by(NumberLocalidad) %>%
    summarize(LocalidadStudents = sum(Students)) %>%ungroup()

residence_students = residence_students %>% dplyr::select(-NameLocalidad) %>%
    left_join(total_students, by = "NumberLocalidad") %>%
    mutate(PropResidence = Students/LocalidadStudents)

localidad_students_residents = residence_students %>%
    dplyr::select(NumberLocalidad, PropResidence, Residence) %>%
    rename(ResidenceLocalidad = Residence) %>%
    right_join(localidad_students) %>%
    mutate(ResidentStudents  = PropResidence * Students) %>%
    filter(NumberLocalidad != 20) ## Exclude Sumapaz

    
    ## spread(key = Residence, value = PropResidence) %>%
    ## right_join(localidad_students) %>%
    ## gather(key = ResidenceLocalidad, value = ResidenceProp, -c("NumberLocalidad", "NameLocalidad", "Grade", "Students", "MinAge", "MaxAge")) %>%
    ## mutate(ResidentStudents = ResidenceProp * Students)%>%
    ##     filter(NumberLocalidad != 20) ## Exclude Sumapaz

write_csv(localidad_students_residents, '../data/processed_data/schooldata/Students_by_localidad.csv')    

##=======================================#
## Private school data--------
##=======================================#
schools_priv = '../data/raw_data/schooldata/Consulta Anexo5A 31032020.xlsx'

priv_school_data = readxl::read_xlsx(schools_priv, skip = 4, sheet = 'P1') %>%
    rename(SchoolCode = "Código DANE del establecimiento",
           SchoolName = "Nombre Establecimiento Educativo",
           SchoolType = "Clase de Colegio",
           Grade = "Grado",
           Age = "Edad del estudiante",
           NumberStudents = "Matrícula") %>%
    mutate(SchoolCode = as.character(SchoolCode)) %>%
    mutate(SchoolName = str_replace_all(SchoolName, ",", ""))

priv_school_location = readxl::read_xlsx(schools_priv, skip = 4, sheet = 'P2')  %>%
    rename(SchoolCode = "Código DANE del establecimiento",
           SchoolSedeCode = "Código DANE de la Sede",
           SchoolName = "Nombre Sede Educativa",
           NumberLocalidad = "Número de la localidad",
           NameLocalidad = "Localidad",
           SchoolType = "Clase de Colegio",
           SchoolAddress = "Dirección de la sede") %>%
    mutate(SchoolCode = as.character(SchoolCode)) %>%
    dplyr::select(-SchoolName, -SchoolType) %>%
    filter(SchoolCode %in% priv_school_data$SchoolCode) %>%
    left_join(dplyr::select(ipm_data,-SchoolCode), by = c('SchoolCode' = 'CODIGO_DANE_Texto')) %>%
    mutate(Income = ifelse(is.na(Income), median(Income, na.rm = T), Income))


priv_school_location_df = left_join(priv_school_data, priv_school_location, by = 'SchoolCode') %>%      
    mutate(Grade = trimws(str_replace_all(Grade, "(\\(.*\\))", ""))) %>%
    mutate(NameLocalidad = tolower(NameLocalidad)) %>%
    filter(NumberLocalidad != 20, NumberStudents > 0) %>%
    rename(latitude = Latitud, longitude = Longitud) %>%
    mutate(Zone = "", latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
    mutate(MinAge = ifelse(Age <= 19, Age, 20), MaxAge = ifelse(Age <= 19, Age, max(Age))) %>%
    group_by(SchoolCode, SchoolName, Localidad, SchoolAddress, latitude, longitude, SchoolType, Grade, MinAge, MaxAge, Income) %>%
    summarize(Students = sum(NumberStudents)) %>%
    ungroup() %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    rename(NumberLocalidad = Localidad)

##=======================================#
## PRIV. Add georef school data--------
##=======================================#
priv_school_unique = priv_school_location_df %>%
    group_by(SchoolCode, NumberLocalidad) %>%
    summarize(latitude = mean(latitude), longitude = mean(longitude)) %>%
    ungroup() %>%
    mutate(Zone = "")

## Add zone from ESC data
localidad_list = unique(school_location_df$NumberLocalidad)
priv_school_location_loc = tibble()
#for(ll in 1:length(localidad_list)){
#    ind_coors = which(priv_school_unique$NumberLocalidad == localidad_list[ll])
#    loc_unidad_catastral = filter(unidad_catastral, Localidad == localidad_list[ll])
#    esc_loc_shp = esc_shp[esc_shp@data$SCACODIGO %in% loc_unidad_catastral$SCACODIGO,]
#    tmp_school_location = filter(priv_school_unique, NumberLocalidad == localidad_list[ll])
#    school_coor = coordinates(tmp_school_location %>% dplyr::select(latitude, longitude)) 
#    colnames(school_coor) = c("LAT", "LON")
#    school_coor = as.data.frame(school_coor)
#    coordinates(school_coor) = ~  LON + LAT
#    proj4string(school_coor) = proj4string(esc_shp)
#    
#    for(ss in 1:nrow(esc_loc_shp)){
#        school_esc = sp::over(school_coor, esc_loc_shp[ss,])
#        if(length(which(!is.na(school_esc[,1]))) > 0){
#            tmp_school_location$Zone[which(!is.na(school_esc[,1]))] = as.character(esc_loc_shp@data$SCACODIGO[ss])
#        }
#    }
#    if(length(which(tmp_school_location$Zone == "")) > 0){
#        tmp_school_location$Zone[which(tmp_school_location$Zone == "")] = as.character(esc_loc_shp@data$SCACODIGO[as.numeric(apply(rgeos::gDistance(school_coor[which(tmp_school_location$Zone == "")], esc_loc_shp, byid = T), 2,which.min))])
#    }
#    priv_school_location_loc = bind_rows(priv_school_location_loc, tmp_school_location)
#}

for(ll in 1:length(localidad_list)){
    ind_coors = which(priv_school_unique$NumberLocalidad == localidad_list[ll])
    loc_unidad_catastral = filter(unidad_catastral, Localidad == localidad_list[ll])
    esc_loc_shp = esc_shp[esc_shp$SCACODIGO %in% loc_unidad_catastral$SCACODIGO,]
    tmp_school_location = filter(priv_school_unique, NumberLocalidad == localidad_list[ll])
    school_coor = st_as_sf(tmp_school_location, coords = c("longitude", "latitude"),crs = st_crs(esc_shp))
    
    for(ss in 1:nrow(esc_loc_shp)){
      school_esc = sf::st_join(school_coor, esc_loc_shp[ss,])
      if(length(which(!is.na(school_esc$SCACODIGO))) > 0){
        tmp_school_location$Zone[which(!is.na(school_esc$SCACODIGO))] = as.character(esc_loc_shp$SCACODIGO[ss])
      }
    }
    if(length(which(tmp_school_location$Zone == "")) > 0){
      tmp_school_location$Zone[which(tmp_school_location$Zone == "")] = as.character(esc_loc_shp$SCACODIGO[as.numeric(apply(sf::st_distance(school_coor[which(tmp_school_location$Zone == ""),], esc_loc_shp), 1,which.min))])
    }
    priv_school_location_loc = bind_rows(priv_school_location_loc, tmp_school_location)
}

priv_school_location_df_out = priv_school_location_df %>%
    left_join(priv_school_location_loc[,c('SchoolCode','Zone')], by = 'SchoolCode') %>%
    mutate(SchoolType = "Private")


write_csv(priv_school_location_df_out, '../data/processed_data/schooldata/Schools_processed_capacity_bogota_private.csv')

##=======================================#
## PRIV. SCHOOLS MOVILITY--------
##=======================================#
priv_school_localidad = readxl::read_xlsx('../data/raw_data/schooldata/EstudiantesLocalidadPrivados.xlsx',
                                          skip = 1) %>%
    gather(key = LocalidadSchool, value = PropResidenceStudents, -LocalidadResidencia) %>%
    group_by(LocalidadSchool) %>%
    mutate(PropResidenceStudents = PropResidenceStudents / sum(PropResidenceStudents)) %>%
    ungroup() %>%
    mutate(LocalidadSchool = as.numeric(LocalidadSchool)) %>%
    rename(ResidenceLocalidad = LocalidadResidencia)

priv_localidad_students = priv_school_location_df_out %>%
    mutate(AgeGroup = sprintf('%d-%d',MinAge, MaxAge)) %>%
    group_by(NumberLocalidad, AgeGroup) %>%
    summarize(Students = sum(Students)) %>%
    ungroup() %>%
    left_join(priv_school_localidad, by = c("NumberLocalidad" = "LocalidadSchool")) %>%
    mutate(ResidentStudents  = PropResidenceStudents * Students) %>%
    separate(AgeGroup, into = c("MinAge", "MaxAge"), remove = F) %>%
    rename(Grade = AgeGroup)

write_csv(priv_localidad_students, '../data/processed_data/schooldata/Students_by_localidad_private.csv')

##=======================================#
## COLLEGES--------
##=======================================#
ies_df = readxl::read_xlsx('../data/raw_data/schooldata/Matricula_IES_IETDH.xlsx', sheet = 'ESBOGOTA IES') %>%
    separate('# Localidad', into = c('Localidad_ID', 'Localidad_Name')) %>%
    mutate(Localidad_ID = as.numeric(Localidad_ID)) %>%
    rename(name = 'Nombre Institución',
           code = 'Código Institución SNIES',
           total_students = 'SNIES Matrícula - 2019') %>%
    dplyr::select(name, code, total_students, Localidad_ID, Localidad_Name) %>%
    mutate(Code_sede = 1)

sedes_df = ies_df %>% group_by(name) %>%
    summarize(sedes_students = sum(total_students, na.rm = T), sedes = n()) %>%    
    ungroup() %>%
    arrange(desc(sedes_students))
for(ss in 1:nrow(sedes_df)){
    if(sedes_df$sedes[ss] > 1){
        prob_school = rnorm(sedes_df$sedes[ss], mean = sedes_df$sedes[ss] / 2, sd = sedes_df$sedes[ss] / 6)
        if(min(prob_school) < 0){
            prob_school = prob_school + abs(min(prob_school))
        }
        if(min(prob_school) == 0){
            prob_school = prob_school + 1 / (3*sedes_df$sedes[ss])
        }
        prob_school = prob_school / sum(prob_school)
        list_of_students = as.numeric(table(sample.int(n = sedes_df$sedes[ss], size = sedes_df$sedes_students[ss], replace = T, prob = prob_school)))
        ies_df$total_students[ies_df$name == sedes_df$name[ss]] = list_of_students
        ies_df$Code_sede[ies_df$name == sedes_df$name[ss]] = 1:length(list_of_students)

    }else{
        ies_df$total_students[ies_df$name == sedes_df$name[ss]] = sedes_df$sedes_students[ss]
    }    
}


ies_df = ies_df %>%
    dplyr::filter(!is.na(total_students)) %>%
    mutate(Income = 0, Zone = "")

ies_df$Localidad_ID[which(is.na(ies_df$Localidad_ID))] = sample.int(19, length(which(is.na(ies_df$Localidad_ID))), replace = T)

## Assign lat,lon

#localities_data_shp = localities_shp@data %>% mutate(ROW = row_number())
localities_data_shp = localities_shp %>% mutate(ROW = row_number())
localities_data_shp$Localidad_ID = as.numeric(localities_data_shp$Identificad)
ies_df$LAT = 0
ies_df$LON = 0
#for(ll in 1:nrow(localities_data_shp)){
#    cat("\rLocalidad: ", ll)
#    localidad_tmp_shp = localities_shp[ll,]
#    proj4string(localidad_tmp_shp) = proj4string(localities_shp)
#    ies_loc_ind = which(ies_df$Localidad_ID == localities_data_shp$Localidad_ID[ll])
#    if(length(ies_loc_ind) == 0){next}
#    ## MAybe use tryCatch if too many errors with iter too small
#    ies_points = sp::spsample(localidad_tmp_shp, n = length(ies_loc_ind), "random",iter = 20)
#    ies_df$LAT[ies_loc_ind] = coordinates(ies_points)[,'y']
#    ies_df$LON[ies_loc_ind] = coordinates(ies_points)[,'x']               
#}   

for(ll in 1:nrow(localities_data_shp)){
  cat("\rLocalidad: ", ll)
  localidad_tmp_shp = localities_shp[ll,]
  localidad_tmp_shp <- st_set_crs(localidad_tmp_shp, st_crs(localities_shp))
  ies_loc_ind = which(ies_df$Localidad_ID == localities_data_shp$Localidad_ID[ll])
  if(length(ies_loc_ind) == 0){next}
  
  ## MAybe use tryCatch if too many errors with iter too small
  ies_points <- sf::st_sample(localidad_tmp_shp, size = length(ies_loc_ind), type = "random", iter = 20)
  ies_df$LAT[ies_loc_ind] <- st_coordinates(ies_points)[, "Y"]
  ies_df$LON[ies_loc_ind] <- st_coordinates(ies_points)[, "X"]
}   

## Assign an ESC code
#ies_coor = coordinates(ies_df %>% dplyr::select(LAT, LON)) 
#ies_coor = as.data.frame(ies_coor)
#coordinates(ies_coor) = ~  LON + LAT
#proj4string(ies_coor) = proj4string(esc_shp)
#crs(ies_coor) <- crs(esc_shp)
ies_coor = ies_df %>% dplyr::select(LAT, LON)
ies_coor <- st_as_sf(ies_coor, coords = c("LON", "LAT"), crs = st_crs(esc_shp))

#for(ss in 1:nrow(esc_shp)){
#    ies_esc = sp::over(ies_coor, esc_shp[ss,])
#    if(length(which(!is.na(ies_esc[,1]))) > 0){
#        ies_df$Zone[which(!is.na(ies_esc[,1]))] = as.character(esc_shp@data$SCACODIGO[ss])
#    }
#}

#if(length(which(ies_df$Zone == "")) > 0){
#    ies_df$Zone[which(ies_df$Zone == "")] = as.character(esc_shp@data$SCACODIGO[as.numeric(apply(rgeos::gDistance(ies_coor[which(ies_df$Zone == "")], esc_shp, byid = T), 2,which.min))])
#}
for(ss in 1:nrow(esc_shp)){
  ies_esc = sf::st_join(ies_coor, esc_shp[ss,])
  if(length(which(!is.na(ies_esc$SCACODIGO))) > 0){
    ies_df$Zone[which(!is.na(ies_esc$SCACODIGO))] = as.character(esc_shp$SCACODIGO[ss])
  }
}

if(length(which(ies_df$Zone == "")) > 0){
  ies_df$Zone[which(ies_df$Zone == "")] = as.character(esc_shp$SCACODIGO[as.numeric(apply(sf::st_distance(ies_coor[which(ies_df$Zone == "")], esc_shp), 1,which.min))])
}

ies_df = ies_df %>%
    mutate(mun_code = 11001, school_id = as.numeric(sprintf("11001%06d%02d", code, Code_sede)),
           prek = 0, kinder = 0, gr01_gr12 = 0, ungraded = total_students) %>%
    dplyr::select(mun_code, school_id, name, prek, kinder, gr01_gr12, ungraded, Zone, Income, LAT, LON)
write_csv(ies_df,
          '../data/processed_data/schooldata/IES_Bogota_Geo_ESC.csv')

##=======================================#
## COLLEGES--------
##=======================================#
## college_df = read_csv('../data/processed_data/schooldata/Universities_Bogota_Geo.csv')
## college_df$Zone = ""
## ## Not planning to use income for colleges, but maybe improve this assignment
## college_df$Income = 0
## college_coor = coordinates(college_df %>% dplyr::select(LAT, LON)) 
## colnames(college_coor) = c("LAT", "LON")
## college_coor = as.data.frame(college_coor)
## coordinates(college_coor) = ~  LON + LAT
## proj4string(college_coor) = proj4string(esc_shp)
    
## for(ss in 1:nrow(esc_shp)){
##     college_esc = sp::over(college_coor, esc_shp[ss,])
##     if(length(which(!is.na(college_esc[,1]))) > 0){
##         college_df$Zone[which(!is.na(college_esc[,1]))] = as.character(esc_shp@data$SCACODIGO[ss])
##     }
## }
## if(length(which(college_df$Zone == "")) > 0){
##     college_df$Zone[which(college_df$Zone == "")] = as.character(esc_shp@data$SCACODIGO[as.numeric(apply(rgeos::gDistance(college_coor[which(college_df$Zone == "")], esc_shp, byid = T), 2,which.min))])
## }

## write_csv(college_df,
##           '../data/processed_data/schooldata/Universities_Bogota_Geo_ESC.csv')
