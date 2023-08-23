##=======================================#
## Author: Guido España
## Process demographic data for Colombia
## Date: 2018/10/03
##=======================================#
## Libraries and data files--------
##=======================================#
library(tidyverse)

setwd("scripts")

datadir = '../data/raw_data/popdata'
popfile = file.path(datadir, 'OSB_Demografia-PiramideBogotaLocalidades.csv')

##=======================================#
## Localidades--------
##=======================================#
## read the overall headers
fileConn<-file(popfile)
poplines = readLines( fileConn,encoding = "latin1")
close(fileConn)

for(nn in 1:length(poplines)){
    poplines[nn] = str_replace_all(poplines[nn], "\\.","")
}
pop_df = tibble()
for(nn in 24:length(poplines)){
    if(str_detect(poplines[nn], "Grupo.* de edad")){
        tmp_df =  read.delim(text = paste(poplines[nn:(nn+17)], collapse = "\n"), sep = ";") %>%
            dplyr::select(starts_with('Grupo'), Hombres.2, Mujeres.2,Total.2) %>%
            rename(Total = Total.2,
                   Female = Mujeres.2,
                   Male = Hombres.2,
                   AgeGroup = 'Grupo.de.edad') %>%
            mutate( AgeGroup = str_replace_all(AgeGroup," y.*m.*s","-above")) %>%
            gather(key = Gender, value = Pop, -AgeGroup)
        localidad_str = poplines[nn-1]
        tmp_df$Year = 2020
        tmp_df$Code = 11001
        tmp_df$Zone = str_match(localidad_str, "2005\\s+(.*?);")[2]
        pop_df = bind_rows(pop_df,tmp_df)
    }
}

pop_data = pop_df %>% 
    filter(AgeGroup != 'Total', Gender != 'Total') %>%
    mutate(AgeGroup = str_remove_all(AgeGroup, " ")) %>%
    dplyr::select(AgeGroup, Gender, Year, Pop, Code, Zone)

## mutate(MaxAge = str_replace_all(MaxAge,"above",'200')) %>%
## mutate(MinAge = as.integer(MinAge), MaxAge = as.integer(MaxAge))
## separate(col=AgeGroup, into=c("MinAge","MaxAge")) %>%
write.csv(x = pop_data,file='../data/processed_data/popdata/bogota_population_data.csv',row.names = FALSE)

##=======================================#
## Sector catastral--------
##=======================================#
popfile_sec = '../data/raw_data/popdata/Bogota_sdp.xlsx'

pop_household =  readxl::read_xlsx(popfile_sec, skip = 7, sheet = 2, n_max = 1088) %>%
    gather(key = PersonsHousehold, value = NumHouses, -c('Total', 'CODIGO_SEC')) %>%
    mutate(PersonsHousehold = as.numeric(PersonsHousehold), NumHouses = as.numeric(NumHouses)) %>%
    replace_na(list(NumHouses = 0)) %>%
    rename(Zone = CODIGO_SEC) %>%
    mutate(Year = 2018, Code = 11001) %>%
    dplyr::select(Code, Zone, NumHouses, PersonsHousehold, Year)

pop_age =  readxl::read_xlsx(popfile_sec, skip = 7, sheet = 5, n_max = 1088) %>%
    gather(key = AgeGroup, value = Pop, -CODIGO_SEC) %>%
    rename(Zone = CODIGO_SEC) %>%
    arrange(Zone) %>%
    mutate(Pop=  as.numeric(Pop)) %>%
    replace_na(list(Pop = 0)) %>%
    mutate(AgeGroup = str_replace(AgeGroup, "de ([0-9]+?) .* ([0-9]+).*", "\\1-\\2")) %>%
    mutate(AgeGroup = str_replace(AgeGroup, "de 100.*", "100-120")) %>%
    separate(AgeGroup, into = c("MinAge", "MaxAge")) %>%
    mutate(MinAge = as.numeric(MinAge), MaxAge = as.numeric(MaxAge)) %>%
    mutate(AgeGroup = sprintf("%d-%d", MinAge,MaxAge)) %>%
    mutate(AgeGroup = str_replace(AgeGroup, "100-120", "100-above")) %>%
    mutate(Year = 2018, Code = 11001, Gender = "Female")

vec_gender = 1:nrow(pop_age) %% 42
vec_gender[vec_gender == 0] = 42
pop_age$Gender[vec_gender <= 21] = "Male"

pop_age = pop_age %>%
    dplyr::select(AgeGroup, Gender, Year, Pop, Code, Zone)

write.csv(x = pop_age,file='../data/processed_data/popdata/bogota_population_data_sec.csv',row.names = FALSE)
write.csv(x = pop_household,file='../data/processed_data/popdata/bogota_household_composition_sec.csv',row.names = FALSE)

