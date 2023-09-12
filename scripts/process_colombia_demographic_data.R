##=======================================#
## Author: Guido España
## Process demographic data for Colombia
## Date: 2018/10/03
##=======================================#
## Libraries and data files--------
##=======================================#
library(tidyverse)
setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')

datadir = '../data/raw_data/popdata'
popfile = file.path(datadir,'Edades_Simples_1985-2020.xls')

##=======================================#
## Make the data long--------
##=======================================#
# read the overall headers
pop_year =  readxl::read_xls(popfile, sheet="Total",skip=9,n_max=1)
pop_year = as.numeric(as.tibble(str_extract(colnames(pop_year),'[0-9]{4}') ) %>% drop_na() %>% pull())
pop_colnames = colnames(readxl::read_xls(popfile, sheet="Total",skip=10,n_max=1))[-1]

pop_data = tibble()

# Go in 19-line blocks and bind the data at the end
c_row = 11
# c_row = 16826
repeat{
  code_adm = colnames(readxl::read_xls(popfile, sheet="Total",skip=c_row,n_max=1))[1]
  cat("\r",code_adm)
  #The file ends with Fuente: DANE, so that's my flag to break
  if(str_detect(code_adm,'Fuente.*DANE') == TRUE){break}
  pop_data_tmp = readxl::read_xls(popfile, sheet="Total",skip=c_row+1,n_max=18, col_names =pop_colnames) %>%
    rename(AgeGroup=`Grupos de edad`) %>%
    gather(key = Gender, value = Pop, -AgeGroup) %>%
    separate(Gender, c("Gender", "Year")) %>% replace(is.na(.),0) %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Year = pop_year[Year+1]) %>%
    mutate(Code = code_adm) %>%
    mutate(Gender = str_replace_all(Gender,"Mujeres","Female")) %>%
    mutate(Gender = str_replace_all(Gender,"Hombres","Male")) %>%
    mutate( AgeGroup = str_replace_all(AgeGroup," Y.*M.*S","-above"))
  
  pop_data = pop_data %>% bind_rows(pop_data_tmp)
  c_row = c_row + 19
}

pop_data = pop_data %>% 
  filter(AgeGroup != 'Total', Gender != 'Total') %>%
  separate(col=AgeGroup, into=c("MinAge","MaxAge")) %>%
  mutate(MaxAge = str_replace_all(MaxAge,"above",'200')) %>%
  mutate(MinAge = as.integer(MinAge), MaxAge = as.integer(MaxAge))

write.csv(x = pop_data,file='../data/processed_data/popdata/colombia_population_data.csv',row.names = FALSE)
