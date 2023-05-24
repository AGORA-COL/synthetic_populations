##=======================================#
## Author: Guido España
## Process demographic data for Colombia
## Date: 2018/10/03
##=======================================#
## Libraries and data files--------
##=======================================#
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(RCurl)
library(data.table)
options(digits = 20,scipen = 999)


schools_file = '../output/formatted_populations/colombia_11001/colombia_11001_schools.txt'
people_file = '../output/formatted_populations/colombia_11001/colombia_11001_synth_people.txt'

ies_file = '../data/processed_data/schooldata/IES_Bogota_Geo_ESC.csv'

schools_df = read_csv(schools_file)
people_df = read_csv(people_file)
ies_df = read_csv(ies_file)

##=======================================#
## assign students to IES--------
##=======================================#
