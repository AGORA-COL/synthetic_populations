##===============================================================#
## Filter micro data
## Author: Guido España
## Date: 2018/10/03
##===============================================================#
## Set up--------
##===============================================================#
library(tidyverse)

setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')
##===============================================================#
## Read and filter micro-data Colombia--------
##===============================================================#
microdata_file = '../data/raw_data_/microdata/ipumsi_colombia.csv'
country_code = 170
adm_codes = c(63001,11001,5001,27001,76001,8002)
divipola_codes = c(170063001, 170011001, 170005001,170066001, 170076001, 170013001)
country_microdata = read_csv(microdata_file)
for(n in 1:length(adm_codes)){
    am = adm_codes[n]
    cat(sprintf("filtering code %d\n",am))
    adm_code = as.numeric(sprintf('%03d%06d',country_code,am)) #adm_code refers to IPUMS code!!
  
    micro_data = country_microdata %>% filter(GEOLEV2 == adm_code) %>%
        dplyr::select(c(COUNTRY,SERIAL,HHWT,GEOLEV1,GEOLEV2,
                 GQ,URBAN,OWNERSHIP,HHTYPE,NFAMS, PERNUM,
                 AGE,SEX,SCHOOL,EMPSTAT,EMPSTATD,RELATE))
    
    write.csv(x = micro_data,
              file=sprintf('../data/processed_data/microdata/colombia_microdata_%d.csv',divipola_codes[n]), row.names = FALSE)
}