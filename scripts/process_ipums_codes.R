##===============================================================#
## Setup--------
##===============================================================#
library(tidyverse)
##===============================================================#
## Read colombian codes--------
##===============================================================#
colombia_codes = read_csv('data/raw_data/admindata/Colombia_Adm2_Codes.csv',
                          col_names = c('Mun', 'Dept'))

divipola_names = read_delim('data/raw_data/microdata/divipola_names.txt', delim = ":",
                            col_names = c('DPTO_CODE', "MUN_CODE", 'DPTO','MUN')) %>%
    mutate(MUN_CODE = as.numeric(MUN_CODE))

ipums_rel = read_delim('data/raw_data/microdata/IpumsDivipola.txt',
                       delim = ' ', col_names = c('ipums', 'divipola')) %>%
    left_join(divipola_names, by = c('divipola' = 'MUN_CODE'))

ipums_names = read_delim('data/raw_data/microdata/Ipums_divipola_geo2.csv',delim=':')

ipums_bloc = read_delim('data/raw_data/microdata/Ipums_names_bloc.csv',delim=':') %>%
    mutate(BPLCO2 = as.numeric(BPLCO2))


ipums_processed = ipums_rel %>% left_join(left_join(ipums_bloc, ipums_names, by = "NAME"), by = c('ipums' = "BPLCO2"))


write_delim(ipums_processed, path="data/processed_data/microdata/ipums_relation_processed.csv", delim = ":",)
