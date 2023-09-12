##===============================================================#
## Filter Synthetic demography generator
## Author: Guido España
## Date: 2018/10/03
##===============================================================#
## Set up--------
##===============================================================#
setwd('/mnt/disco_aux/trace/apps/synthetic_populations/scripts')

library(tidyverse)
library(mipfp)
library(simPop)
library(surveysd)
source('./synthetic_demography_functions.R')

##===============================================================#
## Create a list with all the posible countries--------------
##===============================================================#
countries_metadata = rjson::fromJSON(
                                file = '../data/param_files/countries_latam_metadata.json',
                                simplify = F)
##===============================================================#
## Use the population sinthesizer--------
##===============================================================#
outputdir = "../output/synthesized_microdata"
if(!dir.exists(outputdir)){
    dir.create(outputdir)
}

run_all = FALSE
country = "colombia"

##==========================================================================#
## Run the pop synthesizer --------------------
##==========================================================================#
if(run_all == TRUE){
    country_names = names(countries_metadata)
}else{
    country_names = c(country)
}
processed_data_dir = file.path("..","data", "processed_data")

for(cc in country_names){
    country_data = countries_metadata[[cc]]
    for(rr in 1:length(country_data)){        
        city_data = country_data[[rr]]

        ## These names come from the preprocess data step
        city_data$microdata_file = file.path(
            processed_data_dir,  "microdata", 
            sprintf('%s_microdata_%03d%06d.csv',
                    cc,
                    unlist(city_data$country_code),
                    unlist(city_data$city_ipums_code))
        )
        
        city_data$agepop_file = file.path(
            processed_data_dir, "popdata",
            sprintf('%s_population_data.csv', cc)
        )
        
        print(sprintf("creating synthetic population for country %s code %d",
                      cc, unlist(city_data$city_code)))
        
        synth_data = synthesize_population(
            unlist(city_data$microdata_file),
            unlist(city_data$agepop_file),
            unlist(city_data$country_code),
            unlist(city_data$city_ipums_code),
            unlist(city_data$city_code),
            unlist(city_data$year_pop)
        )
        
        write_csv(synth_data$synth_people, 
                  sprintf("%s/synthetic_microdata_people_%s_%d.csv",
                          outputdir,
                          cc,
                          unlist(city_data$city_code))
                  )
        
        write_csv(synth_data$synth_houses, 
                  sprintf("%s/synthetic_microdata_%s_%d.csv",
                          outputdir,
                          cc,
                          unlist(city_data$city_code))
                  )
    }
}
