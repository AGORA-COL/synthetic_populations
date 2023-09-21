##===============================================================#
## Filter Synthetic demography generator functions
## Author: Guido España
## Date: 2019/04/05
##===============================================================#
## IPU POP SYNTHESIS--------
##===============================================================#
#' synthesize_population()
#' 
#' This function creates a synthetic population using microdata
#' from IPUMS-International and using age and gender-stratified estimates of
#' population
#' @param microdata_file (character) path of the file with the IPUMS microdata for the region of interest
#' @param agepop_file (character) path of the file with the population estimates by gender and agegroups
#' @param country_code (numeric) 3 digit number for the country, e.g., 170 -> Colombia
#' @param adm_code (numeric) administrative code used in IPUMS for the region of interest
#' @param adm_census_code (numeric) administrative code used by the national government for statistical purposes
#' @param year_pop (numeric) year of the population estimates
#' @param fitHCons (boolean) (default = FALSE) should house constraints be included?
#' 
#' @return A list of dataframes for synthetic people and their synthetic houses
#' @export
#' @examples
#'
#' synth_data = synthesize_population(microdata_file = "colombia_microdata.csv",
#' agepop_file = "colombia_pop_2010.csv", country_code = 170, adm_code = 25005,
#' adm_census_code = 25307, year_pop = 2010)
#' 
synthesize_population = function(microdata_file, agepop_file, country_code,
                                 adm_code, adm_census_code, year_pop,
                                 school_file,university_file, workplace_file,
                                 people_file, house_file,
                                 fitHCons = FALSE, capPop = -1,
                                 subcity = FALSE, subcity_counter = 0,
                                 subcity_zone = "", subcity_total_pop = 0){
    ##browser()
    if(subcity == FALSE || subcity_counter == 0){
        unlink(people_file)
        unlink(house_file)
    }
    micro_data = read_csv(microdata_file) %>% mutate(HHWT = as.double(HHWT)) %>%
        drop_na()
    micro_data$AGE[micro_data$AGE > 99] = 99

    propCap = 1.0
    if(capPop > 100000){
        tmp_n = read_csv(agepop_file) %>%
            filter(Code == sprintf("%05d",adm_census_code), Year == year_pop, 
                   AgeGroup != 'Total', Gender != 'Total') %>%
            tally(Pop) %>% pull(n)
        if(tmp_n > capPop){
            propCap = capPop / tmp_n
        }
    }
    ## Read marginal distributions

    agepop_data = read_csv(agepop_file) %>% 
        filter(Year == year_pop)


    agepop_data = read_csv(agepop_file) %>% 
        filter(Code == sprintf("%05d",adm_census_code), Year == year_pop, 
               AgeGroup != 'Total', Gender != 'Total') %>%
        mutate(Gender = ifelse(Gender == "Male","m","f")) %>%
        unite(AGEGENDER,Gender,AgeGroup, sep = "") %>% 
        dplyr::select(AGEGENDER, Pop) %>%
        mutate(AGEGENDER = str_replace_all(AGEGENDER,"-","_")) %>%
        mutate(Pop = Pop * propCap)  

    breaks_df = filter(agepop_data, str_detect(AGEGENDER ,'f')) %>%
        transmute(AgeRange = substring(AGEGENDER,2),
                  Labels = substring(AGEGENDER,2)) %>%
        mutate(AgeRange = str_replace_all(AgeRange, "above","200")) %>%
        separate(AgeRange, into = c("MinAge","MaxAge"), convert = TRUE)
    
    ## Prepare constraints for the IPF algorithm
    ## Sex: Male = 1, Female = 2    
    brks = c(as.integer(breaks_df$MinAge),200)
    lbls = breaks_df$Labels
    cat(sprintf("breaks and labels created for %d-%d\n",country_code,adm_code))
    microdata_hhsize = micro_data %>% 
        group_by(SERIAL) %>% summarize(HHSIZE = n()) %>% ungroup()
    
    micro_data = micro_data %>%
        mutate(GENDER = ifelse(SEX == 1, "m","f")) %>%
        mutate(AGEGROUP = cut(micro_data$AGE,breaks = brks, labels = lbls, right = FALSE)) %>%
        unite(AGEGENDER, GENDER, AGEGROUP, sep = "", remove= FALSE) %>%
        left_join(microdata_hhsize, by = "SERIAL") %>%
        mutate(WORKSTATUS = ifelse(EMPSTAT == 1, "w", "nw"),
               SCHOOLSTATUS = ifelse(SCHOOL != 1, "ns",
                                     ifelse(AGE < 19, "bs", "cs")))

    cat(sprintf("crosstabulating constraints for %d-%d\n",country_code,adm_code))
    consP = xtabs(Pop ~ AGEGENDER, data = agepop_data)        

    microdata_df = tibble()
    
    ## Workers CONSTRAINTS
    workers_coverage = read_csv(workplace_file) %>%
        mutate(mun_code = codigo) %>%
        rename(Pop = poblacion, total_workplaces = "Unidades economicas total",
               total_workers = "Personas que trabajaron (Prom.) total") %>%
        mutate(total_workers = total_workers / Pop) %>%
        dplyr::select(mun_code, total_workers) %>% replace_na(list(total_workers = 0)) %>%
        filter(mun_code == city_code) 
    
    if(workers_coverage$total_workers == 0){
        ## at least 1% employed
        workers_coverage$total_workers = 0.01
    }

    workers_data = data.frame(w = floor(workers_coverage$total_workers * sum(agepop_data$Pop)),stringsAsFactors = F) %>%
        mutate(nw = sum(agepop_data$Pop) - w) %>%
        gather(key = WORKSTATUS, value = Pop)

    consW = xtabs(Pop ~ WORKSTATUS, data = workers_data)
    
    ## SCHOOL CONSTRAINTS
    ## 1. Assume people only go to school from 4 to 18
    school_coverage = read_csv(school_file) %>%
        rename(year = "AÑO", mun_code = "CÓDIGO_MUNICIPIO", coverage = "TASA_MATRICULACIÓN_5_16") %>%
        dplyr::select(year, mun_code, coverage) %>% filter(year == 2018, mun_code == city_code) %>%
        mutate(coverage = coverage / 100)
    
    university_coverage = readxl::read_xlsx(universities_file, skip = 5) %>%
        filter(Semestre == 1) %>%
        rename(mun_code = "Código del \r\nMunicipio\r\n(Programa)",
               name = "Institución de Educación Superior (IES)",
               capacity = "Matriculados 2018") %>%
        dplyr::select(mun_code, name, capacity) %>%
        mutate(mun_code = sprintf("%05d",mun_code)) %>%
        filter(mun_code == sprintf("%05d",as.numeric(city_code)))%>%
        group_by(mun_code, name) %>%
        summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
        filter(capacity > 10) %>% group_by(mun_code) %>%
        summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
        mutate(year = 2018, mun_code = as.numeric(mun_code))


    if(subcity == TRUE){
        if(year_pop == 2020){
            university_coverage$year = 2018
            school_coverage$year = 2020
        }
    }

    print(school_coverage)
    print(university_coverage)
    
    school_year_pop = read_csv(agepop_file) %>%
        filter(Code == sprintf("%05d",adm_census_code), Year == school_coverage$year, 
               AgeGroup != 'Total', Gender != 'Total') %>%
        tally(Pop) %>% pull(n)
    
    school_pop = filter(agepop_data, AGEGENDER %in% c("m5_9","f5_9","m10_14","f10_14")) %>%
        summarize(Pop = sum(Pop)) %>%
        bind_rows(filter(agepop_data,AGEGENDER %in% c("m0_4","f0_4")) %>% summarize(Pop = sum(Pop)/5)) %>%
        bind_rows(filter(agepop_data, AGEGENDER %in% c("m15_19","f15_19")) %>% summarize(Pop = sum(Pop)*4/5)) %>%
        summarize(Pop = sum(Pop)) %>% pull(Pop)
    
    school_coverage$coverage = floor(school_coverage$coverage * school_pop)
    if(subcity == TRUE){
        school_year_pop = subcity_total_pop
    }
    university_coverage$coverage = floor((university_coverage$capacity / school_year_pop)  * sum(agepop_data$Pop))
    
    students_data = data.frame(bs = school_coverage$coverage, cs = university_coverage$coverage, ns = sum(agepop_data$Pop) - school_coverage$coverage - university_coverage$coverage, stringsAsFactors = F) %>%
        gather(key = SCHOOLSTATUS, value = Pop)
    print(students_data)
    consS = xtabs(Pop ~ SCHOOLSTATUS, data = students_data)
        
    ## IPF Algorithm 
    setDT(micro_data)
    if(fitHCons == TRUE){
        HCons_df$HHSIZE = HCons_df$PersonsHousehold
        consH = list(xtabs(NumHouses ~ HHSIZE, data = HCons_df))
    }else{
        consH = NULL
    }


    cat(sprintf("calibrating weights for %d-%d\n",country_code,adm_code))
    calibrated_weights = surveysd::ipf(micro_data, hid = NULL, 
                              conP = list(consP, consW, consS),
                              conH = consH,
                              epsP = 1e-07,
                              epsH = 0.01,
                              w = "HHWT",
                              bound = NULL, 
                              verbose = TRUE, 
                              maxIter = 2000)
    
    micro_data$ADJHHWT = calibrated_weights$calibWeight
    micro_data = as_tibble(micro_data)
    
    total_pop = 0
    total_houses = 0

    final_pop = sum(agepop_data$Pop)
    print(sprintf("People to create: %d", final_pop))
    micro_data = micro_data %>% drop_na()
    if(nrow(micro_data) == 0){
        stop("Something went wrong when adjusting the weights, all outputs are NA")
    }
    while(total_pop < final_pop){      
        tmphouse = sample_n(micro_data, 1,replace = T, weight = micro_data$ADJHHWT)
        tmppeople = filter(micro_data,SERIAL == tmphouse$SERIAL)
        tmphouse$HHID = sprintf('%03d%06d%07d', country_code,adm_census_code,total_houses + 1)
        tmppeople$HHID = sprintf('%03d%06d%07d', country_code,adm_census_code,total_houses + 1)
        if(subcity == TRUE){
            tmphouse$ZONE = subcity_zone
            tmppeople$ZONE = subcity_zone
        }
        if(subcity == FALSE){
            if(total_pop == 0){
                write_csv(tmphouse,house_file,append = F)
                write_csv(tmppeople,people_file,append = F)
            }else{
                write_csv(tmphouse,house_file,append = T)
                write_csv(tmppeople,people_file,append = T)
            }
        }else{
            if(total_pop == 0 & subcity_counter == 0){
                write_csv(tmphouse,house_file,append = F)
                write_csv(tmppeople,people_file,append = F)
            }else{
                write_csv(tmphouse,house_file,append = T)
                write_csv(tmppeople,people_file,append = T)
            }
        }

        total_pop = total_pop + tmphouse$HHSIZE
        total_houses = total_houses + 1
        if(total_houses %% 1000 == 0){
            cat("\rSubcity counter: ",subcity_counter, " Houses created:",total_houses, " People created: ", total_pop, "final pop: ", final_pop)
        }
    }    
    return(total_pop)
}


#' synthesize_population_bog()
#' 
#' This function creates a synthetic population using microdata
#' from IPUMS-International and using age and gender-stratified estimates of
#' population
#' @param microdata_file (character) path of the file with the IPUMS microdata for the region of interest
#' @param agepop_file (character) path of the file with the population estimates by gender and agegroups
#' @param country_code (numeric) 3 digit number for the country, e.g., 170 -> Colombia
#' @param adm_code (numeric) administrative code used in IPUMS for the region of interest
#' @param adm_census_code (numeric) administrative code used by the national government for statistical purposes
#' @param year_pop (numeric) year of the population estimates
#' @param fitHCons (boolean) (default = FALSE) should house constraints be included?
#' 
#' @return A list of dataframes for synthetic people and their synthetic houses
#' @export
#' @examples
#'
#' synth_data = synthesize_population(microdata_file = "colombia_microdata.csv",
#' agepop_file = "colombia_pop_2010.csv", country_code = 170, adm_code = 25005,
#' adm_census_code = 25307, year_pop = 2010)
#' 
synthesize_population_bog = function(microdata_file, agepop_file, country_code,
                                 adm_code, adm_census_code, year_pop,
                                 school_file,university_file, workplace_file,
                                 people_file, house_file,
                                 fitHCons = FALSE, HCons_df = NULL,
                                 capPop = -1,
                                 subcity = FALSE, subcity_counter = 0,
                                 subcity_zone = "", subcity_total_pop = 0,
                                 house_counter = 0){
    browser()
    if(subcity == FALSE || subcity_counter == 0){
        unlink(people_file)
        unlink(house_file)
    }
    micro_data = read_csv(microdata_file) %>% mutate(HHWT = as.double(HHWT)) %>%
        drop_na()
    micro_data$AGE[micro_data$AGE > 99] = 99

    propCap = 1.0
    if(capPop > 100000){
        tmp_n = read_csv(agepop_file) %>%
            filter(Code == sprintf("%05d",adm_census_code), Year == year_pop, 
                   AgeGroup != 'Total', Gender != 'Total') %>%
            tally(Pop) %>% pull(n)
        if(tmp_n > capPop){
            propCap = capPop / tmp_n
        }
    }
    ## Read marginal distributions
    agepop_data = read_csv(agepop_file) %>% 
        filter(Code == sprintf("%05d",adm_census_code), Year == year_pop, 
               AgeGroup != 'Total', Gender != 'Total') %>%
        mutate(Gender = ifelse(Gender == "Male","m","f")) %>%
        unite(AGEGENDER,Gender,AgeGroup, sep = "") %>% 
        dplyr::select(AGEGENDER, Pop) %>%
        mutate(AGEGENDER = str_replace_all(AGEGENDER,"-","_")) %>%
        mutate(Pop = Pop * propCap)  

    breaks_df = filter(agepop_data, str_detect(AGEGENDER ,'f')) %>%
        transmute(AgeRange = substring(AGEGENDER,2),
                  Labels = substring(AGEGENDER,2)) %>%
        mutate(AgeRange = str_replace_all(AgeRange, "above","200")) %>%
        separate(AgeRange, into = c("MinAge","MaxAge"), convert = TRUE)
    
    ## Prepare constraints for the IPF algorithm
    ## Sex: Male = 1, Female = 2    
    brks = c(as.integer(breaks_df$MinAge),200)
    lbls = breaks_df$Labels
    cat(sprintf("breaks and labels created for %d-%d\n", country_code, adm_code))
    microdata_hhsize = micro_data %>% 
        group_by(SERIAL) %>% summarize(HHSIZE = n()) %>% ungroup()
    
    micro_data = micro_data %>%
        mutate(GENDER = ifelse(SEX == 1, "m","f")) %>%
        mutate(AGEGROUP = cut(micro_data$AGE,breaks = brks, labels = lbls, right = FALSE)) %>%
        unite(AGEGENDER, GENDER, AGEGROUP, sep = "", remove= FALSE) %>%
        left_join(microdata_hhsize, by = "SERIAL") %>%
        mutate(WORKSTATUS = ifelse(EMPSTAT == 1, "w", "nw"),
               SCHOOLSTATUS = ifelse(SCHOOL != 1, "ns",
                                     ifelse(AGE < 19, "bs", "cs")))

    cat(sprintf("crosstabulating constraints for %d-%d\n",country_code,adm_code))
    consP = xtabs(Pop ~ AGEGENDER, data = agepop_data)        

    if(is.null(nrow(HCons_df))){
        stop("Please specify Houses per zone")
    }

    ## Workers CONSTRAINTS
    workers_coverage = read_csv(workplace_file)
    total_workers_coverage = sum(workers_coverage$TotalWorkers) / subcity_total_pop
    
    if(total_workers_coverage == 0){
        ## at least 1% employed
        total_workers_coverage = 0.01
    }

    workers_data = data.frame(w = floor(total_workers_coverage * sum(agepop_data$Pop)),stringsAsFactors = F) %>%
        mutate(nw = sum(agepop_data$Pop) - w) %>%
        gather(key = WORKSTATUS, value = Pop)
    
    consW = xtabs(Pop ~ WORKSTATUS, data = workers_data)

    ## SCHOOL CONSTRAINTS
    ## 1. Assume people only go to school from 4 to 18
    school_coverage = read_csv(school_file) %>%
        rename(year = "AÑO", mun_code = "CÓDIGO_MUNICIPIO", coverage = "TASA_MATRICULACIÓN_5_16") %>%
        dplyr::select(year, mun_code, coverage) %>% filter(year == 2018, mun_code == city_code) %>%
        mutate(coverage = coverage / 100)
    
    university_coverage = readxl::read_xlsx(universities_file, skip = 5) %>%
        filter(Semestre == 1) %>%
        rename(mun_code = "Código del \r\nMunicipio\r\n(Programa)",
               name = "Institución de Educación Superior (IES)",
               capacity = "Matriculados 2018") %>%
        dplyr::select(mun_code, name, capacity) %>%
        mutate(mun_code = sprintf("%05d",mun_code)) %>%
        filter(mun_code == sprintf("%05d",as.numeric(city_code)))%>%
        group_by(mun_code, name) %>%
        summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
        filter(capacity > 10) %>% group_by(mun_code) %>%
        summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
        mutate(year = 2018, mun_code = as.numeric(mun_code))


    if(subcity == TRUE){
        if(year_pop == 2020){
            university_coverage$year = 2018
            school_coverage$year = 2020
        }
    }

    print(school_coverage)
    print(university_coverage)
    
    school_year_pop = read_csv(agepop_file) %>%
        filter(Code == sprintf("%05d",adm_census_code), Year == school_coverage$year, 
               AgeGroup != 'Total', Gender != 'Total') %>%
        tally(Pop) %>% pull(n)
    
    school_pop = filter(agepop_data, AGEGENDER %in% c("m5_9","f5_9","m10_14","f10_14")) %>%
        summarize(Pop = sum(Pop)) %>%
        bind_rows(filter(agepop_data,AGEGENDER %in% c("m0_4","f0_4")) %>% summarize(Pop = sum(Pop)/5)) %>%
        bind_rows(filter(agepop_data, AGEGENDER %in% c("m15_19","f15_19")) %>% summarize(Pop = sum(Pop)*4/5)) %>%
        summarize(Pop = sum(Pop)) %>% pull(Pop)
    
    school_coverage$coverage = floor(school_coverage$coverage * school_pop)
    if(subcity == TRUE){
        school_year_pop = subcity_total_pop
    }
    university_coverage$coverage = floor((university_coverage$capacity / school_year_pop)  * sum(agepop_data$Pop))
    
    students_data = data.frame(bs = school_coverage$coverage, cs = university_coverage$coverage, ns = sum(agepop_data$Pop) - school_coverage$coverage - university_coverage$coverage, stringsAsFactors = F) %>%
        gather(key = SCHOOLSTATUS, value = Pop)
    print(students_data)
    consS = xtabs(Pop ~ SCHOOLSTATUS, data = students_data)

    
    ## IPF Algorithm
    setDT(micro_data)
    if(fitHCons == TRUE){
        HCons_df$HHSIZE = HCons_df$PersonsHousehold
        consH = list(xtabs(NumHouses ~ HHSIZE, data = HCons_df))
    }else{
        consH = NULL
    }
    
    cat(sprintf("calibrating weights for %d-%d\n",country_code,adm_code))
    calibrated_weights = surveysd::ipf(micro_data, hid = NULL, 
                              conP = list(consP, consW, consS),
                              conH = consH,
                              #epsP = 1e-7,
                              epsP = 1e-7,
                              epsH = 1e-3,
                              w = "HHWT",
                              bound = NULL, 
                              verbose = TRUE, 
                              maxIter = 5000)

    ##browser()
    
    micro_data$ADJHHWT = calibrated_weights$calibWeight
    micro_data = as_tibble(micro_data)
    
    total_pop = 0
    total_houses = house_counter

    final_pop = sum(agepop_data$Pop)
    print(sprintf("People to create: %d", final_pop))
    micro_data = micro_data %>% drop_na()
    if(nrow(micro_data) == 0){
        stop("Something went wrong when adjusting the weights, all outputs are NA")
    }
    while(total_pop < final_pop){
        tmphouse = sample_n(micro_data, 1,replace = T, weight = micro_data$ADJHHWT)
        tmppeople = filter(micro_data,SERIAL == tmphouse$SERIAL)
        tmphouse$HHID = sprintf('%03d%06d%07d', country_code,adm_census_code,total_houses + 1)
        tmppeople$HHID = sprintf('%03d%06d%07d', country_code,adm_census_code,total_houses + 1)
        if(subcity == TRUE){
            tmphouse$ZONE = subcity_zone
            tmppeople$ZONE = subcity_zone
        }
        if(subcity == FALSE){
            if(total_pop == 0){
                write_csv(tmphouse,house_file,append = F)
                write_csv(tmppeople,people_file,append = F)
            }else{
                write_csv(tmphouse,house_file,append = T)
                write_csv(tmppeople,people_file,append = T)
            }
        }else{
            if(total_pop == 0 & subcity_counter == 0){
                write_csv(tmphouse,house_file,append = F)
                write_csv(tmppeople,people_file,append = F)
            }else{
                write_csv(tmphouse,house_file,append = T)
                write_csv(tmppeople,people_file,append = T)
            }
        }

        total_pop = total_pop + tmphouse$HHSIZE
        total_houses = total_houses + 1
        if(total_houses %% 1000 == 0){
            cat("\rSubcity counter: ",subcity_counter, " Houses created:",total_houses, " People created: ", total_pop, "final pop: ", final_pop)
        }
    }    
    return(list(total_pop=total_pop, total_houses = total_houses))
}


#' synthesize_population_col()
#' 
#' This function creates a synthetic population using microdata
#' from IPUMS-International and using age and gender-stratified estimates of
#' population
#' @param microdata_file (character) path of the file with the IPUMS microdata for the region of interest
#' @param agepop_file (character) path of the file with the population estimates by gender and agegroups
#' @param country_code (numeric) 3 digit number for the country, e.g., 170 -> Colombia
#' @param adm_code (numeric) administrative code used in IPUMS for the region of interest
#' @param adm_census_code (numeric) administrative code used by the national government for statistical purposes
#' @param year_pop (numeric) year of the population estimates
#' @param fitHCons (boolean) (default = FALSE) should house constraints be included?
#' 
#' @return A list of dataframes for synthetic people and their synthetic houses
#' @export
#' @examples
#'
#' synth_data = synthesize_population(microdata_file = "colombia_microdata.csv",
#' agepop_file = "colombia_pop_2010.csv", country_code = 170, adm_code = 25005,
#' adm_census_code = 25307, year_pop = 2010)
#' 
synthesize_population_col = function(microdata_file, agepop_file, country_code,
                                 adm_code, adm_census_code, year_pop,
                                 school_file,university_file, workplace_file,
                                 people_file, house_file,
                                 fitHCons = FALSE, HCons_df = NULL,
                                 capPop = -1,
                                 subcity = FALSE, subcity_counter = 0,
                                 subcity_zone = "", subcity_total_pop = 0,
                                 house_counter = 0){

    ##browser()

    if(subcity == FALSE || subcity_counter == 0){
        unlink(people_file)
        unlink(house_file)
    }
    micro_data = read_csv(microdata_file) %>% mutate(HHWT = as.double(HHWT)) %>%
        drop_na()
    micro_data$AGE[micro_data$AGE > 99] = 99

    propCap = 1.0
    if(capPop > 100000){
        tmp_n = read_csv(agepop_file) %>%
            filter(as.numeric(Zone) == adm_census_code, Year == year_pop, 
                   AgeGroup != 'Total', Gender != 'Total') %>%
            tally(Pop) %>% pull(n)
        if(tmp_n > capPop){
            propCap = capPop / tmp_n
        }
    }
    ## Read marginal distributions
    agepop_data = read_csv(agepop_file) %>% 
        filter(as.numeric(Zone) == adm_census_code, Year == year_pop, 
               AgeGroup != 'Total', Gender != 'Total') %>%
        mutate(Gender = ifelse(Gender == "Male","m","f")) %>%
        unite(AGEGENDER,Gender,AgeGroup, sep = "") %>% 
        dplyr::select(AGEGENDER, Pop) %>%
        mutate(AGEGENDER = str_replace_all(AGEGENDER,"-","_")) %>%
        mutate(Pop = Pop * propCap)  

    breaks_df = filter(agepop_data, str_detect(AGEGENDER ,'f')) %>%
        transmute(AgeRange = substring(AGEGENDER,2),
                  Labels = substring(AGEGENDER,2)) %>%
        mutate(AgeRange = str_replace_all(AgeRange, "above","200")) %>%
        separate(AgeRange, into = c("MinAge","MaxAge"), convert = TRUE)
    
    ## Prepare constraints for the IPF algorithm
    ## Sex: Male = 1, Female = 2    
    brks = c(as.integer(breaks_df$MinAge),200)
    lbls = breaks_df$Labels
    cat(sprintf("breaks and labels created for %d-%d\n", country_code, adm_code))
    microdata_hhsize = micro_data %>% 
        group_by(SERIAL) %>% summarize(HHSIZE = n()) %>% ungroup()
    
    micro_data = micro_data %>%
        mutate(GENDER = ifelse(SEX == 1, "m","f")) %>%
        mutate(AGEGROUP = cut(micro_data$AGE,breaks = brks, labels = lbls, right = FALSE)) %>%
        unite(AGEGENDER, GENDER, AGEGROUP, sep = "", remove= FALSE) %>%
        left_join(microdata_hhsize, by = "SERIAL") %>%
        mutate(WORKSTATUS = ifelse(EMPSTAT == 1, "w", "nw"),
               SCHOOLSTATUS = ifelse(SCHOOL != 1, "ns",
                                     ifelse(AGE < 19, "bs", "cs")))

    cat(sprintf("crosstabulating constraints for %d-%d\n",country_code,adm_code))
    consP = xtabs(Pop ~ AGEGENDER, data = agepop_data)        

    if(is.null(nrow(HCons_df))){
        stop("Please specify Houses per zone")
    }

    ## Workers CONSTRAINTS
    workers_coverage = read_csv(workplace_file) %>% filter(CODE == mun_code_)
    total_workers_coverage = sum(workers_coverage$TotalWorkers) / subcity_total_pop
    
    if(total_workers_coverage == 0){
        ## at least 1% employed
        total_workers_coverage = 0.01
    }

    workers_data = data.frame(w = floor(total_workers_coverage * sum(agepop_data$Pop)),stringsAsFactors = F) %>%
        mutate(nw = sum(agepop_data$Pop) - w) %>%
        gather(key = WORKSTATUS, value = Pop)
    
    consW = xtabs(Pop ~ WORKSTATUS, data = workers_data)

    ## SCHOOL CONSTRAINTS
    ## 1. Assume people only go to school from 4 to 18
    school_coverage = read_csv(school_file) %>%
        rename(year = "AÑO", mun_code = "CÓDIGO_MUNICIPIO", coverage = "TASA_MATRICULACIÓN_5_16") %>%
        dplyr::select(year, mun_code, coverage) %>% filter(year == 2018, mun_code == mun_code_) %>%
        mutate(coverage = coverage / 100)
    
    university_coverage = readxl::read_xlsx(universities_file, skip = 7) %>%
        filter(SEMESTRE == 1) %>%
        rename(mun_code = "CÓDIGO DEL MUNICIPIO (PROGRAMA)",
                name = "INSTITUCIÓN DE EDUCACIÓN SUPERIOR (IES)",
                capacity = "MATRICULADOS") %>%
        dplyr::select(mun_code, name, capacity) %>%
        filter(as.numeric(mun_code) == mun_code_) %>%
        group_by(mun_code, name) %>%
        summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
        filter(capacity > 10) %>% group_by(mun_code) %>%
        summarize(capacity = sum(capacity, na.rm = T)) %>% ungroup() %>%
        mutate(year = 2018, mun_code = as.numeric(mun_code))

    if(unlist(nrow(university_coverage)) == 0){
        university_coverage = data.frame(year = 2018, mun_code = as.numeric(mun_code_), capacity = 0)
    }

    if(subcity == TRUE){
        if(year_pop == 2020){
            university_coverage$year = 2018
            school_coverage$year = 2020
        }
    }

    print(school_coverage)
    print(university_coverage)
    
    school_year_pop = read_csv(agepop_file) %>%
        filter(as.numeric(Zone) == adm_census_code, Year == school_coverage$year, 
               AgeGroup != 'Total', Gender != 'Total') %>%
        tally(Pop) %>% pull(n)
    
    school_pop = filter(agepop_data, AGEGENDER %in% c("m5_9","f5_9","m10_14","f10_14")) %>%
        summarize(Pop = sum(Pop)) %>%
        bind_rows(filter(agepop_data,AGEGENDER %in% c("m0_4","f0_4")) %>% summarize(Pop = sum(Pop)/5)) %>%
        bind_rows(filter(agepop_data, AGEGENDER %in% c("m15_19","f15_19")) %>% summarize(Pop = sum(Pop)*4/5)) %>%
        summarize(Pop = sum(Pop)) %>% pull(Pop)
    
    school_coverage$coverage = floor(school_coverage$coverage * school_pop)
    if(subcity == TRUE){
        school_year_pop = subcity_total_pop
    }
    university_coverage$coverage = floor((university_coverage$capacity / school_year_pop)  * sum(agepop_data$Pop))
    
    students_data = data.frame(bs = school_coverage$coverage, cs = university_coverage$coverage, ns = sum(agepop_data$Pop) - school_coverage$coverage - university_coverage$coverage, stringsAsFactors = F) %>%
        gather(key = SCHOOLSTATUS, value = Pop)
    print(students_data)
    consS = xtabs(Pop ~ SCHOOLSTATUS, data = students_data)

    ##browser()

    ## IPF Algorithm
    setDT(micro_data)
    if(fitHCons == TRUE){
        HCons_df$HHSIZE = HCons_df$PersonsHousehold
        consH = list(xtabs(NumHouses ~ HHSIZE, data = HCons_df))
    }else{
        consH = NULL
    }
    
    cat(sprintf("calibrating weights for %d-%d\n",country_code, adm_census_code))
    calibrated_weights = surveysd::ipf(micro_data, hid = NULL, 
                              conP = list(consP, consW, consS),
                              conH = consH,
                              #epsP = 1e-7,
                              epsP = 1e-7,
                              epsH = 1e-3,
                              w = "HHWT",
                              bound = NULL, 
                              verbose = TRUE, 
                              maxIter = 5000)

    ##browser()
    
    micro_data$ADJHHWT = calibrated_weights$calibWeight
    micro_data = as_tibble(micro_data)
    
    total_pop = 0
    total_houses = house_counter

    final_pop = sum(agepop_data$Pop)
    print(sprintf("People to create: %d", final_pop))
    micro_data = micro_data %>% drop_na()
    if(nrow(micro_data) == 0){
        stop("Something went wrong when adjusting the weights, all outputs are NA")
    }
    while(total_pop < final_pop){
        print(total_pop)
        tmphouse = sample_n(micro_data, 1, replace = T, weight = micro_data$ADJHHWT)
        tmppeople = filter(micro_data,SERIAL == tmphouse$SERIAL)
        tmphouse$HHID = sprintf('%03d%06d%07d', country_code,adm_census_code,total_houses + 1)
        tmppeople$HHID = sprintf('%03d%06d%07d', country_code,adm_census_code,total_houses + 1)
        if(subcity == TRUE){
            tmphouse$ZONE = subcity_zone
            tmppeople$ZONE = subcity_zone
        }
        if(subcity == FALSE){
            if(total_pop == 0){
                write_csv(tmphouse,house_file,append = F)
                write_csv(tmppeople,people_file,append = F)
            }else{
                write_csv(tmphouse,house_file,append = T)
                write_csv(tmppeople,people_file,append = T)
            }
        }else{
            if(total_pop == 0 & subcity_counter == 0){
                write_csv(tmphouse,house_file,append = F)
                write_csv(tmppeople,people_file,append = F)
            }else{
                write_csv(tmphouse,house_file,append = T)
                write_csv(tmppeople,people_file,append = T)
            }
        }

        total_pop = total_pop + tmphouse$HHSIZE
        total_houses = total_houses + 1
        if(total_houses %% 1000 == 0){
            cat("\rSubcity counter: ",subcity_counter, " Houses created:",total_houses, " People created: ", total_pop, "final pop: ", final_pop)
        }
    }    
    return(list(total_pop=total_pop, total_houses = total_houses))
}
