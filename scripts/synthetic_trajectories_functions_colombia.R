#' assign_schools_workplaces
#' Assigns students to school based on distance
#' and workers to workplaces
#' @param people_df: file with synthesized microdata for people
#' @param houses_df: file with formatted houses
#' @param schools_df: file with formatted schools
#' @param synth_priv_schools: file with formatted private schools
#' @return  a dataframe of synthetic people with school and workplace assigned
#' @export
#' @examples assign_school_workplace('microdata_people_file.csv','formated_houses.txt','formatted_schools.txt','formatted_workplaces.txt')
#'
#' 
assign_schools <- function(people_df, houses_df, schools_df,
                           students_mobility_df, geodata_info, 
                           departament_code, esc_shp) {

    ##browser()
  
    geodata_info = geodata_info %>% filter(as.numeric(COD_DEPTO) == departament_code)
    municipio_list = sort(unique(geodata_info$COD_MUN))

    schools_df <- merge(schools_df, geodata_info, by.x = 'COD_MUN', by.y = 'COD_MUN') %>%
                  filter(as.numeric(COD_DEPTO) == departament_code)

    ##===============================================================#
    ## 2. Assign students to schools--------
    ##===============================================================#  
    ## 3.2 Schools assigned in multiple steps:
    ## 3.2.1 Mark synth people as students based on their Municipio of residence
    college_ind = which(people_df$SCHOOLSTATUS == 'cs')
    people_college_df = people_df[college_ind,]

    people_df = people_df[-college_ind,]
    potential_students_ind = which(people_df$AGE >= min(students_mobility_df$Min_age) & people_df$AGE <= max(students_mobility_df$Max_age))
    people_df$SCHOOLSTATUS[people_df$SCHOOLSTATUS == 'bs'] = 'ns'

    students_df = people_df[potential_students_ind,]
    students_df$Municipio = 0
    students_df$SchoolType = ""
    
    nonstudents_df = people_df[-potential_students_ind,]

    # Loop to go through all the localities to assign public schools
    for(ll in 1:length(municipio_list)){
        tmp_mun = municipio_list[ll]
        print(tmp_mun)
        ## For each locality and grade find the number of students and assign their school's locality
        geodata_mun = geodata_info %>% filter(geodata_info$COD_MUN == tmp_mun)
        
        ## PUBLIC SCHOOLS:        
        mun_schools = schools_df %>% filter(COD_MUN == tmp_mun)
        mun_students = students_mobility_df %>%
            filter(origin_municipio == tmp_mun)
        
        grades_list = sort(unique(mun_students$Grade))   
        for(gg in 1:length(grades_list) ){
            ## Get number of students in each grade and select their indices from the population
            ## Sample students and assign them a locality to go to school
            tmp_students = filter(mun_students, Grade == grades_list[gg]) %>%
                dplyr::select(destination_municipio, Min_age, Max_age, ResidentStudents) %>%
                mutate(ResidentProp = ResidentStudents / sum(ResidentStudents))
            
            tmp_ind_student = which(students_df$AGE >= tmp_students$Min_age[1] & students_df$AGE <= tmp_students$Max_age[1] & students_df$stcotrbg %in% geodata_mun$COD_MUN & students_df$SCHOOLSTATUS == 'ns')

            students_to_sample = ifelse(round(sum(tmp_students$ResidentStudents)) <= length(tmp_ind_student), round(sum(tmp_students$ResidentStudents)), length(tmp_ind_student))

            if(students_to_sample > 0){
                ind_student_municpio = sample(tmp_ind_student, size = students_to_sample)
                students_df$SCHOOLSTATUS[ind_student_municpio] = 'bs'
                students_df$SchoolType[ind_student_municpio] = 'Public'

                if(length(tmp_students$destination_municipio) > 1){
                    students_df$Municipio[ind_student_municpio] = sample(tmp_students$destination_municipio, size = length(ind_student_municpio), prob = tmp_students$ResidentProp, replace = TRUE)
                }else{
                    students_df$Municipio[ind_student_municpio] = tmp_students$destination_municipio
                }
            }
        }
    }
  
    ## Re-generate the students dataframe
    nonstudents_df = bind_rows(nonstudents_df, students_df[which(students_df$SCHOOLSTATUS == "ns"),])
    students_df = students_df[which(students_df$SCHOOLSTATUS == "bs"),]


    ## 2. Compute distances between SCACODIGOs  
    geodata_mun = geodata_info #%>% filter(geodata_info$COD_MUN == tmp_mun)
    mun_shp = esc_shp[esc_shp@data$ID_ESPACIA %in% geodata_mun$COD_MUN,]
    distance_esc = sapply(1:nrow(mun_shp@data), function(x){rgeos::gDistance(mun_shp[x,], mun_shp, byid = T)})
    colnames(distance_esc) = as.numeric(mun_shp@data$ID_ESPACIA)
    rownames(distance_esc) = as.numeric(mun_shp@data$ID_ESPACIA)

    for(ll in 1:length(municipio_list)){
    # for(ll in 1:1){
        tmp_mun = municipio_list[ll]
        print(sprintf("Assigning students in schools of municipality %d", tmp_mun))
        
        ## 3. PUBLIC SCHOOL STUDENTS
        mun_schools = schools_df %>% filter(COD_MUN == tmp_mun)
        grades_list = sort(unique(mun_schools$Grade))
        ## For each locality and grade find the number of students and assign their schools
        for(gg in 1:length(grades_list) ){
        # for(gg in 1:1 ){
            
            tmp_schools = filter(mun_schools, Grade == grades_list[gg]) %>%
                mutate(assignedStudents = 0)
            tmp_ind_students = which(students_df$Municipio == tmp_mun & students_df$AGE >= tmp_schools$Min_age[1] & students_df$AGE <= tmp_schools$Max_age[1] & students_df$SchoolType == "Public")
            print(sprintf("Grade %s for Public schools municipality %d. Size of schools %d, size of students %d", grades_list[gg], tmp_mun, nrow(tmp_schools), length(tmp_ind_students)))
            if(length(tmp_ind_students) == 0){
                next
            }
            
            school_students_grade = as.data.frame(table(sample(1:nrow(tmp_schools), length(tmp_ind_students), replace = T, prob = tmp_schools$Students / sum(tmp_schools$Students))))
            print(head(school_students_grade))
            colnames(school_students_grade) = c("Indx", "Students")
            tmp_schools$Students[as.numeric(school_students_grade$Indx)] = as.numeric(school_students_grade$Students)
            tmp_schools$Students[tmp_schools$Students == 0] = 1                        

            for(pp in 1:length(tmp_ind_students)){
                if(students_df$stcotrbg[tmp_ind_students[pp]] %in% rownames(distance_esc)){
                    ## If in same locality, sample based on distance and capacity
                    dist_zones = as.numeric(distance_esc[as.character(students_df$stcotrbg[tmp_ind_students[pp]]), as.character(tmp_schools$COD_MUN)])
                    prob_schools = (tmp_schools$assignedStudents < tmp_schools$Students)

                    if(sum(dist_zones) > 0 & length(dist_zones) > 1){
                        if(var(dist_zones) > 0){
                            dist_prob = exp(-(dist_zones^2)/(2*var(dist_zones)))
                        }else{
                            dist_prob =exp(-dist_zones^2)
                        }
                        if(sum(dist_prob) <=  0.0000001){
                            dist_prob = dist_prob + 1/length(dist_prob)
                        }
                        ind_school = sample(1:nrow(tmp_schools), size = 1, replace = F, prob = dist_prob * prob_schools)
                    }else{
                        ind_school = sample(1:nrow(tmp_schools), size = 1, replace = F, prob = prob_schools /nrow(tmp_schools))
                    }
                    students_df$SCHOOL_ID[tmp_ind_students[pp]] = as.character(tmp_schools$School_Code[ind_school])
                    tmp_schools$assignedStudents[ind_school] = tmp_schools$assignedStudents[ind_school] + 1
                }else{
                    ## If not in locality, just assign randomly
                    prob_schools =   (tmp_schools$assignedStudents < tmp_schools$Students) / nrow(tmp_schools)
                    ind_school = sample(1:nrow(tmp_schools),size = 1, replace = FALSE, prob = prob_schools)
                    students_df$SCHOOL_ID[tmp_ind_students[pp]] = as.character(tmp_schools$School_Code[ind_school])
                    tmp_schools$assignedStudents[ind_school] = tmp_schools$assignedStudents[ind_school] + 1
                }
            }
        }
    }
    
    ##browser()
    # Reassemble the people dataframe
    people_df = bind_rows(students_df, nonstudents_df, people_college_df)

    # Create final schools dataframe
    schools_out_df = schools_df %>% 
        group_by(School_Code, School_Name, COD_MUN, NOM_MUNICI, NOM_DEPART, STABBR, Type, Longitude, Latitude, Zone) %>%
        summarize(N = n())%>%
        ungroup() %>%
        left_join(schools_df %>%
                  filter(Max_age <= 4) %>%
                  group_by(School_Code)  %>%
                  summarize(prek = sum(Students)) %>%
                  ungroup(), by = "School_Code")%>%   
        left_join(schools_df %>%
                  filter(Max_age == 5 & Min_age == 5) %>%
                  group_by(School_Code)  %>%
                  summarize(kinder = sum(Students))  %>%
                  ungroup(), by = "School_Code") %>%
        left_join(schools_df %>%
                  filter(Max_age <= 18 & Min_age >= 6) %>%
                  group_by(School_Code)  %>%
                  summarize(gr01_gr12 = sum(Students))  %>%
                  ungroup(), by = "School_Code"
                  ) %>%
        mutate(schooltype = 0)  # Indicating public schools
  
    # Create the output dataframe
    schools_out = schools_out_df %>% 
        rename( "latitude" = "Latitude",
                "longitude" = "Longitude") %>%
        mutate(ungraded = 0) %>%
        replace_na(list(prek = 0, kinder = 0, gr01_gr12 = 0)) %>%
        mutate( sp_id = sprintf("%.0f", School_Code), 
                stabbr = STABBR, 
                address = "unspecified", 
                city = NOM_MUNICI, 
                county = NOM_DEPART, 
                zipcode = COD_MUN, 
                zip4 = "0", 
                nces_id = "0", 
                total = prek + kinder + gr01_gr12, 
                source = "SED", 
                stco = 11001, 
                name = School_Name, 
                income = 0, 
                stcotrbg = sprintf("%s", COD_MUN)) %>% 
        dplyr::select(sp_id,name, stabbr, address, city, county, zipcode, zip4, nces_id, total, prek, kinder, gr01_gr12, ungraded, latitude, longitude, source, stco,income, stcotrbg, schooltype)
  
    return(list(people_out_df = people_df, schools_out = schools_out))
}


#' assign_schools_workplaces
#' Assigns students to school based on distance
#' and workers to workplaces
#' @param people_df: file with synthesized microdata for people
#' @param houses_df: file with formatted houses
#' @param colleges_df: file with formatted schools
#' 
#' @return  a dataframe of synthetic people with school and workplace assigned
#' @export
#' @examples assign_colleges('microdata_people_file.csv','formated_houses.txt','formatted_schools.txt','formatted_workplaces.txt')
#'
#' 
assign_colleges <- function(people_df, 
                            houses_df,
                            college_df,
                            geodata_info,
                            departament_code){    

    college_df$SchoolType = 'College'
    college_df <- merge(college_df, geodata_info, by.x = 'Mun_code', by.y = 'COD_MUN') %>%
                  filter(as.numeric(COD_DEPTO) == departament_code)

    ##===============================================================#
    ## 2. Assign students to schools--------
    ##===============================================================#
    ## Assign codes to universities
    college_df$sp_id = college_df$School_id

    ## 3.1 Universities are assigned at random, no distance involved
    ind_college_students    = which(people_df$SCHOOLSTATUS == 'cs')
    ind_universities        = college_df %>%  dplyr::select(sp_id, Ungraded)
    people_df$SCHOOL_ID[ind_college_students] = sample(rep(ind_universities$sp_id, times = ind_universities$Ungraded), size = length(ind_college_students), replace = T)
    
    ##===============================================================#
    ## 4. Write output--------
    ##===============================================================#        
    schools_out =  college_df %>%
        rename( "school_id" = "School_id",
                "name"      = "Name",
                "ungraded"  = "Ungraded",
                "prek"      = "Prek",
                "kinder"    = "Kinder",
                "gr01_gr12" = "Gr01_gr12",
                "income"    = "Income") %>%
        mutate(schooltype = 2) %>%
        mutate( stabbr  = STABBR, 
                address = "unspecified", 
                city    = NOM_MUNICI, 
                county  = NOM_DEPART, 
                zipcode = Mun_code, 
                zip4    = "0", 
                nces_id = "0", 
                total   = prek + kinder + gr01_gr12 + ungraded, 
                source  = "SED", 
                stco    = Mun_code, 
                stcotrbg = sprintf("%s", Mun_code), 
                income = income) %>%
        #rename(latitude = LAT, longitude = LON) %>%
        dplyr::select(sp_id, name, stabbr, address, city, county, zipcode, zip4, nces_id, total, prek, kinder, gr01_gr12, ungraded, source, stco, income, stcotrbg,schooltype)
    
    return(list(people_out = people_df, schools_out = schools_out))
}


#' assign_workplaces
#' Assigns students to school based on distance
#' and workers to workplaces
#' @param people_df: file with synthesized microdata for people
#' @param houses_df: file with formatted houses
#' @param workers_mobility: file with formatted workplaces
#' @param workplaces_df: file with formatted workplaces
#' @return  a dataframe of synthetic people with school and workplace assigned
#' @export
#' @examples assign_workplaces(people_df, houses_df, workers_mobility,workplace_df)
#'
#' 
assign_workplaces <- function(  people_df, 
                                houses_df, 
                                workers_mobility,
                                workplaces_df, 
                                geodata_info,
                                departament_code){

    ##browser()

    geodata_info = geodata_info %>% filter(as.numeric(COD_DEPTO) == departament_code)
    municipio_list = sort(unique(geodata_info$COD_MUN))

    ##===============================================================#
    ## 3. Assign workers -------------
    ##===============================================================#
    workers_ind = which(people_df$WORKSTATUS == "w")
    people_df$WORKPLACE_ID = NA
    people_df$Zone = as.numeric(people_df$Zone)
    workplaces_df$AssignedWorkers = 0
    mov_df = workers_mobility
    #     filter(Code_Destination != 20, OrigenMunicipio != 20) %>%
    #     right_join(data.frame(expand.grid(OrigenMunicipio = 1:19, DestinoMunicipio = 1:19)))

    workplace_list = data.frame(
        workplace_id = rep(workplaces_df$workplace_id, times = workplaces_df$TotalWorkers),
        Municipio = rep(workplaces_df$CODE, times = workplaces_df$TotalWorkers),
        stringsAsFactors = F)
    
    workers_summary =  people_df[workers_ind,] %>% 
                        left_join(geodata_info, by = c("stcotrbg" = "COD_MUN")) %>% 
                        group_by(stcotrbg) %>% 
                        summarize(People = n()) %>% 
                        ungroup() %>% 
                        rename(COD_MUN_ORIGEN = stcotrbg)

    workplaces_N =  workplace_list %>% group_by(Municipio) %>% summarize(N = n())  %>% ungroup() %>% mutate(People = N / sum(N) * sum(workers_summary$People)) %>% rename(COD_MUN_DESTINO = Municipio)
        
    ## Gotta be careful here with the workplace capacity, so far, I'm assuming there are at least enough workplaces with capacity for everyone in the city
    ##browser()
    municipio_list_people = workers_summary %>% arrange(People) %>% pull(COD_MUN_ORIGEN)
    for(ll in 1:length(municipio_list_people)){
        print(ll)
        tmp_mun = municipio_list_people[ll]
        geodata_mun = geodata_info %>% filter(as.numeric(COD_MUN) == tmp_mun)
        tmp_ind_workers = which(people_df$WORKSTATUS == "w" & people_df$stcotrbg %in% geodata_mun$COD_MUN)

        mun_mov = mov_df %>% filter(Code_Origin == tmp_mun)

        if(sum(mun_mov$Mobility_Rate) == 0){
            mun_mov$Mobility_Rate = 0.01
        }

        workers_municpio = sample(mun_mov$Code_Destination, size = length(tmp_ind_workers), replace = T, prob = mun_mov$Mobility_Rate)
        for(dl in 1:length(municipio_list)){
            if(length(which(workers_municpio == municipio_list[dl])) > 0){
                ind_workplace = which(workplace_list$Municipio == municipio_list[dl])
                if(length(ind_workplace) > 0){
                    if(length(which(workers_municpio == municipio_list[dl])) > length(ind_workplace)){
                        ind_workplace_sel = sample(ind_workplace, size = length(ind_workplace), replace = F)
                        ind_workplace_noloc = sample((1:nrow(workplace_list))[-ind_workplace], size = length(which(workers_municpio == municipio_list[dl])) - length(ind_workplace), replace = F)
                        combined_ind_workplace = c(ind_workplace_sel, ind_workplace_noloc)
                        people_df$WORKPLACE_ID[tmp_ind_workers[workers_municpio == municipio_list[dl]]] = workplace_list$workplace_id[combined_ind_workplace]
                        workplace_list = workplace_list[-combined_ind_workplace,]
                    }else{
                        ind_workplace_sel = sample(ind_workplace, size = length(which(workers_municpio == municipio_list[dl])), replace = F)
                        people_df$WORKPLACE_ID[tmp_ind_workers[workers_municpio == municipio_list[dl]]] = workplace_list$workplace_id[ind_workplace_sel]
                        workplace_list = workplace_list[-ind_workplace_sel,]
                    }
                }else{                    
                    ind_workplace_sel = sample((1:nrow(workplace_list)), size = length(which(workers_municpio == municipio_list[dl])), replace = F)
                    people_df$WORKPLACE_ID[tmp_ind_workers[workers_municpio == municipio_list[dl]]] = workplace_list$workplace_id[ind_workplace_sel]
                    workplace_list = workplace_list[-ind_workplace_sel,]
                }
            }
        }        
    }
   
    ##===============================================================#
    ## 4. Write output--------
    ##===============================================================#

    municipio_name_list = schools_df %>%
        dplyr::select(COD_MUN, Zone) %>%
        group_by(COD_MUN) %>%
        summarize(Zone = Zone[1]) %>%
        ungroup()
    
    ## Format workplaces
    workplaces_out = workplaces_df %>% rename(sp_id = workplace_id, workers = TotalWorkers) %>%
        dplyr::select(sp_id, workers)
    
    return(list(people_out = people_df, workplaces_out = workplaces_out))
}


format_people_out <- function(people_df) {
    people_out_df = people_df %>%
            mutate(sp_id = sprintf("%s%02d",as.character(HHID),PERNUM), sp_hh_id = HHID,
                   serialno = SERIAL, age = AGE, sex = SEX, race = 1, sporder = PERNUM, relate = RELATE, sp_school_id = SCHOOL_ID, sp_work_id = WORKPLACE_ID) %>%
            dplyr::select(sp_id, sp_hh_id, serialno, stcotrbg, age, sex, race, sporder, relate, sp_school_id, sp_work_id)%>%
            arrange(sp_hh_id, sporder)
    return(list(people_out = people_out_df))
}



