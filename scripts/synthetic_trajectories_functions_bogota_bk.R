#' assign_schools_workplaces
#' Assigns students to school based on distance
#' and workers to workplaces
#' @param people_file: file with synthesized microdata for people
#' @param synth_houses: file with formatted houses
#' @param synth_schools: file with formatted schools
#' @param synth_priv_schools: file with formatted private schools
#' @param synth_workplaces: file with formatted workplaces
#' @return  a dataframe of synthetic people with school and workplace assigned
#' @export
#' @examples assign_school_workplace('microdata_people_file.csv','formated_houses.txt','formatted_schools.txt','formatted_workplaces.txt')
#'
#' 
assign_schools_workplaces <- function(people_file, synth_houses, synth_schools, synth_priv_schools,
                                      students_mobility_df,
                                      workers_mobility,
                                      synth_colleges,
                                      synth_workplaces, geodata_info,esc_shp){
    schools_df = read_csv(synth_schools)
    priv_schools_df = read_csv(synth_priv_schools)
    college_df = read_csv(synth_colleges)
    college_df$SchoolType = 'College'
    houses_df = read_csv(synth_houses)

    people_df = read_csv(people_file) %>% dplyr::select(PERNUM, SERIAL, GENDER, SEX, AGE, HHID,RELATE, WORKSTATUS, SCHOOLSTATUS) %>%
        mutate(SCHOOL_ID = NA, WORKPLACE_ID = NA) %>%
        right_join(read_csv(synth_houses) %>% dplyr::select(sp_id, stcotrbg, latitude,longitude),by = c("HHID" = "sp_id"))
    people_df$Zone = substr(people_df$stcotrbg, 6, 13)
    people_df$SCHOOL_ID = as.character(people_df$SCHOOL_ID)
    people_df$WORKPLACE_ID = as.character(people_df$WORKPLACE_ID)

    geodata_info = geodata_info %>% drop_na() %>% dplyr::select(SCACODIGO, Localidad) %>%
        group_by(SCACODIGO) %>% summarize(Localidad = Localidad[1])
    localidad_list = sort(unique(geodata_info$Localidad))


    ##===============================================================#
    ## 3. Assign workers -------------
    ##===============================================================#
    workers_ind = which(people_df$WORKSTATUS == "w")
    people_df$WORKPLACE_ID = NA
    workplaces_df = read_csv(synth_workplaces) %>% mutate(AssignedWorkers = 0)
    mov_df = read_csv(workers_mobility) %>%
        filter(DestinoLocalidad != 20, OrigenLocalidad != 20) %>%
        right_join(data.frame(expand.grid(OrigenLocalidad = 1:19, DestinoLocalidad = 1:19)))

    workplace_list = data.frame(
        workplace_id = rep(workplaces_df$workplace_id, times = workplaces_df$TotalWorkers),
        Localidad = rep(workplaces_df$Localidad, times = workplaces_df$TotalWorkers),
        stringsAsFactors = F)

    ## people_df$WORKPLACE_ID[workers_ind] = sample(workplace_list$workplace_id, size = length(workers_ind))
    
    workers_summary =  people_df[workers_ind,] %>% left_join(geodata_info,by = c("Zone" = "SCACODIGO")) %>% group_by(Localidad) %>% summarize(People = n()) %>% ungroup() %>% rename(OrigenLocalidad = Localidad)

    workplaces_N =  workplace_list %>% group_by(Localidad) %>% summarize(N = n())  %>% ungroup() %>% mutate(People = N / sum(N) * sum(workers_summary$People)) %>% rename(DestinoLocalidad = Localidad)
        
    ## Gotta be careful here with the workplace capacity, so far, I'm assuming there are at least enough workplaces with capacity for everyone in the city
    
    localidad_list_people = workers_summary %>% arrange(People) %>% pull(OrigenLocalidad)
    
    for(ll in 1:length(localidad_list_people)){
        print(ll)
        tmp_loc = localidad_list_people[ll]
        geodata_loc = geodata_info %>% filter(geodata_info$Localidad == tmp_loc)
        tmp_ind_workers = which(people_df$WORKSTATUS == "w" & people_df$Zone %in% geodata_loc$SCACODIGO)

        loc_mov = mov_df %>% filter(OrigenLocalidad == tmp_loc)

        workers_localidad = sample(loc_mov$DestinoLocalidad, size = length(tmp_ind_workers), replace = T, prob = loc_mov$Trips)
        for(dl in 1:length(localidad_list)){
            if(length(which(workers_localidad == localidad_list[dl])) > 0){
                ind_workplace = which(workplace_list$Localidad == localidad_list[dl])
                if(length(ind_workplace) > 0){
                    if(length(which(workers_localidad == localidad_list[dl])) > length(ind_workplace)){
                        ind_workplace_sel = sample(ind_workplace, size = length(ind_workplace), replace = F)
                        ind_workplace_noloc = sample((1:nrow(workplace_list))[-ind_workplace], size = length(which(workers_localidad == localidad_list[dl])) - length(ind_workplace), replace = F)
                        combined_ind_workplace = c(ind_workplace_sel, ind_workplace_noloc)
                        people_df$WORKPLACE_ID[tmp_ind_workers[workers_localidad == localidad_list[dl]]] = workplace_list$workplace_id[combined_ind_workplace]
                        workplace_list = workplace_list[-combined_ind_workplace,]
                    }else{
                        ind_workplace_sel = sample(ind_workplace, size = length(which(workers_localidad == localidad_list[dl])), replace = F)
                        people_df$WORKPLACE_ID[tmp_ind_workers[workers_localidad == localidad_list[dl]]] = workplace_list$workplace_id[ind_workplace_sel]
                        workplace_list = workplace_list[-ind_workplace_sel,]
                    }
                }else{                    
                    ind_workplace_sel = sample((1:nrow(workplace_list)), size = length(which(workers_localidad == localidad_list[dl])), replace = F)
                    people_df$WORKPLACE_ID[tmp_ind_workers[workers_localidad == localidad_list[dl]]] = workplace_list$workplace_id[ind_workplace_sel]
                    workplace_list = workplace_list[-ind_workplace_sel,]
                }
            }
        }        
    }


    ##===============================================================#
    ## 2. Assign students to schools--------
    ##===============================================================#
    ## Assign codes to universities
    college_df$sp_id = sprintf("170011001%05d", 1:nrow(college_df))

    ## 3.1 Universities are assigned at random, no distance involved
    ind_college_students = which(people_df$SCHOOLSTATUS == 'cs')
    ind_universities = college_df %>%  dplyr::select(sp_id, ungraded)
    people_df$SCHOOL_ID[ind_college_students] = sample(rep(ind_universities$sp_id, times = ind_universities$ungraded), size = length(ind_college_students), replace = T)


    ## 3.2 Schools assigned in multiple steps:
    ## 3.2.1 Mark synth people as students based on their localidad of residence
    potential_students_ind = which(people_df$AGE >= min(students_mobility_df$MinAge) & people_df$AGE <= max(students_mobility_df$MaxAge))
    people_df$SCHOOLSTATUS[people_df$SCHOOLSTATUS == 'bs'] = 'ns'
    
    students_df = people_df[potential_students_ind,]
    students_df$Localidad = 0
    students_df$SchoolType = ""
    
    nonstudents_df = people_df[-potential_students_ind,]
    
    for(ll in 1:length(localidad_list)){
        tmp_loc = localidad_list[ll]
        print(tmp_loc)
        ## For each locality and grade find the number of students and assign their school's locality
        geodata_loc = geodata_info %>% filter(geodata_info$Localidad == tmp_loc)
        
        ## PUBLIC SCHOOLS:        
        loc_schools = schools_df %>% filter(NumberLocalidad == tmp_loc)
        loc_students = students_mobility_df %>%
            filter(ResidenceLocalidad == tmp_loc, SchoolType == "Public")
        
        grades_list = sort(unique(loc_students$Grade))
        
        for(gg in 1:length(grades_list) ){
            ## Get number of students in each grade and select their indices from the population
            ## Sample students and assign them a locality to go to school
            tmp_students = filter(loc_students, Grade == grades_list[gg]) %>%
                dplyr::select(NumberLocalidad, MinAge, MaxAge, ResidentStudents) %>%
                mutate(ResidentProp = ResidentStudents / sum(ResidentStudents))
            
            tmp_ind_student = which(students_df$AGE >= tmp_students$MinAge[1] & students_df$AGE <= tmp_students$MaxAge[1] & students_df$Zone %in% geodata_loc$SCACODIGO & students_df$SCHOOLSTATUS == 'ns')

            students_to_sample = ifelse(round(sum(tmp_students$ResidentStudents)) <= length(tmp_ind_student), round(sum(tmp_students$ResidentStudents)), length(tmp_ind_student))

            if(students_to_sample > 0){
                ind_student_locality = sample(tmp_ind_student, size = students_to_sample)
                students_df$SCHOOLSTATUS[ind_student_locality] = 'bs'
                students_df$SchoolType[ind_student_locality] = 'Public'

                if(length(tmp_students$NumberLocalidad) > 1){
                    students_df$Localidad[ind_student_locality] = sample(tmp_students$NumberLocalidad, size = length(ind_student_locality), prob = tmp_students$ResidentProp, replace = TRUE)
                }else{
                    students_df$Localidad[ind_student_locality] = tmp_students$NumberLocalidad
                }
            }
        }

        ## PRIVATE SCHOOLS:
        loc_schools = priv_schools_df %>% filter(NumberLocalidad == tmp_loc)
        loc_students = students_mobility_df %>%
            filter(ResidenceLocalidad == tmp_loc, SchoolType == "Private")

        print("Private schools")
        grades_list = sort(unique(loc_students$Grade))
        for(gg in 1:length(grades_list) ){
            ## Get number of students in each grade and select their indices from the population
            tmp_students = filter(loc_students, Grade == grades_list[gg]) %>%
                dplyr::select(NumberLocalidad, MinAge, MaxAge, ResidentStudents) %>%
                mutate(ResidentProp = ResidentStudents / sum(ResidentStudents))
            
            tmp_ind_student = which(students_df$AGE >= tmp_students$MinAge[1] & students_df$AGE <= tmp_students$MaxAge[1] & students_df$Zone %in% geodata_loc$SCACODIGO & students_df$SCHOOLSTATUS == 'ns')
            students_to_sample = ifelse(round(sum(tmp_students$ResidentStudents)) <= length(tmp_ind_student), round(sum(tmp_students$ResidentStudents)), length(tmp_ind_student))
            
            ## Sample students and assign them a locality to go to school
            if(students_to_sample > 0){
                ind_student_locality = sample(tmp_ind_student, size = students_to_sample)
                students_df$SCHOOLSTATUS[ind_student_locality] = 'bs'
                students_df$SchoolType[ind_student_locality] = 'Private'

                if(length(tmp_students$NumberLocalidad) > 1){
                    students_df$Localidad[ind_student_locality] = sample(tmp_students$NumberLocalidad, size = length(ind_student_locality), prob = tmp_students$ResidentProp, replace = TRUE)
                }else{
                    students_df$Localidad[ind_student_locality] = tmp_students$NumberLocalidad
                }
            }
        }

    }

    ## Re-generate the students dataframe
    nonstudents_df = bind_rows(nonstudents_df, students_df[which(students_df$SCHOOLSTATUS == "ns"),])
    students_df = students_df[which(students_df$SCHOOLSTATUS == "bs"),]
    
    
    for(ll in 1:length(localidad_list)){
        tmp_loc = localidad_list[ll]
        print(sprintf("Assigning students in schools of locality %d", tmp_loc))
        
        ## 2. Compute distances between SCACODIGOs  
        geodata_loc = geodata_info %>% filter(geodata_info$Localidad == tmp_loc)
        loc_shp = esc_shp[esc_shp@data$SCACODIGO %in% geodata_loc$SCACODIGO,]
        distance_esc = sapply(1:nrow(loc_shp@data), function(x){rgeos::gDistance(loc_shp[x,], loc_shp, byid = T)})
        colnames(distance_esc) = loc_shp@data$SCACODIGO
        rownames(distance_esc) = loc_shp@data$SCACODIGO

        ## 3. PUBLIC SCHOOL STUDENTS
        loc_schools = schools_df %>% filter(NumberLocalidad == tmp_loc)
        grades_list = sort(unique(loc_schools$Grade))
        
        ## For each locality and grade find the number of students and assign their schools
        for(gg in 1:length(grades_list) ){
            print(sprintf("Grade %s for Public schools locality %d", grades_list[gg], tmp_loc))
            tmp_schools = filter(loc_schools, Grade == grades_list[gg]) %>%
                mutate(assignedStudents = 0)
            tmp_ind_students = which(students_df$Localidad == tmp_loc & students_df$AGE >= tmp_schools$MinAge[1] & students_df$AGE <= tmp_schools$MaxAge[1] & students_df$SchoolType == "Public")
            
            school_students_grade = as.data.frame(table(sample(1:nrow(tmp_schools), length(tmp_ind_students), replace = T, prob = tmp_schools$Students / sum(tmp_schools$Students))))
            colnames(school_students_grade) = c("Indx", "Students")
            tmp_schools$Students[as.numeric(school_students_grade$Indx)] = as.numeric(school_students_grade$Students)
            tmp_schools$Students[tmp_schools$Students == 0] = 1                        
            
            for(pp in 1:length(tmp_ind_students)){
                if(students_df$Zone[tmp_ind_students[pp]] %in% rownames(distance_esc)){
                    ## If in same locality, sample based on distance and capacity
                    dist_zones = as.numeric(distance_esc[students_df$Zone[tmp_ind_students[pp]],tmp_schools$Zone])
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
                    students_df$SCHOOL_ID[tmp_ind_students[pp]] = as.character(tmp_schools$SchoolCode[ind_school])
                    tmp_schools$assignedStudents[ind_school] = tmp_schools$assignedStudents[ind_school] + 1
                }else{
                    ## If not in locality, just assign randomly
                    prob_schools =   (tmp_schools$assignedStudents < tmp_schools$Students) / nrow(tmp_schools)
                    ind_school = sample(1:nrow(tmp_schools),size = 1, replace = FALSE, prob = prob_schools)
                    students_df$SCHOOL_ID[tmp_ind_students[pp]] = as.character(tmp_schools$SchoolCode[ind_school])
                    tmp_schools$assignedStudents[ind_school] = tmp_schools$assignedStudents[ind_school] + 1
                }
            }
        }
        
        ## 4. PRIVATE SCHOOL STUDENTS
        loc_schools = priv_schools_df %>% filter(NumberLocalidad == tmp_loc) %>%
            mutate(Grade = sprintf("%d-%d", MinAge, MaxAge))
        grades_list = sort(unique(loc_schools$Grade))
        
        ## For each locality and grade find the number of students and assign their schools
        for(gg in 1:length(grades_list) ){
            tmp_schools = filter(loc_schools, Grade == grades_list[gg]) %>%
                mutate(assignedStudents = 0)
            tmp_ind_students = which(students_df$Localidad == tmp_loc & students_df$AGE >= tmp_schools$MinAge[1] & students_df$AGE <= tmp_schools$MaxAge[1] & students_df$SchoolType == "Private")
            
            school_students_grade = as.data.frame(table(sample(1:nrow(tmp_schools), length(tmp_ind_students), replace = T, prob = tmp_schools$Students / sum(tmp_schools$Students))))
            colnames(school_students_grade) = c("Indx", "Students")
            tmp_schools$Students[as.numeric(school_students_grade$Indx)] = as.numeric(school_students_grade$Students)
            tmp_schools$Students[tmp_schools$Students == 0] = 1                        
            
            for(pp in 1:length(tmp_ind_students)){
                if(students_df$Zone[tmp_ind_students[pp]] %in% rownames(distance_esc)){
                    ## If in same locality, sample based on distance and capacity
                    dist_zones = as.numeric(distance_esc[students_df$Zone[tmp_ind_students[pp]],tmp_schools$Zone])
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
                    students_df$SCHOOL_ID[tmp_ind_students[pp]] = as.character(tmp_schools$SchoolCode[ind_school])
                    tmp_schools$assignedStudents[ind_school] = tmp_schools$assignedStudents[ind_school] + 1
                }else{
                    ## If not in locality, just assign randomly
                    prob_schools = (tmp_schools$assignedStudents < tmp_schools$Students) / nrow(tmp_schools)
                    ind_school = sample(1:nrow(tmp_schools),size = 1, replace = FALSE, prob = prob_schools)
                    students_df$SCHOOL_ID[tmp_ind_students[pp]] = as.character(tmp_schools$SchoolCode[ind_school])
                    tmp_schools$assignedStudents[ind_school] = tmp_schools$assignedStudents[ind_school] + 1
                }
            }
        }
    }
    
    people_df = bind_rows(students_df, nonstudents_df)
    
    ## Take schools and format them for FRED's output
    ##students_df = filter(people_df, SCHOOLSTATUS == 'bs') %>%
    ##    mutate(GRADE = ifelse(AGE < 5, "prek",
    ##                  ifelse(AGE == 5, "kinder",
    ##                   ifelse(AGE <= 18, "gr01_gr12"))))

    
    ##===============================================================#
    ## 4. Write output--------
    ##===============================================================#
    ## Finally, combine all data frames
    ## Format people
    ## "sp_id","sp_hh_id","serialno","stcotrbg","age","sex","race","sporder","relate","sp_school_id","sp_work_id"
    ## 164091696,14350288,1,"420035094001",24,1,1,1,0,,513967705
    people_out_df = people_df %>%
        mutate(sp_id = sprintf("%s%02d",as.character(HHID),PERNUM), sp_hh_id = HHID,
               serialno = SERIAL, age = AGE, sex = SEX, race = 1, sporder = PERNUM, relate = RELATE, sp_school_id = SCHOOL_ID, sp_work_id = WORKPLACE_ID) %>%
        dplyr::select(sp_id, sp_hh_id, serialno, stcotrbg, age, sex, race, sporder, relate, sp_school_id, sp_work_id)%>%
        arrange(sp_hh_id, sporder)

    localidad_name_list = schools_df %>%
        dplyr::select(NumberLocalidad, NameLocalidad) %>%
        group_by(NumberLocalidad) %>%
        summarize(NameLocalidad = NameLocalidad[1]) %>%
        ungroup()        
    
    ## Format public schools
    schools_out_df = schools_df %>%
        group_by(SchoolCode, SchoolName, NumberLocalidad,NameLocalidad, SchoolType, SchoolAddress, longitude, latitude, Zone, Income) %>%
        summarize(N = n())%>%
        ungroup() %>%
        left_join(schools_df %>%
                  filter(MaxAge <= 4) %>%
                  group_by(SchoolCode)  %>%
                  summarize(prek = sum(Students)) %>%
                  ungroup(), by = "SchoolCode")%>%   
        left_join(schools_df %>%
                  filter(MaxAge == 5 & MinAge == 5) %>%
                  group_by(SchoolCode)  %>%
                  summarize(kinder = sum(Students))  %>%
                  ungroup(), by = "SchoolCode") %>%
        left_join(schools_df %>%
                  filter(MaxAge <= 18 & MinAge >= 6) %>%
                  group_by(SchoolCode)  %>%
                  summarize(gr01_gr12 = sum(Students))  %>%
                  ungroup(), by = "SchoolCode"
                  ) %>%
        mutate(schooltype = 0)


    ## Format private schools
    priv_schools_out_df = priv_schools_df %>%
        left_join(localidad_name_list, by = "NumberLocalidad") %>%
        group_by(SchoolCode, SchoolName, NumberLocalidad,NameLocalidad, SchoolType, SchoolAddress, longitude, latitude, Zone, Income) %>%
        summarize(N = n())%>%
        ungroup() %>%
        left_join(priv_schools_df %>%
                  filter(MaxAge <= 4) %>%
                  group_by(SchoolCode)  %>%
                  summarize(prek = sum(Students)) %>%
                  ungroup(), by = "SchoolCode") %>%
        left_join(priv_schools_df %>%
                  filter(MaxAge == 5 & MinAge == 5) %>%
                  group_by(SchoolCode)  %>%
                  summarize(kinder = sum(Students))  %>%
                  ungroup(), by = "SchoolCode") %>%   
        left_join(priv_schools_df %>%
                  filter(MaxAge <= 18 & MinAge >= 6) %>%
                  group_by(SchoolCode)  %>%
                  summarize(gr01_gr12 = sum(Students))  %>%
                  ungroup(), by = "SchoolCode"
                  ) %>%
        mutate(schooltype = 1)

    
    schools_out = bind_rows(schools_out_df, priv_schools_out_df) %>%
        mutate(ungraded = 0) %>%
        replace_na(list(prek = 0, kinder = 0, gr01_gr12 = 0)) %>%
        mutate(sp_id = sprintf("%.0f",SchoolCode), stabbr = "BOG", address = SchoolAddress, city = "Bogota", county = NameLocalidad, zipcode = Zone, zip4 = "0", nces_id = "0", total = prek + kinder + gr01_gr12, source = "SED", stco = 11001, name = SchoolName, income = Income, stcotrbg = sprintf("%d%s",11001,Zone)) %>% 
        dplyr::select(sp_id,name, stabbr, address, city, county, zipcode, zip4, nces_id, total, prek, kinder, gr01_gr12, ungraded, latitude, longitude, source, stco,income, stcotrbg, schooltype) %>%
        bind_rows(  
            college_df %>%
            mutate(schooltype = 2) %>%
            mutate(stabbr = "BOG", address = "unspecified", city = "Bogota", county = "Bogota", zipcode = Zone, zip4 = "0", nces_id = "0", total = prek + kinder + gr01_gr12 + ungraded, source = "SED", stco = 11001, stcotrbg = sprintf("%d%s",11001,Zone), income = Income) %>%
            rename(latitude = LAT, longitude = LON) %>%
            dplyr::select(sp_id,name, stabbr, address, city, county, zipcode, zip4, nces_id, total, prek, kinder, gr01_gr12, ungraded, latitude, longitude, source, stco, income, stcotrbg,schooltype)
        )     
    
    ## Format workplaces
    workplaces_out = workplaces_df %>% rename(sp_id = workplace_id, workers = TotalWorkers) %>%
        dplyr::select(sp_id, workers, latitude, longitude)
    
    return(list(people_out = people_out_df, schools_out = schools_out, workplaces_out = workplaces_out))
}
