#' assign_schools_workplaces
#' Assigns students to school based on distance
#' and workers to workplaces
#' @param people_file: file with synthesized microdata for people
#' @param synth_houses: file with formatted houses
#' @param synth_schools: file with formatted schools
#' @param synth_workplaces: file with formatted workplaces
#' @return  a dataframe of synthetic people with school and workplace assigned
#' @export
#' @examples assign_school_workplace('microdata_people_file.csv','formated_houses.txt','formatted_schools.txt','formatted_workplaces.txt')
#' 
assign_schools_workplaces <- function(people_file, synth_houses, synth_schools, synth_workplaces){
    schools_df = read_csv(synth_schools)
    workplaces_df = read_csv(synth_workplaces)
    
    people_df = read_csv(people_file) %>% dplyr::select(PERNUM, SERIAL, GENDER, SEX, AGE, HHID,RELATE, WORKSTATUS, SCHOOLSTATUS) %>%
        mutate(SCHOOL_ID = NA, WORKPLACE_ID = NA) %>%
        right_join(read_csv(synth_houses) %>% dplyr::select(sp_id, latitude,longitude),by = c("HHID" = "sp_id"))
    
    ##===============================================================#
    ## 2. Assign students to schools--------
    ##===============================================================#
    ## 3.1 Universities are assigned at random, no distance involved
    ind_college_students = which(people_df$SCHOOLSTATUS == 'cs')
    ind_universities = schools_df %>% filter(ungraded > 0) %>% dplyr::select(sp_id, ungraded)
    people_df$SCHOOL_ID[ind_college_students] = sample(rep(ind_universities$sp_id, times = ind_universities$ungraded), size = length(ind_college_students), replace = F)

    ## 3.2 Schools should be distance-based
    students_df = filter(people_df, SCHOOLSTATUS == 'bs') %>%
        mutate(GRADE = ifelse(AGE < 5, "prek",
                       ifelse(AGE == 5, "kinder",
                       ifelse(AGE <= 18, "gr01_gr12"))))

    ## I think the easiest way is to locate schools and people in GRIDS, then go through each gridcell and choose students for a school

    ## 1. GRID
    patch_size = 1/120
    minlongitude = min(c(people_df$longitude, schools_df$longitude))
    maxlongitude = max(c(people_df$longitude, schools_df$longitude))
    minlatitude = min(c(people_df$latitude, schools_df$latitude))
    maxlatitude = max(c(people_df$latitude, schools_df$latitude))

    xcells = ceiling(( maxlongitude- minlongitude) / patch_size)
    ycells = ceiling((maxlatitude - minlatitude) / patch_size)
    students_assign = tibble()

    for(grade_assign in c("prek","kinder","gr01_gr12")){
        cat("\n",grade_assign,"\n")
        students_grid = matrix(0, nrow = xcells, ncol = ycells)
        schools_grid = matrix(0, nrow = xcells, ncol = ycells)
        
        tmp_students = filter(students_df, GRADE == grade_assign)
        
        tmp_schools = schools_df %>% gather(key = grade, value = capacity, c(prek,kinder,gr01_gr12)) %>%
            filter(grade == grade_assign, capacity > 0)
        ## Fill theme grids
        tmp_students$xcell = floor((tmp_students$longitude - minlongitude) / patch_size) + 1
        tmp_students$ycell = floor((tmp_students$latitude - minlatitude) / patch_size) + 1
        tmp_schools$xcell = floor((tmp_schools$longitude - minlongitude) / patch_size) + 1
        tmp_schools$ycell = floor((tmp_schools$latitude - minlatitude) / patch_size) + 1
        tmp_schools$xycell = sprintf("%d-%d",tmp_schools$xcell,tmp_schools$ycell)
        
        
        for(xx in 1:nrow(students_grid)){
            for(yy in 1:ncol(students_grid)){
                schools_grid[xx,yy] = tmp_schools %>% filter(xcell == xx, ycell == yy) %>% summarize(capacity = sum(capacity)) %>% pull(capacity)
                students_grid[xx,yy] = nrow(tmp_students %>% filter(xcell == xx, ycell == yy))
            }
        }
        
        ## Assign students
        for(xx in sample(1:nrow(students_grid), nrow(students_grid))){
            for(yy in sample(1:ncol(students_grid), ncol(students_grid))){
                ##print(sprintf("x:%d y:%d",xx,yy))
                cell_students = tmp_students %>% filter(xcell == xx, ycell == yy)
                if(nrow(cell_students) < 1){next}
                for(nn in 1:nrow(cell_students)){
                    schools_available = 0
                    cell_nn = 0
                    
                    ## Look in viccinity
                    while(schools_available < 1){
                        if(cell_nn > max(nrow(students_grid), ncol(students_grid))){
                            stop("Something went wrong with finding availability")
                        }
                        cell_nn = cell_nn + 1
                        yymin = yy - cell_nn
                        yymax = yy + cell_nn
                        xxmin = xx - cell_nn
                        xxmax = xx + cell_nn
                        if(yymin < 1){yymin = 1}
                        if(yymax > ncol(students_grid)){yymax = ncol(students_grid)}
                        if(xxmin < 1){xxmin = 1}
                        if(xxmax > nrow(students_grid)){xxmax = nrow(students_grid)}            
                        yn = seq(from = yymin, to = yymax)
                        xn = seq(from = xxmin, to = xxmax)
                        
                        schools_available = sum(schools_grid[xn,yn])
                    }
                    if(schools_available < 1){
                        stop("Something is wrong with school availability\n")
                    }
                    school_cells = expand.grid(xn = xn, yn = yn)
                    school_enrolled = sample(which(tmp_schools$xycell %in% sprintf("%d-%d",school_cells$xn, school_cells$yn) & tmp_schools$capacity > 0),1)
                    cell_students$SCHOOL_ID[nn] = tmp_schools$sp_id[school_enrolled]
                    tmp_schools$capacity[school_enrolled] = as.numeric(tmp_schools$capacity[school_enrolled]) - 1
                    schools_grid[tmp_schools$xcell[school_enrolled], tmp_schools$ycell[school_enrolled]] = schools_grid[tmp_schools$xcell[school_enrolled], tmp_schools$ycell[school_enrolled]] - 1                        
                }
                students_assign = bind_rows(students_assign, cell_students)
            }
            cat("\rNumber of students assigned: ", nrow(students_assign), "out of: ", nrow(students_df))
        }    
    }
    cat("\n")
    people_st_df = filter(people_df, SCHOOLSTATUS != "bs") %>%
        bind_rows(students_assign)
    rm(people_df)

    ##===============================================================#
    ## 3. Assign workers to workplaces--------
    ##===============================================================#
    ## 3.2 Schools should be distance-based
    workers_df = filter(people_st_df, WORKSTATUS == "w")
    
    ## I think the easiest way is to locate schools and people in GRIDS, then go through each gridcell and choose students for a school

    ## 1. GRID
    patch_size = 1/120
    minlongitude = min(c(workers_df$longitude, workplaces_df$longitude))
    maxlongitude = max(c(workers_df$longitude, workplaces_df$longitude))
    minlatitude = min(c(workers_df$latitude, workplaces_df$latitude))
    maxlatitude = max(c(workers_df$latitude, workplaces_df$latitude))

    xcells = ceiling((maxlongitude - minlongitude) / patch_size)
    ycells = ceiling((maxlatitude - minlatitude) / patch_size)
    workers_assign = tibble()

    workers_grid = matrix(0, nrow = xcells, ncol = ycells)
    workplaces_grid = matrix(0, nrow = xcells, ncol = ycells)


    workers_df$xcell = floor((workers_df$longitude - minlongitude) / patch_size) + 1
    workers_df$ycell = floor((workers_df$latitude - minlatitude) / patch_size) + 1
    workplaces_df$xcell = floor((workplaces_df$longitude - minlongitude) / patch_size) + 1
    workplaces_df$ycell = floor((workplaces_df$latitude - minlatitude) / patch_size) + 1
    workplaces_df$xycell = sprintf("%d-%d",workplaces_df$xcell,workplaces_df$ycell)
    
    
    for(xx in 1:nrow(workers_grid)){
        for(yy in 1:ncol(workers_grid)){
            workplaces_grid[xx,yy] = workplaces_df %>% filter(xcell == xx, ycell == yy) %>% summarize(workers = sum(workers)) %>% pull(workers)
            workers_grid[xx,yy] = nrow(workers_df %>% filter(xcell == xx, ycell == yy))
        }
    }
    
    ## Assign workers
    for(xx in sample(1:nrow(workers_grid), nrow(workers_grid))){
        for(yy in sample(1:ncol(workers_grid), ncol(workers_grid))){
            ##print(sprintf("x:%d y:%d",xx,yy))
            cell_workers = workers_df %>% filter(xcell == xx, ycell == yy)
            if(nrow(cell_workers) < 1){next}
            for(nn in 1:nrow(cell_workers)){
                workplaces_available = 0
                cell_nn = 0
                
                ## Look in viccinity
                while(workplaces_available < 1){
                    if(cell_nn > max(nrow(workers_grid), ncol(workers_grid))){
                        stop("Something went wrong with finding availability for workplaces")
                    }
                    cell_nn = cell_nn + 1
                    yymin = yy - cell_nn
                    yymax = yy + cell_nn
                    xxmin = xx - cell_nn
                    xxmax = xx + cell_nn
                    if(yymin < 1){yymin = 1}
                    if(yymax > ncol(workers_grid)){yymax = ncol(workers_grid)}
                    if(xxmin < 1){xxmin = 1}
                    if(xxmax > nrow(workers_grid)){xxmax = nrow(workers_grid)}            
                    yn = seq(from = yymin, to = yymax)
                    xn = seq(from = xxmin, to = xxmax)
                    
                    workplaces_available = sum(workplaces_grid[xn,yn])
                }
                if(workplaces_available < 1){
                    stop("Something is wrong with workplace availability\n")
                }
                workplaces_cells = expand.grid(xn = xn, yn = yn)
                workplace_enrolled = sample(which(workplaces_df$xycell %in% sprintf("%d-%d",workplaces_cells$xn, workplaces_cells$yn) & workplaces_df$workers > 0),1)
                cell_workers$WORKPLACE_ID[nn] = workplaces_df$sp_id[workplace_enrolled]
                workplaces_df$workers[workplace_enrolled] = as.numeric(workplaces_df$workers[workplace_enrolled]) - 1
                workplaces_grid[workplaces_df$xcell[workplace_enrolled], workplaces_df$ycell[workplace_enrolled]] = workplaces_grid[workplaces_df$xcell[workplace_enrolled], workplaces_df$ycell[workplace_enrolled]] - 1                        
            }
            workers_assign = bind_rows(workers_assign, cell_workers)
            cat("\rNumber of workers assigned: ", nrow(workers_assign), "out of: ", nrow(workers_df))
        }
    }
    cat("\n")

    ##===============================================================#
    ## 4. Write output--------
    ##===============================================================#
    ## Finally, combine all data frames
    ## Format people
    ## "sp_id","sp_hh_id","serialno","stcotrbg","age","sex","race","sporder","relate","sp_school_id","sp_work_id"
    ## 164091696,14350288,1,"420035094001",24,1,1,1,0,,513967705
    browser()
    people_out_df = filter(people_st_df, WORKSTATUS == "nw") %>%
        bind_rows(workers_assign) %>%
        mutate(sp_id = as.numeric(sprintf("%s%02d",HHID,PERNUM)), sp_hh_id = HHID,
               serialno = SERIAL, stcotrbg = as.character(HHID), age = AGE, sex = SEX, race = 1, sporder = PERNUM, relate = RELATE, sp_school_id = SCHOOL_ID, sp_work_id = WORKPLACE_ID) %>%
        dplyr::select(sp_id, sp_hh_id, serialno, stcotrbg, age, sex, race, sporder, relate, sp_school_id, sp_work_id)%>%
        arrange(sp_hh_id)
    return(people_out_df)
}
