##===============================================================#
## Functions to place households to match population density
## This function also locates other locations from OSM &
## Assigns them with proper labels
##
## Requirements:
## library(tidyverse)
## library(raster)
## library(rgdal)
## library(maptools)
## library(osmdata)
## library(sf)
## library(RCurl)
##
## Author: Guido España
## Date: 2019/04/03
##===============================================================#
#' lonlat2UTM
#' @param lonlat: a vector of longitude and latitude
#' @return  a string with the CRS details
#' @export
#' @examples lonlat2UTM(c(-5,-72.29))
#' 
lonlat2UTM = function(lonlat) {
    utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
    if(lonlat[2] > 0) {
            epsg_code = utm + 32600
    } else{
            epsg_code =  utm + 32700
    }
    return(sf::st_crs(epsg_code)$proj4string)
}

#' download_worldpop_data
#'
#' This is a wrapper of the worldpop API and curl to download worldpop data for specific countries
#' @param data_dir_dest (default = '../data/raw_data/geodata') a directory path to save the downloaded files
#' @param country_name (character) name of the country e.g., colombia
#' @param country_name_gdam (character) ISO-3 name of the country e.g., COL
#' 
#' @return The filepath of the raster file with worldpop persons per pixel information for 2015
#'
#' @export
#' @examples
#' download_worldpop_data(".", "colombia", "COL")
#'
download_worldpop_data = function(data_dir_dest = "../data/raw_data/geodata",
                                  country_name,
                                  country_name_gdam){
    
    print(file.path(data_dir_dest,country_name))
    if(dir.exists(file.path(data_dir_dest,country_name))){
        print(sprintf("WorldPop data already downloaded, if you really want to re-install it, just remove the folder: %s",
                      file.path(data_dir_dest, country_name)))
        raster_file = Sys.glob(file.path(data_dir_dest, country_name, "*UNadj.tif"))
        return(raster_file)
    }
    ## Get the ftp address to download the 7z compressed raster file
    metadata_worldpop = RCurl::getURL(sprintf("https://www.worldpop.org/rest/data/pop/pic?iso3=%s",country_name_gdam))
    world_files_url = rjson::fromJSON(metadata_worldpop)$data[[1]]$files
    print(sprintf("Worldpop data not found downloading from %s", world_files_url))
    data_dir_dest = "../data/raw_data/geodata"
    dir.create("tmpdownloadworldpop") 
    download.file(url = world_files_url, destfile = "tmpdownloadworldpop/tmpdownloadworldpop.7z")
    system("cd tmpdownloadworldpop; 7za x tmpdownloadworldpop.7z")

    ## Remove unnecessary files:
    filestocopy = Sys.glob(file.path("tmpdownloadworldpop", "*ppp*2015*UNadj*"))
    dir.create(file.path(data_dir_dest,country_name))
    file.rename(filestocopy,file.path(data_dir_dest,country_name,basename(filestocopy)))
    unlink("tmpdownloadworldpop",recursive = TRUE)
    raster_file = Sys.glob(file.path(data_dir_dest, country_name, "*UNadj.tif"))
    return(raster_file)
}

#' synthesize_locations
#'
#' The purpose of this function is to geo-locate houses and other locations using openstreetmaps and worldpop as a starting point
#' 
#' @param house_file (dataframe) synthesized houses dataframe (from synthesize_population())
#' @param moz_datadir (default = "../data/raw_data/mozdata") directory where the mosquito occurrence probabilities are saved
#' @return a dataframe (or tibble) of the synthesized buildings with their location 
#'
#' @export
#' @examples
#' synthesize_locations()
#'
synthesize_locations = function(house_file, 
                                country_name_gdam,
                                country_code,
                                city_name,
                                city_code,
                                city_levels,
                                country_shp_file,
                                school_df,
                                workplace_df,
                                urban_limit_in = 1,
                                raw_data_path = '../data/raw_data/geodata',
                                osm_retag_file = '../data/processed_data/geodata/retag_OSM_buildings.csv',
                                report_locations = FALSE, prop_houses_other_buildings = 0.5,
                                report_dir = "."){
    if(report_locations == TRUE){
        library(rmarkdown)
        if(!rmarkdown::pandoc_available()){report_locations = FALSE}
    }
    country_shp = rgdal::readOGR(country_shp_file)
    country_data_shp = country_shp@data %>% mutate(ROW = row_number())

    ## We're looking for municipalities    
    city_indx = country_data_shp %>%
        filter(ID_ESPACIA == city_code) %>%
        pull(ROW)
    
    if(length(city_indx) == 0){
        stop(sprintf("\nERROR: City %s not found\n",city_name))
    }else if(length(city_indx) > 1){
        stop(sprintf("\nERROR: Too many matches for city %s -> %d\n",city_name, length(city_indx)))
    }
    
    
    country_raster_file = download_worldpop_data(data_dir_dest = raw_data_path,
                                                 country_name = country_name,
                                                 country_name_gdam = country_name_gdam)
    if(length(country_raster_file) == 0){
        stop("country_raster_file invalid, something must've gone wrong with the worldpop download manager")
    }else if (country_raster_file == ""){
        stop("country_raster_file invalid, something must've gone wrong with the worldpop download manager")
    }    
    country_raster = raster::raster(country_raster_file)
    country_shp = spTransform(country_shp, crs(country_raster))
    houses_df = readr::read_csv(house_file)

    ## Mask the shapefile and density--------
    city_shp = country_shp[city_indx,]
    city_shp = spTransform(city_shp, CRS(proj4string(country_raster)))
    city_bbx = bbox(city_shp)
    
    print(sprintf("cropping raster with shapefile of city: %s", city_name))
    print(proj4string(country_raster))
    print(sprintf("minx: %.2f maxx: %.2f miny: %.2f maxy: %.2f\n",
                  city_bbx[1,1], city_bbx[1,2],city_bbx[2,1],city_bbx[2,2]))
    mid_x = city_bbx[1,1] + (city_bbx[1,2] - city_bbx[1,1]) / 2
    mid_y = city_bbx[2,1] + (city_bbx[2,2] - city_bbx[2,1]) / 2
    
    utm_zone = lonlat2UTM(c(mid_x,mid_y))
    print(sprintf("GRID mid points:%.2f - %.2f: %s",mid_x, mid_y, utm_zone))

    mid_pts = data.frame(lon = city_bbx[1,], lat = city_bbx[2,])
    coordinates(mid_pts) = ~ lon + lat
    proj4string(mid_pts) = proj4string(city_shp)
    mid_xycor = dplyr::as_tibble(sp::coordinates(spTransform(mid_pts, utm_zone))) %>%
        dplyr::transmute(xcor = lon, ycor = lat)
    print(mid_xycor)
    print(sprintf("X dist: %.2f Kms Y dist:%.2f Kms", (mid_xycor[2,1] - mid_xycor[1,1]) / 1000, (mid_xycor[2,2] -  mid_xycor[1,2]) / 1000))
    
    ex = raster::extent(city_bbx[1,1], city_bbx[1,2],
                    city_bbx[2,1], city_bbx[2,2])
    city_cropped = raster::crop(country_raster,ex)
    print(sum(raster::extract(city_cropped,ex), na.rm = T))
    
    city_raster = raster::mask(city_cropped,city_shp)

    ## Define a bounding box spatialpolygon for the covered area
    city_box = as.data.frame(city_bbx)
    city_xcoor <- c(city_box$min[1],city_box$max[1],city_box$max[1],city_box$min[1])
    city_ycoor <- c(city_box$min[2],city_box$min[2],city_box$max[2],city_box$max[2])       
    

    
    ## Assign houses to grid --------    
    ## Now, I have to convert the population density to a probability
    ## Then, I have to draw a cell, based on that probability
    print(sprintf("creating probability array from population density grids for %s",city_name))

    urban_limit = urban_limit_in

    prob_array = raster::getValues(city_raster) / sum(raster::getValues(city_raster), na.rm = T)
    prob_array[is.na(prob_array)] = 0
    
    urban_pr_array = prob_array; rural_pr_array = prob_array
    urban_pr_array[prob_array * sum(houses_df$HHSIZE) < urban_limit] = 0
    urban_pr_array = urban_pr_array / sum(urban_pr_array)
    min_cell_size = min(prob_array[prob_array > 0]*sum(houses_df$HHSIZE)) 

    if(min_cell_size > urban_limit){
        urban_limit = ceiling(min_cell_size)
        print(sprintf("WARNING: Urban limit is too low, adjusting to %d", urban_limit))        
    }
    rural_pr_array[prob_array * sum(houses_df$HHSIZE) >= urban_limit] = 0    
    rural_pr_array = rural_pr_array / sum(rural_pr_array)
    ##coordinates_array = raster::xyFromCell(city_raster,1:length(prob_array))
    coordinates_array = sp::coordinates(city_raster)
    cell_size = max(coordinates_array[2,] - coordinates_array[1,])
    urban_houses = dplyr::filter(houses_df, URBAN != 1)
    rural_houses = dplyr::filter(houses_df, URBAN == 1)
    
    urban_houses$CELL = sample(x = 1:length(urban_pr_array), size = nrow(urban_houses), prob = urban_pr_array, replace = T)
    rural_houses$CELL = sample(x = 1:length(rural_pr_array), size = nrow(rural_houses), prob = rural_pr_array, replace = T)
    
    ## Go through each house, find the grid and assign a random location within that grid
    urban_houses$LAT = coordinates_array[urban_houses$CELL,'y']  - cell_size / 2 + runif(nrow(urban_houses),0,cell_size)
    urban_houses$LON = coordinates_array[urban_houses$CELL,'x']  - cell_size / 2 +    runif(nrow(urban_houses),0,cell_size)

    ## Put back in those houses out of the shape file
    if(nrow(urban_houses) > 0){
        urban_pts = urban_houses
        coordinates(urban_pts) = ~ LON + LAT
        proj4string(urban_pts) = proj4string(city_shp)
        houses_in_city = sp::over(urban_pts, as(region_shp,"SpatialPolygons"))
        n_out = which(is.na(houses_in_city))
        while(length(n_out) > 0){
            urban_houses$LAT[n_out] = coordinates_array[urban_houses$CELL[n_out],'y']  
            urban_houses$LON[n_out] = coordinates_array[urban_houses$CELL[n_out],'x']
            urban_pts = urban_houses
            coordinates(urban_pts) = ~ LON + LAT
            proj4string(urban_pts) = proj4string(city_shp)
            houses_in_city = sp::over(urban_pts, as(region_shp,"SpatialPolygons"))
            n_out = which(is.na(houses_in_city))
            cat('\nurban houses outside: ',length(n_out), '\n')
        }
    }
    
    rural_houses$LAT = coordinates_array[rural_houses$CELL,'y']  - cell_size / 2 +
        runif(nrow(rural_houses),0,cell_size)
    rural_houses$LON = coordinates_array[rural_houses$CELL,'x'] - cell_size / 2 +
        runif(nrow(rural_houses),0,cell_size)
    
    ## Put back in those houses out of the shape file
    rural_pts = rural_houses
    if(nrow(rural_houses) > 0){
        coordinates(rural_pts) = ~ LON + LAT
        proj4string(rural_pts) = proj4string(city_shp)
        houses_in_city = sp::over(rural_pts, as(region_shp,"SpatialPolygons"))
        n_out = which(is.na(houses_in_city))
        while(length(n_out) > 0){
            rural_houses$LAT[n_out] = coordinates_array[rural_houses$CELL[n_out],'y']  
            rural_houses$LON[n_out] = coordinates_array[rural_houses$CELL[n_out],'x']
            rural_pts = rural_houses
            coordinates(rural_pts) = ~ LON + LAT
            proj4string(rural_pts) = proj4string(city_shp)
            houses_in_city = sp::over(rural_pts, as(region_shp,"SpatialPolygons"))
            n_out = which(is.na(houses_in_city))
            cat('\nrural houses outside: ',length(n_out), '\n')
        }
    }
        
    houses_df = bind_rows(urban_houses, rural_houses)

    ## Find other buildings from OSM --------
    final_structures = c("HOUSE", "COMMERCIAL", "OTHER",
                         "EDUCATION", "RELIGION", "INSTITUTIONS",
                         "HEALTH", "LEISURE")
    
    buildings_osm = osmplotr::extract_osm_objects(key = "building", bbox = city_bbx, sf = F)
    
    buildings_pts = as.data.frame(getSpPPolygonsLabptSlots(buildings_osm))
    buildings_pts$building = buildings_osm$building
    coordinates(buildings_pts) = ~ V1 + V2
    proj4string(buildings_pts) = proj4string(city_shp)
    in_city = sp::over(buildings_pts, as(region_shp,"SpatialPolygons"))
    buildings_pts = buildings_pts[!is.na(in_city) & !is.na(buildings_pts$building),]

    ##-TODO!!!#######################################################
    ## ---- What should we do when there are no buildings?????
    ##-##############################################################    
    structure_retags = readr::read_csv(osm_retag_file)
    buildings_pts@data = dplyr::left_join(buildings_pts@data, structure_retags, by = c("building" = "Structure"))

    ## 1. Calculate proportions of building categories & expected number of buildings
    initial_building_proportions  = dplyr::filter(as_tibble(buildings_pts@data), !is.na(NewTag)) %>%  
        dplyr::group_by(NewTag) %>% dplyr::tally() %>% dplyr::arrange(desc(n)) %>% dplyr::ungroup() %>%
        dplyr::mutate(Nprop = n / sum(n))
    
    building_proportions  = dplyr::filter(as_tibble(buildings_pts@data), NewTag != "HOUSE" & !is.na(NewTag)) %>%  
        group_by(NewTag) %>% tally() %>% arrange(desc(n)) %>% ungroup()

    for(ss in final_structures){
        if(ss != "HOUSE" && !(ss %in% building_proportions$NewTag)){
            building_proportions = dplyr::bind_rows(building_proportions, data.frame(NewTag = as.character(ss), n = as.integer(1)))
        }
    }

    number_schools = nrow(school_df)
    schools_buildings = filter(building_proportions, NewTag == "EDUCATION")
    schools_buildings$n = number_schools
    building_proportions = filter(building_proportions, NewTag != "EDUCATION")
    
    ## Make sure that we have enough buildings and houses !!!
    expected_workplaces = length(which(workplace_df$capacity > 1)) - number_schools # capacity one will be for houses

    
    if(sum(building_proportions$n) < expected_workplaces){
        building_proportions$n = floor((building_proportions$n / sum(building_proportions$n)) * expected_workplaces)
    }
    
    if(sum(building_proportions$n) < expected_workplaces){
        tmp_size = table(sample(1:nrow(building_proportions), size = expected_workplaces - sum(building_proportions$n), replace = T))
        building_proportions$n[as.numeric(names(tmp_size))] = building_proportions$n[as.numeric(names(tmp_size))] + as.numeric(tmp_size)
    }
    building_proportions = bind_rows(building_proportions, schools_buildings)
    
    building_proportions$Nprop = building_proportions$n / sum(building_proportions$n)
    expected_buildings = nrow(houses_df) + sum(building_proportions$n)        
    
    if(expected_buildings > nrow(buildings_pts@data)){
        print(sprintf("Generating new buildings because there are not enough from OSM %s",city_name))
        ## Create new buildings with synthetic coordinates
        expected_new_buildings = expected_buildings - nrow(buildings_pts@data)        
        new_buildings_cells = sample(x = 1:length(prob_array), size = expected_new_buildings, prob = prob_array, replace = T)
        new_buildings_lat = coordinates_array[new_buildings_cells,'y']  - cell_size / 2 +
            runif(expected_new_buildings,0,cell_size)
        new_buildings_lon = coordinates_array[new_buildings_cells,'x']  - cell_size / 2 +
            runif(expected_new_buildings,0,cell_size)
        
        new_buildings_geo = data.frame(Lon = new_buildings_lon, Lat = new_buildings_lat,
                                       building = "yes", NewTag = NA)
        
        new_buildings_pts = new_buildings_geo
        coordinates(new_buildings_pts) = ~ Lon + Lat
        proj4string(new_buildings_pts) = proj4string(city_shp)
        ## Put back in those houses out of the shape file
        buildings_in_city = sp::over(new_buildings_pts, as(region_shp,"SpatialPolygons"))
        n_out = which(is.na(buildings_in_city))
        
        while(length(n_out) > 0){
            new_buildings_geo$Lat[n_out] = coordinates_array[new_buildings_cells[n_out],'y']  
            new_buildings_geo$Lon[n_out] = coordinates_array[new_buildings_cells[n_out],'x']
            rural_pts = rural_houses
            new_buildings_pts = new_buildings_geo
            coordinates(new_buildings_pts) = ~ Lon + Lat
            proj4string(new_buildings_pts) = proj4string(city_shp)
            buildings_in_city = sp::over(new_buildings_pts, as(region_shp,"SpatialPolygons"))
            n_out = which(is.na(buildings_in_city))
            cat('\nbuildings outside: ',length(n_out), '\n')
        }        
        buildings_pts = spRbind(buildings_pts, new_buildings_pts)    
    }

    if(nrow(houses_df) > length(which(buildings_pts@data$NewTag == "HOUSE"))){
        newhouses_ind = sample(which(is.na(buildings_pts@data$NewTag)),
                               nrow(houses_df) - length(which(buildings_pts@data$NewTag == "HOUSE")))
        buildings_pts@data$NewTag[newhouses_ind] = "HOUSE"
    }else if(nrow(houses_df) < length(which(buildings_pts@data$NewTag == "HOUSE"))){
        newNA_ind = sample(which(buildings_pts@data$NewTag == "HOUSE"),
                           length(which(buildings_pts@data$NewTag == "HOUSE")) - nrow(houses_df))
        buildings_pts@data$NewTag[newNA_ind] = NA
    }

    print(sprintf("Retagging buildings for %s",city_name))
    ## 2. Re-label the remaining buildings to match the final-structures list
    ## At least have one of each category before re-tagging
    
    building_empty_indx = which(is.na(buildings_pts@data$NewTag))

    building_initial_prop = group_by(buildings_pts@data, NewTag) %>% summarize(n = n()) %>% filter(NewTag != "HOUSE", !is.na(NewTag))

    building_proportions = left_join(building_proportions, building_initial_prop, by = "NewTag", suffix = c("",".y")) %>%
        replace_na(list(n.y = 0)) 
    
    building_proportions$Nprop = (building_proportions$n / sum(building_proportions$n)) * length(building_empty_indx)
    newtags_vector = rep(building_proportions$NewTag, building_proportions$Nprop)

    buildings_pts@data$NewTag[sample(building_empty_indx, size = length(newtags_vector))] = newtags_vector
    buildings_pts@data$NewTag[is.na(buildings_pts@data$NewTag)] = sample(
        newtags_vector, size = length(which(is.na(buildings_pts@data$NewTag))))

    ## Re distribute schools in case they don't match
    adjusted_proportions = group_by(buildings_pts@data, NewTag) %>% summarize(n = n())

    if(adjusted_proportions$n[adjusted_proportions$NewTag == "EDUCATION"] > number_schools){
        school_indx = which(buildings_pts@data$NewTag == "EDUCATION")
        new_indx = sample(school_indx, size = adjusted_proportions$n[adjusted_proportions$NewTag == "EDUCATION"] - number_schools, replace = F)
        tmp_proportions = filter(adjusted_proportions, NewTag != "HOUSE", NewTag != "EDUCATION")
        buildings_pts@data$NewTag[new_indx] = sample(tmp_proportions$NewTag, size = length(new_indx), replace = T)
    }else if (adjusted_proportions$n[adjusted_proportions$NewTag == "EDUCATION"] < number_schools){
        no_school_indx = which(buildings_pts@data$NewTag != "EDUCATION" & buildings_pts@data$NewTag != "HOUSE")
        new_indx = sample(no_school_indx, size = number_schools - adjusted_proportions$n[adjusted_proportions$NewTag == "EDUCATION"], replace = F)
        buildings_pts@data$NewTag[new_indx] = "EDUCATION"
    }
    adjusted_proportions = group_by(buildings_pts@data, NewTag) %>% summarize(n = n())
    print(colnames(houses_df))


    ## Export houses
    ## 3. Create list of buildings and add to houses data frame
    ## FRED format:
    ## "sp_id","serialno","stcotrbg","hh_race","hh_income","hh_size","hh_age","latitude","longitude"
    ## 11815529,1,"420030103002",1,3000,1,24,40.4379396,-79.9919442
    city_houses = as_tibble(houses_df) %>% mutate(landuse = "HOUSE") %>%
        dplyr::mutate(longitude = LON, latitude = LAT, sp_id = HHID, stcotrbg = as.character(HHID),
               serialno = SERIAL,hh_race = 1, hh_size = HHSIZE, hh_age = AGE,hh_income = 1) %>%
        dplyr::select(sp_id, serialno,stcotrbg, hh_race, hh_income, hh_size, hh_age, latitude, longitude)

    ## Export schools
    ## FRED FORMAT:
    ## "sp_id","name","stabbr","address","city","county","zipcode","zip4","nces_id","total","prek","kinder","gr01_gr12","ungraded","latitude","longitude","source","stco"
    ## "450140545","UNNAMED450140545","PA",,,"Allegheny",,,"A42003",875,0,62,813,0,40.572011,-80.028754,"FRED_web","42003"

    tmp_schools = sample_n(school_df, size = nrow(school_df), replace = F)
    city_schools = as_tibble(sp::coordinates(buildings_pts)) %>%
        rename(longitude = V1, latitude = V2) %>%
        mutate(landuse = buildings_pts@data$NewTag) %>%
        dplyr::filter(landuse == "EDUCATION") %>%
        bind_cols(tmp_schools) %>%
        mutate(sp_id = school_id, stabbr = city_name, address = NA, city = city_name, county = city_name, zipcode = NA, zip4 = NA, nces_id = NA, total = prek + kinder + gr01_gr12 + ungraded, source = "MINEDUCACION", stco = city_code) %>%
        dplyr::select(sp_id,name, stabbr, address, city, county, zipcode, zip4, nces_id, total, prek, kinder, gr01_gr12, ungraded, latitude, longitude, source, stco)

    
    ## Export workplaces
    ## FRED FORMAT:
    ## "sp_id","workers","latitude","longitude"
    ## "513955410",2,40.4132047,-79.8218767    

    tmp_wk = sample_n(filter(workplace_df, capacity > 1), size = length(which(workplace_df$capacity > 1)), replace = F)
    
    city_workplaces = as_tibble(sp::coordinates(buildings_pts)) %>%
        rename(longitude = V1, latitude = V2) %>%
        mutate(landuse = buildings_pts@data$NewTag) %>%
        dplyr::filter(landuse != "HOUSE") %>%
        bind_cols(tmp_wk)
    
    city_workplaces = sample_n(city_houses, size = length(which(workplace_df$capacity == 1))) %>%
        mutate(landuse = "SELFWORK") %>%
        bind_cols(filter(workplace_df, capacity == 1)) %>%
        bind_rows(city_workplaces) %>%
        mutate(workers = capacity, sp_id = workplace_id) %>%
        dplyr::select(sp_id, workers, latitude, longitude)

    print('passed city locations\n')
        
    ## ------------------------------------------------------
    ## CREATE REPORT
    ## ------------------------------------------------------    
    if(report_locations == TRUE){
        save(city_raster,country_name,city_name,
             city_shp,
             rural_houses,
             urban_houses,
             city_workplaces,
             city_schools,
             buildings_pts,
             file = "synth_tmp_locations.RData")
        if(report_dir == ""){
            report_dir = "."
        }else if(report_dir != "."){
            dir.create(report_dir)
        }
        rmarkdown::render(
                       input="./synthetic_locations_report_template.rmd",
                       output_file = c(sprintf("%s/synth_locations_report_%s%d.pdf",
                                               report_dir,country_name,city_code)
                                       ),
                       output_format = c("pdf_document"))
        ##unlink("synth_tmp_locations.RData")
    }    
    return(list(city_houses = city_houses, city_workplaces = city_workplaces, city_schools = city_schools))
}


