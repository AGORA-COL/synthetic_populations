
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

#' synthesize_locations_zones
#'
#' The purpose of this function is to geo-locate houses and other locations using openstreetmaps and worldpop as a starting point
#' 
#' @param house_file (dataframe) synthesized houses dataframe (from synthesize_population())
#' @param moz_datadir (default = "../data/raw_data/mozdata") directory where the mosquito occurrence probabilities are saved
#' @return a dataframe (or tibble) of the synthesized buildings with their location 
#'
#' @export
#' @examples
#' synthesize_locations_zones()
#'
synthesize_locations_zones = function(house_file, 
                                country_name_gdam,
                                country_code,
                                city_name,
                                city_code,
                                city_levels,
                                country_shp_file,
                                raw_data_path = '../data/raw_data/geodata',
                                zones_shp = NULL,
                                block_shp = NULL){
    
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

    ##mutate(ZONE = tolower(iconv(ZONE,from="UTF-8",to="ASCII//TRANSLIT")))


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

    city_box_shp = SpatialPolygons(list(Polygons(list(Polygon(cbind(city_xcoor, city_ycoor))),1)))
    proj4string(city_box_shp) = proj4string(city_shp)
    region_shp = city_shp
    
    ## Assign houses to grid --------    
    ## Now, I have to convert the population density to a probability
    ## Then, I have to draw a cell, based on that probability
    print(sprintf("creating probability array from population density grids for %s",city_name))


    zones_data_shp = zones_shp@data %>% mutate(ROW = row_number())
    ##ZONE =  tolower(iconv(Nombre_de_l,from="UTF-8",to="ASCII//TRANSLIT")))
    houses_df$Localidad = 0
    houses_df$LAT = 0
    houses_df$LON = 0
    for(zz in 1:nrow(zones_data_shp)){
        cat("\rZone: ", zz)
        zone_tmp_shp = zones_shp[zz,]
        proj4string(zone_tmp_shp) = proj4string(city_shp)
        zone_houses_ind = which(houses_df$ZONE == zones_data_shp$ZONE[zz])
        if(length(zone_houses_ind) == 0){next}
        
        houses_df$Localidad[zone_houses_ind] = as.numeric(zone_tmp_shp@data$Localidad[1])
        zone_blocks = block_shp[block_shp@data$SCACODIGO == zones_data_shp$ZONE[zz],]
        if(nrow(zone_blocks) == 0){
            zone_blocks = zone_tmp_shp
        }

        ## MAybe use tryCatch if too many errors with iter too small
        house_points = sp::spsample(zone_blocks, n = length(zone_houses_ind), "random",iter = 20)
        houses_df$LAT[zone_houses_ind] = coordinates(house_points)[,'y']
        houses_df$LON[zone_houses_ind] = coordinates(house_points)[,'x']               
    }   
    
    ## ZONE should go into stcotrbg -> 11001 + zone + 001 (nbg = 1) (11 digits)
    city_houses = as_tibble(houses_df) %>% mutate(landuse = "HOUSE") %>%
        dplyr::mutate(longitude = LON, latitude = LAT, sp_id = HHID,
                      stcotrbg = sprintf("%d%s",city_code,ZONE),
                      serialno = SERIAL,hh_race = 1, hh_size = HHSIZE, hh_age = AGE,hh_income = 1) %>%
        dplyr::select(sp_id, serialno,stcotrbg, hh_race, hh_income, hh_size, hh_age, latitude, longitude)


    
    ## Export houses
    ## 3. Create list of buildings and add to houses data frame
    ## FRED format:
    ## "sp_id","serialno","stcotrbg","hh_race","hh_income","hh_size","hh_age","latitude","longitude"
    ## 11815529,1,"420030103002",1,3000,1,24,40.4379396,-79.9919442


    ## ZONE should go into nces_id
    ## Export schools
    ## FRED FORMAT:
    ## "sp_id","name","stabbr","address","city","county","zipcode","zip4","nces_id","total","prek","kinder","gr01_gr12","ungraded","latitude","longitude","source","stco"
    ## "450140545","UNNAMED450140545","PA",,,"Allegheny",,,"A42003",875,0,62,813,0,40.572011,-80.028754,"FRED_web","42003"

    ## tmp_schools = sample_n(school_df, size = nrow(school_df), replace = F)
    ## city_schools = as_tibble(sp::coordinates(buildings_pts)) %>%
    ##     rename(longitude = V1, latitude = V2) %>%
    ##     mutate(landuse = buildings_pts@data$NewTag) %>%
    ##     dplyr::filter(landuse == "EDUCATION") %>%
    ##     bind_cols(tmp_schools) %>%
    ##     mutate(sp_id = school_id, stabbr = city_name, address = NA, city = city_name, county = city_name, zipcode = NA, zip4 = NA, nces_id = NA, total = prek + kinder + gr01_gr12 + ungraded, source = "MINEDUCACION", stco = city_code) %>%
    ##     dplyr::select(sp_id,name, stabbr, address, city, county, zipcode, zip4, nces_id, total, prek, kinder, gr01_gr12, ungraded, latitude, longitude, source, stco)

    
    ## Export workplaces
    ## FRED FORMAT:
    ## "sp_id","workers","latitude","longitude"
    ## "513955410",2,40.4132047,-79.8218767    

    ## tmp_wk = sample_n(filter(workplace_df, capacity > 1), size = length(which(workplace_df$capacity > 1)), replace = F)
    
    ## city_workplaces = as_tibble(sp::coordinates(buildings_pts)) %>%
    ##     rename(longitude = V1, latitude = V2) %>%
    ##     mutate(landuse = buildings_pts@data$NewTag) %>%
    ##     dplyr::filter(landuse != "HOUSE") %>%
    ##     bind_cols(tmp_wk)
    
    ## city_workplaces = sample_n(city_houses, size = length(which(workplace_df$capacity == 1))) %>%
    ##     mutate(landuse = "SELFWORK") %>%
    ##     bind_cols(filter(workplace_df, capacity == 1)) %>%
    ##     bind_rows(city_workplaces) %>%
    ##     mutate(workers = capacity, sp_id = workplace_id) %>%
    ##     dplyr::select(sp_id, workers, latitude, longitude)

    print('passed city locations\n')        
    return(list(city_houses = city_houses))
}


