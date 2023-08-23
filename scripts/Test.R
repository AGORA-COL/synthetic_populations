for(ll in 1:length(localidad_list)){
  ind_coors = which(school_location_df$NumberLocalidad == localidad_list[ll])
  loc_unidad_catastral = filter(unidad_catastral, Localidad == localidad_list[ll])
  esc_loc_shp = esc_shp[esc_shp$SCACODIGO %in% loc_unidad_catastral$SCACODIGO,]
  tmp_school_location = filter(school_location_df, NumberLocalidad == localidad_list[ll])
  tmp_school_location <- tmp_school_location %>% dplyr::select(longitude,latitude)
  school_coor = st_as_sf(tmp_school_location, coords = c("longitude", "latitude"),crs = st_crs(esc_shp)) 

  for(ss in 1:nrow(esc_loc_shp)){
    school_esc = sf::st_join(school_coor, esc_loc_shp[ss,])
    if(length(which(!is.na(school_esc$SCACODIGO))) > 0){
      tmp_school_location$Zone[which(!is.na(school_esc$SCACODIGO))] = as.character(esc_loc_shp$SCACODIGO[ss])
    }
  }
  if(length(which(tmp_school_location$Zone == "")) > 0){
    tmp_school_location$Zone[which(tmp_school_location$Zone == "")] = as.character(esc_loc_shp$SCACODIGO[as.numeric(apply(sf::st_distance(school_coor[which(tmp_school_location$Zone == ""),], esc_loc_shp), 1,which.min))])
  }
  school_location_loc = bind_rows(school_location_loc, tmp_school_location)
}


ind_coors = which(school_location_df$NumberLocalidad == localidad_list[1])
loc_unidad_catastral = filter(unidad_catastral, Localidad == localidad_list[1])
esc_loc_shp = esc_shp[esc_shp$SCACODIGO %in% loc_unidad_catastral$SCACODIGO,]
tmp_school_location = filter(school_location_df, NumberLocalidad == localidad_list[1])
school_coor = st_as_sf(tmp_school_location, coords = c("longitude", "latitude"),crs = st_crs(esc_shp)) 


for(ss in 1:nrow(esc_loc_shp)){
  school_esc = sf::st_join(school_coor, esc_loc_shp[ss,])
  if(length(which(!is.na(school_esc$SCACODIGO))) > 0){
    tmp_school_location$Zone[which(!is.na(school_esc$SCACODIGO))] = as.character(esc_loc_shp$SCACODIGO[ss])
  }
}


head(school_esc$SCACODIGO)

plot(esc_loc_shp$geometry)
plot(school_coor$geometry)
plot(esc_loc_shp, axes = TRUE, xlim = c(xmin, xmax), ylim = c(ymin, ymax), col = "blue")
ggplot() +
  geom_sf(data = esc_loc_shp) + geom_sf(data = school_coor)
