



for(ss in 1:nrow(esc_shp)){
  ies_esc = sf::st_join(ies_coor, esc_shp[ss,])
  if(length(which(!is.na(ies_esc$SCACODIGO))) > 0){
    ies_df$Zone[which(!is.na(ies_esc$SCACODIGO))] = as.character(esc_shp$SCACODIGO[ss])
  }
}

if(length(which(ies_df$Zone == "")) > 0){
  ies_df$Zone[which(ies_df$Zone == "")] = as.character(esc_shp$SCACODIGO[as.numeric(apply(sf::st_distance(ies_coor[which(ies_df$Zone == "")], esc_shp), 1,which.min))])
}




plot(esc_loc_shp$geometry)
plot(school_coor$geometry)
plot(esc_loc_shp, axes = TRUE, xlim = c(xmin, xmax), ylim = c(ymin, ymax), col = "blue")
ggplot() +
  geom_sf(data = esc_loc_shp) + geom_sf(data = school_coor)

##2
for(ss in 1:nrow(esc_loc_shp)){
  school_esc = sf::st_join(school_coor, esc_loc_shp[ss,])
  if(length(which(!is.na(school_esc$SCACODIGO))) > 0){
    tmp_school_location$Zone[which(!is.na(school_esc$SCACODIGO))] = as.character(esc_loc_shp$SCACODIGO[ss])
  }
}
if(length(which(tmp_school_location$Zone == "")) > 0){
  tmp_school_location$Zone[which(tmp_school_location$Zone == "")] = as.character(esc_loc_shp$SCACODIGO[as.numeric(apply(sf::st_distance(school_coor[which(tmp_school_location$Zone == ""),], esc_loc_shp), 1,which.min))])
}
priv_school_location_loc = bind_rows(priv_school_location_loc, tmp_school_location)
}
