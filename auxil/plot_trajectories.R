
plot_trajectories <- function(Data, 
                              color,
                              title=NULL) {
  # DESCRIPTION:
  # This function plots the trajectories of one or more specified float(s).
  #
  # PREREQUISITE: Sprof file for the specified float(s) must exist locally.
  #
  # INPUTS:
  #   Data  : struct that must contain LONGITUDE and LATITUDE fields
  #   color : either 'multiple' (different colors for different floats),
  #           or any standard R color descriptor ('red', 'blue', 'green',
  #           'black' etc.) (all trajectories will be plotted in the same color)
  #   title : title of the plot
  #
  # UPDATE RECORD: 
  #   Version 1:   June 2021 
  #   Version 1.1: January 2022 
  #
  # CITATION:
  #   M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), R. Sauzede (IMEV) and 
  #   C. Schmechtig (OSU ECCE TERRA), 2021.
  #   BGC-Argo-R: A R toolbox for accessing and visualizing Biogeochemical Argo data. 
  #   Zenodo. http://doi.org/10.5281/zenodo.5028139
  
  
  
  # Determine which floats have been imported
  floats = names(Data)
  nfloats = length(floats)
  
  limits = get_lon_lat_lims(Data)
  lon_lim = limits$lon_lim
  lat_lim = limits$lat_lim
  Data = limits$Data
  
  # Set lat and lon limits
  latlim = c(lat_lim[1]-5, lat_lim[2]+5)
  lonlim = c(lon_lim[1]-5, lon_lim[2]+5)
  # Adjust limits outside range to minimum and maximum limits
  latlim[latlim < -90] = -90
  latlim[latlim >  90] =  90
  # use 0..360 range if all points are within 30 degrees of the dateline
  if ( "ALT_LON" %in% names(Data[[1]]) ) {
    lonlim[lonlim < 0] = 0
    lonlim[lonlim >  360] =  360
    use_alt_lon = TRUE
  } else { # using a range of -180..180
    lonlim[lonlim < -180] = -180
    lonlim[lonlim >  180] =  180
    use_alt_lon = FALSE
  }
  
  # get the land map
  world = map_data("world")
  
  if ( use_alt_lon ) {
    world$long[world$long<0] = world$long[world$long<0] + 360
    # Note : this messes up the map when plotted over the entire range but
    #        ALT_LON is only supposed to be used when all the data is within
    #        30 degrees of the dateline
  }
  
  # create a dataframe for ggplot
  df = NULL
  for (i in 1:nfloats) {
    df_i = data.frame(LATITUDE=Data[[i]]$LATITUDE[1,])
    
    if ( use_alt_lon ) {
      df_i$LONGITUDE = Data[[i]]$ALT_LON[1,]
    } else {
      df_i$LONGITUDE = Data[[i]]$LONGITUDE[1,]
    }
    
    df_i$WMO = floats[i]
    
    df = rbind(df, df_i)
  }
  
  g1 = ggplot(df, mapping=aes(x=LONGITUDE, y=LATITUDE, group=WMO)) +
    theme_bw() +
    ggtitle(title)+
    geom_polygon(data=world, aes(x=long, y=lat, group=group), 
                 fill="#dddddd")

  if (color=='multiple') {
    g1 = g1 +
      geom_path(aes(color=WMO)) +
      geom_point(aes(color=WMO))
   } else {
    g1 = g1 +
      geom_path(color=color) +
      geom_point(color=color)
   }
  
  g1 = g1 +
    coord_cartesian(xlim=lonlim, ylim=latlim) +
    theme(axis.title.x = element_text(size=12, colour="black", face="bold", family="serif"), 
          axis.text.x = element_text(size=12, colour="black", face="bold", family="serif"),
          axis.title.y = element_text(size=12, colour="black", face="bold", family="serif"), 
          axis.text.y = element_text(size=12, colour="black", face="bold", family="serif") ) +
    labs(x = expression(bold(Longitude~"("~"degrees"~E~")")),
         y = expression (bold(Latitude~"("~"degrees"~N~")")) ) +
    theme(legend.text = element_text(size=12, face='bold', family="serif")) +
    labs(colour=expression(bold("WMO ID")),size=5, family="serif")
   
  return(g1)
  
}
