get_lon_lat_lims <- function(Data) {
  
  
  # DESCRIPTION:
  #   This function obtains maximum and minimum latitude and longitude values
  #   from input data
  #
  # USAGE:
  #   [lon_lim, lat_lim, Data] = get_lon_lat_lims(Data)
  #
  # PREREQUISITE: 
  #   None
  #
  # INPUTS:
  #   Data     : a struct that must contain at least LONGITUDE and LATITUDE
  #              fields
  #
  # OUTPUTS:
  #   lon_lim  : a 2-element vector with minimum,maximum longitudes
  #              (normally using a range of -180..180 degrees)
  #   lat_lim  : a 2-element vector with minimum,maximum latitudes
  #   Data     : if all points are within 30 degrees of 180W/E, a field
  #              ALT_LON is added that uses a 0..360 range instead;
  #              it is unchanged from the input otherwise
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
  
  
  
  
  floats = names(Data)
  nfloats = length(floats)
  
  # NOTE: assuming -180..180 convention for longitude
  lon_lim = c(180, -180)
  lat_lim = c(90, -90)
  
  for ( i in 1:nfloats ) {
    lon_lim[1] = min(lon_lim[1], Data[[floats[i]]]$LONGITUDE, na.rm=T)
    lon_lim[2] = max(lon_lim[2], Data[[floats[i]]]$LONGITUDE, na.rm=T)
    lat_lim[1] = min(lat_lim[1], Data[[floats[i]]]$LATITUDE, na.rm=T)
    lat_lim[2] = max(lat_lim[2], Data[[floats[i]]]$LATITUDE, na.rm=T)
  }
  
  # special treatment for floats that cross the dateline
  if ( lon_lim[1] <= -150 & lon_lim[2] >= 150 ) {
    other_pts = 0
    for ( i in 1:nfloats ) {
      other_pts = other_pts + length(which(Data[[floats[i]]]$LONGITUDE > -150 & 
                                           Data[[floats[i]]]$LONGITUDE < 150))
    }
    # if all points are within 30 degrees of the dateline
    if ( other_pts == 0 ) {
      lon_lim = c(360, 0)
      for ( i in 1:nfloats ) {
        Data[[floats[i]]]$ALT_LON = Data[[floats[i]]]$LONGITUDE
        
        Data[[floats[i]]]$ALT_LON[Data[[floats[i]]]$ALT_LON < 0] = 
            Data[[floats[i]]]$ALT_LON[Data[[floats[i]]]$ALT_LON < 0] + 360
        
        lon_lim[1] = min(lon_lim[1], Data[[floats[i]]]$ALT_LON, na.rm=T)
        lon_lim[2] = max(lon_lim[2], Data[[floats[i]]]$ALT_LON, na.rm=T)
      }
    }
  }
  
  return(list(lon_lim=lon_lim, lat_lim=lat_lim, Data=Data))
  
}