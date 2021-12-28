select_profiles <- function(lon_lim=c(-180,180), lat_lim=c(-90, 90), 
                            start_date="1995-01-01", end_date="2038-01-19",
                            outside="none", sensor=NULL) {
  # select_profiles  
  # This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  # It returns the indices of profiles and floats that match the given
  # criteria (spatial, temporal, sensor availability).
  #
  # Inputs:
  #
  # lon_lim    : Longitude limits
  # lat_lim    : Latitude limits
  #            * Longitude can be input in either the -180 to 180 degrees
  #            format or 0 to 360 degrees format
  # start_date : start date
  # end_date   : end date
  #            * Dates should be in one of the following formats:
  #            "YYYY-MM-DD HH:MM-SS" or "YYYY-MM-DD"
  #
  # Optional inputs (key,value pairs):
  #
  # 'outside', 'none' 'time' 'space' 'both': By default, only float profiles
  #           that are within both the temporal and spatial constraints are
  #           returned ('none'); specify to also maintain profiles outside
  #           the temporal constraints ('time'), spatial constraints
  #           ('space'), or both constraints ('both')
  # 'sensor', 'SENSOR_TYPE': By default, all floats within the lon/lat/time
  #           limits are considered. This option allows the selection by 
  #           sensor type. Available are: DOXY, CHLA, BBP700, 
  #           PH_IN_SITU_TOTAL, NITRATE, DOWN_IRRADIANCE380,
  #           DOWN_IRRADIANCE412, DOWN_IRRADIANCE490, DOWNWELLING_PAR
  #           (Currently, only one sensor type can be selected.)
  #
  # Outputs:
  #
  #   profiles : array with the global indices of all matching profiles
  #   floats   : array with the global indices of all matching floats
  #
  # PREREQUISITE: 
  #   Globals Sprof and Setting
  #
  # CITATION:
  # BGC-Argo-R: A R toolbox for accessing and visualizing
  # Biogeochemical Argo data,
  #
  # AUTHORS: 
  # M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), 
  # R. Sauzede (IMEV) and C. Schmechtig (OSU ECCE TERRA),
  #
  # Adapted from the Matlab toolbox BGC-Argo-Mat:  https://doi.org/10.5281/zenodo.4971318
  # (H. Frenzel, J. Sharp, A. Fassbender (NOAA-PMEL),
  # J. Plant, T. Maurer, Y. Takeshita (MBARI), D. Nicholson (WHOI),
  # and A. Gray (UW))
  
  # Update 24 June 2021


  # set defaults
  profiles = NULL # will be assigned if successful
  floats = NULL # will be assigned if successful

  # check if specified sensor exists
  if ( ! is.null(sensor) ) {
    if ( ! sensor %in% Setting$avail_vars ) {
      warning("unknown sensor: ", sensor)
    }
  }

  # FORMAT LATITUDE AND LONGITUDE LIMITS
  # Wrap lon to -180..180 deg
  lon_lim[lon_lim > 180] = lon_lim[lon_lim > 180] - 360 
  lon_lim[lon_lim < -180] = lon_lim[lon_lim < -180] + 360
  
  # ADJUST INPUT DATES TO DATENUM FORMAT
  dn1 = as.POSIXct(start_date, tz="UTC")
  dn2 = as.POSIXct(end_date, tz="UTC")
  
  # GET INDEX OF PROFILES WITHIN USER-SPECIFIED GEOGRAPHIC POLYGON
  if ( lon_lim[1] > lon_lim[2] ) { # crossing the dateline
    lonv1 = c(lonv[1], 180)
    lonv2 = c(-180, lonv[2])
    inpoly =  ( (Sprof$lon>lonv1[1] & Sprof$lon<lonv1[2]) | 
                (Sprof$lon>lonv2[1] & Sprof$lon<lonv2[2]) ) & 
                      (Sprof$lat>lat_lim[1] & Sprof$lat<lat_lim[2])
  } else {
    inpoly = (Sprof$lon>lon_lim[1] & Sprof$lon<lon_lim[2] & 
              Sprof$lat>lat_lim[1] & Sprof$lat<lat_lim[2])
  }
  
  # Find index of dates that are within the time window
  indate = (Sprof$date >= dn1 & Sprof$date <= dn2)
  
  # SELECT BY SENSOR
  if ( is.null(sensor) ) {
    has_sensor = rep(TRUE, length(indate)) # no sensor was selected
  } else {
    has_sensor = grepl(sensor, Sprof$sens)
  }
  
  if ( sum(has_sensor) == 0 ) {
    warning("no data found for sensor", sensor)
  } else if ( outside == 'none' ) {
    profiles = which(inpoly & indate & has_sensor)
  } else {
    # identify all profiles of all those floats that have at least
    # one profile within given time and space constraints
    use_wmo = unique(Sprof$wmo[which(inpoly & indate & has_sensor)])
    use_idx = ( Sprof$wmo %in% use_wmo )
    # now apply the given constraints
    if ( outside == 'time' ) { # must meet space constraint
      profiles = which(inpoly & use_idx & has_sensor);
    } else if ( outside == 'space' ) { # must meet time constraint
      profiles = which(indate & use_idx & has_sensor);
    } else if ( outside == 'both' ) { # no time or space constraint
      profiles = which(use_idx & has_sensor);
    } else {
      warning('no such setting for "outside": ', outside)
    }
  }

  if (length(profiles) > 0 ) {
    floats = unique(Sprof$wmo[profiles])
  }

  return(list(profiles=profiles, floats=floats))
}
