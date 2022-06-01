select_profiles_per_type <- function(Profiles,
                                         lon_lim=c(-180,180), 
                                         lat_lim=c(-90, 90), 
                                         start_date="1995-01-01", 
                                         end_date="2038-01-19",
                                         sensor=NULL,
                                         ocean=NULL) {
  
  # DESCRIPTION:
  #   This function returns the IDs of floats that match
  #   the given criteria (spatial, temporal, sensor availability). It is a
  #   helper function for select_profiles and does not select by all
  #   criteria specified by the user with arguments to select_profiles.
  #   Note: due to inconsistencies between index and Profiles files, selecting
  #   by data mode  is not performed in the first round, only in the second
  #   round (in select_profiles)
  #
  # PREREQUISITE: 
  #   Globals Sprof and Setting
  #
  # INPUTS:
  #   lon_lim    : Longitude limits (a two element vector)
  #   lat_lim    : Latitude limits (a two element vector)
  #              * Longitude can be input in either the -180 to 180 degrees
  #              format or 0 to 360 degrees format
  #   start_date : start date
  #   end_date   : end date
  #              * Dates should be in one of the following formats:
  #              "YYYY-MM-DD HH:MM-SS" or "YYYY-MM-DD"
  #
  #   sensor : 'SENSOR_TYPE': By default, all floats within the lon/lat/time
  #             limits are considered. This option allows the selection by 
  #             sensor type. Available are: PRES, PSAL, TEMP, DOXY, BBP,
  #             BBP470, BBP532, BBP700, TURBIDITY, CP, CP660, CHLA, CDOM,
  #             NITRATE, BISULFIDE, PH_IN_SITU_TOTAL, DOWN_IRRADIANCE,
  #             DOWN_IRRADIANCE380, DOWN_IRRADIANCE412, DOWN_IRRADIANCE443, 
  #             DOWN_IRRADIANCE490, DOWN_IRRADIANCE555, DOWN_IRRADIANCE670, 
  #             UP_RADIANCE, UP_RADIANCE412, UP_RADIANCE443, UP_RADIANCE490,
  #             UP_RADIANCE555, DOWNWELLING_PAR, DOXY2, DOXY3
  #             (Currently, only one sensor type can be selected.)
  #   ocean :   Valid choices are 'A' (Atlantic), 'P' (Pacific), and
  #             'I' (Indian). This selection is in addition to the specified
  #             longitude and latitude limits. (To select all floats and 
  #             profiles from one ocean basin, leave lon_lim and lat_lim
  #             empty.)
  #
  # OUTPUTS:
  #   float_ids   : array with the WMO IDs of all matching floats
  #
  #
  #
  # AUTHORS:
  #   Marin Cornec (NOAA-PMEL), Yibin Huang (NOAA-PMEL), 
  #   Quentin Jutard (OSU ECCE TERRA), Raphaelle Sauzede (IMEV) and 
  #   Catherine Schmechtig (OSU ECCE TERRA).
  #
  # CITATION:
  #   M. Cornec, Y. Huang, Q. Jutard, R. Sauzede, and C. Schmechtig, 2022. 
  #   OneArgo-R: A R toolbox for accessing and visualizing Argo data.
  #   Zenodo. XXXXX
  #
  # LICENSE: oneargo_r_license.m
  #
  # DATE: JUNE 1, 2022  (Version 1.0.1)
  
  float_ids<-list()
  
  # FORMAT LATITUDE AND LONGITUDE LIMITS
  # Wrap lon to -180..180 deg
  lon_lim[lon_lim > 180] = lon_lim[lon_lim > 180] - 360 
  lon_lim[lon_lim < -180] = lon_lim[lon_lim < -180] + 360
  
  # ADJUST INPUT DATES TO DATENUM FORMAT
  dn1 = as.POSIXct(start_date, tz="UTC")
  dn2 = as.POSIXct(end_date, tz="UTC")
  
  # GET INDEX OF PROFILES WITHIN USER-SPECIFIED GEOGRAPHIC POLYGON
  if ( lon_lim[1] > lon_lim[2] ) { # crossing the dateline
    lonv1 = c(lon_lim[1], 180)
    lonv2 = c(-180, lon_lim[2])
    inpoly =  ( (Profiles$lon>lonv1[1] & Profiles$lon<lonv1[2]) | 
                  (Profiles$lon>lonv2[1] & Profiles$lon<lonv2[2]) ) & 
      (Profiles$lat>lat_lim[1] & Profiles$lat<lat_lim[2])
  } else {
    inpoly = (Profiles$lon>lon_lim[1] & Profiles$lon<lon_lim[2] & 
                Profiles$lat>lat_lim[1] & Profiles$lat<lat_lim[2])
  }
  
  if(any(inpoly)==F | length(inpoly)==0 ){
    warning('no matching profile found', sensor)
  }
  
  # Find index of dates that are within the time window
  
  indate_poly = (Profiles$date[inpoly] >= dn1 & Profiles$date[inpoly] <= dn2)
  
  # Now create an indate array of TRUE/FALSE that has the same
  # size as inpoly so that it can be used in the & operations below
  
  indate = rep(FALSE,length(inpoly))
  all_floats = 1:length(inpoly)
  sel_floats_space = all_floats[inpoly]
  indate[sel_floats_space[indate_poly]]<-TRUE
  
  # SELECT BY SENSOR
  if ( is.null(sensor) ) {
    has_sensor = rep(TRUE, length(indate)) # no sensor was selected
  } else {
    has_sensor = grepl(sensor, Profiles$sens)
  }
  if(any(has_sensor)==F){
    warning('no data found for sensor ', sensor)
  }
  
  # SELECT BY OCEAN BASIN
  if ( is.null(ocean) ) {
    is_ocean = rep(TRUE, length(inpoly)) # no ocean was selected
  } else {
    is_ocean = grepl(ocean, Profiles$ocean)
  }
  
  # perform selection
  profiles = which(inpoly & indate & has_sensor & is_ocean
                   )
  float_ids = unique(Profiles$wmo[profiles])
  
  if(length(float_ids)==0){
    float_ids = list()
  }
  
  
  
  return(float_ids=float_ids)
  
  
}

