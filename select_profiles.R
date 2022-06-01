select_profiles <- function(lon_lim=c(-180,180), 
                            lat_lim=c(-90, 90), 
                            start_date="1995-01-01", 
                            end_date="2038-01-19",
                            outside="none", 
                            sensor=NULL,
                            ocean=NULL,
                            mode="RAD",
                            type="all") {
  
  # DESCRIPTION:
  #   This function returns the indices of profiles and floats that match
  #   the given criteria (spatial, temporal, sensor availability).
  #   It calls function initialize_argo if necessary.
  #   Prof and Sprof files that match most criteria (except data mode, if 
  #   specified) and those that have missing longitude/latitude values in the 
  #   index file are downloaded from a GDAC.
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
  # OPTIONAL INPUTS:
  #   outside = 'none' / 'time' / 'space' / 'both': By default, only float profiles
  #             that are within both the temporal and spatial constraints are
  #             returned ('none'); specify to also maintain profiles outside
  #             the temporal constraints ('time'), spatial constraints
  #             ('space'), or both constraints ('both')
  #   sensor = 'SENSOR_TYPE': By default, all floats within the lon/lat/time
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
  #   mode :    Valid modes are 'R' (real-time), 'A' (adjusted), and
  #             'D', in any combination. Only profiles with the selected
  #             mode(s) will be listed in float_profs.
  #             Default is 'RAD' (all modes).
  #             If multiple sensors are specified, all of them must be in 
  #             the selected mode(s).
  #             If 'sensor' option is not used, the 'mode' option is ignored.
  #   type :    type: Valid choices are 'bgc' (select BGC floats only),
  #            'phys' (select core and deep floats only), and 'all'
  #             (select all floats that match other criteria; the default)
  #             If type is not specified, but sensors are, then the type will
  #             be set to 'all' if only pTS (PRES, PSAL, TEMP, CNDC) sensors
  #             are specified, and to 'bgc' otherwise.
  #
  # OUTPUTS:
  #   float_ids   : array with the WMO IDs of all matching floats
  #   float_profs : cell array with the per-float indices of all matching profiles
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
  
  
  
  
  # make sure Setting is initialized
  if (exists("Setting")==F) {
    initialize_argo()
  }
  
  # check if specified sensor exists
  if ( ! is.null(sensor) ) {
    if ( ! sensor %in% Setting$avail_vars ) {
      warning("unknown sensor: ", sensor)
      Sys.sleep(3)
      sensor = NULL
    }
  }
  
  # only use mode is sensor was specified
  if (is.null(sensor)){
    mode = "ADR"
  }
  
  # check if type selection is coherent with sensor selection
  if( ! is.null(sensor)){
    bgc_sensors<-F
    for (g in sensor) {
      if(!(g %in% c("PRES","PSAL","TEMP","CNDC"))){
        bgc_sensors<-T
      }
    }
    if(bgc_sensors==T) {
      if(type=="phys") {
        warning('You specified BGC sensors and  type "phys".')
        warning('Please revise either setting!')
        stop()
      } else {
        # setting may have been 'all', 'bgc', or no setting yet
        # in any case, since BGC sensors are requested, only BGC
        # floats will be considered
        type="bgc"
      }
    }
  }
  
  # check if specified ocean is correct
  if(!is.null(ocean)){
    if(ocean != "P" & ocean != "A" & ocean != "I") {
      warning("no such ocean: ", ocean)
      ocean<-NULL
    }
  }
  
  # check if specified data modes are correct
  new_mode<-NULL
  for (i in 1:nchar(mode)){
    if(substr(mode,i,i) %in% c('R','A','D')) {
      new_mode<-c(new_mode,substr(mode,i,i))
    } else {
      warning("no such mode: ",substr(mode,i,i))
    }
  }
  
  if(is.null(new_mode)){
    mode<-c("ADR")
  } else {
    new_mode<-sort(new_mode)
    mode<-NULL
    for(i in 1:length(new_mode)){
      mode<-paste0(mode,new_mode[i])
    } 
  }
  
  # make sure Sprof is initialized
  if (exists("Sprof")==F || exists("Prof")==F) {
    initialize_argo()
  }
  
  # FORMAT LATITUDE AND LONGITUDE LIMITS
  # Wrap lon to -180..180 deg
  lon_lim[lon_lim > 180] = lon_lim[lon_lim > 180] - 360 
  lon_lim[lon_lim < -180] = lon_lim[lon_lim < -180] + 360
  
  # ADJUST INPUT DATES TO DATENUM FORMAT
  dn1 = as.POSIXct(start_date, tz="UTC")
  dn2 = as.POSIXct(end_date, tz="UTC")
  
  # select bgc and phys floats separately, then combine the results
  if (type == 'bgc' |  type== 'all'){
    bgc_float_ids = select_profiles_per_type(Sprof,
                                             lon_lim, 
                                             lat_lim, 
                                             dn1, 
                                             dn2, 
                                             sensor,
                                             ocean)
  } else {
    bgc_float_ids = NULL
  }
  
  if (type == 'phys' |  type== 'all'){
    phys_float_ids = select_profiles_per_type(Prof,
                                              lon_lim, 
                                              lat_lim, 
                                              dn1, 
                                              dn2, 
                                              sensor,ocean)
    if(is.null(phys_float_ids)==F){
      phys_float_idx<-which(Float$wmoid %in% phys_float_ids)
      phys_float_ids<-phys_float_ids[which(Float$type[phys_float_idx]=='phys')]
    }
  } else {
    phys_float_ids = NULL
  }
  
  float_ids = unique(c(bgc_float_ids, phys_float_ids))
  float_profs = list()
  
  
  if(length(float_ids)!=0){
    # download Prof and Sprof files if necessary
    good_float_ids = download_multi_floats(float_ids)
    
    # the information from the index files is only used for an initial
    # filtering of floats, the actual information from the prof/Sprof files
    # is used in a second step
    
    float_ids = good_float_ids
    
    for (fl in 1:length(good_float_ids)) {
      filename = paste(Setting$prof_dir,Float$file_name[which(Float$wmoid==good_float_ids[fl])],sep="")
      n_prof = get_dims(filename)$n_prof
      n_param = get_dims(filename)$n_param
      fl_idx = which(Float$wmoid==good_float_ids[fl])
      n_prof_exp = Float$prof_idx2[fl_idx] - Float$prof_idx1[fl_idx] + 1
      if (n_prof_exp > n_prof) {
        type<-"prof"
        if(length(grep("Sprof",filename))==1){
          type<-"Sprof"
        }
        warning(paste("The index file lists", n_prof_exp,"profiles for float",
                      good_float_ids[fl],"but the",type," file has only",n_prof,"profiles."))
      }
      
      nc<-nc_open(filename)
      lon = ncvar_get(nc, 'LONGITUDE')
      lat = ncvar_get(nc, 'LATITUDE')
      juld = ncvar_get(nc, 'JULD')
      
      if(!is.null(sensor) & mode!="ADR"){
        params = ncvar_get(nc,"PARAMETER")
        # find the index of a profile that has the most sensors available
        tmp = nchar(trimws(params))
        sum_tmp<-NULL
        for (ii in 1:n_prof){
          sum_tmp<-c(sum_tmp,sum(tmp[,1,ii]))
        }
        pidx = which.max(sum_tmp)[1]
        param_names = rep(NA,n_param)
        for(p in 1:n_param){
          param_names[p]<-trimws(params[[p,1,pidx]])
        }
        param_idx = which(param_names==sensor)
        data_mode = ncvar_get(nc,"PARAMETER_DATA_MODE")  
      }
      
      date = as.POSIXct(juld*3600*24,origin=as.Date("1950-01-01"), tz="UTC")
      
      if ( lon_lim[1] > lon_lim[2] ) { # crossing the dateline
        lonv1 = c(lon_lim[1], 180)
        lonv2 = c(-180, lon_lim[2])
        inpoly =  ((lon>lonv1[1] & lon<lonv1[2]) | 
                     (lon>lonv2[1] & lon<lonv2[2])) & 
          (lat>lat_lim[1] & lat<lat_lim[2])
      } else {
        inpoly = (lon>lon_lim[1] & lon<lon_lim[2] & 
                    lat>lat_lim[1] & lat<lat_lim[2])
      }
      
      
      indate = date >= dn1 & date <= dn2
      
      if ( is.null(sensor) ) {
        has_sensor = rep(TRUE, length(inpoly)) # no sensor was selected
      } else {
        param = ncvar_get(nc, 'PARAMETER')
        has_sensor = rep(FALSE, length(inpoly))
        for (p in 1:n_prof) {
          has_sensor[p] = any(grepl(sensor, param[,,p]))
          end
        }
      }
      nc_close(nc)
      
      if(is.null(ocean)){
        is_ocean = rep(TRUE, length(inpoly))
      } else {
        is_ocean = rep(FALSE, length(inpoly))
        for(ii in 1:length(lon)) {
          if(is.na(lon[ii]) | is.na(lat[ii])) {
            warning("NA lon or lat for float ", good_float_ids[fl], "; profile ",ii)
            next
          }
          slon_diff<-abs(Prof$lon - lon[ii])
          slat_diff<-abs(Prof$lat - lat[ii])
          ssum<-slon_diff+slat_diff
          if(Prof$ocean[which.min(ssum)[1]]==ocean){
            is_ocean[ii]<-TRUE
          }
        }
      }
      
      if (mode=="ADR") {
        has_mode = rep(TRUE, length(inpoly))
      } else {
        has_mode = rep(FALSE, length(inpoly))
        for(m in 1:nchar(mode)) {
          has_mode[which(substr(data_mode,param_idx,param_idx)==
                           substr(mode,m,m))]<- TRUE
        }
      }
      
      # now apply the given constraints
      
      if (outside == 'none' ){
        float_profs[[fl]] = which(inpoly & indate & has_sensor & 
                                    is_ocean & has_mode)
      } else if ( outside == 'time' ) { # must meet time constraint
        float_profs[[fl]] = which(inpoly & has_sensor & is_ocean & has_mode)
      } else if ( outside == 'space' ) { # must meet time constraint
        float_profs[[fl]] = which(indate & has_sensor & is_ocean & has_mode)
      } else if ( outside == 'both' ) { # no time or space constraint
        float_profs[[fl]] = which(has_sensor & is_ocean & has_mode)
      } else {
        warning('no such setting for "outside": ', outside)
      }
      
      if (length(float_profs[[fl]])==0 | is.null(float_profs[[fl]])){
        warning('no matching profiles found for float ', good_float_ids[fl])
        float_ids[which(float_ids == good_float_ids[fl])] = NA
      }
      
    }
    
    float_profs = float_profs[which(is.na(float_ids)==F)]
    float_ids = float_ids[which(is.na(float_ids)==F)]
    
    if(length(float_ids)==0){
      float_ids = list()
    }
    
  }else{
    float_ids = list()
  }
  
  
  return(list(float_ids=float_ids, float_profs=float_profs))
}

