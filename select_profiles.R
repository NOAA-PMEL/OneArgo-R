select_profiles <- function(lon_lim=c(-180,180), 
                            lat_lim=c(-90, 90), 
                            start_date="1995-01-01", 
                            end_date="2038-01-19",
                            outside="none", 
                            sensor=NULL,
                            ocean=NULL,
                            mode="RAD") {
  
  # DESCRIPTION:
  #   This function returns the indices of profiles and floats that match the given
  #   criteria (spatial, temporal, sensor availability). It does not download
  #   any files.
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
  #
  # OUTPUTS:
  #   float_ids   : array with the WMO IDs of all matching floats
  #   float_profs : cell array with the per-float indices of all matching profiles
  #
  #
  # UPDATE RECORD: 
  #   Version 1 & 2:   June 2021 
  #   Version 2.1: January 2022 
  #
  # CITATION:
  #   M. Cornec (LOV, now at NOAA-PMEL), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), R. Sauzede (IMEV) and 
  #   C. Schmechtig (OSU ECCE TERRA), 2021.
  #   BGC-Argo-R: A R toolbox for accessing and visualizing Biogeochemical Argo data. 
  #   Zenodo. http://doi.org/10.5281/zenodo.5028138
  
  
  
  
  
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
  if (exists("Sprof")==F) {
    initialize_argo()
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
    lonv1 = c(lon_lim[1], 180)
    lonv2 = c(-180, lon_lim[2])
    inpoly =  ( (Sprof$lon>lonv1[1] & Sprof$lon<lonv1[2]) | 
                  (Sprof$lon>lonv2[1] & Sprof$lon<lonv2[2]) ) & 
      (Sprof$lat>lat_lim[1] & Sprof$lat<lat_lim[2])
  } else {
    inpoly = (Sprof$lon>lon_lim[1] & Sprof$lon<lon_lim[2] & 
                Sprof$lat>lat_lim[1] & Sprof$lat<lat_lim[2])
  }
  
  # Find index of dates that are within the time window
  
  indate_poly = (Sprof$date[inpoly] >= dn1 & Sprof$date[inpoly] <= dn2)
  
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
    has_sensor = grepl(sensor, Sprof$sens)
  }
  if(any(has_sensor)==F){
    warning('no data found for sensor ', sensor)
  }
  
  # SELECT BY OCEAN BASIN
  if ( is.null(ocean) ) {
    is_ocean = rep(TRUE, length(inpoly)) # no ocean was selected
  } else {
    is_ocean = grepl(ocean, Sprof$ocean)
  }
  
  # SELECT BY DATA MODE
  # Note: due to inconsistencies between index and Sprof files, this
  # matching is not performed in the first round, only in the second
  # round below (based on Sprof files)
  # if ( is.null(mode) | mode=="ADR" ) {
     has_mode = rep(TRUE, length(inpoly)) 
  # } else {
  #   has_mode = rep(FALSE, length(inpoly)) # no sensor was selected
  #   is_good = inpoly & indate & has_sensor & is_ocean
  #   idx = all_floats[is_good]
  #   varlist<-strsplit(Sprof$sens," ")
  #   if(length(idx)!=0){
  #     for (i in 1:length(idx)) {
  #       pos = sensor == varlist[[idx[i]]]
  #       if(any(pos)){
  #         for (j in 1:nchar(mode)){
  #           if (substr(mode,j,j) == substr(Sprof$data_mode[idx[i]],
  #                                          which(pos),
  #                                          which(pos))){
  #             has_mode[idx[i]]<-TRUE
  #           }
  #         }
  #       }
  #     }
  #   }
  # }
  
  
  profiles = which(inpoly & indate & has_sensor & is_ocean & has_mode)
  float_ids = unique(Sprof$wmo[profiles])
  float_profs = list()
  
  if(length(float_ids)!=0){
    # download Sprof files if necessary
    good_float_ids = download_multi_floats(float_ids)
    
    # the information from the index file is only used for an initial
    # filtering of floats, the actual information from the Sprof files
    # is used in a second step
    
    float_ids = good_float_ids
    
    for (fl in 1:length(good_float_ids)) {
      filename = paste(Setting$prof_dir,good_float_ids[fl],"_Sprof.nc",sep="")
      n_prof = get_dims(filename)$n_prof
      n_param = get_dims(filename)$n_param
      fl_idx = which(Float$wmoid==good_float_ids[fl])
      n_prof_exp = Float$prof_idx2[fl_idx] - Float$prof_idx1[fl_idx] + 1
      if (n_prof_exp > n_prof) {
        warning(paste("The index file lists", n_prof_exp,"profiles for float",
                      good_float_ids[fl],"but the Sprof file has only",n_prof,"profiles."))
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
          slon_diff<-abs(Sprof$lon - lon[ii])
          slat_diff<-abs(Sprof$lat - lat[ii])
          ssum<-slon_diff+slat_diff
          if(Sprof$ocean[which.min(ssum)[1]]==ocean){
            is_ocean[ii]<-TRUE
          }
        }
        sprof_loc = as.character(paste0(Sprof$lon,'i ',Sprof$lat))
        this_loc = as.character(paste0(lon,'i ',lat))
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

