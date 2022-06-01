get_lon_lat_time=function(float_ids, float_profs=NULL){
  
  
  # DESCRIPTION:
  #   This function loads longitude, latitude, and time information
  #   for the specified floats (and their specified profiles, if given).
  #
  # INPUT:
  #   float_ids: WMO ID(s) of one or more floats
  #
  # OPTIONAL INPUT:
  #   float_profs : float profile is an array with the per-float indices 
  #                 as returned by function "select_profiles";  
  #
  # OUTPUTS:
  #   lon  : cell array with longitude values for all specified floats
  #   lat  : cell array with latitude values for all specified floats
  #   time : cell array with time values for all specified floats (in 
  #     datenum format)   
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
  
  lon=NULL
  lat=NULL
  time=NULL
  
  if(is.null(float_ids)){
    stop("no floats specified")
  }
  
  Data = load_float_data(float_ids=float_ids,float_profs=float_profs)$Data
  
  nfloats = length(Data)
  fnames = names(Data)
  
  

  for (f in 1:nfloats){
   lon[[f]]<-Data[[fnames[f]]]$LONGITUDE[1,]
   names(lon)[f] <-  names(Data[f]) #
   lat[[f]]<-Data[[fnames[f]]]$LATITUDE[1,]
   names(lat)[f] <-  names(Data[f]) #
   time[[f]]<-Data[[fnames[f]]]$TIME[1,]
   names(  time)[f] <-  names(Data[f]) #
  
     
  }
  
  
  
  return(list(lon=lon,lat=lat,time=time))


}

