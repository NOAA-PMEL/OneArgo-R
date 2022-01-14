
get_lon_lat_time=function(float_ids, float_profs=NULL){
  # get_lon_lat_time  This function is part of the
  # MATLAB toolbox for accessing BGC Argo float data.

  # DESCRIPTION:
  #   This function loads longitude, latitude, and time information
  #   for the specified floats (and their specified profiles, if given).
  #
  # INPUT:
  #   float_ids   : WMO ID(s) of one or more floats
  #
  # OPTIONAL INPUT:
  #   float_profs : cell array with indices of selected profiles (per float,
                                                                     # %                 not global)
  #
  # OUTPUTS:
  #   lon  : cell array with longitude values for all specified floats
  #   lat  : cell array with latitude values for all specified floats
  #   time : cell array with time values for all specified floats (in 
                                                                 #    %          datenum format)   
  #
  # AUTHORS: 
  #  H. Frenzel, J. Sharp, A. Fassbender (NOAA-PMEL), N. Buzby (UW),
  #   J. Plant, T. Maurer, Y. Takeshita (MBARI), D. Nicholson (WHOI),
  #   and A. Gray (UW)
  #
    
  # DATE: DECEMBER 1, 2021  (Version 1.1)

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

