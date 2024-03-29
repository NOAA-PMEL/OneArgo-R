download_float<-function(floatid) {
  
  # DESCRIPTION:
  # This is function is designed to download the Sprof file for one float with a given floatid.
  #
  # PREREQUISITE:
  #   The Sprof index file must have been downloaded already. 
  #
  # INPUT:
  #   floatid  : WMO ID of a float (integer)
  #
  # OUTPUT:
  #   success  : 1 for success, 0 for failure
  #
  #
  # AUTHORS:
  #   Marin Cornec (NOAA-PMEL), Yibin Huang (NOAA-PMEL), 
  #   Quentin Jutard (OSU ECCE TERRA), Raphaelle Sauzede (IMEV) and 
  #   Catherine Schmechtig (OSU ECCE TERRA).
  #
  # CITATION:
  #   M. Cornec, Y. Huang, Q. Jutard, R. Sauzede, and C. Schmechtig, 2022. 
  #   OneArgo-R: An R toolbox for accessing and visualizing Argo data.
  #   Zenodo. https://doi.org/10.5281/zenodo.6604650
  #
  # LICENSE: oneargo_r_license.m
  #
  # DATE: JUNE 1, 2022  (Version 1.0.1) 
  
  
  
  #global Settings Float;
  
  if (missing(floatid)) { stop('usage: download_float(WMO_ID)')
  }
  
  
  success = 0 # set to 1 after successful download
  
  # make sure Float is initialized
  if(is.null(Float)){
    initialize_argo()
  }
  
  ind = c(1:Float$nfloats)
  float_idx = ind[which(Float$wmoid %in% floatid)]
  
  if (length(float_idx)==0) {
    print(paste('Float', floatid,"was not found!"))
  }
  
  
  local_path = paste(Setting$prof_dir,Float$file_name[float_idx],sep="")
  # now check if the Sprof file exists locally already,
  # and if so, if it is up-to-date
  if (file.exists(local_path)) {
    tryCatch({
      profile=nc_open(local_path)
      sprof_date=ymd_hms(ncvar_get(profile,'DATE_UPDATE'))
      nc_close(profile)
      # allow a small tolerance value for numerical imprecision
      update = difftime(sprof_date, ymd_hms(Float$update[float_idx]), units = "days")
      update = update+0.01
      if (update>0) {
        # existing file has all profiles, no need to download again
        success = 1
      } else {success = try_download(paste('dac/',Float$file_path[float_idx],sep=""),
                                     local_path)}}
      , error = function(e) warning('something went wrong, try downloading the file again')
    )
  } else {success = try_download(paste('dac/',Float$file_path[float_idx],sep=""),
                                      local_path)}
  
  
  
  return(success)
  
}
