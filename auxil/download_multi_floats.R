download_multi_floats<- function(float_ids) {
  
  # DESCRIPTION:
  # This is function is designed to download the Sprof file for one float with a given floatid.
  #
  # PREREQUISITES:
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
  #   OneArgo-R: A R toolbox for accessing and visualizing Argo data.
  #   Zenodo. XXXXX
  #
  # LICENSE: oneargo_r_license.m
  #
  # DATE: JUNE 1, 2022  (Version 1.0.1)
  
  
  good_float_ids = c()
  not_found = NULL
  count = 0
  for (i in c(1:length(float_ids))) {
    if (download_float(float_ids[i])==1) {
      good_float_ids<-c(good_float_ids,float_ids[i])
    }
    else {
      not_found= cat(paste(not_found,"\n not found",float_ids[i]))
      count = count + 1
      
    }
  }
  
  if (length(not_found)!=0) {
    print('Sprof files could not be downloaded for floats:')
    print(cat(paste(not_found)))
  }
  
  return(good_float_ids)
  
}
