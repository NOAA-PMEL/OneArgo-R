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
  # UPDATE RECORD: 
  #   Version 1 & 2:   June 2021 
  #   Version 2.1: January 2022 
  #
  # CITATION:
  #   M. Cornec (LOV, now at NOAA-PMEL), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), R. Sauzede (IMEV) and 
  #   C. Schmechtig (OSU ECCE TERRA), 2021.
  #   BGC-Argo-R: A R toolbox for accessing and visualizing Biogeochemical Argo data. 
  #   Zenodo. http://doi.org/10.5281/zenodo.5028138
  
  
  
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
