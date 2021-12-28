download_multi_floats<- function(float_ids) {
  # download_multi_floats  
  #
  #This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  # This function downloads Sprof*.nc files for specified float(s).
  # A message is shown if profiles for any of these floats could not be
  # downloaded.
  #
  # Input:
  #   float_ids : WMO ID(s) of the float(s)
  #
  # Output:
  #   good_float_ids : WMO ID(s) of the float(s) that were downloaded
  #
  # Authors: H. Frenzel, J. Sharp, A. Fassbender (NOAA-PMEL),
  # J. Plant, T. Maurer, Y. Takeshita (MBARI), D. Nicholson (WHOI),
  # and A. Gray (UW)
  #
  # CITATION:
  # BGC-Argo-R: A R toolbox for accessing and visualizing
  # Biogeochemical Argo data,
  #
  #  AUTHORS: 
  # M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), 
  # R. Sauzede (IMEV) and C. Schmechtig (OSU ECCE TERRA),
  #
  # Adapted from the Matlab toolbox BGC-Argo-Mat:  https://doi.org/10.5281/zenodo.4971318
  # (H. Frenzel, J. Sharp, A. Fassbender (NOAA-PMEL),
  # J. Plant, T. Maurer, Y. Takeshita (MBARI), D. Nicholson (WHOI),
  # and A. Gray (UW))
  
  # Update 24 June 2021
  
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
