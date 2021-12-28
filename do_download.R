do_download<-function(dest_path) {
  # do_download  
  # 
  #This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  # This function determines if a file should be downloaded or not
  # (i.e., if it exists already at the given dest_path), based on the 
  # "update" option from the global Setting structure.
  #
  # Input:
  #   dest_path : local destination path for a file, which may not yet
  #               exist
  #
  # Output:
  #   tf        : True (1) or false (0) - is download needed? (This depends
  #               on the value of Setting.update and the existence and age
  #               of the file.)
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
  

  
  if (!file.exists(dest_path)) {tf = 1}
  else if (Setting$update == 0 || Setting$update == 1) {tf = Setting$update}
  else {finf = file.info(dest_path)
  file_age = difftime(Sys.time(), finf[,"mtime"], units = "secs")
  if(file_age > Setting$update) {tf=1} else {tf=0}}
  
  return(tf)
  
}
