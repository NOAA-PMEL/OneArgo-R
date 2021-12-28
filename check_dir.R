check_dir<-function(ddir) {
  # check_dir  
  
  #This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  # This function determines if a directory needs to be created and does so
  # if necessary.
  #
  # Input:
  #   ddir : directory (can be a relative or absolute path)
  #
  # Output:
  #   success : 0 if ddir did not exist yet and cannot be created; 1
  #             otherwise
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
  
  success = 1 # default
  if (!dir.exists(ddir)) {
    tryCatch(dir.create(ddir), 
             error = function(e) print(paste('Could not create directory #s',ddir)))
    if(!dir.exists(ddir)) {success = 0}
  }
  
  return(success)

  
}
