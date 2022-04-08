check_dir<-function(ddir) {
  
  # DESCRIPTION:
  #   This function determines if a directory needs to be created and does so
  #   if necessary.
  #
  # INPUT:
  #   ddir : directory (can be a relative or absolute path)
  #
  # OUTPUT:
  #   success : 0 if ddir did not exist yet and cannot be created; 1
  #             otherwise
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
  
  
  success = 1 # default
  if (!dir.exists(ddir)) {
    tryCatch(dir.create(ddir), 
             error = function(e) print(paste('Could not create directory #s',ddir)))
    if(!dir.exists(ddir)) {success = 0}
  }
  
  return(success)

  
}
