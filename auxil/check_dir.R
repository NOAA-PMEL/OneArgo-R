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
  
  success = 1 # default
  if (!dir.exists(ddir)) {
    tryCatch(dir.create(ddir), 
             error = function(e) print(paste('Could not create directory #s',ddir)))
    if(!dir.exists(ddir)) {success = 0}
  }
  
  return(success)

  
}
