do_download<-function(dest_path) {
  
  # DESCRIPTION:
  #   This function determines if a file should be downloaded or not
  #   (i.e., if it exists already at the given dest_path), based on the 
  #   "update" option from the global Setting structure.
  #
  # INPUT:
  #   dest_path : local destination path for a file, which may not yet
  #               exist
  #
  # OUTPUT:
  #   tf        : True (1) or false (0) - is download needed? (This depends
  #               on the value of Setting.update and the existence and age
  #               of the file.)
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

  
  if (!file.exists(dest_path)) {tf = 1}
  else if (Setting$update == 0 || Setting$update == 1) {tf = Setting$update}
  else {finf = file.info(dest_path)
  file_age = difftime(Sys.time(), finf[,"mtime"], units = "secs")
  if(file_age > Setting$update) {tf=1} else {tf=0}}
  
  return(tf)
  
}
