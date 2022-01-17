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
  # UPDATE RECORD: 
  #   Version 1:   June 2021 
  #   Version 1.1: January 2022 
  #
  # CITATION:
  #   M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), R. Sauzede (IMEV) and 
  #   C. Schmechtig (OSU ECCE TERRA), 2021.
  #   BGC-Argo-R: A R toolbox for accessing and visualizing Biogeochemical Argo data. 
  #   Zenodo. http://doi.org/10.5281/zenodo.5028139
  

  
  if (!file.exists(dest_path)) {tf = 1}
  else if (Setting$update == 0 || Setting$update == 1) {tf = Setting$update}
  else {finf = file.info(dest_path)
  file_age = difftime(Sys.time(), finf[,"mtime"], units = "secs")
  if(file_age > Setting$update) {tf=1} else {tf=0}}
  
  return(tf)
  
}
