# cleanup_snapshot This function is part of the
# R toolbox for accessing Argo float data.
#
# USAGE:
#   download_snapshot(floats_to_keep)
#
# DESCRIPTION:
#   This function removes the files that your not corresponding to the specified
#   float list in the Snapshot folder to save space.
#
# INPUT: 
#   'floats_tp_keep': a vector of floats WMO that the user will keep for 
#   further analysis
#
# OUTPUT: None
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

cleanup_snapshot <- function(floats_to_keep) {
  # Display warning if no floats are specified
  if (length(floats_to_keep) == 0) {
    cat("You did not specify any floats to keep from the snapshot.\n")
    cat("All profile files of this snapshot will be deleted.\n")
    answer <- readline(prompt = "Are you sure? Enter 'yes' to delete all snapshot profile files: ")
    if (tolower(answer) != "yes") {
      cat("No files will be deleted.\n")
      return()
    }
  }
  
  # Define snapshot path
  snap_path_prof <- paste0(Setting$snap_dir, Setting$snap_path, "Profiles/")
  
  # Delete files based on Settings$default_type
  if (Setting$default_type == "bgc") {
    # Keep only Sprof files
    prof_files <- list.files(path = snap_path_prof, pattern = "_prof.nc", full.names = TRUE)
    for (file in prof_files) {
      file.remove(file)
    }
  } else if (Setting$default_type == "phys") {
    # Keep only prof files
    sprof_files <- list.files(path = snap_path_prof, pattern = "_Sprof.nc", full.names = TRUE)
    for (file in sprof_files) {
      file.remove(file)
    }
  }
  
  # Delete profile files not in floats_to_keep
  prof_files <- list.files(path = snap_path_prof, pattern = "prof.nc", full.names = TRUE)
  for (file in prof_files) {
    wmoid <- as.numeric(gsub(pattern = "_.*", replacement = "", x = basename(file)))
    if (!(wmoid %in% floats_to_keep)) {
      file.remove(file)
    }
  }
}
