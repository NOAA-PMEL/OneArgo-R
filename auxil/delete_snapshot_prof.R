delete_snapshot_prof <- function(snap_path_dac, fn_tar, keep, verbose) {
  # delete_snapshot_prof  This function is part of the
  # R toolbox for accessing Argo float data.
  #
  # USAGE:
  #   delete_snapshot_prof(snap_path_dac, fn_tar, keep, verbose)
  #
  # DESCRIPTION:
  #   This function deletes the individual profile files of a full
  #   snapshot file. The calling function must check if keep is set to 3,
  #   which indicates to keep these files.
  #   Unless keep is non-zero, meta, tech, and traj files will be deleted
  #   as well.
  #
  # INPUTS:
  #   snap_path_dac: path to the 'dac' directory
  #   fn_tar: name of the dac tarball (e.g., 'aoml_core.tar.gz')
  #   keep: if 0, tech, meta, and traj files are deleted,
  #         if > 0, they will be kept
  #   verbose: if > 0, show which directories are being deleted
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
  
  # extract the name of the dac of this tarball
  match_dac <- regmatches(fn_tar, regexpr("[a-z]+_", fn_tar))
  this_dac <- substr(match_dac, 1, nchar(match_dac) - 1)
  dac_contents <- list.files(path = paste0(snap_path_dac, "/", this_dac))
  for (j in 1:length(dac_contents)) {
    if (file.info(paste0(snap_path_dac, "/", this_dac,"/",dac_contents[j]))$isdir &&
        !grepl("^\\.\\.$|^\\.$", dac_contents[j])) {
      dir_prof <- file.path(snap_path_dac, this_dac, dac_contents[j], "profiles")
      if (file.exists(dir_prof) && file.info(dir_prof)$isdir) {
        if (verbose > 1) {
          cat(sprintf("Deleting %s\n", dir_prof))
        }
        unlink(dir_prof, recursive = TRUE)
      }
      # full snapshots contain meta, tech, and traj files,
      # which will be kept only if requested
      if (keep==0) {
        dir_float <- file.path(snap_path_dac, this_dac, dac_contents[j])
        float_files <- list.files(dir_float, full.names = TRUE)
        for (fn_file in float_files) {
          if (grepl("\\.nc$", fn_file) && !grepl("prof", fn_file)) {
            file.remove(fn_file)
          }
        }
      }
    }
  }
}
