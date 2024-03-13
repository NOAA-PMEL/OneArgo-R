reorg_snapshot_files <- function(full_snap_path, orig_dac, floats, float_dacs, snap_type, keep) {
  # reorg_snapshot_files  This function is part of the
  # R toolbox for accessing Argo float data.
  #
  # USAGE:
  #   reorg_snapshot_files(full_snap_path, orig_dac, floats, ...
  #       float_dacs, snap_type, keep)
  #
  # DESCRIPTION:
  #   This function reorganizes the files for one snapshot of Argo data
  #   so that the directory tree is the same as for GDAC files.
  #
  # INPUTS: 
  #   full_snap_path: relative or absolute path to the main directory of
  #         the currently selected snapshot
  #   orig_dac: cell array of DAC names whose float files will be kept
  #         (empty to keep all float files unless floats are specified)
  #   floats: list of WMOIDs of floats whose files will be kept (in addition
  #         to those from orig_dac DACs, if specified); can be empty
  #   float_dacs: list of DACs handling the specified floats; can be empty
  #   snap_type: either 'all', 'phys', or 'bgc'
  #   keep: if 0, tech, meta, and traj files will be deleted by this
  #         function; they will be kept for keep > 0
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
  
  snap_path_dac <- paste0(full_snap_path, 'dac')
  
  contents <- list.files(snap_path_dac, full.names = TRUE)
  for (i in seq_along(contents)) {
    if (file.info(contents[i])$isdir) {
      if (basename(contents[i]) != '.' && basename(contents[i]) != '..') {
        if (snap_type == 'bgc') {
          if ((!is.null(orig_dac) || !is.null(floats)) &&
              !basename(contents[i]) %in% orig_dac &&
              !basename(contents[i]) %in% float_dacs) {
            unlink(contents[i], recursive = TRUE)
            next
          }
          dac_files <- list.files(path = paste0(snap_path_dac, '/', basename(contents[i])), pattern = '*prof.nc', full.names = TRUE)
        } else {
          dac_files <- list.files(path = paste0(snap_path_dac, '/', basename(contents[i])), pattern = '*.nc', recursive = TRUE, full.names = TRUE)
        }
        for (f in seq_along(dac_files)) {
          if (!is.null(floats) && !basename(contents[i]) %in% orig_dac) {
            keep_this <- FALSE
            for (fl in seq_along(floats)) {
              if (startsWith(basename(dac_files[f]), paste0(floats[fl], '_'))) {
                keep_this <- TRUE
                break
              }
            }
            if (!keep_this) {
              file.remove(dac_files[f])
              next
            }
          }
          if (grepl('prof', basename(dac_files[f]))) {
            new_path <- paste0(full_snap_path, '/Profiles')
          } else if (grepl('meta', basename(dac_files[f]))) {
            new_path <- paste0(full_snap_path, '/Meta')
          } else if (grepl('tech', basename(dac_files[f]))) {
            new_path <- paste0(full_snap_path, '/Tech')
          } else if (grepl('traj', basename(dac_files[f]))) {
            new_path <- paste0(full_snap_path, '/Traj')
          } else {
            new_path <- full_snap_path  # this should not happen
          }
          if (keep || grepl('prof', basename(dac_files[f]))) {
            file.rename(dac_files[f], file.path(new_path, basename(dac_files[f])))
          }
        }
      }
    } else {  # regular file
      this_file <- contents[i]
      if (endsWith(basename(this_file), '.gz') && !grepl('tar', this_file)) {
        gunzip(this_file)
        this_file <- sub('.gz$', '', this_file)  # strip out '.gz'
        if (file.exists(this_file)) {
          file.remove(paste0(this_file, '.gz'))
        }
      }
      if (grepl('index', basename(this_file))) {
        file.rename(this_file, file.path(full_snap_path, 'Index', basename(this_file)))
      } else if (!grepl('tar', this_file)) {
        file.rename(this_file, file.path(full_snap_path, basename(this_file)))
      }
    }
  }
}
