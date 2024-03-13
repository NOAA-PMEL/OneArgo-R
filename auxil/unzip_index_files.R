unzip_index_files <- function(verbose) {
  # unzip_index_files  This function is part of the
  # R toolbox for accessing Argo float data.
  #
  # USAGE:
  #   [path_index, path_index_bgc] = unzip_index_files(verbose)
  #
  # DESCRIPTION:
  #   This function unzips the Sprof index file and for full snapshots 
  #   also the full index file for one snapshot of Argo data.
  #   It deletes the gzipped files afterwards.
  #   Setting$snap_dir and Setting$snap_path
  #   values are used to determine the path.
  #
  # INPUT: 
  #   verbose: if > 0, show information about what is being done
  #
  # OUTPUTS:
  #   path_index: path to the Sprof index file in case of BGC snapshots
  #               or to the prof index file in case of full snapshots
  #   path_index_bgc: path to the Sprof index file
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
  
  # all snapshot tarballs contain the Sprof index file
  path_index <- paste0(Setting$snap_dir, Setting$snap_path, "dac/argo_synthetic-profile_index.txt.gz")
  if (verbose > 0) {
    cat(sprintf("Unzipping %s\n", path_index))
  }
  gunzip(path_index)
  path_index <- substr(path_index, 1, nchar(path_index) - 3)  # strip out trailing '.gz'
  path_index_bgc <- path_index
  
  # full snapshot tarballs also contain the prof index file
  if (!grepl('Bgc', Setting$snap_path)) {
    index_file <- paste0(Setting$snap_dir, Setting$snap_path, "dac/ar_index_global_prof.txt.gz")
    path_index <- index_file
    if (verbose > 0) {
      cat(sprintf("Unzipping %s\n", path_index))
    }
    gunzip(path_index)
    path_index <- substr(path_index, 1, nchar(path_index) - 3)  # strip out trailing '.gz'
  }
  
  return(list(path_index = path_index, path_index_bgc = path_index_bgc))
}
