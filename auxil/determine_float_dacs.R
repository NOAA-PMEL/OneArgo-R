determine_float_dacs <- function(floats, path_index, path_index_bgc) {
  # determine_float_dacs  This function is part of the
  # R toolbox for accessing Argo float data.
  #
  # USAGE:
  #   [float_dacs, float_is_bgc] = determine_float_dacs(...
  #       floats, path_index, path_index_bgc)
  #
  # DESCRIPTION:
  #   This function determines which DACs handle the specified floats and
  #   whether or not they are BGC floats.
  #   This function (instead of list_dacs) is needed for the use with 
  #   snapshots, when the index files are not yet available in their
  #   final locations.
  #
  # INPUTS:
  #   floats: array of WMO IDs
  #   path_index: path to the index file of all floats
  #   path_index_bgc: path to the index file of BGC floats
  #
  # OUTPUTS:
  #   float_dacs  : cell array of DACs
  #   float_is_bgc: array of TRUE(s) and FALSE(s)
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
  
  if (length(floats) == 0) {
    float_dacs <- NULL
    float_is_bgc <- NULL
    return(list(float_dacs, float_is_bgc))
  }
  
  floats <- unique(floats) # make sure that there are no duplicates
  # need to use index file to determine the DACs handling these floats;
  # need a custom algorithm because list_dacs uses the Float struct,
  # which isn't created yet at this point
  index_table <- read.csv(path_index, header = TRUE, skip = 8)
  split_path <- strsplit(index_table$file, "/")
  float_ids<-matrix(unlist(split_path), ncol=4, byrow=TRUE)[,2]

  index_table_bgc <- read.csv(path_index_bgc, header = TRUE, skip = 8)
  split_path_bgc <- strsplit(index_table_bgc$file, "/")
  float_ids_bgc<-matrix(unlist(split_path_bgc), ncol=4, byrow=TRUE)[,2]
  
  float_dacs <- vector("list", length = length(floats))
  float_is_bgc <- rep(NA, length(floats))
  
  for (f in seq_along(floats)) {
    all_float_dacs <-matrix(unlist(split_path[float_ids %in% floats[f]]), ncol=4, byrow=TRUE)[,1] 
      split_path[float_ids %in% floats[f]] #needed below
    if (length(all_float_dacs) == 0) {
      warning(sprintf("float %d not found", floats[f]))
    } else {
      float_dacs[[f]] <- all_float_dacs[1]
    }
    float_is_bgc[f] <- floats %in% float_ids_bgc
  }
  
  float_dacs <- float_dacs[!sapply(float_dacs, is.null)]
  
  return(list(float_dacs, float_is_bgc))
}
