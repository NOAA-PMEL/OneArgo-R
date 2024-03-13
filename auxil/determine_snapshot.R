determine_snapshot <- function() {
  # determine_snapshot  This function is part of the
  # R toolbox for accessing Argo float data.
  #
  # USAGE:
  #   determine_snapshot()
  #
  # DESCRIPTION:
  #   This function determines which snapshot of Argo data will be used,
  #   based on the values of Settings.use_snapshots and
  #   Setting$default_type.
  #   Setting$snap_path, Setting$snap_date, Setting$snap_url, 
  #   Setting$snap_file, and Setting$snap_size will be modified.
  #
  # INPUT: None
  #
  # OUTPUT: None.
  #   Global variable Settings will be modified.     
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
  
  snap_date <- Setting$use_snapshots # shorthand names
  snap_type <- Setting$default_type
  Setting$snap_path <<- NULL
  
  if (!snap_date) {
    return(NULL)
  } else if (snap_date > 1 && (snap_date < 201212 || snap_date > 203800)) {
    warning('Wrong format of snapshot date, must be 1 or YYYYMM')
    return(NULL)
  }
  
  # set search patterns for parsing of web page
  if (snap_type == 'all' || snap_type == 'phys') {
    pattern <- 'Global GDAC Argo data files \\('
    pat_month <- 'Global GDAC Argo data files \\(20\\d{2}-\\d{2}-\\d{2} snapshot'
  } else if (snap_type == 'bgc') {
    pattern <- 'BGC Sprof data files \\('
    pat_month <- 'BGC Sprof data files \\(20\\d{2}-\\d{2}-\\d{2} snapshot\\)'
  } else {
    cat(sprintf('No such type of snapshot file: "%s"\n', snap_type))
    return(NULL)
  }
  
  mth_idx <- nchar(pattern) 
  
  page <- paste(readLines('https://www.seanoe.org/data/00311/42182/'), collapse='')

  idx<-gregexpr(pattern, page)[[1]]
  
  if (length(idx) == 0) {
    cat('The snapshot web page could not be read properly.\n')
    return(NULL)
  }
  
  nidx <- length(idx)
  snap_month <- rep(NA, nidx)
  snap_size <- rep(NA, nidx)
  snap_url <- vector("list", nidx)
  count <- 0
  
  for (i in seq_along(idx)) {
    this_link <- substr(page,idx[i],idx[i]+400)
    match_month <- regmatches(this_link, regexpr(pat_month, this_link))
    pat_url<- '"fileUrl":"https?://[^"]+"'
    match_url <- regmatches(this_link, regexpr(pat_url, this_link, perl=T))
    pat_size <- '"size":\\d+'
    match_size <- regmatches(this_link, regexpr(pat_size, this_link))
    
    if (!is.na(match_month) && length(match_url) > 0) {
      count <- count + 1
      year <- as.numeric(paste(substr(match_month, mth_idx, mth_idx + 3)))
      month <- as.numeric(paste(substr(match_month, mth_idx + 5, mth_idx + 6)))
      snap_month[count] <- 100 * year + month
      snap_url[[count]] <- substr(match_url, 12, nchar(match_url) - 1)
      
      if (length(match_size) > 0) {
        snap_size[count] <- as.numeric(paste(substr(match_size, 8, nchar(match_size))))
      } else {
        snap_size[count] <- -1
      }
    }
  }
  
  if (count == 0) {
    cat('No matching snapshots were found.\n')
    return(NULL)
  }
  
  if (snap_date == T) {
    # use the most recent snapshot, need to sort the available ones by snap_date
    # most recent will be first
    isort <- order(snap_month[!is.na(snap_month)], decreasing = TRUE)
    isnap <- isort[1]
    Setting$snap_date <<- snap_month[isnap]
  } else {
    mini <- min(abs(snap_month - snap_date), na.rm = TRUE)
    isnap <- which(abs(snap_month - snap_date) == mini)[1]
    if (mini > 0) {
      cat(sprintf('No snapshot found for %d\n', snap_date))
      cat(sprintf('The nearest available snapshot is %d\n', snap_month[isnap]))
      return(NULL)
    }
    Setting$snap_date <<- snap_date
  }
  
  if (snap_type == 'all' || snap_type == 'phys') {
    Setting$snap_path <<- sprintf('%d-ArgoData/', Setting$snap_date)
  } else {
    Setting$snap_path <<- sprintf('%d-BgcArgoSprof/', Setting$snap_date)
  }
  
  Setting$snap_url <<- snap_url[[isnap]]
  # This is the most commonly used format for file names:
  Setting$snap_file <<- regmatches(Setting$snap_url, regexpr('\\d+\\.tar.gz', Setting$snap_url, perl = TRUE), invert = FALSE)
  if (is.na(Setting$snap_file)) {  # Alternate file name format
    Setting$snap_file <<- regmatches(Setting$snap_url, regexpr('\\d+\\.tgz', Setting$snap_url, perl = TRUE), invert = FALSE)
  }
  if (is.na(Setting$snap_file)) {  # This should not happen
    cat(sprintf('Unexpected file name for snapshot: "%s"\n', Setting$snap_url))
    return()
  }
  
  Setting$snap_size <<- snap_size[isnap]  # in GB
}
