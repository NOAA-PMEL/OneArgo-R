download_snapshot <- function(dac = NULL,
                              floats = NULL,
                              force = FALSE,
                              keep = 0,
                              verbose = 1) {
  
  # download_snapshot  This function is part of the
  # R toolbox for accessing Argo float data.
  #
  # USAGE:
  #   download_snapshot(varargin)
  #
  # DESCRIPTION:
  #   This function downloads and unpacks one snapshot of Argo data.
  #   Settings.default_type and Settings.use_snapshot
  #   values are used to determine which snapshot will be downloaded.
  #
  # INPUT: None
  #
  # OPTIONAL INPUTS  (key,value pairs):
  #   'dac, dac: keep only files from the specified DAC(s), e.g., 'AOML'
  #         or c('jma';'coriolis')
  #   'force', force: if FALSE (default), do not download a snapshot file if
  #         at least *some* of its contents exist locally already (NOTE:
  #         the function does not check if specific files requested with
  #         'dac' or 'floats' are present);
  #         if TRUE, download snapshot in any case - this should be used if
  #         additional floats or dac files need to be unpacked
  #   'floats', floats: keep only files for the float(s) with the specified
  #         WMO ID(s)
  #         If both 'dac' and 'floats' are specified, all files needed for
  #         both specifications are downloaded.
  #         NOTE: If none of the specified floats are found for the specified
  #         type of snapshot and 'dac' is not specified, a warning will be
  #         issued and no files will be unpacked.
  #   'keep', keep: if 0 (default), all files that are not used by this
  #         toolbox are deleted; if >0, tech, meta, and traj files are kept
  #         from full snapshots;
  #         if >1, most files are kept ('aux' and 'geo' sub directories,
  #         full tarball, intermediate tarballs in addition to files listed
  #         for 'keep' set to 1)
  #         if 3, individual profile netcdf files for full snapshots are
  #         kept as well;
  #         note that settings of 2 and 3 require a lot of disk space,
  #         esp. for full snapshots
  #         (If 'dac' and/or 'floats' options are used, non-matching float
  #         profiles will be deleted even if keep is set to 2 or 3.)
  #         If BGC snapshots are used, the only file affected by 'keep'
  #         is the snapshot tarball file itself - kept if 'keep' is 2 or 3.
  #   'verbose', verbose: if 1, show more information about what is being
  #         done; if 0, only show errors and warnings; default: 1
  #         if 2, show even more output about progress
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

  
  
  # make sure Settings is initialized
  if (is.null(Settings)) {
    initialize_argo()
  }
  
  # set default values
  snap_type <- Setting$default_type
  snap_date <- Setting$use_snapshots
  
  if (!snap_date) {
    cat('\nSetting$use_snapshots is set to FALSE, so no snapshot will be downloaded.\n')
    cat('If you want to switch to using snapshots, update the setting in\n')
    cat('initialize_argo.R, then run initialize_argo.\n\n')
    return()
  }
  
  determine_snapshot()
  if (is.null(Setting$snap_path)) {
    return()
  }
  
  full_snap_path <- paste0(Setting$snap_dir, Setting$snap_path)
  if (file.exists(full_snap_path) && !force) {
    if (snap_date == 1) {
      cat('Most recent snapshot was downloaded already\n')
    } else {
      cat(sprintf('Snapshot for %d was downloaded already\n', snap_date))
    }
    return()
  }
  
  # always use cells for DAC(s); convert char array to cell
  if (is.character(dac)) {
    dac <- as.list(dac)
  }
  
  # check if the requested snapshot tarball exists already
  download_snap <- TRUE
  if (file.exists(Setting$snap_file)) {
    # check if size of previously downloaded file matches the expected size
    file_info <- file.info(Setting$snap_file)
    file_size <- file_info$size
    if (file_size == Setting$snap_size) {
      download_snap <- FALSE
    } else {
      cat(sprintf('"%s" was not downloaded correctly before.\n', Setting$snap_file))
      cat(sprintf('Expected file size: %s bytes\n', Setting$snap_size))
      cat(sprintf('Actual file size:   %s bytes\n', file_size))
    }
  }
  
  if (download_snap) {
    cat(sprintf('Starting download of "%s" now\n', Setting$snap_file))
    cat(sprintf('Its file size is %.1f GB - this may take a while.\n', Setting$snap_size*1e-9))
    download.file(Setting$snap_url, destfile = Setting$snap_file, mode = "wb")
    if (!file.exists(Setting$snap_file)) {
      cat(sprintf('"%s" could not be downloaded\n', Setting$snap_file))
      return()
    }
    # check if size of downloaded file matches the expected size
    file_info <- file.info(Setting$snap_file)
    file_size <- file_info$size
    if (file_size != Setting$snap_size) {
      cat(sprintf('"%s" was not downloaded correctly.\n', Setting$snap_file))
      cat(sprintf('Expected file size: %s bytes\n', Setting$snap_size))
      cat(sprintf('Actual file size:   %s bytes\n', file_size))
      return()
    }
  }

  tryCatch({
    start_time <- Sys.time()
    cat('Untarring the snapshot, this may take a few minutes... ')
    untar(Setting$snap_file, exdir = Setting$snap_dir)
    cat('done!\n')
    end_time <- Sys.time()
    cat('Elapsed time: ', end_time - start_time, '\n')
  }, error = function(e) {
    cat('error! Aborting...\n')
    return()
  })
  
  path_indexes<-unzip_index_files(verbose)
  path_index <-path_indexes$path_index
  path_index_bgc <- path_indexes$path_index_bgc
  
  float_dacs <- NULL 
  float_is_bgc <- NULL
  
  if (!is.null(floats)) {
    float_dacs <- determine_float_dacs(floats, path_index, path_index_bgc)[[1]]
    float_is_bgc <- determine_float_dacs(floats, path_index, path_index_bgc)[[2]]
  }
  
  if (!is.null(floats) && is.null(float_dacs)) {
    warning('None of the specified floats was found')
    if (is.null(dac)) {
      warning('Snapshot will not be unpacked!')
      return()
    }
  }
  
  # the original tarball is still present after untarring
  # wait until here with the deletion in case wrong float IDs were used
  # accidentally

  if (keep < 2) {
    file.remove(Setting$snap_file) # delete it to free up disk space
  }
  
  # If both 'dac' and 'floats' are specified, all files needed for
  # both specifications are downloaded.
  orig_dac <- dac # keep track if dac was specified as an option
  dac <- union(dac, float_dacs)
  
  # determine which tarballs need to be unpacked
  if (snap_type != 'bgc' && !is.null(dac)) {
    if (is.null(floats)) {
      need_dac_core <- rep(T, length(dac))
      need_dac_bgc <- rep((snap_type == 'all'), length(dac))
    } else {
      need_dac_core <- rep(F, length(dac))
      need_dac_bgc <- rep(F, length(dac))
      for (d in seq_along(dac)) {
        if (dac[d] %in% orig_dac) {
          need_dac_core[d] <- T
          need_dac_bgc[d] <- T # not checked below if type is 'phys'
        } else {
          # find the specified floats from the current dac
          is_dac <- which(unlist(float_dacs)==dac[d])
          if (any(float_is_bgc[is_dac])) {
            need_dac_bgc[d] <- T
          }
          if (any(!float_is_bgc[is_dac]) || snap_type == 'phys') {
            need_dac_core[d] <- T
          }
        }
      }
    }
  }
  
  # Individual (S)prof files are sorted by dac subdirectory, which is
  # a different approach from this toolbox's normal way of organizing files.
  # Therefore, files will be reorganized.
  snap_path_dac <- paste0(full_snap_path, 'dac')
  
  # In the case of full snapshots, there are core and bgc tarballs for
  # each DAC, which must be unpacked first.
  if (snap_type != 'bgc') {
    # contents of aux and geo directory trees are not used by this toolbox
    if (keep < 2) {
      unlink(paste0(full_snap_path, '/aux'), recursive = TRUE)
      unlink(paste0(full_snap_path, '/geo'), recursive = TRUE)
    }
    tarballs <- list.files(path = snap_path_dac, pattern = "\\.tar\\.gz$", full.names = TRUE)
    for (i in seq_along(tarballs)) {
      if (snap_type == 'phys' && grepl("bgc\\.tar\\.gz$", tarballs[i])) {
        if (keep < 2) {
          if (verbose) {
            cat(sprintf('Deleting "%s"\n', basename(tarballs[i])))
          }
          file.remove(tarballs[i])
        }
        # if keep is set to 2 or 3, the bgc tarballs are kept, but not
        # unpacked
        next
      }
      if (!is.null(dac) && keep < 2) {
        keep_tar <- FALSE
        for (d in seq_along(dac)) {
          if (startsWith(basename(tarballs[i]), as.character(paste(dac[d])))) {
            if (grepl("bgc\\.tar\\.gz$", basename(tarballs[i]))) {
              keep_tar <- need_dac_bgc[d]
            } else if (grepl("core\\.tar\\.gz$", basename(tarballs[i]))) {
              keep_tar <- need_dac_core[d]
            }
            break
          }
        }
        if (keep_tar==F) {
          if (verbose) {
            cat(sprintf('Deleting "%s"\n', basename(tarballs[i])))
          }
          file.remove(tarballs[i])
          next
        }
      }
      file_info <- file.info(tarballs[i])
      file_size <- file_info$size
      if (file_size > 1e9) {
        cat(sprintf('Untarring "%s" (%.0f GB) may take a few minutes... ', basename(tarballs[i]), 1e-9 * file_size))
      } else if (verbose) {
        cat(sprintf('Untarring "%s"... ', basename(tarballs[i])))
      }
      tryCatch({
        untar(tarballs[i], exdir = snap_path_dac)
        cat('done!\n')
        if (keep < 2) {
          file.remove(tarballs[i]) # delete tarball ASAP to free up disk space
        }
      }, error = function(e) {
        cat('Error during untar!\n')
      })
      # the full snapshot archives include individual profile files,
      # which are not used by this toolbox
      if (keep < 3) {
        if (verbose >= 1) {
          cat(sprintf('Deleting individual profile files from %s\n', basename(tarballs[i])))
        }
        delete_snapshot_prof(snap_path_dac, basename(tarballs[i]), keep, verbose)
      }
    }
  }
  
  # Sort files into appropriate subdirectories - use the same setup
  # as for files downloaded directly from the GDAC
  # start by creating subdirectories
  if (keep > 0 && snap_type != 'bgc') {
    subdirs <- c('Index', 'Profiles', 'Meta', 'Tech', 'Traj')
  } else {
    subdirs <- c('Index', 'Profiles')
  }
  
  for (subdir in subdirs) {
    dir_path <- file.path(full_snap_path, subdir)
    if (!dir.create(dir_path)) {
      cat(sprintf('%s could not be created:\n', dir_path))
      return()
    }
  }
  
  reorg_snapshot_files(full_snap_path, orig_dac, floats, float_dacs, snap_type, keep)
  
  # if keep is 3, everything is kept
  if (keep <= 1 || snap_type == 'bgc') {
    # the dac subdirectory should have no files left in it at this point
    unlink(snap_path_dac, recursive = TRUE)
  } else if (keep == 2) {
    # there are many empty directories below the 'dac' directory,
    # but also the tarball files that will be kept
    dir_tar <- file.path(full_snap_path, 'tarballs')
    dir.create(dir_tar)
    for (fn in fn_tar) {
      file.rename(file.path(snap_path_dac, fn), file.path(dir_tar, fn))
    }
    # the dac subdirectory should have no files left in it at this point
    unlink(snap_path_dac, recursive = TRUE)
    # rename the directory with the tarball files to 'dac'
    file.rename(dir_tar, snap_path_dac)
  }
}

