load_float_data <- function (float_ids, variables=NULL, float_profs=NULL) {
  # load_floats  This function is part of the
  # GO-BGC workshop Matlab tutorial for accessing BGC Argo float data.
  #
  # This function loads data of at least one specified float.
  #
  # Inputs:
  #   float_ids   : WMO ID of one or more floats
  #                 (if not set: a default float is used as a demo)
  #
  # Optional inputs:
  #   variables   : cell array with variable names to be loaded
  #   float_profs : cell array with IDs of selected profiles (per float,
  #                 not global)
  #
  # Output:
  #   Data        : struct with the requested variables (including QC flags.
  #                 adjusted values if available) and general ones
  #                 (LONGITUDE,LATITUDE,JULD)
  #   Mdata       : struct with meta data (WMO_NUMBER)

  # only some variables are always loaded, others only by request
  all_vars = c('CYCLE_NUMBER', 'DIRECTION', 'JULD', 'JULD_QC', 
      'JULD_LOCATION','LATITUDE','LONGITUDE','PARAMETER_DATA_MODE',
      'PARAMETER')

  if (!is.null(variables)) {
    variables[length(variables)+1] = 'PRES'
    all_vars[length(all_vars)+1] = 'PROFILE_PRES_QC'
  } 

  # INITIALIZE DATA FRAME FOR Data OUTPUT
  Data = NULL
  Mdata = NULL

  add_vars = is.element(Setting$avail_vars, variables)
  new_vars = Setting$avail_vars[add_vars]

  # always include all associated variables
  if (!all(add_vars == FALSE)) { # There is something to add 
    for ( i in 1:length(new_vars) ) {
      all_vars[length(all_vars)+1] = new_vars[i]
      all_vars[length(all_vars)+1] = paste0(new_vars[i], '_QC')
      if (new_vars[i] != 'PRES') {
        all_vars[length(all_vars)+1] = paste0(new_vars[i], '_dPRES')
      }
      all_vars[length(all_vars)+1] = paste0(new_vars[i], '_ADJUSTED')
      all_vars[length(all_vars)+1] = paste0(new_vars[i], '_ADJUSTED_QC')
      all_vars[length(all_vars)+1] = paste0(new_vars[i], '_ADJUSTED_ERROR')
    }
  } ## enf of something to add

  # download Sprof files if necessary
  good_float_ids = download_multi_floats(float_ids)

  # LOOP TO IMPORT PROFILES AND EXTRACT VARIABLES
  for ( n in 1:length(good_float_ids)) {
    floatnum = good_float_ids[n] 
    filename = paste0(Setting$prof_dir, floatnum,'_Sprof.nc')
    FWMO = paste0('F',floatnum)
    
    # LOAD VARIABLES FROM FILE
    info = nc_open(filename)	
    dims = info$dim

    n_prof = dims$N_PROF$len
    n_param = dims$N_PARAM$len
    n_levels = dims$N_LEVELS$len

    amt = length(all_vars)

    names = all_vars
    mnames = all_vars

    Data[[FWMO]] = NULL
    Mdata[[FWMO]] = NULL

    for (l in 1:amt) {

      # Read in data
	    Data[[FWMO]][[names[l]]] = ncvar_get(info,names[l])
      Mdata[[FWMO]][[mnames[l]]] = Data[[FWMO]][[names[l]]]

      # For measured variables
      if (length(info$var[[names[l]]]$dimids) == 2 &&
          all(info$var[[names[l]]]$dimids == 
              c(dims$N_LEVELS$id, dims$N_PROF$id)) ) {
        
        # Remove metadata fields
        Mdata[[FWMO]][[mnames[l]]] = NULL
        mnames[l] = NA
        
        # For descriptive meta variables (1 value per profile)
      } else if (length(info$var[[names[l]]]$dimids) == 1 && 
                 all(info$var[[names[l]]]$dimids == c(dims$N_PROF$id)) ) {

        if (names[l] == "DIRECTION") {
          Data[[FWMO]][[names[l]]] = 
            matrix(rep(unlist(strsplit(Data[[FWMO]][[names[l]]], "")), each=n_levels),
                   nrow=n_levels, ncol=n_prof)
        } else {
          # transform one column variables into matrix 
          Data[[FWMO]][[names[l]]] = 
              matrix(rep(Data[[FWMO]][[names[l]]],each=n_levels),
                     nrow=n_levels, ncol=n_prof)
        }
        
        # Remove metadata fields
        Mdata[[FWMO]][[mnames[l]]] = NULL
        mnames[l] = NA
        
      # For informational meta variables
      } else {
        # Save in metadata structure
        Mdata[[FWMO]][[mnames[l]]] = Data[[FWMO]][[names[l]]]
        
        # Remove data fields
        Data[[FWMO]][[mnames[l]]] = NULL
            
        names[l] = NA
      }
    } ## end loop amt

    # Remove unused variable names
    names = names[!is.na(names)]
    mnames = mnames[!is.na(mnames)]
    
    # Add WMO float number to metadata
    Mdata[[FWMO]]$WMO_NUMBER = floatnum
    
    # CONVERT QUALITY FLAGS TO NUMERIC FORMAT
    for (l in 1:length(names)) {
      if (endsWith(names[l],'_QC') &&  # Check for QC identifier
          !startsWith(names[l],'PROF')) { # But not a profile QC
        # Vectorize
        Data[[FWMO]][[names[l]]] = 
            unlist(strsplit(Data[[FWMO]][[names[l]]], split=""))
        # Replace blanks with zeros
        Data[[FWMO]][[names[l]]] = gsub(" ", "0", Data[[FWMO]][[names[l]]])
        # Convert to numeric
        Data[[FWMO]][[names[l]]] = as.numeric(Data[[FWMO]][[names[l]]])
        # Reshape
        Data[[FWMO]][[names[l]]] = 
            matrix(Data[[FWMO]][[names[l]]], nrow=n_levels, ncol=n_prof)
      }
    }
    
    # parse parameter names
    for (l in 1:length(mnames)) {
      if (mnames[l] == 'PARAMETER') {
        # extract parameter names as coherent strings
	       temp=rep(NA,n_param)
        for ( m in 1:n_param ) {
          temp[m] = trim(Mdata[[FWMO]][[mnames[l]]][m,1,1])
        }
        params_keep = is.element(temp, new_vars)
        Mdata[[FWMO]][[mnames[l]]] = temp[params_keep]
      }
    }
      
    # parse parameter data modes
    for (l in 1:length(mnames)) {
      if (mnames[l] == 'PARAMETER_DATA_MODE'){
        # create data mode variable for each parameter
        # expand that variable to match size of data matrix
        z=1
        for (m in 1:n_param) {
          if (params_keep[m]) { 
            Data[[FWMO]][[paste0(Mdata[[FWMO]]$PARAMETER[z],'_DATA_MODE')]] = 
                matrix(
                    rep(substr(Mdata[[FWMO]][[mnames[l]]],m,m), each=n_levels),
                    nrow=n_levels, ncol=n_prof)
            z=z+1
          }
        }
      }
    }

    # clear both parameter and parameter data mode from metadata
    Mdata[[FWMO]]$PARAMETER = NULL
    Mdata[[FWMO]]$PARAMETER_DATA_MODE = NULL
 
    # CONVERT JULD VARIABLE TO SERIAL DATE (SINCE YEAR 1950)
    # AND SAVE AS 'TIME'
    ## CCC to check date format

    #Data[[FWMO]]$TIME = matrix(as.character(as.Date(Data[[FWMO]]$JULD, origin=as.Date("1950-01-01"))),
    #                           nrow=n_levels, ncol=n_prof)
    Data[[FWMO]]$TIME = matrix(nrow=n_levels, ncol=n_prof, 
      as.character(as.POSIXct(Data[[FWMO]]$JULD*3600*24, 
                              origin=as.Date("1950-01-01"), tz="UTC"))
      )

    names[length(names)+1] = 'TIME' # Add 'TIME' to list of variable names
    
    
    if (!is.null(float_profs)) {
      for (l in 1:length(names)) {
        # Select only specified profiles
        Data[[FWMO]][[names[l]]] = Data[[FWMO]][[names[l]]][,float_profs[[n]]]
      }
    }
    
    nc_close(info)
    
  } # end loop good floats
  
  return(list(Data=Data, Mdata=Mdata))
}
