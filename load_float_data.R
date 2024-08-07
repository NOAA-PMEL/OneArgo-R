load_float_data <- function (float_ids, variables=NULL, float_profs=NULL,format=NULL) {
  
  
  # DESCRIPTION: 
  #   This function loads data (at least one variable)
  #   of at least one specified float.
  #
  # INPUTS:
  #   float_ids: WMO ID of one or more floats
  #
  # OPTIONAL INPUTS:
  #   variables: cell array with variable names to be loaded (use 'ALL'
  #                  # to load all available variables, which may differ by
  #                  float)
  #   float_profs : float profile is an array with the per-float indices 
  #                 as returned by function "select_profiles" 
  #                
  #   format= 'dataframe' :    
  #                 specific the format of data loaded (use 'dataframe'
  #                 to export the data in the format of "data frame" 
  #                 (if not set, the default output is a "list" with multiple matrix)
  #
  # OUTPUTS:
  #   Output_1 (if "format" option is not be specified):
  #    Data        : struct with the requested variables (including QC flags.
  #                 adjusted values if available) and general ones
  #                 (LONGITUDE,LATITUDE,JULD)
  #    Mdata       : struct with meta data (WMO_NUMBER)
  #
  #   Output_2 (if "format" option is set to "dataframe"):
  #    Data: A data frame including all floats data
  #          Note: The data will be exported as a 
  #          list containing multiple data frame for each float data if 
  #          the length of merged data frame exceeds the  
  #          memory limit of data frame. 
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
  
  
  
  if (exists("Setting")==F) {
    initialize_argo()
  }
  
  add_pres = 0;# Default: do not add 'PRES' to list of variables
  
  
  
  if ( is.null   (variables)  ){ # Load all the parameters if "variables" are not specific
    
    variables="ALL"
    
  } else{
    if (  !is.element('PRES', variables)  ){
      add_pres = 1;
    }
  }
  
  if ( is.null   (format)  ){ # Set to export the data in the format of listif "format" are not specific
    
    format="list"
    
  }
  
  
  # only some variables are always loaded, others only by request
  
  base_vars = c('CYCLE_NUMBER', 'DIRECTION', 'JULD', 'JULD_QC', 
                'JULD_LOCATION','LATITUDE','LONGITUDE','POSITION_QC',
                'PARAMETER_DATA_MODE','PARAMETER','DATA_MODE'
  )
  
  if ( any(variables=="ALL")  ) { 
    use_all_vars = 1;
    base_vars[length(base_vars )+1] = 'PROFILE_PRES_QC';
    
  } else{
    
    use_all_vars = 0
    
    if (add_pres==1){ #if no variables are specified (e.g., to plot trajectories only),
      # loading pressure and associated variables is not needed
      variables[length(variables )+1] = 'PRES';
      base_vars[length(base_vars )+1] = 'PROFILE_PRES_QC';
    }
    
    add_vars = is.element(Setting$avail_vars , variables);
    new_vars = Setting$avail_vars [add_vars];
    all_vars = combine_variables(base_vars, new_vars)
    all_vars = unique(all_vars)
    
  }
  
  
  # INITIALIZE DATA FRAME FOR Data OUTPUT
  Data = NULL
  Mdata = NULL
  
  # download Sprof files if necessary
  good_float_ids = download_multi_floats(float_ids)
  
  
  
  # LOOP TO IMPORT PROFILES AND EXTRACT VARIABLES
  for ( n in 1:length(good_float_ids)) {
    floatnum = good_float_ids[n] 
    is_bgc_float = Float$type[which(Float$wmoid==floatnum)]=="bgc"
    FWMO = paste0('F',floatnum)
    filename = paste0(Setting$prof_dir, Float$file_name[which(Float$wmoid==floatnum)])
    
    
    # LOAD VARIABLES FROM FILE
    info = nc_open(filename)	 #  Read netcdf information
    
    
    if ( use_all_vars==1 ){ # Load the all variables 
      
      these_vars =  names(info$var) 
      add_vars = is.element(Setting$avail_vars, these_vars)
      new_vars = Setting$avail_vars[add_vars]
      all_vars = combine_variables(base_vars, new_vars)
      all_vars = unique(all_vars)
      
      
    }
    
    # Find 'number of profiles', 'number of parameters', and 'number of
    # depth levels'
    dims=get_dims(filename)
    n_prof = dims$n_prof
    n_param =dims$n_param
    n_levels = dims$n_levels
    
    amt = length(all_vars)
    
    names = all_vars
    mnames = all_vars
    
    Data[[FWMO]] = NULL
    Mdata[[FWMO]] = NULL
    
    for (l in 1:amt) {
      
      if (is_bgc_float==F) {
        if (all_vars[l]== 'PARAMETER_DATA_MODE' | 
            endsWith(all_vars[l],'dPRES')){
          next # these variables are only in Sprof files
        }
      }
      if (is_bgc_float==T) {
        if(all_vars[l]== 'DATA_MODE'){
          next # these variables are only in prof files
        }
      }
      
      # Catch up the error 
      if (!names[l] %in% names(info$var) ){
        print (paste(names[l], 'not found in', FWMO)) # 防止读到没有的变量
        mnames[l] = NA
        names[l] = NA
      }
      
      if (names[l] %in% names(info$var) ){ # check if the variables in the Netcdf file 
        
        if (names[l] == 'DATA_MODE') {
          # emulate the set up for core/deep floats
          Mdata[[FWMO]][["PARAMETER_DATA_MODE"]]=unlist(strsplit(ncvar_get(info,names[l]), split=""))
        }else{
          Data[[FWMO]][[names[l]]] =ncvar_get(info,names[l])
          Mdata[[FWMO]][[mnames[l]]] = Data[[FWMO]][[names[l]]]
        }
        
        # CONVERT QUALITY FLAGS TO NUMERIC FORMAT
        if (endsWith(names[l],'_QC') &&  # Check for QC identifier
            !startsWith(names[l],'PROF')) { # But not a profile QC
          
          if(length(info$var[[names[l]]]$size) == 2 &&
             all(info$var[[names[l]]]$size == 
                 c(dims$n_levels, dims$n_prof))){
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
        
        # For measured variables
        if (length(info$var[[names[l]]]$size) == 2 &&
            all(info$var[[names[l]]]$size == 
                c(dims$n_levels, dims$n_prof))) {
          
          if(names[l] !="PARAMETER_DATA_MODE"){
            # Remove metadata fields
            Mdata[[FWMO]][[mnames[l]]] = NULL
            mnames[l] = NA
          }
          
          # For descriptive meta variables (1 value per profile)
        } else if (length(info$var[[names[l]]]$size) == 1 && 
                   all(info$var[[names[l]]]$size == c(dims$n_prof)) ) {
          
          if (names[l] == "DIRECTION" |  # Check for QC identifier
              startsWith(names[l],'PROF')) {       # transform qc variables into matrix for "direction_qc" and "profile_qc) 
            Data[[FWMO]][[names[l]]] = 
              matrix(rep(unlist(strsplit(Data[[FWMO]][[names[l]]], "")), each=n_levels),
                     nrow=n_levels, ncol=n_prof)
          } else {
            if(endsWith(names[l],'_QC')){ # numeric QC for JULD_QC, POSITION_QC
              Data[[FWMO]][[names[l]]] = 
                matrix(rep(as.numeric(unlist(strsplit(Data[[FWMO]][[names[l]]], ""))), each=n_levels),
                       nrow=n_levels, ncol=n_prof)
            } else if (names[l]!="DATA_MODE"){
              # transform one column variables into matrix 
              Data[[FWMO]][[names[l]]] = 
                matrix(rep(Data[[FWMO]][[names[l]]],each=n_levels),
                       nrow=n_levels, ncol=n_prof)
            }
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
      } 
    } ## end loop amt
    
    
    
    # Remove unused variable names
    names = names[!is.na(names)]
    mnames = mnames[!is.na(mnames)]
    
    
    if("PARAMETER" %in% mnames){
      temp=NULL
      Mdata[[FWMO]][['PARAMETER_SELECT']]<-NULL
      for(np in 1:n_prof){
        temp[[paste0("P",np)]]<-NULL
        for ( m in 1:n_param ) {
          temp[[paste0("P",np)]][m] = trim(Mdata[[FWMO]][['PARAMETER']][m,1,np])
        }
        params_keep = is.element(temp[[paste0("P",np)]], new_vars)
        Mdata[[FWMO]][['PARAMETER_SELECT']][[paste0("P",np)]] = temp[[paste0("P",np)]][params_keep]
      }
    }
    
    if("PARAMETER_DATA_MODE" %in% mnames){
      # create data mode variable for each parameter
      # expand that variable to match size of data matrix
      # Initalize empty list
      for(m in new_vars){
        Data[[FWMO]][[paste0(m,'_DATA_MODE')]] =
          matrix(
            rep(NA, each=n_levels),
            nrow=n_levels, ncol=n_prof)
      }
      
      for(np in 1:n_prof){
        z=1
        params_keep = is.element(temp[[paste0("P",np)]], new_vars)
        for (m in 1:n_param) {
          if (params_keep[m]) {
            if(is_bgc_float==T){ #for BGC Argo floats. Initalize empty list
              Data[[FWMO]][[paste0(Mdata[[FWMO]]$PARAMETER_SELECT[[np]][z],'_DATA_MODE')]][,np] = 
                substr(Mdata[[FWMO]][["PARAMETER_DATA_MODE"]][np],m,m)
              z=z+1
            } else { # for core/deep. Extract the parameter data mode
              Data[[FWMO]][[paste0(Mdata[[FWMO]]$PARAMETER_SELECT[[np]][z],'_DATA_MODE')]][,np]= 
                Mdata[[FWMO]][["PARAMETER_DATA_MODE"]][np]
              z=z+1
            }
          }
        }
      }
    }
  
    # clear both parameter and parameter data mode from metadata
    Mdata[[FWMO]]$PARAMETER = NULL
    Mdata[[FWMO]]$PARAMETER_SELECT = NULL
    Mdata[[FWMO]]$PARAMETER_DATA_MODE = NULL
    Data[[FWMO]]$PARAMETER_DATA_MODE = NULL
    
    
    # add information about deploying organization and PI to meta data
    Mdata[[FWMO]]$WMO_NUMBER = floatnum
    Mdata[[FWMO]]$ PROJECT_NAME  =   ncvar_get(info,"PROJECT_NAME")[1] 
    Mdata[[FWMO]]$ PI_NAME  = ncvar_get(info,"PI_NAME")[1] 
    Mdata[[FWMO]]$ DATA_CENTRE  = ncvar_get(info,"DATA_CENTRE")[1]   
    
    
    # CONVERT JULD VARIABLE TO SERIAL DATE (SINCE YEAR 1950)
    # AND SAVE AS 'TIME'
    ## CCC to check date format
    
    Data[[FWMO]]$TIME = matrix(as.character(as.Date(Data[[FWMO]]$JULD, origin=as.Date("1950-01-01"))),
                               nrow=n_levels, ncol=n_prof)
    # Data[[FWMO]]$TIME = 
    #  as.Date(as.POSIXct(Data[[FWMO]]$JULD*3600*24, 
    #        origin=as.Date("1950-01-01"), tz="UTC"))
    # )   # (Since year 1950)
    
    names<-names(Data[[FWMO]])
    
    if (!is.null(float_profs)) {
      for (l in 1:length(names)) {
        if(n_prof!=1){ #in case of the float having only one profile
          # Select only specified profiles
          Data[[FWMO]][[names[l]]] = Data[[FWMO]][[names[l]]][,float_profs[[n]]]
        }
      }
    }
    
    nc_close(info)
    
    print(paste("Progress:",n/length(good_float_ids)*100 ,"%" ))
    
  } # end loop good floats
  
  
  if (format!="dataframe"){
    return(list(Data=Data, Mdata=Mdata))
  }
  
  
  if (format=="dataframe"){ # convert the data into the data frame format 
    
    float_data_list_dtfr= vector("list",
                                 length(Data)
    )# Create a list to store the multiple data frame for each float data
    
    
    for (i in 1:length(Data)  ){ # loop for each float data 
      
      float_data_single=Data[[i]] # Pull out each float
      
      length_float_data=length(  float_data_single$CYCLE_NUMBER)
      number_variable_float_data=length(float_data_single)
      
      
      # create a matrix to deposite the float data
      float_data_single_dtfr= matrix (nrow=   length_float_data,
                                      ncol=   number_variable_float_data
                                      
      ) 
      float_data_single_dtfr=as.data.frame(  float_data_single_dtfr)
      colnames(float_data_single_dtfr) = names(float_data_single) # names the data frame
      
      
      # loop to tranform the each variable 
      for (ii in 1:   number_variable_float_data ){  
        
        
        float_data_single_dtfr[,ii]=as.vector(float_data_single[[ii]])
        
        
      } # end loop in number_variable_float_data
      
      
      float_data_single_dtfr$WMOID= names(Data[i])  # add the WMOID in data frame 
      
      
      #  assign data frame into the list array 
      float_data_list_dtfr[[i]]=  float_data_single_dtfr 
      names(  float_data_list_dtfr)[i] <-  names(Data[i]) #  name the each element in list 
      
    }# end loop in float_data
    
    # merge the multiple data frame into the single one
    tryCatch( {
      
      float_data_list_dtfr=bind_rows(float_data_list_dtfr)
    },  error = function(e){
      
      print("data exceeds the memorylimit of dataframe so data isinput as a list containing multiple data frame for each float ")
    }
    )
    
    
    return( float_data_list_dtfr)
    
    
  } # end loop format=="dataframe"
}
