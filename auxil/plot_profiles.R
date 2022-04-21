plot_profiles <- function(Data,
                          Mdata,
                          variables,
                          method = "all", # show all profiles per variable in one plot
                          per_float = TRUE,
                          obs = "off",
                          raw = "no",
                          title_add = NULL,
                          qc_flags = 0:9) {
  
  # DESCRIPTION: 
  #   This function plots profiles of one or more specified float(s) for 
  #   the specified variable(s).
  #
  # PREREQUISITE: 
  #    Sprof file(s) for the specified float(s) must exist locally.
  #
  # INPUTS:
  #   Data      : struct that must contain the PRES field and the given
  #               variables (_ADJUSTED fields are used if available)
  #   Mdata     : struct that must contain the WMO_ID field
  #   variables : cell array with names of the measured fields (e.g., DOXY)
  #
  # OPTIONAL INPUTS :
  #  method ='all'/'mean': 'all' (default) shows one plot
  #                         per variable or 'mean'
  #                         mean and standard deviation across profiles;
  #                          
  #  per_float='TRUE'/'FALSE':   "TRUE" (default) show profiles separately for each float；
  #                               "FALSE" show all in one plot 
  #  obs = 'on'/'off' :      "off" (default") only shows the lines for each
  #                           profile; 'on' shows points on the profile at which
  #                           shows points on the profile at which
  #                           each measurement was made
  #  raw = 'yes'/'no' : plot raw, i.e., unadjusted data if set to n;
  #                      default: y (i.e., plot adjusted data if available)
  #  qc_flags          : show only values with the given QC flags (as an array)
  #                      0: no QC was performed; 
  #                      1: good data; 
  #                      2: probably good data;
  #                      3: probably bad data that are potentially correctable;
  #                      4: bad data; 
  #                      5: value changed; 
  #                      6,7: not used;
  #                      8: estimated value; 
  #                      9: missing value
  #                      default setting: 
  #                      [1,2] for adjusted data; [0:9] for raw data
  # OUTPUTS:
  #   mean_prof : mean across profiles (list of lists of column 
  #               vectors ([[variable]][[loat]]) if per_float is set to 1, 
  #               list of column vectors ({variable}) if per_float
  #               is set to 0)
  #   std_prof  : standard deviation across profiles (same type as mean_prof)
  #   mean_pres : mean pressure across profiles (list of column
  #               vectors if per_float is set to 1,
  #               column vector if per_float is 0)
  #
  # UPDATE RECORD: 
  #   Version 1 & 2:   June 2021 
  #   Version 2.1: January 2022 
  #
  # CITATION:
  #   M. Cornec (LOV, now at NOAA-PMEL), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), R. Sauzede (IMEV) and 
  #   C. Schmechtig (OSU ECCE TERRA), 2021.
  #   BGC-Argo-R: A R toolbox for accessing and visualizing Biogeochemical Argo data. 
  #   Zenodo. http://doi.org/10.5281/zenodo.5028138
  
  
  # Remove the floats that show empty variables
  index_non_empty<-NULL
  for(i in 1:length(names(Data))) {
    if(length(Data[[i]][["CYCLE_NUMBER"]])!=0){
      index_non_empty<-c(index_non_empty,i)
    }
  }
  
  Data<-Data[index_non_empty]
  Mdata<-Mdata[index_non_empty]
  
  if(length(Data)==0){
    print('No available profiles for the plot selected plot options')
    stop()
  }
  
  # empty return values in case of warnings
  mean_prof = NULL
  std_prof = NULL
  mean_pres = NULL
  
  
  if (!method %in% c("all", "mean")) {
    print('no such method! Only "all" and "mean" are possible.')
    stop()
  }
  
  floats = names(Data)
  nfloats = length(floats)
  if (!nfloats) {
    print('no floats found in Data structure')
    stop()
  }
  
  float_ids = names(Mdata)
  nvars = length(variables)
  
  if (per_float==T){
    nplots = nfloats * nvars
  } else {
    nplots =  nvars
  }
  if (nplots > Setting$max_plots) {
    print(
      'too many plots requested - use fewer profiles and/or variables,or increase Setting$max_plots if possible'
    )
    stop()
  }
  
  
  
  # unless 'raw' is specified, plot adjusted data
  if (raw == "yes") {
    xlabel_add = ' [raw values]'
  } else{
    title_add=list(list=floats)
    # names(title_add) = names(Data)
    xlabel_add = ' [adj. values]'
    for (v in 1:nvars) {
      # if all floats have adjusted values available for a variable,
      # they will be used instead of raw values
      has_adj = 0
      for (f in 1:nfloats) {
        # the "_ADJUSTED" variable usually exists, but it may not
        # be filled with actual values
        if (paste0(variables[v], "_ADJUSTED") %in% names(Data[[floats[f]]]) &
            !all(is.na(Data[[floats[f]]][[paste0(variables[v], "_ADJUSTED")]]))) {
          has_adj = has_adj + 1
          title_add[[floats[f]]][[variables[v]]] = '' 
        }
        else if (variables[v] %in% names(Data[[floats[f]]]) &
                 !all(is.na(Data[[floats[f]]][[variables[v]]]))) {
          print(
            paste0(
              'adjusted values for ',
              variables[v],
              ' for float ',
              floats[f],
              ' are not available,',
              ' showing raw values for all profiles instead'
            )
          )
          title_add[[floats[f]]][[variables[v]]] = '[raw values]'
        } else {
          print(
            paste0(variables[v],
                   ' for float ',
                   floats[f],
                   ' is not available.'
            )
          )
          has_adj = has_adj + 1
          title_add[[floats[f]]][[variables[v]]] = ''
        }
      }
      if (has_adj == nfloats) {
        variables[v] = paste0(variables[v], '_ADJUSTED')
      }
    }
  }
  
  
  Datai = NULL
  # vertical interpolation to depths with regular intervals
  for (f in 1:nfloats) {
    Datai[[floats[f]]] = depth_interp(Data[[floats[f]]], qc_flags)
  }
  
  if (per_float) {
    for (f in 1:nfloats){
      mean_pres[[f]]<-Datai[[floats[f]]]$PRES[,1]
    }
  }
  
  for (v in 1:nvars) {
    if(per_float) {
      for (f in 1:nfloats) {
        if(is.null(Data[[floats[f]]][[variables[v]]])){ # Check if the float has variable
          print(paste("No",variables[v],"available for float",floats[f]))
          next
        }
        if ('PRES_ADJUSTED' %in% names(Data[[floats[f]]])) {
          PRES = Data[[floats[f]]][['PRES_ADJUSTED']]
          if(length(which(is.finite(PRES))) < 0.5*length(PRES)){
            PRES = Data[[floats[f]]][['PRES']]
          }
        } else {
          PRES = Data[[floats[f]]][['PRES']]
        }
        
        nprofs = ncol( as.matrix((Data[[floats[f]]][['PRES']]))) # convert the matrix to avoid the error when profile number is only 1 (ncol does not work in the case of vector)
        if (per_float) {
          x11()
          par(las=1)
          this_mean_prof = get_multi_profile_mean(Datai[f], variables[v])$mean_prof
          this_std_prof = get_multi_profile_mean(Datai[f], variables[v])$std_prof
          this_mean_pres = mean_pres[[f]]
        }
        
        if (method == "all") {
          long_name = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['long_name']]
          units = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['units']]
          plot(range(Data[[floats[f]]][[variables[v]]],na.rm=T)[1],range(PRES, na.rm=T)[1],type='n', 
               ylim = rev(range(PRES, na.rm=T)),
               xlim = range(Data[[floats[f]]][[variables[v]]],na.rm=T),
               xlab = bquote(.(long_name)~.(units)~.(xlabel_add)),
               ylab = 'Pressure (dbar)')
          for (p in 1:nprofs) {
            idx = !is.na(as.matrix(Data[[floats[f]]][[variables[v]]])[, p]) &    # convert to the matrix to avoid the error when profile number is only 1 (Data[[floats[f]]][[variables[v]]])[, p]  does not work in the case of vector))   
              !is.na(as.matrix(PRES)[, p]) &
              as.matrix( Data[[floats[f]]][[paste0(variables[v], '_QC')]])[, p] %in% qc_flags   # convert to the matrix to avoid the error when profile number is only 1 (Data[[floats[f]]][[variables[v]]])[, p]  does not work in the case of vector))    
            if (sum(idx)) {
              points(
                as.matrix( Data[[floats[f]]][[variables[v]]])[idx, p],
                as.matrix(PRES)[idx, p],
                type = 'l'
              )
              if (obs == "on") {
                points(      as.matrix(Data[[floats[f]]][[variables[v]]])[idx, p], # convert to the matrix to avoid the error when profile number is only 1 (Data[[floats[f]]][[variables[v]]])[, p]  does not work in the case of vector))      
                             as.matrix(PRES) [idx, p],  # convert to the matrix to avoid the error when profile number is only 1 (Data[[floats[f]]][[variables[v]]])[, p]  does not work in the case of vector))   
                             type = 'points',
                             ylim = rev(range(as.matrix( PRES)[idx, p], na.rm=T))
                )
              }
            }else
            {
              print(
                paste0(
                  'no valid observations matching selected QC flags found for profile ',
                  p,
                  ' of float ',
                  floats[f]
                )
              )
            }
          }
          points(
            this_mean_prof,
            this_mean_pres,
            type = "l",
            lwd = 2,
            col="red"
          )
        }
        else{
          long_name = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['long_name']]
          units = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['units']]
          plot(range(Data[[floats[f]]][[variables[v]]],na.rm=T)[1],range(PRES, na.rm=T)[1],type='n', 
               ylim = rev(range(PRES, na.rm=T)),
               xlim = range(Data[[floats[f]]][[variables[v]]],na.rm=T),
               xlab = bquote(.(long_name)~.(units)~.(xlabel_add)),
               ylab = 'Pressure (dbar)')
          points(
            this_mean_prof,
            this_mean_pres,
            type = "l",
            lwd = 2
          )
          points(
            this_mean_prof - this_std_prof,
            this_mean_pres,
            type = "l",
            lwd = 2,
            col="blue"
          )
          points(
            this_mean_prof + this_std_prof,
            this_mean_pres,
            type = "l",
            lwd = 2,
            col="blue"
          )
        }
        
        title(paste('Float', Mdata[[float_ids[f]]]$WMO_NUMBER, 
                    title_add[[floats[f]]][[unlist(strsplit(variables[v],'_ADJUSTED'))]], sep = " "))
        
      }
    }
    
    if (!per_float) {
      x11()
      this_mean_prof = get_multi_profile_mean(Datai, variables[v])$mean_prof
      this_std_prof = get_multi_profile_mean(Datai, variables[v])$std_prof
      this_mean_pres = get_multi_profile_mean(Datai, variables[v])$mean_pres
      
      long_name = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['long_name']]
      units = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['units']]
      
      # Define the range limits for the global plots
      range_var<-NULL
      range_pres<-NULL
      for (f in 1:nfloats) {
        if(is.null(Data[[floats[f]]][[variables[v]]])){ # Check if the float has variable
          next
        }
        idx = Data[[floats[f]]][[paste0(variables[v], '_QC')]] %in% qc_flags # base only the range of the values from the chosen QC
        range_var<-c(range_var, range(Data[[floats[f]]][[variables[v]]][which(idx==T)],na.rm=T))
        if ('PRES_ADJUSTED' %in% names(Data[[floats[f]]])) {
          PRES = Data[[floats[f]]][['PRES_ADJUSTED']]
          if(length(which(is.finite(PRES))) < 0.5*length(PRES)){
            PRES = Data[[floats[f]]][['PRES']]
          }
        } else {
          PRES = Data[[floats[f]]][['PRES']]
        }
        idx = !is.na(Data[[floats[f]]][[variables[v]]]) &    # base only the range of PRES values according to the variable values from the chosen QC
          Data[[floats[f]]][[paste0(variables[v], '_QC')]] %in% qc_flags
        PRES<-PRES[which(idx==T)]
        range_pres<-c(range_pres,range(PRES, na.rm=T))
      }
      
      range_var<-range(range_var[which(is.finite(range_var))], na.rm=T)
      range_pres<-range(range_pres[which(is.finite(range_pres))], na.rm=T)
      
      plot(range_var[1],range_pres[1],type='n', 
           ylim = rev(range_pres),
           xlim = range_var,
           xlab = bquote(.(long_name)~.(units)~.(xlabel_add)),
           ylab = 'Pressure (dbar)')
      
      if (method == "all") {
        
        for (f in 1:nfloats) {
          if(is.null(Data[[floats[f]]][[variables[v]]])){ # Check if the float has variable
            print(paste("No",variables[v],"available for float",floats[f]))
            next
          }
          if ('PRES_ADJUSTED' %in% names(Data[[floats[f]]])) {
            PRES = Data[[floats[f]]][['PRES_ADJUSTED']]
            if(length(which(is.finite(PRES))) < 0.5*length(PRES)){
              PRES = Data[[floats[f]]][['PRES']]
            }
          } else {
            PRES = Data[[floats[f]]][['PRES']]
          }
          
          nprofs = ncol( as.matrix((Data[[floats[f]]][['PRES']]))) # convert the matrix to avoid the error when profile number is only 1 (ncol does not work in the case of vector)
          
          
          for (p in 1:nprofs) {
            idx = !is.na(as.matrix(Data[[floats[f]]][[variables[v]]])[, p]) &    # convert to the matrix to avoid the error when profile number is only 1 (Data[[floats[f]]][[variables[v]]])[, p]  does not work in the case of vector))   
              !is.na(as.matrix(PRES)[, p]) &
              as.matrix( Data[[floats[f]]][[paste0(variables[v], '_QC')]])[, p] %in% qc_flags   # convert to the matrix to avoid the error when profile number is only 1 (Data[[floats[f]]][[variables[v]]])[, p]  does not work in the case of vector))    
            if (sum(idx)) {
              points(
                as.matrix( Data[[floats[f]]][[variables[v]]])[idx, p],
                as.matrix(PRES)[idx, p],
                type = 'l'
              )
              if (obs == "on") {
                points(      as.matrix(Data[[floats[f]]][[variables[v]]])[idx, p], # convert to the matrix to avoid the error when profile number is only 1 (Data[[floats[f]]][[variables[v]]])[, p]  does not work in the case of vector))      
                             as.matrix(PRES) [idx, p],  # convert to the matrix to avoid the error when profile number is only 1 (Data[[floats[f]]][[variables[v]]])[, p]  does not work in the case of vector))   
                             type = 'points',
                             ylim = rev(range(as.matrix( PRES)[idx, p], na.rm=T))
                )
              }
            } else {
              print(
                paste0(
                  'no valid observations matching selected QC flags found for profile ',
                  p,
                  ' of float ',
                  floats[f]
                )
              )
            }
          }
          points(
            this_mean_prof,
            this_mean_pres,
            type = "l",
            lwd = 2,
            col="red"
          )
        }
      }
      else{
        points(
          this_mean_prof,
          this_mean_pres,
          type = "l",
          lwd = 2
        )
        points(
          this_mean_prof - this_std_prof,
          this_mean_pres,
          type = "l",
          lwd = 2,
          col="blue"
        )
        points(
          this_mean_prof + this_std_prof,
          this_mean_pres,
          type = "l",
          lwd = 2,
          col="blue"
        )
      }
      if (nfloats < 4) {
        ttitle = 'Floats'
        for (ff in 1:nfloats) {
          if(is.null(Data[[floats[ff]]][[variables[v]]])){ # Check if the float has variable
            next
          }
          ttitle = paste(ttitle, Mdata[[float_ids[ff]]][['WMO_NUMBER']], 
                         title_add[[floats[ff]]][[unlist(strsplit(variables[v], '_ADJUSTED'))]], sep=' ')
        }
      }
      else{
        ttitle = 'All selected floats '
      }
      title(ttitle)
    }
  }
}
