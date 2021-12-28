plot_profiles <- function(Data,
                          Mdata,
                          variables,
                          method = "all", # show all profiles per variable in one plot
                          per_float = 1,
                          obs = "off",
                          raw = "no",
                          title_add = NULL,
                          qc_flags = 0:9) {
  # plot_profiles  
  #
  #This function is part of the
  # GO-BGC workshop Matlab tutorial for accessing BGC Argo float data.
  #
  # This function plots profiles of one or more specified float(s) for 
  # the specified variable(s).
  #
  # Prerequisite: Sprof file(s) for the specified float(s) must exist locally.
  #
  # Inputs:
    #   Data      : struct that must contain the PRES field and the given
  #               variables (_ADJUSTED fields are used if available)
  #   Mdata     : struct that must contain the WMO_ID field
  #   variables : cell array with names of the measured fields (e.g., DOXY)
  # OPTIONAL INPUTS :
  #  'obs',on/off  : by default (obs='off') only lines are shown for each
  #                  profile; 'obs','on' shows points on the profile at which
  #                  each measurement was made
  #  'raw', yes/no     : plot raw, i.e., unadjusted data if set to n;
  #                  default: y (i.e., plot adjusted data if available)
  #  'qc_flags'    : show only values with the given QC flags (as an array)
  #                  0: no QC was performed; 
  #                  1: good data; 
  #                  2: probably good data;
  #                  3: probably bad data that are potentially correctable;
  #                  4: bad data; 
  #                  5: value changed; 
  #                  6,7: not used;
  #                  8: estimated value; 
  #                  9: missing value
  #                  default setting: 
  #                  [1,2] for adjusted data; [0:9] for raw data
  #
  # CITATION:
  # BGC-Argo-R: A R toolbox for accessing and visualizing
  # Biogeochemical Argo data,
  #
  # AUTHORS: 
  # M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), 
  # R. Sauzede (IMEV) and C. Schmechtig (OSU ECCE TERRA),
  #
  # Adapted from the Matlab toolbox BGC-Argo-Mat:  https://doi.org/10.5281/zenodo.4971318
  # (H. Frenzel, J. Sharp, A. Fassbender (NOAA-PMEL),
  # J. Plant, T. Maurer, Y. Takeshita (MBARI), D. Nicholson (WHOI),
  # and A. Gray (UW))
  
  # Update 24 June 2021


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
  nplots = nfloats * nvars
  if (nplots > Setting$max_plots) {
    print(
      'too many plots requested - use fewer profiles and/or variables/nor increase Setting$max_plots if possible'
    )
    stop()
  }
  
  # unless 'raw' is specified, plot adjusted data
  if (raw == "yes") {
    xlabel_add = ' [raw values]'
  } else{
    title_add=list(list=floats)
    # names(title_add) = names(Data)
    xlabel_add = ''
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
        else{
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
  
  # mean over multiple floats is treated separately below
  if (per_float | nfloats == 1 | method == 'all') {
    for (v in 1:nvars) {
      if (!per_float & nfloats > 1) {
        x11()
        mean_prof = get_multi_profile_mean(Datai, variables[v])$mean_prof
        std_prof = get_multi_profile_mean(Datai, variables[v])$std_prof
        mean_pres = get_multi_profile_mean(Datai, variables[v])$mean_pres
      }
      for (f in 1:nfloats) {
        if ('PRES_ADJUSTED' %in% names(Data[[floats[f]]])) {
          PRES = Data[[floats[f]]][['PRES_ADJUSTED']]
        }
        else{
          PRES = Data[[floats[f]]][['PRES']]
        }
        
        nprofs = ncol( as.matrix((Data[[floats[f]]][['PRES']]))) # convert the matrix to avoid the error when profile number is only 1 (ncol does not work in the case of vector)
        if (per_float | nfloats == 1) {
          x11()
          par(las=1)
          mean_prof = rowMeans(Datai[[floats[f]]][[variables[v]]], na.rm = T)
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
              else{
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
          }
          if (per_float | nfloats == 1) {
            points(
                mean_prof,
                Datai[[floats[f]]][['PRES']][, 1],
                type = "l",
                lwd = 2,
                col="red"
              )
            }
          else{
              points(
                mean_prof,
                mean_pres,
                type = 'l',
                lwd = 2,
                col="red"
              )
          }
        }
        if (method == "mean") {
          long_name = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['long_name']]
          units = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['units']]
          std_prof = apply(Datai[[floats[f]]][[variables[v]]], 1, function(x) {sd(x, na.rm = T)})
          plot(range(mean_prof-std_prof)[1],range(PRES, na.rm=T)[1], type='n', 
               ylim = rev(range(PRES, na.rm=T)),
               xlim = range(c(mean_prof-std_prof, mean_prof+std_prof),na.rm=T),
               xlab = bquote(.(long_name)~.(units)~.(xlabel_add)),
               ylab = 'Pressure (dbar)')
          
          points(
            mean_prof,
            Datai[[floats[f]]]$PRES[, 1],
            type = "l",
            lwd = 2
          )
          points(
            mean_prof - std_prof ,
            Datai[[floats[f]]]$PRES[, 1],
            type = "l",
            lwd = 2,
            col = "blue"
          )
          points(
            mean_prof + std_prof,
            Datai[[floats[f]]]$PRES[, 1],
            type = "l",
            lwd = 2,
            col = "blue"
          )
        }
        # dev.off()
        if (per_float==1) {
           title(paste('Float', Mdata[[float_ids[f]]]$WMO_NUMBER, 
                      title_add[[floats[f]]][[unlist(strsplit(variables[v],'_ADJUSTED'))]], sep = " "))
          }
        else{
              if (nfloats < 4) {
                ttitle = 'Floats '
                for (ff in 1:nfloats) {
                  ttitle = paste(ttitle, Mdata[[float_ids[ff]]]$WMO_NUMBER, 
                                  title_add[[floats[f]]][[unlist(strsplit(variables[v],'_ADJUSTED'))]], sep=" ")
                }
              }
              else{
                ttitle = 'All selected floats '
              }
              title(paste(ttitle, title_add[[floats[f]]][[unlist(strsplit(variables[v],'_ADJUSTED'))]], sep = " "))
            }
      }
    }
  }
    
  else{
      # Mean over multiple floats
      for (v in 1:nvars) {
        mean_prof = get_multi_profile_mean(Datai, variables[v])$mean_prof
        std_prof = get_multi_profile_mean(Datai, variables[v])$std_prof
        mean_pres = get_multi_profile_mean(Datai, variables[v])$mean_pres
        
        long_name = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['long_name']]
        units = get_var_name_units(unlist(strsplit(variables[v],'_ADJUSTED')))[['units']]
        
        x11()
        plot(
          mean_prof,
          mean_pres,
          type = 'lines',
          ylim = rev(range(mean_pres)),
          xlim = range(c(mean_prof-std_prof,mean_prof+std_prof)),
          xlab = bquote(.(long_name)~.(units)~.(xlabel_add)),
          ylab = 'Pressure (dbar)'
        )
        points(mean_prof - std_prof,
               mean_pres,
               type = 'lines',
               col = 'blue')
        points(mean_prof + std_prof,
               mean_pres,
               type = 'lines',
               col = 'blue')
        # dev.off()
        if (nfloats < 4) {
          ttitle = 'Floats'
          for (ff in 1:nfloats) {
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
    