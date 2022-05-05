
plot_sections <- function(Data, 
                          Mdata, 
                          variables, 
                          nvars, 
                          plot_isopyc, 
                          plot_mld, 
                          max_depth=NULL, 
                          raw="no", 
                          obs="off",
                          qc_flags=0:9) {

  # DESCRIPTION:
  #   This function plots sections of one or more specified float(s) for
  #   the specified variable(s).
  #
  # PREREQUISITES: 
  #   Sprof file(s) for the specified float(s) must exist locally.
  #
  # INPUTS:
  #   Data        : struct that must contain the PRES field and the given
  #                 variables (_ADJUSTED fields are used if available)
  #   Mdata       : struct that must contain the WMO_ID field
  #   variables   : cell array with names of the measured fields (e.g., DOXY)
  #   nvars       : only the first "nvars" variables from the Data field
  #                 will be plotted (if plotting isopycnal lines and/or
  #                 mixed layer depth is requested, TEMP and PSAL may have
  #                 been added to the "variables" cell array)
  #   plot_isopyc : if set to 1, isopycnal lines will be plotted  
  #   plot_mld    : if set to 1 or 2, mixed layer depth will be plotted,
  #                 using either a temperature (1) or density (2) criterion
  #   time_label  : either 'year' or 'month' - type of time labeling on
  #                 the x-axis
  #   max_depth   : maximum depth to plot (an empty array signals the
  #                 plotting of all available depths)
  #   obs         : if 'on', add dots at the depths of observations
  #                 each measurement was made (default = 'no')
  #   raw         : if 'no', use adjusted variables if available,
  #                 if 'yes', always use raw values
  #
  # OPTIONAL INPUTS:
  #  'qc',flags   : show only values with the given QC flags (array)
  #                 0: no QC was performed; 
  #                 1: good data; 
  #                 2: probably good data;
  #                 3: probably bad data that are 
  #                    potentially correctable;
  #                 4: bad data; 
  #                 5: value changed; 
  #                 6,7: not used;
  #                 8: estimated value; 
  #                 9: missing value
  #                 default setting: [1,2]
  #                 See Table 7 in Bittig et al.:
  #                 https://www.frontiersin.org/files/Articles/460352/fmars-06-00502-HTML-r1/image_m/fmars-06-00502-t007.jpg
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
  
  
  floats = names(Data)
  float_ids = names(Mdata)
  nfloats = length(floats)
  # note that Data may contain additional variables (e.g., TEMP and PSAL,
  # which are needed to compute density, but plotting their sections was
  # not requested - they will not be counted in nvars)
  nplots = nfloats * nvars
  if ( nplots > Setting$max_plots ) {
    warning("too many plots requested - use fewer profiles and/or variables\n",
            "or increase Settings.max_plots if possible")
    return(1)
  }
  
  calc_dens = plot_isopyc
  
  # unless 'raw' is specified, plot adjusted data
  if ( raw == "yes" ) {
    title_add = ' [raw values]'
  } else {
    title_add = ''
    for ( v in 1:nvars ) {
      # if all floats have adjusted values available for a variable,
      # they will be used instead of raw values
      has_adj = 0
      for ( f in 1:nfloats ) {
        # the "_ADJUSTED" variable usually exists, but it may not
        # be filled with actual values
        if ( (paste0(variables[v], "_ADJUSTED") %in% names(Data[[floats[f]]])) &
            any(
              is.finite(Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED')]])
              ) ) {
          has_adj = has_adj + 1
         } else if (variables[v] %in% names(Data[[floats[f]]]) &
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
      if ( has_adj == nfloats ) {
        variables[v] = paste0(variables[v], '_ADJUSTED')
      }
    }
  }
  
  # vertical interpolation to depths with regular intervals
  for ( f in 1:nfloats ) {
    
    prs_res = 2
    
    Datai = depth_interp(Data[[floats[f]]], qc_flags, calc_dens=calc_dens, 
                calc_mld_temp=(plot_mld==1), calc_mld_dens=(plot_mld==2),
                prs_res=prs_res)
    
    df = NULL
    for (name in names(Datai)) {
      if (name == "TIME") {
        df[[name]] = as.POSIXct(Datai[[name]], tz="UTC")
      } else {
        df[[name]] = as.vector(Datai[[name]])
      }
    }
    df = data.frame(df)
    
    # Create min/max parameters to use the geom_rect() function that can plot
    # rectangles with variable width/height
    
    if ("PRES_ADJUSTED" %in% names(Datai)) {
      df$ymin = df$PRES_ADJUSTED - prs_res/2
      df$ymax = df$PRES_ADJUSTED + prs_res/2
    } else {
      df$ymin = df$PRES - prs_res/2
      df$ymax = df$PRES + prs_res/2
    }
    
    nna = which(!is.na(Datai$TIME[1,]))
    nc = length(nna)
    
    xvec = as.POSIXct(Datai$TIME[1,nna], tz="UTC")
    xmin = as.POSIXct(rep(NA, nc), tz="UTC")
    xmax = as.POSIXct(rep(NA, nc), tz="UTC")

    xmin[2:(nc-1)] = xvec[2:(nc-1)] - ( xvec[2:(nc-1)] - xvec[1:(nc-2)] ) / 2
    xmax[2:(nc-1)] = xvec[2:(nc-1)] + ( xvec[3:nc] - xvec[2:(nc-1)] ) / 2
    
    xmin[1] = xvec[1] - ( xvec[2] - xvec[1] ) / 2
    xmax[nc] = xvec[nc] + ( xvec[nc] - xvec[nc-1] ) / 2
    xmin[nc] = xmax[nc-1]
    xmax[1] = xmin[2]
    
    full_xmin = as.POSIXct(rep(NA, ncol(Datai$TIME)), tz="UTC")
    full_xmax = as.POSIXct(rep(NA, ncol(Datai$TIME)), tz="UTC")
    full_xmin[nna] = xmin
    full_xmax[nna] = xmax

    df$xmin = rep(full_xmin, each=nrow(Datai$TIME))
    df$xmax = rep(full_xmax, each=nrow(Datai$TIME))
    
    for ( v in 1:nvars ) {
      if(is.null(Data[[floats[f]]][[variables[v]]])){ # Check if the float has variable
        print(paste("No",variables[v],"available for float",floats[f]))
        next
      }
      
      if ("PRES_ADJUSTED" %in% names(Datai)) {
        g1 = ggplot(df, aes(x=TIME, y=PRES_ADJUSTED))
      } else {
        g1 = ggplot(df, aes(x=TIME, y=PRES))
      }
      
      g1 = g1 +
        geom_rect(aes(fill=.data[[variables[v]]], xmin=xmin, xmax=xmax,
                      ymin=ymin, ymax=ymax)) +
        scale_fill_viridis_c() +
        theme_bw()
      
      name_units = get_var_name_units(variables[v])
      long_name = name_units$long_name
      units = name_units$units
      
      if (obs == "on") {
        index = which(!is.na(Data[[floats[f]]][[variables[v]]]))
        g1 = g1 + geom_point(aes(x = x, y = y),
                             data = data.frame(x = as.POSIXct(Data[[floats[f]]]$TIME[index], tz="UTC"),
                                               y = as.vector(Data[[floats[f]]]$PRES[index])),
                             size=0.1, alpha=0.2
        )
      }
      
      if (plot_mld == 1) {
        g1 = g1 + geom_line(aes(x = x, y = y),
                            data = data.frame(x = as.POSIXct(Datai$TIME[1,], tz="UTC"),
                                              y = as.vector(Datai$MLD_TEMP)),
                            size=2
        )
      } else if (plot_mld == 2 & "MLD_DENS" %in% names(Datai)) {
        # some old core floats don't have PSAL and therefore
        # density cannot be computed
        g1 = g1 + geom_line(aes(x = x, y = y),
                            data = data.frame(x = as.POSIXct(Datai$TIME[1,], tz="UTC"),
                                              y = as.vector(Datai$MLD_DENS)),
                            size=2
        )
      }
      
      if ( plot_isopyc ) {
        if ("DENS_ADJUSTED" %in% names(Datai)) {
          g1 = g1 + geom_contour(aes(z = DENS_ADJUSTED), color="red") +
            geom_label_contour(aes(z = DENS_ADJUSTED))
        } else {
          g1 = g1 + geom_contour(aes(z = DENS), color="red") +
            geom_label_contour(aes(z = DENS))
        }
      }

      if ( !is.null(max_depth) ) {
        g1 = g1 + scale_y_reverse(limits = c(max_depth, 0))
      } else {
        g1 = g1 + scale_y_reverse()
      }
      
      g1 = g1 +
        labs(title = paste0("Float ", Mdata[[float_ids[f]]]$WMO_NUMBER,": ",
                            long_name, title_add),
             x = "Time",
             y = "Pressure (dbar)",
             fill = units)
      
      x11()
      plot(g1)
    }
  }
  
  
}
