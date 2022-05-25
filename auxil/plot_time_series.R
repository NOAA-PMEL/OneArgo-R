
plot_time_series <- function(Data, 
                          Mdata, 
                          plot_depth,
                          variables, 
                          nvars, 
                          raw, 
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
  #   plot_depth :  espefic the depth for plotting the time series 
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
        } else {
          warning("adjusted values for ", variables[v], " for float ", 
                  floats[f], 
                  " are not available, showing raw value profiles instead")
          title_add = ' [raw values]'
        }
      }
      if ( has_adj == nfloats ) {
        variables[v] = paste0(variables[v], '_ADJUSTED')
      }
    }
  } # end for the "if ( raw == "yes") "
  
  
  # vertical interpolation to depths with regular intervals
  float_data_depth_specific=  as.vector(nfloats,"list")# create a list to store the depth-resolved data
  for ( f in 1:nfloats ) {
    
    prs_res = 2
    
    Datai = depth_interp(Data=Data[[floats[f]]], 
                         qc_flags, 
                         calc_dens=1, 
                         calc_mld_temp=1, 
                         calc_mld_dens=1,
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
    
    
    
    
    
    # extract the data at the given depth for each float 
    if ("PRES_ADJUSTED" %in% names(Datai)) {
      df=subset(  df,  df$PRES_ADJUSTED==plot_depth)
      
    } else {
      df=subset(  df,  df$PRES_ADJUSTED==PRES)
      
    }
    
    df$WMOID=Mdata[[float_ids[f]]]$WMO_NUMBER
    float_data_depth_specific[[f]]=df # assign the float to the list
    
  }  # end loop for "for ( f in 1:nfloats )"
  
  float_data_depth_specific=bind_rows( float_data_depth_specific) # convert the list to the dataframe
   
  
  # Plot the figures for each variables
  
   for ( v in 1:nvars ) {
     g1 = ggplot(data=float_data_depth_specific)
     g1=g1 +
       geom_point(aes(x=TIME,  
                  y=.data[[variables[v]]],
                  color=as.factor(float_data_depth_specific$WMOID)),
                  size=1)
     g1=g1+ geom_line(aes(x=TIME,  
                           y=.data[[variables[v]]],
                           color=as.factor(float_data_depth_specific$WMOID)),
                       size=0.5)
    g1=g1+theme_bw()+theme(legend.title=element_blank())
     
   name_units = get_var_name_units(variables[v])
   long_name = name_units$long_name
   units = name_units$units
   units2 <- expr(variables[v]~!!units)
  
   g1 = g1 +
   labs(title = paste0("Depth: ", plot_depth,"m"),
        x = "Time",
        y =   expr(bold(!!variables[v]~!!units))
        )
   g1= g1+theme (axis.title.y = element_text(size=16,colour = "black",face = "bold",family = "serif") ) 
   g1= g1+theme (axis.title.x = element_text(size=16,colour = "black",face = "bold",family = "serif") ) 
   g1= g1+theme (axis.text.y = element_text(size=16,colour = "black",face = "bold",family = "serif") ) 
   g1= g1+theme (axis.text.x = element_text(size=16,colour = "black",face = "bold",family = "serif") )
   g1=g1+theme(legend.text = element_text(size = 16,face = 'bold',family = "serif"))#
   g1=g1+theme(legend.title=element_blank())#隐藏图例标题
   g1
   x11()
   plot(g1)
   
   }  # end loop for " for ( v in 1:nvars )"
    
    
}