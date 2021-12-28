show_sections <- function(float_ids=Setting$demo_float, variables="DOXY",
                          plot_isopyc=1, plot_mld=0, max_depth=NULL, raw="no", 
                          obs="off", qc_flags=0:9) {
  # show_sections  
  #
  # This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  # It is an intermediary function that downloads profile(s) for the given
  # float(s) and calls plot_sections to create the plot(s).
  #
  # Inputs:
  #   float_ids  : WMO ID(s) of one or more floats 
  #                (if not set: 5904021 is used as a demo)
  #   variables  : cell array of variable(s) (i.e., sensor(s)) to show 
  #                (if not set: {'DOXY'} (=O2) is used)
  #
  # Optional inputs:
  #   'isopyc',isopyc    : plot isopycnal lines if set (default: 1=on)
  #   'mld',mld          : plot mixed layer depth, using either a 
  #                        temperature criterion (mld=1) or a density
  #                        criterion (mld=2); default: 0=off
  #   'time_label',label : use either years ('y', by default) or months ('m')
  #   'max_depth',depth  : maximum depth to be plotted (default: all)
  #   'qc',flags         : show only values with the given QC flags (as an array)
  #                        0: no QC was performed; 
  #                        1: good data; 
  #                        2: probably good data;
  #                        3: probably bad data that are potentially correctable;
  #                        4: bad data; 
  #                        5: value changed; 
  #                        6,7: not used;
  #                        8: estimated value; 
  #                        9: missing value
  #                        default setting: [1,2]
  #                        See Table 7 in Bittig et al.:
  #                        https://www.frontiersin.org/files/Articles/460352/fmars-06-00502-HTML-r1/image_m/fmars-06-00502-t007.jpg
  #
  # Output:
  #   good_float_ids : array of the float IDs whose Sprof files were
  #                    successfully downloaded or existed already
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
  
  # download Sprof files if necessary
  good_float_ids = download_multi_floats(float_ids)
  
  if ( length(good_float_ids) == 0 ) {
    warning('no valid floats found')
  } else {
    nvars = length(variables)
    # add the necessary variables now, but don't plot their profiles
    if ( plot_isopyc | plot_mld ) {
      if (!any(variables == 'TEMP')) {
        variables = c(variables, 'TEMP')            
      }
      if (!any(variables == 'PSAL')) {
        variables = c(variables, 'PSAL')            
      }
    }
    
    loaded = load_float_data(good_float_ids, variables)
    Data = loaded$Data
    Mdata = loaded$Mdata
  
    plot_sections(Data=Data, Mdata=Mdata, variables=variables, nvars=nvars, 
                  plot_isopyc=plot_isopyc, plot_mld=plot_mld, 
                  max_depth=max_depth, raw=raw, obs=obs, qc_flags=qc_flags)
  }
  
  return(good_float_ids)
}