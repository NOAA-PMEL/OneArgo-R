show_profiles=function( float_ids,
                        variables="DOXY", 
                        float_profs=NULL,
                        obs = "off",  
                        qc_flags = 0:9,
                        raw = "no", 
                        method = "all", 
                        title_add = NULL,  
                        per_float = T){
  
  # DESCRIPTION:
  #   This an intermediary function that downloads profile(s) for the given
  #   float(s) and calls plot_profile to create the plot(s).
  #   Require to install the "xquartz" for macOS system for figure plot ("https://www.xquartz.org/")
  # 
  # INPUTS:
  #      float_ids   : WMO ID(s) of the float(s)
  #      variables   : cell array of variable(s) (i.e., sensor(s)) to show 
  #                  (if not set: {'DOXY'} (=O2) is used)
  #
  # OPTIONAL INPUTS:
  #   float_profs : float profile is an array with the per-float indices 
  #                 as returned by function "select_profiles";  
  #   obs = 'on' / 'off : by default (obs='off') only lines are shown 
  #                              for each profile; 'obs','on' shows points on 
  #                              the profile at which each measurement was made
  #   per_float = 'TRUE' / 'FALSE': show profiles separately for each float (T)
  #                              or all in one plot (F) default: T
  #   raw= 'yes'/ 'no':         plot raw, i.e., unadjusted data if set to n;
  #                              default: y (i.e., plot adjusted data if 
  #                              available)
  #   qc_flags=   : show only values with the given QC flags 
  #                   (as an array)
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
  #                  See Table 7 in Bittig et al.:
  #                  https://www.frontiersin.org/files/Articles/460352/fmars-06-00502-HTML-r1/image_m/fmars-06-00502-t007.jpg
  #   method = 'all' / 'mean'    :  by default (method = 'all') show all profiles 
  #                               per variable in one plot; use method='mean' 
  #                               to plot mean and standard deviation 
  #   title_add = text:   add the given text to all titles
  #
  # OUTPUT:
  #   good_float_ids : array of the float IDs whose Sprof files were
  #                    successfully downloaded or existed already
  #   mean_prof      : mean across profiles (cell array of cell arrays
  #                    ({variable}{float}) of column vectors if per_float
  #                    is set to T,
  #                    cell array ({variable}) of column vectors if per_float
  #                    is set to F)
  #   std_prof  :      standard deviation across profiles 
  #                    (same type as mean_prof)
  #   mean_pres :      mean pressure across profiles (cell array of column
  #                    vectors if per_float is set to 1,
  #                    column vector if per_float is 0)
  #
  # Update record: 
  #   Version 1: 24 June 2021 
  #   Version 1.1: January 2022 
  
  # CITATION:
  #   M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), R. Sauzede (IMEV) and 
  #   C. Schmechtig (OSU ECCE TERRA), 2021.
  #   BGC-Argo-R: A R toolbox for accessing and visualizing Biogeochemical Argo data. 
  #   Zenodo. http://doi.org/10.5281/zenodo.5028139
  
  
  
  # make sure Setting is initialized
  if (exists("Setting")==F) {
    initialize_argo()
  }
  
  good_float_ids = NULL
  
  
  if ( is.null(float_ids) ){
    warning('no floats specified')
  } 
  
  
  if ( min(qc_flags) < 0 | max(qc_flags)>9){
    warning('only QC flags 0..9 are allowed!')
  }
  
  # convert requested variable to cell array if necessary (string was used)
  variables = as.character(variables)
  
  # download Sprof files if necessary
  good_float_ids = download_multi_floats(float_ids)
  
  if ( is.null( good_float_ids) ){
    warning('no valid floats found')
  } else {
    Float_Data=load_float_data(float_ids = good_float_ids, 
                               variables = variables, 
                               float_profs = float_profs)
    plot_profiles(Data =  Float_Data$Data, 
                            Mdata = Float_Data$Mdata, 
                            variables = variables,
                            method = method ,
                            per_float = per_float,
                            obs = obs, 
                            raw = raw, 
                            title_add = title_add,
                            qc_flags = qc_flags
    )
  }
}  


