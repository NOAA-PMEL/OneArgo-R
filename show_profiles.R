

show_profiles=function( profile_ids,
                        variables, 
                        type = "profiles", 
                        obs = "off",  
                        qc_flags = 0:9,
                        raw = "no", 
                        method = "all", 
                        title_add = NULL,  
                        per_float = 1 ){
  # show_profiles 
  #
  # This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  # USAGE:
  #   good_float_ids = show_profiles(profile_ids, variables, varargin)
  #
  # DESCRIPTION:
  #   This an intermediary function that downloads profile(s) for the given
  #   float(s) and calls plot_profile to create the plot(s).
  #
  # INPUTS:
  #   profile_ids  : internally used indices of individual profiles
  #   variables    : cell array of variable(s) (i.e., sensor(s)) to show 
  #                  (if not set: {'DOXY'} (=O2) is used)
  #
  # OPTIONAL INPUTS:
  #  'type', profiles/floats   : by default (type='profiles'), the given IDs refer to
  #                  profile IDs (obtained with select_profiles); use
  #                  'type','floats' to show the profiles of a given float
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
  #                  See Table 7 in Bittig et al.:
  #                  https://www.frontiersin.org/files/Articles/460352/fmars-06-00502-HTML-r1/image_m/fmars-06-00502-t007.jpg
  # method, all/mean:  show all profiles per variable in one plot 
  #  'title_add',text : add the given text to all titles
  #
  # OUTPUT:
  #   good_float_ids : array of the float IDs whose Sprof files were
  #                    successfully downloaded or existed already
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
  
  if ( min(qc_flags) < 0 | max(qc_flags)>9){
    warning('only QC flags 0..9 are allowed!')
  }
  
  # convert requested variable to cell array if necessary (string was used)
  variables = as.character(variables)
  
  if (type == 'profiles') {
    # profile IDs need to be converted to float IDs
    all_float_ids = Sprof$wmo[ profile_ids ]
  } else if (type == 'floats') {
    all_float_ids = profile_ids;
  } else {
    warning('type must be either "profiles" or "floats"')
  }
  
  uniq_float_ids = unique(all_float_ids)
  
  # download Sprof files if necessary
  good_float_ids = download_multi_floats(uniq_float_ids)
  
  if ( is.null( good_float_ids) ){
    warning('no valid floats found')
  } 
  
  # Load the float data 
  
  if (type== 'floats'){
    
    Float_Data=load_float_data(good_float_ids, 
                               variables, 
                               float_profs=NULL);

  }  
  
  if (type== 'profiles'){
    
    # create the   float_profs used for the input in load_float_data
    float_profs=NULL
    for (i in 1:length(good_float_ids)){
      idx = (all_float_ids == good_float_ids[i]) # index based on float id
      # obtain profiles of that float to plot
      float_profs[[i]] =Sprof$fprofid[profile_ids[idx]]
    }
    
    Float_Data=load_float_data(float_ids = good_float_ids, 
                               variables = variables, 
                               float_profs = float_profs);
    
  }

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

