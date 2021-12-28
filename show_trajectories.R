show_trajectories <- function(float_ids=Setting$demo_float, color='multiple', 
                              prof_ids=NULL, return_ggplot=FALSE) {
  # show_trajectories  
  
  # This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  # It is an intermediary function that downloads profiles for at least
  # one given float and calls plot_trajectories to create the plot.
  #
  # Inputs:
  #   float_ids : WMO ID(s) of one or more floats 
  #               (if not set: Settings.demo_float is used as a demo)
  #
  # Optional inputs:
  #   'color',color : color (string) can be either 'multiple' (different
  #                   colors for different floats), or any standard R
  #                   color descriptor ('red', 'blue', 'green', 'black' etc.)
  #                   (all trajectories will be plotted in the same color)
  #  'prof_ids',ids : ids is an array with the global indices of the
  #                   selected profiles, as returned by function
  #                   select_profiles - use this optional argument if you
  #                   don't want to plot the full trajectories of the
  #                   given floats, but only those that match spatial
  #                   and/or temporal constraints
  #
  # Output:
  #   good_float_ids : array of the float IDs whose Sprof files were
  #                    successfully downloaded or existed already
  #
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

  # Last update: June 24, 2021
  
  # download Sprof files if necessary
  good_float_ids = download_multi_floats(float_ids)

  if ( is.null(prof_ids) ) {
    float_profs = NULL
  } else {
    # convert global profile IDs to individual (per float) profile IDs
    float_profs = NULL
    all_float_ids = Sprof$wmo[prof_ids] # get all float ids
    for (i in 1:length(good_float_ids)) {
      idx = (all_float_ids == good_float_ids[i]) # index based on float id
      # obtain profiles of that float to plot
      float_profs[[i]] = Sprof$fprofid[prof_ids[idx]]
    }
  }
  
  if ( length(good_float_ids) == 0 ) {
    warning('no valid floats found')
    return(1)
  } else {
    # meta data return values and observations are not needed here
    loaded = load_float_data(float_ids=good_float_ids, float_profs=float_profs)
    Data = loaded$Data
    Mdata = loaded$Mdata
    
    g1 = plot_trajectories(Data=Data, color=color)
    
    if ( return_ggplot ) {
      return(g1)
    } else {
      x11()
      plot(g1)
    }
    
  }
}