show_trajectories <- function(float_ids=Setting$demo_float, 
                              color='multiple',
                              float_profs=NULL,
                              position=NULL,
                              title="Float trajectories",
                              return_ggplot=FALSE
) {
  
  # DESCRIPTION:
  #   This is function is an intermediary function that downloads profiles for at least
  #   one given float and calls plot_trajectories to create the plot.
  #
  # INPUT:
  #   float_ids : WMO ID(s) of one or more floats 
  #               (if not set: Settings.demo_float is used as a demo)
  #
  # OPTIONAL INPUTS:
  #   color='multiple' or 'red/blue/green'  : color (string) can be either 'multiple' (different
  #                   colors for different floats), or any standard R
  #                   color descriptor ('red', 'blue', 'green', 'black' etc.)
  #                   (all trajectories will be plotted in the same color)
  #   float_profs : float profile is an array with the per-float indices 
  #                 as returned by function "select_profiles";   
  #                 use this optional argument if you
  #                 don't want to plot the full trajectories of the
  #                 given floats, but only those locations that match
  #                 spatial and/or temporal constraints
  #
  #   position='first' or 'last:  show only the selected position (either 'first' or
  #                             'last')
  #   title          : title for the plot (default: "Float trajectories")
  #   return_ggplot='TRUE' or 'FALSE' : FALSE' (by default) returns the plot to X11; 'TRUE'
  #                  returns the plot to the ggplot panel if setting to "TRUE"; By default,
  #                               
  #
  # OUPUT:
  #   good_float_ids : array of the float IDs whose Sprof files were
  #                    successfully downloaded or existed already
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
  
  
  # make sure Settings is initialized
  if (exists("Setting")==F) {
    initialize_argo()
  }
  
  # download Sprof files if necessary
  good_float_ids = download_multi_floats(float_ids)
  
  if ( length(good_float_ids) == 0 ) {
    warning('no valid floats found')
    return(1)
  } else {
    # meta data return values and observations are not needed here
    loaded = load_float_data(float_ids=good_float_ids, float_profs=float_profs)
    Data = loaded$Data
    
    if(!is.null(position)){
      nfloats = length(Data)
      if(position=="first"){
        for (f in 1:nfloats){
          #only lon/lat fields are used by plot_trajectories
          Data[[f]]$LONGITUDE<-Data[[f]]$LONGITUDE[,1]
          Data[[f]]$LATITUDE<-Data[[f]]$LATITUDE[,1]
        }
          
      } else if(position=="last"){
        for (f in 1:nfloats){
          Data[[f]]$LONGITUDE<-Data[[f]]$LONGITUDE[,ncol(Data[[f]]$LONGITUDE)]
          Data[[f]]$LATITUDE<-Data[[f]]$LATITUDE[,ncol(Data[[f]]$LATITUDE)]
        }
      }
    }
    
    g1 = plot_trajectories(Data=Data, color=color, title=title)
    
    if ( return_ggplot ) {
      return(g1)
    } else {
      x11()
      plot(g1)
    }
    
  }
}