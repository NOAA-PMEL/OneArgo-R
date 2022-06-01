do_pause <-function() {
  
  # DESCRIPTION:
  #   It asks the user to hit ENTER if Settings.use_pause is 1.
  #   Otherwise, flow control returns the caller.
  #
  #
  # AUTHORS:
  #   Marin Cornec (NOAA-PMEL), Yibin Huang (NOAA-PMEL), 
  #   Quentin Jutard (OSU ECCE TERRA), Raphaelle Sauzede (IMEV) and 
  #   Catherine Schmechtig (OSU ECCE TERRA).
  #
  # CITATION:
  #   M. Cornec, Y. Huang, Q. Jutard, R. Sauzede, and C. Schmechtig, 2022. 
  #   OneArgo-R: A R toolbox for accessing and visualizing Argo data.
  #   Zenodo. XXXXX
  #
  # LICENSE: oneargo_r_license.m
  #
  # DATE: JUNE 1, 2022  (Version 1.0.1)
  
    if(Setting$use_pause==1) {
    cat ("Please hit ENTER to continue")
    line <- readline()
    }
}








