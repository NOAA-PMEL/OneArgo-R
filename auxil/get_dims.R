get_dims<- function(filename) {

  # DESCRIPTION:
  #   This function determines the number of profiles, parameters,
  #   and depth levels in an Sprof netcdf file.
  #
  # INPUTS:
  #   filename    : the name of the Sprof file
  #
  # OUTPUT:
  #   n_prof      : number of profiles (integer)
  #   n_param     : number of parameters (integer)
  #   n_levels    : number of depth levels (integers)
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
  
  
  
    
  nc = nc_open(filename) # open ncdf  
  
  
  # Find 'number of profiles', 'number of parameters',
  # and 'number of depth levels'
  n_prof = nc$dim$N_PROF$len
  n_param = nc$dim$N_PARAM$len
  n_levels = nc$dim$N_LEVELS$len
  
  nc_close(nc) # close ncdf  
  
  return(list(n_prof=n_prof,n_param=n_param,n_levels=n_levels))

}
