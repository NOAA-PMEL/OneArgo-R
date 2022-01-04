get_dims<- function(filename) {

# This function is part of the
# GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
#
# DESCRIPTION:
#   This function determines the number of profiles, parameters,
#   and depth levels in an Sprof netcdf file.
#
# INPUTS:
#   filename    : the name of the Sprof file
#
#
# OUTPUT:
#   n_prof      : number of profiles (integer)
#   n_param     : number of parameters (integer)
#   n_levels    : number of depth levels (integers)
#
# REQUIRES : ncdf4 package to deal with netcdf file
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

# Update 4 January 2022

nc = nc_open(filename) # open ncdf  


# Find 'number of profiles', 'number of parameters',
# and 'number of depth levels'
n_prof = nc$dim$N_PROF$len
n_param = nc$dim$N_PARAM$len
n_levels = nc$dim$N_LEVELS$len

nc_close(nc) # close ncdf  

return(list(n_prof=n_prof,n_param=n_param,n_levels=n_levels))

}
