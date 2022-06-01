get_var_name_units <- function(short_name){
  
  # DESCRIPTION:
  # This function returns the long name and the units for the variable
  # with the given short name.
  #
  # INPUT:
  #   short_name : case-sensitive name of a variable as it appears in 
  #                the Sprof index file, e.g., TEMP or DOXY
  #
  # OUTPUTS:
  #   long_name  : long name of the variable
  #   units      : units of the variable
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

  long_name = NULL
  units = NULL
  
  if(startsWith(short_name, 'TEMP')){
    long_name = 'Temperature'
    units = '(°C)'
  }
  if(startsWith(short_name, 'PSAL')){
    long_name = 'Salinity'
    units = '(PSU)'
  }
  if(startsWith(short_name, 'DENS')){
    long_name = 'Density'
    units = parse(text='"("*kg~m^{-3}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'PRES')){
    long_name = 'Pressure'
    units = '(dbar)'
  }
  if(startsWith(short_name, 'DOXY')){
    long_name = 'Dissolved Oxygen'
    units = parse(text='"("*mu*mol~kg^{-1}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'NITRATE')){
    long_name = 'Nitrate'
    units = parse(text='"("*mu*mol~kg^{-1}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'AOU')){
    long_name = 'Apparent Oxygen Utilization'
    units = parse(text='"("*mu*mol~kg^{-1}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'PH_IN_SITU')){
    long_name = 'pH'
    units = ''
  }
  if(startsWith(short_name, 'CHLA')){
    long_name = 'Chlorophyll-a'
    units = parse(text='"("*mg~m^{-3}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'CDOM')){
    long_name = 'Colored Dissolved Organic Matter'
    units = '(ppb)'
  }
  if(startsWith(short_name, 'BBP')){
    long_name = 'Backscatter'
    units = parse(text='"("*m^{-1}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'DOWN_IRRADIANCE')){
    long_name = 'Downwelling irradiance'
    units = parse(text='"("*W~m^{-2}~nm^{-1}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'UP_RADIANCE')){
    long_name = 'Upwelling irradiance'
    units = parse(text='"("*W~m^{-2}~nm^{-1}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'DOWNWELLING_PAR')){
    long_name = 'Downwelling PAR'
    units = parse(text='"("*mu*mol~Quanta~m^{-2}~sec^{-1}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'BISULFIDE')){
    long_name = 'Bisulfide'
    units = parse(text='"("*mu*mol~kg^{-1}*")"', keep.source=FALSE)[[1]]
  }
  if(startsWith(short_name, 'TURBIDITY')){
    long_name = 'Turbidity'
    units = '(ntu)'
  }
  
  return(list(long_name=long_name,units=units))
}