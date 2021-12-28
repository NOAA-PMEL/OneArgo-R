if (!require("gsw")) { install.packages("gsw"); library(gsw) }

calc_auxil <- function(Data, calc_dens=0, calc_mld_temp=0, temp_thresh=0.2, calc_mld_dens=0, dens_thresh=0.03){

  #This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  #This function calculates various auxiliary variables from Argo
  #float data: Density, mixed layer depth (MLD), based either on
  #a temperature or a density threshold.
  #Any combination of these variables can be requested.
  #
  #Input:
  # Data      : struct (note that this can be on the original or the
  #                                        interpolated pressure/depth axis)
  #Optional inputs (all calc* values are 0=off by default, set them to
  #                   % 1=on to activate the calculation):
  # 'calc_dens', calc_dens:         if set, calculate in situ density
  # 'calc_mld_temp', calc_mld_temp: if set, compute MLD based on T threshold
  # 'temp_thresh', temp_threshold : temperature threshold for MLD calculation
  #                                 (default: 0.2 dg C); ignored if
  #                                 calc_mld_temp is not set to 1
  # 'calc_mld_dens', calc_mld_dens: if set, compute MLD based on rho
  #                                 threshold
  # 'dens_thresh', dens_threshold : density threshold for MLD calculation
  #                                 (default: 0.03 kg/m3); ignored if
  #                                 calc_mld_dens is not set to 1
  #
  # Output:
  # Data        : struct with added auxiliary variables - for all variables,
  #               either raw or adjusted fields are added to the structure.
  #               The raw fields are (adjusted fields have the same names
  #                                                  with _ADJUSTED added at the end):
  #               calc_dens: DENS (in situ density),
  #                          PSAL_ABS (absolute salinity),
  #                          TEMP_CNS (conservative temperature)
  #               calc_mld_temp: MLD_TEMP (mixed layer depth based on a
  #                                                                     temperature threshold)
  #               calc_mld_dens: MLD_DENS (mixed layer depth based on a
  #                                                                     density threshold)
 
  # CITATION:
  # BGC-Argo-R: A R toolbox for accessing and visualizing
  # Biogeochemical Argo data,
  #
  #  AUTHORS: 
  # M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), 
  # R. Sauzede (IMEV) and C. Schmechtig (OSU ECCE TERRA),
  #
  # Adapted from the Matlab toolbox BGC-Argo-Mat:  https://doi.org/10.5281/zenodo.4971318
  # (H. Frenzel, J. Sharp, A. Fassbender (NOAA-PMEL),
  # J. Plant, T. Maurer, Y. Takeshita (MBARI), D. Nicholson (WHOI),
  # and A. Gray (UW))
  
  # Update 24 June 2021
  
  # Calculate in situ density:
  if(calc_dens){
    if('PRES_ADJUSTED' %in% names(Data)){
        Data[['PSAL_ABS_ADJUSTED']] = gsw_SA_from_SP(SP = Data[['PSAL_ADJUSTED']],
                                                p = Data[['PRES_ADJUSTED']],
                                                longitude = Data[['LONGITUDE']],
                                                latitude = Data[['LATITUDE']]) # Calculate absolute salinity   
        Data[['TEMP_CNS_ADJUSTED']] = gsw_CT_from_t(SA = Data[['PSAL_ABS_ADJUSTED']],
                                               t = Data[['TEMP_ADJUSTED']],
                                               p = Data[['PRES_ADJUSTED']]) # Calculate conservative temperature
        Data[['DENS_ADJUSTED']] = gsw_rho(SA = Data[['PSAL_ABS_ADJUSTED']],
                                     CT = Data[['TEMP_CNS_ADJUSTED']],
                                     p = Data[['PRES_ADJUSTED']]) # Calculate in situ density
      }
      else{
        Data[['PSAL_ABS']] = gsw_SA_from_SP(SP = Data[['PSAL']],
                                       p = Data[['PRES']],
                                       longitude = Data[['LONGITUDE']],
                                       latitude = Data[['LATITUDE']]) # Calculate absolute salinity   
        Data[['TEMP_CNS']] = gsw_CT_from_t(SA = Data[['PSAL_ABS']],
                                      t = Data[['TEMP']],
                                      p = Data[['PRES']]) # Calculate conservative temperature
        Data[['DENS']] = gsw_rho(SA = Data[['PSAL_ABS']],
                            CT = Data[['TEMP_CNS']],
                            p = Data[['PRES']]) # Calculate in situ density
      }
  }
  
  if(calc_mld_temp | calc_mld_dens){
    if('TEMP_ADJUSTED' %in% names(Data)){
        temp = Data[['TEMP_ADJUSTED']]
    }
    else{
        temp = Data[['TEMP']]
    }
    if('PRES_ADJUSTED' %in% names(Data)){
      pres = Data[['PRES_ADJUSTED']]
    }
    else{
      pres = Data[['PRES']]
    }
  }
  
  if(calc_mld_temp){
    # Pre-allocate mixed layer
    Data[['MLD_TEMP']] = rep(NaN, ncol(temp))
    # Calculate density based on temperature threshold
    for(n in 1:ncol(temp)){
      pressure_prof = pres[,n] # extract pressure profile
      temperature_prof = temp[,n] # extract temperature profile
      # determine pressure closest to 10
      ref_idx = which.min(abs(pressure_prof-10))
      # define reference temperature as closest to P = 10
      temperature_ref = temperature_prof[ref_idx]
      under_ref = (ref_idx+1):length(pressure_prof) # index temp. below reference
      # truncate pressure profile to below reference
      pressure_prof = pressure_prof[under_ref]
      # truncate temperature profile to below reference
      temperature_prof = temperature_prof[under_ref]
      MLDt_idx = which(temperature_prof < temperature_ref-temp_thresh)
      if(length(MLDt_idx)>0){
        Data[['MLD_TEMP']][n] = pressure_prof[MLDt_idx[1]]
      }
    }
  }
 
  if(calc_mld_dens){
    if('PSAL_ADJUSTED' %in% names(Data)){
      salt = Data[['PSAL_ADJUSTED']]
    }
    else{
      salt = Data[['PSAL']]
    }
    
    # Calculate potential density with respect to surface pressure (=0)
    pdensity = gsw_rho(SA = salt,CT = temp, p = matrix(0,ncol = ncol(temp), nrow = nrow(temp)))
    
    # Pre-allocate mixed layer
    Data[['MLD_DENS']] = rep(NaN,ncol(temp))
    
    # Identify first instance of potential density that is
    # "dens_thres" greater than surface potential density
    for(n in 1:ncol(temp)){
      pressure_prof = pres[,n] # extract pressure profile
      # determine pressure closest to 10 dbar; use that as reference
      ref_idx = which.min(abs(pressure_prof-10))
      density_prof = pdensity[,n] # extract n-th density profile
      # define reference density as closest to P = 10 dbar
      density_ref = density_prof[ref_idx]
      # is infinite density ---> à reprendre pas compris
      if(is.finite(density_ref)){ # make sure that a valid density was found
        under_ref = (ref_idx+1):length(pressure_prof) # index to dens. below reference
        pressure_prof = pressure_prof[under_ref]
        # truncate pressure profile to below reference
        density_prof = density_prof[under_ref]
        # truncate density profile to below reference
        MLDd_idx = which(density_prof > density_ref+dens_thresh)
        if(length(MLDd_idx)>0){
          Data[['MLD_DENS']][n] = pressure_prof[MLDd_idx[1]]
        }
      }
    }
  }
  return(Data)
}
