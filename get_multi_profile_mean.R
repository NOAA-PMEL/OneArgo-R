get_multi_profile_mean <- function (Datai, variable) {
  # get_multi_profile_mean  
  #
  #This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  #
  # Inputs:
  # Optional inputs:
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
  
  floats = names(Datai)
  nfloats = length(floats)
  
  # all Datai structs have the same depth resolution, but the maximum
  # value varies by float; need to find the float with the most depths
  imax = 0
  max_npres = 0
  this_npres = rep(NA,nfloats)
  
  if (nfloats==1) {
    
    all_profs=Datai[[floats]][[variable]]
    
  } else {
    
    for (f in 1:nfloats) {
      this_npres[f]= dim(Datai[[floats[f]]]$PRES)[1]
      if (this_npres[f] > max_npres) {
        imax = f
        max_npres = this_npres[f]
      }
    }
    mean_pres = Datai[[floats[imax]]]$PRES  # this is PRES_ADJUSTED if available
    ndepths = dim(mean_pres)[1]
    total_nprofs = 0
    this_nprofs = rep(NA,nfloats)
    
    for (f in 1:nfloats) {
      this_nprofs[f] = dim(Datai[[floats[f]]]$PRES)[2]
      total_nprofs = total_nprofs + this_nprofs[f]
    }
    
    all_profs = matrix(NA,nrow=ndepths, ncol=total_nprofs) # pre-allocate
    count_profs = 0
    for (f in 1:nfloats) {
      all_profs[1:this_npres[f], (count_profs+1):(count_profs + this_nprofs[f])] = Datai[[floats[f]]][[variable]]
      count_profs =  count_profs + this_nprofs[f]
    }
  }
  
  mean_prof = apply(all_profs,1,function(x){mean(x,na.rm=TRUE)})
  std_prof =  apply(all_profs,1,function(x){sd(x,na.rm=TRUE)})
  mean_pres = mean_pres[,1]
  
  return(list(mean_prof=mean_prof, std_prof= std_prof, mean_pres=mean_pres))
  
}
