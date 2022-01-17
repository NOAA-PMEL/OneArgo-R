get_multi_profile_mean <- function (Datai, variable) {
  
  # DESCRIPTION:
  #   This function computes mean and standard deviation of a
  #   depth-interpolated variable. Missing values are omitted in the 
  #   calculations.
  #
  # INPUTS:
  #   Datai     : struct with depth-interpolated fields from multiple floats
  #               that must include PRES and the given variable
  #   variable  : string with the name of the variable (e.g., DOXY)
  #
  # OUTPUTS:
  #   mean_prof : mean value of the variable across floats (column vector)
  #   std_prof  : standard variation of the variable across floats (column vector)
  #   mean_pres : mean pressure across floats (column vector)
  #
  # UPDATE RECORD: 
  #   Version 1:   June 2021 
  #   Version 1.1: January 2022 
  #
  # CITATION:
  #   M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), R. Sauzede (IMEV) and 
  #   C. Schmechtig (OSU ECCE TERRA), 2021.
  #   BGC-Argo-R: A R toolbox for accessing and visualizing Biogeochemical Argo data. 
  #   Zenodo. http://doi.org/10.5281/zenodo.5028139
  
  
  
  floats = names(Datai)
  nfloats = length(floats)
  
  mean_pres<-NULL
  
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
