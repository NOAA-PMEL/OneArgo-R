combine_variables=function(base_vars, new_vars){
  

  # DESCRIPTION:
  #   This function combines the given variables along with all
  #   associated variables (e.g., _ADJUSTED, _QC, etc.) and returns them.
  #
  # USAGE:
  #   all_vars = combine_variables(base_vars, new_vars)
  #
  # INPUTS:
  #   base_vars : cell array with names of the basic variables
  #   new_vars  : cell array with names of additional variables (tracer
  #               fields etc.) that have the standard associated variables
  #
  # OUTPUT:
  #  all_vars   : cell array with names of all variables
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
  
  
  
  # always include all associated variables
  cnt_vars = length(base_vars);
  
  nvars = cnt_vars + 6*(length(new_vars)) - sum(is.element('PRES', new_vars));
  all_vars = vector(mode = "logical",length =  nvars ); # pre-allocated
  all_vars[1:cnt_vars] = base_vars;

  for ( i in 1:length(new_vars) ) {
    
      cnt_vars = cnt_vars+1;
      all_vars[cnt_vars] = new_vars[i]
      
      cnt_vars = cnt_vars+1;
      all_vars[cnt_vars] = paste0(new_vars[i], '_QC')
      
      if (new_vars[i] != 'PRES') {
        cnt_vars = cnt_vars+1;
        all_vars[cnt_vars] = paste0(new_vars[i], '_dPRES')
      }
      
      cnt_vars = cnt_vars+1;
      all_vars[   cnt_vars] = paste0(new_vars[i], '_ADJUSTED')
      cnt_vars = cnt_vars+1;
      
      all_vars[   cnt_vars] = paste0(new_vars[i], '_ADJUSTED_QC')
      
      cnt_vars = cnt_vars+1;
      all_vars[   cnt_vars] = paste0(new_vars[i], '_ADJUSTED_ERROR')
      
      cnt_vars = cnt_vars+1;
      all_vars[   cnt_vars] = paste0('PROFILE_',new_vars[i], '_QC')
  }

  
  return(   all_vars)
}
  
  
