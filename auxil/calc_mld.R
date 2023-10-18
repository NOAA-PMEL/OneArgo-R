calc_mld<-function(Data,
                   calc_mld_dens=0,
                   dens_thres=0.03,
                   calc_mld_temp=0,
                   temp_thres=0.2){
  # DESCRIPTION:
  #   This function computes mixed layer depth using a density and/or
  #   temperature threshold for all floats in the Data structure.
  #
  # INPUTS:
  #   Data     : struct with data for one or more floats; expected fields are
  #              PRES, TIME, LONGITUDE, LATITUDE, CYCLE_NUMBER, TEMP, PSAL
  #
  # OPTIONAL INPUTS:
  #   'raw',raw                     : use raw data if 'yes', adjusted data
  #                                   if no (default: 'no')
  #   'calc_mld_dens',calc_mld_dens : if set to 1, calculate mixed layer
  #                                   depth (MLD) based on a density criterion
  #   'dens_thres',dens_thres       : density threshold for MLD calculation;
  #                                   default value is set in initialize_argo
  #   'calc_mld_temp',calc_mld_temp : if set to 1, calculate mixed layer
  #                                   depth based on a temperature criterion
  #   'temp_thres',temp_thres       : temperature threshold for MLD calculation;
  #                                   default value is set in initialize_argo
  #   (Note that MLD can be computed both ways at the same time.)
  
  if(calc_mld_dens==0 & calc_mld_temp==0){
    warning("Neither MLD method was selected!")
    stop()
  }
  
  floats<-names(Data)
  
  for(f in 1:(length(floats))){
    DataFloat<-Data[[floats[f]]]
    field<-names(DataFloat)
    
    if(calc_mld_dens==1){
      if("DENS" %in% field==F & "DENS_ADJUSTED" %in% field==F){
        DataFloat<-calc_auxil(DataFloat,calc_dens = 1)
      }
      DataFloat<-calc_auxil(DataFloat,calc_mld_dens = 1)
    }
    if(calc_mld_temp==1){
      DataFloat<-calc_auxil(DataFloat,calc_mld_temp = 1)
    }
    Data[[floats[f]]]<-DataFloat
  }
  return(Data)
}