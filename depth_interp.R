depth_interp <- function(Data, qc_flags, 
                         prs_res=2, calc_dens=0, calc_mld_dens=0, dens_thres=0.03, calc_mld_temp=0, temp_thres=0.2){
  
  #This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  #This function interpolates values for BGC-Argo parameters against depth
  # for a set of float profiles.
  #
  # Inputs:
  #   Data     : struct with data for one or more floats; expected fields are
  #              PRES, TIME, LONGITUDE, LATITUDE, CYCLE_NUMBER
  #   qc_flags : array with accepted QC flags (see plot_profiles.m for a full
  #              description)
  #
  # Optional inputs:
  #   'prs_res',prs_res             : pressure resolution (default: 2 dbar)
  #   'calc_dens',calc_dens         : if set to 1, calculate density on
  #                                   interpolated depth levels
  #   'calc_mld_dens',calc_mld_dens : if set to 1, calculate mixed layer
  #                                   depth (MLD) based on a density criterion
  #   'dens_thres',dens_thres       : density threshold for MLD calculation;
  #                                   default value is set in initialize_argo
  #   'calc_mld_temp',calc_mld_temp : if set to 1, calculate mixed layer
  #                                   depth based on a temperature criterion
  #   'dens_thres',dens_thres       : temperature threshold for MLD calculation;
  #                                   default value is set in initialize_argo
  #
  # Note that MLD can be computed both ways at the same time.
  # 
  # Output:
  # Datai : struct with depth-interpolated variables
  
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
  
  
  # DEFINE PRESSURE DATA AS 'X'
  if('PRES_ADJUSTED' %in% names(Data)){
    X = Data[['PRES_ADJUSTED']]
  }
  else{
    X = Data[['PRES']]
  }
  
  X=as.matrix(X)
  
  # DEFINE Datai
  
  Datai=NULL
  
  # CONSTRUCT DEPTH AXIS ON WHICH TO INTERPOLATE DEPENDENT VARIABLES
  xi = seq(0,round(max(X, na.rm=T)),prs_res)
  xi = matrix(xi, nrow=length(xi), ncol=ncol(X))# convert the matrix to avoid the error when profile number is only 1 (ncol does not work in the case of vector)(X))   
  
  # START LOOP FOR EACH DEPENDENT VARIABLE
  vars = names(Data)
  for(k in 1:length(vars)){
    # CHECK FOR EXISTENCE OF FIELD
   if(any(startsWith(vars[k] , Setting$avail_vars)) & !endsWith(vars[k],'_QC')){
      # DEFINE VARIABLE AS 'Y'
      Y =as.matrix( Data[[vars[k]]])
      # PRE-ALLOCATE INTERPOLATED DEPENDENT VARIABLE
      yi = matrix(NaN, nrow=nrow(xi), ncol=ncol(X))
      
      # LOOP INTERPOLATIONS FOR EACH PROFILE
      for(n in 1:ncol(X)){
        if(all(is.na(X[,n])) | all(is.na(Y[,n]))){
          
          yi[,n]=NaN # FILL VECTOR WITH NaN IF EMPTY INPUT
        }
        else{
          # INTERPOLATE FOR EACH PROFILE
          if(paste0(vars[k], '_QC') %in% names(Data)){
            qc_mask =  as.matrix(Data[[paste0(vars[k],'_QC')]])[,n]
          }
          else{
            qc_mask = rep(1,nrow(X))
          }
          idx = !is.na(X[,n]) & !is.nan(X[,n]) & !is.na(Y[,n]) & !is.nan(Y[,n]) & qc_mask %in% qc_flags
          x_filt  = X[,n]
          x_filt = x_filt[idx]
          y_filt = Y[,n]
          y_filt = y_filt[idx]
          try({
            yi[,n] = approx(x = x_filt,y = y_filt, xout = xi[,n], rule=1)$y
          }, silent=TRUE)
        }
      }
      Datai[[vars[k]]] = yi  # ADD INTERPOLATED DEPENDENT VARIABLE TO OUTPUT
    }
  }
  
  Datai[['PRES']] = xi # ADD INTERPOLATED PRESSURE GRID TO OUTPUT
  
  # RESHAPE AND ADD DATE, LATITUDE, AND LONGITUDE TO OUTPUT
  Datai[['TIME']] = matrix(rep(as.matrix( Data[['TIME']])  [1,], each=nrow(xi)), nrow=nrow(xi), ncol=ncol(xi))  # convert to the matrix to avoid the error when profile number is only 1 ( Data[['CYCLE_NUMBER']])  [1,] does not work in the case of vector)(X))   
  Datai[['LATITUDE']] = matrix(rep(as.matrix( Data[['LATITUDE']])  [1,], each=nrow(xi)), nrow=nrow(xi), ncol=ncol(xi))# convert to the matrix to avoid the error when profile number is only 1 ( Data[['CYCLE_NUMBER']])  [1,] does not work in the case of vector)(X))   
  Datai[['LONGITUDE']] = matrix(rep(as.matrix( Data[['LONGITUDE']])  [1,], each=nrow(xi)), nrow=nrow(xi), ncol=ncol(xi))# convert to the matrix to avoid the error when profile number is only 1 ( Data[['CYCLE_NUMBER']])  [1,] does not work in the case of vector)(X))     
  Datai[['CYCLE_NUMBER']] = matrix(rep(as.matrix( Data[['CYCLE_NUMBER']])  [1,], each=nrow(xi)), nrow=nrow(xi), ncol=ncol(xi))# convert to the matrix to avoid the error when profile number is only 1 ( Data[['CYCLE_NUMBER']])  [1,]  does not work in the case of vector)(X))   
  
  if(calc_dens | calc_mld_dens | calc_mld_temp){
    Datai = calc_auxil(Data=Datai, calc_dens=calc_dens, calc_mld_dens=calc_mld_dens, 
                       calc_mld_temp=calc_mld_temp, temp_thres=temp_thres, dens_thres=dens_thres)
  }
  return(Datai)
}
