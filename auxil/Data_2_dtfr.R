# Data_2_dtfr  This function is part of the
# R toolbox for accessing Argo float data.
#
# USAGE:
#   float_data_list_dtfr = Data_2_dtfr(Data, variable)
#
# DESCRIPTION:
#   This function generates a dataframe with the selected variables.
#
# INPUTS:
#   Data     : struct that must contain the given variables
#              (_ADJUSTED fields are used if available), as returned by
#              function load_float_data
#   variables: name(s) of the measured field(s)
#              This can be given as string (e.g., 'BBP700') for a single
#              variable or vector for multiple variables, e.g.
#              c('DOXY','NITRATE')
#
#
#
# OUTPUT:
#   float_data_list_dtfr: dtfr that contains the selected variables +
#                         "JULD","TIME", "LONGITUDE", "LATITUDE",
#                         "CYCLE_NUMBER", "WMOID"
#
# AUTHORS:
#   Marin Cornec (NOAA-PMEL), Yibin Huang (NOAA-PMEL), 
#   Quentin Jutard (OSU ECCE TERRA), Raphaelle Sauzede (IMEV) and 
#   Catherine Schmechtig (OSU ECCE TERRA).
#
# CITATION:
#   M. Cornec, Y. Huang, Q. Jutard, R. Sauzede, and C. Schmechtig, 2022. 
#   OneArgo-R: An R toolbox for accessing and visualizing Argo data.
#   Zenodo. https://doi.org/10.5281/zenodo.6604650
#
# LICENSE: oneargo_r_license.m
#
# DATE: JUNE 28, 2023  
#
Data_2_dtfr<-function(Data, 
                      variables="JULD"){
  
  variables<-c(variables,c("JULD",
                         "TIME",
                         "LONGITUDE",
                         "LATITUDE",
                         "CYCLE_NUMBER"))
  variables<-unique(variables)
  
  float_data_list_dtfr= vector("list",
                               length(Data)
  )# Create a list to store the multiple data frame for each float data
  
  for (i in 1:length(Data)){ # loop for each float data 
    
    length_float_data=length(Data[[i]]$CYCLE_NUMBER)
    number_variable_float_data=length(variables)
    
    
    # create a matrix to deposite the float data
    float_data_single_dtfr= matrix (nrow=   length_float_data,
                                    ncol=   number_variable_float_data
                                    
    ) 
    float_data_single_dtfr=as.data.frame(  float_data_single_dtfr)
    colnames(float_data_single_dtfr) = variables # names the data frame
    
    
    # loop to tranform the each variable 
    for (ii in 1: number_variable_float_data ){  
      if(variables[ii] %in% names(Data[[i]])){
        if(is.null(dim(Data[[i]][[variables[ii]]])) &
           length(Data[[i]]$CYCLE_NUMBER)!=length(Data[[i]][[variables[ii]]])){
          n_depth<-length(Data[[i]]$CYCLE_NUMBER)
          var_vec<-NULL
          if(is.null(dim(Data[[i]]$CYCLE_NUMBER)[1])){
            for(ij in 1:length(Data[[i]][[variables[ii]]])){
              var_vec<-c(var_vec,rep(Data[[i]][[variables[ii]]][ij],n_depth))
            }
            float_data_single_dtfr[,ii]<-var_vec
          }else{
            n_depth<-dim(Data[[i]]$CYCLE_NUMBER)[1]
            
            for(ij in 1:length(Data[[i]][[variables[ii]]])){
              var_vec<-c(var_vec,rep(Data[[i]][[variables[ii]]][ij],n_depth))
            }
            float_data_single_dtfr[,ii]<-var_vec
          }
          
        }else{
          float_data_single_dtfr[,ii]=as.vector(Data[[i]][[variables[ii]]])
        }
      }else{
        warning(paste("variable", variables[ii],"not found for float",names(Data[i])))
      }
    } # end loop in number_variable_float_data
    
    
    float_data_single_dtfr$WMOID= names(Data[i])  # add the WMOID in data frame 
    
    
    #  assign data frame into the list array 
    float_data_list_dtfr[[i]]=  float_data_single_dtfr 
    names(  float_data_list_dtfr)[i] <-  names(Data[i]) #  name each element in list 
    
  }# end loop in float_data
  
  # merge the multiple data frame into the single one
  tryCatch( {
    
    float_data_list_dtfr=bind_rows(float_data_list_dtfr)
    float_data_list_dtfr=unique(float_data_list_dtfr)
  },  error = function(e){
    
    print("data exceeds the memorylimit of dataframe so data isinput as a list containing multiple data frame for each float ")
  }
  )
  
  
  return( float_data_list_dtfr)
}

