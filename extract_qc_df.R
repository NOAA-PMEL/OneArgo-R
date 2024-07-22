# extract_data_qc  This function is part of the
# R toolbox for accessing Argo float data.
#
# USAGE:
#   Data_good = extract_qc_df(Data, variables, qc_flags)
#
# DESCRIPTION:
#   This function generates a new data structure (either list or dataframe) 
#   composed of chosen variables based on provided QC flag values, and data mode.
#
# INPUTS:
#   Data     : list that must contain the given variables
#              (_ADJUSTED fields are used if available), as returned by
#              function load_float_data
#   variables: name(s) of the measured field(s)
#              This can be given as string (e.g., 'BBP700') for a single
#              variable or vector for multiple variables, e.g.
#              c('DOXY','NITRATE')
#
# OPTIONAL INPUTS:
#   qc_flags: numerical array of QC flag values (default: c(1,2))
#
#   raw     : "yes", "yes_strict", "no", "no_strict". "yes" (default) raw data will be used
#              if no adjusted data are available, "yes"_strict" only raw data will be used,
#             "no" : adjusted data for the 
#              given parameter. "no_strict": skip the float if one of the variable
#              is not adjusted
#
#   type     : "cleaned", "detailed. "cleaned" (default) output will contain only
#              the data with the requested QC, "detailed" : output will contain 
#              all the original data and additional columns with the corresponding QC
#   mode     : TRUE (default) will add a column displaying the data mode of the corresponding
#              variable (R = "Real Time", A= "Adjusted, D= "Delayed")
#
#
# OUTPUT:
#   Data_good: - if "format" option is not be specified:
#              list that contains all the variables from the input Data
#              values that match the given QC flags are conserved;
#              all other values are set to NA (the size of the arrays is
#              unchanged)
#              - if "format" option is set to "dataframe": list will be 
#              converted to a data frame
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
# DATE: JULY 22, 2024  
#

extract_qc_df<-function(Data, 
                    variables="PRES", 
                    qc_flags=NULL,
                    format=NULL,
                    raw="yes",
                    type="cleaned",
                    mode=T
){
  
  # assign default qc_flags if none provided as input
  if(is.null(qc_flags)){
    qc_flags = c(1,2)
  }
  
  if ( is.null   (format)  ){ # Set to export the data in the format of list if "format" are not specific
    format="list"
  }
  
  # Add pres variable
  if("PRES" %in% variables==F){
    if("PRES_ADJUSTED" %in% variables==F){
      variables<-c(variables,"PRES")
    }
  }
  nvar = length(variables)
  
  # establish qc list to reference
  qc_by_var<-list()
  for (v in c(1:nvar)){
    qc_by_var[[variables[v]]] = qc_flags
  }
  
  variables = names(qc_by_var)
  nvar = length(variables)
  
  floats = names(Data)
  nfloats = length(floats)
  
  if(type=="cleaned"){
    Data_good<-list()
    for (f in (1:nfloats)){
      # create basic lists to build off of
      
      Data_good[[floats[f]]]$CYCLE_NUMBER<-Data[[floats[f]]]$CYCLE_NUMBER
      Data_good[[floats[f]]]$TIME<-Data[[floats[f]]]$TIME
      Data_good[[floats[f]]]$LATITUDE<-Data[[floats[f]]]$LATITUDE
      Data_good[[floats[f]]]$LONGITUDE<-Data[[floats[f]]]$LONGITUDE
      Data_good[[floats[f]]]$JULD<-Data[[floats[f]]]$JULD
      
      for (v in (1:nvar)){
        if ( variables[v] %in% names(Data[[f]])==F){
          warning(paste("float",  floats[f] ,"does not contain variable", variables[v])) 
        }
        
        else{
          if (raw=="yes_strict"){
            Data_good[[floats[f]]][[variables[v]]]<-
              Data[[floats[f]]][[variables[v]]]
            if(mode==T){
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                Data[[floats[f]]][[paste0(variables[v], '_DATA_MODE')]]
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                replace(Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]], TRUE, 'R')
            }
            #Case of one profiles
            if(is.null(dim(Data_good[[floats[f]]][[variables[v]]])) |
               length(dim(Data_good[[floats[f]]][[variables[v]]]))==1
               ){
              for (uno in c(1:length(Data[[floats[f]]][[variables[v]]]))){
                if(Data[[floats[f]]][[paste0(variables[v], '_QC')]][uno] %in% qc_by_var[[variables[v]]]==F){
                  Data_good[[floats[f]]][[variables[v]]][uno]<-NA
                  if(mode==T){
                    Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]][uno]<-NA
                  }
                }
              }
            }else { # Multi profiles         
              for (uno in c(1:dim(Data[[floats[f]]][[variables[v]]])[1])){
                for(duo in c(1:dim(Data[[floats[f]]][[variables[v]]])[2])){
                  
                  if(Data[[floats[f]]][[paste0(variables[v], '_QC')]][uno,duo] %in% qc_by_var[[variables[v]]]==F){
                    Data_good[[floats[f]]][[variables[v]]][uno,duo]<-NA
                    if(mode==T){
                      Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]][uno,duo]<-NA
                    }
                  }
                }
              }
            }
          }
           else if (paste0(variables[v],"_ADJUSTED") %in% names(Data[[f]])==T &&
              all(is.na(Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED')]]))==F){
            Data_good[[floats[f]]][[variables[v]]]<-
              Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED')]]
            if(mode==T){
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                Data[[floats[f]]][[paste0(variables[v], '_DATA_MODE')]]
            }
            #in Case of only one profile
            if(is.null(dim(Data_good[[floats[f]]][[paste0(variables[v],"_ADJUSTED")]])) |
               length(dim(Data_good[[floats[f]]][[paste0(variables[v],"_ADJUSTED")]]))==1
               ){
              for (uno in c(1:length(Data[[floats[f]]][[paste0(variables[v],"_ADJUSTED")]]))){
                if(Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED_QC')]][uno] %in% qc_by_var[[variables[v]]]==F){
                  Data_good[[floats[f]]][[variables[v]]][uno]<-NA
                  if(mode==T){
                    Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]][uno]<-NA
                  }
                }
              }
            } else{ #multi profiles
              for (uno in c(1:dim(Data[[floats[f]]][[paste0(variables[v],"_ADJUSTED")]])[1])){
                for(duo in c(1:dim(Data[[floats[f]]][[paste0(variables[v],"_ADJUSTED")]])[2])){
                  if(Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED_QC')]][uno,duo] %in% qc_by_var[[variables[v]]]==F){
                    Data_good[[floats[f]]][[variables[v]]][uno,duo]<-NA
                    if(mode==T){
                      Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]][uno,duo]<-NA
                    }
                  }
                }
              }
            }
          }else if(raw=="no_strict"){
            warning(paste("adjusted values for float",floats[f],"for", 
                          variables[v],"are not available, this float will not be used"))
            Data_good[[floats[f]]]<-NULL
            break
          }else if(raw=="no"){
            warning(paste("adjusted values for float",floats[f],"for", 
                          variables[v],"are not available, this float will not be used"))
            next
          }else{
            warning(paste("adjusted values for", variables[v],"are not available"))
            Data_good[[floats[f]]][[variables[v]]]<-
              Data[[floats[f]]][[variables[v]]]
            if(mode==T){
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                Data[[floats[f]]][[paste0(variables[v], '_DATA_MODE')]]
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                replace(Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]], TRUE, 'R')
            }
            if(is.null(dim(Data_good[[floats[f]]][[variables[v]]]))){
              for (uno in c(1:length(Data[[floats[f]]][[variables[v]]]))){
                if(Data[[floats[f]]][[paste0(variables[v], '_QC')]][uno] %in% qc_by_var[[variables[v]]]==F){
                  Data_good[[floats[f]]][[variables[v]]][uno]<-NA
                  if(mode==T){
                    Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]][uno]<-NA
                  }
                }
              }
            } else {          
              for (uno in c(1:dim(Data[[floats[f]]][[variables[v]]])[1])){
                for(duo in c(1:dim(Data[[floats[f]]][[variables[v]]])[2])){
                  
                  if(Data[[floats[f]]][[paste0(variables[v], '_QC')]][uno,duo] %in% qc_by_var[[variables[v]]]==F){
                    Data_good[[floats[f]]][[variables[v]]][uno,duo]<-NA
                    if(mode==T){
                      Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]][uno,duo]<-NA
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  } else if(type=="detailed"){
    Data_good<-list()
    for (f in (1:nfloats)){
      # create basic lists to build off of
      
      Data_good[[floats[f]]]$CYCLE_NUMBER<-Data[[floats[f]]]$CYCLE_NUMBER
      Data_good[[floats[f]]]$TIME<-Data[[floats[f]]]$TIME
      Data_good[[floats[f]]]$LATITUDE<-Data[[floats[f]]]$LATITUDE
      Data_good[[floats[f]]]$LONGITUDE<-Data[[floats[f]]]$LONGITUDE
      Data_good[[floats[f]]]$JULD<-Data[[floats[f]]]$JULD
      
      for (v in (1:nvar)){
        if ( variables[v] %in% names(Data[[f]])==F){
          warning(paste("float",  floats[f] ,"does not contain variable", variables[v])) 
        }
        
        else{
          if(raw=="yes_strict"){
            Data_good[[floats[f]]][[variables[v]]]<-
              Data[[floats[f]]][[variables[v]]]
            Data_good[[floats[f]]][[paste0(variables[v],'_QC')]]<-
              Data[[floats[f]]][[paste0(variables[v], '_QC')]]
            if(mode==T){
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                Data[[floats[f]]][[paste0(variables[v], '_DATA_MODE')]]
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                replace(Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]], TRUE, 'R')
            }
          }
          else if (paste0(variables[v],"_ADJUSTED") %in% names(Data[[f]])==T &&
              all(is.na(Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED')]]))==F){
            Data_good[[floats[f]]][[variables[v]]]<-
              Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED')]]
            Data_good[[floats[f]]][[paste0(variables[v],'_QC')]]<-
              Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED_QC')]]
            if(mode==T){
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                Data[[floats[f]]][[paste0(variables[v], '_DATA_MODE')]]
            }
          }else if(raw=="no_strict"){
            warning(paste("adjusted values for float",floats[f],"for", 
                          variables[v],"are not available, this float will not be used"))
            Data_good[[floats[f]]]<-NULL
            break
          }else if(raw=="no"){
            warning(paste("adjusted values for float",floats[f],"for", 
                          variables[v],"are not available, this float will not be used"))
            next
          }else{
            warning(paste("adjusted values for", variables[v],"are not available"))
            Data_good[[floats[f]]][[variables[v]]]<-
              Data[[floats[f]]][[variables[v]]]
            Data_good[[floats[f]]][[paste0(variables[v],'_QC')]]<-
              Data[[floats[f]]][[paste0(variables[v], '_QC')]]
            if(mode==T){
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                Data[[floats[f]]][[paste0(variables[v], '_DATA_MODE')]]
              Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]]<-
                replace(Data_good[[floats[f]]][[paste0(variables[v], '_MODE')]], TRUE, 'R')
            }
          }
        }
      }
    }
  }
  
  if (format!="dataframe"){
    return(Data_good)
  }
  
  if (format=="dataframe"){ # convert the data into the data frame format 
    float_data_list_dtfr= vector("list",
                                 length(Data_good)
    )# Create a list to store the multiple data frame for each float data
    
    for (i in 1:length(Data_good)  ){ # loop for each float data 
      
      float_data_single=Data_good[[i]] # Pull out each float
      length_float_data=length(  float_data_single$CYCLE_NUMBER)
      number_variable_float_data=length(float_data_single)
      
      # create a matrix to deposit the float data
      float_data_single_dtfr= matrix (nrow=   length_float_data,
                                      ncol=   number_variable_float_data) 
      float_data_single_dtfr=as.data.frame(  float_data_single_dtfr)
      colnames(float_data_single_dtfr) = names(float_data_single) # names the data frame
      
      # loop to transform the each variable 
      for (ii in 1:   number_variable_float_data ){  
        float_data_single_dtfr[,ii]=as.vector(float_data_single[[ii]])
      } # end loop in number_variable_float_data
      
      float_data_single_dtfr$WMOID= names(Data[i])  # add the WMOID in data frame 
      
      #  assign data frame into the list array 
      float_data_list_dtfr[[i]]=  float_data_single_dtfr 
      names(  float_data_list_dtfr)[i] <-  names(Data_good[i]) #  name the each element in list 
      
    }# end loop in float_data
    
    # merge multiple dataframes into a single one
    tryCatch( {
      float_data_list_dtfr=bind_rows(float_data_list_dtfr)
    },  error = function(e){
      print("data exceeds the memorylimit of dataframe so data isinput as a list containing multiple data frame for each float ")
    }
    )
    
    return(float_data_list_dtfr)
    
  } # end loop format=="dataframe"
  
  
}






