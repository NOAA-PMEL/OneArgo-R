# qc_filter  This function is part of the
# R toolbox for accessing Argo float data.
#
# USAGE:
#   Data_good = qc_filter(Data, variables, qc_flags)
#
# DESCRIPTION:
#   This function generates a new data structure composed of chosen variables
#   based on provided QC flag values.
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
# OPTIONAL INPUTS:
#   qc_flags: numerical array of QC flag values (default: c(1,2))
#
#
# OUTPUT:
#   Data_good: struct that contains all the varaiables from the input Data
#              struct that match the given QC flags;
#              all other values are set to NA (the size of the arrays is
#              unchanged)
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

qc_filter<-function(Data, 
                    variables="PRES", 
                    qc_flags=NULL,
                    format=NULL
){
  
  # assign default qc_flags if none provided as input
  if(is.null(qc_flags)){
    qc_flags = c(1,2)
  }
  
  if ( is.null   (format)  ){ # Set to export the data in the format of listif "format" are not specific
    
    format="list"
    
  }
  
  # Add pres varaible
  if("PRES" %in% variables==F){
    if("PRES_ADJUSTED" %in% variables==F){
      variables<-c(variables,"PRES")
    }
  }
  nvar = length(variables)
  
  # establish qc structure to reference
  qc_by_var<-list()
  for (v in c(1:nvar)){
    qc_by_var[[variables[v]]] = qc_flags
  }
  
  variables = names(qc_by_var)
  nvar = length(variables)
  
  floats = names(Data)
  nfloats = length(floats)
  
  
  Data_good<-list()
  for (f in (1:nfloats)){
    # create basic structure to build off of
    
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
        if (paste0(variables[v],"_ADJUSTED") %in% names(Data[[f]])==T ){
          Data_good[[floats[f]]][[variables[v]]]<-
            Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED')]]
          if(is.null(dim(Data_good[[floats[f]]][[variables[v]]]))){
            for (uno in c(1:length(Data[[floats[f]]][[paste0(variables[v],"_ADJUSTED")]]))){
              if(Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED_QC')]][uno] %in% qc_by_var[[variables[v]]]==F){
                Data_good[[floats[f]]][[variables[v]]][uno]<-NA
              }
            }
          }else{
            for (uno in c(1:dim(Data[[floats[f]]][[paste0(variables[v],"_ADJUSTED")]])[1])){
              for(duo in c(1:dim(Data[[floats[f]]][[paste0(variables[v],"_ADJUSTED")]])[2])){
                if(Data[[floats[f]]][[paste0(variables[v], '_ADJUSTED_QC')]][uno,duo] %in% qc_by_var[[variables[v]]]==F){
                  Data_good[[floats[f]]][[variables[v]]][uno,duo]<-NA
                  
                }
              }
            }
          }
        }else{
          warning(paste("adjusted values for", variables[v],"are not available"))
          Data_good[[floats[f]]][[variables[v]]]<-
            Data[[floats[f]]][[variables[v]]]
          if(is.null(dim(Data_good[[floats[f]]][[variables[v]]]))){
            for (uno in c(1:length(Data[[floats[f]]][[variables[v]]]))){
              if(Data[[floats[f]]][[variables[v]]][uno] %in% qc_by_var[[variables[v]]]==F){
                Data_good[[floats[f]]][[variables[v]]][uno]<-NA
              }
            }
          } else {          
            for (uno in c(1:dim(Data[[floats[f]]][[variables[v]]])[1])){
              for(duo in c(1:dim(Data[[floats[f]]][[variables[v]]])[2])){
                
                if(Data[[floats[f]]][[paste0(variables[v], '_QC')]][uno,duo] %in% qc_by_var[[variables[v]]]==F){
                  Data_good[[floats[f]]][[variables[v]]][uno,duo]<-NA
                  
                }
              }
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
      
      
      # create a matrix to deposite the float data
      float_data_single_dtfr= matrix (nrow=   length_float_data,
                                      ncol=   number_variable_float_data
                                      
      ) 
      float_data_single_dtfr=as.data.frame(  float_data_single_dtfr)
      colnames(float_data_single_dtfr) = names(float_data_single) # names the data frame
      
      
      # loop to tranform the each variable 
      for (ii in 1:   number_variable_float_data ){  
        
        
        float_data_single_dtfr[,ii]=as.vector(float_data_single[[ii]])
        
        
      } # end loop in number_variable_float_data
      
      
      float_data_single_dtfr$WMOID= names(Data[i])  # add the WMOID in data frame 
      
      
      #  assign data frame into the list array 
      float_data_list_dtfr[[i]]=  float_data_single_dtfr 
      names(  float_data_list_dtfr)[i] <-  names(Data_good[i]) #  name the each element in list 
      
    }# end loop in float_data
    
    # merge the multiple data frame into the single one
    tryCatch( {
      
      float_data_list_dtfr=bind_rows(float_data_list_dtfr)
    },  error = function(e){
      
      print("data exceeds the memorylimit of dataframe so data isinput as a list containing multiple data frame for each float ")
    }
    )
    
    
    return( float_data_list_dtfr)
    
    
  } # end loop format=="dataframe"
  
  
}






