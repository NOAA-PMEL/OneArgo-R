
load_float_data_dtfr=function(float_ids,variables=NULL,float_profs=NULL,format=NULL) {
 
  # load_floats in format of data frame
  # This function is part of the
  # GO-BGC workshop Matlab tutorial for accessing BGC Argo float data.
  #
  # This function loads data of at least one specified float.
  #
  # Inputs:
  #   float_ids   : WMO ID of one or more floats
  #                
  #
  # Optional inputs:
  #
  #   variables   : cell array with variable names to be loaded (use 'ALL'
  #                  # to load all available variables, which may differ by
  #                  float)
  #
  #   float_profs : cell array with IDs of selected profiles (per float,
  #                 not global); by default, all profiles of the given floats are loaded
  #   format:by default, the data frame for each float is deposited in the list file 
  #
  # Output:
  #
  #   Single data frame : when the data are within the length limit of the data frame
  #   A List containing multiple data frame : in the case where data exceeds the length limit of the data frame
  
  
  # Load the float data first 
  float_data= load_float_data(float_ids,variables="PSAL",float_profs=NULL)
 
  
  
  if (is.null(format)){ # set the default of "format" to "list"
    format="list"
  }
  # Create a list to store the multiple data frame for each float data
  
  float_data_list_dtfr= vector("list",
                                length(float_data$Data)
                                 )# Create a list to store the multiple data frame for each float data
   
   
  for (i in 1:length(float_data$Data)  ){ # loop for each float data 
   
   float_data_single= float_data$Data[[i]] # Pull out each float
   
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
   

   float_data_single_dtfr$WMOID= names(float_data$Data[i])  # add the WMOID in data frame 
   
   
   #  assign data frame into the list array 
   float_data_list_dtfr[[i]]=  float_data_single_dtfr 
   names(  float_data_list_dtfr)[i] <-  names(float_data$Data[i]) #  name the each element in list 
   
  }# end loop in float_data
 

 
 # Convert the list to single data frame 
  if (format!="list"){
    
    tryCatch( {
      
      float_data_list_dtfr=bind_rows(float_data_list_dtfr)
    },  error = function(e){
      
      print("data exceeds the memorylimit of dataframe so data isinput as a list containing multiple data frame for each float ")
    }
    )
    
    
  }
   
 return( float_data_list_dtfr)
   
}