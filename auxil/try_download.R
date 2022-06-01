try_download<-function (filename, dest_path) {
  
  # DESCRIPTION:
  #   This function attempts to download a file from any of the GDACs specified 
  #   in the Setting$hosts cell array.
  #
  # INPUTS:
  #   filename  : name of the file at the GDAC
  #   dest_path : full (relative or absolute) path to the local file
  #
  # OUTPUT:
  #   success   : 1 for successful download; 2 for unsuccessful download,
  #               but the file exists locally already; 0 for failure
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
  
  success = 0 # default: failure
  for (h in 1:length(Setting$hosts)) {
    
    if (Setting$verbose==1) {
      print(paste("Attempting download",Setting$hosts[h],"into",dest_path,"..."))
      
      tryCatch(download.file(paste(Setting$hosts[h],filename,sep=""),
                             dest_path, mode = "wb", quiet = T), 
               error = function(e) success = 2)
      if (file.exists(dest_path)) {
        print("success!")
        success = 1
        break
      } else {
        print("failure!")
        if(file.exists(paste0(dest_path,".html"))){
          file.remove(paste0(dest_path,".html"))
        }
          }
    }
    
    if (Setting$verbose==0) {
      tryCatch(download.file(paste(Setting$hosts[h],filename,sep=""),
                             dest_path, mode = "wb", quiet = T), 
               error = function(e) success = 2)
      if (file.exists(dest_path)) {
        success = 1
        break
      }
    }
    
    
    if (h == length(Setting$hosts)) {
      if(!file.exists(dest_path)) {
        success==0
      } else {success==2}
    }
  }
  return(success)
}


