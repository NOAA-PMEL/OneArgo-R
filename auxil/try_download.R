try_download<-function (filename, dest_path, ftype) {
  
  # DESCRIPTION:
  #   This function attempts to download a file from any of the GDACs specified in the
  #   Setting.hosts cell array.
  #
  # INPUTS:
  #   filename  : name of the file at the GDAC
  #   dest_path : full (relative or absolute) path to the local file
  #   ftype     : type of the file (used for an informational message only)
  #
  # OUTPUT:
  #   success   : 1 for successul download; 2 for unsuccessful download,
  #               but the file exists locally already; 0 for failure
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


