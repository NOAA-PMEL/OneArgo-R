if (!require("gsw")) { install.packages("gsw"); library(gsw) }
if (!require("R.utils")) { install.packages("R.utils"); library(R.utils) } # to gunzip S index file
if (!require("lubridate")) { install.packages("lubridate"); library(lubridate) } # convert date from Sprof to date object
if (!require("Matrix")) { install.packages("Matrix"); library(Matrix) } # create reduced-size matrix


initialize_argo <- function() {
  # initialize_argo  
  #
  #This function is part of the
  # GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
  #
  # It defines standard Setting and paths and downloads index files.
  #
  # CITATION:
  # BGC-Argo-R: A R toolbox for accessing and visualizing
  # Biogeochemical Argo data,
  #
  # AUTHORS: 
  # M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), 
  # R. Sauzede (IMEV) and C. Schmechtig (OSU ECCE TERRA),
  #
  # Adapted from the Matlab toolbox BGC-Argo-Mat:  https://doi.org/10.5281/zenodo.4971318
  # (H. Frenzel, J. Sharp, A. Fassbender (NOAA-PMEL),
  # J. Plant, T. Maurer, Y. Takeshita (MBARI), D. Nicholson (WHOI),
  # and A. Gray (UW))
  
  # Update 24 June 2021
  
  Setting<<-list() 
  Sprof<<-list() 
  Float<<-list()
  
  
  library(gsw)
  library(R.utils) # to gunzip S index file
  library(lubridate) # convert date from Sprof to date object
  library(Matrix) # create reduced-size matrix
  
  # By default, the tutorial pauses at several steps when it is run in non-desktop mode.  Set this to 0
  # if you want to run everything without stopping.
  
  Setting$use_pause <<- 1
  
  # By default, actively running commands are described with output
  # to the command windows. Set this to 0 to suppress this output.
  # Values larger than 1 can be used to help in debugging.
  Setting$verbose <<- 1
  
  # Maximum number of plots that can be created with one call to
  # show_profiles etc.
  # Increase this number if necessary, if you are sure that 
  # your system can handle it
  Setting$max_plots <<- 20
  
  # Profiles are stored in subdirectory 'Profiles'
  Setting$prof_dir <<- './Profiles/'
  
  # Index files are stored in subdirectory 'Index'
  Setting$index_dir <<- './Index/'
  
  # Create Index directory if needed
  if (check_dir(Setting$index_dir)==0) {
    stop('Index directory could not be created')
  }
  
  
  # Create Profile directory if needed
  if (check_dir(Setting$prof_dir)==0) {
    stop('Profile directory could not be created')
  }
  
  Setting$demo_float <<- 5904021
  
  # By default, don't update if files are less than 1 hour old
  # alternative Setting are 0 (don't update at all if files exist
  # locally already) or 1 (always update)
  Setting$update <<- 3600 # time is given in seconds
  
  # default values for computation of mixed layer depth
  Setting$temp_thresh <<- 0.2
  Setting$dens_thresh <<- 0.03
  
  # Default: try French GDAC before US GDAC
  host_ifremer = 'https://data-argo.ifremer.fr/'
  host_godae = 'https://usgodae.org/ftp/outgoing/argo/'
  # Additional hosts could be added here
  # Setting$hosts = {host_ifremer;host_godae};
  Setting$hosts <<- c(host_godae,host_ifremer)
  
  Setting$avail_vars <<- c('PRES','PSAL','TEMP','DOXY','BBP','BBP470','BBP532',
                           'BBP700','TURBIDITY','CP','CP660','CHLA','CDOM','NITRATE','BISULFIDE',
                           'PH_IN_SITU_TOTAL','DOWN_IRRADIANCE','DOWN_IRRADIANCE380',
                           'DOWN_IRRADIANCE412','DOWN_IRRADIANCE443','DOWN_IRRADIANCE490',
                           'DOWN_IRRADIANCE555','DOWN_IRRADIANCE670','UP_RADIANCE',
                           'UP_RADIANCE412','UP_RADIANCE443','UP_RADIANCE490','UP_RADIANCE555',
                           'UP_RADIANCE','UP_RADIANCE412','UP_RADIANCE443','UP_RADIANCE490',
                           'UP_RADIANCE555')
  
  # Write Sprof index file from GDAC to Index directory
  sprof = 'argo_synthetic-profile_index.txt' # file used locally
  sprof_gz = paste(sprof,'.gz',sep="") # file that is available at GDAC
  Setting$dest_path_sprof <<- paste(Setting$index_dir,sprof,sep="")
  dest_path_sprof_gz = paste(Setting$index_dir,sprof_gz,sep="")
  if (do_download(dest_path_sprof_gz)==1){
    if (Setting$verbose==1) {
      print('Sprof index file will now be downloaded.')
      print('Depending on your internet connection, this may take a while.')
    }
    
    
    
    if(try_download(sprof_gz,dest_path_sprof_gz,"sprof")!=1) {
      print('Sprof index file could not be downloaded')
    }
    
    if(file.exists(Setting$dest_path_sprof)) {unlink(Setting$dest_path_sprof) }
    gunzip(dest_path_sprof_gz,destname=Setting$dest_path_sprof, remove=F)
  }
  
  
  
  # Extract information from Sprof index file
  # NOTE that some quantities will be kept per float (struct Float):
  # file_path, file_name, dac, params, wmoid, update
  # Others will be kept per profile (struct Sprof):
  # date, lat, lon, sens(ors), data_mode
  
  H<-read.table(Setting$dest_path_sprof, skip=9, sep = ",")
  
  sprof_urls = as.character(H[,1])
  sprof_date = H[,2]
  Sprof$date <<-ymd_hms(sprof_date)
  Sprof$lat <<- H[,3]
  Sprof$lon <<- H[,4]
  Sprof$ocean <<- H[,5]
  # column 5: ocean basin
  # column 6: profiler type
  # column 7: institution
  Sprof$sens <<- H[,8]
  Sprof$data_mode <<- H[,9]
  sprof_update <<- H[,10]
  Sprof$nprofs <<- length(H[,1])
  
  
  
  # Extract unique floats
  Sprof$wmo<<-matrix(unlist(strsplit(sprof_urls,"/")), ncol=4, byrow=TRUE)[,2]
  
  uwmo = unique(Sprof$wmo) # keep list order
  ia = seq_along(Sprof$wm)[!duplicated(Sprof$wm)]  ## logical vector of unique values
  ulist = sprof_urls[ia]
  dacs = matrix(unlist(strsplit(ulist,"/")), ncol=4, byrow=TRUE)[,1]
  Sprof_fnames = paste(uwmo,"_Sprof.nc",sep="")
  tmp = matrix(unlist(strsplit(ulist,"/")), ncol=4, byrow=TRUE)[,c(1,2)]
  tmp<-paste(tmp[,1],tmp[,2],sep="/")
  Sprof_fp = paste(tmp,Sprof_fnames,sep="/")
  
  # Put per-float information into global struct Float
  Float$file_path <<- Sprof_fp
  Float$file_name <<- Sprof_fnames
  Float$dac <<- dacs
  Float$wmoid <<- uwmo
  Float$update <<- ymd_hms(sprof_update[ia])
  Float$nfloats <<- length(uwmo)
  # range of profile indices per float
  Float$prof_idx1 <<- ia
  ia<-c(ia,(length(sprof_urls) + 1))
  Float$prof_idx2 <<- ia[2:length(ia)] - 1
  
  # Set up float/profile conversion matrix and profile-per-float IDs
  # Details about the Sprof.fprofid array:
  # It has N non-zero entries, where N is the total number of profiles that
  # the Sprof index file contains, which corresponds to the number of lines
  # in that file (minus 9, which is the number of its header lines).
  # These entries are the overall indices of all profiles. 
  # The values are the per-profile indices, i.e.,
  # Sprof(i) = j  -> The i-th overall profile is the j-th profile for that
  # particular float.
  #
  # The Sprof.p2f sparse matrix is used to select profiles of floats
  # that have at least one profile that matches the given space and time
  # constraints.
  # Its dimensions are Sprof.nprofs x Float.nfloats.
  # A vector with Sprof.nprofs entries that match the criteria (1=yes,0=no)
  # multiplied by Sprof.p2f results in a vector that has positive values for
  # all floats that have at least one matching profile, 0s for all floats
  # that do not have any matching profiles. (The number is equal to the 
  # number of profiles per float that match the given constraints.)
  # Multiplying this result with the transpose of Sprof.p2f results in 
  # a vector with Sprof.nprofs entries, positive for all profiles from all
  # floats that have at least one matching profile, zeros for all others.
  # See function select_profiles for the implementation of this 
  # selection algorithm.
  Sprof$p2f <<- Matrix(matrix(0L,nrow=Sprof$nprofs, ncol=Float$nfloats),sparse=T)
  Sprof$fprofid <<- rep(0,Sprof$nprofs) # pre-allocate
  for (f in c(1:Float$nfloats)) {
    Sprof$p2f[Float$prof_idx1[f]:Float$prof_idx2[f],f] <<- 1
    Sprof$fprofid[Float$prof_idx1[f]:Float$prof_idx2[f]] <<-
      c(Float$prof_idx1[f]:Float$prof_idx2[f]) - Float$prof_idx1[f]  + 1
  }
  
}
