initialize_argo <- function() {
 
  
  # DESCRIPTION:
  #   It defines standard Setting and paths and downloads index files.
  #   It must be called once before any other functions
  #   can be used, either directly or indirectly by calling any of
  #   the functions load_float_data, select_profiles, show_profiles,
  #   show_sections, or show_trajectories.
  #
  # UPDATE RECORD: 
  #   Version 1 & 2:   June 2021 
  #   Version 2.1: January 2022 
  #
  # CITATION:
  #   M. Cornec (LOV, now at NOAA-PMEL), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), R. Sauzede (IMEV) and 
  #   C. Schmechtig (OSU ECCE TERRA), 2021.
  #   BGC-Argo-R: A R toolbox for accessing and visualizing Biogeochemical Argo data. 
  #   Zenodo. http://doi.org/10.5281/zenodo.5028138
  
  
  Setting<<-list() 
  Sprof<<-list() 
  Float<<-list()

  ###########################################################################
  # BEGINNING OF SECTION WITH USER SPECIFIC OPTIONS
  # this part of the function can be modified to meet specific needs
  ###########################################################################
  
  # By default, the tutorial pauses at several steps when it is run in non-desktop mode.  Set this to 0
  # if you want to run everything without stopping.
  
  Setting$use_pause <<- 1
  
  # By default, actively running commands are described with output
  # to the command window. Set this to 0 to suppress this output.
  # Values larger than 1 can be used to help in debugging.
  Setting$verbose <<- 1
  
  # Maximum number of plots that can be created with one call to
  # show_profiles etc.
  # Increase this number if necessary, if you are sure that 
  # your system can handle it.
  Setting$max_plots <<- 20
  
  # Profiles are stored in subdirectory 'Profiles'
  Setting$prof_dir <<- './Profiles/'
  
  # Index files are stored in subdirectory 'Index'
  Setting$index_dir <<- './Index/'
  
  
  Setting$demo_float <<- 5904021
  
  # By default, don't update if files are less than 1 hour old
  # alternative Setting are 0 (don't update at all if files exist
  # locally already) or 1 (always update)
  Setting$update <<- 3600 # time is given in seconds
  
  # default values for computation of mixed layer depth
  Setting$temp_thresh <<- 0.2
  Setting$dens_thresh <<- 0.03
  
  # Default: try US GDAC before French GDAC
  host_ifremer = 'https://data-argo.ifremer.fr/'
  host_godae = 'https://usgodae.org/ftp/outgoing/argo/'
  # Additional hosts could be added here
  
  Setting$hosts <<- c(host_godae,host_ifremer)
  # Setting$hosts = {host_ifremer;host_godae}; #alternate order of hosts
  
  ###########################################################################
  # END OF SECTION WITH USER SPECIFIC OPTIONS
  # the rest of this function should not be modified
  ###########################################################################
  
  
  # Create Index directory if needed
  if (check_dir(Setting$index_dir)==0) {
    stop('Index directory could not be created')
  }
  
  # Create Profile directory if needed
  if (check_dir(Setting$prof_dir)==0) {
    stop('Profile directory could not be created')
  }
  
  # Full set of available variables (but not all floats have all sensors)
  Setting$avail_vars <<- c('PRES','PSAL','TEMP','DOXY','BBP','BBP470','BBP532',
                           'BBP700','TURBIDITY','CP','CP660','CHLA','CDOM','NITRATE','BISULFIDE',
                           'PH_IN_SITU_TOTAL','DOWN_IRRADIANCE','DOWN_IRRADIANCE380',
                           'DOWN_IRRADIANCE412','DOWN_IRRADIANCE443','DOWN_IRRADIANCE490',
                           'DOWN_IRRADIANCE555','DOWN_IRRADIANCE670','UP_RADIANCE',
                           'UP_RADIANCE412','UP_RADIANCE443','UP_RADIANCE490','UP_RADIANCE555',
                           'UP_RADIANCE','UP_RADIANCE412','UP_RADIANCE443','UP_RADIANCE490',
                           'UP_RADIANCE555','DOWNWELLING_PAR','DOXY2','DOXY3')
  
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
  # column 6: profiler type
  # column 7: institution
  Sprof$sens <<- H[,8]
  Sprof$data_mode <<- H[,9]
  Sprof$date_update <<- ymd_hms(H[,10])
  
  
  
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
  Float$nfloats <<- length(uwmo)
  # range of profile indices per float
  Float$prof_idx1 <<- ia
  ia<-c(ia,(length(sprof_urls) + 1))
  Float$prof_idx2 <<- ia[2:length(ia)] - 1
  Float$update <<-Sprof$date_update[Float$prof_idx2]
  
}
