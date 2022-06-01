initialize_argo <- function() {
  
  
  # DESCRIPTION:
  #   It defines standard Setting and paths and downloads index files.
  #   It must be called once before any other functions
  #   can be used, either directly or indirectly by calling any of
  #   the functions load_float_data, select_profiles, show_profiles,
  #   show_sections, or show_trajectories.
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
  
  Setting<<-list() 
  Sprof<<-list() 
  Prof<<-list() 
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
  dest_path_sprof<-paste(Setting$index_dir,sprof,sep="")
  dest_path_sprof_gz = paste(Setting$index_dir,sprof_gz,sep="")
  if (do_download(dest_path_sprof_gz)==1){
    if (Setting$verbose==1) {
      print('Sprof index file will now be downloaded.')
      print('Depending on your internet connection, this may take a while.')
    }
    
    
    
    if(try_download(sprof_gz,dest_path_sprof_gz)!=1) {
      print('Sprof index file could not be downloaded')
    }
    
    if(file.exists(dest_path_sprof)) {unlink(dest_path_sprof) }
    gunzip(dest_path_sprof_gz,destname=dest_path_sprof, remove=F)
  }
  
  
  
  # Extract information from Sprof index file
  # NOTE that some quantities will be kept per float (struct Float):
  # file_path, file_name, dac, params, wmoid, update
  # Others will be kept per profile (struct Sprof):
  # date, lat, lon, sens(ors), data_mode
  
  H<-read.table(dest_path_sprof, skip=9, sep = ",")
  
  Sprof$urls <<- as.character(H[,1])
  sprof_date = H[,2]
  Sprof$date <<-ymd_hms(sprof_date)
  Sprof$lat <<- H[,3]
  Sprof$lon <<- H[,4]
  Sprof$ocean <<- H[,5]
  # column 6: profiler type
  # column 7: institution
  Sprof$sens <<- H[,8]
  Sprof$split_sens <<- strsplit(Sprof$sens," ") 
  Sprof$data_mode <<- H[,9]
  Sprof$date_update <<- ymd_hms(H[,10])
  
  # Extract unique floats
  Sprof$wmo<<-matrix(unlist(strsplit(Sprof$urls,"/")), ncol=4, byrow=TRUE)[,2]
  
  uwmo_sprof = unique(Sprof$wmo) # keep list order
  ia = seq_along(Sprof$wm)[!duplicated(Sprof$wm)]  ## logical vector of unique values
  bgc_prof_idx1 <- ia
  ia<-c(ia,(length(Sprof$urls) + 1))
  bgc_prof_idx2 <- ia[2:length(ia)] - 1
  
  # Download prof index file from GDAC to Index directory
  prof = 'ar_index_global_prof.txt' # file used locally
  prof_gz = paste(prof,'.gz',sep="") # file that is available at GDAC
  dest_path_prof <- paste(Setting$index_dir,prof,sep="")
  dest_path_prof_gz = paste(Setting$index_dir,prof_gz,sep="")
  if (do_download(dest_path_prof_gz)==1){
    if (Setting$verbose==1) {
      print('prof index file will now be downloaded.')
      print('Depending on your internet connection, this may take a while.')
    }
    
    
    
    if(try_download(prof_gz,dest_path_prof_gz)!=1) {
      print('prof index file could not be downloaded')
    }
    
    if(file.exists(dest_path_prof)) {unlink(dest_path_prof) }
    gunzip(dest_path_prof_gz,destname=dest_path_prof, remove=F)
  }
  
  H<-read.table(dest_path_prof, skip=9, sep = ",")
  
  Prof$urls <<- as.character(H[,1])
  prof_date = H[,2]
  Prof$date <<-ymd_hms(prof_date)
  Prof$lat <<- H[,3]
  Prof$lon <<- H[,4]
  Prof$ocean <<- H[,5]
  Prof$profiler <<- H[,6]
  # column 7: institution
  Prof$date_update <<- ymd_hms(H[,8])
  
  # the split_sens field is needed by select_profiles_per_type
  pT = list(c('PRES','TEMP')) # for old floats without salinity sensor
  pTS = list(c('PRES','TEMP','PSAL')) # for all new core and deep floats
  Prof$split_sens <<- rep(pTS,length(Prof$urls))
  Prof$split_sens[which(Prof$profiler==845)]<<-pT
  
  # Extract unique floats
  # note that older floats have 5-digit IDs
  Prof$wmo<<-matrix(unlist(strsplit(Prof$urls,"/")), ncol=4, byrow=TRUE)[,2]
  uwmo_prof = unique(Prof$wmo) # keep list order
  ia2 = seq_along(Prof$wm)[!duplicated(Prof$wm)]  ## logical vector of unique values
  ulist = Prof$urls[ia2]
  dacs = matrix(unlist(strsplit(ulist,"/")), ncol=4, byrow=TRUE)[,1]
  prof_fnames = paste(uwmo_prof,"_prof.nc",sep="")
  tmp = matrix(unlist(strsplit(ulist,"/")), ncol=4, byrow=TRUE)[,c(1,2)]
  tmp<-paste(tmp[,1],tmp[,2],sep="/")
  prof_fp = paste(tmp,prof_fnames,sep="/")
  
  # need to find out which floats are phys (in Prof only) and bgc (in Sprof)
  is_uniq_bgc = uwmo_prof %in% uwmo_sprof
  nbgc = length(which(is_uniq_bgc==T)) # of bgc floats (this may be revised later)
  
  # determine index pointers from prof to Sprof files for all BGC floats
  # (this needs to be done before the type is changed for those floats
  # that are listed in Sprof index file but don't have BGC sensors)
  bgc_idx_full = rep(0,(length(is_uniq_bgc)))
  bgc_idx_full[which(is_uniq_bgc==T)] = 1:nbgc
  
  # Put per-float information into global struct Float
  Float$file_path <<- prof_fp
  Float$file_name <<- prof_fnames
  Float$dac <<- dacs
  Float$wmoid <<- uwmo_prof
  Float$nfloats <<- length(uwmo_prof)
  # range of profile indices per float
  Float$prof_idx1 <<- ia2
  ia2<-c(ia2,(length(Prof$urls) + 1))
  Float$prof_idx2 <<- ia2[2:length(ia2)] - 1
  Float$profiler <<- Prof$profiler[Float$prof_idx2]
  # use the update date of the last profile
  Float$update <<-Prof$date_update[Float$prof_idx2]
  type_phys = list('phys') # for old floats without salinity sensor
  Float$type <<- rep(type_phys,length(uwmo_prof))
  Float$type[which(is_uniq_bgc==T)] <<- 'bgc'
  
  # determine types of sensors/variables that are present for some and for
  # all profiles of any given float; also re-flag floats from BGC to phys
  # if they don't have any BGC variables available
  Float$min_sens <<- vector(mode = "list", length = Float$nfloats) # pre-allocate cell arrays
  Float$max_sens <<- vector(mode = "list", length = Float$nfloats)
  len_sens = lapply(Sprof$sens,nchar)
  count = 0
  index_bgc = 0
  is_true_bgc = rep(T,length(bgc_prof_idx1))
  for (f in c(1:Float$nfloats)) {
    if (Float$type[f]== 'phys'){
      # ar_index_global_prof.txt does not contain information about
      # the available sensors per profile
      if (Float$profiler[f] == 845) {
        Float$min_sens[f] <<- list(c('PRES','TEMP'))
      }
      else {1
        Float$min_sens[f] <<- list(c('PRES','TEMP','PSAL'))
      }
      Float$max_sens[f] <<- Float$min_sens[f]
    } else { # BGC float
      index_bgc = index_bgc + 1
      f2 = bgc_idx_full[f]
      idx1 = which.min(unlist(len_sens[bgc_prof_idx1[f2]:bgc_prof_idx2[f2]]))
      idx2 = which.max(unlist(len_sens[bgc_prof_idx1[f2]:bgc_prof_idx2[f2]]))
      # assumption: the shortest string has sensors that are shared among all
      # profiles and the longest string has the union of all available sensors
      Float$min_sens[f] <<- Sprof$split_sens[bgc_prof_idx1[f2] + idx1 - 1]
      Float$max_sens[f] <<- Sprof$split_sens[bgc_prof_idx1[f2] + idx2 - 1]
      # if there are no profiles for this float with any BGC sensors
      # set its type to 'phys'
      bgc_sensors = unlist(Float$max_sens[f])
      tag<-F
      for(w in bgc_sensors) {
        if(!(w %in% c("PRES","TEMP","PSAL","CNDC"))){
          tag<-T
        }
      }
      if(tag==F){
        Float$type[f] <<- 'phys'
        count = count + 1
        is_true_bgc[index_bgc] = F
      } 
    }
  }
  
  print(paste("Note:",count,"floats from Sprof index file do not have BGC sensors"))
  
  # assign index of first and last profile to true BGC floats only, referring
  # to the indices within the Sprof struct
  # these variables should never be used for non-BGC floats, so their value
  # of 0 serves as a flag that would result in an out-of-bounds error
  Float$bgc_prof_idx1 <<- rep(NA,length(Float$prof_idx1))
  Float$bgc_prof_idx2 <<- Float$bgc_prof_idx1
  bgc_prof_idx1 = bgc_prof_idx1[which(is_true_bgc == T)]
  bgc_prof_idx2 = bgc_prof_idx2[which(is_true_bgc == T)]
  Float$bgc_prof_idx1[which(Float$type=='bgc')] <<- bgc_prof_idx1
  Float$bgc_prof_idx2[which(Float$type=='bgc')] <<- bgc_prof_idx2
  
  
  # for all "true" BGC floats, i.e., those that are listed in the Sprof
  # index file and have more than pTS sensors, Sprof files will be used
  # instead of prof files
  idx_bgc = which(Float$type=='bgc')
  print(paste(length(idx_bgc),"true BGC floats were found"))
  idx_phys = which(Float$type=='phys')
  print(paste(length(idx_phys),"core and deep floats were found"))
  Float$file_path[which(Float$type=='bgc')] <<- gsub('prof', 'Sprof',Float$file_path[which(Float$type=='bgc')])
  Float$file_name[which(Float$type=='bgc')] <<- gsub('prof', 'Sprof',Float$file_name[which(Float$type=='bgc')])
  
  # determine profile indices per float for all "true" BGC floats
  Float$update[which(Float$type=='bgc')] = Sprof$date_update[bgc_prof_idx2]
}
