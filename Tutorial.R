# Tutorial.R

# Driver routine for the GO-BGC workshop R tutorial
# June 28-30, 2021 (https://www.youtube.com/watch?v=w_6pEGNXQQ4)
# Demonstrates the downloading of BGC-Argo float data with sample plots,
# a discussion of available data, quality control flags etc.
#
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
# DATE: JUNE 1, 2022  (Version 1.0.1)

# Close figures, clean up workspace, clear command window
cat("\014")
rm(list = ls())


# Fill here the path to the code directory, you can instead set the code
# directory as the working directory with setwd()
path_code = ""


# Load the functions and libraries--------------------------------
setwd(path_code)
func.sources = list.files(path_code,pattern="*.R")
func.sources = func.sources[which(func.sources %in% c('Tutorial.R',
                                                      "oneargo_r_license.R")==F)]

if(length(grep("Rproj",func.sources))!=0){
  func.sources = func.sources[-grep("Rproj",func.sources)]
}
invisible(sapply(paste0(func.sources),source,.GlobalEnv))

aux.func.sources = list.files(paste0(path_code,"/auxil"),pattern="*.R")
invisible(sapply(paste0(path_code,"/auxil/",aux.func.sources),source,.GlobalEnv))

# Exercise 0: Initialize --------------------------------------------------
# This function defines standard settings and paths and creates Index
# and Profiles folders in your current path. It also downloads the Sprof 
# index file from the GDAC to your Index folder. The Sprof index is 
# referenced when downloading and subsetting float data based on user 
# specified criteria in other functions.

initialize_argo() # Take some minutes to download the global Index

do_pause()

## Examine global structures
# These global structures contain a variety of useful variables for
# downloading and manipulating float data. 'Sprof' contains fields
# with information for each profile, 'Float' contains fields with
# information for each float, 'Settings' contains settings to be used in
# the backgroud during plotting, etc. Variables in the global structures 
# can be altered within the initialize_argo.m file.

# Example: Look at the profile ID numbers and available sensors for the
# profiles that have been executed by new GO-BGC float #5906439.


float_idx <-which(Float$wmoid=='5906439') # float IDs for float #5906439 in the S_file index
float_idx 

prof_ids = c(Float$bgc_prof_idx1[float_idx]:Float$bgc_prof_idx2[float_idx]) # profile IDs for float #5906439 in the S_file index
prof_ids 

dates = Sprof$date[prof_ids] # dates of each profile from float #5906439
dates  

sensors = unique(Sprof$sens[prof_ids]) # sensors available for float #5906439
sensors 

do_pause()

rm (list= c("float_idx","prof_ids","sensors","dates")) # clean up workspace


# Exercise 1: SOCCOM float ------------------------------------------------
# In this exercise, we download the NetCDF file for a Southern Ocean  
# BGC float, inspect its contents, show the trajectory, plot profiles
# for unadjusted and adjusted data, and show the effect of adjustments 
# made to the nitrate concentrations.

# Download NetCDF file for float #5904183, a SOCCOM float with multiple seasons under ice
WMO = 5904859 
success = download_float(WMO)

#  Display attributes, dimensions, and variables available in the NetCDF
float_file = nc_open(paste0("Profiles/", WMO,"_Sprof.nc"))
float_file

# Extract informational data from the NetCDF
names (float_file$var) 

# Close the file
nc_close(float_file)

do_pause()

# We see that NITRATE is available, so load it (along with TEMP and PSAL) from the NetCDF
data = load_float_data( float_ids= WMO, # specify WMO number
                        variables=c('PSAL','TEMP','NITRATE') # specify variables
)

names(data$Data[[paste0('F', WMO)]]) # show data that has been loaded into R

do_pause()


# Load the float data in the R with the format of data frame if "format" is specificed
data_df = load_float_data( float_ids= WMO, # specify WMO number
                        variables=c('PSAL','TEMP','NITRATE'), # specify variables,
                        format="dataframe" # specify format;  
)

colnames(data_df) # show data that has been loaded into R


# Show the trajectory of the downloaded float
show_trajectories(float_ids=WMO, 
                  return_ggplot="True" # return the plot to ggplot panel
                  )

do_pause()

# Show all profiles for salinity and nitrate from the downloaded float
# this plots the raw, unadjusted data, and includes multiple profiles 
# compromised by biofouling that has affected the optics.

show_profiles( float_ids=WMO, 
               variables=c('PSAL','NITRATE'),
               obs='on', # 'on' shows points on the profile at which each measurement was made
               raw="yes" # show the unadjusted data ,
              
)


# this plots the adjusted data.
show_profiles(float_ids=WMO, 
              variables=c('PSAL','NITRATE'),
              obs='on', # 'on' shows points on the profile at which each measurement was made
              raw="no",
)

# this plots the adjusted, good (qc flag 1) and probably-good (qc flag 2) data.
show_profiles(float_ids = WMO, 
              variables=c('PSAL','NITRATE'),
              obs='on', # 'on' shows points on the profile at which each measurement was made
              qc_flags =c(1:2) # tells the function to plot good and probably-good data
)

do_pause()

# Show sections for nitrate
# this shows the raw, unadjusted data (pcolor plot)
# mixed layer depth is shown based on the temperature threshold
# (set the value to 2 after 'mld' to use the density threshold instead)
show_sections(float_ids=WMO, 
              variables= c('NITRATE'),
              plot_mld=1,       # tells the function to plot mixed layer depth
              raw="yes") # tells the function to plot raw data

show_sections( float_ids=WMO, 
               variables= c('NITRATE'),
               plot_mld=1, #tells the function to plot mixed layer depth
               raw="no") # tells the function to plot adjusted data (that is the default and could be left out in this call)

show_sections( float_ids=WMO ,
               variables= c('NITRATE'),
               plot_mld=1, #tells the function to plot mixed layer depth
               raw="no",
               qc=c(1:2) # tells the function to plot good and probably-good data
)

do_pause()

## Clean up the workspace
cat("\014")
rm (list= c("data","float_file","success","WMO","data_df")) 


# Exercise 2: Ocean Station Papa floats -----------------------------------
# In this exercise, we define a region in the Northeast Pacific along with
# a duration of time, and identify the float profiles matching that
# criteria. We show the trajectories of all the matching floats and plot
# profiles that match the criteria for one of the floats.

# Set limits near Ocean Station Papa from 2008 to 2018
lat_lim=c(45, 60)
lon_lim=c(-150, -135)
start_date="2008-01-01"
end_date="2018-12-31"

# Select profiles based on those limits with specified sensor (NITRATE)

OSP_data= select_profiles ( lon_lim, 
                            lat_lim, 
                            start_date,
                            end_date,
                            sensor=c('NITRATE'), # this selects only floats with nitrate sensors
                            outside="both" #  All floats that cross into the time/space limits
)  # are identified from the Sprof index. The optional 
# 'outside' argument allows the user to specify
# whether to retain profiles from those floats that
# lie outside the space limits ('space'), time
# limits ('time'), both time and space limits 
# ('both'), or to exclude all profiles that fall 
# outside the limits ('none'). The default is 'none'


# Display the number of matching floats and profiles
print(paste('# of matching profiles:',sum(lengths(OSP_data$float_profs))))

print(paste('# of matching floats:',length(OSP_data$float_ids)))

# Load the data for the matching float with format of data frame
data_OSP_df= load_float_data( float_ids= OSP_data$float_ids, # specify WMO number
                           float_profs=OSP_data$float_profs, # specify selected profiles
                            variables="ALL", # load all the variables
                            format="dataframe" # specify format;  
)


# Show trajectories for the matching floats
# This function downloads the specified floats from the GDAC (unless the
# files have already been downloaded) and then loads the data for plotting.
# Adding the optional input pair 'color','multiple' will plot different
# floats in different colors
trajectory = show_trajectories(float_ids = OSP_data$float_ids,
                               return_ggplot = TRUE #do not plot and return a ggplot object
) # this plots different floats in different colors

x11()# create a new window
plot(trajectory) # plot the ggplot object

# show domain of interest
trajectory = trajectory + geom_rect( aes(xmin = lon_lim[1], 
                                         xmax = lon_lim[2],
                                         ymin = lat_lim[1],
                                         ymax = lat_lim[2]),
                                     color="black",fill=NA
)

x11()# create a new window
plot(trajectory) # plot the ggplot object

do_pause()


# Show profile plots for the first of these matching floats

# Case #1: all profiles from one float (1)

show_profiles(float_ids=OSP_data$float_ids[1], 
              variables=c('PSAL','DOXY'),
              obs='on',# 'on' shows points on the profile at which
                       #  each measurement was made
)

# Case #2: mean and standard deviation of all profiles from one float (1)

show_profiles(float_ids=OSP_data$float_ids[1], 
              variables=c('PSAL','DOXY'),
              obs='on', # 'on' shows points on the profile at which
              #  each measurement was made
              method="mean" # this tells the function to just plot the mean profile
)

do_pause()

# clean up the workspace
rm (list= c("OSP_data","data_OSP_df")) 


# Exercise 3: Hawaii floats -----------------------------------------------
# In this exercise, we define a region near Hawaii along with a duration of
# time. Again, we identify the float profiles matching those criteria, show
# their trajectories, plot all the matching profiles on one figure, and
# show sections for the unadjusted and adjusted values of salinity and
# dissolved oxygen.

# Set limits near Hawaii from 2017 to 2019

lat_lim=c(22, 26)
lon_lim=c(-160, -155)
start_date="2017-01-01"
end_date="2019-12-31"


# Select profiles based on those limits

HW_data= select_profiles ( lon_lim, 
                           lat_lim, 
                           start_date,
                           end_date,
                           outside="none" # exclude profiles outside the time and space limits
)


# display the number of matching floats and profiles
print(paste('# of matching profiles:',sum(lengths(HW_data$float_profs))))

print(paste('# of matching floats:',length(HW_data$float_ids)))


# Show trajectories for the matching floats, along with the geo limits
# This function downloads the specified floats from the GDAC (unless the
# files have already been downloaded) and then loads the data for plotting.

trajectory = show_trajectories(float_ids=HW_data$float_ids, 
                               return_ggplot=TRUE)  # this plots different floats in different colors

x11() # new window
plot(trajectory)

# show domain of interest
trajectory = trajectory + geom_rect(
  aes(xmin = lon_lim[1], 
      xmax = lon_lim[2],
      ymin = lat_lim[1],
      ymax = lat_lim[2], 
  ),
  color="black",
  fill=NA
)

x11() # new window
plot(trajectory)

do_pause()

# Show matching profiles from all floats
# Show profiles (from all floats) within specified domain and times
show_profiles( float_ids=HW_data$float_ids, 
               variables=c('PSAL','DOXY'),
               float_profs=HW_data$float_profs,
               per_float=F,  # show all profiles in one plot
               qc_flags =c(1,2) # tells the function to plot good and probably-good data
)

do_pause()

## Show only matching profiles from September
# determine profiles that occur in September for each float separately
date<-get_lon_lat_time(float_ids=HW_data$float_ids,
                       float_profs=HW_data$float_profs)$time

HW_float_profs_Sep<-list()
for (f in 1:length(HW_data$float_ids)){
  HW_float_profs_Sep[[f]] <-
    HW_data$float_profs[[f]][which(month(as.POSIXct(date[[f]]))==9)]
  
}

show_profiles( float_ids=HW_data$float_ids, 
               variables=c('PSAL','DOXY'),
               float_profs = HW_float_profs_Sep, 
               per_float=F,  # show all profiles in one plot:
               obs='on', # plot a marker at each observation
               qc_flags=c(1,2),  # apply QC flags
               title_add= 'September' 
)

do_pause()

# Show sections for pH and oxygen for the fifth float in the list of Hawaii floats
# this shows the raw, unadjusted data 
# mixed layer depth is shown based on the temperature threshold
# (set the value to 2 after 'mld' to use the density threshold instead)
show_sections( float_ids=HW_data$float_ids[5], 
               variables=c('PH_IN_SITU_TOTAL','DOXY'),
               plot_mld=1,   # tells the function to plot mixed layer depth
               raw="yes" # tells the function to plot raw (unadjusted) data
) # tells the function to plot raw (unadjusted) data

do_pause()

# Show sections for pH and oxygen for the fifth float in the list of Hawaii floats
# this shows the adjusted data
show_sections( float_ids=HW_data$float_ids[5], 
               variables=c('PH_IN_SITU_TOTAL','DOXY'),
               plot_mld=1,   # tells the function to plot mixed layer depth
               raw="no"  # tells the function to plot adjusted data
)

# Show time series of near-surface pH and oxygen for two floats

# The pH sensor for float 5906039 failed in late 2019, 
# which is evident from the premature end to 
# the blue line halfway through the first figure.
show_time_series ( float_ids=HW_data$float_ids[4:5], 
                   variables=c('PH_IN_SITU_TOTAL','DOXY'),
                   plot_depth=20, # tells the function to plot the time-series for the given depth 
                   raw="no"   # tells the function to plot raw data
) 


# clean up the workspace
cat("\014")
rm(list = ls())







