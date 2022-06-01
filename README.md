# Argo Toolbox for R (OneArgo-R)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6604650.svg)](https://doi.org/10.5281/zenodo.6604650)

## NOTE

This is the first release of this toolbox under the OneArgo-R name. It is a further development of the [BGC-Argo-R](https://github.com/euroargodev/BGC-ARGO_R_WORKSHOP) toolbox.

## ABOUT
This toolbox contains a variety of functions for accessing, processing, and visualizing [Argo](https://argo.ucsd.edu) data, including Core, Deep, and Biogeochemical Argo. Functions are designed to be maximally efficient, to provide access to the most up-to-date data available, and to allow for downloading and plotting of those data based on numerous user-defined conditions.

## CITATION

Please cite this toolbox as:

M. Cornec, Y. Huang, Q. Jutard, R. Sauzede, and C. Schmechtig, 2022. OneArgo-R: An R toolbox for accessing and visualizing Argo data. Zenodo. https://doi.org/10.5281/zenodo.6604650


Note: This toolbox is adapted from the [OneArgo-Mat](https://github.com/NOAA-PMEL/OneArgo-Mat) MATLAB Toolbox (H. Frenzel, J. Sharp, A. Fassbender, N. Buzby, 2022. OneArgo-Mat: A MATLAB toolbox for accessing and visualizing Argo data. Zenodo. https://doi.org/10.5281/zenodo.6588041)

## REQUIREMENTS
1. Rstudio version 1.4 and R version 3.4 (or more recent) are needed to use these functions without modifications.  
2. OSmac users need to install the ["xquartz"](https://www.xquartz.org/) for figure plot.
3. An Internet connection is needed to get the latest versions of index and Sprof files; but the repository includes versions of these files so that it can be run offline. 
4. Memory requirements depend on the number of profiles and variables that are simultaneously loaded into memory. 

## QUICK DEMO

This <a href="demo_notebook.md">sample code</a> shows an example of selecting floats that match geographic and temporal limits and visualizing some of the data.

## INSTALLATION AND USE
This repository can be cloned using the command line or GitHub Desktop. Or the files can be directly downloaded in zipped format.

Before use, make sure the files are placed in a [directory](https://support.rstudio.com/hc/en-us/articles/200711843-Working-Directories-and-Workspaces) that is in the R [search path](https://support.rstudio.com/hc/en-us/articles/200711843-Working-Directories-and-Workspaces). Or add the directory where they are located to the search path. Or run it in the directory where the Tutorial script was placed.

For an overview of how to use this toolbox, step through the 'Tutorial' script (launching it in the R Console, or in the Rstudio console using the Run button or Ctrl+Alt+Enter, the Run All shortcut in R studio), a tutorial that was developed for the [2021 GO-BGC Scientific Workshop](https://www.us-ocb.org/joint-gobgc-workshop/).

## FUNCTIONS

### Main functions (to be called from script or command window):
initialize_argo.R        : defines standard settings and paths and downloads synthetic profile index file<br/>
load_float_data.R        : loads data of one or more specified float(s) into memory<br/>
select_profiles.R        : returns profiles and corresponding floats based on input criteria<br/>
show_profiles.R          : downloads float data and calls plot_profiles to create plot<br/>
show_sections.R          : downloads float data and calls plot_sections to create plot<br/>
show_trajectories.R      : downloads float data and calls plot_trajectories to create plot<br/>
show_time_series.R       : downloads float data and calls plot_time_series to create plot<br/>
get_lon_lat_time.R       : extracts the longitude, latitude, and time information for the specified floats)<br/>

Tutorial.R               : tutorial script for GO-BGC Scientific Workshop (6/30/21)<br/>

### Background functions (primarily called by main functions in background):

calc_auxil.R             : calculates various auxiliary variables from Argo float data<br/>
check_dir.R              : determines if a directory needs to be created and does so if necessary<br/>
combine_variables.R      : combines the given variables along with all associated variables and returns them<br/>
depth_interp.R           : interpolates values for BGC-Argo parameters against depth<br/>
do_download.R            : determines if a file should be downloaded or not<br/>
do_pause.R               : pauses execution of Tutorial (if used without desktop)<br/>
download_float.R         : downloads the Sprof NetCDF file for one float<br/>
download_multi_floats.R  : calls download_float to download Sprof NetCDF files for multiple floats<br/>
get_dims                 : determines the number of profiles, parameters,and depth levels in an Sprof netcdf file<br/> 
get_lon_lat_lims.R       : obtains maximum/minimum latitude and longitude values from input data<br/>
get_multi_profile_mean   : calculates the mean profile of multiple profiles<br/>
get_var_name_units.R     : returns the long variable name and units name for a given short parameter name input<br/>
load_library.R           : load/install the required functions<br/>
plot_profiles.R          : plots profiles of one or more specified float(s) for the specified variable(s)<br/>
plot_time_series.R       : plots time-series of one or more specified float(s) for the specified variable(s)<br/>
plot_sections.R          : plots sections of one or more specified float(s) for the specified variable(s)<br/>
plot_trajectories.R      : plots trajectories of one or more specified float(s)<br/>
try_download.R           : attempts to download a file from any of the specified GDACs<br/>


You can open the corresponding R script file to check out the description of input and output parameters for individual functions. 

## COMMENTS, BUGS etc.?
Please feel free to use the GitHub Issues and Pull Requests features to report any problems with this code and to suggest bug fixes or additional features.

## TOOLBOX TUTORIAL
[Video Youtube: R Tutorial with GO-BGC data](https://www.youtube.com/watch?v=w_6pEGNXQQ4&feature=youtu.be)<br/>
[Demo Notebook](https://github.com/mcornec/workshop_R_GO_BGC/blob/Yibin/demo_notebook.md) 

## BGC-ARGO GUIDE
More detailed information about quality control flags, raw and adjusted modes, etc., can be found in
H. C. Bittig et al., Front. Mar. Sci., 2019, https://doi.org/10.3389/fmars.2019.00502

## TOOLBOX IN OTHER LANGUAGES
This toolbox has been translated from MATLAB:<br/>
[MATLAB Toolbox](https://github.com/NOAA-PMEL/OneArgo-Mat)

A similar toolbox in Python:<br/>
[Python toolbox](https://github.com/go-bgc/workshop-python)

[Video tutorials for the toolbox](https://www.go-bgc.org/getting-started-with-go-bgc-data)

## LEGAL DISCLAIMER

This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
