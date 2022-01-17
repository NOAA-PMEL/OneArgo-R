
# Load/Install required libraries

if (!require("gsw")) { install.packages("gsw"); library(gsw) } # calculate sea water paramters 
if (!require("R.utils")) { install.packages("R.utils"); library(R.utils) } # gunzip S index file
if (!require("lubridate")) { install.packages("lubridate"); library(lubridate) } # convert date from Sprof file to date object
if (!require("Matrix")) { install.packages("Matrix"); library(Matrix) } # create reduced-size matrix to deal with the Synthetic profile Index
if (!require("ncdf4")) { install.packages("ncdf4"); library(ncdf4) } # deal with netcdf files
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse) # convert thd data format to data frame 
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2) # convert thd data format to data frame 
if (!require("metR")) { install.packages("metR"); library(metR) }
if (!require("gsw")) { install.packages("gsw"); library(gsw) }
if (!require("R.utils")) { install.packages("R.utils"); library(R.utils) } # to gunzip S index file
