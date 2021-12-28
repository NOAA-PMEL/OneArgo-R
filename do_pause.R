
# do_pause  
#
#This function is part of the
# GO-BGC workshop R tutorial and R toolbox for accessing BGC Argo float data.
#
# USAGE:
  #   do_pause()
#
# DESCRIPTION:
  #   It asks the user to hit ENTER if Settings.use_pause is 1.
#   Otherwise, flow control returns the caller.
#
#
# CITATION:
# BGC-Argo-R: A R toolbox for accessing and visualizing
# Biogeochemical Argo data,
#
#  AUTHORS: 
# M. Cornec (LOV), Y. Huang (NOAA-PMEL), Q. Jutard (OSU ECCE TERRA), 
# R. Sauzede (IMEV) and C. Schmechtig (OSU ECCE TERRA),
#
# Adapted from the Matlab toolbox BGC-Argo-Mat:  https://doi.org/10.5281/zenodo.4971318
# (H. Frenzel, J. Sharp, A. Fassbender (NOAA-PMEL),
# J. Plant, T. Maurer, Y. Takeshita (MBARI), D. Nicholson (WHOI),
# and A. Gray (UW))

# Update 24 June 2021


  do_pause <-function() {
    if(Setting$use_pause==1) {
    cat ("Please hit ENTER to continue")
    line <- readline()
    }
}








