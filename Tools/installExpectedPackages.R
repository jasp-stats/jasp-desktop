setwd('./flatpak')
source('gather-r-package-info.R')
installRequiredPackages()

# For anybody that wants to install all R packages in a custom directory, try the script below:
#
# setwd('~/github/jasp-desktop/Tools/flatpak')
# .libPaths("~/R/x86_64-pc-linux-gnu-library/3.6_JASP/") # <- change this to the directory where you want the packages
# source('gather-r-package-info.R')
# installRequiredPackages(stopOnError = FALSE)
