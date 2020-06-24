#Create the R-nl language files

args <- commandArgs(trailingOnly = TRUE)

if (length(args)!=1) {
  stop("Give the root folder of the package or the root of the R-code of JASP")
} else {
  rootfolder<- args[1]
}

Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

try(tools::update_pkg_po(rootfolder),silent=TRUE)
