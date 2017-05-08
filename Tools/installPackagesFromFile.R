file <- file.choose() # or an url
packages <- read.delim(file, header=FALSE, sep=",")
packages <- as.vector(as.matrix(packages))
packages <- packages[packages != "" & is.na(packages) == FALSE]
install.packages(packages, repos="https://cloud.r-project.org", dependencies=TRUE) # set lib= to destination folder