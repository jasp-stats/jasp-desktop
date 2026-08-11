#!/usr/bin/env Rscript 
options(repos='https://cran.r-project.org')

modules = commandArgs(trailingOnly=TRUE)

if(length(modules) == 0) {
    stop("Usage: ./buildModuleBundlesLocally.R /path/to/your/module/source/folder(s)")
}

print("Creating user library and bundles build directory")
workdir     <- './ModuleBundleBuildDir'
userLibrary <- paste0(workdir, '/LibraryR')

print(userLibrary)
dir.create(userLibrary, recursive = TRUE)

print("Installingen required packages")
.libPaths(new=userLibrary)
install.packages('remotes', lib=userLibrary)
library("devtools")

remotes::install_github('jasp-stats/jaspModuleTools', force=TRUE)
library("jaspModuleTools")

print("Make bundles")
options(jaspRemoteCellarRedownload=FALSE)
f <- function(mod) {jaspModuleTools::compile(mod, workdir=file.path(workdir, 'build_dir'), resultdir='../Modules/local', bundleAll=TRUE)}
sapply(modules, f)

print("Done!")
