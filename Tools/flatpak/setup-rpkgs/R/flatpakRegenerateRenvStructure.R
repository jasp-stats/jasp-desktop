# To run this locally, first run flatpakGeneratePkgsList.R and then do:
# next, do:
# setwd("flatpak_folder")


helperfile <- list.files(pattern = "functions\\.R", full.names = TRUE, recursive = TRUE)
source(helperfile)
# source("flatpak-helper/r-helpers/functions.R")

prettyCat(getwd())
prettyCat(dir(getwd()))
prettyCat(.libPaths())

# these directories point to everything in the zip file
dirs <- setupJaspDirs(clearAll = FALSE)
prettyCat(dirs)

# if you're running this locally, we pretend /app is just app so the code can be run
runningLocally <- !dir.exists("/app")
dirApp <- if (runningLocally) "app" else "/app"
dirLib64 <- file.path(dirApp, "lib64")
flatpakDirs <- setupJaspDirs(dirLib64, jaspSubdir = "", clearAll = FALSE, renvOnly = TRUE)

prettyCat(dirApp)
prettyCat(dirLib64)
prettyCat(flatpakDirs)

Sys.setenv("RENV_PATHS_CACHE" = flatpakDirs["renv-cache"])
Sys.setenv("RENV_PATHS_ROOT"  = flatpakDirs["renv-root"])

options(install.opts = "--no-html")
options(renv.cache.linkable = TRUE)
options(renv.config.install.verbose = TRUE)

renvPath <- file.path(dirs["bootstrap"], "renv.tar.gz")
prettyCat(renvPath)
install.packages(renvPath)

file.copy(from = dirs["cellar"], to = dirLib64, recursive = TRUE)
