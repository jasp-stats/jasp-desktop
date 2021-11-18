# Since everybody has a different setup, it might be that you need to adjust some (minor) things
# when running this on a new PC. The things you may need to adjust can be found by searching for
# the comment `| HERE |`.
#
# Some other remarks:
#
# - you MUST set a GITHUB_PAT, otherwise you'll get an error. Do not commit that to this file because then GitHub will invalidate it.
# - you can call this from cmd via Rscript R/flatpakGeneratePkgsList.R. (in Tools/flatpak/setup-rpkgs)

# TODO:
#
# - look at https://github.com/andrie/miniCRAN/issues/50#issuecomment-374624319
#   it would be nice to be able to outsource this to miniCRAN which has a method for adding local pkgs to a repository
#   the only downside is that it looks like miniCRAN is not actively maintained anymore.
#
# - there is some rudimentary support for caching downloaded CRAN pkgs.
#   this should be extended to GitHub packages as well, but it doesn't work because I delete all old files related to a particular
#   github pkg if renv alreadyd downloaded it. So now we're downloading jaspBase 20 times.

expectedDirs <- c("R", "renv")
if (!all(expectedDirs %in% list.dirs(getwd(), recursive = FALSE, full.names = FALSE)))
  stop("Incorrect working directory! Expected these directories at the top level: ", paste(expectedDirs, collapse = ", "))

# | HERE | you may want to adjust the paths when running this file on a new computer
jaspDir    <- normalizePath(file.path(getwd(), 	"..", "..", ".."))  		# local clone of https://github.com/jasp-stats/jasp-desktop
flatpakDir <- normalizePath(file.path(jaspDir, "..", "org.jaspstats.JASP")) # local clone of https://github.com/flathub/org.jaspstats.JASP

renvProject <- file.path(jaspDir, "Tools", "flatpak", "setup-rpkgs")
if (!identical(renv::project(), renvProject))
  renv::activate(renvProject)

if (!identical(renv::project(), renvProject))
  stop("Failed to set renv project")

source(file.path("R", "functions.R"))
source(file.path("R", "validators.R"))

validateSetup(jaspDir, flatpakDir)

options(repos = list(repos = c(CRAN = "https://cran.rstudio.com")))
# for binary packages, but this does not yet work well
# options(repos = list(repos = c(RSPM = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")))
# options("binaryPkgs" = TRUE)

# NOTE: if you change the flatpak_dir anywhere you must also change it in the flatpak builder script!
dirs <- setupJaspDirs("flatpak_folder")

Sys.setenv("RENV_PATHS_CACHE" = dirs["renv-cache"])
Sys.setenv("RENV_PATHS_ROOT"  = dirs["renv-root"])

# this version uses the default branch of all modules -- always the latest version
# jaspModules <- paste0("jasp-stats/", Filter(function(x) startsWith(x, "jasp"), dir(file.path(jaspDir, "Modules"))))

# this version uses the local checked-out versions -- but modules that are dependencies are still retrieved from github
isJaspModule <- function(path) file.exists(file.path(path, "DESCRIPTION")) && file.exists(file.path(path, "inst", "Description.qml"))
jaspModules <- Filter(isJaspModule, list.dirs(file.path(jaspDir, "Modules"), recursive = FALSE))
names(jaspModules) <- basename(jaspModules)

# | HERE | you can add modules to exclude
jaspModules <- jaspModules[setdiff(names(jaspModules), c("jaspProcessControl"))]

getModuleEnvironments(jaspModules)
# system("beep_finished.sh")

moveMissingTarBalls(dirs)

installRecommendedPackages(dirs)

cleanupBigPackages(dirs)

updateV8Rpackage(dirs)

createLocalPackageRepository(dirs)

# Test if multiple versions are present
# pkgs <- list.files(file.path(dirs["local-cran"], "src", "contrib"), pattern = "\\.tar\\.gz$")
# tb <- table(sapply(strsplit(pkgs, "_", fixed = TRUE), `[`, 1))
# all(tb == 1)
# tb[tb != 1] # these packages appear more than once

downloadV8ifneeded()
copyV8Lib(dirs)
copyRfiles(dirs)

validateFlatpakFolder(dirs)

# debugonce(createTarArchive)
info <- createTarArchive(dirs, jaspDir, verbose = FALSE, compression = "best")

# | HERE | you may wish to flip the local flag to adjust the script so it uses the local archive
writeRpkgsJson(file.path(flatpakDir, "RPackages.json"), info, local = TRUE)

# IF you have ssh setup this will upload the tar.gz to static-jasp. It's nicer to do this via a terminal because there you see a progress bar
uploadTarArchive(info["tar-file"], printOnly = TRUE)
