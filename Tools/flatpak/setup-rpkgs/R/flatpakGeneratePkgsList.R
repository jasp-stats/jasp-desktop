# This file needs internet!

# TODO: look at https://github.com/andrie/miniCRAN/issues/50#issuecomment-374624319
# it would be nice to be able to outsource this to miniCRAN which has a method for adding local pkgs to a repository
# the only downside is that it looks like miniCRAN is not actively maintained anymore.

source("R/functions.R")

# you probably want to set a GITHUB_PAT because otherwise you WILL get rate-limited by GitHub.
# Sys.setenv("GITHUB_PAT" = ...)

options(repos = list(repos = c(CRAN = "https://cran.rstudio.com")))

dirs <- setupJaspDirs("flatpak_folder")
# NOTE: if you change the flatpak_dir anywhere you must also change it in the flatpak builder script!

Sys.setenv("RENV_PATHS_CACHE" = dirs["renv-cache"])
Sys.setenv("RENV_PATHS_ROOT"  = dirs["renv-root"])

# all jaspModules
jaspModules <- paste0("jasp-stats/", Filter(function(x) startsWith(x, "jasp"), dir("~/github/jasp-desktop/Modules/")))
names(jaspModules) <- basename(jaspModules)

moduleEnvironments <- getModuleEnvironments(jaspModules)
saveRDS(moduleEnvironments, file = file.path(dirs["module-environments"], "module-environments.rds"))
# moduleEnvironments <- readRDS(file.path(dirs["module-environments"], "module-environments.rds"))
# names(moduleEnvironments[[1]]$records)[1:5]

installRecommendedPackages(dirs)

cleanupBigPackages(dirs)

downloadFakeV8(dirs)

createLocalPackageRepository(dirs)

# updateV8Rpackage(dirs)

# TRUE implies all pkgs only appear once -- not sure what will happen when this is not true anymore
# pkgs <- list.files("toplevelRepository/src/contrib/", pattern = "\\.tar\\.gz$")
# all(table(sapply(strsplit(pkgs, "_", fixed = TRUE), `[`, 1)) == 1)

# downloadV8(dirs)
copyRfiles(dirs)

# debugonce(createTarArchive)
info <- createTarArchive(dirs, verbose = FALSE, compression = "best")

# update Rpackages.json & install build flatpak
writeRpkgsJson("~/github/flatpak/org.jaspstats.JASP/RPackages.json", info)

# IF you have ssh setup this will upload the tar.gz to static-jasp. It's nicer to do this via a terminal because there you see a progress bar
# uploadTarArchive()
