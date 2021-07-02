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

options("repos" = c("local" = file.path("file:", dirs["local-cran"])))
options(install.opts = "--no-html")
options(renv.cache.linkable = TRUE)
# options(renv.config.install.verbose = TRUE)

# uncomment to test locally (and not pollute renv/library)
# .libPaths(c(tempdir(), .libPaths()))

# remotes is called through loadNamespace somewhere although we no longer need/ use it...
install.packages(c("renv", "remotes"))

# V8 needs to be present after installing as well
file.copy(from = file.path(dirs["other-dependencies"], "v8"), to = dirLib64, recursive = TRUE)

dirV8 <- file.path(dirLib64, "v8")
# This must be an absolute path, since installation is staged
if (runningLocally) dirV8 <- normalizePath(dirV8)

configureVars <- c(
  V8 = sprintf("INCLUDE_DIR=%1$s/include LIB_DIR=%1$s/lib", dirV8)
)
options(configure.vars = configureVars)
prettyCat(configureVars)

# set environment variable for V8
libArch <- system("uname -m", intern = TRUE)
Sys.setenv("LIB_ARCH" = if (identical(libArch, "x86_64")) "x64" else "aarch64")
prettyCat(Sys.getenv("LIB_ARCH"))

# install V8 here so later it only needs to be retrieved from the cache
renv::install("V8")

installJaspStats(c("jaspBase", "jaspGraphs"), dirs)

prettyCat(setNames(lapply(.libPaths(), dir), .libPaths()))

file.copy(from = dirs["local-cran"],                         to = dirLib64, recursive = TRUE)
file.copy(from = dirs["local-github"],                       to = dirLib64, recursive = TRUE)
file.copy(from = file.path(dirs["r-helpers"], "Rprofile.R"), to = dirLib64)
