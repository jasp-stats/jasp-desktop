develop <- function(path, makePersistent = TRUE) {
  # path: ~/Github/jasp-desktop
  # file: ~/R/library/jasptools/
  
  path <- normalizePath(path)
  if (!isJaspDesktopDir(path))
    stop(paste("incorrect path.\n\nPath should point to the github location of jasp-desktop.",
               "Common and Desktop should be subdirectories."))
  
  if (makePersistent) {
    jasptoolsDir <- path.package("jaspTools", quiet = FALSE)
    file <- file.path(jasptoolsDir, "jasp-desktop_Location.txt")
    if (file.exists(file))
      message(sprintf("Overwriting existing path at %s.", file))
    else message(sprintf("Creating %s", file))
    
    fileConn <- file(file)
    writeLines(path, fileConn)
    close(fileConn)
  }
  .jasptoolsInit(path)
  
  return(invisible(TRUE))
}

undevelop <- function() {
  message("Not working yet!")
}

isJaspDesktopDir <- function(path) {

  # check if path is the location of github/jasp-desktop
  dirs <- dir(path, pattern = "JASP-*")
  return(all(c("Common", "Desktop", "Engine", "R-Interface") %in% dirs))
}

.getDirOSXFramework <- function(pathJaspBuildEnvir) {
  basePathPkgs <- file.path(pathJaspBuildEnvir, "Frameworks", "R.framework", "Versions")
  rVersions <- list.files(basePathPkgs)
  if (identical(rVersions, character(0)))
    return(NULL)

  rVersions <- suppressWarnings(as.numeric(rVersions))
  r <- sort(rVersions, decreasing = TRUE)[1]
  potentialPkgDir <- file.path(basePathPkgs, r, "Resources", "library")
  
  if (.dirHasBundledPackages(potentialPkgDir, hasPkg="Rcpp"))
    return(potentialPkgDir)
    
  return(NULL)
}

.getDirPackagesInBuildEnvir <- function(pathJaspBuildEnvir, os) {
  potentialPkgDirs <- .getPotentialPackageDirs(pathJaspBuildEnvir)
  
  if (os == "osx")
    hasPkg <- "jaspGraphs"
  else
    hasPkg <- "Rcpp"
    
  for (potentialPkgDir in potentialPkgDirs) {
    if (.dirHasBundledPackages(potentialPkgDir, hasPkg))
      return(potentialPkgDir)
  }
  
  return(NULL)
}

.dirHasBundledPackages <- function(dir, hasPkg) {
  if (!dir.exists(dir))
    return(FALSE)
    
  pkgs <- list.files(dir)
  if (!identical(pkgs, character(0)) && hasPkg %in% pkgs) {
    return(TRUE)
  }
  
  return(FALSE)
}

.getPotentialPackageDirs <- function(pathJaspBuildEnvir) {
  dirs <- list.dirs(pathJaspBuildEnvir, recursive=FALSE)
  if (identical(dirs, character(0)))
    return(NULL)
  
  # order by date; we want to use the library that was most recently updated if we have a choice
  dirs <- dirs[order(file.mtime(dirs), decreasing=TRUE)]
  
  # put folders that contain references to 32 bit last on 64 bit machines
  if (.Machine$sizeof.pointer == 8) { # it's a 64 bit machine
    dir32 <- grepl("(32)|(x86)", dirs)
    if (any(dir32)) {
      indices <- which(dir32)
      for (index in indices)
        dirs <- c(dirs[-index], dirs[index])
    }
  }
  
  potentialPackageDirs <- file.path(dirs, "R", "library")
  
  return(potentialPackageDirs)
}

.getOS <- function() {
  os <- NULL
  if (!is.null(Sys.info())) {
    os <- Sys.info()["sysname"]
    if (os == "Darwin")
      os <- "osx"
  } else {
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}

.jasptoolsInit <- function(jaspPath) {

  if (is.null(jaspPath)) {
    .internal <- list2env(list(
      initPaths = NULL,
      envir     = .GlobalEnv,
      dataset   = NULL,
      state     = NULL
    ))
  } else {
    # temporarily change wd
    oldwd <- getwd()
    setwd(jaspPath)
    on.exit(setwd(oldwd), add = TRUE)

    # get the path to JASP R packages so users do not need to install any additional packages
    # retrieving os bit: http://conjugateprior.org/2015/06/identifying-the-os-from-r/
    pathToPackages <- NULL

    os <- .getOS()
    if (!is.null(os)) {
      if (os == "linux") {
        message("Identified OS as Linux. Assuming R packages required for JASP were installed manually.")
      } else {
        pathToPackages <- .getDirPackagesInBuildEnvir(file.path(".."), os)
        if (os == "osx") {
          pathToFramework <- .getDirOSXFramework(file.path(".."))
          pathToPackages <- c(pathToFramework, pathToPackages)
        }
        
        if (!is.null(pathToPackages))
          message("Successfully found the bundled R packages.")
      }
    }

    if (is.null(pathToPackages) && (is.null(os) || os != "linux"))
      message("Unable to find the bundled R packages.
      Required packages will have to be installed manually, or 'pkgs.dir' must be set.")
    
    # set locations of all required resources (json, analyses, html, packages)
    relativePaths <- list(
      common.r.dir = file.path("Engine", "jaspBase", "R"),
      html.dir = file.path("Desktop", "html"),
      common.qml.dir = file.path("Resources"),
      data.dir = file.path("Resources", "Data Sets"),
      tests.dir = file.path("Tests", "R", "tests", "testthat"),
      tests.figs.dir = file.path("Tests", "R", "tests", "figs"),
      tests.data.dir = file.path("Tests", "R", "tests", "datasets")
    )

    if (!is.null(pathToPackages)) {
      relativePaths[["pkgs.dir"]] <- pathToPackages
    }

    absolutePaths <- lapply(relativePaths, normalizePath)
    pathsToResources <- absolutePaths

    # create the temp (html) directory for the output
    pathToTools <- file.path(tempdir(), "jaspTools")
    if (!dir.exists(pathToTools)) {
      dir.create(file.path(pathToTools, "html", "plots"), recursive = TRUE)
      dir.create(file.path(pathToTools, "state"))
      message(paste("Note: temp output files may be found at", pathToTools))
    }

    initPaths <- FALSE
    if (!is.null(pathsToResources))
      initPaths <- pathsToResources

    .internal <- list2env(list(
      initPaths = initPaths,
      envir     = .GlobalEnv,
      dataset   = NULL,
      state     = NULL
    ))

  }

  # create globals for setup / JASP to find
  # env <- as.environment("package:jasptools")
  env <- try(as.environment("package:jasptools"), silent = TRUE)
  if (inherits(env, "try-error"))
    stop("please load jasptools first!")
  isLocked <- environmentIsLocked(env)
  if (isLocked) {
    try(silent = TRUE, {
      unlockBinding(".internal",     env)
      unlockBinding("perform",       env)
      unlockBinding(".ppi",          env)
      unlockBinding(".baseCitation", env)
      unlockBinding(".masks",        env)
    })
  }

  assign(".internal",     .internal,            envir = env)
  assign("perform",       NULL,                 envir = env)
  assign(".ppi",          NULL,                 envir = env)
  assign(".baseCitation", "x",                  envir = env)
  assign(".masks",        c("perform", ".ppi"), envir = env)

  if (isLocked) {
    try(silent = TRUE, {
      lockBinding(".internal",     env)
      lockBinding("perform",       env)
      lockBinding(".ppi",          env)
      lockBinding(".baseCitation", env)
      lockBinding(".masks",        env)
    })
  }
  
  # this is not used in combination with getAnywhere() in the code so it cannot be found
  assign(".automaticColumnEncDecoding", FALSE, envir = .GlobalEnv)
}
