develop <- function(path, makePersistent = TRUE) {

  # path: ~/Github/jasp-desktop
  # file: ~/R/library/jasptools/

  path <- normalizePath(path)
  if (!isJaspDesktopDir(path))
    stop(paste("incorrect path.\n\nPath should point to the github location of jasp-desktop.",
               "JASP-Common and JASP-Desktop should be subdirectories."))

  if (makePersistent) {
    jasptoolsDir <- path.package("jasptools", quiet = FALSE)
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
  return(all(c("JASP-Common", "JASP-Desktop", "JASP-Engine", "JASP-R-Interface") %in% dirs))
}

.findDirPackages <- function(pathToBuild, needle) {
  dirs <- list.files(pathToBuild)
  locations <- NULL
  if (!identical(dirs, character(0))) {
    for (dirName in dirs) {
      name <- unlist(strsplit(dirName, "[\\W_]", perl = TRUE))
      if (all(needle %in% tolower(name))) {
        location <- file.path(pathToBuild, dirName, "R", "library")
        locations <- c(locations, location)
      }
    }
  }
  return(locations)
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
    pathsToPackages <- NULL

    os <- .getOS()
    if (!is.null(os)) {
      if (os == "osx") {
        basePathPackages <- file.path("..", "Frameworks", "R.framework", "Versions")
        rVersions <- list.files(basePathPackages)
        if (!identical(rVersions, character(0))) {
          rVersions <- suppressWarnings(as.numeric(rVersions))
          r <- sort(rVersions, decreasing = TRUE)[1]
          pathsToPackages <- file.path(basePathPackages, r, "Resources", "library")
        }
      } else if (os == "windows") {
        if (.Machine$sizeof.pointer == 8) { # 64 bits
          pathsToPackages <- .findDirPackages(file.path(".."), c("jasp", "64"))
        } else { # 32 bits
          pathsToPackages <- .findDirPackages(file.path(".."), c("jasp", "32"))
        }

        if (is.null(pathsToPackages)) {
          pathsToPackages <- .findDirPackages(file.path(".."), c("build", "jasp"))
        }
      } else if (os == "linux") {
        message("Identified OS as Linux. Assuming R packages required for JASP were installed manually.")
      }

    }

    pathToPackages <- NULL
    if (!is.null(pathsToPackages)) {
      for (path in pathsToPackages) {
        packages <- list.files(path)
        if (!identical(packages, character(0)) && "base" %in% packages) {
          message("Successfully found the bundled R packages.")
          pathToPackages <- path
          break
        }
      }
    }

    if (is.null(pathToPackages) && (is.null(os) || os != "linux")) {
      message("Unable to find the bundled R packages.
      Required packages will have to be installed manually, or 'pkgs.dir' must be set.")
    }

    # set locations of all required resources (json, analyses, html, packages)
    relativePaths <- list(
      r.dir = file.path("JASP-Engine", "JASP", "R"),
      html.dir = file.path("JASP-Desktop", "html"),
      json.dir = file.path("Resources", "Library"),
      data.dir = file.path("Resources", "Data Sets"),
      tests.dir = file.path("JASP-Tests", "R", "tests", "testthat"),
      tests.data.dir = file.path("JASP-Tests", "R", "tests", "datasets")
    )

    if (!is.null(pathToPackages)) {
      relativePaths[["pkgs.dir"]] <- pathToPackages
    }

    absolutePaths <- lapply(relativePaths, normalizePath)
    pathsToResources <- absolutePaths

    # create the temp (html) directory for the output
    pathToTools <- file.path(tempdir(), "jasptools")
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
}
