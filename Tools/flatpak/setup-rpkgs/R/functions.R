assignFunctionInPackage <- function(fun, name, package) {
  # copied from jaspBase
  ns <- getNamespace(package)
  unlockBinding(name, ns)
  assign(name, fun, ns)
  lockBinding(name, ns)
}

packageFromUrl <- function(url) {
  split <- strsplit(url, "/")[[1]]
  if (grepl("github", url, fixed = TRUE)) {
    idx <- which(split == "tarball")[1L]
    return(split[idx - 1])
  } else {
    return(split[length(split) - 1])
  }
}

stopEarlyExit <- function(message = "expected error: early exit", call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c("earlyExit", "condition")
  )
  stop(err)
}

download_override <- function(url, destfile, mode = "wb", quiet = FALSE, headers = NULL) {

  # this allows us to just use the standard renv construction for installing stuff
  tryCatch({
    options("renv.download.override" = NULL)
    on.exit(options("renv.download.override" = download_override))

    # a little bit hacky, but without the correct type the GITHUB_PAT is not used (and we really want that to be used)
    type <- get("type", parent.frame(1))

    file <- renv:::download(url, destfile, type = type, quiet = quiet, headers = headers)
    if (!identical(type, "github")) {
      maybecat(sprintf("skipping url: %s\n", url))
    } else {

      to <- file.path(dirs["local-github"], stripUrl(url))
      maybecat(sprintf("recording github url: %s to %s\n", url, to))
      if (file.exists(to))
        maybecat(sprintf("%s already exists!", to))
      file.copy(from = destfile, to = to, overwrite = TRUE)

    }

    # renv does need this, but we don't
    if (endsWith(url, "PACKAGES.rds") || endsWith(destfile, ".json") || startsWith(basename(destfile), "renv-")) {
      return(file)
    }
    # browser()
    # maybecat(sprintf("recording url: %s, destfile: %s\n", url, destfile))
    maybecat(sprintf("recording package url: %s\n", url))

    # resultsEnv <- getResultsEnv()

    storeUrl(url, destfile)

    return(file)
#
#     # here we optionally download the package to a temporary directory (useful for testing a local CRAN repo)
#     if (resultsEnv$downloadPkgs) {
#     # renv expects this function to return where a package was downloaded
#       path <- renv:::download(url, destfile, type = type, quiet = quiet, headers = headers)
#
#       maybe_copy(path)
#
#       if (resultsEnv$fromLockFile)
#         stopEarlyExit()
#
#       return(path)
#     } else {
#
#       if (resultsEnv$fromLockFile)
#         stopEarlyExit()
#
#       file.copy(from = resultsEnv$fakepkgtar, to = destfile, overwrite = TRUE)
#
#       return(destfile)
#     }

    }, error = function(e) {
      browser()
    }
  )

}

maybecat <- function(x) {
  if (getResultsEnv()$debug) cat(x)
}

maybe_copy <- function(path) {
  # if (dir.exists(resultsEnv$dirForPkgs) && !startsWith(basename(path), "renv-"))
  #   file.copy(path, resultsEnv$dirForPkgs)
}

getResultsEnv <- function() get("resultsEnv", envir = .GlobalEnv)

createResultsEnv <- function(dirForPkgs, downloadPkgs, fromLockFile = FALSE, debug = TRUE) {
  if (exists("resultsEnv", envir = .GlobalEnv))
    rm(list = "resultsEnv", envir = .GlobalEnv)
  assign("resultsEnv", new.env(parent = .GlobalEnv), pos = .GlobalEnv)
  resultsEnv <- getResultsEnv()
  resultsEnv$index        <- 1L
  resultsEnv$dirForPkgs   <- dirForPkgs
  resultsEnv$downloadPkgs <- downloadPkgs
  resultsEnv$fromLockFile <- fromLockFile
  resultsEnv$debug        <- debug

  # if (!downloadPkgs && !fromLockFile) {
  #   fakepkgpath <- file.path(tempdir(), "fakepkg")
  #   mkdir(fakepkgpath)
  #
  #   if (!file.exists(file.path(fakepkgpath, "fakepkg")))
  #     utils::package.skeleton(name = "fakepkg", path = fakepkgpath)
  #
  #   fakepkgtar <- file.path(tempdir(), "fakepkgtar.tar.gz")
  #   if (!file.exists(fakepkgtar)) {
  #     oldwd <- getwd()
  #     on.exit(setwd(oldwd), add = TRUE)
  #     setwd(file.path(fakepkgpath))
  #     tar(fakepkgtar, list.files(), compression = "gzip", tar = "tar")
  #   }
  #   resultsEnv$fakepkgtar <- fakepkgtar
  # }


  resultsEnv
}

storeUrl <- function(url, destfile) {
  resultsEnv <- getResultsEnv()
  # destfile <- file.path(resultsEnv$dirForPkgs, basename(destfile))

  i <- resultsEnv$index

  resultsEnv$url[i]       <- url
  resultsEnv$destfile[i]  <- destfile
  resultsEnv$index        <- resultsEnv$index + 1
}

stripUrl <- function(url) {
  # since repos is hardcoded in renv:::renv_remotes_resolve_github_ref_impl and we modify host,
  # this ooks like the safest approach
  tolower(gsub("/+", "_", strsplit(url, "repos/", fixed = TRUE)[[1L]][2]))
}

mkdir <- function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE)

hasRenvLockFile <- function(modulePkg) {
  return(file.exists(file.path(modulePkg, "renv.lock")))
}

postProcessResults <- function() {

  resultsEnv <- getResultsEnv()
  resultsEnv$packages <- character(length(resultsEnv$url))
  resultsEnv$version  <- character(length(resultsEnv$url))

  idx_cran <- grep("src/contrib/",    resultsEnv$url)
  temp <- strsplit(basename(resultsEnv$url[idx_cran]), "_", fixed = TRUE)

  resultsEnv$packages[idx_cran] <- vapply(temp, `[`, character(1L), 1L)
  resultsEnv$version [idx_cran] <- sub(".tar.gz$", "", vapply(temp, `[`, character(1L), 2L))
  resultsEnv$source  [idx_cran] <- "repository"

  idx_github <- grep("api.github.com/", resultsEnv$url)

  resultsEnv$packages[idx_github] <- gsub(".*/(.+)/tarball/.*", "\\1", resultsEnv$url[idx_github])
  resultsEnv$version [idx_github] <- basename(resultsEnv$url[idx_github]) # actually just the commit
  resultsEnv$source  [idx_github] <- "github"

  for (i in seq_along(resultsEnv$records))
    if (isGitHubRecord(resultsEnv$records[[i]]))
      resultsEnv$records[[i]]$Path <- makePathRelative(resultsEnv$records[[i]]$Path)

  return(resultsEnv)
}

makePathRelative <- function(path, base = getwd()) {
  gsub(pattern = paste0(base, .Platform$file.sep), replacement = "", x = path, fixed = TRUE)
}

getFlatpakJSONFromDESCRIPTION <- function(pathToModule, dirForPkgs = tempdir(), downloadPkgs = FALSE) {

  if (!downloadPkgs)
    warning("with downloadPkgs = FALSE dependencies of DEPENDENCIES won't show up in the results!", immediate. = TRUE)

  # options("renv.download.override" = download_override)
  # on.exit(options("renv.download.override" = NULL))

  # oldinstall.opts <- options("install.opts")
  # on.exit(options(install.opts = oldinstall.opts), add = TRUE)
  # options(install.opts = "--no-byte-compile --no-test-load --fake --no-R --no-libs --no-data --no-help --no-demo --no-exec --no-inst")

  # oldCache <- Sys.getenv("RENV_PATHS_CACHE")
  # tempCache <- tempdir()
  # if (dir.exists(file.path(tempCache, "v5")))
  #   unlink(file.path(tempCache, "v5"), recursive = TRUE)
  # Sys.setenv("RENV_PATHS_CACHE" = tempCache)
  # on.exit(Sys.setenv("RENV_PATHS_CACHE" = oldCache), add = TRUE)

  # old_renv_install <- renv:::renv_install
  # on.exit(assignFunctionInPackage(old_renv_install, "renv_install", "renv"), add = TRUE)
  # assignFunctionInPackage(identity, "renv_install", "renv")
  #
  # library <- file.path(dirForPkgs, "library")
  # mkdir(library)
  #
  # resultsEnv <- createResultsEnv(dirForPkgs, downloadPkgs)
  # records <- renv::install(packages = pathToModule, library = library, rebuild = TRUE, sources = "")
  # resultsEnv$records <- records

  library <- file.path(dirForPkgs, "library")
  mkdir(library)
  resultsEnv <- createResultsEnv(dirForPkgs, downloadPkgs)
  resultsEnv$records <- customRenvInstall(packages = pathToModule, library = library, rebuild = TRUE)

}

customRenvInstall <- function(packages, library = NULL, rebuild = TRUE, customDownload = TRUE) {

  if (customDownload) {
    options("renv.download.override" = download_override)
    on.exit(options("renv.download.override" = NULL))
  }

  old_renv_install <- renv:::renv_install
  on.exit(assignFunctionInPackage(old_renv_install, "renv_install", "renv"), add = TRUE)
  assignFunctionInPackage(identity, "renv_install", "renv")

  renv::install(packages = packages, library = library, rebuild = rebuild)

}

installRecommendedPackages <- function(dirs) {

  .libPaths()
  installed <- installed.packages(.libPaths())
  rec_pkgs <- unname(installed[installed[, "Priority"] %in% "recommended", "Package"])
  customRenvInstall(rec_pkgs, customDownload = FALSE)

  downloadRenv(file.path(dirs["local-cran"], "src", "contrib"))
  downloadRemotes(file.path(dirs["local-cran"], "src", "contrib"))
}

getFlatpakJSONFromLockfile <- function(pathToModule, dirForPkgs = tempdir(), downloadPkgs = FALSE) {

  options("renv.download.override" = download_override)
  on.exit(options("renv.download.override" = NULL))

  lockfile <- jsonlite::fromJSON(file.path(pathToModule, "renv.lock"))
  records <- lockfile$Packages
  records <- Filter(function(x) !x$Package %in% c("jaspTools", "jaspResults"), records)

  nrecords <- length(records)

  resultsEnv <- createResultsEnv(dirForPkgs, downloadPkgs, fromLockFile = TRUE)
  resultsEnv$packages <- character(nrecords)
  resultsEnv$url      <- character(nrecords)
  resultsEnv$destfile <- character(nrecords)
  resultsEnv$records  <- vector("list", nrecords)

  pb <- utils::txtProgressBar(max = nrecords, style = 3)
  on.exit(close(pb), add = TRUE)
  for (i in seq_along(records)) {

    # TODO: consider the same hack as in getFlatpakJSONFromDESCRIPTION to overwrite renv:::renv_install

    tryCatch(
      # rebuild ensures we bypass the cache
      capture.output(renv::install(records[i], rebuild = TRUE, sources = "")),
      earlyExit = function(e) {},
      error = function(e) {
        if (!identical(e[["message"]], "failed to retrieve 'Error: expected error: early exit\n' [expected error: early exit]")) {
          browser(e)
          print(paste("got an error:", e[["message"]]))
        }
      }
    )

    utils::setTxtProgressBar(pb, i)

  }

}

getFlatpakJSONFromModule <- function(pathToModule, dirForPkgs = tempdir(), downloadPkgs = FALSE) {

  mkdir(dirForPkgs)

  # for now we assume no modules have a lockfile
  # if (hasRenvLockFile(pathToModule)) {
  #   getFlatpakJSONFromLockfile(pathToModule, dirForPkgs, downloadPkgs)
  # } else {
  getFlatpakJSONFromDESCRIPTION(pathToModule, dirForPkgs, downloadPkgs)
  # }

  postProcessResults()

}

# repository infrastructure
createFolderStructure <- function(toplevel = file.path(getwd(), "toplevelRepository")) {

  contrib  <- file.path(toplevel, "src", "contrib")
  github   <- file.path(toplevel, "github")
  mkdir(contrib)
  mkdir(github)
  return(c("root" = toplevel, "contrib" = contrib, "github" = github))

}

getPackageFilesBySource <- function(resultsEnv, source, invert = FALSE) {

  idx <- if (invert) resultsEnv$source != source else resultsEnv$source == source
  return(paste0(resultsEnv$packages[idx], "_", resultsEnv$version[idx], ".tar.gz"))

}

createLocalRepository <- function(contrib, sourcePkgsDir) {

  # nonRepositoryPkgsNames <- Reduce(unique, lapply(
  #   moduleEnvironments,
  #   function(resultsEnv) getPackageFilesBySource(resultsEnv, "repository", invert = TRUE)
  # ))

  # repositoryPkgFiles <- setdiff(list.files(sourcePkgsDir, pattern = "\\.tar\\.gz$", recursive = TRUE), nonRepositoryPkgsNames)

  repositoryPkgFiles <- list.files(sourcePkgsDir, pattern = "\\.tar\\.gz$", recursive = TRUE, full.names = TRUE)

  file.copy(repositoryPkgFiles, to = file.path(contrib, basename(repositoryPkgFiles)))
  tools::write_PACKAGES(contrib, type = "source")

}

createLocalGithubRepository <- function(resultsEnv, githubDir, sourcePkgsDir) {

  githubPkgsNames <- getPackageFilesBySource(resultsEnv, "github")

  githubRecords <- Filter(function(x) x[["Source"]] == "GitHub", resultsEnv$records)
  temp <- vapply(githubRecords, function(x) paste0(x$Package, "_", x$RemoteSha, ".tar.gz"), character(1L))
  githubPkgsNames <- githubPkgsNames[match(temp, githubPkgsNames)]

  fmt <- "%s/repos/%s/%s/tarball/%s"
  for (i in seq_along(githubRecords)) {
    record <- githubRecords[[i]]
    newPath <- file.path(githubDir, sprintf(fmt, record$RemoteUsername, record$RemoteRepo, record$RemoteSha, githubPkgsNames[i]))
    mkdir(dirname(newPath))
    file.copy(file.path(sourcePkgsDir, githubPkgsNames[i]), newPath)
  }

}

createLocalPackageRepository <- function(dirs) {

  sourcePkgsDir <- file.path(dirs["renv-root"], "source", "repository")

  paths <- createFolderStructure(dirs["local-cran"])

  createLocalRepository(paths[["contrib"]], sourcePkgsDir)
  # createLocalGithubRepository(resultsEnv, paths[["github"]],  sourcePkgsDir)
  paths
}

setupJaspDirs <- function(root = getwd(), jaspSubdir = "jasp-build", flatpakSubdir = "flatpak-helper", clearAll = FALSE, renvOnly = FALSE) {

  paths <- c(
    # folder structure should be identical to JASP
    "Modules"    = file.path(root, jaspSubdir, "Modules"),
    "renv-cache" = file.path(root, jaspSubdir, "renv-cache"),
    "renv-root"  = file.path(root, jaspSubdir, "renv-root"),

    # this directories are only relevant for building flatpak
    "module-environments" = file.path(root, flatpakSubdir, "module-environments"),
    "other-dependencies"  = file.path(root, flatpakSubdir, "other-dependencies"),
    "r-helpers"           = file.path(root, flatpakSubdir, "r-helpers"),
    "local-cran"          = file.path(root, flatpakSubdir, "local-cran"),
    "local-github"        = file.path(root, flatpakSubdir, "local-github"),
    "root"                = root,
    "jasp-subdir"         = file.path(root, jaspSubdir),
    "flatpak-dir"         = file.path(root, flatpakSubdir)
  )

  if (renvOnly)
    paths <- paths[startsWith(names(paths), "renv-")]

  if (clearAll)
    lapply(paths, unlink, recursive = TRUE)

  lapply(paths, mkdir)

  return(sapply(paths, normalizePath))
}

getModuleEnvironments <- function(jaspModules) {
  moduleEnvironments <- setNames(vector("list", length(jaspModules)), names(jaspModules))

  for (url in jaspModules) {
    nm <- basename(url)
    moduleEnvironments[[nm]] <- getFlatpakJSONFromModule(url, downloadPkgs = TRUE)
  }
  return(moduleEnvironments)
}

installGitHubRecords <- function(githubRecords, tempLib) {

  # pretend the github packages are local packages (which they are)
  # the order of githubRecords follows the order of the obtained URLs so the module of interest is always last
  for (temprecord in githubRecords) {
    record <- list(list(
      Package   = temprecord[["Package"]],
      Version   = temprecord[["Version"]],
      Path      = fixLocalPath(temprecord[["Path"]]), # renv requires an absolute path
      Source    = "Local",
      Cacheable = TRUE
    ))
    names(record) <- temprecord[["Package"]]
    print("installing record:")
    print(record)
    renv::install(record, library = tempLib, project = tempLib[1])
  }

}

fixLocalPath <- function(path) {

  # TODO: this function is unnessecary if we'd store the correct path immediately!
  # the problem is that if you modify flatpakGeneratePkgsList, this might modify the path
  # and this function is a convenient fallback

  # .. I should really delete this
  path <- sub(pattern = "jasp_build", replacement = "jasp-build", x = path, fixed = TRUE)

  cat("wd:\t", getwd(), "\ndir():\t", dir(getwd()), "\npath:\t", path, '\n')
  cat(file.path(getwd(), path), "\n")
  if (file.exists(path))
    return(normalizePath(path))


  print(dir(file.path(getwd(), "jasp-build", "renv-root/source/github/jaspBase/")))
  print(dir(file.path(getwd(), "jasp-build", "renv-root/source/github/")))
  print(dir(file.path(getwd(), "jasp-build", "renv-root/source/")))

  orgpath <- path
  oldpath <- path
  newpath <- sub("[^/]+/", "", oldpath)

  # drop the highest directory until the thingy exists
  while (oldpath != newpath) {
    cat(sprintf("trying path %s\n", file.path(getwd(), newpath)))
    if (file.exists(file.path(getwd(), newpath)))
        return(normalizePath(file.path(getwd(), newpath)))
    oldpath <- newpath
    newpath <- sub("[^/]+/", "", newpath)
  }
  stop(sprintf("Could not make this path work: %s", orgpath))

}

isGitHubRecord <- function(x) {
  x[["Source"]] == "GitHub"
}

installModules <- function(dirs, moduleEnvironments) {

  moduleNames <- names(moduleEnvironments)
  for (i in seq_along(moduleEnvironments)) {

    env <- moduleEnvironments[[i]]
    githubRecords <- Filter(isGitHubRecord, env$records)

    lib <- file.path(dirs["Modules"], moduleNames[i])
    unlink(lib, recursive = TRUE)
    mkdir(lib)

    # we use here the last libPaths. Usually that one only contains the default packages (e.g., when using renv)
    # when this also contains other packages, the installation behavior might differ from flatpak
    libpaths <- .libPaths()
    installGitHubRecords(githubRecords, c(normalizePath(lib), libpaths[length(libpaths)]))

  }

}

downloadFile <- function(url, destdir) {
  mkdir(destdir)
  destfile <- file.path(destdir, basename(url))
  download.file(url = url, destfile = destfile)
  return(destfile)
}

downloadRenv <- function(destdir) {
  # TODO: don't hardcode the renv version? maybe use 1 older than the current release so the url always works?
  downloadFile("https://cran.r-project.org/src/contrib/renv_0.13.2.tar.gz", destdir)
}

downloadRemotes <- function(destdir) {
  downloadFile("https://cran.r-project.org/src/contrib/remotes_2.4.0.tar.gz", destdir)
}

downloadV8 <- function(dirs) {

  # see https://github.com/jeroen/V8/blob/master/configure#L61
  # this might need to be updated from time to time!
  tarfile <- downloadFile("http://jeroen.github.io/V8/v8-8.3.110.13-linux.tar.gz", dirs["other-dependencies"])
  untar(tarfile, exdir = dirs["other-dependencies"]) # otherwise we need to do this on flatpak
  unlink(tarfile)
  # delelte all the ._ files?
  # unlink(list.files(path = dirs["other-dependencies"], pattern = "\\._(.+)", recursive = TRUE, full.names = TRUE))

}

copyRfiles <- function(dirs) {
  file.copy(from = list.files("R", pattern = "*\\.R$", full.names = TRUE), to = dirs["r-helpers"], overwrite = TRUE)
}

createTarArchive <- function(dirs, outputPath = "archives/flatpak_archive.tar.gz", compression = c("fast", "none", "best"),
                             verbose = TRUE, update = FALSE) {

  # TODO: update does not work

  if (update && !identical(compression[1L], "none")) {
    warning("can only update when compression is 'none'! Not updating instead.", immediate. = TRUE)
    update <- FALSE
  }

  if (!is.numeric(compression)) {
    compression <- match.arg(compression)
    compression <- switch(compression,
      "none" = "",
      "best" = "-I 'gzip -9'",
      "fast" = "-I 'gzip -1'"
    )
  } else if (!is.numeric(compression) || (is.numeric(compression) && !(compression >= 1 && compression <= 9))) {
    stop("compression must be character ('best' or 'fast') or numeric (1-9)")
  }

  dirsForArchive <- dirs["flatpak-dir"]
  # dirsForArchive <- c(dirs["flatpak-dir"], file.path(dirs["renv-root"], "source", "github"))

  # files <- file.path("..", basename(getwd()), makePathRelative(dirsForArchive))
  files <- makePathRelative(dirsForArchive, base = dirname(dirs["root"]))

  mkdir(dirname(outputPath))

  creatArchive <- sprintf(
    "tar %s -%s%sf %s %s",
    compression,
    if (verbose) "v" else "",
    if (update)  "u" else "c",
    outputPath,
    paste(files, collapse = " ")
  )
  cat('running: ', creatArchive, '\n')
  system(creatArchive)

  sha256 <- strsplit(system2("sha256sum", outputPath, stdout = TRUE), " ", fixed = TRUE)[[1]][1]
  cat("\nsha256sum\n")
  cat(sha256, '\n')

  rfile <- makePathRelative(file.path(dirs["r-helpers"], "flatpakRegenerateRenvStructure.R"), base = dirs["root"])
  cat(sprintf("command for flatpak\nR --vanilla --file=%s\n", rfile))

  return(c(
    "tar-file" = outputPath,
    "sha256"   = sha256,
    "r-file"   = rfile
  ))
}

uploadTarArchive <- function(archivePath = "archives/flatpak_archive.tar.gz") {

  if (!file.exists(archivePath))
    stop("Archive does not exist")

  archivePath <- normalizePath(archivePath)

  cmd <- sprintf("scp %s jonathonlove@static.jasp-stats.org:static.jasp-stats.org/flatpak_archive.tar.gz", archivePath)
  system(cmd)

}

writeRpkgsJson <- function(path, info, local = FALSE) {

  template <- '{
	"name": "RPackages2",
	"buildsystem": "simple",
	"build-commands": [],
	"modules":
	[
		{
			"name": "Rpackages",
			"buildsystem": "simple",
			"sources": [
				{
					"type": "archive",
					"url": "%s",
					"sha256": "%s"
				}
			],
			"build-commands": [ "R --vanilla --file=%s" ]
		}
	]
}
'

  location <- if (local) {
    file.path("file:/", normalizePath(info["tar-file"]))
  } else {
    "http://static.jasp-stats.org/flatpak_archive.tar.gz"
  }

  new <- sprintf(template, location, info["sha256"], info["r-file"])
  writeLines(new, path)

}

prettyCat <- function(x) {
  name <- deparse(substitute(x))
  if (is.list(x)) {
    cat(name, '\n')
    cat(unlist(lapply(seq_along(x), function(i) paste(c(names(x[i]), x[[i]]), collapse = "\n"))), sep = '\n\n')
  } else if (!is.null(names(x)))
    cat(name, '\n', paste(format(names(x)), unname(x), sep = '\t', collapse = '\n'), '\n', sep = "")
  else if (length(x) == 1L)
    cat(name, '\t', x, '\n', sep = "")
  else
    cat(name, '\n', paste(x, collapse = '\n'), '\n', sep = "")
}

updateV8Rpackage <- function(dirs) {

  pathV8 <- list.files(file.path(dirs["local-cran"], "src", "contrib"), pattern = "^V8_*", full.names = TRUE)
  dirTemp <- file.path(tempdir(), "V8_fix")
  mkdir(dirTemp)
  untar(tarfile = pathV8, exdir = dirTemp)
  dirTempV8 <- file.path(dirTemp, "V8")
  configureLines <- readLines(file.path(dirTempV8, "configure"))
  configureLines[startsWith(configureLines, "PKG_LIBS=\"-lv8")] <- "PKG_LIBS=\"-lv8_monolith\""
  configureLines[startsWith(configureLines, "PKG_CFLAGS=\"-I/usr/include/v8")] <- "PKG_CFLAGS=\"\""
  writeLines(configureLines, file.path(dirTempV8, "configure"))

  newPathV8 <- file.path(dirTemp, basename(pathV8))
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  setwd(dirTemp)
  tar(tarfile = newPathV8, files = "V8", compression = "gzip")

  file.copy(from = newPathV8, to = pathV8, overwrite = TRUE)

}

installJaspStats <- function(pkgs, dirs) {

  paths <- character(length(pkgs))
  for (i in seq_along(pkgs)) {
    pathsFound <- list.files(path = dirs["local-github"], pattern = sprintf("^jasp-stats_%s_tarball_", tolower(pkgs[i])), full.names = TRUE)
    if (length(pathsFound) != 1L)
      stop("There are ", if (length(pathsFound) < 1L) "zero" else "multiple", " ", pkgs[i], "_*.tar.gz present!")
    paths[i] <- pathsFound
  }

  prettyCat(paths)

  for (path in paths) {
    pathtemp <- tempfile(fileext = ".tar.gz")
    file.copy(path, to = pathtemp)
    renv::install(pathtemp)
  }

}

makeTar <- function(pathOriginal, dirTemp) {

  newPathV8 <- file.path(dirTemp, basename(pathOriginal))
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  setwd(dir)
  tar(tarfile = newPathV8, files = "V8", compression = "gzip")

  file.copy(from = newPathV8, to = pathOriginal, overwrite = TRUE)

}

cleanupBigPackages <- function(dirs) {

  oldwd <- getwd()
  on.exit(setwd(oldwd))
  paths <- list.files(dirs["local-github"], pattern = "*_tarball_*", full.names = TRUE)

  mkdir2 <- function(x) {
    if (dir.exists(x)) unlink(x, recursive = TRUE)
    mkdir(x)
  }

  dirTemp <- file.path(tempdir(), "resize-github-pkgs")
  mkdir2(dirTemp)

  rpath <- file.path(Sys.getenv("R_HOME"), "bin", "R")

  for (i in seq_along(paths)) {

    path <- paths[i]
    dirTempPkg <- file.path(dirTemp, basename(path))
    mkdir2(dirTempPkg)
    untar(path, exdir = dirTempPkg)

    newPath <- file.path(dirTempPkg, dir(dirTempPkg))

    unlink(file.path(newPath, c(
      "vignettes",
      "tests"
    )), recursive = TRUE)

    dirTemp2 <- file.path(tempdir(), basename(path))
    mkdir2(dirTemp2)

    setwd(dirTemp2)
    cmd <- sprintf("%s CMD build --no-build-vignettes --no-manual --resave-data %s", rpath, newPath)
    system(cmd)

    file.copy(from = dir(dirTemp2), to = path, overwrite = TRUE)

  }

}
