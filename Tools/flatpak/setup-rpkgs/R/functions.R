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

  # TODO:
  # tweak this so that previously downloaded packages are not downloaded again
  # just make an environment with recorded urls + local file locations
  # right now, jaspBase + jaspGraphs are redownloaded for every module...

  # this allows us to just use the standard renv construction for installing stuff
  tryCatch({
    options("renv.download.override" = NULL)
    on.exit(options("renv.download.override" = download_override))

    # a little bit hacky, but without the correct type the GITHUB_PAT is not used (and we really want that to be used)
    type <- get("type", parent.frame(1))

    dirs <- getDirs()

    if (!is.null(type)) {
      if (type == "repository") {

        # check if this pkg has been downloaded before
        pkg <- basename(url)
        if (pkg != "PACKAGES.rds") {
          pkgName <- gsub("_.*", "", pkg)

          localFile0 <- file.path(dirs["local-cran-source"], pkg)
          if (getOption("binaryPkgs", FALSE)) {
            localFile1 <- file.path(dirs["renv-root-binary"], pkgName, pkg)
          } else {
            localFile1 <- file.path(dirs["renv-root-source"], pkgName, pkg)
          }

          if (file.exists(localFile1)) {

            ws <- strrep(" ", 35 - nchar(pkg)) # NetworkComparisonTest is the longest package name I encountered
            maybecat(sprintf("Already downloaded %s%sreusing %s\n", pkg, ws, makePathRelative(localFile1, dirs["jasp-subdir"])))
            if (!file.exists(localFile0))
              file.copy(from = localFile1, to = localFile0)
            return(localFile1)
          }
        }
      } else if (type == "github") {

        if (grepl("/tarball/.+$", url)) {

          pieces <- strsplit(url, "/", TRUE)[[1]]
          pkgName <- pieces[6]
          SHA     <- pieces[8]

          localFile0 <- file.path(dirs["local-github"], stripUrl(url))
          localFile1 <- file.path(dirs["renv-root"], "source", "github", pkgName, paste0(pkgName, "_", SHA, ".tar.gz"))
          if (file.exists(localFile1)) {
            ws <- strrep(" ", 35 - nchar(pkgName)) # NetworkComparisonTest is the longest package name I encountered
            maybecat(sprintf("Already downloaded %s%sreusing %s\n", pkgName, ws, makePathRelative(localFile1, dirs["jasp-subdir"])))
            if (!file.exists(localFile0))
              file.copy(from = localFile1, to = localFile0)
            return(localFile1)
          }
        }
      }
    }

    file <- renv:::download(url, destfile, type = type, quiet = quiet, headers = headers)
    if (!identical(type, "github")) {
      maybecat(sprintf("skipping url: %s\n", url))
    } else {

      to <- file.path(dirs["local-github"], stripUrl(url))
      maybecat(sprintf("recording github url: %s to %s\n", makeGitHubUrlRelative(url), makePathRelative(to)))
      if (file.exists(to)) {
        maybecat(sprintf("%s already exists, deleting older versions!\n", makePathRelative(to)))
        existingFiles <- list.files(path = dirname(to), pattern = paste(basename(to), "*"), full.names = TRUE)
        res <- file.remove(existingFiles)
        if (!all(res))
          stop2("Failed to remove previously downloaded GitHub packages: ", paste(existingFiles[!res], collapse = ", "))
      }
      file.copy(from = destfile, to = to, overwrite = TRUE)

    }

    # renv does need this, but we don't
    if (endsWith(url, "PACKAGES.rds") || endsWith(destfile, ".json") || startsWith(basename(destfile), "renv-")) {
      return(file)
    }
    maybecat(sprintf("recording package url: %s\n", url))

    storeUrl(url, destfile)

    return(file)

    }, error = function(e) {
      if (interactive())
        browser()
      else
        stop2(e)
    }
  )

}

maybecat <- function(x) {
  if (getResultsEnv()$debug) cat(x)
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
  # this looks like the safest approach
  gsub("/+", "_", strsplit(url, "repos/", fixed = TRUE)[[1L]][2])
}

mkdir <- function(x, deleteIfExists = FALSE) {
  if (deleteIfExists) {
    if (dir.exists(x))
      unlink(x, recursive = TRUE)
    dir.create(x, recursive = TRUE)
  } else if (!dir.exists(x)) {
    dir.create(x, recursive = TRUE)
  }
}

hasRenvLockFile <- function(modulePkg) {
  return(file.exists(file.path(modulePkg, "renv.lock")))
}

postProcessResults <- function() {

  resultsEnv <- getResultsEnv()
  resultsEnv$packages <- character(length(resultsEnv$url))
  resultsEnv$version  <- character(length(resultsEnv$url))

  idx_cran <- grep("src/contrib/", resultsEnv$url)
  if (length(resultsEnv$url) > 0L) {
    temp <- strsplit(basename(resultsEnv$url[idx_cran]), "_", fixed = TRUE)

    resultsEnv$packages[idx_cran] <- vapply(temp, `[`, character(1L), 1L)
    resultsEnv$version [idx_cran] <- sub(".tar.gz$", "", vapply(temp, `[`, character(1L), 2L))
    resultsEnv$source  [idx_cran] <- "repository"

    idx_github <- grep("api.github.com/", resultsEnv$url)

    resultsEnv$packages[idx_github] <- gsub(".*/(.+)/tarball/.*", "\\1", resultsEnv$url[idx_github])
    resultsEnv$version [idx_github] <- basename(resultsEnv$url[idx_github]) # actually just the commit
    resultsEnv$source  [idx_github] <- "github"
  }

  for (i in seq_along(resultsEnv$records))
    if (isGitHubRecord(resultsEnv$records[[i]]))
      resultsEnv$records[[i]]$Path <- makePathRelative(resultsEnv$records[[i]]$Path)

  return(resultsEnv)
}

makePathRelative <- function(path, base = getwd(), prepend = TRUE) {
  if (prepend) base <- paste0(base, .Platform$file.sep)
  gsub(pattern = base, replacement = "", x = path, fixed = TRUE)
}

makeGitHubUrlRelative <- function(url) {
  makePathRelative(url, "https://api.github.com/", FALSE)
}

getFlatpakJSONFromDESCRIPTION <- function(pathToModule, dirForPkgs = tempdir(), downloadPkgs = FALSE) {

  if (!downloadPkgs)
    warning("with downloadPkgs = FALSE dependencies of DEPENDENCIES won't show up in the results!", immediate. = TRUE)

  library <- file.path(dirForPkgs, "library")
  mkdir(library)
  resultsEnv <- createResultsEnv(dirForPkgs, downloadPkgs)
  resultsEnv$records <- customRenvInstall(packages = pathToModule, library = library, rebuild = TRUE)

}

guardForNonStandardPackagesInSystemLibrary <- function() {
  # look for packages in the system library and tell renv to rebuild them so it doesn't try to reuse them
  installed <- installed.packages(.libPaths()[-1L])
  idx <- !startsWith(installed[, "License"], "Part of R ")
  badPackagesFoundInSystemLibrary <- installed[idx, "Package"]
  if (length(badPackagesFoundInSystemLibrary) > 0L) {
    cat("Warning: The following packages were found in the system library but they are not shipped with R!\nThis script should still work, but you probably don't want this.\n")
    cat(badPackagesFoundInSystemLibrary, sep = ", ")
    cat("\n")
  }
  options(badPackagesFoundInSystemLibrary = badPackagesFoundInSystemLibrary)
}

customRenvInstall <- function(packages, library = NULL, rebuild = TRUE, customDownload = TRUE) {

  if (customDownload) {
    options("renv.download.override" = download_override)
    on.exit(options("renv.download.override" = NULL))
  }

  old_renv_impl_install <- renv:::renv_install_impl
  on.exit(assignFunctionInPackage(old_renv_impl_install, "renv_install_impl", "renv"), add = TRUE)
  assignFunctionInPackage(identity, "renv_install_impl", "renv")

  rebuild <- c(packages, getOption("badPackagesFoundInSystemLibrary", default = character()))
  renv::install(packages = packages, library = library, rebuild = rebuild)

}

installRecommendedPackages <- function(dirs) {

  installed <- installed.packages(.libPaths())
  recPkgs <- unname(installed[installed[, "Priority"] %in% "recommended", "Package"])

  alreadyDownloadedPkgs <- list.files(dirs["local-cran-source"], pattern = "\\.tar\\.gz$", recursive = TRUE, full.names = FALSE)
  alreadyDownloadedPkgs <- gsub("_(.*)", "", basename(alreadyDownloadedPkgs))

  toInstall <- c(recPkgs,
    # for some reason, "devtools" and "roxygen2" are missing...
    # renv is not a direct dependency and is installed by default
    # jasp-desktop does somewhere loadNamespace("remotes"), which is why we also download it here
    "devtools", "roxygen2", "renv", "remotes"
  )

  toInstall <- setdiff(toInstall, alreadyDownloadedPkgs)

  # these must be installed from CRAN
  options(repos = list(repos = c(CRAN = "https://cran.rstudio.com")))
  if (length(toInstall) > 0L) {
    cat("Downloading recommended packages:", paste(toInstall, collapse = ", "))
    customRenvInstall(toInstall, customDownload = FALSE)
  } else {
    cat("All recommended packages are downloaded.")
  }

}

# getFlatpakJSONFromLockfile <- function(pathToModule, dirForPkgs = tempdir(), downloadPkgs = FALSE) {
#
#   options("renv.download.override" = download_override)
#   on.exit(options("renv.download.override" = NULL))
#
#   lockfile <- jsonlite::fromJSON(file.path(pathToModule, "renv.lock"))
#   records <- lockfile$Packages
#   records <- Filter(function(x) !x$Package %in% c("jaspTools", "jaspResults"), records)
#
#   nrecords <- length(records)
#
#   resultsEnv <- createResultsEnv(dirForPkgs, downloadPkgs, fromLockFile = TRUE)
#   resultsEnv$packages <- character(nrecords)
#   resultsEnv$url      <- character(nrecords)
#   resultsEnv$destfile <- character(nrecords)
#   resultsEnv$records  <- vector("list", nrecords)
#
#   pb <- utils::txtProgressBar(max = nrecords, style = 3)
#   on.exit(close(pb), add = TRUE)
#   for (i in seq_along(records)) {
#
#     # TODO: consider the same hack as in getFlatpakJSONFromDESCRIPTION to overwrite renv:::renv_install
#
#     tryCatch(
#       # rebuild ensures we bypass the cache
#       capture.output(renv::install(records[i], rebuild = TRUE, sources = "")),
#       earlyExit = function(e) {},
#       error = function(e) {
#         if (!identical(e[["message"]], "failed to retrieve 'Error: expected error: early exit\n' [expected error: early exit]")) {
#           browser(e)
#           print(paste("got an error:", e[["message"]]))
#         }
#       }
#     )
#
#     utils::setTxtProgressBar(pb, i)
#
#   }
#
# }

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
  mkdir(contrib)
  contribBinary  <- file.path(toplevel, "bin", "linux", "contrib")
  mkdir(contribBinary)
  return(c("root" = toplevel, "source" = contrib, "binary" = contribBinary))

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

  createLocalRepository(dirs["local-cran-source"], dirs[c("renv-root-source", "renv-root-binary")])

}

copy_of_renv_bootstrap_platform_prefix <- function() {
  # as the name suggests, a copy of renv:::renv_bootstrap_platform_prefix
  version <- paste(R.version$major, R.version$minor, sep = ".")
  prefix <- paste("R", numeric_version(version)[1, 1:2], sep = "-")
  devel <- identical(R.version[["status"]], "Under development (unstable)") ||
    identical(R.version[["nickname"]], "Unsuffered Consequences")
  if (devel)
    prefix <- paste(prefix, R.version[["svn rev"]], sep = "-r")
  components <- c(prefix, R.version$platform)
  # prefix <- renv_bootstrap_platform_prefix_impl()
  prefix <- NA
  if (!is.na(prefix) && nzchar(prefix))
    components <- c(prefix, components)
  paste(components, collapse = "/")
}

setupJaspDirs <- function(root = getwd(), jaspSubdir = "jasp-build", flatpakSubdir = "flatpak-helper", clearAll = FALSE, renvOnly = FALSE) {

  paths <- c(
    # folder structure should be identical to JASP
    "Modules"          = file.path(root, jaspSubdir, "Modules"),
    "renv-cache"       = file.path(root, jaspSubdir, "renv-cache"),
    "renv-root"        = file.path(root, jaspSubdir, "renv-root"),
    "renv-root-source" = file.path(root, jaspSubdir, "renv-root", "source", "repository"),
    "renv-root-binary" = file.path(root, jaspSubdir, "renv-root", "binary", copy_of_renv_bootstrap_platform_prefix(), "repository"),

    # these directories are only relevant for building flatpak
    "module-environments" = file.path(root, flatpakSubdir, "module-environments"),
    "other-dependencies"  = file.path(root, flatpakSubdir, "other-dependencies"),
    "r-helpers"           = file.path(root, flatpakSubdir, "r-helpers"),
    "local-cran"          = file.path(root, flatpakSubdir, "local-cran"),
    "local-cran-source"   = file.path(root, flatpakSubdir, "local-cran", "src", "contrib"),
    "local-cran-binary"   = file.path(root, flatpakSubdir, "local-cran", "bin", "linux", "contrib"),
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
  # moduleEnvironments contains all meta data for the packages (versions, url, etc.)
  moduleEnvironments <- setNames(vector("list", length(jaspModules)), names(jaspModules))

  for (url in jaspModules) {
    nm <- basename(url)
    moduleEnvironments[[nm]] <- getFlatpakJSONFromModule(url, downloadPkgs = TRUE)
  }
  return(invisible(moduleEnvironments))
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
  stop2(sprintf("Could not make this path work: %s", orgpath))

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

copyRfiles <- function(dirs) {
  rfiles <- list.files("R", pattern = "*\\.R$", full.names = TRUE)
  successes <- file.copy(from = rfiles, to = dirs["r-helpers"], overwrite = TRUE)
  if (!all(successes))
    stop2("failed to copy these R files: ", paste(rfiles[!successes], collapse = ", "), " to ", dirs["r-helpers"])

}

createTarArchive <- function(dirs, jaspDir, outputPath = "archives/flatpak_archive_%s.tar.gz", compression = c("fast", "none", "best"),
                             verbose = TRUE, update = FALSE) {

  # TODO: update does not work

  if (grepl("%s", outputPath)) {
    # get the tag of the current jasp-desktop clone or the commit hash as a fallback
    commitHash <- system(sprintf("cd %s && git describe --exact-match --tags 2> /dev/null || git rev-parse HEAD", jaspDir), intern = TRUE)
    outputPath <- sprintf(outputPath, commitHash)
  }

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
    stop2("compression must be character ('best' or 'fast') or numeric (1-9)")
  }

  dirsForArchive <- dirs["flatpak-dir"]
  # dirsForArchive <- c(dirs["flatpak-dir"], file.path(dirs["renv-root"], "source", "github"))

  # files <- file.path("..", basename(getwd()), makePathRelative(dirsForArchive))
  files <- makePathRelative(dirsForArchive, base = dirname(dirs["root"]))

  mkdir(dirname(outputPath))

  creatArchive <- sprintf(
    "tar  --mode=a+rw %s -%s%sf %s %s",
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

uploadTarArchive <- function(archivePath = "archives/flatpak_archive.tar.gz", verbose = TRUE, printOnly = TRUE) {

  if (!file.exists(archivePath))
    stop2("Archive does not exist")

  archivePath <- normalizePath(archivePath)
  archiveName <- basename(archivePath)

  cmd <- sprintf("scp %s jonathonlove@static.jasp-stats.org:static.jasp-stats.org/%s", archivePath, archiveName)
  if (printOnly) cat(cmd) else system(cmd)

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
    paste0("http://static.jasp-stats.org/", basename(info["tar-file"]))
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

installJaspStats <- function(pkgs, dirs) {

  paths <- character(length(pkgs))
  for (i in seq_along(pkgs)) {
    pathsFound <- list.files(path = dirs["local-github"], pattern = sprintf("^jasp-stats_%s_tarball_", pkgs[i]), full.names = TRUE)
    if (length(pathsFound) != 1L)
      stop2("There are ", if (length(pathsFound) < 1L) "zero" else "multiple", " ", pkgs[i], "_*.tar.gz present!")
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

  # So the GitHub downloads contain EVERYTHING in a repo. That's much more than we need.
  # Luckily, almost every package has an .Rbuildignore. So when we unpack the github tarball
  # and ask R to build a source package then anything listed in .Rbuildignore is excluded
  # from the tarball. This provides a very effective way for deleting useless files while not
  # having to make any guesses about what the package authors intended.
  # In addition, this function deletes the folders "tests" and "vignettes".

  oldwd <- getwd()
  on.exit(setwd(oldwd))
  paths <- list.files(dirs["local-github"], pattern = "*_tarball_*", full.names = TRUE)

  dirTemp <- file.path(tempdir(), "resize-github-pkgs")
  mkdir(dirTemp, deleteIfExists = TRUE)

  rpath <- file.path(Sys.getenv("R_HOME"), "bin", "R")

  for (i in seq_along(paths)) {

    path <- paths[i]
    dirTempPkg <- file.path(dirTemp, basename(path))
    mkdir(dirTempPkg, deleteIfExists = TRUE)
    untar(path, exdir = dirTempPkg)

    newPath <- file.path(dirTempPkg, dir(dirTempPkg))

    unlink(file.path(newPath, c(
      "vignettes",
      "tests"
    )), recursive = TRUE)

    dirTemp2 <- file.path(tempdir(), basename(path))
    mkdir(dirTemp2, deleteIfExists = TRUE)

    if (startsWith(basename(path), "dustinfife_flexplot")) {
      # flexplot removes jasp related files in the .Rbuildignore...
      .RbuildignorePath <- file.path(newPath, ".Rbuildignore")
      if (file.exists(.RbuildignorePath)) {
        .Rbuildignore <- readLines(.RbuildignorePath)
        .Rbuildignore <- .Rbuildignore[!(grepl("jasp", .Rbuildignore) & endsWith(.Rbuildignore, ".R"))]
        writeLines(.Rbuildignore, .RbuildignorePath)
      }

    }

    setwd(dirTemp2)
    cmd <- sprintf("%s CMD build --no-build-vignettes --no-manual --resave-data %s", rpath, newPath)
    system(cmd)

    file.copy(from = dir(dirTemp2), to = path, overwrite = TRUE)

  }

}

copyV8Lib <- function(dirs, source = "other_deps/v8") {
  if (!file.copy(source, to = dirs["other-dependencies"], recursive = TRUE))
    stop2("Failed to copy from ", source, " to ", dirs["other-dependencies"])
}

updateV8Rpackage <- function(dirs) {

  pathV8 <- list.files(file.path(dirs["renv-root-source"], "V8"), pattern = "^V8_*", full.names = TRUE)
  if (length(pathV8) == 0L) {
    warning("No V8 package found. This is fine if you adjusted some things and are not building everything.", domain = NA)
    return()
  }

  fixV8Package <- function(pathV8) {
    dirTemp <- file.path(tempdir(), "V8_fix")
    mkdir(dirTemp, deleteIfExists = TRUE)
    untar(tarfile = pathV8, exdir = dirTemp)
    dirTempV8 <- file.path(dirTemp, "V8")
    configureLines <- readLines(file.path(dirTempV8, "configure"))
    configureLines[startsWith(configureLines, "PKG_LIBS=\"-lv8")] <- "PKG_LIBS=\"-lv8_monolith_$LIB_ARCH\""
    configureLines[startsWith(configureLines, "PKG_CFLAGS=\"-I/usr/include/v8")] <- "PKG_CFLAGS=\"\""
    writeLines(configureLines, file.path(dirTempV8, "configure"))

    newPathV8 <- file.path(dirTemp, basename(pathV8))
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(dirTemp)
    tar(tarfile = newPathV8, files = "V8", compression = "gzip")

    result <- file.copy(from = newPathV8, to = pathV8, overwrite = TRUE)
    if (!result)
      stop2("failed to update the V8 package for path: ", pathV8)

  }

  fixV8Package(pathV8)

  # also do this for the cran repo, just in case functions are run out of order
  pathV8cran <- list.files(dirs["local-cran-source"], pattern = "^V8_*", full.names = TRUE)
  if (length(pathV8cran) > 0L)
    fixV8Package(pathV8cran)

}

moveMissingTarBalls <- function(dirs) {
  # sometimes renv realizes that it does not need to redownload a github package because it already has the source in
  # flatpak_folder/jasp-build/renv-root/source/github.
  # this function locates any missing tarballs and copies them to flatpak_folder/flatpak-helper/local-github/

  allFiles <- list.files(dirs["local-github"])

  r <- "^(.*)_contents_DESCRIPTION\\?ref=(.+)$"
  matches <- regmatches(allFiles, regexec(r, allFiles))
  matches <- matches[lengths(matches) > 0L]

  SHAs <- unlist(lapply(matches, `[[`, 3L), use.names = FALSE)
  names(SHAs) <- unlist(lapply(matches, `[[`, 2L), use.names = FALSE)
  tarballs <- file.path(dirs["local-github"], paste0(names(SHAs), "_tarball_", SHAs))
  missing <- which(!file.exists(tarballs))

  if (length(missing) > 0L) {
    repoNames <- vapply(strsplit(names(SHAs[missing]), "_", fixed = TRUE), `[[`, character(1L), 2L)
    backupTarballNames <- paste0(repoNames, "_", SHAs[missing], ".tar.gz")
    backupTarballs <- file.path(dirs["renv-root"], "source", "github", repoNames, backupTarballNames)

    found <- file.exists(backupTarballs)
    file.copy(from = backupTarballs[found], to = tarballs[missing][found])

    cat("Moved these tarballs:\n", paste(basename(tarballs[missing][found]), collapse = ", "), "\n")

    if (any(!found)) {
      warning("Did not find these tarballs: ",     paste(backupTarballs[!found],    collapse = ","),
              " check if these tarballs are ok: ", paste(tarballs[missing][!found], collapse = ","), domain = NA)
    }
  } else {
    cat("No missing tarballs.\n")
  }
}

downloadV8ifneeded <- function(destination = "other_deps") {

  if (!dir.exists(file.path(destination, "v8"))) {
    # ensure parent folder exists
    mkdir(destination)
    downloadFile(url = "http://static.jasp-stats.org/v8.tar.gz", destination)

    oldwd <- getwd()
    setwd(destination)
    on.exit(setwd(oldwd))
    untar(tarfile = normalizePath("v8.tar.gz"))
  }

}

getDirs <- function() {
  get("dirs", envir = .GlobalEnv)
}

stop2 <- function(..., call. = TRUE, domain = NA) {
  stop(cli::col_red(cli::style_bold(paste0(..., collapse = ""))), call. = call., domain = domain)
}

options("error" = function() {
  traceback(4, max.lines = 2L)
  if (!interactive()) {
    quit(status = 1)
  }
})
