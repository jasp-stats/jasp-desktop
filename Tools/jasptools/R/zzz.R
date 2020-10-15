.onAttach <- function(libname, pkgname) {
  file <- file.path(libname, pkgname, "jasp-desktop_Location.txt")
  if (file.exists(file)) {
    path <- readLines(file)
    if (isJaspDesktopDir(path)) {
      packageStartupMessage(sprintf("Using jasp-desktop at %s", path))
      .jasptoolsInit(path)
      .checkJaspDependencies(path)
      return(NULL)
    } else {
      packageStartupMessage("Path of jasp-desktop is corrupted! Please use develop(path_to_jasp_desktop).")
    }
  } else {
    packageStartupMessage("Set jasptools path using develop(path_to_jasp_desktop).")
  }
  .jasptoolsInit(NULL)
  invisible()
}

.checkJaspDependencies <- function(jaspDir) {
  #.checkDuplicates() I need to fix this dependency mess across different libs first
  .checkVersions(jaspDir)
}

.checkDuplicates <- function() {
  libs <- c(.getPkgOption("pkgs.dir"), .libPaths())
  deps <- c("jaspTools", "jaspGraphs", "jaspResults")
  libsPerPkg <- setNames(vector("list", length(deps)), deps)
  
  for (lib in libs)
    for (dep in deps)
      if (dep %in% list.dirs(lib, full.names = FALSE, recursive = FALSE))
        libsPerPkg[[dep]] <- c(libsPerPkg[[dep]], lib)
  
  duplicates <- which(unlist(lapply(libsPerPkg, length)) > 1)
  for (duplicate in duplicates)
    packageStartupMessage(paste0("Warning: ", deps[[duplicate]], " exists in multiple libraries (", paste0(libsPerPkg[[duplicate]], collapse=", "), ")"))

}

.checkVersions <- function(jaspDir) {
  deps <- c("jaspTools", "jaspGraphs", "jaspResults")
  dirs <- setNames(c(file.path(jaspDir, "Tools"), file.path(jaspDir, "Engine"), file.path(jaspDir, "R-Interface")), deps)
  
  for (dep in deps) {
    devVersion <- try(silent=TRUE, { utils::packageVersion(dep, lib.loc=dirs[dep]) })
    if (inherits(devVersion, "try-error") || .getInstalledVersion(dep) >= devVersion)
      next
    
    installPath <- file.path(dirs[dep], dep)
    msg <- paste0("*** There is a newer version available of ", dep, " (", devVersion,"). To update run")
    if (dep == "jaspResults")
      packageStartupMessage(paste0(msg, " `remove.packages(\"jaspResults\"); install.packages(\"", installPath, "\", type=\"source\", repos=NULL)`"))
    else
      packageStartupMessage(paste0(msg, " `devtools::install(\"", installPath, "\")`"))
  }
}

.getInstalledVersion <- function(pkg) {
  libs <- c(.getPkgOption("pkgs.dir"), .libPaths())
  utils::packageVersion(pkg, lib.loc = libs)
}
