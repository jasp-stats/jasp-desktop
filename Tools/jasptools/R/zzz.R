.onAttach <- function(libname, pkgname) {
  packageStartupMessage(sprintf("jasptools version: %s", utils::packageVersion("jasptools")))

  file <- file.path(libname, pkgname, "jasp-desktop_Location.txt")
  if (file.exists(file)) {
    path <- readLines(file)
    if (isJaspDesktopDir(path)) {
      packageStartupMessage(sprintf("Using jasp-desktop at %s", path))
      .jasptoolsInit(path)
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
