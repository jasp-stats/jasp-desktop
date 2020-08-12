# first the externally accessible options
.pkgOptions <- list2env(list(
  common.r.dir = file.path("..", "JASP-Engine", "jaspBase", "R"),
  html.dir = file.path("..", "JASP-Desktop", "html"),
  common.qml.dir = file.path("..", "Resources"),
  module.dir = "",
  data.dir = file.path("..", "Resources", "Data Sets"),
  pkgs.dir = "",
  tests.dir = file.path("..", "JASP-Tests", "R", "tests", "testthat"),
  tests.figs.dir = file.path("..", "JASP-Tests", "R", "tests", "figs"),
  tests.data.dir = file.path("..", "JASP-Tests", "R", "tests", "datasets"),
  .ppi = 96
))



#' View the configuration of jasptools.
#'
#' There are a number of package options you can adjust. These are printed when
#' you call \code{viewPkgOptions}. Most options are paths to resources, which
#' should be configured automatically. If the configuration is correct (you
#' will be notified if it is not) then the only option you should ever change
#' is .ppi. Setting .ppi to a higher value (through
#' \code{jaspTools::setPkgOption}) results in higher resolution images.
#'
#'
#' @return A print of the configurable options.
#' @export viewPkgOptions
viewPkgOptions <- function() {
  for (i in 1:length(names(.pkgOptions))) {
    name <- names(.pkgOptions)[i]
    if (i == 1) {
      value <- .getPkgOption(name, run = FALSE)
    } else { # one warning is enough.
      value <- suppressWarnings(.getPkgOption(name, run = FALSE))
    }
    cat(name, "=", value, "\n")
  }
}



#' Change the value of an option in jasptools.
#'
#' Sets a package option to a new value (to see what is available use
#' \code{jaspTools::viewPkgOptions}). Value changes are automatically
#' incorporated when any jasptools function is called.
#'
#'
#' @param name String name of the option.
#' @param value Value the option should be set to.
#' @examples
#'
#' jaspTools::setPkgOption(".ppi", 196)
#'
#' @export setPkgOption
setPkgOption <- function(name, value) {
  assign(name, value, envir = .pkgOptions)
}

.getPkgOption <- function(name, run = TRUE) {
  if (.jasptoolsReady() == FALSE) {
    if (run) {
      stop("jasptools is not configured correctly. Did you run `jaspTools::develop(\"path_to_jasp_desktop\")`?
            If you did, then please manually ensure the paths in viewPkgOptions() are correct.
            (If the paths are relative, you only need to set your working directory to path_to_jasp_desktop/Tools)")
    } else {
      warning("jasptools is not configured correctly. It will not find the needed resources.
              Please run `jaspTools::develop(\"path_to_jasp_desktop\")` or set your working directory to path_to_jasp_desktop/Tools.")
    }
  }
  return(get(name, envir = .pkgOptions))
}

# ... and the internally accessible options
.setInternal <- function(name, value) {
  .internal <- get(".internal", envir = as.environment("package:jasptools"))
  .internal[[name]] <- value
}

.getInternal <- function(name) {
  .internal <- get(".internal", envir = as.environment("package:jasptools"))
  if (! name %in% names(.internal))
    stop(paste("Could not locate internal variable", name))
  return(.internal[[name]])
}

.resetRunTimeInternals <- function() {
  .setInternal("state", NULL)
  .setInternal("dataset", NULL)
  .setInternal("s3Methods", NULL)
}
