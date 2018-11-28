.loadCorrectDataset <- function(x) {
  if (is.matrix(x) || is.data.frame(x)) {
    return(x)
  } else if (is.character(x)) {
    if (! endsWith(x, ".csv")) {
      x <- paste0(x, ".csv")
    }

    # check if it's a path to a file
    if (file.exists(x)) {
      return(utils::read.csv(x, header = TRUE, check.names = FALSE))
    }

    # check if it's a name of a JASP dataset
    locations <- .datasetLocations()
    files <- c()
    for (location in locations) {
      datasets <- list.files(location)
      datasets <- datasets[endsWith(datasets, ".csv")]
      if (x %in% datasets) {
        fullPath <- file.path(location, x)
        return(utils::read.csv(fullPath, header = TRUE, check.names = FALSE))
      }
      files <- c(files, datasets)
    }
    cat("It appears", x, "could not be found. Please supply either a full filepath or the name of one of the following datasets:\n",
        paste(files, collapse = '\n'), "\n")
    stop(paste(x, "not found"))
  }
  stop(paste("Cannot handle data of type", mode(x)))
}

.datasetLocations <- function() {
  # datasets in different locations may share the same name, we try to be a little smart here
  # and first look in the most likely location;
  # this means tests.data.dir if we're in a testthat context and otherwise data.dir
  locOrder <- c(.getPkgOption("data.dir"), .getPkgOption("tests.data.dir"))
  testthat <- vapply(sys.frames(), function(frame) methods::getPackageName(frame) == "testthat", logical(1))
  if (any(testthat)) {
    locOrder <- rev(locOrder)
  }
  return(locOrder)
}
