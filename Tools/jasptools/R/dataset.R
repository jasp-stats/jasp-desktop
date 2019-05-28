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
    allDatasets <- c()
    for (location in locations) {
      
      files <- list.files(location, recursive = TRUE, include.dirs = TRUE)
      datasets <- files[endsWith(files, ".csv")]
      match <- which(basename(datasets) == x)
      if (length(match) > 0) {
        fullPath <- file.path(location, datasets[match[1]])
        if (length(match) > 1) {
          warning("Multiple datasets exists with the same name, choosing '", datasets[match[1]], "'")
        }
        return(data.table::fread(fullPath, header = TRUE, check.names = FALSE, data.table = FALSE))
      }
      allDatasets <- c(allDatasets, basename(datasets))
      
    }
    
    cat("It appears", x, "could not be found. Please supply either a full filepath or the name of one of the following datasets:\n",
        paste0(sort(allDatasets), collapse = '\n'), "\n")
    stop(paste(x, "not found"))
  }
  stop(paste("Cannot handle data of type", mode(x)))
}

.datasetLocations <- function() {
  # datasets in different locations may share the same name, we try to be a little smart here
  # and first look in the most likely location;
  # this means tests.data.dir if we're in a testthat context and otherwise data.dir
  locOrder <- c(.getPkgOption("data.dir"), .getPkgOption("tests.data.dir"))
  if (.insideTestEnvironment()) {
    locOrder <- rev(locOrder)
  }
  return(locOrder)
}
