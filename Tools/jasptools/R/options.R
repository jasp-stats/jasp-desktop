#' Obtain options to run JASP analyses with.
#'
#' \code{analysisOptions} provides an easy way to create analysis options. You
#' may use the json from the Qt terminal or from the json files found in
#' resources. The former you have to provide yourself, for the latter you only
#' have to specify the name of the analysis.
#'
#'
#' @param source String containing valid json, or the name of a JASP analysis.
#' If you provide json, be sure to use single quotes.
#' @return A list containing options you can supply to \code{jasptools::run}.
#' If \code{source} is an analysis name then all default options have been
#' filled in and booleans set to FALSE. The options that have no default are
#' left empty. If \code{hint} is set to TRUE then hints are set for these empty
#' options; they are placed between \%'s.
#' @examples
#'
#' options <- jasptools::analysisOptions("BinomialTest")
#' options[["variables"]] <- "contBinom"
#'
#' # Above and below are identical (below is taken from the Qt terminal)
#'
#' options <- jasptools::analysisOptions('{
#' "id" : 0,
#' "name" : "BinomialTest",
#' "options" : {
#'   "VovkSellkeMPR" : false,
#'   "confidenceInterval" : false,
#'   "confidenceIntervalInterval" : 0.950,
#'   "descriptivesPlots" : false,
#'   "descriptivesPlotsConfidenceInterval" : 0.950,
#'   "hypothesis" : "notEqualToTestValue",
#'   "plotHeight" : 300,
#'   "plotWidth" : 160,
#'   "testValue" : 0.50,
#'   "variables" : [ "contBinom" ]
#' },
#' "perform" : "run",
#' "revision" : 0,
#' "settings" : {
#'   "ppi" : 192
#' }
#' }')
#'
#' @export analysisOptions
analysisOptions <- function(source) {
  if (! is.character(source) || length(source) > 1) {
    stop("Expecting a character input of length 1 as source,
    either a json string copied from the Qt terminal or an analysis name.")
  }

  options <- NULL
  source <- trimws(source)
  if (grepl("^\\{.*\\}$", source)) {
    analysisName <- stringr::str_match(source, '\\"name\\" : \\"(.*?)\\"')[2L]
    options <- .analysisOptionsFromJSONString(source)
  } else if (grepl("[{}\":]", source)) {
      stop("Your json is invalid, please copy the entire message
           including the outer braces { } that was send to R in the Qt terminal.
           Remember to use single quotes around the message.", call.=FALSE)
  } else {
    analysisName <- source
    options <- .analysisOptionsFromQMLFile(source)
  }
  attr(options, "analysisName") <- analysisName
  return(options)
}

.analysisOptionsFromQMLFile <- function(analysis) {
  file <- .getQMLFile(analysis)
  if (is.null(file))
    stop("Could not find the options file for analysis ", analysis, ".\n",
         "If you're trying to obtain options for an analysis from a module you have to set the module directory with setPkgOption(\"module.dir\", dir/to/module)")
  options <- .readQML(file)
  return(options)
}

.getQMLFile <- function(analysis) {
  if (.isModule()) {
    dir <- .getModuleQmlDir()
    fileName <- tolower(.getModuleQmlFile(analysis))
  } else {
    dir <- .getPkgOption("common.qml.dir")
    fileName <- tolower(paste0(analysis, ".qml"))
  }
  
  pathsToFiles <- list.files(dir, pattern = ".qml$", recursive=TRUE, ignore.case=TRUE)
  fileNames <- tolower(basename(pathsToFiles))
  if (any(fileNames == fileName)) {
    relativePath <- pathsToFiles[which(fileNames == fileName)]
    absolutePath <- file.path(dir, relativePath)
    return(absolutePath)
  }
}

.getModuleQmlDir <- function() {
  qmldir <- file.path(.getPkgOption("module.dir"), "inst", "qml")
  if (!dir.exists(qmldir))
    stop("Could not locate a qml folder in `", .getPkgOption("module.dir"), "/inst/`")
  
  return(qmldir)
}


.getModuleQmlFile <- function(analysis) {
  analysis <- tolower(analysis)
  descr <- .getModuleDescription()
  funcToQml <- list()
  for (i in seq_along(descr$menu)) {
    analysisMeta <- descr$menu[[i]]

    # lookup default qml file, same as in c++, but with "???" replaced for a null check
    if (!("qml" %in% names(analysisMeta)) && "function" %in% names(analysisMeta) && !is.null(analysisMeta[["function"]]))
      analysisMeta[["qml"]] <- paste0(analysisMeta[["function"]], ".qml")

    if (all(c("function", "qml") %in% names(analysisMeta)))
      funcToQml[[tolower(analysisMeta[["function"]])]] <- analysisMeta[["qml"]]
  }
  
  if (length(funcToQml) == 0)
    stop("Could not find any qml or r function definitions in your description.json")
  
  if (!analysis %in% names(funcToQml))
    stop("Could not find the options for ", analysis, ": it is not specified in description.json")
  
  return(funcToQml[[analysis]])
}
