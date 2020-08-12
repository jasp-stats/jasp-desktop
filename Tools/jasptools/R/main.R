#' View the tables and plots in a results object.
#'
#' \code{view} allows you to view output independently of Qt. It uses the same
#' javascript/css/html and should generate identical output. This function may
#' be called directly, but it is more convenient to use \code{jaspTools::run}.
#'
#'
#' @param results A named R list returned from a JASP analysis, or a json
#' results string copied from the Qt terminal.
#' @return A html page is generated and placed in a temp folder.
#' @examples
#'
#' options <- jaspTools::analysisOptions("BinomialTest")
#' results <- jaspTools::run("BinomialTest", "debug", options, view=FALSE)
#' jaspTools::view(results)
#'
#' # Above and below are identical (below is taken from the Qt terminal)
#'
#' jaspTools::view('{
#'    "id" : 6,
#'    "name" : "BinomialTest",
#'    "results" : {
#'       ".meta" : [
#'          {
#'             "name" : "binomial",
#'             "type" : "table"
#'          },
#'          {
#'             "meta" : [],
#'             "name" : "descriptives",
#'             "type" : "object"
#'          }
#'       ],
#'       "binomial" : {
#'          "citation" : [ "JASP Team (2017). JASP (Version 0.8.2) [Computer software]." ],
#'          "data" : [
#'             {
#'                "case" : "",
#'                "counts" : ".",
#'                "level" : ".",
#'                "lowerCI" : ".",
#'                "p" : ".",
#'                "proportion" : ".",
#'                "total" : ".",
#'                "upperCI" : "."
#'             }
#'          ],
#'          "footnotes" : [
#'             {
#'                "symbol" : "<em>Note.</em>",
#'                "text" : "Proportions tested against value: 0.5."
#'             }
#'          ],
#'          "schema" : {
#'             "fields" : [
#'                {
#'                   "combine" : true,
#'                   "name" : "case",
#'                   "title" : "",
#'                   "type" : "string"
#'                },
#'                {
#'                   "name" : "level",
#'                   "title" : "Level",
#'                   "type" : "string"
#'                },
#'                {
#'                   "name" : "counts",
#'                   "title" : "Counts",
#'                   "type" : "integer"
#'                },
#'                {
#'                   "name" : "total",
#'                   "title" : "Total",
#'                   "type" : "integer"
#'                },
#'                {
#'                   "format" : "sf:4;dp:3",
#'                   "name" : "proportion",
#'                   "title" : "Proportion",
#'                   "type" : "number"
#'                },
#'                {
#'                   "format" : "dp:3;p:.001",
#'                   "name" : "p",
#'                   "title" : "p",
#'                   "type" : "number"
#'                }
#'             ]
#'          },
#'          "title" : "Binomial Test"
#'       },
#'       "title" : "Binomial Test"
#'    },
#'    "revision" : 2,
#'    "status" : "complete"
#' }')
#'
#' @export view
view <- function(results) {
  if (is.character(results) && jsonlite::validate(results) == TRUE) { # assuming a json string
    results <- jsonlite::fromJSON(results, simplifyVector=FALSE)
    if (!"results" %in% names(results))
      stop("Incorrect json provided. Could not locate required field 'results'")
  } else if (!is.list(results) || !"results" %in% names(results)) {
    stop("Incorrect object provided in results,
    please enter a valid json string or a named results list.")
  }

  content <- list(
    id = ifelse(is.null(results[["id"]]), 0, results[["id"]]),
    name = ifelse(is.null(results[["name"]]), "analysis", results[["name"]]),
    status = ifelse(is.null(results[["status"]]), "complete", results[["status"]]),
    results = results[["results"]]
  )
  json <- .convertResultsListToJson(content)
  
  .initializeOutputFolder(file.path(tempdir(), "jaspTools", "html"))
  
  htmlFile <- file.path(tempdir(), "jaspTools", "html", "tmp-index.html")
  .insertJsonInHtml(json, htmlFile)
  utils::browseURL(htmlFile)
}



#' Run a JASP analysis in R.
#'
#' \code{run} makes it possible to execute a JASP analysis in R. Usually this
#' process is a bit cumbersome as there are a number of objects unique to the
#' JASP environment. Think .ppi, data-reading, etc. These (rcpp) objects are
#' replaced in the jasptools so you do not have to deal with them. Note that
#' \code{run} sources JASP analyses every time it runs, so any change in
#' analysis code between calls is incorporated. The output of the analysis is
#' shown automatically through a call to \code{jaspTools::view} and returned
#' invisibly.
#'
#'
#' @param name String indicating the name of the analysis to run. This name is
#' identical to that of the main function in a JASP analysis.
#' @param dataset Data.frame, matrix, string name or string path; if it's a string then jasptools
#' first checks if it's valid path and if it isn't if the string matches one of the JASP datasets (e.g., "debug.csv").
#' By default the folder in Resources is checked first, unless called within a testthat environment, in which case tests/datasets is checked first.
#' @param options List of options to supply to the analysis (see also
#' \code{jaspTools::analysisOptions}).
#' @param perform String containing either "run" (default) or "init".
#' @param view Boolean indicating whether to view the results in a webbrowser.
#' @param quiet Boolean indicating whether to suppress messages from the
#' analysis.
#' @param makeTests Boolean indicating whether to create testthat unit tests and print them to the terminal.
#' @param sideEffects Boolean or character vector indicating which side effects
#' are allowed.  Side effects are persistent changes made by jasptools or
#' analyses run in jasptools, they include loading of packages ("pkgLoading"),
#' setting of .libPaths ("libPaths"), modifying of global options() ("options")
#' and altering the global environment ("globalEnv"). Supply the desired side
#' effects in a character vector (or simply TRUE for all). jasptools will make
#' an effort to prevent any side effect not included in the vector (or all if
#' set to FALSE)
#' @examples
#'
#' options <- jaspTools::analysisOptions("BinomialTest")
#' options[["variables"]] <- "contBinom"
#' jaspTools::run("BinomialTest", "debug", options)
#'
#' # Above and below are identical (below is taken from the Qt terminal)
#'
#' options <- jaspTools::analysisOptions('{
#'    "id" : 6,
#'    "name" : "BinomialTest",
#'    "options" : {
#'       "VovkSellkeMPR" : false,
#'       "confidenceInterval" : false,
#'       "confidenceIntervalInterval" : 0.950,
#'       "descriptivesPlots" : false,
#'       "descriptivesPlotsConfidenceInterval" : 0.950,
#'       "hypothesis" : "notEqualToTestValue",
#'       "plotHeight" : 300,
#'       "plotWidth" : 160,
#'       "testValue" : 0.50,
#'       "variables" : [ "contBinom" ]
#'    },
#'    "perform" : "run",
#'    "revision" : 1,
#'    "settings" : {
#'       "ppi" : 192
#'    }
#' }')
#' jaspTools::run("BinomialTest", "debug.csv", options)
#'
#' # If we want R functions sourced to the global env
#' jaspTools::run("BinomialTest", "debug.csv", options, sideEffects="globalEnv")
#'
#' # Or additionally have the .libPaths() set to the JASP R packages
#' jaspTools::run("BinomialTest", "debug.csv", options, sideEffects=c("globalEnv", "libPaths"))
#'
#' @export run
run <- function(name, dataset, options, perform = "run", view = TRUE, quiet = FALSE, makeTests = FALSE, sideEffects = FALSE) {
  if (missing(name)) {
    name <- attr(options, "analysisName")
    if (is.null(name))
      stop("please supply an analysis name")
  }
  
  if (!is.null(names(options)) && ".meta" %in% names(options))
    options[[".meta"]] <- NULL
  
  if (.insideTestEnvironment()) {
    view <- FALSE
    quiet <- TRUE
  }
  
  envir <- .GlobalEnv
  if (! isTRUE(sideEffects)) {
    if (! is.logical(sideEffects)) # users can supply a character vector
      sideEffects <- tolower(sideEffects)
    if (! "globalenv" %in% sideEffects || identical(sideEffects, FALSE))
      envir <- new.env()

    loadedPkgs <- loadedNamespaces()
    opts <- options()
    libPaths <- .libPaths()
    on.exit({
      if (! "options" %in% sideEffects || identical(sideEffects, FALSE))
        .restoreOptions(opts)
      if (! "libpaths" %in% sideEffects || identical(sideEffects, FALSE))
        .libPaths(libPaths)
    })
  }
  
  on.exit({
    .removeS3Methods()
    .resetRunTimeInternals()
    if (quiet)
      suppressWarnings(sink(NULL))
    if (!identical(envir, .GlobalEnv))
      rm(envir)
  }, add = TRUE)
  
  .initRunEnvironment(envir = envir, dataset = dataset, perform = perform)

  if (! tolower(name) %in% tolower(names(envir)))
    stop("Could not find the R analysis function ", name, ".\n",
         "If you're trying to run the R script of an analysis from a module you have to set the module directory with setPkgOption(\"module.dir\", dir/to/module)")
  
  name <- .getCasedNameMatchWithFunction(name, envir)
  
  possibleArgs <- list(
    name = name,
    title = "",
    requiresInit = TRUE,
    options = jsonlite::toJSON(options),
    dataKey = "null",
    resultsMeta = "null",
    stateKey = "null",
    perform = perform
  )

  usesJaspResults <- .usesJaspResults(name)
  if (usesJaspResults) {
    runFun <- "runJaspResults"
    
    jaspResults::initJaspResults()
    
    # this list is a stand in for the 'jaspResultsModule' inside runJaspResults()
    envir[["jaspResultsModule"]] <- list(
      create_cpp_jaspResults   = function(name, state) get("jaspResults", envir = .GlobalEnv)$.__enclos_env__$private$jaspObject
    )

  } else {
    runFun <- "run"
  }
  runArgs <- formals(envir[[runFun]])
  argNames <- intersect(names(possibleArgs), names(runArgs))
  args <- possibleArgs[argNames]

  if (makeTests)
    set.seed(1)
  
  if (quiet) {
    sink(tempfile())
    results <- suppressWarnings(do.call(envir[[runFun]], args, envir=envir))
    sink(NULL)
  } else {
    results <- do.call(envir[[runFun]], args, envir=envir)
  }

  if (usesJaspResults) {
    results <- jaspResults$.__enclos_env__$private$getResults()
    .transferPlotsFromjaspResults()
  }
  
  if (.insideTestEnvironment())
    .setInternal("lastResults", results)

  if (view)
    view(results)

  if (jsonlite::validate(results))
    results <- jsonlite::fromJSON(results, simplifyVector=FALSE)

  results[["state"]] <- .getInternal("state")

  figures <- results$state$figures
  if (length(figures) > 1 && !is.null(names(figures)))
    results$state$figures <- figures[order(as.numeric(tools::file_path_sans_ext(names(figures))))]
  
  if (makeTests)
    .makeUnitTestsFromResults(results, name, dataset, options, usesJaspResults)

  return(invisible(results))
}
