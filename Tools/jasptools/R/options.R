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
#' @param hint Boolean. Should additional hints be placed in the output so you
#' know how to give values to differents types of options? Only works if
#' \code{source} is set to the name of an analysis.
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
analysisOptions <- function(source, hint = FALSE) {
  if (! is.character(source) || length(source) > 1) {
    stop("Expecting a character input of length 1 as source,
    either a json string or analysis name.")
  }

  type <- "file"
  if (jsonlite::validate(source) == TRUE) {
    type <- "qt"
  }

  options <- NULL
  if (type == "qt") {
    analysisName <- stringr::str_match(source, '\\"name\\" : \\"(.*?)\\"')[2L]
    options <- .analysisOptionsFromQt(source)
  } else {
    analysisName <- source
    rawOptions <- .analysisOptionsFromFile(source)
    options <- .fillOptions(rawOptions, hint)
  }
  attr(options, "analysisName") <- analysisName
  return(options)
}

.analysisOptionsFromFile <- function(analysis) {
  file <- file.path(.getPkgOption("json.dir"), paste0(analysis, ".json"))
  analysisOpts <- try(jsonlite::read_json(file), silent = TRUE)

  if (inherits(analysisOpts, "try-error")) {
    stop("The JSON file for the analysis you supplied could not be found.
    Please ensure that (1) its name matches the main R function
    and (2) your working directory is set properly.")
  }

  if ("options" %in% names(analysisOpts)) {
    return(analysisOpts[["options"]])
  } else if ("options" %in% names(analysisOpts[["input"]])) {
    return(analysisOpts[["input"]][["options"]])
  } else {
    stop("The JSON file was found, but it appears to be invalid")
  }

}

.analysisOptionsFromQt <- function(x) {
  json <- try(jsonlite::fromJSON(x, simplifyVector=FALSE), silent = TRUE)

  if (inherits(json, "try-error")) {
    stop("Your json is invalid, please copy the entire message
    including the outer braces { } that was send to R in the Qt terminal.
    Remember to use single quotes around the message.")
  }

  if ("options" %in% names(json)) {
    return(json[["options"]])
  } else {
    stop("The JSON file appears to be invalid")
  }

}

.fillOptions <- function(options, hint = FALSE) {
  output <- list()
  for (i in 1:length(options)) {
    option <- options[[i]]
    if ("default" %in% names(option)) {
      output[[option[["name"]]]] <- option[["default"]]
    } else {
      if (option[["type"]] == "Table" && hint) {
        template <- option[["template"]]
        output[[option[["name"]]]] <- list(list())
        for (j in 1:length(template)) {
          name <- template[[j]][["name"]]
          value <- .optionTypeToValue(template[[j]], hint)
          output[[option[["name"]]]][[1]][[name]] <- value
        }
      } else {
        output[[option[["name"]]]] <- .optionTypeToValue(option, hint)
      }
    }
  }

  return(output)
}

.optionTypeToValue <- function(option, hint = FALSE) {
  switch(option[["type"]],
         Boolean =
           FALSE,

         Integer =
           if (hint) {
             "%420%"
           } else {
             ""
           },

         IntegerArray =
           if (hint) {
             c("%25%", "%95%")
           } else {
             list()
           },

         List =
           option[["options"]][[1]],

         Number =
           option[["value"]],

         Table =
           list(),

         String =
           if (hint) {
             "%SomeString%"
           } else {
             ""
           },

         Term =
           if (hint) {
             "%variable1%"
           } else {
             ""
           },

         Terms =
           if (hint) {
             list(c("%variable1%"),
                  c("%variable2%"),
                  c("%variable1%", "%variable3%"))
           } else {
             list()
           },

         Variable =
           if (hint) {
             "%variable1%"
           } else {
             ""
           },

         Variables =
           if (hint) {
             c("%variable1%", "%variable2%")
           } else {
             list()
           },

         VariablesGroups =
           if (hint) {
             list(c("%variable1%", "%variable2%"),
                  c("%variable3%", "%variable4%"))
           } else {
             list()
           },

         NULL
  )
}
