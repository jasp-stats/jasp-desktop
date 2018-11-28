#' Test a specific JASP analysis.
#'
#' Tests a specific R analysis found under JASP-Tests. Useful to perform before
#' making a pull request, to prevent failing builds.
#'
#'
#' @param analysis String name of the analysis to test.
#' @examples
#'
#' jasptools::testAnalysis("AnovaBayesian")
#'
#' @export testAnalysis
testAnalysis <- function(analysis) {
  analysis <- .validateAnalysis(analysis)
  root <- .getPkgOption("tests.dir")
  file <- file.path(root, paste0("test-", analysis, ".R"))
  testthat::test_file(file)
}



#' Test all JASP analyses.
#'
#' Tests all R analyses found under JASP-Tests. Useful to perform before making
#' a pull request, to prevent failing builds.
#'
#'
#' @export testAll
testAll <- function() {
  testDir <- .getPkgOption("tests.dir")
  testthat::test_dir(testDir)
}



#' Visually inspect new/failed test plots.
#'
#' This function is a wrapper around \code{vdiffr::manage_cases()}. It allows
#' visual inspection of the plots in the unit tests that were newly added or
#' produced an error. If no analysis is specified it will iterate over all test
#' cases.
#'
#'
#' @param analysis Optional string name of the analysis whose plots should be
#' tested.
#' @return A Shiny app that shows all new/failed/orphaned cases. The app allows
#' test plots to be validated, at which point they are placed in the figs
#' folder and used as a reference for future tests.
#' @examples
#'
#' # jasptools::inspectTestPlots("Anova")
#'
#' @export inspectTestPlots
inspectTestPlots <- function(analysis = NULL) {
  .Deprecated(msg = "'inspectTestPlots' is deprecated.\nCreate tests for ggplot objects directly with testthat.")
  # if (! is.null(analysis)) {
  #   analysis <- .validateAnalysis(analysis)
  #   analysis <- paste0("^", analysis, "$")
  # }
  # testDir <- .getPkgOption("tests.dir")
  # on.exit(unloadNamespace("SomePkg")) # unload fake pkg in JASP unit tests, which is needed to run vdiffr
  # vdiffr::manage_cases(testDir, analysis)
}



#' Aids in the creation of tests for tables.
#'
#' This function is designed to make it easier to create unit tests for tables.
#' It strips off attributes and flattens the structure until a list remains
#' with dimension 1. Output is then produced which can be immediately placed in
#' the test file.
#'
#'
#' @param rows A list with lists of rows (i.e., a JASP table).
#' @return Copy-paste ready output which may serve as the reference to test
#' tables against.
#' @examples
#'
#' options <- jasptools::analysisOptions("BinomialTest")
#' options[["variables"]] <- "contBinom"
#' results <- jasptools::run("BinomialTest", "debug", options, view=FALSE)
#' jasptools::makeTestTable(results[["results"]][["binomial"]][["data"]])
#'
#' @export makeTestTable
makeTestTable <- function(rows) {
  x <- collapseTable(rows)
  result <- ""
  nChars <- 0
  for (i in 1:length(x)) {
    element <- x[[i]]
    if (! is.numeric(element)) {
      element <- paste0("\"", element, "\"")
    }

    nOldChars <- nchar(result)
    if (nchar(result) == 0) {
      result <- element
    } else {
      result <- paste(result, element, sep=", ")
    }

    nChars <- nChars + nchar(result) - nOldChars
    if (nChars >= 60) {
      result <- paste0(result, "\n")
      nChars <- 0
    }
  }

  result <- gsub("\n,", ",\n", result, fixed=TRUE)
  if (endsWith(result, "\n")) {
    result <- substr(result, 1, nchar(result)-1)
  }
  result <- paste0("list(", result, ")")

  cat(result)
}

#' @export approxMatch
approxMatch <- function(new, old, tol = 1e-5) {

  idxNumNew <- sapply(new, is.numeric)
  idxNumOld <- sapply(old, is.numeric)
  idxCharNew <- sapply(new, is.character)
  idxCharOld <- sapply(old, is.character)

  numNew <- unlist(new[idxNumNew])
  numOld <- unlist(old[idxNumOld])
  charNew <- unlist(new[idxCharNew])
  charOld <- unlist(old[idxCharOld])

  idxCharMis <- !charOld %in% charNew
  idxMissingChars <- which(idxCharOld)[idxCharMis]

  idxFoundNums <- NULL
  idxMissNums <- NULL
  for (i in seq_along(numOld)) {
    v <- abs(numOld[i] - numNew)
    idx <- which.min(v)
    if (isTRUE(v[idx] < tol)) {
      idxFoundNums <- rbind(idxFoundNums, c(i, idx))
    } else {
      idxMissNums <- rbind(idxMissNums, c(i, idx))
    }
  }
  idxNumNewMiss <- which(idxNumNew)[idxMissNums[, 2]]
  idxNumOldMiss <- which(idxNumOld)[idxMissNums[, 1]]

  if (length(idxMissingChars) > 0) {
    cat("Missing character(s) in old\n")
    print(old[idxMissingChars])
  }
  if (length(idxMissNums) > 0) {
    cat("Could not match the following value(s) from old in new.\n")
    for (i in 1:nrow(idxMissNums)) {
      cat(sprintf("value: %.6f at index %d\n",
                  old[[idxNumOldMiss[i]]], idxNumOldMiss[i]))
      cat("Closest match:\n")
      cat(sprintf("value: %.6f at index %d\n",
                  new[[idxNumNewMiss[i]]], idxNumNewMiss[i]))
    }
  }
  if (length(idxMissingChars) > 0 || length(idxMissNums) > 0) {
    cat("Some elements of old were not found in.\nPlease compare the results carefully!")
  } else {
    cat("All elements of old appear in new.")
  }

  return(invisible(list(
    idxMissingChars = idxMissingChars,
    idxNumNewMiss = idxNumNewMiss,
    idxNumOldMiss = idxNumOldMiss
  )))

}
