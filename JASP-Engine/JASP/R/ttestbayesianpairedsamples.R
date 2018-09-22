#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

# abbreviation:
# ttestBPS = ttestBayesianPairedSamples
TTestBayesianPairedSamples <- function(jaspResults, dataset, options, state = NULL) {

  # initialize
  jaspResults$title <- "Bayesian Paired Samples T-Test"
  options <- .ttestBayesianInitOptions(jaspResults, options, "paired")
  dataset <- .ttestBayesianReadData(dataset, options[["pairs"]], missing = options[["missingValues"]])
  errors  <- .ttestBayesianGetErrorsPerVariable(options, dataset)
  .ttestBayesianInitBayesFactorPackageOptions()

  # do t-test and create main table
  ttestResults <- .ttestBPSTTest(jaspResults, dataset, options, errors)

  # create descriptives table and plots
  .ttestBayesianDescriptives(jaspResults, dataset, options, errors)

  # create inferential plots
  .ttestBayesianInferentialPlots(jaspResults, dataset, options, ttestResults, errors)

  return()

}

.ttestBPSTTest <- function(jaspResults, dataset, options, errors) {

  if (!is.null(jaspResults[["ttestTable"]]) && !options[["anyNewVariables"]]) {
    return(jaspResults[["stateTTestResults"]]$object)
  }

  ttestTable <- createJaspTable(title = "Bayesian Paired Samples T-Test")
  jaspResults[["ttestTable"]] <- ttestTable
  dependencies <- options[["stateKey"]][["ttestResults"]]
  ttestTable$dependOnOptions(c(dependencies, "bayesFactorType", "pairs"))
  .ttestBPSTTestMarkup(ttestTable, options)

  dependents <- options[["variables"]]
  ttestResults <- .ttestBayesianEmptyObject(options)

  if (!options[["canDoAnalysis"]]) {
    for (var in dependents) {
      pair <- options[["pairs"]][[var]]
      ttestTable$addRows(list(variable1 = pair[[1L]], .separator = "-", variable2 = pair[[2L]]))
    }
  } else {
    ttestState <- jaspResults[["stateTTestResults"]]$object # is there useable data?
    ttestRows <- ttestState$ttestRows

    oneSided <- options[["oneSided"]]
    bf.type <- options[["bayesFactorType"]]
    BFH1H0 <- !bf.type == "BF01"

    for (var in dependents) {

      if (!is.null(ttestRows[[var]])) {

        # row retrieved from state, only possible change is BF01 to BF10/ log(BF01)
        ttestRows[[var]][["BF"]] <-
          .recodeBFtype(bfOld     = ttestRows[[var]][["BF"]],
                        newBFtype = options[["bayesFactorType"]],
                        oldBFtype = ttestState[["bayesFactorType"]]
          )
        row <- ttestRows[[var]]

      } else {

        pair <- options[["pairs"]][[var]]
        row <- list(variable1 = pair[[1L]], separator = "-", variable2 = pair[[2L]])

        if (!(pair[[1L]] == "" || pair[[2L]] == "")) {

          if (!isFALSE(errors[[var]])) {
            errorMessage <- errors[[var]]$message

            row[["BF"]]    <-  .clean(NaN)
            row[["error"]] <- ""
            ttestTable$addFootnote(message = errorMessage, row_names = var)

          } else {

            subDataSet <- dataset[, .v(c(pair[[1L]], pair[[2L]]))]
            subDataSet <- subDataSet[!is.na(subDataSet), ]

            c1 <- subDataSet[[1L]]
            c2 <- subDataSet[[2L]]

            r <- try({.generalTtestBF(x = c1, y = c2, paired = TRUE, oneSided = oneSided, options = options)})

            if (isTryError(r)) {

              errorMessage <- .extractErrorMessage(r)
              ttestResults$status[var] <- "error"
              ttestResults$errorFootnotes[var] <- errorMessage
              ttestTable$addFootnote(message = errorMessage, row_names = var)
              row <- list(variable1 = pair[[1L]], separator = "-", variable2 = pair[[2L]], BF = .clean(NaN), error = "")

            } else {

              bf.raw <- r[["bf"]]
              ttestResults$tValue[var] <- r[["tValue"]]
              ttestResults$n1[var] <- r[["n1"]]

              if (is.na(bf.raw)) {
                plottingError[[var]] <- "Bayes factor could not be calculated"
              } else if (is.infinite(bf.raw)) {
                plottingError[[var]] <- "Bayes factor is infinite"
              } else if (is.infinite(1 / bf.raw)) {
                plottingError[[var]] <- "The Bayes factor is too small"
              }

              ttestResults$BF10post[var] <- .recodeBFtype(bfOld = bf.raw,
                                                          newBFtype = bf.type,
                                                          oldBFtype = "BF10")

              error <- .clean(r[["error"]])
              row[["BF"]]    <- ttestResults$BF10post[var]
              row[["error"]] <- error
            }
          }
        }
      }
      ttestTable$addRows(row, rowNames = var)
    }

    ttestResults$ttestRows <- ttestRows
    ttestResults$BFH1H0    <- BFH1H0

    tmp <- createJaspState(
      object = ttestResults,
      title  = "mainResultsObject"
    )
    tmp$dependOnOptions(dependencies)
    jaspResults[["stateTTestResults"]] <- tmp

  }

  ttestTable$status <- "complete"

  return(ttestResults)

}

.ttestBPSTTestMarkup <- function(jaspTable, options) {

  if (options$effectSizeStandardized == "default") {
  	citations <- list(
  		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
  		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237."
  		)
  } else if (options$effectSizeStandardized == "informative") {
    citations <- list("Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2017). Informed Bayesian T-Tests. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1704.02479")
  }
	for (c in citations)
		jaspTable$addCitation(c)

  bfType <- options$bayesFactorType

  hypothesis <- switch(options[["hypothesis"]],
    "groupsNotEqual"  = "equal",
    "groupOneGreater" = "greater",
    "groupTwoGreater" = "smaller"
  )
  bfTitle <- .ttestBayesianGetBFTitle(bfType, hypothesis)

  jaspTable$addColumnInfo(name = "variable1", title = "",      type = "string")
  jaspTable$addColumnInfo(name = "separator", title = "",      type = "string")
  jaspTable$addColumnInfo(name = "variable2", title = "",      type = "string")
  jaspTable$addColumnInfo(name = "BF",        title = bfTitle, type = "number", format = "sf:4;dp:3")

  if (options$hypothesis == "notEqualToTestValue") {
    fmt <- "sf:4;dp:3"
  } else {
    fmt <- "sf:4;dp:3;~"
  }
  jaspTable$addColumnInfo(name = "error", type = "number", format = fmt, title = "error %")
}

