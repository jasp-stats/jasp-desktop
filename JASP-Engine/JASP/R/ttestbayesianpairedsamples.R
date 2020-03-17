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
TTestBayesianPairedSamples <- function(jaspResults, dataset, options) {

  .ttestBayesianRunAnalysis(jaspResults, dataset, options, "paired")

}

.ttestBPSTTest <- function(ttestContainer, dataset, options, derivedOptions, errors, ttestState) {

  ttestTable <- .ttestBPSTTestMarkup(options)
  ttestResults <- .ttestBayesianEmptyObject(options, derivedOptions, ttestState)
  dependents <- derivedOptions[["variables"]]
  ttestRows <- .ttestBayesianCreateTtestRows(dependents, options, derivedOptions, ttestState)
  ttestTable$setData(ttestRows)

  if (!is.null(derivedOptions[["footnotes"]]))
    ttestTable$addFootnote(derivedOptions[["footnotes"]])

  ttestContainer[["ttestTable"]] <- ttestTable
  if (!derivedOptions[["ready"]])
    return(ttestResults)

  alreadyComputed <- !is.na(ttestRows[, "BF"])
  .ttestBayesianSetFootnotesMainTable(ttestTable, ttestResults, dependents[alreadyComputed])
  .ttestBayesianInitBayesFactorPackageOptions()

  oneSided <- derivedOptions[["oneSided"]]
  bf.type <- options[["bayesFactorType"]]
  BFH1H0 <- ttestResults[["BFH1H0"]]

  for (var in dependents[!alreadyComputed]) {

    pair <- derivedOptions[["pairs"]][[var]]
    row <- list(variable1 = pair[[1L]], separator = "-", variable2 = pair[[2L]])

    if (!(pair[[1L]] == "" || pair[[2L]] == "")) {

      if (!isFALSE(errors[[var]])) {

        errorMessage <- errors[[var]]$message
        ttestTable$addFootnote(errorMessage, rowNames = var)
        ttestResults[["status"]][var] <- "error"
        ttestResults[["errorFootnotes"]][[var]] <- errorMessage
        ttestRows[var, c("BF", "error")] <- NaN

      } else {

        # these objects are made here so they don't need to be created every time a try fails,
        # which means they could be forgotten and not created
        bf.raw <- NaN
        error  <- NaN

        subDataSet <- dataset[, .v(c(pair[[1L]], pair[[2L]]))]
        subDataSet <- subDataSet[complete.cases(subDataSet), ]

        x <- subDataSet[[1L]]
        y <- subDataSet[[2L]]

        r <- try({.generalTtestBF(x = x, y = y, paired = TRUE, oneSided = oneSided, options = options)})

        if (isTryError(r)) {

          errorMessage <- .extractErrorMessage(r)
          ttestResults[["status"]][var] <- "error"
          ttestResults[["errorFootnotes"]][[var]] <- errorMessage
          ttestTable$addFootnote(message = errorMessage, rowNames = var)

        } else {

          bf.raw <- r[["bf"]]
          error  <- r[["error"]]
          ttestResults[["tValue"]][[var]] <- r[["tValue"]]
          ttestResults[["n1"]][var]       <- r[["n1"]]
          # ttestResults[["n2"]][var]       <- r[["n2"]]
          ttestResults[["tValue"]][var]   <- r[["tValue"]]

          if (!is.null(error) && is.na(error) && grepl("approximation", r[["method"]])) {
            error <- NaN
            ttestTable$addFootnote(
              message = gettext("t-value is large. A Savage-Dickey approximation was used to compute the Bayes factor but no error estimate can be given."),
              symbol = "", rowNames = var, colNames = "error")
          }
          if (is.null(error) && options[["effectSizeStandardized"]] == "informative" && 
              options[["informativeStandardizedEffectSize"]] == "normal") {
            error <- NA_real_
            ttestTable$addFootnote(message = gettext("No error estimate is available for normal priors."))
          }
        }
        ttestResults[["BF10post"]][var] <- bf.raw
        BF <- .recodeBFtype(bfOld     = bf.raw,
                            newBFtype = bf.type,
                            oldBFtype = "BF10")

        msg <- .ttestBayesianCheckBFPlot(BF)
        if (!is.null(msg)) {
          ttestResults[["plottingError"]][[var]] <- msg
          ttestResults[["status"]][var] <- "error"
        }
        ttestRows[var, "BF"]    <- BF
        ttestRows[var, "error"] <- error
      }
    }
    ttestTable$setData(ttestRows)
  }

  ttestResults[["ttestRows"]] <- ttestRows

  return(ttestResults)
}

.ttestBPSTTestMarkup <- function(options) {

  jaspTable <- createJaspTable(title = gettext("Bayesian Paired Samples T-Test"))
  jaspTable$dependOn(c("bayesFactorType", "pairs"))

  if (options[["effectSizeStandardized"]] == "default") {
    citations <- .ttestBayesianCitations[c("MoreyEtal2015", "RouderEtal2009")]
  } else if (options[["effectSizeStandardized"]] == "informative") {
    citations <- .ttestBayesianCitations["GronauEtal2017"]
  }

  jaspTable$addCitation(citations)

  bfType <- options[["bayesFactorType"]]

  hypothesis <- switch(options[["hypothesis"]],
                       "groupsNotEqual"  = "equal",
                       "groupOneGreater" = "greater",
                       "groupTwoGreater" = "smaller"
  )
  bfTitle <- .ttestBayesianGetBFTitle(bfType, hypothesis)

  jaspTable$addColumnInfo(name = "variable1", title = "",      type = "string")
  jaspTable$addColumnInfo(name = "separator", title = "",      type = "separator")
  jaspTable$addColumnInfo(name = "variable2", title = "",      type = "string")
  jaspTable$addColumnInfo(name = "BF",        title = bfTitle, type = "number")

  if (options[["hypothesis"]] == "groupsNotEqual") {
    fmt <- "sf:4;dp:3"
  } else {
    fmt <- "sf:4;dp:3;~"
  }
  jaspTable$addColumnInfo(name = "error", type = "number", format = fmt, title = gettext("error %"))
  return(jaspTable)
}

