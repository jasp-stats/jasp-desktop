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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

EquivalenceBayesianIndependentSamplesTTest <- function(jaspResults, dataset, options) {

  ready <- (length(options$variables) != 0 && options$groupingVariable != "")

  if (ready) {
    dataset <- .ttestBayesianReadData(dataset, options)
    errors  <- .ttestBayesianGetErrorsPerVariable(dataset, options, "independent")
  }

  # Compute the results
  if(options[['equivalenceRegion']] == "lower"){
    options$lowerbound <- -Inf
    options$upperbound <- options$lower_max
  } else if(options[['equivalenceRegion']] == "upper"){
    options$lowerbound <- options$upper_min
    options$upperbound <- Inf
  }
  equivalenceBayesianIndTTestResults <- .equivalenceBayesianIndTTestComputeResults(jaspResults, dataset, options, ready, errors)

  # Output tables and plots
  if (is.null(jaspResults[["equivalenceBayesianIndTTestTable"]]))
    .equivalenceBayesianIndTTestTableMain(jaspResults, dataset, options, equivalenceBayesianIndTTestResults, ready)

  if (options$descriptives && is.null(jaspResults[["equivalenceBayesianDescriptivesTable"]]))
    .equivalenceBayesianIndTTestTableDescriptives(jaspResults, dataset, options, equivalenceBayesianIndTTestResults, ready)

  if (options$priorandposterior)
    .equivalencePriorandPosterior(jaspResults, dataset, options, equivalenceBayesianIndTTestResults, ready)

  if (options$plotSequentialAnalysis)
      .equivalencePlotSequentialAnalysis(jaspResults, dataset, options, equivalenceBayesianIndTTestResults, ready)

  if (options$massPriorPosterior && is.null(jaspResults[["equivalenceMassTable"]]))
    .massPriorPosteriorIndpTTestTable(jaspResults, dataset, options, equivalenceBayesianIndTTestResults, ready)

  return()
}

.equivalenceBayesianIndTTestComputeResults <- function(jaspResults, dataset, options, ready, errors) {

  if (!ready)
    return(list())

  if (!is.null(jaspResults[["stateEquivalenceBayesianIndTTestResults"]]))
    return(jaspResults[["stateEquivalenceBayesianIndTTestResults"]]$object)

  results <- list()

  group  <- options$groupingVariable
  levels <- levels(dataset[[.v(group)]])
  g1     <- levels[1L]
  g2     <- levels[2L]

  idxg1  <- dataset[[.v(group)]] == g1
  idxg2  <- dataset[[.v(group)]] == g2
  idxNAg <- is.na(dataset[[.v(group)]])

  for (variable in options$variables) {

    results[[variable]] <- list()

    if(!isFALSE(errors[[variable]])) {

      errorMessage <- errors[[variable]]$message
      results[[variable]][["status"]] <- "error"
      results[[variable]][["errorFootnotes"]] <- errorMessage

    } else {

      # It is necessary to remove NAs
      idxNA      <- is.na(dataset[[.v(variable)]]) | idxNAg
      subDataSet <- dataset[!idxNA, .v(variable)]

      group1 <- subDataSet[idxg1[!idxNA]]
      group2 <- subDataSet[idxg2[!idxNA]]

      results[[variable]][["n1"]]     <- length(group1)
      results[[variable]][["n2"]]     <- length(group2)
      results[[variable]][["status"]] <- NULL

      r <- try(.generalEquivalenceTtestBF(x       = group1,
                                          y       = group2,
                                          options = options))

      if (isTryError(r)) {
        errorMessage <- .extractErrorMessage(r)
        results[[variable]][["status"]] <- "error"
        results[[variable]][["errorFootnotes"]] <- errorMessage

      } else if (r[["bfEquivalence"]] < 0 || r[["bfNonequivalence"]] < 0) {

        results[[variable]][["status"]] <- "error"
        results[[variable]][["errorFootnotes"]] <- "Not able to calculate Bayes factor while the integration was too unstable"

      } else {
        results[[variable]][["bfEquivalence"]]                   <- r[["bfEquivalence"]]
        results[[variable]][["bfNonequivalence"]]                <- r[["bfNonequivalence"]]
        results[[variable]][["errorPrior"]]                      <- r[["errorPrior"]]
        results[[variable]][["errorPosterior"]]                  <- r[["errorPosterior"]]
        results[[variable]][["tValue"]]                          <- r[["tValue"]]
        results[[variable]][["integralEquivalencePosterior"]]    <- r[["integralEquivalencePosterior"]]
        results[[variable]][["integralEquivalencePrior"]]        <- r[["integralEquivalencePrior"]]
        results[[variable]][["integralNonequivalencePosterior"]] <- r[["integralNonequivalencePosterior"]]
        results[[variable]][["integralNonequivalencePrior"]]     <- r[["integralNonequivalencePrior"]]
      }
    }

  }

  # Save results to state
  jaspResults[["stateEquivalenceBayesianIndTTestResults"]] <- createJaspState(results)
  jaspResults[["stateEquivalenceBayesianIndTTestResults"]]$dependOn(c("variables", "groupingVariable", "equivalenceRegion", "lower", "upper",
                                                                      "region", "lowerbound", "upperbound", "lower_max", "upper_min", "prior", "missingValues"))

  return(results)
}

.equivalenceBayesianIndTTestTableMain <- function(jaspResults, dataset, options, equivalenceBayesianIndTTestResults, ready) {

  # Create table
  equivalenceBayesianIndTTestTable <- createJaspTable(title = gettext("Equivalence Bayesian Independent Samples T-Test"))
  equivalenceBayesianIndTTestTable$dependOn(c("variables", "groupingVariable", "priorWidth",
                                            "effectSizeStandardized", "equivalenceRegion", "lower", "upper", "region", "lowerbound", "upperbound",
                                            "lower_max", "upper_min", "informative", "informativeCauchyLocation", "informativeCauchyScale",
                                            "informativeNormalMean", "informativeNormalStd", "informativeTLocation",
                                            "informativeTScale", "informativeTDf"))

  # Add Columns to table
  equivalenceBayesianIndTTestTable$addColumnInfo(name = "variable",   title = " ",                          type = "string", combine = TRUE)
  equivalenceBayesianIndTTestTable$addColumnInfo(name = "statistic",  title = gettext("Model Comparison"),  type = "string")
  equivalenceBayesianIndTTestTable$addColumnInfo(name = "bf",         title = gettext("BF"),                type = "number")
  equivalenceBayesianIndTTestTable$addColumnInfo(name = "error",      title = gettext("error %"),           type = "number")

  equivalenceBayesianIndTTestTable$showSpecifiedColumnsOnly <- TRUE

  if (ready)
    equivalenceBayesianIndTTestTable$setExpectedSize(length(options$variables))

  message <- gettextf("I ranges from %1$s to %2$s", options$lowerbound, options$upperbound)
  equivalenceBayesianIndTTestTable$addFootnote(message)

  jaspResults[["equivalenceBayesianIndTTestTable"]] <- equivalenceBayesianIndTTestTable

  if (!ready)
    return()

  .equivelanceBayesianIndTTestFillTableMain(equivalenceBayesianIndTTestTable, dataset, options, equivalenceBayesianIndTTestResults)

  return()
}

.equivelanceBayesianIndTTestFillTableMain <- function(equivalenceBayesianIndTTestTable, dataset, options, equivalenceBayesianIndTTestResults) {

  for (variable in options$variables) {

    results <- equivalenceBayesianIndTTestResults[[variable]]

    if (!is.null(results$status)) {
      equivalenceBayesianIndTTestTable$addFootnote(message = results$errorFootnotes, rowNames = variable, colNames = "statistic")
      equivalenceBayesianIndTTestTable$addRows(list(variable = variable, statistic = NaN), rowNames = variable)
    } else {

      equivalenceBayesianIndTTestTable$addRows(list(variable      = variable,
                                                    statistic     = "\U003B4 \U02208 I vs. H1",
                                                    bf            = results$bfEquivalence,
                                                    error         = (results$errorPrior + results$errorPosterior) / results$bfEquivalence))

      equivalenceBayesianIndTTestTable$addRows(list(variable      = variable,
                                                    statistic     = "\U003B4 \U02209 I vs. H1",
                                                    bf            = results$bfNonequivalence,
                                                    error         = (results$errorPrior + results$errorPosterior) / results$bfNonequivalence))

      equivalenceBayesianIndTTestTable$addRows(list(variable      = variable,
                                                    statistic     = "\U003B4 \U02208 I vs. \U003B4 \U02209 I", # equivalence vs. nonequivalence"
                                                    bf            = results$bfEquivalence / results$bfNonequivalence,
                                                    error         = (2*(results$errorPrior + results$errorPosterior)) / (results$bfEquivalence / results$bfNonequivalence)))

      equivalenceBayesianIndTTestTable$addRows(list(variable      = variable,
                                                    statistic     = "\U003B4 \U02209 I vs. \U003B4 \U02208 I", # non-equivalence vs. equivalence
                                                    bf            = 1 / (results$bfEquivalence / results$bfNonequivalence),
                                                    error         = (2*(results$errorPrior + results$errorPosterior)) / (1/(results$bfEquivalence / results$bfNonequivalence))))
    }
  }

  return()
}

.equivalenceBayesianIndTTestTableDescriptives <- function(jaspResults, dataset, options, equivalenceBayesianIndTTestResults, ready) {

  # Create table
  equivalenceBayesianDescriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  equivalenceBayesianDescriptivesTable$dependOn(c("variables", "groupingVariable", "descriptives", "missingValues"))

  # Add Columns to table
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "variable",   title = "",                   type = "string", combine = TRUE)
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "level",      title = gettext("Group"),     type = "string")
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "N",          title = gettext("N"),         type = "integer")
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "mean",       title = gettext("Mean"),      type = "number")
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "sd",         title = gettext("SD"),        type = "number")
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "se",         title = gettext("SE"),        type = "number")

  title <- gettextf("95%% Credible Interval")
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "lowerCI", type = "number", format = "sf:4;dp:3", title = gettext("Lower"), overtitle = title)
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "upperCI", type = "number", format = "sf:4;dp:3", title = gettext("Upper"), overtitle = title)

  equivalenceBayesianDescriptivesTable$showSpecifiedColumnsOnly <- TRUE

  if (ready)
    equivalenceBayesianDescriptivesTable$setExpectedSize(length(options$variables))

  jaspResults[["equivalenceBayesianDescriptivesTable"]] <- equivalenceBayesianDescriptivesTable

  if (!ready)
    return()

  .equivalenceBayesianFillDescriptivesTable(equivalenceBayesianDescriptivesTable, dataset, options, equivalenceBayesianIndTTestResults)

  return()
}

.equivalenceBayesianFillDescriptivesTable <- function(equivalenceBayesianDescriptivesTable, dataset, options, equivalenceBayesianIndTTestResults) {

  for (variable in options$variables) {

    results <- equivalenceBayesianIndTTestResults[[variable]]

    if (!is.null((results$status))) {
      equivalenceBayesianDescriptivesTable$addFootnote(message = results$errorFootnotes, rowNames = variable, colNames = "level")
      equivalenceBayesianDescriptivesTable$addRows(list(variable = variable, level = NaN), rowNames = variable)
    } else {
      # Get data of the grouping variable
      data    <- dataset[[.v(options$groupingVariable)]]
      levels  <- levels(data)
      nlevels <- length(levels)

      for (i in 1:nlevels) {

        # Get data per level
        level <- levels[i]
        groupData <- na.omit(dataset[data == level, .v(variable)])

        # Calculate descriptives per level
        n                <- length(groupData)
        mean             <- mean(groupData)
        sd               <- sd(groupData)
        se               <- sd/sqrt(n)
        posteriorSummary <- .posteriorSummaryGroupMean(variable = groupData, descriptivesPlotsCredibleInterval = 0.95)
        ciLower          <- .clean(posteriorSummary[["ciLower"]])
        ciUpper          <- .clean(posteriorSummary[["ciUpper"]])

        equivalenceBayesianDescriptivesTable$addRows(list(variable      = variable,
                                                          level         = level,
                                                          N             = n,
                                                          mean          = mean,
                                                          sd            = sd,
                                                          se            = se,
                                                          lowerCI       = ciLower,
                                                          upperCI       = ciUpper))
      }
    }
  }

  return()
}

.massPriorPosteriorIndpTTestTable <- function(jaspResults, dataset, options, equivalenceBayesianIndTTestResults, ready) {
  equivalenceMassTable <- createJaspTable(title = gettext("Prior and Posterior Mass Table"))
  equivalenceMassTable$dependOn(c("variables", "groupingVariable", "priorWidth", "lower", "upper", "region",
                                  "effectSizeStandardized", "equivalenceRegion", "lowerbound", "upperbound", "lower_max", "upper_min",
                                  "informative", "informativeCauchyLocation", "informativeCauchyScale",
                                  "informativeNormalMean", "informativeNormalStd", "informativeTLocation",
                                  "informativeTScale", "informativeTDf"))

  equivalenceMassTable$addColumnInfo(name = "variable",         title = gettext("Variable"),        type = "string", combine = TRUE)
  equivalenceMassTable$addColumnInfo(name = "section",          title = gettext("Section"),         type = "string")
  equivalenceMassTable$addColumnInfo(name = "mass",             title = gettext("Mass"),            type = "number")

  equivalenceMassTable$showSpecifiedColumnsOnly <- TRUE

  if (ready)
    equivalenceMassTable$setExpectedSize(length(options$variables))

  jaspResults[["equivalenceMassTable"]] <- equivalenceMassTable

  if (!ready)
    return()

  .equivalenceMassFillTableMain(equivalenceMassTable, dataset, options, equivalenceBayesianIndTTestResults)

  return()

}

.equivalenceMassFillTableMain <- function(equivalenceMassTable, dataset, options, equivalenceBayesianIndTTestResults) {
  for (variable in options$variables) {

    results <- equivalenceBayesianIndTTestResults[[variable]]

    if (!is.null(results$status)) {
      equivalenceMassTable$addFootnote(message = results$errorFootnotes, rowNames = variable, colNames = "mass")
      equivalenceMassTable$addRows(list(variable = variable, mass = NaN), rowNames = variable)
    } else {

      equivalenceMassTable$addRows(list(variable   = variable,
                                        section    = "p(\U003B4 \U02208 I | H1)",
                                        mass       = results$integralEquivalencePrior))

      equivalenceMassTable$addRows(list(variable   = variable,
                                        section    = "p(\U003B4 \U02208 I | H1, y)",
                                        mass       = results$integralEquivalencePosterior))

      equivalenceMassTable$addRows(list(variable   = variable,
                                        section    = "p(\U003B4 \U02209 I | H1)",
                                        mass       = results$integralNonequivalencePrior))

      equivalenceMassTable$addRows(list(variable   = variable,
                                        section    = "p(\U003B4 \U02209 I | H1, y)",
                                        mass       = results$integralNonequivalencePosterior))
    }
  }
  return()
}
