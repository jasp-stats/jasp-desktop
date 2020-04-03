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
#

EquivalenceBayesianOneSampleTTest <- function(jaspResults, dataset, options) {
  
  ready <- (length(options$variables) > 0)
  
  if (ready) {
    dataset <- .ttestBayesianReadData(dataset, options)
    errors <- .ttestBayesianGetErrorsPerVariable(dataset, options, "one-sample")
  }
  
  # Compute the results
  if (is.null(jaspResults[["equivalenceBayesianOneTTestTable"]]))
    equivalenceBayesianOneTTestResults <- .equivalenceBayesianOneTTestComputeResults(jaspResults, dataset, options, ready, errors)
  
  # Output tables and plots
  .equivalenceBayesianOneTTestTableMain(jaspResults, dataset, options, equivalenceBayesianOneTTestResults, ready)
  
  if(options$descriptives && is.null(jaspResults[["equivalenceBayesianDescriptivesTable"]]))
    .equivalenceBayesianOneTTestTableDescriptives(jaspResults, dataset, options, equivalenceBayesianOneTTestResults, ready)
  
  if (options$priorandposterior)
    .equivalencePriorandPosterior(jaspResults, dataset, options, equivalenceBayesianOneTTestResults, ready)
  
  if (options$plotSequentialAnalysis)
    .equivalencePlotSequentialAnalysis(jaspResults, dataset, options, equivalenceBayesianOneTTestResults, ready)
  
  if (options$densityPriorPosterior && is.null(jaspResults[["equivalenceDensityTable"]])) 
    .densityPriorPosteriorOneTTestTable(jaspResults, dataset, options, equivalenceBayesianOneTTestResults, ready)
  
  return()
}

.equivalenceBayesianOneTTestComputeResults <- function(jaspResults, dataset, options, ready, errors) {

  if (!ready)
    return(list())
  
  if (!is.null(jaspResults[["stateEquivalenceBayesianOneTTestResults"]]))
    return(jaspResults[["stateEquivalenceBayesianOneTTestResults"]]$object)

  results <- list()

  for (variable in options$variables) {
    
    results[[variable]] <- list()
    
    if(!isFALSE(errors[[variable]])) {
      
       errorMessage <- errors[[variable]]$message
       results[[variable]][["status"]]  <- "error"
       results[[variable]][["errorFootnotes"]] <- errorMessage
       
    } else {

      x <- dataset[[.v(variable)]]
      x <- x[!is.na(x)] - options$mu
      
      results[[variable]][["n1"]] <- length(x)
      results[[variable]][["n2"]] <- NULL
      
      r <- try(.generalEquivalenceTtestBF(x       = x, 
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
  jaspResults[["stateEquivalenceBayesianOneTTestResults"]] <- createJaspState(results)
  jaspResults[["stateEquivalenceBayesianOneTTestResults"]]$dependOn(c("variables", "mu", "lowerbound", "upperbound",
                                                                      "priorWidth", "effectSizeStandardized","informative", "informativeCauchyLocation", "informativeCauchyScale",
                                                                      "informativeNormalMean", "informativeNormalStd", "informativeTLocation",
                                                                      "informativeTScale", "informativeTDf", "missingValues"))
  return(results)
}
  
.equivalenceBayesianOneTTestTableMain <- function(jaspResults, dataset, options, equivalenceBayesianOneTTestResults, ready) {

  # Create table
  equivalenceBayesianOneTTestTable <- createJaspTable(title = gettext("Equivalence Bayesian One Sample T-Test"))
  equivalenceBayesianOneTTestTable$dependOn(c("variables", "mu", "lowerbound", "upperbound", "priorWidth", 
                                              "effectSizeStandardized","informative", "informativeCauchyLocation", "informativeCauchyScale",
                                              "informativeNormalMean", "informativeNormalStd", "informativeTLocation",
                                              "informativeTScale", "informativeTDf", "missingValues"))
  equivalenceBayesianOneTTestTable$showSpecifiedColumnsOnly <- TRUE

  # Add Columns to table
  equivalenceBayesianOneTTestTable$addColumnInfo(name = "variable",   title = " ",                          type = "string", combine = TRUE)
  equivalenceBayesianOneTTestTable$addColumnInfo(name = "statistic",  title = gettext("Model Comparison"),  type = "string")
  equivalenceBayesianOneTTestTable$addColumnInfo(name = "bf",         title = gettext("BF"),                type = "number")
  equivalenceBayesianOneTTestTable$addColumnInfo(name = "error",      title = gettext("error %"),           type = "number")

  if (ready)
    equivalenceBayesianOneTTestTable$setExpectedSize(length(options$variables))

  message <- gettextf("I ranges from %1$s to %2$s", options$lowerbound, options$upperbound)
  equivalenceBayesianOneTTestTable$addFootnote(message)

  jaspResults[["equivalenceBayesianOneTTestTable"]] <- equivalenceBayesianOneTTestTable

  if (!ready)
    return()

  .equivelanceBayesianOneTTestFillTableMain(equivalenceBayesianOneTTestTable, dataset, options, equivalenceBayesianOneTTestResults)

  return()
}

.equivelanceBayesianOneTTestFillTableMain <- function(equivalenceBayesianOneTTestTable, dataset, options, equivalenceBayesianOneTTestResults) {
  
  for (variable in options$variables) {
    
    results <- equivalenceBayesianOneTTestResults[[variable]]
    
    if (!is.null(results$status)) { 
      equivalenceBayesianOneTTestTable$addFootnote(message = results$errorFootnotes, rowNames = variable, colNames = "statistic")
      equivalenceBayesianOneTTestTable$addRows(list(variable = variable, statistic = NaN), rowNames = variable)
    } else {
      equivalenceBayesianOneTTestTable$addRows(list(variable      = variable,
                                                 statistic        = "\U003B4 \U02208 I vs. H1",
                                                 bf               = results$bfEquivalence,
                                                 error            = (results$errorPrior + results$errorPosterior) / results$bfEquivalence))
      
      equivalenceBayesianOneTTestTable$addRows(list(variable      = variable,
                                                 statistic        = "\U003B4 \U02209 I vs. H1",
                                                 bf               = results$bfNonequivalence,
                                                 error            = (results$errorPrior + results$errorPosterior) / results$bfNonequivalence))
      
      equivalenceBayesianOneTTestTable$addRows(list(variable      = variable,
                                                 statistic        = "\U003B4 \U02208 I vs. \U003B4 \U02209 I", # equivalence vs. nonequivalence"
                                                 bf               = results$bfEquivalence / results$bfNonequivalence,
                                                 error            = (2*(results$errorPrior + results$errorPosterior)) / (results$bfEquivalence / results$bfNonequivalence)))
      
      equivalenceBayesianOneTTestTable$addRows(list(variable      = variable,
                                                 statistic        = "\U003B4 \U02209 I vs. \U003B4 \U02208 I", # non-equivalence vs. equivalence
                                                 bf               = 1 / (results$bfEquivalence / results$bfNonequivalence),
                                                 error            = (2*(results$errorPrior + results$errorPosterior)) / (1/(results$bfEquivalence / results$bfNonequivalence))))
    }
  }
  
  return()
}

.equivalenceBayesianOneTTestTableDescriptives <- function(jaspResults, dataset, options, equivalenceBayesianOneTTestResults, ready) {
  if(!is.null(jaspResults[["equivalenceBayesianDescriptivesTable"]])) return()
  
  # Create table
  equivalenceBayesianDescriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  equivalenceBayesianDescriptivesTable$dependOn(c("variables", "descriptives", "missingValues"))
  equivalenceBayesianDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  
  # Add Columns to table
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "variable",   title = "",                   type = "string", combine = TRUE)
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "N",          title = gettext("N"),         type = "integer")
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "mean",       title = gettext("Mean"),      type = "number")
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "sd",         title = gettext("SD"),        type = "number")
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "se",         title = gettext("SE"),        type = "number")
  
  title <- gettextf("95%% Credible Interval")
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "lowerCI", type = "number", format = "sf:4;dp:3", title = gettext("Lower"), overtitle = title)
  equivalenceBayesianDescriptivesTable$addColumnInfo(name = "upperCI", type = "number", format = "sf:4;dp:3", title = gettext("Upper"), overtitle = title)
  
  jaspResults[["equivalenceBayesianDescriptivesTable"]] <- equivalenceBayesianDescriptivesTable
  
  for (variable in options$variables) {
    
    # Get data of the variable
    data <- dataset[[.v(variable)]]

    n    <- length(data)
    mean <- mean(data)
    sd   <- sd(data)
    se   <- sd/sqrt(n) 
    
    posteriorSummary <- .posteriorSummaryGroupMean(variable = data, descriptivesPlotsCredibleInterval = 0.95)
    ciLower <- .clean(posteriorSummary[["ciLower"]])
    ciUpper <- .clean(posteriorSummary[["ciUpper"]])
      
    equivalenceBayesianDescriptivesTable$addRows(list(variable      = variable, 
                                                      N             = n,
                                                      mean          = mean,
                                                      sd            = sd, 
                                                      se            = se,
                                                      lowerCI       = ciLower,
                                                      upperCI       = ciUpper))
  }
}

.densityPriorPosteriorOneTTestTable <- function(jaspResults, dataset, options, equivalenceBayesianOneTTestResults, ready) {
  
  equivalenceDensityTable <- createJaspTable(title = "Prior and Posterior Density Table")
  equivalenceDensityTable$dependOn(c("variables", "priorWidth", "mu",
                                     "effectSizeStandardized", "lowerbound", "upperbound", 
                                     "informative", "informativeCauchyLocation", "informativeCauchyScale",
                                     "informativeNormalMean", "informativeNormalStd", "informativeTLocation", 
                                     "informativeTScale", "informativeTDf"))
  
  equivalenceDensityTable$addColumnInfo(name = "variable",         title = gettext("Variable"),        type = "string", combine = TRUE)
  equivalenceDensityTable$addColumnInfo(name = "section",          title = gettext("Section"),         type = "string")
  equivalenceDensityTable$addColumnInfo(name = "density",          title = gettext("Density"),         type = "number")
  
  equivalenceDensityTable$showSpecifiedColumnsOnly <- TRUE
  
  if (ready)
    equivalenceDensityTable$setExpectedSize(length(options$variables))
  
  jaspResults[["equivalenceDensityTable"]] <- equivalenceDensityTable
  
  if (!ready)
    return()
  
  .equivalenceDensityFillTableMain(equivalenceDensityTable, dataset, options, equivalenceBayesianOneTTestResults)
  
  return()
  
}

.equivalenceDensityFillTableMain <- function(equivalenceDensityTable, dataset, options, equivalenceBayesianOneTTestResults) {
  
  for (variable in options$variables) {
    
    results <- equivalenceBayesianOneTTestResults[[variable]]
    
    if (!is.null(results$status)) {
      equivalenceDensityTable$addFootnote(message = results$errorFootnotes, rowNames = variable, colNames = "density")
      equivalenceDensityTable$addRows(list(variable = variable, density = NaN), rowNames = variable)
    } else {
      
      equivalenceDensityTable$addRows(list(variable   = variable,
                                           section       = "p(\U003B4 \U02208 I | H1)",
                                           density       = results$integralEquivalencePrior))
      
      equivalenceDensityTable$addRows(list(variable   = variable,
                                           section       = "p(\U003B4 \U02208 I | H1, y)",
                                           density       = results$integralEquivalencePosterior)) 
      
      equivalenceDensityTable$addRows(list(variable   = variable,
                                           section       = "p(\U003B4 \U02209 I | H1)",
                                           density       = results$integralNonequivalencePrior)) 
      
      equivalenceDensityTable$addRows(list(variable   = variable,
                                           section       = "p(\U003B4 \U02209 I | H1, y)",
                                           density       = results$integralNonequivalencePosterior)) 
    }
  }
  return()
}
