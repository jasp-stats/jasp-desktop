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

SummaryStatsTTestBayesianOneSample <- function(jaspResults, dataset = NULL, options, ...) {
  
  # Reading in a datafile is not necessary
  # Check user input for possible errors
  .checkErrors.summarystats.onesample.pairedsamples(options)
  
  # Compute the results and create main results table
  summaryStatsOneSampleResults <- .summaryStatsOneSampleMainFunction(jaspResults, options)
  # Output plots 
  .ttestBayesianPriorPosteriorPlot.summarystats(jaspResults, summaryStatsOneSampleResults, options)
  .ttestBayesianPlotRobustness.summarystats(jaspResults, summaryStatsOneSampleResults, options)
  
  return()
}

# Execute Bayesian one sample t-test ----
.summaryStatsOneSampleMainFunction <- function(jaspResults, options) {
  
  # This function is the main workhorse, and also makes the table
  if (is.null(jaspResults[["ttestContainer"]])) {
    jaspResults[["ttestContainer"]] <- createJaspContainer()
    # add dependencies for main table (i.e., when does it have to recompute values for the main table)
    jaspResults[["ttestContainer"]]$dependOn(c("tStatistic"                   , "n1Size"                , "hypothesis",     # standard entries
                                               "defaultStandardizedEffectSize", "informativeStandardizedEffectSize"   ,     # informative or default
                                               "priorWidth"                   , "effectSizeStandardized",                   # default prior
                                               "informativeCauchyLocation"    , "informativeCauchyScale",                   # informed cauchy priors
                                               "informativeNormalMean"        , "informativeNormalStd"  ,                   # informed normal priors
                                               "informativeTLocation"         , "informativeTScale"     , "informativeTDf"  # informed t-distribution
    ))
  }
  
  # If table already exists in the state, return it
  if (!is.null(jaspResults[["ttestContainer"]][["oneSampleTTestTable"]]))
    return(jaspResults[["ttestContainer"]][["stateSummaryStatsOneSampleResults"]]$object)
  
  # Otherwise: create the empty table before executing the analysis
  hypothesisList <- .hypothesisType.summarystats.ttest.onesample(hypothesis = options$hypothesis, bayesFactorType = options$bayesFactorType)
  jaspResults[["ttestContainer"]][["oneSampleTTestTable"]] <- .summaryStatsOneSamplePairedSamplesTableMain(options, hypothesisList, title = "Bayesian One Sample T-Test")
  
  if (!is.null(jaspResults[["ttestContainer"]][["stateSummaryStatsOneSampleResults"]])) {
    results <- jaspResults[["ttestContainer"]][["stateSummaryStatsOneSampleResults"]]$object
    # only change possible: BF type
    results[["oneSampleTTestTable"]][["BF"]] <- results[["BFlist"]][[options$bayesFactorType]]
  } else {
    results <- .summaryStatsOneSamplePairedSamplesComputeResults(hypothesisList, options)
    # Save results to state
    jaspResults[["ttestContainer"]][["stateSummaryStatsOneSampleResults"]] <- createJaspState(results)
    
    if (!is.null(results[["errorMessageTable"]]))
      jaspResults[["ttestContainer"]][["oneSampleTTestTable"]]$setError(results[["errorMessageTable"]])
  }
  
  #  fill table if ready
  if (results[["ready"]])
    jaspResults[["ttestContainer"]][["oneSampleTTestTable"]]$setData(results[["ttestTable"]])
  # if necessary, set footnote message for % error estimate
  if (!is.null(results[["ttestTableMessage"]])) jaspResults[["ttestContainer"]][["oneSampleTTestTable"]]$addFootnote(results[["ttestTableMessage"]])
  
  return(results)
}

# helper functions
.hypothesisType.summarystats.ttest.onesample <- function(hypothesis_option, bayesFactorType) {
  if (hypothesis_option == "notEqualToTestValue") {
    
    hypothesis   <- "twoSided"
    oneSided     <- FALSE
    nullInterval <- c(-Inf, Inf)
    message      <- NULL
    
  } else if (hypothesis_option == "greaterThanTestValue") {
    
    hypothesis   <- "plusSided"
    oneSided     <- "right"
    nullInterval <- c(0, Inf)
    message      <- "For all tests, the alternative hypothesis specifies that the mean is greater than 0."
    
  } else if (hypothesis_option == "lessThanTestValue") {
    
    hypothesis   <- "minSided"
    oneSided     <- "left"
    nullInterval <- c(-Inf, 0)
    message      <- "For all tests, the alternative hypothesis specifies that the mean is lesser than 0."
    
  }
  
  bfTitle      <- .getBayesfactorTitle.summarystats(bayesFactorType, hypothesis)
  
  return(list(hypothesis    = hypothesis,
              oneSided      = oneSided,
              message       = message,
              nullInterval  = nullInterval,
              bfTitle       = bfTitle)
  )
}

# Check commonsummarystatsttestbayesian.R for code that
  # (1) creates main table
  # (2) computes results for one sample t-test
  # (3) creates prior and posterior plot 
  # (4) creates robustness plot
  # (5) checks user input for errors