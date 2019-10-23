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

SummaryStatsTTestBayesianPairedSamples <- function(jaspResults, dataset = NULL, options, ...) {
  
  # Reading in a datafile is not necessary
  # Check user input for possible errors
  .checkErrors.summarystats.onesample.pairedsamples(options)
  
  # Compute the results and create main results table
  summaryStatsPairedSamplesResults <- .summaryStatsPairedSamplesMainFunction(jaspResults, options)
  # Output plots 
  .ttestBayesianPriorPosteriorPlot.summarystats(jaspResults, summaryStatsPairedSamplesResults, options)
  .ttestBayesianPlotRobustness.summarystats(jaspResults, summaryStatsPairedSamplesResults, options)
  
  return()
}

# Execute Bayesian paired samples t-test ----
.summaryStatsPairedSamplesMainFunction <- function(jaspResults, options) {
  
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
  if (!is.null(jaspResults[["ttestContainer"]][["pairedSamplesTTestTable"]]))
    return(jaspResults[["ttestContainer"]][["stateSummaryStatsPairedSamplesResults"]]$object)
  
  # Otherwise: create the empty table before executing the analysis
  hypothesisList <- .hypothesisType.summarystats.ttest.pairedsamples(hypothesis = options$hypothesis, bayesFactorType = options$bayesFactorType)
  jaspResults[["ttestContainer"]][["pairedSamplesTTestTable"]] <- .summaryStatsOneSamplePairedSamplesTableMain(options, 
                                                                                                               hypothesisList, 
                                                                                                               title = "Bayesian Paired Samples T-Test")

  if (!is.null(jaspResults[["ttestContainer"]][["stateSummaryStatsPairedSamplesResults"]])) {
    results <- jaspResults[["ttestContainer"]][["stateSummaryStatsPairedSamplesResults"]]$object
    # only change possible: BF type
    results[["pairedSamplesTTestTable"]][["BF"]] <- results[["BFlist"]][[options$bayesFactorType]]
  } else {
    results <- .summaryStatsOneSamplePairedSamplesComputeResults(hypothesisList, options)
    # Save results to state
    jaspResults[["ttestContainer"]][["stateSummaryStatsPairedSamplesResults"]] <- createJaspState(results)
    
    if (!is.null(results[["errorMessageTable"]]))
      jaspResults[["ttestContainer"]][["pairedSamplesTTestTable"]]$setError(results[["errorMessageTable"]])
  }
  
  #  fill table if ready
  if (results[["ready"]])
    jaspResults[["ttestContainer"]][["pairedSamplesTTestTable"]]$setData(results[["ttestTable"]])
  # if necessary, set footnote message for % error estimate
  if (!is.null(results[["ttestTableMessage"]])) jaspResults[["ttestContainer"]][["pairedSamplesTTestTable"]]$addFootnote(results[["ttestTableMessage"]])
  
  return(results)
}

# helper functions
.hypothesisType.summarystats.ttest.pairedsamples <- function(hypothesis_option, bayesFactorType) {
  if (hypothesis_option == "groupsNotEqual") {
    
    hypothesis   <- "twoSided"
    oneSided     <- FALSE
    nullInterval <- c(-Inf, Inf)
    message      <- NULL
    
  } else if (hypothesis_option == "groupOneGreater") {
    
    hypothesis   <- "plusSided"
    oneSided     <- "right"
    nullInterval <- c(0, Inf)
    message      <- "For all tests, the alternative hypothesis specifies that measure 1 is greater than measure 2."
    
  } else if (hypothesis_option == "groupTwoGreater") {
    
    hypothesis   <- "minSided"
    oneSided     <- "left"
    nullInterval <- c(-Inf, 0)
    message      <- "For all tests, the alternative hypothesis specifies that measure 1 is lesser than measure 2."
    
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