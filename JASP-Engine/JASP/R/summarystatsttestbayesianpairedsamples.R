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
  # Error checking is not necessary
  
  # Compute the results
  summaryStatsPairedSamplesResults <- .summaryStatsPairedSamplesComputeResults(jaspResults, options)
  
  # # Output tables and plots
  .summaryStatsPairedSamplesTableMain(         jaspResults, options, summaryStatsPairedSamplesResults)
  # Output plots 
  .ttestBayesianPriorPosteriorPlot.summarystats(jaspResults, summaryStatsOneSampleResults, options)
  .ttestBayesianPlotRobustness.summarystats(jaspResults, summaryStatsOneSampleResults, options)
  
  return()
}

# Execute Bayesian paired sample t-test ----
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
  jaspResults[["ttestContainer"]][["pairedSamplesTTestTable"]] <- .summaryStatsPairedSamplesTableMain(options, hypothesisList)

  if (!is.null(jaspResults[["ttestContainer"]][["stateSummaryStatsPairedSamplesResults"]])) {
    results <- jaspResults[["ttestContainer"]][["stateSummaryStatsPairedSamplesResults"]]$object
    # only change possible: BF type
    results[["pairedSamplesTTestTable"]][["BF"]] <- results[["BFlist"]][[options$bayesFactorType]]
  } else {
    results <- .summaryStatsOneSampleComputeResults(hypothesisList, options)
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

# Main table ----
.summaryStatsPairedSamplesTableMain <- function(jaspResults, options, summaryStatsPairedSamplesResults){
  if (!is.null(jaspResults[["PairedSamplesTTestTable"]])) return()
  
  tableResults    <- summaryStatsPairedSamplesResults[["ttestTable"]]
  
  # extract important parameters
  hypothesisList <- summaryStatsPairedSamplesResults[["hypothesisList"]]
  hypothesis     <- hypothesisList$hypothesis
  
  # create table and state dependencies
  pairedSamplesTTestTable <- createJaspTable("Bayesian Paired Samples T-Test")
  pairedSamplesTTestTable$dependOn(optionsFromObject = jaspResults[["stateSummaryStatsPairedSamplesResults"]])
  pairedSamplesTTestTable$position <- 1
  
  # set title for different Bayes factor types
  bfTitle        <- hypothesisList$bfTitle
  
  # set table citations and footnote message for different hypothesis types
  if (options$effectSizeStandardized == "default") {
    
    pairedSamplesTTestTable$addCitation(.summaryStatsCitations[c("MoreyRounder2015", "RounderEtAl2009")])
    
  } else if (options$effectSizeStandardized == "informative") {
    
    pairedSamplesTTestTable$addCitation(.summaryStatsCitations[c("GronauEtAl2017")])
    
  }
  
  message <- hypothesisList$message
  if (!is.null(message)) pairedSamplesTTestTable$addFootnote(message)
  
  pairedSamplesTTestTable$addColumnInfo(name = "t"      , title = "t"       , type = "number", format = "sf:4;dp:3")
  pairedSamplesTTestTable$addColumnInfo(name = "n1"     , title = "n"       , type = "integer")
  pairedSamplesTTestTable$addColumnInfo(name = "BF"     , title = bfTitle   , type = "number", format = "sf:4;dp:3")
  pairedSamplesTTestTable$addColumnInfo(name = "error"  , title = "error %" , type = "number", format = "sf:4;dp:3")
  pairedSamplesTTestTable$addColumnInfo(name = "pValue" , title = "p"       , type = "number", format = "sf:4;dp:3")
  
  jaspResults[["pairedSamplesTTestTable"]] <- pairedSamplesTTestTable
  
  # extract rows from tableResults
  pairedSamplesTTestTable$addRows(tableResults)
}

.summaryStatsOneSampleComputeResults <- function(hypothesisList, options) {
  
  # Extract important information from options list
  hypothesis <- hypothesisList$hypothesis
  t          <- options$tStatistic
  n1         <- options$n1Size
  
  # Checks before executing the analysis
  # 1. check user input
  ready <- !(n1 == 0)
  
  if (!ready)
    return(list(ready = ready))
  
  # Conduct frequentist and Bayesian independent samples t-test
  ttestResults <- .generalSummaryTtestBF(options = options)
  BF10         <- ttestResults$bf
  
  BFlist       <- list(BF10    = BF10,
                       BF01    = 1/BF10,
                       LogBF10 = log(BF10))
  
  # Add rows to the main table
  ttestTable <- list(
    t        = t,
    n1       = n1,
    BF       = BFlist[[options$bayesFactorType]],
    error    = ttestResults$properror,
    pValue   = ttestResults$pValue[[hypothesis]]
  )
  # check whether %error could be computed
  if(is.na(ttestTable$error) || is.null(ttestTable$error)){
    ttestTable$error   <- NaN
    ttestTableMessage  <- "Proportional error estimate could not be computed."
  } else {
    ttestTableMessage  <- NULL
  }
  
  # Add information for plots
  
  ttestPriorPosteriorPlot <- list(
    t        = t,
    n1       = n1,
    n2       = NULL,
    paired   = TRUE,
    oneSided = hypothesisList$oneSided,
    BF       = BFlist[[options$bayesFactorType]],
    BFH1H0   = BFlist[["BF10"]]
  )
  
  ttestRobustnessPlot <- list(
    t                     = t,
    n1                    = n1,
    n2                    = 0 ,
    paired                = FALSE,
    BF10user              = BFlist[["BF10"]],
    nullInterval          = hypothesisList$nullInterval,
    rscale                = options$priorWidth,
    oneSided              = hypothesisList$oneSided
  )
  
  # This will be the object that we fill with results
  results        <- list(
    hypothesisList          = hypothesisList,
    ttestPriorPosteriorPlot = ttestPriorPosteriorPlot,
    ttestRobustnessPlot     = ttestRobustnessPlot,
    ttestTable              = ttestTable,
    ttestTableMessage       = ttestTableMessage,
    ready                   = ready,
    BFlist                  = BFlist
  )
  
  # Return results object
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
  
  bfSubscripts <- .setBFsubscripts.summarystats(hypothesis)
  bfTitle      <- .getBayesfactorTitle.summarystats(bayesFactorType, hypothesis)
  
  return(list(hypothesis    = hypothesis,
              oneSided      = oneSided,
              message       = message,
              nullInterval  = nullInterval,
              bfSubscripts  = bfSubscripts,
              bfTitle       = bfTitle)
  )
}