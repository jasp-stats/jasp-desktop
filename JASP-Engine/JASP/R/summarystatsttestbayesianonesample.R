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
  # Error checking is not necessary
  
  # Compute the results
  summaryStatsOneSampleResults <- .summaryStatsOneSampleMainFunction(jaspResults, options)
  
  # Output plots
  .summaryStatsTTestBayesianInferentialPlots(jaspResults, options, summaryStatsOneSampleResults)
  # .summaryStatsOneSamplePriorPosteriorPlot(jaspResults, options, summaryStatsOneSampleResults)
  # .summaryStatsOneSampleRobustnessPlot(    jaspResults, options, summaryStatsOneSampleResults)
  
  return()
}

# Execute Bayesian one sample t-test ----
.summaryStatsOneSampleMainFunction <- function(jaspResults, options) {
  
  # Save results to state
  defaultOptions <- c("tStatistic", "n1Size", "hypothesis", "bayesFactorType",           # standard entries
                      "priorWidth", "effectSizeStandardized",                            # default prior
                      "informativeCauchyLocation", "informativeCauchyScale",             # informed cauchy priors
                      "informativeNormalMean", "informativeNormalStd",                   # informed normal priors
                      "informativeTLocation", "informativeTScale", "informativeTDf"      # informed t-distribution
  )
  
  # This function is the main workhorse, and also makes the table
  if (is.null(jaspResults[["ttestContainer"]])) {
    jaspResults[["ttestContainer"]] <- createJaspContainer()
    jaspResults[["ttestContainer"]]$dependOn(defaultOptions)
  }
  
  # If table already exists in the state, return it
  if (!is.null(jaspResults[["ttestContainer"]][["oneSampleTTestTable"]]))
    return(jaspResults[["ttestContainer"]][["stateSummaryStatsOneSampleResults"]]$object)
  
  # Otherwise: create the empty table before executing the analysis
  hypothesisList <- .hypothesisType.summarystats.ttest.onesample(hypothesis = options$hypothesis, bayesFactorType = options$bayesFactorType)
  jaspResults[["ttestContainer"]][["oneSampleTTestTable"]] <- .summaryStatsOneSampleTableMain(options, hypothesisList)
  
  if (!is.null(jaspResults[["ttestContainer"]][["stateSummaryStatsOneSampleResults"]])) {
    results <- jaspResults[["ttestContainer"]][["stateSummaryStatsOneSampleResults"]]$object
    # only change possible: BF type
    results[["oneSampleTTestTable"]][["BF"]] <- results[["BFlist"]][[options$bayesFactorType]]
  } else {
    results <- .summaryStatsOneSampleComputeResults(hypothesisList, options)
    # Save results to state
    jaspResults[["ttestContainer"]][["stateSummaryStatsBinomialResults"]] <- createJaspState(results)
    
    if (!is.null(results[["errorMessageTable"]]))
      jaspResults[["ttestContainer"]][["oneSampleTTestTable"]]$setError(results[["errorMessageTable"]])
  }
  
  #  fill table if ready
  if (results[["ready"]])
    jaspResults[["ttestContainer"]][["oneSampleTTestTable"]]$setData(results[["ttestTable"]])
  
  return(results)
}

.summaryStatsOneSampleComputeResults <- function(hypothesisList, options) {
  
  # Extract important information from options list
  hypothesis <- hypothesisList$hypothesis
  t          <- options$tStatistic
  n1         <- options$n1Size
  
  # Checks before executing the analysis
  # 1. check user input
  ready <- !(n1 == 0) && !(t ==0)
  
  if (!ready)
    return(list(ready = ready))
  
  # 2. check for possible errors
  errorMessageTable <- NULL
  
  if (n1 == 1) {
    
    errorMessageTable <- "Not enough observations."
    
  }
  
  if (!is.null(errorMessageTable)) {
    return(list(ready = ready, errorMessageTable = errorMessageTable))
  }
  
  # Conduct frequentist and Bayesian independent samples t-test
  ttestResults <- .generalSummaryTtestBF(options = options, paired = FALSE)
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
  
  # Add information for plots
  
  ttestPriorPosteriorPlot <- list(
    t        = t,
    n1       = n1,
    n2       = NULL,
    paired   = FALSE,
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
    oneSided              = hypothesisList$oneSided,
    isInformative         = (options$effectSizeStandardized == "informative"),
    additionalInformation = options$plotBayesFactorRobustnessAdditionalInfo
  )
  
  # This will be the object that we fill with results
  results        <- list(
    hypothesisList          = hypothesisList,
    ttestPriorPosteriorPlot = ttestPriorPosteriorPlot,
    ttestRobustnessPlot     = ttestRobustnessPlot,
    ttestTable              = ttestTable
  )
  results[["ready"]] <- ready
  results[["BFlist"]] <- BFlist
  
  # Return results object
  return(results)
}

# Main table ----
.summaryStatsOneSampleTableMain <- function(options, hypothesisList){
  
  # create table and state dependencies
  oneSampleTTestTable <- createJaspTable("Bayesian One Sample T-Test")
  oneSampleTTestTable$dependOn("bayesFactorType")
  oneSampleTTestTable$position <- 1
  
  # set title for different Bayes factor types
  bfTitle        <- hypothesisList$bfTitle
  
  # set table citations and footnote message for different hypothesis types
  if (options$effectSizeStandardized == "default") {
    
    oneSampleTTestTable$addCitation(.summaryStatsCitations[c("MoreyRounder2015", "RounderEtAl2009")])
    
  } else if (options$effectSizeStandardized == "informative") {
    
    oneSampleTTestTable$addCitation(.summaryStatsCitations[c("GronauEtAl2017")])
    
  }
  
  message <- hypothesisList$message
  if (!is.null(message)) oneSampleTTestTable$addFootnote(message)
  
  oneSampleTTestTable$addColumnInfo(name = "t"      , title = "t"       , type = "number", format = "sf:4;dp:3")
  oneSampleTTestTable$addColumnInfo(name = "n1"     , title = "n"       , type = "integer")
  oneSampleTTestTable$addColumnInfo(name = "BF"     , title = bfTitle   , type = "number", format = "sf:4;dp:3")
  oneSampleTTestTable$addColumnInfo(name = "error"  , title = "error %" , type = "number", format = "sf:4;dp:3")
  oneSampleTTestTable$addColumnInfo(name = "pValue" , title = "p"       , type = "number", format = "sf:4;dp:3")
  
  return(oneSampleTTestTable)

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