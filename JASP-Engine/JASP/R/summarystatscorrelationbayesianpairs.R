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

SummaryStatsCorrelationBayesianPairs <- function(jaspResults, dataset = NULL, options, ...) {
  
  # Reading in a datafile is not necessary
  # Error checking is not necessary
  
  # Compute the results
  summaryStatsCorrelationResults <- .summaryStatsCorrelationComputeResults(jaspResults, options)
  
  # # Output tables and plots
  .summaryStatsCorrelationTableMain(         jaspResults, options, summaryStatsCorrelationResults)
  # .summaryStatsCorrelationPriorPosteriorPlot(jaspResults, options, summaryStatsCorrelationResults)
  # .summaryStatsCorrelationRobustnessPlot(    jaspResults, options, summaryStatsCorrelationResults)
  
  return()
}

# Results functions ----
.summaryStatsCorrelationComputeResults <- function(jaspResults, options) {
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateSummaryStatsCorrelationResults"]])) 
    return(jaspResults[["stateSummaryStatsCorrelationResults"]]$object)
  
  # This will be the object that we fill with results
  results        <- list(hypothesisList                = list(),
                         correlationTable              = list(),
                         correlationPriorPosteriorPlot = list(),
                         correlationRobustnessPlot     = list())
  
  # Extract hypothesis
  hypothesisList <- .hypothesisType.summarystats.correlation(hypothesis = options$hypothesis, bayesFactorType = options$bayesFactorType)
  hypothesis     <- hypothesisList$hypothesis
  
  # Conduct frequentist and Bayesian correlation test
  correlationResults <- .calculateBF.summarystats.correlation(options)
  BF10               <- correlationResults$bf10
  
  BFlist       <- list(BF10    = BF10,
                       BF01    = 1/BF10,
                       LogBF10 = log(BF10))
  
  # Add results to results object
  results[["hypothesisList"]] <- hypothesisList
  results[["correlationTable"]] <- list(
    n        = options$sampleSize,
    corr     = correlationResults$stat,
    BF       = BFlist[[options$bayesFactorType]],
    pValue   = correlationResults$pValue[[hypothesis]]
  )
  # results[["correlationPriorPosteriorPlot"]] <- list(
  # t        = options$tStatistic,
  # n1       = options$n1Size,
  # oneSided = hypothesisList[["oneSided"]],
  # BF       = BFlist[[options$bayesFactorType]],
  # BFH1H0   = BFlist[["BF10"]]
  # rscale   = ,
  # delta    =
  # )
  # results[["correlationRobustnessPlot"]] <- list(
  #   a         = a,
  #   b         = b,
  #   successes = successes,
  #   n         = n,
  #   theta0    = theta0,
  #   BF        = BFlist
  # )
  
  # Save results to state
  defaultOptions <- c("correlationCoefficient", "pearsonRhoValue", "kendallTauValue",
                      "priorWidth", "bayesFactorType", "sampleSize", "hypothesis")
  jaspResults[["stateSummaryStatsCorrelationResults"]] <- createJaspState(results)
  jaspResults[["stateSummaryStatsCorrelationResults"]]$dependOn(defaultOptions)
  
  # Return results object
  return(results)
}

# Main table ----
.summaryStatsCorrelationTableMain <- function(jaspResults, options, summaryStatsCorrelationResults){
  if (!is.null(jaspResults[["CorrelationTTestTable"]])) return()
  
  tableResults    <- summaryStatsCorrelationResults[["correlationTable"]]
  
  # extract important parameters
  hypothesisList <- summaryStatsCorrelationResults[["hypothesisList"]]
  hypothesis     <- hypothesisList$hypothesis
  
  # create table and state dependencies
  if (options$correlationCoefficient == "pearsonRho") {
    
    tableTitle       <- "Bayesian Pearson Correlation"
    correlationTitle <- "r"
    
  } else if (options$correlationCoefficient == "kendallTau") {
    
    tableTitle        <- "Bayesian Kendall Correlation"
    correlationTitle <- "tau"
    
  }
  
  correlationTable <- createJaspTable(tableTitle)
  correlationTable$dependOn(optionsFromObject = jaspResults[["stateSummaryStatsCorrelationResults"]])
  correlationTable$position <- 1
  
  # set title for different Bayes factor types
  bfTitle        <- hypothesisList$bfTitle
  
  # set table citations and footnote message for different hypothesis types
  correlationTable$addCitation(.summaryStatsCitations[c("LyEtAl2016")])
  
  message <- hypothesisList$message
  if (!is.null(message)) correlationTable$addFootnote(message)
  
  correlationTable$addColumnInfo(name = "n"      , title = "n"             , type = "integer")
  correlationTable$addColumnInfo(name = "corr"   , title = correlationTitle, type = "number", format = "sf:4;dp:3")
  correlationTable$addColumnInfo(name = "BF"     , title = bfTitle         , type = "number", format = "sf:4;dp:3")
  correlationTable$addColumnInfo(name = "pValue" , title = "p"             , type = "number", format = "sf:4;dp:3")
  
  jaspResults[["correlationTable"]] <- correlationTable
  
  # extract rows from tableResults
  correlationTable$addRows(tableResults)
}

# helper functions
.calculateBF.summarystats.correlation <- function(options) {
  # Calculate the Bayes factors for correlation pairs
  #
  # Input:
  #     options: user options
  #
  # Ouput:
  #     list containing -
  #         bf: three Bayes factors (corresponding to all possible hypotheses)
  #         pValues: three p-values
  
  some.n   <- options$sampleSize
  BFObject <- list(BF10 = NA, BFPlus0 = NA, BFMin0 = NA)
  
  if (options$correlationCoefficient == "pearsonRho") {
    
    some.r     <- options$pearsonRhoValue
    BFObject   <- .bfPearsonCorrelation(n=some.n, r=some.r, kappa=options$priorWidth)
    allPValues <- .pValueFromCor(corrie=some.r, n=some.n, method="pearson")
    
  } else if (options$correlationCoefficient == "kendallTau") {
    
    some.r     <- options$kendallTauValue
    BFObject   <- .bfKendallTau(n=some.n, tauObs=some.r, kappa=options$priorWidth)
    allPValues <- .pValueFromCor(corrie=some.r, n=some.n, method="kendall")
    
  } else if (options$correlationCoefficient == "spearman"){
    # TODO: Johnny
    # Without code this will print a NULL, if we go through here
  }
  
  BFObject$pValue <- allPValues
  return(BFObject)
}

.hypothesisType.summarystats.correlation <- function(hypothesis_option, bayesFactorType) {
  if (hypothesis_option == "correlated") {
    
    hypothesis   <- "twoSided"
    oneSided     <- FALSE
    nullInterval <- c(-Inf, Inf)
    message      <- NULL
    
  } else if (hypothesis_option == "correlatedPositively") {
    
    hypothesis   <- "plusSided"
    oneSided     <- "right"
    nullInterval <- c(0, Inf)
    message      <- "For all tests, the alternative hypothesis specifies that the correlation is positive."
    
  } else if (hypothesis_option == "correlatedNegatively") {
    
    hypothesis   <- "minSided"
    oneSided     <- "left"
    nullInterval <- c(-Inf, 0)
    message      <- "For all tests, the alternative hypothesis specifies that the correlation is negative."
    
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