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
  # .summaryStatsPairedSamplesPriorPosteriorPlot(jaspResults, options, summaryStatsPairedSamplesResults)
  # .summaryStatsPairedSamplesRobustnessPlot(    jaspResults, options, summaryStatsPairedSamplesResults)
  
  return()
}

# Results functions ----
.summaryStatsPairedSamplesComputeResults <- function(jaspResults, options) {
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateSummaryStatsPairedSamplesResults"]])) 
    return(jaspResults[["stateSummaryStatsPairedSamplesResults"]]$object)
  
  # This will be the object that we fill with results
  results        <- list(hypothesisList          = list(),
                         ttestTable              = list(),
                         ttestPriorPosteriorPlot = list(),
                         ttestRobustnessPlot     = list())
  
  # Extract hypothesis
  hypothesisList <- .hypothesisType.summarystats.ttest.pairedsamples(hypothesis = options$hypothesis, bayesFactorType = options$bayesFactorType)
  hypothesis     <- hypothesisList$hypothesis
  
  # Conduct frequentist and Bayesian independent samples t-test
  ttestResults <- .generalSummaryTtestBF(options = options, paired = TRUE)
  BF10         <- ttestResults$bf
  
  BFlist       <- list(BF10    = BF10,
                       BF01    = 1/BF10,
                       LogBF10 = log(BF10))
  
  # Add results to results object
  results[["hypothesisList"]] <- hypothesisList
  results[["ttestTable"]] <- list(
    t        = options$tStatistic,
    n1       = options$n1Size,
    BF       = BFlist[[options$bayesFactorType]],
    error    = ttestResults$properror,
    pValue   = ttestResults$pValue[[hypothesis]]
  )
  # results[["ttestPriorPosteriorPlot"]] <- list(
  # t        = options$tStatistic,
  # n1       = options$n1Size,
  # oneSided = hypothesisList[["oneSided"]],
  # BF       = BFlist[[options$bayesFactorType]],
  # BFH1H0   = BFlist[["BF10"]]
  # rscale   = ,
  # delta    =
  # )
  # results[["ttestRobustnessPlot"]] <- list(
  #   a         = a,
  #   b         = b,
  #   successes = successes,
  #   n         = n,
  #   theta0    = theta0,
  #   BF        = BFlist
  # )
  
  # Save results to state
  defaultOptions <- c("tStatistic", "n1Size", "hypothesis", "bayesFactorType",           # standard entries
                      "priorWidth", "effectSizeStandardized",                            # default prior
                      "informativeCauchyLocation", "informativeCauchyScale",             # informed cauchy priors
                      "informativeNormalMean", "informativeNormalStd",                   # informed normal priors
                      "informativeTLocation", "informativeTScale", "informativeTDf"      # informed t-distribution
  )
  jaspResults[["stateSummaryStatsPairedSamplesResults"]] <- createJaspState(results)
  jaspResults[["stateSummaryStatsPairedSamplesResults"]]$dependOn(defaultOptions)
  
  # Return results object
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

# # Prior and Posterior plot ----
# .summaryStatsPairedSamplesPriorPosteriorPlot <- function(jaspResults, options, summaryStatsBinomialResults) {
#   
#   plotResults     <- summaryStatsPairedSamplesResults[["ttestPriorPosteriorPlot"]]
#   hypothesisList  <- summaryStatsPairedSamplesResults[["hypothesisList"]]
#   
#   # extract parameters needed for prior and posterior plot
#   
#   # Prior and posterior plot
#   if(options$plotPriorAndPosterior) {
#     
#       p <- .plotPriorPosterior(
#         t                      = plotResults$t,
#         n1                     = plotResults$n1,
#         n2                     = plotResults$n2,
#         paired                 = paired,
#         oneSided               = plotResults$oneSided,
#         BF                     = plotResults$BF,
#         BFH1H0                 = plotResults$BFH1H0,
#         rscale                 = rscale,
#         delta                  = delta,
#         addInformation         = options$plotPriorAndPosteriorAdditionalInfo,
#         wilcoxTest             = wilcoxTest,
#         options                = options,
#         ...
#       )
#     } 
#     
#     # create JASP object
#     plot <- createJaspPlot(
#       title       = "Prior and Posterior",
#       width       = 530,
#       height      = 400,
#       plot        = p,
#       aspectRatio = 0.7
#     )
#     plot$position <- 2
#     plot$dependOn(optionsFromObject = jaspResults[["stateSummaryStatsBinomialResults"]], 
#                   options           = c("plotPriorAndPosterior, plotPriorAndPosteriorAdditionalInfo"))
#     jaspResults[["priorPosteriorPlot"]] <- plot
# }

# # Robustness plot ----
# .summaryStatsBinomialPlot <- function(jaspResults, options, summaryStatsBinomialResults) {
#   
#   plotResults <- summaryStatsBinomialResults[["binomPlot"]]
#   hypothesis  <- summaryStatsBinomialResults[["hypothesis"]]
#   
#   if (hypothesis == "two.sided") {
#     bfSubscripts <- "BF[1][0]"
#   }
#   else if (hypothesis == "greater"){
#     bfSubscripts <- "BF['+'][0]"
#   }
#   else if (hypothesis == "less"){
#     bfSubscripts <- "BF['-'][0]"
#   }
#   
#   # extract parameters needed for prior and posterior plot
#   a         <- plotResults$a
#   b         <- plotResults$b
#   successes <- plotResults$successes
#   n         <- plotResults$n
#   theta0    <- plotResults$theta0
#   BF10      <- plotResults$BF[["BF10"]]
#   
#   # Prior and posterior plot
#   if(options$plotPriorAndPosterior) {
#     quantiles       <- .credibleIntervalPlusMedian(credibleIntervalInterval = .95, a, b, successes, n, hyp = hypothesis, theta0 = theta0)
#     medianPosterior <- quantiles$ci.median
#     CIlower         <- quantiles$ci.lower
#     CIupper         <- quantiles$ci.upper
#     ppCri           <- c(CIlower, CIupper)
#     dfLinesPP       <- .dfLinesPP( a = a, b = b, hyp = hypothesis, theta0 = theta0, counts = successes, n = n)
#     dfPointsPP      <- .dfPointsPP(a = a, b = b, hyp = hypothesis, theta0 = theta0, counts = successes, n = n)
#     xName           <- expression(paste("Population proportion ", theta))
#     
#     if(options$plotPriorAndPosteriorAdditionalInfo){
#       p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName, BF01 = 1/BF10,
#                                              CRI = ppCri, median = medianPosterior, drawCRItxt = TRUE, bfSubscripts = bfSubscripts)
#     } 
#     else {
#       p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName, bfSubscripts = bfSubscripts)
#     }
#     
#     # create JASP object
#     plot <- createJaspPlot(
#       title       = "Prior and Posterior",
#       width       = 530,
#       height      = 400,
#       plot        = p,
#       aspectRatio = 0.7
#     )
#     plot$position <- 2
#     plot$dependOn(optionsFromObject = jaspResults[["stateSummaryStatsBinomialResults"]], 
#                   options           = c("plotPriorAndPosterior, plotPriorAndPosteriorAdditionalInfo"))
#     jaspResults[["priorPosteriorPlot"]] <- plot
#   }
# }

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