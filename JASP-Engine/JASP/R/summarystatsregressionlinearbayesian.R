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

SummaryStatsRegressionLinearBayesian <- function(jaspResults, dataset = NULL, options, ...) {
  
  # Reading in a datafile is not necessary
  
  # Error checking 
  .summarystatsRegressionCheckErrors(options)
  
  # Compute the results
  summaryStatsRegressionResults <- .summaryStatsRegressionComputeResults(jaspResults, options)
  
  # # Output tables and plots
  .summaryStatsRegressionTableMain(         jaspResults, options, summaryStatsRegressionResults)
  # .summaryStatsRegressionRobustnessPlot(    jaspResults, options, summaryStatsRegressionResults)
  
  return()
}

# Results functions ----
.summaryStatsRegressionComputeResults <- function(jaspResults, options) {
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateSummaryStatsRegressionResults"]])) 
    return(jaspResults[["stateSummaryStatsRegressionResults"]]$object)
  
  # This will be the object that we fill with results
  results        <- list(tableInfo                = list(),
                         regressionTable          = list(),
                         regressionRobustnessPlot = list())
  
  # Run linear regression
  N             <- options$sampleSize
  rScale        <- options$priorWidth
  nCovariatesH0 <- options$numberOfCovariatesNull
  rSquaredH0    <- options$unadjustedRSquaredNull
  nCovariatesH1 <- options$numberOfCovariatesAlternative
  rSquaredH1    <- options$unadjustedRSquaredAlternative
  
  # Extract relevant information
  tableInfo          <- .tableInfo.summarystats.regression(options)
  nullModelSpecified <- tableInfo$nullModelSpecified
  
  # Conduct Bayesian linear regression for H1
  regressionResultsH1 <- BayesFactor::linearReg.R2stat(N = N, p = nCovariatesH1, R2 = rSquaredH1, rscale = rScale)
  LogBF10_H1          <- regressionResultsH1$bf
  BFlist_H1           <- list(BF10    = exp(LogBF10_H1),
                              BF01    = 1/exp(LogBF10_H1),
                              LogBF10 = LogBF10_H1)
  
  # Add results to results object
  results[["tableInfo"]]       <- tableInfo
  results[["regressionTable"]] <- list(
    sampleSize        = N,
    nCovariates       = nCovariatesH1,
    R2                = rSquaredH1,
    BF                = BFlist_H1[[options$bayesFactorType]],
    error             = regressionResultsH1[["properror"]]
  )
  
  # If specified: conduct Bayesian linear regression for H0
  if(nullModelSpecified) {
    
    regressionResultsH0 <- BayesFactor::linearReg.R2stat(N = N, p = nCovariatesH0, R2 = rSquaredH0, rscale = rScale)
    LogBF10_H0          <- regressionResultsH0$bf
    BFlist_H0           <- list(BF10    = exp(LogBF10_H0),
                                BF01    = 1/exp(LogBF10_H0),
                                LogBF10 = LogBF10_H0)
    
    # Add results to results object
    results[["regressionTable"]] <- data.frame(
      sampleSize        = c("Null model"                        , "Alternative model"),
      nCovariates       = c(nCovariatesH0                       , nCovariatesH1),
      R2                = c(rSquaredH0                          , rSquaredH1),
      BF                = c(BFlist_H0[[options$bayesFactorType]], BFlist_H1[[options$bayesFactorType]]),
      error             = c(regressionResultsH0[["properror"]]  , regressionResultsH1[["properror"]])
    )
    
  }
  
  # results[["regressionRobustnessPlot"]] <- list(
  #   a         = a,
  #   b         = b,
  #   successes = successes,
  #   n         = n,
  #   theta0    = theta0,
  #   BF        = BFlist
  # )
  
  # Save results to state
  defaultOptions <- c("priorWidth", "bayesFactorType", "sampleSize",
                      "unadjustedRSquaredNull"       , "numberOfCovariatesNull", 
                      "unadjustedRSquaredAlternative", "numberOfCovariatesAlternative")
  jaspResults[["stateSummaryStatsRegressionResults"]] <- createJaspState(results)
  jaspResults[["stateSummaryStatsRegressionResults"]]$dependOn(defaultOptions)
  
  # Return results object
  return(results)
}

# Main table ----
.summaryStatsRegressionTableMain <- function(jaspResults, options, summaryStatsRegressionResults){
  if (!is.null(jaspResults[["regressionTable"]])) return()
  
  tableResults    <- summaryStatsRegressionResults[["regressionTable"]]
  
  # extract important parameters
  tableInfo        <- summaryStatsRegressionResults[["tableInfo"]]
  mainResultsTitle <- tableInfo$mainResultsTitle
  
  # create table and state dependencies
  regressionTable <- createJaspTable(mainResultsTitle)
  regressionTable$dependOn(optionsFromObject = jaspResults[["stateSummaryStatsRegressionResults"]])
  regressionTable$position <- 1
  
  # set title for different Bayes factor types
  sampleSizeTitle    <- tableInfo$sampleSizeTitle
  sampleSizeCellType <- tableInfo$sampleSizeCellType
  bfTitle            <- tableInfo$bfTitle
  
  # set table citations and footnote message for different hypothesis types
  regressionTable$addCitation(.summaryStatsCitations[c("LiangEtAl2008", "RounderMoreyInPress")])
  
  
  message <- tableInfo$message
  if (!is.null(message)) regressionTable$addFootnote(message)
  
  regressionTable$addColumnInfo(name = "sampleSize" , title = sampleSizeTitle       , type = sampleSizeCellType)
  regressionTable$addColumnInfo(name = "nCovariates", title = "Number of covariates", type = "integer")
  regressionTable$addColumnInfo(name = "R2"         , title = "R\u00B2"             , type = "number", format = "dp:3")
  regressionTable$addColumnInfo(name = "BF"         , title = bfTitle               , type = "number", format = "sf:4;dp:3")
  regressionTable$addColumnInfo(name = "error"      , title = "error %"             , type = "number", format = "sf:4;dp:3")
  
  jaspResults[["regressionTable"]] <- regressionTable
  
  # extract rows from tableResults
  regressionTable$addRows(tableResults)
}

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
.tableInfo.summarystats.regression <- function(options) {
  
  # set footnote message and Bayes factor title
  message      <- paste0("r scale used is: ", options$priorWidth, ".")
  bfTitle      <- .getBayesfactorTitle.summarystats(options$bayesFactorType, hypothesis = 'twoSided')
  
  # determine title for main results table
  nullModelSpecified <- TRUE
  if(options$numberOfCovariatesNull==0 && options$unadjustedRSquaredNull==0) nullModelSpecified <- FALSE 
  
  if(nullModelSpecified) {
    
    mainResultsTitle   <- "Model Comparison"
    sampleSizeTitle    <- paste0("n = ", options$sampleSize)
    sampleSizeCellType <- "string"

    
  } else {
    
    mainResultsTitle  <- "Bayesian Linear Regression"
    sampleSizeTitle   <- "n"
    sampleSizeCellType <- "integer"
    
  }
  
  return(list(message            = message,
              bfTitle            = bfTitle,
              mainResultsTitle   = mainResultsTitle,
              sampleSizeTitle    = sampleSizeTitle,
              sampleSizeCellType = sampleSizeCellType,
              nullModelSpecified = nullModelSpecified)
  )
}
.summarystatsRegressionCheckErrors <- function(options){
  
  # check if number of covariates is correct in H1
  if(options$numberOfCovariatesAlternative!=0 && options$sampleSize!=0 && ((options$sampleSize - options$numberOfCovariatesAlternative) < 2)) {
    
    .quitAnalysis("Number of Covariates must be less than N-1 (sample size minus 1)")
    
  }
  
  # check if number of covariates is correct in H0
  if(options$numberOfCovariatesNull==0 && options$unadjustedRSquaredNull==0) {
    
    if(options$numberOfCovariatesNull!=0 && options$sampleSize!=0 && ((options$sampleSize - options$numberOfCovariatesNull) < 2)) {
    
      .quitAnalysis("Number of Covariates must be less than N-1 (sample size minus 1)")
      
    }
  } 
  
  # check if R squared input is correct
  if((options$numberOfCovariatesAlternative > options$numberOfCovariatesNull) && (options$unadjustedRSquaredAlternative < options$unadjustedRSquaredNull)) {
    
    .quitAnalysis("Input: When number of covariates for Alternative hypothesis is greater than that of Null hypothesis, the R\u00B2 has to be higher under Alternative than under Null hypothesis")
    
  }
  
}