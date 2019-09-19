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
  .summaryStatsOneSamplePriorPosteriorPlot(jaspResults, options, summaryStatsOneSampleResults)
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
  
  # Add information for plot 
  ttestPriorPosteriorPlot <- list(
    t        = t,
    n1       = n1,
    n2       = NULL,
    paired   = FALSE,
    oneSided = hypothesisList$oneSided,
    BF       = BFlist[[options$bayesFactorType]],
    BFH1H0   = BFlist[["BF10"]]
  )
  
  # ttestRobustnessPlot <- list(
  #   a         = a,
  #   b         = b,
  #   successes = successes,
  #   n         = n,
  #   theta0    = theta0,
  #   BF        = BFlist
  # )
  
  # This will be the object that we fill with results
  results        <- list(
    hypothesisList          = hypothesisList,
    ttestPriorPosteriorPlot = ttestPriorPosteriorPlot,
    # ttestRobustnessPlot     = ttestRobustnessPlot,
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

# Inferential plots ----
.summaryStatsTTestBayesianInferentialPlots <- function(jaspResults, options, summaryStatsOneSampleResults) {
  
  opts <- c("plotPriorAndPosterior", "plotBayesFactorRobustness")
  if (!any(unlist(options[opts])))
    return()
  
  if (is.null(jaspResults[["ttestContainer"]][["inferentialPlots"]])) {
    inferentialPlotsCollection <- createJaspContainer("Inferential Plots")
    inferentialPlotsCollection$dependOn(c("plotPriorAndPosterior", "plotPriorAndPosteriorAdditionalInfo", "plotBayesFactorRobustness"))
    jaspResults[["ttestContainer"]][["inferentialPlots"]] <- inferentialPlotsCollection
  } else {
    inferentialPlotsCollection <- jaspResults[["ttestContainer"]][["inferentialPlots"]]
  }
  
  # determine plot titles
  whichPlotTitles <- which(unlist(options[unlist(opts)]))
  
  # create all empty plots and containers before filling them in one-by-one, to avoid the screen from flashing
  dependencies <- list(
    c("plotPriorAndPosterior",     "plotPriorAndPosteriorAdditionalInfo"),
    c("plotBayesFactorRobustness", "plotBayesFactorRobustnessAdditionalInfo", "bayesFactorType")
  )
  
  plotTitles <- c("Prior and Posterior", "Bayes Factor Robustness Check")
  jaspTitles <- c("plotPriorAndPosterior", "plotRobustness")
    
    for (i in whichPlotTitles) { # add empty plot in the container for this variable
      if (is.null(inferentialPlotsCollection[[jaspTitles[i]]])) {
        plot <- createJaspPlot(title = plotTitles[i], width = 530, height = 400)
        plot$dependOn(options = dependencies[[i]])
        plot$position <- i
        inferentialPlotsCollection[[jaspTitles[i]]] <- plot
      }
    }
  
  if (!ttestResults[["ready"]])
    return()
  
  hypothesisList <- summaryStatsOneSampleResults[["hypothesisList"]]
  
  if (options[["plotPriorAndPosterior"]]) {
    
    priorPosteriorInfo    <- summaryStatsOneSampleResults[["ttestPriorPosteriorPlot"]]
    
    obj <- try(.plotPriorPosterior(
      t                      = priorPosteriorInfo$t,
      n1                     = priorPosteriorInfo$n1,
      n2                     = priorPosteriorInfo$n2,
      paired                 = priorPosteriorInfo$paired,
      oneSided               = priorPosteriorInfo$oneSided,
      BF                     = priorPosteriorInfo$BF,
      BFH1H0                 = priorPosteriorInfo$BFH1H0,
      rscale                 = options$informativeCauchyScale, 
      delta                  = options$informativeCauchyLocation,
      addInformation         = options$plotPriorAndPosteriorAdditionalInfo,
      options                = options
    ))
    if (isTryError(obj)) {
      plot$setError(.extractErrorMessage(obj))
    } else {
      plot$plotObject <- obj
    }
  }
  
  if (options[["plotBayesFactorRobustness"]]) {
    
    robustnessInfo <- summaryStatsOneSampleResults[["ttestRobustnessPlot"]]
    
    obj <- try(.plotBF.robustnessCheck.ttest2(
      x                     = group1,
      y                     = group2,
      BF10post              = robustnessInfo$BF10post,
      paired                = robustnessInfo$paired,
      oneSided              = hypothesisList$oneSided,
      nullInterval          = hypothesisList$nullInterval,
      rscale                = rscale,
      BFH1H0                = BFH1H0,
      additionalInformation = additionalInformation,
      ...
    ))
    if (isTryError(obj)) {
      plot$setError(.extractErrorMessage(obj))
    } else {
      plot$plotObject <- obj
    }
  }
  
}

.summaryStatsOneSampleRobustnessPlot <- function(jaspResults, options, summaryStatsOneSampleResults) {
  
  if (!options$plotBayesFactorRobustness)
    return()
  
  plot <- createJaspPlot(
    title       = "Prior and Posterior",
    width       = 530,
    height      = 400,
    aspectRatio = 0.7
  )
  plot$position <- 2
  plot$dependOn(options = c("plotPriorAndPosterior, plotPriorAndPosteriorAdditionalInfo"))
  jaspResults[["ttestContainer"]][["priorPosteriorPlot"]] <- plot
  
  if (!summaryStatsOneSampleResults[["ready"]] || jaspResults[["ttestContainer"]]$getError())
    return()
  
  plotResults    <- summaryStatsOneSampleResults[["ttestPriorPosteriorPlot"]]
  hypothesisList <- summaryStatsOneSampleResults[["hypothesisList"]]
  
  # Prior and posterior plot
  if(options$plotPriorAndPosterior) {
    
    p <- .plotPriorPosterior(
      t                      = plotResults$t,
      n1                     = plotResults$n1,
      n2                     = plotResults$n2,
      paired                 = plotResults$paired,
      oneSided               = plotResults$oneSided,
      BF                     = plotResults$BF,
      BFH1H0                 = plotResults$BFH1H0,
      rscale                 = options$informativeCauchyScale, 
      delta                  = options$informativeCauchyLocation,
      addInformation         = options$plotPriorAndPosteriorAdditionalInfo,
      options                = options
    )
  }
  
  # create JASP object
  plot$plotObject <- p
  return()
}

# Robustness plot ----
.plotBF.robustnessCheck.ttest.summarystats <- function(
  x = NULL, y = NULL, paired = FALSE, BF10post, nullInterval, formula = NULL, data = NULL, rscale = 1, oneSided = FALSE,
  BFH1H0 = TRUE, additionalInformation = FALSE) {
  
  r <- .ttestBayesianGetRScale(rscale)
  
  if (r > 1.5) {
    rValues <- seq(0.0005, 2.0, length.out = 535)
  } else {
    rValues <- seq(0.0005, 1.5, length.out = 400)
  }
  
  # compute BF10
  BF10                    <- vector("numeric", length(rValues)
  options_robustness_plot <- options 
  for (i in seq_along(rValues)) {
    options_robustness_plot$informativeCauchyScale <- rValues[i]
    BF10[i] <- .generalSummaryTtestBF(options = options, paired = FALSE)$bf
  }
  
  # maximum BF value
  idx <- which.max(BF10)
  maxBF10 <- BF10[idx]
  maxBFrVal <- rValues[idx]
  
  # # sometimes BayesFactor fails and returns NaN, e.g. BayesFactor::ttest.tstat(t = 10, n1 = 5, n2 = 0, nullInterval = NULL, rscale = 0.0005)
  # # we'll remove up to 5% of the NaN's and otherwise just return an error for the plot
  # validValues <- is.finite(BF10)
  # if (sum(validValues) < (0.95 * length(BF10)))
  #   stop("could not calculate enough valid Bayes Factors for different values of the prior")
  # 
  # BF10 <- BF10[validValues]
  # rValues <- rValues[validValues]
  # 
  # # add BF10 = 1 for r = 0
  # rValues <- c(0, rValues)
  # BF10 <- c(1, BF10)
  # 
  # # maximum BF value
  # maxBF10 <- max(BF10)
  # maxBFrVal <- rValues[which.max(BF10)]
  # BF10maxText <- .clean(maxBF10)
  # 
  # # BF10 "medium" prior
  # BF10m <- BayesFactor::ttest.tstat(t = t, n1 = n1, n2 = n2, nullInterval = nullInterval,
  #                                   rscale = "medium")
  # BF10m <- .clean(exp(BF10m$bf))
  # BF10mText <- BF10m
  # 
  # # BF10 "wide" prior
  # BF10w <- BayesFactor::ttest.tstat(t = t, n1 = n1, n2 = n2, nullInterval = nullInterval,
  #                                   rscale = "wide")
  # BF10w <- .clean(exp(BF10w$bf))
  # BF10wText <- BF10w
  # 
  # # BF10 "ultrawide" prior
  # BF10ultra <- BayesFactor::ttest.tstat(t = t, n1 = n1, n2 = n2, nullInterval = nullInterval,
  #                                       rscale = "ultrawide")
  # BF10ultra <- .clean(exp(BF10ultra$bf))
  # BF10ultraText <- BF10ultra
  # 
  # # BF10 user prior
  # BF10user <- BF10post
  # BF10userText <- BF10user
  
  if (isFALSE(oneSided)) {
    
    # BF10 "medium" prior
    BF10m     <- BayesFactor::ttestBF(x = x, y = y, paired = paired, nullInterval = nullInterval, rscale = "medium")
    BF10w     <- BayesFactor::ttestBF(x = x, y = y, paired = paired, nullInterval = nullInterval, rscale = "wide")
    BF10ultra <- BayesFactor::ttestBF(x = x, y = y, paired = paired, nullInterval = nullInterval, rscale = "ultrawide")
    
    BF10m     <- BayesFactor::extractBF(BF10m,     logbf = FALSE, onlybf = FALSE)[1L, "bf"]
    BF10w     <- BayesFactor::extractBF(BF10w,     logbf = FALSE, onlybf = FALSE)[1L, "bf"]
    BF10ultra <- BayesFactor::extractBF(BF10ultra, logbf = FALSE, onlybf = FALSE)[1L, "bf"]
    
  } else {
    
    # BF10 "medium" prior
    BF10m     <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, oneSided = oneSided, r = "medium")
    BF10w     <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, oneSided = oneSided, r = "wide")
    BF10ultra <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, oneSided = oneSided, r = "ultrawide")
  }
  
  # BF10 user prior
  # BF10user <- if (BFH1H0) BF10post else 1 / BF10post
  
  dfLines <- data.frame(
    x = rValues,
    y = log(BF10)
  )
  
  BF10user <- BF10post
  if (BFH1H0) {
    bfType <- "BF10"
  } else {
    bfType <- "BF01"
    dfLines$y <- -dfLines$y
    BF10user  <- 1 / BF10user
    maxBF10   <- 1 / maxBF10
    BF10w     <- 1 / BF10w
    BF10ultra <- 1 / BF10ultra
  }
  
  BFsubscript <- .ttestBayesianGetBFnamePlots(BFH1H0, nullInterval)
  
  # to mimic old behavior  
  # getBFSubscript <- function(x) .ttestBayesianGetBFnamePlots(x <= 1, nullInterval)
  # getBFValue     <- function(x) if (x <= 1) 1 / x else x
  dfPoints <- data.frame(
    x = c(maxBFrVal, r, 1, sqrt(2)),
    y = log(c(maxBF10, BF10user, BF10w, BF10ultra)),
    g = JASPgraphs::parseThis(c(
      sprintf("paste(max, ~%s, ':',   phantom(phollll), %s, ~at, ~'r'==%s)", BFsubscript, format(maxBF10,   digits = 4), format(maxBFrVal, digits = 4)),
      sprintf("paste(user~prior, ':', phantom(phll[0]), ~%s==%s)",           BFsubscript, format(BF10user,  digits = 4)),
      sprintf("paste(wide~prior, ':', phantom(ph[0][0]), ~%s==%s)",          BFsubscript, format(BF10w,     digits = 4)),
      sprintf("paste(ultrawide~prior, ':', ~%s==%s)",                        BFsubscript, format(BF10ultra, digits = 4))
    )),
    stringsAsFactors = FALSE
  )
  
  hypothesis <- switch(oneSided,
                       "right" = "greater",
                       "left"  = "smaller",
                       "equal"
  )
  
  plot <- JASPgraphs::PlotRobustnessSequential(
    dfLines      = dfLines,
    dfPoints     = dfPoints,
    pointLegend  = additionalInformation,
    xName        = "Cauchy prior width",
    hypothesis   = hypothesis,
    bfType       = bfType
  )
  
  return(plot)
  
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