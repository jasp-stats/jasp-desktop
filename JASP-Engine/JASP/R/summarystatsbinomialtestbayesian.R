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

SummaryStatsBinomialTestBayesian <- function(jaspResults, dataset = NULL, options, ...) {
  
  # Reading in a datafile is not necessary
  # Check for possible errors
  .checkErrorsSummaryStatsBinomial(options)
  
  # testValue is a formulaField: parse it and save the result in the state
  options <- .parseAndStoreFormulaOptions(jaspResults, options, c("testValue"))

  # Compute the results and create main results table
  summaryStatsBinomialResults <- .summaryStatsBinomialMainFunction(jaspResults, options)
  
  # Output plots
  .summaryStatsBinomialPlot(jaspResults, options, summaryStatsBinomialResults)
  
  return()
}

# Execute Bayesian binomial test ----
.summaryStatsBinomialMainFunction <- function(jaspResults, options) {
  
  # This function is the main workhorse, and also makes the table
  container <- jaspResults[["binomialContainer"]]
  if (is.null(container)) {
    container <- createJaspContainer(dependencies=c("successes", "failures", "betaPriorParamA", "betaPriorParamB", "testValue", "hypothesis"))
    jaspResults[["binomialContainer"]] <- container
  }
  
  # If table already exists in the state, return it
  if (!is.null(container[["bayesianBinomialTable"]]))
    return(container[["stateSummaryStatsBinomialResults"]]$object)
  
  # Otherwise: create the empty table before executing the analysis
  hypothesisList        <- .hypothesisTypeSummaryStatsBinomial(options$hypothesis, options$testValueUnparsed, options$bayesFactorType)
  container[["bayesianBinomialTable"]] <- .summaryStatsBinomialTableMain(options, hypothesisList)
  
  if (!is.null(container[["stateSummaryStatsBinomialResults"]])) {
    results <- container[["stateSummaryStatsBinomialResults"]]$object
    # only change possible: BF type
    results[["binomTable"]][["BF"]] <- results[["BFlist"]][[options$bayesFactorType]]
  } else {
    results <- .summaryStatsBinomialComputeResults(hypothesisList, options)
    # Save results to state
    container[["stateSummaryStatsBinomialResults"]] <- createJaspState(results)
    
    if (!is.null(results[["errorMessageTable"]]))
      container[["bayesianBinomialTable"]]$setError(results[["errorMessageTable"]])
  }
  
  #  fill table if ready
  if (results[["ready"]])
    container[["bayesianBinomialTable"]]$setData(results[["binomTable"]])

  return(results)
}

.summaryStatsBinomialComputeResults <- function(hypothesisList, options) {
  
  # Extract important information from options list
  hypothesis <- hypothesisList$hypothesis
  a          <- options$betaPriorParamA
  b          <- options$betaPriorParamB
  successes  <- options$successes
  failures   <- options$failures
  n          <- successes + failures
  theta0     <- options$testValue
  
  # Checks before executing the analysis
  # 1. check user input
  ready <- !(n == 0)
  
  if (!ready)
    return(list(ready = ready))
  
  # Conduct frequentist and Bayesian binomial test
  pValue <- stats::binom.test(x = successes, n = n, p = theta0, alternative = hypothesis)$p.value
  # if p-value cannot be computed, return NA
  if(!is.numeric(pValue)) pValue <- NaN
  BF10   <- .bayesBinomialTest(counts = successes, n = n, theta0 = theta0, hypothesis = hypothesis, a = a, b = b)
  
  BFlist <- list(BF10    = BF10,
                 BF01    = 1/BF10,
                 LogBF10 = log(BF10))
  
  # Add rows to the main table
  binomTable <- list(
    successes = successes,
    failures  = failures,
    theta0    = theta0,
    BF        = BFlist[[options$bayesFactorType]],
    pValue    = pValue
  )
  
  # Add information for plot
  binomPlot <- list(
    a         = a,
    b         = b,
    successes = successes,
    n         = n,
    theta0    = theta0,
    BF        = BFlist
  )
  # This will be the object that we fill with results
  results        <- list(
    hypothesisList = hypothesisList,
    binomPlot      = binomPlot,
    binomTable     = binomTable
  )
  results[["ready"]] <- ready
  results[["BFlist"]] <- BFlist
  
  # Return results object
  return(results)
}

# Main table ----
.summaryStatsBinomialTableMain <- function(options, hypothesisList){
  
  # create table and state dependencies
  bayesianBinomialTable <- createJaspTable(gettext("Bayesian Binomial Test"))
  bayesianBinomialTable$dependOn("bayesFactorType")
  bayesianBinomialTable$position <- 1
  bayesianBinomialTable$info <- gettext("- *Bayes factor*: If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is larger than the test value
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is smaller than the test value
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population mean is larger
   than the test value
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that that the population mean is
  smaller than the test value
  - **p**: p-value corresponding to t-statistic.")

  
  # set title for different Bayes factor types
  bfTitle <- hypothesisList$bfTitle

  # set table citations and footnote message for different hypothesis types
  bayesianBinomialTable$addCitation(.summaryStatsCitations[c("Jeffreys1961", "OHagan2004", "Haldane1932")])
  
  message <- hypothesisList$message
  if (!is.null(message)) bayesianBinomialTable$addFootnote(message)
  
  bayesianBinomialTable$addColumnInfo(name = "successes", title = gettext("Successes") , type = "integer")
  bayesianBinomialTable$addColumnInfo(name = "failures" , title = gettext("Failures")  , type = "integer")
  bayesianBinomialTable$addColumnInfo(name = "theta0"   , title = gettext("Test value"), type = "number")
  bayesianBinomialTable$addColumnInfo(name = "BF"       , title = bfTitle,				 type = "number")
  bayesianBinomialTable$addColumnInfo(name = "pValue"   , title = gettext("p")         , type = "pvalue")
  
  return(bayesianBinomialTable)

}

# Prior and Posterior plot ----
.summaryStatsBinomialPlot <- function(jaspResults, options, summaryStatsBinomialResults) {
  
  if (!options[["plotPriorAndPosterior"]])
    return()
  
  plot <- createJaspPlot(
    title       = gettext("Prior and Posterior"),
    width       = 530,
    height      = 400,
    aspectRatio = 0.7
  )
  plot$position <- 2
  plot$dependOn(options = c("plotPriorAndPosterior", "plotPriorAndPosteriorAdditionalInfo"))
  jaspResults[["binomialContainer"]][["priorPosteriorPlot"]] <- plot
  
  if (!summaryStatsBinomialResults[["ready"]] || jaspResults[["binomialContainer"]]$getError())
    return()
  
  plotResults    <- summaryStatsBinomialResults[["binomPlot"]]
  hypothesisList <- summaryStatsBinomialResults[["hypothesisList"]]
  hypothesis     <- hypothesisList$hypothesis
  hypForPlots    <- .binomHypothesisForPlots(hypothesis)
  
  # extract parameters needed for prior and posterior plot
  a         <- plotResults$a
  b         <- plotResults$b
  successes <- plotResults$successes
  n         <- plotResults$n
  theta0    <- plotResults$theta0
  BF10      <- plotResults$BF[["BF10"]]
  
  # Prior and posterior plot
  quantiles       <- .credibleIntervalPlusMedian(credibleIntervalInterval = .95, a, b, successes, n, hyp = hypothesis, theta0 = theta0)
  medianPosterior <- quantiles$ci.median
  CIlower         <- quantiles$ci.lower
  CIupper         <- quantiles$ci.upper
  
  # error check: Posterior too peaked?
  if(abs(CIlower - CIupper) <= .Machine$double.eps){
    plot$setError(gettext("Plotting not possible: Posterior too peaked!"))
    return()
  }
  
  ppCri           <- c(CIlower, CIupper)
  dfLinesPP       <- .dfLinesPP(dataset = NULL, a = a, b = b, hyp = hypothesis, theta0 = theta0, counts = successes, n = n)
  dfPointsPP      <- .dfPointsPP(dataset = NULL, a = a, b = b, hyp = hypothesis, theta0 = theta0, counts = successes, n = n)
  xName <- bquote(paste(.(gettext("Population proportion")), ~theta))
  
  # error check: Cannot evaluate prior or posterior density?
  if(any(is.na(c(dfPointsPP$y, dfLinesPP$y))) || any(is.infinite(c(dfPointsPP$y, dfLinesPP$y)))){
    plot$setError(gettext("Plotting not possible: Cannot evaluate prior or posterior density!"))
    return()
  }
  
  if(options$plotPriorAndPosteriorAdditionalInfo){
    
    # error check: infinite Bayes factors?
    if(!is.numeric(BF10) || is.infinite(BF10)){
      plot$setError(gettext("Plotting not possible: Bayes factor should be numeric!"))
      return()
    }
    
    p <- try(JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName, BF = BF10,
                                               bfType = "BF10", hypothesis = hypForPlots,
                                               CRI = ppCri, median = medianPosterior, drawCRItxt = TRUE))
  } 
  else {
    p <- try(JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName))
  }
  
  # create JASP object
  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    plot$setError(errorMessage)
  } else {
    plot$plotObject <- p
  }
  return()
}

# helper functions
.hypothesisTypeSummaryStatsBinomial <- function(hypothesis_option, theta0, bayesFactorType) {
  if (hypothesis_option == "notEqualToTestValue") {
    
    hypothesis_for_common_functions   <- "twoSided"
    hypothesis                        <- "two.sided"
    message <- gettextf("Proportions tested against value: %s.", theta0)
    
  } else if (hypothesis_option == "greaterThanTestValue") {
    
    hypothesis_for_common_functions   <- "plusSided"
    hypothesis                        <- "greater"
    message <- gettextf("For all tests, the alternative hypothesis specifies that the proportion is greater than %s.", theta0)
    
  } else if (hypothesis_option == "lessThanTestValue") {
    
    hypothesis_for_common_functions   <- "minSided"
    hypothesis                        <- "less"
    message <- gettextf("For all tests, the alternative hypothesis specifies that the proportion is less than %s.", theta0)
  }
  
  bfTitle      <- .getBayesfactorTitleSummaryStats(bayesFactorType, hypothesis_for_common_functions)
  
  hypothesisList <- list(hypothesis    = hypothesis,
                          message      = message,
                          bfTitle      = bfTitle)
  
  return(hypothesisList)
}
.checkErrorsSummaryStatsBinomial <- function(options) {
  
  # perform a check on the hypothesis
  custom <- function() {
    if (options$testValue == 1 && options$hypothesis == "greaterThanTestValue")
      return(gettext("Cannot test the hypothesis that the test value is greater than 1."))
    else if (options$testValue == 0 && options$hypothesis == "lessThanTestValue")
      return(gettext("Cannot test the hypothesis that the test value is less than 0."))
  }
  
  # Error Check 1: Number of levels of the variables and the hypothesis
  .hasErrors(
    dataset              = matrix(options$successes), # mock dataset so the error check runs
    custom               = custom,
    exitAnalysisIfErrors = TRUE
  )

}
