#
# Copyright (C) 2018 University of Amsterdam
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

.pValueFromCor <- function(corrie, n, method="pearson") {
  # Function returns the p value from correlation, thus,
  ##  corrie = r    when  method = "pearson"
  #   corrie = tau  when  method = "kendall"
  #   corrie = rho  when  method = "spearman"
  #
  # Args:
  #   corrie: correlation input by user
  #   n: sample size
  #   oneSided: hypothesis type: left or right
  #   method: pearson, kenall, or spearman
  #
  # Output:
  #   list of three p-values

  result <- list()

  if (n <= 2){
      # Given NULL or NA result

      result$twoSided <- NA
      # tau < 0
      result$minSided <- NA
      # tau > 0
      result$plusSided <- NA
      return(result)
  }

  if (method == "pearson"){
      # Use t-distribution based on bivariate normal assumption using r to t transformation
      #
      df <- n - 2
      t <- corrie*sqrt(df/(1-corrie^2))
      result <- .pValueFromT(t=t, n1=n-1, n2=0, var.equal=TRUE)
  } else if (method == "kendall"){
      if (n > 2 && n < 50) {
          # Exact sampling distribution
          # tau neq 0
          result$twoSided <- 1 - SuppDists::pKendall(q=abs(corrie), N=n) + SuppDists::pKendall(q=-abs(corrie), N=n)
          # tau < 0
          result$minSided <- SuppDists::pKendall(q=corrie, N=n)
          # tau > 0
          result$plusSided <- SuppDists::pKendall(q=corrie, N=n, lower.tail = FALSE)
      } else if (n >= 50){
          # normal approximation
          #
          someSd <- sqrt(2*(2*n+5)/(9*n*(n-1)))

          # tau neq 0
          result$twoSided <- 2 * stats::pnorm(-abs(corrie), sd=someSd)
          # tau < 0
          result$minSided <- stats::pnorm(corrie, sd=someSd)
          # tau > 0
          result$plusSided <- stats::pnorm(corrie, sd=someSd, lower.tail = FALSE)
      }
  } else if (method == "spearman"){
      # TODO: Johnny
      # Without code this will print a NULL, if we go through here
  }
  return(result)
}

# Conduct Bayesian T-Test BF from Summary Statistic

# TODO(raoul): - Change to combined one-sample/two-sample/dependent t-test function
#              - Add uniform informed prior

.generalSummaryTtestBF <- function(tValue=options$tStatistic, size=options$n1Size, options, paired=TRUE) {
  # Converts a t-statistic and sample size into the corresponding Bayes Factor.
  #
  # Args:
  #   tValue:  the value of the t-statistic to be converted
  #   size:    the sample size underlying the t-statistic
  #   options: options object passed from JASP
  #
  # Value:
  #   list with components:
  #     bf:         the Bayes Factor
  #     properror:  percentage of error in BF estimate
  #     tValue:     the input t-statistic
  #     n1:         the sample size
  #     pValue:     p-value associated with tValue and n1

  # help vars
  n1       <- size
  n2       <- if (!is.null(options$n2Size)) options$n2Size else 0 # single sample case
  oneSided <- !(options$hypothesis %in% c("notEqualToTestValue","groupsNotEqual"))

  ### Default case: a non-informative zero-centered Cauchy prior
  if(options$effectSizeStandardized == "default") {
    nullInterval <-

      switch(options$hypothesis,
        greaterThanTestValue  = c(0, Inf),
        groupOneGreater       = c(0, Inf),
        lessThanTestValue     = c(-Inf,0),
        groupTwoGreater       = c(-Inf, 0),
                                c(-Inf,Inf))    # default is notEqualToTestValue

    bfObject <- BayesFactor::ttest.tstat(t=tValue, n1=n1, n2=n2, rscale=options$priorWidth,
                                         nullInterval = nullInterval)
    bf <- exp(bfObject$bf)
    error <- 100*bfObject$properror
  }

  ### Informed prior case: non-central scaled Cauchy, Student t, or Normal (uniform is lacking?)
  if (options$effectSizeStandardized == "informative") {
    # Note that strictly speaking, in case of the independent samples t-test,
    # for the informed prior n1 corresponds to n1 and n2 to n2 and not vice-versa.
    # However, since in the expression for the Bayes factor they only appear
    # as an "effective" sample size and in the degrees of freedom for which it does
    # not matter whether we swap the two, we retain this order for easier extension
    # of the one-sample case.

    side = switch(options$hypothesis, greaterThanTestValue = "right", groupOneGreater = "right",
                  lessThanTestValue= "left", groupTwoGreater = "left", FALSE)

    # Note: .bf10_ functions gives weired value if paired = FALSE in single sample case
    if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
      bfObject <- .bf10_t(t = tValue, n1 = n1, n2 = n2, oneSided = side,
                          independentSamples = !paired,
                          prior.location = options[["informativeCauchyLocation"]],
                          prior.scale = options[["informativeCauchyScale"]],
                          prior.df = 1)
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "t") {
      bfObject <- .bf10_t(t = tValue, n1 = n1, n2 = n2, oneSided = side,
                          independentSamples = !paired,
                          prior.location = options[["informativeTLocation"]],
                          prior.scale = options[["informativeTScale"]],
                          prior.df = options[["informativeTDf"]])
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
      bf <- .bf10_normal(t = tValue, n1 = n1, n2 = n2, oneSided = side,
                         independentSamples = !paired,
                         prior.mean = options[["informativeNormalMean"]],
                         prior.variance = options[["informativeNormalStd"]]^2)
      error <- NULL
    }
  }
  result <- list(bf = bf, properror = error, tValue = tValue, n1 = n1,
                 pValue = .pValueFromT(t=tValue, n1=n1, n2=n2))
  return(result)
}
.pValueFromT <- function(t, n1, n2 = 0, var.equal = TRUE) {
  # Function returns the p value from t statistic
  #
  # Args:
  #   t: t value input by user
  #   n1: sample size of group 1
  #   n2: sample size of group 2 (Note the hack by setting n2 = 0)
  #   var.equal: Note: always true: var.equal, we do not have enough info for different
  #              variances. In that case we also need s1 and s2
  #
  # Output:
  #   number in [0, 1] which is the p value

  result <- list()

  if (n2 > 0) {
    # If n2 > 0, then two-sample
    someDf <- n1 + n2 - 2
  } else {
    # If n2 <= 0, then one-sample
    someDf <- n1 - 1
  }

  # mu \neq 0
  result$twoSided <- 2 * stats::pt(-abs(t), df = someDf)
  # mu < 0
  result$minSided <- stats::pt(t, df = someDf)
  # mu > 0
  result$plusSided <- stats::pt(t, df = someDf, lower.tail = FALSE)

  return(result)
}

# Execute Bayesian T-Test
.summaryStatsTTestMainFunction <- function(jaspResults, options, analysis) {

  # This function is the main workhorse, and also makes the table
  container <- jaspResults[["ttestContainer"]]
  if (is.null(container)) {
    container <- createJaspContainer()
    # add dependencies for main table (i.e., when does it have to recompute values for the main table)
    container$dependOn(c("tStatistic", "n1Size", "n2Size", "hypothesis", "bayesFactorType",            # standard entries
                         "defaultStandardizedEffectSize" , "informativeStandardizedEffectSize",        # informative or default
                         "priorWidth"                    , "effectSizeStandardized",                   # default prior
                         "informativeCauchyLocation"     , "informativeCauchyScale",                   # informed cauchy priors
                         "informativeNormalMean"         , "informativeNormalStd"  ,                   # informed normal priors
                         "informativeTLocation"          , "informativeTScale"     , "informativeTDf"  # informed t-distribution
    ))
    jaspResults[["ttestContainer"]] <- container
  }

  # If table already exists in the state, return it
  if (!is.null(container[["ttestTable"]]))
    return(container[["stateSummaryStatsTTestResults"]]$object)

  # Otherwise: create the empty table before executing the analysis
  hypothesisList <- .hypothesisTypeSummaryStatsTTest(options$hypothesis, options$bayesFactorType, analysis)

  container[["ttestTable"]] <- .summaryStatsTTestTableMain(options, hypothesisList)

  if (!is.null(container[["stateSummaryStatsTTestResults"]])) {
    results <- container[["stateSummaryStatsTTestResults"]]$object
  } else {
    results <- .summaryStatsTTestComputeResults(hypothesisList, options, analysis)
    # Save results to state
    container[["stateSummaryStatsTTestResults"]] <- createJaspState(results)

    if (!is.null(results[["errorMessageTable"]]))
      container[["ttestTable"]]$setError(results[["errorMessageTable"]])
  }

  #  fill table if ready
  if (results[["ready"]])
    container[["ttestTable"]]$setData(results[["ttestTableData"]])
  # if necessary, set footnote message for % error estimate
  if (!is.null(results[["ttestTableMessage"]])) container[["ttestTable"]]$addFootnote(results[["ttestTableMessage"]], colNames = "error")

  return(results)
}

# Create main table
.summaryStatsTTestTableMain <- function(options, hypothesisList){

  # create table and state dependencies
  title      <- hypothesisList$tableTitle
  ttestTable <- createJaspTable(title)
  ttestTable$dependOn("bayesFactorType")
  ttestTable$position <- 1

  # set title for different Bayes factor types
  bfTitle        <- hypothesisList$bfTitle

  # set table citations and footnote message for different hypothesis types
  if (options$effectSizeStandardized == "default") {

    ttestTable$addCitation(.summaryStatsCitations[c("MoreyRounder2015", "RounderEtAl2009")])

  } else if (options$effectSizeStandardized == "informative") {

    ttestTable$addCitation(.summaryStatsCitations[c("GronauEtAl2017")])

  }

  message <- hypothesisList$message
  if (!is.null(message)) ttestTable$addFootnote(message)
  
  ttestTable$addColumnInfo(name = "t"      , title = gettext("t")       , type = "number")
  
  if(title == gettext("Bayesian Independent Samples T-Test")) { #This check might mess up with translations, but what to do about it?
    
    ttestTable$addColumnInfo(name = "n1"   , title = gettextf("n%s", "\u2081") , type = "integer")
    ttestTable$addColumnInfo(name = "n2"   , title = gettextf("n%s", "\u2082") , type = "integer")
    
  } else {
    
    ttestTable$addColumnInfo(name = "n1"   , title = gettext("n")       , type = "integer")
    
  }
  
  ttestTable$addColumnInfo(name = "BF"     , title = bfTitle            , type = "number")
  ttestTable$addColumnInfo(name = "error"  , title = gettext("error %") , type = "number")
  ttestTable$addColumnInfo(name = "pValue" , title = gettext("p")       , type = "pvalue")
  
  return(ttestTable)

}

# Compute Results
.summaryStatsTTestComputeResults <- function(hypothesisList, options, analysis) {

  # Extract important information from options list
  hypothesis <- hypothesisList$hypothesis
  t          <- options$tStatistic
  n1         <- options$n1Size

  # Checks before executing the analysis
  # 1. check user input
  if(analysis == "oneSample" || analysis == "pairedSamples"){

    ready               <- !(n1 == 0)
    isPairedOrOneSample <- TRUE

  } else if(analysis == "independentSamples"){

    n2                  <- options$n2Size
    ready               <- !(n1 == 0 || n2 == 0)
    isPairedOrOneSample <- FALSE

  }

  if (!ready)
    return(list(ready = ready))

  # Conduct frequentist and Bayesian t-test
  ttestResults <- .generalSummaryTtestBF(options = options, paired = isPairedOrOneSample)
  BF10         <- ttestResults$bf

  BFlist       <- list(BF10    = BF10,
                       BF01    = 1/BF10,
                       LogBF10 = log(BF10))

  # Add rows to the main table
  ttestTableData <- list(t = t, n1 = n1)
  if(analysis == "independentSamples")
    ttestTableData$n2 <- n2
  ttestTableData$BF     <- BFlist[[options$bayesFactorType]]
  ttestTableData$error  <- ttestResults$properror
  ttestTableData$pValue <- ttestResults$pValue[[hypothesis]]

  # check whether % error could be computed
  if(is.na(ttestTableData$error) || is.null(ttestTableData$error)){
    ttestTableData$error <- NaN
    ttestTableMessage    <- gettext("Proportional error estimate could not be computed.")
  } else {
    ttestTableMessage  <- NULL
  }

 # Add BF10 or BF01 label for plots; never show the log Bayes factor in the plots (it interferes with the pie charts)
  if (options$bayesFactorType == "BF01")
    bfPlotsH1H0 <- FALSE
  else 
    bfPlotsH1H0 <- TRUE


  ttestPriorPosteriorPlot <- list(
    t        = t,
    n1       = n1,
    n2       = NULL,
    paired   = TRUE,
    oneSided = hypothesisList$oneSided,
    BF       = BFlist[["BF10"]],
    BFH1H0   = bfPlotsH1H0
  )

  ttestRobustnessPlot <- list(
    t                         = t,
    n1                        = n1,
    n2                        = 0 ,
    paired                    = FALSE,
    BF10user                  = BFlist[["BF10"]],
    yAxisLegendRobustnessPlot = bfPlotsH1H0,
    nullInterval              = hypothesisList$nullInterval,
    rscale                    = options$priorWidth,
    oneSided                  = hypothesisList$oneSided
  )

  if(analysis == "independentSamples"){

    ttestPriorPosteriorPlot$n2 <- n2
    ttestRobustnessPlot$n2     <- n2

  }

  # This will be the object that we fill with results
  results        <- list(
    hypothesisList          = hypothesisList,
    ttestPriorPosteriorPlot = ttestPriorPosteriorPlot,
    ttestRobustnessPlot     = ttestRobustnessPlot,
    ttestTableData          = ttestTableData,
    ttestTableMessage       = ttestTableMessage,
    ready                   = ready,
    BFlist                  = BFlist
  )

  # Return results object
  return(results)
}

# Prior & Posterior plot
.ttestBayesianPriorPosteriorPlotSummaryStats <- function(jaspResults, summaryStatsTTestResults, options){

  if (!options[["plotPriorAndPosterior"]] || !is.null(jaspResults[["ttestContainer"]][["priorPosteriorPlot"]]))
    return()

  plot <- createJaspPlot(
    title       = gettext("Prior and Posterior"),
    width       = 530,
    height      = 400,
    aspectRatio = 0.7
  )
  plot$position <- 2
  # when do we need to draw the plot again
  plot$dependOn(options = c("plotPriorAndPosterior", "plotPriorAndPosteriorAdditionalInfo"))
  jaspResults[["ttestContainer"]][["priorPosteriorPlot"]] <- plot

  if (!summaryStatsTTestResults[["ready"]] || jaspResults[["ttestContainer"]]$getError())
    return()

  # Prior and posterior plot
  priorPosteriorInfo <- summaryStatsTTestResults[["ttestPriorPosteriorPlot"]]
  p <- try(.plotPriorPosterior(
    t                      = priorPosteriorInfo$t,
    n1                     = priorPosteriorInfo$n1,
    n2                     = priorPosteriorInfo$n2,
    paired                 = priorPosteriorInfo$paired,
    oneSided               = priorPosteriorInfo$oneSided,
    BF                     = priorPosteriorInfo$BF,
    BFH1H0                 = priorPosteriorInfo$BFH1H0,
    rscale                 = options$priorWidth,
    addInformation         = options$plotPriorAndPosteriorAdditionalInfo,
    options                = options
  ))

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    plot$setError(errorMessage)
  } else {
    plot$plotObject <- p
  }
  return()
}

# Bayes FactorRobustness Check plot
.ttestBayesianPlotRobustnessSummaryStats <- function(jaspResults, summaryStatsTTestResults, options){

  if (!options[["plotBayesFactorRobustness"]] || !is.null(jaspResults[["ttestContainer"]][["BayesFactorRobustnessPlot"]]))
    return()

  plot <- createJaspPlot(
    title       = gettext("Bayes Factor Robustness Check"),
    width       = 530,
    height      = 400,
    aspectRatio = 0.7
  )
  plot$position <- 3
  plot$dependOn(options = c("plotBayesFactorRobustness", "plotBayesFactorRobustnessAdditionalInfo", "bayesFactorType"))
  jaspResults[["ttestContainer"]][["BayesFactorRobustnessPlot"]] <- plot

  if (!summaryStatsTTestResults[["ready"]] || jaspResults[["ttestContainer"]]$getError())
    return()

  robustnessInfo <- summaryStatsTTestResults[["ttestRobustnessPlot"]]
  hypothesisList <- summaryStatsTTestResults[["hypothesisList"]]

  # error check: Informative prior?
  if ((options$effectSizeStandardized == "informative")) {
    plot$setError(gettext("Plotting not possible: Bayes factor robustness check plot currently not supported for informed prior."))
    return()
  }

  # Bayes Factor Robustness Check plot
  p <- try(.plotBFRobustnessCheckSummaryStatsTTest(
    t                     = robustnessInfo$t,
    n1                    = robustnessInfo$n1,
    n2                    = robustnessInfo$n2,
    paired                = robustnessInfo$paired,
    BF10user              = robustnessInfo$BF10user,
    bfType                = robustnessInfo$yAxisLegendRobustnessPlot,
    nullInterval          = hypothesisList$nullInterval,
    rscale                = robustnessInfo$rscale,
    oneSided              = hypothesisList$oneSided,
    isInformative         = robustnessInfo$isInformative,
    additionalInformation = options$plotBayesFactorRobustnessAdditionalInfo
  ))

  if (isTryError(p)) {
    errorMessage <- gettextf("Plotting not possible: %s", .extractErrorMessage(p))
    plot$setError(errorMessage)
  } else {
    plot$plotObject <- p
  }
  return()
}
.plotBFRobustnessCheckSummaryStatsTTest <- function(t, n1, n2, paired = FALSE, BF10user, bfType = "BF10", nullInterval, rscale = 0.707, oneSided = FALSE,
                                                       isInformative = FALSE, additionalInformation = FALSE) {

  if (rscale > 1.5) {
    rValues <- seq(0.0005, 2.0, length.out = 535)
  } else {
    rValues <- seq(0.0005, 1.5, length.out = 400)
  }

  # compute BF10
  BF10 <- vector("numeric", length(rValues))
  for (i in seq_along(rValues)) {
    BF10[i] <- BayesFactor::ttest.tstat(t = t, n1 = n1, n2 = n2, nullInterval = nullInterval, rscale = rValues[i])$bf
  }

  if (sum(is.na(BF10)) > 100){
    stop(gettext("Bayes factors could not be computed for 100 values of the prior."))
  }

  # maximum BF value
  idx       <- which.max(BF10)
  maxBF10   <- exp(BF10[idx])
  maxBFrVal <- rValues[idx]

  # BF10 prior
  BF10m     <- BayesFactor::ttest.tstat(t = t, n1 = n1, n2 = n2, nullInterval = nullInterval, rscale = "medium")$bf
  BF10w     <- BayesFactor::ttest.tstat(t = t, n1 = n1, n2 = n2, nullInterval = nullInterval, rscale = "wide")$bf
  BF10ultra <- BayesFactor::ttest.tstat(t = t, n1 = n1, n2 = n2, nullInterval = nullInterval, rscale = "ultrawide")$bf

  BF10m     <- .clean(exp(BF10m))
  BF10w     <- .clean(exp(BF10w))
  BF10ultra <- .clean(exp(BF10ultra))

  dfLines <- data.frame(
    x = rValues,
    y = BF10
  )

  dfLines <- na.omit(dfLines)

  BFH1H0 <- !(bfType == "BF01")
  if (!BFH1H0) {
    dfLines$y <- - dfLines$y
    BF10user  <- 1 / BF10user
    idx       <- which.max(1/BF10)
    maxBF10   <- exp(BF10[idx])
    maxBFrVal <- rValues[idx]
    BF10w     <- 1 / BF10w
    BF10ultra <- 1 / BF10ultra
  }

  BFsubscript <- .ttestBayesianGetBFnamePlots(BFH1H0, nullInterval, subscriptsOnly = TRUE)

  if(additionalInformation){

    label1 <- c(
      gettextf("max BF%s", BFsubscript),
      gettext("user prior"),
      gettext("wide prior"),
      gettext("ultrawide prior")
    )
    # some failsafes to parse translations as expressions
    label1[1] <- gsub(pattern = "\\s+", "~", label1[1])
    label1[-1] <- paste0("\"", label1[-1], "\"")
    label1 <- paste0("paste(", label1, ", ':')")

    BFandSubscript <- gettextf("BF%s", BFsubscript)
    BFandSubscript <- gsub(pattern = "\\s+", "~", BFandSubscript)
    label2 <- c(
      gettextf("%s at r==%s",      format(maxBF10,  digits = 4), format(maxBFrVal, digits = 4)),
      paste0(BFandSubscript, "==", format(BF10user, digits = 4)),
      paste0(BFandSubscript, "==", format(BF10w,    digits = 4)),
      paste0(BFandSubscript, "==", format(BF10ultra,digits = 4))
    )
    label2[1L] <- gsub(pattern = "\\s+", "~", label2[1])

    dfPoints <- data.frame(
      x = c(maxBFrVal, rscale, 1, sqrt(2)),
      y = log(c(maxBF10, BF10user, BF10w, BF10ultra)),
      g = label1,
      label1 = JASPgraphs::parseThis(label1),
      label2 = JASPgraphs::parseThis(label2),
      stringsAsFactors = FALSE
    )
  } else {
    dfPoints <- NULL
  }

  hypothesis <- switch(oneSided,
                       "right" = "greater",
                       "left"  = "smaller",
                       "equal"
  )

  p <- JASPgraphs::PlotRobustnessSequential(
    dfLines      = dfLines,
    dfPoints     = dfPoints,
    pointLegend  = additionalInformation,
    xName        = gettext("Cauchy prior width"),
    hypothesis   = hypothesis,
    bfType       = bfType
  )

  return(p)

}

# helper functions for One Sample and Paired Samples T-Test
.hypothesisTypeSummaryStatsTTest <- function(hypothesis_option, bayesFactorType, analysis) {

  if (hypothesis_option == "groupsNotEqual" || hypothesis_option == "notEqualToTestValue") {

    hypothesis   <- "twoSided"
    oneSided     <- FALSE
    nullInterval <- c(-Inf, Inf)
    message      <- NULL

  } else if (hypothesis_option == "groupOneGreater" || hypothesis_option == "greaterThanTestValue") {

    hypothesis   <- "plusSided"
    oneSided     <- "right"
    nullInterval <- c(0, Inf)

    message <- switch (analysis,
                       "independentSamples" = gettext("For all tests, the alternative hypothesis specifies that group 1 is greater than group 2."),
                       "oneSample"          = gettext("For all tests, the alternative hypothesis specifies that the mean is greater than 0."),
                       "pairedSamples"      = gettext("For all tests, the alternative hypothesis specifies that measure 1 is greater than measure 2.")
    )

  } else if (hypothesis_option == "groupTwoGreater" || hypothesis_option == "lessThanTestValue") {

    hypothesis   <- "minSided"
    oneSided     <- "left"
    nullInterval <- c(-Inf, 0)

    message <- switch (analysis,
                          "independentSamples" = gettext("For all tests, the alternative hypothesis specifies that group 1 is less than group 2."),
                          "oneSample"          = gettext("For all tests, the alternative hypothesis specifies that the mean is less than 0."),
                          "pairedSamples"      = gettext("For all tests, the alternative hypothesis specifies that measure 1 is less than measure 2.")
    )

  }

  # Set Table Title
  tableTitle <- switch (analysis,
    "independentSamples" = gettext("Bayesian Independent Samples T-Test"),
    "oneSample"          = gettext("Bayesian One Sample T-Test"),
    "pairedSamples"      = gettext("Bayesian Paired Samples T-Test")
  )

  bfTitle      <- .getBayesfactorTitleSummaryStats(bayesFactorType, hypothesis)

  return(list(hypothesis    = hypothesis,
              oneSided      = oneSided,
              message       = message,
              nullInterval  = nullInterval,
              bfTitle       = bfTitle,
              tableTitle    = tableTitle)
  )
}
.checkErrorsSummaryStatsTTest <- function(options, analysis) {

  # perform a check on the hypothesis
  if(analysis == "oneSample" || analysis == "pairedSamples"){

    custom <- function() {
      if (options$n1Size == 1)
        return(gettext("Not enough observations."))
    }

  } else {

    custom <- function() {
      if (options$n1Size == 1 || options$n2Size == 1)
        return(gettext("Not enough observations."))
    }

  }

  # Error Check 1: Number of levels of the variables and the hypothesis
  .hasErrors(
    dataset              = matrix(options$n1Size), # mock dataset so the error check runs
    custom               = custom,
    exitAnalysisIfErrors = TRUE
  )

}
