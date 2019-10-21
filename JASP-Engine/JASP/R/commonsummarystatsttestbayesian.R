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
  n1 <- size
  n2 <- if (!is.null(options$n2Size)) options$n2Size else 0 # single sample case
  oneSided = !(options$hypothesis %in% c("notEqualToTestValue","groupsNotEqual"))
  
  ### Default case: a non-informative zero-centered Cauchy prior
  if(options$effectSizeStandardized == "default") {
    nullInterval <-
      switch(options$hypothesis, greaterThanTestValue = c(0, Inf), groupOneGreater = c(0, Inf),
             lessThanTestValue = c(-Inf,0), groupTwoGreater = c(-Inf, 0), c(-Inf,Inf))    # default is notEqualToTestValue
    
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

# Prior & Posterior plot 
.ttestBayesianPriorPosteriorPlot.summarystats <- function(jaspResults, summaryStatsTTestResults, options){
  
  if (!options[["plotPriorAndPosterior"]])
    return()
  
  plot <- createJaspPlot(
    title       = "Prior and Posterior",
    width       = 530,
    height      = 400,
    aspectRatio = 0.7
  )
  plot$position <- 2
  plot$dependOn(options = c("plotPriorAndPosterior", "plotPriorAndPosteriorAdditionalInfo"))
  jaspResults[["ttestContainer"]][["priorPosteriorPlot"]] <- plot
  browser()
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
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    plot$setError(errorMessage)
  } else {
    plot$plotObject <- p
  }
  return()
}

# Bayes FactorRobustness Check plot
.ttestBayesianPlotRobustness.summarystats <- function(jaspResults, summaryStatsTTestResults, options){
  
  if (!options[["plotBayesFactorRobustness"]])
    return()
  
  plot <- createJaspPlot(
    title       = "Bayes Factor Robustness Check",
    width       = 530,
    height      = 400,
    aspectRatio = 0.7
  )
  plot$position <- 3
  plot$dependOn(options = c("plotBayesFactorRobustness", "plotBayesFactorRobustnessAdditionalInfo"))
  jaspResults[["ttestContainer"]][["BayesFactorRobustnessPlot"]] <- plot
  
  if (!summaryStatsTTestResults[["ready"]] || jaspResults[["ttestContainer"]]$getError())
    return()
  
  robustnessInfo <- summaryStatsTTestResults[["ttestRobustnessPlot"]] 
  hypothesisList <- summaryStatsTTestResults[["hypothesisList"]]
  
  # error check: Informative prior?
  if (robustnessInfo$isInformative) {
    plot$setError("Plotting not possible: Bayes factor robustness check plot currently not supported for informed prior.")
    return()
  } 
  
  # Bayes Factor Robustness Check plot
  p <- try(.plotBF.robustnessCheck.ttest.summarystats(
    t                     = robustnessInfo$t,
    n1                    = robustnessInfo$n1,
    n2                    = robustnessInfo$n2,
    paired                = robustnessInfo$paired,
    BF10user              = robustnessInfo$BF10user,
    bfType                = options$bayesFactorType,
    nullInterval          = hypothesisList$nullInterval,
    rscale                = robustnessInfo$rscale,
    oneSided              = hypothesisList$oneSided,
    isInformative         = robustnessInfo$isInformative,
    additionalInformation = robustnessInfo$additionalInformation
  ))
  
  if (isTryError(p)) {
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    plot$setError(errorMessage)
  } else {
    plot$plotObject <- p
  }
  return()
}
.plotBF.robustnessCheck.ttest.summarystats <- function(t, n1, n2, paired = FALSE, BF10user, bfType = "BF10", nullInterval, rscale = 0.707, oneSided = FALSE,
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
  
  BFH1H0 <- !(bfType == "BF01")
  if (!BFH1H0) {
    dfLines$y <- - dfLines$y
    BF10user  <- 1 / BF10user
    maxBF10   <- 1 / maxBF10
    BF10w     <- 1 / BF10w
    BF10ultra <- 1 / BF10ultra
  }
  
  BFsubscript <- .ttestBayesianGetBFnamePlots(BFH1H0, nullInterval)
  
  dfPoints <- data.frame(
    x = c(maxBFrVal, rscale, 1, sqrt(2)),
    y = log(c(maxBF10, BF10user, BF10w, BF10ultra)),
    g = JASPgraphs::parseThis(c(
      sprintf("paste(max, ~%s, ':',   phantom(phollll), %s, ~at, ~'r'==%s)", BFsubscript, format(maxBF10  , digits = 4), format(maxBFrVal, digits = 4)),
      sprintf("paste(user~prior, ':', phantom(phll[0]), ~%s==%s)"          , BFsubscript, format(BF10user , digits = 4)),
      sprintf("paste(wide~prior, ':', phantom(ph[0][0]), ~%s==%s)"         , BFsubscript, format(BF10w    , digits = 4)),
      sprintf("paste(ultrawide~prior, ':', ~%s==%s)"                       , BFsubscript, format(BF10ultra, digits = 4))
    )),
    stringsAsFactors = FALSE
  )
  
  hypothesis <- switch(oneSided,
                       "right" = "greater",
                       "left"  = "smaller",
                       "equal"
  )
  
  p <- JASPgraphs::PlotRobustnessSequential(
    dfLines      = dfLines,
    dfPoints     = dfPoints,
    pointLegend  = additionalInformation,
    xName        = "Cauchy prior width",
    hypothesis   = hypothesis,
    bfType       = bfType
  )
  
  return(p)
  
}