#
# Copyright (C) 2015 University of Amsterdam
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

BinomialTestBayesian <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$variables) > 0 && .RCodeInOptionsIsOk(options[c("testValue", "priorA", "priorB")])
  
  # testValue, priorA & priorB are formulaFields: parse them and save the results in the state
  options <- .parseAndStoreFormulaOptions(jaspResults, options, c("testValue", "priorA", "priorB"))

  if (ready) {
    dataset <- .binomReadData(dataset, options)

    .binomCheckErrors(dataset, options)
  }

  # Output tables and plots
  .bayesBinomTableMain(       jaspResults, dataset, options, ready)
  .bayesBinomInferentialPlots(jaspResults, dataset, options, ready)
  .binomPlotsDescriptive(     jaspResults, dataset, options, ready, ciName = "descriptivesPlotsCredibleInterval")
}

# Results function ----
.bayesBinomComputeResults <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["binomResults"]]))
    return(jaspResults[["binomResults"]]$object)
  
  # This will be the object that we fill with results
  results <- list()
  hyp <- .binomTransformHypothesis(options$hypothesis)
  
  for (variable in options$variables) {
    
    results[[variable]] <- list()
    
    data <- na.omit(dataset[[.v(variable)]])
    
    for (level in levels(data)) {
      
      counts <- sum(data == level)
      BF10  <- .bayesBinomialTest(counts, length(data), theta0=options$testValue, hypothesis = hyp, a = options$priorA, b = options$priorB)
      
      # Add results for each level of each variable to results object
      results[[variable]][[level]] <- list(
        case          = variable,
        level         = level,
        counts        = counts,
        total         = length(data),
        proportion    = counts / length(data),
        BF10          = BF10,
        BF01          = 1/BF10,
        LogBF10       = log(BF10)
      )
    
    }
  }
  
  # Save results to state
  jaspResults[["binomResults"]] <- createJaspState(results)
  jaspResults[["binomResults"]]$dependOn(
    c("variables", "testValue", "hypothesis", "priorA", "priorB")
  )
  
  # Return results object
  return(results)
}

# Main Table ----
.bayesBinomTableMain <- function(jaspResults, dataset, options, ready){
  if (!is.null(jaspResults[["binomTable"]]))
    return()
  
  binomTable <- createJaspTable(gettext("Bayesian Binomial Test"))
  binomTable$dependOn(c("variables", "testValue", "hypothesis", "bayesFactorType", "priorA", "priorB"))
  binomTable$position <- 1
  binomTable$showSpecifiedColumnsOnly <- TRUE
  
  binomTable$addCitation(c(
    "Jeffreys, H. (1961). Theory of Probability. Oxford, Oxford University Press.",
    "O'Hagan, A., & Forster, J. (2004). Kendall's advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.). London: Arnold.",
    "Haldane, J. B. S. (1932). A note on inverse probability. Mathematical Proceedings of the Cambridge Philosophical Society, 28, 55-61."
  ))
  
  bfTitleSpec <- list(null="\u2080")
  if (options$hypothesis == "notEqualToTestValue")
    bfTitleSpec[["other"]] <- "\u2081"
  else if (options$hypothesis == "greaterThanTestValue")
    bfTitleSpec[["other"]] <- "\u208A"
  else if (options$hypothesis == "lessThanTestValue")
    bfTitleSpec[["other"]] <- "\u208B"
  
  bfType <- options$bayesFactorType
  if (grepl("BF10", bfType)) {
    bfTitle <- paste0("BF", bfTitleSpec[["other"]], bfTitleSpec[["null"]])
    if (bfType == "LogBF10")
      bfTitle <- paste0("Log(", bfTitle, ")")
  } else {
    bfTitle <- paste0("BF", bfTitleSpec[["null"]], bfTitleSpec[["other"]])
  }
  
  binomTable$addColumnInfo(name = "case",       title = "",                    type = "string", combine = TRUE)
  binomTable$addColumnInfo(name = "level",      title = gettext("Level"),      type = "string")
  binomTable$addColumnInfo(name = "counts",     title = gettext("Counts"),     type = "integer")
  binomTable$addColumnInfo(name = "total",      title = gettext("Total"),      type = "integer")
  binomTable$addColumnInfo(name = "proportion", title = gettext("Proportion"), type = "number")
  binomTable$addColumnInfo(name = bfType,       title = bfTitle,               type = "number")
  
  if (options$hypothesis == "lessThanTestValue")
    note <- gettextf("For all tests, the alternative hypothesis specifies that the proportion is less than %s.", options$testValueUnparsed)
  else if (options$hypothesis == "greaterThanTestValue")
    note <- gettextf("For all tests, the alternative hypothesis specifies that the proportion is greater than %s.", options$testValueUnparsed)
  else
    note <- gettextf("Proportions tested against value: %s.", options$testValueUnparsed)

  binomTable$addFootnote(message = note)

  jaspResults[["binomTable"]] <- binomTable

  if (!ready)
    return()
  
  binomTable$setExpectedSize(sum(unlist(lapply(dataset, nlevels))))
    
  bayesBinomResults <- .bayesBinomComputeResults(jaspResults, dataset, options)
  
  # we can use the frequentist function for this
  .binomFillTableMain(binomTable, bayesBinomResults)
}

#Bayes Factor Computation ----
.bayesBinomialTest.twoSided <- function(counts, n, theta0, a, b) {
  
  if (theta0 == 0 && counts == 0) {
    
    # in this case, counts*log(theta0) should be zero, omit to avoid numerical issue with log(0)
    
    logBF10 <- lbeta(counts + a, n - counts + b) -  lbeta(a, b) - (n - counts)*log(1 - theta0)
    
  } else if (theta0 == 1 && counts == n) {
    
    # in this case, (n - counts)*log(1 - theta0) should be zero, omit to avoid numerical issue with log(0)
    
    logBF10 <- lbeta(counts + a, n - counts + b) -  lbeta(a, b) - counts*log(theta0) 
    
  } else {
    
    logBF10 <- lbeta(counts + a, n - counts + b) -  lbeta(a, b) - counts*log(theta0) - (n - counts)*log(1 - theta0)
  }
  
  BF10 <- exp(logBF10)
  
  return(BF10)
  
}

.bayesBinomialTest.oneSided <- function(counts, n, theta0, a, b, hypothesis) {
  
  if (hypothesis == "less") {
    
    lowerTail <- TRUE
    
  } else if (hypothesis == "greater") {
    
    lowerTail <- FALSE
    
  }
  
  if (theta0 == 0 && counts == 0) {
    
    # in this case, counts*log(theta0) should be zero, omit to avoid numerical issue with log(0)
    logMLikelihoodH0 <- (n - counts)*log(1 - theta0)
    
  } else if (theta0 == 1 && counts == n) {
    
    # in this case, (n - counts)*log(1 - theta0) should be zero, omit to avoid numerical issue with log(0)
    logMLikelihoodH0 <- counts*log(theta0)
    
  } else {
    
    logMLikelihoodH0 <- counts*log(theta0) + (n - counts)*log(1 - theta0)
    
  }
  
  term1 <- pbeta(theta0, a + counts, b + n - counts, lower.tail = lowerTail, log.p = TRUE) +
    lbeta(a + counts, b + n - counts)
  term2 <- lbeta(a,b) + pbeta(theta0, a, b, lower.tail = lowerTail, log.p = TRUE)
  logMLikelihoodH1 <- term1 - term2
  BF10 <- exp(logMLikelihoodH1 - logMLikelihoodH0)
  
  return(BF10)
  
}

.bayesBinomialTest <- function(counts, n, theta0, hypothesis, a, b) {
  
  if (hypothesis == "two.sided") {
    
    BF10 <- try(.bayesBinomialTest.twoSided(counts, n, theta0, a, b), silent = TRUE)
    
  } else {
    
    #if (theta0 == 0 || theta0 == 1) {
    #	
    #	BF10 <- NA
    #	
    #} else {
    
    BF10 <- try(.bayesBinomialTest.oneSided(counts, n, theta0, a, b, hypothesis), silent = TRUE)
    
    #}
  }
  
  if (isTryError(BF10))
    BF10 <- NA
  
  return(BF10)
  
}

.bayesBinomGetSubscript <- function(hypothesis) {
  if (hypothesis == "notEqualToTestValue")
    return("BF[1][0]")
  else if (hypothesis == "greaterThanTestValue")
    return("BF['+'][0]")
  else
    return("BF['-'][0]")
}

#plots ----
.bayesBinomInferentialPlots <- function(jaspResults, dataset, options, ready) {
  if (!options$plotPriorAndPosterior && !options$plotSequentialAnalysis)
    return()
  
  if (is.null(jaspResults[["inferentialPlots"]])) { 
    inferentialPlots <- createJaspContainer(gettext("Inferential Plots"))
    inferentialPlots$dependOn(c("testValue", "priorA", "priorB", "hypothesis"))
    inferentialPlots$position <- 2
    jaspResults[["inferentialPlots"]] <- inferentialPlots
  } else {
    inferentialPlots <- jaspResults[["inferentialPlots"]]
  }

  if (!ready) {
    # show a placeholder plot if someone says he wants a plot but does not enter any variables
    inferentialPlots[["placeholder"]] <- createJaspPlot(width = 530, height = 400, dependencies = "variables")
    return()
  }
  
  hyp <- .binomTransformHypothesis(options$hypothesis)
  
  for (var in options$variables) {
    
    data <- na.omit(dataset[[.v(var)]])
    if (length(data) == 0)
      next
      
    for (level in levels(data)) {
      id <- paste0(var, " - ", level)
      
      if (is.null(inferentialPlots[[id]])) {
        levelPlotContainer <- createJaspContainer(title = id)
        levelPlotContainer$dependOn(optionContainsValue=list(variables=var))
        inferentialPlots[[id]] <- levelPlotContainer
      } else {
        levelPlotContainer <- inferentialPlots[[id]]
      }
      
      counts <- sum(data == level)
      BF10   <- .bayesBinomialTest(counts, length(data), options$testValue, hypothesis = hyp, a = options$priorA, b = options$priorB)

      plotName <- paste0(var, level, "priorposterior")
      .bayesBinomPriorPosteriorPlot(levelPlotContainer, plotName, options, BF10, counts, length(data), hyp)
      
      plotName <- paste0(var, level, "sequential")
      .bayesBinomSequentialPlot(levelPlotContainer, plotName, options, BF10, counts, length(data), hyp, var, data, level)
    }
    
  }
}

.bayesBinomPriorPosteriorPlot <- function(container, plotName, options, BF10, counts, n, hyp) {
  if (!options$plotPriorAndPosterior || !is.null(container[[plotName]]))
    return()
  
  plot <- createJaspPlot(title = gettext("Prior and Posterior"), width = 530, height = 400, aspectRatio = 0.7)
  plot$dependOn(c("plotPriorAndPosterior", "plotPriorAndPosteriorAdditionalInfo"))
  
  container[[plotName]] <- plot 

  bfSubscripts <- .bayesBinomGetSubscript(options$hypothesis)
  quantiles <- .credibleIntervalPlusMedian(credibleIntervalInterval = .95, options$priorA, options$priorB, counts, n, hypothesis=hyp, theta0 = options$testValue)
  dfLinesPP <- .dfLinesPP(a=options$priorA, b=options$priorB, hyp = hyp, theta0 = options$testValue, counts = counts, n = n)
  dfPointsPP <- .dfPointsPP(a=options$priorA, b=options$priorB, hyp = hyp, theta0 = options$testValue, counts = counts, n = n)
  xName <- bquote(paste(.(gettext("Population proportion")), ~theta))
  
  hypForPlots <- .binomHypothesisForPlots(hyp)
  
  if (!options$plotPriorAndPosteriorAdditionalInfo)
    p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName)
  else
    p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName, BF = BF10, bfType = "BF10",
                                           CRI = c(quantiles$ci.lower, quantiles$ci.upper), median = quantiles$ci.median, 
                                           hypothesis = hypForPlots, drawCRItxt = TRUE)
  plot$plotObject <- p
}

.bayesBinomSequentialPlot <- function(container, plotName, options, BF10, counts, n, hyp, var, data, level) {   
  if (!options$plotSequentialAnalysis || !is.null(container[[plotName]]))
    return()
  
  plot <- createJaspPlot(title = gettext("Sequential Analysis"), width = 530, height = 400, aspectRatio = 0.7)
  plot$dependOn(c("plotSequentialAnalysis", "bayesFactorType"))
  
  container[[plotName]] <- plot
  
  hypForPlots <- .binomHypothesisForPlots(hyp)

  p <- try({
    bfTypeIgnoreLog <- if(options[["bayesFactorType"]] == "BF01") "BF01"  else "BF10" # see https://github.com/jasp-stats/INTERNAL-jasp/issues/1101
    bf <- if(bfTypeIgnoreLog == "BF01") 1 / BF10  else BF10
    bfSubscripts <- .bayesBinomGetSubscript(options$hypothesis)
    dfLinesSR   <- .dfLinesSR(d = data, var = var, split = level, a = options$priorA, b = options$priorB, hyp = hyp, theta0 = options$testValue, bfType = bfTypeIgnoreLog)
    JASPgraphs::PlotRobustnessSequential(dfLines = dfLinesSR, xName = "n", BF = bf, bfType = bfTypeIgnoreLog, hypothesis = hypForPlots)
  })
  
  if (inherits(p, "try-error"))
    plot$setError(.extractErrorMessage(p))
  else
    plot$plotObject <- p
}

.dfLinesPP <- function(dataset, a = 1, b = 1, hyp = "two-sided", theta0 = .5, counts, n){
  if (a == 1 && b == 1) {
    
    theta <- seq(0, 1, length.out = 1000)

  } else {
    
    theta <- seq(0.001, 0.999, length.out = 1000)
  }

  size <- length(theta)
  if      (hyp == "two.sided"){
    linesGroup <- c(dbeta(theta, a + counts, b + n - counts), dbeta(theta, a, b))
  }
  else if (hyp == "greater")   {
    linesPrior <- linesPosterior <- numeric(size) # initializes everything to 0
    idx <- theta >= theta0 # the nonzero components
    # fill in the posterior
    linesPosterior[idx] <- dbeta(theta[idx], a + counts, b + n - counts) / pbeta(theta0, a + counts, b + n - counts, lower.tail = FALSE)
    # fill in the prior
    linesPrior[idx] <- dbeta(theta[idx], a, b) / pbeta(theta0, a, b, lower.tail = FALSE)
    linesGroup <- c(linesPosterior, linesPrior)
  }
  else if (hyp == "less")      {
    linesPrior <- linesPosterior <- numeric(size) # initializes everything to 0
    idx        <- theta <= theta0 # the nonzero components
    # fill in the posterior
    linesPosterior[idx] <- dbeta(theta[idx], a + counts, b + n - counts) / pbeta(theta0, a + counts, b + n - counts, lower.tail = TRUE)
    # fill in the prior
    linesPrior[idx] <- dbeta(theta[idx], a, b) / pbeta(theta0, a, b, lower.tail = TRUE)
    linesGroup      <- c(linesPosterior, linesPrior)
  }
  thetaGroup <- c(theta, theta)
  nameGroup  <- c(rep("Posterior", length(theta)), rep("Prior", length(theta)))
  dat        <- data.frame(x = thetaGroup, y = linesGroup, g = nameGroup)
  return(dat)
}

.dfPointsPP <- function(dataset, a = 1, b = 1, hyp = "two-sided", theta0 = .5, counts, n){
  if      (hyp == "two.sided"){
    heightPosteriorTheta0 <- dbeta(theta0, a + counts, b + n - counts)
    heightPriorTheta0     <- dbeta(theta0, a, b)
  }
  else if (hyp == "greater")   {
    heightPosteriorTheta0 <- dbeta(theta0, a + counts, b + n - counts)/pbeta(theta0, a + counts, b + n - counts, lower.tail = FALSE)
    heightPriorTheta0     <- dbeta(theta0, a, b) / pbeta(theta0, a, b, lower.tail = FALSE)
  }
  else if (hyp == "less")      {
    heightPosteriorTheta0 <- dbeta(theta0, a + counts, b + n - counts)/pbeta(theta0, a + counts, b + n - counts)
    heightPriorTheta0     <- dbeta(theta0, a, b) / pbeta(theta0, a, b)
  }
  pointXVal <- c(theta0, theta0)
  pointYVal <- c(heightPosteriorTheta0, heightPriorTheta0)
  nameGroup <- c("Posterior", "Prior")
  dat       <- data.frame(x = pointXVal, y = pointYVal, g = nameGroup)
  return(dat)
}

.dfLinesSR <- function(d, var, level, split, a = 1, b = 1, hyp = "two-sided", theta0 = .5, bfType = c("BF01", "BF10")){
  bfType <- match.arg(bfType)
  x <- 1 * (d == split)
  BF10 <- numeric(length(x))
  for (i in seq_along(x)) {
    
    counts <- sum(x[1:i] == 1)
    n <- i  #length(x[1:i])
    BF10[i] <- .bayesBinomialTest(counts = counts, n = n, theta0, hyp, a, b)
    
    if (is.na(BF10[i]))
      stop(gettext("One or more Bayes factors cannot be computed"))
    
    if (is.infinite(BF10[i]))
      stop(gettext("One or more Bayes factors are infinite"))
  }
  if(bfType == "BF01") {
    dat <- data.frame(x = 1:length(x), y = -log(BF10))
  } else {
    dat <- data.frame(x = 1:length(x), y = log(BF10))
  }
  return(dat)
}

#CRI and Median ----
.credibleIntervalPlusMedian <- function(credibleIntervalInterval = .95, a = 1, b = 1, counts = 10, n = 20, hypothesis = "two.sided", theta0 = .5) {
  
  lower <- (1 - credibleIntervalInterval) / 2
  upper <- 1 - lower
  
  if (hypothesis == "two.sided") {
    
    quantiles <- qbeta(c(lower, .5, upper), a + counts , b + n - counts)
    
  } else if (hypothesis == "greater") {
    
    rightArea <- pbeta(theta0, a + counts , b + n - counts, lower.tail = FALSE)
    leftArea  <- 1 - rightArea
    quantiles <- qbeta(leftArea + rightArea * c(lower, .5, upper), a + counts , b + n - counts)
    
  } else if (hypothesis == "less") {
    
    leftArea  <- pbeta(theta0, a + counts , b + n - counts)
    quantiles <- qbeta(leftArea * c(lower, .5, upper), a + counts , b + n - counts)
    
  }
  
  return(list(ci.lower = quantiles[1], ci.median = quantiles[2], ci.upper = quantiles[3]))
  
}

.binomHypothesisForPlots <- function(hyp){
  if(hyp == "greater")
    return("greater")
  else if(hyp == "less")
    return("smaller")
  else if(hyp == "two.sided")
    return("equal")
  else
    stop(gettext("Undefined hypothesis"))
}
