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

BinomialTestBayesian <- function(jaspResults, dataset, options, ...) {
  
  # Read dataset
  dataset <- .binomReadData(dataset, options)
  
  # Error checking
  errors <- .binomCheckErrors(dataset, options)
  
  # Compute the results
  bayesianBinomResults <- .bayesianBinomComputeResults(jaspResults, dataset, options, errors)

  # Output tables and plots
  .bayesianBinomTableMain(jaspResults, dataset, options, bayesianBinomResults, errors)
  .bayesianBinomPlots(    jaspResults, dataset, options, bayesianBinomResults, errors)
  .binomContainerPlots(   jaspResults, dataset, options, binomResults = bayesianBinomResults, errors)
  .binomPlotsDescriptive( jaspResults, dataset, options, binomResults = bayesianBinomResults, errors)
  
  return()
}

# Results functions ----
.bayesianBinomComputeResults <- function(jaspResults, dataset, options, errors) {
  
  if (!is.null(errors) && errors == "No variables") return()
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateBayesianBinomResults"]])) return(jaspResults[["stateBayesianBinomResults"]]$object)
  
  # This will be the object that we fill with results
  results <- list()
  
  # First, we perform precalculation of variables we use throughout the analysis
  results[["spec"]] <- .binomCalcSpecs(dataset, options)
  results[["binom"]] <- list()
  
  for (variable in options$variables) {
    
    results[["binom"]][[variable]] <- list()
    
    # Prepare for running the binomial test
    column <- dataset[[.v(variable)]]
    data   <- column[!is.na(column)]
    levels <- levels(data)
    
    for (level in levels) {
      n   <- length(data)
      counts <- sum(data == level)
      prop   <- counts / n
      
      # Binomial test for plots
      plotResults <- stats::binom.test(
        x           = counts,
        n           = n,
        p           = options$testValue,
        alternative = "two.sided",
        conf.level  = options$descriptivesPlotsConfidenceInterval
      )
      
      # Summary statistics for plots
      plotDat <- data.frame(
        label      = level,
        proportion = prop,
        lowerCI    = plotResults$conf.int[1],
        upperCI    = plotResults$conf.int[2]
      )
      
      # beta distribution parameters
      a <- options$priorA
      b <- options$priorB
      
      if (options$hypothesis == "notEqualToTestValue") 
        hyp <- "two.sided"
      else if (options$hypothesis == "greaterThanTestValue")
        hyp <- "greater"
      else
        hyp <- "less"
      
      BF10  <- .bayesBinomialTest(counts, n, options$testValue, hypothesis = hyp, a = a, b = b)
      
      BF <- BF10
      
      if (options$bayesFactorType == "BF01") {
        BF <- 1/BF10
      } else if(options$bayesFactorType == "LogBF10") {
        BF <- log(BF10)
      }
      
      # Add results for each level of each variable to results object
      results[["binom"]][[variable]][[level]] <- list(
        case          = variable,
        level         = level,
        counts        = counts,
        total         = n,
        proportion    = prop,
        BF            = BF,
        plotDat       = plotDat
      )
    }
  }
  
  # Save results to state
  jaspResults[["stateBayesianBinomResults"]] <- createJaspState(results)
  jaspResults[["stateBayesianBinomResults"]]$dependOn(
    c("variables", "testValue", "hypothesis", "descriptivesPlotsConfidenceInterval")
  )
  
  # Return results object
  return(results)
}

# Main Table ----
.bayesianBinomTableMain <- function(jaspResults, dataset, options, bayesianBinomResults, errors){
  if (!is.null(jaspResults[["bayesianBinomialTable"]])) return()
  variables <- unlist(options$variables)
  theta0<- .5
  
  bayesianBinomialTable                   <- createJaspTable("Bayesian Binomial Test")
  bayesianBinomialTable$dependOn(c("variables", "testValue", "hypothesis", "bayesFactorType", "Prior"))
  bayesianBinomialTable$position          <- 1
  
  bayesianBinomialTable$addCitation(c("Jeffreys, H. (1961). Theory of Probability. Oxford, Oxford University Press.",
                                      "O'Hagan, A., & Forster, J. (2004). Kendall's advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.). London: Arnold.",
                                      "Haldane, J. B. S. (1932). A note on inverse probability. Mathematical Proceedings of the Cambridge Philosophical Society, 28, 55-61."))
  
  if (options$bayesFactorType == "BF01") {
    
    BFH1H0 <- FALSE
    if (options$hypothesis == "notEqualToTestValue") 
      bf.title <- "BF\u2080\u2081"
    else if (options$hypothesis == "greaterThanTestValue")
      bf.title <- "BF\u2080\u208A"
    else if (options$hypothesis == "lessThanTestValue")
      bf.title <- "BF\u2080\u208B"
    
  } else if (options$bayesFactorType == "BF10") {
    
    BFH1H0 <- TRUE
    
    if (options$hypothesis == "notEqualToTestValue")
      bf.title <- "BF\u2081\u2080"
    else if (options$hypothesis == "greaterThanTestValue")
      bf.title <- "BF\u208A\u2080"
    else if (options$hypothesis == "lessThanTestValue")
      bf.title <- "BF\u208B\u2080"
    
  } else if (options$bayesFactorType == "LogBF10") {
    
    BFH1H0 <- TRUE
    
    if (options$hypothesis == "notEqualToTestValue")
      bf.title <- "Log(\u0042\u0046\u2081\u2080)"
    else if (options$hypothesis == "greaterThanTestValue")
      bf.title <-"Log(\u0042\u0046\u208A\u2080)"
    else if (options$hypothesis == "lessThanTestValue")
      bf.title <- "Log(\u0042\u0046\u208B\u2080)"
    
  }
  
  if (options$hypothesis == "notEqualToTestValue") {
    
    hyp     <- "two.sided"
    message <- paste0("Proportions tested against value: ", options$testValue, ".")
    bayesianBinomialTable$addFootnote(message)
    
  } else if (options$hypothesis == "greaterThanTestValue") {
    
    hyp  <- "greater"
    note <- "For all tests, the alternative hypothesis specifies that the proportion is greater than "
    message <- paste0(note, options$testValue, ".")
    bayesianBinomialTable$addFootnote(message)
    
  } else {
    
    hyp  <- "less"
    note <- "For all tests, the alternative hypothesis specifies that the proportion is less than "
    message <- paste0(note, options$testValue, ".")
    bayesianBinomialTable$addFootnote(message)
    
  }
  
  bayesianBinomialTable$addColumnInfo(name="case", title="", type="string", combine=TRUE)
  bayesianBinomialTable$addColumnInfo(name="level", title="Level", type="string")
  bayesianBinomialTable$addColumnInfo(name="counts", title="Counts", type="integer")
  bayesianBinomialTable$addColumnInfo(name="total", title="Total", type="integer")
  bayesianBinomialTable$addColumnInfo(name="proportion", title="Proportion", type="number", format="sf:4;dp:3")
  bayesianBinomialTable$addColumnInfo(name="BF", title=bf.title, type="number", format="sf:4;dp:3")
  
  if (options$testValue == 1 && hyp == "greater") {
    
    errorMessageTable <- "Cannot test the hypothesis that the test value is greater than 1."
    
  } else if (options$testValue == 0 && hyp == "less") {
    
    errorMessageTable <- "Cannot test the hypothesis that the test value is less than 0."
  }
  
  jaspResults[["bayesianBinomialTable"]] <- bayesianBinomialTable
  
  if (!is.null(errors) && errors == "No variables")
    return()
  
  for (variable in options$variables) {
    for (level in bayesianBinomResults[["spec"]][["levels"]][[variable]]) {
      row <- bayesianBinomResults[["binom"]][[variable]][[level]][1:6]
      bayesianBinomialTable$addRows(row, rowNames = paste0(variable, " - ", level))
    }
  }
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
  
  if (class(BF10) == "try-error")
    BF10 <- NA
  
  return(BF10)
  
}

#plots ----
.bayesianBinomPlots <- function(jaspResults, dataset, options, bayesianBinomResults, errors) {
  # beta distribution parameters
  a <- options$priorA
  b <- options$priorB
  if (options$hypothesis == "notEqualToTestValue") {
    hyp <- "two.sided"
    bfSubscripts = "BF[1][0]"
  }
  else if (options$hypothesis == "greaterThanTestValue"){
    hyp <- "greater"
    bfSubscripts = "BF['+'][0]"
  }
  else {
    hyp <- "less"
    bfSubscripts = "BF['-'][0]"
  }
  variables <- unlist(options$variables)
  if(is.null(jaspResults[["bayesianBinomPlots"]])) { 
    jaspResults[["bayesianBinomPlots"]] <- createJaspContainer("Inferential Plots")
    jaspResults[["bayesianBinomPlots"]]$dependOn(c("Prior", "variables", "hypothesis", "plotPriorAndPosterior", "plotSequentialAnalsis", "plotPriorAndPosteriorAdditionalInfo"))
    jaspResults[["bayesianBinomPlots"]]$position <- 2
  }
  bayesianBinomPlots <- jaspResults[["bayesianBinomPlots"]]
  for(var in variables) {
    d <- dataset[[.v(var)]]
    d <- d[!is.na(d)]
    if (length(d) == 0){
      message <- paste0("<em>Warning.</em>", var, "is excluded as it has no valid observations.")
      bayesianBinomPlots$addFootnote(message)
    }
    
    levels <- levels(d)
    n <- length(d)
    varSplitFactor     <- dataset[[.v(var)]]
    if(length(varSplitFactor[var])==0)
      return(createJaspPlot(error="Plotting is not possible: Variable only contains NA!", dependencies="variables"))
    #gives the different split values 
    varSplitLevels    <- levels(varSplitFactor)
    # remove missing values from the grouping variable
    vardataset           <- dataset[!is.na(varSplitFactor), ]
    varSplitData     <- split(vardataset, varSplitFactor)
    for( lev in 1:length(varSplitLevels)){ 
      split <- varSplitLevels[[lev]]
      splitWord <- paste0("deeperBayesianBinomPlots", var, lev)
      deeperBayesianBinomPlots <- createJaspContainer(paste0(var, " - ", varSplitLevels[lev]))
      bayesianBinomPlots[[splitWord]] <- deeperBayesianBinomPlots
      counts <- sum(d == split)
      prop <- counts/n
      BF10   <- .bayesBinomialTest(counts, n, options$testValue, hypothesis = hyp, a = a, b = b)
      if(options$plotPriorAndPosterior) {
        quantiles <- .credibleIntervalPlusMedian(credibleIntervalInterval = .95, a, b, counts, n, hypothesis=hyp, theta0=options$testValue)
        medianPosterior <- quantiles$ci.median
        CIlower <- quantiles$ci.lower
        CIupper <- quantiles$ci.upper
        ppCri     <- c(CIlower, CIupper)
        dfLinesPP    <- .dfLinesPP(a=a, b=b, hyp = hyp, theta0=options$testValue, counts=counts, n=n)
        dfPointsPP   <- .dfPointsPP(a=a, b=b, hyp = hyp, theta0=options$testValue, counts=counts, n=n)
        xName  <- expression(paste("Population proportion ", theta))
        if(options$plotPriorAndPosteriorAdditionalInfo){
          p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName, BF01 = 1/BF10,
                                                 CRI = ppCri, median = medianPosterior, drawCRItxt = TRUE, bfSubscripts = bfSubscripts)
        }
        else {
          p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName, bfSubscripts = bfSubscripts
          )
        }
        plot <- createJaspPlot(
          title       = "Prior and Posterior",
          width       = 530,
          height      = 400,
          plot        = p,
          aspectRatio = 0.7
        )
        plot$dependOn(c("plotPriorAndPosterior", "Prior", "hypothesis", "plotPriorAndPosteriorAdditionalInfo"))
        bayesianBinomPlots[[splitWord]][[paste0(var, lev)]] <- plot
      }
      if(options$plotSequentialAnalysis){
        dfLinesSR   <- .dfLinesSR(d=d, var=var, split = split, a = a, b = b, hyp = hyp, theta0=options$testValue)
        dfPointsSR  <- NULL
        xName  <- "n"
        p <- JASPgraphs::PlotRobustnessSequential(dfLines = dfLinesSR, dfPoints = dfPointsSR, xName = xName, BF01 = 1/BF10, hasRightAxis = TRUE, bfSubscripts = bfSubscripts)
        plot <- createJaspPlot(
          title       = "Sequential Analysis",
          width       = 530,
          height      = 400,
          plot        = p,
          aspectRatio = 0.7
        )
        plot$dependOn(c("plotSequentialAnalysis", "Prior", "hypothesis"))
        bayesianBinomPlots[[splitWord]][[paste0(var, lev, "sequential")]] <- plot
        
        
      }
    }
  }
}

.dfLinesPP <- function(dataset, a = 1, b = 1, hyp = "two-sided", theta0=.5, counts, n){
  if (a == 1 && b == 1) {
    
    theta <- seq(0, 1, length.out = 1000)
    
  } else {
    
    theta <- seq(0.001, 0.999, length.out = 1000)
  }
  x <- theta
  if      (hyp == "two.sided"){
    linesGroup <- c(dbeta(x, a + counts, b + n - counts), dbeta(x, a, b))
  }
  else if (hyp== "greater")   {
    linesGroup <- c(ifelse (x >= theta0, 
                            dbeta(x, a + counts, b + n - counts)/pbeta(theta0, a + counts, b + n - counts, lower.tail = FALSE), 
                            0), 
                    ifelse (x >= theta0, 
                            dbeta(x, a, b) / pbeta(theta0, a, b, lower.tail = FALSE), 
                            0)
    )
  }
  else if (hyp== "less")      {
    linesGroup <- c(ifelse (x <= theta0,
                            dbeta(x, a + counts, b + n - counts) / pbeta(theta0, a + counts, b + n - counts),
                            0), 
                    ifelse (x <= theta0,
                            dbeta(x, a, b) / pbeta(theta0, a, b),
                            0)
    )
  }
  thetaGroup <- c(theta, theta)
  nameGroup  <- c(rep("Posterior", length(theta)), rep("Prior", length(theta)))
  dat <- data.frame(x = thetaGroup, y = linesGroup, g=nameGroup)
  return(dat)
}

.dfPointsPP <- function(dataset, a = 1, b = 1, hyp = "two-sided", theta0=.5, counts, n){
  if      (hyp == "two.sided"){
    heightPosteriorTheta0 <- dbeta(theta0, a + counts, b + n - counts)
    heightPriorTheta0 <- dbeta(theta0, a, b)
  }
  else if (hyp== "greater")   {
    heightPosteriorTheta0 <- dbeta(theta0, a + counts, b + n - counts)/pbeta(theta0, a + counts, b + n - counts, lower.tail = FALSE)
    heightPriorTheta0 <- dbeta(theta0, a, b) / pbeta(theta0, a, b, lower.tail = FALSE)
  }
  else if (hyp== "less")      {
    heightPosteriorTheta0 <- dbeta(theta0, a + counts, b + n - counts)/pbeta(theta0, a + counts, b + n - counts)
    heightPriorTheta0 <- dbeta(theta0, a, b) / pbeta(theta0, a, b)
  }
  pointXVal <- c(theta0, theta0)
  pointYVal <- c(heightPosteriorTheta0, heightPriorTheta0)
  nameGroup  <- c("Posterior", "Prior")
  dat <- data.frame(x = pointXVal, y = pointYVal, g=nameGroup)
  return(dat)
}

.dfLinesSR <- function(d, var, level, split, a = 1, b = 1, hyp = "two-sided", theta0=.5){
  x <- ifelse (d == split, 1, 0)
  BF10 <- vector("numeric", length(x))
  for (i in seq_along(x)) {
    
    counts <- sum(x[1:i] == 1)
    n <- length(x[1:i])
    BF10[i] <- .bayesBinomialTest(counts = counts, n = n, theta0, hyp, a, b)
    
    if (is.na(BF10[i]))
      stop("One or more Bayes factors cannot be computed")
    
    if (is.infinite(BF10[i]))
      stop("One or more Bayes factors are infinity")
  }
  dat <- data.frame(x = 1:length(x), y = log(BF10))
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
    leftArea <- 1 - rightArea
    quantiles <- qbeta(leftArea + rightArea * c(lower, .5, upper), a + counts , b + n - counts)
    
  } else if (hypothesis == "less") {
    
    leftArea <- pbeta(theta0, a + counts , b + n - counts)
    quantiles <- qbeta(leftArea * c(lower, .5, upper), a + counts , b + n - counts)
    
  }
  
  return(list(ci.lower = quantiles[1], ci.median = quantiles[2], ci.upper = quantiles[3]))
  
}