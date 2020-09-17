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

# abbreviation:
# ttestBIS = ttestBayesianIndependentSamples
TTestBayesianIndependentSamples <- function(jaspResults, dataset, options) {

  .ttestBayesianRunAnalysis(jaspResults, dataset, options, "independent")

}

# Execute t-test ----
.ttestBISTTest <- function(ttestContainer, dataset, options, derivedOptions, errors, ttestState) {

	# this function is the main workhorse, and also makes a table
	grouping   <- options[["groupingVariable"]]
	levels     <- levels(dataset[[.v(grouping)]])
	g1 <- levels[1L]
	g2 <- levels[2L]

  # does all addcolumninfo etc.
	ttestTable <- .ttestBISTTestMarkup(options, derivedOptions, g1, g2)

	# this is a standardized object. The names are identical to those in the other Bayesian t-tests
  ttestResults <- .ttestBayesianEmptyObject(options, derivedOptions, ttestState)
  dependents <- options[["variables"]]

  # create empty object for the table, this has previously computed rows already filled in
  ttestRows <- .ttestBayesianCreateTtestRows(dependents, options, derivedOptions, ttestState)
  ttestTable$setData(ttestRows)
  ttestContainer[["ttestTable"]] <- ttestTable
	if (!derivedOptions[["ready"]]) # user provided no grouping variable or empty columns
  	return(ttestResults)

  # we can do the analysis
  alreadyComputed <- !is.na(ttestRows[, "BF"]) & ttestResults[["hypothesis"]] == options[["hypothesis"]]
  .ttestBayesianSetFootnotesMainTable(ttestTable, ttestResults, dependents[alreadyComputed])

  nvar <- length(dependents)
  BFH1H0 <- ttestResults[["BFH1H0"]]
  oneSided <- derivedOptions[["oneSided"]]
  .ttestBayesianInitBayesFactorPackageOptions()

  if (derivedOptions[["wilcoxTest"]])
    .ttestBayesianSetupWilcoxProgressBar(nvar, ttestState, options[["wilcoxonSamplesNumber"]])

  idxg1 <- dataset[[.v(options[["groupingVariable"]])]] == g1
  idxg2 <- dataset[[.v(options[["groupingVariable"]])]] == g2
  idxNAg <- is.na(dataset[[.v(grouping)]])

  for (var in dependents[!alreadyComputed]) {

    if (!isFALSE(errors[[var]])) {

      errorMessage <- errors[[var]]$message
      ttestTable$addFootnote(errorMessage, rowNames = var)
      ttestResults[["status"]][var] <- "error"
      ttestResults[["errorFootnotes"]][[var]] <- errorMessage
      ttestRows[var, -1L] <- NaN # everything except the first because the length and names differ for student vs wilcoxon

    } else {

      # these objects are made here so they don't need to be created every time a try fails,
      # which means they could be forgotten and not created
      bf.raw <- NaN
      error  <- NaN

      # BayesFactor package doesn't handle NAs, so it is necessary to exclude them
      idxNA <- is.na(dataset[[.v(var)]]) | idxNAg
      subDataSet <- dataset[!idxNA, .v(var)]

      group1 <- subDataSet[idxg1[!idxNA]]
      group2 <- subDataSet[idxg2[!idxNA]]

      ttestResults[["n1"]][var] <- length(group1)
      ttestResults[["n2"]][var] <- length(group2)

      if (!derivedOptions[["wilcoxTest"]]) {

        r <- try(.generalTtestBF(x = group1, y = group2, paired = FALSE, oneSided = oneSided, options = options))

        if (isTryError(r)) {

          errorMessage <- .extractErrorMessage(r)
	    		ttestResults[["status"]][[var]] <- "error"
	    		ttestResults[["errorFootnotes"]][[var]] <- errorMessage
	    		ttestTable$addFootnote(message = errorMessage, rowNames = var)

        } else {

          bf.raw <- r[["bf"]]
          error  <- r[["error"]]
          ttestResults[["tValue"]][[var]] <- r[["tValue"]]

          if (!is.null(error) && is.na(error) && grepl("approximation", r[["method"]])) {
            error <- NaN
            message <- gettext("t-value is large. A Savage-Dickey approximation was used to compute the Bayes factor but no error estimate can be given.")
            ttestTable$addFootnote(message = message, symbol = "", rowNames = var, colNames = "error")
            ttestResults[["footnotes"]][[var]] <- c(ttestResults[["footnotes"]][[var]], message)
          }
          if (is.null(error) && options[["effectSizeStandardized"]] == "informative" && 
              options[["informativeStandardizedEffectSize"]] == "normal") {
            error <- NA_real_
            message <- gettext("No error estimate is available for normal priors.")
            ttestTable$addFootnote(message = message)
            ttestResults[["globalFootnotes"]] <- c(ttestResults[["globalFootnotes"]], message)
          }
        }

      } else { # wilcoxtest

        # If the samples can be reused, don't call the Gibbs sampler again, but recalculate the
        # Bayes factor with new settings and take the samples from state.
        if (!is.null(ttestResults[["delta"]][[var]]) && !is.na(ttestResults[["delta"]][[var]])) {

          bf.raw <- try(.computeBayesFactorWilcoxon(
            deltaSamples         = ttestResults[["delta"]][[var]],
            cauchyPriorParameter = options[["priorWidth"]],
            oneSided             = oneSided
          ))

        } else {

          .setSeedJASP(options)
          r <- try(.rankSumGibbsSampler(
            x = group1, y = group2, nSamples = options[["wilcoxonSamplesNumber"]], nBurnin = 0,
            cauchyPriorParameter = options[["priorWidth"]]
          ))

          if (isTryError(r)) {

            errorMessage <- .extractErrorMessage(r)
            ttestResults[["status"]][var] <- "error"
            ttestResults[["errorFootnotes"]][[var]] <- errorMessage
            ttestTable$addFootnote(message = errorMessage, rowNames = var)
            
          } else {
            ttestResults[["delta"]][[var]]  <- r[["deltaSamples"]]
            bf.raw <- .computeBayesFactorWilcoxon(
              deltaSamples         = r[["deltaSamples"]],
              cauchyPriorParameter = options[["priorWidth"]],
              oneSided             = oneSided)

            ttestRows[var, "rHat"] <- r[["rHat"]]

          }
        }

        if (!is.null(ttestResults[["delta"]][[var]]))
          ttestResults[["tValue"]][[var]] <- median(ttestResults[["delta"]][[var]])
        wValue <- unname(wilcox.test(group1, group2, paired = FALSE)[["statistic"]])
        error <- wValue

      }

      ttestResults[["BF10post"]][var] <- bf.raw
      BF <- JASP:::.recodeBFtype(bfOld     = bf.raw,
                          newBFtype = options[["bayesFactorType"]],
                          oldBFtype = "BF10"
      )

      msg <- .ttestBayesianCheckBFPlot(BF)
      if (!is.null(msg)) {
        ttestResults[["plottingError"]][[var]] <- msg
        ttestResults[["status"]][var] <- "error"
      }
      ttestRows[var, "BF"]    <- BF
      ttestRows[var, "error"] <- error
    }
    # set data construction is necessary for the slower rank based analysis
    ttestTable$setData(ttestRows)
    # ttestTable$addRows(row, rowNames = var)
  }

  ttestResults[["ttestRows"]] <- ttestRows
  return(ttestResults)

}

.ttestBISTTestMarkup <- function(options, derivedOptions, g1 = NULL, g2 = NULL) {

  jaspTable <- createJaspTable()
	jaspTable$dependOn(c("bayesFactorType", "variables"))

  jaspTable$title <- if (derivedOptions[["wilcoxTest"]]) {
    gettext("Bayesian Mann-Whitney U Test")
  } else {
    gettext("Bayesian Independent Samples T-Test")
  }
    
  if (options[["effectSizeStandardized"]] == "default" && !derivedOptions[["wilcoxTest"]]) {
    citations <- .ttestBayesianCitations[c("MoreyEtal2015", "RouderEtal2009")]
  } else if (derivedOptions[["wilcoxTest"]]) {
    citations <- .ttestBayesianCitations["vanDoornEtal2018"]
  } else if (options[["effectSizeStandardized"]] == "informative") {
    citations <- .ttestBayesianCitations["GronauEtal2017"]
  }

  jaspTable$addCitation(citations)

  jaspTable$addColumnInfo(name = "variable", title = "", type = "string")

  bfType <- options$bayesFactorType
  hypothesis <- switch(
    options[["hypothesis"]],
    "groupsNotEqual"  = "equal",
    "groupOneGreater" = "greater",
    "groupTwoGreater" = "smaller"
  )
  bfTitle <- .ttestBayesianGetBFTitle(bfType, hypothesis)
  jaspTable$addColumnInfo(name = "BF", type = "number", title = bfTitle)

  if (derivedOptions[["wilcoxTest"]]) {
    jaspTable$addColumnInfo(name = "error", type = "number", title = "W")
    jaspTable$addColumnInfo(name = "rHat", type = "number", title = "Rhat")
    jaspTable$addFootnote(gettextf("Result based on data augmentation algorithm with 5 chains of %.0f iterations.", options[["wilcoxonSamplesNumber"]]))
  } else {
    if (options[["hypothesis"]] == "groupsNotEqual") {
      fmt <- "sf:4;dp:3"
    } else {
      fmt <- "sf:4;dp:3;~"
    }
    jaspTable$addColumnInfo(name = "error", type = "number", format = fmt, title = gettext("error %"))
  }

  if (!(is.null(g1) || is.null(g2))) {
    message <- NULL
    if (options$hypothesis == "groupOneGreater") {
      message <- gettextf("For all tests, the alternative hypothesis specifies that group <em>%1$s</em> is greater than group <em>%2$s</em>.", g1, g2)
    } else if (options$hypothesis == "groupTwoGreater") {
      message <- gettextf("For all tests, the alternative hypothesis specifies that group <em>%1$s</em> is less than group <em>%2$s</em>.", g1, g2)
    }
    if (!is.null(message))
      jaspTable$addFootnote(message)
  }
  return(jaspTable)
}

# Wilcoxon functions ----
.rankSumGibbsSampler <- function(xVals, yVals, nSamples = 1e3, cauchyPriorParameter = 1/sqrt(2),
                                 nBurnin = 1, nGibbsIterations = 10, nChains = 5){

  n1 <- length(xVals)
  n2 <- length(yVals)

  allRanks <- rank(c(xVals,yVals))
  xRanks <- allRanks[1:n1]
  yRanks <- allRanks[(n1+1):(n1+n2)]

  deltaSamples <- numeric(nSamples)
  deltaSamplesMatrix <- matrix(ncol = nChains, nrow = nSamples-nBurnin)
  totalIterCount <- 0

  for(thisChain in 1:nChains) {

    currentVals <- sort(rnorm((n1+n2)))[allRanks] # initial values

    oldDeltaProp <- 0

    for (j in 1:nSamples) {

      for (i in sample(1:(n1+n2))) {

        currentRank <- allRanks[i]

        currentBounds <- .upperLowerTruncation(ranks=allRanks, values=currentVals, currentRank=currentRank)
        if (i <= n1) {
          oldDeltaProp <- -0.5*oldDeltaProp
        } else {
          oldDeltaProp <- 0.5*oldDeltaProp
        }

        currentVals[i] <- .truncNormSample(currentBounds[["under"]], currentBounds[["upper"]], mu=oldDeltaProp, sd=1)

      }
      
      xVals <- currentVals[1:n1]
      yVals <- currentVals[(n1+1):(n1+n2)]
      gibbsResult <- .sampleGibbsTwoSampleWilcoxon(x = xVals, y = yVals, nIter = nGibbsIterations,
                                                   rscale = cauchyPriorParameter)

      deltaSamples[j] <- oldDeltaProp <- gibbsResult
      progressbarTick()

    }
    if (nBurnin > 0) {
      deltaSamples <- -deltaSamples[-(1:nBurnin)]
    } else {
      deltaSamples <- -deltaSamples
    }
    deltaSamplesMatrix[, thisChain] <- deltaSamples
  }

  betweenChainVar <- (nSamples / (nChains - 1)) * sum((apply(deltaSamplesMatrix, 2, mean)  - mean(deltaSamplesMatrix))^2)
  withinChainVar <- (1/ nChains) * sum(apply(deltaSamplesMatrix, 2, var))

  fullVar <- ((nSamples - 1) / nSamples) * withinChainVar + (betweenChainVar / nSamples)
  rHat <- sqrt(fullVar/withinChainVar)

  return(list(deltaSamples = as.vector(deltaSamplesMatrix), rHat = rHat))
}

.sampleGibbsTwoSampleWilcoxon <- function(x, y, nIter = 10, rscale = 1/sqrt(2)) {
  meanx <- mean(x)
  meany <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sigmaSq <- 1 # Arbitrary number for sigma
  g <- 1
  for(i in 1:nIter){
    #sample mu
    varMu <- (4 * g * sigmaSq) / ( 4 + g * (n1 + n2) )
    meanMu <- (2 * g * (n2 * meany - n1 * meanx)) / ((g * (n1 + n2) + 4))
    mu <- rnorm(1, meanMu, sqrt(varMu))
    # sample g
    betaG <- (mu^2 + sigmaSq * rscale^2) / (2*sigmaSq)
    g <- 1/rgamma(1, 1, betaG)
    # convert to delta
    delta <- mu / sqrt(sigmaSq)
  }
  return(delta)
}

.truncNormSample <- function(lBound = -Inf, uBound = Inf, mu = 0, sd = 1) {

  lBoundUni <- pnorm(lBound, mean = mu, sd = sd)
  uBoundUni <- pnorm(uBound, mean = mu, sd = sd)
  mySample <- qnorm(runif(1, lBoundUni, uBoundUni), mean = mu, sd = sd)

  return(mySample)
}

.upperLowerTruncation <- function(ranks, values, currentRank, n, ranksAreIndices = FALSE) {

  if (currentRank == min(ranks)) {
    under <- -Inf
  } else {
    under <- max(values[ranks < currentRank])
  }

  if (currentRank == max(ranks)) {
    upper <- Inf
  } else {
    upper <- min(values[ranks > currentRank])
  }

  return(list(under=under, upper=upper))
}

.computeBayesFactorWilcoxon <- function(deltaSamples, cauchyPriorParameter, oneSided) {
  postDens <- logspline::logspline(deltaSamples)
  densZeroPoint <- logspline::dlogspline(0, postDens)
  priorDensZeroPoint <- dcauchy(0, scale = cauchyPriorParameter)

  corFactorPosterior <- logspline::plogspline(0, postDens)
  if (oneSided == "right")
    corFactorPosterior <- 1 - corFactorPosterior
  corFactorPrior <-  pcauchy(0, scale = cauchyPriorParameter, lower.tail = (oneSided != "right" ))

  bf <- if (isFALSE(oneSided)) priorDensZeroPoint / densZeroPoint else (priorDensZeroPoint / corFactorPrior) / (densZeroPoint / corFactorPosterior)

  return(bf)
}
