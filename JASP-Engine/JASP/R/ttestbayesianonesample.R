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
# ttestBOS = ttestBayesianOneSample
TTestBayesianOneSample <- function(jaspResults, dataset, options, state = NULL) {

  .ttestBayesianRunAnalysis(jaspResults, dataset, options, "one-sample")

}

.ttestBOSTTest <- function(ttestContainer, dataset, options, derivedOptions, errors, ttestState) {

  ttestTable <- .ttestBOSTTestMarkup(options, derivedOptions)
  ttestResults <- .ttestBayesianEmptyObject(options, derivedOptions, ttestState)
  ttestContainer[["ttestTable"]] <- ttestTable
  if (!derivedOptions[["ready"]])
    return(ttestResults)

  dependents <- options[["variables"]]

  # create empty object for the table, this has previously computed rows already filled in
  ttestRows <- .ttestBayesianCreateTtestRows(dependents, options, derivedOptions, ttestState)
  alreadyComputed <- !is.na(ttestRows[, "BF"]) & ttestResults[["hypothesis"]] == options[["hypothesis"]]
  
  ttestTable$setData(ttestRows)
  .ttestBayesianSetFootnotesMainTable(ttestTable, ttestResults, dependents[alreadyComputed])

  oneSided <- derivedOptions[["oneSided"]]
  bf.type <- options[["bayesFactorType"]]
  BFH1H0 <- ttestResults[["BFH1H0"]]
  .ttestBayesianInitBayesFactorPackageOptions()

  nvar <- length(dependents)
  if (derivedOptions[["wilcoxTest"]])
    .ttestBayesianSetupWilcoxProgressBar(nvar, ttestState, options[["wilcoxonSamplesNumber"]])
  
  for (var in dependents[!alreadyComputed]) {

    if (!isFALSE(errors[[var]])) {

      errorMessage <- errors[[var]]$message
      ttestTable$addFootnote(errorMessage, rowNames = var)
      ttestResults[["status"]][var] <- "error"
      ttestResults[["errorFootnotes"]][[var]] <- errorMessage
      ttestRows[var, c("BF", "error")] <- NaN

    } else {

      # these objects are made here so they don't need to be created every time a try fails,
      # which means they could be forgotten and not created
      bf.raw <- NaN
      error  <- NaN

      x <- dataset[[.v(var)]]
      x <- x[!is.na(x)]  - options[["testValue"]]

      if (!derivedOptions[["wilcoxTest"]]) {
        
        r <- try(.generalTtestBF(x = x, oneSided = oneSided, options = options))
  
        if (isTryError(r)) {
  
          errorMessage <- .extractErrorMessage(r)
          ttestResults[["status"]][var] <- "error"
          ttestResults[["errorFootnotes"]][[var]] <- errorMessage
          ttestTable$addFootnote(message = errorMessage, rowNames = var)
  
        } else {
  
          bf.raw <- r[["bf"]]
          error  <- r[["error"]]
          ttestResults[["tValue"]][[var]] <- r[["tValue"]]
          ttestResults[["n1"]][var]       <- r[["n1"]]
          # ttestResults[["n2"]][var]       <- r[["n2"]]
          ttestResults[["tValue"]][var]   <- r[["tValue"]]
  
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
          rHat <- ttestResults[["rHat"]][[var]]
          
        } else {
          
          .setSeedJASP(options)
          r <- try(.signRankGibbsSampler(
            x = x, nSamples = options[["wilcoxonSamplesNumber"]], testValue = 0, nBurnin = 0,
            cauchyPriorParameter = options[["priorWidth"]]
          ))
          
          if (isTryError(r)) {
            
            errorMessage <- .extractErrorMessage(r)
            ttestResults[["status"]][var] <- "error"
            ttestResults[["errorFootnotes"]][[var]] <- errorMessage
            ttestTable$addFootnote(message = errorMessage, rowNames = var)

          } else {
            
            ttestResults[["delta"]][[var]]  <- r[["deltaSamples"]] * -1
            ttestResults[["rHat"]][[var]]  <- r[["rHat"]]
            
            bf.raw <- .computeBayesFactorWilcoxon(
              deltaSamples         = r[["deltaSamples"]] * -1,
              cauchyPriorParameter = options[["priorWidth"]],
              oneSided             = oneSided)
            
            rHat <- r[["rHat"]]
            
          }
        }
        
        if (!is.null(ttestResults[["delta"]][[var]]))
          ttestResults[["tValue"]][[var]] <- median(ttestResults[["delta"]][[var]])
        ttestResults[["n1"]][var]       <- length(x)
        wValue <- unname(wilcox.test(x)[["statistic"]])
        error <- wValue
        ttestRows[var, "rHat"] <- rHat
        
      }
      
      ttestResults[["BF10post"]][var] <- bf.raw
      BF <- JASP:::.recodeBFtype(bfOld     = bf.raw,
                          newBFtype = bf.type,
                          oldBFtype = "BF10")

      msg <- .ttestBayesianCheckBFPlot(BF)
      if (!is.null(msg)) {
        ttestResults[["plottingError"]][[var]] <- msg
        ttestResults[["status"]][var] <- "error"
      }
      ttestRows[var, "BF"]    <- BF
      ttestRows[var, "error"] <- error
    }
    # set data construction to facilitate implementation of the slower rank based analysis
    ttestTable$setData(ttestRows)
  }

  # this is a standardized object. The names are identical to those in the other Bayesian t-tests
  ttestResults[["ttestRows"]] <- ttestRows

  return(ttestResults)

}

.ttestBOSTTestMarkup <- function(options, derivedOptions) {

  jaspTable <- createJaspTable()
  jaspTable$title <- if (derivedOptions[["wilcoxTest"]]) {
    gettext("Bayesian Wilcoxon Signed-Rank Test")
  } else {
    gettext("Bayesian One Sample T-Test")
  }
  
  jaspTable$dependOn(c("bayesFactorType", "variables", "testValue"))
  
  if (options[["effectSizeStandardized"]] == "default") {
    citations <- .ttestBayesianCitations[c("MoreyEtal2015", "RouderEtal2009")]
  } else if (derivedOptions[["wilcoxTest"]]) {
    citations <- .ttestBayesianCitations["vanDoornEtal2018"]
  } else if (options[["effectSizeStandardized"]] == "informative") {
    citations <- .ttestBayesianCitations["GronauEtal2017"]
  }
  jaspTable$addCitation(citations)

  bfType <- options[["bayesFactorType"]]
	hypothesis <- switch(options[["hypothesis"]],
	  "notEqualToTestValue"  = "equal",
	  "greaterThanTestValue" = "greater",
	  "lessThanTestValue"    = "smaller"
	)
	bfTitle <- .ttestBayesianGetBFTitle(bfType, hypothesis)

  testValueFormatted <- format(options[["testValue"]], drop0trailing = TRUE)
  message <- switch(
    options[["hypothesis"]],
    "greaterThanTestValue" = gettextf(
      "For all tests, the alternative hypothesis specifies that the population mean is greater than %s.",
      testValueFormatted
    ),
    "lessThanTestValue"    = gettextf(
      "For all tests, the alternative hypothesis specifies that the population mean is less than %s.",
      testValueFormatted
    ),
    "notEqualToTestValue"  = gettextf(
      "For all tests, the alternative hypothesis specifies that the population mean differs from %s.",
      testValueFormatted
    )
  )
  jaspTable$addFootnote(message)

  jaspTable$addColumnInfo(name = "variable", title = "",      type = "string")
  jaspTable$addColumnInfo(name = "BF",       title = bfTitle, type = "number")

  if (derivedOptions[["wilcoxTest"]]) {
    jaspTable$addColumnInfo(name = "error", type = "number", title = "W")
    jaspTable$addColumnInfo(name = "rHat", type = "number", title = "Rhat")
    jaspTable$addFootnote(gettextf("Result based on data augmentation algorithm with 5 chains of %.0f iterations.", options[["wilcoxonSamplesNumber"]]))
  } else {
    if (options[["hypothesis"]] == "notEqualToTestValue") {
      fmt <- "sf:4;dp:3"
    } else {
      fmt <- "sf:4;dp:3;~"
    }
    jaspTable$addColumnInfo(name = "error", type = "number", format = fmt, title = gettext("error %"))
  }
  
  return(jaspTable)
}


.oneSidedTtestBFRichard <- function(x=NULL, y=NULL, paired=FALSE, oneSided="right", r= sqrt(2)/2, iterations=10000) {

  # sample from delta posterior
  samples <- BayesFactor::ttestBF(x=x, y=y, paired=paired, posterior=TRUE, iterations=iterations, rscale=r, progress = FALSE)

  if (is.null(y) || paired) {

    N <- length(x)
    varBeta <- samples[,'sig2'] / (1 * N + 1/samples[,'g'])

    if (paired) {

      meanBeta <- sum(x - y) * varBeta / samples[,'sig2']

    } else {

      meanBeta <- sum(x) * varBeta / samples[,'sig2']
    }

  } else {

    sumN <- length(y) + length(x)
    diffN <- length(y) - length(x)

    varBeta <- samples[,'sig2'] / (sumN/4 + 1/samples[,'g'])

    meanBeta <- varBeta / samples[,'sig2'] * ((sum(x) - sum(y)) + samples[,'mu'] * (diffN)) / 2
  }

  logProbMin <- BayesFactor::logSummaryStats(pnorm(0, meanBeta, sqrt(varBeta), log=TRUE))$logMean

  BF <- BayesFactor::ttestBF(x, y, paired=paired, rscale=r)
  BF10 <- BayesFactor::extractBF(BF, onlybf = TRUE, logbf=TRUE)

  if (oneSided == "right") {

    logProbPlus = pexp(-logProbMin, log=TRUE)
    BFplus1 = log(2) + logProbPlus
    BFplus0 <- BFplus1 + BF10

    return(exp(BFplus0))

  } else if (oneSided == "left") {

    BFmin1 <- log(2) + logProbMin
    BFmin0 <- BFmin1 + BF10
    return(exp(BFmin0))
  }
}

.oneSidedTtestBFRichardAdaptive <- function(x=NULL, y=NULL, paired=FALSE, oneSided="right", r= sqrt(2)/2, nTests=5, nIterations=2000, criterion=.02, nMaxIterations=2050000) {

  variability <- criterion + 1

  while (variability > criterion && nIterations < nMaxIterations) {

    BF <- numeric(nTests)

    for (i in seq_len(nTests)) {

      BF[i] <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, r=r, oneSided = oneSided, iterations = nIterations)

    }

    variability <- sd(abs(log(BF))) / mean(abs(log(BF)))
    nIterations <- 2 * nIterations

  }

  return(mean(BF))

}

.likelihoodShiftedT <- function(par, data) {

  - sum(log( dt((data - par[1]) / par[2], par[3]) / par[2]))

}

.dposteriorShiftedT <- function(x, parameters, oneSided) {

  if (oneSided == FALSE) {

    dt((x - parameters[1]) / parameters[2], parameters[3]) / parameters[2]

  } else if (oneSided == "right") {

    ifelse (x >= 0, (dt((x - parameters[1]) / parameters[2], parameters[3]) / parameters[2]) / pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=FALSE) , 0 )

  } else if (oneSided == "left") {

    ifelse (x <= 0, (dt((x - parameters[1]) / parameters[2], parameters[3]) / parameters[2]) / pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE), 0)

  }

}

.qShiftedT <- function(q, parameters, oneSided) {

  if (oneSided == FALSE) {

    qt(q, df=parameters[3]) * parameters[2] + parameters[1]

  } else if (oneSided == "right") {

    areaSmallerZero <- pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE)

    qt(areaSmallerZero + q * (1 - areaSmallerZero), df=parameters[3]) * parameters[2] + parameters[1]

  } else if (oneSided == "left") {

    areaSmallerZero <- pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE)

    qt(q * areaSmallerZero, df=parameters[3]) * parameters[2] + parameters[1]

  }
}

# pdf cauchy prior
.dprior <- function(x, r, oneSided= oneSided){

  if (oneSided == "right") {

    y <- ifelse(x < 0, 0, 2/(pi*r*(1+(x/r)^2)))
    return(y)
  }

  if (oneSided == "left") {

    y <- ifelse(x > 0, 0, 2/(pi*r*(1+(x/r)^2)))
    return(y)
  }	else {

    return(1/(pi*r*(1+(x/r)^2)))
  }
}

.qt.shiftedT <- function(prob, parameters) {

  qt(prob, parameters[3]) * parameters[2] + parameters[1]

}

.posteriorSummaryGroupMean <- function(variable, descriptivesPlotsCredibleInterval=.95) {

  # Assumes that data are normally distributed
  # Jeffreys prior on mu and sigma: p(mu, sigma) proportional to 1/sigma
  # Compare Gelman et al. "Bayesian Data Analysis" for derivation of marginal posterior distribution of mu (inference for unknown mean and variance of a normal distribution)
  if (is.null(variable)) return(NULL)

  ciLower <- (1 - descriptivesPlotsCredibleInterval) / 2
  ciUpper <- ciLower + descriptivesPlotsCredibleInterval

  df <- length(variable) - 1
  location <- mean(variable)
  scale <- sd(variable) / sqrt(length(variable))

  outTmp <- .qt.shiftedT(c(ciLower, .5, ciUpper), parameters=c(location, scale, df))
  out <- list(ciLower=outTmp[1], median=outTmp[2], ciUpper=outTmp[3])

  return(out)

}


#-------------------------------------------------------------------------------
# GENERAL T-TEST BF FUNCTION
#-------------------------------------------------------------------------------
.generalTtestBF <- function(x = NULL, y = NULL, paired = FALSE, oneSided = FALSE, options) {

  tValue <- unname(t.test(x, y, paired = paired, var.equal = TRUE)$statistic)
  # numeric multiplication is more robust in R
  n1 <- as.numeric(length(x))
  n2 <- if (paired) 0 else as.numeric(length(y))
  method <- NULL

  if(options[["effectSizeStandardized"]] == "default") {

    ### default zero-centered Cauchy prior
    if (oneSided == FALSE) {
      nullInterval <- c(-Inf, Inf)
    } else if (oneSided == "right") {
      nullInterval <- c(0, Inf)
    } else if (oneSided == "left") {
      nullInterval <- c(-Inf, 0)
    }

    bfObject <- BayesFactor::ttest.tstat(
      t = tValue,
      n1 = n1,
      n2 = n2,
      rscale = options$priorWidth,
      nullInterval = nullInterval)

    bf    <- exp(bfObject$bf)
    error <- 100*bfObject$properror
    method <- bfObject[["method"]]

  } else if (options[["effectSizeStandardized"]] == "informative") {

    ### informed prior ###

    # Note that strictly speaking, in case of the independent samples t-test,
    # for the informed prior n1 corresponds to n2 and n2 to n1 and not vice-versa.
    # However, since in the expression for the Bayes factor they only appear
    # as an "effective" sample size and in the degrees of freedom for which it does
    # not matter whether we swap the two, we retain this order for easier extension
    # of the one-sample case.

    if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
      bfObject <- .bf10_t(t = tValue, n1 = n1, n2 = n2, oneSided = oneSided,
                          independentSamples = ! paired && !is.null(y),
                          prior.location = options[["informativeCauchyLocation"]],
                          prior.scale = options[["informativeCauchyScale"]],
                          prior.df = 1)
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "t") {
      bfObject <- .bf10_t(t = tValue, n1 = n1, n2 = n2, oneSided = oneSided,
                          independentSamples = ! paired && !is.null(y),
                          prior.location = options[["informativeTLocation"]],
                          prior.scale = options[["informativeTScale"]],
                          prior.df = options[["informativeTDf"]])
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
      bf <- .bf10_normal(t = tValue, n1 = n1, n2 = n2, oneSided = oneSided,
                         independentSamples = ! paired && !is.null(y),
                         prior.mean = options[["informativeNormalMean"]],
                         prior.variance = options[["informativeNormalStd"]]^2)
      error <- NULL
    }

  }

  return(list(bf = bf, error = error, tValue = tValue, n1 = n1, n2 = n2, method = method))
}
