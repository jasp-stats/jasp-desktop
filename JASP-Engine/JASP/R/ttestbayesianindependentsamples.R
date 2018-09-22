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
TTestBayesianIndependentSamples <- function(jaspResults, dataset, options, state = NULL) {

  # initialize & error handling
  if (options$testStatistic ==  "Wilcoxon") {
    jaspResults$title <- "Bayesian Mann-Whitney U Test"
  } else {
    jaspResults$title <- "Bayesian Independent Samples T-Test"
  }
	options <- .ttestBayesianInitOptions(jaspResults, options, "independent")
	dataset <- .ttestBayesianReadData(dataset, options[["variables"]],
	                                  options[["groupingVariable"]], options[["missingValues"]])
	errors  <- .ttestBayesianGetErrorsPerVariable(options = options, dataset = dataset)

	# Main analysis
	ttestResults <- .ttestBISTTest(jaspResults, dataset, options, errors)

	# create descriptives table and plots
  .ttestBayesianDescriptives(jaspResults, dataset, options, errors)

  # create inferential plots
	.ttestBayesianInferentialPlots(jaspResults, dataset, options, ttestResults, errors)

	return()
}

# Execute t-test ----
.ttestBISTTest <- function(jaspResults, dataset, options, errors) {

	# this function is the main workhorse, and also makes a table

	# check if we actually need to compute things
  if (!is.null(jaspResults[["ttestTable"]]) && !options[["anyNewVariables"]])
  	return(jaspResults[["stateTTestResults"]]$object)

	dependents <- options[["variables"]]
	ttestTable <- createJaspTable(title = "")
	jaspResults[["ttestTable"]] <- ttestTable
	dependencies <- options[["stateKey"]][["ttestResults"]]
	ttestTable$dependOnOptions(c(dependencies, "bayesFactorType", "variables"))

	grouping   <- options$groupingVariable
	levels <- base::levels(dataset[[.v(grouping)]])
	g1 <- levels[1L]
	g2 <- levels[2L]

  # does all addcolumninfo etc.
	.ttestBISTTestMarkup(ttestTable, options, g1, g2)
  ttestResults <- .ttestBayesianEmptyObject(options)

	if (!options[["canDoAnalysis"]]) {

		# user provided no grouping variable or empty columns
    dat <- data.frame(variable = dependents)
    ttestTable$setData(dat)
  	ttestTable$status <- "complete"
		return(ttestResults)

	}

  # we can do the analysis
  # get state
  ttestState <- jaspResults[["stateTTestResults"]]$object
  ttestRows <- ttestState$ttestRows

  nvar <- length(dependents)

  BFH1H0 <- !options$bayesFactorType == "BF01"

  oneSided <- options[["oneSided"]]

  if (options$wilcoxTest) {

  	# all variables - the ones we sampled before
  	todo <- nvar
  	if (!is.null(ttestState$delta))
  		todo <- todo - sum(sapply(ttestState$delta, function(x) isTRUE(!is.null(x) && !is.na(x))))
  	if (todo > 0)
  		jaspResults$startProgressbar(expectedTicks = todo * options[["wilcoxonSamplesNumber"]],
  																 timeBetweenUpdatesInMs = 100)

  }

  idxg1 <- dataset[[.v(options$groupingVariable)]] == g1
  idxg2 <- dataset[[.v(options$groupingVariable)]] == g2

  idxNAg <- is.na(dataset[[.v(grouping)]])
  for (var in dependents) {

  	# BayesFactor package doesn't handle NAs, so it is necessary to exclude them
  	subDataSet <- dataset[, .v(c(var, grouping))]
  	idxNA <- is.na(subDataSet[[1]]) | idxNAg
  	subDataSet <- subDataSet[!idxNA, ]

  	group1 <- subDataSet[idxg1[!idxNA], 1L]
  	group2 <- subDataSet[idxg2[!idxNA], 1L]

  	if (!is.null(ttestRows[[var]])) {

  		# row retrieved from state, only possible change is BF01 to BF10/ log(BF01)
  		ttestRows[[var]][["BF"]] <-
  			.recodeBFtype(bfOld     = ttestRows[[var]][["BF"]],
  										newBFtype = options[["bayesFactorType"]],
  										oldBFtype = ttestState[["bayesFactorType"]]
  			)

  		row <- ttestRows[[var]]

  	} else { # compute row

  		if (!isFALSE(errors[[var]])) {

  			errorMessage <- errors[[var]]$message
  			ttestTable$addFootnote(errorMessage, row_names = var)
  			ttestResults$status[var] <- "error"
  			errorFootnotes[var] <- errorMessage
  			BF = .clean(NaN)
  			error = ""

  		} else {
  			errorMessage <- NULL

  			ttestResults$n1[var] <- length(group1)
  			ttestResults$n2[var] <- length(group2)

  			if (!options$wilcoxTest) {

  				r <- try (silent = FALSE, expr =
  					.generalTtestBF(x = group1, y = group2, paired = FALSE, oneSided = oneSided, options = options)
  				)

  				if (isTryError(r))
  					r <- list(bf = NA, error = "", tValue = NA)

  				bf.raw <- r[["bf"]]
  				error <- .clean(r[["error"]])
  				ttestResults$tValue[var] <- r[["tValue"]]
  				ttestResults$delta[[var]] <- NA

  			} else if (options$wilcoxTest) {

  				# If the samples can be reused, don't call the Gibbs sampler again, but recalculate the
  				# Bayes factor with new settings and take the samples from state.
  				if (!is.null(ttestRows$delta[[var]]) && !is.na(ttestRows$delta[[var]])) {

  					bf.raw <- try(silent = FALSE, expr =
  						.ttestBISComputeBayesFactorWilcoxon(
  							deltaSamples         = ttestRows$delta[[var]],
  							cauchyPriorParameter = options$priorWidth,
  							oneSided             = oneSided)
  					)

  				} else {

  					r <- try(silent = FALSE, expr =
  						.ttestBISRankSumGibbsSampler(
  							x = group1, y = group2, nSamples = options$wilcoxonSamplesNumber, nBurnin = 0,
  							cauchyPriorParameter = options$priorWidth, jaspResults = jaspResults)
  					)

  					if (isTryError(r)) {
  						ttestResults$delta[[var]] <- NULL
  						bf.raw <- NA
  					} else {
  						ttestResults$delta[[var]] <- r[["deltaSamples"]]
  						bf.raw <- .ttestBISComputeBayesFactorWilcoxon(
  							deltaSamples         = r[["deltaSamples"]],
  							cauchyPriorParameter = options$priorWidth,
  							oneSided             = oneSided)
  					}
  				}

  				wValue <- unname(wilcox.test(group2, group1, paired = FALSE)$statistic)
  				error <- wValue
  				ttestResults$tValue[var] <- median(ttestResults$delta[[var]])

  			}

  			bf.raw <- .recodeBFtype(bfOld     = bf.raw,
  															newBFtype = options[["bayesFactorType"]],
  															oldBFtype = "BF10"
  			)

  			ttestResults$BF10post[var] <- bf.raw
  			BF <- .clean(bf.raw)

  			if (is.na(bf.raw)) {
  				ttestResults$status[var] <- "error"
  				plottingError[var] <- "Plotting is not possible: Bayes factor could not be calculated"
  			}

  			if (is.infinite(bf.raw) || is.infinite(1 / bf.raw)) {
  				ttestResults$status[var] <- "error"
  				if (is.infinite(bf.raw)) {
  					ttestResults$plottingError[var] <- "Plotting is not possible: Bayes factor is infinite"
  				} else {
  					ttestResults$plottingError[var] <- "Plotting is not possible: The Bayes factor is too small"
  				}
  			}

  			if (!is.null(errorMessage)) {

  				BF = .clean(NaN)
  				error = ""
  				ttestTable$addFootnote(errorMessage, row_names = var)
  				ttestResults$status[var] <- "error"
  				errorFootnotes[var] <- errorMessage
  			}
  		}
  		row <- list(variable = var, BF = BF, error = error)
  	}
  	ttestRows[[var]] <- row
  	ttestTable$addRows(row, rowNames = var)
  }
  # this is a standardized object. The names are identical to those in the other Bayesian t-tests
  ttestResults$ttestRows <- ttestRows
  ttestResults$BFH1H0    <- BFH1H0

  tmp <- createJaspState(
  	object = ttestResults,
  	title = "mainResultsObject"
  )
  tmp$dependOnOptions(dependencies)
  jaspResults[["stateTTestResults"]] <- tmp
  ttestTable$status <- "complete"

	return(ttestResults)
}

.ttestBISTTestMarkup <- function(jaspTable, options, g1 = NULL, g2 = NULL) {

	jaspTable$title <- ifelse(options$wilcoxTest, "Bayesian Mann-Whitney U Test", "Bayesian Independent Samples T-Test")
		if (options$effectSizeStandardized == "default" & !options$wilcoxTest) {
			citations <- list(
				"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
				"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237."
			)
		} else if (options$wilcoxTest) {
			citations <- list(
				"van Doorn, J., Ly, A., Marsman, M., & Wagenmakers, E. J. (2018). Bayesian Latent-Normal Inference for the Rank Sum Test, the Signed Rank Test, and Spearman's rho. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1703.01805"
			)
		} else if (options$effectSizeStandardized == "informative") {
			citations <- list(
				"Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2017). Informed Bayesian T-Tests. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1704.02479"
			)
		}

		for (c in citations)
			jaspTable$addCitation(c)

		jaspTable$addColumnInfo(name = "variable", title = "", type = "string")

		bfType <- options$bayesFactorType
		hypothesis <- switch(
		  options[["hypothesis"]],
		  "groupsNotEqual"  = "equal",
		  "groupOneGreater" = "greater",
		  "groupTwoGreater" = "smaller"
		)
		bfTitle <- .ttestBayesianGetBFTitle(bfType, hypothesis)
		jaspTable$addColumnInfo(name = "BF", type = "number", format = "sf:4;dp:3", title = bfTitle)

		if (!options$wilcoxTest) {
		  errTitle <- "error %"
		  if (options$hypothesis != "groupsNotEqual") {
		    fmt <- "sf:4;dp:3;~"
		  } else {
		    fmt <- "sf:4;dp:3"
		  }
	  } else {
		  errTitle <- "W"
		  fmt <- "sf:4;dp:3;"
		}
		jaspTable$addColumnInfo(name = "error", type = "number", format = fmt, title = errTitle)

		if (!(is.null(g1) || is.null(g2))) {
		  message <- NULL
		  m0 <- "For all tests, the alternative hypothesis specifies that group <em>"
		  if (options$hypothesis == "groupOneGreater") {
		    message <- paste0(m0, g1, "</em> is greater than group <em>", g2, "</em>.")
		  } else if (options$hypothesis == "groupTwoGreater") {
		    message <- paste0(m0, g1, "</em> is less than group <em>", g2, "</em>.")
		  }
		  if (!is.null(message))
		    jaspTable$addFootnote(message)
	  }
}

# Wilcoxon functions ----
.ttestBISRankSumGibbsSampler <- function(xVals, yVals, nSamples = 1e3, cauchyPriorParameter = 1 / sqrt(2),
                                 nBurnin = 0, nGibbsIterations = 10, jaspResults){
  n1 <- length(xVals)
  n2 <- length(yVals)
  allRanks <- rank(c(xVals, yVals))
  xRanks <- allRanks[1:n1]
  yRanks <- allRanks[(n1 + 1):(n1 + n2)]

  allVals <- sort(rnorm(n1 + n2))[allRanks] # initial values
  nSamples <- nSamples + nBurnin
  deltaSamples <- gSamples <- muSamples <- numeric(nSamples)

  oldMuProp <- oldDeltaProp <- 0

  for (j in 1:nSamples) {

    # if (j %% 1e2 == 0 ) {
    #   response <- progressbar()
    #   if (response[["status"]] != "ok")
    #     return()
    # }

    for (i in sample(1:(n1 + n2))) {
      underx <- allVals[allRanks < allRanks[i]][order(allVals[allRanks < allRanks[i]], decreasing = TRUE)][1]
      upperx <- allVals[allRanks > allRanks[i]][order(allVals[allRanks > allRanks[i]], decreasing = FALSE)][1]
      if (is.na(underx)) {underx <- -Inf}
      if (is.na(upperx)) {upperx <- Inf}

      if (i <= n1) {
        allVals[i] <- .ttestBISTruncNormSample(mu = (-0.5 * oldMuProp), sd = 1, lBound = underx, uBound = upperx)
      } else if (i > n1) {
        allVals[i] <- .ttestBISTruncNormSample(mu = (0.5 * oldMuProp), sd = 1, lBound = underx, uBound = upperx)
      }
    }

    xVals <- allVals[1:n1]
    yVals <- allVals[(n1 + 1):(n1 + n2)]

    gibbsResult <- .ttestBISSampleGibbsTwoSampleWilcoxon(
      x = xVals, y = yVals, n1 = n1, n2 = n2,
      nIter = nGibbsIterations, rscale = cauchyPriorParameter
    )

    muSamples[j] <- oldMuProp <- gibbsResult[3]
    deltaSamples[j] <- oldDeltaProp <- gibbsResult[1]
    jaspResults$progressbarTick()
  }

  deltaSamples <- -1 * deltaSamples[- (1:nBurnin)]

  return(list(deltaSamples = deltaSamples))
}

.ttestBISSampleGibbsTwoSampleWilcoxon <- function(x, y, n1, n2, nIter = 10, rscale = 1 / sqrt(2)) {
  meanx <- mean(x)
  meany <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sigmaSq <- 1 # Arbitrary number for sigma
  g <- 1
  for (i in 1:nIter){
    #sample mu
    varMu <- (4 * g * sigmaSq) / (4 + g * (n1 + n2))
    meanMu <- (2 * g * (n2 * meany - n1 * meanx)) / ((g * (n1 + n2) + 4))
    mu <- rnorm(1, meanMu, sqrt(varMu))
    # sample g
    betaG <- (mu^2 + sigmaSq * rscale^2) / (2 * sigmaSq)
    g <- 1 / rgamma(1, 1, betaG)
    # convert to delta
    delta <- mu / sqrt(sigmaSq)
  }
  return(c(delta, sigmaSq, mu, g))
}

.ttestBISTruncNormSample <- function(lBound = -Inf, uBound = Inf, mu = 0, sd = 1) {

  lBoundUni <- pnorm(lBound, mean = mu, sd = sd)
  uBoundUni <- pnorm(uBound, mean = mu, sd = sd)
  mySample <- qnorm(runif(1, lBoundUni, uBoundUni), mean = mu, sd = sd)

  return(mySample)
}

.ttestBISComputeBayesFactorWilcoxon <- function(deltaSamples, cauchyPriorParameter, oneSided) {
  postDens <- logspline::logspline(deltaSamples)
  densZeroPoint <- logspline::dlogspline(0, postDens)
  priorDensZeroPoint <- dcauchy(0, scale = cauchyPriorParameter)

  if (oneSided == "right") {
    corFactorPosterior <- 1 - logspline::plogspline(0, postDens)
  } else {
    corFactorPosterior <- logspline::plogspline(0, postDens)
  }
  corFactorPrior <-  pcauchy(0, scale = cauchyPriorParameter, lower.tail = (oneSided != "right"))

  bf <- ifelse(oneSided == FALSE,  priorDensZeroPoint / densZeroPoint,
               (priorDensZeroPoint / corFactorPrior) / (densZeroPoint / corFactorPosterior))
  return(bf)
}

