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

SummaryStatsTTestBayesianOneSample <- function(dataset = NULL, options, perform = 'run', callback = function(...) 0,  ...) {

	run <- (perform == "run")
	state <- .retrieveState()

	options[["wilcoxTest"]] <- FALSE

	# difference between the previous state options and current options
	diff <- NULL

	if (!is.null(state)) {
		diff <- .diff(options, state$options)
	}

	### options contains:
	## essentials:
	#   tStatistic:       numeric, value of the t-statistic to be converted into a BF
	#   n1Size:           integer, the sample size
	#   hypothesis:       string, one of ["notEqualToTestValue", "greaterThanTestValue", "lessThanTestValue"]
	#   priorWidth:       numeric, width of the prior
	#   bayesFactorType:  string, one of ["BF10", "BF01", "LogBF10"]
	## plotting:
	#   plotPriorAndPosterior:                    logical, make this plot?
	#   plotPriorAndPosteriorAdditionalInfo:      logical,
	#   plotBayesFactorRobustness:                logical,
	#   plotBayesFactorRobustnessAdditionalInfo:  logical,
	## advanced analysis:
	#   effectSize:                     string, one of ["standardized", "dienes"]
	#   effectSizeStandardized:         string, one of ["default", "informative"]
	#   defaultStandardizedEffectSize:  string, can only be "cauchy" for now
	#  -
	#   informativeStandardizedEffectSize:  string, one of ["cauchy", "normal","t"]
	#   informativeCauchyLocation:      numeric, -3 ≤ value ≤ 3, distribution used is dcauchy((tStatistic - CauchyLocation) / CauchyScale)
	#   informativeCauchyScale:         numeric,
	#  -
	#   informativeTLocation:           numeric, -3 ≤ value ≤ 3, distribution used is dt((tStatistic - Tlocation) / Tscale, TDf)
	#   informativeTScale:              numeric, 0 ≤ value ≤ 2
	#   informativeTDf:                 integer, 1 ≤ value ≤ 500
	#  -
	#   informativeNormalMean:          numeric, -3 ≤ value ≤ 3, normal used is dnorm((tStatistic - NormalMean) / NormalStd)
	#   informativeNormalStd:           numeric, 0 ≤ value ≤ 2
	#  -
	#   dienesEffectSize:               string, one of ["uniform", "normal", "half_normal"]
	#   uniformDienesLowerBound:        numeric, 0 ≤ value ≤ 2, not guarantee to be smaller than uniformDienesUpperBound (???)
	#   uniformDienesUpperBound:        numeric, 0 ≤ value ≤ 2
	#   normalDienesMean:               numeric, 0 ≤ value ≤ 2
	#   normalDienesStd:                numeric, 0 ≤ value ≤ 2
	#   halfNormalDienesStd:            numeric, 0 ≤ value ≤ 2


  ### Compute results for the analysis

	hypothesis.variables <- .hypothesisType.summarystats.ttest.one(options$hypothesis)
	oneSided <- hypothesis.variables$oneSided

	outputTableElements <- .getOutputRow.summarystats.ttest.one(run = run,
	           options = options, state = state, diff = diff,
	           hypothesis.variables = hypothesis.variables)
	rowsTTestBayesianOneSample <- outputTableElements$row
	bayesFactorObject <- outputTableElements$bayesFactorObject



	### Define the Output: meta, table(s), plot(s), and results objects

	## Output Meta Description

	meta <- list()
	meta[[1]] <- list(name = "table", type = "table")
	meta[[2]] <- list(name = "inferentialPlots", type = "object",
	                  meta = list(
	                    list(name = "PriorPosteriorPlot", type = "image"),
	                    list(name = "BFrobustnessPlot", type = "image")
	                  )
	             )


	## Prepare Table Output

	table <- list()
	table[["title"]] <- "Bayesian One Sample T-Test"

	# BF title used in table
	bftype <- .getBayesfactorTitle.summarystats.ttest(
	  bayesFactorType = options$bayesFactorType, hypothesis = options$hypothesis)
	bf.title <- bftype$bftitle
	#BFH1H0 <- bftype$BFH1H0

	# Define table schema
	fields <- list(
	  list(name = "tStatistic", type = "number", format = "sf:4;dp:3", title = "t"),
	  list(name = "n1Size", type = "number", title = "n"),
	  list(name = "BF", type = "number", format = "sf:4;dp:3", title = bf.title)
	)
	if (rowsTTestBayesianOneSample$errorEstimate != "NaN") {
	  fields[[length(fields)+1]] <-
	    list(name = "errorEstimate", type = "number", format = "sf:4;dp:3", title = "error %")
	}
	fields[[length(fields)+1]] <- list(name = "pValue", type = "number", format = "sf:4;dp:3", title = "p")
	table[["schema"]] <- list(fields = fields)

	# Populate the table
	table[["data"]] <- list(rowsTTestBayesianOneSample)

	# Add footnotes to the analysis result
	footnotes <- .newFootnotes()
	if (options$hypothesis != "notEqualToTestValue") {
	  .addFootnote(footnotes, symbol = "<em>Note.</em>", text = hypothesis.variables$message)
	}
	table[["footnotes"]] <- as.list(footnotes)

	# Add citation reference list
	if (options$effectSizeStandardized == "default") {
	  table[["citation"]] <- list(
	    "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
	    "Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
	} else if (options$effectSizeStandardized == "informative") {
	  table[["citation"]] <- list(
	    "Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2017). Informed Bayesian T-Tests. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1704.02479")
	}# Prepare Plot Output

	# initialize variables
	plots.sumstats.ttest <- list()
	plotTypes <- list()
	priorAndPosteriorPlot <- NULL
	bayesFactorRobustnessPlot <- NULL

	# Get prior and posterior plot
	if (options$plotPriorAndPosterior) {
		priorAndPosteriorPlot <- .getPriorAndPosteriorPlot.summarystats.ttest(
																run = run,
																options = options,
																state = state,
																diff = diff,
																bayesFactorObject = bayesFactorObject,
																oneSided = oneSided,
																paired = FALSE
															)
		plots.sumstats.ttest[[length(plots.sumstats.ttest) + 1]] <- priorAndPosteriorPlot
		if(options$plotPriorAndPosteriorAdditionalInfo) {
			plotTypes[[length(plotTypes) + 1]] <- "posteriorPlotAddInfo"
		} else {
			plotTypes[[length(plotTypes) + 1]] <- "posteriorPlot"
		}
	}

	# Get Bayes factor robustness plot
	if (options$plotBayesFactorRobustness) {
		bayesFactorRobustnessPlot <- .getBayesFactorRobustnessPlot.summarystats.ttest(
																		run = run,
																		options = options,
																		state = state,
																		diff = diff,
																		bayesFactorObject = bayesFactorObject,
																		oneSided = oneSided
																	)
		plots.sumstats.ttest[[length(plots.sumstats.ttest) + 1]] <- bayesFactorRobustnessPlot
		if(options$plotBayesFactorRobustnessAdditionalInfo) {
			plotTypes[[length(plotTypes) + 1]] <- "robustnessPlotAddInfo"
		} else {
			plotTypes[[length(plotTypes) + 1]] <- "robustnessPlot"
		}
	}


  ### Create results object to be returned

	results <- list()
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian One Sample T-Test"
	results[["table"]] <- table

	if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness) {
		results[["inferentialPlots"]] <- list(
										title = ifelse(sum(c(options$plotPriorAndPosterior,
																				options$plotBayesFactorRobustness)) > 1,
														"Inferential Plots",
														"Inferential Plot"),
										PriorPosteriorPlot = priorAndPosteriorPlot,
										BFrobustnessPlot = bayesFactorRobustnessPlot
								)
	}

	### Finish off: collect objects to keep; update status and state

	keep <- NULL
	for (plot in plots.sumstats.ttest) {
		keep <- c(keep, plot$data)
	}

	if (run) {
		status <- "complete"
		state <- list(options = options, bayesFactorObject = bayesFactorObject,
								rowsTTestBayesianOneSample = rowsTTestBayesianOneSample,
								plotsTtest = plots.sumstats.ttest, plotTypes = plotTypes)
	} else {
		status <- "inited"
	}

	return(list(results = results,
							status = status,
							state = state,
							keep = keep)
				)
}


.getOutputRow.summarystats.ttest.one <-
  function(run, options, state, diff, hypothesis.variables) {
	# Returns a row to be shown in output tables
	#
	# Args:
	#   run: state of analysis - init or run
	#   options: a list of user options
	#   state: previous options state
	#   diff: diff between previous and current options
	#
	# Output:
	#   list containing:
	#      row containing output elements to be shown in table
	#      Bayes factor object

	rowsTTestBayesianOneSample <- NULL
	bayesFactorObject <- NULL
	status <- NULL

	## If previous table row is available and still valid, return it; otherwise compute it

	if (!is.null(state) && !is.null(diff) && !is.null(state$bayesFactorObject) &&
	    !any(unlist(diff))) {

		rowsTTestBayesianOneSample <- state$rowsTTestBayesianOneSample
		bayesFactorObject <- state$bayesFactorObject

	} else {

		status <- .isInputValid.summarystats.ttest(options = options, independent = FALSE)
		rowsTTestBayesianOneSample <- status$row

		if (run) {
			if (status$ready) {

			  ## Compute the statistics

			  bayesFactorObject <- .generalSummaryTtestBF(options = options)


			  ## Format the statistics for output

			  bf <- bayesFactorObject$bf
			  BF <- switch(options$bayesFactorType, BF10=bf, BF01=1/bf, log(bf))

				allPValues <- bayesFactorObject$pValue
				pValue <- switch(as.character(hypothesis.variables$oneSided), left=allPValues$minSided,
				                 right=allPValues$plusSided, allPValues$twoSided)

				## Store statistics in table row ouput structure

				rowsTTestBayesianOneSample$BF <- .clean(BF)
				rowsTTestBayesianOneSample$errorEstimate <- .clean(bayesFactorObject$properror)
				rowsTTestBayesianOneSample$pValue <- .clean(pValue)

			}
		}
	}
	return(list(row = rowsTTestBayesianOneSample, bayesFactorObject = bayesFactorObject))
}


.hypothesisType.summarystats.ttest.one <- function(hypothesis) {
	# Returns different values that are based on the hypothesis chosen
	#   by the user
	#
	# Args:
	#   hypothesis: the hypothesis selected by user (one of
  #     "notEqualToTestValue", "greaterThanTestValue", or "lessThanTestValue")
	#
	# Output:
	#   list containing:
	#     nullInterval: numeric vector containing lower and upper bounds of an interval hypothesis
	#     oneSided: boolean or string, one of {TRUE, "right", "left"}
  #     message:  string, if oneSided is a string, table footnote that specifies alternative hypothesis
	#
	nullInterval <- NULL
	oneSided <- FALSE

	if (hypothesis == "greaterThanTestValue") {

		nullInterval <- c(0, Inf)
		oneSided <- "right"
		message <- paste("For all tests, the alternative hypothesis specifies that the mean is greater than 0.", sep = "")

	} else if (hypothesis == "lessThanTestValue") {

		nullInterval <- c(-Inf, 0)
		oneSided <- "left"
		message <- paste("For all tests, the alternative hypothesis specifies that the mean is lesser than 0.", sep = "")
	}

	return(list(nullInterval = nullInterval,
							oneSided = oneSided,
							message = message)
				)
}



### GENERAL One Sample T-TEST BF from Summary statistic FUNCTION

# TODO(raoul): - Change to combined one-sample/two-sample/dependent t-test function
#              - Add uniform informed prior

.generalSummaryTtestBF <-
  function(tValue=options$tStatistic, size=options$n1Size, options, paired=TRUE) {
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
    # for the informed prior n1 corresponds to nx and n2 to ny and not vice-versa.
    # However, since in the expression for the Bayes factor they only appear
    # as an "effective" sample size and in the degrees of freedom for which it does
    # not matter whether we swap the two, we retain this order for easier extension
    # of the one-sample case.

    side = switch(options$hypothesis, greaterThanTestValue = "right", groupOneGreater = "right",
                  lessThanTestValue= "left", groupTwoGreater = "left", FALSE)

    # Note: .bf10_ functions gives weired value if paired = FALSE in single sample case
    if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
      bfObject <- .bf10_t(t = tValue, ny = n1, nx = n2, oneSided = side,
                          independentSamples = !paired,
                          prior.location = options[["informativeCauchyLocation"]],
                          prior.scale = options[["informativeCauchyScale"]],
                          prior.df = 1)
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "t") {
      bfObject <- .bf10_t(t = tValue, ny = n1, nx = n2, oneSided = side,
                          independentSamples = !paired,
                          prior.location = options[["informativeTLocation"]],
                          prior.scale = options[["informativeTScale"]],
                          prior.df = options[["informativeTDf"]])
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
      bf <- .bf10_normal(t = tValue, ny = n1, nx = n2, oneSided = side,
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
