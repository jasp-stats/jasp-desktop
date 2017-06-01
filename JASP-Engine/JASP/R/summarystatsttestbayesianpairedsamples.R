#
# Copyright (C) 2017 University of Amsterdam
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

SummaryStatsTTestBayesianPairedSamples <- function(dataset = NULL, options, perform = "run", callback = function(...) 0,  ...) {

	run <- (perform == "run")
	state <- .retrieveState()

	# difference between the previous state options and current options
	diff <- NULL

	if (!is.null(state)) {
		diff <- .diff(options, state$options)
	}

	# Bayes factor type (BF10, BF01, log(BF10)) and title
	bftype <- .getBayesfactorTitle.summarystats.ttest(
								bayesFactorType = options$bayesFactorType,
								hypothesis = options$hypothesis
							)
	bf.title <- bftype$bftitle
	BFH1H0 <- bftype$BFH1H0
	
	hypothesis.variables <- .hypothesisType.summarystats.ttest.paired(options$hypothesis)
	oneSided <- hypothesis.variables$oneSided

	# initialize variables
	plots.sumstats.ttest <- list()
	plotTypes <- list()
	priorAndPosteriorPlot <- NULL
	bayesFactorRobustnessPlot <- NULL

	outputTableElements <- .getOutputRow.summarystats.ttest.paired(
														run = run,
														options = options,
														state = state,
														diff = diff,
														hypothesis.variables = hypothesis.variables
													)
	rowsTTestBayesianPairedSamples <- outputTableElements$row
	bayesFactorObject <- outputTableElements$bayesFactorObject

	# Get prior and posterior plot
	if (options$plotPriorAndPosterior) {
		priorAndPosteriorPlot <- .getPriorAndPosteriorPlot.summarystats.ttest(
																run = run,
																options = options,
																state = state,
																diff = diff,
																bayesFactorObject = bayesFactorObject,
																oneSided = oneSided,
																paired = TRUE
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

	# Populate the output table
	meta <- list()
	meta[[1]] <- list(name = "table", type = "table")
	meta[[2]] <- list(name = "inferentialPlots", type = "object",
										meta = list(list(name = "PriorPosteriorPlot", type = "image"),
																list(name = "BFrobustnessPlot", type = "image"))
									)

	fields <- list()
	fields[[length(fields)+1]] <- list(name = "tStatistic", type = "number", format = "sf:4;dp:3", title = "t")
	fields[[length(fields)+1]] <- list(name = "n1Size", type = "number", title = "n")
	fields[[length(fields)+1]] <- list(name = "BF", type = "number", format = "sf:4;dp:3", title = bf.title)
	if (rowsTTestBayesianPairedSamples$errorEstimate != "NaN") {
		fields[[length(fields)+1]] <- list(name = "errorEstimate", type = "number", format = "sf:4;dp:3", title = "error %")
	}
	fields[[length(fields)+1]] <- list(name = "pValue", type = "number", format = "sf:4;dp:3", title = "p")

	# add footnotes to the analysis result
	footnotes <- .newFootnotes()
	if (options$hypothesis != "groupsNotEqual") {
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = hypothesis.variables$message)
	}

	table <- list()
	table[["title"]] <- "Bayesian Paired Samples T-Test"
	table[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
	table[["footnotes"]] <- as.list(footnotes)
	table[["schema"]] <- list(fields = fields)
	table[["data"]] <- list(rowsTTestBayesianPairedSamples)

	results <- list()
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian T-Test"
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

	keep <- NULL

	for (plot in plots.sumstats.ttest) {
		keep <- c(keep, plot$data)
	}

	if (run) {
		status <- "complete"
		state <- list(options = options, bayesFactorObject = bayesFactorObject,
								rowsTTestBayesianPairedSamples = rowsTTestBayesianPairedSamples,
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


.getOutputRow.summarystats.ttest.paired <- function(run, options, state, diff, hypothesis.variables) {
	# Returns a row to be shown in output tables
	#
	# Args:
	#   run: state of analysis - init or run
	#   options: a list of user options
	#   state: previous options state
	#   diff: diff between previous and current options
	#   hypothesis.variables: list of variables that depend on hypothesis type
	#
	# Output:
	#   list containing:
	#      row containing output elements to be shown in table
	#      Bayes factor object

	rowsTTestBayesianPairedSamples <- NULL
	bayesFactorObject <- NULL
	status <- NULL

	# If available from previous state, fetch it
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
			(is.list(diff) && (diff$bayesFactorType == FALSE && diff$tStatistic == FALSE &&
			diff$n1Size == FALSE && diff$priorWidth == FALSE && diff$hypothesis == FALSE))) &&
			!is.null(state$bayesFactorObject)) {

		rowsTTestBayesianPairedSamples <- state$rowsTTestBayesianPairedSamples
		bayesFactorObject <- state$bayesFactorObject

	} else {
		status <- .isInputValid.summarystats.ttest(options = options, independent = FALSE)
		rowsTTestBayesianPairedSamples <- status$row

		# if state of analysis is run
		if (run) {
			if (status$ready) {
				bayesFactorObject <- .calculateBF.summarystats.ttest(
																options = options,
																state = state,
																diff = diff,
																hypothesis.variables = hypothesis.variables
															)

				if (options$bayesFactorType == "BF10") {
					BF <- .clean(exp(bayesFactorObject$bf))
				} else if(options$bayesFactorType == "BF01") {
					BF <- .clean(1/exp(bayesFactorObject$bf))
				} else {
					BF <- .clean(bayesFactorObject$bf)
				}

				rowsTTestBayesianPairedSamples$BF <- BF
				rowsTTestBayesianPairedSamples$errorEstimate <- .clean(bayesFactorObject$properror)
				
				allPValues <- bayesFactorObject$pValue
				
				if (hypothesis.variables$oneSided == FALSE){
				  rowsTTestBayesianPairedSamples$pValue <- .clean(allPValues$twoSided)
				} else if (hypothesis.variables$oneSided == "left") {
				  rowsTTestBayesianPairedSamples$pValue <- .clean(allPValues$minSided)
				} else if (hypothesis.variables$oneSided == "right") {
				  rowsTTestBayesianPairedSamples$pValue <- .clean(allPValues$plusSided)
				}
			}
		}
	}

	return(list(row = rowsTTestBayesianPairedSamples, bayesFactorObject = bayesFactorObject))
}


.hypothesisType.summarystats.ttest.paired <- function(hypothesis) {
	# Returns different values that are based on the hypothesis chosen
	#   by the user
	#
	# Args:
	#   hypothesis: the hypothesis selected by user
	#
	# Output:
	#   list containing:
	#     nullInterval: vector containing lower and upper bounds of an interval hypothesis
	#     oneSided: if the hypothesis is one sided
	#
	nullInterval <- NULL
	oneSided <- FALSE

	if (hypothesis == "groupOneGreater") {

		nullInterval <- c(0, Inf)
		oneSided <- "right"
		message <- paste("For all tests, the alternative hypothesis specifies that measure 1 is greater than measure 2", sep = "")

	} else if (hypothesis == "groupTwoGreater") {

		nullInterval <- c(-Inf, 0)
		oneSided <- "left"
		message <- paste("For all tests, the alternative hypothesis specifies that measure 1 is lesser than measure 2", sep = "")
	}

	return(list(nullInterval = nullInterval,
							oneSided = oneSided,
							message = message)
				)
}
