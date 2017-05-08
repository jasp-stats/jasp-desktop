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

SummaryStatsBinomialTestBayesian <- function(dataset = NULL, options, perform = 'run', callback = function(...) 0,  ...) {

	# Initialize variables
	state <- .retrieveState()
	run <- (perform == "run")
	diff <- NULL
	priorAndPosteriorPlot <- NULL

	# Difference between the previous state variables(options) and current options
	if (!is.null(state)) {
		diff <- .diff(options, state$options)
	}

	# Bayes factor type (BF10, BF01, log(BF10)) and title
	bftype <- .getBayesfactorTitle.summarystats.ttest(
								bayesFactorType = options$bayesFactorType,
								hypothesis = options$hypothesis
							)
	bf.title <- bftype$bftitle

	hypothesis.variables <- .hypothesisType.summarystats.binomial(
																							hypothesis = options$hypothesis,
																							test.value = options$testValue
																						)
	hyp <- hypothesis.variables$hyp

	# Output table row and Bayes factor objec
	outputTableElements <- .getOutputRow.summarystats.binomial(
														run = run,
														options = options,
														state = state,
														diff = diff,
														hyp = hyp
													)
	rowsBinomialTest <- outputTableElements$row
	bayesFactorObject <- outputTableElements$bayesFactorObject

	# Prior and Posterior plot
	if (options$plotPriorAndPosterior) {
		priorAndPosteriorPlot <- .getPriorAndPosteriorPlot.summarystats.binomial(
																run = run,
																options = options,
																state = state,
																diff = diff,
																bayesFactorObject = bayesFactorObject,
																hyp = hyp
															)
	}

	# Add footnotes to the analysis result
	footnotes <- .newFootnotes()
	.addFootnote(footnotes, symbol = "<em>Note.</em>", text = hypothesis.variables$message)

	# Populate the output table
	meta <- list()
	meta[[1]] <- list(name = "table", type = "table")
	meta[[2]] <- list(name = "inferentialPlots", type = "object",
										meta = list(list(name = "PriorPosteriorPlot", type = "image")))

	fields <- list()
	fields[[length(fields)+1]] <- list(name = "successes", type = "integer", title = "Successes")
	fields[[length(fields)+1]] <- list(name = "failures", type = "integer", title = "Failures")
	fields[[length(fields)+1]] <- list(name = "testValue", type = "number", title = "Test value")
	fields[[length(fields)+1]] <- list(name = "BF", type = "number", format = "sf:4;dp:3", title = bf.title)
	fields[[length(fields)+1]] <- list(name = "pValue", type = "number", format = "sf:4;dp:3", title = "p")

	table <- list()
	table[["title"]] <- "Bayesian Binomial Test"
	table[["schema"]] <- list(fields=fields)
	table[["citation"]] <- list("Jeffreys, H. (1961). Theory of Probability. Oxford, Oxford University Press.",
								"O’Hagan, A., & Forster, J. (2004). Kendall’s advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.). London: Arnold.",
								"Haldane, J. B. S. (1932). A note on inverse probability. Mathematical Proceedings of the Cambridge Philosophical Society, 28, 55-61.")
	table[["footnotes"]] <- as.list(footnotes)
	table[["data"]] <- list(rowsBinomialTest)

	results <- list()
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Binomial Test"
	if (options$plotPriorAndPosterior) {
		results[["inferentialPlots"]] <- list(title = "Inferential Plot", PriorPosteriorPlot = priorAndPosteriorPlot)
	}
	results[["table"]] <- table

	keep <- NULL

	if (options$plotPriorAndPosterior) {
		keep <- c(keep, priorAndPosteriorPlot$data)
	}

	# return values
	if (run) {
		status <- "complete"
		state <- list(options = options, bayesFactorObject = bayesFactorObject,
								rowsBinomialTest = rowsBinomialTest,
								priorAndPosteriorPlot = priorAndPosteriorPlot)
	} else {
		status <- "inited"
	}

	return(list(results = results,
							status = status,
							state = state,
							keep = keep)
				)
}


.getPriorAndPosteriorPlot.summarystats.binomial <- function(
																									run, options, state,
																									diff, bayesFactorObject,
																									hyp) {
	# Returns the prior and posterior plot. If available from previous,
	#   the function returns that. Else, it calls the plotPosterior function
	#
	# Args:
	#   run: state of analysis - init or run
	#   options: a list of options given by user
	#   state: previous state variables
	#   bayesFactorObject: Bayes factor object containing bf and properror
	#   hyp: type of hypothesis
	#
	# Output:
	#   plot - prior and posterior plot

	returnPlot <- NULL

	# Check if available from previous state
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
			(is.list(diff) && (diff$successes == FALSE && diff$failures == FALSE &&
			diff$testValue == FALSE && diff$hypothesis == FALSE &&
			diff$betaPriorParamA == FALSE && diff$betaPriorParamB == FALSE &&
			diff$plotPriorAndPosteriorAdditionalInfo == FALSE)) &&
			!is.null(state$priorAndPosteriorPlot))) {

		returnPlot <- state$priorAndPosteriorPlot

	} else {

		width  <- 530
		height <- 400

		plot <- list()
		plot[["title"]]  <- "Prior and Posterior"
		plot[["width"]]  <- width
		plot[["height"]] <- height
		plot[["status"]] <- "waiting"

		dontPlotData <- TRUE

		if (run) {
			dontPlotData <- FALSE
		}

		p <- try(silent = FALSE, expr = {
			# image <- .beginSaveImage(width, height)
			# .plotPosterior.binomTest(
			# 		counts = options$successes, n = (options$failures + options$successes),
			# 		theta0 = options$testValue, a = options$betaPriorParamA,
			# 		b = options$betaPriorParamB, BF10 = bayesFactorObject, hypothesis = hyp,
			# 		addInformation = options$plotPriorAndPosteriorAdditionalInfo,
			# 		dontPlotData = dontPlotData
			# 	)
			# plot[["data"]] <- .endSaveImage(image)
			
			.plotFunc <- function() {
				.plotPosterior.binomTest(
					counts = options$successes, n = (options$failures + options$successes),
					theta0 = options$testValue, a = options$betaPriorParamA,
					b = options$betaPriorParamB, BF10 = bayesFactorObject, hypothesis = hyp,
					addInformation = options$plotPriorAndPosteriorAdditionalInfo,
					dontPlotData = dontPlotData
				)
			}
			content <- .writeImage(width = width, height = height, plot = .plotFunc, obj = TRUE)
			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]
			
		})

		if (class(p) == "try-error") {
			errorMessage <- .extractErrorMessage(p)

			plot[["error"]] <- list(error = "badData",
							errorMessage = paste("Plotting is not possible: ", errorMessage))
		}

		if (run) {
			plot[["status"]] <- "complete"
		}
		returnPlot <- plot
	}

	return(returnPlot)
}


.getOutputRow.summarystats.binomial <- function(run, options, state, diff, hyp) {
	# Returns a row to be shown in output tables
	#
	# Args:
	#   run: state of analysis - init or run
	#   options: a list of user options
	#   state: previous options state
	#   diff: diff between previous and current options
	#   hyp: type of hypothesis selected by user
	#
	# Output:
	#   list containing:
	#      row containing output elements to be shown in table
	#      Bayes factor object


	rowsBinomialTest <- NULL
	bayesFactorObject <- NULL

	# if available from previous state, fetch it
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
			(is.list(diff) && (diff$bayesFactorType == FALSE && diff$successes == FALSE &&
			diff$failures == FALSE && diff$testValue == FALSE &&
			diff$betaPriorParamA == FALSE && diff$betaPriorParamB == FALSE &&
			diff$hypothesis == FALSE)))) {

		bayesFactorObject <- state$bayesFactorObject
		rowsBinomialTest <- state$rowsBinomialTest

	} else {
		status <- .isInputValid.summarystats.binomial(options)
		rowsBinomialTest <- status$row

		if (run) {
			if (status$ready) {
				bayesFactorObject <- .bayesBinomialTest(
																counts = options$successes,
																n = (options$successes + options$failures),
																theta0 = options$testValue,
																hypothesis = hyp,
																a = options$betaPriorParamA,
																b = options$betaPriorParamB
															)

				if (options$bayesFactorType == "BF10") {
					BF <- bayesFactorObject
				} else if(options$bayesFactorType == "BF01") {
					BF <- 1 / bayesFactorObject
				} else {
					BF <- log(bayesFactorObject)
				}

				rowsBinomialTest$BF <- .clean(BF)
				rowsBinomialTest$pValue <- .clean(stats::binom.test(x=c(options$successes, options$failures), 
				                                                    p=options$testValue, alternative=hyp)$p.value)
			}
		}
	}

	return(list(row = rowsBinomialTest, bayesFactorObject = bayesFactorObject))
}


.isInputValid.summarystats.binomial <- function(options) {
	# Checks if the input given is valid
	# If input is valid, it returns 'ready' to carry out the analysis
	#
	# Args:
	#   options: a list of options from the user
	#
	# Output:
	#   A list containing:
	#     ready: if ready to carry out the analysis
	#     row: the output row to be shown in table to user

	ready <- TRUE

	testValue <- options$testValue
	successes <- options$successes
	failures  <- options$failures

	if (is.null(options$testValue)) {
		ready <- FALSE
		n1Value <- "."
	}

	if (is.null(options$successes)) {
		ready <- FALSE
		successes <- "."
	}

	if (is.null(options$failures)) {
		ready <- FALSE
		failures <- "."
	}

	row <- list(
						BF = ".",
						successes = successes,
						failures = failures,
						testValue = testValue
					)

	return(list(ready = ready, row = row))
}


.hypothesisType.summarystats.binomial <- function(hypothesis, test.value) {
	# Returns different values that are based on the hypothesis chosen
	#   by the user
	#
	# Args:
	#   hypothesis: the hypothesis selected by user
	#   test.value: the test value input by user
	#
	# Output:
	#   list containing:
	#     message: message shown in footnotes
	#     hyp: whether hyp is two.sided, less or greater
	#

	if (hypothesis == "notEqualToTestValue") {
		hyp <- "two.sided"
		message <- paste0("Proportions tested against value: ", test.value, ".")
	} else if (hypothesis == "greaterThanTestValue") {
		hyp <- "greater"
		note <- "For all tests, the alternative hypothesis specifies that the proportion
					is greater than "
		message <- paste0(note, test.value, ".")
	} else {
		hyp <- "less"
		note <- "For all tests, the alternative hypothesis specifies that the proportion
					is less than "
		message <- paste0(note, test.value, ".")
	}

	return(list(message = message, hyp = hyp))
}
