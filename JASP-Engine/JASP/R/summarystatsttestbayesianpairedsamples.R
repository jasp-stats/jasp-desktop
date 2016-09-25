#
# Copyright (C) 2016 University of Amsterdam
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

SummaryStatsTTestBayesianPairedSamples <- function(dataset = NULL, options, perform = "run", callback) {

	run <- (perform == "run")
	state <- .retrieveState()

	# difference between the previous state options and current options
	diff <- NULL

	if (!is.null(state)) {
		diff <- .diff(options, state$options)
	}

	# Bayes factor type (BF10, BF01, log(BF10)) and title
	bftype <- .getBayesfactorTitle(
								bayesFactorType = options$bayesFactorType,
								hypothesis = options$hypothesis
							)
	bf.title <- bftype$bftitle
	BFH1H0 <- bftype$BFH1H0

	hypothesis.variables <- .hypothesisType(options$hypothesis)
	oneSided <- hypothesis.variables$oneSided

	# initialize variables
	plots.sumstats.ttest <- list()
	plotTypes <- list()
	priorAndPosteriorPlot <- NULL
	bayesFactorRobustnessPlot <- NULL

	outputTableElements <- .getOutputTableRow(
																		run,
																		options,
																		state,
																		diff,
																		hypothesis.variables$nullInterval
																	)
	rowsTTestBayesianPairedSamples <- outputTableElements$row
	bayesFactorObject <- outputTableElements$bayesFactorObject

	# Get prior and posterior plot
	if (options$plotPriorAndPosterior) {
		priorAndPosteriorPlot <- .getPriorAndPosteriorPlot(
																		run = run,
																		options = options,
																		state = state,
																		diff = diff,
																		bayesFactorObject = bayesFactorObject,
																		oneSided = oneSided
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
		bayesFactorRobustnessPlot <- .getBayesFactorRobustnessPlot(
																		run = run,
																		options = options,
																		state = state,
																		diff = diff,
																		bayesFactorObject = bayesFactorObject,
																		oneSided = oneSided
																	)
		plots.sumstats.ttest[[length(plots.sumstats.ttest) + 1]] <- bayesFactorRobustnessPlot
		plotTypes[[length(plotTypes) + 1]] <- "robustnessPlot"
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


.getOutputTableRow <- function(run, options, state, diff, nullInterval) {
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

	rowsTTestBayesianPairedSamples <- NULL
	bayesFactorObject <- NULL
	status <- NULL

	# If available from previous state, fetch it
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
			(is.list(diff) && (diff$bayesFactorType == FALSE && diff$tStatistic == FALSE &&
			diff$n1Size == FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) &&
			!is.null(state$bayesFactorObject)) {

		rowsTTestBayesianPairedSamples <- state$rowsTTestBayesianPairedSamples
		bayesFactorObject <- state$bayesFactorObject

	} else {
		status <- .isInputValidPairedSamples(options)
		rowsTTestBayesianPairedSamples <- status$row

		# if state of analysis is run
		if (run) {
			if (status$ready) {
				bayesFactorObject <- .calcluateBFPairedSamples(
																options,
																state,
																diff,
																nullInterval
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
			}
		}
	}

	return(list(row = rowsTTestBayesianPairedSamples, bayesFactorObject = bayesFactorObject))
}


.getPriorAndPosteriorPlot <- function(run, options, state, diff, bayesFactorObject, oneSided) {
	# Returns the prior and posterior plot. If available from previous,
	#   the function returns that. Else, it calls the plotPosterior function
	#
	# Args:
	#   run: state of analysis - init or run
	#   options: a list of options given by user
	#   bayesFactorObject: Bayes factor object containing bf and properror
	#   oneSided: type of hypothesis
	#
	# Output:
	#   plot - prior and posterior plot

	returnPlot <- NULL
	BFH1H0 <- ifelse((options$bayesFactorType == "BF01"), FALSE, TRUE)
	plotType <- "posteriorPlot"

	if (options$plotPriorAndPosteriorAdditionalInfo) {
		plotType <- "posteriorPlotAddInfo"
	}

	# Check if available from previous state
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
			(is.list(diff) && (diff$bayesFactorType == FALSE && diff$tStatistic == FALSE &&
			diff$n1Size == FALSE && diff$priorWidth == FALSE && diff$hypothesis == FALSE))) &&
			plotType %in% state$plotTypes) {

		index <- which(state$plotTypes == plotType)
		returnPlot <- state$plotsTtest[[index]]

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
			image <- .beginSaveImage(width, height)
			.plotPosterior.ttest.summaryStats(
						t = options$tStatistic, n1 = options$n1Size, n2 = NULL, paired = TRUE,
						BFH1H0 = BFH1H0, dontPlotData = dontPlotData, rscale = options$priorWidth,
						addInformation = options$plotPriorAndPosteriorAdditionalInfo,
						BF = exp(bayesFactorObject$bf), oneSided = oneSided
				)
			plot[["data"]] <- .endSaveImage(image)
		})

		if (class(p) == "try-error") {
			errorMessage <- .extractErrorMessage(p)

			if (errorMessage == "'from' cannot be NA, NaN or infinite") {
				errorMessage <- "The Bayes factor is infinite"
			} else if (!is.null(bayesFactorObject)) {
				if (.clean(exp(bayesFactorObject$bf)) == "\u221E") {
					errorMessage <- "The Bayes factor is infinite"
				} else if (.clean(exp(bayesFactorObject$bf)) == "NaN") {
					errorMessage <- "The Bayes factor could not be calcluated"
				}
			}

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


.getBayesFactorRobustnessPlot <- function(run, options, state, diff, bayesFactorObject, oneSided) {
	# Returns the Bayes factor robustness plot. If available from previous state
	#   the function returns that. Otherwise, based on 'run' state, it calls
	#   the plotBayesFactorRobustness function.
	#
	# Args:
	#   run: state of analysis - init or run
	#   options: user input options
	#   bayesFactorObject: Bayes factor object containing bf and properror
	#   oneSided: type of hypothesis
	#
	# Output:
	#   plot - Bayes factor robustness check

	returnPlot <- NULL
	BFtypeRequiresNewPlot <- TRUE
	BFH1H0 <- ifelse((options$bayesFactorType == "BF01"), FALSE, TRUE)
	bftype.current <- options$bayesFactorType
	bftype.previous <- state$options$bayesFactorType

	# check if BF type requires a new plot
	if (bftype.current == bftype.previous) {
		BFtypeRequiresNewPlot <- FALSE
	}

	if (!(is.null(state)) && BFtypeRequiresNewPlot) {
		equivalent.bf <- list("BF10", "LogBF10")

		if (bftype.current %in% equivalent.bf && bftype.previous %in% equivalent.bf) {
			BFtypeRequiresNewPlot <- FALSE
		}
	}

	# remove temporary variables from workspace
	rm(bftype.current, bftype.previous, equivalent.bf)

	# if available, get the plot from state
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
			(is.list(diff) && (diff$tStatistic == FALSE && BFtypeRequiresNewPlot == FALSE &&
			diff$n1Size == FALSE && diff$priorWidth == FALSE && diff$hypothesis == FALSE))) &&
			"robustnessPlot" %in% state$plotTypes) {

		index <- which(state$plotTypes == "robustnessPlot")
		returnPlot <- state$plotsTtest[[index]]

	} else {

		width  <- 530
		height <- 400

		plot <- list()
		plot[["title"]]  <- "Bayes Factor Robustness Check"
		plot[["width"]]  <- width
		plot[["height"]] <- height
		plot[["status"]] <- "waiting"

		BF10post <- NULL
		if (run) {
			dontPlotData <- FALSE

			if (!is.null(bayesFactorObject)) {
				BF10post <- ifelse(BFH1H0,
													.clean(exp(bayesFactorObject$bf)),
													.clean(1/exp(bayesFactorObject$bf))
										)
			}
		} else {
			dontPlotData <- TRUE
		}

		# plot Bayes factor robustness
		p <- try(silent = FALSE, expr = {
			image <- .beginSaveImage(width, height)
			.plotBF.robustnessCheck.bffromt(
						t = options$tStatistic, n1 = options$n1Size, n2 = 0,
						BFH1H0 = BFH1H0, dontPlotData = dontPlotData,
						rscale = options$priorWidth, oneSided = oneSided,
						BF10post = BF10post
				)
			plot[["data"]] <- .endSaveImage(image)
		})

		if (class(p) == "try-error") {
			errorMessage <- .extractErrorMessage(p)
			plot[["error"]] <- list(error="badData",
							errorMessage = paste("Plotting is not possible: ", errorMessage))
		}

		if (run) {
			plot[["status"]] <- "complete"
		}

		returnPlot <- plot
	}

	return(returnPlot)
}


.calcluateBFPairedSamples <- function(options, state, diff, nullInterval) {
	# calculates the Bayes Factor for the paired samples case
	#
	# Args:
	#   options: list of options input by user
	#   state: previous state - if possible get the BF from previous state
	#   diff: diff between previous and current options
	#   hypothesis.variables: different variables that depend on the hypothesis
	#      specified. See documentation under the function .hypothesisType
	#
	# Output:
	#   A list containing:
	#      bf: the required Bayes Factor
	#      properror: % error in calculating the Bayes factor

	if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
		(is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE &&
		diff$tStatistic == FALSE && diff$n1Size==FALSE)))) {

		bf10 <- state$bayesFactorObject
	} else {

		bf10 <- BayesFactor::ttest.tstat(
								t = options$tStatistic,
								n1 = options$n1Size,
								n2 = 0,
								rscale = options$priorWidth,
								nullInterval = nullInterval
						)
	}

	return(list(bf = bf10$bf, properror = bf10$properror))
}


.getBayesfactorTitle <- function(bayesFactorType, hypothesis) {
	# returns the Bayes factor title to be shown on the table
	#
	# Args:
	#   bayesFactorType: the BF type selected by user
	#   hypothesis: hypothesis type selected by user
	#
	# Output:
	#   A list containing:
	#     bftitle: title of Bayes factor to be used in the output table
	#     BFH1H0: true if BF10 or Log(BF10) is selected

	if (bayesFactorType == "BF01") {
		BFH1H0 <- FALSE

		if (hypothesis == "groupsNotEqual") {
			bf.title <- "BF\u2080\u2081"
		} else if (hypothesis == "groupOneGreater") {
			bf.title <- "BF\u2080\u208A"
		} else if (hypothesis == "groupTwoGreater") {
			bf.title <-  "BF\u2080\u208B"
		}
	} else if (bayesFactorType == "BF10") {
		BFH1H0 <- TRUE

		if (hypothesis == "groupsNotEqual") {
			bf.title <- "BF\u2081\u2080"
		} else if (hypothesis == "groupOneGreater") {
			bf.title <- "BF\u208A\u2080"
		} else if (hypothesis == "groupTwoGreater") {
			bf.title <- "BF\u208B\u2080"
		}
	} else if (bayesFactorType == "LogBF10") {
		BFH1H0 <- TRUE

		if (hypothesis == "groupsNotEqual") {
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
		} else if (hypothesis == "groupOneGreater") {
			bf.title <-"Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
		} else if (hypothesis == "groupTwoGreater") {
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
		}
	}

	return(list(bftitle = bf.title, BFH1H0 = BFH1H0))
}


.hypothesisType <- function(hypothesis) {
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


.isInputValidPairedSamples <- function(options) {
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

	n1Value <- options$n1Size
	tStatValue <- options$tStatistic

	if (options$n1Size == 0 || is.null(options$n1Size)) {
		ready <- FALSE
		n1Value <- "."
	}

	if (is.null(options$tStatistic)) {
		ready <- FALSE
		tStatValue <- "."
	}

	row <- list(BF = ".",
							tStatistic = tStatValue,
							n1Size = n1Value,
							errorEstimate = "."
						)

	return(list(ready = ready, row = row))
}
