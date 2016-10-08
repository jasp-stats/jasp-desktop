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

SummaryStatsCorrelationBayesianPairs <- function(dataset = NULL, options,
														perform = 'run', callback = function(...) 0, ...) {

	run <- (perform == "run")
	state <- .retrieveState()

	# difference between the previous state options and current options
	diff <- NULL

	if (!is.null(state)) {
		diff <- .diff(options, state$options)
	}

	# Bayes factor type (BF10, BF01, log(BF10))
	bf.type <- .getBayesfactorTitle.summarystats.ttest(
								bayesFactorType = options$bayesFactorType,
								hypothesis = options$hypothesis
							)
	bf.title <- bf.type$bftitle
	BFH1H0 <- bf.type$BFH1H0

	hypothesis.variables <- .hypothesisType.summarystats.correlation(options$hypothesis)
	oneSided <- hypothesis.variables$oneSided

	# add footnotes to the analysis result
	footnotes <- .newFootnotes()
	if (options$hypothesis != "correlated") {
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = hypothesis.variables$message)
	}

	# initialize variables
	plots.sumstats.correlation <- list()
	plotTypes <- list()
	priorAndPosteriorPlot <- NULL
	bayesFactorRobustnessPlot <- NULL

	outputTableElements <- .getOutputRow.summarystats.correlation(
														run = run,
														options = options,
														diff = diff,
														state = state
													)
	rowsCorrelationBayesianPairs <- outputTableElements$row
	bayesFactorObject <- outputTableElements$bayesFactorObject

	# get prior and posterior plot
	if (options$plotPriorAndPosterior) {
		priorAndPosteriorPlot <- .getPriorAndPosteriorPlot.summarystats.correlation(
																run = run,
																options = options,
																state = state,
																diff = diff,
																bayesFactorObject = bayesFactorObject,
																oneSided = oneSided
															)
		plots.sumstats.correlation[[length(plots.sumstats.correlation) + 1]] <- priorAndPosteriorPlot
		if(options$plotPriorAndPosteriorAdditionalInfo) {
			plotTypes[[length(plotTypes) + 1]] <- "posteriorPlotAddInfo"
		} else {
			plotTypes[[length(plotTypes) + 1]] <- "posteriorPlot"
		}
	}

	# FIXME: plotBayesFactorRobustnessAdditionalInfo checkbox is not shown in ui - have to modify
	#        .plotPosterior.correlation function to define addInformation.
	# get Bayes factor robustness plot
	if (options$plotBayesFactorRobustness) {
		bayesFactorRobustnessPlot <- .getBayesFactorRobustnessPlot.summarystats.correlation(
																		run = run,
																		options = options,
																		state = state,
																		diff = diff,
																		bayesFactorObject = bayesFactorObject,
																		oneSided = oneSided
																	)
		plots.sumstats.correlation[[length(plots.sumstats.correlation) + 1]] <- bayesFactorRobustnessPlot
		if(options$plotBayesFactorRobustnessAdditionalInfo) {
			plotTypes[[length(plotTypes) + 1]] <- "robustnessPlotAddInfo"
		} else {
			plotTypes[[length(plotTypes) + 1]] <- "robustnessPlot"
		}
	}

	# populate the output table
	meta <- list()
	meta[[1]] <- list(name = "table", type = "table")
	meta[[2]] <- list(name = "inferentialPlots", type = "object",
										meta = list(list(name = "PriorPosteriorPlot", type = "image"),
																list(name = "BFrobustnessPlot", type = "image"))
									)

	fields <- list()
	fields[[length(fields)+1]] <- list(name = "sampleSize", type = "number", title = "n")
	fields[[length(fields)+1]] <- list(name = "pearsonsR", type = "number", format = "sf:4;dp:3",
																			title = "r")
	fields[[length(fields)+1]] <- list(name = "BF", type = "number", format = "sf:4;dp:3",
																			title = bf.title)

	table <- list()
	table[["title"]] <- "Bayesian Pearson Correlation"
	table[["schema"]] <- list(fields = fields)
	table[["citation"]] <- list(paste("Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2014).",
																		"Harold Jeffreys's Default Bayes Factor Hypothesis Tests:",
																		"Explanation, Extension, and Application in Psychology.",
																		"Manuscript submitted for publication.",
																		sep = "")
																	)
	table[["footnotes"]] <- as.list(footnotes)
	table[["data"]] <- list(rowsCorrelationBayesianPairs)

	results <- list()
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Correlation Pairs"
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

	for (plot in plots.sumstats.correlation) {
		keep <- c(keep, plot$data)
	}

	if (run) {
		status <- "complete"
		state <- list(options = options, bayesFactorObject = bayesFactorObject,
								rowsCorrelationBayesianPairs = rowsCorrelationBayesianPairs,
								plotsCorrelationTest = plots.sumstats.correlation, plotTypes = plotTypes)
	} else {
		status <- "inited"
	}

	return(list(results = results,
							status = status,
							state = state,
							keep = keep)
				)
}


.getPriorAndPosteriorPlot.summarystats.correlation <- function(run, options, state, diff,
																												bayesFactorObject, oneSided) {
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
			(is.list(diff) && (diff$bayesFactorType == FALSE && diff$sampleSize == FALSE &&
			diff$pearsonsR == FALSE && diff$priorWidth == FALSE && diff$hypothesis == FALSE )) &&
			plotType %in% state$plotTypes)) {

		index <- which(state$plotTypes == plotType)
		returnPlot <- state$plotsCorrelationTest[[index]]

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
			.plotPosterior.correlation(
						r = options$pearsonsR, n = options$sampleSize, oneSided = oneSided,
						dontPlotData = dontPlotData, kappa = options$priorWidth, BFH1H0 = BFH1H0,
						addInformation = options$plotPriorAndPosteriorAdditionalInfo, BF = bayesFactorObject$bf
					)
			plot[["data"]] <- .endSaveImage(image)
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


.getBayesFactorRobustnessPlot.summarystats.correlation <- function(run, options, state, diff,
																												bayesFactorObject, oneSided) {
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

	plotType <- "robustnessPlot"

	if (options$plotBayesFactorRobustnessAdditionalInfo) {
		plotType <- "robustnessPlotAddInfo"
	}

	# check if BF type requires a new plot
	if (!is.null(bftype.previous) && bftype.current == bftype.previous) {
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
			(is.list(diff) && (diff$sampleSize == FALSE && BFtypeRequiresNewPlot == FALSE &&
			diff$pearsonsR == FALSE && diff$priorWidth == FALSE && diff$hypothesis == FALSE))) &&
			plotType %in% state$plotTypes) {

		index <- which(state$plotTypes == plotType)
		returnPlot <- state$plotsCorrelationTest[[index]]

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
													bayesFactorObject$bf,
													1 / bayesFactorObject$bf
												)
			}
		} else {
			dontPlotData <- TRUE
		}

		# plot Bayes factor robustness
		p <- try(silent = FALSE, expr = {
			image <- .beginSaveImage(width, height)
			.plotBF.robustnessCheck.correlation(
						r = options$pearsonsR, n = options$sampleSize, oneSided = oneSided, BFH1H0 = BFH1H0,
						kappa = options$priorWidth, dontPlotData = dontPlotData, BF10post = BF10post
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


.isInputValid.summarystats.correlation <- function(options) {
	# Checks if the input values provided are valid and ready to calculate
	#     Bayes factor
	#
	# Input:
	#     options: the list of options provided by user
	#
	# Output:
	#     list containing two attributes -
	#         ready: if we can carry out the analysis
	#         row: the values shown in the output table

	ready <- TRUE

	sampleSizeValue <- options$sampleSize
	pearsonsRValue <- options$pearsonsR

	if (is.null(options$sampleSize) || options$sampleSize == 0) {
		ready <- FALSE
		sampleSizeValue <- "."
	}

	if (is.null(options$pearsonsR)) {
		ready <- FALSE
		pearsonsRValue <- "."
	}

	row <- list(BF = ".", sampleSize = sampleSizeValue, pearsonsR = pearsonsRValue)

	return(list(ready = ready, row = row))
}


.calculateBF.summarystats.correlation <- function(options, state, diff) {
	# Calculate the Bayes factors for correlation pairs
	#
	# Input:
	#     options: user options
	#     state: previous state variables
	#     diff: difference between previous and current options
	#
	# Ouput:
	#     list containing -
	#         bf: the required Bayes factor value
	#         tooPeaked: whether it is too peaked to plot

	some.n <- options$sampleSize
	some.r <- options$pearsonsR

	all.bfs <- list(bf10 = NA, bfPlus0 = NA, bfMin0 = NA)
	method.number <- 1

	while (any(is.na(c(all.bfs$bf10, all.bfs$bfPlus0, all.bfs$bfMin0))) && method.number <= 4) {
		# Note: Try all normal methods
		all.bfs <- .bfCorrieKernel(n = some.n, r = some.r, kappa = options$priorWidth,
																method = method.number)
		method.number <- method.number + 1
	}

	if (any(is.na(all.bfs))) {
		# Note: all normal methods FAILED. Use Jeffreys approximation
		all.bfs <- .bfCorrieKernel(n = some.n, r = some.r, kappa = options$priorWidth,
																method = "jeffreysApprox")
	}

	some.bf10 <- all.bfs$bf10
	some.bfPlus0 <- all.bfs$bfPlus0
	some.bfMin0 <- all.bfs$bfMin0

	switch(options$hypothesis,
		correlated = {
			some.bf <- some.bf10
			tooPeaked <- all.bfs$twoSidedTooPeaked
		},
		correlatedPositively = {
			some.bf <- some.bfPlus0
			tooPeaked <- all.bfs$plusSidedTooPeaked
		},
		correlatedNegatively = {
			some.bf <- some.bfMin0
			tooPeaked <- all.bfs$minSidedTooPeaked
		}
	)

	return(list(bf = some.bf, peaked = tooPeaked))
}


.getOutputRow.summarystats.correlation <- function(run, options, diff, state) {
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

	rowsCorrelationBayesianPairs <- NULL
	bayesFactorObject <- NULL
	status <- NULL

	# If available from previous state, fetch it
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
			(is.list(diff) && (diff$bayesFactorType == FALSE && diff$sampleSize == FALSE &&
			diff$pearsonsR == FALSE && diff$priorWidth == FALSE && diff$hypothesis == FALSE))) &&
			!is.null(state$bayesFactorObject)) {

		rowsCorrelationBayesianPairs <- state$rowsCorrelationBayesianPairs
		bayesFactorObject <- state$bayesFactorObject

	} else {

		status <- .isInputValid.summarystats.correlation(options = options)
		rowsCorrelationBayesianPairs <- status$row

		if (run) {
			if (status$ready) {
				bayesFactorObject <- .calculateBF.summarystats.correlation(
																options = options,
																state = state,
																diff = diff
															)
				BF <- switch(
								options$bayesFactorType,
								BF10 = .clean(bayesFactorObject$bf),
								BF01 = .clean(1 / bayesFactorObject$bf),
								LogBF10 = .clean(log(bayesFactorObject$bf))
							)
				rowsCorrelationBayesianPairs$BF <- BF
			}
		}
	}

	return(list(row = rowsCorrelationBayesianPairs, bayesFactorObject = bayesFactorObject))
}


.hypothesisType.summarystats.correlation <- function(hypothesis) {
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

	if (hypothesis == "correlatedPositively") {

		nullInterval <- c(0, Inf)
		oneSided <- "right"
		message <- paste("For all tests,",
											"the alternative hypothesis specifies that the correlation is positive.",
											sep = "")

	} else if (hypothesis == "correlatedNegatively") {

		nullInterval <- c(-Inf, 0)
		oneSided <- "left"
		message <- paste("For all tests,",
											"the alternative hypothesis specifies that the correlation is negative.",
											sep = "")
	}

	return(list(nullInterval = nullInterval,
							oneSided = oneSided,
							message = message)
				)
}
