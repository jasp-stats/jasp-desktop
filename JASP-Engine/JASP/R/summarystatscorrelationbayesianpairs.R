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
	bfObject <- outputTableElements$bfObject

	# get prior and posterior plot
	if (options$plotPriorAndPosterior) {
		priorAndPosteriorPlot <- .getPriorAndPosteriorPlot.summarystats.correlation(
														run = run,
														options = options,
														state = state,
														diff = diff,
														bfObject = bfObject,
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
														bfObject = bfObject,
														oneSided = oneSided
													)
		plots.sumstats.correlation[[length(plots.sumstats.correlation) + 1]] <- bayesFactorRobustnessPlot
		if(options$plotBayesFactorRobustnessAdditionalInfo) {
			plotTypes[[length(plotTypes) + 1]] <- "robustnessPlotAddInfo"
		} else {
			plotTypes[[length(plotTypes) + 1]] <- "robustnessPlot"
		}
	}

	if (options$correlationCoefficient == "pearsonRho") {
		correlation.title <- "r"
	} else if (options$correlationCoefficient == "kendallTau") {
		correlation.title <- "tau"
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
																	title = correlation.title)
	fields[[length(fields)+1]] <- list(name = "BF", type = "number", format = "sf:4;dp:3",
																			title = bf.title)
	fields[[length(fields)+1]] <- list(name = "pValue", type = "number", format = "sf:4;dp:3", title = "p")

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
		state <- list(options = options, bfObject = bfObject,
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
																bfObject, oneSided) {
	# Returns the prior and posterior plot. If available from previous,
	#   the function returns that. Else, it calls the plotPosterior function
	#
	# Args:
	#   run: state of analysis - init or run
	#   options: a list of options given by user
	#   bfObject: Bayes factor object containing bf and properror
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
		diff$correlationCoefficient == FALSE && diff$pearsonRhoValue == FALSE &&
		diff$kendallTauValue == FALSE && diff$priorWidth == FALSE && diff$hypothesis == FALSE )) &&
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

		if (options$correlationCoefficient == "pearsonRho") {
			cor.value <- options$pearsonRhoValue
			cor.coefficient <- "Pearson"
		} else if (options$correlationCoefficient == "kendallTau") {
			cor.value <- options$kendallTauValue
			cor.coefficient <- "Kendall"
		}

		p <- try(silent = FALSE, expr = {

			# image <- .beginSaveImage(width, height)
			
			# TODO: fix .plotPosterior.correlation take in bfObject and have switch bfObject$bf 

			someBf <- switch(options$hypothesis,
			                 correlated = bfObject$bf10,
			                 correlatedPositively = bfObject$bfPlus0,
			                 correlatedNegatively =bfObject$bfMin0
			)

			
			.plotFunc <- function() {
				if (!BFH1H0) {
					someBf <- 1 / someBf
				}

				.plotPosterior.correlation(
							r = cor.value, n = options$sampleSize, oneSided = oneSided,
							corCoefficient = cor.coefficient, dontPlotData = dontPlotData,
							kappa = options$priorWidth, BFH1H0 = BFH1H0, BF = someBf,
							addInformation = options$plotPriorAndPosteriorAdditionalInfo
						)
			}
			content <- .writeImage(width = width, height = height, plot = .plotFunc, obj = TRUE)
			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]
			# plot[["data"]] <- .endSaveImage(image)
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
																bfObject, oneSided) {
	# Returns the Bayes factor robustness plot. If available from previous state
	#   the function returns that. Otherwise, based on 'run' state, it calls
	#   the plotBayesFactorRobustness function.
	#
	# Args:
	#   run: state of analysis - init or run
	#   options: user input options
	#   bfObject: Bayes factor object containing bf and properror
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
		diff$correlationCoefficient == FALSE && diff$pearsonRhoValue == FALSE &&
		diff$kendallTauValue == FALSE && diff$priorWidth == FALSE && diff$hypothesis == FALSE))) &&
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

			if (!is.null(bfObject)) {

			  # TODO: fix .plotPosterior.correlation take in bfObject and have switch bfObject$bf

			  someBf <- switch(options$hypothesis,
			                   correlated = bfObject$bf10,
			                   correlatedPositively = bfObject$bfPlus0,
			                   correlatedNegatively =bfObject$bfMin0
			  )

				BF10post <- ifelse(BFH1H0, someBf, 1/someBf)
			}
		} else {
			dontPlotData <- TRUE
		}

		if (options$correlationCoefficient == "pearsonRho") {
			cor.value <- options$pearsonRhoValue
			cor.coefficient <- "Pearson"
		} else if (options$correlationCoefficient == "kendallTau") {
			cor.value <- options$kendallTauValue
			cor.coefficient <- "Kendall"
		}

		# plot Bayes factor robustness
		p <- try(silent = FALSE, expr = {
			# image <- .beginSaveImage(width, height)
			# .plotBF.robustnessCheck.summarystats.correlation(
			# 	r = cor.value, n = options$sampleSize, oneSided = oneSided, BFH1H0 = BFH1H0,
			# 	corCoefficient = cor.coefficient, kappa = options$priorWidth,
			# 	dontPlotData = dontPlotData, BF10post = BF10post,
			# 	addInformation = options$plotBayesFactorRobustnessAdditionalInfo
			# )
			# plot[["data"]] <- .endSaveImage(image)

			.plotFunc <- function() {
				.plotBF.robustnessCheck.summarystats.correlation(
					r = cor.value, n = options$sampleSize, oneSided = oneSided, BFH1H0 = BFH1H0,
					corCoefficient = cor.coefficient, kappa = options$priorWidth,
					dontPlotData = dontPlotData, BF10post = BF10post,
					addInformation = options$plotBayesFactorRobustnessAdditionalInfo
				)
			}
			content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]

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

	if (is.null(options$sampleSize) || options$sampleSize == 0) {
		ready <- FALSE
		sampleSizeValue <- "."
	}

	if (options$correlationCoefficient == "pearsonRho") {
		cor.value <- options$pearsonRhoValue

		if (is.null(options$pearsonRhoValue)) {
			ready <- FALSE
			cor.value <- "."
		}
	} else if (options$correlationCoefficient == "kendallTau") {
		cor.value <- options$kendallTauValue

		if (is.null(options$kendallTauValue)) {
			ready <- FALSE
			cor.value <- "."
		}
	}

	row <- list(BF = ".", sampleSize = sampleSizeValue, pearsonsR = cor.value)

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
	#         bf: three Bayes factors
	#         pValues: three p-values

	some.n <- options$sampleSize
	bfObject <- list(bf10 = NA, bfPlus0 = NA, bfMin0 = NA)

	if (options$correlationCoefficient == "pearsonRho") {
	    some.r <- options$pearsonRhoValue
	    bfObject <- .bfPearsonCorrelation(n=some.n, r=some.r, kappa=options$priorWidth)
		allPValues <- .pValueFromCor(corrie=some.r, n=some.n, method="pearson")
	} else if (options$correlationCoefficient == "kendallTau") {
		some.r <- options$kendallTauValue
		bfObject <- .bfKendallTau(n=some.n, tauObs=some.r, kappa=options$priorWidth)
		allPValues <- .pValueFromCor(corrie=some.r, n=some.n, method="kendall")
	} else if (options$correlationCoefficient == "spearman"){
	  # TODO: Johnny
	  # Without code this will print a NULL, if we go through here
	}
	
	bfObject$pValue <- allPValues
	return(bfObject)
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
	bfObject <- NULL
	status <- NULL

	# If available from previous state, fetch it
	if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
			(is.list(diff) && (diff$bayesFactorType == FALSE && diff$sampleSize == FALSE &&
			diff$correlationCoefficient == FALSE && diff$priorWidth == FALSE &&
			diff$hypothesis == FALSE && diff$pearsonRhoValue == FALSE &&
			diff$kendallTauValue == FALSE))) && !is.null(state$bfObject)) {

		rowsCorrelationBayesianPairs <- state$rowsCorrelationBayesianPairs
		bfObject <- state$bfObject

	} else {

		status <- .isInputValid.summarystats.correlation(options = options)
		rowsCorrelationBayesianPairs <- status$row

		if (run) {
			if (status$ready) {
			  # Calculate BF object
				bfObject <- .calculateBF.summarystats.correlation(
																options = options,
																state = state,
																diff = diff
															)
				# Note: bfObject contains infor about whether the posteriors are too peaked

				# Note: Choose the right side of the Bfs and pValue
				switch(options$hypothesis,
				       correlated = {
				         someBf <- bfObject$bf10
				         rowsCorrelationBayesianPairs$pValue <- .clean(bfObject$pValue$twoSided)
				       },
				       correlatedPositively = {
				         someBf <- bfObject$bfPlus0
				         rowsCorrelationBayesianPairs$pValue <- .clean(bfObject$pValue$plusSided)
				       },
				       correlatedNegatively = {
				         someBf <- bfObject$bfMin0
				         rowsCorrelationBayesianPairs$pValue <- .clean(bfObject$pValue$minSided)
				       }
				)

				# Note: Choose the display mode for Bf
				rowsCorrelationBayesianPairs$BF <- switch(options$bayesFactorType,
				                                          BF10 = .clean(someBf),
				                                          BF01 = .clean(1 / someBf),
				                                          LogBF10 = .clean(log(someBf))
				)
			}
		}
	}

	return(list(row = rowsCorrelationBayesianPairs, bfObject=bfObject))
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


.plotBF.robustnessCheck.summarystats.correlation <- function(r = NULL, n = NULL, paired = FALSE,
			BF10post = NULL, kappa = 1, callback = function(...) 0, oneSided = FALSE, lwd = 2,
			cexPoints = 1.4, cexAxis = 1.2, cexYXlab = 1.5,  cexText = 1.2, cexLegend = 1.4,
			lwdAxis = 1.2, cexEvidence = 1.6, BFH1H0 = TRUE, dontPlotData = FALSE,
			corCoefficient = "Pearson", addInformation = TRUE) {

	useKendall <- corCoefficient == "Kendall"
	usePearson <- corCoefficient == "Pearson"

	if (addInformation) {
		par(mar = c(5, 6, 6, 7) + 0.1, las = 1)
	} else {
		par(mar = c(5.6, 5, 4, 7) + 0.1, las = 1)
	}

	if (dontPlotData) {
		plot(1, type = 'n', xlim = 0:1, ylim = 0:1, bty = 'n', axes = FALSE, xlab = "", ylab = "")

		axis(1, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, xlab = "")
		axis(2, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, ylab = "")

		if (oneSided == FALSE) {
			if (BFH1H0) {
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line = 3.1)
			} else {
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line = 3.1)
			}
		} else if (oneSided == "right") {
			if (BFH1H0) {
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line = 3.1)
			} else {
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line = 3.1)
			}
		} else if (oneSided == "left") {
			if (BFH1H0) {
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line = 3.1)
			} else {
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line = 3.1)
			}
		}
		mtext("Stretched beta prior width", side = 1, cex = cexYXlab, line = 2.5)

		return()
	}

	# get BFs
	kappaValues <- .makeKappas(50)
	kappaValues <- c(0, kappaValues)

	BF10 <- vector("numeric", length(kappaValues))
	BF10[1] <- 1  # set first one manually to one

	for (i in seq_along(kappaValues)[-1]) {
		bfObject <- list(bf10=NA, bfPlus0=NA, bfMin0=NA)
		
		if (usePearson) {
		    bfObject <- .bfPearsonCorrelation(n=n, r=r, kappa=kappaValues[i], ciValue=NULL)
		} else if (useKendall) {
		    bfObject <- .bfKendallTau(n=n, tauObs=r, kappa=kappaValues[i], ciValue=NULL)
		}
		
		if (oneSided == FALSE) {
			if (bfObject$bf10 == 0) {
				bfObject$bf10 <- 1
			}
			BF10[i] <- bfObject$bf10
		} else if (oneSided == "right") {
			if (is.na(bfObject$bfPlus0)) {
				bfObject$bfPlus0 <- 1
			}
			BF10[i] <- bfObject$bfPlus0
		} else if (oneSided == "left") {
			if (is.na(bfObject$bfMin0)) {
				bfObject$bfMin0 <- 1
			}
			BF10[i] <- bfObject$bfMin0
		}

		if (is.na(BF10[i])) {
			stop("One or more Bayes factors cannot be computed")
		} else if (is.infinite(BF10[i])) {
			stop("One or more Bayes factors are infinity")
		}
	}

	# BF10 user prior
	BF10user <- BF10post
	BF10userText <- BF10user

	# maximum BF value
	maxBF10 <- max(BF10)
	maxBFkappaVal <- kappaValues[which.max(BF10)]
	print(maxBFkappaVal)
	print(maxBF10)
	BF10maxText <- .clean(maxBF10)

	if (! .shouldContinue(callback())) {
		return()
	}

	####################### scale y axis #######################
	BF <- c(BF10, BF10user, maxBF10)

	if (!BFH1H0) {
		BF <- 1 / BF
		BF10 <- 1 / BF10
		maxBF10 <- 1 / maxBF10
	}

	# y-axis labels larger than 1
	y1h <- "1"
	i <- 1

	while (eval(parse(text= y1h[i])) < max(BF10)) {
		if (grepl(pattern = "e",y1h[i])) {
			newy <- paste(strsplit(y1h[i], split = "+", fixed = TRUE)[[1]][1], "+",
						as.numeric(strsplit(y1h[i],split = "+", fixed = TRUE)[[1]][2]) + 1,
						sep = "")
		} else {
			newy <- paste(y1h[i], "0", sep = "")
		}

		if (eval(parse(text=newy)) >= 10^6) {
			newy <- format(as.numeric(newy), digits = 3, scientific = TRUE)
		}

		y1h <- c(y1h, newy)
		i <- i + 1
	}

	y3h <- "3"
	i <- 1

	while (eval(parse(text= y3h[i])) < max(BF10)) {
		if (grepl(pattern = "e",y3h[i])) {
			newy <- paste(strsplit(y3h[i], split = "+", fixed = TRUE)[[1]][1], "+",
						as.numeric(strsplit(y3h[i], split = "+", fixed = TRUE)[[1]][2]) + 1,
						sep = "")
		} else {
			newy <- paste(y3h[i], "0", sep = "")
		}

		if (as.numeric(newy) >= 10^6) {
			newy <- format(as.numeric(newy), digits = 3, scientific = TRUE)
		}

		y3h <- c(y3h, newy)
		i <- i + 1
	}
	
	yhigh <- vector("numeric", length(y1h) + length(y3h))
	o <- 1
	e <- 1

	for (i in seq_along(yhigh)) {
		if (i %% 2 == 1) {
			yhigh[i] <- y1h[o]
			o <- o + 1
		} else if (i %% 2 == 0) {
			yhigh[i] <- y3h[e]
			e <- e + 1
		}
	}

	yhighLab <- as.character(yhigh)
	# y-axis labels smaller than 1
	y1l <- "1/1"
	i <- 1

	while (eval(parse(text = y1l[i])) > min(BF10)) {
		if (grepl(pattern = "e",y1l[i])) {
			newy <- paste(strsplit(y1l[i], split = "+", fixed = TRUE)[[1]][1], "+",
						as.numeric(strsplit(y1l[i],split = "+", fixed = TRUE)[[1]][2]) + 1,
						sep = "")
		} else {
			newy <- paste(y1l[i], "0", sep = "")
		}

		if (eval(parse(text= newy)) <= 10^(-6)) {
			newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}

		y1l <- c(y1l, newy)
		i <- i + 1
	}

	y3l <- "1/3"
	i <- 1

	while (eval(parse(text = y3l[i])) > min(BF10)) {
		if (grepl(pattern = "e", y3l[i])) {
			newy <- paste(strsplit(y3l[i], split = "+", fixed = TRUE)[[1]][1], "+",
						as.numeric(strsplit(y3l[i], split = "+", fixed = TRUE)[[1]][2]) + 1,
						sep = "")
		} else {
			newy <- paste(y3l[i], "0", sep = "")
		}

		if (newy == "1/3e+9") {
			newy <- "1/3e+09"
		}

		if (eval(parse(text= newy)) <= 10^(-6) & eval(parse(text = newy)) > 10^(-9)) {

			newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
			newy <- paste(substring(newy, 1, nchar(newy) - 1), as.numeric(substring(newy, nchar(newy), nchar(newy)))-1, sep = "")
			newy <- sub(".33", "", newy)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}

		y3l <- c(y3l, newy)
		i <- i + 1
	}

	ylow <- vector("numeric", length(y1l) + length(y3l))
	o <- 1
	e <- 1

	if (! .shouldContinue(callback())) {
		return()
	}

	for (i in seq_along(ylow)) {
		if (i %% 2 == 1) {
			ylow[i] <- y1l[o]
			o <- o + 1
		} else if (i %% 2 == 0) {
			ylow[i] <- y3l[e]
			e <- e + 1
		}
	}

	yLab <- c(rev(ylow[-1]), yhighLab)
	# remove 3's if yLab vector is too long
	omit3s <- FALSE

	if (length(yLab) > 9) {
		omit3s <- TRUE
		ind <- which(yLab == "3")
		yLabsHigh <- yLab[ind:length(yLab)]

		if (length(yLabsHigh) > 1) {
			yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh), 2)]
		} else {
			yLabsHigh <- character(0)
		}

		yLabsLow <- yLab[1:(ind - 1)]
		yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]

		yLab1s <- c(yLabsLow, yLabsHigh)

		if (max(BF10) > eval(parse(text= yLab1s[length(yLab1s)]))) {
			for (i in 1:2) {
				if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {
					newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed = TRUE)[[1]][1], "+",
								as.numeric(strsplit(yLab1s[length(yLab1s)],
								split = "+", fixed=TRUE)[[1]][2]) + 1, sep = "")
				} else {
					newy <- paste(yLab1s[length(yLab1s)], "0", sep = "")
				}

				if (eval(parse(text=newy)) >= 10^6) {
					newy <- format(eval(parse(text=newy)), digits = 3, scientific = TRUE)
				}

				yLab1s <- c(yLab1s, newy)
			}
		}

		if (max(BF10) > eval(parse(text = yLab1s[length(yLab1s)-1]))) {

			if (grepl(pattern = "e", yLab1s[length(yLab1s)])) {
				newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed = TRUE)[[1]][1],
							"+", as.numeric(strsplit(yLab1s[length(yLab1s)], split = "+", fixed = TRUE)[[1]][2]) + 1,
							sep = "")
			} else {
				newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
			}

			if (eval(parse(text = newy)) >= 10^6) {
				newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
			}

			yLab1s <- c(yLab1s, newy)
		}

		if (yLab1s[1] == "1") {
			yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
		}
		if (yLab1s[length(yLab1s)] == "1") {
			yLab1s <- c(yLab1s, "10")
		}

		if (min(BF10) < eval(parse(text= yLab1s[1]))) {
			for (i in 1:2) {
				if (grepl(pattern = "e",yLab1s[1])) {
					newy <- paste(strsplit(yLab1s[1], split = "+", fixed = TRUE)[[1]][1], "+",
								as.numeric(strsplit(yLab1s[1],split = "+", fixed = TRUE)[[1]][2]) + 1,
								sep = "")
				} else {
					newy <- paste(yLab1s[1], "0", sep = "")
				}

				if (eval(parse(text= newy)) <= 10^(-6)) {

					newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
					newy <-  sub("-", "+", x = newy)
					newy <- substring(newy, nchar(newy) - 4, nchar(newy))
					newy <- paste0("1/", newy)
				}
			}
			yLab1s <- c(newy, yLab1s)
		}

		if (min(BF10) < eval(parse(text= yLab1s[2]))) {
			if (grepl(pattern = "e",yLab1s[1])) {
				newy <- paste(strsplit(yLab1s[1], split = "+", fixed = TRUE)[[1]][1], "+",
							as.numeric(strsplit(yLab1s[1],split = "+", fixed = TRUE)[[1]][2])+1,
							sep = "")
			} else {
				newy <- paste(yLab1s[1], "0", sep = "")
			}

			if (eval(parse(text= newy)) <= 10^(-6)) {

				newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
				newy <-  sub("-", "+", x = newy)
				newy <- substring(newy, nchar(newy) - 4, nchar(newy))
				newy <- paste0("1/", newy)
			}

			yLab1s <- c(newy, yLab1s)
		}

		yLab <- yLab1s
	}

	if (!.shouldContinue(callback())) {
		return()
	}

	while (length(yLab) > 9) {
		ind <- which(yLab == "1")

		if (ind == 1) {
			yLabLow <- character(0)
		} else {
			yLabLow <- yLab[1:(ind-1)]
		}

		if (ind == length(yLab)) {
			yLabHigh <- character(0)
		} else {
			yLabHigh <- yLab[(ind+1):length(yLab)]
		}

		if (length(yLabLow) > 1) {
			yLabLow <- yLabLow[seq(length(yLabLow)-1, 1, -2)]
		} else {
			yLabLow <- yLabLow
		}

		if (length(yLabHigh) > 1) {
			yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
		} else {
			yLabHigh <- yLabHigh
		}

		if (length(yLabLow) == 1) {
			yLabLow <- paste("1/", yLabHigh[1], sep="")
		}

		if (length(yLabHigh) == 1) {
			yLabHigh <- strsplit(x = yLabLow[1], "/", fixed=TRUE)[[1]][2]
		}

		yLab <- c(rev(yLabLow), "1", yLabHigh)
	}

	if (!.shouldContinue(callback())) {
		return()
	}

	while (eval(parse(text=yLab[2])) > min(BF10)) {
		interval <- as.numeric(strsplit(format(eval(parse(text = yLab[1])), digits = 3, scientific = TRUE), "-", fixed = TRUE)[[1]][2]) -
					as.numeric(strsplit(format(eval(parse(text = yLab[2])), digits = 3, scientific = TRUE), "-", fixed = TRUE)[[1]][2])
		pot <- as.numeric(strsplit(format(eval(parse(text = yLab[1])), digits = 3, scientific = TRUE), "-", fixed = TRUE)[[1]][2]) + interval

		if (nchar(pot) == 1) {
			pot <- paste("0", pot, sep="")
		}

		newy <- paste("1/1e", "+", pot, sep="")
		yLab <- c(newy, yLab)
	}

	while (eval(parse(text=yLab[length(yLab)-1])) < max(BF10)) {

		interval <- as.numeric(strsplit(format(eval(parse(text = yLab[length(yLab)])), digits = 3, scientific = TRUE), "+", fixed = TRUE)[[1]][2]) -
					as.numeric(strsplit(format(eval(parse(text = yLab[length(yLab) - 1])), digits = 3, scientific = TRUE), "+", fixed = TRUE)[[1]][2])
		pot <- as.numeric(strsplit(format(eval(parse(text = yLab[length(yLab)])), digits = 3, scientific = TRUE), "+", fixed = TRUE)[[1]][2]) + interval

		if (nchar(pot) == 1) {
			pot <- paste("0", pot, sep = "")
		}

		newy <- paste(strsplit(format(eval(parse(text = yLab[length(yLab)])), digits = 3, scientific = TRUE), "+", fixed = TRUE)[[1]][1], "+", pot, sep = "")
		yLab <- c( yLab, newy)
	}

	yAt <- vector("numeric", length(yLab))

	for (i in seq_along(yLab)) {
		yAt[i] <- log(eval(parse(text = yLab[i])))
	}

	####################### plot #######################
	xLab <- pretty(range(kappaValues))
	xlim <- range(xLab)
	ylow <- log(eval(parse(text = yLab[1])))
	yhigh <- log(eval(parse(text = yLab[length(yLab)])))
	ylim <- c(ylow, yhigh)

	plot(1, 1, xlim = xlim, ylim = ylim, ylab = "", xlab = "", type = "n", axes = FALSE)

	for (i in seq_along(yAt)) {
		lines(x = xlim, y = rep(yAt[i], 2), col = 'darkgrey', lwd = 1.3, lty = 2)
	}

	lines(xlim, rep(0, 2), lwd = lwd)

	axis(1, at = xLab, labels = xLab, cex.axis = cexAxis, lwd = lwdAxis)
	axis(2, at = yAt, labels = yLab, cex.axis = cexAxis, lwd = lwdAxis)

	# enable plotting in margin
	par(xpd = TRUE)
	xx <- grconvertX(0.79, "ndc", "user")

	yAthigh <- yAt[yAt >= 0]

	if (!omit3s & eval(parse(text = yLab[1])) >= 1/300 & eval(parse(text = yLab[length(yLab)])) <= 300) {
		for (i in 1:(length(yAthigh) - 1)) {
			yy <- mean(c(yAthigh[i], yAthigh[i+1]))

			if (yAthigh[i] == log(1)) {
				text(x = xx, yy, "Anecdotal", pos = 4, cex = cexText)
			} else if (yAthigh[i] == log(3)) {
				text(x = xx, yy, "Moderate", pos = 4, cex = cexText)
			} else if (yAthigh[i] == log(10)) {
				text(x = xx, yy, "Strong", pos = 4, cex = cexText)
			} else if (yAthigh[i] == log(30)) {
				text(x = xx, yy, "Very strong", pos = 4, cex = cexText)
			} else if (yAthigh[i] == log(100)) {
				text(x = xx, yy, "Extreme", pos = 4, cex = cexText)
			}
		}

		yAtlow <- rev(yAt[yAt <= 0])

		for (i in 1:(length(yAtlow) - 1)) {
			yy <- mean(c(yAtlow[i], yAtlow[i+1]))

			if (yAtlow[i] == log(1)) {
				text(x = xx, yy, "Anecdotal", pos = 4, cex = cexText)
			} else if (yAtlow[i] == log(1/3)) {
				text(x = xx, yy, "Moderate", pos = 4, cex = cexText)
			} else if (yAtlow[i] == log(1/10)) {
				text(x = xx, yy, "Strong", pos = 4, cex = cexText)
			} else if (yAtlow[i] == log(1/30)) {
				text(x = xx, yy, "Very strong", pos = 4, cex = cexText)
			} else if (yAtlow[i] == log(1/100)) {
				text(x = xx, yy, "Extreme", pos = 4, cex = cexText)
			}
		}

		axis(side = 4, at = yAt, tick = TRUE, las = 2, cex.axis = cexAxis, lwd = lwdAxis,
			labels = FALSE, line = -0.6)

		xx <- grconvertX(0.96, "ndc", "user")
		yy <- grconvertY(0.5, "npc", "user")

		text(xx, yy, "Evidence", srt = -90, cex = cexEvidence)
	}

	if (omit3s) {
		if (eval(parse(text = yLab[1])) <= 1/10^6) {
			line <- 4.75
		} else {
			line <- 4.3
		}

		if (oneSided == FALSE) {
			if (BFH1H0) {
				mtext(text = expression(BF[1][0]), side = 2, las = 0, cex = cexYXlab, line = line)
			} else {
				mtext(text = expression(BF[0][1]), side = 2, las = 0, cex = cexYXlab, line = line)
			}
		} else if (oneSided == "right") {
			if (BFH1H0) {
				mtext(text = expression(BF["+"][0]), side = 2, las = 0, cex = cexYXlab, line = line)
			} else {
				mtext(text = expression(BF[0]["+"]), side = 2, las = 0, cex = cexYXlab, line = line)
			}
		} else if (oneSided == "left") {
			if (BFH1H0) {
				mtext(text = expression(BF["-"][0]), side = 2, las = 0, cex = cexYXlab, line = line)
			} else {
				mtext(text = expression(BF[0]["-"]), side = 2, las = 0, cex = cexYXlab, line = line)
			}
		}
	}

	if (omit3s == FALSE) {
		if (oneSided == FALSE) {
			if (BFH1H0) {
				mtext(text = expression(BF[1][0]), side = 2, las = 0, cex = cexYXlab, line = 3.1)
			} else {
				mtext(text = expression(BF[0][1]), side = 2, las = 0, cex = cexYXlab, line = 3.1)
			}
		} else if (oneSided == "right") {
			if (BFH1H0) {
				mtext(text = expression(BF["+"][0]), side = 2, las = 0, cex = cexYXlab, line = 3.1)
			} else {
				mtext(text = expression(BF[0]["+"]), side = 2, las = 0, cex = cexYXlab, line = 3.1)
			}
		} else if (oneSided == "left") {
			if (BFH1H0) {
				mtext(text = expression(BF["-"][0]), side = 2, las = 0, cex = cexYXlab, line = 3.1)
			} else {
				mtext(text = expression(BF[0]["-"]), side = 2, las = 0, cex = cexYXlab, line = 3.1)
			}
		}
	}

	mtext("Stretched beta prior width", side = 1, cex = cexYXlab, line = 2.5)

	xx <- grconvertX(0.1, "npc", "user")
	yy1 <- yAt[length(yAt) - 1]
	yy2 <- yAt[length(yAt)]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4* diff(c(yy1, yy2))

	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd = lwd)

	xxt <- grconvertX(0.28, "npc", "user")

	if (oneSided == FALSE) {
		if (BFH1H0) {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex = cexText)
		} else {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		}
	} else if (oneSided == "right") {
		if (BFH1H0) {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex = cexText)
		} else {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		}
	} else if (oneSided == "left") {
		if (BFH1H0) {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex = cexText)
		} else {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		}
	}

	yy1 <- yAt[2]
	yy2 <- yAt[1]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))

	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd = lwd)

	if (oneSided == FALSE) {
		if (BFH1H0) {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		} else {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex = cexText)
		}
	} else if (oneSided == "right") {
		if (BFH1H0) {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		} else {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex = cexText)
		}
	} else if (oneSided == "left") {
		if (BFH1H0) {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		} else {
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex = cexText)
		}
	}

	if (!.shouldContinue(callback())) {
		return()
	}

	# display BF10
	lines(kappaValues, log(BF10), col = "black", lwd = 2.7)

	if (addInformation) {
		# display user prior BF
		points(kappa, log(BF10user), pch = 21, bg = "grey", cex = cexPoints, lwd = 1.3)
		points(maxBFkappaVal, log(maxBF10), pch = 21, bg = "red", cex = 1.3, lwd = 1.3)

		####################### add legend #######################
		# user Bayes factor
		if (BFH1H0) {
			BF01userText <- 1 / BF10userText
		} else {
			BF10userText <- 1 / BF10userText
			BF01userText <- 1 / BF10userText
		}

		if (BF10userText >= 1000000 | BF01userText >= 1000000) {
			BF10usert <- format(BF10userText, digits = 4, scientific = TRUE)
			BF01usert <- format(BF01userText, digits = 4, scientific = TRUE)
		}
		if (BF10userText < 1000000 & BF01userText < 1000000) {
			BF10usert <- formatC(BF10userText, 3, format = "f")
			BF01usert <- formatC(BF01userText, 3, format = "f")
		}

		if (oneSided == FALSE) {
			if( BF10userText >= BF01userText) {
				userBF <- bquote(BF[10] == .(BF10usert))
			} else {
				userBF <- bquote(BF[0][1] == .(BF01usert))
			}
		} else if (oneSided == "right") {
			if (BF10userText >= BF01userText) {
				userBF <- bquote(BF["+"][0] == .(BF10usert))
			} else {
				userBF <- bquote(BF[0]["+"] == .(BF01usert))
			}
		} else if (oneSided == "left") {
			if (BF10userText >= BF01userText) {
				userBF <- bquote(BF["-"][0] == .(BF10usert))
			} else {
				userBF <- bquote(BF[0]["-"] == .(BF01usert))
			}
		}

		# maximum value of Bayes factor
		if (BF10maxText >= 1000000) {
			BF10maxt <- format(BF10maxText, digits = 4, scientific = TRUE)
		} else {
			BF10maxt <- formatC(BF10maxText, 3, format = "f", drop0trailing = TRUE)
		}
		maxBFkappaValt <- formatC(maxBFkappaVal, digits = 4, format = "f", drop0trailing = TRUE )
		maxBF <- bquote(.(BF10maxt) ~ .('at r') == .(maxBFkappaValt))

		if (oneSided == FALSE) {
			maxBF10LegendText <- bquote(max~BF[1][0]*":")
		} else if (oneSided == "right") {
			maxBF10LegendText <- bquote(max~BF["+"][0]*":")
		} else if (oneSided == "left") {
			maxBF10LegendText <- bquote(max~BF["-"][0]*":")
		}

		xx <- grconvertX(0.26, "ndc", "user")
		yy <- grconvertY(0.952, "ndc", "user")

		BFind <- sort(c(BF10userText, BF10maxText), decreasing = TRUE, index.return = TRUE)$ix
		BFsort <- sort(c(BF10userText, BF10maxText), decreasing = TRUE, index.return = TRUE)$x

		legend <- c("user prior:", as.expression(maxBF10LegendText))
		pt.bg <- c("grey", "red")
		pt.cex <- c(cexPoints, 1.1)

		legend(xx, yy, legend = legend[BFind], pch = rep(21,3), pt.bg = pt.bg[BFind], bty = "n",
			cex = cexLegend, lty = rep(NULL,3), pt.lwd = rep(1.3,3), pt.cex = pt.cex[BFind])

		xx <- grconvertX(0.46, "ndc", "user")
		y1 <- grconvertY(0.898, "ndc", "user")
		y2 <- grconvertY(0.847, "ndc", "user")
		yy <- c(y1, y2)

		text(xx, yy[BFsort == BF10userText], userBF, cex = 1.3, pos = 4)
		text(xx, yy[BFsort == BF10maxText], maxBF, cex = 1.3, pos = 4)
	}
}
