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

BASRegressionLinearLink <- function (dataset = NULL, options, perform = "run",
									callback = function(...) list(status = "ok"), ...) {
	# dependent <- unlist(options$dependent)
	# covariates <- unlist(options$covariates)
	#
	# if (covariates == "") {
	# 	covariates <- NULL
	# }
	#
	# if (is.null(dataset)) {
	# 	if (perform == "run") {
	# 		if (options$missingValues == "excludeListwise") {
	# 			dataset <- .readDataSetToEnd(columns.as.numeric = dependent, columns.as.factor = covariates,
	# 										exclude.na.listwise = c(dependent, covariates))
	# 		} else {
	# 			dataset <- .readDataSetToEnd(columns.as.numeric = dependent, columns.as.factor = covariates,
	# 										exclude.na.listwise = covariates)
	# 		}
	# 	} else {
	# 		dataset <- .readDataSetHeader(columns.as.numeric = dependent, columns.as.factor = covariates)
	# 	}
	# }
	run <- (perform == "run")
	state <- .retrieveState()
	if (!is.null(state)) {
		change <- .diff(options, state$options)
		if (!base::identical(change, FALSE) && (change$dependent || change$priorCovariates)) {
			state <- NULL
		} else {
			perform <- "run"
		}
	}

	# data
	# if (is.null(state)) {
	# 	dataset <- .readBayesianLinearModelData(dataset, options, perform)
	# }
	if (length(options$covariates) > 0 && options$dependent != "") {
		# FIXME: this is inefficient. Read only the new columns.
		dataset <- .readBayesianLinearModelData(dataset, options, perform)
	}

	# initialize
	rowsBASRegressionLinearLink <- list()
	plot <- NULL

	if (length(options$covariates) > 0 && options$dependent != "" && perform == "run") {
		bas_lm <- .calculateBASRegressionLinear(
			run = run,
			state = state,
			diff = diff,
			options = options,
			dataset = dataset
		)

		rowsBASRegressionLinearLink <- bas_lm$rows

		plot <- .plotPosterior.models.basRegression.linear(bas_lm$bas_obj)
	}

	# Populate the output table
	meta <- list()
	meta[[1]] <- list(name = "table", type = "table")
	meta[[2]] <- list(name = "inferentialPlots", type = "object",
						meta = list(list(name = "PosteriorPlotModels", type = "image"))
					)

	if (options$bayesFactorType == "BF10") {
		bfm.title <- "BF<sub>M</sub>"
		bf.title <- "BF<sub>10</sub>"
	} else if (options$bayesFactorType == "BF01") {
		bfm.title <- "BF<sub>M</sub>"
		bf.title <- "BF<sub>01</sub>"
	} else if (options$bayesFactorType == "LogBF10") {
		bfm.title <- "Log(BF<sub>M</sub>)"
		bf.title <- "Log(BF<sub>10</sub>)"
	}

	fields <- list(
		list(name = "model", type = "string", title = "Model"),
		list(name = "bf", type = "number", format = "sf:4;dp:3", title = "BF")
	)

	# fields <- list(
	# 			list(name = "model", type = "string"),
	# 			list(name = "P(M)", type = "number", format = "sf:4;dp:3"),
	# 			list(name = "P(M|data)", type = "number", format = "sf:4;dp:3;log10"),
	# 			list(name = "BFM", type = "number", format = "sf:4;dp:3;log10",
	# 					title = paste (bfm.title, sep = "")),
	# 			list(name = "bf", type = "number", format = "sf:4;dp:3;log10",
	# 					title = paste (bf.title, sep = "")),
	# 			list(name = "error %", type="number", format="sf:4;dp:3")
	# 		)
	# if (options$bayesFactorType == "LogBF10") {
	# 	fields[[4]] <- list(name = "BFM", type = "number", format = "sf:4;dp:3", title = paste (bfm.title, sep = ""))
	# 	fields[[5]] <- list(name = "BF10", type = "number", format = "sf:4;dp:3", title = paste (bf.title, sep = ""))
	# }

	# # add footnotes to the analysis result
	# footnotes <- .newFootnotes()
	# if (options$hypothesis != "notEqualToTestValue") {
	# 	.addFootnote(footnotes, symbol = "<em>Note.</em>", text = hypothesis.variables$message)
	# }

	table <- list()
	table[["title"]] <- "Model comparison"
	table[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
	table[["schema"]] <- list(fields = fields)
	table[["data"]] <- rowsBASRegressionLinearLink

	# print(rowsBASRegressionLinearLink)

	results <- list()
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Adaptive Sampling"
	results[["table"]] <- table

	if (!is.null(plot)) {
		results[["inferentialPlots"]] <- list(
										title = "Inferential Plot",
										PosteriorPlotModels = plot
								)
	}

	keep <- NULL

	if (run) {
		status <- "complete"
		state <- list(options = options)
	} else {
		status <- "inited"
	}

	return (list(results = results,
				status = status,
				state = state,
				keep = keep)
			)
}


.calculateBASRegressionLinear <- function(run, state, diff, options, dataset) {
	# Bayesian Adaptive Sampling without replacement for Variable selection
	# in Linear Models
	#
	# Args:
	#   run: state of analysis - init or run
	#   options: a list of user options
	#   state: previous options state
	#   diff: diff between previous and current options
	#   dataset: dataset input by user
	#
	# Return:
	#

	# generate the formula
	covariates <- .v(options$covariates)
	dependent <- .v(options$dependent)
	formula <- as.formula(paste(dependent, "~", paste(covariates, collapse="+")))

	# FIXME: betaBinomialParamA and B are NULL
	# select the type of model prior
	if (options$modelPrior == "beta.binomial") {
		modelPrior = BAS::beta.binomial(options$betaBinomialParamA, options$betaBinomialParamB)
	} else if (options$modelPrior == "uniform") {
		modelPrior = BAS::uniform()
	}

	# iterations for MCMC
	MCMC.iterations <- NULL
	if (grepl("MCMC", options$samplingMethod, fixed = TRUE)) {
		MCMC.iterations <- options$posteriorEstimatesMCMCIterations
		# if iterations is not set by user
		if (MCMC.iterations == 0 || is.null(MCMC.iterations)) {
			MCMC.iterations <- options$numberOfModels * 10
		}
	}

	# hyper parameter for g-prior
	alpha <- switch(
		options$priorRegressionCoefficients,
		g_prior =,
		hyper_g =,
		hyper_g_laplace =,
		zs_null =,
		zs_full = options$gPriorParameter,
		NULL
	)

	print(options$betaBinomialParamB)
	print(options$betaBinomialParamA)

	# FIXME: sampling method currently does not allow specification of MCMC+BAS
	# FIXME: bestmodel is not given as a choice

	# print(options)

	# Bayesian Adaptive Sampling
	bas_lm <- BAS::bas.lm(
		formula = formula,
		data = dataset,
		# prior = options$priorRegressionCoefficients,
		# alpha = alpha,
		# modelprior = modelPrior,
		# n.models = options$numberOfModels,
		# method = options$samplingMethod,
		# MCMC.iterations = options$iterationsMCMC
	)

	rowsBASRegressionLinearLink <- .getOutputRowBASLinearLink(
		bas_obj = bas_lm,
		options = options,
		state = state,
		dataset = dataset
	)

	return (list(bas_obj = bas_lm, rows = rowsBASRegressionLinearLink))
}


.getOutputRowBASLinearLink <- function(bas_obj, options, state, dataset) {
	# Return the output row
	# Args:
	#    - bas_obj: bas object
	#
	# Return:
	#    - list containing row/s

	# default number of models to be shown
	models.number = 5  # FIXME: get number of models from user-input

	if (models.number > length(bas_obj$which)) {
		models.number <- length(bas_obj$which)
	}

	# TODO: show the null model - only intercept

	# ordered indices based on posterior probabilities of the models
	models.ordered <- base::order(bas_obj$postprobs, decreasing = TRUE)[1:models.number]
	models <- bas_obj$which[models.ordered]


	# print("=--------=")
	# print(.unv(colnames(dataset)))
	# print(options$covariates)
	# print(models[[1]])
	# print(.unv(bas_obj$namesx[2:length(bas_obj$namesx)]))
	# print("=--------=")

	# generate the model names
	models.names <- base::lapply(
		models,
		function(x) {
			return (paste(.unv(bas_obj$namesx[2:length(bas_obj$namesx)])[x[2:length(x)]],
					collapse = " + "))
		}
	)

	# get the Bayes factors for the models
	models.bf <- exp(bas_obj$logmarg[models.ordered] -
						max(bas_obj$logmarg[models.ordered]))

	# print(models.names)

	output.rows <- vector("list", models.number)

	# TODO: microbenchmark - compare this against using mapply
	for (i in 1:models.number) {
		output.rows[[i]] <- list(model = models.names[[i]], bf = models.bf[[i]])
	}
	# output.list <- as.list(data.frame(mapply(c, models.names, models.bf)))
	#
	# # Generate the output row
	# output.rows <- vector("list", length(output.list))
	# row.number <- 1
	#
	# for (row in output.list) {
	# 	output.rows
	# }
	#
	# output.row <- base::lapply(
	# 	output.list,
	# 	function(x) {
	# 		names(x) <- c("model", "bf")
	# 		return (as.list(x))
	# 	}
	# )

	return (output.rows)
}


.plotPosterior.models.basRegression.linear <- function(bas_obj) {
	#

	len.bas_obj <- length(bas_obj)
	# use the actual column names
	# bas_obj$namesx[2:len.bas_obj] <- .unv(bas_obj$namesx[2:len.bas_obj])
	print(bas_obj$namesx)

	width  <- 530
	height <- 400

	plot <- list()
	plot[["title"]]  <- "Bayes Factor Robustness Check"
	plot[["width"]]  <- width
	plot[["height"]] <- height
	plot[["status"]] <- "waiting"

	p <- try(silent = FALSE, expr = {
		image <- .beginSaveImage(width, height)

		BAS::image.bas(bas_obj, rotate = FALSE)

		plot[["data"]] <- .endSaveImage(image)
	})

	if (class(p) == "try-error") {
		errorMessage <- .extractErrorMessage(p)
		plot[["error"]] <- list(error="badData",
						errorMessage = paste("Plotting is not possible: ", errorMessage))
	}

	plot[["status"]] <- "complete"

	return (plot)
}
