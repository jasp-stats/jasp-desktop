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

BASRegressionLinearLink <- function (
	dataset = NULL,
	options,
	perform = "run",
	callback = function(...) list(status = "ok"),
	...
) {

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
	# Read the selected columns
	if (length(options$covariates) > 0 && options$dependent != "") {
		# FIXME: this function is inefficient. Read only the new columns.
		dataset <- .readBayesianLinearModelData(dataset, options, perform)
	}

	# Initialize the variables
	rowsBASRegressionLinearLink <- list()
	plotPosteriorLogOdds <- NULL
	plotCoefficientsPosterior <- NULL
	plotResidualsVsFitted <- NULL
	plotModelProbabilities <- NULL
	plotModelComplexity <- NULL
	plotInclusionProbabilites <- NULL

	# get the bas lm object
	if (length(options$covariates) > 0 && options$dependent != "" && perform == "run") {
		# FIXME: if empty, fetch from previous state
		if (options$BAS || options$MCMC) {
			bas_lm <- .calculateBASRegressionLinear(
				run = run,
				state = state,
				diff = diff,
				options = options,
				dataset = dataset
			)
			rowsBASRegressionLinearLink <- bas_lm$rows

			bas_obj <- bas_lm$bas_obj

			len.bas_obj <- length(bas_obj$namesx)
			# use the actual column names
			bas_obj$namesx[2:len.bas_obj] <- .unv(bas_obj$namesx[2:len.bas_obj])

			if (options$plotLogPosteriorOdds) {
				plotPosteriorLogOdds <- .plotPosterior.models.basRegression.linear(
					bas_obj
				)
			}

			if (options$plotCoefficientsPosterior) {
				plotCoefficientsPosterior <- .plotPosterior.coefficents.basRegression.linear(
					bas_obj, options, state
				)
			}

			if (options$plotResidualsVsFitted) {
				plotResidualsVsFitted <- .plotResidualsVsFitted.basRegression.linear(
					bas_obj, options, state, 1
				)
			}

			if (options$plotModelProbabilities) {
				plotModelProbabilities <- .plotResidualsVsFitted.basRegression.linear(
					bas_obj, options, state, 2
				)
			}

			if (options$plotModelComplexity) {
				plotModelComplexity <- .plotResidualsVsFitted.basRegression.linear(
					bas_obj, options, state, 3
				)
			}

			if (options$plotInclusionProbabilites) {
				plotInclusionProbabilites <- .plotResidualsVsFitted.basRegression.linear(
					bas_obj, options, state, 4
				)
			}
		}
	}

	# Populate the output table
	results <- .populateOutputTable.bas.linearlink(options=options)
	results$table$data <- rowsBASRegressionLinearLink

	if (options$plotLogPosteriorOdds || options$plotCoefficientsPosterior ||
		options$plotResidualsVsFitted || options$plotModelProbabilities ||
		options$plotModelComplexity || options$plotInclusionProbabilites) {
		results[["inferentialPlots"]] <- list(
			title = "Inferential Plots",
			PosteriorPlotModels = plotPosteriorLogOdds,
			ResidualsVsFittedPlot = plotResidualsVsFitted,
			ModelProbabilitiesPlot = plotModelProbabilities,
			ModelComplexityPlot = plotModelComplexity,
			InclusionProbabilitiesPlot = plotInclusionProbabilites,
			coefficentsPlots = list(
				title = "Coefficent plots",
				collection = plotCoefficientsPosterior
			)
		)
	}

	keep <- NULL

	if (run) {
		status <- "complete"
		state <- list(options = options)
	} else {
		status <- "inited"
	}

	return (list(
		keep = keep,
		results = results,
		status = status,
		state = state)
	)
}


.populateOutputTable.bas.linearlink <- function(options) {
	# Creates and returns the 'results' list required to populate
	# the output table

	meta <- list()
	meta[[1]] <- list(name = "table", type = "table")
	meta[[2]] <- list(
		name="inferentialPlots",
		type="object",
		meta=list(
			list(name = "PosteriorPlotModels", type = "image"),
			list(name = "ResidualsVsFittedPlot", type = "image"),
			list(name = "ModelProbabilitiesPlot", type = "image"),
			list(name = "ModelComplexityPlot", type = "image"),
			list(name = "InclusionProbabilitiesPlot", type = "image"),
			list(
				name = "coefficentsPlots",
				type = "collection",
				meta = list(
					name = "plotGroups",
					type = "object",
					meta = list(
						list(name = "PosteriorCoefficients", type = "image"))
		)))
	)

	fields <- list(
		list(name = "model", type = "string", title = "Model"),
		list(name = "bf", type = "number", format = "sf:4;dp:3", title = "BF"),
		list(name = "R2", type = "number", format = "sf:4;dp:3", title = "R2")
	)
	if (options$priorprobs) {
		fields[[length(fields)+1]] <- list(name = "priorprobs", type = "number", format = "sf:4;dp:3", title = "Prior")
	}
	fields[[length(fields)+1]] <- list(name = "postprobs", type = "number", format = "sf:4;dp:3", title = "Posterior")
	if (options$logmarg) {
		fields[[length(fields)+1]] <- list(name = "logmarg", type = "number", format = "sf:4;dp:3", title = "Log likelihood")
	}

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
	# table[["data"]] <- rowsBASRegressionLinearLink

	results <- list()
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Adaptive Sampling"
	results[["table"]] <- table

	return (results)
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
	#   list containing the bas_lm object and the output rows

	# generate the formula
	covariates <- .v(options$covariates)
	dependent <- .v(options$dependent)
	formula <- as.formula(paste(dependent, "~", paste(covariates, collapse="+")))

	# sampling method
	samplingMethod <- NULL
	if (options$MCMC && options$BAS) {
		samplingMethod <- "MCMC+BAS"
	} else if (options$MCMC) {
		samplingMethod <- "MCMC"
	} else if (options$BAS) {
		samplingMethod <- "BAS"
	}

	# select the type of model prior
	if (options$modelPrior == "beta.binomial") {
		modelPrior = BAS::beta.binomial(options$betaBinomialParamA, options$betaBinomialParamB)
	} else if (options$modelPrior == "uniform") {
		modelPrior = BAS::uniform()
	} else if (options$modelPrior == "Bernoulli") {
		modelPrior = BAS::Bernoulli(options$bernoulliParam)
	}

	# number of models
	n.models <- options$numberOfModels
	if (n.models == 0) {
		n.models <- NULL
	}

	# iterations for MCMC
	MCMC.iterations <- options$iterationsMCMC
	if (grepl("MCMC", samplingMethod, fixed = TRUE)) {
		# if iterations is not set by user
		if (MCMC.iterations == 0) {
			MCMC.iterations <- NULL
		}

		if (is.null(n.models)) {
			MCMC.iterations <- NULL
		} else {
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

	# FIXME: bestmodel is not given as a choice

	# Bayesian Adaptive Sampling
	bas_lm <- BAS::bas.lm(
		formula = formula,
		data = dataset,
		prior = options$priorRegressionCoefficients,
		alpha = alpha,
		modelprior = modelPrior,
		n.models = n.models,
		method = samplingMethod,
		MCMC.iterations = MCMC.iterations
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

	output.rows <- vector("list", models.number)

	# TODO: microbenchmark - compare for loop against using mapply
	for (i in 1:models.number) {
		output.rows[[i]] <- list(
			model = models.names[[i]], bf = models.bf[[i]],
			postprobs = bas_obj$postprobs[[models.ordered[i]]],
			R2 = bas_obj$R2[[models.ordered[i]]],
			priorprobs = bas_obj$priorprobs[[models.ordered[i]]],
			logmarg = bas_obj$logmarg[[models.ordered[i]]]
		)
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
	# Plot the posterior log odds for different models

	plot <- list()
	plot[["title"]] <- "Posterior Log Odds"
	plot[["width"]] <- 530
	plot[["height"]] <- 400
	plot[["status"]] <- "waiting"

	p <- try(silent = FALSE, expr = {
		image <- .beginSaveImage(530, 400)
		BAS::image.bas(bas_obj, rotate = FALSE)
		plot[["data"]] <- .endSaveImage(image)
	})

	if (class(p) == "try-error") {
		errorMessage <- .extractErrorMessage(p)
		plot[["error"]] <- list(
			error = "badData",
			errorMessage = paste("Plotting is not possible: ", errorMessage)
		)
	}
	plot[["status"]] <- "complete"

	return (plot)
}


.plotPosterior.coefficents.basRegression.linear <- function(bas_obj, options, state) {
	# Plot the posterior of the beta coefficent posterior distribution

	number.parameters <- length(bas_obj$namesx)
	returnPlots <- list()

	for (i in 1:number.parameters) {
		plot <- list()
		plot[["title"]] <- "Posterior distribution"
		plot[["width"]] <- 530
		plot[["height"]] <- 400
		plot[["status"]] <- "waiting"

		p <- try(silent = FALSE, expr = {
			image <- .beginSaveImage(530, 400)
			BAS::plot.coef.bas(BAS::coef.bas(bas_obj), subset = list(i), ask = FALSE)
			plot[["data"]] <- .endSaveImage(image)
		})

		if (class(p) == "try-error") {
			errorMessage <- .extractErrorMessage(p)
			plot[["error"]] <- list(
				error = "badData",
				errorMessage = paste("Plotting is not possible: ", errorMessage)
			)
		}
		plot[["status"]] <- "complete"

		returnPlots[[i]] <- list(
			title = bas_obj$namesx[i],
			name = bas_obj$namesx[i],
			PosteriorCoefficients = plot
		)
	}

	return (returnPlots)
}


.plotResidualsVsFitted.basRegression.linear <- function(bas_obj, state, options, number) {

	plot <- list()
	plot[["title"]] <- "Residuals Vs Fitted"
	plot[["width"]] <- 530
	plot[["height"]] <- 400
	plot[["status"]] <- "waiting"

	p <- try(silent = FALSE, expr = {
		image <- .beginSaveImage(530, 400)
		BAS::plot.bas(bas_obj, ask = FALSE, which = c(number))
		plot[["data"]] <- .endSaveImage(image)
	})

	if (class(p) == "try-error") {
		errorMessage <- .extractErrorMessage(p)
		plot[["error"]] <- list(
			error = "badData",
			errorMessage = paste("Plotting is not possible: ", errorMessage)
		)
	}
	plot[["status"]] <- "complete"

	return (plot)
}
