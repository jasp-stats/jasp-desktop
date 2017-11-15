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
	state = NULL,
	...
) {

	# Configure the state
	modelOpts <- c("dependent", "covariates", "wlsWeights", "modelTerms",
						 		 "priorRegressionCoefficients", "gPriorParameter",
						 	 	 "modelPrior", "betaBinomialParamA", "betaBinomialParamB", "bernoulliParam",
						 	 	 "samplingMethod", "iterationsMCMC", "numberOfModels")
	stateKey <- list(
		bas_obj = modelOpts,
		descriptives = c("dependent", "covariates"),
		plotPosteriorLogOdds = c(modelOpts, "plotLogPosteriorOdds"),
		plotCoefficientsPosterior = c(modelOpts, "plotCoefficientsPosterior"),
		plotResidualsVsFitted = c(modelOpts, "plotResidualsVsFitted"),
		plotModelProbabilities = c(modelOpts, "plotModelProbabilities"),
		plotModelComplexity = c(modelOpts, "plotModelComplexity"),
		plotInclusionProbabilities = c(modelOpts, "plotInclusionProbabilities")
	) # TODO: check if this dependence is correct

	# Initialize the variables
	bas_obj <- state$bas_obj
	descriptives <- state$descriptives
	plotPosteriorLogOdds <- state$plotPosteriorLogOdds
	plotCoefficientsPosterior <- state$plotCoefficientsPosterior
	plotResidualsVsFitted <- state$plotResidualsVsFitted
	plotModelProbabilities <- state$plotModelProbabilities
	plotModelComplexity <- state$plotModelComplexity
	plotInclusionProbabilities <- state$plotInclusionProbabilities
	status <- state$status

	# Read the selected columns
	if (length(options$covariates) > 0 && options$dependent != "") {
		# FIXME: this function is inefficient. Read only the new columns.
		dataset <- .readData.basReg(perform, options)
	}

	# Set the status
	if (is.null(bas_obj)) { # status can only change in bas_obj
		status <- .setStatus.basReg(dataset, perform, options)
	}

	# Calculate the necessary components
	if (perform == "run" && status$ready) {
		# get the bas lm object
		if (is.null(bas_obj)) {
			bas_output <- .calcModel.basReg(
				dataset = dataset,
				status = status,
				options = options
			)

			status <- bas_output$status
			bas_obj <- bas_output$bas_obj

			if (! status$error) {
				# use the actual column names
				len.bas_obj <- length(bas_obj$namesx)
				bas_obj$namesx[2:len.bas_obj] <- .unvf(bas_obj$namesx[2:len.bas_obj]) # first is intercept
			}
		}

	}

	regTableData <- NULL
	if (! is.null(bas_obj)) {
		# Populate the output table
		regTableData <- .calcDataRegTable.basReg(
				bas_obj = bas_obj,
				status = status,
				options = options
		)
	}

	if (options$descriptives && is.null(descriptives)) {
		descriptives <- .descriptivesTable.basReg(
			dataset, status, perform, options
		)
	}

	if (options$plotLogPosteriorOdds && is.null(plotPosteriorLogOdds)) {
		plotPosteriorLogOdds <- .plotLogOdds.basReg(
			bas_obj = bas_obj, status = status, perform = perform
		)
	}

	if (options$plotCoefficientsPosterior && is.null(plotCoefficientsPosterior)) {
		plotCoefficientsPosterior <- .plotPosterior.basReg(
			bas_obj = bas_obj, status = status, perform = perform
		)
	}

	if (options$plotResidualsVsFitted && is.null(plotResidualsVsFitted)) {
		plotResidualsVsFitted <- .plotDiagnostics.basReg(
			bas_obj = bas_obj, status = status, perform = perform, which = 1
		)
	}

	if (options$plotModelProbabilities && is.null(plotModelProbabilities)) {
		plotModelProbabilities <- .plotDiagnostics.basReg(
			bas_obj = bas_obj, status = status, perform = perform, which = 2
		)
	}

	if (options$plotModelComplexity && is.null(plotModelComplexity)) {
		plotModelComplexity <- .plotDiagnostics.basReg(
			bas_obj = bas_obj, status = status, perform = perform, which = 3
		)
	}

	if (options$plotInclusionProbabilities && is.null(plotInclusionProbabilities)) {
		plotInclusionProbabilities <- .plotDiagnostics.basReg(
			bas_obj = bas_obj, status = status, perform = perform, which = 4
		)
	}

	# Assign to results
	results <- list()
	results[[".meta"]] <- .createMeta.basReg()
	results[["title"]] <- "Bayesian Adaptive Sampling"

	results[["regressionTable"]] <- .fillRegTable.basReg(data = regTableData,
		status = status, perform = perform, options = options)

	if (options$descriptives) {
		results[["descriptivesTable"]] <- descriptives
	}

	if (options$plotLogPosteriorOdds || options$plotCoefficientsPosterior ||
		options$plotResidualsVsFitted || options$plotModelProbabilities ||
		options$plotModelComplexity || options$plotInclusionProbabilities) {

		results[["inferentialPlots"]] <-
			list(
				title = "Inferential Plots",
				PosteriorPlotModels = plotPosteriorLogOdds,
				ResidualsVsFittedPlot = plotResidualsVsFitted,
				ModelProbabilitiesPlot = plotModelProbabilities,
				ModelComplexityPlot = plotModelComplexity,
				InclusionProbabilitiesPlot = plotInclusionProbabilities,
				coefficentsPlots =
					list(
						title = "Coefficent plots",
						collection = plotCoefficientsPosterior
					)
			)

	}

	# Set keep and the state
	if (perform == "init") {
		statusAnalysis <- "inited"
		keep <- state$keep
	} else { #run
		statusAnalysis <- "complete"
		keep <- c(
			plotPosteriorLogOdds$data,
			sapply(plotCoefficientsPosterior, function(x) x$PosteriorCoefficients$data),
			plotResidualsVsFitted$data,
			plotModelProbabilities$data,
			plotModelComplexity$data,
			plotInclusionProbabilities$data
		)

		state <- list(
			options = options,
			bas_obj = bas_obj,
			descriptives = descriptives,
			plotPosteriorLogOdds = plotPosteriorLogOdds,
			plotCoefficientsPosterior = plotCoefficientsPosterior,
			plotResidualsVsFitted = plotResidualsVsFitted,
			plotModelProbabilities = plotModelProbabilities,
			plotModelComplexity = plotModelComplexity,
			plotInclusionProbabilities = plotInclusionProbabilities,
			status = status,
			keep = keep
		)
		attr(state, "key") <- stateKey
	}

	return (list(
		keep = keep,
		results = results,
		status = statusAnalysis,
		state = state)
	)
}

.createMeta.basReg <- function() {
	# Creates and returns the 'meta' list required as a template to populate
	# the output
	#
	# Return:
	#   A meta object

	meta <- list()
	meta[[1]] <- list(name = "regressionTable", type = "table")
	meta[[2]] <- list(name = "descriptivesTable", type = "table")
	meta[[3]] <- list(
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

	return (meta)
}

.readData.basReg <- function(perform, options) {
	# Get the relevant data
	#
	# Args:
	#   perform: 'run' or 'init'
	#   options: a list of user options
	#
	# Return:
	#   The (numeric) columns given as dependent/covariates/wlsWeights

	vars <- c(options$dependent, unlist(options$covariates))
	if (options$wlsWeights != "") {
		vars <- c(vars, options$wlsWeights)
	}

	if (perform == "run") {
		dataset <- .readDataSetToEnd(columns.as.numeric = vars, exclude.na.listwise = vars)
	} else {
		dataset <- .readDataSetHeader(columns.as.numeric = vars)
	}

	return (dataset)
}

.setStatus.basReg <- function(dataset, perform, options) {
	# Create status object and do error checking; exit the analysis if any errors are found
	#
	# Args:
	#   dataset: dataset input by user
	#   perform: 'run' or 'init'
	#   options: a list of user options
	#
	# Return:
	#   A status object containing "ready", "error", "error.message"

	status <- list(ready = TRUE, error = FALSE, error.message = NULL)

	if (options$dependent == "" || length(options$modelTerms) == 0) {
		status$ready <- FALSE
	}

	customChecks <- list(
		function() {
			nuisanceTerms <- sapply(options$modelTerms, function(term) term$isNuisance)
			if (sum(nuisanceTerms) == length(options$modelTerms)) {
				return("All effects are specified as nuisance")
			}
		},

		function() {
			maxModelComponents <- max(sapply(options$modelTerms, function(term) length(term$components)))
			if (maxModelComponents < 2) {
				return()
			}
			for (term in options$modelTerms) {
				if (length(term$components) < 2) {
					next
				}
				required <- 2^length(term$components) - 1
				included <- 0
				for (term2 in options$modelTerms) {
					if (all(term2$components %in% term$components)) {
						included <- included + 1
					}
				}
				if (sum(included) != required) {
					return("Main effects and lower-order interactions must be included whenever the corresponding higher-order interaction is included")
				}
			}
		},

		function() {
			for (term in options$modelTerms) {
				if (term$isNuisance == FALSE || length(term$components) < 2) {
					next
				}
				for (term2 in options$modelTerms) {
					if (term2$isNuisance == TRUE) {
						next
					}
					if (all(term2$components %in% term$components)) {
						return("Main effects and lower-order interactions must be specified as nuisance whenever the corresponding higher-order interaction is specified as nuisance")
					}
				}
			}
		},

		function() {
			if (options$wlsWeights != "") {
				weightsVar <- options$wlsWeights
				min.weight <- min(dataset[[ .v(weightsVar) ]])
				if (min.weight <= 0) {
					return("There are nonpositive weights")
				}
			}
		})

	.hasErrors(dataset = dataset, perform = perform,
		type=c("infinity", "observations", "variance"), custom = customChecks,
		infinity.target = c(options$covariates, options$dependent, options$wlsWeight),
		observations.target = options$dependent, observations.amount = paste("<", length(options$modelTerms) + 1),
		variance.target = c(options$covariates, options$dependent),
		exitAnalysisIfErrors = TRUE)

	return(status)
}

.calcModel.basReg <- function(dataset, status, options) {
	# Bayesian Adaptive Sampling without replacement for Variable selection
	# in Linear Models
	#
	# Args:
	#   dataset: dataset input by user
	#   status: current status of the analysis
	#   options: a list of user options
	#
	# Return:
	#   list containing the bas_lm object (containing also a vector
	#		describing which are nuisance terms) and the status object

	# generate the formula and identify nuisance terms
	nPreds <- length(options$modelTerms)
	isNuisance <- rep(FALSE, nPreds)
	formula <- c(options$dependent, "~")
	for (i in 1:length(options$modelTerms)) {
		term <- options$modelTerms[[i]]
		termName <- paste(term$component, collapse=":")
		names(isNuisance)[i] <- termName
		sep <- ifelse(i == 1, "", "+")
		formula <- c(formula, sep, termName)
		if (term$isNuisance) {
			isNuisance[i] <- TRUE
		}
	}
	names(isNuisance) <- .vf(names(isNuisance))
	formula <- as.formula(.vf(paste(formula, collapse="")))

	# set initprobs (BAS' method for nuisance terms)
	initProbs <- rep(0.5, nPreds + 1) # the + 1 is the intercept
	index <- c(1, which(isNuisance) + 1)
	initProbs[index] <- 1

	# get the weights
	wlsWeights <- NULL
	if (options$wlsWeights != "") {
		weightsVar <- options$wlsWeights
		wlsWeights <- dataset[[ .v(weightsVar) ]]
	}

	# sampling method
	if (options$samplingMethod == "MCMCBAS") {
		samplingMethod <- "MCMC+BAS"
	} else {
		samplingMethod <- options$samplingMethod
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
			MCMC.iterations <- options$numberOfModels * 10 #FIXME: should we allow models to influence MCMC samples?
		}
	}

	# hyper parameter for g-prior
	alpha <- switch(
		options$priorRegressionCoefficients,
		g_prior =,
		hyper_g =,
		hyper_g_laplace =,
		hyper_g_n=,
		zs_null =,
		zs_full = options$gPriorParameter,
		NULL
	)

	# Bayesian Adaptive Sampling
	bas_lm <- try(BAS::bas.lm(
		formula = formula,
		data = dataset,
		prior = options$priorRegressionCoefficients,
		alpha = alpha,
		modelprior = modelPrior,
		n.models = n.models,
		method = samplingMethod,
		MCMC.iterations = MCMC.iterations,
		initprobs = initProbs,
		weights = wlsWeights
	))

	if (isTryError(bas_lm)) {
		status$ready <- FALSE
		status$error <- TRUE
		status$error.message <- .extractErrorMessage(bas_lm)
	} else {
		bas_lm[["nuisanceTerms"]] <- isNuisance
		# fix for prior probs all returning 1 with uniform and bernoulli 0.5 priors
		bas_lm[["priorprobs"]] <- bas_lm[["priorprobs"]] / sum(bas_lm[["priorprobs"]])
	}

	return(list(bas_obj = bas_lm, status = status))
}

.calcDataRegTable.basReg <- function(bas_obj, status, options) {
	# Calculate the data needed for the main table
	#
	# Args:
	#   bas_obj: bas object (including nuisanceTerms entry)
	#   status: current status of the analysis
	#   options: a list of user options
	#
	# Return:
	#   list with table footnotes and data rows (ready to insert in 'data' of table)

	if (status$error == TRUE) {
		return(NULL)
	}

	nuisanceTerms <- bas_obj$nuisanceTerms # vector of TRUE / FALSE

	# default number of models to be shown
	nRows <- NULL
	if (options$shownModels == "limited") {
		nRows <- options$numShownModels
	}

	if (is.null(nRows) || nRows > length(bas_obj$which)) {
		nRows <- length(bas_obj$which)
	}

	# ordered indices based on posterior probabilities of the models
	# TODO: determine if we like this functionality ^.^
	models.ordered <- order(bas_obj$postprobs, decreasing = TRUE)
	if (options$bayesFactorOrder == "nullModelTop") {
		bestModelIndices <- models.ordered[1:nRows]
		if (1 %in% bestModelIndices) { # null model has index 1
			index <- which(bestModelIndices == 1)
			models.ordered <- c(1, bestModelIndices[-index]) # change position of null model
		} else {
			models.ordered <- c(1, bestModelIndices[-nRows]) # remove last model
		}
	} else { # best model top
		models.ordered <- models.ordered[1:nRows]
	}
	models <- bas_obj$which[models.ordered]

  # null model name
	null.model <- "Null model"
	if (sum(nuisanceTerms) > 0) {
		null.model <- paste("Null model (incl. ", paste(.unvf(names(which(nuisanceTerms))), collapse = ", "), ")", sep = "")
	}

	# generate all model names
  model.names <- vector("character", length(models))
  for (i in 1:length(models)) {
    model <- models[[i]]
    if (length(model) == 1) { # only has intercept
      model.names[i] <- null.model
      next
    }
    model <- model[-1]  # pop the intercept term (not in the nuisance vector)
    nuisanceInModel <- sum(nuisanceTerms[model])
    if (nuisanceInModel == length(model)) { # found the null model
      model.names[i] <- null.model
    } else {
      nonNuisance <- which(!nuisanceTerms[model])
      model.names[i] <- paste(.unvf(names(nonNuisance)), collapse = " + ")
    }
  }

	# get the Bayes factors for the models
	if (options$bayesFactorType == "BF10") {
		models.bf <- exp(bas_obj$logmarg[models.ordered] -
							bas_obj$logmarg[models.ordered][1])
	} else if (options$bayesFactorType == "BF01") {
		models.bf <- exp(bas_obj$logmarg[models.ordered][1] -
							bas_obj$logmarg[models.ordered])
	} else { # logBF10
		models.bf <- bas_obj$logmarg[models.ordered] -
							bas_obj$logmarg[models.ordered][1]
	}

	output.rows <- vector("list", nRows)

	# TODO: microbenchmark - compare for loop against using mapply
	for (i in 1:nRows) {
		output.rows[[i]] <- list(
			"Models" = model.names[i],
			"BF" = models.bf[i],
			"P(M|data)" = bas_obj$postprobs[[models.ordered[i]]],
			"R2" = bas_obj$R2[[models.ordered[i]]],
			"P(M)" = bas_obj$priorprobs[[models.ordered[i]]],
			"logmarg" = bas_obj$logmarg[[models.ordered[i]]]
		)
	}

	# notes
	footnotes <- NULL
	if (sum(nuisanceTerms) > 0) {
		footnotes <- .newFootnotes()
		footnote <- paste("All models include ", paste(.unvf(names(which(nuisanceTerms))), collapse = ", "), ".", sep = "")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = footnote)
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

	return(list(rows=output.rows, notes=footnotes))
}

.fillRegTable.basReg <- function(data = NULL, status, perform, options) {
	# Fills and returns the complete main table
	#
	# Args:
	#   data: list with footnotes and data rows calculated by .calcDataRegTable.basReg()
	#   status: current status of the analysis
	#   perform: 'run' or 'init'
	#   options: a list of user options
	#
	# Return:
	#   List with completed table; may be inserted in results as is

	if (options$bayesFactorType == "BF10") {
		bf.title <- "BF<sub>10</sub>"
	} else if (options$bayesFactorType == "BF01") {
		bf.title <- "BF<sub>01</sub>"
	} else if (options$bayesFactorType == "LogBF10") {
		bf.title <- "Log(BF<sub>10</sub>)"
	}

	fields <-
		list(
			list(name = "Models", type = "string"),
			list(name = "P(M)", type = "number", format = "sf:4;dp:3"),
			list(name = "P(M|data)", type = "number", format = "sf:4;dp:3"),
			list(name = "BF", type = "number", format = "sf:4;dp:3", title = paste(bf.title, sep = "")),
			list(name = "R2", type = "number", format = "dp:3", title = "R\u00B2")
		)

	if (options$logmarg) {
		fields[[length(fields)+1]] <- list(name = "logmarg", type = "number", format = "sf:4;dp:3", title = "Log likelihood")
	}

	table <- list()
	table[["title"]] <- "Model Comparison"
	table[["citation"]] <- list(
		"Clyde, M. A. (2017). BAS: Bayesian Adaptive Sampling for Bayesian Model Averaging. (Version 1.4.7)[Computer software].", #FIXME: fix version
		"Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive sampling for variable selection and model averaging. Journal of Computational and Graphical Statistics, 20, 80-101.")
	table[["schema"]] <- list(fields = fields)

	if (! is.null(data) && ! status$error) {
		table[["data"]] <- data$rows
		table[["footnotes"]] <- as.list(data$notes)
	} else {
		names <- sapply(fields, function(x) x$name)
		table[["data"]][[1]] <- setNames(as.list(rep(".", length(names))), names)
	}

	if (status$error) {
		table[["error"]] <- list(errorType = "badData", errorMessage = status$error.message)
	}

	return (table)
}

.descriptivesTable.basReg <- function(dataset, status, perform, options) {
	# Generate a descriptives table (mean, N, SD) of the dependent/covariates
	#
	# Args:
	#   dataset: data read by .readData.basReg()
	#   status: current status of the analysis
	#   perform: 'run' or 'init'
	#   options: a list of user options
	#
	# Return:
	#   List with completed descriptives table; may be inserted in results as is

	descriptives <- list()

	descriptives[["title"]] <- "Descriptives"

	fields <-
		list(
			list(name="v",    title="",   type="string"),
			list(name="N",    title="N",  type="integer"),
			list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
			list(name="sd",   title="SD", type="number",   format="sf:4;dp:3")
	)

	descriptives[["schema"]] <- list(fields=fields)

	if (status$ready) {
		rows <- list()
		variables <- c(options$dependent, unlist(options$covariates))

			for (variable in variables) {
				if (perform == "run") {

					data <- na.omit(dataset[[ .v(variable) ]])
					n <- .clean(length(data))
					mean <- .clean(mean(data))
					sd <- .clean(sd(data))

					rows[[length(rows) + 1]] <- list(v = variable, N = n, mean = mean, sd = sd)

				} else {

					rows[[length(rows) + 1]] <- list(v = variable, N = ".", mean = ".", sd = ".")

				}
			}
			descriptives[["data"]] <- rows

		} else {
			names <- sapply(fields, function(x) x$name)
			descriptives[["data"]][[1]] <- setNames(as.list(rep(".", length(names))), names)
		}

	return (descriptives)
}


.plotLogOdds.basReg <- function(bas_obj, status, perform) {
	# Plot the posterior log odds for different models
	#
	# Args:
	#   bas_obj: bas object
	#   status: current status of the analysis
	#   perform: 'run' or 'init'
	#
	# Return:
	#   list with plot data

	# if there were errors or we're in init, show just empty x and y-axes
	emptyPlot <- .makeEmptyPlot.basReg(title = "Posterior Log Odds", status = status)

	if (status$ready && perform == "run") {

		plot <- list()
		plot[["title"]] <- "Posterior Log Odds"

		p <- try(silent = FALSE, expr = {

			plotFunc <- function() {
				BAS:::image.bas(bas_obj, rotate = FALSE)
			}
			content <- .writeImage(width = 530, height = 400, plot = plotFunc)

			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]
			plot[["status"]] <- "complete"

		})

		if (isTryError(p)) {
			errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
			plot <- emptyPlot
			plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
		}

	} else { # init or errors

		plot <- emptyPlot

	}

	return(plot)
}


.plotPosterior.basReg <- function(bas_obj, status, perform) {
	# Plot the posterior of the beta coefficent posterior distribution
	#
	# Args:
	#   bas_obj: bas object
	#   status: current status of the analysis
	#   perform: 'run' or 'init'
	#
	# Return:
	#   list (collection) of lists with plot data

	# if there were errors or we're in init, show just empty x and y-axes
	emptyPlot <- .makeEmptyPlot.basReg(title = "Posterior distribution", status = status)

	if (status$ready && perform == "run") {

		isNuisance <- bas_obj$nuisanceTerms
		number.parameters <- length(bas_obj$namesx)
		plots <- list()

		for (i in 1:number.parameters) {

			terms <- .unvf(names(isNuisance))
			term <- bas_obj$namesx[i]
			if (term != "Intercept" && isNuisance[which(terms == term)]) {
				next
			}

			plot <- list()
			plot[["title"]] <- "Posterior distribution"

			p <- try(silent = FALSE, expr = {

				plotFunc <- function() {
					BAS:::plot.coef.bas(BAS:::coef.bas(bas_obj), subset = list(i), ask = FALSE,
															bty = "n")
				}
				content <- .writeImage(width = 530, height = 400, plot = plotFunc)

				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				plot[["status"]] <- "complete"

			})

			if (isTryError(p)) {
				errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
				plot <- emptyPlot
				plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
			}

			plots[[length(plots) + 1]] <- list(
				title = term,
				name = term,
				PosteriorCoefficients = plot
			)
		}

	} else { # init or errors

		plots <- list(list(
			title = "",
			name = "",
			PosteriorCoefficients = emptyPlot
		))

	}

	return(plots)
}

.plotDiagnostics.basReg <- function(bas_obj, status, perform, which) {
	# Plot several different diagnostics of the BAS object
	#
	# Args:
	#   bas_obj: bas object
	#   status: current status of the analysis
	#   perform: 'run' or 'init'
	#		which: integer specifying which type of diagnostic to plot:
	#					 1=residuals, 2=model probs, 3=model complexity, 4=inclusion probs
	#
	# Return:
	#   list with plot data

	titles <- c(
		"Residuals vs Fitted",
		"Model Probabilities",
		"Model Complexity",
		"Inclusion Probabilities"
	)

	# if there were errors or we're in init, show just empty x and y-axes
	emptyPlot <- .makeEmptyPlot.basReg(title = titles[which], status = status)

	if (status$ready && perform == "run") {

		plot <- list()
		plot[["title"]] <- titles[which]

		p <- try(silent = FALSE, expr = {

			plotFunc <- function() {
				BAS:::plot.bas(bas_obj, ask = FALSE, which = c(which), caption = "",
											 bty = "n")
			}
			content <- .writeImage(width = 530, height = 400, plot = plotFunc)

			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]
			plot[["status"]] <- "complete"

		})

		if (isTryError(p)) {
			errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
			plot <- emptyPlot
			plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
		}

	} else { # init or errors

		plot <- emptyPlot

	}

	return(plot)
}

.makeEmptyPlot.basReg <- function(xlab = NULL, ylab = NULL, title = NULL, status) {
	# Convenience function to create empty x and y-axes
	#
	# Args:
	#   xlab: label for x-axis
	#   ylab: label for y-axis
	#   title: title of the plot in the meta list
	#   status: current status of the analysis
	#
	# Return:
	#   list with an empty plot (if there are errors in the analysis it is a badData plot)

	plot <- list()
	plot[["title"]] <- title

	plotFunc <- function() {
		plot(1, type = "n", xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, xlab = "", ylab = "")

		axis(1, at = 0:1, labels = FALSE, cex.axis = 1.6, lwd = 2, xlab = "")
		axis(2, at = 0:1, labels = FALSE, cex.axis = 1.6, lwd = 2, ylab = "")

		if (is.null(ylab)) {
			mtext(text = ylab, side = 2, las = 0, cex = 1.6, line = 3.25)
		}

		if (is.null(xlab)) {
			mtext(xlab, side = 1, cex = 1.5, line = 2.6)
		}
	}

	content <- .writeImage(width = 530, height = 400, plot = plotFunc)

	plot[["obj"]] <- content[["obj"]]
	plot[["data"]] <- content[["png"]]
	plot[["status"]] <- "complete"

	if (status$error) {
		plot[["error"]] <- "badData"
	}

	return(plot)
}
