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

RegressionLinearBayesian <- function (
	dataset = NULL,
	options,
	perform = "run",
	callback = function(...) list(status = "ok"),
	state = NULL,
	...
) {

	# Configure the state
	modelOpts <- c("dependent", "covariates", "wlsWeights", "modelTerms",
						 		 "priorRegressionCoefficients", "alpha", "rScale",
						 	 	 "modelPrior", "betaBinomialParamA", "betaBinomialParamB", "bernoulliParam",
						 	 	 "samplingMethod", "iterationsMCMC", "numberOfModels")
	stateKey <- list(
		bas_obj = modelOpts,
		postSummary = c(modelOpts, "postSummary", "summaryType"),
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
	postSummary <- state$postSummary
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
	
	if (options$postSummary && is.null(postSummary)) {
		postSummary <- .posteriorSummaryTable.basReg(
			bas_obj, status, perform, options
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
	results[["title"]] <- "Bayesian Linear Regression"

	results[["regressionTable"]] <- .fillRegTable.basReg(data = regTableData,
		status = status, perform = perform, options = options)

	if (options$postSummary) {
		results[["posteriorSummaryTable"]] <- postSummary
	}

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

		# code below tries to avoid weird behaviour from bas_obj that chrashes base::save. 
		attr(attr(bas_obj[["model"]], "terms"), ".Environment") <- NULL
		attr(bas_obj$terms, ".Environment") <- NULL
		# reparsing the object seems to avoid errors with base::save.
		try(bas_obj[["mle.se"]] <- eval(parse(text = capture.output(dput(bas_obj[["mle.se"]])))))

		state <- list(
			options = options,
			bas_obj = bas_obj,
			postSummary = postSummary,
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

	return(list(
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
	meta[[2]] <- list(name = "posteriorSummaryTable", type = "table")
	meta[[3]] <- list(name = "descriptivesTable", type = "table")
	meta[[4]] <- list(
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

	return(meta)
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

	return(dataset)
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

	customChecks <- list( #TODO: add these to hasErrors officially
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

	# select the type of model prior
	if (options$modelPrior == "beta.binomial") {
		modelPrior <- BAS::beta.binomial(options$betaBinomialParamA, options$betaBinomialParamB)
	} else if (options$modelPrior == "uniform") {
		modelPrior <- BAS::uniform()
	} else if (options$modelPrior == "Bernoulli") {
		modelPrior <- BAS::Bernoulli(options$bernoulliParam)
	}

	# number of models
	n.models <- NULL
	if (options$samplingMethod == "BAS" && options$numberOfModels > 0) {
		n.models <- options$numberOfModels
	}

	# iterations for MCMC
	MCMC.iterations <- NULL
	if (options$samplingMethod == "MCMC" && options$iterationsMCMC > 0) {
		MCMC.iterations <- options$numberOfModels
	}

	# parameter for hyper-g's or jzs (all use same alpha param in bas.lm)
	alpha <- switch(
		options$priorRegressionCoefficients,
		hyper_g = options$alpha,
		hyper_g_laplace = options$alpha,
		hyper_g_n = options$alpha,
		JZS = options$rScale^2,
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
		method = options$samplingMethod,
		MCMC.iterations = MCMC.iterations,
		initprobs = initProbs,
		weights = wlsWeights,
		renormalize = TRUE
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
	models.ordered <- order(bas_obj$postprobs, decreasing = TRUE)
	if (options$bayesFactorOrder == "nullModelTop") {
		bestModelIndices <- models.ordered[1:nRows]
		if (1 %in% bestModelIndices) { # null model has index 1
			index <- which(bestModelIndices == 1)
			models.ordered <- c(1, bestModelIndices[-index]) # change position of null model
		} else {
			nRows <- nRows + 1 # show null model + best n
			models.ordered <- c(1, bestModelIndices)
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

	# calculate the BFM for the models
	nModels <- length(bas_obj$postprobs)
	postProbs <- bas_obj$postprobs
	priorProbs <- bas_obj$priorprobs
	index <- which(is.finite(postProbs))
	BFM <- sapply(1:nModels, function(m) {
		if (m %in% index) {
			i <- which(index == m)
			(postProbs[m] / (1 - postProbs[m])) / (priorProbs[m] / (1 - priorProbs[m]))
		} else {
			NA
		}
	})
	BFM <- BFM[models.ordered]
	
	# populate the row list
	output.rows <- vector("list", nRows)
	for (i in 1:nRows) {
		output.rows[[i]] <- list(
			"Models" = .clean(model.names[i]),
			"BF" = .clean(models.bf[i]),
			"BFM" = .clean(BFM[i]),
			"P(M|data)" = .clean(bas_obj$postprobs[[models.ordered[i]]]),
			"R2" = .clean(bas_obj$R2[[models.ordered[i]]]),
			"P(M)" = .clean(bas_obj$priorprobs[[models.ordered[i]]])
		)
	}

	# notes
	footnotes <- .newFootnotes()
	if (sum(nuisanceTerms) > 0) {
		footnote <- paste("All models include ", paste(.unvf(names(which(nuisanceTerms))), collapse = ", "), ".", sep = "")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = footnote)
	}

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
			list(name = "BFM", type = "number", format = "sf:4;dp:3", title = "BF<sub>M</sub>"),
			list(name = "BF", type = "number", format = "sf:4;dp:3", title = paste(bf.title, sep = "")),
			list(name = "R2", type = "number", format = "dp:3", title = "R\u00B2")
		)

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

	return(table)
}

.posteriorSummaryTable.basReg <- function(bas_obj, status, perform, options) {
	# Generate a posterior summary table of the coefficients
	#
	# Args:
	#   bas_obj: data read by .readData.basReg()
	#   status: current status of the analysis
	#   perform: 'run' or 'init'
	#   options: a list of user options
	#
	# Return:
	#   List with completed posterior table; may be inserted in results as is

	posterior <- list()

	posterior[["title"]] <- "Marginal Posterior Summaries of Coefficients"

	fields <-
		list(
			list(name="coefficient", title="Coefficient", type="string"),
			list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
			list(name="sd", title="SD", type="number", format="sf:4;dp:3"),
			list(name="pIncl", title ="P(incl|data)", type="number", format="sf:4;dp:3")
	)

	posterior[["schema"]] <- list(fields=fields)

	if (status$ready && perform == "run") {
		rows <- list()
		
		estimator <- switch(
			options$summaryType,
			best="HPM",
			median="MPM",
			"BMA"
		)

		coef <- BAS:::coef.bas(bas_obj, estimator=estimator)
		coefficients <- coef$namesx
		nModels <- coef$n.models
		
			for (i in 1:length(coefficients)) {
			
				coefficient <- .clean(coefficients[i])
				pIncl <- .clean(coef$probne0[i])
				if (options$summaryType == "complex") {
					mean <- .clean(unname(coef$conditionalmeans[nModels, i])) # most complex model is in the final row
					sd <- .clean(unname(coef$conditionalsd[nModels, i]))
				} else {
					mean <- .clean(coef$postmean[i])
					sd <- .clean(coef$postsd[i])
				}
				
				rows[[length(rows) + 1]] <- list(coefficient = coefficient, mean = mean, sd = sd, pIncl = pIncl)

			}
			posterior[["data"]] <- rows

		} else {
			names <- sapply(fields, function(x) x$name)
			posterior[["data"]][[1]] <- setNames(as.list(rep(".", length(names))), names)
		}

	return(posterior)
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

	return(descriptives)
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
		    # plotObj <- .plotImage.basReg(bas_obj) # to be implemented later
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

			    # the function BAS:::coef.bas calls list2matrix.bas which appears pretty broken
				plotObj <- .plotCoef.basReg(BAS:::coef.bas(bas_obj), subset = list(i))
				content <- .writeImage(width = 530, height = 400, plot = plotObj)

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
		"Log(P(data|M)) vs. model size",
		"Inclusion Probabilities"
	)

	# if there were errors or we're in init, show just empty x and y-axes
	emptyPlot <- .makeEmptyPlot.basReg(title = titles[which], status = status)

	if (status$ready && perform == "run") {

		plot <- list()
		plot[["title"]] <- titles[which]

		p <- try(silent = FALSE, expr = {

			plotObj <- .plotBas.basReg(bas_obj, which = c(which))
			content <- .writeImage(width = 530, height = 400, plot = plotObj)

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

.plotCoef.basReg <- function (x, e = 1e-04, subset = 1:x$n.vars, ...) {

	# based on BAS:::plot.coef.bas.

	# start of copied code
	df = x$df
	i <- subset[[1]]

	sel = x$conditionalmeans[, i] != 0
	prob0 = 1 - x$probne0[i]
	mixprobs = x$postprobs[sel]/(1 - prob0)
	means = x$conditionalmeans[sel, i, drop = TRUE]
	sds = x$conditionalsd[sel, i, drop = TRUE]
	name = x$namesx[i]
	df.sel = df[sel]

	df <- df.sel # modified from original

	nsteps = 500
	if (prob0 == 1 | length(means) == 0) {
		xlower = -0
		xupper = 0
		xmax = 1
	} else {
		qmin = min(qnorm(e/2, means, sds))
		qmax = max(qnorm(1 - e/2, means, sds))
		xlower = min(qmin, 0)
		xupper = max(0, qmax)
	}

	xx = seq(xlower, xupper, length.out = nsteps)
	yy = rep(0, times = length(xx))
	maxyy = 1
	if (prob0 < 1 & length(sds) > 0) {
		yy = mixprobs %*% apply(matrix(xx, ncol = 1), 1,
								FUN = function(x, d, m, s) {
									dt(x = (x - m)/s, df = d)/s
								}, d = df, m = means, s = sds)
		maxyy = max(yy)
	}
	ymax = max(prob0, 1 - prob0)
	# end of copied code

	dfLines <- data.frame(
		x = c(0, 0, xx),
		y = c(0, prob0, (1 - prob0) * yy/maxyy),
		g = factor(rep(1:2, c(2, length(xx))))
	)
	xBreaks <- pretty(range(xx), 3)

	g <- JASPgraphs::drawLines(dat = dfLines,
							   mapping = ggplot2::aes(x = x, y = y, group = g, color = g),
							   show.legend = FALSE) +
	    ggplot2::ylab("log(Marginal)") +
	    ggplot2::scale_x_continuous(name = name, breaks = xBreaks, limits = range(xBreaks))
	g <- g + ggplot2::scale_color_manual(values = c("gray", "black"))
	g <- JASPgraphs::themeJasp(g)

	return(g)
}

.plotBas.basReg <- function(x, which = c(which), id.n = 3,
							labels.id = NULL, sub.caption = NULL, ...) {
	# based on BAS::plot.bas

	# browser()
	show <- rep(FALSE, 4)
	show[which] <- TRUE
	iid <- 1:id.n
	if (show[1]) {
		yhat = fitted(x, estimator = "BMA")
		r = x$Y - yhat
		n <- length(r)
		if (id.n > 0) {
			if (is.null(labels.id))
				labels.id <- paste(1:n)
			show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
		}
	}
	# text.id <- function(x, y, ind, adj.x = TRUE) {
	#     labpos <- if (adj.x)
	#         label.pos[1 + as.numeric(x > mean(range(x)))]
	#     else 3
	#     text(x, y, labels.id[ind], cex = cex.id, xpd = TRUE,
	#          pos = labpos, offset = 0.25)
	# }
	# if (any(show[2:3])) {
	#     show.m = sort.list(x$logmarg, decreasing = TRUE)[iid]
	#     label.m = paste(1:x$n.models)
	# }
	if (is.null(sub.caption)) {
		cal <- x$call
		if (!is.na(m.f <- match("formula", names(cal)))) {
			cal <- cal[c(1, m.f)]
			names(cal)[2] <- ""
		}
		cc <- deparse(cal, 80)
		nc <- nchar(cc[1])
		abbr <- length(cc) > 1 || nc > 75
		if (abbr) {
			sub.caption <- paste(substr(cc[1], 1, min(75, nc)), "...")
		} else {
			sub.caption <- cc[1]
		}
	}
	# one.fig <- prod(par("mfcol")) == 1

	if (show[1]) {
		# browser()
		dfPoints <- data.frame(
			x = yhat,
			y = r
		)

		xBreaks <- JASPgraphs::getPrettyAxisBreaks(dfPoints[["x"]], 3)
		g <- JASPgraphs::drawAxis()
		g <- g + ggplot2::geom_hline(yintercept = 0, linetype = 2, col = "gray")
		g <- JASPgraphs::drawSmooth(g, dat = dfPoints, color = "red", alpha = .7)
		g <- JASPgraphs::drawPoints(g, dat = dfPoints, size = 2, alpha = .85) +
			ggplot2::ylab("Residuals") +
			ggplot2::scale_x_continuous(name = "Predictions under BMA", breaks = xBreaks, limits = range(xBreaks))
		g <- JASPgraphs::themeJasp(g)

		return(g)

	}
	if (show[2]) {
		# browser()
		cum.prob = cumsum(x$postprobs)
		m.index = 1:x$n.models

		dfPoints <- data.frame(
			x = m.index,
			y = cum.prob
		)

		xBreaks <- round(seq(1, x$n.models, length.out = min(5, x$n.models)))
		g <- JASPgraphs::drawSmooth(dat = dfPoints, color = "red", alpha = .7)
		g <- JASPgraphs::drawPoints(g, dat = dfPoints, size = 4) +
			ggplot2::ylab("Cumulative Probability") +
			ggplot2::scale_x_continuous(name = "Model Search Order", breaks = xBreaks)
		g <- JASPgraphs::themeJasp(g)
		return(g)
	}
	if (show[3]) {
		# browser()
		logmarg = x$logmarg
		dim = x$size

		dfPoints <- data.frame(
			x = dim,
			y = logmarg
		)

		# gonna assume here that dim (the number of parameters) is always an integer
		xBreaks <- unique(round(pretty(dim)))
		g <- JASPgraphs::drawPoints(dat = dfPoints, size = 4) +
			ggplot2::ylab("log(Marginal)") +
			ggplot2::xlab("Model Dimension") +
			ggplot2::scale_x_continuous(breaks = xBreaks)
		g <- JASPgraphs::themeJasp(g)
		return(g)

	}
	if (show[4]) {
		# browser()
		probne0 = x$probne0
		variables = x$namesx # 1:x$n.vars
		priorProb <- x$priorprobs[1:x$n.vars]

		# reorder from high to low
		o <- order(probne0, decreasing = FALSE)
		probne0 <- probne0[o]
		variables <- variables[o]
		priorProb <- priorProb[o]

		width <- .8 # width of the bars
		dfBar <- data.frame(
			x = factor(variables, levels = variables),
			y = probne0
		)
		dfLine <- data.frame(
			x = rep(1:x$n.vars, each = 2) + c(-width/2, width/2),
			y = rep(priorProb, each = 2),
			g = rep(factor(variables), each = 2)
		)

		g <- JASPgraphs::drawBars(dat = dfBar, width = width)
		g <- JASPgraphs::drawLines(g, dat = dfLine,
								   mapping = ggplot2::aes(x = x, y = y, group = g), linetype = 2) +
			ggplot2::ylab("Marginal Inclusion Probability") +
			ggplot2::xlab("")
		# ggplot2::xlab(paste0("Model Dimension\n", sub.caption))
		g <- JASPgraphs::themeJasp(g, horizontal = TRUE)
		return(g)

	}
}

.plotImage.basReg <- function(x, top.models = 20, intensity = TRUE, prob = TRUE,
							  log = TRUE, rotate = TRUE, color = "rainbow", subset = NULL,
							  offset = 0.75, digits = 3, vlas = 2, plas = 0, rlas = 0,
							  ...) {
	# code from BAS:::image.bas
	postprob = x$postprobs
	top.models = min(top.models, x$n.models)
	best = order(-x$postprobs)[1:top.models]
	postprob = postprob[best]/sum(postprob[best])
	which.mat <- BAS:::list2matrix.which(x, best)
	nvar <- ncol(which.mat)
	if (is.null(subset))
		subset = 1:nvar
	which.mat = which.mat[, subset, drop = FALSE]
	nvar = ncol(which.mat)
	namesx = x$namesx[subset]
	scale = postprob
	prob.lab = "Posterior Probability"
	if (log) {
		scale = log(postprob) - min(log(postprob))
		prob.lab = "Log Posterior Odds"
	}
	if (intensity)
		which.mat = sweep(which.mat, 1, scale + offset, "*")
	if (rotate)
		scale = rev(scale)
	if (prob)
		m.scale = cumsum(c(0, scale))
	else m.scale = seq(0, top.models)
	mat = (m.scale[-1] + m.scale[-(top.models + 1)])/2
	colors = switch(color,
					rainbow = c("black", rainbow(top.models +1, start = 0.75, end = 0.05)),
					blackandwhite = gray(seq(0, 1, length = top.models)))

	# end of code from BAS:::image.bas

	w <- diff(mat)
	w <- c(w[1], w)

	dfHeat <- data.frame(
		x = rep(rev(mat), ncol(which.mat)),
		y = rep(1:nvar, each = nrow(which.mat)),
		z = c(which.mat[, nvar:1]),
		zCat = 1* (abs(c(which.mat[, nvar:1])) > .Machine$double.eps),
		w = rev(w)
	)
	dfHeat$x <- dfHeat$x - dfHeat$w / 2
	# above line is required since width expands half of widht left and half of width right
	# check code below to verify
	# cbind(dfHeat$x - dfHeat$w / 2, dfHeat$x + dfHeat$w / 2)
	nr <- nrow(dfHeat)
	dfLines <- data.frame(
		x = rep(c(mat - w, mat[length(mat)]), each = 2),
		y = rep(c(.5, nvar+.5), length(mat)+1),
		g = factor(rep(1:(length(mat)+1), each = 2))
	)

	# browser()
	discrete <- TRUE
	if (discrete) {
		show.legend <- FALSE
		colors[colors != "black"] <- "green"
		colors[colors == "black"] <- "white"
		mapping = ggplot2::aes(x = x, y = y, fill = zCat, width = w)
	} else {
		show.legend <- TRUE
		mapping = ggplot2::aes(x = x, y = y, fill = z, width = w)
	}

	g <- JASPgraphs::drawHeatmap(dat = dfHeat, show.legend = show.legend, fillColor = colors,
								 mapping = mapping,
								 geom = "tile")
	g
	xBreaks <- mat - w/2
	g <- JASPgraphs::drawAxis(graph = g, xName = prob.lab, xBreaks = xBreaks, xLabels = round(scale, digits = digits),
				  yName = "", yBreaks = 1:nvar, yLabels = namesx, xLimits = NULL, yLimits = NULL,
				  secondaryXaxis = list(~.,name = "Model Rank", breaks = xBreaks, labels = top.models:1),
				  xTrans = scales::reverse_trans())
	g <- JASPgraphs::drawLines(g, dat = dfLines, mapping = ggplot2::aes(x = x, y = y, group = g),
				   color = "gray50", alpha = .7, size = 2)
	g <- JASPgraphs::themeJasp(graph = g, legend.position = "right", axisTickLength = 0,
							   bty = "o")

	# this plot needs some additional treatment
	unit <- JASPgraphs::graphOptions("axisTickLengthUnit")
	fillLg <- ggplot2::guide_colorbar(title = "", default.unit = unit,
									  barheight = 5, barwidth = 1)
	# browser()
	g <- g + ggplot2::theme(
		axis.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(0, 0, .5, 0, unit)),
		axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(.5, 0, 0, 0, unit))
	) + ggplot2::guides(fill = fillLg)

	return(g)

}

