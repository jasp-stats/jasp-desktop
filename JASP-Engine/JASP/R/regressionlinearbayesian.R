#
# Copyright (C) 2018 University of Amsterdam
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
	summaryOpts <- c("summaryType", "posteriorSummaryPlotCredibleIntervalValue", "nSimForCRI")
	stateKey <- list(
		bas_obj = modelOpts,
		postSummary = c(modelOpts, summaryOpts),
		postSummaryTable = c(modelOpts, summaryOpts, "postSummaryTable"),
		postSummaryPlot = c(modelOpts, summaryOpts, "postSummaryPlot", "omitIntercept"),
		descriptives = c("dependent", "covariates"),
		plotPosteriorLogOdds = c(modelOpts, "plotLogPosteriorOdds"),
		plotCoefficientsPosterior = c(modelOpts, "plotCoefficientsPosterior"),
		plotResidualsVsFitted = c(modelOpts, "plotResidualsVsFitted"),
		plotModelProbabilities = c(modelOpts, "plotModelProbabilities"),
		plotModelComplexity = c(modelOpts, "plotModelComplexity"),
		plotInclusionProbabilities = c(modelOpts, "plotInclusionProbabilities")
	)

	# Initialize the variables
	bas_obj <- state$bas_obj
	postSummary <- state[["postSummary"]] # otherwise postSummaryPlot could get retrieved instead of postSummary
	postSummaryTable <- state[["postSummaryTable"]]
	postSummaryPlot <- state$postSummaryPlot
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
				bas_obj$namesx[-1] <- .unvf(bas_obj$namesx[-1])
			}
		}

		# calculate summary information
		if (is.null(postSummary) && !status$error) {
			postSummary <- .calculatePosteriorSummary(bas_obj, dataset, options)
			
			# keep all relevant information in one object
			bas_obj[["posteriorSummary"]] <- postSummary
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

	if (options$postSummaryTable && is.null(postSummaryTable)) {
		postSummaryTable <- .posteriorSummaryTable.basReg(
			bas_obj, status, perform, options
		)
	}
	
	if (options$postSummaryPlot && is.null(postSummaryPlot)) {
	    postSummaryPlot <- .plotPosteriorSummary(
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
			bas_obj = bas_obj, status = status, perform = perform, samplingMethod = options[["samplingMethod"]]
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
	
	results[["posteriorSummary"]] <- list(
		title = "Posterior Summary",
		posteriorSummaryTable = postSummaryTable,
		posteriorSummaryPlot = postSummaryPlot
	)

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
			plotInclusionProbabilities$data,
			postSummaryPlot$data
		)

		state <- list(
			options = options,
			bas_obj = bas_obj,
			postSummary = postSummary,
			postSummaryTable = postSummaryTable,
			postSummaryPlot = postSummaryPlot,
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
	}

	if (!is.null(state))
		attr(state, "key") <- stateKey

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
	meta[[2]] <- list(
	    name = "posteriorSummary",
	    type = "object",
			meta = list(
				list(name = "posteriorSummaryTable", type = "table"),
	    	    list(name = "posteriorSummaryPlot", type = "image")
		)
	)
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
		dataset <- .readDataSetToEnd(columns = vars, exclude.na.listwise = vars)
	} else {
		dataset <- .readDataSetHeader(columns = vars)
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
	hasInteraction <- any(attr(stats::terms.formula(formula), "order") > 1)

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
	footnoteInteraction <- NULL
	pInteraction <- 0.5 # probability of model inclusion conditional on inclusion parents
	if (options$modelPrior == "beta.binomial") {
		modelPrior <- BAS::beta.binomial(options$betaBinomialParamA, options$betaBinomialParamB)
		if (hasInteraction) {
			footnoteInteraction <- paste("Prior model probabilities for models with interaction effects",
																	 "are obtained from a Bernoulli (p = 0.5) prior.",
																	 "We advice using a different model prior, or excluding interaction effects.")
		}
	} else if (options$modelPrior == "uniform") {
		modelPrior <- BAS::uniform()
	} else if (options$modelPrior == "Bernoulli") {
		modelPrior <- BAS::Bernoulli(options$bernoulliParam)
		pInteraction <- options$bernoulliParam
	}

	# number of models
	n.models <- NULL
	if (options$samplingMethod == "BAS" && options$numberOfModels > 0) {
		n.models <- options$numberOfModels
	}

	# iterations for MCMC
	MCMC.iterations <- NULL
	if (options$samplingMethod == "MCMC" && options$iterationsMCMC > 0) {
		MCMC.iterations <- options$iterationsMCMC
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
		if (bas_lm$n.models > 1 && nPreds > 1) # can crash without this check
			bas_lm <- BAS::force.heredity.bas(bas_lm)
		bas_lm[["interaction"]] <- list(
			hasInteraction = hasInteraction,
			footnote = footnoteInteraction,
			pInteraction = pInteraction
		)
		
		bas_lm[["nuisanceTerms"]] <- isNuisance
		# fix for prior probs all returning 1 with uniform and bernoulli 0.5 priors
		bas_lm[["priorprobs"]] <- bas_lm[["priorprobs"]] / sum(bas_lm[["priorprobs"]])
		bas_lm[["priorprobsPredictor"]] <- .calcPriorMarginalInclusionProbs(bas_lm)
		bas_lm[["formula"]] <- formula
		bas_lm[["weights"]] <- wlsWeights
		bas_lm[["BFinclusion"]] <- .calcInlusionBF(bas_lm)
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
	if (length(nuisanceTerms) != length(bas_obj$namesx) - 1) {
	# if there are categorical predictors

		nuisanceTerms2 <- logical(length(bas_obj$namesx) - 1)
		names(nuisanceTerms2) <- bas_obj$namesx[-1]
		for (i in which(nuisanceTerms[nuisanceTerms])) {

			idx <- grep(pattern = names(nuisanceTerms[i],
				x = names(nuisanceTerms2)[i]), fixed = TRUE)
			nuisanceTerms2[idx] <- TRUE
		}
		nuisanceTerms <- nuisanceTerms2

	} else {
		names(nuisanceTerms) <- .unvf(names(nuisanceTerms))
	}

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
		null.model <- paste("Null model (incl. ", paste(names(which(nuisanceTerms)), collapse = ", "), ")", sep = "")
	}

	# generate all model names
  allModelsVisited <- any(lengths(models) == 0) # analysis will chrash if TRUE
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
      model.names[i] <- paste(names(nonNuisance), collapse = " + ")
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
	index <- is.finite(postProbs)
	BFM <- numeric(length(postProbs))
	BFM[index] <- (postProbs[index] / (1 - postProbs[index])) / (priorProbs[index] / (1 - priorProbs[index]))
	BFM[!index] <- NA
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
	if (!is.null(bas_obj[["interaction"]][["footnote"]])) {
		footnote <- bas_obj[["interaction"]][["footnote"]]
		.addFootnote(footnotes, symbol = "<em>Warning.</em>", text = footnote)
	}
	if (sum(nuisanceTerms) > 0) {
		footnote <- paste("All models include ", paste(names(which(nuisanceTerms)), collapse = ", "), ".", sep = "")
		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = footnote)
	}
	# if (!allModelsVisited) {
	#     footnote <- "Not all models were visited during the sampling procedure."
	#     .addFootnote(footnotes, symbol = "<em>Note.</em>", text = footnote)
	# }

	return(list(rows=output.rows, notes=footnotes))
}

.calcPriorMarginalInclusionProbs <- function(bas_obj) {
	# Calculate the prior inclusions probabilities for each predictor
	#
	# Args:
	#   bas_obj: bas object (including nuisanceTerms entry)
	#
	# Return:
	#   vector of inclusion probabilities (including intercept)

	allModels <- bas_obj$which
	modelProbs <- bas_obj$priorprobs
	nPreds <- length(bas_obj$probne0)

	# model prior has been modified, recalculate the prior inclusion probs
	nModels <- length(allModels)
	priorProbs <- numeric(nPreds)

	for (i in 1:nModels) {

		idx <- allModels[[i]] + 1 # +1 to change 0 for intercept into a 1 so it can be used as an index
		priorProbs[idx] = priorProbs[idx] + modelProbs[i]

	}

	return(priorProbs)
}

.calcInlusionBF <- function(bas_obj) {

	nModels <- bas_obj[["n.models"]]
	nPred <- length(bas_obj[["probne0"]])
	
	# should this work on a log scale??
	# first row is numerator of the odds; second row is denominator
	priorOdds <- matrix(0, 2, nPred)
	posteriorOdds <- matrix(0, 2, nPred)
	for (i in seq_len(nModels)) {

		idxN <- bas_obj[["which"]][[i]] + 1
		idxD <- (1:nPred)[-idxN]
		
		# increment numerators
		priorOdds[1, idxN] <- priorOdds[1, idxN] + bas_obj[["priorprobs"]][i]
		posteriorOdds[1, idxN] <- posteriorOdds[1, idxN] + bas_obj[["postprobs"]][i]	
		
		# increment denominators
		priorOdds[2, idxD] <- priorOdds[2, idxD] + bas_obj[["priorprobs"]][i]
		posteriorOdds[2, idxD] <- posteriorOdds[2, idxD] + bas_obj[["postprobs"]][i]	
		
	}

	priOdds <- priorOdds[1, ] / priorOdds[2, ]
	posOdds <- posteriorOdds[1, ] / posteriorOdds[2, ]
	BFinclusion <- posOdds / priOdds

	# nuisance terms and intercept are always included
	BFinclusion[-1][bas_obj[["nuisanceTerms"]]] <- 1 # nuisance terms
	BFinclusion[1] <- 1 # intercept
	return(BFinclusion)
	
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
	#   dataset: the dataset
	#   status: current status of the analysis
	#   perform: 'run' or 'init'
	#   options: a list of user options
	#
	# Return:
	#   List with completed posterior table; may be inserted in results as is

	posterior <- list()

	posterior[["title"]] <- "Posterior Summaries of Coefficients"

	overTitle <- sprintf("%s%% Credible Interval", format(100*options[["posteriorSummaryPlotCredibleIntervalValue"]], digits = 3))
	fields <-
		list(
			list(name="coefficient", title="Coefficient", type="string"),
			list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
			list(name="sd", title="SD", type="number", format="sf:4;dp:3"),
			list(name="pInclprior", title ="P(incl)", type="number", format="sf:4;dp:3"),
			list(name="pIncl", title ="P(incl|data)", type="number", format="sf:4;dp:3"),
			list(name="BFincl", title ="BF<sub>inclusion</sub>", type="number", format="sf:4;dp:3"),
			list(name="lowerCri", title = "Lower", type="number", format="sf:4;dp:3", overTitle = overTitle),
			list(name="upperCri", title = "Upper", type="number", format="sf:4;dp:3", overTitle = overTitle)
	)

	posterior[["schema"]] <- list(fields=fields)

	if (status$ready && perform == "run") {
		rows <- list()

		BFinclusion <- bas_obj[["BFinclusion"]]
		coef <- bas_obj[["posteriorSummary"]][["coef"]]
		coefficients <- bas_obj[["posteriorSummary"]][["coefficients"]]
		probne0 <- bas_obj[["posteriorSummary"]][["probne0"]]
		priorProbs <- bas_obj[["priorprobsPredictor"]]
		loopIdx <- bas_obj[["posteriorSummary"]][["loopIdx"]]
		confInt <- bas_obj[["posteriorSummary"]][["conf95"]]

		nModels <- coef$n.models
		topm <- order(-bas_obj$postprobs)[1:nModels]
		mostComplex <- which.max(lengths(bas_obj$which)[topm])

			for (i in loopIdx) {

				coefficient <- .clean(coefficients[i])
				pIncl <- .clean(probne0[i])
				pInclprior <- .clean(priorProbs[i])
				BFincl <- .clean(BFinclusion[i])

				if (options$summaryType == "complex") {
					mean <- .clean(unname(coef$conditionalmeans[mostComplex, i]))
					sd <- .clean(unname(coef$conditionalsd[mostComplex, i]))
				} else {
					mean <- .clean(coef$postmean[i])
					sd <- .clean(coef$postsd[i])
				}
				lowerCri <- .clean(confInt[i, 1])
				upperCri <- .clean(confInt[i, 2])

				rows[[length(rows) + 1]] <- list(coefficient = coefficient, mean = mean, sd = sd, pIncl = pIncl,
												 pInclprior = pInclprior, BFincl = BFincl, lowerCri = lowerCri, upperCri = upperCri)

			}
			posterior[["data"]] <- rows
			
			footnotes <- .newFootnotes()
        	if (!is.null(bas_obj[["posteriorSummary"]][["footnotes"]])) {
        		footnote <- bas_obj[["posteriorSummary"]][["footnotes"]]
        		.addFootnote(footnotes, symbol = "<em>Warning.</em>", text = footnote)
        	}
			
			posterior[["footnotes"]] <- as.list(footnotes)

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


.plotLogOdds.basReg <- function(bas_obj, status, perform, samplingMethod) {
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
			plot[["width"]] <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "complete"

		})

		if (isTryError(p)) {
		    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
		    if (samplingMethod == "MCMC")
		        errorMessage <- "Cannot display Posterior Log Odds when method = MCMC."
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

		coefBas <- bas_obj[["posteriorSummary"]][["coefBMA"]]
		conf95 <- bas_obj[["posteriorSummary"]][["conf95BMA"]]
		for (i in 1:number.parameters) {

			terms <- .unvf(names(isNuisance))
			term <- bas_obj$namesx[i]
			if (term != "Intercept" && isNuisance[which(terms == term)]) {
				next
			}

			plot <- list()
			plot[["title"]] <- "Posterior distribution"

			p <- try(silent = FALSE, expr = {

				plotObj <- .plotCoef.basReg(coefBas, subset = list(i), conf95 = conf95)
				content <- .writeImage(width = 530, height = 400, plot = plotObj)

				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				plot[["width"]] <- 530
				plot[["height"]] <- 400
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

			if (c(which) != 4) {
				w <- 530
				h <- 400
			} else {
				w <- 700
				h <- 400
			}

			plotObj <- .plotBas.basReg(bas_obj, which = c(which))
			content <- .writeImage(width = w, height = h, plot = plotObj)

			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]
			plot[["width"]] <- w
			plot[["height"]] <- h
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
	plot[["width"]] <- 530
	plot[["height"]] <- 400
	plot[["status"]] <- "complete"

	if (status$error) {
		plot[["error"]] <- "badData"
	}

	return(plot)
}

.plotCoef.basReg <- function (x, e = 1e-04, subset = 1:x$n.vars, conf95 = NULL, ...) {

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
		if (i > 1) {
			xlower = min(qmin, 0)
			xupper = max(0, qmax)
		} else {
			xlower <- qmin
			xupper <- qmax
		}
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

	dens <- (1 - prob0) * yy/maxyy
	dfLines <- data.frame(
		x = c(0, 0, xx),
		y = c(0, prob0, dens),
		g = factor(rep(1:2, c(2, length(xx))))
	)
	
	xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(xlower, xupper))
	yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, 1.15*max(dfLines$y)))

	# figure out whether to draw text left or right of 0
	step <- (xupper + abs(xlower)) / (nsteps - 1)  # stepsize of grid
	idx0 <- round(abs(xlower / step))              # idx of x closest to 0
	idxMax <- which.max(dens)                      # idx of maximum of density
	maxX <- xx[idxMax]                             # x value at maximum of density
	maxHeight <- dens[idxMax]                        # y value at maximum of density
	if (prob0 > maxHeight) { # if text drawn above posterior no action is required
	    
	    xText <- 0.05 * xBreaks[length(xBreaks)]
	    hjust = "left"
	    # text below maxheight
	    
	} else {

	    # text is drawn right if:
	    # - density is below textheight
	    # - peak of density is left of textheight
	    
	    # text drawn at similar height as posterior
	    if (maxX < 0 && dens[idx0] < prob0) {
	        # peak is left of text; density is below text height
	        xText <- 0.05 * xBreaks[length(xBreaks)]
	        hjust = "left"
	        
	    } else {
	        
	        xText <- -abs(0.05 * xBreaks[1])
	        hjust = "right"
	        
	    }
	    
	}
	dfText <- data.frame(
	    x = xText,
	    y = prob0,
	    label = format(prob0, digits = 3, scientific = -2)
	)

    # obtain credible interval given that predictor is in model
	cri <- conf95[i, 1:2]
	# find closest x-locations on grid to credible interval
    idxCri <- c(
        which.min(abs(xx - cri[1])),
        which.min(abs(xx - cri[2]))
    )
    dfCri <- data.frame(
        xmin = xx[idxCri[1]],
        xmax = xx[idxCri[2]],
        y = 0.9 * yBreaks[length(yBreaks)]
    )
    hBarHeight <- 0.05 * yBreaks[length(yBreaks)]
    dfCriText <- data.frame(
        x = xx[idxCri],
        y = 0.975 * yBreaks[length(yBreaks)],
        label = format(cri, digits = 3, scientific = -2)
    )

  g <- ggplot2::ggplot(data = dfLines, mapping = ggplot2::aes(x = x, y = y, group = g, color = g)) +
    ggplot2::geom_line(size = 1.25, show.legend = FALSE) +
    ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, limits = range(yBreaks)) + 
	  ggplot2::scale_x_continuous(name = name, breaks = xBreaks, limits = range(xBreaks)) +
	  ggplot2::scale_color_manual(values = c("gray", "black"))
	if (prob0 > 0.01)
	    g <- g + ggplot2::geom_text(data = dfText, mapping = ggplot2::aes(x = x, y = y, label = label), 
	                                size = 6, hjust = hjust, inherit.aes = FALSE)
	g <- g + ggplot2::geom_errorbarh(data = dfCri, mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = y), 
	                          height = hBarHeight, inherit.aes = FALSE) + 
	  ggplot2::geom_text(data = dfCriText, mapping = ggplot2::aes(x = x, y = y, label = label), size = 6, 
	                     hjust = c("right", "left"), inherit.aes = FALSE)
	
	g <- JASPgraphs::themeJasp(g)

	return(g)
}

.plotBas.basReg <- function(x, which = c(which), id.n = 3,
							labels.id = NULL, sub.caption = NULL, ...) {
	# based on BAS::plot.bas

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
		dfPoints <- data.frame(
			x = yhat,
			y = r
		)

		xBreaks <- JASPgraphs::getPrettyAxisBreaks(dfPoints[["x"]], 3)
		g <- JASPgraphs::drawAxis()
		g <- g + ggplot2::geom_hline(yintercept = 0, linetype = 2, col = "gray")
		g <- JASPgraphs::drawPoints(g, dat = dfPoints, size = 2, alpha = .85)
		g <- JASPgraphs::drawSmooth(g, dat = dfPoints, color = "red", alpha = .7) +
			ggplot2::ylab("Residuals") +
			ggplot2::scale_x_continuous(name = "Predictions under BMA", breaks = xBreaks, limits = range(xBreaks))
		g <- JASPgraphs::themeJasp(g)

		return(g)

	}
	if (show[2]) {
		cum.prob = cumsum(x$postprobs)
		m.index = 1:x$n.models

		dfPoints <- data.frame(
			x = m.index,
			y = cum.prob
		)

		xBreaks <- round(seq(1, x$n.models, length.out = min(5, x$n.models)))
		g <- JASPgraphs::drawSmooth(dat = dfPoints, color = "red", alpha = .7)
		g <- JASPgraphs::drawPoints(g, dat = dfPoints, size = 4) +
		    ggplot2::scale_y_continuous(name = "Cumulative Probability", limits = 0:1) +
			ggplot2::scale_x_continuous(name = "Model Search Order", breaks = xBreaks)
		g <- JASPgraphs::themeJasp(g)
		return(g)
	}
	if (show[3]) {
		logmarg = x$logmarg
		dim = x$size

		dfPoints <- data.frame(
			x = dim,
			y = logmarg
		)

		# gonna assume here that dim (the number of parameters) is always an integer
		xBreaks <- unique(round(pretty(dim)))
		yBreaks <- JASPgraphs::getPrettyAxisBreaks(range(logmarg), eps.correct = 2)
		g <- JASPgraphs::drawPoints(dat = dfPoints, size = 4) +
			ggplot2::scale_y_continuous(name = "Log(P(data|M))", breaks = yBreaks, limits = range(yBreaks)) +
			ggplot2::scale_x_continuous(name = "Model Dimension", breaks = xBreaks)
		g <- JASPgraphs::themeJasp(g)
		return(g)

	}
	if (show[4]) {
		probne0 = x$probne0[-1]
		variables = x$namesx[-1] # 1:x$n.vars
		priorProb <- x$priorprobsPredictor[1:x$n.vars][-1]

		# reorder from low to high
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
			x = rep(1:(x$n.vars-1), each = 2) + c(-width/2, width/2),
			y = rep(priorProb, each = 2),
			g = rep(factor(variables), each = 2),
			g0 = factor(1)
		)
		base <- .1
		yLimits <- c(0, base * ceiling(max(c(priorProb, probne0)) / base))
		yBreaks <- seq(yLimits[1], yLimits[2], length.out = 5)

		g <- JASPgraphs::drawBars(dat = dfBar, width = width)
		g <- JASPgraphs::drawLines(g, dat = dfLine,
								   mapping = ggplot2::aes(x = x, y = y, group = g, linetype = g0), show.legend = TRUE) +
			ggplot2::scale_y_continuous("Marginal Inclusion Probability", breaks = yBreaks, limits = yLimits) +
			ggplot2::xlab("") +
			ggplot2::scale_linetype_manual(name = "", values = 2, labels = "Prior\nInclusion\nProbabilities")
		
		g <- JASPgraphs::themeJasp(g, horizontal = TRUE, legend.position = "right") +
			ggplot2::theme(
				legend.title = ggplot2::element_text(size = .8*JASPgraphs::graphOptions("fontsize"))
			)
		
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
	g <- g + ggplot2::theme(
		axis.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(0, 0, .5, 0, unit)),
		axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(.5, 0, 0, 0, unit))
	) + ggplot2::guides(fill = fillLg)

	return(g)

}

.plotPosteriorSummary <- function(bas_obj, status, perform, options) {
    
    # start markup
    title <- sprintf("Posterior Coefficients with %s%% Credible Interval", 
                     format(100*options$posteriorSummaryPlotCredibleIntervalValue, digits = 3))
    # do calculations
    if (status$ready && perform == "run") {

    	coef <- bas_obj[["posteriorSummary"]][["coef"]]
		confInt <- bas_obj[["posteriorSummary"]][["conf95"]]
        loopIdx <- bas_obj[["posteriorSummary"]][["loopIdx"]]
        coefficients <- bas_obj[["posteriorSummary"]][["coefficients"]]
        
        # exlude intercept if it's not the only predictor?
        if (options[["omitIntercept"]] && length(loopIdx) > 1)
        	loopIdx <- loopIdx[-1, drop = FALSE]
        
        confInt <- confInt[loopIdx, , drop = FALSE] # only plot parameters present in table
        df <- data.frame(
            x = factor(coefficients[loopIdx], levels = coefficients[loopIdx]),
            y = confInt[, 3],
            lower = confInt[, 1],
            upper = confInt[, 2]
        )

        yBreaks <- JASPgraphs::getPrettyAxisBreaks(range(c(confInt)), eps.correct = 2)
        g <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y, ymin = lower, ymax = upper)) + 
            ggplot2::geom_point(size = 4) + 
            ggplot2::geom_errorbar(, width = 0.2) + 
            ggplot2::scale_x_discrete(name = "") + 
            ggplot2::scale_y_continuous(name = expression(beta), breaks = yBreaks, limits = range(yBreaks))
        plotObj <- JASPgraphs::themeJasp(g) + 
            ggplot2::theme(
                axis.title.y = ggplot2::element_text(angle = 0, vjust = .5, size = 20)
            )

        w <- 530
        h <- 400
        content <- .writeImage(width = w, height = h, plot = plotObj)
        plot <- list(
            title = title,
            width = w,
            height = h,
            convertible = TRUE,
            status = "complete",
            obj = content[["obj"]],
            data = content[["png"]]
        )

    } else {
        plot <- .makeEmptyPlot.basReg(title = title, status = status)
    }
    
    return (plot)
    
}

.calculatePosteriorSummary <- function(bas_obj, dataset, options) {
	
	estimator <- switch(
		options$summaryType,
		best="HPM",
		median="MPM",
		"BMA"
	)

	# required for the marginal posterior plots
	# done here such that the information in the plots and tables always matches
	# if a user selects the same options. (The method uses approximations and otherwise decimals are off)
	footnote <- NULL
	if (!is.null(bas_obj[["posteriorSummary"]]) && 
		identical(bas_obj[["posteriorSummary"]][["nSimForCRI"]], options[["nSimForCRI"]])) {
		coefBMA <- bas_obj[["posteriorSummary"]][["coefBMA"]]
		conf95BMA <- bas_obj[["posteriorSummary"]][["conf95BMA"]]
	} else {
		# only need to recalculate this if bas_obj was remade (which implies bas_obj[["posteriorSummary"]] is NULL)
		coefBMA <- .coefBas(bas_obj, estimator = "BMA", dataset = dataset, weights = bas_obj[["weights"]])
		
		conf95BMA <- try(stats::confint(coefBMA, level = 0.95, nsim = options$nSimForCRI))
		if (isTryError(conf95BMA)) {
		    conf95BMA <- cbind(NA, NA, coefBMA$postmean)
		    rownames(conf95BMA) <- coefBMA$namesx
		    colnames(conf95BMA) <- c("2.5%", "97.5%", "beta")
		    conf95BMA[is.nan(conf95BMA)] <- NA
		    footnote <- "Parameters estimates and/ or credible intervals could not be calculated."
		}
	}

	# check if results of table and plots should match
	criVal <- options[["posteriorSummaryPlotCredibleIntervalValue"]]
	if (estimator == "BMA" && isTRUE(all.equal(criVal, 0.95))) { # what we show under Marginal Posterior distributions
		coef <- coefBMA
		conf95 <- conf95BMA
	} else {
		coef <- .coefBas(bas_obj, estimator = estimator, dataset = dataset, weights = bas_obj[["weights"]])
		conf95 <- stats::confint(coef, level = criVal, nsim = options$nSimForCRI)
	}

	probne0 <- coef[["probne0"]]
	coefficients <- bas_obj[["namesx"]]
	if (estimator == "HPM") {
		loopIdx <- which(abs(coef$postmean) > sqrt(.Machine$double.eps))
	} else if (estimator == "MPM") {
		loopIdx <- which(abs(coef$postmean) > sqrt(.Machine$double.eps))
		probne0 <- bas_obj[["probne0"]]
	} else {
		loopIdx <- seq_along(coefficients)
	}

	return(list(coef = coef, loopIdx = loopIdx, coefficients = coefficients, probne0 = probne0,
				conf95 = conf95, coefBMA = coefBMA, conf95BMA = conf95BMA, footnote = footnote,
				nSimForCRI = options[["nSimForCRI"]]))
}

.coefBas <- function (object, n.models, estimator = "BMA",
					  dataset, weights = NULL, ...) {

	# this function is an adaptation of BAS:::coef.bas
	# additional arguments:
	#
	# dataset
	# weights
	#
	# in addition, the formula object should be stored in the bas object.
	#
	# the original function evaluates things via eval(calls) constructions
	# JASP does not guarantree that this lookup structure works
	# so we need to modify this function.
	# this is only the case for the median model!
	
	# if there are future updates to the BAS package, this function can probably be removed
	# the code below is a small test for when an error happens.
	
	# data(UScrime, package = "MASS")
	# UScrime <- UScrime[, 1:5]
	# form <- M ~ So + Ed + Po1 + Po2
	# crime.bic =  BAS::bas.lm(
	#   formula = M ~ So + Ed + Po1 + Po2, # <-- toggle this one (works)
	# 	# formula = form,                  # <-- and this one    (errors)
	# 	data = UScrime, 
	# 	prior = "JZS",
	# 	initprobs = c(1, 0.5, 0.5, 0.5, 0.5),
	# 	renormalize = TRUE)
	# BAS:::coef.bas(crime.bic, estimator = "MPM") # <-- this function call will error
	
	# additionaly, the code previously failed (in JASP) for the correlation dataset (Big 5)
	# and selecting estimator = "MPM" (median model)
	# if neither of these errors occur in a future version then the original function can
	# probably be used again

	if (estimator == "MPM") {
		nvar = object$n.vars - 1
		bestmodel <- (0:nvar)[object$probne0 > 0.5]
		best = 1
		models <- rep(0, nvar + 1)
		models[bestmodel + 1] <- 1
		if (sum(models) > 1) {
			# this if statement is ugly but crucial
			if (is.null(weights)) {
				object <- BAS::bas.lm(formula = object$formula, data = dataset, 
									  weights = NULL,
									  n.models = 1, 
									  alpha = object$g, initprobs = object$probne0, 
									  prior = object$prior, modelprior = object$modelprior, 
									  update = NULL, bestmodel = models, prob.local = 0)
		
			} else {
				object <- BAS::bas.lm(formula = object$formula, data = dataset, 
									  weights = weights,
									  n.models = 1, 
									  alpha = object$g, initprobs = object$probne0, 
									  prior = object$prior, modelprior = object$modelprior, 
									  update = NULL, bestmodel = models, prob.local = 0)
			}
		}
	}
	postprobs = object$postprobs
	if (estimator == "MPM" | estimator == "HPM") 
		n.models = 1
	if (missing(n.models)) 
		n.models = length(postprobs)
	topm = order(-postprobs)[1:n.models]
	postprobs = postprobs[topm]/sum(postprobs[topm])
	shrinkage = object$shrinkage[topm]
	conditionalmeans = BAS:::list2matrix.bas(object, "mle")[topm, 
													  , drop = F]
	conditionalmeans[, -1] = sweep(conditionalmeans[, -1, drop = F], 
								   1, shrinkage, FUN = "*")
	postmean = as.vector(postprobs %*% conditionalmeans)
	conditionalsd = BAS:::list2matrix.bas(object, "mle.se")[topm, 
													  , drop = F]
	if (!(object$prior == "AIC" || object$prior == "BIC")) {
		conditionalsd[, -1] = sweep(conditionalsd[, -1, drop = F], 
									1, sqrt(shrinkage), FUN = "*")
	}
	postsd = sqrt(postprobs %*% conditionalsd^2 + postprobs %*% 
				  	((sweep(conditionalmeans, 2, postmean, FUN = "-"))^2))
	postsd = as.vector(postsd)
	if (is.null(object$df[topm])) {
		df = rep(object$n, length(postprobs))
		if (object$prior == "BIC" | object$prior == "AIC") {
			df = df - object$size
		}
		else {
			df = df - 1
		}
	}
	else df = object$df[topm]
	out = list(postmean = postmean, postsd = postsd, probne0 = object$probne0, 
			   conditionalmeans = conditionalmeans, conditionalsd = conditionalsd, 
			   namesx = object$namesx, postprobs = postprobs, n.vars = object$n.vars, 
			   n.models = n.models, df = df, estimator = estimator)
	class(out) = "coef.bas"
	return(out)
}
