#
# Copyright (C) 2013-2015 University of Amsterdam
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

.readBayesianRepeatedMeasuresDataOptions <- function (dataset = NULL, options = list (), perform = "init") {
	if (("" %in% options$repeatedMeasuresCells) == FALSE) {
		rm.vars <- options$repeatedMeasuresCells
		if (is.null (dataset)) {
			bs.factors <- options$betweenSubjectFactors
			bs.covariates <- options$covariates
			rm.factors <- options$repeatedMeasuresFactors
			all.variables <- c (bs.factors, bs.covariates, rm.vars)

			if (perform == "run") {
				dataset <- .readDataSetToEnd (columns.as.numeric = c (rm.vars, bs.covariates),
					columns.as.factor = bs.factors,
					exclude.na.listwise = all.variables)
				dataset <- .shortToLong (dataset, rm.factors, rm.vars, c (bs.factors, bs.covariates))
			} else {
				dataset <- data.frame (dependent = numeric (), subject = factor (levels = 2))
				if (length (rm.factors) > 0) {
					for(i in 1:length (rm.factors)) {
						dataset <- cbind (dataset, factor (levels = 2))
						names (dataset) [dim (dataset) [2]] <- .v (unlist (rm.factors [[i]]$name))
					}
				}
				if (length (bs.factors) > 0) {
					bs.factor.names <- .v (unlist (bs.factors))
					for (i in 1:length (bs.factors)) {
						dataset <- cbind (dataset, factor (levels = 2))
						names (dataset) [dim (dataset) [2]] <- bs.factor.names [i]
					}
				}
				if (length (bs.covariates) > 0) {
					bs.covariate.names <- .v (unlist (bs.covariates))
					for (i in 1:length (bs.covariates)) {
						dataset <- cbind (dataset, numeric ())
						names (dataset) [dim (dataset) [2]] <- bs.covariate.names [i]
					}
				}
			}

			options$dependent <- "dependent"
			options$randomFactors <- "subject"
			variable.names <- names (dataset)
			i <- which (variable.names == "dependent")
			j <- which (variable.names == "subject")
			variable.names [i] <- .v ("dependent")
			variable.names [j] <- .v ("subject")
			names (dataset) <- variable.names
			variable.names <- variable.names [-c (i,j)]

			if (length (variable.names) > 0)
				options$fixedFactors <- as.list (.unv (variable.names))

			options$modelTerms [[length (options$modelTerms) + 1]] <- list (components = "subject", isNuisance = TRUE)
		}

		return (list (dataset = dataset, options = options))
	} else {
		options$dependent <- ""
		return (list (dataset = NULL, options = options))
	}
}

.readBayesianRepeatedMeasuresShortData <- function (options = list (), perform = "init") {
	numeric.vars <- c (unlist (options$repeatedMeasuresCells), unlist (options$covariates))
	numeric.vars <- numeric.vars [numeric.vars != ""]
	
	factor.vars <- c (unlist (options$betweenSubjectFactors))
	factor.vars <- factor.vars [factor.vars != ""]

	if (perform == "run") {
		dataset <- .readDataSetToEnd (columns.as.numeric = numeric.vars, columns.as.factor = factor.vars, 
			exclude.na.listwise = c (numeric.vars, factor.vars))
	} else {
		dataset <- .readDataSetHeader (columns.as.numeric = numeric.vars, columns.as.factor = factor.vars)
	}
	
	return (dataset)
}

.readBayesianLinearModelData <- function (dataset = NULL, options = list (), perform = "init") {
	numeric.vars <- c (unlist (options$covariates), unlist (options$dependent))
	numeric.vars <- numeric.vars [numeric.vars != ""]

	factor.vars <- c (unlist (options$fixedFactors), unlist (options$randomFactors))
	factor.vars <- factor.vars [factor.vars != ""]

	if (is.null (dataset)) {
		if (perform == "run") {
			dataset <- .readDataSetToEnd (columns.as.numeric = numeric.vars, columns.as.factor = factor.vars,
				exclude.na.listwise = c (numeric.vars, factor.vars))
		} else {
			dataset <- .readDataSetHeader (columns.as.numeric = numeric.vars, columns.as.factor = factor.vars)
		}
	}

	return (dataset)
}

.setBayesianLinearModelStatus <- function (dataset = NULL, options = list (), perform = "init") {
	if ((options$dependent == "") || (length (options$modelTerms) == 0))
		error.message <- NULL

	if (!exists ("error.message") && perform == "run") {
		variable.names <- NULL
		for (covariate in options$covariates) {
			if (any (!is.finite (dataset [[.v (covariate)]])))
				variable.names <- c (variable.names, covariate)
		}
		if ( any (!is.finite (dataset [[.v (options$dependent)]])))
			variable.names <- c (variable.names, options$dependent)
		if ( !is.null (variable.names))
			error.message <- paste ("Bayes factor is undefined -- the variable(s) ", variable.names, " contain(s) infinity", sep = "")
	}

	if (!exists ("error.message") && perform == "run") {
		factor.names <- NULL
		for (fact in options$fixedFactors) {
			if (length (unique (dataset[[.v (fact)]])) < 2) {
				factor.names <- c (factor.names, fact)
				error.message <- paste ("Bayes factor is undefined -- the factor(s) ",
					paste (factor.names, collapse = ", "),
					" contain(s) less than two levels (possibly only after rows with missing values are excluded)", sep = "")
			}
		}
	}

	if (!exists ("error.message")) {
		nuisance.terms <- sapply (options$modelTerms, function (term) {
			term$isNuisance
		})
		if (sum (nuisance.terms) == length (options$modelTerms))
			error.message <- "Bayes factor is undefined -- all effects are specified as nuisance"
	}

	if (!exists ("error.message")
		&& perform == "run"
		&& (length (dataset[[.v(options$dependent)]]) <= (1 + length (options$modelTerms))))
		error.message <- "Bayes factor is undefined -- too few observations (possibly only after rows with missing values are excluded)"
	
	if (!exists ("error.message")) {
		max.no.components <- max (sapply (options$modelTerms, function(term){length (term$components)}))
		if (max.no.components > 1) {
			for (term in options$modelTerms) {
				if (exists ("error.message"))
					break		
				components <- term$components
				if (length (components) > 1) {
					no.children <- 2^length (components) - 1
					inclusion <- sapply (options$modelTerms, function (terms) {
							term.components <- terms$components
							if (sum (term.components %in% components) == length (term.components)) {
								return (TRUE)
							}
							return (FALSE)
						})
					if (sum (inclusion) != no.children)
						error.message <- "Main effects and lower-order interactions must be included whenever the corresponding higher-order interaction is included"
				}
			}
		}
	}

	if (!exists ("error.message")) {
		for (term in options$modelTerms) {
			if (exists ("error.message"))
				break
			if (term$isNuisance) {
				components <- term$components
				withmain.conflict <- sapply (options$modelTerms, function (terms) {
					if (!terms$isNuisance) {
						term.components <- terms$components
						if (sum (term.components %in% components) == length (term.components)) {
							return (TRUE)
						}
					}
					return (FALSE)
				})
				if (any (withmain.conflict))
					error.message <- "Main effects and lower-order interactions must be specified as nuisance whenever the corresponding higher-order interaction is specified as nuisance"
			}
		}
	}

	if (!exists ("error.message"))
		return (list (ready = TRUE, error = FALSE, error.message = NULL))
	if (is.null(error.message))
		return (list (ready = FALSE, error = FALSE, error.message = NULL))
	if (!is.null(error.message))
		return (list (ready = FALSE, error = TRUE, error.message = error.message))
}

.updateResultsBayesianLinearModels <- function (results, model.object, effects.matrix, interactions.matrix, neverExclude, null.model, options, status) {
	results [["model comparison"]] <- .theBayesianLinearModelsComparison (
		list (
			models = model.object, 
			effects = effects.matrix,
			interactions.matrix = interactions.matrix, 
			nuisance = neverExclude,
			null.model = null.model), 
		options, perform = "run", status, populate = TRUE)$modelTable

	results [["effects"]] <- .theBayesianLinearModelsEffects (
		list (
			models = model.object, 
			effects = effects.matrix,
			interactions.matrix = interactions.matrix, 
			nuisance = neverExclude,
			null.model = null.model), 
		options, perform = "run", status, populate = TRUE)
	
	if (options$posteriorEstimates) {
		results [["effects"]] <- .theBayesianLinearModelEstimates (
			list (
				models = model.object, 
				effects = effects.matrix,
				interactions.matrix = interactions.matrix, 
				nuisance = neverExclude,
				null.model = null.model), 
			options, perform = "init", status, populate = TRUE)
	}
	
	return(results)
}

.theBayesianLinearModels <- function (dataset = NULL, options = list (), perform = "init",
									  status = list (), .callbackBayesianLinearModels,
									  .callbackBFpackage, results = list(),
									  analysisType = "") {
	
	# priors
	rscaleFixed <- "medium"     # 1/2
	rscaleRandom <- "nuisance"  # 1
	rscaleCont <- "medium"      # sqrt(2)/4
	
	# MCMC iterations
	if (analysisType != "Regression") {
		if (options$sampleMode == "auto") {
			iter <- 10000
		} else if (options$sampleMode == "manual") {
			iter <- options$fixedSamplesNumber
		}
	} else {
		iter <- NA
	}
	
	if (analysisType == "ANOVA") {
		rscaleFixed <- options$priorFixedEffects
		rscaleRandom <- options$priorRandomEffects
	} else if (analysisType == "ANCOVA" || analysisType == "RM-ANOVA") {
		rscaleFixed <- options$priorFixedEffects
		rscaleRandom <- options$priorRandomEffects
		rscaleCont <- options$priorCovariates
	} else if (analysisType == "Regression") {
		rscaleCont <- options$priorCovariates
	}
	
	if (! status$ready && ! status$error)
		return (list (model =  list (models = NULL, effects = NULL), status = status))

	#Extract the model components and nuisance terms
	model.formula <- paste (.v (options$dependent), " ~ ", sep = "")
	neverExclude <- NULL
	effects <- NULL
	for (term in options$modelTerms) {
		if (is.null (effects) & is.null (neverExclude)){
			model.formula <- paste (model.formula,
				paste (.v (term$components), collapse = ":"), sep = "")
		} else {
			model.formula <- paste (model.formula, " + ",
				paste (.v (term$components), collapse = ":"), sep = "")
		}
		if (term$isNuisance) {
			neverExclude <- c (neverExclude, paste (.v (term$components), collapse = ":"))
		} else {
			effects <- c (effects, paste (.v (term$components), collapse = ":"))
		}
	}
	model.formula <- formula (model.formula)

	#Intermediate Callback
	response <- .callbackBayesianLinearModels ()
	if (response$status != "ok") {
		return ()
	} else {
		if ( ! is.null (response$options))
			options <- response$options
	}

	#Make a list of models to compare
	model.list <- try (BayesFactor::enumerateGeneralModels (model.formula, 
		whichModels = "withmain", neverExclude = paste ("^", neverExclude, "$", sep = "")), 
		silent = TRUE)
	if (class (model.list) == "try-error") {
		if (! status$error) {
			status$ready <- FALSE
			status$error <- TRUE
			status$error.message <- "An unknown error occured. Please contact the authors."
			return (list (model =  list (models = NULL, effects = NULL, interactions.matrix = NULL, 
				nuisance = neverExclude, null.model = NULL), status = status))
		}
		model.list <- list (model.formula)
	}

	#Intermediate Callback
	response <- .callbackBayesianLinearModels ()
	if (response$status != "ok") {
		return ()
	} else {
		if ( ! is.null (response$options))
			options <- response$options
	}

	#Run Null model
	null.model <- list ()
	if (!is.null (neverExclude)) {
		for (m in 1:length (model.list)){
			model.title <- base::strsplit(x = as.character (model.list [[m]]) [[3]], 
				split = "+", fixed = TRUE) [[1]]
			model.title <- stringr::str_trim (model.title)
			model.title <- model.title [model.title != ""]
			if (sum (!(model.title %in% neverExclude)) == 0) break
		}
		null.formula <- model.list [[m]]
		model.list <- model.list [-m]

		if (perform == "run" && status$ready) {
			bf <- try (BayesFactor::lmBF (null.formula,
				data = dataset, whichRandom = .v (unlist (options$randomFactors)),
				progress = FALSE, posterior = FALSE, callback = .callbackBFpackage,
				rscaleFixed = rscaleFixed, rscaleRandom = rscaleRandom, rscaleCont = rscaleCont,
				iterations = iter))
			
			null.model$bf <- bf
			
			if (inherits (bf, "try-error")) {
				message <- .extractErrorMessage (bf)
				if (message == "Operation cancelled by callback function.")
					return()
				status$ready <- FALSE
				status$error <- TRUE
				status$error.message <- "Bayes factor is undefined -- the null model could not be computed"
			}
		}
	}

	#Run all other models and store the results in the model.object
	no.effects <- length (effects)
	no.models <- length (model.list)
	if (no.models > 0 && no.effects > 0) {
		effects.matrix <- matrix (FALSE, nrow = no.models, ncol = no.effects)
		colnames (effects.matrix) <- effects
		rownames (effects.matrix) <- paste ("Model", 1:no.models)
		effects <- stringr::str_trim (effects)

		interactions.matrix <- matrix (FALSE, nrow = no.effects, ncol = no.effects)
		rownames (interactions.matrix) = colnames (interactions.matrix) <- effects
		if (no.effects > 1){
			effect.components <- sapply (effects, function (effect) {
				base::strsplit (effect, split = ":", fixed = TRUE)
				})
				for (e in 1:no.effects){
					interactions.matrix [e, ] <- sapply (1:no.effects, function(ee) {
						(sum (effect.components [[e]] %in% effect.components [[ee]]) == length (effect.components [[e]]))
					})
				}
			diag (interactions.matrix) <- FALSE
		}

		model.object <- list()
		for (m in 1:no.models) {
			model.object [[m]] <- list ("ready" = TRUE)
			model.effects <- base::strsplit (x = as.character (model.list [[m]]) [[3]], 
				split = "+", fixed = TRUE) [[1]]
			model.effects <- stringr::str_trim (model.effects)

			if (no.effects > 1) {
				for (effect in model.effects) {
					components <- base::strsplit (effect, split = ":", fixed = TRUE) [[1]]
					inclusion <- sapply (effect.components, function (effect.component) {
						if (length (components) != length (effect.component)) {
							return (FALSE)
						} else {
							if (sum (components %in% effect.component) == length (components)) {
								return (TRUE)
							} else {
								return (FALSE)
							}
						}
					})
					effects.matrix[m, which (inclusion == TRUE)] <- TRUE
				}
			} else {
				effects.matrix [1,1] <- TRUE
			}

			model.title <- base::strsplit (x = as.character (model.list [[m]]) [[3]], 
				split = "+", fixed = TRUE) [[1]]
			model.title <- stringr::str_trim (model.title)
			model.title <- model.title [model.title != ""]
			model.title <- model.title [!(model.title %in% neverExclude)]
			model.object [[m]]$title <- .unvf (paste (model.title, collapse = " + "))
			
			model.object [[m]]$ready <- FALSE #to ensure that intermediate results can be called
		}
		
		#Create empty tables
		results <- .updateResultsBayesianLinearModels (results, model.object, 
			effects.matrix, interactions.matrix, neverExclude, null.model, options, status)

		#Intermediate Callback
		response <- .callbackBayesianLinearModels (results)
		if (response$status != "ok") {
			return ()
		} else {
			if ( ! is.null (response$options))
				options <- response$options
		}

		#Now compute Bayes Factors for each model in the list, and populate the tables accordingly
		for(m in 1:no.models) {
			if (perform == "run" && status$ready) {
				bf <- try (BayesFactor::lmBF (model.list [[m]],
					data = dataset, whichRandom = .v (unlist (options$randomFactors)),
					progress = FALSE, posterior = FALSE, callback = .callbackBFpackage,
					rscaleFixed = rscaleFixed, rscaleRandom = rscaleRandom, rscaleCont = rscaleCont,
					iterations = iter))
				model.object [[m]]$bf <- bf

				if (inherits (bf, "try-error")) {
					message <- .extractErrorMessage (bf)
					if (message == "Operation cancelled by callback function.")
						return()
					model.object [[m]]$error.message <- "Bayes factor could not be computed"
				} else {
					model.object [[m]]$ready <- TRUE
				}

				if (length (neverExclude) > 0){
					model.object [[m]]$bf <- model.object [[m]]$bf / null.model$bf
				}
			}
			
			#Create empty tables
			results <- .updateResultsBayesianLinearModels (results, model.object, 
				effects.matrix, interactions.matrix, neverExclude, null.model, options, status)

			#Intermediate Callback
			response <- .callbackBayesianLinearModels (results)
			if (response$status != "ok") {
				return ()
			} else {
				if ( ! is.null (response$options))
					options <- response$options
			}
		}

		if (options$posteriorEstimates && perform == "run" && status$ready) {
			complexity <- rowSums (effects.matrix)
			m <- which (complexity == max (complexity))
			chains <- try (BayesFactor::lmBF (model.list [[m]],
					data = dataset, whichRandom = .v (unlist (options$randomFactors)),
					progress = FALSE, posterior = TRUE, callback = .callbackBFpackage, 
					iterations = options$posteriorEstimatesMCMCIterations,
					rscaleFixed = rscaleFixed, rscaleRandom = rscaleRandom, rscaleCont = rscaleCont,
					iterations = iter))
			
			if (inherits (bf, "try-error")) {
				message <- .extractErrorMessage (bf)
				if (message == "Operation cancelled by callback function.")
					return()
				model.object [[m]]$error.message <- "Posterior could not be computed"
				return (list (model =  list (models = model.object, effects = effects.matrix,
					interactions.matrix = interactions.matrix, nuisance = neverExclude,
					null.model = null.model), status = status))
			} 		
			
			model.object [[m]]$chains <- chains
			return (list (model =  list (models = model.object, effects = effects.matrix,
				interactions.matrix = interactions.matrix, nuisance = neverExclude,
				null.model = null.model), status = status))
		}
		return (list (model = list (models = model.object, effects = effects.matrix,
			interactions.matrix = interactions.matrix, nuisance = neverExclude,
			null.model = null.model), status = status))
	} 

	if (! status$error) {
			status$ready <- FALSE
			status$error <- TRUE
			status$error.message <- "An unknown error occured. Please contact the authors."
	}
	return (list (model =  list (models = NULL, effects = NULL, interactions.matrix = NULL, 
		nuisance = neverExclude, null.model = NULL), status = status))
}

.theBayesianLinearModelsComparison <- function (model = NULL, options = list (), perform = "init", status = list (), populate = TRUE) {
	modelTable <- list ()
	modelTable [["title"]] <- "Model Comparison"
	modelTable [["citation"]] <-
		list (
			"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
			"Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012). Default Bayes factors for ANOVA designs. Journal of Mathematical Psychology, 56, 356-374."
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

	fields <-
		list (
			list (name = "Models", type = "string"),
			list (name = "P(M)", type = "number", format = "sf:4;dp:3"),
			list (name = "P(M|data)", type = "number", format = "sf:4;dp:3;log10"),
			list (name = "BFM", type = "number", format = "sf:4;dp:3;log10", title = paste (bfm.title, sep = "")),
			list (name = "BF10", type = "number", format = "sf:4;dp:3;log10", title = paste (bf.title, sep = "")),
			list (name = "error %", type="number", format="sf:4;dp:3")
		)
	if (options$bayesFactorType == "LogBF10") {
		fields[[4]] <- list (name = "BFM", type = "number", format = "sf:4;dp:3", title = paste (bfm.title, sep = ""))
		fields[[5]] <- list (name = "BF10", type = "number", format = "sf:4;dp:3", title = paste (bf.title, sep = ""))
	}
	modelTable [["schema"]] <- list (fields = fields)

	if (! status$ready && ! status$error)
		return (list (modelTable = modelTable, model = model))

	## Footnotes
	footnotes <- .newFootnotes()
	null.model <- "Null model"
	if (length (model$nuisance) > 0) {
			null.model <- paste ("Null model (incl. ", paste (.unvf (model$nuisance), collapse = ", "), ")", sep = "")
			footnote <- paste ("All models include ", paste (.unvf (model$nuisance), collapse = ", "), ".", sep = "")
			.addFootnote (footnotes, symbol = "<em>Note.</em>", text = footnote)
	}

	## Data
	no.models <- length (model$models)
	bayes.factors <- numerical.error <- model.names <- rep (NA, no.models + 1)

	rows <- list ()

	rows[[1]] <- list ("Models" = null.model)

	bayes.factors [1] <- 0
	numerical.error[1] <- 0
	if (no.models > 0) {
		for (m in 1:no.models) {
			rows[[m + 1]] <- list ("Models" = model$models [[m]]$title)
			if (perform == "run" && status$ready && model$models [[m]]$ready) {
				bayes.factors [m + 1] <- model$models [[m]]$bf@bayesFactor$bf
				numerical.error [m + 1] <- model$models [[m]]$bf@bayesFactor$error
			}
		}
	}

	## Populate table
	if (perform == "run" && status$ready) {
		
		bestModelTop <- ! is.null(options$bayesFactorOrder) && options$bayesFactorOrder == "bestModelTop" && populate == FALSE
		if (bestModelTop) {
			ordering <- order(bayes.factors, decreasing=TRUE)	
			bestModelIndex <- ordering[1] - 1 # The null model is not in the model list, so shift by 1
			if (bestModelIndex != 0) {
				bestModel <- model$models[[bestModelIndex]]$bf
				bayes.factors[1] <- 0 - bestModel@bayesFactor$bf
				numerical.error[1] <- bestModel@bayesFactor$error
				for (m in 1:no.models) {
					if (model$models[[m]]$ready) {
						newBF <- model$models[[m]]$bf / bestModel
						bayes.factors[m + 1] <- newBF@bayesFactor$bf
						numerical.error[m + 1] <- newBF@bayesFactor$error 
					}
				}
			}
		}
		
		if (populate == FALSE) {
			#This is set for intermediate populating of tables
			ln.prob <- rep (NA, no.models + 1)
			index <- which (!is.na (bayes.factors) & is.finite (bayes.factors))
			maxLBF <- max (bayes.factors [index])
			ln.prob [index] <- bayes.factors [index] - maxLBF
			ln.prob [index] <- ln.prob [index] - log (sum (exp (ln.prob [index])))
			ln.BF.model <- sapply (1:(no.models+1),function(m) {
				if (m %in% index) {
					i <- which (index == m)
					bayes.factors [m] - maxLBF - log (sum (exp (bayes.factors[index] [-i] - maxLBF))) + log (no.models)
				} else {
					NA
				}
			})
			model$effects <- cbind (model$effects, 1 / (no.models + 1), exp (ln.prob [-1]))

			rows [[1]] [["P(M)"]] <- .clean (1 / (no.models + 1))
			rows [[1]] [["P(M|data)"]] <- .clean (ln.prob [1] / log (10))
		}#end populate == FALSE
		
		if (populate == FALSE) {
			if (options$bayesFactorType == "LogBF10") { 
				rows [[1]] [["BFM"]] <- .clean (ln.BF.model [1])
			} else {
				rows [[1]] [["BFM"]] <- .clean (ln.BF.model [1] / log (10))
			}
		}
		rows [[1]] [["BF10"]] <- 0

		if (bestModelTop && bestModelIndex != 0) {
			rows[[1]][["error %"]] = .clean(100 * numerical.error[1])
			if (options$bayesFactorType == "LogBF10") {
				rows[[1]][["BF10"]] <- .clean(bayes.factors[1])
			} else if (options$bayesFactorType == "BF10") {
				rows[[1]][["BF10"]] <- .clean(bayes.factors[1] / log(10))
			} else {
				rows[[1]][["BF10"]] <- .clean(- bayes.factors[1] / log(10))
			}
		}

		for (m in 1:no.models) {
			if (model$models [[m]]$ready) {
				if (populate == FALSE) { 
					#This is set for intermediate populating of tables
					rows [[m+1]] [["P(M)"]] <- .clean (1 / (no.models + 1))
					rows [[m+1]] [["P(M|data)"]] <- .clean (ln.prob [m + 1] / log (10))
				}#end populate == FALSE
				if (options$bayesFactorType == "LogBF10") {
					if (populate == FALSE)
						rows [[m+1]] [["BFM"]] <- .clean (ln.BF.model [m + 1])
					rows [[m+1]] [["BF10"]] <- .clean (bayes.factors [m + 1])
				} else{
					if (populate == FALSE)
						rows [[m+1]] [["BFM"]] <- .clean (ln.BF.model [m + 1] / log (10))
					if (options$bayesFactorType == "BF10") {
						rows [[m+1]] [["BF10"]] <- .clean (bayes.factors [m + 1] / log (10))
					} else {
						rows [[m+1]] [["BF10"]] <- .clean (- bayes.factors [m + 1] / log (10))
					}
				}
				rows [[m+1]] [["error %"]] <- .clean (100*numerical.error [m + 1])
			} else {
				if (populate == FALSE) {
					index <- .addFootnote (footnotes, text = model$models [[m]]$error.message)
					rows [[m+1]] [["BF10"]] <- .clean(NaN)
					rows [[m+1]] [[".footnotes"]] <- list ("BFM" = list (index))
				}
			}
		}

		if (bestModelTop) {
			rows <- rows[ordering]
		}

	  rows[[1]][["error %"]] <- ""

	}
	
	if (is.null(status$analysis.type) || status$analysis.type != "rmANOVA") {
		modelTable [["title"]] <- paste ("Model Comparison - ", options$dependent, sep = "")
	}
	modelTable [["data"]] <- rows
	modelTable [["footnotes"]] <- as.list (footnotes)

	if (status$error)
		modelTable [["error"]] <- list (errorType = "badData", errorMessage = status$error.message)
	
	return (list (modelTable = modelTable, model = model))
}

.theBayesianLinearModelsEffects <- function (model = NULL, options = list (), perform = "init", status = list (), populate = TRUE) {

	if ( ! options$effects)
		return (NULL)

	effectsTable <- list ()
	effectsTable [["title"]] <- "Analysis of Effects"
	effectsTable [["citation"]] <-
		list (
			"Morey, R. D. & Rouder, J. N. (2015). BayesFactor (Version 0.9.10-2)[Computer software].",
			"Rouder, J. N., Morey, R. D., Speckman, P. L., Province, J. M., (2012) Default Bayes Factors for ANOVA Designs. Journal of Mathematical Psychology. 56. p. 356-374."
		)

	if (options$bayesFactorType == "LogBF10"){
		inclusion.title <- "Log(BF<sub>Inclusion</sub>)"
		forward.title <- "Log(BF<sub>Forward</sub>)"
		backward.title <- "Log(BF<sub>Backward</sub>)"
	} else {
		inclusion.title <- "BF<sub>Inclusion</sub>"
		forward.title <- "BF<sub>Forward</sub>"
		backward.title <- "BF<sub>Backward</sub>"
	}

	if (options$effectsStepwise) {
		fields <-
			list (
				list (name = "Effects", type = "string"),
				list (name = "P(incl)", type = "number", format = "sf:4;dp:3"),
				list (name = "P(incl|data)", type = "number", format = "sf:4;dp:3"),
				list (name = "BF<sub>Inclusion</sub>", type="number", format = "sf:4;dp:3", 
					title = paste (inclusion.title, sep = "")),
				list (name = "BF<sub>Backward</sub>", type="number", format = "sf:4;dp:3", 
					title = paste (backward.title, sep = "")),
				list (name = "% errorB", type = "number", format = "sf:4;dp:3"),
				list (name = "BF<sub>Forward</sub>", type = "number", format = "sf:4;dp:3", 
					title = paste (forward.title, sep = "")),
				list (name = "% errorF", type = "number", format = "sf:4;dp:3")
			)
	} else{
		fields <-
			list (
				list (name = "Effects", type = "string"),
				list (name = "P(incl)", type = "number", format = "sf:4;dp:3"),
				list (name = "P(incl|data)", type = "number", format = "sf:4;dp:3"),
				list (name = "BF<sub>Inclusion</sub>", type = "number", format = "sf:4;dp:3", 
					title = paste (inclusion.title, sep = ""))
			)
	}

	effectsTable [["schema"]] <- list (fields = fields)

	if (! status$ready && ! status$error)
		return (effectsTable)

	effects.matrix <- model$effects
	if (perform == "run" && status$ready && !populate) {
		prior.probabilities <- model$effects [, ncol (effects.matrix) - 1]
		posterior.probabilities <- model$effects [, ncol (effects.matrix)]
		effects.matrix <- matrix (model$effects [1:nrow (model$effects), 1:(ncol (model$effects) - 2)],
			nrow = nrow (model$effects),
			ncol = ncol (model$effects) - 2)

		effectNames <- colnames (effects.matrix) <- colnames (model$effects) [1:(ncol (model$effects) - 2)]
		no.models <- nrow (effects.matrix)
		no.effects <- ncol (effects.matrix)

		dim (prior.probabilities) <- c (1, no.models)
		dim (posterior.probabilities) <- c (1, no.models)
		prior.inclusion.probabilities <- prior.probabilities %*% effects.matrix
		posterior.inclusion.probabilities <- posterior.probabilities %*% effects.matrix
		posterior.inclusion.probabilities[posterior.inclusion.probabilities > 1] <- 1
		posterior.inclusion.probabilities[posterior.inclusion.probabilities < 0] <- 0
		bayes.factor.inclusion <- (posterior.inclusion.probabilities / (1 - posterior.inclusion.probabilities)) /
			(prior.inclusion.probabilities / (1 - prior.inclusion.probabilities))
		model.complexity <- rowSums (effects.matrix)
	}

	no.effects <- ncol (effects.matrix)
	effectNames <- colnames (effects.matrix)
	if (!is.null (no.effects) && no.effects > 0) {
		rows <- list ()
		for (e in 1:no.effects) {
			row <- list ()
			row$"Effects" <- .unvf (effectNames [e])
			if (perform == "run" && status$ready && !populate) {
				row$"P(incl)" = .clean (prior.inclusion.probabilities [e])
				row$"P(incl|data)" = .clean (posterior.inclusion.probabilities [e])
				if (options$bayesFactorType == "LogBF10"){
					row$"BF<sub>Inclusion</sub>" = .clean (log (bayes.factor.inclusion [e]))
				} else {
					row$"BF<sub>Inclusion</sub>" = .clean (bayes.factor.inclusion [e])
				}

				if (options$effectsStepwise && no.effects > 1) {
					#Forward
					include <- which (effects.matrix[, e] == TRUE)
					forward <- include [which (model.complexity [include] == min (model.complexity [include]))]
					if (model.complexity [forward] > 1){
						effects.forward <- effects.matrix [forward, ]
						effects.forward [e] <- FALSE
						forward.effects <- sapply (1:no.models, function (m) {
							(sum (effects.matrix [m, effects.forward == TRUE]) == sum (effects.forward))
						})
						exclude <- which (!effects.matrix[, e] & forward.effects)
						comparison <- exclude [which (model.complexity [exclude] == min (model.complexity [exclude]))]
						if (model.complexity [comparison] < model.complexity [forward]) {
							bf.forward <- model$models [[forward]]$bf / model$models [[comparison]]$bf
						} else {
							bf.forward <- model$models [[forward]]$bf
						}
					} else {
						bf.forward <- model$models [[forward]]$bf
					}
					#Backward
					if (sum (effects.matrix [, e]) == 1 ) {
						bf.bacward <- model$models [[which (effects.matrix [, e] == TRUE)]]$bf
					} else {
						no.interactions <- sapply (1:no.models, function (m) {
							sum (effects.matrix [m, model$interactions.matrix[e, ] == TRUE]) == 0
						})
						include <- which ((effects.matrix [, e] == TRUE) & no.interactions)
						backward <- include [which (model.complexity [include] == max (model.complexity [include]))]
						if (model.complexity [backward] > 1) {
							effects.backward <- effects.matrix [backward, ]
							effects.backward [e] <- FALSE
							backward.effects <- sapply (1:no.models, function (m) {
								((sum (effects.matrix [m, effects.backward == TRUE]) == sum (effects.backward))
								&&
								(sum (effects.matrix[m, effects.backward == FALSE]) == 0))
							})
							exclude <- which (backward.effects)
							comparison <- exclude [which (model.complexity [exclude] == max (model.complexity [exclude]))]
							bf.backward <- model$models [[backward]]$bf / model$models [[comparison]]$bf
						} else {
							bf.backward <- model$models [[backward]]$bf
						}
					}
					#Output
					if (options$bayesFactorType == "LogBF10"){
						row [["BF<sub>Forward</sub>"]] <- .clean (bf.forward@bayesFactor$bf)
						row [["BF<sub>Backward</sub>"]] <- .clean (bf.backward@bayesFactor$bf)
					} else {
						row [["BF<sub>Forward</sub>"]] <- .clean (exp (bf.forward@bayesFactor$bf))
						row [["BF<sub>Backward</sub>"]] <- .clean (exp (bf.backward@bayesFactor$bf))
					}
					row [["% errorF"]] <- .clean (100 * bf.forward@bayesFactor$error)
					row [["% errorB"]] <- .clean (100 * bf.backward@bayesFactor$error)
				}
			}
			rows [[length (rows) + 1]] <- row
		}
		effectsTable [["data"]] <- rows
	}

	if (is.null(status$analysis.type) || status$analysis.type != "rmANOVA") {
		effectsTable [["title"]] <- paste ("Analysis of Effects - ", options$dependent, sep = "")
	}

	if (! status$ready) # TODO why do we need this?
		effectsTable [["error"]] <- list (errorType = "badData")

	return (effectsTable)
}

.theBayesianLinearModelEstimates <- function (model = NULL, options = list (), perform = "init", status = list (), populate = FALSE) {
	if (! options$posteriorEstimates)
		return (NULL)

	estimatesTable <- list ()
	estimatesTable [["title"]] <- "Parameter Estimates"
	estimatesTable [["citation"]] <-
		list (
			"Morey, R. D. & Rouder, J. N. (2015). BayesFactor (Version 0.9.10-2)[Computer software].",
			"Rouder, J. N., Morey, R. D., Speckman, P. L., Province, J. M., (2012) Default Bayes Factors for ANOVA Designs. Journal of Mathematical Psychology. 56. p. 356-374."
		)
	
	alpha <- (1 - options$posteriorEstimatesCredibleIntervalInterval) / 2
	fields <-
		list (
			list (name = "Effects", type = "string"),
			list (name = "Posterior Median", type = "number", format = "sf:4;dp:3"),
			list (name = "Posterior SD", type = "number", format = "sf:4;dp:3"),
			list (name = "Lower Bound", title = paste (alpha, "%", sep=""), type = "number", format = "sf:4;dp:3"),
			list (name = "Upper Bound", title = paste (1-alpha, "%", sep=""), type = "number", format = "sf:4;dp:3")
		)
	estimatesTable [["schema"]] <- list (fields = fields)

	if (! status$ready && ! status$error)
		return (estimatesTable)
	
	if ( perform == "init" && ! populate)
		return (estimatesTable)

	if (! populate ) {
		effects.matrix <- matrix (model$effects [1:nrow (model$effects), 1:(ncol (model$effects) - 2)],
			nrow = nrow (model$effects),
			ncol = ncol (model$effects) - 2)
		complexity <- rowSums (effects.matrix)
		m <- which (complexity == max (complexity))
	} else {
		effects.matrix <- model$effects
		complexity <- rowSums (effects.matrix)
		m <- which (complexity == max (complexity))
	}
	
	
	if (! populate && is.null (model$models[[m]]$error.message)) {
		chains <- model$models[[m]]$chains
		parameters <- colnames (chains)
		if (length (options$randomFactors) > 0) {
			rfnames <- .v (unlist (options$randomFactors))
			remove <- NULL
			for (name in rfnames) 
				remove <- c (remove, base::grep (x = parameters, pattern = name, fixed = TRUE, ignore.case = TRUE))
			chains <- chains [, - remove]
			parameters <- colnames (chains)
		}
	
		estimates <- matrix (0, nrow = length (parameters), ncol = 4)
		for (e in 1:length (parameters)) {
			estimates [e, 1] <- median (chains[, e])
			estimates [e, 2] <- sd (chains[, e])
			estimates [e, 3] <- quantile (chains [, e], alpha)
			estimates [e, 4] <- quantile (chains [, e], 1-alpha)
		}
		
		for (parameter in 1:length (parameters)) {
			name <- stringr::str_trim (parameters [parameter])
			name <- base::strsplit (name, split = "-", fixed = TRUE) [[1]]
			if (length (name) > 1) {
				levels <- base::strsplit (name [2], split = ".&.", fixed = TRUE) [[1]]
				effects <- base::strsplit (name [1], split = ":", fixed = TRUE) [[1]]
				name <- NULL
				for (e in 1:length (effects)) {
					if (stringr::str_trim (effects [e]) == stringr::str_trim (levels [e])) {
						name <- c (name, .unvf (effects [e]))
					} else {
						name <- c (name, paste (.unvf (effects [e]), levels [e], sep = " -- "))
					}
				}
				name <- paste (name, collapse = " & ")
			} else {
				if (base::substr (name, 1, 1) == "X") {
					name <- .unv (name)
				} else {
					if (name == "sig2") {
						name <- "\u03C3\u00B2"
					} else if (name == "mu") {
						name <- "Intercept"
					} else if (name == "g") {
						name <- "g (prior)"
					} else if (base::substr(name,1,2) == "g_") {
						name <- base::substr(name,3, base::nchar(name))
						if (base::substr (name, 1, 1) == "X")
							name <- .unvf (name)
						name <- paste ("g (prior: ", name,")", sep = "")
					}
				}
			}
			parameters [parameter] <- name
		}
		no.effects <- ncol (chains)
		rows <- list ()
		for (e in 1:no.effects) {
			row <- list ()
			row$"Effects" <- parameters [e]
			row$"Posterior Median" <- .clean ( estimates [e, 1])
			row$"Posterior SD" <- .clean ( estimates [e, 2])
			row$"Lower Bound" <- .clean ( estimates [e, 3])
			row$"Upper Bound" <- .clean ( estimates [e, 4])
			rows [[length (rows) + 1]] <- row
		}
		estimatesTable [["data"]] <- rows
	} else {
		if (! is.null (model$models[[m]]$error.message)) {
			## Footnotes
			footnotes <- .newFootnotes()
			if (length (model$nuisance) > 0) {
					footnote <- paste (model$models[[m]]$error.message, sep = "")
					.addFootnote (footnotes, symbol = "<em>Note.</em>", text = footnote)
			}
			estimatesTable [["footnotes"]] <- as.list (footnotes)
		}

		if ( length (options$fixedFactors) == 0) {
			parameters <- c(.unvf(colnames (effects.matrix)), "\u03C3\u00B2", "g (prior)")
		} else {
			parameters <- c("Intercept", .unvf(colnames (effects.matrix)), "\u03C3\u00B2", "g (prior)")
		}
		no.effects <- length (parameters)
		rows <- list ()
		for (e in 1:no.effects) {
			row <- list ()
			row$"Effects" <- parameters [e]
			row$"Posterior Median" <- ""
			row$"Posterior SD" <- ""
			row$"Lower Bound" <- ""
			row$"Upper Bound" <- ""
			rows [[length (rows) + 1]] <- row
		}
		estimatesTable [["data"]] <- rows
	}

	if (! status$ready)
		estimatesTable [["error"]] <- list (errorType = "badData")

 	estimatesTable [["title"]] <- paste ("Parameter Estimates - ", options$dependent, sep = "")
	
	return (estimatesTable)
}
