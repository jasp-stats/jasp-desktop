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

				data.names <- names(dataset)
				XorF <- substr(data.names,1,1)
				if (any (XorF == "F"))
					data.names [XorF == "F"] <- paste ("X",
						base::substr (data.names [XorF == "F"], start = 2, stop = 1000), sep = "")
				names(dataset) <- data.names
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

	if (!exists ("error.message") && any (!is.finite (dataset [[.v (options$dependent)]])))
		error.message <- "Bayes factor is undefined -- the dependent variable contains infinity"

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

	if (!exists ("error.message"))
		return (list (ready = TRUE, error.message = NULL))
	if (exists ("error.message"))
		return (list (ready = FALSE, error.message = error.message))
}

.theBayesianLinearModels <- function (dataset = NULL, options = list (), perform = "init", status = list ()) {
	if (!status$ready && is.null (status$error.message))
		return (list (model =  list (models = NULL, effects = NULL), status = status))

	model.formula <- paste (.v (options$dependent), " ~ ", sep = "")
	neverExclude <- NULL
	effects <- NULL

	for (term in options$modelTerms) {
		model.formula <- paste (model.formula, " + ",
			paste (.v (term$components), collapse = ":"), sep = "")
		if (term$isNuisance) {
			neverExclude <- c (neverExclude, paste (.v (term$components), collapse = ":"))
		} else {
			effects <- c (effects, paste (.v (term$components), collapse = ":"))
		}
	}
	model.formula <- formula (model.formula)

	model.list <- BayesFactor::enumerateGeneralModels (model.formula, whichModels = "withmain", neverExclude = neverExclude)

	null.model <- list ("ready" = TRUE)
	if (!is.null (neverExclude)) {
		for (m in 1:length (model.list)){
			model.title <- base::strsplit(x = as.character (model.list [[m]]) [[3]], split = "+", fixed = TRUE) [[1]]
			model.title <- stringr::str_trim (model.title)
			model.title <- model.title [model.title != ""]
			if (sum (!(model.title %in% neverExclude)) == 0) break
		}
		null.formula <- model.list [[m]]
		model.list <- model.list [-m]

		if (perform == "run" && status$ready) {
			bf <- try (BayesFactor::lmBF (null.formula,
				data = dataset, whichRandom = .v (unlist (options$randomFactors)),
				progress = FALSE, posterior = FALSE))
			null.model$bf <- bf
			if (class (bf) == "try-error") {
				status$ready <- FALSE
				status$error.message <- "Bayes factor is undefined -- the null model could not be computed"
			}
		}
	}

	no.effects <- length (effects)
	no.models <- length (model.list)

	if (no.models == 0)
		return(list (model =  list(models = NULL, effects = NULL, nuisance = neverExclude), status = status))


	effects.matrix <- matrix (FALSE, nrow = no.models, ncol = no.effects)
	effects <- stringr::str_trim (effects)

	colnames (effects.matrix) <- effects
	rownames (effects.matrix) <- paste ("Model", 1:no.models)

	model.object <- list()
	for (m in 1:no.models) {
		model.object[[m]] <- list("ready" = TRUE)
		model.effects <- base::strsplit (x = as.character (model.list [[m]]) [[3]], split = "+", fixed = TRUE) [[1]]
		model.effects <- stringr::str_trim (model.effects)

		for (effect in model.effects) {
			i <- which (effects == effect)
			effects.matrix[m, i] <- TRUE
		}

		model.title <- strsplit (x = as.character (model.list [[m]]) [[3]], split = "+", fixed = TRUE) [[1]]
		model.title <- stringr::str_trim (model.title)
		model.title <- model.title [model.title != ""]
		model.title <- model.title [!(model.title %in% neverExclude)]
		model.object [[m]]$title <- .unvf (paste (model.title, collapse = " + "))

		if (perform == "run" && status$ready) {
			bf <- try (BayesFactor::lmBF (model.list [[m]],
				data = dataset, whichRandom = .v (unlist (options$randomFactors)),
				progress = FALSE, posterior = FALSE))
			model.object [[m]]$bf <- bf

			if (class (bf) == "try-error") {
				model.object [[m]]$ready <- FALSE
				model.object [[m]]$error.message <- "Bayes factor could not be computed"
			}

			if (length (neverExclude) > 0){
				model.object [[m]]$bf <- model.object [[m]]$bf / null.model$bf
			}
		}
	}
	return (list (model =  list (models = model.object, effects = effects.matrix, nuisance = neverExclude, null.model = null.model), status = status))
}

.theBayesianLinearModelsComparison <- function (model = NULL, options = list (), perform = "init", status = list ()) {
	modelTable <- list ()
	modelTable [["title"]] <- "Model Comparison"
	modelTable [["citation"]] <-
		list (
			"Morey, R. D. & Rouder, J. N. (2015). BayesFactor (Version 0.9.10-2)[Computer software].",
			"Rouder, J. N., Morey, R. D., Speckman, P. L., Province, J. M., (2012) Default Bayes Factors for ANOVA Designs. Journal of Mathematical Psychology. 56. p. 356-374."
		)

	if (options$bayesFactorType == "BF10") {
		bfm.title <- "BF<sub>M</sub>"
		bf.title <- "BF<sub>10</sub>"
	} else if (options$bayesFactorType == "BF01") {
		bfm.title <- "BF<sub>M</sub>"
		bf.title <- "BF<sub>01</sub>"
	} else if (options$bayesFactorType == "LogBF10"){
		bfm.title <- "Log(BF<sub>M</sub>)"
		bf.title <- "Log(BF<sub>10</sub>)"
	}

	fields <-
		list (
			list (name = "Models", type = "string"),
			list (name = "P(M)", type = "number", format = "sf:4;dp:3"),
			list (name = "P(M|data)", type = "number", format = "sf:4;dp:3"),
			list (name = "BFM", type = "number", format = "sf:4;dp:3", title = paste (bfm.title, sep = "")),
			list (name = "BF10", type = "number", format = "sf:4;dp:3", title = paste (bf.title, sep = "")),
			list (name = "% error", type="number", format="sf:4;dp:3")
		)
	
	modelTable [["schema"]] <- list (fields = fields)

	if (!status$ready && is.null (status$error.message))
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

	bayes.factors [1] <- 1
	numerical.error[1]<- 0
	if (no.models > 0) {
		for (m in 1:no.models) {
			rows[[m + 1]] <- list ("Models" = model$models [[m]]$title)
			if (perform == "run" && status$ready && model$models [[m]]$ready) {
				bayes.factors [m + 1] <- exp(model$models [[m]]$bf@bayesFactor$bf)
				numerical.error [m + 1] <- model$models [[m]]$bf@bayesFactor$error
			}
		}
	}

	## Populate table
	if (perform == "run" && status$ready) {
		if (any (is.na (bayes.factors)) || any (!is.finite (bayes.factors))) {
			tmp <- which (!is.na (bayes.factors) & is.finite (bayes.factors))
			posterior.probabilities <- rep (0 , no.models + 1)
			posterior.probabilities [tmp] <- bayes.factors [tmp] / sum (bayes.factors [tmp])
		} else {
			posterior.probabilities <- bayes.factors / sum (bayes.factors)
		}
		prior.probabilities <- rep(1 / (no.models + 1), no.models + 1)
		model$effects <- cbind (model$effects, prior.probabilities [-1], posterior.probabilities [-1])

		BFmodel <- (posterior.probabilities / (1 - posterior.probabilities)) / (prior.probabilities / (1 - prior.probabilities))

		rows [[1]] [["P(M)"]] <- .clean (prior.probabilities [1])
		rows [[1]] [["P(M|data)"]] <- .clean (posterior.probabilities [1])
		if(options$bayesFactorType == "LogBF10") {
			rows [[1]] [["LogBFM"]] <- .clean (log (BFmodel [1]))
			rows [[1]] [["LogBF10"]] <- 0
		} else {
			rows [[1]] [["BFM"]] <- .clean (BFmodel [1])
			rows [[1]] [["BF10"]] <- 1
		}

		for (m in 1:no.models) {
			if (model$models [[m]]$ready) {
				rows [[m+1]] [["P(M)"]] <- .clean (prior.probabilities [m+1])
				rows [[m+1]] [["P(M|data)"]] <- .clean (posterior.probabilities [m+1])
				if (options$bayesFactorType == "LogBF10") {
					rows [[m+1]] [["BFM"]] <- .clean (log (BFmodel [m+1]))
					rows [[m+1]] [["BF10"]] <- .clean (log (bayes.factors [m + 1]))
				} else{
					rows [[m+1]] [["BFM"]] <- .clean (BFmodel [m+1])
					if (options$bayesFactorType == "BF10") {
						rows [[m+1]] [["BF10"]] <- .clean (bayes.factors [m + 1])
					} else {
						rows [[m+1]] [["BF10"]] <- .clean (1 / bayes.factors [m + 1])
					}
				}
				rows [[m+1]] [["% error"]] <- .clean (100*numerical.error [m + 1])
			} else {
				index <- .addFootnote (footnotes, text = model$models [[m]]$error.message)
				rows [[m+1]] [["BF10"]] <- .clean(NaN)
				rows [[m+1]] [[".footnotes"]] <- list ("BFM" = list (index))
			}
		}
	}
	modelTable [["title"]] <- paste ("Model Comparison - ", options$dependent, sep = "")
	modelTable [["data"]] <- rows
	modelTable [["footnotes"]] <- as.list (footnotes)

	if (!status$ready)
		modelTable [["error"]] <- list (errorType = "badData", errorMessage = status$error.message)

	return (list (modelTable = modelTable, model = model))
}

.theBayesianLinearModelsEffects <- function (model = NULL, options = list (), perform = "init", status = list ()) {
	if (!options$outputEffects)
		return (NULL)

	effectsTable <- list ()
	effectsTable [["title"]] <- "Analysis of Effects"
	effectsTable [["citation"]] <-
		list (
			"Morey, R. D. & Rouder, J. N. (2015). BayesFactor (Version 0.9.10-2)[Computer software].",
			"Rouder, J. N., Morey, R. D., Speckman, P. L., Province, J. M., (2012) Default Bayes Factors for ANOVA Designs. Journal of Mathematical Psychology. 56. p. 356-374."
		)
	
	if (options$bayesFactorType == "LogBF10"){
		bf.inclusion.title <- "Log(BF<sub>Inclusion</sub>)"
		bf.forward.title <- "Log(BF<sub>Backward</sub>)"
		bf.backward.title <- "Log(BF<sub>Forward</sub>)"
	} else {
		bf.inclusion.title <- "BF<sub>Inclusion</sub>"
		bf.forward.title <- "BF<sub>Backward</sub>"
		bf.backward.title <- "BF<sub>Forward</sub>"			
	}

	if (options$effectsStepwise) {
		fields <-
			list (
				list (name = "Effects", type = "string"),
				list (name = "P(incl)", type = "number", format = "sf:4;dp:3"),
				list (name = "P(incl|data)", type = "number", format = "sf:4;dp:3"),
				list (name = "BF<sub>Inclusion</sub>", type="number", format = "sf:4;dp:3", title = paste (bf.inclusion.title, sep = "")),
				list (name = "BF<sub>Backward</sub>", type="number", format = "sf:4;dp:3", title = paste (bf.backward.title, sep = "")),
				list (name = "% errorB", type = "number", format = "sf:4;dp:3"),
				list (name = "BF<sub>Forward</sub>", type = "number", format = "sf:4;dp:3", title = paste (bf.forward.title, sep = "")),
				list (name = "% errorF", type = "number", format = "sf:4;dp:3")
			)
	} else{
		fields <-
			list (
				list (name = "Effects", type = "string"),
				list (name = "P(incl)", type = "number", format = "sf:4;dp:3"),
				list (name = "P(incl|data)", type = "number", format = "sf:4;dp:3"),
				list (name = "BF<sub>Inclusion</sub>", type = "number", format = "sf:4;dp:3", title = paste (bf.inclusion.title, sep = ""))
			)
	}

	effectsTable [["schema"]] <- list (fields = fields)

	if (!status$ready && is.null (status$error.message))
		return (effectsTable)

	effects.matrix <- model$effects
	if (perform == "run" && status$ready) {
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
		bayes.factor.inclusion <- (posterior.inclusion.probabilities / (1 - posterior.inclusion.probabilities)) /
			(prior.inclusion.probabilities / (1 - prior.inclusion.probabilities))
		model.complexity <- rowSums (effects.matrix)
	}

	no.effects <- ncol (effects.matrix)
	effectNames <- colnames (effects.matrix)

	if (!is.null(no.effects) && no.effects > 0) {
		rows <- list ()
		for (e in 1:no.effects) {
			row <- list ()
			row$"Effects" <- .unvf (effectNames [e])
			if (perform == "run" && status$ready) {
				row$"P(incl)" = .clean (prior.inclusion.probabilities [e])
				row$"P(incl|data)" = .clean (posterior.inclusion.probabilities [e])
				if (options$bayesFactorType == "LogBF10"){
					row$"BF<sub>Inclusion</sub>" = .clean (log (bayes.factor.inclusion [e]))
				} else {
					row$"BF<sub>Inclusion</sub>" = .clean (bayes.factor.inclusion [e])
				}

				if (options$effectsStepwise) {
					include.effect <- which (effects.matrix [, e] == TRUE)
					exclude.effect <- which (effects.matrix [, e] == FALSE)
					forward.model <- include.effect [which (model.complexity [include.effect] == min (model.complexity [include.effect]))]
					if (options$bayesFactorType == "LogBF10"){
						row [["BF<sub>Forward</sub>"]] <- .clean (model$models [[forward.model]]$bf@bayesFactor$bf)
					} else {
						row [["BF<sub>Forward</sub>"]] <- .clean (exp (model$models [[forward.model]]$bf@bayesFactor$bf))
					}
					row [["% errorF"]] <- .clean (100 * model$models [[forward.model]]$bf@bayesFactor$error)

					if (length (exclude.effect) > 0) { #there must be only one model, otherwise nuisance...
						exclude <- exclude.effect [which (model.complexity [exclude.effect] == max (model.complexity [exclude.effect]))]
						include <- include.effect [which (model.complexity [include.effect] == max (model.complexity [include.effect]))]

						backward.bf <- model$models [[include]]$bf / model$models [[exclude]]$bf
						if (options$bayesFactorType == "LogBF10"){
							row [["BF<sub>Backward</sub>"]] <- .clean (backward.bf@bayesFactor$bf)
						} else {
							row [["BF<sub>Backward</sub>"]] <- .clean (exp (backward.bf@bayesFactor$bf))
						}
						row [["% errorB"]] <- .clean (100 * backward.bf@bayesFactor$error)
					}
				}
			}
			rows [[length (rows) + 1]] <- row
		}
		effectsTable [["data"]] <- rows
	}

 	effectsTable [["title"]] <- paste ("Analysis of Effects - ", options$dependent, sep = "")

	if (!status$ready)
		effectsTable [["error"]] <- list (errorType = "badData")

	return (effectsTable)
}
