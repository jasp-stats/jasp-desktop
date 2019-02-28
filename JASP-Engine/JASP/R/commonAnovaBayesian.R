#
# Copyright (C) 2013-2018 University of Amsterdam
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

# Abbreviations:
# HF = helper function

.BANOVArunAnalysis <- function(jaspResults, dataset, options, analysisType = c("ANOVA", "ANCOVA", "RM-ANOVA")) {

  # the main workhorse of the Bayesian ANOVA, ANCOVA, and Repeated Measures.

  analysisType <- match.arg(analysisType)
  dataset <- .BANOVAreadData(dataset, options, analysisType)
  errors  <- .BANOVAerrorhandling(dataset, options, analysisType)

  model <- .BANOVAestimateModels(jaspResults, dataset, options, errors, analysisType)
  model[["posteriors"]] <- .BANOVAestimatePosteriors(jaspResults, dataset, options, model)

  .BANOVAeffectsTable  (jaspResults, options, model)
  .BANOVAestimatesTable(jaspResults, options, model)

  # model averaged plots
  .BANOVAposteriorPlot(jaspResults, dataset, options, model)
  .BANOVAqqplot       (jaspResults, options, model)
  .BANOVArsqplot      (jaspResults, options, model)

  .BANOVAnullControlPostHocTable(jaspResults, dataset, options, model)

  # single model plots and tables
  .BANOVAsmi(jaspResults, dataset, options, model)

  # descriptives
  .BANOVAdescriptives(jaspResults, dataset, options, errors, analysisType)

  return()
}

.BANOVAerrorhandling <- function(dataset, options, analysisType) {

  if (analysisType != "RM-ANOVA") {
    hasDV <- options$dependent != ""
    hasIV <- any(lengths(options[c("fixedFactors", "covariates")]) != 0)
    fixed <- options$fixedFactors
    noVariables <- !(hasDV && hasIV)
    target <- c(options$covariates, options$dependent)
  } else {
    hasDV  <- !any(options$repeatedMeasuresCells == "")
    hasIV  <- any(lengths(options[c("betweenSubjectFactors", "covariates")]) != 0)
    fixed  <- options$betweenSubjectFactors
    target <- c(options$covariates, "dependent")
    noVariables <- !hasDV
  }

  errors <- NULL
  if (!noVariables) {
    errors <- .hasErrors(
      dataset = dataset,
      perform = "run",
      type    = c("infinity", "observations", "variance", "factorLevels"),
      infinity.target     = target,
      variance.target     = target,
      observations.target = target,
      observations.amount = paste("<", length(options$modelTerms) + 1),
      factorLevels.target = fixed,
      factorLevels.amount = " < 2",
      exitAnalysisIfErrors = TRUE
    )
  }
  return(list(noVariables = noVariables, errors = errors, hasIV = hasIV, hasDV = hasDV))
}

# model comparison ----
.BANOVAestimateModels <- function(jaspResults, dataset, options, errors,
                                  analysisType = c("ANOVA", "ANCOVA", "RM-ANOVA")) {

  # also makes the model comparison table
  stateObj <- jaspResults[["tableModelComparisonState"]]$object
  if (!is.null(jaspResults[["tableModelComparison"]])) {
    stateObj$completelyReused <- TRUE # means that posteriors won't need to be resampled
    return(stateObj)
  } else if (errors$noVariables) {
    modelTable <- .BANOVAinitModelComparisonTable(options, NULL)
    jaspResults[["tableModelComparison"]] <- modelTable
    return(list(analysisType = analysisType))
  } else if (!is.null(stateObj)) {
    if ((identical(stateObj$fixedFactors, options$fixedFactors) &&
         identical(stateObj$modelTerms,   options$modelTerms)   &&
         identical(stateObj$covariates,   options$covariates)
       )) {

      # if the statement above is TRUE then no new variables were added
      # and the only change is in the Bayes factor type or the ordering
      modelTable <- .BANOVAinitModelComparisonTable(options, stateObj$models)
      internalTableObj <- .BANOVAfinalizeInternalTable(options, stateObj$internalTableObj$internalTable)
  	  modelTable$setData(internalTableObj$table)
      jaspResults[["tableModelComparison"]] <- modelTable
      
      stateObj$completelyReused <- TRUE # means that posteriors won't need to be resampled
	    return(stateObj)
  	  
    }
  }

	rscaleFixed   <- options$priorFixedEffects
	rscaleRandom  <- options$priorRandomEffects
	modelTerms    <- options$modelTerms
	dependent     <- options$dependent
	randomFactors <- options$randomFactors
	fixedFactors  <- options$fixedFactors

	if (analysisType == "RM-ANOVA") {
	  rscaleCont <- options$priorCovariates
	  modelTerms[[length(modelTerms) + 1L]] <- list(components = "subject", isNuisance = TRUE)
	  
	  dependent     <- "dependent"
	  randomFactors <- "subject"
	  
	  # idx <- match(c("dependent", "subject"), colnames(dataset))
	  # colnames(dataset)[idx] <- .v(colnames(dataset)[idx])
	  # if (length(colnames(dataset)[-idx]) > 0)
	  #   fixedFactors <- colnames(dataset)[-idx]
	  
	} else if (analysisType == "ANCOVA") {
	  rscaleCont <- options$priorCovariates
	} else {
	  rscaleCont <- "medium" # sqrt(2)/4
	}
	iter <- NA

	tmp <- .BANOVAcreateModelFormula(dependent, modelTerms)
	model.formula <- tmp$model.formula
	nuisance      <- tmp$nuisance
	effects       <- tmp$effects
	#Make a list of models to compare
	model.list <- .BANOVAgenerateAllModelFormulas(model.formula, nuisance)
	
	if (length(model.list) == 1L) {
	  modelTable <- .BANOVAinitModelComparisonTable(options)
	  modelTable$setError("Bayes factor is undefined -- all effects are specified as nuisance.")
	  jaspResults[["tableModelComparison"]] <- modelTable
	  return(list(analysisType = analysisType))
	}

	# TODO: discuss if we want a hard limit if length(model.list) > ...
	# that would avoid peoples computers from slowing down tremendously.

	#Run all other models and store the results in the model.object
	neffects <- length(effects)
	nmodels <- length(model.list)
	modelObject <- vector("list", nmodels)
	if (nmodels > 0L && neffects > 0L) {
	  effects.matrix <- matrix(data = FALSE, nrow = nmodels, ncol = neffects, 
	                           dimnames = list(c("Null model", paste("Model", seq_len(nmodels - 1L))), effects))
	  # effects <- stringr::str_trim(effects)

	  interactions.matrix <- matrix(FALSE, nrow = neffects, ncol = neffects)
	  rownames(interactions.matrix) <- colnames(interactions.matrix) <- effects
	  if (neffects > 1L) {
	    effect.components <- sapply(effects, strsplit, split = ":", fixed = TRUE)

	    for (e in seq_len(neffects)) {
	      interactions.matrix[e, ] <- sapply(1:neffects, function(ee) {
	        (sum(effect.components[[e]] %in% effect.components[[ee]]) == length(effect.components[[e]]))
	      })
	    }
	    diag(interactions.matrix) <- FALSE
	  }

	  for (m in seq_len(nmodels)) {
	    modelObject[[m]] <- list(ready = TRUE)
	    if (m == 1L) {
	      if (is.null(nuisance)) { # intercept only
	        modelObject[[m]]$title <- "Null model"
	        next # all effects are FALSE anyway
	      } else {
	        modelObject[[m]]$title <- paste0("Null model (incl. ", paste(.unvf(nuisance), collapse = ", "), ")")
	      }
	    }
	    model.effects <- .BANOVAgetFormulaComponents(model.list[[m]])
	    
	    idx <- match(model.effects, effects, nomatch = 0L)
	    idx <- idx[!is.na(idx)]
	    effects.matrix[m, idx] <- TRUE

	    if (m > 1L) {
	      model.title <- setdiff(model.effects, nuisance)
	      modelObject[[m]]$title <- .unvf(paste(model.title, collapse = " + "))
	    }
	  }
	}

	modelTable <- .BANOVAinitModelComparisonTable(options)
	modelNames <- sapply(modelObject, `[[`, "title")

	modelTable[["Models"]] <- modelNames
	jaspResults[["tableModelComparison"]] <- modelTable
	# internalTable is an internal representation of the model comparison table
	internalTable <- matrix(NA, nmodels, 5L, 
	                        dimnames = list(modelNames, c("P(M)", "P(M|data)", "BFM", "BF10", "error %")))
	# set BF null model and p(M)
	internalTable[1L, 4L] <- 0
	internalTable[, 1L] <- 1 / nmodels

	#Now compute Bayes Factors for each model in the list, and populate the tables accordingly
	jaspResults$startProgressbar(nmodels)

	# check if any models can be resued from the state
  if (!is.null(stateObj[["modelTerms"]])) {

    oldFormulas <- sapply(stateObj$model.list, .BANOVAreorderFormulas)
    newFormulas <- sapply(model.list, .BANOVAreorderFormulas)

    reuseable   <- match(newFormulas, oldFormulas)

  } else {
    reuseable <- rep(NA, nmodels)
  }

	if (analysisType == "RM-ANOVA") { 
	  # the default null-model contains subject
	  anyNuisance <- TRUE
	} else {
	  # the default null-model is intercept only
	  anyNuisance <- length(nuisance) > 0L
	}

  # without these there is no error
  bfIterations <- if (options[["sampleModeNumAcc"]] == "auto") 1e4L else options[["fixedNumAcc"]]

	for (m in seq_len(nmodels)) {
	  # loop over all models, where the first is the null-model and the last the most complex model
	  if (is.na(reuseable[m])) {
	    if (!is.null(model.list[[m]])) {
	      bf <- try(BayesFactor::lmBF(
	        formula      = model.list[[m]],
	        data         = dataset,
	        whichRandom  = .v(unlist(randomFactors)),
	        progress     = FALSE,
	        posterior    = FALSE,
	        # callback     = .callbackBFpackage,
	        rscaleFixed  = rscaleFixed,
	        rscaleRandom = rscaleRandom,
	        rscaleCont   = rscaleCont,
	        iterations   = bfIterations))
	      if (isTryError(bf)) {
	        .quitAnalysis("Bayes factor is undefined -- the null model could not be computed")
	      } else {
	        # delete the data object -- otherwise it gets saved in the state
	        bf@data <- data.frame()
	      }
	    } else {
	      bf <- NULL
	    }
	  } else {
	    bf <- stateObj$models[[reuseable[m]]]$bf
	  }
	  modelObject[[m]]$bf <- bf

	  if (!is.null(bf)) {
	    if (isTryError(bf)) {
	      message <- .extractErrorMessage(bf)
	      modelObject[[m]]$error.message <- "Bayes factor could not be computed"
	    } else {
	      # bfObj can have a modified denominator, but the saved objects are always compared against intercept only
        bfObj <- modelObject[[m]]$bf
	      if (anyNuisance)
	        bfObj <- bfObj / modelObject[[1L]]$bf

	      internalTable[m, "BF10"]    <- bfObj@bayesFactor[, "bf"] # always LogBF10!
	      internalTable[m, "error %"] <- bfObj@bayesFactor[, "error"]
	      modelObject[[m]]$ready <- TRUE
	    }

	    modelTable[["BF10"]]    <- .recodeBFtype(internalTable[, "BF10"],
	                                             newBFtype = options$bayesFactorType,
	                                             oldBFtype = "LogBF10")
	    modelTable[["error %"]] <- internalTable[, "error %"]
	  }
	  jaspResults$progressbarTick()
	}

	internalTableObj <- .BANOVAfinalizeInternalTable(options, internalTable)
	modelTable$setData(internalTableObj$table)
	
	if (anyNuisance) {
	  message <- paste("All models include", paste0(.unv(nuisance), collapse = ", "))
	  modelTable$addFootnote(message = message, symbol = "<em>Note.</em>")
	}

	model <- list(
	  models              = modelObject,
	  postProbs           = internalTableObj$internalTable[, "P(M|data)"],
	  internalTableObj    = internalTableObj,
	  effects             = effects.matrix,
	  interactions.matrix = interactions.matrix,
	  nuisance            = nuisance,
	  analysisType        = analysisType,
	  # these are necessary for partial reusage of the state (e.g., when a fixedFactor is added/ removed)
	  model.list          = model.list,
	  fixedFactors        = fixedFactors,
	  randomFactors       = randomFactors, # stored because they are modified in RM-ANOVA
	  modelTerms          = modelTerms,
	  reuseable           = reuseable,
	  RMFactors           = options[["repeatedMeasuresFactors"]]
	)

	# save state
	stateObj <- createJaspState(object = model, dependencies = c(
    "dependent", "priorFixedEffects", "priorRandomEffects", "sampleModeNumAcc", "fixedNumAcc", "repeatedMeasuresCells"
	))
  jaspResults[["tableModelComparisonState"]] <- stateObj

	return(model)
}

.BANOVAeffectsTable <- function(jaspResults, options, model) {

  if (!is.null(jaspResults[["tableEffects"]]) || !options[["effects"]])
    return()

  if (model[["analysisType"]] != "RM-ANOVA" && options[["dependent"]] != "") {
		title <- paste0("Analysis of Effects - ", options[["dependent"]])
  } else {
	  title <- "Analysis of Effects"
	}

  effectsTable <- createJaspTable(title = title)
  jaspResults[["tableEffects"]] <- effectsTable
  effectsTable$position <- 2
  effectsTable$dependOnOptions(c(
    "effects", "effectsType", "dependent", "randomFactors", "priorFixedEffects", "priorRandomEffects", 
    "sampleModeNumAcc", "fixedNumAcc", "bayesFactorType", "modelTerms", "fixedFactors"
	))

	effectsTable$addCitation("Morey, R. D. & Rouder, J. N. (2015). BayesFactor (Version 0.9.10-2)[Computer software].")
	effectsTable$addCitation("Rouder, J. N., Morey, R. D., Speckman, P. L., Province, J. M., (2012) Default Bayes Factors for ANOVA Designs. Journal of Mathematical Psychology. 56. p. 356-374.")

	if (options$bayesFactorType == "LogBF10") {
		inclusion.title <- "Log(BF<sub>Inclusion</sub>)"
		forward.title   <- "Log(BF<sub>Forward</sub>)"
		backward.title  <- "Log(BF<sub>Backward</sub>)"
	} else {
		inclusion.title <- "BF<sub>Inclusion</sub>"
		forward.title   <- "BF<sub>Forward</sub>"
		backward.title  <- "BF<sub>Backward</sub>"
	}

	effectsTable$addColumnInfo(name = "Effects",      type = "string")
	effectsTable$addColumnInfo(name = "P(incl)",      type = "number", format = "sf:4;dp:3")
	effectsTable$addColumnInfo(name = "P(incl|data)", type = "number", format = "sf:4;dp:3")
	effectsTable$addColumnInfo(name = "BFInclusion",  type = "number", format = "sf:4;dp:3", title = inclusion.title)

	if (options$effectsType == "matchedModels") {
	  effectsTable$addFootnote(message = paste0("Compares models that contain the effect to equivalent models stripped",
	  "of the effect. Higher-order interactions are excluded. Analysis suggested by Sebastiaan Mathôt."),
	  symbol = "<em>Note.</em>")
	}

	if (is.null(model$models)) {
	  effectsTable$setExpectedRows(1L)
	  return()
	}

	effects.matrix <- model$effects
	no.effects <- ncol(effects.matrix)
	effectNames <- colnames(model$effects)

	if (options$effectsType == "allModels") {

	  # note that the postInclProb is equivalent to model$posteriors$weights[-1] * (1 - model$postProbs[1])
	  priorInclProb <- colMeans(effects.matrix)
	  postInclProb  <- crossprod(effects.matrix[-1L, , drop = FALSE], model$postProbs[-1L])
	  
	   # deal with numerical error
	  postInclProb[postInclProb > 1] <- 1
	  postInclProb[postInclProb < 0] <- 0
	  bfIncl <- (postInclProb / (1 - postInclProb)) / (priorInclProb / (1 - priorInclProb))

	} else { 
	   # this method is inspired by this post: https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp

		priorInclProb <- postInclProb <- bfIncl <- numeric(length(effectNames))
		for (i in seq_along(effectNames)) {
		  effect <- effectNames[i]

		  # get all higher order interactions of which this effect is a component
		  # e.g., V1 is a component of V1:V2
		  idx1 <- which(model$interactions.matrix[effect, ])

		  # get all models that exclude the predictor, but that always include the lower order main effects
	    # e.g., V1:V2 is compared against models that always include V1 and V2
	    idx2 <- which(model$interactions.matrix[, effect]) # if effect is V1:V2, idx3 contains c(V1, V2)
	    
	    # idx3 is FALSE if a model contains higher order interactions of effect, TRUE otherwise
	    idx3 <- !matrixStats::rowAnys(effects.matrix[, idx1, drop = FALSE])

	    # all models that include the effect, without higher order interactions
	    idx4 <- idx3 & effects.matrix[, i]
	    priorInclProb[i] <- mean(idx4)
	    postInclProb[i]  <- sum(idx4 * model[["postProbs"]])
	    
	    # the models to consider for the prior/ posterior exclusion probability. 
	    # idx5 includes models that have: all subcomponents & no higher order interaction & not the effect
	    idx5 <- matrixStats::rowAlls(effects.matrix[, idx2, drop = FALSE]) & idx3 & !effects.matrix[, i]

	    priorExclProb <- mean(idx5)
      postExclProb  <- sum(idx5 * model$postProbs)

      # compute inclusion BF
      bfIncl[i]     <- (postInclProb[i] / postExclProb) / (priorInclProb[i] / priorExclProb)
		}
	}

	effectsTable[["Effects"]]      <- .unvf(effectNames)
	effectsTable[["P(incl)"]]      <- priorInclProb
	effectsTable[["P(incl|data)"]] <- postInclProb
	# FIXME: remove .clean after this is handled by jaspResults
	if (options[["bayesFactorType"]] == "LogBF10") {
	  effectsTable[["BFInclusion"]] <- sapply(log(bfIncl), .clean)
	} else {
	  effectsTable[["BFInclusion"]] <- sapply(bfIncl, .clean)
	}
	return()
}

.BANOVAinitModelComparisonTable <- function(options, modelObject = NULL) {

  # function that creates an empty JASP table to be filled later
  modelTable <- createJaspTable(title = "Model Comparison")
  modelTable$position <- 1L
  modelTable$addCitation("Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].")
  modelTable$addCitation("Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012). Default Bayes factors for ANOVA designs. Journal of Mathematical Psychology, 56, 356-374.")
  modelTable$dependOnOptions(c(
    "dependent", "randomFactors", "covariates", "priorFixedEffects", "priorRandomEffects", "sampleModeNumAcc",
    "fixedNumAcc", "bayesFactorType", "bayesFactorOrder", "modelTerms", "fixedFactors", "betweenSubjectFactors", 
    "repeatedMeasuresFactors", "repeatedMeasuresCells"
  ))
  
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
  
  modelTable$addColumnInfo(name = "Models",    type = "string")
  modelTable$addColumnInfo(name = "P(M)",      type = "number", format = "sf:4;dp:3")
  modelTable$addColumnInfo(name = "P(M|data)", type = "number", format = "sf:4;dp:3")
  modelTable$addColumnInfo(name = "BFM",       type = "number", format = "sf:4;dp:3", title = bfm.title)
  modelTable$addColumnInfo(name = "BF10",      type = "number", format = "sf:4;dp:3", title = bf.title)
  modelTable$addColumnInfo(name = "error %",   type = "number", format = "sf:4;dp:3")
  
  if (!is.null(modelObject)) {
    modelTable[["Models"]] <- c("Null model", sapply(modelObject, `[[`, "title"))
    modelTable[["P(M)"]]   <- rep(1 / (length(modelObject) + 1), length(modelObject) + 1L)
  } else {
    modelTable$setExpectedRows(1L)
  }
  return(modelTable)
}

.BANOVAfinalizeInternalTable <- function(options, internalTable) {
  
  # function that actually fills in the table created by .BANOVAinitModelComparisonTable
  if (anyNA(internalTable[, "P(M|data)"])) { 
    # if TRUE, called from analysis
    if (!anyNA(internalTable[, "BF10"])) {
      # no errors, proceed normally and complete the table
      
      logSumExp <- matrixStats::logSumExp
      
      logbfs <- internalTable[, "BF10"]
      logsumbfs <- logSumExp(logbfs)
      internalTable[, "P(M|data)"] <-  exp(logbfs - logsumbfs)
      
      nmodels <- nrow(internalTable)
      mm <- max(logbfs)
      for (i in seq_len(nmodels)) {
        internalTable[i, "BFM"] <- logbfs[i] - logSumExp(logbfs[-i]) + log(nmodels - 1L)
      }
      
    } else {
      # errors, attempt to salvage some results
    }
  } # else: results already computed
  
  # create the output table
  table <- as.data.frame(internalTable)
  table[["Models"]] <- rownames(internalTable)
  if (options[["bayesFactorType"]] == "LogBF10") {
    table[["BFM"]]  <- internalTable[, "BFM"]
  } else {
    table[["BFM"]]  <- exp(internalTable[, "BFM"])
  }

  o <- order(table[["BF10"]], decreasing = TRUE)
  table <- table[o, ]
  idxNull <- which(o == 1L)
  if (options[["bayesFactorOrder"]] == "nullModelTop") {
    table[idxNull, "error %"] <- NA
  } else {

    table[["BF10"]] <- table[["BF10"]] - table[1L, "BF10"]

    # recompute error (see BayesFactor:::`.__T__/:base`$`BFBayesFactor#BFBayesFactor`)
    table[idxNull, "error %"] <- 0
    table[["error %"]] <- sqrt(table[["error %"]]^2 + table[["error %"]][1L]^2)
    table[1L, "error %"] <- NA
  }
  
  table[["BF10"]] <- .recodeBFtype(table[["BF10"]], newBFtype = options[["bayesFactorType"]], oldBFtype = "LogBF10")
  table[["error %"]] <- 100 * table[["error %"]]
  
  return(list(table = table, internalTable = internalTable, footnotes = NULL))
  
}

# posterior inference ----
.BANOVAestimatePosteriors <- function(jaspResults, dataset, options, model) {

  userNeedsPosteriorSamples <- options$posteriorEstimates || options$posteriorPlot || options$qqPlot || options$rsqPlot
  if (is.null(model$models) || !userNeedsPosteriorSamples)
    return()

  # model$completelyReused is needed because it can happen that some posterior samples can be reused (e.g., 
  # when the modelTerms change)
  stateObj <- jaspResults[["statePosteriors"]]$object
  if (!is.null(stateObj) && isTRUE(model$completelyReused)) {
    return(stateObj)
  }
	# calculate posteriors
  posteriors <- .BANOVAsamplePosteriors(jaspResults, dataset, options, model, stateObj)
  
  stateObj <- createJaspState(object = posteriors)
  stateObj$copyDependenciesFromJaspObject(jaspResults[["tableModelComparisonState"]])
  jaspResults[["statePosteriors"]] <- stateObj
  
  return(posteriors)
}

.BANOVAestimatesTable <- function(jaspResults, options, model) {
  
  if (!is.null(jaspResults[["tablePosteriorEstimates"]]) || !options[["posteriorEstimates"]])
    return()

  estsTable <- createJaspTable(title = "Model Averaged Posterior Summary")
  estsTable$position <- 3
  jaspResults[["tablePosteriorEstimates"]] <- estsTable
  estsTable$dependOnOptions(c(
    "dependent", "randomFactors", "priorFixedEffects", "priorRandomEffects", "sampleModeMCMC",
    "fixedMCMCSamples", "bayesFactorType", "modelTerms", "fixedFactors", "posteriorEstimates",
    "repeatedMeasuresFactors"
	))

  overTitle <- sprintf("%s%% Credible Interval", format(100 * 0.95, digits = 3))
  estsTable$addColumnInfo(name = "Variable", type = "string")
  estsTable$addColumnInfo(name = "Level",    type = "string")
  estsTable$addColumnInfo(name = "Mean",     type = "number", format = "sf:4;dp:3")
	estsTable$addColumnInfo(name = "SD",       type = "number", format = "sf:4;dp:3")
	estsTable$addColumnInfo(name = "Lower",    type = "number", format = "sf:4;dp:3", overtitle = overTitle)
	estsTable$addColumnInfo(name = "Upper",    type = "number", format = "sf:4;dp:3", overtitle = overTitle)

	if (is.null(model[["posteriors"]])) {
    estsTable$setExpectedRows(1L)	  
	  return()
	}
	.BANOVAfillEstimatesTable(
	  jaspTable   = estsTable,
	  mus         = model$posteriors$weightedMeans,
	  sds         = model$posteriors$weightedSds,
	  cri         = model$posteriors$weightedCRIs,
	  hasNoLevels = model$posteriors$allContinuous,
	  isRandom    = model$posteriors$isRandom
	)
	return()
}

.BANOVAfillEstimatesTable <- function(jaspTable, mus, sds, cri, hasNoLevels, isRandom = NULL) {

  if (!is.null(isRandom) && any(isRandom)) {
    # remove random effects
    mus <- mus[!isRandom]
    sds <- sds[!isRandom]
    cri <- cri[!isRandom, ]
  }
  
  table <- .BANOVAgetLevelsFromParamNames(names(mus))
  colnames(table) <- c("Variable", "Level")
  
  # set repeated parameter names to ""
  idxDup <- duplicated(table[, "Variable"])
  table[idxDup, "Variable"]  <- ""
  # decode base64 variables
  table[!idxDup, "Variable"] <- .unvf(table[!idxDup, "Variable"])
  # rename mu to Intercept
  table[1L, "Variable"] <- "Intercept"
  # attach posterior means and sds
  table <- cbind(as.data.frame(table), 
                 "Mean"  = mus, 
                 "SD"    = sds, 
                 "Lower" = cri[, 1L],
                 "Upper" = cri[, 2L])
  if (hasNoLevels)
    table <- table[, -2L]
  jaspTable$setData(table)
  return()
}

# Plots wrappers ----
.BANOVAposteriorPlot <- function(jaspResults, dataset, options, model) {

  # meta wrapper for model averaged posterior plots
  if (!is.null(jaspResults[["posteriorPlot"]]) || !options$posteriorPlot)
    return()

  posteriorPlotContainer <- createJaspContainer(title = "Model Averaged Posterior Distributions")
  jaspResults[["posteriorPlot"]] <- posteriorPlotContainer
  posteriorPlotContainer$dependOnOptions(c("posteriorPlot", "modelTerms"))
  posteriorPlotContainer$position <- 4

  if (is.null(model$models)) {
    posteriorPlotContainer[["dummyplot"]] <- createJaspPlot(title = "Posterior distribution", width = 400, height = 400,
                                                            plot = NULL)
  } else {
    posteriorPlotContainer$copyDependenciesFromJaspObject(jaspResults[["tableModelComparisonState"]])
    .BANOVAfillPosteriorPlotContainer(
      container = posteriorPlotContainer,
      densities = model$posterior$weightedDensities[, -1L, ], # omit intercept
      cris      = model$posterior$weightedCRIs[-1L, ],        # omit intercept
      isRandom  = model$posteriors$isRandom[-1L]              # omit intercept
    )
  }
  return()
}

.BANOVAfillPosteriorPlotContainer <- function(container, densities, cris, isRandom = NULL) {
  
  allParamNames <- colnames(densities)

  tmp <- .BANOVAgetLevelsFromParamNames(allParamNames)
  plotTitles <- .unvf(tmp[, "parameter"])
  xNames <- tmp[, "level"]

  if (is.null(isRandom)) {
    indices <- seq_along(allParamNames)
  } else {
    indices <- which(!isRandom)
  }
  
  for (i in indices) {

    # make prior posterior plot
    df <- data.frame(x = densities[, i, "x"], 
                     y = densities[, i, "y"])

    p <- JASPgraphs::PlotPriorAndPosterior(
      dfLines    = df, 
      xName      = xNames[i], 
      CRI        = cris[i, ], 
      drawCRItxt = FALSE
    )

    plot <- createJaspPlot(title = plotTitles[i], width = 400, height = 400, plot = p)
    plot$dependOnOptions("dependent") # gotta depend on something
    container[[allParamNames[i]]] <- plot
  }
  return()
  
}

.BANOVAqqplot <- function(jaspResults, options, model) {

  if (!is.null(jaspResults[["QQplot"]]) || !options[["qqPlot"]])
    return()

  plot <- createJaspPlot(
    title       = "Model Averaged Q-Q Plot",
    width       = 400,
    height      = 400,
    aspectRatio = 1
  )
  
  if (!is.null(model[["models"]])) {
    plot$plotObject <- JASPgraphs::plotQQnorm(
      residuals = model[["posteriors"]][["weightedResidSumStats"]][,"mean"], 
      lower     = model[["posteriors"]][["weightedResidSumStats"]][,"cri.2.5%"], 
      upper     = model[["posteriors"]][["weightedResidSumStats"]][,"cri.97.5%"]
    )
    plot$copyDependenciesFromJaspObject(jaspResults[["tableModelComparisonState"]])
  } 

  plot$dependOnOptions(c("qqPlot", "modelTerms"))
  plot$position <- 5
  jaspResults[["QQplot"]] <- plot
  return()
}

.BANOVArsqplot <- function(jaspResults, options, model) {

  if (!is.null(jaspResults[["rsqplot"]]) || !options[["rsqPlot"]])
    return()

  plot <- createJaspPlot(
    title       = "Model Averaged Posterior R\u00B2",
    width       = 400,
    height      = 400,
    aspectRatio = 1
  )
  
  if (!is.null(model[["models"]])) {
    dd     <- model[["posteriors"]][["weightedRsqDens"]]
    rsqCri <- model[["posteriors"]][["weightedRsqCri"]]

    df <- data.frame(x = dd$x, y = dd$y)
    xName <- expression(R^2)
    plot$plotObject <- JASPgraphs::PlotPriorAndPosterior(dfLines = df, xName = xName, CRI = rsqCri, drawCRItxt = FALSE)
    plot$copyDependenciesFromJaspObject(jaspResults[["tableModelComparisonState"]])
  }

  plot$dependOnOptions(c("rsqPlot", "modelTerms"))
  plot$position <- 6
  jaspResults[["rsqplot"]] <- plot
  return()
}

# Post hoc comparison ----
.BANOVAnullControlPostHocTable <- function(jaspResults, dataset, options, model) {

  if (!is.null(jaspResults[["collectionPosthoc"]]) || length(options$postHocTestsVariables) == 0L)
    return()

  postHocCollection <- createJaspContainer(title = "Post Hoc Tests")
  postHocCollection$position <- 7
  jaspResults[["collectionPosthoc"]] <- postHocCollection
  postHocCollection$addCitation("Jeffreys, H. (1938). Significance tests when several degrees of freedom arise simultaneously. Proceedings of the Royal Society of London. Series A, Mathematical and Physical Sciences, 165, 161–198.")
  postHocCollection$addCitation("Westfall, P. H., Johnson, W. O., & Utts, J. M. (1997). A Bayesian perspective on the Bonferroni adjustment. Biometrika, 84, 419-427.")

  # the same footnote for all the tables
  footnote <-
    "The posterior odds have been corrected for multiple testing by
		fixing to 0.5 the prior probability that the null hypothesis holds
		across all comparisons (Westfall, Johnson, & Utts, 1997). Individual
		comparisons are based on the default t-test with a Cauchy (0, r =
		1/sqrt(2)) prior. The \"U\" in the Bayes factor denotes that it is uncorrected."

  if (options$bayesFactorType == "BF10") {
    bf.title <- "BF<sub>10, U</sub>"
    format   <- "sf:4;dp:3"#;log10"
  } else if (options$bayesFactorType == "BF01") {
    bf.title <- "BF<sub>01, U</sub>"
    format   <- "sf:4;dp:3"#;log10"
  } else if (options$bayesFactorType == "LogBF10") {
    bf.title <- "Log(BF<sub>10, U</sub>)"
    format   <- "sf:4;dp:3"
  }
  
  priorWidth <- 1 / sqrt(2)
  posthoc.variables <- unlist(options$postHocTestsVariables)
  if (model[["analysisType"]] == "RM-ANOVA") {
    dependent <- "dependent"
  } else {
    dependent <- options[["dependent"]]
  }

  for (posthoc.var in posthoc.variables) {

    postHocTable <- createJaspTable(title = paste0("Post Hoc Comparisons - ", posthoc.var))
    postHocCollection[[paste0("postHoc_", posthoc.var)]] <- postHocTable

    postHocTable$addColumnInfo(name = "(I)",            type = "string",                      title = "", combine=TRUE)
    postHocTable$addColumnInfo(name = "(J)",            type = "string",                      title = "")
    postHocTable$addColumnInfo(name = "Prior Odds",     type = "number", format = "sf:4;dp:3")
    postHocTable$addColumnInfo(name = "Posterior Odds", type = "number", format = "sf:4;dp:3")
    postHocTable$addColumnInfo(name = "BF",             type = "number", format = format,     title = bf.title)
    postHocTable$addColumnInfo(name = "error %",        type = "number", format = "sf:4;dp:3")

    postHocTable$addFootnote(symbol = "<em>Note.</em>", message = footnote)
    
    if (is.null(model$models)) {
      postHocTable$setExpectedRows(1L)
      next
    }

    fixed <- unlist(c(options$fixedFactors, sapply(options$repeatedMeasuresFactors, `[[`, "name")))
    if (model$analysisType == "RM-ANOVA" && posthoc.var %in% fixed && !posthoc.var %in% options$betweenSubjectFactors) {
      variable.levels <- options$repeatedMeasuresFactors[[which(lapply(options$repeatedMeasuresFactors, function(x) x$name) == posthoc.var)]]$levels
      paired <- TRUE
    } else if (posthoc.var %in% c(options$fixedFactors, options$betweenSubjectFactors, options$randomFactors)) {
      variable.levels <- levels(dataset[[.v(posthoc.var)]])
      paired <- FALSE
    } else {
      next
    }

    if (length(variable.levels) < 2L)
      next

    pairs <- utils::combn(variable.levels, 2)

    allSplits <- split(dataset[[.v(dependent)]], dataset[[.v(posthoc.var)]])

    for (i in 1:ncol(pairs)) {

      if (options$postHocTestsNullControl && !is.null(model$models)) {

        x <- na.omit(allSplits[[pairs[1L, i]]])
        y <- na.omit(allSplits[[pairs[2L, i]]])

        ttest <- try(BayesFactor::ttestBF(x = x, y = y, rscale = priorWidth, paired = paired), silent=TRUE)

        if (isTryError(ttest)) {

          priorOdds <- postOdds <- logBF <- error <- "NaN"
          message <- .extractErrorMessage(ttest)
          postHocTable$addFootnote(symbol = "<em>Note.</em>", message = message)

        } else {

          pH0 <- 0.5^(2 / length(variable.levels))
          logBF <- ttest@bayesFactor$bf
          if (options$bayesFactorType == "BF01") {
            priorOdds <- pH0 / (1 - pH0)
            logBF <- -logBF
          } else {
            priorOdds <- (1 - pH0) / pH0
          }

          postOdds <- log(priorOdds) + logBF
          postOdds <- exp(postOdds)
          if (options[["bayesFactorType"]] != "LogBF10")
            logBF <- exp(logBF)

          error <- ttest@bayesFactor$error * 100
        }
      }

      row <- list(
        "(I)"            = pairs[1L, i],
        "(J)"            = pairs[2L, i],
        "Prior Odds"     = priorOdds,
        "Posterior Odds" = postOdds,
        "BF"             = logBF,
        "error %"        = error
      )
      postHocTable$addRows(row)
    }
  }
  return()
}

# Data reading ----
.BANOVAreadData <- function(dataset, options, analysisType) {
  
  if (is.null(dataset)) {
    if (analysisType == "RM-ANOVA")
      return(.BANOVAreadRManovaData(dataset, options))
    
    numeric.vars <- c(unlist(options$covariates), unlist(options$dependent))
    numeric.vars <- numeric.vars[numeric.vars != ""]
    
    factor.vars <- c(unlist(options$fixedFactors), unlist(options$randomFactors))
    factor.vars <- factor.vars[factor.vars != ""]
    
    dataset <- .readDataSetToEnd(
      columns.as.numeric  = numeric.vars,
      columns.as.factor   = factor.vars,
      exclude.na.listwise = c(numeric.vars, factor.vars)
    )
  }
  
	return(dataset)
}

.BANOVAreadRManovaData <- function(dataset, options) {
  
  if (!("" %in% options$repeatedMeasuresCells)) {
    rm.vars <- options$repeatedMeasuresCells
    
    bs.factors <- options$betweenSubjectFactors
    bs.covariates <- options$covariates
    rm.factors <- options$repeatedMeasuresFactors
    all.variables <- c (bs.factors, bs.covariates, rm.vars)
    
    dataset <- .readDataSetToEnd(
      columns.as.numeric  = c (rm.vars, bs.covariates),
      columns.as.factor   = bs.factors,
      exclude.na.listwise = all.variables
    )
    dataset <- .shortToLong(dataset, rm.factors, rm.vars, c(bs.factors, bs.covariates))
    
    idx <- match(c("dependent", "subject"), colnames(dataset))
    colnames(dataset)[idx] <- .v(colnames(dataset)[idx])
    
    # options$dependent <- "dependent"
    # options$randomFactors <- "subject"
    # variable.names <- names(dataset)
    # i <- which(variable.names == "dependent")
    # j <- which(variable.names == "subject")
    # variable.names[i] <- .v("dependent")
    # variable.names[j] <- .v("subject")
    # names (dataset) <- variable.names
    # variable.names <- variable.names [-c (i,j)]
    
    # if (length(variable.names) > 0)
    # 	options$fixedFactors <- as.list(.unv(variable.names))
    # 
    # options$modelTerms[[length(options$modelTerms) + 1]] <- list(components = "subject", isNuisance = TRUE)
  } 
  return(dataset)
}

# Descriptives ----
.BANOVAdescriptives <- function(jaspResults, dataset, options, errors, analysisType) {
  
  # the main use of this function is that descriptives can now be reused for the frequentist ANOVAs
  # without the container, the position could mess things up
  descriptivesContainer <- jaspResults[["descriptivesContainer"]]
  if (is.null(descriptivesContainer)) {
    descriptivesContainer <- createJaspContainer()
    descriptivesContainer$dependOnOptions(c("dependent", "repeatedMeasuresCells"))
    descriptivesContainer$position <- 900 # always last
    jaspResults[["descriptivesContainer"]] <- descriptivesContainer
  }
  
  .BANOVAdescriptivesTable(descriptivesContainer, dataset, options, errors, analysisType)
  .BANOVAdescriptivesPlots(descriptivesContainer, dataset, options, errors, analysisType)
  return()
  
}

.BANOVAdescriptivesTable <- function(jaspResults, dataset, options, errors, analysisType) {

  if (!options[["descriptives"]] || !is.null(jaspResults[["tableDescriptives"]]))
    return()

  if (analysisType == "RM-ANOVA") {
    dependent <- "dependent"
    fixed <- unlist(c(lapply(options[["repeatedMeasuresFactors"]], `[[`, "name"), options[["betweenSubjectFactors"]]))
    title <- "Descriptives"
  } else {
    dependent <- options[["dependent"]]
    fixed <- options[["fixedFactors"]]
    if (!is.null(dependent) && dependent != "") {
      title <- paste("Descriptives - ", options$dependent, sep = "")
    } else {
      title <- "Descriptives"
    }
  }

  descriptivesTable <- createJaspTable(title = title)
  jaspResults[["tableDescriptives"]] <- descriptivesTable
  descriptivesTable$position <- 1
  descriptivesTable$dependOnOptions(c("dependent", "fixedFactors", "betweenSubjectFactors", "descriptives"))

  # internal names: change " " into "." because R does that to dataframe names. 
  # Add a . in case variable is "Mean", "SD" or "N" 
  nms <- paste0(gsub(pattern = " ", ".", fixed), ".")
  for (i in seq_along(fixed)) {
    descriptivesTable$addColumnInfo(name = nms[i], type = "string", title = fixed[i], combine = TRUE)
  }

  descriptivesTable$addColumnInfo(name = "Mean", type = "number", format = "sf:4;dp:3")
  descriptivesTable$addColumnInfo(name = "SD",   type = "number", format = "sf:4;dp:3")
  descriptivesTable$addColumnInfo(name = "N",    type = "number", format = "dp:0")

  if (errors$noVariables) {
    descriptivesTable$setExpectedRows(1L)
    return()
  }

  fixedB64     <- .v(fixed)
  dependentB64 <- .v(dependent)

  # by pasting the fixedFactors together we obtain the unique indices to group on. This excludes
  # non-existent combinations. A "." is added to deal with the level "".
  ind <- apply(dataset[, fixedB64, drop = FALSE], 1L, paste0, ".", collapse = "")

  # temporary function to calculate all descriptives
  tmpFun <- function(data, fixedB64, dependentB64) {

    row <- list()
    for (j in fixedB64)
      row[[paste0(.unv(j), ".")]] <- as.character(data[1L, j])

    N <- nrow(data)
    row[["N"]] <- N

    if (N == 0L) {

      row[["Mean"]] <- ""
      row[["SD"]]   <- ""

    } else if (N == 1L) {

      row[["Mean"]] <- data[[dependentB64]]
      row[["SD"]]   <- ""

    } else {

      row[["Mean"]] <- mean(data[[dependentB64]])
      row[["SD"]]   <- stats::sd(data[[dependentB64]])
    }
    return(row)
  }

  # apply tempFun on each subset defined by ind
  rows <- by(dataset, ind, tmpFun, fixedB64 = fixedB64, dependentB64 = dependentB64)
  
  # do.call(rbind, rows) turns rows into a data.frame (from a list) for jaspResults
  data <- do.call(rbind.data.frame, rows)
  
  descriptivesTable$setData(data)

  return()
}

.BANOVAdescriptivesPlots <- function(jaspResults, dataset, options, errors, analysisType) {

  if (length(options[["plotHorizontalAxis"]]) == 0L 
      || options[["plotHorizontalAxis"]] == "" 
      || !is.null(jaspResults[["containerDescriptivesPlots"]]))
    return()

  descriptivesPlotContainer <- createJaspContainer(title = "Descriptives plots")
  descriptivesPlotContainer$position <- 2
  jaspResults[["containerDescriptivesPlots"]] <- descriptivesPlotContainer

  # either Bayesian or Frequentist anova
  if (is.null(options$confidenceIntervalInterval)) { # TRUE implies Bayesian
    plotErrorBars <- options$plotCredibleInterval
    errorBarType  <- "confidenceInterval"
    conf.interval <- options$plotCredibleIntervalInterval
    descriptivesPlotContainer$dependOnOptions(c("dependent", "plotCredibleInterval", "plotCredibleIntervalInterval"))

  } else {
    plotErrorBars <- options$plotErrorBars
    errorBarType  <- "confidenceInterval"
    conf.interval <- options$confidenceIntervalInterval
  }

  descriptivesPlotContainer$setOptionMustContainDependency("plotHorizontalAxis", options$plotHorizontalAxis)
  descriptivesPlotContainer$setOptionMustContainDependency("plotSeparateLines",  options$plotSeparateLines)
  descriptivesPlotContainer$setOptionMustContainDependency("plotSeparatePlots",  options$plotSeparatePlots)

  if (errors$noVariables) { 
    descriptivesPlotContainer[["dummyplot"]] <- createJaspPlot(title = "Descriptives Plot")
    return()
  }

  groupVars <- c(options$plotHorizontalAxis, options$plotSeparateLines, options$plotSeparatePlots)
  groupVars <- groupVars[groupVars != ""]
  groupVarsV <- .v(groupVars)
  dependentV <- .v(options$dependent)
  if (analysisType == "RM-ANOVA")
    dependentV <- .v("dependent")

  summaryStat <- .summarySE(as.data.frame(dataset), measurevar = dependentV, groupvars = groupVarsV,
                            conf.interval = conf.interval, na.rm = TRUE, .drop = FALSE,
                            errorBarType = errorBarType)

  colnames(summaryStat)[colnames(summaryStat) == dependentV] <- "dependent"

  if (options$plotHorizontalAxis != "") {
    colnames(summaryStat)[colnames(summaryStat) == .v(options$plotHorizontalAxis)] <- "plotHorizontalAxis"
  }

  if (options$plotSeparateLines != "") {
    colnames(summaryStat)[colnames(summaryStat) == .v(options$plotSeparateLines)] <- "plotSeparateLines"
  }

  if (options$plotSeparatePlots != "") {
    colnames(summaryStat)[colnames(summaryStat) == .v(options$plotSeparatePlots)] <- "plotSeparatePlots"
  }

  base_breaks_x <- function(x){
    b <- unique(as.numeric(x))
    d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
    list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1))
  }

  base_breaks_y <- function(x, plotErrorBars){
    if (plotErrorBars) {
      ci.pos <- c(x[,"dependent"], x[,"dependent"]-x[,"ci"],x[,"dependent"]+x[,"ci"])
      b <- pretty(ci.pos)
      d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
      list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
           ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
    } else {
      b <- pretty(x[,"dependent"])
      d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
      list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
           ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
    }
  }

  if (options$plotSeparatePlots != "") {
    subsetPlots <- levels(summaryStat[,"plotSeparatePlots"])
    nPlots <- length(subsetPlots)
  } else {
    nPlots <- 1
  }

  for (i in seq_len(nPlots)) {
    
    if (nPlots > 1L) {
      title <- paste(options$plotSeparatePlots,": ",subsetPlots[i], sep = "")
    } else {
      title <- "Descriptives Plot"
    }
    descriptivesPlot <- createJaspPlot(title = title)
    descriptivesPlot$copyDependenciesFromJaspObject(descriptivesPlotContainer)
    descriptivesPlotContainer[[title]] <- descriptivesPlot
    
    if (options$plotSeparateLines != "") {
      descriptivesPlot$width  <- options$plotWidthDescriptivesPlotLegend
      descriptivesPlot$height <- options$plotHeightDescriptivesPlotLegend
    } else {
      descriptivesPlot$width  <- options$plotWidthDescriptivesPlotNoLegend
      descriptivesPlot$height <- options$plotHeightDescriptivesPlotNoLegend
    }
    
    if (options$plotSeparatePlots != "") {
      summaryStatSubset <- subset(summaryStat,summaryStat[,"plotSeparatePlots"] == subsetPlots[i])
    } else {
      summaryStatSubset <- summaryStat
    }
    
    if(options$plotSeparateLines == "") {
      
      p <- ggplot2::ggplot(summaryStatSubset, ggplot2::aes(x=plotHorizontalAxis,
                                                           y=dependent,
                                                           group=1))
      
    } else {
      
      p <- ggplot2::ggplot(summaryStatSubset, ggplot2::aes(x=plotHorizontalAxis,
                                                           y=dependent,
                                                           group=plotSeparateLines,
                                                           shape=plotSeparateLines,
                                                           fill=plotSeparateLines))
      
    }
    
    if (plotErrorBars) {
      
      pd <- ggplot2::position_dodge(.2)
      p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower,
                                                  ymax=ciUpper),
                                     colour="black", width=.2, position=pd)
      
    } else {
      
      pd <- ggplot2::position_dodge(0)
      
    }
    
    guideLegend <- ggplot2::guide_legend(nrow = min(10, nlevels(summaryStatSubset$plotSeparateLines)), title = options$plotSeparateLines, keywidth = 0.1, keyheight = 0.3, default.unit = "inch")

    p <- p + ggplot2::geom_line(position=pd, size = .7) +
      ggplot2::geom_point(position=pd, size=4) +
      ggplot2::scale_fill_manual(values = c(rep(c("white","black"),5),rep("grey",100)), guide=guideLegend) +
      ggplot2::scale_shape_manual(values = c(rep(c(21:25),each=2),21:25,7:14,33:112), guide=guideLegend) +
      ggplot2::scale_color_manual(values = rep("black",200),guide=guideLegend) +
      ggplot2::ylab(options$dependent) +
      ggplot2::xlab(options$plotHorizontalAxis) +
      base_breaks_y(summaryStat, plotErrorBars) +
      base_breaks_x(summaryStatSubset[,"plotHorizontalAxis"])
    
    p <- JASPgraphs::themeJasp(p, legend.position = "right")
    
    descriptivesPlot$plotObject <- p
  }
  return()
}


# Sample posteriors ----
.BANOVAsamplePosteriors <- function(jaspResults, dataset, options, model, state) {

  # TODO: the density approximation can become more efficient with a fast parametric density approximation
  # TODO: Bayes factor samples unobserved interaction levels, what to do?

  # if the most complex model is retrieved from the state?
  nIter <- if (options[["sampleModeMCMC"]] == "auto") 1e4L else options[["fixedMCMCSamples"]]
  nmodels    <- length(model[["models"]])
  postProbs  <- model[["postProbs"]]
  statistics <- vector("list", nmodels)
  
  levelInfo  <- .BANOVAgetLevelInfo(dataset, model[["model.list"]][[nmodels]])

  renameFrom <- renameTo <- NULL
  if (!is.null(state)) { # can we reuse some posteriors?
    reuseable <- model[["reuseable"]]

    if (model[["analysisType"]] == "RM-ANOVA") {
      # it's possible that a user just renames a level of a repeated measures factor
      # in that case everything can be reused, but we have to rename some parameters.

      oldLevelInfo <- state[["levelInfo"]]$levelNames
      newLevelInfo <- levelInfo$levelNames

      if (!identical(newLevelInfo, oldLevelInfo)) {
        for (nm in intersect(names(oldLevelInfo), names(newLevelInfo))) {
          idx <- !(oldLevelInfo[[nm]] %in% newLevelInfo[[nm]])
          renameFrom <- c(renameFrom, oldLevelInfo[[nm]][idx])
          renameTo   <- c(renameTo,   newLevelInfo[[nm]][idx])
        }
      }
    }
  } else {
    reuseable <- rep(NA, nmodels)
  }

  # NOTE: some code checks require saving all samples. To do so, change all samples to samples[[i]] and uncomment:
  # samples <- vector("list", nmodels)

  allParamNames <- c("mu", unlist(levelInfo$levelNames))
  nparam <- length(allParamNames)
  weightedMeans <- weights <- numeric(nparam)
  names(weights) <- names(weightedMeans) <- allParamNames
  allContinuous <- TRUE

  jaspResults$startProgressbar(nmodels)
  for (i in seq_len(nmodels)) {
    if (is.na(reuseable[i])) {

     if (i == 1L && is.null(model$models[[i]]$bf)) {
        
        # NULL model only contains an intercept, use custom sampler
        # NOTE: RM-ANOVA never enters here (and would crash if it did)
        samples <- .BANOVAsampleNullModel(dataset[[.v(options$dependent)]], nsamples = nIter)
        types <- NULL

      } else {

        # NOTE: we have to sample the random effects, otherwise we cant make predictions (needed for residuals and R^2)
        # put the dataset back in
        bfObj <- model$models[[i]][[3L]]
        bfObj@data <- dataset
        samples <- BayesFactor::posterior(bfObj, iterations = nIter)
        types <- samples@model@dataTypes

        # BayesFactor stores an internal copy of the dataset, so it can still have old names
        if (length(renameFrom) > 0) {
          cnms <- colnames(samples)
          cnms <- plyr::mapvalues(cnms, renameFrom, renameTo, warn_missing = FALSE) # NOTE: dependency could be removed
          colnames(samples) <- cnms
        }

        # keep only relevant columns, drop sig2, g_xxx, ...
        idx <- match(allParamNames, colnames(samples), nomatch = 0L)
        samples <- samples[, idx, drop = FALSE]

        # for some odd reason, Bayesfactor uses as column name contcor1-contcor1
        # if there is a covariate AND fixed factors, but only contcor1 if all variables are continuous...
        if (all(types == "continuous")) {
          cnms <- colnames(samples)[-1L] # omit the intercept (mu) which is not changed by Bayesfactor
          colnames(samples)[-1L] <- paste0(cnms, "-", cnms)
        } else {
          allContinuous <- FALSE
        }
      }

      # although matrixStats::colMeans2 is faster than .colMeans the cost of matrixStats:: is not worth it.
      nms <- colnames(samples)
      statistics[[i]]$names  <- nms # <- these are the names for all objects within one sublist
      statistics[[i]]$mean   <- .colMeans                (samples, m = nIter, n = NCOL(samples))
      statistics[[i]]$var    <- matrixStats::colVars     (samples)
      statistics[[i]]$cri    <- matrixStats::colQuantiles(samples, probs = c(0.025, 0.975))
      
      statistics[[i]]$approx <- .BANOVAfitDensity(samples = samples)
      statistics[[i]]$types  <- types

    } else { # reuse state
      statistics[[i]] <- state$statistics[[reuseable[i]]]
      nms             <- statistics[[i]]$names
      types           <- statistics[[i]]$types
      if (length(renameFrom) > 0) {
        nms <- plyr::mapvalues(nms, renameFrom, renameTo, warn_missing = FALSE) # NOTE: dependency could be removed
        statistics[[i]]$names <- nms
      }
      if (allContinuous)
        allContinuous <- all(types == "continuous")
    }

    # compute model averaged posterior means
    weightedMeans[nms] <- weightedMeans[nms] + postProbs[i] * statistics[[i]]$mean
    weights      [nms] <- weights      [nms] + postProbs[i]
    jaspResults$progressbarTick()
  }

  # find out which parameters are random -- this uses types from the last iteration above
  isRandom <- logical(nparam)
  idx <- which(types == "random")
  for (i in idx)
    isRandom <- isRandom | startsWith(names(weights), names(types)[i])

  # the weights used above don't sum to 1 because we consider a subset of the models. 
  # Now we renormalize to ensure the weights used in each weighted mean do sum to 1. 
  weightedMeans <- weightedMeans / weights

  # given the model averaged posterior means calculate the weighted posterior standard deviations
  weightedSds <- 0 * weightedMeans # keeps the names
  r <- nIter / (nIter - 1)
  for (i in seq_len(nmodels)) {
    nms <- statistics[[i]]$names

    var <- statistics[[i]]$var # ~ sum((x - mean(x))^2
    mu  <- statistics[[i]]$mean

    # cc = sum((x - y)^2) - sum((x - mean(x))^2), where y is the weighted mean
    # hence, we can get the weighted sum of squares from the individual posterior variances
    cc <- weightedMeans[nms]^2 + mu^2 - 2 * mu * weightedMeans[nms]
    weightedSds[nms] <- weightedSds[nms] + postProbs[i] * (var + cc * r)

  }
  weightedSds <- sqrt(weightedSds / weights)

  # the loops above are optimized versions of the code below that also calculates the 
  # weighted mean and weighted sd, but needs to store all samples at once.
  # xx <- lapply(samples, function(x, y) {
  #   if (y %in% colnames(x)) {dd <- model$posteriors$weightedRsqDens
  #     x[, y]
  #   } else {
  #     NULL
  #   }
  # }, y = "XY29udEJpbm9t-1") # XY29udEJpbm9t = contBinom
  # idx <- lengths(xx) > 0
  # xx <- unlist(xx[idx])
  # w <- postProbs[idx]
  # sum(w) # equals weights[2]
  # weighted.mean(xx,  rep(w / sum(w), each = nIter)) # equals weightedMeans[2]
  # sqrt(Hmisc::wtd.var(xx, rep(w / sum(w), each = nIter), method = "unbiased")) # equals weightedSds[2]
  
  # compute model averaged densities
  steps <- 2^9 # grid size for densities is 2^steps
  weightedDensities <- array(0, dim = c(steps, nparam, 2), dimnames = list(NULL, names(weightedMeans), c("x", "y")))
  ranges <- matrix(0, nparam, 2L, dimnames = list(allParamNames, NULL))

  # get the outermost x-values for each densities this could be vectorized with matrixStats::rowRanges if the 
  # data are stored as a matrix but that is memory inefficient
  for (i in seq_len(nmodels)) {
    nms <- statistics[[i]]$names
    indices <- which(nms %in% allParamNames)
    for (j in indices) {
      ranges[j, ] <- range(ranges[j, ], statistics[[i]]$approx$xRanges[j, ])
    }
  }

  # construct one common grid for each observed density
  for (i in seq_len(nparam)) {
    weightedDensities[, i, 1L] <- seq(ranges[i, 1], ranges[i, 2], length.out = steps)
  }

  for (i in seq_len(nmodels)) {
    nms <- statistics[[i]]$names
    ind <- match(nms, allParamNames)
    for (j in seq_along(ind)) {
      # approximate all distributions on a common grid
      ap <- approx(x    = statistics[[i]]$approx$fit[, j,      1L], 
                   y    = statistics[[i]]$approx$fit[, j,      2L],
                   xout = weightedDensities         [, ind[j], 1L], 
                   # not observed is approximated to 0
                   yleft = 0, yright = 0)
      
      weightedDensities[, ind[j], 2L] <- weightedDensities[, ind[j], 2L] + ap$y * postProbs[i]
      
    }
  }
  # postProbs don't sum to one so we renormalize the densities
  weightedDensities[, , 2L] <- sweep(weightedDensities[, , 2L], 2, weights, FUN = `/`)

  # compute weighted CRIs
  weightedCRIs <- matrix(NA, nparam, 2L, dimnames = list(names(weights), NULL))
  for (i in seq_len(nparam)) {
    weightedCRIs[i, ] <- .BANOVAapproxCRI(weightedDensities[, i, 1L], weightedDensities[, i, 2L], 0.95)
  }

  # compute residuals and r-squared
  # sample from the joint posterior over models and parameters
  tmp  <- .BANOVAgetSMIResidRsq(weightedDensities, dataset, model$model.list[[nmodels]], nIter, weights)
  means  <- rowMeans(tmp$resid)
  quants <- matrixStats::rowQuantiles(tmp$resid, probs = c(0.025, 0.975))

  # the code above is equivalent to the code below, but the code below needs to keep all posterior samples of 
  # all models in memory.
  # weights <- rep(postProbs, each = nIter)
  # independentVariable <- all.vars(.BANOVAgetModelFormulaFromBFobj(model$models[[2L]]))[1L]
  # resids <- matrix(NA, nrow(dataset), 0)
  # rsq <- vector("list", nmodels)
  # # get residuals of all models individually
  # for (i in seq_len(nmodels)) {
  #   if (is.null(model$models[[i]]$bf)) {
  #     tmp2    <- .BANOVAresidualsNullModel(nIter, dataset[[independentVariable]])
  #   } else {
  #     tmp2 <- .BANOVAgetSMIResidRsq(
  #       posterior = samples[[i]],
  #       dataset   = dataset,
  #       formula   = .BANOVAgetModelFormulaFromBFobj(model$models[[i]])
  #     )
  #   }
  #   resids <- cbind(resids, tmp2$resid)
  #   rsq[[i]] <- tmp2$rsq
  # }
  # # compute weighted mean for each row
  # means2 <- tcrossprod(weights / nIter, resids)
  # plot(means, means2); abline(0, 1)
  # quants2 <- apply(resids, 1L, Hmisc::wtd.quantile, weights = weights, probs = c(0.025, 0.975))
  # plot(quants[, 1], quants2[1, ]); abline(0, 1)
  # plot(quants[, 2], quants2[2, ]); abline(0, 1)

  # all information for q-q plot of residuals
  weightedResidSumStats <- matrix(c(means, quants), nrow = length(means), ncol = 3L, 
         dimnames = list(NULL, c("mean", "cri.2.5%", "cri.97.5%")))

  # all information for r-squared density plot
  weightedRsqDens <- density(tmp$rsq, n = 2^11, from = 0, to = 1)
  weightedRsqCri <- quantile(tmp$rsq, probs   = c(0.025, 0.975))

  return(list(
    statistics = statistics, weights = weights, weightedCRIs = weightedCRIs,
    weightedMeans = weightedMeans, weightedSds = weightedSds, weightedDensities = weightedDensities,
    weightedResidSumStats = weightedResidSumStats, weightedRsqDens = weightedRsqDens, weightedRsqCri = weightedRsqCri,
    allContinuous = allContinuous, isRandom = isRandom, levelInfo = levelInfo)
  )
}

.BANOVAsampleNullModel <- function(nsamples, dependent) {

  # sample from posterior under NULL model, needed to compute model averaged residuals.

  rt.scaled <- function (n, df, mean = 0, sd = 1, ncp) {
    mean + sd * stats::rt(n, df, ncp = ncp)
  }

  # sample from the marginal posterior distribution of the mean t-distribution, based on Murphy (2007)
  n      <- length(dependent)
  muObs  <- mean(dependent)
  varObs <- var(dependent)

  # uninformative priors
  k0  <- 1e-6
  a0  <- 1e-5
  b0  <- 1e-5

  an  <- a0 + n / 2
  bn  <- b0 + 0.5 * (n - 1L) * varObs + k0 * n * muObs / (2 * (k0 + n))
  kn  <- k0 + n
  mun <- n * muObs / kn

  samples <- matrix(rt.scaled(n = nsamples, df = 2 * an, mean = mun, sd = bn / (an * kn), ncp = 0), nsamples,
                    1L, dimnames = list(NULL, "mu"))
  return(samples)
}

.BANOVAresidualsNullModel <- function(nsamples, dependent) {

  # sample from posterior under NULL model, needed to compute model averaged residuals.

  rt.scaled <- function (n, df, mean = 0, sd = 1, ncp) {
    mean + sd * stats::rt(n, df, ncp = ncp)
  }

  # sample from the marginal posterior distribution of the mean t-distribution, based on Murphy (2007)
  n      <- length(dependent)
  muObs  <- mean(dependent)
  varObs <- var(dependent)

  # uninformative priors
  k0  <- 1e-6
  a0  <- 1e-5
  b0  <- 1e-5

  an  <- a0 + n / 2
  bn  <- b0 + 0.5 * (n - 1L) * varObs + k0 * n * muObs / (2 * (k0 + n))
  kn  <- k0 + n
  mun <- n * muObs / kn

  samples <- rt.scaled(n = nsamples, df = 2 * an, mean = mun, sd = bn / (an * kn), ncp = 0)

  # compute all pairwise differences (residuals)
  preds <- tcrossprod(rep(1, n), samples)

  resids <- dependent - preds

  rsq <- .BANOVAcomputeRsq(dependent, preds)

  return(list(resids = resids, rsq = rsq))

}

.BANOVAfitDensity <- function(samples, gridsize = 100L, getXRange = TRUE) {
  
  # ideally we don't do kernel density estimation but instead use some parametric approximation that is suited
  # for unimodal distributions. It should also be fast, e.g., fit using method of moments (or another analytic method)
  nc <- ncol(samples)
  fits  <- array(
    data     = NA, 
    dim      = c(gridsize, nc, 2), 
    dimnames = list(NULL, colnames(samples), c("x", "y"))
  )
  
  for (i in seq_len(nc)) {
    fits[, i, ] <- do.call(cbind, KernSmooth::bkde(samples[, i], gridsize = gridsize))
  }
  if (getXRange) {
    if (nc == 1L) {
      xRanges <- matrix(range(fits[, , "x"]), 1L, 2L)
    } else {
      xRanges <- matrixStats::colRanges(fits[, , "x"])
    }
    rownames(xRanges) <- colnames(samples)
  } else {
    xRanges <- NULL
  }
  return(list(fit = fits, xRanges = xRanges))
}

.BANOVAapproxCRI <- function(x, y, cri = 0.95) {
  
  # approximate cdf
  y2 <- cumsum(y)
  y2 <- y2 / y2[length(y2)]
  
  h <- (1 - cri) / 2 
  # find closest observed match
  idx <- c(
    which.min(abs(y2 - h)), 
    which.min(abs(y2 - 1 + h))
  )
  return(x[idx])
}

# plot posteriors ----
.BANOVAgetBMAdensity <- function(samples, weights, fromTo = NULL, n = 2^10) {

  # @param samples, list of samples
  # @param weigths, vector of numeric weights
  # @param fromTo vector of length 2 specifying lower and upper bound (optional).
  # @return a list with $x the x-coordinates and $y the y-coordinates.

  if (length(samples) != length(weights))
    stop("length of samples must be equal to length of weights!")

  # remove NULL indices
  idxNonNull <- lengths(samples) > 0
  weights <- weights[idxNonNull]
  samples <- samples[idxNonNull]

  # renormalize the weights so output is a proper density function (TODO: do we want this, or BAS style?)
  weights <- weights / sum(weights)

  # create x-grid for density
  if (is.null(fromTo))
    fromTo <- range(sapply(samples, range))
  xs <- seq(fromTo[1L], fromTo[2L], length.out = 2^10)

  # compute weighted density
  ys <- numeric(n)
  for (i in seq_along(samples)) {
    ys <- ys + weights[i] * density(samples[[i]], from = fromTo[1L], to = fromTo[2L], n = n)$y
  }

  return(list(x = xs, y = ys))
}

.BANOVAreSample <- function(n, x, y, prop0 = NULL) {

  cdf <- cumsum(y)
  cdf <- cdf / cdf[length(cdf)]
  if (!is.null(prop0) && prop0 <= (1 - sqrt(.Machine$double.eps))) {
    i1 <- runif(n) <= prop0
    samples <- numeric(n)
    samples[i1] <- approx(cdf, x, runif(sum(i1)), rule = 2)$y
  } else {
    samples <- approx(cdf, x, runif(n), rule = 2)$y
  }
  return(samples)
}

.BANOVAgetSMIResidRsq <- function(posterior, dataset, formula, nIter, prop0) {

  # @param posterior  object from Bayesfactor package, or SxPx2 array of weighted densities
  # @param dataset    dataset
  # @param formula    the formula for this specific model. Supply the formula of the most complex model for BMA inference.
  # @param nIter      number of posterior samples
  #       
  # @return           a list with residuals, predictions, and r-squared

  # @details the matrix multiplication in this function allocates an array of nobs * nsamples, which can be enormous.
  # if is more memory efficient to use for loops (but slower in R) to calculate predictions for each observation
  # and posterior sample. This could be done in the future if performance is an issue. However, this likely cannot be
  # done efficiently in R.

  # here we need the data in a one-hot encoded way
  # first one is dependent variable, rest are independent variables
  dvs <- all.vars(formula)[1L]
  ivs <- all.vars(formula)[-1L]

  # idx contains variables that need to be one-hot encoded
  idx <- sapply(dataset[ivs], is.factor)
  # idx <- isFactor & names(isFactor) %in% dvs

  # do one-hot encoding (thank base R for calling a function `contrast` and giving it the argument `contrast`...)
  datOneHot <- model.matrix(formula, data = dataset[c(dvs, ivs)],
                            contrasts.arg = lapply(dataset[ivs][idx], contrasts, contrasts = FALSE))

  if (length(dim(posterior)) == 3L) {
    # we're doing model averaged inference
    
    # sample from the BMA posterior
    samples <- matrix(NA, nrow = nIter, ncol = ncol(posterior))
    for (i in seq_len(ncol(posterior))) {
      samples[, i] <- .BANOVAreSample(n = nIter, x = posterior[, i, "x"], y = posterior[, i, "y"], prop0 = prop0[i])
    }

    preds <- tcrossprod(datOneHot, samples)

  } else {
    # TODO: ensure that column order is always correct for both!
    # otherwise we're doing inference for a single model
    preds <- tcrossprod(datOneHot, posterior)
  }

  # calculate residuals (correctly recycles dat[[dvs]])
  resid <- dataset[[dvs]] - preds

  rsq <- .BANOVAcomputeRsq(dataset[[dvs]], preds)
  # eta <- .BANOVAcomputeEtasq(dat[[dvs]], preds)

  return(list(resid = resid, preds = preds, rsq = rsq))
}


# HF computation ----
.BANOVAinitBayesFactor <- function() {

  defaults <- list(
    BFMaxModels         = 50000,
    BFpretestIterations = 100,
    BFapproxOptimizer   = "optim",
    BFapproxLimits      = c(-15, 15),
    BFprogress          = interactive(),
    BFfactorsMax        = 5
  )
  idx <- setdiff(names(defaults), names(options()))
  options(defaults[idx])

}

.BANOVAcomputeRsq <- function(obs, preds) {

  # NOTE: R^2 != cor(obs, predict) because the predictions from the posterior samples are not OLS estimates. 
  if (is.null(dim(preds)))
    preds <- matrix(preds, ncol = 1L)

  # definition from http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2.pdf
  ee <- matrixStats::colVars(preds)
  ff <- matrixStats::colVars(obs - preds)
  gg <- ee / (ee + ff)
  return(gg)

  # mu <- mean(obs)
  # n <- length(obs)
  # sstot <- var(obs)
  # ssreg <- colMeans((preds - mu)^2) * (n / (n - 1))
  # return(ssreg / sstot)
}

.BANOVAcomputeEtasq <- function(obs, preds) {
  
  # partial eta^2
  if (is.null(dim(preds)))
    preds <- matrix(preds, ncol = 1L)

  # return(1 - matrixStats::colVars(preds - obs) / var(obs))
  # return(1 - matrixStats::rowVars(preds - obs) / var(obs))
  return(matrixStats::colVars(preds) / var(obs))

}
# HF formulas ----
.BANOVAgetFormulaComponents <- function(x, what = c("components", "variables")) {
  what <- match.arg(what)
  if (what == "components") {
    return(colnames(attr(terms(x), "factors")))
  } else {
    return(all.vars(x)[-1L])
  }
}

.BANOVAgenerateAllModelFormulas <- function(formula, nuisance = NULL) {

  neverExclude <- paste("^", nuisance, "$", sep = "")
  out <- try(
    BayesFactor::enumerateGeneralModels(formula, whichModels = "withmain", neverExclude = neverExclude),
		silent = TRUE)

	if (isTryError(out))
	  .quitAnalysis("An unknown error occured in BayesFactor::enumerateGeneralModels.")

  if (is.null(nuisance)) {
    return(c(list(NULL), out))
  } else {
    # put the null-model first
    i <- length(out)
    return(c(out[[i]], out[-i]))
  }
}

.BANOVAcreateModelFormula <- function(dependent, modelTerms) {

  model.formula <- paste(.v(dependent), " ~ ", sep = "")
	nuisance <- NULL
	effects <- NULL
	for (term in modelTerms) {
		if (is.null (effects) & is.null (nuisance)){
			model.formula <- paste0(model.formula,
				paste(.v(term$components), collapse = ":"))
		} else {
			model.formula <- paste0(model.formula, " + ",
				paste(.v(term$components), collapse = ":"))
		}
		if (!is.null(term$isNuisance) && term$isNuisance) {
			nuisance <- c(nuisance, paste(.v(term$components), collapse = ":"))
		} else {
			effects <- c(effects, paste(.v(term$components), collapse = ":"))
		}
	}
	model.formula <- formula (model.formula)
  return(list(model.formula = model.formula, nuisance = nuisance, effects = effects))
}

.BANOVAgetModelFormulaFromBFobj <- function(BayesFactorObj, asCharacter = FALSE) {
  out <- BayesFactorObj$bf@numerator[[1L]]@identifier$formula
  if (asCharacter) {
    return(out)
  } else {
    return(as.formula(out))
  }
}

.BANOVAgetLevelInfo <- function(dataset, formula) {

  # we use the components by splitting the formula rather than those in options$modelTerms
  # the reason is that the BayesFactor::enumerateGeneralModels swaps the order of interaction effects
  # depending on whether a variable is nuisance or not
  nms <- attr(stats::terms(formula), "term.labels")
  components <- strsplit(nms, ":")

  # compute how many levels each predictor has. Returns 1 for continuous predictors.
  levls <- lapply(dataset, levels)

  obsLevelCounts <- lengths(levls) # counts of the factors in the data
  levelNames     <- vector("list", length(components)) # for each parameter, all levels
  names(levelNames) <- nms

  for (i in seq_along(components)) {
    idx <- components[[i]]
    # exclude any non-factors
    idx <- idx[obsLevelCounts[idx] != 0L]

    if (length(idx) > 0) { # we're dealing with factors
      tmp <- unique(dataset[idx])
      # ensure order is alphabetical, rather than in order of appearance.
      tmp <- tmp[do.call(order, tmp), , drop = FALSE] 
      combs <- apply(tmp, 1, paste, collapse = ".&.") # all _observed_ combinations of factors
      levelNames[[i]] <- paste0(nms[i], "-", combs)
    } else {
      # continuous variables get their name-name, because Bayesfactor returns it like that
      levelNames[[i]] <- paste0(nms[i], "-", nms[i])
    }
  }
  levelNames <- levelNames[lengths(levelNames) > 0]
  levelCounts <- lengths(levelNames) # counts of the factors including interaction terms
  return(list(levelCounts = levelCounts, levelNames = levelNames))
}

.BANOVAgetLevelsFromParamNames <- function(names) {
  
  # NOTE: this works because base64 does not contain "-"
  # split on first "-"; 2 implies output of length 2, i.e., only split once
  out <- do.call(rbind, stringr::str_split(names, "-", 2L))
  
  # continous variables have as level name the variable name
  idx <- out[, 1L] == out[, 2L]
  out[idx, 2L] <- ""
  
  # change dots into spaces for aesthetic purposes
	out[, 2L] <- gsub(".", " ", out[, 2L], fixed = TRUE)
	colnames(out) <- c("parameter", "level")
  return(out)
	
}

# .BANOVAas.character.formula <- function(x, ...) {
#   # we could also extend the S3 function as.character
#   Reduce(paste, trimws(deparse(x)))
# }

.BANOVAreorderFormulas <- function(x) {
  
  # This function reorders the terms of a formula such that they are alphabetical
  # e.g., a ~ c + b + c:b becomes a ~ b + b:c
  # This is necessary because BayesFactor::enumerateGeneralModels always appends the nuisance terms
  # and a ~ b + c != a ~ c + b
  # so without this function the state does not get reused whenever a user modifies the nuisance terms
  if (is.null(x))
    return("NULL")

  s <- strsplit(attr(stats::terms.formula(x), "term.labels"), ":") 
  for (i in which(lengths(s) > 1L))
    s[[i]] <- sort(s[[i]])
  return(paste(all.vars(x)[1L], "~", paste(sort(unlist(s)), collapse = " + ")))
}

# Single Model Inference (SMI) ----
.BANOVAsmi <- function(jaspResults, dataset, options, model) {

  # TODO: allCOntinuous is missing in singleModel
  # checks of .BANOVAsmiQqPlot and others are wrong
  userWantsSMI <- options$singleModelPosteriorPlot || options$singleModelqqPlot || options$singleModelrsqPlot ||
    options$singleModelEffects
  if (!(options[["singleModelEffects"]] || userWantsSMI))
    return()

  if (!is.null(jaspResults[["collectionSingleModel"]])) {
    singleModelContainer <- jaspResults[["containerSingleModel"]]
  } else {
    singleModelContainer <- createJaspContainer(title = "Single Model Inference")
    singleModelContainer$dependOnOptions(c(
      "singleModelTerms", "dependent", "sampleModeMCMC", "fixedMCMCSamples", "priorCovariatesEffects", 
      "priorFixedEffects", "priorRandomEffects", "repeatedMeasuresCells"
    ))
    jaspResults[["containerSingleModel"]] <- singleModelContainer
    singleModelContainer$position <- 8
  }

  singleModel <- jaspResults[["singleModelState"]]$object
  if (is.null(singleModel) && length(options$singleModelTerms) > 0L && userWantsSMI) {
    singleModel <- try(.BANOVAsmiSamplePosterior(dataset, options, model[["analysisType"]]))
    if (isTryError(singleModel)) {
      # singleModelContainer$setError(paste("Error in single model inference:", .extractErrorMessage(singleModel)))
      singleModel <- NULL
    } else {
      singleModelState <- createJaspState(object = singleModel)
      singleModelState$copyDependenciesFromJaspObject(singleModelContainer)
      jaspResults[["singleModelState"]] <- singleModelState
    }
  }

  .BANOVAsmiEffects(singleModelContainer, options, singleModel)
  .BANOVAsmiQqPlot (singleModelContainer, options, singleModel)
  .BANOVAsmiRsqPlot(singleModelContainer, options, singleModel)
  
  .BANOVAsmiPosteriorPlot(singleModelContainer, dataset, options, singleModel)

  return()

}

.BANOVAsmiSamplePosterior <- function(dataset, options, analysisType) {

  nIter <- if (options[["sampleModeMCMC"]] == "auto") 1e3L else options[["fixedMCMCSamples"]]
  modelTerms <- options$singleModelTerms

  dependent     <- options$dependent
  randomFactors <- unlist(options$randomFactors)
  rscaleFixed   <- options$priorFixedEffects
	rscaleRandom  <- options$priorRandomEffects

  if (analysisType == "RM-ANOVA") {
    dependent <- "dependent"
    rscaleCont <- options[["priorCovariates"]]
    randomFactors <- "subject"
    modelTerms[[length(modelTerms) + 1L]] <- list(components = "subject", isNuisance = TRUE)
  } else if (analysisType == "ANCOVA") {
    rscaleCont <- options[["priorCovariates"]]
  } else {
    rscaleCont <- "medium" # sqrt(2)/4
  }

  formula <- .BANOVAcreateModelFormula(dependent, modelTerms)$model.formula
  levelInfo  <- .BANOVAgetLevelInfo(dataset, formula)
  allParamNames <- c("mu", unlist(levelInfo$levelNames))

  samples <- BayesFactor::lmBF(
    formula      = formula,
    data         = dataset,
    whichRandom  = .v(unlist(randomFactors)),
    progress     = TRUE,
    posterior    = TRUE,
    rscaleFixed  = rscaleFixed,
    rscaleRandom = rscaleRandom,
    rscaleCont   = rscaleCont,
    iterations   = nIter
  )

  types <- samples@model@dataTypes

  # keep only relevant columns, drop sig2, g_xxx, ...
  idx <- match(allParamNames, colnames(samples), nomatch = 0L)
  samples <- samples[, idx, drop = FALSE]

  # for some odd reason, Bayesfactor uses as column name contcor1-contcor1
  # if there is a covariate AND fixed factors, but only contcor1 if all variables are continuous...
  allContinuous <- all(types == "continuous")
  if (allContinuous) {
    cnms <- colnames(samples)[-1L] # omit the intercept (mu) which is not changed by Bayesfactor
    colnames(samples)[-1L] <- paste0(cnms, "-", cnms)
  }

  means <- colMeans(samples)
  sds <- matrixStats::colSds(samples)
  names(means) <- names(sds) <- colnames(samples)
  
  h <- (1 - 0.95) / 2
  cri <- matrixStats::colQuantiles(samples, probs = c(h, 1 - h))

  densities <- .BANOVAfitDensity(samples, 2^9, FALSE)

  tmp  <- .BANOVAgetSMIResidRsq(samples, dataset, formula, nIter)
  residmeans  <- rowMeans(tmp$resid)
  residquants <- matrixStats::rowQuantiles(tmp$resid, probs = c(0.025, 0.975))

  # all information for q-q plot of residuals
  residSumStats <- matrix(c(residmeans, residquants), nrow = length(residmeans), ncol = 3L, 
         dimnames = list(NULL, c("mean", "cri.2.5%", "cri.97.5%")))

  # all information for r-squared density plot
  rsqDens <- density(tmp$rsq, n = 2^11, from = 0, to = 1)
  rsqCri <- quantile(tmp$rsq, probs   = c(0.025, 0.975))

  return(list(
    means = means, sds = sds, CRIs = cri, densities = densities$fit,
    residSumStats = residSumStats, rsqDens = rsqDens, rsqCri = rsqCri,
    allContinuous = allContinuous
  ))
}

.BANOVAsmiEffects <- function(jaspContainer, options, model) {

  if (!is.null(jaspContainer[["SMItablePosteriorEstimates"]]) || !options[["singleModelEffects"]])
    return()

  estsTable <- createJaspTable(title = "Single Model Posterior Summary")
  estsTable$position <- 1
  jaspContainer[["SMItablePosteriorEstimates"]] <- estsTable
  estsTable$dependOnOptions("singleModelEffects")

  overTitle <- sprintf("%s%% Credible Interval", format(100 * 0.95, digits = 3))
  estsTable$addColumnInfo(name = "Variable", type = "string")
  estsTable$addColumnInfo(name = "Level",    type = "string")
  estsTable$addColumnInfo(name = "Mean",     type = "number", format = "sf:4;dp:3")
	estsTable$addColumnInfo(name = "SD",       type = "number", format = "sf:4;dp:3")
	estsTable$addColumnInfo(name = "Lower",    type = "number", format = "sf:4;dp:3", overtitle = overTitle)
	estsTable$addColumnInfo(name = "Upper",    type = "number", format = "sf:4;dp:3", overtitle = overTitle)

	if (is.null(model) || estsTable$getError()) {
    estsTable$setExpectedRows(1L)	  
	  return()
	}
	.BANOVAfillEstimatesTable(
	  jaspTable   = estsTable,
	  mus         = model$means,
	  sds         = model$sds,
	  cri         = model$CRIs,
	  hasNoLevels = model$allContinuous
	)
	return()

}

.BANOVAsmiPosteriorPlot <- function(jaspContainer, dataset, options, model) {

  # meta wrapper for model averaged posterior plots, single model posterior plots, and Q-Q plots
  if (!is.null(jaspContainer[["SMIposteriorPlot"]]) || !options$singleModelPosteriorPlot)
    return()

  posteriorPlotContainer <- createJaspContainer(title = "Posterior Distributions")
  jaspContainer[["SMIposteriorPlot"]] <- posteriorPlotContainer
  posteriorPlotContainer$position <- 2
  posteriorPlotContainer$dependOnOptions("singleModelPosteriorPlot")
  if (is.null(model) || posteriorPlotContainer$getError()) {
    posteriorPlotContainer[["dummyplot"]] <- createJaspPlot(title = "Posterior distribution", width = 400, height = 400,
                                                            plot = NULL)
  } else {
    .BANOVAfillPosteriorPlotContainer(
      container = posteriorPlotContainer,
      densities = model$densities[, -1L, ],
      cris      = model$CRIs[-1L, ]
    )
  }
  return()
}

.BANOVAsmiQqPlot <- function(jaspContainer, options, model) {

  if (!is.null(jaspContainer[["QQplot"]]) || !options$singleModelqqPlot)
    return()

  if (is.null(model) || jaspContainer$getError()) {
    p <- NULL
  } else {
    p <- JASPgraphs::plotQQnorm(
      residuals = model$residSumStats[,"mean"], 
      lower     = model$residSumStats[,"cri.2.5%"], 
      upper     = model$residSumStats[,"cri.97.5%"]
    )
  } 
  plot <- createJaspPlot(
    title       = "Q-Q Plot",
    width       = 400,
    height      = 400,
    plot        = p,
    aspectRatio = 1
  )
  plot$dependOnOptions("singleModelqqPlot")
  plot$position <- 3
  jaspContainer[["QQplot"]] <- plot
  return()
}

.BANOVAsmiRsqPlot <- function(jaspContainer, options, model) {

  if (!is.null(jaspContainer[["smirsqplot"]]) || !options$singleModelrsqPlot)
    return()

  if (is.null(model) || jaspContainer$getError()) {
    p <- NULL
  } else {
    dd     <- model$rsqDens
    rsqCri <- model$rsqCri
    df     <- data.frame(x = dd$x, y = dd$y)
    xName <- expression(R^2)
    p <- JASPgraphs::PlotPriorAndPosterior(dfLines = df, xName = xName, CRI = rsqCri, drawCRItxt = FALSE)
  }
  plot <- createJaspPlot(
    title       = "Posterior R\u00B2",
    width       = 400,
    height      = 400,
    plot        = p,
    aspectRatio = 1
  )
  plot$dependOnOptions("singleModelrsqPlot")
  plot$position <- 4
  jaspContainer[["smirsqplot"]] <- plot
  return()
}
