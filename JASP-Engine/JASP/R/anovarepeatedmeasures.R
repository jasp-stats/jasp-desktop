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

AnovaRepeatedMeasures <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	numeric.variables <- c(unlist(options$repeatedMeasuresCells), unlist(options$covariates))
	numeric.variables <- numeric.variables[numeric.variables != ""]
	factor.variables <- c(unlist(options$betweenSubjectFactors))
	factor.variables <- factor.variables[factor.variables != ""]

	if (is.null(dataset)) {

		if (perform == "run") {

			dataset <- .readDataSetToEnd(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables, exclude.na.listwise=c(numeric.variables, factor.variables))

		} else {

			dataset <- .readDataSetHeader(columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
		}

	} else {

		dataset <- .vdf(dataset, columns.as.numeric=numeric.variables, columns.as.factor=factor.variables)
	}

	results <- list()



	## Retrieve State

	state <- .retrieveState()
	anovaModel <- NULL
	stateDescriptivesPlot <- NULL
	stateDescriptivesTable <- NULL
	stateLevene <- NULL
	statePostHoc <- NULL
	stateContrasts <- NULL
	stateSimpleEffects <- NULL
	stateSphericity <- NULL
	stateFriedman <- NULL
	stateConnover <- NULL
	
	if ( ! is.null(state)) {  # is there state?

		diff <- .diff(options, state$options)  # compare old and new options

		if (is.list(diff) && diff[['withinModelTerms']] == FALSE && diff[['betweenModelTerms']] == FALSE && diff[['repeatedMeasuresCells']] == FALSE && diff[['postHocTestPooledError']] &&
			diff[['repeatedMeasuresFactors']] == FALSE && diff[['sumOfSquares']] == FALSE && diff[['covariates']] == FALSE && diff[['betweenSubjectFactors']] == FALSE) {

			# old model can be used

			anovaModel <- state$model
			statePostHoc <- state$statePostHoc
			stateContrasts <- state$stateContrasts
			stateSphericity <- state$stateSphericity

		}

		if (is.list(diff) && diff[['plotHorizontalAxis']] == FALSE && diff[['plotSeparateLines']] == FALSE && diff[['plotSeparatePlots']] == FALSE &&
			diff[['plotErrorBars']] == FALSE && diff[['labelYAxis']] == FALSE && !(diff[['errorBarType']] == TRUE && options$plotErrorBars == TRUE) &&
			!(diff[['confidenceIntervalInterval']] == TRUE && options$errorBarType == "confidenceInterval" && options$plotErrorBars == TRUE) &&
			diff[['plotWidthDescriptivesPlotLegend']] == FALSE && diff[['plotHeightDescriptivesPlotLegend']] == FALSE &&
			diff[['plotWidthDescriptivesPlotNoLegend']] == FALSE && diff[['plotHeightDescriptivesPlotNoLegend']] == FALSE &&
			diff[['repeatedMeasuresFactors']] == FALSE && diff[['repeatedMeasuresCells']] == FALSE && diff[['usePooledStandErrorCI']] == FALSE) {

			# old descriptives plots can be used

			stateDescriptivesPlot <- state$stateDescriptivesPlot
		}

		if (is.list(diff) && diff[['betweenSubjectFactors']] == FALSE && diff[['repeatedMeasuresFactors']] == FALSE  && diff[['repeatedMeasuresCells']] == FALSE &&
			diff[['descriptives']] == FALSE) {

			# old descriptives table can be used

			stateDescriptivesTable <- state$stateDescriptivesTable

		}

		if (is.list(diff) && diff[['withinModelTerms']] == FALSE && diff[['betweenModelTerms']] == FALSE && diff[['repeatedMeasuresCells']] == FALSE &&
			diff[['repeatedMeasuresFactors']] == FALSE && diff[['homogeneityTests']] == FALSE && diff[["VovkSellkeMPR"]] == FALSE) {

			# old levene's table can be used

			stateLevene <- state$stateLevene
		}
		
		if (is.list(diff) && diff[['withinModelTerms']] == FALSE && diff[['betweenModelTerms']] == FALSE && diff[['repeatedMeasuresCells']] == FALSE &&
		    diff[['repeatedMeasuresFactors']] == FALSE && diff[['sumOfSquares']] == FALSE && diff[['simpleFactor']] == FALSE && 
		    diff[['moderatorFactorOne']] == FALSE && diff[['moderatorFactorTwo']] == FALSE && diff[['poolErrorTermSimpleEffects']] == FALSE ) {
		  
		  # old simple effects tables can be used
		  
		  stateSimpleEffects <- state$stateSimpleEffects
		  
		}
		
		if (is.list(diff) && diff[['withinModelTerms']] == FALSE && diff[['betweenModelTerms']] == FALSE && 
		    diff[['repeatedMeasuresCells']] == FALSE && diff[['friedmanWithinFactor']] == FALSE && 
		    diff[['friedmanBetweenFactor']] == FALSE && diff[['contrasts']] == FALSE && diff[['connoverTest']]) {
		  
		  # old Friedman table can be used
		  
		  stateFriedman <- state$stateFriedman
		  stateConnover <- state$stateConnover
		}
	}



	## Create Title

	results[["title"]] <- "Repeated Measures ANOVA"


	status <- .rmAnovaCheck(dataset, options, perform)



	## Perform ANOVA

	model <- NULL

	if (is.null(anovaModel)) { # if not retrieved from state

		if (perform == "run" && status$ready && status$error == FALSE) {

			anovaModel <- .rmAnovaModel(dataset, options, status)

			model <- anovaModel$model
			epsilon <- anovaModel$epsilon
			mauchly <- anovaModel$mauchly
			fullModel <- anovaModel$fullModel
			status <- anovaModel$status

			if (is.null(fullModel) || (length(class(fullModel)) == 1 && class(fullModel) == "try-error")) {

				referenceGrid <- NULL
				statePostHoc <- NULL
				stateContrasts <- NULL
				stateSphericity <- NULL

			} else {
				referenceGrid <- .referenceGrid(options, fullModel)
				statePostHoc <- .resultsPostHoc(referenceGrid, options, dataset, fullModel)
				stateContrasts <- .resultsContrasts(dataset, options, referenceGrid)
				stateSphericity <- .resultsSphericity(options, epsilon, mauchly)

			}
		}

	} else {

		model <- anovaModel$model
		status <- anovaModel$status

	}



	## Create Within Subjects Effects Table

	result <- .rmAnovaWithinSubjectsTable(dataset, options, perform, model, stateSphericity, status)

	results[["withinSubjectsEffects"]] <- result$result
	status <- result$status



	## Create Between Subjects Effects Table
  # if(length(unique(unlist(options$betweenSubjectFactors))) > 0 ){
  	result <- .rmAnovaBetweenSubjectsTable(dataset, options, perform, model, status)
  
  	results[["betweenSubjectsEffects"]] <- result$result
  	status <- result$status
  # }


	## Create Sphericity Assumption Table

	if (options$sphericityTests) {

		result <- .sphericityTest(options, stateSphericity, perform, status)
		resultSphericity <- result$result
		status <- result$status

	} else {

		resultSphericity <- NULL

	}



	## Create Levene's Table

	if (is.null(stateLevene)) {

		result <- .rmAnovaLevenesTable(dataset, options, perform, status, stateLevene, model)
		resultLevene <- result$result
		status <- result$status
		stateLevene <- result$stateLevene

	} else {

		resultLevene <- stateLevene

	}

  	## Create Simple Effects Table
  	
  	if (is.null(stateSimpleEffects)) {
  	  
  	  result <- .rmAnovaSimpleEffects(dataset, options, perform, fullModel, results[["withinSubjectsEffects"]], 
  	                                results[["betweenSubjectsEffects"]], status, singular, stateSimpleEffects)
  	  results[["simpleEffects"]] <- result$result
  	  status <- result$status
  	  stateSimpleEffects <- result$stateSimpleEffects
  	  
  	} else {
  	  
  	  results[["simpleEffects"]] <- stateSimpleEffects
  	  
  	}
  	
  	if (is.null(stateFriedman)) {
  	  
  	  result <- .rmAnovaFriedman(dataset, fullModel, options, perform, status, singular, stateFriedman)
  	  results[["friedman"]] <- result$result
  	  status <- result$status
  	  stateFriedman <- result$stateFriedman
  	  
  	} else {
  	  
  	  results[["friedman"]] <- stateFriedman
  	  
  	}
  	
  	if (is.null(stateConnover)) {
  	  
  	  result <- .rmAnovaConnoverTable(dataset, options, perform, fullModel, status, stateConnover, singular)
  	  results[["connover"]] <- list(collection=result$result, title = "Connover's Post Hoc Tests")
  	  status <- result$status
  	  stateConnover <- result$stateConnover
  	  
  	} else {
  	  
  	  results[["connover"]] <- stateConnover
  	  
  	}

	## Create Assumption Check Object

	results[["assumptionsObj"]] <- list(title="Assumption Checks", sphericity=resultSphericity, levene=resultLevene)



	## Create Contrast Tables

	result <- .rmAnovaContrastTable(options, perform, status, stateContrasts)

	results[["contrasts"]] <- list(collection=result$result, title = "Contrasts")
	status <- result$status



	## Create Post Hoc Tables
	result <- .rmAnovaPostHocTable(dataset, options, perform, status, statePostHoc)

	results[["posthoc"]] <- list(collection=result$result, title = "Post Hoc Tests")
	status <- result$status



	## Create Descriptives Table

	if (is.null(stateDescriptivesTable)) {

		result <- .rmAnovaDescriptivesTable(dataset, options, perform, status, stateDescriptivesTable)
		descriptivesTable <- result$result
		status <- result$status
		stateDescriptivesTable <- result$stateDescriptivesTable

	} else {

		descriptivesTable <- stateDescriptivesTable

	}



	## Create Descriptives Plots

	titleDescriptivesPlot <- "Descriptives Plots"

	if (is.null(stateDescriptivesPlot)) {

		result <- .rmAnovaDescriptivesPlot(dataset, options, perform, status, stateDescriptivesPlot)
		descriptivesPlot <- result$result
		status <- result$status
		stateDescriptivesPlot <- result$stateDescriptivesPlot

	} else {

		descriptivesPlot <- stateDescriptivesPlot
	}

	if (length(descriptivesPlot) == 1) {

		results[["descriptivesObj"]] <- list(title="Descriptives", descriptivesTable=descriptivesTable, descriptivesPlot=descriptivesPlot[[1]])

	} else {

		results[["descriptivesObj"]] <- list(title="Descriptives", descriptivesTable=descriptivesTable, descriptivesPlot=list(collection=descriptivesPlot, title = titleDescriptivesPlot))
	}



	## META definitions

	.meta <- list(
		list(name="withinSubjectsEffects", type="table"),
		list(name="betweenSubjectsEffects", type="table"),
		list(name="assumptionsObj", type="object", meta=list(list(name="sphericity", type="table"), list(name="levene", type="table"))),
		list(name="contrasts", type="collection", meta="table"),
		list(name="posthoc", type="collection", meta="table"),
		list(name="simpleEffects", type="table"),
		list(name="friedman", type="table"),
		list(name="connover", type="collection", meta="table")
	)

	if (length(descriptivesPlot) == 1) {

		.meta[[length(.meta) + 1]] <- list(name="descriptivesObj", type="object", meta=list(list(name="descriptivesTable", type="table"), list(name="descriptivesPlot", type="image")))

	} else {

		.meta[[length(.meta) + 1]] <- list(name="descriptivesObj", type="object", meta=list(list(name="descriptivesTable", type="table"), list(name="descriptivesPlot", type="collection", meta="image")))

	}

	results[[".meta"]] <- .meta



	## Save State

	anovaModel$fullModel <- NULL
	state[["model"]] <- anovaModel
	state[["options"]] <- options
	state[["stateDescriptivesPlot"]] <- stateDescriptivesPlot
	state[["stateDescriptivesTable"]] <- stateDescriptivesTable
	state[["stateLevene"]] <- stateLevene
	state[["statePostHoc"]] <- statePostHoc
	state[["stateContrasts"]] <- stateContrasts
	state[["stateSphericity"]] <- stateSphericity
	state[["stateSimpleEffects"]] <- stateSimpleEffects
	state[["stateFriedman"]] <- stateFriedman
	state[["stateConnover"]] <- stateConnover
	
	keepDescriptivesPlot <- lapply(stateDescriptivesPlot, function(x)x$data)



	if (perform == "init" && status$ready && status$error == FALSE) {

		return(list(results=results, status="inited", state=state, keep=keepDescriptivesPlot))

	} else {

		return(list(results=results, status="complete", state=state, keep=keepDescriptivesPlot))
	}
}

.rmAnovaCheck <- function(dataset, options, perform) {

	error <- FALSE
	errorMessage <- NULL
	ready <- "" %in% options$repeatedMeasuresCells == FALSE && length(options$withinModelTerms) > 0

	if (ready && perform == "run") {

		components <- unique(unlist(options$betweenSubjectFactors))
		independentsWithLessThanTwoLevels <- c()

		for (component in components) {

			nLevels <- length(levels(dataset[[ .v(component) ]]))

			if (nLevels < 2)
				independentsWithLessThanTwoLevels <- c(independentsWithLessThanTwoLevels, component)
		}

		if (length(independentsWithLessThanTwoLevels) > 0) {

			error <- TRUE
			if(length(independentsWithLessThanTwoLevels) == 1) {
				errorMessage <- paste("Factor: <em>", independentsWithLessThanTwoLevels, "</em>, contains fewer than two levels.", sep="")
			} else {
				errorMessage <- paste("Factors: <em>", paste(independentsWithLessThanTwoLevels, collapse=",", sep=""), "</em>, contain fewer than two levels.", sep="")
			}
		}

		repeatedMeasuresData <- list()
		for(i in options$repeatedMeasuresCells) {
		    repeatedMeasuresData[[i]] <- dataset[[.v(i)]]
		}
		infiniteRM <- unlist(lapply(repeatedMeasuresData,function(x)sum(is.infinite(x)) > 0))

		if (!is.null(infiniteRM) && sum(infiniteRM) > 0) {

			error <- TRUE
			if(sum(infiniteRM) == 1) {
				errorMessage <- paste("The repeated measure: <em>", options$repeatedMeasuresCells[infiniteRM], "</em>, contains infinite values.", sep="")
			} else {
				errorMessage <- paste("The repeated measures: <em>", paste(options$repeatedMeasuresCells[infiniteRM], collapse=", "), "</em>, contain infinite values.", sep="")
			}
		}

		covariatesData <- list()
		for(i in options$covariates) {
			covariatesData[[i]] <- dataset[[.v(i)]]
		}
		infiniteCov <- unlist(lapply(covariatesData,function(x)sum(is.infinite(x)) > 0))

		if (!is.null(infiniteCov) && sum(infiniteCov) > 0) {

			error <- TRUE
			if(sum(infiniteCov) == 1) {
				errorMessage <- paste("The covariate: <em>", options$covariates[infiniteCov], "</em>, contains infinite values.", sep="")
			} else {
				errorMessage <- paste("The covariates: <em>", paste(options$covariates[infiniteCov], collapse=", "), "</em>, contain infinite values.", sep="")
			}
		}
		
		allNames <- unlist(lapply(options[['repeatedMeasuresFactors']], function(x) x$name)) # Factornames 
		for(factorName in allNames){
		  if (any(factorName %in% options$betweenSubjectFactors )) {
		    error <- TRUE
		    errorMessage <- paste("Please choose a name for the RM factors that differs from those for the 
		                          between subjects factors.", sep="")
		  }
		} 

	}

	list(ready=ready, error=error, errorMessage=errorMessage)
}

.rmModelFormula <- function(options) {

	termsRM.base64 <- c()
	termsRM.normal <- c()

	for (term in options$withinModelTerms) {

		components <- unlist(term$components)
		termRM.base64 <- paste(.v(components), collapse=":", sep="")
		termRM.normal <- paste(components, collapse=" \u273B ", sep="")

		termsRM.base64 <- c(termsRM.base64, termRM.base64)
		termsRM.normal <- c(termsRM.normal, termRM.normal)
	}

	termsBS.base64 <- c()
	termsBS.normal <- c()

	for (term in options$betweenModelTerms) {

		components <- unlist(term$components)
		termBS.base64 <- paste(.v(components), collapse=":", sep="")
		termBS.normal <- paste(components, collapse=" \u273B ", sep="")

		termsBS.base64 <- c(termsBS.base64, termBS.base64)
		termsBS.normal <- c(termsBS.normal, termBS.normal)
	}

	terms.base64 <- list()
	terms.normal <- list()
	terms.base64[[1]] <- termsBS.base64
	terms.normal[[1]] <- termsBS.normal

	for (i in 1:length(termsRM.base64)) {
		if (is.null(termsBS.base64)) {
			terms.base64[[i+1]] <- termsRM.base64[i]
			terms.normal[[i+1]] <- termsRM.normal[i]
		} else if (!is.null(termsRM.base64)){
			terms.base64[[i+1]] <- c(termsRM.base64[i], paste(termsRM.base64[i], termsBS.base64, sep = ":"))
			terms.normal[[i+1]] <- c(termsRM.normal[i], paste(termsRM.normal[i], termsBS.normal, sep = " \u273B "))
		}
	}

	main <- paste("(",paste(unlist(terms.base64), collapse=" + "),")", sep="")
	termsBS <- paste("(",paste(termsBS.base64, collapse=" + "),")", sep="")
	errorRM <- paste("Error(",paste("subject/(", termsRM.base64, ")",sep="", collapse=" + "),")",sep="")

	if (is.null(termsBS.base64) && is.null(termsRM.base64)) {
		model.def <- dependent ~ 1
	} else if (is.null(termsBS.base64)) {
		model.def <- paste("dependent", "~", paste(main, errorRM, sep=" + "))
	} else if (is.null(termsRM.base64)) {
		model.def <- paste("dependent", "~", main)
	} else {
		model.def <- paste("dependent", "~", paste(main, errorRM, termsBS, sep=" + "))
	}

	list(model.def = model.def, terms.normal = terms.normal, terms.base64 = terms.base64, termsRM.normal = termsRM.normal, termsRM.base64 = termsRM.base64)
}

.rmAnovaModel <- function(dataset, options, status) {

	modelDef <- .rmModelFormula(options)
	model.formula <- as.formula(modelDef$model.def)

	dataset <- .shortToLong(dataset, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, c(options$betweenSubjectFactors, options$covariates))

	variables <- unlist(c(options$betweenSubjectFactors, lapply(options$repeatedMeasuresFactors, function(x) x$name)))

	for (i in variables)
		dataset[[.v(i)]] <- .v(dataset[[.v(i)]])

	options(contrasts=c("contr.sum","contr.poly"))

  # set these options once for all afex::aov_car calls,
  # this ensures for instance that afex::aov_car always returns objects of class afex_aov.
  afex::afex_options(
    check_contrasts = TRUE, correction_aov = "GG", 
    emmeans_model = "univariate", es_aov = "ges", factorize = TRUE, 
    lmer_function = "lmerTest", method_mixed = "KR", return_aov = "afex_aov", 
    set_data_arg = FALSE, sig_symbols = c(" +", " *", " **", " ***"), type = 3
  )
	if (options$sumOfSquares == "type1") {

		result <- try(stats::aov(model.formula, data=dataset), silent = TRUE)
		fullModel <- try(afex::aov_car(model.formula, data=dataset, type= 3, factorize = FALSE), silent = TRUE)

	} else if (options$sumOfSquares == "type2") {

		result <- try(afex::aov_car(model.formula, data=dataset, type= 2, factorize = FALSE), silent = TRUE)

	} else {

		result <- try(afex::aov_car(model.formula, data=dataset, type= 3, factorize = FALSE), silent = TRUE)
	}

	model <- NULL
	epsilon <- NULL
	mauchly <- NULL
  
	if (length(class(result)) == 1 && class(result) == "try-error") {

		fullModel <- result

		status$error <- TRUE
		status$errorMessage <- .extractErrorMessage(result)

		if (status$errorMessage == "Some parameters are not estimable, most likely due to empty cells of the design (i.e., structural missings). Check your data.") {

			status$errorMessage <- "Some parameters are not estimable, most likely due to empty cells of the design."

		}

	} else {

		if (options$sumOfSquares == "type1" && class(fullModel) != "try-error") {

			model <- summary(result)
			epsilon <- summary(fullModel)$pval.adjustments
			mauchly <- summary(fullModel)$sphericity.tests

		} else if (options$sumOfSquares == "type1" && class(fullModel) == "try-error") {

			model <- summary(result)

		} else {

			summaryResult <- summary(result)
			model <- summaryResult$univariate.tests
			epsilon <- summaryResult$pval.adjustments
			mauchly <- summaryResult$sphericity.tests
			fullModel <- result

		}
	}

	list(model = model, epsilon = epsilon, mauchly = mauchly, fullModel = fullModel, status = status)
}

.resultsSphericity <- function(options, epsilon, mauchly) {

	modelDef <- .rmModelFormula(options)
	termsRM.base64 <- modelDef$termsRM.base64
	termsRM.normal <- modelDef$termsRM.normal

	epsilonTable <- NULL

	epsilonTable <- as.data.frame(matrix(0, length(termsRM.base64), 6))
	colnames(epsilonTable) <- c("W", "p", "GG", "HF", "twoLevels", "termsNormal")


	rownames(epsilonTable) <- termsRM.base64
	if (is.null(rownames(mauchly))) {
		modelTermsResults <- list()
	} else {
		modelTermsResults <- strsplit(rownames(mauchly), ":")
	}

	for (i in .indices(termsRM.base64)) {

		modelTermsCase <- unlist(strsplit(termsRM.base64[[i]],":"))
		index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
		epsilonTable[i,"termsNormal"] <- termsRM.normal[[i]]

		if (sum(index) == 0 ) {

		  epsilonTable[i,"W"] <- 1
			epsilonTable[i,"p"] <- NaN
			epsilonTable[i,"GG"] <- 1
			epsilonTable[i,"HF"] <- 1
			epsilonTable[i,"twoLevels"] <- TRUE

		} else if (is.na(epsilon[1])) {
		  
		  epsilonTable[i,"W"] <- NaN
		  epsilonTable[i,"p"] <- NaN
		  epsilonTable[i,"GG"] <- NaN
		  epsilonTable[i,"HF"] <- NaN
		  epsilonTable[i,"twoLevels"] <- FALSE
		  
		} else {

			HF <- epsilon[index, "HF eps"]

			if (HF > 1)
				HF <- 1

			epsilonTable[i,"W"] <- mauchly[index,"Test statistic"]
			epsilonTable[i,"p"] <- mauchly[index,"p-value"]
			epsilonTable[i,"GG"] <- epsilon[index, "GG eps"]
			epsilonTable[i,"HF"] <- HF
			epsilonTable[i,"twoLevels"] <- FALSE

		}
	}

	return(epsilonTable)
}

.referenceGrid <- function (options, fullModel) {
		referenceGridList <- list()
		variables <- unlist(c(lapply(options$betweenModelTerms, 
		  function(x) {
		    if (length(x$components) == 1) {
		      return (x$components)
		    } else {
		      return(NULL)
		    }
		  }), lapply(options$withinModelTerms,
		  function(x) {
		    if (length(x$components) == 1) {
		      return (x$components)
		    } else {
		      return(NULL)
		    }
		  })
		))

		for (var in variables) {

			formula <- as.formula(paste("~", .v(var)))
			referenceGrid <- emmeans::emmeans(fullModel, formula)

			referenceGridList[[var]] <- referenceGrid

		}

		return(referenceGridList)
}

.resultsPostHoc <- function (referenceGrid, options, dataset, fullModel) {
    resultsPostHoc <- list()
  
    variables <- unlist(c(lapply(options$betweenModelTerms, 
                                 function(x) {
                                   if (length(x$components) == 1) {
                                     return (x$components)
                                   } else {
                                     return(NULL)
                                   }
                                 }), lapply(options$withinModelTerms,
                                            function(x) {
                                              if (length(x$components) == 1) {
                                                return (x$components)
                                              } else {
                                                return(NULL)
                                              }
                                            })
    ))
    

		postHocData <- fullModel$data$wide
		factorNamesV <- colnames(postHocData) # Names to use to refer to variables in data
    # Because there are multiple names for each variable in JASP, one of the things the following code does is make sure to get the correct naming
		# and refer to the correct actual variable. The different names are the actual name of the variable, the name the user gives in jasp for the lvel and factor, 
		# and also the name that JASP gives to it, which is a concatenation of "Level#_Level#', where the first refers to the factor and second to the level. 
		
		
		
		rmFactorIndex <- 1
		allNames <- unlist(lapply(options$repeatedMeasuresFactors, function(x) x$name)) # Factornames 
		
		# for(factorName in allNames){
		#   if (any(factorName %in% options$betweenSubjectFactors )) {
		#     error <- TRUE
		#     errorMessage <- paste("Please choose a different name for the RM factor.", sep="")
		#     posthoc.table[["error"]] <- errorMessage
		#   }
		# }

		for (var in variables) {

		  # Results using the Tukey method
		  resultTukey <- summary(pairs(referenceGrid[[var]], adjust="tukey"))
		  
		  # Results using the Scheffe method
		  resultScheffe <- summary(pairs(referenceGrid[[var]], adjust="scheffe"))
		   
		  # Results using the Bonferroni method
		  resultBonf <- summary(pairs(referenceGrid[[var]], adjust="bonferroni"), infer = TRUE)
		  comparisons <- strsplit(as.character(resultBonf$contrast), " - ")
		  
		  # Results using the Holm method
		  resultHolm <- summary(pairs(referenceGrid[[var]], adjust="holm"))
	
		  orderOfTerms <- unlist(lapply(options$repeatedMeasuresFactors, function(x) x$name))
		  indexofOrderFactors <- match(allNames,orderOfTerms)
		  
		  if (any(var == allNames)) {     ## If the variable is a repeated measures factor
		    
		    if (!options$postHocTestPooledError) {
		      
		      levelsOfThisFactor <- unlist(lapply(options$repeatedMeasuresFactors[rmFactorIndex], function(x) x$levels)) # Levels within Factor
		      numberOfLevels <- length(unique(levelsOfThisFactor))
  		    splitNames <- unlist(lapply(strsplit(factorNamesV,  split = "_"), function(x) x[indexofOrderFactors[rmFactorIndex]]))
  		    listVarNamesToLevel <- list()  # create a list of vectors of variable names, used to group the dataset for the post-hoc t-tests
  		    for(i in 1:numberOfLevels){
  		      listVarNamesToLevel[[i]] <- factorNamesV[(splitNames %in% .v(levelsOfThisFactor[i]))]  
  		    }

    		  countr <- 1
    		  allEstimates <- allTees <- allSE <- allPees <- numeric() 
    		  for (k in 1:numberOfLevels) {  ### Loop over all the levels within factor and do pairwise t.tests on them
    		    for (i in .seqx(k+1, numberOfLevels)) {
    		      tResult <- t.test(rowMeans(postHocData[listVarNamesToLevel[[k]]]),rowMeans(postHocData[listVarNamesToLevel[[i]]]), paired= TRUE, var.equal = FALSE)
    		      allEstimates[countr] <- tResult$estimate
    		      allTees[countr] <- tResult$statistic
    		      allSE[countr] <- tResult$estimate / tResult$statistic
    		      allPees[countr] <- tResult$p.value
    		      countr <- countr + 1
    		    }
    		  }
    		  bonferPvals <- p.adjust(allPees, method = "bonferroni")  # correct all pvalues according to bonf
    		  resultGeneral <- list(estimate = allEstimates, t.ratio = allTees, SE = allSE, p.value = bonferPvals )
    		  resultBonf['t.ratio'] <- allTees
    		  resultBonf['p.value'] <- bonferPvals
    		  resultBonf['SE'] <- allSE
    		  resultBonf['estimate'] <- allEstimates 
    		  resultHolm['p.value'] <- p.adjust(allPees, method = "holm")  # correct all pvalues according to holm
		    }
  		  resultScheffe['p.value'] <- rep("-", length(resultScheffe['p.value']))
  		  resultTukey['p.value'] <-  rep("-", length(resultTukey['p.value']))
  		  rmFactorIndex <- rmFactorIndex + 1
  		  }


			resultsPostHoc[[var]] <- list(resultBonf = resultBonf, resultHolm = resultHolm, 
			                              resultTukey = resultTukey, resultScheffe = resultScheffe,
										                comparisons = comparisons)
			
		}

		return(resultsPostHoc)
}

.resultsContrasts <- function (dataset, options, referenceGrid) {

		resultsContrasts <- list()
		datasetLong <- .shortToLong(dataset, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, options$betweenSubjectFactors)

		contrastTypes <- c("none", "deviation", "simple", "Helmert", "repeated", "difference", "polynomial")

		for (contrast in options$contrasts) {
		  
		  if (! contrast$variable %in% names(referenceGrid)) {
		    next
		  }

			resultsContrasts[[contrast$variable]] <- list()

			column <- datasetLong[[.v(contrast$variable)]]

			for(contrastType in contrastTypes) {

			  contrastMatrix <- .rmAnovaCreateContrast(column, contrastType)
				contrCoef <- lapply(as.data.frame(contrastMatrix), as.vector)
				names(contrCoef) <- .v(.anovaContrastCases(column, contrastType))
        
				if (contrastType == "none") {
					r <- NULL
				} else {
					r <- emmeans::contrast(referenceGrid[[contrast$variable]], contrCoef)
				}
				resultsContrasts[[contrast$variable]][[contrastType]] <- summary(r)
			}
		}

		return(resultsContrasts)
}

.rmAnovaCreateContrast <- function (column, contrast.type) {
  
  levels <- levels(column)
  n.levels <- length(levels)
  
  contr <- NULL

  if (contrast.type == "none") {
    
    options(contrasts = c("contr.sum","contr.poly"))
    contr <- NULL
    
  } else if (contrast.type == "deviation") {
    
    contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
    for (i in 2:n.levels) {
      contr[,(i-1)] <-  -1 / n.levels
      contr[i,(i-1)] <- (n.levels - 1) / n.levels
    }
    
  } else if (contrast.type == "simple") {
    
    contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
    for (i in 1:n.levels-1) {
      contr[c(1,i+1),i]<- c(1,-1) * -1
    }
    
  } else if (contrast.type == "Helmert") {
    
    contr <- contr.helmert(levels) 
    contr <- apply(contr, 2, function(x){ x/max(abs(x))})
    contr <- matrix(rev(contr), ncol = ncol(contr), nrow = nrow(contr))
    
  } else if (contrast.type == "repeated") {
    
    contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
    
    for (i in 1:(n.levels-1)) {
      contr[i,i] <- 1
      contr[i+1,i] <- -1
    }
    
  } else if (contrast.type == "difference") {

    contr <- contr.helmert(levels) 
    contr <- apply(contr, 2, function(x){ x/max(abs(x))})
    
  } else if (contrast.type == "polynomial") {
    
    contr <- contr.poly(levels)
  }

  if (! is.null(contr)) {
    dimnames(contr) <- list(NULL, 1:dim(contr)[2])
  }
  
  contr
}


.identicalTerms <- function(x,y) {

	equalLength <- length(x) == length(y)
	equalTerms <- all(x %in% y)

	if(equalLength && equalTerms) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

.rmAnovaBetweenSubjectsTable <- function(dataset, options, perform, model, status) {

	anova <- list()

	anova[["title"]] <- "Between Subjects Effects"

	fields <- list()
	footnotes <- .newFootnotes()

	fields[[length(fields) + 1]] <- list(name="case", type="string", title="", combine=TRUE)
	fields[[length(fields) + 1]] <- list(name="SS", type="number", format="sf:4;dp:3", title="Sum of Squares")
	fields[[length(fields) + 1]] <- list(name="df", type="integer")
	fields[[length(fields) + 1]] <- list(name="MS", type="number", format="sf:4;dp:3", title="Mean Square")
	fields[[length(fields) + 1]] <- list(name="F", type="number", format="sf:4;dp:3")
	fields[[length(fields) + 1]] <- list(name="p", type="number", format="dp:3;p:.001")

	if (options$VovkSellkeMPR) {
    fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
                                        title = "VS-MPR\u002A",
                                        type = "number",
                                        format = "sf:4;dp:3")
  }

	if (options$effectSizeEstimates) {

		if(options$effectSizeEtaSquared) {
			fields[[length(fields) + 1]] <- list(name="eta", type="number", title="\u03B7\u00B2", format="dp:3")
		}
		if(options$effectSizePartialEtaSquared) {
			fields[[length(fields) + 1]] <- list(name="partialEta", type="number", title="\u03B7\u00B2\u209A", format="dp:3")
		}
		if(options$effectSizeOmegaSquared) {
			fields[[length(fields) + 1]] <- list(name="omega", type="number", title="\u03C9\u00B2", format="dp:3")
		}
	}

	anova[["schema"]] <- list(fields=fields)



	if (options$sumOfSquares == "type1") {

		.addFootnote(footnotes, text = "Type I Sum of Squares", symbol = "<em>Note.</em>")

	} else if (options$sumOfSquares == "type2") {

		.addFootnote(footnotes, text = "Type II Sum of Squares", symbol = "<em>Note.</em>")

	} else if (options$sumOfSquares == "type3") {

		.addFootnote(footnotes, text = "Type III Sum of Squares", symbol = "<em>Note.</em>")

	}

	if (options$VovkSellkeMPR){
		.addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
		<em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
		possible odds in favor of H\u2081 over H\u2080 equals
		1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
		(Sellke, Bayarri, & Berger, 2001).")
	}

	modelDef <- .rmModelFormula(options)
	terms.normal <- modelDef$terms.normal
	terms.base64 <- modelDef$terms.base64
	termsRM.base64 <- modelDef$termsRM.base64

	if (is.null(model)) {

		anova.rows <- list()

		if (length(terms.base64) > 0) {

			for (j in .indices(terms.base64[[1]])) {

				if (j == 1) {
					newGroup <- TRUE
				} else {
					newGroup <- FALSE
				}

				row <- list("case"=terms.normal[[1]][j], "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
				if (options$VovkSellkeMPR){
					row[["VovkSellkeMPR"]] <- "."
				}
				anova.rows[[length(anova.rows) + 1]] <- row

			}
		}

		row <- list("case"="Residual", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = TRUE)
		if (options$VovkSellkeMPR){
			row[['VovkSellkeMPR']] <- "."
		}
		anova.rows[[length(anova.rows) + 1]] <- row

		anova[["data"]] <- anova.rows

		if (status$error)
			anova[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)

	} else {

		anova.rows <- list()

		if (options$sumOfSquares == "type1") {

			resultTable <- model[["Error: subject"]][[1]]

			modelTermsResults <- strsplit(gsub(" ", "", rownames(resultTable), fixed = TRUE), ":")

			for (j in .indices(terms.base64[[1]])) {

				if (j == 1) {
					newGroup <- TRUE
				} else {
					newGroup <- FALSE
				}

				modelTermsCase <- strsplit(terms.base64[[1]],":")[[j]]
				index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))

				if (sum(index) > 0) {

					SS <- resultTable[index,"Sum Sq"]
					df <- resultTable[index,"Df"]
					MS <- resultTable[index,"Mean Sq"]
					F <- resultTable[index,"F value"]
					p <- resultTable[index,"Pr(>F)"]

					if (is.null(F) || is.null(p)) {
						F <- .clean(NaN)
						p <- .clean(NaN)
					}

				} else {

					SS <- 0
					df <- 0
					MS <- .clean(NaN)
					F <- .clean(NaN)
					p <- .clean(NaN)

				}

				row <- list("case"=terms.normal[[1]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)
				if (options$VovkSellkeMPR){
					row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
				}

				if (options$effectSizeEstimates) {

					if (sum(gsub(" ", "", row.names(resultTable), fixed = TRUE) == "Residuals") > 0) {

						SSt <- sum(resultTable[,"Sum Sq"])
						SSr <- resultTable["Residuals","Sum Sq"]
						MSr <- SSr/resultTable["Residuals","Df"]
  
						row[["eta"]] <- SS / SSt
						row[["partialEta"]] <- SS / (SS + SSr)
						omega <- (SS - (df * MSr)) / (SSt + MSr) 

						if (omega < 0) {
							row[["omega"]] <- 0
						} else {
							row[["omega"]] <- omega
						}

					} else {

						row[["eta"]] <- .clean(NaN)
						row[["partialEta"]] <- .clean(NaN)
						row[["omega"]] <- .clean(NaN)

					}
				}

				anova.rows[[length(anova.rows) + 1]] <- row

			}

			if (sum(gsub(" ", "", row.names(resultTable), fixed = TRUE) == "Residuals") > 0) {

				SS <- resultTable["Residuals","Sum Sq"]
				df <- resultTable["Residuals","Df"]
				MS <- resultTable["Residuals","Mean Sq"]

			} else {

				SS <- 0
				df <- 0
				MS <- .clean(NaN)
			}

			row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = TRUE)
			if (options$VovkSellkeMPR){
				row[["VovkSellkeMPR"]] <- ""
			}
			anova.rows[[length(anova.rows) + 1]] <- row

		} else {	# if sum of squares is equal to type 2 or 3

			result <- model

			modelTermsResults <- strsplit(rownames(result), ":")

			for (j in .indices(terms.base64[[1]])) {

				if (j == 1) {
					newGroup <- TRUE
				} else {
					newGroup <- FALSE
				}

				modelTermsCase <- strsplit(terms.base64[[1]],":")[[j]]
				index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))

				SS <- result[index,"Sum Sq"]
				df <- result[index,"num Df"]
				MS <- SS / df
				F <- .clean(result[index,"F value"])
				p <- .clean(result[index,"Pr(>F)"])

				row <- list("case"=terms.normal[[1]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)

				if (options$VovkSellkeMPR){
					row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
				}

				if (options$effectSizeEstimates) {

					modelTermsCases <- strsplit(terms.base64[[1]],":")

					indices <- c()
					for (case in .indices(terms.base64[[1]])) {
						indices <- c(indices, which(unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCases[[case]])))))
					}

					if (result[index,"Error SS"] > 0) {

						SSr <- result[index,"Error SS"]
						SSt <- sum(result[indices,"Sum Sq"]) + SSr
						MSr <- SSr/result[index,"den Df"]

						row[["eta"]] <- SS / SSt
						row[["partialEta"]] <- SS / (SS + SSr)
						omega <- (SS - (df * MSr)) / (SSt + MSr)

						if (omega < 0) {
							row[["omega"]] <- 0
						} else {
							row[["omega"]] <- omega
						}

					} else {

						row[["eta"]] <- .clean(NaN)
						row[["partialEta"]] <- .clean(NaN)
						row[["omega"]] <- .clean(NaN)

					}
				}

				anova.rows[[length(anova.rows) + 1]] <- row

			}

			indexResidual <- 1

			SS <- result[indexResidual,"Error SS"]
			df <- result[indexResidual,"den Df"]

			if (SS == 0 && df == 0) {
				MS <- .clean(NaN)
			} else {
				MS <- SS / df
			}

			row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = TRUE)
			if (options$VovkSellkeMPR){
				row[["VovkSellkeMPR"]] <- ""
			}
			anova.rows[[length(anova.rows) + 1]] <- row

		}

		anova[["data"]] <- anova.rows
		anova[["status"]] <- "complete"

	}

	anova[["footnotes"]] <- as.list(footnotes)

	list(result = anova, status = status)
}

.rmAnovaWithinSubjectsTable <- function(dataset, options, perform, model, stateSphericity, status) {

	anova <- list()

	anova[["title"]] <- "Within Subjects Effects"

	corrections <- NULL

	if (options$sphericityCorrections) {

		if (options$sphericityNone) {
			corrections <- c(corrections, "None")
		}

		if (options$sphericityGreenhouseGeisser) {
			corrections <- c(corrections, "Greenhouse-Geisser")
		}

		if (options$sphericityHuynhFeldt) {
			corrections <- c(corrections, "Huynh-Feldt")
		}
	}

	fields <- list()
	footnotes <- .newFootnotes()

	fields[[length(fields) + 1]] <- list(name="case", type="string", title="", combine=TRUE)

	if (options$sphericityCorrections && !is.null(corrections))
		fields[[length(fields) + 1]] <- list(name="cor", type="string", title="Sphericity Correction")

	fields[[length(fields) + 1]] <- list(name="SS", type="number", format="sf:4;dp:3", title="Sum of Squares")

	if (options$sphericityCorrections && !is.null(corrections)) {
		fields[[length(fields) + 1]] <- list(name="df", type="number", format="sf:4;dp:3")
	} else {
		fields[[length(fields) + 1]] <- list(name="df", type="integer")
	}

	fields[[length(fields) + 1]] <- list(name="MS", type="number", format="sf:4;dp:3", title="Mean Square")
	fields[[length(fields) + 1]] <- list(name="F", type="number", format="sf:4;dp:3")
	fields[[length(fields) + 1]] <- list(name="p", type="number", format="dp:3;p:.001")

	if (options$VovkSellkeMPR) {
		fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
																				title = "VS-MPR\u002A",
																				type = "number",
																				format = "sf:4;dp:3")
	}

	if (options$effectSizeEstimates) {

		if(options$effectSizeEtaSquared) {
			fields[[length(fields) + 1]] <- list(name="eta", type="number", title="\u03B7\u00B2", format="dp:3")
		}
		if(options$effectSizePartialEtaSquared) {
			fields[[length(fields) + 1]] <- list(name="partialEta", type="number", title="\u03B7\u00B2\u209A", format="dp:3")
		}
		if(options$effectSizeOmegaSquared) {
			fields[[length(fields) + 1]] <- list(name="omega", type="number", title="\u03C9\u00B2", format="dp:3")
		}
	}

	anova[["schema"]] <- list(fields=fields)



	if (options$sumOfSquares == "type1") {

		.addFootnote(footnotes, text = "Type I Sum of Squares", symbol = "<em>Note.</em>")

	} else if (options$sumOfSquares == "type2") {

		.addFootnote(footnotes, text = "Type II Sum of Squares", symbol = "<em>Note.</em>")

	} else if (options$sumOfSquares == "type3") {

		.addFootnote(footnotes, text = "Type III Sum of Squares", symbol = "<em>Note.</em>")

	}

	if (options$VovkSellkeMPR){
		.addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
		<em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
		possible odds in favor of H\u2081 over H\u2080 equals
		1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
		(Sellke, Bayarri, & Berger, 2001).")
	}

	modelDef <- .rmModelFormula(options)
	terms.normal <- modelDef$terms.normal
	terms.base64 <- modelDef$terms.base64
	termsRM.base64 <- modelDef$termsRM.base64

	#	(options$sphericityCorrections && !is.null(corrections) && !(length(corrections) == 1 && corrections == "None"))
	#	(options$sphericityCorrections && perform == "init")

	if (is.null(model)) {

		anova.rows <- list()

		if (length(terms.base64) > 1) {

			for (i in 2:length(terms.base64)) {

				for (j in .indices(terms.base64[[i]])) {

					if (j == 1) {
						newGroup <- TRUE
					} else {
						newGroup <- FALSE
					}

					if (options$sphericityCorrections && !is.null(corrections)) {

						counter <- 1

						if (options$sphericityNone) {
							row <- list("case"=terms.normal[[i]][j], "cor"="None", "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = (newGroup && counter == 1), ".isNewSubGroup" = (!newGroup && counter == 1 && length(corrections) > 1))
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- "."
							}
							anova.rows[[length(anova.rows) + 1]] <- row
							counter <- counter + 1
						}

						if (options$sphericityGreenhouseGeisser) {
							row <- list("case"=terms.normal[[i]][j], "cor"="Greenhouse-Geisser", "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = (newGroup && counter == 1), ".isNewSubGroup" = (!newGroup && counter == 1 && length(corrections) > 1))
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- "."
							}
							anova.rows[[length(anova.rows) + 1]] <- row
							counter <- counter + 1
						}

						if (options$sphericityHuynhFeldt) {
							row <- list("case"=terms.normal[[i]][j], "cor"="Huynh-Feldt", "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = (newGroup && counter == 1), ".isNewSubGroup" = (!newGroup && counter == 1 && length(corrections) > 1))
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- "."
							}
							anova.rows[[length(anova.rows) + 1]] <- row
							counter <- counter + 1
						}

					} else {

						row <- list("case"=terms.normal[[i]][j], "SS"=".", "df"=".", "MS"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- "."
						}
						anova.rows[[length(anova.rows) + 1]] <- row

					}
				}

				if (options$sphericityCorrections && !is.null(corrections)) {

					counter <- 1

					if (options$sphericityNone) {
						row <- list("case"="Residual", "cor"="None", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = (counter == 1))
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}
						anova.rows[[length(anova.rows) + 1]] <- row
						counter <- counter + 1
					}

					if (options$sphericityGreenhouseGeisser) {
						row <- list("case"="Residual", "cor"="Greenhouse-Geisser", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = (counter == 1))
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}
						anova.rows[[length(anova.rows) + 1]] <- row
						counter <- counter + 1
					}

					if (options$sphericityHuynhFeldt) {
						row <- list("case"="Residual", "cor"="Huynh-Feldt", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = (counter == 1))
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}
						anova.rows[[length(anova.rows) + 1]] <- row
						counter <- counter + 1
					}

				} else {

					row <- list("case"="Residual", "SS"=".", "df"=".", "MS"=".", "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = TRUE)
					if (options$VovkSellkeMPR){
						row[["VovkSellkeMPR"]] <- ""
					}
					anova.rows[[length(anova.rows) + 1]] <- row

				}
			}
		}

		anova[["data"]] <- anova.rows

		if (status$error)
			anova[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)

		#		if (options$sphericityCorrections && !is.null(corrections) && !(length(corrections) == 1 && corrections == "None"))
		#			anova[["error"]] <- list(errorType="badData", errorMessage="Could not estimate sphericity corrections due to problems with estimating the correction parameters.")

	} else {

		anova.rows <- list()

		if (is.null(corrections))
			corrections <- "empty"

		if (options$sumOfSquares == "type1") {

			result <- model

			for (i in 2:length(terms.base64)) {

				resultTable <- result[[paste("Error: subject", termsRM.base64[[i-1]], sep=":")]][[1]]

				modelTermsResults <- strsplit(gsub(" ", "", rownames(resultTable), fixed = TRUE), ":")

				for (j in .indices(terms.base64[[i]])) {

					if (j == 1) {
						newGroup <- TRUE
					} else {
						newGroup <- FALSE
					}

					modelTermsCase <- strsplit(terms.base64[[i]],":")[[j]]
					index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))

					if (sum(index) > 0) {

						SS <- resultTable[index,"Sum Sq"]
						df <- resultTable[index,"Df"]
						MS <- resultTable[index,"Mean Sq"]
						F <- resultTable[index,"F value"]
						p <- resultTable[index,"Pr(>F)"]
						dfR <- resultTable["Residuals","Df"]

						if (is.null(F) && is.null(p)) {
							F <- .clean(NaN)
							p <- .clean(NaN)
						}

						if (is.na(dfR)) {
							dfR <- 0
						}

					} else {

						SS <- 0
						df <- 0
						MS <- .clean(NaN)
						F <- .clean(NaN)
						p <- .clean(NaN)
						dfR <- 0

					}

					counter <- 0

					for (cor in corrections) {

						counter <- counter + 1

						row.footnotes <- NULL

						if (!is.null(stateSphericity) && !is.na(stateSphericity[i-1,"p"]) && stateSphericity[i-1,"p"] < .05) {

							foot.index <- .addFootnote(footnotes, text="Mauchly's test of sphericity indicates that the assumption of sphericity is violated (p < .05).")
							row.footnotes <- list(SS=list(foot.index), df=list(foot.index), MS=list(foot.index), F=list(foot.index), p=list(foot.index))

						}

						if (!options$sphericityCorrections || cor == "empty") {

							row <- list("case"=terms.normal[[i]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup, .footnotes=row.footnotes)
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
							}

						} else if (cor == "None") {

							row <- list("case"=terms.normal[[i]][j], "cor"="None", "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = (newGroup && counter == 1), ".isNewSubGroup" = (!newGroup && counter == 1 && length(corrections) > 1), .footnotes=row.footnotes)
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
							}

						} else if (cor == "Greenhouse-Geisser") {

							dfGG <- df * stateSphericity[i-1,"GG"]
							MSGG <- SS / dfGG
							dfRGG <- dfR * stateSphericity[i-1,"GG"]

							if (F != "NaN") {
								pGG <-  pf(F,dfGG,dfRGG,lower.tail=FALSE)
							} else {
								pGG <- .clean(NaN)
							}

							row <- list("case"=terms.normal[[i]][j], "cor"="Greenhouse-Geisser", "SS"=SS, "df"=dfGG, "MS"=MSGG, "F"=F, "p"=pGG, ".isNewGroup" = (newGroup && counter == 1), ".isNewSubGroup" = (!newGroup && counter == 1 && length(corrections) > 1), .footnotes=row.footnotes)
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
							}

						} else if (cor == "Huynh-Feldt") {

							dfHF <- df * stateSphericity[i-1,"HF"]
							MSHF <- SS / dfHF
							dfRHF <- dfR * stateSphericity[i-1,"HF"]

							if (F != "NaN") {
								pHF <- pf(F,dfHF,dfRHF,lower.tail=FALSE)
							} else {
								pHF <- .clean(NaN)
							}

							row <- list("case"=terms.normal[[i]][j], "cor"="Huynh-Feldt", "SS"=SS, "df"=dfHF, "MS"=MSHF, "F"=F, "p"=pHF, ".isNewGroup" = (newGroup && counter == 1), ".isNewSubGroup" = (!newGroup && counter == 1 && length(corrections) > 1), .footnotes=row.footnotes)
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
							}

						}

						if (options$effectSizeEstimates) {

							if (sum(gsub(" ", "", row.names(resultTable), fixed = TRUE) == "Residuals") > 0) {
							  index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
							  
								SSt <- sum(resultTable[,"Sum Sq"])
								SSr <- resultTable["Residuals","Sum Sq"]
								MSr <- SSr/resultTable["Residuals","Df"]
								row[["eta"]] <- SS / SSt
								row[["partialEta"]] <- SS / (SS + SSr)
								n <- resultTable["Residuals","Df"]  + 1
								MSm <- resultTable[index, "Mean Sq"]
								MSb <- result["Error: subject"][[1]][[1]]$`Mean Sq`
								MSb <- MSb[length(MSb)]
								omega <- (df / (n * (df + 1)) * (MSm - MSr)) / 
      								   (MSr + ((MSb - MSr) / (df + 1)) +
      								   (df / (n * (df + 1))) * (MSm - MSr))

								if (omega < 0) {
									row[["omega"]] <- 0
								} else {
									row[["omega"]] <- omega
								}

							} else {

								row[["eta"]] <- .clean(NaN)
								row[["partialEta"]] <- .clean(NaN)
								row[["omega"]] <- .clean(NaN)

							}
						}

						anova.rows[[length(anova.rows) + 1]] <- row
					}
				}

				if (sum(gsub(" ", "", row.names(resultTable), fixed = TRUE) == "Residuals") > 0) {

					SS <- resultTable["Residuals","Sum Sq"]
					df <- resultTable["Residuals","Df"]
					MS <- resultTable["Residuals","Mean Sq"]

				} else {

					SS <- 0
					df <- 0
					MS <- .clean(NaN)
				}

				counter <- 0

				for (cor in corrections) {

					counter <- counter + 1

					if (!options$sphericityCorrections || cor == "empty") {

						row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = TRUE)
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}

					} else if (cor == "None") {

						row <- list("case"="Residual", "cor"="None", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = (counter == 1))
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}

					} else if (cor == "Greenhouse-Geisser") {

						dfGG <- df * stateSphericity[i-1,"GG"]

						if (SS > 0) {
							MSGG <- SS / dfGG
						} else {
							MSGG <- .clean(NaN)
						}

						row <- list("case"="Residual", "cor"="Greenhouse-Geisser", "SS"=SS, "df"=dfGG, "MS"=MSGG, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = (counter == 1))
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}

					} else if (cor == "Huynh-Feldt") {

						dfHF <- df * stateSphericity[i-1,"HF"]

						if (SS > 0) {
							MSHF <- SS / dfHF
						} else {
							MSHF <- .clean(NaN)
						}

						row <- list("case"="Residual", "cor"="Huynh-Feldt", "SS"=SS, "df"=dfHF, "MS"=MSHF, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = (counter == 1))
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}

					}

					anova.rows[[length(anova.rows) + 1]] <- row

				}
			}

		} else {	# if sum of squares is equal to type 2 or 3

			result <- model

			modelTermsResults <- strsplit(rownames(result), ":")

			for (i in 2:length(terms.base64)) {

				for (j in .indices(terms.base64[[i]])) {

					if (j == 1) {
						newGroup <- TRUE
					} else {
						newGroup <- FALSE
					}

					modelTermsCase <- strsplit(terms.base64[[i]],":")[[j]]
					index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))

					SS <- result[index,"Sum Sq"]
					df <- result[index,"num Df"]
					MS <- SS / df
					F <- .clean(result[index,"F value"])
					p <- .clean(result[index,"Pr(>F)"])
					dfR <- result[index,"den Df"]

					counter <- 0

					for (cor in corrections) {

						counter <- counter + 1

						row.footnotes <- NULL

						if (!is.null(stateSphericity) && !is.na(stateSphericity[i-1,"p"]) && stateSphericity[i-1,"p"] < .05) {

							foot.index <- .addFootnote(footnotes, text="Mauchly's test of sphericity indicates that the assumption of sphericity is violated (p < .05).")
							row.footnotes <- list(SS=list(foot.index), df=list(foot.index), MS=list(foot.index), F=list(foot.index), p=list(foot.index))

						}

						if (!options$sphericityCorrections || cor == "empty") {

							row <- list("case"=terms.normal[[i]][j], "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup, .footnotes=row.footnotes)
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
							}

						} else if (cor == "None") {

							row <- list("case"=terms.normal[[i]][j], "cor"="None", "SS"=SS, "df"=df, "MS"=MS, "F"=F, "p"=p, ".isNewGroup" = (newGroup && counter == 1), ".isNewSubGroup" = (!newGroup && counter == 1 && length(corrections) > 1), .footnotes=row.footnotes)
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
							}

						} else if (cor == "Greenhouse-Geisser") {

							dfGG <- df * stateSphericity[i-1,"GG"]
							MSGG <- SS / dfGG
							dfRGG <- dfR * stateSphericity[i-1,"GG"]

							if (F != "NaN") {
								pGG <-  pf(F,dfGG,dfRGG,lower.tail=FALSE)
							} else {
								pGG <- .clean(NaN)
							}

							row <- list("case"=terms.normal[[i]][j], "cor"="Greenhouse-Geisser", "SS"=SS, "df"=dfGG, "MS"=MSGG, "F"=F, "p"=pGG, ".isNewGroup" = (newGroup && counter == 1), ".isNewSubGroup" = (!newGroup && counter == 1 && length(corrections) > 1), .footnotes=row.footnotes)
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
							}

						} else if (cor == "Huynh-Feldt") {

							dfHF <- df * stateSphericity[i-1,"HF"]
							MSHF <- SS / dfHF
							dfRHF <- dfR * stateSphericity[i-1,"HF"]

							if (F != "NaN") {
								pHF <- pf(F,dfHF,dfRHF,lower.tail=FALSE)
							} else {
								pHF <- .clean(NaN)
							}

							row <- list("case"=terms.normal[[i]][j], "cor"="Huynh-Feldt", "SS"=SS, "df"=dfHF, "MS"=MSHF, "F"=F, "p"=pHF, ".isNewGroup" = (newGroup && counter == 1), ".isNewSubGroup" = (!newGroup && counter == 1 && length(corrections) > 1), .footnotes=row.footnotes)
							if (options$VovkSellkeMPR){
								row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
							}

						}


						if (options$effectSizeEstimates) {

							modelTermsCases <- strsplit(terms.base64[[i]],":")

							indices <- c()
							for (case in .indices(terms.base64[[i]])) {
								indices <- c(indices, which(unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCases[[case]])))))
							}

							if (result[index,"Error SS"] > 0) {

								SSr <- result[index,"Error SS"]
								SSt <- sum(result[indices,"Sum Sq"]) + SSr
								MSr <- SSr/result[index,"den Df"]
								row[["eta"]] <- SS / SSt
								row[["partialEta"]] <- SS / (SS + SSr)
								n <- result[1, 'den Df'] + 1
								MSm <- result[index, "Sum Sq"] / result[index, "num Df"] 
								MSb <- result[1, 'Error SS'] / (n - 1)
								omega <- (df / (n * (df + 1)) * (MSm - MSr)) / 
								         (MSr + ((MSb - MSr) / (df + 1)) +
						             (df / (n * (df + 1))) * (MSm - MSr))

								if (omega < 0) {
									row[["omega"]] <- 0
								} else {
									row[["omega"]] <- omega
								}

							} else {

								row[["eta"]] <- .clean(NaN)
								row[["partialEta"]] <- .clean(NaN)
								row[["omega"]] <- .clean(NaN)

							}
						}

						anova.rows[[length(anova.rows) + 1]] <- row

					}
				}

				modelTermsCase <- unlist(strsplit(termsRM.base64[[i-1]], ":"))
				indexResidual <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))

				SS <- result[indexResidual,"Error SS"]
				df <- result[indexResidual,"den Df"]

				if (SS == 0 && df == 0) {
					MS <- .clean(NaN)
				} else {
					MS <- SS / df
				}

				counter <- 0

				for (cor in corrections) {

					counter <- counter + 1

					if (!options$sphericityCorrections || cor == "empty") {

						row <- list("case"="Residual", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = TRUE)
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}

					} else if (cor == "None") {

						row <- list("case"="Residual", "cor"="None", "SS"=SS, "df"=df, "MS"=MS, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = (counter == 1))
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}

					} else if (cor == "Greenhouse-Geisser") {

						dfGG <- df * stateSphericity[i-1,"GG"]

						if (SS > 0) {
							MSGG <- SS / dfGG
						} else {
							MSGG <- .clean(NaN)
						}

						row <- list("case"="Residual", "cor"="Greenhouse-Geisser", "SS"=SS, "df"=dfGG, "MS"=MSGG, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = (counter == 1))
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}

					} else if (cor == "Huynh-Feldt") {

						dfHF <- df * stateSphericity[i-1,"HF"]

						if (SS > 0) {
							MSHF <- SS / dfHF
						} else {
							MSHF <- .clean(NaN)
						}

						row <- list("case"="Residual", "cor"="Huynh-Feldt", "SS"=SS, "df"=dfHF, "MS"=MSHF, "F"="", "p"="", "eta"="", "partialEta"="", "omega" = "", ".isNewSubGroup" = (counter == 1))
						if (options$VovkSellkeMPR){
							row[["VovkSellkeMPR"]] <- ""
						}

					}

					anova.rows[[length(anova.rows) + 1]] <- row

				}
			}
		}

		anova[["data"]] <- anova.rows
		anova[["status"]] <- "complete"

	}

	anova[["footnotes"]] <- as.list(footnotes)

	list(result = anova, status = status)
}

.sphericityTest <- function(options, stateSphericity, perform, status) {

	sphericity <- list()

	sphericity[["title"]] <- "Test of Sphericity"


	if (options$VovkSellkeMPR) {
		fields <- list(
			list(name="case", type="string", title=""),
			list(name="W", type="number", format="sf:4;dp:3", title="Mauchly's W"),
			list(name="p", type="number", format="dp:3;p:.001"),
			list(name = "VovkSellkeMPR", title = "VS-MPR", type = "number", format = "sf:4;dp:3"),
			list(name="GG", type="number", format="sf:4;dp:3", title="Greenhouse-Geisser \u03B5"),
			list(name="HF", type="number", format="sf:4;dp:3", title="Huynh-Feldt \u03B5"))
	 } else {
		 fields <- list(
			 list(name="case", type="string", title=""),
			 list(name="W", type="number", format="sf:4;dp:3", title="Mauchly's W"),
			 list(name="p", type="number", format="dp:3;p:.001"),
			 list(name="GG", type="number", format="sf:4;dp:3", title="Greenhouse-Geisser \u03B5"),
			 list(name="HF", type="number", format="sf:4;dp:3", title="Huynh-Feldt \u03B5"))

	 }

	sphericity[["schema"]] <- list(fields=fields)

	footnotes <- .newFootnotes()

	modelDef <- .rmModelFormula(options)
	termsRM.base64 <- modelDef$termsRM.base64
	termsRM.normal <- modelDef$termsRM.normal

	sphericity.rows <- list()

	if (is.null(stateSphericity)) {

		for(i in .indices(termsRM.base64)) {

			if (i == 1) {
				newGroup <- TRUE
			} else {
				newGroup <- FALSE
			}

			row <- list("case"=termsRM.normal[[i]], "W"=".", "p"=".", "GG"=".", "HF"=".", ".isNewGroup" = newGroup)
			sphericity.rows[[length(sphericity.rows) + 1]] <- row

		}

		sphericity[["data"]] <- sphericity.rows

		if (status$error)
			sphericity[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)

		#		if (is.null(stateSphericity))
		#			sphericity[["error"]] <- list(errorType="badData", errorMessage="Could not estimate sphericity parameters due to singularity problems.")

	} else {

		for (i in .indices(stateSphericity$termsNormal)) {

			if (i == 1) {
				newGroup <- TRUE
			} else {
				newGroup <- FALSE
			}

			if (stateSphericity$twoLevels[i]) {

				foot.index <- .addFootnote(footnotes, text="Singular error SSP matrix: The repeated measure has only two levels, or more levels than observations. When the repeated measure has two levels, the assumption of sphericity is always met.")
				row.footnotes <- list(W=list(foot.index), p=list(foot.index), GG=list(foot.index), HF=list(foot.index))

			} else {

				row.footnotes <- NULL
			}


			row <- list("case"=stateSphericity$termsNormal[i], "W"=stateSphericity$W[i], "p"=.clean(stateSphericity$p[i]), "GG"=stateSphericity$GG[i],
						"HF"=stateSphericity$HF[i], ".isNewGroup" = newGroup, .footnotes=row.footnotes)

			if (options$VovkSellkeMPR){
			  row[["VovkSellkeMPR"]] <- .VovkSellkeMPR(row[["p"]])
			}

			sphericity.rows[[length(sphericity.rows) + 1]] <- row

		}

		sphericity[["data"]] <- sphericity.rows
		sphericity[["status"]] <- "complete"

	}

	sphericity[["footnotes"]] <- as.list(footnotes)

	list(result = sphericity, status = status)
}

.rmAnovaLevenesTable <- function(dataset, options, perform, status, stateLevene, model) {

	if (options$homogeneityTests == FALSE)
		return (list(result=NULL, status=status))

	levenes.table <- list()

	levenes.table[["title"]] <- "Test for Equality of Variances (Levene's)"

	fields <- list(
		list(name="case", type="string", title=""),
		list(name="F", type="number", format="sf:4;dp:3"),
		list(name="df1", type="number", format="dp:0"),
		list(name="df2", type="number", format="dp:0"),
		list(name="p", type="number", format="dp:3;p:.001"))

	if (options$VovkSellkeMPR) {
    fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
                                        title = "VS-MPR",
                                        type = "number",
                                        format = "sf:4;dp:3")
  }

	levenes.table[["schema"]] <- list(fields=fields)

	if (perform == "run" && status$ready && status$error == FALSE && length(options$betweenSubjectFactors) > 0) {

		levenes.rows <- list()

		for (i in .indices(options$repeatedMeasuresCells)) {

			if (i == 1) {
				newGroup <- TRUE
			} else {
				newGroup <- FALSE
			}
		  
		  interaction <- paste(.v(options$betweenSubjectFactors), collapse=":", sep="")
		  if( length(options$covariates) > 0 ){
		    covterms <- paste(.v(options$covariates), collapse="+", sep="")
		    combterms <- paste(c(interaction,covterms), collapse="+", sep="")
		    levene.def <- paste(.v(options$repeatedMeasuresCells[i]), "~", combterms)
		    }else{levene.def <- paste(.v(options$repeatedMeasuresCells[i]), "~", interaction)}
		 
			levene.formula <- as.formula(levene.def)
# 			r <- car::leveneTest(levene.formula, dataset, center = "mean")
		  
		  dummyAov <- aov(levene.formula, data = dataset, qr = T)
		  resids <- abs(dummyAov$residuals)
		  levene.def <- paste("resids", "~", interaction)
		  levene.formula <- as.formula(levene.def)
		  
		  #r <- car::leveneTest(levene.formula, dataset, center = "mean")
		  r <- summary(aov(levene.formula, dataset))
		  error <- base::tryCatch(summary(aov(levene.formula, dataset)),error=function(e) e, warning=function(w) w)
			
			row <- list("case"=options$repeatedMeasuresCells[i],"F"=.clean(r[[1]]$`F value`[1]), "df1"=r[[1]]$Df[1], "df2"=r[[1]]$Df[2], "p"=.clean(r[[1]]$`Pr(>F)`[1]), ".isNewGroup"=newGroup)

			
			if (options$VovkSellkeMPR) {
				row["VovkSellkeMPR"] <- .VovkSellkeMPR(row[["p"]])
		  }

			levenes.rows[[length(levenes.rows) + 1]] <- row

		}

		levenes.table[["data"]] <- levenes.rows
		levenes.table[["status"]] <- "complete"

	} else {

		levenes.rows <- list()

		for (i in .indices(options$repeatedMeasuresCells)) {

			if (i == 1) {
				newGroup <- TRUE
			} else {
				newGroup <- FALSE
			}

			row <- list("case"=options$repeatedMeasuresCells[i], "F"=".", "df1"=".", "df2"=".", "p"=".", ".isNewGroup"=newGroup)
			if (options$VovkSellkeMPR) {
				row["VovkSellkeMPR"] <- "."
		  }
			levenes.rows[[length(levenes.rows) + 1]] <- row

		}

		levenes.table[["data"]] <- levenes.rows
	}

	if (length(options$betweenSubjectFactors) == 0)
		levenes.table[["error"]] <- list(errorType="badData", errorMessage="No between subjects factors specified.")

	if (status$error)
		levenes.table[["error"]] <- list(error="badData")

	if (perform == "run" && status$ready && status$error == FALSE) {

		stateLevene <- levenes.table

	} else {

		stateLevene <- NULL

	}

	list(result=levenes.table, status=status, stateLevene=stateLevene)
}

.rmAnovaContrastTable <- function(options, perform, status, stateContrasts) {

	contrastTables <- list()

	for (contrast in options$contrasts) {
	  
	  if (! contrast$variable %in% names(stateContrasts)) {
	    next
	  }

		if (contrast$contrast != "none") {

			contrastTable <- list()

			contrastType <- unlist(strsplit(contrast$contrast,""))
			contrastType[1] <- toupper(contrastType[1])
			contrastType <- paste(contrastType, collapse="")

			contrastTable[["title"]] <- paste(contrastType, " Contrast", " - ",  contrast$variable, sep="")
			contrastTable[["name"]] <- paste(contrastType, "Contrast_",  contrast$variable, sep="")

			fields <- list(
				list(name="Comparison", type="string"),
				list(name="Estimate", type="number", format="sf:4;dp:3"),
				list(name="SE", type="number", format="sf:4;dp:3"),
				list(name="t", type="number", format="sf:4;dp:3"),
				list(name="p", type="number", format="dp:3;p:.001"))

			contrastTable[["schema"]] <- list(fields=fields)

			rows <- list()

			if (!is.null(stateContrasts)) {

				result <- stateContrasts[[contrast$variable]][[contrast$contrast]]

				for (i in 1:length(result$estimate)) {

					row <- list()

					row[["Comparison"]] <- .unv(result$contrast[i])
					row[["Estimate"]] <- .clean(as.numeric(result$estimate[i]))
					row[["SE"]] <- .clean(as.numeric(result$SE[i]))
					if ("z.ratio" %in% names(result)) {
					  contrastTable$schema$fields[[4]]$name <- "z"
					  row[["z"]] <- .clean(as.numeric(result$z.ratio[i]))
					} else {
					  row[["t"]] <- .clean(as.numeric(result$t.ratio[i]))
					}
					row[["p"]] <- .clean(as.numeric(result$p.value[i]))

					if(length(rows) == 0)  {
						row[[".isNewGroup"]] <- TRUE
					} else {
						row[[".isNewGroup"]] <- FALSE
					}

					rows[[length(rows)+1]] <- row
				}
			}

			contrastTable[["data"]] <- rows
			contrastTable[["status"]] <- "complete"

			if (status$error)
				contrastTable[["error"]] <- list(errorType="badData")

			contrastTables[[length(contrastTables)+1]] <- contrastTable
		}
	}

	list(result=contrastTables, status=status)
}

.rmAnovaPostHocTable <- function(dataset, options, perform, status, statePostHoc) {

	posthoc.variables <- unlist(options$postHocTestsVariables)
 
	posthoc.tables <- list()
	
	for (posthoc.var in posthoc.variables) {

		posthoc.table <- list()

		posthoc.table[["title"]] <- paste("Post Hoc Comparisons - ", posthoc.var, sep="")
		posthoc.table[["name"]] <- paste("postHoc_", posthoc.var, sep="")

		fields <- list(
			list(name="(I)", title="", type="string", combine=TRUE),
			list(name="(J)", title="", type="string"),
			list(name="Mean Difference", type="number", format="sf:4;dp:3"),
			list(name="SE", type="number", format="sf:4;dp:3"),
			list(name="t", type="number", format="sf:4;dp:3"))
		
		if (options$postHocTestEffectSize) {
		  fields[[length(fields) + 1]] <- list(name="Cohen's d", title="Cohen's d", type="number", format="sf:4;dp:3")
		  posthoc.table[["footnotes"]] <- list(list(symbol="<i>Note.</i>", 
		                                            text="Cohen's d does not correct for multiple comparisons."))
		}
		
		if (options$postHocTestsTukey)
			fields[[length(fields) + 1]] <- list(name="tukey", title="p<sub>tukey</sub>", type="number", format="dp:3;p:.001")

		if (options$postHocTestsScheffe)
			fields[[length(fields) + 1]] <- list(name="scheffe", title="p<sub>scheffe</sub>", type="number", format="dp:3;p:.001")

		if (options$postHocTestsBonferroni)
			fields[[length(fields) + 1]] <- list(name="bonferroni", title="p<sub>bonf</sub>", type="number", format="dp:3;p:.001")

		if (options$postHocTestsHolm)
			fields[[length(fields) + 1]] <- list(name="holm",title="p<sub>holm</sub>", type="number", format="dp:3;p:.001")

		posthoc.table[["schema"]] <- list(fields=fields)

		rows <- list()

		if (posthoc.var %in% options$betweenSubjectFactors) {

			variable.levels <- levels(dataset[[ .v(posthoc.var) ]])

		} else {

			variable.levels <- options$repeatedMeasuresFactors[[which(lapply(options$repeatedMeasuresFactors, function(x) x$name) == posthoc.var)]]$levels

		}
		# index <- 0
		for (i in 1:length(variable.levels)) {
			for (j in .seqx(i+1, length(variable.levels))) {

				row <- list("(I)"=variable.levels[[i]], "(J)"=variable.levels[[j]])

				pTukey <- ""
				pScheffe <- ""
				pBonf <- ""
				pHolm <- ""
				effectSize <- ""
				
				if (!is.null(statePostHoc)) {

					if (length(class(statePostHoc[[posthoc.var]]$resultBonf)) == 1 && class(statePostHoc[[posthoc.var]]$resultBonf) == "try-error") {

						md <- ""
						SE  <- ""
						t <- ""
						p  <- 1

						posthoc.table[["footnotes"]] <- list(list(symbol="<i>Note.</i>", text="Some comparisons could not be performed. Possibly too few samples."))

					} else {

						for (c in 1:length(statePostHoc[[posthoc.var]]$comparisons)) {
							if (all(statePostHoc[[posthoc.var]]$comparisons[[c]] %in% c(.v(variable.levels[[i]]), .v(variable.levels[[j]])))) {
							  index <- c
							  
								reverse <- TRUE
			
								if (statePostHoc[[posthoc.var]]$comparisons[[c]][1] == .v(variable.levels[[i]]))
									reverse <- FALSE
							}
						}
					  # index <- index + 1
					  
					  
					  
						if (reverse) {
							md <- .clean(-as.numeric(statePostHoc[[posthoc.var]]$resultBonf$estimate[index]))
						} else {
							md <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultBonf$estimate[index]))
						}

						SE  <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultBonf$SE[index]))

						if (reverse) {
						  
						  if ("z.ratio" %in% names(statePostHoc[[posthoc.var]]$resultBonf)) {
						    posthoc.table$schema$fields[[5]]$name <- "z"
						    z <- .clean(-as.numeric(statePostHoc[[posthoc.var]]$resultBonf$z.ratio[index]))
						  } else {
						    t <- .clean(-as.numeric(statePostHoc[[posthoc.var]]$resultBonf$t.ratio[index]))
						  }

						} else {
						  
						  if ("z.ratio" %in% names(statePostHoc[[posthoc.var]]$resultBonf)) {
						    posthoc.table$schema$fields[[5]]$name <- "z"
						    z <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultBonf$z.ratio[index]))
						  } else {
						    t <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultBonf$t.ratio[index]))
						  }
						  
						}

						if (options$postHocTestEffectSize) 
						  effectSize <- .clean(t/sqrt(nrow(dataset)))
						
						if (options$postHocTestsTukey){
							pTukey <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultTukey$p.value[index]))
							if(!is.numeric(statePostHoc[[posthoc.var]]$resultTukey$p.value[index])){
							  posthoc.table[["footnotes"]] <- list(list(symbol="<i>Note.</i>", 
							                                            text="Tukey corrected p-values are not appropriate for repeated measures post-hoc tests (Maxwell, 1980; Field, 2012)."))
							  }
						}

						if (options$postHocTestsScheffe){
							pScheffe <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultScheffe$p.value[index]))
							if(!is.numeric(statePostHoc[[posthoc.var]]$resultScheffe$p.value[index])){
							  posthoc.table[["footnotes"]] <- list(list(symbol="<i>Note.</i>", 
							                                            text="Scheffe corrected p-values are not appropriate for repeated measures post-hoc tests (Maxwell, 1980; Field, 2012)."))}
							  
						}
							

						if (options$postHocTestsBonferroni)
							pBonf <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultBonf$p.value[index]))

						if (options$postHocTestsHolm)
							pHolm <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultHolm$p.value[index]))
					}

					row[["Mean Difference"]] <- md
					row[["SE"]]  <- SE
					if (posthoc.table$schema$fields[[5]]$name == "z") {
					  row[["z"]] <- z
					} else {
					  row[["t"]] <- t
					}
					row[["Cohen's d"]] <- effectSize
					row[["tukey"]] <- pTukey
					row[["scheffe"]] <- pScheffe
					row[["bonferroni"]] <- pBonf
					row[["holm"]] <- pHolm

					posthoc.table[["status"]] <- "complete"

				}

				if(length(rows) == 0)  {
					row[[".isNewGroup"]] <- TRUE
				} else {
					row[[".isNewGroup"]] <- FALSE
				}

				rows[[length(rows)+1]] <- row
			}
		}

		posthoc.table[["data"]] <- rows
		posthoc.table[["status"]] <- "complete"

		if (status$error)
			posthoc.table[["error"]] <- list(errorType="badData")

		posthoc.tables[[length(posthoc.tables)+1]] <- posthoc.table
	}

	list(result=posthoc.tables, status=status)
}

.rmAnovaDescriptivesTable <- function(dataset, options, perform, status, stateDescriptivesTable) {

	if (options$descriptives == FALSE)
		return(list(result=NULL, status=status))

	rmFactors <- c()
	rmLevels <- list()

	for (i in .indices(options$repeatedMeasuresFactors)) {

		rmFactors[i] <- options$repeatedMeasuresFactors[[i]]$name
		rmLevels[[i]] <- options$repeatedMeasuresFactors[[i]]$levels

	}

	bsFactors <- c()
	bsLevels <- list()

	for (i in .indices(options$betweenSubjectFactors)) {

		bsFactors[i] <- options$betweenSubjectFactors[i]
		bsLevels[[i]] <- levels(dataset[[ .v(options$betweenSubjectFactors[i]) ]])

	}

	factors <- c(rmFactors, bsFactors)
	lvls <- c(rmLevels, bsLevels)

	descriptives.table <- list()

	descriptives.table[["title"]] <- "Descriptives"

	fields <- list()

	for (variable in factors) {

		name <- paste(".", variable, sep="")  # in case variable is "Mean", "SD" or "N"
		fields[[length(fields)+1]] <- list(name=name, type="string", title=variable, combine=TRUE)

	}

	fields[[length(fields)+1]] <- list(name="Mean", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="SD", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="N", type="number", format="dp:0")

	descriptives.table[["schema"]] <- list(fields=fields)

	cases <- rev(expand.grid(rev(lvls)))

	namez <- unlist(factors)
	column.names <- paste(".", namez, sep="")

	if (length(factors) > 0) {

		rows <- list()

		if (perform == "run" && status$ready && status$error == FALSE) {

			dataset <- .shortToLong(dataset, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, options$betweenSubjectFactors)

			for (i in 1:dim(cases)[1]) {

				row <- list()

				for (j in 1:dim(cases)[2])
					row[[ column.names[[j]] ]] <- as.character(cases[i, j])

				sub  <- eval(parse(text=paste("dataset$", .v(namez), " == \"", row, "\"", sep="", collapse=" & ")))

				data <- base::subset(dataset, sub, select="dependent")[[1]]

				N <- base::length(data)

				row[["N"]] <- N

				if (N == 0) {

					row[["Mean"]] <- ""
					row[["SD"]]   <- ""

				} else if (N == 1) {

					row[["Mean"]] <- data
					row[["SD"]]   <- ""

				} else {

					row[["Mean"]] <- base::mean(data)
					row[["SD"]]   <- stats::sd(data)
				}

				if(cases[i,dim(cases)[2]] == lvls[[ dim(cases)[2] ]][[1]]) {
					row[[".isNewGroup"]] <- TRUE
				} else {
					row[[".isNewGroup"]] <- FALSE
				}

				rows[[i]] <- row
			}

		} else {

			for (i in 1:dim(cases)[1]) {

				row <- list()

				for (j in 1:dim(cases)[2])
					row[[ column.names[[j]] ]] <- as.character(cases[i, j])

				if(cases[i,dim(cases)[2]] == lvls[[ dim(cases)[2] ]][[1]]) {
					row[[".isNewGroup"]] <- TRUE
				} else {
					row[[".isNewGroup"]] <- FALSE
				}

				rows[[i]] <- row
			}
		}

		descriptives.table[["data"]] <- rows

		if (perform == "run" && status$ready && status$error == FALSE)
			descriptives.table[["status"]] <- "complete"
	}

	if (status$error)
		descriptives.table[["error"]] <- list(error="badData")

	if (perform == "run" && status$ready && status$error == FALSE) {

		stateDescriptivesTable <- descriptives.table

	} else {

		stateDescriptivesTable <- NULL

	}

	list(result=descriptives.table, status=status, stateDescriptivesTable=stateDescriptivesTable)
}

.rmAnovaDescriptivesPlot <- function(dataset, options, perform, status, stateDescriptivesPlot) {

	descriptivesPlotList <- list()

	if (perform == "run" && status$ready && !status$error && options$plotHorizontalAxis != "") {

		dataset <- .shortToLong(dataset, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, options$betweenSubjectFactors)

		groupVars <- c(options$plotHorizontalAxis, options$plotSeparateLines, options$plotSeparatePlots)
		groupVars <- groupVars[groupVars != ""]
		groupVarsV <- .v(groupVars)

		betweenSubjectFactors <- groupVars[groupVars %in% options$betweenSubjectFactors]
		repeatedMeasuresFactors <- groupVars[groupVars %in% sapply(options$repeatedMeasuresFactors,function(x)x$name)]
    
		usePooledSE <- ifelse(is.null(options$usePooledStandErrorCI), FALSE, options$usePooledStandErrorCI)
		
		if (length(repeatedMeasuresFactors) == 0) {

			summaryStat <- .summarySE(as.data.frame(dataset), measurevar = "dependent", groupvars = .v(betweenSubjectFactors),
							conf.interval = options$confidenceIntervalInterval, na.rm = TRUE, .drop = FALSE, errorBarType = options$errorBarType, 
							usePooledSE=usePooledSE)

		} else {

			summaryStat <- .summarySEwithin(as.data.frame(dataset), measurevar="dependent", betweenvars=.v(betweenSubjectFactors), withinvars=.v(repeatedMeasuresFactors),
							idvar="subject", conf.interval=options$confidenceIntervalInterval, na.rm=TRUE, .drop=FALSE, errorBarType=options$errorBarType, 
							usePooledSE=usePooledSE)

		}

		if ( options$plotHorizontalAxis != "" ) {
			colnames(summaryStat)[which(colnames(summaryStat) == .v(options$plotHorizontalAxis))] <- "plotHorizontalAxis"
		}

		if ( options$plotSeparateLines != "" ) {
			colnames(summaryStat)[which(colnames(summaryStat) == .v(options$plotSeparateLines))] <- "plotSeparateLines"
		}

		if ( options$plotSeparatePlots != "" ) {
			colnames(summaryStat)[which(colnames(summaryStat) == .v(options$plotSeparatePlots))] <- "plotSeparatePlots"
		}

		base_breaks_x <- function(x){
			b <- unique(as.numeric(x))
			d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
			list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1))
		}

		base_breaks_y <- function(x, plotErrorBars){
			if (plotErrorBars) {
				ci.pos <- c(x[,"dependent"], x[,"ciLower"],x[,"ciUpper"])
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

		for (i in 1:nPlots) {

			descriptivesPlot <- list()

			if (options$plotSeparateLines != "") {

				descriptivesPlot[["width"]] <- options$plotWidthDescriptivesPlotLegend
				descriptivesPlot[["height"]] <- options$plotHeightDescriptivesPlotLegend
				descriptivesPlot[["custom"]] <- list(width="plotWidthDescriptivesPlotLegend", height="plotHeightDescriptivesPlotLegend")

			} else {

				descriptivesPlot[["width"]] <- options$plotWidthDescriptivesPlotNoLegend
				descriptivesPlot[["height"]] <- options$plotHeightDescriptivesPlotNoLegend
				descriptivesPlot[["custom"]] <- list(width="plotWidthDescriptivesPlotNoLegend", height="plotHeightDescriptivesPlotNoLegend")

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

			if (options$plotErrorBars) {

				pd <- ggplot2::position_dodge(.2)
				p = p + ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower,
															ymax=ciUpper),
															colour="black", width=.2, position=pd)

			} else {

				pd <- ggplot2::position_dodge(0)

			}

			p <- p + ggplot2::geom_line(position=pd, size = .7) +
				ggplot2::geom_point(position=pd, size=4) +
				ggplot2::scale_fill_manual(values = c(rep(c("white","black"),5),rep("grey",100)), guide=ggplot2::guide_legend(nrow=10)) +
				ggplot2::scale_shape_manual(values = c(rep(c(21:25),each=2),21:25,7:14,33:112), guide=ggplot2::guide_legend(nrow=10)) +
				ggplot2::scale_color_manual(values = rep("black",200),guide=ggplot2::guide_legend(nrow=10)) +
				ggplot2::ylab(options$labelYAxis) +
				ggplot2::xlab(options$plotHorizontalAxis) +
				ggplot2::labs(shape=options$plotSeparateLines, fill=options$plotSeparateLines) +
				base_breaks_y(summaryStat, options$plotErrorBars) +
				base_breaks_x(summaryStatSubset[,"plotHorizontalAxis"])
				
			p <- JASPgraphs::themeJasp(p, legend.position="right")

			if (nPlots > 1) {
				descriptivesPlot[["title"]] <- paste(options$plotSeparatePlots,": ",subsetPlots[i], sep = "")
			} else {
				descriptivesPlot[["title"]] <- "Descriptives Plot"
			}

			if (options$plotSeparateLines != "") {

				# image <- .beginSaveImage(options$plotWidthDescriptivesPlotLegend, options$plotHeightDescriptivesPlotLegend)
				content <- .writeImage(width = options$plotWidthDescriptivesPlotLegend, 
									   height = options$plotHeightDescriptivesPlotLegend, 
									   plot = p, obj = TRUE)

			} else {

				# image <- .beginSaveImage(options$plotWidthDescriptivesPlotNoLegend, options$plotHeightDescriptivesPlotNoLegend)
				content <- .writeImage(width = options$plotWidthDescriptivesPlotNoLegend, 
									   height = options$plotHeightDescriptivesPlotNoLegend, 
									   plot = p, obj = TRUE)

			}

			# content <- .endSaveImage(image)
			
			descriptivesPlot[["convertible"]] <- TRUE
			descriptivesPlot[["obj"]] <- content[["obj"]]
			descriptivesPlot[["data"]] <- content[["png"]]

			# descriptivesPlot[["data"]] <- content
			descriptivesPlot[["status"]] <- "complete"

			descriptivesPlotList[[i]] <- descriptivesPlot

		}

		stateDescriptivesPlot <- descriptivesPlotList

	} else if (options$plotHorizontalAxis != "") {

		if (options$plotSeparatePlots != "") {

			repeatedMeasuresNames <- sapply(options$repeatedMeasuresFactors, function(x) x$name)
			repeatedMeasuresLevels <- lapply(options$repeatedMeasuresFactors, function(x) x$levels)

			if (sum(options$plotSeparatePlots == repeatedMeasuresNames) > 0) {

				index <- which(options$plotSeparatePlots == repeatedMeasuresNames)
				nPlots <- length(unlist(repeatedMeasuresLevels[[index]]))

			} else {

				nPlots <- length(levels(dataset[[ .v(options$plotSeparatePlots) ]]))

			}

		} else {

			nPlots <- 1

		}

		for (i in 1:nPlots) {

			descriptivesPlot <- list()

			if (nPlots == 1) {
				descriptivesPlot[["title"]] <- "Descriptives Plot"
			} else {
				descriptivesPlot[["title"]] <- ""
			}

			if (options$plotSeparateLines != "") {

				descriptivesPlot[["width"]] <- options$plotWidthDescriptivesPlotLegend
				descriptivesPlot[["height"]] <- options$plotHeightDescriptivesPlotLegend
				descriptivesPlot[["custom"]] <- list(width="plotWidthDescriptivesPlotLegend", height="plotHeightDescriptivesPlotLegend")

			} else {

				descriptivesPlot[["width"]] <- options$plotWidthDescriptivesPlotNoLegend
				descriptivesPlot[["height"]] <- options$plotHeightDescriptivesPlotNoLegend
				descriptivesPlot[["custom"]] <- list(width="plotWidthDescriptivesPlotNoLegend", height="plotHeightDescriptivesPlotNoLegend")

			}

			descriptivesPlot[["data"]] <- ""

			if (status$error)
				descriptivesPlot[["error"]] <- list(errorType="badData")

			descriptivesPlotList[[i]] <- descriptivesPlot
		}

		stateDescriptivesPlot <- NULL

	}

	list(result=descriptivesPlotList, status=status, stateDescriptivesPlot=stateDescriptivesPlot)
}

.rmAnovaSimpleEffects <- function(dataset, options, perform, fullModel, fullAnovaTableWithin, 
                                  fullAnovaTableBetween, status, singular, stateSimpleEffects) {
  
  if (identical(options$simpleFactor, "") | identical(options$moderatorFactorOne, "")) {
    return (list(result=NULL, status=status))
  }
  
  
  terms <- c(options$moderatorFactorOne,options$moderatorFactorTwo)
  terms.base64 <- c()
  terms.normal <- c()
  simpleFactor.base64 <- .v(options[['simpleFactor']])
  simpleFactor <- options[['simpleFactor']]
  moderatorFactorOne <- options[['moderatorFactorOne']]
  moderatorFactorTwo <- options[['moderatorFactorTwo']]
  
  for (term in terms) {
    
    components <- unlist(term)
    term.base64 <- paste(.v(components), collapse=":", sep="")
    term.normal <- paste(components, collapse=" \u273B ", sep="")
    
    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }
  
  simpleEffectsTable <- list()
  simpleEffectsTable[["title"]] <- paste("Simple Main Effects - ", simpleFactor, sep = "")
  
  fields <- list(
    list(name="ModOne", type="string", combine = TRUE, title = paste0("Level of ", terms.normal[1])),
    list(name="ModTwo", type="string", combine = TRUE, title = paste0("Level of ", terms.normal[2])),
    list(name="SumSquares", type="number", format="sf:4;dp:3", title = "Sum of Squares"),
    list(name="df", type="integer", title = "df"),
    list(name="MeanSquare", type="number", format="sf:4;dp:3", title = "Mean Square"),
    list(name="F", type="number", format="sf:4;dp:3", title = "F"),
    list(name="p", type="number", format="dp:3;p:.001", title = "p"))
  
  # If there is no second moderator, so remove from table:
  if (identical(options$moderatorFactorTwo, ""))
    fields <- fields[-2]
  
  footnotes <- .newFootnotes()
  
  simpleEffectsTable[["schema"]] <- list(fields=fields)
  
  if (perform == "run" && status$ready && status$error == FALSE && !isTryError(fullModel))  {
  
    isMixedAnova <-   length(options[['betweenSubjectFactors']]) > 0
    isSimpleFactorWithin <- !simpleFactor %in% unlist(options[['betweenModelTerms']] )
    isModeratorOneWithin <- !moderatorFactorOne %in% unlist(options[['betweenModelTerms']] )
    isModeratorTwoWithin <- !moderatorFactorTwo %in% unlist(options[['betweenModelTerms']] )
    errorIndexWithin <- length(fullAnovaTableWithin$data)
    errorIndexBetween <-  length(fullAnovaTableBetween$data)
    ssWithin <- fullAnovaTableWithin$data[[errorIndexWithin]]$SS
    ssBetween <- fullAnovaTableBetween$data[[errorIndexBetween]]$SS
    dfWithin <- fullAnovaTableWithin$data[[errorIndexWithin]]$df
    dfBetween <- fullAnovaTableBetween$data[[errorIndexBetween]]$df
    fullAnovaTable <- fullAnovaTableWithin
    tableCounter <- 1
    if(isMixedAnova & isSimpleFactorWithin){
      fullAnovaMS <- ssWithin / dfWithin
      fullAnovaDf <- dfWithin
    } else if(isMixedAnova & !isSimpleFactorWithin){
      fullAnovaMS <- ssBetween / dfBetween
      fullAnovaDf <- dfBetween
    } else {
      fullAnovaMS <- fullAnovaTable$data[[length(fullAnovaTable$data)]]$MS 
      fullAnovaDf <- fullAnovaTable$data[[length(fullAnovaTable$data)]]$df
    }
    
  
    simpleEffectRows <- list()
    rows <- list()
    
    termsBothModerators <- as.vector(c(moderatorFactorOne, moderatorFactorTwo))
    if(identical(moderatorFactorTwo, "")) {
      termsBothModerators <- termsBothModerators[1]
    }
    
    lvls <- list()
    factors <- list()
    
    for (variable in termsBothModerators) {
      if (variable %in% unlist(options[['withinModelTerms']])) {
        whichFactor <- unlist(lapply(options[['repeatedMeasuresFactors']], 
                                     FUN = function(x){x$name == variable}))
        lvls[[variable]] <- options$repeatedMeasuresFactors[[which(whichFactor == TRUE)]]$levels 
      } else if (variable %in% unlist(options[['betweenModelTerms']])) {
        thisFactor <- dataset[[ .v(variable) ]]
        factors[[length(factors)+1]] <- thisFactor
        lvls[[variable]] <- levels(thisFactor)
      }
    }
    #
    allNames <- unlist(lapply(options[['repeatedMeasuresFactors']], function(x) x$name)) # Factornames 

    # make separate covariate dataframe to avoid mismatching dataframe names
    covDataset <- dataset[.v(options[['covariates']])]
    wideDataset <- fullModel$data$wide[,(-1)]

    covariatesInModel <- ifelse(length(options[['covariates']]) > 0, TRUE, FALSE)
    if (covariatesInModel) {
      dataset <- dataset[, names(dataset) != (.v(options[['covariates']]))]
      wideDataset <- wideDataset[ -match(covDataset, wideDataset)]
    }
    # Following steps to make sure column ordering is the same for the datasets
    
    factorNamesV <- colnames(wideDataset)
    withinFactorNamesV <- factorNamesV[!(.unv(factorNamesV) %in% options[['betweenSubjectFactors']])] 
    
    withinOrder <- match(wideDataset, dataset)
    betweenOrder <- match(names(wideDataset), names(dataset))
    betweenOrder[is.na(betweenOrder)] <- withinOrder[!is.na(withinOrder)]
    fullOrder <- betweenOrder
    
    dataset <- dataset[fullOrder]
    # orderOfTerms <- unlist(options[['withinModelTerms']][[length(options[['withinModelTerms']])]]$components)
    orderOfTerms <- unlist(lapply(options$repeatedMeasuresFactors, function(x) x$name))
    
    indexofOrderFactors <- match(allNames,orderOfTerms)

    for (level in lvls[[1]]) {
      # For each level of the first moderator factor, take a subset of the dataset, and adjust the options object
      # Suboptions is the same as options, except that the first moderator factor has been removed as a predictor 
      # (because each subset only has one level of that factor). The same procedure is applied to the second moderator, if specified.
      
      subOptions <- options
      # subOptions[['covariates']] <- list()
      # Prepare the options object to handle the contional dataset
      # Based on whether the first moderator variable is within or between
      if (isModeratorOneWithin) {
        rmFactorIndex <- which(lapply(options[['repeatedMeasuresFactors']], 
                                      FUN = function(x){x$name == moderatorFactorOne}) == TRUE)
        splitNames <- unlist(lapply(strsplit(factorNamesV,  split = "_"), 
                                      FUN = function(x) x[indexofOrderFactors[rmFactorIndex]]))
        splitWithinNames <- unlist(lapply(strsplit(withinFactorNamesV,  split = "_"), 
                                      FUN = function(x) x[indexofOrderFactors[rmFactorIndex]]))
        subDataset <- dataset[, ((splitNames %in% .v(level)) | (names(dataset)) %in% .v(options[['betweenSubjectFactors']]))]
        subCovDataset <- covDataset
        subFactorNamesV <- factorNamesV[((splitNames %in% .v(level)) | (names(dataset)) %in% .v(options[['betweenSubjectFactors']]))]
        whichFactorsBesidesModerator <- !unlist(lapply((options[['withinModelTerms']]), FUN = function(x){grepl(moderatorFactorOne, x)}))
        subOptions[['withinModelTerms']] <- options[['withinModelTerms']][whichFactorsBesidesModerator]
        subOptions[['repeatedMeasuresFactors']] <- options[['repeatedMeasuresFactors']][-rmFactorIndex]
        subOptions[['repeatedMeasuresCells']]<- options$repeatedMeasuresCells[(splitWithinNames %in% .v(level))]
      } else {
        subDataset <- subset(dataset, dataset[terms.base64[1]] == level)
        if (covariatesInModel) {
          subCovDataset  <- subset(covDataset, dataset[terms.base64[1]] == level)
        } else {
          subCovDataset <- covDataset[dataset[terms.base64[1]] == level,] 
        }
        subFactorNamesV <- factorNamesV
        whichTermsBesidesModerator <- !unlist(lapply((options[['betweenModelTerms']]), FUN = function(x){grepl(moderatorFactorOne, x)}))
        whichFactorsBesidesModerator <- !unlist(lapply((options[['betweenSubjectFactors']]), FUN = function(x){grepl(moderatorFactorOne, x)}))
        
        subOptions[['betweenModelTerms']] <- options[['betweenModelTerms']][whichTermsBesidesModerator]
        subOptions[['betweenSubjectFactors']] <- options[['betweenSubjectFactors']][whichFactorsBesidesModerator]
      }
      areSimpleFactorCellsDropped <- ifelse(isSimpleFactorWithin, FALSE, (nrow(unique(subDataset[simpleFactor.base64])) <  
                                                                            nrow(unique(dataset[simpleFactor.base64]))))
      model <- NULL
      singular <- FALSE
      if (identical(moderatorFactorTwo, "")) {
        # This means there is only one moderator variable (i.e., one factor on which to condition)
        newGroup <- ifelse( level == lvls[[1]][1], TRUE, FALSE )
        if (nrow(subDataset) < 3 || areSimpleFactorCellsDropped ) {
          row <- list("ModOne"=level, "SumSquares"=".", "df"=".", "MeanSquare"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
          .addFootnote(footnotes, text = paste0("Not enough observations in cell ", level, " of ", subOptions$moderatorFactorOne), 
                       symbol = "<em>Note.</em>")
        } else {
          if (length(subOptions[['repeatedMeasuresFactors']]) > 0) {
            # There are still multiple levels of RM factors, so proceed with conditional RM ANOVA
            anovaModel <- .rmAnovaModel(cbind(subDataset, subCovDataset), subOptions, status = status)
            modelSummary <- anovaModel$model
            if (subOptions$sumOfSquares != "type1") {
              modOneIndex <- which(row.names(modelSummary) == .v(simpleFactor))
              df <- modelSummary[modOneIndex,'num Df']
              SS <- modelSummary[modOneIndex,'Sum Sq']  
              if (!options$poolErrorTermSimpleEffects) {
                fullAnovaMS <- modelSummary[modOneIndex,'Error SS'] / modelSummary[modOneIndex,'den Df']
                fullAnovaDf <- modelSummary[modOneIndex,'den Df']
              }
            } else {
              modelSummary <- modelSummary[[-1]][[1]]
              modOneIndex <- which(row.names(modelSummary) == .v(simpleFactor))
              df <- modelSummary[modOneIndex,'Df']
              SS <- modelSummary[modOneIndex,'Sum Sq']   
              if (!options$poolErrorTermSimpleEffects) {
                fullAnovaMS <- modelSummary['Residuals','Mean Sq']
                fullAnovaDf <- modelSummary['Residuals','Df']
              }
            }
           
          } else {
            # There is only one level of RM factor left, so proceed with conditional simple ANOVA
            subOptionsSimpleAnova <- subOptions
            subOptionsSimpleAnova['fixedFactors']  <- list(subOptions[['betweenSubjectFactors']])
            subOptionsSimpleAnova['modelTerms'] <- list(subOptions[['betweenModelTerms']])
            subOptionsSimpleAnova['dependent'] <-  options$repeatedMeasuresCells[splitWithinNames %in% .v(level)]
            
            anovaModel <- .anovaModel(cbind(subDataset, subCovDataset), options = subOptionsSimpleAnova)
            model <- anovaModel$model
            modelSummary <- summary(model)[[1]]
            modOneIndex <- which(subOptionsSimpleAnova[['fixedFactors']] == simpleFactor)
            df <- modelSummary$Df[modOneIndex]
            SS <- modelSummary$`Sum Sq`[modOneIndex]
            if (!options$poolErrorTermSimpleEffects) {
              fullAnovaMS <- modelSummary$`Mean Sq`[length(modelSummary$`Mean Sq`)]
              fullAnovaDf <- modelSummary$Df[length(modelSummary$Df)]
            }
          }
          MS <- SS / df
          F <- MS / fullAnovaMS
          p <- pf(F, df, fullAnovaDf, lower.tail = FALSE)
          row <- list("ModOne"=level, "SumSquares"=SS, "df"=df, "MeanSquare"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)
        }
        simpleEffectRows[[length(simpleEffectRows) + 1]] <- row
      } else {
        # This means there are two moderator variables (i.e., two factors on which to condition)
          for (levelTwo in lvls[[2]]) {
            newGroup <- ifelse( levelTwo == lvls[[2]][1], TRUE, FALSE )
            # Prepare the options object to handle the contional dataset
            # Based on whether the second moderator variable is within or between
            if (isModeratorTwoWithin) {
              rmFactorIndex <- which(lapply(options[['repeatedMeasuresFactors']], 
                                              FUN = function(x){x$name == moderatorFactorTwo}) == TRUE)
              splitNames <- unlist(lapply(strsplit(subFactorNamesV,  split = "_"), 
                                              FUN = function(x) x[indexofOrderFactors[rmFactorIndex]]))
              subWithinFactorNamesV <- subFactorNamesV[ !(names(subDataset) %in% .v(options[['betweenSubjectFactors']]))]
              splitWithinNames <- unlist(lapply(strsplit(subWithinFactorNamesV,  split = "_"), 
                                              FUN = function(x) x[indexofOrderFactors[rmFactorIndex]]))
              subSubDataset <- subDataset[, (splitNames %in% .v(levelTwo)) | (names(subDataset) %in% .v(subOptions[['betweenSubjectFactors']]))]
              subSubCovDataset <- subCovDataset
              subSubOptions <- subOptions
              whichFactorsBesidesModerator <- !unlist(lapply((subOptions[['withinModelTerms']]), FUN = function(x){grepl(moderatorFactorTwo, x)}))
              subSubOptions[['withinModelTerms']] <- subOptions[['withinModelTerms']][whichFactorsBesidesModerator]
              whichFactorToRemove <- which(lapply(subOptions[['repeatedMeasuresFactors']], 
                                                  FUN = function(x){x$name == moderatorFactorTwo}) == TRUE)
              subSubOptions[['repeatedMeasuresFactors']] <- subOptions[['repeatedMeasuresFactors']][-whichFactorToRemove]
              subSubOptions[['repeatedMeasuresCells']]<- subOptions$repeatedMeasuresCells[(splitWithinNames %in% .v(levelTwo))]

            } else {
              subSubDataset <- subset(subDataset, subDataset[terms.base64[2]] == levelTwo)
              if (covariatesInModel) {
                subSubCovDataset  <- subset(subCovDataset, subDataset[terms.base64[2]] == levelTwo)
              } else {
                subSubCovDataset <- subCovDataset[subDataset[terms.base64[2]] == levelTwo,] 
              }
              subSubOptions <- subOptions
              whichFactorsBesidesModerator <- !unlist(lapply((subOptions[['betweenModelTerms']]), FUN = function(x){grepl(moderatorFactorTwo, x)}))
              subSubOptions[['betweenModelTerms']] <- subOptions[['betweenModelTerms']][whichFactorsBesidesModerator]
            }

            areSimpleFactorCellsDropped <- ifelse(isSimpleFactorWithin, FALSE, (nrow(unique(subSubDataset[simpleFactor.base64])) <  
                                                                                  nrow(unique(dataset[simpleFactor.base64]))))
            
            if (nrow(subSubDataset) < 3 || areSimpleFactorCellsDropped) {
              row <- list("ModOne"=level, "ModTwo" = levelTwo, "SumSquares"=".", "df"=".", "MeanSquare"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
              .addFootnote(footnotes, text = paste0("Not enough observations in cell ",level, " of ", moderatorFactorOne, " and ",
                                                    levelTwo," of ", moderatorFactorTwo), symbol = "<em>Note.</em>")
            } else {
              if (length(subSubOptions[['repeatedMeasuresFactors']]) > 0) {
                # There are still multiple levels of RM factors, so proceed with conditional RM ANOVA
                anovaModel <- .rmAnovaModel(cbind(subSubDataset, subSubCovDataset), subSubOptions, status = status)
                modelSummary <- anovaModel$model
                if (subSubOptions$sumOfSquares != "type1") {
                  modTwoIndex <- which(row.names(modelSummary) == .v(simpleFactor))
                  df <- modelSummary[modTwoIndex,'num Df']
                  SS <- modelSummary[modTwoIndex,'Sum Sq']   
                  if (!options$poolErrorTermSimpleEffects) {
                    fullAnovaMS <- modelSummary[modTwoIndex,'Error SS'] / modelSummary[modTwoIndex,'den Df']
                    fullAnovaDf <- modelSummary[modTwoIndex,'den Df']
                  }
                } else {
                  modelSummary <- modelSummary[[-1]][[1]]
                  modTwoIndex <- which(row.names(modelSummary) == .v(simpleFactor))
                  df <- modelSummary[modTwoIndex,'Df']
                  SS <- modelSummary[modTwoIndex,'Sum Sq']   
                  if (!options$poolErrorTermSimpleEffects) {
                    fullAnovaMS <- modelSummary['Residuals','Mean Sq']
                    fullAnovaDf <- modelSummary['Residuals','Df']
                  }
                }
                
              } else {
                # There is only one level of RM factor left, so proceed with conditional simple ANOVA
                subSubOptionsSimpleAnova <- subSubOptions
                subSubOptionsSimpleAnova['fixedFactors']  <- subSubOptions[['betweenSubjectFactors']]
                subSubOptionsSimpleAnova['modelTerms'] <- list(subSubOptions[['betweenModelTerms']])
                subSubOptionsSimpleAnova['dependent'] <-  subSubOptions[['repeatedMeasuresCells']]
                anovaModel <- .anovaModel(cbind(subSubDataset, subSubCovDataset), options = subSubOptionsSimpleAnova)
                model <- anovaModel$model
                modelSummary <- summary(model)[[1]]
                modTwoIndex <- which(subSubOptionsSimpleAnova[['fixedFactors']] == simpleFactor)
                df <- modelSummary$Df[modTwoIndex]
                SS <- modelSummary$`Sum Sq`[modTwoIndex]
                if (!options$poolErrorTermSimpleEffects) {
                  fullAnovaMS <- modelSummary$`Mean Sq`[length(modelSummary$`Mean Sq`)]
                  fullAnovaDf <- modelSummary$Df[length(modelSummary$Df)]
                }
              }
              
              MS <- SS / df
              F <- MS / fullAnovaMS
              p <- pf(F, df, fullAnovaDf, lower.tail = FALSE)
              row <- list("ModOne"=level, "ModTwo" = levelTwo, "SumSquares"=SS, "df"=df, "MeanSquare"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)
            }
            simpleEffectRows[[length(simpleEffectRows) + 1]] <- row
          }
        }
      
      
    }
   
    simpleEffectsTable[["data"]] <- simpleEffectRows
    
    if (options$sumOfSquares == "type1") {
      
      .addFootnote(footnotes, text = "Type I Sum of Squares", symbol = "<em>Note.</em>")
      
    } else if (options$sumOfSquares == "type2") {
      
      .addFootnote(footnotes, text = "Type II Sum of Squares", symbol = "<em>Note.</em>")
      
    } else if (options$sumOfSquares == "type3") {
      
      .addFootnote(footnotes, text = "Type III Sum of Squares", symbol = "<em>Note.</em>")
      
    }
    
  } else {
    if(options$sumOfSquares == "type1" ) {
      .addFootnote(footnotes, text = "Simple effects not yet available for type 1 SS.", 
                   symbol = "<em>Note.</em>")  }
    simpleEffectsTable[["data"]]  <- list(list("ModOne"=terms.normal, "SumSquares"=".", "df"=".", "MeanSquare"=".", "F"=".", "p"=".", ".isNewGroup" = TRUE))
  }
  
  simpleEffectsTable[["footnotes"]] <- as.list(footnotes)
  simpleEffectsTable[["status"]] <- "complete"
  
  if (perform == "run" && status$ready && status$error == FALSE)  {
    
    stateSimpleEffects <- simpleEffectsTable
    
  } else {
    
    stateSimpleEffects <- NULL
    
  }
  simpleEffectsTable[["citation"]] <- list(
    "Howell, D. C. (2002). Statistical Methods for Psychology (8th. ed.). Pacific Grove, CA: Duxberry. "
  )
  
  list(result=simpleEffectsTable, status=status, stateSimpleEffects=stateSimpleEffects)
}

.rmAnovaFriedman <- function(dataset, fullModel, options, perform, status, singular, stateFriedman) {
  
  if (length(options$friedmanWithinFactor) == 0)
    return (list(result=NULL, status=status))

  withinTerms <- options$friedmanWithinFactor
  betweenTerm <- options$friedmanBetweenFactor
  
  withinTerms.base64 <- .v(withinTerms)
  betweenTerms.base64 <- .v(betweenTerm)
  
  result <- list()
  
  if( any(!(withinTerms %in% unlist(options$withinModelTerms))) | 
      (betweenTerm %in% unlist(options$withinModelTerms)) ) {
    status$error <- TRUE
    status$errorMessage <- "Please specify appropriate terms for the Friedman/Durbin test."
    result[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)
  }
  
  result[["title"]] <- paste("Friedman Test")
  result[["name"]] <- paste("friedmanTable")
  
  fields <- list()
  fields[[length(fields) + 1]] <- list(name="Factor", type="string")
  fields[[length(fields) + 1]] <- list(name="Chi-Squared", type="number", format="sf:4;dp:3")
  fields[[length(fields) + 1]] <- list(name="df", type="integer")
  fields[[length(fields) + 1]] <- list(name="p", type="number", format="dp:3;p:.001")
  fields[[length(fields) + 1]] <- list(name="Kendall's W", type="number", format="sf:4;dp:3")
  # fields[[length(fields) + 1]] <- list(name="Kendall's W corr.", type="number", format="sf:4;dp:3")
  
  footnotes <- .newFootnotes()

  rows <- list()
  
  if (perform == "run" && status$ready && status$error == FALSE)  {
    
    longData <- fullModel$data$long
    
    if (identical(betweenTerm, "")) {
      betweenTerms.base64 <- 'subject'
    }

    for (i in 1:length(withinTerms)) {
      
      groups <- as.factor(longData[, withinTerms.base64[i]])
      blocks <- as.factor(longData[, betweenTerms.base64])
      y <- longData[, 'dependent']
      
      useDurbin <- any(table(groups, blocks) != 1)
      
      t <- nlevels(groups)
      b <- nlevels(blocks)
      r <- unique(table(groups))
      k <- unique(table(blocks))
      
      
      if (length(r) == 1 & length(k) == 1) {
        rankPerBlock <- unlist(tapply(y, blocks, rank))
        rankPerGroup <- unlist(tapply(y, groups, rank))    

        rankJ <- tapply(rankPerBlock, groups, sum)    
        
        sumRanks <- sum(rankPerBlock^2)
        cVal <- (b * k * (k + 1)^2) / 4
        dVal <- sum(rankJ^2) - r * cVal
        testStatOne <- (t - 1) / (sumRanks - cVal) * dVal
        testStatTwo <- (testStatOne / (t - 1)) / ((b * k - b - testStatOne) / (b * k - b - t + 1))
        
        ## Code from PMCMRplus
        dfChi <- t - 1
        dfOneF <- k - 1
        dfTwoF <- b * k - b - t + 1 
        pValOne <- pchisq(testStatOne, dfChi, lower.tail = FALSE)
        pValTwo <- pf(testStatTwo, dfOneF, dfTwoF, lower.tail = FALSE)
        
        # Kendall W
        rankMatrixRM <- matrix(rankPerGroup, ncol = t)
        rowSumsMatrix <- rowSums(rankMatrixRM)
        nTies <- unlist(apply(rankMatrixRM, 2, function(x) {
          tab <- table(x)
          tab[tab > 1] }))
        nTies <- sum(nTies^3 - nTies)
        kendallW <- (sum(rowSumsMatrix^2) - sum(rowSumsMatrix)^2 / b) / (t^2 * (b^3 - b) / 12)
        kendallWcor <-(sum(rowSumsMatrix^2) - sum(rowSumsMatrix)^2 / b) / ((t^2 * (b^3 - b) - t * nTies) / 12)
        
        row <- list()
        
        row[["Factor"]] <- withinTerms[i]
        row[["Chi-Squared"]] <- .clean(testStatOne)
        row[["df"]] <- .clean(dfChi)
        row[["p"]] <- .clean(pValOne)
        row[["Kendall's W"]] <- .clean(kendallWcor)
        # row[["Kendall's W corr."]] <- .clean(kendallWcor)
        
                
        if (useDurbin) {
          result[["title"]] <- "Durbin Test"

          row[["F"]] <- .clean(testStatTwo)
          row[["df num"]] <- .clean(dfOneF)
          row[["df den"]] <- .clean(dfTwoF)
          row[["pF"]] <-.clean(pValTwo)
          
          if (i == 1) {
            fields[[length(fields) + 1]] <- list(name="F", type="number", format="sf:4;dp:3")
            fields[[length(fields) + 1]] <- list(name="df num", type="integer")
            fields[[length(fields) + 1]] <- list(name="df den", type="integer")
            fields[[length(fields) + 1]] <- list(name="pF", title="p<sub>F</sub>",type="number", format="dp:3;p:.001")
          }
          
        } 
        
        rows[[i]] <- row
        
      } else {
        status$error <- TRUE
        status$errorMessage <- "Specified ANOVA design is not balanced."
        result[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)
        
        row <- list()
        row[["Factor"]] <- "."
        row[["Statistic"]] <- "."
        row[["df"]] <- "."
        row[["p"]] <- "."
        
        rows[[i]] <- row
      }
    }
  } else {
    
    row <- list()
    row[["Factor"]] <- "."
    row[["Statistic"]] <- "."
    row[["df"]] <- "."
    row[["p"]] <- "."
    
    rows[[1]] <- row
  }

  
  result[["data"]] <- rows
  result[["status"]] <- "complete"
  
  result[["schema"]] <- list(fields=fields)
  result[["footnotes"]] <- as.list(footnotes)
  
  
  
  if (perform == "run" && status$ready && status$error == FALSE)  {
    
    stateFriedman <- result
    
  } else {
    
    stateFriedman <- NULL
    
  }
  
  list(result=result, status=status, stateFriedman=stateFriedman)
}

.rmAnovaConnoverTable <- function(dataset, options, perform, fullModel, status, stateConnover, singular) {
  
  if (options$connoverTest == FALSE | identical(options$friedmanWithinFactor, ""))
    return (list(result=NULL, status=status, stateConnover=NULL))
  
  groupingVariables <- unlist(options$friedmanWithinFactor)
  blockingVar <- ifelse( identical(options$friedmanBetweenFactor, ""), "subject", .v(options$friedmanBetweenFactor))
  
  connoverTables <- list()
  stateConnover <- list()
  
  for (groupingVar in groupingVariables) {
    
    connoverTable <- list()
    
    connoverTable[["title"]] <- paste("Connover's Post Hoc Comparisons - ", groupingVar, sep="")
    connoverTable[["name"]] <- paste("connoverTest_", groupingVar, sep="")
    
    fields <- list(
      list(name="(I)",title="", type="string", combine=TRUE),
      list(name="(J)",title="", type="string"),
      list(name="t",  title="T-Stat", type="number", format="sf:4;dp:3"),
      list(name="df", type="integer"),
      list(name="wA", title="W<sub>i</sub>", type="number", format="sf:4;dp:3"),
      list(name="wB", title="W<sub>j</sub>", type="number", format="sf:4;dp:3"),
      list(name="pval", title="p", type="number", format="dp:3;p:.001"),
      list(name="bonferroni", title="p<sub>bonf</sub>", type="number", format="dp:3;p:.001"),
      list(name="holm",title="p<sub>holm</sub>", type="number", format="dp:3;p:.001")
    )
    
    connoverTable[["schema"]] <- list(fields=fields)
    
    rows <- list()
    
    if (perform == "run" && status$ready && status$error == FALSE)  {

      longData <- fullModel$data$long

      groups <- as.factor(longData[, .v(groupingVar)])
      blocks <- as.factor(longData[, blockingVar])
      y <- longData[, 'dependent']

      groupNames <- .unv(levels(groups))
      ## Code from PMCMRplus
      t <- nlevels(groups)
      b <- nlevels(blocks)
      r <- unique(table(groups))
      k <- unique(table(blocks)) 
      rij <- unlist(tapply(y, blocks, rank))
      Rj <- unname(tapply(rij, groups, sum))
      A <- sum(rij^2)
      C <- (b * k * (k + 1)^2) / 4
      D <- sum(Rj^2) - r * C
      T1 <- (t - 1) / (A - C) * D
      
      denom <- sqrt(((A - C) * 2 * r) / (b * k - b - t + 1) *
                      (1 - T1 / (b * (k -1))))
      df <- b * k - b - t + 1

      for (i in 1:t) {
        
        for (j in .seqx(i+1, t)) {
          
          row <- list("(I)"=groupNames[[i]], "(J)"=groupNames[[j]])
         
          diff <-  abs(Rj[i] - Rj[j]) 
          tval <- diff / denom
          pval <- 2 * pt(q = abs(tval), df = df, lower.tail=FALSE)
          
          row[["t"]] <- .clean(tval)
          row[["wA"]]  <- .clean(Rj[i])
          row[["wB"]] <- .clean(Rj[j])
          row[["pval"]] <- .clean(pval)
          row[["bonferroni"]] <- .clean(pval)
          row[["holm"]] <- .clean(pval)
          row[["df"]] <- .clean(df)

          connoverTable[["status"]] <- "complete"
          rows[[length(rows)+1]] <- row
          
        }
        
        if (length(rows) == 0)  {
          row[[".isNewGroup"]] <- TRUE
        } else {
          row[[".isNewGroup"]] <- FALSE
        }
      }
      
      allP <- unlist(lapply(rows, function(x) x$p))
      allBonf <- p.adjust(allP, method = "bonferroni")
      allHolm <- p.adjust(allP, method = "holm")
      
      for (p in 1:length(rows)) {
        rows[[p]][['bonferroni']] <- .clean(allBonf[p])
        rows[[p]][['holm']] <- .clean(allHolm[p])
      }
      
    } else {
      row <- list("(I)"= ".", "(J)"= ".")
      row[["t"]] <- "."
      row[["wA"]]  <- "."
      row[["wB"]] <- "."
      row[["pval"]] <- "."
      row[["bonferroni"]] <- "."
      row[["holm"]] <- "."
      row[["df"]] <- "."
      
      rows[[length(rows)+1]] <- row
    }
    
    connoverTable[["data"]] <- rows
    
    connoverTables[[length(connoverTables)+1]] <- connoverTable
    stateConnover <- connoverTables
  }
  
  list(result=connoverTables, status=status, stateConnover=stateConnover)
}

.summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE, 
                       errorBarType="confidenceInterval", usePooledSE=FALSE) {

	# New version of length which can handle NA's: if na.rm==T, don't count them
	length2 <- function (x, na.rm=FALSE) {
		if (na.rm) {
			sum(!is.na(x))
		} else {
			length(x)
		}
	}

	# This does the summary. For each group's data frame, return a vector with
	# N, mean, and sd
	# First aggregate over RM factors, if desired:
	if (usePooledSE) {
	  data <- plyr::ddply(data, c("subject", groupvars), plyr::summarise, dependent = mean(dependent))
	  names(data)[which(names(data) == "dependent")] <- measurevar
	}
	
	datac <- plyr::ddply(data, groupvars, .drop=.drop,
						 .fun = function(xx, col) {
						 	c(N    = length2(xx[[col]], na.rm=na.rm),
						 	  mean = mean   (xx[[col]], na.rm=na.rm),
						 	  sd   = sd     (xx[[col]], na.rm=na.rm)
						 	)
						 },
						 measurevar
	)

	# Rename the "mean" column
	datac <- plyr::rename(datac, c("mean" = measurevar))

	datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

	# Confidence interval multiplier for standard error
	# Calculate t-statistic for confidence interval:
	# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
	ciMult <- qt(conf.interval/2 + .5, datac$N-1)
	datac$ci <- datac$se * ciMult

	if (errorBarType == "confidenceInterval") {

		datac$ciLower <- datac[,measurevar] - datac[,"ci"]
		datac$ciUpper <- datac[,measurevar] + datac[,"ci"]

	} else {

		datac$ciLower <- datac[,measurevar] - datac[,"se"]
		datac$ciUpper <- datac[,measurevar] + datac[,"se"]

	}

	return(datac)
}

.normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL, na.rm=FALSE, .drop=TRUE) {

	# Measure var on left, idvar + between vars on right of formula.
	data.subjMean <- plyr::ddply(data, c(idvar, betweenvars), .drop=.drop,
								 .fun = function(xx, col, na.rm) {
								 	c(subjMean = mean(xx[,col], na.rm=na.rm))
								 },
								 measurevar,
								 na.rm
	)



	# Put the subject means with original data
	data <- base::merge(data, data.subjMean)

	# Get the normalized data in a new column
	measureNormedVar <- paste(measurevar, "_norm", sep="")
	data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
		mean(data[,measurevar], na.rm=na.rm)

	# Remove this subject mean column
	data$subjMean <- NULL

	return(data)
}

.summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE, 
                             conf.interval=.95, .drop=TRUE, errorBarType="confidenceInterval", usePooledSE=FALSE) {

	# Get the means from the un-normed data
	datac <- .summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm, 
	                    conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType, usePooledSE=usePooledSE)
	# Drop all the unused columns (these will be calculated with normed data)
	datac$sd <- NULL
	datac$se <- NULL
	datac$ci <- NULL
	datac$ciLower <- NULL
	datac$ciUpper <- NULL

	# Norm each subject's data
	ndata <- .normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)

	# This is the name of the new column
	measurevar_n <- paste(measurevar, "_norm", sep="")

	# Collapse the normed data - now we can treat between and within vars the same
	ndatac <- .summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType,
	                     usePooledSE=usePooledSE)

	# Apply correction from Morey (2008) to the standard error and confidence interval
	# Get the product of the number of conditions of within-S variables
	nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels, FUN.VALUE=numeric(1)))
	correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

	# Apply the correction factor
	ndatac$sd <- ndatac$sd * correctionFactor
	ndatac$se <- ndatac$se * correctionFactor
	ndatac$ci <- ndatac$ci * correctionFactor

	if (errorBarType == "confidenceInterval") {

		ndatac$ciLower <- datac[,measurevar] - ndatac[,"ci"]
		ndatac$ciUpper <- datac[,measurevar] + ndatac[,"ci"]

	} else {

		ndatac$ciLower <- datac[,measurevar] - ndatac[,"se"]
		ndatac$ciUpper <- datac[,measurevar] + ndatac[,"se"]

	}

	# Combine the un-normed means with the normed results
	merge(datac, ndatac)
}
