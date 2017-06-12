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
	stateSphericity <- NULL

	if ( ! is.null(state)) {  # is there state?

		diff <- .diff(options, state$options)  # compare old and new options

		if (is.list(diff) && diff[['withinModelTerms']] == FALSE && diff[['betweenModelTerms']] == FALSE && diff[['repeatedMeasuresCells']] == FALSE &&
			diff[['repeatedMeasuresFactors']] == FALSE && diff[['sumOfSquares']] == FALSE && diff[['covariates']] == FALSE && diff[['betweenSubjectFactors']] == FALSE) {

			# old model can be used

			anovaModel <- state$model
			statePostHoc <- state$statePostHoc
			stateContrasts <- state$stateContrasts
			stateSphericity <- state$stateSphericity

		}

		if (is.list(diff) && diff[['plotHorizontalAxis']] == FALSE && diff[['plotSeparateLines']] == FALSE && diff[['plotSeparatePlots']] == FALSE &&
			diff[['plotErrorBars']] == FALSE && !(diff[['errorBarType']] == TRUE && options$plotErrorBars == TRUE) &&
			!(diff[['confidenceIntervalInterval']] == TRUE && options$errorBarType == "confidenceInterval" && options$plotErrorBars == TRUE) &&
			diff[['plotWidthDescriptivesPlotLegend']] == FALSE && diff[['plotHeightDescriptivesPlotLegend']] == FALSE &&
			diff[['plotWidthDescriptivesPlotNoLegend']] == FALSE && diff[['plotHeightDescriptivesPlotNoLegend']] == FALSE &&
			diff[['repeatedMeasuresFactors']] == FALSE && diff[['repeatedMeasuresCells']] == FALSE) {

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
		list(name="posthoc", type="collection", meta="table")
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
		modelTermsResults <- strsplit(rownames(epsilon), ":")
	}

	for (i in .indices(termsRM.base64)) {

		modelTermsCase <- unlist(strsplit(termsRM.base64[[i]],":"))
		index <- unlist(lapply(modelTermsResults, function(x) .identicalTerms(x,modelTermsCase)))
		epsilonTable[i,"termsNormal"] <- termsRM.normal[[i]]

		if (sum(index) == 0) {

			epsilonTable[i,"W"] <- 1
			epsilonTable[i,"p"] <- NaN
			epsilonTable[i,"GG"] <- 1
			epsilonTable[i,"HF"] <- 1
			epsilonTable[i,"twoLevels"] <- TRUE

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
		variables <- unlist(c(options$betweenSubjectFactors, lapply(options$repeatedMeasuresFactors, function(x) x$name)))
		for (var in variables) {

			formula <- as.formula(paste("~", .v(var)))
			referenceGrid <- lsmeans::lsmeans(fullModel, formula)

			referenceGridList[[var]] <- referenceGrid

		}

		return(referenceGridList)
}

.resultsPostHoc <- function (referenceGrid, options, dataset, fullModel) {
    resultsPostHoc <- list()
  
		variables <- unlist(c(options$betweenSubjectFactors, lapply(options$repeatedMeasuresFactors, function(x) x$name)))
		
		postHocData <- fullModel$data$wide
		factorNamesV <- colnames(postHocData) # Names to use to refer to variables in data
    # Because there are multiple names for each variable in JASP, one of the things the following code does is make sure to get the correct naming
		# and refer to the correct actual variable. The different names are the actual name of the variable, the name the user gives in jasp for the lvel and factor, 
		# and also the name that JASP gives to it, which is a concatenation of "Level#_Level#', where the first refers to the factor and second to the level. 
		
		rmFactorIndex <- 1
		allNames <- unlist(lapply(options$repeatedMeasuresFactors, function(x) x$name)) # Factornames 
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
	
		  orderOfTerms <- unlist(options$withinModelTerms[[length(options$withinModelTerms)]]$components)
		  indexofOrderFactors <- match(allNames,orderOfTerms)
		  
		  if(any(var == allNames)){     ## If the variable is a repeated measures factor
		    
		    levelsOfThisFactor <- unlist(lapply(options$repeatedMeasuresFactors[rmFactorIndex], function(x) x$levels)) # Levels within Factor
		    numberOfLevels <- length(unique(levelsOfThisFactor))
		    splitNames <- unlist(lapply(strsplit(factorNamesV,  split = "_"), function(x) x[indexofOrderFactors[rmFactorIndex]]))
		    
		    listVarNamesToLevel <- list()  # create a list of vectors of variable names, used to group the dataset for the post-hoc t-tests
		    for(i in 1:numberOfLevels){
		      listVarNamesToLevel[[i]] <- factorNamesV[grep(splitNames, pattern = .v(levelsOfThisFactor[i]))]  
		    }
		    
  		  countr <- 1
  		  allEstimates <- allTees <- allSE <- allPees <- numeric() 
  		  for (k in 1:numberOfLevels) {  ### Loop over all the levels within factor and do pairwise t.tests on them
  		    for (i in .seqx(k+1, numberOfLevels)) {
  		      tResult <- t.test(unlist(postHocData[listVarNamesToLevel[[k]]]),unlist(postHocData[listVarNamesToLevel[[i]]]), paired= T, var.equal = F)
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
  		  resultScheffe['p.value'] <- rep("-", length(allPees))
  		  resultTukey['p.value'] <- rep("-", length(allPees))
  		  rmFactorIndex <- rmFactorIndex + 1
  		  }


			resultsPostHoc[[var]] <- list(resultBonf = resultBonf, resultHolm = resultHolm, resultTukey = resultTukey, resultScheffe = resultScheffe,
										  comparisons = comparisons)
			
		}

		return(resultsPostHoc)
}

.resultsContrasts <- function (dataset, options, referenceGrid) {

		resultsContrasts <- list()

		datasetLong <- .shortToLong(dataset, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, options$betweenSubjectFactors)

		contrastTypes <- c("none", "deviation", "simple", "Helmert", "repeated", "difference", "polynomial")

		for (contrast in options$contrasts) {

			resultsContrasts[[contrast$variable]] <- list()

			column <- datasetLong[[.v(contrast$variable)]]

			for(contrastType in contrastTypes) {

				contrastMatrix <- .anovaCreateContrast(column, contrastType)
				c <- lapply(as.data.frame(contrastMatrix), as.vector)
				names(c) <- .v(.anovaContrastCases(column, contrastType))

				if (contrastType == "none") {
					r <- NULL
				} else {
					r <- lsmeans::contrast(referenceGrid[[contrast$variable]], c)
				}

				resultsContrasts[[contrast$variable]][[contrastType]] <- summary(r)
			}
		}

		return(resultsContrasts)
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

				SS <- result[index,"SS"]
				df <- result[index,"num Df"]
				MS <- SS / df
				F <- .clean(result[index,"F"])
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
						SSt <- sum(result[indices,"SS"]) + SSr
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

			indexResidual <- 2

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

					SS <- result[index,"SS"]
					df <- result[index,"num Df"]
					MS <- SS / df
					F <- .clean(result[index,"F"])
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
								SSt <- sum(result[indices,"SS"]) + SSr
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

				foot.index <- .addFootnote(footnotes, text="The repeated measure has only two levels. When the repeated measure has two levels, the assumption of sphericity is always met.")
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
					row[["t"]] <- .clean(as.numeric(result$t.ratio[i]))
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
							t <- .clean(-as.numeric(statePostHoc[[posthoc.var]]$resultBonf$t.ratio[index]))
						} else {
							t <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultBonf$t.ratio[index]))
						}

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
					row[["t"]] <- t
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

		if (length(repeatedMeasuresFactors) == 0) {

			summaryStat <- .summarySE(as.data.frame(dataset), measurevar = "dependent", groupvars = .v(betweenSubjectFactors),
							conf.interval = options$confidenceIntervalInterval, na.rm = TRUE, .drop = FALSE, errorBarType = options$errorBarType)

		} else {

			summaryStat <- .summarySEwithin(as.data.frame(dataset), measurevar="dependent", betweenvars=.v(betweenSubjectFactors), withinvars=.v(repeatedMeasuresFactors),
							idvar="subject", conf.interval=options$confidenceIntervalInterval, na.rm=TRUE, .drop=FALSE, errorBarType=options$errorBarType)

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
				ggplot2::ylab(options$dependent) +
				ggplot2::xlab(options$plotHorizontalAxis) +
				ggplot2::labs(shape=options$plotSeparateLines, fill=options$plotSeparateLines) +
				ggplot2::theme_bw() +
				ggplot2::theme(#legend.justification=c(0,1), legend.position=c(0,1),
					panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
					panel.grid.major=ggplot2::element_blank(),
					axis.title.x = ggplot2::element_text(size=18,vjust=-.2), axis.title.y = ggplot2::element_text(size=18,vjust=-1),
					axis.text.x = ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15),
					panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
					plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
					legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
					panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
					legend.key = ggplot2::element_blank(), #legend.key.width = grid::unit(10,"mm"),
					legend.title = ggplot2::element_text(size=12),
					legend.text = ggplot2::element_text(size = 12),
					axis.ticks = ggplot2::element_line(size = 0.5),
					axis.ticks.margin = grid::unit(1,"mm"),
					axis.ticks.length = grid::unit(3, "mm"),
					plot.margin = grid::unit(c(.5,0,.5,.5), "cm")) +
				base_breaks_y(summaryStat, options$plotErrorBars) +
				base_breaks_x(summaryStatSubset[,"plotHorizontalAxis"])

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

			# print(p)
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

.summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE, errorBarType="confidenceInterval") {

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

.summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE, errorBarType="confidenceInterval") {

	# Get the means from the un-normed data
	datac <- .summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType)

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
	ndatac <- .summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType)

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
