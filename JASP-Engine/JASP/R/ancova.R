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

Ancova <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	numeric.variables <- c(unlist(options$dependent),unlist(options$covariates),unlist(options$wlsWeight))
	numeric.variables <- numeric.variables[numeric.variables != ""]
	factor.variables <- c(unlist(options$fixedFactors),unlist(options$randomFactors),unlist(options$repeatedMeasures))
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
	statePostHoc <- NULL
	stateqqPlot <- NULL
	stateDescriptivesPlot <- NULL
	stateContrasts <- NULL
	stateLevene <- NULL
	stateMarginalMeans <- NULL
	stateSimpleEffects <- NULL
	stateDescriptivesTable <- NULL
	stateKruskal <- NULL
	stateDunn <- NULL

	if ( ! is.null(state)) {  # is there state?

		diff <- .diff(options, state$options)  # compare old and new options

		if (is.list(diff) && diff[['modelTerms']] == FALSE && diff[['dependent']] == FALSE && diff[['wlsWeights']] == FALSE && diff[['contrasts']] == FALSE) {

			# old model and contrasts can be used

			anovaModel <- state$model
			stateContrasts <- state$stateContrasts

		}

		if (is.list(diff) && diff[['modelTerms']] == FALSE && diff[['dependent']] == FALSE && diff[['wlsWeights']] == FALSE && diff[['postHocTestsVariables']] == FALSE) {

			# old post hoc results can be used

			statePostHoc <- state$statePostHoc
		}

		if (is.list(diff) && diff[['modelTerms']] == FALSE && diff[['dependent']] == FALSE && diff[['wlsWeights']] == FALSE && diff[['qqPlot']] == FALSE &&
			diff[['plotWidthQQPlot']] == FALSE && diff[['plotHeightQQPlot']] == FALSE) {

			# old Q-Q plot can be used

			stateqqPlot <- state$stateqqPlot
		}

		if (is.list(diff) && diff[['dependent']] == FALSE && diff[['plotHorizontalAxis']] == FALSE && diff[['plotSeparateLines']] == FALSE && diff[['plotSeparatePlots']] == FALSE &&
			diff[['plotErrorBars']] == FALSE && !(diff[['errorBarType']] == TRUE && options$plotErrorBars == TRUE) &&
			!(diff[['confidenceIntervalInterval']] == TRUE && options$errorBarType == "confidenceInterval" && options$plotErrorBars == TRUE) &&
			diff[['plotWidthDescriptivesPlotLegend']] == FALSE && diff[['plotHeightDescriptivesPlotLegend']] == FALSE &&
			diff[['plotWidthDescriptivesPlotNoLegend']] == FALSE && diff[['plotHeightDescriptivesPlotNoLegend']] == FALSE) {

			# old descriptives plots can be used

			stateDescriptivesPlot <- state$stateDescriptivesPlot
		}

		if (is.list(diff) && diff[['modelTerms']] == FALSE && diff[['dependent']] == FALSE && diff[['wlsWeights']] == FALSE && diff[['homogeneityTests']] == FALSE && diff[['VovkSellkeMPR']] == FALSE) {

			# old levene's table can be used

			stateLevene <- state$stateLevene

		}

		if (is.list(diff) && diff[['fixedFactors']] == FALSE && diff[['dependent']] == FALSE && diff[['descriptives']] == FALSE) {

			# old descriptives table can be used

			stateDescriptivesTable <- state$stateDescriptivesTable

		}

		if (is.list(diff) && diff[['modelTerms']] == FALSE && diff[['dependent']] == FALSE && diff[['wlsWeights']] == FALSE &&
			diff[['marginalMeansTerms']] == FALSE && diff[['marginalMeansCompareMainEffects']] == FALSE && diff[['marginalMeansCIAdjustment']] == FALSE) {

			# old marginal means tables can be used

			stateMarginalMeans <- state$stateMarginalMeans

		}

		if (is.list(diff) && diff[['modelTerms']] == FALSE && diff[['dependent']] == FALSE && diff[['wlsWeights']] == FALSE &&
		    diff[['simpleFactor']] == FALSE && diff[['moderatorFactorOne']] == FALSE && diff[['moderatorFactorTwo']] == FALSE) {

		  # old simple effects tables can be used

		  stateSimpleEffects <- state$stateSimpleEffects

		}
		
		if (is.list(diff) && diff[['modelTerms']] == FALSE && diff[['dependent']] == FALSE && 
		    diff[['kruskalVariablesAssigned']] == FALSE && diff[['contrasts']] == FALSE && diff[['dunnTest']]) {
		  
		  # old Kruskal/Dunn table can be used
		  
		  stateKruskal <- state$stateKruskal
		  stateDunn <- state$stateDunn
		  
		}
	}


	## Create Title

	if (is.null(options$covariates)) {

		results[["title"]] <- "ANOVA"

	} else {

		results[["title"]] <- "ANCOVA"

	}


	status <- .anovaCheck(dataset, options, perform)



	## Setup Contrasts

	if (perform == "run" && status$ready && status$error == FALSE)
		dataset <- .anovaSetupContrasts(dataset, options)



	## Perform ANOVA

	model <- NULL
	singular <- FALSE

	if (is.null(anovaModel)) { # if not retrieved from state

		if (perform == "run" && status$ready && status$error == FALSE) {

			anovaModel <- .anovaModel(dataset, options)

			model <- anovaModel$model
			singular <- anovaModel$singular
		}

	} else {

		model <- anovaModel$model
		singular <- anovaModel$singular
	}


	## Create ANOVA Table

	result <- .anovaTable(options, model, status, singular)
	results[["anova"]] <- result$result
	status <- result$status
  


	## Create Levene's Table

	if (is.null(stateLevene)) {

		result <- .anovaLevenesTable(dataset, options, perform, status, stateLevene, model)
		resultLevene <- result$result
		status <- result$status
		stateLevene <- result$stateLevene

	} else {

		resultLevene <- stateLevene

	}



	## Create QQ Plot

	if (is.null(stateqqPlot)) {

		result <- .qqPlot(model, options, perform, status, stateqqPlot)
		resultQQplot <- result$result
		status <- result$status
		stateqqPlot <- .imgToResults(result$stateqqPlot)

	} else {

		resultQQplot <- stateqqPlot

	}


	## Create Assumption Check Object

	results[["assumptionsObj"]] <- list(title="Assumption Checks", levene=resultLevene, qqPlot=resultQQplot)



	## Create Contrasts Tables

	if (is.null(stateContrasts)) {

		result <- .anovaContrastsTable(dataset, options, perform, model, status, stateContrasts, singular)
		results[["contrasts"]] <- list(collection=result$result, title = "Contrasts")
		status <- result$status
		stateContrasts <- result$stateContrasts

	} else {

		results[["contrasts"]] <- list(collection=stateContrasts, title = "Contrasts")

	}



	## Create Post Hoc Tables

	result <- .anovaPostHocTable(dataset, options, perform, model, status, statePostHoc, singular)

	results[["posthoc"]] <- list(collection=result$result, title = "Post Hoc Tests")
	status <- result$status
	statePostHoc <- result$statePostHoc



	## Create Marginal Means Table

	if (is.null(stateMarginalMeans)) {
		result <- .anovaMarginalMeans(dataset, options, perform, model, status, singular, stateMarginalMeans)
		results[["marginalMeans"]] <- list(collection=result$result, title = "Marginal Means")
		status <- result$status
		stateMarginalMeans <- result$stateMarginalMeans

	} else {

		results[["marginalMeans"]] <- list(collection=stateMarginalMeans, title = "Marginal Means")

	}

	## Create Simple Effects Table

	if (is.null(stateSimpleEffects)) {

	  result <- .anovaSimpleEffects(dataset, options, perform, results[["anova"]], status, singular, stateSimpleEffects)
	  results[["simpleEffects"]] <- result$result
	  status <- result$status
	  stateSimpleEffects <- result$stateSimpleEffects

	} else {

	  results[["simpleEffects"]] <- stateSimpleEffects

	}

	if (is.null(stateKruskal)) {
	  
	  result <- .anovaKruskal(dataset, options, perform, status, singular, stateKruskal)
	  results[["kruskal"]] <- result$result
	  status <- result$status
	  stateKruskal <- result$stateKruskal
	  
	} else {
	  
	  results[["kruskal"]] <- stateKruskal
	  
	}
	
	if (is.null(stateDunn)) {
	  
	  result <- .anovaDunnTable(dataset, options, perform, model, status, stateDunn, singular)
	  results[["dunn"]] <- list(collection=result$result, title = "Dunn's Post Hoc Tests")
	  status <- result$status
	  stateDunn <- result$stateDunn

	} else {
	  
	  results[["dunn"]] <- stateDunn
	  
	}

	## Create Descriptives Table

	if(is.null(stateDescriptivesTable)) {

		result <- .anovaDescriptivesTable(dataset, options, perform, status, stateDescriptivesTable)
		descriptivesTable <- result$result
		status <- result$status
		stateDescriptivesTable <- result$stateDescriptivesTable

	} else {

		descriptivesTable <- stateDescriptivesTable

	}



	## Create Descriptives Plots

	titleDescriptivesPlot <- "Descriptives Plots"

	if (is.null(stateDescriptivesPlot)) {

		result <- .anovaDescriptivesPlot(dataset, options, perform, status, stateDescriptivesPlot)
		descriptivesPlot <- result$result
		status <- result$status
		stateDescriptivesPlot <- .imgToResults(result$stateDescriptivesPlot)

	} else {

		descriptivesPlot <- stateDescriptivesPlot

	}

	if (length(descriptivesPlot) == 1) {

		results[["descriptivesObj"]] <- list(title="Descriptives", descriptivesTable=descriptivesTable, descriptivesPlot=descriptivesPlot[[1]])

	} else {

		results[["descriptivesObj"]] <- list(title="Descriptives", descriptivesTable=descriptivesTable, descriptivesPlot=list(collection=descriptivesPlot, title = titleDescriptivesPlot))
	}


	# META definitions
	.meta <- list(
		list(name="anova", type="table"),
		list(name="assumptionsObj", type="object", meta=list(list(name="levene", type="table"), list(name="qqPlot", type="image"))),
		list(name="contrasts", type="collection", meta="table"),
		list(name="posthoc", type="collection", meta="table"),
		list(name="marginalMeans", type="collection", meta="table"),
		list(name="simpleEffects", type="table"),
		list(name="kruskal", type="table"),
		list(name="dunn", type="collection", meta="table")
	)

	if (length(descriptivesPlot) == 1) {

		.meta[[length(.meta) + 1]] <- list(name="descriptivesObj", type="object", meta=list(list(name="descriptivesTable", type="table"), list(name="descriptivesPlot", type="image")))

	} else {

		.meta[[length(.meta) + 1]] <- list(name="descriptivesObj", type="object", meta=list(list(name="descriptivesTable", type="table"), list(name="descriptivesPlot", type="collection", meta="image")))

	}

	results[[".meta"]] <- .meta

	keepDescriptivesPlot <- lapply(stateDescriptivesPlot, function(x) x$data)
	state[["model"]] <- anovaModel
	state[["options"]] <- options
	state[["statePostHoc"]] <- statePostHoc
	state[["stateqqPlot"]] <- stateqqPlot
	state[["stateDescriptivesPlot"]] <- stateDescriptivesPlot
	state[["stateContrasts"]] <- stateContrasts
	state[["stateLevene"]] <- stateLevene
	state[["stateDescriptivesTable"]] <- stateDescriptivesTable
	state[["stateMarginalMeans"]] <- stateMarginalMeans
	state[["stateSimpleEffects"]] <- stateSimpleEffects
	state[["stateKruskal"]] <- stateKruskal
	state[["stateDunn"]] <- stateDunn
	
	if (perform == "init" && status$ready && status$error == FALSE) {

		return(list(results=results, status="inited", state=state, keep=c(stateqqPlot$data, keepDescriptivesPlot)))

	} else {

		return(list(results=results, status="complete", state=state, keep=c(stateqqPlot$data, keepDescriptivesPlot)))
	}
}

.anovaContrastCases <- function(column, contrast.type) {

	levels <- levels(column)
	n.levels <- length(levels)

	cases <- list()

	if (n.levels == 1) {

		cases[[1]] <- "."

	} else if (contrast.type == "deviation") {

		for (i in 1:(n.levels - 1))
			cases[[i]] <- paste(levels[i + 1], " - ", paste(levels,collapse=", "), sep="")

	} else if (contrast.type == "simple") {

		for (i in 1:(n.levels - 1))
			cases[[i]] <- paste(levels[i+1], " - ", levels[1], sep="")

	} else if (contrast.type == "Helmert") {

		for (i in 1:(n.levels - 1))
			cases[[i]] <- paste(levels[i], " - ", paste(levels[-(1:i)], collapse=", "), sep="")

	} else if (contrast.type == "repeated") {

		for (i in 1:(n.levels-1))
			cases[[i]] <- paste(levels[i], " - ", levels[i+1], sep="")

	} else if (contrast.type=="difference") {

		for (i in 1:(n.levels - 1))
			cases[[i]] <- paste(levels[i + 1], " - ", paste(levels[1:i], collapse=", "), sep="")

	} else if (contrast.type == "polynomial") {

		poly.names <- c("linear", "quadratic", "cubic", "quartic", "quintic", "sextic", "septic", "octic")
		for (i in 1:(n.levels - 1)) {
			if (i <= 8) {
				cases[[i]] <- poly.names[i]
			} else {
				cases[[i]] <- paste("degree", i, "polynomial", sep=" ")
			}
		}
	}

	cases
}

.anovaCreateContrast <- function (column, contrast.type) {

	levels <- levels(column)
	n.levels <- length(levels)

	contr <- NULL

	if (contrast.type == "none") {

		options(contrasts = c("contr.sum","contr.poly"))
		contr <- NULL

	} else if (contrast.type == "deviation") {

		contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)

		for (i in 1:n.levels-1) {
			contr[c(1,i+1),i]<- c(1,-1)
		}

		contr <- contr * -1

	} else if (contrast.type == "simple") {

		contr <- contr.treatment(levels) - 1/n.levels

	} else if (contrast.type == "Helmert") {

		contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)

		for (i in 1:(n.levels - 1)) {

			k <- 1 / (n.levels - (i - 1))
			contr[i:n.levels,i] <- c(k * (n.levels - i), rep(-k, n.levels - i))
		}

	} else if (contrast.type == "repeated") {

	  contr <- MASS::contr.sdif(levels) * -1

	} else if (contrast.type == "difference") {

		contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)

		for (i in 1:(n.levels - 1)) {

		  k <- 1 / (i +1)
		  contr[1:(i+1),i] <- c( rep(-k, i), k * i)
		}

	} else if (contrast.type == "polynomial") {

		contr <- contr.poly(levels)
	}

	if ( ! is.null(contr))
		dimnames(contr) <- list(NULL, 1:dim(contr)[2])

	contr
}

.anovaCheck <- function(dataset, options, perform) {

	error <- FALSE
	errorMessage <- NULL
	ready <- options$dependent != "" && length(options$modelTerms) > 0

	if (ready && perform == "run") {

		components <- unique(unlist(options$fixedFactors))
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

		if (sum(is.infinite(dataset[[ .v(options$dependent) ]])) > 0) {

			error <- TRUE
			errorMessage <- paste("The dependent variable: <em>", options$dependent, "</em>, contains infinite values.", sep="")
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

		if (sum(dataset[[ .v(options$wlsWeights) ]] <= 0) > 0) {

			error <- TRUE
			errorMessage <- paste("The variable: <em>", options$wlsWeights, "</em>, contains negative and/or zero values.<br><br>(only positive WLS weights allowed)", sep="")
		}

		if (sum(is.infinite(dataset[[ .v(options$wlsWeights) ]])) > 0) {

			error <- TRUE
			errorMessage <- paste("The variable: <em>", options$wlsWeights, "</em>, contains infinite values.", sep="")
		}

	}

	list(ready=ready, error=error, errorMessage=errorMessage)
}

.anovaSetupContrasts <- function(dataset, options) {

	for (contrast in options$contrasts) {

		v <- .v(contrast$variable)

		column <- dataset[[v]]
		contrasts(column) <- .anovaCreateContrast(column, contrast$contrast)
		dataset[[v]] <- column
	}

	dataset
}

.reorderModelTerms <- function(options) {

	if(length(options$modelTerms) > 0) {

		fixedFactors <- list()
		covariates <- list()

		k <- 1
		l <- 1

		for(i in 1:length(options$modelTerms)) {
			if (sum(unlist(options$modelTerms[[i]]$components) %in% options$covariates) > 0) {
				covariates[[k]] <- options$modelTerms[[i]]
				k <- k + 1
			} else {
				fixedFactors[[l]] <- options$modelTerms[[i]]
				l <- l + 1
			}
		}

		if(length(covariates) > length(options$covariates)) {
			modelTerms <- options$modelTerms
			interactions <- TRUE
		} else {
			modelTerms <- c(fixedFactors,covariates)
			interactions <- FALSE
		}

	} else {

		modelTerms <- list()
		interactions <- FALSE
	}

	list(modelTerms = modelTerms, interactions = interactions)
}

.modelFormula <- function(modelTerms, options) {

	dependent.normal <- options$dependent
	dependent.base64 <- .v(options$dependent)

	terms.base64 <- c()
	terms.normal <- c()

	for (term in modelTerms) {

		components <- unlist(term$components)
		term.base64 <- paste(.v(components), collapse=":", sep="")
		term.normal <- paste(components, collapse=" \u273B ", sep="")

		terms.base64 <- c(terms.base64, term.base64)
		terms.normal <- c(terms.normal, term.normal)
	}

	model.def <- paste(dependent.base64, "~", paste(terms.base64, collapse="+"))

	list(model.def = model.def, terms.base64 = terms.base64, terms.normal = terms.normal)
}

.anovaModel <- function(dataset, options) {
  
	reorderModelTerms <-  .reorderModelTerms(options)
	modelTerms <- reorderModelTerms$modelTerms

	modelDef <- .modelFormula(modelTerms, options)
	model.formula <- as.formula(modelDef$model.def)

	WLS <- NULL
	if ( ! is.null(options$wlsWeights))
		WLS <- dataset[[ .v(options$wlsWeights) ]]

	model <- aov(model.formula, dataset, weights=WLS)

	modelError <- try(silent = TRUE, lm(model.formula, dataset, weights=WLS, singular.ok = FALSE))
	errorMessage <- ""

	if (class(modelError) == "try-error")
		errorMessage <- .extractErrorMessage(modelError)

	singular <- FALSE
	if (errorMessage == "singular fit encountered")
		singular <- TRUE

	list(model = model, singular = singular)
}

.anovaTable <- function(options, model, status, singular) {

	anova <- list()

	if (is.null(options$covariates)) {

		if (options$dependent != "") {

			anova[["title"]] <- paste("ANOVA - ", options$dependent, sep = "")

		} else {

			anova[["title"]] <- "ANOVA"

		}

	} else {

		if (options$dependent != "") {

			anova[["title"]] <- paste("ANCOVA - ", options$dependent, sep = "")

		} else {

			anova[["title"]] <- "ANCOVA"

		}
	}

	fields <- list(
		list(name="Cases", type="string"),
		list(name="Sum of Squares", type="number", format="sf:4;dp:3"),
		list(name="df", type="number", format="dp:0"),
		list(name="Mean Square", type="number", format="sf:4;dp:3"),
		list(name="F", type="number", format="sf:4;dp:3"),
		list(name="p", type="number", format="dp:3;p:.001"))

	if (options$VovkSellkeMPR) {
    fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
                                        title = "VS-MPR\u002A",
                                        type = "number",
                                        format = "sf:4;dp:3")
 	}

	if (options$effectSizeEstimates) {

		if(options$effectSizeEtaSquared) {
			fields[[length(fields) + 1]] <- list(name="\u03B7\u00B2", type="number", format="dp:3")
		}
		if(options$effectSizePartialEtaSquared) {
			fields[[length(fields) + 1]] <- list(name="\u03B7\u00B2\u209A", type="number", format="dp:3")
		}
		if(options$effectSizeOmegaSquared) {
			fields[[length(fields) + 1]] <- list(name="\u03C9\u00B2", type="number", format="dp:3")
		}
	}

	anova[["schema"]] <- list(fields=fields)

	reorderModelTerms <-  .reorderModelTerms(options)
	modelTerms <- reorderModelTerms$modelTerms

	modelDef <- .modelFormula(modelTerms, options)
	terms.normal <- modelDef$terms.normal
	terms.base64 <- modelDef$terms.base64

	footnotes <- .newFootnotes()

	if (options$sumOfSquares == "type1") {

		.addFootnote(footnotes, text = "Type I Sum of Squares", symbol = "<em>Note.</em>")

	} else if (options$sumOfSquares == "type2") {

		.addFootnote(footnotes, text = "Type II Sum of Squares", symbol = "<em>Note.</em>")

	} else if (options$sumOfSquares == "type3") {

		.addFootnote(footnotes, text = "Type III Sum of Squares", symbol = "<em>Note.</em>")

	}

	if (options$VovkSellkeMPR) {
		.addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
		<em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum
		possible odds in favor of H\u2081 over H\u2080 equals
		1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
		(Sellke, Bayarri, & Berger, 2001).")
	}

	if (is.null(model)) {

		anova.rows <- list()

		for (i in .indices(terms.normal)) {

			if(i == 1 || (!is.null(unlist(options$covariates)) && terms.normal[i] == options$covariates[[1]] && !reorderModelTerms$interactions)) {
				newGroup <- TRUE
			} else {
				newGroup <- FALSE
			}

			row <- list("Cases"=terms.normal[i], "Sum of Squares"=".", "df"=".", "Mean Square"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
			anova.rows[[length(anova.rows) + 1]] <- row
		}

		row <- list("Cases"="Residual", "Sum of Squares"=".", "df"=".", "Mean Square"=".", "F"=".", "p"=".", ".isNewGroup" = TRUE)
		anova.rows[[length(anova.rows) + 1]] <- row

		anova[["data"]] <- anova.rows

		if (status$error)
			anova[["error"]] <- list(errorType="badData", errorMessage=status$errorMessage)


	} else {

		anova.rows <- try (silent = FALSE, expr = {

			rows <- list()

			if (options$sumOfSquares == "type1") {

				result <- base::tryCatch(stats::anova(model),error=function(e) e, warning=function(w) w)

				if (!is.null(result$message) && result$message == "ANOVA F-tests on an essentially perfect fit are unreliable")
					stop(result$message)

				SSt <- sum(result[,"Sum Sq"], na.rm = TRUE)

			} else if (options$sumOfSquares == "type2") {

				result <- car::Anova(model, type=2)
				SSt <- sum(result[,"Sum Sq"], na.rm = TRUE)

			} else if (options$sumOfSquares == "type3") {

				result <- car::Anova(model, type=3, singular.ok=TRUE)
				SSt <- sum(result[-1,"Sum Sq"], na.rm = TRUE)

			}

			for (i in 1:(length(terms.base64)+1)) {

				if (i <= length(terms.base64)) {
					term <- terms.base64[i]
				} else {
					term <- "Residuals"
				}

				df <- result[term,"Df"]

				if (is.na(df) || df == 0) {
					SS <- 0
					df <- 0
					MS <- ""
				} else {
					SS <- result[term,"Sum Sq"]
					MS <- result[term,"Sum Sq"]/result[term,"Df"]
				}

				F <- if (is.na(result[term,"F value"])) {""} else { result[term, "F value"] }
				p <- if (is.na(result[term,"Pr(>F)"] )) {""} else { result[term, "Pr(>F)"] }

				if(i == 1 || term == "Residuals" || (!is.null(unlist(options$covariates)) && terms.normal[i] == options$covariates[[1]] && !reorderModelTerms$interactions)) {
					newGroup <- TRUE
				} else {
					newGroup <- FALSE
				}

				if (i <= length(terms.base64)) {
					row <- list("Cases"=terms.normal[i], "Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)
				} else {
					row <- list("Cases"="Residual", "Sum of Squares"=SS, "df"=df, "Mean Square"=MS, "F"="", "p"="", ".isNewGroup" = newGroup)
				}

				if (options$effectSizeEstimates) {
					SSr <- result["Residuals","Sum Sq"]
					MSr <- SSr/result["Residuals","Df"]

					if (i <= length(terms.base64)) {

						row[["\u03B7\u00B2"]] <- SS / SSt
						row[["\u03B7\u00B2\u209A"]] <- SS / (SS + SSr)
						omega <- (SS - (df * MSr)) / (SSt + MSr)

						if (omega < 0) {
							row[["\u03C9\u00B2"]] <- 0
						} else {
							row[["\u03C9\u00B2"]] <- omega
						}

					} else {

						row[["\u03B7\u00B2"]] <- ""
						row[["\u03B7\u00B2\u209A"]] <- ""
						row[["\u03C9\u00B2"]] <- ""
					}

				}

				if (options$VovkSellkeMPR){
					row[["VovkSellkeMPR"]] <-  ifelse(p!="",.VovkSellkeMPR(p),"")
				}

				rows[[length(rows) + 1]] <- row
			}

			rows
		})

		if (class(anova.rows) == "try-error") {

			errorMessage <- .extractErrorMessage(anova.rows)

			if (errorMessage == "U[1,1] = 0" || errorMessage == "NA/NaN/Inf in foreign function call (arg 1)" || errorMessage == "undefined columns selected" ||
				errorMessage == "ANOVA F-tests on an essentially perfect fit are unreliable") {

				errorMessage <- "Residual sums of squares and/or residual degrees of freedom are equal to zero indicating perfect fit.<br><br>(ANOVA F-tests on an essentially perfect fit are unreliable)"

			}

			status$error <- TRUE
			status$errorMessage <- errorMessage

			anova[["error"]] <- list(errorType="badData", errorMessage = errorMessage)

			anova.rows <- list()

			for (i in .indices(terms.normal)) {
				row <- list("Cases"=terms.normal[i], "Sum of Squares"="", "df"="", "Mean Square"="", "F"="", "p"="")
				anova.rows[[length(anova.rows) + 1]] <- row
			}
		}

		anova[["data"]] <- anova.rows
		anova[["status"]] <- "complete"

		if (singular)
			.addFootnote(footnotes, text = "Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables", symbol = "<em>Warning.</em>")

	}

	anova[["footnotes"]] <- as.list(footnotes)

	list(result=anova, status=status)
}

.anovaContrastsTable <- function(dataset, options, perform, model, status, stateContrasts, singular) {

	no.contrasts <- TRUE

	for (contrast in options$contrasts) {

		if (contrast$contrast != "none")
			no.contrasts <- FALSE
	}

	if (no.contrasts)
		return(list(result=NULL, status=status))

	contrast.tables <- list()

	if (perform == "run" && status$ready && status$error == FALSE)
		contrast.summary <- summary.lm(model)[["coefficients"]]

	for (contrast in options$contrasts) {

		if (contrast$contrast != "none") {

			variable <- contrast$variable
			contrast.type <- contrast$contrast

			contrast.table <- list()

			contrastType <- unlist(strsplit(contrast.type,""))
			contrastType[1] <- toupper(contrastType[1])
			contrastType <- paste(contrastType, collapse="")

			contrast.table[["title"]] <- paste(contrastType, " Contrast", " - ",  variable, sep="")
			contrast.table[["name"]] <- paste(contrastType, "Contrast_",  variable, sep="")

			contrast.table[["schema"]] <- list(fields = list(
				list(name="Comparison", type="string"),
				list(name="Estimate", type="number", format="sf:4;dp:3"),
				list(name="Std. Error", type="number", format="sf:4;dp:3"),
				list(name="t", type="number", format="sf:4;dp:3"),
				list(name="p", type="number", format="dp:3;p:.001")))

			footnotes <- .newFootnotes()

			v <- .v(variable)

			column <- dataset[[ v ]]

			cases <- .anovaContrastCases(column, contrast.type)

			if (contrast == "polynomial" && length(cases) > 5)
				cases <- cases[1:5]

			contrast.rows <- list()

			if (perform == "init" || status$error || !status$ready || singular) {

				for (case in cases) {

					row <- list(Comparison=case)

					if(length(contrast.rows) == 0)  {
						row[[".isNewGroup"]] <- TRUE
					} else {
						row[[".isNewGroup"]] <- FALSE
					}

					contrast.rows[[length(contrast.rows)+1]] <- row
				}

				if (singular) {

					.addFootnote(footnotes, text = "Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables", symbol = "<em>Warning.</em>")

				}

			} else {

				for (i in .indices(cases)) {

					case <- cases[[i]]

					nam <- paste(v, i, sep="")

					est <- contrast.summary[nam,"Estimate"]
					SE  <- contrast.summary[nam,"Std. Error"]
					t   <- contrast.summary[nam,"t value"]
					p   <- contrast.summary[nam,"Pr(>|t|)"]

					if (is.na(p))
						p <- ""

					row <- list("Comparison"=case, "Estimate"=est, "Std. Error"=SE, "t"=t, "p"=p)

					if(length(contrast.rows) == 0)  {
						row[[".isNewGroup"]] <- TRUE
					} else {
						row[[".isNewGroup"]] <- FALSE
					}

					contrast.rows[[length(contrast.rows)+1]] <- row
				}
			}

			contrast.table[["data"]] <- contrast.rows
			contrast.table[["footnotes"]] <- as.list(footnotes)

			if (perform == "run" && status$ready && status$error == FALSE)
				contrast.table[["status"]] <- "complete"

			if (status$error)
				contrast.table[["error"]] <- list(errorType="badData")

			contrast.tables[[length(contrast.tables)+1]] <- contrast.table
		}
	}

	if (perform == "init" || status$error || !status$ready) {

		stateContrasts <- NULL

	} else {

		stateContrasts <- contrast.tables

	}

	list(result=contrast.tables, status=status, stateContrasts=stateContrasts)
}

.postHocContrasts <- function(variable.levels, dataset, options) {

	contrasts <- NULL
	nLevels <- length(variable.levels)

	for (i in 1:nLevels) {

		for (j in .seqx(i+1, nLevels)) {

			name <- paste(variable.levels[[i]], "-", variable.levels[[j]], sep = " ")
			contrast <- rep(0, nLevels)
			contrast[i] <- -1
			contrast[j] <- 1

			arg <- list(contrasts, contrast)
			names(arg)[2] <- name
			contrasts <- do.call(rbind, arg)

		}
	}

	return(contrasts)
}

.anovaPostHocTable <- function(dataset, options, perform, model, status, statePostHoc, singular) {

	posthoc.variables <- unlist(options$postHocTestsVariables)

	posthoc.tables <- list()

	if (is.null(statePostHoc))
		statePostHoc <- list()

	for (posthoc.var in posthoc.variables) {

		posthoc.table <- list()

		posthoc.table[["title"]] <- paste("Post Hoc Comparisons - ", posthoc.var, sep="")
		posthoc.table[["name"]] <- paste("postHoc_", posthoc.var, sep="")

		fields <- list(
			list(name="(I)",title="", type="string", combine=TRUE),
			list(name="(J)",title="", type="string"),
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

		variable.levels <- levels(droplevels(dataset[[ .v(posthoc.var) ]]))
		nLevels <- length(variable.levels)

		if (perform == "run" && status$ready && status$error == FALSE && is.null(statePostHoc[[posthoc.var]]) && !singular) {

			statePostHoc[[posthoc.var]] <- list()

			# Results using the Tukey method

			method <- list("Tukey")
			names(method) <- .v(posthoc.var)
			statePostHoc[[posthoc.var]]$resultTukey <- summary(multcomp::glht(model,do.call(multcomp::mcp, method)))

			# Results using the Scheffe method

			tTukey <- statePostHoc[[posthoc.var]]$resultTukey$test$tstat
			modelRank <- model$rank
			dfResidual <- model$df.residual
			statePostHoc[[posthoc.var]]$resultScheffe <- 1-pf(tTukey**2/(modelRank-1),modelRank-1,dfResidual)

			# Results using the Bonferroni method

			contrastMatrix <- list(.postHocContrasts(variable.levels, dataset, options))
			names(contrastMatrix) <- .v(posthoc.var)
			r <- multcomp::glht(model,do.call(multcomp::mcp, contrastMatrix))
			statePostHoc[[posthoc.var]]$resultBonf <- summary(r,test=multcomp::adjusted("bonferroni"))
      
			# Results using the Holm method

			statePostHoc[[posthoc.var]]$resultHolm <- summary(r,test=multcomp::adjusted("holm"))

			statePostHoc[[posthoc.var]]$comparisonsTukSchef <- strsplit(names(statePostHoc[[posthoc.var]]$resultTukey$test$coefficients)," - ")
			statePostHoc[[posthoc.var]]$comparisonsBonfHolm <- strsplit(names(statePostHoc[[posthoc.var]]$resultBonf$test$coefficients)," - ")
			
		}
		
		for (i in 1:length(variable.levels)) {

			for (j in .seqx(i+1, length(variable.levels))) {

				row <- list("(I)"=variable.levels[[i]], "(J)"=variable.levels[[j]])

				pTukey <- ""
				pScheffe <- ""
				pBonf <- ""
				pHolm <- ""
				effectSize <- ""


				if (!is.null(statePostHoc[[posthoc.var]])) {

					if (length(class(statePostHoc[[posthoc.var]]$resultTukey)) == 1 && class(statePostHoc[[posthoc.var]]$resultTukey) == "try-error") {

						md <- ""
						SE  <- ""
						t <- ""
						p  <- 1

						posthoc.table[["footnotes"]] <- list(list(symbol="<i>Note.</i>", text="Some comparisons could not be performed. Possibly too few samples."))

					} else {

						for (c in 1:length(statePostHoc[[posthoc.var]]$comparisonsTukSchef)) {
							if (all(statePostHoc[[posthoc.var]]$comparisonsTukSchef[[c]] %in% c(variable.levels[[i]], variable.levels[[j]]))) {
								index1 <- c

								reverse <- TRUE
								if (statePostHoc[[posthoc.var]]$comparisonsTukSchef[[c]][1] == variable.levels[[i]])
									reverse <- FALSE
							}

							if (all(statePostHoc[[posthoc.var]]$comparisonsBonfHolm[[c]] %in% c(variable.levels[[i]], variable.levels[[j]]))) {
								index2 <- c
							}
						}

						if (reverse) {
							md <- .clean(-as.numeric(statePostHoc[[posthoc.var]]$resultTukey$test$coefficients[index1]))
						} else {
							md <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultTukey$test$coefficients[index1]))
						}

						SE  <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultTukey$test$sigma[index1]))

						if (reverse) {
							t <- .clean(-as.numeric(statePostHoc[[posthoc.var]]$resultTukey$test$tstat[index1]))
						} else {
							t <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultTukey$test$tstat[index1]))
						}
						
						if (options$postHocTestEffectSize & nrow(dataset) > 0) {
						  x <- dataset[(dataset[.v(posthoc.var)] == variable.levels[[i]]), .v(options$dependent)]
						  y <- dataset[(dataset[.v(posthoc.var)] == variable.levels[[j]]), .v(options$dependent)]
						  n1 <- length(x)
						  n2 <- length(y)
						  den <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
						  effectSize <- .clean(md / den)
						}

						if (options$postHocTestsTukey)
							pTukey <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultTukey$test$pvalues[index1]))

						if (options$postHocTestsScheffe)
							pScheffe <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultScheffe[index1]))

						if (options$postHocTestsBonferroni)
							pBonf <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultBonf$test$pvalues[index2]))

						if (options$postHocTestsHolm)
							pHolm <- .clean(as.numeric(statePostHoc[[posthoc.var]]$resultHolm$test$pvalues[index2]))
					}

					row[["Mean Difference"]] <- md
					row[["SE"]]  <- SE
					row[["t"]] <- t
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

		if (singular)
			posthoc.table[["footnotes"]] <- list(list(symbol = "<em>Warning.</em>", text = "Singular fit encountered; one or more predictor variables are a linear combination of other predictor variables"))

		if (status$error)
			posthoc.table[["error"]] <- list(errorType="badData")

		posthoc.tables[[length(posthoc.tables)+1]] <- posthoc.table
	}

	list(result=posthoc.tables, status=status, statePostHoc=statePostHoc)
}

.anovaDescriptivesTable <- function(dataset, options, perform, status, stateDescriptivesTable) {

	if (options$descriptives == FALSE)
		return(list(result=NULL, status=status))

	descriptives.table <- list()

	if (options$dependent != "") {

		descriptives.table[["title"]] <- paste("Descriptives - ", options$dependent, sep = "")

	} else {

		descriptives.table[["title"]] <- "Descriptives"

	}

	fields <- list()

	for (variable in options$fixedFactors) {

		name <- paste(".", variable, sep="")  # in case variable is "Mean", "SD" or "N"
		fields[[length(fields)+1]] <- list(name=name, type="string", title=variable, combine=TRUE)
	}

	fields[[length(fields)+1]] <- list(name="Mean", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="SD", type="number", format="sf:4;dp:3")
	fields[[length(fields)+1]] <- list(name="N", type="number", format="dp:0")

	descriptives.table[["schema"]] <- list(fields=fields)

	lvls <- list()
	factors <- list()

	for (variable in options$fixedFactors) {

		factor <- dataset[[ .v(variable) ]]
		factors[[length(factors)+1]] <- factor
		lvls[[ variable ]] <- levels(factor)
	}

	cases <- rev(expand.grid(rev(lvls)))

	namez <- unlist(options$fixedFactors)
	column.names <- paste(".", namez, sep="")

	if (length(options$fixedFactors) > 0) {

		rows <- list()

		for (i in 1:dim(cases)[1]) {

			row <- list()

			for (j in 1:dim(cases)[2])
				row[[ column.names[[j]] ]] <- as.character(cases[i, j])

			if (perform == "run" && status$ready && status$error == FALSE) {

				sub  <- eval(parse(text=paste("dataset$", .v(namez), " == \"", row, "\"", sep="", collapse=" & ")))

				data <- base::subset(dataset, sub, select=.v(options$dependent))[[1]]

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

			}

			if(cases[i,dim(cases)[2]] == lvls[[ dim(cases)[2] ]][1]) {
				row[[".isNewGroup"]] <- TRUE
			} else {
				row[[".isNewGroup"]] <- FALSE
			}

			rows[[i]] <- row
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

.anovaLevenesTable <- function(dataset, options, perform, status, stateLevene, model) {

	if (options$homogeneityTests == FALSE)
		return (list(result=NULL, status=status, stateLevene=NULL))

	levenes.table <- list()

	levenes.table[["title"]] <- "Test for Equality of Variances (Levene's)"

	fields <- list(
		list(name="F", type="number", format="sf:4;dp:3"),
		list(name="df1", type="number", format="dp:0"),
		list(name="df2", type="number", format="dp:0"),
		list(name="p", type="number", format="dp:3;p:.001"))

	footnotes <- .newFootnotes()

	if (options$VovkSellkeMPR) {
		.addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
		<em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum
		possible odds in favor of H\u2081 over H\u2080 equals
		1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
		(Sellke, Bayarri, & Berger, 2001).")
		fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
																				title = "VS-MPR\u002A",
																				type = "number",
																				format = "sf:4;dp:3")
	}

	levenes.table[["schema"]] <- list(fields=fields)


	if (perform == "run" && status$ready && status$error == FALSE && length(options$fixedFactors) > 0) {

		interaction <- paste(.v(options$fixedFactors), collapse=":", sep="")
		# levene.def <- paste(.v(options$dependent), "~", interaction)
		resids <- abs(model$residuals)
		levene.def <- paste("resids", "~", interaction)
		levene.formula <- as.formula(levene.def)

		#r <- car::leveneTest(levene.formula, dataset, center = "mean")
		r <- summary(aov(levene.formula, dataset))
		error <- base::tryCatch(summary(aov(levene.formula, dataset)),error=function(e) e, warning=function(w) w)

		if (!is.null(error$message) && error$message == "ANOVA F-tests on an essentially perfect fit are unreliable") {

			errorMessage <- "F-value equal to zero indicating perfect fit.<br><br>(Levene's tests on an essentially perfect fit are unreliable)"
			levenes.table[["error"]] <- list(error="badData", errorMessage = errorMessage)

		}

		if (options$VovkSellkeMPR){
		  levenes.table[["data"]] <- list(list("F"=.clean(r[[1]]$`F value`[1]), "df1"=r[[1]]$Df[1], "df2"=r[[1]]$Df[2], "p"=.clean(r[[1]]$`Pr(>F)`[1]), "VovkSellkeMPR"=.VovkSellkeMPR(r[[1]]$`Pr(>F)`[1]), ".isNewGroup"=TRUE))
		} else {
		  levenes.table[["data"]] <- list(list("F"=.clean(r[[1]]$`F value`[1]), "df1"=r[[1]]$Df[1], "df2"=r[[1]]$Df[2], "p"=.clean(r[[1]]$`Pr(>F)`[1]), ".isNewGroup"=TRUE))
		}

		# if (options$VovkSellkeMPR){
		#   levenes.table[["data"]] <- list(list("F"=.clean(r[1,2]), "df1"=r[1,1], "df2"=r[2,1], "p"=.clean(r[1,3]), "VovkSellkeMPR"=.VovkSellkeMPR(r[1,3]), ".isNewGroup"=TRUE))
		# } else {
		# 	levenes.table[["data"]] <- list(list("F"=.clean(r[1,2]), "df1"=r[1,1], "df2"=r[2,1], "p"=.clean(r[1,3]), ".isNewGroup"=TRUE))
		# }

		levenes.table[["footnotes"]] <- as.list(footnotes)
		levenes.table[["status"]] <- "complete"

		stateLevene <- levenes.table

	} else {

		if (options$VovkSellkeMPR){
			levenes.table[["data"]] <- list(list("F"=".", "df1"=".", "df2"=".", "p"=".", "VovkSellkeMPR"=".", ".isNewGroup"=TRUE))
		} else {
			levenes.table[["data"]] <- list(list("F"=".", "df1"=".", "df2"=".", "p"=".", ".isNewGroup"=TRUE))
		}

		levenes.table[["footnotes"]] <- as.list(footnotes)

		stateLevene <- NULL
	}

	if (status$error)
		levenes.table[["error"]] <- list(error="badData")

	list(result=levenes.table, status=status, stateLevene=stateLevene)
}

.anovaMarginalMeans <- function(dataset, options, perform, model, status, singular, stateMarginalMeans) {

	if (is.null(options$marginalMeansTerms))
		return (list(result=NULL, status=status))

	terms <- options$marginalMeansTerms

	terms.base64 <- c()
	terms.normal <- c()

	for (term in terms) {

		components <- unlist(term)
		term.base64 <- paste(.v(components), collapse=":", sep="")
		term.normal <- paste(components, collapse=" \u273B ", sep="")

		terms.base64 <- c(terms.base64, term.base64)
		terms.normal <- c(terms.normal, term.normal)
	}

	marginalMeans <- list()

	for (i in .indices(terms.base64)) {

		result <- list()

		result[["title"]] <- paste("Marginal Means - ",terms.normal[i], sep="")
		result[["name"]] <- paste("marginalMeans_",gsub("\u273B","*",gsub(" ", "", terms.normal[i], fixed=TRUE), fixed=TRUE), sep="")

		fields <- list()

		for(j in .indices(terms[[i]]))
			fields[[j]] <- list(name=terms[[i]][[j]], type="string", combine=TRUE)

		fields[[length(fields) + 1]] <- list(name="Marginal Mean", type="number", format="sf:4;dp:3")
		fields[[length(fields) + 1]] <- list(name="SE", type="number", format="sf:4;dp:3")
		fields[[length(fields) + 1]] <- list(name="Lower CI", type="number", format="sf:4;dp:3")
		fields[[length(fields) + 1]] <- list(name="Upper CI", type="number", format="sf:4;dp:3")

		footnotes <- .newFootnotes()

		if(options$marginalMeansCompareMainEffects) {
			fields[[length(fields) + 1]] <- list(name="t", type="number", format="sf:4;dp:3")
			fields[[length(fields) + 1]] <- list(name="p", type="number", format="dp:3;p:.001")

			if(options$marginalMeansCIAdjustment == "bonferroni") {
				.addFootnote(footnotes, text = "Bonferroni CI adjustment", symbol = "<em>Note.</em>")
			} else if(options$marginalMeansCIAdjustment == "sidak") {
				.addFootnote(footnotes, text = "Sidak CI adjustment", symbol = "<em>Note.</em>")
			}
		}

		result[["schema"]] <- list(fields=fields)

		termsTemp <- as.vector(terms[[i]])

		lvls <- list()
		factors <- list()

		for (variable in termsTemp) {

			factor <- dataset[[ .v(variable) ]]
			factors[[length(factors)+1]] <- factor
			lvls[[variable]] <- levels(factor)
		}

		cases <- rev(expand.grid(rev(lvls)))
		cases <- as.data.frame(apply(cases,2,as.character))

		nRows <- dim(cases)[1]
		nCol <- dim(cases)[2]

		if (perform == "run" && status$ready && status$error == FALSE)  {

			formula <- as.formula(paste("~", terms.base64[i]))

			if(options$marginalMeansCIAdjustment == "bonferroni") {
				adjMethod <- "bonferroni"
			} else if(options$marginalMeansCIAdjustment == "sidak") {
				adjMethod <- "sidak"
			} else {
				adjMethod <- "none"
			}

			r <- summary(emmeans::lsmeans(model, formula), adjust = adjMethod, infer = c(TRUE,TRUE))

			rows <- list()

			for(k in 1:nRows) {

				row <- list()

				for(j in 1:nCol)
					row[[ colnames(cases)[j] ]] <- cases[k,j]

				if(nCol > 1) {
					index <- apply(r[,1:nCol], 1, function(x) all(x==cases[k,]))
				} else {
					index <- k
				}

				row[["Marginal Mean"]] <- .clean(r$lsmean[index])
				row[["SE"]] <- .clean(r$SE[index])
				row[["Lower CI"]] <- .clean(r$lower.CL[index])
				row[["Upper CI"]] <- .clean(r$upper.CL[index])

				if(options$marginalMeansCompareMainEffects) {
					row[["t"]] <- .clean(r$t.ratio[index])
					row[["p"]] <- .clean(r$p.value[index])
				}

				if(cases[k,nCol] == lvls[[ nCol ]][1]) {
					row[[".isNewGroup"]] <- TRUE
				} else {
					row[[".isNewGroup"]] <- FALSE
				}

				rows[[k]] <- row

			}

			result[["data"]] <- rows
			result[["status"]] <- "complete"

		} else {

			rows <- list()

			for(k in 1:nRows) {

				row <- list()

				for(j in 1:nCol)
					row[[ colnames(cases)[j] ]] <- cases[k,j]

				row[["Marginal Mean"]] <- "."
				row[["SE"]] <- "."
				row[["Lower CI"]] <- "."
				row[["Upper CI"]] <- "."

				if(options$marginalMeansCompareMainEffects) {
					row[["t"]] <- "."
					row[["p"]] <- "."
				}

				if(cases[k,nCol] == lvls[[ nCol ]][1]) {
					row[[".isNewGroup"]] <- TRUE
				} else {
					row[[".isNewGroup"]] <- FALSE
				}

				rows[[k]] <- row

			}

			result[["data"]] <- rows
		}

		result[["footnotes"]] <- as.list(footnotes)

		if (status$error)
			result[["error"]] <- list(error="badData")

		marginalMeans[[i]] <- result

	}

	if (perform == "run" && status$ready && status$error == FALSE)  {

		stateMarginalMeans <- marginalMeans

	} else {

		stateMarginalMeans <- NULL

	}

	list(result=marginalMeans, status=status, stateMarginalMeans=stateMarginalMeans)
}

.anovaSimpleEffects <- function(dataset, options, perform, fullAnovaTable, status, singular, stateSimpleEffects) {
  
  if (identical(options$simpleFactor, "") | identical(options$moderatorFactorOne, ""))
    return (list(result=NULL, status=status))


  terms <- c(options$moderatorFactorOne,options$moderatorFactorTwo)
  terms.base64 <- c()
  terms.normal <- c()
  simpleFactor.base64 <- .v(options$simpleFactor)

  for (term in terms) {

    components <- unlist(term)
    term.base64 <- paste(.v(components), collapse=":", sep="")
    term.normal <- paste(components, collapse=" \u273B ", sep="")

    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }
  simpleEffectsTable <- list()
  simpleEffectsTable[["title"]] <- paste("Simple Main Effects - ", options$simpleFactor, sep = "")

  fields <- list(
    list(name="ModOne", type="string", combine = TRUE, title = paste0("Level of ", terms.normal[1])),
    list(name="ModTwo", type="string", combine = TRUE, title = paste0("Level of ", terms.normal[2])),
    list(name="SumSquares", type="number", format="sf:4;dp:3", title = "Sum of Squares"),
    list(name="df", type="integer", title = "df"),
    list(name="MeanSquare", type="number", format="sf:4;dp:3", title = "Mean Square"),
    list(name="F", type="number", format="sf:4;dp:3", title = "F"),
    list(name="p", type="number", format="dp:3;p:.001", title = "p"))

  if (identical(options$moderatorFactorTwo, ""))
    fields <- fields[-2]

  footnotes <- .newFootnotes()

  simpleEffectsTable[["schema"]] <- list(fields=fields)


  tableCounter <- 1
  fullAnovaMS <- fullAnovaTable$data[[length(fullAnovaTable$data)]]$`Mean Square`
  fullAnovaDf <- fullAnovaTable$data[[length(fullAnovaTable$data)]]$df
  simpleEffectRows <- list()
  rows <- list()

  termsBothModerators <- as.vector(c(options$moderatorFactorOne, options$moderatorFactorTwo))
  lvls <- list()
  factors <- list()

  for (variable in termsBothModerators) {

    factor <- dataset[[ .v(variable) ]]
    factors[[length(factors)+1]] <- factor
    lvls[[variable]] <- levels(factor)
  }
  if (perform == "run" && status$ready && status$error == FALSE)  {

    for (level in lvls[[1]]) {
      # For each level of the first moderator factor, take a subset of the dataset, and adjust the options object
      # Suboptions is the same as options, except that the first moderator factor has been removed as a predictor
      # (because each subset only has one level of that factor). The same procedure is applied to the second moderator, if specified.
      subDataset <- subset(dataset, dataset[terms.base64[1]] == level)
      subOptions <- options
      subOptions$fixedFactors <- options$fixedFactors[options$fixedFactors != options$moderatorFactorOne]
      subOptions$modelTerms <- options$modelTerms[ !grepl(options$moderatorFactorOne, unlist(options$modelTerms, recursive = FALSE)) ]
      model <- NULL
      singular <- FALSE
      if (identical(options$moderatorFactorTwo, "")) {
        newGroup <- ifelse( level == lvls[[1]][1], TRUE, FALSE )
        if (nrow(subDataset) < 2 || nrow(unique(subDataset[simpleFactor.base64])) <  nrow(unique(dataset[simpleFactor.base64]))) {
          row <- list("ModOne"=level, "SumSquares"=".", "df"=".", "MeanSquare"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
          .addFootnote(footnotes, text = "Not enough observations in cell", symbol = "<em>Note.</em>")
        } else {
            anovaModel <- .anovaModel(subDataset, subOptions)
            model <- anovaModel$model
            singular <- anovaModel$singular
            modelSummary <- summary(model)[[1]]
            modOneIndex <- which(subOptions$fixedFactors == options$simpleFactor)
            df <- modelSummary$Df[modOneIndex]
            MS <- modelSummary$`Mean Sq`[modOneIndex]
            SS <- modelSummary$`Sum Sq`[modOneIndex]
            F <- MS / fullAnovaMS
            p <- pf(F, df, fullAnovaDf, lower.tail = FALSE)
            row <- list("ModOne"=level, "SumSquares"=SS, "df"=df, "MeanSquare"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)

        }
        simpleEffectRows[[length(simpleEffectRows) + 1]] <- row
      } else {
          for (levelTwo in lvls[[2]]) {
            newGroup <- ifelse( levelTwo == lvls[[2]][1], TRUE, FALSE )
            subSubDataset <- subset(subDataset, subDataset[terms.base64[2]] == levelTwo)
            subSubOptions <- subOptions
            subSubOptions$fixedFactors <- subOptions$fixedFactors[subOptions$fixedFactors != subOptions$moderatorFactorTwo]
            subSubOptions$modelTerms <- subOptions$modelTerms[ !grepl(subOptions$moderatorFactorTwo, unlist(subOptions$modelTerms, recursive = FALSE)) ]
            if (nrow(subSubDataset) < 2 || nrow(unique(subSubDataset[simpleFactor.base64])) <  nrow(unique(dataset[simpleFactor.base64]))) {
              row <- list("ModOne"=level, "ModTwo" = levelTwo, "SumSquares"=".", "df"=".", "MeanSquare"=".", "F"=".", "p"=".", ".isNewGroup" = newGroup)
              .addFootnote(footnotes, text = "Not enough observations in cell", symbol = "<em>Note.</em>")
            } else {
                anovaModel <- .anovaModel(subSubDataset, subSubOptions)
                model <- anovaModel$model
                singular <- anovaModel$singular
                modTwoIndex <- which(subSubOptions$fixedFactors == options$simpleFactor)
                modelSummary <- summary(model)[[1]]
                df <- modelSummary$Df[modTwoIndex]
                MS <- modelSummary$`Mean Sq`[modTwoIndex]
                SS <- modelSummary$`Sum Sq`[modTwoIndex]
                F <- MS / fullAnovaMS
                p <- pf(F, df, fullAnovaDf, lower.tail = FALSE)
                row <- list("ModOne"=level, "ModTwo" = levelTwo, "SumSquares"=SS, "df"=df, "MeanSquare"=MS, "F"=F, "p"=p, ".isNewGroup" = newGroup)
            }
            simpleEffectRows[[length(simpleEffectRows) + 1]] <- row
          }
      }


    }

    simpleEffectsTable[["data"]] <- simpleEffectRows

  } else {

    simpleEffectsTable[["data"]]  <- list(list("ModOne"=terms.normal, "SumSquares"=".", "df"=".", "MeanSquare"=".", "F"=".", "p"=".", ".isNewGroup" = TRUE))
  }

  simpleEffectsTable[["footnotes"]] <- as.list(footnotes)
  simpleEffectsTable[["status"]] <- "complete"

  if (perform == "run" && status$ready && status$error == FALSE)  {

    stateSimpleEffects <- simpleEffectsTable

  } else {

    stateSimpleEffects <- NULL

  }

  list(result=simpleEffectsTable, status=status, stateSimpleEffects=stateSimpleEffects)
}

.anovaKruskal <- function(dataset, options, perform, status, singular, stateKruskal) {
  
  if (is.null(options$kruskalVariablesAssigned))
    return (list(result=NULL, status=status))
  
  terms <- options$kruskalVariablesAssigned
  
  terms.base64 <- c()
  terms.normal <- c()
  
  for (term in terms) {
    
    components <- unlist(term)
    term.base64 <- paste(.v(components), collapse=":", sep="")
    term.normal <- paste(components, collapse=" \u273B ", sep="")
    
    terms.base64 <- c(terms.base64, term.base64)
    terms.normal <- c(terms.normal, term.normal)
  }

  nRows <- length(terms.base64)
  
  result <- list()
  
  result[["title"]] <- paste("Kruskal-Wallis Test")
  result[["name"]] <- paste("kruskalTable")
  
  fields <- list()
  fields[[length(fields) + 1]] <- list(name="Factor", type="string")
  fields[[length(fields) + 1]] <- list(name="Statistic", type="number", format="sf:4;dp:3")
  fields[[length(fields) + 1]] <- list(name="df", type="integer")
  fields[[length(fields) + 1]] <- list(name="p", type="number", format="dp:3;p:.001")
  
  footnotes <- .newFootnotes()
  result[["schema"]] <- list(fields=fields)
  
  rows <- list()
  
  for (i in .indices(terms.base64)) {

    if (perform == "run" && status$ready && status$error == FALSE)  {
        
        row <- list()
        
        reorderModelTerms <-  .reorderModelTerms(options)
        modelTerms <- reorderModelTerms$modelTerms
        model.formula <- as.formula(paste(.v(options$dependent), terms.base64[i], sep= "~"))
        r <- kruskal.test(model.formula, data = dataset)
        
        row[["Factor"]] <- terms.normal[i]
        row[["Statistic"]] <- .clean(r$statistic[[1]])
        row[["df"]] <- .clean(r$parameter[[1]])
        row[["p"]] <-.clean(r$p.value[[1]])
        
        rows[[i]] <- row
        
    } else {
      
        row <- list()
        row[["Factor"]] <- terms.normal[i]
        row[["Statistic"]] <- "."
        row[["df"]] <- "."
        row[["p"]] <- "."
        
        rows[[i]] <- row
          
    }
  }
  
  result[["data"]] <- rows
  result[["status"]] <- "complete"
    
  result[["footnotes"]] <- as.list(footnotes)
    

  
  if (perform == "run" && status$ready && status$error == FALSE)  {
    
    stateKruskal <- result
    
  } else {
    
    stateKruskal <- NULL
    
  }
  
  list(result=result, status=status, stateKruskal=stateKruskal)
}

.anovaDunnTable <- function(dataset, options, perform, model, status, stateDunn, singular) {
  
  if (options$dunnTest == FALSE)
    return (list(result=NULL, status=status, stateDunn=NULL))
  
  dunnVariables <- unlist(options$kruskalVariablesAssigned)
  dependentVar <- options$dependent
  
  dunnTables <- list()
  stateDunn <- list()
  
  for (dunnVar in dunnVariables) {
    
    dunnTable <- list()
    
    dunnTable[["title"]] <- paste("Dunn's Post Hoc Comparisons - ", dunnVar, sep="")
    dunnTable[["name"]] <- paste("dunnTest_", dunnVar, sep="")
    
    fields <- list(
      list(name="(I)",title="", type="string", combine=TRUE),
      list(name="(J)",title="", type="string"),
      list(name="z", type="number", format="sf:4;dp:3"),
      list(name="wA", title="W<sub>i</sub>", type="number", format="sf:4;dp:3"),
      list(name="wB", title="W<sub>j</sub>", type="number", format="sf:4;dp:3"),
      list(name="pval", title="p", type="number", format="dp:3;p:.001"),
      list(name="bonferroni", title="p<sub>bonf</sub>", type="number", format="dp:3;p:.001"),
      list(name="holm",title="p<sub>holm</sub>", type="number", format="dp:3;p:.001")
    )
    
    dunnTable[["schema"]] <- list(fields=fields)
    
    rows <- list()
    
    if (perform == "run" && status$ready && status$error == FALSE)  {
      
      variableLevels <- levels(dataset[[ .v(dunnVar) ]])
      nLevels <- length(variableLevels)
      nPerGroup <- unname(unlist(table(dataset[[ .v(dunnVar) ]])))
      bigN <- sum(nPerGroup)
      
      fullRanks <- rank(dataset[[ .v(dependentVar) ]])
      ranksPerGroup <- by(fullRanks, dataset[[ .v(dunnVar) ]], list)
      sumPerGroup <- unlist(lapply(ranksPerGroup, FUN = sum))
      meanPerGroup <- unname(sumPerGroup/nPerGroup)
      
      tab <- table(unlist(ranksPerGroup))
      nTies <- tab[tab > 1] 
      nTies <- sum(nTies^3 - nTies)
      
      for (i in 1:nLevels) {
        
        for (j in .seqx(i+1, nLevels)) {
          
          row <- list("(I)"=variableLevels[[i]], "(J)"=variableLevels[[j]])

          sigmaAB <- sqrt( ( (bigN * (bigN + 1))/12 - nTies/(12 * (bigN - 1)) ) * (1/nPerGroup[i] + 1/nPerGroup[j] )  )
          zAB <- (meanPerGroup[i] - meanPerGroup[j]) / sigmaAB 
          pValAB <- pnorm(abs(zAB), lower.tail = FALSE)
          
          row[["z"]] <- .clean(zAB)
          row[["wA"]]  <- .clean(meanPerGroup[i])
          row[["wB"]] <- .clean(meanPerGroup[j])
          row[["pval"]] <- .clean(pValAB)
          row[["bonferroni"]] <- .clean(pValAB)
          row[["holm"]] <- .clean(pValAB)
            
          dunnTable[["status"]] <- "complete"
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
      
      for (k in 1:length(rows)) {
        rows[[k]][['bonferroni']] <- .clean(allBonf[k])
        rows[[k]][['holm']] <- .clean(allHolm[k])
      }
      
    } else {
      row <- list("(I)"= ".", "(J)"= ".")
      row[["z"]] <- "."
      row[["wA"]]  <- "."
      row[["wB"]] <- "."
      row[["pval"]] <- "."
      row[["bonferroni"]] <- "."
      row[["holm"]] <- "."
      
      rows[[length(rows)+1]] <- row
    }

    dunnTable[["data"]] <- rows
    
    dunnTables[[length(dunnTables)+1]] <- dunnTable
    stateDunn <- dunnTables
  }

  list(result=dunnTables, status=status, stateDunn=stateDunn)
}

.anovaDescriptivesPlot <- function(dataset, options, perform, status, stateDescriptivesPlot) {

	descriptivesPlotList <- list()

	if (perform == "run" && status$ready && !status$error && options$plotHorizontalAxis != "" && options$dependent != "") {

		groupVars <- c(options$plotHorizontalAxis, options$plotSeparateLines, options$plotSeparatePlots)
		groupVars <- groupVars[groupVars != ""]
		groupVarsV <- .v(groupVars)
		dependentV <- .v(options$dependent)

		summaryStat <- .summarySE(as.data.frame(dataset), measurevar = dependentV, groupvars = groupVarsV,
						conf.interval = options$confidenceIntervalInterval, na.rm = TRUE, .drop = FALSE, errorBarType = options$errorBarType)

		colnames(summaryStat)[which(colnames(summaryStat) == dependentV)] <- "dependent"

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
			
			guideLegend <- ggplot2::guide_legend(nrow = min(10, nlevels(summaryStatSubset$plotSeparateLines)), title = options$plotSeparateLines, keywidth = 0.1, keyheight = 0.3, default.unit = "inch")
			
			p <- p + ggplot2::geom_line(position=pd, size = .7) +
				ggplot2::geom_point(position=pd, size=4) +
				ggplot2::scale_fill_manual(values = c(rep(c("white","black"),5),rep("grey",100)), guide=guideLegend) +
				ggplot2::scale_shape_manual(values = c(rep(c(21:25),each=2),21:25,7:14,33:112), guide=guideLegend) +
				ggplot2::scale_color_manual(values = rep("black",200),guide=guideLegend) +
				ggplot2::ylab(options$dependent) +
				ggplot2::xlab(options$plotHorizontalAxis) +
				base_breaks_y(summaryStat, options$plotErrorBars) +
				base_breaks_x(summaryStatSubset[,"plotHorizontalAxis"])
				
			p <- JASPgraphs::themeJasp(p, legend.position = "right")

			if (nPlots > 1) {
				descriptivesPlot[["title"]] <- paste(options$plotSeparatePlots,": ",subsetPlots[i], sep = "")
			} else {
				descriptivesPlot[["title"]] <- "Descriptives Plot"
			}

			if (options$plotSeparateLines != "") {

				content <- .writeImage(width = options$plotWidthDescriptivesPlotLegend,
									   height = options$plotHeightDescriptivesPlotLegend,
									   plot = p, obj = TRUE)

			} else {

				content <- .writeImage(width = options$plotWidthDescriptivesPlotNoLegend,
									   height = options$plotHeightDescriptivesPlotNoLegend,
									   plot = p, obj = TRUE)

			}

			descriptivesPlot[["data"]] <- content[["png"]]
			descriptivesPlot[["obj"]] <- content[["obj"]]
			descriptivesPlot[["convertible"]] <- TRUE

			#descriptivesPlot[["data"]] <- content
			descriptivesPlot[["status"]] <- "complete"

			descriptivesPlotList[[i]] <- descriptivesPlot

		}

		stateDescriptivesPlot <- descriptivesPlotList

	} else if (options$plotHorizontalAxis != "") {

		if (options$plotSeparatePlots != "") {

			nPlots <- length(levels(dataset[[ .v(options$plotSeparatePlots) ]]))

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

.qqPlot <- function(model, options, perform, status, stateqqPlot) {
    
	if (!options$qqPlot)
		return(list(result=NULL, status=status))

	qqPlot <- list()

	if (perform == "run" && status$ready && !status$error && !is.null(model)) {

		qqPlot$title <- "Q-Q Plot"
		
		# Hardcode plot dimensions
		options$plotWidthQQPlot <- 400
		options$plotHeightQQPlot <- 400
		
		qqPlot$width <- options$plotWidthQQPlot
		qqPlot$height <- options$plotHeightQQPlot
		#qqPlot$custom <- list(width="plotWidthQQPlot", height="plotHeightQQPlot")

		standResid <- as.data.frame(stats::qqnorm(rstandard(model), plot.it=FALSE))

		standResid <- na.omit(standResid)
		xVar <- standResid$x
		yVar <- standResid$y

		# Format x ticks
		xlow <- min(pretty(xVar))
		xhigh <- max(pretty(xVar))
		xticks <- pretty(c(xlow, xhigh))
		
		# format x labels
		xLabs <- vector("character", length(xticks))
		for (i in seq_along(xticks)) {
			if (xticks[i] < 10^6) {
				xLabs[i] <- format(xticks[i], digits= 3, scientific = FALSE)
			} else {
				xLabs[i] <- format(xticks[i], digits= 3, scientific = TRUE)
			}
		}

		# Format y ticks
		ylow <- min(pretty(yVar))
		yhigh <- max(pretty(yVar))        
		yticks <- pretty(c(ylow, yhigh))
		
		# format y labels
		yLabs <- vector("character", length(yticks))
		for (i in seq_along(yticks)) {
		    if (yticks[i] < 10^6) {
		        yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
		    } else {
		        yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
		    }
		}
		
		p <- JASPgraphs::drawAxis(xName = "Theoretical Quantiles", yName = "Standardized Residuals", xBreaks = xticks, yBreaks = xticks, yLabels = xLabs, xLabels = xLabs, force = TRUE)
	    p <- p + ggplot2::geom_line(data = data.frame(x = c(min(xticks), max(xticks)), y = c(min(xticks), max(xticks))), mapping = ggplot2::aes(x = x, y = y), col = "darkred", size = 1)
		p <- JASPgraphs::drawPoints(p, dat = data.frame(xVar, yVar), size = 3)
		
		# JASP theme
	    p <- JASPgraphs::themeJasp(p)
		
		content <- .writeImage(width = options$plotWidthQQPlot,
									   height = options$plotHeightQQPlot,
									   plot = p, obj = TRUE)

		qqPlot[["convertible"]] <- TRUE
		qqPlot[["obj"]] <- content[["obj"]]
		qqPlot$data <- content[["png"]]
		qqPlot$status <- "complete"

		stateqqPlot <- qqPlot

	} else {

		qqPlot$title <- "Q-Q Plot"
		
		# Hardcode plot dimensions
		options$plotWidthQQPlot <- 400
		options$plotHeightQQPlot <- 400
		
		qqPlot$width <- options$plotWidthQQPlot
		qqPlot$height <- options$plotHeightQQPlot
		#qqPlot$custom <- list(width="plotWidthQQPlot", height="plotHeightQQPlot")
		qqPlot$data <- NULL

		stateqqPlot <- NULL

		if (status$error)
			qqPlot$error <- list(errorType="badData")

	}

	list(result=qqPlot, status=status, stateqqPlot=stateqqPlot)
}
