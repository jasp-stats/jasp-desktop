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

AncovaBayesian	 <- function(dataset = NULL, options, perform = "run", callback = function(...) list(status = "ok"), ...) {
##PREAMBLE
	if (is.null(base::options()$BFMaxModels))
		base::options(BFMaxModels = 50000)
	if (is.null(base::options()$BFpretestIterations))
		base::options(BFpretestIterations = 100)
	if (is.null(base::options()$BFapproxOptimizer))
		base::options(BFapproxOptimizer = "optim")
	if (is.null(base::options()$BFapproxLimits))
		base::options(BFapproxLimits = c(-15, 15))
	if (is.null(base::options()$BFprogress))
		base::options(BFprogress = interactive())
	if (is.null(base::options()$BFfactorsMax))
		base::options(BFfactorsMax = 5)
		
	env <- environment()

	.callbackBFpackage <- function(...) {
		response <- .callbackBayesianLinearModels()
		if(response$status == "ok")
			return(as.integer(0))
		return(as.integer(1))
	}

	.callbackBayesianLinearModels <- function (results = NULL, progress = NULL) {
		response <- callback(results, progress)
		if (response$status == "changed") {
		
			change <- .diff(env$options, response$options)
			
			env$options <- response$options
			
			if (change$modelTerms || 
				change$dependent ||
				change$covariates ||
				change$fixedFactors ||
				change$randomFactors ||
				change$priorFixedEffects ||
				change$priorRandomEffects ||
				change$priorCovariates ||
				change$sampleMode ||
				change$fixedSamplesNumber)
				return(response)
			response$status <- "ok"
		}
		return(response)
	}

	state <- .retrieveState()
	if (! is.null(state)) {
		change <- .diff(options, state$options)
		if (! base::identical(change, FALSE) && (change$modelTerms || 
				change$dependent ||
				change$covariates ||
				change$fixedFactors ||
				change$randomFactors ||
				change$posteriorEstimates ||
				change$posteriorEstimatesMCMCIterations ||
				change$priorFixedEffects ||
				change$priorRandomEffects ||
				change$priorCovariates ||
				change$sampleMode ||
				change$fixedSamplesNumber)) {
			state <- NULL
		} else {
			perform <- "run"
		}
	}

## META
	results <- list()
	meta <- list()
	meta[[1]] <- list(name = "title", type = "title")
	meta[[2]] <- list(name = "model comparison", type = "table")
	meta[[3]] <- list(name = "effects", type = "table")
	meta[[4]] <- list(name = "estimates", type = "table")
	meta[[5]] <- list(name = "posthoc", type = "collection", meta = "table")
	
	wantsTwoPlots <- options$plotSeparatePlots
	if (wantsTwoPlots == "") {
		meta[[6]] <- list(
			name = "descriptivesObj", type = "object", 
			meta = list(list(name = "descriptivesTable", type = "table"), list(name = "descriptivesPlot", type = "image"))
			)
	} else {
		meta[[6]] <- list(
			name = "descriptivesObj", type = "object", 
			meta = list(list(name = "descriptivesTable", type = "table"), list(name = "descriptivesPlot", type = "collection", meta = "image"))
			)	
	}
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian ANCOVA"

## DATA
	dataset <- .readBayesianLinearModelData(dataset, options, perform)

	if (is.null(state)) {

##STATUS (INITIAL)
		status <- .setBayesianLinearModelStatus(dataset, options, perform)

## MODEL
		model.object <- .theBayesianLinearModels(dataset, options, perform, status, .callbackBayesianLinearModels, .callbackBFpackage, results, analysisType = "ANCOVA")
	
		if (is.null(model.object)) # analysis cancelled by the callback
			return()
	
		model <- model.object$model
		status <- model.object$status
	} else {
		model <- state$model
		status <- state$status
	}
	
## Posterior Table
	model.comparison <- .theBayesianLinearModelsComparison(model, options, perform, status, populate = FALSE)
	results[["model comparison"]] <- model.comparison$modelTable
	
	if (is.null(state))
		model <- model.comparison$model

## Effects Table
	results[["effects"]] <- .theBayesianLinearModelsEffects(model, options, perform, status, populate = FALSE)

## Posterior Estimates
	results[["estimates"]] <- .theBayesianLinearModelEstimates(model, options, perform, status)
	
## Post Hoc Table
	results[["posthoc"]] <- .anovaNullControlPostHocTable(dataset, options, perform, status, analysisType = "ANCOVA")

## Descriptives Table
	descriptivesTable <- .anovaDescriptivesTable(dataset, options, perform, status, stateDescriptivesTable = NULL)[["result"]]

## Descriptives Plot
	options$plotErrorBars <- options$plotCredibleInterval
	options$errorBarType <- "confidenceInterval"
	options$confidenceIntervalInterval <- options$plotCredibleIntervalInterval
	plotOptionsChanged <- isTRUE( identical(wantsTwoPlots, options$plotSeparatePlots) == FALSE )
	descriptivesPlot <- .anovaDescriptivesPlot(dataset, options, perform, status, stateDescriptivesPlot = NULL)[["result"]]
	
	if (length(descriptivesPlot) == 1) {
		results[["descriptivesObj"]] <- list(
			title = "Descriptives", descriptivesTable = descriptivesTable, 
			descriptivesPlot = descriptivesPlot[[1]]
			)
			
		if (plotOptionsChanged) 
			results[[".meta"]][[6]][["meta"]][[2]] <- list(name = "descriptivesPlot", type = "image")
			
	} else {	
		results[["descriptivesObj"]] <- list(
			title = "Descriptives", descriptivesTable = descriptivesTable, 
			descriptivesPlot = list(collection = descriptivesPlot, title = "Descriptives Plots")
			)
			
		if (plotOptionsChanged)
			results[[".meta"]][[6]][["meta"]][[2]] <- list(name = "descriptivesPlot", type = "collection", meta = "image")
				
	}

	keepDescriptivesPlot <- lapply(descriptivesPlot, function(x) x$data)
		
	new.state <- list(options = options, model = model, status = status, keep = keepDescriptivesPlot)
	
	if (perform == "run" || ! status$ready || ! is.null(state)) {
		return(list(results = results, status = "complete", state = new.state, keep = keepDescriptivesPlot))
	} else {
		return(list(results = results, status = "inited", keep = keepDescriptivesPlot))
	}
}
