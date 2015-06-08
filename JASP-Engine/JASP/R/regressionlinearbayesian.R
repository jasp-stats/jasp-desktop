RegressionLinearBayesian <- function (dataset = NULL, options, perform = "run", callback = function(...) list(status = "ok"), ...) {
##PREAMBLE
	if (is.null (base::options ()$BFMaxModels))
		base::options (BFMaxModels = 50000)
	if (is.null (base::options ()$BFpretestIterations))
		base::options (BFpretestIterations = 100)
	if (is.null (base::options ()$BFapproxOptimizer))
		base::options (BFapproxOptimizer = "optim")
	if (is.null (base::options ()$BFapproxLimits))
		base::options (BFapproxLimits = c (-15,15))
	if (is.null (base::options ()$BFprogress))
		base::options (BFprogress = interactive())
	if (is.null (base::options ()$BFfactorsMax))
		base::options (BFfactorsMax = 5)

	state <- .retrieveState ()
	print(state)
	if ( ! is.null (state)) {
		change <- .diff (options, state$options)
		if ( ! base::identical(change, FALSE) && (change$dependent || change$modelTerms)) {
			state <- NULL
		} else {
			perform <- "run"
		}
	}
## META
	results <- list ()
	meta <- list ()
	meta [[1]] <- list (name = "title", type = "title")
	meta [[2]] <- list (name = "model comparison", type = "table")
	meta [[3]] <- list (name = "effects", type = "table")
	results [[".meta"]] <- meta
	results [["title"]] <- "Bayesian ANOVA"

## DATA
	if (is.null(state)) {
		dataset <- .readBayesianLinearModelData (dataset, options, perform)

##STATUS (INITIAL)
		status <- .setBayesianLinearModelStatus (dataset, options, perform)

## MODEL
		model.object <- .theBayesianLinearModels (dataset, options, perform, status, callback, results = results)
	
		if (is.null(model.object))
			return()

		model <- model.object$model
		status <- model.object$status
	} else {
		model <- state$model
		status <- state$status
	}

## Posterior Table
	model.comparison <- .theBayesianLinearModelsComparison (model, options, perform, status, populate = FALSE)
	results [["model comparison"]] <- model.comparison$modelTable
	
	if ( is.null (state))
		model <- model.comparison$model

## Effects Table
	results [["effects"]] <- .theBayesianLinearModelsEffects (model, options, perform, status, populate = FALSE)

	new.state <- list (options = options, model = model, status = status)
	
	if (perform == "run" || !status$ready || ! is.null (state)) {
		return (list (results = results, status = "complete", state = new.state))
	} else {
		return (list (results = results, status = "inited"))
	}
}
