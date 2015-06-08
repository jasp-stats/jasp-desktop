AnovaBayesian <- function (dataset = NULL, options, perform = "run", callback = function(...) list(status = "ok"), ...) {
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

## META
	results <- list ()
	meta <- list ()
	meta [[1]] <- list (name = "title", type = "title")
	meta [[2]] <- list (name = "model comparison", type = "table")
	meta [[3]] <- list (name = "effects", type = "table")
	results [[".meta"]] <- meta
	results [["title"]] <- "Bayesian ANOVA"

## DATA
	dataset <- .readBayesianLinearModelData (dataset, options, perform)

##STATUS (INITIAL)
	status <- .setBayesianLinearModelStatus (dataset, options, perform)

## MODEL
	model.object <- .theBayesianLinearModels (dataset, options, perform, status, callback, results = results)
	
	if (is.null(model.object)) # analysis cancelled by the callback
		return()
		
	model <- model.object$model
	status <- model.object$status

## Posterior Table
	model.comparison <- .theBayesianLinearModelsComparison (model, options, perform, status, populate = FALSE)
	results [["model comparison"]] <- model.comparison$modelTable
	model <- model.comparison$model

## Effects Table
	results [["effects"]] <- .theBayesianLinearModelsEffects (model, options, perform, status, populate = FALSE)

	if (perform == "run" || !status$ready) {
		return (list (results = results, status = "complete"))
	} else {
		return (list (results = results, status = "inited"))
	}
}
