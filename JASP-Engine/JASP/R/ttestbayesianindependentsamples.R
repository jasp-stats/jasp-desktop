
TTestBayesianIndependentSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	dependents <- unlist(options$variables)
	
	grouping   <- options$groupingVariable
	if (grouping == "")
		grouping <- NULL

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- read.dataset.to.end(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=dependents)
			
			} else {
		
				dataset <- read.dataset.to.end(columns.as.numeric=dependents, columns.as.factor=grouping)
			}
		

		} else {
		
			dataset <- read.dataset.header(columns.as.numeric=dependents, columns.as.factor=grouping)
		}
	}

	results <- list()

	results[["ttest"]] <- .ttestBayesianIndependentSamplesTTest(dataset, options, perform)
	results[["descriptives"]] <- .ttestIndependentSamplesDescriptives(dataset, options, perform)

	results
}

.ttestBayesianIndependentSamplesTTest <- function(dataset, options, perform) {

	ttest <- list()

	ttest[["title"]] <- "Bayesian Independent Samples T-Test"

	fields <- list(
		list(name=".variable", title="", type="string", combine=TRUE))
	
	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4", title="BF<sub>10</sub>")
	fields[[length(fields)+1]] <- list(name="error", type="number", format="sf:4")
		
	ttest[["schema"]] <- list(fields=fields)
	
	ttest.results <- list()
	
	for (variable in options[["variables"]]) {

		ttest.results[[length(ttest.results)+1]] <- list(.variable=variable)
	}
	
	if (perform == "run" && length(options$variables) != 0 && options$groupingVariable != "") {

		levels <- unique(dataset[[ .v(options$groupingVariable) ]])
		
		if (length(levels) != 2) {
		
			ttest[["error"]] <- list(errorType="badData", errorMessage="The Grouping Variable must have 2 levels")
			
		} else {
		
			rowNo <- 1
		
			for (variable in options[["variables"]]) {

				variableData <- dataset[[ .v(variable) ]]
				
				result <- try (silent=FALSE, expr= {

					f <- as.formula(paste( .v(variable), "~", .v(options$groupingVariable)))
					
					if (options$tails == "oneTailedGreaterThan") {
					
						bf <- BayesFactor::ttestBF(data=dataset, formula=f, r=1, nullInterval=c(-Inf, 0))
						bf <- bf[1]
					
					} else if (options$tails == "oneTailedLessThan") {
	
						bf <- BayesFactor::ttestBF(data=dataset, formula=f, r=1, nullInterval=c(0, Inf))
						bf <- bf[1]
					
					} else {
					
						bf <- BayesFactor::ttestBF(data=dataset, formula=f, r=1)
					
					}
				
					BF <- .clean(exp(as.numeric(bf@bayesFactor$bf)))
					error <- .clean(as.numeric(bf@bayesFactor$error))
				
					list(.variable=variable, BF=BF, error=error)					
				})

				if (class(result) == "try-error") {
			
					result <- list(.variable=variable, BF="", error="")
				}
			
				ttest.results[[rowNo]] <- result
				rowNo <- rowNo + 1
			}
		}
	}
	
	ttest[["data"]] <- ttest.results
	
	ttest

}

