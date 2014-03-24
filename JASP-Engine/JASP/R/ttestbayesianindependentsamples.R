
TTestBayesianIndependentSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- read.dataset.to.end(exclude.na.listwise = options$variables)
			
			} else {
		
				dataset <- read.dataset.to.end()
			}
		

		} else {
		
			dataset <- read.dataset.header()
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

		levels <- unique(dataset[[options$groupingVariable]])
		
		if (length(levels) != 2) {
		
			ttest[["error"]] <- list(errorType="badData", errorMessage="The Grouping Variable must have 2 levels")
			
		} else {
		
			rowNo <- 1
		
			for (variable in options[["variables"]]) {

				variableData <- dataset[[variable]]
				
				result <- try (silent=FALSE, expr= {

					f <- as.formula(paste(variable, "~", options$groupingVariable))
					r <- BayesFactor::ttestBF(data=dataset, formula=f)
				
					BF <- .clean(exp(as.numeric(r@bayesFactor$bf)))
					error <- .clean(as.numeric(r@bayesFactor$error))
				
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

