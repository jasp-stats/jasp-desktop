
TTestBayesianOneSample <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	all.variables <- unlist(options$variables)

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
			
			} else {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
			}
			
		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
		}
	}

	results <- list()
	
	
	
	meta <- list()
	
	meta[[1]] <- list(name="ttest", type="table")
	
	results[[".meta"]] <- meta
	
	
	
	ttest <- list()

	ttest[["title"]] <- "Bayesian One Sample T-Test"

	fields <- list(
		list(name="Variable", type="string", title=""),
		list(name="BF10", type="number", format="sf:4;dp:3", title="BF\u2081\u2080"),
		list(name="error", type="number", format="sf:4;dp:3", title="error %"))

	ttest[["schema"]] <- list(fields=fields)

	if (length(options[["variables"]]) > 0)
	{
		ttest.results <- list()

		for (variable in options[["variables"]])
		{
			ttest.results[[length(ttest.results)+1]] <- list(Variable=variable, "BF10"=".", error=".")	
		}
		
		if (perform == "run") {

			c <- 1

			for (variable in options[["variables"]])
			{
				result <- try (silent = TRUE, expr = {
				
					variableData <- dataset[[ .v(variable) ]]
					variableData <- variableData[ ! is.na(variableData) ]

					r <- BayesFactor::ttestBF(variableData, r=options$priorWidth)
		
					BF <- .clean(exp(as.numeric(r@bayesFactor$bf)))
					error <- .clean(as.numeric(r@bayesFactor$error))

					list(Variable=variable, "BF10"=BF, error=error)
				})

				if (class(result) == "try-error")
					result <- list(Variable=variable, "BF10"="", error="")
		
				ttest.results[[c]] <- result
				c <- c + 1
		
				ttest[["data"]] <- ttest.results
	
				results[["ttest"]] <- ttest
				
				if (callback(results) != 0)
					return(NULL)
			}
		}
		
		ttest[["data"]] <- ttest.results
	}

	results[["ttest"]] <- ttest

	results
}

