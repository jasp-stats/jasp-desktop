
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
	
	ttest[["citation"]] <- list(
		"Morey, R. D. & Rouder, J. N. (2014). BayesFactor (Version 0.99)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t-tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 752-760")

	bf.type <- options$bayesFactorType
	
	if (bf.type == "BF10") {
		bf.title <- "BF\u2081\u2080"
	} else {
		bf.title <- "BF\u2080\u2081"
	}

	fields <- list(
		list(name="Variable", type="string", title=""),
		list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
		list(name="error", type="number", format="sf:4;dp:3", title="error %"))

	ttest[["schema"]] <- list(fields=fields)

	footnotes <- .newFootnotes()
	
	#if (options$hypothesis == "greaterThanTestValue") {
	#
	#	message <- paste("All tests, hypothesis is sample mean is greater than ", 0, sep="")
	#	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	#
	#} else if (options$hypothesis == "lessThanTestValue") {
	#
	#	message <- paste("All tests, hypothesis is sample mean is less than ", 0, sep="")
	#	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	#	
	#} else {
	#
	#}

	if (length(options[["variables"]]) > 0)
	{
		ttest.rows <- list()

		for (variable in options[["variables"]])
		{
			ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable, "BF"=".", error=".")	
		}
		
		if (perform == "run") {

			i <- 1

			for (variable in options[["variables"]])
			{
				result <- try (silent = TRUE, expr = {
				
					variableData <- dataset[[ .v(variable) ]]
					variableData <- variableData[ ! is.na(variableData) ]

					r <- BayesFactor::ttestBF(variableData, r=options$priorWidth)
		
					bf.raw <- exp(as.numeric(r@bayesFactor$bf))
					if (bf.type == "BF01")
						bf.raw <- 1 / bf.raw
						
					BF <- .clean(bf.raw)
					error <- .clean(as.numeric(r@bayesFactor$error))

					list(Variable=variable, BF=BF, error=error)
				})

				if (class(result) == "try-error") {
				
					errorMessage <- .extractErrorMessage(result)
						
					if (errorMessage == "x or y must not contain missing or infinite values.") {
				
						errorMessage <- paste("BayesFactor is undefined - the sample contains infinity")
					
					#} else if (errorMessage == "data are essentially constant") {
					#				
					#	errorMessage <- paste("BayesFactor is undefined - the sample contains all the same value (the variance is zero)")
					#
					} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
					
						errorMessage <- "BayesFactor is undefined - the sample has too few observations"	
					}
				
					index <- .addFootnote(footnotes, errorMessage)
				
					result <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
				}
		
				ttest.rows[[i]] <- result
				
				i <- i + 1
		
				ttest[["data"]] <- ttest.rows
				ttest[["footnotes"]] <- as.list(footnotes)
	
				results[["ttest"]] <- ttest
				
				if (callback(results) != 0)
					return(NULL)
			}
		}
		
		ttest[["data"]] <- ttest.rows
		ttest[["footnotes"]] <- as.list(footnotes)
	}

	results[["ttest"]] <- ttest

	results
}

