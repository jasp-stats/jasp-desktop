
TTestBayesianIndependentSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	dependents <- unlist(options$variables)
	
	grouping   <- options$groupingVariable
	if (grouping == "")
		grouping <- NULL

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=dependents)
			
			} else {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping)
			}
		

		} else {
		
			dataset <- .readDataSetHeader(columns.as.numeric=dependents, columns.as.factor=grouping)
		}
	}

	results <- list()
	
	
	meta <- list()
	
	meta[[1]] <- list(name="ttest", type="table")
	meta[[2]] <- list(name="descriptives", type="table")
	meta[[3]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta
	
	results[["ttest"]] <- .ttestBayesianIndependentSamplesTTest(dataset, options, perform)
	results[["descriptives"]] <- .ttestIndependentSamplesDescriptives(dataset, options, perform)
	results[["plots"]] <- .ttestBayesianIndependentSamplesTTestPlots(dataset, options, perform)

	results
}

.ttestBayesianIndependentSamplesTTestPlots <- function(dataset, options, perform) {
	
	if (options$plots == FALSE)
		return(NULL)

	plots <- list()
	
	for (variable in options[["variables"]])
		plots[[length(plots)+1]] <- list(title=variable, width=options$plotWidth, height=options$plotHeight, custom=list(width="plotWidth", height="plotHeight"))
	
	if (perform == "run" && length(options$variables) != 0 && options$groupingVariable != "") {
		
		rowNo <- 1
	
		for (variable in options[["variables"]]) {

			variableData <- dataset[[ .v(variable) ]]
			
			f <- as.formula(paste( .v(variable), "~", .v(options$groupingVariable)))
			r.size <- options$priorWidth
			
			if (options$tails == "oneTailedGroupOneGreater") {
			
				null.interval <- c(-Inf, 0)
			
			} else if (options$tails == "oneTailedGroupTwoGreater") {

				null.interval <- c(0, Inf)
			
			} else {
			
				null.interval <- c(-Inf, Inf)
			}
			
			result <- try (silent=FALSE, expr= {
			
				bf    <- BayesFactor::ttestBF(data=dataset, formula=f, r=r.size, nullInterval=null.interval)[1]
				BF    <- .clean(exp(as.numeric(bf@bayesFactor$bf)))
				error <- .clean(as.numeric(bf@bayesFactor$error))
				
				image <- .beginSaveImage(options$plotWidth, options$plotHeight)

				posterior.samples <- BayesFactor::ttestBF(data=dataset, formula=f, r=r.size, nullInterval=null.interval, posterior=TRUE, iterations=10000, progress=FALSE)

				hist(posterior.samples, main=variable, xlab="", ylab="Frequency", col=rainbow(20))
				
				data <- .endSaveImage(image)
				
				data
			})

			if (class(result) == "try-error") {
		
				result <- NULL
			}

			plots[[rowNo]][["data"]] <- result
		
			rowNo <- rowNo + 1
		}
	}
	
	plots

}

.ttestBayesianIndependentSamplesTTest <- function(dataset, options, perform) {

	ttest <- list()

	ttest[["title"]] <- "Bayesian Independent Samples T-Test"

	fields <- list(
		list(name=".variable", title="", type="string", combine=TRUE))
	
	if (options$bayesFactorType == "BF01") {
	
		fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u2081")
	}
	else {

		fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2081\u2080")	
	}
	
	
	fields[[length(fields)+1]] <- list(name="error", type="number", format="sf:4;dp:3", title="error %")
		
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
				
				f <- as.formula(paste( .v(variable), "~", .v(options$groupingVariable)))
				r.size <- options$priorWidth
				
				if (options$tails == "oneTailedGroupOneGreater") {
				
					null.interval <- c(-Inf, 0)
				
				} else if (options$tails == "oneTailedGroupTwoGreater") {

					null.interval <- c(0, Inf)
				
				} else {
				
					null.interval <- c(-Inf, Inf)
				}
				
				result <- try (silent=FALSE, expr= {
				
					bf    <- BayesFactor::ttestBF(data=dataset, formula=f, r=r.size, nullInterval=null.interval)[1]
					
					if (options$bayesFactorType == "BF01")
						bf <- 1 / bf
					
					BF    <- .clean(exp(as.numeric(bf@bayesFactor$bf)))
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

