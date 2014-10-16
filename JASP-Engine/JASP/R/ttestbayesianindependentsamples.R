
TTestBayesianIndependentSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	dependents <- unlist(options$variables)
	
	grouping   <- options$groupingVariable
	if (grouping == "")
		grouping <- NULL

	if (is.null(dataset))
	{
		if (perform == "run") {
		
			if (options$missingValues == "excludeListwise") {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=c(dependents, grouping))
			
			} else {
		
				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=grouping)
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

			subDataSet <- subset(dataset,    subset=( ! is.na( dataset[[ .v(variable) ]] )), select=c(.v(variable), .v(options$groupingVariable)))
			subDataSet <- subset(subDataSet, subset=( ! is.na( dataset[[ .v(options$groupingVariable) ]] )))
			
			f <- as.formula(paste( .v(variable), "~", .v(options$groupingVariable)))
			r.size <- options$priorWidth
			
			if (options$hypothesis == "groupOneGreater") {
			
				null.interval <- c(-Inf, 0)
			
			} else if (options$hypothesis == "groupTwoGreater") {

				null.interval <- c(0, Inf)
			
			} else {
			
				null.interval <- c(-Inf, Inf)
			}
			
			result <- try (silent=FALSE, expr= {
			
				bf    <- BayesFactor::ttestBF(data=subDataSet, formula=f, r=r.size, nullInterval=null.interval)[1]
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
		
	} else {

		fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2081\u2080")	
	}
	
	
	fields[[length(fields)+1]] <- list(name="error", type="number", format="sf:4;dp:3", title="error %")
		
	ttest[["schema"]] <- list(fields=fields)
	
	footnotes <- .newFootnotes()
	
	
	ttest.rows <- list()
	
	for (variable in options[["variables"]]) {

		ttest.rows[[length(ttest.rows)+1]] <- list(.variable=variable)
	}
	
	if (perform == "run" && length(options$variables) != 0 && options$groupingVariable != "") {

		levels <- unique(dataset[[ .v(options$groupingVariable) ]])
		
		if (length(levels) != 2) {
		
			ttest[["error"]] <- list(errorType="badData", errorMessage="The Grouping Variable must have 2 levels")
			
		} else {
		
			rowNo <- 1
		
			for (variable in options[["variables"]]) {

				# BayesFactor package doesn't handle NAs, so it is necessary to exclude them
				
				subDataSet <- subset(dataset, select=c(.v(variable), .v(options$groupingVariable)))
				subDataSet <- na.omit(subDataSet)

				f <- as.formula(paste( .v(variable), "~", .v(options$groupingVariable)))
				r.size <- options$priorWidth
				
				if (options$hypothesis == "groupOneGreater") {
			
					null.interval <- c(0, Inf)
			
				} else if (options$hypothesis == "groupTwoGreater") {

					null.interval <- c(-Inf, 0)
			
				} else {
			
					null.interval <- c(-Inf, Inf)
				}
				
				result <- try (silent=FALSE, expr= {
				
					bf    <- BayesFactor::ttestBF(data=subDataSet, formula=f, r=r.size, nullInterval=null.interval)[1]
					
					if (options$bayesFactorType == "BF01")
						bf <- 1 / bf
					
					BF    <- .clean(exp(as.numeric(bf@bayesFactor$bf)))
					error <- .clean(as.numeric(bf@bayesFactor$error))
				
					list(.variable=variable, BF=BF, error=error)					
				})

				if (class(result) == "try-error") {
			
					errorMessage <- .extractErrorMessage(result)
					
					if (errorMessage == "Dependent variable must not contain missing or infinite values.") {
					
						errorMessage <- "BayesFactor is undefined - the dependent variable contains infinity"
						
					} else if (errorMessage == "grouping factor must have exactly 2 levels") {
					
						# We know that the grouping factor *does* have two levels, because we've checked this earlier on
						# This error means that all of one factor has been excluded because of missing values in the dependent
						
						errorMessage <- "BayesFactor is undefined - the grouping variable contains less than two levels once missing values in the dependent are excluded"
						
					} else if (errorMessage == "data are essentially constant") {
					
						errorMessage <- "BayesFactor is undefined - one or both levels of the dependent contain all the same value (the variance is zero)"
						
					} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
					
						errorMessage <- "BayesFactor is undefined - one or both levels of the dependent contain too few observations"
					}
					
					index <- .addFootnote(footnotes, errorMessage)

					result <- list(.variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
				}
				
				ttest.rows[[rowNo]] <- result
			
				rowNo <- rowNo + 1
			}

			if (options$hypothesis == "groupOneGreater") {
			
				gs <- base::levels(levels)
				g1 <- gs[1]
				g2 <- gs[2]
				message <- paste("All tests, hypothesis is group <em>", g1, "</em> greater than group <em>", g2, "</em>", sep="")
			
				.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
				
			} else if (options$hypothesis == "groupTwoGreater") {
			
				gs <- base::levels(levels)
				g1 <- gs[1]
				g2 <- gs[2]
				message <- paste("All tests, hypothesis is group <em>", g1, "</em> less than group <em>", g2, "</em>", sep="")
			
				.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
			}
			
			ttest[["footnotes"]] <- as.list(footnotes)
		}
	}
	
	ttest[["data"]] <- ttest.rows
	
	ttest

}

