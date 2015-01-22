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
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="ttest", type="table")
	meta[[3]] <- list(name="descriptives", type="table")
	meta[[4]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian T-Test"
	
	ttest.results <- .ttestBayesianIndependentSamplesTTest(dataset, options, perform)
	results[["ttest"]] <- ttest.results[[1]]
	status <- ttest.results[[2]]
	g1 <- ttest.results[[3]]
	g2 <- ttest.results[[4]]
	results[["plots"]] <- .ttestBayesianIndependentSamplesTTestPlots(dataset, options, perform, status, g1, g2)
	results[["descriptives"]] <- .ttestIndependentSamplesDescriptives(dataset, options, perform)
	
	results
}

.ttestBayesianIndependentSamplesTTestPlots <- function(dataset, options, perform, status, g1, g2) {
	
	if(is.null(options()$BFMaxModels)) options(BFMaxModels = 50000)
	if(is.null(options()$BFpretestIterations)) options(BFpretestIterations = 100)
	if(is.null(options()$BFapproxOptimizer)) options(BFapproxOptimizer = "optim")
	if(is.null(options()$BFapproxLimits)) options(BFapproxLimits = c(-15,15))
	if(is.null(options()$BFprogress)) options(BFprogress = interactive())
	if(is.null(options()$BFfactorsMax)) options(BFfactorsMax = 5)
	
	
	if (!options$plotPriorAndPosterior & !options$plotSequentialAnalysis & !options$plotSequentialAnalysisRobustness & !options$plotBayesFactorRobustness){
		return(NULL)
	}
	
	plots.ttest <- list()
	
	q <- 1
	
	for (variable in options[["variables"]]){
	
		if (options$plotPriorAndPosterior){
			plot <- list()
			
			plot[["title"]] <- variable
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["custom"]] <- list(width="chartWidth", height="chartHeight")
			
			plots.ttest[[q]] <- plot
			q <- q + 1
		}
		
		if (options$plotSequentialAnalysis){
			plot <- list()
			
			plot[["title"]] <- variable
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["custom"]] <- list(width="chartWidth", height="chartHeight")
			
			plots.ttest[[q]] <- plot
			q <- q + 1
		}
		
		if (options$plotSequentialAnalysisRobustness){
			plot <- list()
			
			plot[["title"]] <- variable
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["custom"]] <- list(width="chartWidth", height="chartHeight")
			
			plots.ttest[[q]] <- plot
			q <- q + 1
		}
		
		if (options$plotBayesFactorRobustness){
			plot <- list()
			
			plot[["title"]] <- variable
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["custom"]] <- list(width="chartWidth", height="chartHeight")
			
			plots.ttest[[q]] <- plot
			q <- q + 1
		}
	}
		
	
	if (perform == "run" && length(options$variables) != 0 && options$groupingVariable != "") {
		
		 if(status == "error"){
		 
			return(plots.ttest)
		}
		
		z <- 1
	
		for (variable in options[["variables"]]) {
		

			subDataSet <- subset(dataset, select=c(.v(variable), .v(options$groupingVariable)))
			subDataSet <- na.omit(subDataSet)
			
			
			r.size <- options$priorWidth
			
			group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(variable)] 
			group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(variable)] 
			
			
			if (options$hypothesis == "groupOneGreater") {
			
				oneSided <- "right"
			
			} else if (options$hypothesis == "groupTwoGreater") {

				oneSided <- "left"
			
			} else {
			
				oneSided <- FALSE
			}
			
			
			if(options$plotPriorAndPosterior){
						
				image <- .beginSaveImage(530, 400)
				
				.plotPosterior.ttest(x= group2, y= group1, paired= FALSE, oneSided= oneSided, rscale = options$priorWidth)
									
				content <- .endSaveImage(image)
				
				plot <- plots.ttest[[z]]
				
				plot[["data"]]  <- content
				
				plots.ttest[[z]] <- plot
				z <- z + 1
			}
			
			if(options$plotBayesFactorRobustness){
			
				image <- .beginSaveImage(530, 400)
				
				.plotBF.robustnessCheck.ttest(x= group2, y= group1, paired= FALSE, oneSided= oneSided, rscale = options$priorWidth)
									
				content <- .endSaveImage(image)
				
				plot <- plots.ttest[[z]]
				
				plot[["data"]]  <- content
				
				plots.ttest[[z]] <- plot
				z <- z + 1
			}
			
			if(options$plotSequentialAnalysis){
			
				image <- .beginSaveImage(530, 400)
				
				.plotSequentialBF.ttest(x= group2, y= group1, paired= FALSE, oneSided= oneSided, rscale = options$priorWidth)
									
				content <- .endSaveImage(image)
				
				plot <- plots.ttest[[z]]
				
				plot[["data"]]  <- content
				
				plots.ttest[[z]] <- plot
				z <- z + 1
			}
			
			if(options$plotSequentialAnalysisRobustness){
			
				image <- .beginSaveImage(530, 400)
				
				.plotSequentialBF.ttest(x= group2, y= group1, paired= FALSE, oneSided= oneSided, rscale = options$priorWidth, plotDifferentPriors= TRUE)
									
				content <- .endSaveImage(image)
				
				plot <- plots.ttest[[z]]
				
				plot[["data"]]  <- content
				
				plots.ttest[[z]] <- plot
				z <- z + 1
			}
		}
	}
	
	plots.ttest

}

.ttestBayesianIndependentSamplesTTest <- function(dataset, options, perform) {

	g1 <- NULL
	g2 <- NULL
	status <- "ready"

	ttest <- list()

	ttest[["title"]] <- "Bayesian Independent Samples T-Test"
	
	ttest[["citation"]] <- list(
		"Morey, R. D. & Rouder, J. N. (2014). BayesFactor (Version 0.99)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16(2), 225–237.")

	fields <- list(
		list(name=".variable", title="", type="string", combine=TRUE))
	
	if (options$bayesFactorType == "BF01") {
	
		if (options$hypothesis == "groupsNotEqual"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u2081")
		}
		if (options$hypothesis == "groupOneGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u208A")
		}
		if (options$hypothesis == "groupTwoGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u208B")
		}
		
	} else {
	
		if (options$hypothesis == "groupsNotEqual"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2081\u2080")
		}
		if (options$hypothesis == "groupOneGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u208A\u2080")
		}
		if (options$hypothesis == "groupTwoGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u208B\u2080")
		}	
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
		
		gs <- base::levels(levels)
				g1 <- gs[1]
				g2 <- gs[2]		
		
		if (length(levels) != 2) {
		
			ttest[["error"]] <- list(errorType="badData", errorMessage="The Grouping Variable must have 2 levels")
			status <- "error"
			
		} else {
		
			rowNo <- 1
		
			for (variable in options[["variables"]]) {
				
								
				# BayesFactor package doesn't handle NAs, so it is necessary to exclude them
				
				subDataSet <- subset(dataset, select=c(.v(variable), .v(options$groupingVariable)))
				subDataSet <- na.omit(subDataSet)
				
				gs <- base::levels(levels)
				
				group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== gs[1],.v(variable)] 
				group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== gs[2],.v(variable)] 

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
					
					bf.raw <- exp(as.numeric(bf@bayesFactor$bf))
					BF    <- .clean(bf.raw)
					error <- .clean(as.numeric(bf@bayesFactor$error))
					errorMessage <- NULL
					
					if(bf.raw == Inf){
						
						if(options$plotPriorAndPosterior | options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness | options$plotBayesFactorRobustness){
					
							errorMessage <- "BayesFactor is infinity: plotting not possible"
							status <- "error"
						}
					}
					
					ind <- which(group1 == group1[1])
					idData <- sum((ind+1)-(1:(length(ind))) == 1)
					
					ind2 <- which(group2 == group2[1])
					idData2 <- sum((ind2+1)-(1:(length(ind2))) == 1)
					
					if(idData > 1 && idData2 > 1 && (options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness)){
					
						if(options$plotPriorAndPosterior | options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness | options$plotBayesFactorRobustness){
						
							errorMessage <- paste("Sequential Analysis not possible: The first observations are identical")
							index <- .addFootnote(footnotes, errorMessage)
							
							status <- "error"
						}
					}
										
					if(!is.null(errorMessage)){
					
						index <- .addFootnote(footnotes, errorMessage)
						list(.variable=variable, BF=BF, error=error, .footnotes=list(BF=list(index)))
					} else {
					
						list(.variable=variable, BF=BF, error=error)
					}
				})

				if (class(result) == "try-error") {
			
					errorMessage <- .extractErrorMessage(result)
					
					if (errorMessage == "Dependent variable must not contain missing or infinite values.") {
					
						errorMessage <- "BayesFactor is undefined - the dependent variable contains infinity"
						status <- "error"
						
					} else if (errorMessage == "grouping factor must have exactly 2 levels") {
					
						# We know that the grouping factor *does* have two levels, because we've checked this earlier on
						# This error means that all of one factor has been excluded because of missing values in the dependent
						
						errorMessage <- "BayesFactor is undefined - the grouping variable contains less than two levels once missing values in the dependent are excluded"
						status <- "error"
						
					} else if (errorMessage == "data are essentially constant") {
					
						errorMessage <- "BayesFactor is undefined - one or both levels of the dependent contain all the same value (the variance is zero)"
						status <- "error"
						
					} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
					
						errorMessage <- "BayesFactor is undefined - one or both levels of the dependent contain too few observations"
						status <- "error"
					}
										
					index <- .addFootnote(footnotes, errorMessage)

					result <- list(.variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
					
					status <- "error"
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
	
	list(ttest, status, g1, g2)

}