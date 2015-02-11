
TTestBayesianPairedSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	
	if(is.null(options()$BFMaxModels)) options(BFMaxModels = 50000)
	if(is.null(options()$BFpretestIterations)) options(BFpretestIterations = 100)
	if(is.null(options()$BFapproxOptimizer)) options(BFapproxOptimizer = "optim")
	if(is.null(options()$BFapproxLimits)) options(BFapproxLimits = c(-15,15))
	if(is.null(options()$BFprogress)) options(BFprogress = interactive())
	if(is.null(options()$BFfactorsMax)) options(BFfactorsMax = 5)

	all.variables <- unique(unlist(options$pairs))
	all.variables <- all.variables[all.variables != ""]

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
	
	meta[[1]] <- list(name="title", type="title")
	meta[[2]] <- list(name="ttest", type="table")
	meta[[3]] <- list(name="inequalityOfVariances", type="table")
	meta[[4]] <- list(name="descriptives", type="table")
	meta[[5]] <- list(name="plots", type="images")
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian T-Test"
	
	ttest <- list()

	ttest[["title"]] <- "Bayesian Paired Samples T-Test"
	
	ttest[["citation"]] <- list(
		"Morey, R. D. & Rouder, J. N. (2014). BayesFactor (Version 0.99)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16(2), 225–237.")
	
	bf.type <- options$bayesFactorType
	
	if (bf.type == "BF10") {
	
		BFH1H0 <- TRUE
	
		if (options$hypothesis == "groupsNotEqual") {
			bf.title <- "BF\u2081\u2080"
		}
		if (options$hypothesis == "groupOneGreater") {
			bf.title <- "BF\u208A\u2080"
		}
		if (options$hypothesis == "groupTwoGreater") {
			bf.title <- "BF\u208B\u2080"
		}
	} else {
	
		BFH1H0 <- FALSE
	
		if (options$hypothesis == "groupsNotEqual") {
			bf.title <- "BF\u2080\u2081"
		}
		if (options$hypothesis == "groupOneGreater") {
			bf.title <- "BF\u2080\u208A"
		}
		if (options$hypothesis == "groupTwoGreater") {
			bf.title <- "BF\u2080\u208B"
		}
	}
	
	if (options$hypothesis == "groupsNotEqual") {
		nullInterval <- NULL
		oneSided <- FALSE
	}
	if (options$hypothesis == "groupOneGreater") {
		nullInterval <- c(0, Inf)
		oneSided <- "right"
	}
	if (options$hypothesis == "groupTwoGreater") {
		nullInterval <- c(-Inf, 0)
		oneSided <- "left"
	}

	fields <- list(
		list(name=".variable1", type="string", title=""),
		list(name=".separator", type="separator", title=""),
		list(name=".variable2", type="string", title=""),
		list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
		list(name="error", type="number", format="sf:4;dp:3", title="error %"))

	ttest[["schema"]] <- list(fields=fields)

	ttest.rows <- list()
	plots.ttest <- list()
	
	footnotes <- .newFootnotes()
		
	for (pair in options$pairs)
	{
		if (options$plotPriorAndPosterior) {

			plot <- list()
			
			plot[["title"]] <- paste(pair, collapse=" - ")
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "waiting"
			
			image <- .beginSaveImage(530, 400)
			.plotPosterior.ttest(x=NULL, y=NULL, paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, addInformation=options$plotPriorAndPosteriorAdditionalInfo, dontPlotData=TRUE)
			plot[["data"]] <- .endSaveImage(image)
						
			plots.ttest[[length(plots.ttest)+1]] <- plot
		}
		
		if (options$plotBayesFactorRobustness) {

			plot <- list()
			
			plot[["title"]] <- paste(pair, collapse=" - ")
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "waiting"
						
			plots.ttest[[length(plots.ttest)+1]] <- plot
		}
		
		if (options$plotSequentialAnalysis || options$plotSequentialAnalysisRobustness){

			plot <- list()
			
			plot[["title"]] <- paste(pair, collapse=" - ")
			plot[["width"]]  <- 530
			plot[["height"]] <- 400
			plot[["status"]] <- "waiting"
						
			plots.ttest[[length(plots.ttest)+1]] <- plot
		}
	}	
	
	
	results[["plots"]] <- plots.ttest
	
	
	pair.statuses <- list()
	
	
	for (i in .indices(options$pairs))
	{
		pair <- options$pairs[[i]]
	
		if (pair[[1]] == "" || pair[[2]] == "") {
		
			p1 <- ifelse(pair[[1]] != "", pair[[1]], "...") 
			p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
		
			pair.statuses[[i]] <- list(ready=FALSE, error=FALSE, unplotable=TRUE)

			result <- list(.variable1=p1, .separator="-", .variable2=p2, BF="", error="")
		
		} else {

			if (perform == "init") {
			
				pair.statuses[[i]] <- list(ready=FALSE, error=FALSE, unplotable=TRUE)

				result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=".", error=".")		
			
			} else {
			
				unplotable <- FALSE
				unplotableMessage <- NULL

				result <- try (silent = TRUE, expr = {

					subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
					subDataSet <- na.omit(subDataSet)
			
					c1 <- subDataSet[[ .v(pair[[1]]) ]]
					c2 <- subDataSet[[ .v(pair[[2]]) ]]
	
					r <- BayesFactor::ttestBF(c1, c2, paired = TRUE, r=options$priorWidth, nullInterval= nullInterval)
					
					bf.raw <- exp(as.numeric(r@bayesFactor$bf))[1]

					if (is.infinite(bf.raw)) {
					
						unplotable <- TRUE
						unplotableMessage <- "Bayes factor is infinite"
					}
					
					if (is.infinite(1 / bf.raw)) {
					
						unplotable <- TRUE
						unplotableMessage <- "Bayes factor is one divided by infinity"
					}
					
					if (bf.type == "BF01")
						bf.raw <- 1 / bf.raw
			
					BF <- .clean(bf.raw)
					error <- .clean(as.numeric(r@bayesFactor$error))[1]
			
					list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=BF, error=error)
			
				})
		
				if (class(result) == "try-error") {
				
					errorMessage <- .extractErrorMessage(result)
					
					if (errorMessage == "x or y must not contain missing or infinite values.") {
				
						errorMessage <- paste("BayesFactor is undefined - one or both of the variables contain infinity")
					
					#} else if (errorMessage == "data are essentially constant") {
					#				
					#	errorMessage <- paste("BayesFactor is undefined - the sample contains all the same value (the variance is zero)")
					#
					} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
					
						errorMessage <- "BayesFactor is undefined - one or both of the variables has too few observations (possibly only after missing values are excluded)"	
					}

					pair.statuses[[i]] <- list(ready=FALSE, error=TRUE, errorMessage=errorMessage, unplotable=TRUE, unplotableMessage=errorMessage)

					index <- .addFootnote(footnotes, errorMessage)

					result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
					
				} else {
				
					pair.statuses[[i]] <- list(ready=TRUE, error=FALSE, unplotable=unplotable, unplotableMessage=unplotableMessage)
						
					result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=BF, error=error)
				}
			}
		}
		
		ttest.rows[[length(ttest.rows)+1]] <- result
	}
	
	if (length(ttest.rows) == 0)
		ttest.rows <- list(list(.variable1="...", .separator="-", .variable2="...", BF="", error=""))
	
	ttest[["data"]] <- ttest.rows
	ttest[["footnotes"]] <- as.list(footnotes)
	
	if (perform == "run")
		ttest[["status"]] <- "complete"
	
	results[["ttest"]] <- ttest
	
	if (options$descriptives) {
	
		descriptives <- list()

		descriptives[["title"]] <- "Descriptives"

		fields <- list(
			list(name=".variable", type="string", title=""),
			list(name="N", type="integer"),
			list(name="mean", type="number", format="sf:4"),
			list(name="sd", type="number", format="dp:4;p:.001"),
			list(name="SE", type="number", format="dp:4;p:.001"))

		descriptives[["schema"]] <- list(fields=fields)
		
		descriptives.results <- list()
		
		variables <- unlist(options$pairs)
		variables <- unique(variables)
		variables <- variables[variables != ""]
		
		for (variable in variables) {

			if (perform == "run") {

				result <- try (silent = TRUE, expr = {
				
					n <- .clean(as.numeric(length(dataset[[ .v(variable) ]])))
					m <- .clean(as.numeric(mean(dataset[[ .v(variable) ]], na.rm = TRUE)))
					std <- .clean(as.numeric(sd(dataset[[ .v(variable) ]], na.rm = TRUE)))
					if(is.numeric(std)){
						se <- .clean(as.numeric(std/sqrt(n)))}
					else
						se <- .clean(NaN)
								
					list(.variable=variable, N=n, mean=m, sd=std, SE=se)
				})
			
				if (class(result) == "try-error") {
			
					result <- list(.variable=variable, N="", mean="", sd="", SE="")
				}
				
			} else {
			
				result <- list(.variable=variable, N=".", mean=".", sd=".", SE=".")
			}
			
			descriptives.results[[length(descriptives.results)+1]] <- result
		}
		
		descriptives[["data"]] <- descriptives.results
		descriptives[["status"]] <- "complete"
		
		results[["descriptives"]] <- descriptives
	}
	
	
	# PLOTS
	
	if (length(options$pairs) > 0 && (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis)) {
	
		results[["plots"]][[1]][["status"]] <- "running"

		if (callback(results) != 0)
			return()
		
		n.plots.per.variable <- sum(options$plotPriorAndPosterior, options$plotBayesFactorRobustness, options$plotSequentialAnalysis)
		n.plots <- length(options$pairs) * n.plots.per.variable
		
		j <- 1
		
		for (i in .indices(options$pairs)) {
			
			pair <- options$pairs[[i]]
			
			status <- pair.statuses[[i]]
			
			p1 <- ifelse(pair[[1]] != "", pair[[1]], "...") 
			p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
			
			sequentialIsViable <- TRUE
			
			if (perform == "run" && status$unplotable == FALSE) {
				
				subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
				subDataSet <- na.omit(subDataSet)
				
				c1 <- subDataSet[[ .v(pair[[1]]) ]]
				c2 <- subDataSet[[ .v(pair[[2]]) ]]
				
				ind <- which(c1 == c1[1])
				idData <- sum((ind+1)-(1:(length(ind))) == 1)
				
				ind2 <- which(c2 == c2[1])
				idData2 <- sum((ind2+1)-(1:(length(ind2))) == 1)
			}
			else
			{
				c1 <- NULL
				c2 <- NULL
			}
		
			if (options$plotPriorAndPosterior) {
			
				plot <- plots.ttest[[j]]

				if (status$unplotable == FALSE) {
				
					image <- .beginSaveImage(530, 400)
					
					.plotPosterior.ttest(x=c1, y=c2, paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, addInformation=options$plotPriorAndPosteriorAdditionalInfo)
					
					plot[["data"]] <- .endSaveImage(image)
					
				} else if (status$unplotable && "unplotableMessage" %in% names(status)) {
				
					message <- paste("Plotting is not possible:", status$unplotableMessage)
					plot[["error"]] <- list(error="badData", errorMessage=message)
				}
				
				plot[["status"]] <- "complete"
				
				plots.ttest[[j]] <- plot
				
				if (j < n.plots) {
				
					plots.ttest[[j+1]]$status <- "running"
					results[["plots"]] <- plots.ttest

					if (callback(results) != 0)
						return()
					
				} else {
			
					results[["plots"]] <- plots.ttest
				}
				
				j <- j + 1
			}
		
			if (options$plotBayesFactorRobustness) {
				
				plot <- plots.ttest[[j]]

				if (status$unplotable == FALSE) {
				
					image <- .beginSaveImage(530, 400)
				
					.plotBF.robustnessCheck.ttest(x=c1, y=c2, paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, BFH1H0=BFH1H0)
				
					content <- .endSaveImage(image)
				
					plot[["data"]]  <- content
					
				} else if (status$unplotable && "unplotableMessage" %in% names(status)) {
				
					message <- paste("Plotting is not possible:", status$unplotableMessage)
					plot[["error"]] <- list(error="badData", errorMessage=message)
				}
				
				plot[["status"]] <- "complete"
				
				plots.ttest[[j]] <- plot
				
				if (j < n.plots) {
				
					plots.ttest[[j+1]]$status <- "running"
					results[["plots"]] <- plots.ttest
					
					if (callback(results) != 0)
						return()
					
				} else {
			
					results[["plots"]] <- plots.ttest
				}
				
				j <- j + 1
			}
		
			if (options$plotSequentialAnalysis || options$plotSequentialAnalysisRobustness) {
		
				plot <- plots.ttest[[j]]

				if (status$unplotable == FALSE && sequentialIsViable) {

					image <- .beginSaveImage(530, 400)

					.plotSequentialBF.ttest(x=c1, y=c2, paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, plotDifferentPriors=options$plotSequentialAnalysisRobustness, BFH1H0=BFH1H0)

					content <- .endSaveImage(image)
			
					plot[["data"]]  <- content
				}
				
				if (sequentialIsViable == FALSE) {
				
					plot[["error"]] <- list(error="badData", errorMessage="Sequential Analysis not possible: The first observations are identical")

				} else if (status$unplotable && "unplotableMessage" %in% names(status)) {
				
					message <- paste("Plotting is not possible:", status$unplotableMessage)
					plot[["error"]] <- list(error="badData", errorMessage=message)	
				}
				
				plot[["status"]] <- "complete"
			
				plots.ttest[[j]] <- plot
				
				if (j < n.plots) {
				
					plots.ttest[[j+1]]$status <- "running"
					results[["plots"]] <- plots.ttest
					
					if (callback(results) != 0)
						return()
					
				} else {
			
					results[["plots"]] <- plots.ttest
				}
		
				j <- j + 1
			}
		}
	}
	
	results[["ttest"]] <- ttest
	results[["plots"]] <- plots.ttest
	
	results
}

