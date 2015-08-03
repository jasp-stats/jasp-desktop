
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
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
	
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
		
	} else if (bf.type == "LogBF10") {
		
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "groupsNotEqual") {
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
		}
		if (options$hypothesis == "groupOneGreater") {
			bf.title <- "Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
		}
		if (options$hypothesis == "groupTwoGreater") {
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
		}
		
	} else if (bf.type == "BF01") {
	
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
	
	if (options$hypothesis == "groupsNotEqual") {
	
		fields <- list(
			list(name=".variable1", type="string", title=""),
			list(name=".separator", type="separator", title=""),
			list(name=".variable2", type="string", title=""),
			list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
			list(name="error", type="number", format="sf:4;dp:3", title="error %"))
	
	} else {
	
		fields <- list(
			list(name=".variable1", type="string", title=""),
			list(name=".separator", type="separator", title=""),
			list(name=".variable2", type="string", title=""),
			list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
			list(name="error", type="number", format="sf:4;dp:3;~", title="error %"))
	}

	ttest[["schema"]] <- list(fields=fields)

	ttest.rows <- list()
	plots.ttest <- list()
	plotTypes <- list()
	plotPairs <- list()
	tablePairs <- list()
	errorFootnotes <- rep("no", length(options$pairs))
	
	state <- .retrieveState()
	
	diff <- NULL
	
	if (!is.null(state)) {
	
		diff <- .diff(options, state$options)

	}
	
	
	footnotes <- .newFootnotes()
	
	
	for (pair in options$pairs)
	{
	
		currentPair <- paste(pair, collapse=" - ")
	
		if (options$plotPriorAndPosterior) {
		
		
			if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
				&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "posteriorPlotAddInfo")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
					
			} else if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
						&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && !options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# if the requested plot already exists use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "posteriorPlot")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
				
			} else {

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
			
			if (options$plotPriorAndPosteriorAdditionalInfo) {
			
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"
			
			} else {
			
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
			}
			
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
			
		}
		
		if (options$plotBayesFactorRobustness) {
		
		
			if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
				&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && "robustnessPlot" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "robustnessPlot")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
				
			} else {
			
				plot <- list()
				
				plot[["title"]] <- paste(pair, collapse=" - ")
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"
				
				image <- .beginSaveImage(530, 400)
				.plotBF.robustnessCheck.ttest (oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
				plot[["data"]] <- .endSaveImage(image)
				
				plots.ttest[[length(plots.ttest)+1]] <- plot
			}
			
			plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
		}
		
		if (options$plotSequentialAnalysis){
		
		
			if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
				&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "sequentialRobustnessPlot")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
				
			} else if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
						&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && !options$plotSequentialAnalysisRobustness  && "sequentialPlot" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# if the requested plot already exists use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "sequentialPlot")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
				
			} else {
				
				plot <- list()
				
				plot[["title"]] <- paste(pair, collapse=" - ")
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"
				
				image <- .beginSaveImage(530, 400)
				.plotSequentialBF.ttest(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
				plot[["data"]] <- .endSaveImage(image)
				
				plots.ttest[[length(plots.ttest)+1]] <- plot
			}
			
			if (options$plotSequentialAnalysisRobustness) {
			
				plotTypes[[length(plotTypes)+1]] <- "sequentialRobustnessPlot"
			
			} else {
			
				plotTypes[[length(plotTypes)+1]] <- "sequentialPlot"
			}
			
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
		}
	}	
	
	
	results[["plots"]] <- plots.ttest
	
	pair.statuses <- list()
	
	i <- 1
	
	BF10post <- numeric(length(options$pairs))
	
	
	for (i in .indices(options$pairs))
	{
		pair <- options$pairs[[i]]
		
		tablePairs[[length(tablePairs)+1]] <- paste(pair, collapse=" - ")
	
		if (pair[[1]] == "" || pair[[2]] == "") {
		
			p1 <- ifelse(pair[[1]] != "", pair[[1]], "...") 
			p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
		
			pair.statuses[[i]] <- list(ready=FALSE, error=FALSE, unplotable=TRUE)
			
			result <- list(.variable1=p1, .separator="-", .variable2=p2, BF="", error="")
		
		} else {
			
			if (perform == "init") {
			
				if (!is.null(state) && tablePairs[[i]] %in% state$tablePairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE)))) {
				
					stateIndex <- which(state$tablePairs == paste(pair, collapse=" - "))[1]
					
					pair.statuses[[i]] <- state$pairStatuses[[stateIndex]]
				
					if (state$errorFootnotes[stateIndex] == "no") {
				
						result <- state$results$ttest$data[[stateIndex]]
					
					} else {
					
						index2 <- .addFootnote(footnotes, state$errorFootnotes[stateIndex])
						
						errorFootnotes[i] <- state$errorFootnotes[stateIndex]
						
						result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=.clean(NaN), error="", .footnotes=list(BF=list(index2)))
					}
				
				} else {
			
					pair.statuses[[i]] <- list(ready=FALSE, error=FALSE, unplotable=TRUE)
	
					result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=".", error=".")
				}
			
			} else {
			
				unplotable <- FALSE
				unplotableMessage <- NULL
				
				
				if (!is.null(state) && tablePairs[[i]] %in% state$tablePairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE)))) {
				
					stateIndex <- which(state$tablePairs == paste(pair, collapse=" - "))[1]
				
					if (state$errorFootnotes[stateIndex] == "no") {
				
						result <- state$results$ttest$data[[stateIndex]]
					
					} else {
					
						index2 <- .addFootnote(footnotes, state$errorFootnotes[stateIndex])
						
						errorFootnotes[i] <- state$errorFootnotes[stateIndex]
						
						result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=.clean(NaN), error="", .footnotes=list(BF=list(index2)))
					}
					
					pair.statuses[[i]] <- state$pairStatuses[[stateIndex]]
					
					BF10post[i] <- state$BF10post[stateIndex]
				
				} else {
					
					result <- try (silent = TRUE, expr = {
	
						subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
						subDataSet <- na.omit(subDataSet)
				
						c1 <- subDataSet[[ .v(pair[[1]]) ]]
						c2 <- subDataSet[[ .v(pair[[2]]) ]]
				
						r <- BayesFactor::ttestBF(c1, c2, paired = TRUE, r=options$priorWidth)
						
						bf.raw <- exp(as.numeric(r@bayesFactor$bf))[1]
						
						# deltaHat <- mean(c1 - c2) / sd(c1 - c2)
						# 	
						# N <- length(c1)
						# df <- N - 1
						# sigmaStart <- 1 / N
						# 
						# if (sigmaStart < .01) 
						# 	sigmaStart <- .01
						
						if (oneSided == "right") {
							
							samples <- BayesFactor::ttestBF(c1, c2, paired=TRUE, posterior = TRUE, iterations = 10000, rscale= options$priorWidth)
							delta <- samples[, "delta"]
							
							if (is.infinite(bf.raw)) {
						
								if (mean(delta) > 0) {
						
									bf.raw <- Inf
									
								} else {
								
									bf.raw <- 1 / Inf
								}
						
							} else {
							
								bf.raw <- .oneSidedTtestBFRichard(c1, c2, paired=TRUE, oneSided="right", r=options$priorWidth)
								
								# parameters <- try(silent=TRUE, expr= optim(par = c(deltaHat, sigmaStart, df), fn=.likelihoodShiftedT, data= delta , method="BFGS")$par)
								# 
								# 
								# if (class(parameters) == "try-error") {
								# 
								# 	parameters <- try(silent=TRUE, expr= optim(par = c(deltaHat, sigmaStart, df), fn=.likelihoodShiftedT, data= delta , method="Nelder-Mead")$par)
								# }
								# 
								# bf.raw <- 2 * bf.raw * pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=FALSE)							
							}
						}
						
						if (oneSided == "left") {
							
							samples <- BayesFactor::ttestBF(c1, c2, paired=TRUE, posterior = TRUE, iterations = 10000, rscale= options$priorWidth)
							delta <- samples[, "delta"]
							
							if (is.infinite(bf.raw)) {
						
								if (mean(delta) < 0) {
						
									bf.raw <- Inf
									
								} else {
								
									bf.raw <- 1 / Inf
								}
								
							} else {
							
								bf.raw <- .oneSidedTtestBFRichard(c1, c2, paired=TRUE, oneSided="left", r=options$priorWidth)
							
								# parameters <- try(silent=TRUE, expr= optim(par = c(deltaHat, sigmaStart, df), fn=.likelihoodShiftedT, data= delta , method="BFGS")$par)
								# 
								# if (class(parameters) == "try-error") {
								# 
								# 	parameters <- try(silent=TRUE, expr= optim(par = c(deltaHat, sigmaStart, df), fn=.likelihoodShiftedT, data= delta , method="Nelder-Mead")$par)
								# }
								# 
								# bf.raw <- 2 * bf.raw * pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE)
							}
						}
						
						if (is.na(bf.raw)) {
					
							unplotable <- TRUE
							unplotableMessage <- "Bayes factor could not be calculated"
							
						} else if (is.infinite(bf.raw)) {
						
							unplotable <- TRUE
							unplotableMessage <- "Bayes factor is infinite"
							
						} else if (is.infinite(1 / bf.raw)) {
						
							unplotable <- TRUE
							unplotableMessage <- "The Bayes factor is too small"
						}
							
						
						if (bf.type == "BF01")
							bf.raw <- 1 / bf.raw
						
						BF10post[i] <- bf.raw
						BF <- .clean(bf.raw)
						
						if (options$bayesFactorType == "LogBF10") {
								
								BF <- log(BF10post[i])
								BF <- .clean(BF)
						}
						
						error <- .clean(as.numeric(r@bayesFactor$error))[1]
				
						list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=BF, error=error)
				
					})
					
					
					if (class(result) == "try-error") {
					
						errorMessage <- .extractErrorMessage(result)
						
						if (errorMessage == "x or y must not contain missing or infinite values.") {
					
							errorMessage <- paste("Bayes factor is undefined - one or both of the variables contain infinity")
						
						} else if (errorMessage == "data are essentially constant") {
										
							errorMessage <- paste("Bayes factor is undefined - the sample contains all the same value (zero variance)")
						
						} else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
						
							errorMessage <- "Bayes factor is undefined - one or both of the variables has too few observations (possibly only after missing values are excluded)"	
						}
	
						pair.statuses[[i]] <- list(ready=FALSE, error=TRUE, errorMessage=errorMessage, unplotable=TRUE, unplotableMessage=errorMessage)
	
						index <- .addFootnote(footnotes, errorMessage)
						
						errorFootnotes[i] <- errorMessage
	
						result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
						
					} else {
					
						pair.statuses[[i]] <- list(ready=TRUE, error=FALSE, unplotable=unplotable, unplotableMessage=unplotableMessage)
							
						result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], BF=BF, error=error)
					}
				}
			}
		}
		
		ttest.rows[[length(ttest.rows)+1]] <- result
		i <- i + 1
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
			list(name="v", type="string", title=""),
			list(name="N",                  type="integer"),
			list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
			list(name="sd",   title="SD",   type="number", format="sf:4;dp:3"),
			list(name="se",   title="SE",   type="number", format="sf:4;dp:3"))

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
					
					list(v=variable, N=n, mean=m, sd=std, se=se)
				})
			
				if (class(result) == "try-error") {
			
					result <- list(v=variable, N="", mean="", sd="", se="")
				}
				
			} else {
			
				result <- list(v=variable, N=".", mean=".", sd=".", se=".")
			}
			
			descriptives.results[[length(descriptives.results)+1]] <- result
		}
		
		descriptives[["data"]] <- descriptives.results
		descriptives[["status"]] <- "complete"
		
		results[["descriptives"]] <- descriptives
	}
	
	
	# PLOTS
	
	if (perform == "run" && length(options$pairs) > 0 && (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis)) {
		
		if ( ! .shouldContinue(callback(results)))
			return()
		
		n.plots.per.variable <- sum(options$plotPriorAndPosterior, options$plotBayesFactorRobustness, options$plotSequentialAnalysis)
		n.plots <- length(options$pairs) * n.plots.per.variable
		
		j <- 1
		i <- 1
		
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
			
			
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "posteriorPlotAddInfo")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
						
				} else if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
							&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && !options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# if the requested plot already exists use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "posteriorPlot")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
					
				} else {
			
					plots.ttest[[j]]$status <- "running"
					results[["plots"]] <- plots.ttest
					
					if ( ! .shouldContinue(callback(results)))
						return()
				
					plot <- plots.ttest[[j]]
					
					
					if (status$unplotable == FALSE) {
					
					
						p <- try(silent= FALSE, expr= {
						
							image <- .beginSaveImage(530, 400)
						
							.plotPosterior.ttest(x=c1, y=c2, paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, addInformation=options$plotPriorAndPosteriorAdditionalInfo, BF=BF10post[i], BFH1H0=BFH1H0)
						
							plot[["data"]] <- .endSaveImage(image)
							})
							
							
						if (class(p) == "try-error") {
						
							errorMessage <- .extractErrorMessage(p)
							
							if (errorMessage == "not enough data") {
							
									errorMessage <- "Plotting is not possible: The Bayes factor is too small"
							} else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
							
								errorMessage <- "Plotting is not possible: The Bayes factor is too small"
							}
							
							plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
						}
						
					} else if (status$unplotable && "unplotableMessage" %in% names(status)) {
					
						message <- paste("Plotting is not possible:", status$unplotableMessage)
						plot[["error"]] <- list(error="badData", errorMessage=message)
					}
					
					plot[["status"]] <- "complete"
					
					plots.ttest[[j]] <- plot
				}
				
				results[["plots"]] <- plots.ttest
				
				if ( ! .shouldContinue(callback(results)))
						return()
				
				j <- j + 1
			}
		
			if (options$plotBayesFactorRobustness) {
			
			
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && "robustnessPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "robustnessPlot")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
						
				} else {
				
					plots.ttest[[j]]$status <- "running"
					results[["plots"]] <- plots.ttest
					
					if ( ! .shouldContinue(callback(results)))
						return()
					
					plot <- plots.ttest[[j]]
	
					if (status$unplotable == FALSE) {
					
						image <- .beginSaveImage(530, 400)
					
						.plotBF.robustnessCheck.ttest(x=c1, y=c2, BF10post=BF10post[i], paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, BFH1H0=BFH1H0)
					
						content <- .endSaveImage(image)
					
						plot[["data"]]  <- content
						
					} else if (status$unplotable && "unplotableMessage" %in% names(status)) {
					
						message <- paste("Plotting is not possible:", status$unplotableMessage)
						plot[["error"]] <- list(error="badData", errorMessage=message)
					}
					
					plot[["status"]] <- "complete"
					
					plots.ttest[[j]] <- plot
				}
				
				results[["plots"]] <- plots.ttest
				
				if ( ! .shouldContinue(callback(results)))
						return()
				
				j <- j + 1
			}
		
			if (options$plotSequentialAnalysis) {
			
			
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "sequentialRobustnessPlot")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
						
				} else if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
							&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE))) && !options$plotSequentialAnalysisRobustness  && "sequentialPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# if the requested plot already exists use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "sequentialPlot")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
					
				} else {
		
					plots.ttest[[j]]$status <- "running"
					results[["plots"]] <- plots.ttest
					
					if ( ! .shouldContinue(callback(results)))
						return()
					
					plot <- plots.ttest[[j]]
	
					if (status$unplotable == FALSE && sequentialIsViable) {
	
						image <- .beginSaveImage(530, 400)
	
						.plotSequentialBF.ttest(x=c1, y=c2, paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, BF10post=BF10post[i], plotDifferentPriors=options$plotSequentialAnalysisRobustness, BFH1H0=BFH1H0)
	
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
				}
				
				results[["plots"]] <- plots.ttest
				
				if ( ! .shouldContinue(callback(results)))
						return()
				
				j <- j + 1
			}
		}
	}
	
	results[["ttest"]] <- ttest
	results[["plots"]] <- plots.ttest
	
	keep <- NULL
	
	for (plot in plots.ttest)
		keep <- c(keep, plot$data)
	
	if (perform == "init") {
		
		return(list(results=results, status="inited", state=state, keep=keep))
		
	} else {
	
		return(list(results=results, status="complete", state=list(options=options, results=results, plotsTtest=plots.ttest, plotTypes=plotTypes, plotPairs=plotPairs,
		pairStatuses=pair.statuses, BF10post=BF10post, tablePairs=tablePairs, errorFootnotes=errorFootnotes), keep=keep))
	}
}

