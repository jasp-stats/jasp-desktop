#
# Copyright (C) 2015 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

BinomialTestBayesian <- function(dataset = NULL, options, perform = "run",
						   callback = function(...) 0,  ...) {

	variables <- unlist(options$variables)
	
	
	if (is.null(dataset)) {
		
		if (perform == "run") {
			
			dataset <- .readDataSetToEnd(columns.as.numeric=NULL, columns.as.factor=variables, exclude.na.listwise=NULL)
		
		} else {
			
			dataset <- .readDataSetHeader(columns.as.numeric=NULL, columns.as.factor=variables)
		}
	
	} else {
		
		dataset <- .vdf(dataset, columns.as.numeric=NULL, columns.as.factor=variables)
	}
	
	results <- list()
	
	results[["title"]] <- "Bayesian Binomial Test"
	
	meta <- list(list(name="binomial", type="table"))
	meta[[2]] <- list(name="plots", type="collection", meta=list(name="plotGroups", type="object",
																 meta=list(list(name="PriorPosteriorPlot", type="image"),
																		   list(name="SequentialAnalysisPlot", type="image"))))
	results[[".meta"]] <- meta
	
	table <- list()
	
	table[["title"]] <- "Bayesian Binomial Test"
	table[["citation"]] <- list("Jeffreys, H. (1961). Theory of Probability. Oxford, Oxford University Press.",
								"O’Hagan, A., & Forster, J. (2004). Kendall’s advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.). London: Arnold.",
								"Haldane, J. B. S. (1932). A note on inverse probability. Mathematical Proceedings of the Cambridge Philosophical Society, 28, 55-61.")
	
	if (options$bayesFactorType == "BF01") {
		
		BFH1H0 <- FALSE
		
		if (options$hypothesis == "notEqualToTestValue"){
			bf.title <- "BF\u2080\u2081"
		} else if (options$hypothesis == "greaterThanTestValue"){
			bf.title <- "BF\u2080\u208A"
		} else if (options$hypothesis == "lessThanTestValue"){
			bf.title <-  "BF\u2080\u208B"
		}
		
	} else if (options$bayesFactorType == "BF10") {
		
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "notEqualToTestValue"){
			bf.title <- "BF\u2081\u2080"
		} else if (options$hypothesis == "greaterThanTestValue"){
			bf.title <- "BF\u208A\u2080"
		} else if (options$hypothesis == "lessThanTestValue"){
			bf.title <- "BF\u208B\u2080"
		}
		
	} else if (options$bayesFactorType == "LogBF10") {
		
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "notEqualToTestValue"){
			bf.title <- "Log(\u0042\u0046\u2081\u2080)"
		} else if (options$hypothesis == "greaterThanTestValue"){
			bf.title <-"Log(\u0042\u0046\u208A\u2080)"
		} else if (options$hypothesis == "lessThanTestValue"){
			bf.title <- "Log(\u0042\u0046\u208B\u2080)"
		}
	}
	
	
	footnotes <- .newFootnotes()
	
	if (options$hypothesis == "notEqualToTestValue") {
		
		hyp <- "two.sided"
		message <- paste0("Proportions tested against value: ", options$testValue, ".")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		
	} else if (options$hypothesis == "greaterThanTestValue") {
		
		hyp <- "greater"
		note <- "For all tests, the alternative hypothesis specifies that the proportion
					is greater than "
		message <- paste0(note, options$testValue, ".")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		
	} else {
		
		hyp <- "less"
		note <- "For all tests, the alternative hypothesis specifies that the proportion
					is less than "
		message <- paste0(note, options$testValue, ".")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		
	}
	
	
	schema <- list(fields=list(
		list(name="case", title="", type="string", combine=TRUE),
		list(name="level", title="Level", type="string"),
		list(name="counts", title="Counts", type="integer"),
		list(name="total", title="Total", type="integer"),
		list(name="proportion", title="Proportion", type="number", format="sf:4;dp:3"),
		list(name="BF", title=bf.title, type="number", format="sf:4;dp:3", title = bf.title)
		))
	
	table[["schema"]] <- schema
	
	# if state, retrieve and check differences
	
	state <- .retrieveState()
	diff <- NULL
	
	if (!is.null(state))
		diff <- .diff(options, state$options)
	
	data <- list()
	plotGroups <- list()
	plotsBinomtest <- list()
	rowsBinomtest <- list()
	rowsBF10 <- numeric()
	plotIdentifier <- character()
	rowIdentifier <- character()
	
	# beta distribution parameters
	a <- options$priorA
	b <- options$priorB
	
	errorMessageTable <- NULL
	
	if (options$testValue == 1 && hyp == "greater") {
	
		errorMessageTable <- "Cannot test the hypothesis that the test value is greater than 1."
	
	} else if (options$testValue == 0 && hyp == "less") {
	
		errorMessageTable <- "Cannot test the hypothesis that the test value is less than 0."
	}
	
	if (perform == "run" && !is.null(variables)) {
	
		i <- 1 # plotGroups index
		j <- 1 # plot index
		r <- 1 # rowIdentifier index
		
		for (var in variables) {
			
			d <- dataset[[.v(var)]]
			d <- d[!is.na(d)]
			
			levels <- levels(d)
			n <- length(d)
			
			for (lev in levels) {
				
				rowIdentifier[r] <- paste(c(var, lev), collapse = " - ")
				counts <- sum(d == lev)
				prop <- counts/n
				
				if (!is.null(state) && rowIdentifier[r] %in% state$rowIdentifier && ! diff$testValue && ! diff$hypothesis && ! diff$priorA &&
					! diff$priorB && ! diff$bayesFactorType) {
						
						index <- which(state$rowIdentifier == rowIdentifier[r])
						row <- state$rowsBinomtest[[index]]
						BF10 <- state$rowsBF10[index]
						rowsBF10[r] <- BF10
						
				} else {
					
					BF10 <- .bayesBinomialTest(counts, n, options$testValue, hypothesis = hyp, a = a, b = b)
					rowsBF10[r] <- BF10
					
					# TODO: If try-error extract error message and display
					
					BF <- BF10
					
					if (options$bayesFactorType == "BF01") {
						BF <- 1/BF10
					} else if(options$bayesFactorType == "LogBF10") {
						BF <- log(BF10)
					}
					
					row <- list(case=var, level=lev, counts=.clean(counts), total=.clean(n), proportion=.clean(prop), BF=.clean(BF))
					
					if (lev == levels[1]) {
						row[[".isNewGroup"]] <- TRUE
					} else {
						row[[".isNewGroup"]] <- FALSE
					}
				}
				
				data[[length(data) + 1]] <- row
				rowsBinomtest[[r]] <- row
				r <- r + 1
				
				####################################################
				###               PLOTS run phase                ###
				####################################################
				
				
				if (options$plotPriorAndPosterior || options$plotSequentialAnalysis) {
			
					plotGroups[[i]] <- list()
					plotGroups[[i]][["title"]] <- paste(c(var, lev), collapse = " - ")
					plotGroups[[i]][["name"]] <- plotGroups[[i]][["title"]]
					
				}
				
				if (options$plotPriorAndPosterior && is.null(errorMessageTable)) {
					
					plotType <- ifelse(options$plotPriorAndPosteriorAdditionalInfo, "PriorPosteriorAddInfoPlot:", "PriorPosteriorPlot:")
					plotIdentifier[j] <- paste0(plotType, plotGroups[[i]][["name"]])
					
					
					if (!is.null(state) && plotIdentifier[j] %in% state$plotIdentifier && ! diff$testValue && ! diff$hypothesis && ! diff$priorA && ! diff$priorB) {
						
						index <- which(state$plotIdentifier == plotIdentifier[j])
						plot <- state$plotsBinomtest[[index]]
					
					} else {
						
						plot <- list()
					
						plot[["title"]] <- "Prior and Posterior"
						plot[["width"]]  <- 530
						plot[["height"]] <- 400
					
						p <- try(silent=FALSE, expr= {
							
									func <- function() { 
										 .plotPosterior.binomTest(counts, n, options$testValue, a = a, b = b, BF10, hypothesis = hyp,
 											addInformation = options$plotPriorAndPosteriorAdditionalInfo)
									}
									imgObj <- .writeImage(530, 400, func)
									
									plot[["data"]] <- imgObj[["png"]]
									plot[["obj"]] <- imgObj[["obj"]]
									plot[["convertible"]] <- TRUE
									plot[["status"]] <- "complete"
									
								})
								
						if (class(p) == "try-error") {
						
							errorMessage <- .extractErrorMessage(p)
							plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
						}
						
					}
					
					plotGroups[[i]][["PriorPosteriorPlot"]] <- plot
					plotsBinomtest[[j]] <- plot
					j <- j + 1
					
				}
				
				if (options$plotSequentialAnalysis && is.null(errorMessageTable)) {
					
					plotIdentifier[j] <- paste("SequentialAnalysisPlot:", plotGroups[[i]][["name"]])
					
					if (!is.null(state) && plotIdentifier[j] %in% state$plotIdentifier && ! diff$testValue && ! diff$hypothesis && ! diff$bayesFactorType && ! diff$priorA && ! diff$priorB) {
						
						index <- which(state$plotIdentifier == plotIdentifier[j])
						plot <- state$plotsBinomtest[[index]]
					
					} else {
						
						plot <- list()
						
						plot[["title"]] <- "Sequential Analysis"
						plot[["width"]]  <- 530
						plot[["height"]] <- 400
						
						p <- try(silent=FALSE, expr= {
									# image <- .beginSaveImage(530, 400)
									# .plotSequentialBF.binomTest(d, lev, options$testValue, a = a, b = b, BF10table = BF10, hypothesis = hyp, BFH1H0 = BFH1H0)
									# plot[["data"]] <- .endSaveImage(image)
						    .plotFunc <- function() {
						        .plotSequentialBF.binomTest(d, lev, options$testValue, a = a, b = b, BF10table = BF10, hypothesis = hyp, BFH1H0 = BFH1H0)
						    }
						    content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
						    plot[["convertible"]] <- TRUE
						    plot[["obj"]] <- content[["obj"]]
						    plot[["data"]] <- content[["png"]]
						})
								
						if (isTryError(p)) {
						    errorMessage <- .extractErrorMessage(p)
							plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
						}
						
					}
					
					plotGroups[[i]][["SequentialAnalysisPlot"]] <- plot
					plotsBinomtest[[j]] <- plot
					j <- j + 1
					
				}
				
				i <- i + 1
				
			}
		}
		
	} else {
		
		
		if (is.null(variables)) {
			
			data[[length(data) + 1]] <- list(case=".", level=".", counts=".", total=".",  proportion=".", p=".")
			variables <- ""
		}
	
		for (var in variables) {
		
			d <- dataset[[.v(var)]]
			levels <- levels(d)
			
			for (lev in levels) {
				
				rowIdentifier[length(rowIdentifier) + 1] <- paste(c(var, lev), collapse = " - ")
				
				if (!is.null(state) && rowIdentifier[length(rowIdentifier)] %in% state$rowIdentifier && ! diff$testValue && ! diff$hypothesis && ! diff$priorA &&
					! diff$priorB && ! diff$bayesFactorType) {
						
						index <- which(state$rowIdentifier == rowIdentifier[length(rowIdentifier)])
						data[[length(data) + 1]] <- state$rowsBinomtest[[index]]
						rowsBinomtest[[length(rowsBinomtest) + 1]] <- state$rowsBinomtest[[index]]
						
				} else {
					
					data[[length(data) + 1]] <- list(case=var, level=lev, counts=".", total=".",  proportion=".", p=".")
					rowsBinomtest[[length(rowsBinomtest) + 1]] <- list(case=var, level=lev, counts=".", total=".",  proportion=".", p=".")
				}
				
				####################################################
				###               PLOTS init phase               ###
				####################################################
				
				
				if (options$plotPriorAndPosterior || options$plotSequentialAnalysis) {
			
					plotGroups[[length(plotGroups) + 1]] <- list()
					plotGroups[[length(plotGroups)]][["title"]] <- paste(c(var, lev), collapse = " - ")
					plotGroups[[length(plotGroups)]][["name"]] <- plotGroups[[length(plotGroups)]][["title"]]
					
				}
				
				if (options$plotPriorAndPosterior && is.null(errorMessageTable)) {
					
					plotType <- ifelse(options$plotPriorAndPosteriorAdditionalInfo, "PriorPosteriorAddInfoPlot:", "PriorPosteriorPlot:")
					plotIdentifier[length(plotIdentifier) + 1] <- paste0(plotType, plotGroups[[length(plotGroups)]][["name"]])
					
					if (!is.null(state) && plotIdentifier[length(plotIdentifier)] %in% state$plotIdentifier && ! diff$testValue && ! diff$hypothesis && ! diff$priorA && ! diff$priorB) {
						
						index <- which(state$plotIdentifier == plotIdentifier[length(plotIdentifier)])
						plot <- state$plotsBinomtest[[index]]
						
					} else {
						
						plot <- list()
						
						plot[["title"]] <- "Prior and Posterior"
						plot[["width"]]  <- 530
						plot[["height"]] <- 400
						
						func <- function() {
							.plotPosterior.binomTest(dontPlotData = TRUE, addInformation = options$plotPriorAndPosteriorAdditionalInfo)
						}
						imgObj <- .writeImage(530, 400, func)
						
						plot[["data"]] <- imgObj[["png"]]
						plot[["obj"]] <- imgObj[["obj"]]
						plot[["convertible"]] <- TRUE
						plot[["status"]] <- "complete"
					}
					
					plotGroups[[length(plotGroups)]][["PriorPosteriorPlot"]] <- plot
					plotsBinomtest[[length(plotsBinomtest) + 1]] <- plot
					
				}
				
				if (options$plotSequentialAnalysis && is.null(errorMessageTable)) {
					
					plotIdentifier[length(plotIdentifier) + 1] <- paste("SequentialAnalysisPlot:", plotGroups[[length(plotGroups)]][["name"]])
					
					if (!is.null(state) && plotIdentifier[length(plotIdentifier)] %in% state$plotIdentifier && ! diff$testValue && ! diff$hypothesis && ! diff$bayesFactorType &&
						! diff$priorA && ! diff$priorB) {
						
						index <- which(state$plotIdentifier == plotIdentifier[length(plotIdentifier)])
						plot <- state$plotsBinomtest[[index]]
					
					} else {
						
						plot <- list()
						
						plot[["title"]] <- "Sequential Analysis"
						plot[["width"]]  <- 530
						plot[["height"]] <- 400
						
						.plotFunc <- function() {
								.plotSequentialBF.binomTest(dontPlotData = TRUE, hypothesis = hyp, BFH1H0 = BFH1H0)
						}
						content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
						
						plot[["convertible"]] <- TRUE
						plot[["obj"]] <- content[["obj"]]
						plot[["data"]] <- content[["png"]]
						
					}
					
					plotGroups[[length(plotGroups)]][["SequentialAnalysisPlot"]] <- plot
					plotsBinomtest[[length(plotsBinomtest) + 1]] <- plot
					
				}
			}
		}
	}
	
	table[["data"]] <- data
	
	
	if ( ! is.null(errorMessageTable))
		table[["error"]] <- list(errorType = "badData", errorMessage = errorMessageTable)
	
	table[["footnotes"]] <- as.list(footnotes)
	
	results[["binomial"]] <- table
	
	if (options$plotPriorAndPosterior || options$plotSequentialAnalysis)
		results[["plots"]] <- list(title=ifelse(length(plotGroups) == 1 && length(plotGroups[[1]]) == 3, "Plot", "Plots"), collection=plotGroups)
	
	keep <- NULL
	
	for (plot in plotsBinomtest)
		keep <- c(keep, plot$data)
	
	if (perform == "init") {
		
		return(list(results=results, status="inited", state=state, keep=keep))
		
	} else {
	
		return(list(results=results, status="complete", state=list(options=options, results=results, plotsBinomtest=plotsBinomtest,
					plotIdentifier=plotIdentifier, rowsBinomtest=rowsBinomtest, rowIdentifier=rowIdentifier, rowsBF10=rowsBF10), keep=keep))
	}
} 

#.bayesBinomialTest.twoSided.jeffreys <- function(counts, n, theta0) {
#	# assuming a = b = 1, i.e., uniform prior
#	
#	logBF01 <- lgamma(n + 2) - lgamma(counts + 1) - lgamma(n - counts + 1) + counts*log(theta0) + (n - counts)*log(1 - theta0)
#	BF10 <- 1 / exp(logBF01)
#	
#	return(BF10)
#	
#}

.bayesBinomialTest.twoSided <- function(counts, n, theta0, a, b) {
	
	if (theta0 == 0 && counts == 0) {
	
		# in this case, counts*log(theta0) should be zero, omit to avoid numerical issue with log(0)
		
		logBF10 <- lbeta(counts + a, n - counts + b) -  lbeta(a, b) - (n - counts)*log(1 - theta0)
		
	} else if (theta0 == 1 && counts == n) {
	
		# in this case, (n - counts)*log(1 - theta0) should be zero, omit to avoid numerical issue with log(0)
		
		logBF10 <- lbeta(counts + a, n - counts + b) -  lbeta(a, b) - counts*log(theta0) 
		
	} else {
	
		logBF10 <- lbeta(counts + a, n - counts + b) -  lbeta(a, b) - counts*log(theta0) - (n - counts)*log(1 - theta0)
	}
	
	BF10 <- exp(logBF10)
	
	return(BF10)
	
}

.bayesBinomialTest.oneSided <- function(counts, n, theta0, a, b, hypothesis) {
  
  if (hypothesis == "less") {
    
    lowerTail <- TRUE
    
  } else if (hypothesis == "greater") {
    
    lowerTail <- FALSE
    
  }
  
  if (theta0 == 0 && counts == 0) {
    
    # in this case, counts*log(theta0) should be zero, omit to avoid numerical issue with log(0)
    logMLikelihoodH0 <- (n - counts)*log(1 - theta0)
    
  } else if (theta0 == 1 && counts == n) {
    
    # in this case, (n - counts)*log(1 - theta0) should be zero, omit to avoid numerical issue with log(0)
    logMLikelihoodH0 <- counts*log(theta0)

  } else {
  
  logMLikelihoodH0 <- counts*log(theta0) + (n - counts)*log(1 - theta0)
  
  }
  
  term1 <- pbeta(theta0, a + counts, b + n - counts, lower.tail = lowerTail, log.p = TRUE) +
    lbeta(a + counts, b + n - counts)
  term2 <- lbeta(a,b) + pbeta(theta0, a, b, lower.tail = lowerTail, log.p = TRUE)
  logMLikelihoodH1 <- term1 - term2
  BF10 <- exp(logMLikelihoodH1 - logMLikelihoodH0)
  
  return(BF10)
  
}

.bayesBinomialTest <- function(counts, n, theta0, hypothesis, a, b) {
	
	if (hypothesis == "two.sided") {
		
		BF10 <- try(.bayesBinomialTest.twoSided(counts, n, theta0, a, b), silent = TRUE)
		
	} else {
		
		#if (theta0 == 0 || theta0 == 1) {
		#	
		#	BF10 <- NA
		#	
		#} else {
			
			BF10 <- try(.bayesBinomialTest.oneSided(counts, n, theta0, a, b, hypothesis), silent = TRUE)
			
		#}
	}
	
	if (class(BF10) == "try-error")
		BF10 <- NA
	
	return(BF10)
	
}

.dpriorTheta <- function(x, a = 1, b = 1, hypothesis = "two.sided", theta0 = .5) {
	
	if (hypothesis == "two.sided") {
		
		dbeta(x, a, b)
		
	} else if (hypothesis == "greater") {
		
		ifelse (x >= theta0,
				dbeta(x, a, b) / pbeta(theta0, a, b, lower.tail = FALSE),
				0)
		
	} else if (hypothesis == "less") {
		
		ifelse (x <= theta0,
				dbeta(x, a, b) / pbeta(theta0, a, b),
				0)
	
	}
	
}

.dposteriorTheta <- function(x, a = 1, b = 1, counts = 10, n = 20, hypothesis = "two.sided", theta0 = .5) {
	
	if (hypothesis == "two.sided") {
		
		dbeta(x, a + counts, b + n - counts)
		
	} else if (hypothesis == "greater") {
		
		ifelse (x >= theta0,
				dbeta(x, a + counts, b + n - counts) / pbeta(theta0, a + counts, b + n - counts, lower.tail = FALSE),
				0)
		
	} else if (hypothesis == "less") {
		
		ifelse (x <= theta0,
				dbeta(x, a + counts, b + n - counts) / pbeta(theta0, a + counts, b + n - counts),
				0)
	
	}
	
}

.credibleIntervalPlusMedian <- function(credibleIntervalInterval = .95, a = 1, b = 1, counts = 10, n = 20, hypothesis = "two.sided", theta0 = .5) {
	
	lower <- (1 - credibleIntervalInterval) / 2
	upper <- 1 - lower
	
	if (hypothesis == "two.sided") {
		
		quantiles <- qbeta(c(lower, .5, upper), a + counts , b + n - counts)
		
	} else if (hypothesis == "greater") {
		
		rightArea <- pbeta(theta0, a + counts , b + n - counts, lower.tail = FALSE)
		leftArea <- 1 - rightArea
		quantiles <- qbeta(leftArea + rightArea * c(lower, .5, upper), a + counts , b + n - counts)
		
	} else if (hypothesis == "less") {
		
		leftArea <- pbeta(theta0, a + counts , b + n - counts)
		quantiles <- qbeta(leftArea * c(lower, .5, upper), a + counts , b + n - counts)
		
	}
	
	return(list(ci.lower = quantiles[1], ci.median = quantiles[2], ci.upper = quantiles[3]))
	
}

.plotPosterior.binomTest <- function(counts, n, theta0, a = 1, b = 1, BF10, hypothesis = "two.sided",
									 addInformation = TRUE, dontPlotData = FALSE,
									 lwd = 2, cexPoints = 1.5, cexAxis = 1.2, cexYlab = 1.5,
									 cexXlab = 1.5, cexTextBF = 1.4, cexCI = 1.1, cexLegend = 1.2,
									 lwdAxis = 1.2, credibleIntervalInterval = .95) {
	
	
	if (addInformation) {
	
		par(mar = c(5.6, 5, 7, 4) + 0.1, las = 1)
		drawCI <- TRUE
	
	} else {
	
		par(mar = c(5.6, 5, 4, 4) + 0.1, las = 1)
		drawCI <- FALSE
	}
	
	
	if (dontPlotData) {
	
		plot(1, type = "n", xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, xlab = "", ylab = "")
		
		axis(1, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, xlab = "")
		axis(2, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, ylab = "")
		
		mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 3.25)
		mtext(expression("Population proportion" ~ theta), side = 1, cex = cexXlab, line = 2.6)
	
		return()
	}
	
	if (is.infinite(BF10))
		stop("Bayes factor is infinite")
	
	if (is.infinite(1/BF10))
		stop("Bayes factor is too small")
	
	# set limits plot
	
	xlim <- c(0, 1)
	
	if (hypothesis == "two.sided") {
	
		stretch <- 1.35
		
	} else if (hypothesis == "greater") {
	
		stretch <- 1.45
		
	} else if (hypothesis == "less") {
	
		stretch <- 1.45
	}
	
	# calculate position of "nice" tick marks and create labels
	
	xticks <- seq(0, 1, 0.2)
	xlabels <- c("0", "0.2", "0.4", "0.6", "0.8", "1")
	
	# compute 95% credible interval & median:
	
	quantiles <- try(.credibleIntervalPlusMedian(credibleIntervalInterval = credibleIntervalInterval,
					 a = a, b = b, counts = counts, n = n, hypothesis, theta0 = theta0),
					 silent = TRUE)
	
	if (class(quantiles) == "try-error") {
		
		drawCI <- FALSE
		
	} else {
		
		CIlow <- quantiles$ci.lower
		medianPosterior <- quantiles$ci.median
		CIhigh <- quantiles$ci.upper
	}
	
	if (a == 1 && b == 1) {
		
		theta <- seq(0, 1, length.out = 1000)
		
	} else {
		
		theta <- seq(0.001, 0.999, length.out = 1000)
	}
	
	priorLine <- .dpriorTheta(theta, a, b, hypothesis, theta0)
	posteriorLine <- .dposteriorTheta(theta, a, b, counts, n, hypothesis, theta0)
	
	dmax <- max(c(posteriorLine[is.finite(posteriorLine)], priorLine[is.finite(priorLine)]))
	
	ylim <- vector("numeric", 2)
	ylim[1] <- 0
	ylim[2] <- stretch * dmax
	
	yticks <- pretty(ylim)
	
	ylim <- range(yticks)
	ylabels <- formatC(yticks, 1, format = "f")
	
	
	plot(1, xlim = xlim, ylim = range(yticks), ylab = "", xlab = "", type = "n", axes = FALSE)
	
	lines(theta, posteriorLine, lwd = lwd)
	lines(theta, priorLine, lwd = lwd, lty = 3)
	
	axis(1, at = xticks, labels = xlabels, cex.axis = cexAxis, lwd = lwdAxis)
	axis(2, at = yticks, labels = ylabels, cex.axis = cexAxis, lwd = lwdAxis)
	
	
	if (nchar(ylabels[length(ylabels)]) > 4) {
		
		mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 4)
		
	} else if (nchar(ylabels[length(ylabels)]) == 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line = 3.25)
		
	} else if (nchar(ylabels[length(ylabels)]) < 4) {
		
		mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 2.85)
	}
	
	mtext(expression("Population proportion" ~ theta), side = 1, cex = cexXlab, line = 2.6)
	
	
	evalPosterior <- posteriorLine[posteriorLine > 0]
	
	if (theta0 == 0) {
		
		heightPriorTheta0 <- priorLine[1]
		heightPosteriorTheta0 <- posteriorLine[1]
		
		points(theta[1], heightPriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
		points(theta[1], heightPosteriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
	
	} else if (theta0 == 1) {
		
		heightPriorTheta0 <- priorLine[length(priorLine)]
		heightPosteriorTheta0 <- posteriorLine[length(posteriorLine)]
		
		points(theta[length(theta)], heightPriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
		points(theta[length(theta)], heightPosteriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
	
	} else {
	
		heightPriorTheta0 <- .dpriorTheta(theta0, a, b, hypothesis, theta0)
		heightPosteriorTheta0 <- .dposteriorTheta(theta0, a, b, counts, n, hypothesis, theta0)
		
		points(theta0, heightPriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
		points(theta0, heightPosteriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
	}
	
	par(xpd = TRUE) # enable plotting in margin
	
	yCI <- grconvertY(dmax, "user", "ndc") + 0.04
	yCI <- grconvertY(yCI, "ndc", "user")
	
	if (drawCI) {
		
		arrows(CIlow, yCI, CIhigh, yCI, angle = 90, code = 3, length = 0.1, lwd = lwd)
		medianText <- formatC(medianPosterior, digits = 3, format = "f")
	}
	
	if (addInformation) {
		
		BF01 <- 1 / BF10
		
		# display BF10 value
		
		offsetTopPart <- 0.06
		
		yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
		
		xx <- min(xticks)
		
		if (BF10 >= 1000000 || BF01 >= 1000000) {
		
			BF10t <- format(BF10, digits = 4, scientific = TRUE)
			BF01t <- format(BF01, digits = 4, scientific = TRUE)
		}
		
		if (BF10 < 1000000 && BF01 < 1000000) {
		
			BF10t <- formatC(BF10,3, format = "f")
			BF01t <- formatC(BF01,3, format = "f")
		}
		
		if (hypothesis == "two.sided") {
			
			text(xx, yy2, bquote(BF[10] == .(BF10t)), cex = cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0][1] == .(BF01t)), cex = cexTextBF, pos = 4)
		
		} else if (hypothesis == "greater") {
			
			text(xx, yy2, bquote(BF["+"][0] == .(BF10t)), cex = cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["+"] == .(BF01t)), cex = cexTextBF, pos = 4)
		
		} else if (hypothesis == "less") {
			
			text(xx, yy2, bquote(BF["-"][0] == .(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["-"] == .(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
		
		if (drawCI) {
			
			CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow, 3, format = "f"))), ", ",  bquote(.(formatC(CIhigh, 3, format = "f"))), "]", sep = "")
			medianLegendText <- paste("median =", medianText)
			
			text(max(xticks), yy2, medianLegendText, cex = 1.1, pos = 2)
			text(max(xticks), yy, CIText, cex = 1.1, pos = 2)
		}
		
		### probability wheel ###
		
		if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
			xx <- grconvertX(0.44, "ndc", "user")
		} else if (max(nchar(BF10t), nchar(BF01t)) == 5) {
			xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
		} else if (max(nchar(BF10t), nchar(BF01t)) == 6) {
			xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
		} else if (max(nchar(BF10t), nchar(BF01t)) == 7) {
			xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		} else if (max(nchar(BF10t), nchar(BF01t)) == 8) {
			xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		} else if (max(nchar(BF10t), nchar(BF01t)) > 8) {
			xx <- grconvertX(0.44 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
		
		# make sure that colored area is centered
		
		radius <- 0.06 * diff(range(xticks))
		A <- radius^2 * pi
		alpha <- 2 / (BF01 + 1) * A / radius^2
		startpos <- pi/2 - alpha/2
		
		# draw probability wheel
		
		plotrix::floating.pie(xx, yy,c(BF10, 1),radius = radius, col = c("darkred", "white"), lwd = 2,startpos = startpos)
		
		yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
		
		if (hypothesis == "two.sided") {
			
			text(xx, yy, "data|H1", cex = cexCI)
			text(xx, yy2, "data|H0", cex = cexCI)
			
		} else if (hypothesis == "greater") {
			
			text(xx, yy, "data|H+", cex = cexCI)
			text(xx, yy2, "data|H0", cex = cexCI)
			
		} else if (hypothesis == "less") {
			
			text(xx, yy, "data|H-", cex = cexCI)
			text(xx, yy2, "data|H0", cex = cexCI)
		}
	}
	
	if (medianPosterior >= .5) {
		
		legendPosition <- min(xticks)
 		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty = c(1,3), bty = "n", lwd = c(lwd,lwd), cex = cexLegend, xjust = 0, yjust = 1)
		
	} else {
		
		legendPosition <- max(xticks)
 		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty = c(1,3), bty = "n", lwd = c(lwd,lwd), cex = cexLegend, xjust = 1, yjust = 1)
	}
	
}

.plotSequentialBF.binomTest <- function(d, level, theta0 = .5, a = 1, b = 1, BF10table, hypothesis = "two.sided",
										callback = function(...) 0, lwd = 2, cexPoints = 1.4, cexAxis = 1.2,
										cexYlab = 1.5, cexXlab = 1.6, cexTextBF = 1.4, cexText = 1.2,
										cexLegend = 1.2, cexEvidence = 1.6, lwdAxis = 1.2, plotDifferentPriors = FALSE,
										dontPlotData = FALSE, BFH1H0 = TRUE) {
	
	
	#### settings ####
	
	if ( ! plotDifferentPriors) {
		
		evidenceText <-  TRUE
		
	} else {
		
		evidenceText <-  FALSE
	}
	
	
	par(mar = c(5.6, 6, 7, 7) + 0.1, las = 1)
	
	if (dontPlotData) {
		
		plot(1, type = 'n', xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, xlab = "", ylab = "")
		
		axis(1, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, xlab = "")
		axis(2, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, ylab = "")
		
		mtext("n", side = 1, cex = cexXlab, line= 2.5)
		
		if (hypothesis == "two.sided") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las = 0, cex = cexYlab, line = 3.1)
			}
		}
		
		if (hypothesis == "greater") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (hypothesis == "less") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las = 0, cex = cexYlab, line = 3.1)
			}
		}
		
		return()
	}
	
	if (is.infinite(BF10table))
		stop("Bayes factor is infinite")
	
	if (is.infinite(1/BF10table))
		stop("Bayes factor is too small")
	
	# convert to zero's and one's (ones denote "successes") with respect to the level of interest
	
	x <- ifelse (d == level, 1, 0)
	
	BF10 <- vector("numeric", length(x))
	
	for (i in seq_along(x)) {
		
		counts <- sum(x[1:i] == 1)
		n <- length(x[1:i])
		BF10[i] <- .bayesBinomialTest(counts, n, theta0, hypothesis, a = a, b = b)
		
		if (is.na(BF10[i]))
			stop("One or more Bayes factors cannot be computed")
		
		if (is.infinite(BF10[i]))
			stop("One or more Bayes factors are infinity")
	}
	
	if (BFH1H0) {
	
		BF <- BF10
		
	} else {
	
		BF <- 1 / BF10
	}
	
	bfAxis <- .scaleBFaxis(BF)
	
	yAt <- bfAxis$yAt
	yLab <- bfAxis$yLab
	omit3s <- bfAxis$omit3s
	
	####################### plot ###########################
	
	xLab <- pretty(c(0, length(BF10)+2))
	xlim <- range(xLab)
	ylow <- log(eval(parse(text= yLab[1])))
	yhigh <- log(eval(parse(text= yLab[length(yLab)])))
	
	if (is.infinite(yhigh))
		yhigh <- 1e+308
	
	ylim <- c(ylow, yhigh)
	
	plot(1, 1, xlim = xlim, ylim = ylim, ylab = "", xlab = "", type = "n", axes = FALSE)
	
	
	for (i in seq_along(yAt))
		lines(x = xlim, y = rep(yAt[i], 2), col = "darkgrey", lwd = 1.3, lty = 2)
	
	lines(xlim, rep(0, 2), lwd = lwd)
	
	axis(1, at = xLab, labels = xLab, cex.axis = cexAxis, lwd = lwdAxis)
	axis(2, at = yAt, labels = yLab, cex.axis = cexAxis, lwd = lwdAxis)
	
	par(xpd = TRUE) # enable plotting in margin
	
	xx <- grconvertX(0.79, "ndc", "user")
	
	yAthigh <- yAt[yAt >= 0]
	
	if (!omit3s & eval(parse(text = yLab[1])) >= 1/300 & eval(parse(text = yLab[length(yLab)])) <= 300) {
		
		for (i in 1:(length(yAthigh)-1)) {
			
			yy <- mean(c(yAthigh[i], yAthigh[i+1]))
			
			if (yAthigh[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos = 4, cex = cexText)
			}
			if (yAthigh[i] == log(3)) {
				text(x = xx, yy,"Moderate", pos = 4, cex = cexText)
			}
			if (yAthigh[i] == log(10)) {
				text(x = xx, yy,"Strong", pos = 4, cex = cexText)
			}
			if (yAthigh[i] == log(30)) {
				text(x = xx, yy,"Very strong", pos = 4, cex = cexText)
			}
			if (yAthigh[i] == log(100)) {
				text(x = xx, yy,"Extreme", pos = 4, cex = cexText)
			}
		}
		
		yAtlow <- rev(yAt[yAt <= 0])
		
		for (i in 1:(length(yAtlow)-1)) {
			
			yy <- mean(c(yAtlow[i], yAtlow[i+1]))
			
			if (yAtlow[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos = 4, cex = cexText)
			}
			if (yAtlow[i] == log(1/3)) {
				text(x = xx, yy,"Moderate", pos = 4, cex = cexText)
			}
			if (yAtlow[i] == log(1/10)) {
				text(x = xx, yy,"Strong", pos = 4, cex = cexText)
			}
			if (yAtlow[i] == log(1/30)) {
				text(x = xx, yy,"Very strong", pos = 4, cex = cexText)
			}
			if (yAtlow[i] == log(1/100)) {
				text(x = xx, yy,"Extreme", pos = 4, cex = cexText)
			}
		}
		
		if ( ! .shouldContinue(callback()))
			return()
		
		axis(side = 4, at = yAt, tick = TRUE, las = 2, cex.axis = cexAxis, lwd = lwdAxis, labels = FALSE, line = -0.6)
		
		xx <- grconvertX(0.96, "ndc", "user")
		yy <- grconvertY(0.5, "npc", "user")
		
		text(xx, yy, "Evidence", srt = -90, cex = cexEvidence)
	}
	
	if (omit3s) {
		
		if (hypothesis == "two.sided") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las = 0, cex = cexYlab, line = 4.3)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las = 0, cex = cexYlab, line = 4.3)
			}
		}
		
		if (hypothesis == "greater") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las = 0, cex = cexYlab, line = 4.3)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las = 0, cex = cexYlab, line = 4.3)
			}
		}
		
		if (hypothesis == "less") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las = 0, cex = cexYlab, line = 4.3)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las = 0, cex = cexYlab, line = 4.3)
			}
		}
	}
	
	if (omit3s == FALSE) {
		
		if (hypothesis == "two.sided") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las = 0, cex = cexYlab, line = 3.1)
			}
		}
		
		if (hypothesis == "greater") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las = 0, cex = cexYlab, line = 3.1)
			}
		}
		
		if (hypothesis == "less") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line = 3.1)
			}
		}
	}
	
	mtext("n", side = 1, cex = cexXlab, line= 2.5)
	
	xx <- grconvertX(0.1, "npc", "user")
	yy1 <- yAt[length(yAt)-1]
	yy2 <- yAt[length(yAt)]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4* diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd = lwd)
	
	xxt <- grconvertX(0.28, "npc", "user")
	
	if (hypothesis == "two.sided") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		}
	}
	
	if (hypothesis == "greater") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		}
	}
	
	if (hypothesis == "less") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		}
	}
	
	
	yy1 <- yAt[2]
	yy2 <- yAt[1]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd = lwd)
	
	if (hypothesis == "two.sided") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex = cexText)
		}
	}
	
	if (hypothesis == "greater"){
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex = cexText)
		}
	}
	
	if (hypothesis == "less") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex = cexText)
		}
	}
	
	
	BF10e <- BF10table
	BF01e <- 1 / BF10e
	
	# display BF10 value
	
	offsetTopPart <- 0.06
	
	xx <- min(xLab)
	yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
	yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
	
	if (BF10e >= 1000000 | BF01e >= 1000000) {
		
		BF10t <- formatC(BF10e,3, format = "e")
		BF01t <- formatC(BF01e,3, format = "e")
	}
	
	if (BF10e < 1000000 & BF01e < 1000000) {
		
		BF10t <- formatC(BF10e, 3, format = "f")
		BF01t <- formatC(BF01e, 3, format = "f")
	}
	
	if (hypothesis == "two.sided") {
		
		text(xx, yy2, bquote(BF[10] == .(BF10t)), cex = cexTextBF, pos = 4, offset = -.2)
		text(xx, yy, bquote(BF[0][1] == .(BF01t)), cex = cexTextBF, pos = 4, offset = -.2)
	}
	
	if (hypothesis == "greater") {
		
		text(xx, yy2, bquote(BF["+"][0] == .(BF10t)), cex = cexTextBF, pos = 4, offset = -.2)
		text(xx, yy, bquote(BF[0]["+"] == .(BF01t)), cex = cexTextBF, pos = 4, offset = -.2)
	}
	
	if (hypothesis == "less") {
		
		text(xx, yy2, bquote(BF["-"][0] == .(BF10t)), cex = cexTextBF, pos = 4, offset = -.2)
		text(xx, yy, bquote(BF[0]["-"] == .(BF01t)), cex = cexTextBF, pos = 4, offset = -.2)
	}
	
	
	# probability wheel
	
	if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
		xx <- grconvertX(0.44, "ndc", "user")
	} else if (max(nchar(BF10t), nchar(BF01t)) == 5) {
		xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
	} else if (max(nchar(BF10t), nchar(BF01t)) == 6) {
		xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
	} else if (max(nchar(BF10t), nchar(BF01t)) == 7) {
		xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
	} else if (max(nchar(BF10t), nchar(BF01t)) == 8) {
		xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
	} else if (max(nchar(BF10t), nchar(BF01t)) > 8) {
		xx <- grconvertX(0.445 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
	}
	
	yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
	
	
	# make sure that colored area is centered
	
	radius <- grconvertX(0.2, "ndc", "user") - grconvertX(0.16, "ndc", "user")
	A <- radius^2*pi
	alpha <- 2 / (BF01e + 1) * A / radius^2
	startpos <- pi/2 - alpha/2
	
	if ( ! .shouldContinue(callback()))
		return()
	
	# draw probability wheel
	
	plotrix::floating.pie(xx, yy,c(BF10e, 1),radius = radius, col = c("darkred", "white"), lwd = 2, startpos = startpos)
	
	yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
	yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
	
	if (hypothesis == "two.sided") {
		
		text(xx, yy, "data|H1", cex = 1.1)
		text(xx, yy2, "data|H0", cex =  1.1)
	}
	
	if (hypothesis == "greater") {
		
		text(xx, yy, "data|H+", cex =  1.1)
		text(xx, yy2, "data|H0", cex =  1.1)
	}
	
	if (hypothesis == "less") {
		
		text(xx, yy, "data|H-", cex =  1.1)
		text(xx, yy2, "data|H0", cex =  1.1)
	}
	
	if (length(BF10) <= 60) {
		
		points(log(BF), pch = 21, bg = "grey", cex = cexPoints, lwd = 1.3)
		
	} else {
		
		lines(log(BF), col = "black", lwd = 2.7)
	}
	
	BFevidence <- BF10e
	
	if (evidenceText) {
		
		if (BF10e < 1)
			BFevidence <- 1 / BF10e
		
		if (BFevidence >= 1 & BFevidence <= 3) {
			lab <- "Anecdotal"
		} else if (BFevidence > 3 & BFevidence <= 10) {
			lab <- "Moderate"
		} else if (BFevidence > 10 & BFevidence <= 30) {
			lab <- "Strong"
		} else if (BFevidence > 30 & BFevidence <= 100) {
			lab <- "Very strong"
		} else if (BFevidence > 100) {
			lab <- "Extreme"
		}
		
		xxT <- max(xLab)
		yyT <- grconvertY(0.775 + offsetTopPart, "ndc", "user")
		
		if (BF10e >= 1) {
			
			if (hypothesis == "two.sided") {
				text(xxT, yyT, paste("Evidence for H1:\n", lab), cex = 1.4, pos = 2, offset = -.2)
			} else if (hypothesis == "greater") {
				text(xxT, yyT, paste("Evidence for H+:\n", lab), cex = 1.4, pos = 2, offset = -.2)
			} else if (hypothesis == "less") {
				text(xxT, yyT, paste("Evidence for H-:\n", lab), cex = 1.4, pos = 2, offset = -.2)
			}
		}
		
		if (BF10e < 1)
			text(xxT, yyT, paste("Evidence for H0:\n", lab), cex = 1.4, pos = 2, offset = -.2)
	}
}

.scaleBFaxis <- function(BF, callback = function(...) 0) {
	
	# y-axis labels larger than 1
	
	y1h <- "1"
	
	i <- 1
	
	while (eval(parse(text = y1h[i])) < max(BF)) {
		
		if (grepl(pattern = "e",y1h[i])) {
			
			newy <- paste(strsplit(y1h[i], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i], split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
			
		} else {
			
			newy <- paste(y1h[i], "0", sep = "")
		}
		
		if (eval(parse(text=newy)) >= 10^6) {
			
			newy <- format(as.numeric(newy), digits = 3, scientific = TRUE)
		}
		
		y1h <- c(y1h, newy)
		i <- i + 1
	}
	
	y3h <- "3"
	
	i <- 1
	
	while (eval(parse(text = y3h[i])) < max(BF)) {
		
		if (grepl(pattern = "e",y3h[i])) {
			
			newy <- paste(strsplit(y3h[i], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i], split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
		} else {
			
			newy <- paste(y3h[i], "0", sep = "")
		}
		
		if (as.numeric(newy) >= 10^6) {
			
			newy <- format(as.numeric(newy), digits = 3, scientific = TRUE)
		}
		
		y3h <- c(y3h, newy)
		
		i <- i + 1
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	yhigh <- vector("numeric", length(y1h) + length(y3h))
	
	o <- 1
	e <- 1
	
	for (i in seq_along(yhigh)) {
		
		if (i %% 2 == 1) {
			
			yhigh[i] <- y1h[o]
			o <- o + 1
		}
		
		if (i %% 2 == 0) {
			
			yhigh[i] <- y3h[e]
			e <- e + 1
		}
	} 
	
	yhighLab <- as.character(yhigh)
	
	
	# y-axis labels smaller than 1
	
	y1l <- "1/1"
	
	i <- 1
	
	while (eval(parse(text = y1l[i])) > min(BF)) {
		
		if (grepl(pattern = "e",y1l[i])) {
			
			newy <- paste(strsplit(y1l[i], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i], split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
		} else {
			
			newy <- paste(y1l[i], "0", sep = "")
		}
		
		if (eval(parse(text= newy)) <= 10^(-6)) {
			
			newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}
		
		y1l <- c(y1l, newy)
		i <- i + 1
	}
	
	y3l <- "1/3"
	
	i <- 1
	
	while (eval(parse(text= y3l[i])) > min(BF)) {
		
		if (grepl(pattern = "e",y3l[i])) {
			
			newy <- paste(strsplit(y3l[i], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i], split = "+", fixed = TRUE)[[1]][2])+1, sep = "")
		} else {
			
			newy <- paste(y3l[i], "0", sep = "")
		}
		
		if (newy == "1/3e+9") {
			
			newy <- "1/3e+09"
		}
		
		if (eval(parse(text = newy)) <= 10^(-6) & eval(parse(text = newy)) > 10^(-9)) {
			
			newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
			newy <- paste(substring(newy, 1, nchar(newy)-1), as.numeric(substring(newy, nchar(newy), nchar(newy))) - 1, sep = "")
			newy <- sub(".33", "", newy)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}
		
		y3l <- c(y3l, newy)
		i <- i + 1
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	ylow <- vector("numeric", length(y1l) + length(y3l))
	o <- 1
	e <- 1
	
	for (i in seq_along(ylow)) {
		
		if (i %% 2 == 1) {
			
			ylow[i] <- y1l[o]
			o <- o + 1
		}
		
		if (i %% 2 == 0) {
			
			ylow[i] <- y3l[e]
			e <- e + 1
		}
	}
	
	yLab <- c(rev(ylow[-1]), yhighLab)
	
	
	# remove 3's if yLab vector is too long
	omit3s <- FALSE
	
	if (length(yLab) > 9) {
		
		omit3s <- TRUE
		
		ind <- which(yLab == "3")
		
		yLabsHigh <- yLab[ind:length(yLab)]
		
		if (length(yLabsHigh) > 1) {
			
			yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh), 2)]
			
		} else {
			
			yLabsHigh <- character(0)
		}
		
		yLabsLow <- yLab[1:(ind-1)]
		yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]
		
		yLab1s <- c(yLabsLow, yLabsHigh)
		
		
		if (max(BF) > eval(parse(text = yLab1s[length(yLab1s)-1]))) {
			
			#for (i in 1) {
				
				if(grepl(pattern = "e", yLab1s[length(yLab1s)])){
					
					newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)], split = "+", fixed = TRUE)[[1]][2])+1, sep = "")
					
				} else {
					
					newy <- paste(yLab1s[length(yLab1s)], "0", sep = "")
				}
				
				if (eval(parse(text = newy)) >= 10^6) {
					
					newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
				}
				
				yLab1s <- c(yLab1s, newy)
			#}
		}
		
		if (yLab1s[1] == "1") {
			
			yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
		}
		
		if (yLab1s[length(yLab1s)] == "1") {
			
			yLab1s <- c(yLab1s, "10")
		}
		
		if (min(BF) < eval(parse(text = yLab1s[2]))) {
			
			#for (i in 1:2) {
				
				if (grepl(pattern = "e",yLab1s[1])) {
					
					newy <- paste(strsplit(yLab1s[1], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
					
				} else {
					
					newy <- paste(yLab1s[1], "0", sep = "")
				}
				
				if (eval(parse(text = newy)) <= 10^(-6)) {
					
					newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
					newy <-  sub("-", "+", x = newy)
					newy <- substring(newy, nchar(newy) - 4, nchar(newy))
					newy <- paste0("1/", newy)
				}
			#}
			
			yLab1s <- c(newy, yLab1s)
		}
		
		yLab <- yLab1s
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	while (length(yLab) > 9) {
		
		ind <- which(yLab == "1")
		
		if (ind == 1) {
			
			yLabLow <- character(0)
			
		} else {
			
			yLabLow <- yLab[1:(ind-1)]
		}
		
		if (ind == length(yLab)) {
			
			yLabHigh <- character(0)
			
		} else {
			
			yLabHigh <- yLab[(ind+1):length(yLab)]
		}
		
		if (length(yLabLow) > 1) {
			
			yLabLow <- yLabLow[seq(length(yLabLow)-1, 1, -2)]
			
		} else {
			
			yLabLow <- yLabLow
		}
		
		if (length(yLabHigh) > 1) {
			
			yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
			
		} else {
			
			yLabHigh <- yLabHigh
		}
		
		if (length(yLabLow) == 1) {
			
			yLabLow <- paste("1/", yLabHigh[1], sep = "")
		}
		
		if (length(yLabHigh) == 1) {
			
			yLabHigh <- strsplit(x = yLabLow[1], "/", fixed = TRUE)[[1]][2]
		}
		
		yLab <- c(rev(yLabLow), "1", yLabHigh)
	}
	
	
	while (eval(parse(text=yLab[1])) > min(BF)) {
		
		for (i in 1:2) {
			
			interval <- as.numeric(strsplit(yLab[1], "+", fixed = TRUE)[[1]][2]) - as.numeric(strsplit(yLab[2], "+", fixed = TRUE)[[1]][2])
			pot <- as.numeric(strsplit(yLab[1], "+", fixed = TRUE)[[1]][2]) + interval
			
			newy <- paste(strsplit(yLab[1], "+", fixed = TRUE)[[1]][1], "+", pot, sep = "")
			yLab <- c(newy, yLab)
		}
	}
	
	while (eval(parse(text = yLab[length(yLab)])) < max(BF)) {
		
		for (i in 1:2) {
			
			interval <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed = TRUE)[[1]][2]) - as.numeric(strsplit(yLab[length(yLab)-1], "+", fixed = TRUE)[[1]][2])
			pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed = TRUE)[[1]][2]) + interval
			newy <- paste(strsplit(yLab[length(yLab)], "+", fixed = TRUE)[[1]][1], "+", pot, sep ="")
			yLab <- c( yLab, newy)
		}
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	yAt <- vector("numeric", length(yLab))
	
	for (i in seq_along(yLab)) {
		
		yAt[i] <- log(eval(parse(text = yLab[i])))
	}
	
	return(list(yAt = yAt, yLab = yLab, omit3s = omit3s))
}
