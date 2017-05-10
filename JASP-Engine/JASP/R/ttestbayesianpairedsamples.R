#
# Copyright (C) 2013-2015 University of Amsterdam
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
	
	meta[[1]] <- list(name="ttest", type="table")
	meta[[2]] <- list(name="descriptives", type="object", meta=list(list(name="descriptivesTable", type="table"), list(name = "descriptivesPlots", type = "collection", meta="image")))
	meta[[3]] <- list(name="inferentialPlots", type="collection", meta=list(	name="plotGroups", type="object",
																	meta=list(
																				list(name="PriorPosteriorPlot", type="image"),
																				list(name="BFrobustnessPlot", type="image"),
																				list(name="BFsequentialPlot", type="image")
																				)))
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Paired Samples T-Test"
	
	ttest <- list()
	
	ttest[["title"]] <- "Bayesian Paired Samples T-Test"
	
	if (options$effectSizeStandardized == "default") {
	  ttest[["citation"]] <- list(
	    "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
	    "Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
	} else if (options$effectSizeStandardized == "informative") {
	  ttest[["citation"]] <- list(
	    "Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2017). Informed Bayesian T-Tests. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1704.02479")
	}
	
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
	plotGroups <- list()
	plots.ttest <- list()
	plotTypes <- list()
	plotPairs <- list()
	descriptivesPlots <- list()
	descriptPlotPairs <- list()
	tablePairs <- list()
	errorFootnotes <- rep("no", length(options$pairs))
	
	state <- .retrieveState()
	
	diff <- NULL
	
	if (!is.null(state)) {
		
		diff <- .diff(options, state$options)
	
	}
	
	if (options$descriptives || options$descriptivesPlots)
		results[["descriptives"]] <- list(title="Descriptives")
	
	footnotes <- .newFootnotes()
	
	i <- 1
	
	for (pair in options$pairs)
	{
		
		currentPair <- paste(pair, collapse=" - ")
		
		if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis) {
			
			plotGroups[[i]] <- list()
			plotGroups[[i]][["title"]] <- currentPair
			plotGroups[[i]][["name"]] <- currentPair
		}
		
		if (options$descriptivesPlots) {
			
			if (!is.null(state) && currentPair %in% state$descriptPlotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$missingValues == FALSE && diff$plotWidth == FALSE &&
				diff$plotHeight == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {
				
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				index <- which(state$descriptPlotPairs == currentPair)
				
				descriptivesPlots[[length(descriptivesPlots)+1]] <- state$descriptivesPlots[[index]]
				
				
			} else {
				
				descriptivesPlot <- list()
				
				descriptivesPlot[["title"]] <- currentPair
				descriptivesPlot[["width"]] <- options$plotWidth
				descriptivesPlot[["height"]] <- options$plotHeight
				descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")
				descriptivesPlot[["status"]] <- "waiting"
				descriptivesPlot[["data"]] <- ""
				
				descriptivesPlots[[length(descriptivesPlots)+1]] <- descriptivesPlot
				
			}
			
			descriptPlotPairs[[length(descriptPlotPairs)+1]] <- currentPair
		}
		
		if (options$plotPriorAndPosterior) {
			
			
			if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
				&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
				diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
				diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
				options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "posteriorPlotAddInfo")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
					
			} else if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
						&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
						diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
						diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
						!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# if the requested plot already exists use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "posteriorPlot")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
				
			} else {
				
				plot <- list()
				
				plot[["title"]] <- "Prior and Posterior"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"

				.plotFunc <- function() {
				  .plotPosterior.summarystats.ttest(addInformation = options$plotPriorAndPosteriorAdditionalInf, dontPlotData = TRUE)
				}
				
				content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				
				
				plots.ttest[[length(plots.ttest)+1]] <- plot
			}
			
			plotGroups[[i]][["PriorPosteriorPlot"]] <- plots.ttest[[length(plots.ttest)]]
			
			if (options$plotPriorAndPosteriorAdditionalInfo) {
				
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"
				
			} else {
				
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
			}
			
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
			
		}
		
		if (options$plotBayesFactorRobustness) {
			
			
			if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
				&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
				diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
				diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) && "robustnessPlot" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "robustnessPlot")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
				
			} else {
				
				plot <- list()
				
				plot[["title"]] <- "Bayes Factor Robustness Check"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"

				.plotFunc <- function() {
					.plotBF.robustnessCheck.ttest (oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
				}
				content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				
				plots.ttest[[length(plots.ttest)+1]] <- plot
			}
			
			plotGroups[[i]][["BFrobustnessPlot"]] <- plots.ttest[[length(plots.ttest)]]
			plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
		}
		
		if (options$plotSequentialAnalysis){
			
			if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
				&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
				diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
				diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
				options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "sequentialRobustnessPlot")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
				
			} else if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
						&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
						diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
						diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
						!options$plotSequentialAnalysisRobustness  && "sequentialPlot" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# if the requested plot already exists use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "sequentialPlot")
				
				plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
				
			} else {
				
				plot <- list()
				
				plot[["title"]] <- "Sequential Analysis"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"

				.plotFunc <- function() {
					.plotSequentialBF.ttest(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE, options = options)
				}
				content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				
				plots.ttest[[length(plots.ttest)+1]] <- plot
			}
			
			if (options$plotSequentialAnalysisRobustness) {
				
				plotTypes[[length(plotTypes)+1]] <- "sequentialRobustnessPlot"
				
			} else {
				
				plotTypes[[length(plotTypes)+1]] <- "sequentialPlot"
			}
			
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
			plotGroups[[i]][["BFsequentialPlot"]] <- plots.ttest[[length(plots.ttest)]]
		}
		
		i <- i + 1
		
	}
	
	if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis)
			results[["inferentialPlots"]] <- list(title=ifelse(length(options$pairs) > 1 || sum(c(options$plotPriorAndPosterior, options$plotBayesFactorRobustness, options$plotSequentialAnalysis)) > 1,
				"Inferential Plots", "Inferential Plot"), collection=plotGroups)
	
	if (options$descriptivesPlots)
		results[["descriptives"]][["descriptivesPlots"]] <- list(title=ifelse(length(options$pairs) > 1, "Descriptives Plots", "Descriptives Plot"), collection=descriptivesPlots)
	
	pair.statuses <- list()
	
	BF10post <- numeric(length(options$pairs))
	
	tValue <- rep(NA, length(.indices(options$pairs)))
	n <- rep(NA, length(.indices(options$pairs)))
	
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
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$effectSizeStandardized == FALSE && diff$informativeStandardizedEffectSize == FALSE &&
					diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE && diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
					diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE)))) {
					
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
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$effectSizeStandardized == FALSE && diff$informativeStandardizedEffectSize == FALSE &&
					diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE && diff$informativeTScale == FALSE &&
					diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE)))) {
					
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
					tValue[i] <- state$tValue[stateIndex]
					n[i] <- state$n[stateIndex]
					
				} else {
					
					result <- try (silent = TRUE, expr = {
						
						subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
						subDataSet <- na.omit(subDataSet)
						
						c1 <- subDataSet[[ .v(pair[[1]]) ]]
						c2 <- subDataSet[[ .v(pair[[2]]) ]]
						
						r <- .generalTtestBF(x = c1, y = c2, paired = TRUE, oneSided = oneSided, options = options)
						bf.raw <- r[["bf"]]
						tValue[i] <- r[["tValue"]]
						n[i] <- r[["n1"]]
						
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
						
						error <- .clean(r[["error"]])
						
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
		
	}
	
	if (length(ttest.rows) == 0)
		ttest.rows <- list(list(.variable1="...", .separator="-", .variable2="...", BF="", error=""))
	
	ttest[["data"]] <- ttest.rows
	ttest[["footnotes"]] <- as.list(footnotes)
	
	if (perform == "run")
		ttest[["status"]] <- "complete"
	
	results[["ttest"]] <- ttest
	
	descriptives <- NULL
	
	if (options$descriptives) {
		
		descriptives <- list()
		
		descriptives[["title"]] <- "Descriptives"
		
		fields <- list(
			list(name="v", type="string", title=""),
			list(name="N",                  type="integer"),
			list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
			list(name="sd",   title="SD",   type="number", format="sf:4;dp:3"),
			list(name="se",   title="SE",   type="number", format="sf:4;dp:3"))
		
			## add credible interval values if asked for in plot
			if (options$descriptivesPlots) {
				interval <- 100 * options$descriptivesPlotsCredibleInterval
				title <- paste0(interval, "% Credible Interval")
				fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
													 format = "sf:4;dp:3", title = "Lower",
													 overTitle = title)
				fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
													 format = "sf:4;dp:3", title = "Upper",
													 overTitle = title)
			}
			
		descriptives[["schema"]] <- list(fields=fields)
		
		descriptives.results <- list()
		
		variables <- unlist(options$pairs)
		variables <- unique(variables)
		variables <- variables[variables != ""]
		
		for (variable in variables) {
			
			if (perform == "run") {
				
				result <- try (silent = TRUE, expr = {
					
					variableData <- dataset[[.v(variable)]]
					variableDataOm <- na.omit(variableData)

					posteriorSummary <- .posteriorSummaryGroupMean(variable=variableDataOm, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
					ciLower <- .clean(posteriorSummary$ciLower)
					ciUpper <- .clean(posteriorSummary$ciUpper)

					n <- .clean(as.numeric(length(variableDataOm)))
					m <- .clean(as.numeric(mean(variableDataOm)))
					std <- .clean(as.numeric(sd(variableDataOm)))
					if(is.numeric(std)){
						se <- .clean(as.numeric(std/sqrt(n)))}
					else
						se <- .clean(NaN)
					
						list(v=variable, N=n, mean=m, sd=std, se=se, lowerCI=ciLower, upperCI=ciUpper)
				})
				
				if (class(result) == "try-error") {
					
					result <- list(v=variable, N="", mean="", sd="", se="", lowerCI="", upperCI="")
				}
				
			} else {
				
				result <- list(v=variable, N=".", mean=".", sd=".", se=".", lowerCI=".", upperCI=".")
			}
			
			descriptives.results[[length(descriptives.results)+1]] <- result
		}
		
		descriptives[["data"]] <- descriptives.results
		descriptives[["status"]] <- "complete"
		
		
	}
	
	results[["descriptives"]][["descriptivesTable"]] <- descriptives
	
	
	# PLOTS
	
	if (perform == "run" && length(options$pairs) > 0 && (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis || options$descriptivesPlots)) {
		
		if ( ! .shouldContinue(callback(results)))
			return()
		
		n.plots.per.variable <- sum(options$plotPriorAndPosterior, options$plotBayesFactorRobustness, options$plotSequentialAnalysis)
		n.plots <- length(options$pairs) * n.plots.per.variable
		
		j <- 1
		descriptInd <- 1
		
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
			
			if (options$descriptivesPlots) {
				
				if (!is.null(state) && tablePairs[[i]] %in% state$descriptPlotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$missingValues == FALSE &&
					diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {
					
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					index <- which(state$descriptPlotPairs == tablePairs[[i]])
					
					descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]
					
					
				} else {
					
					results[["descriptives"]][["descriptivesPlots"]][["collection"]][[descriptInd]][["status"]] <- "running"
					
					if ( ! .shouldContinue(callback(results)))
						return()
					
					plot <- descriptivesPlots[[descriptInd]]
					
					p <- try(silent= FALSE, expr= {

							p <- .plot2GroupMeansBayesIndTtest(v1 = c1, v2 = c2, nameV1 = pair[[1]], nameV2 = pair[[2]], descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
							content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = p, obj = TRUE)
							
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
						})
						
					if (class(p) == "try-error") {
						
						errorMessageTmp <- .extractErrorMessage(p)
						errorMessage <- paste0("Plotting not possible: ", errorMessageTmp)
						plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
					}
					
					plot[["status"]] <- "complete"
					
					descriptivesPlots[[descriptInd]] <- plot
					
				}
				
				results[["descriptives"]][["descriptivesPlots"]][["collection"]] <- descriptivesPlots
				
				if ( ! .shouldContinue(callback(results)))
					return()
				
				descriptInd <- descriptInd + 1
				
			}
			
			if (options$plotPriorAndPosterior) {
				
				
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
					diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
					diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
					options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "posteriorPlotAddInfo")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
						
				} else if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
							&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
							diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
							diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
							!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# if the requested plot already exists use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "posteriorPlot")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
					
				} else {
					
					results[["inferentialPlots"]][["collection"]][[i]][["PriorPosteriorPlot"]][["status"]] <- "running"
					
					if ( ! .shouldContinue(callback(results)))
						return()
					
					plot <- plots.ttest[[j]]
					
					
					if (status$unplotable == FALSE) {
						
						
						p <- try(silent= FALSE, expr= {
						  
						  .plotFunc <- function() {
						    .plotPosterior.summarystats.ttest(t = tValue[i], n1 = n[i], n2 = n[i], paired = TRUE,
						                                      oneSided = oneSided, BF = BF10post[i], BFH1H0 = BFH1H0,
						                                      rscale = options$priorWidth,
						                                      addInformation = options$plotPriorAndPosteriorAdditionalInfo,
						                                      options = options)
						  }
							
							content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
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
				
				plotGroups[[i]][["PriorPosteriorPlot"]] <- plots.ttest[[j]]
				results[["inferentialPlots"]][["collection"]] <- plotGroups
				
				if ( ! .shouldContinue(callback(results)))
					return()
				
				j <- j + 1
			}
			
			if (options$plotBayesFactorRobustness) {
				
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
					diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
					diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
					"robustnessPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "robustnessPlot")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
					
				} else {
					
					results[["inferentialPlots"]][["collection"]][[i]][["BFrobustnessPlot"]][["status"]] <- "running"
					
					if ( ! .shouldContinue(callback(results)))
						return()
					
					plot <- plots.ttest[[j]]
					
					if (options$effectSizeStandardized == "informative") {
					  plot[["error"]] <- list(error="badData", errorMessage="Bayes factor robustness check plot currently not supported for informed prior.")
					} else if (status$unplotable == FALSE) {
					
						p <- try(silent= FALSE, expr= {

						  .plotFunc <- function() {
								.plotBF.robustnessCheck.ttest(x=c1, y=c2, BF10post=BF10post[i], paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, BFH1H0=BFH1H0)
							}
							content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
			
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
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
				
				plotGroups[[i]][["BFrobustnessPlot"]] <- plots.ttest[[j]]
				results[["inferentialPlots"]][["collection"]] <- plotGroups
				
				if ( ! .shouldContinue(callback(results)))
					return()
				
				j <- j + 1
			}
			
			if (options$plotSequentialAnalysis) {
				
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
					diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
					diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
					options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "sequentialRobustnessPlot")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
					
				} else if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
							&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$effectSizeStandardized == FALSE &&
							diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE && diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
							diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) && !options$plotSequentialAnalysisRobustness &&
							"sequentialPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# if the requested plot already exists use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "sequentialPlot")
					
					plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
					
				} else {
					
					results[["inferentialPlots"]][["collection"]][[i]][["BFsequentialPlot"]][["status"]] <- "running"
					
					if ( ! .shouldContinue(callback(results)))
						return()
					
					plot <- plots.ttest[[j]]
					
					if (options$plotSequentialAnalysisRobustness && options$effectSizeStandardized == "informative") {
					  plot[["error"]] <- list(error="badData", errorMessage="Sequential analysis robustness check plot currently not supported for informed prior.")
					} else if (status$unplotable == FALSE && sequentialIsViable) {
						
						p <- try(silent= FALSE, expr= {

						  .plotFunc <- function() {
								.plotSequentialBF.ttest(x=c1, y=c2, paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, BF10post=BF10post[i],
								                        plotDifferentPriors=options$plotSequentialAnalysisRobustness, BFH1H0=BFH1H0, options = options)
							}
							content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
			
							plot[["convertible"]] <- TRUE
							plot[["obj"]] <- content[["obj"]]
							plot[["data"]] <- content[["png"]]
							
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
				
				plotGroups[[i]][["BFsequentialPlot"]] <- plots.ttest[[j]]
				results[["inferentialPlots"]][["collection"]] <- plotGroups
				
				if ( ! .shouldContinue(callback(results)))
					return()
				
				j <- j + 1
			}
			
		}
	}
	
	results[["ttest"]] <- ttest
	
	keep <- NULL
	
	for (plot in plots.ttest)
		keep <- c(keep, plot$data)
	
	for (plot in descriptivesPlots)
		keep <- c(keep, plot$data)
	
	if (perform == "init") {
		
		return(list(results=results, status="inited", state=state, keep=keep))
		
	} else {
	
		return(list(results=results, status="complete", state=list(options=options, results=results, plotsTtest=plots.ttest, plotTypes=plotTypes, plotPairs=plotPairs,
		descriptPlotPairs=descriptPlotPairs, descriptivesPlots=descriptivesPlots, pairStatuses=pair.statuses, BF10post=BF10post, tablePairs=tablePairs,
		errorFootnotes=errorFootnotes, tValue = tValue, n = n), keep=keep))
	}
}

.plot2GroupMeansBayesPairedTtest <- function(v1=NULL, v2=NULL, nameV1=NULL, nameV2=NULL, descriptivesPlotsCredibleInterval=.95) {
	
	v1 <- na.omit(v1)
	v2 <- na.omit(v2)
	
	if (any(is.infinite(v1)) || any(is.infinite(v2)))
		stop("Plotting not possible: Variable contains infinity")
	
	posteriorSummary1 <- .posteriorSummaryGroupMean(variable=v1, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
	posteriorSummary2 <- .posteriorSummaryGroupMean(variable=v2, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
	summaryStat <- data.frame(groupingVariable=c(nameV1, nameV2), dependent=c(posteriorSummary1$median, posteriorSummary2$median), ciLower=c(posteriorSummary1$ciLower, posteriorSummary2$ciLower), ciUpper=c(posteriorSummary1$ciUpper, posteriorSummary2$ciUpper))
	
	pd <- ggplot2::position_dodge(.2)
	
	p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=1)) +
		ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
		ggplot2::geom_line(position=pd, size = .7) + 
		ggplot2::geom_point(position=pd, size=4) +
		ggplot2::ylab(NULL) +
		ggplot2::xlab(NULL) +
		ggplot2::theme_bw() +
		ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
			panel.grid.major=ggplot2::element_blank(),
			axis.title.x = ggplot2::element_text(size=18,vjust=-.2), axis.title.y = ggplot2::element_text(size=18,vjust=-1),
			axis.text.x = ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15),
			panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
			plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
			legend.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
			panel.border = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
			legend.key = ggplot2::element_blank(),
			legend.title = ggplot2::element_text(size=12),
			legend.text = ggplot2::element_text(size = 12),
			axis.ticks = ggplot2::element_line(size = 0.5),
			axis.ticks.margin = grid::unit(1,"mm"),
			axis.ticks.length = grid::unit(3, "mm"),
			plot.margin = grid::unit(c(.5,0,.5,.5), "cm")) +
		.base_breaks_y3(summaryStat) +
		.base_breaks_x(summaryStat$groupingVariable) +
		ggplot2::scale_x_discrete(labels=c(nameV1, nameV2))
	
	print(p)
}
