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


CorrelationBayesianPairs <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
    # Note: This is the default failed bfObject for wrong data
    #
    failedBfObject <- list(n=NaN, r=NaN, stat=NA, bf10=NA, bfPlus0=NA, bfPlus0=NA, bfMin0=NA, ciValue=options$ciValue, ci=list())
    
    useKendall <- options$corcoefficient == "Kendall"
    usePearson <- options$corcoefficient == "Pearson"
	allVariables <- unique(unlist(options$pairs))
	allVariables <- allVariables[allVariables != ""]
	
	if (is.null(dataset)) {
		if (perform == "run") {
		    if (options$missingValues == "excludeListwise") {
		        dataset <- .readDataSetToEnd(columns.as.numeric=allVariables, exclude.na.listwise=allVariables)
		    } else {
		        dataset <- .readDataSetToEnd(columns.as.numeric=allVariables)
		    }
		} else {
		    dataset <- .readDataSetHeader(columns.as.numeric=allVariables)
		}
	} else {
		if (options$missingValues == "excludeListwise") {
		    dataset <- .vdf(dataset, columns.as.numeric=allVariables, exclude.na.listwise=allVariables)
		} else {
		    dataset <- .vdf(dataset, columns.as.numeric=allVariables)
		}
	}
	
	
	results <- list()
	meta <- list()
	
	# MarkUp: General: Tell the parser that correlation is a table
	#
	meta[[1]] <- list(name="correlation", type="table")
	
	# MarkUp: General: Tell that plots are a collection
	#   within colletion of plots, there is plotGroups which is an object, consisting of 
	#       -1 Scatterplot  
	#       -2 Prior posterior plot
	#       -3 BF robustness plot
	#       -4 BF sequential plot
	meta[[2]] <- list(name="plots", type="collection", 
	                  meta=list(name="plotGroups", type="object",
	                            meta=list(list(name="ScatterPlot", type="image"),
	                                      list(name="PriorPosteriorPlot", type="image"),
	                                      list(name="BFrobustnessPlot", type="image"),
	                                      list(name="BFsequentialPlot", type="image")
	                             )
	                  )
	)
	
	# MarkUp: General: Store the meta in the results
	#
	results[[".meta"]] <- meta
	
	# MarkUp: General: Name the results object 
	#
	results[["title"]] <- "Bayesian Correlation Pairs"
	
	
	# MarkUp: General: Table: Output for the table
	#
	correlation <- list()
	if (usePearson) {
	    correlation[["title"]] <- "Bayesian Pearson Correlation"
	    correlation[["citation"]] <- list(
	        "Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2014). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication."
	    )
	    nameForFields <- "r"
	} else if (useKendall) { 
	    correlation[["title"]] <- "Bayesian Kendall Correlation"
	    correlation[["citation"]] <- list(
	        "van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2016). Bayesian Inference for Kendall’s Rank Correlation Coefficient. Manuscript submitted for publication."
	    )
	    nameForFields <- "tau"
	} else if (usePearson && useKendall) {
	    correlation[["title"]] <- "Bayesian Correlation Table"
	    correlation[["citation"]] <- list(
	        "Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2014). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication.",
	        "van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2016). Bayesian Inference for Kendall’s Rank Correlation Coefficient. Manuscript submitted for publication."
	    )
	    nameForFields <- "Statistic"
	}
  	
	# MarkUp: General: Table: Choose the bf type in the table
	#
	bfType <- options$bayesFactorType
	
	if (bfType == "BF10") {
		BFH1H0 <- TRUE
		if (options$hypothesis == "correlated") {
			bfTitle <- "BF\u2081\u2080"
			oneSided <- FALSE
		} else if (options$hypothesis == "correlatedPositively") {
			bfTitle <- "BF\u208A\u2080"
			oneSided <- "right"
		} else if (options$hypothesis == "correlatedNegatively") {
			bfTitle <- "BF\u208B\u2080"
			oneSided <- "left"
		}
	} else if (bfType == "LogBF10") {
		BFH1H0 <- TRUE
		if (options$hypothesis == "correlated") {
			bfTitle <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
			oneSided <- FALSE
		} else if (options$hypothesis == "correlatedPositively") {
			bfTitle <- "Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
			oneSided <- "right"
		} else if (options$hypothesis == "correlatedNegatively") {
			bfTitle <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
			oneSided <- "left"
		}
	} else if (bfType == "BF01") {
		BFH1H0 <- FALSE
		if (options$hypothesis == "correlated") {
			bfTitle <- "BF\u2080\u2081"
			oneSided <- FALSE
		} else if (options$hypothesis == "correlatedPositively") {
			bfTitle <- "BF\u2080\u208A"
			oneSided <- "right"
		} else if (options$hypothesis == "correlatedNegatively") {
			bfTitle <- "BF\u2080\u208B"
			oneSided <- "left"
		}
	}
	
	
	# MarkUp: General: Table: Define the columns
	# 
	fields <- list(
		list(name=".variable1", type="string", title=""),
		list(name=".separator", type="separator", title=""),
		list(name=".variable2", type="string", title=""),
		list(name="r", type="number", format="sf:4;dp:3", title=nameForFields),
		list(name="BF", type="number", format="sf:4;dp:3", title=bfTitle)
	)
	
	if (isTRUE(options$credibleInterval)) {
	    interval <- 100 * options$ciValue
	    title <- paste0(interval, "% Credible interval")
	    
	    fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
	                                         format = "sf:4;dp:3", title = "Lower",
	                                         overTitle = title)
	    fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
	                                         format = "sf:4;dp:3", title = "Upper",
	                                         overTitle = title)
	}
	
	# MarkUp: General: Table: Define the columns
	correlation[["schema"]] <- list(fields=fields)
	
	# Define list of rows 
	correlationRows <- list()
	
	# TODO: Check 
	pairStatuses <- list()
	
	footnotes <- .newFootnotes()
	
	# MarkUp: Define footnotes
	if (options$hypothesis == "correlatedPositively") {
		.addFootnote(footnotes, "For all tests, the alternative hypothesis specifies that the correlation is positive.", symbol="<i>Note</i>.")
	} else if (options$hypothesis == "correlatedNegatively") {
		.addFootnote(footnotes, "For all tests, the alternative hypothesis specifies that the correlation is negative.", symbol="<i>Note</i>.")
	}
	
	# TODO MarkUp: Plots General: 
	plotGroups <- list()
	plots.correlation <- list()
	
	plotTypes <- list()
	plotPairs <- list()
	tablePairs <- list()
	errorFootnotes <- rep("no", length(options$pairs))
	
	state <- .retrieveState()
	
	diff <- NULL
	
	if (!is.null(state)) {
		diff <- .diff(options, state$options)
	}
	
	# TODO: looping
	i <- 1
	
	for (pair in options$pairs)	{
	    # Markup: Table: Row. Create "variableName1 - variableName2"
		currentPair <- paste(pair, collapse=" - ")
		
		if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis || options$plotScatter) {
			plotGroups[[i]] <- list()
			plotGroups[[i]][["title"]] <- currentPair
			plotGroups[[i]][["name"]] <- currentPair
		}
		
		if (options$plotScatter) {
			if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && "plotScatter" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "plotScatter")[1]
				
				plots.correlation[[length(plots.correlation)+1]] <- state$plotsCorrelation[[stateIndex]]
				
			} else {
		
				plot <- list()
				
				plot[["title"]] <- "Scatterplot"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"
				
				# image <- .beginSaveImage(530, 400)
				# .plotScatter.Bcorrelationpairs(xlab=pair[[1]], ylab=pair[[2]], dontPlotData=TRUE)
				# plot[["data"]] <- .endSaveImage(image)
				
				.plotFunc <- function() {
					.plotScatter.Bcorrelationpairs(xlab=pair[[1]], ylab=pair[[2]], dontPlotData=TRUE)
				}
				content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				
				plots.correlation[[length(plots.correlation)+1]] <- plot
			}
			
			plotTypes[[length(plotTypes)+1]] <- "plotScatter"
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
			plotGroups[[i]][["ScatterPlot"]] <- plots.correlation[[length(plots.correlation)]]
		
		}
	
		if (options$plotPriorAndPosterior) {
		
			if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
				&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "posteriorPlotAddInfo")[1]
				
				plots.correlation[[length(plots.correlation)+1]] <- state$plotsCorrelation[[stateIndex]]
					
			} else if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
						&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && !options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
				
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# if the requested plot already exists use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "posteriorPlot")[1]
				
				plots.correlation[[length(plots.correlation)+1]] <- state$plotsCorrelation[[stateIndex]]
				
			} else {
				
				plot <- list()
				
				plot[["title"]] <- "Prior and Posterior"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"
				
				# image <- .beginSaveImage(530, 400)
				# .plotPosterior.correlation(r=NULL, n=NULL, oneSided=oneSided, dontPlotData=TRUE, addInformation=options$plotPriorAndPosteriorAdditionalInfo, corCoefficient=options$corcoefficient)
				# plot[["data"]] <- .endSaveImage(image)
				
				.plotFunc <- function() {
					.plotPosterior.correlation(n=NULL, r=NULL, oneSided=oneSided, dontPlotData=TRUE, addInformation=options$plotPriorAndPosteriorAdditionalInfo, corCoefficient=options$corcoefficient)
				}
				content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				
				plots.correlation[[length(plots.correlation)+1]] <- plot
			}
			
			if (options$plotPriorAndPosteriorAdditionalInfo) {
			    plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"
			} else {
			    plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
			}
			
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
			plotGroups[[i]][["PriorPosteriorPlot"]] <- plots.correlation[[length(plots.correlation)]]
			
		}
		if (options$plotBayesFactorRobustness) {
		    if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
				&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && "robustnessPlot" %in% state$plotTypes) {
		        #
				# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "robustnessPlot")[1]
				
				plots.correlation[[length(plots.correlation)+1]] <- state$plotsCorrelation[[stateIndex]]
		    } else {
		        plot <- list()
				
				plot[["title"]] <- "Bayes Factor Robustness Check"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"
				
				# image <- .beginSaveImage(530, 400)
				# .plotBF.robustnessCheck.correlation (oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE, corCoefficient=options$corcoefficient)
				# plot[["data"]] <- .endSaveImage(image)
				
				.plotFunc <- function() {
					.plotBF.robustnessCheck.correlation (oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE, corCoefficient=options$corcoefficient)
				}
				content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				
				plots.correlation[[length(plots.correlation)+1]] <- plot
			}
			
			plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
			plotGroups[[i]][["BFrobustnessPlot"]] <- plots.correlation[[length(plots.correlation)]]
		}
		
		if (options$plotSequentialAnalysis) {
		    if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
				&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
		        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# then, if the requested plot already exists, use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "sequentialRobustnessPlot")[1]
				plots.correlation[[length(plots.correlation)+1]] <- state$plotsCorrelation[[stateIndex]]
		    } else if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
						&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && !options$plotSequentialAnalysisRobustness  && "sequentialPlot" %in% state$plotTypes) {
		        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				# if the requested plot already exists use it
				
				stateIndex <- which(state$plotPairs == currentPair & state$plotTypes == "sequentialPlot")[1]
				plots.correlation[[length(plots.correlation)+1]] <- state$plotsCorrelation[[stateIndex]]
		    } else {
		        plot <- list()
				
				plot[["title"]] <- "Sequential Analysis"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				plot[["status"]] <- "waiting"
				
				# image <- .beginSaveImage(530, 400)
				# .plotSequentialBF.correlation(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE, corCoefficient=options$corcoefficient)
				# plot[["data"]] <- .endSaveImage(image)
				
				.plotFunc <- function() {
					.plotSequentialBF.correlation(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE, corCoefficient=options$corcoefficient)
				}
				content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				
				plots.correlation[[length(plots.correlation)+1]] <- plot
			}
			
			if (options$plotSequentialAnalysisRobustness) {
			    plotTypes[[length(plotTypes)+1]] <- "sequentialRobustnessPlot"
			} else {
			    plotTypes[[length(plotTypes)+1]] <- "sequentialPlot"
			}
			
			plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
			plotGroups[[i]][["BFsequentialPlot"]] <- plots.correlation[[length(plots.correlation)]]
		}
		i <- i + 1
	}
	
	if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis || options$plotScatter) {
	    results[["plots"]] <- list(title="Plots", collection=plotGroups)
	}
	
	# TODO: Collection of r and s for each pairs Why not make a collection of bfObjects?
	#
	rs <- numeric()
	ns <- numeric()
	BF10post <- numeric()
	
	for (i in .indices(options$pairs)) {
	    index <- NULL
	    pair <- options$pairs[[i]]
	    tablePairs[[length(tablePairs)+1]] <- paste(pair, collapse=" - ")
	
		if (pair[[1]] == "" || pair[[2]] == "") {
		    p1 <- ifelse(pair[[1]] != "", pair[[1]], "...") 
			p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
			
			pairStatuses[[i]] <- list(ready=FALSE, error=FALSE, unplotable=TRUE, unplotableScatter=TRUE)
			result <- list(.variable1=p1, .separator="-", r="", .variable2=p2, BF="")
		} else {
		    if (perform == "init") {
		        if (!is.null(state) && tablePairs[[i]] %in% state$tablePairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$corcoefficient == FALSE))) {
		            
		            stateIndex <- which(state$tablePairs == paste(pair, collapse=" - "))[1]
		            pairStatuses[[i]] <- state$pairStatuses[[stateIndex]]
				
					if (state$errorFootnotes[stateIndex] == "no") {
					    result <- state$results$correlation$data[[stateIndex]]
					} else {
					    index2 <- .addFootnote(footnotes, state$errorFootnotes[stateIndex])
					    errorFootnotes[i] <- state$errorFootnotes[stateIndex]
					    result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], r=.clean(NaN), BF=.clean(NaN), .footnotes=list(r=list(index2)))
					}
		        } else {
		            pairStatuses[[i]] <- list(ready=FALSE, error=FALSE, unplotable=TRUE, unplotableScatter=TRUE)
		            result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], r=".", BF=".")
		        }
		    } else {
		        unplotable <- FALSE
				unplotableMessage <- NULL
				unplotableScatter <- FALSE
				unplotableMessageScatter <- NULL
				
				if (!is.null(state) && tablePairs[[i]] %in% state$tablePairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$corcoefficient == FALSE))) {
					
					stateIndex <- which(state$tablePairs == paste(pair, collapse=" - "))[1]
					
					if (state$errorFootnotes[stateIndex] == "no") {
					    # TODO: Is this the state retrieval? Retrieval of produced row
					    #
					    result <- state$results$correlation$data[[stateIndex]]
					} else {
					    index2 <- .addFootnote(footnotes, state$errorFootnotes[stateIndex])
					    errorFootnotes[i] <- state$errorFootnotes[stateIndex]
					    result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], r=.clean(NaN), BF=.clean(NaN), .footnotes=list(r=list(index2)))
					}
					
					pairStatuses[[i]] <- state$pairStatuses[[stateIndex]]
					BF10post[i] <- state$BF10post[stateIndex]
					rs[i] <- state$rs[stateIndex]
					ns[i] <- state$ns[stateIndex]
				} else {
				    # Data checks TODOTODO: This works well for list wise
				    errorMessage <- NULL
				    
				    errors <- .hasErrors(dataset, perform = perform, message = 'short', 
				                         type = c('observations','variance', 'infinity'),
				                         all.target = c(pair[[1]], pair[[2]]), observations.amount = '< 2')
				    
				    # 
				    # if (options$missingValues == "excludeListwise"){
				    #     errors <- .hasErrors(dataset, perform = perform, message = 'short', 
				    #                          type = c('observations','variance', 'infinity'),
				    #                          all.target = c(pair[[1]], pair[[2]]), observations.amount = '< 2')
				    # } else if (options$missingValues=="excludeAnalysisByAnalysis") {
				    #     subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
				    #     subDataSet <- na.omit(subDataSet)
				    #     
				    #     errors <- .hasErrors(dataset = subDataSet, perform=perform, message="short", 
				    #                          type = c('observations','variance', 'infinity'),
				    #                          all.target = c(pair[[1]], pair[[2]]), observations.amount = '< 2')
				    # } 
					
					# Note: Data and bfs check [start]
					if (!identical(errors, FALSE)) {
					    # Note: Data: NOT ok, 
					    # bf10: can't compute
					    errorMessage <- errors$message
					    unplotable <- TRUE
					    unplotableMessage <- errors$message
					    unplotableScatter <- TRUE
					    unplotableMessageScatter <- errors$message
					    
					    obsFootnote <- errors$message
					    index <- .addFootnote(footnotes, obsFootnote)
						
					    bfObject <- failedBfObject
					} else {
					    # Data okay, load data
					    #
					    if (options$missingValues=="excludeAnalysisByAnalysis") {
					        subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
					        subDataSet <- na.omit(subDataSet)
					        
					        v1 <- subDataSet[[ .v(pair[[1]]) ]]
					        v2 <- subDataSet[[ .v(pair[[2]]) ]]
					    } else if (options$missingValues == "excludeListwise") {
					        v1 <- dataset[[ .v(pair[[1]]) ]]
					        v2 <- dataset[[ .v(pair[[2]]) ]]
					    }
					    
					    #----------------------- compute r & BF ----------------------#
					    if (usePearson) {
					        rObs <- cor(v1, v2)
					    } else if (useKendall) {
					        rObs <- cor(v1, v2, method="kendall")
					    }
					    
					    nObs <- length(v1)
					    
					    # Data check
					    #
					    if (identical(all.equal(abs(rObs), 1), TRUE)) {
					        unplotable <- TRUE
					        unplotableMessage <- "Sample correlation coefficient r is 1 or -1"
					    }
					    
					    if (usePearson) {
					        bfObject <- .bfPearsonCorrelation(n=nObs, r=rObs, kappa=options$priorWidth, ciValue=options$ciValue)
					    } else if (useKendall) {
					        # TODO Johnny I removed var=1 as default etc
					        bfObject <- .bfKendallTau(n=nObs, tauObs=rObs, kappa=options$priorWidth, ciValue=options$ciValue)
					    }
					}
					
					
					# Note: Store for the ith pair
					#
					rs[i] <- bfObject$stat
					ns[i] <- bfObject$n
					
					# Extract infor from bfObject
					# 
					if (options$hypothesis == "correlated") {
					    someLowerCi <- bfObject$ci$twoSided[1]
					    someUpperCi <- bfObject$ci$twoSided[3]
					    
					    if (options$bayesFactorType=="BF10") {
					        BF10post[i] <-bfObject$bf10
					    } else if (options$bayesFactorType == "BF01") {
							BF10post[i] <- 1/bfObject$bf10
						} else if (options$bayesFactorType == "LogBF10") {
						    BF10post[i] <-log(bfObject$bf10)
						}
					} else if (options$hypothesis == "correlatedPositively") {
					    someLowerCi <- bfObject$ci$plusSided[1]
					    someUpperCi <- bfObject$ci$plusSided[3]
						
						if (options$bayesFactorType=="BF10") {
						    BF10post[i] <- bfObject$bfPlus0
						} else if (options$bayesFactorType == "BF01") {
							BF10post[i] <- 1/bfObject$bfPlus0
						} else if (options$bayesFactorType == "LogBF10") {
						    BF10post[i] <- log(bfObject$bfPlus0)
						}
					} else if (options$hypothesis == "correlatedNegatively") {
					    someLowerCi <- bfObject$ci$minSided[1]
						someUpperCi <- bfObject$ci$minSided[3]
						
						if (options$bayesFactorType=="BF10") {
						    BF10post[i] <- bfObject$bfMin0
						} else if (options$bayesFactorType == "BF01") {
						    BF10post[i] <- 1/bfObject$bfMin0
						} else if (options$bayesFactorType == "LogBF10") {
						    BF10post[i] <- log(bfObject$bfMin0)
						}
					}
					
					if (is.null(errorMessage) && is.infinite(BF10post[i])) {
						unplotable <- TRUE
						unplotableMessage <- "Bayes factor is infinity"
					} else if (is.null(errorMessage) && BF10post[i] == 1 / Inf) {
						unplotable <- TRUE
						unplotableMessage <- "The Bayes factor is too small"
					}
					
					if (!is.null(index)) {
					    result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], r=.clean(bfObject$stat), BF=.clean(BF10post[i]), .footnotes=list(r=list(index)), upperCI=.clean(someUpperCi), lowerCI=.clean(someLowerCi))
					} else {
						result <- list(.variable1=pair[[1]], .separator="-", .variable2=pair[[2]], r=.clean(bfObject$stat), BF=.clean(BF10post[i]), upperCI=.clean(someUpperCi), lowerCI=.clean(someLowerCi))
					}
					
					pairStatuses[[i]] <- list(ready=TRUE, error=FALSE, unplotable=unplotable, unplotableMessage=unplotableMessage, unplotableScatter=unplotableScatter, unplotableMessageScatter=unplotableMessageScatter)
				}
			}
		}
		
		correlationRows[[length(correlationRows)+1]] <- result
	}

	if (length(correlationRows) == 0)
		correlationRows <- list(list(.variable1="...", .separator="-", .variable2="...", r= "", BF=""))
	
	correlation[["data"]] <- correlationRows
	correlation[["footnotes"]] <- as.list(footnotes)
	
	results[["correlation"]] <- correlation
	
	
	# PLOTS
	
	if (perform == "run" && length(options$pairs) > 0 && (options$plotScatter || options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis)) {
	
		if ( ! .shouldContinue(callback(results)))
				return()
			
		j <- 1
		
		for (i in .indices(options$pairs)) {
			
			pair <- options$pairs[[i]]
			
			status <- pairStatuses[[i]]
			
			p1 <- ifelse(pair[[1]] != "", pair[[1]], "...") 
			p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
	
			if (perform == "run" && status$unplotable == FALSE) {
				
				subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
				subDataSet <- na.omit(subDataSet)
				
				v1 <- subDataSet[[ .v(pair[[1]]) ]]
				v2 <- subDataSet[[ .v(pair[[2]]) ]]
			
			} else {
			
				v1 <- NULL
				v2 <- NULL
			}
			
			if (perform == "run" && status$unplotableScatter == FALSE) {
				
				subDataSet2 <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
				subDataSet2 <- na.omit(subDataSet2)
				
				vs1 <- subDataSet2[[ .v(pair[[1]]) ]]
				vs2 <- subDataSet2[[ .v(pair[[2]]) ]]
			
			} else {
			
				vs1 <- NULL
				vs2 <- NULL
			}
			
			
			if (options$plotScatter) {
			
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs  && !is.null(diff) && ((is.list(diff)  && (diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$corcoefficient == FALSE))) && "plotScatter" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "plotScatter")[1]
					
					plots.correlation[[j]] <- state$plotsCorrelation[[stateIndex]]
				
				} else {
				
					results[["plots"]][["collection"]][[i]][["ScatterPlot"]][["status"]] <- "running"
					
					if ( ! .shouldContinue(callback(results)))
						return()
					
					plot <- plots.correlation[[j]]
					
					if (status$unplotableScatter == FALSE) {
						
						p <- try(silent=FALSE, expr= {
							
							# image <- .beginSaveImage(530, 400)
							# .plotScatter.Bcorrelationpairs(xVar=vs1, yVar=vs2, xlab=pair[[1]], ylab=pair[[2]])
							# plot[["data"]] <- .endSaveImage(image)
							
							.plotFunc <- function() {
								.plotScatter.Bcorrelationpairs(xVar=vs1, yVar=vs2, xlab=pair[[1]], ylab=pair[[2]])
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
					} else if (status$unplotableScatter && "unplotableMessageScatter" %in% names(status)) {
					
						errorMessage <- status$unplotableMessageScatter
						plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
					}
					
					plot[["status"]] <- "complete"
					plots.correlation[[j]] <- plot
				}
				
				
				plotGroups[[i]][["ScatterPlot"]] <- plots.correlation[[j]]
				results[["plots"]][["collection"]] <- plotGroups
				
				j <- j + 1
				
				if ( ! .shouldContinue(callback(results)))
					return()
				
			}
			
			if (options$plotPriorAndPosterior) {
			
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "posteriorPlotAddInfo")[1]
					
					plots.correlation[[j]] <- state$plotsCorrelation[[stateIndex]]
						
				} else if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
							&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && !options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# if the requested plot already exists use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "posteriorPlot")
					
					plots.correlation[[j]] <- state$plotsCorrelation[[stateIndex]]
					
				} else {
					
					results[["plots"]][["collection"]][[i]][["PriorPosteriorPlot"]][["status"]] <- "running"
					
					if ( ! .shouldContinue(callback(results)))
					 		return()
					
					plot <- plots.correlation[[j]]
					
					if (status$unplotable == FALSE) {
					
						p <- try(silent=FALSE, expr= {
						
							# image <- .beginSaveImage(530, 400)
							# 
							# .plotPosterior.correlation(r=rs[i], n=ns[i], oneSided=oneSided, BF=BF10post[i], BFH1H0=BFH1H0, addInformation=options$plotPriorAndPosteriorAdditionalInfo, kappa=options$priorWidth,corCoefficient=options$corcoefficient)
							# 
							# plot[["data"]] <- .endSaveImage(image)
							.plotFunc <- function() {
								.plotPosterior.correlation(r=rs[i], n=ns[i], oneSided=oneSided, BF=BF10post[i], BFH1H0=BFH1H0, addInformation=options$plotPriorAndPosteriorAdditionalInfo, kappa=options$priorWidth,corCoefficient=options$corcoefficient)
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
						
					
					} else if (status$unplotable && "unplotableMessage" %in% names(status)) {
					
						message <- paste("Plotting is not possible:", status$unplotableMessage)
						plot[["error"]] <- list(error="badData", errorMessage=message)
					}
					
					plot[["status"]] <- "complete"
					
					plots.correlation[[j]] <- plot
					
				}
				
				plotGroups[[i]][["PriorPosteriorPlot"]] <- plots.correlation[[j]]
				results[["plots"]][["collection"]] <- plotGroups
				
				j <- j + 1
				
				if ( ! .shouldContinue(callback(results)))
					return()
			}
			
			if (options$plotBayesFactorRobustness) {
			
			
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && "robustnessPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "robustnessPlot")[1]
					
					plots.correlation[[j]] <- state$plotsCorrelation[[stateIndex]]
						
				} else {
					
					results[["plots"]][["collection"]][[i]][["BFrobustnessPlot"]][["status"]] <- "running"
					
					if ( ! .shouldContinue(callback(results)))
					 		return()
				
					plot <- plots.correlation[[j]]
	
					if (status$unplotable == FALSE) {
					
						p <- try(silent=FALSE, expr= {
						
							# image <- .beginSaveImage(530, 400)
							# 
							# .plotBF.robustnessCheck.correlation(r=rs[i], n=ns[i], oneSided=oneSided, BF10post=BF10post[i], BFH1H0=BFH1H0, kappa=options$priorWidth, corCoefficient=options$corcoefficient)
							# 
							# plot[["data"]] <- .endSaveImage(image)
							
							.plotFunc <- function() {
								.plotBF.robustnessCheck.correlation(r=rs[i], n=ns[i], oneSided=oneSided, BF10post=BF10post[i], BFH1H0=BFH1H0, kappa=options$priorWidth, corCoefficient=options$corcoefficient)
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
						
					} else if (status$unplotable && "unplotableMessage" %in% names(status)) {
					
						message <- paste("Plotting is not possible:", status$unplotableMessage)
						plot[["error"]] <- list(error="badData", errorMessage=message)
					}
					
					plot[["status"]] <- "complete"
					
					plots.correlation[[j]] <- plot
				}
				
				plotGroups[[i]][["BFrobustnessPlot"]] <- plots.correlation[[j]]
				results[["plots"]][["collection"]] <- plotGroups
				
				j <- j + 1
				
				if ( ! .shouldContinue(callback(results)))
					return()
			}
			
			
			if (options$plotSequentialAnalysis) {
			
				if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
					&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "sequentialRobustnessPlot")[1]
					
					plots.correlation[[j]] <- state$plotsCorrelation[[stateIndex]]
						
				} else if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$corcoefficient == FALSE
							&& diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)) && !options$plotSequentialAnalysisRobustness  && "sequentialPlot" %in% state$plotTypes) {
					
					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# if the requested plot already exists use it
					
					stateIndex <- which(state$plotPairs == tablePairs[[i]] & state$plotTypes == "sequentialPlot")[1]
					
					plots.correlation[[j]] <- state$plotsCorrelation[[stateIndex]]
					
				} else {
					
					results[["plots"]][["collection"]][[i]][["BFsequentialPlot"]][["status"]] <- "running"
					
					if ( ! .shouldContinue(callback(results)))
					 		return()
					
					plot <- plots.correlation[[j]]
					
					if (status$unplotable == FALSE) {
						p <- try(silent=FALSE, expr= {
						
							# image <- .beginSaveImage(530, 400)
							# 
							# .plotSequentialBF.correlation(x=v1, y=v2, oneSided=oneSided, BF=BF10post[i], BFH1H0=BFH1H0, kappa=options$priorWidth,corCoefficient=options$corcoefficient)
							# 
							# plot[["data"]] <- .endSaveImage(image)
							
							.plotFunc <- function() {
								.plotSequentialBF.correlation(x=v1, y=v2, oneSided=oneSided, BF=BF10post[i], BFH1H0=BFH1H0, kappa=options$priorWidth,corCoefficient=options$corcoefficient)
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
						
					
					} else if (status$unplotable && "unplotableMessage" %in% names(status)) {
					
						message <- paste("Plotting is not possible:", status$unplotableMessage)
						plot[["error"]] <- list(error="badData", errorMessage=message)
					}
					
					plot[["status"]] <- "complete"
					
					plots.correlation[[j]] <- plot
				}
				
				plotGroups[[i]][["BFsequentialPlot"]] <- plots.correlation[[j]]
				results[["plots"]][["collection"]] <- plotGroups
				
				j <- j + 1
				
				
				if ( ! .shouldContinue(callback(results)))
					return()
			}
			
		}
	}
	
	keep <- NULL
	
	for (plot in plots.correlation)
		keep <- c(keep, plot$data)
	  
	if (perform == "init") {
		
		return(list(results=results, status="inited", state=state, keep=keep))
		
	} else {
	
		return(list(results=results, status="complete", state=list(options=options, results=results, plotsCorrelation=plots.correlation, plotTypes=plotTypes, plotPairs=plotPairs,
		pairStatuses=pairStatuses, BF10post=BF10post, tablePairs=tablePairs, errorFootnotes=errorFootnotes, ns=ns, rs=rs), keep=keep))
	}
}


.plotScatter.Bcorrelationpairs <- function(xVar=NULL, yVar=NULL, xlab, ylab, dontPlotData=FALSE, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2, lwdAxis=1.2) {
	
	op <- par(mar= c(5.6, 7, 4, 4) + 0.1, las=1, xpd=FALSE)
	
	if (dontPlotData) {
		
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexXAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexYAxis, lwd=lwdAxis, ylab="")
		mtext(text = xlab, side = 1, cex=1.5, line = 2.9)
		mtext(text = ylab, side = 2, cex=1.5, line = 3.25, las=0)
		
		return()
	}
	
	d <- data.frame(xx= xVar, yy= yVar)
	d <- na.omit(d)
	xVar <- d$xx
	yVar <- d$yy
	
	fit <- lm(yy ~ xx, data=d)
	
	xlow <- min((min(xVar) - 0.1* min(xVar)), min(pretty(xVar)))
	xhigh <- max((max(xVar) + 0.1* max(xVar)), max(pretty(xVar)))
	xticks <- pretty(c(xlow, xhigh))
	ylow <- min((min(yVar) - 0.1* min(yVar)), min(pretty(yVar)), min(.poly.pred(fit, line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
	yhigh <- max((max(yVar) + 0.1* max(yVar)), max(pretty(yVar)), max(.poly.pred(fit, line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
	
	yticks <- pretty(c(ylow, yhigh))
	
	yLabs <- vector("character", length(yticks))
	
	for (i in seq_along(yticks)) {
		
		if (yticks[i] < 10^6) {
			
			yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
			
		} else {
			
			yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
		}
	}
	
	plot(xVar, yVar, col="black", pch=21, bg = "grey", ylab="", xlab="", axes=F, ylim= range(yticks), xlim= range(xticks), cex= cexPoints)
	.poly.pred(fit, line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)
	
	par(las=1)
	
	axis(1, line= 0.4, labels= xticks, at= xticks, cex.axis= cexXAxis, lwd=lwdAxis)
	axis(2, line= 0.2, labels= yLabs, at= yticks, cex.axis= cexYAxis, lwd=lwdAxis)
	
	maxYlab <- max(nchar(yLabs))
	distLab <- maxYlab / 1.8
	mtext(text = xlab, side = 1, cex=1.5, line = 2.9)
	mtext(text = ylab, side = 2, cex=1.5, line = distLab + 2.1, las=0)
	
	par(op)
	
}

.plotPosterior.correlation <- function(n, r, kappa=1, oneSided= FALSE, BF, BFH1H0, addInformation= TRUE, dontPlotData=FALSE, lwd= 2,corCoefficient="Pearson",
										cexPoints= 1.5, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.5, cexTextBF= 1.4, cexCI= 1.1, cexLegend= 1.2, lwdAxis= 1.2) {
	
  useKendall <- corCoefficient == "Kendall"
  usePearson <- corCoefficient == "Pearson"
  
	if (addInformation) {
	
		par(mar= c(5.6, 5, 7, 4) + 0.1, las=1)
		drawCI <- TRUE
	
	} else {
	
		par(mar= c(5.6, 5, 4, 4) + 0.1, las=1)
		drawCI <- FALSE
	}
	
	
	if (dontPlotData) {
	
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
		mtext(expression("Population correlation" ~ rho), side = 1, cex = cexXlab, line= 2.6)
	
		return()
	}
	
	# set limits plot
	xlim <- c(-1, 1)
	
	if (oneSided == FALSE) {
		stretch <- 1.2
	}
	
	if (oneSided == "right") {
		stretch <- 1.32
	}
	
	if (oneSided == "left") {
		stretch <- 1.32
	}
	
	# calculate position of "nice" tick marks and create labels
	xticks <- seq(-1.0, 1.0, 0.25)
	xlabels <- c("-1", "-0.75", "-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1")
	
	# compute 95% credible interval & median:
	if (usePearson) {
  	someFit <- .posteriorBetaParameters(n=n, r=r, kappa=kappa)
  	betaA <- someFit$betaA
  	betaB <- someFit$betaB
  	ci <- .computePearsonCredibleInterval(betaA, betaB, .95)

  	if (oneSided == FALSE) {
  		CIlow <- ci$twoSided[1]
  		medianPosterior <- ci$twoSided[2]
  		CIhigh <- ci$twoSided[3]
  	} else if (oneSided == "right") {
  		CIlow <- ci$plusSided[1]
  		medianPosterior <- ci$plusSided[2]
  		CIhigh <- ci$plusSided[3]
  	} else if (oneSided == "left") {
  		CIlow <- ci$minSided[1]
  		medianPosterior <- ci$minSided[2]
  		CIhigh <- ci$minSided[3]
  	}
  	
  	if (any(is.na(c(CIlow, medianPosterior, CIhigh))))
  		drawCI <- FALSE
  	
  	rho <- seq(-0.99, 0.99, length.out = 1000)
  	
  	betaApproximation <- FALSE
  	
  	if (oneSided == FALSE) {
  		priorLine <- .priorRho(rho=rho, kappa=kappa)
  		posteriorLine <- .posteriorRho(rho=rho, n=n, r=r, kappa=kappa)
  		
  		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
  			
  			betaApproximation <- TRUE
  			
  			if (any(is.na(c(betaA, betaB))))
  				stop("Posterior is too peaked")
  			
  			posteriorLine <- .stretchedBeta(alpha=betaA, beta=betaB, rho=rho)
  			
  			if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine)))
  				stop("Posterior is too peaked")
  		}
  	
  	} else if (oneSided == "right") {
  	
  		priorLine <- .priorRhoPlus(rho=rho, kappa=kappa)
  		posteriorLine <- .posteriorRhoPlus(rho=rho, n=n, r=r, kappa= kappa)
  		
  		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
  		
  			betaApproximation <- TRUE
  			
  			if (any(is.na(c(betaA, betaB))))
  				stop("Posterior is too peaked")
  			
  			posteriorLine <- .stretchedBeta(alpha=betaA, beta=betaB, rho=rho) / pbeta(1/2,  betaA, betaB, lower.tail=FALSE)
  			posteriorLine[rho < 0] <- 0
  			
  			if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine)))
  				stop("Posterior is too peaked")
  				
  		}
  		
  		
  	} else if (oneSided == "left") {
  	
  		priorLine <- .priorRhoMin(rho=rho, kappa=kappa)
  		posteriorLine <- .posteriorRhoMin(rho=rho, n=n, r=r, kappa=kappa)
  		
  		if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
  		
  			betaApproximation <- TRUE
  			
  			if (any(is.na(c(betaA, betaB))))
  				stop("Posterior is too peaked")
  			
  			posteriorLine <- .stretchedBeta(alpha=betaA, beta=betaB, rho=rho) / pbeta(1/2,  betaA, betaB, lower.tail=TRUE)
  			posteriorLine[rho > 0] <- 0
  			
  			if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine)))
  				stop("Posterior is too peaked")
  				
  		}
  	}
	}
	if (useKendall) {
	  betaApproximation <- FALSE
	    if (oneSided == FALSE) {
	    ci <- .credibleIntervalKendallTau(n=n, tauObs=r, ciValue = 0.95, kappa=kappa, test="two-sided")
	    CIlow <- ci[[1]]
	    medianPosterior <- ci[[2]]
	    CIhigh <- ci[[3]]
	    
	  } else if (oneSided == "right") {
	    ci <- .credibleIntervalKendallTau(n=n, tauObs=r, ciValue = 0.95, kappa=kappa, test="positive")
	    CIlow <- ci[[1]]
	    medianPosterior <- ci[[2]]
	    CIhigh <- ci[[3]]
	    
	  } else if (oneSided == "left") {
	    ci <- .credibleIntervalKendallTau(n=n, tauObs=r, var = 1, ciValue = 0.95, kappa=kappa, test="negative")
	    CIlow <- ci[[1]]
	    medianPosterior <- ci[[2]]
	    CIhigh <- ci[[3]]
	  }
	  
	  if (any(is.na(c(CIlow, medianPosterior, CIhigh)))) {
	    drawCI <- FALSE
	    }
	  
	  rho <- seq(-0.99, 0.99, length.out = 1000)
	  
	  if (oneSided == FALSE) {
	    
	    priorLine <- .priorTau(tauPop=rho, kappa=kappa)
	    posteriorLine <- .posteriorTau(n=n, tauObs=r, tauPop=rho, kappa=kappa, var=1, test="two-sided" )

	    if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
        stop("Posterior is too peaked")
	    }
	    
	  } else if (oneSided == "right") {
	    
	    priorLine <- .priorTauPlus(tauPop= rho, kappa=kappa)
	    posteriorLine <- .posteriorTau(n=n, tauObs=r, tauPop=rho, kappa=kappa, var=1, test="positive" )
	    
	    if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
	      stop("Posterior is too peaked")
	    }
	  } else if (oneSided == "left") {
	    priorLine <- .priorTauMin(tauPop=rho, kappa=kappa)
	    posteriorLine <- .posteriorTau(n=n, tauObs=r, tauPop=rho, kappa=kappa, var=1, test="negative" )
	    if (sum(is.na(posteriorLine)) > 1 || any(posteriorLine < 0) || any(is.infinite(posteriorLine))) {
	      stop("Posterior is too peaked")
	    }
	  }
	}
	
	
	dmax <- max(c(posteriorLine, priorLine))
		
	ylim <- vector("numeric", 2)
	ylim[1] <- 0
	ylim[2] <- stretch * dmax
	
	yticks <- pretty(ylim)
	
	ylim <- range(yticks)
	ylabels <- formatC(yticks, 1, format= "f")
	
	
	plot(1, 1, xlim=xlim, ylim=range(yticks), ylab= "", xlab="", type= "n", axes= FALSE)
	
	lines(rho, posteriorLine, lwd= lwd)
	lines(rho, priorLine, lwd= lwd, lty=3)
	
	axis(1, at=xticks, labels=xlabels, cex.axis=cexAxis, lwd=lwdAxis)
	axis(2, at=yticks, labels=ylabels, , cex.axis=cexAxis, lwd=lwdAxis)
	
	
	if (nchar(ylabels[length(ylabels)]) > 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 4)
		
	} else if (nchar(ylabels[length(ylabels)]) == 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 3.25)
		
	} else if (nchar(ylabels[length(ylabels)]) < 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line= 2.85)
	}
	
	if (usePearson) {
	  mtext(expression("Population correlation" ~ rho), side = 1, cex = cexXlab, line= 2.6)
	} else { 
	    mtext(expression("Population correlation" ~ tau), side = 1, cex = cexXlab, line= 2.6)
	}
	
	
	evalPosterior <- posteriorLine[posteriorLine > 0]
	
	if (oneSided == "right") {
	    heightPosteriorAtZero <- evalPosterior[1]
	    
		if (usePearson) {
		  points(0, .priorRhoPlus(rho=0, kappa=kappa), col="black", pch=21, bg = "grey", cex=cexPoints)
		} else if (useKendall) {
		  points(0, .priorTauPlus(tauPop=0, kappa=kappa), col="black", pch=21, bg = "grey", cex=cexPoints)
		}
		points(0, heightPosteriorAtZero, col="black", pch=21, bg = "grey", cex= cexPoints)
		
	} else if (oneSided == "left") {
		heightPosteriorAtZero <- evalPosterior[length(evalPosterior)]
		if (usePearson) {
		  points(0, .priorRhoMin(rho=0, kappa=kappa), col="black", pch=21, bg = "grey", cex=cexPoints)
		} else if (useKendall) {
		  points(0, .priorTauMin(tauPop=0, kappa=kappa), col="black", pch=21, bg="grey", cex=cexPoints)
		}		
		points(1e-15, heightPosteriorAtZero, col="black", pch=21, bg="grey", cex=cexPoints)
		
	} else {
		if (betaApproximation && usePearson) {
		  points(0, .priorRho(rho=0, kappa=kappa), col="black", pch=21, bg = "grey", cex= cexPoints)
			points(0, .stretchedBeta(alpha=betaA, beta=betaB, rho=0), col="black", pch=21, bg="grey", cex=cexPoints)
		} else if (usePearson) {
		    # Quentin: Will we ever get here?
		    points(0, .priorRho(rho=0, kappa=kappa), col="black", pch=21, bg="grey", cex=cexPoints)
		    points(1e-15, .posteriorRho(rho=1e-8, n=n, r=r, kappa=kappa), col="black", pch=21, bg="grey", cex=cexPoints)
		} else if (useKendall) {
		    points(1e-15, .posteriorTau(n=n, tauObs=r, tauPop=1e-8, kappa=kappa), col="black", pch=21, bg = "grey", cex= cexPoints)
		    points(0, .priorTau(tauPop=0, kappa=kappa), col="black", pch=21, bg="grey", cex=cexPoints)
		}
	}
	
	
	# enable plotting in margin
	par(xpd=TRUE)

	yCI <- grconvertY(dmax, "user", "ndc") + 0.04
	yCI <- grconvertY(yCI, "ndc", "user")
	
	if (drawCI) {
		arrows(CIlow, yCI, CIhigh, yCI, angle=90, code=3, length= 0.1, lwd= lwd)
		medianText <- formatC(medianPosterior, digits=3, format="f")
	}
	
	if (addInformation) {
		if (BFH1H0) {
			BF10 <- BF
			BF01 <- 1 / BF10
		} else {
			BF01 <- BF
			BF10 <- 1 / BF01
		}
		
		# display BF10 value
		offsetTopPart <- 0.06
		
		yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
		
		xx <- min(xticks)
		
		if (BF10 >= 1000000 | BF01 >= 1000000) {
			BF10t <- format(BF10, digits= 4, scientific = TRUE)
			BF01t <- format(BF01, digits= 4, scientific = TRUE)
		}
		
		if (BF10 < 1000000 & BF01 < 1000000) {
		    BF10t <- formatC(BF10,3, format = "f")
			BF01t <- formatC(BF01,3, format = "f")
		}
		
		if (oneSided == FALSE) {
		    text(xx, yy2, bquote(BF[10]==.(BF10t)), cex=cexTextBF, pos=4)
			text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos=4)
		}
		
		if (oneSided == "right") {
		    text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex=cexTextBF, pos=4)
			text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex=cexTextBF, pos=4)
		}
		
		if (oneSided=="left") {
		    text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex=cexTextBF, pos=4)
			text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex=cexTextBF, pos=4)
		}
		
		yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
		
		if (drawCI) {
			CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow,3, format="f"))), ", ",  bquote(.(formatC(CIhigh,3, format="f"))), "]", sep="")
			medianLegendText <- paste("median =", medianText)
			
			text(max(xticks) , yy2, medianLegendText, cex= 1.1, pos= 2)
			text(max(xticks) , yy, CIText, cex= 1.1, pos= 2)
		}
		
		
		# probability wheel
		if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
			xx <- grconvertX(0.44, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 5) {
			xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 6) {
			xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 7) {
			xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) == 8) {
			xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		if (max(nchar(BF10t), nchar(BF01t)) > 8) {
			xx <- grconvertX(0.44 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
		
		# make sure that colored area is centered
		radius <- 0.06 * diff(range(xticks))
		A <- radius^2 * pi
		alpha <- 2 / (BF01 + 1) * A / radius^2
		startpos <- pi/2 - alpha/2
		
		# draw probability wheel
		plotrix::floating.pie(xx, yy,c(BF10, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)
		
		yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
		
		if (oneSided == FALSE) {
			text(xx, yy, "data|H1", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "right") {
			text(xx, yy, "data|H+", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
		
		if (oneSided == "left") {
			text(xx, yy, "data|H-", cex= cexCI)
			text(xx, yy2, "data|H0", cex= cexCI)
		}
	}
	
	if (oneSided == "right") {
		legendPosition <- min(xticks)
 		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 0, yjust= 1)
	} else if (oneSided == "left") {
		legendPosition <- max(xticks)
 		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 1, yjust= 1)
	} else if (oneSided == FALSE) {
	    if (r >= 0) {
	        legendPosition <- min(xticks)
			legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 0, yjust= 1)
		} else {
			legendPosition <- max(xticks)
			legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty=c(1,3), bty= "n", lwd = c(lwd,lwd), cex= cexLegend, xjust= 1, yjust= 1)	
		}
	}
}

.makeKappas <- function(n) {
	someKappas <- sin(seq(1.5*pi, 2*pi, length=n))+1
	someKappas[1] <- someKappas[2]/10
	someKappas[n] <- 1
	someKappas <- 2*someKappas
	
	return(someKappas)
}

.plotBF.robustnessCheck.correlation <- function(r=NULL, n=NULL, paired=FALSE, BF10post=NULL, kappa=1, callback=function(...) 0, oneSided=FALSE, lwd=2, cexPoints=1.4, cexAxis=1.2,
                                                cexYXlab= 1.5, cexText=1.2, cexLegend= 1.4, lwdAxis=1.2, cexEvidence=1.6, BFH1H0 =TRUE, dontPlotData=FALSE, corCoefficient="Pearson") { 
    useKendall <- corCoefficient == "Kendall"
    usePearson <- corCoefficient == "Pearson"
  
	par(mar=c(5, 6, 4, 7) + 0.1, las=1)

	if (dontPlotData) {
	    plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
	    
	    axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		if (oneSided == FALSE) {
		    if (BFH1H0) {
		        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
		    } else {
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "right") {
			if (BFH1H0) {
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			} else {
			    mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "left") {
		    if (BFH1H0) {
		        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			} else {
			    mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		mtext("Stretched beta prior width", side = 1, cex = cexYXlab, line= 2.5)
		return()
	}
	
	#### get BFs ###
	kappaValues <- .makeKappas(50)
	
	# BF10
	BF10 <- vector("numeric", length(kappaValues))
	BF10[1] <- 1 # set first one manually to one
	
	
	for (i in seq_along(kappaValues)[-1]) {
	    if (usePearson) {
	        bfObject <- .bfPearsonCorrelation(n=n, r=r, kappa=kappaValues[i], ciValue=NULL)
	    } else if (useKendall) {
	        # TODO (Johnny): I removed var=1, because it's done by default already, 
	        bfObject <- .bfCorrieKernelKendallTau(n=n, tauObs=r, kappa=kappaValues[i], var=1, ciValue=NULL)
	    }
	    
		if (oneSided == FALSE) {
		    if (bfObject$bf10 == 0) {
		        bfObject$bf10 <- 1
		    }
			BF10[i] <- bfObject$bf10
		} else if (oneSided == "right") {
			if (is.na(bfObject$bfPlus0)) {
			    bfObject$bfPlus0 <- 1
			}
			BF10[i] <- bfObject$bfPlus0
		} else if (oneSided == "left") {
			if (is.na(bfObject$bfMin0)) {
			    bfObject$bfMin0 <- 1
			}
		    BF10[i] <- bfObject$bfMin0
		}
		
		
		if (is.na(BF10[i])) {
		    stop("One or more Bayes factors cannot be computed")
		}

		if (is.infinite(BF10[i])) {
		    stop("One or more Bayes factors are infinity")
		}
	}
	
	
	# BF10 user prior
	BF10user <- BF10post 
	BF10userText <- BF10user
	
	if ( ! .shouldContinue(callback()))
		return()
	
	####################### scale y axis ###########################
	
	BF <- c(BF10, BF10user)
	if (!BFH1H0) {
	    BF <- 1 / BF
		BF10 <- 1 / BF10
	}
	
	# y-axis labels larger than 1
	y1h <- "1"
	i <- 1
	
	while (eval(parse(text= y1h[i])) < max(BF10)) {
		if (grepl(pattern = "e",y1h[i])) {
		    newy <- paste(strsplit(y1h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
		    newy <- paste(y1h[i], "0", sep= "")
		}
		
		if (eval(parse(text=newy)) >= 10^6) {
			newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
		}
		
		y1h <- c(y1h, newy)
		i <- i + 1
	}
	
	y3h <- "3"
	i <- 1
	
	while (eval(parse(text= y3h[i])) < max(BF10)) {
		if (grepl(pattern = "e",y3h[i])) {
		    newy <- paste(strsplit(y3h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
		    newy <- paste(y3h[i], "0", sep= "")
		}
		
		if (as.numeric(newy) >= 10^6) {
			newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
		}
		
		y3h <- c(y3h, newy)
		i <- i + 1
	}
	
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
	
	while (eval(parse(text= y1l[i])) > min(BF10)) {
		if (grepl(pattern = "e",y1l[i])) {
			newy <- paste(strsplit(y1l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
			newy <- paste(y1l[i], "0", sep= "")
		}
		
		if (eval(parse(text= newy)) <= 10^(-6)) {
			newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}
		
		y1l <- c(y1l, newy)
		i <- i + 1
	}
	
	y3l <- "1/3"
	i <- 1
	
	while (eval(parse(text= y3l[i])) > min(BF10)) {
		if (grepl(pattern = "e",y3l[i])) {
			newy <- paste(strsplit(y3l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
		    newy <- paste(y3l[i], "0", sep= "")
		}
		
		if (newy == "1/3e+9") {
			newy <- "1/3e+09"
		}	
		
		if (eval(parse(text= newy)) <= 10^(-6) & eval(parse(text= newy)) > 10^(-9)) {
			newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
			newy <- paste(substring(newy, 1, nchar(newy)-1), as.numeric(substring(newy, nchar(newy), nchar(newy)))-1,sep="")
			newy <- sub(".33", "", newy)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}
		
		y3l <- c(y3l, newy)
		i <- i + 1
	}
	
	ylow <- vector("numeric", length(y1l) + length(y3l))
	o <- 1
	e <- 1
	
	if ( ! .shouldContinue(callback()))
		return()
	
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
			yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh),2)]
		} else {
			yLabsHigh <- character(0)
		}
		
		yLabsLow <- yLab[1:(ind-1)]
		yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]
		yLab1s <- c(yLabsLow, yLabsHigh)
		
		if (max(BF10) > eval(parse(text= yLab1s[length(yLab1s)]))) {
			for (i in 1:2) {
				if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {
					newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
					split = "+", fixed=TRUE)[[1]][2])+1, sep="")
				} else {
				    newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
				}
				
				if (eval(parse(text=newy)) >= 10^6) {
					newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
				}
				
				yLab1s <- c(yLab1s, newy)
			}
		}
		
		if (max(BF10) > eval(parse(text= yLab1s[length(yLab1s)-1]))) {
			if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {
			    newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
				split = "+", fixed=TRUE)[[1]][2])+1, sep="")
			} else {
				newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
			}
			
			if (eval(parse(text=newy)) >= 10^6) {
				newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
			}
			
			yLab1s <- c(yLab1s, newy)
		}
		
		if (yLab1s[1] == "1") {
			yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
		}
		
		if (yLab1s[length(yLab1s)] == "1") {
			yLab1s <- c(yLab1s, "10")
		}
		
		if (min(BF10) < eval(parse(text= yLab1s[1]))) {
			for (i in 1:2) {
				if (grepl(pattern = "e",yLab1s[1])) {
					newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
				} else {
					newy <- paste(yLab1s[1], "0", sep= "")
				}
				
				if (eval(parse(text= newy)) <= 10^(-6)) {
					newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
					newy <-  sub("-", "+", x = newy)
					newy <- substring(newy, nchar(newy)-4, nchar(newy))
					newy <- paste0("1/", newy)
				}
			}
			
			yLab1s <- c(newy, yLab1s)
		}
		
		if (min(BF10) < eval(parse(text= yLab1s[2]))) {
			if (grepl(pattern = "e",yLab1s[1])) {
			    newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
			} else {
			    newy <- paste(yLab1s[1], "0", sep= "")
			}
				
			if (eval(parse(text= newy)) <= 10^(-6)) {
				newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
				newy <-  sub("-", "+", x = newy)
				newy <- substring(newy, nchar(newy)-4, nchar(newy))
				newy <- paste0("1/", newy)
			}
			
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
			yLabLow <- paste("1/", yLabHigh[1], sep="")
		}
		
		if (length(yLabHigh) == 1) {
			yLabHigh <- strsplit(x = yLabLow[1], "/", fixed=TRUE)[[1]][2]
		}
		
		yLab <- c(rev(yLabLow), "1", yLabHigh)
	}
	
	
	if ( ! .shouldContinue(callback())) {
	    return()
	}
	
	while (eval(parse(text=yLab[2])) > min(BF10)) {
	    interval <- as.numeric(strsplit(format(eval(parse(text=yLab[1])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(format(eval(parse(text=yLab[2])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2])
		pot <- as.numeric(strsplit(format(eval(parse(text=yLab[1])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2]) + interval
		
		if (nchar(pot) == 1) {
		    pot <- paste("0", pot, sep="")
		}
		
		newy <- paste("1/1e", "+", pot, sep="")
		yLab <- c(newy, yLab)
	}
	
	while (eval(parse(text=yLab[length(yLab)-1])) < max(BF10)) {
	    interval <- as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)-1])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2])
		pot <- as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2]) + interval
		
		if (nchar(pot) == 1) {
		    pot <- paste("0", pot, sep="")
		}
		
		newy <- paste(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
		yLab <- c( yLab, newy)
	}
	
	
	yAt <- vector("numeric", length(yLab))
	for (i in seq_along(yLab)) {
	    yAt[i] <- log(eval(parse(text= yLab[i])))
	}
	
	
	####################### plot ###########################
	xLab <- pretty(range(kappaValues))
	xlim <- range(xLab)
	ylow <- log(eval(parse(text= yLab[1])))
	yhigh <- log(eval(parse(text= yLab[length(yLab)])))
	ylim <- c(ylow, yhigh)
	
	plot(1, 1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
	
	
	for (i in seq_along(yAt)) {
		lines(x=xlim, y=rep(yAt[i], 2), col='darkgrey', lwd= 1.3, lty=2)
	}
	
	lines(xlim, rep(0, 2), lwd= lwd)
	axis(1, at= xLab, labels = xLab, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yAt, labels= yLab, cex.axis= cexAxis, lwd= lwdAxis)
	
	# enable plotting in margin
	par(xpd= TRUE)
	xx <- grconvertX(0.79, "ndc", "user")
	
	yAthigh <- yAt[yAt >= 0]
	
	if (!omit3s & eval(parse(text= yLab[1])) >= 1/300 & eval(parse(text= yLab[length(yLab)])) <= 300) {
	    for (i in 1:(length(yAthigh)-1)) {
			yy <- mean(c(yAthigh[i], yAthigh[i+1]))
			
			if (yAthigh[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
			}
			
			if (yAthigh[i] == log(3)) {
				text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
			}
			
			if (yAthigh[i] == log(10)) {
				text(x = xx, yy,"Strong", pos= 4, cex= cexText)
			}
			
			if (yAthigh[i] == log(30)) {
				text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
			}
			
			if (yAthigh[i] == log(100)) {
				text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
			}
		}
		
		yAtlow <- rev(yAt[yAt <= 0])
		
		for (i in 1:(length(yAtlow)-1)) {
		    yy <- mean(c(yAtlow[i], yAtlow[i+1]))
			
			if (yAtlow[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/3)) {
				text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/10)) {
				text(x = xx, yy,"Strong", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/30)) {
				text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/100)) {
				text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
			}
		}
		
		axis(side=4, at= yAt,tick=TRUE,las=2, cex.axis= cexAxis, lwd= lwdAxis, labels=FALSE, line= -0.6)
		
		xx <- grconvertX(0.96, "ndc", "user")
		yy <- grconvertY(0.5, "npc", "user")
		text(xx, yy, "Evidence", srt= -90, cex= cexEvidence)
	}
	
	if (omit3s) {
	    if (eval(parse(text= yLab[1])) <= 1/10^6) {
	        line <- 4.75
	    } else {
	        line <- 4.3
		}
		
		if (oneSided == FALSE) {
			if (BFH1H0) {
			    mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= line)
			} else {
			    mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= line)
			}
		}
		
		if (oneSided == "right") {
			if (BFH1H0) {
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= line)
			} else {
			    mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= line)
			}
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= line)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= line)
			}
		}
	}
	
	if (omit3s == FALSE) {
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
			}
		}
	}
	
	mtext("Stretched beta prior width", side = 1, cex = cexYXlab, line= 2.5)
	
	xx <- grconvertX(0.1, "npc", "user")
	yy1 <- yAt[length(yAt)-1]
	yy2 <- yAt[length(yAt)]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4* diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
	
	xxt <- grconvertX(0.28, "npc", "user")
	
	if (oneSided == FALSE) {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
	}
	
	if (oneSided == "right") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
	}
	
	if (oneSided == "left") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
	}
	
	yy1 <- yAt[2]
	yy2 <- yAt[1]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
	
	if (oneSided == FALSE) {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
		}
	}
	
	if (oneSided == "right") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
		}
	}
	
	if (oneSided == "left") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
		}
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	
	# display BF10
	lines(kappaValues, log(BF10), col="black", lwd = 2.7)
	
	# display user prior BF
	points(kappa, log(BF10user), pch=21, bg="grey", cex= cexPoints, lwd = 1.3)
	
	#### add legend
	
	# BFuser
	
	if (BFH1H0) {
	
		BF01userText <- 1 / BF10userText
		
	} else {
	
		BF10userText <- 1 / BF10userText
		BF01userText <- 1 / BF10userText
	}
	
	if (BF10userText >= 1000000 | BF01userText >= 1000000) {
	
		BF10usert <- format(BF10userText, digits= 4, scientific = TRUE)
		BF01usert <- format(BF01userText, digits= 4, scientific = TRUE)
	}
	if (BF10userText < 1000000 & BF01userText < 1000000) {
	
		BF10usert <- formatC(BF10userText, 3, format = "f")
		BF01usert <- formatC(BF01userText, 3, format = "f")
	}
	
	if (oneSided == FALSE) {
	
		if ( BF10userText >= BF01userText) {
			userBF <- bquote(BF[10]==.(BF10usert))
		} else {
			userBF <- bquote(BF[0][1]==.(BF01usert))
		}
	}
	if (oneSided == "right") {
	
		if (BF10userText >= BF01userText) {
			userBF <- bquote(BF["+"][0]==.(BF10usert))
		} else {
			userBF <- bquote(BF[0]["+"]==.(BF01usert))
		}
	}
	if (oneSided == "left") {
	
		if (BF10userText >= BF01userText) {
			userBF <- bquote(BF["-"][0]==.(BF10usert))
		} else {
			userBF <- bquote(BF[0]["-"]==.(BF01usert))
		}
	}
	
	
	xx <- grconvertX(0.26, "ndc", "user")
	yy <- grconvertY(0.952, "ndc", "user")
	
	pt.bg <-  c("grey", "white", "black")
	pt.cex <-  c(cexPoints, 1.1, 1.1)
	
	legend(xx, yy, legend = "user prior:", pch=21, pt.bg= "grey", bty= "n", cex= cexLegend, lty=rep(NULL,3), pt.lwd=1.3, pt.cex=cexPoints)
	
	xx <- grconvertX(0.46, "ndc", "user")
	yy <- grconvertY(0.892, "ndc", "user")
	
	text(xx, yy, userBF, cex= 1.3, pos = 4)
}


.plotSequentialBF.correlation <- function(x= NULL, y= NULL, BF10post, kappa=1, callback=function(...) 0, oneSided= FALSE, lwd= 2, cexPoints= 1.4, cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.6,
 cexTextBF= 1.4, cexText=1.2, cexLegend= 1.2, cexEvidence= 1.6,	lwdAxis= 1.2, plotDifferentPriors= FALSE, BFH1H0= TRUE, dontPlotData= FALSE,corCoefficient="Pearson") {
	
	#### settings ####
  useKendall <- corCoefficient == "Kendall"
  usePearson <- corCoefficient == "Pearson"
  
	if (!plotDifferentPriors) {
		
		evidenceText <-  TRUE
	} else {
		
		evidenceText <-  FALSE
	}
	
	
	par(mar= c(5.6, 6, 7, 7) + 0.1, las=1)
	
	if (dontPlotData) {
		
		plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")
		
		axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
		axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")
		
		mtext("n", side = 1, cex = cexXlab, line= 2.5)
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		return()
	}
	
	BF10 <- vector("numeric", length(x))
	
	for (i in seq_along(x)) {
		
		if (i == 1) {
			
			BF10[i] <- 1
			
		} else if (sd(x[1:i]) == 0 || sd(y[1:i]) == 0) {
		
			BF10[i] <- 1
		
		} else {
		
			nObs <- i
			
			if (usePearson) {
			    rObs <- cor(x[1:i], y[1:i])
			    bfObject <- .bfPearsonCorrelation(n=nObs, r=rObs, kappa=kappa, ciValue=NULL)
			} else if (useKendall) {
			    rObs <- cor(x[1:i], y[1:i], method="kendall")
			    # TODO Johnny: I removed var=1 as it's default
			    bfObject <- .bfKendallTau(n=nObs, tauObs=rObs, kappa=kappa, ciValue=NULL)
			}
			
			if (oneSided == FALSE) {
				
				BF10[i] <- bfObject$bf10
				
			} else if (oneSided == "right") {
			
				BF10[i] <- bfObject$bfPlus0
				
			} else if (oneSided == "left") {
			
				BF10[i] <- bfObject$bfMin0
			}
			
			if (is.na(BF10[i]))
				stop("One or more Bayes factors cannot be computed")
			
			if (is.infinite(BF10[i]))
				stop("One or more Bayes factors are infinity")
		}
	}
	
	
	if (BFH1H0) {
	
		BF <- BF10
		
	} else {
	
		BF <- 1 / BF10
	}
	
	
	# y-axis labels larger than 1
	
	y1h <- "1"
	
	i <- 1
	
	while (eval(parse(text= y1h[i])) < max(BF)) {
		
		if (grepl(pattern = "e",y1h[i])) {
			
			newy <- paste(strsplit(y1h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
			
			newy <- paste(y1h[i], "0", sep= "")
		}
		
		if (eval(parse(text=newy)) >= 10^6) {
			
			newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
		}
		
		y1h <- c(y1h, newy)
		i <- i + 1
	}
	
	
	y3h <- "3"
	
	i <- 1
	
	while (eval(parse(text= y3h[i])) < max(BF)) {
		
		if (grepl(pattern = "e",y3h[i])) {
			
			newy <- paste(strsplit(y3h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
			
			newy <- paste(y3h[i], "0", sep= "")
		}
		
		if (as.numeric(newy) >= 10^6) {
			
			newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
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
	
	while (eval(parse(text= y1l[i])) > min(BF)) {
		
		if (grepl(pattern = "e",y1l[i])) {
			
			newy <- paste(strsplit(y1l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
			
			newy <- paste(y1l[i], "0", sep= "")
		}
		
		if (eval(parse(text= newy)) <= 10^(-6)) {
			
			newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
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
			
			newy <- paste(strsplit(y3l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
		} else {
			
			newy <- paste(y3l[i], "0", sep= "")
		}
		
		if (newy == "1/3e+9") {
			
			newy <- "1/3e+09"
		}
		
		if (eval(parse(text= newy)) <= 10^(-6) & eval(parse(text= newy)) > 10^(-9)) {
			
			newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
			newy <- paste(substring(newy, 1, nchar(newy)-1), as.numeric(substring(newy, nchar(newy), nchar(newy)))-1,sep="")
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
			
			yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh),2)]
			
		} else {
			
			yLabsHigh <- character(0)
		}
		
		yLabsLow <- yLab[1:(ind-1)]
		yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]
		
		yLab1s <- c(yLabsLow, yLabsHigh)
		
		
		if (max(BF) > eval(parse(text= yLab1s[length(yLab1s)]))) {
			
			for (i in 1:2) {
				
				if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {
					
					newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][2])+1, sep="")
					
				} else {
					
					newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
				}
				
				if (eval(parse(text=newy)) >= 10^6) {
					
					newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
				}
				
				yLab1s <- c(yLab1s, newy)
			}
		}
		
		
		if (yLab1s[1] == "1") {
			
			yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
		}
		
		if (yLab1s[length(yLab1s)] == "1") {
			
			yLab1s <- c(yLab1s, "10")
		}
		
		if (min(BF) < eval(parse(text= yLab1s[1]))) {
			
			for (i in 1:2) {
				
				if (grepl(pattern = "e",yLab1s[1])) {
					
					newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
					
				} else {
					
					newy <- paste(yLab1s[1], "0", sep= "")
				}
				
				if (eval(parse(text= newy)) <= 10^(-6)) {
					
					newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
					newy <-  sub("-", "+", x = newy)
					newy <- substring(newy, nchar(newy)-4, nchar(newy))
					newy <- paste0("1/", newy)
				}
			}
			
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
			
			yLabLow <- paste("1/", yLabHigh[1], sep="")
		}
		
		if (length(yLabHigh) == 1) {
			
			yLabHigh <- strsplit(x = yLabLow[1], "/", fixed=TRUE)[[1]][2]
		}
		
		yLab <- c(rev(yLabLow), "1", yLabHigh)
	}
	
	
	while (eval(parse(text=yLab[1])) > min(BF)) {
		
		for (i in 1:2) {
			
			interval <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[2], "+", fixed= TRUE)[[1]][2])
			pot <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) + interval
			
			newy <- paste(strsplit(yLab[1], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
			yLab <- c(newy, yLab)
		}
	}
	
	while (eval(parse(text=yLab[length(yLab)])) < max(BF)) {
		
		for (i in 1:2) {
			
			interval <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[length(yLab)-1], "+", fixed= TRUE)[[1]][2])
			pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) + interval
			newy <- paste(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
			yLab <- c( yLab, newy)
		}
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	yAt <- vector("numeric", length(yLab))
	
	for (i in seq_along(yLab)) {
		
		yAt[i] <- log(eval(parse(text= yLab[i])))
	}
	
	####################### plot ###########################
	
	xLab <- pretty(c(0, length(BF10)+2))
	xlim <- range(xLab)
	ylow <- log(eval(parse(text= yLab[1])))
	yhigh <- log(eval(parse(text= yLab[length(yLab)])))
	
	if (is.infinite(yhigh)) {
		
		yhigh <- 1e+308
	}
	
	
	ylim <- c(ylow, yhigh)
	
	plot(1, 1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)
	
	
	for (i in seq_along(yAt)) {
		
		lines(x= xlim, y= rep(yAt[i], 2), col='darkgrey', lwd= 1.3, lty=2)
	}
	
	lines(xlim, rep(0, 2), lwd= lwd)
	
	axis(1, at= xLab, labels = xLab, cex.axis= cexAxis, lwd= lwdAxis)
	axis(2, at= yAt, labels= yLab, cex.axis= cexAxis, lwd= lwdAxis)
	
	# enable plotting in margin
	par(xpd= TRUE)
	xx <- grconvertX(0.79, "ndc", "user")
	
	yAthigh <- yAt[yAt >= 0]
	
	if (!omit3s & eval(parse(text= yLab[1])) >= 1/300 & eval(parse(text= yLab[length(yLab)])) <= 300) {
		
		for (i in 1:(length(yAthigh)-1)) {
			
			yy <- mean(c(yAthigh[i], yAthigh[i+1]))
			
			if (yAthigh[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(3)) {
				text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(10)) {
				text(x = xx, yy,"Strong", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(30)) {
				text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
			}
			if (yAthigh[i] == log(100)) {
				text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
			}
		}
		
		yAtlow <- rev(yAt[yAt <= 0])
		
		for (i in 1:(length(yAtlow)-1)) {
			
			yy <- mean(c(yAtlow[i], yAtlow[i+1]))
			
			if (yAtlow[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/3)) {
				text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/10)) {
				text(x = xx, yy,"Strong", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/30)) {
				text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
			}
			if (yAtlow[i] == log(1/100)) {
				text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
			}
		}
		
	if ( ! .shouldContinue(callback()))
				return()
		
		axis(side=4, at= yAt,tick=TRUE,las=2, cex.axis= cexAxis, lwd= lwdAxis, labels=FALSE, line= -0.6)
		
		xx <- grconvertX(0.96, "ndc", "user")
		yy <- grconvertY(0.5, "npc", "user")
		
		text(xx, yy, "Evidence", srt= -90, cex= cexEvidence)
	}
	
	if (omit3s) {
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 4.3)
			}
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 4.3)
			}
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 4.3)
			}
		}
	}
	
	if (omit3s == FALSE) {
		
		if (oneSided == FALSE) {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (oneSided == "right") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (oneSided == "left") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
	}
	
	mtext("n", side = 1, cex = cexXlab, line= 2.5)
	
	xx <- grconvertX(0.1, "npc", "user")
	yy1 <- yAt[length(yAt)-1]
	yy2 <- yAt[length(yAt)]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4* diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
	
	xxt <- grconvertX(0.28, "npc", "user")
	
	if (oneSided == FALSE) {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
	}
	
	if (oneSided == "right") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
	}
	
	if (oneSided == "left") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		}
	}
	
	
	yy1 <- yAt[2]
	yy2 <- yAt[1]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)
	
	if (oneSided == FALSE) {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex= cexText)
		}
	}
	
	if (oneSided == "right") {
		if (BFH1H0) {
		    text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
		} else {
		    text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex= cexText)
		}
	}
	
	if (oneSided == "left") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex= cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex= cexText)
		}
	}
	
	
	if (BFH1H0) {
	
		BF10e <- BF10post
		BF01e <- 1 / BF10e
		
	} else {
	
		BF01e <- BF10post
		BF10e <- 1 / BF01e
	}
	
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
	
	if (oneSided == FALSE) {
		
		text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
		text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
	}
	
	if (oneSided == "right") {
		
		text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
		text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
	}
	
	if (oneSided == "left") {
		
		text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
		text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
	}
	
	
	# probability wheel
	
	if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
		xx <- grconvertX(0.44, "ndc", "user")
	}
	
	if (max(nchar(BF10t), nchar(BF01t)) == 5) {
		xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
	}
	
	if (max(nchar(BF10t), nchar(BF01t)) == 6) {
		xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
	}
	
	if (max(nchar(BF10t), nchar(BF01t)) == 7) {
		xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
	}
	
	if (max(nchar(BF10t), nchar(BF01t)) == 8) {
		xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
	}
	
	if (max(nchar(BF10t), nchar(BF01t)) > 8) {
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
	
	plotrix::floating.pie(xx, yy,c(BF10e, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)
	
	yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
	yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
	
	if (oneSided == FALSE) {
		
		text(xx, yy, "data|H1", cex= 1.1)
		text(xx, yy2, "data|H0", cex=  1.1)
	}
	
	if (oneSided == "right") {
		
		text(xx, yy, "data|H+", cex=  1.1)
		text(xx, yy2, "data|H0", cex=  1.1)
	}
	
	if (oneSided == "left") {
		
		text(xx, yy, "data|H-", cex=  1.1)
		text(xx, yy2, "data|H0", cex=  1.1)
	}
	
	if (length(BF10) <= 60) {
		
		points(log(BF), pch=21, bg="grey", cex= cexPoints, lwd = 1.3) # user prior
		
	} else {
		
		lines(log(BF), col="black", lwd = 2.7) # user prior
	}
	
	BFevidence <- BF10e
	
	if (evidenceText) {
		
		if (BF10e < 1) {
			BFevidence <- 1 / BF10e
		}
		if (BFevidence >= 1 & BFevidence <= 3) {
			lab <- "Anecdotal"
		}
		if (BFevidence > 3 & BFevidence <= 10) {
			lab <- "Moderate"
		}
		if (BFevidence > 10 & BFevidence <= 30) {
			lab <- "Strong"
		}
		if (BFevidence > 30 & BFevidence <= 100) {
			lab <- "Very strong"
		}
		if (BFevidence > 100) {
			lab <- "Extreme"
		}
		
		xxT <- max(xLab)
		yyT <- grconvertY(0.775 + offsetTopPart, "ndc", "user")
		
		if (BF10e >= 1) {
			
			if (oneSided == FALSE) {
				text(xxT, yyT, paste("Evidence for H1:\n", lab), cex= 1.4, pos= 2, offset= -.2)
			}
			if (oneSided == "right") {
				text(xxT, yyT, paste("Evidence for H+:\n", lab), cex= 1.4, pos= 2, offset= -.2)
			}
			if (oneSided == "left") {
				text(xxT, yyT, paste("Evidence for H-:\n", lab), cex= 1.4, pos= 2, offset= -.2)
			}
		}
		
		if (BF10e < 1) {
			text(xxT, yyT, paste("Evidence for H0:\n", lab), cex= 1.4, pos= 2, offset= -.2)
		}
	}
}


