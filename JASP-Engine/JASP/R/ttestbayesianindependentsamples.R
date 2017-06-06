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

TTestBayesianIndependentSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	
	dependents <- unlist(options$variables)
	grouping   <- options$groupingVariable
	
	if (grouping == ""){
	    grouping <- NULL
	}
		
	if (is.null(dataset)) {	
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
	
	# MarkUp: General: Make container for all the results
	results <- list()
	
	# MarkUp: General: Make container for the structure 
	meta <- list()
	
	meta[[1]] <- list(name="ttest", type="table")
	meta[[2]] <- list(name="descriptives", type="object", meta=list(list(name="descriptivesTable", type="table"), list(name = "descriptivesPlots", type = "collection", meta="image")))
	meta[[3]] <- list(name="inferentialPlots", type="collection", 
	                  meta=list(name="plotGroups", type="object", 
	                            meta=list(list(name="PriorPosteriorPlot", type="image"),
	                                      list(name="BFrobustnessPlot", type="image"),
	                                      list(name="BFsequentialPlot", type="image")
	                            )
	                  )
	)
	
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Independent Samples T-Test"
	
	state <- .retrieveState()
	diff <- NULL
	
	if (!is.null(state)) {
		diff <- .diff(options, state$options)
	}
	
	ttest.results <- .ttestBayesianIndependentSamplesTTest(dataset, options, perform, state=state, diff=diff)
	
	results[["ttest"]] <- ttest.results[[1]]
	status <- ttest.results[[2]]
	g1 <- ttest.results[[3]]
	g2 <- ttest.results[[4]]
	BFH1H0 <- ttest.results[[5]]
	plottingError <- ttest.results[[6]]
	BF10post <- ttest.results[[7]]
	errorFootnotes <- ttest.results[[8]]
	
	if(is.null(options()$BFMaxModels)) options(BFMaxModels = 50000)
	if(is.null(options()$BFpretestIterations)) options(BFpretestIterations = 100)
	if(is.null(options()$BFapproxOptimizer)) options(BFapproxOptimizer = "optim")
	if(is.null(options()$BFapproxLimits)) options(BFapproxLimits = c(-15,15))
	if(is.null(options()$BFprogress)) options(BFprogress = interactive())
	if(is.null(options()$BFfactorsMax)) options(BFfactorsMax = 5)
	
	descriptivesTable <- .ttestBayesianIndependentSamplesDescriptives(dataset, options, perform)
	results[["descriptives"]] <- list(descriptivesTable = descriptivesTable, title = "Descriptives")
	
	if (options$hypothesis == "groupOneGreater") {
		oneSided <- "right"
	} else if (options$hypothesis == "groupTwoGreater") {
	    oneSided <- "left"
	} else {
		oneSided <- FALSE
	}
	
	plotGroups <- list()
	plots.ttest <- list()
	descriptPlotVariables <- list()
	descriptivesPlots <- list()
	plotTypes <- list()
	plotVariables <- list()
	
	
	
	if (options$plotPriorAndPosterior || options$plotSequentialAnalysis || options$plotBayesFactorRobustness || options$descriptivesPlots) {
		iint <- 1
		q <- 1
		descriptInd <- 1
		
		BFtype <- options$bayesFactorType
		BFtypeRequiresNewPlot <- TRUE
		
		if (!is.null(state)) {
		    BFtypeRequiresNewPlot <- FALSE
			BFtypeState <- state$options$bayesFactorType
			
			if ((BFtypeState == "LogBF10" || BFtypeState == "BF10") && BFtype == "BF01") {
				BFtypeRequiresNewPlot <- TRUE
			} else if (BFtypeState == "BF01" && (BFtype == "LogBF10" || BFtype == "BF10")) {
				BFtypeRequiresNewPlot <- TRUE
			}
		}
		
		for (variable in options[["variables"]]){
		    plotGroups[[iint]] <- list()
			plotGroups[[iint]][["title"]] <- variable
			plotGroups[[iint]][["name"]] <- variable
			
			if (options$descriptivesPlots) {
			    if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE &&
					diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {
			        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					index <- which(state$descriptPlotVariables == variable)
					descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]
			    } else {
			        descriptivesPlot <- list()
					
					descriptivesPlot[["title"]] <- variable
					descriptivesPlot[["width"]] <- options$plotWidth
					descriptivesPlot[["height"]] <- options$plotHeight
					descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")
					descriptivesPlot[["status"]] <- "waiting"
					descriptivesPlot[["data"]] <- ""
					
					descriptivesPlots[[descriptInd]] <- descriptivesPlot
				}
				
				descriptPlotVariables[[length(descriptPlotVariables)+1]] <- variable
				descriptInd <- descriptInd + 1
			}
			
			if (options$plotPriorAndPosterior){
			    if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) &&
					options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
			        # 
			        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlotAddInfo")
					plots.ttest[[q]] <- state$plotsTtest[[index]]
			    } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
							&& diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && 
							!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
			        # 
			        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# if the requested plot already exists use it
					
					index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlot")
					plots.ttest[[q]] <- state$plotsTtest[[index]]
			    } else {
			        plot <- list()
					
					plot[["title"]] <- "Prior and Posterior"
					plot[["width"]]  <- 530
					plot[["height"]] <- 400
					plot[["status"]] <- "waiting"
					
					# image <- .beginSaveImage(530, 400)
					# .plotPosterior.ttest(x=NULL, y=NULL, paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, addInformation=options$plotPriorAndPosteriorAdditionalInfo, dontPlotData=TRUE)
					# plot[["data"]] <- .endSaveImage(image)
					
					.plotFunc <- function() {
						.plotPosterior.ttest(x=NULL, y=NULL, paired=TRUE, oneSided=oneSided, rscale=options$priorWidth, addInformation=options$plotPriorAndPosteriorAdditionalInfo, dontPlotData=TRUE)
					}
					content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

					plot[["convertible"]] <- TRUE
					plot[["obj"]] <- content[["obj"]]
					plot[["data"]] <- content[["png"]]
					
					plots.ttest[[q]] <- plot
				}
				
				
				if (options$plotPriorAndPosteriorAdditionalInfo) {
				    plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"
				} else {
				    plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
				}
				
				plotGroups[[iint]][["PriorPosteriorPlot"]] <- plots.ttest[[q]]
				plotVariables[[length(plotVariables)+1]] <- variable
				
				q <- q + 1
			} 
			
			if (options$plotBayesFactorRobustness) {
			    if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& BFtypeRequiresNewPlot == FALSE && diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) &&
					"robustnessPlot" %in% state$plotTypes) {
			        #
			        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					index <- which(state$plotVariables == variable & state$plotTypes == "robustnessPlot")
					plots.ttest[[q]] <- state$plotsTtest[[index]]
			    } else {
			        plot <- list()
					
					plot[["title"]] <- "Bayes Factor Robustness Check"
					plot[["width"]]  <- 530
					plot[["height"]] <- 400
					plot[["status"]] <- "waiting"
					
					# image <- .beginSaveImage(530, 400)
					# .plotBF.robustnessCheck.ttest (oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
					# plot[["data"]] <- .endSaveImage(image)
					
					.plotFunc <- function() {
						.plotBF.robustnessCheck.ttest (oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
					}
					content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

					plot[["convertible"]] <- TRUE
					plot[["obj"]] <- content[["obj"]]
					plot[["data"]] <- content[["png"]]
					
					plots.ttest[[q]] <- plot
				}
				
				
				plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
				plotVariables[[length(plotVariables)+1]] <- variable
				plotGroups[[iint]][["BFrobustnessPlot"]] <- plots.ttest[[q]]
				
				q <- q + 1
			}
			
			if (options$plotSequentialAnalysis) {
			    if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
					&& BFtypeRequiresNewPlot == FALSE && diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) &&
					options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
			        #
			        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it
					
					index <- which(state$plotVariables == variable & state$plotTypes == "sequentialRobustnessPlot")
					plots.ttest[[q]] <- state$plotsTtest[[index]]
			    } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
							&& BFtypeRequiresNewPlot == FALSE && diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && 
							!options$plotSequentialAnalysisRobustness && "sequentialPlot" %in% state$plotTypes) {
			        #
			        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists use it
					
					index <- which(state$plotVariables == variable & state$plotTypes == "sequentialPlot")
					plots.ttest[[q]] <- state$plotsTtest[[index]]
			    } else {
			        plot <- list()
					
					plot[["title"]] <- "Sequential Analysis"
					plot[["width"]]  <- 530
					plot[["height"]] <- 400
					plot[["status"]] <- "waiting"
					
					# image <- .beginSaveImage(530, 400)
					# .plotSequentialBF.ttest(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
					# plot[["data"]] <- .endSaveImage(image)
					
					.plotFunc <- function() {
						.plotSequentialBF.ttest(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE)
					}
					content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

					plot[["convertible"]] <- TRUE
					plot[["obj"]] <- content[["obj"]]
					plot[["data"]] <- content[["png"]]
					
					plots.ttest[[q]] <- plot
				}
				
				
				if (options$plotSequentialAnalysisRobustness) {
				    plotTypes[[length(plotTypes)+1]] <- "sequentialRobustnessPlot"
				} else {
				    plotTypes[[length(plotTypes)+1]] <- "sequentialPlot"
				}
				
				plotVariables[[length(plotVariables)+1]] <- variable
				plotGroups[[iint]][["BFsequentialPlot"]] <- plots.ttest[[q]]
				
				q <- q + 1
			}
			
			iint <- iint + 1
		} 
		
		if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis){
		    results[["inferentialPlots"]] <- list(title=ifelse(length(options[["variables"]]) > 1 || sum(c(options$plotPriorAndPosterior, options$plotBayesFactorRobustness, options$plotSequentialAnalysis)) > 1,
		                                                       "Inferential Plots", "Inferential Plot"), collection=plotGroups)
		}
			
		if (options$descriptivesPlots){
		    results[["descriptives"]][["descriptivesPlots"]] <- list(title=ifelse(length(options[["variables"]]) > 1, "Descriptives Plots", "Descriptives Plot"), collection=descriptivesPlots)
		}
			
		
		
		if (perform == "run" && length(options$variables) > 0 && !is.null(grouping)) {
			if ( ! .shouldContinue(callback(results))){
			    return()
			}
		    
		    statusInd <- 1
			i <- 1
			z <- 1
			descriptInd <- 1
			
			for (variable in options[["variables"]]) {
			    subDataSet <- subset(dataset, select=c(.v(variable), .v(options$groupingVariable)))
				subDataSet <- na.omit(subDataSet)
				
				r.size <- options$priorWidth
				
				group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(variable)] 
				group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(variable)] 
				
				if (options$descriptivesPlots) {
				    if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE &&
						diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {
				        #
						# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
						# then, if the requested plot already exists, use it
						
						index <- which(state$descriptPlotVariables == variable)
						descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]
				    } else {
				        results[["descriptives"]][["descriptivesPlots"]][["collection"]][[i]][["status"]] <- "running"
						
						if ( ! .shouldContinue(callback(results))){
						    return()
						}
							
						plot <- descriptivesPlots[[descriptInd]]
						
						p <- try(silent= FALSE, expr= {
						    # image <- .beginSaveImage(options$plotWidth, options$plotHeight)
						    # .plot2GroupMeansBayesIndTtest(v1 = group2, v2 = group1, nameV1 = g1, nameV2 = g2, groupingName = options$groupingVariable, dependentName = variable, descriptivesPlotsCredibleInterval=
						    # options$descriptivesPlotsCredibleInterval)
						    # plot[["data"]] <- .endSaveImage(image)
						    
						    p <- .plot2GroupMeansBayesIndTtest(v1 = group2, v2 = group1, nameV1 = g1, nameV2 = g2, 
						                                       groupingName = options$groupingVariable, dependentName = variable, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
						    content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = p, obj = TRUE)
						    
						    plot[["convertible"]] <- TRUE
						    plot[["obj"]] <- content[["obj"]]
						    plot[["data"]] <- content[["png"]]
						})
						
						if (isTryError(p)) {
						    errorMessageTmp <- .extractErrorMessage(p)
							errorMessage <- paste0("Plotting not possible: ", errorMessageTmp)
							plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
						}
						
						plot[["status"]] <- "complete"
						descriptivesPlots[[descriptInd]] <- plot
				    }
				    
				    results[["descriptives"]][["descriptivesPlots"]][["collection"]] <- descriptivesPlots
				    descriptInd <- descriptInd + 1
					
					if ( ! .shouldContinue(callback(results))){
					    return()
					}
				}
				
				if (options$plotPriorAndPosterior) {
				    if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
						&& diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) &&
						options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {
				        #
				        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
						# then, if the requested plot already exists, use it
						
						index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlotAddInfo")
						plots.ttest[[z]] <- state$plotsTtest[[index]]
				    } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE &&
								diff$hypothesis == FALSE && diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && 
								!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {
				        #
				        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
						# if the requested plot already exists use it
						
						index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlot")
						plots.ttest[[z]] <- state$plotsTtest[[index]]
				    } else {
				        results[["inferentialPlots"]][["collection"]][[i]][["PriorPosteriorPlot"]][["status"]] <- "running"
						
						if ( ! .shouldContinue(callback(results))){
						    return()
						}
				        
				        plot <- plots.ttest[[z]]
				        
				        if (status[statusInd] != "error") {
				            p <- try(silent= FALSE, expr= {
				                # image <- .beginSaveImage(530, 400)
				                # .plotPosterior.ttest(x= group2, y= group1, paired= FALSE, oneSided= oneSided, rscale = options$priorWidth, addInformation= options$plotPriorAndPosteriorAdditionalInfo, BF=BF10post
				                # [i], BFH1H0=BFH1H0)
				                # plot[["data"]] <- .endSaveImage(image)
				                
				                .plotFunc <- function() {
				                    .plotPosterior.ttest(x= group2, y= group1, paired= FALSE, oneSided= oneSided, rscale = options$priorWidth,
				                                         addInformation= options$plotPriorAndPosteriorAdditionalInfo, BF=BF10post[i], BFH1H0=BFH1H0)
				                }
				                
				                content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
				                
				                plot[["convertible"]] <- TRUE
				                plot[["obj"]] <- content[["obj"]]
				                plot[["data"]] <- content[["png"]]
				            })
				            
				            if (isTryError(p)) {
				                errorMessage <- .extractErrorMessage(p)
								
								if (errorMessage == "not enough data") {
								    errorMessage <- "Plotting is not possible: The Bayes factor is too small"
								} else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
								    errorMessage <- "Plotting is not possible: The Bayes factor is too small"
								}
								
								plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
				            }
				        } else {
				            plot[["error"]] <- list(error="badData", errorMessage=plottingError[statusInd])
						}
						
						plot[["status"]] <- "complete"
						plots.ttest[[z]] <- plot
					}
					
					plotGroups[[i]][["PriorPosteriorPlot"]] <- plots.ttest[[z]]
					results[["inferentialPlots"]][["collection"]] <- plotGroups
					
					z <- z + 1
					
					if ( ! .shouldContinue(callback(results))){
					    return()
					}
				}
				
				
				if (isTRUE(options$plotBayesFactorRobustness)) {
				    if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
						&& BFtypeRequiresNewPlot == FALSE && diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) &&
						"robustnessPlot" %in% state$plotTypes) {
				        # 
				        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
						# then, if the requested plot already exists, use it
						
						index <- which(state$plotVariables == variable & state$plotTypes == "robustnessPlot")
						plots.ttest[[z]] <- state$plotsTtest[[index]]
						results[["inferentialPlots"]][["collection"]][[i]][["BFrobustnessPlot"]][["status"]] <- "complete"
				    } else {
				        results[["inferentialPlots"]][["collection"]][[i]][["BFrobustnessPlot"]][["status"]] <- "running"
						
						if ( ! .shouldContinue(callback(results))){
						    return()
						}
							
						plot <- plots.ttest[[z]]
						
						if (status[statusInd] != "error") {
						    p <- try(silent= FALSE, expr= {
						        # image <- .beginSaveImage(530, 400)
						        # .plotBF.robustnessCheck.ttest(x= group2, y= group1, BF10post=ifelse((options$bayesFactorType=="LogBF10"), exp(BF10post[i]), BF10post[i]), paired= FALSE, oneSided= oneSided, rscale = options$priorWidth, BFH1H0= BFH1H0)
						        # content <- .endSaveImage(image)
						        # plot[["data"]]  <- content
						        
						        .plotFunc <- function() {
						            .plotBF.robustnessCheck.ttest(x= group2, y= group1, BF10post=ifelse((options$bayesFactorType=="LogBF10"), exp(BF10post[i]), BF10post[i]), 
						                                          paired= FALSE, oneSided= oneSided, rscale = options$priorWidth, BFH1H0= BFH1H0)
						        }
						        
						        content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
						        
						        plot[["convertible"]] <- TRUE
						        plot[["obj"]] <- content[["obj"]]
						        plot[["data"]] <- content[["png"]]
						    })
						    
						    if (isTryError(p)) {
						        errorMessage <- .extractErrorMessage(p)
								
								if (errorMessage == "not enough data") {
								    errorMessage <- "Plotting is not possible: The Bayes factor is too small"
								} else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
								    errorMessage <- "Plotting is not possible: The Bayes factor is too small"
								}
						        plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
						    }
						} else {
						    plot[["error"]] <- list(error="badData", errorMessage=plottingError[statusInd])
						}
						
						plot[["status"]] <- "complete"
						plots.ttest[[z]] <- plot
					}
					
					plotGroups[[i]][["BFrobustnessPlot"]] <- plots.ttest[[z]]
					results[["inferentialPlots"]][["collection"]] <- plotGroups # add plots without image object to results
					
					z <- z + 1
					
					if ( ! .shouldContinue(callback(results))){
					    return()
					}
				}
				
				if (isTRUE(options$plotSequentialAnalysis)) {
				    if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
						&& BFtypeRequiresNewPlot == FALSE && diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) &&
						options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {
				        #
				        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
						# then, if the requested plot already exists, use it
						
						index <- which(state$plotVariables == variable & state$plotTypes == "sequentialRobustnessPlot")
						
						plots.ttest[[z]] <- state$plotsTtest[[index]]
						results[["inferentialPlots"]][["collection"]][[i]][["BFsequentialPlot"]][["status"]] <- "complete"
				    } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE &&
								diff$hypothesis == FALSE && BFtypeRequiresNewPlot == FALSE  && diff$groupingVariable == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE)))
								&& !options$plotSequentialAnalysisRobustness && "sequentialPlot" %in% state$plotTypes) {
				        # 
				        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
						# then, if the requested plot already exists use it
						
						index <- which(state$plotVariables == variable & state$plotTypes == "sequentialPlot")
						
						plots.ttest[[z]] <- state$plotsTtest[[index]]
						results[["inferentialPlots"]][["collection"]][[i]][["BFsequentialPlot"]][["status"]] <- "complete"
					} else {
					    results[["inferentialPlots"]][["collection"]][[i]][["BFsequentialPlot"]][["status"]] <- "running"
						
						if ( ! .shouldContinue(callback(results))){
						    return()
						}
						
						plot <- plots.ttest[[z]]
						
						if (status[statusInd] != "error" && status[statusInd] != "sequentialNotPossible") {
						    p <- try(silent= FALSE, expr= {
						        # image <- .beginSaveImage(530, 400)
						        # .plotSequentialBF.ttest(x= group2, y= group1, paired= FALSE, oneSided= oneSided, rscale = options$priorWidth, BFH1H0= BFH1H0, BF10post=BF10post[i],
						        # 						plotDifferentPriors= options$plotSequentialAnalysisRobustness, subDataSet=subDataSet, level1=g1, level2=g2)
						        # content <- .endSaveImage(image)
						        # plot[["data"]]  <- content
						        
						        .plotFunc <- function() {
						            .plotSequentialBF.ttest(x= group2, y= group1, paired= FALSE, oneSided= oneSided, rscale = options$priorWidth, BFH1H0= BFH1H0, BF10post=BF10post[i],
						                                    plotDifferentPriors= options$plotSequentialAnalysisRobustness, subDataSet=subDataSet, level1=g1, level2=g2)
						        }
						        
						        content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
						        
						        plot[["convertible"]] <- TRUE
						        plot[["obj"]] <- content[["obj"]]
						        plot[["data"]] <- content[["png"]]
						    })
						    
						    if (isTryError(p)) {
						        errorMessage <- .extractErrorMessage(p)
								
								if (errorMessage == "not enough data") {
								    errorMessage <- "Plotting is not possible: The Bayes factor is too small"
								} else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
								    errorMessage <- "Plotting is not possible: The Bayes factor is too small"
								}
								
								plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
						    }
						} else {
						    plot[["error"]] <- list(error="badData", errorMessage=plottingError[statusInd])
						}
						
						plot[["status"]] <- "complete"
						plots.ttest[[z]] <- plot
					}
					
					plotGroups[[i]][["BFsequentialPlot"]] <- plots.ttest[[z]]
					results[["inferentialPlots"]][["collection"]] <- plotGroups
					
					z <- z + 1
					
					if ( ! .shouldContinue(callback(results))){
					    return()
					}
				}
				
				statusInd <- statusInd + 1
				i <- i + 1
			}
		}
	}
	
	
	keep <- NULL
	
	for (plot in plots.ttest){
	    keep <- c(keep, plot$data)
	}
		
	for (plot in descriptivesPlots){
	    keep <- c(keep, plot$data)
	}
	
	descriptivesPlots <- descriptivesPlots
	# TODO why?
	plots.ttest <- plots.ttest

	if (perform == "init") {
	    return(list(results=results, status="inited", state=state, keep=keep))
	} else {
	    return(list(results=results, status="complete", state=list(options=options, results=results, plotsTtest=plots.ttest, plotTypes=plotTypes, plotVariables=plotVariables,
		descriptPlotVariables=descriptPlotVariables, descriptivesPlots=descriptivesPlots, status=status, plottingError=plottingError, BF10post=BF10post, errorFootnotes=errorFootnotes),
		keep=keep))
	}
}


.ttestBayesianIndependentSamplesTTest <- function(dataset, options, perform, state, diff) {
    g1 <- NULL
	g2 <- NULL
	
	ttest <- list()
	
	ttest[["title"]] <- "Bayesian Independent Samples T-Test"
	ttest[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
	
	## TODOTODO:setup table for the Bayesian independent samples t-test; 
	#
	# fields <- list(list(name = "v", title = "", type = "string", combine = TRUE),
	#                list(name = "test", type = "string", title = "Test"),
	#                list(name = "df", type = "number", format = "sf:4;dp:3"),
	#                list(name = "p", type = "number", format = "dp:3;p:.001"))
	
	fields <- list(
		list(name=".variable", title="", type="string", combine=TRUE)
	)
	
	if (options$bayesFactorType == "BF01") {
	    BFH1H0 <- FALSE
		
		if (options$hypothesis == "groupsNotEqual"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u2081")
		}
		if (options$hypothesis == "groupOneGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u208A")
		}
		if (options$hypothesis == "groupTwoGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u208B")
		}
		
	} else if (options$bayesFactorType == "BF10") {
	    BFH1H0 <- TRUE
		
		if (options$hypothesis == "groupsNotEqual"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2081\u2080")
		}
		if (options$hypothesis == "groupOneGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u208A\u2080")
		}
		if (options$hypothesis == "groupTwoGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u208B\u2080")
		}
	} else if (options$bayesFactorType == "LogBF10") {
	    BFH1H0 <- TRUE
		
		if (options$hypothesis == "groupsNotEqual"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="Log(\u2009\u0042\u0046\u2081\u2080\u2009)")
		}
		if (options$hypothesis == "groupOneGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="Log(\u2009\u0042\u0046\u208A\u2080\u2009)")
		}
		if (options$hypothesis == "groupTwoGreater"){
			fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="Log(\u2009\u0042\u0046\u208B\u2080\u2009)")
		}
	}
	
	if (options$hypothesis == "groupsNotEqual") {
	    fields[[length(fields)+1]] <- list(name="error", type="number", format="sf:4;dp:3", title="error %")
	} else {
	    fields[[length(fields)+1]] <- list(name="error", type="number", format="sf:4;dp:3;~", title="error %")
	}
	
	ttest[["schema"]] <- list(fields=fields)
	
	footnotes <- .newFootnotes()
	
	levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])
	
	if (length(levels) != 2) {
	    g1 <- "1"
		g2 <- "2"
	} else {
	    g1 <- levels[1]
		g2 <- levels[2]
	}
	
	if (options$hypothesis == "groupOneGreater") {
	    message <- paste("For all tests, the alternative hypothesis specifies that group <em>", g1, "</em> is greater than group <em>", g2, "</em>.", sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	} else if (options$hypothesis == "groupTwoGreater") {
	    message <- paste("For all tests, the alternative hypothesis specifies that group <em>", g1, "</em> is less than group <em>", g2, "</em>.", sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}
	
	ttest.rows <- list()
	
	status <- rep("ok", length(options$variables))
	BF10post <- numeric(length(options$variables))
	plottingError <- rep("error", length(options$variables))
	errorFootnotes <- rep("no", length(options$variables))
	
	for (variable in options[["variables"]]) {
	    if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
			&& diff$groupingVariable == FALSE && diff$missingValues == FALSE)))) {
	        # TODO: Note
	        index <- which(state$options$variables == variable)
	        
	        if (state$errorFootnotes[index] == "no") {
	            ttest.rows[[length(ttest.rows)+1]] <- state$results$ttest$data[[index]]
	            
	            if (! (is.logical(diff) && diff == FALSE) && diff$bayesFactorType) {
	                if (state$options$bayesFactorType == "BF10") {
	                    if (options$bayesFactorType == "BF01") {
	                        ttest.rows[[length(ttest.rows)]]$BF <- 1 / state$results$ttest$data[[index]]$BF
	                    } else if (options$bayesFactorType == "LogBF10") {
	                        ttest.rows[[length(ttest.rows)]]$BF <- log(state$results$ttest$data[[index]]$BF)
	                    }
	                } else if (state$options$bayesFactorType == "BF01") {
	                    if (options$bayesFactorType == "BF10") {
	                        ttest.rows[[length(ttest.rows)]]$BF <- 1 / state$results$ttest$data[[index]]$BF
	                    } else if (options$bayesFactorType == "LogBF10") {
	                        ttest.rows[[length(ttest.rows)]]$BF <- log(1 / state$results$ttest$data[[index]]$BF)
	                    }
	                } else if (state$options$bayesFactorType == "LogBF10") {
	                    if (options$bayesFactorType == "BF10") {
	                        ttest.rows[[length(ttest.rows)]]$BF <- exp(state$results$ttest$data[[index]]$BF)
	                    } else if (options$bayesFactorType == "BF01") {
	                        ttest.rows[[length(ttest.rows)]]$BF <- 1 / exp(state$results$ttest$data[[index]]$BF)
	                    }
	                }
	            }
	        } else {
	            index2 <- .addFootnote(footnotes, state$errorFootnotes[index])
	            ttest.rows[[length(ttest.rows)+1]] <- list(.variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index2)))
	        }
	    } else {
	        ttest.rows[[length(ttest.rows)+1]] <- list(.variable=variable)
	    }
	}
	
	rowCompleted <- logical(length(ttest.rows))
	
	for (i in seq_along(ttest.rows)){
	    rowCompleted[i] <- ifelse(length(ttest.rows[[i]]) > 1, TRUE, FALSE)
	}
	
	if (!is.null(state) && all(options[["variables"]] %in% state$options$variables) && options$groupingVariable == state$options$groupingVariable && all(rowCompleted)){
	    # TODO: description
	    ttest[["status"]] <- "complete"
	}
		
	if (perform == "run" && length(options$variables) != 0 && options$groupingVariable != "") {
	    if (length(levels) != 2) {
	        ttest[["error"]] <- list(errorType="badData", errorMessage="The Grouping Variable must have 2 levels")
	        
	        status <- rep("error", length(options$variables))
	        plottingError <- rep("Plotting is not possible: The Grouping Variable must have 2 levels", length(options$variables))
	    } else {
	        rowNo <- 1
	        i <- 1
	        
	        for (variable in options[["variables"]]) {
	            # BayesFactor package doesn't handle NAs, so it is necessary to exclude them
	            # TODO: Add .hasErrors
	            
	            subDataSet <- subset(dataset, select=c(.v(variable), .v(options$groupingVariable)))
				subDataSet <- na.omit(subDataSet)
				
				gs <- base::levels(levels)
				
				group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(variable)] 
				group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(variable)] 
				
				f <- as.formula(paste( .v(variable), "~", .v(options$groupingVariable)))
				r.size <- options$priorWidth
				
				if (options$hypothesis == "groupOneGreater") {
				    null.interval <- c(0, Inf)
					oneSided <- "right"
				} else if (options$hypothesis == "groupTwoGreater") {
				    null.interval <- c(-Inf, 0)
					oneSided <- "left"
				} else {
				    null.interval <- c(-Inf, Inf)
					oneSided <- FALSE
				}
				
				
				if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
				    && diff$groupingVariable == FALSE && diff$missingValues == FALSE)))) {
				    
				    index <- which(state$options$variables == variable)
					
					if (state$errorFootnotes[index] == "no") {
					    ttest.rows[[rowNo]] <- state$results$ttest$data[[index]]
					    
					    if (! (is.logical(diff) && diff == FALSE) && diff$bayesFactorType) {
					        if (state$options$bayesFactorType == "BF10") {
					            if (options$bayesFactorType == "BF01") {
									ttest.rows[[rowNo]]$BF <- 1 / state$results$ttest$data[[index]]$BF
								} else if (options$bayesFactorType == "LogBF10") {
									ttest.rows[[rowNo]]$BF <- log(state$results$ttest$data[[index]]$BF)
								}
					        } else if (state$options$bayesFactorType == "BF01") {
					            if (options$bayesFactorType == "BF10") {
									ttest.rows[[rowNo]]$BF <- 1 / state$results$ttest$data[[index]]$BF
								} else if (options$bayesFactorType == "LogBF10") {
									ttest.rows[[rowNo]]$BF <- log(1 / state$results$ttest$data[[index]]$BF)
								}
					        } else if (state$options$bayesFactorType == "LogBF10") {
					            if (options$bayesFactorType == "BF10") {
									ttest.rows[[rowNo]]$BF <- exp(state$results$ttest$data[[index]]$BF)
								} else if (options$bayesFactorType == "BF01") {
									ttest.rows[[rowNo]]$BF <- 1 / exp(state$results$ttest$data[[index]]$BF)
								}
							}
					    }
					} else {
					    index2 <- .addFootnote(footnotes, state$errorFootnotes[index])
					    errorFootnotes[rowNo] <- state$errorFootnotes[index]
					    ttest.rows[[rowNo]] <- list(.variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index2)))
					} 
				    
				    BF10post[rowNo] <- ttest.rows[[rowNo]]$BF # state$BF10post[index]
					status[rowNo] <- state$status[index]
					plottingError[rowNo] <- state$plottingError[index]
				} else {
				    result <- try (silent=FALSE, expr= {
				        bf    <- BayesFactor::ttestBF(data=subDataSet, formula=f, r=r.size)[1]
				        bf.raw <- exp(as.numeric(bf@bayesFactor$bf))
				        N1 <- length(group2)
						N2 <- length(group1)
						
						# sdPooled <- sqrt(((N1 - 1) * var(group2) + (N2 - 1) * var(group1)) / (N1 + N2))
						# deltaHat <- (mean(group2) - mean(group1)) / sdPooled
						# df <- N1 + N2 - 2
						# sigmaStart <- sqrt(N1 * N2 / (N1 + N2))
						# 
						# if (sigmaStart < .01) 
						# 	sigmaStart <- .01
						
						if (oneSided == "right") {
						    samples <- BayesFactor::ttestBF(data=subDataSet, formula=f, r=r.size, posterior = TRUE, iterations = 10000)
						    delta <- samples[, "delta"]
							
							if (is.infinite(bf.raw)) {
							    if (mean(delta) > 0) {
							        bf.raw <- Inf
							    } else {
							        bf.raw <- 1 / Inf
							    }
							} else {
							    bf.raw <- .oneSidedTtestBFRichardAdaptive(x=group2, y=group1, r=r.size, oneSided="right")
								
								# parameters <- try(silent=TRUE, expr= optim(par = c(deltaHat, sigmaStart, df), fn=.likelihoodShiftedT, data= delta , method="BFGS")$par)
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
						    samples <- BayesFactor::ttestBF(data=subDataSet, formula=f, r=r.size, posterior = TRUE, iterations = 10000)
							delta <- samples[, "delta"]
							
							if (is.infinite(bf.raw)) {
							    if (mean(delta) < 0) {
							        bf.raw <- Inf
							    } else {
							        bf.raw <- 1 / Inf
							    }
							} else {
							    bf.raw <- .oneSidedTtestBFRichardAdaptive(x=group2, y=group1, r=r.size, oneSided="left")
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
						
						if (options$bayesFactorType == "BF01"){
						    bf.raw <- 1 / bf.raw
						}
						
						BF10post[i] <- bf.raw
						BF <- .clean(bf.raw)
						
						if (options$bayesFactorType == "LogBF10") {
						    BF <- log(BF10post[i])
							BF <- .clean(BF)
						}
						
						error <- .clean(as.numeric(bf@bayesFactor$error))
						errorMessage <- NULL
						
						if (is.na(bf.raw)) {
						    status[rowNo] <- "error"
							plottingError[rowNo] <- "Plotting is not possible: Bayes factor could not be calculated"
						}
						
						if (is.infinite(bf.raw)){
						    if(options$plotPriorAndPosterior | options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness | options$plotBayesFactorRobustness){
						        status[rowNo] <- "error"
								plottingError[rowNo] <- "Plotting is not possible: Bayes factor is infinite"
							}
						}
						
						if(is.infinite(1/bf.raw)){
						    if(options$plotPriorAndPosterior | options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness | options$plotBayesFactorRobustness){
						        status[rowNo] <- "error"
								plottingError[rowNo] <- "Plotting is not possible: The Bayes factor is too small"
							}
						}
						
						ind <- which(group1 == group1[1])
						idData <- sum((ind+1)-(1:(length(ind))) == 1)
						
						ind2 <- which(group2 == group2[1])
						idData2 <- sum((ind2+1)-(1:(length(ind2))) == 1)
						
						if(idData > 1 && idData2 > 1 && (options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness)){
						    if(options$plotPriorAndPosterior | options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness | options$plotBayesFactorRobustness){
						        # errorMessage <- paste("Sequential Analysis not possible: The first observations are identical")
								# index <- .addFootnote(footnotes, errorMessage)
								# 
								# status[rowNo] <- "sequentialNotPossible"
								# plottingError[rowNo] <- "Sequential Analysis not possible: The first observations are identical"
						        # TODO: ?
							}
						}
						
						if(!is.null(errorMessage)){
						    index <- .addFootnote(footnotes, errorMessage)
							list(.variable=variable, BF=BF, error=error, .footnotes=list(BF=list(index)))
						} else {
						    list(.variable=variable, BF=BF, error=error)
						}
					})
					
					if (isTryError(result)) {
					    errorMessage <- .extractErrorMessage(result)
					    plottingError[rowNo] <- paste("Plotting is not possible:", errorMessage, sep=" ")
					    
					    if (errorMessage == "Dependent variable must not contain missing or infinite values.") {
					        errorMessage <- "Bayes factor is undefined - the dependent variable contains infinity"
							status[rowNo] <- "error"
							plottingError[rowNo] <- "Plotting is not possible: Bayes factor is undefined - the dependent variable contains infinity"
					    } else if (errorMessage == "grouping factor must have exactly 2 levels") {
					        #
					        # We know that the grouping factor *does* have two levels, because we've checked this earlier on
							# This error means that all of one factor has been excluded because of missing values in the dependent
							#
							errorMessage <- "Bayes factor is undefined - the grouping variable contains less than two levels once missing values in the dependent are excluded"
							status[rowNo] <- "error"
							plottingError[rowNo] <- "Plotting is not possible: Bayes factor is undefined - the grouping variable contains less than two levels once missing values in the dependent are excluded"
					    } else if (errorMessage == "data are essentially constant") {
					        #
					        errorMessage <- "Bayes factor is undefined - one or both levels of the dependent contain all the same value (zero variance)"
							status[rowNo] <- "error"
							plottingError[rowNo] <- "Plotting is not possible: Bayes factor is undefined - one or both levels of the dependent contain all the same value (zero variance)"
					    } else if (errorMessage == "Insufficient sample size for t analysis." || errorMessage == "not enough observations") {
					        errorMessage <- "Bayes factor is undefined - one or both levels of the dependent contain too few observations"
							status[rowNo] <- "error"
							plottingError[rowNo] <- "Plotting is not possible: Bayes factor is undefined - one or both levels of the dependent contain too few observations"
						}
						
						index <- .addFootnote(footnotes, errorMessage)
						errorFootnotes[rowNo] <- errorMessage
						result <- list(.variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
						
						status[rowNo] <- "error"
					}
				    ttest.rows[[rowNo]] <- result
				}
				rowNo <- rowNo + 1
				i <- i + 1
			}
		}
		ttest[["status"]] <- "complete"
	}
	ttest[["footnotes"]] <- as.list(footnotes)
	ttest[["data"]] <- ttest.rows
	
	list(ttest, status, g1, g2, BFH1H0, plottingError, BF10post, errorFootnotes)
}

.base_breaks_x <- function(x) {
	
	b <- unique(as.numeric(x))
	d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
	list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1))
	
}

.base_breaks_y3 <- function(x) {
	
	ci.pos <- c(x$ciLower, x$ciUpper)
	b <- pretty(ci.pos)
	d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
	list(	ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
			ggplot2::scale_y_continuous(breaks=c(min(b),max(b)))	)
}

.plot2GroupMeansBayesIndTtest <- function(v1=NULL, v2=NULL, nameV1=NULL, nameV2=NULL, groupingName=NULL, dependentName=NULL, descriptivesPlotsCredibleInterval=.95) {
	
	v1 <- na.omit(v1)
	v2 <- na.omit(v2)
	
	if (any(is.infinite(v1)) || any(is.infinite(v2)))
		stop("Plotting not possible: Variable contains infinity")
	
	posteriorSummary1 <- .posteriorSummaryGroupMean(variable=v1, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
	posteriorSummary2 <- .posteriorSummaryGroupMean(variable=v2, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
	summaryStat <- data.frame(	groupingVariable=c(nameV1, nameV2), dependent=c(posteriorSummary1$median, posteriorSummary2$median),
								ciLower=c(posteriorSummary1$ciLower, posteriorSummary2$ciLower), ciUpper=c(posteriorSummary1$ciUpper,
								posteriorSummary2$ciUpper))
	
	pd <- ggplot2::position_dodge(.2)
	
	p <-	ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=1)) +
			ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
			ggplot2::geom_line(position=pd, size = .7) + 
			ggplot2::geom_point(position=pd, size=4) +
			ggplot2::ylab(dependentName) +
			ggplot2::xlab(groupingName) +
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
			.base_breaks_x(summaryStat$groupingVariable)
	
	return(p)
}

.ttestBayesianIndependentSamplesDescriptives <- function(dataset, options, perform,
												 state = NULL, diff = NULL) {
	if (options$descriptives == FALSE) return(NULL)

	descriptives = list("title" = "Group Descriptives")

	## sets up the table for the descriptives
	fields <- list(
		list(name = "variable", title = "", type = "string", combine = TRUE),
		list(name = "group", title = "Group", type = "string"),
		list(name = "N", title = "N", type = "number"),
		list(name = "mean", title = "Mean", type = "number", format = "sf:4;dp:3"),
		list(name = "sd", title = "SD", type = "number", format = "sf:4;dp:3"),
		list(name = "se", title = "SE", type = "number", format = "sf:4;dp:3")
	)

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

	descriptives[["schema"]] <- list(fields = fields)
	data <- list()


	## function to check if everything is alright with the options
	isAllright <- function(variable, options, state = NULL, diff = NULL) {

		# check if the variable is in the state variables
		cond1 <- !is.null(state) && variable %in% state$options$variables

		# check if either diff is true, or it's a list and descriptives,
		# and groupingVariable, missingValues are FALSE
		cond2 <- (!is.null(diff) && (is.logical(diff) && diff == FALSE) || (is.list(diff)
				 && !any(diff$descriptives,diff$groupingVariable, diff$missingValues)))

		cond1 && cond2
	}

	variables <- options$variables
	if (length(variables) == 0) variables <- "."

	for (variable in variables) {

		if (isAllright(variable, options, state, diff)) {

			stateDat <- state$results$descriptives$data
			descriptivesVariables <- as.character(length(stateDat))

			for (i in seq_along(stateDat))
				descriptivesVariables[i] <- stateDat[[i]]$variable

			indices <- which(descriptivesVariables == variable)
			data[[length(data) + 1]] <- stateDat[[indices[1]]]
			data[[length(data) + 1]] <- stateDat[[indices[2]]]

		} else {
			data[[length(data) + 1]] <- list(variable = variable, .isNewGroup = TRUE)
			data[[length(data) + 1]] <- list(variable = variable)
		}
	}

	## check if we are done with all this crap
	done <- (!is.null(state) &&
			 state$options$descriptives &&
			 all(variables %in% state$options$variables))

	if (done) descriptives[["status"]] <- "complete"

	groups <- options$groupingVariable

	## if we actually have to do the test, and we have a grouping variable
	if (perform == "run" && groups != "") {
		levels <- base::levels(dataset[[ .v(groups) ]])

		## if people don't know what a t-test is...
		if (length(levels) != 2) {
			descriptives[["error"]] <- list(errorType = "badData")

		} else {

			rowNo <- 1
			groupingData <- dataset[[.v(groups)]]

			## do the whole loop as above again
			for (variable in variables) {

				# if everything is alright, add stuff to data
				if (isAllright(variable, options, state, diff)) {

					stateDat <- state$results$descriptives$data
					descriptivesVariables <- as.character(length(stateDat))

					for (i in seq_along(stateDat))
						descriptivesVariables[i] <- stateDat[[i]]$variable

					indices <- which(descriptivesVariables == variable)

					data[[rowNo]] <- stateDat[[indices[1]]]
					data[[rowNo]] <- stateDat[[indices[2]]]

					rowNo <- rowNo + 2

				} else {

					for (i in 1:2) {

					  level <- levels[i]
					  variableData <- dataset[[.v(variable)]]

					  groupData <- variableData[groupingData == level]
					  groupDataOm <- na.omit(groupData)

					  if (class(groupDataOm) != "factor") {

							posteriorSummary <- .posteriorSummaryGroupMean(variable=groupDataOm, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
							ciLower <- .clean(posteriorSummary$ciLower)
							ciUpper <- .clean(posteriorSummary$ciUpper)

						  n <- .clean(length(groupDataOm))
						  mean <- .clean(mean(groupDataOm))
						  std <- .clean(sd(groupDataOm))
						  sem <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))

						  result <- list(variable = variable, group = level,
										 N = n, mean = mean, sd = std, se = sem, lowerCI = ciLower,
										 upperCI = ciUpper)

					  } else {

						n <- .clean(length(groupDataOm))
						result <- list(variable = variable, group = "",
									   N = n, mean = "", sd = "", se = "", lowerCI = "",
										 upperCI = "")
					}

					if (i == 1) {
						result[[".isNewGroup"]] <- TRUE
					}

					data[[rowNo]] <- result
					rowNo <- rowNo + 1
					}
				}
			}
		}
		descriptives[["status"]] <- "complete"
	}

	descriptives[["data"]] <- data
	descriptives
}
