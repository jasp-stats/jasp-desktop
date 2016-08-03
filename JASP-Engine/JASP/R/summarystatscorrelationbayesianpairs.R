#
# Copyright (C) 2016 University of Amsterdam
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

SummaryStatsCorrelationBayesianPairs <- function(dataset=NULL, options, perform = 'run', callback = function(...) 0,  ...)
{
	results <- list()

	meta <- list()

	meta[[1]] <- list(name="table", type="table")
	meta[[2]] <- list(name="inferentialPlots", type="object", meta=list(list(name="PriorPosteriorPlot", type="image"),
																		list(name="BFrobustnessPlot", type="image")))

	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Correlation Pairs"

	state <- .retrieveState()

	diff <- NULL

	if (!is.null(state))     #difference between the previous state variables(options) and current options
	{
		diff <- .diff(options, state$options)
	}

	fields=list()
	fields[[length(fields)+1]] <- list(name="sampleSize", type="number", title="n")	
	fields[[length(fields)+1]] <- list(name="pearsonsR", type="number", format="sf:4;dp:3", title="r")

	#Bayes factor type (BF10, BF01, log(BF10))
	bf.type <- options$bayesFactorType
	
	if (bf.type == "BF10")
	{
		BFH1H0 <- TRUE
	
		if (options$hypothesis == "correlated")
		{
			bf.title <- "BF\u2081\u2080"
			oneSided <- FALSE
		}
		else if (options$hypothesis == "correlatedPositively")
		{
			bf.title <- "BF\u208A\u2080"
			oneSided <- "right"
		}
		else if (options$hypothesis == "correlatedNegatively")
		{
			bf.title <- "BF\u208B\u2080"
			oneSided <- "left"
		}
	}
	else if (bf.type == "LogBF10")
	{
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "correlated")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
			oneSided <- FALSE
		}
		else if (options$hypothesis == "correlatedPositively")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
			oneSided <- "right"
		}
		else if (options$hypothesis == "correlatedNegatively")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
			oneSided <- "left"
		}
	}
	else if (bf.type == "BF01")
	{
		BFH1H0 <- FALSE
	
		if (options$hypothesis == "correlated")
		{
			bf.title <- "BF\u2080\u2081"
			oneSided <- FALSE
		}
		else if (options$hypothesis == "correlatedPositively")
		{
			bf.title <- "BF\u2080\u208A"
			oneSided <- "right"
		}
		else if (options$hypothesis == "correlatedNegatively")
		{
			bf.title <- "BF\u2080\u208B"
			oneSided <- "left"
		}
	}

	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title=bf.title)

	table <- list()
	table[["title"]] <- "Bayesian Pearson Correlation"
	table[["schema"]] <- list(fields=fields)
	table[["citation"]] <- list(
				"Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2014). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Manuscript submitted for publication.")

	footnotes <- .newFootnotes()
	
	if (options$hypothesis == "correlatedPositively")
	{
		.addFootnote(footnotes, "For all tests, the alternative hypothesis specifies that the correlation is positive.", symbol="<i>Note</i>.")
	}
	else if (options$hypothesis == "correlatedNegatively")
	{
		.addFootnote(footnotes, "For all tests, the alternative hypothesis specifies that the correlation is negative.", symbol="<i>Note</i>.")
	}

	table[["footnotes"]] <- as.list(footnotes)

	rowsCorrelationBayesianPairs <- list()
	bayesFactorObject <- NULL
	priorAndPosteriorPlot <- NULL
	priorAndPosteriorPlotAddInfo <- NULL
	bayesFactorRobustnessPlot <- NULL
	plot.sumstats.correlation <- list()
	plotTypes <- list()

	if(perform=="run")
	{
		if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
			diff$sampleSize==FALSE && diff$pearsonsR==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && !is.null(state$bayesFactorObject))
		{
			rowsCorrelationBayesianPairs <- state$rowsCorrelationBayesianPairs
			bayesFactorObject <- state$bayesFactorObject
		}
		else
		{
			status <- .checkInputSummaryStatsCorrelationPairs(options)
			row <- status$row

			if(status$ready)                           #check if data has been entered
			{
				bayesFactor10 <- .calcluateBFCorrelationPairs(options, state, diff) #calculate Bayes factor from t value

				if(options$bayesFactorType == "BF10")
				{
					BF <- .clean(bayesFactor10$bf)
				}
				else if(options$bayesFactorType == "BF01")
				{
					BF <- .clean(1/(bayesFactor10$bf))
				}
				else
				{
					BF <- .clean(log(bayesFactor10$bf))
				}

				bayesFactorObject <- bayesFactor10
				row <- list(BF=BF, sampleSize=options$sampleSize, pearsonsR=options$pearsonsR)
			}

			rowsCorrelationBayesianPairs <- row
		}

		if(options$plotPriorAndPosterior)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$sampleSize==FALSE && diff$pearsonsR==FALSE && diff$priorWidth==FALSE && diff$hypothesis == FALSE))) && 
				options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlotAddInfo
				index <- which(state$plotTypes == "posteriorPlotAddInfo")
				plot.sumstats.correlation[[length(plot.sumstats.correlation)+1]] <- state$plotsCorrelationTest[[index]]
				priorAndPosteriorPlotAddInfo <- priorAndPosteriorPlot
			}
			else if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$sampleSize==FALSE && diff$pearsonsR==FALSE && diff$priorWidth==FALSE && diff$hypothesis == FALSE))) && 
				!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlot
				index <- which(state$plotTypes == "posteriorPlot")
				plot.sumstats.correlation[[length(plot.sumstats.correlation)+1]] <- state$plotsCorrelationTest[[index]]
			}
			else
			{
				width  <- 530
				height <- 400

				plot <- list()

				plot[["title"]] <- "Prior and Posterior"
				plot[["width"]]  <- width
				plot[["height"]] <- height
				plot[["status"]] <- "waiting"
				
				image <- .beginSaveImage(width, height)
				.plotPosterior.correlation(r=options$pearsonsR, n=options$sampleSize, oneSided=oneSided, dontPlotData=FALSE, addInformation=options$plotPriorAndPosteriorAdditionalInfo,
										   kappa=options$priorWidth, BFH1H0=BFH1H0, BF=bayesFactorObject$bf)
				plot[["data"]] <- .endSaveImage(image)
				plot[["status"]] <- "complete"

				plots.sumstats.correlation[[length(plots.sumstats.correlation)+1]] <- plot
			}

			priorAndPosteriorPlot <- plots.sumstats.correlation[[length(plots.sumstats.correlation)]]

			if(options$plotPriorAndPosteriorAdditionalInfo)
			{
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"
			}
			else
			{
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
			}
		}

		if(options$plotBayesFactorRobustness)
		{
			BFtypeRequiresNewPlot <- TRUE
			
			if (!(is.null(state)))
			{
				BFtypeRequiresNewPlot <- FALSE
				BFtype <- options$bayesFactorType
				BFtypeState <- state$options$bayesFactorType
				
				if ((BFtypeState == "LogBF10" || BFtypeState == "BF10") && BFtype == "BF01")
				{
					BFtypeRequiresNewPlot <- TRUE
				}
				else if(BFtypeState == "BF01" && (BFtype == "LogBF10" || BFtype == "BF10"))
				{
					BFtypeRequiresNewPlot <- TRUE
				}
			}

			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$sampleSize==FALSE && diff$pearsonsR==FALSE && diff$priorWidth==FALSE && diff$hypothesis == FALSE))) && 
				"robustnessPlot" %in% state$plotTypes)
			{
				index <- which(state$plotTypes == "robustnessPlot")				
				plots.sumstats.correlation[[length(plots.sumstats.correlation)+1]] <- state$plotsCorrelationTest[[index]]
			}
			else
			{
				width  <- 530
				height <- 400

				plot <- list()

				plot[["title"]]  <- "Bayes Factor Robustness Check"
				plot[["width"]]  <- width
				plot[["height"]] <- height
				plot[["status"]] <- "waiting"

				image <- .beginSaveImage(width, height)
				.plotBF.robustnessCheck.correlation (r=options$pearsonsR, n=options$sampleSize, oneSided=oneSided, dontPlotData=FALSE,
													 kappa=options$priorWidth, BFH1H0=BFH1H0, BF=bayesFactorObject$bf)
				plot[["data"]] <- .endSaveImage(image)
				plot[["status"]] <- "complete"

				plots.sumstats.correlation[[length(plots.sumstats.correlation)+1]] <- plot
			}

			bayesFactorRobustnessPlot <- plots.sumstats.correlation[[length(plots.sumstats.correlation)]]
			plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
		}
	}
	else #init state
	{
		if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
			diff$sampleSize==FALSE && diff$pearsonsR==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && !is.null(state$bayesFactorObject))
		{
			rowsCorrelationBayesianPairs <- state$rowsCorrelationBayesianPairs
			bayesFactorObject <- state$bayesFactorObject
		}
		else
		{
			rowsCorrelationBayesianPairs <- .checkInputSummaryStatsCorrelationPairs(options)$row
		}

		if(options$plotPriorAndPosterior)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$sampleSize==FALSE && diff$pearsonsR==FALSE && diff$priorWidth==FALSE && diff$hypothesis == FALSE))) && 
				options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlotAddInfo
				index <- which(state$plotTypes == "posteriorPlotAddInfo")
				plot.sumstats.correlation[[length(plot.sumstats.correlation)+1]] <- state$plotsCorrelationTest[[index]]
				priorAndPosteriorPlotAddInfo <- priorAndPosteriorPlot
			}
			else if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$sampleSize==FALSE && diff$pearsonsR==FALSE && diff$priorWidth==FALSE && diff$hypothesis == FALSE))) && 
				!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlot
				index <- which(state$plotTypes == "posteriorPlot")
				plot.sumstats.correlation[[length(plot.sumstats.correlation)+1]] <- state$plotsCorrelationTest[[index]]
			}
			else
			{
				width  <- 530
				height <- 400

				plot <- list()

				plot[["title"]] <- "Prior and Posterior"
				plot[["width"]]  <- width
				plot[["height"]] <- height
				plot[["status"]] <- "waiting"
				
				image <- .beginSaveImage(width, height)
				.plotPosterior.correlation(r=NULL, n=NULL, oneSided=oneSided, dontPlotData=TRUE, addInformation=options$plotPriorAndPosteriorAdditionalInfo)
				plot[["data"]] <- .endSaveImage(image)

				plots.sumstats.correlation[[length(plots.sumstats.correlation)+1]] <- plot
			}

			priorAndPosteriorPlot <- plots.sumstats.correlation[[length(plots.sumstats.correlation)]]

			if(options$plotPriorAndPosteriorAdditionalInfo)
			{
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"
			}
			else
			{
				plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
			}
		}

		if(options$plotBayesFactorRobustness)
		{
			BFtypeRequiresNewPlot <- TRUE
			
			if (!(is.null(state)))
			{
				BFtypeRequiresNewPlot <- FALSE
				BFtype <- options$bayesFactorType
				BFtypeState <- state$options$bayesFactorType
				
				if ((BFtypeState == "LogBF10" || BFtypeState == "BF10") && BFtype == "BF01")
				{
					BFtypeRequiresNewPlot <- TRUE
				}
				else if(BFtypeState == "BF01" && (BFtype == "LogBF10" || BFtype == "BF10"))
				{
					BFtypeRequiresNewPlot <- TRUE
				}
			}

			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$sampleSize==FALSE && diff$pearsonsR==FALSE && diff$priorWidth==FALSE && diff$hypothesis == FALSE))) && 
				"robustnessPlot" %in% state$plotTypes)
			{
				index <- which(state$plotTypes == "robustnessPlot")				
				plots.sumstats.correlation[[length(plots.sumstats.correlation)+1]] <- state$plotsCorrelationTest[[index]]
			}
			else
			{
				width  <- 530
				height <- 400

				plot[["title"]]  <- "Bayes Factor Robustness Check"
				plot[["width"]]  <- width
				plot[["height"]] <- height
				plot[["status"]] <- "waiting"

				image <- .beginSaveImage(width, height)
				.plotBF.robustnessCheck.correlation (oneSided= oneSided, BFH1H0= (options$bayesFactorType == "BF10"), dontPlotData= TRUE)
				plot[["data"]] <- .endSaveImage(image)

				plots.sumstats.correlation[[length(plots.sumstats.correlation)+1]] <- plot
			}

			bayesFactorRobustnessPlot <- plots.sumstats.correlation[[length(plots.sumstats.correlation)]]
			plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
		}
	}

	table[["data"]] <- list(rowsCorrelationBayesianPairs)

	results[["table"]] <- table
	
	if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness)
	{
		results[["inferentialPlots"]] <- list(title=ifelse(sum(c(options$plotPriorAndPosterior, options$plotBayesFactorRobustness)) > 1,"Inferential Plots", "Inferential Plot"), PriorPosteriorPlot=priorAndPosteriorPlot, BFrobustnessPlot=bayesFactorRobustnessPlot)
	}

	keep <- NULL
	
	for (plot in plots.sumstats.correlation)
		keep <- c(keep, plot$data)

	if (perform == "init")
	{
		return(list(results=results, status="inited", state=state, keep=keep))
	}
	else
	{
		return(list(results=results, status="complete", state=list(options=options, results=results, bayesFactorObject=bayesFactorObject, bayesFactorRobustnessPlot=bayesFactorRobustnessPlot, priorAndPosteriorPlot=priorAndPosteriorPlot, 
					rowsCorrelationBayesianPairs=rowsCorrelationBayesianPairs, plotsCorrelationTest=plots.sumstats.correlation, priorAndPosteriorPlotAddInfo=priorAndPosteriorPlotAddInfo, plotTypes=plotTypes), keep=keep))
	}
}

.checkInputSummaryStatsCorrelationPairs <- function(options)
{
	ready <- TRUE

	sampleSizeValue <- options$sampleSize
	pearsonsRValue <- options$pearsonsR

	if(options$sampleSize==0 || is.null(options$sampleSize))
	{
		ready <- FALSE
		sampleSizeValue <- "."
	}

	if(is.null(options$pearsonsR))
	{
		ready <- FALSE
		pearsonsRValue <- "p."
	}

	row <- list(BF = ".", sampleSize = sampleSizeValue, pearsonsR = pearsonsRValue)
	
	list(ready=ready, row=row)	
}


.calcluateBFCorrelationPairs <- function(options, state, diff)
{
	some.n <- options$sampleSize
	some.r <- options$pearsonsR

	all.bfs <- list(bf10=NA, bfPlus0=NA, bfMin0=NA)
	method.number <- 1
	
	while (any(is.na(c(all.bfs$bf10, all.bfs$bfPlus0, all.bfs$bfMin0))) && method.number <= 4)
	{
		# Note: Try all normal methods
		all.bfs <- .bfCorrieKernel(n=some.n, r=some.r, kappa=options$priorWidth, method=method.number)
		method.number <- method.number + 1
	}
	
	if (any(is.na(all.bfs)))
	{
		# Note: all normal methods FAILED. Use Jeffreys approximation
		all.bfs <- .bfCorrieKernel(n=some.n, r=some.r, kappa=options$priorWidth, method="jeffreysApprox")
	}
	
	some.bf10 <- all.bfs$bf10
	some.bfPlus0 <- all.bfs$bfPlus0
	some.bfMin0 <- all.bfs$bfMin0

	if (options$hypothesis == "correlated")
	{
		some.bf <- some.bf10
	}
	else if (options$hypothesis == "correlatedPositively")
	{
		some.bf <- some.bfPlus0
	}
	else if (options$hypothesis == "correlatedNegatively")
	{
		some.bf <- some.bfMin0
	}

	list(bf=some.bf)
}


