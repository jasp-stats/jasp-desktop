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

SummaryStatsTTestBayesianPairedSamples <- function(dataset=NULL, options, perform = 'run', callback)
{
	run <- (perform == "run")

	results <- list()

	meta <- list()

	meta[[1]] <- list(name="table", type="table")
	meta[[2]] <- list(name="inferentialPlots", type="object", meta=list(list(name="PriorPosteriorPlot", type="image"),
																		list(name="BFrobustnessPlot", type="image")))

	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian T-Test"

	state <- .retrieveState()

	diff <- NULL

	if (!is.null(state))     #difference between the previous state variables(options) and current options
	{
		diff <- .diff(options, state$options)
	}

	fields=list()
	fields[[length(fields)+1]] <- list(name="tStatistic", type="number", format="sf:4;dp:3", title="t")
	fields[[length(fields)+1]] <- list(name="n1Size", type="number", title="n")

	#Bayes factor type (BF10, BF01, log(BF10))
	if (options$bayesFactorType == "BF01")
	{
		BFH1H0 <- FALSE

		if (options$hypothesis == "groupsNotEqual")
		{
			bf.title <- "BF\u2080\u2081"
		}
		else if (options$hypothesis == "groupOneGreater")
		{
			bf.title <- "BF\u2080\u208A"
		}
		else if (options$hypothesis == "groupTwoGreater")
		{
			bf.title <-  "BF\u2080\u208B"
		}	
	}
	else if (options$bayesFactorType == "BF10")
	{
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "groupsNotEqual")
		{
			bf.title <- "BF\u2081\u2080"
		}
		else if (options$hypothesis == "groupOneGreater")
		{
			bf.title <- "BF\u208A\u2080"
		}
		else if (options$hypothesis == "groupTwoGreater")
		{
			bf.title <- "BF\u208B\u2080"
		}	
	}
	else if (options$bayesFactorType == "LogBF10")
	{
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "groupsNotEqual")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
		}
		else if (options$hypothesis == "groupOneGreater")
		{
			bf.title <-"Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
		}
		else if (options$hypothesis == "groupTwoGreater")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
		}
	}

	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title=bf.title)


	table <- list()
	table[["title"]] <- "Bayesian Paired Samples T-Test"
	
	table[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")

	#add footnotes to the analysis result
	footnotes <- .newFootnotes()
	if (options$hypothesis == "groupOneGreater")
	{
		message <- paste("For all tests, the alternative hypothesis specifies that measure 1 is greater than measure 2", sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}
	else if(options$hypothesis == "groupTwoGreater")
	{
		message <- paste("For all tests, the alternative hypothesis specifies that measure 1 is lesser than measure 2", sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}

	table[["footnotes"]] <- as.list(footnotes)

	if(options$hypothesis == "groupsNotEqual")
	{
		oneSidedHypothesis <- FALSE
	}
	else if (options$hypothesis == "groupOneGreater")
	{
		oneSidedHypothesis <- "right"
	}
	else
	{
		oneSidedHypothesis <- "left"
	}

	rowsTTestBayesianPairedSamples <- list()
	bayesFactorObject <- NULL
	priorAndPosteriorPlot <- NULL
	priorAndPosteriorPlotAddInfo <- NULL
	bayesFactorRobustnessPlot <- NULL
	plots.sumstats.ttest <- list()
	plotTypes <- list()


	if(perform=="run")
	{
		if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
			diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE &&
			diff$hypothesis==FALSE))) && !is.null(state$bayesFactorObject))
		{
			rowsTTestBayesianPairedSamples <- state$rowsTTestBayesianPairedSamples
			bayesFactorObject <- state$bayesFactorObject
		}
		else
		{
			status <- .isInputValidPairedSamples(options) #check validity of data
			row <- status$row

			if(status$ready)                           #check if data has been entered
			{
				bayesFactor10 <- .calcluateBFPairedSamples(options, state, diff) #calculate Bayes factor from t value

				if(options$bayesFactorType == "BF10")
				{
					BF <- .clean(exp(bayesFactor10$bf))
				}
				else if(options$bayesFactorType == "BF01")
				{
					BF <- .clean(1/exp(bayesFactor10$bf))
				}
				else
				{
					BF <- .clean(bayesFactor10$bf)
				}

				bayesFactorObject <- bayesFactor10
				row <- list(BF=BF, tStatistic=options$tStatistic, n1Size=options$n1Size, errorEstimate=.clean(bayesFactor10$properror))
			}

			rowsTTestBayesianPairedSamples <- row
		}


		if(options$plotPriorAndPosterior)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlotAddInfo
				index <- which(state$plotTypes == "posteriorPlotAddInfo")
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
				priorAndPosteriorPlotAddInfo <- priorAndPosteriorPlot
			}
			else if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlot
				index <- which(state$plotTypes == "posteriorPlot")
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
			}
			else
			{
				width  <- 530
				height <- 400

				plot <- list()

				plot[["title"]]  <- "Prior and Posterior"
				plot[["width"]]  <- width
				plot[["height"]] <- height
				plot[["status"]] <- "waiting"

				image <- .beginSaveImage(width, height)
				.plotPosterior.ttest.summaryStats (t=options$tStatistic, n1=options$n1Size, n2=NULL, paired=TRUE, BFH1H0=(options$bayesFactorType == "BF10"), 
												   dontPlotData= FALSE, rscale=options$priorWidth, addInformation = options$plotPriorAndPosteriorAdditionalInfo,
												   BF = exp(bayesFactorObject$bf), oneSided = oneSidedHypothesis)
				plot[["data"]]   <- .endSaveImage(image)
				plot[["status"]] <- "complete"

				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- plot
			}

			priorAndPosteriorPlot <- plots.sumstats.ttest[[length(plots.sumstats.ttest)]]

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
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				"robustnessPlot" %in% state$plotTypes)
			{
				index <- which(state$plotTypes == "robustnessPlot")				
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
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
				.plotBF.robustnessCheck.bffromt (t=options$tStatistic, n1=options$n1Size, n2=0, BFH1H0=(options$bayesFactorType == "BF10"), 
												 dontPlotData= FALSE, rscale=options$priorWidth, 
												 BF10post = ifelse((options$bayesFactorType == "BF10"),.clean(exp(bayesFactorObject$bf)), .clean(1/exp(bayesFactorObject$bf))), oneSided = oneSidedHypothesis)
				plot[["data"]]   <- .endSaveImage(image)
				plot[["status"]] <- "complete"

				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- plot
			}

			bayesFactorRobustnessPlot <- plots.sumstats.ttest[[length(plots.sumstats.ttest)]]
			plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
		}
	}
	else #init phase
	{
		if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
			diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE &&
			diff$hypothesis==FALSE))) && !is.null(state$bayesFactorObject))
		{
			rowsTTestBayesianPairedSamples <- state$rowsTTestBayesianPairedSamples
			bayesFactorObject <- state$bayesFactorObject
		}
		else
		{
			row <- .isInputValidPairedSamples(options)$row
			rowsTTestBayesianPairedSamples <- row
		}

		if(options$plotPriorAndPosterior)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlotAddInfo
				index <- which(state$plotTypes == "posteriorPlotAddInfo")
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
				priorAndPosteriorPlotAddInfo <- priorAndPosteriorPlot
			}
			else if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && diff$hypothesis==FALSE))) && 
				!options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes)
			{
				priorAndPosteriorPlot <- state$priorAndPosteriorPlot
				index <- which( state$plotTypes == "posteriorPlot")
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
			}
			else
			{
				plot <- list()
				
				plot[["title"]] <- "Prior and Posterior"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				
				image <- .beginSaveImage(530, 400)
				.plotPosterior.ttest.summaryStats(BF = 1, dontPlotData = TRUE, addInformation = options$plotPriorAndPosteriorAdditionalInfo)
				plot[["data"]] <- .endSaveImage(image)
			
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- plot
			}

			priorAndPosteriorPlot <- plots.sumstats.ttest[[length(plots.sumstats.ttest)]]

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
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$priorWidth == FALSE && 
				diff$hypothesis==FALSE))) && "robustnessPlot" %in% state$plotTypes)
			{
				index <- which(state$plotTypes == "robustnessPlot")				
				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- state$plotsTtest[[index]]
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
				.plotBF.robustnessCheck.ttest (oneSided= oneSidedHypothesis, BFH1H0= (options$bayesFactorType == "BF10"), dontPlotData= TRUE)
				plot[["data"]] <- .endSaveImage(image)

				plots.sumstats.ttest[[length(plots.sumstats.ttest)+1]] <- plot
			}

			bayesFactorRobustnessPlot <- plots.sumstats.ttest[[length(plots.sumstats.ttest)]]
			plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
		}
	}


	if(rowsTTestBayesianPairedSamples$errorEstimate!="NaN")
	{
		fields[[length(fields)+1]] <- list(name="errorEstimate", type="number", format="sf:4;dp:3", title="error %")
	}

	table[["schema"]] <- list(fields=fields)
	table[["data"]] <- list(rowsTTestBayesianPairedSamples)
	results[["table"]] <- table

	if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness)
	{
		results[["inferentialPlots"]] <- list(title=ifelse(sum(c(options$plotPriorAndPosterior, options$plotBayesFactorRobustness)) > 1,"Inferential Plots", "Inferential Plot"), PriorPosteriorPlot=priorAndPosteriorPlot, BFrobustnessPlot=bayesFactorRobustnessPlot)
	}


	keep <- NULL
	
	for (plot in plots.sumstats.ttest)
		keep <- c(keep, plot$data)


	if (perform == "init")
	{
		return(list(results=results, status="inited", state=state, keep=keep))
	}
	else
	{
		return(list(results=results, status="complete", state=list(options=options, results=results, bayesFactorObject=bayesFactorObject, bayesFactorRobustnessPlot=bayesFactorRobustnessPlot, priorAndPosteriorPlot=priorAndPosteriorPlot, 
					rowsTTestBayesianPairedSamples=rowsTTestBayesianPairedSamples, plotsTtest=plots.sumstats.ttest, priorAndPosteriorPlotAddInfo=priorAndPosteriorPlotAddInfo, plotTypes=plotTypes), keep=keep))
	}
}


.calcluateBFPairedSamples <- function(options, state, diff)
{
	if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && 
		(diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$tStatistic == FALSE && diff$n1Size==FALSE))))
	{
		bf10 <- state$bayesFactorObject
	}
	else
	{
		if (options$hypothesis == "groupsNotEqual")
		{
			nullInterval <- NULL
			oneSided <- FALSE
		}
		else if (options$hypothesis == "groupOneGreater")
		{
			nullInterval <- c(0, Inf)
			oneSided <- "right"
		}
		else if (options$hypothesis == "groupTwoGreater")
		{
			nullInterval <- c(-Inf, 0)
			oneSided <- "left"
		}

		bf10 <- BayesFactor::ttest.tstat(t = options$tStatistic, n1 = options$n1Size, n2 = 0, rscale = options$priorWidth, nullInterval = nullInterval)
	}

	list(bf=bf10$bf, properror=bf10$properror)
}


# checks if the input given is valid
.isInputValidPairedSamples <- function(options)
{
	ready <- TRUE 

	n1Value <- options$n1Size
	tStatValue <- options$tStatistic

	if(options$n1Size==0 || is.null(options$n1Size))
	{
		ready <- FALSE
		n1Value <- "."
	}

	if(is.null(options$tStatistic))
	{
		ready <- FALSE
		tStatValue <- "."
	}

	row <- list(BF = ".", tStatistic = tStatValue, n1Size = n1Value, errorEstimate = ".")

	list(ready=ready, row=row)
}
