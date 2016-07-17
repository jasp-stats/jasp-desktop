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

SummaryStatsTTestBayesianIndependentSamples <- function(dataset=NULL, options, perform = 'run', callback)
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
	fields[[length(fields)+1]] <- list(name="tStatistic", type="number", format="sf:4;dp:3", title="t value")
	fields[[length(fields)+1]] <- list(name="n1Size", type="number", title="n\u2081")
	fields[[length(fields)+1]] <- list(name="n2Size", type="number", title="n\u2082")

	#Bayes factor type (BF10, BF01, log(BF10))
	if(options$bayesFactorType == "BF10")
	{
		fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2081\u2080")
	}
	else if(options$bayesFactorType == "BF01")
	{
		fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="BF\u2080\u2081")
	}
	else
	{
		fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title="Log(BF\u2081\u2080)")
	}

	fields[[length(fields)+1]] <- list(name="errorEstimate", type="number", format="sf:4;dp:3", title="error %")

	table <- list()
	table[["title"]] <- "Bayesian Independent Samples T-Test"
	table[["schema"]] <- list(fields=fields)
	table[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")

	#add footnotes to the analysis result
	footnotes <- .newFootnotes()
	if (options$hypothesis == "groupOneGreater")
	{
		message <- paste("For all tests, the alternative hypothesis specifies that group 1 is greater than group 2", sep="")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}
	else if(options$hypothesis == "groupTwoGreater")
	{
		message <- paste("For all tests, the alternative hypothesis specifies that group 1 is lesser than group 2", sep="")
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

	rowsTTestBayesianIndependentSamples <- list()
	bayesFactorObject <- NULL
	priorAndPosteriorPlot <- NULL
	posteriorPlotAddInfo <- NULL
	bayesFactorRobustnessPlot <- NULL


	if(perform=="run")
	{
		if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
			diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$n2Size==FALSE && diff$priorWidth == FALSE &&
			diff$hypothesis==FALSE))) && !is.null(state$bayesFactorObject))
		{
			print("run state, if part")
			rowsTTestBayesianIndependentSamples <- state$rowsTTestBayesianIndependentSamples
			bayesFactorObject <- state$bayesFactorObject
		}
		else
		{
			print("run state, else part")
			status <- .isInputValidIndependentSamples(options) #check validity of data
			row <- status$row

			if(status$ready)                           #check if data has been entered
			{
				bayesFactor10 <- .calcluateBFIndependentSamples(options, state, diff) #calculate Bayes factor from t value

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
				row <- list(BF=BF, tStatistic=options$tStatistic, n1Size=options$n1Size, n2Size=options$n2Size, errorEstimate=.clean(bayesFactor10$properror))
			}

			rowsTTestBayesianIndependentSamples <- row
		}


		if(options$plotPriorAndPosterior)
		{

		}

		if(options$plotBayesFactorRobustness)
		{
#			print("inside bf robustnessCheck --- run")
			
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

			if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE 
						&& BFtypeRequiresNewPlot == FALSE ))) && !is.null(state$bayesFactorRobustnessPlot))
			{
				bayesFactorRobustnessPlot <- state$bayesFactorRobustnessPlot
				results[["inferentialPlots"]][["BFrobustnessPlot"]][["status"]] <- "complete"
			}
			else
			{
				results[["inferentialPlots"]][["BFrobustnessPlot"]][["status"]] <- "running"

				width  <- 530
				height <- 400

				plot <- list()

				plot[["title"]]  <- "Bayes Factor Robustness Check"
				plot[["width"]]  <- width
				plot[["height"]] <- height
				plot[["status"]] <- "waiting"

				image <- .beginSaveImage(width, height)
				.plotBF.robustnessCheck.bffromt (t=options$tStatistic, n1=options$n1Size, n2=options$n2Size, BFH1H0=(options$bayesFactorType == "BF10"), 
												 dontPlotData= FALSE, rscale=options$priorWidth, 
												 BF10post = ifelse((options$bayesFactorType == "BF10"),.clean(exp(bayesFactorObject$bf)), .clean(1/exp(bayesFactorObject$bf))), oneSided = oneSidedHypothesis)
				plot[["data"]]   <- .endSaveImage(image)
				plot[["status"]] <- "complete"

				bayesFactorRobustnessPlot <- plot
			}
		}
	}
	else #init phase
	{
		if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
			diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$n2Size==FALSE && diff$priorWidth == FALSE &&
			diff$hypothesis==FALSE))) && !is.null(state$bayesFactorObject))
		{
			print("init state, if part")
			rowsTTestBayesianIndependentSamples <- state$rowsTTestBayesianIndependentSamples
			bayesFactorObject <- state$bayesFactorObject
		}
		else
		{
			print("init state, else part")			
			row <- .isInputValidIndependentSamples(options)$row
			rowsTTestBayesianIndependentSamples <- row
		}

		if(options$plotPriorAndPosterior)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && 
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$n2Size==FALSE && diff$priorWidth == FALSE && 
				diff$hypothesis==FALSE && diff$plotPriorAndPosteriorAdditionalInfo==FALSE))) && !is.null(state$priorAndPosteriorPlot))
			{
				plot <- state$priorAndPosteriorPlot
			}
			else
			{
				plot <- list()
				
				plot[["title"]] <- "Prior and Posterior"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
				
				image <- .beginSaveImage(530, 400)
				.plotPosterior.ttest(BF10 = 1, dontPlotData = TRUE, addInformation = options$plotPriorAndPosteriorAdditionalInfo)
				plot[["data"]] <- .endSaveImage(image)
			}

			priorAndPosteriorPlot <- plot
		}

		if(options$plotBayesFactorRobustness)
		{
#			print("inside bf robustnessCheck init")
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
				diff$tStatistic==FALSE && diff$n1Size==FALSE && diff$n2Size==FALSE && diff$priorWidth == FALSE && 
				diff$hypothesis==FALSE && diff$plotBayesFactorRobustness==FALSE && BFtypeRequiresNewPlot==FALSE))) && !is.null(state$bayesFactorRobustnessPlot))
			{
				plot <- state$bayesFactorRobustnessPlot
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

				bayesFactorRobustnessPlot <- plot
			}
		}
	}

	table[["data"]] <- list(rowsTTestBayesianIndependentSamples)
	results[["table"]] <- table

	if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness)
	{
		results[["inferentialPlots"]] <- list(title="Inferential Plots", PriorPosteriorPlot=priorAndPosteriorPlot, BFrobustnessPlot=bayesFactorRobustnessPlot)
	}

	results[["inferentialPlots"]] <- list(title="Inferential Plots", PriorPosteriorPlot=priorAndPosteriorPlot, BFrobustnessPlot=bayesFactorRobustnessPlot)

	if (perform == "init")
	{
		return(list(results=results, status="inited", state=state))
	}
	else
	{
		return(list(results=results, status="complete", state=list(options=options, results=results, bayesFactorObject=bayesFactorObject, bayesFactorRobustnessPlot=bayesFactorRobustnessPlot, priorAndPosteriorPlot=priorAndPosteriorPlot, rowsTTestBayesianIndependentSamples=rowsTTestBayesianIndependentSamples)))
	}
}


.calcluateBFIndependentSamples <- function(options, state, diff)
{
	if(!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && 
		(diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$tStatistic == FALSE && diff$n1Size==FALSE && diff$n2Size==FALSE))))
	{
		bf10 <- state$bayesFactorObject$bf
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

		if (oneSided == FALSE)
		{
			nullInterval <- NULL
		}
		else if (oneSided == "right")
		{
			nullInterval <- c(0, Inf)
		}
		else if (oneSided == "left")
		{
			nullInterval <- c(-Inf, 0)
		}

		bf10 <- BayesFactor::ttest.tstat(t = options$tStatistic, n1 = options$n1Size, n2 = options$n2Size, rscale = options$priorWidth, nullInterval = nullInterval)
	}

	list(bf=bf10$bf, properror=bf10$properror)
}


# checks if the input given is valid
.isInputValidIndependentSamples <- function(options)
{
	ready <- TRUE 

	n1Value <- options$n1Size
	n2Value <- options$n2Size
	tStatValue <- options$tStatistic

	if(options$n1Size==0 || is.null(options$n1Size))
	{
		ready <- FALSE
		n1Value <- "."
	}

	if(options$n2Size==0 || is.null(options$n2Size))
	{
		ready <- FALSE
		n2Value <- "."
	}

	if(is.null(options$tStatistic))
	{
		ready <- FALSE
		tStatValue <- "."
	}

	row <- list(BF = ".", tStatistic = tStatValue, n1Size = n1Value, n2Size = n2Value, errorEstimate = ".")

	list(ready=ready, row=row)
}
