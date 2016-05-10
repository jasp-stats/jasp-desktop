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

BFFromTIndependentSamples <- function(dataset=NULL, options, perform = 'run', callback)
{
	results <- list()

	meta <- list()

	meta[[1]] <- list(name="table", type="table")
	meta[[2]] <- list(name="inferentialPlots", type="object", meta=list(list(name="PriorPosteriorPlot", type="image"),
																		list(name="BFrobustnessPlot", type="image")))

	results[[".meta"]] <- meta
	results[["title"]] <- "Summary Statistics"


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

	#proportional error estimate on the Bayes factor
	fields[[length(fields)+1]] <- list(name="errorEstimate", type="number", format="sf:4;dp:3", title="Error estimate")

	table <- list()
	table[["title"]] <- "BF from <i>t</i> - Independent Samples"
	table[["schema"]] <- list(fields=fields)
	table[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")

	status <- .isInputValidIndependentSamples(options) #check validity of data
	if(status$ready)                           #check if data has been entered
	{
		if(status$error)
		{
			table[["error"]] <- list(errorType = "badData", errorMessage = status$errorMessage)
		}
		else
		{
			bayesFactor10 <- .calcluateBFIndependentSamples(options) #calculate Bayes factor from t value

			if(options$bayesFactorType == "BF10")
			{
				row <- list(BF = .clean(exp(bayesFactor10$bf)), tStatistic = options$tStatistic, n1Size = options$n1Size, n2Size = options$n2Size, errorEstimate = .clean(bayesFactor10$properror))
			}
			else if(options$bayesFactorType == "BF01")
			{
				row <- list(BF = .clean(1/exp(bayesFactor10$bf)), tStatistic = options$tStatistic, n1Size = options$n1Size, n2Size = options$n2Size, errorEstimate = .clean(bayesFactor10$properror))
			}
			else
			{
				row <- list(BF = .clean(bayesFactor10$bf), tStatistic = options$tStatistic, n1Size = options$n1Size, n2Size = options$n2Size, errorEstimate = .clean(bayesFactor10$properror))
			}

			table[["data"]] <- list(row)
		}
	}

	#add footnotes to the analysis result
	footnotes <- .newFootnotes()
	message <- paste("The prior width used is ", options$priorWidth, sep="")
	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	table[["footnotes"]] <- as.list(footnotes)

	results[["table"]] <- table

	bayesFactorRobustnessPlot <- NULL
	priorAndPosteriorPlot <- NULL

	#add Bayes factor Robustness and Prior & posterior plots to result if needed
	if (options$plotBayesFactorRobustness)
	{
		width  <- 530
		height <- 400

		plot <- list()

		plot[["title"]]  <- "Bayes Factor Robustness Check"
		plot[["width"]]  <- width
		plot[["height"]] <- height
		plot[["status"]] <- "waiting"

		image <- .beginSaveImage(width, height)
		.plotBF.robustnessCheck.bffromt (t=options$tStatistic, n1=options$n1Size, n2=0, BFH1H0=(options$bayesFactorType == "BF10"), dontPlotData= FALSE, rscale=options$priorWidth, BF10post = ifelse((options$bayesFactorType == "BF10"),.clean(exp(bayesFactor10$bf)), .clean(1/exp(bayesFactor10$bf))), oneSided = FALSE)
		plot[["data"]]   <- .endSaveImage(image)

		plot[["status"]] <- "complete"

		bayesFactorRobustnessPlot <- plot
	}

	results[["inferentialPlots"]] <- list(title="Inferential Plots", PriorPosteriorPlot=priorAndPosteriorPlot, BFrobustnessPlot=bayesFactorRobustnessPlot)
	
	list(results=results, status="complete")
}


.calcluateBFIndependentSamples <- function(options)
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

	list(bf=bf10$bf, properror=bf10$properror)
}


# checks if the input given is valid
.isInputValidIndependentSamples <- function(options)
{
	error <- FALSE
	errorMessage <- NULL
	ready <- TRUE

	if(is.null(options$tStatistic))
	{
		error <- TRUE
		errorMessage <- paste("argument \"<em>t</em>\" is missing", sep="")
	}
	else if(is.null(options$n1Size))
	{
		error <- TRUE
		errorMessage <- paste("argument \"<em>n<sub>1</sub></em>\" is missing", sep="")
	}
	else if(is.null(options$n1Size))
	{
		error <- TRUE
		errorMessage <- paste("argument \"<em>n<sub>2</sub></em>\" is missing", sep="")
	}

	if(!error)
	{
		if(options$n1Size < 2)
		{
			error <- TRUE
			errorMessage <-paste("argument \"<em>n<sub>1</sub></em>\" : not enough observations", sep="")
		}
		else if(options$n2Size < 2)
		{
			error <- TRUE
			errorMessage <-paste("argument \"<em>n<sub>2</sub></em>\" : not enough observations", sep="")
		}
	}

	if(options$tStatistic==0 && options$n1Size==0 && options$n2Size==0)
	{
		ready <- FALSE
	}

	list(error=error, errorMessage=errorMessage, ready=ready)
}
