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

RegressionBayesianSummaryStatistics <- function(dataset=NULL, options, perform = 'run', callback = function(...) 0,  ...)
{
	state <- .retrieveState()

	diff <- NULL

	if (!is.null(state))     #difference between the previous state variables(options) and current options
	{
		diff <- .diff(options, state$options)
	}

	run <- (perform == "run")


	results <- list()

	meta <- list()
	meta[[1]] <- list(name="table", type="table")
	meta[[2]] <- list(name="inferentialPlots", type="object", meta=list(list(name="PriorPosteriorPlot", type="image")))

	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Binomial Test"

	fields=list()
	fields[[length(fields)+1]] <- list(name="trials", type="integer", title="Trials")
	fields[[length(fields)+1]] <- list(name="successes", type="integer", title="Successes")
	fields[[length(fields)+1]] <- list(name="testValue", type="number", title="Test value")

	#Bayes factor type (BF10, BF01, log(BF10))
	if (options$bayesFactorType == "BF01")
	{
		BFH1H0 <- FALSE

		if (options$hypothesis == "notEqualToTestValue")
		{
			bf.title <- "BF\u2080\u2081"
		}
		else if (options$hypothesis == "greaterThanTestValue")
		{
			bf.title <- "BF\u2080\u208A"
		}
		else if (options$hypothesis == "lessThanTestValue")
		{
			bf.title <-  "BF\u2080\u208B"
		}	
	}
	else if (options$bayesFactorType == "BF10")
	{
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "notEqualToTestValue")
		{
			bf.title <- "BF\u2081\u2080"
		}
		else if (options$hypothesis == "greaterThanTestValue")
		{
			bf.title <- "BF\u208A\u2080"
		}
		else if (options$hypothesis == "lessThanTestValue")
		{
			bf.title <- "BF\u208B\u2080"
		}	
	}
	else if (options$bayesFactorType == "LogBF10")
	{
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "notEqualToTestValue")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
		}
		else if (options$hypothesis == "greaterThanTestValue")
		{
			bf.title <-"Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
		}
		else if (options$hypothesis == "lessThanTestValue")
		{
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
		}
	}

	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title=bf.title)

	table <- list()
	table[["title"]] <- "Bayesian Binomial Test"
	table[["schema"]] <- list(fields=fields)
	table[["citation"]] <- list("Jeffreys, H. (1961). Theory of Probability. Oxford, Oxford University Press.",
								"O’Hagan, A., & Forster, J. (2004). Kendall’s advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.). London: Arnold.",
								"Haldane, J. B. S. (1932). A note on inverse probability. Mathematical Proceedings of the Cambridge Philosophical Society, 28, 55-61.")


	#add footnotes to the analysis result
	footnotes <- .newFootnotes()
	
	if (options$hypothesis == "notEqualToTestValue")
	{
		hyp <- "two.sided"
		message <- paste0("Proportions tested against value: ", options$testValue, ".")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}
	else if (options$hypothesis == "greaterThanTestValue")
	{
		hyp <- "greater"
		note <- "For all tests, the alternative hypothesis specifies that the proportion
					is greater than "
		message <- paste0(note, options$testValue, ".")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}
	else
	{
		hyp <- "less"
		note <- "For all tests, the alternative hypothesis specifies that the proportion
					is less than "
		message <- paste0(note, options$testValue, ".")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
	}

	table[["footnotes"]] <- as.list(footnotes)

	#check if input data is valid
	errorMessageTable <- NULL
	
	if (options$testValue == 1 && hyp == "greater")
	{
		errorMessageTable <- "Cannot test the hypothesis that the test value is greater than 1."
	}
	else if (options$testValue == 0 && hyp == "less")
	{
		errorMessageTable <- "Cannot test the hypothesis that the test value is less than 0."
	}

	if (!is.null(errorMessageTable))
	{
		table[["error"]] <- list(errorType = "badData", errorMessage = errorMessageTable)
	}


	data <- list()
	rowsBinomtest <- list()
	priorAndPosteriorPlot <- NULL

	if(perform=="run")
	{
		if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && diff$successes==FALSE && diff$trials==FALSE && diff$testValue==FALSE && diff$betaPriorParamA == FALSE && diff$betaPriorParamB==FALSE && diff$hypothesis == FALSE))))
		{
			row <- state$rowsBinomtest
			BF10 <- state$BF10
		}
		else
		{
			BF10 <- .bayesBinomialTest(counts=options$successes, n=options$trials, theta0=options$testValue, hypothesis = hyp, a = options$betaPriorParamB, b = options$betaPriorParamB)
			BF <- BF10

			if (options$bayesFactorType == "BF01")
			{
				BF <- 1/BF10
			} 
			else if(options$bayesFactorType == "LogBF10")
			{
				BF <- log(BF10)
			}
			
			row <- list(successes=.clean(options$successes), trials=.clean(options$trials), testValue=.clean(options$testValue), BF=.clean(BF))
		}

		data <- row
		rowsBinomtest <- row

		###### plots ######
		if(options$plotPriorAndPosterior)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && diff$successes==FALSE && diff$trials==FALSE && diff$testValue==FALSE && diff$betaPriorParamA == FALSE && diff$betaPriorParamB==FALSE && diff$hypothesis == FALSE && diff$plotPriorAndPosteriorAdditionalInfo==FALSE))) && !is.null(state$priorAndPosteriorPlot))
			{
				plot <- state$priorAndPosteriorPlot
			}
			else
			{
				plot <- list()
			
				plot[["title"]] <- "Prior and Posterior"
				plot[["width"]]  <- 530
				plot[["height"]] <- 400
			
				p <- try(silent=FALSE, expr= {
							
							image <- .beginSaveImage(530, 400)
							.plotPosterior.binomTest(counts=options$successes, n=options$trials, theta0=options$testValue, a = options$betaPriorParamA, b = options$betaPriorParamB, BF10=BF10, hypothesis = hyp,
								addInformation = options$plotPriorAndPosteriorAdditionalInfo)
							plot[["data"]] <- .endSaveImage(image)
							
						})
						
				if (class(p) == "try-error")
				{
					errorMessage <- .extractErrorMessage(p)
					plot[["error"]] <- list(error="badData", errorMessage= paste("Plotting is not possible:", errorMessage))
				}
			}

			priorAndPosteriorPlot <- plot
		}
	}
	else #init phase
	{
		if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && diff$successes==FALSE && diff$trials==FALSE && diff$testValue==FALSE && diff$betaPriorParamA == FALSE && diff$betaPriorParamB==FALSE && diff$hypothesis == FALSE))))
		{
			data <- state$rowsBinomtest
			rowsBinomtest <- state$rowsBinomtest
			BF10 <- state$BF10
		}
		else
		{
			data <- list(successes=".", trials=".", testValue=".", BF=".")
			rowsBinomtest <- list(successes=".", trials=".", testValue=".", BF=".")
		}

		if(options$plotPriorAndPosterior)
		{
			if (!is.null(state) && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$bayesFactorType==FALSE && diff$successes==FALSE && diff$trials==FALSE && diff$testValue==FALSE && diff$betaPriorParamA == FALSE && diff$betaPriorParamB==FALSE && diff$hypothesis == FALSE && diff$plotPriorAndPosteriorAdditionalInfo==FALSE))) && !is.null(state$priorAndPosteriorPlot))
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
				.plotPosterior.binomTest(BF10 = 1, dontPlotData = TRUE, addInformation = options$plotPriorAndPosteriorAdditionalInfo)
				plot[["data"]] <- .endSaveImage(image)
			}

			priorAndPosteriorPlot <- plot
		}
	}

	table[["data"]] <- list(data)


	if (options$plotPriorAndPosterior)
	{
		results[["inferentialPlots"]] <- list(title="Inferential Plot", PriorPosteriorPlot=priorAndPosteriorPlot)
	}

	results[["table"]] <- table


	if (perform == "init")
	{
		return(list(results=results, status="inited", state=state))
	}
	else
	{
		return(list(results=results, status="complete", state=list(options=options, results=results, priorAndPosteriorPlot=priorAndPosteriorPlot, rowsBinomtest=rowsBinomtest, BF10=BF10)))
	}
}
