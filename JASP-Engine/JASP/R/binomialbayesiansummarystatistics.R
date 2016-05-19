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

BinomialBayesianSummaryStatistics <- function(dataset=NULL, options, perform = 'run', callback)
{
	state <- .retrieveState()

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

	table <- list()
	table[["title"]] <- "Bayesian Binomial Test"
	table[["schema"]] <- list(fields=fields)
	table[["citation"]] <- list("Jeffreys, H. (1961). Theory of Probability. Oxford, Oxford University Press.",
								"O’Hagan, A., & Forster, J. (2004). Kendall’s advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.). London: Arnold.",
								"Haldane, J. B. S. (1932). A note on inverse probability. Mathematical Proceedings of the Cambridge Philosophical Society, 28, 55-61.")

	status <- .isInputValidBinomialTest(options) #check validity of data
	if(status$ready)                             #check if data has been entered
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



}
