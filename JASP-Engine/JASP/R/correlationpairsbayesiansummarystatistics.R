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

CorrelationPairsBayesianSummaryStatistics <- function(dataset=NULL, options, perform = 'run', callback = function(...) 0,  ...)
{
	run <- (perform == "run")

	results <- list()

	meta <- list()

	meta[[1]] <- list(name="table", type="table")

	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Correlation Pairs"

	state <- .retrieveState()

	diff <- NULL

	if (!is.null(state))     #difference between the previous state variables(options) and current options
	{
		diff <- .diff(options, state$options)
	}

	fields=list()
	fields[[length(fields)+1]] <- list(name="sampleSize", type="number", title="Sample size")	
	fields[[length(fields)+1]] <- list(name="r", type="number", format="sf:4;dp:3", title="r")

	#Bayes factor type (BF10, BF01, log(BF10))
	bf.type <- options$bayesFactorType
	
	if (bf.type == "BF10") {
	
		BFH1H0 <- TRUE
	
		if (options$hypothesis == "correlated") {
		
			bf.title <- "BF\u2081\u2080"
			oneSided <- FALSE
			
		} else if (options$hypothesis == "correlatedPositively") {
		
			bf.title <- "BF\u208A\u2080"
			oneSided <- "right"
			
		} else if (options$hypothesis == "correlatedNegatively") {
		
			bf.title <- "BF\u208B\u2080"
			oneSided <- "left"
		}
		
	} else if (bf.type == "LogBF10") {
		
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "correlated") {
		
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
			oneSided <- FALSE
			
		} else if (options$hypothesis == "correlatedPositively") {
		
			bf.title <- "Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
			oneSided <- "right"
			
		} else if (options$hypothesis == "correlatedNegatively") {
		
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
			oneSided <- "left"
		}
		
	} else if (bf.type == "BF01") {
	
		BFH1H0 <- FALSE
	
		if (options$hypothesis == "correlated") {
		
			bf.title <- "BF\u2080\u2081"
			oneSided <- FALSE
			
		} else if (options$hypothesis == "correlatedPositively") {
		
			bf.title <- "BF\u2080\u208A"
			oneSided <- "right"
			
		} else if (options$hypothesis == "correlatedNegatively") {
		
			bf.title <- "BF\u2080\u208B"
			oneSided <- "left"
		}
	}

	fields[[length(fields)+1]] <- list(name="BF", type="number", format="sf:4;dp:3", title=bf.title)


	table <- list()
	table[["title"]] <- "Bayesian Pearson Correlation"
	table[["schema"]] <- list(fields=fields)
	table[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")

	BF10post <- NULL
	bayesFactor10 <- NULL

	#status <- .isInputValidIndependentSamples(options) #check validity of data
	#if(status$ready)                           #check if data has been entered
	{
	#	if(status$error)
		{
	#		table[["error"]] <- list(errorType = "badData", errorMessage = status$errorMessage)
		}
	#	else
		{
			bayesFactor10 <- .calcluateBFCorrelationPairs(options, state, diff) #calculate Bayes factor from t value

			row <- list(r=.clean(options$pearsonsR), BF=.clean(bayesFactor10$bf), sampleSize = options$sampleSize)

			table[["data"]] <- list(row)
		}
	}

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

	results[["table"]] <- table


	if (perform == "init")
	{
		return(list(results=results, status="inited"))
	}
	else
	{
		return(list(results=results, status="complete", state=list(options=options, results=results, bayesFactor10=bayesFactor10)))
	}
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

	properror <- 0.1

	list(bf=some.bf10, properror=properror)
}


