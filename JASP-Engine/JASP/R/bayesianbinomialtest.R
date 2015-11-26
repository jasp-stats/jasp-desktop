#
# Copyright (C) 2015 University of Amsterdam
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

BayesianBinomialTest <- function(dataset = NULL, options, perform = "run",
						   callback = function(...) 0,  ...) {

	variables <- unlist(options$variables)

	if (is.null(dataset)) {

		if (perform == "run") {

			dataset <- .readDataSetToEnd(columns.as.numeric=NULL, columns.as.factor=variables, exclude.na.listwise=NULL)

		} else {

			dataset <- .readDataSetHeader(columns.as.numeric=NULL, columns.as.factor=variables)
		}

	} else {

		dataset <- .vdf(dataset, columns.as.numeric=NULL, columns.as.factor=variables)
	}

	results <- list()

	results[["title"]] <- "Bayesian Binomial Test"

	meta <- list(list(name="binomial", type="table"))
	results[[".meta"]] <- meta

	table <- list()

	table[["title"]] <- "Bayesian Binomial Test"
	
	if (options$bayesFactorType == "BF01") {
		
		if (options$hypothesis == "notEqualToTestValue"){
			bf.title <- "BF\u2080\u2081"
		}
		if (options$hypothesis == "greaterThanTestValue"){
			bf.title <- "BF\u2080\u208A"
		}
		if (options$hypothesis == "lessThanTestValue"){
			bf.title <-  "BF\u2080\u208B"
		}
		
	} else if (options$bayesFactorType == "BF10") {
		
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "notEqualToTestValue"){
			bf.title <- "BF\u2081\u2080"
		}
		if (options$hypothesis == "greaterThanTestValue"){
			bf.title <- "BF\u208A\u2080"
		}
		if (options$hypothesis == "lessThanTestValue"){
			bf.title <- "BF\u208B\u2080"
		}
		
	} else if (options$bayesFactorType == "LogBF10") {
		
		BFH1H0 <- TRUE
		
		if (options$hypothesis == "notEqualToTestValue"){
			bf.title <- "Log(\u2009\u0042\u0046\u2081\u2080\u2009)"
		}
		if (options$hypothesis == "greaterThanTestValue"){
			bf.title <-"Log(\u2009\u0042\u0046\u208A\u2080\u2009)"
		}
		if (options$hypothesis == "lessThanTestValue"){
			bf.title <- "Log(\u2009\u0042\u0046\u208B\u2080\u2009)"
		}
	}
	
	schema <- list(fields=list(
		list(name="case", title="", type="string", combine=TRUE),
		list(name="level", type="string"),
		list(name="counts", type="integer"),
		list(name="total", type="integer"),
		list(name="proportion", type="number", format="sf:4;dp:3"),
		list(name="BF", type="number", format="sf:4;dp:3", title = bf.title)
		))

	table[["schema"]] <- schema

	data <- list()
	
	if (perform == "run" && !is.null(variables)) {

		for (var in variables) {

			d <- dataset[[.v(var)]]
			d <- d[!is.na(d)]
			
			levels <- levels(d)
			n <- length(d)
						
			for (lev in levels) {
				
				counts <- sum(d == lev)
				prop <- counts/n

				if (options$hypothesis == "notEqualToTestValue") {
					hyp <- "two.sided"
				} else if (options$hypothesis == "greaterThanTestValue") {
					hyp <- "greater"
				} else {
					hyp <- "less"
				}
				
				BF10 <- .bayesBinomialTest(counts, n, options$testValue, hypothesis = hyp, a = 1, b = 1)
				
				# TODO: If try-error extract error message and display
				
				BF <- BF10
				
				if (options$bayesFactorType == "BF01") {
					BF <- 1/BF10
				} else if(options$bayesFactorType == "LogBF10") {
					BF <- log(BF10)
				}
				
				row <- list(case=var, level=lev, counts=.clean(counts), total=.clean(n), proportion=.clean(prop), BF=.clean(BF))
				
				if (lev == levels[1]) {
					row[[".isNewGroup"]] <- TRUE
				} else {
					row[[".isNewGroup"]] <- FALSE
				}
				
				data[[length(data) + 1]] <- row
			}
		}
		
	} else {
		
		if (is.null(variables))
			variables <- ""
	
		for (var in variables)
			data[[length(data) + 1]] <- list(case=var, level=".", counts=".", total=".",  proportion=".", p=".")
		
	}

	table[["data"]] <- data
	
	table[["footnotes"]] <- list(list(symbol="<i>Note.</i>", text=paste("proportions tested against value:", options$testValue)))

	results[["binomial"]] <- table

	results
} 

#.bayesBinomialTest.twoSided.jeffreys <- function(counts, n, theta0) {
#	# assuming a = b = 1, i.e., uniform prior
#	
#	logBF01 <- lgamma(n + 2) - lgamma(counts + 1) - lgamma(n - counts + 1) + counts*log(theta0) + (n - counts)*log(1 - theta0)
#	BF10 <- 1 / exp(logBF01)
#	
#	return(BF10)
#	
#}

.bayesBinomialTest.twoSided <- function(counts, n, theta0, a, b) {
	
	logBF10 <- lbeta(counts+a, n-counts+b) -  lbeta(a, b) - counts*log(theta0) - (n-counts)*log(1-theta0) 
	BF10 <- exp(logBF10)
	
	return(BF10)
	
}

.bayesBinomialTest.oneSided <- function(counts, n, theta0, a, b, hypothesis) {
	
	if (hypothesis == "less") {
		
		lowerTail <- TRUE

	} else if(hypothesis == "greater") {
		
		lowerTail <- FALSE

	}

	logMLikelihoodH0 <- counts*log(theta0) + (n - counts)*log(1 - theta0)
	term1 <- pbeta(theta0, a + counts, b + n - counts, lower.tail = lowerTail, log.p = TRUE) +
		lbeta(a + counts, b + n - counts)
	term2 <- lbeta(a,b) + pbeta(theta0, a, b, lower.tail = lowerTail, log.p = TRUE)
	logMLikelihoodH1 <- term1-term2
	BF10 <- exp(logMLikelihoodH1 - logMLikelihoodH0)
	
	return(BF10)
	
}

.bayesBinomialTest <- function(counts, n, theta0, hypothesis, a, b) {

	if (hypothesis == "two.sided") {
		
		BF10 <- try(.bayesBinomialTest.twoSided(counts, n, theta0, a, b), silent = TRUE)
		
	} else {
		
		if(theta0 == 0 || theta0 == 1) {
			
			BF10 <- NA
			
		} else {
			
			BF10 <- try(.bayesBinomialTest.oneSided(counts, n, theta0, a, b, hypothesis), silent = TRUE)
			
		}
	}
	
	if (class(BF10) == "try-error")
		BF10 <- NA
	
	return(BF10)
	
}
