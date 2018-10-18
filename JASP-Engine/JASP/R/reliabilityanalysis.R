#
# Copyright (C) 2018 University of Amsterdam
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

ReliabilityAnalysis <- function(dataset = NULL, options, perform = "run",
								callback = function(...) 0, state = NULL, ...) {

	variables <- unlist(options$variables)

	if (is.null(dataset)) {

		if (perform == "run") {

			dataset <- .readDataSetToEnd(columns.as.numeric=variables, columns.as.factor=NULL, exclude.na.listwise=NULL)

		} else {

			dataset <- .readDataSetHeader(columns.as.numeric=variables, columns.as.factor=NULL)

		}

	} else {

		dataset <- .vdf(dataset, columns.as.numeric=variables, columns.as.factor=NULL)

	}

	## Retrieve State
	# future idea: add correlation matrix to state and make all scale statistics modular.
	defaults = c("variables", "reverseScaledItems", "missingValues")
	stateKey <- list(
		resultsAlpha = defaults,
		confAlpha = c(defaults, "confAlphaLevel")
	)
	resultsAlpha <- state[["resultsAlpha"]]
	resultsAlpha[["ciAlpha"]] <- state[["confAlpha"]]

	# Store results

	results <- list()

	results[["title"]] <- "Reliability Analysis"

	meta <- list(list(name="reliabilityScale", type="table"),
				 list(name="reliabilityItemsObj", type="object", meta=list(list(name="reliabilityItems", type="table"))))

	results[[".meta"]] <- meta

	errorList <- NULL
	
	# do we look up raw or standardized alpha?
	if (options[["alphaScaleStandardized"]][[1]] == "_2standardized") {
	    options$alphaNms <- "std.alpha"
	} else {
	    options$alphaNms <- "raw_alpha"
	}
	
	if (!is.null(variables) && length(variables) > 0) {
		if (is.null(resultsAlpha)) { # was the main analysis retrieved from state?
			
			# check for errors
			anyErrors <- .hasErrors(dataset = dataset, perform = perform,
									type = c("infinity", "variance", "observations"),
									observations.amount = " < 3",
									exitAnalysisIfErrors = TRUE)
			
			resultsAlpha <- .reliabilityResults(dataset, options, variables, perform)
			
		} else if (is.null(state[["confAlpha"]])) { # resultsAlpha retrieved from state, only recalculate CI

			resultsAlpha[["ciAlpha"]] <- .reliabilityAlphaCI(relyFit = resultsAlpha, ci = options[["confAlphaLevel"]])
			
		}
	}
	results[["reliabilityScale"]] <- .reliabalityScaleTable(resultsAlpha, dataset, options, variables, perform)

	if (options$alphaItem || options$gutmannItem || options$itemRestCor || options$meanItem || options$sdItem || options[["mcDonaldItem"]]) {
		results[["reliabilityItemsObj"]] <- list(title="Item Statistics", reliabilityItems=.reliabalityItemsTable(resultsAlpha, options, variables, perform))
	} else {
		results[["reliabilityItemsObj"]] <- NULL
	}

	# Save state

	state[["options"]] <- options
	state[["resultsAlpha"]] <- resultsAlpha
	state[["confAlpha"]] <- resultsAlpha[["ciAlpha"]]
	attr(state, "key") <- stateKey

	if (perform == "init") {

		return(list(results=results, status="inited", state=state))

	} else {

		return(list(results=results, status="complete", state=state))

	}
}

.reliabilityResults <- function (dataset, options, variables, perform) {

	relyFit <- NULL

	if (perform == "run" && !is.null(variables) && length(variables) > 1) {

		# obtain smoothed correlation and covariance matrix
		dataList <- .reliabilityConvertDataToCorrelation(dataset, options)
		nObs <- nrow(dataset)
		nVar <- ncol(dataset)
		
		# generate key for reverse scaled items
		key <- NULL
		if (length(options[["reverseScaledItems"]]) > 0) {

			key <- rep(1, length(variables))
			key[match(.v(unlist(options[["reverseScaledItems"]])), colnames(dataset))] <- -1

		}

		# calculate chronbach alpha, gutmanns lambda6, and average inter item corrrelation
		relyFit <- .quietDuringUnitTest(psych::alpha(dataList[["covariance"]], key = key))

		# because we supply a correlation matrix and not raw data, we have to add these ourselves
		relyFit[["total"]][["mean"]] <- mean(dataList[["itemMeans"]])
		relyFit[["total"]][["sd"]] <- stats::sd(dataList[["itemMeans"]])
		relyFit[["item.stats"]][["mean"]] <- dataList[["itemMeans"]]
		relyFit[["item.stats"]][["sd"]] <- dataList[["itemSds"]]
		relyFit[["nObs"]] <- nObs
		
		# calculate confidence interval for chronbach alpha
		relyFit[["ciAlpha"]] <- .reliabilityAlphaCI(relyFit = relyFit,	ci = options[["confAlphaLevel"]])

		# calculate the greatest lower bound -- only possible for more than 2 variables.
		if (nVar < 3) {

			relyFit[["glb"]] <- "."

		} else { # try since the glb is error prone icm reverse scaled items. Requires further investigation/ this might be a bug in psych.

			relyFit[["glb"]] <- .quietDuringUnitTest(try(psych::glb(r = dataList[["correlation"]], key = key)[["glb.max"]], silent = TRUE))

		}

		# calculate McDonalds omega
		omega <- .quietDuringUnitTest(psych::omega(m = dataList[["correlation"]], nfactors = 1, flip = FALSE, plot = FALSE, 
							  n.iter = 1, n.obs = nObs)[["omega.tot"]])

		# calculate McDonalds omega if item dropped
		omegaDropped <- NULL
		if (nVar > 2) {
			omegaDropped <- numeric(length = nVar)
			for (i in 1:nVar) {

					omegaDropped[i] <- .quietDuringUnitTest(psych::omega(m = dataList[["correlation"]][-i, -i], 
													nfactors = 1, n.iter = 1, n.obs = nObs,
													flip = FALSE, plot = FALSE)[["omega.tot"]])
			}
		}

		relyFit[["omega"]] <- omega
		relyFit[["omegaDropped"]] <- omegaDropped

	}

	return(relyFit)

}

.reliabalityScaleTable <- function (r, dataset, options, variables, perform) {

	table <- list()

	table[["title"]] <- "Scale Reliability Statistics"

	fields = list(list(name="case", title="", type="string"))

	if (options$meanScale)
		fields[[length(fields) + 1]] <- list(name="mu", title="mean", type="number", format="sf:4;dp:3")

	if (options$sdScale)
		fields[[length(fields) + 1]] <- list(name="sd", title="sd", type="number", format="sf:4;dp:3")

	if (options[["mcDonaldScale"]])
		fields[[length(fields) + 1]] <- list(name="omega", title="McDonald's \u03C9", type="number", format="sf:4;dp:3")

	if (options$alphaScale)
		fields[[length(fields) + 1]] <- list(name="alpha", title="Cronbach's \u03B1", type="number", format="sf:4;dp:3")

	if (options$gutmannScale)
		fields[[length(fields) + 1]] <- list(name="lambda", title="Gutmann's \u03BB6", type="number", format="sf:4;dp:3")

	if (options[["glbScale"]])
		fields[[length(fields) + 1]] <- list(name="glb", title="Greatest lower bound", type="number", format="sf:4;dp:3")

	if (options[["averageInterItemCor"]])
		fields[[length(fields) + 1]] <- list(name="rho", title="Average interitem correlation", type="number", format="sf:4;dp:3")

	if (options[["confAlpha"]]) {
		overTitle <- sprintf("%.1f%% Confidence Interval", 100*options[["confAlphaLevel"]])
		fields[[length(fields) + 1]] <- list(name="lower", title="Lower", type="number", format="sf:4;dp:3", overTitle = overTitle)
		fields[[length(fields) + 1]] <- list(name="upper", title="Upper", type="number", format="sf:4;dp:3", overTitle = overTitle)
	}

	table[["schema"]] <- list(fields = fields)

	data <- list()

	if (!is.null(r)) {

		footnotes <- .newFootnotes()

		if (options[["missingValues"]] == "excludeCasesListwise") {

			exclwise = " listwise"

		} else {

			exclwise = " pairwise"

		}

		nObs <- nrow(dataset)
		nExcluded <- sum(!complete.cases(dataset))
		nValid <- nObs - nExcluded

		# message <- paste("Scale consists of items ", paste0(variables, collapse = ", "))
		message <- sprintf("Of the observations, %d were used, %d were excluded%s, and %d were provided.",
						   nValid, nExcluded, exclwise, nObs)

		if (options[["glbScale"]]) {

			if (length(variables) <= 2) {

				message <- paste(message, "Warning: Greatest lower bound can only be calculated for three or more variables.")

			} else if (isTryError(r[["glb"]])) {

				message <- paste(message, "Warning: Greatest lower bound could not be calculated.")

			}

		}

		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = message)

		table[["footnotes"]] <- as.list(footnotes)


		alpha <- NULL
		lambda <- NULL
		mu <- NULL
		sd <- NULL
		rho <- NULL
		omega <- NULL
		glb <- NULL
		lower <- NULL
		upper <- NULL

		if (options[["alphaScale"]])
            alpha <- .clean(r$total[[options$alphaNms]])

		if (options$gutmannScale)
			lambda <- .clean(r$total[["G6(smc)"]])

		if (options$meanScale)
			mu <- .clean(r$total$mean)

		if (options$sdScale)
			sd <- .clean(r$total$sd)

		if (options[["averageInterItemCor"]])
			rho <- .clean(r[["total"]][["average_r"]])

		if (options[["mcDonaldScale"]])
			omega <- .clean(r[["omega"]])

		if (options[["glbScale"]]) {
			if (r[["glb"]] == "." || isTryError(r[["glb"]])) { # unusable information
				glb <- "."
			} else { # a useable value
				glb <- .clean(r[["glb"]])
			}
		}

		if (options[["confAlpha"]]) {
			lower = .clean(r[["ciAlpha"]][1])
			upper = .clean(r[["ciAlpha"]][2])
		}

		data[[1]] <- list(case="scale", alpha=alpha, lambda=lambda, omega = omega, glb = glb, rho=rho, mu=mu, sd=sd, lower = lower, upper = upper)

		table[["status"]] <- "complete"

	} else {

		data[[1]] <- list(case="scale", alpha=".", lambda=".", omega = ".", glb = ".", rho =".", mean=".", sd=".", lower = ".", upper = ".")

	}

	table[["data"]] <- data

	return(table)

}

.reliabalityItemsTable <- function (r, options, variables, perform) {

	table <- list()

	table[["title"]] <- "Item Reliability Statistics"

	overTitle <- paste0("If item dropped")

	fields = list(list(name="case", title="", type="string", combine=TRUE))

	if (options$meanItem)
		fields[[length(fields) + 1]] <- list(name="mu", title="mean", type="number", format="sf:4;dp:3")

	if (options$sdItem)
		fields[[length(fields) + 1]] <- list(name="sd", title="sd", type="number", format="sf:4;dp:3")

	if (options$itemRestCor)
		fields[[length(fields) + 1]] <- list(name="itemRestCor", title="item-rest correlation", type="number", format="sf:4;dp:3")

	if (options[["mcDonaldItem"]])
		fields[[length(fields) + 1]] <- list(name="omega", title="McDonald's \u03C9", type="number", format="sf:4;dp:3", overTitle = overTitle)

	if (options$alphaItem)
		fields[[length(fields) + 1]] <- list(name="alpha", title="Cronbach's \u03B1", type="number", format="sf:4;dp:3", overTitle = overTitle)

	if (options$gutmannItem)
		fields[[length(fields) + 1]] <- list(name="lambda", title="Gutmann's \u03BB6", type="number", format="sf:4;dp:3", overTitle = overTitle)

	table[["schema"]] <- list(fields = fields)

	data <- list()

	footnotes <- .newFootnotes()

	if (length(options$reverseScaledItems) > 0) {
		message <- "reverse-scaled item"
		.addFootnote(footnotes, symbol = "\u207B", text=message)
	}

	# can only be computed if there are at least 3 variables.
	if (options[["mcDonaldItem"]] && length(variables) < 3) {

		message <- "Warning: McDonald's \u03C9 if item dropped can only be calculated for three or more variables."
		.addFootnote(footnotes, text = message)

	}

	# psych::alpha uses a minus to signify reverse scaled item. 
	rowNames <- gsub("-","", rownames(r$alpha.drop))

	if (!is.null(r)) {

		for (var in variables) {

			varV <- .v(var)
			index <- which(varV == rowNames)

			alpha <- NULL
			lambda <- NULL
			itemRestCor <- NULL
			mu <- NULL
			sd <- NULL
			omega <- NULL

			if (var %in% options$reverseScaledItems) {
				case <- paste0(var,"\u207B")
			} else {
				case <- var
			}

			if (options$alphaItem)
				alpha <- .clean(r$alpha.drop[index, options$alphaNms])

			if (options$gutmannItem)
				lambda <- .clean(r$alpha.drop[index, "G6(smc)"])

			if (options$itemRestCor)
				itemRestCor <- .clean(r$item.stats[index,"r.drop"])

			if (options$meanItem)
				mu <- .clean(r$item.stats[index,"mean"])

			if (options$sdItem)
				sd <- .clean(r$item.stats[index,"sd"])

			if (options[["mcDonaldItem"]])
				omega <- .clean(r[["omegaDropped"]][index])

			data[[length(data) + 1]] <- list(case=case, alpha=alpha, lambda=lambda, omega = omega, itemRestCor=itemRestCor, mu=mu, sd=sd)
		}

		table[["status"]] <- "complete"

	} else {

		variablesTemp <- variables

		if (is.null(variables))
			variablesTemp <- "..."

		for (var in variablesTemp) {

			data[[length(data) + 1]] <- list(case=var, alpha=".", lambda=".", omega = ".", itemRestCor=".", mu=".", sd=".")

		}
	}

	table[["data"]] <- data

	table[["footnotes"]] <- as.list(footnotes)

	return(table)

}

.reliabilityAlphaCI <- function(relyFit, ci, nullAlpha = 0) {

	# code taken and modified from http://www.psyctc.org/stats/R/Feldt1.html
	# considering using the bootstrapped version inside psych as an alternative

	#***********************************************************#
	#* program using methods described in Feldt, Woodruff &    *#
	#* Salih (1987) Applied Psychological Measurement 11(1),   *#
	#* pp. 93-103 to carry out omnibus inferential test of     *#
	#* similarity of alpha values from a single sample         *#
	#***********************************************************#
	
	# relyFit is the output from psych::alpha and must contain the sample size as nObs
	# ci is the width of the confidence interval about obs.a desired
	
	estAlpha = relyFit[["total"]][["raw_alpha"]]
	nVar = relyFit[["nvar"]]
	nObs = relyFit[["nObs"]]

	if(estAlpha > nullAlpha) {
		f <- (1 - estAlpha) / (1 - nullAlpha)
	} else {
		f <- (1 - nullAlpha) / (1 - estAlpha)
	}
	nDen <- (nObs - 1) * (nVar - 1)
	nNum <- nObs - 1
	null.p <- stats::pf(f, nNum, nDen) # set the upper and lower p values for the desired C.I.
	p1 <- (1 - ci)/2
	p2 <- ci + p1 # corresponding F values
	f1 <- stats::qf(p1, nNum, nDen)
	f2 <- stats::qf(p2, nNum, nDen) # confidence interval
	lwr <- 1 - (1 - estAlpha) * f2
	upr <- 1 - (1 - estAlpha) * f1
	return(c(lwr, upr))
}

.reliabilityConvertDataToCorrelation <- function(dataset, options) {
	
	if (options[["missingValues"]] == "excludeCasesListwise") {
		
		dataset <- dataset[complete.cases(dataset), ]
		
	}
	
	means = colMeans(dataset, na.rm = TRUE)
	covmat <- stats::cov(dataset, use = "pairwise")
	stdev <- sqrt(diag(covmat))
	cormat <- psych::cor.smooth(stats::cov2cor(covmat), eig.tol = sqrt(.Machine[["double.eps"]]))
	
	return(list(
		correlation = cormat,
		itemSds = stdev,
		itemMeans = means,
		# direct line from: corpcor::rebuild.cov
		covariance = sweep(sweep(cormat, 1, stdev, "*"), 2, stdev, "*")
	))
	
}
