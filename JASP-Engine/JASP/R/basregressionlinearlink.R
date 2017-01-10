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

BASRegressionLinearLink <- function (dataset = NULL, options, perform = "run",
									callback = function(...) list(status = "ok"), ...) {
	print("________________________________________")
	print("BAS REgression")
	print("________________________________________")
	# dependent <- unlist(options$dependent)
	# covariates <- unlist(options$covariates)
	#
	# if (covariates == "") {
	# 	covariates <- NULL
	# }
	#
	# if (is.null(dataset)) {
	# 	if (perform == "run") {
	# 		if (options$missingValues == "excludeListwise") {
	# 			dataset <- .readDataSetToEnd(columns.as.numeric = dependent, columns.as.factor = covariates,
	# 										exclude.na.listwise = c(dependent, covariates))
	# 		} else {
	# 			dataset <- .readDataSetToEnd(columns.as.numeric = dependent, columns.as.factor = covariates,
	# 										exclude.na.listwise = covariates)
	# 		}
	# 	} else {
	# 		dataset <- .readDataSetHeader(columns.as.numeric = dependent, columns.as.factor = covariates)
	# 	}
	# }
	run <- (perform == "run")
	state <- .retrieveState ()
	if (!is.null (state)) {
		change <- .diff(options, state$options)
		if (!base::identical(change, FALSE) && (change$dependent || change$modelTerms ||
			change$priorCovariates)) {
			state <- NULL
		} else {
			perform <- "run"
		}
	}
	# data
	if (is.null(state)) {
		dataset <- .readBayesianLinearModelData(dataset, options, perform)
	}

	if (length(options$covariates) > 0 && options$dependent != "" && perform == "run") {
		a <- .v(options$covariates)
		b <- .v(options$dependent)
		p <- as.formula(paste(b, "~", paste(a, collapse="+")))

		bas_lm <- BAS::bas.lm(formula = p, data = dataset, prior='BIC', modelprior = BAS::beta.binomial(1,1))
	}

	# Populate the output table
	meta <- list()
	meta[[1]] <- list(name = "table", type = "table")
	meta[[2]] <- list(name = "inferentialPlots", type = "object",
						meta = list(list(name = "PriorPosteriorPlot", type = "image"),
									list(name = "BFrobustnessPlot", type = "image"))
					)

	if (options$bayesFactorType == "BF10") {
		bfm.title <- "BF<sub>M</sub>"
		bf.title <- "BF<sub>10</sub>"
	} else if (options$bayesFactorType == "BF01") {
		bfm.title <- "BF<sub>M</sub>"
		bf.title <- "BF<sub>01</sub>"
	} else if (options$bayesFactorType == "LogBF10") {
		bfm.title <- "Log(BF<sub>M</sub>)"
		bf.title <- "Log(BF<sub>10</sub>)"
	}

	fields <- list(
				list(name = "Models", type = "string"),
				list(name = "P(M)", type = "number", format = "sf:4;dp:3"),
				list(name = "P(M|data)", type = "number", format = "sf:4;dp:3;log10"),
				list(name = "BFM", type = "number", format = "sf:4;dp:3;log10",
						title = paste (bfm.title, sep = "")),
				list(name = "BF10", type = "number", format = "sf:4;dp:3;log10",
						title = paste (bf.title, sep = "")),
				list(name = "error %", type="number", format="sf:4;dp:3")
			)
	if (options$bayesFactorType == "LogBF10") {
		fields[[4]] <- list(name = "BFM", type = "number", format = "sf:4;dp:3", title = paste (bfm.title, sep = ""))
		fields[[5]] <- list(name = "BF10", type = "number", format = "sf:4;dp:3", title = paste (bf.title, sep = ""))
	}

	# # add footnotes to the analysis result
	# footnotes <- .newFootnotes()
	# if (options$hypothesis != "notEqualToTestValue") {
	# 	.addFootnote(footnotes, symbol = "<em>Note.</em>", text = hypothesis.variables$message)
	# }

	table <- list()
	table[["title"]] <- "Model comparison"
	table[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
	table[["schema"]] <- list(fields = fields)
	table[["data"]] <- list()

	results <- list()
	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Adaptive Sampling"
	results[["table"]] <- table

	keep <- NULL

	if (run) {
		status <- "complete"
		state <- list(options = options)
	} else {
		status <- "inited"
	}

	return (list(results = results,
				status = status,
				state = state,
				keep = keep)
			)
}
