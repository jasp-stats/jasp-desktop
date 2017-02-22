#
# Copyright (C) 2017 University of Amsterdam
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
								callback = function(...) 0,  ...) {

	variables <- unlist(options$variables)

	if (is.null(dataset)) {

		if (perform == "run") {

			dataset <- .readDataSetToEnd(columns.as.numeric=variables, columns.as.factor=NULL, exclude.na.listwise=NULL)

		} else {

			dataset <- .readDataSetHeader(columns.as.numeric=variables, columns.as.factor=NULL)

		}

	} else {

		dataset <- .vdf(dataset, columns.as.numeric=NULL, columns.as.factor=variables)

	}

	## Retrieve State

	state <- .retrieveState()

	resultsAlpha <- NULL

	if ( ! is.null(state)) {  # is there state?

		diff <- .diff(options, state$options)  # compare old and new options

		if (is.list(diff) && diff[['variables']] == FALSE && diff[['reverseScaledItems']] == FALSE) {

			resultsAlpha <- state$resultsAlpha

		}

	}

	# Store results

	results <- list()

	results[["title"]] <- "Reliability Analysis"

	meta <- list(list(name="reliabilityScale", type="table"),
				 list(name="reliabilityItemsObj", type="object", meta=list(list(name="reliabilityItems", type="table"))))

	results[[".meta"]] <- meta

	errorList <- NULL

	if (is.null(resultsAlpha) && !is.null(variables) && length(variables) > 0) {

		# check for errors
		anyErrors <- .hasErrors(dataset = dataset, perform = perform,
								type = c("infinity", "variance"))

		doUpdate <- base::identical(anyErrors, FALSE)

		if (doUpdate) { # actually do reliability analysis

			resultsAlpha <- .reliabilityResults(dataset, options, variables, perform)

		} else { # show error message

			errorList <- list(errorType = "badData", errorMessage = anyErrors$message)

		}

	} else { # implies results are retrieved from state

		doUpdate <- TRUE

	}

	results[["reliabilityScale"]] <- .reliabalityScaleTable(resultsAlpha, dataset, options, variables, perform)
	results[["reliabilityScale"]][["error"]] <- errorList

	if (options$alphaItem || options$gutmannItem || options$itemRestCor || options$meanItem || options$sdItem || options[["mcDonaldItem"]]) {
		results[["reliabilityItemsObj"]] <- list(title="Item Statistics", reliabilityItems=.reliabalityItemsTable(resultsAlpha, options, variables, perform))
	} else {
		results[["reliabilityItemsObj"]] <- NULL
	}

	# Save state

	state[["options"]] <- options
	state[["resultsAlpha"]] <- resultsAlpha

	if (perform == "init") {

		return(list(results=results, status="inited", state=state))

	} else {

		return(list(results=results, status="complete", state=state))

	}
}

.reliabilityResults <- function (dataset, options, variables, perform) {

	r <- NULL

	if (perform == "run" && !is.null(variables) && length(variables) > 1) {

		d <- as.matrix(dataset[complete.cases(dataset), ])

		# generate key for reverse scaled items
		key <- NULL
		if (length(options$reverseScaledItems) > 0) {

			key <- .v(unlist(options$reverseScaledItems))

		}

		# calculate chronbach alpha and gutmanns lambda6
		r <- psych::alpha(d, key = key)

		# calculate McDonalds omega
		omega <- psych::omega(d, 1, flip = FALSE)[["omega.tot"]]

		# calculate McDonalds omega if item dropped
		omegaDropped <- NULL
		if (ncol(d) > 2) {
			omegaDropped = numeric(length = ncol(d))
			for (i in 1:ncol(d)) {
				omegaDropped[i] <- psych::omega(d[, -i], 1, flip = FALSE)$omega.tot
			}
		}

		r[["omega"]] <- omega
		r[["omegaDropped"]] <- omegaDropped

	}

	return(r)

}


.reliabalityScaleTable <- function (r, dataset, options, variables, perform) {

	table <- list()

	table[["title"]] <- "Scale Reliability Statistics"

	fields = list(list(name="case", title="", type="string"))

	if (options$meanScale)
		fields[[length(fields) + 1]] <- list(name="mu", title="mean", type="number", format="sf:4;dp:3")

	if (options$sdScale)
		fields[[length(fields) + 1]] <- list(name="sd", title="sd", type="number", format="sf:4;dp:3")

	if (options$alphaScale)
		fields[[length(fields) + 1]] <- list(name="alpha", title="Cronbach's \u03B1", type="number", format="sf:4;dp:3")

	if (options$gutmannScale)
		fields[[length(fields) + 1]] <- list(name="lambda", title="Gutmann's \u03BB6", type="number", format="sf:4;dp:3")

	if (options[["mcDonaldScale"]])
		fields[[length(fields) + 1]] <- list(name="omega", title="McDonalds' \u03C9", type="number", format="sf:4;dp:3")

	if (options[["averageInterItemCor"]])
		fields[[length(fields) + 1]] <- list(name="rho", title="Average interitem correlation", type="number", format="sf:4;dp:3")


	table[["schema"]] <- list(fields = fields)

	data <- list()

	if (!is.null(r)) {

		footnotes <- .newFootnotes()

		# is.null can be removed once options[["listwise"]] exists.
		if (!is.null(options[["listwise"]])) {
			exclwise = ifelse(options[["listwise"]], " listwise", " pairwise")
		} else {
			exclwise = ""
		}

		nObs = nrow(dataset)
		nExcluded = sum(!complete.cases(dataset))
		nValid = nObs - nExcluded

		# message <- paste("Scale consists of items ", paste0(variables, collapse = ", "))
		message <- sprintf("Of the observations, %d were used, %d were excluded%s, and %d were provided.",
						   nValid, nExcluded, exclwise, nObs)

		.addFootnote(footnotes, symbol = "<em>Note.</em>", text = message)

		table[["footnotes"]] <- as.list(footnotes)


		alpha <- NULL
		lambda <- NULL
		mu <- NULL
		sd <- NULL
		rho <- NULL
		omega <- NULL

		if (options$alphaScale)
			alpha <- .clean(r$total$raw_alpha)

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

		data[[1]] <- list(case="scale", alpha=alpha, lambda=lambda, omega = omega, rho=rho, mu=mu, sd=sd)

		table[["status"]] <- "complete"

	} else {

		data[[1]] <- list(case="scale", alpha=".", lambda=".", omega = ".", rho =".", mean=".", sd=".")

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

	if (options$alphaItem)
		fields[[length(fields) + 1]] <- list(name="alpha", title="Cronbach's \u03B1", type="number", format="sf:4;dp:3", overTitle = overTitle)

	if (options$gutmannItem)
		fields[[length(fields) + 1]] <- list(name="lambda", title="Gutmann's \u03BB6", type="number", format="sf:4;dp:3", overTitle = overTitle)

	if (options[["mcDonaldItem"]])
		fields[[length(fields) + 1]] <- list(name="omega", title="McDonalds' \u03C9", type="number", format="sf:4;dp:3", overTitle = overTitle)

	table[["schema"]] <- list(fields = fields)

	data <- list()

	footnotes <- .newFootnotes()

	if (length(options$reverseScaledItems) > 0) {
		message <- "reverse-scaled item"
		.addFootnote(footnotes, symbol = "\u207B", text=message)
	}

	rowNames <- gsub("-","", rownames(r$alpha.drop))

	# if all items are reverse scaled then for some reason psych::alpha()
	# pastes the first character of variable name to the end of the string
	if (length(options$reverseScaledItems) == length(rowNames)) {

		for (i in seq_along(rowNames)) { # removes the added characters
			rowNames[i] <- substr(rowNames[i], 1, nchar(rowNames[i]) - 1)
		}

		# regex version of the above loop -- perhaps useful in the future
		# rowNames = gsub('.{1}$', '', rowNames)

	}

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
				alpha <- .clean(r$alpha.drop[index,"raw_alpha"])

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
