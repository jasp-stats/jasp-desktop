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
		
	if (is.null(resultsAlpha)) {
	
		resultsAlpha <- .reliabilityResults(dataset, options, variables, perform)
	
	}
	
	results[["reliabilityScale"]] <- .reliabalityScaleTable(resultsAlpha, options, variables, perform)
	
	if (options$alphaItem || options$gutmannItem || options$itemRestCor || options$meanItem || options$sdItem) {
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
	
	if (perform == "run" && !is.null(variables) && length(variables) > 1) {
		
		d <- as.matrix(dataset)
		
		if (all(options$variables %in% options$reverseScaledItems)) {
			r <- psych::alpha(d)
		} else {
			r <- psych::alpha(d, key = .v(unlist(options$reverseScaledItems)))
		}
		
	} else {
	
		r <- NULL
		
	}
	
	return(r)
}

.reliabalityScaleTable <- function (r, options, variables, perform) {

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
	
	table[["schema"]] <- list(fields = fields)

	data <- list()

	footnotes <- .newFootnotes()
	
	message <- paste("Scale consists of items ", paste0(variables, collapse = ", "))
	.addFootnote(footnotes, symbol = "<em>Note.</em>", text=message)
		
	if (!is.null(r)) {
		
		alpha <- NULL
		lambda <- NULL
		mu <- NULL
		sd <- NULL
		
		if (options$alphaScale)
			alpha <- .clean(r$total$raw_alpha)
			
		if (options$gutmannScale)
			lambda <- .clean(r$total[["G6(smc)"]])
			
		if (options$meanScale)
			mu <- .clean(r$total$mean)
		
		if (options$sdScale)
			sd <- .clean(r$total$sd)
						
		data[[1]] <- list(case="scale", alpha=alpha, lambda=lambda, mu=mu, sd=sd)
		
		table[["status"]] <- "complete"
		
	} else {
		
		data[[1]] <- list(case="scale", alpha=".", lambda=".", mean=".", sd=".")
		
	}
	
	table[["data"]] <- data

	table[["footnotes"]] <- as.list(footnotes)
	
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
	
	table[["schema"]] <- list(fields = fields)

	data <- list()

	footnotes <- .newFootnotes()
	
	if (length(options$reverseScaledItems) > 0) {
		message <- "reverse-scaled item"
		.addFootnote(footnotes, symbol = "\u207B", text=message)
	}
		
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
				
			data[[length(data) + 1]] <- list(case=case, alpha=alpha, lambda=lambda, itemRestCor=itemRestCor, mu=mu, sd=sd)
		}
		
		table[["status"]] <- "complete"

	} else {
		
		variablesTemp <- variables
	
		if (is.null(variables))
			variablesTemp <- "..."
	
		for (var in variablesTemp) {
			
			data[[length(data) + 1]] <- list(case=var, alpha=".", lambda=".", itemRestCor=".", mu=".", sd=".")
			
		}
	}
	
	table[["data"]] <- data

	table[["footnotes"]] <- as.list(footnotes)
	
	return(table)

}
