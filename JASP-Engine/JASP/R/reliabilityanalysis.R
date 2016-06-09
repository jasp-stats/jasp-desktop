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

	results <- list()

	results[["title"]] <- "Reliability Analysis"

	meta <- list(list(name="reliabilityScale", type="table"), 
			 	 list(name="reliabilityItems", type="table"))
			 
	results[[".meta"]] <- meta
	
	r <- .reliabilityResults(dataset, options, variables, perform)
	
	results[["reliabilityScale"]] <- .reliabalityScaleTable(r, options, variables, perform)
	
	if (options$alphaItem || options$gutmannItem || options$itemTotalCor) {
		results[["reliabilityItems"]] <- .reliabalityItemsTable(r, options, variables, perform)
	} else {
		results[["reliabilityItems"]] <- NULL
	}
	
	results
}

.reliabilityResults <- function (dataset, options, variables, perform) {
	
	if (perform == "run" && !is.null(variables) && length(variables) > 1) {
	
		d <- as.matrix(dataset)
		r <- psych::alpha(d)
		
	} else {
	
		r <- NULL
		
	}
	
	return(r)
}

.reliabalityScaleTable <- function (r, options, variables, perform) {

	table <- list()

	table[["title"]] <- "Scale Statistics"

	fields = list(list(name="case", title="", type="string", combine=TRUE))
	
	if (options$alphaTotal)
		fields[[length(fields) + 1]] <- list(name="alpha", title="Cronbach's \u03B1", type="number", format="sf:4;dp:3")
		
	if (options$gutmannTotal)
		fields[[length(fields) + 1]] <- list(name="lambda", title="Gutmann's \u03BB6", type="number", format="sf:4;dp:3")
	
#	if (options$meanTotal)
#		fields[[length(fields) + 1]] <- list(name="mean", title="mean", type="number", format="sf:4;dp:3")
	
	table[["schema"]] <- list(fields = fields)

	data <- list()

	footnotes <- .newFootnotes()
	
	message <- paste("items used in scale: ", paste0(variables, collapse = ", "))
	.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
		
	if (perform == "run" && !is.null(r) && !is.null(variables) && length(variables) > 1) {
		
		alpha <- NULL
		lambda <- NULL
#		mu <- NULL
		
		if (options$alphaTotal)
			alpha <- .clean(r$total$raw_alpha)
			
		if (options$gutmannTotal)
			lambda <- .clean(r$total[["G6(smc)"]])
			
#		if (options$meanTotal)
#			mu <- .clean(r$total$mean)
						
		data[[1]] <- list(case="scale", alpha=alpha, lambda=lambda)
		
	} else {
		
		data[[1]] <- list(case="scale", alpha=".", lambda=".")
		
	}
	
	table[["data"]] <- data

	table[["footnotes"]] <- as.list(footnotes)
	
	return(table)

}

.reliabalityItemsTable <- function (r, options, variables, perform) {

	table <- list()

	table[["title"]] <- "Item Statistics"
	
	overTitle <- paste0("If item dropped")
	
	fields = list(list(name="case", title="", type="string", combine=TRUE))
	
	if (options$alphaItem)
		fields[[length(fields) + 1]] <- list(name="alpha", title="Cronbach's \u03B1", type="number", format="sf:4;dp:3", overTitle = overTitle)
		
	if (options$gutmannItem)
		fields[[length(fields) + 1]] <- list(name="lambda", title="Gutmann's \u03BB6", type="number", format="sf:4;dp:3", overTitle = overTitle)
	
	if (options$itemTotalCor)
		fields[[length(fields) + 1]] <- list(name="itemTotalCor", title="Item-Total Correlation", type="number", format="sf:4;dp:3", overTitle = overTitle)
	
	table[["schema"]] <- list(fields = fields)

	data <- list()

	footnotes <- .newFootnotes()
		
	if (perform == "run" && !is.null(r) && !is.null(variables) && length(variables) > 1) {
				
		for (var in variables) {
		
			alpha <- NULL
			lambda <- NULL
			itemTotalCor <- NULL
		
			if (options$alphaItem)
				alpha <- .clean(r$alpha.drop[.v(var),"raw_alpha"])
		
			if (options$gutmannItem)
				lambda <- .clean(r$alpha.drop[.v(var), "G6(smc)"])
	
			if (options$itemTotalCor)
				itemTotalCor <- .clean(r$item.stats[.v(var),"r.drop"])
				
			data[[length(data) + 1]] <- list(case=var, alpha=alpha, lambda=lambda, itemTotalCor=itemTotalCor)
		}

		
	} else {
		
		variablesTemp <- variables
	
		if (is.null(variables))
			variablesTemp <- ""
	
		for (var in variablesTemp) {
			
			data[[length(data) + 1]] <- list(case=var, alpha=".", lambda=".", itemTotalCor=".")
			
		}
	}
	
	table[["data"]] <- data

	table[["footnotes"]] <- as.list(footnotes)
	
	return(table)

}
