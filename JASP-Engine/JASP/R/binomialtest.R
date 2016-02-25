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

BinomialTest <- function(dataset = NULL, options, perform = "run",
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

	results[["title"]] <- "Binomial Test"

	meta <- list(list(name="binomial", type="table"))
	results[[".meta"]] <- meta

	table <- list()

	table[["title"]] <- "Binomial Test"

	schema <- list(fields=list(
		list(name="case", title="", type="string", combine=TRUE),
		list(name="level", type="string"),
		list(name="counts", type="integer"),
		list(name="total", type="integer"),
		list(name="proportion", type="number", format="sf:4;dp:3"),
		list(name="p", type="number", format="dp:3;p:.001")
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

				r <- stats::binom.test(counts, n, p = options$testValue, alternative = hyp)
				
				p <- r$p.value
				
				if (p == FALSE) {
					p <- 0
				} else if (p == TRUE) {
					p <- 1
				}
								
				row <- list(case=var, level=lev, counts=.clean(counts), total=.clean(n), proportion=.clean(prop), p=.clean(p))
				
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
	
	table[["footnotes"]] <- list(list(symbol="<i>Note.</i>", text=paste0("Proportions tested against value: ", options$testValue, ".")))

	results[["binomial"]] <- table

	results
} 
