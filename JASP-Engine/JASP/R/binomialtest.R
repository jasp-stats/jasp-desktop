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

			dataset <- .readDataSetToEnd(columns.as.numeric=NULL, columns.as.factor=variables, exclude.na.listwise=c(variables))

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
		list(name="Col", type="string"),
		list(name="N", type="integer"),
		list(name="Success", type="integer"),
		list(name="p", type="number", format="dp:3;p:.001")
		))

	table[["schema"]] <- schema

	data <- list()

	if (perform == "run") {

	for (var in variables) {

		d <- dataset[[.v(var)]]

		succ <- sum(d == 1)
		n <- length(d)
		print("D")
		print(d)
		print("N")
		print(n)

		if (options$hypothesis == "notEqualToTestValue") {
			hyp <- "two.sided"
		} else if (options$hypothesis == "greaterThanTestValue") {
			hyp <- "greater"
		} else {
			hyp <- "less"
		}

		r <- stats::binom.test(succ, n, p = options$testValue, alternative = hyp)
		data[[length(data) + 1]] <- list(Col=var,N=n,Success=succ,p=r$p.value)

	}

	}

	table[["data"]] <- data

	results[["binomial"]] <- table

	results
} 
