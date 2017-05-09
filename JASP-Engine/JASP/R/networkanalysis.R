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

# main ----
NetworkAnalysis <- function (
	dataset = NULL,
	options,
	perform = "run",
	callback = function(...) list(status = "ok"),
	...
) {

	## Read Dataset ## ----
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

	# ensure order of variables matches order of columns in dataset
	variables <- variables[match(.unv(colnames(dataset)), variables, nomatch = 0L)]


	## Retrieve State ## (deprecated? state key?) ----

	# state <- .retrieveState()
	#
	toFromState <- NULL
	#
	# if ( ! is.null(state)) {  # is there state?
	#
	# 	diff <- .diff(options, state$options)  # compare old and new options
	#
	# 	# if nothing important was changed retrieve state
	# 	if (is.list(diff) && diff[['variables']] == FALSE) {
	#
	# 		toFromState <- state$results
	#
	# 	}
	#
	# }


	## Initialize Results ## ----
	results <- list(
		title = "Network Analysis",
		.meta = list(
			list(name = "fitMeasuresTB",  type = "table"),
			list(name = "centralityTB",   type = "table"),
			list(name = "centralityPLT",  type = "image"),
			list(name = "networkPLT",     type = "image")
		)
	)

	## Do Analysis ## ----

	if (is.null(toFromState)) { # implies old state was unusable

		# check for errors
		anyErrors <- .hasErrors(dataset = dataset, perform = perform,
								type = c("infinity", "variance", "observations"),
								observations.amount = " < 3",
								exitAnalysisIfErrors = TRUE)

		networkResults <- .doNetworkAnalysis(dataset, options, variables, perform)

	}


	## Create Output ##  ----

	if (options[["plotFitMeasures"]]) {
		results[["fitMeasures"]] <- .fitMeasuresTB(networkResults, options, perform)
	}
	}
	}
	}

	## Save State ##

	state <- list(
		options = options,
		networkResults = toFromState
	)

	## Exit Analysis ## ----

	if (perform == "init") {

		return(list(results=results, status="inited", state=state))

	} else {

		return(list(results=results, status="complete", state=state))

	}

}

# estimator ----
.doNetworkAnalysis <- function(...) NULL

# wrappers for output ----
# ABBREVIATIONS:
# TB = table
# PTL = plot

.fitMeasuresTB <- function(network, options, perform) {

	table <- list()
	table[["title"]] <- "Network Analysis>"
	table[["schema"]] <- list(fields=list())
	table[["data"]] <- list()

	return(table)

}

.centralityTB <- function(network, options, perform) {

	table <- list()
	table[["title"]] <- "Network Analysis>"
	table[["schema"]] <- list(fields=list())
	table[["data"]] <- list()

	return(table)

}

.centralityPLT <- function(network, options, perform) {

	plot <- list(
		width = 530,# options[["plotWidth"]],
		height = 400, #options[["plotHeight"]],
		# custom = list(width = "plotWidth", height = "plotHeight"),
	)

	if (perform == "run") {
		.plotFunc <- function() {
			plot(rnorm(100), bty = "n", las = 1)
		}


		content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

		plot <- c(plot, list(
			convertible = TRUE,
			obj = content[["obj"]],
			data = content[["png"]],
			status = "complete"
			)
		)


	} else {

	#	plot[["data"]] <- ""

	}

	return(plot)

}

.networkPLT <- function(network, options, perform) {

	plot <- list(
		width = 530,# options[["plotWidth"]],
		height = 400, #options[["plotHeight"]],
		# custom = list(width = "plotWidth", height = "plotHeight"),
	)

	if (perform == "run") {
		.plotFunc <- function() {
			plot(rnorm(100), bty = "n", las = 1)
		}


		content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

		plot <- c(plot, list(
			convertible = TRUE,
			obj = content[["obj"]],
			data = content[["png"]],
			status = "complete"
		)
		)


	} else {

		#	plot[["data"]] <- ""

	}

	return(plot)


}

