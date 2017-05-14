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
	state = NULL,
	...
) {

	## Read Dataset ## ----
	variables <- unlist(options$variables)

	if (is.null(dataset)) {

		if (perform == "run") {

			dataset <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = NULL, exclude.na.listwise = NULL)

		} else {

			dataset <- .readDataSetHeader(columns.as.numeric = variables, columns.as.factor = NULL)

		}

	} else {

		dataset <- .vdf(dataset, columns.as.numeric = variables, columns.as.factor = NULL)

	}

	# ensure order of variables matches order of columns in dataset
	variables <- variables[match(.unv(colnames(dataset)), variables, nomatch = 0L)]

	## Initialize Results & statekey ## ----
	results <- list(
		title = "Network Analysis",
		.meta = list(
			list(name = "generalTB",   type = "table"),
			list(name = "fitMeasuresTB",  type = "table"),
			list(name = "centralityTB",   type = "table"),
			list(name = "networkPLT",     type = "image"),
			list(name = "centralityPLT",  type = "image")
		)
	)

	# basically everything but plotting arguments
	stateKey <- list(
		network = c(
			# data
			"variables", "groupingVariable",
			# what kind of network is estimated
			"estimator",
			# arguments for the network
			"correlationMethod", "tuningParameter", "criterion", "isingEstimator",
			"nFolds", "split", "rule", "sampleSize",
			# general arguments
			"weightedNetwork", "signedNetwork", "missingValues"),
		networkPLT = c("plotWidthNetwork", "plotHeightNetwork",
					   "layout", "edgeColors", "repulsion", "edgeSize", "nodeSize",
					   "maxEdgeStrength", "minEdgeStrength", "cut", "showDetails", "nodeColors"),
		centralityPLT = c("plotWidthCentrality", "plotHeightCentrality")

	)

	## Do Analysis ## ----

	if (is.null(state[["network"]])) { # old state is unusable

		state <- NULL # delete state

		# check for errors
		anyErrors <- .hasErrors(dataset = dataset, perform = perform,
								type = c("infinity", "variance", "observations"),
								observations.amount = " < 3",
								exitAnalysisIfErrors = TRUE)

		network <- .doNetworkAnalysis(dataset = dataset, options = options, variables = variables, perform = perform)

	} else { # state is useable, skip estimation

		network <- state[["network"]]

	}

	## Create Output ##  ----

	results[["generalTB"]] <- .NWgeneralTB(network, options, perform)

	if (options[["tableFitMeasures"]]) {
		results[["fitMeasuresTB"]] <- .fitMeasuresTB(network, options, perform)
	}
	if (options[["tableCentrality"]]) {
		results[["centralityTB"]] <- .centralityTB(network, options, perform)
	}
	if (options[["plotNetwork"]]) {
		results[["networkPLT"]] <- .makeNetworkPLT(network, options, perform, oldPlot = state[["networkPLT"]], plotType = "network")
	}
	if (options[["plotCentrality"]]) {
		results[["centralityPLT"]] <- .makeNetworkPLT(network, options, perform, oldPlot = state[["centralityPLT"]], plotType = "centrality")
	}


	## Exit Analysis ## ----

	# Save State
	state <- list(
		options = options,
		network = network,
		networkPLT = results[["networkPLT"]],
		centralityPLT = results[["centralityPLT"]]
	)

	attr(state, "key") <- stateKey

	# return to jasp
	if (perform == "init") {

		return(list(results = results, status = "inited", state = state))

	} else {

		return(list(results = results, status = "complete", state = state))

	}

}

# estimator ----
.doNetworkAnalysis <- function(dataset, options, variables, perform) {

	if (perform != "run")
		return(NULL)

	corMethod <- options[["correlationMethod"]]
	if (corMethod  == "auto")
		corMethod <- "cor_auto"

	data("bfi", package = "psych")
	dataset <- bfi[, 1:25]

	msg <- capture.output(
		network <- bootnet::estimateNetwork(
			data = dataset,
			default = options[["estimator"]],
			corMethod = corMethod
		)
		, type = "message"
	)

	network[["corMessage"]] <- msg

	return(network)

}

# wrappers for output ----
# ABBREVIATIONS:
# TB = table
# PTL = plot

.NWgeneralTB <- function(network, options, perform) {

	table <- list(
		title = "Summary of Network",
		schema = list(fields = list(
			list(name = "info", title = "", type = "string"),
			list(name = "value", title = "", type = "number", format="sf:4;dp:3")
		))
	)

	infos <- c("Number of nodes", "Number of non-zero edges", "Sparsity")

	if (perform != "run") { # fill in with .

		values <- rep(".", 3)
		table[["status"]] <- "inited"

	} else { # fill in with info from bootnet:::print.bootnet

		values <- c(
			nrow(network[["graph"]]),
			sum(network[["graph"]][upper.tri(network[["graph"]], diag = FALSE)] == 0),
			mean(network[["graph"]][upper.tri(network[["graph"]], diag = FALSE)] == 0)
		)
		table[["status"]] <- "complete"

	}

	data <- mapply(function(x, y) list(info = x, value = y),
				   x = infos, y = values, SIMPLIFY = FALSE, USE.NAMES = FALSE)
	table[["data"]] <- data

	return(table)

}

.fitMeasuresTB <- function(network, options, perform) {

	table <- list()
	table[["title"]] <- "Fit Measures"
	table[["schema"]] <- list(fields=list())
	table[["data"]] <- list()

	return(table)

}

.centralityTB <- function(network, options, perform) {

	table <- list()
	table[["title"]] <- "Centrality Measures"
	table[["schema"]] <- list(fields=list())
	table[["data"]] <- list()

	return(table)

}

.plotFunNetwork <- function() {

	# eval(quote()) construction because this function is evaluated inside .writeImage()
	# which needs to look 2 levels up to find the objects network and options.
	eval(quote(
		qgraph::qgraph(input = network[["graph"]], layout = options[["layout"]], repulsion = options[["repulsion"]])
	), envir = parent.frame(2))

}


.makeNetworkPLT <- function(network, options, perform, oldPlot = NULL, plotType) {

	if (!is.null(oldPlot) && !identical(oldPlot[["data"]], ""))
		return(oldPlot)

	if (plotType == "network") {

		plot <- list(
			title = "Network Plot",
			width = options[["plotWidthNetwork"]],
			height = options[["plotHeightNetwork"]],
			custom = list(width="plotWidthNetwork", height="plotHeightNetwork"),
			data = "", status = "inited"
		)

	} else if (plotType == "centrality") {

		plot <- list(
			title = "Centrality Plot",
			width = options[["plotWidthCentrality"]],
			height = options[["plotHeightCentrality"]],
			custom = list(width="plotWidthCentrality", height="plotHeightCentrality"),
			data = "", status = "inited"
		)

	}

	if (perform == "run") {

		plotObjOrFun <- switch(plotType,
			"network" = .plotFunNetwork,
			"centrality" = qgraph::centralityPlot(network[["graph"]], print = FALSE)
		)

		content <- .writeImage(width = plot[["width"]], height = plot[["height"]], plot = plotObjOrFun)

		plot[["convertible"]] <- TRUE
		plot[["data"]] <- content[["png"]]
		plot[["obj"]] <- content[["obj"]]
		plot[["status"]] <- "complete"

	}

	return(plot)

}
