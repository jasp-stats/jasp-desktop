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

# ABBREVIATIONS:
# TB = table
# PTL = plot

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
			
			if (options[["colorNodesBy"]] != "") # load data from variable that indicates groups
				options[["colorNodesByData"]] <- .readDataSetToEnd(columns = options[["colorNodesBy"]], exclude.na.listwise = options[["colorNodesBy"]])[[1]]

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

	stateKey <- list(
		# depends on everything but plotting arguments
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
		# depends only on plotting arguments
		networkPLT = c("plotWidthNetwork", "plotHeightNetwork",
					   "layout", "edgeColors", "repulsion", "edgeSize", "nodeSize", "colorNodesBy",
					   "maxEdgeStrength", "minEdgeStrength", "cut", "showDetails", "nodeColors", "showLegend"),
		# depends only on plotting arguments
		centralityPLT = c("plotWidthCentrality", "plotHeightCentrality")

	)

	## Do Analysis ## ----
	
	# Sort out whether things are set to defaults or not.
	if (is.null(state[["network"]]) && length(variables) >= 2) { # old state is unusable

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

	keep <- NULL
	results[["generalTB"]] <- .NWgeneralTB(network, options, perform)

	if (options[["tableFitMeasures"]]) {
		results[["fitMeasuresTB"]] <- .fitMeasuresTB(network, options, perform)
	}
	if (options[["tableCentrality"]]) {
		results[["centralityTB"]] <- .centralityTB(network, options, perform)
	}
	if (options[["plotNetwork"]]) {
		results[["networkPLT"]] <- .makeNetworkPLT(network, options, perform, oldPlot = state[["networkPLT"]], plotType = "network")
		keep <- c(keep, results[["networkPLT"]][["data"]])
	}
	if (options[["plotCentrality"]]) {
		results[["centralityPLT"]] <- .makeNetworkPLT(network, options, perform, oldPlot = state[["centralityPLT"]], plotType = "centrality")
		keep <- c(keep, results[["centralityPLT"]][["data"]])
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

		return(list(results = results, status = "inited", state = state, keep = keep))

	} else {

		return(list(results = results, status = "complete", state = state, keep = keep))

	}

}

# estimator ----
.doNetworkAnalysis <- function(dataset, options, variables, perform) {

	# early return if init or too little variables
	if (perform != "run" || length(variables) < 2)
		return(NULL)

	# fix input to match bootnet preferences.
	options[["rule"]] <- toupper(options[["rule"]])
	if (options[["correlationMethod"]]  == "auto")
		options[["correlationMethod"]] <- "cor_auto"

	options[["missingValues"]] <- switch(options[["missingValues"]],
		"excludePairwise" = "pairwise",
		"excludeListwise" = "listwise"
	)

	.dots <- list(
		corMethod   = options[["correlationMethod"]],
		tuning      = options[["tuningParameter"]],
		missing     = options[["missingValues"]],
		method      = options[["isingEstimator"]],
		rule        = options[["rule"]],
		nFolds      = options[["nFolds"]],
		weighted    = options[["weightedNetwork"]],
		signed      = options[["signedNetwork"]],
		split       = options[["split"]],
		criterion   = options[["criterion"]],
		sampleSize  = options[["sampleSize"]]
	)

	# get available arguments for specific network estimation function.
	# FOR FUTURE UPDATING: options[["estimator"]] MUST match name of function in bootnet literally.
	nms2keep <- names(formals(utils::getFromNamespace(paste0("bootnet_", options[["estimator"]]), ns = "bootnet")))
	.dots <- .dots[names(.dots) %in% nms2keep]

	# capture.output to get relevant messages (i.e. from qgraph::cor_auto "variables detected as...")
	msg <- capture.output(
		network <- bootnet::estimateNetwork(
			data = dataset,
			default = options[["estimator"]],
			.dots = .dots
		)
		, type = "message"
	)

	network[["corMessage"]] <- msg

	return(network)

}

# wrappers for output ----
.NWgeneralTB <- function(network, options, perform) {

	table <- list(
		title = "Summary of Network",
		schema = list(fields = list(
			list(name = "info", title = "", type = "string"),
			list(name = "value", title = "", type = "number", format="sf:4;dp:3")
		))
	)

	TBcolumns <- list(info = c("Number of nodes", "Number of non-zero edges", "Sparsity"))

	if (perform != "run" || is.null(network)) { # fill in with .

		TBcolumns[["value"]] <- rep(".", 3)
		table[["status"]] <- "inited"

	} else { # fill in with info from bootnet:::print.bootnet

		TBcolumns[["value"]] <- c(
			nrow(network[["graph"]]),
			sum(network[["graph"]][upper.tri(network[["graph"]], diag = FALSE)] == 0),
			mean(network[["graph"]][upper.tri(network[["graph"]], diag = FALSE)] == 0)
		)
		table[["status"]] <- "complete"

		# add footnotes for detected as?

	}

	data <- .TBcolumns2TBrows(TBcolumns)
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

	table <- list(
		title = "Centrality measures per variable",
		schema = list(fields = list(
			list(name = "Variable",    title = "Variable",    type = "string"),
			list(name = "Betweenness", title = "Betweenness", type = "number", format="sf:4;dp:3"),
			list(name = "Closeness",   title = "Closeness",   type = "number", format="sf:4;dp:3"),
			list(name = "Strength",    title = "Strength",    type = "number", format="sf:4;dp:3")
		))
	)

	if (perform != "run") {

		if (!is.null(options[["variables"]]) || !(length(options[["variables"]]) > 0)) {

			TBcolumns <- list(".", ".", ".", ".")

		} else {
			# dataframe since reshape2 below returns dataframes
			TBcolumns <- data.frame(
				.v(options[["variables"]]),
				rep(".", length(options[["variables"]])),
				rep(".", length(options[["variables"]])),
				rep(".", length(options[["variables"]]))
			)
		}

		table[["status"]] <- "inited"

	} else {

		TBcolumns <- qgraph::centralityTable(network[["graph"]])
		TBcolumns <- reshape2::dcast(TBcolumns, node ~ measure, value.var = "value")
		TBcolumns[["node"]] <- .unv(TBcolumns[["node"]])
		table[["status"]] <- "complete"

	}

	names(TBcolumns) <- c("Variable", "Betweenness", "Closeness", "Strength")
	data <- .TBcolumns2TBrows(TBcolumns)
	table[["data"]] <- data

	return(table)

}

# perhaps move to common?
.TBcolumns2TBrows <- function(infoByCol) do.call(mapply, c(FUN = base::list, infoByCol, SIMPLIFY = FALSE, USE.NAMES = FALSE))


.plotFunNetwork <- function() {

	# eval(quote()) construction because this function is evaluated inside .writeImage()
	# which needs to look 2 levels up to find the objects network and options.
	eval(quote({
		
		# ensure input makes sense or ignore these parameters
		minE <- options[["maxEdgeStrength"]]
		maxE <- options[["maxEdgeStrength"]]
		
		if (minE == 0 ||maxE <= minE) {
			minE <- NULL
			maxE <- NULL
		}
		
		
		if (!is.null(options[["colorNodesByData"]])) {
			
			u <- unique(options[["colorNodesByData"]])
			groups <- lapply(u, function(x, y) which(y == x), y = options[["colorNodesByData"]])
			names(groups) <- u
			print('options[["colorNodesByData"]]')
			print(options[["colorNodesByData"]])
			print("groups")
			print(groups)
			
		} else {
			groups <- NULL
		}
		
		# browser()
		qgraph::qgraph(
			input      = network[["graph"]],
			layout     = options[["layout"]],
			groups     = groups,
			repulsion  = options[["repulsion"]],
			cut        = options[["cut"]],
			edge.width = options[["edgeSize"]],
			node.width = options[["nodeSize"]],
			maximum    = maxE, # options[["maxEdgeStrength"]],
			minimum    = minE, # options[["minEdgeStrength"]],
			details    = options[["showDetails"]],
			labels     = .unv(network[["labels"]]),
			palette    = options[["nodeColors"]],
			theme      = options[["edgeColors"]],
			legend     = options[["showLegend"]]
		)}
	), envir = parent.frame(2))
	
}

# general function that makes the json framework around plot functions
.makeNetworkPLT <- function(network, options, perform, oldPlot = NULL, plotType) {

	if (!is.null(oldPlot) && !identical(oldPlot[["data"]], ""))
		return(oldPlot)

	if (plotType == "network") {

		plot <- list(
			title = "Network Plot",
			width = options[["plotWidthNetwork"]],
			height = options[["plotHeightNetwork"]],
			custom = list(width = "plotWidthNetwork", height = "plotHeightNetwork"),
			data = "", 
			status = "inited"
		)

	} else if (plotType == "centrality") {

		plot <- list(
			title = "Centrality Plot",
			width = options[["plotWidthCentrality"]],
			height = options[["plotHeightCentrality"]],
			custom = list(width = "plotWidthCentrality", height = "plotHeightCentrality"),
			data = "", 
			status = "inited"
		)

	}

	if (perform == "run") {

		plotObjOrFun <- switch(plotType,
			"network" = .plotFunNetwork,
			"centrality" = qgraph::centralityPlot(network[["graph"]], labels = .unv(network[["labels"]]), print = FALSE)
		)

		content <- .writeImage(width = plot[["width"]], height = plot[["height"]], plot = plotObjOrFun)

		plot[["convertible"]] <- TRUE
		plot[["data"]] <- content[["png"]]
		plot[["obj"]] <- content[["obj"]]
		plot[["status"]] <- "complete"

	}

	# should go into .writeImage
	# grDevices::graphics.off()

	return(plot)

}
