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
			"normalizeCentrality", "weightedNetwork", "signedNetwork", "missingValues"),
		# depends only on plotting arguments
		networkPLT = c("plotWidthNetwork", "plotHeightNetwork",
					   "layout", "edgeColors", "repulsion", "edgeSize", "nodeSize", "colorNodesBy",
					   "maxEdgeStrength", "minEdgeStrength", "cut", "showDetails", "nodeColors", "showLegend",
					   "normalizeCentrality", "weightedNetwork", "signedNetwork", "missingValues"),
		# depends only on plotting arguments
		centralityPLT = c("plotWidthCentrality", "plotHeightCentrality",
						  "normalizeCentrality", "weightedNetwork", "signedNetwork", "missingValues")

	)

	## Do Analysis ## ----

	# Sort out whether things are set to defaults or not.
	if (is.null(state[["network"]]) && length(variables) > 2) { # old state is unusable

		state <- NULL # delete state

		# check for errors
		checks <- c("infinity", "variance", "observations")
		anyErrors <- .hasErrors(dataset = dataset, perform = perform,
								type = checks,
								observations.amount = " < 3",
								exitAnalysisIfErrors = TRUE)

		network <- .doNetworkAnalysis(dataset = dataset, options = options, variables = variables, perform = perform)

	} else { # state is useable, skip estimation

		network <- state[["network"]]

	}

	## Create Output ##  ----

	keep <- NULL
	results[["generalTB"]] <- .NWgeneralTB(network, dataset, options, perform)

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
.doNetworkAnalysis <- function(dataset, options, variables, perform, oldNetwork = NULL) {

	# early return if init or too little variables
	if (perform != "run" || length(variables) < 2)
		return(NULL)

	network <- oldNetwork
	if (is.null(network[["graph"]])) {

		# fix input to match bootnet preferences
		options[["rule"]] <- toupper(options[["rule"]])
		if (options[["correlationMethod"]] == "auto")
			options[["correlationMethod"]] <- "cor_auto"

		options[["isingEstimator"]] <- switch(options[["isingEstimator"]],
			"pseudoLikelihood" = "pl",
			"univariateRegressions" = "uni",
			"bivariateRegressions" = "bi",
			"logLinear" = "ll"
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

		# get available arguments for specific network estimation function. Remove unused ones.
		# FOR FUTURE UPDATING: options[["estimator"]] MUST match name of function in bootnet literally (see ?bootnet::bootnet_EBICglasso).
		funArgs <- formals(utils::getFromNamespace(paste0("bootnet_", options[["estimator"]]), ns = "bootnet"))
		nms2keep <- names(funArgs)
		.dots <- .dots[names(.dots) %in% nms2keep]

		# for safety, when estimator is changed but missing was pairwise (the default).
		if (!isTRUE("pairwise" %in% eval(funArgs[["missing"]])))
			.dots[["missing"]] <- "listwise"

		# some manual adjustments for these estimators
		if (options[["estimator"]] == "huge") {

			if (.dots[["criterion"]] == "cv")
				.dots[["criterion"]] == "ebic"

		} else if (options[["estimator"]] == "mgm") {

			.dots[["criterion"]] <- toupper(.dots[["criterion"]]) # this function wants capitalized arguments

			if (!(.dots[["criterion"]] %in% eval(funArgs$criterion)))
				.dots[["criterion"]] <- "EBIC"

		} else if (options[["estimator"]] == "adalasso") {

			if (is.na(.dots[["nFolds"]]) || .dots[["nFolds"]] < 2) # estimator crashes otherwise
				.dots[["nFolds"]] <- 2

		}

		# Fake png hack -- qgraph::qgraph() has an unprotected call to `par()`. `par()` always opens a new device if there is none.
		# Perhaps ask Sacha to fix this in qgraph. Line 1119: if (DoNotPlot) par(pty = pty)
		tempFileName <- getTempFileName()
		grDevices::png(filename = tempFileName)

		# capture.output to get relevant messages (i.e. from qgraph::cor_auto "variables detected as...")
		msg <- capture.output(
			network <- bootnet::estimateNetwork(
				data = dataset,
				default = options[["estimator"]],
				.dots = .dots
			)
			, type = "message"
		)

		dev.off() # close the fake png
		unlink(tempFileName) # remove the fake png file

		network[["corMessage"]] <- msg

	}

	if (is.null(network[["centrality"]])) {
		network[["centrality"]] <- qgraph::centrality(network[["graph"]], weighted = options[["weightedNetwork"]], signed = options[["signedNetwork"]], all.shortest.paths = FALSE)

		# note: centrality table is (partially) calculated here so that centralityTable and centralityPlot don't compute the same twice.
		TBcent <- as.data.frame(network[["centrality"]][c("Betweenness", "Closeness", "InDegree")])
		centVars <- apply(TBcent, 2, var)
		if (options[["normalizeCentrality"]] == "normalized") { # normalize only if variances are nonzero
			
			for (i in 1:ncol(TBcent)) { # code modified from base::scale.default
				
				valid <- !is.na(TBcent)
				
				if (sum(valid) < length(valid)) {
					
					obs <- TBcent[valid, i]
					obs <- obs - mean(obs)
					stdev <- sqrt(sum(obs^2) / max(1, length(obs) - 1))
					if (stdev == 0) { # avoid division by zero
						TBcent[valid, i] <- obs
					} else {
						TBcent[valid, i] <- obs / stdev
					}
				}
				
			}

		} else if (options[["normalizeCentrality"]] == "relative") {
			
			for (i in 1:ncol(TBcent)) { # code modified from qgraph::centralityTable
				
				mx <- max(abs(TBcent[, i]), na.rm = TRUE)
				
				if (mx != 0) # avoid division by zero
					TBcent[, i] <- TBcent[, i] / mx

			}
			
		} else { # todo make raw option.
			
		}
		
		TBcent[["node"]] <- .unv(network[["labels"]])
		TBcent <- TBcent[c(4, 1:3)] # put columns in intended order (of schema).
		network[["centralityTable"]] <- TBcent
	}

	return(network)

}

# wrappers for output ----
.NWgeneralTB <- function(network, dataset, options, perform) {

	table <- list(
		title = "Summary of Network",
		schema = list(fields = list(
			list(name = "info", title = "", type = "string"),
			list(name = "value", title = "", type = "number", format="sf:4;dp:3")
		))
	)

	footnotes <- .newFootnotes()
	msg <- NULL
	
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
		if (options[["estimator"]] %in% c("IsingFit", "IsingSampler") &&
			!all(unlist(dataset[!is.na(dataset)]) %in% 0:1))  {

			msg <- c(msg,
				sprintf("Data was binarized using %s.",	options$split)
			)

		}

	}

	data <- .TBcolumns2TBrows(TBcolumns)
	table[["data"]] <- data
	print("msg")
	print(msg)
	if (!is.null(msg)) {

		msg <- paste(msg, collapse = "\n")
		.addFootnote(footnotes = footnotes, text = msg, symbol = "<em>Note: </em>")
		table[["footnotes"]] <- as.list(footnotes)

	}

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

	if (perform != "run") { # make empty table

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

	} else { # fill with results

		TBcolumns <- network[["centralityTable"]]

	}

	names(TBcolumns) <- c("Variable", "Betweenness", "Closeness", "Strength")
	data <- .TBcolumns2TBrows(TBcolumns)
	table[["data"]] <- data

	return(table)

}

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

.centralityPlot2 <- function(wide) {

	Long <- reshape2::melt(wide)
	colnames(Long)[2] <- "measure"
	Long[["type"]] <- NA

	# code adapted from qgraph::centralityPlot()

	Long <- Long[gtools::mixedorder(Long$node), ]
	Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))
	if (length(unique(Long$type)) > 1) {
		g <- ggplot2::ggplot(Long, ggplot2::aes(x = value, y = node, group = type,
							  colour = type))
	} else {
		g <- ggplot2::ggplot(Long, ggplot2::aes(x = value, y = node, group = type))
	}
	g <- g + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point()
	if (length(unique(Long$graph)) > 1) {
		g <- g + ggplot2::facet_grid(graph ~ measure, scales = "free")
	} else {
		g <- g + ggplot2::facet_grid(~measure, scales = "free")
	}

	return(g + ggplot2::theme_bw())

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
			"centrality" = .centralityPlot2(network[["centralityTable"]])#qgraph::centralityPlot(network[["graph"]], labels = .unv(network[["labels"]]), print = FALSE,
											#	  weighted = options[["weightedNetwork"]], signed = options[["signedNetwork"]])
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

# helper functions ----
# helper function to avoid qgraph from opening windows
getTempFileName <- function() {

	if (Sys.getenv("RSTUDIO") == "1") {

		fullPathpng <- tempfile(fileext = ".png")

	} else { # code from writeImage()

		location <- .requestTempFileNameNative("png")
		relativePathpng <- location$relativePath
		fullPathpng <- paste(location$root, relativePathpng, sep="/")
		base::Encoding(fullPathpng) <- "UTF-8"

	}
	return(fullPathpng)
}

# perhaps move to common?
.TBcolumns2TBrows <- function(infoByCol) do.call(mapply, c(FUN = base::list, infoByCol, SIMPLIFY = FALSE, USE.NAMES = FALSE))

