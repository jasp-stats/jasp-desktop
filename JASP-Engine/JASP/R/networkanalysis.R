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

	varsAsFactor <- NULL
	if (options[["groupingVariable"]] != "")
		varsAsFactor <- options[["groupingVariable"]]


	if (is.null(dataset)) {

		if (perform == "run") {

			dataset <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = varsAsFactor, exclude.na.listwise = NULL)

			if (options[["colorNodesBy"]] != "") # load data from variable that indicates groups
				options[["colorNodesByData"]] <- .readDataSetToEnd(columns = options[["colorNodesBy"]], exclude.na.listwise = options[["colorNodesBy"]])[[1]]

		} else {

			dataset <- .readDataSetHeader(columns.as.numeric = variables, columns.as.factor = varsAsFactor)

		}

	} else {

		dataset <- .vdf(dataset, columns.as.numeric = variables, columns.as.factor = varsAsFactor)

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
			list(name = "networkPLT",     type="collection", meta="image"),
			# list(name = "networkPLT",     type = "image"),
			list(name = "centralityPLT",  type = "image")
		)
	)

	defArgs <- c(
		# data
		"variables", "groupingVariable",
		# what kind of network is estimated
		"estimator",
		# arguments for the network
		"correlationMethod", "tuningParameter", "criterion", "isingEstimator",
		"nFolds", "split", "rule", "sampleSize",
		# general arguments
		"weightedNetwork", "signedNetwork", "missingValues"
	)
	stateKey <- list(
		# depends on everything but plotting arguments
		network = defArgs,
		centrality = c(defArgs, "normalizeCentrality"),
		# depends only on plotting arguments
		networkPLT = c(defArgs,
					   "plotWidthNetwork", "plotHeightNetwork",
					   "layout", "edgeColors", "repulsion", "edgeSize", "nodeSize", "colorNodesBy",
					   "maxEdgeStrength", "minEdgeStrength", "cut", "showDetails", "nodeColors", "showLegend", "legendNumber",
					   "weightedNetwork", "signedNetwork", "missingValues"),
		# depends only on plotting arguments
		centralityPLT = c(defArgs,
						  "plotWidthCentrality", "plotHeightCentrality",
						  "normalizeCentrality", "weightedNetwork", "signedNetwork", "missingValues")

	)
	# state[["network"]][["centrality"]] <- state[["centrality"]]

	## Do Analysis ## ----

	# Sort out whether things are set to defaults or not.
	if (length(variables) > 2) {

		# check for errors, but only if there was a change in the data (which implies state[["network"]] is NULL)
		if (is.null(state[["network"]])) {

			groupingVariable <- NULL
			if (options[["groupingVariable"]] != "")
				groupingVariable <- options[["groupingVariable"]]

			checks <- c("infinity", "variance", "observations")
			anyErrors <- .hasErrors(dataset = dataset, perform = perform,
									type = checks,
									variance.target = variables, # otherwise the grouping variable always has variance == 0
									variance.grouping = groupingVariable,
									observations.amount = " < 3",
									observations.grouping = groupingVariable,
									exitAnalysisIfErrors = TRUE)
		}

		network <- .doNetworkAnalysis(dataset = dataset, options = options, variables = variables, perform = perform,
									  oldNetwork = state)

	} else {

		network <- state[["network"]]

	}

	## Create Output ##  ----
	keep <- NULL
	results[["generalTB"]] <- .networkAnalysisGeneralTB(network, dataset, options, perform)
	# browser()
	if (options[["tableFitMeasures"]]) {
		results[["fitMeasuresTB"]] <- .fitMeasuresTB(network, options, perform)
	}
	if (options[["tableCentrality"]]) {
		results[["centralityTB"]] <- .networkAnalysisCentralityTB(network, options, perform)
	}
	if (options[["plotNetwork"]]) {
		results[["networkPLT"]] <- .networkAnalysisNetworkPlot(network, options, perform, oldPlot = state[["networkPLT"]])
		allNetworkPlots <- results[["networkPLT"]][["collection"]]
		for (nw in allNetworkPlots)
			keep <- c(keep, nw[["data"]])
	}
	if (options[["plotCentrality"]]) {
		results[["centralityPLT"]] <- .networkAnalysisCentralityPlot(network, options, perform, oldPlot = state[["centralityPLT"]])
		keep <- c(keep, results[["centralityPLT"]][["data"]])
	}

	## Exit Analysis ## ----

	# Save State
	state <- list(
		options = options,
		network = network[["network"]],
		centrality = network[["centrality"]],
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

	# early return if init
	if (perform != "run")
		return(NULL)

	if (options[["groupingVariable"]] == "") { # one network

		dataset <- list(dataset) # for compatability with the split behaviour

	} else { # multiple networks

		dataset <- split(dataset, dataset[[.v(options[["groupingVariable"]])]], drop = TRUE)

	}

	# empty list for results
	networkList <- list(network = oldNetwork[["network"]], centrality = oldNetwork[["centrality"]])

	# should never happen?
	if (length(oldNetwork) != length(dataset))
		oldNetwork <- NULL


	if (is.null(networkList[["network"]])) { # estimate network

		# first setup all bootnet arguments, then loop over datasets to estimate networks

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

		# for every dataset do the analysis
		for (nw in seq_along(dataset)) {

			data <- dataset[[nw]]
			data[[.v(options[["groupingVariable"]])]] <- NULL # grouping variable is not a node in the network
			network <- oldNetwork[["network"]][[nw]] # NULL or something usuable

			# Fake png hack -- qgraph::qgraph() has an unprotected call to `par()`. `par()` always opens a new device if there is none.
			# Perhaps ask Sacha to fix this in qgraph. Line 1119: if (DoNotPlot) par(pty = pty)
			tempFileName <- getTempFileName()
			grDevices::png(filename = tempFileName)

			# capture.output to get relevant messages (i.e. from qgraph::cor_auto "variables detected as...")
			msg <- capture.output(
				network <- bootnet::estimateNetwork(
					data = data,
					default = options[["estimator"]],
					.dots = .dots
				)
				, type = "message"
			)

			dev.off() # close the fake png
			file.remove(tempFileName) # remove the fake png file

			network[["corMessage"]] <- msg
			networkList[["network"]][[nw]] <- network

		}
	}

	if (is.null(networkList[["centrality"]])) { # calculate centrality measures

		for (nw in seq_along(dataset)) {

			network <- networkList[["network"]][[nw]]
			cent <- qgraph::centrality(network[["graph"]], weighted = options[["weightedNetwork"]], signed = options[["signedNetwork"]], all.shortest.paths = FALSE)

			# note: centrality table is (partially) calculated here so that centralityTable and centralityPlot don't compute the same twice.
			TBcent <- as.data.frame(cent[c("Betweenness", "Closeness", "InDegree", "OutDegree")])

			# adapted from qgraph::centrality_auto
			wmat <- qgraph::getWmat(network$graph)
			directedGraph <- ifelse(base::isSymmetric.matrix(object = wmat, tol = 1e-12), FALSE, TRUE)
			weightedGraph <- ifelse(all(qgraph::mat2vec(wmat) %in% c(0, 1)), FALSE, TRUE)

			if (directedGraph) {

				if (weightedGraph) {

					colnames(TBcent)[3:4] <- c("InStrength", "OutDegree")

				} else { # unweighted

					colnames(TBcent)[3:4] <- c("InDegree", "OutDegree")

				}

			} else { # undirected

				# divide betweenness by 2
				TBcent[, 1] <- TBcent[, 1] / 2
				TBcent <- TBcent[c(1:2, 4)]

				if (weightedGraph) {

					colnames(TBcent)[3] <- "Strength"

				} else { # unweighted

					colnames(TBcent)[3] <- "Degree"

				}

			}

			centVars <- apply(TBcent, 2, var)
			if (options[["normalizeCentrality"]] == "normalized") { # normalize only if variances are nonzero

				for (i in 1:ncol(TBcent)) { # code modified from base::scale.default

					valid <- !is.na(TBcent[, i])

					if (sum(valid) != 0) {

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

			} # else raw centrality measures -> do nothing

			TBcent[["node"]] <- .unv(network[["labels"]])
			nc <- ncol(TBcent)
			TBcent <- TBcent[c(nc, 1:(nc-1))] # put columns in intended order (of schema).
			cent <- TBcent

			networkList[["centrality"]][[nw]] <- cent

		}

	}

	if (is.null(names(dataset))) {
		nms <- "Network"
	} else {
		nms <- names(dataset)
	}

	names(networkList[["network"]]) <- names(networkList[["centrality"]]) <- nms

	return(networkList)

}

# wrappers for output ----
.networkAnalysisGeneralTB <- function(network, dataset, options, perform) {

	nGraphs <- max(1, length(network[["network"]]))
	table <- list(
		title = "Summary of Network",
		schema = list(fields = list(
			list(name = "info", title = "", type = "string")
			#list(name = "value", title = "", type = "number", format="sf:4;dp:3")
		))
	)
	for (i in seq_len(nGraphs)) {
		table[["schema"]][["fields"]][[i+1]] <-
			list(name = paste0("value", i), title = names(network[["network"]])[i], type = "number", format="sf:4;dp:3")
	}

	footnotes <- .newFootnotes()
	msg <- NULL

	TBcolumns <- list(info = c("Number of nodes", "Number of non-zero edges", "Sparsity"))

	if (perform != "run" || is.null(network)) { # fill in with .

		TBcolumns[["value"]] <- rep(".", 3*nGraphs)
		table[["status"]] <- "inited"
		table[["schema"]][["fields"]][[2]][["title"]] <- "Network"

	} else { # fill in with info from bootnet:::print.bootnet
		# browser()
		for (i in seq_len(nGraphs)) {

			nw <- network[["network"]][[i]]

			TBcolumns[[paste0("value", i)]] <- c(
				nrow(nw[["graph"]]),
				sum(nw[["graph"]][upper.tri(nw[["graph"]], diag = FALSE)] == 0),
				mean(nw[["graph"]][upper.tri(nw[["graph"]], diag = FALSE)] == 0)
			)

		}
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

.networkAnalysisCentralityTB <- function(network, options, perform) {

	nGraphs <- max(1, length(network[["network"]]))

	table <- list(
		title = "Centrality measures per variable",
		schema = list(fields = list(
			list(name = "Variable",    title = "Variable",    type = "string")
		))
	)

	# shared titles
	overTitles <- names(network[["network"]])
	if (is.null(overTitles))
		overTitles <- paste0("Network", 1:nGraphs)

	# create the fields
	for (i in seq_len(nGraphs)) {
		# three centrality columns per network

		table[["schema"]][["fields"]][[2 + 3*(i-1)]] <- list(name = paste0("Betweenness", i), title = "Betweenness", type = "number", format="sf:4;dp:3", overTitle = overTitles[i])
		table[["schema"]][["fields"]][[3 + 3*(i-1)]] <- list(name = paste0("Closeness", i),   title = "Closeness",   type = "number", format="sf:4;dp:3", overTitle = overTitles[i])
		table[["schema"]][["fields"]][[4 + 3*(i-1)]] <- list(name = paste0("Strength", i),    title = "Strength",    type = "number", format="sf:4;dp:3", overTitle = overTitles[i])

	}

	if (perform != "run") { # make empty table

		if (!is.null(options[["variables"]]) || !(length(options[["variables"]]) > 0)) {

			TBcolumns <- list(".", ".", ".", ".")
			names(TBcolumns) <- c("Variable", "Betweenness", "Closeness", "Strength")

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

		TBcolumns <- NULL
		for (i in seq_len(nGraphs)) {

			toAdd <- network[["centrality"]][[i]]
			names(toAdd) <- c("Variable", paste0(c("Betweenness", "Closeness", "Strength"), i))
			if (i == 1) {# if more than 1 network drop additional variable column
				TBcolumns <- toAdd
			} else {
				toAdd <- toAdd[, -1]
				TBcolumns <- cbind(TBcolumns, toAdd)
			}

		}

	}

	# names(TBcolumns) <- c("Variable", "Betweenness", "Closeness", "Strength")
	data <- .TBcolumns2TBrows(TBcolumns)
	table[["data"]] <- data

	return(table)

}

.networkAnalysisOneNetworkPlot <- function() {

	# eval(quote()) construction because this function is evaluated inside .writeImage()
	# which needs to look 2 levels up to find the objects network and options.
	eval(quote({

		# ensure input makes sense or ignore these parameters
		minE <- options[["minEdgeStrength"]]
		maxE <- options[["maxEdgeStrength"]]

		if (minE == 0 ||maxE <= minE) {
			minE <- NULL
			maxE <- NULL
		}

		wMat <- network[["graph"]]
		if (!options[["weightedNetwork"]]) {
			wMat <- sign(wMat)
		}
		if (!options[["signedNetwork"]]) {
			wMat <- abs(wMat)
		}

		qgraph::qgraph(
			input      = wMat,
			layout     = layout, # options[["layout"]],
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
			legend     = legend # options[["showLegend"]]
		)}
	), envir = parent.frame(2))

}

.networkAnalysisCentralityPlot <- function(network, options, perform, oldPlot = NULL) {

	if (!is.null(oldPlot) && !identical(oldPlot[["data"]], ""))
		return(oldPlot)

	plot <- list(
		title = "Centrality Plot",
		width = options[["plotWidthCentrality"]],
		height = options[["plotHeightCentrality"]],
		custom = list(width = "plotWidthCentrality", height = "plotHeightCentrality"),
		data = "",
		status = "inited"
	)

	if (perform == "run") {

		wide <- network[["centrality"]]
		wideDf <- Reduce(rbind, wide)
		if (length(wide) > 1) {
			wideDf[["graph"]] <- rep(names(network[["centrality"]]), each = nrow(wideDf) / length(wide))
			Long <- reshape2::melt(wideDf, id.vars = c("node", "graph"))
			colnames(Long)[3] <- "measure"
		} else {
			Long <- reshape2::melt(wideDf, id.vars = "node")
			colnames(Long)[2] <- "measure"
		}
		Long[["type"]] <- NA

		# code modified from qgraph::centralityPlot()

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
		g <- g + ggplot2::theme_bw()

		content <- .writeImage(width = plot[["width"]], height = plot[["height"]], plot = g)

		plot[["convertible"]] <- TRUE
		plot[["data"]] <- content[["png"]]
		plot[["obj"]] <- content[["obj"]]
		plot[["status"]] <- "complete"

	}

	return(plot)

}

.networkAnalysisNetworkPlot <- function(network, options, perform, oldPlot = NULL) {

	if (!is.null(oldPlot) && !identical(oldPlot[["collection"]][[1]][["data"]], ""))
		return(oldPlot)

	plot <- list(
		title = "Network Plot",
		collection = list(),
		status = "inited"
	)

	subPlotBase <- list(
		width = options[["plotWidthNetwork"]],
		height = options[["plotHeightNetwork"]],
		custom = list(width = "plotWidthNetwork", height = "plotHeightNetwork")
	)

	if (perform == "run") {

		allNetworks <- network[["network"]]
		nGraphs <- length(allNetworks)
		if (options[["layout"]] == "circle") {
			layout <- "circle"
		} else {

			# this calls qgraph::qgraph so we need the png trick again
			tempFileName <- getTempFileName()
			grDevices::png(filename = tempFileName)
			layout <- qgraph::averageLayout(network[["network"]], repulsion = options[["repulsion"]])
			dev.off() # close the fake png
			file.remove(tempFileName) # remove the fake png file

		}
		
		groups <- NULL
		allLegends <- rep(FALSE, nGraphs) # no legends
		if (!is.null(options[["colorNodesByData"]])) {
			
			u <- unique(options[["colorNodesByData"]])
			groups <- lapply(u, function(x, y) which(y == x), y = options[["colorNodesByData"]])
			names(groups) <- u
			
			if (options[["showLegend"]] ==  "All plots") {
				
				allLegends <- rep(TRUE, nGraphs)
				
			} else if (options[["showLegend"]] ==  "In plot number: ") {
				
				if (options[["legendNumber"]] > nGraphs) {
					
					allLegends[nGraphs] <- TRUE
					
				} else if (options[["legendNumber"]] < 1) {
					
					allLegends[1] <- TRUE
					
				} else {
					
					allLegends[options[["legendNumber"]]] <- TRUE
					
				}
				
			}
			
		} 
		
		names(allLegends) <- names(allNetworks) # allows indexing by name
		
		for (v in names(allNetworks)) {

			subPlot <- subPlotBase
			subPlot[["title"]] <- v
			subPlot[["name"]] <- v

			network <- allNetworks[[v]]
			legend <- allLegends[[v]]
			content <- .writeImage(width = subPlotBase[["width"]], height = subPlotBase[["height"]], plot = .networkAnalysisOneNetworkPlot)

			subPlot[["convertible"]] <- TRUE
			subPlot[["data"]] <- content[["png"]]
			subPlot[["obj"]] <- content[["obj"]]
			subPlot[["status"]] <- "complete"
			plot[["collection"]][[v]] <- subPlot

		}

		plot[["status"]] <- "complete"

	} else {

		subPlot <- subPlotBase
		subPlot[["network 1"]] <- "network 1"
		subPlot[["data"]] <- ""
		subPlot[["status"]] <- "inited"
		plot[["collection"]][["network 1"]] <- subPlot

	}

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

.networkAnalysisCentrality <- function (x, weighted = TRUE, signed = TRUE) {

	# modified version of centrality_auto that does not compute shortest path lengths

	x <- qgraph::getWmat(x)
	if (is.list(x)) {
		return(lapply(x, .networkAnalysisCentrality,
					  weighted = weighted,
					  signed = signed))
	}
	if (!isTRUE(weighted)) {
		x <- sign(x)
	}
	if (!isTRUE(signed)) {
		x <- abs(x)
	}
	if (!is.matrix(x))
		stop("the input network must be an adjacency or weights matrix")
	diag(x) <- 0
	x <- abs(x)
	directed.gr <- ifelse(isSymmetric.matrix(object = x, tol = 1e-12),
						  FALSE, TRUE)
	weighted.gr <- ifelse(all(qgraph::mat2vec(x) %in% c(0, 1)), FALSE,
						  TRUE)

	# pdf trick
	net_qg <- qgraph(x, diag = FALSE, labels = colnames(x), DoNotPlot = TRUE, minimum = 0)
	centr <- centrality(net_qg)
	if (directed.gr & !weighted.gr)
		centr1 <- data.frame(cbind(Betweenness = centr$Betweenness,
								   Closeness = centr$Closeness, InDegree = centr$InDegree,
								   OutDegree = centr$OutDegree))
	if (directed.gr & weighted.gr)
		centr1 <- data.frame(cbind(Betweenness = centr$Betweenness,
								   Closeness = centr$Closeness, InStrength = centr$InDegree,
								   OutStrength = centr$OutDegree))
	if (!directed.gr & !weighted.gr)
		centr1 <- data.frame(cbind(Betweenness = centr$Betweenness/2,
								   Closeness = centr$Closeness, Degree = centr$OutDegree))
	if (!directed.gr & weighted.gr)
		centr1 <- data.frame(cbind(Betweenness = centr$Betweenness/2,
								   Closeness = centr$Closeness, Strength = centr$OutDegree))
	row.names(centr1) <- colnames(x)
	log <- capture.output({
		largcomp <- component.largest(x, connected = "strong")
	})
	if (sum(largcomp) < ncol(x)) {
		x2 <- x[largcomp, largcomp]
		clos <- centrality(qgraph(x2, diag = FALSE, labels = colnames(x)[largcomp],
								  DoNotPlot = TRUE, minimum = 0))$Closeness
		centr1$Closeness[largcomp] <- clos
		centr1$Closeness[!largcomp] <- NA
	}
	return(centr1)
}