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

			if (options[["colorNodesBy"]] != "") { # load data from variable that indicates groups
				options[["colorNodesByData"]] <- .readDataSetToEnd(columns = options[["colorNodesBy"]], exclude.na.listwise = options[["colorNodesBy"]])[[1]]
				options[["colorNodesByData"]] <- options[["colorNodesByData"]][seq_along(variables)]

			}
			if (options[["mgmVariableType"]] != "") {# load data from variable that indicates variable type
				options[["mgmVariableTypeData"]] <- .readDataSetToEnd(columns = options[["mgmVariableType"]], exclude.na.listwise = options[["mgmVariableType"]])[[1]]

				# some robustness
				if (length(options[["mgmVariableTypeData"]]) < length(variables)) { # too short
					options[["mgmVariableTypeDataOkay"]] <- -1
				} else if (length(options[["mgmVariableTypeData"]]) > length(variables)) { # too long
					options[["mgmVariableTypeDataOkay"]] <- 1
				}
				options[["mgmVariableTypeData"]] <- options[["mgmVariableTypeData"]][seq_along(variables)]
				options[["mgmVariableTypeData"]][is.na(options[["mgmVariableTypeData"]])] <- "g" # set missing to gaussian (in case of too few types supplied)

			}
		} else {

			dataset <- .readDataSetHeader(columns.as.numeric = variables, columns.as.factor = varsAsFactor)

		}

	} else {

		dataset <- .vdf(dataset, columns.as.numeric = variables, columns.as.factor = varsAsFactor)

	}

	# ensure order of variables matches order of columns in dataset
	newOrder <- match(.unv(colnames(dataset)), variables, nomatch = 0L)
	variables <- variables[newOrder]
	options[["mgmVariableTypeData"]] <- options[["mgmVariableTypeData"]][newOrder]

	## Initialize Results & statekey ## ----
	results <- list(
		title = "Network Analysis",
		.meta = list(
			list(name = "generalTB",                type = "table"),
			list(name = "fitMeasuresTB",            type = "table"),
			list(name = "bootstrapTB",              type = "table"),
			list(name = "weightmatrixTB",           type = "table"),
			list(name = "centralityTB",             type = "table"),
			list(name = "mgmTB",                    type = "table"),
			list(name = "networkPLT",               type="collection", meta="image"),
			list(name = "centralityPLT",            type = "image"),
			list(name = "bootstrapEdgePLT",         type="collection", meta="image"),
			list(name = "bootstrapCentPLT",         type="collection", meta="image")
		)
	)

	defArgs <- c(
		# data
		"variables", "groupingVariable",
		# what kind of network is estimated
		"estimator",
		# arguments for the network
		"correlationMethod", "tuningParameter", "criterion", "isingEstimator",
		"nFolds", "split", "rule", "sampleSize", "thresholdBox", "thresholdString", "thresholdValue",
		# general arguments
		"weightedNetwork", "signedNetwork", "missingValues"
	)
	bootstrapArgs <-c(defArgs, "numberOfBootstraps", "BootstrapType",
					  "StatisticsEdges", "StatisticsStrength", "StatisticsCloseness", "StatisticsBetweenness", "StatisticsBetweenness")
	
	stateKey <- list(
		# depends on everything but plotting arguments
		network = defArgs,
		# depends also on bootstrap options
		bootstrap = bootstrapArgs,
		# depends also on normalization of centrality measures
		centrality = c(defArgs, "normalizeCentrality", "maxEdgeStrength", "minEdgeStrength"),
		# depends also on plotting arguments
		networkPLT = c(defArgs,
					   "plotWidthNetwork", "plotHeightNetwork",
					   "layout", "edgeColors", "repulsion", "edgeSize", "nodeSize", "colorNodesBy",
					   "maxEdgeStrength", "minEdgeStrength", "cut", "showDetails", "nodeColors", "showLegend", "legendNumber",
					   "showMgmVariableType", "showVariableNames", "graphSize", "scaleLabels", "labelSize"),
		# depends also on plotting arguments
		centralityPLT = c(defArgs,
						  "plotWidthCentrality", "plotHeightCentrality", "normalizeCentrality"),
		bootstrapCentPLT = bootstrapArgs,
		bootstrapCentPLT = bootstrapArgs
	)

	# show info about variable types. Done here so that hopefully a table gets shown even if .hasErrors finds something bad.
	if (options[["estimator"]] == "mgm" && !is.null(options[["mgmVariableTypeData"]])) {
		
		results[["generalTB"]] <- .networkAnalysisGeneralTable(NULL, dataset, options, perform = "init") # any errors will appear top of this table
		results[["mgmTB"]] <- .networkAnalysisMgmVariableInfoTable(network, options, perform)
		
		if (options[["bootstrapOnOff"]]) # add bootstrap information as well
			results[["bootstrapTB"]] <- .networkAnalysisBootstrapTable(network, dataset, options, perform, when = "before")
			
		if ( ! .shouldContinue(callback(results)))
			return()
	} #else if (options[["bootstrapOnOff"]]) { 
		# 
		# results[["generalTB"]] <- .networkAnalysisGeneralTable(NULL, dataset, options, perform = "init") # any errors will appear top of this table
		# print("Here_0")
		# results[["bootstrapTB"]] <- .networkAnalysisBootstrapTable(network, dataset, options, perform, when = "before", results = results)
		# print("Here_2")
		# print(str(callback(results)))
		# for (i in 1:5) {
		# 	print(paste0("Here_2_", i))
		# 	results[["bootstrapTB"]] <- .networkAnalysisBootstrapTable(network, dataset, options, perform, when = "before", where = i)
		# 	Sys.sleep(1)
		# 	if ( ! .shouldContinue(callback(results)))
		# 		return()
		# }
	# 	print("Here_3")
	# }

	## Do Analysis ## ----

	# Sort out whether things are set to defaults or not.
	if (length(variables) > 2) {

		# check for errors, but only if there was a change in the data (which implies state[["network"]] is NULL)
		if (is.null(state[["network"]])) {

			groupingVariable <- NULL
			if (options[["groupingVariable"]] != "")
				groupingVariable <- options[["groupingVariable"]]

			checks <- c("infinity", "variance", "observations")
			categoricalVars <- NULL
			if (options[["estimator"]] == "mgm" && "c" %in% options[["mgmVariableTypeData"]]) {
				categoricalVars <- variables[options[["mgmVariableTypeData"]] == "c"]
				checks <- c(checks, "factorLevels")
			}

			anyErrors <- .hasErrors(dataset = dataset, perform = perform,
									type = checks,
									variance.target = variables, # otherwise the grouping variable always has variance == 0
									variance.grouping = groupingVariable,
									factorLevels.target = categoricalVars,
									factorLevels.amount = "> 10", # probably a misspecification of mgmVariableType when this happens.
									factorLevels.grouping = groupingVariable,
									observations.amount = " < 3",
									observations.grouping = groupingVariable,
									exitAnalysisIfErrors = TRUE)
		}

		network <- .networkAnalysisRun(dataset = dataset, options = options, variables = variables, perform = perform, oldNetwork = state)
		if (options[["bootstrapOnOff"]]) {
			# for (i in seq_len(options[["numberOfBootstraps"]])) {
				network <- .networkAnalysisBootstrap(network, options, variables, perform)
			# }
		}

	} else {

		network <- NULL

	}

	## Create Output ##  ----
	keep <- NULL
	results[["generalTB"]] <- .networkAnalysisGeneralTable(network, dataset, options, perform)

	if (!is.null(network) && network[["status"]] == "error") {
		results[["generalTB"]][["error"]] <- list(errorMessage = "A proper network could not be estimated. To receive assistance with this problem, please report the message above at: https://jasp-stats.org/bug-reports",
												  errorType    = "badData")
	} else {
		if (FALSE && options[["tableFitMeasures"]]) {
			results[["fitMeasuresTB"]] <- .fitMeasuresTB(network, options, perform)
		}
		if (options[["bootstrapOnOff"]]) {
			results[["bootstrapTB"]] <- .networkAnalysisBootstrapTable(network, dataset, options, perform, when = "after")
			
			# if (options[["BootstrapType"]] %in% c("nonparametric", "parametric")) {
				results[["bootstrapEdgePLT"]] <- .networkAnalysisBootstrapPlot(network, options, perform, "edge", oldPlot = state[["bootstrapEdgePLT"]])
				results[["bootstrapCentPLT"]] <- .networkAnalysisBootstrapPlot(network, options, perform, c("strength", "betweenness", "closeness"), oldPlot = state[["bootstrapCentPLT"]])
				
				allBootstrapPlots <- results[["bootstrapEdgePLT"]][["collection"]]
				for (bt in allBootstrapPlots)
					keep <- c(keep, bt[["data"]])
				
				allBootstrapPlots <- results[["bootstrapCentPLT"]][["collection"]]
				for (bt in allBootstrapPlots)
					keep <- c(keep, bt[["data"]])
				
			# }
			
		}
		if (options[["tableWeightsMatrix"]]) {
			results[["weightmatrixTB"]] <- .networkAnalysisWeightMatrixTable(network, options, variables, perform)
		}
		if (options[["tableCentrality"]]) {
			results[["centralityTB"]] <- .networkAnalysisCentralityTable(network, options, perform)
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
	}
	results <- .networkAnalysisAddReferencesToTables(results, options)
	## Exit Analysis ## ----

	# Save State
	state <- list(
		options          = options,
		network          = network[["network"]],
		bootstrap        = network[["bootstrap"]],
		centrality       = network[["centrality"]],
		networkPLT       = results[["networkPLT"]],
		centralityPLT    = results[["centralityPLT"]],
		bootstrapEdgePLT = results[["bootstrapEdgePLT"]],
		bootstrapCentPLT = results[["bootstrapCentPLT"]]
	)

	attr(state, "key") <- stateKey

	# return to jasp
	if (perform == "init") {

		return(list(results = results, status = "inited", state = state, keep = keep))

	} else {

		return(list(results = results, status = "complete", state = state, keep = keep))

	}

}

# functions for running analyses ----
.networkAnalysisRun <- function(dataset, options, variables, perform, oldNetwork = NULL) {

	# early return if init, return NULL or results from state
	if (perform != "run") {

		if (!is.null(oldNetwork))
			oldNetwork[["status"]] <- .networkAnalysisNetworkHasErrors(oldNetwork)

		return(oldNetwork)

	}

	if (options[["groupingVariable"]] == "") { # one network

		dataset <- list(dataset) # for compatability with the split behaviour

	} else { # multiple networks

		dataset <- split(dataset, dataset[[.v(options[["groupingVariable"]])]], drop = TRUE)

	}

	# empty list for results
	networkList <- list(network = oldNetwork[["network"]], centrality = oldNetwork[["centrality"]])
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

		# additional checks for mgm
		level <- NULL # the levels of categorical variables
		type <- NULL  # the type of each variable
		if (options[["estimator"]] == "mgm") {

			nvar <- length(variables)
			level <- rep(1, nvar)
			if (is.null(options[["mgmVariableTypeData"]])) {
				type <- rep("g", nvar)
			} else {
				type <- options[["mgmVariableTypeData"]]
				invalidType <- is.na(type) | !(type %in% c("g", "c", "p"))
				type[invalidType] <- "g" # set missing to gaussian. TODO: add to table message.

				if (any(invalidType))
					networkList[["message"]] <- c(networkList[["message"]],
												  sprintf("The variable types supplied in %s contain missing values or values that do not start with 'g', 'c', or 'p'. These have been reset to gaussian ('g'). They were indices %s.",
												  		options[["mgmVariableType"]], paste(which(invalidType), sep = ", ")))

				# find out the levels of each categorical variable
				for (i in which(type == "c"))
					level[i] <- max(1, nlevels(dataset[[1]][[i]]), length(unique(dataset[[1]][[i]])))

			}

		}
		
		if (options[["thresholdBox"]] == "value") {
			threshold <- options[["thresholdValue"]]
		} else { # options[["thresholdBox"]] == "method"
			threshold <- options[["thresholdString"]]
		}

		# names of .dots must match argument names of bootnet_{estimator name}
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
			sampleSize  = options[["sampleSize"]],
			type        = type,
			lev         = level,
			threshold   = threshold
		)

		# get available arguments for specific network estimation function. Removes unused ones.
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

			# saveDataToDput(data, options, .dots)

			# Fake png hack -- qgraph::qgraph() has an unprotected call to `par()`. `par()` always opens a new device if there is none.
			# Perhaps ask Sacha to fix this in qgraph. Line 1119: if (DoNotPlot) par(pty = pty)
			tempFileName <- getTempFileName()
			grDevices::png(filename = tempFileName)

			# try since this sometimes failes for unpredictable reasons...
			# saveDataToDput(data, options, .dots)

			# anyError <- try({
				# capture.output to get relevant messages (i.e. from qgraph::cor_auto "variables detected as...") (TODO: use this info)
				msg <- capture.output(
					network <- bootnet::estimateNetwork(
						data = data,
						default = options[["estimator"]],
						.dots = .dots
					)
					, type = "message"
				)
			# })
			dev.off() # close the fake png
			file.remove(tempFileName) # remove the fake png file
			# if (inherits("try-error", anyError))
			# 	return(list(network = NULL, centrality = NULL, status = "error"))

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
			if (options[["weightedNetwork"]])
				wmat <- sign(wmat)

			if (options[["signedNetwork"]])
				wmat <- abs(wmat)

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

	networkList[["status"]] <- .networkAnalysisNetworkHasErrors(networkList)

	return(networkList)

}

.networkAnalysisBootstrap <- function(network, options, variables, perform) {

	if (perform == "run") {
		
		network[["bootstrap"]] <- list()
		allNetworks <- network[["network"]]
		nGraphs <- length(allNetworks)
		
		if (options[["parallelBootstrap"]]) {
			nCores <- parallel::detectCores(TRUE)
			if (is.na(nCores)) # parallel::detectCores returns NA if unknown/ fails
				nCores <- 1
		} else {
			nCores <- 1
		}
		
		statistics <- c("edge", "strength", "closeness", "betweenness")
		statistics <- statistics[unlist(options[c("StatisticsEdges", "StatisticsStrength", "StatisticsCloseness", "StatisticsBetweenness")])]
		
		# Fake png hack -- qgraph::qgraph() has an unprotected call to `par()`. `par()` always opens a new device if there is none.
		# Perhaps ask Sacha to fix this in qgraph. Line 1119: if (DoNotPlot) par(pty = pty)
		tempFileName <- getTempFileName()
		grDevices::png(filename = tempFileName)

		for (nm in names(allNetworks)) {
			
			network[["bootstrap"]][[nm]] <- bootnet::bootnet(data = allNetworks[[nm]],
															 nBoots = options[["numberOfBootstraps"]],
															 nCores = nCores,
															 statistics = statistics,
															 labels = variables
			)
			
		}
		
		dev.off() # close the fake png
		file.remove(tempFileName) # remove the fake png file
		
	}

	return(network)

}

# functions for tables ----
.networkAnalysisGeneralTable <- function(network, dataset, options, perform) {

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
				sprintf("Data was binarized using %s. ",	options$split)
			)

		}

		if (!is.null(options[["colorNodesByData"]]) && length(options[["colorNodesByData"]]) != length(options[["variables"]])) {

			msg <- c(msg,
				sprintf("Only the first %d values of %s were used to color nodes (%d provided). ",
						length(options[["variables"]]),
						options[["colorNodesBy"]],
						length(options[["colorNodesByData"]]))
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

.networkAnalysisBootstrapTable <- function(network, dataset, options, perform, when, where = 0, results = NULL) {

	bootstrapType <- options[["BootstrapType"]]
	substr(bootstrapType, 1, 1) <- toupper(substr(bootstrapType, 1, 1)) # capitalization
	
	table <- list(
		title = "Bootstrap summary of Network",
		schema = list(fields = list(
			list(name = "type", title = "Type", type = "string"),
			list(name = "numberOfBootstraps", title = "Number of bootstraps", type = "number", format="sf:4;dp:3"),
			list(name = "when", title = "Status", type = "string"),
			list(name = "test", title = "test", type = "number", format="sf:4;dp:3")
		)),
		data = list(
			list(type = bootstrapType,
				 numberOfBootstraps = options[["numberOfBootstraps"]],
				 when = "Completed",
				 test = where
			)
		),
		status = "inited"
	)
	
	
	
	if (perform == "run") {
		
		if (when == "before") {
			table[["data"]][[1]][["when"]] <- "In progress"
			table[["status"]] <- "running"
			
			if (!is.null(results)) {
				print("Here_1")
				for (i in 1:5) {
					print(paste0("Here_1_", i))
					table[["data"]][[1]][["test"]] <- i
					
					callbackResult <- callback(results)
					print(str(callbackResult))
					if ( ! .shouldContinue(callbackResult))
						return()
					
				}
			}
		}
		
		table[["status"]] <- "complete"
	} else {
		table[["status"]] <- "inited"
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

.networkAnalysisWeightMatrixTable <- function(network, options, variables, perform) {
	# browser()
	nGraphs <- max(1, length(network[["network"]]))

	table <- list(
		title = "Weights matrix",
		schema = list(fields = list(
			list(name = "Variable", title = "Variable", type = "string")
		))
	)

	# shared titles
	overTitles <- names(network[["network"]])
	if (is.null(overTitles))
		overTitles <- "Network" # paste0("Network", 1:nGraphs)

	nVar <- length(variables)
	for (i in seq_len(nGraphs)) {
		for (v in seq_len(nVar)) { # loop is not entered if nVar == 0
			table[["schema"]][["fields"]][[1 + v + nVar*(i-1)]] <- list(name = paste0(variables[v], i), title = variables[v], type = "number", format="sf:4;dp:3", overTitle = overTitles[i])
		}
	}

	if (perform != "run" || is.null(network)) { # make empty table

		if (is.null(options[["variables"]]) || !length(options[["variables"]]) > 0) { # 1 by 1 table

			TBcolumns <- list(Variable = ".")

		} else { # table of nVariables by nVariables

			# dataframe since reshape2 below returns dataframes
			TBcolumns <- rep(list(rep(".", nVar)), nVar)
			names(TBcolumns) <- paste0(variables, i)
			TBcolumns[["Variable"]] <- variables
		}
		table[["status"]] <- "inited"

	} else { # fill with results

		allNetworks <- network[["network"]]
		for (i in seq_len(nGraphs)) {

			toAdd <- allNetworks[[i]][["graph"]]
			if (!options[["weightedNetwork"]])
				toAdd <- sign(toAdd)
			if (!options[["signedNetwork"]])
				toAdd <- abs(toAdd)

			toAdd <- as.data.frame(toAdd)
			names(toAdd) <- paste0(variables, i)

			if (i > 1) {
				TBcolumns <- cbind(TBcolumns, toAdd)
			} else {
				TBcolumns <- toAdd
			}

		}
		TBcolumns <- cbind(data.frame(Variable = variables), TBcolumns)
		table[["status"]] <- "complete"

	}

	data <- .TBcolumns2TBrows(TBcolumns)
	table[["data"]] <- data

	return(table)

}

.networkAnalysisMgmVariableInfoTable <- function(network, options, perform) {

	table <- list(
		title = "Type of variables",
		schema = list(fields = list(
			list(name = "Variable", title = "Variable", type = "string"),
			list(name = "Type", title = "Type", type = "string")
			# list(name = "Type", title = "Type", type = "string")
		))
	)

	if (perform != "run") {

		vars = "."
		types = "."
		table[["status"]] <- "inited"

	} else {

		vars <- options[["variables"]]
		types <- options[["mgmVariableTypeData"]]
		types <- c("Gaussian", "Categorical", "Poisson")[match(options[["mgmVariableTypeData"]], c("g", "c", "p"))]
		table[["status"]] <- "complete"

	}

	table[["data"]] <- .TBcolumns2TBrows(list(Variable = vars, Type = types))

	return(table)

}

.networkAnalysisCentralityTable <- function(network, options, perform) {

	nGraphs <- max(1, length(network[["network"]]))

	table <- list(
		title = "Centrality measures per variable",
		schema = list(fields = list(
			list(name = "Variable", title = "Variable", type = "string")
		))
	)

	# shared titles
	overTitles <- names(network[["network"]])
	if (is.null(overTitles))
		overTitles <- "Network" # paste0("Network", 1:nGraphs)

	# create the fields
	nameCol3 <- "Strength"
	if ("Degree" %in% colnames(network[["centrality"]][[1]])[3])
		nameCol3 <- "Degree"

	for (i in seq_len(nGraphs)) {
		# three centrality columns per network

		table[["schema"]][["fields"]][[2 + 3*(i-1)]] <- list(name = paste0("Betweenness", i), title = "Betweenness", type = "number", format="sf:4;dp:3", overTitle = overTitles[i])
		table[["schema"]][["fields"]][[3 + 3*(i-1)]] <- list(name = paste0("Closeness", i),   title = "Closeness",   type = "number", format="sf:4;dp:3", overTitle = overTitles[i])
		table[["schema"]][["fields"]][[4 + 3*(i-1)]] <- list(name = paste0(nameCol3, i),    title = "Strength",    type = "number", format="sf:4;dp:3", overTitle = overTitles[i])

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
		table[["status"]] <- "complete"

	}

	# names(TBcolumns) <- c("Variable", "Betweenness", "Closeness", "Strength")
	data <- .TBcolumns2TBrows(TBcolumns)
	table[["data"]] <- data

	return(table)

}

# functions for plots ----
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
			wideDf[["type"]] <- rep(names(network[["centrality"]]), each = nrow(wideDf) / length(wide))
			Long <- reshape2::melt(wideDf, id.vars = c("node", "type"))
			colnames(Long)[3] <- "measure"
			Long[["graph"]] <- Long[["type"]]
			Long[["type"]] <- TRUE # options[["separateCentrality"]]
		} else {
			Long <- reshape2::melt(wideDf, id.vars = "node")
			colnames(Long)[2] <- "measure"
			Long[["graph"]] <- NA
		}

		# code modified from qgraph::centralityPlot(). Type and graph are switched so the legend title says graph

		Long <- Long[gtools::mixedorder(Long$node), ]
		Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))
		if (length(unique(Long$graph)) > 1) {
			g <- ggplot2::ggplot(Long, ggplot2::aes(x = value, y = node, group = graph,
													colour = graph))
		} else {
			g <- ggplot2::ggplot(Long, ggplot2::aes(x = value, y = node, group = graph))
		}
		g <- g + ggplot2::geom_path() + ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::geom_point()
		if (length(unique(Long$type)) > 1) {
			g <- g + ggplot2::facet_grid(type ~ measure, scales = "free")

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

.networkAnalysisOneNetworkPlot <- function() {

	# eval(quote()) construction because this function is evaluated inside .writeImage()
	# which needs to look 2 levels up to find the objects network, options, layout, groups, legend, and shape.
	eval(quote({

		wMat <- network[["graph"]]
		if (!options[["weightedNetwork"]]) {
			wMat <- sign(wMat)
		}
		if (!options[["signedNetwork"]]) {
			wMat <- abs(wMat)
		}

		qgraph::qgraph(
			input       = wMat,
			layout      = layout, # options[["layout"]],
			groups      = groups,
			repulsion   = options[["repulsion"]],
			cut         = options[["cut"]],
			edge.width  = options[["edgeSize"]],
			node.width  = options[["nodeSize"]],
			maximum     = maxE, # options[["maxEdgeStrength"]],
			minimum     = minE, # options[["minEdgeStrength"]],
			details     = options[["showDetails"]],
			labels      = labels, # .unv(network[["labels"]]),
			palette     = options[["nodeColors"]],
			theme       = options[["edgeColors"]],
			legend      = legend, # options[["showLegend"]]
			shape       = shape,
			color       = nodeColor,
			edge.color  = edgeColor,
			nodeNames   = nodeNames,
			label.scale = options[["scaleLabels"]],
			label.cex   = options[["labelSize"]]
		)}
	), envir = parent.frame(2))

}

.networkAnalysisNetworkPlot <- function(network, options, perform, oldPlot = NULL) {

	if (!is.null(oldPlot) && !identical(oldPlot[["collection"]][[1]][["data"]], ""))
		return(oldPlot)

	plot <- list(
		title = "Network Plot",
		collection = list(),
		status = "inited"
	)

	if (options[["graphSize"]] == "graphSizeFree") {
		subPlotBase <- list(
			width = options[["plotWidthNetwork"]],
			height = options[["plotHeightNetwork"]],
			custom = list(width = "plotWidthNetwork", height = "plotHeightNetwork")
		)
	} else if (options[["plotWidthNetwork"]] > options[["plotHeightNetwork"]]) {
		subPlotBase <- list(
			width = options[["plotWidthNetwork"]],
			height = options[["plotWidthNetwork"]],
			custom = list(width = "plotWidthNetwork", height = "plotWidthNetwork")
		)
	} else {
		subPlotBase <- list(
			width = options[["plotHeightNetwork"]],
			height = options[["plotHeightNetwork"]],
			custom = list(width = "plotHeightNetwork", height = "plotHeightNetwork")
		)
	}

	if (perform == "run") {

		allNetworks <- network[["network"]]
		nGraphs <- length(allNetworks)
		if (options[["layout"]] == "circle") {
			layout <- "circle"
		} else if (nGraphs > 1) {

			# this calls qgraph::qgraph so we need the png trick again
			tempFileName <- getTempFileName()
			grDevices::png(filename = tempFileName)
			layout <- qgraph::averageLayout(allNetworks, repulsion = options[["repulsion"]])
			dev.off() # close the fake png
			file.remove(tempFileName) # remove the fake png file

		} else {
			layout <- "spring"
		}

		# ensure minimum/ maximum makes sense or ignore these parameters TODO message in general table.
		minE <- options[["minEdgeStrength"]]
		maxE <- options[["maxEdgeStrength"]]
		
		if (minE == 0)
			minE <- NULL
		if (maxE == 0 || maxE <= minE)
			maxE <- NULL

		groups <- NULL
		allLegends <- rep(FALSE, nGraphs) # no legends
		if (!is.null(options[["colorNodesByData"]])) { # define groups

			u <- unique(options[["colorNodesByData"]])
			groups <- lapply(u, function(x, y) which(y == x), y = options[["colorNodesByData"]])
			names(groups) <- u

		}

		# defaults
		shape <- "circle"
		nodeColor <- NULL
		edgeColor <- NULL
		if (options[["estimator"]] == "mgm" && options[["mgmVariableType"]] != "" ) {

			if (options[["showMgmVariableType"]] == "mgmNodeShape") {
				#         gaussian, categorical, poisson
				opts <- c("circle", "square",    "triangle")
				shape <- opts[match(options[["mgmVariableTypeData"]], c("g", "c", "p"))]
			} else if (options[["showMgmVariableType"]] == "mgmNodeColor") {
				#         gaussian,  categorical, poisson
				opts <- c("#00a9e6", "#fb8b00",   "#00ba63")
				nodeColor <- opts[match(options[["mgmVariableTypeData"]], c("g", "c", "p"))]

			}
		}

		# TODO: footnote if legend off and nodenames used
		if (options[["showVariableNames"]] == "In nodes") {
			nodeNames <- NULL
			labels <- .unv(allNetworks[[1]][["labels"]])

		} else {

			nodeNames <- .unv(allNetworks[[1]][["labels"]])
			labels <- seq_along(nodeNames)

		}

		# do we need to draw legends?
		if (!is.null(groups) || !is.null(nodeNames)) {
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
			if (options[["estimator"]] == "mgm") {

				edgeColor <- network[["results"]][["pairwise"]][["edgecolor"]]
				# version compatability?
				if (is.null(edgeColor) || !all(dim(edgeColor) != dim(network[["graph"]])))
					edgeColor <- network[["results"]][["edgecolor"]]

			}
			# saveDataToDput(network, options, allLegends)

			legend <- allLegends[[v]]
			if (legend && options[["graphSize"]] == "graphSizeFixed") {
				subPlot[["width"]] <- 1.4 * subPlot[["height"]]
			}

			content <- .writeImage(width = subPlot[["width"]], height = subPlot[["height"]], plot = .networkAnalysisOneNetworkPlot)

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

.networkAnalysisOneBootstrapPlot <- function() {
	
	# eval(quote()) construction because this function is evaluated inside .writeImage()
	# which needs to look 2 levels up to find the objects network, options, layout, groups, legend, and shape.
	eval(quote({
		plot(bt, statistic = statistic, order = "sample")
	}), envir = parent.frame(2))
	
}

.networkAnalysisBootstrapPlot <- function(network, options, perform, statistic, oldPlot = NULL) {
	
	if (!is.null(oldPlot) && !identical(oldPlot[["collection"]][[1]][["data"]], ""))
		return(oldPlot)
	
	plot <- list(
		title = "Network Plot",
		collection = list(),
		status = "inited"
	)
	
	subPlotBase <- list(
		width = options[["plotWidthBootstrapPlot"]],
		height = options[["plotHeightBootstrapPlot"]],
		custom = list(width = "plotWidthBootstrapPlot", height = "plotHeightBootstrapPlot")
	)
	
	if (perform == "run") {
		
		allBootstraps <- network[["bootstrap"]]
		nGraphs <- length(allBootstraps)

		for (v in names(allBootstraps)) {
			
			subPlot <- subPlotBase
			subPlot[["title"]] <- v
			subPlot[["name"]] <- v
			
			bt <- allBootstraps[[v]]

			content <- .writeImage(width = subPlot[["width"]], height = subPlot[["height"]], 
								   plot = plot(bt, statistic = statistic, order = "sample") # returns a ggplot object
			)
			
			subPlot[["convertible"]] <- TRUE
			subPlot[["data"]] <- content[["png"]]
			subPlot[["obj"]] <- content[["obj"]]
			subPlot[["status"]] <- "complete"
			plot[["collection"]][[v]] <- subPlot
			
		}
		
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
.networkAnalysisAddReferencesToTables <- function(results, options) {

	# get from every .meta element the type and check if it is "table"
	idxOfTables <- sapply(results[[".meta"]], `[[`, "type") == "table"
	# get the names of .meta elements that have type "table".
	namesOfTables <- sapply(results[[".meta"]], `[[`, "name")[idxOfTables]
	for (nm in namesOfTables) { # use names to index
		if (!is.null(results[[nm]])) { # if table is not empty add reference
			results[[nm]][["citation"]] <- as.list(bootnet:::getRefs(options[["estimator"]]))
		}
	}

	return(results)

}

# avoids qgraph from opening windows
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

.networkAnalysisNetworkHasErrors <- function(network) {

	# returns TRUE if network has errors; FALSE otherwise

	allNetworks <- network[["network"]]
	if (any(sapply(allNetworks, function(x) anyNA(x[["graph"]]))))
		return("error")

	return("success")

}

# TODO: periodically check if this has been implemented in bootnet (Sacha said he'd do so).
.networkAnalysisBootnet_cor <- function (data, corMethod = c("cor_auto", "cov", "cor", "npn"), 
										 missing = c("pairwise", "listwise", "stop"), 
										 sampleSize = c("maximum", "minimim"), 
										 verbose = TRUE, corArgs = list(), threshold = 0) {
	corMethod <- match.arg(corMethod)
	missing <- match.arg(missing)
	sampleSize <- match.arg(sampleSize)
	if (identical(threshold, "none")) {
		threshold <- 0
	}
	if (verbose) {
		msg <- "Estimating Network. Using package::function:"
		msg <- paste0(msg, "\n  - qgraph::qgraph(..., graph = 'pcor') for network computation")
		if (corMethod == "cor_auto") {
			msg <- paste0(msg, "\n  - qgraph::cor_auto for correlation computation\n    - using lavaan::lavCor")
		}
		if (corMethod == "npn") {
			msg <- paste0(msg, "\n  - huge::huge.npn for nonparanormal transformation")
		}
		if (threshold != "none") {
			if (threshold != "locfdr") {
				msg <- paste0(msg, "\n  - psych::corr.p for significance thresholding")
			}
			else {
				msg <- paste0(msg, "\n  - fdrtool for false discovery rate")
			}
		}
		message(msg)
	}
	if (!(is.data.frame(data) || is.matrix(data))) {
		stop("'data' argument must be a data frame")
	}
	if (is.matrix(data)) {
		data <- as.data.frame(data)
	}
	N <- ncol(data)
	Np <- nrow(data)
	if (missing == "stop") {
		if (any(is.na(data))) {
			stop("Missing data detected and missing = 'stop'")
		}
	}
	if (corMethod == "npn") {
		data <- huge::huge.npn(data)
		corMethod <- "cor"
	}
	if (corMethod == "cor_auto") {
		args <- list(data = data, missing = missing, verbose = verbose)
		if (length(corArgs) > 0) {
			for (i in seq_along(corArgs)) {
				args[[names(corArgs)[[i]]]] <- corArgs[[i]]
			}
		}
		corMat <- do.call(qgraph::cor_auto, args)
	}
	else if (corMethod %in% c("cor", "cov")) {
		use <- switch(missing, pairwise = "pairwise.complete.obs", 
					  listwise = "complete.obs")
		args <- list(x = data, use = use)
		if (length(corArgs) > 0) {
			for (i in seq_along(corArgs)) {
				args[[names(corArgs)[[i]]]] <- corArgs[[i]]
			}
		}
		corMat <- do.call(corMethod, args)
	}
	else stop("Correlation method is not supported.")
	if (missing == "listwise") {
		sampleSize <- nrow(na.omit(data))
	}
	else {
		if (sampleSize == "maximum") {
			sampleSize <- sum(apply(data, 1, function(x) !all(is.na(x))))
		}
		else {
			sampleSize <- sum(apply(data, 1, function(x) !any(is.na(x))))
		}
	}
	Results <- getWmat(qgraph::qgraph(corMat, graph = "pcor", 
									  DoNotPlot = TRUE, threshold = threshold, sampleSize = sampleSize))
	return(list(graph = Results, results = Results))
}


# saveDataToDput <- function(...) {
#
# 	path <- "C:/Users/donvd/_Laptop/Werk/JASP/tempDput/"
# 	if (!dir.exists(path))
# 		dir.create(path)
# 	nms <- sapply(substitute(list(...))[-1], deparse)
# 	dots <- list(...)
#
# 	for (i in seq_along(dots)) {
# 		j <- 1
# 		fName <- paste0(path, nms[i], ".txt")
# 		while (file.exists(fName[i]) && j < 1e3) {
# 			fName <- paste0(path, nms[i], "_", j, ".txt")
# 			j <- j + 1
# 		}
# 		sink(file = fName)
# 		dput(dots[[i]])
# 		sink(NULL)
# 	}
#
# }

