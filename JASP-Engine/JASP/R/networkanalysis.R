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

	if (perform != "run")
		return(NULL)

	allBootnetArgsInOpts <- c("correlationMethod", "tuningParameter", "criterion", "isingEstimator",
							  "nFolds", "split", "rule", "missingValues",
							  "weightedNetwork", "signedNetwork", "sampleSize")

	names(options)

	data("bfi", package = "psych")
	dataset <- bfi[, 1:25]
	dataset <- .vdf(dataset, columns.as.ordinal = colnames(dataset))

	# fix input to match bootnet preferences.
	options[["rule"]] <- toupper(options[["rule"]])
	if (options[["correlationMethod"]]  == "auto")
		options[["correlationMethod"]] <- "cor_auto"

	# this does everything
	.input <- checkInput2(
		default    = options[["estimator"]],
		corMethod  = options[["correlationMethod"]],
		tuning     = options[["tuningParameter"]],
		missing    = options[["missingValues"]],
		method     = options[["isingEstimator"]],
		rule       = options[["rule"]],
		nFolds     = options[["nFolds"]],
		weighted   = options[["weightedNetwork"]],
		signed     = options[["signedNetwork"]],
		sampleSize = options[["sampleSize"]],
		split      = options[["split"]],
		criterion  = options[["criterion"]],
		verbose    = FALSE
	)

	msg <- capture.output(
		network <- bootnet::estimateNetwork(
			data = dataset,
			.input = .input
		)
		, type = "message"
	)

	# msg <- capture.output(
	# 	network <- bootnet::estimateNetwork(
	# 		data = dataset,
	# 		default = options[["estimator"]],
	# 		corMethod = options[["correlationMethod"]],
	# 		rule = options[["rule"]]
	# 	)
	# 	, type = "message"
	# )

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

	TBcolumns <- list(info = c("Number of nodes", "Number of non-zero edges", "Sparsity"))

	if (perform != "run") { # fill in with .

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
	eval(quote(
		qgraph::qgraph(input = network[["graph"]], layout = options[["layout"]], repulsion = options[["repulsion"]],
					   labels = .unv(network[["labels"]]))
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
			"centrality" = qgraph::centralityPlot(network[["graph"]], labels = .unv(network[["labels"]]), print = FALSE)
		)

		content <- .writeImage(width = plot[["width"]], height = plot[["height"]], plot = plotObjOrFun)

		plot[["convertible"]] <- TRUE
		plot[["data"]] <- content[["png"]]
		plot[["obj"]] <- content[["obj"]]
		plot[["status"]] <- "complete"

	}

	# should go into .writeImage
	grDevices::graphics.off()

	return(plot)

}

# identical to bootnet:::checkInput except that it removes any arguments not present in
# the network function. Perhaps ask Sacha if this can just be done in bootnet.
checkInput2 <- function (default = c("none", "EBICglasso", "pcor", "IsingFit",
	"IsingSampler", "huge", "adalasso", "mgm", "relimp"), fun,
	prepFun, prepArgs, estFun, estArgs, graphFun, graphArgs,
	intFun, intArgs, sampleSize, verbose = TRUE, construct = c("default",
		"function", "arguments"), .dots = list(), ...)
{
	if (default[[1]] == "glasso")
		default <- "EBICglasso"
	if (default[[1]] == "IsingSampler")
		default <- "IsingSampler"
	default <- match.arg(default)
	construct <- match.arg(construct)
	if (missing(fun)) {
		fun <- NULL
	}
	dots <- c(.dots, list(...))
	argNames <- character(0)
	if (!missing(prepFun)) {
		argNames <- c(argNames, "prepFun")
	}
	if (!missing(prepArgs)) {
		argNames <- c(argNames, "prepArgs")
	}
	if (!missing(estFun)) {
		argNames <- c(argNames, "estFun")
	}
	if (!missing(estArgs)) {
		argNames <- c(argNames, "estArgs")
	}
	if (!missing(graphFun)) {
		argNames <- c(argNames, "graphFun")
	}
	if (!missing(graphArgs)) {
		argNames <- c(argNames, "graphArgs")
	}
	if (!missing(intFun)) {
		argNames <- c(argNames, "intFun")
	}
	if (!missing(intArgs)) {
		argNames <- c(argNames, "intArgs")
	}
	if (length(dots) > 0 && construct == "arguments") {
		stop(paste0("Ambiguous argument specification. Old functonality is used (construct = 'arguments') in combination with new functionality arguments (implying construct = 'function'): ",
			paste0("'", names(dots), "'", collapse = "; "),
			". These arguments are NOT compatible!"))
	}
	if (construct == "arguments" & default == "relimp") {
		stop("default = 'relimp' not supported with old bootnet style (construct = 'arguments')")
	}
	if (length(argNames) > 0 && construct == "function") {
		stop(paste0("Ambiguous argument specification. New functonality is used (construct = 'function') in combination with old functionality arguments (implying construct = 'arguments'): ",
			paste0("'", argNames, "'", collapse = "; "), ". These arguments are NOT compatible!"))
	}
	if (length(argNames) > 0 & length(dots) > 0) {
		stop(paste0("Ambiguous argument specification. Both old functionality arguments are used, compatible with construct = 'arguments': ",
			paste0("'", argNames, "'", collapse = "; "), ", as well as new functionality arguments are used, compatible with construct = 'function': ",
			paste0("'", names(dots), "'", collapse = "; "),
			". These two types of arguments are NOT compatible!"))
	}
	if (construct == "default") {
		construct <- "function"
		if (default == "none" && is.null(fun)) {
			construct <- "arguments"
		}
		if (default != "none" && is.null(fun) && (!missing(prepFun) |
			!missing(prepArgs) | !missing(estFun) | !missing(estArgs))) {
			construct <- "arguments"
		}
	}
	if (default == "none" && construct == "arguments") {
		if (missing(prepFun) | missing(prepArgs) | missing(estFun) |
			missing(estArgs)) {
			stop("If 'default' is not set and 'fun' is missing, 'prepFun', 'prepArgs', 'estFun' and 'estArgs' may not be missing.")
		}
	}
	if (construct == "function") {
		Args <- dots
		if (!missing(prepFun)) {
			warning("'prepFun' argument is ignored as a function is used as arguments. To use 'prepFun', please set construct = 'arguments'")
		}
		if (!missing(prepArgs)) {
			warning("'prepArgs' argument is ignored as a function is used as arguments. To use 'prepArgs', please set construct = 'arguments'")
		}
		if (!missing(estFun)) {
			warning("'estFun' argument is ignored as a function is used as arguments. To use 'estFun', please set construct = 'arguments'")
		}
		if (!missing(estArgs)) {
			warning("'estArgs' argument is ignored as a function is used as arguments. To use 'estArgs', please set construct = 'arguments'")
		}
		if (!missing(graphFun)) {
			warning("'graphFun' argument is ignored as a function is used as arguments. To use 'graphFun', please set construct = 'arguments'")
		}
		if (!missing(graphArgs)) {
			warning("'graphArgs' argument is ignored as a function is used as arguments. To use 'graphArgs', please set construct = 'arguments'")
		}
		if (!missing(intFun)) {
			warning("'intFun' argument is ignored as a function is used as arguments. To use 'intFun', please set construct = 'arguments'")
		}
		if (!missing(intArgs)) {
			warning("'intArgs' argument is ignored as a function is used as arguments. To use 'intArgs', please set construct = 'arguments'")
		}
		if (default == "none") {
			Function <- fun
		}
		else if (default == "EBICglasso") {
			Function <- bootnet_EBICglasso
		}
		else if (default == "IsingFit") {
			Function <- bootnet_IsingFit
		}
		else if (default == "IsingSampler") {
			Function <- bootnet_IsingSampler
		}
		else if (default == "pcor") {
			Function <- bootnet_pcor
		}
		else if (default == "adalasso") {
			Function <- bootnet_adalasso
		}
		else if (default == "huge") {
			Function <- bootnet_huge
		}
		else if (default == "mgm") {
			Function <- bootnet_mgm
		}
		else if (default == "relimp") {
			Function <- bootnet_relimp
		}
		else stop("Currently not supported.")
	}
	else {
		warning("Arguments (prepFun, estFun, etcetera) used to construct estimator. This functionality is deprecated and will no longer be supported in a future version of bootnet. Please consult the manual or contact the authors.")
		if (length(dots) > 0) {
			dotNames <- names(dots)
			warning(paste0("Arguments (prepFun, estFun, etcetera) used to construct estimator. As a result, the following arguments are ignored: ",
				paste0("'", dotNames, "'", collapse = ", "),
				". To use these arguments use construct = 'function' and supply a default set or set the 'fun' argument. In addition, do not use the 'prepFun', 'estFun', etcetera arguments."))
		}
		if (!(default == "none")) {
			if (missing(prepFun)) {
				prepFun <- switch(default, EBICglasso = qgraph::cor_auto,
					IsingFit = binarize, IsingSampler = binarize,
					pcor = qgraph::cor_auto, huge = function(x) huge::huge.npn(na.omit(as.matrix(x)),
						verbose = FALSE), adalasso = identity)
			}
			if (missing(prepArgs)) {
				prepArgs <- switch(default, EBICglasso = ifElse(identical(prepFun,
					qgraph::cor_auto), list(verbose = verbose),
					ifElse(identical(prepFun, cor), list(use = "pairwise.complete.obs"),
						list())), IsingFit = list(), pcor = ifElse(identical(prepFun,
					qgraph::cor_auto), list(verbose = verbose),
					ifElse(identical(prepFun, cor), list(use = "pairwise.complete.obs"),
						list())), IsingSampler = list(), huge = list(),
					adalasso = list())
			}
			if (missing(estFun)) {
				estFun <- switch(default, EBICglasso = qgraph::EBICglasso,
					pcor = corpcor::cor2pcor, IsingFit = IsingFit::IsingFit,
					IsingSampler = IsingSampler::EstimateIsing,
					huge = function(x) huge::huge.select(huge::huge(x,
						method = "glasso", verbose = FALSE), criterion = "ebic",
						verbose = FALSE), adalasso = parcor::adalasso.net)
			}
			if (missing(estArgs)) {
				estArgs <- switch(default, EBICglasso = list(n = sampleSize,
					returnAllResults = TRUE), IsingFit = list(plot = FALSE,
					progress = FALSE), pcor = list(), IsingSampler = list(method = "ll"),
					huge = list(), adalasso = list())
			}
			if (missing(graphFun)) {
				graphFun <- switch(default, EBICglasso = function(x) x[["optnet"]],
					IsingFit = function(x) x[["weiadj"]], pcor = function(x) as.matrix(Matrix::forceSymmetric(x)),
					IsingSampler = function(x) x[["graph"]], huge = function(x) as.matrix(qgraph::wi2net(as.matrix(x$opt.icov))),
					adalasso = function(x) as.matrix(Matrix::forceSymmetric(x$pcor.adalasso)))
			}
			if (missing(graphArgs)) {
				graphArgs <- switch(default, EBICglasso = list(),
					IsingFit = list(), pcor = list(), IsingSampler = list(),
					huge = list(), adalasso = list())
			}
			if (missing(intFun)) {
				intFun <- switch(default, EBICglasso = null,
					IsingFit = function(x) x[["thresholds"]],
					pcor = null, IsingSampler = function(x) x[["thresholds"]],
					huge = null, adalasso = null)
			}
		}
		if (missing(prepFun)) {
			prepFun <- identity
		}
		if (missing(prepArgs)) {
			prepArgs <- list()
		}
		if (missing(graphFun)) {
			graphFun <- identity
		}
		if (missing(graphArgs)) {
			graphArgs <- list()
		}
		if (missing(intFun)) {
			intFun <- null
		}
		if (missing(intArgs)) {
			intArgs <- list()
		}
		Function <- bootnet_argEstimator
		Args <- list(prepFun = prepFun, prepArgs = prepArgs,
			estFun = estFun, estArgs = estArgs, graphFun = graphFun,
			graphArgs = graphArgs, intFun = intFun, intArgs = intArgs)
	}
	Output <- list(data = data, default = default, estimator = Function,
		arguments = Args)

	# remove unneeded arguments
	nms <- names(Output[["arguments"]])
	nmfs <- names(formals(Function))
	nms2remove <- nms[!(nms %in% nmfs)]
	Output[["arguments"]][nms2remove] <- NULL
	if (verbose && length(nms2remove) > 0)
		warning(sprintf("The following argument(s) are not used in this network type: %s", paste(nms2remove, collapse = ", ")))

	return(Output)
}
