#
# Copyright (C) 2018 University of Amsterdam
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

NetworkAnalysis <- function(jaspResults, dataset, options) {

  dataset <- .networkAnalysisReadData(dataset, options)

  mainContainer <- .networkAnalysisSetupMainContainerAndTable(jaspResults, dataset, options)
  .networkAnalysisErrorCheck(mainContainer, dataset, options)

  network <- .networkAnalysisRun(mainContainer, dataset, options)

  .networkAnalysisMainTable(mainContainer, dataset, options, network)

  .networkAnalysisCentralityTable  (mainContainer, network, options)
  .networkAnalysisClusteringTable  (mainContainer, network, options)
  .networkAnalysisPlotContainer    (mainContainer, network, options)
  .networkAnalysisWeightMatrixTable(mainContainer, network, options)

  # done last so that all other results are shown already
  .networkAnalysisBootstrap(mainContainer, network, options)

  return()
}

.networkAnalysisReadData <- function(dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  variables <- unlist(options[["variables"]])
  groupingVariable <- options[["groupingVariable"]]
  vars2read <- c(variables, groupingVariable)
  vars2read <- vars2read[vars2read != ""]
  exclude    <- c()
  if (options[["missingValues"]] == "listwise")
    exclude <- vars2read
  dataset <- .readDataSetToEnd(columns.as.numeric = vars2read, exclude.na.listwise = exclude)
  # dataset <- .readDataSetToEnd(columns = vars2read, exclude.na.listwise = exclude) # jasptools need this, JASP the line above

  if (options[["groupingVariable"]] == "") { # one network
    dataset <- list(dataset) # for compatability with the split behaviour
    names(dataset) <- "Network"
  } else { # multiple networks
    groupingVariableData <- dataset[[.v(options[["groupingVariable"]])]]
    dataset[[.v(options[["groupingVariable"]])]] <- NULL
    dataset <- split(dataset, groupingVariableData, drop = TRUE)
    attr(dataset, "groupingVariableData") <- groupingVariableData
  }
  return(dataset)

}

.networkAnalysisErrorCheck <- function(mainContainer, dataset, options) {

  # some analyses, such as Sacha's EBIGglasso with cor_auto, completely ignore the missing argument
  # and always use pairwise information even though their documentation says they can do listwise
  if (length(options[["variables"]]) < 3)
    return()

  # check for errors, but only if there was a change in the data (which implies state[["network"]] is NULL)
  if (is.null(mainContainer[["networkState"]])) {

    customChecks <- NULL

    # check if data must be binarized
    if (options[["estimator"]] %in% c("IsingFit", "IsingSampler")) {
      idx <- colnames(dataset) != .v(options[["groupingVariable"]])
      dataset[idx] <- bootnet::binarize(dataset[idx], split = options[["split"]], verbose = FALSE, removeNArows = FALSE)

      if (options[["estimator"]] == "IsingFit") {
        # required check since IsingFit removes these variables from the analyses
        customChecks <- c(customChecks,
                          function() {
                            tbs <- apply(dataset, 2, table)
                            if (any(tbs <= 1)) {
                              idx <- which(tbs <= 1, arr.ind = TRUE)
                              return(gettextf(
                                "After binarizing the data too little variance remained in variable(s): %s.",
                                paste0(.unv(colnames(tbs[, idx[, 2], drop = FALSE])), collapse = ", ")
                              ))
                            }
                            return(NULL)
                          }
        )
      }
    }


    # default error checks
    checks <- c("infinity", "variance", "observations", "varCovData")

    # hasErrors wants the dataset as a single thing instead of as a list of data.frames
    # we could also just ignore the grouping elements and loop manually though
    groupingVariable <- attr(dataset, "groupingVariable")
    dataset <- Reduce(rbind.data.frame, dataset)

    if (options[["groupingVariable"]] != "") {
      # these cannot be chained unfortunately
      groupingVariableName <- options[["groupingVariable"]]
      dfGroup <- data.frame(groupingVariable)
      colnames(dfGroup) <- .v(groupingVariableName)
      .hasErrors(dataset = dfGroup,
                 type = c("missingValues", "factorLevels"),
                 missingValues.target = groupingVariableName,
                 factorLevels.target = groupingVariableName,
                 factorLevels.amount = "< 2",
                 exitAnalysisIfErrors = TRUE)
      dataset[[.v(options[["groupingVariable"]])]] <- groupingVariable
      groupingVariable <- options[["groupingVariable"]]
    }

    # estimator 'mgm' requires some additional checks
    categoricalVars <- NULL
    if (options[["estimator"]] == "mgm" && "c" %in% options[["mgmVariableTypeData"]]) {
      categoricalVars <- options[["variables"]][options[["mgmVariableTypeData"]] == "c"]
      checks <- c(checks, "factorLevels")
    }

    # check for errors
    fun <- cor
    if (options[["correlationMethod"]] == "cov")
      fun <- cov

    .hasErrors(dataset = dataset,
               type = checks,
               variance.target = options[["variables"]], # otherwise the grouping variable always has variance == 0
               variance.grouping = groupingVariable,
               factorLevels.target = categoricalVars,
               factorLevels.amount = "> 10", # probably a misspecification of mgmVariableType when this happens.
               factorLevels.grouping = groupingVariable,
               observations.amount = " < 10",
               observations.grouping = groupingVariable,
               varCovData.grouping = groupingVariable,
               varCovData.corFun = fun,
               varCovData.corArgs = list(use = "pairwise"),
               custom = customChecks,
               exitAnalysisIfErrors = TRUE)
  }
}

# tables ----
.networkAnalysisSetupMainContainerAndTable <- function(jaspResults, dataset, options) {

  mainContainer <- jaspResults[["mainContainer"]]
  if (is.null(mainContainer)) {
    mainContainer <- createJaspContainer(dependencies = c(
      # data
      "variables", "groupingVariable", "mgmVariableType",
      # what kind of network is estimated
      "estimator",
      # arguments for the estimator
      "correlationMethod", "tuningParameter", "criterion", "isingEstimator",
      "nFolds", "split", "rule", "sampleSize", "thresholdBox", "thresholdString", "thresholdValue",
      # general arguments
      "weightedNetwork", "signedNetwork", "missingValues"
    ))
    mainContainer$addCitation(.networkAnalysisBootnetGetRefs(options[["estimator"]]))
    jaspResults[["mainContainer"]] <- mainContainer
  }
  .networkAnalysisMainTableMeta(mainContainer, dataset, options)

  return(mainContainer)
}

.networkAnalysisMainTableMeta <- function(mainContainer, dataset, options) {

  if (is.null(mainContainer[["generalTable"]])) {
    tb <- createJaspTable(gettext("Summary of Network"), position = 1, dependencies = c(
      # These are dependencies because specifying them incorrectly is communicated as footnotes on this table
      "computedLayoutX", "computedLayoutY", "bootstrapOnOff", "numberOfBootstraps", "minEdgeStrength"
    ))
    if (length(dataset) > 1L)
      tb$addColumnInfo(name = "info", title = gettext("Network"), type = "string")

    tb$addColumnInfo(name = "nodes",    title = gettext("Number of nodes"),          type = "integer")
    tb$addColumnInfo(name = "nonZero",  title = gettext("Number of non-zero edges"), type = "string")
    tb$addColumnInfo(name = "Sparsity", title = gettext("Sparsity"),                 type = "number")

    mainContainer[["generalTable"]] <- tb
  }
  return()
}

.networkAnalysisMainTable <- function(mainContainer, dataset, options, network) {

  if (is.null(network[["network"]]) || mainContainer$getError())
    return()

  tb <- mainContainer[["generalTable"]]
  nGraphs <- length(dataset)

  # footnotes
  if (options[["estimator"]] %in% c("IsingFit", "IsingSampler") && !all(unlist(dataset[!is.na(dataset)]) %in% 0:1))
    tb$addFootnote(gettextf("Data was binarized using %s. ",	options[["split"]]))

  if (!is.null(options[["colorNodesByData"]]) && length(options[["colorNodesByData"]]) != length(options[["variables"]])) {
    tb$addFootnote(
      gettextf("Only the first %d values of %s were used to color nodes (%d provided). ",
               length(options[["variables"]]),
               as.character(options[["colorNodesBy"]]),
               length(options[["colorNodesByData"]]))
    )
  }

  if (!is.null(options[["layoutMessage"]]))
    tb$addFootnote(options[["layoutMessage"]], symbol = gettext("<em>Warning: </em>"))

  if (!is.null(options[["colorNodesByDataMessage"]]))
    tb$addFootnote(options[["colorNodesByDataMessage"]], symbol = gettext("<em>Warning: </em>"))

  if (options[["minEdgeStrength"]] != 0) {

    ignored <- logical(nGraphs)
    for (i in seq_along(network[["network"]])) {
      ignored[i] <- all(abs(qgraph::getWmat(network[["network"]][[i]])) <= options[["minEdgeStrength"]])
    }

    if (any(ignored)) {
      if (nGraphs == 1L) {
        text <- gettext("Minimum edge strength ignored in the network plot because it was larger than the absolute value of the strongest edge.")
      } else {
        text <- gettextf("Minimum edge strength ignored in the network plot of group%1$s %2$s because it was larger than the absolute value of the strongest edge.",
                         ifelse(sum(ignored) == 2L, "s", ""),
                         paste0(names(network[["network"]])[ignored], collapse = ", ")
        )
      }
      tb$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
    }
  }
  if (xor(options[["bootstrapOnOff"]], options[["numberOfBootstraps"]] > 0L)) {
    text <- gettext("Bootstrapping is only done when 'Bootstrap network' is ticked and 'Number of Bootstraps' is larger than 0.")
    tb$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
  }

  # fill in with info from bootnet:::print.bootnet
  df <- data.frame(nodes = integer(nGraphs), nonZero = numeric(nGraphs), Sparsity = numeric(nGraphs))
  if (nGraphs > 1L)
    df[["info"]] <- names(network[["network"]])

  nVar <- ncol(network[["network"]][[1L]][["graph"]])
  for (i in seq_len(nGraphs)) {

    nw <- network[["network"]][[i]]
    df[["nodes"]][i]    <- nrow(nw[["graph"]])
    df[["nonZero"]][i]  <- paste(sum(nw[["graph"]][upper.tri(nw[["graph"]], diag = FALSE)] != 0), "/", nVar * (nVar-1L) %/% 2)
    df[["Sparsity"]][i] <- mean(nw[["graph"]][upper.tri(nw[["graph"]], diag = FALSE)] == 0)

  }
  tb$setData(df)

}

.networkAnalysisCentralityTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["centralityTable"]]) || !options[["tableCentrality"]])
    return()

  nGraphs <- max(1L, length(network[["network"]]))
  table <- createJaspTable(gettext("Centrality measures per variable"), position = 2,
                           dependencies = c("tableCentrality", "normalizeCentrality", "maxEdgeStrength", "minEdgeStrength"))
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  # shared titles
  overTitles <- names(network[["network"]])
  if (is.null(overTitles))
    overTitles <- gettext("Network") # paste0("Network", 1:nGraphs)

  nameCol3 <- if ("Degree" %in% colnames(network[["centrality"]][[1]])[3]) "Degree" else "Strength"
  for (i in seq_len(nGraphs)) { # three centrality columns per network
    table$addColumnInfo(name = paste0("Betweenness", i),        title = gettext("Betweenness"),        type = "number", overtitle = overTitles[i])
    table$addColumnInfo(name = paste0("Closeness", i),          title = gettext("Closeness"),          type = "number", overtitle = overTitles[i])
    table$addColumnInfo(name = paste0(nameCol3, i),             title = gettext("Strength"),           type = "number", overtitle = overTitles[i])
    table$addColumnInfo(name = paste0("Expected influence", i), title = gettext("Expected influence"), type = "number", overtitle = overTitles[i])
  }

  mainContainer[["centralityTable"]] <- table
  if (is.null(network[["centrality"]]) || mainContainer$getError())
    return()

  # fill with results
  TBcolumns <- NULL
  for (i in seq_len(nGraphs)) {

    toAdd <- network[["centrality"]][[i]]
    names(toAdd) <- c("Variable", paste0(c("Betweenness", "Closeness", "Strength", "Expected influence"), i))
    if (i == 1L) {# if more than 1 network drop the first column which indicates the variable
      TBcolumns <- toAdd
    } else {
      toAdd <- toAdd[, -1L]
      TBcolumns <- cbind(TBcolumns, toAdd)
    }
  }
  table$setData(TBcolumns)
}

.networkAnalysisClusteringTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["clusteringTable"]]) || !options[["tableClustering"]])
    return()

  nGraphs <- max(1L, length(network[["network"]]))
  if (is.null(network[["clustering"]])) {
    measureNms <- list(c(gettext("Barrat"), gettext("Onnela"), gettext("WS"), gettext("Zhang")))
  } else { # TODO: this is probably for backwards compatability... check if this can be removed!
    measureNms <- NULL
    for (i in seq_len(nGraphs))
      measureNms[[i]] <- colnames(network[["clustering"]][[i]])[-1L]

  }
  nMeasures <- lengths(measureNms)

  table <- createJaspTable(gettext("Clustering measures per variable"), position = 3, dependencies = "tableClustering")
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  # shared titles
  overTitles <- names(network[["network"]])
  if (is.null(overTitles))
    overTitles <- gettext("Network") # paste0("Network", 1:nGraphs)

  for (i in seq_len(nGraphs))
    for (j in seq_len(nMeasures[i])) # four clustering columns per network
      table$addColumnInfo(name = paste0(measureNms[[i]][j], i), title = measureNms[[i]][j], type = "number", overtitle = overTitles[i])

  mainContainer[["clusteringTable"]] <- table
  if (is.null(network[["clustering"]]) || mainContainer$getError())
    return()

  # fill with results
  TBcolumns <- NULL
  footnotes <- attr(network[["clustering"]], "footnotes")
  for (i in seq_len(nGraphs)) {

    toAdd <- network[["clustering"]][[i]]
    names(toAdd) <- c("Variable", paste0(measureNms[[i]], i))
    if (i == 1L) {# if more than 1 network drop additional variable column
      TBcolumns <- toAdd
    } else {
      toAdd <- toAdd[, -1L]
      TBcolumns <- cbind(TBcolumns, toAdd)
    }
  }

  table$setData(TBcolumns)
  if (!is.null(footnotes)) {
    # ugly, but jaspResults doesn't suupport any other way
    colNames <- NULL
    for (j in footnotes[["column"]])
      colNames <- c(colNames, table$getColumnName(j))
    table$addFootnote(message = footnotes[["message"]], colNames = colNames)
  }
  if (any(nMeasures != 4L)) {
    nms <- names(network[["network"]])[nMeasures != 4L]
    s <- if (length(nms) == 1L) "" else "s"
    text <- gettextf("Clustering measures could not be computed for network%1$s: %2$s",
                     s, paste0(nms, collapse = ", "))
    table$addFootnote(text, symbol = gettext("<em>Warning: </em>"))
  }
}

.networkAnalysisWeightMatrixTable <- function(mainContainer, network, options) {

  if (!is.null(mainContainer[["weightsTable"]]) || ! options[["tableWeightsMatrix"]])
    return()

  variables <- unlist(options[["variables"]])
  nVar <- length(variables)
  nGraphs <- max(1L, length(network[["network"]]))

  table <- createJaspTable(gettext("Weights matrix"), dependencies = "tableWeightsMatrix", position = 4)
  table$addColumnInfo(name = "Variable", title = gettext("Variable"), type = "string")

  overTitles <- names(network[["network"]])
  if (is.null(overTitles))
    overTitles <- gettext("Network")

  for (i in seq_len(nGraphs))
    for (v in seq_len(nVar))
      table$addColumnInfo(name = paste0(variables[v], i), title = variables[v], type = "number", overtitle = overTitles[i])

  if (length(options[["variables"]]) <= 2L || is.null(network[["network"]]) || mainContainer$getError()) { # make empty table
    if (nVar > 0L) { # otherwise, a 1 by 1 table with a . is generated by default
      # create a table of nVariables by nVariables
      table$setExpectedSize(nVar)
      table[["Variable"]] <- variables

    }
  } else { # fill with results

    allNetworks <- network[["network"]]
    TBcolumns <- data.frame(Variable = variables)
    for (i in seq_len(nGraphs)) {

      toAdd <- allNetworks[[i]][["graph"]]
      if (!options[["weightedNetwork"]])
        toAdd <- sign(toAdd)
      if (!options[["signedNetwork"]])
        toAdd <- abs(toAdd)

      toAdd <- as.data.frame(toAdd)
      names(toAdd) <- paste0(variables, i)

      TBcolumns <- cbind(TBcolumns, toAdd)
    }
    table$setData(TBcolumns)
  }
  mainContainer[["weightsTable"]] <- table
}

.networkAnalysisBootstrapTable <- function(bootstrapContainer, options, position) {

  # a table to show before bootstrapping to give some feedback to the user.
  # this used to give an ETA but with the progress bars that isn't really necessary anymore.
  if (!(length(options[["variables"]]) > 2L && options[["bootstrapOnOff"]] && options[["numberOfBootstraps"]] > 0L))
    return()

  bootstrapType <- options[["BootstrapType"]]
  substr(bootstrapType, 1, 1) <- toupper(substr(bootstrapType, 1, 1)) # capitalize first letter

  table <- createJaspTable(title = gettext("Bootstrap summary of Network"), position = position)
  table$addColumnInfo(name = "type",               title = gettext("Type"), type = "string")
  table$addColumnInfo(name = "numberOfBootstraps", title = gettext("Number of bootstraps"), type = "integer")
  table[["type"]]               <- bootstrapType
  table[["numberOfBootstraps"]] <- options[["numberOfBootstraps"]]
  bootstrapContainer[["table"]] <- table

}

# plots ----
.networkAnalysisPlotContainer <- function(mainContainer, network, options) {

  plotContainer <- mainContainer[["plotContainer"]]
  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(position = 5, dependencies = c("abbreviateLabels", "abbreviateNoChars",
                                                                        "showLegend", "showVariableNames"))
    mainContainer[["plotContainer"]] <- plotContainer
  }

  .networkAnalysisNetworkPlot   (plotContainer, network, options)
  .networkAnalysisCentralityPlot(plotContainer, network, options)
  .networkAnalysisClusteringPlot(plotContainer, network, options)
}

.networkAnalysisCentralityPlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["centralityPlot"]]) || !options[["plotCentrality"]])
    return()

  measuresToShow <- unlist(options[c("Betweenness", "Closeness", "Degree", "ExpectedInfluence")], use.names = FALSE)
  hasMeasures <- any(measuresToShow)

  width <- if (hasMeasures) 120 * sum(measuresToShow) else 120
  plot <- createJaspPlot(title = gettext("Centrality Plot"), position = 52, width = width,
                         dependencies = c("plotCentrality", "Betweenness", "Closeness", "Degree", "ExpectedInfluence"))
  plotContainer[["centralityPlot"]] <- plot
  if (is.null(network[["centrality"]]) || plotContainer$getError() || !hasMeasures)
    return()

  wide <- network[["centrality"]]

  Long <- .networkAnalysisReshapeWideToLong(wide, network, "centrality")

  if (!all(measuresToShow)) {
    measuresToFilter <- c("Betweenness", "Closeness", "Degree", "Expected Influence")[measuresToShow]
    Long <- subset(Long, measure %in% measuresToFilter)
  }

  .networkAnalysisMakePlotFromLong(plot, Long, options)

}

.networkAnalysisClusteringPlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["clusteringPlot"]]) || !options[["plotClustering"]])
    return()

  plot <- createJaspPlot(title = gettext("Clustering Plot"), position = 53, dependencies = "plotClustering", width = 480)
  plotContainer[["clusteringPlot"]] <- plot
  if (is.null(network[["clustering"]]) || plotContainer$getError())
    return()

  wide <- network[["clustering"]]

  len <- lengths(wide)
  idx <- which.max(len)
  if (!all(len == len[idx])) {

    cnms <- colnames(wide[[idx]])[-1]
    for (i in which(len != len[idx])) {

      cnmsToAdd <- cnms[!(cnms %in% colnames(wide[[i]]))]
      for (nms in cnmsToAdd)
        wide[[i]][[nms]] <- NA
      wide[[i]] <- wide[[i]][colnames(wide[[idx]])]

    }
  }

  Long <- .networkAnalysisReshapeWideToLong(wide, network, "clustering")
  .networkAnalysisMakePlotFromLong(plot, Long, options)

}

.networkAnalysisReshapeWideToLong <- function(wide, network, what = c("centrality", "clustering")) {

  what <- match.arg(what)
  wideDf <- Reduce(rbind, wide)
  if (length(wide) > 1L) {
    wideDf[["type"]] <- rep(names(network[[what]]), each = nrow(wideDf) / length(wide))
    Long <- reshape2::melt(wideDf, id.vars = c("node", "type"))
    colnames(Long)[3L] <- "measure"
    Long[["graph"]] <- Long[["type"]]
    Long[["type"]] <- TRUE # options[["separateCentrality"]]
  } else {
    Long <- reshape2::melt(wideDf, id.vars = "node")
    colnames(Long)[2L] <- "measure"
    Long[["graph"]] <- NA
  }
  return(Long)

}

.networkAnalysisMakePlotFromLong <- function(jaspPlot, Long, options) {

  # "Long" is how qgraph refers to this object. This function transforms the
  # long object for centrality or clustering into a ggplot.

  # code modified from qgraph::centralityPlot(). Type and graph are switched so the legend title says graph
  if (options[["abbreviateLabels"]])
    Long[["node"]] <- base::abbreviate(Long[["node"]], options[["abbreviateNoChars"]])

  # code modified from qgraph::centralityPlot(). Type and graph are switched so the legend title says graph
  Long <- Long[gtools::mixedorder(Long$node), ]
  Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))

  Long$nodeLabel <- NA
  if (options[["showVariableNames"]] == "In legend") {
    Long$nodeLabel <- as.character(Long$node)
    Long$node <- factor(match(as.character(Long$node), unique(as.character(Long$node))))
    levels(Long$node) <- rev(levels(Long$node))
    Long$nodeLabel <- paste(as.character(Long$node), "=", Long$nodeLabel)
  }

  if (length(unique(Long$graph)) > 1L) {
    mapping <- ggplot2::aes(x = value, y = node, group = graph, colour = graph)
    guide   <- ggplot2::guides(color = ggplot2::guide_legend(title = options[["groupingVariable"]])) # change the name graph into the variable name for splitting
  } else {
    mapping <- ggplot2::aes(x = value, y = node, group = graph)
    guide   <- NULL
  }

  # add a fill element to the mapping -- this is only used to add a legend for the names of the nodes.
  hasNodeLabels <- !all(is.na(Long[["nodeLabel"]]))
  if (hasNodeLabels)
    mapping$fill <- as.name("nodeLabel")

  g <- ggplot2::ggplot(Long, mapping) + guide

  g <- g + ggplot2::geom_path() + ggplot2::geom_point() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL)

  if (length(unique(Long$type)) > 1) {
    g <- g + ggplot2::facet_grid(type ~ measure, scales = "free")

  } else {
    g <- g + ggplot2::facet_grid(~measure, scales = "free")
  }
  g <- g + ggplot2::theme_bw()

  if (options[["showLegend"]] == "No legend")
    g <- g + ggplot2::theme(legend.position = "none")
  else if (hasNodeLabels) {
    # the fill aestethic introduces a set of points left of `1 = contNormal`.
    # the statement below sets the size of those points to 0, effectively making them invisible
    # keywidth removes the invisible space introduced so that the legends nicely line up (if there are multiple)
    g <- g + ggplot2::guides(fill = ggplot2::guide_legend(keywidth = 0, override.aes = list(size = 0, alpha = 0)))
  }

  jaspPlot$plotObject <- g


}


.networkAnalysisOneNetworkPlot <- function(network, options, minE, layout, groups, maxE, labels, legend, shape,
                                           nodeColor, edgeColor, nodeNames) {

  wMat <- network[["graph"]]
  if (!options[["weightedNetwork"]]) {
    wMat <- sign(wMat)
  }
  if (!options[["signedNetwork"]]) {
    wMat <- abs(wMat)
  }
  if (all(abs(wMat) <= minE))
    minE <- NULL

  return(
    qgraph::qgraph(
      input               = wMat,
      layout              = layout,
      groups              = groups,
      repulsion           = options[["repulsion"]],
      cut                 = options[["cut"]],
      edge.width          = options[["edgeSize"]],
      node.width          = options[["nodeSize"]],
      maximum             = maxE,
      minimum             = minE,
      details             = options[["showDetails"]],
      labels              = labels,
      palette             = if (options[["manualColors"]]) NULL else options[["nodePalette"]],
      theme               = options[["edgeColors"]],
      legend              = legend,
      shape               = shape,
      color               = nodeColor,
      edge.color          = edgeColor,
      nodeNames           = nodeNames,
      label.scale         = options[["scaleLabels"]],
      label.cex           = options[["labelSize"]],
      GLratio             = 1 / options[["legendToPlotRatio"]],
      edge.labels         = options[["edgeLabels"]],
      edge.label.cex      = options[["edgeLabelCex"]],
      edge.label.position = options[["edgeLabelPosition"]]
    ))
}

.networkAnalysisNetworkPlot <- function(plotContainer, network, options) {

  if (!is.null(plotContainer[["networkPlotContainer"]]) || !options[["plotNetwork"]])
    return()

  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)

  # we use an empty container without a name if there is only 1 graph. This container is hidden from the output but it
  # enables us to use the same code for a single network plot and for a collection of network plots.
  title <- if (nGraphs == 1L) "" else gettext("Network Plots")

  networkPlotContainer <- createJaspContainer(title = title, position = 51, dependencies = c(
    "layout", "edgeColors", "repulsion", "edgeSize", "nodeSize", "colorNodesBy",
    "maxEdgeStrength", "minEdgeStrength", "cut", "showDetails", "nodePalette",
    "legendNumber", "showMgmVariableType",
    "scaleLabels", "labelSize", "abbreviateLabels", "abbreviateNoChars",
    "keepLayoutTheSame", "layoutX", "layoutY", "plotNetwork",
    "groupNames", "groupColors", "variablesForColor", "groupAssigned", "manualColors",
    "legendToPlotRatio", "edgeLabels", "edgeLabelCex", "edgeLabelPosition"
  ))
  plotContainer[["networkPlotContainer"]] <- networkPlotContainer

  if (is.null(network[["network"]]) || plotContainer$getError()) {
    networkPlotContainer[["dummyPlot"]] <- createJaspPlot(title = gettext("Network Plot"))
    return()
  }

  layout <- network[["layout"]] # calculated in .networkAnalysisRun()

  # ensure minimum/ maximum makes sense or ignore these parameters.
  # TODO: message in general table if they have been reset.
  minE <- options[["minEdgeStrength"]]
  maxE <- options[["maxEdgeStrength"]]

  if (minE == 0)
    minE <- NULL
  if (maxE == 0 || (!is.null(minE) && maxE <= minE))
    maxE <- NULL

  groups <- NULL
  nodeColor <- NULL
  allLegends <- rep(FALSE, nGraphs) # no legends

  if (length(options[["variablesForColor"]]) > 1L) {
    variablesForColor <- matrix(unlist(options[["variablesForColor"]]), ncol = 2L, byrow = TRUE)
    if (length(unique(variablesForColor[, 1L])) > 1L) {
      # user has defined groups and there are variables in the groups
      groupNames <- matrix(unlist(options[["groupNames"]]), ncol = 2L, byrow = TRUE)
      nGroups <- nrow(groupNames)

      idx <- match(variablesForColor[, 1L], groupNames[, 1L])

      groups <- vector("list", nGroups)
      names(groups) <- groupNames[, 1L]
      for (i in seq_len(nGroups))
        groups[[i]] <- which(idx == i)

      nonEmpty <- lengths(groups) > 0L
      groups <- groups[nonEmpty]

      if (options[["manualColors"]])
        nodeColor <- groupNames[nonEmpty, 2L]
    }
  }

  # defaults
  shape <- "circle"
  edgeColor <- NULL
  if (options[["estimator"]] == "mgm") {

    idx <- integer(length(options[["variables"]]))
    nms <- c("mgmVariableTypeContinuous", "mgmVariableTypeCategorical", "mgmVariableTypeCount")
    for (i in seq_along(nms))
      idx[options[["variables"]] %in% options[[nms[[i]]]]] <- i
    # idx[i] is 1 if variable[i] %in% mgmVariableTypeContinuous, 2 if in mgmVariableTypeCategorical, etc.

    ll <- lengths(options[c("mgmVariableTypeContinuous", "mgmVariableTypeCategorical", "mgmVariableTypeCount")])

    if (options[["showMgmVariableType"]] == "mgmNodeShape") {
      #         gaussian, categorical, poisson
      opts <- c("circle", "square",    "triangle")
      shape <- opts[idx]

    } else if (options[["showMgmVariableType"]] == "mgmNodeColor") {
      #         gaussian,  categorical, poisson
      opts <- c("#00a9e6", "#fb8b00",   "#00ba63")
      nodeColor <- opts[idx]

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

  if (options[["abbreviateLabels"]])
    labels <- base::abbreviate(labels, options[["abbreviateNoChars"]])

  # do we need to draw legends?
  if (!is.null(groups) || !is.null(nodeNames)) {
    if (options[["showLegend"]] ==  "All plots") {

      allLegends <- rep(TRUE, nGraphs)

    } else if (options[["showLegend"]] ==  "In plot number: ") {

      if (options[["legendNumber"]] > nGraphs) {

        allLegends[nGraphs] <- TRUE

      } else if (options[["legendNumber"]] < 1L) {

        allLegends[1L] <- TRUE

      } else {

        allLegends[options[["legendNumber"]]] <- TRUE

      }
    }
  }

  names(allLegends) <- names(allNetworks) # allows indexing by name

  basePlotSize <- 320
  legendMultiplier <- options[["legendToPlotRatio"]] * basePlotSize
  height <- setNames(rep(basePlotSize, nGraphs), names(allLegends))
  width  <- basePlotSize + allLegends * legendMultiplier
  for (v in names(allNetworks))
    networkPlotContainer[[v]] <- createJaspPlot(title = v, width = width[v], height = height[v])

  JASP:::.suppressGrDevice({

    for (v in names(allNetworks)) {

      networkToPlot <- allNetworks[[v]]
      if (options[["estimator"]] == "mgm") {
        edgeColor <- networkToPlot[["results"]][["edgecolor"]]
        if (is.null(edgeColor)) # compatability issues
          edgeColor <- networkToPlot[["results"]][["pairwise"]][["edgecolor"]]
      }

      legend <- allLegends[[v]]
      networkPlotContainer[[v]]$plotObject <- .networkAnalysisOneNetworkPlot(
        network    = networkToPlot,
        options    = options,
        minE       = minE,
        maxE       = maxE,
        layout     = layout,
        groups     = groups,
        labels     = labels,
        legend     = legend,
        shape      = shape,
        nodeColor  = nodeColor,
        edgeColor  = edgeColor,
        nodeNames  = nodeNames
      )
    }
  })
}

.networkAnalysisBootstrapPlot <- function(bootstrapContainer, bootstrapResults, options, statistic = "edge", position) {

  isEdge <- "edge" %in% statistic
  if (isEdge) {
    title <- gettext("Edge Stability")
    name  <- "EdgeStabilityPlots"
    opt   <- "StatisticsEdges"
  } else {
    title <- gettext("Centrality Stability")
    name  <- "StatisticsCentralityPlots"
    opt   <- "StatisticsCentrality"
  }

  if (!options[[opt]] || !is.null(bootstrapContainer[[name]]))
    return()

  plotContainer <- createJaspContainer(title = title, position = position, dependencies = opt)

  for (v in names(bootstrapResults))
    plotContainer[[v]] <- createJaspPlot(title = v)

  bootstrapContainer[[name]] <- plotContainer

  for (v in names(bootstrapResults)) {

    bt <- bootstrapResults[[v]]
    p <- try(JASP:::.suppressGrDevice(plot(bt, statistic = statistic, order = "sample")))

    # sometimes bootnet catches an error and returns a faulty ggplot object.
    # here we ensure that if there was any error, e contains that error.
    e <- p
    if (!isTryError(p))
      e <- try(JASP:::.suppressGrDevice(print(p)))

    if (isTryError(e)) {
      plotContainer[[v]]$setError(gettextf("bootnet crashed with the following error message:\n%s", .extractErrorMessage(e)))
    } else {
      plotContainer[[v]]$plotObject <- p
    }
  }
}


# single network functions ----
.networkAnalysisRun <- function(mainContainer, dataset, options) {

  # list that contains state or is empty
  networkList <- list(
    network    = mainContainer[["networkState"]]$object,
    centrality = mainContainer[["centralityState"]]$object,
    clustering = mainContainer[["clusteringState"]]$object,
    layout     = mainContainer[["layoutState"]]$object
  )

  if (length(options[["variables"]]) <= 2L)
    return(networkList)

  if (is.null(networkList[["network"]]))
    tryCatch(
      networkList[["network"]] <- .networkAnalysisComputeNetworks(options, dataset),
      mgmError = function(e) {
        mainContainer[["generalTable"]]$addFootnote(e[["message"]])
      },
      error = function(e) {
        mainContainer$setError(.networkAnalysisCheckKnownErrors(e))
      }
    )

  if (!mainContainer$getError() && !is.null(networkList[["network"]])) {
    if (is.null(networkList[["centrality"]]))
      networkList[["centrality"]] <- .networkAnalysisComputeCentrality(networkList[["network"]], options[["normalizeCentrality"]], options[["weightedNetwork"]], options[["signedNetwork"]])

    if (is.null(networkList[["clustering"]]))
      networkList[["clustering"]] <- .networkAnalysisComputeClustering(networkList[["network"]])

    if (is.null(networkList[["layout"]]))
      networkList[["layout"]] <- .networkAnalysisComputeLayout(networkList[["network"]], dataset, options)

    names(networkList[["network"]]) <- names(networkList[["centrality"]]) <- names(networkList[["clustering"]]) <-
      names(dataset)

    networkList[["status"]] <- .networkAnalysisNetworkHasErrors(networkList[["network"]])

    mainContainer[["networkState"]]    <- createJaspState(networkList[["network"]])
    mainContainer[["centralityState"]] <- createJaspState(networkList[["centrality"]], dependencies = c("normalizeCentrality", "maxEdgeStrength", "minEdgeStrength"))
    mainContainer[["clusteringState"]] <- createJaspState(networkList[["clustering"]])
    mainContainer[["layoutState"]]     <- createJaspState(networkList[["layout"]], dependencies = c("layout", "repulsion", "layoutX", "layoutY"))

    .networkAnalysisSaveLayout(mainContainer, options, networkList[["layout"]])

  }

  return(networkList)

}

.networkAnalysisMakeBootnetArgs <- function(dataset, options) {

  errorMessage <- NULL
  variables <- unlist(options[["variables"]])
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
    type <- character(nvar)

    tempMat <- matrix(ncol = 2, byrow = TRUE, c(
      "mgmVariableTypeContinuous",  "g",
      "mgmVariableTypeCategorical", "c",
      "mgmVariableTypeCount",       "p"
    ))
    for (i in seq_len(nrow(tempMat))) {
      lookup <- options[[tempMat[i, 1L]]]
      if (length(lookup) > 0L) {
        idx <- match(lookup, variables)
        type[idx] <- tempMat[i, 2L]
      }
    }

    if (any(type == "")) {
      message <- gettext("Please drag all variables to a particular type under \"Analysis options\".")
      e <- structure(class = c("mgmError", "error", "condition"), list(message = message, call = sys.call(-1)))
      stop(e)
    }

    # find out the levels of each categorical variable
    for (i in which(type == "c"))
      level[i] <- max(1L, nlevels(dataset[[1L]][[i]]), length(unique(dataset[[1L]][[i]])))


    # if (is.null(options[["mgmVariableTypeData"]])) {
    #   type <- rep("g", nvar)
    # } else {
    #   type <- options[["mgmVariableTypeData"]]
    #   invalidType <- is.na(type) | !(type %in% c("g", "c", "p"))
    #   type[invalidType] <- "g" # set missing to gaussian. TODO: add to table message.
    #
    #   if (any(invalidType))
    #     networkList[["message"]] <- c(networkList[["message"]],
    #                                   sprintf("The variable types supplied in %s contain missing values or values that do not start with 'g', 'c', or 'p'. These have been reset to gaussian ('g'). They were indices %s.",
    #                                           options[["mgmVariableType"]], paste(which(invalidType), sep = ", ")))
    #
    #   # find out the levels of each categorical variable
    #   for (i in which(type == "c"))
    #     level[i] <- max(1, nlevels(dataset[[1]][[i]]), length(unique(dataset[[1]][[i]])))
    #
    # }
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
  return(.dots)
}

.networkAnalysisComputeNetworks <- function(options, dataset) {

  if (!isNamespaceLoaded("bootnet"))
    try(loadNamespace("bootnet"), silent = TRUE)

  # setup all bootnet arguments, then loop over datasets to estimate networks
  .dots <- .networkAnalysisMakeBootnetArgs(dataset, options)

  networks <- vector("list", length(dataset))
  # for every dataset do the analysis
  for (nw in seq_along(dataset)) {

    JASP:::.suppressGrDevice(
      msg <- capture.output(
        network <- bootnet::estimateNetwork(
          data    = dataset[[nw]],
          default = options[["estimator"]],
          .dots   = .dots
        )
        , type = "message"
      )
    )

    network[["corMessage"]] <- msg
    networks[[nw]] <- network

  }
  return(networks)
}

.networkAnalysisComputeCentrality <- function(networks, normalizeCentrality, weightedNetwork, signedNetwork) {
  centralities <- vector("list", length(networks))
  for (nw in seq_along(networks)) {

    network <- networks[[nw]]
    cent <- qgraph::centrality(network[["graph"]], weighted = weightedNetwork, signed = signedNetwork, all.shortest.paths = FALSE)

    # note: centrality table is (partially) calculated here so that centralityTable and centralityPlot don't compute the same twice.
    TBcent <- as.data.frame(cent[c("Betweenness", "Closeness", "InDegree", "OutDegree", "InExpectedInfluence", "OutExpectedInfluence")])

    # adapted from qgraph::centrality_auto
    wmat <- qgraph::getWmat(network$graph)
    if (weightedNetwork)
      wmat <- sign(wmat)

    if (signedNetwork)
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
      # remove OutDegree and OutExpectedInfluence, since the network is undirected these are equal
      TBcent <- TBcent[-c(3, 5)]
      colnames(TBcent)[4L] <- "Expected Influence"

      if (weightedGraph) {

        colnames(TBcent)[3] <- "Strength"

      } else { # unweighted

        colnames(TBcent)[3] <- "Degree"

      }

    }

    # centVars <- apply(TBcent, 2, var)
    if (normalizeCentrality == "normalized") { # normalize only if variances are nonzero

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

    } else if (normalizeCentrality == "relative") {

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

    centralities[[nw]] <- cent

  }
  return(centralities)
}

.networkAnalysisComputeClustering <- function(networks) {
  clusterings <- vector("list", length(networks))
  footnotes <- NULL
  for (nw in seq_along(networks)) {

    network <- networks[[nw]]
    clust <- qgraph::clusteringTable(network, labels = .unv(network[["labels"]]), standardized = FALSE)
    clust <- reshape2::dcast(clust, graph + node + type ~ measure, value.var = "value")
    TBclust <- as.data.frame(clust[-c(1, 3)])
    TBclust <- TBclust[c(1L, order(colnames(TBclust)[-1L]) + 1L)] # alphabetical order

    # manually standardize the data. This allows for some extra checks on the standard deviation,
    # which can be very very small. If that happens, the data are pretty shitty or perhaps this clustering
    # statistic is just inappropriate for the particular data set. Either way, it makes the unit tests
    # fail because typically the small differences are just machine precision (and so is the sd).
    for (i in 2:ncol(TBclust)) {
      TBclust[, i] <- TBclust[, i] - mean(TBclust[, i])
      sdx <- sd(TBclust[, i])

      # perform some checks on the standard deviationbasically, the data are pretty shitty if this happens, or perhaps this clustering
      # statistic is just inappropriate for the particular data set.
      if (!is.na(sdx) && sdx > sqrt(.Machine$double.eps))
        TBclust[, i] <- TBclust[, i] / sdx
      else if (is.null(footnotes))
        footnotes <- list(column = i, message = gettext("Coefficient could not be standardized because the variance is too small."))
      else
        footnotes$column <- c(footnotes$column, i)

      # this can only be true if the variance check failed. We set this to exactly zero because otherwise
      # the "smart" ggplot2 axis determination will create different axes on different platforms causing unit tests to fail
      if (all(TBclust[, i] < sqrt(.Machine$double.eps)))
        TBclust[, i] <- 0

    }
    clusterings[[nw]] <- TBclust

  }
  if (!is.null(footnotes))
    attr(clusterings, "footnotes") <- footnotes

  return(clusterings)
}

.networkAnalysisComputeLayout <- function(networks, dataset, options) {

  layout <- options[["layout"]]
  userLayout <- .networkAnalysisComputeUserLayout(dataset, options)
  if (layout != "data" || userLayout[["layoutInvalid"]]) {
    if (layout == "data")
      layout <- "circle"

    JASP:::.suppressGrDevice(layout <- qgraph::averageLayout(networks, layout = layout, repulsion = options[["repulsion"]]))
    rownames(layout) <- .unv(colnames(networks[[1L]]))

  } else {
    layout <- userLayout[["layoutData"]]
    nms <- .unv(networks[[1L]][["labels"]])
    idx <- match(nms, rownames(layout))
    layout <- layout[idx, ]
  }
  return(layout)
}

.networkAnalysisComputeUserLayout <- function(dataset, options) {

  layoutInfo <- list()
  variables <- unlist(options[["variables"]])
  # it turns out that we must save the layout in the A1 = ... style.
  if (options[["layoutX"]] != "" && options[["layoutY"]] != "") {

    layoutXData <- .readDataSetToEnd(columns = options[["layoutX"]], exclude.na.listwise = options[["layoutX"]])[[1L]]
    layoutYData <- .readDataSetToEnd(columns = options[["layoutY"]], exclude.na.listwise = options[["layoutY"]])[[1L]]
    xyCoords <- .networkAnalysisSanitizeLayoutData(variables, layoutXData, layoutYData,
                                                   options[["layoutX"]], options[["layoutY"]])
    layoutInfo[["layoutData"]] <- xyCoords[["layoutData"]]
    layoutInfo[["layoutMessage"]] <- xyCoords[["message"]]

  }

  layoutInfo[["layoutInvalid"]] <- FALSE
  # some sanity checks on the layout
  if (!is.null(options[["layoutXData"]]) && (length(options[["layoutXData"]]) < length(variables) || length(options[["layoutYData"]]) < length(variables)))
    layoutInfo[["layoutInvalid"]] <- TRUE

  return(layoutInfo)
}

.networkAnalysisSaveLayout <- function(mainContainer, options, layout) {

  # TODO: bail out if layout computation failed
  if (!options[["addLayoutToData"]] || options[["layout"]] == "data" || is.null(layout) || mainContainer$getError())
    return()

  if (options[["computedLayoutX"]] == "" || options[["computedLayoutY"]] == "") {
    mainContainer[["generalTable"]]$addFootnote(gettext("Please supply a name for both the x and y-coordinates of the layout."))
    return()
  }

  variables <- unlist(options[["variables"]])
  mainContainer[["layoutXColumn"]] <- createJaspColumn(options[["computedLayoutX"]], c("layout", "repulsion", "layoutX", "layoutY", "computedLayoutX"))
  mainContainer[["layoutYColumn"]] <- createJaspColumn(options[["computedLayoutY"]], c("layout", "repulsion", "layoutX", "layoutY", "computedLayoutY"))
  mainContainer[["layoutXColumn"]]$setNominalText(paste(variables, "=", layout[, 1L]))
  mainContainer[["layoutYColumn"]]$setNominalText(paste(variables, "=", layout[, 2L]))

}

# bootstrap network functions ----
.networkAnalysisBootstrap <- function(mainContainer, network, options) {

  # this container is always created so that empty placeholds can be shown for the bootstrap plots
  bootstrapContainer <- mainContainer[["bootstrapContainer"]]
  if (is.null(bootstrapContainer)) {
    bootstrapContainer <- createJaspContainer("", position = 9, dependencies = c(
      "numberOfBootstraps", "BootstrapType", "bootstrapOnOff"
    ))
    mainContainer[["bootstrapContainer"]] <- bootstrapContainer
  }

  .networkAnalysisBootstrapTable(bootstrapContainer, options, position = 91)

  bootstrapResults <- .networkAnalysisComputeBootstrap(bootstrapContainer, network, options)
  if (length(bootstrapResults) > 0L && !bootstrapContainer$getError())
    bootstrapContainer[["bootstrapState"]] <- createJaspState(bootstrapResults)

  .networkAnalysisBootstrapPlot(bootstrapContainer, bootstrapResults, options, statistic = "edge", position = 92)
  .networkAnalysisBootstrapPlot(bootstrapContainer, bootstrapResults, options, statistic = c("strength", "betweenness", "closeness"), position = 93)

}

.networkAnalysisComputeBootstrap <- function(bootstrapContainer, network, options) {

  bootstrapResult <- bootstrapContainer[["bootstrapState"]]$object
  bootstrapReady <- length(options[["variables"]]) > 2L && options[["bootstrapOnOff"]] && options[["numberOfBootstraps"]] > 0L
  if (!bootstrapReady || is.null(network[["network"]]) || bootstrapContainer$getError() || !is.null(bootstrapResult))
    return(bootstrapResult)

  bootstrapResult <- list()
  allNetworks <- network[["network"]]
  nGraphs <- length(allNetworks)
  nCores <- .networkAnalysisGetNumberOfCores(options)
  noTicks <- if (options[["BootstrapType"]] == "jacknife") network[["network"]][[1L]][["nPerson"]] * nGraphs else options[["numberOfBootstraps"]] * nGraphs

  startProgressbar(noTicks * 2L, "Bootstrapping network")
  tryCatch({
    JASP:::.suppressGrDevice({
      for (nm in names(allNetworks)) {

        # .networkAnalysisBootnetBootnet replaces bootnet::bootnet so we can have a progress bar
        bootstrapResult[[nm]] <- .networkAnalysisBootnetBootnet(
          data       = allNetworks[[nm]],
          nBoots     = options[["numberOfBootstraps"]],
          type       = options[["BootstrapType"]],
          nCores     = nCores,
          statistics = c("edge", "strength", "closeness", "betweenness"),
          labels     = options[["variables"]]
        )
      }
    })
  }, error = function(e) bootstrapContainer$setError(.extractErrorMessage(e))
  )
  return(bootstrapResult)
}

# helper functions ----
.networkAnalysisAddReferencesToTables <- function(results, options) {

  # get from every .meta element the type and check if it is "table"
  idxOfTables <- sapply(results[[".meta"]], `[[`, "type") == "table"
  # get the names of .meta elements that have type "table".
  namesOfTables <- sapply(results[[".meta"]], `[[`, "name")[idxOfTables]
  for (nm in namesOfTables) { # use names to index
    if (!is.null(results[[nm]])) { # if table is not empty add reference
      # results[[nm]][["citation"]] <- as.list(bootnet:::getRefs(options[["estimator"]]))
      results[[nm]][["citation"]] <- as.list(.networkAnalysisBootnetGetRefs(options[["estimator"]]))
    }
  }

  return(results)

}

.networkAnalysisGetNumberOfCores <- function(options) {

  if (options[["parallelBootstrap"]]) {
    nCores <- parallel::detectCores(TRUE)
    if (is.na(nCores)) # parallel::detectCores returns NA if unknown/ fails
      nCores <- 1
  } else {
    nCores <- 1
  }

  return(nCores)

}

.networkAnalysisNetworkHasErrors <- function(networks) {

  # returns TRUE if network has errors; FALSE otherwise
  if (any(sapply(networks, function(x) anyNA(x[["graph"]]))))
    return("error")

  return("success")

}

.networkAnalysisFindDataType <- function(variables, variableMatch, asNumeric = FALSE, char = "=",
                                         inputCheck = NULL) {
  ## Input:
  # variables: options[["variables"]].
  # asNumeric: check if data can be converted to numeric.
  # char: what to match on, `=`` by default.
  # validInput: a function that returns TRUE if input is valid amd FALSE otherwise.

  ## Output:
  # Todo:

  errors <- list(fatal = FALSE)
  pattern <- sprintf("%s\\s*(?=[^%s]+$)", char, char)
  matches <- stringr::str_split(variableMatch, pattern)#":\\s*(?=[^:]+$)")

  lens <- lengths(matches)
  if (!all(lens == 2)) {
    errors <- c(errors, list(list(
      matches = unlist(matches[lens != 2]),
      type = "missingChar",
      message = gettextf("Some variables did not contain char (%s)", char)
    )))
    matches <- matches[lens == 2]
    if (length(matches) == 0) {# too poor input to continue
      errors[["fatal"]] <- TRUE
      return(list(errors = errors))
    }
  }

  # bind list to matrix
  matches <- do.call(rbind, matches)
  # check if matches appear in variables
  dontMatch <- !(matches[, 1] %in% variables)
  # check if matches appear in variables while being robust for whitespace issues
  doMatchWs <- trimws(matches[dontMatch, 1]) %in% variables
  matches[doMatchWs, 1] <- trimws(matches[doMatchWs, 1])

  trouble <- dontMatch & !doMatchWs
  if (any(trouble)) { # TRUE if any variable did not appear in any matches

    errors <-  c(errors, list(list(
      matches = unlist(matches[trouble, ]),
      type = "missingMatch",
      message = gettext("Some variables were not matched in the column names of the dataset.")
    )))

    matches <- matches[!trouble, ]
  }

  # unmatched variables
  unmatched <- variables[!(variables %in% matches[, 1])]

  values <- matches[, 2]
  if (asNumeric) {
    # replace commas by points
    values <- gsub(",", ".",  matches[, 2])
    # convert to numeric
    values <- suppressWarnings(as.numeric(values))

    if (anyNA(values)) {
      errors <-  c(errors, list(list(
        matches = matches[is.na(values), ],
        type = "missingNumeric",
        message = gettext("Some variables were not matched in the column names of the dataset.")
      )))
    }
  }

  # check if input actually is correct
  validInput <- NULL
  if (!is.null(inputCheck)) {
    validInput <- inputCheck(matches[, 2])
    errors[["fatal"]] <- !any(validInput)
  }

  return(list(matches = matches, values = values, unmatched = unmatched,
              validInput = validInput, errors = errors))

}

.networkAnalysisSanitizeMgmVariableType <- function(variables, options) {

  checks <- .networkAnalysisFindDataType(variables = variables, variableMatch = options[["mgmVariableTypeData"]], char = "=",
                                         inputCheck = function(x) x %in% c("g", "p", "c"))

  if (checks[["errors"]][["fatal"]]) {
    message <- paste0(gettextf("Data supplied in %s cannot be used to determine variables types. Data should: ", options[["mgmVariableType"]]),
                      gettext("<ul><li>start with the column name of the variable.</ul></li>"),
                      gettext("<ul><li>contain an '=' to distinguish between column name and data type.</ul></li>"),
                      gettext("<ul><li>end with either 'g' for Gaussian, 'c' for categorical, or 'p' for Poisson.</ul></li>")
    )
    .quitAnalysis(message)
  }

  newData <- checks[["matches"]]
  newData[!checks[["validInput"]], 2] <- "g"
  if (length(checks[["unmatched"]] > 0))
    newData <- rbind(newData, cbind(checks[["unmatched"]], "g"))
  newData <- newData[match(variables, newData[, 1]), ]
  return(newData[, 2])

}

.networkAnalysisSanitizeLayoutData <- function(variables, layoutXData, layoutYData,
                                               nameX, nameY) {

  checksX <- .networkAnalysisFindDataType(variables = variables, variableMatch = layoutXData, char = "=",
                                          inputCheck = Negate(is.na), asNumeric = TRUE)
  checksY <- .networkAnalysisFindDataType(variables = variables, variableMatch = layoutYData, char = "=",
                                          inputCheck = Negate(is.na), asNumeric = TRUE)

  message <- NULL
  defMsg <- gettext("Supplied data for layout was not understood and instead a circle layout was used.")
  if (checksX[["errors"]][["fatal"]] || checksY[["errors"]][["fatal"]]) {

    if (checksX[["errors"]][["fatal"]] && checksY[["errors"]][["fatal"]]) {
      firstLine <- gettextf("Data supplied in %1$s AND %2$s could not be used to determine node locations.", nameX, nameY)
    } else if (checksX[["errors"]][["fatal"]]) {
      firstLine <- gettextf("Data supplied in %s could not be used to determine node locations.", nameX)
    } else {
      firstLine <- gettextf("Data supplied in %s could not be used to determine node locations.", nameY)
    }
    message <- gettextf("%1$s %2$s Data should only contain numeric:
                 -start with the column name of the variable.
                 -contain an '=' to distinguish between column name and coordinate.",
                        defMsg, firstLine)
  } else if (length(checksX[["unmatched"]]) > 0 || length(checksY[["unmatched"]]) > 0) {

    unmatchedX <- paste(checksX[["unmatched"]], collapse = ", ")
    unmatchedY <- paste(checksY[["unmatched"]], collapse = ", ")
    message <- defMsg
    if (unmatchedX != "")
      message <- sprintf(ngettext(length(checksX[["unmatched"]]), "%1$s X-Coordinates for variable %2$s was not understood.", "%1$s X-Coordinates for variables %2$s were not understood."), message, unmatchedX)

    if (unmatchedY != "")
      message <- sprintf(ngettext(length(checksY[["unmatched"]]), "%1$s Y-Coordinates for variable %2$s was not understood.", "%1$s Y-Coordinates for variables %2$s were not understood."), message, unmatchedY)
  }

  matchX <- checksX[["matches"]]
  matchY <- checksY[["matches"]]
  orderX <- match(variables, matchX[, 1])
  orderY <- match(variables, matchY[, 1])
  layoutData <- cbind(x = checksX[["values"]][orderX], y = checksY[["values"]][orderY])
  if (!is.null(layoutData)) {
    if (is.null(nrow(layoutData)))
      layoutData <- matrix(layoutData, nrow = 1)
    rownames(layoutData) <- variables
  }
  out <- list(layoutData = layoutData, message = message)

  return(out)

}

.networkAnalysisSanitizeColorNodesByData <- function(variables, options) {

  checks <- .networkAnalysisFindDataType(variables = variables, variableMatch = options[["colorNodesByData"]], char = "=",
                                         inputCheck = Negate(is.na))
  message <- NULL
  if (checks[["errors"]][["fatal"]]) {
    message <- gettextf("Data supplied in %s could not be used to determine variables types. Data should: \n- Start with the column name of the variable. \n- Contain an '=' to distinghuish betweem column name and group.",
                        options[["colorNodesBy"]])
    return(list(newData = NULL, message = message))
  }

  newData <- checks[["matches"]]
  if (length(checks[["unmatched"]] > 0)) {
    undefGroup <- undefGroup0 <- "Undefined Group"
    add <- 1L
    while (undefGroup %in% newData[, 2] && add < 10L) {
      undefGroup <- paste0(undefGroup0, add)
      add <- add + 1L
    }

    newData <- rbind(newData, cbind(checks[["unmatched"]], undefGroup))
    message <- gettextf("Some entries of %1$s were not understood. These are now grouped under '%2$s'.",
                        options[["colorNodesBy"]], undefGroup)
  }
  newData <- newData[match(variables, newData[, 1]), ]
  return(list(newData = newData[, 2], message = message))

}

.networkAnalysisCheckKnownErrors <- function(e) {

  # NOTE: this fails if glmnet decides to translate their error messages one day.
  # Unfortunately, these error appear for particular subsets of the data (from cross validation),
  # so it's difficult to traceable particular errors to the complete data.

  errmsg <- .extractErrorMessage(e)
  # possibly add other checks here in the future
  dataIssue <- startsWith(errmsg, "y is constant") || endsWith(errmsg, "standardization step")

  ans <- if (dataIssue) {
    gettextf("bootnet crashed with the following error message:\n%s\n\nPlease check if there are variables in the network with little variance or few distinct observations.", errmsg)
  } else {
    gettextf("bootnet crashed with the following error message:\n%s", errmsg)
  }

  return(ans)

}

# functions modified from bootnet ----
# exact duplicate of bootnet::bootnet but with progressbar
.networkAnalysisBootnetBootnet <- function(data, nBoots = 1000,
                                           default = c("none", "EBICglasso", "pcor", "IsingFit", "IsingSampler", "huge", "adalasso", "mgm", "relimp", "cor"),
                                           type = c("nonparametric", "parametric", "node", "person", "jackknife", "case"),
                                           nCores = 1, statistics = c("edge", "strength", "closeness", "betweenness"), model = c("detect", "GGM", "Ising"),
                                           fun, prepFun, prepArgs, estFun, estArgs, graphFun, graphArgs, intFun, intArgs, verbose = TRUE,
                                           construct = c("default", "function", "arguments"), labels, alpha = 1, caseMin = 0.05,
                                           caseMax = 0.75, caseN = 10, subNodes = 2:(ncol(data) - 1),
                                           subCases = round((1 - seq(caseMin, caseMax, length = caseN)) *
                                                              nrow(data)), computeCentrality = TRUE, propBoot = 1,
                                           replacement = TRUE, graph, sampleSize, intercepts, weighted,
                                           signed, directed, ...
                                           #progressbar = NULL, resultsForProgressbar = NULL, callback = NULL

) {
  if (default[[1]] == "glasso")
    default <- "EBICglasso"
  default <- match.arg(default)
  type <- match.arg(type)
  if (type == "case")
    type <- "person"
  model <- match.arg(model)
  if (missing(data)) {
    if (type != "parametric") {
      warning("'data' can only be missing if type = 'parametric'. Setting type = 'parametric' and performing parametric bootstrap instead.")
      type <- "parametric"
    }
    if (missing(graph)) {
      stop("'graph' may not be missing in parametric bootstrap when 'data' is missing.")
    }
    if (missing(sampleSize)) {
      stop("'sampleSize' may not be missing in parametric bootstrap when 'data' is missing.")
    }
    N <- ncol(graph)
    Np <- sampleSize
    if (missing(intercepts)) {
      intercepts <- rep(0, Np)
    }
    if (!missing(data)) {
      warning("'data' is ignored when using manual parametric bootstrap.")
      data <- NULL
    }
    manual <- TRUE
    dots <- list(...)
  }
  else {
    manual <- FALSE
    if (is(data, "bootnetResult")) {
      default <- data$default
      inputCheck <- data$.input
      fun <- data$estimator
      dots <- data$arguments
      if (missing(weighted)) {
        weighted <- data$weighted
      }
      if (missing(signed)) {
        signed <- data$signed
      }
      if (missing(directed)) {
        directed <- data$directed
      }
      data <- data$data
      N <- ncol(data)
      Np <- nrow(data)
    }
    else {
      dots <- list(...)
      N <- ncol(data)
      Np <- nrow(data)
      fun <- NULL
      if (!manual) {
        goodColumns <- sapply(data, function(x) is.numeric(x) |
                                is.ordered(x) | is.integer(x))
        if (!all(goodColumns)) {
          if (verbose) {
            warning(paste0("Removing non-numeric columns: ",
                           paste(which(!goodColumns), collapse = "; ")))
          }
          data <- data[, goodColumns, drop = FALSE]
        }
      }
    }
  }
  inputCheck <- bootnet:::checkInput(default = default, fun = fun, prepFun = prepFun,
                                     prepArgs = prepArgs, estFun = estFun, estArgs = estArgs,
                                     graphFun = graphFun, graphArgs = graphArgs, intFun = intFun,
                                     intArgs = intArgs, sampleSize = Np, construct = construct,
                                     .dots = dots)
  if (missing(weighted)) {
    weighted <- TRUE
  }
  if (missing(signed)) {
    signed <- TRUE
  }
  if (missing(directed)) {
    if (!default %in% c("graphicalVAR", "relimp", "DAG"))
      directed <- FALSE
  }
  if (type == "jackknife") {
    message("Jacknife overwrites nBoot to sample size")
    nBoots <- Np
  }
  if (type == "node" & N < 3) {
    stop("Node-wise bootstrapping requires at least three nodes.")
  }
  if (!manual && !(is.data.frame(data) || is.matrix(data))) {
    stop("'data' argument must be a data frame")
  }
  if (!manual && is.matrix(data)) {
    data <- as.data.frame(data)
  }
  if (missing(labels)) {
    if (manual) {
      labels <- colnames(graph)
      if (is.null(labels)) {
        labels <- seq_len(ncol(graph))
      }
    }
    else {
      labels <- colnames(data)
      if (is.null(labels)) {
        labels <- seq_len(ncol(data))
      }
    }
  }
  if (type == "parametric" & model == "detect") {
    if (manual) {
      stop("'model' must be set in parametric bootstrap without 'data'.")
    }
    if (default != "none") {
      model <- ifelse(grepl("ising", default, ignore.case = TRUE),
                      "Ising", "GGM")
    }
    else {
      model <- ifelse(any(grepl("ising", deparse(estFun),
                                ignore.case = TRUE)), "Ising", "GGM")
    }
    message(paste0("model set to '", model, "'"))
  }
  if (!manual) {
    if (verbose) {
      message("Estimating sample network...")
    }
    sampleResult <- bootnet::estimateNetwork(data, default = default,
                                             fun = inputCheck$estimator, .dots = inputCheck$arguments,
                                             labels = labels, verbose = verbose, weighted = weighted,
                                             signed = signed, .input = inputCheck)
  }
  else {
    sampleResult <- list(graph = graph, intercepts = intercepts,
                         labels = labels, nNodes = N, nPerson = Np, estimator = inputCheck$estimator,
                         arguments = inputCheck$arguments, default = default,
                         weighted = weighted, signed = signed)
    class(sampleResult) <- c("bootnetResult", "list")
  }
  if (nCores == 1) {
    bootResults <- vector("list", nBoots)
    if (verbose) {
      message("Bootstrapping...")
      pb <- txtProgressBar(0, nBoots, style = 3)
    }
    for (b in seq_len(nBoots)) {
      tryLimit <- 10
      tryCount <- 0
      repeat {
        if (!type %in% c("node", "person")) {
          nNode <- N
          inSample <- seq_len(N)
          if (type == "jackknife") {
            bootData <- data[-b, , drop = FALSE]
            nPerson <- Np - 1
          }
          else if (type == "parametric") {
            nPerson <- Np
            if (model == "Ising") {
              bootData <- IsingSampler::IsingSampler(round(propBoot *
                                                             Np), noDiag(sampleResult$graph), sampleResult$intercepts)
            }
            else if (model == "GGM") {
              g <- -sampleResult$graph
              diag(g) <- 1
              bootData <- mvtnorm::rmvnorm(round(propBoot *
                                                   Np), sigma = corpcor::pseudoinverse(g))
            }
            else stop(paste0("Model '", model, "' not supported."))
          }
          else {
            nPerson <- Np
            bootData <- data[sample(seq_len(Np), round(propBoot *
                                                         Np), replace = replacement), ]
          }
        }
        else if (type == "node") {
          nPerson <- Np
          nNode <- sample(subNodes, 1)
          inSample <- sort(sample(seq_len(N), nNode))
          bootData <- data[, inSample, drop = FALSE]
        }
        else {
          nNode <- ncol(data)
          nPerson <- sample(subCases, 1)
          inSample <- 1:N
          persSample <- sort(sample(seq_len(Np), nPerson))
          bootData <- data[persSample, , drop = FALSE]
        }
        if (!missing(prepFun)) {
          if (!missing(prepArgs) & is.list(prepArgs) &
              identical(prepFun, qgraph::cor_auto)) {
            prepArgs$verbose <- FALSE
          }
        }
        res <- suppressWarnings(try({
          bootnet::estimateNetwork(bootData, default = default,
                                   fun = inputCheck$estimator, .dots = inputCheck$arguments,
                                   labels = labels[inSample], verbose = FALSE,
                                   weighted = weighted, signed = signed, .input = inputCheck,
                                   memorysaver = TRUE)
        }))
        if (is(res, "try-error")) {
          if (tryCount == tryLimit) {
            stop("Maximum number of errors in bootstraps reached")
          }
          tryCount <- tryCount + 1
        }
        else {
          break
        }

      }
      bootResults[[b]] <- res

      progressbarTick()

      if (verbose) {
        setTxtProgressBar(pb, b)
      }
    }
    if (verbose) {
      close(pb)
    }
  }
  else {
    if (verbose) {
      message("Bootstrapping...")
    }

    if (missing(graph)) {
      graph <- matrix(0, N, N)
    }
    if (missing(data)) {
      data <- matrix(0, Np, N)
    }
    if (missing(intercepts)) {
      intercepts <- rep(0, N)
    }
    if (missing(sampleSize)) {
      sampleSize <- Np
    }
    # excl <- c("prepFun", "prepArgs", "estFun", "estArgs",
    #           "graphFun", "graphArgs", "intFun", "intArgs", "fun")
    # objects to export
    # objToExport <- ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c(excl, "cl", "...")]

    # TODO: this won't work with jaspResults!
    objToExport <- c("type", "data", "N", "Np", "model", "propBoot",
                     "sampleResult", "replacement", "subNodes", "subCases",
                     "default", "inputCheck", "labels", "weighted", "signed")

    setup <- .makeParallelSetup(pb = progressbar, objs = objToExport, env = environment())
    on.exit(eval(setup$stopCluster))
    `%dopar%` <- setup$dopar
    cl <- setup[["cl"]]

    # snow::clusterExport(cl = cl, list = objToExport, envir = environment())

    # bootResults <- parallel::parLapply(cl, seq_len(nBoots), function(b) {
    bootResults <- tryCatch(foreach::foreach(
      b = seq_len(nBoots),
      .options.snow=setup$progress,
      .inorder = FALSE,
      # .export = objToExport,
      .packages = c("bootnet", "mvtnorm", "corpcor"),
      .verbose = TRUE
    ) %dopar% {
      tryLimit <- 10
      tryCount <- 0
      repeat {
        if (!type %in% c("node", "person")) {
          nNode <- ncol(data)
          inSample <- seq_len(N)
          if (type == "jackknife") {
            bootData <- data[-b, , drop = FALSE]
            nPerson <- Np - 1
          }
          else if (type == "parametric") {
            nPerson <- Np
            if (model == "Ising") {
              bootData <- IsingSampler::IsingSampler(round(propBoot *
                                                             Np), noDiag(sampleResult$graph), sampleResult$intercepts)
            }
            else if (model == "GGM") {
              g <- -sampleResult$graph
              diag(g) <- 1
              bootData <- mvtnorm::rmvnorm(round(propBoot *
                                                   Np), sigma = corpcor::pseudoinverse(g))
            }
            else stop(paste0("Model '", model, "' not supported."))
          }
          else {
            nPerson <- Np
            bootData <- data[sample(seq_len(Np), round(propBoot *
                                                         Np), replace = replacement), ]
          }
        }
        else if (type == "node") {
          nPerson <- Np
          nNode <- sample(subNodes, 1)
          inSample <- sort(sample(seq_len(N), nNode))
          bootData <- data[, inSample, drop = FALSE]
        }
        else {
          nNode <- ncol(data)
          nPerson <- sample(subCases, 1)
          inSample <- 1:N
          persSample <- sort(sample(seq_len(Np), nPerson))
          bootData <- data[persSample, , drop = FALSE]
        }
        res <- suppressWarnings(try({
          bootnet::estimateNetwork(bootData, default = default,
                                   fun = inputCheck$estimator, .dots = inputCheck$arguments,
                                   labels = labels[inSample], verbose = FALSE,
                                   weighted = weighted, signed = signed, memorysaver = TRUE)
        }))
        if (is(res, "try-error")) {
          if (tryCount == tryLimit)
            stop("Maximum number of errors in bootstraps reached")
          tryCount <- tryCount + 1
        }
        else {
          break
        }
      }
      return(res)
    }, warning=function(w) w)

    # if aborted
    if (inherits(bootResults, "warning")) {
      if (bootResults$message == gettext("progress function failed: Cancelled by callback")) {
        return()
      }
    }
  }


  if (verbose) {
    message("Computing statistics...")
  }
  statTableOrig <- bootnet:::statTable(sampleResult, name = "sample",
                                       alpha = alpha, computeCentrality = computeCentrality,
                                       statistics = statistics, directed = directed)
  if (nCores == 1) {
    if (verbose) {
      pb <- txtProgressBar(0, nBoots, style = 3)
    }
    statTableBoots <- vector("list", nBoots)
    for (b in seq_len(nBoots)) {
      statTableBoots[[b]] <- bootnet:::statTable(bootResults[[b]],
                                                 name = paste("boot", b), alpha = alpha, computeCentrality = computeCentrality,
                                                 statistics = statistics, directed = directed)
      progressbarTick()
      if (verbose) {
        setTxtProgressBar(pb, b)
      }
    }
    if (verbose) {
      close(pb)
    }
  }
  else {

    objToExport <- c("bootResults", "alpha", "computeCentrality",
                     "statistics", "directed")
    snow::clusterExport(cl = cl, list = objToExport, envir = environment())
    statTableBoots <- tryCatch(foreach::foreach(
      b = seq_len(nBoots),
      .options.snow=setup$progress,
      .inorder = FALSE,
      # .export = objToExport,
      .packages = c("bootnet"),
      .verbose = TRUE
    ) %dopar% {
      bootnet:::statTable(bootResults[[b]], name = paste("boot", b),
                          alpha = alpha, computeCentrality = computeCentrality,
                          statistics = statistics, directed = directed)
    }, warning=function(w) w)

    # if aborted
    if (inherits(bootResults, "warning")) {
      if (bootResults$message == "progress function failed: Cancelled by callback") {
        return()
      }
    }
    parallel::stopCluster(cl)
  }
  Result <- list(sampleTable = dplyr::ungroup(statTableOrig),
                 bootTable = dplyr::ungroup(dplyr::bind_rows(statTableBoots)),
                 sample = sampleResult, boots = bootResults, type = type,
                 sampleSize = Np)
  class(Result) <- "bootnet"
  return(Result)
}

# TODO: there is no init anymore. Does the remark below still apply???
# direct copy of bootnet:::getRefs. Otherwise, bootnet namespace gets loaded on init which takes pretty long.
.networkAnalysisBootnetGetRefs <- function (x) {
  citation <- switch(x,
                     none = "",
                     EBICglasso = c("Friedman, J. H., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9 (3), 432-441.",
                                    "Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. , 23 , 2020-2028.",
                                    "Friedman, J. H., Hastie, T., & Tibshirani, R. (2014). glasso: Graphical lasso estimation of gaussian graphical models. Retrieved from https://CRAN.R-project.org/package=glasso",
                                    "Epskamp, S., Cramer, A., Waldorp, L., Schmittmann, V. D., & Borsboom, D. (2012). qgraph: Network visualizations of relationships in psychometric data. Journal of Statistical Software, 48 (1), 1-18."),
                     glasso = c("Friedman, J. H., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9 (3), 432-441.",
                                "Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. , 23 , 2020-2028.",
                                "Friedman, J. H., Hastie, T., & Tibshirani, R. (2014). glasso: Graphical lasso estimation of gaussian graphical models. Retrieved from https://CRAN.R-project.org/package=glasso",
                                "Epskamp, S., Cramer, A., Waldorp, L., Schmittmann, V. D., & Borsboom, D. (2012). qgraph: Network visualizations of relationships in psychometric data. Journal of Statistical Software, 48 (1), 1-18."),
                     IsingFit = "van Borkulo, C. D., Borsboom, D., Epskamp, S., Blanken, T. F., Boschloo, L., Schoevers, R. A., & Waldorp, L. J. (2014). A new method for constructing networks from binary data. Scientific reports, 4 (5918), 1-10.",
                     IsingSampler = c("Epskamp, S., Maris, G., Waldorp, L., & Borsboom, D. (in press). Network psychometrics. In P. Irwing, D. Hughes, & T. Booth (Eds.), Handbook of psychometrics. New York, NY, USA: Wiley.",
                                      "Epskamp, S. (2014). IsingSampler: Sampling methods and distribution functions for the Ising model. Retrieved from github.com/SachaEpskamp/IsingSampler"),
                     huge = "Zhao, T., Li, X., Liu, H., Roeder, K., Lafferty, J., & Wasserman, L. (2015). huge: High-dimensional undirected graph estimation. Retrieved from https://CRAN.R-project.org/package=huge",
                     adalasso = "Kraeamer, N., Schaeafer, J., & Boulesteix, A.-L. (2009). Regularized estimation of large-scale gene association networks using graphical gaussian models. BMC Bioinformatics, 10 (1), 1-24.",
                     mgm = "Jonas M. B. Haslbeck, Lourens J. Waldorp (2016). mgm: Structure Estimation for Time-Varying Mixed Graphical Models in high-dimensional Data arXiv preprint:1510.06871v2 URL http://arxiv.org/abs/1510.06871v2.")
  citation <- c(citation, "Epskamp, S., Borsboom, D., & Fried, E. I. (2016). Estimating psychological networks and their accuracy: a tutorial paper. arXiv preprint, arXiv:1604.08462.")
  citation
}
