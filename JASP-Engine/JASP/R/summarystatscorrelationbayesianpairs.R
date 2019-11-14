#
# Copyright (C) 2013-2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A P          ARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
SummaryStatsCorrelationBayesianPairs <- function(jaspResults, dataset=NULL, options, ...) {

  # TODO(Alexander): When we allow for other test points, add something like this
  #
  # .checkErrors.summarystats.binomial(options)

  # Compute the results and create main results table
  ready <- options[["n"]] > 0

  bfObject <- .sumStatCorrelationTable(jaspResults, options, ready)
  # Output plots
  bfObject <- .sumStatCorrelationPlots(jaspResults, options, bfObject, ready)

  jaspResults[["bfState"]] <- createJaspState(bfObject, dependencies=c("n", "method", "rObs", "tauObs",
                                                                       "rhoObs", "kappa"))
  # return()
}

# Execute Bayesian binomial test ----
.sumStatCorrelationTable <- function(jaspResults, options, ready) {
  # a. Retrieve from state ------
  #
  correlationContainer <- jaspResults[["correlationContainer"]]

  if (is.null(correlationContainer)) {
    correlationContainer <- createJaspContainer()
    correlationContainer$dependOn(c("n", "method", "rObs", "tauObs", "rhoObs", "alternative", "kappa"))
    jaspResults[["correlationContainer"]] <- correlationContainer
  }

  corBayesTable <- correlationContainer[["corBayesTable"]]
  corBayesTable$addCitation(.getCorCitations(options[["method"]], bayes=TRUE))

  # If table already exists in the state, return it
  if (!is.null(corBayesTable))
    # TODO(Alexander): Here check that bfState has something. Perhaps this might screw up the table
    #
    return(jaspResults[["bfState"]]$object)

  # b. Get ------
  #
  corBayesTable <- .getTableSumStatCorBayes(options)

  # c. Check if it's ready ------
  #
  if (!ready) {
    statObs <- switch(options[["method"]],
                      "pearson"=options[["rObs"]],
                      "kendall"=options[["tauObs"]],
                      "spearman"=options[["rhoObs"]])
    corBayesTable$addRows(list("stat"=statObs))
    jaspResults[["correlationContainer"]][["corBayesTable"]] <- corBayesTable
    return(jaspResults[["bfState"]]$object)
  }

  # d. Compute ------
  #
  saveToState <- FALSE
  bfState <- jaspResults[["bfState"]]

  if (!is.null(bfState)) {
    # TODO(Alexander): This doesn't necessarily means that bfState$object is not null does it?
    bfObject <- bfState$object
  } else {
    statObs <- switch(options[["method"]],
                      "pearson"=options[["rObs"]],
                      "kendall"=options[["tauObs"]],
                      "spearman"=options[["rhoObs"]]
    )

    bfObject <- bcor.testSumStat("n"=options[["n"]], "stat"=statObs, "alternative"=options[["alternative"]],
                                 "method"=options[["method"]], "ciValue"=options[["ciValue"]], "kappa"=options[["kappa"]])
    tempList <- .pValueFromCor("n"=options[["n"]], "stat"=statObs, "method"=options[["method"]])
    bfObject <- modifyList(bfObject, tempList)
  }

  errorMessage <- bfObject[["error"]]

  if (!is.null(errorMessage)) {
    corBayesTable$setError(errorMessage)
    jaspResults[["correlationContainer"]][["corBayesTable"]] <- corBayesTable
    return(bfObject)
  }

  itemList <- c("n", "stat", "bf", "p")
  
  if (options[["ci"]]) {
    bfObject <- .refreshCorCredibleInterval(bfObject, "ciValue"=options[["ciValue"]], 
                                            "method"=options[["method"]])
    itemList <- c(itemList, "lowerCi", "upperCi")
  }
  
  sidedObject <- .getSidedObject(bfObject, alternative=options[["alternative"]])
  rowResult <- sidedObject[itemList]
  
  if (options[["bayesFactorType"]]=="BF01") {
    rowResult[["bf"]] <- 1/rowResult[["bf"]]
  } else if (options[["bayesFactorType"]]=="LogBF10") {
    rowResult[["bf"]] <- log(rowResult[["bf"]])
  }

  # d. Fill ------
  #
  corBayesTable$setData(rowResult)
  jaspResults[["correlationContainer"]][["corBayesTable"]] <- corBayesTable
  return(bfObject)
}

.getTableSumStatCorBayes <- function(options){
  # create table and state dependencies
  corBayesTable <- createJaspTable(title=.getCorTableTitle(options[["test"]], bayes=TRUE))
  corBayesTable$showSpecifiedColumnsOnly <- TRUE
  corBayesTable$position <- 1
  corBayesTable$dependOn(c("bayesFactorType", "ci", "ciValue"))

  corBayesTable$addCitation(.getCorCitations(options[["test"]], bayes=TRUE))

  # Add sided footnote
  #
  if (options[["alternative"]]=="greater")
    corBayesTable$addFootnote(message=.getBfTableSidedFootnote(alternative="greater", analysis="correlation"),
                              symbol="<i>Note</i>.")

  if (options[["alternative"]]=="less")
    corBayesTable$addFootnote(message=.getBfTableSidedFootnote(alternative="less", analysis="correlation"),
                              symbol="<i>Note</i>.")

  bfTitle <- .getBfTitle(options[["bayesFactorType"]], options[["alternative"]])
  statName <- switch(options[["method"]],
                     "pearson"="r",
                     "kendall"="tau",
                     "spearman"="rho"
  )

  corBayesTable$addColumnInfo(name = "n", title = "n" , type = "integer")
  corBayesTable$addColumnInfo(name = "stat", title = statName, type = "number")
  corBayesTable$addColumnInfo(name="bf", title=bfTitle, type="number")
  corBayesTable$addColumnInfo(name = "p", title = "p", type = "number")

  if (options[["ci"]]) {
    overTitle <- paste0(options[["ciValue"]]*100, "% Credible interval")
    corBayesTable$addColumnInfo(name="lowerCi", overtitle=overTitle, type="number", title="Lower")
    corBayesTable$addColumnInfo(name="upperCi", overtitle=overTitle, type="number", title="Upper")
  }

  return(corBayesTable)
}

# Prior and Posterior plot ----
.sumStatCorrelationPlots <- function(jaspResults, options, bfObject, ready) {
  # 
  # 
  plotItems <- .getCorPlotItems(options, sumStat=TRUE)
  
  if (length(plotItems)==0 | !ready)
    return(bfObject)
  
  # a. Get plot container ----- 
  #
  plotContainer <- jaspResults[["correlationContainer"]][["plotContainer"]]
  
  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(title="Inferential Plots")
    plotContainer$dependOn(c("plotPriorPosterior", "plotBfRobustness"))
    plotContainer$position <- 2
    jaspResults[["correlationContainer"]][["plotContainer"]] <- plotContainer
  }
  
  # b. Define dependencies for the plots ----- 
  # For plotPriorPosterior
  # 
  bfPlotPriorPosteriorDependencies <- c("plotPriorPosteriorAddTestingInfo", "plotPriorPosteriorAddEstimationInfo")
  
  if (options[["plotPriorPosteriorAddEstimationInfo"]]) 
    bfPlotPriorPosteriorDependencies <- c(bfPlotPriorPosteriorDependencies, "ciValue")
  
  # For plotBfRobustness
  # 
  bfPlotRobustnessDependencies <- c("plotBfRobustnessAddInfo")
  
  if (options[["plotBfRobustnessAddInfo"]]) 
    bfPlotRobustnessDependencies <- c(bfPlotRobustnessDependencies, "bayesFactorType")
  
  plotItemDependencies <- list(
    "plotPriorPosterior"=bfPlotPriorPosteriorDependencies,
    "plotBfRobustness"=bfPlotRobustnessDependencies
  )
  
  alternative <- options[["alternative"]]
  
  # c. Per plotItem add plot ------
  # 
  for (i in seq_along(plotItems)) {
    item <- plotItems[i]
    jaspPlotResult <- plotContainer[[item]]
    plotResult <- jaspPlotResult$plotObject
    
    # d. Check if plot is in there ------ 
    # 
    if (is.null(plotResult)) {
      itemTitle <- .bfPlotTitles[[item]]
      
      jaspPlotResult <- createJaspPlot(title=itemTitle, width=530, height=400)
      jaspPlotResult$dependOn(options = plotItemDependencies[[item]])
      jaspPlotResult$position <- i
      plotContainer[[item]] <- jaspPlotResult
      
      if (item=="plotPriorPosterior") {
        sidedResult <- .computeCorPosteriorLine("bfObject"=bfObject, "method"=options[["method"]], 
                                                "alternative"=alternative)
        bfObject[[alternative]] <- sidedResult
        
        if (options[["plotPriorPosteriorAddEstimationInfo"]]) 
          bfObject <- .refreshCorCredibleInterval("bfObject"=bfObject, ciValue=options[["ciValue"]], 
                                                  "method"=options[["method"]])
        
        triedPlot <- .drawPosteriorPlotCorBayes(bfObject, options, "methodItems"=options[["method"]], "purpose"="sumStat")
        .checkAndSetPlotCorBayes(triedPlot, jaspPlotResult)
      } else if (item=="plotBfRobustness") {
        tempResult <-.computeCorRobustnessLine(bfObject, "method"=options[["method"]]) 
        bfObject <- modifyList(bfObject, tempResult)
        triedPlot <- .drawBfRobustnessPlotCorBayes(bfObject, options)
        .checkAndSetPlotCorBayes(triedPlot, jaspPlotResult)
      }
    } 
  }
  
  return(bfObject)
}
