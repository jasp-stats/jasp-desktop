#
# Copyright (C) 2013-2020 University of Amsterdam
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
SummaryStatsCorrelationBayesianPairs <- function(jaspResults, dataset=NULL, options, ...) {
  # Note(Alexander): Rename the ridiculous old option names to new names 
  options <- .renameCorOptions(options)
  
  # TODO(Alexander): When we allow for other test points, add a check like this
  #
  # .checkErrors.summarystats.binomial(options)
  # Compute the results and create main results table
  ready <- options[["n"]] > 0
  
  correlationContainer <- .getContainerCorSumStats(jaspResults)
  
  corModel <- .computeModelCorSumStats(correlationContainer, options, ready)
  
  .createTableCorSumStats(correlationContainer, corModel, options)
  
  if (options[["plotPriorAndPosterior"]] || options[["plotBayesFactorRobustness"]])
    .createPlotsCorSumStats(correlationContainer, corModel, options)
}

# Execute Bayesian binomial test ----
.computeModelCorSumStats <- function(correlationContainer, options, ready) {
  if (!is.null(correlationContainer[["corModel"]]))
    return(correlationContainer[["corModel"]]$object)
  
  if (!ready)
    return(NULL)
  
  statObs <- switch(options[["method"]],
                    pearson  = options[["rObs"]],
                    kendall  = options[["tauObs"]],
                    spearman = options[["rhoSObs"]])
  
  corResults <- bstats::bcor.testSumStat(n=options[["n"]], stat=statObs, alternative=options[["alternative"]],
                                         method=options[["method"]], ciValue=options[["ciValue"]], 
                                         kappa=options[["kappa"]])
  pValue <- bstats::pValueFromCor(n=options[["n"]], stat=statObs, method=options[["method"]])
  results <- modifyList(corResults, pValue)
  
  if (!is.null(results[["error"]]))
    correlationContainer$setError(results[["error"]])
  
  correlationContainer[["corModel"]] <- createJaspState(results)
  correlationContainer[["corModel"]]$dependOn("ciValue")
  
  return(results)
}

.getContainerCorSumStats <- function(jaspResults) {
  correlationContainer <- jaspResults[["correlationContainer"]]
  
  if (is.null(correlationContainer)) {
    correlationContainer <- createJaspContainer()
    correlationContainer$dependOn(c("sampleSize", "correlationCoefficient", "pearsonRhoValue", "kendallTauValue", 
                                    "rhoSObs", "priorWidth"))
    jaspResults[["correlationContainer"]] <- correlationContainer
  }
  return(correlationContainer)
}

.createTableCorSumStats <- function(correlationContainer, corModel, options) {
  if (!is.null(correlationContainer[["corBayesTable"]]))
    return()
  
  corBayesTable <- .createTableMarkupCorSumStats(options)
  
  errorMessage <- corModel[["error"]]
  
  if (is.null(corModel) || correlationContainer$getError()) {
    statObs <- switch(options[["method"]],
                      pearson  = options[["rObs"]],
                      kendall  = options[["tauObs"]],
                      spearman = options[["rhoSObs"]])
    
    emptyIshRow <-list("n"=".", "stat"=statObs, "bf"=".", "p"=".")
    
    if (options[["ci"]])
      emptyIshRow <- modifyList(emptyIshRow, list("upperCi"=".", "lowerCi"="."))
    
    corBayesTable$addRows(emptyIshRow)
  } else {
    itemList <- c("n", "stat", "bf", "p")
    
    if (options[["ci"]])
      itemList <- c(itemList, "lowerCi", "upperCi")
    
    sidedObject <- bstats::getSidedObject(corModel, alternative=options[["alternative"]])
    rowResult <- sidedObject[itemList]
    
    if (options[["bayesFactorType"]] == "BF01")
      rowResult[["bf"]] <- 1/rowResult[["bf"]]
    else if (options[["bayesFactorType"]] == "LogBF10")
      rowResult[["bf"]] <- log(rowResult[["bf"]])
    
    corBayesTable$setData(rowResult)
  }
  
  correlationContainer[["corBayesTable"]] <- corBayesTable
}

.createTableMarkupCorSumStats <- function(options){
  # create table and state dependencies
  
  corBayesTable <- createJaspTable(title=.getCorTableTitle(options[["test"]], bayes=TRUE))
  corBayesTable$showSpecifiedColumnsOnly <- TRUE
  corBayesTable$position <- 1
  corBayesTable$dependOn(c("bayesFactorType", "ci", "ciValue", "hypothesis"))
  
  corBayesTable$addCitation(.getCorCitations(options[["method"]], bayes=TRUE))
  
  # Add sided footnote
  #
  if (options[["alternative"]]=="greater")
    corBayesTable$addFootnote(.getBfTableSidedFootnote(alternative="greater", analysis="correlation"))
  
  if (options[["alternative"]]=="less")
    corBayesTable$addFootnote(.getBfTableSidedFootnote(alternative="less", analysis="correlation"))
  
  bfTitle <- .getBfTitle(options[["bayesFactorType"]], options[["alternative"]])
  statName <- switch(options[["method"]],
                     pearson  = gettext("r"),
                     kendall  = gettext("tau"),
                     spearman = gettext("rho")
  )
  
  corBayesTable$addColumnInfo(name = "n", title = gettext("n"), type = "integer")
  corBayesTable$addColumnInfo(name = "stat", title = statName, type = "number")
  corBayesTable$addColumnInfo(name="bf", title=bfTitle, type="number")
  corBayesTable$addColumnInfo(name = "p", title = gettext("p"), type = "number")
  
  if (options[["ci"]]) {
    overTitle <- gettextf("%s%% Credible interval", options[["ciValue"]] * 100)
    corBayesTable$addColumnInfo(name="lowerCi", overtitle=overTitle, type="number", title="Lower")
    corBayesTable$addColumnInfo(name="upperCi", overtitle=overTitle, type="number", title="Upper")
  }
  
  return(corBayesTable)
}

# Prior and Posterior plot ----
.createPlotsCorSumStats <- function(correlationContainer, corModel, options) {
  # a. Get plot container ----- 
  #
  plotContainer <- correlationContainer[["plotContainer"]]
  
  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(title=gettext("Inferential Plots"))
    plotContainer$dependOn("hypothesis")
    plotContainer$position <- 2
    correlationContainer[["plotContainer"]] <- plotContainer
  }
  
  # b. Define dependencies for the plots ----- 
  # For plotPriorPosterior
  # 
  bfPlotPriorPosteriorDependencies <- c("plotPriorAndPosteriorAdditionalInfo", "plotPriorPosteriorAddEstimationInfo", "plotPriorAndPosterior")
  
  if (options[["plotPriorPosteriorAddEstimationInfo"]]) 
    bfPlotPriorPosteriorDependencies <- c(bfPlotPriorPosteriorDependencies, "ciValue")
  
  # For plotBfRobustness
  # 
  bfPlotRobustnessDependencies <- c("plotBayesFactorRobustnessAdditionalInfo", "plotBayesFactorRobustness")
  
  if (options[["plotBayesFactorRobustnessAdditionalInfo"]]) 
    bfPlotRobustnessDependencies <- c(bfPlotRobustnessDependencies, "bayesFactorType")
  
  plotItemDependencies <- list(
    "plotPriorPosterior" = bfPlotPriorPosteriorDependencies,
    "plotBfRobustness" = bfPlotRobustnessDependencies
  )
  
  plotItems <- .getCorPlotItems(options, sumStat=TRUE)
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
      
      if (correlationContainer$getError() || is.null(corModel))
        next
      
      if (item == "plotPriorPosterior")
        plot <- .drawPosteriorPlotCorBayes(correlationContainer, corModel, options, methodItems=options[["method"]], purpose="sumStat")
      else if (item == "plotBfRobustness")
        plot <- .drawBfRobustnessPlotCorBayes(corModel, options, options[["method"]])
      
      .checkAndSetPlotCorBayes(plot, jaspPlotResult)
    } 
  }
}

.oldSumStatsOptionsNames <- c("sampleSize", "correlationCoefficient", 
                              "pearsonRhoValue", "kendallTauValue", "hypothesis", "plotPriorAndPosterior",
                              "plotPriorAndPosteriorAdditionalInfo", "plotBayesFactorRobustness", 
                              "plotBayesFactorRobustnessAdditionalInfo", "priorWidth")

.oldOptionsToNewNames <- list("sampleSize"="n", "correlationCoefficient"="method", 
                             "pearsonRhoValue"="rObs", "kendallTauValue"="tauObs", 
                             "hypothesis"="alternative", 
                             "plotPriorAndPosterior"="plotPriorPosterior",
                             "plotPriorAndPosteriorAdditionalInfo"="plotPriorPosteriorAddTestingInfo", 
                             "plotBayesFactorRobustness"="plotBfRobustness", 
                             "plotBayesFactorRobustnessAdditionalInfo"="plotBfRobustnessAddInfo", 
                             "priorWidth"="kappa")


.renameCorOptions <- function(options) {
  for (currentName in .oldSumStatsOptionsNames) {
    options[[.oldOptionsToNewNames[[currentName]]]] <- 
      switch(currentName, 
             "correlationCoefficient"=switch(options[["correlationCoefficient"]], 
                                             "pearsonRho"="pearson", 
                                             "kendallTau"="kendall", 
                                             "spearman"="spearman"),
             "hypothesis"=switch(options[["hypothesis"]], 
                                 "correlated"="two.sided",
                                 "correlatedPositively"="greater",
                                 "correlatedNegatively"="less"),
             options[[currentName]]
      )
  }
  return(options)
}