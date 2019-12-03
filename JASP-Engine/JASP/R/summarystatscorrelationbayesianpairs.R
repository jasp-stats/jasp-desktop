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

  bfObject <- .computeAndDisplaySumStatCorrelationTable(jaspResults, options, ready)
  # Output plots
  bfObject <- .computeAndDisplaySumStatCorrelationPlots(jaspResults, options, bfObject, ready)

  jaspResults[["bfState"]] <- createJaspState(bfObject, dependencies=c("sampleSize", "correlationCoefficient", 
                                                                       "pearsonRhoValue", "kendallTauValue",
                                                                       "rhoSObs", "priorWidth"))
  # return()
}

# Execute Bayesian binomial test ----
.computeAndDisplaySumStatCorrelationTable <- function(jaspResults, options, ready) {
  # a. Retrieve from state ------
  #
  correlationContainer <- jaspResults[["correlationContainer"]]

  if (is.null(correlationContainer)) {
    correlationContainer <- createJaspContainer()
    correlationContainer$dependOn(c("sampleSize", "correlationCoefficient", "pearsonRhoValue", "kendallTauValue", 
                                    "rhoSObs", "hypothesis", "priorWidth"))
    jaspResults[["correlationContainer"]] <- correlationContainer
  }

  corBayesTable <- correlationContainer[["corBayesTable"]]

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
                      "spearman"=options[["rhoSObs"]])
    
    emptyIshRow <-list("n"=".", "stat"=statObs, "bf"=".", "p"=".")
    
    if (options[["ci"]])
      emptyIshRow <- modifyList(emptyIshRow, list("upperCi"=".", "lowerCi"="."))
      
    corBayesTable$addRows(emptyIshRow)
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
                      "spearman"=options[["rhoSObs"]]
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

  corBayesTable$addCitation(.getCorCitations(options[["method"]], bayes=TRUE))

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
.computeAndDisplaySumStatCorrelationPlots <- function(jaspResults, options, bfObject, ready) {
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
    plotContainer$dependOn(c("plotPriorAndPosterior", "plotBayesFactorRobustness"))
    plotContainer$position <- 2
    jaspResults[["correlationContainer"]][["plotContainer"]] <- plotContainer
  }
  
  # b. Define dependencies for the plots ----- 
  # For plotPriorPosterior
  # 
  bfPlotPriorPosteriorDependencies <- c("plotPriorAndPosteriorAdditionalInfo", "plotPriorPosteriorAddEstimationInfo")
  
  if (options[["plotPriorPosteriorAddEstimationInfo"]]) 
    bfPlotPriorPosteriorDependencies <- c(bfPlotPriorPosteriorDependencies, "ciValue")
  
  # For plotBfRobustness
  # 
  bfPlotRobustnessDependencies <- c("plotBfRobustnessAddInfo")
  
  if (options[["plotBayesFactorRobustnessAdditionalInfo"]]) 
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


.oldSumStatsOptionsNames <- c("sampleSize", "correlationCoefficient", 
                             "pearsonRhoValue", "kendallTauValue", "hypothesis", "plotPriorAndPosterior",
                             "plotPriorAndPosteriorAdditionalInfo", "plotBayesFactorRobustness", 
                             "plotBayesFactorRobustnessAdditionalInfo", "priorWidth")

.renameCorOptions <- function(options) {
  allOptionsNames <- names(options)
  
  for (i in seq_along(allOptionsNames)) {
    currentName <- allOptionsNames[i]

    if (currentName %in% .oldSumStatsOptionsNames) {
      if (currentName=="sampleSize")  {
        options[["n"]] <- options[[currentName]]
      } else if (currentName=="correlationCoefficient") {
        options[["method"]] <- options[[currentName]]
        options[["method"]] <- switch(options[["method"]],
                                      "pearsonRho"="pearson",
                                      "kendallTau"="kendall",
                                      "spearman"="spearman")
      } else if (currentName=="pearsonRhoValue") {
        options[["rObs"]] <- options[[currentName]]
      } else if (currentName=="kendallTauValue") {
        options[["tauObs"]] <- options[[currentName]]
      } else if  (currentName=="hypothesis") {
        options[["alternative"]] <- options[[currentName]]
        options[["alternative"]] <- switch(options[["alternative"]],
                                           "correlated"="two.sided",
                                           "correlatedPositively"="greater",
                                           "correlatedNegatively"="less")
      } else if (currentName=="plotPriorAndPosterior") {
        options[["plotPriorPosterior"]] <- options[[currentName]]
      }


      if (currentName=="plotPriorAndPosteriorAdditionalInfo") {
        options[["plotPriorPosteriorAddTestingInfo"]] <- options[[currentName]]
      }


      if (currentName=="plotBayesFactorRobustness") {
        options[["plotBfRobustness"]] <- options[[currentName]]
      }


      if (currentName=="plotBayesFactorRobustnessAdditionalInfo") {
        options[["plotBfRobustnessAddInfo"]] <- options[[currentName]]
      }


      if (currentName=="priorWidth") {
        options[["kappa"]] <- options[[currentName]]
      }
    }
  }
  
  # tempList <- options[[.oldSumStatsOptionsNames]]
  # names(tempList) <- c("n", "method", "rObs", "tauObs", "method", "plotPriorPosterior", 
  #                      "plotPriorPosteriorAddTestingInfo", "plotBfRobustness", 
  #                      "plotBfRobustnessAddInfo", "kappa")
  # 
  # tempList[["method"]] <- switch(tempList[["method"]], 
  #                                "pearsonRho"="pearson", 
  #                                "kendallTau"="kendall",
  #                                "spearman"="spearman")
  # 
  # tempList[["alternative"]] <- switch(tempList[["alternative"]], 
  #                                    "correlated"="two.sided",
  #                                    "correlatedPositively"="greater",
  #                                    "correlatedNegatively"="less")
  # 
  # options <- modifyList(options, tempList)
  # return(options)
  
  return(options)
}