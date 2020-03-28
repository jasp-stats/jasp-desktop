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

CorrelationBayesian <- function(jaspResults, dataset=NULL, options, ...) {
  ready <- length(options[["variables"]]) >= 2
  pairs <- unique(options[["pairs"]])
  
  # 2. Data retrieval  --------
  #
  if (ready && is.null(dataset))
    dataset <- .corBayesReadData(dataset, options)
  
  corModel <- .computeCorBayes(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready)
  
  # 3. Table: Get, compute and fill -----------
  #
  .createTableCorBayes(jaspResults = jaspResults, corModel = corModel,
                       dataset = dataset, options = options)
  
  # 4. Matrix plot: Get, compute and draw ------
  #
  if (options[["plotMatrix"]])
    .createMatrixPlotCorBayes(jaspResults = jaspResults, corModel = corModel,
                              dataset = dataset, options = options)
  
  # 5. PairsPlot Container: (Optional) ------
  #
  if (options[["plotScatter"]] || options[["plotPriorPosterior"]] || options[["plotBfRobustness"]] || 
      options[["plotBfSequential"]]) {
    pairsPlotCollection <- .getPairPlotsContainerCorBayes(jaspResults, options)
    
    if (!is.null(pairsPlotCollection)) {
      pairsPlotCollection <- .initPlotContainerSubStructureCorBayes(pairsPlotCollection, options)
      
      .fillPairsPlotsCorBayes(jaspResults, pairsPlotCollection, corModel, dataset, options)
    }
  }
}

.computeCorBayes <- function(jaspResults, dataset, options, ready = TRUE) {
  if (!is.null(jaspResults[["corModel"]]))
    return(jaspResults[["corModel"]]$object)
  
  if (!ready)
    return(NULL)
  
  if (options[["missingValues"]] == "excludeListwise")
    .hasErrors(dataset, type="observations", observations.amount='< 2', exitAnalysisIfErrors=TRUE)
  
  result <- list()
  
  pairs <- combn(options[["variables"]], 2, simplify=FALSE)
  
  for (pair in pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    
    # TODO(Alexander) check if they have the same name. Or just let it be, but highly inefficient though
    #
    pairName <- paste(sort(c(var1, var2)), collapse="-")
    
    result[[pairName]] <- list()
    
    for (method in c("pearson", "kendall")) {
      
      errorMsg <- NULL
      dataCheck <- .corBayesCheckPairsErrors(dataset, var1, var2)
      
      if (!identical(dataCheck, FALSE)) {
        errorMsg <- dataCheck[["message"]]
        v1 <- NA
        v2 <- NA
      } else {
        v1 <- dataset[[.v(var1)]]
        v2 <- dataset[[.v(var2)]]
      }
      
      .setSeedJASP(options)
      bfObject <- bstats::bcor.test("x"=v1, "y"=v2, "kappa"=options[["kappa"]],
                                    "method"=method, "ciValue"=options[["ciValue"]])
      
      if (!is.null(errorMsg)) {
        bfObject[["error"]] <- errorMsg
        bfObject[["dataError"]] <- errorMsg
      } else {
        ci <- .computeCorCredibleInterval(bfObject, options[["ciValue"]], method)
        bfObject <- modifyList(bfObject, ci)
      }
      result[[pairName]][[method]] <- bfObject
    }
  }
  
  jaspResults[["corModel"]] <- createJaspState(result)
  jaspResults[["corModel"]]$dependOn(c("missingValues", "variables", "kappa", "ciValue", "setSeed", "seed"))
  
  return(result)
}

.corBayesCheckPairsErrors <- function(dataset, var1, var2) {
  errors <- .hasErrors(dataset, message='short',
                       type=c('observations', 'variance', 'infinity', 'observationsPairwise'),
                       all.target=c(var1, var2), observations.amount='< 2',
                       observationsPairwise.amount=2)
  return(errors)
}

.createTableCorBayes <- function(jaspResults, corModel, dataset, options) {
  if (!is.null(jaspResults[["corBayesTable"]]))
    return()
  
  methodItems <- .getCorMethods(options)
  corBayesTable <- createJaspTable(title=.getCorTableTitle(methodItems, bayes=TRUE))
  corBayesTable$position <- 1
  
  alternative <- options[["alternative"]]
  
  corBayesTable$dependOn(c("pearson", "kendall", "spearman", "alternative", "kappa", "variables",
                           "displayPairwise","reportBayesFactors", "missingValues",
                           "flagSupported", "ci", "ciValue",
                           "reportN", "posteriorMedian", "bayesFactorType",
                           "setSeed", "seed"))
  
  corBayesTable$showSpecifiedColumnsOnly <- TRUE
  corBayesTable$position <- 1
  
  corBayesTable$addCitation(.getCorCitations(methodItems, bayes=TRUE))
  
  if (alternative=="greater")
    corBayesTable$addFootnote(.getBfTableSidedFootnote(alternative="greater", analysis="correlation"))
  
  if (alternative=="less")
    corBayesTable$addFootnote(.getBfTableSidedFootnote(alternative="less", analysis="correlation"))
  
  # Add legend footnote of BFs
  if (options[["flagSupported"]]) {
    tempNote <- .bfFlagTableFootnote(options)
    corBayesTable$addFootnote(message=tempNote, symbol="*")
  }
  
  .addTableColumnMarkupCorBayes(corBayesTable, methodItems, options)
  
  .fillTableCorBayes(corBayesTable, options, corModel)
  
  jaspResults[["corBayesTable"]] <- corBayesTable
}

.addTableColumnMarkupCorBayes <- function(table, methodItems, options) {
  if (options[["displayPairwise"]]) 
    .addPairwiseTableColumnMarkupCorBayes(table, methodItems, options)
  else 
    .addMatrixTableColumnMarkupCorBayes(table, methodItems, options)
}

.addPairwiseTableColumnMarkupCorBayes <- function(table, methodItems, options) {
  # Add the variables names paired
  #
  table$addColumnInfo(name="variable1", title="", type="string")
  table$addColumnInfo(name="separator", title="", type="separator")
  table$addColumnInfo(name="variable2", title="", type="string")
  
  if (options[["reportN"]])
    table$addColumnInfo(name="n", title="n", type="integer")
  
  for (m in seq_along(methodItems)) {
    methodName <- methodItems[m]
    overTitle <- NULL
    
    if (length(methodItems) > 1) {
      # Overwrite the .cormethodNamesList with one that is broken up in overtitle and methodName
      #
      overTitle <- .corOverTitlesList[[methodName]]
      .corMethodNamesList <- list(pearson=gettext("r"), spearman=gettext("rho"), kendall=gettext("tau B"))
    }
    
    # Add's "r", "rho", "tau B"
    table$addColumnInfo(name=paste0(methodName, "stat"), title=.corMethodNamesList[[methodName]],
                        overtitle=overTitle, type="number")
    
    if (options[["reportBayesFactors"]])
      table$addColumnInfo(name=paste0(methodName, "bf"), title=.getBfTitle(options[["bayesFactorType"]], options[["alternative"]]), overtitle=overTitle, type="number")
    
    # # TODO(ALEXANDER): Also report error %? Only useful for mcmc
    # #
    # if (options[["reportPercentageError"]])
    #   table$addColumnInfo(name=paste0(methodName, "bfUq"), title=bfTitle, overtitle=overTitle, type="number")
    
    # TODO(Alexander): Do we want to show the posterior median?
    #
    # if (options[["posteriorMedian"]])
    #   table$addColumnInfo(name=paste0(methodName, "posteriorMedian"), title="Posterior Median", type="number")
    #
    if (options[["ci"]]) {
      table$addColumnInfo(name=paste0(methodName, "lowerCi"), overtitle=overTitle, type="number",
                          title=gettextf("Lower %s%% CI", options[["ciValue"]] * 100))
      
      table$addColumnInfo(name=paste0(methodName, "upperCi"), overtitle=overTitle, type="number",
                          title=gettextf("Upper %s%% CI", options[["ciValue"]] * 100))
    }
  }
  
  # Calculate expected dimension
  #
  nVariables <- length(options[["variables"]])
  if (nVariables <= 1) {
    numberOfRows <- 1
  } else {
    numberOfRows <- choose(nVariables, 2)
  }
  
  if (is.null(methodItems) ||  length(methodItems)==0) {
    numberOfColumns <- 3
  } else {
    numberOfColumns <- 3 + options[["reportN"]] + length(methodItems) * sum(options[["reportBayesFactors"]],
                                                                            2*options[["ci"]])
  }
  
  table$setExpectedSize(numberOfRows, numberOfColumns)
}

.addMatrixTableColumnMarkupCorBayes <- function(table, methodItems, options) {
  # Note(Alexander): Correlation Matrix
  #
  table$addColumnInfo(name="variable", title=gettext("Variable"), type="string", combine=TRUE)
  table$addColumnInfo(name="itemColumn", title="", type="string")
  
  nVariables <- length(options[["variables"]])
  
  if (nVariables < 2) {
    
    if (nVariables == 0) 
      title <- "..."
    else 
      title <- options[["variables"]][1]
    
    table$addColumnInfo(name="firstPlaceholder", type="number", title=title)
    table$addColumnInfo(name="secondPlaceholder", type="number", title="...")
  } else {
    for (variable in options[["variables"]]) 
      table$addColumnInfo(name=variable, type="number",title=variable)
  }
  
  if (length(methodItems)==0) {
    nItems <- sum(options[["reportBayesFactors"]], 2*options[["ci"]], options[["reportN"]])
    
    if (nItems <= 1) {
      numberOfColumns <- nVariables
    } else {
      numberOfColumns <- 2 + nVariables
    }
    numberOfRows <- nVariables
  } else {
    itemsPerMethod <- sum(options[["reportBayesFactors"]], 2*options[["ci"]])
    nVariablesEffective <- max(nVariables, 2)
    
    if (itemsPerMethod==0) {
      numberOfColumns <- 1 + nVariablesEffective
      numberOfRows <- nVariablesEffective
    } else {
      numberOfColumns <- 2 + nVariablesEffective
      numberOfRows <- nVariablesEffective * length(methodItems) *
        (itemsPerMethod + 1) + options[["reportN"]] * nVariablesEffective
    }
  }
  
  table$setExpectedSize(numberOfRows, numberOfColumns)
}

.fillTableCorBayes <- function(table, options, corModel) {
  if (is.null(corModel)) {
    if (options[["displayPairwise"]]) 
      .insertPairwiseDefaultEmptyTableCellsCorBayes(table, options)
    else 
      .insertMatrixDefaultEmptyTableCellsCorBayes(table, options)
  } else {
    if (options[["displayPairwise"]]) 
      .fillPairwiseTableCorBayes(table, options, corModel)
    else 
      .fillMatrixTableCorBayes(table, options, corModel)
  }
}

.insertPairwiseDefaultEmptyTableCellsCorBayes <- function(table, options) {
  nVariables <- length(options[["variables"]])
  if (nVariables < 2) {
    if (nVariables == 0) 
      var1 <- "..."
    else 
      var1 <- options[["variables"]][1]
    
    var2 <- "..."
    table$addRows(list(variable1=var1, separator="\u2014", variable2=var2))
  }
}

.insertMatrixDefaultEmptyTableCellsCorBayes <- function(table, options) {
  variables <- unlist(options[["variables"]])
  
  if (length(variables) == 1) 
    variables <- c(paste("1.", variables), "2. ...")
  else 
    variables <- c("1. ...", "2. ...")
  
  methodItems <- .getCorMethods(options)
  itemNames <- .bSelectItems(options)
  
  # TODO(Alexander): fix length is better
  statColumnPerVar <- c()
  
  for (methodItem in methodItems) {
    statsToReport <- unlist(.bCorRowNames(options, itemNames, method=methodItem))
    statsToReport <- statsToReport[statsToReport != gettext("n")] # we want to only report this once per variable. probably better solved in .bCorRowNames() tho
    statColumnPerVar <- c(statColumnPerVar, statsToReport)
  }
  
  if (options[["reportN"]])
    statColumnPerVar <- c(gettext("n"), statColumnPerVar)
  
  emptyCells <- matrix(c("\u2014", "", ".", "\u2014"), nrow=2, byrow=TRUE)
  
  for (i in seq_along(variables)) {
    for (j in seq_along(statColumnPerVar)) {
      table$addRows(list(.isNewGroup = (i == 2 && j == 1),
                         variable = variables[i],
                         itemColumn = statColumnPerVar[j],
                         firstPlaceholder = emptyCells[i, 1],
                         secondPlaceholder = emptyCells[i, 2]))
    }
  }
}

.fillPairwiseTableCorBayes <- function(table, options, corModel) {
  pairs <- combn(options[["variables"]], 2, simplify=FALSE)
  
  for (pair in pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    
    pairName <- paste(sort(c(var1, var2)), collapse="-")
    tempRow <- list("variable1"=var1, "separator"="-", "variable2"=var2)
    
    itemNames <- .bSelectItems(options)
    
    for (method in .getCorMethods(options)) {
      bfObject <- corModel[[pairName]][[method]]
      
      sidedObject <- bstats::getSidedObject(bfObject, alternative=options[["alternative"]],
                                            itemNames=itemNames)
      rowObject <- sidedObject[itemNames]
      
      sampleSize <- rowObject[["n"]]
      reportBf <- rowObject[["bf"]]
      
      if (options[["reportBayesFactors"]]) {
        if (options[["bayesFactorType"]] == "BF01") 
          rowObject[["bf"]] <- 1/reportBf
        else if (options[["bayesFactorType"]] == "LogBF10") 
          rowObject[["bf"]] <- log(reportBf)
      }
      
      names(rowObject) <- paste0(method, itemNames)
      
      if (!is.null(sampleSize))
        rowObject[["n"]] <- sampleSize
      
      errorMessage <- bfObject[["error"]]
      
      if (!is.null(errorMessage)) 
        table$addFootnote(errorMessage, rowNames = pairName, colNames = paste0(method, "stat"))
      else if (options[["reportBayesFactors"]] && options[["flagSupported"]] && !is.na(reportBf)) 
        .addSupportedBfIndicators(table, reportBf, rowNames = pairName, colNames = paste0(method, "stat"))
      
      tempRow <- modifyList(tempRow, rowObject)
    }
    table$addRows(tempRow, rowNames=pairName)
  }
}

.fillMatrixTableCorBayes <- function(table, options, corModel) {
  nVariables <- length(options[["variables"]])
  
  for (i in 1:nVariables) {
    emptyCellInfo <- vector("list", length=i)
    
    var1 <- options[["variables"]][i]
    var2Names <- options[["variables"]][1:i]
    
    # Note(Alexander): Dash on the diagonal
    # 
    emptyCellInfo[[i]] <- "\u2014"
    
    itemNames <- .bSelectItems(options)
    methodItems <- .getCorMethods(options)
    
    for (m in seq_along(.getCorMethods(options))) {
      # Row info collected here
      #
      methodName <- methodItems[m]
      allItemInfo <- vector("list", length=length(itemNames))
      names(allItemInfo) <- itemNames
      
      for (item in itemNames)
        allItemInfo[[item]] <- emptyCellInfo
      
      # Note(Alexander): RETRIEVE info from state here
      #
      if (i > 1) {
        for (j in 1:(i-1)) {
          var2 <- var2Names[j]
          pairName <- paste(sort(c(var1, var2)), collapse="-")
          
          # Note(Alexander): Here use actual data to fill the table
          #
          bfObject <- corModel[[pairName]][[methodName]]
          
          sidedObject <- bstats::getSidedObject(bfObject, alternative=options[["alternative"]],
                                                itemNames=itemNames)
          reportBf <- sidedObject[["bf"]]
          
          if (options[["bayesFactorType"]] == "BF01") 
            sidedObject[["bf"]] <- 1/reportBf
          else if (options[["bayesFactorType"]] == "LogBF10") 
            sidedObject[["bf"]] <- log(reportBf)
          
          
          # Here add to info to the collection
          # 
          for (item in itemNames) 
            allItemInfo[[item]][j] <- sidedObject[[item]]
          
          errorMessage <- bfObject[["error"]]
          rowName <- paste0(var1, methodName, "stat")
          
          if (!is.null(errorMessage)) 
            table$addFootnote(errorMessage, rowNames = rowName, colNames = var2)
          else if (options[["reportBayesFactors"]] && options[["flagSupported"]] && !is.na(reportBf)) 
            .addSupportedBfIndicators(table, reportBf, rowNames = rowName, colNames = var2)
          
          
        }
      }
      
      # Note(Alexander): Now fill each row. For the ith variable the structure is
      #     variable=var1,
      #     itemColumn,
      #     var2Name[1],
      #     var2Name[2],
      #     ...,
      #     var2Name[i-1],
      #     var2Name[i]
      #
      # To be used for the column "itemName"
      #
      if (m > 1 && options[["reportN"]]) {
        allItemInfo[["n"]] <- NULL
        
        # Note(Alexander): After the first time we added "n", remove it from the other
        # 
        itemNames <- setdiff(itemNames, "n")
      }
      
      rowItemNames <- .bCorRowNames(options, itemNames, method = methodName)
      
      for (k in seq_along(itemNames)) {
        if (k == 1 && m == 1) 
          tempRow <- list(variable = paste0(i, ". ", var1), itemColumn = rowItemNames[[k]], .isNewGroup = TRUE) # First item gets the variable name
        else 
          tempRow <- list(variable = "", itemColumn = rowItemNames[[k]], .isNewGroup = FALSE)
        
        itemInfo <- as.list(allItemInfo[[k]])
        
        names(itemInfo) <- var2Names
        tempRow <- modifyList(tempRow, itemInfo)
        table$addRows(tempRow, rowNames = paste0(var1, methodName, itemNames[k]))
      } # End items loop
    } # End loop over the methodItems
  } # End var1 loop
}

.addSupportedBfIndicators <- function(table, bf, rowNames, colNames) {
  symbol <- NULL
  
  if (bf >= 100) 
    symbol <- "***"
  else if (bf >= 30) 
    symbol <- "**"
  else if (bf >= 10) 
    symbol <- "*"
  
  if (!is.null(symbol))
    table$addFootnote(symbol = symbol, rowNames=rowNames, colNames=colNames)
}

.createMatrixPlotCorBayes <- function(jaspResults, corModel, dataset, options) {
  # a. Retrieve from state ----
  #
  if (!is.null(jaspResults[["matrixPlot"]]))
    return()
  
  nVariables <- length(options[["variables"]])
  
  # b. Compute  -----
  #
  methodItems <- .getCorMethods(options)
  
  # c. Get ----
  #
  matrixPlot <- createJaspPlot(title=gettext("Bayesian Correlation Matrix Plot"))
  matrixPlot$position <- 2
  
  matrixDependencies <- c("variables", "plotMatrix", "plotMatrixDensities", "plotMatrixPosteriors", "missingValues", "setSeed", "seed")
  
  if (options[["plotMatrixPosteriors"]])
    matrixDependencies <- c(matrixDependencies, "pearson", "spearman", "kendall", "alternative", "kappa")
  
  matrixPlot$dependOn(matrixDependencies)
  
  jaspResults[["matrixPlot"]] <- matrixPlot
  
  if (!is.null(corModel)) {
    # d. Draw  -----
    #
    if (nVariables <= 2 && (options[["plotMatrixDensities"]] ||  options[["plotMatrixPosteriors"]])) {
      matrixPlot[["width"]] <- 580
      matrixPlot[["height"]] <- 580
    } else if (nVariables <= 2) {
      matrixPlot[["width"]] <- 400
      matrixPlot[["height"]] <- 400
    } else {
      matrixPlot[["width"]] <- 250 * nVariables + 20
      matrixPlot[["height"]] <- 250 * nVariables + 20
    }
    
    if (length(methodItems) != 0 && options[["plotMatrix"]]) 
      .drawMatrixPlotCorBayes(jaspResults, matrixPlot, corModel, dataset, options)
  }
}

.drawMatrixPlotCorBayes <- function(jaspResults, matrixPlot, corModel, dataset, options) {
  methodItems <- .getCorMethods(options)
  vars <- options[["variables"]]
  nVariables <- length(vars)
  plotMat <- matrix(list(), nVariables, nVariables)
  
  for (row in seq_len(nVariables-1)) {
    for (col in row:nVariables) {
      var1 <- vars[row]
      var2 <- vars[col]
      
      pairName <- paste(sort(c(var1, var2)), collapse="-")
      
      if (row < col) {
        if (!options[["plotMatrixPosteriors"]]) {
          posteriorPlot <- list()
        } else {
          posteriorPlot <- .drawPosteriorPlotCorBayes(jaspResults, corModel, options, methodItems, purpose="matrix", 
                                                      pairName)
        }
        plotMat[[col, row]] <- posteriorPlot
        
        dataError <- corModel[[pairName]][[1]][["dataError"]]
        
        if (!is.null(dataError)) {
          # TODO(Alexander): This is probably not run here anyways, but try avoid displayerror
          scatterPlot <- .displayError(errorMessage=dataError)
        } else {
          subData <- dataset[, .v(c(var1, var2)), drop=FALSE]
          subData <- subData[complete.cases(subData), , drop=FALSE]
          scatterPlot <- .bCorScatter(x=subData[[.v(var2)]], y=subData[[.v(var1)]], options)
        }
        
        plotMat[[row, col]] <- scatterPlot
      }
      
      if (row == col) {
        if (!options[["plotMatrixDensities"]]) {
          densityPlot <- list()
        } else {
          densityPlot <-try(.bCorMarginalDistribution(variable = dataset[, .v(vars[row]), drop=TRUE],
                                                      varName = vars[row], options = options)
          )
          
          if (isTryError(densityPlot))
            # TODO(Alexander): again with error case for matrix
            densityPlot <- .displayError(errorMessage=.extractErrorMessage(densityPlot))
          
        }
        plotMat[[row, row]] <- densityPlot
      }
    }
  }
  
  if (options[["plotMatrixDensities"]]) {
    densityPlot <- try(.bCorMarginalDistribution(variable = dataset[, .v(vars[nVariables]), drop=TRUE],
                                                 varName = vars[nVariables], options = options)
    )
    
    if (isTryError(densityPlot))
      # TODO(Alexander): again with error case for matrix
      densityPlot <- .displayError(errorMessage=.extractErrorMessage(densityPlot))
    
    plotMat[[nVariables, nVariables]] <- densityPlot
  }
  
  
  # TODO(Alexander): Stolen from Descriptives.R
  # slightly adjust the positions of the labels left and above the plots.
  # 
  labelPos <- matrix(.5, 4, 2)
  labelPos[1, 1] <- .55
  labelPos[4, 2] <- .65
  
  obj <- try(JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = vars, topLabels = vars,
                                      scaleXYlabels = 0.9, labelPos=labelPos))
  
  if (isTryError(obj)) {
    matrixPlot$setError(.extractErrorMessage(obj))
    # plotResult$setError(.extractErrorMessage(obj))
  } else {
    matrixPlot$plotObject <- obj
    # plotResult <- obj
  }
  return()
}

.drawPosteriorPlotCorBayes <- function(jaspResults, corModel, options, methodItems,
                                       purpose=c("matrix", "pairs", "sumStat"), pairName = NULL) {
  purpose <- match.arg(purpose)
  
  plotResult <- ""
  
  # TODO(Alexander): Check whether we can do .setErrorMessages() to plots in correlation matrix plot
  
  if (purpose=="matrix") {
    if (!options[["plotMatrixPosteriors"]]) {
      # Note(Alexander): This should never happen as, this shouldn't be called with plotMatrixPosteriors <- TRUE
      # TODO(Alexander) return empty list instead?
      plotResult <- .displayError(errorMessage="")
      return(plotResult)
    }
  } else if (purpose=="pairs" ||  purpose=="sumStat") {
    if (!is.null(options[["plotPriorPosteriors"]])) {
      # Note(Alexander): This should never happen as, this shouldn't be called with plotMatrixPosteriors <- TRUE
      # TODO(Alexander) return empty list instead?
      plotResult <- .displayError(errorMessage="")
      return(plotResult)
    }
  }
  
  if (purpose == "sumStat") {
    pairStats <- list()
    pairStats[[options[["method"]]]] <- corModel
  } else {
    pairStats <- corModel[[pairName]]
  }
  
  if (purpose %in% c("matrix", "pairs")) 
    dataError <- pairStats[[1]][["error"]]
  else if (purpose=="sumStat") 
    dataError <- pairStats[["error"]]
  
  if (!is.null(dataError)) {
    
    if (purpose=="matrix") 
      plotResult <- .displayError(errorMessage=dataError)
    else 
      plotResult <- dataError
    
    return(plotResult)
  }
  
  # a. No data errors, try to plot ----
  #
  postPlotValuesPerMethod <- .getPosteriorPlotValuesCorBayes(jaspResults, pairStats, options, pairName)
  
  for (i in seq_along(postPlotValuesPerMethod)) {
    postPlotValues <- postPlotValuesPerMethod[[i]]
    error <- postPlotValues[["error"]]
    
    if (postPlotValues[["tooPeaked"]]) {
      error <- gettext("Posterior is too peaked")
      break()
    }
    posteriorLine <- postPlotValues[["posteriorLine"]]
    
    if (is.null(posteriorLine) ||  length(posteriorLine) == 1) {
      # TODO(Alexander): This covers the case when posteriorLine <- "Could not compute posterior"
      # RETURN FAILED PLOT
      error <- gettext("Could not compute the posteriors")
      break()
    }
    
  }
  
  # b. Check for error ----
  #
  if (!is.null(error)) {
    
    if (purpose=="matrix") 
      plotResult <- .displayError(errorMessage=error)
    else 
      plotResult <- error
    
    return(plotResult)
  }
  
  # c. No errors, combine plot info ------
  #
  
  xDomain <- postPlotValuesPerMethod[[1]][["xDomain"]] # these values are the same for all methods
  domainLength <- length(xDomain) 
  
  dfPoints <- NULL
  BF10 <- NULL
  CRI <- NULL
  medianPoint <- NULL
  CRItxt <- NULL
  
  if (purpose == "matrix") {
    xName <- NULL
    
    if (length(methodItems) == 1)  {
      # xName <- unlist(.corXNames[methodItems], recursive = FALSE)
      # 
      dfLines <- data.frame(
        x = xDomain,
        y = postPlotValuesPerMethod[[1]][["posteriorLine"]]
      )
    } else {
      # xName <- "Correlation Coefficient"
      # 
      gLegend <- unlist(.corGLegendList[methodItems], use.names = FALSE, recursive = FALSE)
      dfLines <- data.frame(
        x = xDomain, 
        y = unlist(lapply(postPlotValuesPerMethod, function(x) return(x[["posteriorLine"]]))),
        g = rep(gLegend, each=domainLength)
      )
    }
  } else if (purpose %in% c("pairs", "sumStat")) {
    if (purpose=="sumStat") 
      postPlotValues <- postPlotValuesPerMethod[[1]]
    else if (purpose=="pairs") 
      postPlotValues <- postPlotValuesPerMethod[[options[["pairsMethod"]]]]
    
    
    xName <- unlist(.corXNames[[methodItems]], recursive = FALSE)
    gLegend <- c(gettext("Prior"), gettext("Posterior"))
    dfLines <- data.frame(
      x = xDomain,
      y = c(postPlotValues[["priorLine"]], postPlotValues[["posteriorLine"]]),
      g = rep(gLegend, each=domainLength)
    )
    
    if (isTRUE(options[["plotPriorPosteriorAddEstimationInfo"]])) {
      if (postPlotValues[["ciValue"]] != options[["ciValue"]]) {
        if (purpose=="pairs") {
          methodName <- options[["pairsMethod"]]
          sidedBfWithoutPost <- corModel[[pairName]][[methodName]][[options[["alternative"]]]]
        } else if (purpose=="sumStat") {
          methodName <- options[["method"]]
          sidedBfWithoutPost <- corModel[[options[["alternative"]]]]
        }
        # Note(Alexander): Not stored
        # 
        lowerCi <- sidedBfWithoutPost[["lowerCi"]]
        upperCi <- sidedBfWithoutPost[["upperCi"]]
        medianPoint <- sidedBfWithoutPost[["posteriorMedian"]]
      }  else {
        lowerCi <- postPlotValues[["lowerCi"]]
        upperCi <- postPlotValues[["upperCi"]]
        medianPoint <- postPlotValues[["posteriorMedian"]]
      }
      
      ciValue <- options[["ciValue"]]
      CRI <- c(lowerCi, upperCi)
      CRItxt <- gettextf("%s%% CI:", ciValue * 100)
    }
    
    if (isTRUE(options[["plotPriorPosteriorAddTestingInfo"]])) {
      dfPoints <- data.frame(
        x = c(postPlotValues[["h0"]], postPlotValues[["h0"]]),
        y = c(postPlotValues[["priorAtH0"]], postPlotValues[["posteriorAtH0"]]),
        g = c(gettext("Prior"), gettext("Posterior"))
      )
      BF10 <- postPlotValues[["bf"]]
    }
  }
  
  hypothesis <- switch(options[["alternative"]], 
                       "two.sided"="equal",
                       "greater"="greater",
                       "less"="smaller"
  )
  plotResult <- try(JASPgraphs::PlotPriorAndPosterior(
    dfLines, dfPoints, BF10, "CRI" = CRI, "CRItxt" = CRItxt, "median" = medianPoint, "xName" = xName, 
    "hypothesis" = hypothesis, bfType = "BF10")
  )
  return(plotResult)
}

.getPosteriorPlotValuesCorBayes <- function(jaspResults, bfObject, options, pair = NULL) {
  indexName <- paste0("posteriorLine-", pair)
  
  if (!is.null(jaspResults[[indexName]]))
    return(jaspResults[[indexName]]$object)
  
  results <- list()
  
  for (method in names(bfObject)){
    .setSeedJASP(options)
    results[[method]] <- bstats::computeCorPosteriorLine(bfObject = bfObject[[method]], 
                                                         alternative = options[["alternative"]])
  }
  
  jaspResults[[indexName]] <- createJaspState(results)
  
  if (is.null(pair)) # sumStats
    qmlInputElements <- c("hypothesis", "priorWidth", "setSeed", "seed")
  else # pairs
    qmlInputElements <- c("missingValues", "alternative", "kappa", "setSeed", "seed")
  
  jaspResults[[indexName]]$dependOn(qmlInputElements)
  
  return(results)
}

.getPairPlotsContainerCorBayes <- function(jaspResults, options) {
  if (!.readyToCreatePairPlotsCorBayes(options))
    return(NULL)
  
  pairsPlotCollection <- jaspResults[["pairsPlotCollection"]]
  
  if (is.null(pairsPlotCollection)) {
    pairsPlotCollection <- createJaspContainer(title=gettext("Bayesian Correlation Pairwise Plots"))
    pairsPlotCollection$dependOn("missingValues")
    pairsPlotCollection$position <- 3
    jaspResults[["pairsPlotCollection"]] <- pairsPlotCollection
  }
  return(pairsPlotCollection)
}

.readyToCreatePairPlotsCorBayes <- function(options) {
  pairsIndeces <- .getPairsIndeces(options)
  
  if (length(pairsIndeces) == 0)
    return(FALSE)
  
  plotItems <- .getCorPlotItems(options, bayes=TRUE,  sumStat=FALSE)
  
  if (length(plotItems) == 0)
    return(FALSE)
  
  return(TRUE)
}

.initPlotContainerSubStructureCorBayes <- function(pairsPlotCollection, options) {
  plotItems <- .getCorPlotItems(options, bayes=TRUE,  sumStat=FALSE)
  
  bfPlotPriorPosteriorDependencies <- c("pairsMethod", "kappa", "alternative", "setSeed", "seed")
  bfPlotDependencies <- c(bfPlotPriorPosteriorDependencies, "bayesFactorType")
  
  if (options[["plotPriorPosterior"]] && options[["plotPriorPosteriorAddEstimationInfo"]])
    bfPlotPriorPosteriorDependencies <- c(bfPlotPriorPosteriorDependencies, "ciValue")
  
  plotItemDependencies <- list(
    "plotScatter"=c("plotScatter", "plotScatterAddInfo"),
    "plotPriorPosterior"=c("plotPriorPosterior", bfPlotPriorPosteriorDependencies,
                           "plotPriorPosteriorAddTestingInfo", "plotPriorPosteriorAddEstimationInfo"),
    "plotBfRobustness"=c("plotBfRobustness", "plotBfRobustnessAddInfo", bfPlotDependencies),
    "plotBfSequential"=c("plotBfSequential", "plotBfSequentialAddInfo", bfPlotDependencies)
  )
  
  pairs <- options[["pairs"]]
  
  for (i in seq_along(pairs)) {
    # Loop over plotItems
    pair <- pairs[[i]]
    var1 <- pair[1]
    var2 <- pair[2]
    
    # TODO(Alexander): Think about when var1==var2
    #
    pairName <- paste(sort(c(var1, var2)), collapse="-")
    pairContainerTitle <- paste(var1, var2, sep=" - ")
    
    pairContainer <- pairsPlotCollection[[pairName]]
    
    if (is.null(pairContainer)) {
      pairContainer <- createJaspContainer(title = pairContainerTitle)
      pairContainer$dependOn(optionContainsValue = list(pairs = unname(pair)))
      pairsPlotCollection[[pairName]] <- pairContainer
    }
    
    for (i in seq_along(plotItems)) {
      item <- plotItems[i]
      itemTitle <- .bfPlotTitles[[item]]
      itemPlot <- pairContainer[[item]]
      
      if (is.null(itemPlot)) {
        itemPlot <- createJaspPlot(title=itemTitle, width=530, height=400)
        itemPlot$dependOn(options = plotItemDependencies[[item]])
        
        itemPlot$position <- switch(item, 
                                    "plotScatter"=1,
                                    "plotPriorPosterior"=2,
                                    "plotBfRobustness"=3,
                                    "plotBfSequential"=4)
        
        pairContainer[[item]] <- itemPlot
      }
    }
  }
  
  return(pairsPlotCollection)
}

.fillPairsPlotsCorBayes <- function(jaspResults, pairsPlotCollection, corModel, dataset, options) {
  plotItems <- .getCorPlotItems(options, bayes=TRUE,  sumStat=FALSE)
  pairs <- options[["pairs"]]
  
  # TODO(Alexander): Get indeces for useable pairs instead
  pairsIndeces <- .getPairsIndeces(options)
  alternative <- options[["alternative"]]
  thisMethod <- options[["pairsMethod"]]
  
  for (i in pairsIndeces) {
    currentPair <- pairs[[i]]
    var1 <- currentPair[1]
    var2 <- currentPair[2]
    
    pairName <- paste(sort(c(var1, var2)), collapse="-")
    
    pairContainer <- pairsPlotCollection[[pairName]]
    pairStats <- corModel[[pairName]]
    dataError <- pairStats[[1]][["dataError"]]
    
    if (!is.null(dataError)) {
      pairContainer$setError(dataError)
      next
    }
    
    for (i in seq_along(plotItems)) {
      item <- plotItems[i]
      jaspPlotResult <- pairContainer[[item]]
      plotResult <- jaspPlotResult$plotObject
      
      if (is.null(plotResult)) {
        jaspPlotResult$status <- "running"
        
        plot <- NULL
        if (item == "plotScatter") {
          subData <- dataset[, .v(c(var1, var2)), drop=FALSE]
          subData <- subData[complete.cases(subData), , drop=FALSE]
          plot <- try(.bCorScatter(x=subData[[.v(var1)]], y=subData[[.v(var2)]], xName=var1, yName=var2, options))
        } else if (item == "plotPriorPosterior") {
          plot <- .drawPosteriorPlotCorBayes(jaspResults, corModel, options, thisMethod, purpose="pairs", pairName)
        } else if (item == "plotBfRobustness") {
          plot <- .drawBfRobustnessPlotCorBayes(pairStats[[thisMethod]], options, thisMethod)
        } else if (item == "plotBfSequential") {
          plot <- .drawBfSequentialPlotCorBayes(dataset[[.v(var1)]], dataset[[.v(var2)]], pairStats[[thisMethod]], 
                                                options)
        }
        .checkAndSetPlotCorBayes(plot, jaspPlotResult)
      }
    }
  }
  return()
}

.drawBfRobustnessPlotCorBayes <- function(bfObject, options, method) {
  .setSeedJASP(options)
  robustnessValuesPerHypothesis <- bstats::computeCorRobustnessLine(bfObject)
  robustnessValues <- bstats::getSidedObject(robustnessValuesPerHypothesis, alternative=options[["alternative"]], 
                                             itemNames=c("kappaDomain", "kappa"))
  robustnessLine <- robustnessValues[["robustnessLine"]]
  
  if (is.character(robustnessLine)) {
    plotResult <- robustnessLine
  } else {
    xLine <- robustnessValues[["kappaDomain"]]
    yLine <- robustnessLine
    xPoint <- NULL
    yPoint <- NULL
    
    if (isTRUE(options[["plotBfRobustnessAddInfo"]])) {
      maxBf <- robustnessValues[["robustnessMaxBf"]]
      kappaOfMaxBf <- robustnessValues[["robustnessKappaOfMaxBf"]]
      userBf <- bfObject[[options[["alternative"]]]][["bf"]]
      userKappa <- bfObject[["kappa"]]
      
      xPoint <- c(kappaOfMaxBf, userKappa)
      yPoint <- c(maxBf, userBf)
    }
    
    plotResult <- .plotRobustnessCor(xLine=xLine, yLine=yLine, xPoint=xPoint, yPoint=yPoint, 
                                     bfType=options[["bayesFactorType"]], alternative=options[["alternative"]], 
                                     nDigits=4)
  }
  return(plotResult)
}

.plotRobustnessCor <- function(xLine, yLine, xPoint, yPoint, bfType, alternative, nDigits=4, 
                               pointColors=c("red", "grey", "black", "white"), pointFill=c("grey", "black", "white")) {
  # TODO(Alexander): Perhaps add an argument
  #     category=c("max", "user")
  # 
  bfPlotType <- switch(bfType,
                       "BF10"="BF10",
                       "LogBF10"="BF10",
                       "BF01"="BF01"
  )
  
  logYLine <- try(log(yLine))
  
  if (isTryError(logYLine)) 
    return(logYLine)
  
  if (bfType=="BF01")
    logYLine <- -logYLine
  
  dfLines <- data.frame(
    x = xLine,
    y = logYLine
  )
  
  hypothesisJASPgraphsName <- switch(alternative, 
                                     "two.sided"="equal",
                                     "greater"="greater",
                                     "less"="smaller")
  
  if (is.null(xPoint)) {
    dfPoints <- NULL
    pointColors <- NULL
    pointFill <- NULL
  } else {
    nPoints <- length(xPoint)
    pointColors  <- pointColors[1:nPoints]
    pointFill  <- pointFill[1:nPoints]
    
    bfLegendLabel <- JASPgraphs::getBFSubscripts(bfPlotType, hypothesis=hypothesisJASPgraphsName)[1]
    legendText1 <- vector("character", length(xPoint))
    legendText2 <- vector("character", length(xPoint))
    
    for (i in seq_along(xPoint)) {
      if (i==1) {
        legendText1[i] <- gettextf("max %s", bfLegendLabel)
        legendText1[i] <- gsub(pattern = "\\s+", "~", legendText1[i])
        legendText2[i] <- gettextf("%s at kappa==%s", format(yPoint[i], digits=nDigits), format(xPoint[i], digits=nDigits))
        legendText2[i] <- gsub(pattern = "\\s+", "~", legendText2[i])
      } else if (i==2) {
        legendText1[i] <- gettext("user prior")
        legendText1[i] <- paste0("\"", legendText1[i], "\"")
        legendText2[i] <- gettextf("%s at kappa==%s", format(yPoint[i], digits=nDigits), format(xPoint[i], digits=nDigits))
        legendText2[i] <- gsub(pattern = "\\s+", "~", legendText2[i])
      }
      # TODO(Alexander): Do something here, with user-prior, wide etc etc. Also loop over colours
    }
    legendText1 <- paste0("paste(", legendText1, ", ':')")
    
    # TODO(Alexander): Try and think of how to default
    logYPoint <- log(.recodeBFtype(yPoint, newBFtype = bfPlotType, oldBFtype = "BF10"))
    
    dfPoints <- data.frame(
      x = xPoint,
      y = logYPoint,
      g = legendText1,
      label1 = JASPgraphs::parseThis(legendText1),
      label2 = JASPgraphs::parseThis(legendText2),
      stringsAsFactors = FALSE
    )
  }
  
  
  plotResult <- try(JASPgraphs::PlotRobustnessSequential(
    dfLines         = dfLines,
    xName           = bquote(paste(.(gettext("Stretched beta prior width")), ~kappa)),
    dfPoints        = dfPoints,
    bfType          = bfPlotType,
    pointColors     = pointColors,
    pointFill       = pointFill,
    hypothesis      = hypothesisJASPgraphsName,
    plotLineOrPoint = "line"
  ))
  
  return(plotResult)
}

.drawBfSequentialPlotCorBayes <- function(v1, v2, bfObject, options) {
  .setSeedJASP(options)
  sequentialValuesPerHyp <- bstats::computeCorSequentialLine(x=v1, y=v2, bfObject=bfObject)
  sequentialValues <- bstats::getSidedObject(sequentialValuesPerHyp, alternative=options[["alternative"]], 
                                             itemNames="nDomain")
  
  sequentialLine <- sequentialValues[["sequentialLine"]]
  
  if (is.character(sequentialLine)) {
    return(sequentialLine)
  } else {
    y <- log(sequentialLine)
    
    if (options[["bayesFactorType"]]=="BF01") 
      y <- -y
    
    dfLines <- data.frame(
      x = sequentialValues[["nDomain"]],
      y = y
    )
    
    BF <- NULL
    hypothesis <- switch(options[["alternative"]],
                         "two.sided"="equal",
                         "greater"="greater",
                         "less"="smaller"
    )
    
    bfType <- switch(options[["bayesFactorType"]],
                     "LogBF10"="BF10",
                     "BF10"="BF10",
                     "BF01"="BF01"
    )
    
    if (options[["plotBfSequentialAddInfo"]]) {
      BF <- bfObject[[options[["alternative"]]]][["bf"]]
      BF <- .recodeBFtype(BF, newBFtype = bfType, oldBFtype = "BF10")
    }
    
    plotResult <- try(JASPgraphs::PlotRobustnessSequential(
      dfLines      = dfLines,
      xName        = gettext("n"),
      BF           = BF,
      bfType       = bfType,
      hypothesis   = hypothesis
    ))
  }
  return(plotResult)
}

.checkAndSetPlotCorBayes <- function(triedPlot, jaspPlotResult) {
  if (isTryError(triedPlot)) {
    jaspPlotResult$setError(.extractErrorMessage(triedPlot))
  } else if (is.character(triedPlot)) {
    jaspPlotResult$setError(triedPlot)
  } else {
    jaspPlotResult$plotObject <- triedPlot
  }
}

.computeCorCredibleInterval <- function(bfObject, ciValue, method) {
  if (method == "pearson") {
    return(bstats::computePearsonCredibleInterval("betaA"=bfObject[["betaA"]], "betaB"=bfObject[["betaB"]],
                                                  "ciValue"=ciValue))
  } else if (method == "kendall") {
    return(bstats::computeKendallCredibleInterval("n"=bfObject[["two.sided"]][["n"]], "tauObs"=bfObject[["two.sided"]][["stat"]], 
                                                  "kappa"=bfObject[["kappa"]], "var"=1, "ciValue"=ciValue, 
                                                  "h0"=bfObject[["h0"]]))
  } else if (method == "spearman") {
    # TODO(Johnny):
    print("NO THIS IS NOT IT, STILL NEED TO MAKE THIS, THIS IS JUST A PLACEHOLDER")
    return(bstats::computePearsonCredibleInterval("betaA"=bfObject[["betaA"]], "betaB"=bfObject[["betaB"]],
                                                  "ciValue"=ciValue))
  }
}
