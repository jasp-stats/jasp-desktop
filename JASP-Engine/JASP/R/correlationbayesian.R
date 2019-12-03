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

CorrelationBayesian <- function(jaspResults, dataset=NULL, options, ...) {
  readyMatrix <- length(options[["variables"]]) >= 2
  options[["pairs"]] <- unique(options[["pairs"]])
  readyPairs <- .checkPairsReady(options)

  # 1. State retrieveal  ----
  #
  bfState <- jaspResults[["bfState"]]
  
  if (is.null(jaspResults[["bfState"]])) {
    bfState <- createJaspState()
  } 
  
  allBfObjects <- bfState$object

  # 2. Data retrieval  --------
  #
  if (readyMatrix) {
    if (is.null(dataset)) 
      dataset <- .corBayesReadData(dataset, options)
  }

  # 3. Table: Get, compute and fill -----------
  #
  allBfObjects <- .computeAndDisplayTableCorBayes("jaspResults"=jaspResults, "allBfObjects"=allBfObjects,
                                     "dataset"=dataset, "options"=options, "ready"=readyMatrix)

  # 4. Matrix plot: Get, compute and draw ------
  #
  allBfObjects <- .computeAndDisplayMatrixPlot("jaspResults"=jaspResults,
                                  "dataset"=dataset, "options"=options,
                                  "allBfObjects"=allBfObjects, "ready"=readyMatrix)

  # 5. PairsPlot Container: (Optional) ------
  #
  pairsPlotCollection <- .getContainerCorBayes("jaspResults"=jaspResults,
                                               "container"="pairsPlot",
                                               "ready"=readyPairs)
  if (!is.null(pairsPlotCollection)) {
    pairsPlotCollection <- .setPairCollectionCorBayes(pairsPlotCollection,
                                                      options, readyPairs)

    allBfObjects <- .computeCorBayes("allBfObjects"=allBfObjects,"dataset"=dataset,
                                     "options"=options, "computePlots"=TRUE,
                                     "purpose"="pairs", "ready"=readyPairs)

    .fillPairsPlotCorBayes(pairsPlotCollection, allBfObjects, dataset, options)
  }

  # 6. allBfObjects in state -------
  #
  if (options[["missingValues"]]=="excludeListwise") {
    stateDependencies <- c("missingValues", "kappa", "variables")
  } else if (options[["missingValues"]]=="excludePairwise") {
    stateDependencies <- c("missingValues", "kappa")
  }
  
  jaspResults[["bfState"]] <- createJaspState(allBfObjects, dependencies=stateDependencies)
}

# Change with 
.checkPairsReady <- function(options) {
  pairsIndeces <- .getPairsIndeces(options)

  if (length(pairsIndeces) == 0)
    return(FALSE)

  plotItems <- .getCorPlotItems(options, bayes=TRUE,  sumStat=FALSE)

  if (length(plotItems)==0)
    return(FALSE)

  return(TRUE)
}

.getContainerCorBayes <- function(jaspResults, container="matrix", ready=TRUE) {
  if (container=="matrix") {
    # Note(Alexander) 
    # This is now redundant, but then again I can use this to add the table and correlation plot into a container
    matrixContainer <- jaspResults[["matrixContainer"]]

    if (!is.null(matrixContainer)) {
      return()
    } else {
      matrixContainer <- createJaspContainer(title="")
      matrixContainer$dependOn(c("variables", "pearson", "spearman", "kendall",
                                 "alternative", "missingValues", "kappa"))
      matrixContainer$position <- 1
      jaspResults[["matrixContainer"]] <- matrixContainer
    }
    return(matrixContainer)
  } else if (container=="pairsPlot") {
    if (!ready)
      return(NULL)

    pairsPlotCollection <- jaspResults[["pairsPlotCollection"]]

    if (is.null(pairsPlotCollection)) {
      pairsPlotCollection <- createJaspContainer(title="Bayesian Correlation Pairwise Plots")
      pairsPlotCollection$dependOn(c("pairs", "missingValues"))
      pairsPlotCollection$position <- 3
      jaspResults[["pairsPlotCollection"]] <- pairsPlotCollection
    }
    return(pairsPlotCollection)
  }
}

.computeAndDisplayTableCorBayes <- function(jaspResults, allBfObjects, dataset, options, ready) {
  # a. Retrieve from state ----
  #
  corBayesTable <- jaspResults[["corBayesTable"]]
  
  if (!is.null(corBayesTable))
    return(allBfObjects)

  # b. Get table ----
  #
  methodItems <- .getCorMethods(options)
  corBayesTable <- createJaspTable(title=.getCorTableTitle(methodItems, bayes=TRUE))
  corBayesTable$position <- 1

  alternative <- options[["alternative"]]

  corBayesTable$dependOn(c("pearson", "kendall", "spearman", "alternative", "kappa", "variables",
                           "displayPairwise","reportBayesFactors", "missingValues",
                           "flagSupported", "ci", "ciValue",
                           "reportN", "posteriorMedian", "bayesFactorType"))

  corBayesTable$showSpecifiedColumnsOnly <- TRUE
  corBayesTable$position <- 1

  corBayesTable$addCitation(.getCorCitations(methodItems, bayes=TRUE))

  if (alternative=="greater")
    corBayesTable$addFootnote(message=.getBfTableSidedFootnote(alternative="greater", analysis="correlation"),
                              symbol="<i>Note</i>.")

  if (alternative=="less")
    corBayesTable$addFootnote(message=.getBfTableSidedFootnote(alternative="less", analysis="correlation"),
                              symbol="<i>Note</i>.")

  # Get Bayes factor title
  #
  bfTitle <- .getBfTitle(options[["bayesFactorType"]], alternative)

  # Add legend footnote of BFs
  #
  if (options[["flagSupported"]]) {
    tempNote <- .bfFlagTableFootnote(options)
    corBayesTable$addFootnote(message=tempNote, symbol="*")
  }

  # To calculate the expected size of the table
  
  
  nVariables <- length(options[["variables"]])

  if (options[["displayPairwise"]]) {
    # Add the variables names paired
    #
    corBayesTable$addColumnInfo(name="variable1", title="", type="string")
    corBayesTable$addColumnInfo(name="separator", title="", type="string")
    corBayesTable$addColumnInfo(name="variable2", title="", type="string")

    if (options[["reportN"]])
      corBayesTable$addColumnInfo(name="n", title="n", type="integer")

    for (m in seq_along(methodItems)) {
      methodName <- methodItems[m]
      overTitle <- NULL
      
      if (length(methodItems) > 1) {
        # Overwrite the .cormethodNamesList with one that is broken up in overtitle and methodName
        #
        overTitle <- .corOverTitlesList[[methodName]]
        .corMethodNamesList <- list(pearson="r", spearman="rho", kendall="tau B")
      }

      # Add's "r", "rho", "tau B"
      corBayesTable$addColumnInfo(name=paste0(methodName, "stat"), title=.corMethodNamesList[[methodName]],
                                  overtitle=overTitle, type="number")

      if (options[["reportBayesFactors"]])
        corBayesTable$addColumnInfo(name=paste0(methodName, "bf"), title=bfTitle, overtitle=overTitle, type="number")

      # # TODO(ALEXANDER): Also report error %? Only useful for mcmc
      # #
      # if (options[["reportPercentageError"]])
      #   corBayesTable$addColumnInfo(name=paste0(methodName, "bfUq"), title=bfTitle, overtitle=overTitle, type="number")

      # TODO(Alexander): Do we want to show the posterior median?
      #
      # if (options[["posteriorMedian"]])
      #   corBayesTable$addColumnInfo(name=paste0(methodName, "posteriorMedian"), title="Posterior Median", type="number")
      #
      if (options[["ci"]]) {
        corBayesTable$addColumnInfo(name=paste0(methodName, "lowerCi"), overtitle=overTitle, type="number",
                                    title=paste0("Lower ", options[["ciValue"]]*100, "% CI"))

        corBayesTable$addColumnInfo(name=paste0(methodName, "upperCi"), overtitle=overTitle, type="number",
                                    title=paste0("Upper ", options[["ciValue"]]*100, "% CI"))
      }
    }

    # Calculate expected dimension
    #
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
  } else {
    # Note(Alexander): Correlation Matrix
    #
    corBayesTable$addColumnInfo(name="variable", title="", type="string")
    corBayesTable$addColumnInfo(name="itemColumn", title="", type="number")

    if (nVariables <= 1) {
      if (nVariables == 0) {
        corBayesTable$addColumnInfo(name="empty1", type="number", title="...")
      } else {
        corBayesTable$addColumnInfo(name=options[["variables"]][1], type="number", title=options[["variables"]][1])
      }
      corBayesTable$addColumnInfo(name="empty2", type="number", title="...")
    } else {
      for (variable in options[["variables"]]) {
        corBayesTable$addColumnInfo(name=variable, type="number",title=variable)
      }
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
  }

  # c. Assign ----
  #
  corBayesTable$setExpectedSize(numberOfRows, numberOfColumns)
  jaspResults[["corBayesTable"]] <- corBayesTable

  # d. Compute ----
  #
  allBfObjects <- .computeCorBayes("allBfObjects"=allBfObjects, "dataset"=dataset,
                                   "options"=options, "computePlots"=FALSE,
                                   "purpose"="matrix", "ready"=ready)

  # e. Fill ----
  #
  allBfObjects <- .recomputeAndFillCorBayesTable("table"=corBayesTable, "options"=options, 
                                     "allBfObjects"=allBfObjects)
  
  # if (!is.null(allBfObjects))
  #   .recomputeAndFillCorBayesTable("table"=corBayesTable, "options"=options, "allBfObjects"=allBfObjects)


  return(allBfObjects)
}

.computeCorBayes <- function(allBfObjects, dataset, options,
                             computePlots=FALSE, purpose=c("matrix", "pairs"), ready=TRUE) {
  purpose <- match.arg(purpose)
  # Retrieve from state
  #
  if (!ready)
    return(allBfObjects)

  # TODO(Alexander): This seems redundant, because we've read the data at a higher level, but only if ready
  # Furthermore, this implies that it needs to read the data everytime it computes, but then again at a higher
  # level the data is read everytime a variable (pair) is added to options$variables (options$pair)
  #
  if (is.null(dataset))
    dataset <- .corBayesReadData(dataset, options)

  if (options[["missingValues"]]=="excludeListwise")
    .hasErrors(dataset, type="observations", observations.amount='< 2', exitAnalysisIfErrors=TRUE)

  if (purpose=="matrix") {
    pairs <- combn(options[["variables"]], 2, simplify=FALSE)
    methodItems <- .getCorMethods(options)
  } else if (purpose=="pairs") {
    pairs <- options[["pairs"]]
    pairsIndeces <- .getPairsIndeces(options)
    pairs <- pairs[pairsIndeces]
    methodItems <- options[["pairsMethod"]]
  }
  
  for (pair in pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    
    # TODO(Alexander) check if they have the same name. Or just let it be, but highly inefficient though
    #
    pairName <- paste(sort(c(var1, var2)), collapse="-")

    v1 <- NULL
    v2 <- NULL
    bfObjectBig <- allBfObjects[[pairName]]

    for (m in seq_along(methodItems)) {
      storeObject <- FALSE
      methodName <- methodItems[m]
      bfObject <- bfObjectBig[[methodName]]

      if (is.null(bfObject)  ||  purpose=="pairs" && options[["plotBfSequential"]]) {
        # Obj not in allBfObjects
        #
        msg <- NULL

        if (is.null(v1) && is.null(v2)) {
          dataCheck <- .corBayesCheckPairsErrors(dataset, var1, var2)
          #
          if (!identical(dataCheck, FALSE)) {
            msg <- dataCheck[["message"]]
            v1 <- NA
            v2 <- NA
          } else {
            v1 <- dataset[[.v(var1)]]
            v2 <- dataset[[.v(var2)]]
          }
        }

        bfObject <- bstats::bcor.test("x"=v1, "y"=v2, "kappa"=options[["kappa"]],
                                      "method"=methodName, "ciValue"=options[["ciValue"]])

        if (!is.null(msg)) {
          bfObject[["error"]] <- msg
          bfObject[["dataError"]] <- msg
        }

        storeObject <- TRUE
      }

      # Recompute credible intervals if necessary
      #
      if (options[["ciValue"]] != bfObject[["ciValue"]]) {
        # TODO(Tim): Here for state
        bfObject <- .refreshCorCredibleInterval("bfObject"=bfObject, ciValue=options[["ciValue"]],
                                                          "method"=methodName)
        storeObject <- TRUE
      }
      
      if (computePlots) {
        if (purpose=="matrix") {
          plotItems <- "plotPriorPosterior"
        } else if (purpose=="pairs") {
          plotItems <- .getCorPlotItems(options, bayes=TRUE, sumStat=FALSE)
          plotItems <- setdiff(plotItems, "plotScatter")
        }

        for (i in seq_along(plotItems)) {
          itemName <- plotItems[i]

          if (itemName=="plotPriorPosterior") {
            alternative <- options[["alternative"]]
            posteriorLine <- bfObject[[alternative]][["posteriorLine"]]

            if (is.null(posteriorLine)) {
              # TODO(Tim): Here for state
              sidedResult <- bstats::computeCorPosteriorLine("bfObject"=bfObject, "method"=methodName, "alternative"=alternative)
              bfObject[[alternative]] <- sidedResult
              storeObject <- TRUE
            }
          } else if (itemName=="plotBfRobustness") {
            # TODO(Tim): Here for state
            tempResult <- bstats::computeCorRobustnessLine(bfObject, "method"=methodName)
            bfObject <- modifyList(bfObject, tempResult)
            storeObject <- TRUE
          } else if (itemName=="plotBfSequential") {
            # TODO(Tim): Here for state
            tempResult <-bstats::computeCorSequentialLine("x"=v1, "y"=v2, "bfObject"=bfObject, "method"=methodName)
            bfObject <- modifyList(bfObject, tempResult)
            storeObject <- TRUE
          }
          
          if (storeObject) {
            allBfObjects[[pairName]][[methodName]] <- bfObject
          }
        }
      }

      if (storeObject) {
        allBfObjects[[pairName]][[methodName]] <- bfObject
      }
    }
  }
  return(allBfObjects)
}

.recomputeAndFillCorBayesTable <- function(table, options, allBfObjects) {
  methodItems <- .getCorMethods(options)
  nVariables <- length(options[["variables"]])
  
  errorFootnotes <- list()
  errorRowName <- list()
  errorColName <- list()
  
  if (options[["flagSupported"]]) {
    bfFlagKey <- .bfFlagKey(options)
    bfTitle <- .getBfTitle(options[["bayesFactorType"]], options[["alternative"]])
    supportFootnotes <- list()
    supportSymbols <- list()
    supportRowName <- list()
    supportColName <- list()
  }
  
  if (options[["displayPairwise"]]) {
    if (nVariables <= 1) {
      if (nVariables == 0) {
        var1 <- "..."
      } else {
        var1 <- options[["variables"]][1]
      }
      var2 <- "..."
      table$addRows(list("variable1"=var1, "separator"="\u2014", "variable2"=var2))
      return(allBfObjects)
    }
    
    # TODO(Alexander): I don't think that there are cases where allBfObjects is empty anymore
    # 
    if (is.null(allBfObjects)) 
      return(allBfObjects)
    
    # Note(Alexander): Here there must be more than 2 variables
    # 
    pairs <- combn(options[["variables"]], 2, simplify=FALSE)
    for (pair in pairs) {
      var1 <- pair[1]
      var2 <- pair[2]
      
      pairName <- paste(sort(c(var1, var2)), collapse="-")
      tempRow <- list("variable1"=var1, "separator"="-", "variable2"=var2)
      
      itemNames <- .bSelectItems(options)
      
      for (m in seq_along(methodItems)) {
        methodName <- methodItems[m]
        bfObject <- allBfObjects[[pairName]][[methodName]]
        
        if (options[["ci"]])
          # TODO(Tim): Here for state
          bfObject <- .refreshCorCredibleInterval(bfObject, ciValue=options[["ciValue"]], method=methodName)
        
        errorMessage <- bfObject[["error"]]
        
        sidedObject <- bstats::getSidedObject(bfObject, alternative=options[["alternative"]],
                                       itemNames=itemNames)
        rowObject <- sidedObject[itemNames]
        
        sampleSize <- rowObject[["n"]]
        reportBf <- rowObject[["bf"]]
        
        if (options[["reportBayesFactors"]]) {
          if (options[["bayesFactorType"]]=="BF01") {
            rowObject[["bf"]] <- 1/reportBf
          } else if (options[["bayesFactorType"]]=="LogBF10") {
            rowObject[["bf"]] <- log(reportBf)
          }
        }
        
        newNames <- purrr::map_chr(itemNames, function(x, y){paste0(y, x)}, y=methodName)
        names(rowObject) <- newNames
        
        if (!is.null(sampleSize)) 
          rowObject[["n"]] <- sampleSize
        
        if (is.null(errorMessage)) {
          if (options[["reportBayesFactors"]] && options[["flagSupported"]] && !is.na(reportBf)) {
            if (reportBf >= 10) {
              supportRowName <- c(supportRowName, list(pairName))
              supportColName <- c(supportColName, list(paste0(methodName, "stat")))
              if (reportBf >= 100) {
                supportSymbols <- c(supportSymbols, list("***"))
                supportFootnotes <- c(supportFootnotes, list(bfFlagKey[["***"]]))
              } else if (reportBf >= 30) {
                supportSymbols <- c(supportSymbols, list("**"))
                supportFootnotes <- c(supportFootnotes, list(bfFlagKey[["**"]]))
              } else if (reportBf >= 10) {
                supportSymbols <- c(supportSymbols, list("*"))
                supportFootnotes <- c(supportFootnotes, list(bfFlagKey[["*"]]))
              }
            }
          }
        } else {
          errorFootnotes <- c(errorFootnotes, list(errorMessage))
          errorRowName <- c(errorRowName, list(pairName))
          errorColName <- c(errorColName, list(paste0(methodName, "stat")))
        }
        
        tempRow <- modifyList(tempRow, rowObject)
      }
      table$addRows(tempRow, rowNames=pairName)
    }
  } else {
    # Note(Alexander): Correlation MATRIX (wooosh)
    # 
    nVariablesEffective <- max(nVariables, 2)
    
    for (i in 1:nVariablesEffective) {
      emptyCellInfo <- vector("list", length=i)
      
      if (nVariables <= 1) {
        if (i==1) {
          if (nVariables == 0) {
            var1 <- "..."
            var2Names <- c("empty1")
          } else if (nVariables==1) {
            var1 <- options[["variables"]][1]
            var2Names <- c(var1)
          }
        } else if (i==2) {
          if (nVariables == 0) {
            var1 <- "..."
            var2Names <- c("empty1", "empty2")
          } else if (nVariables == 1) {
            var1 <- "..."
            var2Names <- c(options[["variables"]][1], "empty2")
          }
          emptyCellInfo[[1]] <- "."
        }
      } else {
        var1 <- options[["variables"]][i]
        var2Names <- options[["variables"]][1:i]
      }
      
      # Note(Alexander): Dash on the diagonal
      # 
      emptyCellInfo[[i]] <- "\u2014"
      
      itemNames <- .bSelectItems(options)
      nItems <- length(itemNames)
      
      for (m in seq_along(methodItems)) {
        # Row info collected here
        #
        methodName <- methodItems[m]
        allItemInfo <- vector("list", length=nItems)
        names(allItemInfo) <- itemNames
        
        for (item in itemNames)
          allItemInfo[[item]] <- emptyCellInfo
        
        # Note(Alexander): RETRIEVE info from state here
        #
        if (i > 1) {
          for (j in 1:(i-1)) {
            var2 <- var2Names[j]
            pairName <- paste(sort(c(var1, var2)), collapse="-")
            errorMessage <- NULL
            
            if (nVariables <= 1) {
              sidedObject <- list("n"=".", "bf"=".", "stat"=".", "upperCi"=".", "lowerCi"=".")
              reportBf <- NA
            } else if (nVariables >= 2) {
              # Note(Alexander): Here use actual data to fill the table
              #
              bfObject <- allBfObjects[[pairName]][[methodName]]
              
              if (options[["ci"]]) {
                bfObject <- .refreshCorCredibleInterval("bfObject"=bfObject, "ciValue"=options[["ciValue"]],
                                                        "method"=methodName)
                allBfObjects[[pairName]][[methodName]] <- bfObject
              }
              
              errorMessage <- bfObject[["error"]]
              
              sidedObject <- bstats::getSidedObject(bfObject, alternative=options[["alternative"]],
                                             itemNames=itemNames)
              reportBf <- sidedObject[["bf"]]
              
              if (options[["bayesFactorType"]]=="BF01") {
                sidedObject[["bf"]] <- 1/reportBf
              } else if (options[["bayesFactorType"]]=="LogBF10") {
                sidedObject[["bf"]] <- log(reportBf)
              }
            }
            
            # Here add to info to the collection
            # 
            for (item in itemNames) 
              allItemInfo[[item]][j] <- sidedObject[[item]]
            
            if (is.null(errorMessage)) {
              if (options[["flagSupported"]] && options[["reportBayesFactors"]] && !is.na(reportBf)) {
                if (reportBf >= 10) {
                  supportRowName <- c(supportRowName, list(paste0(var1, methodName, "stat")))
                  supportColName <- c(supportColName, list(var2))
                  
                  if (reportBf >= 100) {
                    supportFootnotes <- c(supportFootnotes, list(bfFlagKey[["***"]]))
                    supportSymbols <- c(supportSymbols, list("***"))
                  } else if (reportBf >= 30) {
                    supportFootnotes <- c(supportFootnotes, list(bfFlagKey[["**"]]))
                    supportSymbols <- c(supportSymbols, list("**"))
                  } else if (reportBf >= 10) {
                    supportFootnotes <- c(supportFootnotes, list(bfFlagKey[["*"]]))
                    supportSymbols <- c(supportSymbols, list("*"))
                  }
                }
              }
            } else {
              # Note(Alexander): Found errorpairInfo
              errorFootnotes <- c(errorFootnotes, list(errorMessage))
              errorRowName <- c(errorRowName, list(paste0(var1, methodName, "stat")))
              errorColName <- c(errorColName, list(var2))
            } 
          } # Note(Alexander: End info retrieval
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
          if (nVariables >= 2) {
            allItemInfo[["n"]] <- NULL
          }
          # Note(Alexander): After the first time we added "n", remove it from the other
          # 
          itemNames <- setdiff(itemNames, "n")
        }
        
        rowItemNames <- .bCorRowNames(options, itemNames, "method"=methodName)
        
        for (k in seq_along(itemNames)) {
          if (k==1 && m==1) {
            # First item gets the variable name
            tempRow <- list("variable"=var1, "itemColumn"=rowItemNames[[k]])
          } else {
            tempRow <- list("variable"="", "itemColumn"=rowItemNames[[k]])
          }
          
          if (nVariables >= 2) {
            itemInfo <- as.list(allItemInfo[[k]])
          } else {
            itemInfo <- as.list(emptyCellInfo)
          }
          names(itemInfo) <- var2Names
          tempRow <- modifyList(tempRow, itemInfo)
          table$addRows(tempRow, rowNames=paste0(var1, methodName, itemNames[k]))
        } # End items loop
      } # End loop over the methodItems
    } # End var1 loop
  } # End matrix fill
  
  for (i in seq_along(errorFootnotes)) 
    table$addFootnote(message = errorFootnotes[[i]], rowNames = errorRowName[[i]], colNames = errorColName[[i]])
  
  # Note(Alexander): Now add footnotes
  #
  if (options[["flagSupported"]]) {
    for (i in seq_along(supportFootnotes)) {
      table$addFootnote(symbol = supportSymbols[[i]], rowNames = supportRowName[[i]], colNames = supportColName[[i]])
    }
  }
  return(allBfObjects)
}

.corBayesCheckPairsErrors <- function(dataset, var1, var2) {
  errors <- .hasErrors(dataset, message='short',
                       type=c('observations', 'variance', 'infinity', 'observationsPairwise'),
                       all.target=c(var1, var2), observations.amount='< 2',
                       observationsPairwise.amount=2)
}

.computeAndDisplayMatrixPlot <- function(jaspResults, allBfObjects, dataset, options, ready) {
  # a. Retrieve from state ----
  #
  if (!is.null(jaspResults[["matrixPlot"]]))
    return(allBfObjects)
  
  if (!options[["plotMatrix"]])
    ready <- FALSE
  
  if (!ready)
    return(allBfObjects)
  
  nVariables <- length(options[["variables"]])
  
  # b. Compute  -----
  #
  methodItems <- .getCorMethods(options)
  
  if (options[["plotMatrixPosteriors"]] ||  length(methodItems) > 0) {
    allBfObjects <- .computeCorBayes("allBfObjects"=allBfObjects, "dataset"=dataset,
                                     "options"=options, "computePlots"=TRUE,
                                     "purpose"="matrix", "ready"=ready)
  }
  
  # c. Get ----
  #
  matrixPlot <- createJaspPlot(title="Bayesian Correlation Matrix Plot")
  matrixPlot$position <- 2
  
  matrixDependencies <- c("variables", "plotMatrix", "plotMatrixDensities", "plotMatrixPosteriors", "missingValues")
  
  if (options[["plotMatrixPosteriors"]])
    matrixDependencies <- c(matrixDependencies, "pearson", "spearman", "kendall", "alternative", "kappa")
  
  matrixPlot$dependOn(matrixDependencies)
  
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
  
  if (!length(methodItems)==0 && options[["plotMatrix"]]) 
    .drawMatrixPlotCorBayes("matrixPlot"=matrixPlot, "allBfObjects"=allBfObjects, "dataset"=dataset, "options"=options)
  
  # e. Assign -----
  #
  jaspResults[["matrixPlot"]] <- matrixPlot
  return(allBfObjects)
}

.drawMatrixPlotCorBayes <- function(matrixPlot, allBfObjects, dataset, options) {
  plotResult <- matrixPlot$plotObject
  
  if (!is.null(plotResult)) 
    return()
  
  matrixPlot$status <- "running"
  
  methodItems <- .getCorMethods(options)
  vars <- options[["variables"]]
  nVariables <- length(vars)
  plotMat <- matrix(list(), nVariables, nVariables)
  
  for (row in seq_len(nVariables-1)) {
    for (col in row:nVariables) {
      var1 <- vars[row]
      var2 <- vars[col]
      
      pairName <- paste(sort(c(var1, var2)), collapse="-")
      bfObjectBig <- allBfObjects[[pairName]]
      
      if (row < col) {
        if (!options[["plotMatrixPosteriors"]]) {
          posteriorPlot <- list()
        } else {
          posteriorPlot <- .drawPosteriorPlotCorBayes(bfObjectBig, options, methodItems, purpose="matrix")
        }
        plotMat[[col, row]] <- posteriorPlot
        
        dataError <- bfObjectBig[[1]][["dataError"]]
        
        if (!is.null(dataError)) {
          # TODO(Alexander): This is probably not run here anyways, but try avoid displayerror
          scatterPlot <- .displayError(errorMessage=dataError)
        } else {
          subData <- dataset[, .v(c(var1, var2)), drop=FALSE]
          subData <- subData[complete.cases(subData), , drop=FALSE]
          scatterPlot <- .bCorScatter(x=subData[, 1, drop=TRUE], y=subData[, 2, drop=TRUE], options)
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

.drawPosteriorPlotCorBayes <- function(bfObjectBig, options, methodItems,
                                       purpose=c("matrix", "pairs", "sumStat")) {
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

  if (purpose %in% c("matrix", "pairs")) {
    dataError <- bfObjectBig[[1]][["error"]]
  } else if (purpose=="sumStat") {
    dataError <- bfObjectBig[["error"]]
  }

  if (!is.null(dataError)) {
    if (purpose=="matrix") {
      plotResult <- .displayError(errorMessage=dataError)
    } else {
      plotResult <- dataError
    }
    return(plotResult)
  }

  # a. No data errors, try to plot ----
  #
  alternative <- options[["alternative"]]

  allPosteriorLines <- vector("list", length=length(methodItems))

  for (m in seq_along(methodItems)) {
    methodName <- methodItems[m]

    if (purpose %in% c("matrix", "pairs")) {
      bfObject <- bfObjectBig[[methodName]]
    } else if (purpose=="sumStat") {
      bfObject <- bfObjectBig
    }

    error <- bfObject[["error"]]

    if (!is.null(error)) {
      allPosteriorLines <- NA
      break()
    }

    sidedObject <- bfObject[[alternative]]

    if (sidedObject[["tooPeaked"]]) {
      error <- "Posterior is too peaked"
      allPosteriorLines <- NA
      break()
    }
    posteriorLine <- sidedObject[["posteriorLine"]]

    if (is.null(posteriorLine) ||  length(posteriorLine)==1) {
      # TODO(Alexander): This covers the case when posteriorLine <- "Could not compute posterior"
      # RETURN FAILED PLOT
      error <- "Could not compute the posteriors"
      allPosteriorLines <- NA
      break()
    }
    allPosteriorLines[[m]] <- posteriorLine
  }

  # b. Check for error ----
  #
  if (!is.null(error)) {
    if (purpose=="matrix") {
      plotResult <- .displayError(errorMessage=error)
    } else {
      plotResult <- error
    }
    return(plotResult)
  }
  
  # c. No errors, combine plot info ------
  #
  domainLength <- length(sidedObject[["xDomain"]])

  dfPoints <- NULL
  BF10 <- NULL
  CRI <- NULL
  medianPoint <- NULL
  CRItxt <- NULL

  if (purpose=="matrix") {
    xName <- NULL
    if (length(methodItems)  == 1)  {
      # xName <- unlist(.corXNames[methodItems], recursive = FALSE)
      # 
      dfLines <- data.frame(
        x = sidedObject[["xDomain"]],
        y = allPosteriorLines[[1]]
      )
    } else {
      # xName <- "Correlation Coefficient"
      # 
      gLegend <- unlist(.corGLegendList[methodItems], use.names=FALSE, recursive = FALSE)
      
      dfLines <- data.frame(
        x = rep(sidedObject[["xDomain"]]), 
        y = unlist(allPosteriorLines, recursive = FALSE),
        g = rep(gLegend, each=domainLength)
      )
    }
  } else if (purpose %in% c("pairs", "sumStat")) {
    xName <- unlist(.corXNames[methodItems], recursive = FALSE)
    gLegend <- c("Prior", "Posterior")
    dfLines <- data.frame(
      x = sidedObject[["xDomain"]],
      y = c(sidedObject[["priorLine"]], allPosteriorLines[[1]]),
      g = rep(gLegend, each=domainLength)
    )

    if (isTRUE(options[["plotPriorPosteriorAddEstimationInfo"]])) {
      CRI <- c(sidedObject[["lowerCi"]], sidedObject[["upperCi"]])
      CRItxt <- paste0(sidedObject[["ciValue"]]*100, "% CI:")
      medianPoint <- sidedObject[["posteriorMedian"]]
    }

    if (isTRUE(options[["plotPriorPosteriorAddTestingInfo"]])) {
      dfPoints <- data.frame(
        x = c(sidedObject[["h0"]], sidedObject[["h0"]]),
        y = c(sidedObject[["priorAtH0"]], sidedObject[["posteriorAtH0"]]),
        g = c("Prior", "Posterior")
      )
      BF10 <- sidedObject[["bf"]]
    }
  }
  
  hypothesis <- switch(options[["alternative"]], 
                       "two.sided"="equal",
                       "greater"="greater",
                       "less"="smaller"
  )
  plotResult <- try(JASPgraphs::PlotPriorAndPosterior(
    dfLines, dfPoints, BF10, "CRI"=CRI, "CRItxt"=CRItxt, "median"=medianPoint, "xName"=xName, "hypothesis"=hypothesis)
  )
  return(plotResult)
}




.setPairCollectionCorBayes <- function(pairsPlotCollection, options, ready) {
  
  if (!ready)
    return(pairsPlotCollection)
  
  plotItems <- .getCorPlotItems(options, bayes=TRUE,  sumStat=FALSE)
  
  bfPlotPriorPosteriorDependencies <- c("pairsMethod", "kappa", "alternative")
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
    
    # plotContainer <- createJaspContainer("title"=plotContainerTitle, "dependencies"=plotContainerDependence)
    
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

.fillPairsPlotCorBayes <- function(pairsPlotCollection, allBfObjects, dataset, options) {
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
    bfObjectBig <- allBfObjects[[pairName]]
    dataError <- bfObjectBig[[1]][["dataError"]]
    
    if (!is.null(dataError)) {
      pairContainer$setError(dataError)
      next()
    }
    
    for (i in seq_along(plotItems)) {
      item <- plotItems[i]
      jaspPlotResult <- pairContainer[[item]]
      plotResult <- jaspPlotResult$plotObject
      
      if (is.null(plotResult)) {
        jaspPlotResult$status <- "running"
        
        if (item=="plotScatter") {
          subData <- dataset[, .v(c(var1, var2)), drop=FALSE]
          subData <- subData[complete.cases(subData), , drop=FALSE]
          triedPlot <- try(.bCorScatter(x=subData[, 1, drop=TRUE], y=subData[, 2, drop=TRUE], xName=var1, yName=var2, options))
          .checkAndSetPlotCorBayes(triedPlot, jaspPlotResult)
        } else if (item=="plotPriorPosterior") {
          triedPlot <- .drawPosteriorPlotCorBayes(bfObjectBig, options, thisMethod, purpose="pairs")
          .checkAndSetPlotCorBayes(triedPlot, jaspPlotResult)
        } else if (item=="plotBfRobustness") {
          triedPlot <- .drawBfRobustnessPlotCorBayes(bfObjectBig[[thisMethod]], options)
          .checkAndSetPlotCorBayes(triedPlot, jaspPlotResult)
        } else if (item=="plotBfSequential") {
          triedPlot <- .drawBfSequentialPlotCorBayes(bfObjectBig[[thisMethod]], options)
          .checkAndSetPlotCorBayes(triedPlot, jaspPlotResult)
        }
      }
    }
  }
  return()
}

.drawBfRobustnessPlotCorBayes <- function(bfObject, options) {
  plotResult <- ""
  
  alternative <- options[["alternative"]]
  sidedObject <- bstats::getSidedObject(bfObject, "alternative"=alternative, 
                                 "itemNames"=c("kappaDomain", "kappa"))
  
  robustnessLine <- sidedObject[["robustnessLine"]]
  
  if (is.character(robustnessLine)) {
    plotResult <- robustnessLine
  } else {
    xLine <- sidedObject[["kappaDomain"]]
    yLine <- robustnessLine
    xPoint <- NULL
    yPoint <- NULL
    
    if (isTRUE(options[["plotBfRobustnessAddInfo"]])) {
      maxBf <- sidedObject[["robustnessMaxBf"]]
      kappaOfMaxBf <- sidedObject[["robustnessKappaOfMaxBf"]]
      userBf <- sidedObject[["bf"]]
      userKappa <- sidedObject[["kappa"]]
      
      xPoint <- c(kappaOfMaxBf, userKappa)
      yPoint <- c(maxBf, userBf)
    }
    
    plotResult <- .plotRobustnessCor("xLine"=xLine, "yLine"=yLine, "xPoint"=xPoint, "yPoint"=yPoint, 
                                    "bfType"=options[["bayesFactorType"]], "alternative"=options[["alternative"]], 
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
    legendText <- vector("character", length(xPoint))
    
    for (i in seq_along(xPoint)) {
      if (i==1) {
        legendText[i] <- sprintf("paste(max, ~%s, ':',   phantom(phollll), %s, ~at, ~kappa==%s)", 
                                 bfLegendLabel, format(yPoint[i], digits=nDigits), format(xPoint[i], digits=nDigits))
      } else if (i==2) {
        legendText[i] <- sprintf("paste(user~prior, ':', phantom(phll[0]), ~%s==%s)", bfLegendLabel, 
                                 format(yPoint[i],  digits=nDigits))
      }
      # TODO(Alexander): Do something here, with user-prior, wide etc etc. Also loop over colours
    }
    
    # TODO(Alexander): Try and think of how to default
    logYPoint <- log(yPoint)
    
    dfPoints <- data.frame(
      x = xPoint,
      y = logYPoint,
      g = JASPgraphs::parseThis(legendText),
      stringsAsFactors = FALSE
    )
  }
    
  
  plotResult <- try(JASPgraphs::PlotRobustnessSequential(
    dfLines      = dfLines,
    xName        = expression(paste("Stretched beta prior width ", kappa)),
    dfPoints     = dfPoints,
    bfType       = bfPlotType,
    pointColors  = pointColors,
    pointFill    = pointFill, 
    hypothesis   = hypothesisJASPgraphsName
  ))
  
  return(plotResult)
}

.drawBfSequentialPlotCorBayes <- function(bfObject, options) {
  plotResult <- ""
  alternative <- options[["alternative"]]
  
  sidedObject <- bstats::getSidedObject(bfObject, "alternative"=alternative, "itemNames"="nDomain")
  sequentialLine <- sidedObject[["sequentialLine"]]
  
  if (is.character(sequentialLine)) {
    return(sequentialLine)
  } else {
    y <- log(sequentialLine)
    
    if (options[["bayesFactorType"]]=="BF01") 
      y <- -y
    
    dfLines <- data.frame(
      x = sidedObject[["nDomain"]],
      y = y
    )
    
    BF <- NULL
    hypothesis <- switch(options[["alternative"]],
                         "two.sided"="equal",
                         "greater"="greater",
                         "less"="smaller"
    )
    
    if (options[["plotBfSequentialAddInfo"]])
      BF <- sidedObject[["bf"]]
    
    bfType <- switch(options[["bayesFactorType"]],
                     "LogBF10"="BF10",
                     "BF10"="BF10",
                     "BF01"="BF01"
    )
    
    plotResult <- try(JASPgraphs::PlotRobustnessSequential(
      dfLines      = dfLines,
      xName        = "n",
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

.refreshCorCredibleInterval <- function(bfObject, ciValue, method) {
  if (bfObject[["ciValue"]]==ciValue)
    return(bfObject)

  if (method=="pearson") {
    # TODO(Tim): Here for state
    tempList <- bstats::computePearsonCredibleInterval("betaA"=bfObject[["betaA"]], "betaB"=bfObject[["betaB"]],
                                                "ciValue"=ciValue)
  } else if (method=="kendall") {
    # TODO(Tim): Here for state
    tempList <- bstats::computeKendallCredibleInterval("n"=bfObject[["two.sided"]][["n"]], "tauObs"=bfObject[["two.sided"]][["stat"]], 
                                    "kappa"=bfObject[["kappa"]], "var"=1, "ciValue"=ciValue, 
                                    "h0"=bfObject[["h0"]])
  } else if (method=="spearman") {
    # TODO(Johnny):
    print("NO THIS IS NOT IT, STILL NEED TO MAKE THIS, THIS IS JUST A PLACEHOLDER")
    tempList <- bstats::computePearsonCredibleInterval("betaA"=bfObject[["betaA"]], "betaB"=bfObject[["betaB"]],
                                                "ciValue"=ciValue)
  }
  # TODO(Tim): Here for state
  bfObject <- modifyList(bfObject, tempList)
  return(bfObject)
}
