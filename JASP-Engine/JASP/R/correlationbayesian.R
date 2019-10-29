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
  readyPairs <- .checkPairsReady(options)
  
  # 1. Create main table (always)----
  # 
  matrixContainer <- .getContainerCorBayes(jaspResults, container="matrix", ready=TRUE)
  corBayesTable <- .getTableCorBayes(matrixContainer, options, readyMatrix)
  
  # Note(Alexander): Everytime a variable (pair) is added to options$variables (options$pair) 
  # the data are read
  # 
  if (readyMatrix | readyPairs) {
    if (is.null(dataset)) {
      dataset <- .corBayesReadData(dataset, options)
    }
  }
  
  # 2. Compute: If not ready, then return NULL object ------
  # 
  allBfObjects <- .computeCorBayes("jaspResults"=jaspResults, "dataset"=dataset, 
                                   "options"=options, allBfObjects=NULL, 
                                   "computePlots"=FALSE, "mode"="matrix", "ready"=readyMatrix)
  
  # 3. Fill a table, if not ready, then fill with "..." place holders (Always) ----- 
  # 
  .fillCorBayesTable(corBayesTable, options, allBfObjects)
  
  # 4. Matrix plot (Optional) ------ 
  # 
  matrixPlot <- .getMatrixPlot(matrixContainer, options, readyMatrix)

  if (!is.null(matrixPlot)) {
    allBfObjects <- .computeCorBayes("jaspResults"=jaspResults, "dataset"=dataset,
                                     "options"=options, "allBfObjects"=allBfObjects,
                                     "computePlots"=TRUE, "mode"="matrix", "ready"=readyMatrix)
    .drawMatrixPlotCorBayes("matrixPlot"=matrixPlot, "allBfObjects"=allBfObjects,
                            "dataset"=dataset, "options"=options)
  }
  
  # 5. Pairs plot (Optional) ------
  #
  pairsPlotCollection <- .getContainerCorBayes("jaspResults"=jaspResults,
                                               "container"="pairsPlot",
                                               "ready"=readyPairs)
  if (!is.null(pairsPlotCollection)) {
    pairsPlotCollection <- .setPairCollectionCorBayes(pairsPlotCollection,
                                                      options, readyPairs)

    allBfObjects <- .computeCorBayes("jaspResults"=jaspResults, "dataset"=dataset,
                                     "options"=options, "allBfObjects"=allBfObjects,
                                     "computePlots"=TRUE, "mode"="pairs", "ready"=readyPairs)
    .drawPairsPlotCorBayes(pairsPlotCollection, allBfObjects, dataset, options)

  }
  
  # 6. allBfObjects in state -------
  # 
  if (options[["missingValues"]]=="excludeListwise") {
    stateDependencies <- c("missingValues", "kappa", "variables")
  } else if (options[["missingValues"]]=="excludePairwise") {
    stateDependencies <- c("missingValues", "kappa")
  }
  jaspResults[["bfState"]] <- createJaspState(allBfObjects, dependencies=stateDependencies)
  # .getTableCorBayes <- function(jaspResults, options, ready)
  # 
  # .getMatrixPlot(jaspResults, allBfObjects, dataset, options, ready)
  
  # Pairwise stuff
  # .corBayesPlotScatter(jaspResults, dataset, options, ready)
  # .corBayesPlotPosteriors(jaspResults, allBfObjects, dataset, options, ready)
  # .corBayesPlotRobustness(jaspResults, dataset, options, ready)
  # .corBayesPlotSequential(jaspResults, dataset, options, ready)
}

.getPairsLength <- function(options) {
  pairs <- options[["pairs"]]
  nPairs <- length(pairs)
  
  if (nPairs!=0) {
    lastPair <- pairs[[nPairs]]
    v2 <- lastPair[2]

    if (is.na(v2))
      nPairs <- nPairs -1
  }
  
  return(max(nPairs, 0))
}

.checkPairsReady <- function(options) {
  nPairs <- .getPairsLength(options)

  if (nPairs <= 0) 
    return(FALSE)
  
  plotItems <- .getCorPlotItems(options)
  
  if (length(plotItems)==0)
    return(FALSE)
  
  return(TRUE)
}

.corBayesReadData <- function(dataset, options) {
  firstList <- unlist(options[["variables"]])
  secondList <- unlist(options[["pairs"]])
  allVariables <- unique(c(firstList, secondList))
  allVariables <- allVariables[allVariables != ""]
  
  if (options[["missingValues"]] == "excludeListwise") {
    dataset <- .readDataSetToEnd(columns.as.numeric=allVariables, exclude.na.listwise=allVariables)
  } else {
    dataset <- .readDataSetToEnd(columns.as.numeric=allVariables)
  }
  
  return(dataset)
}
# 
#   } else if (input=="pairs") {
#     browser()
#     
#     allVariables <- unique(unlist(options[["pairs"]]))
#     allVariables <- allVariables[allVariables != ""]
#     
#     if (options[["missingValues"]] == "excludeListwise") {
#       dataset <- .readDataSetToEnd(columns.as.numeric=allVariables, exclude.na.listwise=allVariables)
#     } else {
#       dataset <- .readDataSetToEnd(columns.as.numeric=allVariables)
#     }
#     args(.readDatasetToEndNative)
#     args(.readDataSetToEnd)
#     return(dataset)
#   }
# }

# .corBayesTableMain <- function(jaspResults, dataset, options, ready) {
#   if (!is.null(jaspResults[["corBayesTable"]]))
#     return()
#   
#   corBayesTable <- .getTableCorBayes(options, ready)
#   jaspResults[["corBayesTable"]] <- corBayesTable
# }
#   
#   
#   if (is.null(jaspResults[["bfState"]])) {
#     bfState <- createJaspState()
#   } else {
#     bfState <- jaspResults[["bfState"]]
#   }
#   allBfObjects <- bfState$object
#   
#   allBfObjects <- .computeCorBayes(dataset, options, allBfObjects)
#   .fillCorBayesTable(corBayesTable, options, allBfObjects)
#   
#   if (options[["missingValues"]]=="excludeListwise") {
#     stateDependencies <- c("missingValues", "kappa", "variables")
#   } else if (options[["missingValues"]]=="excludePairwise") {
#     stateDependencies <- c("missingValues", "kappa")
#   }
#   jaspResults[["bfState"]] <- createJaspState(allBfObjects, dependencies=stateDependencies)
#   # .getTableCorBayes <- function(jaspResults, options, ready)
#   # 
#   if (!ready)
#     return()
#   
#   return(allBfObjects)
# }
.getContainerCorBayes <- function(jaspResults, container="matrix", ready=TRUE) {
  if (container=="matrix") {
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
      pairsPlotCollection$dependOn(c("pairs", "missingValues", "pairsTest"))
      pairsPlotCollection$position <- 2
      jaspResults[["pairsPlotCollection"]] <- pairsPlotCollection
    }
    return(pairsPlotCollection)
  }
}

.getTableCorBayes <- function(jaspResults, options, ready) {
  corBayesTable <- jaspResults[["corBayesTable"]]
  
  if (!is.null(corBayesTable))
    return(corBayesTable)
  
  # Create table
  # 
  tests <- .getCorTests(options)
  corBayesTable <- createJaspTable(title=.getCorTableTitle(tests, bayes=TRUE))
  # corBayesTable$dependOn(c("variables", "pearson", "spearman", "kendall", "bayesFactorType",
  #                           "alternative", "missingValues", "kappa",
  #                           "displayPairwise","reportBayesFactors",
  #                           "flagSupported", "ci", "ciValue",
  #                           "reportN", "posteriorMedian"))
  corBayesTable$position <- 1
  
  alternative <- options[["alternative"]]
  
  
  corBayesTable$dependOn(c("displayPairwise","reportBayesFactors",
                           "flagSupported", "ci", "ciValue",
                           "reportN", "posteriorMedian"))
  # corBayesTable$dependOn(c("variables", "pearson", "spearman", "kendall",
  #                          "alternative", "missingValues", "kappa",
  #                          "displayPairwise","reportBayesFactors",
  #                          "flagSupported", "ci", "ciValue",
  #                          "reportN", "posteriorMedian"))
  
  
  corBayesTable$showSpecifiedColumnsOnly <- TRUE
  corBayesTable$position <- 1
  
  # Add citations
  # 
  corBayesTable$addCitation(.getCorCitations(tests, bayes=TRUE))
  
  # Add footnote for hypothesis type
  # 
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
    
    for (m in seq_along(tests)) {
      testName <- tests[m]
      overTitle <- NULL
      if (length(tests) > 1) {
        # Overwrite the .corTestNamesList with on that is broken up in overtitle and testname
        # 
        overTitle <- .corOverTitlesList[[testName]]
        .corTestNamesList <- list(pearson="r", spearman="rho", kendall="tau B")
      }
      
      # Add's "r", "rho", "tau B"
      corBayesTable$addColumnInfo(name=paste0(testName, "stat"), title=.corTestNamesList[[testName]], 
                                  overtitle=overTitle, type="number")
      
      if (options[["reportBayesFactors"]]) 
        corBayesTable$addColumnInfo(name=paste0(testName, "bf"), title=bfTitle, overtitle=overTitle, type="number")
      
      # # TODO(ALEXANDER): Also report error %? Only useful for mcmc
      # # 
      # if (options[["reportPercentageError"]]) 
      #   corBayesTable$addColumnInfo(name=paste0(testName, "Bf"), title=bfTitle, overtitle=overTitle, type="number")
      
      # TODO(Alexander): Do we want to show the posterior median?
      # 
      # if (options[["posteriorMedian"]]) 
      #   corBayesTable$addColumnInfo(name="posteriorMedian", title="Posterior Median", type="number")
      # 
      if (options[["ci"]]) {
        corBayesTable$addColumnInfo(name=paste0(testName, "lowerCi"), overtitle=overTitle, type="number",
                                    title=paste0("Lower ", options[["ciValue"]]*100, "% CI"))
        
        corBayesTable$addColumnInfo(name=paste0(testName, "upperCi"), overtitle=overTitle, type="number",
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
    
    if (is.null(tests) | length(tests)==0) {
      numberOfRows <- nVariables
      numberOfColumns <- 3
    } else {
      numberOfRows <- choose(nVariables, 2)
      numberOfColumns <- 3+ options[["reportN"]] + length(tests) * sum(options[["reportBayesFactors"]], 
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
    
    if (length(tests)==0) {
      nItems <- sum(options[["reportBayesFactors"]], 2*options[["ci"]]+options[["reportN"]])
      
      if (nItems <= 1) {
        numberOfColumns <- nVariables
      } else {
        numberOfColumns <- 2 + nVariables
      }
      numberOfRows <- nVariables
    } else {
      itemsPerTest <- sum(options[["reportBayesFactors"]], 2*options[["ci"]])
      
      nVariablesEffective <- max(nVariables, 2)
      
      if (itemsPerTest==0) {
        numberOfColumns <- 1 + nVariablesEffective
        numberOfRows <- nVariablesEffective
      } else {
        numberOfColumns <- 2 + nVariablesEffective
        numberOfRows <- nVariablesEffective * length(tests) * 
          (itemsPerTest + 1) + options[["reportN"]] * nVariablesEffective
      }
    }
  }
  
  corBayesTable$setExpectedSize(numberOfRows, numberOfColumns)
  jaspResults[["corBayesTable"]] <- corBayesTable
  return(corBayesTable)
}

.computeCorBayes <- function(jaspResults, dataset, options, allBfObjects=NULL, 
                             computePlots=FALSE, mode="matrix", ready=TRUE) {
  
  # Retrieve from state
  # 
  if (is.null(allBfObjects)) {
    # Check and create state  
    # 
    if (is.null(jaspResults[["bfState"]])) {
      bfState <- createJaspState()
    } else {
      bfState <- jaspResults[["bfState"]]
    }
    allBfObjects <- bfState$object
  }
  
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
  
  
  if (mode=="matrix") {
    pairs <- combn(options[["variables"]], 2, simplify=FALSE)
    tests <- .getCorTests(options)
  } else if (mode=="pairs") {
    pairs <- unique(options[["pairs"]])
    tests <- options[["pairsTest"]]
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
    
    for (m in seq_along(tests)) {
      storeObject <- FALSE
      testName <- tests[m]
      bfObject <- bfObjectBig[[testName]]
      
      if (is.null(bfObject)  | mode=="pairs" & options[["plotBfSequential"]]) {
        # Obj not in allBfObjects
        #
        msg <- NULL
        
        if (is.null(v1) & is.null(v2)) {
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
        
        bfObject <- bcor.test("x"=v1, "y"=v2, "kappa"=options[["kappa"]], 
                              "method"=testName, "ciValue"=options[["ciValue"]])
        
        if (!is.null(msg)) {
          bfObject[["error"]] <- msg
          bfObject[["dataError"]] <- msg
        }
        
        storeObject <- TRUE
      }
      
      # Recompute credible intervals if necessary
      # 
      if (options[["ciValue"]] != bfObject[["ciValue"]]) {
        tempList <- .computePearsonCredibleInterval("betaA"=bfObject[["betaA"]], 
                                                    "betaB"=bfObject[["betaB"]],
                                                    "ciValue"=options[["ciValue"]])
        bfObject <- modifyList(bfObject, tempList)
        storeObject <- TRUE
      }
      
      
      # TODO(Alexander): add .getPlotItems so we know what to compute for the pairwise things
      if (computePlots) {
        if (mode=="matrix") {
          plotItems <- "plotPriorPosterior"
        } else if (mode=="pairs") {
          plotItems <- .getCorPlotItems(options)
          plotItems <- setdiff(plotItems, "plotScatter")
        }
        
        for (i in seq_along(plotItems)) {
          itemName <- plotItems[i]
          
          if (itemName=="plotPriorPosterior") {
            alternative <- options[["alternative"]]
            posteriorLine <- bfObject[[alternative]][["posteriorLine"]]
            
            if (is.null(posteriorLine)) {
              sidedResult <- .computeCorPosteriorLine("bfObject"=bfObject, "test"=testName, "alternative"=alternative)
              bfObject[[alternative]] <- sidedResult
              storeObject <- TRUE
            }
          } else if (itemName=="plotBfRobustness") {
            tempResult <-.computeCorRobustnessLine(bfObject, "test"=testName) 
            bfObject <- modifyList(bfObject, tempResult)
            storeObject <- TRUE
          } else if (itemName=="plotBfSequential") {
            browser()
            tempResult <-.computeCorSequentialLine("x"=v1, "y"=v2, "bfObject"=bfObject, "test"=testName)
            bfObject <- modifyList(bfObject, tempResult)
            storeObject <- TRUE
          }
        }
        
      }
      
      if (storeObject) {
        allBfObjects[[pairName]][[testName]] <- bfObject
      }
    }
  }
  return(allBfObjects)
}

# .fillCorBayesTableEmpty <- function(table, options, allBfObjects) {
#   
# }

.fillCorBayesTable <- function(table, options, allBfObjects) {
  tests <- .getCorTests(options)
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
      table$addRows(list("variable1"=var1, "separator"="-", "variable2"=var2))
      return()
    }
    
    pairs <- combn(options[["variables"]], 2, simplify=FALSE)
    
    for (pair in pairs) {
      var1 <- pair[1]
      var2 <- pair[2]
      
      pairName <- paste(sort(c(var1, var2)), collapse="-")
      tempRow <- list("variable1"=var1, "separator"="-", "variable2"=var2)
      
      for (m in seq_along(tests)) {
        testName <- tests[m]
        bfObject <- allBfObjects[[pairName]][[testName]]
        
        errorMessage <- bfObject[["error"]]
        sidedObject <- .getSidedObject(bfObject, alternative=options[["alternative"]], 
                                       itemNames=c("stat", "bf", "lowerCi", "upperCi"))
        sampleSize <- sidedObject[["n"]]
        reportBf <- sidedObject[["bf"]]
        objNames <- names(sidedObject)
        newNames <- purrr::map_chr(objNames, function(x, y){paste0(y, x)}, y=testName)
        names(sidedObject) <- newNames
        sidedObject[["n"]] <- sampleSize
        
        if (is.null(errorMessage)) {
          if (options[["flagSupported"]]) {
            if (!is.na(reportBf)) {
              supportRowName <- c(supportRowName, list(pairName))
              supportColName <- c(supportColName, list(paste0(testName, "stat")))
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
          errorColName <- c(errorColName, list(paste0(testName, "stat")))
        }
        tempRow <- modifyList(tempRow, sidedObject)
      }
      table$addRows(tempRow, rowNames=pairName)
    }
  } else {
    # Note(Alexander): Correlation MATRIX (wooosh)
    
    # Note(Alexander): Placeholder and fail at NA
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
      
      emptyCellInfo[[i]] <- "-"
      
      itemNames <- .bSelectItems(options)
      nItems <- length(itemNames)
      
      for (m in seq_along(tests)) {
        # Row info collected here
        # 
        testName <- tests[m]
        allItemInfo <- vector("list", length=nItems)
        names(allItemInfo) <- itemNames
        
        for (item in itemNames) 
          allItemInfo[[item]] <- emptyCellInfo
        
        # Note(Alexander): RETRIEVE info from state here
        # 
        if (i > 1) {
          for (j in 1:(i-1)) {
            if (nVariables <= 1) {
              next()
            }
            
            var2 <- var2Names[j]
            pairName <- paste(sort(c(var1, var2)), collapse="-")
            
            # Note(Alexander): Retrieve from state
            # 
            bfObject <- allBfObjects[[pairName]][[testName]]
            
            errorMessage <- bfObject[["error"]]
            
            sidedObject <- .getSidedObject(bfObject, alternative=options[["alternative"]], 
                                           itemNames=itemNames)
            reportBf <- sidedObject[["bf"]]
            for (item in itemNames) {
              allItemInfo[[item]][j] <- sidedObject[[item]]
            }
            
            if (is.null(errorMessage)) {
              if (options[["flagSupported"]]) {
                if (!is.na(reportBf)) {
                  supportRowName <- c(supportRowName, list(paste0(var1, testName, "stat")))
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
              errorRowName <- c(errorRowName, list(paste0(var1, testName, "stat")))
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
        if (m > 1 & options[["reportN"]]) {
          if (nVariables >= 2) {
            allItemInfo[["n"]] <- NULL
          }
          itemNames <- setdiff(itemNames, "n")
        }
        
        rowItemNames <- .bCorRowNames(options, itemNames, test=testName)
        
        for (k in seq_along(itemNames)) {
          if (k==1 & m==1) {
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
          table$addRows(tempRow, rowNames=paste0(var1, testName, itemNames[k]))
        } # End items loop 
      } # End loop over the tests
    } # End var1 loop
  } # End matrix fill
  
  for (i in seq_along(errorFootnotes)) {
    table$addFootnote(message = errorFootnotes[[i]], rowNames = errorRowName[[i]], colNames = errorColName[[i]])
  }
  
  # Note(Alexander): Now add footnotes
  # 
  if (options[["flagSupported"]]) {
    for (i in seq_along(supportFootnotes)) {
      table$addFootnote(message=supportFootnotes[[i]], symbol = supportSymbols[[i]], rowNames = supportRowName[[i]], colNames = supportColName[[i]])
    }
  }
  
  return()
}

.corBayesCheckPairsErrors <- function(dataset, var1, var2) {
  errors <- .hasErrors(dataset, message='short',
                       type=c('observations', 'variance', 'infinity', 'observationsPairwise'),
                       all.target=c(var1, var2), observations.amount='< 2', 
                       observationsPairwise.amount=2)
}

.getMatrixPlot <- function(jaspResults, options, ready=TRUE) {
  if (!options[["plotMatrix"]]) 
    ready <- FALSE
  
  tests <- .getCorTests(options)
  
  if (length(tests) ==0) 
    ready <- FALSE
  
  if (!ready)
    return(NULL)
  
  nVariables <- length(options[["variables"]])
  
  matrixPlot <- jaspResults[["matrixPlot"]]
  
  if (!is.null(matrixPlot)) {
    return(matrixPlot)
  } else {
    matrixPlot <- createJaspPlot(title="Bayesian Correlation Matrix Plot")
    matrixPlot$position <- 2
    # matrixPlot$dependOn(c("variables", "pearson", "spearman", "kendall", "alternative", 
    #                       "plotMatrix", "plotMatrixDensities", "plotMatrixPosteriors"))
    matrixPlot$dependOn(c("plotMatrix", "plotMatrixDensities", "plotMatrixPosteriors"))
    
    # Note (Alexander): The other dependencies set on the container are
    # 
    #     "variables", "pearson", "spearman", "kendall", "alternative", 
    #     "missingValues", "bayesFactorType", "kappa"
    # 
    # matrixPlot$dependOn(c("plotMatrix", "plotMatrixDensities", "plotMatrixPosteriors"))
    # matrixPlot$position <- 2
    
    if (nVariables <= 2 & (options[["plotMatrixDensities"]] | options[["plotMatrixPosteriors"]])) {
      matrixPlot[["width"]] <- 580
      matrixPlot[["height"]] <- 580
    } else if (nVariables <= 2) {
      matrixPlot[["width"]] <- 400
      matrixPlot[["height"]] <- 400
    } else {
      matrixPlot[["width"]] <- 250 * nVariables + 20
      matrixPlot[["height"]] <- 250 * nVariables + 20
    }
    
    jaspResults[["matrixPlot"]] <- matrixPlot
  }
  
  # if (!ready)
  #   return(matrixPlot)
  # 
  return(matrixPlot)
}

.drawMatrixPlotCorBayes <- function(matrixPlot, allBfObjects, dataset, options) {
  plotResult <- matrixPlot$plotObject
  
  if (!is.null(plotResult)) {
    return()
  }
  
  matrixPlot$status <- "running"
  
  tests <- .getCorTests(options)
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
        # TODO(Alexander): In function, check and save
        # 
        
        if (!options[["plotMatrixPosteriors"]]) {
          posteriorPlot <- list()
        } else {
          posteriorPlot <- .drawPosteriorPlotCorBayes(bfObjectBig, options, tests, mode="matrix")
        }
        plotMat[[col, row]] <- posteriorPlot
        
        dataError <- bfObjectBig[[1]][["dataError"]]
        
        if (!is.null(dataError)) {
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
          densityPlot <-.bCorMarginalDistribution(variable = dataset[, .v(vars[row]), drop=TRUE], 
                                                  varName = vars[row], options = options)
        }
        plotMat[[row, row]] <- densityPlot
      }
    }
  }
  
  if (options[["plotMatrixDensities"]]) {
    densityPlot <-.bCorMarginalDistribution(variable = dataset[, .v(vars[nVariables]), drop=TRUE], 
                                            varName = vars[nVariables], options = options)
    plotMat[[nVariables, nVariables]] <- densityPlot
  }
  
  obj <- try(JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = vars, topLabels = vars,
                                      scaleXYlabels = NULL))
  
  if (isTryError(obj)) {
    matrixPlot$setError(.extractErrorMessage(obj))
    # plotResult$setError(.extractErrorMessage(obj))
  } else {
    matrixPlot$plotObject <- obj
    # plotResult <- obj
  }
  return()
}



.drawPosteriorPlotCorBayes <- function(bfObjectBig, options, tests, mode="matrix") {
  plotResult <- NULL
  alternative <- options[["alternative"]]
  
  xAxis <- list("pearson"=expression(paste("Pearson's ", rho)), 
                "spearman"=expression(paste("Spearman's ", rho)), 
                "kendall"=expression(paste("Kendall's ", tau))
  )
  
  if (mode=="matrix") {
    if (!options[["plotMatrixPosteriors"]]) {
      plotResult <- list()
      return(plotResult)
    }
  } else if (mode=="pairs") {
    if (!is.null(options[["plotPriorPosteriors"]])) {
      plotResult <- list()
      return(plotResult)
    }
  }
  
  dataError <- bfObjectBig[[1]][["error"]]
  
  if (!is.null(dataError)) {
    plotResult <- .displayError(errorMessage = dataError)
    return(plotResult)
  }
  
  allPosteriorLines <- vector("list", length=length(tests))
  
  error <- NULL
  
  for (m in seq_along(tests)) {
    testName <- tests[m]
    bfObject <- bfObjectBig[[testName]]
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
    
    if (is.na(posteriorLine)) {
      # TODO(Alexander): This means that there's an error
      # RETURN FAILED PLOT
      error <- "Could not compute the posteriors"
      allPosteriorLines <- NA
      break()
    }
    allPosteriorLines[[m]] <- posteriorLine
  }
  
  # Make fail/success plot here
  # 
  if (!is.null(error)) {
    plotResult <- .displayError(errorMessage = error)
    return(plotResult)
  }
  
  n <- length(sidedObject[["xDomain"]])
    
  if (length(tests) == 1) {
    # TODO(Alexander)
    if (tests=="pearson") {
      xLab <- expression(paste("Pearson's", ~rho))
    } else if (tests=="kendall") {
      xLab <- expression(paste("Kendall's", ~tau))
    } else if (tests=="spearman") {
      xLab <- expression(paste("Spearman's", ~rho))
    }
    dfLines <- data.frame(
      x = sidedObject[["xDomain"]],
      y = allPosteriorLines[[1]]
    )
  } else {
    n <- length(sidedObject[["xDomain"]])
    xLab <- "Correlation Coefficient"
    
    if (length(tests)==2) {
      dfLines <- data.frame(
        x = sidedObject[["xDomain"]],
        y = unlist(allPosteriorLines),
        g = rep(c(rho, tau), each=n)
      )
    } else if (length(tests)==3) {
      dfLines <- data.frame(
        x = sidedObject[["xDomain"]],
        y = unlist(allPosteriorLines),
        g = rep(c(rho, tau), each=n)
      )
    }
  }
  
  if (mode=="matrix") {
    plotResult <- JASPgraphs::PlotPriorAndPosterior(dfLines)
  } else if (mode=="pairs") {
    plotResult <- JASPgraphs::PlotPriorAndPosterior(dfLines, 
                                                    xName = xLab)
  }
  return(plotResult)
}

.setPairCollectionCorBayes <- function(pairsPlotCollection, options, ready) {
  
  if (!ready) 
    return(pairsPlotCollection)
  
  plotItems <- .getCorPlotItems(options)
  nPairs <- .getPairsLength(options)
  
  bfPlotDependencies <- c("pairsTest", "kappa", "alternative", "bayesFactorType")
  
  plotItemDependencies <- list(
    "plotScatter"=c("plotScatter", "plotScatterAddInfo"),
    "plotPriorPosterior"=c("plotPriorPosterior", bfPlotDependencies,
                           "plotPriorPosteriorAddTestingInfo", "plotPriorPosteriorAddEstimationInfo"),
    "plotBfRobustness"=c("plotBfRobustness", "plotBfRobustnessAddInfo", bfPlotDependencies),
    "plotBfSequential"=c("plotBfSequential", "plotBfSequentialAddInfo", bfPlotDependencies)
  )
  
  plotTitles <- list("plotScatter"="Scatterplot", 
                     "plotPriorPosterior"="Prior and Posterior", 
                     "plotBfRobustness"="Bayes Factor Robustness Check", 
                     "plotBfSequential"="Sequential Analysis")
  
  nPairs <- .getPairsLength(options)
  pairs <- options[["pairs"]]
  for (i in seq_len(nPairs)) {
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
      itemTitle <- plotTitles[[item]]
      itemPlot <- pairContainer[[itemTitle]]
      
      if (is.null(itemPlot)) {
        itemPlot <- createJaspPlot(title=itemTitle, width=530, height=400)
        itemPlot$dependOn(options = plotItemDependencies[[item]])
        itemPlot$position <- i
        pairContainer[[item]] <- itemPlot
      }
    }
  }
  
  return(pairsPlotCollection)
}

.drawPairsPlotCorBayes <- function(pairsPlotCollection, allBfObjects, dataset, options) {
  plotItems <- .getCorPlotItems(options)
  pairs <- options[["pairs"]]
  nPairs <- .getPairsLength(options)
  alternative <- options[["alternative"]]
  thisTest <- options[["pairsTest"]]

  for (i in seq_len(nPairs)) {
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
          scatterPlot <- try(.bCorScatter(x=subData[, 1, drop=TRUE], y=subData[, 2, drop=TRUE], options))
          
          if (isTryError(scatterPlot)) {
            jaspPlotResult$setError(.extractErrorMessage(scatterPlot))
          } else {
            jaspPlotResult$plotObject <- scatterPlot
          }
        } else if (item=="plotPriorPosterior") {
          plotResult <- .drawPosteriorPlotCorBayes(bfObjectBig, options, thisTest, mode="pairs")
          jaspPlotResult$plotObject <- plotResult
          # print("iets")
        } else if (item=="plotBfRobustness") {
          print("iets")
        } else if (item=="plotBfSequential") {
          print("iets")
        }
      }
    }
  }
  return()
}


