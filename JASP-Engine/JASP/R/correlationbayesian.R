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
  ready <- (length(options[["variables"]]) >= 2 | length(options[["pairs"]]) > 1)
  
  corBayesTable <- .markUpCorBayesTable(options, ready)
  
  # TODO(Alexander) Perhaps think about containers here already 
  # if we want to pair the main table to the matrix plot
  # 
  jaspResults[["corBayesTable"]] <- corBayesTable
  
  # TODO(Alexander) Also mark up for pairwise container, all of this can be done without data
  # 
  
  if (!ready) 
    return()
  
  # Note(Alexander): Everytime a variable (pair) is added to options$variables (options$pair) 
  # the data are read
  # 
  if (ready) {
    if (is.null(dataset)) {
      dataset <- .corBayesReadData(dataset, options)
    }
  }
  
  allBfObjects <- .computeCorBayes(jaspResults, dataset, options)
  
  .fillCorBayesTable(corBayesTable, options, allBfObjects)
  
  # Store state --------
  # 
  if (options[["missingValues"]]=="excludeListwise") {
    stateDependencies <- c("missingValues", "kappa", "variables")
  } else if (options[["missingValues"]]=="excludePairwise") {
    stateDependencies <- c("missingValues", "kappa")
  }
  jaspResults[["bfState"]] <- createJaspState(allBfObjects, dependencies=stateDependencies)
  # .markUpCorBayesTable <- function(jaspResults, options, ready)
  # 
  .corBayesPlotCorMatrix(jaspResults, allBfObjects, dataset, options, ready)
  
  # Pairwise stuff
  # .corBayesPlotScatter(jaspResults, dataset, options, ready)
  # .corBayesPlotPosteriors(jaspResults, allBfObjects, dataset, options, ready)
  # .corBayesPlotRobustness(jaspResults, dataset, options, ready)
  # .corBayesPlotSequential(jaspResults, dataset, options, ready)
}

.corBayesReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    allVariables <- unique(unlist(options[["variables"]]))
    allVariables <- allVariables[allVariables != ""]
    if (options[["missingValues"]] == "excludeListwise") {
      dataset <- .readDataSetToEnd(columns.as.numeric=allVariables, exclude.na.listwise=allVariables)
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric=allVariables)
    }
    return(dataset)
  }
}

# .corBayesTableMain <- function(jaspResults, dataset, options, ready) {
#   if (!is.null(jaspResults[["corBayesTable"]]))
#     return()
#   
#   corBayesTable <- .markUpCorBayesTable(options, ready)
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
#   # .markUpCorBayesTable <- function(jaspResults, options, ready)
#   # 
#   if (!ready)
#     return()
#   
#   return(allBfObjects)
# }

.markUpCorBayesTable <- function(options, ready) {
  if (!is.null(jaspResults[["corBayesTable"]]))
    return()
  
  tests <- .getCorTests(options[["pearson"]], options[["kendall"]], options[["spearman"]])
  # Get table title
  # 
  if (length(tests)==1) {
    tabTitle <- switch(tests,
                       "pearson"="Bayesian Pearson Correlation",
                       "kendall"="Bayesian Kendall Correlation",
                       "spearman"="Bayesian Spearman Correlation"
    )
  } else {
    tabTitle <-"Bayesian Correlation Table"
  }
  
  # Create table
  # 
  corBayesTable <- createJaspTable(title=tabTitle)
  
  # Define dependence
  # 
  corBayesTable$dependOn(c("variables", "pearson", "spearman", "kendall", "displayPairwise",
                           "reportBayesFactors", "flagSupported", "ci", "ciValue", 
                           "alternative", "missingValues", "bayesFactorType", "reportN", "posteriorMedian", 
                           "kappa"))
  
  corBayesTable$showSpecifiedColumnsOnly <- TRUE
  
  # Add citations
  # 
  if (!is.null(tests)) {
    corBayesTable$addCitation(unlist(.bCorCitationsList[tests], use.names=FALSE))
  }
  
  # Add footnote for hypothesis type
  # 
  if (options[["alternative"]]=="greater") 
    corBayesTable$addFootnote(message="For all tests, the alternative hypothesis specifies that the correlation is positive.",
                              symbol="<i>Note</i>.")
  
  if (options[["alternative"]]=="less") 
    corBayesTable$addFootnote(message="For all tests, the alternative hypothesis specifies that the correlation is negative.", 
                              symbol="<i>Note</i>.")
  
  # Get Bayes factor title
  bfTitle <- .getBfTitle(options[["bayesFactorType"]], options[["alternative"]])
  
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
      overTitle <- NULL
      if (length(tests) > 1) {
        # Overwrite the .corTestNamesList with on that is broken up in overtitle and testname
        # 
        overTitle <- .corOverTitlesList[[tests[m]]]
        .corTestNamesList <- list(pearson="r", spearman="rho", kendall="tau B")
      }
      
      # Add's "r", "rho", "tau B"
      corBayesTable$addColumnInfo(name=paste0(tests[m], "stat"), title=.corTestNamesList[[tests[m]]], 
                                  overtitle=overTitle, type="number")
      
      if (options[["reportBayesFactors"]]) 
        corBayesTable$addColumnInfo(name=paste0(tests[m], "bf"), title=bfTitle, overtitle=overTitle, type="number")
      
      # # TODO(ALEXANDER): Also report error %? Only useful for mcmc
      # # 
      # if (options[["reportPercentageError"]]) 
      #   corBayesTable$addColumnInfo(name=paste0(tests[m], "Bf"), title=bfTitle, overtitle=overTitle, type="number")
      
      # TODO(Alexander): Do we want to show the posterior median?
      # 
      # if (options[["posteriorMedian"]]) 
      #   corBayesTable$addColumnInfo(name="posteriorMedian", title="Posterior Median", type="number")
      # 
      if (options[["ci"]]) {
        corBayesTable$addColumnInfo(name=paste0(tests[m], "lowerCi"), overtitle=overTitle, type="number",
                                    title=paste0("Lower ", options[["ciValue"]]*100, "% CI"))
        
        corBayesTable$addColumnInfo(name=paste0(tests[m], "upperCi"), overtitle=overTitle, type="number",
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
  return(corBayesTable)
  # Assign table to jaspResults
}

.computeCorBayes <- function(jaspResults, dataset, options) {
  # Check and create state  --------
  # 
  if (is.null(jaspResults[["bfState"]])) {
    bfState <- createJaspState()
  } else {
    bfState <- jaspResults[["bfState"]]
  }
  
  # Retrieve from state
  # 
  allBfObjects <- bfState$object
  
  tests <- .getCorTests(options[["pearson"]], options[["kendall"]], options[["spearman"]])
  
  if (length(options[["variables"]]) <= 1) {
    # Don't compute
    return()
  }
  
  # TODO(Alexander): This seems redundant, because we've read the data at a higher level, but only if ready
  # Furthermore, this implies that it needs to read the data everytime it computes, but then again at a higher 
  # level the data is read everytime a variable (pair) is added to options$variables (options$pair)
  # 
  if (is.null(dataset)) 
    dataset <- .corBayesReadData(dataset, options)
  
  if (options[["missingValues"]]=="excludeListwise") {
    .hasErrors(dataset, type="observations", observations.amount='< 2', exitAnalysisIfErrors=TRUE)
  }
  
  pairs <- combn(options[["variables"]], 2, simplify=FALSE)
  
  for (pair in pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    pairName <- paste(sort(c(var1, var2)), collapse="-")
    
    v1 <- NULL
    v2 <- NULL
    
    for (m in seq_along(tests)) {
      storeObject <- FALSE
      bfObject <- allBfObjects[[pairName]][[tests[m]]]
      
      if (is.null(bfObject)) {
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
        
        bfObject <- bcor.test(x=v1, y=v2, kappa=options[["kappa"]], 
                              method=tests[m], ciValue=options[["ciValue"]])
        
        bfObject[["error"]] <- msg
        storeObject <- TRUE
      }
      
      if (options[["ciValue"]] != bfObject[["ciValue"]]) {
        tempList <- .computePearsonCredibleInterval("betaA"=bfObject[["betaA"]], "betaB"=bfObject[["betaB"]],
                                                    "ciValue"=options[["ciValue"]])
        bfObject <- modifyList(bfObject, tempList)
        storeObject <- TRUE
      }
      
      if (storeObject) {
        allBfObjects[[pairName]][[tests[m]]] <- bfObject
      }
    }
  }
  return(allBfObjects)
}

# .fillCorBayesTableEmpty <- function(table, options, allBfObjects) {
#   
# }

.fillCorBayesTable <- function(table, options, allBfObjects) {
  tests <- .getCorTests(options[["pearson"]], options[["kendall"]], options[["spearman"]])
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
        bfObject <- allBfObjects[[pairName]][[tests[m]]]
        
        errorMessage <- bfObject[["error"]]
        sidedObject <- .getSidedObject(bfObject, alternative=options[["alternative"]], 
                                     itemNames=c("stat", "bf", "lowerCi", "upperCi"))
        sampleSize <- sidedObject[["n"]]
        reportBf <- sidedObject[["bf"]]
        objNames <- names(sidedObject)
        newNames <- purrr::map_chr(objNames, function(x, y){paste0(y, x)}, y=tests[m])
        names(sidedObject) <- newNames
        sidedObject[["n"]] <- sampleSize
        
        if (is.null(errorMessage)) {
          if (options[["flagSupported"]]) {
            if (!is.na(reportBf)) {
              supportRowName <- c(supportRowName, list(pairName))
              supportColName <- c(supportColName, list(paste0(tests[m], "stat")))
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
          errorColName <- c(errorColName, list(paste0(tests[m], "stat")))
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
            bfObject <- allBfObjects[[pairName]][[tests[m]]]
            
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
                  supportRowName <- c(supportRowName, list(paste0(var1, tests[m], "stat")))
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
              errorRowName <- c(errorRowName, list(paste0(var1, tests[m], "stat")))
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
        
        rowItemNames <- .bCorRowNames(options, itemNames, test=tests[m])
        
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
          table$addRows(tempRow, rowNames=paste0(var1, tests[m], itemNames[k]))
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

.corBayesPlotCorMatrix <- function(jaspResults, allBfObjects, dataset, options, ready) {
  
}

.corBayesPlots <- function(jaspResults, allBfObjects, options, ready) {
  return()
}

.corBayesPlotPosteriors <- function(jaspResults, allBfObjects, options, ready) {
  if (!options[["plotPriorAndPosterior"]])
    return()
  
  plot <- createJaspPlot(
    title       = "Prior and Posterior",
    width       = 530,
    height      = 400,
    aspectRatio = 0.7
  )
  
  plot$position <- 2
  options$plotBayesFactorRobustnessAdditionalInfo
  plot$dependOn(options = c("plotPriorAndPosterior", "plotPriorAndPosteriorAddInfo"))
  jaspResults[["priorPosteriorPlot"]] <- plot
  
  if (!ready || jaspResults[["corBayesTable"]]$getError())
    return()
  
  # TODO*(Alexanader): Compute here
  # plotResult <- 
  .Machine$double.eps
  # error check: Posterior too peaked?
  if(abs(CIlower - CIupper) <= .Machine$double.eps){
    plot$setError("Plotting not possible: Posterior too peaked!")
    return()
  }
  
  ppCri           <- c(CIlower, CIupper)
  dfLinesPP       <- .dfLinesPP(dataset = NULL, a = a, b = b, hyp = alternative, theta0 = theta0, counts = successes, n = n)
  dfPointsPP      <- .dfPointsPP(dataset = NULL, a = a, b = b, hyp = alternative, theta0 = theta0, counts = successes, n = n)
  xName           <- expression(paste("Population proportion ", theta))
  
  # error check: Cannot evaluate prior or posterior density?
  if(any(is.na(c(dfPointsPP$y, dfLinesPP$y))) || any(is.infinite(c(dfPointsPP$y, dfLinesPP$y)))){
    plot$setError("Plotting not possible: Cannot evaluate prior or posterior density!")
    return()
  }
  
  if(options$plotPriorAndPosteriorAdditionalInfo){
    
    # error check: infinite Bayes factors?
    if(!is.numeric(BF10) || is.infinite(BF10)){
      plot$setError("Plotting not possible: Bayes factor should be numeric!")
      return()
    }
    
    p <- try(JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName, BF = 1/BF10,
                                               bfType = options$bfType,
                                               CRI = ppCri, median = medianPosterior, drawCRItxt = TRUE))
  } 
  else {
    p <- try(JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName))
  }
  
  # create JASP object
  if (isTryError(p)) {
    errorMessage <- paste("Plotting not possible:", .extractErrorMessage(p))
    plot$setError(errorMessage)
  } else {
    plot$plotObject <- p
  }
  return()
  
  
}