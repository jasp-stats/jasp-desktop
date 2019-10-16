#
# Copyright (C) 2013-2018 University of Amsterdam
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
Correlation <- function(jaspResults, dataset, options){
  
  # Read dataset ----
  dataset <- .corrReadData(dataset, options)
  # Error checks ----
  ready <- length(options$variables) >= 2
  errors <- .corrCheckErrors(dataset, options)
  
  # Init main table ----
  mainTable <- .corrInitMainTable(jaspResults, options, errors)
  
  # Compute results ----
  corrResults <- .corrComputeResults(jaspResults, dataset, options, ready)
  
  # Fill tables and plots ----
  .corrFillTableMain(mainTable, corrResults, options, ready)
  .corrAssumptions(jaspResults, dataset, options, ready, corrResults)
  .corrPlot(jaspResults, dataset, options, ready, corrResults, errors)
  
  return()
}

# Preprocessing functions ----
.corrReadData <- function(dataset, options){
  if(length(options$conditioningVariables) == 0){
    cond <- FALSE
    vars <- options$variables
  } else{
    cond <- TRUE
    vars <- c(options$variables, options$conditioningVariables)
  }
  
  if(!is.null(dataset)){
    return(dataset)
  } else if(options$missingValues == "excludePairwise"){
    data <- .readDataSetToEnd(columns.as.numeric = vars)
    if(cond) data <- data[complete.cases(data[,.v(options$conditioningVariables)]),,drop=FALSE]
    return(data)
  } else if(options$missingValues == "excludeListwise"){
    return(.readDataSetToEnd(columns.as.numeric = vars, exclude.na.listwise = vars))
  }
}

.corrCheckErrors <- function(dataset, options){
  if(length(options$variables) == 0) return("No variables")
  if(length(options$variables) == 1) return("One variable")
  
  # Check for variance, infinity and observations
  # does not check pairwise observations yet
  errors <-.hasErrors(dataset, message = 'default', 
                      type = c('variance', 'infinity', 'observations'),
                      all.target = c(options$variables, options$conditioningVariables),
                      observations.amount = "< 3",
                      exitAnalysisIfErrors = FALSE)
  
  return(errors)
}

# Init tables ----
.corrInitMainTable <- function(jaspResults, options, errors){
  if(!is.null(jaspResults[['mainTable']])) return(jaspResults[['mainTable']])
  
  if(errors == "No variables"){
    variables <- c("...", "... ") # we need this trailing space so that 1 != 2
  } else if(errors == "One variable"){
    variables <- c(options[['variables']], "...")
  } else {
    variables <- options[['variables']]
  }
  
  if(length(options$conditioningVariables) == 0){
    title <- "Correlation table"
  } else{
    title <- "Partial correlation table"
  }
  
  mainTable <- createJaspTable(title = title)
  mainTable$dependOn(c("variables", "conditioningVariables",
                       "pearson", "spearman", "kendallsTauB", "displayPairwise", "reportSignificance",
                       "flagSignificant", "sampleSize",
                       "confidenceIntervals", "confidenceIntervalsInterval",
                       "VovkSellkeMPR", "hypothesis", "missingValues"))
  mainTable$position <- 1
  
  mainTable$showSpecifiedColumnsOnly <- TRUE
  
  if(options[['displayPairwise']]){
    mainTable <- .corrInitPairwiseTable(mainTable, options, variables)
  } else{
    mainTable <- .corrInitCorrelationTable(mainTable, options, variables)
  }
  
  if(options[['flagSignificant']])
    mainTable$addFootnote(message = "p < .05, ** p < .01, *** p < .001", symbol = "*")
  
  if(length(options$conditioningVariables) >= 0){
    message <- sprintf("Conditioned on variables: %s", paste(options$conditioningVariables, collapse = ", "))
    mainTable$addFootnote(message = message, symbol = " ")
  }
  # show
  jaspResults[['mainTable']] <- mainTable
  
  return(mainTable)
}

.corrInitPairwiseTable <- function(mainTable, options, variables){
  pairs <- combn(.v(variables), 2, simplify = FALSE)
  pairTitles <- combn(variables, 2, simplify = FALSE)
  
  mainTable$addColumnInfo(name = "variable1", title = "", type = "string")
  mainTable$addColumnInfo(name = "separator", title = "", type = "string")
  mainTable$addColumnInfo(name = "variable2", title = "", type = "string")

  mainTable[['variable1']] <- sapply(pairTitles, function(x) x[[1]])
  mainTable[['separator']] <- rep("-", length(pairTitles))
  mainTable[['variable2']] <- sapply(pairTitles, function(x) x[[2]])

  
  mainTable$setExpectedSize(rows = length(pairs))
  for(row in 1:length(pairs)){
    rowName <- paste(pairs[[row]], collapse = "_")
    
    mainTable$setRowName(row, rowName)
  }
  
  options[["kendall"]] <- options[["kendallsTauB"]]
  
  tests <- c("pearson", "spearman", "kendall")
  nTests <- sum(unlist(options[tests]))
  testNames <- c(pearson="Pearson", spearman="Spearman", kendall="Kendall")
  
  if(options$sampleSize) mainTable$addColumnInfo(name = "sample.size", title = "n", type = "integer")
  
  for(test in tests){
    if(options[[test]]){
      if(nTests != 1){
        overtitle <- testNames[test]
      } else{
        overtitle <- NULL
      }
      
      mainTable$addColumnInfo(name = paste0(test, "_estimate"), title = .corrTitlerer(test, nTests),
                              type = "number", overtitle = overtitle)
      
      if(options$reportSignificance)
        mainTable$addColumnInfo(name = paste0(test, "_p.value"), title = "p", type = "pvalue", overtitle = overtitle)
      
      if(options$confidenceIntervals){
        mainTable$addColumnInfo(name = paste0(test, "_lower.ci"), 
                                title = sprintf("Lower %s%% CI", 100*options$confidenceIntervalsInterval), type = "number",
                                overtitle = overtitle)
        mainTable$addColumnInfo(name = paste0(test, "_upper.ci"), 
                                title = sprintf("Upper %s%% CI", 100*options$confidenceIntervalsInterval), type = "number",
                                overtitle = overtitle)
      }
      
      if(options$VovkSellkeMPR){
        mainTable$addColumnInfo(name = paste0(test, "_vsmpr"), title = "VS-MPR", type = "number", overtitle = overtitle)
        mainTable$addFootnote(message = .corrTexts$footnotes$VSMPR, symbol = "\u002A", colNames = paste0(test, "_vsmpr"))
        mainTable$addCitation(.corrTexts$references$Sellke_etal_2001)
      }
    }
  }
  
  return(mainTable) 
}

.corrTitlerer <- function(test, nTests){
  if(nTests > 1){
    coeffs <- c(pearson = "r", spearman = "rho", kendall = "tau B")
  } else{
    coeffs <- c(pearson = "Pearson's r", spearman = "Spearman's rho", kendall = "Kendall's tau B")
  }
  
  return(coeffs[test])
}

.corrInitCorrelationTable <- function(mainTable, options, variables){
  mainTable$transpose <- TRUE
  mainTable$transposeWithOvertitle <- FALSE
  
  mainTable$addColumnInfo(name = "var1", title = "", type = "string", combine = FALSE, overtitle = "Variable")
  mainTable$addColumns(list(var1 = variables))
  
  whichtests <- c(options$pearson, options$spearman, options$kendallsTauB)
  
  
  testsTitles <- c("Pearson's r", "Spearman's rho", "Kendall's Tau B")[whichtests]
  tests <- c("pearson", "spearman", "kendall")[whichtests]
  
  for(vi in seq_along(variables)){
    overtitle <- paste(vi, variables[vi], sep = ". ")
    
    if(options$sampleSize) {
      mainTable$addColumnInfo(name = paste(.v(variables[vi]), "sample.size", sep = "_"), title = "n",
                              type = "integer", overtitle = overtitle)
    }
    
    for(ti in seq_along(tests)){
      .corrInitCorrelationTableRowAsColumn(mainTable, options, variables[vi], testsTitles[ti], tests[ti], overtitle)
    }
    mainTable$setRowName(vi, .v(variables[vi]))
  }
  
  return(mainTable)
}

.corrInitCorrelationTableRowAsColumn <- function(mainTable, options, var, coeff, test, overtitle){
  vvar <- .v(var)
  name <- paste(vvar, test, "%s", sep = "_")
  
  mainTable$addColumnInfo(name = sprintf(name, "estimate"), title = coeff, type = "number", overtitle = overtitle)
  
  if(options$reportSignificance)
    mainTable$addColumnInfo(name = sprintf(name, "p.value"), title = "p-value", type = "pvalue", overtitle = overtitle)
  
  if(options$VovkSellkeMPR){
    mainTable$addColumnInfo(name = sprintf(name, "vsmpr"), title = "VS-MPR", type = "number", overtitle = overtitle)
    mainTable$addFootnote(colNames = sprintf(name, "vsmpr"), symbol = "\u002A",
                          message = .corrTexts$footnotes$VSMPR)
    mainTable$addCitation(.corrTexts$references$Sellke_etal_2001)
  }
  
  if(options$confidenceIntervals){
    mainTable$addColumnInfo(name = sprintf(name, "upper.ci"), 
                            title = sprintf("Upper %s%% CI", 100*options$confidenceIntervalsInterval),
                            type = "number", overtitle = overtitle)
    mainTable$addColumnInfo(name = sprintf(name, "lower.ci"), 
                            title = sprintf("Lower %s%% CI", 100*options$confidenceIntervalsInterval),
                            type = "number", overtitle = overtitle)
  }
}

### Compute results ----
.corrComputeResults <- function(jaspResults, dataset, options, ready){
  if(!ready) return()
  if(!is.null(jaspResults[['results']])) return(jaspResults[['results']]$object)

  vvars <- .v(options[['variables']])
  vcomb <- combn(vvars, 2, simplify = FALSE)
  vpair <- sapply(vcomb, paste, collapse = "_")

  
  alt <- c(correlated = "two.sided",
           correlatedNegatively = "less",
           correlatedPositively = "greater")[options$hypothesis]
  
  if(length(options$conditioningVariables) == 0){
    pcor <- FALSE
  } else{
    pcor <- TRUE
  }
  
  results <- list()
  for(i in seq_along(vpair)){
    # some variable pairs might be reusable, so we don't need to compute them again
    if(!is.null(jaspResults[[vpair[i]]])) {
      results[[vpair[i]]] <- jaspResults[[vpair[i]]]$object
    } else {
      data <- dataset[vcomb[[i]]]
      whichComplete <- complete.cases(data)
      data <- data[whichComplete,,drop=FALSE]
      
      if(pcor) {
        condData <- dataset[,.v(options$conditioningVariables), drop=FALSE]
        condData <- condData[whichComplete,,drop=FALSE]
      } else{
        condData <- NULL
      }
      
      errors <-.hasErrors(data, message = 'default', 
                          type = c('variance', 'infinity', 'observations'),
                          all.target = vcomb[i], observations.amount = "< 3",
                          exitAnalysisIfErrors = FALSE)
      
      stats <- c("estimate", "p.value", "conf.int", "vsmpr")
      statsNames <- c("estimate", "p.value", "lower.ci", "upper.ci", "vsmpr")
      
      if(isFALSE(errors)){
        pearson <- .corr.test(x = data[,1], y = data[,2], z = condData, 
                              method = "pearson", alternative = alt, 
                              conf.level = options$confidenceIntervalsInterval)$result
        spearman <- .corr.test(x = data[,1], y = data[,2], z = condData, 
                               method = "spearman", alternative = alt, 
                               conf.level = options$confidenceIntervalsInterval)$result
        kendall <- .corr.test(x = data[,1], y = data[,2], z = condData, 
                              method = "kendall", alternative = alt, 
                              conf.level = options$confidenceIntervalsInterval)$result
      } else {
        pearson <- spearman <- kendall <- rep(NaN, length(statsNames))
        names(pearson) <- names(spearman) <- names(kendall) <- statsNames
      }
      
      # stolen from manova
      shapiro <- .multivariateShapiroComputation(data, list(dependent = .unv(vcomb[[i]])))
      
      results[[vpair[i]]] <- list(vars = .unv(vcomb[[i]]), vvars = vcomb[[i]], 
                                  res = c(pearson, spearman, kendall, sample.size = nrow(data)),
                                  errors = errors, shapiro = shapiro)
      
      # store state for pair
      state <- createJaspState(object = results[[vpair[i]]])
      state$dependOn(options = c("hypothesis", "confidenceIntervalsInterval", "missingValues"), 
                     optionContainsValue = list(variables = .unv(vcomb[[i]])))
      
      jaspResults[[vpair[i]]] <- state
      
    } 
  }
  
  
  jaspResults[['results']] <- createJaspState(object = results)
  jaspResults[['results']]$dependOn(options = c("variables", "conditioningVariables",  "hypothesis",
                                                "confidenceIntervalsInterval", "missingValues"))
  
  
  return(results)
}

.corrAssumptions <- function(jaspResults, dataset, options, ready, corrResults){
  if(isFALSE(options$multivariateShapiro) && isFALSE(options$pairwiseShapiro)) return()
  if(!is.null(jaspResults[['assumptionsContainer']])) return()
  
  assumptionsContainer <- createJaspContainer(title = "Assumption checks")
  assumptionsContainer$dependOn(c("multivariateShapiro", "pairwiseShapiro", "variables", "missingValues"))
  assumptionsContainer$position <- 2
  
  jaspResults[['assumptionsContainer']] <- assumptionsContainer
  if(isTRUE(options$multivariateShapiro)){
    shapiroTable <- createJaspTable(title = "Shapiro-Wilk Test for Multivariate Normality")
    
    shapiroTable$showSpecifiedColumnsOnly <- TRUE
    
    shapiroTable$addColumnInfo(name = "W", title = "Shapiro-Wilk", type = "number")
    shapiroTable$addColumnInfo(name = "p", title = "p", type = "pvalue")
    
    assumptionsContainer[['multivariateShapiro']] <- shapiroTable
    
    if (ready) {
      dataset <- dataset[complete.cases(dataset),,drop=FALSE]
      shapiroResult <- .multivariateShapiroComputation(dataset, list(dependent = options$variables))
      shapiroErrors <- shapiroResult$errors
      shapiroResult <- shapiroResult$result
      shapiroTable$addRows(list(W = shapiroResult$statistic, p = shapiroResult$p.value))
      
      if (!is.null(shapiroErrors)) 
        shapiroTable$setError(shapiroErrors)
    }
  }
  
  if(isTRUE(options$pairwiseShapiro)){
    shapiroTable <- createJaspTable(title = "Shapiro-Wilk Test for Bivariate Normality")
    
    shapiroTable$showSpecifiedColumnsOnly <- TRUE
    
    shapiroTable$addColumnInfo(name = "var1", title = "", type = "string")
    shapiroTable$addColumnInfo(name = "separator", title = "", type = "string")
    shapiroTable$addColumnInfo(name = "var2", title = "", type = "string")
    shapiroTable$addColumnInfo(name = "W", title = "Shapiro-Wilk", type = "number")
    shapiroTable$addColumnInfo(name = "p", title = "p", type = "pvalue")
    
    shapiroTable$setExpectedSize(rows = max(1, choose(length(options$variables), 2)))
    
    assumptionsContainer[['pairwiseShapiro']] <- shapiroTable
    if(ready){
      for(i in seq_along(corrResults)){
        res <- corrResults[[i]]
        
        shapiroTable$addRows(list(
          var1 = res$vars[1], separator = "-", var2 = res$vars[2],
          W = res$shapiro$result$statistic, p = res$shapiro$result$p.value
        ))
        
        
        name <- paste(res$vvars, collapse = "_")
        shapiroTable$setRowName(rowIndex = i, newName = name)
        if(!is.null(res$shapiro$errors)){
          shapiroTable$addFootnote(message = res$shapiro$errors, rowNames = name)
        }
      }
    }
  }
  
}
### Fill Tables ----
.corrFillTableMain <- function(mainTable, corrResults, options, ready){
  if(!ready) return()
  
  if(options$displayPairwise){
    .corrFillPairwiseTable(mainTable, corrResults, options)
  } else{
    .corrFillCorrelationTable(mainTable, corrResults, options)
  }
}

.corrFillPairwiseTable <- function(mainTable, corrResults, options){
  # extract the list of results
  results <- lapply(corrResults, function(x) x[['res']])
  
  # the stored results can be out of order -> we need to identify the order from the order of the
  # variables in the options list
  combos <- combn(.v(options$variables), 2, simplify = FALSE)
  pairs <- sapply(combos, paste, collapse = "_")
  
  results <- results[pairs]
  
  # now rbind the list so that we can access the columns
  results <- as.data.frame(do.call(rbind, results))
  
  # fill all columns
  for(col in colnames(results)) mainTable[[col]] <- results[[col]]
  
  if(options$flagSignificant){
     .corrFlagSignificant(mainTable, results[["pearson_p.value"]],   "pearson_estimate",  pairs)
     .corrFlagSignificant(mainTable, results[["spearman_p.value"]],  "spearman_estimate", pairs)
     .corrFlagSignificant(mainTable, results[["kendall_p.value"]],   "kendall_estimate",  pairs)
  }
}

.corrFlagSignificant <- function(table, p.values, colName, rowNames){
  p.values <- as.numeric(p.values)
  
  s <- rowNames[!is.na(p.values) & !is.nan(p.values) & p.values < 0.05 & p.values >= 0.01]
  if(length(s) > 0){
    table$addFootnote(colNames = colName, rowNames = s, symbol = "*")
  }
  
  ss <- rowNames[!is.na(p.values) & !is.nan(p.values) & p.values < 0.01 & p.values >= 0.001]
  if(length(ss) > 0){
    table$addFootnote(colNames = colName, rowNames = ss, symbol = "**")
  }
  
  sss <- rowNames[!is.na(p.values) & !is.nan(p.values) & p.values < 0.001]
  if(length(sss) > 0){
    table$addFootnote(colNames = colName, rowNames = sss, symbol = "***")
  }
}
  
.corrFillCorrelationTable <- function(mainTable, corrResults, options){
  vvars <- .v(options$variables)
  statsNames <- names(corrResults[[paste(vvars[1], vvars[2], sep = "_")]]$res)
  for(row in seq_along(options$variables)){
    res <- matrix(NA, nrow = length(options$variables), ncol = length(statsNames)) 
    res <- as.data.frame(res)
    
    for(col in seq_along(options$variables)){
      if(row == col){
        r <- rep(NA, length(statsNames))
      } else if(row < col){
        r <- rep(NA, length(statsNames))
      } else {
        r <- corrResults[[paste(vvars[col], vvars[row], sep = "_")]]$res
      }
      
      res[col, ] <- r
    }
    colnames(res) <- statsNames
    
    for(s in statsNames){
      mainTable[[paste(vvars[row], s, sep = "_")]] <- res[, s, drop=TRUE]
    }
    
    if(options$flagSignificant){
      .corrFlagSignificant(mainTable, res[["pearson_p.value"]],  sprintf("%s_pearson_estimate",  vvars[row]), vvars)
      .corrFlagSignificant(mainTable, res[["spearman_p.value"]], sprintf("%s_spearman_estimate", vvars[row]), vvars)
      .corrFlagSignificant(mainTable, res[["kendall_p.value"]],  sprintf("%s_kendall_estimate",  vvars[row]), vvars)
    }
  }
  
}
### Plot stuff ----
.corrPlot <- function(jaspResults, dataset, options, ready, corrResults, errors){
  if(!ready) return()
  if(isFALSE(options$plotCorrelationMatrix)) return()
  
  if(isTRUE(options[["displayPairwise"]])){
    .corrPairwisePlot(jaspResults, dataset, options, ready, corrResults, errors)
  } else{
    .corrMatrixPlot(jaspResults, dataset, options, ready, corrResults, errors)
  }
}

.corrPairwisePlot <- function(jaspResults, dataset, options, ready, corrResults, errors){
  if(!is.null(jaspResults[['corrPlot']])) return()
  
  plotContainer <- createJaspContainer(title = "Scatter plots")
  plotContainer$dependOn(options = c("variables", "pearson", "spearman", "kendallsTauB", "displayPairwise",
                                     "confidenceIntervals", "confidenceIntervalsInterval", "hypothesis",
                                     "plotCorrelationMatrix", "plotDensities", "plotStatistics", "missingValues"))
  jaspResults[['corrPlot']] <- plotContainer
  
  vars <- options$variables
  vvars <- .v(vars)
  
  comb <- combn(vars, 2, simplify = FALSE)
  pairs <- sapply(comb, paste, collapse = " vs. ")
  vcomb <- combn(vvars, 2, simplify = FALSE)
  vpairs <- sapply(vcomb, paste, collapse = "_")
  
  if(options[['plotDensities']]){
    for(i in seq_along(vcomb)){
      plot <- createJaspPlot(title = pairs[i], width = 550, height = 550)
      plotContainer[[vpairs[i]]] <- plot
      
      plotMat <- matrix(list(), 2, 2)
      
      data <- dataset[vcomb[[i]]]
      data <- data[complete.cases(data),]
      
      # get consistent breaks
      var1Breaks <- JASPgraphs::getPrettyAxisBreaks(c(data[,1], hist(data[,1], plot=FALSE)$breaks), min.n = 3)
      var2Breaks <- JASPgraphs::getPrettyAxisBreaks(c(data[,2], hist(data[,2], plot=FALSE)$breaks), min.n = 3)
      
      plotMat[[1, 1]] <- .corrMarginalDistribution(variable = data[,1,drop=TRUE],
                                                   options = options, errors = errors, yName = NULL)
      plotMat[[1, 2]] <- .corrValuePlot(corrResults[[vpairs[i]]], options = options)
      plotMat[[2, 1]] <- .corrScatter(corrResults[[vpairs[i]]], options = options,
                                      xVar = data[,1,drop=TRUE], yVar = data[,2,drop=TRUE], 
                                      xBreaks = var1Breaks, yBreaks = var2Breaks,
                                      drawAxes = FALSE)
      plotMat[[2, 2]] <- .corrMarginalDistribution(variable = data[,2,drop=TRUE],
                                                   options = options, errors = errors, yName = NULL, coord_flip = TRUE)
        
      
      plot$plotObject <- JASPgraphs::ggMatrixPlot(plotMat, 
                                                  bottomLabels = c(comb[[i]][1], "Density"),
                                                  leftLabels   = c("Density", comb[[i]][2]))
    }
  } else if(options[['plotStatistics']]){
    for(i in seq_along(vcomb)){
      plot <- createJaspPlot(title = pairs[i], width = 600, height = 300)
      plotContainer[[vpairs[i]]] <- plot
      
      data <- dataset[vcomb[[i]]]
      data <- data[complete.cases(data),]
      
      plotMat <- matrix(list(), 1, 2)
      plotMat[[1, 1]] <- .corrScatter(corrResults[[vpairs[i]]], options = options,
                                      xVar = data[,1,drop=TRUE], yVar = data[,1,drop=TRUE], 
                                      xName = comb[[i]][1], yName = comb[[i]][2],
                                      drawAxes = TRUE)
      plotMat[[1, 2]] <- .corrValuePlot(corrResults[[vpairs[i]]], options = options)
      
      plot$plotObject <- JASPgraphs::ggMatrixPlot(plotMat)
    }
  } else{
    for(i in seq_along(vcomb)){
      plot <- createJaspPlot(title = pairs[i], width = 400, height = 400)
      plotContainer[[vpairs[i]]] <- plot
      
      plot$plotObject <- .corrScatter(corrResults[[vpairs[i]]], options = options,
                                      xVar = dataset[[vcomb[[i]][1]]], yVar = dataset[[vcomb[[i]][2]]], 
                                      xName = comb[[i]][1], yName = comb[[i]][2])
    }
  }
}
.corrMatrixPlot <- function(jaspResults, dataset, options, ready, corrResults, errors){
  if(!is.null(jaspResults[['corrPlot']])) return()
  vars <- options$variables
  vvars <- .v(vars)
  len <- length(vars)
  
  
  plot <- createJaspPlot(title = "Correlation plot")
  plot$dependOn(options = c("variables", "pearson", "spearman", "kendallsTauB", "displayPairwise",
                            "confidenceIntervals", "confidenceIntervalsInterval", "hypothesis",
                            "plotCorrelationMatrix", "plotDensities", "plotStatistics", "missingValues"))
  
  if (len <= 2 && (options$plotDensities || options$plotStatistics)) {
    plot$width <- 580
    plot$height <- 580
  } else if (len <= 2) {
    plot$width <- 400
    plot$height <- 400
  } else {
    plot$width <- 250 * len + 20
    plot$height <- 250 * len + 20
  }
  
  
  jaspResults[['corrPlot']] <- plot
  
  
  plotMat <- matrix(list(), len, len)
  for(row in seq_len(len)){
    for(col in seq_len(len)){
      if(row == col) {
        plotMat[[row, col]] <- .corrMarginalDistribution(variable = dataset[[vvars[col]]],
                                                         options = options, errors = errors)
      } else if(row > col){
        plotMat[[row, col]] <- .corrValuePlot(corrResults[[paste(vvars[c(col, row)], collapse = "_")]],
                                              options = options)
      } else {
        plotMat[[row, col]] <- .corrScatter(corrResults[[paste(vvars[c(row, col)], collapse = "_")]],
                                            options = options,
                                            xVar = dataset[[vvars[col]]], yVar = dataset[[vvars[row]]])
      }
    }
  }
  
  p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = vars, topLabels = vars,
                                scaleXYlabels = NULL)
  plot$plotObject <- p
  
}

.corrValuePlot <- function(results, cexText= 2.5, cexCI= 1.7, options = options) {
  if(isFALSE(options$plotStatistics)) return(.displayError(errorMessage = ""))
  if(!isFALSE(results$errors)){
    return(.displayError(errorMessage = results$errors$message))
  }
  
  res <- results$res
  
  
  tests <- c()
  
  if (options$pearson)
    tests <- c(tests, "pearson")
  if (options$spearman)
    tests <- c(tests, "spearman")
  if (options$kendallsTauB)
    tests <- c(tests, "kendall")
  
  CIPossible <- rep(TRUE, length(tests))
  
  p <- ggplot2::ggplot(data = data.frame(), ggplot2::aes(x = seq_along(data.frame()),y = summary(data.frame()))) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = grid::unit(c(1,1,1,1), "cm")
    ) + ggplot2::xlim(0,2) + ggplot2::ylim(0,2)
  
  if(length(tests) == 0){
    return(p)
  } else if(length(tests) == 1){
    ypos <- 1.5
  } else if(length(tests) == 2){
    ypos <- c(1.7, 1.1)
  } else if(length(tests) == 3){
    ypos <- c(1.8, 1.2, .6)
  }
  
  lab <- rep(NA, length(tests))
  cilab <- rep(NA, length(tests))
  
  for(i in seq_along(tests)){
    estimate <- res[[paste(tests[i], "estimate", sep = "_")]]
    if(round(estimate, 8) == 1) {
      CIPossible[i] <- FALSE
      
      lab[i] <- switch(tests[i],
                       pearson =  paste(  "italic(r) == '1.000'"),
                       spearman = paste("italic(rho) == '1.000'"),
                       kendall =  paste("italic(tau) == '1.000'"))
    } else if(round(estimate, 8) == -1){
      CIPossible[i] <- FALSE
      
      lab[i] <- switch(tests[i],
                       pearson =  paste(  "italic(r) == '-1.000'"),
                       spearman = paste("italic(rho) == '-1.000'"),
                       kendall =  paste("italic(tau) == '-1.000'"))
    } else{
      lab[i] <- .corValueString(corValue = estimate, testType = tests[i], decimals = 3)
    }
    
    if(CIPossible[i]){
      lower.ci <- res[[paste(tests[i], "lower.ci", sep = "_")]]
      lower.ci <- formatC(lower.ci, format = "f", digits = 3)
      
      upper.ci <- res[[paste(tests[i], "upper.ci", sep = "_")]]
      upper.ci <- formatC(upper.ci, format = "f", digits = 3)
      
      cilab[i] <- sprintf("%s%% CI: [%s, %s]", 100*options$confidenceIntervalsInterval, lower.ci, upper.ci)
    } else{
      cilab[i] <- ""
    }
  }
  
  p <- p + ggplot2::geom_text(data = data.frame(x = rep(1, length(ypos)), y = ypos),
                              mapping = ggplot2::aes(x = x, y = y, label =lab),
                              size = 7, parse = TRUE)
  
  if(options$confidenceIntervals){
    p <- p + ggplot2::geom_text(data = data.frame(x = rep(1, length(ypos)), y = ypos - 0.25),
                                mapping = ggplot2::aes(x = x, y = y, label = cilab),
                                size = 5)
  }
  
  return(p)
}

.corrMarginalDistribution <- function(variable, varName, options, xName = NULL, yName = "Density", errors, coord_flip = FALSE){
  if(isFALSE(options$plotDensities)) return(.displayError(errorMessage = "")) # return empty plot
  if(!isFALSE(errors)){
    errorsInVariable <- sapply(errors[-"message"], function(x) varName %in% x)
    if(any(errorsInVariable)) return(.displayError(errorMessage = sprintf("Plotting not possible: %s", errors$message)))
  }
  
  if(isTRUE(options$plotRanks)) variable <- rank(variable)
  
  p <- .plotMarginalCor(variable = variable, xName = xName, yName = yName)
  
  if(coord_flip){
    p <- p + ggplot2::coord_flip() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_line(), axis.ticks.x = ggplot2::element_blank(), 
                     axis.text.x = ggplot2::element_blank())
  }
  
  p
}

.corrScatter <- function(results, options, xVar, yVar, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL, 
                         drawAxes = TRUE) {
  if(!isFALSE(results$errors)){
    return(.displayError(errorMessage = sprintf("Plotting not possible: %s", errors$message)))
  }
  
  if(isTRUE(options$plotRanks)) {
    xVar <- rank(xVar)
    yVar <- rank(yVar)
  }
  .plotScatter(xVar = xVar, yVar = yVar, xBreaks = xBreaks, yBreaks = yBreaks, xName = xName, yName = yName, 
               drawAxes = drawAxes)
}

## Old plotting ----

.plotCorrelations <- function(dataset, perform, options) {
  variables <- unlist(options$variables)
  correlation.plot <- NULL

   if (perform == "init" && length(variables) > 1) {
    l <- length(variables)

    if (l <= 2 && (options$plotDensities || options$plotStatistics)) {
      width <- 580
      height <- 580
    } else if (l <= 2) {
      width <- 400
      height <- 400
    } else {
      width <- 250 * l + 20
      height <- 250 * l + 20
    }

    plot <- list()

    plot[["title"]] <- "Correlation Plot"
    plot[["width"]]  <- width
    plot[["height"]] <- height

    correlation.plot <- plot
  } else if (perform == "run" && length(variables) > 1) {
    variable.statuses <- vector("list", length(variables))

    for (i in seq_along(variables)) {
      variable.statuses[[i]]$unplotable <- FALSE
      variable.statuses[[i]]$plottingError <- NULL
      errors <- .hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
                           all.target=variables[i], message="short", observations.amount="< 3")
      
      if (! identical(errors, FALSE)) {
        variable.statuses[[i]]$unplotable <- TRUE
        variable.statuses[[i]]$plottingError <- paste(strwrap(errors$message, 25), collapse="\n") # break msgs so they fit in the matrix
      }
    }

    variables <- .v(variables)
    l <- length(variables)

    if (l <= 2 && (options$plotDensities || options$plotStatistics)) {
      width <- 580
      height <- 580
    } else if (l <= 2) {
      width <- 400
      height <- 400
    } else {
      width <- 250 * l
      height <- 250 * l
    }

    plot <- list()

    plot[["title"]] <- "Correlation Plot"
    plot[["width"]]  <- width
    plot[["height"]] <- height

    correlation.plot <- plot
    cexText <- 1.6

    .plotFunc <- function() {

        plotMat <- matrix(list(), l, l)

        # minor adjustments to plot margin to avoid cutting off the x-axis labels
	       adjMargin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.25, .40, .25, .25), "cm"))

         oldFontSize <- JASPgraphs::getGraphOption("fontsize")
         JASPgraphs::setGraphOption("fontsize", .85 * oldFontSize)

            for (row in seq_len(l)) {
                for (col in seq_len(l)) {
                    if (row == col) {
                        if (options$plotDensities) {
                            if ( ! variable.statuses[[row]]$unplotable) {
                                plotMat[[row, col]] <- .plotMarginalCor(dataset[[variables[row]]]) + adjMargin # plot marginal (histogram with density estimator)
                            } else {
                                errorMessagePlot <- paste0("Undefined density:", "\n", variable.statuses[[row]]$plottingError)
                                plotMat[[row, col]] <- .displayError(errorMessagePlot, cexText=cexText) + adjMargin
                            }
                        } else {

                            p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
                            p <- p + ggplot2::xlab("")
                            p <- p + ggplot2::ylab("")
                            p <- JASPgraphs::themeJasp(p)

                            plotMat[[row, col]] <- p
                        }
                    }

                    if (col > row) {
                        if (options$plotCorrelationMatrix) {
                            if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
                                plotMat[[row, col]] <- .plotScatter(dataset[[variables[col]]], dataset[[variables[row]]]) + adjMargin # plot scatterplot
                            } else {
                                errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
                                errorMessagePlot <- paste0("Undefined correlation:", "\n", errorMessages[1])
                                plotMat[[row, col]] <- .displayError(errorMessagePlot, cexText=cexText) + adjMargin
                            }
                        } else {

                            p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
                            p <- p + ggplot2::xlab("")
                            p <- p + ggplot2::ylab("")
                            p <- JASPgraphs::themeJasp(p)

                            plotMat[[row, col]] <- p
                        }
                    }

                    if (col < row) {
                        if (l < 7) {
                            if (options$plotStatistics) {
                                if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
                                    plotMat[[row, col]] <- .plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], hypothesis= options$hypothesis,
                                                  pearson=options$pearson, kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=options$confidenceIntervalsInterval) + adjMargin # plot r= ...
                                } else {
                                    errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
                                    errorMessagePlot <- paste0("Undefined correlation:", "\n", errorMessages[1])
                                    plotMat[[row, col]] <- .displayError(errorMessagePlot, cexText=cexText) + adjMargin
                                }
                            } else {

                                p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
                                p <- p + ggplot2::xlab("")
                                p <- p + ggplot2::ylab("")
                                p <- JASPgraphs::themeJasp(p)

                                plotMat[[row, col]] <- p
                            }
                        }

                        if (l >= 7) {
                            if (options$plotStatistics) {
                                if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
                                    plotMat[[row, col]] <- .plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], cexCI= 1.2, hypothesis= options$hypothesis,
                                                  pearson=options$pearson, kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=options$confidenceIntervalsInterval) + adjMargin
                                                  # if(col == 1){
                                                  #     plotList[[length(plotList)]] <- plotList[[length(plotList)]] + ggplot2::annotate("text", x = 0, y = 1.5, label = .unv(variables)[row], angle = 90, size = 6, fontface = 2)
                                                  # }
                                } else {
                                    errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
                                    errorMessagePlot <- paste0("Undefined correlation:", "\n", errorMessages[1])
                                    plotMat[[row, col]] <- .displayError(errorMessagePlot, cexText=cexText) + adjMargin
                                }
                            } else {
                                p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
                                p <- p + ggplot2::xlab("")
                                p <- p + ggplot2::ylab("")
                                p <- JASPgraphs::themeJasp(p)

                                plotMat[[row, col]] <- p
                            }
                        }
                    }
                }
            }

        JASPgraphs::setGraphOption("fontsize", oldFontSize)

        # slightly adjust the positions of the labels left and above the plots.
        labelPos <- matrix(.5, 4, 2)
        labelPos[1, 1] <- .55
        labelPos[4, 2] <- .65

        p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = .unv(variables), topLabels = .unv(variables),
  															scaleXYlabels = NULL, labelPos = labelPos)

        return(p)
    }

    obj <- .plotFunc()

    content <- .writeImage(width = width, height = height, plot = obj, obj = TRUE)

    plot <- correlation.plot
    plot[["convertible"]] <- TRUE
    plot[["obj"]] <- content[["obj"]]
    plot[["data"]] <- content[["png"]]

    correlation.plot <- plot
  }

  return(correlation.plot)
}

#### histogram with density estimator ####
.plotMarginalCor <- function(variable, xName = NULL, yName = "Density") {

  variable <- na.omit(variable)
	isNumeric <- !(is.factor(variable) || (is.integer(variable) && length(unique(variable)) <= 10))


	if (isNumeric) {
		p <- ggplot2::ggplot(data = data.frame(x = variable))
		h <- hist(variable, plot = FALSE)
  	hdiff <- h$breaks[2L] - h$breaks[1L]
		xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(variable, h$breaks), min.n = 3)
		dens <- h$density
  	yBreaks <- c(0, 1.2*max(h$density))

  	p <- p + ggplot2::geom_histogram(
  		mapping  = ggplot2::aes(x = x, y = ..density..),
  		binwidth = hdiff,
  		fill     = "grey",
  		col      = "black",
  		size     = .3,
  		center   = hdiff / 2,
  		stat     = "bin"
  	) +
  		ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
	} else {

		p <- ggplot2::ggplot(data = data.frame(x = factor(variable)))
		hdiff <- 1L
		xBreaks <- unique(variable)
		yBreaks <- c(0, max(table(variable)))
		p <- p + ggplot2::geom_bar(
			mapping  = ggplot2::aes(x = x),
			fill     = "grey",
			col      = "black",
			size     = .3,
			stat     = "count"
		) +
			ggplot2::scale_x_discrete(name = xName, breaks = xBreaks)
	}

	yLim <- range(yBreaks)

  if (isNumeric) {
  	density <- density(variable)
  	p <- p + ggplot2::geom_line(data = data.frame(x = density$x, y = density$y),
  															mapping = ggplot2::aes(x = x, y = y), lwd = .7, col = "black")
  }

	thm <- ggplot2::theme(
		axis.ticks.y = ggplot2::element_blank(),
		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
	)
  p <- p +
  	ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, labels = c("", ""), limits = yLim) +
  	ggplot2::theme()
  return(JASPgraphs::themeJasp(p) + thm)

}


#### scatterplots ####

# predictions of fitted model
.poly.pred <- function(fit, plot = NULL, line=FALSE, xMin, xMax, lwd) {
    # create function formula
    f <- vector("character", 0)

    for (i in seq_along(coef(fit))) {
        if (i == 1) {
            temp <- paste(coef(fit)[[i]])
            f <- paste(f, temp, sep="")
        }

        if (i > 1) {
            temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
            f <- paste(f, temp, sep="+")
        }
    }

    x <- seq(xMin, xMax, length.out = 100)
    predY <- eval(parse(text=f))

    if (line == FALSE) {
        return(predY)
    }

    if (line) {
        plot <- plot + ggplot2::geom_line(data = data.frame(x, predY),mapping = ggplot2::aes(x = x, y = predY), size=lwd)
        return(plot)
    }
}

.plotScatter <- function(xVar, yVar, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL, drawAxes = TRUE) {
  
	isNumericX <- !(is.factor(xVar) || (is.integer(xVar) && length(unique(xVar)) <= 10))
	isNumericY <- !(is.factor(yVar) || (is.integer(yVar) && length(unique(yVar)) <= 10))
	bothNumeric <- isNumericX && isNumericY
  d <- data.frame(x = xVar, y = yVar)
  d <- na.omit(d)

  if (!isNumericX)
  	d$x <- as.factor(d$x)

  if (!isNumericY)
  	d$y <- as.factor(d$y)

  if (is.null(xBreaks))
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)

  fit <- NULL
  
  if (bothNumeric) {

  	fit <- lm(y ~ poly(x, 1, raw = TRUE), d)
  	lineObj <- .poly.predDescriptives(fit, line = FALSE, xMin= xBreaks[1], xMax = xBreaks[length(xBreaks)], lwd = lwd)
  	rangeLineObj <- c(lineObj[1], lineObj[length(lineObj)])
  	yLimits <- range(c(pretty(yVar)), rangeLineObj)

  	if (is.null(yBreaks))
  		yBreaks <- JASPgraphs::getPrettyAxisBreaks(yLimits)

  } else if (is.null(yBreaks)) {

  	yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)

  }

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
    JASPgraphs::geom_point()

  if (bothNumeric) {
  	xr <- range(xBreaks)
  	dfLine <- data.frame(x = xr, y = rangeLineObj)
    p <- p + ggplot2::geom_line(data = dfLine, ggplot2::aes(x = x, y = y), size = .7, inherit.aes = FALSE)
  }

  if(drawAxes){
    if (isNumericX) {
    	p <- p + ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
    } else {
    	p <- p + ggplot2::scale_x_discrete(name = xName)
    }
    if (isNumericY) {
    	p <- p + ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))
    } else {
    	p <- p + ggplot2::scale_y_discrete(name = yName)
    }
  } else{
    p <- p + ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, labels = NULL, limits = range(xBreaks))
    p <- p + ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, labels = NULL, limits = range(yBreaks))
  }

  return(JASPgraphs::themeJasp(p))
}

#### display correlation value ####
.plotCorValue <- function(xVar, yVar, cexText= 2.5, cexCI= 1.7, hypothesis = "correlated", pearson=options$pearson,
                          kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=0.95) {

    CIPossible <- TRUE

    tests <- c()

    if (pearson)
        tests <- c(tests, "pearson")

    if (spearman)
        tests <- c(tests, "spearman")

    if (kendallsTauB)
        tests <- c(tests, "kendall")


    p <- ggplot2::ggplot(data = data.frame(), ggplot2::aes(x = seq_along(data.frame()),y = summary(data.frame()))) +
        ggplot2::theme_void() +
        ggplot2::theme(
            plot.margin = grid::unit(c(1,1,1,1), "cm")
            ) + ggplot2::xlim(0,2) + ggplot2::ylim(0,2)

    lab <- vector("list")

    if (length(tests) == 1) {
        ypos <- 1.5
    }

    if (length(tests) == 2) {
        ypos <- c(1.6, 1.2)
    }

    if (length(tests) == 3) {
        ypos <- c(1.7, 1.2, .7)
    }

    for (i in seq_along(tests)) {
        if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == 1){
            CIPossible <- FALSE

            if(tests[i] == "pearson"){
                lab[[i]] <- paste("italic(r) == '1.000'")
            }

            if(tests[i] == "spearman"){
                lab[[i]] <- paste("italic(rho) == '1.000'")
            }

            if(tests[i] == "kendall"){
                lab[[i]] <- paste("italic(tau) == '1.000'")
            }
        } else if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == -1){
            CIPossible <- FALSE

            if(tests[i] == "pearson"){
                lab[[i]] <- paste("italic(r) == '-1.000'")
            }

            if(tests[i] == "spearman"){
                lab[[i]] <- paste("italic(rho) == '-1.000'")
            }

            if(tests[i] == "kendall"){
                lab[[i]] <- paste("italic(tau) == '-1.000'")
            }
        } else {
            if(tests[i] == "pearson"){
                #lab[[i]] <- paste0("italic(r) == ",as.character(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3))[1])
                lab[[i]] <- .corValueString(cor.test(xVar, yVar, method=tests[i])$estimate, "pearson", 3) # fix for rounding off decimals
            }

            if(tests[i] == "spearman"){
                #lab[[i]] <- paste0("italic(rho) == ",as.character(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
                lab[[i]] <- .corValueString(cor.test(xVar, yVar, method=tests[i])$estimate, "spearman", 3) # fix for rounding off decimals
            }

            if(tests[i] == "kendall"){
                #lab[[i]] <- paste0("italic(tau) == ",as.character(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
                lab[[i]] <- .corValueString(cor.test(xVar, yVar, method=tests[i])$estimate, "kendall", 3) # fix for rounding off decimals
            }
        }
    }

    for(i in seq_along(tests)){
        p <- p + ggplot2::geom_text(data = data.frame(x = rep(1, length(ypos)), y = ypos), mapping = ggplot2::aes(x = x, y = y, label = unlist(lab)), size = 7, parse = TRUE)
    }


    if (hypothesis == "correlated" & length(tests) == 1 & any(tests == "pearson")) {
        alternative <- "two.sided"
        ctest <- cor.test(xVar, yVar, method= tests, conf.level=confidenceInterval)
    }

    if (hypothesis != "correlated" & length(tests) == 1 & any(tests == "pearson")) {
        if (hypothesis == "correlatedPositively") {
            ctest <- cor.test(xVar, yVar, method=tests, alternative="greater", conf.level=confidenceInterval)
        } else if (hypothesis == "correlatedNegatively") {
            ctest <- cor.test(xVar, yVar, method=tests, alternative="less", conf.level=confidenceInterval)
        }
    }

    if (any(tests == "pearson")& length(tests) == 1 && CIPossible) {
        CIlow <- formatC(round(ctest$conf.int[1],3), format = "f", digits = 3)
        CIhigh <- formatC(round(ctest$conf.int[2],3), format = "f", digits = 3)

        if(length(p)>0){
            p <- p + ggplot2::geom_text(data = data.frame(x = 1, y = 1.2), mapping = ggplot2::aes(x = x, y = y, label = paste(100 * confidenceInterval, "% CI: [", CIlow, ", ", CIhigh, "]", sep="")), size = 5)
        }

    }

    return(p)

}

### empty Plot with error message ###
.displayError <- function(errorMessage=NULL, cexText=1.6, lwdAxis= 1.2) {
    p <- ggplot2::ggplot(data = data.frame(), ggplot2::aes(x = seq_along(data.frame()),y = summary(data.frame()))) +
        ggplot2::theme(
            panel.border = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            plot.margin = grid::unit(c(2,1,1,2), "cm"),
            axis.text.x =ggplot2::element_blank(),
            axis.title = ggplot2::element_blank()) +
        ggplot2::annotate("text", x = 0, y = 0, label = errorMessage, size = 5) +
        ggplot2::xlim(-30, 30) +
        ggplot2::ylim(-30, 30)
    return(p)
}

### Utility functions for nonparametric confidence intervals ###
.concordanceFunction <- function(i, j) {
  concordanceIndicator <- 0
  ij <- (j[2] - i[2]) * (j[1] - i[1])
  if (ij > 0) concordanceIndicator <- 1
  if (ij < 0) concordanceIndicator <- -1
  return(concordanceIndicator)
}

.addConcordances <- function(x, y, i) {
  concordanceIndex <- 0
  for (k in 1:length(x)) {
    if (k != i) {
      concordanceIndex <- concordanceIndex + .concordanceFunction(c(x[i], y[i]), c(x[k], y[k]))
    }
  }
  return(concordanceIndex)
}

.createNonparametricConfidenceIntervals <- function(x, y, obsCor, hypothesis = "two-sided", confLevel = 0.95, method = "kendall"){
  # Based on sections 8.3 and 8.4 of Hollander, Wolfe & Chicken, Nonparametric Statistical Methods, 3e.
  alpha <- 1 - confLevel
  missingIndices <- as.logical(is.na(x) + is.na(y)) # index those values that are missing
  x <- x[!missingIndices] # remove those values
  y <- y[!missingIndices]
  n <- length(x)

 if (method == "kendall") {
   concordanceSumsVector <- numeric(n)
    for (i in 1:n) {
      concordanceSumsVector[i] <- .addConcordances(x, y, i)
    }
    sigmaHatSq <- 2 * (n-2) * var(concordanceSumsVector) / (n*(n-1))
    sigmaHatSq <- sigmaHatSq + 1 - (obsCor)^2
    sigmaHatSq <- sigmaHatSq * 2 / (n*(n-1))

    if (hypothesis=="correlated"){
      z <- qnorm(alpha/2, lower.tail = FALSE)
    } else if (hypothesis!="correlated") {
      z <- qnorm(alpha, lower.tail = FALSE)
    }
    ciLow <- obsCor - z * sqrt(sigmaHatSq)
    ciUp <- obsCor + z * sqrt(sigmaHatSq)
    if (hypothesis=="correlatedPositively") {
      ciUp <- 1
    } else if (hypothesis=="correlatedNegatively") {
      ciLow <- -1
    }
  } else if (method == "spearman") {
    stdErr = 1/sqrt(n-3)
    if (hypothesis=="correlated") {
      z <- qnorm(alpha/2, lower.tail = FALSE)
    } else if (hypothesis!="correlated") {
      z <- qnorm(alpha, lower.tail = FALSE)
    }

    ciLow = tanh(atanh(obsCor) - z * stdErr)
    ciUp = tanh(atanh(obsCor) + z * stdErr)

    if (hypothesis=="correlatedPositively") {
      ciUp <- 1
    } else if (hypothesis=="correlatedNegatively") {
      ciLow <- -1
    }
  }
  return(c(ciLow,ciUp))
}

.corValueString <- function(corValue = NULL, testType = NULL, decimals = 3){
    if (testType == "pearson")
      type <- "italic(r)"
    else if (testType == "spearman")
      type <- "italic(rho)"
    else #kendall
      type <- "italic(tau)"

    formattedValue <- formatC(round(corValue, decimals), format = "f", digits = decimals)

    return(paste0(type, ' ~ "=" ~ ', '"', formattedValue, '"'))
}

.corrTexts <- list(
  footnotes = list(
    VSMPR = "Vovk-Sellke Maximum <em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum possible odds in favor of H\u2081 over H\u2080 equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37 (Sellke, Bayarri, & Berger, 2001)."
  ),
  references = list(
    Sellke_etal_2001 = "Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of p Values for Testing Precise Null Hypotheses. <i>The American Statistician,</i> 55(<i>1</i>), p. 62-71."
  )
)

# helper fn
.corr.test <- function(x, y, z = NULL, alternative, method, exact = NULL, conf.level = 0.95, continuity = FALSE, ...){
  stats <- c("estimate", "p.value", "conf.int", "vsmpr")
  statsNames <- c("estimate", "p.value", "lower.ci", "upper.ci", "vsmpr")
  
  if(is.null(z)){
    result <- try(expr = {
      cor.test(x = x, y = y, alternative = alternative, method = method, exact = exact, 
                       conf.level = conf.level, continuity = continuity, ... = ...)}, silent = TRUE)
    
    if(isTryError(result)) {
      errors <- .extractErrorMessage(result)
      result <- rep(NaN, length(statsNames))
      names(result) <- paste(method, statsNames, sep = "_")
    } else{
      errors <- FALSE
      
      if(method != "pearson"){
        result$conf.int <- .createNonparametricConfidenceIntervals(x = x, y = y, obsCor = result$estimate,
                                                                   hypothesis = alternative, confLevel = conf.level,
                                                                   method = method)
      }
      result$vsmpr <- .VovkSellkeMPR(result$p.value)
      result$vsmpr <- ifelse(result$vsmpr == "∞", Inf, result$vsmpr)
      result <- unlist(result[stats], use.names = FALSE)
      names(result) <- paste(method, statsNames, sep = "_")
    }
  } else{
    result <- try(expr = {ppcor::pcor.test(x = x, y = y, z = z, method = method)}, silent = TRUE)
    if(isTryError(result)) {
      errors <- .extractErrorMessage(result)
      result <- rep(NaN, length(statsNames))
      names(result) <- paste(method, statsNames, sep = "_")
    } else{
      errors <- FALSE
      result <- as.list(result)
      if(alternative == "less"){
        if(result$estimate <= 0){
          result$p.value <- result$p.value/2
        } else{
          result$p.value <- 1 - result$p.value/2
        }
      } else if(alternative == "greater"){
        if(result$estimate >= 0){
          result$p.value <- result$p.value/2
        } else{
          result$p.value <- 1 - result$p.value/2
        }
      }
      result$vsmpr <- .VovkSellkeMPR(result$p.value)
      result$vsmpr <- ifelse(result$vsmpr == "∞", Inf, result$vsmpr)
      result$lower.ci <- NA
      result$upper.ci <- NA
      result <- unlist(result[statsNames], use.names = FALSE)
      names(result) <- paste(method, statsNames, sep = "_")
    }
  }
  
  return(list(result = result, errors = errors))
}
