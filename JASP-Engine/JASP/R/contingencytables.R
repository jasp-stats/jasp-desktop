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

ContingencyTables <- function(jaspResults, dataset, options, ...) {
  # Read dataset
  dataset <- .crossTabReadData(dataset, options)
  
  ready <- !(length(options$rows) == 0 || length(options$columns) == 0)

  # Error checking
  .crossTabCheckErrors(dataset, options)
  
  # Compute the combinations of rows, columns, layers
  analyses <- .crossTabComputeAnalyses(dataset, options, ready)
  
  # Tables container
  .crossTabContainer(jaspResults, options, analyses, ready)
    
  # Output tables (each calls its own results function)
  .crossTabMain(      jaspResults, dataset, options, analyses, ready)
  .crossTabChisq(     jaspResults, dataset, options, analyses, ready)
  .crossTabLogOdds(   jaspResults, dataset, options, analyses, ready)
  .crossTabNominal(   jaspResults, dataset, options, analyses, ready)
  .crossTabGamma(     jaspResults, dataset, options, analyses, ready)
  .crossTabKendallTau(jaspResults, dataset, options, analyses, ready)
    
  return()
}

# Preprocessing functions 
.crossTabReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else {
    layer.variables <- c()
    for (layer in options$layers)
      layer.variables <- c(layer.variables, unlist(layer$variables))
    counts.var <- options$counts
    if (counts.var == "")
      counts.var <- NULL
    
    factors <- c(unlist(options$rows), unlist(options$columns), layer.variables)
    
    return(.readDataSetToEnd(columns.as.factor  = factors, 
                             columns.as.numeric = counts.var))
  }
}

.crossTabCheckErrors <- function(dataset, options) {
  .hasErrors(dataset, 
             type = c('negativeValues', 'infinity'), 
             all.target = c(options$counts), 
             exitAnalysisIfErrors = TRUE)
}

# Combinations of rows, columns, layers 
.crossTabComputeAnalyses <- function(dataset, options, ready) {
  rows    <- as.character(options$rows)
  columns <- as.character(options$columns)
  
  if (length(rows) == 0)
    rows <- ""
  
  if (length(columns) == 0)
    columns <- ""
  analyses <- list()
  analyses <- data.frame("columns" = columns, stringsAsFactors = FALSE)
  analyses <- cbind(analyses, "rows" = rep(rows, each = nrow(analyses)), 
                    stringsAsFactors = FALSE)
  
  for (layer in options$layers) {
    layer.vars <- as.character(layer$variables)
    analyses <- cbind(analyses, rep(layer.vars, each = nrow(analyses)), 
                      stringsAsFactors = FALSE)
    names(analyses)[ncol(analyses)] <- layer$name
  }
  return(analyses)
}

# Container 
.crossTabContainer <- function(jaspResults, options, analyses, ready) {
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    if (is.null(jaspResults[[paste0("container", i)]])) {
      container <- createJaspContainer()
      container$dependOn(options              = c("layers", "counts"),
                         optionContainsValue  = list(rows     = analysis$rows, 
                                                     columns  = analysis$columns))
      jaspResults[[paste0("container", i)]] <- container
    }
  }
}

# Output Tables 
.crossTabMain <- function(jaspResults, dataset, options, analyses, ready) {
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["crossTabMain"]])) 
      next
    
    # Create table
    crossTabMain <- createJaspTable(title = "Contingency Tables")
    crossTabMain$dependOn(c("countsExpected", "percentagesRow",  "percentagesColumn", 
                            "percentagesTotal", "rowOrder", "columnOrder"))
    crossTabMain$showSpecifiedColumnsOnly <- TRUE
    crossTabMain$position <- 1
      #
    .crossTabLayersColumns(crossTabMain, analysis)
    if(analysis$rows == "")
      crossTabMain$addColumnInfo(name = analysis$rows, title = " ", 
                                 type = "string", combine = TRUE)
    else 
      crossTabMain$addColumnInfo(name = analysis$rows, type = "string", 
                                 combine = TRUE)
    
    counts.fp <- .crossTabCountsFp(dataset, options)

    if (options$countsExpected || options$percentagesRow || 
        options$percentagesColumn || options$percentagesTotal )  
      crossTabMain$addColumnInfo(name = "type[counts]", title = "", 
                                 type = "string")
    if (options$countsExpected) 
      crossTabMain$addColumnInfo(name = "type[expected]", title = "", 
                                 type = "string")
    if (options$percentagesRow) 
      crossTabMain$addColumnInfo(name = "type[row.proportions]", title = "", 
                                 type = "string")
    if (options$percentagesColumn) 
      crossTabMain$addColumnInfo(name = "type[col.proportions]", title = "", 
                                 type = "string")
    if (options$percentagesTotal) 
      crossTabMain$addColumnInfo(name = "type[proportions]", title = "", 
                                 type = "string")

    .crossTabMainOvertitle(dataset, options, crossTabMain, analysis, counts.fp)
    # Totals columns
    if (counts.fp || options$countsExpected || options$percentagesRow || 
        options$percentagesColumn || options$percentagesTotal) {
      crossTabMain$addColumnInfo(name = "total[counts]",   title = "Total", 
                                 type = "number", format = "sf:4;dp:2")
      if (options$countsExpected) 
        crossTabMain$addColumnInfo(name = "total[expected]", title = "Total", 
                                   type = "number", format = "sf:4;dp:2")
      if (options$percentagesRow)
        crossTabMain$addColumnInfo(name = "total[row.proportions]", title = "Total", 
                                   type = "number", format = "dp:1;pc")
      if (options$percentagesColumn) 
        crossTabMain$addColumnInfo(name = "total[col.proportions]", title = "Total", 
                                   type = "number", format = "dp:1;pc")
      if (options$percentagesTotal) 
        crossTabMain$addColumnInfo(name = "total[proportions]", title = "Total", 
                                   type = "number", format = "dp:1;pc")
    } else 
      crossTabMain$addColumnInfo(name = "total[counts]", title = "Total", 
                                 type = "integer")
    analysisContainer[["crossTabMain"]] <- crossTabMain
    analysis <- as.list(analysis)
    # Compute/get Group List
    groupList <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready)
    
    res <- try(.crossTabCountsRows(analysisContainer, analysis$rows, groupList, options, ready, counts.fp))
    .crossTabSetErrorOrFill(res, crossTabMain)
  }
}

.crossTabChisq <- function(jaspResults, dataset, options, analyses, ready) {
  if(!options$chiSquared && 
     !options$chiSquaredContinuityCorrection && 
     !options$likelihoodRatio) 
    return()
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["crossTabChisq"]])) 
      next
    
    # Create table
    crossTabChisq <- createJaspTable(title = "Chi-Squared Tests")
    crossTabChisq$dependOn(c("chiSquared", "chiSquaredContinuityCorrection", 
                             "likelihoodRatio", "VovkSellkeMPR"))
    crossTabChisq$showSpecifiedColumnsOnly <- TRUE
    crossTabChisq$position <- 2
    
    counts.fp <- .crossTabCountsFp(dataset, options)
    
    # Add columns to table
    .crossTabLayersColumns(crossTabChisq, analysis)
    if (options$chiSquared)
      .crossTabChisqAddColInfo(fold = "chiSquared", crossTabChisq, options)
    if (options$chiSquaredContinuityCorrection)
      .crossTabChisqAddColInfo(fold = "chiSquared-cc", crossTabChisq, options)
    if (options$likelihoodRatio)
      .crossTabChisqAddColInfo(fold = "likelihood", crossTabChisq, options)
    .crossTabChisqAddColInfo(fold = "N", crossTabChisq, options, counts.fp)
    if(options$VovkSellkeMPR){
      message <- ("Vovk-Sellke Maximum  <em>p</em>-Ratio: Based the <em>p</em>-value, 
      the maximum possible odds in favor of H\u2081 over H\u2080 equals 
      1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
      (Sellke, Bayarri, & Berger, 2001).")
      crossTabChisq$addFootnote(message, symbol = "\u002A")
    }
      
    analysisContainer[["crossTabChisq"]] <- crossTabChisq
    analysis <- as.list(analysis)
    # Compute/get Group List
    groupList <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready)
    res <- try(.crossTabTestsRows(analysisContainer, groupList$rows, groupList, options, ready, counts.fp))
    .crossTabSetErrorOrFill(res, crossTabChisq)
  }
}

.crossTabLogOdds <- function(jaspResults, dataset, options, analyses, ready) {
  if(!options$oddsRatio)
    return()
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["crossTabLogOdds"]])) 
      next

    # Create table
    crossTabLogOdds <- createJaspTable(title = "Log Odds Ratio")
    crossTabLogOdds$dependOn(c("oddsRatio", "oddsRatioConfidenceIntervalInterval"))
    crossTabLogOdds$showSpecifiedColumnsOnly <- TRUE
    crossTabLogOdds$position <- 3
    
    ci.label <- paste(100*options$oddsRatioConfidenceIntervalInterval, 
                      "% Confidence Intervals", sep = "")
    
    # Add columns to table
    .crossTabLayersColumns(crossTabLogOdds, analysis)
    .crossTabLogOddsAddColInfo(crossTabLogOdds, fold = "oddsRatio",  ci.label)
    .crossTabLogOddsAddColInfo(crossTabLogOdds, fold = "FisherTest", ci.label)
    
    analysisContainer[["crossTabLogOdds"]] <- crossTabLogOdds
    analysis <- as.list(analysis)
    # Compute/get Group List
    groupList <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready)
    
    res <- try(.crossTabOddsRatioRows(analysisContainer, analysis$rows, groupList, options, ready))
    .crossTabSetErrorOrFill(res, crossTabLogOdds)
  }
}

.crossTabNominal <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$contingencyCoefficient && !options$phiAndCramersV && !options$lambda)
    return()
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["crossTabNominal"]])) 
      next
    # Create table
    crossTabNominal <- createJaspTable(title = "Nominal")
    crossTabNominal$dependOn(c("contingencyCoefficient", "phiAndCramersV"))
    crossTabNominal$showSpecifiedColumnsOnly <- TRUE
    crossTabNominal$position <- 4
    # Add columns to table
    .crossTabLayersColumns(crossTabNominal, analysis)
    if (options$contingencyCoefficient)
      .crossTabNominalAddColInfo(crossTabNominal, "ContCoef")
    if (options$phiAndCramersV) {
      .crossTabNominalAddColInfo(crossTabNominal, "PhiCoef")
      .crossTabNominalAddColInfo(crossTabNominal, "CramerV")
    }
    if (options$lambda) {
      .crossTabNominalAddColInfo(crossTabNominal, "LambdaR")
      .crossTabNominalAddColInfo(crossTabNominal, "LambdaC")
    }
    analysisContainer[["crossTabNominal"]] <- crossTabNominal
    analysis <- as.list(analysis)
    # Compute/get Group List
    groupList <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready)
    
    res <- try(.crossTabNominalRows(analysisContainer, analysis$rows, groupList, options, ready))
    .crossTabSetErrorOrFill(res, crossTabNominal)
  }
}

.crossTabGamma <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$gamma)
    return()
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["crossTabGamma"]])) 
      next

    # Create table
    crossTabGamma <- createJaspTable(title = "Ordinal Gamma")
    crossTabGamma$dependOn(c("gamma"))
    crossTabGamma$showSpecifiedColumnsOnly <- TRUE
    crossTabGamma$position <- 5
    
    ci.label <- paste("95% Confidence Intervals")
    
    # Add columns to table
    .crossTabLayersColumns(crossTabGamma, analysis)
    crossTabGamma$addColumnInfo(name = "value[gammaCoef]", title = "Gamma",          
                                type = "number")
    crossTabGamma$addColumnInfo(name = "Sigma[gammaCoef]", title = "Standard Error", 
                                type = "number", format = "dp:3")
    crossTabGamma$addColumnInfo(name = "low[gammaCoef]",   title = "Lower",  
                                type = "number", format = "dp:3", overtitle = ci.label)
    crossTabGamma$addColumnInfo(name = "up[gammaCoef]",    title = "Upper",  
                                type = "number", format = "dp:3", overtitle = ci.label)
  
    analysisContainer[["crossTabGamma"]] <- crossTabGamma
    analysis <- as.list(analysis)
    # Compute/get Group List
    groupList <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready)
    
    res <- try(.crossTabGammaRows(analysisContainer, analysis$rows, groupList, options, ready))
    .crossTabSetErrorOrFill(res, crossTabGamma)
  }
}

.crossTabKendallTau <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$kendallsTauB)
    return()
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["crossTabKendallTau"]])) 
      next
    
    # Create table
    crossTabKendallTau <- createJaspTable(title = "Kendall's Tau")
    crossTabKendallTau$dependOn(c("kendallsTauB", "VovkSellkeMPR"))
    crossTabKendallTau$showSpecifiedColumnsOnly <- TRUE
    crossTabKendallTau$position <- 6
    
    # Add columns to table
    .crossTabLayersColumns(crossTabKendallTau, analysis)
    crossTabKendallTau$addColumnInfo(name = "value[kTauB]", title = "Kendall's Tau-b ", 
                                     type = "number")
    crossTabKendallTau$addColumnInfo(name = "statistic[kTauB]", title = "Z", 
                                     type = "number", format = "dp:3")
    crossTabKendallTau$addColumnInfo(name = "p[kTauB]", title = "p", type = "pvalue")
    if (options$VovkSellkeMPR) 
      crossTabKendallTau$addColumnInfo(name = "MPR[kTauB]", title = "VS-MPR\u002A", 
                                       type = "number")

    analysisContainer[["crossTabKendallTau"]] <- crossTabKendallTau
    analysis <- as.list(analysis)
    # Compute/get Group List
    groupList <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready)
    
    res <- try(.crossTabKendallsTauRows(analysisContainer, analysis$rows, groupList, options, ready))
    .crossTabSetErrorOrFill(res, crossTabKendallTau)
  }
}

# Table functions
.crossTabLayersColumns <- function(table, analysis) {
  if ((length(analysis)) >= 3)
    for (j in (length(analysis)):3)
      table$addColumnInfo(name = analysis[[j]], type = "string", combine = TRUE)
}

.crossTabMainOvertitle <- function(dataset, options, table, analysis, counts.fp) {
  
  lvls <- c()
  overTitle <- unlist(analysis$columns)
  
  if(analysis$columns == "") {
    lvls <- c("a", "b")
    for (column.name in lvls) {
      private.name <- paste0(column.name,"[counts]")
      
      if (counts.fp || options$countsExpected || options$percentagesRow || 
          options$percentagesColumn || options$percentagesTotal ) 
        table$addColumnInfo(name = private.name, title = ".", 
                            overtitle = ".", type = "number", format = "sf:4;dp:2")
      else 
        table$addColumnInfo(name = private.name, title = ".", 
                            overtitle = ".", type = "integer")
      
      if (options$countsExpected) { 
        private.name <- paste0(column.name,"[expected]")
        table$addColumnInfo(name = private.name, title = ".", 
                            type = "number", format = "sf:4;dp:2")
      }
      if (options$percentagesRow) {
        private.name <- paste0(column.name,"[row.proportions]")
        table$addColumnInfo(name = private.name, title = ".", 
                            type = "number", format = "dp:1;pc")
      }
      if (options$percentagesColumn) {
        private.name <- paste0(column.name,"[col.proportions]")
        table$addColumnInfo(name = private.name, title = ".", 
                            type = "number", format = "dp:1;pc")
      }
      if (options$percentagesTotal) {
        private.name <- paste0(column.name,"[proportions]")
        table$addColumnInfo(name = private.name, title = ".", 
                            type = "number", format = "dp:1;pc")
      }
    }
  } else {
    if (is.factor(dataset[[ .v(analysis$columns) ]] )) {
      lvls <- levels(dataset[[ .v(analysis$columns) ]])
      if (options$columnOrder == "descending")
        lvls <- rev(lvls)
    } else {
      lvls <- unique(dataset[[ .v(analysis$columns) ]])
      if (options$columnOrder == "descending")
        lvls <- rev(lvls, decreasing = TRUE)
    }
      
    for (column.name in lvls) {
      private.name <- paste0(column.name,"[counts]")
      
      if (counts.fp || options$countsExpected || options$percentagesRow || 
          options$percentagesColumn || options$percentagesTotal ) 
        table$addColumnInfo(name = private.name, title = column.name, 
                            overtitle = overTitle, type = "number", 
                            format = "sf:4;dp:2")
      else 
        table$addColumnInfo(name = private.name, title = column.name, 
                            overtitle = overTitle, type = "integer")
      
      if (options$countsExpected) { 
        private.name <- paste0(column.name,"[expected]")
        table$addColumnInfo(name = private.name, title = column.name, 
                            type = "number", format = "sf:4;dp:2")
      }
      if (options$percentagesRow) {
        private.name <- paste0(column.name,"[row.proportions]")
        table$addColumnInfo(name = private.name, title = column.name, 
                            type = "number", format = "dp:1;pc")
      }
      if (options$percentagesColumn) {
        private.name <- paste0(column.name,"[col.proportions]")
        table$addColumnInfo(name = private.name, title = column.name, 
                            type = "number", format = "dp:1;pc")
      }
      if (options$percentagesTotal) {
        private.name <- paste0(column.name,"[proportions]")
        table$addColumnInfo(name = private.name, title = column.name, 
                            type = "number", format = "dp:1;pc")
      }
    }
  }
}

.crossTabSetErrorOrFill <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
  else
    for (level in 1:length(res)) 
      table$addRows(res[[level]])
}

.crossTabChisqAddColInfo <- function(fold, table, options, counts.fp = FALSE) {
  table$addColumnInfo(name = paste0("type[", fold, "]"),  title = "", type = "string")
  if(fold == "N" && counts.fp == FALSE)
    table$addColumnInfo(name = paste0("value[", fold, "]"), title = "Value", type = "integer")
  else
    table$addColumnInfo(name = paste0("value[", fold, "]"), title = "Value", type = "number")
  table$addColumnInfo(name = paste0("df[", fold, "]"),    title = "df", type = "integer")
  table$addColumnInfo(name = paste0("p[", fold, "]"),     title = "p",  type = "pvalue")
  if (options$VovkSellkeMPR)
    table$addColumnInfo(name = paste0("MPR[", fold, "]"), title = "VS-MPR\u002A", type = "number")
}

.crossTabLogOddsAddColInfo <- function(table, fold, ci.label) {
  table$addColumnInfo(name = paste0("type[", fold, "]"),  title = "", type = "string")
  table$addColumnInfo(name = paste0("value[", fold, "]"), title = "Log Odds Ratio", 
                                type = "number")
  table$addColumnInfo(name = paste0("low[", fold, "]"),   title = "Lower", 
                                overtitle = ci.label, type = "number", format = "dp:3")
  table$addColumnInfo(name = paste0("up[", fold, "]"),    title = "Upper", 
                                overtitle = ci.label, type = "number", format = "dp:3")
}

.crossTabNominalAddColInfo <- function(table, fold){
  table$addColumnInfo(name = paste0("type[", fold, "]"),  title = "", type = "string")
  table$addColumnInfo(name = paste0("value[", fold, "]"), title = "Value", type = "number")
}

.crossTabLayerNames <- function(row, group) {
  for (layer in names(group)) {
    level <- group[[layer]]
    if (level == "")
      row[[layer]] <- "Total"
    else 
      row[[layer]] <- level
  }
  return(row)
}

.crossTabComputeGroups <- function(dataset, options, analysisContainer, analysis, ready) {
  if(!is.null(analysisContainer[["groupList"]]))
    return(analysisContainer[["groupList"]]$object)
  groupsList <- list() #list of groups, group.matrices
  counts.var <- options$counts
  if (counts.var == "")
    counts.var <- NULL
  if(ready) {
    all.vars   <- c(unlist(analysis), counts.var)
    subdataset <- subset(dataset, select = .v(all.vars))
  }
  else
    subdataset <- dataset
  # the following creates a 'groups' list
  # a 'group' represents a combinations of the levels from the layers
  # if no layers are specified, groups is null
  if (length(analysis) >= 3) {  # if layers are specified
    
    lvls <- levels(subdataset[[ .v(analysis[[3]]) ]])
    
    if (length(lvls) < 2)
      lvls <- ""
    else 
      lvls <- c(lvls, "")  # blank means total
    
    # here we create all combinations of the levels from the layers
    # it is easiest to do this with a data frame
    # at the end we convert this to a list of rows
    
    groups <- data.frame(lvls, stringsAsFactors=FALSE)
    names(groups) <- analysis[[3]]
    
    if (length(analysis) >= 4) {
      
      for (j in 4:(length(analysis))) {
        lvls <- levels(subdataset[[ .v(analysis[[j]]) ]])
        lvls <- c(lvls, "")  # blank means total
        
        groups <- cbind(rep(lvls, each=dim(groups)[1]), groups, 
                        stringsAsFactors=FALSE)
        names(groups)[1] <- analysis[[j]]
      }
    }
    # convert all the combinations to a list of rows
    groups <- .dataFrameToRowList(groups)
    
  } else  # if layers are not specified
    groups <- NULL
  groupsList$groups <- groups
  if (!is.null(counts.var))
    counts <- stats::na.omit(subdataset[[ .v(counts.var) ]])
  grp.mat <- .crossTabGroupMatrices(subdataset, analysis$rows, 
                                      analysis$columns, groups, 
                                      counts.var, 
                                      options$rowOrder=="descending", 
                                      options$columnOrder=="descending", 
                                      ready)
  groupsList$group.matrices <- grp.mat
  analysisContainer[["groupList"]] <- createJaspState(groupsList)
  return(groupsList)
}

.crossTabCountsMatrixToRow <- function(matrix, counts.matrix, type) {
  if (is.character(matrix[1,1]))
    rowname <- matrix[1,]
  else {
    if(type %in% c("expected", "col.proportions", "proportions"))
      row <- colSums(matrix)
    else if(type == "row.proportions"){
      m <- margin.table(counts.matrix, 2)
      rowprop <- prop.table(m)
      row <- rowprop
    }
  }
  
  row <- as.list(row)
  names(row) <- paste0(names(row),"[", type, "]")
  
  if (is.character(matrix[1,1]))
    row[[paste0("total[", type, "]")]] <- ""
  else {
    if(type == "col.proportions") {
      row.sum  <- margin.table(matrix, 1)
      row.prop <- prop.table(row.sum)
      col.prop <- sum(row.prop)
      row[[paste0("total[", type, "]")]] <- col.prop
    } else if(type == "row.proportions")
      row[[paste0("total[", type, "]")]] <- sum(rowprop)
    else
      row[[paste0("total[", type, "]")]] <- sum(matrix)
  }
  return(row)
}

.crossTabNominalMatrixToRow <- function(analysisContainer, row, counts.matrix, type, ready) {
  if(type == "ContCoef") {
    row[["type[ContCoef]"]] <- "Contingency coefficient"
    val <- "contingency"
  } else if(type == "PhiCoef") {
    row[["type[PhiCoef]"]] <- "Phi-coefficient"
    val <- "phi"
  } else if(type == "CramerV") {
    row[["type[CramerV]"]] <- "Cramer's V "
    val <- "cramer"
  }
  if (ready) {
    chi.result <- try({
      chi.result <- vcd::assocstats(counts.matrix)
    })
    if (isTryError(chi.result)) 
      row[[paste0("value[", type, "]")]] <- NaN
    else if (is.na(chi.result[[val]])) {
      row[[paste0("value[", type, "]")]] <- NaN
      message <- "Value could not be calculated - At least one row or column contains all zeros"
      row[[paste0("type[", type ,"]")]] <- paste0(row[[paste0("type[", type ,"]")]], "\u207A")
      analysisContainer[["crossTabNominal"]]$addFootnote(message, symbol = "\u207A")
    } else
      row[[paste0("value[", type, "]")]] <- chi.result[[val]]
  } 
  else
    row[[paste0("value[", type, "]")]] <- "."
  return(row)
}

.crossTabCountsFp <- function(dataset, options) {
  if (options$counts != "") {
    counts <- dataset[[ .v(options$counts) ]]
    if(!(counts == as.integer(counts)))     
      return(TRUE)
  }
  return(FALSE)
}

# Group matrix
.crossTabGroupMatrices <- function(dataset, rows, columns, groups, counts = NULL, 
                                   rowOrderDescending = FALSE, 
                                   columnOrderDescending = FALSE, ready) {
  # this creates count matrices for each of the groups
  matrices <- list()
  
  if (is.null(groups)) {
    
    if (!ready) {
      
      row.levels <- c(" .", " . ")
      col.levels <- c(" .", " . ")
      if (rows != "")
        row.levels <- levels(dataset[[ .v(rows) ]])
      if (columns != "")
        col.levels <- levels(dataset[[ .v(columns) ]])
      ss.matrix <- matrix(0, 
                          nrow     = length(row.levels), 
                          ncol     = length(col.levels), 
                          dimnames = list(row.levels, col.levels))
    } else if (is.null(counts)) {
      ss.dataset <- subset(dataset, select = .v(c(rows, columns)))
      ss.table   <- table(ss.dataset)
      ss.matrix  <- matrix(ss.table, nrow = dim(ss.table)[1], 
                           ncol = dim(ss.table)[2], 
                           dimnames = dimnames(ss.table))
    } else {
      ss.dataset <- subset(dataset, select = .v(c(rows, columns, counts)))
      ss.matrix  <- tapply(ss.dataset[[ .v(counts) ]], 
                           list(ss.dataset[[ .v(rows) ]], 
                                ss.dataset[[ .v(columns) ]]), 
                           sum)
      ss.matrix[is.na(ss.matrix)] <- 0
    }
    if (rowOrderDescending)
      ss.matrix <- apply(ss.matrix, 2, rev)
    if (columnOrderDescending)
      ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
    
    ss.matrix[is.na(ss.matrix)] <- 0
    
    matrices[[1]] <- ss.matrix
  } else {
    for (group in groups) {
      group <- group[group != ""]
      if (!ready) {} # do nothing
      else if (length(group) == 0)
        ss.dataset <- subset(dataset, select = .v(c(rows, columns, counts)))
      else {
        ss.filter.string <- paste(.v(names(group)), "==\"", group, "\"", 
                                        sep = "", collapse = "&")
        ss.expression    <- parse(text = ss.filter.string)
        ss.dataset	     <- subset(dataset, 
                                   select = .v(c(rows, columns, counts)), 
                                   subset = eval(ss.expression))
      }
      if (!ready)
        ss.matrix <- matrix(c(0,0,0,0), nrow = 2, ncol = 2)
      else if (is.null(counts)) {
        ss.table  <- table(ss.dataset)
        ss.matrix <- matrix(ss.table, 
                            nrow     = dim(ss.table)[1], 
                            ncol     = dim(ss.table)[2], 
                            dimnames = dimnames(ss.table))
      } else {
        ss.matrix <- tapply(ss.dataset[[ .v(counts) ]], 
                            list(ss.dataset[[ .v(rows) ]], 
                                 ss.dataset[[ .v(columns) ]]), 
                            sum)
      }
      
      ss.matrix[is.na(ss.matrix)] <- 0
      
      if (rowOrderDescending)
        ss.matrix <- apply(ss.matrix, 2, rev)
      if (columnOrderDescending)
        ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
      matrices[[length(matrices) + 1]] <- ss.matrix
    }
  }
  return(matrices)
}

# Table Results
.crossTabCountsRows <- function(analysisContainer, var.name, groupList, options, ready, counts.fp) {
  if(!is.null(analysisContainer[["resultsMain"]]))
    return()
  counts.rows     <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL

    rows                  <- list()
    row.count             <- list()
    row.expected          <- list()
    row.row.proportions   <- list()
    row.col.proportions   <- list()
    row.total.proportions <- list()
    row.proportions       <- list()
    row.count[["type[counts]"]] <- "Count"
    
    if (ready) {
      expected.matrix <- try({
        stats::chisq.test(counts.matrix, correct = FALSE)$expected
      })
      if (isTryError(expected.matrix)) {
        expected.matrix    <- counts.matrix
        expected.matrix[,] <- "&nbsp;"
      }
      
      row.proportions.matrix <- try({
        prop.table(counts.matrix, 1)
      })
      if (isTryError(row.proportions.matrix)) {
        row.proportions.matrix    <- counts.matrix
        row.proportions.matrix[,] <- "&nbsp;"
      }
      
      col.proportions.matrix <- try({
        prop.table(counts.matrix, 2)
      })
      if (isTryError(col.proportions.matrix)) {
        col.proportions.matrix    <- counts.matrix
        col.proportions.matrix[,] <- "&nbsp;"
      }
      
      proportions.matrix <- try({
        prop.table(counts.matrix, margin = NULL)
      })
      if (isTryError(proportions.matrix)) {
        proportions.matrix    <- counts.matrix
        proportions.matrix[,] <- "&nbsp;"
      }
      
    } else {
      expected.matrix <- counts.matrix
      row.proportions.matrix <- counts.matrix
      col.proportions.matrix <- counts.matrix
      proportions.matrix <- counts.matrix
    }
    
    for (j in 1:dim(counts.matrix)[[1]]) {
      
      if (ready) {
        
        row <- as.list(counts.matrix[j,])
        names(row) <- paste0(names(row),"[counts]")
        sum <- sum(counts.matrix[j,])
        if(counts.fp || options$countsExpected || options$percentagesRow || 
           options$percentagesColumn || options$percentagesTotal)
          row[["total[counts]"]] <- sum
        else
          row[["total[counts]"]] <- as.integer(sum)
        row <- c(row.count, row)
        
        if (options$countsExpected) {
          
          row.expected[["type[expected]"]] <- "Expected count"
          
          expected <- as.list(expected.matrix[j,])
          names(expected) <- paste(names(expected),"[expected]",  sep = "")
          
          if (is.character(expected.matrix[1,1]))
            expected[["total[expected]"]] <- ""
          else 
            expected[["total[expected]"]] <- base::sum(expected.matrix[j,])

          expected <- c(row.expected, expected)
          row <- c(row, expected)
        }
        
        if (options$percentagesRow) {
          row.proportions <- list()
          row.row.proportions[["type[row.proportions]"]] <- " % within row"
          
          row.proportions <- as.list(row.proportions.matrix[j,])
          names(row.proportions) <- paste(names(row.proportions),
                                          "[row.proportions]",  sep = "")
          
          if (is.character(row.proportions.matrix[1,1]))
            row.prop <- ""
          else
            row.prop <- sum(row.proportions.matrix[j,])
          row.proportions[["total[row.proportions]"]] <- row.prop
          
          row.proportions <- c(row.row.proportions, row.proportions)
          row <- c(row, row.proportions)
        }
        
        if (options$percentagesColumn) {
          
          row.col.proportions[["type[col.proportions]"]] <- " % within column"
          
          col.proportions <- as.list(col.proportions.matrix[j,])
          names(col.proportions) <- paste(names(col.proportions),
                                          "[col.proportions]",  sep = "")
          
          if (is.character(col.proportions.matrix[1,1]))
            col.proportions[["total[col.proportions]"]] <- ""
          else {
            row.sum  <- margin.table(counts.matrix, 1)
            row.prop <- as.list(prop.table(row.sum))
            col.proportions[["total[col.proportions]"]] <- row.prop[[j]]
          }
          
          col.proportions <- c(row.col.proportions, col.proportions)
          row <- c(row, col.proportions)
        }
        
        if (options$percentagesTotal) {
          total.proportions <- list()
          row.total.proportions[["type[proportions]"]] <- " % of total"
          
          total.proportions <- as.list(proportions.matrix[j,])
          names(total.proportions) <- paste(names(total.proportions),
                                            "[proportions]",  sep = "")
          
          if (is.character(proportions.matrix[1,1]))
            tot.prop <- ""
          else 
            tot.prop <- sum(proportions.matrix[j,])
          total.proportions[["total[proportions]"]] <- tot.prop
          
          total.proportions <- c(row.total.proportions, total.proportions)
          row <- c(row, total.proportions)
        }
        
      } else 
        row <- list()
      row[[var.name]] <- dimnames(counts.matrix)[[1]][j]
      row <- .crossTabLayerNames(row, group)
      
      if (j == 1 && !options$countsExpected && !options$percentagesRow && 
          !options$percentagesCol &&  !options$percentagesTotal)
        row[[".isNewGroup"]] <- TRUE
      rows[[length(rows) + 1]] <- row
    }
    
    if (ready) {
      
      row <- colSums(counts.matrix)
      row <- as.list(row)
      names(row) <- paste0(names(row),"[counts]")
      sum <- sum(counts.matrix)
      if(counts.fp || options$countsExpected || options$percentagesRow || 
         options$percentagesColumn || options$percentagesTotal)
        row[["total[counts]"]] <- sum
      else
        row[["total[counts]"]] <- as.integer(sum)
      row <- c(row.count, row)
      
      if (options$countsExpected) {
        expected <- .crossTabCountsMatrixToRow(expected.matrix, counts.matrix, type = "expected")
        
        expected <- c(row.expected, expected)
        
        row <- c(row,  expected)
      }
      
      if (options$percentagesRow) {
        row.proportions <- .crossTabCountsMatrixToRow(row.proportions.matrix, counts.matrix, type = "row.proportions")
        
        row.proportions<-c(row.row.proportions, row.proportions)
        
        row <- c(row,  row.proportions)
      }
      
      if (options$percentagesColumn) {
        col.proportions <- .crossTabCountsMatrixToRow(col.proportions.matrix, counts.matrix, type = "col.proportions")
        
        col.proportions<-c(row.col.proportions, col.proportions)
        
        row <- c(row,  col.proportions)
      }
      
      if (options$percentagesTotal) {
        total.proportions <- .crossTabCountsMatrixToRow(proportions.matrix, counts.matrix, type = "proportions")
        
        total.proportions <- c(row.total.proportions, total.proportions)
        
        row <- c(row,  total.proportions)
      }
      
    } else 
      row <- list()
    if(var.name != "")
      row[[var.name]] <- "Total"
    if (!options$countsExpected && !options$percentagesRow && 
        !options$percentagesCol && !options$percentagesTotal)
      row[[".isNewGroup"]] <- TRUE
    
    row <- .crossTabLayerNames(row, group)
    rows[[length(rows) + 1]] <- row
    counts.rows <- c(counts.rows, rows)
  }
  main  <- createJaspState(counts.rows)
  table <- analysisContainer[["crossTabMain"]]
  main$dependOn(optionsFromObject = table)
  analysisContainer[["resultsMain"]] <- main
  return(counts.rows)
}

.crossTabTestsRows <- function(analysisContainer, var.name, groupList, options, ready, counts.fp) {
  tests.rows     <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    row[["type[N]"]] <- "N"
    row[["df[N]"]]   <- ""
    row[["p[N]"]]    <- ""
    row[["MPR[N]"]]  <- ""
    
    if (ready){
      sum <- sum(counts.matrix)
      if(counts.fp)
        row[["value[N]"]] <- sum
      else
        row[["value[N]"]] <- as.integer(sum)
    }
    else 
      row[["value[N]"]] <- "."
    
    if (options$chiSquared) {
      
      row[["type[chiSquared]"]] <- "\u03A7\u00B2"
      
      if (ready) {
        
        chi.result <- try({
          chi.result <- stats::chisq.test(counts.matrix, correct = FALSE)
        })
        
        if (isTryError(chi.result) || is.na(chi.result$statistic)) {
          row[["value[chiSquared]"]] <- NaN
          row[["df[chiSquared]"]]    <- " "
          row[["p[chiSquared]"]]     <- " "
          row[["MPR[chiSquared]"]]   <- " "
          row[["type[chiSquared]"]] <- "\u03A7\u00B2\u207B"
          message <- "\u03A7\u00B2 could not be calculated - At least one row or column contains all zeros"
          analysisContainer[["crossTabChisq"]]$addFootnote(message, symbol = "\u207B")
        } else {
          row[["value[chiSquared]"]] <- unname(chi.result$statistic)
          row[["df[chiSquared]"]]    <- unname(chi.result$parameter)
          row[["p[chiSquared]"]]     <- unname(chi.result$p.value)
          row[["MPR[chiSquared]"]]   <- .VovkSellkeMPR(row[["p[chiSquared]"]])
        }
      } else 
        row[["value[chiSquared]"]] <- "."
    }
    
    if (options$chiSquaredContinuityCorrection) {
      
      row[["type[chiSquared-cc]"]] <- "\u03A7\u00B2 continuity correction"
      
      if (ready) {
        
        chi.result <- try({
          chi.result <- stats::chisq.test(counts.matrix)
          #row <- list(Method = "Pearson's Chi-squared", 
                       #X2 = unname(chi$statistic), 
                       #df = unname(chi$parameter), 
                       #p  = chi$p.value)
        })
        
        if (isTryError(chi.result) || is.na(chi.result$statistic)) {
          row[["value[chiSquared-cc]"]] <- NaN
          row[["df[chiSquared-cc]"]]    <- " "
          row[["p[chiSquared-cc]"]]     <- " "
          row[["MPR[chiSquared-cc]"]]   <- " "
          row[["type[chiSquared-cc]"]] <- "\u03A7\u00B2 continuity correction\u1D43"
          message <- "\u03A7\u00B2 could not be calculated - At least one row or column contains all zeros"
          analysisContainer[["crossTabChisq"]]$addFootnote(message, symbol = "\u1D43")
        } else {
          row[["value[chiSquared-cc]"]] <- unname(chi.result$statistic)
          row[["df[chiSquared-cc]"]]    <- unname(chi.result$parameter)
          pVal                          <- unname(chi.result$p.value)
          row[["p[chiSquared-cc]"]]     <- pVal
          row[["MPR[chiSquared-cc]"]]   <- .VovkSellkeMPR(pVal)
        }
        
      } else 
        row[["value[chiSquared-cc]"]] <- "."
    }
    
    if (options$likelihoodRatio) {
      
      row[["type[likelihood]"]] <- "Likelihood ratio"
      
      if (ready) {
        
        chi.result <- try({
          chi.result <- vcd::assocstats(counts.matrix)
        })
        
        if (isTryError(chi.result)) {
          row[["value[likelihood]"]] <- NaN
          row[["df[likelihood]"]]    <- ""
          row[["p[likelihood]"]]     <- ""
          if (options$VovkSellkeMPR)
            row[["MPR[likelihood]"]]   <- ""
        } else {
          row[["value[likelihood]"]] <- chi.result$chisq_tests[1]
          row[["df[likelihood]"]]    <- chi.result$chisq_tests[3]
          pVal                       <- chi.result$chisq_tests[5]
          row[["p[likelihood]"]]     <- pVal
          if (options$VovkSellkeMPR)
            row[["MPR[likelihood]"]]   <- .VovkSellkeMPR(pVal)
        }
      } else 
        row[["value[likelihood]"]] <- "."
    }
    row <- .crossTabLayerNames(row, group)
    tests.rows[[length(tests.rows) + 1]] <- row
  }
  return(tests.rows)
}

.crossTabOddsRatioRows <- function(analysisContainer, var.name, groupList, options, ready) {
  odds.ratio.rows <- list()
  group.matrices  <- groupList$group.matrices
  groups          <- groupList$groups
  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    row[["type[oddsRatio]"]] <- "Odds ratio"
    if (ready) {
      if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {
        row[["value[oddsRatio]"]] <- NaN
        row[["low[oddsRatio]"]]   <- ""
        row[["up[oddsRatio]"]]    <- ""
        stop("Odds ratio restricted to 2 x 2 tables")
      } else {
        chi.result <- try({
          chi.result  <- vcd::oddsratio(counts.matrix)
          level       <- options$oddsRatioConfidenceIntervalInterval
          CI          <- stats::confint(chi.result, level = level)
          LogOR       <- unname(chi.result$coefficients)
          log.CI.low  <- CI[1]
          log.CI.high <- CI[2]
        })
        
        if (isTryError(chi.result)) 
          row[["value[oddsRatio]"]] <- NaN
        else if (is.na(chi.result))
          row[["value[oddsRatio]"]] <- NaN
        else {
          row[["value[oddsRatio]"]] <- LogOR
          row[["low[oddsRatio]"]]   <- log.CI.low
          row[["up[oddsRatio]"]]    <- log.CI.high
        }
        row[["value[oddsRatio]"]] <- LogOR
        row[["low[oddsRatio]"]]   <- log.CI.low
        row[["up[oddsRatio]"]]    <- log.CI.high
      }
    }

    row[["type[FisherTest]"]] <- "Fisher's exact test "
    if (ready) {
      
      if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {
        row[["value[FisherTest]"]] <- NaN
        row[["low[FisherTest]"]]   <- ""
        row[["up[FisherTest]"]]    <- ""
      } else {
        chi.result <- try({
          conf.level  <- options$oddsRatioConfidenceIntervalInterval
          chi.result  <- stats::fisher.test(counts.matrix, conf.level = conf.level)
          OR          <- unname(chi.result$estimate)
          logOR       <- log(OR)
          log.CI.low  <- log(chi.result$conf.int[1])
          log.CI.high <- log(chi.result$conf.int[2])
        })
        
        if (isTryError(chi.result)) 
          row[["value[FisherTest]"]] <- NaN
        else if (is.na(chi.result)) 
          row[["value[FisherTest]"]] <- NaN
        else {
          row[["value[FisherTest]"]] <- logOR
          row[["low[FisherTest]"]]   <- log.CI.low
          row[["up[FisherTest]"]]    <- log.CI.high
        }
        row[["value[FisherTest]"]] <- logOR
        row[["low[FisherTest]"]]   <- log.CI.low
        row[["up[FisherTest]"]]    <- log.CI.high
      }
    }
    
    row <- .crossTabLayerNames(row, group)
    odds.ratio.rows[[length(odds.ratio.rows) + 1]] <- row
  }
  return(odds.ratio.rows)
}

.crossTabNominalRows <- function(analysisContainer, var.name, groupList, options, ready) {
  nominal.rows   <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) 
      group <- groups[[g]]
    else 
      group <- NULL
    
    row <- list()
    if (options$contingencyCoefficient) 
      row <- .crossTabNominalMatrixToRow(analysisContainer, row, counts.matrix, type = "ContCoef", ready)
    if (options$phiAndCramersV) {
      row <- .crossTabNominalMatrixToRow(analysisContainer, row, counts.matrix, type = "PhiCoef", ready)
      row <- .crossTabNominalMatrixToRow(analysisContainer, row, counts.matrix, type = "CramerV", ready)
    }
    if (options$lambda) {
      row[["type[LambdaR]"]] <- paste("Lambda (", options$rows, "dependent)", sep =  " ")
      row[["type[LambdaC]"]] <- paste("Lambda (", options$columns, "dependent)", sep =  " ")
      if (ready) {
        N       <- sum(counts.matrix)
        E1R     <- N - max(rowSums(counts.matrix))
        E2R     <- sum(apply(counts.matrix,2, function (x) sum(x) - max(x) ))
        lambdaR <- (E1 - E2)/E1
        row[["value[LambdaR]"]] <- lambdaR
        if (is.na(lambdaR))
          row[["value[LambdaR]"]] <- NaN
        E1C     <- N - max(colSums(counts.matrix))
        E2C     <- sum(apply(counts.matrix, 1, function (x) sum(x) - max(x) ))
        lambdaC <- (E1 - E2)/E1
        row[["value[LambdaC]"]] <- lambdaC
        if (is.na(lambdaC))
          row[["value[LambdaC]"]] <- NaN
      } else {
        row[["value[LambdaR]"]] <- "."
        row[["value[LambdaC]"]] <- "."
      }
    }
    
    row <- .crossTabLayerNames(row, group)
    nominal.rows[[length(nominal.rows) + 1]] <- row
  }
  return(nominal.rows)
}

.crossTabGammaRows <- function(analysisContainer, var.name, groupList, options, ready) {
  ordinal.rows   <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) 
      group <- groups[[g]]
    else 
      group <- NULL
    
    row <- list()
    if(ready) {
      chi.result <- try({
        chi.result <- vcdExtra::GKgamma(counts.matrix)
      })
      if (isTryError(chi.result)) {
        row[["value[gammaCoef]"]] <- NaN
      } else {
        row[["value[gammaCoef]"]] <- chi.result$gamma
        row[["Sigma[gammaCoef]"]] <- chi.result$sigma
        row[["low[gammaCoef]"]]   <- chi.result$CI[1]
        row[["up[gammaCoef]"]]    <- chi.result$CI[2]
      }
    }
    else {
      row[["value[gammaCoef]"]] <- "."
      row[["Sigma[gammaCoef]"]] <- "."
      row[["low[gammaCoef]"]]   <- "."
      row[["up[gammaCoef]"]]    <- "."
    }
    row <- .crossTabLayerNames(row, group)
    ordinal.rows[[length(ordinal.rows) + 1]] <- row
  }
  return(ordinal.rows)
}

.crossTabKendallsTauRows <- function(analysisContainer, var.name, groupList, options, ready) {
  kendalls.rows  <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
    
  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) 
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    if (ready) {
      chi.result <- try({
        count.dat  <- stats::ftable(counts.matrix)
        count.dat  <- as.data.frame(count.dat)
        Var1       <- rep(count.dat[,1],times = count.dat$Freq)
        Var2       <- rep(count.dat[,2],times = count.dat$Freq)
        chi.result <- stats::cor.test(as.numeric(Var1), 
                                      as.numeric(Var2), 
                                      method = "kendall")
      })
      
      if (isTryError(chi.result)) 
        row[["value[kTauB]"]] <- NaN
      else {
        row[["value[kTauB]"]] <- unname(chi.result$estimate)
        row[["p[kTauB]"]]     <- chi.result$p.value
        if (options$VovkSellkeMPR)
          row[["MPR[kTauB]"]] <- .VovkSellkeMPR(row[["p[kTauB]"]])
        row[["statistic[kTauB]"]] <- unname(chi.result$statistic)
      }
    } else {
      row[["value[kTauB]"]] <- "."
      row[["p[kTauB]"]]     <- "."
      if (options$VovkSellkeMPR)
        row[["MPR[kTauB]"]] <- "."
      row[["statistic[kTauB]"]] <- "."
    }
    row <- .crossTabLayerNames(row, group)
    kendalls.rows[[length(kendalls.rows) + 1]] <- row
  }
  return(kendalls.rows)
}
