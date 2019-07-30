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

# Preprocessing functions ----
.crossTabReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
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

# Combinations of rows, columns, layers ----
.crossTabComputeAnalyses <- function(dataset, options, ready) {
  rows    <- as.vector(options$rows,    "character")
  columns <- as.vector(options$columns, "character")
  
  if (length(rows) == 0)
    rows <- ""
  
  if (length(columns) == 0)
    columns <- ""
  analyses <- list()
  analyses$analyses <- data.frame("columns" = columns, stringsAsFactors = FALSE)
  analyses$analyses <- cbind(analyses$analyses, 
                             "rows" = rep(rows, each = dim(analyses$analyses)[1]), 
                    stringsAsFactors = FALSE)
  
  for (layer in options$layers) {
    layer.vars <- as.vector(layer$variables, "character")
    analyses$analyses <- cbind(analyses$analyses, 
                               rep(layer.vars, each = dim(analyses$analyses)[1]), 
                      stringsAsFactors = FALSE)
    names(analyses$analyses)[dim(analyses$analyses)[2]] <- layer$name
  }
  
  analyses$analyses <- .dataFrameToRowList(analyses$analyses)
  for (i in 1:length(analyses$analyses)) {
    analysis   <- analyses$analyses[[i]]
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
      
      lvls <- base::levels(subdataset[[ .v(analysis[[3]]) ]])
      
      if (length(lvls) < 2)
        lvls <- ""
      else 
        lvls <- c(lvls, "")  # blank means total
      
      # here we create all combinations of the levels from the layers
      # it is easiest to do this with a data frame
      # at the end we convert this to a list of rows
      
      groups <- data.frame(lvls, stringsAsFactors=FALSE)
      base::names(groups) <- analysis[[3]]
      
      if (length(analysis) >= 4) {
        
        for (j in 4:(length(analysis))) {
          lvls <- base::levels(subdataset[[ .v(analysis[[j]]) ]])
          lvls <- c(lvls, "")  # blank means total
          
          groups <- cbind(rep(lvls, each = dim(groups)[1]), groups, 
                          stringsAsFactors = FALSE)
          names(groups)[1] <- analysis[[j]]
        }
      }
      
      # convert all the combinations to a list of rows
      groups <- .dataFrameToRowList(groups)
      
    } else  # if layers are not specified
      groups <- NULL
    analyses$groups[[i]] <- groups
    if (!is.null(counts.var))
      counts <- stats::na.omit(subdataset[[ .v(counts.var) ]])
    grp.mat <- .crossTabGroupMatrices(subdataset, analysis$rows, 
                                             analysis$columns, groups, 
                                             counts.var, 
                                             options$rowOrder=="descending", 
                                             options$columnOrder=="descending", 
                                             ready)
    analyses$group.matrices[[i]] <- grp.mat
  }
  return(analyses)
}

# Container ----
.crossTabContainer <- function(jaspResults, options, analyses, ready) {
  for (i in 1:length(analyses$analyses)){
    analysis <- analyses$analyses[[i]]
    if (is.null(jaspResults[[paste0("tables", i)]])) {
      container <- createJaspContainer()
      container$dependOn(options              = c("layers", "counts"),
                         optionContainsValue  = list(rows     = analysis$rows, 
                                                     columns  = analysis$columns))
      jaspResults[[paste0("tables", i)]] <- container
    }
  }
}

# Output Tables ----
.crossTabMain <- function(jaspResults, dataset, options, analyses, ready) {
  for (i in 1:length(analyses$analyses)){
    
    analysis <- analyses$analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
    if (!is.null(analysisContainer[["crossTabMain"]])) 
      next
    
    # Create table
    crossTabMain <- createJaspTable(title = "Contingency Tables")
    crossTabMain$dependOn(c("countsExpected", "percentagesRow", 
                            "percentagesColumn", "percentagesTotal", 
                            "rowOrder", "columnOrder"))
    crossTabMain$showSpecifiedColumnsOnly <- TRUE
    crossTabMain$position <- 1
      
    .crossTabLayersColumns(crossTabMain, analysis)
    if(analysis$rows == "")
      crossTabMain$addColumnInfo(name = analysis$rows, title = " ", 
                                 type = "string", combine = TRUE)
    else 
      crossTabMain$addColumnInfo(name = analysis$rows, type = "string", 
                                 combine = TRUE)
    
    # whether the counts are float point or not; changes formatting
    counts.fp <- FALSE  
      
    if (options$counts != "") {
      counts <- dataset[[ .v(options$counts) ]]
      if (identical(counts, as.integer(counts)) == FALSE)          
        counts.fp <- TRUE
    }
    
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
    res <- try(.crossTabCountsRows(jaspResults, analysis$rows, analyses$group.matrices[[i]], 
                                   analyses$groups[[i]], analysisContainer, options, ready, counts.fp))
    .crossTabSetErrorOrFill(res, crossTabMain)
  }
}

.crossTabChisq <- function(jaspResults, dataset, options, analyses, ready) {
  if(!options$chiSquared && 
     !options$chiSquaredContinuityCorrection && 
     !options$likelihoodRatio) 
    return()
  for (i in 1:length(analyses$analyses)){
    analysis <- analyses$analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
    if (!is.null(analysisContainer[["crossTabChisq"]])) 
      next
    
    # Create table
    crossTabChisq <- createJaspTable(title = "Chi-Squared Tests")
    crossTabChisq$dependOn(c("chiSquared", "chiSquaredContinuityCorrection", 
                             "likelihoodRatio", "VovkSellkeMPR"))
    crossTabChisq$showSpecifiedColumnsOnly <- TRUE
    crossTabChisq$position <- 2
    
    # whether the counts are float point or not; changes formatting
    counts.fp <- FALSE  
    
    if (options$counts != "") {
      counts <- dataset[[ .v(options$counts) ]]
      if (identical(counts, as.integer(counts)) == FALSE)          
        counts.fp <- TRUE
    }
    
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
    
    res <- try(.crossTabTestsRows(jaspResults, analysis$rows, analyses$group.matrices[[i]], 
                                  analyses$groups[[i]], analysisContainer, options, ready, counts.fp))
    .crossTabSetErrorOrFill(res, crossTabChisq)
  }
}

.crossTabLogOdds <- function(jaspResults, dataset, options, analyses, ready) {
  if(!options$oddsRatio)
    return()
  for (i in 1:length(analyses$analyses)){
    analysis <- analyses$analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
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
    crossTabLogOdds$addColumnInfo(name = "type[oddsRatio]",  title = "", type = "string")
    crossTabLogOdds$addColumnInfo(name = "value[oddsRatio]", title = "Log Odds Ratio", 
                                  type = "number")
    crossTabLogOdds$addColumnInfo(name = "low[oddsRatio]",   title = "Lower", 
                                  overtitle = ci.label, type = "number", format = "dp:3")
    crossTabLogOdds$addColumnInfo(name = "up[oddsRatio]",    title = "Upper", 
                                  overtitle = ci.label, type = "number", format = "dp:3")
    
    crossTabLogOdds$addColumnInfo(name = "type[FisherTest]",  title = "", type = "string")
    crossTabLogOdds$addColumnInfo(name = "value[FisherTest]", title = "Log Odds Ratio", 
                                  type = "number")
    crossTabLogOdds$addColumnInfo(name = "low[FisherTest]",   title = "Lower", 
                                  overtitle = ci.label, type = "number", format = "dp:3")
    crossTabLogOdds$addColumnInfo(name = "up[FisherTest]",    title = "Upper", 
                                  overtitle = ci.label, type = "number", format = "dp:3")
    
    analysisContainer[["crossTabLogOdds"]] <- crossTabLogOdds
    
    res <- try(.crossTabOddsRatioRows(jaspResults, analysis$rows, 
                                      analyses$group.matrices[[i]], 
                                      analyses$groups[[i]], analysisContainer, options, ready))
    .crossTabSetErrorOrFill(res, crossTabLogOdds)
  }
}

.crossTabNominal <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$contingencyCoefficient && !options$phiAndCramersV && !options$lambda)
    return()
  for (i in 1:length(analyses$analyses)){
    analysis <- analyses$analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
    if (!is.null(analysisContainer[["crossTabNominal"]])) 
      next
    
    # Create table
    crossTabNominal <- createJaspTable(title = "Nominal")
    crossTabNominal$dependOn(c("contingencyCoefficient", "phiAndCramersV"))
    crossTabNominal$showSpecifiedColumnsOnly <- TRUE
    crossTabNominal$position <- 4
    
    # Add columns to table
    .crossTabLayersColumns(crossTabNominal, analysis)
    if (options$contingencyCoefficient){
      crossTabNominal$addColumnInfo(name = "type[ContCoef]",  title = "", 
                                    type = "string")
      crossTabNominal$addColumnInfo(name = "value[ContCoef]", title = "Value", 
                                    type = "number")
    }
    
    if (options$phiAndCramersV) {
      crossTabNominal$addColumnInfo(name = "type[PhiCoef]",  title = "", type = "string")
      crossTabNominal$addColumnInfo(name = "value[PhiCoef]", title = "Value", 
                                    type = "number")
      crossTabNominal$addColumnInfo(name = "type[CramerV]",  title = "", type = "string")
      crossTabNominal$addColumnInfo(name = "value[CramerV]", title = "Value", 
                                    type = "number")
    }
    
    if (options$lambda) {
      crossTabNominal$addColumnInfo(name = "type[LambdaR]",  title = "", type = "string")
      crossTabNominal$addColumnInfo(name = "value[LambdaR]", title = "Value", 
                                    type = "number")
      crossTabNominal$addColumnInfo(name = "type[LambdaC]",  title = "", type = "string")
      crossTabNominal$addColumnInfo(name = "value[LambdaC]", title = "Value", 
                                    type = "number")
    }
    
    analysisContainer[["crossTabNominal"]] <- crossTabNominal
    
    res <- try(.crossTabNominalRows(jaspResults, analysis$rows, 
                                    analyses$group.matrices[[i]], 
                                    analyses$groups[[i]], analysisContainer, options, ready))
    .crossTabSetErrorOrFill(res, crossTabNominal)
  }
}

.crossTabGamma <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$gamma)
    return()
  for (i in 1:length(analyses$analyses)){
    analysis <- analyses$analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
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
    
    res <- try(.crossTabGammaRows(jaspResults, analysis$rows, 
                                  analyses$group.matrices[[i]], 
                                  analyses$groups[[i]], analysisContainer, options, ready))
    .crossTabSetErrorOrFill(res, crossTabGamma)
  }
}

.crossTabKendallTau <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$kendallsTauB)
    return()
  for (i in 1:length(analyses$analyses)){
    analysis <- analyses$analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
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
    crossTabKendallTau$addColumnInfo(name = "p[kTauB]", title = "p", 
                                     type = "number", format = "dp:3;p:.001")
    if (options$VovkSellkeMPR) 
      crossTabKendallTau$addColumnInfo(name = "MPR[kTauB]", title = "VS-MPR\u002A", 
                                       type = "number")

    analysisContainer[["crossTabKendallTau"]] <- crossTabKendallTau
    
    res <- try(.crossTabKendallsTauRows(jaspResults, analysis$rows, 
                                        analyses$group.matrices[[i]], 
                                        analyses$groups[[i]], analysisContainer, options, ready))
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
      private.name <- base::paste(column.name,"[counts]", sep = "")
      
      if (counts.fp || options$countsExpected || options$percentagesRow || 
          options$percentagesColumn || options$percentagesTotal ) 
        table$addColumnInfo(name = private.name, title = ".", 
                            overtitle = ".", type = "number", format = "sf:4;dp:2")
      else 
        table$addColumnInfo(name = private.name, title = ".", 
                            overtitle = ".", type = "integer")
      
      if (options$countsExpected) { 
        private.name <- base::paste(column.name,"[expected]", sep = "")
        table$addColumnInfo(name = private.name, title = ".", 
                            type = "number", format = "sf:4;dp:2")
      }
      if (options$percentagesRow) {
        private.name <- base::paste(column.name,"[row.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = ".", 
                            type = "number", format = "dp:1;pc")
      }
      if (options$percentagesColumn) {
        private.name <- base::paste(column.name,"[col.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = ".", 
                            type = "number", format = "dp:1;pc")
      }
      if (options$percentagesTotal) {
        private.name <- base::paste(column.name,"[proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = ".", 
                            type = "number", format = "dp:1;pc")
      }
    }
  } else {
    if (is.factor(dataset[[ .v(analysis$columns) ]] )) {
      lvls <- base::levels(dataset[[ .v(analysis$columns) ]])
      if (options$columnOrder == "descending")
        lvls <- base::rev(lvls)
    } else {
      lvls <- base::unique(dataset[[ .v(analysis$columns) ]])
      if (options$columnOrder == "descending")
        lvls <- base::rev(lvls, decreasing = TRUE)
    }
      
    for (column.name in lvls) {
      private.name <- base::paste(column.name,"[counts]", sep = "")
      
      if (counts.fp || options$countsExpected || options$percentagesRow || 
          options$percentagesColumn || options$percentagesTotal ) 
        table$addColumnInfo(name = private.name, title = column.name, 
                            overtitle = overTitle, type = "number", 
                            format = "sf:4;dp:2")
      else 
        table$addColumnInfo(name = private.name, title = column.name, 
                            overtitle = overTitle, type = "integer")
      
      if (options$countsExpected) { 
        private.name <- base::paste(column.name,"[expected]", sep = "")
        table$addColumnInfo(name = private.name, title = column.name, 
                            type = "number", format = "sf:4;dp:2")
      }
      if (options$percentagesRow) {
        private.name <- base::paste(column.name,"[row.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = column.name, 
                            type = "number", format = "dp:1;pc")
      }
      if (options$percentagesColumn) {
        private.name <- base::paste(column.name,"[col.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = column.name, 
                            type = "number", format = "dp:1;pc")
      }
      if (options$percentagesTotal) {
        private.name <- base::paste(column.name,"[proportions]", sep = "")
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

.crossTabLayerNames <- function(row, group) {
  for (layer in names(group)) {
    level <- group[[layer]]
    if (level == "")
      row[[layer]] <- "Total"
    else 
      row[[layer]] <- level
  }
  #browser()
  return(row)
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
        row.levels <- base::levels(dataset[[ .v(rows) ]])
      if (columns != "")
        col.levels <- base::levels(dataset[[ .v(columns) ]])
      
      ss.matrix <- base::matrix(0, 
                                nrow  = length(row.levels), 
                                ncol     = length(col.levels), 
                                dimnames = list(row.levels, col.levels))
      
    } else if (is.null(counts)) {
      ss.dataset <- base::subset(dataset, select = .v(c(rows, columns)))
      ss.table   <- base::table(ss.dataset)
      ss.matrix  <- base::matrix(ss.table, nrow = dim(ss.table)[1], 
                                 ncol = dim(ss.table)[2], 
                                 dimnames = dimnames(ss.table))
      
    } else {
      ss.dataset <- base::subset(dataset, select = .v(c(rows, columns, counts)))
      ss.matrix  <- base::tapply(ss.dataset[[ .v(counts) ]], 
                                 list(ss.dataset[[ .v(rows) ]], 
                                      ss.dataset[[ .v(columns) ]]), 
                                 base::sum)
      ss.matrix[is.na(ss.matrix)] <- 0
    }
    
    if (rowOrderDescending)
      ss.matrix <- base::apply(ss.matrix, 2, base::rev)
    else 
      ss.matrix <- ss.matrix
    
    if (columnOrderDescending)
      ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
    else 
      ss.matrix <- ss.matrix
    
    ss.matrix[base::is.na(ss.matrix)] <- 0
    
    matrices[[1]] <- ss.matrix
  } else {
    
    for (group in groups) {
      
      group <- group[group != ""]
      
      if (!ready) { # do nothing
      } else if (length(group) == 0) {
        ss.dataset <- base::subset(dataset, select = .v(c(rows, columns, counts)))
      } else {
        ss.filter.string <- base::paste(.v(names(group)), "==\"", group, "\"", 
                                        sep = "", collapse = "&")
        ss.expression    <- base::parse(text = ss.filter.string)
        ss.dataset	     <- base::subset(dataset, 
                                         select = .v(c(rows, columns, counts)), 
                                         subset = eval(ss.expression))
      }
      
      if (!ready) {
        ss.matrix <- base::matrix(c(0,0,0,0), nrow = 2, ncol = 2)
      } else if (is.null(counts)) {
        ss.table  <- base::table(ss.dataset)
        ss.matrix <- base::matrix(ss.table, 
                                  nrow = dim(ss.table)[1], 
                                  ncol = dim(ss.table)[2], 
                                  dimnames = dimnames(ss.table))
      } else {
        ss.matrix <- base::tapply(ss.dataset[[ .v(counts) ]], 
                                  list(ss.dataset[[ .v(rows) ]], 
                                       ss.dataset[[ .v(columns) ]]), 
                                  base::sum)
      }
      
      ss.matrix[base::is.na(ss.matrix)] <- 0
      
      if (rowOrderDescending)
        ss.matrix <- base::apply(ss.matrix, 2, base::rev)
      else 
        ss.matrix <- ss.matrix
      
      if (columnOrderDescending)
        ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
      else 
        ss.matrix <- ss.matrix
      matrices[[length(matrices) + 1]] <- ss.matrix
    }
  }
  return(matrices)
}

# Table Results
.crossTabCountsRows <- function(jaspResults, var.name, group.matrices, 
                                groups, analysisContainer, options, ready, counts.fp) {
  if(!is.null(analysisContainer[["resultsMain"]]))
    return()
  counts.rows     <- list()

  for (g in 1:length(group.matrices)) {
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
        base::prop.table(counts.matrix, 1)
      })
      if (isTryError(row.proportions.matrix)) {
        row.proportions.matrix    <- counts.matrix
        row.proportions.matrix[,] <- "&nbsp;"
      }
      
      col.proportions.matrix <- try({
        base::prop.table(counts.matrix, 2)
      })
      if (isTryError(col.proportions.matrix)) {
        col.proportions.matrix    <- counts.matrix
        col.proportions.matrix[,] <- "&nbsp;"
      }
      
      proportions.matrix <- try({
        base::prop.table(counts.matrix, margin = NULL)
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
        names(row) <- base::paste(names(row),"[counts]",	sep = "")
        sum <- base::sum(counts.matrix[j,])
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
          
          if (inherits(expected.matrix[1,1], "character"))
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
          
          if (inherits(row.proportions.matrix[1,1], "character"))
            row.prop <- ""
          else
            row.prop <- base::sum(row.proportions.matrix[j,])
          row.proportions[["total[row.proportions]"]] <- row.prop
          
          row.proportions <- c(row.row.proportions, row.proportions)
          row <- c(row, row.proportions)
        }
        
        if (options$percentagesColumn) {
          
          row.col.proportions[["type[col.proportions]"]] <- " % within column"
          
          col.proportions <- as.list(col.proportions.matrix[j,])
          names(col.proportions) <- paste(names(col.proportions),
                                          "[col.proportions]",  sep = "")
          
          if (inherits(col.proportions.matrix[1,1], "character")) {
            col.proportions[["total[col.proportions]"]] <- ""
          } else {
            row.sum  <- base::margin.table(counts.matrix, 1)
            row.prop <- as.list( base::prop.table(row.sum))
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
          
          if (inherits(proportions.matrix[1,1], "character"))
            tot.prop <- ""
          else 
            tot.prop <- base::sum(proportions.matrix[j,])
          total.proportions[["total[proportions]"]] <- tot.prop
          
          total.proportions <- c(row.total.proportions, total.proportions)
          row <- c(row, total.proportions)
        }
        
      } else 
        row <- list()
      row[[var.name]] <- dimnames(counts.matrix)[[1]][j]
      row <- .crossTabLayerNames(row, group)
      
      if (j == 1 && options$countsExpected == FALSE && 
          options$percentagesRow == FALSE && 
          options$percentagesCol == FALSE && 
          options$percentagesTotal == FALSE)
        row[[".isNewGroup"]] <- TRUE
      rows[[length(rows) + 1]] <- row
    }
    
    if (ready) {
      
      row <- apply(counts.matrix, 2, base::sum)
      row <- as.list(row)
      names(row) <- base::paste(names(row),"[counts]",	sep = "")
      sum <- base::sum(counts.matrix)
      if(counts.fp || options$countsExpected || options$percentagesRow || 
         options$percentagesColumn || options$percentagesTotal)
        row[["total[counts]"]] <- sum
      else
        row[["total[counts]"]] <- as.integer(sum)
      row <- c(row.count, row)
      
      if (options$countsExpected) {
        
        if (inherits(expected.matrix[1,1], "character"))
          expected <- expected.matrix[1,]
        else
          expected <- apply(expected.matrix, 2, base::sum)
        
        expected <- as.list(expected)
        names(expected) <- paste(names(expected),"[expected]", sep = "")
        
        if (inherits(expected.matrix[1,1], "character"))
          expected[["total[expected]"]] <- ""
        else 
          expected[["total[expected]"]] <- base::sum(expected.matrix)
        
        expected <- c(row.expected, expected)
        
        row <- c(row,  expected)
      }
      
      if (options$percentagesRow) {
        
        if (inherits(row.proportions.matrix[1,1], "character"))
          row.proportions <- row.proportions.matrix[1,]
        else {
          m <- base::margin.table(counts.matrix, 2)
          rowproportion <- base::prop.table(m)
        }
        
        row.proportions <- as.list(rowproportion)
        names(row.proportions) <- paste(names(row.proportions),
                                        "[row.proportions]", sep = "")
        
        if (inherits(row.proportions.matrix[1,1], "character"))
          row.prop <- ""
        else 
          row.prop <- base::sum(rowproportion)
        row.proportions[["total[row.proportions]"]] <- row.prop
        
        row.proportions<-c(row.row.proportions, row.proportions)
        
        row <- c(row,  row.proportions)
      }
      
      if (options$percentagesColumn) {
        
        if (inherits(col.proportions.matrix[1,1], "character"))
          col.proportions <- col.proportions.matrix[1,]
        else 
          colproportion <- apply(col.proportions.matrix, 2, base::sum)
        
        col.proportions <- as.list(colproportion)
        names(col.proportions) <- paste(names(col.proportions),
                                        "[col.proportions]", sep = "")
        
        if (inherits(row.proportions.matrix[1,1], "character")) {
          col.proportions[["total[col.proportions]"]] <- ""
        } else {
          row.sum  <- base::margin.table(counts.matrix, 1)
          row.prop <- base::prop.table(row.sum)
          col.prop <- base::sum(row.prop)
          col.proportions[["total[col.proportions]"]] <- col.prop
        }
        
        col.proportions<-c(row.col.proportions, col.proportions)
        
        row <- c(row,  col.proportions)
      }
      
      if (options$percentagesTotal) {
        
        if (inherits(proportions.matrix[1,1], "character"))
          total.proportions <- proportions.matrix[1,]
        else 
          total.proportions <- apply(proportions.matrix, 2, base::sum)
        
        total.proportions <- as.list(total.proportions)
        names(total.proportions) <- paste(names(total.proportions),
                                          "[proportions]", sep = "")
        
        if (inherits(proportions.matrix[1,1], "character")) {
          total.proportions[["total[proportions]"]] <- ""
        } else {
          tot.prop <- base::sum(proportions.matrix)
          total.proportions[["total[proportions]"]] <- tot.prop
        }
        
        total.proportions <- c(row.total.proportions, total.proportions)
        
        row <- c(row,  total.proportions)
      }
      
    } else 
      row <- list()
    if(var.name != "")
      row[[var.name]] <- "Total"
    if (options$countsExpected == FALSE && options$percentagesRow   == FALSE && 
        options$percentagesCol == FALSE && options$percentagesTotal == FALSE)
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

.crossTabTestsRows <- function(jaspResults, var.name, group.matrices, 
                               groups, analysisContainer, options, ready, counts.fp) {
  if(!options$chiSquared && 
     !options$chiSquaredContinuityCorrection && 
     !options$likelihoodRatio) 
    return()
  if(!is.null(analysisContainer[["resultsChisq"]]))
    return()
  
  tests.rows <- list()
  for (g in 1:length(group.matrices)) {
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
      sum <- base::sum(counts.matrix)
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
          message <- "\u03A7\u00B2 could not be calculated - At least one row or column contains all zeros"
          analysisContainer[["crossTabChisq"]]$addFootnote(message)
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
          #row <- list(Method="Pearson's Chi-squared", 
                       #X2=unname(chi$statistic), 
                       #df=unname(chi$parameter), 
                       #p = chi$p.value)
        })
        
        if (isTryError(chi.result) || is.na(chi.result$statistic)) {
          
          row[["value[chiSquared-cc]"]] <- NaN
          row[["df[chiSquared-cc]"]]    <- " "
          row[["p[chiSquared-cc]"]]     <- " "
          row[["MPR[chiSquared-cc]"]]   <- " "
          message <- "\u03A7\u00B2 could not be calculated - At least one row or column contains all zeros"
          analysisContainer[["crossTabChisq"]]$addFootnote(message)
          
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
  chisq <- createJaspState(tests.rows)
  table <- analysisContainer[["crossTabChisq"]]
  chisq$dependOn(optionsFromObject = table)
  analysisContainer[["resultsChisq"]] <- chisq
  return(tests.rows)
}

.crossTabOddsRatioRows <- function(jaspResults, var.name,  group.matrices, 
                                   groups, analysisContainer, options, ready) {
  if(!options$oddsRatio)
    return()
  if(!is.null(analysisContainer[["resultsLogOdds"]]))
    return()
  odds.ratio.rows <- list()
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    if (options$oddsRatio ) {
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
    } else 
      row[["value[oddsRatio]"]] <- "."
    
    if (options$oddsRatio ) {
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
      
    } else 
      row[["value[FisherTest]"]] <- "."
   
    row <- .crossTabLayerNames(row, group)
    odds.ratio.rows[[length(odds.ratio.rows) + 1]] <- row
  }
  logOdds <- createJaspState(odds.ratio.rows)
  table   <- analysisContainer[["crossTabLogOdds"]]
  logOdds$dependOn(optionsFromObject = table)
  analysisContainer[["resultsLogOdds"]] <- logOdds
  return(odds.ratio.rows)
}

.crossTabNominalRows <- function(jaspResults, var.name, group.matrices, 
                                 groups, analysisContainer, options, ready) {
  if (!options$contingencyCoefficient && !options$phiAndCramersV && !options$lambda)
    return()
  if(!is.null(analysisContainer[["resultsNominal"]]))
    return()
  
  nominal.rows <- list()
  
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) 
      group <- groups[[g]]
    else 
      group <- NULL
    
    row <- list()
    if (options$contingencyCoefficient) {
      row[["type[ContCoef]"]] <- "Contingency coefficient"
      if (ready) {
        chi.result <- try({
          chi.result <- vcd::assocstats(counts.matrix)
        })
        if (isTryError(chi.result)) 
          row[["value[ContCoef]"]] <- NaN
        else if (is.na(chi.result$contingency)) {
          row[["value[ContCoef]"]] <- NaN
          message <- "Value could not be calculated - At least one row or column contains all zeros"
          analysisContainer[["crossTabNominal"]]$addFootnote(message)
        } else
          row[["value[ContCoef]"]] <- chi.result$contingency
      } 
      else
        row[["value[ContCoef]"]] <- "."
    }
    if (options$phiAndCramersV) {
      row[["type[PhiCoef]"]] <- "Phi-coefficient"
      if (ready) {
        chi.result <- try({
          chi.result <- vcd::assocstats(counts.matrix)
        })
        if (isTryError(chi.result))
          row[["value[PhiCoef]"]] <- NaN
        else if (is.na(chi.result$phi)){
          row[["value[PhiCoef]"]] <- NaN
          message <- "Value could not be calculated - At least one row or column contains all zeros"
          analysisContainer[["crossTabNominal"]]$addFootnote(message)
        } else
          row[["value[PhiCoef]"]] <- chi.result$phi
      } 
      else 
        row[["value[PhiCoef]"]] <- "."
      row[["type[CramerV]"]] <- "Cramer's V "
      if (ready) {
        chi.result <- try({
          chi.result <- vcd::assocstats(counts.matrix)
        })
        if (isTryError(chi.result))
          row[["value[CramerV]"]] <- NaN
        else if (is.na(chi.result$cramer)){
          row[["value[CramerV]"]] <- NaN
          message <- "Value could not be calculated - At least one row or column contains all zeros"
          analysisContainer[["crossTabNominal"]]$addFootnote(message)
        } else 
          row[["value[CramerV]"]] <- chi.result$cramer
      } 
      else
        row[["value[CramerV]"]] <- "."
    }
    if (options$lambda) {
      row[["type[LambdaR]"]] <- paste("Lambda (", options$rows, 
                                      "dependent)", sep =  " ")
      if (ready) {
        N      <- sum(counts.matrix)
        E1     <- N - max(rowSums(counts.matrix))
        E2     <- sum(apply(counts.matrix,2, function (x) sum(x) - max(x) ))
        lambda <- (E1 - E2)/E1
        row[["value[LambdaR]"]] <- lambda
        if (is.na(lambda))
          row[["value[LambdaR]"]] <- NaN
        else
          row[["value[LambdaR]"]] <- "."
        row[["type[LambdaC]"]] <- paste("Lambda (", options$columns, 
                                        "dependent)", sep =  " ")
        if (ready) {
          N      <- sum(counts.matrix)
          E1     <- N - max(colSums(counts.matrix))
          E2     <- sum(apply(counts.matrix,1, function (x) sum(x)-max(x) ))
          lambda <- (E1 - E2)/E1
          row[["value[LambdaC]"]] <- lambda
        } else 
          row[["value[LambdaC]"]] <- "."
      }
    }
    
    row <- .crossTabLayerNames(row, group)
    nominal.rows[[length(nominal.rows) + 1]] <- row
  }
  nominal <- createJaspState(nominal.rows)
  table   <- analysisContainer[["crossTabNominal"]]
  nominal$dependOn(optionsFromObject = table)
  analysisContainer[["resultsNominal"]] <- nominal
  return(nominal.rows)
}

.crossTabGammaRows <- function(jaspResults, var.name, group.matrices, 
                               groups, analysisContainer, options, ready) {
  if (!options$gamma)
    return()
  if(!is.null(analysisContainer[["resultsGamma"]]))
    return()
  
  ordinal.rows <- list()
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) 
      group <- groups[[g]]
    else 
      group <- NULL
    
    row <- list()
    if (options$gamma) {
      
      row[["type[gammaCoef]"]] <- "Gamma coefficient"
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
    }
    row <- .crossTabLayerNames(row, group)
    ordinal.rows[[length(ordinal.rows) + 1]] <- row
  }
  gamma <- createJaspState(ordinal.rows)
  table <- analysisContainer[["crossTabGamma"]]
  gamma$dependOn(optionsFromObject = table)
  analysisContainer[["resultsGamma"]] <- gamma
  return(ordinal.rows)
}

.crossTabKendallsTauRows <- function(jaspResults, var.name, group.matrices, 
                                     groups, analysisContainer, options, ready) {
  if (!options$kendallsTauB)
    return()
  if(!is.null(analysisContainer[["resultsKendallsTau"]]))
    return()
  kendalls.rows <- list()
    
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) 
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    if (options$kendallsTauB) {
      row[["type[kTauB]"]] <- "Kendall's Tau-b"
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
    }
    row <- .crossTabLayerNames(row, group)
    kendalls.rows[[length(kendalls.rows) + 1]] <- row
  }
  kendalls <- createJaspState(kendalls.rows)
  table    <- analysisContainer[["crossTabKendallTau"]]
  kendalls$dependOn(optionsFromObject = table)
  analysisContainer[["resultsKendallsTau"]] <- kendalls
  return(kendalls.rows)
}
