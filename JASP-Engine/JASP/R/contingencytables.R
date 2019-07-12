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
  
  # Compute the results
  analyses <- .crossTabComputeAnalyses(dataset, options, ready)
  
  # Tables container
  .crossTabContainer(jaspResults, options, analyses, ready)
    
  # Output tables
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

# Results ----
.crossTabComputeAnalyses <- function(dataset, options, ready) {
  rows    <- as.vector(options$rows,    "character")
  columns <- as.vector(options$columns, "character")
  
  if (length(rows) == 0)
    rows <- ""
  
  if (length(columns) == 0)
    columns <- ""
  
  analyses <- data.frame("columns" = columns, stringsAsFactors = FALSE)
  analyses <- cbind(analyses, "rows" = rep(rows, each = dim(analyses)[1]), 
                    stringsAsFactors = FALSE)
  
  for (layer in options$layers) {
    layer.vars <- as.vector(layer$variables, "character")
    analyses <- cbind(analyses, rep(layer.vars, each = dim(analyses)[1]), 
                      stringsAsFactors = FALSE)
    names(analyses)[dim(analyses)[2]] <- layer$name
  }
  
  analyses <- .dataFrameToRowList(analyses)
  
  add.groups <- list()
  add.group.matrices <- list()
  
  for (i in 1:length(analyses)) {
    analysis <- analyses[[i]]
    counts.var <- options$counts
    if (counts.var == "")
      counts.var <- NULL
    
    if(ready) {
      all.vars <- c(unlist(analysis), counts.var)
      dataset <- subset(dataset, select = .v(all.vars))
    }
    
    # the following creates a 'groups' list
    # a 'group' represents a combinations of the levels from the layers
    # if no layers are specified, groups is null
    
    if (length(analysis) >= 3) {  # if layers are specified
      
      lvls <- base::levels(dataset[[ .v(analysis[[3]]) ]])
      
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
        
        for (j in 4:(length(analysis)-2)) {
          lvls <- base::levels(dataset[[ .v(analysis[[j]]) ]])
          lvls <- c(lvls, "")  # blank means total
          
          groups <- cbind(rep(lvls, each=dim(groups)[1]), groups, 
                          stringsAsFactors=FALSE)
          names(groups)[1] <- analysis[[j]]
        }
      }
      
      # convert all the combinations to a list of rows
      groups <- .dataFrameToRowList(groups)
      
    } else {  # if layers are not specified
      groups <- NULL
    }
    analyses[[i]] <- c(analyses[[i]], list("groups" = groups))
    if (!is.null(counts.var))
      counts <- stats::na.omit(dataset[[ .v(counts.var) ]])
    grp.mat <- .crosstabsCreateGroupMatrices(dataset, analysis$rows, 
                                             analysis$columns, groups, 
                                             counts.var, 
                                             options$rowOrder=="descending", 
                                             options$columnOrder=="descending", 
                                             ready)
    analyses[[i]] <- c(analyses[[i]], list("group.matrices" = grp.mat))
  }
  
  return(analyses)
}

# Container ----
.crossTabContainer <- function(jaspResults, options, analyses, ready) {
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    if (is.null(jaspResults[[paste0("tables", i)]])) {
      jaspResults[[paste0("tables", i)]] <- createJaspContainer()
      jaspResults[[paste0("tables", i)]]$dependOn(optionContainsValue = 
                                                  list("rows" = analysis$rows, 
                                                       "columns" = analysis$columns, 
                                                       "layers" = options$layers))
    }
  }
}

# Output Tables ----
.crossTabMain <- function(jaspResults, dataset, options, analyses, ready) {
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    if (!is.null(jaspResults[[paste0("tables", i)]][["crossTabMain"]])) 
      return()
      
    # Create table
    crossTabMain <- createJaspTable(title = "Contingency Tables")
    crossTabMain$dependOn(c("counts", "countsExpected", "percentagesRow", 
                            "percentagesColumn", "percentagesTotal"))
    crossTabMain$showSpecifiedColumnsOnly <- TRUE
      
    .crossTabMainAddLayersColumns(crossTabMain, analysis)
    if(analysis$rows == "")
      crossTabMain$addColumnInfo(name = analysis$rows, title = " ", 
                                 type = "string", combine = TRUE)
    else 
      crossTabMain$addColumnInfo(name = analysis$rows, type = "string", 
                                 combine = TRUE)

    counts.fp <- FALSE  # whether the counts are float point or not; changes formatting
      
    if (length(options$counts) > 0) {
      counts <- dataset[[ .v(options$counts) ]]
      if (identical(counts, as.integer(counts)) == FALSE)          
        counts.fp <- TRUE
    }
      
    if (options$countsExpected || options$percentagesRow || 
        options$percentagesColumn || options$percentagesTotal ) {
      crossTabMain$addColumnInfo(name = "type[counts]", title = "", type = "string")
        
      if (options$countsExpected) {
        crossTabMain$addColumnInfo(name = "type[expected]",        title = "", type = "string")
      }
      if (options$percentagesRow) {
        crossTabMain$addColumnInfo(name = "type[row.proportions]", title = "", type = "string")
      }
      if (options$percentagesColumn) {
        crossTabMain$addColumnInfo(name = "type[col.proportions]", title = "", type = "string")
      }
      if (options$percentagesTotal) {
        crossTabMain$addColumnInfo(name = "type[total.proportions]",     title = "", type = "string")
      }
    }
    
    .crossTabMainOvertitle(dataset, options, crossTabMain, analysis)
      
    # Totals columns
    if (counts.fp || options$countsExpected || options$percentagesRow || 
        options$percentagesColumn || options$percentagesTotal) 
      crossTabMain$addColumnInfo(name = "total[counts]",   title = "Total", type = "number", format = "sf:4;dp:2")
    else 
      crossTabMain$addColumnInfo(name = "total[counts]",   title = "Total", type = "integer")
      
    if (options$countsExpected) 
      crossTabMain$addColumnInfo(name = "total[expected]", title = "Total", type = "number", format = "sf:4;dp:2")

    if (options$percentagesRow)
      crossTabMain$addColumnInfo(name = "total[row.proportions]", title = "Total", type = "number", format = "dp:1;pc")
    
    if (options$percentagesColumn) 
      crossTabMain$addColumnInfo(name = "total[col.proportions]", title = "Total", type = "number", format = "dp:1;pc")
    
    if (options$percentagesTotal) 
      crossTabMain$addColumnInfo(name = "total[total.proportions]", title = "Total", type = "number", format = "dp:1;pc")
    
    jaspResults[[paste0("tables", i)]][["crossTabMain"]] <- crossTabMain
    
    res <- try(.crosstabsCreateCountsRows(analysis$rows, analysis$group.matrices, 
                                          analysis$groups, i, options, ready))
    if(isTryError(res))
      crossTabMain$setError(res)
    else {
      for (level in 1:length(res)) {
        row <- res[[level]]
        crossTabMain$addRows(row)
      }
    }
  }
}

.crossTabChisq <- function(jaspResults, dataset, options, analyses, ready) {
  if(!options$chiSquared && !options$chiSquaredContinuityCorrection && !options$likelihoodRatio) 
    return()
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    if (!is.null(jaspResults[[paste0("tables", i)]][["crossTabChisq"]])) 
      return()
    # Create table
    crossTabChisq <- createJaspTable(title = "Chi-Squared Tests")
    crossTabChisq$dependOn(c("chiSquared", "chiSquaredContinuityCorrection", 
                             "likelihoodRatio", "VovkSellkeMPR"))
    crossTabChisq$showSpecifiedColumnsOnly <- TRUE
    
    counts.fp <- FALSE  # whether the counts are float point or not; changes formatting
    
    if (length(options$counts) > 0) {
      
      counts <- dataset[[ .v(options$counts) ]]
      if (identical(counts, as.integer(counts)) == FALSE) 
        counts.fp <- TRUE
    }
    # Add columns to table
    .crossTabMainAddLayersColumns(crossTabChisq, analysis)
    if(options$chiSquared) {
      crossTabChisq$addColumnInfo(name = "type[chiSquared]",  title = "",      type = "string")
      crossTabChisq$addColumnInfo(name = "value[chiSquared]", title = "Value", type = "number", format = "sf:4;dp:3")
      crossTabChisq$addColumnInfo(name = "df[chiSquared]",    title = "df",    type = "integer")
      crossTabChisq$addColumnInfo(name = "p[chiSquared]",     title = "p",     type = "number", format = "dp:3;p:.001")
      if (options$VovkSellkeMPR)
        crossTabChisq$addColumnInfo(name = "MPR[chiSquared]", title = "VS-MPR", type = "number", format = "sf:4;dp:3")
    }
      
    if(options$chiSquaredContinuityCorrection) {
      crossTabChisq$addColumnInfo(name = "type[chiSquared-cc]", title = "", type = "string")
      crossTabChisq$addColumnInfo(name = "value[chiSquared-cc]", title = "Value", type = "number", format = "sf:4;dp:3")
      crossTabChisq$addColumnInfo(name = "df[chiSquared-cc]", title = "df", type = "integer")
      crossTabChisq$addColumnInfo(name = "p[chiSquared-cc]", title = "p", type = "number", format = "dp:3;p:.001")
      if (options$VovkSellkeMPR) 
        crossTabChisq$addColumnInfo(name = "MPR[chiSquared-cc]", title = "VS-MPR", type = "number", format = "sf:4;dp:3")
    }
       
    if(options$likelihoodRatio) {
      crossTabChisq$addColumnInfo(name = "type[likelihood]", title = "", type = "string")
      crossTabChisq$addColumnInfo(name = "value[likelihood]", title = "Value", type = "number", format = "sf:4;dp:3")
      crossTabChisq$addColumnInfo(name = "df[likelihood]", title = "df", type = "integer")
      crossTabChisq$addColumnInfo(name = "p[likelihood]", title = "p", type = "number", format = "dp:3;p:.001")
      if (options$VovkSellkeMPR)
        crossTabChisq$addColumnInfo(name = "MPR[likelihood]", title = "VS-MPR", type = "number", format = "sf:4;dp:3")
    }
    
    crossTabChisq$addColumnInfo(name = "type[N]", title = "", type = "string")
    
    if (counts.fp)
      crossTabChisq$addColumnInfo(name = "value[N]", title = "Value", type = "number", format = "sf:4;dp:2")
    else 
      crossTabChisq$addColumnInfo(name = "value[N]", title = "Value", type = "integer")
    
    crossTabChisq$addColumnInfo(name = "df[N]", title = "df")
    crossTabChisq$addColumnInfo(name = "p[N]", title = "p")
    if (options$VovkSellkeMPR)
      crossTabChisq$addColumnInfo(name = "MPR[N]", title = "VS-MPR", type = "number", format = "sf:4;dp:3")
    
    jaspResults[[paste0("tables", i)]][["crossTabChisq"]] <- crossTabChisq
    
    res <- try(.crosstabsCreateTestsRows(analysis$rows, analysis$group.matrices, 
                                         analysis$groups, i, options, ready))
    if(isTryError(res))
      crossTabChisq$setError(res)
    else {
      crossTabChisq$setData(res)
    }
  }
}

.crossTabLogOdds <- function(jaspResults, dataset, options, analyses, ready) {
  if(!options$oddsRatio)
    return()
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    if (!is.null(jaspResults[[paste0("tables", i)]][["crossTabLogOdds"]])) 
      return()
    
    # Create table
    crossTabLogOdds <- createJaspTable(title = "Log Odds Ratio")
    crossTabLogOdds$dependOn(c("oddsRatio"))
    crossTabLogOdds$showSpecifiedColumnsOnly <- TRUE
    
    ci.label <- paste(100*options$oddsRatioConfidenceIntervalInterval, 
                      "% Confidence Intervals", sep = "")
    
    # Add columns to table
    .crossTabMainAddLayersColumns(crossTabLogOdds, analysis)
    crossTabLogOdds$addColumnInfo(name = "type[oddsRatio]", title = "", type = "string")
    crossTabLogOdds$addColumnInfo(name = "value[oddsRatio]", title = "Log Odds Ratio", type = "number", format = "sf:4;dp:3")
    crossTabLogOdds$addColumnInfo(name = "low[oddsRatio]", title = "Lower",overtitle = ci.label, type = "number", format = "dp:3")
    crossTabLogOdds$addColumnInfo(name = "up[oddsRatio]",  title = "Upper",overtitle = ci.label, type = "number", format = "dp:3")
    
    crossTabLogOdds$addColumnInfo(name = "type[FisherTest]", title = "", type = "string")
    crossTabLogOdds$addColumnInfo(name = "value[FisherTest]", title = "Log Odds Ratio", type = "number", format = "sf:4;dp:3")
    crossTabLogOdds$addColumnInfo(name = "low[FisherTest]", title = "Lower", overtitle = ci.label, type = "number", format = "dp:3")
    crossTabLogOdds$addColumnInfo(name = "up[FisherTest]",  title = "Upper", overtitle = ci.label, type = "number", format = "dp:3")
    
    jaspResults[[paste0("tables", i)]][["crossTabLogOdds"]] <- crossTabLogOdds
    
    res <- try(.crosstabsCreateOddsRatioRows(analysis$rows, analysis$group.matrices, 
                                             analysis$groups, i, options, ready))
    if(isTryError(res))
      crossTabLogOdds$setError(res)
    else {
      crossTabLogOdds$setData(res)
    }
  }
}

.crossTabNominal <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$contingencyCoefficient && !options$phiAndCramersV && !options$lambda)
    return()
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    
    if (!is.null(jaspResults[[paste0("tables", i)]][["crossTabNominal"]])) 
      return()
    
    # Create table
    crossTabNominal <- createJaspTable(title = "Nominal")
    crossTabNominal$dependOn(c("contingencyCoefficient", "phiAndCramersV"))
    crossTabNominal$showSpecifiedColumnsOnly <- TRUE
    
    # Add columns to table
    .crossTabMainAddLayersColumns(crossTabNominal, analysis)
    if (options$contingencyCoefficient){
      crossTabNominal$addColumnInfo(name = "type[ContCoef]", title = "", type = "string")
      crossTabNominal$addColumnInfo(name = "value[ContCoef]", title = "Value", type = "number", format = "sf:4;dp:3")
    }
    
    if (options$phiAndCramersV) {
      crossTabNominal$addColumnInfo(name = "type[PhiCoef]", title = "", type = "string")
      crossTabNominal$addColumnInfo(name = "value[PhiCoef]", title = "Value", type = "number", format = "sf:4;dp:3")
      crossTabNominal$addColumnInfo(name = "type[CramerV]", title = "", type = "string")
      crossTabNominal$addColumnInfo(name = "value[CramerV]", title = "Value", type = "number", format = "sf:4;dp:3")
    }
    
    if (options$lambda) {
      crossTabNominal$addColumnInfo(name = "type[LambdaR]", title = "", type = "string")
      crossTabNominal$addColumnInfo(name = "value[LambdaR]", title = "Value", type = "number", format = "sf:4;dp:3")
      crossTabNominal$addColumnInfo(name = "type[LambdaC]", title = "", type = "string")
      crossTabNominal$addColumnInfo(name = "value[LambdaC]", title = "Value", type = "number", format = "sf:4;dp:3")
    }
    
    jaspResults[[paste0("tables", i)]][["crossTabNominal"]] <- crossTabNominal
    
    res <- try(.crosstabsCreateNominalRows(analysis$rows, analysis$group.matrices, 
                                           analysis$groups, i, options, ready))
    if(isTryError(res))
      crossTabNominal$setError(res)
    else {
      crossTabNominal$setData(res)
    }
  }
}

.crossTabGamma <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$gamma)
    return()
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    if (!is.null(jaspResults[[paste0("tables", i)]][["crossTabGamma"]])) 
      return()
    
    # Create table
    crossTabGamma <- createJaspTable(title = "Ordinal Gamma")
    crossTabGamma$dependOn(c("gamma"))
    crossTabGamma$showSpecifiedColumnsOnly <- TRUE
    
    ci.label <- paste("95% Confidence Intervals")
    
    # Add columns to table
    .crossTabMainAddLayersColumns(crossTabGamma, analysis)
    crossTabGamma$addColumnInfo(name = "value[gammaCoef]", title = "Gamma",          type = "number", format = "sf:4;dp:3")
    crossTabGamma$addColumnInfo(name = "Sigma[gammaCoef]", title = "Standard Error", type = "number", format = "dp:3")
    crossTabGamma$addColumnInfo(name = "low[gammaCoef]",   title = "Lower",  type = "number", format = "dp:3", overtitle = ci.label)
    crossTabGamma$addColumnInfo(name = "up[gammaCoef]",    title = "Upper",  type = "number", format = "dp:3", overtitle = ci.label)
  
    jaspResults[[paste0("tables", i)]][["crossTabGamma"]] <- crossTabGamma
    
    res <- try(.crosstabsCreateGammaRows(analysis$rows, analysis$group.matrices, 
                                         analysis$groups, i, options, ready))
    if(isTryError(res))
      crossTabGamma$setError(res)
    else {
      crossTabGamma$setData(res)
    }
  }
}

.crossTabKendallTau <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$kendallsTauB)
    return()
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    if (!is.null(jaspResults[[paste0("tables", i)]][["crossTabKendallTau"]])) 
      return()
    # Create table
    crossTabKendallTau <- createJaspTable(title = "Kendall's Tau")
    crossTabKendallTau$dependOn(c("kendallsTauB", "VovkSellkeMPR"))
    crossTabKendallTau$showSpecifiedColumnsOnly <- TRUE
    
    # Add columns to table
    .crossTabMainAddLayersColumns(crossTabKendallTau, analysis)
    crossTabKendallTau$addColumnInfo(name = "value[kTauB]", title = "Kendall's Tau-b ", type = "number", format = "sf:4;dp:3")
    crossTabKendallTau$addColumnInfo(name = "statistic[kTauB]", title = "Z", type = "number", format = "dp:3")
    crossTabKendallTau$addColumnInfo(name = "p[kTauB]", title = "p", type = "number", format = "dp:3;p:.001")
    if (options$VovkSellkeMPR)
      crossTabKendallTau$addColumnInfo(name = "MPR[kTauB]", title = "VS-MPR", type = "number", format = "sf:4;dp:3")

    jaspResults[[paste0("tables", i)]][["crossTabKendallTau"]] <- crossTabKendallTau
    
    res <- try(.crosstabsCreateKendallsTauRows(analysis$rows, analysis$group.matrices, 
                                               analysis$groups, i, options, ready))
    if(isTryError(res))
      crossTabKendallTau$setError(res)
    else 
      crossTabKendallTau$setData(res)
  }
}

# Other ----
.crossTabMainAddLayersColumns <- function(table, analysis) {
  if ((length(analysis)-2) >= 3)
    for (j in (length(analysis)-2):3)
      table$addColumnInfo(name = analysis[[j]], type = "string", combine = TRUE)
}

.crossTabMainOvertitle <- function(dataset, options, table, analysis) {
  
  lvls <- c()
  overTitle <- unlist(analysis$columns)
  counts.fp <- FALSE  # whether the counts are float point or not; changes formatting
  
  if (length(options$counts) > 0) {
    counts <- dataset[[ .v(options$counts) ]]
    if (identical(counts, as.integer(counts)) == FALSE)  
      counts.fp <- TRUE
  }
  if(analysis$columns == "") {
    lvls <- c("a", "b")
    for (column.name in lvls) {
      private.name <- base::paste(column.name,"[counts]", sep = "")
      
      if (counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal ) 
        table$addColumnInfo(name = private.name, title = ".", overtitle = ".", type = "number", format = "sf:4;dp:2")
      else 
        table$addColumnInfo(name = private.name, title = ".", overtitle = ".", type = "integer")
      
      if (options$countsExpected) { 
        private.name <- base::paste(column.name,"[expected]", sep = "")
        table$addColumnInfo(name = private.name, title = ".", type = "number", format = "sf:4;dp:2")
      }
      if (options$percentagesRow) {
        private.name <- base::paste(column.name,"[row.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = ".", type = "number", format = "dp:1;pc")
      }
      if (options$percentagesColumn) {
        private.name <- base::paste(column.name,"[col.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = ".", type = "number", format = "dp:1;pc")
      }
      if (options$percentagesTotal) {
        private.name <- base::paste(column.name,"[total.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = ".", type = "number", format = "dp:1;pc")
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
      
      if (counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal ) 
        table$addColumnInfo(name = private.name, title = column.name, overtitle = overTitle, type = "number", format = "sf:4;dp:2")
      else 
        table$addColumnInfo(name = private.name, title = column.name, overtitle = overTitle, type = "integer")
      
      if (options$countsExpected) { 
        private.name <- base::paste(column.name,"[expected]", sep = "")
        table$addColumnInfo(name = private.name, title = column.name, type = "number", format = "sf:4;dp:2")
      }
      if (options$percentagesRow) {
        private.name <- base::paste(column.name,"[row.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = column.name, type = "number", format = "dp:1;pc")
      }
      if (options$percentagesColumn) {
        private.name <- base::paste(column.name,"[col.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = column.name, type = "number", format = "dp:1;pc")
      }
      if (options$percentagesTotal) {
        private.name <- base::paste(column.name,"[total.proportions]", sep = "")
        table$addColumnInfo(name = private.name, title = column.name, type = "number", format = "dp:1;pc")
      }
    }
  }
}

.crosstabsCreateGroupMatrices <- function(dataset, rows, columns, groups, 
                                          counts = NULL,  
                                          rowOrderDescending=FALSE, 
                                          columnOrderDescending=FALSE, 
                                          ready) {
  
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
    
    if (rowOrderDescending) {
      ss.matrix <- base::apply(ss.matrix, 2, base::rev)
    } else {
      ss.matrix <- ss.matrix
    }
    
    if (columnOrderDescending) {
      ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
    } else {
      ss.matrix <- ss.matrix
    }
    
    ss.matrix[base::is.na(ss.matrix)] <- 0
    
    matrices[[1]] <- ss.matrix
  } else {
    
    for (group in groups) {
      
      group <- group[group != ""]
      
      if (!ready) {
        # do nothing
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
      
      if (rowOrderDescending) {
        ss.matrix <- base::apply(ss.matrix, 2, base::rev)
      } else {
        ss.matrix <- ss.matrix
      }
      
      if (columnOrderDescending) {
        ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
      } else {
        ss.matrix <- ss.matrix
      }
      matrices[[length(matrices)+1]] <- ss.matrix
    }
  }
  return(matrices)
}

.crosstabsCreateCountsRows <- function(var.name, group.matrices, 
                                       groups,i, options, ready) {
  if(!is.null(jaspResults[[paste0("tables", i)]][["resultsMain"]]))
    return()
  
  counts.rows     <- list()
  
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
  
    rows <- list()
    row.count <- list()
    row.expected <- list()
    row.row.proportions <- list()
    row.col.proportions <- list()
    row.total.proportions <- list()
    row.proportions <- list()
    row.count[["type[counts]"]] <- "Count"
    
    if (ready) {
      expected.matrix <- try({
        stats::chisq.test(counts.matrix, correct=FALSE)$expected
      })
      if (class(expected.matrix) == "try-error") {
        expected.matrix <- counts.matrix
        expected.matrix[,] <- "&nbsp;"
      }
      
      row.proportions.matrix <- try({
        base::prop.table(counts.matrix, 1)
      })
      if (class(row.proportions.matrix) == "try-error") {
        row.proportions.matrix <- counts.matrix
        row.proportions.matrix[,] <- "&nbsp;"
      }
      
      col.proportions.matrix <- try({
        base::prop.table(counts.matrix, 2)
      })
      if (class(col.proportions.matrix) == "try-error") {
        col.proportions.matrix    <- counts.matrix
        col.proportions.matrix[,] <- "&nbsp;"
      }
      
      proportions.matrix <- try({
        base::prop.table(counts.matrix, margin = NULL)
      })
      if (class(proportions.matrix) == "try-error") {
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
        row[["total[counts]"]] <- base::sum(counts.matrix[j,])
        row <- c(row.count, row)
        
        if (options$countsExpected) {
          
          row.expected[["type[expected]"]] <- "Expected count"
          
          expected <- as.list(expected.matrix[i,])
          names(expected) <- paste(names(expected),"[expected]",  sep = "")
          
          if (class(expected.matrix[1,1]) == "character") {
            expected[["total[expected]"]] <- ""
          } else {
            expected[["total[expected]"]] <- base::sum(expected.matrix[i,])
          }
          
          expected <- c(row.expected, expected)
          row <- c(row, expected)
        }
        #browser()
        if (options$percentagesRow) {
          row.proportions <- list()
          row.row.proportions[["type[row.proportions]"]] <- " % within row"
          
          row.proportions <- as.list(row.proportions.matrix[j,])
          row.proportions <- .clean(row.proportions)
          names(row.proportions) <- paste(names(row.proportions),
                                          "[row.proportions]",  sep = "")
          
          if (class(row.proportions.matrix[1,1]) == "character") {
            row.proportions[["total[row.proportions]"]] <- ""
          } else {
            row.proportions[["total[row.proportions]"]] <- .clean(base::sum(row.proportions.matrix[j,]))
          }
          
          row.proportions <- c(row.row.proportions, row.proportions)
          row <- c(row, row.proportions)
        }
        
        if (options$percentagesColumn) {
          
          row.col.proportions[["type[col.proportions]"]] <- " % within column"
          
          col.proportions <- as.list(col.proportions.matrix[j,])
          col.proportions <- .clean(col.proportions)
          names(col.proportions) <- paste(names(col.proportions),
                                          "[col.proportions]",  sep = "")
          
          if (class(col.proportions.matrix[1,1]) == "character") {
            col.proportions[["total[col.proportions]"]] <- ""
          } else {
            row.sum  <- base::margin.table(counts.matrix, 1)
            row.prop <- as.list( base::prop.table(row.sum))
            row.prop <- .clean(row.prop)
            col.proportions[["total[col.proportions]"]] <- row.prop[[j]]
          }
          
          col.proportions <- c(row.col.proportions, col.proportions)
          row <- c(row, col.proportions)
        }
        
        if (options$percentagesTotal) {
          total.proportions <- list()
          row.total.proportions[["type[total.proportions]"]] <- " % of total"
          
          total.proportions <- as.list(proportions.matrix[j,])
          total.proportions <- .clean(total.proportions)
          names(total.proportions) <- paste(names(total.proportions),
                                            "[total.proportions]",  sep="")
          
          if (class(proportions.matrix[1,1]) == "character")
            total.proportions[["total[total.proportions]"]] <- ""
          else 
            total.proportions[["total[total.proportions]"]] <- .clean(base::sum(proportions.matrix[j,]))
          
          total.proportions <- c(row.total.proportions, total.proportions)
          row <- c(row, total.proportions)
        }
        
      } else {
        
        row <- list()
      }
      
      row[[var.name]] <- dimnames(counts.matrix)[[1]][j]
      
      for (layer in names(group)) {
        
        level <- group[[layer]]
        
        if (level == "") 
          row[[layer]] <- "Total"
        else 
          row[[layer]] <- level
      }
      
      if (j == 1 && options$countsExpected == FALSE && 
          options$percentagesRow == FALSE && 
          options$percentagesCol == FALSE && 
          options$percentagesTotal == FALSE)
        row[[".isNewGroup"]] <- TRUE
      rows[[length(rows)+1]] <- row
    }
    
    if (ready) {
      
      row <- apply(counts.matrix, 2, base::sum)
      row <- as.list(row)
      names(row) <- base::paste(names(row),"[counts]",	sep="")
      row[["total[counts]"]] <- base::sum(counts.matrix)
      row <- c(row.count, row)
      
      if (options$countsExpected) {
        
        if (class(expected.matrix[1,1]) == "character") {
          expected <- expected.matrix[1,]
        } else {
          expected <- apply(expected.matrix, 2, base::sum)
        }
        
        expected <- as.list(expected)
        names(expected) <- paste(names(expected),"[expected]", sep="")
        
        if (class(expected.matrix[1,1]) == "character") {
          expected[["total[expected]"]] <- ""
        } else {
          expected[["total[expected]"]] <- base::sum(expected.matrix)
        }
        
        expected <- c(row.expected, expected)
        
        row <- c(row,  expected)
      }
      
      if (options$percentagesRow) {
        
        if (class(row.proportions.matrix[1,1]) == "character") {
          row.proportions <- row.proportions.matrix[1,]
        } else {
          m <- base::margin.table(counts.matrix, 2)
          rowproportion <- base::prop.table(m)
        }
        
        row.proportions <- as.list(rowproportion)
        row.proportions <- .clean(row.proportions)
        names(row.proportions) <- paste(names(row.proportions),
                                        "[row.proportions]", sep="")
        
        if (class(row.proportions.matrix[1,1]) == "character") {
          row.proportions[["total[row.proportions]"]] <- ""
        } else {
          row.proportions[["total[row.proportions]"]] <- .clean(base::sum(rowproportion))
        }
        
        row.proportions<-c(row.row.proportions, row.proportions)
        
        row <- c(row,  row.proportions)
      }
      
      if (options$percentagesColumn) {
        
        if (class(col.proportions.matrix[1,1]) == "character") {
          col.proportions <- col.proportions.matrix[1,]
        } else {
          colproportion <- apply(col.proportions.matrix, 2, base::sum)
        }
        
        col.proportions <- as.list(colproportion)
        col.proportions <- .clean(col.proportions)
        names(col.proportions) <- paste(names(col.proportions),"[col.proportions]", sep="")
        
        if (class(row.proportions.matrix[1,1]) == "character") {
          col.proportions[["total[col.proportions]"]] <- ""
        } else {
          row.sum <- base::margin.table(counts.matrix, 1)
          row.prop <- base::prop.table(row.sum)
          col.proportions[["total[col.proportions]"]] <- .clean(base::sum(row.prop))
        }
        
        col.proportions<-c(row.col.proportions, col.proportions)
        
        row <- c(row,  col.proportions)
      }
      
      if (options$percentagesTotal) {
        
        if (class(proportions.matrix[1,1]) == "character") {
          total.proportions <- proportions.matrix[1,]
        } else {
          total.proportions <- apply(proportions.matrix, 2, base::sum)
        }
        
        total.proportions <- as.list(total.proportions)
        total.proportions <- .clean(total.proportions)
        names(total.proportions) <- paste(names(total.proportions),"[total.proportions]", sep="")
        
        if (class(proportions.matrix[1,1]) == "character") {
          total.proportions[["total[total.proportions]"]] <- ""
        } else {
          total.proportions[["total[total.proportions]"]] <- .clean(base::sum(proportions.matrix))
        }
        
        total.proportions <- c(row.total.proportions, total.proportions)
        
        row <- c(row,  total.proportions)
      }
      
    } else {
      row <- list()
    }
    
    row[[var.name]] <- "Total"
    if (options$countsExpected == FALSE && options$percentagesRow == FALSE && 
        options$percentagesCol == FALSE && options$percentagesTotal == FALSE)
      row[[".isNewGroup"]] <- TRUE
    
    for (layer in names(group)) {
      
      level <- group[[layer]]
      
      if (level == "")
        row[[layer]] <- "Total"
      else 
        row[[layer]] <- level
    }
    rows[[length(rows)+1]] <- row
    counts.rows <- c(counts.rows, rows)
  }
  main <- createJaspState(counts.rows)
  main$dependOn(optionsFromObject = jaspResults[[paste0("tables", i)]][["crossTabMain"]])
  jaspResults[[paste0("tables", i)]][["resultsMain"]] <- main
  return(counts.rows)
}

.crosstabsCreateTestsRows <- function(var.name, group.matrices, groups, 
                                      i, options, ready) {
  if(!options$chiSquared && 
     !options$chiSquaredContinuityCorrection && 
     !options$likelihoodRatio) 
    return()
  if(!is.null(jaspResults[[paste0("tables", i)]][["resultsChisq"]]))
    return()
  
  tests.rows <- list()
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
  
    row <- list()
    
    for (layer in names(group)) {
      level <- group[[layer]]
      if (level == "")
        row[[layer]] <- "Total"
      else 
        row[[layer]] <- level
    }
    
    row[["type[N]"]] <- "N"
    row[["df[N]"]]   <- ""
    row[["p[N]"]]    <- ""
    row[["MPR[N]"]]  <- ""
    
    if (ready)
      row[["value[N]"]] <- base::sum(counts.matrix)
    else 
      row[["value[N]"]] <- "."
    
    if (options$chiSquared) {
      
      row[["type[chiSquared]"]] <- "\u03A7\u00B2"
      
      if (ready) {
        
        chi.result <- try({
          chi.result <- stats::chisq.test(counts.matrix, correct = FALSE)
        })
        
        if (class(chi.result) == "try-error") {
          
          row[["value[chiSquared]"]] <- .clean(NaN)
          row[["df[chiSquared]"]]    <- " "
          row[["p[chiSquared]"]]     <- " "
          row[["MPR[chiSquared]"]]   <- " "
        } else if (is.na(chi.result$statistic)) {
          
          row[["value[chiSquared]"]] <- .clean(NaN)
          row[["df[chiSquared]"]]    <- " "
          row[["p[chiSquared]"]]     <- " "
          row[["MPR[chiSquared]"]]   <- " "
          
        } else {
          
          row[["value[chiSquared]"]] <- unname(chi.result$statistic)
          row[["df[chiSquared]"]]    <- unname(chi.result$parameter)
          row[["p[chiSquared]"]]     <- unname(chi.result$p.value)
          row[["MPR[chiSquared]"]]   <- .VovkSellkeMPR(row[["p[chiSquared]"]])
        }
      } else {
        row[["value[chiSquared]"]] <- "."
      }
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
        
        if (class(chi.result) == "try-error") {
          
          row[["value[chiSquared-cc]"]] <- .clean(NaN)
          row[["df[chiSquared-cc]"]]    <- " "
          row[["p[chiSquared-cc]"]]     <- " "
          row[["MPR[chiSquared-cc]"]]   <- " "
          
        } else if (is.na(chi.result$statistic)) {
          
          row[["value[chiSquared-cc]"]] <- .clean(NaN)
          row[["df[chiSquared-cc]"]]    <- " "
          row[["p[chiSquared-cc]"]]     <- " "
          row[["MPR[chiSquared-cc]"]]   <- " "
          
        } else {
          
          row[["value[chiSquared-cc]"]] <- unname(chi.result$statistic)
          row[["df[chiSquared-cc]"]]    <- unname(chi.result$parameter)
          row[["p[chiSquared-cc]"]]     <- unname(chi.result$p.value)
          row[["MPR[chiSquared-cc]"]]   <- .VovkSellkeMPR(row[["p[chiSquared-cc]"]])
          
        }
        
      } else {
        
        row[["value[chiSquared-cc]"]] <- "."
      }
    }
    
    if (options$likelihoodRatio) {
      
      row[["type[likelihood]"]] <- "Likelihood ratio"
      
      if (ready) {
        
        chi.result <- try({
          chi.result <- vcd::assocstats(counts.matrix)
        })
        
        if (class(chi.result) == "try-error") {
          
          row[["value[likelihood]"]] <- .clean(NaN)
          row[["df[likelihood]"]]    <- ""
          row[["p[likelihood]"]]     <- ""
          row[["MPR[likelihood]"]]   <- ""
        } else {
          row[["value[likelihood]"]] <- chi.result$chisq_tests[1]
          row[["df[likelihood]"]]    <- chi.result$chisq_tests[3]
          row[["p[likelihood]"]]     <- chi.result$chisq_tests[5]
          row[["MPR[likelihood]"]]   <- .VovkSellkeMPR(row[["p[likelihood]"]])
        }
      } else {
        row[["value[likelihood]"]] <- "."
      }
    }
    tests.rows <- c(tests.rows, row)
  }
  chisq <- createJaspState(tests.rows)
  chisq$dependOn(optionsFromObject = jaspResults[[paste0("tables", i)]][["crossTabChisq"]])
  jaspResults[[paste0("tables", i)]][["resultsChisq"]] <- chisq
  return(tests.rows)
}

.crosstabsCreateOddsRatioRows <- function(var.name, group.matrices, 
                                          groups, i, options, ready) {
  if(!options$oddsRatio)
    return()
  if(!is.null(jaspResults[[paste0("tables", i)]][["resultsLogOdds"]]))
    return()
  odds.ratio.rows <- list()
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
  
    row <- list()
    
    for (layer in names(group)) {
      level <- group[[layer]]
      
      if (level == "")
        row[[layer]] <- "Total"
      else 
        row[[layer]] <- level
    }
    if (options$oddsRatio ) {
      row[["type[oddsRatio]"]] <- "Odds ratio"
      if (ready) {
        if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {
          row[["value[oddsRatio]"]] <- .clean(NaN)
          row[["low[oddsRatio]"]]   <- ""
          row[["up[oddsRatio]"]]    <-  ""
        } else {
          chi.result <- try({
            chi.result <- vcd::oddsratio(counts.matrix)
            level <- options$oddsRatioConfidenceIntervalInterval
            CI <- stats::confint(chi.result, level = level)
            LogOR <- unname(chi.result$coefficients)
            log.CI.low <- CI[1]
            log.CI.high <- CI[2]
          })
          
          if (class(chi.result) == "try-error") 
            row[["value[oddsRatio]"]] <- .clean(NaN)
          else if (is.na(chi.result))
            row[["value[oddsRatio]"]] <- .clean(NaN)
          else {
            row[["value[oddsRatio]"]] <-LogOR
            row[["low[oddsRatio]"]] <- log.CI.low
            row[["up[oddsRatio]"]] <- log.CI.high
          }
          row[["value[oddsRatio]"]] <- .clean(LogOR)
          row[["low[oddsRatio]"]] <- .clean(log.CI.low)
          row[["up[oddsRatio]"]] <-  .clean(log.CI.high)
        }
      }
    } else {
      row[["value[oddsRatio]"]] <- "."
    }
    
    if (options$oddsRatio ) {
      row[["type[FisherTest]"]] <- "Fisher's exact test "
      
      if (ready) {
        
        if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {
          row[["value[FisherTest]"]] <- .clean(NaN)
          row[["low[FisherTest]"]]   <- ""
          row[["up[FisherTest]"]]    <- ""
        } else {
          chi.result <- try({
            conf.level <- options$oddsRatioConfidenceIntervalInterval
            chi.result  <- stats::fisher.test(counts.matrix, 
                                              conf.level = conf.level)
            OR          <- unname(chi.result$estimate)
            logOR       <- log(OR)
            log.CI.low  <- log(chi.result$conf.int[1])
            log.CI.high <- log(chi.result$conf.int[2])
          })
          
          if (class(chi.result) == "try-error") 
            row[["value[FisherTest]"]] <- .clean(NaN)
          else if (is.na(chi.result)) 
            row[["value[FisherTest]"]] <- .clean(NaN)
          else {
            row[["value[FisherTest]"]] <- logOR
            row[["low[FisherTest]"]]   <- log.CI.low
            row[["up[FisherTest]"]]    <- log.CI.high
          }
          row[["value[FisherTest]"]] <- .clean(logOR)
          row[["low[FisherTest]"]]   <- .clean(log.CI.low)
          row[["up[FisherTest]"]]    <- .clean(log.CI.high)
        }
      }
      
    } else {
      row[["value[FisherTest]"]] <- "."
    }
   
    odds.ratio.rows <- c(odds.ratio.rows, row)
  }
  logOdds <- createJaspState(odds.ratio.rows)
  logOdds$dependOn(optionsFromObject = jaspResults[[paste0("tables", i)]][["crossTabLogOdds"]])
  jaspResults[[paste0("tables", i)]][["resultsLogOdds"]] <- logOdds
  return(odds.ratio.rows)
}

.crosstabsCreateNominalRows <- function(var.name, group.matrices, 
                                        groups, i, options, ready) {
  if (!options$contingencyCoefficient && !options$phiAndCramersV && !options$lambda)
    return()
  if(!is.null(jaspResults[[paste0("tables", i)]][["resultsNominal"]]))
    return()
  
  nominal.rows    <- list()
  
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) 
      group <- groups[[g]]
    else 
      group <- NULL
    row <- list()
    for (layer in names(group)) {
      level <- group[[layer]]
      if (level == "") 
        row[[layer]] <- "Total"
      else 
        row[[layer]] <- level
    }
    if (options$contingencyCoefficient) {
      row[["type[ContCoef]"]] <- "Contingency coefficient"
      if (ready) {
        chi.result <- try({
          chi.result <- vcd::assocstats(counts.matrix)
        })
        if (class(chi.result) == "try-error") 
          row[["value[ContCoef]"]] <- .clean(NaN)
        else if (is.na(chi.result$contingency))
          row[["value[ContCoef]"]] <- .clean(NaN)
        else
          row[["value[ContCoef]"]] <- chi.result$contingency
      } else
        row[["value[ContCoef]"]] <- "."
    }
    if (options$phiAndCramersV) {
      row[["type[PhiCoef]"]] <- "Phi-coefficient"
      if (ready) {
        chi.result <- try({
          chi.result <- vcd::assocstats(counts.matrix)
        })
        if (class(chi.result) == "try-error")
          row[["value[PhiCoef]"]] <- .clean(NaN)
        else if (is.na(chi.result$phi))
          row[["value[PhiCoef]"]] <- .clean(NaN)
        else
          row[["value[PhiCoef]"]] <- chi.result$phi
      } else 
        row[["value[PhiCoef]"]] <- "."
      row[["type[CramerV]"]] <- "Cramer's V "
      if (ready) {
        chi.result <- try({
          chi.result <- vcd::assocstats(counts.matrix)
        })
        if (class(chi.result) == "try-error")
          row[["value[CramerV]"]] <- .clean(NaN)
        else if (is.na(chi.result$cramer))
          row[["value[CramerV]"]] <- .clean(NaN)
        else 
          row[["value[CramerV]"]] <- chi.result$cramer
      } else
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
          row[["value[LambdaR]"]] <- .clean(NaN)
        else
          row[["value[LambdaR]"]] <- "."
        row[["type[LambdaC]"]] <- paste("Lambda (", options$columns, 
                                        "dependent)", sep =  " ")
        if (ready) {
          N      <- sum(counts.matrix)
          E1     <- N- max(colSums(counts.matrix))
          E2     <- sum(apply(counts.matrix,1, function (x) sum(x)-max(x) ))
          lambda <- (E1-E2)/E1
          row[["value[LambdaC]"]] <- lambda
        } else {
          row[["value[LambdaC]"]] <- "."
        }
      }
    }
    nominal.rows <- c(nominal.rows, row)
  }
  nominal <- createJaspState(nominal.rows)
  nominal$dependOn(optionsFromObject = jaspResults[[paste0("tables", i)]][["crossTabNominal"]])
  jaspResults[[paste0("tables", i)]][["resultsNominal"]] <- nominal
  return(nominal.rows)
}

.crosstabsCreateGammaRows <- function(var.name, group.matrices, 
                                      groups, i, options, ready) {
  if (!options$gamma)
    return()
  if(!is.null(jaspResults[[paste0("tables", i)]][["resultsGamma"]]))
    return()
  
  ordinal.rows <- list()
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) 
      group <- groups[[g]]
    else 
      group <- NULL
    row <- list()
    for (layer in names(group)) {
      level <- group[[layer]]
      if (level == "") 
        row[[layer]] <- "Total"
      else 
        row[[layer]] <- level
    }
  
    if (options$gamma) {
      
      row[["type[gammaCoef]"]] <- "Gamma coefficient"
      
      chi.result <- try({
        chi.result <- vcdExtra::GKgamma(counts.matrix)
      })
     if (class(chi.result) == "try-error") {
        row[["value[gammaCoef]"]] <- .clean(NaN)
      } else {
        row[["value[gammaCoef]"]] <- chi.result$gamma
        row[["Sigma[gammaCoef]"]] <- chi.result$sigma
        row[["low[gammaCoef]"]]   <- chi.result$CI[1]
        row[["up[gammaCoef]"]]    <- chi.result$CI[2]
      }
    }
    ordinal.rows <- c(ordinal.rows, row)
  }
  gamma <- createJaspState(ordinal.rows)
  gamma$dependOn(optionsFromObject = jaspResults[[paste0("tables", i)]][["crossTabGamma"]])
  jaspResults[[paste0("tables", i)]][["resultsGamma"]] <- gamma
  return(ordinal.rows)
}

.crosstabsCreateKendallsTauRows <- function(var.name, group.matrices, 
                                            groups,  i, options, ready) {
  if (!options$kendallsTauB)
    return()
  if(!is.null(jaspResults[[paste0("tables", i)]][["resultsKendallsTau"]]))
    return()
  kendalls.rows <- list()
    
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) 
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    for (layer in names(group)) {
      level <- group[[layer]]
      if (level == "")
        row[[layer]] <- "Total"
      else
        row[[layer]] <- level
    }
    if (options$kendallsTauB) {
      row[["type[kTauB]"]] <- "Kendall's Tau-b"
      if (ready) {
        chi.result <- try({
          count.dat  <- stats::ftable(counts.matrix)
          count.dat  <- as.data.frame(count.dat)
          Var1       <- rep(count.dat[,1],times=count.dat$Freq)
          Var2       <- rep(count.dat[,2],times=count.dat$Freq)
          chi.result <- stats::cor.test(as.numeric(Var1), 
                                        as.numeric(Var2), 
                                        method="kendall")
        })
        
        if (class(chi.result) == "try-error") {
          row[["value[kTauB]"]] <- .clean(NaN)
        } else {
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
    kendalls.rows <- c(kendalls.rows, row)
  }
  kendalls <- createJaspState(kendalls.rows)
  kendalls$dependOn(optionsFromObject = jaspResults[[paste0("tables", i)]][["crossTabKendallTau"]])
  jaspResults[[paste0("tables", i)]][["resultsKendallsTau"]] <- kendalls
  return(kendalls.rows)
}
