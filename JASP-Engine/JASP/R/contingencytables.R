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
             type = c('negativeValues', 'infinity', 'missingValues'),
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
      container$position <- 1
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
    crossTabMain <- createJaspTable(title = gettext("Contingency Tables"))
    crossTabMain$dependOn(c("countsExpected", "percentagesRow",  "percentagesColumn", "percentagesTotal", "rowOrder", "columnOrder"))
    crossTabMain$showSpecifiedColumnsOnly <- TRUE
    crossTabMain$position <- 1
      #
    .crossTabLayersColumns(crossTabMain, analysis)

    colTitleHere <- analysis$rows
    if(analysis$rows == "")  colTitleHere <- " "
    
    crossTabMain$addColumnInfo(name = analysis$rows, title = colTitleHere, type = "string", combine = TRUE)
    
    
    counts.fp <- .crossTabCountsFp(dataset, options)

    if (options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal )
                                    crossTabMain$addColumnInfo(name = "type[counts]",           title = "", type = "string")
    if (options$countsExpected)     crossTabMain$addColumnInfo(name = "type[expected]",         title = "", type = "string")
    if (options$percentagesRow)     crossTabMain$addColumnInfo(name = "type[row.proportions]",  title = "", type = "string")
    if (options$percentagesColumn)  crossTabMain$addColumnInfo(name = "type[col.proportions]",  title = "", type = "string")
    if (options$percentagesTotal)   crossTabMain$addColumnInfo(name = "type[proportions]",      title = "", type = "string")

    .crossTabMainOvertitle(dataset, options, crossTabMain, analysis, counts.fp)
    
    # Totals columns
    totalTitle <- gettext("Total")
    if (counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal) {
                                      crossTabMain$addColumnInfo(name = "total[counts]",          title = totalTitle, type = "number", format = "sf:4;dp:2")
      if (options$countsExpected)     crossTabMain$addColumnInfo(name = "total[expected]",        title = totalTitle, type = "number", format = "sf:4;dp:2")
      if (options$percentagesRow)     crossTabMain$addColumnInfo(name = "total[row.proportions]", title = totalTitle, type = "number", format = "dp:1;pc")
      if (options$percentagesColumn)  crossTabMain$addColumnInfo(name = "total[col.proportions]", title = totalTitle, type = "number", format = "dp:1;pc")
      if (options$percentagesTotal)   crossTabMain$addColumnInfo(name = "total[proportions]",     title = totalTitle, type = "number", format = "dp:1;pc")
    } else 
                                      crossTabMain$addColumnInfo(name = "total[counts]",          title = totalTitle, type = "integer")

    analysisContainer[["crossTabMain"]] <- crossTabMain
    analysis                            <- as.list(analysis)
    groupList                           <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                 <- try(.crossTabCountsRows(analysisContainer, analysis$rows, groupList, options, ready, counts.fp))
    
    .crossTabSetErrorOrFill(res, crossTabMain)
  }
}

#All the following .crossTabBlabla functions look suspiciously similar, they can probably be all merged to a very large extent... 
.crossTabChisq <- function(jaspResults, dataset, options, analyses, ready) {
  if(!(options$chiSquared || options$chiSquaredContinuityCorrection || options$likelihoodRatio))
    return()

  for (i in 1:nrow(analyses)){
    analysis          <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["crossTabChisq"]])) 
      next
    
    # Create table
    crossTabChisq <- createJaspTable(title = gettext("Chi-Squared Tests"))
    crossTabChisq$dependOn(c("chiSquared", "chiSquaredContinuityCorrection", "likelihoodRatio", "VovkSellkeMPR"))
    crossTabChisq$showSpecifiedColumnsOnly <- TRUE
    crossTabChisq$position <- 2
    
    counts.fp <- .crossTabCountsFp(dataset, options)
    
    # Add columns to table
    .crossTabLayersColumns(crossTabChisq, analysis)

    if (options$chiSquared)                       .crossTabChisqAddColInfo(fold = "chiSquared",     crossTabChisq, options)
    if (options$chiSquaredContinuityCorrection)   .crossTabChisqAddColInfo(fold = "chiSquared-cc",  crossTabChisq, options)
    if (options$likelihoodRatio)                  .crossTabChisqAddColInfo(fold = "likelihood",     crossTabChisq, options)
                                                  .crossTabChisqAddColInfo(fold = "N",              crossTabChisq, options, counts.fp)

    if(options$VovkSellkeMPR){
      message <- gettextf("Vovk-Sellke Maximum  <em>p</em>-Ratio: Based the <em>p</em>-value, 
      the maximum possible odds in favor of H%1$s over H%2$s equals %3$s
      (Sellke, Bayarri, & Berger, 2001).", "\u2081", "\u2080", "1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37")
      crossTabChisq$addFootnote(message, symbol = "\u002A")
    }
      
    analysisContainer[["crossTabChisq"]]  <- crossTabChisq
    analysis                              <- as.list(analysis)
    groupList                             <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                   <- try(.crossTabTestsRows(analysisContainer, groupList$rows, groupList, options, ready, counts.fp))
    
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
    crossTabLogOdds <- createJaspTable(title = gettext("Log Odds Ratio"))
    crossTabLogOdds$dependOn(c("oddsRatio", "oddsRatioConfidenceIntervalInterval", "oddsRatioHypothesis"))
    crossTabLogOdds$showSpecifiedColumnsOnly <- TRUE
    crossTabLogOdds$position <- 3
    
    ci.label <- gettextf("%.0f%% Confidence Intervals", 100*options$oddsRatioConfidenceIntervalInterval)
    
    # Add columns to table
    .crossTabLayersColumns(crossTabLogOdds, analysis)
    .crossTabLogOddsAddColInfo(crossTabLogOdds, fold = "oddsRatio",  ci.label)
    .crossTabLogOddsAddColInfo(crossTabLogOdds, fold = "FisherTest", ci.label)
    
    analysisContainer[["crossTabLogOdds"]] <- crossTabLogOdds
    analysis                               <- as.list(analysis)
    groupList                              <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    
    # Add note for one-sided tests
    .crossTabLogOddsNote(crossTabLogOdds, groupList, options, ready)
    
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
    crossTabNominal <- createJaspTable(title = gettext("Nominal"))
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

    analysisContainer[["crossTabNominal"]]  <- crossTabNominal
    analysis                                <- as.list(analysis)
    groupList                               <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                     <- try(.crossTabNominalRows(analysisContainer, analysis$rows, groupList, options, ready))
    
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
    crossTabGamma <- createJaspTable(title = gettext("Ordinal Gamma"))
    crossTabGamma$dependOn(c("gamma"))
    crossTabGamma$showSpecifiedColumnsOnly <- TRUE
    crossTabGamma$position <- 5
    
    ci.label <- gettextf("95%% Confidence Intervals")
    
    # Add columns to table
    .crossTabLayersColumns(crossTabGamma, analysis)
    crossTabGamma$addColumnInfo(name = "value[gammaCoef]", title = gettext("Gamma"),          type = "number")
    crossTabGamma$addColumnInfo(name = "Sigma[gammaCoef]", title = gettext("Standard Error"), type = "number", format = "dp:3")
    crossTabGamma$addColumnInfo(name = "low[gammaCoef]",   title = gettext("Lower"),          type = "number", format = "dp:3", overtitle = ci.label)
    crossTabGamma$addColumnInfo(name = "up[gammaCoef]",    title = gettext("Upper"),          type = "number", format = "dp:3", overtitle = ci.label)
  
    analysisContainer[["crossTabGamma"]]  <- crossTabGamma
    analysis                              <- as.list(analysis)
    groupList                             <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                   <- try(.crossTabGammaRows(analysisContainer, analysis$rows, groupList, options, ready))
    
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
                                crossTabKendallTau$addColumnInfo(name = "value[kTauB]",     title = gettext("Kendall's Tau-b "),    type = "number")
                                crossTabKendallTau$addColumnInfo(name = "statistic[kTauB]", title = gettext("Z"),                   type = "number", format = "dp:3")
                                crossTabKendallTau$addColumnInfo(name = "p[kTauB]",         title = gettext("p"),                   type = "pvalue")
    if (options$VovkSellkeMPR)  crossTabKendallTau$addColumnInfo(name = "MPR[kTauB]",       title = gettextf("VS-MPR%s", "\u002A"), type = "number")

    analysisContainer[["crossTabKendallTau"]] <- crossTabKendallTau
    analysis                                  <- as.list(analysis)
    groupList                                 <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                       <- try(.crossTabKendallsTauRows(analysisContainer, analysis$rows, groupList, options, ready))
    
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
  
  lvls                 <- c("a", "b")
  useColumnNameAsTitle <- FALSE
  overTitle            <- "."


  if(analysis$columns != "") {
    if (is.factor(dataset[[ .v(analysis$columns) ]] )) 
          lvls <- levels(dataset[[ .v(analysis$columns) ]])
    else  lvls <- unique(dataset[[ .v(analysis$columns) ]])
      
    if (options$columnOrder == "descending")
        lvls <- sort(lvls, decreasing = TRUE)

    overTitle            <- unlist(analysis$columns)
    useColumnNameAsTitle <- TRUE
  }

  for (column.name in lvls) {

    if(useColumnNameAsTitle) myTitle <- column.name
    else                     myTitle <- "."

    pr.format <- NULL
    pr.type   <- "integer"
    if (counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal )
    {
      pr.format <- "sf:4;dp:2"
      pr.type   <- "number"
    }

                                   table$addColumnInfo(name = paste0(column.name,"[counts]"),          title = myTitle, type = pr.type,  format = pr.format, overtitle = overTitle)
    if (options$countsExpected)    table$addColumnInfo(name = paste0(column.name,"[expected]"),        title = myTitle, type = "number", format = "sf:4;dp:2")
    if (options$percentagesRow)    table$addColumnInfo(name = paste0(column.name,"[row.proportions]"), title = myTitle, type = "number", format = "dp:1;pc")
    if (options$percentagesColumn) table$addColumnInfo(name = paste0(column.name,"[col.proportions]"), title = myTitle, type = "number", format = "dp:1;pc")
    if (options$percentagesTotal)  table$addColumnInfo(name = paste0(column.name,"[proportions]"),     title = myTitle, type = "number", format = "dp:1;pc")
  }
}

.crossTabSetErrorOrFill <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
  else
    for (level in 1:length(res$rows)) 
      table$addRows(res$rows[[level]], rowNames = res$rownames)
}

.crossTabChisqAddColInfo <- function(fold, table, options, counts.fp = FALSE) {
  if(fold == "N" && counts.fp == FALSE) valueFoldType <- "integer"
  else                                  valueFoldType <- "number"
  
                              table$addColumnInfo(name = paste0("type[",  fold, "]"),   title = "",                             type = "string")
                              table$addColumnInfo(name = paste0("value[", fold, "]"),   title = gettext("Value"),               type = valueFoldType)
                              table$addColumnInfo(name = paste0("df[",    fold, "]"),   title = gettext("df"),                  type = "integer")
                              table$addColumnInfo(name = paste0("p[",     fold, "]"),   title = gettext("p"),                   type = "pvalue")
  if (options$VovkSellkeMPR)  table$addColumnInfo(name = paste0("MPR[",   fold, "]"),   title = gettextf("VS-MPR%s", "\u002A"), type = "number")
}

.crossTabLogOddsAddColInfo <- function(table, fold, ci.label) {
  table$addColumnInfo(name = paste0("type[",  fold, "]"), title = "",                        type = "string")
  table$addColumnInfo(name = paste0("value[", fold, "]"), title = gettext("Log Odds Ratio"), type = "number")
  table$addColumnInfo(name = paste0("low[",   fold, "]"), title = gettext("Lower"),          type = "number", overtitle = ci.label, format = "dp:3")
  table$addColumnInfo(name = paste0("up[",    fold, "]"), title = gettext("Upper"),          type = "number", overtitle = ci.label, format = "dp:3")
  table$addColumnInfo(name = paste0("p[",     fold, "]"), title = gettext("p"),              type = "pvalue")

}

.crossTabNominalAddColInfo <- function(table, fold){
  table$addColumnInfo(name = paste0("type[",  fold, "]"),  title = "",                type = "string")
  table$addColumnInfo(name = paste0("value[", fold, "]"),  title = gettext("Value"),  type = "number")
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
                                      options$rowOrder   =="descending", 
                                      options$columnOrder=="descending", 
                                      ready)
  groupsList$group.matrices <- grp.mat
  analysisContainer[["groupList"]] <- createJaspState(groupsList)
  analysisContainer[["groupList"]]$dependOn(c("rowOrder", "columnOrder"))
  return(groupsList)
}

.crossTabCountsMatrixToRow <- function(matrix, counts.matrix, type) {
  if (is.character(matrix[1,1]))
    return(NULL)
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

.crossTabNominalMatrixToRow <- function(analysisContainer, row, counts.matrix, type, ready, rowname) {
  
  switch(type,
  ContCoef= { 
    row[["type[ContCoef]"]] <- gettext("Contingency coefficient")
    val                     <- "contingency"
  },
  PhiCoef= { 
    row[["type[PhiCoef]"]]  <- gettext("Phi-coefficient")
    val                     <- "phi"
  },
  CramerV= {
    row[["type[CramerV]"]]  <- gettext("Cramer's V ")
    val                     <- "cramer"
  })

  if (ready) {
    chi.result <- try(vcd::assocstats(counts.matrix))

    if (isTryError(chi.result)) 
      row[[paste0("value[", type, "]")]] <- NaN
    else if (is.na(chi.result[[val]])) {
      row[[paste0("value[", type, "]")]] <- NaN
      message                            <- gettext("Value could not be calculated - At least one row or column contains all zeros")
      
      analysisContainer[["crossTabNominal"]]$addFootnote(message, rowNames = rowname, colNames = paste0("value[", type, "]"))
    } else
      row[[paste0("value[", type, "]")]] <- chi.result[[val]]
  } 
  else
    row[[paste0("value[", type, "]")]] <- "."

  return(row)
}

.crossTabCountsFp <- function(dataset, options) {
  # check if the counts column has floating point numbers
  if (options$counts != "") {
    counts <- dataset[[ .v(options$counts) ]]
    return(!all((counts %% 1) == 0))
  }
  return(FALSE)
}

.crossTabLogOddsNote <- function(crossTabLogOdds, groupList, options, ready){
  if(ready){
    if(length(groupList$group.matrices) >= 1  & options$oddsRatioHypothesis != "two.sided"){

      gp1 <- dimnames(groupList$group.matrices[[1]])[[1]][1]
      gp2 <- dimnames(groupList$group.matrices[[1]])[[1]][2]

      if(options$oddsRatioHypothesis == "less") lessIsMore <- gettext("is less than")
      else                                      lessIsMore <- gettext("is greater than")

      message <- gettextf("For all tests, the alternative hypothesis specifies that group <em>%1$s</em> %2$s <em>%3$s</em>.", gp1, lessIsMore, gp2)
      
      crossTabLogOdds$addFootnote(message)
    }
  }
}

.crossTabRowName <- function(groups) {
  rowNames <- lapply(groups, function(x) paste(c("row", unlist(x)), collapse="-"))
  return(unlist(rowNames))
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
  counts.rows    <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)
  
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
        expected.matrix[,] <- ""
      }
      
      row.proportions.matrix <- try({
        prop.table(counts.matrix, 1)
      })
      if (isTryError(row.proportions.matrix)) {
        row.proportions.matrix    <- counts.matrix
        row.proportions.matrix[,] <- ""
      }
      
      col.proportions.matrix <- try({
        prop.table(counts.matrix, 2)
      })
      if (isTryError(col.proportions.matrix)) {
        col.proportions.matrix    <- counts.matrix
        col.proportions.matrix[,] <- ""
      }
      
      proportions.matrix <- try({
        prop.table(counts.matrix, margin = NULL)
      })
      if (isTryError(proportions.matrix)) {
        proportions.matrix    <- counts.matrix
        proportions.matrix[,] <- ""
      }
      
    } else {
      expected.matrix <- counts.matrix
      row.proportions.matrix <- counts.matrix
      col.proportions.matrix <- counts.matrix
      proportions.matrix <- counts.matrix
    }
    
    for (j in 1:dim(counts.matrix)[[1]]) {
      
      if (ready) {
        
        row        <- as.list(counts.matrix[j,])
        names(row) <- paste0(names(row),"[counts]")
        sum        <- sum(counts.matrix[j,])
        
        if(counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal)
              row[["total[counts]"]] <- sum
        else  row[["total[counts]"]] <- as.integer(sum)

        row          <- c(row.count, row)
        
        if (options$countsExpected) {
          
          row.expected[["type[expected]"]] <- gettext("Expected count")          
          expected                         <- as.list(expected.matrix[j,])
          names(expected)                  <- paste0(names(expected),"[expected]")
          
          if (is.character(expected.matrix[1,1])) expected[["total[expected]"]] <- ""
          else                                    expected[["total[expected]"]] <- base::sum(expected.matrix[j,])

          expected      <- c(row.expected, expected)
          row           <- c(row, expected)
        }
        
        if (options$percentagesRow) {
          row.row.proportions[["type[row.proportions]"]]  <- gettextf(" %% within row") #This is possibly combined somewhere else into a full sentence and might need to be placed in a gettextf there. But I suspect this will do for now
          row.proportions                                 <- as.list(row.proportions.matrix[j,])
          names(row.proportions)                          <- paste0(names(row.proportions), "[row.proportions]")
          
          if (is.character(row.proportions.matrix[1,1])) row.prop <- ""
          else                                           row.prop <- sum(row.proportions.matrix[j,])

          row.proportions[["total[row.proportions]"]] <- row.prop
          
          row.proportions <- c(row.row.proportions, row.proportions)
          row             <- c(row, row.proportions)
        }
        
        if (options$percentagesColumn) {
          row.col.proportions[["type[col.proportions]"]] <- gettextf(" %% within column")
          
          col.proportions        <- as.list(col.proportions.matrix[j,])
          names(col.proportions) <- paste0(names(col.proportions), "[col.proportions]")
          
          if (is.character(col.proportions.matrix[1,1]))
            col.proportions[["total[col.proportions]"]] <- ""
          else {
            row.sum  <- margin.table(counts.matrix, 1)
            row.prop <- as.list(prop.table(row.sum))
            col.proportions[["total[col.proportions]"]] <- row.prop[[j]]
          }
          
          col.proportions <- c(row.col.proportions, col.proportions)
          row             <- c(row, col.proportions)
        }
        
        if (options$percentagesTotal) {
          row.total.proportions[["type[proportions]"]]  <- gettextf(" %% of total")
          total.proportions                             <- as.list(proportions.matrix[j,])
          names(total.proportions)                      <- paste0(names(total.proportions), "[proportions]")
          
          if (is.character(proportions.matrix[1,1]))   tot.prop <- ""
          else                                         tot.prop <- sum(proportions.matrix[j,])
          total.proportions[["total[proportions]"]] <- tot.prop
          
          total.proportions <- c(row.total.proportions, total.proportions)
          row               <- c(row, total.proportions)
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
      
      row         <- as.list(colSums(counts.matrix))
      names(row)  <- paste0(names(row),"[counts]")
      sum         <- sum(counts.matrix)

      if(counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn || options$percentagesTotal)
            row[["total[counts]"]] <- sum
      else  row[["total[counts]"]] <- as.integer(sum)
      
      row <- c(row.count, row)
      
      if (options$countsExpected) {
        expected  <- .crossTabCountsMatrixToRow(expected.matrix, counts.matrix, type = "expected")
        expected  <- c(row.expected, expected)
        row       <- c(row,  expected)
      }
      
      if (options$percentagesRow) {
        row.proportions <- .crossTabCountsMatrixToRow(row.proportions.matrix, counts.matrix, type = "row.proportions")
        row.proportions <- c(row.row.proportions, row.proportions)
        row             <- c(row,  row.proportions)
      }
      
      if (options$percentagesColumn) {
        col.proportions <- .crossTabCountsMatrixToRow(col.proportions.matrix, counts.matrix, type = "col.proportions")
        col.proportions <- c(row.col.proportions, col.proportions)
        row             <- c(row,  col.proportions)
      }
      
      if (options$percentagesTotal) {
        total.proportions <- .crossTabCountsMatrixToRow(proportions.matrix, counts.matrix, type = "proportions")
        total.proportions <- c(row.total.proportions, total.proportions)
        row               <- c(row,  total.proportions)
      }
      
    } else 
      row <- list()
      
    if(var.name != "")
      row[[var.name]] <- gettext("Total")

    if (!(options$countsExpected || options$percentagesRow || options$percentagesCol || options$percentagesTotal))
      row[[".isNewGroup"]] <- TRUE
    
    row                       <- .crossTabLayerNames(row, group)
    rows[[length(rows) + 1]]  <- row
    counts.rows               <- c(counts.rows, rows)
  }

  main                                <- createJaspState(counts.rows)
  table                               <- analysisContainer[["crossTabMain"]]
  analysisContainer[["resultsMain"]]  <- main
  
  main$dependOn(optionsFromObject = table)

  return(list(rows = counts.rows, rownames = row.rownames))
}

.crossTabTestsRows <- function(analysisContainer, var.name, groupList, options, ready, counts.fp) {
  tests.rows     <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)

  for (g in seq_along(group.matrices)) {
    
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) group <- groups[[g]]
    else                  group <- NULL

    row <- list()
    row[["type[N]"]] <- gettext("N")
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

          message <- gettextf("%s could not be calculated - At least one row or column contains all zeros", "\u03A7\u00B2")
          analysisContainer[["crossTabChisq"]]$addFootnote(message, rowNames = row.rownames[g], colNames = "value[chiSquared]")
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
      
      row[["type[chiSquared-cc]"]] <- gettextf("%s continuity correction", "\u03A7\u00B2")
      
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

          message <- gettextf("%s could not be calculated - At least one row or column contains all zeros", "\u03A7\u00B2")
          analysisContainer[["crossTabChisq"]]$addFootnote(message, rowNames = row.rownames[g], colNames = "value[chiSquared-cc]")
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
      
      row[["type[likelihood]"]] <- gettext("Likelihood ratio")
      
      if (ready) {
        
        chi.result <- try(vcd::assocstats(counts.matrix))
        
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
  return(list(rows = tests.rows, rownames = row.rownames))
}

.crossTabOddsRatioRows <- function(analysisContainer, var.name, groupList, options, ready) {
  odds.ratio.rows <- list()
  group.matrices  <- groupList$group.matrices
  groups          <- groupList$groups
  row.rownames    <- .crossTabRowName(groups)
  
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
        row[["p[oddsRatio]"]]     <- ""
        stop(gettext("Odds ratio restricted to 2 x 2 tables"))
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
          row[["p[oddsRatio]"]]     <- ""
        }
        row[["value[oddsRatio]"]] <- LogOR
        row[["low[oddsRatio]"]]   <- log.CI.low
        row[["up[oddsRatio]"]]    <- log.CI.high
        row[["p[oddsRatio]"]]     <- ""
      }
    }

    row[["type[FisherTest]"]] <- gettext("Fisher's exact test ")
    if (ready) {
      
      if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {
        row[["value[FisherTest]"]] <- NaN
        row[["low[FisherTest]"]]   <- ""
        row[["up[FisherTest]"]]    <- ""
        row[["p[FisherTest]"]]     <- ""
      } else {
        chi.result <- try({
          conf.level  <- options$oddsRatioConfidenceIntervalInterval
          chi.result  <- stats::fisher.test(counts.matrix, conf.level = conf.level,
                                            alternative = options$oddsRatioHypothesis)
          OR          <- unname(chi.result$estimate)
          logOR       <- log(OR)
          log.CI.low  <- log(chi.result$conf.int[1])
          log.CI.high <- log(chi.result$conf.int[2])
          p           <- chi.result$p.value
        })
        
        if (isTryError(chi.result)) 
          row[["value[FisherTest]"]] <- NaN
        else if (is.na(chi.result)) 
          row[["value[FisherTest]"]] <- NaN
        else {
          row[["value[FisherTest]"]] <- logOR
          row[["low[FisherTest]"]]   <- log.CI.low
          row[["up[FisherTest]"]]    <- log.CI.high
          row[["p[FisherTest]"]]     <- p
        }
        row[["value[FisherTest]"]] <- logOR
        row[["low[FisherTest]"]]   <- log.CI.low
        row[["up[FisherTest]"]]    <- log.CI.high
        row[["p[FisherTest]"]]     <- p
      }
    }
    
    row <- .crossTabLayerNames(row, group)
    odds.ratio.rows[[length(odds.ratio.rows) + 1]] <- row
  }
  return(list(rows = odds.ratio.rows, rownames = row.rownames))
}

.crossTabNominalRows <- function(analysisContainer, var.name, groupList, options, ready) {
  nominal.rows   <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)
  
  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    
    if (!is.null(groups)) group <- groups[[g]]
    else                  group <- NULL
    
    row <- list()

    if (options$contingencyCoefficient) {
      row          <- .crossTabNominalMatrixToRow(analysisContainer, row, counts.matrix, type = "ContCoef", ready, row.rownames[g])
    }

    if (options$phiAndCramersV) {
      row          <- .crossTabNominalMatrixToRow(analysisContainer, row, counts.matrix, type = "PhiCoef", ready, row.rownames[g])
      row          <- .crossTabNominalMatrixToRow(analysisContainer, row, counts.matrix, type = "CramerV", ready, row.rownames[g])
    }

    if (options$lambda) {
      row[["type[LambdaR]"]] <- gettextf("Lambda ( %i dependent)", options$rows)
      row[["type[LambdaC]"]] <- gettextf("Lambda ( %i dependent)", options$columns)
      
      if (ready) { #The following looks like it could be a function instead of ctrl+c/+v...
        N                    <- sum(counts.matrix)

        E1R                  <- N - max(rowSums(counts.matrix))
        E2R                  <- sum(apply(counts.matrix,2, function (x) sum(x) - max(x) ))
        lambdaR              <- (E1 - E2)/E1
        if(is.na(lambdaR)) 
          lambdaR            <- NaN
        
        E1C                  <- N - max(colSums(counts.matrix))
        E2C                  <- sum(apply(counts.matrix, 1, function (x) sum(x) - max(x) ))
        lambdaC              <- (E1 - E2)/E1
        if (is.na(lambdaC))
          lambdaC            <- NaN

        row[["value[LambdaR]"]] <- lambdaR
        row[["value[LambdaC]"]] <- lambdaC
      } else {
        row[["value[LambdaR]"]] <- "."
        row[["value[LambdaC]"]] <- "."
      }
    }
    
    nominal.rows[[length(nominal.rows) + 1]] <- .crossTabLayerNames(row, group)
  }
  return(list(rows = nominal.rows, rownames = row.rownames))
}

.crossTabGammaRows <- function(analysisContainer, var.name, groupList, options, ready) {
  ordinal.rows   <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)
  
  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))  group <- groups[[g]]
    else                   group <- NULL
    
    row <- list()
    if(ready) {
      chi.result <- try({ chi.result <- vcdExtra::GKgamma(counts.matrix) }) # in for a penny in for a dime I guess
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
    
    ordinal.rows[[length(ordinal.rows) + 1]] <- .crossTabLayerNames(row, group)
  }
  return(list(rows = ordinal.rows, rownames = row.rownames))
}

.crossTabKendallsTauRows <- function(analysisContainer, var.name, groupList, options, ready) {
  kendalls.rows  <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)

  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    
    if (!is.null(groups)) group <- groups[[g]]
    else                  group <- NULL
    
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
                                    row[["value[kTauB]"]]     <- unname(chi.result$estimate)
                                    row[["p[kTauB]"]]         <- chi.result$p.value
        if (options$VovkSellkeMPR)  row[["MPR[kTauB]"]]       <- .VovkSellkeMPR(row[["p[kTauB]"]])
                                    row[["statistic[kTauB]"]] <- unname(chi.result$statistic)
      }
    } else {
                                    row[["value[kTauB]"]]     <- "."
                                    row[["p[kTauB]"]]         <- "."
      if (options$VovkSellkeMPR)    row[["MPR[kTauB]"]]       <- "."
                                    row[["statistic[kTauB]"]] <- "."
    }
    
    kendalls.rows[[length(kendalls.rows) + 1]] <- .crossTabLayerNames(row, group)
  }
  return(list(rows = kendalls.rows, rownames = row.rownames))
}
