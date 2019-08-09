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

#' The plan of this file is to successively abstract functionality
#' from the individual t-tests into a common interface to reduce clutter
#'
.ttestReadData <- function(dataset, options, type) {
  if (!is.null(dataset))
    return(dataset)
  else {
    groups  <- options$groupingVariable
    if (!is.null(groups) && groups == "") 
      groups <- NULL
    if(type %in% c("one-sample", "independent"))
      depvars <- unlist(options$variables)
    else if (type == 'paired') {
      depvars <- unlist(options$pairs)
      depvars <- depvars[depvars != ""]
    }
    exclude <- NULL
    if (options$missingValues == "excludeListwise")
      exclude <- depvars
    return(.readDataSetToEnd(columns.as.numeric  = depvars,
                             columns.as.factor   = groups,
                             exclude.na.listwise = exclude))
  }
}

.ttestCheckErrors <- function(dataset, options, type) {
  if(type == "paired") {
    for (i in 1:length(options$pairs)) {
      p1 <- options$pairs[[i]][[1]]
      p2 <- options$pairs[[i]][[2]]
      datasetErrorCheck <- data.frame(dataset[[.v(p1)]] - dataset[[.v(p2)]])
      colnames(datasetErrorCheck) <- .v(paste0("Difference between ", p1, " and ", p2))
      .hasErrors(datasetErrorCheck, 
                 type = "variance", 
                 exitAnalysisIfErrors = TRUE)
      .hasErrors(dataset,
                 type = c('observations', 'variance', 'infinity'),
                 all.target = c(p1, p2), 
                 observations.amount  = c('< 3', '> 5000'), 
                 exitAnalysisIfErrors = TRUE)
    }
  }
  else if(type == "one-sample") {
    for (variable in options$variables)
      .hasErrors(dataset, 
                 type = c('observations', 'variance', 'infinity'),
                 all.target = variable,
                 observations.amount  = c('< 3', '> 5000'),
                 exitAnalysisIfErrors = TRUE)
  }
  else if(type == "independent") {
    if (length(options$variables) != 0 && options$groupingVariable != '') {
      .hasErrors(dataset,
                 type = 'factorLevels',
                 factorLevels.target  = options$groupingVariable, 
                 factorLevels.amount  = '!= 2',
                 exitAnalysisIfErrors = TRUE)
    }
    variables <- options$variables
    factor <- options$groupingVariable
    levels <- levels(dataset[[.v(factor)]])
    
    if (length(variables) == 0) variables = "."
    if (length(levels) == 0) levels = c(".", ".")
    
    for (variable in variables) {
      for (level in levels) {
        .hasErrors(dataset, 
                   type = c('observations', 'variance', 'infinity'),
                   all.target = variable,
                   observations.amount = c('< 3', '> 5000'),
                   all.grouping = factor,
                   all.groupingLevel = level,
                   exitAnalysisIfErrors = TRUE)
      }
    }
  }
}

.ttestAssumptionCheckContainer <- function(jaspResults, options, type) {
  if(!options$normalityTests)
    return()
  if(type == "independent")
    if(!options$equalityOfVariancesTests)
      return()
  if (is.null(jaspResults[["AssumptionChecks"]])) {
    container <- createJaspContainer("Assumption Checks")
    dependList <- c("variables", "groupingVariable", "pairs",
                    "normalityTests", "equalityOfVariancesTests")
    container$dependOn(dependList)
    jaspResults[["AssumptionChecks"]] <- container
  }
}

.ttestNormalTable <- function(jaspResults, dataset, options, ready, type) {
  # Container
  .ttestAssumptionCheckContainer(jaspResults, options, type)
  container <- jaspResults[["AssumptionChecks"]]
  if (!options$normalityTests || !is.null(container[["ttestNormalTable"]])) 
    return()
  container <- jaspResults[["AssumptionChecks"]]
  # Create table
  ttestNormalTable <- createJaspTable(title = "Test of Normality (Shapiro-Wilk)")
  #dependList <- c()
  #ttestMainTable$dependOn(dependList)
  ttestNormalTable$showSpecifiedColumnsOnly <- TRUE
  
  if(type == "paired") {
    ttestNormalTable$addColumnInfo(name = "v1",  type = "string",    title = "")
    #ttestNormalTable$addColumnInfo(name = "sep", type = "separator", title = "")
    ttestNormalTable$addColumnInfo(name = "v2",  type = "string",    title = "")
  } else if(type == "independent") {
    ttestNormalTable$addColumnInfo(name = "dep", type = "string", title = "", combine = TRUE)
    ttestNormalTable$addColumnInfo(name = "lev", type = "string", title = "")
  } else if(type == "one-sample")
    ttestNormalTable$addColumnInfo(name = "v", title = "", type = "string")
  ttestNormalTable$addColumnInfo(name = "W",   type = "number", title = "W")
  ttestNormalTable$addColumnInfo(name = "p",   type = "pvalue", title = "p")
  
  message <- "Significant results suggest a deviation from normality."
  ttestNormalTable$addFootnote(message)
  
  container[["ttestNormalTable"]] <- ttestNormalTable
  
  res <- try(.ttestNormalFill(container, dataset, options, ready, type))
  .ttestSetError(res, ttestNormalTable)
}

.ttestNormalFill <- function(container, dataset, options, ready, type) {
  if(type == "paired") {
    pairs <- options$pairs
    rowNo <- 1
    if (!ready)
      pairs[[1]] <- list(".", ".")
    for (pair in pairs) {
      p1 <- pair[[1]]
      p2 <- pair[[2]]
      
      if (ready && p1 != p2) {
        c1   <- dataset[[ .v(p1) ]]
        c2   <- dataset[[ .v(p2) ]]
        data <- na.omit(c1 - c2)
        
        r <- stats::shapiro.test(data)
        W <- as.numeric(r$statistic)
        p <- r$p.value
        row <- list(v1 = p1, sep = "-", v2 = p2, W = W,    p = p)
      } else 
        row <- list(v1 = p1, sep = "-", v2 = p2, W = ".",  p = ".")
      row[[".isNewGroup"]] <- rowNo == 1
      container[["ttestNormalTable"]]$addRows(row)
      rowNo <- rowNo + 1
    }
  } 
  else if(type == "independent") {
    ## for a independent t-test, we need to check both group vectors for normality
    variables <- options$variables
    factor    <- options$groupingVariable
    levels    <- levels(dataset[[.v(factor)]])
    
    if (length(variables) == 0) 
      variables = "."
    if (length(levels) == 0) 
      levels = c(".", ".")
    
    for (variable in variables) {
      ## there will be two levels, otherwise .hasErrors will quit
      for (level in levels) {
        if (ready) {
          ## get the dependent variable at a certain factor level
          data <- na.omit(dataset[[.v(variable)]][dataset[[.v(factor)]] == level])
          r    <- stats::shapiro.test(data)
          W    <- as.numeric(r$statistic)
          p    <- r$p.value
            
          result <- list(dep = variable, lev = level, W = W, p = p)
        } else 
          result <- list(dep = variable, lev = level, W = ".", p = ".")
        result[[".isNewGroup"]] <- level == levels[1]
        container[["ttestNormalTable"]]$addRows(result)
      }
    }
  }
  else if(type == "one-sample") {
    variables <- options$variables
    rowNo <- 1
    if (!ready) 
      variables = "."
    for (variable in variables) {
      
      if (ready) {
        
        data <- na.omit(dataset[[.v(variable)]])
        
        r <- stats::shapiro.test(data)
        W <- as.numeric(r$statistic)
        p <- r$p.value
        
        row <- list(v = variable, W = W, p = p)
        
      } else 
        row <- list(v = variable, W = ".", p = ".")
      row[[".isNewGroup"]] <- rowNo == 1
      container[["ttestNormalTable"]]$addRows(row)
    }
  }
}

.ttestDescriptivesContainer <- function(jaspResults, options) {
  if(!options$descriptives && !options$descriptivesPlots) return()
  if (is.null(jaspResults[["ttestDescriptives"]])) {
    container <- createJaspContainer("Descriptives")
    container$dependOn(c("descriptives", "descriptivesPlots", 
                         "variables", "pairs", "groupingVariable"))
    jaspResults[["ttestDescriptives"]] <- container
  }
}

.ttestDescriptivesTable <- function(jaspResults, dataset, options, ready, type) {
  # Container
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  if (!options$descriptives || !is.null(container[["table"]])) 
    return()
  if(type == "independent")
    title <- "Group Descriptives"
  else
    title <- "Descriptives"
  # Create table
  ttestDescriptivesTable <- createJaspTable(title = title)
  #dependList <- c()
  #ttestPairMainTable$dependOn(dependList)
  ttestDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  if(type == "independent") {
    ttestDescriptivesTable$addColumnInfo(name = "variable", title = "", type = "string", combine = TRUE)
    ttestDescriptivesTable$addColumnInfo(name = "group", title = "Group", type = "string")
  } else 
    ttestDescriptivesTable$addColumnInfo(name = "v",    type = "string",  title = "")
  ttestDescriptivesTable$addColumnInfo(name = "N",    type = "integer", title = "N")
  ttestDescriptivesTable$addColumnInfo(name = "mean", type = "number",  title = "Mean")
  ttestDescriptivesTable$addColumnInfo(name = "sd",   type = "number",  title = "SD")
  ttestDescriptivesTable$addColumnInfo(name = "se",   type = "number",  title = "SE")

  container[["table"]] <- ttestDescriptivesTable

  if(!ready) 
    return()
  res <- try(.ttestDescriptivesFill(container, dataset, options, type))
  .ttestSetError(res, ttestDescriptivesTable)
}

.ttestDescriptivesFill <- function(container, dataset, options, type) {
  if(type %in% c("paired", "one-sample")) {
    if(type == "paired") {
      if(length(unlist(options$pairs)) == 0) return()
      descriptives.results <- list()
      desc.vars <- unique(unlist(options$pairs))
      desc.vars <- desc.vars[desc.vars != ""]
    } else if(type == "one-sample") {
      desc.vars <- unique(unlist(options$variables))
      desc.vars <- desc.vars[desc.vars != ""]
    }
    
    if(length(desc.vars) == 0)
      return()
    rowNo <- 1
    for (var in desc.vars) {
      row <- list(v = var)
      
      dat <- na.omit(dataset[[ .v(var) ]])
      n   <- as.numeric(length(dat))
      m   <- as.numeric(mean(dat, na.rm = TRUE))
      std <- as.numeric(sd(dat,   na.rm = TRUE))
      
      if (is.numeric(std))
        se <- as.numeric(std/sqrt(n))
      else
        se <- NaN
      
      row[["N"]]    <- n
      row[["mean"]] <- m
      row[["sd"]]   <- std
      row[["se"]]   <- se
      if(type == "one-sample")
        row[[".isNewGroup"]] <- rowNo == 1
      rowNo <- rowNo + 1
      container[["table"]]$addRows(row)
    }
  }
  else if(type == "independent") {
    data <- list()
    
    variables <- options$variables
    if (length(variables) == 0) variables <- "."
    
    groups <- options$groupingVariable
    
    ## if we actually have to do the test, and we have a grouping variable
    if (groups != "") {
      levels <- base::levels(dataset[[ .v(groups) ]])
      
      groupingData <- dataset[[.v(groups)]]
      
      ## do the whole loop as above again
      for (variable in variables) {
        
        for (i in 1:2) { #only 2 levels thanks to error check
          
          level <- levels[i]
          variableData <- dataset[[.v(variable)]]
          
          groupData   <- variableData[groupingData == level]
          groupDataOm <- na.omit(groupData)
          
          if (class(groupDataOm) != "factor") {
            
            n    <- length(groupDataOm)
            mean <- mean(groupDataOm)
            std  <- sd(groupDataOm)
            sem  <- std / sqrt(n)
            
            result <- list(variable = variable, group = level,
                           N = n, mean = mean, sd = std, se = sem)
            
          } else {
            n      <- length(groupDataOm)
            result <- list(variable = variable, group = "",
                           N = n, mean = "", sd = "", se = "")
          }
          if (i == 1) {
            result[[".isNewGroup"]] <- TRUE
          }
          
          data[[length(data) + 1]] <- result
        }
      }
    }
    container[["table"]]$addRows(data)
  }
}

.ttestVovkSellke <- function(table, options) {
  if (options$VovkSellkeMPR) {
    message <-"Vovk-Sellke Maximum <em>p</em>-Ratio: Based on a two-sided <em>p</em>-value, 
    the maximum possible odds in favor of H\u2081 over H\u2080 equals 1/(-e <em>p</em> log(<em>p</em>)) 
    for <em>p</em> \u2264 .37 (Sellke, Bayarri, & Berger, 2001)."
    table$addFootnote(message, symbol = "\u002A")
    table$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number")
  }
}

.ttestSetError <- function(res, table){
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}
