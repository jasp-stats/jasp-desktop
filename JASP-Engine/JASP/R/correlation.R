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
Correlation <- function(jaspResults, dataset, options){
  dataset <- .corrReadData(dataset, options)
  ready <- length(options$variables) >= 2
  
  corrResults <- .corrMainResults(jaspResults, dataset, options, ready)
  
  .corrAssumptions(jaspResults, dataset, options, ready, corrResults)
  .corrPlot(jaspResults, dataset, options, ready, corrResults)
  .corrHeatmap(jaspResults, options, corrResults, ready)
  
  return()
}

.corrGetTests <- function(options){
  tests <- c("pearson", "spearman", "kendall")
  testsNames <- c(gettext("Pearson's"), gettext("Spearman's"), gettext("Kendall's Tau"))
  
  whichTests <- c(options[['pearson']], options[['spearman']], options[['kendallTauB']])
  usedTests <- tests[whichTests]
  usedTestsNames <- testsNames[whichTests]
  
  return(list(
    tests = tests, testsNames = testsNames,
    usedTests = usedTests, usedTestsNames = usedTestsNames
  ))
}

.corrTestChecked <- function(test = c('pearson', 'spearman', 'kendallsTauB'), options){
  test <- match.arg(test) # ensures that 'kendall' is also valid
  
  return(isTRUE(options[[test]]))
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

### Main function ----
.corrMainResults <- function(jaspResults, dataset, options, ready){
  if(!is.null(jaspResults[['mainTable']]) && !is.null(jaspResults[['results']])) return(jaspResults[['results']]$object)
  
  # Init main table
  mainTable <- .corrInitMainTable(jaspResults, options)
  
  # Compute results
  corrResults <- .corrComputeResults(jaspResults, dataset, options, ready)
  
  # Fill table
  .corrFillTableMain(mainTable, corrResults, options, ready)
  
  return(corrResults)
}
# Init tables ----
.corrInitMainTable <- function(jaspResults, options){
  if(!is.null(jaspResults[['mainTable']])) return(jaspResults[['mainTable']])
  
  if(length(options[['variables']]) == 0){
    variables <- c("...", "... ") # we need this trailing space so that 1 != 2
  } else if(length(options[['variables']]) == 1){
    variables <- c(options[['variables']], "...")
  } else {
    variables <- options[['variables']]
  }
  
  tests <- c(gettext("Pearson's"), gettext("Spearman's"), gettext("Kendall's Tau"))[c(options$pearson, options$spearman, options$kendallsTauB)]
  
  if(length(tests) != 1){
    if(length(options$conditioningVariables) == 0){
      title <- gettext("Correlation Table")
    } else{
      title <- gettext("Partial Correlation Table")
    }
  } else{
    if(length(options$conditioningVariables) == 0){
      title <- gettextf("%s Correlations", tests)
    } else{
      title <- gettextf("%s Partial Correlations", tests)
    }
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
    .corrInitPairwiseTable(mainTable, options, variables)
  } else{
    .corrInitCorrelationTable(mainTable, options, variables)
  }
  
  if(options[['hypothesis']] == "correlatedPositively"){
    mainTable$addFootnote(message = gettext("All tests one-tailed, for positive correlation"))
    additionToFlagSignificant <- gettext(", one-tailed")
  } else if(options[['hypothesis']] == "correlatedNegatively"){
    mainTable$addFootnote(message = gettext("All tests one-tailed, for negative correlation"))
    additionToFlagSignificant <- gettext(", one-tailed")
  } else{
    additionToFlagSignificant <- ""
  }
  if(options[['flagSignificant']]) mainTable$addFootnote(message = gettextf("p < .05, ** p < .01, *** p < .001%s",
                                                                           additionToFlagSignificant), symbol = "*")
  
  if(length(options$conditioningVariables) > 0){
    message <- gettextf("Conditioned on variables: %s", paste(options$conditioningVariables, collapse = ", "))
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
  mainTable$addColumnInfo(name = "separator", title = "", type = "separator")
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
  testNames <- c(pearson=gettext("Pearson"), spearman=gettext("Spearman"), kendall=gettext("Kendall"))
  
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
        mainTable$addColumnInfo(name = paste0(test, "_p.value"), title = gettext("p"), type = "pvalue", overtitle = overtitle)
      
      if(options$confidenceIntervals){
        mainTable$addColumnInfo(name = paste0(test, "_lower.ci"), 
                                title = gettextf("Lower %s%% CI", 100*options$confidenceIntervalsInterval), type = "number",
                                overtitle = overtitle)
        mainTable$addColumnInfo(name = paste0(test, "_upper.ci"), 
                                title = gettextf("Upper %s%% CI", 100*options$confidenceIntervalsInterval), type = "number",
                                overtitle = overtitle)
      }
      
      if(options$VovkSellkeMPR){
        mainTable$addColumnInfo(name = paste0(test, "_vsmpr"), title = gettext("VS-MPR"), type = "number", overtitle = overtitle)
        mainTable$addFootnote(message = .corrGetTexts()$footnotes$VSMPR, symbol = "\u2020", colNames = paste0(test, "_vsmpr"))
        mainTable$addCitation(.corrGetTexts()$references$Sellke_etal_2001)
      }
    }
  }
}

.corrTitlerer <- function(test, nTests){
  if(nTests > 1){
    coeffs <- c(pearson = gettext("r"), spearman = gettext("rho"), kendall = gettext("tau B"))
  } else{
    coeffs <- c(pearson = gettext("Pearson's r"), spearman = gettext("Spearman's rho"), kendall = gettext("Kendall's tau B"))
  }
  
  return(coeffs[test])
}

.corrInitCorrelationTable <- function(mainTable, options, variables){
  mainTable$transpose <- TRUE
  mainTable$transposeWithOvertitle <- FALSE
  
  mainTable$addColumnInfo(name = "var1", title = "", type = "string", combine = FALSE, overtitle = "Variable")
  
  whichtests <- c(options$pearson, options$spearman, options$kendallsTauB)
  
  #Apparently we are defining these titles over and over again, maybe .corrTitlerer could be reused or this stored somewhere globally?
  testsTitles <- c(gettext("Pearson's r"), gettext("Spearman's rho"), gettext("Kendall's Tau B"))[whichtests]
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
}

.corrInitCorrelationTableRowAsColumn <- function(mainTable, options, var, coeff, test, overtitle){
  vvar <- .v(var)
  name <- paste(vvar, test, "%s", sep = "_")
  
  mainTable$addColumnInfo(name = sprintf(name, "estimate"), title = coeff, type = "number", overtitle = overtitle)
  
  if(options$reportSignificance)
    mainTable$addColumnInfo(name = sprintf(name, "p.value"), title = gettext("p-value"), type = "pvalue", overtitle = overtitle)
  
  if(options$VovkSellkeMPR){
    mainTable$addColumnInfo(name = sprintf(name, "vsmpr"), title = gettext("VS-MPR"), type = "number", overtitle = overtitle)
    mainTable$addFootnote(colNames = sprintf(name, "vsmpr"), symbol = "\u2020",
                          message = .corrGetTexts()$footnotes$VSMPR)
    mainTable$addCitation(.corrGetTexts()$references$Sellke_etal_2001)
  }
  
  if(options$confidenceIntervals){
    mainTable$addColumnInfo(name = sprintf(name, "upper.ci"), 
                            title = gettextf("Upper %s%% CI", 100*options$confidenceIntervalsInterval),
                            type = "number", overtitle = overtitle)
    mainTable$addColumnInfo(name = sprintf(name, "lower.ci"), 
                            title = gettextf("Lower %s%% CI", 100*options$confidenceIntervalsInterval),
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

  pcor <- !length(options$conditioningVariables) == 0

  results <- list()
  #startProgressbar(length(vpair)) 
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
      
      errors <-.hasErrors(data, message = 'short', 
                          type = c('variance', 'infinity', 'observations'),
                          all.target = .unv(vcomb[[i]]), 
                          observations.amount = sprintf("< %s", 3+length(options$conditioningVariables)),
                          exitAnalysisIfErrors = FALSE)
      
      # for some reason .hasErrors does not flag this case
      if(nrow(data) == 0) errors <- list(observations = "All missing")
      
      # shorten the message for observations.amount (do not list variables which is apparent in the output)
      if(is.list(errors) && !is.null(errors$observations)){
        errors$message <- gettextf("Number of observations is < %s", 3+length(options$conditioningVariables))
      }
      
      currentResults <- list()
      testErrors     <- list()
      currentResults[['sample.size']] <- nrow(data)
      
      # even if we do not want the specific tests results
      # we still want the output as NaN - to fill the jaspTables correctly
      # so we still loop over all tests - .corr.test() returns empty lists if isFALSE(compute)
      for(test in c('pearson', 'spearman', 'kendall')){
        compute <- isFALSE(errors) && .corrTestChecked(test, options)
        
        r <- .corr.test(x = data[,1], y = data[,2], z = condData,
                        method = test, alternative = alt,
                        conf.interval = options$confidenceIntervals,
                        conf.level = options$confidenceIntervalsInterval,
                        compute = compute, sample.size = currentResults[['sample.size']])
        testErrors[[test]] <- r[['errors']]
        currentResults[[test]] <- r[['result']]
      }
      # stolen from manova.R
      shapiro <- .multivariateShapiroComputation(data, list(dependent = .unv(vcomb[[i]])))
      
      results[[vpair[i]]] <- list(vars       = .unv(vcomb[[i]]), 
                                  vvars      = vcomb[[i]],
                                  res        = currentResults,
                                  errors     = errors, 
                                  testErrors = testErrors,
                                  shapiro    = shapiro)
      
      # store state for pair
      state <- createJaspState(object = results[[vpair[i]]])
      state$dependOn(options = c("conditioningVariables", "hypothesis",
                                 "confidenceIntervals", "confidenceIntervalsInterval",
                                 "missingValues"),
                     optionContainsValue = list(variables = .unv(vcomb[[i]])))
      
      jaspResults[[vpair[i]]] <- state
    }
    
    #progressbarTick()
  }
  
  
  jaspResults[['results']] <- createJaspState(object = results)
  jaspResults[['results']]$dependOn(options = c("variables", "conditioningVariables",  "hypothesis",
                                                "confidenceIntervalsInterval", "missingValues",
                                                "pearson", "spearman", "kendallsTauB"))
  
  
  return(results)
}

# helper that unifies output of cor.test and ppcor::pcor.test
.corr.test <- function(x, y, z = NULL, alternative, method, exact = NULL, conf.interval = TRUE, conf.level = 0.95, continuity = FALSE, compute=TRUE, sample.size, ...){
  stats <- c("estimate", "p.value", "conf.int", "vsmpr")
  statsNames <- c("estimate", "p.value", "lower.ci", "upper.ci", "vsmpr")
  
  if(isFALSE(compute)){
    result <- rep(NaN, length(statsNames))
    names(result) <- statsNames
    errors <- FALSE
  } else if(is.null(z)){
    result <- try(expr = {
      cor.test(x = x, y = y, alternative = alternative, method = method, exact = exact, 
               conf.level = conf.level, continuity = continuity, ... = ...)}, silent = TRUE)
    
    if(isTryError(result)) {
      errors <- .extractErrorMessage(result)
      result <- rep(NaN, length(statsNames))
      names(result) <- statsNames
    } else{
      errors <- FALSE
      
      if(method != "pearson" && conf.interval){
        result$conf.int <- .createNonparametricConfidenceIntervals(x = x, y = y, obsCor = result$estimate,
                                                                   hypothesis = alternative, confLevel = conf.level,
                                                                   method = method)
      } else if(is.null(result$conf.int)){
        result$conf.int <- c(NA, NA)
      }
      
      result$vsmpr <- JASP:::.VovkSellkeMPR(result$p.value)
      result$vsmpr <- ifelse(result$vsmpr == "∞", Inf, result$vsmpr)
      result <- unlist(result[stats], use.names = FALSE)
      names(result) <- statsNames
    }
  } else{
    result <- try(expr = {ppcor::pcor.test(x = x, y = y, z = z, method = method)}, silent = TRUE)
    if(isTryError(result)) {
      errors <- .extractErrorMessage(result)
      if(startsWith(errors, "reciprocal condition number")) errors <- gettext("Partial correlation cannot be computed: covariance matrix is computationally singular.")
      result <- rep(NaN, length(statsNames))
      names(result) <- statsNames
      result$lower.ci <- NA
      result$upper.ci <- NA
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
      result$vsmpr <- JASP:::.VovkSellkeMPR(result$p.value)
      result$vsmpr <- ifelse(result$vsmpr == "∞", Inf, result$vsmpr)
      # TODO: CIs for partial correlations
      result$lower.ci <- NA
      result$upper.ci <- NA
      result <- unlist(result[statsNames], use.names = FALSE)
      names(result) <- statsNames
    }
  }
  
  return(list(result = result, errors = errors))
}


.corrAssumptions <- function(jaspResults, dataset, options, ready, corrResults){
  # uses .multivariateShapiroComputation from manova.R
  if(isFALSE(options$multivariateShapiro) && isFALSE(options$pairwiseShapiro)) return()
  
  if(is.null(jaspResults[['assumptionsContainer']])){
    assumptionsContainer <- createJaspContainer(title = gettext("Assumption checks"))
    assumptionsContainer$dependOn(c("variables", "conditioningVariables"))
    assumptionsContainer$position <- 2
    
    jaspResults[['assumptionsContainer']] <- assumptionsContainer
  } else {
    assumptionsContainer <- jaspResults[['assumptionsContainer']]
  }
  
  if(isTRUE(options$multivariateShapiro) && is.null(assumptionsContainer[['multivariateShapiro']]))
    .corrMultivariateShapiro(assumptionsContainer, dataset, options, ready, corrResults)

  if(isTRUE(options$pairwiseShapiro) && is.null(assumptionsContainer[['pairwiseShapiro']]))
    .corrPairwiseShapiro(assumptionsContainer, dataset, options, ready, corrResults)
  
}

.corrMultivariateShapiro <- function(assumptionsContainer, dataset, options, ready, corrResults){
  shapiroTable <- createJaspTable(title = gettext("Shapiro-Wilk Test for Multivariate Normality"))
  shapiroTable$dependOn("multivariateShapiro")
  shapiroTable$position <- 1
  shapiroTable$showSpecifiedColumnsOnly <- TRUE

  if(length(options$conditioningVariables) == 0){

    shapiroTable$addColumnInfo(name = "W", title = gettext("Shapiro-Wilk"), type = "number")
    shapiroTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")

    assumptionsContainer[['multivariateShapiro']] <- shapiroTable

    if(ready) {
      dataset <- dataset[complete.cases(dataset),,drop=FALSE]
      shapiroResult <- .multivariateShapiroComputation(dataset, list(dependent = options$variables))
      shapiroErrors <- shapiroResult$errors
      shapiroResult <- shapiroResult$result
      shapiroTable$addRows(list(W = shapiroResult$statistic, p = shapiroResult$p.value))

      if (!is.null(shapiroErrors))shapiroTable$setError(shapiroErrors)
    }
  } else{
    shapiroTable$addColumnInfo(name = "vars", title = gettext("Variables"),    type = "string")
    shapiroTable$addColumnInfo(name = "W",    title = gettext("Shapiro-Wilk"), type = "number")
    shapiroTable$addColumnInfo(name = "p",    title = gettext("p"),            type = "pvalue")

    assumptionsContainer[['multivariateShapiro']] <- shapiroTable

    if(ready){
      dataset <- dataset[complete.cases(dataset),,drop=FALSE]
      shapiroResult <- list()
      shapiroResult[['All']]          <- .multivariateShapiroComputation(dataset, list(dependent = c(options$variables,
                                                                                                     options$conditioningVariables)))
      shapiroResult[['Conditioned']]  <- .multivariateShapiroComputation(dataset, list(dependent = options$variables))
      shapiroResult[['Conditioning']] <- .multivariateShapiroComputation(dataset, list(dependent = options$conditioningVariables))

      form <- sprintf("cbind(%s) ~ %s",
                      paste(.v(options$variables), collapse = ", "),
                      paste(.v(options$conditioningVariables), collapse = " + "))
      resids <- try(expr = {residuals(lm(formula = form, data = dataset))}, silent = TRUE)

      if(isTryError(resids)){
        shapiroResult[['Residuals']] <- list(errors = .extractErrorMessage(resids))
      } else{
        shapiroResult[['Residuals']] <- .multivariateShapiroComputation(resids, list(dependent = options$variables))
      }

      for(i in seq_along(shapiroResult)){
        if(!is.null(shapiroResult[[i]]$errors)){
          shapiroTable$setError(shapiroResult[[i]]$errors)
          break
        }
        shapiroTable$addRows(list(vars = names(shapiroResult)[i],
                                  W    = shapiroResult[[i]]$result$statistic,
                                  p    = shapiroResult[[i]]$result$p.value))
      }
    }
  }
}

.corrPairwiseShapiro <- function(assumptionsContainer, dataset, options, ready, corrResults){
  shapiroTable <- createJaspTable(title = gettext("Shapiro-Wilk Test for Bivariate Normality"))
  shapiroTable$dependOn(c("pairwiseShapiro", "missingValues"))
  shapiroTable$position <- 2
  shapiroTable$showSpecifiedColumnsOnly <- TRUE

  shapiroTable$addColumnInfo(name = "var1",      title = "",                      type = "string")
  shapiroTable$addColumnInfo(name = "separator", title = "",                      type = "separator")
  shapiroTable$addColumnInfo(name = "var2",      title = "",                      type = "string")
  shapiroTable$addColumnInfo(name = "W",         title = gettext("Shapiro-Wilk"), type = "number")
  shapiroTable$addColumnInfo(name = "p",         title = gettext("p"),            type = "pvalue")

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

      if(!is.null(res$shapiro$errors))  shapiroTable$addFootnote(message = res$shapiro$errors, rowNames = name)
    }
  }
}

### Fill Tables ----
.corrFillTableMain <- function(mainTable, corrResults, options, ready){
  if(!ready) return()
  
  if(options$displayPairwise){
    .corrFillPairwiseTable(mainTable, corrResults, options)
  } else{
    .corrFillCorrelationMatrix(mainTable, corrResults, options)
  }
  
  if(length(options$conditioningVariables) != 0 && isTRUE(options$confidenceIntervals))
    mainTable$addFootnote(message = gettext("Confidence intervals for partial correlations not yet available."))
}

.corrFillPairwiseTable <- function(mainTable, corrResults, options){
  # extract the list of results
  pairs <- names(corrResults)
  results <- lapply(corrResults, function(x) x[['res']])
  errors <- lapply(corrResults,function(x) x[['errors']])
  testErrors     <- lapply(corrResults, function(x) x[['testErrors']])
  
  mainTable[['sample.size']] <- sapply(results, function(x) x[['sample.size']])
  
  # would be nice to be able to fill table cell-wise, i.e., mainTable[[row, col]] <- value
  colNames <- character() # this is for error footnotes
  for(test in c("pearson", "spearman", "kendall")){
    res <- data.frame(do.call(rbind, lapply(results, function(x) {x[[test]]})), stringsAsFactors = FALSE)
    
    for(col in colnames(res)) mainTable[[paste(test, col, sep = "_")]] <- res[[col]]
    
    if(options[['flagSignificant']]) .corrFlagSignificant(mainTable, res[['p.value']],
                                                          paste(test, "estimate", sep="_"), pairs)
    
    colNames <- c(colNames, paste(test, colnames(res), sep="_"))
  }
  for(i in seq_along(errors)){
    # add footnotes for general errors
    if(!isFALSE(errors[[i]])) mainTable$addFootnote(message = errors[[i]]$message, rowNames = pairs[i],
                                                    colNames = colNames)
    
    # add footnotes for test specific errors
    for(test in c("pearson", "spearman", "kendall")){
      if(!isFALSE(testErrors[[i]][[test]])){
        errorColNames <- colNames[startsWith(colNames, test)]
        mainTable$addFootnote(message = testErrors[[i]][[test]], rowNames = pairs[i], colNames = errorColNames)
      }
    }
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
  
.corrFillCorrelationMatrix <- function(mainTable, corrResults, options){
  vars  <- options$variables
  vvars <- .v(vars)
  pairs <- strsplit(names(corrResults), "_")
  
  results <- lapply(corrResults, function(x) {
    res <- unlist(x[['res']]) # flatten the structure but preserve names (hierarchy separated by ".")
    # replace the first dot with _ to separate test from statistic
    names(res) <- sub("(pearson\\.)",  "pearson_", names(res))
    names(res) <- sub("(spearman\\.)", "spearman_", names(res))
    names(res) <- sub("(kendall\\.)",  "kendall_", names(res))
    res
    })
  
  errors         <- lapply(corrResults, function(x) x[['errors']])
  testErrors     <- lapply(corrResults, function(x) x[['testErrors']])
  statsNames     <- names(results[[1]])
  nStats         <- length(statsNames)
  
  # would be really (!) nice to be able to fill table cell-wise, i.e., mainTable[[row, col]] <- value
  # in the meantime we have to collect and fill the entire table in the resultList
  resultList <- list(var1 = vars)
  for(colVar in seq_along(vars)){
    for(statName in statsNames){
      currentColumnName <- paste(vvars[[colVar]], statName, sep = "_")
      resList <- list()
      for(rowVar in seq_along(vars)){
        currentPairName <- paste(vvars[rowVar], vvars[colVar], sep = "_")
        if(rowVar == colVar){
          r <- "\u2014" # long dash
        } else if(rowVar > colVar){
          r <- NA # upper triangle is empty
        } else {
          r <- results[[currentPairName]][[statName]]
        }
        resList[[vvars[rowVar]]] <- r
      }
      resultList[[currentColumnName]] <- resList
    }
  }
  
  mainTable$setData(resultList)
  
  # Flag significant
  if(options[['flagSignificant']]){
    p.valueColumns <- names(resultList)[endsWith(names(resultList), "p.value")]
    
    for(columnName in p.valueColumns){
      p.values <- unlist(resultList[[columnName]])
      .corrFlagSignificant(table = mainTable, p.values = p.values,
                           colName = gsub("p.value", "estimate", columnName),
                           rowNames = vvars)
    }
  }
  
  # Report errors as footnotes
  for(i in seq_along(errors)){
    # display general errors (i.e., too much missing data, etc. identified from .hasErrors)
    if(is.list(errors[[i]])){
      pair <- pairs[[i]]
      colNames <- statsNames[statsNames != "sample.size"]
      colNames <- paste(pair[2], colNames, sep = "_")
      mainTable$addFootnote(message = errors[[i]]$message, colNames = colNames, rowNames = pair[1])
    }
    
    # display test errors (i.e., during calculating results, such as failure to invert a correlation matrix, etc.)
    for (test in c("pearson", "spearman", "kendall")) {
      if (!isFALSE(testErrors[[i]][[test]])) {
        pair <- pairs[[i]]
        colNames <- statsNames[startsWith(statsNames, test)]
        colNames <- paste(pair[2], colNames, sep = "_")
        mainTable$addFootnote(message = testErrors[[i]][[test]], colNames = colNames, rowNames = pair[1])
      }
    }
  }
}
### Plots ----
.corrPlot <- function(jaspResults, dataset, options, ready, corrResults){
  if(!ready) return()
  if(isFALSE(options$plotCorrelationMatrix)) return()
  
  if(isTRUE(options[["displayPairwise"]])){
    .corrPairwisePlot(jaspResults, dataset, options, ready, corrResults)
  } else{
    .corrMatrixPlot(jaspResults, dataset, options, ready, corrResults)
  }
}

.corrPairwisePlot <- function(jaspResults, dataset, options, ready, corrResults, errors=NULL){
  if(!is.null(jaspResults[['corrPlot']])) return()
  
  plotContainer <- createJaspContainer(title = gettext("Scatter plots"))
  plotContainer$dependOn(options = c("variables", "conditioningVariables", "pearson", "spearman", "kendallsTauB",
                                     "displayPairwise", "confidenceIntervals", "confidenceIntervalsInterval", "hypothesis",
                                     "plotCorrelationMatrix", "plotDensities", "plotStatistics", "plotConfidenceIntervals", 
                                     "plotConfidenceIntervalsInterval", "plotPredictionIntervalsInterval",
                                     "plotPredictionIntervals", "missingValues"))
  plotContainer$position <- 3
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
      
      plotMat[[1, 1]] <- .corrMarginalDistribution(variable = data[,1,drop=TRUE], varName = comb[[i]][1],
                                                   options = options, yName = NULL)
      plotMat[[2, 2]] <- .corrMarginalDistribution(variable = data[,2,drop=TRUE], varName = comb[[i]][2],
                                                   options = options, yName = NULL, coord_flip = TRUE)
      plotMat[[1, 2]] <- .corrValuePlot(corrResults[[vpairs[i]]], options = options)
      
      # get consistent breaks for scatterplot with the densities
      var1Breaks <- try(expr = {ggplot2::ggplot_build(plotMat[[1, 1]])$layout$panel_params[[1]]$x.major_source},
                        silent=TRUE)
      if(isTryError(var1Breaks)) var1Breaks <- NULL
      var2Breaks <- try(expr = {ggplot2::ggplot_build(plotMat[[2, 2]])$layout$panel_params[[1]]$y.major_source},
                        silent=TRUE)
      if(isTryError(var2Breaks)) var2Breaks <- NULL
      plotMat[[2, 1]] <- .corrScatter(xVar = data[,1,drop=TRUE], yVar = data[,2,drop=TRUE], 
                                      options = options,
                                      xBreaks = var1Breaks,
                                      yBreaks = var2Breaks,
                                      drawAxes = FALSE)
        
      
      plot$plotObject <- JASPgraphs::ggMatrixPlot(plotMat, 
                                                  bottomLabels = c(comb[[i]][1],       gettext("Density")),
                                                  leftLabels   = c(gettext("Density"), comb[[i]][2]))
    }
  } else if(options[['plotStatistics']]){
    for(i in seq_along(vcomb)){
      plot <- createJaspPlot(title = pairs[i], width = 600, height = 300)
      plotContainer[[vpairs[i]]] <- plot
      
      data <- dataset[vcomb[[i]]]
      data <- data[complete.cases(data),]
      
      plotMat <- matrix(list(), 1, 2)
      plotMat[[1, 1]] <- .corrScatter(xVar = data[,1,drop=TRUE], yVar = data[,2,drop=TRUE], 
                                      options = options,
                                      xName = comb[[i]][1], yName = comb[[i]][2],
                                      drawAxes = TRUE)
      plotMat[[1, 2]] <- .corrValuePlot(corrResults[[vpairs[i]]], options = options)
      
      plot$plotObject <- JASPgraphs::ggMatrixPlot(plotMat)
    }
  } else{
    for(i in seq_along(vcomb)){
      plot <- createJaspPlot(title = pairs[i], width = 400, height = 400)
      plotContainer[[vpairs[i]]] <- plot
      
      data <- dataset[vcomb[[i]]]
      data <- data[complete.cases(data),]
      
      plot$plotObject <- .corrScatter(xVar = data[,1,drop=TRUE], yVar = data[,2,drop=TRUE], 
                                      options = options,
                                      xName = comb[[i]][1], yName = comb[[i]][2], 
                                      drawAxes = TRUE)
    }
  }
}
.corrMatrixPlot <- function(jaspResults, dataset, options, ready, corrResults, errors=NULL){
  if(!is.null(jaspResults[['corrPlot']])) return()
  vars <- options$variables
  vvars <- .v(vars)
  len <- length(vars)
  
  
  plot <- createJaspPlot(title = gettext("Correlation plot"))
  plot$dependOn(options = c("variables", "conditioningVariables", "pearson", "spearman", "kendallsTauB", 
                            "displayPairwise", "confidenceIntervals", "confidenceIntervalsInterval", "hypothesis",
                            "plotCorrelationMatrix", "plotDensities", "plotStatistics", "plotConfidenceIntervals", 
                            "plotConfidenceIntervalsInterval", "plotPredictionIntervalsInterval",
                            "plotPredictionIntervals", "missingValues"))
  plot$position <- 3
  
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
      data <- dataset[,vvars[c(col,row)],drop=FALSE]
      data <- data[complete.cases(data),,drop=FALSE]
      
      if(row == col) {
        plotMat[[row, col]] <- .corrMarginalDistribution(variable = data[,1,drop=TRUE], 
                                                         varName = vars[col],
                                                         options = options, errors = errors)
      } else if(row > col){
        plotMat[[row, col]] <- .corrValuePlot(corrResults[[paste(vvars[c(col, row)], collapse = "_")]],
                                              options = options)
      } else {
        plotMat[[row, col]] <- .corrScatter(xVar = data[,1,drop=TRUE], yVar = data[,2,drop=TRUE],
                                            options = options)
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
    return(.displayError(errorMessage = gettextf("Correlation undefined: %s", results$errors$message)))
  }
  
  res   <- results$res
  tests <- c()

  if (options$pearson)      tests <- c(tests, "pearson")
  if (options$spearman)     tests <- c(tests, "spearman")
  if (options$kendallsTauB) tests <- c(tests, "kendall")
  
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
    estimate <- res[[tests[i]]][['estimate']]
    if (is.na(estimate)) {
      CIPossible[i] <- FALSE
      lab[i] <- switch(tests[i],
                       pearson =  paste(  "italic(r) == 'NA'"),
                       spearman = paste("italic(rho) == 'NA'"),
                       kendall =  paste("italic(tau) == 'NA'"))
    } else if (round(estimate, 8) == 1) {
      CIPossible[i] <- FALSE
      
      #no clue as to what is going on down there... Should this be translated?
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
      #these statements here could all be put together in the call to gettextf right? something like %.3d?
      lower.ci <- res[[tests[i]]][['lower.ci']]
      lower.ci <- formatC(lower.ci, format = "f", digits = 3)
      
      upper.ci <- res[[tests[i]]][['upper.ci']]
      upper.ci <- formatC(upper.ci, format = "f", digits = 3)
      
      cilab[i] <- gettextf("%s%% CI: [%s, %s]", 100*options$confidenceIntervalsInterval, lower.ci, upper.ci)
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
  if(isFALSE(options$plotDensities))  return(.displayError(errorMessage = "")) # return empty plot
  if(length(variable) < 3)            return(.displayError(errorMessage = gettext("Plotting not possible:\n Number of observations is < 3")))
  if(any(is.infinite(variable)))      return(.displayError(errorMessage = gettext("Plotting not possible: Infinite value(s)")))

  
  if(isTRUE(options$plotRanks)) variable <- rank(variable)
  
  p <- .plotMarginalCor(variable = variable, xName = xName, yName = yName)
  
  if(coord_flip){
    p <- p + ggplot2::coord_flip() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_line(), axis.ticks.x = ggplot2::element_blank(), 
                     axis.text.x = ggplot2::element_blank())
  }
  
  p
}

.corrScatter <- function(xVar, yVar, options, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL, 
                         drawAxes = TRUE) {
  if(length(xVar) <= 1 || length(yVar) <= 1) return(.displayError(errorMessage = gettext("Plotting not possible:\n Number of observations is < 2")))
  errors <- .hasErrors(data.frame(xVar = xVar, yVar = yVar), message = 'short', 
                       type = c('infinity'),
                       all.target = c("xVar", "yVar"), 
                       exitAnalysisIfErrors = FALSE)
  if(is.list(errors)) return(.displayError(errorMessage = gettextf("Plotting not possible: %s", errors$message)))
  
  if(isTRUE(options$plotRanks)) {
    xVar <- rank(xVar)
    yVar <- rank(yVar)
  }
  .plotScatter(xVar = xVar, yVar = yVar, options, xBreaks = xBreaks, yBreaks = yBreaks, xName = xName, yName = yName, 
               drawAxes = drawAxes)
}

.corrHeatmap <- function(jaspResults, options, corrResults, ready){
  if(isFALSE(options$plotHeatmap)) return()
  
  hw <- 30 + 80*length(options$variables)
  
  #TODO: The following looks rather familiar and all these defines should, I think, all be put together in one place instead of scattered throughout this file...
  tests <- c("pearson", "spearman", "kendall")
  if(length(options$conditioningVariables) == 0){
    names(tests) <- c(gettext("Pearson's r"), gettext("Spearman's rho"), gettext("Kendall's tau B"))
  } else{
    names(tests) <- c(gettext("Partial Pearson's r"), gettext("Partial Spearman's rho"), gettext("Partial Kendall's tau B"))
  }
  tests <- tests[c(options$pearson, options$spearman, options$kendallsTauB)]
  
  if(length(tests) == 0){
    return()
  } else if(length(tests) == 1){
    plot <- createJaspPlot(title = gettextf("%s heatmap", names(tests)), width = hw, height = hw)
    plot$dependOn(c("variables", "conditioningVariables", "missingValues", "pearson", "spearman", "kendallsTauB", 
                    "flagSignificant", "plotHeatmap"))
    plot$position <- 4
    jaspResults[['heatmaps']] <- plot
    
    if(ready) plot$plotObject <- .corrPlotHeatmap(tests, options, corrResults)
  } else{
    heatmaps <- createJaspContainer(title = gettext("Heatmaps"))
    heatmaps$dependOn(c("variables", "conditioningVariables", "missingValues", "pearson", "spearman", "kendallsTauB",
                        "flagSignificant", "plotHeatmap"))
    heatmaps$position <- 4
    jaspResults[['heatmaps']] <- heatmaps
    
    for(i in seq_along(tests)){
      plot <- createJaspPlot(title = names(tests[i]), width = hw, height = hw)
      heatmaps[[tests[[i]]]] <- plot
      
      if(ready) plot$plotObject <- .corrPlotHeatmap(tests[[i]], options, corrResults)
    }
  }
}

.corrPlotHeatmap <- function(method, options, corrResults){
  data <- lapply(corrResults, function(x){
    c(var1 = x[['vars']][1], var2 = x[['vars']][2], 
      cor = x[['res']][[method]][['estimate']], 
      p = x[['res']][[method]][['p.value']])
  })
  
  data <- do.call(rbind, data)
  data <- rbind(data, data[, c(2, 1, 3, 4)])
  data <- data.frame(data, stringsAsFactors = FALSE)
  data <- rbind(data, data.frame(var1 = options$variables, var2 = options$variables, cor = NA, p = 1))
  
  data$var1 <- factor(data$var1, levels = options$variables)
  data$var2 <- factor(data$var2, levels = rev(options$variables))
  data$cor <- as.numeric(data$cor)
  data$p <- as.numeric(data$p)
  
  data$label <- round(data$cor, 3)
  if(options$flagSignificant){
    data$label <- ifelse(data$p < 0.05 & !is.na(data$cor), paste0(data$label, "*"), data$label)
    data$label <- ifelse(data$p < 0.01 & !is.na(data$cor), paste0(data$label, "*"), data$label)
    data$label <- ifelse(data$p < 0.001 & !is.na(data$cor), paste0(data$label, "*"), data$label)
  }
  p <- ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2, fill = cor)) + 
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(x = var1, y = var2, label = label), size = 5) +
    ggplot2::scale_fill_gradient2(limits = c(-1, 1), na.value = "white") +
    ggplot2::coord_equal() + 
    ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
    #ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 270, hjust = 0, vjust = 0.5))
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  
  JASPgraphs::themeJasp(p)
}

## Old plotting (still in use)----
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

.plotScatter <- function(xVar, yVar, options = NULL, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL, drawAxes = TRUE) {
  
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
    
    if (options$plotConfidenceIntervals) {
      ci <- as.data.frame(stats::predict(fit, interval = "confidence", level = options$plotConfidenceIntervalsInterval))
      ci[["x"]] <- d$x
      
      p <- p + ggplot2::geom_line(data = ci, ggplot2::aes(x = x, y = lwr), size = 1, color = "darkblue", linetype = "dashed") +
        ggplot2::geom_line(data = ci, ggplot2::aes(x = x, y = upr), size = 1, color = "darkblue", linetype = "dashed")
    }
    
    if (options$plotPredictionIntervals) {
      pi <- as.data.frame(stats::predict(fit, interval = "prediction", level = options$plotPredictionIntervalsInterval))
      pi[["x"]] <- d$x
      
      p <- p + ggplot2::geom_line(data = pi, ggplot2::aes(x = x, y = lwr), size = 1, color = "darkgreen", linetype = "longdash") +
        ggplot2::geom_line(data = pi, ggplot2::aes(x = x, y = upr), size = 1, color = "darkgreen", linetype = "longdash")
    }
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

    #Again copy-paste from somewhere else. This should be put in the same place as all those other random lists  being made in this analysis
    tests <- c()
    if (pearson)      tests <- c(tests, "pearson")
    if (spearman)     tests <- c(tests, "spearman")
    if (kendallsTauB) tests <- c(tests, "kendall")


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

            #Im guessing these statements are related to my confusion at line 840. I guess we could gettext here and there and then it will work but be translated? Unless the user changes language maybe?
            #I'll leave it as is for now JCG 6-1-20
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
            p <- p + ggplot2::geom_text(data = data.frame(x = 1, y = 1.2), mapping = ggplot2::aes(x = x, y = y, label = gettextf("%1$d%% CI: [%2$d, %3$d]", 100 * confidenceInterval, CIlow, CIhigh)), size = 5)
        }

    }

    return(p)

}

### empty Plot with error message ###
.displayError <- function(errorMessage=NULL, cexText=1.6, lwdAxis= 1.2, wrap = 20) {
  if(!is.null(wrap)) errorMessage <- paste(strwrap(errorMessage, wrap), collapse="\n")
  
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

### helpers ----
.corrNormalApproxConfidenceIntervals <- function(obsCor, n, hypothesis="two.sided", confLevel=0.95){
  zCor <- atanh(obsCor)
  se   <- 1/sqrt(n-3)
  
  alpha <- 1-confLevel
  
  if(hypothesis == "two.sided"){
    z <- qnorm(p = alpha/2, lower.tail = FALSE)
    upper.ci <- tanh(zCor + z * se)
    lower.ci <- tanh(zCor - z * se)
  } else if(hypothesis == "less"){
    z <- qnorm(p = alpha, lower.tail = FALSE)
    upper.ci <- tanh(zCor + z * se)
    lower.ci <- -1
  } else if(hypothesis == "greater"){
    z <- qnorm(p = alpha, lower.tail = FALSE)
    upper.ci <- 1
    lower.ci <- tanh(zCor - z * se)
  }
  
  return(c(lower.ci,upper.ci))
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
  
  hypothesis <- switch(hypothesis, 
                       "two.sided" = "correlated",
                       "greater" = "correlatedPositively",
                       "less" = "correlatedNegatively",
                       hypothesis)
 if (method == "kendall") {
   concordanceSumsVector <- numeric(n)
    for (i in 1:n) {
      # progressbarTick() #Started in .corrComputeResults
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
      type <- gettext("italic(r)")
    else if (testType == "spearman")
      type <- gettext("italic(rho)")
    else #kendall
      type <- gettext("italic(tau)")

    formattedValue <- formatC(round(corValue, decimals), format = "f", digits = decimals)

    return(paste0(type, ' ~ "=" ~ ', '"', formattedValue, '"'))
}

.corrGetTexts <- function() {
  list(
  footnotes = list(
    VSMPR = gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum possible odds in favor of H%1$s over H%2$s equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37 (Sellke, Bayarri, & Berger, 2001).","\u2081","\u2080","\u2264")
  ),
  references = list(
    Sellke_etal_2001 = gettext("Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of p Values for Testing Precise Null Hypotheses. The American Statistician, 55(1), p. 62-71.")
  )
  )
}
