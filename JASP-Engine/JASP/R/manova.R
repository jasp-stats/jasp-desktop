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

Manova <- function(jaspResults, dataset = NULL, options) {
  
  # Check if we're ready to actually compute something or just show empty tables
  ready <- length(options$dependent) > 1 && length(options$fixedFactors) > 0 && length(options$modelTerms) > 0

  dependentVariables <- unlist(options$dependent)
  randomFactors <- unlist(options$fixedFactors)

  # Read dataset
  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = dependentVariables, 
                                 columns.as.factor = randomFactors,
                                 exclude.na.listwise = c(dependentVariables, randomFactors))
  }

  # Error checking
  .manovaCheckErrors(dataset, options, ready)

  # Compute the results
  manovaResults <- .manovaComputeResults(jaspResults, dataset, options, ready)
  
  # Output tables
  .manovaTableMain(jaspResults, dataset, options, manovaResults, ready)

  .uniAnovaTables(jaspResults, dataset, options, manovaResults, ready)
  
  .assumptionTables(jaspResults, dataset, options, ready)
  
  return()
  
}

.manovaComputeResults <- function(jaspResults, dataset, options, ready) {
  
  if (!ready) return()
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateManovaResults"]])) return(jaspResults[["stateManovaResults"]]$object)
  
  # This will be the object that we fill with results
  results <- list(manova = list())
  
  dependentVariables <- unlist(options$dependent)
  randomFactors <- unlist(options$fixedFactors)
  
  datasetDep <- as.matrix(dataset[.v(dependentVariables)])
  datasetFac <- dataset[.v(randomFactors)]
  
  
  factorModelTerms <- lapply(options$modelTerms, 
                             function(x) paste(.v(x[[1]]), collapse = ":"))
  
  modelFormula <- paste("datasetDep ~", paste(factorModelTerms, collapse = "+"))
  
  # Main Manova test result for tables
  manovaModel <- stats::aov(terms(formula(modelFormula), keep.order = TRUE), datasetFac)
  
  modelTerms <- sapply(options$modelTerms, function(x) paste0(x[[1]], collapse = " \u273B "))
  attr(manovaModel$terms, "term.labels") <- modelTerms

  # Run seperate ANOVA for each DV
  results[["anova"]] <- summary(manovaModel, intercept = options$includeIntercept)

  # Set up the different tests for Manova
  allTests <- c("Pillai", "Wilks", "Hotelling-Lawley", "Roy") 
  whichTests <- allTests[c(options$testPillai, 
                           options$testWilks, 
                           options$testHotellingLawley, 
                           options$testRoy)]

  for (thisTestType in whichTests) {
    
    manovaSummary <- summary.manova(manovaModel, 
                             test = thisTestType, 
                             intercept = options$includeIntercept)
    
    # Add results for each test type for each variable to results object
    for (case in manovaSummary[[1]]) {

      thisRow <- manovaSummary$stats[case, ]
      p <- thisRow['Pr(>F)']

      results[["manova"]][[thisTestType]][[case]] <- list(
        testType = thisTestType,
        cases      = case,
        testStat = thisRow[thisTestType],
        df         = thisRow['Df'],
        appF        = thisRow['approx F'],
        dfNum         = thisRow['num Df'],
        dfDen    = thisRow['den Df'],
        p             = p,
        VovkSellkeMPR = .VovkSellkeMPR(p)
      )
    }
  }


  # Save results to state
  jaspResults[["stateManovaResults"]] <- createJaspState(results)
  jaspResults[["stateManovaResults"]]$dependOn(c("dependent", "fixedFactors",
                                                 "testPillai", "testWilks",
                                                 "testHotellingLawley", "testRoy", 
                                                 "includeIntercept", "VovkSellkeMPR",
                                                 "modelTerms", "includeAnovaTables"))
  
  # Return results object
  return(results)
}

.manovaTableMain <- function(jaspResults, dataset, options, manovaResults, ready) {
  
  if (!is.null(jaspResults[["manovaContainer"]])) return()
  
  manovaContainer <- createJaspContainer(title = gettext("MANOVA"))
  jaspResults[["manovaContainer"]] <- manovaContainer
  
  manovaContainer$dependOn(c("dependent", "fixedFactors", "testPillai", "testWilks",
                         "testHotellingLawley", "testRoy", "includeIntercept",
                         "VovkSellkeMPR", "modelTerms", "includeAnovaTables"))

  # Set up the different tests for Manova
  allTests <- c("Pillai", "Wilks", "Hotelling-Lawley", "Roy") 
  whichTests <- allTests[c(options$testPillai, 
                           options$testWilks, 
                           options$testHotellingLawley, 
                           options$testRoy)]
  
  nameStatistic <- c(Pillai = "Trace<sub>Pillai</sub>", Wilks = "Wilks' \u039B",
                     `Hotelling-Lawley` = "Trace<sub>H-L</sub>", Roy = "Largest Root")
  
  for (thisTest in whichTests) {
    
    # Create table
    manovaTable <- createJaspTable(title = paste0("MANOVA: ", thisTest, " Test"))
    
    manovaTable$showSpecifiedColumnsOnly <- TRUE
    
    # Add columns to table
    manovaTable$addColumnInfo(name = "cases",   title = "Cases",                  type = "string")
    manovaTable$addColumnInfo(name = "df",      title = "df",                     type = "integer")
    manovaTable$addColumnInfo(name = "appF",    title = "Approx. F",              type = "number")
    manovaTable$addColumnInfo(name = "testStat",title = nameStatistic[thisTest],  type = "number")
    manovaTable$addColumnInfo(name = "dfNum",   title = "Num df",                 type = "integer")
    manovaTable$addColumnInfo(name = "dfDen",   title = "Den df",                 type = "number")
    manovaTable$addColumnInfo(name = "p",       title = "p",                      type = "pvalue")
    
    if (options$VovkSellkeMPR) {
      manovaTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number")
    }
    
    jaspResults[["manovaContainer"]][[thisTest]] <- manovaTable
    
    if (!ready)
      next
    

    for (case in names(manovaResults[["manova"]][[thisTest]])) {
      row <- manovaResults[["manova"]][[thisTest]][[case]]
      if (case == "Residuals") 
        row[["VovkSellkeMPR"]] <- ""
      manovaTable$addRows(row, rowNames = paste0(thisTest, " - ", case))
    }
    
    # Add footnote: VovkSellkeMPR
    if (options$VovkSellkeMPR) {
      manovaTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
    }
  }
}

.uniAnovaTables <- function(jaspResults, dataset, options, manovaResults, ready) {
  
  if (!is.null(jaspResults[["anovaContainer"]]) || !options$includeAnovaTables) return()
  
  anovaContainer <- createJaspContainer(title = gettext("ANOVA"))
  jaspResults[["anovaContainer"]] <- anovaContainer
  
  anovaContainer$dependOn(c("dependent", "fixedFactors", "testPillai", "testWilks",
                             "testHotellingLawley", "testRoy", "includeIntercept",
                             "VovkSellkeMPR", "modelTerms", "includeAnovaTables"))
  
  anovaResults <- manovaResults$anova

  for (thisVar in names(anovaResults)) {
    
    thisResult <- anovaResults[thisVar]
    
    # Create table
    varName <- .unv(gsub(thisVar, pattern = " Response ",replacement =  ""))
    
    anovaTable <- createJaspTable(title = paste0("ANOVA: ", varName))
    anovaTable$dependOn(c("dependent", "fixedFactors", "includeIntercept", 
                          "VovkSellkeMPR", "modelTerms"))
    
    anovaTable$showSpecifiedColumnsOnly <- TRUE
    
    # Add columns to table
    anovaTable$addColumnInfo(name = "cases",    title = "Cases",          type = "string")
    anovaTable$addColumnInfo(name = "Sum Sq",   title = "Sum of Squares", type = "number")
    anovaTable$addColumnInfo(name = "Df",       title = "df",             type = "integer")
    anovaTable$addColumnInfo(name = "Mean Sq",  title = "Mean Square",    type = "number")
    anovaTable$addColumnInfo(name = "F value",  title = "F",              type = "number")
    anovaTable$addColumnInfo(name = "Pr(>F)",   title = "p",              type = "pvalue")
    
    if (options$VovkSellkeMPR) {
      anovaTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number")
    }
    
    if (!ready)
      return()
    
    jaspResults[["anovaContainer"]][[thisVar]] <- anovaTable
    
    for (case in rownames(manovaResults[["anova"]][[thisVar]])) {
      row <- as.list(manovaResults[["anova"]][[thisVar]][case, ])
      row["cases"] <- stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", case)) # summary() seemingly escapes unicode on windows
      row["VovkSellkeMPR"] <- if(trimws(case) == "Residuals") "" else .VovkSellkeMPR(row[["Pr(>F)"]])
      anovaTable$addRows(row, rowNames = paste0(thisVar, " - ", case))
    }
    
    # Add footnote: VovkSellkeMPR
    if (options$VovkSellkeMPR) {
      anovaTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
    }
    
  }
}

.boxComputation <- function(dataset, options) {

  depData <- dataset[, .v(options$dependent)]
  factorData <- as.matrix(dataset[, .v(options$fixedFactors)])
  grouping <- apply(factorData, 1, function(x) as.factor(paste0(x, collapse = "")))
  
  # from the biotools package
  p <- ncol(depData)
  nlev <- nlevels(grouping)
  lev <- levels(grouping)
  dfs <- tapply(grouping, grouping, length) - 1
  
  errors <- NULL
  if (any(dfs < p)) 
    errors <- "Too few observations to calculate statistic. Each (sub)group must have at least as many observations as there are dependent variables."
  
  mats <- list() 
  aux <- list()
  for(i in 1:nlev) {
    mats[[i]] <- cov(depData[grouping == lev[i], ])
    aux[[i]] <- mats[[i]] * dfs[i]
  }

  names(mats) <- lev
  pooled <- Reduce("+", aux) / sum(dfs)
  logdet <- log(unlist(lapply(mats, det)))
  minus2logM <- sum(dfs) * log(det(pooled)) - sum(logdet * dfs)
  sum1 <- sum(1 / dfs)
  Co <- (((2 * p^2) + (3 * p) - 1) / (6 * (p + 1) * (nlev - 1))) * (sum1 - (1 / sum(dfs)))
  chiSq <- minus2logM * (1 - Co)
  df <- (choose(p, 2) + p) * (nlev - 1)
  pval <- pchisq(chiSq, df, lower.tail = FALSE)
  
  return(list(result = list(ChiSq = chiSq, df = df, p = pval), errors = errors))
}

.multivariateShapiroComputation <- function(dataset, options) {

  # From mvnormtest
  depData <- t(as.matrix(dataset[, .v(options$dependent)]))
  Us <- apply(depData, 1, mean, na.rm = TRUE)
  R <- depData - Us

  tryResult <- try(expr = {
    M.1 <- solve(R %*% t(R), tol = 1e-200)
    Rmax <- diag(t(R) %*% M.1 %*% R)
    C <- M.1 %*% R[, which.max(Rmax)]
    Z <- t(C) %*% depData
    
    result <- stats::shapiro.test(Z)
  }, silent = TRUE)

  if (isTryError(tryResult)) {
    result <- NULL
    if (grepl(tryResult[[1]], pattern = "singular"))
      errors <- gettext("The design matrix is not invertible. This might be due to collinear dependent variables.")
    else
      errors <- .extractErrorMessage(tryResult)
  } else {
    errors <- NULL
  }
  
   
  return(list(result = result, errors = errors))
}

.assumptionTables <- function(jaspResults, dataset, options, ready) {
  
  if (!is.null(jaspResults[["assumptionsContainer"]]) || (!options$boxMTest && !options$shapiroTest)) return()
  
  assumptionsContainer <- createJaspContainer(title = gettext("Assumption Checks"))
  jaspResults[["assumptionsContainer"]] <- assumptionsContainer
  
  assumptionsContainer$dependOn(c("dependent", "fixedFactors", "modelTerms", "boxMTest", "shapiroTest"))
  
  if (options$boxMTest) {
    # Make Box test table
    boxMTable <- createJaspTable(title = "Box's M-test for Homogeneity of Covariance Matrices")
    
    boxMTable$showSpecifiedColumnsOnly <- TRUE
    
    boxMTable$addColumnInfo(name = "ChiSq",   title = "\u03C7\u00B2",                  type = "number")
    boxMTable$addColumnInfo(name = "df",      title = "df",                     type = "integer")
    boxMTable$addColumnInfo(name = "p",       title = "p",                      type = "pvalue")
    
    jaspResults[["assumptionsContainer"]][["boxMTable"]] <- boxMTable
    
    if (ready) {
      boxResult <- .boxComputation(dataset, options)
      boxErrors <- boxResult$errors
      boxResult <- boxResult$result
      boxMTable$addRows(boxResult)
    
      if (!is.null(boxErrors))
        boxMTable$setError(boxErrors)
    }
  }
  
  if (options$shapiroTest) {
    # Make multivariate normal Shaprio table
    shapiroTable <- createJaspTable(title = "Shapiro-Wilk Test for Multivariate Normality")
    
    shapiroTable$showSpecifiedColumnsOnly <- TRUE
    
    shapiroTable$addColumnInfo(name = "W", title = "Shapiro-Wilk", type = "number")
    shapiroTable$addColumnInfo(name = "p", title = "p", type = "pvalue")
    
    jaspResults[["assumptionsContainer"]][["shapiroTable"]] <- shapiroTable
    
    if (ready) {
      shapiroResult <- .multivariateShapiroComputation(dataset, options)
      shapiroErrors <- shapiroResult$errors
      shapiroResult <- shapiroResult$result
      shapiroTable$addRows(list(W = shapiroResult$statistic, p = shapiroResult$p.value))
      
      if (!is.null(shapiroErrors)) 
        shapiroTable$setError(shapiroErrors)
    }
  }
}


.manovaCheckErrors <- function(dataset, options, ready) {
  if (!ready) return()

  # Error check
  for(i in length(options$modelTerms):1) {
    .hasErrors(
      dataset = dataset, 
      type = c('observations', 'variance', 'infinity', 'varCovData', 'factorLevels'),
      all.target = options$dependent, 
      all.grouping = options$modelTerms[[i]][['components']],
      factorLevels.amount  = "< 2",
      observations.amount = paste("<", length(options$dependent)+1), 
      exitAnalysisIfErrors = TRUE)
  }

}
