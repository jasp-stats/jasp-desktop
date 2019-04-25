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

Manova <- function(jaspResults, dataset, options) {
  
  dependentVariables <- unlist(options$dependent)
  randomFactors <- unlist(options$fixedFactors)

  # Read dataset
  dataset <- .readDataSetToEnd(columns.as.numeric=dependentVariables, 
                               columns.as.factor=randomFactors)

  # Error checking
  errors <- .manovaCheckErrors(dataset, options)

  # Compute the results
  manovaResults <- .manovaComputeResults(jaspResults, dataset, options, errors)
  
  # Output tables
  .manovaTableMain(jaspResults, dataset, options, manovaResults, errors)

  .uniAnovaTables(jaspResults, dataset, options, manovaResults, errors)
  
  return()
  
}

.manovaComputeResults <- function(jaspResults, dataset, options, errors) {
  
  if (!is.null(errors) && errors == "No variables") return()
  
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

.manovaTableMain <- function(jaspResults, dataset, options, manovaResults, errors) {
  
  if (!is.null(jaspResults[["manovaContainer"]])) return()
  
  manovaContainer <- createJaspContainer(title = "MANOVA")
  jaspResults[["manovaContainer"]] <- manovaContainer
  
  manovaContainer$dependOn(c("dependent", "fixedFactors", "testPillai", "testWilks",
                         "testHotellingLawley", "testRoy", "includeIntercept",
                         "VovkSellkeMPR", "modelTerms", "includeAnovaTables"))

  allTests <- if(is.null(names(manovaResults$manova))) "Pillai" else names(manovaResults$manova)
  
  for (thisTest in allTests) {
    
    # Create table
    manovaTable <- createJaspTable(title = paste0("MANOVA: ", thisTest, " Test"))
    
    manovaTable$showSpecifiedColumnsOnly <- TRUE
    
    # Add columns to table
    manovaTable$addColumnInfo(name = "cases",   title = "Cases", type = "string")
    manovaTable$addColumnInfo(name = "df",      title = "df",      type = "integer")
    manovaTable$addColumnInfo(name = "appF",    title = "Approx. F",      type = "number")
    manovaTable$addColumnInfo(name = "testStat",title = "Test statistic",     type = "number")
    manovaTable$addColumnInfo(name = "dfNum",   title = "Num df",      type = "integer")
    manovaTable$addColumnInfo(name = "dfDen",   title = "Den df",      type = "number")
    manovaTable$addColumnInfo(name = "p",       title = "p",          type = "pvalue")
    
    if (options$VovkSellkeMPR) {
      manovaTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number")
    }
    
    jaspResults[["manovaContainer"]][[thisTest]] <- manovaTable
    
    if (!is.null(errors) && errors == "No variables")
      return()
    

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

.uniAnovaTables <- function(jaspResults, dataset, options, manovaResults, errors) {
  
  if (!is.null(jaspResults[["anovaContainer"]]) || !options$includeAnovaTables) return()
  
  anovaContainer <- createJaspContainer(title = "ANOVA")
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
    anovaTable$addColumnInfo(name = "cases",   title = "Cases", type = "string")
    anovaTable$addColumnInfo(name = "Df",      title = "df",      type = "integer")
    anovaTable$addColumnInfo(name = "Sum Sq",    title = "Sum of Squares",      type = "number")
    anovaTable$addColumnInfo(name = "Mean Sq",title = "Mean Square",     type = "number")
    anovaTable$addColumnInfo(name = "F value",   title = "F",      type = "number")
    anovaTable$addColumnInfo(name = "Pr(>F)",       title = "p",          type = "pvalue")
    
    if (options$VovkSellkeMPR) {
      anovaTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number")
    }
    
    if (!is.null(errors) && errors == "No variables")
      return()
    
    jaspResults[["anovaContainer"]][[thisVar]] <- anovaTable
    
    for (case in rownames(manovaResults[["anova"]][[thisVar]])) {
      row <- as.list(manovaResults[["anova"]][[thisVar]][case, ])
      row["cases"] <- case
      row["VovkSellkeMPR"] <- if(trimws(case) == "Residuals") "" else .VovkSellkeMPR(row[["Pr(>F)"]])
      anovaTable$addRows(row, rowNames = paste0(thisVar, " - ", case))
    }
    
    # Add footnote: VovkSellkeMPR
    if (options$VovkSellkeMPR) {
      anovaTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
    }
    
  }
}

.manovaCheckErrors <- function(dataset, options) {
  
  # Check if results can be computed
  if ((length(options$dependent) < 2) || length(options$fixedFactors) == 0 || length(options$modelTerms) == 0)
    return("No variables")
  
  # Error check
  for(i in length(options$modelTerms):1) {
    .hasErrors(
      dataset = dataset, 
      type = c('observations', 'variance', 'infinity', 'varCovData', 'factorLevels'),
      all.target = options$dependent, 
      all.grouping = options$modelTerms[[i]][['components']],
      factorLevels.amount  = "< 2",
      observations.amount = c('< 2'), 
      exitAnalysisIfErrors = TRUE)
  }

}
