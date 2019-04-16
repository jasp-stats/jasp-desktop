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

Manova <- function(jaspResults, dataset, options, ...)
{
  
  dependentVariables <- unlist(options$dependent)
  randomFactors <- unlist(options$fixedFactors)

  # Read dataset
  dataset <- .readDataSetToEnd(columns.as.numeric=dependentVariables, 
                               columns.as.factor=randomFactors)

  # Error checking
  errors <- .manovaCheckErrors(dataset, options)
  # browser()
  
  # Compute the results
  manovaResults <- .manovaComputeResults(jaspResults, dataset, options, errors)
  
  # Output tables and plots
  .manovaTableMain(jaspResults, dataset, options, manovaResults, errors)

  return()
  
}

.manovaComputeResults <- function(jaspResults, dataset, options, errors) {
  
  if (!is.null(errors) && errors == "No variables") return()
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateManovaResults"]])) return(jaspResults[["stateManovaResults"]]$object)
  
  # This will be the object that we fill with results
  results <- list()
  
  results[["manova"]] <- list()
  
  dependentVariables <- unlist(options$dependent)
  randomFactors <- unlist(options$fixedFactors)
  
  datasetDep <- as.matrix(dataset[.v(dependentVariables)])
  datasetFac <- as.matrix(dataset[.v(randomFactors)])
  
  # Main Manova test result for tables
  manovaModel <- stats::manova(datasetDep ~ datasetFac)
  
  allTests <- c("Pillai", "Wilks", "Hotelling-Lawley", "Roy") 
  whichTests <- allTests[c(options$testPillai, 
                           options$testWilks, 
                           options$testHotellingLawley, 
                           options$testRoy)]

  for (thisTestType in whichTests) {
    
    manovaSummary <- summary(manovaModel, 
                             test = thisTestType, 
                             intercept = options$includeIntercept)
    
    for (case in manovaSummary[[1]][1]) {
      
      thisRow <- manovaSummary$stats[case, ]
      p <- thisRow['Pr(>F)']
      
      # Add results for each test type to results object
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
  # jaspResults[["stateManovaResults"]]$dependOn(
  #   c("dependent", "fixedFactors", "testPillai", "testWilks", 
  #     "testHotellingLawley", "testRoy", "includeIntercept",
  #     "VovkSellkeMPR")
  # )
  
  # Return results object
  return(results)
}

.manovaTableMain <- function(jaspResults, dataset, options, manovaResults, errors) {
  if (!is.null(jaspResults[["manovaTable"]])) return()
  
  # Create table
  manovaTable <- createJaspTable(title = "MANOVA")
  # manovaTable$dependOnOptions <- c("dependent", "fixedFactors", "testPillai", "testWilks", 
                         # "testHotellingLawley", "testRoy", "includeIntercept",
                         # "VovkSellkeMPR")
  

  # Add columns to table
  manovaTable$addColumnInfo(name = "cases",   title = "Cases", type = "string", combine = TRUE)
  manovaTable$addColumnInfo(name = "df",      title = "df",      type = "integer")
  manovaTable$addColumnInfo(name = "appF",    title = "Approx. F",      type = "number")
  manovaTable$addColumnInfo(name = "testStat",title = "Wilks",     type = "number")
  manovaTable$addColumnInfo(name = "dfNum",   title = "Num df",      type = "integer")
  manovaTable$addColumnInfo(name = "dfDen",   title = "Den df",      type = "integer")
  manovaTable$addColumnInfo(name = "p",       title = "p",          type = "pvalue")
  
  if (options$VovkSellkeMPR) {
    manovaTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number")
  }
  
  
  jaspResults[["manovaTable"]] <- manovaTable
  
  if (!is.null(errors) && errors == "No variables")
    return()
  
  for (thisTest in names(manovaResults$manova)) {
    for (case in names(manovaResults[["manova"]][[thisTest]])) {
      row <- manovaResults[["manova"]][[thisTest]][[case]]
      manovaTable$addRows(row, rowNames = paste0(thisTest, " - ", case))
    }
  }
  
  
  # Add footnote: VovkSellkeMPR
  if (options$VovkSellkeMPR) {
    manovaTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
  
}


.manovaCheckErrors <- function(dataset, options) {
  
  # Check if results can be computed
  if ((length(options$dependent) < 2) | length(options$fixedFactors) == 0)
    return("No variables")
  
  # Error Check 1: Number of levels of the variables
  .hasErrors(
    dataset              = dataset,
    perform              = "run",
    type                 = "factorLevels",
    factorLevels.target  = options$fixedFactors,
    factorLevels.amount  = "< 2",
    exitAnalysisIfErrors = TRUE
  )
  
  # Error check 2: < 2 observations for a level of a variable
  for (depVariable in options$dependent) {

    depColumn <- dataset[[.v(depVariable)]]
    depData   <- depColumn[!is.na(depColumn)]
    
    for (facVariable in options$fixedFactors) {
    
      facColumn <- dataset[[.v(facVariable)]]
      facData   <- facColumn[!is.na(facColumn)]
      levels <- levels(facData)
      
      for (level in levels) {
        .hasErrors(
          dataset              = depData[depData == level],
          perform              = "run",
          type                 = "observations",
          observations.amount  = "< 2",
          exitAnalysisIfErrors = TRUE
        )
      }
    }
  }
}