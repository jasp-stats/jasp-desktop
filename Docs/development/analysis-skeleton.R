#
# Copyright (C) 2018 University of Amsterdam
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

# Main function ----
MockAnalysis <- function(jaspResults, dataset, options) {
  # Set title
  jaspResults$title <- "Mock Analysis"
  # Init options: add variables to options to be used in the remainder of the analysis
  options <- .mockInitOptions(jaspResults, options)
  # read dataset
  dataset <- .mockReadData(options)
  # error checking
  errors <- .mockErrorHandling(dataset, options)

  # Compute (a list of) results from which tables and plots can be created
  mockResults <- .mockComputeResults(jaspResults, dataset, options)

  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .mockContainerMain( jaspResults, options, mockResults)
  .mockTableSomething(jaspResults, options, mockResults)
  .mockTableSthElse(  jaspResults, options, mockResults)
  .mockPlotSomething( jaspResults, options, mockResults)

  return()
}

# Init functions ----
.mockInitOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis
  options
}

.mockReadData <- function(options) {
  # Read in the dataset using the built-in functions
  if (options$groupvar == "") {
    dataset <- .readDataSetToEnd(columns = variables)
  } else {
    dataset <- .readDataSetToEnd(columns = variables, columns.as.factor = options$groupvar)
  }
  dataset
}

.mockErrorHandling <- function(dataset, options) {
  # See error handling
  # Either it should be like this
  .hasErrors(dataset, "run", type = c('observations', 'variance', 'infinity'),
             all.target = options$variables,
             observations.amount = '< 2',
             exitAnalysisIfErrors = TRUE)

  # Or like this, if you want to do sth with the errors
  errors <- .hasErrors(dataset, "run", message = "short",
    	                 type = c('observations', 'variance', 'infinity'),
                       all.target = options$variables,
                       observations.amount = '< 2')
  errors
}

# Results functions ----
.mockComputeResults <- function(jaspResults, dataset, options) {
  if (is.null(jaspResults[["stateMockResults"]])) {
    mockResults <- .mockResultsHelper(dataset)

    jaspResults[["stateMockResults"]] <- createJaspState(mockResults)
    jaspResults[["stateMockResults"]]$dependOnOptions("variables")

  } else {
    mockResults <- jaspResults[["stateMockResults"]]$object
  }
  mockResults
}

.mockResultsHelper <- function(dataset) {
  # do actual computations
}

# Output functions ----
.mockContainerMain <- function(jaspResults, options, mockResults) {
  if (!is.null(jaspResults[["mockMainContainer"]])) return()
  
  mainContainer <- createJaspContainer("Model fit tables")
  mainContainer$dependOnOptions(c("variables", "someotheroption"))
  
  jaspResults[["mockMainContainer"]] <- mainContainer
}

.mockTableSomething <- function(jaspResults, options, mockResults) {
  if (!is.null(jaspResults[["mockMainContainer"]][["mockTable"]])) return()

  # Below is one way of creating a table
  mockTable <- createJaspTable(title = "Mock Table")
  mockTable$dependOnOptions(c("variables", "someotheroption")) # not strictly necessary because container

  # Bind table to jaspResults
  jaspResults[["mockMainContainer"]][["mockTable"]] <- mockTable

  # Add column info
  mockTable$addColumnInfo(name = "chisq",  title = "\u03a7\u00b2", type = "number", format = "sf:4")
  mockTable$addColumnInfo(name = "pvalue", title = "p",            type = "number", format = "dp:3;p:.001")
  mockTable$addColumnInfo(name = "BF",     title = "Bayes Factor", type = "number", format = "sf:4")
  mockTable$addColumnInfo(name = "sth",    title = "Some Title",   type = "string")

  # Add data per column
  mockTable[["chisq"]]  <- mockResults$column1
  mockTable[["pvalue"]] <- mockResults$column2
  mockTable[["BF"]]     <- mockResults$column3
  mockTable[["sth"]]    <- mockResults$sometext
}

.mockTableSthElse <- function(jaspResults, options, mockResults) {
  if (!is.null(jaspResults[["mockMainContainer"]][["mockTable2"]])) return()
  
  # Below is one way of creating a table
  mockTable2 <- createJaspTable(title = "Mock Table Something Else")
  mockTable2$dependOnOptions(c("variables", "someotheroption"))
  
  # Bind table to jaspResults
  jaspResults[["mockMainContainer"]][["mockTable2"]] <- mockTable2
  
  # Add column info
  mockTable2$addColumnInfo(name = "hallo", title = "Hallo", type = "string")
  mockTable2$addColumnInfo(name = "doei",  title = "Doei",  type = "string")
  
  # Calculate some data from results
  mockSummary <- summary(mockResults$someObject)
  
  # Add data per column. Calculations are allowed here too!
  mockTable2[["hallo"]] <- ifelse(mockSummary$hallo > 1, "Hallo!", "Hello!")
  mockTable2[["doei"]]  <- mockSummary$doei^2
}

.mockPlotSomething <- function(jaspResults, options, mockResults) {
  if (!is.null(jaspResults[["mockPlot"]])) return()

  mockPlot <- createJaspPlot(title = "Mock Plot", height = 320, width = 480)
  mockPlot$dependOnOptions(c("variables", "someotheroption"))
  
  # Bind plot to jaspResults
  jaspResults[["mockPlot"]] <- mockPlot

  mockPlot$plotObject <- plot(mockResults$someObject)
}
