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

# When making changes to this file always mention @koenderks as a 
# reviewer in the Pull Request

classicalAudit <- function(jaspResults, dataset, options, ...){
    
  ### PROCEDURE ###
  .auditClassicalProcedureStage(options, 
                                jaspResults)

  ### PLANNING ###
  .auditClassicalPlanningStage(options, 
                                jaspResults)
  
  ### SELECTION ###
  if(!options[["samplingChecked"]] || 
      jaspResults[["planningContainer"]]$getError()) 
      return() # Stop if "To Selection" is not pressed

  .auditClassicalSelectionStage(options, 
                                jaspResults)
  
  ### EXECUTION ###
  .auditExecutionStage(options,
                        jaspResults)

  ### EVALUATION ###
  if(!options[["evaluationChecked"]] || 
      jaspResults[["planningContainer"]]$getError() || 
      jaspResults[["selectionContainer"]]$getError()) 
      return()  # Stop if "To Evaluation" is not pressed

  .auditClassicalEvaluationStage(options, 
                                  jaspResults)

  ### CONCLUSION ###
  .classicalConclusionStage(options, 
                            jaspResults)

  ### BADGES ###
  badgeReady <- options[["auditResult"]] != ""

  .auditBadgeSection(options,
                     type = "workflow",
                     stateContainer = NULL,
                     jaspResults, 
                     badgeReady, 
                     position = 6)
}

.auditClassicalProcedureStage <- function(options, 
                                          jaspResults){
  # Extract the record number and book value columns
  dataset <- .auditReadDataProcedure(options, 
                                     jaspResults)
  # Error checks for infinity, zero variance, and missing values
  .auditProcedureErrorChecks(options, 
                             dataset)
  # Deduct the nessecary values from the input options
  planningOptions <- .auditPlanningOptions(options,
                                           jaspResults,
                                           rawData = TRUE)

  # Create the procedure paragraph
  .auditExplanatoryTextProcedure(options, 
                                 planningOptions, 
                                 jaspResults, 
                                 positionInContainer = 1)

  # --- TABLES

  # Create state for the table numbers
  .auditCreateTableNumber(jaspResults)
  
  # Create a table containing descriptive statistics for the book values
  .auditBookValueDescriptiveTable(options, 
                                  planningOptions,
                                  jaspResults,
                                  positionInContainer = 2)

  # ---  
  
  # --- PLOTS

    # Create state for the figure numbers
  .auditCreateFigureNumber(jaspResults)

  # Create a plot of the population book values (if the user wants it)
  .auditBookValueDistributionPlot(dataset, 
                                  options,
                                  planningOptions, 
                                  jaspResults, 
                                  positionInContainer = 3)

  # ---

  # Create the audit risk model paragraph
  .auditRiskModelParagraph(options, 
                           planningOptions, 
                           jaspResults, 
                           position = 2)
}

.auditClassicalPlanningStage <- function(options, 
                                         jaspResults){

  # We're doing a frequentist analysis
  type <- "frequentist"

  # Deduct the nessecary values from the input options
  planningOptions <- .auditPlanningOptions(options,
                                           jaspResults,
                                           rawData = TRUE)

  # Check if the options have valid values for running the analysis
  ready <- .auditPlanningReady(options, 
                               planningOptions)

  # Create the container that holds the planning output
  planningContainer <- .auditPlanningGetContainer(jaspResults, 
                                                  position = 3)

  # Perfrom early error checks
  .auditPlanningErrorChecks(options, 
                            planningOptions, 
                            planningContainer, 
                            ready)

  # Get the planning state if it exists, otherwise make one
  planningState <- .auditPlanningState(options, 
                                       planningOptions, 
                                       planningContainer, 
                                       ready, 
                                       type)

  # Create explanatory text for the planning
  .auditExplanatoryTextPlanning(options, 
                                planningOptions, 
                                planningState, 
                                planningContainer, 
                                ready, 
                                type, 
                                positionInContainer = 1)

  # --- TABLES

  # Create the summary table
  .auditPlanningSummaryTable(options, 
                             planningOptions, 
                             planningState, 
                             planningContainer, 
                             jaspResults,
                             ready, 
                             type, 
                             positionInContainer = 2)

  # ---

  # --- PLOTS

  # Create the decision analysis plot
  .decisionAnalysisPlot(options, 
                        planningOptions, 
                        planningState, 
                        planningContainer, 
                        jaspResults,
                        ready, 
                        type, 
                        positionInContainer = 3)

  # Create the implied sampling distribution plot
  .samplingDistributionPlot(options, 
                            planningOptions, 
                            planningState, 
                            planningContainer, 
                            jaspResults,
                            ready, 
                            positionInContainer = 5)

  # ---
}

.auditClassicalSelectionStage <- function(options, 
                                          jaspResults){

  # Create the container that holds the selection output
  selectionContainer <- .auditSelectionGetContainer(jaspResults, 
                                                    position = 4)
  # Read in additional variables
  dataset <- .auditAddSelectionColumns(options, jaspResults)

  # Import options and results from the planning stage 
  planningOptions <- .auditPlanningOptions(options,
                                           jaspResults,
                                           rawData = TRUE)

  planningContainer <- jaspResults[["planningContainer"]]
  planningState <- planningContainer[["planningState"]]$object

  if(is.null(planningState))
    return()

  # Perform the sampling
  selectionState <- .auditSelectionState(dataset,
                                         options, 
                                         planningState, 
                                         selectionContainer)

  # Create explanatory text for the selection
  .auditExplanatoryTextSelection(options, 
                                 planningOptions,
                                 planningState, 
                                 selectionState,
                                 selectionContainer, 
                                 positionInContainer = 1)

  # --- TABLES

  # Create a table containing information about the selection process
  .auditSelectionSummaryTable(options, 
                              planningOptions,
                              planningState,
                              selectionState,
                              selectionContainer,
                              jaspResults, 
                              positionInContainer = 2)
  
  # Create a table containing descriptive statistics of the sample
  .auditSelectionDescriptivesTable(options, 
                                   selectionState, 
                                   selectionContainer,
                                   jaspResults,
                                   positionInContainer = 3)
  
  # Create a table displaying the selection
  .auditSelectionSampleTable(options,
                             selectionState,
                             selectionContainer,
                             jaspResults,
                             positionInContainer = 4) 

  # ---
}

.auditClassicalEvaluationStage <- function(options, 
                                           jaspResults){

  # Create the container that holds the selection output
  evaluationContainer <- .auditEvaluationGetContainer(jaspResults, 
                                                      position = 5)
  # Read in additional variables
  dataset <- .auditAddEvaluationColumns(options,
                                        jaspResults)
  
  # See if analysis can be run
  ready <- options[["auditResult"]] != ""

  # Extract only the sample
  if(ready)
    sample <- subset(dataset, dataset[, .v(options[["sampleFilter"]])] != 0)

  # Import options and results from the planning and selection stages 
  planningOptions <- .auditPlanningOptions(options,
                                           jaspResults,
                                           rawData = TRUE)

  planningContainer <- jaspResults[["planningContainer"]]
  planningState <- planningContainer[["planningState"]]$object

  selectionContainer <- jaspResults[["selectionContainer"]]
  selectionState <- selectionContainer[["selectionState"]]$object

  if(is.null(selectionState))
    return()
    
  # Perform the evaluation
  evaluationState <- .auditEvaluationState(options,
                                           planningOptions,
                                           sample,
                                           evaluationContainer)

  # Create explanatory text for the evaluation
  .auditExplanatoryTextEvaluation(options,
                                  planningOptions,
                                  planningState,
                                  evaluationContainer, 
                                  positionInContainer = 1)

  # --- TABLES

  # Create a table containing information about the evaluation process  
  .auditEvaluationSummaryTable(options,
                               planningOptions,
                               evaluationState,
                               evaluationContainer,
                               jaspResults,
                               positionInContainer = 2)

  # ---

  # --- PLOTS

  # Create a plot containing evaluation information
  .auditEvaluationInformationPlot(options,
                                  planningOptions,
                                  evaluationState,
                                  evaluationContainer,
                                  jaspResults,
                                  positionInContainer = 3)

  # Create a plot containing the correlation between the book and audit values
  .auditCorrelationPlot(options,
                        planningOptions,
                        sample,
                        evaluationContainer,
                        jaspResults,
                        positionInContainer = 5)

  # ---
}

.classicalConclusionStage <- function(options, 
                                      jaspResults){

  if(!is.null(jaspResults[["conclusionContainer"]]) || 
      options[["auditResult"]] == "") 
    return()

  # Explanatory text for conclusion
  if(options[["explanatoryText"]]){

    # Import options and results from the planning and selection stages 
    planningOptions <- .auditPlanningOptions(options,
                                            jaspResults,
                                            rawData = TRUE)

    # Import result of analysis from jaspResults
    evaluationContainer <- jaspResults[["evaluationContainer"]]
    evaluationState <- evaluationContainer[["evaluationState"]]$object

    if(is.null(evaluationState))
      return()

    # Create a container for the conclusion
    conclusionContainer <- createJaspContainer(title = "<u>Conclusion</u>")
    conclusionContainer$position <- 5
    conclusionContainer$dependOn(optionsFromObject = evaluationContainer)
    conclusionContainer$dependOn(options = "explanatoryText")

    # Produce relevant terms conditional on the analysis result
    conclusion <- evaluationState[["conclusion"]]

    if(conclusion == "Approve population"){

      relative <- "below"
      containsMisstatement <- "<b>no material misstatement</b>"

    } else {

      relative <- "above"
      containsMisstatement <- "<b>material misstatement</b>"

    }

    message <- paste0("To approve these data, a <b>", 
                      planningOptions[["confidenceLabel"]],
                      "</b> upper confidence bound on the population proportion of full errors had to be determined to be
                      lower than materiality, in this case <b>", 
                      planningOptions[["materialityLabel"]],
                      "</b>. For the current data, the confidence bound is <b>", 
                      relative ,"</b> materiality. The conclusion for 
                      these data is that the data contain ", 
                      containsMisstatement,
                      ".")

    conclusionContainer[["conclusionParagraph"]] <- createJaspHtml(message, "p")
    conclusionContainer[["conclusionParagraph"]]$position <- 1
    conclusionContainer[["conclusionParagraph"]]$dependOn(optionsFromObject = conclusionContainer)

    # Finsh conclusion
    jaspResults[["conclusionContainer"]] <- conclusionContainer
  }
}
