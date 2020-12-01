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

################################################################################
################## The Audit Workflow ##########################################
################################################################################

.jfa.workflow.analysis <- function(options, jaspResults){
  
  ### PROCEDURE STAGE ###
  .jfa.procedure.stage(options, jaspResults)
  ready <- .jfa.readyForNextStage.check(options, jaspResults, stage = "procedure")
  if(!ready) return() # Stop if not enough information is given to quantify the sampling objectives
  
  ### PLANNING STAGE ###
  .jfa.planning.stage(options, jaspResults, workflow = TRUE)
  
  ready <- .jfa.readyForNextStage.check(options, jaspResults, stage = "planning")
  if(!ready) return() # Stop if "To Selection" is not pressed
  
  ### SELECTION STAGE ###
  .jfa.selection.stage(options, jaspResults, workflow = TRUE)
  
  ### EXECUTION STAGE ###
  .jfa.execution.stage(options, jaspResults)
  
  ready <- .jfa.readyForNextStage.check(options, jaspResults, stage = "execution")
  if(!ready) return() # Stop if "To Evaluation" is not pressed
  
  ### EVALUATION STAGE ###
  .jfa.evaluation.stage(options, jaspResults, workflow = TRUE)
  
  ### CONCLUSION STAGE ###
  .jfa.conclusion.stage(options, jaspResults)
}

################################################################################
################## The Separate Stages of the Audit Workflow ###################
################################################################################

#####################################
######### PROCEDURE STAGE ###########
#####################################

.jfa.procedure.stage <- function(options, jaspResults){
  
  # Extract the record number and book value columns
  dataset <- .jfa.dataset.read(options, jaspResults, stage = "procedure")
  
  # Check for errors due to incompatible options (variables)
  .jfa.inputOptions.check(options, dataset, parentContainer = NULL, stage = "procedure")
  
  # Deduce the necessary values from the input options
  planningOptions <- .jfa.inputOptions.collect(options, dataset, jaspResults, stage = "planning",
                                               rawData = TRUE)
  
  # Create the procedure paragraph
  .jfa.explanatoryText.add(options, planningOptions, stageContainer = NULL, stageState = NULL,
                           jaspResults, stage = "procedure", positionInContainer = 1)
  
  # Create the audit risk model paragraph
  .jfa.auditRiskModel.add(options, jaspResults, position = 2)
  
  # --- TABLES
  
  .jfa.tableNumber.add(jaspResults) # Initialize table numbers
  
  # Create a table containing descriptive statistics for the book values
  .jfa.bookvalueDescriptives.table(options, planningOptions, jaspResults, positionInContainer = 2)
  
  # --- PLOTS
  
  .jfa.figureNumber.add(jaspResults) # Initialize figure numbers
  
  # Create a plot of the population book values (if the user wants it)
  .jfa.bookvalueDescriptives.plot(options, dataset, jaspResults, positionInContainer = 3)
  
}

#####################################
######### PLANNING STAGE ############
#####################################

.jfa.planning.stage <- function(options, jaspResults, workflow){
  
  if(workflow){
    
    .jfa.criticalTransactions.init(options, jaspResults)
    
    # Deduce the necessary values from the input options
    planningOptions <- .jfa.inputOptions.collect(options, dataset = NULL, jaspResults,
                                                 stage = "planning", rawData = TRUE)
    
  } else if(!workflow){
    
    .jfa.tableNumber.add(jaspResults)  # Initialize table numbers
    .jfa.figureNumber.add(jaspResults) # Initialize figure numbers
    
    # Deduce the necessary values from the input options
    planningOptions <- .jfa.inputOptions.collect(options, dataset = NULL, jaspResults,
                                                 stage = "planning", rawData = FALSE)
    
    # Create the procedure paragraph
    .jfa.explanatoryText.add(options, planningOptions, stageContainer = NULL, stageState = NULL,
                             jaspResults, stage = "procedure", positionInContainer = 1)
    
    # Create the audit risk model paragraph
    .jfa.auditRiskModel.add(options, jaspResults, position = 2)
  }
  
  # Check if the options have valid values for running the analysis
  ready <- .jfa.ready.check(options, planningOptions, stage = "planning")
  
  if(!(options[['performanceMateriality']] || options[["minimumPrecision"]]))
    return() # Stop if no sampling objective is selected
  
  # Create the container that holds the planning output
  planningContainer <- .jfa.stageContainer.add(jaspResults, stage = "planning", position = 3)
  
  # Perfrom early error checks
  .jfa.inputOptions.check(options, dataset = NULL, planningContainer, stage = "planning",
                          ready, planningOptions)
  
  # Get the planning state if it exists, otherwise make one
  planningState <- .jfa.planning.state(options, planningOptions, planningContainer, ready, jaspResults)
  
  # Create explanatory text for the planning
  .jfa.explanatoryText.add(options, planningOptions, planningContainer, planningState, jaspResults,
                           stage = "planning", positionInContainer = 1)
  
  # --- TABLES
  
  # Create the summary table
  .jfa.planning.table(options, planningOptions, planningState, planningContainer, jaspResults,
                      ready, positionInContainer = 2)
  
  if(options[["bayesianAnalysis"]]){
    # Create the implicit sample table
    .jfa.implicitSample.table(options, planningState, planningContainer, jaspResults,
                              ready, positionInContainer = 3)
    
    # Cerate the prior and posterior statistics table
    .jfa.distributionStatistics.table(options, planningOptions, planningState, planningContainer,
                                      jaspResults, ready, positionInContainer = 4, stage = "planning")
  }
  
  # --- PLOTS
  
  # Create the sample size comparison plot
  .jfa.sampleSize.plot(options, planningOptions, planningState, planningContainer, jaspResults,
                       ready, positionInContainer = 5)
  
  if(!options[["bayesianAnalysis"]]){
    # Create the implied distribution plot
    .jfa.samplingDistribution.plot(options, planningOptions, planningState, planningContainer,
                                   jaspResults, ready, positionInContainer = 7)
  } else if(options[["bayesianAnalysis"]]){
    # Create the prior and expected posterior plot
    .jfa.prior.plot(options, planningOptions, planningState, planningContainer,
                    jaspResults, ready, positionInContainer = 7)
  }
}

#####################################
######### SELECTION STAGE ###########
#####################################

.jfa.selection.stage <- function(options, jaspResults, workflow){
  
  if(workflow){
    
    # Create the container that holds the selection output
    selectionContainer <- .jfa.stageContainer.add(jaspResults, stage = "selection-workflow", position = 4)
    
    # Read in additional variables
    dataset <- .jfa.selectionResult.add(options, jaspResults)
    
    # Import options and results from the planning stage
    selectionOptions <- .jfa.inputOptions.collect(options, dataset, jaspResults, stage = "planning", rawData = TRUE)
    
    planningContainer   <- jaspResults[["planningContainer"]]
    planningState       <- planningContainer[["planningState"]]$object
    
    error <- .jfa.inputOptions.check(options, dataset, selectionContainer, stage = "selection-workflow")
    if(error) return() # Quit on errors
    
    if(is.null(planningState)) return() # Quit if no planning was done
    
    # Perform the sampling
    selectionState <- .jfa.selection.state(options, dataset, planningState, selectionContainer)
    
  } else if(!workflow){
    
    .jfa.figureNumber.add(jaspResults) # Initialize figure numbers
    .jfa.tableNumber.add(jaspResults) # Initialize table numbers
    
    # Create a custom container for the selection analysis
    selectionContainer <- .jfa.stageContainer.add(jaspResults, stage = "selection", position = 1)
    
    # Read in the relevant variables from the data set
    dataset <- .jfa.dataset.read(options, jaspResults, stage = "selection")
    
    # Deduce relevant quantities from input options
    selectionOptions <- .jfa.inputOptions.collect(options, dataset, jaspResults, stage = "selection")
    
    # Check for errors due to incompatible options
    error <- .jfa.inputOptions.check(options, dataset, selectionContainer, stage = "selection", parentOptions = selectionOptions)
    if(error) return() # Quit on errors
    
    options[["materiality"]] <- ifelse(options[["selectionType"]] == "musSampling", yes = "materialityAbsolute", no = "materialityRelative")
    
    # Create a planning state
    planningState <- .jfa.previousStage.calculation(options, stage = "selection")
    
    # Perform error checks
    .jfa.inputOptions.check(options, dataset, parentContainer = NULL, stage = "procedure")
    
    # Perform the sampling
    selectionState <- .jfa.selection.calculation(options, dataset, planningState, selectionContainer)
    
    # Add the sample indicator to the data
    .jfa.selectionIndicator.add(options, selectionOptions, selectionState, jaspResults)
    
  }
  
  # Create explanatory text for the selection
  .jfa.explanatoryText.add(options, selectionOptions, selectionContainer, selectionState, jaspResults, stage = "selection", positionInContainer = 1, prevState = planningState)
  
  # --- TABLES
  
  # Create a table containing information about the selection process
  .jfa.selection.table(options, dataset, selectionOptions, planningState, selectionState, selectionContainer, jaspResults, positionInContainer = 2)
  
  # Create a table containing information about the monetary interval selection
  .jfa.selectionInterval.table(options, dataset, selectionOptions, planningState, selectionContainer, selectionState, jaspResults, positionInContainer = 4)
  
  # Create a table containing descriptive statistics of the sample
  .jfa.selectionDescriptives.table(options, selectionState, selectionContainer, jaspResults, positionInContainer = 5)
  
  # Create a table displaying the selection
  .jfa.selectionSample.table(options, selectionOptions, selectionState, selectionContainer, jaspResults, positionInContainer = 6)
}

#####################################
######### EXECUTION STAGE ###########
#####################################

.jfa.execution.stage <- function(options, jaspResults){
  
  if(options[["pasteVariables"]]){
    # Add the two computed colums to the data set
    planningOptions <- .jfa.inputOptions.collect(options, dataset = NULL, jaspResults, stage = "planning", rawData = TRUE)
    selectionState <- .jfa.selection.state(options, dataset, jaspResults[["planningState"]], jaspResults[["selectionContainer"]])
    
    selectionState                <- data.frame(selectionState)
    dataset                       <- .readDataSetToEnd(columns.as.numeric = options[["recordNumberVariable"]])
    sampleFilter                  <- rep(0, planningOptions[["populationSize"]])
    rowNumber                     <- selectionState[["rowNumber"]]
    sampleFilter[rowNumber]       <- selectionState[["count"]]
    sampleFilter                  <- as.numeric(sampleFilter)
    auditDataVariable             <- rep(NA, planningOptions[["populationSize"]])
    auditDataVariable[options[["performAudit"]][[1]]$rowIndices] <- options[["performAudit"]][[1]]$values
    
    if(is.null(jaspResults[["sampleFilter"]]))
      jaspResults[["sampleFilter"]] <- createJaspColumn(columnName = options[["sampleFilter"]], dependencies = "sampleFilter")
    if(is.null(jaspResults[["variableName"]]))
      jaspResults[["variableName"]] <- createJaspColumn(columnName = options[["variableName"]], dependencies = "variableName")
    
    jaspResults[["sampleFilter"]]$setScale(sampleFilter)
    jaspResults[["variableName"]]$setScale(auditDataVariable)
    
  }
}

#####################################
######### EVALUATION STAGE ##########
#####################################

.jfa.evaluation.stage <- function(options, jaspResults, workflow){
  
  if(workflow){
    
    # Create the container that holds the selection output
    evaluationContainer <- .jfa.stageContainer.add(jaspResults, stage = "evaluation-workflow", position = 5)
    
    # Read in additional variables
    dataset <- .jfa.evaluationResult.add(options, jaspResults)
    
    # See if analysis can be run
    ready <- options[["auditResult"]] != ""
    
    # Extract only the sample
    if(ready)
      sample <- subset(dataset, dataset[, .v(options[["sampleFilter"]])] != 0)
    
    # Import options and results from the planning and selection stages
    evaluationOptions <- .jfa.inputOptions.collect(options, dataset, jaspResults, stage = "planning", rawData = TRUE)
    
    planningContainer <- jaspResults[["planningContainer"]]
    planningState <- planningContainer[["planningState"]]$object
    
    selectionContainer <- jaspResults[["selectionContainer"]]
    selectionState <- selectionContainer[["selectionState"]]$object
    
    if(is.null(selectionState)) return()
    
    # Perform the evaluation
    evaluationState <- .jfa.evaluation.state(options, sample, evaluationOptions, evaluationContainer, selectionState)
    
    # Create explanatory text for the evaluation
    .jfa.explanatoryTextEvaluation.add(options, evaluationOptions, planningState, selectionState, evaluationContainer, positionInContainer = 1)
    
  } else if(!workflow){
    
    .jfa.tableNumber.add(jaspResults) # Initialize table numbers
    .jfa.figureNumber.add(jaspResults) # Initialize figure numbers
    
    # Create an empty container for the evaluation analysis
    evaluationContainer <- .jfa.stageContainer.add(jaspResults, stage = "evaluation", position = 1)
    
    # Read in the relevant variables from the data set
    sample <- .jfa.dataset.read(options, jaspResults, stage = "evaluation")
    
    # Remove the critical transactions if wanted
    if(options[["flagCriticalTransactions"]] && options[["handleCriticalTransactions"]] == "remove" && options[["monetaryVariable"]] != "")
      sample <- subset(sample, sample[, .v(options[["monetaryVariable"]])] >= 0)
    
    # Check for errors due to incompatible options
    error <- .jfa.inputOptions.check(options, sample, evaluationContainer, stage = "evaluation")
    if(error) return()
    
    # Deduce relevant quantities from input options
    evaluationOptions <- .jfa.inputOptions.collect(options, sample, jaspResults,
                                                   stage = "evaluation")
    
    # Create the evaluation state that holds the results
    evaluationState <- .jfa.evaluationAnalysis.state(options, sample, evaluationOptions,
                                                     evaluationContainer)
    
    # Backwards create a planningstate and a selectionstate
    planningState <- .jfa.previousStagePlanning.state(options, sample, evaluationOptions)
    selectionState <- .jfa.previousStage.calculation(options, stage = "evaluation")
    
    # Create explanatory text for the evaluation
    .jfa.explanatoryTextEvaluation.add(options, evaluationOptions, planningState, selectionState, evaluationContainer, positionInContainer = 1)
    
  }
  
  # --- TABLES
  
  # Create a table containing information about the evaluation process
  .jfa.evaluation.table(options, evaluationOptions, evaluationState, evaluationContainer, jaspResults, positionInContainer = 2)
  
  if(options[["bayesianAnalysis"]]){
    # Create a table containing assumption checks
    .jfa.assumptionCheck.table(options, sample, evaluationContainer, jaspResults, positionInContainer = 3)
    
    # Create a table containing information regarding the prior and posterior
    .jfa.distributionStatistics.table(options, evaluationOptions, evaluationState, evaluationContainer, jaspResults, ready = NULL, positionInContainer = 4, stage = "evaluation")
  }
  
  # --- PLOTS
  
  if(options[["bayesianAnalysis"]]){
    # Create a plot containing the prior and posterior distribution
    .jfa.posterior.plot(options, evaluationOptions, planningState, evaluationState, evaluationContainer, jaspResults, positionInContainer = 5)
  }
  
  # Create a plot containing evaluation information
  .jfa.samplingObjectives.plot(options, evaluationOptions, evaluationState, evaluationContainer, jaspResults, positionInContainer = 7)
  
  # Create a plot containing the correlation between the book and audit values
  if(options[["variableType"]] == "variableTypeAuditValues")
    .jfa.correlation.plot(options, sample, evaluationOptions, evaluationContainer, jaspResults, positionInContainer = 9)
}

#####################################
######### CONCLUSION STAGE ##########
#####################################

.jfa.conclusion.stage <- function(options, jaspResults){
  
  if(!is.null(jaspResults[["conclusionContainer"]]) || options[["auditResult"]] == "")
    return()
  
  evaluationContainer <- jaspResults[["evaluationContainer"]]
  evaluationState <- evaluationContainer[["evaluationState"]]$object
  
  # Create a container for the conclusion
  conclusionContainer <- createJaspContainer(title = gettext("<u>Conclusion</u>"))
  conclusionContainer$position <- 5
  conclusionContainer$dependOn(optionsFromObject = evaluationContainer)
  conclusionContainer$dependOn(options = "explanatoryText")
  jaspResults[["conclusionContainer"]] <- conclusionContainer
  
  .jfa.explanatoryText.add(options, stageOptions = NULL, stageContainer = NULL, stageState = NULL, jaspResults, stage = "conclusion", positionInContainer = 1)
  
  .jfa.additionalSamples.table(options, jaspResults, positionInContainer = 2)
}

################################################################################
################## Common functions for figure and table numbers ###############
################################################################################

.jfa.figureNumber.add <- function(jaspResults){
  # Initialize figure numbers
  jaspResults[["figNumber"]] <- createJaspState(0)
}

.jfa.tableNumber.add <- function(jaspResults){
  # Initialize table numbers
  jaspResults[["tabNumber"]] <- createJaspState(0)
}

.jfa.tableNumber.update <- function(jaspResults){
  # Update table numbers + 1
  currentNumber <- jaspResults[["tabNumber"]]$object
  jaspResults[["tabNumber"]] <- createJaspState(currentNumber + 1)
}

.jfa.figureNumber.update <- function(jaspResults){
  # Update figure numbers + 1
  currentNumber <- jaspResults[["figNumber"]]$object
  jaspResults[["figNumber"]] <- createJaspState(currentNumber + 1)
}

################################################################################
################## Common functions for reading data and options ###############
################################################################################

.jfa.variable.read <- function(options, varType){
  if(varType == "recordNumber"){
    # Read in the transaction ID's
    recordNumberVariable <- options[["recordNumberVariable"]]
    if(recordNumberVariable == "")
      recordNumberVariable <- NULL
    return(recordNumberVariable)
  } else if(varType == "monetary"){
    # Read in the book values
    monetaryVariable <- options[["monetaryVariable"]]
    if(monetaryVariable == "")
      monetaryVariable <- NULL
    return(monetaryVariable)
  } else if(varType == "auditResult"){
    # Read in the audit result
    auditResult <- options[["auditResult"]]
    if(auditResult == "")
      auditResult <- NULL
    return(auditResult)
  } else if(varType == "sampleCounter"){
    # Read in the sample counter
    sampleCounter <- options[["sampleCounter"]]
    if(sampleCounter == "")
      sampleCounter <- NULL
    return(sampleCounter)
  } else if(varType == "ranking"){
    # Read in the ranking variable
    rankingVariable <- options[["rankingVariable"]]
    if(rankingVariable == "")
      rankingVariable <- NULL
    return(rankingVariable)
  } else if(varType == "additional"){
    # Read in additional variables
    additionalVariables <- unlist(options[["additionalVariables"]])
    return(additionalVariables)
  } else if(varType == "critical"){
    criticalVariable <- options[["criticalTransactions"]]
    return(criticalVariable)
  }
}

.jfa.dataset.read <- function(options, jaspResults, stage){
  
  if(stage == "procedure"){
    
    recordNumberVariable  <- .jfa.variable.read(options, varType = "recordNumber")
    monetaryVariable      <- .jfa.variable.read(options, varType = "monetary")
    
    analysisOptions <- list()
    
    if(!is.null(recordNumberVariable)){
      dataset <- .readDataSetToEnd(columns.as.factor = recordNumberVariable)
      dataset[, .v(recordNumberVariable)] <- as.character(levels(dataset[, .v(recordNumberVariable)]))
      
      analysisOptions[["populationSize"]] <- nrow(dataset)
      analysisOptions[["uniqueN"]] <- length(unique(dataset[, .v(options[["recordNumberVariable"]])]))
      
      if(!is.null(monetaryVariable)){
        dataset <- .readDataSetToEnd(columns.as.numeric = monetaryVariable, columns.as.factor = recordNumberVariable)
        dataset[, .v(recordNumberVariable)] <- as.character(levels(dataset[, .v(recordNumberVariable)]))
        
        monetaryColumn <- dataset[, .v(monetaryVariable)]
        analysisOptions[["populationValue"]] <- sum(monetaryColumn, na.rm = TRUE)
        analysisOptions[["absPopulationValue"]] <- sum(abs(monetaryColumn), na.rm = TRUE)
        analysisOptions[["meanValue"]] <- mean(monetaryColumn, na.rm = TRUE)
        analysisOptions[["sigmaValue"]] <- sd(monetaryColumn, na.rm = TRUE)
        analysisOptions[["quantileValue"]] <- as.numeric(quantile(monetaryColumn, probs = c(0.25, 0.50, 0.75), na.rm = TRUE))
        analysisOptions[["ready"]] <- TRUE
      } else {
        analysisOptions[["populationValue"]] <- 0.01
        analysisOptions[["ready"]] <- ifelse(options[["materiality"]] == "materialityRelative",
                                             yes = TRUE, no = FALSE)
      }
    } else {
      dataset <- NULL
      analysisOptions[["populationSize"]] <- 0
      analysisOptions[["uniqueN"]] <- 0
      analysisOptions[["populationValue"]] <- 0.01
      analysisOptions[["ready"]] <- FALSE
    }
    
    materiality <- ifelse(options[["materiality"]] == "materialityRelative",
                          yes = options[["materialityPercentage"]],
                          no = options[["materialityValue"]])
    
    if(materiality == 0)
      analysisOptions[["ready"]] <- FALSE
    
    jaspResults[["procedureOptions"]] <- createJaspState(analysisOptions)
    jaspResults[["procedureOptions"]]$dependOn(c("recordNumberVariable",
                                                 "monetaryVariable",
                                                 "materiality",
                                                 "materialityPercentage",
                                                 "materialityValue"))
    return(dataset)
    
  } else if(stage == "selection"){
    recordNumberVariable  <- .jfa.variable.read(options, varType = "recordNumber")
    monetaryVariable      <- .jfa.variable.read(options, varType = "monetary")
    rankingVariable       <- .jfa.variable.read(options, varType = "ranking")
    additionalVariables   <- .jfa.variable.read(options, varType = "additional")
    variables             <- c(recordNumberVariable, monetaryVariable, rankingVariable, additionalVariables)
  } else if(stage == "evaluation"){
    recordNumberVariable  <- .jfa.variable.read(options, varType = "recordNumber")
    monetaryVariable      <- .jfa.variable.read(options, varType = "monetary")
    auditResult           <- .jfa.variable.read(options, varType = "auditResult")
    sampleCounter         <- .jfa.variable.read(options, varType = "sampleCounter")
    variables             <- c(recordNumberVariable, monetaryVariable, auditResult, sampleCounter)
  }
  
  if(!is.null(variables)){
    dataset <- .readDataSetToEnd(columns.as.factor = recordNumberVariable, columns.as.numeric = variables[which(variables != recordNumberVariable)])
    dataset[, .v(recordNumberVariable)] <- as.character(dataset[, .v(recordNumberVariable)])
    if(stage == "evaluation" && !is.null(sampleCounter)) # Apply sample filter
      dataset <- subset(dataset, dataset[, .v(options[["sampleCounter"]])] > 0)
    return(dataset)
  } else {
    return(NULL)
  }
}

.jfa.inputOptions.collect <- function(options, dataset, jaspResults, stage, rawData = FALSE){
  
  inputOptions <- list()
  
  if(stage == "planning"){
    
    inputOptions[["valuta"]] <- base::switch(options[["valuta"]],
                                             "euroValuta" = "\u20AC",
                                             "dollarValuta" = "\u0024",
                                             "otherValuta" = options[["otherValutaName"]])
    
    inputOptions[["confidence"]] <- options[["confidence"]]
    inputOptions[["confidenceLabel"]] <- paste0(round(options[["confidence"]] * 100, 2), "%")
    
    if(!rawData){
      
      inputOptions[["populationSize"]] <- options[["populationSize"]]
      inputOptions[["populationValue"]] <- ifelse(options[["populationValue"]] == 0,
                                                  yes = 0.01,
                                                  no = options[["populationValue"]])
      
    } else {
      
      procedureOptions <- jaspResults[["procedureOptions"]]$object
      inputOptions[["populationSize"]] <- procedureOptions[["populationSize"]]
      inputOptions[["populationValue"]] <- procedureOptions[["populationValue"]]
      
    }
    
    inputOptions[["absRel"]] <- ifelse(options[["materiality"]] == "materialityRelative",
                                       yes = gettext("<b>percentage</b>"),
                                       no = gettext("<b>amount</b>"))
    
    inputOptions[["materiality"]] <- ifelse(options[["materiality"]] == "materialityRelative",
                                            yes = options[["materialityPercentage"]],
                                            no = options[["materialityValue"]] / inputOptions[["populationValue"]])
    
    inputOptions[["materialityLabel"]] <- ifelse(options[["materiality"]] == "materialityRelative",
                                                 yes = paste0(round(inputOptions[["materiality"]] * 100, 2), "%"),
                                                 no = paste(inputOptions[["valuta"]], format(options[["materialityValue"]],
                                                                                             scientific = FALSE)))
    
    inputOptions[["expectedErrors"]] <- ifelse(options[["expectedErrors"]] == "expectedRelative",
                                               yes = options[["expectedPercentage"]],
                                               no = options[["expectedNumber"]])
    if(options[["expectedErrors"]] == "expectedAbsolute" && options[["materiality"]] == "materialityAbsolute")
      inputOptions[["expectedErrors"]] <- inputOptions[["expectedErrors"]] / inputOptions[["populationValue"]]
    
    inputOptions[["expectedErrorsLabel"]] <- ifelse(options[["expectedErrors"]] == "expectedRelative",
                                                    yes = paste0(round(inputOptions[["expectedErrors"]] * 100, 2), "%"),
                                                    no = options[["expectedNumber"]])
    if(options[["materiality"]] == "materialityAbsolute" && options[["expectedErrors"]] == "expectedAbsolute")
      inputOptions[["expectedErrorsLabel"]] <- paste(inputOptions[["valuta"]], inputOptions[["expectedErrorsLabel"]])
    
    inputOptions[["likelihood"]] <- base::switch(options[["planningModel"]],
                                                 "Poisson" = "poisson",
                                                 "binomial" = "binomial",
                                                 "hypergeometric" = "hypergeometric")
    
    inputOptions[["minimumPrecision"]] <- options[["minimumPrecisionPercentage"]]
    inputOptions[["minimumPrecisionLabel"]] <- paste0(round(inputOptions[["minimumPrecision"]] * 100, 4), "%")
    
  } else if(stage == "selection"){
    
    inputOptions[["valuta"]] <- base::switch(options[["valuta"]],
                                             "euroValuta" = "\u20AC",
                                             "dollarValuta" = "\u0024",
                                             "otherValuta" = options[["otherValutaName"]])
    inputOptions[["populationSize"]] <- ifelse(is.null(dataset),
                                               yes = 0,
                                               no = nrow(dataset))
    if(options[["monetaryVariable"]] != "" && options[["recordNumberVariable"]] != "")
      inputOptions[["populationValue"]] <- sum(dataset[, .v(options[["monetaryVariable"]])])
    
  } else if(stage == "evaluation"){
    
    confidence <- options[["confidence"]]
    confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")
    populationSize <- options[["populationSize"]]
    
    populationValue <- options[["populationValue"]]
    if(populationValue == 0)
      populationValue <- 0.01
    
    materiality <- ifelse(options[["materiality"]] == "materialityRelative",
                          yes = options[["materialityPercentage"]],
                          no = options[["materialityValue"]] / populationValue)
    materialityLabel <- ifelse(options[["materiality"]] == "materialityRelative",
                               yes = paste0(round(materiality * 100, 2), "%"),
                               no = paste("$", format(options[["materialityValue"]], scientific = FALSE)))
    expectedErrors <- ifelse(options[["expectedErrors"]] == "expectedRelative",
                             yes = options[["expectedPercentage"]],
                             no = options[["expectedNumber"]] / populationValue)
    likelihood <- base::switch(options[["estimator"]],
                               "betaBound" = "binomial",
                               "gammaBound" = "poisson",
                               "betabinomialBound" = "hypergeometric")
    
    inputOptions[["materiality"]] <- materiality
    inputOptions[["materialityLabel"]] <- materialityLabel
    inputOptions[["populationSize"]] <- populationSize
    inputOptions[["populationValue"]] <- populationValue
    inputOptions[["valuta"]] <- "$"
    inputOptions[["confidence"]] <- confidence
    inputOptions[["confidenceLabel"]] <- confidenceLabel
    inputOptions[["expectedErrors"]] <- expectedErrors
    inputOptions[["likelihood"]] <- likelihood
    inputOptions[["minimumPrecision"]] <- options[["minimumPrecisionPercentage"]]
    
  }
  return(inputOptions)
}

.jfa.previousStage.calculation <- function(options, stage){
  if(stage == "selection"){
    state <- list("sampleSize" = options[["sampleSize"]])
  } else if(stage == "evaluation"){
    state <- data.frame(count = 1)
  }
  return(state)
}

################################################################################
################## Common functions for containers #############################
################################################################################

.jfa.stageContainer.add <- function(jaspResults, stage, position = 1){
  
  if(stage == "procedure"){
    
    if(!is.null(jaspResults[["procedureContainer"]]))
      return(jaspResults[["procedureContainer"]])
    
    container <- createJaspContainer(title = gettext("<u>Procedure</u>"))
    container$position <- position
    container$dependOn(options = c("explanatoryText",
                                   "confidence",
                                   "materiality",
                                   "materialityValue",
                                   "materialityPercentage",
                                   "valuta",
                                   "otherValutaName",
                                   "monetaryVariable",
                                   "recordNumberVariable"))
    
    jaspResults[["procedureContainer"]] <- container
    
  } else if(stage == "planning"){
    
    if(!is.null(jaspResults[["planningContainer"]]))
      return(jaspResults[["planningContainer"]])
    
    container <- createJaspContainer(title = gettext("<u>Planning</u>"))
    container$position <- position
    container$dependOn(options = c("IR",
                                   "irCustom",
                                   "CR",
                                   "crCustom",
                                   "confidence",
                                   "populationSize",
                                   "populationValue",
                                   "materiality",
                                   "materialityPercentage",
                                   "materialityValue",
                                   "expectedPercentage",
                                   "expectedErrors",
                                   "expectedNumber",
                                   "planningModel",
                                   "recordNumberVariable",
                                   "monetaryVariable",
                                   "valuta",
                                   "otherValutaName",
                                   "separateKnownAndUnknownMisstatement",
                                   "minimumPrecisionPercentage",
                                   "minimumPrecision",
                                   "performanceMateriality",
                                   "sampleSizeIncrease",
                                   "priorConstructionMethod",
                                   "sampleN",
                                   "sampleK",
                                   "factor",
                                   "pHmin",
                                   "pHplus"))
    
    jaspResults[["planningContainer"]] <- container
    
  } else if(stage == "selection"){
    
    if(!is.null(jaspResults[["selectionContainer"]]))
      return(jaspResults[["selectionContainer"]])
    
    container <- createJaspContainer(title = "")
    container$position <- position
    container$dependOn(options = c("recordNumberVariable",
                                   "monetaryVariable",
                                   "additionalVariables",
                                   "rankingVariable",
                                   "selectionMethod",
                                   "selectionType",
                                   "seed",
                                   "intervalStartingPoint",
                                   "sampleSize",
                                   "valuta",
                                   "otherValutaName",
                                   "shufflePopulationBeforeSampling"))
    
    jaspResults[["selectionContainer"]] <- container
    
  } else if(stage == "selection-workflow"){
    
    prevContainer <- jaspResults[["planningContainer"]]
    prevState <- prevContainer[["planningState"]]$object
    
    if(!is.null(jaspResults[["selectionContainer"]])){
      return(jaspResults[["selectionContainer"]])
    } else if(!is.null(prevState)){
      container <- createJaspContainer(title = gettext("<u>Selection</u>"))
      container$position <- position
      container$dependOn(optionsFromObject = prevContainer,
                         options = c("samplingChecked",
                                     "selectionMethod",
                                     "selectionType",
                                     "seed",
                                     "intervalStartingPoint",
                                     "additionalVariables",
                                     "rankingVariable",
                                     "valuta",
                                     "otherValutaName",
                                     "separateKnownAndUnknownMisstatement",
                                     "shufflePopulationBeforeSampling"))
      
      jaspResults[["selectionContainer"]] <- container
    }
    
  } else if(stage == "evaluation"){
    
    if(!is.null(jaspResults[["evaluationContainer"]]))
      return(jaspResults[["evaluationContainer"]])
    
    container <- createJaspContainer(title = "")
    container$position <- position
    container$dependOn(options = c("recordNumberVariable",
                                   "monetaryVariable",
                                   "auditResult",
                                   "sampleCounter",
                                   "variableType",
                                   "confidence",
                                   "populationSize",
                                   "populationValue",
                                   "IR",
                                   "irCustom",
                                   "CR",
                                   "crCustom",
                                   "expectedErrors",
                                   "expectedPercentage",
                                   "expectedNumber",
                                   "materiality",
                                   "materialityPercentage",
                                   "materialityValue",
                                   "useSumStats",
                                   "nSumStats",
                                   "kSumStats",
                                   "estimator",
                                   "estimator2",
                                   "areaUnderPosterior",
                                   "stringerBoundLtaAdjustment",
                                   "display",
                                   "priorType",
                                   "separateKnownAndUnknownMisstatement",
                                   "minimumPrecisionPercentage",
                                   "minimumPrecision",
                                   "performanceMateriality",
                                   "sampleSizeIncrease",
                                   "priorConstructionMethod",
                                   "sampleN",
                                   "sampleK",
                                   "factor",
                                   "pHmin",
                                   "pHplus",
                                   "flagCriticalTransactions",
                                   "criticalTransactions",
                                   "flagNegativeValues",
                                   "handleCriticalTransactions"))
    
    jaspResults[["evaluationContainer"]] <- container
    
  } else if(stage == "evaluation-workflow"){
    
    prevContainer <- jaspResults[["selectionContainer"]]
    prevState <- prevContainer[["selectionState"]]$object
    
    if(!is.null(jaspResults[["evaluationContainer"]])){
      return(jaspResults[["evaluationContainer"]])
    } else if(!is.null(prevState)){
      container <- createJaspContainer(title = gettext("<u>Evaluation</u>"))
      container$position <- position
      container$dependOn(options = c("evaluationChecked",
                                     "auditResult",
                                     "mostLikelyError",
                                     "estimator",
                                     "performAudit",
                                     "stringerBoundLtaAdjustment",
                                     "areaUnderPosterior",
                                     "display"))
      
      jaspResults[["evaluationContainer"]] <- container
    }
  }
  return(container)
}

################################################################################
################## Common functions for error checks ###########################
################################################################################

.jfa.inputOptions.check <- function(options, dataset, parentContainer, stage,
                                    ready = NULL, parentOptions = NULL){
  
  if(stage == "procedure"){
    
    variables <- NULL
    if(options[["recordNumberVariable"]] != "")
      variables <- c(variables, options[["recordNumberVariable"]])
    if(options[["monetaryVariable"]] != "")
      variables <- c(variables, options[["monetaryVariable"]])
    if (length(variables) == 0)
      return()
    
    # Check for infinity, zero variance, and any missing observations
    .hasErrors(dataset, type = c("infinity", "variance", "observations"),
               all.target = variables, message = "short",
               observations.amount = paste0("< ", nrow(dataset)),
               exitAnalysisIfErrors = TRUE)
    
  } else if(stage == "planning"){
    if(ready){
      if(options[["materiality"]] == "materialityAbsolute" && options[["materialityValue"]] >= parentOptions[["populationValue"]]){
        # Error if the value of the performance materiality exceeds the total population value
        parentContainer$setError(gettext("Analysis not possible: Your materiality is higher than, or equal to the total value of the observations."))
        return(TRUE)
      }
      expTMP <- ifelse(options[['expectedErrors']] == "expectedRelative",
                       yes = options[["expectedPercentage"]],
                       no = options[["expectedNumber"]])
      if(options[["materiality"]] == "materialityAbsolute")
        expTMP <- expTMP / parentOptions[["populationValue"]]
      if(expTMP >= parentOptions[["materiality"]] && expTMP < 1 && !options[["minimumPrecision"]]){
        # Error if the expected errors exceed the performance materiality
        parentContainer$setError(gettext("Analysis not possible: Your expected errors are higher than materiality."))
        return(TRUE)
      }
      if(.jfa.auditRiskModel.calculation(options) >= 1){
        # Error if the detection risk of the analysis is higher than one
        parentContainer$setError(gettextf("The detection risk is equal to or higher than 100%%. Please re-specify your custom values for the Inherent risk and/or Control risk, or the confidence."))
        return(TRUE)
      }
      if(options[["bayesianAnalysis"]] && !options[["performanceMateriality"]] && !options[["priorConstructionMethod"]] %in% c("none", "sample", "factor")){
        # Error if the prior construction method does not match the sampling objective
        parentContainer$setError(gettext("You cannot incorporate this prior information into your analysis because you are not testing against a performance materiality."))
        return(TRUE)
      }
      if(options[["priorConstructionMethod"]] %in% c("median", "hypotheses") && options[["planningModel"]] == "hypergeometric"){
        # Error if equal prior probabilities are chosen with a beta-binomial distribution
        parentContainer$setError(gettext("The prior distribution with equal prior probabilities cannot be constructed using the beta-binomial distribution. You can use the beta or the gamma distribution in this case."))
        return(TRUE)
      }
      if(options[["priorConstructionMethod"]] %in% c("median", "hypotheses") && expTMP != 0 && options[["planningModel"]] == "Poisson"){
        # Error if equal prior probabilities are chosen in combination with a gamma prior and expected errors != 0
        parentContainer$setError(gettext("The gamma prior distribution with equal prior probabilities cannot be constructed when you expect errors in the sample. You can revert to the beta distribution in this case."))
        return(TRUE)
      }
    }
    # No error in the planning options
    return(FALSE)
    
  } else if(stage == "selection"){
    
    if(!is.null(dataset) && options[["sampleSize"]] >= nrow(dataset) && options[["selectionType"]] == "recordSampling"){
      # Error if the sample size is larger than the population size in case of record sampling.
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      parentContainer$setError(gettext("Your sample size is larger than (or equal to) your population size. Cannot take a record sample larger than the population."))
      return(TRUE)
    } else if(options[["recordNumberVariable"]] != "" && !is.null(dataset) && options[["sampleSize"]] >= parentOptions[["populationValue"]] && options[["selectionType"]] == "musSampling"){
      # Error if the sample size is larger than the population value in case of mus sampling.
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      parentContainer$setError(gettext("Your sample size is larger than (or equal to) your population value. Cannot take a MUS sample larger than the population value."))
      return(TRUE)
    } else if(!is.null(dataset) && options[["sampleSize"]] == 1){
      # Error if the sample size is 1.
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      parentContainer$setError(gettext("Your sample size must be larger than 1."))
      return(TRUE)
    } else if(options[["recordNumberVariable"]] != "" && !is.null(dataset) && nrow(dataset) != length(unique(dataset[, .v(options[["recordNumberVariable"]])]))){
      # Error if the transaction ID's are not unique
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      parentContainer$setError(gettext("You must specify unique transaction ID's. The row numbers of the data set are sufficient."))
      return(TRUE)
    } else {
      # No error in the selection options
      return(FALSE)
    }
    
  } else if(stage == "selection-workflow") {
    
    if(options[["recordNumberVariable"]] != "" && !is.null(dataset) && nrow(dataset) != length(unique(dataset[, .v(options[["recordNumberVariable"]])]))){
      # Error if the transaction ID's are not unique
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      parentContainer$setError(gettext("You must specify unique transaction ID's. The row numbers of the data set are sufficient."))
      return(TRUE)
    } else {
      # No error in the selection options
      return(FALSE)
    }
    
  } else if(stage == "evaluation"){
    
    if(options[["variableType"]] == "variableTypeCorrect" &&
       !options[["useSumStats"]] && options[["auditResult"]] != "" &&
       !all(unique(dataset[, .v(options[["auditResult"]])]) %in% c(0, 1))){
      # Error if the audit result does not contain only zero's and one's.
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      parentContainer$setError(gettext("Your audit result does not contain only 0's (correct) and 1's (incorrect). Select the Soll values annotation method from the options or use the summary statistics to evaluate your sample."))
      return(TRUE)
    } else if(!options[["bayesianAnalysis"]] && options[["variableType"]] == "variableTypeCorrect" &&
              options[["estimator2"]] == "hyperBound" && options[["populationSize"]] == 0){
      # Error if the population size is not defined when the hypergeometric bound is used.
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      parentContainer$setError(gettext("The hypergeometric upper bound requires that you specify the population size."))
      return(TRUE)
    } else if((!options[["useSumStats"]] && !is.null(dataset) && options[["populationSize"]] < nrow(dataset)) ||
              (options[["useSumStats"]] && options[["populationSize"]] < options[["nSumStats"]])){
      # Error if the sample size is larger than the population size.
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      parentContainer$setError(gettext("Your sample size is larger than (or equal to) your population size. Please adjust your population size accordingly."))
      return(TRUE)
    } else if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound") &&
              (options[["populationValue"]] == 0 || options[["populationSize"]] == 0)){
      # Error if the population size or the population value are zero when using direct, difference, ratio, or regression.
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      parentContainer$setError(gettext("The direct, difference, ratio, and regression bounds require that you specify the population size and the population value."))
      return(TRUE)
    } else if(!options[["useSumStats"]] && options[["recordNumberVariable"]] != "" && !is.null(dataset) && nrow(dataset) != length(unique(dataset[, .v(options[["recordNumberVariable"]])]))){
      # Error if the transaction ID's are not unique
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Selection summary"))
      parentContainer$setError(gettext("Your must specify unique transaction ID's. The row numbers of the data set are sufficient."))
      return(TRUE)
    } else if(.jfa.auditRiskModel.calculation(options) >= 1){
      # Error if the detection risk of the analysis is higher than one
      parentContainer[["errorMessage"]] <- createJaspTable(gettext("Evaluation summary"))
      parentContainer$setError(gettextf("The detection risk is equal to or higher than 100%%. Please re-specify your values for the Inherent risk and/or Control risk, or the confidence."))
      return(TRUE)
    } else {
      # No error in the evaluation options
      return(FALSE)
    }
  }
}

.jfa.readyForNextStage.check <- function(options, jaspResults, stage){
  if(stage == "procedure"){
    # Check whether any of the two sampling objectives is selected
    ready <- ((options[["performanceMateriality"]] && ((options[["materiality"]] == "materialityRelative" && options[["materialityPercentage"]] != 0) || (options[["materiality"]] == "materialityAbsolute" && options[["materialityValue"]] != 0))) || (options[["minimumPrecision"]] && options[["minimumPrecisionPercentage"]] != 0))
  } else if(stage == "planning"){
    # Check whether the "To selection" button is pressed and no error occurred in the previous stage
    ready <- options[["samplingChecked"]] && !jaspResults[["planningContainer"]]$getError() && (options[["performanceMateriality"]] || options[["minimumPrecision"]])
  } else if(stage == "selection"){
    # No check for selection
  } else if(stage == "execution"){
    # Check whether the "To evaluation" button is pressed and no error occurred in the previous stage
    ready <- options[["evaluationChecked"]] && !jaspResults[["planningContainer"]]$getError() && !jaspResults[["selectionContainer"]]$getError()
  } else if(stage == "evaluation"){
    # No check for evaluation
  }
  return(ready)
}

.jfa.ready.check <- function(options, parentOptions, stage){
  
  if(stage == "planning"){
    if(!(options[["performanceMateriality"]] || options[["minimumPrecision"]])){
      ready <- FALSE
    }
    if(options[["performanceMateriality"]] && !options[["minimumPrecision"]] && options[["materiality"]] == "materialityAbsolute"){
      ready <- options[["materialityValue"]] != 0 && parentOptions[["populationSize"]] != 0 &&
        parentOptions[["populationValue"]] != 0 && parentOptions[["populationValue"]] != 0.01
    }
    if(options[["performanceMateriality"]] && !options[["minimumPrecision"]] && options[["materiality"]] == "materialityRelative"){
      ready <- options[["materialityPercentage"]] != 0 && parentOptions[["populationSize"]] != 0
    }
    if(options[["minimumPrecision"]] && !options[["performanceMateriality"]]){
      ready <- options[["minimumPrecisionPercentage"]] != 0 && parentOptions[["populationSize"]] != 0 && parentOptions[["populationValue"]] != 0
    }
    if(options[["minimumPrecision"]] && options[["performanceMateriality"]]){
      if(options[["materiality"]] == "materialityAbsolute"){
        ready <- options[["materialityValue"]] != 0 && parentOptions[["populationSize"]] != 0 &&
          parentOptions[["populationValue"]] != 0 && parentOptions[["populationValue"]] != 0.01 &&
          options[["minimumPrecisionPercentage"]] != 0
      } else {
        ready <- options[["materialityPercentage"]] != 0 && parentOptions[["populationSize"]] != 0 && options[["minimumPrecisionPercentage"]] != 0
      }
    }
  }
  return(ready)
}

################################################################################
################## Common functions for the explanatory text ###################
################################################################################

.jfa.explanatoryText.add <- function(options, stageOptions, stageContainer, stageState,
                                     jaspResults, stage, positionInContainer, prevState = NULL){
  
  if(options[["explanatoryText"]]){
    
    if(stage == "procedure"){
      
      procedureContainer <- .jfa.stageContainer.add(jaspResults, stage = "procedure",
                                                    position = 1)
      
      if(!options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
        procedureText <- gettextf("Select one or more sampling objectives from the top left corner to begin planning an audit sample.\n\n%1$s <b>Test against a performance materiality</b>\n\nEnable this objective if you want to <i>test</i> whether the total misstatement in the population exceeds a certain limit (i.e., the performance materiality) based on a sample. This approach allows you to plan a sample such that, when the sample meets your expectations, the maximum error is said to be below performance materiality. In the evaluation you will be able to quantify the evidence that your sample contains for or against the statement that the population misstatement does not exceed the performance materiality.\n\n%2$s <b>Obtain a required minimum precision</b>\n\nEnable this objective if you want to obtain a required minimum precision when <i>estimating</i> the total misstatement in the population based on a sample. This approach allows you to plan a sample such that, when the sample meets expectations, the accuracy of your estimate is below a tolerable percentage. In the evaluation you will be able to quantify the accuracy of your estimate of the population misstatement.", "\u25CF", "\u25CF")
      } else if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
        procedureText <- gettextf("The objective of this sampling procedure is to determine with a confidence of <b>%1$s</b> whether the %2$s of misstatement in the population is lower than the performance materiality of <b>%3$s</b>.",
                                  stageOptions[["confidenceLabel"]],
                                  stageOptions[["absRel"]],
                                  stageOptions[["materialityLabel"]])
      } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
        procedureText <- gettextf("The objective of this sampling procedure is to determine the misstatement in the population with a confidence of <b>%1$s</b> and a required minimum precision of <b>%2$s</b>.",
                                  stageOptions[["confidenceLabel"]],
                                  stageOptions[["minimumPrecisionLabel"]])
      } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
        procedureText <- gettextf("The objective of this sampling procedure is to determine with a confidence of <b>%1$s</b> whether the %2$s of misstatement in the population is lower than the performance materiality of <b>%3$s</b> with a required minimum precision of <b>%4$s</b>.",
                                  stageOptions[["confidenceLabel"]],
                                  stageOptions[["absRel"]],
                                  stageOptions[["materialityLabel"]],
                                  stageOptions[["minimumPrecisionLabel"]])
      }
      
      procedureContainer[["procedureParagraph"]] <- createJaspHtml(procedureText, "p")
      procedureContainer[["procedureParagraph"]]$position <- positionInContainer
      
    } else if(stage == "planning") {
      
      if(is.null(stageContainer[["planningParagraph"]]) && !stageContainer$getError()){
        
        if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
          samplingObjectivesMessage <- gettextf("a performance materiality of <b>%1$s</b>", stageOptions[["materialityLabel"]])
          samplingObjectivesMessage2 <- gettext("the maximum misstatement is determined to be below the performance materiality")
        } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
          samplingObjectivesMessage <- gettextf("a required minimum precision of <b>%1$s</b>", stageOptions[["minimumPrecisionLabel"]])
          samplingObjectivesMessage2 <- gettext("the misstatement is determined with the required minimum precision")
        } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
          samplingObjectivesMessage <- gettextf("a performance materiality of <b>%1$s</b> and a required minimum precision of <b>%2$s</b>", stageOptions[["materialityLabel"]], stageOptions[["minimumPrecisionLabel"]])
          samplingObjectivesMessage2 <- gettext("the maximum misstatement is determined with the required minimum precision to be below the performance materiality")
        }
        
        separateMisstatementMessage <- ifelse(options[["separateKnownAndUnknownMisstatement"]],
                                              yes = gettext("\n\nFurthermore, the uncertainty regarding the misstatement will only be extrapolated over the unseen part of the population. This requires the additional <i>post-hoc</i> assumption that the sample taints are homogeneous."),
                                              no = "")
        
        distribution <- options[["planningModel"]]
        if(options[["bayesianAnalysis"]])
          distribution <- base::switch(options[["planningModel"]], "Poisson" = gettext("gamma"), "binomial" = gettext("beta"), "hypergeometric" = gettext("beta-binomial"))
        
        if(options[["priorConstructionMethod"]] == "none"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assumption that every value of the misstatement is equally likely, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%6$s</b>, %7$s. %8$s",
                                                                           stageOptions[["expectedErrorsLabel"]],
                                                                           samplingObjectivesMessage,
                                                                           stageState[["expectedSampleError"]],
                                                                           stageState[["sampleSize"]],
                                                                           distribution,
                                                                           stageState[["expectedSampleError"]],
                                                                           samplingObjectivesMessage2,
                                                                           separateMisstatementMessage), "p")
        } else if(options[["priorConstructionMethod"]] == "arm"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assessments of inherent risk <b>(%6$s)</b> and control risk <b>(%7$s)</b> from the Audit Risk Model, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%8$s</b>, %9$s. %10$s",
                                                                           stageOptions[["expectedErrorsLabel"]],
                                                                           samplingObjectivesMessage,
                                                                           stageState[["expectedSampleError"]],
                                                                           stageState[["sampleSize"]],
                                                                           distribution,
                                                                           options[["IR"]],
                                                                           options[["CR"]],
                                                                           stageState[["expectedSampleError"]],
                                                                           samplingObjectivesMessage2,
                                                                           separateMisstatementMessage), "p")
        } else if (options[["priorConstructionMethod"]] == "median"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assumption that tolerable misstatement is equally likely to occur as intolerable misstatement, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%6$s</b>, %7$s. %8$s",
                                                                           stageOptions[["expectedErrorsLabel"]],
                                                                           samplingObjectivesMessage,
                                                                           stageState[["expectedSampleError"]],
                                                                           stageState[["sampleSize"]],
                                                                           distribution,
                                                                           stageState[["expectedSampleError"]],
                                                                           samplingObjectivesMessage2,
                                                                           separateMisstatementMessage), "p")
        } else if (options[["priorConstructionMethod"]] == "hypotheses"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assumption that tolerable misstatement occurs with a probability <b>%6$s</b> of and intolerable misstatement occurs with a probability of <b>%7$s</b>, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%8$s</b>, %9$s. %10$s",
                                                                           stageOptions[["expectedErrorsLabel"]],
                                                                           samplingObjectivesMessage,
                                                                           stageState[["expectedSampleError"]],
                                                                           stageState[["sampleSize"]],
                                                                           distribution,
                                                                           options[["pHmin"]],
                                                                           1 - options[["pHmin"]],
                                                                           stageState[["expectedSampleError"]],
                                                                           samplingObjectivesMessage2,
                                                                           separateMisstatementMessage), "p")
        } else if (options[["priorConstructionMethod"]] == "sample"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, the <i>a priori</i> assumption that an earlier sample of <b>%6$s</b> transactions containing <b>%7$s</b> errors is seen, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%8$s</b>, %9$s. %10$s",
                                                                           stageOptions[["expectedErrorsLabel"]],
                                                                           samplingObjectivesMessage,
                                                                           stageState[["expectedSampleError"]],
                                                                           stageState[["sampleSize"]],
                                                                           distribution,
                                                                           options[["sampleN"]],
                                                                           options[["sampleK"]],
                                                                           stageState[["expectedSampleError"]],
                                                                           samplingObjectivesMessage2,
                                                                           separateMisstatementMessage), "p")
        } else if (options[["priorConstructionMethod"]] == "factor"){
          stageContainer[["planningParagraph"]] <- createJaspHtml(gettextf("The most likely error in the sample is expected to be <b>%1$s</b>. The sample size that is required for %2$s, assuming the sample contains <b>%3$s</b> full errors, is <b>%4$s</b>. This sample size is based on the <b>%5$s</b> distribution, an <i>a priori</i> assumption that an earlier sample of <b>%6$s</b> transactions containing <b>%7$s</b> errors weighted by a factor <b>%8$s</b> is seen, and the expected errors. \n\nConsequently, if this sample is evaluated and the sum of (proportional) errors in the audited transactions is lower than (or equal to) <b>%9$s</b>, %10$s. %11$s",
                                                                           stageOptions[["expectedErrorsLabel"]],
                                                                           samplingObjectivesMessage,
                                                                           stageState[["expectedSampleError"]],
                                                                           stageState[["sampleSize"]],
                                                                           distribution,
                                                                           options[["sampleN"]],
                                                                           options[["sampleK"]],
                                                                           options[["factor"]],
                                                                           stageState[["expectedSampleError"]],
                                                                           samplingObjectivesMessage2,
                                                                           separateMisstatementMessage), "p")
        }
        
        stageContainer[["planningParagraph"]]$position <- positionInContainer
        stageContainer[["planningParagraph"]]$dependOn(options = "explanatoryText")
      }
      
    } else if(stage == "selection"){
      
      samplingLabel <- base::switch(options[["selectionMethod"]],
                                    "randomSampling" = gettext("random"),
                                    "systematicSampling" = gettext("fixed interval"),
                                    "cellSampling" = gettext("cell"))
      
      if(options[["selectionType"]] == "musSampling"){
        samplingVariableText <- gettext("Ist position (<i>...</i>)")
        samplingUnitText <- base::switch(options[["valuta"]],
                                         "euroValuta" = gettext("Euros"),
                                         "dollarValuta" = gettext("Dollars"),
                                         "otherValuta" = options[["otherValutaName"]])
        if(options[["monetaryVariable"]] != "")
          samplingVariableText <- gettextf("Ist position (<i>%1$s</i>)", options[["monetaryVariable"]])
      } else if(options[["selectionType"]] == "recordSampling"){
        samplingUnitText <- "records"
        samplingVariableText <- gettext("record variable (<i>...</i>)")
        if(options[["recordNumberVariable"]] != "")
          samplingVariableText <- gettextf("record variable (<i>%1$s</i>)", options[["recordNumberVariable"]])
      }
      
      if(!is.null(stageState) && !is.null(stageState[["musFailed"]])){
        # MUS has failed for some reason, fall back to record sampling
        
        message <- gettextf("From the population of <b>%1$s</b> %2$s transactions, <b>%3$s</b> sampling units (<i>%4$s</i>) are selected from the %5$s using a <b>%6$s record sampling</b> method. <br><b>Warning:</b> A monetary unit sampling method was tried but failed.",
                            stageOptions[["populationSize"]],
                            ifelse(options[["shufflePopulationBeforeSampling"]], yes = gettext("randomized"), no = "non-randomized"),
                            prevState[["sampleSize"]],
                            samplingUnitText,
                            samplingVariableText,
                            samplingLabel)
        
      } else {
        
        samplingLabel <- base::switch(options[["selectionType"]],
                                      "recordSampling" = gettextf("%1$s record sampling", samplingLabel),
                                      "musSampling" = gettextf("%1$s monetary unit sampling", samplingLabel))
        
        message <- gettextf("From the population of <b>%1$s</b> %2$s transactions, <b>%3$s</b> sampling units (<i>%4$s</i>) are selected from the %5$s using a <b>%6$s</b> method.",
                            stageOptions[["populationSize"]],
                            ifelse(options[["shufflePopulationBeforeSampling"]], yes = gettext("randomized"), no = "non-randomized"),
                            prevState[["sampleSize"]],
                            samplingUnitText,
                            samplingVariableText,
                            samplingLabel)
        
      }
      
      if(!is.null(stageState) && sum(stageState[["count"]]) > nrow(stageState)){
        
        message <- gettextf("%1$s \n\n<b>Note:</b> The selected sample (%2$s) contains fewer transactions than the planned sample size (%3$s) because some transactions are selected more than once. The transactions containing these %4$s extra selected sampling units will be counted multiple times in the evaluation.",
                            message,
                            nrow(stageState),
                            prevState[["sampleSize"]],
                            prevState[["sampleSize"]] - nrow(stageState))
        
      }
      
      
      stageContainer[["samplingParagraph"]] <- createJaspHtml(message, "p")
      stageContainer[["samplingParagraph"]]$position <- positionInContainer
      stageContainer[["samplingParagraph"]]$dependOn(options = "explanatoryText")
      
    } else if(stage == "conclusion"){
      
      # Import options and results from the planning and selection stages
      planningOptions <- .jfa.inputOptions.collect(options, dataset = NULL, jaspResults,
                                                   stage = "planning", rawData = TRUE)
      
      # Import result of analysis from jaspResults
      evaluationContainer   <- jaspResults[["evaluationContainer"]]
      evaluationState       <- evaluationContainer[["evaluationState"]]$object
      
      if(is.null(evaluationState)) return()
      
      conclusionContainer <- jaspResults[["conclusionContainer"]]
      
      # Produce relevant terms conditional on the analysis result
      approveMateriality <- TRUE
      if(options[["performanceMateriality"]]){
        if(evaluationState[["confBound"]] < planningOptions[["materiality"]]){
          aboveBelowMateriality <- gettext("below")
          lowerHigherMateriality <- gettext("lower")
          approveMateriality <- TRUE
        } else {
          aboveBelowMateriality <- gettext("above")
          lowerHigherMateriality <- gettext("higher")
          approveMateriality <- FALSE
        }
      }
      
      approvePrecision <- TRUE
      if(options[["minimumPrecision"]]){
        if(evaluationState[["precision"]] <= planningOptions[["minimumPrecision"]]){
          lowerHigherPrecision <- gettext("lower")
          approvePrecision <- TRUE
        } else {
          lowerHigherPrecision <- gettext("higher")
          approvePrecision <- FALSE
        }
      }
      
      additionalMessage <- ifelse(approveMateriality && approvePrecision,
                                  yes = gettext("\n\n<b>Objectives:</b> You have achieved your specified sampling objectives."),
                                  no = gettext("\n\n<b>Objectives:</b> You have not achieved your specified sampling objectives. It is recommended to draw more samples from this population and to continue audit procedures."))
      
      if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
        message <- gettextf("The objective of this audit sampling procedure was to determine with %1$s confidence whether the misstatement in the population is lower than the specified performance materiality, in this case %2$s. For the current data, the %3$s upper bound on the misstatement is <b>%4$s</b> the performance materiality. \n\nThe conclusion on the basis of these results is that, with at least %5$s confidence, the misstatement in the population is <b>%6$s</b> than the performance materiality. %7$s",
                            planningOptions[["confidenceLabel"]],
                            planningOptions[["materialityLabel"]],
                            planningOptions[["confidenceLabel"]],
                            aboveBelowMateriality,
                            planningOptions[["confidenceLabel"]],
                            lowerHigherMateriality,
                            additionalMessage)
      } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
        message <- gettextf("The objective of this audit sampling procedure was to determine the misstatement in the population with %1$s confidence and a minimum precision of %2$s. For the current data, the obtained precision is <b>%3$s</b> than the required minimum precision. \n\nThe conclusion on the basis of these results is that the total misstatement in the population has been determined with at least %4$s confidence and a precision of %5$s. %6$s",
                            planningOptions[["confidenceLabel"]],
                            paste0(options[["minimumPrecisionPercentage"]] * 100, "%"),
                            lowerHigherPrecision,
                            planningOptions[["confidenceLabel"]],
                            paste0(round(evaluationState[["precision"]] * 100, 3), "%"),
                            additionalMessage)
      } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
        message <- gettextf("The objective of this audit sampling procedure was to determine with %1$s confidence, and a minimum precision of %2$s, whether the misstatement in the population is lower than the specified performance materiality, in this case %3$s. For the current data, the %4$s upper bound on the misstatement is <b>%5$s</b> the performance materiality and the obtained precision is <b>%6$s</b> than the required minimum precision. \n\nThe conclusion on the basis of these results is that, with at least %7$s confidence and a precision of %8$s, the misstatement in the population is <b>%9$s</b> than the performance materiality. %10$s",
                            planningOptions[["confidenceLabel"]],
                            paste0(options[["minimumPrecisionPercentage"]] * 100, "%"),
                            planningOptions[["materialityLabel"]],
                            planningOptions[["confidenceLabel"]],
                            aboveBelowMateriality,
                            lowerHigherPrecision,
                            planningOptions[["confidenceLabel"]],
                            paste0(round(evaluationState[["precision"]] * 100, 3), "%"),
                            lowerHigherMateriality,
                            additionalMessage)
      }
      conclusionContainer[["conclusionParagraph"]] <- createJaspHtml(message, "p")
      conclusionContainer[["conclusionParagraph"]]$position <- 1
      conclusionContainer[["conclusionParagraph"]]$dependOn(optionsFromObject = conclusionContainer)
      
      # Finsh conclusion
      jaspResults[["conclusionContainer"]] <- conclusionContainer
    }
  }
}

################################################################################
################## Common functions for the procedure stage ####################
################################################################################

.jfa.bookvalueDescriptives.table <- function(options, parentOptions, jaspResults, positionInContainer){
  
  parentContainer <- .jfa.stageContainer.add(jaspResults, stage = "procedure", position = 1)
  
  if(!options[["bookValueDescriptives"]] || options[["monetaryVariable"]] == "")
    return()
  
  .jfa.tableNumber.update(jaspResults)
  
  if(is.null(parentContainer[["bookValueDescriptives"]])){
    
    title <- gettextf("<b>Table %1$i.</b> Descriptive Statistics for Ist Position",
                      jaspResults[["tabNumber"]]$object)
    
    table <- createJaspTable(title)
    table$position <- positionInContainer
    table$dependOn(options = c("bookValueDescriptives",
                               "sampleDescriptives",
                               "displaySample",
                               "samplingChecked",
                               "evaluationChecked"))
    
    table$addColumnInfo(name = 'N',   		title = gettext("Population size"), type = 'string')
    table$addColumnInfo(name = 'val',  		title = gettext("Value"),       	type = 'string')
    table$addColumnInfo(name = 'absval',   	title = gettext("Absolute value"),  type = 'string')
    table$addColumnInfo(name = 'mean',      title = gettext("Mean"),            type = 'string')
    table$addColumnInfo(name = 'sigma',     title = gettext("Std. deviation"),  type = 'string')
    table$addColumnInfo(name = 'q1',        title = gettext("1"),               type = 'string',  overtitle = gettext("Quartile"))
    table$addColumnInfo(name = 'q2',        title = gettext("2"),               type = 'string',  overtitle = gettext("Quartile"))
    table$addColumnInfo(name = 'q3',        title = gettext("3"),               type = 'string',  overtitle = gettext("Quartile"))
    
    parentContainer[["bookValueDescriptives"]] <- table
    
    if(options[["monetaryVariable"]] == "" || options[["recordNumberVariable"]] == "")
      return()
    
    prevOptions <- jaspResults[["procedureOptions"]]$object
    valuta 		<- parentOptions[["valuta"]]
    
    row <- data.frame(N    		= prevOptions[["populationSize"]],
                      val 	= paste(valuta, round(prevOptions[["populationValue"]], 2)),
                      absval 	= paste(valuta, round(prevOptions[["absPopulationValue"]], 2)),
                      mean    = paste(valuta, round(prevOptions[["meanValue"]], 2)),
                      sigma   = paste(valuta, round(prevOptions[["sigmaValue"]], 2)),
                      q1      = paste(valuta, round(prevOptions[["quantileValue"]][1], 2)),
                      q2      = paste(valuta, round(prevOptions[["quantileValue"]][2], 2)),
                      q3      = paste(valuta, round(prevOptions[["quantileValue"]][3], 2)))
    
    table$addRows(row)
  }
}

.jfa.bookvalueDescriptives.plot <- function(options, dataset, jaspResults, positionInContainer){
  
  parentContainer <- .jfa.stageContainer.add(jaspResults, stage = "procedure", position = 1)
  
  if(!options[["bookValueDistribution"]] || options[["monetaryVariable"]] == "")
    return()
  
  .jfa.figureNumber.update(jaspResults)
  
  if(is.null(parentContainer[["bookValueDistribution"]])){
    
    figure <- createJaspPlot(plot = NULL, title = gettext("Distribution of Ist Values"), width = 600, height = 300)
    figure$position <- positionInContainer
    figure$dependOn(options = c("bookValueDistribution", "valuta"))
    parentContainer[["bookValueDistribution"]] <- figure
    
    if(options[["recordNumberVariable"]] == "")
      return()
    
    prevOptions <- jaspResults[["procedureOptions"]]$object
    values      <- dataset[, .v(options[["monetaryVariable"]])]
    mean        <- prevOptions[["meanValue"]]
    stdev       <- prevOptions[["sigmaValue"]]
    quantiles   <- prevOptions[["quantileValue"]]
    valuta 		<- base::switch(options[["valuta"]],
                             "euroValuta" = "\u20AC",
                             "dollarValuta" = "\u0024",
                             "otherValuta" = options[["otherValutaName"]])
    
    legendData <- data.frame(x = c(0, 0, 0), y = c(0, 0, 0), l = c("1", "2", "3"))
    
    h <- hist(values, plot = FALSE, breaks = 20)
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, h$counts), min.n = 4)
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(values, h$breaks), min.n = 4)
    
    plot <- ggplot2::ggplot(data = data.frame(values), mapping = ggplot2::aes(x = values, y = ..count..)) +
      ggplot2::scale_x_continuous(name = gettextf("Ist (%1$s)", valuta), breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_y_continuous(name = gettext("Frequency"), breaks = yBreaks, limits = c(0, max(yBreaks))) +
      ggplot2::geom_histogram(binwidth = (h$breaks[2] - h$breaks[1]), fill = "grey", col = "black",
                              size = .7, center = ((h$breaks[2] - h$breaks[1])/2)) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = quantiles[1], y = 0),
                          shape = 21, fill = "orange", stroke = 1.5, size = 2) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = quantiles[2], y = 0),
                          shape = 21, fill = "orange", stroke = 1.5, size = 2) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = quantiles[3], y = 0),
                          shape = 21, fill = "orange", stroke = 1.5, size = 2) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = mean, y = 0),
                          shape = 21, fill = "red", stroke = 1.5, size = 4) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = mean + stdev, y = 0),
                          shape = 21, fill = "dodgerblue1", stroke = 1.5, size = 3) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = mean - stdev, y = 0),
                          shape = 21, fill = "dodgerblue1", stroke = 1.5, size = 3) +
      ggplot2::geom_point(data = legendData, mapping = ggplot2::aes(x = x, y = y, shape = l),
                          size = 0, color = rgb(0, 1, 0, 0)) +
      ggplot2::scale_shape_manual(name = "", values = c(21, 21, 21),
                                  labels = c(gettext("Mean"), gettextf("Mean %1$s St.dev", "\u00B1"), gettext("Quartile"))) +
      ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = c(4, 3, 2), shape = c(21, 21, 21),
                                                                        fill = c("red", "dodgerblue1", "orange"), stroke = 1.5, color = "black")), order = 1)
    
    jfaTheme <- ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -10, r = 50)),
                               panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", size = 0.5),
                               legend.position = "top")
    
    plot <- JASPgraphs::themeJasp(plot) + jfaTheme
    
    figure$plotObject <- plot
  }
  
  if(options[["explanatoryText"]]){
    figureCaption <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The distribution of Ist values in the population. The red and blue dots respectively represent the mean and the values exactly one standard deviation from the mean. The orange dots represent the first, second (median) and third quartiles of the Ist values.", jaspResults[["figNumber"]]$object), "p")
    figureCaption$position <- positionInContainer + 1
    figureCaption$dependOn(optionsFromObject = parentContainer[["bookValueDistribution"]])
    figureCaption$dependOn(options = "explanatoryText")
    parentContainer[["bookValuePlotText"]] <- figureCaption
  }
}

################################################################################
################## Common functions for the Audit Risk Model ###################
################################################################################

.jfa.auditRiskModel.calculation <- function(options){
  
  # Audit risk 		= Inherent risk x Control risk x Detection risk
  # Detection risk 	= Audit risk / (Inherent risk x Control risk)
  
  auditRisk 		<- 1 - options[["confidence"]]
  inherentRisk 	<- base::switch(options[["IR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["irCustom"]])
  controlRisk 	<- base::switch(options[["CR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["crCustom"]])
  detectionRisk 	<- auditRisk / (inherentRisk * controlRisk)
  
  return(detectionRisk)
}

.jfa.auditRiskModel.add <- function(options, jaspResults, position){
  
  if(!is.null(jaspResults[["ARMcontainer"]]) || (!options[["performanceMateriality"]] && !options[["minimumPrecision"]]))
    return()
  
  if(options[["priorConstructionMethod"]] != "arm" || !options[["performanceMateriality"]])
    return()
  
  container <- createJaspContainer(title = gettext("<u>Audit Risk Model</u>"))
  container$position <- position
  container$dependOn(options = c("confidence",
                                 "IR",
                                 "irCustom",
                                 "CR",
                                 "crCustom",
                                 "materiality",
                                 "materialityPercentage",
                                 "materialityValue",
                                 "explanatoryText",
                                 "valuta",
                                 "otherValutaName",
                                 "performanceMateriality",
                                 "minimumPrecision",
                                 "priorConstructionMethod"))
  
  jaspResults[["ARMcontainer"]] <- container
  
  auditRisk 		<- 1 - options[["confidence"]]
  inherentRisk 	<- base::switch(options[["IR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["irCustom"]])
  controlRisk 	<- base::switch(options[["CR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["crCustom"]])
  detectionRisk 	<- auditRisk / (inherentRisk * controlRisk)
  
  if(options[["explanatoryText"]]){
    irLabel <- paste0(options[["IR"]], " = " , round(inherentRisk * 100, 2))
    crLabel <- paste0(options[["CR"]], " = " , round(controlRisk * 100, 2))
  } else {
    irLabel <- round(inherentRisk * 100, 2)
    crLabel <- round(controlRisk * 100, 2)
  }
  
  textARM <- gettextf("Audit risk (%1$s%%) = Inherent risk (%2$s%%) x Control risk (%3$s%%) x Detection risk (%4$s%%)",
                      round(auditRisk * 100, 2),
                      irLabel,
                      crLabel,
                      round(detectionRisk * 100, 2))
  
  container[["ARMformula"]] <- createJaspHtml(textARM, "h3", "21cm")
  container[["ARMformula"]]$position <- 2
  
  if(options[["explanatoryText"]]){
    
    irLabel 				<- paste0(options[["IR"]], " (", round(inherentRisk * 100, 2), "%)")
    crLabel 				<- paste0(options[["CR"]], " (", round(controlRisk * 100, 2), "%)")
    auditRiskLabel 			<- paste0(round(auditRisk * 100, 2), "%")
    dectectionRiskLabel 	<- paste0(round(detectionRisk * 100, 2), "%")
    
    message <- gettextf("Prior to the sampling procedure, the inherent risk was determined to be <b>%1$s</b>. The internal control risk was determined to be <b>%2$s</b>. According to the Audit Risk Model, the required detection risk to maintain an audit risk of <b>%3$s</b> should be <b>%4$s</b>.",
                        irLabel,
                        crLabel,
                        auditRiskLabel,
                        dectectionRiskLabel)
    
    if(options[["IR"]] == "Custom" || options[["CR"]] == "Custom"){
      message <- gettextf("%1$s\n\nThe translation of High, Medium and Low to probabilities is done according custom preferences</b>.",
                          message)
    } else {
      message <- gettextf("%1$s\n\nThe translation of High, Medium and Low to probabilities is done using default values. To learn more about the choice of these values and how to adjust these, see the help file of this analysis.",
                          message)
    }
    
    container[["AuditRiskModelParagraph"]] <- createJaspHtml(message, "p")
    container[["AuditRiskModelParagraph"]]$position <- 1
  }
}

################################################################################
################## Common functions for the planning stage #####################
################################################################################

.jfa.planning.state <- function(options, parentOptions, parentContainer, ready, jaspResults){
  
  if(!is.null(parentContainer[["planningState"]])){
    
    return(parentContainer[["planningState"]]$object)
    
  } else if(ready && !parentContainer$getError()){
    
    if(options[["workflow"]])
      dataset <- .jfa.dataset.read(options, jaspResults, stage = "procedure")
    
    minPrecision <- NULL
    if(options[["minimumPrecision"]])
      minPrecision <- options[["minimumPrecisionPercentage"]]
    
    performanceMateriality <- NULL
    if(options[["performanceMateriality"]])
      performanceMateriality <- parentOptions[["materiality"]]
    
    N <- parentOptions[["populationSize"]]
    if(options[["workflow"]] && options[["monetaryVariable"]] != "")
      N <- ceiling(parentOptions[["populationValue"]])
    if(!options[["workflow"]] && options[["populationValue"]] > 0)
      N <- ceiling(parentOptions[["populationValue"]])
    
    confidence <- options[["confidence"]]
    if(options[["priorConstructionMethod"]] == "arm"){
      
      auditRisk 		<- 1 - options[["confidence"]]
      inherentRisk 	<- base::switch(options[["IR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["irCustom"]])
      controlRisk 	<- base::switch(options[["CR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["crCustom"]])
      detectionRisk 	<- auditRisk / (inherentRisk * controlRisk)
      
      if(detectionRisk >= 1){
        parentContainer$setError(gettextf("The detection risk is equal to or higher than 100%%. Please re-specify your custom values for the Inherent risk and/or Control risk."))
        return()
      }
      
      confidence <- 1 - detectionRisk
    }
    
    if(!options[["bayesianAnalysis"]]){
      
      result <- try({
        jfa::planning(materiality = performanceMateriality,
                      confidence = confidence,
                      expectedError = parentOptions[["expectedErrors"]],
                      minPrecision = minPrecision,
                      likelihood = parentOptions[["likelihood"]],
                      N = N,
                      increase = options[["sampleSizeIncrease"]])
      })
      
    } else if(options[["bayesianAnalysis"]]){
      
      prior <- jfa::auditPrior(materiality = performanceMateriality,
                               confidence = parentOptions[["confidence"]],
                               expectedError = parentOptions[["expectedErrors"]],
                               likelihood = parentOptions[["likelihood"]],
                               N = N,
                               ir = inherentRisk,
                               cr = controlRisk,
                               method = options[["priorConstructionMethod"]],
                               sampleN = options[["sampleN"]],
                               sampleK = options[["sampleK"]],
                               factor = options[["factor"]],
                               pHplus = 1 - options[["pHmin"]],
                               pHmin = options[["pHmin"]])
      
      result <- try({
        
        if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != ""){
          
          .jfa.separatedMisstatementPlanning.state(options, dataset, prior, parentOptions)
          
        } else {
          
          jfa::planning(materiality = performanceMateriality,
                        confidence = parentOptions[["confidence"]],
                        expectedError = parentOptions[["expectedErrors"]],
                        likelihood = parentOptions[["likelihood"]],
                        N = N,
                        prior = prior,
                        minPrecision = minPrecision,
                        increase = options[["sampleSizeIncrease"]],
                        maxSize = 10000)
        }
      })
    }
    
    if(isTryError(result)){
      
      if(JASP:::.extractErrorMessage(result) == "Sample size could not be calculated, please increase the maxSize argument"){
        parentContainer$setError(gettext("You cannot achieve your current sampling objectives with this population. The resulting sample size exceeds 5000. Adjust your sampling objectives or variables accordingly."))
        return()
      }
      
      parentContainer$setError(gettextf("An error occurred: %1$s", JASP:::.extractErrorMessage(result)))
      return()
    }
    
    # This causes an error in the planning stage [needs a further look]
    if(options[["workflow"]]){
      if(options[["selectionType"]] == "recordSampling" && result[["sampleSize"]] > parentOptions[["populationSize"]]){
        parentContainer$setError(gettext("You cannot achieve your current sampling objectives with this population. The resulting sample size is larger than the number of transactions in the population. Adjust your sampling objectives or variables accordingly."))
        return()
      } else if(options[["selectionType"]] == "musSampling" && result[["sampleSize"]] > parentOptions[["populationValue"]]){
        parentContainer$setError(gettext("You cannot achieve your current sampling objectives with this population. The resulting sample size is larger than the total value of the population. Adjust your sampling objectives or variables accordingly."))
        return()
      }
    }
    
    parentContainer[["planningState"]] <- createJaspState(result)
    parentContainer[["planningState"]]$dependOn(options = c("planningModel"))
    
    return(result)
    
  } else {
    
    N <- parentOptions[["populationSize"]]
    if(options[["workflow"]] && options[["monetaryVariable"]] != "")
      N <- ceiling(parentOptions[["populationValue"]])
    if(!options[["workflow"]] && options[["populationValue"]] > 0)
      N <- ceiling(parentOptions[["populationValue"]])
    
    bPrior <- ifelse(options[["planningModel"]] == "Poisson", yes = 0, no = 1)
    
    noResults <- list(sampleSize = 0,
                      materiality = parentOptions[["materiality"]],
                      N = N,
                      expectedSampleError = 0,
                      prior = list(description = list(alpha = 1, beta = bPrior, implicitn = 0, implicitk = 0)))
    
    return(noResults)
  }
}

.jfa.planning.table <- function(options, parentOptions, parentState, parentContainer, jaspResults,
                                ready, positionInContainer){
  
  .jfa.tableNumber.update(jaspResults)
  
  if(!is.null(parentContainer[["summaryTable"]]))
    return()
  
  tableTitle <- gettextf("<b>Table %1$i.</b> Planning Summary", jaspResults[["tabNumber"]]$object)
  
  table <- createJaspTable(tableTitle)
  table$position <- positionInContainer
  table$dependOn(options = c("bookValueDescriptives",
                             "sampleDescriptives",
                             "displaySample",
                             "samplingChecked",
                             "evaluationChecked",
                             "planningModel",
                             "expectedEvidenceRatio",
                             "expectedBayesFactor",
                             "expectedErrors",
                             "minimumPrecision",
                             "performanceMateriality",
                             "separateKnownAndUnknownMisstatement",
                             "sampleSizeIncrease"))
  
  # Add columns to table layout
  if(options[["performanceMateriality"]])
    table$addColumnInfo(name = 'materiality', 	title = gettext("Performance materiality"), type = 'string')
  if(options[["minimumPrecision"]])
    table$addColumnInfo(name = 'precision', 	title = gettext("Required precision"), 		type = 'string')
  if(options[["performanceMateriality"]] && options[["priorConstructionMethod"]] == "arm"){
    table$addColumnInfo(name = 'IR', 			title = gettext("Inherent risk"), 			type = 'string')
    table$addColumnInfo(name = 'CR', 			title = gettext("Control risk"), 			type = 'string')
    table$addColumnInfo(name = 'DR', 			title = gettext("Detection risk"), 			type = 'string')
  } else {
    table$addColumnInfo(name = 'AR',			title = gettext("Audit risk"),  			type = 'string')
  }
  
  table$addColumnInfo(name = 'k', title = gettext("Expected errors"), type = ifelse(options[["expectedErrors"]] == "expectedAllPossible", yes = "string", no = "number"))
  table$addColumnInfo(name = 'n', title = gettext("Required sample size"), type = 'integer')
  
  if(options[["bayesianAnalysis"]] && options[["expectedEvidenceRatio"]] && options[["performanceMateriality"]])
    table$addColumnInfo(name = 'expectedEvidenceRatio', title = gettext("Expected posterior odds"), type = 'number')
  
  if(options[["bayesianAnalysis"]] && options[["expectedBayesFactor"]] && options[["performanceMateriality"]])
    table$addColumnInfo(name = 'expectedBayesFactor', title = gettextf("Expected %1$s", "BF\u208B\u208A"), type = 'number')
  
  parentContainer[["summaryTable"]] <- table
  
  auditRisk <- 1 - options[["confidence"]]
  if(options[["priorConstructionMethod"]] == "arm"){
    inherentRisk 	<- base::switch(options[["IR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["irCustom"]])
    controlRisk 	<- base::switch(options[["CR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["crCustom"]])
    detectionRisk 	<- auditRisk / (inherentRisk * controlRisk)
  }
  
  N <- parentOptions[["populationSize"]]
  if(options[["workflow"]] && options[["monetaryVariable"]] != "")
    N <- ceiling(parentOptions[["populationValue"]])
  if(!options[["workflow"]] && options[["populationValue"]] > 0)
    N <- ceiling(parentOptions[["populationValue"]])
  
  if(!ready || parentContainer$getError()){
    
    if(!options[["bayesianAnalysis"]]){
      message <- base::switch(options[["planningModel"]],
                              "Poisson" = gettext("The required sample size is based on the <b>Poisson</b> distribution."),
                              "binomial" =  gettext("The required sample size is based on the <b>binomial</b> distribution."),
                              "hypergeometric" = gettext("The required sample size is based on the <b>hypergeometric</b> distribution."))
    } else if(options[["bayesianAnalysis"]]){
      message <- base::switch(options[["planningModel"]],
                              "Poisson" = gettext("The required sample size is based on the <b>gamma</b> distribution."),
                              "binomial" = gettext("The required sample size is based on the <b>beta</b> distribution."),
                              "hypergeometric" = gettextf("The required sample size is based on the <b>beta-binomial</b> distribution (N = %1$s).", N))
    }
    table$addFootnote(message)
    
    row <- data.frame(k = ".", n = ".")
    
    if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
      row <- cbind(row, materiality = parentOptions[["materialityLabel"]])
    } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
      row <- cbind(row, precision = paste0(round(options[["minimumPrecisionPercentage"]] * 100, 2), "%"))
    } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
      row <- cbind(row, materiality = parentOptions[["materialityLabel"]],
                   precision = paste0(round(options[["minimumPrecisionPercentage"]] * 100, 2), "%"))
    }
    
    if(options[["performanceMateriality"]] && options[["priorConstructionMethod"]] == "arm"){
      row <- cbind(row, IR = paste0(round(inherentRisk * 100, 2), "%"),
                   CR = paste0(round(controlRisk * 100, 2), "%"),
                   DR = paste0(round(detectionRisk * 100, 2), "%"))
    } else {
      row <- cbind(row, AR = paste0(round(auditRisk * 100, 2), "%"))
    }
    
    if(options[["bayesianAnalysis"]] && options[["expectedEvidenceRatio"]] && options[["performanceMateriality"]])
      row <- cbind(row, expectedEvidenceRatio = ".")
    if(options[["bayesianAnalysis"]] && options[["expectedBayesFactor"]] && options[["performanceMateriality"]])
      row <- cbind(row, expectedBayesFactor = ".")
    
    table$addRows(row)
    table$addFootnote(message = gettext("Either the materiality (if applicable), the population size, or the population value is defined as zero."),
                      symbol = gettextf("%1$s <b>Results could not be calculated.</b>", "\u26A0"))
    
    return()
  }
  
  if(!options[["bayesianAnalysis"]]){
    
    message <- base::switch(options[["planningModel"]],
                            "Poisson" = gettextf("The required sample size is based on the <b>Poisson</b> distribution <i>(%1$s = %2$s)</i>.",
                                                 "\u03BB",
                                                 round(parentState[["materiality"]] * parentState[["sampleSize"]], 4)),
                            "binomial" =  gettextf("The required sample size is based on the <b>binomial</b> distribution <i>(p = %1$s)</i>",
                                                   round(parentState[["materiality"]], 2)),
                            "hypergeometric" = gettextf("The required sample size is based on the <b>hypergeometric</b> distribution <i>(N = %1$s, K = %2$s)</i>.",
                                                        parentState[["N"]],
                                                        ceiling(parentState[["N"]] * parentState[["materiality"]])))
    
  } else if(options[["bayesianAnalysis"]]){
    
    message <- base::switch(options[["planningModel"]],
                            "Poisson" = gettextf("The required sample size is based on the <b>gamma</b> distribution <i>(%1$s = %2$s, %3$s = %4$s)</i>",
                                                 "\u03B1",
                                                 round(parentState[["prior"]][["description"]]$alpha, 3),
                                                 "\u03B2",
                                                 round(parentState[["prior"]][["description"]]$beta, 3)),
                            "binomial" = gettextf("The required sample size is based on the <b>beta</b> distribution <i>(%1$s = %2$s, %3$s = %4$s)</i>.",
                                                  "\u03B1",
                                                  round(parentState[["prior"]][["description"]]$alpha, 3),
                                                  "\u03B2",
                                                  round(parentState[["prior"]][["description"]]$beta, 3)),
                            "hypergeometric" = gettextf("The required sample size is based on the <b>beta-binomial</b> distribution <i>(N = %1$s, %2$s = %3$s, %4$s = %5$s)</i>.",
                                                        parentState[["N"]] - parentState[["sampleSize"]] + parentState[["expectedSampleError"]],
                                                        "\u03B1",
                                                        round(parentState[["prior"]][["description"]]$alpha, 3),
                                                        "\u03B2",
                                                        round(parentState[["prior"]][["description"]]$beta, 3)))
    
  }
  
  table$addFootnote(message)
  
  n <- parentState[["sampleSize"]]
  k <- base::switch(options[["expectedErrors"]],
                    "expectedRelative" = parentState[["expectedSampleError"]],
                    "expectedAbsolute" = options[["expectedNumber"]],
                    "expectedAllPossible" = paste0("0 - ", n))
  
  if(options[["performanceMateriality"]] && options[["materiality"]] == "materialityValue")
    k <- paste(parentOptions[["valuta"]], k)
  
  row <- data.frame(k = k, n = n)
  
  if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
    row <- cbind(row, materiality = parentOptions[["materialityLabel"]])
  } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
    row <- cbind(row, precision = paste0(round(options[["minimumPrecisionPercentage"]] * 100, 2), "%"))
  } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
    row <- cbind(row, materiality = parentOptions[["materialityLabel"]],
                 precision = paste0(round(options[["minimumPrecisionPercentage"]] * 100, 2), "%"))
  }
  
  if(options[["performanceMateriality"]] && options[["priorConstructionMethod"]] == "arm"){
    row <- cbind(row, IR = paste0(round(inherentRisk * 100, 2), "%"),
                 CR = paste0(round(controlRisk * 100, 2), "%"),
                 DR = paste0(round(detectionRisk * 100, 2), "%"))
  } else {
    row <- cbind(row, AR = paste0(round(auditRisk * 100, 2), "%"))
  }
  
  if(options[["bayesianAnalysis"]] && options[["performanceMateriality"]] && (options[["expectedEvidenceRatio"]] || options[["expectedBayesFactor"]])){
    if(options[["expectedEvidenceRatio"]])
      row <- cbind(row, expectedEvidenceRatio = parentState[["expectedPosterior"]][["hypotheses"]]$oddsHmin)
    if(options[["expectedBayesFactor"]])
      row <- cbind(row, expectedBayesFactor = parentState[["expectedPosterior"]][["hypotheses"]]$expectedBf)
  }
  
  table$addRows(row)
  
  if(options[["bayesianAnalysis"]] && options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != ""){
    message <- gettextf("The value %1$s is automatically used as a starting point for the fixed interval selection.", parentState[["startingPoint"]])
    table$addFootnote(message, symbol = gettextf("%1$s", "\u26A0"))
  }
}

.jfa.criticalTransactions.init <- function(options, jaspResults){
  
  if(options[["recordNumberVariable"]] == "" || options[["monetaryVariable"]] == "")
    return()
  
  dataset <- .jfa.dataset.read(options, jaspResults, stage = "procedure")
  
  if(options[["flagCriticalTransactions"]]){
    criticalTransactions <- rep(0, nrow(dataset))
    if(options[["flagNegativeValues"]])
      criticalTransactions[which(dataset[, .v(options[["monetaryVariable"]])] < 0)] <- 1
  } else {
    criticalTransactions <- rep(NA, nrow(dataset))
  }
  
  if(is.null(jaspResults[["criticalTransactions"]]))
    jaspResults[["criticalTransactions"]] <- createJaspColumn(columnName = options[["criticalTransactions"]], dependencies = "criticalTransactions")
  
  jaspResults[["criticalTransactions"]]$setScale(criticalTransactions)
}

.jfa.criticalTransactions.add <- function(options, sample){
  
  if(options[["workflow"]] && options[["flagCriticalTransactions"]] && options[["handleCriticalTransactions"]] == "inspect"){
    
    monetaryVariable <- .jfa.variable.read(options, varType = "monetary")
    auditResult <- .jfa.variable.read(options, varType = "auditResult")
    criticalVariable <- .jfa.variable.read(options, varType = "critical")
    variables <- c(monetaryVariable, auditResult, criticalVariable)
    
    criticalTransactions <- .readDataSetToEnd(columns.as.numeric = variables)
    criticalTransactions <- subset(criticalTransactions, criticalTransactions[, .v(criticalVariable)] > 0)
    
    criticalData <- data.frame(x = as.numeric(rownames(criticalTransactions)),
                               y = criticalTransactions[, .v(options[["monetaryVariable"]])],
                               l = rep(1, nrow(criticalTransactions)),
                               m = criticalTransactions[, .v(options[["auditResult"]])],
                               z = rep(1, nrow(criticalTransactions)))
    colnames(criticalData) <- colnames(sample)
    rownames(criticalData) <- rownames(criticalTransactions)
    
    sample <- rbind(sample, criticalData)
    return(sample)
  } else {
    return(sample)
  }
}

.jfa.sampleSize.plot <- function(options, parentOptions, parentState, parentContainer, jaspResults,
                                 ready, positionInContainer){
  
  if(!options[["decisionPlot"]])
    return()
  
  if(!options[["performanceMateriality"]])
    return()
  
  if(options[["separateKnownAndUnknownMisstatement"]])
    return()
  
  if(!options[["priorConstructionMethod"]] %in% c("none", "arm", "sample", "factor"))
    return()
  
  .jfa.figureNumber.update(jaspResults)
  
  if(is.null(parentContainer[["decisionPlot"]])){
    
    collection <- createJaspContainer(gettext("Comparison of Required Sample Sizes"))
    collection$dependOn(options = c("decisionPlot"))
    collection$position <- positionInContainer
    
    parentContainer[["decisionPlot"]] <- collection
    
    if(!ready || parentContainer$getError())
      return()
    
    confidence <- options[["confidence"]]
    if(!options[["bayesianAnalysis"]])
      confidence <- 1 - .jfa.auditRiskModel.calculation(options)
    
    minPrecision <- NULL
    if(options[["minimumPrecision"]])
      minPrecision <- parentOptions[["minimumPrecision"]]
    
    performanceMateriality <- NULL
    if(options[["performanceMateriality"]])
      performanceMateriality <- parentOptions[["materiality"]]
    
    N <- parentOptions[["populationSize"]]
    if(options[["workflow"]] && options[["monetaryVariable"]] != "")
      N <- ceiling(parentOptions[["populationValue"]])
    if(!options[["workflow"]] && options[["populationValue"]] > 0)
      N <- ceiling(parentOptions[["populationValue"]])
    
    if(options[["bayesianAnalysis"]])
      dist <- base::switch(parentOptions[["likelihood"]], "binomial" = "Beta", "poisson" = "Gamma", "hypergeometric" = "Beta-binomial")
    if(!options[["bayesianAnalysis"]])
      dist <- base::switch(parentOptions[["likelihood"]], "binomial" = "Binomial", "poisson" = "Poisson", "hypergeometric" = "Hypergeometric")
    
    if(is.null(collection[["comparisonDistributions"]])){
      
      # First plot: Comparison across probability distributions
      figure <- createJaspPlot(plot = NULL, title = gettextf("Across Probability Distributions (Current: %1$s)", dist),
                               width = 600, height = 300)
      
      collection[["comparisonDistributions"]] <- figure
      
      likelihoods <- c("binomial", "poisson", "hypergeometric")
      n <- numeric(length(likelihoods))
      k <- numeric(length(likelihoods))
      
      startProgressbar(length(likelihoods))
      
      for(i in 1:length(likelihoods)){
        if(options[["bayesianAnalysis"]]){
          names <- c("Beta", "Gamma", "Beta-binomial")
          ir <- base::switch(options[["IR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["irCustom"]])
          cr <- base::switch(options[["CR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["crCustom"]])
          
          # Create a prior distribution that incorporates the existing information
          prior <- jfa::auditPrior(materiality = performanceMateriality,
                                   confidence = confidence,
                                   expectedError = parentOptions[["expectedErrors"]],
                                   likelihood =  likelihoods[i],
                                   N = N,
                                   ir = ir,
                                   cr = cr,
                                   method = options[["priorConstructionMethod"]],
                                   sampleN = options[["sampleN"]],
                                   sampleK = options[["sampleK"]],
                                   factor = options[["factor"]],
                                   pHplus = 1 - options[["pHmin"]],
                                   pHmin = options[["pHmin"]])
        } else {
          names <- c("Binomial", "Poisson", "Hypergeometric")
          prior <- FALSE
        }
        result <- jfa::planning(materiality = performanceMateriality,
                                confidence = confidence,
                                expectedError = parentOptions[["expectedErrors"]],
                                likelihood = likelihoods[i],
                                minPrecision = minPrecision,
                                N = N,
                                increase = options[["sampleSizeIncrease"]],
                                prior = prior)
        n[i] <- result[["sampleSize"]]
        k[i] <- result[["expectedSampleError"]]
        progressbarTick()
      }
      
      dPlot <- data.frame(y = c(n, k), x = rep(names, 2), type = rep(c(gettext("Expected error-free"), gettext("Expected errors")), each = 3))
      dPlot$x <- factor(x = dPlot$x, levels = levels(dPlot$x)[c(2, 3, 1)])
      dPlot$type <- factor(x = dPlot$type, levels = levels(dPlot$type)[c(1, 2)])
      
      yBreaks <- JASPgraphs::getPrettyAxisBreaks(0:(ceiling(1.1 * max(n))), min.n = 4)
      yLimits <- c(0, ceiling(1.2 * max(n)))
      
      jfaTheme <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                                 axis.ticks.y = ggplot2::element_blank(),
                                 axis.text.y = ggplot2::element_text(hjust = 0),
                                 panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb", size = 0.5),
                                 legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30)))
      
      plot <- ggplot2::ggplot(data = dPlot, mapping = ggplot2::aes(x = x, y = y, fill = type)) +
        ggplot2::geom_bar(stat = "identity", col = "black", size = 1) +
        ggplot2::scale_y_continuous(name = gettext("Required sample size (n)"), breaks = yBreaks, limits = yLimits) +
        ggplot2::coord_flip() +
        ggplot2::annotate("text", y = k, x = c(3, 2, 1), label = k, size = 6, vjust = 0.5, hjust = -0.25) +
        ggplot2::annotate("text", y = n, x = c(3, 2, 1), label = n, size = 6, vjust = 0.5, hjust = -0.75) +
        ggplot2::xlab("") +
        ggplot2::labs(fill = "") +
        ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE))
      plot <- JASPgraphs::themeJasp(plot, sides = "", legend.position = "top") + jfaTheme
      
      figure$plotObject <- plot
    }
    
    if(is.null(collection[["comparisonErrors"]])){
      
      # Second plot: Comparison across expected errors
      figure <- createJaspPlot(plot = NULL, title = gettextf("Across Expected Errors (Current: %1$s)", round(parentState[["expectedSampleError"]], 2)),
                               width = 150, height = 200)
      collection[["comparisonErrors"]] <- figure
      
      if(options[["bayesianAnalysis"]]){
        
        # Create a prior distribution that incorporates the existing information
        prior <- jfa::auditPrior(materiality = performanceMateriality,
                                 confidence = confidence,
                                 expectedError = parentOptions[["expectedErrors"]],
                                 likelihood = parentOptions[["likelihood"]],
                                 N = N,
                                 ir = ir,
                                 cr = cr,
                                 method = options[["priorConstructionMethod"]],
                                 sampleN = options[["sampleN"]],
                                 sampleK = options[["sampleK"]],
                                 factor = options[["factor"]],
                                 pHplus = 1 - options[["pHmin"]],
                                 pHmin = options[["pHmin"]])
      } else {
        prior <- FALSE
      }
      
      n <- numeric(length(1:4))
      for(i in 1:4){
        result <- jfa::planning(materiality = performanceMateriality,
                                confidence = confidence,
                                likelihood = parentOptions[["likelihood"]],
                                expectedError = i - 1,
                                N = N,
                                minPrecision = minPrecision,
                                prior = prior,
                                increase = options[["sampleSizeIncrease"]])
        n[i] <- result[["sampleSize"]]
      }
      
      dPlot <- data.frame(x = c("0", "1", "2", "3"), y = c(2, 2, 2, 2))
      dPlot$x <- factor(dPlot$x, levels = c("3", "2", "1", "0"))
      
      plot <- ggplot2::ggplot(data = dPlot, mapping = ggplot2::aes(x = x, y = y, fill = x)) +
        ggplot2::geom_bar(stat = "identity", col = "black", size = 1) +
        ggplot2::scale_y_continuous(name = "", breaks = NULL, limits = c(0, 2.3)) +
        ggplot2::scale_x_discrete(name = "") +
        ggplot2::scale_fill_manual(values = c("red", "darkorange1", "orange", "#7FE58B")) +
        ggplot2::coord_flip() +
        ggplot2::labs(fill = "") +
        ggplot2::annotate("text", y = c(0.5, 0.5, 0.5, 0.5), x = c(4, 3, 2, 1),
                          label = paste0("n = ", n), size = 6, vjust = 0.5, hjust = 0.2, fontface = "italic")
      plot <- JASPgraphs::themeJasp(plot, sides = "", legend.position = "none") + jfaTheme
      
      figure$plotObject <- plot
    }
    
    if(options[["explanatoryText"]] && ready){
      figureCaption <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The first panel shows a comparison of the required sample sizes under different probability distributions. The bars represent the required sample sizes for the current expected errors in the sample. The number of expected errors in each bar is colored in red and the number of expected error-free transactions is colored in green. The second panel shows a comparison of the required sample sizes under different expected errors for the current probability distribution.",
                                               jaspResults[["figNumber"]]$object), "p")
      
      figureCaption$position <- positionInContainer + 1
      figureCaption$dependOn(optionsFromObject = parentContainer[["decisionPlot"]])
      figureCaption$dependOn(options = "explanatoryText")
      parentContainer[["decisionPlotText"]] <- figureCaption
    }
  }
}

.jfa.samplingDistribution.plot <- function(options, parentOptions, parentState, parentContainer, jaspResults,
                                           ready, positionInContainer){
  
  if(!options[["samplingDistribution"]])
    return()
  
  .jfa.figureNumber.update(jaspResults)
  
  if(is.null(parentContainer[["samplingDistribution"]])){
    
    likelihood <- base::switch(options[["planningModel"]], "Poisson" = "Poisson",
                               "binomial" = "Binomial", "hypergeometric" = "Hypergeometric")
    
    title <- gettextf("Implied %1$s Distribution of Errors", likelihood)
    
    figure <- createJaspPlot(plot = NULL, title = title, width = 600, height = 300)
    figure$position <- positionInContainer
    figure$dependOn(options = c("planningModel",
                                "samplingDistribution"))
    
    parentContainer[["samplingDistribution"]] <- figure
    
    if(!ready || parentContainer$getError())
      return()
    
    xVals <- 0:parentState[["sampleSize"]]
    limx <- parentState[["sampleSize"]] + 1
    if(limx > 31)
      limx <- 31
    xVals <- xVals[1:limx]
    
    if(parentState[["likelihood"]] == "poisson"){
      dErrorFree 	<- stats::dpois(x = xVals, lambda = parentState[["materiality"]] * parentState[["sampleSize"]])
      dError 		<- stats::dpois(x = 0:parentState[["expectedSampleError"]], lambda = parentState[["materiality"]] * parentState[["sampleSize"]])
    } else if(parentState[["likelihood"]] == "binomial"){
      dErrorFree 	<- stats::dbinom(x = xVals, size = parentState[["sampleSize"]], prob = parentState[["materiality"]])
      dError 		<- stats::dbinom(x = 0:parentState[["expectedSampleError"]], size = parentState[["sampleSize"]], prob = parentState[["materiality"]])
    } else if(parentState[["likelihood"]] == "hypergeometric"){
      dErrorFree 	<- stats::dhyper(x = xVals, m = parentState[["populationK"]], n = parentState[["N"]] - parentState[["populationK"]], k = parentState[["sampleSize"]])
      dError 		<- stats::dhyper(x = 0:parentState[["expectedSampleError"]], m = parentState[["populationK"]], n = parentState[["N"]] - parentState[["populationK"]], k = parentState[["sampleSize"]])
    }
    
    dataErrorFree 	<- data.frame(x = xVals, y = dErrorFree)
    dataError 		<- data.frame(x = 0:parentState[["expectedSampleError"]], y = dError)
    dataLegend 		<- data.frame(x = c(0, 0), y = c(0, 0), type = c(gettext("Expected error-free"), gettext("Expected errors")))
    dataLegend$type <- factor(x = dataLegend[["type"]], levels = levels(dataLegend[["type"]])[c(2,1)])
    
    xTicks <- JASPgraphs::getPrettyAxisBreaks(c(0, xVals))
    yTicks <- JASPgraphs::getPrettyAxisBreaks(c(0, dataErrorFree[["y"]]))
    
    jfaLegend <- ggplot2::guide_legend(override.aes = list(size = 12, shape = 22, fill = c("#FF6666", "#7FE58B"),
                                                           stroke = 1.5, color = "black"))
    
    jfaTheme <- ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", size = 0.5),
                               legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30)))
    
    plot <- ggplot2::ggplot(data = dataLegend, mapping = ggplot2::aes(x = x, y = y, fill = type)) +
      ggplot2::geom_point(shape = 2, alpha = 0) +
      ggplot2::scale_x_continuous(name = gettext("Errors"), labels = xTicks, breaks = xTicks) +
      ggplot2::scale_y_continuous(name = gettext("Probability"), labels = yTicks, breaks = yTicks, limits = c(0, max(yTicks))) +
      ggplot2::geom_bar(data = dataErrorFree, mapping = ggplot2::aes(x = x, y = y), stat = "identity",
                        fill = "#7FE58B", size = 0.5, color = "black") +
      ggplot2::geom_bar(data = dataError, mapping = ggplot2::aes(x = x, y = y), stat = "identity",
                        fill = "#FF6666", size = 0.5, color = "black") +
      ggplot2::geom_point(data = dataLegend, mapping = ggplot2::aes(x = x, y = y, fill = type), size = 0) +
      ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::labs(fill = "") +
      ggplot2::guides(fill = jfaLegend)
    
    plot <- JASPgraphs::themeJasp(plot, legend.position = "top") + jfaTheme
    figure$plotObject <- plot
  }
  
  if(options[["explanatoryText"]] && ready){
    
    figureCaption <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The implied <b>%2$s</b> distribution. The number of expected errors in the selection is colored in red and the number of expected error-free observations is colored in green. The total probability of the errors does not exceed the detection risk as specified through the audit risk model.",
                                             jaspResults[["figNumber"]]$object,
                                             options[["planningModel"]]), "p")
    
    figureCaption$position <- positionInContainer + 1
    figureCaption$dependOn(optionsFromObject = parentContainer[["samplingDistribution"]])
    figureCaption$dependOn(options = "explanatoryText")
    parentContainer[["samplingDistributionText"]] <- figureCaption
  }
}

################################################################################
################## Common functions for the selection stage ####################
################################################################################

.jfa.selectionResult.add <- function(options, jaspResults){
  
  dataset <- .jfa.dataset.read(options, jaspResults, stage = "procedure")
  
  rankingVariable <- .jfa.variable.read(options, varType = "ranking")
  additionalVariables <- .jfa.variable.read(options, varType = "additional")
  additionalVariables <- additionalVariables[!(additionalVariables%in%.unv(colnames(dataset)))]
  if(length(additionalVariables) == 0)
    additionalVariables <- NULL
  variables <- c(rankingVariable, additionalVariables)
  if(!is.null(variables)){
    additionalColumns <- .readDataSetToEnd(columns.as.numeric = variables)
    dataset <- cbind(dataset, additionalColumns)
    return(dataset)
  } else {
    return(dataset)
  }
}

.jfa.selectionIndicator.add <- function(options, prevOptions, parentState, jaspResults){
  
  if(!options[["addSampleIndicator"]] || options[["sampleIndicatorColumn"]] == "")
    return()
  
  if(is.null(jaspResults[["sampleIndicatorColumn"]])){
    
    sampleIndicatorColumn <- numeric(length = prevOptions[["populationSize"]])
    sampleRowNumbers <- parentState[["rowNumber"]]
    sampleCounts <- parentState[["count"]]
    sampleIndicatorColumn[sampleRowNumbers] <- sampleCounts
    jaspResults[["sampleIndicatorColumn"]] <- createJaspColumn(columnName = options[["sampleIndicatorColumn"]])
    jaspResults[["sampleIndicatorColumn"]]$dependOn(options = c("recordNumberVariable",
                                                                "monetaryVariable",
                                                                "additionalVariables",
                                                                "rankingVariable",
                                                                "selectionMethod",
                                                                "selectionType",
                                                                "seed",
                                                                "intervalStartingPoint",
                                                                "sampleSize",
                                                                "addSampleIndicator",
                                                                "sampleIndicatorColumn"))
    jaspResults[["sampleIndicatorColumn"]]$setNominal(sampleIndicatorColumn)
  }
}

.jfa.selection.state <- function(options, dataset, prevState, parentContainer){
  
  if(!is.null(parentContainer[["selectionState"]])){
    
    return(parentContainer[["selectionState"]]$object)
    
  } else if(!is.null(prevState)){
    
    result <- try({
      .jfa.selection.calculation(options, dataset, prevState, parentContainer)
    })
    
    if(isTryError(result)){
      if(options[["selectionType"]] == "musSampling"){
        # MUS has failed for some reason, fall back to record sampling
        result <- try({
          .jfa.selection.calculation(options, dataset, prevState, parentContainer, unitsExtra = "records")
        })
      }
      if(isTryError(result)){
        parentContainer$setError(gettextf("An error occurred: %1$s", JASP:::.extractErrorMessage(result)))
        return()
      } else {
        # MUS has failed for some reason, return an indication for this
        result[["musFailed"]] <- TRUE
      }
    }
    
    parentContainer[["selectionState"]] <- createJaspState(result)
    return(result)
  }
}

.jfa.selection.calculation <- function(options, dataset, prevState, parentContainer, unitsExtra = NULL){
  
  if(options[["recordNumberVariable"]] == "")
    return(NULL)
  
  if(!is.null(unitsExtra)){
    units <- unitsExtra
  } else {
    units <- base::switch(options[["selectionType"]], "recordSampling" = "records", "musSampling" = "mus")
  }
  
  algorithm <- base::switch(options[["selectionMethod"]], "randomSampling" = "random",
                            "cellSampling" = "cell", "systematicSampling" = "interval")
  
  if(options[["rankingVariable"]] != ""){
    rank <- dataset[, .v(options[["rankingVariable"]])]
    dataset <- dataset[order(rank), ]
  }
  
  if(options[["monetaryVariable"]] != ""){
    bookValues <- .v(options[["monetaryVariable"]])
  } else {
    bookValues <- NULL
  }
  
  if(prevState[["sampleSize"]] == 0 || is.null(dataset))
    return()
  
  if(algorithm == "interval"){
    interval <- base::switch(units, "records" = nrow(dataset) / prevState[["sampleSize"]],
                             "mus" = sum(dataset[, bookValues]) / prevState[["sampleSize"]])
    if(options[["seed"]] > interval){
      parentContainer$setError(gettext("Your specified starting point lies outside the selection interval."))
      return()
    }
  }
  
  startingPointSeed <- options[["seed"]]
  if(!is.null(prevState[["startingPoint"]])){
    startingPointSeed <- prevState[["startingPoint"]]
  }
  
  if(options[["shufflePopulationBeforeSampling"]]){
    names <- colnames(dataset)
    dataset <- as.data.frame(dataset[sample(1:nrow(dataset), size = nrow(dataset), replace = FALSE), ])
    colnames(dataset) <- names
  }
  
  sample <- jfa::selection(population = dataset,
							sampleSize = prevState[["sampleSize"]],
							algorithm = algorithm,
							units = units,
							seed = options[["seed"]],
							ordered = FALSE,
							bookValues = bookValues,
							intervalStartingPoint = startingPointSeed)
  
  sample <- data.frame(sample[["sample"]])
  sample[, 1:2] <- apply(X = sample[, 1:2], MARGIN = 2, as.numeric)
  sample[, .v(options[["recordNumberVariable"]])] <- as.character(sample[, .v(options[["recordNumberVariable"]])])
  
  return(sample)
}

.jfa.selection.table <- function(options, dataset, prevOptions, prevState, parentState, parentContainer,
                                 jaspResults, positionInContainer){
  
  .jfa.tableNumber.update(jaspResults)
  
  if(!is.null(parentContainer[["selectionInformationTable"]]))
    return()
  
  title <- gettextf("<b>Table %1$i.</b> Selection Summary", jaspResults[["tabNumber"]]$object)
  
  table <- createJaspTable(title)
  table$position <- positionInContainer
  table$dependOn(options = c("bookValueDescriptives",
                             "sampleDescriptives",
                             "displaySample",
                             "samplingChecked",
                             "evaluationChecked"))
  
  table$addColumnInfo(name = "size", 				title = gettext("Selected sampling units"), type = "integer")
  table$addColumnInfo(name = "transactions", 		title = gettext("Selected transactions"), type = "integer")
  if(options[["selectionType"]] == "musSampling"){
    table$addColumnInfo(name = "value", 		title = gettext("Selection value"), type = "string")
    table$addColumnInfo(name = "percentage", 	title = gettextf("%% of population value"), type = "string")
  } else {
    table$addColumnInfo(name = "percentage", 	title = gettextf("%% of total observations"), type = "string")
  }
  if(options[["selectionMethod"]] != "randomSampling")
    table$addColumnInfo(name = "interval", 		title ="Interval", type = "string")
  
  if(options[["selectionMethod"]] != "systematicSampling"){
    message <- gettextf("The sample is drawn with random number generator <i>seed %1$s</i>.", options[["seed"]])
  } else {
    startingPointSeed <- options[["seed"]]
    if(!is.null(prevState[["startingPoint"]]))
      startingPointSeed <- prevState[["startingPoint"]]
    message <- gettextf("Sampling unit %1$s is selected from each interval.", startingPointSeed)
  }
  table$addFootnote(message)
  
  parentContainer[["selectionInformationTable"]] <- table
  
  if(is.null(parentState))
    return()
  
  if(options[["selectionType"]] == "recordSampling" || !is.null(parentState[["musFailed"]])){
    interval <- (prevOptions[["populationSize"]] / prevState[["sampleSize"]])
  } else {
    interval <- (prevOptions[["populationValue"]] / prevState[["sampleSize"]])
  }
  
  sampleSize <- sum(parentState[["count"]])
  
  if(options[["selectionType"]] == "musSampling"){
    value <- round(sum(abs(parentState[, .v(options[["monetaryVariable"]])])), 2)
    percentage <- paste0(round(value / prevOptions[["populationValue"]] * 100, 2), "%")
    row  <- data.frame("size" = sampleSize,
                       "transactions" = nrow(parentState),
                       "value" = paste(prevOptions[["valuta"]], value),
                       "percentage" = percentage)
  } else {
    percentage <- paste0(round(sampleSize / prevOptions[["populationSize"]] * 100, 2), "%")
    row <- data.frame("size" = sampleSize, "transactions" = nrow(parentState), "percentage" = percentage)
  }
  
  if(options[["selectionMethod"]] != "randomSampling"){
    if(options[["selectionType"]] == "musSampling" && is.null(parentState[["musFailed"]])){
      row <- cbind(row, interval = paste(prevOptions[["valuta"]], round(interval, 2)))
    } else {
      row <- cbind(row, interval = round(interval, 2))
    }
  }
  
  table$addRows(row)
}

.jfa.selectionInterval.table <- function(options, dataset, prevOptions, prevState, parentContainer,
                                         parentState, jaspResults, positionInContainer){
  
  if(options[["recordNumberVariable"]] == "")
    return()
  
  if(options[["selectionType"]] == "musSampling" && options[["selectionMethod"]] != "randomSampling"){
    
    .jfa.tableNumber.update(jaspResults)
    
    if(!is.null(parentContainer[["stratumTable"]]))
      return()
    
    title <- gettextf("<b>Table %1$i.</b> Information about Monetary Interval Selection", jaspResults[["tabNumber"]]$object)
    table <- createJaspTable(title)
    table$position <- positionInContainer + 1
    table$dependOn(options = c("bookValueDescriptives",
                               "sampleDescriptives",
                               "displaySample",
                               "samplingChecked",
                               "evaluationChecked"))
    
    table$addColumnInfo(name = "stratum", 			title = "", type = "string")
    table$addColumnInfo(name = "size", 				title = gettext("Transactions"), type = "integer")
    table$addColumnInfo(name = "value", 			title = gettext("Total value"), type = "string")
    table$addColumnInfo(name = "unitSize", 			title = gettext("Selected units"), type = "integer")
    table$addColumnInfo(name = "sampleSize", 		title = gettext("Selected transactions"), type = "integer")
    table$addColumnInfo(name = "sampleValue", 		title = gettext("Selection value"), type = "string")
    table$addColumnInfo(name = "stratumPercentage", title = gettext("% of total value"), type = "string")
    
    parentContainer[["stratumTable"]] <- table
    
    sampleSize <- prevState[["sampleSize"]]
    
    if(sampleSize == 0){
      row <- data.frame(stratum = c("Total population", "Ist value > Interval", "Ist value < Interval"),
                        size = c(nrow(dataset), ".", "."),
                        value = c(paste(prevOptions[["valuta"]], round(prevOptions[["populationValue"]], 2)),".", "."),
                        unitSize = rep(".", 3),
                        sampleSize = rep(".", 3),
                        sampleValue = rep(".", 3),
                        stratumPercentage = rep(".", 3))
      table$setData(row)
      return()
    }
    
    interval <- round(prevOptions[["populationValue"]] / prevState[["sampleSize"]], 2)
    
    topStratum <- dataset[which(dataset[, .v(options[["monetaryVariable"]])] > interval), ]
    bottomStratum <- dataset[which(dataset[, .v(options[["monetaryVariable"]])] <= interval), ]
    bottomStratumSample <- parentState[which(parentState[, .v(options[["monetaryVariable"]])] <= interval), ]
    
    topStratumValue <- round(sum(topStratum[, .v(options[["monetaryVariable"]])]), 2)
    bottomStratumValuePopulation <- round(sum(bottomStratum[, .v(options[["monetaryVariable"]])]), 2)
    bottomStratumValueSample <- round(sum(bottomStratumSample[, .v(options[["monetaryVariable"]])]), 2)
    
    row <- data.frame(stratum = c("Total population", "Ist value > Interval", "Ist value < Interval"),
                      size = c(nrow(dataset), nrow(topStratum), nrow(dataset) - nrow(topStratum)),
                      value = c(paste(prevOptions[["valuta"]], round(prevOptions[["populationValue"]], 2)),
                                paste(prevOptions[["valuta"]], topStratumValue),
                                paste(prevOptions[["valuta"]], bottomStratumValuePopulation)),
                      unitSize = c(sampleSize, sampleSize - nrow(bottomStratumSample), nrow(bottomStratumSample)),
                      sampleSize = c(nrow(topStratum) + nrow(bottomStratumSample), nrow(topStratum), nrow(bottomStratumSample)),
                      sampleValue = c(paste(prevOptions[["valuta"]], bottomStratumValueSample + topStratumValue),
                                      paste(prevOptions[["valuta"]], topStratumValue),
                                      paste(prevOptions[["valuta"]], bottomStratumValueSample)),
                      stratumPercentage = c(paste0(round((bottomStratumValueSample + topStratumValue) / prevOptions[["populationValue"]] * 100, 2), "%"),
                                            "100%",
                                            paste0(round(bottomStratumValueSample / bottomStratumValuePopulation * 100, 2), "%")))
    
    table$setData(row)
  }
}

.jfa.selectionSample.table <- function(options, prevOptions, parentState, parentContainer, jaspResults,
                                       positionInContainer){
  
  if(!options[["displaySample"]])
    return()
  
  .jfa.tableNumber.update(jaspResults)
  
  if(is.null(parentContainer[["selectionSampleTable"]])){
    
    title <- gettextf("<b>Table %1$i.</b> Selected Transactions", jaspResults[["tabNumber"]]$object)
    table <- createJaspTable(title)
    table$position <- positionInContainer
    table$dependOn(options = c("bookValueDescriptives",
                               "sampleDescriptives",
                               "displaySample",
                               "samplingChecked",
                               "evaluationChecked"))
    
    recordNumberVariable  <- .jfa.variable.read(options, varType = "recordNumber")
    monetaryVariable      <- .jfa.variable.read(options, varType = "monetary")
    rankingVariable       <- .jfa.variable.read(options, varType = "ranking")
    additionalVariables   <- .jfa.variable.read(options, varType = "additional")
    columnNames           <- c("Row number", "Count", unique(c(recordNumberVariable, monetaryVariable, 
                                                               rankingVariable, additionalVariables)))
    
    for(i in columnNames){
      table$addColumnInfo(name = i, type = "string", title = i)
    }
    
    parentContainer[["sampleTable"]] <- table
    
    if(is.null(parentState) || parentContainer$getError())
      return()
    
    parentState <- as.data.frame(parentState)
    
    columns <- numeric()
    for(i in 1:length(columnNames)){
      if(i == 1){
        columns <- cbind(columns, parentState[, "rowNumber"])
      } else if(i == 2){
        columns <- cbind(columns, parentState[, "count"])
      } else {
        columns <- cbind(columns, parentState[, .v(columnNames[i])])
      }
      colnames(columns)[i] <- columnNames[i]
    }
    
    table$setData(columns)
  }
}

.jfa.selectionDescriptives.table <- function(options, parentState, parentContainer, jaspResults,
                                             positionInContainer){
  
  if(!options[["sampleDescriptives"]])
    return()
  
  .jfa.tableNumber.update(jaspResults)
  
  if(is.null(parentContainer[["sampleDescriptivesTable"]])){
    
    recordVariable 		<- .jfa.variable.read(options, varType = "record")
    rankingVariable 	<- .jfa.variable.read(options, varType = "ranking")
    monetaryVariable 	<- .jfa.variable.read(options, varType = "monetary")
    additionalVariables <- .jfa.variable.read(options, varType = "additional")
    variables 			<- unique(c(rankingVariable, monetaryVariable, additionalVariables))
    
    title <- gettextf("<b>Table %1$i.</b> Descriptive Statistics for Selected Transactions",
                      jaspResults[["tabNumber"]]$object)
    
    table <- createJaspTable(title)
    table$transpose <- TRUE
    table$position <- positionInContainer
    table$dependOn(options = c("sampleDescriptives",
                               "mean",
                               "sd",
                               "var",
                               "range",
                               "min",
                               "max",
                               "median",
                               "bookValueDescriptives",
                               "sampleDescriptives",
                               "displaySample",
                               "samplingChecked",
                               "evaluationChecked"))
    
    table$addColumnInfo(name = "name", title = "", type = "string")
    table$addColumnInfo(name = "cases", title = gettext("Valid cases"), type = "integer")
    if (options[["mean"]])		table$addColumnInfo(name = "Mean", 				title = gettext("Mean"), type = "number")
    if (options[["median"]])  	table$addColumnInfo(name = "Median", 			title = gettext("Median"), type = "number")
    if (options[["sd"]])  		table$addColumnInfo(name = "Std. Deviation", 	title = gettext("Std. Deviation"), type = "number")
    if (options[["var"]])		table$addColumnInfo(name = "Variance", 			title = gettext("Variance"), type = "number")
    if (options[["range"]]) 	table$addColumnInfo(name = "Range", 			title = gettext("Range"), type = "number")
    if (options[["min"]]) 		table$addColumnInfo(name = "Minimum", 			title = gettext("Minimum"), type = "number")
    if (options[["max"]])    	table$addColumnInfo(name = "Maximum", 			title = gettext("Maximum"), type = "number")
    
    parentContainer[["sampleDescriptivesTable"]]   <- table
    
    if(is.null(parentState) || parentContainer$getError())
      return()
    
    for (variable in variables) {
      column <- parentState[[ .v(variable) ]]
      row <- list()
      row[["name"]] <- variable
      row[["cases"]] <- base::length(column)
      if(!is.factor(column)){
        if(options[["mean"]])  	row[["Mean"]]          	<- base::mean(column, na.rm = TRUE)
        if(options[["sd"]])    	row[["Std. Deviation"]]	<- stats::sd(column, na.rm = TRUE)
        if(options[["var"]])   	row[["Variance"]]      	<- stats::var(column, na.rm = TRUE)
        if(options[["median"]])	row[["Median"]]        	<- stats::median(column, na.rm = TRUE)
        if(options[["range"]]) 	row[["Range"]]         	<- base::abs(base::range(column, na.rm = TRUE)[1] - base::range(column, na.rm = TRUE)[2])
        if(options[["min"]])   	row[["Minimum"]]       	<- base::min(column, na.rm = TRUE)
        if(options[["max"]])   	row[["Maximum"]]       	<- base::max(column, na.rm = TRUE)
      }
      table$addRows(row)
    }
  }
}

################################################################################
################## Common functions for the evaluation #########################
################################################################################

.jfa.evaluationResult.add <- function(options, jaspResults){
  
  dataset <- .jfa.selectionResult.add(options, jaspResults)
  
  sampleFilter 	<- options[["sampleFilter"]]
  auditResult 	<- options[["auditResult"]]
  critical 		<- options[["criticalTransactions"]]
  variables 		<- c(sampleFilter, auditResult, critical)
  
  if(!("" %in% variables)){
    additionalColumns <- .readDataSetToEnd(columns.as.numeric = variables)
    dataset <- cbind(dataset, additionalColumns)
    return(dataset)
  } else {
    return(dataset)
  }
}

.jfa.evaluation.state <- function(options, sample, prevOptions, parentContainer, prevState){
  
  if(options[["auditResult"]] == "")
    return()
  
  if(!is.null(parentContainer[["evaluationState"]])){
    
    return(parentContainer[["evaluationState"]]$object)
    
  } else {
    
    # Add critical transactions to the sample
    sample <- .jfa.criticalTransactions.add(options, sample)
    
    auditRisk <- 1 - options[["confidence"]]
    prior <- NULL
    
    materiality <- NULL
    if(options[["performanceMateriality"]])
      materiality <- prevOptions[["materiality"]]
    
    if(options[["priorConstructionMethod"]] == "arm"){
      ir <- base::switch(options[["IR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["irCustom"]])
      cr <- base::switch(options[["CR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["crCustom"]])
      dr <- auditRisk / (ir * cr)
      confidence <- 1 - dr
    }
    
    if(options[["bayesianAnalysis"]]){
      
      prior <- jfa::auditPrior(materiality = materiality,
                               confidence = prevOptions[["confidence"]],
                               expectedError = prevOptions[["expectedErrors"]],
                               likelihood = prevOptions[["likelihood"]],
                               N = prevOptions[["populationSize"]],
                               ir = ir,
                               cr = cr,
                               method = options[["priorConstructionMethod"]],
                               sampleN = options[["sampleN"]],
                               sampleK = options[["sampleK"]],
                               factor = options[["factor"]],
                               pHplus = 1 - options[["pHmin"]],
                               pHmin = options[["pHmin"]])
      
      confidence <- options[["confidence"]]
      
      if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != ""){
        
        result <- .jfa.separatedMisstatementEvaluation.state(options, sample, prior, prevOptions,
                                                             prevState, parentContainer)
        
        return(result)
      }
    }
    
    # Select evaluation method
    if(options[["variableType"]] == "variableTypeCorrect"){
      
      method <- options[["planningModel"]]
      if(method == "Poisson")
        method <- "poisson"
      
      result <- try({
        # call jfa evaluation
        jfa::evaluation(sample = sample,
                        counts = prevState[["count"]],
                        confidence = confidence,
                        nSumstats = nrow(sample),
                        kSumstats = length(which(sample[, .v(options[["auditResult"]])] == 1)),
                        method = method,
                        materiality = materiality,
                        N = prevOptions[["populationSize"]],
                        prior = prior)
      })
      
    } else if(options[["variableType"]] == "variableTypeAuditValues"){
      
      method <- base::switch(options[["estimator"]],
                             "stringerBound"     = "stringer",
                             "regressionBound"   = "regression",
                             "directBound"       = "direct",
                             "differenceBound"   = "difference",
                             "ratioBound"        = "quotient",
                             "coxAndSnellBound"  = "coxsnell",
                             "betaBound"         = "binomial",
                             "gammaBound"        = "poisson",
                             "betabinomialBound" = "hypergeometric")
      
      if(method == "stringer" && options[["stringerBoundLtaAdjustment"]])
        method <- "stringer-lta"
      
      # Bayesian regression is not implemented in jfa R package (will become WASEM)
      if(options[["bayesianAnalysis"]] && method == "regression"){
        
        result <- try({
          .jfa.bayesRegression.calculation(options, sample, prevOptions)
        })
        
      } else {
        
        result <- try({
          # call jfa evaluation
          jfa::evaluation(sample = sample,
                          counts = prevState[["count"]],
                          confidence = confidence,
                          bookValues = .v(options[["monetaryVariable"]]),
                          auditValues = .v(options[["auditResult"]]),
                          method = method,
                          materiality = materiality,
                          N = prevOptions[["populationSize"]],
                          populationBookValue = prevOptions[["populationValue"]],
                          prior = prior)
        })
      }
    }
    
    if(isTryError(result)){
      parentContainer$setError(paste0("An error occurred: ", JASP:::.extractErrorMessage(result)))
      return()
    }
    
    if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){
      result[["confBound"]] <- (prevOptions[["populationValue"]] - result[["lowerBound"]]) / prevOptions[["populationValue"]]
      if(result[["confBound"]] < prevOptions[["materiality"]]){
        result[["conclusion"]] <- "Approve population"
      } else {
        result[["conclusion"]] <- "Do not approve population"
      }
    }
    
    parentContainer[["evaluationState"]] <- createJaspState(result)
    return(result)
  }
}

.jfa.explanatoryTextEvaluation.add <- function(options, planningOptions, planningState, selectionState,
                                               evaluationContainer, positionInContainer = 1){
  
  if(options[["explanatoryText"]]){
    
    ready <- FALSE
    if((options[["variableType"]] == "variableTypeCorrect" && !options[["useSumStats"]] && options[["auditResult"]] != "" && options[["recordNumberVariable"]] != "" && ((options[["performanceMateriality"]] && planningOptions[["materiality"]] > 0) || (options[["minimumPrecision"]] && options[["minimumPrecisionPercentage"]] > 0))) ||
       (options[["variableType"]] == "variableTypeAuditValues" && !options[["useSumStats"]] && options[["auditResult"]] != "" && options[["recordNumberVariable"]] != "" && options[["monetaryVariable"]] != "" && ((options[["performanceMateriality"]] && planningOptions[["materiality"]] > 0) || (options[["minimumPrecision"]] && options[["minimumPrecisionPercentage"]] > 0))) ||
       (options[["variableType"]] == "variableTypeCorrect" && options[["useSumStats"]] && options[["nSumStats"]] > 0 && ((options[["performanceMateriality"]] && planningOptions[["materiality"]] > 0) || (options[["minimumPrecision"]] && options[["minimumPrecisionPercentage"]] > 0)))){
      ready <- TRUE
    }
    
    if(ready){
      
      evaluationState 	<- evaluationContainer[["evaluationState"]]$object
      errorLabel 		<- evaluationState[["k"]]
      
      if(options[["display"]] == "displayNumbers"){
		  if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){
		    boundLabel <- round(evaluationState[["upperBound"]] / planningOptions[["populationSize"]], 3)
        	mleLabel <- round(evaluationState[["mle"]] / planningOptions[["populationSize"]], 3)
        	precisionLabel <- round(evaluationState[["precision"]] / planningOptions[["populationSize"]], 3)
		  } else {
		    boundLabel <- round(evaluationState[["confBound"]], 3)
        	mleLabel <- round(evaluationState[["mle"]], 3)
        	precisionLabel <- round(evaluationState[["precision"]], 3)
		  }
      } else if(options[["display"]] == "displayPercentages"){
		  if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){
        	boundLabel <- paste0(round(evaluationState[["upperBound"]] / planningOptions[["populationValue"]] * 100, 3), "%")
        	mleLabel <- paste0(round(evaluationState[["mle"]] / planningOptions[["populationValue"]] * 100, 3), "%")
        	precisionLabel <- paste0(round(evaluationState[["precision"]] / planningOptions[["populationValue"]] * 100, 3), "%")  
		  } else {
        	boundLabel <- paste0(round(evaluationState[["confBound"]] * 100, 3), "%")
        	mleLabel <- paste0(round(evaluationState[["mle"]] * 100, 3), "%")
        	precisionLabel <- paste0(round(evaluationState[["precision"]] * 100, 3), "%")  
		  }
      } else if(options[["display"]] == "displayValues"){
		  if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){
        	boundLabel <- paste(planningOptions[["valuta"]], round(evaluationState[["upperBound"]], 2))
        	mleLabel <- paste(planningOptions[["valuta"]], round(evaluationState[["mle"]], 2))
        	precisionLabel <- paste(planningOptions[["valuta"]], round(evaluationState[["precision"]], 2))  
		  } else {
        	boundLabel <- paste(planningOptions[["valuta"]], round(evaluationState[["confBound"]] * planningOptions[["populationValue"]], 2))
        	mleLabel <- paste(planningOptions[["valuta"]], round(evaluationState[["mle"]] * planningOptions[["populationValue"]], 2))
        	precisionLabel <- paste(planningOptions[["valuta"]], round(evaluationState[["precision"]] * planningOptions[["populationValue"]], 2))  
		  }
      }
      
    } else {
      
      boundLabel <- errorLabel <- mleLabel <- precisionLabel <- "..."
      
    }
    
    if(sum(selectionState[["count"]]) > nrow(selectionState)){
      sampleSizeMessage <- paste0(planningState[["sampleSize"]],
                                  " (",
                                  nrow(selectionState),
                                  " + ",
                                  sum(selectionState[["count"]][which(selectionState[["count"]] != 1)] - 1),
                                  ")")
    } else {
      sampleSizeMessage <- planningState[["sampleSize"]]
    }
    
    if(!options[["bayesianAnalysis"]]){
      additionalMessage <- gettextf("\n\nThe cumulative knowledge states that there is a %1$s probability that, when one would repeatedly sample from this population, the upper bound on the misstatement in the population is lower than <b>%2$s</b> with a precision of <b>%3$s</b>.",
                                    planningOptions[["confidenceLabel"]],
                                    boundLabel,
                                    precisionLabel)
    } else if(options[["bayesianAnalysis"]]){
      additionalMessage <- gettextf("\n\nThe cumulative knowledge states that there is a %1$s probability that the misstatement in the population is lower than <b>%2$s</b> with a precision of <b>%3$s</b>.",
                                    planningOptions[["confidenceLabel"]],
                                    boundLabel,
                                    precisionLabel)
    }
    
    message <- gettextf("The selection consisted of <b>%1$s</b> sampling units, of which a total of <b>%2$s</b> were misstated. The information from this sample combined with the prior information results in a most likely error in the population of <b>%3$s</b> and an %4$s upper bound of <b>%5$s</b>. %6$s",
                        sampleSizeMessage,
                        errorLabel,
                        mleLabel,
                        planningOptions[["confidenceLabel"]],
                        boundLabel,
                        additionalMessage)
    
    evaluationContainer[["evaluationParagraph"]] <- createJaspHtml(message, "p")
    evaluationContainer[["evaluationParagraph"]]$position <- positionInContainer
    evaluationContainer[["evaluationParagraph"]]$dependOn(options = "explanatoryText")
  }
}

.jfa.previousStagePlanning.state <- function(options, dataset, evaluationOptions){
  
  if(((options[["variableType"]] == "variableTypeAuditValues" && options[["recordNumberVariable"]] != "" && options[["monetaryVariable"]] != "" && options[["auditResult"]] != "") ||
      (options[["variableType"]] == "variableTypeCorrect" && options[["recordNumberVariable"]] != "" && options[["auditResult"]] != "") ||
      (options[["variableType"]] == "variableTypeCorrect" && options[["useSumStats"]] && options[["nSumStats"]] > 0)) &&
     ((options[["performanceMateriality"]] && evaluationOptions[["materiality"]] != 0) || (options[["minimumPrecision"]] && options[["minimumPrecisionPercentage"]] > 0))){
    
    performanceMateriality <- NULL
    if(options[["performanceMateriality"]])
      performanceMateriality <- evaluationOptions[["materiality"]]
    
    minPrecision <- NULL
    if(options[["minimumPrecision"]])
      minPrecision <- options[["minimumPrecisionPercentage"]]
    
    if(!options[["bayesianAnalysis"]]){
      
      planningState <- list()
      planningState[["sampleSize"]] <- ifelse(options[["useSumStats"]],
                                              yes = options[["nSumStats"]],
                                              no = nrow(dataset))
      return(planningState)
      
    } else if(options[["bayesianAnalysis"]]){
      
      inherentRisk <- base::switch(options[["IR"]],
                                   "High" = 1,
                                   "Medium" = 0.60,
                                   "Low" = 0.36,
                                   "Custom" = options[["irCustom"]])
      
      controlRisk <- base::switch(options[["CR"]],
                                  "High" = 1,
                                  "Medium" = 0.60,
                                  "Low" = 0.36,
                                  "Custom" = options[["crCustom"]])
      
      prior <- jfa::auditPrior(materiality = performanceMateriality,
                               confidence = evaluationOptions[["confidence"]],
                               expectedError = evaluationOptions[["expectedErrors"]],
                               likelihood = evaluationOptions[["likelihood"]],
                               N = evaluationOptions[["populationSize"]],
                               ir = inherentRisk,
                               cr = controlRisk,
                               method = options[["priorConstructionMethod"]],
                               sampleN = options[["sampleN"]],
                               sampleK = options[["sampleK"]],
                               factor = options[["factor"]],
                               pHplus = 1 - options[["pHmin"]],
                               pHmin = options[["pHmin"]])
      
      if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != "" && options[["auditResult"]] != ""){
        
        result <- .jfa.separatedMisstatementPlanning.state(options, dataset, prior, evaluationOptions)
        return(result)
      }
      
      planningState <- jfa::planning(materiality = performanceMateriality,
                                     confidence = evaluationOptions[["confidence"]],
                                     expectedError = evaluationOptions[["expectedErrors"]],
                                     N = evaluationOptions[["populationSize"]],
                                     minPrecision = minPrecision,
                                     prior = prior,
                                     increase = 1)
      
      planningState[["sampleSize"]] <- ifelse(options[["useSumStats"]],
                                              yes = options[["nSumStats"]],
                                              no = nrow(dataset))
      
      return(planningState)
      
    }
    
  } else {
    
    planningState <- list()
    planningState[["sampleSize"]] <- "..."
    return(planningState)
    
  }
}

.jfa.evaluationAnalysis.state <- function(options,
                                          sample,
                                          planningOptions,
                                          evaluationContainer){
  

  # Check whether there is enough data to perform an analysis
  if(!options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
    return()
  } else if(options[["performanceMateriality"]] && planningOptions[["materiality"]] == 0){
    return()
  } else if(options[["variableType"]] == "variableTypeCorrect" && !options[["useSumStats"]] &&
            (options[["auditResult"]] == "" || options[["recordNumberVariable"]] == "")){
    return()
  } else if(options[["variableType"]] == "variableTypeAuditValues" && !options[["useSumStats"]] &&
            (options[["auditResult"]] == "" || options[["recordNumberVariable"]] == "" || options[["monetaryVariable"]] == "")){
    return()
  } else if(options[["useSumStats"]] && options[["nSumStats"]] == 0){
    return()
  }
  
  if(!is.null(evaluationContainer[["evaluationState"]])){
    
    return(evaluationContainer[["evaluationState"]]$object)
    
  } else {
    
    auditRisk <- 1 - options[["confidence"]]
    
    inherentRisk <- base::switch(options[["IR"]],
                                 "High" = 1,
                                 "Medium" = 0.60,
                                 "Low" = 0.36,
                                 "Custom" = options[["irCustom"]])
    
    controlRisk <- base::switch(options[["CR"]],
                                "High" = 1,
                                "Medium" = 0.60,
                                "Low" = 0.36,
                                "Custom" = options[["crCustom"]])
    
    minPrecision <- NULL
    if(options[["minimumPrecision"]])
      minPrecision <- options[["minimumPrecisionPercentage"]]
    
    performanceMateriality <- NULL
    if(options[["performanceMateriality"]])
      performanceMateriality <- planningOptions[["materiality"]]
    
    N <- planningOptions[["populationSize"]]
    if(options[["workflow"]] && options[["monetaryVariable"]] != "")
      N <- ceiling(planningOptions[["populationValue"]])
    if(!options[["workflow"]] && options[["populationValue"]] > 0)
      N <- ceiling(planningOptions[["populationValue"]])
    
    if(!options[["bayesianAnalysis"]]){
      
      detectionRisk <- (1 - options[["confidence"]]) / inherentRisk / controlRisk
      confidence <- 1 - detectionRisk
      prior <- FALSE
      
    } else if(options[["bayesianAnalysis"]]){
      
      confidence <- options[["confidence"]]
      
      prior <- jfa::auditPrior(materiality = performanceMateriality, 
                               confidence = planningOptions[["confidence"]],
                               expectedError = planningOptions[["expectedErrors"]],
                               likelihood = planningOptions[["likelihood"]],
                               N = N,
                               ir = inherentRisk,
                               cr = controlRisk,
                               method = options[["priorConstructionMethod"]],
                               sampleN = options[["sampleN"]],
                               sampleK = options[["sampleK"]],
                               factor = options[["factor"]],
                               pHplus = 1 - options[["pHmin"]],
                               pHmin = options[["pHmin"]])
      
    }
    
    # Select evaluation method
    if(options[["variableType"]] == "variableTypeCorrect"){
      
      if(!options[["bayesianAnalysis"]]){
        method <- base::switch(options[["estimator2"]],
                               "binomialBound" = "binomial",
                               "poissonBound" = "poisson",
                               "hyperBound" = "hypergeometric")
      } else if(options[["bayesianAnalysis"]]){
        method <- base::switch(options[["estimator"]],
                               "betaBound" = "binomial",
                               "gammaBound" = "poisson",
                               "betabinomialBound" = "hypergeometric")
      }
      
      if(!options[["useSumStats"]]){
        nSumstats <- nrow(sample)
        kSumstats <- length(which(sample[, .v(options[["auditResult"]])] == 1))
      } else {
        nSumstats <- options[["nSumStats"]]
        kSumstats <- options[["kSumStats"]]
      }
      
      result <- try({
        
        # call jfa evaluation
        jfa::evaluation(confidence = confidence,
                        nSumstats = nSumstats,
                        kSumstats = kSumstats,
                        method = method,
                        materiality = planningOptions[["materiality"]],
                        N = planningOptions[["populationSize"]],
                        prior = prior)
        
      })
      
    } else if(options[["variableType"]] == "variableTypeAuditValues"){
      
      method <- base::switch(options[["estimator"]],
                             "stringerBound"     = "stringer",
                             "regressionBound"   = "regression",
                             "directBound"       = "direct",
                             "differenceBound"   = "difference",
                             "ratioBound"        = "quotient",
                             "coxAndSnellBound"  = "coxsnell",
                             "betaBound"         = "binomial",
                             "gammaBound"        = "poisson",
                             "betabinomialBound" = "hypergeometric")
      
      if(method == "stringer" && options[["stringerBoundLtaAdjustment"]])
        method <- "stringer-lta"
      
      # Bayesian regression is not implemented in jfa R package
      if(options[["bayesianAnalysis"]] && method == "regression"){
        
        result <- try({
          
          .jfa.bayesRegression.calculation(options, sample, planningOptions)
          
        })
        
      } else {
        
        if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != "" && options[["auditResult"]] != ""){
          
          result <- .jfa.separatedMisstatementEvaluation.state(options, sample, prior,
                                                               planningOptions, selectionState, evaluationContainer)
          
          return(result)
        }
        
        result <- try({
          
          counter <- NULL
          if(options[["sampleCounter"]] != "")
            counter <- sample[, .v(options[["sampleCounter"]])]
          # call jfa evaluation
          jfa::evaluation(sample = sample,
                          counts = counter,
                          confidence = confidence,
                          bookValues = .v(options[["monetaryVariable"]]),
                          auditValues = .v(options[["auditResult"]]),
                          method = method,
                          materiality = performanceMateriality,
                          minPrecision = minPrecision,
                          N = planningOptions[["populationSize"]],
                          populationBookValue = planningOptions[["populationValue"]],
                          prior = prior)
          
        })
        
      }
    }
    
    if(isTryError(result)){
      
      evaluationContainer$setError(paste0("An error occurred: ",
                                          JASP:::.extractErrorMessage(result)))
      return()
    }
    
    if(options[["variableType"]] == "variableTypeAuditValues" &&
       options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){
      
      result[["confBound"]] <- (planningOptions[["populationValue"]] - result[["lowerBound"]]) /
        planningOptions[["populationValue"]]
      
      if(result[["confBound"]] < planningOptions[["materiality"]]){
        result[["conclusion"]] <- "Approve population"
      } else {
        result[["conclusion"]] <- "Do not approve population"
      }
    }
    
    evaluationContainer[["evaluationState"]] <- createJaspState(result)
    
    return(result)
  }
}

.jfa.evaluation.table <- function(options, prevOptions, parentState, parentContainer, jaspResults,
                                  positionInContainer){
  
  .jfa.tableNumber.update(jaspResults)
  
  if(!is.null(parentContainer[["evaluationTable"]]))
    return()
  
  title <- gettextf("<b>Table %1$i.</b> Evaluation Summary", jaspResults[["tabNumber"]]$object)
  table <- createJaspTable(title)
  table$position  <- positionInContainer
  table$dependOn(options = c("bookValueDescriptives",
                             "sampleDescriptives",
                             "displaySample",
                             "samplingChecked",
                             "evaluationChecked",
                             "auditResult",
                             "evidenceRatio",
                             "bayesFactor",
                             "valuta",
                             "otherValutaName",
                             "mostLikelyError",
                             "IR",
                             "irCustom",
                             "CR",
                             "crCustom",
                             "obtainedPrecision",
                             "display"))
  
  if(options[["performanceMateriality"]])
    table$addColumnInfo(name = 'materiality', 	title = gettext("Performance materiality"), type = 'string')
  if(options[["minimumPrecision"]])
    table$addColumnInfo(name = 'minPrecision', 	title = gettext("Required precision"), type = 'string')
  table$addColumnInfo(name = 'sampleSize', 		title = gettext("Sample size"), type = 'integer')
  table$addColumnInfo(name = 'fullErrors', 		title = gettext("Errors"), type = 'integer')
  table$addColumnInfo(name = 'totalTaint', 		title = gettext("Total tainting"), type = 'string')
  if(options[["mostLikelyError"]])
    table$addColumnInfo(name = 'mle', 			title = gettext("Most likely error"), type = 'string')
  
  if(!options[["bayesianAnalysis"]]){
    auditRisk 		<- 1 - options[["confidence"]]
    detectionRisk 	<- .jfa.auditRiskModel.calculation(options)
    title <- gettextf("%1$s%% Upper bound", round((1 - detectionRisk) * 100, 2))
    table$addColumnInfo(name = 'bound', title = title, type = 'string')
  } else if(options[["bayesianAnalysis"]]){
    if(options[["areaUnderPosterior"]] == "displayCredibleBound"){
      title <- paste0(options[["confidence"]] * 100,"% Upper bound")
      table$addColumnInfo(name = 'bound', title = title, type = 'string')
    } else if (options[["areaUnderPosterior"]] == "displayCredibleInterval"){
      title <- paste0(options[["confidence"]] * 100,"% Credible interval")
      table$addColumnInfo(name = 'lowerBound', title = gettext("Lower"), type = 'string', overtitle = title)
      table$addColumnInfo(name = 'upperBound', title = gettext("Upper"), type = 'string', overtitle = title)
    }
  }
  
  if(options[["obtainedPrecision"]])
    table$addColumnInfo(name = 'precision', title = gettext("Obtained precision"), type = 'string')
  if(options[["bayesianAnalysis"]] && options[["performanceMateriality"]] && options[["evidenceRatio"]])
    table$addColumnInfo(name = 'evidenceRatio', title = gettext("Odds"), type = 'number')
  if(options[["bayesianAnalysis"]] && options[["performanceMateriality"]] && options[["bayesFactor"]])
    table$addColumnInfo(name = 'bayesFactor', title = gettextf("BF%1$s", "\u208B\u208A"), type = 'number')
  
  criterion <- options[["estimator"]]
  if(!options[["workflow"]] && options[["variableType"]] == "variableTypeCorrect" && !options[["bayesianAnalysis"]])
    criterion <- options[["estimator2"]]
  
  message <- base::switch(criterion,
                          "poissonBound" = gettext("The upper bound is calculated according to the <b>Poisson</b> distributon."),
                          "binomialBound" = gettext("The upper bound is calculated according to the <b>binomial</b> distributon."),
                          "hyperBound" = gettext("The upper bound is calculated according to the <b>hypergeometric</b> distribution."),
                          "stringerBound" = gettext("The upper bound is calculated according to the <b>Stringer</b> method."),
                          "regressionBound" = gettext("The upper bound is calculated according to the <b>regression</b> method."),
                          "directBound" = gettext("The upper bound is calculated according to the <b>direct</b> method."),
                          "differenceBound" = gettext("The upper bound is calculated according to the <b>difference</b> method."),
                          "ratioBound" = gettext("The upper bound is calculated according to the <b>ratio</b> method."),
                          "betaBound" = gettext("The upper bound is calculated according to the <b>beta</b> distribution and requires the assumption that the sample taints are interchangeable."),
                          "gammaBound" = gettext("The upper bound is calculated according to the <b>gamma</b> distribution and requires the assumption that the sample taints are interchangeable."),
                          "betabinomialBound" = gettext("The upper bound is calculated according to the <b>beta-binomial</b> distribution and requires the assumption that the sample taints are interchangeable."),
                          "coxAndSnellBound" = gettext("The upper bound is calculated according to the <b>Cox and Snell</b> method and requires the assumption that the population taints are uniformly distributed."))
  
  if(options[["estimator"]] == "stringerBound" && options[["stringerBoundLtaAdjustment"]] && options[["variableType"]] == "variableTypeAuditValues")
    message <- gettext("The upper bound is calculated according to the <b>Stringer</b> method with <b>LTA adjustment</b>.")
  
  if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != "")
    message <- gettext("The upper bound is calculated according to the <b>beta</b> distribution. It requires the assumptions that the sample taints are interchangeable and that the sample taints are homogeneous.")
  
  table$addFootnote(message)
  
  parentContainer[["evaluationTable"]] <- table
  
  if(is.null(parentState) || (options[["auditResult"]] == "" && !options[["useSumStats"]])){
    
    if(options[["workflow"]]){
      table$addFootnote(message = gettext("The audit result column is empty."),
                        symbol = gettextf("%1$s <b>Results cound not be calculated.</b>", "\u26A0"))
    } else {
      table$addFootnote(message = gettext("Either the materiality, the population size, or the population value is defined as zero, or one of the required variables is missing."),
                        symbol = gettextf("%1$s <b>Results could not be calculated.</b>", "\u26A0"))
    }
    return()
  }
  
  taintLabel <- base::switch(options[["display"]],
                             "displayNumbers" = round(parentState[["t"]], 3),
                             "displayPercentages" = paste0(round(parentState[["t"]] / parentState[["n"]] * 100, 3), "%"),
                             "displayValues" = paste(prevOptions[["valuta"]], round(parentState[["t"]], 2)))
  
  if(options[["bayesianAnalysis"]] && options[["areaUnderPosterior"]] == "displayCredibleInterval"){
    
    credibleInterval 	<- .jfa.credibleInterval.calculation(options, parentState)
    lowerBound 			<- credibleInterval[["lowerBound"]]
    upperBound 			<- credibleInterval[["upperBound"]]
    
    LowerBoundLabel <- base::switch(options[["display"]],
                                    "displayNumbers" = round(lowerBound, 3),
                                    "displayPercentages" = paste0(round(lowerBound * 100, 3), "%"),
                                    "displayValues" = paste(prevOptions[["valuta"]], round(lowerBound * prevOptions[["populationValue"]], 2)))
    UpperBoundLabel <- base::switch(options[["display"]],
                                    "displayNumbers" = round(upperBound, 3),
                                    "displayPercentages" = paste0(round(upperBound * 100, 3), "%"),
                                    "displayValues" = paste(prevOptions[["valuta"]], round(upperBound * prevOptions[["populationValue"]], 2)))
    
    row <- data.frame(sampleSize = parentState[["n"]],
                      fullErrors = parentState[["k"]],
                      totalTaint = taintLabel,
                      lowerBound = LowerBoundLabel,
                      upperBound = UpperBoundLabel)
    
  } else {

	if(parentState[["method"]] %in% c("direct", "difference", "quotient", "regression")){
		boundLabel <- base::switch(options[["display"]],
								"displayNumbers" = round(parentState[["upperBound"]] / prevOptions[["populationSize"]], 3),
								"displayPercentages" = paste0(round(parentState[["upperBound"]] / prevOptions[["populationValue"]] * 100, 3), "%"),
								"displayValues" = paste(prevOptions[["valuta"]], round(parentState[["upperBound"]], 2)))
	} else {
		boundLabel <- base::switch(options[["display"]],
								"displayNumbers" = round(parentState[["confBound"]], 3),
								"displayPercentages" = paste0(round(parentState[["confBound"]] * 100, 3), "%"),
								"displayValues" = paste(prevOptions[["valuta"]], round(parentState[["confBound"]] * prevOptions[["populationValue"]], 2)))
	}   
    
    row <- data.frame(sampleSize = parentState[["n"]],
                      fullErrors = parentState[["k"]],
                      totalTaint = taintLabel,
                      bound = boundLabel)
    
  }
  
  if(options[["performanceMateriality"]]){
    materiality <- base::switch(options[["display"]],
                                "displayNumbers" = round(prevOptions[["materiality"]], 3),
                                "displayPercentages" = paste0(round(prevOptions[["materiality"]] * 100, 3), "%"),
                                "displayValues" = paste(prevOptions[["valuta"]], round(prevOptions[["materiality"]] * prevOptions[["populationValue"]], 2)))
    row <- cbind(row, materiality = materiality)
  }
  
  if(options[["minimumPrecision"]]){
    minPrecision <- base::switch(options[["display"]],
                                 "displayNumbers" = round(prevOptions[["minimumPrecision"]], 3),
                                 "displayPercentages" = paste0(round(prevOptions[["minimumPrecision"]] * 100, 3), "%"),
                                 "displayValues" = paste(prevOptions[["valuta"]], round(prevOptions[["minimumPrecision"]] * prevOptions[["populationValue"]], 2)))
    row <- cbind(row, minPrecision = minPrecision)
  }
  
  if(options[["mostLikelyError"]]){
	  if(parentState[["method"]] %in% c("direct", "difference", "quotient", "regression")){
		mleLabel <- base::switch(options[["display"]],
								"displayNumbers" = round(parentState[["mle"]] /  prevOptions[["populationSize"]], 3),
								"displayPercentages" = paste0(round(parentState[["mle"]] /  prevOptions[["populationValue"]] * 100, 3), "%"),
								"displayValues" = paste(prevOptions[["valuta"]], round(parentState[["mle"]], 2)))
	  } else {
		mleLabel <- base::switch(options[["display"]],
								"displayNumbers" = round(parentState[["mle"]], 3),
								"displayPercentages" = paste0(round(parentState[["mle"]] * 100, 3), "%"),
								"displayValues" = paste(prevOptions[["valuta"]], round(parentState[["mle"]] * prevOptions[["populationValue"]], 2)))
	  }
    row <- cbind(row, mle = mleLabel)
  }
  
  if(options[["obtainedPrecision"]]){
	if(parentState[["method"]] %in% c("direct", "difference", "quotient", "regression")){
		precisionLabel <- base::switch(options[["display"]],
									"displayNumbers" = round(parentState[["precision"]] /  prevOptions[["populationSize"]], 3),
									"displayPercentages" = paste0(round(parentState[["precision"]] /  prevOptions[["populationValue"]] * 100, 3), "%"),
									"displayValues" = paste(prevOptions[["valuta"]], round(parentState[["precision"]], 2)))
	} else {
		precisionLabel <- base::switch(options[["display"]],
									"displayNumbers" = round(parentState[["precision"]], 3),
									"displayPercentages" = paste0(round(parentState[["precision"]] * 100, 3), "%"),
									"displayValues" = paste(prevOptions[["valuta"]], round(parentState[["precision"]] * prevOptions[["populationValue"]], 2)))
	}   
    row <- cbind(row, precision = precisionLabel)
  }
  
  if(options[["bayesianAnalysis"]] && options[["performanceMateriality"]] && (options[["evidenceRatio"]] || options[["bayesFactor"]])){
    if(options[["evidenceRatio"]])
      row <- cbind(row, evidenceRatio =parentState[["posterior"]][["hypotheses"]]$oddsHmin)
    if(options[["bayesFactor"]])
      row <- cbind(row, bayesFactor = parentState[["posterior"]][["hypotheses"]]$bf)
  }
  
  table$addRows(row)
}

.jfa.assumptionCheck.table <- function(options, sample, parentContainer, jaspResults,
                                       positionInContainer = 3){
  
  if(!options[["evaluationAssumptionChecks"]] || !options[["separateKnownAndUnknownMisstatement"]] || options[["monetaryVariable"]] == "")
    return()
  
  .jfa.tableNumber.update(jaspResults)
  
  title <- gettextf("<b>Table %1$i.</b> Assumption Checks", jaspResults[["tabNumber"]]$object)
  table <- createJaspTable(title)
  overTitle <- gettextf("%1$s%% Confidence interval", options[["evaluationAssumptionChecksConfidence"]] * 100)
  table$position  <- positionInContainer
  table$dependOn(options = c("evaluationAssumptionChecks"))
  
  table$addColumnInfo(name = 'type', 			title = "", type = 'string')
  table$addColumnInfo(name = 'n', 				title = "n", type = 'integer')
  table$addColumnInfo(name = 'correlation', 	title = gettext("Pearson's <i>r</i>"), type = 'number')
  table$addColumnInfo(name = 'lowerCI', 		title = gettext("Lower"), type = 'number', overtitle = overTitle)
  table$addColumnInfo(name = 'upperCI', 		title = gettext("Upper"), type = 'number', overtitle = overTitle)
  table$addColumnInfo(name = 'pvalue', 			title = gettext("p"), type = 'pvalue')
  table$addColumnInfo(name = 'bayesfactor', 	title = gettextf("BF%1$s", "\u2081\u2080"), type = 'number')
  
  parentContainer[["assumptionTable"]] <- table
  
  if(options[["auditResult"]] == ""){
    row <- list(type = gettext("The sample taints are homogeneous"))
    table$addRows(row)
    return()
  }
  
  ist 	<- sample[, .v(options[["monetaryVariable"]])]
  soll 	<- sample[, .v(options[["auditResult"]])]
  taint 	<- (ist - soll) / ist # Select all taints
  
  test 	<- cor.test(ist, taint, alternative = "two.sided", method = "pearson",
                    conf.level = options[["evaluationAssumptionChecksConfidence"]])
  est  	<- test[["estimate"]]
  lCi   	<- as.numeric(test[["conf.int"]])[1]
  uCi   	<- as.numeric(test[["conf.int"]])[2]
  pval    <- test[["p.value"]]
  bf10 	<- try({ 1 / .jfa.bayesCorrelation.calculation(r = est, n = nrow(sample))})
  
  if(isTryError(bf10)){
    bf10 <- NA
    table$addFootnote("An error occurred while calculating the Bayes factor", symbol = "<b>Warning.</b>")
  }
  
  row <- list(type = gettext("The sample taints are homogeneous"),
              n = length(taint),
              correlation = est,
              lowerCI = lCi,
              upperCI = uCi,
              pvalue = pval,
              bayesfactor = bf10)
  
  table$addRows(row)
}

.jfa.samplingObjectives.plot <- function(options, prevOptions, parentState, parentContainer, jaspResults,
                                         positionInContainer = 3){
  
  if(!options[["evaluationInformation"]])
    return()
  
  .jfa.figureNumber.update(jaspResults)
  
  if(is.null(parentContainer[["evaluationInformation"]])){
    
    figure <- createJaspPlot(plot = NULL, title = gettext("Evaluation of Sampling Objectives"),
                             width = 600, height = 300)
    figure$position <- positionInContainer
    figure$dependOn(options = c("evaluationInformation",
                                "display"))
    
    parentContainer[["evaluationInformation"]] <- figure
    
    if(((options[["auditResult"]] == "" || options[["recordNumberVariable"]] == "") && !options[["useSumStats"]]) ||
       (options[["useSumStats"]] && options[["nSumStats"]] == 0) ||
       (prevOptions[["materiality"]] == 0 && options[["performanceMateriality"]]) ||
       parentContainer$getError())
      return()
    
    materiality 	<- parentState[["materiality"]]
    minPrecision 	<- options[["minimumPrecisionPercentage"]]
    
    if(options[["variableType"]] == "variableTypeAuditValues" &&  options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){
      bound 		<- parentState[["upperBound"]] / prevOptions[["populationValue"]]
	  mle 			<- parentState[["mle"]] / prevOptions[["populationValue"]]
	  precision 	<- parentState[["precision"]] / prevOptions[["populationValue"]]
    } else {
	  bound 		<- parentState[["confBound"]]
	  mle 			<- parentState[["mle"]]
	  precision 	<- parentState[["precision"]]
	}
    
    objectiveColor 	<- "orange"
    boundColor 		<- ifelse(bound < materiality, yes = rgb(0, 1, .7, 1), no = rgb(1, 0, 0, 1))
    precisionColor 	<- ifelse(precision < minPrecision, yes = rgb(0, 1, .7, 1), no = rgb(1, 0, 0, 1))
    
    if(options[["performanceMateriality"]] && !options[["minimumPrecision"]]){
      label <- rev(c(gettext("Performance materiality"), gettext("Maximum error"), gettext("Most likely error")))
      values <- rev(c(materiality, bound, mle))
      fill <- rev(c(objectiveColor, boundColor, "#1380A1"))
    } else if(!options[["performanceMateriality"]] && options[["minimumPrecision"]]){
      label <- rev(c(gettext("Required precision"), gettext("Obtained precision"), gettext("Maximum error"), gettext("Most likely error")))
      values <- rev(c(minPrecision, precision, bound, mle))
      fill <- rev(c(objectiveColor, precisionColor, "#1380A1", "#1380A1"))
    } else if(options[["performanceMateriality"]] && options[["minimumPrecision"]]){
      label <- rev(c(gettext("Required precision"), gettext("Obtained precision"), gettext("Performance materiality"), gettext("Maximum error"), gettext("Most likely error")))
      values <- rev(c(minPrecision, precision, materiality, bound, mle))
      fill <- rev(c(objectiveColor, precisionColor, objectiveColor, boundColor, "#1380A1"))
    }
    
    if(options[["display"]] == "displayValues")
      values <- values * prevOptions[["populationValue"]]
    
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, values), min.n = 4)
    
    if(options[["display"]] == "displayValues"){
      x.labels <- format(JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1 * max(values), length.out = 100), min.n = 4), scientific = FALSE)
      values.labels <- paste(prevOptions[["valuta"]], ceiling(values))
    } else {
      x.labels <- paste0(round(JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1 * max(values), length.out = 100), min.n = 4) * 100, 4), "%")
      values.labels <- paste0(round(values * 100, 2), "%")
    }
    
    plotData 	<- data.frame(x = label, y = values)
    plotData$x 	<- factor(plotData$x, levels = plotData$x)
    
    yLimits <- c(0, 1.1 * max(values))
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1 * max(values), length.out = 100), min.n = 4)
    
    if(mle < 0 || bound < 0){
      # Here we adjust the axes if the mle turns out to be negative
      yBreaks <- JASPgraphs::getPrettyAxisBreaks(seq(min(values), 1.1 * max(values), length.out = 100), min.n = 4)
      x.labels <- format(JASPgraphs::getPrettyAxisBreaks(seq(min(values), 1.1 * max(values), length.out = 100), min.n = 4), scientific = FALSE)
      yLimits <- c(min(values), 1.1 * max(values))
    }
    
    plot <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_bar(stat = "identity", col = "black", size = 1, fill = fill) +
      ggplot2::coord_flip() +
      ggplot2::xlab(NULL) +
      ggplot2::annotate("text", y = values, x = 1:length(values), label = values.labels,
                        size = 6, vjust = 0.5, hjust = -0.3) +
      ggplot2::scale_y_continuous(name = "", breaks = yBreaks, limits = yLimits, labels = x.labels)
    
    jfaTheme <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                               axis.ticks.y = ggplot2::element_blank(),
                               axis.text.y = ggplot2::element_text(hjust = 0),
                               panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb", size = 0.5))
    
    plot <- JASPgraphs::themeJasp(plot, sides = "") + jfaTheme
    
    figure$plotObject <- plot
  }
  
  if(options[["explanatoryText"]]){
    
    figureCaption <- createJaspHtml(gettextf("<b>Figure %1$i.</b> Evaluation information for the current annotated selection. The materiality is compared with the maximum misstatement and the most likely error. The most likely error (MLE) is an estimate of the true misstatement in the population. The maximum error is an estimate of the maximum error in the population.",
                                             jaspResults[["figNumber"]]$object), "p")
    
    figureCaption$position <- positionInContainer + 1
    figureCaption$dependOn(optionsFromObject = parentContainer[["evaluationInformation"]])
    figureCaption$dependOn(options = "explanatoryText")
    parentContainer[["evaluationInformationText"]] <- figureCaption
  }
}

.jfa.correlationLine.add <- function(fit, plot = NULL, xMin, xMax, lwd) {
  # create function formula
  f <- vector("character", 0)
  for (i in seq_along(coef(fit))) {
    if (i == 1) {
      temp <- paste(coef(fit)[[i]])
      f <- paste(f, temp, sep="")
    }
    if (i > 1) {
      temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep = "")
      f <- paste(f, temp, sep = "+")
    }
  }
  
  x <- seq(xMin, xMax, length.out = 100)
  predY <- eval(parse(text = f))
  
  plot <- plot + ggplot2::geom_line(data = data.frame(x, predY), mapping = ggplot2::aes(x = x, y = predY), size = lwd, lty = 1)
  return(plot)
}

.jfa.correlation.plot <- function(options, sample, prevOptions, parentContainer, jaspResults,
                                  positionInContainer){
  
  if(!options[["correlationPlot"]])
    return()
  
  .jfa.figureNumber.update(jaspResults)
  
  if(is.null(parentContainer[["correlationPlot"]])){
    
    figure <- createJaspPlot(plot = NULL, title = gettext("Scatter Plot of Ist and Soll Values"),
                             width = 500, height = 400)
    figure$position <- positionInContainer
    figure$dependOn(options = c("correlationPlot",
                                "valuta",
                                "correlationPlotShowIds",
                                "correlationPlotShowCorrelation"))
    
    parentContainer[["correlationPlot"]] <- figure
    
    if(options[["auditResult"]] == "" || parentContainer$getError())
      return()
    
    plotData <- data.frame(ist = sample[,.v(options[["monetaryVariable"]])],
                           soll = sample[,.v(options[["auditResult"]])])
    plotData <- na.omit(plotData) # Just to be sure
    
    ist <- plotData[["ist"]]
    soll <- plotData[["soll"]]
    corResult <- cor(x = ist, y = soll, method = "pearson")
    
    fit <- vector("list", 1)
    fit[[1]] <- lm(soll ~ poly(ist, 1, raw = TRUE), data = plotData)
    bestModel <- 1 # which.min(Bic)
    
    # format ticks and labels
    ticks       <- pretty(c(ist, soll), min.n = 4)
    minTicks    <- min(ticks)
    maxTicks    <- max(ticks)
    labs        <- format(ticks, digits = 3, scientific = FALSE)
    
    corResult <- round(corResult, 3)
    
    cols <- rep("gray", nrow(plotData))
    cols[which(plotData$ist != plotData$soll)] <- rgb(0.9, 0, 0, 1)
    
    plot <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = ist, y = soll)) +
      ggplot2::geom_line(data = data.frame(x = c(minTicks, maxTicks), y = c(minTicks, maxTicks)),
                         mapping = ggplot2::aes(x = x, y = y), size = 0.35, linetype = "dashed") +
      ggplot2::scale_x_continuous(name = gettextf("Ist value (%1$s)", prevOptions[["valuta"]]),
                                  breaks = ticks, labels = labs, limits = range(ticks)) +
      ggplot2::scale_y_continuous(name = gettextf("Soll value (%1$s)", prevOptions[["valuta"]]),
                                  breaks = ticks, labels = labs, limits = range(ticks)) +
      JASPgraphs::geom_point(size = 3, fill = cols) +
      ggplot2::annotate("text", x = ticks[length(ticks)-2], y = ticks[length(ticks)-1], label = gettext("Ist = Soll"),
                        size = 4, hjust = 0, vjust = -0.5, fontface = "italic")
    
    if(options[["correlationPlotShowCorrelation"]]){
      plot <- .jfa.correlationLine.add(fit = fit[[bestModel]], plot = plot, xMin = minTicks, xMax = maxTicks, lwd = 1)
      plot <- plot + ggplot2::annotate("text", x = ticks[1], y = (ticks[length(ticks)] - ((ticks[length(ticks)] - ticks[length(ticks) - 1]) / 2)),
                                       label = paste0("italic(r) == ", corResult), size = 8, parse = TRUE, hjust = -0.5, vjust = 0.5)
    }
    
    if(options[["correlationPlotShowIds"]])
      plot <- plot + ggrepel::geom_text_repel(ggplot2::aes(label = sample[, .v(options[["recordNumberVariable"]])], x = ist, y = soll), hjust=-1, vjust=1, data = plotData)
    
    jfaTheme <- ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb", size = 0.5),
                               panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", size = 0.5))
    
    plot <- JASPgraphs::themeJasp(plot) + jfaTheme
    
    figure$plotObject <- plot
  }
  
  if(options[["explanatoryText"]]){
    figureCaption <- createJaspHtml(gettextf("<b>Figure %1$i.</b> Scatter plot of the Ist values in the selection and their corresponding Soll values. Grey dots on the diagonal (dashed line) indicate matching Ist and Soll values. Red dots off the diagonal indicate transactions whose Soll value did not match their original Ist value. If these red dots lie below the diagonal, the transactions are overstated. If these red dots lie above the diagonal they are understated. %2$s",
                                             jaspResults[["figNumber"]]$object,
                                             ifelse(options[["correlationPlotShowCorrelation"]], yes = "The value <i>r</i> is the Pearson correlation coefficient of the Ist values and the Soll values, an indicator of the strength of the linear relationship (solid line) between the two.", no = "")), "p")
    
    figureCaption$position <- positionInContainer + 1
    figureCaption$dependOn(optionsFromObject = parentContainer[["correlationPlot"]])
    figureCaption$dependOn(options = "explanatoryText")
    parentContainer[["correLationPlotText"]] <- figureCaption
  }
}

################################################################################
################## Common functions for the conclusion #########################
################################################################################

.jfa.additionalSamples.table <- function(options, jaspResults, positionInContainer = 2){

  if(!options[["bayesianAnalysis"]] || !options[["additionalSamples"]])
	return()

  prevContainer   	<- jaspResults[["evaluationContainer"]]
  prevState       	<- prevContainer[["evaluationState"]]$object
  parentContainer 	<- jaspResults[["conclusionContainer"]]

  if(is.nan(prevState[["t"]]))
	return()
  
  # Produce relevant terms conditional on the analysis result
  approveMateriality <- TRUE
  if(options[["performanceMateriality"]]){
    if(prevState[["confBound"]] < prevState[["materiality"]]){
      approveMateriality <- TRUE
    } else {
      approveMateriality <- FALSE
    }
  }
  
  approvePrecision <- TRUE
  if(options[["minimumPrecision"]]){
    if(prevState[["precision"]] <= options[["minimumPrecisionPercentage"]]){
      approvePrecision <- TRUE
    } else {
      approvePrecision <- FALSE
    }
  }
  
  if(!is.null(parentContainer[["extraSampleTable"]]) || (approvePrecision && approveMateriality))
    return()
  
  if(options[["separateKnownAndUnknownMisstatement"]])
    return() # temporarily
  
  .jfa.tableNumber.update(jaspResults)
  
  title <- gettextf("<b>Table %i.</b> Suggested Sample Extension to Achieve Objectives",
                    jaspResults[["tabNumber"]]$object)
  table <- createJaspTable(title)
  table$position <- positionInContainer
  table$dependOn(options = "additionalSamples")
  
  if(options[["bayesianAnalysis"]])
    table$addColumnInfo(name = 'prior', title = gettext("Prior distribution"), type = 'string')
  
  table$addColumnInfo(name = 'extraK', title = gettext("Additional tolerable errors"), type = 'integer')
  table$addColumnInfo(name = 'extraN', title = gettext("Additional sample size"), type = 'integer')
  
  if(options[["bayesianAnalysis"]])
    table$addColumnInfo(name = 'posterior', title = gettext("Posterior distribution"), type = 'string')
  
  parentContainer[["extraSampleTable"]] <- table
  
  if(parentContainer$getError())
    return()
  
  prevOptions <- .jfa.inputOptions.collect(options, dataset = NULL, jaspResults, stage = "planning", rawData = TRUE)
  
  minPrecision <- NULL
  if(options[["minimumPrecision"]])
    minPrecision <- options[["minimumPrecisionPercentage"]]
  
  performanceMateriality <- NULL
  if(options[["performanceMateriality"]])
    performanceMateriality <- prevOptions[["materiality"]]
  
  N <- prevOptions[["populationSize"]]
  if(options[["monetaryVariable"]] != "")
    N <- ceiling(prevOptions[["populationValue"]])
  
  currentK <- prevState[["t"]]
  currentN <- prevState[["n"]]
  newK <- currentK + 0:2
  newN <- rep(NA, length(newK))
  if(options[["bayesianAnalysis"]]){
    newPriors <- rep(prevState[["posterior"]]$posterior, length(newK))
    newPosteriors <- rep(NA, length(newK))
  }
  
  for(i in 1:length(newK)){
    
    if(!options[["bayesianAnalysis"]]){
      if(options[["variableType"]] == "variableTypeCorrect"){
        # Wald's sequential sampling technique (Touw and Hoogduin 2012, pp. 168-172)
        currentK <- ceiling(currentK)
        alpha <- 1 - options[["confidence"]]
        beta <- 1 - options[["confidence"]]
        minSpr <- alpha / (1 - beta)
        p0 <- performanceMateriality
        p1 <- prevState[["mle"]]
        ASN <- ( log((1 - alpha) / beta) * log((1 - beta)/alpha) ) / ( log((1 - p1)/(1 - p0)) * log(p0 / p1))
        maxN <- ceiling(2.5 * ASN)
        for(n in currentN:maxN){
          spr <- (p0 / p1)^(currentK + newK[i]) * ((1 - p0) / (1 - p1))^(currentN + n - (currentK + newK[i]))
          if(spr < minSpr){
            newN[i] <- currentN + n
            break
          }
          newN[i] <- maxN
        }
      } else if(options[["variableType"]] == "variableTypeAuditValues"){
        # Rough estimation of required sample size (Touw and Hoogduin 2020, pp. 197-198)
        w <- (3/4) * ((prevState[["confBound"]] * currentN)/ performanceMateriality)
        newN[1] <- ceiling(w * (1 + sqrt((1 - ((2 * currentN)/(3 * w)) ))))
        
        avgTaint <- prevState[["t"]] / currentK
        newT <- c(rep(avgTaint, currentK), 1)
        eval <- jfa:::.stringerBound(taints = newT, confidence = 0.95, n = currentN)
        w <- (3/4) * ((eval * currentN)/ performanceMateriality)
        newN[2] <- ceiling(w * (1 + sqrt((1 - ((2 * currentN)/(3 * w)) ))))
        
        newT <- c(rep(avgTaint, currentK), 1, 1)
        eval <- jfa:::.stringerBound(taints = newT, confidence = 0.95, n = currentN)
        w <- (3/4) * ((eval * currentN)/ performanceMateriality)
        newN[3] <- ceiling(w * (1 + sqrt((1 - ((2 * currentN)/(3 * w)) ))))
        
        message <- gettext("The sample sizes shown are a rough indication.")
        table$addFootnote(message)
      }
    } else if(options[["bayesianAnalysis"]]){
      
      ir <- base::switch(options[["IR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["irCustom"]])
      cr <- base::switch(options[["CR"]], "High" = 1, "Medium" = 0.60, "Low" = 0.36, "Custom" = options[["crCustom"]])
      
      # Incorporate the information from the prior and the sample into a new prior
      prior <- jfa::auditPrior(materiality = performanceMateriality,
                               confidence = options[["confidence"]],
                               expectedError = prevOptions[["expectedErrors"]],
                               likelihood = prevOptions[["likelihood"]],
                               N = N,
                               ir = ir,
                               cr = cr,
                               method = options[["priorConstructionMethod"]],
                               sampleN = options[["sampleN"]],
                               sampleK = options[["sampleK"]],
                               factor = options[["factor"]],
                               pHplus = 1 - options[["pHmin"]],
                               pHmin = options[["pHmin"]])
      
      newN[i] <- jfa::planning(materiality = performanceMateriality,
                               confidence = prevOptions[["confidence"]],
                               expectedError = newK[i],
                               likelihood = prevOptions[["likelihood"]],
                               N = N,
                               prior = prior,
                               minPrecision = minPrecision,
                               increase = 1,
                               maxSize = 10000)[["sampleSize"]]
      
      newPosteriors[i] <-  jfa::evaluation(sample = NULL,
                                           counts = NULL,
                                           confidence = prevOptions[["confidence"]],
                                           nSumstats = newN[i],
                                           kSumstats = newK[i],
                                           method = prevOptions[["likelihood"]],
                                           materiality = performanceMateriality,
                                           N = N,
                                           prior = prior)[["posterior"]]$posterior
    }
  }
  
  additionalN <- newN - currentN
  
  rows <- data.frame(extraK = 0:2, extraN = additionalN)
  if(options[["bayesianAnalysis"]])
    rows <- cbind(rows, prior = newPriors, posterior = newPosteriors)
  table$addRows(rows)
}

################################################################################
################## Common functions for the separate misstatement methods ######
################################################################################

.jfa.separatedMisstatementPlanning.state <- function(options, dataset, prior, parentOptions){
  
  # Plan a sample for the efficiency technique Separate known and unknown misstatement
  for(n in seq(5, nrow(dataset), by = options[["sampleSizeIncrease"]])){
    
    interval <- (parentOptions[["populationValue"]] / n)
    topStratum <- subset(dataset, dataset[, .v(options[["monetaryVariable"]])] > interval)
    bottomStratum <- subset(dataset, dataset[, .v(options[["monetaryVariable"]])] <= interval)
    
    m_seen <- sum(topStratum[, .v(options[["monetaryVariable"]])])
    set.seed(rnorm(1) + options[["sampleSizeIncrease"]] + parentOptions[["populationValue"]])
    intervalStartingPoint <- sample(1:(interval - 1), size = 1)
    
    intervalSelection <- intervalStartingPoint + 0:(n - 1) * interval
    index <- NULL
    for(i in 1:n){
      index <- c(index, which(intervalSelection[i] < cumsum(dataset[, .v(options[["monetaryVariable"]])]))[1])
    }
    sample <- dataset[index, ]
    sample <- unique(sample)
    
    bottomStratumSample <- sample[which(sample[, .v(options[["monetaryVariable"]])] <= interval), ]
    
    m_seen <- m_seen + sum(bottomStratumSample[, .v(options[["monetaryVariable"]])])
    m_seen_percentage <- m_seen / parentOptions[["populationValue"]]
    
    m_unseen <- parentOptions[["populationValue"]] - m_seen
    
    if(options[["expectedErrors"]] == "expectedAllPossible"){
      a <- prior[["description"]]$alpha + 0:n
      b <- prior[["description"]]$beta + n - 0:n
    } else if(options[["expectedErrors"]] == "expectedRelative"){
      a <- prior[["description"]]$alpha + 0:ceiling(n * (parentOptions[["expectedErrors"]]))
      b <- prior[["description"]]$beta + n - 0:ceiling(n * (parentOptions[["expectedErrors"]]))
    } else if(options[["expectedErrors"]] == "expectedAbsolute"){
      a <- prior[["description"]]$alpha + 0:ceiling(parentOptions[["expectedErrors"]])
      b <- prior[["description"]]$beta + n - 0:ceiling(parentOptions[["expectedErrors"]])
    }
    
    v95 <- qbeta(options[["confidence"]], a, b)
    v <- ((a - 1) / (a + b - 2))
    relativeInaccuracy <- v95 - v
    correctedInaccuracy <- options[["minimumPrecisionPercentage"]] * (1 / (1 - m_seen_percentage))
    diff <- relativeInaccuracy - correctedInaccuracy
    
    # Sampling objectives
    if(options[["minimumPrecision"]] && !options[["performanceMateriality"]]){
      if(all(diff <= 0))
        break
    } else if(!options[["minimumPrecision"]] && options[["performanceMateriality"]]){
      if(all(v95 < (parentOptions[["materiality"]] / (1 - (m_seen / parentOptions[["populationValue"]])))))
        break
    } else if(options[["minimumPrecision"]] && options[["performanceMateriality"]]){
      if(all(diff <= 0) && all(v95 < (parentOptions[["materiality"]] / (1 - (m_seen / parentOptions[["populationValue"]])))))
        break
    }
  }
  
  adjustedMateriality <- (parentOptions[["materiality"]] / (1 - (m_seen / parentOptions[["populationValue"]])))
  expErrors <- ceiling(n * (parentOptions[["expectedErrors"]] * 2))
  alphaPosterior 	<- prior[["description"]]$alpha + expErrors
  betaPosterior 	<- prior[["description"]]$beta + n - expErrors
  expectedPosterior <- list(description = list(alpha = alphaPosterior, beta = betaPosterior),
				 			statistics = list(mean = alphaPosterior / (alphaPosterior + betaPosterior), 
							 				  mode = (alphaPosterior - 1) / (alphaPosterior + betaPosterior - 2), 
											  ub = qbeta(options[["confidence"]], alphaPosterior, betaPosterior), 
											  precision = qbeta(options[["confidence"]], alphaPosterior, betaPosterior) - ((alphaPosterior - 1) / (alphaPosterior + betaPosterior - 2))),
							hypotheses = list(pHmin = pbeta(adjustedMateriality, alphaPosterior, betaPosterior), 
											  pHplus = pbeta(adjustedMateriality, alphaPosterior, betaPosterior, lower.tail = F), 
											  oddHmin = pbeta(adjustedMateriality, alphaPosterior, betaPosterior) / pbeta(adjustedMateriality, alphaPosterior, betaPosterior, lower.tail = F)))
  
  
  result <- list(sampleSize = n,
                 confidence = options[["confidence"]],
                 materiality = parentOptions[["materiality"]],
                 adjustedMateriality = adjustedMateriality,
                 N = parentOptions[["populationSize"]],
                 expectedSampleError = expErrors,
                 likelihood = "binomial",
                 prior = prior,
				 expectedPosterior = expectedPosterior,
                 startingPoint = intervalStartingPoint)
  
  return(result)
}

.jfa.separatedMisstatementEvaluation.state <- function(options, sample, prior, prevOptions, prevState, parentContainer){
  
  k <- length(which(sample[, .v(options[["monetaryVariable"]])] != sample[, .v(options[["auditResult"]])]))
  if(options[["workflow"]]){
    n <- sum(prevState[["count"]])
  } else{
    if(options[["sampleCounter"]] == ""){
      n <- nrow(sample)
    } else {
      n <- sum(sample[, .v(options[["sampleCounter"]])])
    }
  }
  
  overstatements <- (sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])])
  unseen_value <- prevOptions[["populationValue"]] - sum(sample[, .v(options[["monetaryVariable"]])])
  
  taintings <- overstatements / sample[, .v(options[["monetaryVariable"]])]
  if(options[["workflow"]]){
    totalTaint <- sum(taintings * prevState[["count"]])
  } else {
    if(options[["sampleCounter"]] == ""){
      totalTaint <- sum(taintings)
    } else {
      totalTaint <- sum(taintings * sample[, .v(options[["sampleCounter"]])])
    }
  }
  avgTaint <- totalTaint / n
  posteriorMode <- (prior[["description"]]$alpha + totalTaint - 1) / (prior[["description"]]$alpha + totalTaint + prior[["description"]]$beta + n - totalTaint - 2)
  
  # Find out the total error in the critital transactions (if needed)
  if(options[["workflow"]] && options[["flagCriticalTransactions"]] && options[["handleCriticalTransactions"]] == "inspect"){
    criticalTransactions <- subset(sample, sample[, .v(options[["criticalTransactions"]])] > 0)
    if(nrow(criticalTransactions) == 0){
      Vk <- 0
    } else {
      Vk <- sum(criticalTransactions[, .v(options[["monetaryVariable"]])] - criticalTransactions[, .v(options[["auditResult"]])]) # No absolute value
    }
    sample <- subset(sample, sample[, .v(options[["criticalTransactions"]])] == 0)
  } else if(!options[["workflow"]] && options[["flagCriticalTransactions"]] && options[["handleCriticalTransactions"]] == "inspect"){
    criticalTransactions <- subset(sample, sample[, .v(options[["monetaryVariable"]])] < 0)
    Vk <- sum(criticalTransactions[, .v(options[["monetaryVariable"]])] - criticalTransactions[, .v(options[["auditResult"]])]) # No absolute value
  } else {
    Vk <- 0
  }
  
  Vs <- sum(overstatements)               # The total error in the sample (known error)
  Vt <- posteriorMode * unseen_value      # The total error in the unseen observations (unknown error)
  Vt95 <- qbeta(options[["confidence"]], shape1 = prior[["description"]]$alpha + totalTaint, shape2 = prior[["description"]]$beta + n - totalTaint) * unseen_value # The upper bound on the total error in the unseen observations
  
  # The inferred total error and upper bound for the population
  V <- Vk + Vs + Vt
  VAsFraction <- V / prevOptions[["populationValue"]]
  
  V95 <- Vk + Vs + Vt95
  V95AsFraction <- V95 / prevOptions[["populationValue"]]
  
  # The total obtained precision
  precisionAsFraction <- (V95 - V) / prevOptions[["populationValue"]]

  adjustedMateriality <- (prevOptions[["materiality"]] / (1 - (sum(sample[, .v(options[["monetaryVariable"]])]) / prevOptions[["populationValue"]])))
  alphaPosterior 	<- prior[["description"]]$alpha + totalTaint
  betaPosterior 	<- prior[["description"]]$beta + n - totalTaint
  posterior 		<- list(description = list(alpha = alphaPosterior, beta = betaPosterior),
				 			statistics = list(mean = alphaPosterior / (alphaPosterior + betaPosterior), 
							 				  mode = (alphaPosterior - 1) / (alphaPosterior + betaPosterior - 2), 
											  ub = qbeta(options[["confidence"]], alphaPosterior, betaPosterior), 
											  precision = qbeta(options[["confidence"]], alphaPosterior, betaPosterior) - ((alphaPosterior - 1) / (alphaPosterior + betaPosterior - 2))),
							hypotheses = list(pHmin = pbeta(adjustedMateriality, alphaPosterior, betaPosterior), 
											  pHplus = pbeta(adjustedMateriality, alphaPosterior, betaPosterior, lower.tail = F), 
											  oddHmin = pbeta(adjustedMateriality, alphaPosterior, betaPosterior) / pbeta(adjustedMateriality, alphaPosterior, betaPosterior, lower.tail = F)))
  
  result <- list(confBound = V95AsFraction,
                 confBoundUnseen = Vt95 / unseen_value,
                 precision = precisionAsFraction,
                 precisionUnseen = qbeta(options[["confidence"]], shape1 = prior[["description"]]$alpha + totalTaint, shape2 = prior[["description"]]$beta + n - totalTaint) - posteriorMode,
                 unseenValue = unseen_value,
                 k = k,
                 t = totalTaint,
                 n = n,
                 prior = prior,
				 posterior = posterior,
                 confidence = options[["confidence"]],
                 method = "binomial",
                 likelihood = "binomial",
                 mle = VAsFraction,
                 mleUnseen = posteriorMode,
                 materiality = prevOptions[["materiality"]],
                 populationValue = prevOptions[["populationValue"]],
                 adjustedMateriality = (prevOptions[["materiality"]] / (1 - (sum(sample[, .v(options[["monetaryVariable"]])]) / prevOptions[["populationValue"]]))))
  
  parentContainer[["evaluationState"]] <- createJaspState(result)
  
  return(result)
}

################################################################################
################## End functions ###############################################
################################################################################
