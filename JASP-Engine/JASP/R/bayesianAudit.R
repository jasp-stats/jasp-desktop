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

# When making changes to this file always mention @koenderks as a reviewer in the Pull Request

bayesianAudit <- function(jaspResults, dataset, options, ...){
    ### PROCEDURE ###
    .bayesianProcedure(options, jaspResults)
    ### AUDIT RISK MODEL ###
    .auditRiskModel(options, jaspResults)
    ### PLANNING ###
    .bayesianPlanning(dataset, options, jaspResults)
    ### SELECTION ###
    if(!options[["samplingChecked"]] || jaspResults[["planningContainer"]]$getError()) return()    # Stop if "To Selection" is not pressed
    .bayesianSelection(options, jaspResults)
    ### EXECUTION ###
    .execution(options, jaspResults)
    ### EVALUATION ###
    if(!options[["evaluationChecked"]] || jaspResults[["planningContainer"]]$getError() || jaspResults[["selectionContainer"]]$getError()) return()  # Stop if "To Evaluation" is not pressed
    .bayesianEvaluation(options, jaspResults)
    ### CONCLUSION ###
    .bayesianConclusion(options, jaspResults)
}

.bayesianProcedure <- function(options, jaspResults){
  # Read in data
  dataset <- .readDataProcedure(options, jaspResults)
  # Error handling
  .errorHandlingProcedure(options, dataset)
  # Valuta title
  jaspResults[["valutaTitle"]] <- createJaspState(base::switch(options[["valuta"]], "euroValuta" = "\u20AC", "dollarValuta" = "\u0024", "otherValuta" = options[["otherValutaName"]]))
  # Create state for the figure number
  jaspResults[["figNumber"]] <- createJaspState(1)
  jaspResults[["figNumber"]]$dependOn(options = c("bookValueDistribution", "decisionPlot"))
  # Create container for the procedure
  procedureContainer <- createJaspContainer(title= "<u>Procedure</u>")
  procedureContainer$position <- 1
  # Interpretation for the procedure
  if(options[["explanatoryText"]] && is.null(procedureContainer[["procedureParagraph"]])){
    if(is.null(jaspResults[["confidenceLevelLabel"]])){
      jaspResults[["confidenceLevelLabel"]] <- createJaspState(paste0(round(options[["confidence"]] * 100, 2), "%"))
      jaspResults[["confidenceLevelLabel"]]$dependOn(options = c("confidence"))
    }
    criterion <- base::switch(options[["materiality"]], "materialityRelative" = "<b>percentage</b>", "materialityAbsolute" = "<b>amount</b>")
    materialityLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]] * 100, 2), "%"), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
    procedureContainer[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", jaspResults[["confidenceLevelLabel"]]$object, ")</b> whether the ", criterion ," of
                                                                                          misstatement in the target population is lower than the specified materiality of <b>", materialityLabel, "</b>."), "p")
    procedureContainer[["procedureParagraph"]]$position <- 1
    procedureContainer[["procedureParagraph"]]$dependOn(options = c("explanatoryText", "confidence", "valuta"))
  }
  # Create a table of the population descriptives (if the user wants it)
  if(options[["bookValueDescriptives"]])
    .bookValueDescriptives(dataset, options, jaspResults, position = 2, procedureContainer)
  # Create a plot of the population book values (if the user wants it)
  if(options[["bookValueDistribution"]])
    .bookValueDistribution(dataset, options, jaspResults, position = 3, procedureContainer)
  # Finish procedure
  jaspResults[["procedureContainer"]] <- procedureContainer
}

.bayesianPlanning <- function(dataset, options, jaspResults){
  # Create a container for the planning
  planningContainer <- createJaspContainer(title= "<u>Planning</u>")
  planningContainer$position <- 3
  # Rewrite the materiality to a proportion of the total value
  if(jaspResults[["ready"]]$object || is.null(jaspResults[["materiality"]])){
    materiality <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = options[["materialityValue"]] / jaspResults[["total_data_value"]]$object, no = options[["materialityPercentage"]])
    jaspResults[["materiality"]] <- createJaspState(materiality)
    jaspResults[["materiality"]]$dependOn(options = c("materialityValue", "materialityPercentage", "monetaryVariable", "recordNumberVariable", "materiality"))

    if(options[["materiality"]] == "materialityAbsolute" && options[["materialityValue"]] >= jaspResults[["total_data_value"]]$object && jaspResults[["ready"]]$object)
     planningContainer$setError("Analysis not possible: Your materiality is higher than the total value of the book values.") 
    expTMP <- ifelse(options[['expectedErrors']] == "expectedRelative", yes = options[["expectedPercentage"]], no = options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object)
    if(expTMP > materiality && jaspResults[["ready"]]$object)
      planningContainer$setError("Analysis not possible: Your expected errors are higher than materiality.")
    if(jaspResults[["ready"]]$object && jaspResults[["uniqueN"]]$object != jaspResults[["N"]]$object)
      planningContainer$setError("Analysis not possible: Your record identification numbers should only contain unique values.")
    
  }
  # Calculate the sample size and return the calculation as an object
  planningResult <- .bayesianPlanningHelper(options, jaspResults, planningContainer)
  # Summarize the planning result in a summary table
  .bayesianPlanningTable(dataset, options, planningResult, jaspResults, position = 2, planningContainer)
  # Rewrite the required sample size when the planning has not been run yet
  requiredSampleSize <- 0
  if(!is.null(jaspResults[["planningResult"]]))
    requiredSampleSize <- planningResult[["n"]]
  # Calculate the number of expected errors and the maximum number of allowed errors
  expected.errors   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = paste0(round(options[["expectedPercentage"]] * 100, 2), "%"), no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))
  max.errors        <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = floor(options[["expectedPercentage"]] * requiredSampleSize) + 1, no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]] + 1))
  # Explanatory text for the planning
  if(options[["explanatoryText"]] && is.null(planningContainer[["planningParagraph"]])){
    materialityLevelLabel <- ifelse(options[["materiality"]] == "materialityRelative", yes = paste0(round(jaspResults[["materiality"]]$object, 4) * 100, "%"), no = paste0(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
    jaspResults[["materialityLevelLabel"]] <- createJaspState(materialityLevelLabel)
    planningContainer[["planningParagraph"]] <- createJaspHtml(paste0("The most likely error in the data was expected to be <b>", expected.errors ,"</b>.  The sample size that is required to prove a materiality of <b>", materialityLevelLabel ,"</b>, assuming
                                                                                              the sample contains <b>", expected.errors ,"</b> full errors, is <b>", planningResult[["n"]] ,"</b>. This sample size is calculated according to the <b>", options[["planningModel"]] , "</b> distribution, the inherent risk <b>(", options[["IR"]] , ")</b>,
                                                                                              the control risk <b>(", options[["CR"]] , ")</b> and the expected errors. The specific distribution that corresponds with this prior knowledge is the
                                                                                              <b>Beta(",round(planningResult[["priorA"]],2), ",", round(planningResult[["priorB"]],2),")</b> distribution. The information in this prior distribution states that there is a <b>",
                                                                                              round(pbeta(jaspResults[["materiality"]]$object, planningResult[["priorA"]], planningResult[["priorB"]]) * 100, 2) ,"%</b> prior probability that the population misstatement
                                                                                              is lower than materiality. Consequently, if the sum of errors from the audited observations exceeds <b>", max.errors ,"</b> the maximum misstatement
                                                                                              exceeds materiality and the population cannot be approved."), "p")
    planningContainer[["planningParagraph"]]$position <- 1
    planningContainer[["planningParagraph"]]$dependOn(options = c("expectedPercentage", "expectedErrors", "expectedNumber", "planningModel", "IR", "CR", "materialityPercentage", "confidence", "materialityValue", "materiality"))
  }
  # Create the implicit sample table (if the user wants it)
  if(options[["implicitSampleTable"]])
    .implicitSampleTable(options, planningResult, jaspResults, position = 3, planningContainer)
  # Create a decision plot (if the user wants it)
  if(options[["decisionPlot"]])
    .decisionAnalysis(options, jaspResults, position = 4, planningContainer, type = "bayesian")
  # Plot the prior (and optional expected posterior) distribution (if the user wants it)
  if(options[["priorPlot"]])
    .plotPrior(options, planningResult, jaspResults, position = 6, planningContainer)
  # Finish planning
  jaspResults[["planningContainer"]] <- planningContainer
}

.bayesianSelection <- function(options, jaspResults){
  # Create a container for the selection
  selectionContainer <- createJaspContainer(title= "<u>Selection</u>")
  selectionContainer$position <- 4
  selectionContainer$dependOn(options = c("samplingChecked"))
  # Read in data for selection
  dataset <- .readDataSelection(options)
  # Import stored objects from jaspResults
  total_data_value              <- jaspResults[["total_data_value"]]$object
  planningResult                <- jaspResults[["planningResult"]]$object
  jaspResults[["sampleSize"]]   <- createJaspState(planningResult[["n"]])
  monetaryVariable              <- unlist(options[["monetaryVariable"]])
  # Perform the sampling and create the tables for displaying the selection
  base::switch(options[["selectionMethod"]],
                  "randomSampling"      = .randomSampling(dataset, options, jaspResults, position = 4, selectionContainer),
                  "systematicSampling"  = .systematicSampling(dataset, options, jaspResults, position = 4, selectionContainer),
                  "cellSampling"        = .cellSampling(dataset, options, jaspResults, position = 4, selectionContainer))
  # Explanatory text for selection
  if(options[["explanatoryText"]]){
    technique <- base::switch(options[["selectionMethod"]], "randomSampling" = "random", "systematicSampling" = "fixed interval", "cellSampling" = "cell")
    technique <- base::switch(options[["selectionType"]], "recordSampling" = paste(technique, "record sampling"), "musSampling" = paste(technique, "monetary unit sampling"))
    if(!jaspResults[["containsDoubleObservations"]]$object){
      message <- paste0("From the population of <b>", jaspResults[["N"]]$object, "</b> observations, <b>", planningResult[["n"]], "</b> observations were selected using a <b>", technique, "</b> method.")
    } else {
      message <- paste0("From the population of <b>", jaspResults[["N"]]$object, "</b> observations, <b>", planningResult[["n"]], "</b> observations were selected using a <b>", technique, "</b> method.
                        <b>Note:</b> The selected population subset (", nrow(jaspResults[["sample"]]$object) ,") is smaller than the planned sample size (", planningResult[["n"]] ,"), as observations are selected multiple times due 
                        to selecting with replacement. These observations (", planningResult[["n"]] - nrow(jaspResults[["sample"]]$object) ,") are counted multiple times in the evaluation.")
    }
    selectionContainer[["samplingParagraph"]] <- createJaspHtml(message, "p")
    selectionContainer[["samplingParagraph"]]$position <- 1
    selectionContainer[["samplingParagraph"]]$dependOn(options = c("samplingType", "samplingMethod", "seed", "intervalStartingPoint"))
  }
  # Create a table at the top of the selection with information about the selection process
  .selectionInformationTable(dataset, options, jaspResults, position = 2, selectionContainer)
  # Create a table with descriptive statistics for the selection (if the user wants it)
  if(options[["sampleDescriptives"]])
    .sampleDescriptivesTable(dataset, options, jaspResults, position = 3, selectionContainer)
  # Finish selection
  jaspResults[["selectionContainer"]] <- selectionContainer
}

.bayesianEvaluation <- function(options, jaspResults){
  # Create a container for the evaluation
  evaluationContainer <- createJaspContainer(title = "<u>Evaluation</u>")
  evaluationContainer$position <- 5
  # Read data for the evaluation
  dataset <- .readDataEvaluation(options, jaspResults)
  # Import stored objects from jaspResults
  total_data_value              <- jaspResults[["total_data_value"]]$object
  planningResult                <- jaspResults[["planningResult"]]$object
  runEvaluation                 <- jaspResults[["runEvaluation"]]$object
  # Apply the selection filter to the dataset
  if(runEvaluation)
    dataset <- subset(dataset, dataset[, .v(options[["sampleFilter"]])] != 0)
  # Perform the evaluation conditional on the type of variable
  if(options[["variableType"]] == "variableTypeCorrect"){
    # Correct / Incorrect evaluation
    evaluationResult <- .bayesianAttributesBound(dataset, options, jaspResults)
    # Create the summary table for the evaluation
    .bayesianAttributesBoundTable(options, evaluationResult, jaspResults, position = 2, evaluationContainer)
  } else if(options[["variableType"]] == "variableTypeAuditValues"){
    # Audit value evaluation
    if(options[["estimator"]] == "coxAndSnellBound"){
      evaluationResult <- .coxAndSnellBound(dataset, options, jaspResults, priorA = planningResult[["priorA"]], priorB = planningResult[["priorB"]])
    } else if(options[["estimator"]] == "regressionBound"){
      evaluationResult <- .regressionBoundBayesian(dataset, options, total_data_value, jaspResults)
    }
    # Create the summary table for the evaluation
    .bayesianAuditValueBoundTable(options, evaluationResult, jaspResults, position = 2, evaluationContainer)
  }
  # Explanatory text for the evaluation
  if(options[["explanatoryText"]]){
    boundLabel      <- ifelse(runEvaluation, yes = paste0(round(evaluationResult[["bound"]] * 100, 2), "%"), no = ".....")
    extraObsLabel   <- ifelse(jaspResults[["containsDoubleObservations"]]$object, no = nrow(dataset), yes = paste0(nrow(dataset), " + ", planningResult[["n"]] - nrow(jaspResults[["sample"]]$object)))
    sampleSizeLabel <- ifelse(options[["auditResult"]] == "", yes = ".....", no = extraObsLabel)
    evaluationContainer[["resultParagraph"]] <- createJaspHtml(paste0("The selection consisted of <b>", sampleSizeLabel , "</b> observations, <b>",evaluationResult[["k"]], "</b> of which were found to contain an error. The knowledge from these data, com-
                                                          bined with the prior knowledge results in an <b>", jaspResults[["confidenceLevelLabel"]]$object , "</b> upper confidence bound of <b>", boundLabel ,"</b>. The cumulative knowledge states that there
                                                          is a true probability of <b>", jaspResults[["confidenceLevelLabel"]]$object , "</b> that the misstatement in the population is lower than <b>", boundLabel ,"</b>."), "p")
    evaluationContainer[["resultParagraph"]]$position <- 1
    evaluationContainer[["resultParagraph"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "materialityPercentage", "estimator", "materialityValue", "materiality", "performAudit"))
  }
  # Create a plot containing evaluation information (if the user wants it)
  if(options[["evaluationInformation"]])
    .evaluationInformation(options, evaluationResult, jaspResults, position = 3, evaluationContainer)
  # Create a plot containing the prior and posterior information (if the user wants it)
  if(options[["priorAndPosteriorPlot"]])
    .priorAndPosteriorPlot(options, evaluationResult, jaspResults, position = 5, evaluationContainer)
  # Create a plot containing the correlation between the book values and audit values (if the user wants it)
  if(options[["correlationPlot"]])
    .correlationPlot(dataset, options, jaspResults, position = 7, evaluationContainer)
  # Finish evaluation
  jaspResults[["evaluationContainer"]] <- evaluationContainer
}

.bayesianConclusion <- function(options, jaspResults){
  if(!is.null(jaspResults[["conclusionContainer"]])) return()
  # Import result of analysis from jaspResults
  evaluationResult <- jaspResults[["evaluationResult"]]$object
  # Explanatory text for conclusion
  if(options[["explanatoryText"]] && jaspResults[["runEvaluation"]]$object){
    # Create a container for the conclusion
    conclusionContainer <- createJaspContainer(title = "<u>Conclusion</u>")
    conclusionContainer$position <- 5
    conclusionContainer$dependOn(options = c("IR", "CR", "confidence", "auditResult", "materialityPercentage", "estimator", "materialityValue", "sampleFilter", "materiality", "explanatoryText", "performAudit"))
    # Produce relevant terms conditional on the analysis result
    above_below   <- ifelse(evaluationResult[["bound"]] < jaspResults[["materiality"]]$object, yes = "lower", no = "higher")
    approve       <- ifelse(evaluationResult[["bound"]] < jaspResults[["materiality"]]$object, yes = "<b>no material misstatement</b>", no = "<b>material misstatement</b>")
    conclusionContainer[["conclusionParagraph"]] <- createJaspHtml(paste0("To approve these data, a <b>", jaspResults[["confidenceLevelLabel"]]$object ,"</b> upper credible bound on the population proportion of errors should be determined to be
                                                                lower than materiality, in this case <b>", jaspResults[["materialityLevelLabel"]]$object ,"</b>. For the current data, the credible bound is <b>", above_below ,"</b> than materiality. The conclusion 
                                                                for these data is that the data contain ", approve ,"."), "p")
    conclusionContainer[["conclusionParagraph"]]$position <- 1
    conclusionContainer[["conclusionParagraph"]]$dependOn(optionsFromObject =conclusionContainer)
    # Finsh conclusion
    jaspResults[["conclusionContainer"]] <- conclusionContainer
  }
}
