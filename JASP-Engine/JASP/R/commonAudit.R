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
################## Functions for the figure and table numbers ##################
################################################################################

.auditCreateFigureNumber <- function(jaspResults){
    jaspResults[["figNumber"]] <- createJaspState(0)
}

.auditCreateTableNumber <- function(jaspResults){
    jaspResults[["tabNumber"]] <- createJaspState(0)
}

.updateTabNumber <- function(jaspResults){
  currentNumber <- jaspResults[["tabNumber"]]$object
  jaspResults[["tabNumber"]] <- createJaspState(currentNumber + 1)
}

.updateFigNumber <- function(jaspResults){
  currentNumber <- jaspResults[["figNumber"]]$object
  jaspResults[["figNumber"]] <- createJaspState(currentNumber + 1)
}

################################################################################
################## Functions for the Audit Risk Model ##########################
################################################################################

.auditRiskModelParagraph <- function(options, 
                                     planningOptions, 
                                     jaspResults, 
                                     position){

  if(!is.null(jaspResults[["ARMcontainer"]])) 
    return()

  ARMcontainer <- createJaspContainer(title= "<u>Audit Risk Model</u>")
  ARMcontainer$position <- position
  ARMcontainer$dependOn(options = c("confidence", 
                                    "IR",
                                    "irCustom", 
                                    "CR", 
                                    "crCustom",
                                    "materiality", 
                                    "materialityPercentage", 
                                    "materialityValue", 
                                    "explanatoryText", 
                                    "valuta",
                                    "otherValutaName"))
  
  jaspResults[["ARMcontainer"]] <- ARMcontainer

  auditRisk <- 1 - options[["confidence"]]

  if(options[["IR"]] != "Custom"){

    inherentRisk <- base::switch(options[["IR"]], 
                                 "Low" = 0.50, 
                                 "Medium" = 0.60, 
                                 "High" = 1)

  } else {

    inherentRisk <- options[["irCustom"]]
  }

  if(options[["CR"]] != "Custom"){

    controlRisk <- base::switch(options[["CR"]], 
                                "Low" = 0.50, 
                                "Medium" = 0.60, 
                                "High" = 1)

  } else {

    controlRisk <- options[["crCustom"]]

  }

  detectionRisk <- auditRisk / inherentRisk / controlRisk

  textARM <- paste0("Audit risk (", 
                    round(auditRisk * 100, 2),
                    "%) = Inherent risk (", 
                    round(inherentRisk * 100, 2), 
                    "%) x Control risk (", 
                    round(controlRisk * 100, 2), 
                    "%) x Detection risk (", 
                    round(detectionRisk * 100, 2), 
                    "%)")
  
  ARMcontainer[["ARMformula"]] <- createJaspHtml(textARM, "h3")
  ARMcontainer[["ARMformula"]]$position <- 2

  if(options[["explanatoryText"]]){

    auditRiskLabel <- paste0(round(auditRisk * 100, 2), "%")
    dectectionRiskLabel <- paste0(round(detectionRisk * 100, 2), "%")

    message <- paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>", 
                      options[["IR"]] ,
                      "</b>. The internal control risk was determined
                      to be <b>", 
                      options[["CR"]] ,
                      "</b>. According to the Audit Risk Model, the required detection risk to maintain an audit risk of <b>", 
                      auditRiskLabel, 
                      "</b> for a materiality
                      of <b>", 
                      planningOptions[["materialityLabel"]],
                      "</b> should be <b>", 
                      dectectionRiskLabel , 
                      "</b>.")

    if(options[["IR"]] == "Custom" || options[["CR"]] == "Custom"){

      message <- paste0(message, 
                        " The translation of High, Medium and Low to probabilities is done according custom values</b>.")
    
    } else {

      message <- paste0(message, 
                        " The translation of High, Medium and Low to probabilities is done according to <b>IODAD (2007)</b>.")
    
    }

    ARMcontainer[["AuditRiskModelParagraph"]] <- createJaspHtml(message, "p")
    ARMcontainer[["AuditRiskModelParagraph"]]$position <- 1
  }
}

################################################################################
################## Functions for the procedure #################################
################################################################################

.auditReadDataProcedure <- function(options, 
                                    jaspResults){

  recordNumberVariable <- options[["recordNumberVariable"]]
  if(recordNumberVariable == "") 
    recordNumberVariable <- NULL 

  monetaryVariable <- options[["monetaryVariable"]]
  if(monetaryVariable == "")              
    monetaryVariable <- NULL 

  procedureOptions <- list()
  
  if(!is.null(recordNumberVariable)){

    dataset <- .readDataSetToEnd(columns.as.numeric = recordNumberVariable)

    procedureOptions[["populationSize"]] <- nrow(dataset)
    procedureOptions[["uniqueN"]] <- length(unique(
                                      dataset[, .v(options[["recordNumberVariable"]])]
                                    ))

    if(!is.null(monetaryVariable)){

      variables <- c(recordNumberVariable, monetaryVariable)
      dataset <- .readDataSetToEnd(columns.as.numeric = variables)

      monetaryColumn <- dataset[, .v(monetaryVariable)]
      
      procedureOptions[["populationValue"]] <- sum(monetaryColumn)
      procedureOptions[["absPopulationValue"]] <- sum(abs(monetaryColumn))
      procedureOptions[["meanValue"]] <- mean(monetaryColumn)
      procedureOptions[["sigmaValue"]] <- sd(monetaryColumn)
      procedureOptions[["quantileValue"]] <- as.numeric(quantile(monetaryColumn, 
                                                                 probs = c(0.25, 
                                                                           0.50, 
                                                                           0.75)))
      procedureOptions[["ready"]] <- TRUE

    } else {

      procedureOptions[["populationValue"]] <- 0.01
      procedureOptions[["ready"]] <- ifelse(options[["materiality"]] == "materialityRelative",
                                            yes = TRUE,
                                            no = FALSE)

    }

  } else {

      dataset <- NULL
      procedureOptions[["populationSize"]] <- 0
      procedureOptions[["uniqueN"]] <- 0
      procedureOptions[["populationValue"]] <- 0.01
      procedureOptions[["ready"]] <- FALSE
  }

  materiality <- ifelse(options[["materiality"]] == "materialityRelative", 
                        yes = options[["materialityPercentage"]], 
                        no = options[["materialityValue"]])

  if(materiality == 0)
    procedureOptions[["ready"]] <- FALSE

  jaspResults[["procedureOptions"]] <- createJaspState(procedureOptions)
  jaspResults[["procedureOptions"]]$dependOn(c("recordNumberVariable", 
                                               "monetaryVariable", 
                                               "materiality",
                                               "materialityPercentage",
                                               "materialityValue"))
  return(dataset)
}

.auditProcedureErrorChecks <- function(options, 
                                       dataset){
  
  variables <- NULL

  if(options[["recordNumberVariable"]] != "")
    variables <- c(variables, options[["recordNumberVariable"]])

  if(options[["monetaryVariable"]] != "")
    variables <- c(variables, options[["monetaryVariable"]])

  N <- nrow(dataset)

  # Check for infinity, zero variance, and any missing observations
  .hasErrors(dataset, 
             type = c("infinity", 
                      "variance", 
                      "observations"),
             all.target = variables, 
             message = "short", 
             observations.amount = paste0("< ", N),
             exitAnalysisIfErrors = TRUE)
}

.auditProcedureGetContainer <- function(jaspResults,
                                        position){

  if(!is.null(jaspResults[["procedureContainer"]])){

    return(jaspResults[["procedureContainer"]])

  } else {
                                         
    procedureContainer <- createJaspContainer(title= "<u>Procedure</u>")
    procedureContainer$position <- position

    procedureContainer$dependOn(options = c("explanatoryText", 
                                            "confidence", 
                                            "materiality", 
                                            "materialityValue", 
                                            "materialityPercentage", 
                                            "valuta",
                                            "otherValutaName",
                                            "monetaryVariable",
                                            "recordNumberVariable"))

    jaspResults[["procedureContainer"]] <- procedureContainer

    return(procedureContainer)
  }
}

.auditExplanatoryTextProcedure <- function(options, 
                                           planningOptions, 
                                           jaspResults, 
                                           positionInContainer){

  if(options[["explanatoryText"]]){

    procedureContainer <- .auditProcedureGetContainer(jaspResults,
                                                      position = 1)


    procedureText <- paste0("The objective of this substantive testing procedure is to determine with a specified confidence <b>(", 
                            planningOptions[["confidenceLabel"]], 
                            ")</b> whether the ", 
                            planningOptions[["absRel"]] ,
                            " of
                            misstatement in the target population is lower than the specified materiality of <b>", 
                            planningOptions[["materialityLabel"]],
                            "</b>.")

    procedureContainer[["procedureParagraph"]] <- createJaspHtml(procedureText, "p")
    procedureContainer[["procedureParagraph"]]$position <- positionInContainer
  }
}

.auditBookValueDescriptiveTable <- function(options, 
                                            planningOptions,
                                            jaspResults,
                                            positionInContainer){

  procedureContainer <- .auditProcedureGetContainer(jaspResults,
                                                    position = 1)

  if(!options[["bookValueDescriptives"]] ||
      options[["monetaryVariable"]] == "") 
    return() 

  .updateTabNumber(jaspResults)

  if(is.null(procedureContainer[["bookValueDescriptives"]])){

    tableTitle <- paste0("<b>Table ", 
                        jaspResults[["tabNumber"]]$object, 
                        ".</b> Book Value Descriptive Statistics")
    
    descriptiveTable <- createJaspTable(tableTitle)
    descriptiveTable$position <- positionInContainer
    descriptiveTable$dependOn(options = c("bookValueDescriptives",
                                          "sampleDescriptives",
                                          "displaySample",
                                          "samplingChecked",
                                          "evaluationChecked"))

    descriptiveTable$addColumnInfo(name = 'populationSize',     
                                  title = "Population size",        
                                  type = 'string')
    descriptiveTable$addColumnInfo(name = 'populationValue',       
                                  title = "Total value",            
                                  type = 'string')
    descriptiveTable$addColumnInfo(name = 'absValue',       
                                  title = "Absolute value",            
                                  type = 'string')
    descriptiveTable$addColumnInfo(name = 'meanValue',        
                                  title = "Mean",                   
                                  type = 'string')
    descriptiveTable$addColumnInfo(name = 'sigmaValue',          
                                  title = "Std. deviation",         
                                  type = 'string')
    descriptiveTable$addColumnInfo(name = 'q1',          
                                  title = "25%",                    
                                  type = 'string', 
                                  overtitle = "Percentile")
    descriptiveTable$addColumnInfo(name = 'q2',          
                                  title = "50%",                    
                                  type = 'string', 
                                  overtitle = "Percentile")
    descriptiveTable$addColumnInfo(name = 'q3',          
                                  title = "75%",                    
                                  type = 'string', 
                                  overtitle = "Percentile")

    procedureContainer[["bookValueDescriptives"]] <- descriptiveTable

    if(options[["monetaryVariable"]] == "" || 
        options[["recordNumberVariable"]] == "")
      return()

    procedureOptions <- jaspResults[["procedureOptions"]]$object
    valuta <- planningOptions[["valuta"]]

    row <- data.frame(populationSize = procedureOptions[["populationSize"]], 
                      populationValue = paste(valuta, 
                                              round(procedureOptions[["populationValue"]], 2)), 
                      absValue = paste(valuta,
                                        round(procedureOptions[["absPopulationValue"]], 2)),
                      meanValue = paste(valuta, 
                                        round(procedureOptions[["meanValue"]], 2)), 
                      sigmaValue = paste(valuta, 
                                        round(procedureOptions[["sigmaValue"]], 2)), 
                      q1 = paste(valuta, 
                                round(procedureOptions[["quantileValue"]][1], 2)), 
                      q2 = paste(valuta, 
                                round(procedureOptions[["quantileValue"]][2], 2)), 
                      q3 = paste(valuta, 
                                round(procedureOptions[["quantileValue"]][3], 2)))
    
    descriptiveTable$addRows(row)
  }
}

.auditBarPlot <- function(column, 
                          variableName, 
                          valuta){

  h <- hist(column, plot = FALSE)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, h$counts), min.n = 4)
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(column, h$breaks), 
                                            min.n = 4)

  p <- JASPgraphs::drawAxis(
                    xName = paste0("Book values (", valuta, ")"), 
                    yName = "Counts", 
                    xBreaks = xBreaks,
                    yBreaks = yBreaks, 
                    force = TRUE,
                    xLimits = range(xBreaks),
                    yLimits = c(0, max(yBreaks))) +
        ggplot2::geom_histogram(
                  data = data.frame(column),
                  mapping = ggplot2::aes(x = column, y = ..count..),
                  binwidth = (h$breaks[2] - h$breaks[1]),
                  fill = "grey",
                  col = "black",
                  size = .7,
                  center = ((h$breaks[2] - h$breaks[1])/2))

  p <- JASPgraphs::themeJasp(p,
                             axisTickWidth = .7,
                             bty = list(type = "n", ldwX = .7, lwdY = 1))
  return(p)
}

.auditBookValueDistributionPlot <- function(dataset,  
                                            options,
                                            planningOptions, 
                                            jaspResults, 
                                            positionInContainer){

  procedureContainer <- .auditProcedureGetContainer(jaspResults,
                                                    position = 1)
  
  if(!options[["bookValueDistribution"]] ||
      options[["monetaryVariable"]] == "") 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(procedureContainer[["bookValueDistribution"]])){

    bookValuePlot <- createJaspPlot(plot = NULL, 
                                    title = "Book Value Distribution", 
                                    width = 600, 
                                    height = 300)

    bookValuePlot$position <- positionInContainer
    bookValuePlot$dependOn(options = c("bookValueDistribution", 
                                      "valuta"))

    procedureContainer[["bookValueDistribution"]] <- bookValuePlot

    if(options[["recordNumberVariable"]] == "") 
      return()

    procedureOptions <- jaspResults[["procedureOptions"]]$object

    bookValue <- dataset[, .v(options[["monetaryVariable"]])]
    meanValue <- procedureOptions[["meanValue"]]
    sigmaValue <- procedureOptions[["sigmaValue"]]
    quantileValue <- procedureOptions[["quantileValue"]]

    legendData <- data.frame(x = c(0, 0, 0), 
                            y = c(0, 0, 0), 
                            l = c("1", "2", "3"))

    p <- .auditBarPlot(column = bookValue, 
                      variableName = options[["monetaryVariable"]], 
                      valuta = planningOptions[["valuta"]])

    p <- p + ggplot2::geom_point(mapping = ggplot2::aes(x = quantileValue[1], 
                                                        y = 0), 
                                shape = 21, 
                                fill = "orange", 
                                stroke = 2, 
                                size = 2) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = quantileValue[2], 
                                                        y = 0), 
                                shape = 21, 
                                fill = "orange", 
                                stroke = 2, 
                                size = 2) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = quantileValue[3], 
                                                        y = 0), 
                                shape = 21, 
                                fill = "orange", 
                                stroke = 2, 
                                size = 2) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = meanValue, 
                                                        y = 0), 
                                shape = 21, 
                                fill = "red", 
                                stroke = 2, 
                                size = 4) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = meanValue + sigmaValue, 
                                                        y = 0), 
                                shape = 21, 
                                fill = "dodgerblue1", 
                                stroke = 2, 
                                size = 3) +
            ggplot2::geom_point(mapping = ggplot2::aes(x = meanValue - sigmaValue, 
                                                        y = 0), 
                                shape = 21, 
                                fill = "dodgerblue1", 
                                stroke = 2, 
                                size = 3) +
            ggplot2::geom_point(data = legendData, 
                                mapping = ggplot2::aes(x = x, 
                                                        y = y, 
                                                        shape = l), 
                                size = 0, 
                                color = rgb(0, 1, 0, 0)) +
            ggplot2::scale_shape_manual(name = "", 
                                        values = c(21, 21, 21), 
                                        labels = c("Mean", 
                                                    "Mean \u00B1 sd", 
                                                    "Quartile")) +
            ggplot2::guides(shape = ggplot2::guide_legend(
                              override.aes = list(size = c(4, 3, 2), 
                                                  shape = c(21, 21, 21), 
                                                  fill = c("red",
                                                          "dodgerblue1", 
                                                          "orange"), 
                                                  stroke = 2, 
                                                  color = "black")), 
                            order = 1) 

    myTheme <- ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -10, r = 50)),
                              panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"))
    
    p <- JASPgraphs::themeJasp(p, legend.position = "top") + myTheme

    bookValuePlot$plotObject <- p

  }

  if(options[["explanatoryText"]]){

      bookValuePlotText <- createJaspHtml(paste0("<b>Figure ", 
                                                 jaspResults[["figNumber"]]$object ,
                                                 ".</b> The distribution of book values in the population. The red and blue dots respectively represent the mean
                                                 and the values exactly one standard deviation from the mean. The orange dots represent the 25th, 50th (median) and
                                                 75th percentile of the book values."), "p")
      
      bookValuePlotText$position <- positionInContainer + 1
      bookValuePlotText$dependOn(optionsFromObject = procedureContainer[["bookValueDistribution"]])
      bookValuePlotText$dependOn(options = "explanatoryText")
      procedureContainer[["bookValuePlotText"]] <- bookValuePlotText

  }
}

################################################################################
################## Functions for the planning ##################################
################################################################################

.auditPlanningGetContainer <- function(jaspResults, 
                                       position){

  if(!is.null(jaspResults[["planningContainer"]])){

    return(jaspResults[["planningContainer"]])

  } else {
                                         
    planningContainer <- createJaspContainer(title = "<u>Planning</u>")
    planningContainer$position <- position
    planningContainer$dependOn(options = c("IR", 
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
                                           "recordNumberVariable",
                                           "monetaryVariable",
                                           "valuta",
                                           "otherValutaName"))

    jaspResults[["planningContainer"]] <- planningContainer

    return(planningContainer)
  }
}

.auditPlanningErrorChecks <- function(options, 
                                      planningOptions, 
                                      planningContainer, 
                                      ready){

  if(ready){
    if(options[["materiality"]] == "materialityAbsolute" && 
        options[["materialityValue"]] >= planningOptions[["populationValue"]])
      planningContainer$setError("Analysis not possible: Your materiality is higher than the total value of the observations.") 

    expTMP <- ifelse(options[['expectedErrors']] == "expectedRelative", 
                     yes = options[["expectedPercentage"]], 
                     no = options[["expectedNumber"]] / 
                          planningOptions[["populationValue"]])

    if(expTMP > planningOptions[["materiality"]])
      planningContainer$setError("Analysis not possible: Your expected errors are higher than materiality.")
  }
}

.auditPlanningOptions <- function(options, 
                                  jaspResults, 
                                  rawData = FALSE){

  valuta <- base::switch(options[["valuta"]], 
                         "euroValuta" = "\u20AC", 
                         "dollarValuta" = "\u0024", 
                         "otherValuta" = options[["otherValutaName"]])
  
  confidence <- options[["confidence"]]
  confidenceLabel <- paste0(round(confidence * 100, 2), "%")

  if(!rawData){

    populationSize <- options[["populationSize"]]
    populationValue <- ifelse(options[["populationValue"]] == 0, 
                              yes = 0.01, 
                              no = options[["populationValue"]])

  } else {
    
    procedureOptions <- jaspResults[["procedureOptions"]]$object
    populationSize <- procedureOptions[["populationSize"]]
    populationValue <- procedureOptions[["populationValue"]]

  }

  absRel <- ifelse(options[["materiality"]] == "materialityRelative", 
                   yes = "<b>percentage</b>", 
                   no = "<b>amount</b>")

  materiality <- ifelse(options[["materiality"]] == "materialityRelative",
                        yes = options[["materialityPercentage"]], 
                        no = options[["materialityValue"]] / 
                             populationValue)

  materialityLabel <- ifelse(options[["materiality"]] == "materialityRelative",
                             yes = paste0(round(materiality * 100, 2), "%"), 
                             no = paste(valuta, 
                                        format(options[["materialityValue"]], 
                                               scientific = FALSE)))
  
  expectedErrors <- ifelse(options[["expectedErrors"]] == "expectedRelative", 
                           yes = options[["expectedPercentage"]], 
                           no = options[["expectedNumber"]] / 
                                populationValue)

  expectedErrorsLabel <- ifelse(options[["expectedErrors"]] == "expectedRelative", 
                                yes = paste0(round(expectedErrors * 100, 2), "%"), 
                                no = paste(valuta, options[["expectedNumber"]]))
  
  likelihood <- base::switch(options[["planningModel"]], 
                             "Poisson" = "poisson", 
                             "binomial" = "binomial", 
                             "hypergeometric" = "hypergeometric")
  
  optionsList <- list("valuta" = valuta, 
                      "confidence" = confidence, 
                      "confidenceLabel" = confidenceLabel, 
                      "absRel" = absRel, 
                      "populationSize" = populationSize, 
                      "populationValue" = populationValue, 
                      "materiality" = materiality, 
                      "materialityLabel" = materialityLabel, 
                      "expectedErrors" = expectedErrors, 
                      "expectedErrorsLabel" = expectedErrorsLabel,
                      "likelihood" = likelihood)

  return(optionsList)
}

.auditPlanningReady <- function(options, 
                                planningOptions){

  if(options[["materiality"]] == "materialityAbsolute"){

    ready <- options[["materialityValue"]] != 0 && 
              planningOptions[["populationSize"]] != 0 && 
              planningOptions[["populationValue"]] != 0 && 
              planningOptions[["populationValue"]] != 0.01

  } else {

    ready <- options[["materialityPercentage"]] != 0 && 
              planningOptions[["populationSize"]] != 0

  }

  return(ready)
}

.auditPlanningState <- function(options, 
                                planningOptions, 
                                planningContainer, 
                                ready, 
                                type){
                                  
  if(!is.null(planningContainer[["planningState"]])){

    return(planningContainer[["planningState"]]$object)

  } else if(ready){

  auditRisk <- 1 - options[["confidence"]]

  if(options[["IR"]] != "Custom"){

    inherentRisk <- base::switch(options[["IR"]], 
                                 "Low" = 0.50, 
                                 "Medium" = 0.60, 
                                 "High" = 1)

  } else {

    inherentRisk <- options[["irCustom"]]
  }

  if(options[["CR"]] != "Custom"){

    controlRisk <- base::switch(options[["CR"]], 
                                "Low" = 0.50, 
                                "Medium" = 0.60, 
                                "High" = 1)

  } else {

    controlRisk <- options[["crCustom"]]

  }

  detectionRisk <- auditRisk / inherentRisk / controlRisk

    if(type == "frequentist"){

      adjustedConfidence <- 1 - detectionRisk

      result <- try({
        jfa::planning(materiality = planningOptions[["materiality"]], 
                      confidence = adjustedConfidence, 
                      expectedError = planningOptions[["expectedErrors"]], 
                      likelihood = planningOptions[["likelihood"]], 
                      N = planningOptions[["populationSize"]])
                    })

    } else if(type == "bayesian"){

      result <- try({

        prior <- jfa::auditPrior(materiality = planningOptions[["materiality"]], 
                                confidence = planningOptions[["confidence"]],
                                expectedError = planningOptions[["expectedErrors"]], 
                                likelihood = planningOptions[["likelihood"]], 
                                N = planningOptions[["populationSize"]], 
                                ir = inherentRisk, 
                                cr = controlRisk)

        jfa::planning(materiality = planningOptions[["materiality"]], 
                      confidence = planningOptions[["confidence"]], 
                      expectedError = planningOptions[["expectedErrors"]], 
                      N = planningOptions[["populationSize"]], 
                      prior = prior)

                    })

    }

    if(isTryError(result)){

      planningContainer$setError(paste0("An error occurred: ", 
                                        JASP:::.extractErrorMessage(result)))
      return()
    }

    if(result[["sampleSize"]] > planningOptions[["populationSize"]]){

      planningContainer$setError("The resulting sample size is larger than the population size.")
      return()
    }

    planningContainer[["planningState"]] <- createJaspState(result)
    planningContainer[["planningState"]]$dependOn(options = c("planningModel"))
    
    return(result)

  } else {

    bPrior <- ifelse(options[["planningModel"]] == "Poisson", 
                     yes = 0, 
                     no = 1)

    noResults <- list(sampleSize = 0, 
                      materiality = planningOptions[["materiality"]], 
                      N = planningOptions[["populationSize"]], 
                      expectedSampleError = 0,
                      prior = list(aPrior = 1, 
                                   bPrior = bPrior, 
                                   nPrior = 0, 
                                   kPrior = 0))

    return(noResults)

  }
}

.auditExplanatoryTextPlanning <- function(options, 
                                          planningOptions, 
                                          planningState, 
                                          planningContainer, 
                                          ready, 
                                          type, 
                                          positionInContainer){

  if(options[["explanatoryText"]] && 
      is.null(planningContainer[["planningParagraph"]]) && 
      !planningContainer$getError()){

    if(type == "frequentist"){

      planningContainer[["planningParagraph"]] <- createJaspHtml(paste0("The most likely error in the data was expected to be <b>", 
                                                                        planningOptions[["expectedErrorsLabel"]],
                                                                        "</b>. The sample size that is required to for a materiality of <b>", 
                                                                        planningOptions[["materialityLabel"]] ,
                                                                        "</b>, assuming
                                                                        the sample contains <b>", 
                                                                        planningOptions[["expectedSampleError"]],
                                                                        "</b> full errors, is <b>", 
                                                                        planningState[["sampleSize"]] ,
                                                                        "</b>. This sample size is based on the <b>", 
                                                                        options[["planningModel"]], 
                                                                        "</b> distribution, the inherent risk <b>(", 
                                                                        options[["IR"]], 
                                                                        ")</b>, the
                                                                        control risk <b>(", 
                                                                        options[["CR"]], 
                                                                        ")</b> and the expected errors. Consequently, if the sum of errors from the audited observations remains below <b>", 
                                                                        planningOptions[["expectedErrorsLabel"]] ,
                                                                        "</b>, the
                                                                        maximum misstatement is estimated to be below materiality and the population can be approved."), "p")
    
    } else if(type == "bayesian"){

      distribution <- base::switch(planningOptions[["likelihood"]], 
                                  "poisson" = "gamma", 
                                  "binomial" = "beta", 
                                  "hypergeometric" = "beta-binomial")

      planningContainer[["planningParagraph"]] <- createJaspHtml(paste0("The most likely error in the data was expected to be <b>", 
                                                                        planningOptions[["expectedErrorsLabel"]],
                                                                        "</b>.  The sample size that is required to for a materiality of <b>", 
                                                                        planningOptions[["materialityLabel"]],
                                                                        "</b>, assuming
                                                                        the sample contains <b>", 
                                                                        planningState[["expectedSampleError"]],
                                                                        "</b> full errors, is <b>", 
                                                                        planningState[["sampleSize"]],
                                                                        "</b>. This sample size is calculated according to the <b>", 
                                                                        distribution, 
                                                                        "</b> distribution, the inherent risk <b>(", 
                                                                        options[["IR"]], 
                                                                        ")</b>,
                                                                        the control risk <b>(", 
                                                                        options[["CR"]], 
                                                                        ")</b> and the expected errors. The information in this prior distribution states that there is a <b>",
                                                                        round(pbeta(planningState[["materiality"]], planningState[["prior"]]$aPrior, planningState[["prior"]]$bPrior) * 100, 2),
                                                                        "%</b> prior probability 
                                                                        that the population misstatement is lower than materiality. Consequently, if the sum of errors from the audited observations remains 
                                                                        below <b>", 
                                                                        planningOptions[["expectedErrorsLabel"]],
                                                                        "</b> the maximum misstatement is estimated to be below materiality and the population can be approved."), "p")
    }

    planningContainer[["planningParagraph"]]$position <- positionInContainer
    planningContainer[["planningParagraph"]]$dependOn(options = "explanatoryText")
  }
}

.auditPlanningSummaryTable <- function(options, 
                                       planningOptions, 
                                       planningState, 
                                       planningContainer, 
                                       jaspResults,
                                       ready, 
                                       type, 
                                       positionInContainer){

  .updateTabNumber(jaspResults)

  if(!is.null(planningContainer[["summaryTable"]]))
    return()

  tableTitle <- paste0("<b>Table ", 
                      jaspResults[["tabNumber"]]$object, 
                      ".</b> Planning Summary")                                        

  summaryTable <- createJaspTable(tableTitle)
  summaryTable$position <- positionInContainer
  summaryTable$dependOn(options = c("bookValueDescriptives",
                                    "sampleDescriptives",
                                    "displaySample",
                                    "samplingChecked",
                                    "evaluationChecked",
                                    "planningModel",
                                    "expectedBF"))

  summaryTable$addColumnInfo(name = 'materiality',          
                             title = "Materiality",          
                             type = 'string')
  summaryTable$addColumnInfo(name = 'IR',                   
                             title = "Inherent risk",        
                             type = 'string')
  summaryTable$addColumnInfo(name = 'CR',                   
                             title = "Control risk",         
                             type = 'string')
  summaryTable$addColumnInfo(name = 'DR',                   
                             title = "Detection risk",       
                             type = 'string')
  summaryTable$addColumnInfo(name = 'k',                    
                             title = "Expected errors",       
                             type = 'string')
  summaryTable$addColumnInfo(name = 'n',                    
                             title = "Required sample size", 
                             type = 'string')

  if(type == "bayesian" && options[["expectedBF"]]){
    summaryTable$addColumnInfo(name = 'expBF',              
                               title = "Expected BF\u208B\u208A", 
                               type = 'string')
  }

  planningContainer[["summaryTable"]] <- summaryTable

  auditRisk <- 1 - options[["confidence"]]

  if(options[["IR"]] != "Custom"){

    inherentRisk <- base::switch(options[["IR"]], 
                                 "Low" = 0.50, 
                                 "Medium" = 0.60, 
                                 "High" = 1)

  } else {

    inherentRisk <- options[["irCustom"]]
  }

  if(options[["CR"]] != "Custom"){

    controlRisk <- base::switch(options[["CR"]], 
                                "Low" = 0.50, 
                                "Medium" = 0.60, 
                                "High" = 1)

  } else {

    controlRisk <- options[["crCustom"]]

  }

  detectionRisk <- auditRisk / inherentRisk / controlRisk

  if(!ready || planningContainer$getError()){

    if(type == "frequentist"){

      message <- base::switch(options[["planningModel"]],
                              "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution."),
                              "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution."),
                              "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution."))
    
    } else if(type == "bayesian"){

      message <- base::switch(options[["planningModel"]],
                              "Poisson" = "The required sample size is based on the <b>gamma</b> distribution.",
                              "binomial" = "The required sample size is based on the <b>beta</b> distribution.",
                              "hypergeometric" = paste0("The required sample size is based on the <b>beta-binomial</b> distribution (N = ", options[["populationSize"]] ,")."))
    
    }

    summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

    row <- data.frame(materiality = planningOptions[["materialityLabel"]], 
                      IR = paste0(round(inherentRisk * 100, 2), "%"), 
                      CR = paste0(round(controlRisk * 100, 2), "%"), 
                      DR = paste0(round(detectionRisk * 100, 2), "%"), 
                      k = ".", 
                      n = ".")

    if(type == "bayesian" && options[["expectedBF"]])
      row <- cbind(row, expBF = ".")
    
    summaryTable$addRows(row)
    summaryTable$addFootnote(message = "Either the materiality, the population size, or the population value is defined as zero.", symbol="<b>Analysis not ready.</b>")
    
    return()
  }

  if(type == "frequentist"){

    message <- base::switch(options[["planningModel"]],
                            "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution <i>(\u03BB = ", 
                                          round(
                                            planningState[["materiality"]] * 
                                            planningState[["sampleSize"]], 
                                            4), 
                                          ")</i>."),
                            "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution <i>(p = ", 
                                            round(
                                              planningState[["materiality"]], 
                                              2) ,
                                            ")</i>."),
                            "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution <i>(N = ", 
                                                      planningState[["N"]],
                                                      ", K = ", 
                                                      ceiling(
                                                        planningState[["N"]] * 
                                                        planningState[["materiality"]]
                                                        ),
                                                      ")</i>."))
  
  } else if(type == "bayesian"){

    message <- base::switch(options[["planningModel"]],
                            "Poisson" = paste0("The required sample size is based on the <b>gamma</b> distribution <i>(\u03B1 = ", 
                                               planningState[["prior"]]$aPrior,
                                               ", \u03B2 = ", 
                                               planningState[["prior"]]$bPrior, 
                                               ")</i>."),
                            "binomial" = paste0("The required sample size is based on the <b>beta</b> distribution <i>(\u03B1 = ", 
                                                planningState[["prior"]]$aPrior,
                                                ", \u03B2 = ", 
                                                planningState[["prior"]]$bPrior, 
                                                ")</i>."),
                            "hypergeometric" = paste0("The required sample size is based on the <b>beta-binomial</b> distribution <i>(N = ", 
                                                      planningState[["N"]] - 
                                                      planningState[["sampleSize"]] +
                                                      planningState[["expectedSampleError"]],
                                                      ", \u03B1 = ", 
                                                      planningState[["prior"]]$aPrior, 
                                                      ", \u03B2 = ", 
                                                      planningState[["prior"]]$bPrior, 
                                                      ")</i>."))
  
  }

  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

  k <- base::switch(options[["expectedErrors"]], 
                    "expectedRelative" = planningState[["expectedSampleError"]], 
                    "expectedAbsolute" = paste(
                                          planningOptions[["valuta"]], 
                                          options[["expectedNumber"]]))
  
  n <- planningState[["sampleSize"]]

  row <- data.frame(materiality = planningOptions[["materialityLabel"]], 
                    IR = paste0(round(inherentRisk * 100, 2), "%"), 
                    CR = paste0(round(controlRisk * 100, 2), "%"), 
                    DR = paste0(round(detectionRisk * 100, 2), "%"), 
                    k = k, 
                    n = n)

  if(type == "bayesian" && options[["expectedBF"]]){

    BFresult <- .expectedBF(planningState)
    expectedBF <- BFresult[["expectedBF"]]
    row <- cbind(row, expBF = expectedBF)
  
  }

  summaryTable$addRows(row)
}

.decisionAnalysisPlot <- function(options, 
                                  planningOptions, 
                                  planningState, 
                                  planningContainer, 
                                  jaspResults,
                                  ready, 
                                  type, 
                                  positionInContainer){

  if(!options[["decisionPlot"]]) 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(planningContainer[["decisionPlot"]])){

    decisionPlot <- createJaspPlot(plot = NULL, 
                                  title = "Decision Analysis Plot", 
                                  width = 600, 
                                  height = 300)

    decisionPlot$position <- positionInContainer
    decisionPlot$dependOn(options = c("decisionPlot"))

    planningContainer[["decisionPlot"]] <- decisionPlot

    if(!ready || planningContainer$getError()) 
      return()

    auditRisk <- 1 - options[["confidence"]]

    if(options[["IR"]] != "Custom"){

      inherentRisk <- base::switch(options[["IR"]], 
                                  "Low" = 0.50, 
                                  "Medium" = 0.60, 
                                  "High" = 1)

    } else {

      inherentRisk <- options[["irCustom"]]
    }

    if(options[["CR"]] != "Custom"){

      controlRisk <- base::switch(options[["CR"]], 
                                  "Low" = 0.50, 
                                  "Medium" = 0.60, 
                                  "High" = 1)

    } else {

      controlRisk <- options[["crCustom"]]

    }

    detectionRisk <- auditRisk / inherentRisk / controlRisk

    if(type == "frequentist"){

      adjustedConfidence <- 1 - detectionRisk

      startProgressbar(3)

      n1 <- jfa::planning(materiality = planningOptions[["materiality"]], 
                          confidence = adjustedConfidence, 
                          expectedError = planningOptions[["expectedErrors"]], 
                          likelihood = "binomial", 
                          N = planningOptions[["populationSize"]])

      progressbarTick() 

      n2 <- jfa::planning(materiality = planningOptions[["materiality"]], 
                          confidence = adjustedConfidence, 
                          expectedError = planningOptions[["expectedErrors"]], 
                          likelihood = "poisson", 
                          N = planningOptions[["populationSize"]])

      progressbarTick()      

      n3 <- jfa::planning(materiality = planningOptions[["materiality"]], 
                          confidence = adjustedConfidence, 
                          expectedError = planningOptions[["expectedErrors"]], 
                          likelihood = "hypergeometric", 
                          N = planningOptions[["populationSize"]])

      progressbarTick()
      
      n <- c(n1$sampleSize, 
             n2$sampleSize, 
             n3$sampleSize)   

      k <- c(n1$expectedSampleError, 
             n2$expectedSampleError, 
             n3$expectedSampleError)

      d <- data.frame(y = c(n, k), 
                      dist = rep(c("Binomial", "Poisson", "Hypergeometric"), 2),
                      nature = rep(c("Expected error-free", "Expected errors"), 
                                   each = 3))
      d$dist <- factor(x = d$dist, levels = levels(d$dist)[c(2, 3, 1)])
      d$nature <- factor(d$nature, levels = levels(d$nature)[c(1, 2)])

    } else if(type == "bayesian"){

      startProgressbar(3)

      p1 <- jfa::auditPrior(materiality = planningOptions[["materiality"]], 
                            confidence = planningOptions[["confidence"]],
                            expectedError = planningOptions[["expectedErrors"]], 
                            likelihood = "binomial", 
                            N = planningOptions[["populationSize"]], 
                            ir = inherentRisk, 
                            cr = controlRisk)

      n1 <- jfa::planning(materiality = planningOptions[["materiality"]], 
                          confidence = planningOptions[["confidence"]], 
                          expectedError = planningOptions[["expectedErrors"]], 
                          N = planningOptions[["populationSize"]], 
                          prior = p1)
      
      progressbarTick()
      
      p2 <- jfa::auditPrior(materiality = planningOptions[["materiality"]], 
                            confidence = planningOptions[["confidence"]],
                            expectedError = planningOptions[["expectedErrors"]], 
                            likelihood = "poisson", 
                            N = planningOptions[["populationSize"]], 
                            ir = inherentRisk, 
                            cr = controlRisk)

      n2 <- jfa::planning(materiality = planningOptions[["materiality"]], 
                          confidence = planningOptions[["confidence"]], 
                          expectedError = planningOptions[["expectedErrors"]], 
                          N = planningOptions[["populationSize"]], 
                          prior = p2)
      
      progressbarTick()

      p3 <- jfa::auditPrior(materiality = planningOptions[["materiality"]], 
                            confidence = planningOptions[["confidence"]],
                            expectedError = planningOptions[["expectedErrors"]], 
                            likelihood = "hypergeometric", 
                            N = planningOptions[["populationSize"]], 
                            ir = inherentRisk, 
                            cr = controlRisk)

      n3 <- jfa::planning(materiality = planningOptions[["materiality"]], 
                          confidence = planningOptions[["confidence"]], 
                          expectedError = planningOptions[["expectedErrors"]], 
                          N = planningOptions[["populationSize"]], 
                          prior = p3)

      progressbarTick()

      n <- c(n1$sampleSize, 
             n2$sampleSize, 
             n3$sampleSize)  

      k <- c(n1$expectedSampleError, 
             n2$expectedSampleError, 
             n3$expectedSampleError)

      d <- data.frame(y = c(n, k), 
                      dist = rep(c("Beta", "Gamma", "Beta-binomial"), 2),
                      nature = rep(c("Expected error-free", "Expected errors"), 
                                   each = 3))
      d$dist <- factor(x = d$dist, levels = levels(d$dist)[c(2, 3, 1)])
      d$nature <- factor(x = d$nature, levels = levels(d$nature)[c(1, 2)])
    }

    yBreaks <- JASPgraphs::getPrettyAxisBreaks(0:(ceiling(1.1 * max(n))), min.n = 4)
    yLimits <- c(0, ceiling(1.2 * max(n)))

    myTheme <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), 
                              axis.ticks.y = ggplot2::element_blank(), 
                              axis.text.y = ggplot2::element_text(hjust = 0),
                              panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
                              legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30)))

    p <- ggplot2::ggplot(data = d, 
                         mapping = ggplot2::aes(x = dist, y = y, fill = nature)) +
          ggplot2::geom_bar(stat = "identity", 
                            col = "black", 
                            size = 1) +
          ggplot2::scale_y_continuous(breaks = yBreaks, 
                                      limits = yLimits) +
          ggplot2::coord_flip() +
          ggplot2::annotate("text", 
                            y = k, 
                            x = c(3, 2, 1), 
                            label = k, 
                            size = 6, 
                            vjust = 0.5, 
                            hjust = -0.3) + 
      ggplot2::annotate("text", 
                        y = n, 
                        x = c(3, 2, 1), 
                        label = n, 
                        size = 6, 
                        vjust = 0.5, 
                        hjust = -0.5) +
      ggplot2::xlab("") +
      ggplot2::ylab("Required sample size") +
      ggplot2::labs(fill = "") +
      ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), 
                                 guide = ggplot2::guide_legend(reverse = TRUE))

    p <- JASPgraphs::themeJasp(p, 
                              sides = "", 
                              legend.position = "top") + myTheme

    decisionPlot$plotObject <- p
  }

  if(options[["explanatoryText"]] && ready){

    decisionPlotText <- createJaspHtml(paste0("<b>Figure ", 
                                              jaspResults[["figNumber"]]$object,
                                              ".</b> Decision analysis for the current options. The bars represent the sample size that is required under different planning distributions.
                                              The number of expected errors in the selection is colored in red and the number of expected error-free observations is colored in green."), "p")
    
    decisionPlotText$position <- positionInContainer + 1
    decisionPlotText$dependOn(optionsFromObject = planningContainer[["decisionPlot"]])
    decisionPlotText$dependOn(options = "explanatoryText")
    planningContainer[["decisionPlotText"]] <- decisionPlotText  
  }
}

.samplingDistributionPlot <- function(options, 
                                      planningOptions, 
                                      planningState, 
                                      planningContainer, 
                                      jaspResults,
                                      ready, 
                                      positionInContainer){

  if(!options[["samplingDistribution"]]) 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(planningContainer[["samplingDistribution"]])){

    likelihood <- base::switch(options[["planningModel"]], 
                               "Poisson" = "Poisson", 
                               "binomial" = "Binomial", 
                               "hypergeometric" = "Hypergeometric")

    plotTitle <- paste0("Implied ", likelihood, " Sampling Distribution")

    samplingDistribution <- createJaspPlot(plot = NULL, 
                                           title = plotTitle, 
                                           width = 600, 
                                           height = 300)

    samplingDistribution$position <- positionInContainer
    samplingDistribution$dependOn(options = c("planningModel", 
                                              "samplingDistribution"))

    planningContainer[["samplingDistribution"]] <- samplingDistribution

    if(!ready || planningContainer$getError()) 
      return()

    xVals <- 0:planningState[["sampleSize"]]
    limx <- planningState[["sampleSize"]] + 1
    if(limx > 31)
        limx <- 31
    xVals <- xVals[1:limx]

    if(planningState[["likelihood"]] == "poisson"){

      dErrorFree <- stats::dpois(x = xVals, 
                                 lambda = planningState[["materiality"]] * 
                                          planningState[["sampleSize"]])

      dError <- stats::dpois(x = 0:planningState[["expectedSampleError"]], 
                             lambda = planningState[["materiality"]] * 
                                      planningState[["sampleSize"]])

    } else if(planningState[["likelihood"]] == "binomial"){

      dErrorFree <- stats::dbinom(x = xVals, 
                                  size = planningState[["sampleSize"]], 
                                  prob = planningState[["materiality"]])

      dError <- stats::dbinom(x = 0:planningState[["expectedSampleError"]], 
                              size = planningState[["sampleSize"]], 
                              prob = planningState[["materiality"]])

    } else if(planningState[["likelihood"]] == "hypergeometric"){

      dErrorFree <- stats::dhyper(x = xVals, 
                                  m = planningState[["populationK"]], 
                                  n = planningState[["N"]] - 
                                      planningState[["populationK"]], 
                                  k = planningState[["sampleSize"]])

      dError <- stats::dhyper(x = 0:planningState[["expectedSampleError"]], 
                              m = planningState[["populationK"]], 
                              n = planningState[["N"]] - 
                                  planningState[["populationK"]], 
                              k = planningState[["sampleSize"]])

    }

    dataErrorFree <- data.frame(x = xVals, 
                                y = dErrorFree)
    dataError <- data.frame(x = 0:planningState[["expectedSampleError"]], 
                            y = dError)

    dataLegend <- data.frame(x = c(0, 0), 
                             y = c(0, 0), 
                             type = c("Expected error-free", "Expected errors"))
    dataLegend$type <- factor(x = dataLegend[["type"]], 
                              levels = levels(dataLegend[["type"]])[c(2,1)])

    xTicks <- JASPgraphs::getPrettyAxisBreaks(xVals)
    yTicks <- JASPgraphs::getPrettyAxisBreaks(dataErrorFree[["y"]])

    myLegend <- ggplot2::guide_legend(override.aes=list(size = 12, 
                                                       shape = 22, 
                                                       fill = c("#FF6666", 
                                                                "#7FE58B"), 
                                                       stroke = 1.5, 
                                                       color = "black"))
    
    myTheme <- ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
                              legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30)))

    p <- ggplot2::ggplot(data = dataLegend, 
                         mapping = ggplot2::aes(x = x, y = y, fill = type)) +
          ggplot2::geom_point(shape = 2, 
                              alpha = 0) +
          ggplot2::scale_x_continuous(name = "n", 
                                      labels = xTicks, 
                                      breaks = xTicks) +
          ggplot2::scale_y_continuous(name = "Probability", 
                                      labels = yTicks, 
                                      breaks = yTicks) +
          ggplot2::geom_bar(data = dataErrorFree, 
                            mapping = ggplot2::aes(x = x, y = y), 
                            stat = "identity", 
                            fill = "#7FE58B", 
                            size = 0.5, 
                            color = "black") +
          ggplot2::geom_bar(data = dataError, 
                            mapping = ggplot2::aes(x = x, y = y), 
                            stat = "identity", 
                            fill = "#FF6666", 
                            size = 0.5, 
                            color = "black") +
          ggplot2::geom_point(data = dataLegend, 
                              mapping = ggplot2::aes(x = x, y = y, fill = type), 
                              size = 0) +
          ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), 
                                     guide = ggplot2::guide_legend(reverse = TRUE)) +
          ggplot2::labs(fill = "") +
          ggplot2::guides(fill = myLegend)
    
    p <- JASPgraphs::themeJasp(p, legend.position = "top") + myTheme

    samplingDistribution$plotObject <- p

  }

  if(options[["explanatoryText"]] && ready){

    samplingDistributionText <- createJaspHtml(paste0("<b>Figure ", 
                                                      jaspResults[["figNumber"]]$object ,
                                                      ".</b> The implied <b>", 
                                                      options[["planningModel"]], 
                                                      "</b> sampling distribution. The number of expected errors in the selection is colored in 
                                                      red and the number of expected error-free observations is colored in green. The total probability of the errors does 
                                                      not exceed the detection risk as specified through the audit risk model."), "p")
    
    samplingDistributionText$position <- positionInContainer + 1
    samplingDistributionText$dependOn(optionsFromObject = planningContainer[["samplingDistribution"]])
    samplingDistributionText$dependOn(options = "explanatoryText")
    planningContainer[["samplingDistributionText"]] <- samplingDistributionText
  }
}

################################################################################
################## Functions for the selection #################################
################################################################################

.auditAddSelectionColumns <- function(options, 
                                      jaspResults){

  dataset <- .auditReadDataProcedure(options, 
                                     jaspResults)
  
  rankingVariable <- options[["rankingVariable"]]
  if(rankingVariable == "")
    rankingVariable <- NULL

  additionalVariables <- unlist(options[["additionalVariables"]])
  variables <- c(rankingVariable, additionalVariables)

  if(!is.null(variables)){

    additionalColumns <- .readDataSetToEnd(columns.as.numeric = variables)
    dataset <- cbind(dataset, additionalColumns)
    return(dataset)

  } else {

    return(dataset)

  }
}

.auditSelectionGetContainer <- function(jaspResults, 
                                        position){

  planningContainer <- jaspResults[["planningContainer"]]
  planningState <- planningContainer[["planningState"]]$object

  if(!is.null(jaspResults[["selectionContainer"]])){

    return(jaspResults[["selectionContainer"]])

  } else if(!is.null(planningState)){
                                         
    selectionContainer <- createJaspContainer(title= "<u>Selection</u>")
    selectionContainer$position <- position
    selectionContainer$dependOn(optionsFromObject = planningContainer,
                                options = c("samplingChecked",
                                            "selectionMethod",
                                            "selectionType",
                                            "seed",
                                            "intervalStartingPoint",
                                            "additionalVariables",
                                            "rankingVariable",
                                            "valuta",
                                            "otherValutaName"))

    jaspResults[["selectionContainer"]] <- selectionContainer

    return(selectionContainer)
  }
}

.auditExplanatoryTextSelection <- function(options, 
                                           planningOptions,
                                           planningState, 
                                           selectionState,
                                           selectionContainer, 
                                           positionInContainer){

  if(options[["explanatoryText"]]){

    samplingLabel <- base::switch(options[["selectionMethod"]], 
                                  "randomSampling" = "random", 
                                  "systematicSampling" = "fixed interval", 
                                  "cellSampling" = "cell")

    samplingLabel <- base::switch(options[["selectionType"]], 
                                  "recordSampling" = paste(samplingLabel, "record sampling"), 
                                  "musSampling" = paste(samplingLabel, "monetary unit sampling"))

    message <- paste0("From the population of <b>", 
                        planningOptions[["populationSize"]], 
                        "</b> observations, <b>", 
                        planningState[["sampleSize"]], 
                        "</b> observations were selected using a <b>", 
                        samplingLabel, 
                        "</b> method.")

    if(sum(selectionState[["count"]]) > nrow(selectionState)){

      message <- paste0(message, " 
                        <b>Note:</b> The selected subset (", 
                        nrow(selectionState) ,
                        ") is smaller than the planned sample size (", 
                        planningState[["sampleSize"]] ,
                        "), as observations are selected multiple times due 
                        to their high value. These observations (", 
                        planningState[["sampleSize"]] - nrow(selectionState),
                        ") are counted multiple times in the evaluation.")
      
    }


    selectionContainer[["samplingParagraph"]] <- createJaspHtml(message, "p")
    selectionContainer[["samplingParagraph"]]$position <- positionInContainer
    selectionContainer[["samplingParagraph"]]$dependOn(options = "explanatoryText")
  }
}

.auditSelectionState <- function(dataset,
                                 options, 
                                 planningState, 
                                 selectionContainer){
                                  
  if(!is.null(selectionContainer[["selectionState"]])){

    return(selectionContainer[["selectionState"]]$object)

  } else if(!is.null(planningState)){

    result <- try({

      .auditSampling(dataset,
                     options,
                     planningState,
                     selectionContainer)

    })

    if(isTryError(result)){

      selectionContainer$setError(paste0("An error occurred: ", 
                                        JASP:::.extractErrorMessage(result)))
      return()
    }

    selectionContainer[["selectionState"]] <- createJaspState(result)
    
    return(result)
  }
}

.auditSampling <- function(dataset,
                           options,
                           planningState,
                           selectionContainer){

  algorithm <- base::switch(options[["selectionMethod"]],
                            "randomSampling" = "random",
                            "cellSampling" = "cell",
                            "systematicSampling" = "interval")

  if(options[["rankingVariable"]] != ""){
    rankingColumn <- dataset[, .v(options[["rankingVariable"]])]
    dataset <- dataset[order(rankingColumn), ]
  }

  if(options[["monetaryVariable"]] != ""){
    bookValues <- .v(options[["monetaryVariable"]])
  } else {
    bookValues <- NULL
  }

  units <- base::switch(options[["selectionType"]],
                        "recordSampling" = "records",
                        "musSampling" = "mus")
  sample <- jfa::sampling(population = dataset, 
                          sampleSize = planningState[["sampleSize"]], 
                          algorithm = algorithm, 
                          units = units, 
                          seed = options[["seed"]],
                          ordered = FALSE,
                          bookValues = bookValues,
                          intervalStartingPoint = 1)                                

  sample <- data.frame(apply(X = sample[["sample"]], MARGIN = 2, as.numeric))
  return(sample)
}

.auditSelectionSummaryTable <- function(options, 
                                        planningOptions,
                                        planningState,
                                        selectionState,
                                        selectionContainer,
                                        jaspResults, 
                                        positionInContainer){

  .updateTabNumber(jaspResults)

  if(!is.null(selectionContainer[["selectionInformationTable"]])) 
    return()

  tableTitle <- paste0("<b>Table ", 
                      jaspResults[["tabNumber"]]$object, 
                      ".</b> Selection Summary")
  
  selectionInformationTable <- createJaspTable(tableTitle)
  selectionInformationTable$position <- positionInContainer
  selectionInformationTable$dependOn(options = c("bookValueDescriptives",
                                                "sampleDescriptives",
                                                "displaySample",
                                                "samplingChecked",
                                                "evaluationChecked"))
  
  selectionInformationTable$addColumnInfo(name = "size", 
                                          title ="Selection size", 
                                          type = "integer")

  if(options[["materiality"]] == "materialityAbsolute"){

    selectionInformationTable$addColumnInfo(name = "value", 
                                            title = "Selection value", 
                                            type = "string")
    selectionInformationTable$addColumnInfo(name = "percentage", 
                                            title = "% of population value", 
                                            type = "string")  
  } else {

    selectionInformationTable$addColumnInfo(name = "percentage", 
                                            title = "% of total observations", 
                                            type = "string")  

  }

  if(options[["selectionMethod"]] != "randomSampling")
    selectionInformationTable$addColumnInfo(name = "interval", 
                                            title ="Interval", 
                                            type = "string")

  if(options[["selectionMethod"]] != "systematicSampling"){
    message <- paste0("The sample is drawn with <i>seed ", 
                  options[["seed"]], 
                  "</i>.")
  } else {
    message <- "The first unit from every interval is selected."
  }

  selectionInformationTable$addFootnote(message = message, 
                                        symbol = "<i>Note.</i>")

  selectionContainer[["selectionInformationTable"]] <- selectionInformationTable

  if(options[["selectionType"]] == "recordSampling"){

    interval <- ceiling(planningOptions[["populationSize"]] / 
                        planningState[["sampleSize"]])

  } else {

    interval <- ceiling(planningOptions[["populationValue"]] / 
                        planningState[["sampleSize"]])

  }

  sampleSize <- sum(selectionState[["count"]])

  if(options[["materiality"]] == "materialityAbsolute"){

    value <- ceiling(sum(abs(selectionState[, .v(options[["monetaryVariable"]])])))
    percentage <- paste0(round(value / planningOptions[["populationValue"]] * 100, 2), "%")

    row  <- data.frame("size" = sampleSize, 
                       "value" = paste(planningOptions[["valuta"]], value), 
                       "percentage" = percentage)

  } else {

    percentage <- paste0(round(sampleSize / planningOptions[["populationSize"]] * 100, 2), "%")

    row <- data.frame("size" = sampleSize, 
                      "percentage" = percentage)
  
  }

  if(options[["selectionMethod"]] != "randomSampling"){

    if(options[["selectionType"]] == "musSampling"){

      row <- cbind(row, 
                   interval = paste(planningOptions[["valuta"]], interval))
    } else {
      row <- cbind(row, 
                  interval = interval)
    }
  }

  selectionInformationTable$addRows(row)
}

.auditSelectionSampleTable <- function(options,
                                       selectionState,
                                       selectionContainer,
                                       jaspResults,
                                       positionInContainer){

  if(!options[["displaySample"]]) 
    return()

  .updateTabNumber(jaspResults)

  if(is.null(selectionContainer[["selectionSampleTable"]])){

    tableTitle <- paste0("<b>Table ", 
                        jaspResults[["tabNumber"]]$object, 
                        ".</b> Selected Observations")
    
    sampleTable <- createJaspTable(tableTitle)
    sampleTable$position <- positionInContainer
    sampleTable$dependOn(options = c("bookValueDescriptives",
                                    "sampleDescriptives",
                                    "displaySample",
                                    "samplingChecked",
                                    "evaluationChecked"))

    columnNames <- c("Row number",
                    "Count")

    recordVariable                  <- unlist(options[["recordNumberVariable"]])
    if(recordVariable == "")        recordVariable <- NULL
    rankingVariable                 <- unlist(options[["rankingVariable"]])
    if(rankingVariable == "")       rankingVariable <- NULL
    monetaryVariable                <- unlist(options[["monetaryVariable"]])
    if(monetaryVariable == "")      monetaryVariable <- NULL
    variables                       <- unlist(options[["additionalVariables"]])

    additionalNames <- c(recordVariable, 
                        monetaryVariable, 
                        rankingVariable, 
                        variables)

    columnNames <- c(columnNames, additionalNames)
    
    for(i in columnNames){

      sampleTable$addColumnInfo(name = i,     
                                type = "string",
                                title = i)

    }

    selectionContainer[["sampleTable"]] <- sampleTable

    if(is.null(selectionState) ||  
      selectionContainer$getError())
        return()
    
    dat <- as.data.frame(selectionState)
    colnames(dat) <- columnNames

    sampleTable$setData(dat)
  }
}

.auditSelectionDescriptivesTable <- function(options, 
                                             selectionState, 
                                             selectionContainer,
                                             jaspResults,
                                             positionInContainer){

  if(!options[["sampleDescriptives"]]) 
    return()
  
  .updateTabNumber(jaspResults)

  if(is.null(selectionContainer[["sampleDescriptivesTable"]])){

    recordVariable <- options[["recordNumberVariable"]]
    if(recordVariable == "")        
      recordVariable <- NULL
    rankingVariable <- options[["rankingVariable"]]
    if(rankingVariable == "")        
      rankingVariable <- NULL
    monetaryVariable <- unlist(options[["monetaryVariable"]])
    if(monetaryVariable == "")      
      monetaryVariable <- NULL

    variables <- unlist(options[["additionalVariables"]])

    all.variables <- c(rankingVariable, 
                        monetaryVariable, 
                        variables)

    tableTitle <- paste0("<b>Table ", 
                      jaspResults[["tabNumber"]]$object, 
                      ".</b> Selection Descriptive Statistics")
    
    sampleDescriptivesTable <- createJaspTable(tableTitle)
    sampleDescriptivesTable$transpose <- TRUE
    sampleDescriptivesTable$position <- positionInContainer

    sampleDescriptivesTable$addFootnote(message = "Not all statistics may be 
                                                    available for <i>Nominal Text</i> 
                                                    variables", 
                                        symbol="<i>Note.</i>")

    sampleDescriptivesTable$dependOn(options = c("sampleDescriptives", 
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

    sampleDescriptivesTable$addColumnInfo(name = "name",                        
                                          type = "string", 
                                          title = "")
    sampleDescriptivesTable$addColumnInfo(name = "Valid cases",                 
                                          type = "integer")
    if (options[["mean"]])               
      sampleDescriptivesTable$addColumnInfo(name = "Mean",
                                            type = "number",
                                            title = "Mean")
    if (options[["median"]])             
      sampleDescriptivesTable$addColumnInfo(name = "Median",
                                            type = "number",
                                            title = "Median")
    if (options[["sd"]])                 
      sampleDescriptivesTable$addColumnInfo(name = "Std. Deviation",
                                            type = "number", 
                                            title = "Std. Deviation")
    if (options[["var"]])                
      sampleDescriptivesTable$addColumnInfo(name = "Variance",                    
                                            type = "number", 
                                            title = "Variance")
    if (options[["range"]])              
      sampleDescriptivesTable$addColumnInfo(name = "Range", 
                                            type = "number", 
                                            title = "Range")
    if (options[["min"]])                
      sampleDescriptivesTable$addColumnInfo(name = "Minimum", 
                                            type = "number",
                                            title = "Minimum")
    if (options[["max"]])                
      sampleDescriptivesTable$addColumnInfo(name = "Maximum", 
                                            type = "number",
                                            title = "Maximum")

    selectionContainer[["sampleDescriptivesTable"]]   <- sampleDescriptivesTable

    if(is.null(selectionState) ||  
      selectionContainer$getError())
        return()

    for (variable in all.variables) {

      column    <- selectionState[[ .v(variable) ]]
      row <- list()

      row[["name"]]                   <- variable
      row[["Valid cases"]]            <- base::length(column)

      if(!is.factor(column))
      {
      if(options[["mean"]])              row[["Mean"]]                   <- base::mean(column, na.rm = TRUE)
      if(options[["sd"]])                row[["Std. Deviation"]]         <- stats::sd(column, na.rm = TRUE)
      if(options[["var"]])               row[["Variance"]]               <- stats::var(column, na.rm = TRUE)
      if(options[["median"]])            row[["Median"]]                 <- stats::median(column, na.rm = TRUE)
      if(options[["range"]])             row[["Range"]]                  <- base::abs(base::range(column, na.rm = TRUE)[1] - base::range(column, na.rm = TRUE)[2])
      if(options[["min"]])               row[["Minimum"]]                <- base::min(column, na.rm = TRUE)
      if(options[["max"]])               row[["Maximum"]]                <- base::max(column, na.rm = TRUE)
      }
      sampleDescriptivesTable$addRows(row)
    }
  }
}

################################################################################
################## Functions for the execution #################################
################################################################################

.auditExecutionStage <- function(options,
                                 jaspResults){

  if(options[["pasteVariables"]]){  

    planningOptions <- .auditPlanningOptions(options,
                                             jaspResults,
                                             rawData = TRUE)
    selectionState <- .auditSelectionState(dataset,
                                           options, 
                                           jaspResults[["planningState"]], 
                                           jaspResults[["selectionContainer"]])
    selectionState <- data.frame(selectionState)
    dataset                       <- .readDataSetToEnd(columns.as.numeric = options[["recordNumberVariable"]])
    sampleFilter                  <- rep(0, planningOptions[["populationSize"]])

    rowNumber                     <- selectionState[["rowNumber"]]
    sampleFilter[rowNumber]       <- selectionState[["count"]]
    sampleFilter                  <- as.numeric(sampleFilter)
    auditDataVariable             <- rep(NA, planningOptions[["populationSize"]])

    auditDataVariable[options[["performAudit"]][[1]]$rowIndices] <- options[["performAudit"]][[1]]$values

    if(is.null(jaspResults[["sampleFilter"]]))  
      jaspResults[["sampleFilter"]] <- createJaspColumn(columnName = options[["sampleFilter"]], 
                                                        dependencies = "sampleFilter")

    if(is.null(jaspResults[["variableName"]]))  
      jaspResults[["variableName"]] <- createJaspColumn(columnName = options[["variableName"]], 
                                                        dependencies = "variableName")

    jaspResults[["sampleFilter"]]$setScale(sampleFilter)
    jaspResults[["variableName"]]$setScale(auditDataVariable)
  }
}

################################################################################
################## Functions for the evaluation ################################
################################################################################

.auditEvaluationGetContainer <- function(jaspResults, 
                                         position){

  selectionContainer <- jaspResults[["selectionContainer"]]
  selectionState <- selectionContainer[["selectionState"]]$object

  if(!is.null(jaspResults[["evaluationContainer"]])){

    return(jaspResults[["evaluationContainer"]])

  } else if(!is.null(selectionState)){
                                         
    evaluationContainer <- createJaspContainer(title = "<u>Evaluation</u>")
    evaluationContainer$position <- position
    evaluationContainer$dependOn(options = c("evaluationChecked",
                                             "auditResult",
                                             "mostLikelyError",
                                             "estimator",
                                             "performAudit",
                                             "stringerBoundLtaAdjustment"))

    jaspResults[["evaluationContainer"]] <- evaluationContainer

    return(evaluationContainer)
  }
}

.auditAddEvaluationColumns <- function(options, 
                                       jaspResults){

  dataset <- .auditAddSelectionColumns(options, 
                                       jaspResults)

  sampleFilter <- options[["sampleFilter"]]
  auditResult <- options[["auditResult"]]

  variables <- c(sampleFilter, auditResult)

  if(!("" %in% variables)){

    additionalColumns <- .readDataSetToEnd(columns.as.numeric = variables)
    dataset <- cbind(dataset, additionalColumns)
    return(dataset)

  } else {

    return(dataset)

  }
}

.auditEvaluationState <- function(options, 
                                  planningOptions,
                                  sample, 
                                  evaluationContainer){

  if(options[["auditResult"]] == "")
    return()
                                  
  if(!is.null(evaluationContainer[["evaluationState"]])){

    return(evaluationContainer[["evaluationState"]]$object)

  } else {

    auditRisk <- 1 - options[["confidence"]]

    if(options[["IR"]] != "Custom"){

      inherentRisk <- base::switch(options[["IR"]], 
                                  "Low" = 0.50, 
                                  "Medium" = 0.60, 
                                  "High" = 1)

    } else {

      inherentRisk <- options[["irCustom"]]
    }

    if(options[["CR"]] != "Custom"){

      controlRisk <- base::switch(options[["CR"]], 
                                  "Low" = 0.50, 
                                  "Medium" = 0.60, 
                                  "High" = 1)

    } else {

      controlRisk <- options[["crCustom"]]

    }

    detectionRisk <- auditRisk / inherentRisk / controlRisk
    confidence <- 1 - detectionRisk

    # Select evaluation method
    if(options[["variableType"]] == "variableTypeCorrect"){

      method <- options[["planningModel"]]

      nSumstats <- nrow(sample)
      kSumstats <- length(which(sample[, .v(options[["auditResult"]])] == 1))

      result <- try({
      
        # call jfa evaluation
        jfa::evaluation(sample = sample,
                        confidence = confidence,
                        nSumstats = nSumstats,
                        kSumstats = kSumstats,
                        method = method,
                        materiality = planningOptions[["materiality"]],
                        N = planningOptions[["populationSize"]])

      })

    } else if(options[["variableType"]] == "variableTypeAuditValues"){

      method <- base::switch(options[["estimator"]],
                              "stringerBound"     = "stringer",
                              "regressionBound"   = "regression",
                              "directBound"       = "direct",
                              "differenceBound"   = "difference",
                              "ratioBound"        = "quotient")

      if(method == "stringer" && options[["stringerBoundLtaAdjustment"]])
        method <- "stringer-lta"

      if(method %in% c("direct", "difference", "quotient", "regression")){
        confidence <- confidence + ((1 - confidence) / 2)
      }

      result <- try({
      
        # call jfa evaluation
        jfa::evaluation(sample = sample,
                        confidence = confidence, # Adjust for arm
                        bookValues = .v(options[["monetaryVariable"]]),
                        auditValues = .v(options[["auditResult"]]),
                        method = method,
                        materiality = planningOptions[["materiality"]],
                        N = planningOptions[["populationSize"]],
                        populationBookValue = planningOptions[["populationValue"]])

      })
    }

    if(isTryError(result)){

      evaluationContainer$setError(paste0("An error occurred: ", 
                                        JASP:::.extractErrorMessage(result)))
      return()
    }

    if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){

      result[["confBound"]] <- (planningOptions[["populationValue"]] - result[["lowerBound"]]) / planningOptions[["populationValue"]]
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

.auditExplanatoryTextEvaluation <- function(options,
                                            planningOptions,
                                            planningState,
                                            evaluationContainer, 
                                            positionInContainer = 1){

  if(options[["explanatoryText"]]){

    ready <- options[["auditResult"]] != ""

    if(ready){

      evaluationState <- evaluationContainer[["evaluationState"]]$object

      if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){

        confidenceBound <- (planningOptions[["populationValue"]] - evaluationState[["lowerBound"]]) / planningOptions[["populationValue"]]

      } else {

        confidenceBound <- evaluationState[["confBound"]]

      }

      errorLabel <- evaluationState[["k"]]

      if(options[["materiality"]] == "materialityRelative"){

        boundLabel <- paste0(round(confidenceBound * 100, 2), "%")

      } else if(options[["materiality"]] == "materialityAbsolute"){

        boundLabel <- paste(planningOptions[["valuta"]], 
                            round(confidenceBound * planningOptions[["populationValue"]], 2))

      }

    } else {

      boundLabel <- "..."
      errorLabel <- "..."

    }

    message <- paste0("The selection consisted of <b>", 
                      planningState[["sampleSize"]] , 
                      "</b> observations, of which <b>", 
                      errorLabel, 
                      "</b> were found to contain an error. The knowledge from these data, com-
                      bined with the risk assessments results in an <b>", 
                      planningOptions[["confidenceLabel"]], 
                      "</b> upper confidence bound of <b>", 
                      boundLabel ,
                      "</b>. The cumulative knowledge states that there
                      is a <b>", 
                      planningOptions[["confidenceLabel"]], 
                      "</b> probability that, when one would repeaditly sample from this population, the maximum misstatement is calculated to be lower
                      than <b>", boundLabel ,"</b>.")

    evaluationContainer[["evaluationParagraph"]] <- createJaspHtml(message, "p")
    evaluationContainer[["evaluationParagraph"]]$position <- positionInContainer
    evaluationContainer[["evaluationParagraph"]]$dependOn(options = "explanatoryText")
  }
}

.auditEvaluationSummaryTable <- function(options,
                                         planningOptions,
                                         evaluationState,
                                         evaluationContainer,
                                         jaspResults,
                                         positionInContainer){

  .updateTabNumber(jaspResults)

  if(!is.null(evaluationContainer[["evaluationTable"]])) 
    return()

  tableTitle <- paste0("<b>Table ", 
                    jaspResults[["tabNumber"]]$object, 
                    ".</b> Evaluation Summary")
  
  evaluationTable <- createJaspTable(tableTitle)
  evaluationTable$position  <- positionInContainer
  evaluationTable$dependOn(options = c("bookValueDescriptives",
                                        "sampleDescriptives",
                                        "displaySample",
                                        "samplingChecked",
                                        "evaluationChecked",
                                        "auditResult"))

  evaluationTable$addColumnInfo(name = 'materiality',   
                                title = "Materiality",
                                type = 'string')
  evaluationTable$addColumnInfo(name = 'sampleSize', 
                                title = "Sample size", 
                                type = 'string')
  evaluationTable$addColumnInfo(name = 'fullErrors',  
                                title = "Errors", 
                                type = 'string')
  evaluationTable$addColumnInfo(name = 'totalTaint',             
                                title = "Total tainting",
                                type = 'string')

  if(options[["mostLikelyError"]])
    evaluationTable$addColumnInfo(name = 'mle',         
                                  title = "MLE", 
                                  type = 'string')

  evaluationTable$addColumnInfo(name = 'bound',         
                                title = paste0(options[["confidence"]] * 100,"% Confidence bound"), 
                                type = 'string')

  if(options[["monetaryVariable"]] != "")
    evaluationTable$addColumnInfo(name = 'projm',         
                              title = "Maximum Misstatement",           
                              type = 'string')

  message <- base::switch(options[["estimator"]],
                          "gammaBound" = "The confidence bound is calculated 
                          according to the <b>Poisson</b> distributon.",
                          "binomialBound" = "The confidence bound is calculated 
                          according to the <b>binomial</b> distributon.",
                          "hyperBound" = "The confidence bound is calculated 
                          according to the <b>hypergeometric</b> distribution.",
                          "stringerBound" = "The confidence bound is calculated 
                          according to the <b>Stringer</b> method.",
                          "regressionBound" = "The confidence bound is calculated 
                          according to the <b>regression</b> method.",
                          "directBound" = "The confidence bound is calculated 
                          according to the <b>direct</b> method.",
                          "differenceBound" = "The confidence bound is calculated 
                          according to the <b>difference</b> method.",
                          "ratioBound" = "The confidence bound is calculated 
                          according to the <b>ratio</b> method.")

  if(options[["estimator"]] == "stringerBound" &&
      options[["stringerBoundLtaAdjustment"]])
  message <- "The confidence bound is calculated according to the <b>Stringer</b>
              method with <b>LTA adjustment</b>."

  evaluationTable$addFootnote(message = message, 
                              symbol="<i>Note.</i>")

  evaluationContainer[["evaluationTable"]] <- evaluationTable

  if(is.null(evaluationState) || 
      options[["auditResult"]] == "")
    return()

  taintLabel <- round(evaluationState[["t"]], 2)
  boundLabel <- paste0(round(evaluationState[["confBound"]] * 100, 3), "%")

  row <- data.frame(materiality = planningOptions[["materialityLabel"]],
                    sampleSize = evaluationState[["n"]],
                    fullErrors = evaluationState[["k"]],
                    totalTaint = taintLabel,
                    bound = boundLabel)

  if(options[["mostLikelyError"]]){

    if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){

      mle <- (planningOptions[["populationValue"]] - evaluationState[["pointEstimate"]]) / planningOptions[["populationValue"]]
    
    } else {

      mle <- evaluationState[["t"]] / evaluationState[["n"]]

    }

    if(options[["materiality"]] == "materialityRelative"){

      mleLabel <- paste0(round(mle * 100, 3), "%")

    } else if(options[["materiality"]] == "materialityAbsolute"){

      mleLabel <- paste(planningOptions[["valuta"]], 
                          round(mle * planningOptions[["populationValue"]], 3))

    }

    row <- cbind(row, 
                 mle = mleLabel)
  }

  if(options[["monetaryVariable"]] != ""){

    projm <- round(evaluationState[["confBound"]] * 
                   planningOptions[["populationValue"]], 2)
    projmLabel <- paste(planningOptions[["valuta"]], projm)
    row <- cbind(row, 
                 projm = projmLabel)

  }
  
  evaluationTable$addRows(row)
}

.auditEvaluationInformationPlot <- function(options,
                                            planningOptions,
                                            evaluationState,
                                            evaluationContainer,
                                            jaspResults,
                                            positionInContainer = 3){

  if(!options[["evaluationInformation"]]) 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(evaluationContainer[["evaluationInformation"]])){

    evaluationInformation <- createJaspPlot(plot = NULL, 
                                            title = "Evaluation Information", 
                                            width = 600, 
                                            height = 300)
    evaluationInformation$position <- positionInContainer
    evaluationInformation$dependOn(options = "evaluationInformation")

    evaluationContainer[["evaluationInformation"]] <- evaluationInformation

    if(options[["auditResult"]] == "" || 
        evaluationContainer$getError()) 
      return()

    materiality <- evaluationState[["materiality"]]
    bound <- evaluationState[["confBound"]]
    mle <- evaluationState[["t"]] / evaluationState[["n"]]
  
    label <- rev(c("Materiality", "Maximum error", "Most likely error"))
    values <- rev(c(materiality, bound, mle))
    
    if(options[["variableType"]] == "variableTypeAuditValues" && 
        options[["materiality"]] == "materialityAbsolute")
      values <- values * planningOptions[["populationValue"]]
    
    boundColor <- ifelse(bound < materiality, 
                        yes = rgb(0,1,.7,1), 
                        no = rgb(1,0,0,1))

    fillUp <- rev(c("#1380A1", boundColor, "#1380A1"))

    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, values), min.n = 4)
    
    if(options[["variableType"]] == "variableTypeAuditValues" && 
        options[["materiality"]] == "materialityAbsolute"){

      x.labels <- format(JASPgraphs::getPrettyAxisBreaks(
                          seq(0, 1.1 * max(values), length.out = 100), 
                          min.n = 4), 
                        scientific = FALSE)
      values.labels <- paste(planningOptions[["valuta"]], ceiling(values))

    } else {

      x.labels <- paste0(round(
                      JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1 * max(values), length.out = 100), 
                      min.n = 4) * 100, 4), 
                    "%")
      values.labels <- paste0(round(values * 100, 2), "%")

    }

    plotData <- data.frame(x = label, 
                          y = values)
    plotData$x <- factor(plotData$x, levels = plotData$x)

    yLimits <- c(0, 1.1 * max(values))
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1 * max(values), length.out = 100), min.n = 4)

    p <- ggplot2::ggplot(data = plotData, 
                        mapping = ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_bar(stat = "identity", 
                          col = "black", 
                          size = 1, 
                          fill = fillUp) +
        ggplot2::coord_flip() +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::annotate("text", 
                          y = values, 
                          x = c(1, 2, 3), 
                          label = values.labels, 
                          size = 6, 
                          vjust = 0.5, 
                          hjust = -0.3) + 
        ggplot2::scale_y_continuous(breaks = yBreaks, 
                                    limits = yLimits, 
                                    labels = x.labels)
    
    myTheme <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), 
                              axis.ticks.y = ggplot2::element_blank(), 
                              axis.text.y = ggplot2::element_text(hjust = 0),
                              panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"))

    p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE) + myTheme

    evaluationInformation$plotObject <- p

  }

  if(options[["explanatoryText"]]){

    evaluationInformationText <- createJaspHtml(paste0("<b>Figure ", 
                                                jaspResults[["figNumber"]]$object,
                                                ".</b> Evaluation information regarding the evaluation of the selection. The materiality is compared with the 
                                                maximum misstatement and the most likely error. The most likely error (MLE) is an estimate of the true misstatement 
                                                in the population. The maximum error is an estimate of the maximum error in the population."), "p")
    
    evaluationInformationText$position <- positionInContainer + 1
    evaluationInformationText$dependOn(optionsFromObject = evaluationContainer[["evaluationInformation"]])
    evaluationInformationText$dependOn(options = "explanatoryText")
    evaluationContainer[["evaluationInformationText"]] <- evaluationInformationText 
  }
}

.auditCorrelationPlotAddLine <- function(fit, 
                                         plot = NULL, 
                                         line = FALSE, 
                                         xMin, 
                                         xMax, 
                                         lwd) {
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
    plot <- plot + ggplot2::geom_line(data = data.frame(x, predY),
                                      mapping = ggplot2::aes(x = x, y = predY), 
                                      size=lwd, 
                                      lty = 1)
    return(plot)
  }
}

.auditCorrelationPlot <- function(options,
                                  planningOptions,
                                  sample,
                                  evaluationContainer,
                                  jaspResults,
                                  positionInContainer){

  if(!options[["correlationPlot"]]) 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(evaluationContainer[["correlationPlot"]])){

    correlationPlot <- createJaspPlot(plot = NULL, 
                                      title = "Correlation Plot", 
                                      width = 500, 
                                      height = 400)

    correlationPlot$position <- positionInContainer
    correlationPlot$dependOn(options = c("correlationPlot", 
                                        "valuta"))

    evaluationContainer[["correlationPlot"]] <- correlationPlot

    if(options[["auditResult"]] == ""|| 
        evaluationContainer$getError()) 
      return()

    plotData <- data.frame(x = sample[,.v(options[["monetaryVariable"]])], 
                          y = sample[,.v(options[["auditResult"]])])

    plotData <- na.omit(plotData)

    corResult <- cor(x = plotData[["x"]], 
                    y = plotData[["y"]], 
                    method = "pearson")

    xVar <- plotData[["x"]]
    yVar <- plotData[["y"]]

    fit <- vector("list", 1)
    fit[[1]] <- lm(y ~ poly(x, 1, raw = TRUE), data = plotData)
    bestModel <- 1 # which.min(Bic)

    # format x labels
    xlow <- min(pretty(xVar))
    xhigh <- max(pretty(xVar))
    xticks <- pretty(c(xlow, xhigh))
    xLabs <- vector("character", length(xticks))
    xLabs <- format(xticks, digits = 3, scientific = FALSE)

    # Format y labels
    yticks <- xticks
    yLabs <- vector("character", length(yticks))
    yLabs <- format(yticks, digits = 3, scientific = FALSE)

    corResult <- round(corResult, 3)

    cols <- rep("gray", nrow(plotData))
    cols[which(plotData$x != plotData$y)] <- "red"

    p <- JASPgraphs::drawAxis(xName = paste0("Book values (", planningOptions[["valuta"]], ")"), 
                              yName = paste0("Audit values (", planningOptions[["valuta"]], ")"), 
                              xBreaks = xticks, 
                              yBreaks = yticks, 
                              yLabels = yLabs, 
                              xLabels = xLabs, 
                              force = TRUE)
    p <- JASPgraphs::drawPoints(p, dat = plotData, size = 3, fill = cols)

    p <- .auditCorrelationPlotAddLine(fit = fit[[bestModel]], 
                                      plot = p, 
                                      line = TRUE, 
                                      xMin= xticks[1], 
                                      xMax = xticks[length(xticks)], 
                                      lwd = 1)

    p <- p + ggplot2::annotate("text", 
                              x = xticks[1], 
                              y = (yticks[length(yticks)] - ((yticks[length(yticks)] - yticks[length(yticks) - 1]) / 2)),
                              label = paste0("italic(r) == ", corResult), 
                              size = 8, 
                              parse = TRUE, 
                              hjust = -0.5, 
                              vjust = 0.5)
    
    myTheme <- ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"), 
                            panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"))
    
    p <- JASPgraphs::themeJasp(p) + myTheme

    correlationPlot$plotObject <- p

  }

  if(options[["explanatoryText"]]){

    correLationPlotText <- createJaspHtml(paste0("<b>Figure ", 
                                          jaspResults[["figNumber"]]$object ,
                                          ".</b> Scatterplot of the book values in the selection and their audit values. Red dots indicate observations that 
                                          did not match their original book value. If these red dots lie in the bottom part of the graph, the book values are overstated. 
                                          If these red dots lie in the upper part of the graph, they are understated. The value <i>r</i> is the Pearson correlation coefficient 
                                          of the book values and the audit values, an indicator of the strengh of the linear relationship between the two variables."), "p")
    
    correLationPlotText$position <- positionInContainer + 1
    correLationPlotText$dependOn(optionsFromObject = evaluationContainer[["correlationPlot"]])
    correLationPlotText$dependOn(options = "explanatoryText")
    evaluationContainer[["correLationPlotText"]] <- correLationPlotText
  }
}

################################################################################
################## Functions for the badges ####################################
################################################################################

.auditBadgeSection <- function(options, 
                               type,
                               stateContainer = NULL,
                               jaspResults, 
                               ready, 
                               position){

  if(is.null(jaspResults[["badgeSection"]]) && 
     ready && 
     options[["reportBadges"]]){

    if(!is.null(stateContainer) && stateContainer$getError())
      return()

    badgeSection <- createJaspContainer(title= "<u>Report Badges</u>")
    badgeSection$position <- position
    badgeSection$dependOn(options = c("values",
                                      "confidence",
                                      "explanatoryText",
                                      "reportBadges",
                                      "IR", 
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
                                      "bookValues", 
                                      "auditValues", 
                                      "estimator",
                                      "digits",
                                      "auditResult",
                                      "stringerBoundLtaAdjustment"))

    # Badge for annotation
    annotatedBadge <- options[["explanatoryText"]]

    if(annotatedBadge){
      
      badge <- .auditCreateBadge(type = "annotated")
      annotationBadge <- createJaspPlot(plot = badge, 
                          title = "Badge: <i>Annotated</i>", 
                          width = 150, 
                          height = 150)

      annotationBadge$position <- 1
      badgeSection[["annotationBadge"]] <- annotationBadge

    }

    if(type == "benfordsLaw"){

      state <- .auditClassicalBenfordsLawState(dataset, 
                                                options, 
                                                stateContainer,
                                                ready)

      observed <- state[["N"]] * state[["percentages"]]
      expected <- state[["N"]] * state[["inBenford"]]
      chiSquare <- sum( (observed - expected)^2 / expected )
      df <- 8
      p <- round(pchisq(q = chiSquare, df = df, lower.tail = FALSE), 4)

      approveBadge <- p >= (1 - options[["confidence"]])

    } else if(type == "workflow"){

      evaluationContainer <- jaspResults[["evaluationContainer"]]
      evaluationState <- evaluationContainer[["evaluationState"]]$object

      if(options[["estimator"]] %in% c("directBound", "differenceBound", "ratioBound", "regressionBound")){

        bound <- (evaluationState[["popBookvalue"]] - evaluationState[["lowerBound"]]) / evaluationState[["popBookvalue"]]

        if(bound < evaluationState[["materiality"]]){

          conclusion <- "Approve population"

        } else {

          conclusion <- "Do not approve population"

        }
      } else {
        conclusion <- evaluationState[["conclusion"]]
      }

      approveBadge <- conclusion == "Approve population"
    }

    if(type %in% c("benfordsLaw", "workflow")){

      # Badge for result
      if(approveBadge){
        plotTitle <- "Badge: <i>Approved</i>"
        plotType <- "approved"
      } else {
        plotTitle <- "Badge: <i>Not approved</i>"
        plotType <- "not approved"
      }

      badge <- .auditCreateBadge(type = plotType)
      resultBadge <- createJaspPlot(plot = badge, 
                                    title = plotTitle, 
                                    width = 150, 
                                    height = 150)

      resultBadge$position <- 2
      badgeSection[["resultBadge"]] <- resultBadge

    }

    jaspResults[["badgeSection"]] <- badgeSection

  }
}

.auditCreateBadge <- function(type){

  center <- 1
  radius <- 1
  h <- radius
  w <- sqrt(3)/2 * radius
  m <- 1.02

  if(type == "approved"){
    fillA <- "#3CB371"
  } else if(type == "not approved"){
    fillA <- "#ff0000"
  } else if(type == "annotated"){
    fillA <- "#57A7E0"
  }

  hexd <- data.frame(x = 1 + c(rep(-sqrt(3)/2, 2), 0, rep(sqrt(3)/2, 2), 0), 
                     y = 1 + c(0.5, -0.5, -1, -0.5, 0.5, 1))
  hexd <- rbind(hexd, hexd[1, ])

  plot <- ggplot2::ggplot() + 
          ggplot2::geom_polygon(mapping = ggplot2::aes_(x = ~x, y = ~y), 
                                data = hexd, size = 1.2, 
                                color = "black", 
                                fill = NA) + 
          ggplot2::geom_polygon(mapping = ggplot2::aes_(x = ~x, y = ~y), 
                                data = hexd,
                                fill = fillA, 
                                color = NA) + 
          ggplot2::coord_fixed() +
          ggplot2::scale_y_continuous(expand = c(0, 0), 
                                      limits = c(center - h * m, center + h * m)) + 
          ggplot2::scale_x_continuous(expand = c(0, 0), 
                                      limits = c(center - w * m, center + w * m)) +
          ggplot2::geom_text(data = data.frame(x = 1, y = 0.08, label = "JASP for Audit"), 
                             mapping = ggplot2::aes(x = x, y = y, label = label), 
                             size = 2, 
                             color = "black",
                             angle = 30, 
                             hjust = 0)

  if(type == "approved"){
    plot <- plot + ggplot2::geom_segment(ggplot2::aes(x = 0.8, 
                                                      xend = 1.6, 
                                                      y = 0.6, 
                                                      yend = 1.4), 
                                        color = "black", 
                                        size = 8) +
                    ggplot2::geom_segment(ggplot2::aes(x = 0.9, 
                                                       xend = 0.55, 
                                                       y = 0.72, 
                                                       yend = 1.07),
                                          color = "black", 
                                          size = 8)
  } else if(type == "not approved"){
    plot <- plot + ggplot2::geom_segment(ggplot2::aes(x = 0.6, 
                                                      xend = 1.4, 
                                                      y = 0.6, 
                                                      yend = 1.4), 
                                         color = "black", 
                                         size = 8) +
                    ggplot2::geom_segment(ggplot2::aes(x = 0.6, 
                                                       xend = 1.4, 
                                                       y = 1.4, 
                                                       yend = 0.6), 
                                          color = "black", 
                                          size = 8)
  } else if(type == "annotated"){
    plot <- plot + ggplot2::geom_segment(ggplot2::aes(x = 1, 
                                                      xend = 1, 
                                                      y = 0.5, 
                                                      yend = 1.1), 
                                         color = "black", 
                                         size = 7,
                                         lineend = "round") +
                    ggplot2::geom_segment(ggplot2::aes(x = 1, 
                                                       xend = 1, 
                                                       y = 1.5, 
                                                       yend = 1.5), 
                                          color = "black", 
                                          size = 7,
                                          lineend = "round")
  }

  myTheme <- ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", 
                                                                     colour = NA), 
                          plot.background = ggplot2::element_rect(fill = "transparent", 
                                                                  colour = NA), 
                          legend.key = ggplot2::element_rect(fill = "transparent", 
                                                             colour = NA), 
                          legend.background = ggplot2::element_rect(fill = "transparent", 
                                                                    colour = NA),
                          plot.margin = ggplot2::margin(b = -0.5, 
                                                        l = 1.5, 
                                                        r = 1.5, 
                                                        unit = "lines"), 
                          strip.text = ggplot2::element_blank(), 
                          line = ggplot2::element_blank(), 
                          text = ggplot2::element_blank(), 
                          title = ggplot2::element_blank())
  
  plot <- plot + myTheme     

  return(plot)
}

################################################################################
################## End functions ###############################################
################################################################################

# The following function will be deprecated
.bookValueDescriptives <- function(dataset, options, jaspResults, position, procedureContainer){

  if(!is.null(procedureContainer[["bookValueDescriptives"]])) return() #The options for this table didn't change so we don't need to rebuild it

  dataTable                                                 <- createJaspTable("Book Value Descriptives")
  dataTable$position                                        <- position
  dataTable$dependOn(options = c("monetaryVariable", "recordNumberVariable", "bookValueDescriptives"))

  dataTable$addColumnInfo(name = 'popSize',     title = "Population size",        type = 'string')
  dataTable$addColumnInfo(name = 'value',       title = "Total value",            type = 'string')
  dataTable$addColumnInfo(name = 'mean',        title = "Mean",                   type = 'string')
  dataTable$addColumnInfo(name = 'sd',          title = "Std. deviation",         type = 'string')
  dataTable$addColumnInfo(name = 'p1',          title = "25%",                    type = 'string', overtitle = "Percentile")
  dataTable$addColumnInfo(name = 'p2',          title = "50%",                    type = 'string', overtitle = "Percentile")
  dataTable$addColumnInfo(name = 'p3',          title = "75%",                    type = 'string', overtitle = "Percentile")

  procedureContainer[["bookValueDescriptives"]]        <- dataTable

  if(options[["monetaryVariable"]] == "" || options[["recordNumberVariable"]] == "")
    return()

  popSize                           <- jaspResults[["N"]]$object
  values                            <- dataset[, .v(options[["monetaryVariable"]])]
  total.value                       <- paste(jaspResults[["valutaTitle"]]$object, round(sum(abs(values)), 2))
  mean.value                        <- paste(jaspResults[["valutaTitle"]]$object, round(mean(values), 2))
  sd.value                          <- paste(jaspResults[["valutaTitle"]]$object, round(sd(values), 2))
  Q                                 <- paste(jaspResults[["valutaTitle"]]$object, round(as.numeric(quantile(values, c(0.25, 0.50, 0.75))), 2))

  row <- data.frame(popSize = popSize, value = total.value, mean = mean.value, sd = sd.value, p1 = Q[1], p2 = Q[2], p3 = Q[3])
  dataTable$addRows(row)
}

# The following function will be deprecated
.bookValueDistribution <- function(dataset, options, jaspResults, position, procedureContainer){

  if(!is.null(procedureContainer[["bookValueDistribution"]])) return()

  bookValuePlot <- createJaspPlot(plot = NULL, title = "Book Value Distribution", width = 600, height = 300)
  bookValuePlot$position <- position
  bookValuePlot$dependOn(options = c("bookValueDistribution", "monetaryVariable", "valuta"))

  procedureContainer[["bookValueDistribution"]] <- bookValuePlot

  if(options[["monetaryVariable"]] == "" || options[["recordNumberVariable"]] == "") return()

  values  <- dataset[, .v(options[["monetaryVariable"]])]
  meanx   <- mean(values)
  sdx     <- sd(values)
  q       <- as.numeric(quantile(values, c(0.25, 0.5, 0.75)))
  minx    <- min(q[1], meanx - sdx)
  maxx    <- max(q[3], meanx + sdx)

  p <- .plotMarginalJfA(values, options[["monetaryVariable"]], jaspResults)

  p <- p + ggplot2::geom_point(ggplot2::aes(x = q[1], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = q[2], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = q[3], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx, y = 0), shape = 21, fill = "red", stroke = 2, size = 5)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx + sdx, y = 0), shape = 21, fill = "dodgerblue1", stroke = 2, size = 4)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx - sdx, y = 0), shape = 21, fill = "dodgerblue1", stroke = 2, size = 4)

  pdata <- data.frame(x = c(0,0,0), y = c(0,0,0), l = c("1","2","3"))
  p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = c(rgb(0,1,0,0))) +
            ggplot2::scale_shape_manual(name = "", values = c(21,21,21), labels = c("Mean", "Mean \u00B1 sd", "Quartile")) +
            ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = c(5, 4, 3), shape = 21, fill = c("red","dodgerblue1", "orange"), stroke = 2, color = "black")), order = 1) +
            ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -10, r = 50))) +
            ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color ="#cbcbcb"))
  
  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  bookValuePlot$plotObject <- p

  if(options[["explanatoryText"]]){
      figure1 <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The distribution of book values in the audit population. The red and blue dots respectively represent the mean
                                                                                        and the values exactly one standard deviation from the mean. The orange dots represent the 25th, 50th (median) and
                                                                                        75th percentile of the book values."), "p")
      figure1$position <- position + 1
      figure1$dependOn(optionsFromObject= bookValuePlot)
      procedureContainer[["figure1"]] <- figure1
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
      jaspResults[["figNumber"]]$dependOn(options = c("bookValueDistribution", "decisionPlot"))
  }
}

# The following function will be deprecated
.plotMarginalJfA <- function(column, variableName, jaspResults, rugs = FALSE, displayDensity = FALSE) {
  column <- as.numeric(column)
  variable <- na.omit(column)

  if(length(variable) == 0)
    return(NULL)

  h <- hist(variable, plot = FALSE)

  if (!displayDensity)
    yhigh <- max(h$counts)
  else {
    dens <- density(variable)
    yhigh <- max(max(h$density), max(dens$y))
  }

  ylow <- 0

  xticks <- base::pretty(c(variable, h$breaks), min.n = 3)

  if (!displayDensity)
    p <-
      JASPgraphs::drawAxis(
        xName = paste0("Book values (", jaspResults[["valutaTitle"]]$object, ")"), yName = "Counts", xBreaks = xticks,
        yBreaks = base::pretty(c(0, h$counts)), force = TRUE, xLabels = xticks
      )
  else
    p <-
      JASPgraphs::drawAxis(
        xName = variableName, yName = "Density", xBreaks = xticks,
        yBreaks = c(0,  1.05 * yhigh), force = TRUE, yLabels = NULL,
        xLabels = xticks
      )

  if (displayDensity)
    p <- p +
      ggplot2::geom_histogram(
        data = data.frame(variable),
        mapping = ggplot2::aes(x = variable, y = ..density..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill = "grey",
        col = "black",
        size = .7,
        center = ((h$breaks[2] - h$breaks[1])/2)
      ) +
      ggplot2::geom_line(
        data = data.frame(x = dens$x, y = dens$y),
        mapping = ggplot2::aes(x = x, y = y),
        lwd = 1,
        col = "black"
      )
  else
    p <- p +
      ggplot2::geom_histogram(
        data     = data.frame(variable),
        mapping  = ggplot2::aes(x = variable, y = ..count..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill     = "grey",
        col      = "black",
        size     = .7,
        center    = ((h$breaks[2] - h$breaks[1])/2)
      )

  # JASP theme
  p <- JASPgraphs::themeJasp(p,
                             axisTickWidth = .7,
                             bty = list(type = "n", ldwX = .7, lwdY = 1))
  # TODO: Fix jaspgraphs axis width X vs Y. See @vandenman.

  if (displayDensity)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(p)
}

# The following function will be deprecated
.evaluationInformation <- function(options, evaluationResult, jaspResults, position, evaluationContainer){

  if(!is.null(evaluationContainer[["evaluationInformation"]])) return()

  evaluationInformation <- createJaspPlot(plot = NULL, title = "Evaluation Information", width = 600, height = 300)
  evaluationInformation$position <- position
  evaluationInformation$dependOn(options = c("IR", "CR", "confidence", "auditResult", "evaluationInformation", "materialityPercentage", "estimator", "materialityValue", "valuta", "performAudit"))

  evaluationContainer[["evaluationInformation"]] <- evaluationInformation

  if(!jaspResults[["runEvaluation"]]$object) return()

  materiality       <- jaspResults[["materiality"]]$object
  bound             <- evaluationResult[["bound"]]
  proj.misstatement <- bound * jaspResults[["total_data_value"]]$object
  if(options[["variableType"]] == "variableTypeCorrect"){
    if(options[["estimator"]] == "gammaBound" || options[["estimator"]] == "binomialBound" || options[["estimator"]] == "hyperBound"){
      mle <- evaluationResult[["k"]] / evaluationResult[["n"]]
    } else {
      mle <- (evaluationResult[["posteriorA"]] - 1) / (evaluationResult[["posteriorA"]] + evaluationResult[["posteriorB"]] - 2)
    }
  } else {
    if(options[["estimator"]] == "stringerBound"){
      mle <- sum(evaluationResult[["z"]]) / evaluationResult[["n"]]
    } else if(options[["estimator"]] == "coxAndSnellBound") {
      mle <- evaluationResult[["mf"]] * ( (evaluationResult[["df1"]] - 2)  / evaluationResult[["df1"]] ) * ( evaluationResult[["df2"]] / (evaluationResult[["df2"]] + 2) ) 
    } else {
      mle <- abs(evaluationResult[["mleTable"]])
    }
  }
 
  label             <- rev(c("Materiality", "Maximum error", "Most likely error"))
  values            <- rev(c(materiality, bound, mle))
  
  if(options[["variableType"]] == "variableTypeAuditValues" && options[["materiality"]] == "materialityAbsolute")
    values          <- values * jaspResults[["total_data_value"]]$object
  
  boundColor        <- ifelse(bound < materiality, yes = rgb(0,1,.7,1), no = rgb(1,0,0,1))
  fillUp            <- rev(c("#1380A1", boundColor, "#1380A1"))
  yBreaks           <- as.numeric(JASPgraphs::getPrettyAxisBreaks(c(0, values), min.n = 4))
  
  if(options[["variableType"]] == "variableTypeAuditValues" && options[["materiality"]] == "materialityAbsolute"){
    x.labels        <- format(JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1*max(values), length.out = 100), min.n = 4), scientific = FALSE)
    values.labels   <- paste(jaspResults[["valutaTitle"]]$object, ceiling(values))
    x.title         <- ""
  } else {
    x.labels        <- paste0(round(JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1*max(values), length.out = 100), min.n = 4) * 100, 4), "%")
    values.labels   <- paste0(round(values * 100, 2), "%")
    x.title         <- ""
  }

  tb                <- data.frame(x = label, values = values)
  tb$x              <- factor(tb$x, levels = tb$x)
  p                 <- ggplot2::ggplot(data = data.frame(x = tb[, 1], y = tb[, 2]), ggplot2::aes(x = x, y = y)) +
                        ggplot2::geom_bar(stat = "identity", col = "black", size = 1, fill = fillUp) +
                        ggplot2::coord_flip() +
                        ggplot2::xlab(NULL) +
                        ggplot2::ylab(x.title) +
                        ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(hjust = 0)) +
                        ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"))+
                        ggplot2::annotate("text", y = values, x = c(1, 2, 3), label = values.labels, size = 6, vjust = 0.5, hjust = -0.3) + 
                        ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1*max(values), length.out = 100), min.n = 4), limits = c(0, 1.1*max(values)), labels = x.labels)
  p                 <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)

  evaluationInformation$plotObject <- p

  if(options[["explanatoryText"]]){
      figure4 <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Evaluation information regarding the evaluation of the selection. The materiality is compared with the 
                                        maximum misstatement and the most likely error. The most likely error (MLE) is an estimate of the true misstatement 
                                        in the population. The maximum error is an estimate of the maximum error in the population."), "p")
      figure4$position <- position + 1
      figure4$dependOn(optionsFromObject = evaluationInformation)
      evaluationContainer[["figure4"]] <- figure4
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  }
}

# The following function will be deprecated
.correlationPlot <- function(dataset, options, jaspResults, position, evaluationContainer) {

  if(!is.null(evaluationContainer[["correlationPlot"]])) return()

  correlationPlot <- createJaspPlot(plot = NULL, title = "Correlation Plot", width = 500, height = 400)
  correlationPlot$position <- position
  correlationPlot$dependOn(options = c("auditResult", "correlationPlot", "monetaryVariable", "valuta", "performAudit"))

  evaluationContainer[["correlationPlot"]] <- correlationPlot

  if(!jaspResults[["runEvaluation"]]$object) return()

  d <- data.frame(xx= dataset[,.v(options[["monetaryVariable"]])], yy= dataset[,.v(options[["auditResult"]])])
  co <- cor(d$xx, d$yy, method = "pearson")
  d <- na.omit(d)
  d <- ceiling(d)
  xVar <- d$xx
  yVar <- d$yy

  fit <- vector("list", 1)# vector("list", 4)
  fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), data = d)

  bestModel <- 1 # which.min(Bic)

  # format x labels
  xlow <- min(pretty(xVar))
  xhigh <- max(pretty(xVar))
  xticks <- pretty(c(xlow, xhigh))
  xLabs <- vector("character", length(xticks))
  xLabs <- format(xticks, digits= 3, scientific = FALSE)

  # Format y labels
  yticks <- xticks
  yLabs <- vector("character", length(yticks))
  yLabs <- format(yticks, digits= 3, scientific = FALSE)

  co <- round(co, 3)

  cols <- rep("gray", nrow(d))
  cols[which(d$xx != d$yy)] <- "red"

  p <- JASPgraphs::drawAxis(xName = paste0("Book values (", jaspResults[["valutaTitle"]]$object, ")"), yName = paste0("Audit values (", jaspResults[["valutaTitle"]]$object, ")"), xBreaks = xticks, yBreaks = yticks, yLabels = yLabs, xLabels = xLabs, force = TRUE)
  p <- JASPgraphs::drawPoints(p, dat = d, size = 3, fill = cols)
  p <- .poly.predJfA(fit[[bestModel]], plot = p, line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd = 1)
  p <- p + ggplot2::annotate("text", x = xticks[1], y = (yticks[length(yticks)] - ((yticks[length(yticks)] - yticks[length(yticks) - 1]) / 2)),
                              label = paste0("italic(r) == ", co), size = 8, parse = TRUE, hjust = -0.5, vjust = 0.5)
  p <- p + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"), panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"))
  
  p <- JASPgraphs::themeJasp(p)

  correlationPlot$plotObject <- p

  if(options[["explanatoryText"]]){
      figure6 <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Scatterplot of the book values in the selection and their audit values. Red dots indicate observations that 
                                        did not match their original book value. If these red dots lie in the bottom part of the graph, the book values are overstated. 
                                        If these red dots lie in the upper part of the graph, they are understated. The value <i>r</i> is the Pearson correlation coefficient 
                                        of the book values and the audit values, an indicator of the strengh of the linear relationship between the two variables."), "p")
      figure6$position <- position + 1
      figure6$dependOn(optionsFromObject = correlationPlot)
      evaluationContainer[["figure6"]] <- figure6
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  }
}

# The following function will be deprecated
.readDataProcedure <- function(options, jaspResults){
  
  recordNumberVariable                    <- options[["recordNumberVariable"]]
  if(recordNumberVariable == "")          recordNumberVariable <- NULL 
  monetaryVariable                        <- options[["monetaryVariable"]]
  if(monetaryVariable == "")              monetaryVariable <- NULL 
  
  if(!is.null(recordNumberVariable)){
    variables                             <- recordNumberVariable
    if(!is.null(monetaryVariable)){
      variables <- c(variables, monetaryVariable)
      dataset <- .readDataSetToEnd(columns.as.numeric = variables)
      jaspResults[["N"]]                  <- createJaspState(nrow(dataset))
      jaspResults[["uniqueN"]]            <- createJaspState(length(unique(dataset[, .v(options[["recordNumberVariable"]])])))
      jaspResults[["total_data_value"]]   <- createJaspState( ceiling(sum(dataset[, .v(monetaryVariable)])))
      jaspResults[["ready"]]              <- createJaspState(TRUE) # Ready for analysis
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = variables)
      jaspResults[["N"]]                  <- createJaspState(nrow(dataset))
      jaspResults[["uniqueN"]]            <- createJaspState(length(unique(dataset[, .v(options[["recordNumberVariable"]])])))
      jaspResults[["total_data_value"]]   <- createJaspState(0.01)
      if(options[["materiality"]] == "materialityRelative"){
        jaspResults[["ready"]]            <- createJaspState(TRUE) # Ready for analysis
      } else {
        jaspResults[["ready"]]            <- createJaspState(FALSE) # Ready for analysis
      }
    }
  } else {
      dataset                             <- NULL
      jaspResults[["N"]]                  <- createJaspState(0)
      jaspResults[["uniqueN"]]            <- createJaspState(0)
      jaspResults[["total_data_value"]]   <- createJaspState(0.01)
      jaspResults[["ready"]]              <- createJaspState(FALSE)
  }
  materialityReady <- ifelse(options[["materiality"]] == "materialityRelative", yes = options[["materialityPercentage"]], no = options[["materialityValue"]])
  if(materialityReady == 0)
    jaspResults[["ready"]]              <- createJaspState(FALSE)

  jaspResults[["N"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable"))
  jaspResults[["uniqueN"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable"))
  jaspResults[["total_data_value"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable"))
  jaspResults[["ready"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable", "materiality"))
  return(dataset)
}

# This function will be deprecated
.readDataSelection <- function(options){
  recordVariable                  <- unlist(options[["recordNumberVariable"]])
  if(recordVariable == "")        recordVariable <- NULL
  rankingVariable                 <- unlist(options[["rankingVariable"]])
  if(rankingVariable == "")       rankingVariable <- NULL
  monetaryVariable                <- unlist(options[["monetaryVariable"]])
  if(monetaryVariable == "")      monetaryVariable <- NULL
  variables                       <- unlist(options[["additionalVariables"]])
  variables.to.read               <- c(recordVariable, variables, rankingVariable, monetaryVariable)
  dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)
  return(dataset)
}

# The following function will be deprecated
.execution <- function(options, jaspResults){
  if(options[["pasteVariables"]]){  
    dataset                       <- .readDataSetToEnd(columns.as.numeric = options[["recordNumberVariable"]])
    sampleFilter                  <- rep(0, jaspResults[["N"]]$object)
    rowNumber                     <- which(dataset[, .v(options[["recordNumberVariable"]])] %in% jaspResults[["sample"]]$object[, .v(options[["recordNumberVariable"]])])
    noOfTimesInSample             <- table(jaspResults[["sampleVector"]]$object)
    sampleFilter[rowNumber]       <- 1 * noOfTimesInSample
    sampleFilter                  <- as.numeric(sampleFilter)
    auditDataVariable             <- rep(NA, jaspResults[["N"]]$object)

    auditDataVariable[options[["performAudit"]][[1]]$rowIndices] <- options[["performAudit"]][[1]]$values

    if(is.null(jaspResults[["sampleFilter"]]))  jaspResults[["sampleFilter"]] <- createJaspColumn(columnName=options[["sampleFilter"]], dependencies="sampleFilter")
    if(is.null(jaspResults[["variableName"]]))  jaspResults[["variableName"]] <- createJaspColumn(columnName=options[["variableName"]], dependencies="variableName")

    jaspResults[["sampleFilter"]]$setScale(sampleFilter)
    jaspResults[["variableName"]]$setScale(auditDataVariable)
  }
}

# The following function will be deprecated
.readDataEvaluation <- function(options, jaspResults){
  recordVariable                  <- unlist(options[["recordNumberVariable"]])
  if(recordVariable == "")        recordVariable <- NULL
  monetaryVariable                <- unlist(options[["monetaryVariable"]])
  if(monetaryVariable == "")      monetaryVariable <- NULL
  sampleFilter                    <- unlist(options[["sampleFilter"]])
  if(sampleFilter == "")          sampleFilter <- NULL
  auditResult                     <- unlist(options[["auditResult"]])
  if(auditResult == "")           auditResult <- NULL
  variables.to.read               <- c(recordVariable, auditResult, sampleFilter, monetaryVariable)
  dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

  jaspResults[["runEvaluation"]] <- createJaspState( (!is.null(auditResult) && !is.null(sampleFilter)) )
  jaspResults[["runEvaluation"]]$dependOn(options = c("auditResult", "sampleFilter", "performAudit"))
  return(dataset)
}

# The following function will be deprecated
.errorHandlingProcedure <- function(options, dataset){
  variables <- NULL
  if(options[["recordNumberVariable"]] != "")
    variables <- c(variables, options[["recordNumberVariable"]])
  if(options[["monetaryVariable"]] != "")
    variables <- c(variables, options[["monetaryVariable"]])
  n <- nrow(dataset)

    .hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
            all.target = variables, message="short", observations.amount= paste0("< ", n),
            exitAnalysisIfErrors = TRUE)
}

# The following function will be deprecated
.decisionAnalysis <- function(options, jaspResults, position, planningContainer, type){

  if(!is.null(planningContainer[["decisionPlot"]])) return()

  decisionPlot <- createJaspPlot(plot = NULL, title = "Decision Analysis", width = 600, height = 300)
  decisionPlot$position <- position
  decisionPlot$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", "decisionPlot", "materialityValue", "explanatoryText"))

  planningContainer[["decisionPlot"]] <- decisionPlot

  if(!jaspResults[["ready"]]$object || planningContainer$getError()) return()

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr

  if(type == "frequentist"){

      n <- c(.calc.n.poisson(options, alpha, jaspResults),
          .calc.n.binomial(options, alpha, jaspResults),
          .calc.n.hypergeometric(options, alpha, jaspResults))

      kpois <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(options[["expectedPercentage"]] * n[1], 2), "expectedAbsolute" = round(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n[1], 2))
      kbinom <- base::switch(options[["expectedErrors"]], "expectedRelative" = ceiling(options[["expectedPercentage"]] * n[2]), "expectedAbsolute" = ceiling(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n[2]))
      khyper <- base::switch(options[["expectedErrors"]], "expectedRelative" = ceiling(options[["expectedPercentage"]] * n[3]), "expectedAbsolute" = ceiling(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n[3]))

      k <- c(round(kpois, 2), kbinom, khyper)

      d <- data.frame(y = c(n, k), 
                      dist = rep(c("Poisson", "Binomial", "Hypergeometric"), 2),
                      nature = rep(c("Expected error-free", "Expected errors"), each = 3))
      d$dist = factor(d$dist,levels(d$dist)[c(2,1,3)])
      d$nature = factor(d$nature,levels(d$nature)[c(1,2)])
      
      p <- ggplot2::ggplot(data = d, ggplot2::aes(x = dist, y = y, fill = nature)) +
          ggplot2::geom_bar(stat = "identity", col = "black", size = 1) +
          ggplot2::coord_flip() +
          ggplot2::xlab("") +
          ggplot2::ylab("Required sample size") +
          ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(hjust = 0)) +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb")) +
          ggplot2::labs(fill = "") +
          ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE)) +
          ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30))) +
          ggplot2::annotate("text", y = k, x = c(3, 2, 1), label = k, size = 6, vjust = 0.5, hjust = -0.3) + 
          ggplot2::annotate("text", y = n, x = c(3, 2, 1), label = n, size = 6, vjust = 0.5, hjust = -0.5) + 
          ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(0:(ceiling(1.1*max(n))), min.n = 4), limits = c(0, ceiling(1.1*max(n)))) +
          ggplot2::ylim(0, ceiling(1.2*max(n)))
      p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")

      optN <- base::switch(which.min(n), "1" = "Poisson", "2" = "binomial", "3" = "hypergeometric")
      jaspResults[["mostEfficientPlanningDistribution"]] <- createJaspState(optN)
      jaspResults[["mostEfficientPlanningDistribution"]]$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", 
                                                                          "decisionPlot", "materialityValue"))

  } else if(type == "bayesian"){

      n <- c(.calc.n.beta(options, alpha, jaspResults), .calc.n.betabinom(options, alpha, jaspResults))
      k <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(options[["expectedPercentage"]] * n, 2), "expectedAbsolute" = round(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n, 2))
      
      d <- data.frame(y = c(n, k), 
                      dist = rep(c("Beta", "Beta-binomial"), 2),
                      nature = rep(c("Expected error-free", "Expected errors"), each = 2))
      d$dist = factor(d$dist,levels(d$dist)[c(2,1)])
      d$nature = factor(d$nature,levels(d$nature)[c(1,2)])
      
      p <- ggplot2::ggplot(data = d, ggplot2::aes(x = dist, y = y, fill = nature)) +
          ggplot2::geom_bar(stat = "identity", col = "black", size = 1) +
          ggplot2::coord_flip() +
          ggplot2::xlab("") +
          ggplot2::ylab("Required sample size") +
          ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(hjust = 0)) +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb")) +
          ggplot2::labs(fill = "") +
          ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE)) +
          ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30))) +
          ggplot2::annotate("text", y = k, x = c(2, 1), label = k, size = 6, vjust = 0.5, hjust = -0.3) + 
          ggplot2::annotate("text", y = n, x = c(2, 1), label = n, size = 6, vjust = 0.5, hjust = -0.5) + 
          ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(0:(ceiling(1.1*max(n))), min.n = 4), limits = c(0, ceiling(1.1*max(n)))) +
          ggplot2::ylim(0, ceiling(1.2*max(n)))
      p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")

      optN <- base::switch(which.min(n), "1" = "beta", "2" = "beta-binomial")
      jaspResults[["mostEfficientPlanningDistribution"]] <- createJaspState(optN)
      jaspResults[["mostEfficientPlanningDistribution"]]$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", 
                                                                            "decisionPlot", "materialityValue"))
  }

  decisionPlot$plotObject <- p

  if(options[["explanatoryText"]]){
        figure2 <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Decision analysis for the current options. The bars represent the sample size that is required under different planning distributions.
                                                                                    The the number of expected errors in the selection is colored in red and the number of expected error-free observations is colored in green. 
                                                                                    The most efficient distribution for these options is the <b>", jaspResults[["mostEfficientPlanningDistribution"]]$object ,"</b> distribution."), "p")
        figure2$position <- position + 1
        figure2$dependOn(optionsFromObject = decisionPlot)
        planningContainer[["figure2"]] <- figure2
        jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  }
}

# The following function will be deprecated
.auditRiskModel <- function(options, jaspResults, position){

  if(!is.null(jaspResults[["ARMcontainer"]])) return()

  ARMcontainer <- createJaspContainer(title= "<u>Audit Risk Model</u>")
  ARMcontainer$position <- position
  ARMcontainer$dependOn(options = c("confidence", "IR", "CR", "materialityPercentage", "materialityValue", "materiality", "explanatoryText", "valuta", "irCustom", "crCustom"))

  #  Audit Risk Model formula
  .ARMformula(options, jaspResults, position = 2, ARMcontainer)
  DR <- jaspResults[["DR"]]$object
  
  if(!is.null(ARMcontainer[["AuditRiskModelParagraph"]])){
    return()
  } else {
    if(options[["explanatoryText"]]){
      materialityLevelLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]], 10) * 100, "%"), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
      auditRiskLabel        <- paste0(round((1 - options[["confidence"]]) * 100, 2), "%")
      dectectionRiskLabel   <- paste0(round(DR * 100, 2), "%")

      message <- paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>", options[["IR"]] ,"</b>. The internal control risk was determined
                                                                      to be <b>", options[["CR"]] ,"</b>. According to the Audit Risk Model, the required detection risk to maintain an audit risk of <b>", auditRiskLabel, "</b> for a materiality
                                                                      of <b>", materialityLevelLabel ,"</b> should be <b>", dectectionRiskLabel , "</b>.")
      if(options[["IR"]] == "Custom" || options[["CR"]] == "Custom"){
        message <- paste0(message, " The translation of High, Medium and Low to probabilities is done according custom values</b>.")
      } else {
        message <- paste0(message, " The translation of High, Medium and Low to probabilities is done according to <b>IODAD (2007)</b>.")
      }
      ARMcontainer[["AuditRiskModelParagraph"]] <- createJaspHtml(message, "p")
      ARMcontainer[["AuditRiskModelParagraph"]]$position <- 1
      ARMcontainer[["AuditRiskModelParagraph"]]$dependOn(options = c("confidence", "IR", "CR", "materialityPercentage", "materialityValue", "valuta"))
    }
  }
  jaspResults[["ARMcontainer"]] <- ARMcontainer
  return(DR)
}

# The following function will be deprecated
.ARMformula <- function(options, jaspResults, position = 2, ARMcontainer){

    if(!is.null(ARMcontainer[["ARMformula"]])) return()

    AR                      <- 1 - options[["confidence"]]

    if(options[["IR"]] != "Custom"){
      IR <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    } else {
      IR <- options[["irCustom"]]
    }

    if(options[["CR"]] != "Custom"){
      CR <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    } else {
      CR <- options[["crCustom"]]
    }

    DR                      <- AR / IR / CR

    jaspResults[["DR"]]     <- createJaspState(DR)
    jaspResults[["DR"]]     $dependOn(options = c("IR", "CR", "confidence", "irCustom", "crCustom"))

    text <- paste0("Audit risk (", round(AR * 100, 2),"%) = Inherent risk (", round(IR * 100, 2), "%) x Control risk (", round(CR * 100, 2), "%) x Detection risk (", round(DR * 100, 2), "%)")

    ARMcontainer[["ARMformula"]] <- createJaspHtml(text, "h3")
    ARMcontainer[["ARMformula"]]$position <- position
    ARMcontainer[["ARMformula"]]$dependOn(options = c("IR", "CR", "confidence", "irCustom", "crCustom"))
}