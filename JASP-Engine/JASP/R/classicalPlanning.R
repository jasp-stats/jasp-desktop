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

classicalPlanning <- function(jaspResults, dataset, options, ...){

  # Set the nessecary options before the analysis
  planningOptions <- .classicalPlanningOptions(options)

  # Explanatory text for the procedure paragraph
  .classicalPlanningExplanatoryTextProcedure(options, planningOptions, jaspResults)

  # Explanatory text for the audit risk model paragraph
  .auditRiskModel(options, jaspResults)

  # Check if analysis can be run
  ready <- .classicalPlanningReady(options, planningOptions)

  # Create the planning container
  planningContainer <- createJaspContainer(title= "<u>Planning</u>")
  planningContainer$position <- 3
  jaspResults[["planningContainer"]] <- planningContainer

  if(ready){
    # Perform early error handling
    if(options[["materiality"]] == "materialityAbsolute" && options[["materialityValue"]] >= planningOptions[["populationValue"]])
     planningContainer$setError("Analysis not possible: Your materiality is higher than the total value of the observations.") 

    expTMP <- ifelse(options[['expectedErrors']] == "expectedRelative", yes = options[["expectedPercentage"]], no = options[["expectedNumber"]] / planningOptions[["populationValue"]])
    if(expTMP > planningOptions[["materiality"]]){
      planningContainer$setError("Analysis not possible: Your expected errors are higher than materiality.")
    }
  }

  # Get the planningResult object
  planningState <- .classicalPlanningState(options, planningOptions, planningContainer, ready)
  
  # Fill the planning summary table
  .classicalPlanningResultTable(options, planningOptions, planningState, planningContainer, ready)

  # Explanatory text for the planning paragraph
  .classicalPlanningExplanatoryTextPlanning(options, planningOptions, planningState, planningContainer, ready)
  
  # Create an index for figure numbers
  planningContainer[["figNumber"]] <- createJaspState(1)

  # Create the implied sampling distribution plot
  .samplingDistributionPlot(options, planningOptions, planningState, planningContainer, position = 4, ready)

  # Create the decision analysis plot
  .decisionAnalysisPlot(options, planningOptions, planningState, planningContainer, type = "frequentist", position = 5, ready)
}

.classicalPlanningOptions <- function(options){
  
  valuta <- base::switch(options[["valuta"]], "euroValuta" = "\u20AC", "dollarValuta" = "\u0024", "otherValuta" = options[["otherValutaName"]])
  
  confidence <- options[["confidence"]]
  confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")
  absRel <- base::switch(options[["materiality"]], "materialityRelative" = "<b>percentage</b>", "materialityAbsolute" = "<b>amount</b>")
  
  populationSize <- options[["populationSize"]]
  populationValue <- ifelse(options[["populationValue"]] == 0, yes = 0.01, no = options[["populationValue"]])
  
  materiality <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = options[["materialityValue"]] / populationValue, no = options[["materialityPercentage"]])
  materialityLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]] * 100, 2), "%"), "materialityAbsolute" = paste(valuta, format(options[["materialityValue"]], scientific = FALSE)))
  
  expectedErrors <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = options[["expectedPercentage"]], no = options[["expectedNumber"]] / options[["populationValue"]])
  expectedErrorsLabel <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = paste0(round(options[["expectedPercentage"]] * 100, 2), "%"), no = paste(valuta, options[["expectedNumber"]]))
  
  likelihood <- base::switch(options[["planningModel"]], "Poisson" = "poisson", "binomial" = "binomial", "hypergeometric" = "hypergeometric")
  
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

.classicalPlanningExplanatoryTextProcedure <- function(options, planningOptions, jaspResults){
  if(options[["explanatoryText"]] && is.null(jaspResults[["procedureContainer"]])){
    procedureContainer <- createJaspContainer(title= "<u>Procedure</u>")
    procedureContainer$position <- 1
    procedureContainer[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", planningOptions[["confidenceLabel"]], ")</b> whether the ", planningOptions[["absRel"]] ," of
                                                                          misstatement in the target population is lower than the specified materiality of <b>", planningOptions[["materialityLabel"]], "</b>."), "p")
    procedureContainer[["procedureParagraph"]]$position <- 1
    procedureContainer$dependOn(options = c("explanatoryText", "confidence", "materiality", "materialityValue", "materialityPercentage", "valuta"))
    jaspResults[["procedureContainer"]] <- procedureContainer
  }
}

.classicalPlanningReady <- function(options, planningOptions){
  if(options[["materiality"]] == "materialityAbsolute"){
    ready <- options[["materialityValue"]] != 0 && planningOptions[["populationSize"]] != 0 && planningOptions[["populationValue"]] != 0
  } else {
    ready <- options[["materialityPercentage"]] != 0 && planningOptions[["populationSize"]] != 0
  }
  return(ready)
}

.classicalPlanningState <- function(options, planningOptions, planningContainer, ready){
  if(!is.null(planningContainer[["planningState"]])){
    return(planningContainer[["planningState"]]$object)
  } else if(ready){
    auditRisk <- 1 - options[["confidence"]]
    if(options[["IR"]] != "Custom"){
      inherentRisk <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    } else {
      inherentRisk <- options[["irCustom"]]
    }
    if(options[["CR"]] != "Custom"){
      controlRisk <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    } else {
      controlRisk <- options[["crCustom"]]
    }
    detectionRisk <- auditRisk / inherentRisk / controlRisk
    adjustedConfidence <- 1 - detectionRisk
    result <- try({
      jfa::planning(materiality = planningOptions[["materiality"]], confidence = adjustedConfidence, 
                    expectedError = planningOptions[["expectedErrors"]], likelihood = planningOptions[["likelihood"]], 
                    N = planningOptions[["populationSize"]])
      })
    if(isTryError(result)){
      planningContainer$setError(paste0("An error occurred: ", JASP:::.extractErrorMessage(result)))
      return()
    }
    planningContainer[["planningState"]] <- createJaspState(result)
    planningContainer[["planningState"]]$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "populationSize", "expectedPercentage", "expectedNumber",
                                                              "planningModel", "materialityValue", "populationValue", "materiality", "irCustom", "crCustom"))
    return(result)
  } else {
    return(list())
  }
}

.classicalPlanningResultTable <- function(options, planningOptions, planningState, planningContainer, ready){

  summaryTable <- createJaspTable("Planning Summary")
  summaryTable$position <- 2
  summaryTable$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "populationSize", "expectedPercentage", "expectedNumber", "expectedBF",
                                  "planningModel", "materialityValue", "populationValue", "materiality", "valuta"))

  summaryTable$addColumnInfo(name = 'materiality',          title = "Materiality",          type = 'string')
  summaryTable$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  summaryTable$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  summaryTable$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  summaryTable$addColumnInfo(name = 'k',                    title = "Expected errors",       type = 'string')
  summaryTable$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')

  planningContainer[["summaryTable"]] <- summaryTable

  auditRisk <- 1 - options[["confidence"]]
  if(options[["IR"]] != "Custom"){
    inherentRisk <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  } else {
    inherentRisk <- options[["irCustom"]]
  }
  if(options[["CR"]] != "Custom"){
    controlRisk <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  } else {
    controlRisk <- options[["crCustom"]]
  }
  detectionRisk <- auditRisk / inherentRisk / controlRisk

  if(!ready || planningContainer$getError()){

    message <- base::switch(options[["planningModel"]],
                        "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution."),
                        "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution."),
                        "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution."))
    summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

    row <- data.frame(materiality = ".", IR = paste0(round(inherentRisk * 100, 2), "%"), CR = paste0(round(controlRisk * 100, 2), "%"), DR = paste0(round(detectionRisk * 100, 2), "%"), k = ".", n = ".")
    summaryTable$addRows(row)
    summaryTable$addFootnote(message = "The materiality is defined as zero.", symbol="<b>Analysis not ready.</b>")
    return()
  }

  message <- base::switch(options[["planningModel"]],
                        "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution <i>(\u03BB = ", round(planningState[["materiality"]] * planningState[["sampleSize"]], 4) , ")</i>."),
                        "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution <i>(p = ", round(planningState[["materiality"]], 2) ,")</i>."),
                        "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution <i>(N = ", planningState[["N"]] ,", K = ", ceiling(planningState[["N"]] * planningState[["materiality"]]) ,")</i>."))
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

  k <- base::switch(options[["expectedErrors"]], "expectedRelative" = planningState[["expectedSampleError"]], "expectedAbsolute" = paste(planningOptions[["valuta"]], options[["expectedNumber"]]))
  n <- planningState[["sampleSize"]]

  materialityTitle <- paste0(round(planningState[["materiality"]] * 100, 2), "%")
  materialityValue <- base::switch(options[["materiality"]], "materialityRelative" = ceiling(planningState[["materiality"]] * options[["populationValue"]]), "materialityAbsolute" = options[["materialityValue"]])
  materiality <- base::switch(options[["materiality"]], "materialityRelative" = materialityTitle, "materialityAbsolute" = paste(planningOptions[["valuta"]], materialityValue))

  row <- data.frame(materiality = materiality, IR = paste0(round(inherentRisk * 100, 2), "%"), CR = paste0(round(controlRisk * 100, 2), "%"), DR = paste0(round(detectionRisk * 100, 2), "%"), k = k, n = n)
  summaryTable$addRows(row)
}

.classicalPlanningExplanatoryTextPlanning <- function(options, planningOptions, planningState, planningContainer, ready){
  if(options[["explanatoryText"]] && is.null(planningContainer[["planningParagraph"]]) && !planningContainer$getError()){
    planningContainer[["planningParagraph"]] <- createJaspHtml(paste0("The most likely error in the data was expected to be <b>", planningOptions[["expectedErrorsLabel"]] ,"</b>. The sample size that is required to prove a materiality of <b>", planningOptions[["materialityLabel"]] ,"</b>, assuming
                                                                                              the sample contains <b>", planningOptions[["expectedErrorsLabel"]] ,"</b> full errors, is <b>", planningState[["sampleSize"]] ,"</b>. This sample size is based on the <b>", options[["planningModel"]] , "</b> distribution, the inherent risk <b>(", options[["IR"]] , ")</b>, the
                                                                                              control risk <b>(", options[["CR"]] , ")</b> and the expected errors. Consequently, if the sum of errors from the audited observations stays below <b>", planningOptions[["expectedErrorsLabel"]] ,"</b>, the
                                                                                              maximum misstatement is estimated to be below materiality and the population can be approved."), "p")
    planningContainer[["planningParagraph"]]$position <- 1
    planningContainer[["planningParagraph"]]$dependOn(options = c("expectedPercentage", "expectedErrors", "expectedNumber", "planningModel", "IR", "CR", "materialityPercentage", "confidence", "materialityValue"))
  }
}

.samplingDistributionPlot <- function(options, planningOptions, planningState, planningContainer, position, ready){

  if(!is.null(planningContainer[["samplingDistribution"]]) || !options[["samplingDistribution"]]) return()

  samplingDistribution <- createJaspPlot(plot = NULL, title = "Implied Sampling Distribution", width = 600, height = 300)
  samplingDistribution$position <- position
  samplingDistribution$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "planningModel", 
                                            "expectedNumber", "samplingDistribution", "materialityValue", "explanatoryText", "irCustom", "crCustom"))

  planningContainer[["samplingDistribution"]] <- samplingDistribution

  if(!ready || planningContainer$getError()) 
    return()

  limx <- length(0:planningState[["sampleSize"]])
  if(limx > 31) {
      limx <- 31
  }

  xVals <- (0:planningState[["sampleSize"]])[1:limx]
  if(planningState[["likelihood"]] == "poisson"){
    d0 <- stats::dpois(x = xVals, lambda = planningState[["materiality"]] * planningState[["sampleSize"]])
    d1 <- stats::dpois(x = 0:planningState[["expectedSampleError"]], lambda = planningState[["materiality"]] * planningState[["sampleSize"]])
  } else if(planningState[["likelihood"]] == "binomial"){
    d0 <- stats::dbinom(x = xVals, size = planningState[["sampleSize"]], prob = planningState[["materiality"]])
    d1 <- stats::dbinom(x = 0:planningState[["expectedSampleError"]], size = planningState[["sampleSize"]], prob = planningState[["materiality"]])
  } else if(planningState[["likelihood"]] == "hypergeometric"){
    d0 <- stats::dhyper(x = xVals, m = planningState[["populationK"]], n = planningState[["N"]] - planningState[["populationK"]], k = planningState[["sampleSize"]])
    d1 <- stats::dhyper(x = 0:planningState[["expectedSampleError"]], m = planningState[["populationK"]], n = planningState[["N"]] - planningState[["populationK"]], k = planningState[["sampleSize"]])
  }

  data0 <- data.frame(x = xVals, y = d0)
  data1 <- data.frame(x = 0:planningState[["expectedSampleError"]], y = d1)

  xTicks <- JASPgraphs::getPrettyAxisBreaks(xVals)
  yTicks <- JASPgraphs::getPrettyAxisBreaks(data0$y)

  pdata <- data.frame(x = c(0, 0), y = c(0, 0), type = c("Expected error-free", "Expected errors"))
  pdata$type <- factor(pdata$type, levels(pdata$type)[c(2,1)])

  p <- ggplot2::ggplot(data = pdata, ggplot2::aes(x = x, y = y, fill = type)) +
        ggplot2::geom_point(shape = 2, alpha = 0) +
        ggplot2::labs(fill = "") +
        ggplot2::scale_x_continuous(name = "n", labels = xTicks, breaks = xTicks) +
        ggplot2::scale_y_continuous(name = "Probability", labels = yTicks, breaks = yTicks) +
        ggplot2::geom_bar(data = data0, mapping = ggplot2::aes(x = x, y = y), stat = "identity", fill = "#7FE58B", size = 0.5, color = "black") +
        ggplot2::geom_bar(data = data1, mapping = ggplot2::aes(x = x, y = y), stat = "identity", fill = "#FF6666", size = 0.5, color = "black") +
        ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, fill = type), size = 0) +
        ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30))) +
        ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE)) +
        ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 12, shape = 22, fill = c("#FF6666", "#7FE58B"), stroke = 1.5, color = "black")))
  p <- JASPgraphs::themeJasp(p, legend.position = "top") + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"))

  samplingDistribution$plotObject <- p

  if(options[["explanatoryText"]]){
        figure3 <- createJaspHtml(paste0("<b>Figure ", planningContainer[["figNumber"]]$object ,".</b> The implied <b>", options[["planningModel"]], "</b> distribution. The number of expected errors in the selection is colored in red and 
                                          the number of expected error-free observations is colored in green. The total probability of the errors does not exceed the detection risk."), "p")
        figure3$position <- position + 1
        figure3$dependOn(optionsFromObject = samplingDistribution)
        planningContainer[["figure3"]] <- figure3
        planningContainer[["figNumber"]] <- createJaspState(planningContainer[["figNumber"]]$object + 1)
  }
}

.decisionAnalysisPlot <- function(options, planningOptions, planningState, planningContainer, type, position, ready){

  if(!is.null(planningContainer[["decisionPlot"]]) || !options[["decisionPlot"]]) return()

  decisionPlot <- createJaspPlot(plot = NULL, title = "Decision Analysis Plot", width = 600, height = 300)
  decisionPlot$position <- position
  decisionPlot$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", 
                                    "expectedNumber", "decisionPlot", "materialityValue", "explanatoryText", "irCustom", "crCustom"))

  planningContainer[["decisionPlot"]] <- decisionPlot

  if(!ready || planningContainer$getError()) 
    return()

  auditRisk <- 1 - options[["confidence"]]
  if(options[["IR"]] != "Custom"){
    inherentRisk <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  } else {
    inherentRisk <- options[["irCustom"]]
  }
  if(options[["CR"]] != "Custom"){
    controlRisk <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  } else {
    controlRisk <- options[["crCustom"]]
  }
  detectionRisk <- auditRisk / inherentRisk / controlRisk
  adjustedConfidence <- 1 - detectionRisk

  if(type == "frequentist"){

    startProgressbar(3)
    progressbarTick()
    n1 <- jfa::planning(materiality = planningOptions[["materiality"]], confidence = adjustedConfidence, 
                    expectedError = planningOptions[["expectedErrors"]], likelihood = "poisson", 
                    N = planningOptions[["populationSize"]])
    progressbarTick()                
    n2 <- jfa::planning(materiality = planningOptions[["materiality"]], confidence = adjustedConfidence, 
                expectedError = planningOptions[["expectedErrors"]], likelihood = "binomial", 
                N = planningOptions[["populationSize"]])
    progressbarTick()            
    n3 <- jfa::planning(materiality = planningOptions[["materiality"]], confidence = adjustedConfidence, 
                    expectedError = planningOptions[["expectedErrors"]], likelihood = "hypergeometric", 
                    N = planningOptions[["populationSize"]])
    
    n <- c(n1$sampleSize, n2$sampleSize, n3$sampleSize)   
    k <- c(n1$expectedSampleError, n2$expectedSampleError, n3$expectedSampleError)
    d <- data.frame(y = c(n, k), dist = rep(c("Poisson", "Binomial", "Hypergeometric"), 2),
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
          ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(0:(ceiling(1.1 * max(n))), min.n = 4), limits = c(0, ceiling(1.2 * max(n))))
      p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")

      optN <- base::switch(which.min(n), "1" = "Poisson", "2" = "binomial", "3" = "hypergeometric")
      planningContainer[["mostEfficientPlanningDistribution"]] <- createJaspState(optN)
      planningContainer[["mostEfficientPlanningDistribution"]]$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", 
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
        figure2 <- createJaspHtml(paste0("<b>Figure ", planningContainer[["figNumber"]]$object ,".</b> Decision analysis for the current options. The bars represent the sample size that is required under different planning distributions.
                                                                                    The number of expected errors in the selection is colored in red and the number of expected error-free observations is colored in green. 
                                                                                    The most efficient distribution for these options is the <b>", planningContainer[["mostEfficientPlanningDistribution"]]$object ,"</b> distribution."), "p")
        figure2$position <- position + 1
        figure2$dependOn(optionsFromObject = decisionPlot)
        planningContainer[["figure2"]] <- figure2
        planningContainer[["figNumber"]] <- createJaspState(planningContainer[["figNumber"]]$object + 1)
  }
}