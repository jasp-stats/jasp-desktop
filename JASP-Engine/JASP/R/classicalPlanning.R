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

  # Valuta title
  jaspResults[["valutaTitle"]] <- createJaspState(base::switch(options[["valuta"]], "euroValuta" = "\u20AC", "dollarValuta" = "\u0024", "otherValuta" = options[["otherValutaName"]]))

  # Interpretation for the Global Options phase
  if(options[["explanatoryText"]] && is.null(jaspResults[["procedureContainer"]])){
    procedureContainer <- createJaspContainer(title= "<u>Procedure</u>")
    procedureContainer$position <- 1
    if(is.null(jaspResults[["confidenceLevelLabel"]])){
      jaspResults[["confidenceLevelLabel"]] <- createJaspState(paste0(round(options[["confidence"]] * 100, 2), "%"))
      jaspResults[["confidenceLevelLabel"]]$dependOn(options = c("confidence"))
    }
    criterion <- base::switch(options[["materiality"]], "materialityRelative" = "<b>percentage</b>", "materialityAbsolute" = "<b>amount</b>")
    materialityLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]] * 100, 2), "%"), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
    procedureContainer[["procedureParagraph"]] <- createJaspHtml(paste0("The objective of a substantive testing procedure is to determine with a specified confidence <b>(", jaspResults[["confidenceLevelLabel"]]$object, ")</b> whether the ", criterion ," of
                                                                                          misstatement in the target population is lower than the specified materiality of <b>", materialityLabel, "</b>."), "p")
    procedureContainer[["procedureParagraph"]]$position <- 1
    procedureContainer$dependOn(options = c("explanatoryText", "confidence", "materiality", "materialityValue", "materialityPercentage", "valuta"))
    jaspResults[["procedureContainer"]] <- procedureContainer
  }

  .auditRiskModel(options, jaspResults)

  if(options[["materiality"]] == "materialityAbsolute"){
    jaspResults[["ready"]] <- createJaspState(options[["materialityValue"]] != 0 && options[["populationSize"]] != 0 && options[["populationValue"]] != 0)
  } else {
    jaspResults[["ready"]] <- createJaspState(options[["materialityPercentage"]] != 0 && options[["populationSize"]] != 0)
  }
  jaspResults[["ready"]]$dependOn(options = c("materialityValue", "materialityPercentage", "populationSize", "populationValue", "materiality"))

  planningContainer <- createJaspContainer(title= "<u>Planning</u>")
  planningContainer$position <- 3

  if(jaspResults[["ready"]]$object){
    if(options[["populationValue"]] == 0) { populationValue <- 0.01 } else { populationValue <- options[["populationValue"]] }
    jaspResults[["total_data_value"]] <- createJaspState(populationValue)
    materiality <- ifelse(options[["materiality"]] == "materialityAbsolute", yes = options[["materialityValue"]] / populationValue, no = options[["materialityPercentage"]])
    jaspResults[["materiality"]] <- createJaspState(materiality)
    jaspResults[["materiality"]]$dependOn(options = c("materialityValue", "materialityPercentage", "populationSize", "populationValue", "materiality"))

    if(options[["materiality"]] == "materialityAbsolute" && options[["materialityValue"]] >= jaspResults[["total_data_value"]]$object)
     planningContainer$setError("Analysis not possible: Your materiality is higher than the total value of the observations.") 
    expTMP <- ifelse(options[['expectedErrors']] == "expectedRelative", yes = options[["expectedPercentage"]], no = options[["expectedNumber"]] / populationValue)
    if(expTMP > materiality){
      planningContainer$setError("Analysis not possible: Your expected errors are higher than materiality.")
    }
  }

  planningResult <- .classicalPlanningManual(options, jaspResults, planningContainer)
  
  if(options[["explanatoryText"]] && is.null(planningContainer[["planningParagraph"]])){
    materialityLevelLabel           <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]], 4) * 100, "%"), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
    expected.errors <- max.errors <- requiredSampleSize <- 0
    if(!is.null(jaspResults[["planningResult"]])){
      requiredSampleSize <- planningResult[["n"]]
      expected.errors   <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = paste0(round(options[["expectedPercentage"]] * 100, 2), "%"), no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))
      max.errors        <- ifelse(options[["expectedErrors"]] == "expectedRelative", yes = ceiling(options[["expectedPercentage"]] * planningResult[["n"]]), no = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]] + 1))
    }
    planningContainer[["planningParagraph"]] <- createJaspHtml(paste0("The most likely error in the data was expected to be <b>", expected.errors ,"</b>. The sample size that is required to prove a materiality of <b>", materialityLevelLabel ,"</b>, assuming
                                                                                              the sample contains <b>", expected.errors ,"</b> full errors, is <b>", requiredSampleSize ,"</b>. This sample size is based on the <b>", options[["planningModel"]] , "</b> distribution, the inherent risk <b>(", options[["IR"]] , ")</b>, the
                                                                                              control risk <b>(", options[["CR"]] , ")</b> and the expected errors. Consequently, if the sum of errors from the audited observations exceeds <b>", max.errors ,"</b>, the
                                                                                              maximum misstatement exceeds materiality and the population cannot be approved."), "p")
    planningContainer[["planningParagraph"]]$position <- 1
    planningContainer[["planningParagraph"]]$dependOn(options = c("expectedPercentage", "expectedErrors", "expectedNumber", "planningModel", "IR", "CR", "materialityPercentage", "confidence", "materialityValue"))
  }
  jaspResults[["figNumber"]] <- createJaspState(1)

  # Create a decision plot (if the user wants it)
  if(options[["decisionPlot"]])
    .decisionAnalysis(options, jaspResults, position = 4, planningContainer, type = "frequentist")
  # Finish planning
  jaspResults[["planningContainer"]] <- planningContainer
}

.classicalPlanningManual <- function(options, jaspResults, planningContainer){

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

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr
  DRtable                 <- paste0(round(alpha, 3) * 100, "%")

  if(!jaspResults[["ready"]]$object || planningContainer$getError()){

    message <- base::switch(options[["planningModel"]],
                        "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution."),
                        "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution."),
                        "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution."))
    summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

    row <- data.frame(materiality = ".", IR = options[["IR"]], CR = options[["CR"]], DR = DRtable, k = ".", n = ".")
    summaryTable$addRows(row)
    summaryTable$addFootnote(message = "The materiality is defined as 0.", symbol="<b>ANALYSIS NOT READY.</b>")
    return()
  }

  jaspResults[["N"]] <- createJaspState(options[["populationSize"]])
  jaspResults[["N"]]$dependOn(options = c("populationSize"))

  if(is.null(jaspResults[["planningResult"]])){
    n <- base::switch(options[["planningModel"]],
                        "Poisson"         = .calc.n.poisson(options, alpha, jaspResults),
                        "binomial"        = .calc.n.binomial(options, alpha, jaspResults),
                        "hypergeometric"  = .calc.n.hypergeometric(options, alpha, jaspResults))
    k <- base::switch(options[["expectedErrors"]], "expectedRelative" = options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]] / options[["populationValue"]])

  resultList <- list()
  resultList[["n"]]             <- n
  resultList[["k"]]             <- k
  resultList[["IR"]]            <- options[["IR"]]
  resultList[["CR"]]            <- options[["CR"]]
  resultList[["alpha"]]         <- alpha
  resultList[["confidence"]]    <- options[["confidence"]]
  jaspResults[["planningResult"]] <- createJaspState(resultList)
  jaspResults[["planningResult"]]$dependOn(options = c("IR", "CR", "confidence", "expectedErrors", "materialityPercentage", "populationSize", "expectedPercentage", "expectedNumber",
                                                      "planningModel", "materialityValue", "populationValue", "materiality"))
  }

  resultList <- jaspResults[["planningResult"]]$object

  message <- base::switch(options[["planningModel"]],
                        "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution <i>(\u03BB = ", round(jaspResults[["materiality"]]$object * resultList[["n"]], 4) , ")</i>."),
                        "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution <i>(p = ", round(jaspResults[["materiality"]]$object, 2) ,")</i>."),
                        "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution <i>(N = ", options[["populationSize"]] ,", K = ", ceiling(options[["populationSize"]] * jaspResults[["materiality"]]$object) ,")</i>."))
  summaryTable$addFootnote(message = message, symbol="<i>Note.</i>")

  if(!is.null(jaspResults[["errorInSampler"]])){
    planningContainer$setError("There is no sample size (< 5000) large enough to prove the current materiality. Please try other values.")
    return()
  }

  if(resultList[["n"]] > jaspResults[["N"]]$object && jaspResults[["ready"]]$object){
    planningContainer$setError("The required sample size is larger than the population size. You cannot audit this population with this materiality and this amount of confidence.")
    return()
  }

  ktable <- base::switch(options[["expectedErrors"]], "expectedRelative" = (resultList[["k"]] * resultList[["n"]]), "expectedAbsolute" = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))
  if(options[["planningModel"]] != "Poisson" && options[["expectedErrors"]] == "expectedRelative")
    ktable <- ceiling(ktable)

  materialityTitle <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue <- base::switch(options[["materiality"]], "materialityRelative" = ceiling(jaspResults[["materiality"]]$object * options[["populationValue"]]), "materialityAbsolute" = options[["materialityValue"]])
  materiality <- base::switch(options[["materiality"]], "materialityRelative" = materialityTitle, "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, materialityValue))

  row <- data.frame(materiality = materiality, IR = resultList[["IR"]], CR = resultList[["CR"]], DR = DRtable, k = ktable, n = resultList[["n"]])
  summaryTable$addRows(row)

  return(resultList)
}
