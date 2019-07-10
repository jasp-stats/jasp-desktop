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

.calc.n.binomial <- function(options, alpha, jaspResults){
  startProgressbar(5000)
  for(n in 1:5000){
    progressbarTick()
    impk <- base::switch(options[["expectedErrors"]], "expectedRelative" = ceiling(n * options[["expectedPercentage"]]), "expectedAbsolute" = ceiling(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n))
    if(impk >= n){ next }
    x <- dbinom(0:impk, size = n, prob = jaspResults[["materiality"]]$object)
    if(sum(x) < alpha){
      return(n)
    }
  }
  jaspResults[["errorInSampler"]] <- createJaspState(TRUE)
  jaspResults[["errorInSampler"]]$dependOn(options = c("materiality", "materialityPercentage", "materialityValue", "expectedNumber", "expectedErrors", "populationValue", "populationSize"))
  return(1)
}

.calc.n.hypergeometric <- function(options, alpha, jaspResults){
  startProgressbar(5000)
  K <- ceiling(jaspResults[["N"]]$object * jaspResults[["materiality"]]$object)
  for(n in 1:5000){
    progressbarTick()
    impk <- base::switch(options[["expectedErrors"]], "expectedRelative" = ceiling(n * options[["expectedPercentage"]]), "expectedAbsolute" = ceiling(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n))
    if(impk >= n) { next }
    x <- dhyper(x = 0:impk, m = K, n = jaspResults[["N"]]$object - K, k = n)
    if(sum(x) < alpha){
      return(n)
    }
  }
  jaspResults[["errorInSampler"]] <- createJaspState(TRUE)
  jaspResults[["errorInSampler"]]$dependOn(options = c("materiality", "materialityPercentage", "materialityValue", "expectedNumber", "expectedErrors", "populationValue", "populationSize"))
  return(1)
}

.calc.n.poisson <- function(options, alpha, jaspResults){
  startProgressbar(5000)
  for(n in 1:5000){
    progressbarTick()
    k <- base::switch(options[["expectedErrors"]], "expectedRelative" = (n * options[["expectedPercentage"]]), "expectedAbsolute" = (options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n))
    x <- pgamma(jaspResults[["materiality"]]$object, shape = 1 + k, scale = 1 / n)
    # ppois(a; b) = 1 - pgamma(b; 1 + a; 1)
    if(x >= (1 - alpha)){
      return(n)
    }
  }
  jaspResults[["errorInSampler"]] <- createJaspState(TRUE)
  jaspResults[["errorInSampler"]]$dependOn(options = c("materiality", "materialityPercentage", "materialityValue", "expectedNumber", "expectedErrors", "populationValue", "populationSize"))
  return(1)
}

.classicalPlanningHelper <- function(options, jaspResults, planningContainer){

    if(!is.null(jaspResults[["planningResult"]])) return(jaspResults[["planningResult"]]$object)

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    k                       <- 0

    if(jaspResults[["ready"]]$object && !planningContainer$getError()){
      n <- base::switch(options[["planningModel"]],
                          "Poisson"         = .calc.n.poisson(options, alpha, jaspResults),
                          "binomial"        = .calc.n.binomial(options, alpha, jaspResults),
                          "hypergeometric"  = .calc.n.hypergeometric(options, alpha, jaspResults))
      k <- base::switch(options[["expectedErrors"]], "expectedRelative" = options[["expectedPercentage"]], "expectedAbsolute" = options[["expectedNumber"]] / n)
    }

    resultList <- list()
    resultList[["n"]]             <- n
    resultList[["k"]]             <- k
    resultList[["IR"]]            <- options[["IR"]]
    resultList[["CR"]]            <- options[["CR"]]
    resultList[["alpha"]]         <- alpha
    resultList[["confidence"]]    <- options[["confidence"]]

    jaspResults[["planningResult"]] <- createJaspState(resultList)
    jaspResults[["planningResult"]]$dependOn(options = c("IR", "CR", "confidence", "expectedNumber", "materialityPercentage", "planningModel", "expectedPercentage", "expectedNumber",
                                                      "materialityValue", "materiality", "recordNumberVariable", "monetaryVariable"))

    return(jaspResults[["planningResult"]]$object)
}

.classicalPlanningTable <- function(dataset, options, planningResult, jaspResults, position = 1, planningContainer){

  if(!is.null(planningContainer[["planningSummary"]])) return() #The options for this table didn't change so we don't need to rebuild it

  planningSummary                              <- createJaspTable("Planning Summary")
  planningSummary$position                     <- position
  planningSummary$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "planningModel", "recordNumberVariable", "monetaryVariable",
                                    "expectedErrors" , "expectedPercentage", "expectedNumber", "materialityValue", "materiality"))

  planningSummary$addColumnInfo(name = 'materiality',          title = "Materiality",          type = 'string')
  planningSummary$addColumnInfo(name = 'IR',                   title = "Inherent risk",        type = 'string')
  planningSummary$addColumnInfo(name = 'CR',                   title = "Control risk",         type = 'string')
  planningSummary$addColumnInfo(name = 'DR',                   title = "Detection risk",       type = 'string')
  planningSummary$addColumnInfo(name = 'k',                    title = "Expected errors",      type = 'string')
  planningSummary$addColumnInfo(name = 'n',                    title = "Required sample size", type = 'string')

  if(!jaspResults[["ready"]]$object){
    message <- base::switch(options[["planningModel"]],
                          "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution."),
                          "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution."),
                          "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution."))
    planningSummary$addFootnote(message = message, symbol="<i>Note.</i>")
  }

  planningContainer[["planningSummary"]]        <- planningSummary

  if(!is.null(jaspResults[["errorInSampler"]])){
    planningContainer$setError("There is no sample size (< 5000) large enough to prove the current materiality. Please try other values.")
    return()
  }

  ktable <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(planningResult[["k"]] * planningResult[["n"]], 2), "expectedAbsolute" = paste(jaspResults[["valutaTitle"]]$object, options[["expectedNumber"]]))
  if(options[["planningModel"]] != "Poisson" && options[["expectedErrors"]] == "expectedRelative")
    ktable <- ceiling(ktable)
  DRtable <- paste0(round(planningResult[["alpha"]], 3) * 100, "%")

  if(jaspResults[["materiality"]]$object == 0){
    row <- data.frame(materiality = ".", IR = planningResult[["IR"]], CR = planningResult[["CR"]], DR = DRtable, k = 0, n = ".")
    planningSummary$addRows(row)
    planningSummary$addFootnote(message = "The materiality is defined as 0.", symbol="<b>ANALYSIS NOT READY.</b>")
    return()
  }

  materialityTitle  <- paste0(round(jaspResults[["materiality"]]$object * 100, 2), "%")
  materialityValue  <- base::switch(options[["materiality"]], "materialityRelative" = ceiling(jaspResults[["materiality"]]$object * sum(dataset[, .v(options[["monetaryVariable"]])])), "materialityAbsolute" = options[["materialityValue"]])
  materiality       <- base::switch(options[["materiality"]], "materialityRelative" = materialityTitle, "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, materialityValue))

  if(!jaspResults[["ready"]]$object || planningContainer$getError()){
    row <- data.frame(materiality = materiality, IR = planningResult[["IR"]], CR = planningResult[["CR"]], DR = DRtable, k = ktable, n = ".")
    planningSummary$addRows(row)
    return()
  }

  if(planningResult[["n"]] > jaspResults[["N"]]$object && jaspResults[["ready"]]$object){
    planningContainer$setError("The required sample size is larger than the population size. You cannot audit this population with this materiality and this amount of confidence.")
    return()
  }

  message <- base::switch(options[["planningModel"]],
                        "Poisson" = paste0("The required sample size is based on the <b>Poisson</b> distribution <i>(\u03BB = ", round(jaspResults[["materiality"]]$object * planningResult[["n"]], 4) , ")</i>."),
                        "binomial" =  paste0("The required sample size is based on the <b>binomial</b> distribution <i>(p = ", round(jaspResults[["materiality"]]$object, 2) ,")</i>."),
                        "hypergeometric" = paste0("The required sample size is based on the <b>hypergeometric</b> distribution <i>(N = ", jaspResults[["N"]]$object ,", K = ", ceiling(jaspResults[["N"]]$object * jaspResults[["materiality"]]$object) ,")</i>."))
  planningSummary$addFootnote(message = message, symbol="<i>Note.</i>")

  row <- data.frame(materiality = materiality, IR = planningResult[["IR"]], CR = planningResult[["CR"]], DR = DRtable, k = ktable, n = planningResult[["n"]])
  planningSummary$addRows(row)
}

.attributesBound <- function(dataset, options, jaspResults){

  if(!is.null(jaspResults[["evaluationResult"]]))
    return(jaspResults[["evaluationResult"]]$object)

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr

  n                       <- 0
  k                       <- 0
  bound                   <- "."

  if(jaspResults[["runEvaluation"]]$object){
    if(options[["estimator"]] == "binomialBound"){
      n                     <- jaspResults[["sampleSize"]]$object
      kIndex                <- which(dataset[,.v(options[["auditResult"]])] == 1)

      if(options[["sampleFilter"]] != ""){
        kNumber               <- dataset[kIndex ,.v(options[["sampleFilter"]])]
        k                     <- length(rep(kIndex, times = kNumber))
      } else {
        k <- length(kIndex)
      }

      binomResult <- binom.test(x = k,
                                n = n,
                                p = jaspResults[["materiality"]]$object,
                                alternative = "less",
                                conf.level = (1 - alpha))
      bound                 <- binomResult$conf.int[2]
    } else if(options[["estimator"]] == "hyperBound"){

      N <- jaspResults[["N"]]$object
      n                     <- jaspResults[["sampleSize"]]$object
      kIndex                <- which(dataset[,.v(options[["auditResult"]])] == 1)

      if(options[["sampleFilter"]] != ""){
        kNumber               <- dataset[kIndex ,.v(options[["sampleFilter"]])]
        k                     <- length(rep(kIndex, times = kNumber))
      } else {
        k <- length(kIndex)
      }

      for(K in 1000:1){
       x <- phyper(k, K, N - K, n)
       if(x >= 0.05)
           break
      }
      bound <- K / N
    } else if(options[["estimator"]] == "gammaBound"){
      n                     <- jaspResults[["sampleSize"]]$object
      kIndex                <- which(dataset[,.v(options[["auditResult"]])] == 1)
      kNumber               <- dataset[kIndex ,.v(options[["sampleFilter"]])]
      k                     <- length(rep(kIndex, times = kNumber))
      bound <- qgamma(p = (1 - alpha), shape = 1 + k, scale = 1 / n)
    }
  }

  resultList <- list()
  resultList[["n"]]           <- n
  resultList[["k"]]           <- k
  resultList[["IR"]]          <- options[["IR"]]
  resultList[["CR"]]          <- options[["CR"]]
  resultList[["confidence"]]  <- options[["confidence"]]
  resultList[["bound"]]       <- bound
  resultList[["alpha"]]       <- alpha

  jaspResults[["evaluationResult"]] <- createJaspState(resultList)
  jaspResults[["evaluationResult"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materialityPercentage", "materialityValue", "performAudit", "estimator"))
  return(jaspResults[["evaluationResult"]]$object)
}

.attributesBoundTable <- function(options, evaluationResult, jaspResults, position = 1, evaluationContainer){

    if(!is.null(evaluationContainer[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation Summary")
    evaluationTable$position              <- position
    evaluationTable$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "auditResult", "sampleFilter", "planningModel",
                                      "mostLikelyError", "materialityValue", "auditType", "valuta", "performAudit", "estimator"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",          type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",          type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Errors",               type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                  type = 'string')
    evaluationTable$addColumnInfo(name = 'bound',         title = paste0(options[["confidence"]] * 100,"% Confidence bound"), type = 'string')
    if(options[["materiality"]] == "materialityAbsolute" || options[["monetaryVariable"]] != "")
      evaluationTable$addColumnInfo(name = 'projm',         title = "Maximum Misstatement",           type = 'string')

    message <- base::switch(options[["estimator"]],
                              "gammaBound" = "The confidence bound is calculated according to the <b>gamma</b> distributon.",
                              "binomialBound" = "The confidence bound is calculated according to the <b>binomial</b> distributon.",
                              "hyperBound" = "The confidence bound is calculated according to the <b>hypergeometric</b> distribution.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")

    evaluationContainer[["evaluationTable"]]      <- evaluationTable

    materialityTable <- ifelse(options[["materiality"]] == "materialityRelative", yes = paste0(round(jaspResults[["materiality"]]$object, 2) * 100, "%"), no = paste(jaspResults[["valutaTitle"]]$object, options[["materialityValue"]]))
    # Return empty table
    if(!jaspResults[["runEvaluation"]]$object){
      row <- data.frame(materiality = materialityTable, n = ".", k = ".", bound = ".")
      if(options[["materiality"]] == "materialityAbsolute" || options[["monetaryVariable"]] != "")
        row <- cbind(row, projm = ".")
      evaluationTable$addRows(row)
      return()
    }

    mle <- 0
    if(jaspResults[["N"]]$object != 0)
      mle <- paste0(round(evaluationResult[["k"]] / evaluationResult[["n"]], 4) * 100, "%")

    boundTable          <- "."
    if(evaluationResult[["bound"]] != ".")
        boundTable <- paste0(round(evaluationResult[["bound"]], 4) * 100, "%")

    row <- data.frame(materiality = materialityTable, n = evaluationResult[["n"]], k = evaluationResult[["k"]], bound = boundTable)
    if(options[["materiality"]] == "materialityAbsolute" || options[["monetaryVariable"]] != "")
      row <- cbind(row, projm = paste(jaspResults[["valutaTitle"]]$object, round(evaluationResult[["bound"]] * jaspResults[["total_data_value"]]$object, 2)))
    if(options[["mostLikelyError"]])
      row <- cbind(row, mle = mle)
    evaluationTable$addRows(row)
}

.stringerBound <- function(dataset, options, jaspResults){
    if(!is.null(jaspResults[["evaluationResult"]]))
      return(jaspResults[["evaluationResult"]]$object)
    # Based on the paper:
    # Stringer, K. W. (1963). Practical aspects of statistical sampling in auditing. In Proceedings of the Business and Economic Statistics Section (pp. 405-411). American Statistical Association.
    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    NoOfErrors              <- 0
    z                       <- 0
    bound                   <- "."

    if(jaspResults[["runEvaluation"]]$object){
      sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
      t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])]
      z                       <- t / sample[, .v(options[["monetaryVariable"]])]
      if(options[["sampleFilter"]] != ""){
        z                       <- rep(z, times = dataset[ ,.v(options[["sampleFilter"]])])
      }
      n                       <- length(z)
      zplus                   <- sort(subset(z, z > 0), decreasing = TRUE)
      zmin                    <- sort(subset(z, z < 0), decreasing = TRUE)
      M                       <- length(zplus)
      NoOfErrors              <- length(which(z != 0))
      bound                   <- 1 - alpha^(1 / n)
      if(M > 0){
          prop.sum            <- 0
          for(i in 1:M){
              prop.sum        <- prop.sum + ((qbeta(1 - alpha, i + 1, n - i) - qbeta(1 - alpha, (i - 1) + 1, n - (i - 1) ))  * zplus[i])
          }
          bound               <- bound + prop.sum
      }
      if(options[["stringerBoundLtaAdjustment"]]){
        bound <- bound - ( (1/n) * sum(abs(zmin)) )
      }
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- NoOfErrors
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["alpha"]]       <- alpha

    jaspResults[["evaluationResult"]] <- createJaspState(resultList)
    jaspResults[["evaluationResult"]]$dependOn(options = c("IR", "CR", "confidence", "variableType", "auditResult", "materiality", "estimator", "materialityValue", "materialityPercentage", "stringerBoundLtaAdjustment", "performAudit"))
    return(jaspResults[["evaluationResult"]]$object)
}

.auditValueBoundTable <- function(options, evaluationResult, jaspResults, position = 1, evaluationContainer){

    if(!is.null(evaluationContainer[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    evaluationTable                       <- createJaspTable("Evaluation Summary")
    evaluationTable$position              <- position
    evaluationTable$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "auditResult", "planningModel", "mostLikelyError", "sampleFilter", "variableType",
                                      "estimator", "monetaryVariable", "materialityValue", "valuta", "stringerBoundLtaAdjustment", "performAudit"))

    evaluationTable$addColumnInfo(name = 'materiality',   title = "Materiality",                      type = 'string')
    evaluationTable$addColumnInfo(name = 'n',             title = "Sample size",                      type = 'string')
    evaluationTable$addColumnInfo(name = 'fk',            title = "Errors",                           type = 'string')
    evaluationTable$addColumnInfo(name = 'k',             title = "Total tainting",                   type = 'string')
    if(options[["mostLikelyError"]])
      evaluationTable$addColumnInfo(name = 'mle',         title = "MLE",                              type = 'string')
    evaluationTable$addColumnInfo(name = 'bound',         title = paste0(options[["confidence"]] * 100,"% Confidence bound"), type = 'string')
    evaluationTable$addColumnInfo(name = 'projm',         title = "Maximum Misstatement",           type = 'string')

    message <- base::switch(options[["estimator"]],
                              "stringerBound" = "The confidence bound is calculated according to the <b>Stringer</b> method.",
                              "regressionBound" = "The confidence bound is calculated according to the <b>regression</b> method.",
                              "directBound" = "The confidence bound is calculated according to the <b>direct</b> method.",
                              "differenceBound" = "The confidence bound is calculated according to the <b>difference</b> method.",
                              "ratioBound" = "The confidence bound is calculated according to the <b>ratio</b> method.")
    evaluationTable$addFootnote(message = message, symbol="<i>Note.</i>")

    evaluationContainer[["evaluationTable"]]      <- evaluationTable

    materialityTable <- ifelse(options[["materiality"]] == "materialityRelative", yes = paste0(round(jaspResults[["materiality"]]$object, 2) * 100, "%"), no = paste(jaspResults[["valutaTitle"]]$object, options[["materialityValue"]]))

    # Return empty table with materiality
    if(!jaspResults[["runEvaluation"]]$object){
      row <- data.frame(materiality = materialityTable, n = ".", fk = ".", k = ".", bound = ".", projm = ".")
      evaluationTable$addRows(row)
      return()
    }

    total_data_value <- jaspResults[["total_data_value"]]$object

    mle <- 0
    if(options[["estimator"]] == "stringerBound"){
      mle <- paste0(round(sum(evaluationResult[["z"]]) / evaluationResult[["n"]], 4) * 100, "%")
    } else {
      mle <- paste(jaspResults[["valutaTitle"]]$object, round(evaluationResult[["mleTable"]] * total_data_value, 2))
    }
    errors <- round(sum(evaluationResult[["z"]]), 2)

    boundTable          <- "."
    projectedMisstatement <- "."
    if(evaluationResult[["bound"]] != "."){
        boundTable <- round(evaluationResult[["bound"]], 4)
        projectedMisstatement <- paste(jaspResults[["valutaTitle"]]$object, ceiling(evaluationResult[["bound"]] * total_data_value))
        boundTable <- paste0(boundTable * 100, "%")
    }

    row <- data.frame(materiality = materialityTable, n = evaluationResult[["n"]], fk = evaluationResult[["k"]], k = errors, bound = boundTable, projm = projectedMisstatement)
    if(options[["mostLikelyError"]])
      row <- cbind(row, mle = mle)
    evaluationTable$addRows(row)
}

.directBound <- function(dataset, options, jaspResults){

  if(!is.null(jaspResults[["evaluationResult"]]))
    return(jaspResults[["evaluationResult"]]$object)

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    M                       <- 0
    z                       <- 0
    bound                   <- "."
    mle                     <- 0
    mleTable                <- 0

    if(jaspResults[["runEvaluation"]]$object){

        sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
        n                       <- nrow(sample)

        t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])]
        z                       <- t / sample[, .v(options[["monetaryVariable"]])]
        M                       <- length(which(t != 0))
        B                       <- jaspResults[["total_data_value"]]$object

        N                       <- jaspResults[["N"]]$object
        w                       <- sample[, .v(options[["auditResult"]])]
        meanw                   <- mean(w)

        mle                     <- N * meanw
        stand.dev               <- sd(w) * (N / sqrt(n)) * sqrt( (N-n) / (N-1) )
        lowerValue              <- mle - qt(p = 1 - alpha, df = n - 1) * stand.dev
        bound                   <- (B - lowerValue) / B
        mleTable                <- (B - mle) / B
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- M
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["alpha"]]       <- alpha
    resultList[["mle"]]         <- mle
    resultList[["mleTable"]]    <- mleTable

    jaspResults[["evaluationResult"]] <- createJaspState(resultList)
    jaspResults[["evaluationResult"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materialityPercentage", "estimator", "monetaryVariable", "materialityValue", "variableType", "performAudit"))
    return(jaspResults[["evaluationResult"]]$object)
}

.differenceBound <- function(dataset, options, jaspResults){

  if(!is.null(jaspResults[["evaluationResult"]]))
    return(jaspResults[["evaluationResult"]]$object)

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    M                       <- 0
    z                       <- 0
    bound                   <- "."
    mle                     <- 0
    mleTable                <- 0

    if(jaspResults[["runEvaluation"]]$object){

        sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
        n                       <- nrow(sample)

        t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])]
        z                       <- t / sample[, .v(options[["monetaryVariable"]])]
        M                       <- length(which(t != 0))
        B                       <- jaspResults[["total_data_value"]]$object

        N                       <- jaspResults[["N"]]$object
        w                       <- sample[, .v(options[["auditResult"]])]
        meanw                   <- mean(w)
        meant                   <- mean(t)

        mle                     <- B - N * meant
        stand.dev               <- sd(t) * (N / sqrt(n)) * sqrt( (N-n) / (N-1) )
        lowerValue              <- mle - qt(p = 1 - alpha, df = n - 1) * stand.dev
        if(lowerValue == 0){
          bound                 <- 0
        } else {
          bound                 <- (B - lowerValue) / B
        }
        mleTable                <- (B - mle) / B
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- M
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["alpha"]]       <- alpha
    resultList[["mle"]]         <- mle
    resultList[["mleTable"]]    <- mleTable

    jaspResults[["evaluationResult"]] <- createJaspState(resultList)
    jaspResults[["evaluationResult"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materialityPercentage", "estimator", "monetaryVariable", "materialityValue", "variableType", "performAudit"))
    return(jaspResults[["evaluationResult"]]$object)
}

.ratioBound <- function(dataset, options, jaspResults){

  if(!is.null(jaspResults[["evaluationResult"]]))
    return(jaspResults[["evaluationResult"]]$object)

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    M                       <- 0
    z                       <- 0
    bound                   <- "."
    mle                     <- 0
    mleTable                <- 0

    if(jaspResults[["runEvaluation"]]$object){

        sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
        n                       <- nrow(sample)

        t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])]
        z                       <- t / sample[, .v(options[["monetaryVariable"]])]
        M                       <- length(which(t != 0))
        B                       <- jaspResults[["total_data_value"]]$object

        N                       <- jaspResults[["N"]]$object
        w                       <- sample[, .v(options[["auditResult"]])]
        b                       <- sample[, .v(options[["monetaryVariable"]])]
        meanw                   <- mean(w)
        meanb                   <- mean(b)
        sumw                    <- sum(w)
        sumb                    <- sum(b)
        q                       <- sumw / sumb

        mle                     <- q * B
        stand.dev               <- sqrt( ( sum(t^2) - 2 * (1-q) * sum(b*t) + (1 - q)^2 * sum(b^2) ) / (n - 1)) * (N / sqrt(n)) * sqrt( (N-n) / (N-1) )
        lowerValue              <- mle - qt(p = 1 - alpha, df = n - 1) * stand.dev
        if(lowerValue == 0){
          bound                 <- 0
        } else {
          bound                 <- (B - lowerValue) / B
        }
        mleTable                <- (B - mle) / B
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- M
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["alpha"]]       <- alpha
    resultList[["mle"]]         <- mle
    resultList[["mleTable"]]    <- mleTable

    jaspResults[["evaluationResult"]] <- createJaspState(resultList)
    jaspResults[["evaluationResult"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materialityPercentage", "estimator", "monetaryVariable", "materialityValue", "variableType", "performAudit"))
    return(jaspResults[["evaluationResult"]]$object)
}

.regressionBound <- function(dataset, options, jaspResults){

  if(!is.null(jaspResults[["evaluationResult"]]))
    return(jaspResults[["evaluationResult"]]$object)

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    M                       <- 0
    z                       <- 0
    bound                   <- "."
    mleregression           <- 0
    mleTable                <- 0

    if(jaspResults[["runEvaluation"]]$object){

        sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
        n                       <- nrow(sample)

        t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])]
        z                       <- t / sample[, .v(options[["monetaryVariable"]])]
        M                       <- length(which(t != 0))

        B                       <- jaspResults[["total_data_value"]]$object
        N                       <- jaspResults[["N"]]$object
        b                       <- sample[, .v(options[["monetaryVariable"]])]
        w                       <- sample[, .v(options[["auditResult"]])]
        b1                      <- as.numeric(coef(lm(w ~ b))[2])

        meanb                   <- mean(b)
        meanw                   <- mean(w)

        mleregression           <- N * meanw + b1 * (B - N * meanb)
        stand.dev               <- sd(w) * sqrt(1 - cor(b, w)^2) * ( N / sqrt(n)) * sqrt( (N-n) / (N-1) )
        lowerValue              <- mleregression - qt(p = 1 - alpha, df = n - 1) * stand.dev
        if(lowerValue == 0){
          bound                 <- 0
        } else {
          bound                 <- (B - lowerValue) / B
        }
        mleTable                <- (B - mleregression) / B
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- M
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["alpha"]]       <- alpha
    resultList[["mle"]]         <- mleregression
    resultList[["mleTable"]]    <- mleTable

    jaspResults[["evaluationResult"]] <- createJaspState(resultList)
    jaspResults[["evaluationResult"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materialityPercentage",
                                                            "estimator", "monetaryVariable", "materialityValue", "variableType", "performAudit"))
    return(jaspResults[["evaluationResult"]]$object)
}
