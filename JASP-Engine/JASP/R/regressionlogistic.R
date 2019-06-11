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

RegressionLogistic <- function(dataset=NULL, options, perform="run",
                               callback=function(...) 0, ...) {
  
  # DATASET LOADING
  numericVars <- unlist(c(options[["covariates"]],
                          options[["wlsWeights"]]))
  numericVars <- numericVars[numericVars != ""]
  factorVars <- unlist(c(options[["dependent"]],
                         options[["factors"]]))
  factorVars <- factorVars[factorVars != ""]
  if (is.null (dataset)) {
    if (perform == "run") {
      dataset <- .readDataSetToEnd(columns.as.numeric = numericVars,
                                   columns.as.factor = factorVars,
                                   exclude.na.listwise = c(numericVars,
                                                           factorVars))
    } else {
      dataset <- .readDataSetHeader(columns.as.numeric = numericVars,
                                    columns.as.factor = factorVars)
    }
  }
  
  # ERROR HANDLING
  if (options[["dependent"]] != "") {
    errors <- .hasErrors(dataset, perform, type = "factorLevels",
                         factorLevels.target = options[["dependent"]],
                         factorLevels.amount = '!= 2',
                         exitAnalysisIfErrors = TRUE)
  }
  
  if (options[["wlsWeights"]] != "") {
    errors <- .hasErrors(dataset, perform, type = "limits",
                         limits.target = options[["wlsWeights"]],
                         limits.min = 0, limits.max = Inf,
                         exitAnalysisIfErrors = TRUE)
  }
  
  if (length(options[["covariates"]]) != 0) {
    errors <- .hasErrors(dataset, perform,
                         type = c("observations", "infinity", "variance"),
                         all.target = options[["covariates"]],
                         observations.amount = "< 2",
                         exitAnalysisIfErrors = TRUE)
  }
  
  if (perform == "run" && nrow(dataset) == 0) {
    .quitAnalysis("Dataset has no observations, check for missing values!")
  }
  
  # STATE SYSTEM
  # load state
  state <- .retrieveState()
  
  # init output variables
  lrObj <- # glm object
    modelSummary <- # fit/summary table
    estimatesTable <- # parameter estimates table
    estimatesTableBootstrapping <- # parameter estimates table bootstrapping
    casewiseDiagnosticsTable <- # casewise diagnostics table
    confusionMatrix <- # confusion matrix table
    perfMetrics <- # performance metrics of full model
    estimatesPlots <- # plots for estimates
    predictedPlot <- # predicted - residuals plot
    predictorPlots <- # predictor - residuals plots
    squaredPearsonPlot <- # squared pearson - predicted prob plot
    factorDescriptives <- # factor descriptives table
    NULL
  
  # diff check
  if (!is.null(state) && perform == "run") {
    diff <- .diff(options, state[["options"]])
    with(diff, { # with(diff, {}) makes us need "<<-" to assign to global env
      if (!any(dependent, covariates, factors, wlsWeights, modelTerms,
               includeIntercept, wlsWeights, method)) {
        lrObj <<- state[["lrObj"]]
        modelSummary <<- state[["modelSummary"]]
        
        if (!any(coeffEstimates, coeffCI, coeffCIInterval, coeffCIOR, stdCoeff,
                 oddsRatios, VovkSellkeMPR, robustSEOpt)) {
          # estimates table can be reused
          estimatesTable <<- state[["estimatesTable"]]
        }
        
        if (!any(coeffEstimatesBootstrappingReplicates)) {
          # estimates table bootstrapping can be reused
          estimatesTableBootstrapping <<- state[["estimatesTableBootstrapping"]]
        }
        
        if (!any(casewiseDiagnostics, casewiseDiagnosticsType, casewiseDiagnosticsCooksDistance)) {
          # estimates table bootstrapping can be reused
          casewiseDiagnosticsTable <<- state[["casewiseDiagnosticsTable"]]
        }
        
        if (!any(confusionMatrixOpt, confusionMatrixProportions)) {
          # confusionMatrix can be reused
          confusionMatrix <<- state[["confusionMatrix"]]
        }
        if (!any(AUC, Sens, Spec, Prec, Fmsr, BrierScr, Hmsr)) {
          # metrics table can be reused
          perfMetrics <<- state[["perfMetrics"]]
        }
        
        if (!any(estimatesPlotsOpt, estimatesPlotsCI, plotWidth, plotHeight,
                 showPoints)) {
          # estimates plots can be reused
          estimatesPlots <<- state[["estimatesPlots"]]
        }
        
        if (!any(predictedPlotOpt, plotWidth, plotHeight, residualType)) {
          # predicted - residuals plot can be reused
          predictedPlot <<- state[["predictedPlot"]]
        }
        
        if (!any(predictorPlotsOpt, plotWidth, plotHeight, residualType)) {
          # predictor - residuals plots can be reused
          predictorPlots <<- state[["predictorPlots"]]
        }
        
        if (!any(squaredPearsonPlotOpt, plotWidth, plotHeight)) {
          # squared pearson plot can be reused
          squaredPearsonPlot <<- state[["squaredPearsonPlot"]]
        }
        
        if (!any(factorDescriptivesOpt)) {
          # descriptives table can be reused
          factorDescriptives <<- state[["factorDescriptives"]]
        }
      }
    })
  } else if (!is.null(state)) {
    lrObj <- state[["lrObj"]]
    modelSummary <- state[["modelSummary"]]
    estimatesTable <- state[["estimatesTable"]]
    estimatesTableBootstrapping <- state[["estimatesTableBootstrapping"]]
    casewiseDiagnosticsTable <- state[["casewiseDiagnosticsTable"]]
    confusionMatrix <- state[["confusionMatrix"]]
    perfMetrics <- state[["perfMetrics"]]
    estimatesPlots <- state[["estimatesPlots"]]
    predictedPlot <- state[["predictedPlot"]]
    predictorPlots <- state[["predictorPlots"]]
    squaredPearsonPlot <- state[["squaredPearsonPlot"]]
    factorDescriptives <- state[["factorDescriptives"]]
  }
  
  
  # META INFORMATION
  .pdMeta <- list(list(name = "confusionMatrix", type = "table"),
                  list(name = "perfMetrics", type = "table"))
  .rpMeta <- list(list(name = "predictedPlot", type = "image"),
                  list(name = "predictorPlots", type = "collection",
                       meta = "image"),
                  list(name = "squaredPearsonPlot", type = "image"))
  
  .meta <-  list(
    list(name = "title", type = "title"),
    list(name = "modelSummary", type = "table"),
    list(name = "estimatesTable", type = "table"),
    list(name = "estimatesTableBootstrapping", type = "table"),
    list(name = "casewiseDiagnosticsTable", type = "table"),
    list(name = "factorDescriptives", type = "table"),
    list(name = "perfDiagnostics", type = "object", meta = .pdMeta),
    list(name = "estimatesPlots", type = "collection", meta = "image"),
    list(name = "residualsPlots", type = "object", meta = .rpMeta)
  )
  
  # RESULTS GENERATION
  # for each non-null result, generate results
  if (is.null(lrObj)) {
    lrObj <- .jaspGlm(dataset, options, perform, type = "binomial")
  }
  
  
  if (is.null(modelSummary)) {
    modelSummary <- .glmModelSummary(lrObj, options, perform, type = "binomial")
  }
  
  if (is.null(estimatesTable) && options[["coeffEstimates"]]) {
    estimatesTable <- .glmEstimatesTable(lrObj, options, perform,
                                         type = "binomial")
  }
  
  if (is.null(estimatesTableBootstrapping) && options[["coeffEstimatesBootstrapping"]] && options[["coeffEstimates"]]) {
    estimatesTableBootstrapping <- .estimatesTableBootstrapping(dataset, options, perform)
    if(estimatesTableBootstrapping == "Bootstrapping options have changed")
      return()
    
    estimatesTableBootstrappingState <- estimatesTableBootstrapping
  } else if(options[["coeffEstimatesBootstrapping"]] && options[['coeffEstimates']]){
    estimatesTableBootstrappingState <- estimatesTableBootstrapping
  } else{
    estimatesTableBootstrappingState <- estimatesTableBootstrapping
    estimatesTableBootstrapping <- NULL
  }
  
  if (is.null(casewiseDiagnosticsTable) && options[["casewiseDiagnostics"]]) {
    casewiseDiagnosticsTable <- .casewiseDiagnosticsTable(dataset, lrObj, options)
  }
  
  if (is.null(confusionMatrix) && options[["confusionMatrixOpt"]]) {
    confusionMatrix <- .glmConfusionMatrix(lrObj, options, perform,
                                           type = "binomial")
  }
  
  wantsPerfMetrics <- with(options, any(AUC, Sens, Spec, Prec, Fmsr, BrierScr,
                                        Hmsr))
  if (is.null(perfMetrics) && wantsPerfMetrics) {
    perfMetrics <- .glmPerformanceMetrics(lrObj, options, perform,
                                          type = "binomial")
  }
  
  perfDiagnostics <- list("confusionMatrix" = confusionMatrix,
                          "perfMetrics" = perfMetrics,
                          "title" = "Performance Diagnostics")
  
  if (is.null(estimatesPlots) && options[["estimatesPlotsOpt"]]) {
    estimatesPlots <- .glmEstimatesPlots(lrObj, options, perform,
                                         type = "binomial")
  }
  
  if (is.null(predictedPlot) && options[["predictedPlotOpt"]]) {
    predictedPlot <- .glmPredictedResidualsPlot(lrObj, options, perform,
                                                type = "binomial")
  }
  
  if (is.null(predictorPlots) && options[["predictorPlotsOpt"]]) {
    predictorPlots <- .glmPredictorResidualsPlots(lrObj, options, perform,
                                                  type = "binomial")
  }
  
  if (is.null(squaredPearsonPlot) && options[["squaredPearsonPlotOpt"]]) {
    squaredPearsonPlot <- .glmSquaredPearsonResidualsPlot(lrObj, options,
                                                          perform,
                                                          type = "binomial")
  }
  
  if(is.null(factorDescriptives) && options[["factorDescriptivesOpt"]]) {
    factorDescriptives <- .glmFactorDescriptives(dataset, options, perform,
                                                 type = "binomial")
    
  }
  
  residualsPlots <- list("predictedPlot" = predictedPlot,
                         "predictorPlots" = predictorPlots,
                         "squaredPearsonPlot" = squaredPearsonPlot,
                         "title" = "Residual plots")
  
  results <- list()
  results[[".meta"]] <- .meta
  results[["title"]] <- "Logistic Regression"
  results[["modelSummary"]] <- modelSummary
  results[["estimatesTable"]] <- estimatesTable
  results[["estimatesTableBootstrapping"]] <- estimatesTableBootstrapping
  results[["casewiseDiagnosticsTable"]] <- casewiseDiagnosticsTable
  results[["perfDiagnostics"]] <- perfDiagnostics
  results[["estimatesPlots"]] <- estimatesPlots
  results[["residualsPlots"]] <- residualsPlots
  results[["factorDescriptives"]] <- factorDescriptives
  
  
  # RETURN RESULTS
  
  plotPaths <- c(.lrGetPlotPaths(estimatesPlots),
                 .lrGetPlotPaths(predictorPlots),
                 predictedPlot[["data"]], squaredPearsonPlot[["data"]])
  
  if (perform == "run") {
    state <- list()
    state[["options"]] <- options
    state[["lrObj"]] <- lrObj
    state[["modelSummary"]] <- modelSummary
    state[["estimatesTable"]] <- estimatesTable
    state[["estimatesTableBootstrapping"]] <- estimatesTableBootstrappingState
    state[["casewiseDiagnosticsTable"]] <- casewiseDiagnosticsTable
    state[["confusionMatrix"]] <- confusionMatrix
    state[["perfMetrics"]] <- perfMetrics
    state[["estimatesPlots"]] <- estimatesPlots
    state[["predictedPlot"]] <- predictedPlot
    state[["predictorPlots"]] <- predictorPlots
    state[["squaredPearsonPlot"]] <- squaredPearsonPlot
    state[["factorDescriptives"]] <- factorDescriptives
    
    return(list(results=results, status="complete", state=state,
                keep = plotPaths))
    
  } else {
    
    return(list(results=results, status="inited", state=state,
                keep = plotPaths))
  }
}

.lrGetPlotPaths <- function(plotObj) {
  if (is.null(plotObj)) {
    return(NULL)
  } else {
    out <- vector("list", length(plotObj[["collection"]]))
    for (i in seq_along(plotObj[["collection"]])) {
      out[[i]] <- plotObj[["collection"]][[i]][["data"]]
    }
    return(out)
  }
}

.estimatesTableBootstrapping <- function(dataset, options, perform) {
  
  out <- NULL
  out[["title"]] <- "Bootstrap Coefficients"
  
  if (options[["method"]] == "enter") {
    multimod <- FALSE
    paramtitle <- ""
  } else {
    multimod <- TRUE
    paramtitle <- "Parameter"
  }
  
  # first define all the fields
  fields <- list(
    list(name="model", title = "Model", type="string", combine = TRUE),
    list(name="param", title = paramtitle, type="string"),
    list(name="est", title = "Estimate", type="number", format="dp:3"),
    list(name="bias", title = "Bias", type="number", format="dp:3"),
    list(name="se", title = "Standard Error", type="number", format="dp:3"),
    list(name="cilo", title = "Lower bound", type="number", format="dp:3", overTitle="95% bca\u002A Confidence interval"),
    list(name="ciup", title = "Upper bound", type="number", format="dp:3", overTitle="95% bca\u002A Confidence interval")
  )
  
  if (! multimod) {
    fields <- fields[-1]
  }
  
  out[["schema"]] <- list(fields=fields)
  
  testResult <- .jaspGlm(dataset, options, perform = perform, type = "binomial")
  ci.fails <- FALSE
  
  if (perform == "run" && !is.null(testResult)) {
    ticks <- options[['coeffEstimatesBootstrappingReplicates']]
    progress <- .newProgressbar(ticks = ticks, callback = callback, response = TRUE)
    rows <- list()
    
    for (i in 1:length(testResult)) {
      
      if (! multimod && i != 2) {
        next
      }
      
      rn <- rownames(summary(testResult[[i]])[["coefficients"]])
      rn[which(rn == "(Intercept)")] <- .v("(Intercept)")
      
      
      .bootstrapping <- function(data, indices, model.formula, options) {
        pr <- progress()
        response <- .optionsDiffCheckBootstrapLogisticRegression(pr, options)
        
        if(response$status == "changed" || response$status == "aborted")
          stop("Bootstrapping options have changed")
        
        d <- data[indices, , drop = FALSE] # allows boot to select sample
        result <- glm(model.formula, family = "binomial", data = d)
        
        return(coef(result))
      }
      
      bootstrap.summary <- try(boot::boot(data = dataset, statistic = .bootstrapping,
                                          R = options$coeffEstimatesBootstrappingReplicates,
                                          model.formula = formula(testResult[[i]]),
                                          options = options),
                               silent = TRUE)
      if(inherits(bootstrap.summary, "try-error") &&
         identical(attr(bootstrap.summary, "condition")$message, "Bootstrapping options have changed"))
        return("Bootstrapping options have changed")
      
      bootstrap.coef <- matrixStats::colMedians(bootstrap.summary$t, na.rm = TRUE)
      bootstrap.bias <- colMeans(bootstrap.summary$t, na.rm = TRUE) - bootstrap.summary$t0
      bootstrap.se <- matrixStats::colSds(as.matrix(bootstrap.summary$t), na.rm = TRUE)
      
      for (j in seq_along(rn)) {
        
        result.bootstrap.ci <- try(boot::boot.ci(bootstrap.summary, type="bca", conf = 0.95, index=j))
        if(!inherits(result.bootstrap.ci, "try-error")){
          bootstrap.ci <- result.bootstrap.ci
        } else if(identical(attr(result.bootstrap.ci, "condition")$message, "estimated adjustment 'a' is NA")){
          ci.fails <- TRUE
          bootstrap.ci <- list(bca = rep(NA, 5))
        } else{
          bootstrap.ci <- result.bootstrap.ci
        }
        
        row <- list(
          model = as.character(i),
          param = .clean(.formatTerm(rn[j], testResult[[i]])),
          est = .clean(as.numeric(bootstrap.coef[j])),
          bias = .clean(as.numeric(bootstrap.bias[j])),
          se = .clean(as.numeric(bootstrap.se[j])),
          cilo = .clean(as.numeric(bootstrap.ci$bca[4])),
          ciup = .clean(as.numeric(bootstrap.ci$bca[5]))
        )
        if (j == 1) {
          row[[".isNewGroup"]] <- TRUE
        } else {
          row[[".isNewGroup"]] <- FALSE
        }
        
        rows[[length(rows) + 1]] <- row
      }
      
    }
    
  } else {
    rows <- list(
      list(model = ".", param = ".", est = ".", bias = ".", se = ".", cilo = ".", ciup = ".")
    )
  }
  
  footnotes <- .newFootnotes()
  if(ci.fails){
    .addFootnote(footnotes,
                 symbol = "<i>Note.</i>", 
                 text = "Some confidence intervals could not be computed. Possibly too few bootstrap replicates.")
  }
  .addFootnote(footnotes, symbol = "<em>Note.</em>",
               text = paste0("Bootstrapping based on ", options[['coeffEstimatesBootstrappingReplicates']], " replicates."))
  .addFootnote(footnotes, symbol = "<em>Note.</em>",
               text = "Coefficient estimate is based on the median of the bootstrap distribution.")
  .addFootnote(footnotes, symbol = "\u002A",
               text = "Bias corrected accelerated.")
  out[['footnotes']] <- as.list(footnotes)
  
  out[["data"]] <- rows
  return(out)
}

.casewiseDiagnosticsTable <- function(dataset, model, options) {
  
  casewiseDiagnostics <- list()
  casewiseDiagnostics[["title"]] <- "Casewise Diagnostics"
  
  # Declare table elements
  fields <- list(
    list(name = "caseNumber", title = "Case Number", type="integer"),
    list(name = "dependentVariable", title = "Observed", type="string"),
    list(name = "predicted", title = "Predicted", type = "number", format = "sf:4;dp:3"),
    list(name = "predictedGroup", title = "Predicted Group", type="string"),
    list(name = "residual", title = "Residual", type = "number", format = "sf:4;dp:3"),
    list(name = "residualZ", title = "Studentized Residual", type = "number", format = "sf:4;dp:3"),
    list(name = "cooksD", title = "Cook's Distance", type = "number", format = "sf:4;dp:3")
  )
  
  casewiseDiagnostics[["schema"]] <- list(fields = fields)
  
  casewiseDiagnostics.rows <- list()
  
  if (perform == "run") {
    
    if (is.null(model)) {
      
      casewiseDiagnostics.rows[[length(casewiseDiagnostics.rows)+1]] <- list(caseNumber=".", dependentVariable=".", predicted = ".", predictedGroup = ".", residual = ".", residualZ = ".", cooksD = ".")
      
    } else {
      
      casewiseDiag <- .casewiseDiagnosticsLogisticRegression(dataset, model, options)
      caseNumbers <- casewiseDiag$index
      
      if (is.na(caseNumbers)) {
        
        casewiseDiagnostics.rows[[length(casewiseDiagnostics.rows)+1]] <- list(caseNumber=".", dependentVariable=".", predicted = ".", predictedGroup = ".", residual = ".", residualZ = ".", cooksD = ".")
        
      } else {
        
        for (case in seq_along(caseNumbers))
          casewiseDiagnostics.rows[[length(casewiseDiagnostics.rows)+1]] <- list(caseNumber=caseNumbers[case], 
                                                                                 dependentVariable=casewiseDiag$dependent[case],
                                                                                 predicted = casewiseDiag$predicted[case],
                                                                                 predictedGroup = casewiseDiag$predictedGroup[case],
                                                                                 residual = casewiseDiag$residual[case],
                                                                                 residualZ = casewiseDiag$residualZ[case],
                                                                                 cooksD=casewiseDiag$cooksD[case])
      }
    }
    
  } else {
    
    # init phase
    casewiseDiagnostics.rows[[length(casewiseDiagnostics.rows)+1]] <- list(caseNumber=".", dependentVariable=".",
                                                                           predicted = ".", predictedGroup = ".",
                                                                           residual = ".", residualZ = ".", cooksD = ".")
  }
  
  casewiseDiagnostics[["data"]] <- casewiseDiagnostics.rows
  return(casewiseDiagnostics)
}


.casewiseDiagnosticsLogisticRegression <- function(dataset, model, options) {
  
  last <- length(model)
  
  # Values for all cases
  dependentAll <- dataset[[.v(options$dependent)]]
  dependentAllNumeric <- rep(0, nrow(dataset))
  dependentAllNumeric[dependentAll == levels(dataset[[.v(options$dependent)]])[2]] <- 1
  predictedAll <- predict(model[[last]], dataset, type = "response")
  predictedGroupAll <- rep(levels(dataset[[.v(options$dependent)]])[1], nrow(dataset))
  predictedGroupAll[predictedAll >= 0.5] <- levels(dataset[[.v(options$dependent)]])[2]
  residualAll <- resid(model[[last]], type = "response")
  residualZAll <- resid(model[[last]], type = "pearson")
  cooksDAll <- cooks.distance(model[[last]])
  
  # This will be the variables for the return object
  dependent <- NA
  predicted <- NA
  predictedGroup <- NA
  residual <- NA
  residualZ <- NA
  cooksD <- NA
  
  if (options$casewiseDiagnosticsType == "residualZ") {
    
    index <- which(abs(residualZAll) > options$casewiseDiagnosticsResidualZ)
    
  } else if (options$casewiseDiagnosticsType == "cooksDistance") {
    
    index <- which(abs(cooksDAll) > options$casewiseDiagnosticsCooksDistance)
    
  } else {
    
    index <- seq_along(cooksDAll)
  }
  
  if (length(index) == 0) {
    
    index <- NA
    
  } else {
    
    dependent <- dependentAll[index]
    predicted <- predictedAll[index]
    predictedGroup <- predictedGroupAll[index]
    residual <- residualAll[index]
    residualZ <- residualZAll[index]
    cooksD <- cooksDAll[index]
    
  }
  
  return(list(index=unname(index),
              dependent=as.character(dependent),
              predicted=unname(predicted),
              predictedGroup=as.character(predictedGroup),
              residual=unname(residual),
              residualZ=unname(residualZ),
              cooksD=unname(cooksD))
  )
}

.optionsDiffCheckBootstrapLogisticRegression <- function(response, options) {
  if(response$status == "changed"){
    change <- .diff(options, response$options)
    
    if(change$dependent || change$covariates || change$factors || change$wlsWeights ||
       change$modelTerms || change$coeffEstimates || change$includeIntercept ||
       change$coeffEstimatesBootstrapping ||
       change$coeffEstimatesBootstrappingReplicates)
      return(response)
    
    response$status <- "ok"
  }
  
  return(response)
}
