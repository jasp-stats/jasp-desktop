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
