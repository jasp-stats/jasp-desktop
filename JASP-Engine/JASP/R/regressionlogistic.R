#
# Copyright (C) 2013-2017 University of Amsterdam
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

# NB: this file has custom code folding enabled. If you're in atom, install the 
# "custom-folds" package. In other editors you might be able to define
# the <editor-fold> and </editor-fold> as start- and endpoints of a code fold.

RegressionLogistic <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
  
  # <editor-fold> DATASET LOADING BLOCK ----
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
  # </editor-fold> DATASET LOADING BLOCK
  
  # <editor-fold> STATE SYSTEM BLOCK ----
  # load state
  state <- .retrieveState()
  
  # init output variables
  lrObj <- # glm object
  modelSummary <- # fit/summary table
  estimatesTable <- # parameter estimates table
  confusionMatrix <- # confusion matrix table
  perfMetrics <- # performance metrics of full model
  estimatesPlots <- NULL # plots for estimates
  
  
  # diff check
  if (!is.null(state)) {
    diff <- .diff(options, state[["options"]])
    with(diff, {
      if (!any(dependent, covariates, factors, wlsWeights, modelTerms,
                includeIntercept)) {
        print('results object & model summary table can be reused')
        lrObj <<- state[["lrObj"]]
        modelSummary <<- state[["modelSummary"]]
        
        if (!any(coeffEstimates, coeffCI, stdCoeff, oddsRatios, VovkSellkeMPR)) {
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
        
        if (!any(estimatesPlotsOpt, estimatesPlotsCI)) {
          # estimates plots can be reused
          estimatesPlots <<- state[["estimatesPlots"]]
        }
      }      
    })
  }
  
  # </editor-fold> STATE SYSTEM BLOCK
  
  # <editor-fold> META INFORMATION BLOCK ----
  
  .pdMeta <- list(list(name = "confusionMatrix", type = "table"),
                  list(name = "perfMetrics", type = "table"))
  
  .meta <-  list(
		list(name = "title", type = "title"),
		list(name = "modelSummary", type = "table"),
		list(name = "estimatesTable", type = "table"),
		list(name = "perfDiagnostics", type = "object", meta = .pdMeta),
		list(name = "estimatesPlots", type = "image")
	)
  
  # </editor-fold> META INFORMATION BLOCK
  
  # <editor-fold> RESULTS GENERATION BLOCK ----
  # for each non-null result, generate results
  if (is.null(lrObj)) {
    print("calculating lrObj")
    lrObj <- .jaspGlm(dataset, options, perform, type = "binomial")
  }
  
  if (is.null(modelSummary)) {
    print("calculating model summary table")
    modelSummary <- .glmModelSummary(lrObj, options, perform, type = "binomial")
  }
  
  if (is.null(estimatesTable) && options[["coeffEstimates"]]) {
    
    print("calculating estimates table")
    estimatesTable <- .glmEstimatesTable(lrObj, options, perform, type = "binomial")
  }
  
  if (is.null(confusionMatrix) && options[["confusionMatrixOpt"]]) {
    confusionMatrix <- .glmConfusionMatrix(lrObj, options, perform, type = "binomial")
  }
  
  wantsPerfMetrics <- with(options, any(AUC, Sens, Spec, Prec, Fmsr, BrierScr, 
                                        Hmsr))
  if (is.null(perfMetrics) && wantsPerfMetrics) {
    perfMetrics <- .glmPerformanceMetrics(lrObj, options, perform, type = "binomial")
  }
  
  perfDiagnostics <- list("confusionMatrix" = confusionMatrix, 
                          "perfMetrics" = perfMetrics, 
                          "title" = "Performance Diagnostics")
  
  if (is.null(estimatesPlots) && options[["estimatesPlotsOpt"]]) {
    estimatesPlots <- .glmEstimatesPlots(lrObj, options, perform, type = "binomial")
  }
  
  
  results <- list()
  results[[".meta"]] <- .meta
  results[["title"]] <- "Logistic Regression"
  results[["modelSummary"]] <- modelSummary
  results[["estimatesTable"]] <- estimatesTable
  results[["perfDiagnostics"]] <- perfDiagnostics
  results[["estimatesPlots"]] <- estimatesPlots
  
  str(results)
  
  
  # </editor-fold> RESULTS GENERATION BLOCK
  
  # <editor-fold> RETURN RESULTS BLOCK ----
  
  plotPaths <- .lrGetPlotPaths(estimatesPlots)
  
  if (perform == "run") {
    state <- list()
    state[["options"]] <- options
    state[["lrObj"]] <- lrObj
    state[["modelSummary"]] <- modelSummary  
    state[["estimatesTable"]] <- estimatesTable
    state[["confusionMatrix"]] <- confusionMatrix
    state[["perfMetrics"]] <- perfMetrics
    state[["estimatesPlots"]] <- estimatesPlots
    
    return(list(results=results, status="complete", state=state,
                keep = plotPaths))

  } else {
    
    return(list(results=results, status="inited", state=state,
                keep = plotPaths))

  }
  # </editor-fold> RETURN RESULTS BLOCK
  
}

.lrGetPlotPaths <- function(estPlots) {
  if (is.null(estPlots)) {
    return(NULL)
  } else {
    out <- vector(list, length(estPlots[["collection"]]))
    for (i in seq_along(estPlots[["collection"]])) {
      out[[i]] <- estPlots[["collection"]][[i]][["data"]]
    }
    return(out)
  }
}
