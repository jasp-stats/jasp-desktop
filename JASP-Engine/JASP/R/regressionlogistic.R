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
	
	print(options)
	
	# <editor-fold> DATASET LOADING BLOCK ----
	numericVars <- unlist(c(options$covariates, 
													options$dependent, 
													options$wlsWeights))
	numericVars <- numericVars[numericVars != ""]
	factorVars <- unlist(options$factors)
	
	
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
		diff <- .diff(options, state$options)
		with(diff, {
			if (!any(dependent, covariates, factors, wlsWeights, modelTerms,
				 			 includeIntercept)) {
				# results object & model summary table can be reused
				lrObj <- state[["lrObj"]]
				modelSummary <- state[["modelSummary"]]
				
				if (!any(coeffEstimates, coeffCI, stdCoeff, oddsRatios, VovkSellkeMPR)) {
					# estimates table can be reused
					estimatesTable <- state[["estimatesTable"]]
				}
				
				if (!any(confusionMatrixOpt, confusionMatrixProportions)) {
					# confusionMatrix can be reused
					confusionMatrix <- state[["perfDiagnostics"]][["confusionMatrix"]]
				}
				if (!any(AUC, Sens, Spec, Prec, Fmsr, BrierScr, Hmsr)) {
					# metrics table can be reused
					perfMetrics <- state[["perfDiagnostics"]][["perfMetrics"]]
				}
				
				if (!any(estimatesPlotsOpt, estimatesPlotsCI)) {
					# estimates plots can be reused
					estimatesPlots <- state[["estimatesPlots"]]
				}
			}			
		})
	}
	# </editor-fold> STATE SYSTEM BLOCK
	
	
}

.readLogRegData <- function (dataset = NULL, options = list (), perform = "init", type = "binomial") {
	numeric.vars <- c (unlist(options$covariates), unlist(options$dependent))
	numeric.vars <- numeric.vars [numeric.vars != ""]

	factor.vars <- c (unlist (options$fixedFactors), unlist (options$randomFactors))
	factor.vars <- factor.vars [factor.vars != ""]

	if (is.null (dataset)) {
		if (perform == "run") {
			dataset <- .readDataSetToEnd (columns.as.numeric = numeric.vars, columns.as.factor = factor.vars,
				exclude.na.listwise = c (numeric.vars, factor.vars))
		} else {
			dataset <- .readDataSetHeader (columns.as.numeric = numeric.vars, columns.as.factor = factor.vars)
		}
	}

	return (dataset)
}
