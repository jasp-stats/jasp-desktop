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

RegressionLinear <- function(jaspResults, dataset, options, state = NULL) {

  # Define state if empty
  if (is.null(state)) {
    state <- list()
  }
  
  # Add main effects and interaction effects to options
  for (modelTerm in options$modelTerms) {
    if (length(modelTerm$components) == 1 && modelTerm$components[[1]] %in% options$covariates) {
      options[["main.effects.numeric"]]        <- c(options[["main.effects.numeric"]], modelTerm$components[[1]])
      options[["main.effects.numeric.base64"]] <- c(options[["main.effects.numeric.base64"]], .v(modelTerm$components[[1]]))
    } else if (length(modelTerm$components) == 1 && modelTerm$components[[1]] %in% options$factors) {
      options[["main.effects.factors"]]        <- c(options[["main.effects.factors"]], modelTerm$components[[1]])
      options[["main.effects.factors.base64"]] <- c(options[["main.effects.factors.base64"]], .v(modelTerm$components[[1]]))
    } else if (length(modelTerm$components) > 1) {
      options[["interaction.effects"]]         <- c(options[["interaction.effects"]], 
                                                    paste0(unlist(modelTerm$components), collapse=":"))
      options[["interaction.effects.base64"]]  <- c(options[["interaction.effects.base64"]],
                                                    paste0(.v(unlist(modelTerm$components)), collapse=":"))
    }
  }
  
  # Define model terms
  if (options$includeConstant == TRUE) {
    options[["model.terms"]] <- c("(Intercept)", options$main.effects.numeric.base64, options$interaction.effects.base64)
  } else {
    options[["model.terms"]] <- c(options$main.effects.numeric.base64, options$interaction.effects.base64)
  }
  
  # Add dependent variable and weights to options
  if (options$dependent != "") {
    options[["dependent.variable"]]        <- options$dependent
    options[["dependent.variable.base64"]] <- .v(options$dependent)
  } else {
    options[["dependent.variable"]]        <- NULL
  }
  
  if (options$wlsWeights != "") {
    options[["weights"]]                   <- options$wlsWeights
  } else {
    options[["weights"]]                   <- NULL
  }
  
  # Read dataset (including weights)
	dataset <- .readDataSetToEnd(	columns.as.factor   = options$main.effects.factors, 
		                            columns.as.numeric  = c(options$dependent.variable, options$main.effects.numeric, 
		                                                    options$weights),
		                            exclude.na.listwise = c(options$dependent.variable, options$main.effects.numeric, 
		                                                    options$main.effects.factors, options$weights))
	
	# Set title
	jaspResults$title <- "Linear Regression"
	
	# Check if results can be computed
	ready <- (options$dependent != "" && length(options$modelTerms) > 0)
	
	# Check for errors
	if (ready) {
	  
	  # Error check 1: Check for nonpositive weights
	  if (! is.null(options$weights)) {
	    .hasErrors(dataset, perform = "run", type = c('nonPositive'), all.target = options$weights, exitAnalysisIfErrors = TRUE)
	  }
	  
	  # Error check 2: Check for too few observations, variables with zero variance, and infinite values
	  .hasErrors(dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
	             all.target = c(options$dependent.variable, options$main.effects.numeric, 
	                            options$main.effects.factors, options$weights),
	             observations.amount = c('< 3'), exitAnalysisIfErrors = TRUE)
	  
	  # Error check 3: Check for non-positive definite variance-covariance matrix
	  if (length(options$main.effects.numeric) > 0) {
	    if (! is.null(options$weights)) {
	      # If we have weights, take this into account for the variance-covariance matrix
	      covwt <- function(...) return(stats::cov.wt(..., wt=dataset[[.v(options$weights)]])$cov)
	      # Do not include factors in the variance-covariance matrix
	      errors <- .hasErrors(dataset[, -which(colnames(dataset) %in% c(.v(options$weights)))], 
	                           perform = "run", type = c('varCovData'), exitAnalysisIfErrors = TRUE, 
	                           varCovData.target = c(options$dependent.variable, options$main.effects.numeric), 
	                           varCovData.corFun = covwt)
	    }
	    else {
	      covnwt <- stats::cov
	      # Do not include factors in the variance-covariance matrix
	      errors <- .hasErrors(dataset, perform = "run", type = c('varCovData'), exitAnalysisIfErrors = TRUE, 
	                           varCovData.target = c(options$dependent.variable, options$main.effects.numeric),
	                           varCovData.corFun = covnwt)
	    }
	  }
	  
	  # Error check 4: Check value of stepping method criteria if a stepwise method is used
	  if (options$method != "enter") {
	    
	    if (options$steppingMethodCriteriaType == "usePValue" && 
	        options$steppingMethodCriteriaPEntry > options$steppingMethodCriteriaPRemoval) {
	      
	      .quitAnalysis(message = "Error in Stepping Method Criteria: Entry p-value needs to be smaller than removal p-value")
	      
	    } else if (options$steppingMethodCriteriaType == "useFValue" && 
	               options$steppingMethodCriteriaFEntry < options$steppingMethodCriteriaFRemoval) {
	      
	      .quitAnalysis(message = "Error in Stepping Method Criteria: Entry F-value needs to be larger than removal F-value")
	    }
	  }
	  
	  # Error check 5: Check for interactin effects if a stepwise method is used
	  if (options$method != "enter" && ! is.null(options$interaction.effects)) {
	    .quitAnalysis(message = "Stepwise procedures are not supported for models containing interaction terms")
	  }
	}
	
	# Update weights variable
	if (! is.null(options$weights)) {
	  colnames(dataset)[which(names(dataset) == .v(options$weights))] <- .v("weights")
	} else {
	  dataset[[.v("weights")]] <- rep(1, nrow(dataset))
	}
	options[["weights"]]                   <- "weights"
	options[["weights.base64"]]            <- .v("weights")
	
	# Fit Linear Model
	linearModels <- .fitLinearRegressionModel(dataset = dataset, options = options, ready = ready)
	
	# Create Model Summary Table
	.createLinearRegressionModelSummaryTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready,
	                                         linearModels = linearModels)
	
	# Create Anova Table (if wanted)
	if (options$modelFit) {
	  .createLinearRegressionAnovaTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready,
	                                    linearModels = linearModels)
	}

	# Create Coefficients Table (if wanted)
	if (options$regressionCoefficientsEstimates) {
	  .createLinearRegressionCoefficientsTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready, 
	                                           linearModels = linearModels)
	}
	
	# Create Bootstrapping Coefficients Table (if wanted)
	if (options$regressionCoefficientsBootstrapping) {
	  .createLinearRegressionBootstrappingCoefficientsTable(jaspResults = jaspResults, dataset = dataset, options = options, 
	                                                        ready = ready)
	}
	
	# Create Descriptives Table (if wanted)
	if (options$descriptives) {
	  .createLinearRegressionDescriptivesTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready)
	}
	
	# Create Part and Partial Correlation Table (if wanted)
	if (options$partAndPartialCorrelations) {
	  .createLinearRegressionPartAndPartialCorrelationTable(jaspResults = jaspResults, dataset = dataset, options = options, 
	                                                        ready = ready)
	}
	
	# Create Coefficients Covariance Matrix Table (if wanted)
	if (options$regressionCoefficientsCovarianceMatrix) {
	  .createLinearRegressionCovarianceMatrixTable(jaspResults = jaspResults, dataset = dataset, options = options, 
	                                               ready = ready)
	}
	
	# Create Collinearity Diagnostics Table (if wanted)
	if (options$collinearityDiagnostics) {
	  .createLinearRegressionCollinearityDiagnosticsTable(jaspResults = jaspResults, dataset = dataset, options = options, 
	                                                      ready = ready)
	}
	
	# Create Casewise Diagnostics Table (if wanted)
	if (options$residualsCasewiseDiagnostics) {
	  .createLinearRegressionCasewiseDiagnosticsTable(jaspResults = jaspResults, dataset = dataset, options = options, 
	                                                  ready = ready)
	}
	
	# Create Residuals Statistics Table (if wanted)
	if (options$residualsDurbinWatson) {
	  .createLinearRegressionResidualsStatisticsTable(jaspResults = jaspResults, dataset = dataset, options = options, 
	                                                  ready = ready)
	}
	
	# Create Assumption Checks Container (if wanted)
	if (options$plotResidualsDependent || options$plotResidualsCovariates || options$plotResidualsPredicted ||
	    options$plotResidualsHistogram || options$plotResidualsQQ || options$plotsPartialRegression) {
	  .createLinearRegressionAssumptionsChecksContainer(jaspResults = jaspResults, dataset = dataset, options = options, 
	                                                    ready = ready)
	}
	
	# Bring state up-to-date
	state[["options"]] <- options
	
	return(state = state)
}

.fitLinearRegressionModel <- function(dataset, options, ready) {
  
  # If analysis cannot be conducted, return empty model
  if (ready == FALSE) {
    return(list())
  }
  
  lm.model <- list()
  empty.model <- list(lm.fit = NULL, variables = NULL)
  lm.fit.index.one.model <- 1
  includes.nuisance <- FALSE
  
  variables.in.model <- NULL
  variables.in.model.base64 <- NULL
  variables.in.null.model <- NULL
  variables.in.null.model.base64 <- NULL
    
  for (i in seq_along(options$modelTerms)) {
      
    components <- options$modelTerms[[i]]$components
    nuisance <- options$modelTerms[[i]]$isNuisance
      
    if (length(components) == 1) {
        
      variables.in.model <- c(variables.in.model, components[[1]])
      variables.in.model.base64 <- c(variables.in.model.base64, .v(components[[1]]))
        
    } else {
      components.unlisted <- unlist(components)
      term.base64 <- paste0(.v(components.unlisted), collapse=":")
      term <- paste0(components.unlisted, collapse=":")
      variables.in.model <- c(variables.in.model, term)
      variables.in.model.base64 <- c(variables.in.model.base64, term.base64)
    }
      
    if (!is.null(nuisance) && nuisance) {
      if (length(components) == 1) {
        variables.in.null.model <- c(variables.in.null.model, components[[1]])
        variables.in.null.model.base64 <- c(variables.in.null.model.base64, .v(components[[1]]))
      } else {
        variables.in.null.model <- c(variables.in.null.model, term)
        variables.in.null.model.base64 <- c(variables.in.null.model.base64, term.base64)
      }
    }
      
  }
    
  independent.base64 <- variables.in.model.base64
  independent.null.base64 <- variables.in.null.model.base64
  variables.in.model <- variables.in.model[ variables.in.model != ""]
  variables.in.null.model <- variables.in.null.model[ variables.in.null.model != ""]
  variables.in.model.copy <- variables.in.model
  includes.nuisance <- (length(variables.in.null.model) > 0)
  lm.fit.index.one.model <- 1 + as.numeric(includes.nuisance && (!identical(variables.in.model,variables.in.null.model)))
  
  dependent.base64 <- .v(options$dependent)
    
  if (options$wlsWeight != "" ) {
    weight.base64 <- .v(options$wlsWeight)
    weights <- dataset[[ weight.base64 ]]
  } else {
    weights <- rep(1,length(dataset[[ dependent.base64 ]] ))
  }
    
  if (options$method == "backward") {
    lm.model <- .backwardRegression(dependent.base64, independent.base64, independent.null.base64, dataset, options, weights)
  }
  
  if (options$method == "forward") {
    lm.model <- .forwardRegression(dependent.base64, independent.base64, independent.null.base64, dataset, options, weights)
  }
  
  if (options$method == "stepwise") {
    lm.model <- .stepwiseRegression(dependent.base64, independent.base64, independent.null.base64, dataset, options, weights)
  }
  
  if (options$method != "enter") {
    
    if (includes.nuisance) {
      
      if (options$includeConstant == TRUE) {
        null.model.definition <- paste(dependent.base64, "~", paste(independent.null.base64, collapse = "+"))
      } else {
        null.model.definition <- paste(dependent.base64, "~", paste(independent.null.base64, collapse = "+"), "-1")
      }
      
      null.model.formula <- as.formula(null.model.definition)
      lm.fit.null <- try( stats::lm( null.model.formula, data = dataset, weights = weights, x=TRUE ), silent = TRUE)
      if ( !identical((lm.model[[1]][[1]]$coefficients), lm.fit.null$coefficients)) {
        lm.model[(1:length(lm.model))+1] <- lm.model[1:length(lm.model)]
        lm.model[[1]] <- list(lm.fit = lm.fit.null, variables = variables.in.null.model)
      }
    }
  }
        
  if (options$method == "enter") {
    
    if (length(variables.in.model) > 0 ) {
        
      if (options$includeConstant == TRUE) {
        model.definition <- paste(dependent.base64, "~", paste(independent.base64, collapse = "+"))
        null.model.definition <- paste(dependent.base64, "~", paste(independent.null.base64, collapse = "+"))
      } else {
        model.definition <- paste(dependent.base64, "~", paste(independent.base64, collapse = "+"), "-1")
        null.model.definition <- paste(dependent.base64, "~", paste(independent.null.base64, collapse = "+"), "-1")
      }
        
    } else {
        
      if (options$includeConstant == TRUE) {
          model.definition <- paste(dependent.base64, "~ 1")
      } else {
        model.definition <- NULL #this model has no parameters
      }
    }
      
      
    if (!is.null(model.definition)) {
      
      model.formula <- as.formula(model.definition)
      lm.fit <- try( stats::lm( model.formula, data = dataset, weights = weights, x=TRUE ), silent = TRUE)
        
      if (includes.nuisance) {
        null.model.formula <- as.formula(null.model.definition)
        lm.fit.null <- try( stats::lm( null.model.formula, data = dataset, weights = weights, x=TRUE ), silent = TRUE)
      }
        
      if ( class(lm.fit) == "lm") {
          
        lm.model[[lm.fit.index.one.model]] <- list(lm.fit = lm.fit, variables = variables.in.model)
        if(includes.nuisance && class(lm.fit.null) == "lm" ){ 
          lm.model[[1]] <- list(lm.fit = lm.fit.null, variables = variables.in.null.model)
        }
          
      } else {
          
        list.of.errors[[ length(list.of.errors) + 1 ]]  <- "An unknown error occurred, please contact the author."
        lm.model[[lm.fit.index.one.model]] <- list(lm.fit = NULL, variables = variables.in.model)
        if (includes.nuisance) { 
          lm.model[[1]] <- list(lm.fit = NULL, variables = variables.in.null.model)
        }
      }
        
    } else {
        
      lm.model[[lm.fit.index.one.model]] <- list(lm.fit = NULL, variables = variables.in.model)
      if (includes.nuisance) { 
        lm.model[[1]] <- list(lm.fit = NULL, variables = variables.in.null.model)
      }
    }
  }
  
  return(lm.model)
}

.createLinearRegressionModelSummaryTable <- function(jaspResults, dataset, options, ready, linearModels) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["linearRegressionModelSummaryTable"]])) {
    return(NULL)
  }
  
  # Create table
  linearRegressionModelSummaryTable <- createJaspTable(title = "Model Summary")
  jaspResults[["linearRegressionModelSummaryTable"]] <- linearRegressionModelSummaryTable
  #linearRegressionModelSummaryTable$showSpecifiedColumnsOnly <- TRUE
  linearRegressionModelSummaryTable$dependOnOptions(c("dependent", "wlsWeights", "modelTerms", "rSquaredChange",
                                                      "residualsDurbinWatson", "missingValues"))
  
  # Add columns to table
  linearRegressionModelSummaryTable$addColumnInfo(  name = "modelNo",   title = "Model",            type = "integer")
  linearRegressionModelSummaryTable$addColumnInfo(  name = "R",         title = "R",                type = "number",
                                                    format = "sf:4;dp:3")
  linearRegressionModelSummaryTable$addColumnInfo(  name = "R2",        title = "R\u00B2",          type = "number",
                                                    format = "sf:4;dp:3")
  linearRegressionModelSummaryTable$addColumnInfo(  name = "aR2",       title = "Adjusted R\u00B2", type = "number",
                                                    format = "sf:4;dp:3")
  linearRegressionModelSummaryTable$addColumnInfo(  name = "rmse",      title = "RMSE",             type = "number",
                                                    format = "sf:4;dp:3")
  if (options$rSquaredChange == TRUE) {
    linearRegressionModelSummaryTable$addColumnInfo(name = "R2.change", title = "R\u00B2 Change",   type = "number",
                                                    format = "sf:4;dp:3")
    linearRegressionModelSummaryTable$addColumnInfo(name = "F.change",  title = "F Change",         type = "number",
                                                    format = "sf:4;dp:3")
    linearRegressionModelSummaryTable$addColumnInfo(name = "df1",       title = "df1",              type = "integer")
    linearRegressionModelSummaryTable$addColumnInfo(name = "df2",       title = "df2",              type = "integer")
    linearRegressionModelSummaryTable$addColumnInfo(name = "p",         title = "p",                type = "number",
                                                    format = "sf:4;dp:3")
  }
  if (options$residualsDurbinWatson == TRUE) {
    linearRegressionModelSummaryTable$addColumnInfo(name = "durWat",    title = "Durbin-Watson",    type = "number", 
                                                    format = "sf:4;dp:3")
  }
      
  # Fill up table with results
  .fillUpLinearRegressionModelSummaryTable(linearRegressionModelSummaryTable = linearRegressionModelSummaryTable,
                                    dataset = dataset, options = options, ready = ready, linearModels = linearModels)
  
  return(NULL)
}

.fillUpLinearRegressionModelSummaryTable <- function(linearRegressionModelSummaryTable, dataset, options, ready, 
                                                     linearModels) {
  
  # If results can be computed, compute them and add row for each variable
  if (ready) {
    for (modelNo in 1:length(linearModels)) {
      .addRowForLinearRegressionModelSummaryTable(linearRegressionModelSummaryTable = linearRegressionModelSummaryTable,
                                                  dataset = dataset, options = options, modelNo = modelNo,
                                                  linearModels = linearModels)
    }
  # If results cannot be computed, add an empty row
  } else {
    row <- list(modelNo = ".", R = ".", R2 = ".", aR2 = ".", rmse = ".", R2.change = ".", F.change = ".", df1 = ".", 
                df2 = ".", p = ".", durWat = ".")
    linearRegressionModelSummaryTable$addRows(rows = row)
  }
  
  return(NULL)
}

.addRowForLinearRegressionModelSummaryTable <- function(linearRegressionModelSummaryTable, dataset, options, modelNo, 
                                                        linearModels) {
  
  model      <- linearModels[[modelNo]]
  lm.summary <- summary(model$lm.fit)
  
  if (modelNo > 1) {
    model.old      <- linearModels[[modelNo - 1]]
    lm.summary.old <- summary(linearModels[[modelNo - 1]]$lm.fit)
  }
    
  R       <- sqrt(lm.summary$r.squared)
  R2      <- lm.summary$r.squared
  aR2     <- lm.summary$adj.r.squared
  rmse    <- lm.summary$sigma
    
  if (modelNo == 1) {
    R2.old  <- 0
    df1 <- abs(length(model$variables))
  } else {
    R2.old  <- lm.summary.old$r.squared
    df1 <- abs(length(model$variables) - length(model.old$variables) )
  }
    
  R2.change     <- R2 - R2.old
  df2 <- length( dataset[[ options$dependent.variable.base64 ]]) - length(model$variables) - 1
  F.change <- (df2 * R2.change) / (df1 * (1 - R2))
    
  p <- pf(q = F.change, df1 = df1, df2 = df2, lower.tail = FALSE )
    
  if (modelNo == length(linearModels)) {
    durWat <- car::durbinWatsonTest(model$lm.fit)$dw
  } else {
    durWat <- ""
  }
  
  # Add row to the table
  row <- list(modelNo = modelNo, R = R, R2 = R2, aR2 = aR2, rmse = rmse, R2.change = R2.change, F.change = F.change, 
              df1 = df1, df2 = df2, p = p, durWat = durWat)
  linearRegressionModelSummaryTable$addRows(rows = row, rowNames = modelNo)
}

.createLinearRegressionAnovaTable <- function(jaspResults, dataset, options, ready, linearModels) {

  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["linearRegressionAnovaTable"]])) {
    return(NULL)
  }
  
  # Create table
  linearRegressionAnovaTable <- createJaspTable(title = "ANOVA")
  jaspResults[["linearRegressionAnovaTable"]] <- linearRegressionAnovaTable
  #linearRegressionAnovaTable$showSpecifiedColumnsOnly <- TRUE
  linearRegressionAnovaTable$dependOnOptions(c("dependent", "wlsWeights", "modelTerms", "VovkSellkeMPR", "missingValues"))
  
  # Add columns to table
  linearRegressionAnovaTable$addColumnInfo(  name = "modelNo",       title = "Model",          type = "integer",
                                             combine = TRUE)
  linearRegressionAnovaTable$addColumnInfo(  name = "type",          title = "",               type = "string")
  linearRegressionAnovaTable$addColumnInfo(  name = "ssq",           title = "Sum of Squares", type = "number",
                                             format = "sf:4;dp:3")
  linearRegressionAnovaTable$addColumnInfo(  name = "df",            title = "df",             type = "integer")
  linearRegressionAnovaTable$addColumnInfo(  name = "msq",           title = "Mean Square",    type = "number",
                                             format = "sf:4;dp:3")
  linearRegressionAnovaTable$addColumnInfo(  name = "F",             title = "F",              type = "number",
                                             format = "sf:4;dp:3")
  linearRegressionAnovaTable$addColumnInfo(  name = "p",             title = "p",              type = "number",
                                             format = "sf:4;dp:3")
  if (options$VovkSellkeMPR) {
    linearRegressionAnovaTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A",   type = "number",
                                             format = "sf:4;dp:3")
  }
  
  # Fill up table with results
  .fillUpLinearRegressionAnovaTable(linearRegressionAnovaTable = linearRegressionAnovaTable, dataset = dataset, 
                                    options = options, ready = ready, linearModels = linearModels)
  
  return(NULL)
}

.fillUpLinearRegressionAnovaTable <- function(linearRegressionAnovaTable, dataset, options, ready, linearModels) {
  
  # If results can be computed, compute them and add row for each variable
  if (ready) {
    for (modelNo in 1:length(linearModels)) {
      .addRowForLinearRegressionAnovaTable(linearRegressionAnovaTable = linearRegressionAnovaTable, dataset = dataset, 
                                           options = options, modelNo = modelNo, linearModels = linearModels)
    }
    
    if (options$VovkSellkeMPR) {
      linearRegressionAnovaTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
    }
  # If results cannot be computed, add an empty row
  } else {
    row <- list(modelNo = ".", type = ".", ssq = ".", df = ".", msq = ".", F = ".", p = ".", VovkSellkeMPR = ".")
    linearRegressionAnovaTable$addRows(rows = row)
  }
  
  return(NULL)
}

.addRowForLinearRegressionAnovaTable <- function(linearRegressionAnovaTable, dataset, options, modelNo, linearModels) {
  
  model      <- linearModels[[modelNo]]
  lm.summary <- summary(model$lm.fit)
  
  # Compute results
  F			       <- lm.summary$fstatistic[1]
  msq.residual <- (lm.summary$sigma) ^2
  msq.model	   <- F * msq.residual
  df.residual	 <- lm.summary$fstatistic[3]
  df.model		 <- lm.summary$fstatistic[2]
  df.total		 <- df.residual + df.model
  ssq.residual <- msq.residual * df.residual
  ssq.model		 <- msq.model * df.model
  ssq.total		 <- ssq.residual + ssq.model
  p            <- pf(q = F, df1 = df.model, df2 = df.residual, lower.tail = FALSE )
  
  # Add "Regression" row to the table
  row <- list(modelNo = modelNo, type = "Regression", ssq = .clean(ssq.model), df = .clean(df.model), 
              msq = .clean(msq.model), F = .clean(F), p = .clean(p), VovkSellkeMPR = .clean(.VovkSellkeMPR(p)))
  linearRegressionAnovaTable$addRows(rows = row, rowNames = "Regression")
  
  # Add "Residual" row to the table
  row <- list(modelNo = modelNo, type = "Residual", ssq = .clean(ssq.residual), df = .clean(df.residual), 
              msq = .clean(msq.residual), F = "", p = "", VovkSellkeMPR = "")
  linearRegressionAnovaTable$addRows(rows = row, rowNames = "Residual")
  
  # Add "Total" row to the table
  row <- list(modelNo = modelNo, type = "Total", ssq = .clean(ssq.total), df = .clean(df.total), 
              msq = "", F = "", p = "", VovkSellkeMPR = "")
  linearRegressionAnovaTable$addRows(rows = row, rowNames = "Total")
}

.createLinearRegressionCoefficientsTable <- function(jaspResults, dataset, options, ready, linearModels) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["linearRegressionCoefficientsTable"]])) {
    return(NULL)
  }
  
  # Create table
  linearRegressionCoefficientsTable <- createJaspTable(title = "Coefficients")
  jaspResults[["linearRegressionCoefficientsTable"]] <- linearRegressionCoefficientsTable
  #linearRegressionCoefficientsTable$showSpecifiedColumnsOnly <- TRUE
  linearRegressionCoefficientsTable$dependOnOptions(c("dependent", "wlsWeights", "modelTerms", "VovkSellkeMPR", 
                                                      "regressionCoefficientsEstimates", "missingValues",
                                                      "regressionCoefficientsConfidenceIntervals",
                                                      "regressionCoefficientsConfidenceIntervalsInterval",
                                                      "collinearityDiagnostics", "includeConstant",
                                                      "method", "steppingMethodCriteriaType", 
                                                      "steppingMethodCriteriaPEntry", "steppingMethodCriteriaPRemoval", 
                                                      "steppingMethodCriteriaFEntry", "steppingMethodCriteriaFRemoval"))
  
  # Add columns to table
  linearRegressionCoefficientsTable$addColumnInfo(  name = "modelNo",       title = "Model",          type = "integer",
                                                    combine = TRUE)
  linearRegressionCoefficientsTable$addColumnInfo(  name = "variable",      title = "",               type = "string")
  linearRegressionCoefficientsTable$addColumnInfo(  name = "unst.coef",     title = "Unstandardized", type = "number",
                                                    format = "sf:4;dp:3")
  linearRegressionCoefficientsTable$addColumnInfo(  name = "se",            title = "SE",             type = "number",
                                                    format = "sf:4;dp:3")
  linearRegressionCoefficientsTable$addColumnInfo(  name = "stan.coef",     title = "Standardized",   type = "number",
                                                    format = "sf:4;dp:3")
  linearRegressionCoefficientsTable$addColumnInfo(  name = "t",             title = "t",              type = "number",
                                                    format = "sf:4;dp:3")
  linearRegressionCoefficientsTable$addColumnInfo(  name = "p",             title = "p",              type = "number",
                                                    format = "sf:4;dp:3")
  if (options$VovkSellkeMPR) {
    linearRegressionCoefficientsTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A",   type = "number",
                                                    format = "sf:4;dp:3")
  }
  if (options$regressionCoefficientsConfidenceIntervals == TRUE) {
    linearRegressionCoefficientsTable$addColumnInfo(name = "lowerCI",       title = "Lower",          type = "number",
                                                    format = "sf:4;dp:3",   overtitle = paste0(options$regressionCoefficientsConfidenceIntervalsInterval, "% CI"))
    linearRegressionCoefficientsTable$addColumnInfo(name = "upperCI",       title = "Upper",          type = "number",
                                                    format = "sf:4;dp:3",   overtitle = paste0(options$regressionCoefficientsConfidenceIntervalsInterval, "% CI"))
  }
  if (options$collinearityDiagnostics) {
    linearRegressionCoefficientsTable$addColumnInfo(name = "tolerance",     title = "Tolerance",      type = "number",
                                                    format = "sf:4;dp:3",   overtitle = "Collinearity Statistics")
    linearRegressionCoefficientsTable$addColumnInfo(name = "vif",           title = "VIF",            type = "number",
                                                    format = "sf:4;dp:3",   overtitle = "Collinearity Statistics")
  }

  # Fill up table with results
  .fillUpLinearRegressionCoefficientsTable(linearRegressionCoefficientsTable = linearRegressionCoefficientsTable, 
                                           dataset = dataset, options = options, ready = ready, linearModels = linearModels)
  
  return(NULL)
}
  
.fillUpLinearRegressionCoefficientsTable <- function(linearRegressionCoefficientsTable, dataset, options, ready, 
                                                     linearModels) {
  
  # If results can be computed, compute them and add row for each variable
  if (ready) {
    for (modelNo in 1:length(linearModels)) {
      lm.model <- linearModels[[modelNo]]$lm.fit
      lm.summary <- summary(lm.model)
      lm.estimates <- lm.summary$coefficients
      lm.confidence.interval <- confint(lm.model, level = options$regressionCoefficientsConfidenceIntervalsInterval / 100)
      lm.collinearity.diagnostics <- .collinearityDiagnostics(lm.model, dataset, includeConstant = options$includeConstant)
      
      for (term in rownames(lm.estimates)) {
        results <- lm.estimates[term, ]
        resultsCI <- lm.confidence.interval[term, ]
        resultsCD <- list(tolerance = lm.collinearity.diagnostics$tolerance[[term]], 
                          vif = lm.collinearity.diagnostics$VIF[[term]])
        .addRowForLinearRegressionCoefficientsTable(linearRegressionCoefficientsTable = linearRegressionCoefficientsTable, 
                                                    dataset = dataset, options = options, modelNo = modelNo, term = term, 
                                                    results = results, resultsCI = resultsCI, resultsCD = resultsCD)
      }
    }
    if (options$VovkSellkeMPR) {
      linearRegressionCoefficientsTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
    }
  # If results cannot be computed, add an empty row
  } else {
    row <- list(modelNo = ".", variable = ".", unst.coef = ".", se = ".", stan.coef = ".", t = ".", p = ".", 
                VovkSellkeMPR = ".", lowerCI = ".", upperCI = ".", tolerance = ".", vif = ".")
    linearRegressionCoefficientsTable$addRows(rows = row)
  }
  
  return(NULL)
}

.addRowForLinearRegressionCoefficientsTable <- function(linearRegressionCoefficientsTable, dataset, options, modelNo, 
                                                        term, results, resultsCI, resultsCD) {
  
  unst.coef <- results[1]
  se        <- results[2]
  t         <- results[3]
  p         <- results[4]
  lowerCI   <- resultsCI[1]
  upperCI   <- resultsCI[2]
  if (term == "(Intercept") {
    stan.coef <- "."
    tolerance <- "."
    vif       <- "."
    variable  <- term
  } else {
    if (grepl(":", term)) {
      vars <- unlist(strsplit(term, split = ":"))
      int.var <- rep(1, nrow(dataset))
      for (var in vars) {
        int.var <- int.var * dataset[[ var ]]
      }
      sd.ind <- sd(int.var)
      variable <- paste0(.unv(vars), collapse="\u2009\u273b\u2009")
    } else {
      sd.ind    <- sd( dataset[[ term ]] )
      variable <- .unv(term)
    }
    sd.dep    <- sd( dataset[[ options[["dependent.variable.base64"]] ]] )
    stan.coef <- unst.coef * sd.ind / sd.dep
    tolerance <- resultsCD$tolerance
    vif       <- resultsCD$vif
  }
  
  # Add row
  row <- list(modelNo = modelNo, variable = variable, unst.coef = .clean(unst.coef), se = .clean(se), 
              stan.coef = .clean(stan.coef), t = .clean(t), p = .clean(p), VovkSellkeMPR = .clean(.VovkSellkeMPR(p)),
              lowerCI = .clean(lowerCI), upperCI = .clean(upperCI), tolerance = .clean(tolerance), vif = .clean(vif))
  linearRegressionCoefficientsTable$addRows(rows = row, rowNames = paste0(modelNo, " - ", variable))
  
  return(NULL)
}

.createLinearRegressionBootstrappingTable <- function(jaspResults, dataset, options, ready, linearModels) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["linearRegressionBootstrappingTable"]])) {
    return(NULL)
  }
  
  # Create table
  linearRegressionBootstrappingTable <- createJaspTable(title = "Coefficients")
  jaspResults[["linearRegressionBootstrappingTable"]] <- linearRegressionBootstrappingTable
  #linearRegressionBootstrappingTable$showSpecifiedColumnsOnly <- TRUE
  linearRegressionBootstrappingTable$dependOnOptions(c("dependent", "wlsWeights", "modelTerms", 
                                                      "regressionCoefficientsBootstrapping", "missingValues",
                                                      "regressionCoefficientsBootstrappingReplicates", "includeConstant",
                                                      "method", "steppingMethodCriteriaType", 
                                                      "steppingMethodCriteriaPEntry", "steppingMethodCriteriaPRemoval", 
                                                      "steppingMethodCriteriaFEntry", "steppingMethodCriteriaFRemoval"))
  
  # Add columns to table
  linearRegressionBootstrappingTable$addColumnInfo(  name = "modelNo",       title = "Model",          type = "integer",
                                                     combine = TRUE)
  linearRegressionBootstrappingTable$addColumnInfo(  name = "variable",      title = "",               type = "string")
  linearRegressionBootstrappingTable$addColumnInfo(  name = "unst.coef",     title = "Unstandardized", type = "number",
                                                     format = "sf:4;dp:3")
  linearRegressionBootstrappingTable$addColumnInfo(  name = "bias",          title = "Bias",           type = "number",
                                                     format = "sf:4;dp:3")
  linearRegressionBootstrappingTable$addColumnInfo(  name = "se",            title = "SE",             type = "number",
                                                     format = "sf:4;dp:3")
  linearRegressionBootstrappingTable$addColumnInfo(  name = "lowerCI",       title = "Lower",          type = "number",
                                                     format = "sf:4;dp:3",   overtitle = paste0("95% CI"))
  linearRegressionBootstrappingTable$addColumnInfo(  name = "upperCI",       title = "Upper",          type = "number",
                                                     format = "sf:4;dp:3",   overtitle = paste0("95% CI"))

  # Fill up table with results
  .fillUpLinearRegressionBootstrappingTable(linearRegressionBootstrappingTable = linearRegressionBootstrappingTable,
                                            dataset = dataset, options = options, ready = ready, linearModels = linearModels)
  
  return(NULL)
}

.fillUpLinearRegressionBootstrappingTable <- function(linearRegressionBootstrappingTable, dataset, options, ready,
                                                      linearModels) {
  
  # If results can be computed, compute them and add row for each variable
  if (ready) {
    for (modelNo in 1:length(linearModels)) {
      lm.model <- linearModels[[modelNo]]$lm.fit
      lm.summary <- summary(lm.model)
      lm.estimates <- lm.summary$coefficients
      lm.confidence.interval <- confint(lm.model, level = options$regressionCoefficientsConfidenceIntervalsInterval / 100)
      lm.collinearity.diagnostics <- .collinearityDiagnostics(lm.model, dataset, includeConstant = options$includeConstant)
      
      for (term in rownames(lm.estimates)) {
        results <- lm.estimates[term, ]
        resultsCI <- lm.confidence.interval[term, ]
        resultsCD <- list(tolerance = lm.collinearity.diagnostics$tolerance[[term]], 
                          vif = lm.collinearity.diagnostics$VIF[[term]])
        .addRowForLinearRegressionCoefficientsTable(linearRegressionCoefficientsTable = linearRegressionCoefficientsTable, 
                                                    dataset = dataset, options = options, modelNo = modelNo, term = term, 
                                                    results = results, resultsCI = resultsCI, resultsCD = resultsCD)
      }
    }
  # If results cannot be computed, add an empty row
  } else {
    row <- list(modelNo = ".", variable = ".", unst.coef = ".", bias = ".", se = ".", lowerCI = ".", upperCI = ".")
    linearRegressionBootstrappingTable$addRows(rows = row)
  }
  
  return(NULL)
}



      for (m in 1:length(lm.model)) {
        
        if ( class(lm.model[[ m ]]$lm.fit) == "lm" && (! (length(lm.model[[m]]$variables) == 0 && options$includeConstant == FALSE))) {
          
          na.estimate.names <- NULL
          
          if(any(is.na(lm.model[[m]]$lm.fit$coefficients))){
            
            #these estimates give back NA
            na.estimate.names <- names(lm.model[[m]]$lm.fit$coefficients)[which(is.na(lm.model[[m]]$lm.fit$coefficients))]
            # !!!!! if(all(is.na(tmp)))
          }
          
          .bootstrapping <- function(data, indices, formula, wlsWeights) {
            d <- data[indices, , drop = FALSE] # allows boot to select sample
            if (.unv(wlsWeights) == "") {
              fit <- lm(formula = formula, data=d)
            } else {
              weights <- d[[wlsWeights]]
              fit <- lm(formula = formula, data=d, weights = weights)
            }
            return(coef(fit))
          }
          
          bootstrap.summary <- boot::boot(data = dataset, statistic = .bootstrapping, R = options$regressionCoefficientsBootstrappingReplicates, formula = formula(lm.model[[m]]$lm.fit), wlsWeights = .v(options$wlsWeights))
          bootstrap.coef <- bootstrap.summary$t0
          bootstrap.bias <- colMeans(bootstrap.summary$t, na.rm = TRUE) - bootstrap.coef
          bootstrap.se <- matrixStats::colSds(bootstrap.summary$t, na.rm = TRUE)
          
          len.reg <- length(bootstrap.regression.result) + 1
          v <- 0
          
          if (options$includeConstant == TRUE) {
            
            if(is.null(na.estimate.names) || na.estimate.names[1] != "(Intercept)"){
              
              v <- v + 1
              
              bootstrap.regression.result[[ len.reg ]] <- empty.line
              bootstrap.regression.result[[ len.reg ]]$"Model" <- as.integer(m - as.numeric(includes.nuisance))
              bootstrap.regression.result[[ len.reg ]]$"Name" <- as.character("(Intercept)")
              bootstrap.regression.result[[ len.reg ]]$"Coefficient" <- as.numeric(bootstrap.coef[v])
              bootstrap.regression.result[[ len.reg ]]$"Bias" <- as.numeric(bootstrap.bias[v])
              bootstrap.regression.result[[ len.reg ]]$"Standard Error" <- as.numeric(bootstrap.se[v])
              bootstrap.regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
              
              if (options$regressionCoefficientsConfidenceIntervals == TRUE) {
                bootstrap.ci <- boot::boot.ci(bootstrap.summary, type="bca", conf = alpha, index=v)
                bootstrap.regression.result[[ len.reg ]]$"Lower Bound" <- as.numeric( bootstrap.ci$bca[4] )
                bootstrap.regression.result[[ len.reg ]]$"Upper Bound" <- as.numeric( bootstrap.ci$bca[5] )
              }
              
            } else {
              
              bootstrap.regression.result[[ len.reg ]] <- empty.line
              bootstrap.regression.result[[ len.reg ]]$"Model" <- as.integer(m - as.numeric(includes.nuisance))
              bootstrap.regression.result[[ len.reg ]]$"Name" <- as.character("(Intercept)")
              bootstrap.regression.result[[ len.reg ]]$"Coefficient" <- "NA"
              bootstrap.regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
            }
            
            len.reg <- len.reg + 1
          }
          
          sd.dep <- sd( dataset[[ dependent.base64 ]] )
          
          if (length(lm.model[[ m ]]$variables) > 0) {
            
            variables.in.model <- lm.model[[ m ]]$variables
            
            for (var in 1:length(variables.in.model)) {
              
              if (!is.null(na.estimate.names) && .v(variables.in.model[var])%in%na.estimate.names) {
                
                v <- v - 1
                bootstrap.regression.result[[ len.reg ]] <- empty.line
                
                if (var == 1 && options$includeConstant == FALSE) {
                  
                  bootstrap.regression.result[[ len.reg ]]$"Model" <- as.integer(m - as.numeric(includes.nuisance))
                  bootstrap.regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
                }
                
                if (grepl(":", variables.in.model[var])) {
                  
                  # if interaction term
                  
                  vars <- unlist(strsplit(variables.in.model[var], split = ":"))
                  name <- paste0(vars, collapse="\u2009\u273b\u2009")
                  
                } else {
                  
                  name <- as.character(variables.in.model[ var])
                }
                
                bootstrap.regression.result[[ len.reg ]]$"Name" <- name
                bootstrap.regression.result[[ len.reg ]]$"Coefficient" <- "NA"
                
                len.reg <- len.reg + 1
                
              } else {
                
                if (grepl(":", variables.in.model[var])) {
                  
                  # if interaction term
                  
                  vars <- unlist(strsplit(variables.in.model[var], split = ":"))
                  
                  int.var <- rep(1, nrow(dataset))
                  
                  for (i in seq_along(vars))
                    int.var <- int.var * dataset[[ .v(vars[i]) ]]
                  
                  sd.ind <- sd(int.var)
                  
                } else {
                  
                  sd.ind <- sd( dataset[[ .v(variables.in.model[var]) ]])
                }
                
                bootstrap.regression.result[[ len.reg ]] <- empty.line
                
                if (var == 1 && options$includeConstant == FALSE) {
                  
                  bootstrap.regression.result[[ len.reg ]]$"Model" <- as.integer(m - as.numeric(includes.nuisance))
                  bootstrap.regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
                }
                
                if (grepl(":", variables.in.model[var])) {
                  
                  # if interaction term
                  
                  vars <- unlist(strsplit(variables.in.model[var], split = ":"))
                  name <- paste0(vars, collapse="\u2009\u273b\u2009")
                  
                } else {
                  
                  name <- as.character(variables.in.model[ var])
                }
                
                bootstrap.regression.result[[ len.reg ]]$"Name" <- name
                bootstrap.regression.result[[ len.reg ]]$"Coefficient" <- as.numeric(bootstrap.coef[v+var])
                bootstrap.regression.result[[ len.reg ]]$"Bias" <- as.numeric(bootstrap.bias[v+var])
                bootstrap.regression.result[[ len.reg ]]$"Standard Error" <- as.numeric(bootstrap.se[v+var])
                
                if (options$regressionCoefficientsConfidenceIntervals == TRUE) {
                  bootstrap.ci <- boot::boot.ci(bootstrap.summary, type="bca", conf = alpha, index=v+var)
                  bootstrap.regression.result[[ len.reg ]]$"Lower Bound" <- as.numeric( bootstrap.ci$bca[4] )
                  bootstrap.regression.result[[ len.reg ]]$"Upper Bound" <- as.numeric( bootstrap.ci$bca[5] )
                }
                
                len.reg <- len.reg + 1
              }
            }
          }
          
        } 
        else {
          
          len.reg <- length(bootstrap.regression.result) + 1
          bootstrap.regression.result[[ len.reg ]] <- dotted.line
          bootstrap.regression.result[[ len.reg ]]$"Model" <- as.integer(m - as.numeric(includes.nuisance))
          
          if (length(lm.model[[ m ]]$variables) > 0) {
            
            variables.in.model <- lm.model[[ m ]]$variables
            
            
            if (options$includeConstant == TRUE) {
              
              bootstrap.regression.result[[ len.reg ]]$"Name" <- as.character("(Intercept)")
            }
            
            len.reg <- len.reg + 1
            
            for (var in 1:length(variables.in.model)) {
              
              bootstrap.regression.result[[ len.reg ]] <- dotted.line
              
              if (grepl(":", variables.in.model[var])) {
                
                # if interaction term
                
                vars <- unlist(strsplit(variables.in.model[var], split = ":"))
                name <- paste0(vars, collapse="\u2009\u273b\u2009")
                
              } else {
                
                name <- as.character(variables.in.model[ var])
              }
              
              bootstrap.regression.result[[ len.reg ]]$"Name" <- name
              len.reg <- len.reg + 1
            }
          }
        }
      }
      
    } else {
      
      if (length(lm.model) > 0 ) {
        
        for (m in 1:length(lm.model)) {
          
          len.reg <- length(bootstrap.regression.result) + 1
          
          if (options$includeConstant == TRUE) {
            
            bootstrap.regression.result[[ len.reg ]] <- dotted.line
            bootstrap.regression.result[[ len.reg ]]$"Model" <- as.integer(m - as.numeric(includes.nuisance))
            bootstrap.regression.result[[ len.reg ]]$"Name" <- as.character("(Intercept)")
            bootstrap.regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
            len.reg <- len.reg + 1
          }
          
          if (length(lm.model[[ m ]]$variables) > 0) {
            
            variables.in.model <- lm.model[[ m ]]$variables
            
            for (var in 1:length(variables.in.model)) {
              
              bootstrap.regression.result[[ len.reg ]] <- dotted.line
              bootstrap.regression.result[[ len.reg ]]$"Model" <- ""
              
              if (var == 1 && options$includeConstant == FALSE) {
                bootstrap.regression.result[[ len.reg ]]$"Model" <- as.integer(m - as.numeric(includes.nuisance))
                bootstrap.regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
              }
              
              if (grepl(":", variables.in.model[var])) {
                
                # if interaction term
                
                vars <- unlist(strsplit(variables.in.model[var], split = ":"))
                name <- paste0(vars, collapse="\u2009\u273b\u2009")
                
              } else {
                
                name <- as.character(variables.in.model[ var])
              }
              
              bootstrap.regression.result[[ len.reg ]]$"Name" <- name
              len.reg <- len.reg + 1
            }
          }
        }
        
      } else {
        
        len.reg <- length(bootstrap.regression.result) + 1
        bootstrap.regression.result[[ len.reg ]] <- dotted.line
        bootstrap.regression.result[[ len.reg ]]$"Model" <- as.integer(m - as.numeric(includes.nuisance))
        
        if (options$includeConstant == TRUE) {
          
          bootstrap.regression.result[[ len.reg ]]$"Name" <- as.character("(Intercept)")
          bootstrap.regression.result[[ len.reg ]][[".isNewGroup"]] <- TRUE
        }
      }
      
      if(length(list.of.errors) > 0){
        
        bootstrap.regression[["error"]] <- list(errorType="badData")
      }
    }
    
    bootstrap.regression[["data"]] <- bootstrap.regression.result
    
    # Check whether variables in the regression model are redundant
    for(i in 1:length(bootstrap.regression$data)) {
      if (bootstrap.regression$data[[i]]$Coefficient=="NA") {
        # Add footnote
        footnotes <- .newFootnotes()
        .addFootnote(footnotes, "The regression coefficient for one or more of the variables specified in the regression model could not be estimated (that is, the coefficient is not available (NA)). The most likely reasons for this to occur are multicollinearity or a large number of missing values.", symbol = "\u207A")
        # Add footnote symbol to name of the redundant variable
        bootstrap.regression$data[[i]]$Name <- paste0(bootstrap.regression$data[[i]]$Name, "\u207A")
      }
    }
    
    bootstrap.regression[["footnotes"]] <- as.list(footnotes)
    results[["bootstrap.regression"]] <- bootstrap.regression
    
  }
}



.createLinearRegressionDescriptivesTable <- function(jaspResults, dataset, options, ready) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["linearRegressionDescriptivesTable"]])) {
    return(NULL)
  }
  
  # Create table
  linearRegressionDescriptivesTable <- createJaspTable(title = "Descriptive Statistics")
  jaspResults[["linearRegressionDescriptivesTable"]] <- linearRegressionDescriptivesTable
  #linearRegressionDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  linearRegressionDescriptivesTable$dependOnOptions(c("dependent", "wlsWeights", "modelTerms", "descriptives",
                                                      "missingValues"))
  
  # Add columns to table
  linearRegressionDescriptivesTable$addColumnInfo(name = "variable", title = "",               type = "string")
  linearRegressionDescriptivesTable$addColumnInfo(name = "N",        title = "N",              type = "integer")
  linearRegressionDescriptivesTable$addColumnInfo(name = "mean",     title = "Mean",           type = "number",
                                                  format = "sf:4;dp:3")
  linearRegressionDescriptivesTable$addColumnInfo(name = "sd",       title = "Std. Deviation", type = "number",
                                                  format = "sf:4;dp:3")
  linearRegressionDescriptivesTable$addColumnInfo(name = "se",       title = "Std. Error",     type = "number",
                                                  format = "sf:4;dp:3")
  
  # Fill up table with results
  .fillUpLinearRegressionDescriptivesTable(linearRegressionDescriptivesTable = linearRegressionDescriptivesTable,
                                         dataset = dataset, options = options, ready = ready)
  
  return(NULL)
}

.fillUpLinearRegressionDescriptivesTable <- function(linearRegressionDescriptivesTable, dataset, options, ready) {
  
  # If results can be computed, compute them and add row for each variable
  if (ready) {
    for (variable in c(options$dependent.variable, options$main.effects.numeric, options$main.effects.factors)) {
      .addRowForLinearRegressionDescriptivesTable(linearRegressionDescriptivesTable = linearRegressionDescriptivesTable,
                                                  dataset = dataset, options = options, variable = variable)
    }
    
    if (options$wlsWeights != "") {
      message <- paste0("Cases are weighted by ", options$wlsWeights)
      linearRegressionDescriptivesTable$addFootnote(message = message, symbol = "<em>Note.</em>")
    }
    
  # If results cannot be computed, add an empty row
  } else {
    row <- list(variable = ".", N = ".", mean = ".", sd = ".", se = ".")
    linearRegressionDescriptivesTable$addRows(rows = row)
  }
  
  return(NULL)
}

.addRowForLinearRegressionDescriptivesTable <- function(linearRegressionDescriptivesTable, dataset, options, variable) {
  
  data <- dataset[(! is.na(dataset[[.v(variable)]]) & ! is.na(dataset[[options[["weights.base64"]]]])), 
                  c(.v(variable), options[["weights.base64"]])]
  
  # Compute results
  if (class(dataset[[.v(variable)]]) != "factor") {
    
    nObs <- nrow(data)
    mean <- Hmisc::wtd.mean(    data[[.v(variable)]], weights = data[[options$weights.base64]], na.rm = TRUE)
    std <-  sqrt(Hmisc::wtd.var(data[[.v(variable)]], weights = data[[options$weights.base64]], na.rm = TRUE))
    sem <-  std / sqrt(nObs)
    
  } else {
    
    nObs <- nrow(data)
    mean <- "."
    std <-  "."
    sem <-  "."
    
  }
  
  # Add row to the table
  row <- list(variable = variable, N = nObs, mean = mean, sd = std, se = sem)
  linearRegressionDescriptivesTable$addRows(rows = row, rowNames = variable)
  
  return(NULL)
}

.createLinearRegressionPartAndPartialCorrelationTable <- function(jaspResults, dataset, options, ready) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["linearRegressionPartAndPartialCorrelationTable"]])) {
    return(NULL)
  }
  
  # Create table
  linearRegressionPartAndPartialCorrelationTable <- createJaspTable(title = "Part And Partial Correlations")
  jaspResults[["linearRegressionPartAndPartialCorrelationTable"]] <- linearRegressionPartAndPartialCorrelationTable
  #linearRegressionPartAndPartialCorrelationTable$showSpecifiedColumnsOnly <- TRUE
  linearRegressionPartAndPartialCorrelationTable$dependOnOptions(c("dependent", "wlsWeights", "modelTerms", 
                                                                   "partAndPartialCorrelations", "missingValues"))
  
  # Declare table elements
  fields <- list(
    list(name = "Model", type = "integer"),
    list(name = "Name", title = "  ", type = "string"),
    list(name = "Partial", title = "Partial", type = "number", format = "dp:3"),
    list(name = "Part", title = "Part", type="number", format = "dp:3"))
  
  
  # Add columns to table
  linearRegressionPartAndPartialCorrelationTable$addColumnInfo(name = "model",    title = "Model",   type = "integer")
  linearRegressionPartAndPartialCorrelationTable$addColumnInfo(name = "variable", title = "",        type = "string")
  linearRegressionPartAndPartialCorrelationTable$addColumnInfo(name = "partial",  title = "Partial", type = "number",
                                                               format = "sf:4;dp:3")
  linearRegressionPartAndPartialCorrelationTable$addColumnInfo(name = "part",     title = "Part",    type = "number",
                                                               format = "sf:4;dp:3")
  
  # Fill up table with results
  .fillUpLinearRegressionPartAndPartialCorrelationTable(linearRegressionPartAndPartialCorrelationTable = linearRegressionPartAndPartialCorrelationTable,
                                                        dataset = dataset, options = options, ready = ready)
  
  return(NULL)
}

.fillUpLinearRegressionPartAndPartialCorrelationTable <- function(linearRegressionPartAndPartialCorrelationTable, dataset, 
                                                                  options, ready) {
  
  # If results can be computed, compute them and add row for each variable
  if (ready) {
    for (model in lm.model) {
      if (length(lm.model[[m]]$variables) > 0) {
        for (variable in lm.model[[m]]$variables) {
          .addRowForlinearRegressionPartAndPartialCorrelationTable(linearRegressionPartAndPartialCorrelationTable = linearRegressionPartAndPartialCorrelationTable,
                                                               dataset = dataset, options = options, variable = variable)
        }
      }
    }
    
    if (options$wlsWeights != "") {
      message <- paste0("Cases are weighted by ", options$wlsWeights)
      linearRegressionPartAndPartialCorrelationTable$addFootnote(message = message, symbol = "<em>Note.</em>")
    }
    
    # If results cannot be computed, add an empty row
  } else {
    row <- list(model = ".", variable = ".", partial = ".", part = ".")
    linearRegressionPartAndPartialCorrelationTable$addRows(rows = row)
  }
  
  return(NULL)
}

.addRowForlinearRegressionPartAndPartialCorrelationTable <- function(linearRegressionPartAndPartialCorrelationTable, dataset, 
                                                                     options, variable) {
  
  if (grepl(":", variable)) {
    
    # if interaction term
    
    vars <- unlist(strsplit(variable, split = ":"))
    name <- paste0(vars, collapse="\u2009\u273b\u2009")
    
  } else {
    
    name <- variable
  }
  
  if ( which(variables.model == variable) == 1) {
    
    partAndPartial <- .partAndPartialCorrelation(dependent.variable, variable, variables.model, dataset)
    partial <- .clean(partAndPartial$partialCor)
    part <- .clean(partAndPartial$partCor)
    correlations.rows[[length(correlations.rows)+1]] <- list(Model=as.integer(m - as.numeric(includes.nuisance)), Name=name, Partial=partial, Part=part, .isNewGroup=TRUE)
    
  } else {
    
    partAndPartial <- .partAndPartialCorrelation(dependent.variable, variable, variables.model, dataset)
    partial <- .clean(partAndPartial$partialCor)
    part <- .clean(partAndPartial$partCor)
    correlations.rows[[length(correlations.rows)+1]] <- list(Model="", Name=name, Partial=partial, Part=part)
  }
  
  # Add row to the table
  row <- list(variable = variable, N = nObs, mean = mean, sd = std, se = sem)
  linearRegressionDescriptivesTable$addRows(rows = row, rowNames = variable)
  
  return(NULL)
}

.partAndPartialCorrelation <- function(dependent.variable, variable.of.interest, model.variables, dataset) {
  
  dataset <- na.omit(dataset)
  dependent <- dataset[[ .v(dependent.variable) ]]
  
  # remove variable.of.interest from model.variables
  index <- which(model.variables == variable.of.interest)
  variables.to.control.for <- model.variables[-index]
  
  # if there are no variables to control for, return regular correlation
  if (length(variables.to.control.for) == 0) {
    
    if (grepl(":", variable.of.interest)) {
      
      # if interaction term
      
      vars <- unlist(strsplit(variable.of.interest, split = ":"))
      vVars <- .v(vars)
      int.var <- rep(1, nrow(dataset))
      
      for (i in seq_along(vVars ))
        int.var <- int.var * dataset[[ vVars[i] ]]
      
      correlation <- cor(dependent, int.var)
      
    } else {
      
      correlation <- cor(dependent, dataset[[ .v(variable.of.interest) ]])
    }
    
    return(list(partCor=correlation, partialCor=correlation))
  }
  
  # v variables
  variables.to.control.for <- .vWithInteraction(variables.to.control.for)
  dependent.variable <- .v(dependent.variable)
  variable.of.interest <- .vWithInteraction(variable.of.interest)
  
  if (grepl(":", variable.of.interest)) {
    
    # if interaction term
    
    vars <- unlist(strsplit(variable.of.interest, split = ":"))
    int.var <- rep(1, nrow(dataset))
    
    for (i in seq_along(vars))
      int.var <- int.var * dataset[[ vars[i] ]]
    
    variable.of.interest <- paste0(vars, collapse=".")
    dataset[[variable.of.interest]] <- int.var
    
  }
  
  # create formulas
  definition1 <- paste(variable.of.interest, "~", paste(variables.to.control.for, collapse="+"))
  formula1 <- as.formula(definition1)
  definition2 <- paste(dependent.variable, "~", paste(variables.to.control.for, collapse="+"))
  formula2 <- as.formula(definition2)
  
  # remove variables.to.control.for from variable.of.interest
  cleaned.variable.of.interest <- residuals( lm(formula1, data=dataset) )
  
  # remove variables.to.control.for from dependent.variable
  cleaned.dependent.variable <- residuals( lm(formula2, data=dataset) )
  
  # part (semi-partial) correlation
  partCor <- cor(cleaned.variable.of.interest, dependent)
  
  # partial correlation
  partialCor <- cor(cleaned.variable.of.interest, cleaned.dependent.variable)
  
  return(list(partCor=partCor, partialCor=partialCor))
  
}

.collinearityDiagnostics <- function(lm.fit, dataset, includeConstant=TRUE) {
  
  ### create predictor variable matrix
  X <- lm.fit$x
  
  ### scale predictor matrix
  for (i in seq_len(ncol(X))) {
    
    X[ ,i] <- X[ ,i] / sqrt(sum(X[ ,i]^2)) # scale each column using Euclidean norm
    
  }
  
  ### eigenvalues
  eigenvalues <- svd(X)$d^2 # see Liao & Valliant (2012)
  
  ### condition indices
  conditionIndices <- sqrt(max(eigenvalues) / eigenvalues)
  
  ### variance proportions ( see e.g., Liao & Valliant, 2012 )
  svdX <- svd(X) # singular value decomposition
  M <- svdX$v %*% solve(diag(svdX$d))
  Q <- M*M # Hadamard (elementwise) product
  tQ <- t(Q)
  
  for (i in seq_len(ncol(tQ))) {
    
    tQ[, i] <- tQ[ ,i] / sum(tQ[ ,i])
    
  }
  
  varianceProportions <- tQ
  
  ### VIF (variance inflation factor)
  
  if ( ! includeConstant) {
    
    predictors <- colnames(lm.fit$x) # predictors in model (remove dependent variable and weights)
    
  } else {
    
    predictors <- colnames(lm.fit$x)[-1]
  }
  
  VIF <- list()
  tolerance <- list()
  
  if (length(predictors) == 1) {
    
    VIF[[predictors]] <- 1
    tolerance[[predictors]] <- 1
    
  } else if (length(predictors) > 1) {
    
    for (predictor in predictors) {
      
      if (grepl(":", predictor)) {
        
        # if interaction term
        
        vars <- unlist(strsplit(predictor, split = ":"))
        int.var <- rep(1, nrow(dataset))
        
        for (i in seq_along(vars))
          int.var <- int.var * dataset[[ vars[i] ]]
        
        predictor.d <- paste0(vars, collapse=".")
        dataset[[predictor.d]] <- int.var
        
      } else {
        
        predictor.d <- predictor
      }
      
      # remove predictor from other predictors
      index <- which(predictors == predictor)
      cleanedPredictors <- predictors[-index]
      
      # create formula
      definition <- paste(predictor.d, "~", paste(cleanedPredictors, collapse="+"))
      formula <- as.formula(definition)
      
      # fit lm
      fitVIF <- try(lm(formula, data=dataset), silent=TRUE)
      
      # VIF (variance inflation factor)
      VIF[[predictor]] <- 1 / (1 - summary(fitVIF)$"r.squared")
      
      # tolerance
      tolerance[[predictor]] <- 1 / VIF[[predictor]]
    }
  }
  
  output <- list(	eigenvalues=eigenvalues,
                  conditionIndices=conditionIndices,
                  varianceProportions=varianceProportions,
                  VIF=VIF,
                  tolerance=tolerance)
  
  return(output)
  
}
