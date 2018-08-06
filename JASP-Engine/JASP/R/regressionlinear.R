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
  
  # Add dependent variable and weights to options
  if (options$dependent != "") {
    options[["dependent.variable"]]        <- options$dependent
    options[["dependent.variable.base64"]] <- .v(options$dependent)
  } else {
    options[["dependent.variable"]]        <- NULL
  }
  
  if (options$wlsWeights != "") {
    options[["weights"]]                   <- options$wlsWeights
    options[["weights.base64"]]            <- .v(options$wlsWeights)
  } else {
    options[["weights"]]                   <- NULL
  }
  
  # Read dataset (including weights)
	dataset <- .readDataSetToEnd(	columns.as.factor   = options$main.effects.factors, 
		                            columns.as.numeric  = c(options$dependent.variable, options$main.effects.numeric, 
		                                                    options$weights),
		                            exclude.na.listwise = c(options$dependent.variable, options$main.effects.numeric, 
		                                                    options$main.effects.factors, options$weights))
	
	if (! is.null(options$weights)) {
	  colnames(dataset[[.v(options$weights)]]) <- .v("weights")
	} else {
	  dataset[[.v("weights")]] <- rep(1, nrow(dataset))
	}
	
	# Set title
	jaspResults$title <- "Linear Regression"
	
	# Check if results can be computed
	ready <- (options$dependent != "" && length(options$modelTerms) > 0)
	
	# Check for errors
	if (ready) {
	  
	  # Error check 1: Check for nonpositive weights
	  if (options$wlsWeight != "") {
	    .hasErrors(dataset, perform = "run", type = c('nonPositive'), all.target = options$weights, 
	               exitAnalysisIfErrors = TRUE)
	  }
	  
	  # Error check 2: Check for too few observations, variables with zero variance, and infinite values
	  .hasErrors(dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
	             all.target = c(options$dependent.variable, options$main.effects.numeric, options$main.effects.factors, options$weights), 
	             observations.amount = c('< 3'), exitAnalysisIfErrors = TRUE)
	  
	  # Error check 3: Check for non-positive definite variance-covariance matrix
	  if (length(options$main.effects.numeric) > 0) {
	    if (! is.null(options$weights)) {
	      # If we have weights, take this into account for the variance-covariance matrix
	      covwt <- function(...) return(stats::cov.wt(..., wt=dataset[[.v(options$weights)]])$cov)
	      # Do not include factors in the variance-covariance matrix
	      errors <- .hasErrors(dataset[, -which(colnames(dataset) %in% c(options$weights.base64))], 
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
	
	# Fit Linear Model
	linearModel <- .fitLinearRegressionModel(dataset = dataset, options = options, ready = ready)
	
	# Create Model Summary Table
	#.createLinearRegressionModelSummaryTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready)
	
	# Create Anova Table (if wanted)
	if (options$modelFit) {
	  #.createLinearRegressionAnovaTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready)
	}

	# Create Coefficients Table (if wanted)
	if (options$regressionCoefficientsEstimates) {
	  #.createLinearRegressionCoefficientsTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready)
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
	if (options$partAndPartialCorrelations) {
	  .createLinearRegressionPartAndPartialCorrelationTable(jaspResults = jaspResults, dataset = dataset, options = options, 
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
    # If results cannot be computed, add an empty row
  } else {
    row <- list(variable = ".", N = ".", mean = ".", sd = ".", se = ".")
    linearRegressionDescriptivesTable$addRows(rows = row)
  }
  
  return(NULL)
}

.addRowForLinearRegressionDescriptivesTable <- function(linearRegressionDescriptivesTable, dataset, options, variable) {
  
  sink("~/Desktop/testCommon.txt", append = TRUE)
  print(dataset[[.v(variable)]])
  print(dataset[dataset[[.v(variable)]] == 1, ])
  print(dataset[[! is.na(dataset[[.v(variable)]])]])
  print(dataset[[! is.na(dataset[[.v(variable)]]) & ! is.na(dataset[[.v("weights")]]), ]])
  print(dataset[[, c(.v(variable), .v("weights"))]])
  sink()
  
  data <- dataset[[! is.na(dataset[[.v(variable)]]) & ! is.na(dataset[[.v("weights")]]), c(.v(variable), .v("weights"))]]
  
  # Compute results
  if (class(data) != "factor") {
    
    nObs <- length(data)
    mean <- Hmisc::wtd.mean(    data[[.v(variable)]], weights = data[[.v("weights")]], na.rm = TRUE)
    std <-  sqrt(Hmisc::wtd.var(data[[.v(variable)]], weights = data[[.v("weights")]], na.rm = TRUE))
    sem <-  std / sqrt(nObs)
    
  } else {
    
    nObs <- length(data)
    mean <- "."
    std <-  "."
    sem <-  "."
    
  }
  
  # Add row to the table
  row <- list(variable = variable, N = nObs, mean = mean, sd = std, se = sem)
  linearRegressionDescriptivesTable$addRows(rows = row, rowNames = variable)
  
  return(NULL)
}