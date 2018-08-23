#
# Copyright (C) 2018 University of Amsterdam
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

ReliabilityAnalysis <- function(jaspResults, dataset, options, state = NULL) {

  # Add additional options that are needed for the analyses
  if (options$alphaScaleStandardized == "_2standardized") {
    options[["alphaScaleStandardizedRec"]] <- "std.alpha"
  } else {
    options[["alphaScaleStandardizedRec"]] <- "raw_alpha"
  }
  
  if (options$missingValues == "excludeCasesListwise") {
    options[["missingValuesRec"]] = "listwise"
  } else {
    options[["missingValuesRec"]] = "pairwise"
  }
  
  # Define state if empty
  if (is.null(state)) {
	  state <- list()
	}
	
  # Read dataset
  if (is.null(dataset)) {
		dataset <- .readDataSetToEnd(columns.as.numeric=options$variables, columns.as.factor=NULL, exclude.na.listwise=NULL)
	} else {
		dataset <- .vdf(dataset, columns.as.numeric=options$variables, columns.as.factor=NULL)
	}

	# Set title
	jaspResults$title <- "Reliability Analysis"
	
	# Check if results can be computed
	ready <- (length(options$variables) > 1)
	
	# Check for errors (infinite values, variance = 0, or too few observations) for each variable
	if (ready) {
	  .hasErrors(dataset = dataset, perform = "run", type = c("infinity", "variance", "observations"),
	             observations.amount = " < 3", exitAnalysisIfErrors = TRUE)
	}
	
	# Compute results
	if (ready) {
	  results <- .computeReliabilityResults(dataset = dataset, options = options)
	}
	
	# Create Scale Reliability Statistics Table (if wanted)
	if (options$alphaScale || options$gutmannScale || options$averageInterItemCor ||options$mcDonaldScale || 
	    options$glbScale || options$meanScale || options$sdScale) {
	  .createReliabilityScaleTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready,
	                               results = results)
	}
	
	# Create Item Reliability Statistics Table (if wanted)
	if (options$alphaItem || options$gutmannItem || options$mcDonaldItem || options$itemRestCor || 
	    options$meanItem || options$sdItem) {
		.createReliabilityItemsTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready,
		                             results = results)
	}
	
	# Bring state up-to-date
	state[["options"]] <- options
	
	return(state)
}

.computeReliabilityResults <- function (dataset, options) {
  
  # This will be the return object
  results <- list()
  
  # Reverse items if specified by the user
  key <- NULL
  if (length(options$reverseScaledItems) > 0) {
    key <- rep(1, length(options$variables))
    key[match(.v(unlist(options$reverseScaledItems)), colnames(dataset))] <- -1
    dataset2 <- psych::reverse.code(key, dataset)
    # This will be used to compute the means and sds because the key cannot be used for this
    dataList2 <- .reliabilityConvertDataToCorrelation(dataset2, options)
  }
  
  # Obtain smoothed correlation and covariance matrix
  dataList <- .reliabilityConvertDataToCorrelation(dataset, options)

  nObs <- nrow(dataset)
  nVar <- ncol(dataset)
  
  # Compute results for chronbachs alpha, gutmanns lambda6, and average inter item corrrelation (both for scale and variable)
  resultsPsych <- .quietDuringUnitTest(psych::alpha(dataList[["covariance"]], key = key))
  resultsPsych[["nObs"]] <- nObs
  
  # Calculate confidence interval for chronbach alpha
  alphaScaleCI <- .reliabilityAlphaCI(results = resultsPsych, ci = options[["confAlphaLevel"]])
  
  # Compute results for Scale Table
  if (is.null(key)) {
    meanScale <-         mean(dataList[["itemMeans"]])
    sdScale <-           stats::sd(dataList[["itemMeans"]])
  } else {
    meanScale <-         mean(dataList2[["itemMeans"]])
    sdScale <-           stats::sd(dataList2[["itemMeans"]])
  }
  alphaScale <-        resultsPsych[["total"]][[options$alphaScaleStandardizedRec]]
  alphaScaleLowerCI <- alphaScaleCI[1]
  alphaScaleUpperCI <- alphaScaleCI[2]
  lambdaScale <-       resultsPsych[["total"]][["G6(smc)"]]
  rhoScale <-          resultsPsych[["total"]][["average_r"]]
  omegaScale <-        .quietDuringUnitTest(psych::omega(m = dataList[["correlation"]], key = key, nfactors = 1, flip = FALSE,
                                                         plot = FALSE, n.iter = 1, n.obs = nObs)[["omega.tot"]])
  glbScale <- "."
  if (nVar > 2) {
    # Try since the glb is error prone icm reverse scaled items. Requires further investigation/ this might be a bug in psych.
    glbScale <- .quietDuringUnitTest(try(psych::glb(r = dataList[["correlation"]], key = key)[["glb.max"]], silent = TRUE))
  }
  
  results[["ScaleTable"]] <- list(case = "Scale", mean = .clean(meanScale), sd = .clean(sdScale), omega = .clean(omegaScale),
                                  alpha = .clean(alphaScale), alphaLowerCI = .clean(alphaScaleLowerCI), alphaUpperCI = .clean(alphaScaleUpperCI),
                                  lambda = .clean(lambdaScale), glb = .clean(glbScale), rho = .clean(rhoScale))
  
  # Compute results for Item Table
  for (variable in options$variables) {
    
    index <- which(rownames(dataList$correlation) == .v(variable) | rownames(dataList$correlation) == paste0(.v(variable), "-"))
    
    meanItem <-         dataList2[["itemMeans"]][[index]]
    sdItem <-           dataList2[["itemSds"]][[index]]
    itemRestCorItem <-  resultsPsych[["item.stats"]][index, "r.drop"]
    alphaItem <-        resultsPsych[["alpha.drop"]][index, options$alphaScaleStandardizedRec]
    lambdaItem <-       resultsPsych[["alpha.drop"]][index, "G6(smc)"]
    omegaItem <- "."
    if (nVar > 2) {
      omegaItem <- .quietDuringUnitTest(psych::omega(m = dataList[["correlation"]][-index, -index], key = key[-index], 
                                                     nfactors = 1, n.iter = 1, n.obs = nObs, flip = FALSE, 
                                                     plot = FALSE)[["omega.tot"]])
    }
    results[["ItemTable"]][[index]] <- list(case = variable, mean = .clean(meanItem), sd = .clean(sdItem), 
                                            itemRestCor = .clean(itemRestCorItem), omega = .clean(omegaItem), 
                                            alpha = .clean(alphaItem), lambda = .clean(lambdaItem))
  }
  
  # Return results
  return(results)
}

.createReliabilityScaleTable <- function (jaspResults, dataset, options, ready, results) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["reliabilityScaleTable"]])) {
    return(NULL)
  }
  
  # Create table
  reliabilityScaleTable <- createJaspTable("Scale Reliability Statistics")
  jaspResults[["reliabilityScaleTable"]] <- reliabilityScaleTable
  reliabilityScaleTable$showSpecifiedColumnsOnly <- TRUE
  reliabilityScaleTable$dependOnOptions(c("variables", "alphaScale", "alphaScaleStandardized", "gutmannScale", 
                                          "averageInterItemCor", "mcDonaldScale", "glbScale", "meanScale",
                                          "sdScale", "reverseScaledItems", "missingValues", "confAlpha",
                                          "confAlphaLevel"))
  
  # Add columns to table
  reliabilityScaleTable$addColumnInfo(  name = "case",         title = "",
                                        type = "string")
  if (options$meanScale) {
    reliabilityScaleTable$addColumnInfo(name = "mean",         title = "Mean",
                                        type = "number",       format = "sf:4;dp:3")
  }
	if (options$sdScale) {
	  reliabilityScaleTable$addColumnInfo(name = "sd",           title = "Std. Deviation",
	                                      type = "number",       format = "sf:4;dp:3")
	}
	if (options$mcDonaldScale) {
	  reliabilityScaleTable$addColumnInfo(name = "omega",        title = "McDonald's \u03C9",
	                                      type = "number",       format = "sf:4;dp:3")
	}
	if (options$alphaScale) {
	  reliabilityScaleTable$addColumnInfo(name = "alpha",        title = "Cronbach's \u03B1",
	                                      type = "number",       format = "sf:4;dp:3")
	}
  if (options$alphaScale && options$confAlpha) {
    reliabilityScaleTable$addColumnInfo(name = "alphaLowerCI", title = "Lower",
                                        type = "number",       format = "sf:4", 
                                        overtitle = paste0(100*options$confAlphaLevel, "% CI for unstandardized Cronbach's \u03B1"))
    reliabilityScaleTable$addColumnInfo(name = "alphaUpperCI", title = "Upper",
                                        type = "number",       format = "sf:4",
                                        overtitle = paste0(100*options$confAlphaLevel, "% CI for unstandardized Cronbach's \u03B1"))
  }
	if (options$gutmannScale) {
	  reliabilityScaleTable$addColumnInfo(name = "lambda",       title = "Gutmann's \u03BB6",
	                                      type = "number",       format = "sf:4;dp:3")
	}
	if (options$glbScale) {
	  reliabilityScaleTable$addColumnInfo(name = "glb",          title = "Greatest lower bound",
	                                      type = "number",       format = "sf:4;dp:3")
	}
	if (options$averageInterItemCor) {
	  reliabilityScaleTable$addColumnInfo(name = "rho",          title = "Average interitem correlation", 
	                                      type = "number",       format = "sf:4;dp:3")
	}
  
  # Fill up table with results
  .fillUpReliabilityScaleTable(reliabilityScaleTable = reliabilityScaleTable, dataset = dataset, options = options,
                               ready = ready, results = results)
  
	# This function does not return anything
}

.fillUpReliabilityScaleTable <- function (reliabilityScaleTable, dataset, options, ready, results) {
  
  # If results can be computed, compute them and add row
  if (ready) {
    
    row <- results[["ScaleTable"]]
    reliabilityScaleTable$addRows(row)
    
    # Add footnote: Missing values
    nObs <- nrow(dataset)
    nExcluded <- sum(!complete.cases(dataset))
    nValid <- nObs - nExcluded
    reliabilityScaleTable$addFootnote(message = sprintf("Of the observations, %d were used, %d were excluded %s, and %d were provided.",
                                                        nValid, nExcluded, options$missingValuesRec, nObs), 
                                      symbol = "<em>Note.</em>")
    
    # Add footnote: Problems for calculation of glb
    if (options$glbScale) {
      if (length(options$variables) < 3) {
        reliabilityScaleTable$addFootnote(message = "Greatest lower bound can only be calculated for three or more variables.",
                                          symbol = "<em>Warning.</em>")
      } else if (isTryError(results[["ScaleTable"]][["glb"]])) {
        reliabilityScaleTable$addFootnote(message = "Greatest lower bound could not be calculated.", 
                                          symbol = "<em>Warning.</em>")
      }
    }
    
  # If results cannot be computed, add an empty row
  } else {
    row <- list(case = "Scale", mean = ".", sd = ".", omega = ".", alpha = ".", alphaLowerCI = ".", 
                alphaUpperCI = ".", lambda = ".", glb = ".", rho = ".")
    reliabilityScaleTable$addRows(row)
  }
  
  # This function does not return anything
}

.createReliabilityItemsTable <- function (jaspResults, dataset, options, ready, results) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["reliabilityItemsTable"]])) {
    return(NULL)
  }
  
  # Create table
  reliabilityItemsTable <- createJaspTable("Item Reliability Statistics")
  jaspResults[["reliabilityItemsTable"]] <- reliabilityItemsTable
  reliabilityItemsTable$showSpecifiedColumnsOnly <- TRUE
  reliabilityItemsTable$dependOnOptions(c("variables", "alphaItem", "alphaScaleStandardized", "gutmannItem", "mcDonaldItem", 
                                          "itemRestCor", "meanItem", "sdItem", "reverseScaledItems", "missingValues"))
  
  # Add columns to table
  reliabilityItemsTable$addColumnInfo(  name = "case",        title = "",
                                        type = "string",      combine = TRUE)
  if (options$meanItem) {
    reliabilityItemsTable$addColumnInfo(name = "mean",        title = "Mean",
                                        type = "number",      format = "sf:4;dp:3")
  }
  if (options$sdItem) {
    reliabilityItemsTable$addColumnInfo(name = "sd",          title = "Std. Deviation",
                                        type = "number",      format = "sf:4;dp:3")
  }
  if (options$itemRestCor) {
    reliabilityItemsTable$addColumnInfo(name = "itemRestCor", title="Item-rest correlation",
                                        type = "number",      format = "sf:4;dp:3")
  }
  if (options$mcDonaldItem) {
    reliabilityItemsTable$addColumnInfo(name = "omega",       title="McDonald's \u03C9",
                                        type = "number",      format = "sf:4;dp:3", 
                                        overtitle = paste0("If item dropped"))
  }
  if (options$alphaItem) {
    reliabilityItemsTable$addColumnInfo(name = "alpha",       title="Cronbach's \u03B1",
                                        type = "number",      format = "sf:4;dp:3", 
                                        overtitle = paste0("If item dropped"))
  }  
  if (options$gutmannItem) {
    reliabilityItemsTable$addColumnInfo(name = "lambda",      title="Gutmann's \u03BB6",
                                        type = "number",      format = "sf:4;dp:3",
                                        overtitle = paste0("If item dropped"))
  }  
  
  # Fill up table with results
  .fillUpReliabilityItemsTable(reliabilityItemsTable = reliabilityItemsTable, dataset = dataset, options = options,
                               ready  = ready, results = results)
  
  # This function does not return anything
}

.fillUpReliabilityItemsTable <- function (reliabilityItemsTable, dataset, options, ready, results) {
  
  # If results can be computed, compute them and add row for each variable
  if (ready) {
    
    for (i in 1:length(results[["ItemTable"]])) {
      row <- results[["ItemTable"]][[i]]
      reliabilityItemsTable$addRows(row, rowNames = row[["case"]])
    }
    
    # Add footnote: Omega can only be computed if there are at least 3 variables.
    if (options$mcDonaldItem && length(options$variables) < 3) {
      reliabilityItemsTable$addFootnote(message = "McDonald's \u03C9 if item dropped can only be calculated for three or more variables.", 
                                        symbol = "<em>Warning.</em>")
    }
    
    # Add footnote: Reverse-scaled items
    if (length(options$reverseScaledItems) > 0) {
      reliabilityItemsTable$addFootnote(message = "reverse-scaled item", symbol = "\u207B", 
                                        col_names = "case", row_names = options$reverseScaledItems)
    }
    
  # If results cannot be computed, add an empty row
  } else {
    row <- list(case = ".", mean = ".", sd = ".", itemRestCor = ".", omega = ".", alpha = ".", lambda = ".")
    reliabilityItemsTable$addRows(row)
  }
    
  # This function does not return anything
}

.reliabilityAlphaCI <- function(results, ci, nullAlpha = 0) {

	# code taken and modified from http://www.psyctc.org/stats/R/Feldt1.html
	# considering using the bootstrapped version inside psych as an alternative

	#***********************************************************#
	#* program using methods described in Feldt, Woodruff &    *#
	#* Salih (1987) Applied Psychological Measurement 11(1),   *#
	#* pp. 93-103 to carry out omnibus inferential test of     *#
	#* similarity of alpha values from a single sample         *#
	#***********************************************************#
	
	# results is the output from psych::alpha and must contain the sample size as nObs
	# ci is the width of the confidence interval about obs.a desired
  
	estAlpha = results[["total"]][["raw_alpha"]]
	nVar = results[["nvar"]]
	nObs = results[["nObs"]]

	if(estAlpha > nullAlpha) {
		f <- (1 - estAlpha) / (1 - nullAlpha)
	} else {
		f <- (1 - nullAlpha) / (1 - estAlpha)
	}
	nDen <- (nObs - 1) * (nVar - 1)
	nNum <- nObs - 1
	null.p <- stats::pf(f, nNum, nDen) # set the upper and lower p values for the desired C.I.
	p1 <- (1 - ci)/2
	p2 <- ci + p1 # corresponding F values
	f1 <- stats::qf(p1, nNum, nDen)
	f2 <- stats::qf(p2, nNum, nDen) # confidence interval
	lwr <- 1 - (1 - estAlpha) * f2
	upr <- 1 - (1 - estAlpha) * f1
	return(c(lwr, upr))
}

.reliabilityConvertDataToCorrelation <- function(dataset, options) {
	
	if (options[["missingValues"]] == "excludeCasesListwise") {
		dataset <- dataset[complete.cases(dataset), ]
	}
	
	means = colMeans(dataset, na.rm = TRUE)
	covmat <- stats::cov(dataset, use = "pairwise")
	stdev <- sqrt(diag(covmat))
	cormat <- psych::cor.smooth(stats::cov2cor(covmat), eig.tol = sqrt(.Machine[["double.eps"]]))
	
	return(list(
		correlation = cormat,
		itemSds = stdev,
		itemMeans = means,
		# direct line from: corpcor::rebuild.cov
		covariance = sweep(sweep(cormat, 1, stdev, "*"), 2, stdev, "*")
	))
}