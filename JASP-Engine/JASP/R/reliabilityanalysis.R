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

  # do we look up raw or standardized alpha?
  if (options[["alphaScaleStandardized"]][[1]] == "_2standardized") {
    options$alphaNms <- "std.alpha"
  } else {
    options$alphaNms <- "raw_alpha"
  }
  
  wantsReliabilityScaleTable <- (options$alphaScale || options$gutmannScale || options$averageInterItemCor || 
                                  options$mcDonaldScale || options$glbScale || options$meanScale || options$sdScale)
  wantsReliabilityItemsTable <- (options$alphaItem || options$gutmannItem || options$mcDonaldItem || 
                                  options$itemRestCor || options$meanItem || options$sdItem)
  
	if (is.null(state)) {
	  state <- list()
	}
	
	if (is.null(dataset)) {
		dataset <- .readDataSetToEnd(columns.as.numeric=options$variables, columns.as.factor=NULL, exclude.na.listwise=NULL)
	} else {
		dataset <- .vdf(dataset, columns.as.numeric=options$variables, columns.as.factor=NULL)
	}

	# Set title
	jaspResults$title <- "Reliability Analysis"
	
	# Check for errors
	if (length(options$variables) > 0) {
	  anyErrors <- .hasErrors(dataset = dataset, perform = "run",
	                        type = c("infinity", "variance", "observations"),
	                        observations.amount = " < 3",
	                        exitAnalysisIfErrors = TRUE)
	}
	
	# Compute Results for both tables
	if (wantsReliabilityScaleTable || wantsReliabilityItemsTable) {
	  reliabilityResults <- .reliabilityResults(dataset, options)
	}
	
	# Create Scale Reliability Statistics Table
	if (wantsReliabilityScaleTable) {
	  .reliabilityScaleTable(jaspResults = jaspResults, dataset = dataset, options = options, reliabilityResults = reliabilityResults)
	}
	
	# Create Item Reliability Statistics Table (if wanted)
	if (wantsReliabilityItemsTable) {
		.reliabilityItemsTable(jaspResults = jaspResults, options = options, reliabilityResults = reliabilityResults)
	}
	
	# Bring state$options up-to-date
	state[["options"]] <- options
	
	return(state = state)
}

.reliabilityResults <- function (dataset, options) {
  
  relyFit <- NULL
  variables <- options$variables
  
  if (length(variables) > 1) {
    
    # obtain smoothed correlation and covariance matrix
    dataList <- .reliabilityConvertDataToCorrelation(dataset, options)
    nObs <- nrow(dataset)
    nVar <- ncol(dataset)
    
    # generate key for reverse scaled items
    key <- NULL
    if (length(options[["reverseScaledItems"]]) > 0) {
      
      key <- rep(1, length(variables))
      key[match(.v(unlist(options[["reverseScaledItems"]])), colnames(dataset))] <- -1
      
    }
    
    # calculate chronbach alpha, gutmanns lambda6, and average inter item corrrelation
    relyFit <- .quietDuringUnitTest(psych::alpha(dataList[["covariance"]], key = key))
    
    # because we supply a correlation matrix and not raw data, we have to add these ourselves
    relyFit[["total"]][["mean"]] <- mean(dataList[["itemMeans"]])
    relyFit[["total"]][["sd"]] <- stats::sd(dataList[["itemMeans"]])
    relyFit[["item.stats"]][["mean"]] <- dataList[["itemMeans"]]
    relyFit[["item.stats"]][["sd"]] <- dataList[["itemSds"]]
    relyFit[["nObs"]] <- nObs
    
    # calculate confidence interval for chronbach alpha
    relyFit[["ciAlpha"]] <- .reliabilityAlphaCI(relyFit = relyFit,	ci = options[["confAlphaLevel"]])

    # calculate the greatest lower bound -- only possible for more than 2 variables.
    if (nVar < 3) {
      
      relyFit[["total"]][["glb"]] <- "."
      
    } else { # try since the glb is error prone icm reverse scaled items. Requires further investigation/ this might be a bug in psych.
      
      relyFit[["total"]][["glb"]] <- .quietDuringUnitTest(try(psych::glb(r = dataList[["correlation"]], key = key)[["glb.max"]], silent = TRUE))
      
    }
    
    # calculate McDonalds omega
    omega <- .quietDuringUnitTest(psych::omega(m = dataList[["correlation"]], nfactors = 1, flip = FALSE, plot = FALSE, 
                                               n.iter = 1, n.obs = nObs)[["omega.tot"]])
    
    # calculate McDonalds omega if item dropped
    omegaDropped <- c()
    for (i in 1:nVar) {
      if (nVar > 2) {
        omegaDropped[i] <- .quietDuringUnitTest(psych::omega(m = dataList[["correlation"]][-i, -i], 
                                                             nfactors = 1, n.iter = 1, n.obs = nObs,
                                                             flip = FALSE, plot = FALSE)[["omega.tot"]])
      } else {
        omegaDropped[i] <- "."
      }
    }
    
    relyFit[["total"]][["omega"]] <- omega
    relyFit[["omegaDropped"]] <- omegaDropped
  }
  
  return(relyFit)
}

.reliabilityScaleTable <- function (jaspResults, dataset, options, reliabilityResults) {
  
  if (!is.null(jaspResults[["reliabilityScaleTable"]])) {
    return() #The options for this table didn't change so we don't need to rebuild it
  }
  
  reliabilityScaleTable <- createJaspTable("Scale Reliability Statistics")
  jaspResults[["reliabilityScaleTable"]] <- reliabilityScaleTable
  reliabilityScaleTable$showSpecifiedColumnsOnly <- TRUE
  reliabilityScaleTable$dependOnOptions(c("variables", "alphaScale", "alphaScaleStandardized", "gutmannScale", 
                                          "averageInterItemCor", "mcDonaldScale", "glbScale", "meanScale",
                                          "sdScale", "reverseScaledItems", "missingValues", "confAlpha",
                                          "confAlphaLevel"))
  
  reliabilityScaleTable$addColumnInfo(  name = "case",         title = "",                              type = "string")
  if (options$meanScale) {
    reliabilityScaleTable$addColumnInfo(name = "mean",         title = "Mean",                          type = "number", format = "sf:4;dp:3")
  }
	if (options$sdScale) {
	  reliabilityScaleTable$addColumnInfo(name = "sd",           title = "Std. Deviation",                type = "number", format = "sf:4;dp:3")
	}
	if (options$mcDonaldScale) {
	  reliabilityScaleTable$addColumnInfo(name = "omega",        title = "McDonald's \u03C9",             type = "number", format = "sf:4;dp:3")
	}
	if (options$alphaScale) {
	  reliabilityScaleTable$addColumnInfo(name = "alpha",        title = "Cronbach's \u03B1",             type = "number", format = "sf:4;dp:3")
	}
  if (options$alphaScale && options$confAlpha) {
    reliabilityScaleTable$addColumnInfo(name = "alphaLowerCI", title = "Lower",                         type = "number", format = "sf:4", overtitle = paste0(100*options$confAlphaLevel, "% Confidence Interval"))
    reliabilityScaleTable$addColumnInfo(name = "alphaUpperCI", title = "Upper",                         type = "number", format = "sf:4", overtitle = paste0(100*options$confAlphaLevel, "% Confidence Interval"))
  }
	if (options$gutmannScale) {
	  reliabilityScaleTable$addColumnInfo(name = "lambda",       title = "Gutmann's \u03BB6",             type = "number", format = "sf:4;dp:3")
	}
	if (options$glbScale) {
	  reliabilityScaleTable$addColumnInfo(name = "glb",          title = "Greatest lower bound",          type = "number", format = "sf:4;dp:3")
	}
	if (options$averageInterItemCor) {
	  reliabilityScaleTable$addColumnInfo(name = "rho",          title = "Average interitem correlation", type = "number", format = "sf:4;dp:3")
	}
  
  # Fill up table with results
  .reliabilityScaleResults(reliabilityScaleTable = reliabilityScaleTable, dataset = dataset, options = options, reliabilityResults = reliabilityResults)
  
	return(NULL)
}

.reliabilityScaleResults <- function (reliabilityScaleTable, dataset = dataset, options, reliabilityResults) {
  
  if (options[["missingValues"]] == "excludeCasesListwise") {
    exclwise = " listwise"
  } else {
    exclwise = " pairwise"
  }
  
  # This is the row that will be added to the table
  row <- list()
  
  if (length(options$variables) > 1) {
    
    mean <-         reliabilityResults[["total"]][["mean"]]
    sd <-           reliabilityResults[["total"]][["sd"]]
    omega <-        reliabilityResults[["total"]][["omega"]]
    alpha <-        reliabilityResults[["total"]][[options$alphaNms]]
    alphaLowerCI <- reliabilityResults[["ciAlpha"]][1]
    alphaUpperCI <- reliabilityResults[["ciAlpha"]][2]
    lambda <-       reliabilityResults[["total"]][["G6(smc)"]]
    glb <-          reliabilityResults[["total"]][["glb"]]
    rho <-          reliabilityResults[["total"]][["average_r"]]

    row <- list(case = "Scale", mean = .clean(mean), sd = .clean(sd), omega = .clean(omega), alpha = .clean(alpha), 
                alphaLowerCI = .clean(alphaLowerCI), alphaUpperCI = .clean(alphaUpperCI), lambda = .clean(lambda), 
                glb = .clean(glb), rho = .clean(rho))
    
    # Add footnote: Missing values
    nObs <- nrow(dataset)
    nExcluded <- sum(!complete.cases(dataset))
    nValid <- nObs - nExcluded
    reliabilityScaleTable$addFootnote(message = sprintf("Of the observations, %d were used, %d were excluded%s, and %d were provided.",
                                                        nValid, nExcluded, exclwise, nObs), symbol = "<em>Note.</em>")
    
    # Add footnote: Problems for calculation of glb
    if (options$glbScale) {
      if (length(options$variables) <= 2) {
        reliabilityScaleTable$addFootnote(message = "Greatest lower bound can only be calculated for three or more variables.", symbol = "<em>Warning.</em>", col_names = "case", row_names = options$reverseScaledItems)
      } else if (isTryError(reliabilityResults[["glb"]])) {
        reliabilityScaleTable$addFootnote(message = "Greatest lower bound could not be calculated.", symbol = "<em>Warning.</em>", col_names = "case", row_names = options$reverseScaledItems)
      }
    }
    
  } else {
    
    row <- list(case = "Scale", mean = ".", sd = ".", omega = ".", alpha = ".", alphaLowerCI = ".", 
                alphaUpperCI = ".", lambda = ".", glb = ".", rho = ".")
  }
  
  reliabilityScaleTable$addRows(row)
  
  return(NULL)
}

.reliabilityItemsTable <- function (jaspResults, options, reliabilityResults) {
  
  if (!is.null(jaspResults[["reliabilityItemsTable"]])) {
    return() #The options for this table didn't change so we don't need to rebuild it
  }
  
  reliabilityItemsTable <- createJaspTable("Item Reliability Statistics")
  jaspResults[["reliabilityItemsTable"]] <- reliabilityItemsTable
  reliabilityItemsTable$showSpecifiedColumnsOnly <- TRUE
  reliabilityItemsTable$dependOnOptions(c("variables", "alphaItem", "gutmannItem", "mcDonaldItem", 
                                          "itemRestCor", "meanItem", "sdItem", "reverseScaledItems", 
                                          "missingValues"))
  
  reliabilityItemsTable$addColumnInfo(  name = "case",         title = "",                    type = "string", combine = TRUE)
  if (options$meanItem) {
    reliabilityItemsTable$addColumnInfo(name = "mean",           title = "Mean",                type = "number", format = "sf:4;dp:3")
  }
  if (options$sdItem) {
    reliabilityItemsTable$addColumnInfo(name = "sd",           title = "Std. Deviation",      type = "number", format = "sf:4;dp:3")
  }
  if (options$itemRestCor) {
    reliabilityItemsTable$addColumnInfo(name = "itemRestCor",  title="Item-rest correlation", type = "number", format = "sf:4;dp:3")
  }
  if (options$mcDonaldItem) {
    reliabilityItemsTable$addColumnInfo(name = "omega",        title="McDonald's \u03C9",     type = "number", format = "sf:4;dp:3", overtitle = paste0("If item dropped"))
  }
  if (options$alphaItem) {
    reliabilityItemsTable$addColumnInfo(name = "alpha",        title="Cronbach's \u03B1",     type = "number", format = "sf:4;dp:3", overtitle = paste0("If item dropped"))
  }  
  if (options$gutmannItem) {
    reliabilityItemsTable$addColumnInfo(name = "lambda",       title="Gutmann's \u03BB6",     type = "number", format = "sf:4;dp:3", overtitle = paste0("If item dropped"))
  }  
  
  # Fill up table with results
  .reliabilityItemsResults(reliabilityItemsTable = reliabilityItemsTable, options = options, reliabilityResults = reliabilityResults)
  
  # Add footnote: Omega can only be computed if there are at least 3 variables.
  if (options$mcDonaldItem && length(options$variables) < 3) {
    reliabilityItemsTable$addFootnote(message = "McDonald's \u03C9 if item dropped can only be calculated for three or more variables.", symbol = "<em>Warning.</em>")
  }
  
  # Add footnote: Reverse-scaled items
  if (length(options$reverseScaledItems) > 0) {
    reliabilityItemsTable$addFootnote(message = "reverse-scaled item", symbol = "\u207B", col_names = "case", row_names = options$reverseScaledItems)
  }
  
  return(NULL)
}

.reliabilityItemsResults <- function (reliabilityItemsTable, options, reliabilityResults) {
  
  if (length(options$variables) > 1) {
    
    # psych::alpha uses a minus to signify reverse scaled item. 
    rowNames <- gsub("-","", rownames(reliabilityResults$alpha.drop))
    
    for (var in options$variables) {
      
      # This is the row that will be added to the table
      row <- list()
      
      varV <- .v(var)
      index <- which(varV == rowNames)
      
      mean <-        reliabilityResults[["item.stats"]][index, "mean"]
      sd <-          reliabilityResults[["item.stats"]][index, "sd"]
      itemRestCor <- reliabilityResults[["item.stats"]][index, "r.drop"]
      omega <-       reliabilityResults[["omegaDropped"]][index]
      alpha <-       reliabilityResults[["alpha.drop"]][index, options$alphaNms]
      lambda <-      reliabilityResults[["alpha.drop"]][index, "G6(smc)"]
      
      row <- list(case = var, mean = .clean(mean), sd = .clean(sd), itemRestCor = .clean(itemRestCor), 
                  omega = .clean(omega), alpha = .clean(alpha), lambda = .clean(lambda))
      
      reliabilityItemsTable$addRows(row, rowNames = var)
    }
    
  } else {
      
    row <- list(case = ".", mean = ".", sd = ".", itemRestCor = ".",
                omega = ".", alpha = ".", lambda = ".")
    
    reliabilityItemsTable$addRows(row)
  }
    
  return(NULL)
}

.reliabilityAlphaCI <- function(relyFit, ci, nullAlpha = 0) {

	# code taken and modified from http://www.psyctc.org/stats/R/Feldt1.html
	# considering using the bootstrapped version inside psych as an alternative

	#***********************************************************#
	#* program using methods described in Feldt, Woodruff &    *#
	#* Salih (1987) Applied Psychological Measurement 11(1),   *#
	#* pp. 93-103 to carry out omnibus inferential test of     *#
	#* similarity of alpha values from a single sample         *#
	#***********************************************************#
	
	# relyFit is the output from psych::alpha and must contain the sample size as nObs
	# ci is the width of the confidence interval about obs.a desired
  
	estAlpha = relyFit[["total"]][["raw_alpha"]]
	nVar = relyFit[["nvar"]]
	nObs = relyFit[["nObs"]]

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
