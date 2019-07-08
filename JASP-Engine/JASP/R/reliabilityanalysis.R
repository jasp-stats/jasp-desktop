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

ReliabilityAnalysis <- function(jaspResults, dataset, options, ...) {
  
  # Read dataset
  dataset <- .relReadData(dataset, options)
  
  # Error checking
  errors <- .relCheckErrors(dataset, options)
  
  # Compute the results
  reliabilityResults <- .reliabilityComputeResults(jaspResults, dataset, options, errors)
  
  # Output tables
  .reliabilityScaleTable(jaspResults, dataset, options, reliabilityResults, errors)
  .reliabilityItemTable (jaspResults, dataset, options, reliabilityResults, errors)
  
  return()
}

# Preprocessing functions ----
.relReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    return(.readDataSetToEnd(columns.as.numeric = options$variables, columns.as.factor = NULL, exclude.na.listwise = NULL))
  }
}  

.relCheckErrors <- function(dataset, options) {
  
  # Check if results can be computed
  if (length(options$variables) == 0) return("No variables")
  
  # Error Check 1: Number of levels of the variables
  .hasErrors(
    dataset              = dataset,
    perform              = "run",
    type                 = "factorLevels",
    factorLevels.target  = options$variables,
    factorLevels.amount  = "< 1",
    exitAnalysisIfErrors = TRUE
  )
  
  # Error check 2: 0 observations for a level of a variable
  for (variable in options$variables) {
    
    column <- dataset[[.v(variable)]]
    data   <- column[!is.na(column)]
    levels <- levels(data)
    
    for (level in levels) {
      .hasErrors(
        dataset              = data[data == level],
        perform              = "run",
        type                 = "observations",
        observations.amount  = "< 1",
        exitAnalysisIfErrors = TRUE
      )
    }
  }
} 

# Results functions ----
.reliabilityComputeResults <- function (jaspResults, dataset, options, errors) {
  
  if (!is.null(errors) && errors == "No variables") return()
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateReliabilityResults"]])) 
    return(jaspResults[["stateReliabilityResults"]]$object)
  
  # This will be the object that we fill with results
  relyFit <- NULL
  
  #These will store the cleaner results for the tables
  results <- list()
  results[["scale"]] <- list()
  results[["item"]]  <- list()
  
  # obtain smoothed correlation and covariance matrix
  dataList  <- .reliabilityConvertDataToCorrelation(dataset, options)
  nObs      <- nrow(dataset)
  nVar      <- ncol(dataset)
  variables <- unlist(options$variables)
  
  # generate key for reverse scaled items
  key <- NULL
  if (length(options$reverseScaledItems) > 0) {
    key <- rep(1, length(variables))
    key[match(.v(unlist(options$reverseScaledItems)), colnames(dataset))] <- -1
  }
  
  if(length(variables) > 1) {
    # calculate chronbach alpha, gutmanns lambda6, and average inter item corrrelation
    relyFit <- .quietDuringUnitTest(psych::alpha(dataList[["covariance"]], 
                                                 key = .v(unlist(options$reverseScaledItems))))
    
    # because we supply a correlation matrix and not raw data, we have to add these ourselves
    relyFit[["total"]][["mean"]]      <- mean(dataList[["itemMeans"]])
    relyFit[["total"]][["sd"]]        <- stats::sd(dataList[["itemMeans"]])
    relyFit[["item.stats"]][["mean"]] <- dataList[["itemMeans"]]
    relyFit[["item.stats"]][["sd"]]   <- dataList[["itemSds"]]
    relyFit[["nObs"]]                 <- nObs
    
    # calculate confidence interval for chronbach alpha
    relyFit[["ciAlpha"]] <- .reliabilityAlphaCI(relyFit,	ci = options$confAlphaLevel)
    
    # calculate the greatest lower bound -- only possible for more than 2 variables.
    if (nVar < 3) {
      relyFit[["glb"]] <- "."
    } else { # try since the glb is error prone icm reverse scaled items. Requires further investigation/ this might be a bug in psych.
      relyFit[["glb"]] <- .quietDuringUnitTest(try(psych::glb(r = dataList[["correlation"]], key = key)[["glb.max"]], silent = TRUE))
    }
    # calculate McDonalds omega
    relyFit[["omega"]] <- .quietDuringUnitTest(psych::omega(m = dataList[["correlation"]], nfactors = 1, flip = FALSE, 
                                                            plot = FALSE,  n.iter = 1, n.obs = nObs)[["omega.tot"]])
    # calculate McDonalds omega if item dropped
    omegaDropped <- NULL
    if (nVar > 2) {
      omegaDropped <- numeric(length = nVar)
      for (i in 1:nVar) {
        omegaDropped[i] <- .quietDuringUnitTest(psych::omega(m = dataList[["correlation"]][-i, -i], 
                                                             nfactors = 1, n.iter = 1, n.obs = nObs,
                                                             flip = FALSE, plot = FALSE)[["omega.tot"]])
      }
      relyFit[["omegaDropped"]] <- omegaDropped
    }
    
    # do we look up raw or standardized alpha?
    if (options[["alphaScaleStandardized"]][[1]] == "_2standardized") {
      options$alphaNms <- "std.alpha"
    } else {
      options$alphaNms <- "raw_alpha"
    }
  }
  if(!is.null(relyFit)){
    alpha  <- NULL
    lambda <- NULL
    mu     <- NULL
    sd     <- NULL
    rho    <- NULL
    omega  <- NULL
    glb    <- NULL
    lower  <- NULL
    upper  <- NULL
    
    if(nVar>=2){
      if (options$alphaScale)
        alpha  <- .clean(relyFit$total[[options$alphaNms]])
      if (options$gutmannScale)
        lambda <- .clean(relyFit$total[["G6(smc)"]])
      if (options$meanScale)
        mu     <- .clean(relyFit$total$mean)
      if (options$sdScale)
        sd     <- .clean(relyFit$total$sd)
      if (options$averageInterItemCor)
        rho    <- .clean(relyFit$total$average_r)
      if (options$mcDonaldScale)
        omega  <- .clean(relyFit$omega)
      if (options$glbScale) {
        if (relyFit$glb == "." || isTryError(relyFit$glb)) { # unusable information
          glb  <- "."
        } else { # a useable value
          glb  <- .clean(relyFit$glb)
        }
      }
      if (options[["confAlpha"]]) {
        lower <- .clean(relyFit[["ciAlpha"]][1])
        upper <- .clean(relyFit[["ciAlpha"]][2])
      }
    }
  }
  else {
    alpha  <- "."
    lambda <- "."
    omega  <- "."
    glb    <- "."
    rho    <- "."
    mu     <- "."
    sd     <- "."
    lower  <- "."
    upper  <- "."
  }
  results[["scale"]] <- list(
    case    = "scale", 
    alpha   = alpha, 
    lambda  = lambda, 
    omega   = omega, 
    glb     = glb, 
    rho     = rho, 
    mu      = mu, 
    sd      = sd, 
    lower   = lower, 
    upper   = upper
  )
  
  # Item table results
  if(!is.null(relyFit)){
    # psych::alpha uses a minus to signify reverse scaled item. 
    rowNames <- gsub("-","", rownames(relyFit$alpha.drop))
    
    if(length(variables) > 1){
      for (var in variables) {
        varV  <- .v(var)
        index <- which(varV == rowNames)
        
        alpha       <- NULL
        lambda      <- NULL
        itemRestCor <- NULL
        mu          <- NULL
        sd          <- NULL
        omega       <- NULL
        
        if (var %in% options$reverseScaledItems) {
          case <- paste0(var,"\u207B")
        } else {
          case <- var
        }
        
        if (options$alphaItem)
          alpha       <- .clean(relyFit$alpha.drop[index, options$alphaNms])
        if (options$gutmannItem)
          lambda      <- .clean(relyFit$alpha.drop[index, "G6(smc)"])
        if (options$itemRestCor)
          itemRestCor <- .clean(relyFit$item.stats[index,"r.drop"])
        if (options$meanItem)
          mu          <- .clean(relyFit$item.stats[index,"mean"])
        if (options$sdItem)
          sd          <- .clean(relyFit$item.stats[index,"sd"])
        if (options$mcDonaldItem)
          omega       <- .clean(relyFit$omegaDropped[index])
        
        results[["item"]][[var]] <- list(
          case        = case, 
          alpha       = alpha, 
          lambda      = lambda, 
          omega       = omega, 
          itemRestCor = itemRestCor, 
          mu          = mu, 
          sd          = sd
        )
      } 
    }
  }
  else {
    variablesTemp <- variables
    
    if (is.null(variables)) 
      variablesTemp <- "..."
    
    for (var in variablesTemp) {
      results[["item"]][[var]] <- list(
        case        = var, 
        alpha       = ".", 
        lambda      = ".", 
        omega       = ".", 
        itemRestCor = ".", 
        mu          = ".", 
        sd          = "."
      )
    }
  }
  # Save results to state
  jaspResults[["stateReliabilityResults"]] <- createJaspState(results)
  jaspResults[["stateReliabilityResults"]]$dependOn(c("variables", "reverseScaledItems", "mcDonaldItem", "alphaItem", "gutmannItem", 
                                                      "meanItem", "sdItem", "itemRestCor", "confAlpha", "mcDonaldScale", "alphaScale", 
                                                      "alphaScaleStandardized", "gutmannScale", "glbScale", "averageInterItemCor", 
                                                      "meanScale", "sdScale", "missingValues", "normalScaledItems", "confAlpha"))
  # Return results object
  return(results)
}

# Output functions ----
.reliabilityScaleTable <- function (jaspResults, dataset, options, reliabilityResults, errors) {
  
  if (!is.null(jaspResults[["reliabilityScaleTable"]])) return()
  
  # Create table
  reliabilityScaleTable <- createJaspTable(title = "Scale Reliability Statistics")
  reliabilityScaleTable$dependOn(c("variables", "mcDonaldScale", "alphaScale", "alphaScaleStandardized",
                                   "gutmannScale", "glbScale", "averageInterItemCor", "meanScale", "sdScale", "missingValues", 
                                   "normalScaledItems", "confAlpha", "confAlphaLevel"))
  reliabilityScaleTable$showSpecifiedColumnsOnly <- TRUE
  reliabilityScaleTable$position <- 1
  
  # Add columns to table
  reliabilityScaleTable$addColumnInfo(name = "case", title = "", type = "string")
  if (options$meanScale)
    reliabilityScaleTable$addColumnInfo(name = "mu",    title = "mean",                          type = "number", format = "sf:4;dp:3")
  if (options$sdScale)
    reliabilityScaleTable$addColumnInfo(name = "sd",    title = "sd",                            type = "number", format = "sf:4;dp:3")
  if (options$mcDonaldScale)
    reliabilityScaleTable$addColumnInfo(name = "omega", title = "McDonald's \u03C9",             type = "number", format = "sf:4;dp:3")
  if (options$alphaScale)
    reliabilityScaleTable$addColumnInfo(name = "alpha", title = "Cronbach's \u03B1",             type = "number", format = "sf:4;dp:3")
  if (options$gutmannScale)
    reliabilityScaleTable$addColumnInfo(name = "lambda",title = "Gutmann's \u03BB6",             type = "number", format = "sf:4;dp:3")
  if (options$glbScale)
    reliabilityScaleTable$addColumnInfo(name = "glb",   title = "Greatest lower bound",          type = "number", format = "sf:4;dp:3")
  if (options$averageInterItemCor)
    reliabilityScaleTable$addColumnInfo(name = "rho",   title = "Average interitem correlation", type = "number", format = "sf:4;dp:3")
  if (options$confAlpha){
    overTitle <- paste0(100*options$confAlphaLevel, "% Confidence Interval")
    reliabilityScaleTable$addColumnInfo(name = "lower", title = "Lower", type = "number", format = "sf:4;dp:3", overtitle = overTitle)
    reliabilityScaleTable$addColumnInfo(name = "upper", title = "Upper", type = "number", format = "sf:4;dp:3", overtitle = overTitle)
  }
  
  if (options$missingValues == "excludeCasesListwise") {
    exclwise = " listwise"
  } else {
    exclwise = " pairwise"
  }
  
  variables <- unlist(options$variables)
  nObs      <- nrow(dataset)
  nExcluded <- sum(!complete.cases(dataset))
  nValid    <- nObs - nExcluded
  
  # message <- paste("Scale consists of items ", paste0(variables, collapse = ", "))
  message <- sprintf("Of the observations, %d were used, %d were excluded%s, and %d were provided.",
                     nValid, nExcluded, exclwise, nObs)
  if (options$glbScale) {
    if (length(variables) <= 2) {
      message <- paste(message, "Warning: Greatest lower bound can only be calculated for three or more variables.")
    } else if (isTryError(reliabilityResults[["scale"]]$glb) || reliabilityResults[["scale"]]$glb == ".") {
      message <- paste(message, "Warning: Greatest lower bound could not be calculated.")
    }
  }
  reliabilityScaleTable$addFootnote(message, symbol = "<em>Note.</em>")
  
  jaspResults[["reliabilityScaleTable"]] <- reliabilityScaleTable
  
  if (!is.null(errors) && errors == "No variables")
    row <- list(case = "scale", alpha = ".", lambda = ".", omega = ".", glb = ".", rho = ".", mean = ".", sd = ".", lower = ".", upper = ".")
  else
    row <- reliabilityResults[["scale"]][1:10]
  reliabilityScaleTable$addRows(row)
}

.reliabilityItemTable <- function (jaspResults, dataset, options, reliabilityResults, errors) {
  
  if (!is.null(jaspResults[["reliabilityItemContainer"]])) 
    return()
  
  if (!options$alphaItem && !options$gutmannItem && !options$itemRestCor && !options$meanItem && !options$sdItem && !options$mcDonaldItem)
    return()
  
  jaspResults[["reliabilityItemContainer"]] <- createJaspContainer("Item Statistics")
  
  # Create table
  reliabilityItemTable <- createJaspTable("Item Reliability Statistics")
  reliabilityItemTable$showSpecifiedColumnsOnly <- TRUE
  reliabilityItemTable$position <- 2
  
  overTitle <- paste0("If item dropped")
  
  # Add columns to table
  reliabilityItemTable$addColumnInfo(name = "case", title = "", type = "string")
  if (options$meanItem)
    reliabilityItemTable$addColumnInfo(name = "mu", title = "mean", type = "number", format = "sf:4;dp:3")
  if (options$sdItem)
    reliabilityItemTable$addColumnInfo(name = "sd", title = "sd",   type = "number", format = "sf:4;dp:3")
  if (options$itemRestCor)
    reliabilityItemTable$addColumnInfo(name = "itemRestCor", title = "item-rest correlation", type = "number", format = "sf:4;dp:3")
  if (options$mcDonaldItem)
    reliabilityItemTable$addColumnInfo(name = "omega", title = "McDonald's \u03C9", type = "number", format = "sf:4;dp:3", overtitle = overTitle)
  if (options$alphaItem)
    reliabilityItemTable$addColumnInfo(name = "alpha", title = "Cronbach's \u03B1", type = "number", format = "sf:4;dp:3", overtitle = overTitle)
  if (options$gutmannItem)
    reliabilityItemTable$addColumnInfo(name = "lambda",title = "Gutmann's \u03BB6", type = "number", format = "sf:4;dp:3", overtitle = overTitle)
  
  # can only be computed if there are at least 3 variables.
  if (options$mcDonaldItem && length(options$variables) < 3) {
    message <- "McDonald's \u03C9 if item dropped can only be calculated for three or more variables."
    reliabilityItemTable$addFootnote(message, symbol = "\u1D43 Warning: ")
  }
  
  jaspResults[["reliabilityItemContainer"]][["table"]] <- reliabilityItemTable 
  jaspResults[["reliabilityItemContainer"]]$dependOn(c("variables", "reverseScaledItems", "mcDonaldItem", "alphaItem", 
                                                       "gutmannItem", "meanItem", "sdItem", "itemRestCor"))
  
  for(var in options$variables) {
    row <- reliabilityResults[["item"]][[var]][1:7]
    reliabilityItemTable$addRows(row)
  }
  
  if (length(options$reverseScaledItems) > 0) {
    reverseScaledItems <- unlist(options$reverseScaledItems)
    message <- "reverse-scaled item"
    reliabilityItemTable$addFootnote(message, symbol = "\u207B", rowNames = reverseScaledItems, colNames = "case")
  }
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
