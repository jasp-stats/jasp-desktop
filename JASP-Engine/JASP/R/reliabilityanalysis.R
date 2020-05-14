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

ReliabilityAnalysis <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$variables) != 0

  if(ready) {
    dataset <- .relReadData(dataset, options)
    .reliabilityCheckErrors(dataset, options)
  }
  
  # Output tables
  .reliabilityScaleTable(jaspResults, dataset, options, ready)
  .reliabilityItemTable (jaspResults, dataset, options, ready)

  return()
}

# Preprocessing functions ----
.relReadData <- function(dataset, options) {
  if (!is.null(dataset)) 
    return(dataset)
  else 
    return(.readDataSetToEnd(columns.as.numeric = options$variables, 
                             columns.as.factor  = NULL, 
                             exclude.na.listwise = NULL))
}  

.reliabilityCheckErrors <- function(dataset, options) {
  # Error check 1: 0 observations for a level of a variable
  for (variable in options$variables) {
    
    column <- dataset[[.v(variable)]]
    data   <- column[!is.na(column)]
    levels <- levels(data)
    
    for (level in levels) {
      .hasErrors(
        dataset              = data[data == level],
        type                 = "observations",
        observations.amount  = "< 3",
        exitAnalysisIfErrors = TRUE
      )
    }
  }

  # Error check 2: One or more variables has infinity
  .hasErrors(dataset = dataset, 
             type    = "infinity",
             infinity.target = options$variables,
             exitAnalysisIfErrors = TRUE)
} 

# Results functions ----
.reliabilityComputeResults <- function (jaspResults, dataset, options) {
  # Take results from state if possible
  if (!is.null(jaspResults[["stateReliabilityResults"]])) 
    return(jaspResults[["stateReliabilityResults"]]$object)
  
  # This will be the object that we fill with results
  relyFit <- NULL
  
  if(length(options$variables) > 1) {
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
    # calculate chronbach alpha, gutmanns lambda6, average inter item corrrelation
    key.base64 <- .v(unlist(options$reverseScaledItems))
    relyFit    <- .quietDuringUnitTest(psych::alpha(dataList[["covariance"]], 
                                                    key = key.base64))
    
    # since we supply a correlation matrix and not raw data, we have to add these ourselves
    relyFit[["total"]][["mean"]]      <- mean(dataList[["itemMeans"]])
    relyFit[["total"]][["sd"]]        <- stats::sd(dataList[["itemMeans"]])
    relyFit[["item.stats"]][["mean"]] <- dataList[["itemMeans"]]
    relyFit[["item.stats"]][["sd"]]   <- dataList[["itemSds"]]
    relyFit[["nObs"]]                 <- nObs
    
    # calculate confidence interval for chronbach alpha
    relyFit[["ciAlpha"]] <- .reliabilityAlphaCI(relyFit,	ci = options$confAlphaLevel)
    
    # calculate the greatest lower bound -- only possible for more than 2 variables.
    if (nVar < 3)
      relyFit[["glb"]] <- "."
    else { 
      # try since the glb is error prone icm reverse scaled items. 
      # Requires further investigation/ this might be a bug in psych.
      r <- dataList[["correlation"]]
      glb <- .quietDuringUnitTest(try(psych::glb(r = r, key = key)[["glb.max"]], 
                                      silent = TRUE))
      relyFit[["glb"]] <- glb
    }
    # calculate McDonalds omega
    relyFit[["omega"]] <- .quietDuringUnitTest(psych::omega(m = dataList[["correlation"]], 
                                                            nfactors = 1, key = key, flip = FALSE, 
                                                            plot = FALSE, n.iter = 1, 
                                                            n.obs = nObs)[["omega.tot"]])
    # calculate McDonalds omega if item dropped
    omegaDropped <- NULL
    if (nVar > 2) {
      omegaDropped <- numeric(length = nVar)
      for (i in 1:nVar) {
        m <- dataList[["correlation"]][-i, -i]
        omegaDropped[i] <- .quietDuringUnitTest(psych::omega(m = m, nfactors = 1, n.iter = 1,
                                                             n.obs = nObs, key = key[-i], flip = FALSE, 
                                                             plot = FALSE)[["omega.tot"]])
      }
      relyFit[["omegaDropped"]] <- omegaDropped
    }
  }
  # Save results to state
  jaspResults[["stateReliabilityResults"]] <- createJaspState(relyFit)
  dependList <- c("variables", "reverseScaledItems", "confAlphaLevel", "missingValues")
  jaspResults[["stateReliabilityResults"]]$dependOn(dependList)
  # Return results object
  return(relyFit)
}

.reliabilityScaleFill <- function(jaspResults, dataset, options, relyFit) {
  nVar      <- ncol(dataset)
  options   <- .reliabilitySetAlphaNms(options)
  
  if(nVar >= 2){
    alpha  <- relyFit$total[[options$alphaNms]]
    lambda <- relyFit$total[["G6(smc)"]]
    mu     <- relyFit$total$mean
    sd     <- relyFit$total$sd
    rho    <- relyFit$total$average_r
    omega  <- relyFit$omega
    if (relyFit$glb == "." || isTryError(relyFit$glb)) # unusable information
      glb  <- "."
    else # a useable value
      glb  <- relyFit$glb
    lower <- relyFit[["ciAlpha"]][1]
    upper <- relyFit[["ciAlpha"]][2]
  } else return()

  # Add to table
  jaspResults[["scaleTable"]]$addRows(list(case    = "scale", 
                                           alpha   = alpha, 
                                           lambda  = lambda, 
                                           omega   = omega, 
                                           glb     = glb, 
                                           rho     = rho, 
                                           mu      = mu, 
                                           sd      = sd, 
                                           lower   = lower, 
                                           upper   = upper))
}

.reliabilityItemFill <- function(jaspResults, dataset, options, relyFit) {
  nVar      <- ncol(dataset)
  variables <- unlist(options$variables)
  options   <- .reliabilitySetAlphaNms(options)
  
  # Item table results
  if(!is.null(relyFit)){
    # psych::alpha uses a minus to signify reverse scaled item. 
    rowNames <- gsub("-","", rownames(relyFit$alpha.drop))
    
    if(length(variables) > 1)
      for (var in variables) {
        varV  <- .v(var)
        index <- which(varV == rowNames)
        
        if (var %in% options$reverseScaledItems)
          case <- paste0(var,"\u207B")
        else
          case <- var
        
        alpha       <- relyFit$alpha.drop[index, options$alphaNms]
        lambda      <- relyFit$alpha.drop[index, "G6(smc)"]
        itemRestCor <- relyFit$item.stats[index,"r.drop"]
        mu          <- relyFit$item.stats[index,"mean"]
        sd          <- relyFit$item.stats[index,"sd"]
        omega       <- relyFit$omegaDropped[index]
        
        jaspResults[["itemTable"]]$addRows(list(case        = case, 
                                                alpha       = alpha, 
                                                lambda      = lambda, 
                                                omega       = omega, 
                                                itemRestCor = itemRestCor, 
                                                mu          = mu, 
                                                sd          = sd))
      } 
  }
}

# Output functions ----
.reliabilityScaleTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["scaleTable"]])) return()
  
  # Create table
  scaleTable <- createJaspTable(title = gettext("Scale Reliability Statistics"))
  dependList <- c("variables", "confAlpha", "mcDonaldScale", "alphaScale", "meanScale",
                  "alphaScaleStandardized", "gutmannScale", "glbScale", "reverseScaledItems",
                  "averageInterItemCor",  "sdScale", "missingValues", "confAlphaLevel")
  scaleTable$dependOn(dependList)
  scaleTable$showSpecifiedColumnsOnly <- TRUE
  scaleTable$position <- 1
  
  # Add columns to table
                                    scaleTable$addColumnInfo(name = "case",   title = "",                                        type = "string")
  if (options$meanScale)            scaleTable$addColumnInfo(name = "mu",     title = gettext("mean"),                           type = "number")
  if (options$sdScale)              scaleTable$addColumnInfo(name = "sd",     title = gettext("sd"),                             type = "number")
  if (options$mcDonaldScale)        scaleTable$addColumnInfo(name = "omega",  title = gettext("McDonald's \u03C9"),              type = "number")
  if (options$alphaScale)           scaleTable$addColumnInfo(name = "alpha",  title = gettext("Cronbach's \u03B1"),              type = "number")
  if (options$gutmannScale)         scaleTable$addColumnInfo(name = "lambda", title = gettext("Gutmann's \u03BB6"),              type = "number")
  if (options$glbScale)             scaleTable$addColumnInfo(name = "glb",    title = gettext("Greatest lower bound"),           type = "number")
  if (options$averageInterItemCor)  scaleTable$addColumnInfo(name = "rho",    title = gettext("Average interitem correlation"),  type = "number")

  if (options$confAlpha && options[["alphaScaleStandardized"]] == "_1unstandardized")
  {
    overTitle <- gettextf("%.0f%% Confidence Interval", 100 * options$confAlphaLevel)
    scaleTable$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overTitle)
    scaleTable$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overTitle)
  }
  
  if (options$missingValues == "excludeCasesListwise")  exclwise <- "listwise"
  else                                                  exclwise <- "pairwise"
  
  jaspResults[["scaleTable"]] <- scaleTable
  
  if(!ready)
    return()
  # Compute/get the results
  relyFit <- .reliabilityComputeResults(jaspResults, dataset, options)
  
  .reliabilityScaleFill(jaspResults, dataset, options, relyFit)
  
  variables <- unlist(options$variables)
  if(length(variables) >= 2) {
    nObs      <- nrow(dataset)
    nExcluded <- sum(!complete.cases(dataset))
    nValid    <- nObs - nExcluded
    message   <- gettextf("Of the observations, %1$i were used, %2$i were excluded %3$s, and %4$i were provided.", nValid, nExcluded, exclwise, nObs)
    
    if (options$glbScale && length(variables) == 2) 
      message <- gettextf("%s\nWarning: Greatest lower bound can only be calculated for three or more variables.", message)

    jaspResults[["scaleTable"]]$addFootnote(message)
  }
}

.reliabilityItemTable <- function (jaspResults, dataset, options, ready) {
  if (!(options$alphaItem || options$gutmannItem || options$itemRestCor ||
        options$meanItem  ||options$mcDonaldItem || options$sdItem) ||
      !is.null(jaspResults[["itemTable"]]))
    return()
  
  # Create table
  itemTable <- createJaspTable("Item Reliability Statistics")
  dependList <- c("variables", "reverseScaledItems","mcDonaldItem", "alphaItem", 
                  "gutmannItem", "meanItem", "sdItem", "itemRestCor", "alphaScaleStandardized")
  itemTable$dependOn(dependList)
  itemTable$showSpecifiedColumnsOnly <- TRUE
  itemTable$position <- 2

  overTitle <- gettext("If item dropped")
  
  # Add columns to table
                            itemTable$addColumnInfo(name = "case",        title = "",                                   type = "string")
  if (options$meanItem)     itemTable$addColumnInfo(name = "mu",          title = gettext("mean"),                      type = "number")
  if (options$sdItem)       itemTable$addColumnInfo(name = "sd",          title = gettext("sd"),                        type = "number")
  if (options$itemRestCor)  itemTable$addColumnInfo(name = "itemRestCor", title = gettext("item-rest correlation"),     type = "number")
  if (options$mcDonaldItem) itemTable$addColumnInfo(name = "omega",       title = gettext("McDonald's \u03C9"),         type = "number", overtitle = overTitle)
  if (options$alphaItem)    itemTable$addColumnInfo(name = "alpha",       title = gettext("Cronbach's \u03B1"),         type = "number", overtitle = overTitle)
  if (options$gutmannItem)  itemTable$addColumnInfo(name = "lambda",      title = gettext("Gutmann's \u03BB6"),         type = "number", overtitle = overTitle)
  
  # can only be computed if there are at least 3 variables.
  if (options$mcDonaldItem && length(options$variables) < 3) {
    message <- gettext("Warning: McDonald's \u03C9 if item dropped can only be calculated for three or more variables.")
    itemTable$addFootnote(message, colName = "omega")
  }
  
  jaspResults[["itemTable"]] <- itemTable 
  
  if(!ready)
    return()
  
  # Compute/get the results
  relyFit <- .reliabilityComputeResults(jaspResults, dataset, options)

  .reliabilityItemFill(jaspResults, dataset, options, relyFit)
  
  if (length(options$reverseScaledItems) > 0)
    itemTable$addFootnote(gettext("reverse-scaled item"), symbol = "\u207B")
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
  
  estAlpha <- relyFit[["total"]][["raw_alpha"]]
  nVar     <- relyFit[["nvar"]]
  nObs     <- relyFit[["nObs"]]
  
  f <- (1 - nullAlpha) / (1 - estAlpha)
  if(estAlpha > nullAlpha)
    f <- 1/f

  nDen <- (nObs - 1) * (nVar - 1)
  nNum <- nObs - 1
  # set the upper and lower p values for the desired C.I.
  null.p <- stats::pf(f, nNum, nDen) 
  p1     <- (1 - ci)/2
  p2     <- ci + p1 
  # corresponding F values
  f1     <- stats::qf(p1, nNum, nDen)
  f2     <- stats::qf(p2, nNum, nDen) 
  lower  <- 1 - (1 - estAlpha) * f2
  upper  <- 1 - (1 - estAlpha) * f1
  return(c(lower, upper))
}

.reliabilityConvertDataToCorrelation <- function(dataset, options) {
  
  if (options[["missingValues"]] == "excludeCasesListwise")
    dataset <- dataset[complete.cases(dataset), ]
  
  means  <- colMeans(dataset, na.rm = TRUE)
  covmat <- stats::cov(dataset, use = "pairwise")
  stdev  <- sqrt(diag(covmat))
  cormat <- psych::cor.smooth(stats::cov2cor(covmat), 
                              eig.tol = sqrt(.Machine[["double.eps"]]))
  
  return(list(
    correlation = cormat,
    itemSds     = stdev,
    itemMeans   = means,
    # direct line from: corpcor::rebuild.cov
    covariance  = sweep(sweep(cormat, 1, stdev, "*"), 2, stdev, "*")
  ))
}

.reliabilitySetAlphaNms <- function(options) {
  # do we look up raw or standardized alpha?
  if (options[["alphaScaleStandardized"]][[1]] == "_2standardized")
    options$alphaNms <- "std.alpha"
  else 
    options$alphaNms <- "raw_alpha"
  return(options)
}
