#
# Copyright (C) 2013-2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.	If not, see <http://www.gnu.org/licenses/>.
#

PrincipalComponentAnalysis2 <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Revelle, W. (2018) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, https://CRAN.R-project.org/package=psych Version = 1.8.12.")
  
  # Read dataset
  dataset <- .pcaReadData(dataset, options)
  ready   <- .pcaCheckErrors(dataset, options)
  
  modelContainer <- .pcaModelContainer(jaspResults)
}

# Preprocessing functions ----
.pcaReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)
  
  return(.readDataSetToEnd(columns = unlist(options$variables)))
}

.pcaCheckErrors <- function(dataset, options) {
  customChecksPCAEFA <- list(
    function() {
      if (length(options$variables) > 0 && options$numberOfFactors > length(options$variables)) {
        return(paste0("Too many factors requested (", options$numberOfFactors, 
                      ") for the amount of included variables"))
      }
    },
    function() {
      if(nrow(dataset) < 3){
        return(paste0("Not enough valid cases (", nrow(dataset),") to run this analysis"))
      }
    },
    # check whether all row variance == 0
    function() {
      varianceZero <- 0
      for (i in 1:nrow(dataset)){
        if(sd(dataset[i,], na.rm = TRUE) == 0) varianceZero <- varianceZero + 1
      }
      if(varianceZero == nrow(dataset)){
        return("Data not valid: variance is zero in each row")
      }
    },
    # check whether all variables correlate with each other
    function() {
      allCorr <- 0
      nVar <- ncol(dataset)
      for (i in 1:(nVar-1)) {
        for (j in (i+1):nVar) {
          thisCor <- cor(dataset[,i],dataset[,j])
          if(!is.na(thisCor) && thisCor == 1) allCorr <- allCorr + 1
        }
      }
      if(allCorr == nVar*(nVar-1)/2){
        return("Data not valid: all variables correlate with each other")
      }
    }
  )
  error <- .hasErrors(dataset = dataset, type = c("infinity", "variance"), custom = customChecksPCAEFA, 
                      exitAnalysisIfErrors = TRUE)
  return(length(options$variables) > 0)
}

.pcaModelContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("rotationMethod", "orthogonalSelector", "obliqueSelector", "variables", "factorMethod", 
                              "eigenValuesBox", "numberOfFactors"))
    jaspResults[["modelContainer"]] <- modelContainer
  }
  
  return(modelContainer)
}



# Results functions ----

.pcaComputeResults <- function(modelContainer, dataset, options, ready) {
  pcaResult <- try(
    psych::principal(
      r        = dataset,
      nfactors = .pcaGetNComp(dataset, options),
      rotate   = ifelse(options$rotationMethod == "orthogonal", options$orthogonalSelector, options$obliqueSelector), 
      scores   = TRUE
    )
  )
  
  if (inherits(medResult, "try-error")) {
    errmsg <- paste("Estimation failed\nMessage:\n", attr(medResult, "condition")$message)
    modelContainer$setError(.decodeVarsInMessage(names(dataset), errmsg))
  }
  
  if (options$se == "bootstrap") {
    startProgressbar(options$bootstrapNumber)
    
    boot_1      <- lavaan::bootstrapLavaan(medResult, R = 1)
    bootres     <- matrix(0, options$bootstrapNumber, length(boot_1))
    bootres[1,] <- boot_1
    i <- 2L
    while (i <= options$bootstrapNumber) {
      boot_i      <- lavaan::bootstrapLavaan(medResult, 1)
      if (length(boot_i) == 0) next # try again upon failure
      bootres[i,] <- boot_i
      progressbarTick()
      i <- i + 1L
    }
    
    medResult@boot       <- list(coef = bootres)
    medResult@Options$se <- "bootstrap"
  }
  
  modelContainer[["model"]] <- createJaspState(medResult)
  return(medResult)
}

.pcaGetNComp <- function(dataset, options) {
  if (options$factorMethod == "manual")           return(options$numberOfFactors)
  fa <- try(psych::fa.parallel(dataset, plot = FALSE))
  if (inherits(fa, "try-error"))                  return(1)
  if (options$factorMethod == "parallelAnalysis") return(max(1, fa$ncomp))
  if (options$factorMethod == "eigenValues")      return(sum(fa$pc.values > options$eigenValuesBox))
}
