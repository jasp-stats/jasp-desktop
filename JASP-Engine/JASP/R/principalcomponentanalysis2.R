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
  
  # output functions
  .pcaGoFTable(           modelContainer, dataset, options, ready)
  .pcaLoadingsTable(      modelContainer, dataset, options, ready)
  .pcaCorrTable(          modelContainer, dataset, options, ready)
  
  # data saving
  .pcaAddComponentsToData(jaspResults, modelContainer, options, ready)
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
  
  if (inherits(pcaResult, "try-error")) {
    errmsg <- paste("Estimation failed\nMessage:\n", attr(pcaResult, "condition")$message)
    modelContainer$setError(.decodeVarsInMessage(names(dataset), errmsg))
  }
  
  modelContainer[["model"]] <- createJaspState(pcaResult)
  return(pcaResult)
}

.pcaGetNComp <- function(dataset, options) {
  if (options$factorMethod == "manual")           return(options$numberOfFactors)
  fa <- try(psych::fa.parallel(dataset, plot = FALSE))
  if (inherits(fa, "try-error"))                  return(1)
  if (options$factorMethod == "parallelAnalysis") return(max(1, fa$ncomp))
  if (options$factorMethod == "eigenValues")      return(sum(fa$pc.values > options$eigenValuesBox))
}

# Output functions ----
.pcaGoFTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["goftab"]])) return()
  
  goftab <- createJaspTable(title = "Chi-squared Test")
  goftab$addColumnInfo(name = "model", title = "",      type = "string")
  goftab$addColumnInfo(name = "chisq", title = "Value", type = "number", format = "dp:3")
  goftab$addColumnInfo(name = "df",    title = "df",    type = "integer")
  goftab$addColumnInfo(name = "p",     title = "p",     type = "number", format = "dp:3;p:.001")
  goftab$position <- 1
  
  modelContainer[["goftab"]] <- goftab
  
  if (!ready) return()
  
  pcaResults <- .pcaComputeResults(modelContainer, dataset, options)
  
  goftab[["model"]] <- "Model"
  goftab[["chisq"]] <- pcaResults$STATISTIC
  goftab[["df"]]    <- pcaResults$dof
  goftab[["p"]]     <- pcaResults$PVAL
  
  if (pcaResults$dof < 0)
    goftab$addFootnote(message = "Degrees of freedom below 0, model is unidentified.", symbol = "<em>Note.</em>")
}

.pcaLoadingsTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["loatab"]]) || !ready) return()
  loatab <- createJaspTable("Component Loadings")
  loatab$dependOn("highlightText")
  loatab$position <- 2
  modelContainer[["loatab"]] <- loatab
  
  if (modelContainer$getError()) return()
  
  pcaResults <- modelContainer[["model"]][["object"]]
  
  coltitle <- ifelse(options$rotationMethod == "orthogonal", "PC", "RC")
  if (options$rotationMethod == "orthogonal" && options$orthogonalSelector == "none") {
    loatab$addFootnote(message = "No rotation method applied.", symbol = "<em>Note.</em>")
  } else {
    loatab$addFootnote(
      message = paste0(
        "Applied rotation method is ", 
        ifelse(options$rotationMethod == "orthogonal", options$orthogonalSelector, options$obliqueSelector),
        "."
      ),
      symbol = "<em>Note.</em>"
    )
  }
  
  loads <- loadings(pcaResults)
  loatab$addColumnInfo(name = "var", title = "", type = "string")
  loatab[["var"]] <- .unv(rownames(loads))
  
  for (i in 1:ncol(loads)) {
    # fix weird "all true" issue
    if (all(abs(loads[, i]) < options$highlightText)) {
      loatab$addColumnInfo(name = paste0("c", i), title = paste0(coltitle, i), type = "string")
      loatab[[paste0("c", i)]] <- rep("", nrow(loads))
    } else {
      loatab$addColumnInfo(name = paste0("c", i), title = paste0(coltitle, i), type = "number", format = "dp:3")
      loatab[[paste0("c", i)]] <- ifelse(abs(loads[, i]) < options$highlightText, NA, loads[ ,i])
    }
  }
  
  loatab$addColumnInfo(name = "uni", title = "Uniqueness", type = "number", format = "dp:3")
  loatab[["uni"]] <- pcaResults$uniquenesses
}

.pcaCorrTable <- function(modelContainer, dataset, options, ready) {
  if (!options[["incl_correlations"]] || !is.null(modelContainer[["cortab"]]) || !ready) return()
  cortab <- createJaspTable("Component Correlations")
  cortab$dependOn("incl_correlations")
  cortab$position <- 3
  modelContainer[["cortab"]] <- cortab
  
  if (modelContainer$getError()) return()
  
  coltitle <- ifelse(options$rotationMethod == "orthogonal", "PC", "RC")
  cors <- zapsmall(modelContainer[["model"]][["object"]][["r.scores"]])
  dims <- ncol(cors)
  
  
  cortab$addColumnInfo(name = "col", title = "", type = "string")
  cortab[["col"]] <- paste0(coltitle, 1:dims)
  
  for (i in 1:dims) {
    thisname <- paste0(coltitle, i)
    cortab$addColumnInfo(name = thisname, title = thisname, type = "number", format = "dp:3")
    cortab[[thisname]] <- cors[,i]
  }
  
}

.pcaAddComponentsToData <- function(jaspResults, modelContainer, options, ready) {
  if(!ready || !options[["addPC"]] || options[["PCPrefix"]] == "" || modelContainer$getError()) return()
  
  scores <- modelContainer[["model"]][["object"]][["scores"]]
  
  for (i in 1:ncol(scores)) {
    scorename <- paste0(options[["PCPrefix"]], "_", i)
    if (is.null(jaspResults[[scorename]])) {
      jaspResults[[scorename]] <- createJaspColumn(scorename)
      jaspResults[[scorename]]$dependOn(optionsFromObject = modelContainer)
      jaspResults[[scorename]]$setScale(scores[, i])
    }
  }
}
