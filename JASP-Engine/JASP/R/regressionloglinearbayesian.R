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

RegressionLogLinearBayesian <- function(jaspResults, dataset, options, ...) {
  # Read dataset
  dataset <- .basRegLogLinReadData(dataset, options)
  
  ready <- length(options$factors) != 0
  
  # Error checking
  .basRegLogLinCheckErrors(dataset, options, ready)
  
  # Container
  .basRegLogLinContainer(jaspResults, dataset, options)

  # Output tables (each calls its own results function)
  .basRegLogLinMainTable(      jaspResults, dataset, options, ready)
  .basRegLogLinSummaryTable(   jaspResults, dataset, options, ready)
  .basRegLogLinSubSummaryTable(jaspResults, dataset, options, ready)
  
  return()
}

# Preprocessing functions 
.basRegLogLinReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else {
    counts  <- NULL
    factors <- NULL
    if(options$counts != "")
      counts <- options$counts
    if(length(options$modelTerms) > 0)
      factors <- options$modelTerms
    return(.readDataSetToEnd(columns.as.factor = factors, 
                             columns.as.numeric = counts))
  }
}  

.basRegLogLinCheckErrors <- function(dataset, options, ready) {
  # Error Check 1
  if (!ready) 
    return()
  
  args <- list(
    dataset = dataset,
    type    = c("missingValues", "modelInteractions"),
    modelInteractions.modelTerms = options$modelTerms,
    missingValues.target = options$factors,
    exitAnalysisIfErrors = TRUE
  )
  
  if (options$counts != "") {
    args$type <- c(args$type, "infinity", "negativeValues")
    args$missingValues.target <- c(options$counts, options$factors)
  }
  
  do.call(.hasErrors, args)
  
  # Error check 2: 0 observations for a level of a variable
  for (factor in options$factors) {
    column <- dataset[[.v(factor)]]
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

# Compute results 
.basRegLogLinComputeBFObject <- function(jaspResults, dataset, options) {
  if(!is.null(jaspResults[["Container"]][["bfObject"]])) 
    return(jaspResults[["Container"]][["bfObject"]]$object)
  
  bfObject <- list("bcctObj"        = NULL, 
                   "variables"      = c("...", "... "),
                   "nModelsVisited" = NULL,
                   "nBurnIn"        = NULL,
                   "bf10s"          = rep(".", length = 2), 
                   "postModelProbs" = rep(".", length = 2), 
                   "modelNames"     = NULL)
  numberOfModels   <- length(options$modelTerms)
  variablesInModel <- NULL
  bcctObj          <- NULL

  if (options$counts == "")
    dataset <- plyr::count(dataset)
  
  # Extract models needed to be compared
  if (options$counts == "")
    dependentVariable <- "freq"
  else
    dependentVariable <- unlist(options$counts)
  
  dependentBase64 <- .v(dependentVariable)
  
  if (length(options$modelTerms) > 0) {
    variablesInModel <- variablesInModelBase64 <- NULL
    
    for (i in seq_along(options$modelTerms)) {
      components <- options$modelTerms[[i]]$components
      
      if (length(components) == 1) {
        term <- components[[1]]
        termBase64 <- .v(components[[1]])
      } else {
        componentsUnlisted <- unlist(components)
        term       <- paste0(componentsUnlisted, collapse = ":")
        termBase64 <- paste0(.v(componentsUnlisted), collapse = ":")
      }
      # Add to tally
      variablesInModel       <- c(variablesInModel, term)
      variablesInModelBase64 <- c(variablesInModelBase64, termBase64)
      
      # Remove empty stuff
      variablesInModel <- variablesInModel[variablesInModel != ""]
    }
  }
  
  # Prune the variables
  if (length(variablesInModel) == 0) {
    variablesInModel <- c("...", "... ")
    modelDefinition  <- NULL #this model has no parameters
  } else if (length(variablesInModel) == 1 && options$counts == "") {
    variablesInModel <- c(variablesInModel, "... ")
    modelDefinition  <- NULL #this model has only one parameter
  } else if (length(variablesInModel) > 1 || options$counts != "") {
    modelDefinition <- paste(dependentBase64, "~", 
                             paste(variablesInModelBase64, collapse = "+"))
  } else {
    # Nothing worked out:
    modelDefinition <- NULL #this model has no parameters
    stop("variables cannot be read")
  }
  
  # Save in object
  bfObject$variables <- variablesInModel
  
  # START analysis Bayesian Log Linear regression
  if (!is.null(modelDefinition)) {
    modelFormula <- as.formula(modelDefinition)
    
    if (options$counts == "")
      names(dataset)[names(dataset) == "freq"] <- dependentBase64
    
    # Calculate here
    #gives an object computed using Bayesian Analysis of Complete Contingency Tables
    bcctObj <- try(conting::bcct(formula = modelFormula, data = dataset, 
                                 prior = "SBH", n.sample = 2000, 
                                 a = options$priorShape, b = options$priorScale), 
                   silent = TRUE)
    bfObject$nBurnIn <- 2000 * 0.2
    
    # Always do auto and then manual adds additional samples
    if (options$sampleMode == "manual"){
      bcctObj <- try(conting::bcctu(object = bcctObj, 
                                    n.sample = options$fixedSamplesNumber), 
                     silent = TRUE)
      bfObject$nBurnIn <- (2000 + options$fixedSamplesNumber) * 0.2
    }
    
    # bcct object checking
    if(inherits(bcctObj, "bcct"))
      bfObject$bcctObj <- bcctObj
  }
  
  # Post processing
  if (inherits(bfObject$bcctObj, "bcct")) {
    # Good case
    # TODO: Here check bcctSummary$totmodsvisit if this is one, 
    # then nothing going on, resample
    #
    bcctSummary <- try(conting::mod_probs(bfObject$bcctObj, scale = 0, 
                                          best = options$maxModels), silent = TRUE)

    if (inherits(bcctSummary, "modprobs")) {
      # Good case
      bfObject$nModelsVisited <- bcctSummary$totmodsvisit
      modelnames <- bcctSummary$table$model_formula
      bfObject$modelNames <- substring(as.character(modelnames), first = 2)
      
      if (bfObject$nModelsVisited == 1)
        bfObject$postModelProbs <- bfObject$bf10s <- 1
      else if (bfObject$nModelsVisited > 1) {
        # Note the following BFs are based on a uniform prior on the models
        if (!is.null(bcctSummary$table$prob.Freq)) {
          postModelProbs <- bcctSummary$table$prob.Freq
          bfObject$postModelProbs <- postModelProbs
          bfObject$bf10s <-  postModelProbs/ max(postModelProbs)
        } else {
          # NAs: nModelsVisited, bf10s, postModelProbs
          stop("R Package error: Cannot retrieve table probabilities")
        }
      }
    }
  } 
  jaspResults[["Container"]][["bfObject"]] <- createJaspState(bfObject)
  jaspResults[["Container"]][["bfObject"]]$dependOn(c("fixedSamplesNumber", "sampleMode", 
                                                      "priorShape", "priorScale"))
  return(bfObject)
}

.basRegLogLinMainResults <- function(jaspResults, dataset, options) {
  # Compute/get the model
  bfObject <- .basRegLogLinComputeBFObject(jaspResults, dataset, options)

  #for empty elements in tables w/ output
  emptyRow    <- .basRegLogLinMainLine(char = "") 
  dotted.line <- .basRegLogLinMainLine(char = ".") #for empty tables
  
  posteriorTableRows <- list()
  results <- list()
  results[["footnotes"]] <- list()
  results[["data"]] <- list()
  
  nModelsReport <- try(min(bfObject$nModelsVisited, options$maxModels))
  if (!is.null(bfObject$modelNames))
    reportNames <- .unvf(bfObject$modelNames)
  else if (!is.null(bfObject$variables)) 
    reportNames <- bfObject$variables
  else 
    reportNames <- c("...", "... ")
  
  if (!is.null(bfObject$bf10s))
    reportBfs <- bfObject$bf10s
  else 
    reportBfs <- rep(NA, length = nModelsReport) 
  if (is.numeric(reportBfs)) {
    if (options$bayesFactorType == "BF01")
      reportBfs <- 1/reportBfs
    else if (options$bayesFactorType == "LogBF10")
      reportBfs <- log(reportBfs)
  }
  if (!is.null(bfObject$postModelProbs))
    reportPostModelProbs <- bfObject$postModelProbs
  else
    reportPostModelProbs <- rep(NA, length = nModelsReport)
  
  for (i in 1:nModelsReport){
    posteriorTableRows[[i]]           <- emptyRow
    posteriorTableRows[[i]]$"number"  <- as.integer(i)
    posteriorTableRows[[i]]$"model"   <- reportNames[i]
    posteriorTableRows[[i]]$"pMdata"  <- reportPostModelProbs[i]
    posteriorTableRows[[i]]$"bf"      <- reportBfs[i]
  }
  
  message <- paste("Total number of models visited =", bfObject$nModelsVisited, sep=" ")
  jaspResults[["Container"]][["MainTable"]]$addFootnote(message)
  jaspResults[["Container"]][["MainTable"]]$addRows(posteriorTableRows)
}

.basRegLogLinSummaryResults <- function(jaspResults, dataset, options) {
  # Compute/get the model
  bfObject <- .basRegLogLinComputeBFObject(jaspResults, dataset, options)

  #for empty elements in tables w/ output
  emptyRow    <- .basRegLogLinSummaryLine(char = "",  prob = TRUE) 
  dotted.line <- .basRegLogLinSummaryLine(char = ".", prob = TRUE) #for empty tables

  results <- list()
  
  lookup.table <- .regressionLogLinearBayesianBuildLookup(dataset, options$factors)
  lookup.table[["(Intercept)"]] <- "(Intercept)"
  
  if(inherits(bfObject$bcctObj, "bcct")) {
    probLevel <- options$regressionCoefficientsCredibleIntervalsInterval
    logBlm.summary   <- summary(bfObject$bcctObj, 
                                n.burnin = bfObject$nBurnIn, 
                                cutoff = options$posteriorProbabilityCutOff, 
                                prob.level = probLevel)
    logBlm.estimates <- logBlm.summary$int_stats
    
    len.Blogreg <- length(results) + 1		
    term.names  <- logBlm.estimates$term			
    
    if (length(bfObject$variables) > 0) {

      variablesInModel <- bfObject$variables
      terms <- as.character(logBlm.estimates$term)
      coef  <- base::strsplit (terms, split = ":", fixed = TRUE)				

      for (var in seq_along(coef)) {
        
        results[[ len.Blogreg ]] <- emptyRow
        terms <- coef[[var]]
        actualName <- list()

        for (j in seq_along(terms))
          actualName[[j]] <- paste(lookup.table[[ terms[j] ]], collapse = " = ")
        varName <- paste0(actualName, collapse = "*")
        
        results[[ len.Blogreg ]]$"Name"      <- varName
        post_prob <- as.numeric(logBlm.estimates$prob[var])
        results[[ len.Blogreg ]]$"post_prob" <- post_prob
        post_mean <- as.numeric(logBlm.estimates$post_mean[var])
        results[[ len.Blogreg ]]$"post_mean" <- post_mean
        post_var <- as.numeric(logBlm.estimates$post_var[var])
        results[[ len.Blogreg ]]$"post_var" <- post_var
        
        if (options$regressionCoefficientsCredibleIntervals == TRUE){			
          lower_lim <- as.numeric(logBlm.estimates$lower[var])
          results[[ len.Blogreg ]]$"lower_lim" <- lower_lim
          upper_lim <- as.numeric(logBlm.estimates$upper[var])
          results[[ len.Blogreg ]]$"upper_lim" <- upper_lim
        }
        
        len.Blogreg <- len.Blogreg + 1
      }		
    }			

  } else {
    
    len.Blogreg <- length(results) + 1
    results[[ len.Blogreg ]] <- dotted.line

    if (length(bfObject$variables) > 0) {
      
      variablesInModel <- bfObject$variables

      len.Blogreg <- len.Blogreg + 1
      
      for (var in 1:length(variablesInModel)) {
        
        results[[ len.Blogreg ]] <- dotted.line

        if (base::grepl(":", variablesInModel[var])) {
          
          # if interaction term					
          vars <- unlist(strsplit(variablesInModel[var], split = ":"))
          name <- paste0(vars, collapse = "\u2009\u273b\u2009")
          
        } else 
          name <- as.character(variablesInModel[ var])
        
        results[[ len.Blogreg ]]$"Name" <- name
        len.Blogreg <- len.Blogreg + 1
      }
    }
  }
  jaspResults[["Container"]][["SummaryTable"]]$addRows(results)
}

.basRegLogLinSubSummaryResults <- function(jaspResults, dataset, options) {
  if(!is.null(jaspResults[["Container"]][["SubSummaryResults"]]) || !options$regressionCoefficientsSubmodel) 
    return()
  # Get Model
  bfObject <- jaspResults[["Container"]][["bfObject"]]$object
  

  #for empty elements in tables w/ output
  emptyRow    <- .basRegLogLinSummaryLine(char = "") 
  dotted.line <- .basRegLogLinSummaryLine(char = ".") #for empty tables

  results <- list()
  results[["footnotes"]] <- list()
  results[["data"]] <- list()
  
  lookup.table <- .regressionLogLinearBayesianBuildLookup(dataset, options$factors)
  lookup.table[["(Intercept)"]] <- "(Intercept)"
  
  if (!is.null(bfObject$bcctObj)  ) {
    probLevel <- options$regressionCoefficientsSubmodelCredibleIntervalsInterval
    order     <- options$regressionCoefficientsSubmodelNo
    logBlm.subestimates = try(conting::sub_model(bfObject$bcctObj, 
                                                 n.burnin   = bfObject$nBurnIn, 
                                                 order      = order, 
                                                 prob.level = probLevel), 
                              silent = TRUE)
    
    if (inherits(logBlm.subestimates, "submod")){
      
      len.Blogreg <- length(results) + 1		
      term.names  <- logBlm.subestimates$term	
      
      extractedModelFormula <- logBlm.subestimates$formula
      
      extractedModelFormula <- as.character(extractedModelFormula)
      extractedModelFormula <- substring(extractedModelFormula, first = 2) # trim leading ~
      extractedModelFormula <- .unvf(extractedModelFormula)	
      jaspResults[["Container"]][["SubSummaryTable"]]$addFootnote(extractedModelFormula, symbol = "<em>Model formula:</em>")
      jaspResults[["Container"]][["SubSummaryTable"]]$addFootnote(paste(round(logBlm.subestimates$post_prob, 3)), symbol = "<em>Posterior model probability =</em>")

      if (length(bfObject$variables) > 0) {
        
        variablesInModel <- bfObject$variables
        terms <- as.character(logBlm.subestimates$term)
        coef  <- base::strsplit (terms, split = ":", fixed = TRUE)				
        
        for (var in seq_along(coef)) {
          
          results[["data"]][[ len.Blogreg ]] <- emptyRow
          terms      <- coef[[var]]
          actualName <- list()
          
          for (j in seq_along(terms))
            actualName[[j]] <- paste(lookup.table[[ terms[j] ]], collapse = " = ")
          varName <- paste0(actualName, collapse = "*")
          
          results[[ len.Blogreg ]]$"Name"      <- varName
          post_mean <- as.numeric(logBlm.subestimates$post_mean[var])
          results[[ len.Blogreg ]]$"post_mean" <- post_mean
          post_var <- as.numeric(logBlm.subestimates$post_var[var])
          results[[ len.Blogreg ]]$"post_var"  <- post_var
          
          if (options$regressionCoefficientsSubmodelCredibleIntervals){			
            lower_lim <- as.numeric(logBlm.subestimates$lower[var])
            results[[ len.Blogreg ]]$"lower_lim" <- lower_lim
            upper_lim <- as.numeric(logBlm.subestimates$upper[var])
            results[[ len.Blogreg ]]$"upper_lim" <- upper_lim
          }
          len.Blogreg <- len.Blogreg + 1
        }		
      }			
      
    } else {
      
      len.Blogreg <- length(results) + 1
      results[["data"]][[ len.Blogreg ]] <- dotted.line
      
      if (length(bfObject$variables) > 0) {
        
        variablesInModel <- bfObject$variables
        
        len.Blogreg <- len.Blogreg + 1
        
        for (var in 1:length(variablesInModel)) {
          
          results[["data"]][[ len.Blogreg ]] <- dotted.line
          
          if (base::grepl(":", variablesInModel[var])) {
            
            # if interaction term					
            vars <- unlist(strsplit(variablesInModel[var], split = ":"))
            name <- paste0(vars, collapse = "\u2009\u273b\u2009")
            
          } else 
            name <- as.character(variablesInModel[ var])
          
          results[["data"]][[ len.Blogreg ]]$"Name" <- name
          len.Blogreg <- len.Blogreg + 1
        }
      }
    }
    
  } else {		
    
    len.Blogreg <- length(results) + 1
    
    if (length(bfObject$variables) > 0) 
      variablesInModel <- bfObject$variables
    
    results[["data"]][[ len.Blogreg ]] <- dotted.line
    results[["data"]][[ len.Blogreg ]]$"Model" <- 1
  }
  jaspResults[["Container"]][["SubSummaryTable"]]$addRows(results)
}

# Container
.basRegLogLinContainer <- function(jaspResults, dataset, options) {
  if(is.null(jaspResults[["Container"]])) {
    jaspResults[["Container"]] <- createJaspContainer()
    jaspResults[["Container"]]$dependOn(c("counts", "modelTerms", 
                                          "priorShape", "priorScale"))
  }
}

# Tables
.basRegLogLinMainTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["Container"]][["MainTable"]])) 
    return()
  
  # Create table
  mainTable <- createJaspTable(title = "Model Comparison")
  mainTable$dependOn(c("bayesFactorType", "maxModels", "posteriorProbabilityCutOff"))
  .basRegLogLinCitation(mainTable)
  mainTable$showSpecifiedColumnsOnly <- TRUE
  mainTable$position <- 1
  
  if (options$bayesFactorType == "BF10") 
    bfTitle <- "BF<sub>10</sub>"
  else if (options$bayesFactorType == "BF01") 
    bfTitle <- "BF<sub>01</sub>"
  else 
    bfTitle <- "Log(BF<sub>10</sub>)"
  
  # Add columns to table
  mainTable$addColumnInfo(name = "number", title = " ",         type = "integer")
  mainTable$addColumnInfo(name = "model",  title = "Models",    type = "string")
  mainTable$addColumnInfo(name = "pMdata", title = "P(M|data)", type = "number", 
                          format = "dp:3")
  mainTable$addColumnInfo(name = "bf", title = bfTitle, type = "number")

  jaspResults[["Container"]][["MainTable"]] <- mainTable
  if (!ready) 
    return()
  res <- try(.basRegLogLinMainResults(jaspResults, dataset, options))
  .basRegLogLinSetError(res, mainTable)
}

.basRegLogLinSummaryTable <- function(jaspResults, dataset, options, ready){
  if (!is.null(jaspResults[["Container"]][["SummaryTable"]]) || 
      !options$regressionCoefficientsEstimates) 
    return()
  
  # Create table
  summaryTable <- createJaspTable(title = "Posterior Summary Statistics")
  summaryTable$dependOn(c("regressionCoefficientsEstimates",
                          "regressionCoefficientsCredibleIntervals",
                          "regressionCoefficientsCredibleIntervalsInterval"))
  .basRegLogLinCitation(summaryTable)
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  summaryTable$position <- 2
  
  # Add columns to table
  summaryTable$addColumnInfo(name = "Name", title = " ", type = "string")
  summaryTable$addColumnInfo(name = "post_prob", title = "P(incl|data)", 
                             type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "post_mean", title = "Mean",
                             type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "post_var",  title = "Variance",
                             type = "number", format = "dp:3")
  if(options$regressionCoefficientsCredibleIntervals){
    ci.label <- paste0(100*options$regressionCoefficientsCredibleIntervalsInterval, 
                       "% Credible intervals")
    summaryTable$addColumnInfo(name = "lower_lim", title = "Lower", 
                               type = "number", overtitle = ci.label)
    summaryTable$addColumnInfo(name = "upper_lim", title = "Upper", 
                               type = "number", overtitle = ci.label)
  }
  
  jaspResults[["Container"]][["SummaryTable"]] <- summaryTable
  
  if (!ready) 
    return()
  
  res <- try(.basRegLogLinSummaryResults(jaspResults, dataset, options))
  
  .basRegLogLinSetError(res, summaryTable)
}

.basRegLogLinSubSummaryTable <- function(jaspResults, dataset, options, ready){
  if (!is.null(jaspResults[["Container"]][["SubSummaryTable"]]) || 
      !options$regressionCoefficientsSubmodel) 
    return()
  
  # Create table
  title <- paste("Posterior Summary Statistics For Submodel", 
                 options$regressionCoefficientsSubmodelNo, sep=" ")
  subSummaryTable <- createJaspTable(title = title)
  subSummaryTable$dependOn(c("regressionCoefficientsSubmodel",
                             "regressionCoefficientsSubmodelCredibleIntervals",
                             "regressionCoefficientsSubmodelCredibleIntervalsInterval",
                             "regressionCoefficientsSubmodelNo"))
  .basRegLogLinCitation(subSummaryTable)
  subSummaryTable$showSpecifiedColumnsOnly <- TRUE
  subSummaryTable$position <- 3
  
  # Add columns to table
  subSummaryTable$addColumnInfo(name = "Name", title = " ", type = "string")
  subSummaryTable$addColumnInfo(name = "post_mean", title = "Mean",
                                type = "number", format = "dp:3")
  subSummaryTable$addColumnInfo(name = "post_var", title = "Variance",
                                type = "number", format = "dp:3")
  if(options$regressionCoefficientsSubmodelCredibleIntervals){
    ciVal <- options$regressionCoefficientsSubmodelCredibleIntervalsInterval
    ci.label <- paste0(100*ciVal, "% Credible intervals")
    subSummaryTable$addColumnInfo(name = "lower_lim", title = "Lower", type = "number", 
                                  overtitle = ci.label)
    subSummaryTable$addColumnInfo(name = "upper_lim", title = "Upper", type = "number", 
                                  overtitle = ci.label)
  }
  
  jaspResults[["Container"]][["SubSummaryTable"]] <- subSummaryTable
  if (!ready) 
    return()
  res <- try(.basRegLogLinSubSummaryResults(jaspResults, dataset, options))
  .basRegLogLinSetError(res, subSummaryTable)
}

# Other 
.basRegLogLinCitation <- function(table) {
  citation <- ("Overstall, A., & King, R. (2014). conting: an R package for 
                Bayesian analysis of complete and incomplete contingency tables. 
                Journal of Statistical Software, 58(7), 1-27.")
  table$addCitation(citation)
}

.basRegLogLinSetError <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}

.basRegLogLinSummaryLine <- function(char, prob = FALSE) {
  line <- list(
    "Name"      = char,
    "post_mean" = char,
    "post_var"  = char,
    "lower_lim" = char,
    "upper_lim" = char)
  if(prob)
    line$post_prob <- char
  return(line)
}

.basRegLogLinMainLine <- function(char) {
  line <- list(
    "number" = char,
    "model"  = char,
    "pMdata" = char,
    "bf"     = char)
  return(line)
}

.regressionLogLinearBayesianBuildLookup <- function(dataset, factors) {
  table <- list()
  
  for (factorName in factors) {
    levels <- base::levels(dataset[[ .v(factorName) ]])
    
    for (i in seq_along(levels)) {
      levelName <- levels[i]
      base64Name <- paste(.v(factorName), i, sep="")
      actualName <- c(factorName, levelName)
      table[[base64Name]] <- actualName
    }
  }
  return(table)
}
