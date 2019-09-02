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
AnovaRepeatedMeasures <- function(jaspResults, dataset = NULL, options) {
  
  numericVariables <- c(unlist(options$repeatedMeasuresCells),unlist(options$covariates))
  numericVariables <- numericVariables[numericVariables != ""]
  factorVariables <- c(unlist(options$betweenSubjectFactors))
  factorVariables <- factorVariables[factorVariables != ""]
  
  if (is.null(dataset)) 
    dataset <- .readDataSetToEnd(columns.as.numeric=numericVariables, columns.as.factor=factorVariables, 
                                 exclude.na.listwise=c(numericVariables, factorVariables))
  
  longData <- .BANOVAreadRManovaData(dataset, options)

  ready <- all(options$repeatedMeasuresCells != "") &&  length(options$withinModelTerms) > 0
  
  .BANOVAerrorhandling(longData, options, "RM-ANOVA")
  
  rmAnovaContainer <- .getRMAnovaContainer(jaspResults)

  .rmAnovaComputeResultsContainer(rmAnovaContainer, longData, options, ready)
  
  .rmAnovaWithinSubjectsTable(rmAnovaContainer, dataset, options, ready)  
  
  .rmAnovaBetweenSubjectsTable(rmAnovaContainer, dataset, options, ready)
  
  .referenceGrid(rmAnovaContainer, options, ready)
  
  .rmAnovaAssumptionsContainer(rmAnovaContainer, dataset, options, ready)
  
  .BANOVAdescriptives(rmAnovaContainer, longData, options, list(noVariables=FALSE), "RM-ANOVA")
  
  .rmAnovaPostHocTable(rmAnovaContainer, dataset, options, ready)

  .rmAnovaContrastTable(rmAnovaContainer, longData, options, ready)

  .rmAnovaMarginalMeansTable(rmAnovaContainer, dataset, options, ready)
  
  .rmAnovaFriedmanTable(rmAnovaContainer, longData, options, ready)
  
  .rmAnovaConoverTable(rmAnovaContainer, longData, options, ready)
  
  .rmAnovaSimpleEffects(rmAnovaContainer, dataset, longData, options, ready) 
  
  return()
}

.getRMAnovaContainer <- function(jaspResults) {
  
  if (!is.null(jaspResults[["rmAnovaContainer"]])) {
    
    anovaContainer <- jaspResults[["rmAnovaContainer"]]
    
  } else {
    
    anovaContainer <- createJaspContainer(title = "Repeated Measures ANOVA")
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    anovaContainer$dependOn(c("withinModelTerms", "betweenModelTerms", "repeatedMeasuresCells", "betweenSubjectFactors",
                              "repeatedMeasuresFactors", "covariates", "sumOfSquares"))
    jaspResults[["rmAnovaContainer"]] <- anovaContainer
  }
  
  return(anovaContainer)
  
}

.rmAnovaCheckErrors <- function(dataset, options, ready) {
  if (!ready) return()
  
  modelTerms <- unlist(options$withinModelTerms, recursive = FALSE)
  betweenModelTerms <- options$betweenModelTerms
  
  for(betweenTerm in rev(betweenModelTerms)) {
    .hasErrors(
      dataset = dataset, 
      type = c('observations', 'variance', 'infinity'),
      all.target = c(options$repeatedMeasuresCells, options$covariates), 
      all.grouping = betweenTerm,
      observations.amount = "< 2", 
      exitAnalysisIfErrors = TRUE)
  }
  
  for(betweenTerm in rev(betweenModelTerms)) {
    .hasErrors(
      dataset = dataset, 
      type = c('infinity', 'factorLevels'),
      all.target = betweenTerm, 
      factorLevels.amount  = "< 2",
      exitAnalysisIfErrors = TRUE)
  }
  
}

.rmAnovaCheck <- function(dataset, options, perform) {
  
  error <- FALSE
  errorMessage <- NULL
  ready <- "" %in% options$repeatedMeasuresCells == FALSE && length(options$withinModelTerms) > 0
  
  if (ready && perform == "run") {
    
    components <- unique(unlist(options$betweenSubjectFactors))
    independentsWithLessThanTwoLevels <- c()
    
    for (component in components) {
      
      nLevels <- length(levels(dataset[[ .v(component) ]]))
      
      if (nLevels < 2)
        independentsWithLessThanTwoLevels <- c(independentsWithLessThanTwoLevels, component)
    }
    
    if (length(independentsWithLessThanTwoLevels) > 0) {
      
      error <- TRUE
      if(length(independentsWithLessThanTwoLevels) == 1) {
        errorMessage <- paste("Factor: <em>", independentsWithLessThanTwoLevels, "</em>, contains fewer than two levels.", sep="")
      } else {
        errorMessage <- paste("Factors: <em>", paste(independentsWithLessThanTwoLevels, collapse=",", sep=""), "</em>, contain fewer than two levels.", sep="")
      }
    }
    
    repeatedMeasuresData <- list()
    for(i in options$repeatedMeasuresCells) {
      repeatedMeasuresData[[i]] <- dataset[[.v(i)]]
    }
    infiniteRM <- unlist(lapply(repeatedMeasuresData,function(x)sum(is.infinite(x)) > 0))
    
    if (!is.null(infiniteRM) && sum(infiniteRM) > 0) {
      
      error <- TRUE
      if(sum(infiniteRM) == 1) {
        errorMessage <- paste("The repeated measure: <em>", options$repeatedMeasuresCells[infiniteRM], "</em>, contains infinite values.", sep="")
      } else {
        errorMessage <- paste("The repeated measures: <em>", paste(options$repeatedMeasuresCells[infiniteRM], collapse=", "), "</em>, contain infinite values.", sep="")
      }
    }
    
    covariatesData <- list()
    for(i in options$covariates) {
      covariatesData[[i]] <- dataset[[.v(i)]]
    }
    infiniteCov <- unlist(lapply(covariatesData,function(x)sum(is.infinite(x)) > 0))
    
    if (!is.null(infiniteCov) && sum(infiniteCov) > 0) {
      
      error <- TRUE
      if(sum(infiniteCov) == 1) {
        errorMessage <- paste("The covariate: <em>", options$covariates[infiniteCov], "</em>, contains infinite values.", sep="")
      } else {
        errorMessage <- paste("The covariates: <em>", paste(options$covariates[infiniteCov], collapse=", "), "</em>, contain infinite values.", sep="")
      }
    }
    
    allNames <- unlist(lapply(options[['repeatedMeasuresFactors']], function(x) x$name)) # Factornames 
    for(factorName in allNames){
      if (any(factorName %in% options$betweenSubjectFactors )) {
        error <- TRUE
        errorMessage <- paste("Please choose a name for the RM factors that differs from those for the 
		                          between subjects factors.", sep="")
      }
    } 
    
  }
  
  list(ready=ready, error=error, errorMessage=errorMessage)
}

.rmModelFormula <- function(options) {
  
  termsRM.base64 <- c()
  termsRM.normal <- c()
  
  for (term in options$withinModelTerms) {
    
    components <- unlist(term$components)
    termRM.base64 <- paste(.v(components), collapse=":", sep="")
    termRM.normal <- paste(components, collapse=" \u273B ", sep="")
    
    termsRM.base64 <- c(termsRM.base64, termRM.base64)
    termsRM.normal <- c(termsRM.normal, termRM.normal)
  }
  
  termsBS.base64 <- c()
  termsBS.normal <- c()
  
  for (term in options$betweenModelTerms) {
    
    components <- unlist(term$components)
    termBS.base64 <- paste(.v(components), collapse=":", sep="")
    termBS.normal <- paste(components, collapse=" \u273B ", sep="")
    
    termsBS.base64 <- c(termsBS.base64, termBS.base64)
    termsBS.normal <- c(termsBS.normal, termBS.normal)
  }
  
  terms.base64 <- list()
  terms.normal <- list()
  terms.base64[[1]] <- termsBS.base64
  terms.normal[[1]] <- termsBS.normal
  
  for (i in 1:length(termsRM.base64)) {
    if (is.null(termsBS.base64)) {
      terms.base64[[i+1]] <- termsRM.base64[i]
      terms.normal[[i+1]] <- termsRM.normal[i]
    } else if (!is.null(termsRM.base64)){
      terms.base64[[i+1]] <- c(termsRM.base64[i], paste(termsRM.base64[i], termsBS.base64, sep = ":"))
      terms.normal[[i+1]] <- c(termsRM.normal[i], paste(termsRM.normal[i], termsBS.normal, sep = " \u273B "))
    }
  }
  
  main <- paste("(",paste(unlist(terms.base64), collapse=" + "),")", sep="")
  termsBS <- paste("(",paste(termsBS.base64, collapse=" + "),")", sep="")
  errorRM <- paste("Error(",paste("Xc3ViamVjdA/(", termsRM.base64, ")",sep="", collapse=" + "),")",sep="")
  
  if (is.null(termsBS.base64) && is.null(termsRM.base64)) {
    model.def <- XZGVwZW5kZW50 ~ 1
  } else if (is.null(termsBS.base64)) {
    model.def <- paste("XZGVwZW5kZW50", "~", paste(main, errorRM, sep=" + "))
  } else if (is.null(termsRM.base64)) {
    model.def <- paste("XZGVwZW5kZW50", "~", main)
  } else {
    model.def <- paste("XZGVwZW5kZW50", "~", paste(main, errorRM, termsBS, sep=" + "))
  }
  
  list(model.def = model.def, terms.normal = terms.normal, terms.base64 = terms.base64, termsRM.normal = termsRM.normal, termsRM.base64 = termsRM.base64)
}

.rmAnovaComputeResultsContainer <- function(rmAnovaContainer, longData, options, ready) {
  if (!ready) return()
  
  # Take results from state if possible
  if (!is.null(rmAnovaContainer[["anovaResult"]]))
    return()
  
  rmAnovaResult <- .rmAnovaComputeResults(longData, options)
  
  if (rmAnovaResult[["tryResult"]] == "try-error") {
    rmAnovaContainer$setError("Some parameters are not estimable, most likely due to empty cells of the design.")
    return()
  }
  
  # Save model to state
  rmAnovaContainer[["anovaResult"]] <- createJaspState(object = rmAnovaResult)
}

.rmAnovaComputeResults <- function(dataset, options, bootstrappingCall = FALSE) {
  
  modelDef <- .rmModelFormula(options)
  model.formula <- as.formula(modelDef$model.def)

  variables <- unlist(c(options$betweenSubjectFactors, lapply(options$repeatedMeasuresFactors, function(x) x$name)))

  for (variable in variables)
    dataset[[.v(variable)]] <- .v(dataset[[.v(variable)]])

  options(contrasts=c("contr.sum","contr.poly"))
  
  # set these options once for all afex::aov_car calls,
  # this ensures for instance that afex::aov_car always returns objects of class afex_aov.
  afex::afex_options(
    check_contrasts = TRUE, correction_aov = "GG", 
    emmeans_model = "univariate", es_aov = "ges", factorize = TRUE, 
    lmer_function = "lmerTest", method_mixed = "KR", return_aov = "afex_aov", 
    set_data_arg = FALSE, sig_symbols = c(" +", " *", " **", " ***"), type = 3
  )

  # Computations:
  if (options$sumOfSquares == "type1") {
    
    tryResult <- try({
      result <- stats::aov(model.formula, data=dataset)
      summaryResultOne <- summary(result, expand.split = FALSE)
    
      result <- afex::aov_car(model.formula, data=dataset, type= 3, factorize = FALSE)
      summaryResult <- summary(result)

      # Reformat the results to make it consistent with types 2 and 3
      model <- as.data.frame(unclass(summaryResult$univariate.tests))

      for (mySub in unlist(summaryResultOne, recursive = FALSE)) {
        for(term in trimws(rownames(mySub)[-nrow(mySub)])) {
          model[term, "Sum Sq"] <- mySub[term, "Sum Sq"]
          model[term, "num Df"] <- mySub[term, "Df"]
          model[term, "F value"] <- mySub[term, "F value"]
          model[term, "Pr(>F)"] <- mySub[term, "Pr(>F)"]
          model[term, "Error SS"] <-  mySub["Residuals", "Sum Sq"]
          model[term, "den Df"] <- mySub["Residuals", "Df"]
        }
      }
    })
    
  } else if (options$sumOfSquares == "type2") {
    
    tryResult <- try({
      result <- afex::aov_car(model.formula, data=dataset, type= 2, factorize = FALSE)
      summaryResult <- summary(result)
      model <- as.data.frame(unclass(summaryResult$univariate.tests))
    })
    
  } else {
    
    tryResult <- try({
      result <- afex::aov_car(model.formula, data=dataset, type= 3, factorize = FALSE)
      summaryResult <- summary(result)
      model <- as.data.frame(unclass(summaryResult$univariate.tests))
    })
    
  }

  if (class(tryResult) == "try-error") {
    return(list(tryResult = "try-error"))
  }

  if (bootstrappingCall)
    return(result)
  


  # Now we reformat the results table some more to make it flow with jaspResults later
  interceptRow <- model["(Intercept", ]
  model <- model[-1,]
  rownames(model) <- trimws(rownames(model))
  model[["isWithinTerm"]] <- model[[".isNewGroup"]] <- logical(nrow(model))

  sortedModel <- model
  cases <- unlist(sapply(modelDef$terms.base64, function(x) x[[1]]))
  residualResults <- sortedModel[cases, ]

  nextNewGroup <- 0
  counter <- 1
  for (modelTerm in modelDef$terms.base64) {

    if (!is.null(modelTerm)) {
      isWithin <- any(modelTerm %in% modelDef$termsRM.base64)
      indices <- .mapAnovaTermsToTerms(modelTerm, rownames(model))
      nextNewGroup <- c(TRUE, rep(FALSE, length(indices) - 1))
      sortedModel[indices, ] <- model[indices, ]
      sortedModel[indices, c(".isNewGroup", "isWithinTerm")] <- c(nextNewGroup, rep(isWithin, length(indices)))
  
      residualResults[modelTerm[[1]], ] <- c(model[indices[1],  c("Error SS", "den Df")], rep(NA, 4), 0, isWithin) 
    }

  }

  rownames(sortedModel) <- unlist(modelDef$terms.base64)
  sortedModel[["case"]] <- unlist(modelDef$terms.normal)
  sortedModel[["Mean Sq"]] <- sortedModel[["Sum Sq"]] / sortedModel[["num Df"]]
  sortedModel[["VovkSellkeMPR"]] <- .VovkSellkeMPR(sortedModel[["Pr(>F)"]])

  rownames(residualResults) <- cases
  residualResults[["Mean Sq"]] <- residualResults[["Sum Sq"]] / residualResults[["num Df"]]
  residualResults[["case"]] <- "Residuals"

  # Now we calculate effect sizes
  SSr <- sortedModel[["Error SS"]]
  MSr <- SSr/sortedModel[["den Df"]]
  
  sortedModel[["eta"]] <- sortedModel[["Sum Sq"]] / (sum(sortedModel[["Sum Sq"]]) + sum(residualResults[["Sum Sq"]]))
  sortedModel[["etaPart"]] <- sortedModel[["Sum Sq"]] / (sortedModel[["Sum Sq"]] + SSr)
  sortedModel[["genEta"]]<- result[["anova_table"]][["ges"]]

  n <- interceptRow[["den Df"]] + 1
  MSb <- interceptRow[["Error SS"]] / (n-1)
  MSm <- sortedModel[["Mean Sq"]]
  df <- sortedModel[["num Df"]]
  
  omega <- (df / (n * (df + 1)) * (MSm - MSr)) / (MSr + ((MSb - MSr) / (df + 1)) + 
                                                                     (df / (n * (df + 1))) * (MSm - MSr))
  sortedModel[["omega"]] <- sapply(omega, max, 0)
  for (i in .indices(summaryResult)) {
    if (any(rownames(summaryResult[[i]]) == "(Intercept)")) 
      summaryResult[[i]] <- summaryResult[[i]][-1, ]
  }

  # Now we include the results from the corrections
  withinAnovaTable <- ggTable <- hfTable <- subset(sortedModel, isWithinTerm == TRUE)
  withinAnovaTable[["correction"]] <- "None"
  corrections <- summaryResult$pval.adjustments
  sphericityTests <- as.data.frame(unclass(summaryResult$sphericity.tests))
  
  if (!is.null(rownames(corrections))) {
    corrections <- corrections[.mapAnovaTermsToTerms(rownames(withinAnovaTable), rownames(corrections)), ]
    sphericityTests <- sphericityTests[.mapAnovaTermsToTerms(rownames(withinAnovaTable), rownames(corrections)), ]
    rownames(corrections) <- rownames(sphericityTests) <- 
      rownames(withinAnovaTable)[.mapAnovaTermsToTerms(rownames(corrections), rownames(withinAnovaTable))]
  }
  
  # Add NA rows to corrections and sphericity tests for within factors with 2 levels
  if (nrow(sphericityTests) != nrow(withinAnovaTable)) {
    
    unavailableCases <- rownames(withinAnovaTable)[!rownames(withinAnovaTable) %in% rownames(sphericityTests)]
    emptyCorrections <- matrix(ncol = 4, nrow = length(unavailableCases), NA, 
                               dimnames = list(unavailableCases, c("GG eps", "Pr(>F[GG])", "HF eps", "Pr(>F[HF])")))
    if (is.null(rownames(corrections)) || all(is.na(corrections[, "GG eps"]))) {
      corrections <- as.data.frame(emptyCorrections)
    } else {
      corrections <- rbind(corrections, as.data.frame(emptyCorrections))
    }
    
    emptyTests <- matrix(ncol = 2, nrow = length(unavailableCases), NA, 
                         dimnames = list(unavailableCases, colnames(sphericityTests)))
    sphericityTests <- as.data.frame(rbind(sphericityTests, emptyTests))
    
  } else {
    corrections <- as.data.frame(corrections)
  }

  withinIndices <- .mapAnovaTermsToTerms(rownames(withinAnovaTable), rownames(corrections))

  ggTable[["num Df"]] <- withinAnovaTable[["num Df"]] * corrections[withinIndices, "GG eps"]
  ggTable[["Mean Sq"]] <-  withinAnovaTable[["Sum Sq"]] / ggTable[["num Df"]]
  ggTable[["den Df"]] <- withinAnovaTable[["den Df"]] * corrections[withinIndices, "GG eps"]
  ggTable[["Pr(>F)"]] <-  pf(withinAnovaTable[["F value"]], ggTable[["num Df"]], 
                             ggTable[["den Df"]], lower.tail = FALSE)
  ggTable[["correction"]] <-  "Greenhouse-Geisser"
  ggTable[[".isNewGroup"]] <- FALSE 

  hfTable[["num Df"]] <-  withinAnovaTable[["num Df"]] * corrections[withinIndices, "HF eps"]
  hfTable[["Mean Sq"]] <- withinAnovaTable[["Sum Sq"]] / hfTable[["num Df"]]
  hfTable[["den Df"]] <- withinAnovaTable[["den Df"]] * corrections[withinIndices, "HF eps"]
  hfTable[["Pr(>F)"]] <-  pf(withinAnovaTable[["F value"]], hfTable[["num Df"]], 
                             hfTable[["den Df"]], lower.tail = FALSE)
  hfTable[["correction"]] <-  "Huynh-Feldt"
  hfTable[[".isNewGroup"]] <- FALSE 

  residualResults[["eta"]] <- residualResults[["etaPart"]] <- residualResults[["genEta"]] <- 
    residualResults[["omega"]] <- residualResults[["p"]] <- as.numeric(NA)
  
  wResidualResults <- wResidualResultsGG <- wResidualResultsHF <- subset(residualResults, isWithinTerm == TRUE)
  wResidualResults[["correction"]] <- "None"
  wResidualResults[[".isNewGroup"]] <- TRUE
  
  residualIndices <- .mapAnovaTermsToTerms(rownames(wResidualResults), rownames(corrections))
  wResidualResultsGG[["num Df"]] <- wResidualResults[["num Df"]] * corrections[residualIndices, "GG eps"]
  wResidualResultsGG[["Mean Sq"]] <- wResidualResults[["Sum Sq"]] / wResidualResultsGG[["num Df"]]
  wResidualResultsGG[["correction"]] <-  "Greenhouse-Geisser"

  wResidualResultsHF[["num Df"]] <- wResidualResults[["num Df"]] * corrections[residualIndices, "HF eps"]
  wResidualResultsHF[["Mean Sq"]] <- wResidualResults[["Sum Sq"]] / wResidualResultsHF[["num Df"]]
  wResidualResultsHF[["correction"]] <-  "Huynh-Feldt"

  withinAnovaTable <- cbind(withinAnovaTable, corrections[withinIndices, ], sphericityTests[withinIndices, ])

  # Makes lists with results
  withinAnovaTableCollection <- list("None" = withinAnovaTable, "Huynh-Feldt" = hfTable, "Greenhouse-Geisser" = ggTable)
  wResidualResultsList <- list("None" = wResidualResults, "Huynh-Feldt" = wResidualResultsHF, 
                               "Greenhouse-Geisser" = wResidualResultsGG)
  # # Corrections not available
  # withinAnovaTableCollection <- list("None" = withinAnovaTable)
  # wResidualResultsList <- list("None" = wResidualResults)
    
 
  wResidualResultsList[["None"]]["BetweenResidualResults", c("Sum Sq", "num Df")] <- interceptRow[, c("Error SS", "den Df")]
  wResidualResultsList[["None"]]["BetweenResidualResults", "Mean Sq"] <- interceptRow[["Error SS"]] / interceptRow[["den Df"]]
  
  for (tableIndex in .indices(withinAnovaTableCollection)) {
    withinAnovaTableCollection[[tableIndex]][["VovkSellkeMPR"]] <- .VovkSellkeMPR(withinAnovaTableCollection[[tableIndex]][["Pr(>F)"]])
  }

  return(list(anovaResult = sortedModel, 
              residualTable = wResidualResultsList, 
              withinAnovaTable = withinAnovaTableCollection,
              assumptionResult = cbind(sphericityTests, corrections[withinIndices, ]), 
              fullModel = result, 
              tryResult = "tryResult"))
}

.mapAnovaTermsToTerms <- function(oneTerms, twoTerms) {
  nTerms <- length(oneTerms)
  indices <- numeric(nTerms)
  counter <- 1
  
  for (i in 1:nTerms) {
    splitFirst <- strsplit(oneTerms[[i]],":")[[1]]
    for (j in 1:length(twoTerms)) {
      matchedTerms <- match(splitFirst, strsplit(twoTerms[[j]],":")[[1]])
      if ((length(strsplit(twoTerms[[j]],":")[[1]]) == length(splitFirst)) && !any(is.na(matchedTerms))) {
        indices[counter] <- j
        counter <- counter + 1
      }
    }
  }
  
  return(indices)
}

.rmAnovaBetweenSubjectsTable <- function(rmAnovaContainer, dataset, options, ready) {
  if(!is.null(rmAnovaContainer[["betweenTable"]])) # || length(options$betweenSubjectFactors) == 0)
    return()
  
  betweenTable <- createJaspTable(title = "Between Subjects Effects")
  
  betweenTable$addColumnInfo(title = "Cases", name = "case", type = "string" )
  betweenTable$addColumnInfo(title = "Sum of Squares", name = "Sum Sq", type = "number")
  betweenTable$addColumnInfo(title = "df", name = "num Df", type = "integer")
  betweenTable$addColumnInfo(title = "Mean Square", name = "Mean Sq", type = "number")
  betweenTable$addColumnInfo(title = "F", name = "F value", type = "number")
  betweenTable$addColumnInfo(title = "p", name = "Pr(>F)", type = "number")
  
  if (options$VovkSellkeMPR) {
    betweenTable$addColumnInfo(title = "VS-MPR\u002A", name = "VovkSellkeMPR", type = "number")
    betweenTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
  
  if (options$effectSizeEstimates) {
    
    if (options$effectSizeEtaSquared) 
      betweenTable$addColumnInfo(title = "\u03B7\u00B2", name = "eta", type = "number")
    
    if (options$effectSizePartialEtaSquared) 
      betweenTable$addColumnInfo(title = "\u03B7\u00B2\u209A", name = "etaPart", type = "number")
    
    if (options$effectSizeGenEtaSquared) 
      betweenTable$addColumnInfo(title = "\u03B7\u00B2<sub>G</sub>", name = "genEta", type = "number")
    
    if (options$effectSizeOmegaSquared) 
      betweenTable$addColumnInfo(title = "\u03C9\u00B2", name = "omega", type = "number")
    
  }
  
  betweenTable$showSpecifiedColumnsOnly <- TRUE
  
  # set the type footnote already
  typeFootnote <- switch(options$sumOfSquares,
                         type1 = "Type I Sum of Squares",
                         type2 = "Type II Sum of Squares",
                         type3 = "Type III Sum of Squares")
  betweenTable$addFootnote(message = typeFootnote, symbol = "<em>Note.</em>")
  
  rmAnovaContainer[["betweenTable"]] <- betweenTable
  
  if (!ready) {
    return()
  }

  result <- rmAnovaContainer[["anovaResult"]]$object$anovaResult
  result <- result[result$isWithinTerm == FALSE, ]
  betweenwResidualResult <- rmAnovaContainer[["anovaResult"]]$object$residualTable$None["BetweenResidualResults", ]
  
  result["Residuals", "num Df"] <- betweenwResidualResult[["num Df"]]
  result["Residuals", "Sum Sq"] <- betweenwResidualResult[["Sum Sq"]]
  result["Residuals", "Mean Sq"] <- betweenwResidualResult[["Mean Sq"]]
  result["Residuals", "case"] <- "Residuals"
  result["Residuals", ".isNewGroup"] <- TRUE
  
  betweenTable$setData(result)
  
  return()
}

.rmAnovaWithinSubjectsTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["withinAnovaTable"]]))
    return()
  
  anovaTable <- createJaspTable(title = "Within Subjects Effects", position = 1)
  rmAnovaContainer[["withinAnovaTable"]] <- anovaTable
  anovaTable$showSpecifiedColumnsOnly <- TRUE
  anovaTable$dependOn(c("sphericityCorrections", "sphericityGreenhouseGeisser", "sphericityHuynhFeldt",
                        "sphericityNone", "VovkSellkeMPR", "effectSizeEstimates", "effectSizeEtaSquared",
                        "effectSizePartialEtaSquared", "effectSizeGenEtaSquared", "effectSizeOmegaSquared"))

  if (options$sphericityCorrections) {
    corrections <- c("None", "Greenhouse-Geisser", "Huynh-Feldt")[c(options$sphericityNone, 
                                                                   options$sphericityGreenhouseGeisser,
                                                                   options$sphericityHuynhFeldt)]
  } else 
    corrections <- "None"
  
  anovaTable$addColumnInfo(title = "Cases", name = "case", type = "string", combine = TRUE)
  
  dfType <- "integer" # Make df an integer unless corrections are applied
  if ((length(corrections) > 1 || any(!"None" %in% corrections)) && options$sphericityCorrections) {
    anovaTable$addColumnInfo(title = "Sphericity Correction", name = "correction", type = "string", combine = TRUE)
    dfType <- "number"
  }
  
  anovaTable$addColumnInfo(title = "Sum of Squares", name = "Sum Sq", type = "number")
  anovaTable$addColumnInfo(title = "df", name = "num Df", type = dfType)
  anovaTable$addColumnInfo(title = "Mean Square", name = "Mean Sq", type = "number")
  anovaTable$addColumnInfo(title = "F", name = "F value", type = "number")
  anovaTable$addColumnInfo(name = "p", type = "number")
  
  if (options$VovkSellkeMPR) {
    anovaTable$addColumnInfo(title = "VS-MPR\u002A", name = "VovkSellkeMPR", type = "number")
    anovaTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
  
  if (options$effectSizeEstimates) {
    
    if (options$effectSizeEtaSquared) {
      anovaTable$addColumnInfo(title = "\u03B7\u00B2", name = "eta", type = "number")
    }
    
    if (options$effectSizePartialEtaSquared) {
      anovaTable$addColumnInfo(title = "\u03B7\u00B2\u209A", name = "etaPart", type = "number")
    }
    
    if(options$effectSizeGenEtaSquared) {
      anovaTable$addColumnInfo(name="genEta", type="number", title="\u03B7\u00B2<sub>G</sub>")
    }
    
    if (options$effectSizeOmegaSquared) {
      anovaTable$addColumnInfo(title = "\u03C9\u00B2", name = "omega", type = "number")
    }
    
  }
  
  typeFootnote <- switch(options$sumOfSquares,
                         type1 = "Type I Sum of Squares",
                         type2 = "Type II Sum of Squares",
                         type3 = "Type III Sum of Squares")
  anovaTable$addFootnote(message = typeFootnote, symbol = "<em>Note.</em>")
  
  if (!ready)
    return()
  
  withinResults <- rmAnovaContainer[["anovaResult"]]$object$withinAnovaTable
  residualResults <- rmAnovaContainer[["anovaResult"]]$object$residualTable
  mauchlyResult <- rmAnovaContainer[["anovaResult"]]$object$assumptionResult
  
  for(i in .indices(withinResults)) {
    names(withinResults[[i]])[names(withinResults[[i]]) == "Pr(>F)"] <- "p"
  }
  
  modelTerms <- .rmModelFormula(options)$termsRM.base64
  allCases <- rownames(withinResults[[1]])
  addResidualAfter <- allCases[.mapAnovaTermsToTerms(modelTerms, allCases) + (length(allCases) / length(modelTerms)) - 1]
  
  for (case in allCases) {

    for (i in .indices(corrections)) {
    
      withinResults[[corrections[i]]][case, ".isNewGroup"] <- i == 1
      anovaTable$addRows(as.list(withinResults[[corrections[i]]][case, ]),
                         rowNames=paste0(case, corrections[i]))
    }

    if (case %in% modelTerms) {
      currentCase <- case
    }  
    if (case %in% addResidualAfter) {
      
      for (i in .indices(corrections)) {

        residualResults[[corrections[i]]][currentCase, ".isNewGroup"] <- i == 1  
        anovaTable$addRows(as.list(residualResults[[corrections[i]]][currentCase, ]), 
                           rowNames=paste0(currentCase, "Resid", corrections[i]))
        
      }
    }
  }

  if (!all(is.na(withinResults[[1]][["Test statistic"]]))) {
    violatedMauchlyCases <- rownames(mauchlyResult)[mauchlyResult[, "p-value"] < 0.05]
    anovaTable$addFootnote(message = "Mauchly's test of sphericity indicates that the assumption of sphericity is violated (p < .05).",
                           colNames = c("Sum Sq", "num Df", "F value", "Mean Sq", "Pr(F)", "p"),
                           rowNames = paste0(violatedMauchlyCases, "None"))
  }
  
  return()
}

.rmAnovaAssumptionsContainer <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["assumptionsContainer"]]))
    return()
  
  assumptionsContainer <- createJaspContainer(title = "Assumption Checks",
                                              dependencies = c("homogeneityTests", "sphericityTests"))
  
  rmAnovaContainer[["assumptionsContainer"]] <- assumptionsContainer

  if (options$homogeneityTests == TRUE)  
    .rmAnovaLevenesTable(rmAnovaContainer, dataset, options, ready)
  
  if (options$sphericityTests == TRUE) 
    .rmAnovaSphericityTable(rmAnovaContainer, dataset, options, ready)
  
  return()
}

.rmAnovaLevenesTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["rmAnovaLevenesTable"]]) || options$homogeneityTests == FALSE)
    return()

  rmAnovaLevenesTable <- createJaspTable("Test for Equality of Variances (Levene's)")
  rmAnovaContainer[["assumptionsContainer"]][["rmAnovaLevenesTable"]] <- rmAnovaLevenesTable
  
  rmAnovaLevenesTable$addColumnInfo(name="case", type="string", title="")
  rmAnovaLevenesTable$addColumnInfo(name="F", type="number")
  rmAnovaLevenesTable$addColumnInfo(name="df1", type="number")
  rmAnovaLevenesTable$addColumnInfo(name="df2", type="number")
  rmAnovaLevenesTable$addColumnInfo(name="p", type="number")
  
  if (options$VovkSellkeMPR) {
    rmAnovaLevenesTable$addColumnInfo(title = "VS-MPR\u002A", name = "VovkSellkeMPR", type = "number")
    rmAnovaLevenesTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
  rmAnovaLevenesTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready)
    return()
  
  rmAnovaLevenesTable$setExpectedSize(length(options$repeatedMeasuresCells))
  if (length(options$betweenModelTerms) == 0) {
    rmAnovaLevenesTable$setError("Cannot perform homogeneity tests because there are no between subjects factors specified.")
    return()
  }
  
  for (i in .indices(options$repeatedMeasuresCells)) {
    
    interaction <- paste(.v(options$betweenSubjectFactors), collapse=":", sep="")
    if (length(options$covariates) > 0 ) {
      
      covterms <- paste(.v(options$covariates), collapse="+", sep="")
      combterms <- paste(c(interaction,covterms), collapse="+", sep="")
      levene.def <- paste(.v(options$repeatedMeasuresCells[i]), "~", combterms)
      
    } else {
      
      levene.def <- paste(.v(options$repeatedMeasuresCells[i]), "~", interaction)
      
    }
    
    levene.formula <- as.formula(levene.def)
    
    dummyAov <- aov(levene.formula, data = dataset, qr = T)
    resids <- abs(dummyAov$residuals)
    levene.def <- paste("resids", "~", interaction)
    levene.formula <- as.formula(levene.def)
    
    r <- summary(aov(levene.formula, dataset))
    error <- base::tryCatch(summary(aov(levene.formula, dataset)),error=function(e) e, warning=function(w) w)
    
    row <- data.frame(case = options$repeatedMeasuresCells[i],
                      F = r[[1]]$`F value`[1], 
                      df1 = r[[1]]$Df[1],
                      df2 = r[[1]]$Df[2],
                      p=r[[1]]$`Pr(>F)`[1],
                      ".isNewGroup" = i == 1,
                      VovkSellkeMPR = .VovkSellkeMPR(r[[1]]$`Pr(>F)`[1]))
    
    rmAnovaLevenesTable$addRows(row)
    
  }
  
  return()
}

.rmAnovaSphericityTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["sphericityTable"]]) || !options$sphericityTests)
    return()

  sphericityTable <- createJaspTable("Test of Sphericity")
  
  sphericityTable$addColumnInfo(name="case", type="string", title="")
  sphericityTable$addColumnInfo(name="Test statistic", type="number", title="Mauchly's W")
  sphericityTable$addColumnInfo(name="approxChi", type="number", title="Approx. \u03A7\u00B2")
  sphericityTable$addColumnInfo(name="dfSphericity", type="integer")
  sphericityTable$addColumnInfo(name="p-value", type="number")
  sphericityTable$addColumnInfo(name="GG eps", type="number", title="Greenhouse-Geisser \u03B5")
  sphericityTable$addColumnInfo(name="HF eps", type="number", title="Huynh-Feldt \u03B5")
  sphericityTable$addColumnInfo(name="LB", type="number", title="Lower Bound \u03B5")

  sphericityTable$showSpecifiedColumnsOnly <- TRUE
  
  rmAnovaContainer[["assumptionsContainer"]][["sphericityTable"]] <- sphericityTable
  
  if(!ready)
    return()

  .approxChi <- function(df, n, W){
    d <- 1 - (2*df^2 + df + 2) / (6*df*(n-1))
    -(n-1)*d*log(W)
  }
  
  assumptionResult <- rmAnovaContainer[["anovaResult"]]$object$assumptionResult
  anovaResult <- rmAnovaContainer[["anovaResult"]]$object$withinAnovaTable$None
  
  # if (nrow(assumptionResult) == 0 || all(is.na(assumptionResult[["GG eps"]]))) {
  if (all(is.na(anovaResult[["Test statistic"]]))) {
    sphericityTable$setError("Cannot perform sphericity tests because there only two levels of the RM factor.")
    return()  
  }
  
  df <- anovaResult[["num Df"]]
  anovaResult[["dfSphericity"]] <- df * (df + 1) / 2 - 1
  n  <- anovaResult[["den Df"]] / df + 1
  
  anovaResult[["approxChi"]] <- .approxChi(df, n, anovaResult[["Test statistic"]])
  anovaResult[["LB"]] <- 1 / df
  anovaResult[["HF eps"]] <- sapply(anovaResult[["HF eps"]], min, 1)
  anovaResult[[".isNewGroup"]] <- FALSE

  includeInTable <- rownames(anovaResult) %in% sapply(options$withinModelTerms, function(x) paste(.v(unlist(x)), collapse=":"))
  sphericityTable$setData(anovaResult[includeInTable & (!is.na(anovaResult[["approxChi"]])), ])
  
  return()
}

.referenceGrid <- function(rmAnovaContainer, options, ready) {
  if (!is.null(rmAnovaContainer[["referenceGrid"]]) || !ready)
    return()
  
  fullModel <- rmAnovaContainer[["anovaResult"]]$object$fullModel 

  referenceGridList <- list()
  variables <- unlist(c(lapply(options$betweenModelTerms, 
                               function(x) {
                                 if (length(x$components) == 1) {
                                   return (.v(x$components))
                                 } else {
                                   return(NULL)
                                 }
                               }), lapply(options$withinModelTerms,
                                          function(x) {
                                            if (length(x$components) == 1) {
                                              return (.v(x$components))
                                            } else {
                                              return(NULL)
                                            }
                                          })
  ))
  
  postHocVariables <- unlist(options$postHocTestsVariables, recursive = FALSE)
  variablesPost <- unname(sapply(postHocVariables, function(x) paste(.v(x), collapse = ":")))
  
  variables <- union(variables, variablesPost)
  
  for (var in variables) {
    formula <- as.formula(paste("~", var))
    referenceGrid <- emmeans::emmeans(fullModel, formula)
    
    referenceGridList[[var]] <- referenceGrid
    
  }
  
  rmAnovaContainer[["referenceGrid"]] <- createJaspState(object = referenceGridList, 
                                                         dependencies = c("withinModelTerms",
                                                                          "betweenModelterms",
                                                                          "postHocTestsVariables"))
  
  return()
}

.rmAnovaPostHocTable <- function(rmAnovaContainer, dataset, options, ready) {
  if(!is.null(rmAnovaContainer[["postHocStandardContainer"]]) || length(options$postHocTestsVariables) ==0)
    return()
  
  postHocContainer <- createJaspContainer(title = "Post Hoc Tests")
  postHocContainer$dependOn(c("postHocTestsVariables", "postHocTestEffectSize", "postHocTestsBonferroni", 
                              "postHocTestsHolm", "postHocTestsScheffe", "postHocTestsTukey",
                              "postHocFlagSignificant", "confidenceIntervalsPostHoc", 
                              "confidenceIntervalIntervalPostHoc", "postHocTestPooledError"))
  
  rmAnovaContainer[["postHocStandardContainer"]] <- postHocContainer
  
  postHocVariables <- unlist(options$postHocTestsVariables, recursive = FALSE)
  postHocVariablesListV <- unname(lapply(postHocVariables, .v))
  variables <- unname(sapply(postHocVariables, function(x) paste(.v(x), collapse = ":")))
  
  options$postHocTestsSidak <- FALSE
  for (postHocVarIndex in 1:length(postHocVariables)) {
    
    thisVarName <- paste(postHocVariables[[postHocVarIndex]], collapse = " \u273B ")
    interactionTerm <- length(postHocVariables[[postHocVarIndex]]) > 1
    
    postHocContainer[[variables[postHocVarIndex]]] <- .createPostHocStandardTable(thisVarName, interactionTerm, options)
  }
  
  if (!ready)
    return()
  
  referenceGrid <- rmAnovaContainer[["referenceGrid"]]$object
  fullModel <- rmAnovaContainer[["anovaResult"]]$object$fullModel
  

  postHocData <- fullModel$data$wide
  factorNamesV <- colnames(postHocData) # Names to use to refer to variables in data
  # Because there are multiple names for each variable in JASP, one of the things the following code does is make sure to get the correct naming
  # and refer to the correct actual variable. The different names are the actual name of the variable, the name the user gives in jasp for the lvel and factor, 
  # and also the name that JASP gives to it, which is a concatenation of "Level#_Level#', where the first refers to the factor and second to the level. 
 
  rmFactorIndex <- 1
  allNames <- unlist(lapply(options$repeatedMeasuresFactors, function(x) x$name)) # Factornames 

  for (var in variables) {
    
    # Results using the Bonferroni method
    resultPostHoc <- summary(pairs(referenceGrid[[var]], adjust="bonferroni"), 
                          infer = TRUE, level = options$confidenceIntervalIntervalPostHoc)
    
    resultPostHoc[["bonferroni"]] <- resultPostHoc[["p.value"]] 
    
    resultPostHoc[["tukey"]] <-  summary(pairs(referenceGrid[[var]], adjust="tukey"))[["p.value"]]
    
    resultPostHoc[["scheffe"]] <-  summary(pairs(referenceGrid[[var]], adjust="scheffe"))[["p.value"]]
    
    resultPostHoc[["holm"]] <-  summary(pairs(referenceGrid[[var]], adjust="holm"))[["p.value"]]
    
    comparisons <- strsplit(as.character(resultPostHoc$contrast), " - ")
    
    orderOfTerms <- unlist(lapply(options$repeatedMeasuresFactors, function(x) x$name))
    indexofOrderFactors <- match(allNames,orderOfTerms)

    if (any(var == .v(allNames))) {     ## If the variable is a repeated measures factor

      if (!options$postHocTestPooledError) {

        levelsOfThisFactor <- unlist(lapply(options$repeatedMeasuresFactors[rmFactorIndex], function(x) x$levels)) # Levels within Factor
        numberOfLevels <- length(unique(levelsOfThisFactor))
        splitNames <- unlist(lapply(strsplit(factorNamesV,  split = "_"), function(x) x[indexofOrderFactors[rmFactorIndex]]))
        listVarNamesToLevel <- list()  # create a list of vectors of variable names, used to group the dataset for the post-hoc t-tests
        
        for(i in 1:numberOfLevels){
          
          listVarNamesToLevel[[i]] <- factorNamesV[(splitNames %in% .v(levelsOfThisFactor[i]))]  
        
        }
        
        countr <- 1
        for (k in 1:numberOfLevels) {  ### Loop over all the levels within factor and do pairwise t.tests on them
          
          for (i in .seqx(k+1, numberOfLevels)) {
            
            bonfAdjustCIlevel <- 1-((1-options$confidenceIntervalIntervalPostHoc)/choose(numberOfLevels, 2))
            tResult <- t.test(rowMeans(postHocData[listVarNamesToLevel[[k]]]),rowMeans(postHocData[listVarNamesToLevel[[i]]]),
                              paired= TRUE, var.equal = FALSE, conf.level = bonfAdjustCIlevel)
            resultPostHoc[["estimate"]][countr] <- tResult$estimate
            resultPostHoc[["t.ratio"]][countr] <- tResult$statistic
            resultPostHoc[["SE"]][countr] <- tResult$estimate / tResult$statistic
            resultPostHoc[["p.value"]][countr] <- tResult$p.value
            resultPostHoc[["lower.CL"]][countr] <- tResult$conf.int[1]
            resultPostHoc[["upper.CL"]][countr] <- tResult$conf.int[2]
            countr <- countr + 1
            
          }
          
        }
        resultPostHoc[["bonferroni"]] <- p.adjust(resultPostHoc[["p.value"]], method = "bonferroni")
        resultPostHoc[["holm"]] <- p.adjust(resultPostHoc[["p.value"]], method = "holm")
      }
      resultPostHoc[["scheffe"]] <- "."
      resultPostHoc[["tukey"]] <-  "."
      
      if (options$postHocTestsScheffe || options$postHocTestsTukey) {
        cors <- paste(c("Tukey", "Scheffe")[c(options$postHocTestsTukey, options$postHocTestsScheffe)], collapse = " and ")
        
        postHocContainer[[var]]$addFootnote(paste(cors, "corrected p-values are not appropriate for repeated", 
                                                   "measures post-hoc tests (Maxwell, 1980; Field, 2012)."))
      }
      rmFactorIndex <- rmFactorIndex + 1
    }

    
    resultPostHoc[['cohenD']] <- resultPostHoc[['t.ratio']] / sqrt(nrow(dataset))

    resultPostHoc[["contrast_A"]] <- lapply(comparisons, function(x) paste(.unv(strsplit(x[[1]], ",")[[1]]), 
                                                                           collapse = ", "))
    resultPostHoc[["contrast_B"]] <- lapply(comparisons, function(x) paste(.unv(strsplit(x[[2]], ",")[[1]]), 
                                                                           collapse = ", "))

    pValMessage <- attr(resultPostHoc, "mesg")[grep(attr(resultPostHoc, "mesg"), pattern = "P value adjustment")]
    if (length(pValMessage) != 0)
      postHocContainer[[var]]$addFootnote(
        message = gsub(x = pValMessage, ": bonferroni method", "Confidence interval"),
        symbol = "<i>Note.</i>")
    
    confMessage <- attr(resultPostHoc, "mesg")[grep(attr(resultPostHoc, "mesg"), pattern = "Conf-level")]
    if (length(confMessage) != 0 && options[["confidenceIntervalsPostHoc"]])
      postHocContainer[[var]]$addFootnote(
        message = gsub(x = confMessage, "Conf-level", "Confidence interval"),
        symbol = "<i>Note.</i>")

    avFootnote <- attr(resultPostHoc, "mesg")[grep(attr(resultPostHoc, "mesg"), pattern = "Results are averaged")]
    if (length(avFootnote) != 0) {
      avTerms <- .unv(strsplit(gsub(avFootnote, pattern = "Results are averaged over the levels of: ", replacement = ""), 
                                 ", ")[[1]])
      postHocContainer[[var]]$addFootnote(
        message = paste0("Results are averaged over the levels of: ", paste(avTerms, collapse = ", ")),
        symbol = "<i>Note.</i>")
    }

    for (pCorrection in c("bonferroni", "scheffe", "tukey", "holm")) {
      if (options$postHocFlagSignificant) {
        for (i in 3:1) {
          signifComparisons <- rownames(resultPostHoc)[resultPostHoc[[pCorrection]] < c(0.05, 0.01, 0.001)[i]]
          if (length(signifComparisons) > 0) {
            colNames <- rep(pCorrection, length(signifComparisons))
            postHocContainer[[var]]$addFootnote(message = "p < .05, ** p < .01, *** p < .001", 
                                                colNames = colNames, rowNames = signifComparisons,
                                                symbol = paste0(rep("*", i), collapse = ""))
          }
        }
      }
    }
    
    resultPostHoc[[".isNewGroup"]] <- !duplicated(resultPostHoc[["contrast_A"]])
    postHocContainer[[var]]$setData(resultPostHoc)
  }
  
  return()
}

.createPostHocStandardTable <- function(myTitle, interactionTerm, options, makeBootstrapTable = FALSE) {
  
  preTitle <- if (!makeBootstrapTable) "Post Hoc Comparisons - " else "Bootstrapped Post Hoc Comparisons - "
  postHocTable <- createJaspTable(title = paste0(preTitle, myTitle))
  
  # postHocTable$addColumnInfo(name="contrast", title="Comparison", type="string")
  postHocTable$addColumnInfo(name="contrast_A", title=" ", type="string", combine = TRUE)
  postHocTable$addColumnInfo(name="contrast_B", title=" ", type="string")
  
  postHocTable$addColumnInfo(name="estimate", title="Mean Difference", type="number")
  
  if (options$confidenceIntervalsPostHoc || makeBootstrapTable) {
    
    if (makeBootstrapTable) {
      thisOverTitle <- paste0(options$confidenceIntervalIntervalPostHoc * 100, "% bca\u002A CI")
    } else {
      thisOverTitle <- paste0(options$confidenceIntervalIntervalPostHoc * 100, "% CI for Mean Difference")
    }
    
    postHocTable$addColumnInfo(name="lower.CL", type = "number", title = "Lower",
                               overtitle = thisOverTitle)
    postHocTable$addColumnInfo(name="upper.CL", type = "number", title = "Upper",
                               overtitle = thisOverTitle)
  } 
  
  postHocTable$addColumnInfo(name="SE", type="number")
  
  if (makeBootstrapTable) {
    
    postHocTable$addColumnInfo(name="bias", type="number")
    
  } 
  
  postHocTable$addColumnInfo(name="t.ratio", title="t", type="number")
  
  if (options$postHocTestEffectSize ) {
    postHocTable$addColumnInfo(name="cohenD", title="Cohen's d", type="number")
    postHocTable$addFootnote("Cohen's d does not correct for multiple comparisons.")
  }
  
  if (options$postHocTestsTukey)
    postHocTable$addColumnInfo(name="tukey", title="p<sub>tukey</sub>", type="number")
  
  if (options$postHocTestsScheffe)
    postHocTable$addColumnInfo(name="scheffe", title="p<sub>scheffe</sub>", type="number")
  
  if (options$postHocTestsBonferroni)
    postHocTable$addColumnInfo(name="bonferroni", title="p<sub>bonf</sub>", type="number")
  
  if (options$postHocTestsHolm)
    postHocTable$addColumnInfo(name="holm",title="p<sub>holm</sub>", type="number")
  
  if(options$postHocTestsSidak)
    postHocTable$addColumnInfo(name="sidak",title="p<sub>sidak</sub>", type="number")
  
  
  postHocTable$showSpecifiedColumnsOnly <- TRUE
  
  return(postHocTable)
}

.resultsContrasts <- function(rmAnovaContainer, dataset, options, ready) {
  if(!ready)
    return()
  
  referenceGrid <- rmAnovaContainer[["referenceGrid"]]$object

  resultsContrasts <- list()
  datasetLong <- .shortToLong(dataset, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, options$betweenSubjectFactors)
  
  contrastTypes <- c("none", "deviation", "simple", "Helmert", "repeated", "difference", "polynomial")
  
  for (contrast in options$contrasts) {
    
    if (! .v(contrast$variable) %in% names(referenceGrid)) {
      next
    }
    
    resultsContrasts[[contrast$variable]] <- list()
    
    column <- datasetLong[[.v(contrast$variable)]]
    
    for(contrastType in contrastTypes) {
      
      contrastMatrix <- .rmAnovaCreateContrast(column, contrastType)
      contrCoef <- lapply(as.data.frame(contrastMatrix), as.vector)
      names(contrCoef) <- .v(.anovaContrastCases(column, contrastType))
      
      if (contrastType == "none") {
        r <- NULL
      } else {
        r <- emmeans::contrast(referenceGrid[[.v(contrast$variable)]], contrCoef)
      }
      resultsContrasts[[contrast$variable]][[contrastType]] <- summary(r)
    }
  }
  
  return(resultsContrasts)
}

.rmAnovaCreateContrast <- function (column, contrast.type) {
  
  levels <- levels(column)
  n.levels <- length(levels)
  
  contr <- NULL
  
  if (contrast.type == "none") {
    
    options(contrasts = c("contr.sum","contr.poly"))
    contr <- NULL
    
  } else if (contrast.type == "deviation") {
    
    contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
    for (i in 2:n.levels) {
      contr[,(i-1)] <-  -1 / n.levels
      contr[i,(i-1)] <- (n.levels - 1) / n.levels
    }
    
  } else if (contrast.type == "simple") {
    
    contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
    for (i in 1:n.levels-1) {
      contr[c(1,i+1),i]<- c(1,-1) * -1
    }
    
  } else if (contrast.type == "Helmert") {
    
    contr <- contr.helmert(levels) 
    contr <- apply(contr, 2, function(x){ x/max(abs(x))})
    contr <- matrix(rev(contr), ncol = ncol(contr), nrow = nrow(contr))
    
  } else if (contrast.type == "repeated") {
    
    contr <- matrix(0,nrow = n.levels, ncol = n.levels - 1)
    
    for (i in 1:(n.levels-1)) {
      contr[i,i] <- 1
      contr[i+1,i] <- -1
    }
    
  } else if (contrast.type == "difference") {
    
    contr <- contr.helmert(levels) 
    contr <- apply(contr, 2, function(x){ x/max(abs(x))})
    
  } else if (contrast.type == "polynomial") {
    
    contr <- contr.poly(levels)
  }
  
  if (! is.null(contr)) {
    dimnames(contr) <- list(NULL, 1:dim(contr)[2])
  }
  
  contr
}

.rmAnovaContrastTable <- function(rmAnovaContainer, longData, options, ready) {
  if (!is.null(rmAnovaContainer[["contrastContainer"]]) || all(grepl("none", options$contrasts)))
    return()
  
  contrastContainer <- createJaspContainer(title = "Contrast Tables")
  contrastContainer$dependOn(c("contrasts", "contrastAssumeEqualVariance", "confidenceIntervalIntervalContrast", 
                               "confidenceIntervalsContrast"))
  
  createContrastTable <- function(myTitle, options) {
    
    contrastTable <- createJaspTable(title = myTitle)
    contrastTable$addColumnInfo(name = "Comparison", type = "string")
    contrastTable$addColumnInfo(name = "estimate", "Estimate", type = "number")
    
    if (options$confidenceIntervalsContrast) {
      
      thisOverTitle <- paste0(options$confidenceIntervalIntervalContrast * 100, "% CI for Mean Difference")
      contrastTable$addColumnInfo(name="lower.CL", type = "number", title = "Lower",
                                  overtitle = thisOverTitle, )
      contrastTable$addColumnInfo(name="upper.CL", type = "number", title = "Upper",
                                  overtitle = thisOverTitle)
      
    } 
    
    contrastTable$addColumnInfo(name = "SE", type = "number")
    
    dfType <- if (options$contrastAssumeEqualVariance) "integer" else "number"
    contrastTable$addColumnInfo(name = "df", type = dfType)
    contrastTable$addColumnInfo(name = "t.ratio", title = "t", type = "number")
    contrastTable$addColumnInfo(name = "p.value", title = "p", type = "number")
    
    contrastTable$showSpecifiedColumnsOnly <- TRUE
    
    return(contrastTable)
  }
  
  
  for (contrast in options$contrasts) {
    
    if (contrast$contrast != "none") {
      
      contrastType <- unlist(strsplit(contrast$contrast, ""))
      contrastType[1] <- toupper(contrastType[1])
      contrastType <- paste0(contrastType, collapse = "")
      
      myTitle <- paste0(contrastType, " Contrast", " - ",  contrast$variable)
      contrastContainer[[paste0(contrast$contrast, "Contrast_",  contrast$variable)]] <- createContrastTable(myTitle, options)
    }
    
  }
  
  rmAnovaContainer[["contrastContainer"]] <- contrastContainer
  
  if (!ready) 
    return()  
  
  referenceGrid <- rmAnovaContainer[["referenceGrid"]]$object
  
  for (contrast in options$contrasts) {
    
    if (contrast$contrast != "none") {
      column <- longData[[.v(contrast$variable)]]
      contrastMatrix <- .rmAnovaCreateContrast(column, contrast$contrast)
      contrCoef <- lapply(as.data.frame(contrastMatrix), as.vector)
      names(contrCoef) <- .v(.anovaContrastCases(column, contrast$contrast))
      
      contrastResult <- emmeans::contrast(referenceGrid[[.v(contrast$variable)]], contrCoef)


    # if (!is.null(contrastResult@misc$avgd.over)) 
      contrastContainer[[paste0(contrast$contrast, "Contrast_",  contrast$variable)]]$addFootnote(
        message = paste0("Results are averaged over the levels of: ", paste(.unv(contrastResult@misc$avgd.over), collapse = ", ")),
        symbol = "<i>Note.</i>")
      
      contrastResult <- cbind(contrastResult, confint(contrastResult, 
                                                      level = options$confidenceIntervalIntervalContrast)[,5:6])
      contrastResult[["Comparison"]] <- .unv(contrastResult[["contrast"]])
      
      # New feature - verify To do!!!!
      if (options$contrastAssumeEqualVariance == FALSE) {
        newDF <- do.call(data.frame, tapply(longData[["XZGVwZW5kZW50"]], longData[[.v(contrast$variable)]], cbind))
        ssNr <- tapply(longData[["Xc3ViamVjdA"]], longData[[.v(contrast$variable)]], cbind)
        
        for (i in 1:ncol(newDF)) {
          newDF[[i]] <- tapply(newDF[[i]], ssNr[[i]], mean)
        }
        newDF <- newDF[1:length(unique(ssNr[[1]])), ]
        
        allTestResults <- list()

        for (coefIndex in 1:length(contrCoef)) {
          allTestResults[[coefIndex]] <- t.test(as.matrix(newDF) %*% contrCoef[[coefIndex]])
        }
        
        contrastResult[["t.ratio"]] <- sapply(allTestResults, function(x) x[["statistic"]])
        contrastResult[["df"]] <- sapply(allTestResults, function(x) x[["parameter"]])
        contrastResult[["SE"]] <- sapply(allTestResults, function(x) x[["estimate"]] /  x[["statistic"]])
        contrastResult[["p.value"]] <- sapply(allTestResults, function(x) x[["p.value"]])
      }
      
      contrastContainer[[paste0(contrast$contrast, "Contrast_",  contrast$variable)]]$setData(contrastResult)
    }
  }
 
}

.rmAnovaMarginalMeansTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["marginalMeansContainer"]]) || length(options$marginalMeansTerms) == 0)
    return ()
  
  marginalMeansContainer <- createJaspContainer(title = "Marginal Means")
  marginalMeansContainer$dependOn(c("marginalMeansTerms",  "marginalMeansCompareMainEffects", "marginalMeansCIAdjustment",
                                    "marginalMeansBootstrapping", "marginalMeansBootstrappingReplicates"))
  
  rmAnovaContainer[["marginalMeansContainer"]] <- marginalMeansContainer
  
  createMarginalMeansTable <- function(myTitle, options, individualTerms, makeBootstrapTable = FALSE, dfType = "integer" ) {
    
    preTitle <- if (!makeBootstrapTable) "Marginal Means - " else "Bootstrapped Marginal Means - "
    marginalMeansTable <- createJaspTable(title = paste0(preTitle, myTitle))
    
    for (i in 1:length(individualTerms))
      marginalMeansTable$addColumnInfo(name=individualTerms[i], type="string", combine = TRUE)
    
    marginalMeansTable$addColumnInfo(name="lsmean", title="Marginal Mean", type="number")
    
    if (makeBootstrapTable) {
      thisOverTitle <- paste0("95% bca\u002A CI")
    } else {
      thisOverTitle <- paste0("95% CI for Mean Difference")
    }
    
    marginalMeansTable$addColumnInfo(name="lower.CL", type = "number", title = "Lower",
                                     overtitle = thisOverTitle, )
    marginalMeansTable$addColumnInfo(name="upper.CL", type = "number", title = "Upper",
                                     overtitle = thisOverTitle)
    
    marginalMeansTable$addColumnInfo(name="SE", type="number")
    
    if (makeBootstrapTable) {
      
      marginalMeansTable$addColumnInfo(name="bias", type="number")
      
      marginalMeansTable$addFootnote(message = paste0("Marginal Means estimate is based on the median of", 
                                                        " the bootstrap distribution."))
      marginalMeansTable$addFootnote(symbol = "\u002A", message = "Bias corrected accelerated.") 
      
    }
    
    if (options$marginalMeansCompareMainEffects) {
      marginalMeansTable$addColumnInfo(name="t.ratio", title="t", type="number")
      marginalMeansTable$addColumnInfo(name="df", type = dfType)
      marginalMeansTable$addColumnInfo(name="p.value", title="p", type="number")
      
      if (options$marginalMeansCIAdjustment == "bonferroni") {
        marginalMeansTable$addFootnote(message = "Bonferroni CI adjustment")
      } else if (options$marginalMeansCIAdjustment == "sidak") {
        marginalMeansTable$addFootnote(message = "Sidak CI adjustment")
      }
    }
    
    marginalMeansTable$showSpecifiedColumnsOnly <- TRUE
    
    return(marginalMeansTable)
  }

  
  marginalTerms <- unlist(options$marginalMeansTerms, recursive = FALSE)


  for (term in marginalTerms) {
    thisVarName <- paste(term, collapse = " \u273B ")
    individualTerms <- term
    if (any(term %in% unlist(options$withinModelTerms))) dfType <- "number" else dfType <- "integer"
    marginalMeansContainer[[paste0(.v(term), collapse = ":")]] <- createMarginalMeansTable(thisVarName, options, 
                                                                                           individualTerms, 
                                                                                           options$marginalMeansBootstrapping,
                                                                                           dfType = dfType)
  }
  
  if (!ready)
    return()
  
  fullModel <- rmAnovaContainer[["anovaResult"]]$object$fullModel
  
  for (term in marginalTerms) {

    termBase64 <- paste0(.v(term), collapse = ":")
    
    formula <- as.formula(paste("~", termBase64))
      
    if(options$marginalMeansCIAdjustment == "bonferroni") {
      adjMethod <- "bonferroni"
    } else if(options$marginalMeansCIAdjustment == "sidak") {
      adjMethod <- "sidak"
    } else {
      adjMethod <- "none"
    }
    
    marginalResult <- summary(emmeans::lsmeans(fullModel, formula), adjust = adjMethod, infer = c(TRUE,TRUE))

    marginalResult[[".isNewGroup"]] <- FALSE
    marginalResult[[".isNewGroup"]][which(marginalResult[, 1] == marginalResult[1, 1])] <- TRUE

    names(marginalResult)[1:length(term)] <- term
    
    for (var in term) 
      marginalResult[[var]] <- .unv(marginalResult[[var]])
    
    
    ### Bootstrapping 
    if (options$marginalMeansBootstrapping) {
      
      startProgressbar(options[["marginalMeansBootstrappingReplicates"]])
      
      nRows <- nrow(marginalResult)
      
      bootstrapMarginalMeans <- try(boot::boot(data = dataset, statistic = .bootstrapMarginalMeansRmAnova, 
                                               R = options[["marginalMeansBootstrappingReplicates"]],
                                               options = options,
                                               nRows = nRows,
                                               termLength = length(term),
                                               formula = formula), silent = TRUE)

      bootstrapSummary <- summary(bootstrapMarginalMeans)
      
      ci.fails <- FALSE
      bootstrapMarginalMeansCI <- t(sapply(1:nrow(bootstrapSummary), function(index){
        res <- try(boot::boot.ci(boot.out = bootstrapMarginalMeans, conf = 0.95, type = "bca",
                                 index = index)[['bca']][1,4:5])
        if(!inherits(res, "try-error")){
          return(res)
        } else if(identical(attr(res, "condition")$message, "estimated adjustment 'a' is NA")){
          ci.fails <<- TRUE
          return(c(NA, NA))
        } else{
          return(res)
        }
      }))
      
      if (ci.fails) {
        marginalMeansContainer[[termBase64]]$addFootnote(message = paste0("Some confidence intervals could not be computed.", 
                                                                          "Possibly too few bootstrap replicates."))
      }

      marginalResult[["lower.CL"]] <- bootstrapMarginalMeansCI[,1]
      marginalResult[["upper.CL"]] <- bootstrapMarginalMeansCI[,2]

      marginalResult[["lsmean"]] <- bootstrapSummary[["bootMed"]]
      marginalResult[["bias"]] <- bootstrapSummary[["bootBias"]]
      marginalResult[["SE"]] <- bootstrapSummary[["bootSE"]]
      
      marginalMeansContainer[[termBase64]]$addFootnote(message = paste0("Bootstrapping based on ", 
                                                                        bootstrapSummary$R[1], " replicates."))
    }

    marginalMeansContainer[[termBase64]]$setData(as.data.frame(marginalResult))
    
  }
  
  return()
}

.bootstrapMarginalMeansRmAnova <- function(data, indices, options, nRows, termLength, formula){
  
  indices <- sample(indices, replace = TRUE)
  resamples <- data[indices, , drop=FALSE]
  
  dataset <- .shortToLong(resamples, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, 
                          c(options$betweenSubjectFactors, options$covariates))        
  idx <- match(c("dependent", "subject"), colnames(dataset))
  colnames(dataset)[idx] <- .v(colnames(dataset)[idx])

  anovaModelBoots <- .rmAnovaComputeResults(dataset, options, bootstrappingCall = TRUE) # refit model

  if (!is.null(anovaModelBoots[["tryResult"]]))
    return(rep(NA, termLength))
  
  resultBoots <- summary(emmeans::lsmeans(anovaModelBoots, formula), infer = c(FALSE,FALSE))
  progressbarTick()
  
  if(length(resultBoots$lsmean) == nRows){ # ensure that the bootstrap has all levels
    return(resultBoots$lsmean)
  } else {
    return(rep(NA, termLength))
  }
}

.rmAnovaFriedmanTable <- function(rmAnovaContainer, longData, options, ready) {
  if (!is.null(rmAnovaContainer[["nonparametricContainer"]]) || length(options$friedmanWithinFactor) == 0 || !ready)
    return ()
  
  rmAnovaContainer[["nonparametricContainer"]] <- createJaspContainer("Nonparametrics")
  rmAnovaContainer[["nonparametricContainer"]]$dependOn(c("friedmanWithinFactor",
                                                           "friedmanBetweenFactor"))

  friedmanTable <- createJaspTable(title = "Friedman Test")
  friedmanTable$addColumnInfo(name="Factor", type="string")
  friedmanTable$addColumnInfo(name="chiSqr", title="Chi-Squared", type="number")
  friedmanTable$addColumnInfo(name="df", type="integer")
  friedmanTable$addColumnInfo(name="p", type="number")
  friedmanTable$addColumnInfo(name="kendall", title="Kendall's W", type="number")

  rmAnovaContainer[["nonparametricContainer"]][["friedmanTable"]] <- friedmanTable

  if (!ready)
    return()
  
  withinTerms <- options$friedmanWithinFactor
  betweenTerm <- options$friedmanBetweenFactor
  
  withinTerms.base64 <- .v(withinTerms)
  betweenTerms.base64 <- .v(betweenTerm)
  
  result <- list()
  
  if( any(!(withinTerms %in% unlist(options$withinModelTerms))) || 
      (betweenTerm %in% unlist(options$withinModelTerms)) ) {
    friedmanTable$setError("Please specify appropriate terms for the Friedman/Durbin test.")
    return()
  }
  
  if (identical(betweenTerm, "")) {
    betweenTerms.base64 <- "Xc3ViamVjdA"
  }
  
  rows <- list()
  
  for (i in 1:length(withinTerms)) {
    
    groups <- as.factor(longData[, withinTerms.base64[i]])
    blocks <- as.factor(longData[, betweenTerms.base64])
    y <- longData[, "XZGVwZW5kZW50"]
    
    useDurbin <- any(table(groups, blocks) != 1)
    
    t <- nlevels(groups)
    b <- nlevels(blocks)
    r <- unique(table(groups))
    k <- unique(table(blocks))
    
    if (length(r) == 1 & length(k) == 1) {
      rankPerBlock <- unlist(tapply(y, blocks, rank))
      rankPerGroup <- unlist(tapply(y, groups, rank))    
      
      rankJ <- tapply(rankPerBlock, groups, sum)    
      
      sumRanks <- sum(rankPerBlock^2)
      cVal <- (b * k * (k + 1)^2) / 4
      dVal <- sum(rankJ^2) - r * cVal
      testStatOne <- (t - 1) / (sumRanks - cVal) * dVal
      testStatTwo <- (testStatOne / (t - 1)) / ((b * k - b - testStatOne) / (b * k - b - t + 1))
      
      ## Code from PMCMRplus
      dfChi <- t - 1
      dfOneF <- k - 1
      dfTwoF <- b * k - b - t + 1 
      pValOne <- pchisq(testStatOne, dfChi, lower.tail = FALSE)
      pValTwo <- pf(testStatTwo, dfOneF, dfTwoF, lower.tail = FALSE)
      
      # Kendall W
      rankMatrixRM <- matrix(rankPerGroup, ncol = t)
      rowSumsMatrix <- rowSums(rankMatrixRM)
      nTies <- unlist(apply(rankMatrixRM, 2, function(x) {
        tab <- table(x)
        tab[tab > 1] }))
      nTies <- sum(nTies^3 - nTies)
      kendallW <- (sum(rowSumsMatrix^2) - sum(rowSumsMatrix)^2 / b) / (t^2 * (b^3 - b) / 12)
      kendallWcor <-(sum(rowSumsMatrix^2) - sum(rowSumsMatrix)^2 / b) / ((t^2 * (b^3 - b) - t * nTies) / 12)
      
      row <- list()
      
      row[["Factor"]] <- withinTerms[i]
      row[["chiSqr"]] <- testStatOne
      row[["df"]] <- dfChi
      row[["p"]] <- pValOne
      row[["kendall"]] <- kendallWcor

      if (useDurbin) {
        friedmanTable$title <- "Durbin Test"
        
        row[["F"]] <- testStatTwo
        row[["dfnum"]] <- dfOneF
        row[["dfden"]] <- dfTwoF
        row[["pF"]] <- pValTwo 
        
        if (i == 1) {
          friedmanTable$addColumnInfo(name="F", type="number")
          friedmanTable$addColumnInfo(name="dfnum", title="df num", type="integer")
          friedmanTable$addColumnInfo(name="dfden", title="df den", type="integer")
          friedmanTable$addColumnInfo(name="pF", title="p<sub>F</sub>",type="number")
        }
        
      } 
      
      rows[[i]] <- row
      
    } else {
      
      friedmanTable$setError("Specified ANOVA design is not balanced.")
      return()
      
    }
    
  }

  friedmanTable$setData(as.data.frame(do.call(rbind,rows)))
  
  return()
}

.rmAnovaConoverTable <- function(rmAnovaContainer, longData, options, ready) {
  if (!is.null(rmAnovaContainer[["nonparametricContainer"]][["conoverContainer"]]) || options$conoverTest == FALSE)
    return ()

  conoverContainer <- createJaspContainer("Conover Test")
  rmAnovaContainer[["nonparametricContainer"]][["conoverContainer"]] <- conoverContainer
  conoverContainer$dependOn(c("conoverTest"))
  
  createConoverTable <- function(myTitle) {
    
    conoverTable <- createJaspTable(title = paste0("Conover's Post Hoc Comparisons - ", myTitle))
    
    conoverTable$addColumnInfo(name="(I)",title="", type="string", combine=TRUE)
    conoverTable$addColumnInfo(name="(J)",title="", type="string")
    conoverTable$addColumnInfo(name="t",  title="T-Stat", type="number")
    conoverTable$addColumnInfo(name="df", type="integer")
    conoverTable$addColumnInfo(name="wA", title="W<sub>i</sub>", type="number")
    conoverTable$addColumnInfo(name="wB", title="W<sub>j</sub>", type="number")
    conoverTable$addColumnInfo(name="pval", title="p", type="number")
    conoverTable$addColumnInfo(name="bonferroni", title="p<sub>bonf</sub>", type="number")
    conoverTable$addColumnInfo(name="holm",title="p<sub>holm</sub>", type="number")
    
    return(conoverTable)
  }

  
  if (!ready)
    return()
  
  
  groupingVariables <- unlist(options$friedmanWithinFactor)
  blockingVar <- ifelse( identical(options$friedmanBetweenFactor, ""), "Xc3ViamVjdA", .v(options$friedmanBetweenFactor))
  y <- longData[, "XZGVwZW5kZW50"]

  for (groupingVar in groupingVariables) {
    
    conoverTable <- createConoverTable(groupingVar)
    conoverTable$addFootnote(paste0("Grouped by ", .unv(blockingVar),"."))
    
    rows <- list()
    
    groups <- as.factor(longData[, .v(groupingVar)])
    blocks <- as.factor(longData[, blockingVar])
    
    groupNames <- levels(groups)
    ## Code from PMCMRplus
    t <- nlevels(groups)
    b <- nlevels(blocks)
    r <- unique(table(groups))
    k <- unique(table(blocks)) 
    rij <- unlist(tapply(y, blocks, rank))
    Rj <- unname(tapply(rij, groups, sum))
    
    df <- b * k - b - t + 1
    
    S2 <- 1 / ( 1 * t -1 ) * (sum(rij^2) - t * b * ( t + 1)^2 / 4)
    T2 <- 1 / S2 * (sum(Rj) - b * ((t + 1) / 2)^2)
    A <- S2 * (2 * b * (t - 1)) / ( b * t - t - b + 1)
    B <- 1 - T2 / (b * (t- 1))
    denom <- sqrt(A) * sqrt(B)

    for (i in 1:t) {
      
      for (j in .seqx(i+1, t)) {
        
        row <- list("(I)"=groupNames[[i]], "(J)"=groupNames[[j]])
        
        diff <-  abs(Rj[i] - Rj[j]) 
        tval <- diff / denom
        pval <- 2 * pt(q = abs(tval), df = df, lower.tail=FALSE)
        
        row[["t"]] <- tval
        row[["wA"]]  <- Rj[i]
        row[["wB"]] <- Rj[j]
        row[["pval"]] <- pval
        row[["bonferroni"]] <- pval
        row[["holm"]] <- pval
        row[["df"]] <- df
        
        rows[[length(rows)+1]] <- row
        
      }
      
      if (length(rows) == 0)  {
        row[[".isNewGroup"]] <- TRUE
      } else {
        row[[".isNewGroup"]] <- FALSE
      }
    }
    
      
    allP <- unlist(lapply(rows, function(x) x$p))
    allBonf <- p.adjust(allP, method = "bonferroni")
    allHolm <- p.adjust(allP, method = "holm")
    
    for (p in 1:length(rows)) {
      rows[[p]][['bonferroni']] <- allBonf[p]
      rows[[p]][['holm']] <- allHolm[p]
    }
      
    conoverTable$setData(as.data.frame(do.call(rbind, rows)))
    rmAnovaContainer[["nonparametricContainer"]][[groupingVar]] <- conoverTable
  }
  
  return()
}

.rmAnovaSimpleEffects <- function(rmAnovaContainer, dataset, longData, options, ready) {
  if (!is.null(rmAnovaContainer[["simpleEffectsContainer"]]) || identical(options$simpleFactor, "") || 
      identical(options$moderatorFactorOne, ""))
    return()
  
  rmAnovaContainer[["simpleEffectsContainer"]] <- createJaspContainer(title = "Simple Main Effects",
                                                                      dependencies = c("simpleFactor", 
                                                                                       "moderatorFactorOne", 
                                                                                       "moderatorFactorTwo",
                                                                                       "poolErrorTermSimpleEffects"))
  
  simpleEffectsTable <- createJaspTable(title = paste0("Simple Main Effects - ", options$simpleFactor))
  rmAnovaContainer[["simpleEffectsContainer"]][["simpleEffectsTable"]] <- simpleEffectsTable
  
  moderatorTerms <- c(options$moderatorFactorOne, options$moderatorFactorTwo[!identical(options$moderatorFactorTwo, "")])
  nMods <- length(moderatorTerms)
  simpleFactor <- options[['simpleFactor']]
  simpleFactorBase64 <- .v(simpleFactor)
  
  simpleEffectsTable[["title"]] <- paste("Simple Main Effects - ", options$simpleFactor, sep = "")
  
  simpleEffectsTable$addColumnInfo(name = "modOne", title = paste0("Level of ", moderatorTerms[1]), 
                                   type = "string", combine = TRUE)
  
  if (nMods == 2)
    simpleEffectsTable$addColumnInfo(name = "modTwo", title = paste0("Level of ", moderatorTerms[2]), 
                                     type = "string", combine = TRUE)
  
  
  simpleEffectsTable$addColumnInfo(name = "SumSq", type = "number", title = "Sum of Squares")
  simpleEffectsTable$addColumnInfo(name = "Df", type = "integer", title = "df")
  simpleEffectsTable$addColumnInfo(name = "MeanSq", type = "number", title = "Mean Square")
  simpleEffectsTable$addColumnInfo(name = "F", type = "number")
  simpleEffectsTable$addColumnInfo(name = "p", type = "number")
  
  simpleEffectsTable$showSpecifiedColumnsOnly <- TRUE
  
  typeFootnote <- switch(options$sumOfSquares,
                         type1 = "Type I Sum of Squares",
                         type2 = "Type II Sum of Squares",
                         type3 = "Type III Sum of Squares")
  simpleEffectsTable$addFootnote(message = typeFootnote, symbol = "<em>Note.</em>")
  
  simpleEffectsTable$addCitation("Howell, D. C. (2002). Statistical Methods for Psychology (8th. ed.). Pacific Grove, CA: Duxberry. ")
  
  if (!ready)
    return()
  
  lvls <- list()
  factors <- list()

  for (variable in moderatorTerms) {
    
    factor <- longData[[ .v(variable) ]]
    factors[[length(factors)+1]] <- factor
    lvls[[variable]] <- levels(factor)
    
  }
  
  simpleEffectResult <- rev(expand.grid(rev(lvls), stringsAsFactors = FALSE))
  colnames(simpleEffectResult) <- c("modOne", "modTwo")[1:nMods]
  
  simpleEffectResult[[".isNewGroup"]] <- FALSE
  
  emptyCaseIndices <- emptyCases <- NULL
  allSimpleModels <- list()
  
  fullResidualTable <- rmAnovaContainer[["anovaResult"]][["object"]][["residualTable"]][["None"]]
 
  isMixedAnova <-   length(options[['betweenModelTerms']]) > 0
  isSimpleFactorWithin <- !simpleFactor %in% unlist(options[['betweenModelTerms']] )
  isModeratorOneWithin <- !moderatorTerms[1] %in% unlist(options[['betweenModelTerms']] )
  isModeratorTwoWithin <- !moderatorTerms[2] %in% unlist(options[['betweenModelTerms']] )

  
  if (isMixedAnova & !isSimpleFactorWithin) {
    
    fullAnovaMS <- fullResidualTable["BetweenResidualResults", "Mean Sq"]
    fullAnovaDf <- fullResidualTable["BetweenResidualResults", "num Df"]
    
  } else {
    
    fullAnovaMS <- fullResidualTable[simpleFactorBase64, "Mean Sq"]
    fullAnovaDf <- fullResidualTable[simpleFactorBase64, "num Df"]
    
  }
    
  # Remove moderator factors from model terms
  simpleOptions <- options
  simpleOptions$betweenModelTerms <-  options$betweenModelTerms[!(grepl(moderatorTerms[1], options$betweenModelTerms) | 
                                                      grepl(moderatorTerms[nMods], options$betweenModelTerms))]
  simpleOptions$withinModelTerms <-  options$withinModelTerms[!(grepl(moderatorTerms[1], options$withinModelTerms) | 
                                                            grepl(moderatorTerms[nMods], options$withinModelTerms))]
  
  performBetweenAnova <- length(simpleOptions$withinModelTerms) == 0

  if (performBetweenAnova) {
    
    simpleOptions[["fixedFactors"]]  <- simpleOptions[['betweenSubjectFactors']]
    simpleOptions[["modelTerms"]] <- simpleOptions[['betweenModelTerms']]
    simpleOptions[["dependent"]] <-  "dependent"
    simpleOptions[["homogeneityBrown"]] <- simpleOptions[["homogeneityWelch"]] <- FALSE
    simpleOptions[["homogeneityNone"]] <- TRUE
    
  }
  
  emptyCaseIndices <- emptyCases <- NULL
  
  for (i in 1:nrow(simpleEffectResult)) {

    subsetStatement  <- eval(parse(text=paste("longData$", .v(moderatorTerms), " == \"", 
                                              simpleEffectResult[i, 1:nMods], 
                                              "\"", sep = "", collapse = " & ")))
    simpleDataset <- base::subset(longData, subsetStatement)
    
    if (simpleEffectResult[i, nMods] == lvls[[ nMods ]][1])
      simpleEffectResult[[i, ".isNewGroup"]] <- TRUE
    
    if (nrow(simpleDataset) < 2 || 
        nrow(unique(simpleDataset[simpleFactorBase64])) <  nrow(unique(longData[simpleFactorBase64]))) {
      
      emptyCaseIndices <- c(emptyCaseIndices, i)
      emptyCases <- c(emptyCases, paste(simpleEffectResult[i, 1:nMods], collapse = ", "))
      allSimpleModels[[i]] <- NA
      
    } else if (performBetweenAnova) {
      
      .anovaModelContainer(rmAnovaContainer[["simpleEffectsContainer"]], simpleDataset, simpleOptions, TRUE)
      .anovaResult(rmAnovaContainer[["simpleEffectsContainer"]], options = simpleOptions)
      anovaResult <- rmAnovaContainer[["simpleEffectsContainer"]][["anovaResult"]]$object$result
      
      rmAnovaContainer[["simpleEffectsContainer"]][["model"]] <- NULL
      
      if (!options$poolErrorTermSimpleEffects) {
        fullAnovaMS <-  anovaResult["Residuals", "Mean Sq"]
        fullAnovaDf <-  anovaResult["Residuals", "Df"]
      }
      
      anovaResult <- anovaResult[simpleFactorBase64, ]
      df <- anovaResult[["Df"]]
      
    } else {
      
      anovaResult <-  .rmAnovaComputeResults(simpleDataset, simpleOptions)$anovaResult[simpleFactorBase64, ]

      if (!options$poolErrorTermSimpleEffects) {
        fullAnovaMS <-  anovaResult[["Error SS"]] / anovaResult[["den Df"]]
        fullAnovaDf <-  anovaResult[["den Df"]]
      }
      
      df <- anovaResult[["num Df"]]
      
    }
    
    MS <- anovaResult[["Mean Sq"]]
    F <- MS / fullAnovaMS
    p <- pf(F, df, fullAnovaDf, lower.tail = FALSE)
    simpleEffectResult[i, c("SumSq", "MeanSq", "Df", "F", "p")] <- c(anovaResult[["Sum Sq"]], MS, df, F, p)
    
  }
  
  if (!is.null(emptyCaseIndices)) {
    simpleEffectsTable$addFootnote(paste0("Not enough observations in cells ", 
                                          paste0(" (", emptyCases, ")", collapse = ","), "."))
  }

  simpleEffectsTable$setData(simpleEffectResult)
  
  return()
}

.summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE, 
                       errorBarType="confidenceInterval", usePooledSE=FALSE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) {
      sum(!is.na(x))
    } else {
      length(x)
    }
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  # First aggregate over unused RM factors, if desired:
  if (usePooledSE & measurevar == "dependent") {
    data <- plyr::ddply(data, c("subject", groupvars), plyr::summarise, dependent = mean(dependent))
    names(data)[which(names(data) == "dependent")] <- measurevar
  } else if (usePooledSE & measurevar == "dependent_norm") {
    data <- plyr::ddply(data, c("subject", groupvars), plyr::summarise, dependent = mean(dependent_norm))
    names(data)[which(names(data) == "dependent")] <- measurevar
  }
  
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )
  
  # Rename the "mean" column
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  if (errorBarType == "confidenceInterval") {
    
    datac$ciLower <- datac[,measurevar] - datac[,"ci"]
    datac$ciUpper <- datac[,measurevar] + datac[,"ci"]
    
  } else {
    
    datac$ciLower <- datac[,measurevar] - datac[,"se"]
    datac$ciUpper <- datac[,measurevar] + datac[,"se"]
    
  }
  
  return(datac)
}

.normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL, na.rm=FALSE, .drop=TRUE) {
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- plyr::ddply(data, c(idvar, betweenvars), .drop=.drop,
                               .fun = function(xx, col, na.rm) {
                                 c(subjMean = mean(xx[,col], na.rm=na.rm))
                               },
                               measurevar,
                               na.rm
  )
  
  
  
  # Put the subject means with original data
  data <- base::merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

.summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE, 
                             conf.interval=.95, .drop=TRUE, errorBarType="confidenceInterval", usePooledSE=FALSE) {
  
  # Get the means from the un-normed data
  datac <- .summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm, 
                      conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType, usePooledSE=usePooledSE)
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  datac$ciLower <- NULL
  datac$ciUpper <- NULL
  
  # Norm each subject's data
  ndata <- .normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- .summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval, .drop=.drop, errorBarType=errorBarType,
                       usePooledSE=usePooledSE)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  # Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels, FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  if (errorBarType == "confidenceInterval") {
    
    ndatac$ciLower <- datac[,measurevar] - ndatac[,"ci"]
    ndatac$ciUpper <- datac[,measurevar] + ndatac[,"ci"]
    
  } else {
    
    ndatac$ciLower <- datac[,measurevar] - ndatac[,"se"]
    ndatac$ciUpper <- datac[,measurevar] + ndatac[,"se"]
    
  }
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

