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
  if (isTryError(longData)) 
    .quitAnalysis(gettext("Error while loading data. Please verify your repeated measures observations."))
  
  ready <- all(options$repeatedMeasuresCells != "") &&  length(options$withinModelTerms) > 0

  rmAnovaContainer <- .getRMAnovaContainer(jaspResults)
  
  .BANOVAerrorhandling(longData, options, "RM-ANOVA")

  .rmAnovaComputeResultsContainer(rmAnovaContainer, longData, options, ready)
  
  .rmAnovaWithinSubjectsTable(rmAnovaContainer, dataset, options, ready)  
  
  .rmAnovaBetweenSubjectsTable(rmAnovaContainer, dataset, options, ready)
  
  .referenceGrid(rmAnovaContainer, options, ready)
  
  .rmAnovaAssumptionsContainer(rmAnovaContainer, dataset, options, ready)
  
  .rmAnovaPostHocTable(rmAnovaContainer, dataset, longData, options, ready)

  .rmAnovaContrastTable(rmAnovaContainer, longData, options, ready)

  .rmAnovaMarginalMeansTable(rmAnovaContainer, dataset, options, ready)
  
  .rmAnovaFriedmanTable(rmAnovaContainer, longData, options, ready)
  
  .rmAnovaConoverTable(rmAnovaContainer, longData, options, ready)
  
  .rmAnovaSimpleEffects(rmAnovaContainer, dataset, longData, options, ready) 
  
  .BANOVAdescriptives(rmAnovaContainer, longData, options, list(noVariables=FALSE), "RM-ANOVA", ready)
  
  return()
}

.getRMAnovaContainer <- function(jaspResults) {
  
  if (!is.null(jaspResults[["rmAnovaContainer"]])) {
    
    anovaContainer <- jaspResults[["rmAnovaContainer"]]
    
  } else {
    
    anovaContainer <- createJaspContainer()
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    anovaContainer$dependOn(c("withinModelTerms", "betweenModelTerms", "repeatedMeasuresCells", "betweenSubjectFactors",
                              "repeatedMeasuresFactors", "covariates", "sumOfSquares", "useMultivariateModelFollowup"))
    jaspResults[["rmAnovaContainer"]] <- anovaContainer
  }
  
  return(anovaContainer)
  
}

.rmAnovaCheckErrors <- function(dataset, options, ready) {
  if (!ready) 
    return()
  
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
        errorMessage <- gettextf("Factor: <em>%s</em>, contains fewer than two levels.", independentsWithLessThanTwoLevels)
      } else {
        errorMessage <- gettextf("Factors: <em>%s</em>, contain fewer than two levels.", paste(independentsWithLessThanTwoLevels, collapse=",", sep=""))
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
        errorMessage <- gettextf("The repeated measure: <em>%s</em>, contains infinite values.", options$repeatedMeasuresCells[infiniteRM])
      } else {
        errorMessage <- gettextf("The repeated measures: <em>%s</em>, contain infinite values.", paste(options$repeatedMeasuresCells[infiniteRM], collapse=", "))
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
        errorMessage <- gettextf("The covariate: <em>%s</em>, contains infinite values.", options$covariates[infiniteCov])
      } else {
        errorMessage <- gettextf("The covariates: <em>%s</em>, contain infinite values.", paste(options$covariates[infiniteCov], collapse=", "))
      }
    }
    
    allNames <- unlist(lapply(options[['repeatedMeasuresFactors']], function(x) x$name)) # Factornames 
    for(factorName in allNames){
      if (any(factorName %in% options$betweenSubjectFactors )) {
        error <- TRUE
        errorMessage <- gettext("Please choose a name for the RM factors that differs from those for the between subjects factors.")
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
  errorRM <- paste("Error(",paste0(.BANOVAsubjectName, "/(", termsRM.base64, ")", collapse=" + "),")",sep="")
  
  if (is.null(termsBS.base64) && is.null(termsRM.base64)) {
    model.def <- dependent ~ 1
  } else if (is.null(termsBS.base64)) {
    model.def <- paste(.BANOVAdependentName, "~", paste(main, errorRM, sep=" + "))
  } else if (is.null(termsRM.base64)) {
    model.def <- paste(.BANOVAdependentName, "~", main)
  } else {
    model.def <- paste(.BANOVAdependentName, "~", paste(main, errorRM, termsBS, sep=" + "))
  }
  
  list(model.def = model.def, terms.normal = terms.normal, terms.base64 = terms.base64, termsRM.normal = termsRM.normal, termsRM.base64 = termsRM.base64)
}

.rmAnovaComputeResultsContainer <- function(rmAnovaContainer, longData, options, ready) {
  if (!ready || !is.null(rmAnovaContainer[["anovaResult"]])) 
    return()
  
  rmAnovaResult <- .rmAnovaComputeResults(longData, options)

  if (rmAnovaResult[["tryResult"]] == "try-error") {
    rmAnovaContainer$setError(gettext("Some parameters are not estimable, most likely due to empty cells of the design."))
    return()
  }
  
  # Save model to state
  rmAnovaContainer[["anovaResult"]] <- createJaspState(object = rmAnovaResult)
}

.rmAnovaComputeResults <- function(dataset, options, returnResultsEarly = FALSE) {

  modelDef <- .rmModelFormula(options)
  model.formula <- as.formula(modelDef$model.def)
  options(contrasts=c("contr.sum","contr.poly"))
  
  # set these options once for all afex::aov_car calls,
  # this ensures for instance that afex::aov_car always returns objects of class afex_aov.
  if (options$useMultivariateModelFollowup)   followupModelType <- "multivariate" else followupModelType <- "univariate"
  afex::afex_options(
    check_contrasts = TRUE, correction_aov = "GG", 
    emmeans_model = followupModelType, es_aov = "ges", factorize = TRUE, 
    lmer_function = "lmerTest", method_mixed = "KR", return_aov = "afex_aov", 
    set_data_arg = FALSE, sig_symbols = c(" +", " *", " **", " ***"), type = 3
  )

  # Computations:
  if (options$sumOfSquares == "type1") {
    tryResult <- try({

      result <- stats::aov(model.formula, data=dataset)
      summaryResultOne <- summary(result, expand.split = FALSE)
    
      result <- afex::aov_car(model.formula, data=dataset, type= 3, factorize = FALSE, include_aov = TRUE)
      summaryResult <- summary(result)

      # Reformat the results to make it consistent with types 2 and 3
      model <- as.data.frame(unclass(summaryResult$univariate.tests))

      for (mySub in unlist(summaryResultOne, recursive = FALSE)) {
        rownames(mySub) <- trimws(rownames(mySub))
        for(term in rownames(mySub)[-nrow(mySub)]) {
          model[term, "Sum Sq"]   <- mySub[term,        "Sum Sq"]
          model[term, "num Df"]   <- mySub[term,        "Df"]
          model[term, "F value"]  <- mySub[term,        "F value"]
          model[term, "Pr(>F)"]   <- mySub[term,        "Pr(>F)"]
          model[term, "Error SS"] <- mySub["Residuals", "Sum Sq"]
          model[term, "den Df"]   <- mySub["Residuals", "Df"]
        }
      }
    })

    } else if (options$sumOfSquares == "type2") {
    
    tryResult <- try({
      result <- afex::aov_car(model.formula, data=dataset, type= 2, factorize = FALSE, include_aov = TRUE)
      summaryResult <- summary(result)
      model <- as.data.frame(unclass(summaryResult$univariate.tests))
    })
    
  } else {
    
    tryResult <- try({
      result <- afex::aov_car(model.formula, data=dataset, type= 3, factorize = FALSE, include_aov = TRUE)
      summaryResult <- summary(result)
      model <- as.data.frame(unclass(summaryResult$univariate.tests))
    })
    
  }

  if (class(tryResult) == "try-error") {
    return(list(tryResult = "try-error"))
  }

  if (returnResultsEarly)
    return(list(result = result, model = model))

  # Now we reformat the results table some more to make it flow with jaspResults later
  interceptRow <- model["(Intercept", ]
  model <- model[-1,]
  rownames(model) <- trimws(rownames(model))
  model[["isWithinTerm"]] <- model[[".isNewGroup"]] <- logical(nrow(model))

  sortedModel <- model
  cases <- unlist(sapply(modelDef$terms.base64, function(x) x[[1]]))
  residualResults <- sortedModel[.mapAnovaTermsToTerms(cases, rownames(model)), ]

  nextNewGroup <- 0
  for (modelTerm in modelDef$terms.base64) {

    if (!is.null(modelTerm)) {
      isWithin <- any(modelTerm %in% modelDef$termsRM.base64)
      indices <- .mapAnovaTermsToTerms(modelTerm, rownames(model))
      nextNewGroup <- c(TRUE, rep(FALSE, length(indices) - 1))
      sortedModel[indices, ] <- model[indices, ]
      sortedModel[indices, c(".isNewGroup", "isWithinTerm")] <- c(nextNewGroup, rep(isWithin, length(indices)))
      
      residualRow <- c(model[indices[1],  c("Error SS", "den Df")], rep(NA, 4), 0, isWithin)
      residualResults[.mapAnovaTermsToTerms(modelTerm[[1]], rownames(residualResults)), ] <-  residualRow
    }

  }

  # Make sure that order of anova result corresponds to order of specified model terms
  mappedRownamesCases <- .mapAnovaTermsToTerms(rownames(sortedModel), unlist(modelDef$terms.base64))

  sortedModel[["case"]] <- unlist(modelDef$terms.normal)[mappedRownamesCases]
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

  if (!is.null(rownames(corrections)) && length(rownames(corrections)) > 0) {
    corrections <- as.data.frame(corrections)
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
    
  } 
  
  # If corrections could not be run, create data frame with NA's
  if (length(rownames(corrections)) == 0 )
    corrections <- matrix(ncol = 4, nrow = nrow(withinAnovaTable), NA, 
                          dimnames = list(rownames(withinAnovaTable), c("GG eps", "Pr(>F[GG])", "HF eps", "Pr(>F[HF])")))
  
  withinIndices <- .mapAnovaTermsToTerms(rownames(withinAnovaTable), rownames(corrections))

  ggTable[["num Df"]]          <- withinAnovaTable[["num Df"]] * corrections[withinIndices, "GG eps"]
  ggTable[["Mean Sq"]]         <- withinAnovaTable[["Sum Sq"]] / ggTable[["num Df"]]
  ggTable[["den Df"]]          <- withinAnovaTable[["den Df"]] * corrections[withinIndices, "GG eps"]
  ggTable[["Pr(>F)"]]          <- pf(withinAnovaTable[["F value"]], ggTable[["num Df"]], ggTable[["den Df"]], lower.tail = FALSE)
  ggTable[["correction"]]      <- gettext("Greenhouse-Geisser")
  ggTable[[".isNewGroup"]]     <- FALSE

  hfTable[["num Df"]]          <- withinAnovaTable[["num Df"]] * corrections[withinIndices, "HF eps"]
  hfTable[["Mean Sq"]]         <- withinAnovaTable[["Sum Sq"]] / hfTable[["num Df"]]
  hfTable[["den Df"]]          <- withinAnovaTable[["den Df"]] * corrections[withinIndices, "HF eps"]
  hfTable[["Pr(>F)"]]          <- pf(withinAnovaTable[["F value"]], hfTable[["num Df"]], hfTable[["den Df"]], lower.tail = FALSE)
  hfTable[["correction"]]      <- gettext("Huynh-Feldt")
  hfTable[[".isNewGroup"]]     <- FALSE

  residualResults[["eta"]]     <- residualResults[["etaPart"]] <- residualResults[["genEta"]] <-
  residualResults[["omega"]]   <- residualResults[["p"]] <- as.numeric(NA)
  
  wResidualResults <- wResidualResultsGG <- wResidualResultsHF <- subset(residualResults, isWithinTerm == TRUE)

  wResidualResults[["correction"]]   <- gettext("None")
  wResidualResults[[".isNewGroup"]]  <- TRUE
  
  residualIndices                    <- .mapAnovaTermsToTerms(rownames(wResidualResults), rownames(corrections))
  wResidualResultsGG[["num Df"]]     <- wResidualResults[["num Df"]] * corrections[residualIndices, "GG eps"]
  wResidualResultsGG[["Mean Sq"]]    <- wResidualResults[["Sum Sq"]] / wResidualResultsGG[["num Df"]]
  wResidualResultsGG[["correction"]] <- gettext("Greenhouse-Geisser")

  wResidualResultsHF[["num Df"]]     <- wResidualResults[["num Df"]] * corrections[residualIndices, "HF eps"]
  wResidualResultsHF[["Mean Sq"]]    <- wResidualResults[["Sum Sq"]] / wResidualResultsHF[["num Df"]]
  wResidualResultsHF[["correction"]] <- gettext("Huynh-Feldt")

  withinAnovaTable <- cbind(withinAnovaTable, corrections[withinIndices, ], sphericityTests[withinIndices, ])

  # Makes lists with results
  withinAnovaTableCollection <- list("None" = withinAnovaTable, "Huynh-Feldt" = hfTable,            "Greenhouse-Geisser" = ggTable)
  wResidualResultsList       <- list("None" = wResidualResults, "Huynh-Feldt" = wResidualResultsHF, "Greenhouse-Geisser" = wResidualResultsGG)
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
  if(!is.null(rmAnovaContainer[["betweenTable"]])) 
    return()
  
  betweenTable <- createJaspTable(title = gettext("Between Subjects Effects"), position = 2)
  betweenTable$dependOn(c("effectSizeEstimates", "effectSizeEtaSquared", "effectSizePartialEtaSquared", 
                             "effectSizeGenEtaSquared", "effectSizeOmegaSquared", "VovkSellkeMPR"))
  
  betweenTable$addColumnInfo(title = gettext("Cases"),          name = "case",    type = "string" )
  betweenTable$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
  betweenTable$addColumnInfo(title = gettext("df"),             name = "num Df",  type = "integer")
  betweenTable$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
  betweenTable$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
  betweenTable$addColumnInfo(title = gettext("p"),              name = "Pr(>F)",  type = "pvalue")
  
  if (options$VovkSellkeMPR && length(options$betweenSubjectFactors) > 0) {
    betweenTable$addColumnInfo(title = gettextf("VS-MPR%s", "\u002A"), name = "VovkSellkeMPR", type = "number")
    betweenTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
  
  if (options$effectSizeEstimates && length(options$betweenSubjectFactors) > 0) {
    
    if (options$effectSizeEtaSquared) 
      betweenTable$addColumnInfo(title = "\u03B7\u00B2", name = "eta", type = "number")
    
    if (options$effectSizePartialEtaSquared) 
      betweenTable$addColumnInfo(title = "\u03B7\u00B2\u209A", name = "etaPart", type = "number")
    
    if (options$effectSizeGenEtaSquared) 
      betweenTable$addColumnInfo(title = gettextf("%s<sub>G</sub>", "\u03B7\u00B2"), name = "genEta", type = "number")
    
    if (options$effectSizeOmegaSquared) 
      betweenTable$addColumnInfo(title = "\u03C9\u00B2", name = "omega", type = "number")
    
  }
  
  betweenTable$showSpecifiedColumnsOnly <- TRUE
  
  .addSumSquaresFootnote(betweenTable, options)
  
  rmAnovaContainer[["betweenTable"]] <- betweenTable
  
  if (!ready || rmAnovaContainer$getError()) {
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
  
  anovaTable <- createJaspTable(title = gettext("Within Subjects Effects"), position = 1)
  rmAnovaContainer[["withinAnovaTable"]] <- anovaTable
  anovaTable$showSpecifiedColumnsOnly <- TRUE
  anovaTable$dependOn(c("sphericityGreenhouseGeisser", "sphericityHuynhFeldt",
                        "sphericityNone", "VovkSellkeMPR", "effectSizeEstimates", "effectSizeEtaSquared",
                        "effectSizePartialEtaSquared", "effectSizeGenEtaSquared", "effectSizeOmegaSquared"))

  corrections <- c("None", "Greenhouse-Geisser", "Huynh-Feldt")[c(options$sphericityNone, 
                                                                 options$sphericityGreenhouseGeisser,
                                                                 options$sphericityHuynhFeldt)]
  if (length(corrections) == 0) corrections <- "None"
  
  anovaTable$addColumnInfo(title = gettext("Cases"), name = "case", type = "string", combine = TRUE)
  
  dfType <- "integer" # Make df an integer unless corrections are applied
  if ((length(corrections) > 1 || any(!"None" %in% corrections))) {
    anovaTable$addColumnInfo(title = gettext("Sphericity Correction"), name = "correction", type = "string")
    dfType <- "number"
  }
  
  anovaTable$addColumnInfo(title = gettext("Sum of Squares"), name = "Sum Sq",  type = "number")
  anovaTable$addColumnInfo(title = gettext("df"),             name = "num Df",  type = dfType)
  anovaTable$addColumnInfo(title = gettext("Mean Square"),    name = "Mean Sq", type = "number")
  anovaTable$addColumnInfo(title = gettext("F"),              name = "F value", type = "number")
  anovaTable$addColumnInfo(title = gettext("p"),              name = "p",       type = "pvalue")
  
  if (options$VovkSellkeMPR) {
    anovaTable$addColumnInfo(title = gettextf("VS-MPR%s", "\u002A"), name = "VovkSellkeMPR", type = "number")
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
      anovaTable$addColumnInfo(name="genEta", type="number", title=gettextf("%s<sub>G</sub>", "\u03B7\u00B2"))
    }
    
    if (options$effectSizeOmegaSquared) {
      anovaTable$addColumnInfo(title = "\u03C9\u00B2", name = "omega", type = "number")
    }
    
  }
  
  .addSumSquaresFootnote(anovaTable, options)

  if (!ready || rmAnovaContainer$getError())
    return()
  
  withinResults   <- rmAnovaContainer[["anovaResult"]]$object$withinAnovaTable
  residualResults <- rmAnovaContainer[["anovaResult"]]$object$residualTable
  mauchlyResult   <- rmAnovaContainer[["anovaResult"]]$object$assumptionResult
  
  for(i in .indices(withinResults)) {
    names(withinResults[[i]])[names(withinResults[[i]]) == "Pr(>F)"] <- "p"
  }
  
  modelTerms <- .rmModelFormula(options)$termsRM.base64
  allCases <- rownames(withinResults[[1]])
  addResidualAfter <- allCases[.mapAnovaTermsToTerms(modelTerms, allCases) + (length(allCases) / length(modelTerms)) - 1]

  for (case in allCases) {

    for (i in .indices(corrections)) {

      withinResults[[corrections[i]]][case, ".isNewGroup"] <- i == 1
      if (!is.na(withinResults[[corrections[i]]][case, "num Df"])) {
        anovaTable$addRows(as.list(withinResults[[corrections[i]]][case, ]),
                           rowNames=paste0(case, corrections[i]))  
      } else {
        anovaTable$addFootnote(gettext("Sphericity corrections not available for factors with 2 levels."))
      }
      
    }

    if (case %in% modelTerms) {
      currentCase <- case
    }  
    if (case %in% addResidualAfter) {
      
      for (i in .indices(corrections)) {

        if (!is.na(withinResults[[corrections[i]]][currentCase, "num Df"])) {
          residualResults[[corrections[i]]][currentCase, ".isNewGroup"] <- i == 1  
          anovaTable$addRows(as.list(residualResults[[corrections[i]]][currentCase, ]), 
                           rowNames=paste0(currentCase, "Resid", corrections[i]))
        }
      }
    }
  }

  if (!all(is.na(withinResults[[1]][["Test statistic"]]))) {
    violatedMauchlyCases <- rownames(mauchlyResult)[mauchlyResult[, "p-value"] < 0.05]
    if (length(violatedMauchlyCases) > 0)
      anovaTable$addFootnote(message = gettext("Mauchly's test of sphericity indicates that the assumption of sphericity is violated (p < .05)."),
                             colNames = c("Sum Sq", "num Df", "F value", "Mean Sq", "Pr(F)", "p"),
                             rowNames = paste0(violatedMauchlyCases, "None"))
  }
  
  return()
}

.rmAnovaAssumptionsContainer <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["assumptionsContainer"]]))
    return()
  
  assumptionsContainer <- createJaspContainer(title = gettext("Assumption Checks"),
                                              dependencies = c("homogeneityTests", "sphericityTests"))
  
  rmAnovaContainer[["assumptionsContainer"]] <- assumptionsContainer

    if (options$homogeneityTests == TRUE)  
    .rmAnovaLevenesTable(rmAnovaContainer, dataset, options, ready)
  
  if (options$sphericityTests == TRUE) 
    .rmAnovaSphericityTable(rmAnovaContainer, dataset, options, ready)
  
  return()
}

.rmAnovaLevenesTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["rmAnovaLevenesTable"]]))
    return()

  rmAnovaLevenesTable <- createJaspTable(gettext("Test for Equality of Variances (Levene's)"))
  rmAnovaContainer[["assumptionsContainer"]][["rmAnovaLevenesTable"]] <- rmAnovaLevenesTable
  
  rmAnovaLevenesTable$addColumnInfo(name="case", type="string", title="")
  rmAnovaLevenesTable$addColumnInfo(name="F", type="number")
  rmAnovaLevenesTable$addColumnInfo(name="df1", type="integer")
  rmAnovaLevenesTable$addColumnInfo(name="df2", type="integer")
  rmAnovaLevenesTable$addColumnInfo(name="p", type="pvalue")
  
  if (options$VovkSellkeMPR) {
    rmAnovaLevenesTable$addColumnInfo(title = "VS-MPR\u002A", name = "VovkSellkeMPR", type = "number")
    rmAnovaLevenesTable$addFootnote(message = .messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }
  rmAnovaLevenesTable$showSpecifiedColumnsOnly <- TRUE

  if (!ready || rmAnovaContainer$getError())
    return()
  
  rmAnovaLevenesTable$setExpectedSize(length(options$repeatedMeasuresCells))
  if (length(options$betweenModelTerms) == 0) {
    rmAnovaLevenesTable$setError(gettext("Cannot perform homogeneity tests because there are no between subjects factors specified."))
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

  sphericityTable <- createJaspTable(gettext("Test of Sphericity"))
  
  sphericityTable$addColumnInfo(name="case",            type="string",  title="")
  sphericityTable$addColumnInfo(name="Test statistic",  type="number",  title=gettext("Mauchly's W"))
  sphericityTable$addColumnInfo(name="approxChi",       type="number",  title=gettextf("Approx. %s", "\u03A7\u00B2"))
  sphericityTable$addColumnInfo(name="dfSphericity",    type="integer", title=gettext("dfSphericity"))
  sphericityTable$addColumnInfo(name="p-value",         type="pvalue",  title=gettext("p-value"))
  sphericityTable$addColumnInfo(name="GG eps",          type="number",  title=gettextf("Greenhouse-Geisser %s", "\u03B5"))
  sphericityTable$addColumnInfo(name="HF eps",          type="number",  title=gettextf("Huynh-Feldt %s", "\u03B5"))
  sphericityTable$addColumnInfo(name="LB",              type="number",  title=gettextf("Lower Bound %s", "\u03B5"))

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
    sphericityTable$setError(gettext("Cannot perform sphericity tests because there only two levels of the RM factor."))
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

  variables <- sapply(c(options$withinModelTerms, options$betweenModelTerms),
                      function(x) {paste(.v(x$components), collapse = ":")}) 
  
  if (length(options$betweenModelTerms) > 0) {
    mixedTerms <- sapply(options$withinModelTerms, 
                         function(x) {sapply(options$betweenModelTerms, 
                                             function(y) {paste(c(.v(y$components), .v(x$components)), collapse = ":")})}) 
    variables <- union(variables, mixedTerms)
  }
  
  for (var in variables) {
    formula <- as.formula(paste("~", var))
    referenceGrid <- emmeans::emmeans(fullModel, formula)
    referenceGridList[[var]] <- referenceGrid
  }
  
  rmAnovaContainer[["referenceGrid"]] <- createJaspState(object = referenceGridList, 
                                                         dependencies = c("withinModelTerms",
                                                                          "betweenModelterms"))
  
  return()
}

.rmAnovaPostHocTable <- function(rmAnovaContainer, dataset, longData, options, ready) {
  if(!is.null(rmAnovaContainer[["postHocStandardContainer"]]) || length(options$postHocTestsVariables) ==0)
    return()
  
  postHocContainer <- createJaspContainer(title = gettext("Post Hoc Tests"))
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
  
  if (!ready || rmAnovaContainer$getError())
    return()
  
  referenceGrid <- rmAnovaContainer[["referenceGrid"]]$object
  fullModel <- rmAnovaContainer[["anovaResult"]]$object$fullModel
  allNames <- unlist(lapply(options$repeatedMeasuresFactors, function(x) x$name)) # Factornames 

  balancedDesign <-   all(sapply(unlist(options$betweenModelTerms), function(x) length(unique(table(dataset[[.v(x)]]))) == 1))
  
  for (var in variables) {

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

      if (!options$postHocTestPooledError && balancedDesign) {

        numberOfLevels <- length(levels(longData[[var]]))
        
        # Loop over all the levels within factor and do pairwise t.tests on them
        bonfAdjustCIlevel <- 1-((1-options$confidenceIntervalIntervalPostHoc)/choose(numberOfLevels, 2))
        
        for (compIndex in .indices(comparisons)) {
          
          levelANoDots <- gsub(.unv(comparisons[[compIndex]][1]), pattern = "\\.", replacement = " ")
          levelBNoDots <- gsub(.unv(comparisons[[compIndex]][2]), pattern = "\\.", replacement = " ")
          facLevelNoDots <- gsub(longData[[var]], pattern = "\\.", replacement = " ")

          # gsubs necessary to deal with X and "." introduced to level names by emmeans
          x <- subset(longData, gsub("X", "", facLevelNoDots) == gsub("X", "", levelANoDots))
          x <- tapply(x[[.BANOVAdependentName]], x[[.BANOVAsubjectName]], mean)
          y <- subset(longData, gsub("X", "", facLevelNoDots) == gsub("X", "", levelBNoDots))
          y <- tapply(y[[.BANOVAdependentName]], y[[.BANOVAsubjectName]], mean)

          tResult <- t.test(x, y, paired = TRUE, var.equal = FALSE, conf.level = bonfAdjustCIlevel)
          tResult <- unname(unlist(tResult[c("estimate", "statistic", "p.value", "conf.int")]))
          resultPostHoc[compIndex, c("estimate", "t.ratio", "p.value", "lower.CL", "upper.CL")] <- tResult
          
        }
        
        resultPostHoc[["SE"]] <- resultPostHoc[["estimate"]] / resultPostHoc[["t.ratio"]]
        resultPostHoc[["bonferroni"]] <- p.adjust(resultPostHoc[["p.value"]], method = "bonferroni")
        resultPostHoc[["holm"]] <- p.adjust(resultPostHoc[["p.value"]], method = "holm")
        
      } else if (!options$postHocTestPooledError) {
        postHocContainer$setError(gettext("Unpooled error term only allowed in balanced designs."))
        return()
      }
      
      resultPostHoc[["scheffe"]] <- "."
      resultPostHoc[["tukey"]] <-  "."
      if (options$postHocTestsScheffe || options$postHocTestsTukey) {
        cors <- paste(c("Tukey", "Scheffe")[c(options$postHocTestsTukey, options$postHocTestsScheffe)], collapse = " and ")
        
        postHocContainer[[var]]$addFootnote(gettextf("%s corrected p-values are not appropriate for repeated measures post-hoc tests (Maxwell, 1980; Field, 2012).", cors))
      }
    }

    if (!grepl(var, pattern = ":"))
      resultPostHoc[['cohenD']] <- resultPostHoc[['t.ratio']] / sqrt(nrow(dataset))

    resultPostHoc[["contrast_A"]] <- lapply(comparisons, function(x) paste(.unv(strsplit(x[[1]], " ")[[1]]), 
                                                                           collapse = ", "))
    resultPostHoc[["contrast_B"]] <- lapply(comparisons, function(x) paste(.unv(strsplit(x[[2]], " ")[[1]]), 
                                                                           collapse = ", "))
    
    if (nrow(resultPostHoc) > 1)
      postHocContainer[[var]]$addFootnote(.getCorrectionFootnoteAnova(resultPostHoc, 
                                                                      options$confidenceIntervalsPostHoc))
    
    avFootnote <- attr(resultPostHoc, "mesg")[grep(attr(resultPostHoc, "mesg"), pattern = "Results are averaged")]
    if (length(avFootnote) != 0) {
      avTerms <- .unv(strsplit(gsub(avFootnote, pattern = "Results are averaged over the levels of: ", replacement = ""), 
                                 ", ")[[1]])
      postHocContainer[[var]]$addFootnote(gettextf("Results are averaged over the levels of: %s", paste(avTerms, collapse = ", ")))
    }
    
    
    resultPostHoc[[".isNewGroup"]] <- !duplicated(resultPostHoc[["contrast_A"]])
    postHocContainer[[var]]$setData(resultPostHoc)

    
    if (options$postHocFlagSignificant)
      .anovaAddSignificanceSigns(someTable = postHocContainer[[var]],
                                 allPvalues = resultPostHoc[c("bonferroni", "scheffe", "tukey", "holm")],
                                 resultRowNames = rownames(resultPostHoc))
  }
  
  return()
}

.getCorrectionFootnoteAnova <- function(postHocObject, includeCI = FALSE) {
  
  pvalAdjust <- attr(postHocObject, "mesg")[grep(attr(postHocObject, "mesg"), pattern = "P value adjustment")]
  nEstimates <- regmatches(pvalAdjust, gregexpr("[[:digit:]]+", pvalAdjust))[[1]]
  confAdjust <- attr(postHocObject, "mesg")[grep(attr(postHocObject, "mesg"), pattern = "Conf-level")]
  confAdjust <- gsub(x = confAdjust, pattern = "Conf-level adjustment: ", "")
  confAdjust <- strsplit(confAdjust, " ")[[1]][1]
  
  if (!includeCI) {
    correctionFootnote <- gettextf("P-value adjusted for comparing a family of %s", as.character(nEstimates))
  } else {
    correctionFootnote <- gettextf("P-value and confidence intervals adjusted for comparing a family of %1$s estimates (confidence intervals corrected using the %2$s method).", nEstimates, confAdjust)
  }
  
  return(correctionFootnote)
}

.createPostHocStandardTable <- function(myTitle, interactionTerm, options, makeBootstrapTable = FALSE) {
  
  preTitle <- if (!makeBootstrapTable) gettext("Post Hoc Comparisons - ") else gettext("Bootstrapped Post Hoc Comparisons - ")
  postHocTable <- createJaspTable(title = paste0(preTitle, myTitle)) #this paste is ok
  
  postHocTable$addColumnInfo(name="contrast_A", title=" ", type="string", combine = TRUE)
  postHocTable$addColumnInfo(name="contrast_B", title=" ", type="string")
  
  postHocTable$addColumnInfo(name="estimate", title=gettext("Mean Difference"), type="number")
  
  if (options$confidenceIntervalsPostHoc || makeBootstrapTable) {
    
    if (makeBootstrapTable) {
      thisOverTitle <- gettextf("%1$s%% bca%2$s CI", options$confidenceIntervalIntervalPostHoc * 100, "\u2020")
    } else {
      thisOverTitle <- gettextf("%s%% CI for Mean Difference", options$confidenceIntervalIntervalPostHoc * 100)
    }
    
    postHocTable$addColumnInfo(name="lower.CL", type = "number", title = gettext("Lower"), overtitle = thisOverTitle)
    postHocTable$addColumnInfo(name="upper.CL", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
  } 
  
  postHocTable$addColumnInfo(name="SE", title=gettext("SE"), type="number")
  
  if (makeBootstrapTable)
    postHocTable$addColumnInfo(name="bias", title=gettext("bias"), type="number")

  
  postHocTable$addColumnInfo(name="t.ratio", title=gettext("t"), type="number")
  
  if (options$postHocTestEffectSize && !interactionTerm) {
    postHocTable$addColumnInfo(name="cohenD", title=gettext("Cohen's d"), type="number")
    postHocTable$addFootnote(gettext("Cohen's d does not correct for multiple comparisons."))
  }
  
  if (options$postHocTestsTukey)
    postHocTable$addColumnInfo(name="tukey",    title=gettext("p<sub>tukey</sub>"), type="pvalue")
  
  if (options$postHocTestsScheffe)
    postHocTable$addColumnInfo(name="scheffe", title=gettext("p<sub>scheffe</sub>"), type="pvalue")
  
  if (options$postHocTestsBonferroni)
    postHocTable$addColumnInfo(name="bonferroni", title=gettext("p<sub>bonf</sub>"), type="pvalue")
  
  if (options$postHocTestsHolm)
    postHocTable$addColumnInfo(name="holm", title=gettext("p<sub>holm</sub>"), type="pvalue")
  
  if (options$postHocTestsSidak)
    postHocTable$addColumnInfo(name="sidak", title=gettext("p<sub>sidak</sub>"), type="pvalue")
  
  
  postHocTable$showSpecifiedColumnsOnly <- TRUE
  
  return(postHocTable)
}

.rmAnovaContrastTable <- function(rmAnovaContainer, longData, options, ready) {
  if (!is.null(rmAnovaContainer[["contrastContainer"]]) || all(grepl("none", options$contrasts)))
    return()
  
  contrastContainer <- createJaspContainer(title = gettext("Contrast Tables"))
  contrastContainer$dependOn(c("contrasts", "contrastAssumeEqualVariance", "confidenceIntervalIntervalContrast", 
                               "confidenceIntervalsContrast", "customContrasts"))
  
  for (contrast in options$contrasts) {
    
    if (contrast$contrast != "none") {
      
      contrastType <- unlist(strsplit(contrast$contrast, ""))
      contrastType[1] <- toupper(contrastType[1])
      contrastType <- paste0(contrastType, collapse = "")
      
      if (length(contrast$variable) == 1) {
        contrastVariable <- contrast$variable
      } else {
        contrastVariable <- paste(contrast$variable, collapse = " \u273B ")
      }
      
      myTitle <- gettextf("%1$s Contrast - %2$s", contrastType,  contrastVariable)
      contrastContainerName <- paste0(contrast$contrast, "Contrast_",  paste(contrast$variable, collapse = ":"))
      dfType <- if (length(contrast$variable) > 1 || contrast$contrast == "custom") "number" else "integer"
      contrastContainer[[contrastContainerName]] <- createJaspContainer()
      contrastContainer[[contrastContainerName]][["contrastTable"]] <- .createContrastTableAnova(myTitle,
                                                                                                 options,
                                                                                                 dfType)
    }
    
  }
  
  rmAnovaContainer[["contrastContainer"]] <- contrastContainer
  
  if (!ready || rmAnovaContainer$getError())
    return()  
  
  referenceGrid <- rmAnovaContainer[["referenceGrid"]]$object

  for (contrast in options$contrasts) {
    
    contrastContainerName <- paste0(contrast$contrast, "Contrast_",  paste(contrast$variable, collapse = ":"))
    
    if (contrast$contrast != "none") {

      if (contrast$contrast == "custom") {
        customContrastSetup <- options$customContrasts[[which(sapply(options$customContrasts, 
                                                                     function(x) all(contrast$variable %in% x$value) &&
                                                                       length(contrast$variable) == length(x$value)))]]
      } else {
        customContrastSetup <- NULL
      }
      
      if (length(contrast$variable) == 1) {
        column <- longData[[ .v(contrast$variable) ]]
      } else {
        column <- factor(apply(longData[ .v(contrast$variable) ], 1, paste, collapse =", "))
      }

      contrastMatrix    <- .createContrastAnova(column, contrast$contrast, customContrastSetup)
      contrCoef         <- lapply(as.data.frame(contrastMatrix), as.vector)

      if (contrast$contrast != "custom") {
        contrCoef         <- lapply(as.data.frame(contrastMatrix), as.vector)
        names(contrCoef)  <- .anovaContrastCases(column, contrast$contrast)
      } else {
        contrCoef         <- apply(contrastMatrix, 1, list)
      }

      contrastResult    <- try(emmeans::contrast(referenceGrid[[paste(.v(contrast$variable), collapse = ":")]], contrCoef),
                               silent = TRUE)
      contrCoefEmmeans <- coef(contrastResult)
      colnames(contrCoefEmmeans) <- c(contrast$variable, paste("Comparison", 1: (ncol(contrCoefEmmeans) - length(contrast$variable))))
      
      if (contrast$contrast == "custom") {
        if (isTryError(contrastResult)) {
          if (grepl(contrastResult[1], pattern = "Nonconforming number")) {
            contrastContainer[[contrastContainerName]]$setError(gettext("Please specify an additional contrast."))
          } else if (grepl(contrastResult[1], pattern = "number of contrast matrix rows")) {
            contrastContainer[[contrastContainerName]]$setError(gettext("Wrong number of custom contrast matrix rows."))
          }
          return()
        } else if (any(apply(contrastMatrix, 1, function(x) all(x == 0) ))) {
          contrastContainer[[contrastContainerName]]$setError(gettext("Please specify non-zero contrast weights."))
          return()
        } 
      }

      if (length(contrastResult@misc$avgd.over) != 0)
        contrastContainer[[contrastContainerName]][["contrastTable"]]$addFootnote(
          message = gettextf("Results are averaged over the levels of: %s", paste(.unv(contrastResult@misc$avgd.over), collapse = ", ")))
      
      contrastResult <- cbind(contrastResult, confint(contrastResult, level = options$confidenceIntervalIntervalContrast)[,5:6])
      contrastResult[["Comparison"]] <- .unv(contrastResult[["contrast"]])
      
      if (options$contrastAssumeEqualVariance == FALSE && contrast$variable %in% unlist(options$withinModelTerms) && 
          length(contrast$variable) == 1 && contrast$contrast != "custom") {

        newDF <- do.call(data.frame, tapply(longData[[.BANOVAdependentName]], longData[[.v(contrast$variable)]], cbind))
        ssNr <- tapply(longData[[.BANOVAsubjectName]], longData[[.v(contrast$variable)]], cbind)
        
        for (i in 1:ncol(newDF)) {
          newDF[[i]] <- tapply(newDF[[i]], ssNr[[i]], mean)
        }
        newDF <- newDF[1:length(unique(ssNr[[1]])), ]
        
        allTestResults <- list()

        for (coefIndex in 1:length(contrCoef)) {
          allTestResults[[coefIndex]] <- t.test(as.matrix(newDF) %*% contrCoef[[coefIndex]])
        }
        
        contrastResult[["estimate"]]<- sapply(allTestResults, function(x) x[["estimate"]])
        contrastResult[["t.ratio"]] <- sapply(allTestResults, function(x) x[["statistic"]])
        contrastResult[["df"]]      <- sapply(allTestResults, function(x) x[["parameter"]])
        contrastResult[["SE"]]      <- sapply(allTestResults, function(x) x[["estimate"]] /  x[["statistic"]])
        contrastResult[["p.value"]] <- sapply(allTestResults, function(x) x[["p.value"]])
        
      } else if (options$contrastAssumeEqualVariance == FALSE) {
        
        contrastContainer[[contrastContainerName]]$setError(gettext("Unequal variances only available for main effects of within subjects factors"))
        return()
        
      }

      if (contrast$contrast == "custom" | length(contrast$variable) > 1) {
        contrastResult$Comparison <- 1:nrow(contrastResult)
        weightType <-  if (all(apply(contrastMatrix, 2, function(x) x %% 1 == 0))) "integer" else "number"
        contrastContainer[[contrastContainerName]][["customCoefTable"]] <- .createCoefficientsTableAnova(contrast, 
                                                                                                         contrCoefEmmeans, 
                                                                                                         weightType)
      }
      contrastContainer[[contrastContainerName]][["contrastTable"]]$setData(contrastResult)
      
    }
  }
 
}

.rmAnovaMarginalMeansTable <- function(rmAnovaContainer, dataset, options, ready) {
  if (!is.null(rmAnovaContainer[["marginalMeansContainer"]]) || length(options$marginalMeansTerms) == 0)
    return ()
  
  marginalMeansContainer <- createJaspContainer(title = gettext("Marginal Means"))
  marginalMeansContainer$dependOn(c("marginalMeansTerms",  "marginalMeansCompareMainEffects", "marginalMeansCIAdjustment",
                                    "marginalMeansBootstrapping", "marginalMeansBootstrappingReplicates"))
  
  rmAnovaContainer[["marginalMeansContainer"]] <- marginalMeansContainer
  
  marginalTerms <- unlist(options$marginalMeansTerms, recursive = FALSE)


  for (term in marginalTerms) {
    thisVarName <- paste(term, collapse = " \u273B ")
    individualTerms <- term
    if (any(term %in% unlist(options$withinModelTerms))) dfType <- "number" else dfType <- "integer"
    marginalMeansContainer[[paste0(.v(term), collapse = ":")]] <- .createMarginalMeansTableAnova(thisVarName, options, 
                                                                                                 individualTerms, 
                                                                                                 options$marginalMeansBootstrapping,
                                                                                                 dfType = dfType)
  }
  
  if (!ready || rmAnovaContainer$getError())
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
      
      startProgressbar(options[["marginalMeansBootstrappingReplicates"]],
                       label = gettext("Bootstrapping Marginal Means"))
      
      nRows <- nrow(marginalResult)

      bootstrapMarginalMeans <- try(boot::boot(data = dataset, statistic = .bootstrapMarginalMeansRmAnova, 
                                               R = options[["marginalMeansBootstrappingReplicates"]],
                                               options = options,
                                               nRows = nRows,
                                               termLength = length(term),
                                               formula = formula), silent = TRUE)

      if (class(bootstrapMarginalMeans) == "try-error") {
        marginalMeansContainer[[termBase64]]$setError(bootstrapMarginalMeans)
        next
      }
      
      bootstrapSummary <- summary(bootstrapMarginalMeans)
      
      ci.fails <- FALSE
      # bootstrapMarginalMeansCI <- confint(bootstrapMarginalMeans, level = 0.95, type = c("bca"))
      bootstrapMarginalMeansCI <- t(sapply(1:nrow(bootstrapSummary), function(index){
        res <- try(boot::boot.ci(boot.out = bootstrapMarginalMeans, conf = 0.95, type = "bca",
                                 index = index)[['bca']][1,4:5])
        if (!inherits(res, "try-error")){
          return(res)
        } else {
          ci.fails <<- TRUE
          return(c(NA, NA))
        } 
      }))
      
      if (ci.fails)
        marginalMeansContainer[[termBase64]]$addFootnote(message = gettext("Some confidence intervals could not be computed. Possibly too few bootstrap replicates."))

      marginalResult[["lower.CL"]] <- bootstrapMarginalMeansCI[,1]
      marginalResult[["upper.CL"]] <- bootstrapMarginalMeansCI[,2]

      marginalResult[["lsmean"]]   <- bootstrapSummary[["bootMed"]]
      marginalResult[["bias"]]     <- bootstrapSummary[["bootBias"]]
      marginalResult[["SE"]]       <- bootstrapSummary[["bootSE"]]
      
      marginalMeansContainer[[termBase64]]$addFootnote(message = gettextf("Bootstrapping based on %s successful replicates.", as.character(bootstrapSummary$R[1])))
    }

    marginalMeansContainer[[termBase64]]$setData(as.data.frame(marginalResult))
    
  }
  
  return()
}

.createMarginalMeansTableAnova <- function(myTitle, options, individualTerms, makeBootstrapTable = FALSE, dfType = "integer" ) {
  
  preTitle <- if (!makeBootstrapTable) gettext("Marginal Means - ") else gettext("Bootstrapped Marginal Means - ")
  marginalMeansTable <- createJaspTable(title = paste0(preTitle, myTitle))
  
  for (i in 1:length(individualTerms))
    marginalMeansTable$addColumnInfo(name=individualTerms[i], type="string", combine = TRUE)
  
  marginalMeansTable$addColumnInfo(name="lsmean", title=gettext("Marginal Mean"), type="number")
  
  if (makeBootstrapTable) {
    thisOverTitle <- gettextf("95%% bca%s CI", "\u002A")
    marginalMeansTable$addColumnInfo(name="bias", title=gettext("bias"), type="number")
    
    marginalMeansTable$addFootnote(message = gettext("Marginal Means estimate is based on the median of the bootstrap distribution."))
    marginalMeansTable$addFootnote(symbol = "\u2020", message = gettext("Bias corrected accelerated."))
    
  } else {
    thisOverTitle <- gettextf("95%% CI for Mean Difference")
  }
  
  marginalMeansTable$addColumnInfo(name="lower.CL", type = "number", title = gettext("Lower"), overtitle = thisOverTitle, )
  marginalMeansTable$addColumnInfo(name="upper.CL", type = "number", title = gettext("Upper"), overtitle = thisOverTitle)
  
  marginalMeansTable$addColumnInfo(name="SE", title=gettext("SE"), type="number")
  
  if (options$marginalMeansCompareMainEffects) {
    marginalMeansTable$addColumnInfo(name="t.ratio", title=gettext("t"),  type="number")
    marginalMeansTable$addColumnInfo(name="df",      title=gettext("df"), type=dfType)
    marginalMeansTable$addColumnInfo(name="p.value", title=gettext("p"),  type="pvalue")
    
    if (options$marginalMeansCIAdjustment == "bonferroni") {
      marginalMeansTable$addFootnote(message = gettext("Bonferroni CI adjustment"))
    } else if (options$marginalMeansCIAdjustment == "sidak") {
      marginalMeansTable$addFootnote(message = gettext("Sidak CI adjustment"))
    }
  }
  
  marginalMeansTable$showSpecifiedColumnsOnly <- TRUE
  
  return(marginalMeansTable)
}

.bootstrapMarginalMeansRmAnova <- function(data, indices, options, nRows, termLength, formula){
  
  # indices <- sample(indices, replace = TRUE)
  resamples <- data[indices, , drop=FALSE]
  
  dataset <- .shortToLong(resamples, options$repeatedMeasuresFactors, options$repeatedMeasuresCells, 
                          c(options$betweenSubjectFactors, options$covariates),
                          dependentName = .BANOVAdependentName, subjectName = .BANOVAsubjectName)

  anovaModelBoots <- .rmAnovaComputeResults(dataset, options, returnResultsEarly = TRUE)$result # refit model

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
  if (!is.null(rmAnovaContainer[["nonparametricContainer"]]) || length(options$friedmanWithinFactor) == 0)
    return ()
  
  rmAnovaContainer[["nonparametricContainer"]] <- createJaspContainer(gettext("Nonparametrics"))
  rmAnovaContainer[["nonparametricContainer"]]$dependOn(c("friedmanWithinFactor",
                                                          "friedmanBetweenFactor"))

  friedmanTable <- createJaspTable(title = gettext("Friedman Test"))
  friedmanTable$addColumnInfo(name="Factor",  title=gettext("Factor"),      type="string")
  friedmanTable$addColumnInfo(name="chiSqr",  title=gettext("Chi-Squared"), type="number")
  friedmanTable$addColumnInfo(name="df",      title=gettext("df"),          type="integer")
  friedmanTable$addColumnInfo(name="p",       title=gettext("p"),           type="pvalue")
  friedmanTable$addColumnInfo(name="kendall", title=gettext("Kendall's W"), type="number")

  rmAnovaContainer[["nonparametricContainer"]][["friedmanTable"]] <- friedmanTable

  if (!ready || rmAnovaContainer$getError())
    return()
  
  withinTerms <- options$friedmanWithinFactor
  betweenTerm <- options$friedmanBetweenFactor
  
  withinTerms.base64 <- .v(withinTerms)
  betweenTerms.base64 <- .v(betweenTerm)
  
  result <- list()
  
  if( any(!(withinTerms %in% unlist(options$withinModelTerms))) || 
      (betweenTerm %in% unlist(options$withinModelTerms)) ) {
    friedmanTable$setError(gettext("Please specify appropriate terms for the Friedman/Durbin test."))
    return()
  }
  
  if (identical(betweenTerm, "")) {
    betweenTerms.base64 <- .BANOVAsubjectName
  }
  
  rows <- list()
  
  for (i in 1:length(withinTerms)) {
    
    groups <- as.factor(longData[, withinTerms.base64[i]])
    blocks <- as.factor(longData[, betweenTerms.base64])
    y <- longData[, .BANOVAdependentName]
    
    useDurbin <- any(table(groups, blocks) != 1)
    
    t <- nlevels(groups)
    b <- nlevels(blocks)
    r <- unique(table(groups))
    k <- unique(table(blocks))
    
    if (length(r) == 1 && length(k) == 1) {
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
        friedmanTable$title <- gettext("Durbin Test")
        
        row[["F"]] <- testStatTwo
        row[["dfnum"]] <- dfOneF
        row[["dfden"]] <- dfTwoF
        row[["pF"]] <- pValTwo 
        
        if (i == 1) {
          friedmanTable$addColumnInfo(name="F",     title=gettext("F"),             type="number")
          friedmanTable$addColumnInfo(name="dfnum", title=gettext("df num"),        type="integer")
          friedmanTable$addColumnInfo(name="dfden", title=gettext("df den"),        type="integer")
          friedmanTable$addColumnInfo(name="pF",    title=gettext("p<sub>F</sub>"), type="pvalue")
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

  conoverContainer <- createJaspContainer(gettext("Conover Test"))
  rmAnovaContainer[["nonparametricContainer"]][["conoverContainer"]] <- conoverContainer
  conoverContainer$dependOn("conoverTest") 
  
  createConoverTable <- function(myTitle) {
    
    conoverTable <- createJaspTable(title = gettextf("Conover's Post Hoc Comparisons - %s", myTitle))
    
    conoverTable$addColumnInfo(name="(I)",        title="",                          type="string", combine=TRUE)
    conoverTable$addColumnInfo(name="(J)",        title="",                          type="string")
    conoverTable$addColumnInfo(name="t",          title=gettext("T-Stat"),           type="number")
    conoverTable$addColumnInfo(name="df",         title=gettext("df"),               type="integer")
    conoverTable$addColumnInfo(name="wA",         title=gettext("W<sub>i</sub>"),    type="number")
    conoverTable$addColumnInfo(name="wB",         title=gettext("W<sub>j</sub>"),    type="number")
    conoverTable$addColumnInfo(name="pval",       title=gettext("p"),                type="pvalue")
    conoverTable$addColumnInfo(name="bonferroni", title=gettext("p<sub>bonf</sub>"), type="pvalue")
    conoverTable$addColumnInfo(name="holm",       title=gettext("p<sub>holm</sub>"), type="pvalue")
    
    return(conoverTable)
  }

  
  if (!ready || rmAnovaContainer$getError())
    return()
  
  
  groupingVariables <- unlist(options$friedmanWithinFactor)
  blockingVar <- ifelse( identical(options$friedmanBetweenFactor, ""), .BANOVAsubjectName, .v(options$friedmanBetweenFactor))
  y <- longData[, .BANOVAdependentName]

  for (groupingVar in groupingVariables) {
    
    conoverTable <- createConoverTable(groupingVar)
    conoverTable$addFootnote(gettextf("Grouped by %s.", .unv(blockingVar)))
    
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

      row[[".isNewGroup"]] <- length(rows) == 0

    }
    
    allP <- unlist(lapply(rows, function(x) x$p))
    allBonf <- p.adjust(allP, method = "bonferroni")
    allHolm <- p.adjust(allP, method = "holm")
    
    for (p in 1:length(rows)) {
      rows[[p]][['bonferroni']] <- allBonf[p]
      rows[[p]][['holm']] <- allHolm[p]
    }
      
    conoverTable$setData(as.data.frame(do.call(rbind, rows)))
    rmAnovaContainer[["nonparametricContainer"]][["conoverContainer"]][[groupingVar]] <- conoverTable
  }
  
  return()
}

.rmAnovaSimpleEffects <- function(rmAnovaContainer, dataset, longData, options, ready) {
  if (!is.null(rmAnovaContainer[["simpleEffectsContainer"]]) || identical(options$simpleFactor, "") || 
      identical(options$moderatorFactorOne, ""))
    return()
  
  rmAnovaContainer[["simpleEffectsContainer"]] <- createJaspContainer(title = gettext("Simple Main Effects"),
                                                                      dependencies = c("simpleFactor", 
                                                                                       "moderatorFactorOne", 
                                                                                       "moderatorFactorTwo",
                                                                                       "poolErrorTermSimpleEffects"))
  
  simpleEffectsTable <- createJaspTable(title = gettextf("Simple Main Effects - %s", options$simpleFactor))
  rmAnovaContainer[["simpleEffectsContainer"]][["simpleEffectsTable"]] <- simpleEffectsTable
  
  moderatorTerms <- c(options$moderatorFactorOne, options$moderatorFactorTwo[!identical(options$moderatorFactorTwo, "")])
  nMods <- length(moderatorTerms)
  simpleFactor <- options[['simpleFactor']]
  simpleFactorBase64 <- .v(simpleFactor)
  
  simpleEffectsTable[["title"]] <- gettextf("Simple Main Effects - %s", options$simpleFactor)
  
  simpleEffectsTable$addColumnInfo(name = "modOne", title = gettextf("Level of %s", moderatorTerms[1]),
                                   type = "string", combine = TRUE)
  
  if (nMods == 2)
    simpleEffectsTable$addColumnInfo(name = "modTwo", title = gettextf("Level of %s", moderatorTerms[2]),
                                     type = "string", combine = TRUE)
  
  
  simpleEffectsTable$addColumnInfo(name = "SumSq",  type = "number",  title = gettext("Sum of Squares"))
  simpleEffectsTable$addColumnInfo(name = "Df",     type = "integer", title = gettext("df"))
  simpleEffectsTable$addColumnInfo(name = "MeanSq", type = "number",  title = gettext("Mean Square"))
  simpleEffectsTable$addColumnInfo(name = "F",      type = "number",  title = gettext("F"))
  simpleEffectsTable$addColumnInfo(name = "p",      type = "pvalue",  title = gettext("p"))
  
  simpleEffectsTable$showSpecifiedColumnsOnly <- TRUE
  
  .addSumSquaresFootnote(simpleEffectsTable, options)
  
  simpleEffectsTable$addCitation("Howell, D. C. (2002). Statistical Methods for Psychology (8th. ed.). Pacific Grove, CA: Duxberry. ")
  
  if (!ready || rmAnovaContainer$getError())
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

  
  if (isMixedAnova && !isSimpleFactorWithin) {
    
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
    simpleOptions[["dependent"]] <- .BANOVAdependentName
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

      anovaResult <-  .rmAnovaComputeResults(simpleDataset, simpleOptions, returnResultsEarly = TRUE)$model[simpleFactorBase64, ]

      if (!options$poolErrorTermSimpleEffects) {
        fullAnovaMS <-  anovaResult[["Error SS"]] / anovaResult[["den Df"]]
        fullAnovaDf <-  anovaResult[["den Df"]]
      }
      
      df <- anovaResult[["num Df"]]
      
    }

    MS <- anovaResult[["Mean Sq"]] <- anovaResult[["Sum Sq"]] /  df
    F <- MS / fullAnovaMS
    p <- pf(F, df, fullAnovaDf, lower.tail = FALSE)
    simpleEffectResult[i, c("SumSq", "MeanSq", "Df", "F", "p")] <- c(anovaResult[["Sum Sq"]], MS, df, F, p)
  }
  
  if (!is.null(emptyCaseIndices)) {
    simpleEffectsTable$addFootnote(gettextf("Not enough observations in cells %s.",
                                          paste0(" (", emptyCases, ")", collapse = ",")))
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
  if (usePooledSE && measurevar == "dependent") {

    data <- plyr::ddply(data, c(.BANOVAsubjectName, groupvars), plyr::summarise, dependent = mean(dependent))
    names(data)[which(names(data) == "dependent")] <- measurevar

  } else if (usePooledSE && measurevar == "dependent_norm") {

    data <- plyr::ddply(data, c(.BANOVAsubjectName, groupvars), plyr::summarise, dependent = mean(dependent_norm))
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

.addSumSquaresFootnote <- function(table, options)
{
  typeFootnote <- switch(options$sumOfSquares,
                         type1 = gettext("Type I Sum of Squares"),
                         type2 = gettext("Type II Sum of Squares"),
                         type3 = gettext("Type III Sum of Squares"))
  table$addFootnote(message = typeFootnote)
}
