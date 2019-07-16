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

RegressionLogLinear <- function(jaspResults, dataset, options, ...) {
  
  # Read dataset
  dataset <- .regLogLinReadData(dataset, options)
  
  ready <- !(length(options$factors) == 0)
  
  # Error checking
  .regLogLinCheckErrors(dataset, options, ready)
   
  # Compute the results
  model <- .regLogLinComputeModel(jaspResults, dataset, options, ready)
  
  # Output tables
  .regLogLinContainer(         jaspResults, dataset, options, model, ready)
  .regLogLinAnovaTable(        jaspResults, dataset, options, model, ready)
  .regLogLinCoefficientsTable (jaspResults, dataset, options, model, ready)
  
  return()
}

# Preprocessing functions ----
.regLogLinReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    counts <- NULL
    factors <- NULL
    if(options$counts != "")
      counts <- options$counts
    if(length(options$modelTerms) > 0)
      factors <- options$modelTerms
    return(.readDataSetToEnd(columns.as.factor = factors, 
                             columns.as.numeric = counts))
  }
}  

.regLogLinCheckErrors <- function(dataset, options, ready) {
  # Error Check 1
  if (length(options$factors) > 0) {
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
  }
  
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

# Compute results ----
.regLogLinComputeModel <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["Container"]][["Model"]])) 
    return(jaspResults[["Container"]][["Model"]]$object)
  if (!ready) 
    return()
  
  results <- list()
  modelTerms <- unlist(options$modelTerms)
  # Fit Loglinear Model
  if (options$counts == "")
    dataset <- plyr::count(dataset)
  else 
    dataset <- dataset
  
  loglm.model <- list()
  empty.model <- list(loglm.fit = NULL, variables = NULL)
  
  if (options$counts == "")
    dependent.variable <- "freq"
  else 
    dependent.variable <- unlist(options$counts)
  
  if (length(options$modelTerms) > 0) {
    
    variables.in.model <- NULL
    variables.in.model.base64 <- NULL
    
    for (i in seq_along(options$modelTerms)) {
      
      components <- options$modelTerms[[i]]$components
      
      if (length(components) == 1) {
        
        variables.in.model <- c(variables.in.model, components[[1]])
        variables.in.model.base64 <- c(variables.in.model.base64, 
                                       .v(components[[1]]))
        
      } else {
        
        components.unlisted <- unlist(components)
        term.base64 <- paste0(.v(components.unlisted), collapse = ":")
        term <- paste0(components.unlisted, collapse = ":")
        variables.in.model <- c(variables.in.model, term)
        variables.in.model.base64 <- c(variables.in.model.base64, term.base64)
      }
    }
    
    independent.base64 <- variables.in.model.base64
    variables.in.model <- variables.in.model[ variables.in.model != ""]
    variables.in.model.copy <- variables.in.model
  }
  
  dependent.base64 <- .v(dependent.variable)
  
  if (length(options$modelTerms) > 0) {
    if (length(variables.in.model) > 0 )
      model.definition <- paste(dependent.base64, "~", 
                                paste(independent.base64, collapse = "+"))
    else 
      model.definition <- NULL #this model has no parameters
    
    if (!is.null(model.definition)) {
      
      model.formula <- as.formula(model.definition)
      
      if (options$counts == "")
        names(dataset)[names(dataset) == "freq"] <- dependent.base64
      
      loglm.fit <- try( stats::glm( model.formula, 
                                    family = poisson(), 
                                    data = dataset), 
                        silent = TRUE)
      
      if ( class(loglm.fit) == "glm")
        loglm.model <- list(loglm.fit = loglm.fit, 
                            variables = variables.in.model)
    } else 
      loglm.model <- list(loglm.fit = NULL, 
                          variables = variables.in.model)
  } else 
    loglm.model <- empty.model
  
  results <- loglm.model
  jaspResults[["Container"]][["Model"]] <- createJaspState(results)
  
  return(results)
}

.regLogLinComputeAnovaResults <- function(jaspResults, dataset, options, 
                                          loglm.model, ready){
 
  if (!is.null(jaspResults[["Container"]][["AnovaResults"]])) 
    return(jaspResults[["Container"]][["AnovaResults"]]$object)
  
  results <- list()
  empty.line <- list( #for empty elements in tables when given output
    "name" = "",
    "df"   = "",
    "deviance"   = "",
    "residualDf" = "",
    "residualDeviance" = "",
    "p" = "")
  if (options$VovkSellkeMPR) {
    empty.line$VovkSellkeMPR <- ""
  }
  
  dotted.line <- list( #for empty tables
    "name" = ".",
    "df"   = ".",
    "deviance"   = ".",
    "residualDf" = ".",
    "residualDeviance" = ".",
    "p" = ".")
  if (options$VovkSellkeMPR)
    dotted.line$VovkSellkeMPR <- "."
  
  if ( class(loglm.model$loglm.fit) == "glm") {
    
    loglm.anova     <- anova(loglm.model$loglm.fit, test = "Chisq")
    loglm.estimates <- loglm.anova
    len.logreg      <- length(results) + 1
    
    v <- 0
    null.model <- "Null model"
    if (length(loglm.model$variables) > 0) {
      
      variables.in.model <- loglm.model$variables
      l <- dim(loglm.estimates)[1]
      name <- unlist(dimnames(loglm.estimates)[[1]])
      
      for (var in 1:l) {
        
        results[[ len.logreg ]] <- empty.line
        model.name <- .unvf(name)
        #may need to change \/
        model.name[[1]] <- " "
        
        if(var == 1){
          results[[ len.logreg ]]$name     <- "NULL"
          results[[ len.logreg ]]$df       <- " "
          results[[ len.logreg ]]$deviance <- " "
          results[[ len.logreg ]]$p        <- " "
          if (options$VovkSellkeMPR){
            results[[ len.logreg ]]$VovkSellkeMPR <- " "
          }
        } else {
          results[[ len.logreg ]]$name     <- model.name[var]
          results[[ len.logreg ]]$df       <- as.integer(loglm.estimates$Df[var])
          results[[ len.logreg ]]$deviance <- as.numeric(loglm.estimates$Deviance[var])
          pVal                             <- as.numeric(loglm.estimates$"Pr(>Chi)"[var])
          results[[ len.logreg ]]$p        <- pVal
          if (options$VovkSellkeMPR){
            results[[ len.logreg ]]$VovkSellkeMPR <- .VovkSellkeMPR(pVal)
          }
        }
        results[[ len.logreg ]]$residualDf <- as.integer(loglm.estimates$"Resid. Df"[var])
        res        <- as.numeric(loglm.estimates$"Resid. Dev"[var])
        if (abs(res) < 10^(-4))
          res      <- 0
        
        results[[ len.logreg ]]$residualDeviance <- res
        
        len.logreg <- len.logreg + 1
      }
    }
    
  } else {
    
    len.logreg <- length(results) + 1
    results[[ len.logreg ]] <- dotted.line
    
    if (length(loglm.model$variables) > 0) {
      
      variables.in.model <- loglm.model$variables
      
      len.logreg <- len.logreg + 1
      
      for (var in 1:length(variables.in.model)) {
        
        results[[ len.logreg ]] <- dotted.line
        
        if (base::grepl(":", variables.in.model[var])) {
          
          # if interaction term
          vars <- unlist(strsplit(variables.in.model[var], split = ":"))
          name <- paste0(vars, collapse = "\u2009\u273b\u2009")
          
        } else {
          name <- as.character(variables.in.model[var])
        }
        
        results[[ len.logreg ]] <- list(
          name             = name,
          df               = ".",
          deviance         = ".",
          residualDf       = ".",
          residualDeviance = ".",
          p                = "."
        )
        if(options$VovkSellkeMPR)
          results$VovkSellkeMPR <- "."
        len.logreg <- len.logreg + 1
      }
    }
  }
    
   
  # Save results to state
  jaspResults[["Container"]][["AnovaResults"]] <- createJaspState(results)
  jaspResults[["Container"]][["AnovaResults"]]$dependOn(c("modelTerms", 
                                                          "counts", 
                                                          "VovkSellkeMPR"))

  return(results)
}

.regLogLinComputeCoefficientsResults <- function(jaspResults, dataset, options, 
                                                 loglm.model, ready){
  if (!options$regressionCoefficientsEstimates)
    return()
  if (!is.null(jaspResults[["Container"]][["CoefficientResults"]])) 
    return(jaspResults[["Container"]][["CoefficientResults"]]$object)
  
  results <- list()
  empty.line <- list( #for empty elements in tables when given output
    "name" = "",
    "Coefficient"   = "",
    "StandardError" = "",
    "Z" = "",
    "p" = "")
  if(options$regressionCoefficientsConfidenceIntervals) {
    empty.line$lower <- ""
    empty.line$upper <- ""
  }
  if (options$VovkSellkeMPR) 
    empty.line$VovkSellkeMPR <- ""
  
  dotted.line <- list( #for empty tables
    "name" = ".",
    "Coefficient"   = ".",
    "StandardError" = ".",
    "Z" = ".",
    "p" = ".")
  if(options$regressionCoefficientsConfidenceIntervals) {
    dotted.line$lower <- "."
    dotted.line$upper <- "."
  }
  if (options$VovkSellkeMPR)
    dotted.line$VovkSellkeMPR <- "."
  
  lookup.table <- .regLogLinBuildLookup(dataset, options$factors)
  lookup.table[["(Intercept)"]] <- "(Intercept)"
  
  #logregression.result <- list()
  
  if ( class(loglm.model$loglm.fit) == "glm") {
      
    loglm.summary   <- summary(loglm.model$loglm.fit)
    loglm.estimates <- loglm.summary$coefficients
    #print(str(loglm.estimates))
    loglm.coeff <- loglm.estimates[,"Estimate"]
    loglm.estimates.SE <- loglm.estimates[,"Std. Error"]
    sig    <- options$regressionCoefficientsConfidenceIntervalsInterval
    alpha  <- (1 - sig) / 2
    lower  <- loglm.coeff+ stats::qnorm(alpha)*loglm.estimates.SE
    upper  <- loglm.coeff+ stats::qnorm(1-alpha)*loglm.estimates.SE
    
    len.logreg <- length(results) + 1
    
    if (length(loglm.model$variables) > 0) {
      
      variables.in.model <- loglm.model$variables
      coefficients <- dimnames(loglm.estimates)[[1]]
      coef <- base::strsplit (coefficients, split = ":", fixed = TRUE)
      
      for (i in seq_along(coef)) {
        
        #results[[ len.logreg ]] <- empty.line
        
        coefficient <- coef[[i]]
        
        actualName <- list()
        for (j in seq_along(coefficient)){
          actualName[[j]] <- paste(lookup.table[[ coefficient[j] ]], 
                                   collapse = " = ")
        }
        var <- paste0(actualName, collapse = "*")
        
        name <- var
        Coefficient <- as.numeric(unname(loglm.estimates[i,1]))
        sd <- as.numeric(loglm.estimates[i,2])
        
        if (options$regressionCoefficientsConfidenceIntervals){
          Lower <- as.numeric(lower[i])
          Upper <- as.numeric(upper[i])
        }
        
        Z <- as.numeric(loglm.estimates[i,3])
        p <- as.numeric(loglm.estimates[i,4])
        if (options$VovkSellkeMPR){
          VovkSellkeMPR <- .VovkSellkeMPR(p)
        }
        results[[len.logreg]] <- list(
          name          = name,
          Coefficient   = Coefficient,
          StandardError = sd
        )
        if(options$regressionCoefficientsConfidenceIntervals){
          results[[len.logreg]]$lower <- Lower
          results[[len.logreg]]$upper <- Upper
        }
        results[[len.logreg]]$Z <- Z
        results[[len.logreg]]$p <- p
        if(options$VovkSellkeMPR)
          results[[len.logreg]]$VovkSellkeMPR <- VovkSellkeMPR
        
        len.logreg <- len.logreg + 1
      }
    }
    
  } else {
    
    len.logreg <- length(results) + 1
    results[[ len.logreg ]]         <- dotted.line
    #results[[ len.logreg ]]$"Model" <- as.integer(m)
    
    if (length(loglm.model$variables) > 0) {
      variables.in.model <- loglm.model$variables
      len.logreg <- len.logreg + 1
      
      for (var in 1:length(variables.in.model)) {
        
        if (base::grepl(":", variables.in.model[var])) {
          
          # if interaction term
          vars <- unlist(strsplit(variables.in.model[var], 
                                  split = ":"))
          name <- paste0(vars, collapse = "\u2009\u273b\u2009")
          
        } else
          name <- as.character(variables.in.model[var])
        
        results[[len.logreg]] <- list(
          name          = name,
          Coefficient   = ".",
          StandardError = "."
        )
        if(options$regressionCoefficientsConfidenceIntervals){
          results[[len.logreg]]$lower <- "."
          results[[len.logreg]]$upper <- "."
        }
        results[[len.logreg]]$Z <- "."
        results[[len.logreg]]$p <- "."
        if(options$VovkSellkeMPR)
          results[[len.logreg]]$VovkSellkeMPR <- "."
        len.logreg <- len.logreg + 1
      }
    }
  }
  theList <- c("regressionCoefficientsEstimates", 
               "regressionCoefficientsConfidenceIntervals",
               "regressionCoefficientsConfidenceIntervalsInterval")
  jaspResults[["Container"]][["CoefficientResults"]] <- createJaspState(results)
  jaspResults[["Container"]][["CoefficientResults"]]$dependOn(theList)
  return(results)
}

# jaspContainer ----
.regLogLinContainer <- function(jaspResults, dataset, options, model, ready) {
  if (is.null(jaspResults[["Container"]])) {
    jaspResults[["Container"]] <- createJaspContainer("Log-Linear Regression")
    jaspResults[["Container"]]$dependOn(c("counts", "modelTerms", "VovkSellkeMPR"))
  }
}

# Output Tables ----
.regLogLinAnovaTable <- function(jaspResults, dataset, options, model, ready) {
  if (!is.null(jaspResults[["Container"]][["AnovaTable"]])) 
    return()
  
  # Create table
  anovaTable <- createJaspTable(title = "ANOVA")
  anovaTable$dependOn(optionsFromObject = 
                      jaspResults[["Container"]][["AnovaResults"]])
  .regLogLinCitation(anovaTable)
  anovaTable$showSpecifiedColumnsOnly <- TRUE
  anovaTable$position <- 1
  
  # Add columns to table
  anovaTable$addColumnInfo(name = "name",             title = "",                   
                           type = "string")
  anovaTable$addColumnInfo(name = "df",               title = "df",                 
                           type = "integer")
  anovaTable$addColumnInfo(name = "deviance",         title = "Deviance",           
                           type = "number", format = "sf:4;dp:3")
  anovaTable$addColumnInfo(name = "residualDf",       title = "Residual df",        
                           type = "integer")
  anovaTable$addColumnInfo(name = "residualDeviance", title = "Residual Deviance",  
                           type = "number", format = "sf:4;dp:3")
  anovaTable$addColumnInfo(name = "p",                title = "p",                  
                           type = "number", format = "dp:3;p:.001")
  if (options$VovkSellkeMPR) {
    anovaTable$addColumnInfo(name = "VovkSellkeMPR",  title = "VS-MPR\u002A",       
                             type = "number", format = "sf:4;dp:3")
    # Footnote
    message <- ("Vovk-Sellke Maximum <em>p</em>-Ratio: Based the <em>p</em>-value, 
                 the maximum possible odds in favor of H\u2081 over H\u2080 equals 
                 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37 
                 (Sellke, Bayarri, & Berger, 2001).")
    anovaTable$addFootnote(message, symbol = "\u002A") 
  }
  
  jaspResults[["Container"]][["AnovaTable"]] <- anovaTable

  .regLogLinReturnOrFill(jaspResults, dataset, options, model, ready, 
                         .regLogLinComputeAnovaResults, anovaTable)
}

.regLogLinCoefficientsTable <- function(jaspResults, dataset, options, model, ready) {
  
  if (!is.null(jaspResults[["Container"]][["CoefficientsTable"]])) 
    return()
  if (!options$regressionCoefficientsEstimates) 
    return()
  
  # Create table
  coefficientsTable <- createJaspTable(title = "Coefficients")
  coefficientsTable$dependOn(optionsFromObject = 
                             jaspResults[["Container"]][["CoefficientResults"]])
  .regLogLinCitation(coefficientsTable)
  coefficientsTable$showSpecifiedColumnsOnly <- TRUE
  coefficientsTable$position <- 2
  
  # Add columns to table
  coefficientsTable$addColumnInfo(name = "name",          title = "", 
                                  type = "string")
  coefficientsTable$addColumnInfo(name = "Coefficient",   title = "Estimate",       
                                  type = "number", format = "dp:3")
  coefficientsTable$addColumnInfo(name = "StandardError", title = "Standard Error", 
                                  type = "number", format = "dp:3")
  if(options$regressionCoefficientsConfidenceIntervals){
    confIntVal <- options$regressionCoefficientsConfidenceIntervalsInterval
    overTitle <- paste0(100*confIntVal, "% Confidence Intervals")
    coefficientsTable$addColumnInfo(name = "lower", title = "Lower", type = "number", 
                                    format = "sf:4;dp:3", overtitle = overTitle)
    coefficientsTable$addColumnInfo(name = "upper", title = "Upper", type = "number", 
                                    format = "sf:4;dp:3", overtitle = overTitle)
  }
  coefficientsTable$addColumnInfo(name = "Z",  type = "number", format = "sf:4;dp:3")
  coefficientsTable$addColumnInfo(name = "p",  type = "number", format = "dp:3;p:.001")
  if (options$VovkSellkeMPR)
    coefficientsTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", 
                                    type = "number", format = "sf:4;dp:3")
  
  jaspResults[["Container"]][["CoefficientsTable"]] <- coefficientsTable
  
  .regLogLinReturnOrFill(jaspResults, dataset, options, model, ready, 
                         .regLogLinComputeCoefficientsResults, coefficientsTable)
}

.regLogLinReturnOrFill <- function(jaspResults, dataset, options, 
                                   model, ready, .func, table){
  if (!ready) 
    return()
  res <- try(.func(jaspResults, dataset, options, model, ready))
  if(isTryError(res))
    table$setError(res)
  else {
    for (level in 1:length(res)) {
      row <- res[[level]]
      table$addRows(row)
    }
  }
}

.regLogLinCitation <- function(table){
  citation <- ("R Core Team (2015). R: A language and environment for 
  statistical computing. 
  R Foundation for Statistical Computing, Vienna, Austria. 
  URL https://www.R-project.org/.")
  table$addCitation(citation)
}

# Other Results setup ----
.regLogLinBuildLookup <- function(dataset, factors) {
  table <- list()
  for (v in factors) {
    levels <- base::levels(dataset[[ .v(v) ]])
    for (l in levels) {
      mangled.name <- paste(.v(v), l, sep = "")
      actual       <- c(v, l)
      table[[mangled.name]] <- actual
    }
  }
  return(table)
}
