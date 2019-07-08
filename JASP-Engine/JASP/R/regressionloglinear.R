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
  dataset <- .regressionLogLinearReadData(dataset, options)
  
  # Error checking
  errors <- .regressionLogLinearCheckErrors(dataset, options)
  
  # Compute the results
  regressionLogLinearResults <- .regressionLogLinearComputeResults(jaspResults, dataset, options, errors)
  
  # Output tables
  .regressionLogLinearContainer(         jaspResults, dataset, options, regressionLogLinearResults, errors)
  .regressionLogLinearAnovaTable(        jaspResults, dataset, options, regressionLogLinearResults, errors)
  .regressionLogLinearCoefficientsTable (jaspResults, dataset, options, regressionLogLinearResults, errors)
  
  return()
}

# Preprocessing functions ----
.regressionLogLinearReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    counts <- NULL
    factors <- NULL
    if(options$counts != "")
      counts <- options$counts
    if(length(options$modelTerms) > 0)
      factors <- options$modelTerms
    return(.readDataSetToEnd(columns.as.factor = factors, columns.as.numeric = counts))
  }
}  

.regressionLogLinearCheckErrors <- function(dataset, options) {
  
  # Check if results can be computed
  if (length(options$factors) == 0) return("No factors")
  
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
.regressionLogLinearComputeResults <- function(jaspResults, dataset, options, errors) {
  
  if (!is.null(errors) && errors == "No factors") 
    return()
  
  # Take results from state if possible
  if (!is.null(jaspResults[["stateLogLinearResults"]])) 
    return(jaspResults[["stateLogLinearResults"]]$object)
  
  results <- list()  
  results[["Coefficients"]] <- list()
  results[["ANOVA"]] <- list()
  modelTerms <- unlist(options$modelTerms)
  
  list.of.errors <- list()
  error.message  <- NULL
  
  ########################################
  ###       LOGLINEAR REGRESSION       ###
  ########################################
  # Fit Loglinear Model
  
  if (options$counts == ""){
    dataset <- plyr::count(dataset)
  } else {
    dataset <- dataset
  }
  
  loglm.model <- list()
  empty.model <- list(loglm.fit = NULL, variables = NULL)
  
  if (options$counts == ""){
    dependent.variable <- "freq"
  } else {
    dependent.variable <- unlist(options$counts)
  }
  
  if (length(options$modelTerms) > 0) {
    
    variables.in.model <- NULL
    variables.in.model.base64 <- NULL
    
    for (i in seq_along(options$modelTerms)) {
      
      components <- options$modelTerms[[i]]$components
      
      if (length(components) == 1) {
        
        variables.in.model <- c(variables.in.model, components[[1]])
        variables.in.model.base64 <- c(variables.in.model.base64, .v(components[[1]]))
        
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
  
  #print (dependent.base64)
  
  if (length(options$modelTerms) > 0) {
    
    if (length(variables.in.model) > 0 ) {
      
      model.definition <- paste(dependent.base64, "~", paste(independent.base64, collapse = "+"))
      
    }  else {
      
      model.definition <- NULL #this model has no parameters
    }
    
    if (!is.null(model.definition) && length(list.of.errors) == 0 ) {
      
      model.formula <- as.formula(model.definition)
      
      if (options$counts == ""){
        names(dataset)[names(dataset) == "freq"] <- dependent.base64
      }
      
      loglm.fit <- try( stats::glm( model.formula, family = poisson(), data = dataset), silent = TRUE)
      
      if ( class(loglm.fit) == "glm") {
        
        loglm.model <- list(loglm.fit = loglm.fit, variables = variables.in.model)
        
      } else {
        if (inherits(loglm.fit, "try-error")) {
          error <- .extractErrorMessage (loglm.fit)}
        list.of.errors[[ length(list.of.errors) + 1 ]] <- error
        loglm.model <- list(loglm.fit = NULL, variables = variables.in.model)
      }
      
    } else {
      
      loglm.model <- list(loglm.fit = NULL, variables = variables.in.model)
    }
    
  } else {
    loglm.model <- empty.model
  }
  
  ########################################
  ###           ANOVA RESULTS          ###
  ########################################
  
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
  if (options$VovkSellkeMPR) {
    dotted.line$VovkSellkeMPR <- "."
  }
  
  if (length(list.of.errors) == 0 ) {
    
    if ( class(loglm.model$loglm.fit) == "glm") {
      
      loglm.anova     <- anova(loglm.model$loglm.fit, test = "Chisq")
      loglm.estimates <- loglm.anova
      len.logreg      <- length(results[["ANOVA"]]) + 1
      
      v <- 0
      null.model <- "Null model"
      if (length(loglm.model$variables) > 0) {
        
        variables.in.model <- loglm.model$variables
        l <- dim(loglm.estimates)[1]
        name <- unlist(dimnames(loglm.estimates)[[1]])
        
        for (var in 1:l) {
          
          results[["ANOVA"]][[ len.logreg ]] <- empty.line
          model.name <- .unvf(name)
          #may need to change \/
          model.name[[1]] <- " "
         
          if(var == 1){
            results[["ANOVA"]][[ len.logreg ]]$name     <- "NULL"
            results[["ANOVA"]][[ len.logreg ]]$df       <- " "
            results[["ANOVA"]][[ len.logreg ]]$deviance <- " "
            results[["ANOVA"]][[ len.logreg ]]$p        <- " "
            if (options$VovkSellkeMPR){
              results[["ANOVA"]][[ len.logreg ]]$VovkSellkeMPR <- " "
            }
          } else {
            results[["ANOVA"]][[ len.logreg ]]$name     <- model.name[var]
            results[["ANOVA"]][[ len.logreg ]]$df       <- as.integer(loglm.estimates$Df[var])
            results[["ANOVA"]][[ len.logreg ]]$deviance <- as.numeric(loglm.estimates$Deviance[var])
            results[["ANOVA"]][[ len.logreg ]]$p        <- as.numeric(loglm.estimates$"Pr(>Chi)"[var])
            if (options$VovkSellkeMPR){
              results[["ANOVA"]][[ len.logreg ]]$VovkSellkeMPR <- .VovkSellkeMPR(results[["ANOVA"]][[ len.logreg ]]$p)
            }
          }
          results[["ANOVA"]][[ len.logreg ]]$residualDf <- as.integer(loglm.estimates$"Resid. Df"[var])
          res        <- as.numeric(loglm.estimates$"Resid. Dev"[var])
          if (abs(res) < 10^(-4))
            res      <- 0
          
          results[["ANOVA"]][[ len.logreg ]]$residualDeviance <- res
          
          len.logreg <- len.logreg + 1
        }
      }
      
    } else {
      
      len.logreg <- length(results[["ANOVA"]]) + 1
      results[["ANOVA"]][[ len.logreg ]] <- dotted.line
      
      if (length(loglm.model$variables) > 0) {
        
        variables.in.model <- loglm.model$variables
        
        len.logreg <- len.logreg + 1
        
        for (var in 1:length(variables.in.model)) {
          
          results[["ANOVA"]][[ len.logreg ]] <- dotted.line
          
          if (base::grepl(":", variables.in.model[var])) {
            
            # if interaction term
            vars <- unlist(strsplit(variables.in.model[var], split = ":"))
            name <- paste0(vars, collapse = "\u2009\u273b\u2009")
            
          } else {
            name <- as.character(variables.in.model[var])
          }
          
          results[["ANOVA"]][[ len.logreg ]] <- list(
            name             = name,
            df               = ".",
            deviance         = ".",
            residualDf       = ".",
            residualDeviance = ".",
            p                = "."
          )
          if(options$VovkSellkeMPR)
            results[["ANOVA"]]$VovkSellkeMPR <- "."
          len.logreg <- len.logreg + 1
        }
      }
    }
    
  } else {
    
    len.logreg <- length(results[["ANOVA"]]) + 1
    
    if (length(loglm.model$variables) > 0) {
      
      variables.in.model <- loglm.model$variables
      
    }
    
    len.logreg <- length(results[["ANOVA"]]) + 1
    results[["ANOVA"]][[ len.logreg ]] <- dotted.line
    results[["ANOVA"]][[ len.logreg ]]$"Model" <- 1
  }
  
  if (length(list.of.errors) > 1){
    
    loglm.fit <- try( stats::glm( model.formula, family = poisson(), data = dataset), silent = TRUE)
    
    if (isTryError(loglm.fit)) {
      error <- .extractErrorMessage (loglm.fit)
    }
    results[["ANOVA"]][["error"]] <- list(errorType = "badData", errorMessage = error)
    
  } else if (length(list.of.errors) == 1){
    
    results[["ANOVA"]][["error"]] <- list(errorType = "badData", errorMessage = list.of.errors[[ 1 ]])
  }
  
  ########################################
  ###    MODEL COEFFICIENTS RESULTS    ###
  ########################################
  
  if (options$regressionCoefficientsEstimates == TRUE) {
    
    
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
    if (options$VovkSellkeMPR) {
      empty.line$VovkSellkeMPR <- ""
    }
    
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
    if (options$VovkSellkeMPR) {
      dotted.line$VovkSellkeMPR <- "."
    }
    
    lookup.table <- .regressionLogLinearBuildLookup(dataset, options$factors)
    lookup.table[["(Intercept)"]] <- "(Intercept)"
    
    #logregression.result <- list()
    
    if (length(list.of.errors) == 0) {
      
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
        
        len.logreg <- length(results[["Coefficients"]]) + 1
        
        if (length(loglm.model$variables) > 0) {
          
          variables.in.model <- loglm.model$variables
          coefficients <- dimnames(loglm.estimates)[[1]]
          coef <- base::strsplit (coefficients, split = ":", fixed = TRUE)
          
          for (i in seq_along(coef)) {
            
            #results[["Coefficients"]][[ len.logreg ]] <- empty.line
            
            coefficient <- coef[[i]]
            
            actualName <- list()
            for (j in seq_along(coefficient)){
              actualName[[j]] <- paste(lookup.table[[ coefficient[j] ]], collapse = " = ")
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
            results[["Coefficients"]][[len.logreg]] <- list(
              name          = name,
              Coefficient   = Coefficient,
              StandardError = sd
            )
            if(options$regressionCoefficientsConfidenceIntervals){
              results[["Coefficients"]][[len.logreg]]$lower <- Lower
              results[["Coefficients"]][[len.logreg]]$upper <- Upper
            }
            results[["Coefficients"]][[len.logreg]]$Z <- Z
            results[["Coefficients"]][[len.logreg]]$p <- p
            if(options$VovkSellkeMPR)
              results[["Coefficients"]][[len.logreg]]$VovkSellkeMPR <- VovkSellkeMPR
            
            len.logreg <- len.logreg + 1
          }
        }
        
      } else {
        
        len.logreg <- length(results[["Coefficients"]]) + 1
        results[["Coefficients"]][[ len.logreg ]]         <- dotted.line
        results[["Coefficients"]][[ len.logreg ]]$"Model" <- as.integer(m)
        
        if (length(loglm.model$variables) > 0) {
          
          variables.in.model <- loglm.model$variables
          
          len.logreg <- len.logreg + 1
          
          for (var in 1:length(variables.in.model)) {
            
            if (base::grepl(":", variables.in.model[var])) {
              
              # if interaction term
              vars <- unlist(strsplit(variables.in.model[var], split = ":"))
              name <- paste0(vars, collapse = "\u2009\u273b\u2009")
              
            } else {
              
              name <- as.character(variables.in.model[var])
            }
            
            results[["Coefficients"]][[len.logreg]] <- list(
              name          = name,
              Coefficient   = ".",
              StandardError = "."
            )
            if(options$regressionCoefficientsConfidenceIntervals){
              results[["Coefficients"]][[len.logreg]]$lower <- "."
              results[["Coefficients"]][[len.logreg]]$upper <- "."
            }
            results[["Coefficients"]][[len.logreg]]$Z <- "."
            results[["Coefficients"]][[len.logreg]]$p <- "."
            if(options$VovkSellkeMPR)
              results[["Coefficients"]][[len.logreg]]$VovkSellkeMPR <- "."
            len.logreg <- len.logreg + 1
          }
        }
      }
      
    } else {
      
      len.logreg <- length(results[["Coefficients"]]) + 1
      
      if (length(loglm.model$variables) > 0) {
        
        variables.in.model <- loglm.model$variables
        
      }
      
      len.logreg <- length(results[["Coefficients"]]) + 1
      results[["Coefficients"]][[ len.logreg ]] <- dotted.line
      results[["Coefficients"]][[ len.logreg ]]$"Model" <- 1
      
    }
    
    if (length(list.of.errors) > 1){
      loglm.fit <- try( stats::glm( model.formula, family = poisson(), data = dataset), silent = TRUE)
      
      if (isTryError(loglm.fit)) {
        error <- .extractErrorMessage (loglm.fit)}
      results[["Coefficients"]][["error"]] <- list(errorType = "badData", errorMessage = error)
      
    } else if (length(list.of.errors) == 1){
      results[["Coefficients"]][["error"]] <- list(errorType = "badData", errorMessage = list.of.errors[[ 1 ]])
    }
    
  }
  
  ########################################
  
  # Save results to state
  jaspResults[["stateLogLinearResults"]] <- createJaspState(results)
  jaspResults[["stateLogLinearResults"]]$dependOn(c("modelTerms", "counts", "VovkSellkeMPR", 
                                                    "regressionCoefficientsEstimates", 
                                                    "regressionCoefficientsConfidenceIntervals",
                                                    "regressionCoefficientsConfidenceIntervalsInterval"))
  
  # Return results object
  return(results)
}

# jaspContainer ----
.regressionLogLinearContainer <- function(jaspResults, dataset, options, regressionLogLinearResults, errors) {
  if (is.null(jaspResults[["regressionLogLinearContainer"]])) {
    jaspResults[["regressionLogLinearContainer"]] <- createJaspContainer("Log-Linear Regression")
    jaspResults[["regressionLogLinearContainer"]]$dependOn(c("counts", "modelTerms", "VovkSellkeMPR"))
  }
}

# Output Tables ----
.regressionLogLinearAnovaTable <- function(jaspResults, dataset, options, regressionLogLinearResults, errors) {
  if (!is.null(jaspResults[["regressionLogLinearContainer"]][["AnovaTable"]])) 
    return()
  
  # Create table
  regressionLogLinearAnovaTable <- createJaspTable(title = "ANOVA")
  regressionLogLinearAnovaTable$dependOn(c("counts", "modelTerms", "VovkSellkeMPR"))
  citation <- "R Core Team (2015). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/."
  regressionLogLinearAnovaTable$addCitation(citation)
  regressionLogLinearAnovaTable$showSpecifiedColumnsOnly <- TRUE
  regressionLogLinearAnovaTable$position <- 1
  
  # Add columns to table
  regressionLogLinearAnovaTable$addColumnInfo(name = "name",             title = "",                   type = "string")
  regressionLogLinearAnovaTable$addColumnInfo(name = "df",               title = "df",                 type = "integer")
  regressionLogLinearAnovaTable$addColumnInfo(name = "deviance",         title = "Deviance",           type = "number", format = "sf:4;dp:3")
  regressionLogLinearAnovaTable$addColumnInfo(name = "residualDf",       title = "Residual df",        type = "integer")
  regressionLogLinearAnovaTable$addColumnInfo(name = "residualDeviance", title = "Residual Deviance",  type = "number", format = "sf:4;dp:3")
  regressionLogLinearAnovaTable$addColumnInfo(name = "p",                title = "p",                  type = "number", format = "dp:3;p:.001")
  if (options$VovkSellkeMPR) {
    regressionLogLinearAnovaTable$addColumnInfo(name = "VovkSellkeMPR",  title = "VS-MPR\u002A",       type = "number", format = "sf:4;dp:3")
    # Footnote
    message <- ("Vovk-Sellke Maximum <em>p</em>-Ratio: Based the <em>p</em>-value, 
                 the maximum possible odds in favor of H\u2081 over H\u2080 equals 
                 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37 
                 (Sellke, Bayarri, & Berger, 2001).")
    regressionLogLinearAnovaTable$addFootnote(message, symbol = "\u002A") 
  }
  
  jaspResults[["regressionLogLinearContainer"]][["AnovaTable"]] <- regressionLogLinearAnovaTable
  
  if (!is.null(errors) && errors == "No factors") 
    return()
  
  for (lev in 1:length(regressionLogLinearResults[["ANOVA"]])) {
    row <- regressionLogLinearResults[["ANOVA"]][[lev]][1:7]
    regressionLogLinearAnovaTable$addRows(row)
  }
}

.regressionLogLinearCoefficientsTable <- function(jaspResults, dataset, options, regressionLogLinearResults, errors) {
  
  if (!is.null(jaspResults[["regressionLogLinearContainer"]][["CoefficientsTable"]])) 
    return()
  
  if (options$regressionCoefficientsEstimates == FALSE) 
    return()
  
  # Create table
  regressionLogLinearCoefficientsTable <- createJaspTable(title = "Coefficients")
  regressionLogLinearCoefficientsTable$dependOn(c("counts", "modelTerms", "VovkSellkeMPR", 
                                                  "regressionCoefficientsEstimates", 
                                                  "regressionCoefficientsConfidenceIntervals", 
                                                  "regressionCoefficientsConfidenceIntervalsInterval"))
  citation <- ("R Core Team (2015). R: A language and environment for statistical computing. 
                R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.")
  regressionLogLinearCoefficientsTable$addCitation(citation)
  regressionLogLinearCoefficientsTable$showSpecifiedColumnsOnly <- TRUE
  regressionLogLinearCoefficientsTable$position <- 2
  
  # Add columns to table
  regressionLogLinearCoefficientsTable$addColumnInfo(name = "name",          title = "",               type = "string")
  regressionLogLinearCoefficientsTable$addColumnInfo(name = "Coefficient",   title = "Estimate",       type = "number", format = "dp:3")
  regressionLogLinearCoefficientsTable$addColumnInfo(name = "StandardError", title = "Standard Error", type = "number", format = "dp:3")
  if(options$regressionCoefficientsConfidenceIntervals){
    overTitle <- paste0(100*options$regressionCoefficientsConfidenceIntervalsInterval, "% Confidence Intervals")
    regressionLogLinearCoefficientsTable$addColumnInfo(name = "lower", title = "Lower", type = "number", format = "sf:4;dp:3", overtitle = overTitle)
    regressionLogLinearCoefficientsTable$addColumnInfo(name = "upper", title = "Upper", type = "number", format = "sf:4;dp:3", overtitle = overTitle)
  }
  regressionLogLinearCoefficientsTable$addColumnInfo(name = "Z",  type = "number", format = "sf:4;dp:3")
  regressionLogLinearCoefficientsTable$addColumnInfo(name = "p",  type = "number", format = "dp:3;p:.001")
  if (options$VovkSellkeMPR)
    regressionLogLinearCoefficientsTable$addColumnInfo(name = "VovkSellkeMPR", title = "VS-MPR\u002A", type = "number", format = "sf:4;dp:3")
  
  jaspResults[["regressionLogLinearContainer"]][["CoefficientsTable"]] <- regressionLogLinearCoefficientsTable
  
  if (!is.null(errors) && errors == "No factors") 
    return()
  
  for (lev in  1:length(regressionLogLinearResults[["Coefficients"]])) {
    row <- regressionLogLinearResults[["Coefficients"]][[lev]]
    regressionLogLinearCoefficientsTable$addRows(row)
  }
  
}

# Other Results setup ----
.regressionLogLinearBuildLookup <- function(dataset, factors) {
  
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
