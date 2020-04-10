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

RegressionLogLinear <- function(jaspResults, dataset = NULL , options, ...) {
  ready <- length(options$factors) != 0
  
  if (ready) {
    dataset <- .regLogLinReadData(dataset, options)
    .regLogLinCheckErrors(dataset, options)
  }
  # Output tables
  .regLogLinAnovaTable(        jaspResults, dataset, options, ready)
  .regLogLinCoefficientsTable (jaspResults, dataset, options, ready)

  return()
}

# Preprocessing functions ----
.regLogLinReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else {
    counts <- factors <- NULL
    if(options$counts != "")
      counts <- options$counts
    if(length(options$modelTerms) > 0)
      factors <- options$modelTerms
    return(.readDataSetToEnd(columns.as.factor = factors, 
                             columns.as.numeric = counts))
  }
}  

.regLogLinCheckErrors <- function(dataset, options) {
  # Error Check 1: missingvalues, modelInteractions and infinity/missing check for counts
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

# Compute results ----
.regLogLinComputeModel <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["Model"]])) 
    return(jaspResults[["Model"]]$object)
  
  modelTerms <- unlist(options$modelTerms)
  if (options$counts == "")
    dataset <- plyr::count(dataset)
  
  loglm.model <- list()
  empty.model <- list(loglm.fit = NULL, variables = NULL)
  
  if (options$counts == "")
    dependent.variable <- "freq"
  else 
    dependent.variable <- unlist(options$counts)
  
  if (length(options$modelTerms) > 0) {
    
    variables.in.model <- variables.in.model.base64 <- NULL
    
    for (i in seq_along(options$modelTerms)) {
      
      components <- options$modelTerms[[i]]$components
      
      if (length(components) == 1) {
        variables.in.model        <- c(variables.in.model, components[[1]])
        variables.in.model.base64 <- c(variables.in.model.base64, .v(components[[1]]))
      } else {
        components.unlisted <- unlist(components)
        term.base64         <- paste0(.v(components.unlisted), collapse = ":")
        term                <- paste0(components.unlisted, collapse = ":")
        variables.in.model  <- c(variables.in.model, term)
        variables.in.model.base64 <- c(variables.in.model.base64, term.base64)
      }
    }
    
    dependent.base64        <- .v(dependent.variable)
    independent.base64      <- variables.in.model.base64
    variables.in.model      <- variables.in.model[ variables.in.model != ""]
    variables.in.model.copy <- variables.in.model
    
    if (length(variables.in.model) > 0 )
      model.definition <- paste(dependent.base64, "~", 
                                paste(independent.base64, collapse = "+"))
    else 
      model.definition <- NULL #this model has no parameters
    
    if (!is.null(model.definition)) {
      
      model.formula <- as.formula(model.definition)
      
      if (options$counts == "")
        names(dataset)[names(dataset) == "freq"] <- dependent.base64
      
      loglm.fit <- try(stats::glm(model.formula, family = poisson(), 
                                  data = dataset), silent = TRUE)
      
      if (inherits(loglm.fit, "glm"))
        loglm.model <- list(loglm.fit = loglm.fit, 
                            variables = variables.in.model)
    } else 
      loglm.model <- list(loglm.fit = NULL, 
                          variables = variables.in.model)
  } else 
    loglm.model <- empty.model
  
  jaspResults[["Model"]] <- createJaspState(loglm.model)
  jaspResults[["Model"]]$dependOn(c("counts","modelTerms"))
  
  return(loglm.model)
}

.regLogLinAnovaResults <- function(jaspResults, dataset, options, loglm.model){
  results <- list()

  # Compute/get the Model
  loglm.model <- .regLogLinComputeModel(jaspResults, dataset, options)
  
  if (inherits(loglm.model$loglm.fit, "glm")) {
    
    loglm.anova     <- anova(loglm.model$loglm.fit, test = "Chisq")
    loglm.estimates <- loglm.anova
    len.logreg      <- length(results) + 1
    
    if (length(loglm.model$variables) > 0) {
      
      variables.in.model <- loglm.model$variables
      l    <- dim(loglm.estimates)[1]
      name <- unlist(dimnames(loglm.estimates)[[1]])
      
      for (var in 1:l) {
        results[[ len.logreg ]] <- .regloglinAnovaLine(options, char = "",  name = "") 
        model.name <- .unvf(name)
        
        if(var == 1){
          results[[ len.logreg ]]$name <- "NULL"
          results[[ len.logreg ]]$df   <- " "
          results[[ len.logreg ]]$dev  <- " "
          results[[ len.logreg ]]$p    <- " "
          if (options$VovkSellkeMPR)
            results[[ len.logreg ]]$VovkSellkeMPR <- " "
        } else {
          results[[ len.logreg ]]$name <- model.name[var]
          results[[ len.logreg ]]$df   <- as.integer(loglm.estimates$Df[var])
          results[[ len.logreg ]]$dev  <- as.numeric(loglm.estimates$Deviance[var])
          pVal                         <- as.numeric(loglm.estimates$"Pr(>Chi)"[var])
          results[[ len.logreg ]]$p    <- pVal
          if (options$VovkSellkeMPR)
            results[[ len.logreg ]]$VovkSellkeMPR <- .VovkSellkeMPR(pVal)
        }
        results[[ len.logreg ]]$resDf <- as.integer(loglm.estimates$"Resid. Df"[var])
        res <- as.numeric(loglm.estimates$"Resid. Dev"[var])
        if (abs(res) < 10^(-4))
          res <- 0
        results[[ len.logreg ]]$resDev <- res

        len.logreg <- len.logreg + 1
      }
    }
  } 
  
  jaspResults[["AnovaTable"]]$addRows(results)
}

.regLogLinCoefficientsResults <- function(jaspResults, dataset, options, loglm.model){
  results <- list()

  lookup.table <- .regLogLinBuildLookup(dataset, options$factors)
  lookup.table[["(Intercept)"]] <- "(Intercept)"
  
  if (inherits(loglm.model$loglm.fit, "glm")) {
    
    loglm.summary      <- summary(loglm.model$loglm.fit)
    loglm.estimates    <- loglm.summary$coefficients
    loglm.coeff        <- loglm.estimates[,"Estimate"]
    loglm.estimates.SE <- loglm.estimates[,"Std. Error"]
    sig    <- options$regressionCoefficientsConfidenceIntervalsInterval
    alpha  <- (1 - sig) / 2
    lower  <- loglm.coeff + stats::qnorm(alpha)*loglm.estimates.SE
    upper  <- loglm.coeff + stats::qnorm(1-alpha)*loglm.estimates.SE
    
    len.logreg <- length(results) + 1
    
    if (length(loglm.model$variables) > 0) {
      
      variables.in.model <- loglm.model$variables
      coefficients <- dimnames(loglm.estimates)[[1]]
      coef <- strsplit(coefficients, split = ":", fixed = TRUE)
      
      for (i in seq_along(coef)) {
        
        coefficient <- coef[[i]]
        
        actualName <- list()
        for (j in seq_along(coefficient))
          actualName[[j]] <- paste(lookup.table[[ coefficient[j] ]], collapse = " = ")
        var <- paste0(actualName, collapse = "*")
        
        name <- var
        Coefficient <- as.numeric(unname(loglm.estimates[i,1]))
        sd <- as.numeric(loglm.estimates[i,2])
        
        Z <- as.numeric(loglm.estimates[i,3])
        p <- as.numeric(loglm.estimates[i,4])
        if (options$VovkSellkeMPR)
          VovkSellkeMPR <- .VovkSellkeMPR(p)
        results[[len.logreg]] <- list(
          name  = name,
          Coeff = Coefficient,
          SE    = sd
        )
        if(options$regressionCoefficientsConfidenceIntervals){
          results[[len.logreg]]$Lower <- as.numeric(lower[i])
          results[[len.logreg]]$Upper <- as.numeric(upper[i])
        }
        results[[len.logreg]]$Z <- Z
        results[[len.logreg]]$p <- p
        if(options$VovkSellkeMPR)
          results[[len.logreg]]$VovkSellkeMPR <- VovkSellkeMPR
        
        len.logreg <- len.logreg + 1
      }
    }
  } 
  jaspResults[["CoefficientsTable"]]$addRows(results)
}

# Output Tables ----
.regLogLinAnovaTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["AnovaTable"]])) 
    return()
  
  # Create table
  anovaTable <- createJaspTable(title = gettext("ANOVA"))
  anovaTable$dependOn(c("counts", "modelTerms", "VovkSellkeMPR"))
  .regLogLinCitation(anovaTable)
  anovaTable$showSpecifiedColumnsOnly <- TRUE
  anovaTable$position <- 1
  
  # Add columns to table
  anovaTable$addColumnInfo(name = "name",   title = "",                           type = "string")
  anovaTable$addColumnInfo(name = "df",     title = gettext("df"),                type = "integer")
  anovaTable$addColumnInfo(name = "dev",    title = gettext("Deviance"),          type = "number")
  anovaTable$addColumnInfo(name = "resDf",  title = gettext("Residual df"),       type = "integer")
  anovaTable$addColumnInfo(name = "resDev", title = gettext("Residual Deviance"), type = "number")
  anovaTable$addColumnInfo(name = "p",      title = gettext("p"),                 type  = "pvalue")
  if (options$VovkSellkeMPR)
    .regLogLinAddVovkSellke(anovaTable)
  
  jaspResults[["AnovaTable"]] <- anovaTable
  
  if (!ready) 
    return()
  
  # Compute/get the Model
  loglm.model  <- .regLogLinComputeModel(jaspResults, dataset, options)
  
  res <- try(.regLogLinAnovaResults(jaspResults, dataset, options, loglm.model))
  .regLogLinSetError(res, anovaTable)
}

.regLogLinCoefficientsTable <- function(jaspResults, dataset, options, ready) {
  if (!options$regressionCoefficientsEstimates || 
      !is.null(jaspResults[["CoefficientsTable"]])) 
    return()
  
  # Create table
  coefficientsTable <- createJaspTable(title = gettext("Coefficients"))
  dependList <- c("counts", "modelTerms", "regressionCoefficientsEstimates", 
                  "VovkSellkeMPR", "regressionCoefficientsConfidenceIntervals",
                  "regressionCoefficientsConfidenceIntervalsInterval")
  coefficientsTable$dependOn(dependList)
  .regLogLinCitation(coefficientsTable)
  coefficientsTable$showSpecifiedColumnsOnly <- TRUE
  coefficientsTable$position <- 2
  
  # Add columns to table
  coefficientsTable$addColumnInfo(name = "name",    title = "",                        type = "string")
  coefficientsTable$addColumnInfo(name = "Coeff",   title = gettext("Estimate"),       type = "number", format = "dp:3")
  coefficientsTable$addColumnInfo(name = "SE",      title = gettext("Standard Error"), type = "number", format = "dp:3")
  if(options$regressionCoefficientsConfidenceIntervals){
    confIntVal <- options$regressionCoefficientsConfidenceIntervalsInterval
    ci <- gettextf("%s%% Confidence Intervals",100*confIntVal)
    coefficientsTable$addColumnInfo(name = "Lower", title = gettext("Lower"), type = "number", overtitle = ci)
    coefficientsTable$addColumnInfo(name = "Upper", title = gettext("Upper"), type = "number", overtitle = ci)
  }
  coefficientsTable$addColumnInfo(name = "Z", title = gettext("Z"), type = "number")
  coefficientsTable$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  if (options$VovkSellkeMPR)
    coefficientsTable$addColumnInfo(name = "VovkSellkeMPR",  title = gettextf("VS-MPR%s", "\u002A"), type = "number")
  
  jaspResults[["CoefficientsTable"]] <- coefficientsTable
  
  if (!ready) 
    return()
  
  # Compute/get the Model
  loglm.model  <- .regLogLinComputeModel(jaspResults, dataset, options)
  
  res <- try(.regLogLinCoefficientsResults(jaspResults, dataset, options, loglm.model))
  .regLogLinSetError(res, coefficientsTable)
}

# Other Setup ----
.regLogLinSetError <- function(res, table){
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}

.regLogLinCitation <- function(table){
  citation <- ("R Core Team (2015). R: A language and environment for 
  statistical computing. R Foundation for Statistical Computing, Vienna, Austria. 
  URL https://www.R-project.org/.")
  table$addCitation(citation)
}

.regloglinAnovaLine <- function(options, char, name) {
  line <- list(
    "name"   = name,
    "df"     = char,
    "dev"    = char,
    "resDf"  = char,
    "resDev" = char,
    "p"      = char)
  if (options$VovkSellkeMPR) 
    line$VovkSellkeMPR <- char
  return(line)
}

.regLogLinAddVovkSellke <- function(table) {
  table$addColumnInfo(name = "VovkSellkeMPR",  title = gettextf("VS-MPR%s","\u002A"), type = "number")
  message <- gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based the <em>p</em>-value, \
the maximum possible odds in favor of H%1$s over H%2$s equals \
1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37 \
(Sellke, Bayarri, & Berger, 2001).", "\u2081", "\u2080", "\u2264")
  table$addFootnote(message, symbol = "\u002A") 
}

.regLogLinBuildLookup <- function(dataset, factors) {
  table <- list()
  for (v in factors) {
    levels <- levels(dataset[[ .v(v) ]])
    for (l in levels) {
      mangled.name <- paste(.v(v), l, sep = "")
      actual       <- c(v, l)
      table[[mangled.name]] <- actual
    }
  }
  return(table)
}
