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

RegressionLogistic <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- options$dependent != "" #&& options$wlsWeights != ""
  if(ready) {
    dataset <- .reglogisticReadData(dataset, options)
    .reglogisticCheckErrors(dataset, options)
  }
  # Output tables
  .reglogisticModelSummaryTable(       jaspResults, dataset, options, ready)
  .reglogisticEstimatesTable(          jaspResults, dataset, options, ready)
  .reglogisticEstimatesTableBootstrap( jaspResults, dataset, options, ready)
  .reglogisticFactorDescriptivesTable( jaspResults, dataset, options)
  .reglogisticCasewiseDiagnosticsTable(jaspResults, dataset, options, ready)
  .reglogisticConfusionMatrixTable(    jaspResults, dataset, options, ready)
  .reglogisticPerformanceMetricsTable( jaspResults, dataset, options, ready)
  
  # Output plots
  .reglogisticEstimatesPlot(              jaspResults, dataset, options, ready)
  .reglogisticPredictedResidualsPlot(     jaspResults, dataset, options, ready)
  .reglogisticPredictorResidualsPlot(     jaspResults, dataset, options, ready)
  .reglogisticSquaredPearsonResidualsPlot(jaspResults, dataset, options, ready)
  return()
}

# Preprocessing functions
.reglogisticReadData <- function(dataset, options) {
  if (!is.null(dataset)) 
    return(dataset)
  else {
    numericVars <- unlist(c(options$covariates, options$wlsWeights))
    numericVars <- numericVars[numericVars != ""]
    factorVars  <- unlist(c(options$dependent, options$factors))
    factorVars  <- factorVars[factorVars != ""]
    return(.readDataSetToEnd(columns.as.numeric  = numericVars,
                             columns.as.factor   = factorVars,
                             exclude.na.listwise = c(numericVars, factorVars)))
  }
}

.reglogisticCheckErrors <- function(dataset, options){
  .hasErrors(dataset, 
             type = "factorLevels",
             factorLevels.target  = options$dependent,
             factorLevels.amount  = '!= 2',
             exitAnalysisIfErrors = TRUE)
  if (options$wlsWeights != "")
    .hasErrors(dataset, 
               type = "limits", 
               limits.target = options$wlsWeights, 
               limits.min = 0, 
               limits.max = Inf,
               exitAnalysisIfErrors = TRUE)
  if (length(options$covariates) != 0)
    .hasErrors(dataset,
               type = c("observations", "infinity", "variance", "varCovData"),
               all.target = options$covariates,
               observations.amount  = "< 2",
               exitAnalysisIfErrors = TRUE)
}

# Performance Diagnostics Container
.reglogisticPerfDiagContainer <- function(jaspResults) {
  if (is.null(jaspResults[["perfDiag"]])) {
    container <- createJaspContainer(gettext("Performance Diagnostics"))
    jaspResults[["perfDiag"]] <- container
  }
}

# Tables
.reglogisticModelSummaryTable <- function(jaspResults, dataset, 
                                          options, ready) {
  if(!is.null(jaspResults[["modelSummary"]]))
    return()
  
  if(options[['dependent']] == "")
    modelSummary <- createJaspTable(gettext("Model Summary"))
  else 
    modelSummary <- createJaspTable(gettextf("Model Summary - %s", options[['dependent']]))
  
  dependList <- c("dependent", "method", "modelTerms", "includeIntercept")
  modelSummary$dependOn(dependList)
  modelSummary$position <- 1
  modelSummary$showSpecifiedColumnsOnly <- TRUE
  
  if(options$method == "enter")
    chiName <- "\u03A7\u00B2"
  else
    chiName <- "\u0394\u03A7\u00B2"
  
  
  modelSummary$addColumnInfo(name = "mod", title = gettext("Model"),    type = "string")
  modelSummary$addColumnInfo(name = "dev", title = gettext("Deviance"), type = "number")
  modelSummary$addColumnInfo(name = "aic", title = gettext("AIC"),      type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "bic", title = gettext("BIC"),      type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "dof", title = gettext("df"),       type = "integer")
  modelSummary$addColumnInfo(name = "chi", title = chiName,             type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "pvl", title = gettext("p"),        type = "pvalue")
  modelSummary$addColumnInfo(name = "fad", title = gettext("McFadden R\u00B2"),    type = "number")
  modelSummary$addColumnInfo(name = "nag", title = gettext("Nagelkerke R\u00B2"),  type = "number")
  modelSummary$addColumnInfo(name = "tju", title = gettext("Tjur R\u00B2"),        type = "number")
  modelSummary$addColumnInfo(name = "cas", title = gettext("Cox & Snell R\u00B2"), type = "number")
  
  jaspResults[["modelSummary"]] <- modelSummary
  
  res <- try(.reglogisticModelSummaryFill(jaspResults, dataset, options, ready))
  
  .reglogisticSetError(res, modelSummary)
}

.reglogisticEstimatesTable <- function(jaspResults, dataset, options, ready) {
  if(!options$coeffEstimates || !is.null(jaspResults[["estimatesTable"]]))
    return()
  
  estimatesTable <- createJaspTable(gettext("Coefficients"))
  estimatesTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                          options             = c("coeffEstimates", "stdCoeff", 
                                                  "oddsRatios", "coeffCI", "robustSEOpt",
                                                  "coeffCIInterval", "coeffCIOR", 
                                                  "robustSEopt", "VovkSellkeMPR"))
  estimatesTable$position <- 2
  estimatesTable$showSpecifiedColumnsOnly <- TRUE
  
  tmp <- .reglogisticEstimatesInfo(options)
  ciTitle    <- tmp[["ciTitle"]]
  seTitle    <- tmp[["seTitle"]]
  multimod   <- tmp[["multimod"]]
  paramtitle <- tmp[["paramtitle"]]

  if(options$method != "enter")
    estimatesTable$addColumnInfo(name = "model", title = gettext("Model"), type = "string", combine = TRUE)
  estimatesTable$addColumnInfo(name = "param",   title = paramtitle, type = "string")
  estimatesTable$addColumnInfo(name = "est",     title = gettext("Estimate"), type = "number", format="dp:3")
  estimatesTable$addColumnInfo(name = "se",      title = seTitle, type = "number", format="dp:3")
  if(options$stdCoeff)
    estimatesTable$addColumnInfo(name = "std",   title = gettext("Standardized\u207A"), type = "number", format="dp:3")
  if(options$oddsRatios)
    estimatesTable$addColumnInfo(name = "or",    title = gettext("Odds Ratio"), type = "number")
  estimatesTable$addColumnInfo(name = "zval",    title = gettext("z"), type = "number")
  estimatesTable$addColumnInfo(name = "waldsta", title = gettext("Wald Statistic"), type = "number", overtitle = "Wald Test")
  estimatesTable$addColumnInfo(name = "walddf",  title = gettext("df"), type = "integer", overtitle = "Wald Test")
  estimatesTable$addColumnInfo(name = "pval",    title = gettext("p"), type = "pvalue", overtitle = "Wald Test")

  if(options$VovkSellkeMPR)
    .reglogisticVovkSellke(estimatesTable, options)

  if(options$coeffCI) {
    estimatesTable$addColumnInfo(name = "cilo", title = gettext("Lower bound"), type = "number", format="dp:3", overtitle = ciTitle)
    estimatesTable$addColumnInfo(name = "ciup", title = gettext("Upper bound"), type = "number", format="dp:3", overtitle = ciTitle)
  }
  
  if (options$stdCoeff)
    estimatesTable$addFootnote(gettext("Standardized estimates represent estimates where the continuous predictors are standardized (X-standardization)."), symbol = "\u207A")

  jaspResults[["estimatesTable"]] <- estimatesTable
  
  if(!ready) return()
  res <- try(.reglogisticEstimatesFill(jaspResults, dataset, options, multimod))
  
  .reglogisticSetError(res, estimatesTable)
}

.reglogisticEstimatesTableBootstrap <- function(jaspResults, dataset, 
                                                options, ready) {
  if(!options$coeffEstimatesBootstrapping || 
     !is.null(jaspResults[["estimatesTableBootstrapping"]]))
    return()
  
  estimatesTableBootstrap <- createJaspTable(gettext("Bootstrap Coefficients"))
  estimatesTableBootstrap$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                                   options             = c("coeffEstimatesBootstrapping",
                                                           "coeffEstimatesBootstrappingReplicates",
                                                           "coeffCI", "coeffCIOR", "stdCoeff", "oddsRatios", "coeffCIInterval",
                                                           "robustSEOpt"))
  estimatesTableBootstrap$position <- 3
  estimatesTableBootstrap$showSpecifiedColumnsOnly <- TRUE
  
  tmp <- .reglogisticEstimatesInfo(options, addBCA = TRUE)
  ciTitle    <- tmp[["ciTitle"]]
  seTitle    <- tmp[["seTitle"]]
  multimod   <- tmp[["multimod"]]
  paramtitle <- tmp[["paramtitle"]]

  if(options$method != "enter")
    estimatesTableBootstrap$addColumnInfo(name = "model", title = gettext("Model"),          type = "string", combine = TRUE)
  estimatesTableBootstrap$addColumnInfo(name = "param",   title = paramtitle,                type = "string")
  estimatesTableBootstrap$addColumnInfo(name = "est",     title = gettext("Estimate"),       type = "number", format="dp:3")
  estimatesTableBootstrap$addColumnInfo(name = "bias",    title = gettext("Bias"),           type = "number", format="dp:3")
  estimatesTableBootstrap$addColumnInfo(name = "se",      title = seTitle,                   type = "number", format="dp:3")
  if(options$stdCoeff) {
    estimatesTableBootstrap$addColumnInfo(name = "std",   title = gettext("Standardized\u207A"), type = "number", format="dp:3")
    estimatesTableBootstrap$addFootnote(gettext("Standardized estimates represent estimates where the continuous predictors are standardized (X-standardization)."), symbol = "\u207A")
  }
  if(options$oddsRatios)
    estimatesTableBootstrap$addColumnInfo(name = "or",    title = gettext("Odds Ratio"), type = "number")
  if (options$coeffCI) {
    estimatesTableBootstrap$addColumnInfo(name = "cilo",    title = gettext("Lower bound"),    type = "number", format="dp:3", overtitle = ciTitle)
    estimatesTableBootstrap$addColumnInfo(name = "ciup",    title = gettext("Upper bound"),    type = "number", format="dp:3", overtitle = ciTitle)
    estimatesTableBootstrap$addFootnote(gettext("Bias corrected accelerated."), symbol = "\u002A")
  }
  
  jaspResults[["estimatesTableBootstrapping"]] <- estimatesTableBootstrap
  
  if(!ready) return()
  res <- try(.reglogisticEstimatesBootstrapFill(jaspResults, dataset, options, multimod))
  
  .reglogisticSetError(res, estimatesTableBootstrap)
  
  if (options$robustSEOpt)
    estimatesTableBootstrap$addFootnote(gettext("Coefficient estimate and robust standard error are based on the median of the bootstrap distribution."))
  else
    estimatesTableBootstrap$addFootnote(gettext("Coefficient estimate is based on the median of the bootstrap distribution."))
}

.reglogisticCasewiseDiagnosticsTable <- function(jaspResults, dataset, options, ready){
  if(!options$casewiseDiagnostics || !is.null(jaspResults[["casewiseDiagnosticsTable"]]))
    return()
  
  casDiag <- createJaspTable(gettext("Casewise Diagnostics"))
  casDiag$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                               options = c("casewiseDiagnostics",
                                           "casewiseDiagnosticsResidualZ",
                                           "casewiseDiagnosticsCooksDistance",
                                           "casewiseDiagnosticsType"))
  casDiag$position <- 4
  casDiag$showSpecifiedColumnsOnly <- TRUE
  
  casDiag$addColumnInfo(name = "caseNumber", title = gettext("Case Number"),          type = "integer")
  casDiag$addColumnInfo(name = "observed",   title = gettext("Observed"),             type = "string")
  casDiag$addColumnInfo(name = "predicted",  title = gettext("Predicted"),            type = "number")
  casDiag$addColumnInfo(name = "predGroup",  title = gettext("Predicted Group"),      type = "string")
  casDiag$addColumnInfo(name = "residual",   title = gettext("Residual"),             type = "number")
  casDiag$addColumnInfo(name = "residualZ",  title = gettext("Studentized Residual"), type = "number")
  casDiag$addColumnInfo(name = "cooksD",     title = gettext("Cook's Distance"),      type = "number")
  
  jaspResults[["casewiseDiagnosticsTable"]] <- casDiag
  
  if(!ready) return()
  res <- try(.reglogisticCasewiseDiagnosticsFill(jaspResults, dataset, options))
  
  .reglogisticSetError(res, casDiag)
}

.reglogisticFactorDescriptivesTable <- function(jaspResults, dataset, options){
  if(!options$factorDescriptivesOpt || 
     !is.null(jaspResults[["factorDescriptives"]]))
    return()
  if(is.null(dataset))
    dataset <- .reglogisticReadData(dataset, options)
  factorDescriptives <- createJaspTable(gettext("Factor Descriptives"))
  factorDescriptives$dependOn(c("factorDescriptivesOpt", "factors"))
  factorDescriptives$position <- 5
  factorDescriptives$showSpecifiedColumnsOnly <- TRUE
  if (length(options$factors) == 0)
    factorDescriptives$addColumnInfo(name = "Factor", title = gettext("Factor"),
                                     type = "string")
  else {
    for (variable in options$factors) {
      name <- paste("__", variable, sep = "")  # distinguish it in case it's "N"
      factorDescriptives$addColumnInfo(name = name, type = "string",
                                       title = variable, combine = TRUE)
    }
  }
  factorDescriptives$addColumnInfo(name = "N", title=gettext("N"), type = "integer")
  
  jaspResults[["factorDescriptives"]] <- factorDescriptives
  
  res <- try(.reglogisticFactorDescriptivesFill(jaspResults, dataset, options))
  
  .reglogisticSetError(res, factorDescriptives)
}

.reglogisticConfusionMatrixTable <- function(jaspResults, dataset, options, ready) {
  .reglogisticPerfDiagContainer(jaspResults)
  container <- jaspResults[["perfDiag"]]
  if(!options$confusionMatrixOpt || 
     !is.null(container[["confusionMatrix"]]))
    return()
  confusionMatrix <- createJaspTable(gettext("Confusion matrix"))
  confusionMatrix$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                           options           = c("confusionMatrixOpt", "confusionMatrixProportions"))
  confusionMatrix$position <- 6
  confusionMatrix$showSpecifiedColumnsOnly <- TRUE
  
  confusionMatrix$addColumnInfo(name = "obs", title = gettext("Observed"), type = "string")
  if (ready) {
    # Compute/Get Model
    glmObj <- .reglogisticComputeModel(jaspResults, dataset, options)
    #modelTerms <- c()
    #for(i in seq_along(options$modelTerms))
    #  modelTerms <- c(modelTerms, options$modelTerms[[i]]$components)
    #levs <- levels(dataset[[.v(modelTerms)]])
    mObj <- glmObj[[length(glmObj)]]
    levs <- levels(mObj[["model"]][,1])
  } else {
    levs   <- c(0,1)
    glmObj <- NULL
  }
  if(options$confusionMatrixProportions)
    .confusionMatAddColInfo(confusionMatrix, levs, "number")
  else
    .confusionMatAddColInfo(confusionMatrix, levs, "integer")
  container[["confusionMatrix"]] <- confusionMatrix
  
  res <- try(.reglogisticConfusionMatrixFill(container, dataset, options, glmObj, ready))
  
  .reglogisticSetError(res, confusionMatrix)
}

.reglogisticPerformanceMetricsTable <- function(jaspResults, dataset, options, ready){
  .reglogisticPerfDiagContainer(jaspResults)
  container <- jaspResults[["perfDiag"]]
  if(!is.null(container[["performanceMetrics"]]))
    return()
  performList <- c("AUC", "Sens", "Spec", "Prec", "Fmsr", "BrierScr", "Hmsr")
  if(!any(unlist(options[performList])))
    return()
  performanceMetrics <- createJaspTable(gettext("Performance metrics"))
  performanceMetrics$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                              options           = performList)
  performanceMetrics$position <- 7
  performanceMetrics$showSpecifiedColumnsOnly <- TRUE
  
  performanceMetrics$addColumnInfo(name = "met", title = "", type = "string")
  performanceMetrics$addColumnInfo(name = "val", title = gettext("Value"), type = "number")
  
  container[["performanceMetrics"]] <- performanceMetrics
  
  res <- try(.reglogisticPerformanceMetricsFill(jaspResults, container, dataset, options, ready))
  
  .reglogisticSetError(res, performanceMetrics)
}

#Table Filling
.reglogisticModelSummaryFill <- function(jaspResults, dataset, options, ready) {
  if(ready) {
    # Compute/Get Model
    glmObj <- .reglogisticComputeModel(jaspResults, dataset, options)
    hasNuisance <- .hasNuisance(options)
    if (hasNuisance) {
      terms <- rownames(summary(glmObj[[1]])[["coefficients"]])
      terms <- sapply(terms[terms!="(Intercept)"], .formatTerm,
                      glmModel=glmObj[[1]])
      message <- gettextf("Null model contains nuisance parameters: %s",
                        paste(terms, collapse = ", "))
      jaspResults[["modelSummary"]]$addFootnote(message)
    }
    if (options$method == "enter") {
      # Two rows: h0 and h1
      lr <- .lrtest(glmObj[[1]], glmObj[[2]])
      
      rows <- list(
        list(mod = "H\u2080",
             dev = glmObj[[1]][["deviance"]],
             aic = glmObj[[1]][["aic"]],
             bic = .bic(glmObj[[1]]),
             dof = glmObj[[1]][["df.residual"]],
             chi = "",
             pvl = "",
             fad = "",
             nag = "",
             tju = "",
             cas = ""),
        list(mod = "H\u2081",
             dev = glmObj[[2]][["deviance"]],
             aic = glmObj[[2]][["aic"]],
             bic = .bic(glmObj[[2]]),
             dof = glmObj[[2]][["df.residual"]],
             chi = lr[["stat"]],
             pvl = lr[["pval"]],
             fad = .mcFadden(  glmObj[[2]], glmObj[[1]]),
             nag = .nagelkerke(glmObj[[2]], glmObj[[1]]),
             tju = .tjur(glmObj[[2]]),
             cas = .coxSnell(glmObj[[2]], glmObj[[1]])
        )
      )
      
      
    } else {
      # multiple rows: m1 - mk
      rows <- vector("list", length(glmObj))
      
      for (midx in 1:length(glmObj)) {
        mObj <- glmObj[[midx]]
        if (midx > 1) {
          if (options$method == "forward" ||
              options$method == "stepwise") {
            fadden <- .mcFadden(mObj, glmObj[[1]])
            nagel  <- .nagelkerke(mObj, glmObj[[1]])
            coxSn  <- .coxSnell(mObj, glmObj[[1]])
          } else {
            fadden <- -1*.mcFadden(glmObj[[1]], mObj)
            nagel  <- -1*.nagelkerke(glmObj[[1]], mObj)
            coxSn  <- -1*.coxSnell(glmObj[[1]], mObj)
          }
          
          lr <- .lrtest(glmObj[[midx]], glmObj[[midx-1]])
          rows[[midx]] <- list(
            mod = as.character(midx),
            dev = mObj[["deviance"]],
            aic = mObj[["aic"]],
            bic = .bic(mObj),
            dof = mObj[["df.residual"]],
            chi = lr[["stat"]],
            pvl = lr[["pval"]],
            fad = fadden,
            nag = nagel,
            tju = .tjur(mObj),
            cas = coxSn
          )
        } else {
          rows[[midx]] <- list(
            mod = as.character(midx),
            dev = mObj[["deviance"]],
            aic = mObj[["aic"]],
            bic = .bic(mObj),
            dof = mObj[["df.residual"]],
            chi = NULL,
            pvl = NULL,
            fad = .mcFadden(mObj, mObj),
            nag = .nagelkerke(mObj, mObj),
            tju = .tjur(mObj),
            cas = .coxSnell(mObj, mObj)
          )
        }
      }
      
    }
  } else {
    rows <- list(
      list(mod = "H\u2080", dev = ".", fad = NULL, 
           nag = NULL, tju = NULL, cas = NULL, aic = "."),
      list(mod = "H\u2081", dev = ".", fad = ".", 
           nag = ".", tju = ".", cas = ".", aic = ".")
    )
  }
  jaspResults[["modelSummary"]]$addRows(rows)
}

.reglogisticEstimatesFill <- function(jaspResults, dataset, options, multimod) {
  # Compute/Get Model
  glmObj <- .reglogisticComputeModel(jaspResults, dataset, options)
  alpha  <- qnorm(1 - (1 - options$coeffCIInterval) / 2)

  if (!multimod) {
    s  <- summary(glmObj[[2]])[["coefficients"]]
    rn <- rownames(s)
    rn[which(rn == "(Intercept)")] <- .v("(Intercept)") #Some magic is going on here, not sure if I should add gettext() at this point and creation of glmObj in commonglm.R... (JCG 6-1-20)
    beta  <- .stdEst(glmObj[[2]], type = "X") # stand. X continuous vars
    
    # Confidence intervals on the odds ratio scale
    if (options$coeffCIOR)
      expon <- function(x) exp(x)
    else
      expon <- function(x) x
    
    if (length(rn) == 1) {
      s <- unname(s)
      if (options$robustSEOpt) {
        s[2] <- unname(.glmRobustSE(glmObj[[2]])) # new se
        s[3] <- s[1]/s[2] # new z
        s[4] <- 2*pnorm(-abs(s[3])) # new p
      }
      waldtest  <- mdscore::wald.test(glmObj[[2]], term = 1)
      jaspResults[["estimatesTable"]]$addRows(
        list(           param   = .formatTerm(rn, glmObj[[2]]),
                        est     = s[1],
                        se      = s[2],
                        std     = as.numeric(beta),
                        or      = exp(s[1]),
                        zval    = s[3],
                        pval    = s[4],
                        waldsta = as.numeric(waldtest$W),
                        walddf  = as.numeric(1),
                        vsmpr   = .VovkSellkeMPR(s[4]),
                        cilo    = expon(s[1] - alpha * s[2]),
                        ciup    = expon(s[1] + alpha * s[2])))
    } else {
      if (options$robustSEOpt) {
        s[,2] <- unname(.glmRobustSE(glmObj[[2]])) # new se
        s[,3] <- s[,1]/s[,2] # new z
        s[,4] <- 2*pnorm(-abs(s[,3])) # new p
      }
      for (i in seq_along(rn)) {
        
        waldtest <- mdscore::wald.test(glmObj[[2]], term = i)
        
        jaspResults[["estimatesTable"]]$addRows(list(
                          param   = .formatTerm(rn[i], glmObj[[2]]),
                          est     = s[i,1],
                          se      = s[i,2],
                          std     = as.numeric(beta[i]),
                          or      = exp(s[i,1]),
                          zval    = s[i,3],
                          pval    = s[i,4],
                          waldsta = as.numeric(waldtest$W),
                          walddf  = as.numeric(1),
                          vsmpr   = .VovkSellkeMPR(s[i,4]),
                          cilo    = expon(s[i,1] - alpha * s[i,2]),
                          ciup    = expon(s[i,1] + alpha * s[i,2])))
      }
    }
  } else {
    for (midx in 1:length(glmObj)) {
      mObj <- glmObj[[midx]]
      s    <- summary(mObj)[["coefficients"]]
      rn   <- rownames(s)
      rn[which(rn == "(Intercept)")] <- .v("(Intercept)") #???
      beta <- .stdEst(mObj, type = "X") # stand. X continuous vars
      
      # Confidence intervals on the odds ratio scale
      if (options$coeffCIOR)
        expon <- function(x) exp(x)
      else
        expon <- function(x) x
      
      
      if (length(rn) == 1) {
        s <- unname(s)
        if (options$robustSEOpt) {
          s[2] <- unname(.glmRobustSE(mObj)) # new se
          s[3] <- s[1]/s[2] # new z
          s[4] <- 2*pnorm(-abs(s[3])) # new p
        }
        waldtest <- mdscore::wald.test(mObj, term = 1)
        
        jaspResults[["estimatesTable"]]$addRows(list(
          model   = as.character(midx),
          param   = .formatTerm(rn, mObj),
          est     = s[1],
          se      = s[2],
          std     = as.numeric(beta),
          or      = exp(s[1]),
          zval    = s[3],
          pval    = s[4],
          waldsta = as.numeric(waldtest$W),
          walddf  = as.numeric(1),
          vsmpr   = .VovkSellkeMPR(s[4]),
          cilo    = expon(s[1] - alpha * s[2]),
          ciup    = expon(s[1] + alpha * s[2]),
          .isNewGroup = TRUE
        ))
      } else {
        if (options$robustSEOpt) {
          s[,2] <- unname(.glmRobustSE(mObj)) # new se
          s[,3] <- s[,1]/s[,2] # new z
          s[,4] <- 2*pnorm(-abs(s[,3])) # new p
        }
        for (i in seq_along(rn)) {
          
          waldtest <- mdscore::wald.test(mObj, term = i)
          jaspResults[["estimatesTable"]]$addRows(list(
            model   = as.character(midx),
            param   = .formatTerm(rn[i], mObj),
            est     = s[i,1],
            se      = s[i,2],
            std     = as.numeric(beta[i]),
            or      = exp(s[i,1]),
            zval    = s[i,3],
            pval    = s[i,4],
            waldsta = as.numeric(waldtest$W),
            walddf  = as.numeric(1),
            vsmpr   = .VovkSellkeMPR(s[i,4]),
            cilo    = expon(s[i,1] - alpha * s[i,2]),
            ciup    = expon(s[i,1] + alpha * s[i,2]),
            .isNewGroup = (i == 1)
          ))
        }
      }
    }
  }

  predVar   <- as.character(glmObj[[1]][["terms"]])[2]
  predLevel <- levels(glmObj[[1]][["data"]][[predVar]])[2]

  jaspResults[["estimatesTable"]]$addFootnote(gettextf("%1$s level '%2$s' coded as class 1.", .unv(predVar), predLevel))
}

.reglogisticEstimatesBootstrapFill <- function(jaspResults, dataset, options, multimod){
  # Compute/Get Model
  glmObj     <- .reglogisticComputeModel(jaspResults, dataset, options)
  ci.fails   <- FALSE

  if (is.null(jaspResults[["bootstrapResults"]])) {
    bootstrapResults <- list()
  } else {
    bootstrapResults <- jaspResults[["bootstrapResults"]]$object
  }
  if (!is.null(glmObj)) {

    expon <- if (options$coeffCIOR) function(x) exp(x) else identity

    startProgressbar(options$coeffEstimatesBootstrappingReplicates *
                     if (multimod) length(glmObj) else 1)
    
    for (i in 1:length(glmObj)) {
      if (!multimod && i != 2)
        next
      
        rn <- rownames(summary(glmObj[[i]])[["coefficients"]])
        rn[which(rn == "(Intercept)")] <- .v("(Intercept)")
        bootname <- paste(rn, collapse = "-")
        
        if (is.null(bootstrapResults[[bootname]])) {

          # vandenman: we compute additional statistics while bootstrapping, but we can't do this using boot
          # so we hack it in there using an environment
          # kucharssim: this is not true, you can boot whatever statistics you want, but ok... the bootstrapping should get some review anyway at some point
          # discussion here: https://github.com/jasp-stats/jasp-desktop/pull/3962#discussion_r394348052
          envir <- new.env()
          envir$idx <- envir$idx_rse <- 0L
          envir$stdEst <- envir$robustSE <-
            matrix(NA, options$coeffEstimatesBootstrappingReplicates, length(coef(glmObj[[i]])))
          
          .bootstrapping    <- function(data, indices, model.formula, options, envir) {
            progressbarTick()
            
            d <- data[indices, , drop = FALSE] # allows boot to select sample
            result <- glm(model.formula, family = "binomial", data = d)
            
            if(length(coef(result)) != length(coef(glmObj[[i]]))) return(rep(NA, length(coef(glmObj[[i]]))))
            if (envir$idx == 0L) {
              # boot::boot does one test run before doing all runs (which it ignores in the results)
              envir$idx     <- 1L
              envir$idx_rse <- 1L
            } else {
              resultStd <- try(unname(.stdEst(result, type = "X")))
              if (!isTryError(resultStd)) {
                envir$stdEst[envir$idx, ] <- resultStd
                envir$idx <- envir$idx + 1L
              }
              robustSE <- try(unname(.glmRobustSE(result)))
              if (!isTryError(resultStd)) {
                envir$robustSE[envir$idx_rse, ] <- robustSE
                envir$idx_rse <- envir$idx_rse + 1L
              }
            }
            return(coef(result))
          }
          
          bootstrap.summary <- try(boot::boot(data = dataset,
                                              statistic = .bootstrapping,
                                              R = options$coeffEstimatesBootstrappingReplicates,
                                              model.formula = formula(glmObj[[i]]),
                                              options = options,
                                              envir = envir),
                                   silent = TRUE)
          
          bootstrap.summary$stdEst   <- envir$stdEst
          bootstrap.summary$robustSE <- envir$robustSE
          bootstrapResults[[bootname]] <- bootstrap.summary
          try(rm(envir, envir = parent.env(envir)), silent = TRUE)

        } else {
          bootstrap.summary <- bootstrapResults[[bootname]]
        }
        
      bootstrap.coef <- matrixStats::colMedians(bootstrap.summary$t, na.rm = TRUE)
      bootstrap.std  <- matrixStats::colMedians(bootstrap.summary$stdEst, na.rm = TRUE)
      bootstrap.bias <- colMeans(bootstrap.summary$t, na.rm = TRUE) - bootstrap.summary$t0
      bootstrap.or   <- matrixStats::colMedians(exp(bootstrap.summary$t), na.rm = TRUE)
      if (options$robustSEOpt)
        bootstrap.se <- matrixStats::colMedians(bootstrap.summary$robustSE, na.rm = TRUE)
      else
        bootstrap.se <- matrixStats::colSds(as.matrix(bootstrap.summary$t), na.rm = TRUE)
      
      jaspResults[['estimatesTableBootstrapping']]$addFootnote(gettextf("Bootstrapping based on %i successful replicates.", sum(complete.cases(bootstrap.summary$t))))
      for (j in seq_along(rn)) {
        
        row <- list(
          model = as.character(i),
          param = .formatTerm(rn[j], glmObj[[i]]),
          est   = as.numeric(bootstrap.coef[j]),
          bias  = as.numeric(bootstrap.bias[j]),
          se    = as.numeric(bootstrap.se[j]),
          .isNewGroup = as.logical(j == 1)
        )

        if (options$coeffCI) {
          result.bootstrap.ci <- try(boot::boot.ci(bootstrap.summary, type = "bca", conf = options$coeffCIInterval, index=j))
          if(!isTryError(result.bootstrap.ci))
            bootstrap.ci <- result.bootstrap.ci
          else if(identical(attr(result.bootstrap.ci, "condition")$message, "estimated adjustment 'a' is NA")){
            # NOTE: the if statement above doesn't work if the package uses gettext and translates error messages.
            ci.fails <- TRUE
            bootstrap.ci <- list(bca = rep(NA, 5))
          } else
            bootstrap.ci <- result.bootstrap.ci
          
          if(ci.fails)
            jaspResults[['estimatesTableBootstrapping']]$addFootnote(gettext("Some confidence intervals could not be computed.\nPossibly too few bootstrap replicates."))

          row[["cilo"]] <- expon(as.numeric(bootstrap.ci$bca[4]))
          row[["ciup"]] <- expon(as.numeric(bootstrap.ci$bca[5]))
        }

        row[["or"]]  <- bootstrap.or[j]
        row[["std"]] <- bootstrap.std[j]

        jaspResults[["estimatesTableBootstrapping"]]$addRows(row)
      }
    }
    bootstrapResultsState <- createJaspState(bootstrapResults)
    bootstrapResultsState$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                                   options             = c("coeffEstimatesBootstrapping", "coeffEstimatesBootstrappingReplicates"))
    jaspResults[["bootstrapResults"]] <- bootstrapResultsState
  } else {
    return()
  }
}

.reglogisticCasewiseDiagnosticsFill <- function(jaspResults, dataset, options){
  # Compute/Get Model
  glmObj <- .reglogisticComputeModel(jaspResults, dataset, options)
  if (is.null(glmObj))
    return()
  else {
    casewiseDiag <- .casewiseDiagnosticsLogisticRegression(dataset, glmObj, options)
    caseNumbers  <- casewiseDiag$index
    if (is.na(caseNumbers))
      return()
    else {
      for (case in seq_along(caseNumbers))
        jaspResults[["casewiseDiagnosticsTable"]]$addRows(
          list(caseNumber = caseNumbers[case], 
               observed   = casewiseDiag$dependent[case],
               predicted  = casewiseDiag$predicted[case],
               predGroup  = casewiseDiag$predictedGroup[case],
               residual   = casewiseDiag$residual[case],
               residualZ  = casewiseDiag$residualZ[case],
               cooksD     = casewiseDiag$cooksD[case]
          )
        )
    }
  }
}

.reglogisticFactorDescriptivesFill <- function(jaspResults, dataset, options){

  if (length(options$factors) > 0) {
    lvls    <- list()
    factors <- list()
    
    for (variable in options$factors) {
      factor <- dataset[[ .v(variable) ]]
      factors[[length(factors)+1]] <- factor
      lvls[[ variable ]] <- levels(factor)
    }
    
    cases <- rev(expand.grid(rev(lvls)))
    namez <- unlist(options$factors)
    columnNames <- paste("__", namez, sep="")
    
    if (length(options$factors) > 0) {
      for (i in 1:dim(cases)[1]) {
        row <- list()
        for (j in 1:dim(cases)[2])
          row[[ columnNames[[j]] ]] <- as.character(cases[i, j])
        
        # eval parse? Is this really the best way to do this?
        sub   <- eval(parse(text=paste("dataset$", .v(namez), " == \"", row, "\"", sep="", collapse = " & ")))
        dat   <- subset(dataset, sub)[[1]]
        N     <- length(dat)
        
        row$N <- N
        row[[".isNewGroup"]]   <- cases[i,dim(cases)[2]] == lvls[[ dim(cases)[2] ]][1]

        jaspResults[["factorDescriptives"]]$addRows(row)
      }
    }
  } else
    jaspResults[["factorDescriptives"]]$addRows(list(Factor = ".", N = "."))

}

.reglogisticConfusionMatrixFill <- function(container, dataset, options, glmObj, ready) {
  if (ready) {
    mObj   <- glmObj[[length(glmObj)]]
    levs   <- levels(mObj[["model"]][,1])
    if (options$confusionMatrixProportions)
      n <- length(mObj[["y"]])
    else
      n <- 1
    m <- .confusionMatrix(mObj, cutoff = 0.5)[["matrix"]]
    container[["confusionMatrix"]]$addRows(list(
      list(obs = levs[1], pred0 = m[1,1]/n, pred1 = m[1,2]/n),
      list(obs = levs[2], pred0 = m[2,1]/n, pred1 = m[2,2]/n)
    ))
  } else
    container[["confusionMatrix"]]$addRows(list(
      list(obs = "0", pred0 = ".", pred1 = "."),
      list(obs = "1", pred0 = ".", pred1 = ".")
    ))
}

.reglogisticPerformanceMetricsFill <- function(jaspResults, container, dataset, options, ready){
  if(ready) {
    # Compute/Get Model
    glmObj <- .reglogisticComputeModel(jaspResults, dataset, options)
    mObj   <- glmObj[[length(glmObj)]]
    m <- .confusionMatrix(mObj, cutoff = 0.5)[["metrics"]]
    rows <- list(
      list(met = "AUC",         val = m[["AUC"]]),
      list(met = "Sensitivity", val = m[["Sens"]]),
      list(met = "Specificity", val = m[["Spec"]]),
      list(met = "Precision",   val = m[["Precision"]]),
      list(met = "F-measure",   val = m[["F"]]),
      list(met = "Brier score", val = m[["Brier"]]),
      list(met = "H-measure",   val = m[["H"]])
    )
  } else
    rows <- list(
      list(met = "AUC",         val = "."),
      list(met = "Sensitivity", val = "."),
      list(met = "Specificity", val = "."),
      list(met = "Precision",   val = "."),
      list(met = "F-measure",   val = "."),
      list(met = "Brier score", val = "."),
      list(met = "H-measure",   val = ".")
    )
  
  # determine which scores we need
  scrNeed  <- with(options, c(AUC, Sens, Spec, Prec, Fmsr, BrierScr, Hmsr))
  rows     <- rows[scrNeed]
  container[["performanceMetrics"]]$addRows(rows)
}

# Plots
.reglogisticResidualPlotContainer <- function(jaspResults, options){
  if(!options$predictedPlotOpt && !options$predictorPlotsOpt && 
     !options$squaredPearsonPlotOpt)
    return()
  if (is.null(jaspResults[["residualPlots"]])) {
    container <- createJaspContainer(gettext("Residual plots"))
    container$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                       options           = c("residualType"))
    jaspResults[["residualPlots"]] <- container
  }
}

.reglogisticEstimatesPlot <- function(jaspResults, dataset, options, ready){
  if(!options$estimatesPlotsOpt)
    return()
  jaspResults[["estimatesPlot"]] <- createJaspContainer(gettext("Estimates plots"))
  jaspResults[["estimatesPlot"]]$dependOn(optionsFromObject = jaspResults[["modelSummary"]])
  container <- jaspResults[["estimatesPlot"]]
  
  if(!ready){
    container[["placeholder"]] <- createJaspPlot(width = 320, height = 320, dependencies = "estimatesPlotsOpt")
    return()
  }
  # Compute/Get Model
  glmObj <- .reglogisticComputeModel(jaspResults, dataset, options)
  mObj   <- glmObj[[length(glmObj)]]
  predictors <- character(0)
  
  for (term in options$modelTerms)
    if (length(term$components) == 1 && 
        (is.null(term$isNuisance) || !term$isNuisance))
      predictors <- c(predictors, term$components)
  
  if (length(predictors) > 0 && !is.null(glmObj)) {
    # plot only predictors selected in the final model
    predictors <- predictors[.v(predictors) %in% attr(mObj[["terms"]], 
                                                      "term.labels")]
    for(pred in predictors) {
      estimatesPlot <- createJaspPlot(title = pred, width = 480, height = 320)
      estimatesPlot$dependOn(options = c("estimatesPlotsOpt", "estimatesPlotsCI", "showPoints"))
      p <- try(.reglogisticEstimatesPlotFill(options, mObj, pred))
      if(isTryError(p))
        estimatesPlot$setError(.extractErrorMessage(p))
      else
        estimatesPlot$plotObject <- p
      container[[pred]] <- estimatesPlot
    }
  }
  return()
}

.reglogisticPredictedResidualsPlot <- function(jaspResults, dataset, options, ready){
  if(!options$predictedPlotOpt)
    return()
  .reglogisticResidualPlotContainer(jaspResults, options)
  container <- jaspResults[["residualPlots"]]
  
  title <- gettext("Predicted - residuals plot")
  predictedPlot <- createJaspPlot(title = title, width = 480, height = 320)
  predictedPlot$dependOn("predictedPlotOpt")
  
  if(ready){
    # Compute/Get Model
    glmObj <- .reglogisticComputeModel(jaspResults, dataset, options)
    mObj   <- glmObj[[length(glmObj)]]
    p      <- try(.reglogisticResidPlotFill(options, mObj))
    if(isTryError(p))
      predictedPlot$setError(.extractErrorMessage(p))
    else
      predictedPlot$plotObject <- p
  }
  container[["predicted"]] <- predictedPlot 
  return()
}

.reglogisticPredictorResidualsPlot <- function(jaspResults, dataset, options, ready){
  if(!options$predictorPlotsOpt)
    return()
  .reglogisticResidualPlotContainer(jaspResults, options)
  container    <- jaspResults[["residualPlots"]]
  container[["subContainer"]] <- createJaspContainer(gettext("Predictor - residuals plots"))
   subcontainer <- container[["subContainer"]]
  if(!ready){
    subcontainer[["placeholder"]] <- createJaspPlot(width = 320, height = 320, dependencies = "predictorPlotsOpt")
    return()
  }
  # Compute/Get Model
  glmObj <- .reglogisticComputeModel(jaspResults, dataset, options)
  mObj   <- glmObj[[length(glmObj)]]
  
  predictors <- character(0)
  for (term in options$modelTerms)
    if (length(term$components) == 1 && 
        (is.null(term$isNuisance) || !term$isNuisance))
      predictors <- c(predictors, term$components)
  # plot only predictors selected in the final model
  predictors <- predictors[.v(predictors) %in% attr(mObj[["terms"]], 
                                                    "term.labels")]
  for(pred in predictors) {
    predictorPlot <- createJaspPlot(title = pred, width = 480, height = 320)
    predictorPlot$dependOn("predictorPlotsOpt")
    
    p <- try(.reglogisticResidPlotFill(options, mObj, pred))
    if(isTryError(p))
      predictorPlot$setError(.extractErrorMessage(p))
    else
      predictorPlot$plotObject <- p
    subcontainer[[pred]] <- predictorPlot
  }
  return()
}

.reglogisticSquaredPearsonResidualsPlot <- function(jaspResults, dataset, options, ready){
  if(!options$squaredPearsonPlotOpt)
    return()
  .reglogisticResidualPlotContainer(jaspResults, options)
  container <- jaspResults[["residualPlots"]]
  
  title <- gettext("Squared Pearson residuals plot")
  squaredPearsonPlot <- createJaspPlot(title = title, width = 480, height = 320)
  squaredPearsonPlot$dependOn("squaredPearsonPlotOpt")
  
  if(ready){
    # Compute/Get Model
    glmObj <- .reglogisticComputeModel(jaspResults, dataset, options)
    mObj   <- glmObj[[length(glmObj)]]
    p      <- try(.reglogisticSquaredPearsonResidualsFill(mObj))
    if(isTryError(p))
      squaredPearsonPlot$setError(.extractErrorMessage(p))
    else
      squaredPearsonPlot$plotObject <- p
  }
  container[["squaredPearson"]] <- squaredPearsonPlot 
  return()
}

# Plot Filling
.reglogisticEstimatesPlotFill <- function(options, mObj, pred){
  # If user wants raw data points, get them from data
  points <- options$showPoints
  if (points) {
    mf <- model.frame(mObj)
    factors <- names(mObj[["xlevels"]])
    if (.v(pred) %in% factors)
      vd <- as.factor(mObj[["data"]][[.v(pred)]])
    else
      vd <- mf[,.v(pred)]
    ggdat <- data.frame(x = vd, y = mObj$y)
  }
  # Calculate ribbon & main line
  ribdat <- .glmLogRegRibbon(mObj, .v(pred), options$estimatesPlotsCI)
  # Find predicted level
  predVar   <- as.character(mObj[["terms"]])[2]
  predLevel <- levels(mObj[["data"]][[predVar]])[2]
  
  # this will become the y-axis title
  ytitle <- substitute(expr = "P("*italic(x)~italic("=")~italic(y)*")",
                       env = list(x = .unv(predVar), y = predLevel))
  if (attr(ribdat, "factor")) {
    # the variable is a factor, plot points with errorbars
    p <- ggplot2::ggplot(ribdat, ggplot2::aes(x = x, y = y))
    
    if (points) {
      p <- p + ggplot2::geom_point(data = ggdat, size = 2,
        position = ggplot2::position_jitter(height = 0.01, width = 0.04),
        color = "dark grey", alpha = 0.3)
    }
    p <- p +
      ggplot2::geom_point(data = ribdat,
        mapping = ggplot2::aes(x = x, y = y),
        size = 4
      ) +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes(x = x, ymin = lo, ymax = hi),
        data = ribdat, width = 0.2
      )
    
  } else {
    # the variable is continuous, plot curve with error ribbon
    p <- ggplot2::ggplot(ribdat, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_ribbon(data = ribdat,
        mapping = ggplot2::aes(x = x, ymax = hi, ymin = lo),
        fill = "light grey", alpha = 0.5
      ) +
      ggplot2::geom_line(data = ribdat,
        mapping = ggplot2::aes(x = x, y = y),
        size = .75, color = "black"
      )
    
    if (points) {
      p <- p + ggplot2::geom_point(data = ggdat, size = 2,
        position = ggplot2::position_jitter(height = 0.03, width = 0),
        color = "dark grey", alpha = 0.3)
    }
  }
  
  # We've got our plot, time for some theming!
  # First, define custom y and x-axes to draw
  custom_y_axis <- function() {
    d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
    list(
      ggplot2::geom_segment(data = d,
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE, size = 1),
      ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1))
    )
  }
  
  custom_x_axis <- function(ribdat) {
    l <- NULL
    xdat <- ribdat[["x"]]
    if (attr(ribdat, "factor"))
      l <- list(ggplot2::scale_x_discrete(labels = levels(xdat)))
    else {
      b <- pretty(xdat)
      d <- data.frame(y = -Inf, yend = -Inf, x = min(b), xend = max(b))
      l <- list(
        ggplot2::geom_segment(data = d,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE, size = 1 ),
        ggplot2::scale_x_continuous(breaks = b)
      )
    }
  }
  
  # then perform the theme and return the ggplot object
  p <- JASPgraphs::themeJasp(p, legend.position = "none")
  
  p <- p + ggplot2::xlab(pred) + ggplot2::ylab(ytitle) +
    custom_x_axis(ribdat) + custom_y_axis() 
  return(p)
}

.reglogisticResidPlotFill <- function(options, glmModel, var = NULL){
  # plots residuals against predicted (var = NULL) or predictor (var = "name")
  type <- options$residualType
  if (!is.null(var))
    ggdat <- data.frame(resid = residuals(glmModel, type = type),
                        x = glmModel[["data"]][[.v(var)]])
  else {
    ggdat <- data.frame(resid = residuals(glmModel, type = type),
                        x = predict(glmModel, type = "response"))
    var <- gettext("Predicted Probability")
  }
  
  if (class(ggdat[["x"]]) == "factor")
    pos <- ggplot2::position_jitter(width = 0.1)
  else
    pos <- ggplot2::position_jitter(width = 0)
  
  custom_y_axis <- function(val) {
    d <- data.frame(x = -Inf, xend = -Inf,
                    y = min(pretty(val)), yend = max(pretty(val)))
    list(
      ggplot2::geom_segment(data = d,
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE, size = 1),
      ggplot2::scale_y_continuous(breaks = pretty(val))
    )
  }
  
  custom_x_axis <- function(val) {
    if (class(val) == "factor")
      l <- list(ggplot2::scale_x_discrete(labels = levels(val)))
    else {
      d <- data.frame(y = -Inf, yend = -Inf,
                      x = min(pretty(val)), xend = max(pretty(val)))
      l <- list(
        ggplot2::geom_segment(data = d,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE, size = 1),
        ggplot2::scale_x_continuous(breaks = pretty(val))
      )
    }
    return(l)
  }
  
  p <- ggplot2::ggplot(data = ggdat,
                       mapping = ggplot2::aes(x = x, y = resid)) +
    ggplot2::geom_point(position = pos, size = 3, 
                        colour = "black", fill = "grey", pch = 21)
  
  p <- p +
    ggplot2::xlab(var) + ggplot2::ylab(gettext("Residuals")) +
    ggplot2::theme_bw() +
    custom_y_axis(ggdat[["resid"]]) + custom_x_axis(ggdat[["x"]]) 
  
  p <- JASPgraphs::themeJasp(p, legend.position = "none")
  
  return(p)
}

.reglogisticSquaredPearsonResidualsFill <- function(glmModel){
  # Squared Pearson residuals plot courtesy of Dan Gillen (UC Irvine)
  plotDat <- data.frame("pres" = residuals(glmModel, type = "pearson")^2,
                        "prob" = predict(glmModel, type = "response"))
  
  custom_y_axis <- function(ydat) {
    b <- pretty(c(ydat,0))
    d <- data.frame(y = 0, yend = max(b), x = -Inf, xend = -Inf)
    l <- list(ggplot2::geom_segment(data = d,
                                    ggplot2::aes(x = x, y = y, xend = xend,
                                                 yend = yend),
                                    inherit.aes = FALSE, size = 1),
              ggplot2::scale_y_continuous(breaks = b))
  }
  
  custom_x_axis <- function() {
    d <- data.frame(y = -Inf, yend = -Inf, x = 0, xend = 1)
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, 
                                                      xend = xend,
                                                      yend = yend),
                               inherit.aes = FALSE, size = 1),
         ggplot2::scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)))
  }
  
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = prob, y = pres), 
                       data = plotDat) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 1),
                          linetype = 3, size = 1, colour = "grey") +
    ggplot2::geom_smooth(se = FALSE, method = "loess", size = 1.2,
                         colour = "darkred") +
    ggplot2::geom_point(size = 3, colour="black", fill = "grey", pch=21) +
    custom_y_axis(plotDat[["pres"]]) + custom_x_axis() +
    ggplot2::labs(x = gettext("Predicted Probability"), y = gettext("Squared Pearson Residual"))
  
  p <- JASPgraphs::themeJasp(p, legend.position = "none")
  
  return(p)
}

.reglogisticEstimatesInfo <- function(options, addBCA = FALSE) {
  # so we only translate this once
  ciTitle <- if (addBCA)
    gettextf("%1$s%% bca\u002A Confidence interval", 100 * options$coeffCIInterval)
  else
    gettextf("%s%% Confidence interval", 100 * options$coeffCIInterval)

  if(options$coeffCIOR)
    ciTitle <- gettextf("%s <br> (odds ratio scale)", ciTitle)

  seTitle <- gettext("Standard Error")
  if (options$robustSEOpt)
    seTitle <- gettextf("Robust <br> %s", seTitle)

  if (options$method == "enter") {
    multimod   <- FALSE
    paramtitle <- ""
  } else {
    multimod   <- TRUE
    paramtitle <- gettext("Parameter")
  }
  return(list(ciTitle = ciTitle, seTitle = seTitle, multimod = multimod, paramtitle = paramtitle))
}
