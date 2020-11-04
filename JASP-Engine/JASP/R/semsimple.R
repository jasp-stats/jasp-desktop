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

SEMSimple <- function(jaspResults, dataset = NULL, options) {
  
  dheader <- .readDataSetHeader(all.columns = TRUE)
  availableVars <- colnames(dheader)
  variables <- .getUsedVars(options$model, availableVars)
  
  ready <- length(variables) > 1 && length(dheader) > 1 && options$model != ""
  
  if (options$groupingVariable != "")
    variables <- unique(c(variables, options$groupingVariable))
  
  if (is.null(dataset)) {
    if (ready && options$Data != "varcov") {
      dataset <- .readDataSetToEnd(columns = variables)
    } else if (ready && options$Data == "varcov") {
      dataset <- .readDataSetToEnd(all.columns = TRUE)
    } else {
      dataset <- dheader
    }
  }
  
  semContainer <- .getSemContainer(jaspResults)
  semContainer[["model"]] <- createJaspState(object = .translateModel(options$model, variables))
  
  semErrorCheck <- .semCheckErrors(dataset, options, ready, semContainer)
  
  if (options$Data == "varcov") 
    options$includeMeanStructure <- FALSE
  
  .getSemResult(semContainer, options, dataset, ready)
  
  .semMainTable(semContainer, options, dataset, ready, position = 1)
  
  .semEstimatesTable(semContainer, options, dataset, ready, variables, position = 2)
  
  .semAdditionalFitMeasuresTables(semContainer, options, dataset, ready, position = 3)
  
  .semRSquaredTable(semContainer, options, dataset, ready, position = 4)
  
  .semCovCorTable(semContainer, options, dataset, ready, position = 5)
  
  .semModIndicesTable(semContainer, options, dataset, ready, position = 6)
  
  .semMardiasCoefficientTable(semContainer, options, dataset, ready, position = 7)

  .lavCreatePathDiagram(semContainer, options, ready, position = 8)
  
  return()  
}

.getSemContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["semContainer"]])) {
    semContainer <- jaspResults[["semContainer"]]
  } else {
    # semContainer <- createJaspContainer(title = "Structural Equation Modeling<br/><span style='color:#888888;font-family:monospace;font-size:12px;font-weight:normal;'>Powered by lavaan.org</span>")
    semContainer <- createJaspContainer()
    
    semContainer$dependOn(c("model",  "model", "SampleSize", "errorCalculationBootstrapSamples", 
                            "groupingVariable", "eq_loadings", "eq_intercepts", "eq_residuals", "eq_residualcovariances",
                            "eq_means", "eq_thresholds", "eq_regressions", "eq_variances", "eq_lvcovariances", 
                            "includeMeanStructure", "assumeFactorsUncorrelated", "fixExogenousCovariates", 
                            "factorStandardisation", "fixManifestInterceptsToZero", "fixLatentInterceptsToZero",
                            "omitResidualSingleIndicator", "residualVariances", "correlateExogenousLatents", 
                            "addThresholds", "addScalingParameters", "correlateDependentVariables",
                            "Data", "errorCalculation", "estimator", "emulation"))
    jaspResults[["semContainer"]] <- semContainer
  }
  return(semContainer)
}

.getSemResult <- function(semContainer, options, dataset, ready) {
  if (!ready  || !is.null(semContainer[["semResultsList"]]))
    return()
  
  groupVar <- NULL
  
  if (.v(options$groupingVariable) %in% names(dataset)) 
    groupVar <- .v(options$groupingVariable)
  
  # group equal:
  groupEqual <- ""
  if (isTRUE(options$eq_loadings)) {
    groupEqual <- c(groupEqual, "loadings")
  }
  
  if (isTRUE(options$eq_intercepts)) {
    groupEqual <- c(groupEqual, "intercepts")
  }
  
  if (isTRUE(options$eq_means)) {
    groupEqual <- c(groupEqual, "means")
  }
  
  if (isTRUE(options$eq_thresholds)) {
    groupEqual <- c(groupEqual, "thresholds")
  }
  
  if (isTRUE(options$eq_regressions)) {
    groupEqual <- c(groupEqual, "regressions")
  }
  
  if (isTRUE(options$eq_residuals)) {
    groupEqual <- c(groupEqual, "residuals")
  }
  
  if (isTRUE(options$eq_residualcovariances)) {
    groupEqual <- c(groupEqual, "residual.covariances")
  }
  
  if (isTRUE(options$eq_variances)) {
    groupEqual <- c(groupEqual, "lv.variances")
  }
  
  if (isTRUE(options$eq_lvcovariances)) {
    groupEqual <- c(groupEqual, "lv.covariances")
  }
  
  if (length(groupEqual)>1) {
    groupEqual <- groupEqual[groupEqual!=""]
  }
  
  # Mean structure:
  if (!isTRUE(options$includeMeanStructure)) {
    options$includeMeanStructure <- "default"
  }

  ### RUN SEM ###
  semResults <- lavModel <- NULL
  if (ready) {
    # Raw data:
    if (options$Data == "raw"){
      semResults <- try(
        lavaan:::lavaan(model=semContainer[["model"]]$object, data=dataset,
                        auto.delta=options$addScalingParameters,
                        auto.th=options$addThresholds,
                        orthogonal=options$assumeFactorsUncorrelated,
                        auto.cov.y=options$correlateDependentVariables,
                        auto.cov.lv.x=options$correlateExogenousLatents,
                        mimic=ifelse(options$emulation=="none","lavaan", options$emulation),
                        se=options$errorCalculation,
                        bootstrap=options$errorCalculationBootstrapSamples,
                        estimator=ifelse(options$estimator == "automatic", "default", options$estimator),
                        std.lv=options$factorStandardisation == "residualVariance",
                        auto.fix.first=options$factorStandardisation == "factorLoadings",
                        fixed.x=options$fixExogenousCovariates,
                        int.lv.free=!options$fixLatentInterceptsToZero,
                        int.ov.free=!options$fixManifestInterceptsToZero,
                        meanstructure=options$includeMeanStructure,
                        auto.fix.single=options$omitResidualSingleIndicator,
                        auto.var=options$residualVariances,
                        group = groupVar, group.equal = groupEqual)
      )
    } else {
      # Var-cov matrix 
      S <- as.matrix(dataset)
      rownames(S) <- colnames(S)

      semResults <- try(
        lavaan:::lavaan(model=semContainer[["model"]]$object, sample.cov=S,
                        sample.nobs=as.numeric(options$SampleSize),
                        auto.delta=options$addScalingParameters,
                        auto.th=options$addThresholds,
                        orthogonal=options$assumeFactorsUncorrelated,
                        auto.cov.y=options$correlateDependentVariables,
                        auto.cov.lv.x=options$correlateExogenousLatents,
                        mimic=ifelse(options$emulation=="none","lavaan",options$emulation),
                        se=options$errorCalculation,
                        bootstrap=options$errorCalculationBootstrapSamples,
                        estimator=ifelse(options$estimator=="automatic", "default", options$estimator),
                        std.lv=options$factorStandardisation=="residualVariance",
                        auto.fix.first=options$factorStandardisation == "factorLoadings",
                        fixed.x=options$fixExogenousCovariates,
                        int.lv.free = !options$fixLatentInterceptsToZero,
                        int.ov.free = !options$fixManifestInterceptsToZero,
                        meanstructure=options$includeMeanStructure,
                        auto.fix.single=options$omitResidualSingleIndicator,
                        auto.var=options$residualVariances, group=groupVar,
                        group.equal=groupEqual)
      )
    }

    # Check if worked:
    if (isTryError(semResults)) {
      errorMessage <- gsub(as.character(semResults), pattern = "\n", replacement = "")
      # Better Error messages:
      if (errorMessage == "Error in start.idx[i]:end.idx[i] : NA/NaN argument") {
        semContainer$setError(gettext("Model misspecified"))
      } else {
        semContainer$setError(errorMessage)
      }
      return()
    }
  } else {
    
    lavModel <- try(lavaan:::lavaanify(model=semContainer[["model"]]$object, auto.delta=options$addScalingParameters,
                                       auto.th=options$addThresholds,
                                       orthogonal=options$assumeFactorsUncorrelated,
                                       auto.cov.y=options$correlateDependentVariables,
                                       auto.cov.lv.x=options$correlateExogenousLatents,
                                       # mimic = ifelse(options$emulation=="none","lavaan",options$emulation),
                                       # se = options$errorCalculation,
                                       # bootstrap = options$errorCalculationBootstrapSamples,
                                       # estimator = ifelse(options$estimator == "automatic", "default", options$estimator),
                                       std.lv=options$factorStandardisation == "residualVariance",
                                       auto.fix.first=options$factorStandardisation == "factorLoadings",
                                       fixed.x=options$fixExogenousCovariates,
                                       int.lv.free = !options$fixLatentInterceptsToZero,
                                       int.ov.free = !options$fixManifestInterceptsToZero,
                                       meanstructure=options$includeMeanStructure,
                                       auto.fix.single=options$omitResidualSingleIndicator,
                                       auto.var=options$residualVariances,
                                       ngroups=length(unique(dataset[[groupVar]])),
                                       group.equal = groupEqual)
    )
    
    # Check if worked:
    if (isTryError(lavModel)) {
      errorMessage <- as.character(lavModel)
      # Better Error messages:
      if (errorMessage == "Error in start.idx[i]:end.idx[i] : NA/NaN argument\n") {
        semContainer$setError(gettext("Model misspecified"))
      } else {
        semContainer$setError(errorMessage)
      }
      return()
    } 
  }
  
  semContainer[["semResultsList"]] <- createJaspState(object = list(semResults = semResults, 
                                                                    lavModel = lavModel))
  return()
}

.semCheckErrors <- function(dataset, options, ready, semContainer) {
  if (!ready) return()
  
  if (options$Data == "varcov") {
    # Check if dataset is variance covariance matrix:
    .hasErrors(dataset, type = c("varCovMatrix", "infinity"),
               message='default', exitAnalysisIfErrors = TRUE)
  } else if (ncol(dataset) > 0) {
    .hasErrors(dataset, type = c("infinity"),
               message='default', exitAnalysisIfErrors = TRUE)
  } 
  
  # Check mean structure:
  if (options$Data == "varcov") {
    if (isTRUE(options$includeMeanStructure)) {
      semContainer$setError(gettext("Mean structure can not be included when data is variance-covariance matrix"))
      return()
    }
    
    options$includeMeanStructure <- FALSE
    
    if (options$SampleSize == 0) {
      semContainer$setError(gettext("Please set the sample size!"))
      return()
    }
    
    # Check for multiple groups:
    if (options$groupingVariable!="") {
      semContainer$setError(gettext("Multiple group analysis not (yet) supported when data is variance-covariance matrix"))
      return()
    }
    
  } else {
    if (ncol(dataset) > 0 && !nrow(dataset) > ncol(dataset)) {
      semContainer$setError(gettext("Not more cases than number of variables. Is your data a variance-covariance matrix?"))
      return()
    }
  }
}

.semMainTable <- function(semContainer, options, dataset, ready, position) {
  if (!is.null(semContainer[["semFitTable"]]))
    return()
  
  semFitTable <- createJaspTable(title = gettext("Chi Square Test Statistic (unscaled)"))
  semFitTable$position <- position
  
  semFitTable$addColumnInfo(name="Model",   title = "",                      type = "string")
  semFitTable$addColumnInfo(name="Df",      title = gettext("df"),           type = "number")
  semFitTable$addColumnInfo(name="AIC",     title = gettext("AIC"),          type = "number")
  semFitTable$addColumnInfo(name="BIC",     title = gettext("BIC"),          type = "number")
  semFitTable$addColumnInfo(name="Chisq",   title = gettext("&#967;&sup2;"), type = "number")
  semFitTable$addColumnInfo(name="PrChisq", title = gettext("p"),            type = "number")
  
  semFitTable$showSpecifiedColumnsOnly <- TRUE
  semFitTable$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")
  
  semContainer[["semFitTable"]] <- semFitTable

  if (is.null(semContainer[["semResultsList"]]$object$semResults) || !ready || semContainer$getError()) 
    return()
    
  # Current to saturated:
  fitTableResult <- lavaan::lavTestLRT(semContainer[["semResultsList"]]$object$semResults)[-1, ]
  rownames(fitTableResult) <- gettext("Model")
  colnames(fitTableResult) <- c("Df", "AIC", "BIC", "Chisq", "ChisqDiff", "DfDiff", "PrChisq")
  
  
  
  fitTableResult[["Model"]] <- rownames(fitTableResult)
  semFitTable$setData(fitTableResult)
  
  return()
}

.semEstimatesTable <- function(semContainer, options, dataset, ready, variables, position) {
  if (!is.null(semContainer[["semEstimatesTable"]]))
    return()
  
  semEstimatesTable <- createJaspTable(title = "Parameter Estimates")
  semEstimatesTable$position <- position
  
  semEstimatesTable$addColumnInfo(name = "lhs",      title = "",                    type = "string")
  semEstimatesTable$addColumnInfo(name = "op",       title = "",                    type = "string")
  semEstimatesTable$addColumnInfo(name = "rhs",      title = "",                    type = "string")
  semEstimatesTable$addColumnInfo(name = "label",    title = gettext("label"),      type = "string")
  semEstimatesTable$addColumnInfo(name = "est",      title = gettext("est"),        type = "number")
  semEstimatesTable$addColumnInfo(name = "se",       title = gettext("se"),         type = "number")
  semEstimatesTable$addColumnInfo(name = "z",        title = gettext("z"),          type = "number")
  semEstimatesTable$addColumnInfo(name = "pvalue",   title = gettext("p"),          type = "pvalue")
  semEstimatesTable$addColumnInfo(name = "ci.lower", title = gettext("CI (lower)"), type = "number")
  semEstimatesTable$addColumnInfo(name = "ci.upper", title = gettext("CI (upper)"), type = "number")
  semEstimatesTable$addColumnInfo(name = "std.lv",   title = gettext("std (lv)"),   type = "number")
  semEstimatesTable$addColumnInfo(name = "std.all",  title = gettext("std (all)"),  type = "number")
  semEstimatesTable$addColumnInfo(name = "std.nox",  title = gettext("std (nox)"),  type = "number")
  semEstimatesTable$addColumnInfo(name = "group",    title = gettext("group"),      type = "string")
  
  semEstimatesTable$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")
  semEstimatesTable$showSpecifiedColumnsOnly <- TRUE
  
  semContainer[["semEstimatesTable"]] <- semEstimatesTable
  
  if (!ready || semContainer$getError())
    return()

  semEstimates <- lavaan:::parameterEstimates(semContainer[["semResultsList"]]$object$semResults, standardized=TRUE)
  semEstimatesTable$setData(lapply(semEstimates, .unv))
      
  return()
}

.semAdditionalFitMeasuresTables <- function(semContainer, options, dataset, ready, position) {
  if (!options[["outputAdditionalFitMeasures"]] || !is.null(semContainer[["fitMeasuresModelTest"]]))
    return()
  
  fitMeasures <- createJaspContainer()
  fitMeasures$position <- position
  
  semContainer[["fitMeasures"]] <- fitMeasures
  
  ## Model test
  semModelTestTable <- createJaspTable(title = "Model test baseline model")
  
  semModelTestTable$addColumnInfo(name = "model",  title = "",                                         type = "string")
  semModelTestTable$addColumnInfo(name = "fmin",   title = gettext("Minimum Function Test Statistic"), type = "number")
  semModelTestTable$addColumnInfo(name = "chisq",  title = gettext("&#967;&sup2;"),                    type = "number")
  semModelTestTable$addColumnInfo(name = "df",     title = gettext("Degrees of freedom"),              type = "number")
  semModelTestTable$addColumnInfo(name = "pvalue", title = gettext("p"),                               type = "pvalue")
  
  semModelTestTable$transpose <- TRUE
  semModelTestTable$dependOn("outputAdditionalFitMeasures")
  fitMeasures[["fitMeasuresModelTest"]] <- semModelTestTable
  
  ### Baseline
  semBaselineTable <- createJaspTable(title = "User model versus baseline model")
  
  semBaselineTable$addColumnInfo(name = "model", title = "",                                                    type = "string")
  semBaselineTable$addColumnInfo(name = "cfi",   title = gettext("Comparative Fit Index (CFI)"),                type = "number")
  semBaselineTable$addColumnInfo(name = "tli",   title = gettext("Tucker-Lewis Index (TLI)"),                   type = "number")
  semBaselineTable$addColumnInfo(name = "nnfi",  title = gettext("Bentler-Bonett Non-normed Fit Index (NNFI)"), type = "number")
  semBaselineTable$addColumnInfo(name = "nfi",   title = gettext("Bentler-Bonett Normed Fit Index (NFI)"),      type = "number")
  semBaselineTable$addColumnInfo(name = "pnfi",  title = gettext("Parsimony Normed Fit Index (PNFI)"),          type = "number")
  semBaselineTable$addColumnInfo(name = "rfi",   title = gettext("Bollen's Relative Fit Index (RFI)"),          type = "number")
  semBaselineTable$addColumnInfo(name = "ifi",   title = gettext("Bollen's Incremental Fit Index (IFI)"),       type = "number")
  semBaselineTable$addColumnInfo(name = "rni",   title = gettext("Relative Noncentrality Index (RNI)"),         type = "number")
  
  semBaselineTable$transpose <- TRUE
  semBaselineTable$dependOn("outputAdditionalFitMeasures")
  fitMeasures[["fitMeasuresBaseline"]] <- semBaselineTable
  
  ### LogLik measures
  semLoglikTable <- createJaspTable(title = gettext("Loglikelihood and Information Criteria"))
  
  semLoglikTable$addColumnInfo(name="model",             title = "",                                               type = "string")
  semLoglikTable$addColumnInfo(name="logl",              title = gettext("Loglikelihood user model (H0)"),         type = "number")
  semLoglikTable$addColumnInfo(name="unrestricted.logl", title = gettext("Loglikelihood unrestricted model (H1)"), type = "number")
  semLoglikTable$addColumnInfo(name="npar",              title = gettext("Number of free parameters"),             type = "integer")
  semLoglikTable$addColumnInfo(name="aic",               title = gettext("Akaike (AIC)"),                          type = "number")
  semLoglikTable$addColumnInfo(name="bic",               title = gettext("Bayesian (BIC)"),                        type = "number")
  semLoglikTable$addColumnInfo(name="bic2",              title = gettext("Sample-size adjusted Bayesian (BIC)"),   type = "number")
  
  semLoglikTable$transpose <- TRUE
  semLoglikTable$dependOn("outputAdditionalFitMeasures")
  fitMeasures[["fitMeasuresLikelihood"]] <- semLoglikTable
  
  
  ### RMSEA measures
  semRMSEATable <- createJaspTable(title = gettext("Root Mean Square Error of Approximation"))
  
  semRMSEATable$addColumnInfo(name = "model",          title = "",                                type = "string")
  semRMSEATable$addColumnInfo(name = "rmsea",          title = gettext("RMSEA"),                  type = "number")
  semRMSEATable$addColumnInfo(name = "rmsea.ci.upper", title = gettextf("Upper 90%% CI"),           type = "number")
  semRMSEATable$addColumnInfo(name = "rmsea.ci.lower", title = gettextf("Lower 90%% CI"),           type = "number")
  semRMSEATable$addColumnInfo(name = "rmsea.pvalue",   title = gettext("p-value RMSEA <= 0.05 "), type = "pvalue")  
  
  semRMSEATable$transpose <- TRUE
  semRMSEATable$dependOn("outputAdditionalFitMeasures")
  fitMeasures[["fitMeasuresRMSEA"]] <- semRMSEATable
  
  
  ### RMR measures
  semRMRTable <- createJaspTable(title = "Standardized Root Mean Square Residual")
  
  semRMRTable$addColumnInfo(name = "model",      title = "",                       type = "string")
  semRMRTable$addColumnInfo(name = "rmr",        title = gettext("RMR"),           type = "number")
  semRMRTable$addColumnInfo(name = "rmr_nomean", title = gettext("RMR (No Mean)"), type = "number")
  semRMRTable$addColumnInfo(name = "srmr",       title = gettext("SRMR"),          type = "number")
  
  semRMRTable$transpose <- TRUE
  semRMRTable$dependOn("outputAdditionalFitMeasures")
  fitMeasures[["fitMeasuresRMR"]] <- semRMRTable
  
  ### Other fit measures
  semOtherFitTable <- createJaspTable(title = gettext("Other Fit Indices"))
  
  semOtherFitTable$addColumnInfo(name = "model", title = "",                                               type = "string")
  semOtherFitTable$addColumnInfo(name = "cn_05", title = gettext("Hoelter Critical N (CN) alpha=0.05"),    type = "number")
  semOtherFitTable$addColumnInfo(name = "cn_01", title = gettext("Hoelter Critical N (CN) alpha=0.01"),    type = "number")
  semOtherFitTable$addColumnInfo(name = "gfi",   title = gettext("Goodness of Fit Index (GFI)"),           type = "number")
  semOtherFitTable$addColumnInfo(name = "agfi",  title = gettext("Parsimony Goodness of Fit Index (GFI)"), type = "number")
  semOtherFitTable$addColumnInfo(name = "mfi",   title = gettext("McDonald Fit Index (MFI)"),              type = "number")
  # semOtherFitTable$addColumnInfo(name="ecvi", title="Expected Cross-Validation Index (ECVI)", type="number")
  
  semOtherFitTable$transpose <- TRUE
  semOtherFitTable$dependOn("outputAdditionalFitMeasures")
  fitMeasures[["fitMeasuresOther"]] <- semOtherFitTable
  
  if (!ready || semContainer$getError())
    return()
  
  semFitMeasures <- unlist(lavaan:::fitMeasures(semContainer[["semResultsList"]]$object$semResults))
  
  modl <- list(model = gettext("Model"))
  semModelTestTable$setData(c(modl, semFitMeasures[c('fmin', 'chisq', 'df', 'pvalue')]))
  semBaselineTable$setData( c(modl, semFitMeasures[c('cfi', 'tli', 'nnfi', 'nfi', 'pnfi', 'rfi', 'ifi', 'rni')]))
  semLoglikTable$setData(   c(modl, semFitMeasures[c('logl', 'unrestricted.logl', 'npar', 'aic', 'bic', 'bic2')]))
  semRMSEATable$setData(    c(modl, semFitMeasures[c('rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'rmsea.pvalue')]))
  semRMRTable$setData(      c(modl, semFitMeasures[c('rmr', 'rmr_nomean', 'srmr')]))
  semOtherFitTable$setData( c(modl, semFitMeasures[c('cn_05', 'cn_01', 'gfi', 'agfi', 'mfi')]))
  
  return()
}

.semRSquaredTable <- function(semContainer, options, dataset, ready, position) {
  if (!options[["outputRSquared"]] || !is.null(semContainer[["semRSquaredTable"]]))
    return()
  
  semRSquaredTable <- createJaspTable(title = gettext("R-Squared"))
  semRSquaredTable$position <- position
  
  semRSquaredTable$addColumnInfo(name = "var", title = gettext("Variable"), type = "string")
  semRSquaredTable$dependOn("outputRSquared")
  
  semContainer[["rSquaredTable"]] <- semRSquaredTable
  
  if (!ready || semContainer$getError())
    return()
  
  r2 <- lavaan::inspect(semContainer[["semResultsList"]]$object$semResults, "r2")
  nm <- names(r2)

  if (options$groupingVariable == "") {
    semRSquaredTable$addColumnInfo(name = "R2",  title = gettext("R&sup2;"),  type = "number")
    semRSquaredTable$addRows(data.frame(var = .unv(names(r2)), R2 = r2))
    
  } else {
    
    semRSquaredTable$addColumns(data.frame(var = .unv(names(r2[[1]]))))
                                
    for (i in 1:length(r2)) {
      semRSquaredTable$addColumnInfo(name = paste0("R2", i), title = gettextf("Group %s", nm[i]), 
                                     overtitle = gettext("R&sup2;"), type = "number")
      thisCol <- data.frame(rr = r2[[i]])
      names(thisCol) <- paste0("R2", i)
      semRSquaredTable$addColumns(cols = thisCol)
    }
  }

  return()
}

.semModIndicesTable <- function(semContainer, options, dataset, ready, position) {
  if (!options[["outputModificationIndices"]] || !is.null(semContainer[["semModIndicesTable"]]))
    return()
  
  semModIndicesTable <- createJaspTable(title = gettext("Modification Indices"))
  semModIndicesTable$position <- position
  
  semModIndicesTable$addColumnInfo(name = "lhs",       title = "",                    type = "string")
  semModIndicesTable$addColumnInfo(name = "op",        title = "",                    type = "string")
  semModIndicesTable$addColumnInfo(name = "rhs",       title = "",                    type = "string")
  semModIndicesTable$addColumnInfo(name = "mi",        title = gettext("mi"),         type = "number")
  semModIndicesTable$addColumnInfo(name = "epc",       title = gettext("epc"),        type = "number")
  semModIndicesTable$addColumnInfo(name = "sepc.lv",   title = gettext("sepc (lv)"),  type = "number")
  semModIndicesTable$addColumnInfo(name = "sepc.all",  title = gettext("sepc (all)"), type = "number")
  semModIndicesTable$addColumnInfo(name = "sepc.nox",  title = gettext("sepc (nox)"), type = "number")
  
  semModIndicesTable$dependOn(c("outputModificationIndices", "outputModificationIndicesHideLowIndices", 
                                "outputModificationIndicesHideLowIndicesThreshold"))
  semModIndicesTable$showSpecifiedColumnsOnly <- TRUE
  
  semContainer[["semModIndicesTable"]] <- semModIndicesTable
  
  if (!ready || semContainer$getError())
    return()
  
  # Extract modidffication indices:
  semModIndResult <- lavaan:::modificationIndices(semContainer[["semResultsList"]]$object$semResults)
  
  ### Remove NA:
  semModIndResult <- semModIndResult[!is.na(semModIndResult$mi), , drop=FALSE]
  
  ## Sort:
  semModIndResult <- semModIndResult[order(semModIndResult$mi, decreasing=TRUE), , drop=FALSE]
  
  ### Remove low indices:
  if (isTRUE(options$outputModificationIndicesHideLowIndices)) {
    semModIndResult <- semModIndResult[semModIndResult$mi > options$outputModificationIndicesHideLowIndicesThreshold, , drop=FALSE]
  }
  
  semModIndicesTable$setData(lapply(semModIndResult, .unv))
  
  return()
}

.semCovCorTable <- function(semContainer, options, dataset, ready, position) {
  if (!(options$outputObservedCovarianceCorrelations || options$outputFittedCovarianceCorrelations || 
                  options$outputResidualCovarianceCorrelations) || !is.null(semContainer[["semCovCorCollection"]]))
    return()
 
  semCovCorCollection <- createJaspContainer()
  semCovCorCollection$position <- position
  semContainer[["semCovCorCollection"]] <- semCovCorCollection
  
  semCovCorCollection$dependOn(c("outputFittedCovarianceCorrelations", "outputObservedCovarianceCorrelations", 
                                 "outputResidualCovarianceCorrelations"))
  
  if (!ready || semContainer$getError()) 
    return()
    
  include <- c("observed", "fitted", "residual")[ c(options$outputObservedCovarianceCorrelations, 
                                                    options$outputFittedCovarianceCorrelations,
                                                    options$outputResidualCovarianceCorrelations)]

  mainTitle <- gettext("Covariances (lower triangle) / correlations (upper triangle)")
  nm <- ""
  if (options$groupingVariable != "") {
    semCovCorCollection$title <- mainTitle
    nm <- names(lavaan::inspect(semContainer[["semResultsList"]]$object$semResults, "sampstat"))
  }
  
  for (thisGroup in 1:length(nm)) {
    
    if (options$groupingVariable == "") {
      observedCov <- lavaan::inspect(semContainer[["semResultsList"]]$object$semResults, "sampstat")$cov
      fittedCov <- lavaan::fitted(semContainer[["semResultsList"]]$object$semResults)$cov
    } else {
      observedCov <- lavaan::inspect(semContainer[["semResultsList"]]$object$semResults, "sampstat")[[thisGroup]]$cov
      fittedCov <- lavaan::fitted(semContainer[["semResultsList"]]$object$semResults)[[thisGroup]]$cov
    }
    
    residualCov <- observedCov - fittedCov
    
    varNames <- colnames(observedCov)
    
    covList <- list(observed=observedCov,
                    fitted=fittedCov,
                    residual=residualCov)
    
    corList <- list(observed=stats::cov2cor(observedCov),
                    fitted=stats::cov2cor(fittedCov),
                    residual=stats::cov2cor(observedCov) - stats::cov2cor(fittedCov))
    
    n <- ncol(covList[[1]])
    
    matList <- mapply(cov = covList, cor = corList, type = names(covList), FUN=function(cov, cor, type){
      cov[upper.tri(cov,diag=FALSE)] <- cor[upper.tri(cor,diag=FALSE)]
      cbind(..sortingDummy = seq_len(NROW(cov)), ..varName = .unv(rownames(cov)), ..type = type, as.data.frame(cov), stringsAsFactors = FALSE)
    }, SIMPLIFY = FALSE)
    
    matDF <- do.call(rbind, matList)
    matDF <- matDF[matDF$..type %in% include, ]
    matDF$..type <- as.character(matDF$..type)
    matDF <- matDF[order(matDF$..sortingDummy), ]
    matDF <- matDF[, -1]
    
    if (options$groupingVariable == "")
      thisTitle <- mainTitle
    else
      thisTitle <- paste0("Level of ", options$groupingVariable, ": ", nm[thisGroup])
    
    semCovCorCollection[[paste0("semCovCorTable", thisGroup)]] <- createJaspTable(title = thisTitle)
    semCovCorCollection[[paste0("semCovCorTable", thisGroup)]]$addColumnInfo(name="Variable", title="", type="string", 
                                                                             combine = TRUE)
    semCovCorCollection[[paste0("semCovCorTable", thisGroup)]]$addColumnInfo(name="Type", title="", type="string")
    
    for (i in 1:n)
      semCovCorCollection[[paste0("semCovCorTable", thisGroup)]]$addColumnInfo(name=varNames[i], title=.unv(varNames[i]), type="number")

    names(matDF)[1:2] <- c("Variable", "Type")
    semCovCorCollection[[paste0("semCovCorTable", thisGroup)]]$setData(matDF)
  }

  return()
}

.semMardiasCoefficientTable <- function(semContainer, options, dataset, ready, position) {
  if (!options[["outputMardiasCoefficients"]] || !is.null(semContainer[["semMardiasTable"]]))
    return()
  
  semMardiasTable <- createJaspTable(title = gettext("Mardia's coefficients"))
  semMardiasTable$position <- position
  
  semMardiasTable$addColumnInfo(name = "Type",        title = "",                      type = "string")
  semMardiasTable$addColumnInfo(name = "Coefficient", title = gettext("Coefficient"),  type = "number")
  semMardiasTable$addColumnInfo(name = "z",           title = gettext("z"),            type = "number")
  semMardiasTable$addColumnInfo(name = "Chisq",       title = gettext("&#967;&sup2;"), type = "number")
  semMardiasTable$addColumnInfo(name = "DF",          title = gettext("df"),           type = "number")
  semMardiasTable$addColumnInfo(name = "pvalue",      title = gettext("p"),            type = "pvalue")
  
  semMardiasTable$dependOn("outputMardiasCoefficients")
  semContainer[["mardiasTable"]] <- semMardiasTable
  
  if (!ready || semContainer$getError())
    return()
  
  varNames <- lavaan::lavaanNames(semContainer[["semResultsList"]]$object$semResults, type="ov")
  
  if (!all(sapply(dataset[, varNames, drop = FALSE], is.numeric))) {
    semMardiasTable$setError(gettext("Not all used variables are numeric. Mardia's coefficients not available."))
    return()
  }
  
  mardiaSkew <- unname(semTools:::mardiaSkew(dataset[, varNames]))
  mardiaKurtosis <- unname(semTools:::mardiaKurtosis(dataset[, varNames]))
  semMardiasTable$addRows(data.frame(Type=gettext("Skewness"), Coefficient=mardiaSkew[1], z=NA, Chisq=mardiaSkew[2], 
                                     DF=mardiaSkew[3], "pvalue"=mardiaSkew[4]))
  semMardiasTable$addRows(data.frame(Type=gettext("Kurtosis"), Coefficient=mardiaKurtosis[1], z=mardiaKurtosis[2], 
                                     Chisq=NA, DF=NA, "pvalue"=mardiaKurtosis[3]))
  
  
  return()
}

.is.raw.letter <- function(ch) {
  (ch >= 0x61 && ch <= 0x7A) || (ch >= 0x41 && ch <= 0x5A)
}

.is.alpha.numeric <- function(ch) {
  (ch >= 0x61 && ch <= 0x7A) || (ch >= 0x41 && ch <= 0x5A) || (ch >= 0x30 && ch <= 0x39)
}

.extractVariables <- function(model) {
  reserved.words <- c("c", "start", "equal", "NA")
  
  bytes <- c(charToRaw(model), 0)
  
  variables <- c()
  
  none <- 0
  in.double.quote <- 1
  in.single.quote <- 2
  in.unquoted <- 3
  in.comment <- 4
  
  parse.state <- none
  token.start <- 1
  
  sq <- charToRaw("'")
  dq <- charToRaw('"')
  hash <- charToRaw('#')
  nl <- charToRaw('\n')
  
  i <- 1
  while (i <= length(bytes)) {
    ch <- bytes[i]
    
    if (parse.state == none) {
      
      if (.is.raw.letter(ch)) {
        token.start <- i
        parse.state <- in.unquoted
      } else if (ch == sq) {
        token.start <- i
        parse.state <- in.single.quote
      } else if (ch == dq) {
        token.start <- i
        parse.state <- in.double.quote
      } else if (ch == hash) {
        parse.state <- in.comment
      }
    } else if (parse.state == in.single.quote) {
      if (ch == sq) {
        variable <- substr(model, token.start, i)
        variables <- c(variables, variable)
        parse.state <- none
      }
    } else if (parse.state == in.double.quote) {
      if (ch == dq) {
        variable <- substr(model, token.start, i)
        variables <- c(variables, variable)
        parse.state <- none
      }
    } else if (parse.state == in.unquoted) {
      if (.is.alpha.numeric(ch) == FALSE) {
        variable <- substr(model, token.start, i - 1)
        variables <- c(variables, variable)
        parse.state <- none
        i <- i - 1
      }
    } else if (parse.state == in.comment) {
      if (ch == nl) {
        parse.state <- none
      }
    }
    
    i <- i + 1
  }
  
  variables <- unique(variables)
  
  if (length(variables) > 0) {
    for (i in 1:length(variables)) {
      variable <- variables[i]
      if ((regexpr("'.*'", variable) == 1) ||
          (regexpr("\".*\"", variable) == 1)) {
        variable <- substr(variable, 2, nchar(variable) - 1)
      }
      variables[i] <- variable
    }
  }
  
  variables <- variables[ ! (variables %in% reserved.words)]
  return(variables)
}

.translateModel <- function(model, variables) {
  if (length(variables) == 0) {
    return(model)
  }
  
  variables <- variables[order(nchar(variables), decreasing = TRUE)]
  with.s.quotes <- paste("\\b'", variables, "'\\b", sep="")
  with.d.quotes <- paste('\\b"', variables, '"\\b', sep="")
  
  new.names <- .v(variables)
  
  for (i in 1:length(variables)) {
    model <- gsub(with.d.quotes[i], new.names[i], model)
  }
  
  for (i in 1:length(variables)) {
    model <- gsub(with.s.quotes[i], new.names[i], model)
  }
  
  for (i in 1:length(variables)) {
    model <- gsub(paste0("\\b",variables[i], "\\b"), new.names[i], model)
  }
  
  return(model)
}

.getUsedVars <- function(model, vars) {
  vv <- .unv(vars)
  findpattern <- paste0("(?<=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|^)\\Q",
                        vv,
                        "\\E(?=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|$)")
  return(vv[vapply(findpattern,
                   function(p) stringr::str_detect(model, p),
                   FUN.VALUE = TRUE,
                   USE.NAMES = FALSE)])
}

.decodeVarsInMessage <- function(encodedVars, message) {
  if (length(encodedVars) == 0 || !is.character(encodedVars) || !is.character(message))
    return(message)
  
  decodedVars <- .unv(encodedVars)
  names(decodedVars) <- encodedVars
  stringr::str_replace_all(message, decodedVars)
}

.lavCreatePathDiagram <- function(semContainer, options, ready, position) {
  if (!is.null(semContainer[["pathDiagramPlotCollection"]]) || !options[["addPathDiagram"]])
    return()
  
  pathDiagramPlotCollection <- createJaspContainer()
  pathDiagramPlotCollection$position <- position
  
  pathDiagramPlotCollection$dependOn(c("addPathDiagram", "outputpathdiagramstandardizedparameter"))
  
  semContainer[["pathDiagramPlotCollection"]] <- pathDiagramPlotCollection
  
  if (!ready || semContainer$getError()) {
    pathDiagramPlotCollection[["placeHolderPathDiagram"]] <- createJaspPlot(title = "Path Diagram")
    return()
  }
  
  plotArgs <- list(
    DoNotPlot = TRUE,
    ask = FALSE,
    layout = "tree",
    color = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    border.width = 1.5,
    edge.label.cex = 0.9,
    lty = 2,
    title = FALSE
  )

  p <- try({
    if (!is.null(semContainer[["semResultsList"]]$object$semResults)) {
      semPlotModel <- .lavToPlotObj(semContainer[["semResultsList"]]$object$semResults)
      .suppressGrDevice(do.call(semPlot::semPaths,
                                c(plotArgs, list(object = semPlotModel, what = ifelse(options$outputpathdiagramstandardizedparameter, "std", "paths")))
      ))
    } else {
      semPlotModel <- .lavToPlotObj(semContainer[["semResultsList"]]$object$lavModel)
      .suppressGrDevice(do.call(semPlot::semPaths,
                                c(plotArgs, list(object = semPlotModel, what = "par", edge.color = "black"))
      ))
    }
  })
  
  if (isTryError(p)) {
    errorMessage <- .extractErrorMessage(p)
    pathDiagramPlotCollection[["placeHolderPathDiagram"]] <- createJaspPlot(title = gettext("Path Diagram"))
    pathDiagramPlotCollection[["placeHolderPathDiagram"]]$setError(errorMessage)
    return()
  }
  
  if (options$groupingVariable != "") {
    pathDiagramPlotCollection$title <- gettext("Path Diagrams")
    
    # semplot returns an unnamed list, but which plot belongs to which grouping level?
    # The order should match the order of the levels in the lavaan model..
    titles <- semPlotModel@Original[[1]]@Data@group.label
    titles <- paste0("Path Diagram - level of ", options$groupingVariable, ": ", titles)
    
    if (!is.character(titles) || (length(titles) != length(p)))
      titles <- seq_along(p)
    
    plotList <- list()
    keep <- NULL
    for (i in seq_along(p)) {
      .lavWritePathDiagram(p[[i]], titles[i], options, pathDiagramPlotCollection)
    }
    
  } else {
    .lavWritePathDiagram(p, gettext("Path Diagram"), options, pathDiagramPlotCollection)
  }
    
  return()
}

.lavWritePathDiagram <- function(plotObj, title, options, pathDiagramPlotCollection) {
  width <- options$plotWidth
  height <- options$plotHeight
  if (height == 0)
    height <- 1 + 299 * (length(options$variables) / 5)

  pathDiagramPlotCollection[[paste0("pathPlot", title)]] <- createJaspPlot(width = width, 
                         height = height, plot = plotObj, title = title)
  
  return()
}

.lavToPlotObj <- function(lavResult) {
  # Create semplot model and decode the names of the manifest variables
  # Sorry, this code is really ugly but all it does is replace names for plot.
  semPlotMod <- semPlot::semPlotModel(list(lavResult), list(mplusStd = "std"))[[1]]
  
  manifests <- semPlotMod@Vars$name[semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[semPlotMod@Vars$manifest] <- decodeColNames(manifests)
  
  lhsAreManifest <- semPlotMod@Pars$lhs %in% manifests
  if (any(lhsAreManifest)) semPlotMod@Pars$lhs[lhsAreManifest] <- decodeColNames(semPlotMod@Pars$lhs[lhsAreManifest])
  
  rhsAreManifest <- semPlotMod@Pars$rhs %in% manifests
  if (any(rhsAreManifest)) semPlotMod@Pars$rhs[rhsAreManifest] <- decodeColNames(semPlotMod@Pars$rhs[rhsAreManifest])
  
  return(semPlotMod)
}

