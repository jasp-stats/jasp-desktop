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
    variables <- c(variables, options$groupingVariable)
  
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
  
  .semFitTable(semContainer, options, dataset, ready)
  
  .semEstimatesTable(semContainer, options, dataset, ready, variables)
  
  .semModIndicesTable(semContainer, options, dataset, ready)
  
  .semFitMeasuresTables(semContainer, options, dataset, ready)
  
  .semRSquaredTable(semContainer, options, dataset, ready)
  
  .semCovCorTable(semContainer, options, dataset, ready)
  
  .semMardiasCoefficientTable(semContainer, options, dataset, ready)

  .lavCreatePathDiagram(semContainer, options, ready)
  
  return()  
}

.getSemContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["semContainer"]])) {
    semContainer <- jaspResults[["semContainer"]]
  } else {
    semContainer <- createJaspContainer(title = "Structural Equation Modeling<br/><span style='color:#888888;font-family:monospace;font-size:12px;font-weight:normal;'>Powered by lavaan.org</span>")
    semContainer$dependOn(c("model"))
    jaspResults[["semContainer"]] <- semContainer
  }
  return(semContainer)
}

.getSemResult <- function(semContainer, options, dataset, ready) {
  if (!ready)
    return()
  
  groupVar <- NULL
  
  if (.v(options$groupingVariable) %in% names(dataset)) {
    groupVar <- .v(options$groupingVariable)
    # TODO (Sacha): No print I presume (Alexander)
    #print(class(dataset[[ groupVar ]]))
  }
  
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
      semResults <- try(
        lavaan:::lavaan(model=semContainer[["model"]]$object, sample.cov=as.matrix(dataset),
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
      errorMessage <- as.character(semResults)
      # Better Error messages:
      if (errorMessage == "Error in start.idx[i]:end.idx[i] : NA/NaN argument\n") {
        semContainer$setError("Model misspecified")
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
        semContainer$setError("Model misspecified")
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
      semContainer$setError("Mean structure can not be included when data is variance-covariance matrix")
      return()
    }
    
    options$includeMeanStructure <- FALSE
    
    if (options$SampleSize == 0) {
      semContainer$setError("Please set the sample size!")
      return()
    }
    
    # Check for multiple groups:
    if (options$groupingVariable!="") {
      semContainer$setError("Multiple group analysis not (yet) supported when data is variance-covariance matrix")
      return()
    }
    
  } else {
    if (ncol(dataset) > 0 && !nrow(dataset) > ncol(dataset)) {
      semContainer$setError("Not more cases than number of variables. Is your data a variance-covariance matrix?")
      return()
    }
  }
}

.semFitMeasuresTables <- function(semContainer, options, dataset, ready) {
  if (!ready)
    return()
  
  semFitMeasures <- unlist(lavaan:::fitMeasures(semContainer[["semResultsList"]]$object$semResults))
  
  ## Model test
  semModelTestTable <- createJaspTable(title = "Model test baseline model")
  
  semModelTestTable$addColumnInfo(name="model", title="", type="string")
  semModelTestTable$addColumnInfo(name="fmin", title="Minimum Function Test Statistic", type="number")
  semModelTestTable$addColumnInfo(name="chisq", title="&#967;&sup2;", type="number")
  semModelTestTable$addColumnInfo(name="df", title="Degrees of freedom", type="number")
  semModelTestTable$addColumnInfo(name="pvalue", title="p", type="number")
  
  semModelTestTable$setData(c(list(model="Model"), semFitMeasures[c('fmin', 'chisq', 'df', 'pvalue')]))
  
  semContainer[["fitMeasuresModelTest"]] <- semModelTestTable
  
  
  ### Baseline
  semBaselineTable <- createJaspTable(title = "User model versus baseline model")
  
  semBaselineTable$addColumnInfo(name="model", title="", type="string")
  semBaselineTable$addColumnInfo(name="cfi", title="Comparative Fit Index (CFI)", type="number")
  semBaselineTable$addColumnInfo(name="tli", title="Tucker-Lewis Index (TLI)", type="number")
  semBaselineTable$addColumnInfo(name="nnfi", title="Bentler-Bonett Non-normed Fit Index (NNFI)", type="number")
  semBaselineTable$addColumnInfo(name="nfi", title="Bentler-Bonett Normed Fit Index (NFI)", type="number")
  semBaselineTable$addColumnInfo(name="pnfi", title="Parsimony Normed Fit Index (PNFI)", type="number")
  semBaselineTable$addColumnInfo(name="rfi", title="Bollen's Relative Fit Index (RFI)", type="number")
  semBaselineTable$addColumnInfo(name="ifi", title="Bollen's Incremental Fit Index (IFI)", type="number")
  semBaselineTable$addColumnInfo(name="rni", title="Relative Noncentrality Index (RNI)", type="number")
  
  semBaselineTable$setData(c(list(model="Model"), semFitMeasures[c('cfi', 'tli', 'nnfi', 'nfi', 'pnfi', 'rfi', 'ifi', 
                                                                   'rni')]))
  
  semContainer[["fitMeasuresBaseline"]] <- semBaselineTable
  
  ### LogLik measures
  semLoglikTable <- createJaspTable(title = "Loglikelihood and Information Criteria")
  
  semLoglikTable$addColumnInfo(name="model", title="", type="string")
  semLoglikTable$addColumnInfo(name="logl", title="Loglikelihood user model (H0)", type="number")
  semLoglikTable$addColumnInfo(name="unrestricted.logl", title="Loglikelihood unrestricted model (H1)", type="number")
  semLoglikTable$addColumnInfo(name="npar", title="Number of free parameters", type="number", format="dp:0")
  semLoglikTable$addColumnInfo(name="aic", title="Akaike (AIC)", type="number")
  semLoglikTable$addColumnInfo(name="bic", title="Bayesian (BIC)", type="number")
  semLoglikTable$addColumnInfo(name="bic2", title="Sample-size adjusted Bayesian (BIC)", type="number")
  
  semLoglikTable$setData(c(list(model="Model"), semFitMeasures[c('logl', 'unrestricted.logl', 'npar', 'aic', 'bic', 
                                                                 'bic2')]))
  
  semContainer[["fitMeasuresLikelihood"]] <- semLoglikTable
  
  
  ### RMSEA measures
  semRMSEATable <- createJaspTable(title = "Root Mean Square Error of Approximation")
  
  semRMSEATable$addColumnInfo(name="model", title="", type="string")
  semRMSEATable$addColumnInfo(name="rmsea", title="RMSEA", type="number")
  thisOverTitle <- "90% CI"
  semRMSEATable$addColumnInfo(name="rmsea.ci.lower", type = "number", title = "Lower",
                              overtitle = thisOverTitle)
  semRMSEATable$addColumnInfo(name="rmsea.ci.upper", type = "number", title = "Upper",
                              overtitle = thisOverTitle)
  semRMSEATable$addColumnInfo(name="rmsea.pvalue", title="p-value RMSEA <= 0.05 ", type="number")  
  
  semRMSEATable$setData(c(list(model="Model"), semFitMeasures[c('rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'rmsea.pvalue')]))
  
  semContainer[["fitMeasuresRMSEA"]] <- semRMSEATable
  
  
  ### RMR measures
  semRMRTable <- createJaspTable(title = "Standardized Root Mean Square Residual")
  
  semRMRTable$addColumnInfo(name="model", title="", type="string")
  semRMRTable$addColumnInfo(name="rmr", title="RMR", type="number")
  semRMRTable$addColumnInfo(name="rmr_nomean", title="RMR (No Mean)", type="number")
  semRMRTable$addColumnInfo(name="srmr", title="SRMR", type="number")  
  
  semRMRTable$setData(c(list(model="Model"), semFitMeasures[c('rmr', 'rmr_nomean', 'srmr')]))
  
  semContainer[["fitMeasuresRMR"]] <- semRMRTable
  
  ### Other fit measures
  semOtherFitTable <- createJaspTable(title = "Other Fit Indices")
  
  semOtherFitTable$addColumnInfo(name="model", title="", type="string")
  semOtherFitTable$addColumnInfo(name="cn_05", title="Hoelter Critical N (CN) alpha=0.05", type="number")
  semOtherFitTable$addColumnInfo(name="cn_01", title="Hoelter Critical N (CN) alpha=0.01", type="number")
  semOtherFitTable$addColumnInfo(name="gfi", title="Goodness of Fit Index (GFI)", type="number")
  semOtherFitTable$addColumnInfo(name="agfi", title="Parsimony Goodness of Fit Index (GFI)", type="number")
  semOtherFitTable$addColumnInfo(name="mfi", title="McDonald Fit Index (MFI)", type="number")
  # semOtherFitTable$addColumnInfo(name="ecvi", title="Expected Cross-Validation Index (ECVI)", type="number")

  semOtherFitTable$setData(c(list(model="Model"), semFitMeasures[c('cn_05', 'cn_01', 'gfi', 'agfi', 'mfi')]))
  # Todo, ecvi??
  semContainer[["fitMeasuresOther"]] <- semOtherFitTable
  
  return()
}

.semModIndicesTable <- function(semContainer, options, dataset, ready) {
  if (!ready)
    return()
  
  semModIndicesTable <- createJaspTable(title = "Modification Indices")
  
  semModIndicesTable$addColumnInfo(name="lhs", title = "", type="string")
  semModIndicesTable$addColumnInfo(name="op",  title = "",type="string")
  semModIndicesTable$addColumnInfo(name="rhs",  title = "", type="string")
  semModIndicesTable$addColumnInfo(name="mi", type="number")
  semModIndicesTable$addColumnInfo(name="epc", type="number")
  semModIndicesTable$addColumnInfo(name="sepc.lv",  title = "sepc (lv)", type="number")
  semModIndicesTable$addColumnInfo(name="sepc.all", title = "sepc (all)", type="number")
  semModIndicesTable$addColumnInfo(name="sepc.nox",  title = "sepc (nox)",type="number")
  
  semModIndicesTable$showSpecifiedColumnsOnly <- TRUE
  
  if (!is.null(semContainer[["semResultsList"]]$object$semResults)) {
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
    
    # # Converting base 64 to normal
    # varind <- as.matrix(semModIndResult["lhs"]) %in% .v(variables)
    # semModIndResult["lhs"][varind,1] <- .unv(semModIndResult["lhs"][varind,1])
    # varind <- as.matrix(semModIndResult["rhs"]) %in% .v(variables)
    # semModIndResult["rhs"][varind,1] <- .unv(semModIndResult["rhs"][varind,1])
    
    # TODO multiple models
    # modIndices[["cases"]] <- rep("", nrow(semModIndResult))
    # for (i in seq_len(nrow(semModIndResult))) {
    #   modIndices[["data"]][[i]] <- as.list(semModIndResult[i, ])
    #   modIndices[["data"]][[i]][is.na(modIndices[["data"]][[i]])] <- '.'
    # }
    semModIndicesTable$setData(semModIndResult)
    semContainer[["semModIndicesTable"]] <- semModIndicesTable
  }
  
  return()
}

.semEstimatesTable <- function(semContainer, options, dataset, ready, variables) {
  if (!ready)
    return()
  
  semEstimatesTable <- createJaspTable(title = "Parameter Estimates")
  
  semEstimatesTable$addColumnInfo(name="lhs", title = "", type="string")
  semEstimatesTable$addColumnInfo(name="op", title = "", type="string")
  semEstimatesTable$addColumnInfo(name="rhs",  title = "", type="string")
  # semEstimatesTable$addColumnInfo(name="label", type="string")
  semEstimatesTable$addColumnInfo(name="est", type="number")
  semEstimatesTable$addColumnInfo(name="se", type="number")
  semEstimatesTable$addColumnInfo(name="z", type="number")
  semEstimatesTable$addColumnInfo(name="pvalue", title = "p", type="number")
  semEstimatesTable$addColumnInfo(name="ci.lower", title = "CI (lower)", type="number")
  semEstimatesTable$addColumnInfo(name="ci.upper", title = "CI (upper)", type="number")
  semEstimatesTable$addColumnInfo(name="std.lv", title = "std (lv)", type="number")
  semEstimatesTable$addColumnInfo(name="std.all", title = "std (all)", type="number")
  semEstimatesTable$addColumnInfo(name="std.nox", title = "std (nox)", type="number")
  # semEstimatesTable$addColumnInfo(name="group",  title = "group", type="string")
  
  semEstimatesTable$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")
  
  if (!is.null(semContainer[["semResultsList"]]$object$semResults)) {
    
    semEstimates <- lavaan:::parameterEstimates(semContainer[["semResultsList"]]$object$semResults, standardized=TRUE)
    semEstimatesTable$setData(semEstimates)
    semContainer[["semEstimatesTable"]] <- semEstimatesTable
    
  } 
  
  return()
}

.semFitTable <- function(semContainer, options, dataset, ready) {
  if (!ready)
    return()
  
  # semContainer[["semFitTable"]] 
  semFitTable <- createJaspTable(title = "Chi Square Test Statistic (unscaled)")
  
  semFitTable$addColumnInfo(name="Model", title = "", type="string")
  semFitTable$addColumnInfo(name="Df", title = "df", type="number")
  semFitTable$addColumnInfo(name="DfDiff", title = "&#916;df", type="number")
  semFitTable$addColumnInfo(name="AIC", type="number")
  semFitTable$addColumnInfo(name="BIC", type="number")
  semFitTable$addColumnInfo(name="Chisq", title = "&#967;&sup2;", type="number")
  semFitTable$addColumnInfo(name="ChisqDiff", title = "&#916;&#967;&sup2;", type="number")
  semFitTable$addColumnInfo(name="PrChisq", title = "p", type="number")
  
  semFitTable$showSpecifiedColumnsOnly <- TRUE
  semFitTable$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")
  
  if (is.null(semContainer[["semResultsList"]]$object$semResults)) {
    fitTableResult <- as.data.frame(list(Df=c(0, NA), AIC=c(NA, NA), BIC=c(NA, NA),
                                         Chisq = c(0, NA), ChisqDiff=c(NA, NA),
                                         DfDiff = c(NA, NA), PrChisq = c(NA, NA)), 
                                    row.names = c("Saturated", options$modelName))
  } else {
    # Current to saturated:
    fitTableResult <- lavaan::lavTestLRT(semContainer[["semResultsList"]]$object$semResults)
    rownames(fitTableResult)[2] <- options$modelName
    colnames(fitTableResult) <- c("Df", "AIC", "BIC", "Chisq", "ChisqDiff", "DfDiff", "PrChisq")
    
    # TODO add multiple models
    # String to compare to other models:
    # if (length(state$models) > 1) {
    #   str <- paste0("lavaan::lavTestLRT(",paste("state$models[[",seq_along(state$models),"]]",collapse=", "),", model.names = names(state$models))")
    #   toOthers <- eval(parse(text=str))
    #   fitTableResult <- rbind(fitTableResult[1, , drop=FALSE], toOthers)
    #   fitTableResult$`DfDiff`[-1] <- diff(fitTableResult$Df)
    #   fitTableResult$`ChisqDiff`[-1] <- abs(diff(fitTableResult$Chisq))
    #   fitTableResult$`PrChisq`[-1] <- pchisq(fitTableResult$ChisqDiff[-1], fitTableResult$DfDiff[-1], lower.tail = FALSE)
    # }
  }
  
  # for (i in seq_len(NROW(fitTableResult))) {
  #   an0va[["data"]][[i]] <- c(Model=rownames(fitTableResult)[i], as.list(fitTableResult[i, ]))
  #   an0va[["data"]][[i]][is.na(an0va[["data"]][[i]])] <- '.'
  #   names(an0va[["data"]][[i]]) <- gsub("Df", "DF", names(an0va[["data"]][[i]]))
  # }
  
  fitTableResult[["Model"]] <- rownames(fitTableResult)
  semFitTable$setData(fitTableResult)
  semContainer[["semFitTable"]] <- semFitTable
  
  return()
}

.semRSquaredTable <- function(semContainer, options, dataset, ready) {
  if (!ready)
    return()
  
  semRSquaredTable <- createJaspTable(title = "R-Squared")
  
  semRSquaredTable$addColumnInfo(name="var", title = "Variable", type="string")
  semRSquaredTable$addColumnInfo(name = "R2",  title = "R&sup2;",  type = "number")
  
  if (!is.null(semContainer[["semResultsList"]]$object$semResults)) {
    r2 <- lavaan::inspect(semContainer[["semResultsList"]]$object$semResults, "r2")
    nm <- names(r2)
    for (i in 1:length(r2)) {
      semRSquaredTable$addRows(data.frame(var = .unv(names(r2[[i]])), R2 = r2[[i]]))
    }
  }

  semContainer[["rSquaredTable"]] <- semRSquaredTable
  return()
}

.semCovCorTable <- function(semContainer, options, dataset, ready) {
  if (!ready || !(options$outputObservedCovarianceCorrelations || options$outputFittedCovarianceCorrelations || 
                  options$outputResidualCovarianceCorrelations))
    return()
  
  semCovCorTable <- createJaspTable(title = "Covariances (lower triangle) / correlations (upper triangle)")
  
  semCovCorTable$addColumnInfo(name="Variable", title="", type="string")
  semCovCorTable$addColumnInfo(name="Type", title="", type="string")
  
  if (!is.null(semContainer[["semResultsList"]]$object$semResults)) {
    
    include <- c("observed", "fitted", "residual")[ c(options$outputObservedCovarianceCorrelations, 
                                                      options$outputFittedCovarianceCorrelations,
                                                      options$outputResidualCovarianceCorrelations)]
    
    observedCov <- lavaan::inspect(semContainer[["semResultsList"]]$object$semResults, "sampstat")$cov
    fittedCov <- lavaan::fitted(semContainer[["semResultsList"]]$object$semResults)$cov
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
    
    for (i in 1:n) {
      semCovCorTable$addColumnInfo(name=varNames[i], title=.unv(varNames[i]), type="number")
    }
    
    names(matDF)[1:2] <- c("Variable", "Type")
    semCovCorTable$setData(matDF)
  }
  
  semContainer[["covCorTable"]] <- semCovCorTable
  
  return()
}

.semMardiasCoefficientTable <- function(semContainer, options, dataset, ready) {
  if (!ready)
    return()
  
  semMardiasTable <- createJaspTable(title = "Mardia's coefficients")
  
  
  semMardiasTable$addColumnInfo(name="Type", title="", type="string")
  semMardiasTable$addColumnInfo(name="Coefficient", type="number")
  semMardiasTable$addColumnInfo(name="z", type="number")
  semMardiasTable$addColumnInfo(name="Chisq", title="&#967;&sup2;", type="number")
  semMardiasTable$addColumnInfo(name="DF", title="df", type="number")
  semMardiasTable$addColumnInfo(name="pvalue", title="p", type="number")
  
  if (!is.null(semContainer[["semResultsList"]]$object$semResults)) {
    varNames <- lavaan::lavaanNames(semContainer[["semResultsList"]]$object$semResults, type="ov")
    mardiaSkew <- unname(semTools:::mardiaSkew(dataset[, varNames]))
    mardiaKurtosis <- unname(semTools:::mardiaKurtosis(dataset[, varNames]))
    semMardiasTable$addRows(data.frame(Type="Skewness", Coefficient=mardiaSkew[1], z=NA, Chisq=mardiaSkew[2], 
                                       DF=mardiaSkew[3], "pvalue"=mardiaSkew[4]))
    semMardiasTable$addRows(data.frame(Type="Kurtosis", Coefficient=mardiaKurtosis[1], z=mardiaKurtosis[2], 
                                       Chisq=NA, DF=NA, "pvalue"=mardiaKurtosis[3]))
  }
  
  semContainer[["mardiasTable"]] <- semMardiasTable
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

.lavCreatePathDiagram <- function(semContainer, options, ready) {
  if (!is.null(semContainer[["pathDiagramPlotCollection"]]))
    return()
  
  pathDiagramPlotCollection <- createJaspContainer(title = "Path Diagrams")
    
    
  # set dependencies
  # qqPlot$dependOn(c("qqPlot"))
  
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
    semContainer$setError(errorMessage)
    return()
  }
  
  if (options$groupingVariable != "") {
    # semplot returns an unnamed list, but which plot belongs to which grouping level?
    # The order should match the order of the levels in the lavaan model..
    titles <- semPlotModel@Original[[1]]@Data@group.label
    
    if (!is.character(titles) || (length(titles) != length(p)))
      titles <- seq_along(p)
    
    plotList <- list()
    keep <- NULL
    for (i in seq_along(p)) {
      .lavWritePathDiagram(p[[i]], titles[i], options, semContainer)
    }
    
  } else {
    .lavWritePathDiagram(p, "Path Diagram", options, semContainer)
  }

  return()
}

.lavWritePathDiagram <- function(plotObj, title, options, semContainer) {
  pathDiagram <- list()
  pathDiagram$title <- title
  pathDiagram$width <- options$plotWidth
  pathDiagram$height <- options$plotHeight
  if (pathDiagram$height == 0) {
    pathDiagram$height <- 1 + 299 * (length(options$variables)/5)
  }
  pathDiagram$custom <- list(width="plotWidth", height="plotHeight")

  semContainer[[paste0("pathPlot", title)]] <- createJaspPlot(width = pathDiagram$width, 
                         height = pathDiagram$height, plot = plotObj, title = title)
  
  return()
}

.lavToPlotObj <- function(lavResult) {
  # Create semplot model and unv the names of the manifest variables
  # Sorry, this code is really ugly but all it does is replace names for plot.
  semPlotMod <- semPlot::semPlotModel(list(lavResult), list(mplusStd = "std"))[[1]]
  
  manifests <- semPlotMod@Vars$name[semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[semPlotMod@Vars$manifest] <- .unv(manifests)
  
  lhsAreManifest <- semPlotMod@Pars$lhs %in% manifests
  if (any(lhsAreManifest)) semPlotMod@Pars$lhs[lhsAreManifest] <- .unv(semPlotMod@Pars$lhs[lhsAreManifest])
  
  rhsAreManifest <- semPlotMod@Pars$rhs %in% manifests
  if (any(rhsAreManifest)) semPlotMod@Pars$rhs[rhsAreManifest] <- .unv(semPlotMod@Pars$rhs[rhsAreManifest])
  
  return(semPlotMod)
}

