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

# Function to create covariance matrix:
.covTable <- function(fit, title = "Covariance matrix", include=c("observed", "fitted", "residual")) {
  observedCov <- lavaan::inspect(fit, "sampstat")$cov
  fittedCov <- lavaan::fitted(fit)$cov
  residualCov <- observedCov - fittedCov

  varNames <- colnames(observedCov)

  covList <- list(observed=observedCov,
                  fitted=fittedCov,
                  residual=residualCov
  )

  corList <- list(observed=stats::cov2cor(observedCov),
                  fitted=stats::cov2cor(fittedCov),
                  residual=stats::cov2cor(observedCov) - stats::cov2cor(fittedCov)
  )

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

  # Empty object:
  covariances <- list()

  # Fill:
  covariances[["title"]] <- title
  # covariances[["cases"]] <- matDF$..varName
  #  covariances[["cases"]] <- rep("",length(matDF$..varName))

  fields <- list(list(name="Variable", title="", type="text"), list(name="Type", title="", type="text"))

  # Enter fields:
  for (i in 1:n) {
    fields[[i+2]] <- list(name=varNames[i], title=.unv(varNames[i]), type="number", format="dp:3")
  }

  # Enter rows:
  rows <- list()

  for (i in seq_len(NROW(matDF))) {
    variable.name <- varNames[i]
    row <- matDF[i, , drop=FALSE]
  names(row)[1:2] <- c("Variable", "Type")
    rows[[i]] <- as.list(row)
  }

  schema <- list(fields=fields)

  covariances[["schema"]] <- schema
  covariances[["data"]] <- rows
  return(covariances)
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

### SEM Function:
SEMSimple <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
	# this line does not work in jasptools :(
  dheader <- .readDataSetHeader(all.columns = TRUE)
	availableVars <- colnames(dheader)
  variables <- .getUsedVars(options$model, availableVars)
  model <- .translateModel(options$model, variables)

  if (options$groupingVariable != "")
    variables <- c(variables, options$groupingVariable)

  if (perform == "run" && options$Data != "varcov") {
    dataset <- .readDataSetToEnd(columns = variables)
  } else if (perform == "run" && options$Data == "varcov") {
    dataset <- .readDataSetToEnd(all.columns = TRUE)
  } else {
    dataset <- dheader
  }


  # Retrieve state:
  state <- .retrieveState()

  if (is.null(state)) {  # is there state?
    state <- list(models = list())
  }


  #### INPUT CHECK #####
  if (options$Data == "varcov") {
    # Check if dataset is variance covariance matrix:
    errorCheck <- .hasErrors(dataset, perform = perform,
                             type = c("varCovMatrix", "infinity"),
                             message='default', exitAnalysisIfErrors = TRUE)
  } else if (ncol(dataset) > 0) {
    errorCheck <- .hasErrors(dataset, perform = perform, type = c("infinity"),
                             message='default', exitAnalysisIfErrors = TRUE)
  } else {
    errorCheck <- FALSE
  }

  # Check mean structure:
  if (options$Data == "varcov"){
    if (isTRUE(options$includeMeanStructure)) {
      errorCheck <- list(error = TRUE, message="Mean structure can not be included when data is variance-covariance matrix")
    }
    options$includeMeanStructure <- FALSE

    if (options$SampleSize == 0) {
      errorCheck <- list(error = TRUE, message = "Please set the sample size!")
    }

    # Check for multiple groups:
    if (options$groupingVariable!=""){
      errorCheck <- list(error = TRUE, message = "Multiple group analysis not (yet) supported when data is variance-covariance matrix")
    }
  } else {
    if (identical(errorCheck, FALSE)) {
      if (perform == "run" && ncol(dataset) > 0 && !nrow(dataset) > ncol(dataset)) {
        errorCheck <- list(error=TRUE, message="Not more cases than number of variables. Is your data a variance-covariance matrix?")
      }
    }
  }

  inputCorrect <- identical(errorCheck, FALSE)
  ######################


  errorMessage <- ""
  groupVar <- NULL

  if (.v(options$groupingVariable) %in% names(dataset)) {
    groupVar <- .v(options$groupingVariable)
    # TODO (Sacha): No print I presume (Alexander)
    print(class(dataset[[ groupVar ]]))
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
  if (perform == "run" && inputCorrect) {
      # Raw data:
      if (options$Data == "raw"){
          semResults <- try(
            lavaan:::lavaan(model=model, data=dataset,
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
            lavaan:::lavaan(model=model, sample.cov=as.matrix(dataset),
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
        errorMessage <- "Model misspecified"
      }
      semResults <- NULL
    }
  } else {
    semResults <- NULL

    lavModel <- try(lavaan:::lavaanify(model=model, auto.delta=options$addScalingParameters,
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
        errorMessage <- "Model misspecified"
      }
      lavModel <- NULL
    } else {
      variableNames <- lavaan:::lavNames(lavModel)
    }
  }


  # Overwrite resulting model in state:
  if (!is.null(semResults)){
    state$models[[options$modelName]] <- semResults
  }

  ### Output object:
  results <- list()

  meta <- list()

  meta[[1]]  <- list(name="fit", type="table")
  meta[[2]]  <- list(name="parameterEstimates", type="table")
  meta[[3]]  <- list(name="fitMeasures_modelTest", type="table")
  meta[[4]]  <- list(name="fitMeasures_vsBaseline", type="table")
  meta[[5]]  <- list(name="fitMeasures_likelihoodInfo", type="table")
  meta[[6]]  <- list(name="fitMeasures_RMSEA", type="table")
  meta[[7]]  <- list(name="fitMeasures_RMR", type="table")
  meta[[8]]  <- list(name="fitMeasures_Other", type="table")
  meta[[9]]  <- list(name="covcor", type="table")
  meta[[10]] <- list(name="modificationIndices", type="table")
  meta[[11]] <- list(name="mardiasCoefficient", type="table")

  results[[".meta"]] <- meta
  results[["title"]] <- "lavaan: Structural Equation Modeling"

  # Error:
  error <- !inputCorrect || (errorMessage!="" & perform == "run" & options$model != "")

  ### ANOVA table ###
  an0va <- list()
  an0va[["title"]] <- "Chi Square Test Statistic (unscaled)"
  # an0va[["cases"]] <- c("Saturated", "Model")
  an0va[["schema"]] <- list(
    fields = list(
      list(name="Model", title = "", type="string"),
      list(name="DF", title = "df", type="number", format="dp:0"),
      list(name="AIC", type="number", format="dp:1"),
      list(name="BIC", type="number", format="dp:1"),
      list(name="Chisq", title = "\u3C7\uB2", type="number", format="dp:3"),
      list(name="Chisq diff", title = "\u394\u3C7\uB2", type="number", format="dp:3"),
      list(name="Pr(>Chisq)", title = "p", type="number", format="dp:3;p:0.001")
    )
  )
  an0va[["data"]] <- list()
  ### PERFORM = RUN
  if (is.null(semResults)) {
    sem_anova <- structure(list(Df=c(0, NA), AIC=c(NA, NA), BIC=c(NA, NA),
                                Chisq = c(0, NA), `Chisq diff`=c(NA, NA),
                                `Df diff` = c(NA, NA), `Pr(>Chisq)` = c(NA, NA)),
                           .Names = c("Df", "AIC", "BIC", "Chisq", "Chisq diff", "Df diff", "Pr(>Chisq)"),
                           row.names = c("Saturated", options$modelName),
                           class = c("anova", "data.frame"),
                           heading = "Chi Square Test Statistic (unscaled)\n")
  } else {
    # Current to saturated:
    sem_anova <- lavaan::lavTestLRT(semResults)
    rownames(sem_anova)[2] <- options$modelName

    # String to compare to other models:
    if (length(state$models) > 1) {
      str <- paste0("lavaan::lavTestLRT(",paste("state$models[[",seq_along(state$models),"]]",collapse=", "),", model.names = names(state$models))")
      toOthers <- eval(parse(text=str))
      sem_anova <- rbind(sem_anova[1, , drop=FALSE], toOthers)
      sem_anova$`Df diff`[-1] <- diff(sem_anova$Df)
      sem_anova$`Chisq diff`[-1] <- abs(diff(sem_anova$Chisq))
      sem_anova$`Pr(>Chisq)`[-1] <- pchisq(sem_anova$`Chisq diff`[-1], sem_anova$`Df diff`[-1], lower.tail = FALSE)
    }

    # rownames(sem_anova) <- c("Saturated",options$modelName,names(state$models)[names(state$models) != options$modelName])
  #
  #   models <- list(semResults)
  #   names(models) <- options$modelName
  #   sem_anova <- do.call(lavaan::anova,models)
  }

  for (i in seq_len(NROW(sem_anova))) {
    an0va[["data"]][[i]] <- c(Model=rownames(sem_anova)[i], as.list(sem_anova[i, ]))
    an0va[["data"]][[i]][is.na(an0va[["data"]][[i]])] <- '.'
    names(an0va[["data"]][[i]]) <- gsub("Df", "DF", names(an0va[["data"]][[i]]))
  }

  if (error) {
      if (!inputCorrect) {
          errorMessage <- errorCheck$message
      } else {
          errorMessage <- paste0("Lavaan crashed with the following error: ",errorMessage)
      }

      an0va[['error']] <- list(errorType="package", errorMessage=errorMessage)
  }
  results[['fit']] <- an0va

  ### PARAMETER ESTIMATES ####
  parEstimates <- list()
  parEstimates[["title"]] <- "Parameter Estimates"
  parEstimates[["schema"]] <- list(
      fields = list(
      list(name="lhs", title = "", type="character"),
      list(name="op", title = "", type="character"),
      list(name="rhs",  title = "", type="character"),
      list(name="label", type="character"),
      list(name="est", type="number", format = "dp:3"),
      list(name="se", type="number", format = "dp:3"),
      list(name="z", type="number", format = "dp:3"),
      list(name="pvalue", title = "p", type="number", format = "dp:3;p:.001"),
      list(name="ci.lower", title = "CI (lower)", type="number", format = "dp:3"),
      list(name="ci.upper", title = "CI (upper)", type="number", format = "dp:3"),
      list(name="std.lv", title = "std (lv)", type="number", format = "dp:3"),
      list(name="std.all", title = "std (all)", type="number", format = "dp:3"),
      list(name="std.nox", title = "std (nox)", type="number", format = "dp:3"),
      list(name="group",  title = "group", type="character")
      )
  )

  parEstimates[["data"]] <- list()
  if (errorMessage!="" & perform == "run" & options$model != "") {
      parEstimates[['error']] <- list(errorType="badData")
  }

  if (!is.null(semResults)) {
    sem_parest <- lavaan:::parameterEstimates(semResults, standardized=TRUE)
    # parEstimates[["cases"]] <- rep("",NROW(sem_parest))
    for (i in seq_len(NROW(sem_parest))) {
      estimates <- sem_parest[i, ]
      if (estimates["lhs"] %in% .v(variables)) {
        estimates["lhs"] <- .unv(estimates["lhs"])
      }
      if (estimates["rhs"] %in% .v(variables)) {
        estimates["rhs"] <- .unv(estimates["rhs"])
      }

      estimates[is.na(estimates)] <- '.'
      parEstimates[["data"]][[i]] <- as.list(estimates)
    }
  } else {
    parEstimates[["cases"]] <- NULL
  }
  results[["parameterEstimates"]] <- parEstimates

#
#
  ### MODIFICATION INDICES ###
## SORT THEM
  if (options$outputModificationIndices) {
      modIndices <- list()
    modIndices[["title"]] <- "Modification Indices"

    modIndices[["schema"]] <- list(
        fields = list(
        list(name="lhs", title = "", type="character"),
        list(name="op",  title = "",type="character"),
        list(name="rhs",  title = "", type="character"),
        list(name="mi", type="number", format = "dp:3"),
        list(name="epc", type="number", format = "dp:3"),
        list(name="sepc.lv",  title = "sepc (lv)", type="number", format = "dp:3"),
        list(name="sepc.all", title = "sepc (all)", type="number", format = "dp:3"),
        list(name="sepc.nox",  title = "sepc (nox)",type="number", format = "dp:3")
      )
    )

    modIndices[["data"]] <- list()
    if (!is.null(semResults)) {
      # Extract modidffication indices:
      sem_modind <- lavaan:::modificationIndices(semResults)

      ### Remove NA:
      sem_modind <- sem_modind[!is.na(sem_modind$mi), , drop=FALSE]

      ## Sort:
      sem_modind <- sem_modind[order(sem_modind$mi, decreasing=TRUE), , drop=FALSE]

      ### Remove low indices:
      if (isTRUE(options$outputModificationIndicesHideLowIndices)) {
        sem_modind <- sem_modind[sem_modind$mi > options$outputModificationIndicesHideLowIndicesThreshold, , drop=FALSE]
      }

      # Converting base 64 to normal
      varind <- as.matrix(sem_modind["lhs"]) %in% .v(variables)
      sem_modind["lhs"][varind,1] <- .unv(sem_modind["lhs"][varind,1])
      varind <- as.matrix(sem_modind["rhs"]) %in% .v(variables)
      sem_modind["rhs"][varind,1] <- .unv(sem_modind["rhs"][varind,1])

      modIndices[["cases"]] <- rep("", nrow(sem_modind))
      for (i in seq_len(nrow(sem_modind))) {
        modIndices[["data"]][[i]] <- as.list(sem_modind[i, ])
        modIndices[["data"]][[i]][is.na(modIndices[["data"]][[i]])] <- '.'
      }
    } else {
        modIndices[["cases"]] <- NULL
    }

    if (errorMessage!="" & perform == "run" & options$model != "") {
        modIndices[['error']] <- list(errorType="badData")
    }

    results[["modificationIndices"]] <- modIndices
  }


  ## FIT MEASURES ###
  if (options$outputAdditionalFitMeasures) {
      if (!is.null(semResults)) {
          sem_fitm <- unlist(lavaan:::fitMeasures(semResults))
      } else {
          # TODO (Sacha): As a reference to error I normally set objects to NA
          sem_fitm <- character(0)
      }

    # Model test table:
    fitMeasures_modelTest <- list()
    fitMeasures_modelTest[["title"]] <- "Model test baseline model"
    fitMeasures_modelTest[["schema"]] <- list(
        fields=list(
            list(name="model", title="", type="text"),
            list(name="fmin", title="Minimum Function Test Statistic", type="number", format="dp:3"),
            list(name="chisq", title="\u3C7\uB2", type="number", format="dp:3"),
            list(name="df", title="Degrees of freedom", type="number", format = "dp:0"),
            list(name="pvalue", title="p", type="number", format="dp:3;p:0.001")
        )
    )

    fitMeasures_modelTest[["data"]] <- list()

    # TODO (Sacha): concataneting list might lead to unexpected behaviour, try modifyList
    fitMeasures_modelTest[["data"]][[1]] <- c(
        list(model="Model"),
        as.list(sem_fitm[c('fmin', 'chisq', 'df', 'pvalue')])
    )

    fitMeasures_modelTest[["casesAcrossColumns"]] <- TRUE
    results[["fitMeasures_modelTest"]] <- fitMeasures_modelTest

    # vs Baseline:
    fitMeasures_vsBaseline <- list()
    fitMeasures_vsBaseline[["title"]] <- "User model versus baseline model"
    fitMeasures_vsBaseline[["schema"]] <- list(
        fields=list(
            list(name="model", title="", type="text"),
            list(name="cfi", title="Comparative Fit Index (CFI)", type="number", format="dp:3"),
            list(name="tli", title="Tucker-Lewis Index (TLI)", type="number", format="dp:3"),
            list(name="nnfi", title="Bentler-Bonett Non-normed Fit Index (NNFI)", type="number", format="dp:3"),
            list(name="nfi", title="Bentler-Bonett Normed Fit Index (NFI)", type="number", format="dp:3"),
            list(name="pnfi", title="Parsimony Normed Fit Index (PNFI)", type="number", format="dp:3"),
            list(name="rfi", title="Bollen's Relative Fit Index (RFI)", type="number", format="dp:3"),
            list(name="ifi", title="Bollen's Incremental Fit Index (IFI)", type="number", format="dp:3"),
            list(name="rni", title="Relative Noncentrality Index (RNI)", type="number", format="dp:3")
        )
    )

    fitMeasures_vsBaseline[["data"]] <- list()

    # TODO (Sacha): concataneting list might lead to unexpected behaviour, try modifyList
    fitMeasures_vsBaseline[["data"]][[1]] <- c(
        list(model="Model"),
        as.list(sem_fitm[c('cfi', 'tli', 'nnfi', 'nfi', 'pnfi', 'rfi', 'ifi', 'rni')])
    )

    fitMeasures_vsBaseline[["casesAcrossColumns"]] <- TRUE
    results[["fitMeasures_vsBaseline"]] <- fitMeasures_vsBaseline

    # Loglikelihood and Information Criteria:
    fitMeasures_likelihoodInfo <- list()
    fitMeasures_likelihoodInfo[["title"]] <- "Loglikelihood and Information Criteria"
    fitMeasures_likelihoodInfo[["schema"]] <- list(
        fields=list(
            list(name="model", title="", type="text"),
            list(name="logl", title="Loglikelihood user model (H0)", type="number", format="dp:3"),
            list(name="unrestricted.logl", title="Loglikelihood unrestricted model (H1)", type="number", format="dp:3"),
            list(name="npar", title="Number of free parameters", type="number", format="dp:0"),
            list(name="aic", title="Akaike (AIC)", type="number", format="dp:3"),
            list(name="bic", title="Bayesian (BIC)", type="number", format="dp:3"),
            list(name="bic2", title="Sample-size adjusted Bayesian (BIC)", type="number", format="dp:3")
        )
    )

    fitMeasures_likelihoodInfo[["data"]] <- list()

    # TODO (Sacha): concataneting list might lead to unexpected behaviour, try modifyList
    fitMeasures_likelihoodInfo[["data"]][[1]] <- c(
        list(model="Model"),
        as.list(sem_fitm[c('logl', 'unrestricted.logl', 'npar', 'aic', 'bic', 'bic2')])
    )

    fitMeasures_likelihoodInfo[["casesAcrossColumns"]] <- TRUE
    results[["fitMeasures_likelihoodInfo"]] <- fitMeasures_likelihoodInfo

    # RMSEA:
    fitMeasures_RMSEA <- list()
    fitMeasures_RMSEA[["title"]] <- "Root Mean Square Error of Approximation"
    fitMeasures_RMSEA[["schema"]] <- list(
        fields=list(
            list(name="model", title="", type="text"),
            list(name="rmsea", title="RMSEA", type="number", format="dp:3"),
            list(name="rmsea.ci", title="90 Percent Confidence Interval", type="text"),
            list(name="rmsea.pvalue", title="p-value RMSEA <= 0.05 ", type="number", format="dp:3;p:0.001")
        )
    )

    fitMeasures_RMSEA[["data"]] <- list()

    if (is.na(sem_fitm['rmsea.ci.lower'])) {
      CI <- '.'
    } else {
      CI <- paste(formatC(sem_fitm['rmsea.ci.lower'], format='f', digits=3),
                  '-', formatC(sem_fitm['rmsea.ci.upper'], format='f', digits=3)
      )
    }

    # TODO (Sacha): concataneting list might lead to unexpected behaviour, try modifyList
    fitMeasures_RMSEA[["data"]][[1]] <- c(
        list(model="Model"),
        as.list(sem_fitm['rmsea']),
        list(rmsea.ci=CI),
        as.list(sem_fitm['rmsea.pvalue'])
    )

    fitMeasures_RMSEA[["casesAcrossColumns"]] <- TRUE
    results[["fitMeasures_RMSEA"]] <- fitMeasures_RMSEA

    # Standardized Root Mean Square Residual:
    fitMeasures_RMR <- list()
    fitMeasures_RMR[["title"]] <- "Standardized Root Mean Square Residual"
    fitMeasures_RMR[["schema"]] <- list(
        fields=list(
            list(name="model", title="", type="text"),
            list(name="rmr", title="RMR", type="number", format="dp:3"),
            list(name="rmr_nomean", title="RMR (No Mean)", type="number", format="dp:3"),
            list(name="srmr", title="SRMR", type="number", format="dp:3")
        )
    )

    fitMeasures_RMR[["data"]] <- list()

    # TODO (Sacha): concataneting list might lead to unexpected behaviour, try modifyList
    fitMeasures_RMR[["data"]][[1]] <- c(
        list(model="Model"),
        as.list(sem_fitm[c('rmr', 'rmr_nomean', 'srmr')])
    )

    fitMeasures_RMR[["casesAcrossColumns"]] <- TRUE
    results[["fitMeasures_RMR"]] <- fitMeasures_RMR

    # Other Fit Indices
    fitMeasures_Other <- list()
    fitMeasures_Other[["title"]] <- "Other Fit Indices"
    fitMeasures_Other[["schema"]] <- list(
        fields=list(
            list(name="model", title="", type="text"),
            list(name="cn_05", title="Hoelter Critical N (CN) alpha=0.05", type="number", format="dp:3"),
            list(name="cn_01", title="Hoelter Critical N (CN) alpha=0.01", type="number", format="dp:3"),
            list(name="gfi", title="Goodness of Fit Index (GFI)", type="number", format="dp:3"),
            list(name="agfi", title="Parsimony Goodness of Fit Index (GFI)", type="number", format="dp:3"),
            list(name="mfi", title="McDonald Fit Index (MFI)", type="number", format="dp:3"),
            list(name="ecvi", title="Expected Cross-Validation Index (ECVI)", type="number", format="dp:3")
        )
    )

    fitMeasures_Other[["data"]] <- list()

    # TODO (Sacha): concataneting list might lead to unexpected behaviour, try modifyList
    fitMeasures_Other[["data"]][[1]] <- c(
        list(model="Model"),
        as.list(sem_fitm[c('cn_05', 'cn_01', 'gfi', 'agfi', 'mfi', 'ecvi')])
    )

    fitMeasures_Other[["casesAcrossColumns"]] <- TRUE
    results[["fitMeasures_Other"]] <- fitMeasures_Other

    if (errorMessage != "" && perform == "run" && options$model != "") {
        # TODO (Sacha): which error message should this belong to?
        #   It used to be fitMeasures[['error']] <- list(errorType="badData")
        #   but fitMeasures is not initialised
        fitMeasures_Other[['error']] <- list(errorType="badData")
    }
  }

  ### Mardia coefficient
  if (options$outputMardiasCoefficients) {
      mardiasCoefficient <- list()
    mardiasCoefficient[["title"]] <- "Mardia's coefficients"
    mardiasCoefficient[["schema"]] <- list(
        fields = list(
            list(name="Type", title="", type="string"),
            list(name="Coefficient", type="number", format="dp:3"),
            list(name="z", type="number", format="dp:3"),
            list(name="Chisq", title="\u3C7\uB2", type="number", format="dp:3"),
            list(name="DF", title="df", type="number", format="dp:0"),
            list(name="p-value", title="p", type="number", format="dp:3;p:0.001")
        )
    )

    if (!is.null(semResults)) {
      varNames <- lavaan::lavaanNames(semResults, type="ov")

      mardiaSkew <- unname(semTools:::mardiaSkew(dataset[, varNames]))
      mardiaKurtosis <- unname(semTools:::mardiaKurtosis(dataset[, varNames]))
      mardiasCoefficient[["data"]] <- list(
          list(Type="Skewness", Coefficient=mardiaSkew[1], z=".", Chisq=mardiaSkew[2], DF=mardiaSkew[3], "p-value"=mardiaSkew[4]),
          list(Type="Kurtosis", Coefficient=mardiaKurtosis[1], z=mardiaKurtosis[2], Chisq=".", DF=".", "p-value"=mardiaKurtosis[3])
      )
    }

    if (errorMessage !="" && perform == "run" && options$model != "") {
        mardiasCoefficient[['error']] <- list(errorType="badData")
    }
    results[["mardiasCoefficient"]] <- mardiasCoefficient
  }

  ### Covariance table:
  # Only if observed, fitted or residual
  if( options$outputObservedCovarianceCorrelations || options$outputFittedCovarianceCorrelations || options$outputResidualCovarianceCorrelations) {
      if (!is.null(semResults)) {
      results[["covcor"]] <- .covTable(
          semResults, "Covariances (lower triangle) / correlations (upper triangle)",
          include=c("observed", "fitted", "residual")[ c(options$outputObservedCovarianceCorrelations, options$outputFittedCovarianceCorrelations ,options$outputResidualCovarianceCorrelations)]
      )
    }
  }

  results[["parameterEstimates"]][["citation"]] <-
  results[["fit"]][["citation"]] <- list(
    "Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/"
  )
  # Return
  status <- list(ready=TRUE, error=error, errorMessage=errorMessage)
  if (perform == "run" && status$ready) {
      retList <- list(results=results, status="complete", state=state)
  } else {
      retList <- list(results=results, status="inited", state=state)
  }
  return(retList)
}
