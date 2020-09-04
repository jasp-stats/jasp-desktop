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

SelectionModels <- function(jaspResults, dataset, options, state = NULL) {
  
  if (.smCheckReady(options)) {
    # get the data
    dataset <- .smGetData(dataset, options)
    dataset <- .smCheckData(jaspResults, dataset, options)
    
    # fit the models
    .smFit(jaspResults, dataset, options)
  }
  
  # main summary tables
  .smTestsTables(jaspResults, dataset, options)
  .smEstimatesTables(jaspResults, dataset, options)
  
  # the p-value frequency tables
  if (options[["p_table"]])
    .smPFrequencyTable(jaspResults, dataset, options)
  
  # figures
  if (options[["FE_weightfunction"]])
    .smWeightsPlot(jaspResults, dataset, options, "FE")
  if (options[["RE_weightfunction"]])
    .smWeightsPlot(jaspResults, dataset, options, "RE")
  if (options[["plot_models"]])
    .smEstimatesPlot(jaspResults, dataset, options)
  
  return()
}

.smDependencies <- c(
  "auto_reduce", "cutoffs_p", "selection_twosided", "effect_direction", "measures", "mu_transform",
  "input_ES", "input_SE", "input_N", "input_p"
)

.smCheckReady              <- function(options) {
  if (options[["measures"]] == "general") {
    return(options[["input_ES"]] != "" && options[["input_SE"]] != "")
  } else if (options[["measures"]] == "correlation") {
    return(options[["input_ES"]] != "" && options[["input_N"]] != "")
  }
}

.smGetData                 <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    return(.readDataSetToEnd(columns.as.numeric = c(
      options[["input_ES"]],
      if (options[["input_SE"]] != "") options[["input_SE"]],
      if (options[["input_N"]] != "")  options[["input_N"]],
      if (options[["input_p"]] != "")  options[["input_p"]]
    )))
  }
}

.smCheckData               <- function(jaspResults, dataset, options) {
  
  dataset_old <- dataset
  dataset     <- na.omit(dataset)
  
  # store the number of missing values
  if (nrow(dataset_old) > nrow(dataset)) {
    if (!is.null(jaspResults[["nOmitted"]])) {
      nOmitted <- jaspResults[["nOmitted"]]
    } else {
      nOmitted <- createJaspState()
      nOmitted$dependOn(c(.smDependencies))
      jaspResults[["nOmitted"]] <- nOmitted
    }
    nOmitted$object <- nrow(dataset_old) - nrow(dataset)
  }
  
  .hasErrors(dataset               = dataset,
             type                  = c("infinity", "observations"),
             observations.amount   = "< 2",
             exitAnalysisIfErrors  = TRUE)
  
  if (options[["input_SE"]] != "")
    .hasErrors(dataset               = dataset,
               type                  = c("negativeValues"),
               negativeValues.target = options[["input_SE"]],
               exitAnalysisIfErrors  = TRUE)
  
  
  if (options[["input_N"]] != "")
    .hasErrors(dataset               = dataset,
               type                  = c("negativeValues"),
               negativeValues.target = options[["input_N"]],
               exitAnalysisIfErrors  = TRUE)
  
  
  if (options[["input_p"]] != "") 
    if (any(dataset[, .v(options[["input_p"]])] < 0) || any(dataset[, .v(options[["input_p"]])] > 1))
      JASP:::.quitAnalysis(gettextf("Error in %s: All p-values need to be between 0 and 1.", options[["input_p"]]))
  
  
  
  if (options[["input_ES"]] != "" && options[["measures"]] == "correlation")
    if (any(dataset[, .v(options[["input_ES"]])] <= -1) || any(dataset[, .v(options[["input_ES"]])] >= 1))
      JASP:::.quitAnalysis(gettextf("Error in %s: All correlation coefficients need to be between -1 and 1.", options[["input_ES"]]))
  
  
  
  return(dataset)
}

.smGetCutoffs              <- function(options) {
  
  x <- options[["cutoffs_p"]]
  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")
  
  x <- strsplit(x, ",", fixed = TRUE)[[1]]
  
  x <- trimws(x, which = "both")
  x <- x[x != ""]
  
  if (anyNA(as.numeric(x)))
    JASP:::.quitAnalysis(gettext("The p-value cutoffs were set incorectly."))
  
  if (length(x) == 0)
    JASP:::.quitAnalysis(gettext("At least one p-value cuttoff needs to be set."))
  
  x <- as.numeric(x)
  if (options[["selection_twosided"]])
    x <- c(x/2, 1-x/2)
  
  
  if (any(x <= 0 | x >= 1))
    JASP:::.quitAnalysis(gettext("All p-value cutoffs needs to be between 0 and 1."))
  
  
  x <- c(sort(x), 1)
  
  return(x)
}

.smJoinCutoffs             <- function(steps, pval) {
  
  ### this is probably the most complex part of the analysis
  # the fit function protests when there is any p-value interval with lower than 4 p-values
  # the autoreduce should combine all p-value intervals that have too few p-values
  
  # create p-value table
  cutoffsTable <- table(cut(pval, breaks = c(0, steps)))
  
  # start removing from the end, but never 1 at the end
  while (cutoffsTable[length(cutoffsTable)] < 4) {
    
    # remove the one before the last step
    steps <- steps[-(length(steps) - 1)]
    
    # create p-value table
    cutoffsTable <- table(cut(pval, breaks = c(0, steps)))
  }
  
  # and then go from the start(go one by one - two neiboring intervals with 2 p-values will become one with 4 instead of removing all of them)
  while (any(cutoffsTable < 4)) {
    
    # remove the first step that has less than 4 p-values
    steps <- steps[-which.max(cutoffsTable < 4)]
    
    # create p-value table
    cutoffsTable <- table(cut(pval, breaks = c(0, steps)))
  }
  
  # do not fit the models if there is only one p-value interval
  if (length(steps) <= 1)
    stop("No steps")
  
  
  return(steps)
}

.smFillEstimates           <- function(jaspResults, table, fit, options) {
  
  overtitleCI <- gettext("95% Confidence Interval")
  
  table$addColumnInfo(name = "type", title = "",                        type = "string")
  table$addColumnInfo(name = "est",  title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",   title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat", title = gettext("z"),              type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),              type = "pvalue")
  table$addColumnInfo(name = "lCI",  title = gettext("Lower"),          type = "number", overtitle = overtitleCI)
  table$addColumnInfo(name = "uCI",  title = gettext("Upper"),          type = "number", overtitle = overtitleCI)
  
  rowUnadjusted <- list(type = "Unadjusted")
  rowAdjusted   <- list(type = "Adjusted")
  
  if (!is.null(fit)) {
    if (!class(fit) %in% c("simpleError","error")) {
      
      if (fit[["fe"]]) {
        posMean <- 1    
      } else {
        posMean <- 2
      }
      
      rowUnadjusted    <- c(rowUnadjusted, list(
        est  = fit[["unadj_est"]][posMean,1],
        se   = fit[["unadj_se"]][posMean,1],
        stat = fit[["z_unadj"]][posMean,1],
        pval = fit[["p_unadj"]][posMean,1],
        lCI  = fit[["ci.lb_unadj"]][posMean,1],
        uCI  = fit[["ci.ub_unadj"]][posMean,1]
      ))
      rowAdjusted    <- c(rowAdjusted, list(
        est  = fit[["adj_est"]][posMean,1],
        se   = fit[["adj_se"]][posMean,1],
        stat = fit[["z_adj"]][posMean,1],
        pval = fit[["p_adj"]][posMean,1],
        lCI  = fit[["ci.lb_adj"]][posMean,1],
        uCI  = fit[["ci.ub_adj"]][posMean,1]
      ))
      
      noteMessages    <- .smSetNoteMessages(jaspResults, fit, options)      
      warningMessages <- .smSetWarningMessages(fit)
      for(i in seq_along(noteMessages)) {
        table$addFootnote(symbol = gettext("Note:"),    noteMessages[i])
      }
      for(i in seq_along(warningMessages)) {
        table$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
      }
      
    } else {
      table$setError(.smSetErrorMessage(fit))
    }    
  }
  
  table$addRows(rowUnadjusted)
  table$addRows(rowAdjusted)
  
  return(table)
}

.smFillHeterogeneity       <- function(jaspResults, table, fit, options) {
  
  overtitleCI <- gettext("95% Confidence Interval")
  
  table$addColumnInfo(name = "type", title = "",                        type = "string")
  table$addColumnInfo(name = "est",  title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "stat", title = gettext("z"),              type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),              type = "pvalue")
  table$addColumnInfo(name = "lCI",  title = gettext("Lower"),          type = "number", overtitle = overtitleCI)
  table$addColumnInfo(name = "uCI",  title = gettext("Upper"),          type = "number", overtitle = overtitleCI)
  
  rowREUnadjustedTau <- list(type = "Unadjusted")
  rowREAdjustedTau   <- list(type = "Adjusted")
  
  if (!is.null(fit)) {
    if (!class(fit) %in% c("simpleError","error")) {
      rowREUnadjustedTau  <- c(rowREUnadjustedTau, list(
        est  = sqrt(fit[["unadj_est"]][1,1]),
        stat = fit[["z_unadj"]][1,1],
        pval = fit[["p_unadj"]][1,1],
        lCI  = sqrt(ifelse(fit[["ci.lb_unadj"]][1,1] < 0, 0, fit[["ci.lb_unadj"]][1,1])),
        uCI  = sqrt(fit[["ci.ub_unadj"]][1,1])
      ))
      rowREAdjustedTau    <- c(rowREAdjustedTau, list(
        est  = sqrt(fit[["adj_est"]][1,1]),
        stat = fit[["z_adj"]][1,1],
        pval = fit[["p_adj"]][1,1],
        lCI  = sqrt(ifelse(fit[["ci.lb_adj"]][1,1] < 0, 0, fit[["ci.lb_adj"]][1,1])),
        uCI  = sqrt(fit[["ci.ub_adj"]][1,1])
      ))
      
      # add info that tau is on different scale if correlations were used
      if (options[["measures"]] == "correlation")
        table$addFootnote(symbol = gettext("Note:"), gettextf(
          "%s is on %s scale.", 
          "\u03C4",
          if (options[["mu_transform"]] == "cohens_d") gettext("Cohen's <em>d</em>") else gettext("Fisher's <em>z</em>")
        ))
      
      noteMessages    <- .smSetNoteMessages(jaspResults, fit, options)      
      warningMessages <- .smSetWarningMessages(fit)
      for(i in seq_along(noteMessages)) {
        table$addFootnote(symbol = gettext("Note:"),    noteMessages[i])
      }
      for(i in seq_along(warningMessages)) {
        table$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
      }
      
    } else {
      table$setError(.smSetErrorMessage(fit)) 
    }
  }
  
  table$addRows(rowREUnadjustedTau)
  table$addRows(rowREAdjustedTau)
  
  return(table)
}

.smFillWeights             <- function(jaspResults, table, fit, options) {
  
  overtitleCI <- gettext("95% Confidence Interval")
  overtitleP  <- gettext("<em>p</em>-values interval(one-sided)")
  
  table$addColumnInfo(name = "lr",   title = gettext("Lower"),          type = "number", overtitle = overtitleP)
  table$addColumnInfo(name = "ur",   title = gettext("Upper"),          type = "number", overtitle = overtitleP)
  table$addColumnInfo(name = "est",  title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",   title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat", title = gettext("z"),              type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),              type = "pvalue")
  table$addColumnInfo(name = "lCI",  title = gettext("Lower"),          type = "number", overtitle = overtitleCI)
  table$addColumnInfo(name = "uCI",  title = gettext("Upper"),          type = "number", overtitle = overtitleCI)
  
  if (!is.null(fit)) {
    if (!class(fit) %in% c("simpleError","error")) {
      
      if (fit[["fe"]]) {
        weightsAdd <- 0  
      } else {
        weightsAdd <- 1
      }
      
      for(i in 1:length(fit[["steps"]])) {
        if (i == 1) {
          tempRow <- list(
            lr   = 0,
            ur   = fit[["steps"]][1],
            est  = 1,
            se   = 0,
            lCI  = 1,
            uCI  = 1
          )           
        } else {
          tempRow <- list(
            lr   = fit[["steps"]][i-1],
            ur   = fit[["steps"]][i],
            est  = fit[["adj_est"]][i+weightsAdd,1],
            se   = fit[["adj_se"]][i+weightsAdd,1],
            stat = fit[["z_adj"]][i+weightsAdd,1],
            pval = fit[["p_adj"]][i+weightsAdd,1],
            lCI  = ifelse(fit[["ci.lb_adj"]][i+weightsAdd,1] < 0, 0, fit[["ci.lb_adj"]][i+weightsAdd,1]),
            uCI  = fit[["ci.ub_adj"]][i+weightsAdd,1]
          ) 
        }
        table$addRows(tempRow)
      }
      
      noteMessages    <- .smSetNoteMessages(jaspResults, fit, options)
      warningMessages <- .smSetWarningMessages(fit)
      for(i in seq_along(noteMessages)) {
        table$addFootnote(symbol = gettext("Note:"),    noteMessages[i])
      }
      for(i in seq_along(warningMessages)) {
        table$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
      }
      
    } else {
      table$setError(.smSetErrorMessage(fit)) 
    }
  }
  
  return(table)
}

.smFit                     <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["models"]])) {
    return()
  } else {
    models <- createJaspState()
    models$dependOn(c(.smDependencies, "p_table"))
    jaspResults[["models"]] <- models
  }
  
  # get the p-value steps
  steps <- .smGetCutoffs(options)
  
  # get the p-values
  pval  <- .smGetInputPval(dataset, options)
  
  # remove intervals that do not contain enought(3) p-values
  if (options[["auto_reduce"]]) {
    steps <- tryCatch(.smJoinCutoffs(steps, pval), error = function(e)e)
    
    if (class(steps) %in% c("simpleError","error")) {
      models[["object"]] <- list(
        FE = steps,
        RE = steps
      )
      return()
    }
  }
  
  # fit the models
  fit_FE <- tryCatch(weightr::weightfunct(
    effect = .smGetInputES(dataset, options),
    v      = .smGetInputVAR(dataset, options),
    pval   = pval,
    steps  = steps,
    fe     = TRUE
  ),error = function(e)e)
  
  fit_RE <- tryCatch(weightr::weightfunct(
    effect = .smGetInputES(dataset, options),
    v      = .smGetInputVAR(dataset, options),
    pval   = pval,
    steps  = steps,
    fe     = FALSE
  ),error = function(e)e)
  
  # take care of the possibly turned estimates
  if (options[["effect_direction"]] == "negative") {
    fit_FE <- .smTurnEstimatesDirection(fit_FE)
    fit_RE <- .smTurnEstimatesDirection(fit_RE)  
  }
  
  # take care of the transformed estimates
  if (options[["measures"]] == "correlation") {
    fit_FE <- .smTransformEstimates(fit_FE, options)
    fit_RE <- .smTransformEstimates(fit_RE, options)
  }
  
  models[["object"]] <- list(
    FE = fit_FE,
    RE = fit_RE
  )
  
  return()
}

.smTestsTables             <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["fitTests"]])) {
    return()
  } else {
    # create container
    fitTests <- createJaspContainer(title = gettext("Model Tests"))
    fitTests$position <- 1
    fitTests$dependOn(.smDependencies)
    jaspResults[["fitTests"]] <- fitTests
  }
  
  models   <- jaspResults[["models"]]$object
  errorFE <- class(models$FE) %in% c("simpleError","error")
  errorRE <- class(models$RE) %in% c("simpleError","error")
  
  
  ### test of heterogeneity
  heterogeneityTest <- createJaspTable(title = gettext("Test of Heterogeneity"))
  heterogeneityTest$position <- 1
  fitTests[["heterogeneityTest"]] <- heterogeneityTest
  
  heterogeneityTest$addColumnInfo(name = "stat",  title = gettext("Q"),  type = "number")
  heterogeneityTest$addColumnInfo(name = "df",    title = gettext("df"), type = "integer")
  heterogeneityTest$addColumnInfo(name = "pval",  title = gettext("p"),  type = "pvalue")
  
  if (!is.null(models)) {
    
    rowHeterogeneity      <- list()
    
    if (!errorFE) {
      rowHeterogeneity    <- c(rowHeterogeneity, list(
        stat = models[["FE"]][["QE"]],
        df   =(models[["FE"]][["k"]] - models[["FE"]][["npred"]] - 1),
        pval = models[["FE"]][["QEp"]]
      ))
    } else if (!errorRE) {
      rowHeterogeneity    <- c(rowHeterogeneity, list(
        stat = models[["RE"]][["QE"]],
        df   =(models[["RE"]][["k"]] - models[["RE"]][["npred"]] - 1),
        pval = models[["RE"]][["QEp"]]
      ))
    }
    
    heterogeneityTest$addRows(rowHeterogeneity)
    
    noteMessages <- unique(c(
      .smSetNoteMessages(jaspResults, models[["FE"]], options), .smSetNoteMessages(jaspResults, models[["RE"]], options)
    ))
    warningMessages <- unique(c(
      .smSetWarningMessages(models[["FE"]]), .smSetWarningMessages(models[["RE"]])
    ))
    errorMessages   <- unique(c(
      .smSetErrorMessage(models[["FE"]], "FE"), .smSetErrorMessage(models[["RE"]], "RE")
    ))
    for(i in seq_along(noteMessages)) {
      heterogeneityTest$addFootnote(symbol = gettext("Note:"), noteMessages[i])
    }
    for(i in seq_along(warningMessages)) {
      heterogeneityTest$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
    }
    for(i in seq_along(errorMessages)) {
      heterogeneityTest$addFootnote(symbol = gettext("Error:"),   errorMessages[i])
    }
  } else {
    if (!.smCheckReady(options) && options[["input_p"]] != "")
      heterogeneityTest$addFootnote(symbol = gettext("Note:"), .smSetNoteMessages(NULL, NULL, options))
  }
  
  
  ### test of bias
  biasTest <- createJaspTable(title = gettext("Test of Publication Bias"))
  biasTest$position <- 2
  fitTests[["biasTest"]] <- biasTest
  
  biasTest$addColumnInfo(name = "type",  title = "",                type = "string")
  biasTest$addColumnInfo(name = "stat",  title = gettext("ChiSq"),  type = "number")
  biasTest$addColumnInfo(name = "df",    title = gettext("df"),     type = "integer")
  biasTest$addColumnInfo(name = "pval",  title = gettext("p"),      type = "pvalue")
  
  if (!is.null(models)) {
    
    rowBiasHomogeneity   <- list(type = gettext("Assuming homogeneity"))
    rowBiasHeterogeneity <- list(type = gettext("Assuming heterogeneity"))
    
    if (!errorFE) {
      rowBiasHomogeneity <- c(rowBiasHomogeneity, list(
        stat = 2*abs(models[["FE"]][["output_unadj"]][["value"]] - models[["FE"]][["output_adj"]][["value"]]),
        df   = length(models[["FE"]][["output_adj"]][["par"]]) - length(models[["FE"]][["output_unadj"]][["par"]]),
        pval = pchisq(
          2*abs(models[["FE"]][["output_unadj"]][["value"]] - models[["FE"]][["output_adj"]][["value"]]), 
          length(models[["FE"]][["output_adj"]][["par"]]) - length(models[["FE"]][["output_unadj"]][["par"]]),
          lower.tail = FALSE
        )
      ))
    }
    if (!errorRE) {
      rowBiasHeterogeneity <- c(rowBiasHeterogeneity, list(
        stat = 2*abs(models[["RE"]][["output_unadj"]][["value"]] - models[["RE"]][["output_adj"]][["value"]]),
        df   = length(models[["RE"]][["output_adj"]][["par"]]) - length(models[["RE"]][["output_unadj"]][["par"]]),
        pval = pchisq(
          2*abs(models[["RE"]][["output_unadj"]][["value"]] - models[["RE"]][["output_adj"]][["value"]]), 
          length(models[["RE"]][["output_adj"]][["par"]]) - length(models[["RE"]][["output_unadj"]][["par"]]),
          lower.tail = FALSE
        )
      ))
    }
    
    biasTest$addRows(rowBiasHomogeneity)
    biasTest$addRows(rowBiasHeterogeneity)
    
    noteMessages <- unique(c(
      .smSetNoteMessages(jaspResults, models[["FE"]], options), .smSetNoteMessages(jaspResults, models[["RE"]], options)
    ))
    warningMessages <- unique(c(
      .smSetWarningMessages(models[["FE"]]), .smSetWarningMessages(models[["RE"]])
    ))
    errorMessages   <- unique(c(
      .smSetErrorMessage(models[["FE"]], "FE"), .smSetErrorMessage(models[["RE"]], "RE")
    ))
    for(i in seq_along(noteMessages)) {
      biasTest$addFootnote(symbol = gettext("Note:"),    noteMessages[i])
    }
    for(i in seq_along(warningMessages)) {
      biasTest$addFootnote(symbol = gettext("Warning:"), warningMessages[i])
    }
    for(i in seq_along(errorMessages)) {
      biasTest$addFootnote(symbol = gettext("Error:"),   errorMessages[i])
    }
    
  } else {
    if (!.smCheckReady(options) && options[["input_p"]] != "")
      biasTest$addFootnote(symbol = gettext("Note:"), .smSetNoteMessages(NULL, NULL, options))
  }
  
  return()
}

.smEstimatesTables         <- function(jaspResults, dataset, options) {
  
  models   <- jaspResults[["models"]]$object
  
  ### assuming homogeneity
  if (is.null(jaspResults[["estimatesFE"]])) {
    # create container
    estimatesFE <- createJaspContainer(title = gettext("Fixed Effects Estimates"))
    estimatesFE$position <- 2
    estimatesFE$dependOn(c(.smDependencies, "estimatesFE"))
    jaspResults[["estimatesFE"]] <- estimatesFE
  } else {
    estimatesFE <- jaspResults[["estimatesFE"]]    
  }
  
  # mean estimates
  if (is.null(estimatesFE[["estimatesFE"]]) && options[["FE_estimates"]]) {
    estimatesMeanFE <- createJaspTable(title = gettextf(
      "Mean Estimates(%s)",
      if (options[["measures"]] == "correlation") "\u03C1" else "\u03BC"
    ))
    estimatesMeanFE$position  <- 1
    estimatesFE[["meanFE"]] <- estimatesMeanFE
    meanFE <- .smFillEstimates(jaspResults, estimatesMeanFE, models[["FE"]], options)    
  }
  
  # weights estimates
  if (is.null(estimatesFE[["weightsFE"]]) && options[["FE_weights"]] && options[["FE_estimates"]]) {
    weightsFE <- createJaspTable(title = gettext("Estimated Weights"))
    weightsFE$position  <- 2
    weightsFE$dependOn(c("weightsFE"))
    estimatesFE[["weightsFE"]] <- weightsFE
    weightsFE <- .smFillWeights(jaspResults, weightsFE, models[["FE"]], options)
  }
  
  
  ### assuming heterogeneity
  if (is.null(jaspResults[["estimatesRE"]])) {
    # create container
    estimatesRE <- createJaspContainer(title = gettext("Random Effects Estimates"))
    estimatesRE$position <- 3
    estimatesRE$dependOn(c(.smDependencies, "estimatesRE"))
    jaspResults[["estimatesRE"]] <- estimatesRE
  } else {
    estimatesRE <- jaspResults[["estimatesRE"]]    
  }
  
  # mean estimates
  if (is.null(estimatesRE[["meanRE"]]) && options[["RE_estimates"]]) {
    estimatesMeanRE <- createJaspTable(title = gettextf(
      "Mean Estimates(%s)",
      if (options[["measures"]] == "correlation") "\u03C1" else "\u03BC"
    ))
    estimatesMeanRE$position <- 1
    estimatesRE[["meanRE"]] <- estimatesMeanRE
    estimatesMeanRE <- .smFillEstimates(jaspResults, estimatesMeanRE, models[["RE"]], options)    
  }
  
  # tau estimates
  if (is.null(estimatesRE[["heterogeneityRE"]]) && options[["RE_heterogeneity"]] && options[["RE_estimates"]]) {
    heterogeneityRE <- createJaspTable(title = gettextf("Heterogeneity Estimates(%s)", "\u03C4"))
    heterogeneityRE$position <- 2
    heterogeneityRE$dependOn(c("RE_heterogeneity"))
    estimatesRE[["heterogeneityRE"]] <- heterogeneityRE
    heterogeneityRE <- .smFillHeterogeneity(jaspResults, heterogeneityRE, models[["RE"]], options)    
  }
  
  # weights estimates
  if (is.null(estimatesRE[["weightsRE"]]) && options[["RE_weights"]] && options[["RE_estimates"]]) {
    weightsRE <- createJaspTable(title = gettext("Estimated Weights"))
    weightsRE$position  <- 3
    weightsRE$dependOn(c("weightsRE"))
    estimatesRE[["weightsRE"]] <- weightsRE
    weightsRE <- .smFillWeights(jaspResults, weightsRE, models[["RE"]], options)
  }  
  
  return()
}

.smPFrequencyTable         <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["pFrequency"]])) {
    return()
  } else {
    # create container
    pFrequency <- createJaspTable(title = gettext("p-value Frequency"))
    pFrequency$position <- 4
    pFrequency$dependOn(c(.smDependencies, "p_table"))
    jaspResults[["pFrequency"]] <- pFrequency
  }
  
  overtitle <- gettext("<em>p</em>-values interval(one-sided)")
  pFrequency$addColumnInfo(name = "lowerRange", title = gettext("Lower"),     type = "number", overtitle = overtitle)
  pFrequency$addColumnInfo(name = "upperRange", title = gettext("Upper"),     type = "number", overtitle = overtitle)
  pFrequency$addColumnInfo(name = "frequency",  title = gettext("Frequency"), type = "integer")
  
  if (!.smCheckReady(options))
    return()
  
  
  models <- jaspResults[["models"]]$object
  
  # get the p-value steps and p-values(so we don't have to search for them in the models)
  steps <- .smGetCutoffs(options)
  pval  <- .smGetInputPval(dataset, options)
  
  # add a note in case that the models failed to conver due to autoreduce
  if (class(models[["FE"]]) %in% c("simpleError","error") || class(models[["RE"]]) %in% c("simpleError","error")) {
    
    if (options[["auto_reduce"]]) {
      if (class(models[["FE"]]) %in% c("simpleError","error") && class(models[["RE"]]) %in% c("simpleError","error")) {
        if (models[["FE"]]$message == "No steps") {
          pFrequency$addFootnote(gettext("There were no p-value cutoffs after their automatic reduction. The displayed frequencies correspond to the non-reduced p-value cutoffs."))
        }
      } else {
        # the failure wasn't due to the reduce - reduce the p-value cuttoffs
        steps <- .smJoinCutoffs(steps, pval)
      }
    }
  } else {
    if (options[["auto_reduce"]]) {
      steps <- .smJoinCutoffs(steps, pval)      
    }
  }
  
  steps <- c(0, steps)
  cutoffsTable <- table(cut(pval, breaks = steps))
  
  for(i in 1:length(cutoffsTable)) {
    pFrequency$addRows(list(
      lowerRange = steps[i],
      upperRange = steps[i+1],
      frequency  = cutoffsTable[i]
    ))
  }
  
  return()
}

.smWeightsPlot             <- function(jaspResults, dataset, options, type = "FE") {
  
  if (!is.null(jaspResults[[paste0(type, "_weights")]])) {
    return()
  } else {
    plotWeights <- createJaspPlot(
      title  = gettextf(
        "Weight Function(%s)",
        ifelse(type == "FE", gettext("Fixed Effects"), gettext("Random Effects"))
      ),
      width  = 500,
      height = 400)
    plotWeights$dependOn(c(.smDependencies, "rescale_weightfunction", ifelse(type == "FE", "FE_weightfunction", "RE_weightfunction")))
    plotWeights$position <- ifelse(type == "FE", 5, 6)
    jaspResults[[paste0(type, "_weights")]] <- plotWeights
  }
  
  if (!.smCheckReady(options))
    return()
  
  
  # handle errors
  fit <- jaspResults[["models"]]$object[[type]]
  if (class(fit) %in% c("simpleError","error")) {
    plotWeights$setError(.smSetErrorMessage(fit))
    return()
  }
  
  # get the weights and steps
  steps       <- c(0, fit[["steps"]])
  weightsMean <- c(1, fit[["adj_est"]][  ifelse(type == "FE", 2, 3):nrow(fit[["adj_est"]]),  1])
  weightslCI  <- c(1, fit[["ci.lb_adj"]][ifelse(type == "FE", 2, 3):nrow(fit[["ci.lb_adj"]]),1])
  weightsuCI  <- c(1, fit[["ci.ub_adj"]][ifelse(type == "FE", 2, 3):nrow(fit[["ci.ub_adj"]]),1])
  
  # handle NaN in the estimates
  if (any(c(is.nan(weightsMean), is.nan(weightslCI), is.nan(weightsuCI)))) {
    plotWeights$setError(gettext("The figure could not be created since one of the estimates is NaN."))
    return()
  }
  
  # correct the lower bound
  weightslCI[weightslCI < 0] <- 0
  
  # get the ordering for plotting
  coordOrder <- sort(rep(1:(length(steps)-1),2), decreasing = FALSE)
  stepsOrder <- c(1, sort(rep(2:(length(steps)-1), 2)), length(steps))
  
  # axis ticks
  xTics    <- trimws(steps, which = "both", whitespace = "0")
  xTics[1] <- 0
  y_tics    <- JASPgraphs::getPrettyAxisBreaks(range(c(weightsMean, weightslCI, weightsuCI)))
  xSteps   <- if (options[["rescale_weightfunction"]]) seq(0, 1, length.out = length(steps)) else steps
  
  # make the plot happen
  plot <- ggplot2::ggplot()
  # mean
  plot <- plot + ggplot2::geom_polygon(
    ggplot2::aes(
      x = c(xSteps[stepsOrder], rev(xSteps[stepsOrder])),
      y = c(weightslCI[coordOrder], rev(weightsuCI[coordOrder]))
    ),
    fill = "grey80")
  # CI
  plot <- plot +ggplot2::geom_path(
    ggplot2::aes(
      x = xSteps[stepsOrder],
      y = weightsMean[coordOrder]
    ),
    size = 1.25)
  
  plot <- plot + ggplot2::scale_x_continuous(
    gettext("P-value (One-sided)"),
    breaks = xSteps,
    labels = xTics,
    limits = c(0,1))
  plot <- plot + ggplot2::scale_y_continuous(
    gettext("Publication Probability"),
    breaks = y_tics,
    limits = range(y_tics))
  
  plot <- JASPgraphs::themeJasp(plot)
  plotWeights$plotObject <- plot
  
  return()
}

.smEstimatesPlot           <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["plotEstimates"]])) {
    return()
  } else {
    plotEstimates <- createJaspPlot(
      title  = gettextf(
        "Mean Model Estimates (%s)",
        if (options[["measures"]] == "correlation") "\u03C1" else "\u03BC"
      ),
      width  = 500,
      height = 200)
    plotEstimates$dependOn(c(.smDependencies, "plot_models"))
    plotEstimates$position <- 7
    jaspResults[["plotEstimates"]] <- plotEstimates
  }
  
  if (!.smCheckReady(options))
    return()
  
  
  # handle errors
  FE <- jaspResults[["models"]]$object[["FE"]]
  RE <- jaspResults[["models"]]$object[["RE"]]
  
  if (class(FE) %in% c("simpleError","error")) {
    plotEstimates$setError(.smSetErrorMessage(FE))
    return()
  }
  if (class(RE) %in% c("simpleError","error")) {
    plotEstimates$setError(.smSetErrorMessage(RE))
    return()
  }
  
  # get the estimates
  estimates <- data.frame(
    model = c(gettext("Fixed effects"),   gettext("Fixed effects (adjusted)"),   gettext("Random effects"),  gettext("Random effects (adjusted)")),
    mean  = c(FE[["unadj_est"]][1,1],     FE[["adj_est"]][1,1],                  RE[["unadj_est"]][2,1],     RE[["adj_est"]][2,1]),
    lCI   = c(FE[["ci.lb_unadj"]][1,1],   FE[["ci.lb_adj"]][1,1],                RE[["ci.lb_unadj"]][2,1],   RE[["ci.lb_adj"]][2,1]),
    uCI   = c(FE[["ci.ub_unadj"]][1,1],   FE[["ci.ub_adj"]][1,1],                RE[["ci.ub_unadj"]][2,1],   RE[["ci.ub_adj"]][2,1])
  )
  estimates <- estimates[4:1,]
  
  # handle NaN in the estimates
  if (any(c(is.nan(estimates[,"mean"]), is.nan(estimates[,"lCI"]), is.nan(estimates[,"uCI"]))))
    plotEstimates$setError(gettext("The figure could not be created since one of the estimates is NaN."))
  
  
  # make the plot happen
  plot <- ggplot2::ggplot()
  
  plot <- plot + ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = estimates[,"lCI"],
      xmax = estimates[,"uCI"],
      y    = 1:4
    ))
  plot   <- plot + ggplot2::geom_point(
    ggplot2::aes(
      x = estimates[,"mean"],
      y = 1:4),
    shape = 15)
  plot <- plot + ggplot2::geom_line(ggplot2::aes(x = c(0,0), y = c(.5, 4.5)), linetype = "dotted")
  plot <- plot + ggplot2::scale_x_continuous(
    gettextf("Mean Estimates (%s)", if (options[["measures"]] == "correlation") "\u03C1" else "\u03BC"),
    breaks = JASPgraphs::getPrettyAxisBreaks(range(c(0, estimates[,"lCI"], estimates[,"uCI"]))),
    limits = range(c(0, estimates[,"lCI"], estimates[,"uCI"])))
  plot <- plot + ggplot2::scale_y_continuous(
    "",
    breaks = 1:4,
    labels = estimates[,"model"],
    limits = c(0.5, 4.5))
  plot <- plot + ggplot2::theme(
    axis.ticks.y = ggplot2::element_blank()
  )
  
  plot <- JASPgraphs::themeJasp(plot, sides = "b")
  plotEstimates$plotObject <- plot
  
  return()
}

.smSetErrorMessage         <- function(fit, type = NULL) {
  
  if (class(fit) %in% c("simpleError","error")) {
    if (!is.null(type)) {
      model_type <- switch(
        type,
        "FE" = gettext("Fixed effects model: "),
        "RE" = gettext("Random effects model: ")
      )    
    } else {
      model_type <- ""
    }
    
    # add more error messages as we find them I guess
    if (fit$message == "non-finite value supplied by optim") {
      message <- gettextf("%sThe optimizer failed to find a solution. Consider re-specifying the model.", model_type)
    } else if (fit$message == "No steps") {
      message <- gettextf("%sThe automatic cutoffs selection did not find viable p-value cutoffs. Please, specify them manually.", model_type)      
    } else {
      message <- paste0(model_type, fit$message)
    }
  } else {
    message <- NULL
  }
  
  return(message)
}

.smSetWarningMessages      <- function(fit) {
  
  messages   <- NULL
  
  if (!class(fit) %in% c("simpleError","error")) {
    
    ### check for no p-value in cutoffs
    steps <- c(0, fit[["steps"]])
    pval  <- fit[["p"]]
    
    cutoffsTable <- table(cut(pval, breaks = steps))
    if (any(cutoffsTable == 0)) {
      messages <- c(messages, gettext(
        "At least one of the p-value intervals contains no effect sizes, leading to estimation problems. Consider re-specifying the cutoffs."
      ))
    } else if (any(cutoffsTable <= 3)) {
      messages <- c(messages, gettext(
        "At least one of the p-value intervals contains three or fewer effect sizes, which may lead to estimation problems. Consider re-specifying the cutoffs."
      ))
    }
    
    ### check whether the unadjusted estimates is negative - the weightr assumes that the effect sizes are in expected direction
    if (fit[["unadj_est"]][ifelse(fit[["fe"]], 1, 2),1] < 0 && is.null(fit[["estimates_turned"]])) {
      messages <- c(messages, gettext(
        "The unadjusted estimate is negative. The selection model default specification expects that the expected direction of the effect size is positive. Please, check that you specified the effect sizes correctly or change the 'Expected effect size direction' option in the 'Model' tab."
      ))
    }
    
  }
  
  return(messages)
}

.smSetNoteMessages         <- function(jaspResults, fit, options) {
  
  messages <- NULL
  
  if (!.smCheckReady(options) && options[["input_p"]] != "") {
    
    messages <- gettext("The analysis requires both 'Effect Site' and 'Effect Size Standard Error' to be specified.")
    
  } else if (!class(fit) %in% c("simpleError","error")) {
    
    # add note about the ommited steps
    steps <- fit[["steps"]]
    steps <- steps
    
    stepsSettings <- .smGetCutoffs(options)
    
    if (!all(stepsSettings %in% steps) && options[["auto_reduce"]]) {
      messages      <- c(messages, gettextf(
        "Only the following one-sided p-value cutoffs were used: %s.",
        paste(steps[steps != 1], collapse = ", ")
      ))
    }
    
    if (!is.null(jaspResults[["nOmitted"]])) {
      messages      <- c(messages, sprintf(
        ngettext(
          jaspResults[["nOmitted"]]$object,
          "%i observation was removed due to missing values.",
          "%i observations were removed due to missing values."
        ),
        jaspResults[["nOmitted"]]$object
      ))
    }
    
  }
  
  
  return(messages)
}

.smTurnEstimatesDirection  <- function(fit) {
  
  # in the case that the expected direction was negative, the estimated effect sizes will be in the opposite direction
  # this function turns them around
  
  if (!class(fit) %in% c("simpleError","error")) {
    
    if (fit[["fe"]]) {
      posMean <- 1    
    } else {
      posMean <- 2
    }
    
    fit_old <- fit
    
    fit[["unadj_est"]][posMean,1] <- fit_old[["unadj_est"]][posMean,1] * -1
    fit[["z_unadj"]][posMean,1]   <- fit_old[["z_unadj"]][posMean,1]   * -1
    fit[["ci.lb_adj"]][posMean,1] <- fit_old[["ci.ub_adj"]][posMean,1] * -1
    fit[["ci.ub_adj"]][posMean,1] <- fit_old[["ci.lb_adj"]][posMean,1] * -1
    
    fit[["adj_est"]][posMean,1]     <- fit_old[["adj_est"]][posMean,1]     * -1
    fit[["z_adj"]][posMean,1]       <- fit_old[["z_adj"]][posMean,1]       * -1
    fit[["ci.lb_unadj"]][posMean,1] <- fit_old[["ci.ub_unadj"]][posMean,1] * -1
    fit[["ci.ub_unadj"]][posMean,1] <- fit_old[["ci.lb_unadj"]][posMean,1] * -1
    
    fit[["output_unadj"]][["par"]][posMean] <- fit_old[["output_unadj"]][["par"]][posMean] * -1    
    fit[["output_adj"]][["par"]][posMean]   <- fit_old[["output_adj"]][["par"]][posMean]   * -1
    
    fit[["estimates_turned"]] <- TRUE
  }
  
  return(fit)
}

.smTransformEstimates      <- function(fit, options) {
  
  if (options[["measures"]] == "general")
    return(fit)
  
  # in the case that correlation input was used, this part will transform the results back
  # from the estimation scale to the outcome scale
  
  if (!class(fit) %in% c("simpleError","error")) {
    
    if (fit[["fe"]]) {
      posMean <- 1    
    } else {
      posMean <- 2
    }
    
    fit_old <- fit
    
    
    fit[["unadj_est"]][posMean,1] <- .smInvTransform(fit_old[["unadj_est"]][posMean,1], options[["mu_transform"]])
    fit[["ci.lb_adj"]][posMean,1] <- .smInvTransform(fit_old[["ci.lb_adj"]][posMean,1], options[["mu_transform"]])
    fit[["ci.ub_adj"]][posMean,1] <- .smInvTransform(fit_old[["ci.ub_adj"]][posMean,1], options[["mu_transform"]])
    
    fit[["adj_est"]][posMean,1]     <- .smInvTransform(fit_old[["adj_est"]][posMean,1],     options[["mu_transform"]])
    fit[["ci.lb_unadj"]][posMean,1] <- .smInvTransform(fit_old[["ci.lb_unadj"]][posMean,1], options[["mu_transform"]])
    fit[["ci.ub_unadj"]][posMean,1] <- .smInvTransform(fit_old[["ci.ub_unadj"]][posMean,1], options[["mu_transform"]])
    
    fit[["output_unadj"]][["par"]][posMean] <- .smInvTransform(fit_old[["output_unadj"]][["par"]][posMean], options[["mu_transform"]])
    fit[["output_adj"]][["par"]][posMean]   <- .smInvTransform(fit_old[["output_adj"]][["par"]][posMean],   options[["mu_transform"]])
    
    fit[["estimates_transformed"]] <- options[["mu_transform"]]
  }
  
  return(fit)
}

.smGetInputES              <- function(dataset, options) {
  
  # change the direction
  ES <- dataset[, .v(options[["input_ES"]])] * ifelse(options[["effect_direction"]] == "negative", -1, 1)
  
  # do a scale transform if neccessary
  if (options[["measures"]] == "general") {
    return(ES)
  } else if (options[["measures"]] == "correlation") {
    return(.smTransform(ES, transformation = options[["mu_transform"]], what = "ES"))
  }
}

.smGetInputVAR             <- function(dataset, options) {
  
  # change the direction
  ES <- .smGetInputES(dataset, options)
  ES <- ES * ifelse(options[["effect_direction"]] == "negative", -1, 1)
  
  # do a scale transform if neccessary
  if (options[["measures"]] == "general") {
    return(dataset[, .v(options[["input_SE"]])]^2)
  } else if (options[["measures"]] == "correlation") {
    return(.smTransform(ES, dataset[, .v(options[["input_N"]])], transformation = options[["mu_transform"]], what = "VAR"))
  }
}

.smGetInputPval            <- function(dataset, options) {
  # weightfunc uses one-sided p-values as input!
  if (options[["input_p"]] == "") {
    pval <- pnorm(
      .smGetInputES(dataset, options) / sqrt(.smGetInputVAR(dataset, options)),
      lower.tail = FALSE) 
  } else {
    pval <- dataset[, .v(options[["input_p"]])]
  }
  return(pval)
}

.smTransform               <- function(ES, N = NULL, transformation, what) {
  if (what == "ES") {
    return(switch(
      transformation,
      "cohens_d"  = psych::r2d(ES),
      "fishers_z" = psych::fisherz(ES)
    ))
  } else if (what == "VAR") {
    if (is.null(N))stop("The effect size needs to be specified for the VAR transformation.")
    return(switch(
      transformation,
      "cohens_d"  = N/(N/2)^2 + ES^2/(2*N),
      "fishers_z" = 1/(N-3)
    ))
  }
}

.smInvTransform            <- function(ES, transformation) {
  switch(
    transformation,
    "cohens_d"  = psych::d2r(ES),
    "fishers_z" = psych::fisherz2r(ES)
  )
}