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

  if(.SM_ready(options)){
    dataset <- .SM_data_get(dataset, options)
    dataset <- .SM_data_check(jaspResults, dataset, options)
  }
  
  # fit the models
  if(.SM_ready(options)).SM_fit(jaspResults, dataset, options)
  
  # main summary tables
  .SM_fit_tests(jaspResults, dataset, options)
  .SM_fit_estimates(jaspResults, dataset, options)
  
  # the p-value frequency tables
  if(options[["p_table"]]).SM_p_frequency(jaspResults, dataset, options)
  
  # figures
  if(options[["FE_weightfunction"]]).SM_weights_plot(jaspResults, dataset, options, "FE")
  if(options[["RE_weightfunction"]]).SM_weights_plot(jaspResults, dataset, options, "RE")
  if(options[["plot_models"]]).SM_estimates_plot(jaspResults, dataset, options)
  
  return()
}

.SM_dependencies <- c(
  "auto_reduce", "cutoffs_p", "selection_twosided", "effect_direction", "measures", "mu_transform",
  "input_ES", "input_SE", "input_N", "input_p"
)
.SM_ready              <- function(options){
  if(options[["measures"]] == "general"){
    return(options[["input_ES"]] != "" && options[["input_SE"]] != "")
  }else if(options[["measures"]] == "correlation"){
    return(options[["input_ES"]] != "" && options[["input_N"]] != "")
  }
}
.SM_data_get           <- function(dataset, options){
  if(!is.null(dataset)){
    return(dataset)
  }else{
    return(.readDataSetToEnd(columns.as.numeric = c(
      options[["input_ES"]],
      if(options[["input_SE"]] != "") options[["input_SE"]],
      if(options[["input_N"]] != "")  options[["input_N"]],
      if(options[["input_p"]] != "")  options[["input_p"]]
    )))
  }
}
.SM_data_check         <- function(jaspResults, dataset, options){
  
  dataset_old <- dataset
  dataset     <- na.omit(dataset)
  
  # store the number of missing values
  if(nrow(dataset_old) > nrow(dataset)){
    if(!is.null(jaspResults[["ommited_observations"]])){
      ommited_observations <- jaspResults[["ommited_observations"]]
    }else{
      ommited_observations <- createJaspState()
      ommited_observations$dependOn(c(.SM_dependencies))
      jaspResults[["ommited_observations"]] <- ommited_observations
    }
    ommited_observations$object <- nrow(dataset_old) - nrow(dataset)
  }
  
  .hasErrors(dataset               = dataset,
             type                  = c("infinity", "observations"),
             observations.amount   = "< 2",
             exitAnalysisIfErrors  = TRUE)
  
  if(options[["input_SE"]] != ""){
    .hasErrors(dataset               = dataset,
               type                  = c("negativeValues"),
               negativeValues.target = options[["input_SE"]],
               exitAnalysisIfErrors  = TRUE)
  }

  if(options[["input_N"]] != ""){
    .hasErrors(dataset               = dataset,
               type                  = c("negativeValues"),
               negativeValues.target = options[["input_N"]],
               exitAnalysisIfErrors  = TRUE)
  }
  
  if(options[["input_p"]] != ""){
    if(any(dataset[,.v(options[["input_p"]])] <= 0 | dataset[,.v(options[["input_p"]])] >= 1)){
      JASP:::.quitAnalysis(gettextf("Error in %s: All p-value needs to be between 0 and 1.", options[["input_p"]]))
    }
  }
  
  if(options[["input_ES"]] != "" && options[["measures"]] == "correlation"){
    if(any(dataset[,.v(options[["input_ES"]])] <= -1 | dataset[,.v(options[["input_ES"]])] >= 1)){
      JASP:::.quitAnalysis(gettextf("Error in %s: All correlation coefficients needs to be between -1 and 1.", options[["input_ES"]]))
    }
  }
  
  return(dataset)
}
.SM_pcutoffs_get       <- function(options){
  
  x <- options[["cutoffs_p"]]
  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")
  
  x <- strsplit(x, ",", fixed = TRUE)[[1]]
  
  x <- trimws(x, which = "both")
  x <- x[x != ""]
  
  if(anyNA(as.numeric(x)))
    JASP:::.quitAnalysis(gettext("The p-value cutoffs were set incorectly."))
  if(length(x) == 0){
    JASP:::.quitAnalysis(gettext("At least one p-value cuttoff needs to be set."))
  }
  
  x <- as.numeric(x)
  if(options[["selection_twosided"]]){
    x <- c(x/2, 1-x/2)
  }
  
  if(any(x <= 0 | x >= 1)){
    JASP:::.quitAnalysis(gettext("All p-value cutoffs needs to be between 0 and 1."))
  }
  
  x <- c(sort(x), 1)
  
  return(x)
}
.SM_autoreduce         <- function(steps, pval){
  
  ### this is probably the most complex part of the analysis
  # the fit function protests when there is any p-value interval with lower than 4 p-values
  # the autoreduce should combine all p-value intervals that have too few p-values
  
  # create p-value table
  cutoffs_table <- table(cut(pval, breaks = c(0, steps)))
  
  # start removing from the end, but never 1 at the end
  while(cutoffs_table[length(cutoffs_table)] < 4){
    
    # remove the one before the last step
    steps <- steps[-(length(steps) - 1)]
    
    # create p-value table
    cutoffs_table <- table(cut(pval, breaks = c(0, steps)))
  }
  
  # and then go from the start (go one by one - two neiboring intervals with 2 p-values will become one with 4 instead of removing all of them)
  while(any(cutoffs_table < 4)){
    
    # remove the first step that has less than 4 p-values
    steps <- steps[-which.max(cutoffs_table < 4)]
    
    # create p-value table
    cutoffs_table <- table(cut(pval, breaks = c(0, steps)))
  }
  
  # do not fit the models if there is only one p-value interval
  if(length(steps) <= 1){
    stop("No steps")
  }
  
  return(steps)
}
.SM_fill_estimates     <- function(jaspResults, table, fit, options){
  
  CI_overtitle <- gettext("95% Confidence Interval")
  
  table$addColumnInfo(name = "type", title = "",                        type = "string")
  table$addColumnInfo(name = "est",  title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",   title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat", title = gettext("z"),              type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),              type = "pvalue")
  table$addColumnInfo(name = "lCI",  title = gettext("Lower"),          type = "number", overtitle = CI_overtitle)
  table$addColumnInfo(name = "uCI",  title = gettext("Upper"),          type = "number", overtitle = CI_overtitle)
  
  row_unadjusted <- list(type = "Unadjusted")
  row_adjusted   <- list(type = "Adjusted")
  
  if(!is.null(fit)){
    if(!class(fit) %in% c("simpleError","error")){
      
      if(fit[["fe"]]){
        mean_pos <- 1    
      }else{
        mean_pos <- 2
      }
      
      row_unadjusted    <- c(row_unadjusted, list(
        est  = fit[["unadj_est"]][mean_pos,1],
        se   = fit[["unadj_se"]][mean_pos,1],
        stat = fit[["z_unadj"]][mean_pos,1],
        pval = fit[["p_unadj"]][mean_pos,1],
        lCI  = fit[["ci.lb_unadj"]][mean_pos,1],
        uCI  = fit[["ci.ub_unadj"]][mean_pos,1]
      ))
      row_adjusted    <- c(row_adjusted, list(
        est  = fit[["adj_est"]][mean_pos,1],
        se   = fit[["adj_se"]][mean_pos,1],
        stat = fit[["z_adj"]][mean_pos,1],
        pval = fit[["p_adj"]][mean_pos,1],
        lCI  = fit[["ci.lb_adj"]][mean_pos,1],
        uCI  = fit[["ci.ub_adj"]][mean_pos,1]
      ))
      
      note_messages    <- .SM_notes(jaspResults, fit, options)      
      warning_messages <- .SM_warning_messages(fit)
      for(i in seq_along(note_messages)){
        table$addFootnote(symbol = gettext("Note:"),    note_messages[i])
      }
      for(i in seq_along(warning_messages)){
        table$addFootnote(symbol = gettext("Warning:"), warning_messages[i])
      }
      
    }else{
      table$setError(.SM_error_message(fit))
    }    
  }
  
  table$addRows(row_unadjusted)
  table$addRows(row_adjusted)
  
  return(table)
}
.SM_fill_heterogeneity <- function(jaspResults, table, fit, options){
  
  CI_overtitle <- gettext("95% Confidence Interval")
  
  table$addColumnInfo(name = "type", title = "",                        type = "string")
  table$addColumnInfo(name = "est",  title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "stat", title = gettext("z"),              type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),              type = "pvalue")
  table$addColumnInfo(name = "lCI",  title = gettext("Lower"),          type = "number", overtitle = CI_overtitle)
  table$addColumnInfo(name = "uCI",  title = gettext("Upper"),          type = "number", overtitle = CI_overtitle)
  
  row_RE_unadjusted_tau <- list(type = "Unadjusted")
  row_RE_adjusted_tau   <- list(type = "Adjusted")
  
  if(!is.null(fit)){
    if(!class(fit) %in% c("simpleError","error")){
      row_RE_unadjusted_tau  <- c(row_RE_unadjusted_tau, list(
        est  = sqrt(fit[["unadj_est"]][1,1]),
        stat = fit[["z_unadj"]][1,1],
        pval = fit[["p_unadj"]][1,1],
        lCI  = sqrt(ifelse(fit[["ci.lb_unadj"]][1,1] < 0, 0, fit[["ci.lb_unadj"]][1,1])),
        uCI  = sqrt(fit[["ci.ub_unadj"]][1,1])
      ))
      row_RE_adjusted_tau    <- c(row_RE_adjusted_tau, list(
        est  = sqrt(fit[["adj_est"]][1,1]),
        stat = fit[["z_adj"]][1,1],
        pval = fit[["p_adj"]][1,1],
        lCI  = sqrt(ifelse(fit[["ci.lb_adj"]][1,1] < 0, 0, fit[["ci.lb_adj"]][1,1])),
        uCI  = sqrt(fit[["ci.ub_adj"]][1,1])
      ))
      
      # add info that tau is on different scale if correlations were used
      if(options[["measures"]] == "correlation")
        table$addFootnote(symbol = gettext("Note:"), gettextf(
          "%s is on %s scale.", 
          "\u03C4",
          if(options[["mu_transform"]] == "cohens_d") gettext("Cohen's <em>d</em>") else gettext("Fisher's <em>z</em>")
          )
        )
      
      note_messages    <- .SM_notes(jaspResults, fit, options)      
      warning_messages <- .SM_warning_messages(fit)
      for(i in seq_along(note_messages)){
        table$addFootnote(symbol = gettext("Note:"),    note_messages[i])
      }
      for(i in seq_along(warning_messages)){
        table$addFootnote(symbol = gettext("Warning:"), warning_messages[i])
      }
      
    }else{
      table$setError(.SM_error_message(fit)) 
    }
  }
  
  table$addRows(row_RE_unadjusted_tau)
  table$addRows(row_RE_adjusted_tau)
  
  return(table)
}
.SM_fill_weights       <- function(jaspResults, table, fit, options){
  
  CI_overtitle <- gettext("95% Confidence Interval")
  p_overtitle  <- gettext("<em>p</em>-values interval (one-sided)")
  
  table$addColumnInfo(name = "lr",   title = gettext("Lower"),          type = "number", overtitle = p_overtitle)
  table$addColumnInfo(name = "ur",   title = gettext("Upper"),          type = "number", overtitle = p_overtitle)
  table$addColumnInfo(name = "est",  title = gettext("Estimate"),       type = "number")
  table$addColumnInfo(name = "se",   title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "stat", title = gettext("z"),              type = "number")
  table$addColumnInfo(name = "pval", title = gettext("p"),              type = "pvalue")
  table$addColumnInfo(name = "lCI",  title = gettext("Lower"),          type = "number", overtitle = CI_overtitle)
  table$addColumnInfo(name = "uCI",  title = gettext("Upper"),          type = "number", overtitle = CI_overtitle)
  
  if(!is.null(fit)){
    if(!class(fit) %in% c("simpleError","error")){
      
      if(fit[["fe"]]){
        weights_add <- 0  
      }else{
        weights_add <- 1
      }
      
      for(i in 1:length(fit[["steps"]])){
        if(i == 1){
          temp_row <- list(
            lr   = 0,
            ur   = fit[["steps"]][1],
            est  = 1,
            se   = 0,
            lCI  = 1,
            uCI  = 1
          )           
        }else{
          temp_row <- list(
            lr   = fit[["steps"]][i-1],
            ur   = fit[["steps"]][i],
            est  = fit[["adj_est"]][i+weights_add,1],
            se   = fit[["adj_se"]][i+weights_add,1],
            stat = fit[["z_adj"]][i+weights_add,1],
            pval = fit[["p_adj"]][i+weights_add,1],
            lCI  = ifelse(fit[["ci.lb_adj"]][i+weights_add,1] < 0, 0, fit[["ci.lb_adj"]][i+weights_add,1]),
            uCI  = fit[["ci.ub_adj"]][i+weights_add,1]
          ) 
        }
        table$addRows(temp_row)
      }
      
      note_messages    <- .SM_notes(jaspResults, fit, options)
      warning_messages <- .SM_warning_messages(fit)
      for(i in seq_along(note_messages)){
        table$addFootnote(symbol = gettext("Note:"),    note_messages[i])
      }
      for(i in seq_along(warning_messages)){
        table$addFootnote(symbol = gettext("Warning:"), warning_messages[i])
      }
      
    }else{
      table$setError(.SM_error_message(fit)) 
    }
  }
  
  return(table)
}
.SM_fit                <- function(jaspResults, dataset, options){

  if(!is.null(jaspResults[["models"]])){
    return()
  }else{
    models <- createJaspState()
    models$dependOn(c(.SM_dependencies, "p_table"))
    jaspResults[["models"]] <- models
  }
  
  # get the p-value steps
  steps <- .SM_pcutoffs_get(options)
  
  # get the p-values
  pval  <- .SM_model_input_pval(dataset, options)
  
  # remove intervals that do not contain enought (3) p-values
  if(options[["auto_reduce"]]){
    steps <- tryCatch(.SM_autoreduce(steps, pval), error = function(e)e)
    
    if(class(steps) %in% c("simpleError","error")){
      models[["object"]] <- list(
        FE = steps,
        RE = steps
      )
      return()
    }
  }

  # fit the models
  fit_FE <- tryCatch(weightr::weightfunct(
    effect = .SM_model_input_ES(dataset, options),
    v      = .SM_model_input_VAR(dataset, options),
    pval   = pval,
    steps  = steps,
    fe     = TRUE
  ),error = function(e)e)
  
  fit_RE <- tryCatch(weightr::weightfunct(
    effect = .SM_model_input_ES(dataset, options),
    v      = .SM_model_input_VAR(dataset, options),
    pval   = pval,
    steps  = steps,
    fe     = FALSE
  ),error = function(e)e)
  
  # take care of the possibly turned estimates
  if(options[["effect_direction"]] == "negative"){
    fit_FE <- .SM_turn_estimates(fit_FE)
    fit_RE <- .SM_turn_estimates(fit_RE)  
  }
  
  # take care of the transformed estimates
  if(options[["measures"]] == "correlation"){
    fit_FE <- .SM_transform_estimates(fit_FE, options)
    fit_RE <- .SM_transform_estimates(fit_RE, options)
  }
  
  models[["object"]] <- list(
    FE = fit_FE,
    RE = fit_RE
  )
  
  return()
}
.SM_fit_tests          <- function(jaspResults, dataset, options){
  
  if(!is.null(jaspResults[["fit_tests"]])){
    return()
  }else{
    # create container
    fit_tests <- createJaspContainer(title = gettext("Model Tests"))
    fit_tests$position <- 1
    fit_tests$dependOn(.SM_dependencies)
    jaspResults[["fit_tests"]] <- fit_tests
  }
  
  models   <- jaspResults[["models"]]$object
  FE_error <- class(models$FE) %in% c("simpleError","error")
  RE_error <- class(models$RE) %in% c("simpleError","error")
  
  
  ### test of heterogeneity
  heterogeneity_test <- createJaspTable(title = gettext("Test of Heterogeneity"))
  heterogeneity_test$position <- 1
  fit_tests[["heterogeneity_test"]] <- heterogeneity_test
  
  heterogeneity_test$addColumnInfo(name = "stat",  title = gettext("Q"),  type = "number")
  heterogeneity_test$addColumnInfo(name = "df",    title = gettext("df"), type = "integer")
  heterogeneity_test$addColumnInfo(name = "pval",  title = gettext("p"),  type = "pvalue")
  
  if(!is.null(models)){
    
    row_heterogeneity      <- list()
    
    if(!FE_error){
      row_heterogeneity    <- c(row_heterogeneity, list(
        stat = models[["FE"]][["QE"]],
        df   = (models[["FE"]][["k"]] - models[["FE"]][["npred"]] - 1),
        pval = models[["FE"]][["QEp"]]
      ))
    }else if(!RE_error){
      row_heterogeneity    <- c(row_heterogeneity, list(
        stat = models[["RE"]][["QE"]],
        df   = (models[["RE"]][["k"]] - models[["RE"]][["npred"]] - 1),
        pval = models[["RE"]][["QEp"]]
      ))
    }
    
    heterogeneity_test$addRows(row_heterogeneity)
    
    note_messages <- unique(c(
      .SM_notes(jaspResults, models[["FE"]], options), .SM_notes(jaspResults, models[["RE"]], options)
    ))
    warning_messages <- unique(c(
      .SM_warning_messages(models[["FE"]]), .SM_warning_messages(models[["RE"]])
    ))
    error_messages   <- unique(c(
      .SM_error_message(models[["FE"]], "FE"), .SM_error_message(models[["RE"]], "RE")
    ))
    for(i in seq_along(note_messages)){
      heterogeneity_test$addFootnote(symbol = gettext("Note:"), note_messages[i])
    }
    for(i in seq_along(warning_messages)){
      heterogeneity_test$addFootnote(symbol = gettext("Warning:"), warning_messages[i])
    }
    for(i in seq_along(error_messages)){
      heterogeneity_test$addFootnote(symbol = gettext("Error:"),   error_messages[i])
    }
  }else{
    if(!.SM_ready(options) && options[["input_p"]] != ""){
      heterogeneity_test$addFootnote(symbol = gettext("Note:"), .SM_notes(NULL, NULL, options))
    }
  }
  
  
  ### test of bias
  bias_test <- createJaspTable(title = gettext("Test of Publication Bias"))
  bias_test$position <- 2
  fit_tests[["bias_test"]] <- bias_test
  
  bias_test$addColumnInfo(name = "type",  title = "",                type = "string")
  bias_test$addColumnInfo(name = "stat",  title = gettext("ChiSq"),  type = "number")
  bias_test$addColumnInfo(name = "df",    title = gettext("df"),     type = "integer")
  bias_test$addColumnInfo(name = "pval",  title = gettext("p"),      type = "pvalue")
  
  if(!is.null(models)){
    
    row_bias_homogeneity   <- list(type = gettext("Assuming homogeneity"))
    row_bias_heterogeneity <- list(type = gettext("Assuming heterogeneity"))
    
    if(!FE_error){
      row_bias_homogeneity <- c(row_bias_homogeneity, list(
        stat = 2*abs(models[["FE"]][["output_unadj"]][["value"]] - models[["FE"]][["output_adj"]][["value"]]),
        df   = length(models[["FE"]][["output_adj"]][["par"]]) - length(models[["FE"]][["output_unadj"]][["par"]]),
        pval = pchisq(
          2*abs(models[["FE"]][["output_unadj"]][["value"]] - models[["FE"]][["output_adj"]][["value"]]), 
          length(models[["FE"]][["output_adj"]][["par"]]) - length(models[["FE"]][["output_unadj"]][["par"]]),
          lower.tail = FALSE
        )
      ))
    }
    if(!RE_error){
      row_bias_heterogeneity <- c(row_bias_heterogeneity, list(
        stat = 2*abs(models[["RE"]][["output_unadj"]][["value"]] - models[["RE"]][["output_adj"]][["value"]]),
        df   = length(models[["RE"]][["output_adj"]][["par"]]) - length(models[["RE"]][["output_unadj"]][["par"]]),
        pval = pchisq(
          2*abs(models[["RE"]][["output_unadj"]][["value"]] - models[["RE"]][["output_adj"]][["value"]]), 
          length(models[["RE"]][["output_adj"]][["par"]]) - length(models[["RE"]][["output_unadj"]][["par"]]),
          lower.tail = FALSE
        )
      ))
    }
    
    bias_test$addRows(row_bias_homogeneity)
    bias_test$addRows(row_bias_heterogeneity)
    
    note_messages <- unique(c(
      .SM_notes(jaspResults, models[["FE"]], options), .SM_notes(jaspResults, models[["RE"]], options)
    ))
    warning_messages <- unique(c(
      .SM_warning_messages(models[["FE"]]), .SM_warning_messages(models[["RE"]])
    ))
    error_messages   <- unique(c(
      .SM_error_message(models[["FE"]], "FE"), .SM_error_message(models[["RE"]], "RE")
    ))
    for(i in seq_along(note_messages)){
      bias_test$addFootnote(symbol = gettext("Note:"),    note_messages[i])
    }
    for(i in seq_along(warning_messages)){
      bias_test$addFootnote(symbol = gettext("Warning:"), warning_messages[i])
    }
    for(i in seq_along(error_messages)){
      bias_test$addFootnote(symbol = gettext("Error:"),   error_messages[i])
    }
    
  }else{
    if(!.SM_ready(options) && options[["input_p"]] != ""){
      bias_test$addFootnote(symbol = gettext("Note:"), .SM_notes(NULL, NULL, options))
    }
  }
  
  return()
}
.SM_fit_estimates      <- function(jaspResults, dataset, options){
  
  models   <- jaspResults[["models"]]$object
  
  ### assuming homogeneity
  if(is.null(jaspResults[["FE_estimates"]])){
    # create container
    FE_estimates <- createJaspContainer(title = gettext("Fixed Effects Estimates"))
    FE_estimates$position <- 2
    FE_estimates$dependOn(c(.SM_dependencies, "FE_estimates"))
    jaspResults[["FE_estimates"]] <- FE_estimates
  }else{
    FE_estimates <- jaspResults[["FE_estimates"]]    
  }
  
  # mean estimates
  if(is.null(FE_estimates[["FE_estimates"]]) && options[["FE_estimates"]]){
    FE_estimates_mean <- createJaspTable(title = gettextf(
      "Mean Estimates (%s)",
      ifelse(options[["measures"]] == "correlation", "\u03C1" , "\u03BC")
    ))
    FE_estimates_mean$position  <- 1
    FE_estimates[["FE_mean"]] <- FE_estimates_mean
    FE_mean <- .SM_fill_estimates(jaspResults, FE_estimates_mean, models[["FE"]], options)    
  }
  
  # weights estimates
  if(is.null(FE_estimates[["FE_weights"]]) && options[["FE_weights"]] && options[["FE_estimates"]]){
    FE_weights <- createJaspTable(title = gettext("Estimated Weights"))
    FE_weights$position  <- 2
    FE_weights$dependOn(c("FE_weights"))
    FE_estimates[["FE_weights"]] <- FE_weights
    FE_weights <- .SM_fill_weights(jaspResults, FE_weights, models[["FE"]], options)
  }
  
  
  ### assuming heterogeneity
  if(is.null(jaspResults[["RE_estimates"]])){
    # create container
    RE_estimates <- createJaspContainer(title = gettext("Random Effects Estimates"))
    RE_estimates$position <- 3
    RE_estimates$dependOn(c(.SM_dependencies, "RE_estimates"))
    jaspResults[["RE_estimates"]] <- RE_estimates
  }else{
    RE_estimates <- jaspResults[["RE_estimates"]]    
  }
  
  # mean estimates
  if(is.null(RE_estimates[["RE_mean"]]) && options[["RE_estimates"]]){
    RE_estimates_mean <- createJaspTable(title = gettextf(
      "Mean Estimates (%s)",
      ifelse(options[["measures"]] == "correlation", "\u03C1" , "\u03BC")
    ))
    RE_estimates_mean$position <- 1
    RE_estimates[["RE_mean"]] <- RE_estimates_mean
    RE_estimates_mean <- .SM_fill_estimates(jaspResults, RE_estimates_mean, models[["RE"]], options)    
  }
  
  # tau estimates
  if(is.null(RE_estimates[["RE_estimates_tau"]]) && options[["RE_heterogeneity"]] && options[["RE_estimates"]]){
    RE_estimates_tau <- createJaspTable(title = gettextf("Heterogeneity Estimates (%s)", "\u03C4"))
    RE_estimates_tau$position <- 2
    RE_estimates_tau$dependOn(c("RE_heterogeneity"))
    RE_estimates[["RE_estimates_tau"]] <- RE_estimates_tau
    RE_estimates_tau <- .SM_fill_heterogeneity(jaspResults, RE_estimates_tau, models[["RE"]], options)    
  }
  
  # weights estimates
  if(is.null(RE_estimates[["RE_weights"]]) && options[["RE_weights"]] && options[["RE_estimates"]]){
    RE_weights <- createJaspTable(title = gettext("Estimated Weights"))
    RE_weights$position  <- 3
    RE_weights$dependOn(c("RE_weights"))
    RE_estimates[["RE_weights"]] <- RE_weights
    RE_weights <- .SM_fill_weights(jaspResults, RE_weights, models[["RE"]], options)
  }  
  
  return()
}
.SM_p_frequency        <- function(jaspResults, dataset, options){
  
  if(!is.null(jaspResults[["p_frequency"]])){
    return()
  }else{
    # create container
    p_frequency <- createJaspTable(title = gettext("p-value Frequency"))
    p_frequency$position <- 4
    p_frequency$dependOn(c(.SM_dependencies, "p_table"))
    jaspResults[["p_frequency"]] <- p_frequency
  }
  
  overtitle <- gettext("<em>p</em>-values interval (one-sided)")
  p_frequency$addColumnInfo(name = "lowerRange", title = gettext("Lower"),     type = "number", overtitle = overtitle)
  p_frequency$addColumnInfo(name = "upperRange", title = gettext("Upper"),     type = "number", overtitle = overtitle)
  p_frequency$addColumnInfo(name = "frequency",  title = gettext("Frequency"), type = "integer")
  
  if(!.SM_ready(options)){
    return()
  }
  
  models <- jaspResults[["models"]]$object
  
  # get the p-value steps and p-values (so we don't have to search for them in the models)
  steps <- .SM_pcutoffs_get(options)
  pval  <- .SM_model_input_pval(dataset, options)
  
  # add a note in case that the models failed to conver due to autoreduce
  if(class(models[["FE"]]) %in% c("simpleError","error") || class(models[["RE"]]) %in% c("simpleError","error")){
    
    if(options[["auto_reduce"]]){
      if(class(models[["FE"]]) %in% c("simpleError","error") && class(models[["RE"]]) %in% c("simpleError","error")){
        if(models[["FE"]]$message == "No steps"){
          p_frequency$addFootnote(gettext("There were no p-value cutoffs after their automatic reduction. The displayed frequencies correspond to the non-reduced p-value cutoffs."))
        }
      }else{
        # the failure wasn't due to the reduce - reduce the p-value cuttoffs
        steps <- .SM_autoreduce(steps, pval)
      }
    }
  }else{
    if(options[["auto_reduce"]]){
      steps <- .SM_autoreduce(steps, pval)      
    }
  }
  
  steps <- c(0, steps)
  cutoffs_table <- table(cut(pval, breaks = steps))
  
  for(i in 1:length(cutoffs_table)){
    p_frequency$addRows(list(
      lowerRange = steps[i],
      upperRange = steps[i+1],
      frequency  = cutoffs_table[i]
    ))
  }
  
  return()
}
.SM_weights_plot       <- function(jaspResults, dataset, options, type = "FE"){
  
  if(!is.null(jaspResults[[paste0(type, "_weights")]])){
    return()
  }else{
    weights_plot <- createJaspPlot(
      title  = gettextf(
        "Weight Function (%s)",
        ifelse(type == "FE", gettext("Fixed Effects"), gettext("Random Effects"))
      ),
      width  = 500,
      height = 400)
    weights_plot$dependOn(c(.SM_dependencies, "rescale_weightfunction", ifelse(type == "FE", "FE_weightfunction", "RE_weightfunction")))
    weights_plot$position <- ifelse(type == "FE", 5, 6)
    jaspResults[[paste0(type, "_weights")]] <- weights_plot
  }
  
  if(!.SM_ready(options)){
    return()
  }
  
  # handle errors
  fit <- jaspResults[["models"]]$object[[type]]
  if(class(fit) %in% c("simpleError","error")){
    weights_plot$setError(.SM_error_message(fit))
    return()
  }
  
  # get the weights and steps
  steps        <- c(0, fit[["steps"]])
  weights_mean <- c(1, fit[["adj_est"]][  ifelse(type == "FE", 2, 3):nrow(fit[["adj_est"]]),  1])
  weights_lCI  <- c(1, fit[["ci.lb_adj"]][ifelse(type == "FE", 2, 3):nrow(fit[["ci.lb_adj"]]),1])
  weights_uCI  <- c(1, fit[["ci.ub_adj"]][ifelse(type == "FE", 2, 3):nrow(fit[["ci.ub_adj"]]),1])
  
  # handle NaN in the estimates
  if(any(c(is.nan(weights_mean), is.nan(weights_lCI), is.nan(weights_uCI)))){
    weights_plot$setError(gettext("The figure could not be created since one of the estimates is NaN."))
    return()
  }
  
  # correct the lower bound
  weights_lCI[weights_lCI < 0] <- 0
  
  # get the ordering for plotting
  coord_order <- sort(rep(1:(length(steps)-1),2), decreasing = FALSE)
  steps_order <- c(1, sort(rep(2:(length(steps)-1), 2)), length(steps))
  
  # axis ticks
  x_tics    <- trimws(steps, which = "both", whitespace = "0")
  x_tics[1] <- 0
  y_tics    <- JASPgraphs::getPrettyAxisBreaks(range(c(weights_mean, weights_lCI, weights_uCI)))
  x_steps   <- if(options[["rescale_weightfunction"]]) seq(0, 1, length.out = length(steps)) else steps
  
  # make the plot happen
  plot <- ggplot2::ggplot()
  # mean
  plot <- plot + ggplot2::geom_polygon(
    ggplot2::aes(
      x = c(x_steps[steps_order], rev(x_steps[steps_order])),
      y = c(weights_lCI[coord_order], rev(weights_uCI[coord_order]))
    ),
    fill = "grey80")
  # CI
  plot <- plot +ggplot2::geom_path(
    ggplot2::aes(
      x = x_steps[steps_order],
      y = weights_mean[coord_order]
    ),
    size = 1.25)
  
  plot <- plot + ggplot2::scale_x_continuous(
    gettext("P-value (One-sided)"),
    breaks = x_steps,
    labels = x_tics,
    limits = c(0,1))
  plot <- plot + ggplot2::scale_y_continuous(
    gettext("Publication Probability"),
    breaks = y_tics,
    limits = range(y_tics))
  
  plot <- JASPgraphs::themeJasp(plot)
  weights_plot$plotObject <- plot
  
  return()
}
.SM_estimates_plot     <- function(jaspResults, dataset, options){
  
  if(!is.null(jaspResults[["plot_estimates"]])){
    return()
  }else{
    plot_estimates <- createJaspPlot(
      title  = gettextf(
        "Mean Model Estimates (%s)",
        ifelse(options[["measures"]] == "correlation", "\u03C1", "\u03BC")
      ),
      width  = 500,
      height = 200)
    plot_estimates$dependOn(c(.SM_dependencies, "plot_models"))
    plot_estimates$position <- 7
    jaspResults[["plot_estimates"]] <- plot_estimates
  }
  
  if(!.SM_ready(options)){
    return()
  }
  
  # handle errors
  FE <- jaspResults[["models"]]$object[["FE"]]
  RE <- jaspResults[["models"]]$object[["RE"]]
  
  if(class(FE) %in% c("simpleError","error")){
    plot_estimates$setError(.SM_error_message(FE))
    return()
  }
  if(class(RE) %in% c("simpleError","error")){
    plot_estimates$setError(.SM_error_message(RE))
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
  if(any(c(is.nan(estimates[,"mean"]), is.nan(estimates[,"lCI"]), is.nan(estimates[,"uCI"])))){
    plot_estimates$setError(gettext("The figure could not be created since one of the estimates is NaN."))
  }
  
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
    gettextf("Mean Estimates (%s)", ifelse(options[["measures"]] == "correlation", "\u03C1" , "\u03BC")),
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
  plot_estimates$plotObject <- plot
  
  return()
}
.SM_error_message      <- function(fit, type = NULL){
  
  if(class(fit) %in% c("simpleError","error")){
    if(!is.null(type)){
      model_type <- switch(
        type,
        "FE" = gettext("Fixed effects model: "),
        "RE" = gettext("Random effects model: ")
      )    
    }else{
      model_type <- ""
    }
    
    # add more error messages as we find them I guess
    if(fit$message == "non-finite value supplied by optim"){
      message <- gettextf("%sThe optimizer failed to find a solution. Consider re-specifying the model.", model_type)
    }else if(fit$message == "No steps"){
      message <- gettextf("%sThe automatic cutoffs selection did not find viable p-value cutoffs. Please, specify them manually.", model_type)      
    }else{
      message <- paste0(model_type, fit$message)
    }
  }else{
    message <- NULL
  }
  
  return(message)
}
.SM_warning_messages   <- function(fit){
  
  messages   <- NULL
  
  if(!class(fit) %in% c("simpleError","error")){
    
    ### check for no p-value in cutoffs
    steps <- c(0, fit[["steps"]])
    pval  <- fit[["p"]]
    
    cutoffs_table <- table(cut(pval, breaks = steps))
    if(any(cutoffs_table == 0)){
      messages <- c(messages, gettext(
        "At least one of the p-value intervals contains no effect sizes, leading to estimation problems. Consider re-specifying the cutoffs."
      ))
    }else if(any(cutoffs_table <= 3)){
      messages <- c(messages, gettext(
        "At least one of the p-value intervals contains three or fewer effect sizes, which may lead to estimation problems. Consider re-specifying the cutoffs."
      ))
    }
    
    ### check whether the unadjusted estimates is negative - the weightr assumes that the effect sizes are in expected direction
    if(fit[["unadj_est"]][ifelse(fit[["fe"]], 1, 2),1] < 0 && is.null(fit[["estimates_turned"]])){
      messages <- c(messages, gettext(
        "The unadjusted estimate is negative. The selection model default specification expects that the expected direction of the effect size is positive. Please, check that you specified the effect sizes correctly or change the 'Expected effect size direction' option in the 'Model' tab."
      ))
    }
    
  }
  
  return(messages)
}
.SM_notes              <- function(jaspResults, fit, options){
  
  messages <- NULL
  
  if(!.SM_ready(options) && options[["input_p"]] != ""){
    
    messages <- gettext("The analysis requires both 'Effect Site' and 'Effect Size Standard Error' to be specified.")
    
  }else if(!class(fit) %in% c("simpleError","error")){
    
    # add note about the ommited steps
    steps <- fit[["steps"]]
    steps <- steps
    
    steps_settings <- .SM_pcutoffs_get(options)
    
    if(!all(steps_settings %in% steps) && options[["auto_reduce"]]){
      messages      <- c(messages, gettextf(
        "Only the following one-sided p-value cutoffs were used: %s.",
        paste(steps[steps != 1], collapse = ", ")
      ))
    }
    
    if(!is.null(jaspResults[["ommited_observations"]])){
      messages      <- c(messages, sprintf(
        ngettext(
          jaspResults[["ommited_observations"]]$object,
          "%i observation was removed due to missing values.",
          "%i observations were removed due to missing values."
        ),
        jaspResults[["ommited_observations"]]$object
      ))
    }
    
  }
  
  
  return(messages)
}
.SM_turn_estimates     <- function(fit){
  
  # in the case that the expected direction was negative, the estimated effect sizes will be in the opposite direction
  # this function turns them around
  
  if(!class(fit) %in% c("simpleError","error")){
    
    if(fit[["fe"]]){
      mean_pos <- 1    
    }else{
      mean_pos <- 2
    }
    
    fit_old <- fit
    
    fit[["unadj_est"]][mean_pos,1] <- fit_old[["unadj_est"]][mean_pos,1] * -1
    fit[["z_unadj"]][mean_pos,1]   <- fit_old[["z_unadj"]][mean_pos,1]   * -1
    fit[["ci.lb_adj"]][mean_pos,1] <- fit_old[["ci.ub_adj"]][mean_pos,1] * -1
    fit[["ci.ub_adj"]][mean_pos,1] <- fit_old[["ci.lb_adj"]][mean_pos,1] * -1
    
    fit[["adj_est"]][mean_pos,1]     <- fit_old[["adj_est"]][mean_pos,1]     * -1
    fit[["z_adj"]][mean_pos,1]       <- fit_old[["z_adj"]][mean_pos,1]       * -1
    fit[["ci.lb_unadj"]][mean_pos,1] <- fit_old[["ci.ub_unadj"]][mean_pos,1] * -1
    fit[["ci.ub_unadj"]][mean_pos,1] <- fit_old[["ci.lb_unadj"]][mean_pos,1] * -1
    
    fit[["output_unadj"]][["par"]][mean_pos] <- fit_old[["output_unadj"]][["par"]][mean_pos] * -1    
    fit[["output_adj"]][["par"]][mean_pos]   <- fit_old[["output_adj"]][["par"]][mean_pos]   * -1
    
    fit[["estimates_turned"]] <- TRUE
  }
  
  return(fit)
}
.SM_transform_estimates<- function(fit, options){
  
  if(options[["measures"]] == "general"){
    return(fit)
  }
  # in the case that correlation input was used, this part will transform the results back
  # from the estimation scale to the outcome scale
  
  if(!class(fit) %in% c("simpleError","error")){
    
    if(fit[["fe"]]){
      mean_pos <- 1    
    }else{
      mean_pos <- 2
    }
    
    fit_old <- fit
    
    
    fit[["unadj_est"]][mean_pos,1] <- .SM_inv_transform(fit_old[["unadj_est"]][mean_pos,1], options[["mu_transform"]])
    fit[["ci.lb_adj"]][mean_pos,1] <- .SM_inv_transform(fit_old[["ci.ub_adj"]][mean_pos,1], options[["mu_transform"]])
    fit[["ci.ub_adj"]][mean_pos,1] <- .SM_inv_transform(fit_old[["ci.lb_adj"]][mean_pos,1], options[["mu_transform"]])
    
    fit[["adj_est"]][mean_pos,1]     <- .SM_inv_transform(fit_old[["adj_est"]][mean_pos,1],     options[["mu_transform"]])
    fit[["ci.lb_unadj"]][mean_pos,1] <- .SM_inv_transform(fit_old[["ci.ub_unadj"]][mean_pos,1], options[["mu_transform"]])
    fit[["ci.ub_unadj"]][mean_pos,1] <- .SM_inv_transform(fit_old[["ci.lb_unadj"]][mean_pos,1], options[["mu_transform"]])
    
    fit[["output_unadj"]][["par"]][mean_pos] <- .SM_inv_transform(fit_old[["output_unadj"]][["par"]][mean_pos], options[["mu_transform"]])
    fit[["output_adj"]][["par"]][mean_pos]   <- .SM_inv_transform(fit_old[["output_adj"]][["par"]][mean_pos],   options[["mu_transform"]])
    
    fit[["estimates_transformed"]] <- options[["mu_transform"]]
  }
  
  return(fit)
}
.SM_model_input_ES     <- function(dataset, options){
  
  # change the direction
  ES <- dataset[,.v(options[["input_ES"]])] * ifelse(options[["effect_direction"]] == "negative", -1, 1)
  
  # do a scale transform if neccessary
  if(options[["measures"]] == "general"){
    return(ES)
  }else if(options[["measures"]] == "correlation"){
    return(.SM_transform(ES, transformation = options[["mu_transform"]], what = "ES"))
  }
}
.SM_model_input_VAR    <- function(dataset, options){
  
  # change the direction
  ES <- .SM_model_input_ES(dataset, options)
  ES <- ES * ifelse(options[["effect_direction"]] == "negative", -1, 1)
  
  # do a scale transform if neccessary
  if(options[["measures"]] == "general"){
    return(dataset[,.v(options[["input_SE"]])]^2)
  }else if(options[["measures"]] == "correlation"){
    return(.SM_transform(ES, dataset[,.v(options[["input_N"]])], transformation = options[["mu_transform"]], what = "VAR"))
  }
}
.SM_model_input_pval   <- function(dataset, options){
  # weightfunc uses one-sided p-values as input!
  if(options[["input_p"]] == ""){
    pval <- pnorm(
      .SM_model_input_ES(dataset, options) / sqrt(.SM_model_input_VAR(dataset, options)),
      lower.tail = FALSE) 
  }else{
    pval <- dataset[,.v(options[["input_p"]])]
  }
  return(pval)
}
.SM_transform          <- function(ES, N = NULL, transformation, what){
  if(what == "ES"){
    return(switch(
      transformation,
      "cohens_d"  = psych::r2d(ES),
      "fishers_z" = psych::fisherz(ES)
    ))
  }else if(what == "VAR"){
    if(is.null(N))stop("The effect size needs to be specified for the VAR transformation.")
    return(switch(
      transformation,
      "cohens_d"  = N/(N/2)^2 + ES^2/(2*N),
      "fishers_z" = 1/(N-3)
    ))
  }
}
.SM_inv_transform      <- function(ES, transformation){
  switch(
    transformation,
    "cohens_d"  = psych::d2r(ES),
    "fishers_z" = psych::fisherz2r(ES)
  )
}