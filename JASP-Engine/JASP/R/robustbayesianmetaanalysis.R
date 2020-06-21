#
# Copyright (C) 2019 University of Amsterdam
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


RobustBayesianMetaAnalysis <-
  function(jaspResults, dataset, options, state = NULL) {
    
    # clean fitted model if it was changed
    if(!.RoBMA_ready(options))
      .RoBMA_clean_model(jaspResults)
    
    # load data
    if (.RoBMA_ready(options))
      dataset <- .RoBMA_data_get(options, dataset)
    
    # get the priors
    .RoBMA_priors_get(jaspResults, options)
    
    # show the model preview
    if (is.null(jaspResults[["model"]]))
      .RoBMA_model_preview(jaspResults, options)
    
    # fit model model
    if (is.null(jaspResults[["model_notifier"]]) && .RoBMA_ready(options))
      .RoBMA_fit_model(jaspResults, dataset, options)
                  
    ### Priors plot
    if (options[["priors_plot"]])
      .RoBMA_priors_plots(jaspResults, options)
    
    ### Inference, Plots, and Diagnostics are accessible only if a model is fitted
    if (!is.null(jaspResults[["model"]])) {
      ### Inference
      # defaul summary
      .RoBMA_summary(jaspResults, options)
      # models overview
      if (options[["results_models"]])
        .RoBMA_models_overview(jaspResults, options)
      # models summary
      if (options[["results_individual"]])
        .RoBMA_models_summary(jaspResults, options)
      
      ### Plots
      # pooled estimates plots
      if (options[["plots_theta"]])
        .RoBMA_plots(jaspResults, options, "theta")
      if (options[["plots_mu"]])
        .RoBMA_plots(jaspResults, options, "mu")
      if (options[["plots_tau"]])
        .RoBMA_plots(jaspResults, options, "tau")
      if (options[["plots_tau"]] &&
          options[["plots_mu"]] &&
          options[["plots_type"]] == "averaged" &&
          !options[["plots_priors"]])
        .RoBMA_plots(jaspResults, options, c("mu", "tau"))
      if (options[["plots_omega"]])
        .RoBMA_plots(jaspResults, options, "omega")
      
      # individual models
      if (options[["plots_individual_mu"]])
        .RoBMA_individual_plots(jaspResults, options, "mu")
      if (options[["plots_individual_tau"]])
        .RoBMA_individual_plots(jaspResults, options, "tau")
      if (options[["plots_individual_omega"]])
        .RoBMA_individual_plots(jaspResults, options, "omega")
      
      ### Diagnostics
      # overview
      if (options[["diagnostics_overview"]])
        .RoBMA_diagnostics_overview(jaspResults, options)
      # plots
      if ((
        options[["diagnostics_mu"]] ||
        options[["diagnostics_tau"]] ||
        options[["diagnostics_omega"]] ||
        options[["diagnostics_theta"]]
      ) &&
      (
        options[["diagnostics_trace"]] ||
        options[["diagnostics_autocorrelation"]] ||
        options[["diagnostics_samples"]]
      ))
        .RoBMA_diagnostics_plots(jaspResults, options)
      
      ### Save the model
      if (options[["save_path"]] != "" &&
          is.null(jaspResults[["model_saved"]]))
        .RoBMA_save_model(jaspResults, options)
    }
    
    return()
  }

.RoBMA_dependencies <- c(
  "measures",
  "fitted_path",
  "cohensd_testType",
  "input_ES",
  "input_t",
  "input_SE",
  "input_CI",
  "input_N",
  "input_N1",
  "input_N2",
  "input_labels",
  "priors_mu",
  "priors_tau",
  "priors_omega",
  "priors_mu_null",
  "priors_tau_null",
  "priors_omega_null",
  "advanced_control",
  "advanced_omit_prior",
  "advanced_omit_marglik",
  "advanced_omit_theta",
  "advanced_omit_error",
  "advanced_omit_error_value",
  "advanced_omit_ESS_value",
  "advanced_omit_ESS",
  "advanced_omit_rhat_value",
  "advanced_omit_rhat",
  "advanced_omit",
  "advanced_autofit_error",
  "advanced_autofit_time_unit",
  "advanced_autofit_time",
  "advanced_autofit",
  "advanced_thin",
  "advanced_chains",
  "advanced_iteration",
  "advanced_burnin",
  "advanced_adapt",
  "advanced_bridge_iter",
  "advanced_mu_transform",
  "setSeed",
  "seed"
)
# priors related functions
.RoBMA_options2priors       <- function(options_prior) {
  options_prior <- .RoBMA_options2priors_eval(options_prior)
  
  if (options_prior[["type"]] == "normal") {
    return(
      RoBMA::prior(
        distribution = "normal",
        parameters = list(mean       = options_prior[["parMean"]],
                          sd         = options_prior[["parScale"]]),
        truncation = list(
          lower      = options_prior[["truncationLower"]],
          upper      = options_prior[["truncationUpper"]]
        ),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] == "t") {
    return(
      RoBMA::prior(
        distribution = "t",
        parameters = list(
          location   = options_prior[["parMean"]],
          scale      = options_prior[["parScale"]],
          df         = options_prior[["parDF"]]
        ),
        truncation = list(
          lower      = options_prior[["truncationLower"]],
          upper      = options_prior[["truncationUpper"]]
        ),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] == "cauchy") {
    return(
      RoBMA::prior(
        distribution = "cauchy",
        parameters = list(
          location   = options_prior[["parLocation"]],
          scale      = options_prior[["parScale2"]]
        ),
        truncation = list(
          lower      = options_prior[["truncationLower"]],
          upper      = options_prior[["truncationUpper"]]
        ),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] == "gamma_ab") {
    return(
      RoBMA::prior(
        distribution = "gamma",
        parameters = list(shape      = options_prior[["parAlpha"]],
                          rate       = options_prior[["parBeta"]]),
        truncation = list(
          lower      = options_prior[["truncationLower"]],
          upper      = options_prior[["truncationUpper"]]
        ),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] == "gamma_k0") {
    return(
      RoBMA::prior(
        distribution = "gamma",
        parameters = list(shape      = options_prior[["parShape"]],
                          scale      = options_prior[["parScale2"]]),
        truncation = list(
          lower      = options_prior[["truncationLower"]],
          upper      = options_prior[["truncationUpper"]]
        ),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] == "invgamma") {
    return(
      RoBMA::prior(
        distribution = "invgamma",
        parameters = list(shape      = options_prior[["parAlpha"]],
                          scale      = options_prior[["parBeta"]]),
        truncation = list(
          lower      = options_prior[["truncationLower"]],
          upper      = options_prior[["truncationUpper"]]
        ),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] == "spike") {
    return(
      RoBMA::prior(
        distribution = "point",
        parameters = list(location   = options_prior[["parLocation"]]),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] == "uniform") {
    return(
      RoBMA::prior(
        distribution = "uniform",
        parameters = list(a          = options_prior[["parA"]],
                          b          = options_prior[["parB"]]),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] %in% c("Two-sided", "Two-sided2")) {
    return(
      RoBMA::prior(
        distribution = "two.sided",
        parameters = list(alpha      = options_prior[["parAlpha"]],
                          steps      = options_prior[["parCuts"]]),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] == "One-sided (mon.)") {
    return(
      RoBMA::prior(
        distribution = "one.sided",
        parameters = list(alpha      = options_prior[["parAlpha"]],
                          steps      = options_prior[["parCuts"]]),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  } else if (options_prior[["type"]] == "One-sided") {
    return(
      RoBMA::prior(
        distribution = "one.sided",
        parameters = list(
          alpha1     = options_prior[["parAlpha1"]],
          alpha2     = options_prior[["parAlpha2"]],
          steps      = options_prior[["parCuts"]]
        ),
        prior_odds = options_prior[["priorOdds"]]
      )
    )
  }
}
.RoBMA_options2priors_clean <- function(x) {

  x <- trimws(x, which = "both")
  x <- trimws(x, which = "both", whitespace = "c")
  x <- trimws(x, which = "both", whitespace = "\\(")
  x <- trimws(x, which = "both", whitespace = "\\)")
  x <- trimws(x, which = "both", whitespace = ",")
  
  x <- strsplit(x, ",", fixed = TRUE)[[1]]
  
  x <- trimws(x, which = "both")
  x <- x[x != ""]
  
  if (anyNA(as.numeric(x)))
    JASP:::.quitAnalysis(gettext("The priors for publication bias were set incorrectly."))
  return(as.numeric(x))
}
.RoBMA_options2priors_eval  <- function(x) {
  if (x[["type"]] %in% c("Two-sided", "One-sided (mon.)", "One-sided")) {
    x[["priorOdds"]] <- eval(parse(text = x[["priorOdds"]]))
    x[["parAlpha"]]  <- .RoBMA_options2priors_clean(x[["parAlpha"]])
    x[["parAlpha1"]] <-
      .RoBMA_options2priors_clean(x[["parAlpha1"]])
    x[["parAlpha2"]] <-
      .RoBMA_options2priors_clean(x[["parAlpha2"]])
    x[["parCuts"]]   <- .RoBMA_options2priors_clean(x[["parCuts"]])
    
  } else if (x[["type"]] == "spike" &&
             any(names(x) %in% c("parAlpha2"))) {
    x[["priorOdds"]]   <- eval(parse(text = x[["priorOdds"]]))
    x[["parLocation"]] <- 1
    
  } else{
    eval_names <-
      c(
        "parA",
        "parB",
        "parAlpha",
        "parBeta",
        "parDF",
        "parLocation",
        "parMean",
        "parScale",
        "parScale2",
        "parShape",
        "priorOdds",
        "truncationLower",
        "truncationUpper"
      )
    
    for (n in eval_names) {
      if (!is.null(x[[n]]))
        x[[n]] <- eval(parse(text = x[[n]]))
    }
    
  }
  
  
  return(x)
}
# table filling functions
.RoBMA_table_fill_coef      <-
  function(jasp_table,
           results_table,
           add_info,
           options,
           individual = FALSE) {
    # add columns
    jasp_table$addColumnInfo(name = "terms",
                             title = "",
                             type = "string")
    jasp_table$addColumnInfo(name = "mean",
                             title = gettext("Mean"),
                             type = "number")
    jasp_table$addColumnInfo(name = "median",
                             title = gettext("Median") ,
                             type = "number")
    jasp_table$addColumnInfo(
      name = "lowerCI",
      title = gettext("Lower"),
      type = "number",
      overtitle = gettextf("%s%% CI", 100 * options[["results_CI"]])
    )
    jasp_table$addColumnInfo(
      name = "upperCI",
      title = gettext("Upper"),
      type = "number",
      overtitle = gettextf("%s%% CI", 100 * options[["results_CI"]])
    )
    if (individual) {
      jasp_table$addColumnInfo(name = "error",
                               title = gettext("MCMC error"),
                               type = "number")
      jasp_table$addColumnInfo(name = "ess",
                               title = gettext("ESS"),
                               type = "integer")
      jasp_table$addColumnInfo(name = "rhat",
                               title = gettext("Rhat"),
                               type = "number")
    }
    
    
    if (is.null(results_table)) 
      return(jasp_table)
    
    # fill rows
    for (i in c(1:nrow(results_table))[rownames(results_table) %in% c("mu", "tau")]) {
      temp_row <- list(
        terms    = .RoBMA_coef_names(rownames(results_table)[i], add_info),
        mean     = results_table[i, "Mean"],
        median   = results_table[i, "Median"],
        lowerCI  = results_table[i, if(individual) ".025" else as.character(.5 - options[["results_CI"]] / 2)],
        upperCI  = results_table[i, if(individual) ".975" else as.character(.5 + options[["results_CI"]] / 2)]
      )
      if (individual) {
        temp_row[["error"]] <- results_table[i, "MCMC error"]
        temp_row[["ess"]]   <- results_table[i, "ESS"]
        temp_row[["rhat"]]  <- results_table[i, "Rhat"]
      }
      
      jasp_table$addRows(temp_row)
    }

    
    # add footnote
    if (any(rownames(results_table) == "tau")) {
      if (add_info[["effect_size"]] == "r")
        jasp_table$addFootnote(
          gettextf("%s is on %s scale.", 
                   "\u03C4",
                   if(add_info[["mu_transform"]] == "cohens_d") gettext("Cohen's <em>d</em>") else gettext("Fisher's <em>z</em>")
          )
        )
    }
    
    return(jasp_table)
  }
.RoBMA_table_fill_weights   <-
  function(jasp_table,
           results_table,
           add_info,
           options,
           individual = FALSE) {
    # add columns
    jasp_table$addColumnInfo(
      name = "lowerRange",
      title = gettext("Lower"),
      type = "number",
      overtitle = gettextf("<em>p</em>-values interval %s","\u002A")
    )
    jasp_table$addColumnInfo(
      name = "upperRange",
      title = gettext("Upper"),
      type = "number",
      overtitle = gettextf("<em>p</em>-values interval %s", "\u002A")
    )
    jasp_table$addColumnInfo(name = "mean",
                             title = gettext("Mean"),
                             type = "number")
    jasp_table$addColumnInfo(name = "median",
                             title = gettext("Median") ,
                             type = "number")
    jasp_table$addColumnInfo(
      name = "lowerCI",
      title = gettext("Lower"),
      type = "number",
      overtitle = gettextf("%s%% CI", 100 * options[["results_CI"]])
    )
    jasp_table$addColumnInfo(
      name = "upperCI",
      title = gettext("Upper"),
      type = "number",
      overtitle = gettextf("%s%% CI", 100 * options[["results_CI"]])
    )
    if (individual) {
      jasp_table$addColumnInfo(name = "error",
                               title = gettext("MCMC error"),
                               type = "number")
      jasp_table$addColumnInfo(name = "ess",
                               title = gettext("ESS"),
                               type = "integer")
      jasp_table$addColumnInfo(name = "rhat",
                               title = gettext("Rhat"),
                               type = "number")
    }
    
    
    if (is.null(results_table))
      return(jasp_table)
    
    
    
    # fill rows
    for (i in c(1:nrow(results_table))[grepl("omega", rownames(results_table))]) {
      temp_row <- list(
        lowerRange = as.numeric(substr(
          rownames(results_table)[i],
          7,
          regexec(",", rownames(results_table)[i], fixed = TRUE)[[1]] - 1
        )),
        upperRange = as.numeric(substr(
          rownames(results_table)[i],
          regexec(",", rownames(results_table)[i], fixed = TRUE)[[1]] + 1,
          nchar(rownames(results_table)[i]) - 1
        )),
        mean       = results_table[i, "Mean"],
        median     = results_table[i, "Median"],
        lowerCI    = results_table[i, if(individual) ".025" else as.character(.5 - options[["results_CI"]] / 2)],
        upperCI    = results_table[i, if(individual) ".975" else as.character(.5 + options[["results_CI"]] / 2)]
      )
      if (individual) {
        temp_row[["error"]] <- results_table[i, "MCMC error"]
        temp_row[["ess"]]   <- results_table[i, "ESS"]
        temp_row[["rhat"]]  <- results_table[i, "Rhat"]
      }
      
      jasp_table$addRows(temp_row)
    }
    
    
    # add footnote
    jasp_table$addFootnote(
      symbol = "\u002A",
      gettextf("The weights (%s) correspond to %s <em>p</em>-values.", "\u03C9", add_info[["weight_type"]])
    )
    
    return(jasp_table)
  }
.RoBMA_table_fill_studies   <-
  function(jasp_table,
           results_table,
           add_info,
           options,
           individual = FALSE) {
    # add columns
    jasp_table$addColumnInfo(name = "terms",
                             title = "",
                             type = "string")
    jasp_table$addColumnInfo(name = "mean",
                             title = gettext("Mean"),
                             type = "number")
    jasp_table$addColumnInfo(name = "median",
                             title = gettext("Median"),
                             type = "number")
    jasp_table$addColumnInfo(
      name = "lowerCI",
      title = gettext("Lower"),
      type = "number",
      overtitle = gettextf("%s%% CI", 100 * options[["results_CI"]])
    )
    jasp_table$addColumnInfo(
      name = "upperCI",
      title = gettext("Upper"),
      type = "number",
      overtitle = gettextf("%s%% CI", 100 * options[["results_CI"]])
    )
    if (individual) {
      jasp_table$addColumnInfo(name = "error",
                               title = gettext("MCMC error"),
                               type = "number")
      jasp_table$addColumnInfo(name = "ess",
                               title = gettext("ESS"),
                               type = "integer")
      jasp_table$addColumnInfo(name = "rhat",
                               title = gettext("Rhat"),
                               type = "number")
    }
    
    
    if (is.null(results_table))
      return(jasp_table)
    
    
    # fill rows
    temp_i <- 0
    for (i in c(1:nrow(results_table))[grepl("theta", rownames(results_table))]) {
      temp_i <- temp_i + 1
      temp_row <- list(
        terms    = add_info[["study_names"]][temp_i],
        mean     = results_table[i, "Mean"],
        median   = results_table[i, "Median"],
        lowerCI  = results_table[i, if(individual) ".025" else as.character(.5 - options[["results_CI"]] / 2)],
        upperCI  = results_table[i, if(individual) ".975" else as.character(.5 + options[["results_CI"]] / 2)]
      )
      if (individual) {
        temp_row[["error"]] <- results_table[i, "MCMC error"]
        temp_row[["ess"]]   <- results_table[i, "ESS"]
        temp_row[["rhat"]]  <- results_table[i, "Rhat"]
      }
      
      jasp_table$addRows(temp_row)
    }

    
    # add footnote
    if (add_info[["effect_size"]] == "r"){
      jasp_table$addFootnote(
        gettextf("Estimated studies' effects (%s) correspond to effect size %s.","\u03B8","\u03C1")
      )
    }else if (add_info[["effect_size"]] == "d"){
      jasp_table$addFootnote(
        gettextf("Estimated studies' effects (%s) correspond to effect size %s.","\u03B8","\u03B4")
      )
    }
    
    return(jasp_table)
  }
.RoBMA_coef_names           <- function(name, add_info) {
  if (name == "mu")
    return(gettextf("Effect size (%s)",if (add_info[["effect_size"]] == "r") "\u03C1" else if (add_info[["effect_size"]] == "d") "\u03B4" else "\u03BC"))
  if (name == "tau")
    return(gettextf("Heterogeneity (%s)","\u03C4"))
}
# main functions
.RoBMA_ready                <- function(options) {
  if (options[["measures"]] == "fitted") {
    return(options[["fitted_path"]] != "")
  }
  
  ready_arg1 <- any(c(options[["input_ES"]], options[["input_t"]]) != "")
  if (options[["measures"]] == "cohensd") {
    if (options[["cohensd_testType"]] == "one.sample") {
      ready_arg2 <-
        any(c(options[["input_SE"]], options[["input_N"]]) != "",
            sum(unlist(options[["input_CI"]]) != "") == 2)
    } else if (options[["cohensd_testType"]] == "two.sample") {
      ready_arg2 <-
        any(
          c(options[["input_SE"]], options[["input_N"]]) != "",
          sum(unlist(options[["input_CI"]]) != "") == 2,
          all(c(
            options[["input_N1"]], options[["input_N2"]]
          ) != "")
        )
    }
  } else if (options[["measures"]] == "correlation") {
    ready_arg2 <-
      any(c(options[["input_SE"]], options[["input_N"]]) != "",
          sum(unlist(options[["input_CI"]]) != "") == 2)
  } else if (options[["measures"]] == "general") {
    ready_arg2 <-
      any(options[["input_SE"]] != "", sum(unlist(options[["input_CI"]]) != "") == 2)
  }
  
  return(ready_arg1 && ready_arg2)
  
}
.RoBMA_model_notifier       <- function(jaspResults) {
  # We don't wanna delete the RoBMA modele every time settings is change since RoBMA takes a lot of time to fit.
  # Therefore, we don't create dependencies on the fitted model, but on a notifier that tells us when there was
  # a change. If possible, we don't refit the whole model, just update the neccessary parts.
  
  if (is.null(jaspResults[["model_notifier"]])) {
    model_notifier <- createJaspState()
    model_notifier$dependOn(.RoBMA_dependencies)
    jaspResults[["model_notifier"]] <- model_notifier
  }
  
  return()
  
}
.RoBMA_clean_model          <- function(jaspResults) {
  
  if(!is.null(jaspResults[["model"]])){
    jaspResults[["model"]] <- NULL
  }
  
  return()
}
.RoBMA_data_get             <- function(options, dataset) {
  if (options[["measures"]] == "fitted") {
    return(NULL)
  } else{
    if (!is.null(dataset)) {
      return(dataset)
    } else{
      var_names <-
        c(
          options[["input_t"]],
          options[["input_ES"]],
          options[["input_SE"]],
          options[["input_N"]],
          options[["input_N1"]],
          options[["input_N2"]],
          unlist(options[["input_CI"]])
        )
      var_names <- var_names[var_names != ""]
      
      dataset <-
        readDataSetToEnd(columns.as.numeric = var_names,
                         columns = if (options[["input_labels"]] != "")
                           options[["input_labels"]])
      
      # compute SE from CI
      if (sum(unlist(options[["input_CI"]]) != "") == 2) {
        dataset$JASP_computed_SE <-
          abs(dataset[, .v(options[["input_CI"]][[1]][1])] - dataset[, .v(options[["input_CI"]][[1]][2])]) / (2 * 1.96)
      }
    }
    
  }
  
  return(dataset)
}
.RoBMA_priors_get           <- function(jaspResults, options) {
  if (!is.null(jaspResults[["priors"]])) {
    return()
  } else{
    priors <- createJaspState()
    priors$dependOn(.RoBMA_dependencies)
    jaspResults[["priors"]] <- priors
  }
  
  
  object <- list()
  prior_elements <- c("priors_mu", "priors_tau", "priors_omega", "priors_mu_null", "priors_tau_null", "priors_omega_null")
  prior_names    <- c("mu", "tau", "omega", "mu_null", "tau_null", "omega_null")
  for (i in seq_along(prior_elements)) {
    tmp <- NULL
    for (elem in options[[prior_elements[i]]]) {
      tmp_prior <- tryCatch(.RoBMA_options2priors(elem), error = function(e)e)
      if(class(tmp_prior) %in% c("simpleError", "error")){
        JASP:::.quitAnalysis(tmp_prior$message)
      }
      tmp <- c(tmp, list(tmp_prior))
    }
    object[[prior_names[i]]] <- tmp
  }
  
  
  priors[["object"]] <- object
  
  return()
}
.RoBMA_priors_plots         <- function(jaspResults, options) {
  # create / access the container
  if (!is.null(jaspResults[["prior_plots"]])) {
    return()
  } else{
    prior_plots <- createJaspContainer(title = gettext("Prior Plots"))
    prior_plots$dependOn(
      c(
        "priors_mu",
        "priors_tau",
        "priors_omega",
        "null_mu",
        "null_tau",
        "null_omega",
        "priors_plot",
        "measures",
        "advanced_mu_transform"
      )
    )
    prior_plots$position <- 2
    jaspResults[["prior_plots"]] <- prior_plots
  }
  
  
  # extract the priors
  if (is.null(jaspResults[["model"]])) {
    priors  <- jaspResults[["priors"]][["object"]]
  } else{
    fit     <- jaspResults[["model"]][["object"]]
    priors  <- fit[["priors"]]
  }
  priors[["mu"]]    <- c(priors[["mu_null"]],    priors[["mu"]])
  priors[["tau"]]   <- c(priors[["tau_null"]],   priors[["tau"]])
  priors[["omega"]] <- c(priors[["omega_null"]], priors[["omega"]])
  
  
  # create plots for each of the parameters
  for (parameter in c("mu", "tau", "omega")) {
    if (length(priors[[parameter]]) == 0)
      next
    
    temp_plots <-
      createJaspContainer(
        title = if (parameter == "mu")
          gettext("Effect")
        else if (parameter == "tau")
          gettext("Heterogeneity")
        else if (parameter == "omega")
          gettext("Weight Function")
      )
    prior_plots[[parameter]] <- temp_plots
    
    for (i in 1:length(priors[[parameter]])) {
      if (parameter == "omega") {
        temp_plot <- createJaspPlot(width = 500,  height = 400)
      } else{
        temp_plot <- createJaspPlot(width = 400,  height = 300)
      }
      temp_plots[[paste0(parameter, "_", i)]] <- temp_plot
      
      
      if (is.null(jaspResults[["model"]])) {
        p <- plot(
          priors[[parameter]][[i]],
          plot_type   = "ggplot",
          par_name    = parameter,
          samples     = 1e6,
          mu_transform = if (options[["measures"]] == "correlation" &&
                             parameter == "mu")
            options[["advanced_mu_transform"]]
        )
      } else{
        p <- plot(
          priors[[parameter]][[i]],
          plot_type   = "ggplot",
          par_name    = parameter,
          samples     = 1e7,
          mu_transform = if (!is.null(fit[["add_info"]][["r"]]) &&
                             parameter == "mu")
            fit[["add_info"]][["mu_transform"]]
        )
      }
      
      p <- JASPgraphs::themeJasp(p)
      
      temp_plots[[paste0(parameter, "_", i)]][["plotObject"]] <- p
      
    }
    
  }
  
  return()
}
.RoBMA_model_preview        <- function(jaspResults, options) {
  # create / access the container
  if (!is.null(jaspResults[["prior_plots"]])) {
    return()
  } else{
    model_preview <-
      createJaspContainer(title = gettext("Model Preview"))
    model_preview$dependOn(.RoBMA_dependencies)
    model_preview$position <- 1
    jaspResults[["model_preview"]] <- model_preview
  }
  
  
  # extract the priors
  priors  <- jaspResults[["priors"]][["object"]]
  
  # set error if no priors are specified
  if ((length(priors[["mu"]]) == 0 &&
       length(priors[["mu_null"]]) == 0) ||
      (length(priors[["tau"]]) == 0 &&
       length(priors[["tau_null"]]) == 0) ||
      (length(priors[["omega"]]) == 0 &&
       length(priors[["omega_null"]]) == 0)) {
    priors_error <- createJaspTable()
    priors_error$setError(
      gettext(
        "At least one prior distribution per parameter must be specified (either null or alternative)."
      )
    )
    model_preview[["priors_error"]] <- priors_error
    return()
  }
  
  
  # create the setup table
  s.fit   <- RoBMA::check_setup(
    priors_mu         = priors[["mu"]],
    priors_tau        = priors[["tau"]],
    priors_omega      = priors[["omega"]],
    priors_mu_null    = priors[["mu_null"]],
    priors_tau_null   = priors[["tau_null"]],
    priors_omega_null = priors[["omega_null"]],
    models            = TRUE,
    silent            = TRUE
  )
  
  
  ### create overview table
  overall_summary <-
    createJaspTable(title = gettext("Model Summary"))
  overall_summary$position <- 1
  
  overall_summary$addColumnInfo(name = "terms",
                                title = "",
                                type = "string")
  overall_summary$addColumnInfo(name = "models",
                                title = gettext("Models"),
                                type = "string")
  overall_summary$addColumnInfo(name = "priorProb",
                                title = gettext("P(M)"),
                                type = "number")
  
  for (i in 1:nrow(s.fit[["overview"]])) {
    temp_row <- list(
      terms     = if (i == 1)
        gettext("Effect")
      else if (i == 2)
        gettext("Heterogeneity")
      else if (i == 3)
        gettext("Publication bias"),
      models    = paste0(s.fit[["overview"]][["Models"]][i], "/", s.fit[["add_info"]][["n_models"]]),
      priorProb = s.fit[["overview"]][["Prior prob."]][i]
    )
    
    overall_summary$addRows(temp_row)
  }
  
  model_preview[["overall_summary"]] <- overall_summary
  
  
  ### create models overview table
  models_summary <-
    createJaspTable(title = gettext("Models Overview"))
  models_summary$position <- 2
  
  models_summary$addColumnInfo(name = "number",
                               title = "#",
                               type = "integer")
  models_summary$addColumnInfo(
    name = "prior_mu",
    title = gettext("Effect Size"),
    type = "string",
    overtitle = gettext("Prior Distribution")
  )
  models_summary$addColumnInfo(
    name = "prior_tau",
    title = gettext("Heterogeneity"),
    type = "string",
    overtitle = gettext("Prior Distribution")
  )
  models_summary$addColumnInfo(
    name = "prior_omega",
    title = gettext("Publication Bias"),
    type = "string",
    overtitle = gettext("Prior Distribution")
  )
  models_summary$addColumnInfo(name = "priorProb",
                               title = gettext("P(M)"),
                               type = "number")
  
  for (i in 1:nrow(s.fit[["models"]])) {
    temp_row <- list(
      number       = as.numeric(rownames(s.fit[["models"]]))[i],
      prior_mu     = s.fit[["models"]][i, "Prior mu"],
      prior_tau    = s.fit[["models"]][i, "Prior tau"],
      prior_omega  = s.fit[["models"]][i, "Prior omega"],
      priorProb    = s.fit[["models"]][i, "Prior prob."]
    )
    
    models_summary$addRows(temp_row)
  }
  
  model_preview[["models_summary"]] <- models_summary
  
  
  return()
}
.RoBMA_fit_model            <-
  function(jaspResults, dataset, options) {

    if (is.null(jaspResults[["model"]])) {
      model <- createJaspState()
      model$dependOn("measures", "seed", "setSeed")
      jaspResults[["model"]] <- model
      
    } else{
      model <- jaspResults[["model"]]
      fit   <- model[["object"]]
      
    }
    
    if (options[["measures"]] == "fitted") {
      
      fit <- tryCatch({
        fit <- readRDS(file = options[["fitted_path"]])
        if (!RoBMA::is.RoBMA(fit))
          JASP:::.quitAnalysis(gettext("The loaded object is not a RoBMA model."))
        fit
      },error = function(e)e)
      
    } else{
      
      # check whether priors whether RoBMA was already fitted - maybe we don't need to refit everything
      if (!is.null(jaspResults[["model"]]) &&
          options[["advanced_control"]] %in% c("clever", "no_refit")) {
        fit     <- jaspResults[["model"]][["object"]]
        priors  <- jaspResults[["priors"]][["object"]]
        
        # this actually saves the priors in the same format as the RoBMA function does, what a coincidence ;)
        s.fit   <- RoBMA::check_setup(
          priors_mu         = priors[["mu"]],
          priors_tau        = priors[["tau"]],
          priors_omega      = priors[["omega"]],
          priors_mu_null    = priors[["mu_null"]],
          priors_tau_null   = priors[["tau_null"]],
          priors_omega_null = priors[["omega_null"]],
          models            = TRUE,
          silent            = TRUE
        )
        
        needs_refit <- !isTRUE(all.equal(fit[["priors"]], s.fit[["priors"]]))
      } else{
        needs_refit <- TRUE
      }
      
      if (needs_refit) {
        # extract priors
        priors <- jaspResults[["priors"]][["object"]]
        
        fit <- tryCatch(RoBMA::RoBMA(
          t  = if (options[["measures"]] == "cohensd" &&
                   options[["input_t"]] != "")
            dataset[, .v(options[["input_t"]])]
          else
            NULL,
          d  = if (options[["measures"]] == "cohensd" &&
                   options[["input_ES"]] != "")
            dataset[, .v(options[["input_ES"]])]
          else
            NULL,
          r  = if (options[["measures"]] == "correlation" &&
                   options[["input_ES"]] != "")
            dataset[, .v(options[["input_ES"]])]
          else
            NULL,
          y  = if (options[["measures"]] == "general" &&
                   options[["input_ES"]] != "")
            dataset[, .v(options[["input_ES"]])]
          else
            NULL,
          se = if (options[["input_SE"]] != "")
            dataset[, .v(options[["input_SE"]])]
          else if (sum(unlist(options[["input_CI"]]) != "") == 2)
            dataset[, "JASP_computed_SE"]
          else
            NULL,
          n  = if (options[["measures"]] %in% c("cohensd", "correlation") &&
                   options[["input_N"]] != "")
            dataset[, .v(options[["input_N"]])]
          else
            NULL,
          n1 = if (options[["measures"]] == "cohensd" &&
                   options[["input_N1"]] != "")
            dataset[, .v(options[["input_N1"]])]
          else
            NULL,
          n2 = if (options[["measures"]] == "cohensd" &&
                   options[["input_N2"]] != "")
            dataset[, .v(options[["input_N2"]])]
          else
            NULL,
          test_type    = if (options[["measures"]] == "cohensd")
            options[["cohensd_testType"]]
          else
            NULL,
          mu_transform = if (options[["measures"]] == "correlation")
            options[["advanced_mu_transform"]]
          else
            NULL,
          study_names  = if (options[["input_labels"]] != "")
            dataset[, .v(options[["input_labels"]])]
          else
            NULL,
          priors_mu         = priors[["mu"]],
          priors_tau        = priors[["tau"]],
          priors_omega      = priors[["omega"]],
          priors_mu_null    = priors[["mu_null"]],
          priors_tau_null   = priors[["tau_null"]],
          priors_omega_null = priors[["omega_null"]],
          chains  = options[["advanced_chains"]],
          iter    = options[["advanced_iteration"]],
          burnin  = options[["advanced_burnin"]],
          thin    = options[["advanced_thin"]],
          control = list(
            autofit         = options[["advanced_autofit"]],
            max_error       = if (options[["advanced_autofit"]])
              options[["advanced_autofit_error"]],
            max_time        = if (options[["advanced_autofit"]])
              paste0(
                options[["advanced_autofit_time"]],
                options[["advanced_autofit_time_unit"]]
              ),
            adapt           = options[["advanced_adapt"]],
            bridge_max_iter = options[["advanced_bridge_iter"]],
            allow_max_error = if (options[["advanced_omit"]] &&
                                  options[["advanced_omit_error"]])
              options[["advanced_omit_error_value"]],
            allow_max_rhat  = if (options[["advanced_omit"]] &&
                                  options[["advanced_omit_rhat"]])
              options[["advanced_omit_rhat_value"]],
            allow_min_ESS   = if (options[["advanced_omit"]] &&
                                  options[["advanced_omit_ESS"]])
              options[["advanced_omit_ESS_value"]],
            allow_inc_theta = options[["advanced_omit_theta"]],
            balance_prob    = options[["advanced_omit_prior"]] == "conditional",
            silent          = TRUE,
            progress_start  = 'startProgressbar(length(object$models))',
            progress_tick   = 'progressbarTick()'
          ),
          save    = "all",
          seed    = if (options[["setSeed"]])
            options[["seed"]]
        ),error = function(e)e)
        
      } else{
        
        fit <- tryCatch(RoBMA::update.RoBMA(
          object  = fit,
          study_names  = if (options[["input_labels"]] != "")
            dataset[, .v(options[["input_labels"]])]
          else
            NULL,
          chains  = options[["advanced_chains"]],
          iter    = options[["advanced_iteration"]],
          burnin  = options[["advanced_burnin"]],
          thin    = options[["advanced_thin"]],
          control = list(
            autofit         = options[["advanced_autofit"]],
            max_error       = if (options[["advanced_autofit"]])
              options[["advanced_autofit_error"]],
            max_time        = if (options[["advanced_autofit"]])
              paste0(
                options[["advanced_autofit_time"]],
                options[["advanced_autofit_time_unit"]]
              ),
            adapt           = options[["advanced_adapt"]],
            bridge_max_iter = options[["advanced_bridge_iter"]],
            allow_max_error = if (options[["advanced_omit"]] &&
                                  options[["advanced_omit_error"]])
              options[["advanced_omit_error_value"]],
            allow_max_rhat  = if (options[["advanced_omit"]] &&
                                  options[["advanced_omit_rhat"]])
              options[["advanced_omit_rhat_value"]],
            allow_min_ESS   = if (options[["advanced_omit_ESS"]])
              options[["advanced_omit_ESS_value"]],
            allow_inc_theta = options[["advanced_omit_theta"]],
            balance_prob    = options[["advanced_omit_prior"]] == "conditional",
            silent          = TRUE,
            progress_start  = 'startProgressbar(sum(converged_models))',
            progress_tick   = 'progressbarTick()'
          ),
          refit_failed = options[["advanced_control"]] != "no_refit"
        ),error = function(e)e)
        
      }
      
    }
    

    # error handling
    if(any(class(fit) %in% c("simpleError", "error"))){
        JASP:::.quitAnalysis(fit[["message"]])
    }
    
    
    # update the fit and reset notifier
    model[["object"]] <- fit
    .RoBMA_model_notifier(jaspResults)
    
    return()
  }
.RoBMA_summary              <- function(jaspResults, options) {
  if (!is.null(jaspResults[["main_summary"]])) {
    return()
  } else{
    # create container
    main_summary <- createJaspContainer(title = gettext("Summary"))
    main_summary$position <- 3
    summary_dependencies <-
      c(
        .RoBMA_dependencies,
        "bayesFactorType",
        "results_CI",
        "results_conditional",
        "results_theta"
      )
    main_summary$dependOn(summary_dependencies)
    jaspResults[["main_summary"]] <- main_summary
  }

  # remove the model preview
  jaspResults[["model_preview"]] <- NULL
  
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  # some shared info
  s.fit <- summary(
    fit,
    logBF    = options[["bayesFactorType"]] == "LogBF10",
    BF01     = options[["bayesFactorType"]] == "BF01",
    probs    = c(.5 + c(-1, 1) * options[["results_CI"]] / 2),
    conditional   = options[["results_conditional"]],
    include_theta = options[["results_theta"]]
  )
  
  ### create overview table
  overall_summary <-
    createJaspTable(title = gettext("Model Summary"))
  overall_summary$position <- 1
  
  BF_title <- paste0(
    ifelse(
      options[["bayesFactorType"]] == "BF01",
      gettext("Exclusion"),
      gettext("Inclusion")
    ),
    " ",
    ifelse(
      options[["bayesFactorType"]] == "LogBF10",
      gettext("log(BF)"),
      gettext("BF")
    )
  )
  
  overall_summary$addColumnInfo(name = "terms",
                                title = "",
                                type = "string")
  overall_summary$addColumnInfo(name = "models",
                                title = gettext("Models"),
                                type = "string")
  overall_summary$addColumnInfo(name = "priorProb",
                                title = gettext("P(M)"),
                                type = "number")
  overall_summary$addColumnInfo(name = "postProb",
                                title = gettext("P(M|data)"),
                                type = "number")
  overall_summary$addColumnInfo(name = "BF",
                                title = BF_title,
                                type = "number")
  
  if (!any(fit[["add_info"]][["converged"]])) {
    overall_summary$setError(
      gettext(
        "All models failed to converge. Please, consider inspecting the 'MCMC diagnostics' and changing the 'Advanced options'."
      )
    )
    return()
  }
  
  for (i in 1:nrow(s.fit[["overview"]])) {
    temp_row <- list(
      terms     = if (i == 1)
        gettext("Effect")
      else if (i == 2)
        gettext("Heterogeneity")
      else if (i == 3)
        gettext("Publication bias"),
      models    = paste0(
        s.fit[["overview"]][i, "Models"],
        "/",
        s.fit[["add_info"]][["n_models"]] - s.fit[["add_info"]][["failed"]]
      ),
      priorProb = s.fit[["overview"]][i, "Prior prob."],
      postProb  = s.fit[["overview"]][i, "Post. prob."],
      BF        = s.fit[["overview"]][i, 4]
    )
    
    overall_summary$addRows(temp_row)
  }
  if (s.fit[["add_info"]][["failed"]] != 0)
    overall_summary$addFootnote(symbol = gettext("Warning:"),
                                gettextf("%i models failed to converge.", s.fit[["add_info"]][["failed"]]))
  if (!is.null(fit[["add_info"]][["warnings"]])) {
    for (w in fit[["add_info"]][["warnings"]])
      overall_summary$addFootnote(symbol = gettext("Warning:"), w)
  }
  
  main_summary[["overall_summary"]] <- overall_summary
  
  
  ### create model averaged results tables
  # estimate table
  averaged_summary <-
    createJaspTable(title = gettext("Model Averaged Estimates"))
  averaged_summary$position <- 2
  averaged_summary <-
    .RoBMA_table_fill_coef(averaged_summary, s.fit[["averaged"]], s.fit[["add_info"]], options)
  main_summary[["averaged_summary"]] <- averaged_summary
  
  # weights table
  if (any(grepl("omega", rownames(s.fit[["averaged"]])))) {
    averaged_weights <-
      createJaspTable(title = gettextf("Model Averaged Weights (%s)", "\u03C9"))
    averaged_weights$position <- 3
    averaged_weights <-
      .RoBMA_table_fill_weights(averaged_weights, s.fit[["averaged"]], s.fit[["add_info"]], options)
    main_summary[["averaged_weights"]] <- averaged_weights
  }

  # estimated studies table
  if (options[["results_theta"]]) {
    studies_summary <-
      createJaspTable(title = gettextf("Model Averaged Estimated Studies' Effects (%s)", "\u03B8"))
    studies_summary$position <- 4
    studies_summary <-
      .RoBMA_table_fill_studies(studies_summary, s.fit[["averaged"]], s.fit[["add_info"]], options)
    main_summary[["studies_summary"]] <- studies_summary
  }
  
  
  ### create conditional models results tables
  if (options[["results_conditional"]]) {
    # estimate table
    conditional_summary <-
      createJaspTable(title = gettext("Conditional Estimates"))
    conditional_summary$position <- 5
    conditional_summary <-
      .RoBMA_table_fill_coef(conditional_summary,
                             s.fit[["conditional"]],
                             s.fit[["add_info"]],
                             options)
    conditional_summary$addFootnote(
      gettext(
        "Estimates are model averaged over models assuming existence of effect / heterogeneity."
      )
    )
    main_summary[["conditional_summary"]] <- conditional_summary
    
    # weights table
    if (any(grepl("omega", rownames(s.fit[["conditional"]])))) {
      conditional_weights <-
        createJaspTable(title = gettextf("Conditional Weights (%s)", "\u03C9"))
      conditional_weights$position <- 6
      conditional_weights <-
        .RoBMA_table_fill_weights(conditional_weights,
                                  s.fit[["conditional"]],
                                  s.fit[["add_info"]],
                                  options)
      conditional_weights$addFootnote(gettextf("Estimated weights (%s) are model averaged over models assuming existence of publication bias.", "\u03C9"))
      main_summary[["conditional_weights"]] <- conditional_weights
    }
    
    # add the estimated studies effects
    if (options[["results_theta"]]) {
      conditional_studies_summary <-
        createJaspTable(title = gettextf("Conditional Estimated Studies' Effects (%s)","\u03B8"))
      conditional_studies_summary$position <- 7
      conditional_studies_summary <-
        .RoBMA_table_fill_studies(conditional_studies_summary,
                                  s.fit[["conditional"]],
                                  fit[["add_info"]],
                                  options)
      conditional_studies_summary$addFootnote(gettextf("Estimated studies effects (%s) are model averaged over models assuming existence of effect.", "\u03B8"))
      main_summary[["conditional_studies_summary"]] <-
        conditional_studies_summary
    }
    
  }
  
  return()
}
.RoBMA_models_overview      <- function(jaspResults, options) {
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  # some shared info
  s.fit <- summary(
    fit,
    type     = "models",
    logBF    = options[["bayesFactorType"]] == "LogBF10",
    BF01     = options[["bayesFactorType"]] == "BF01",
    probs    = c(.5 + c(-1, 1) * options[["results_CI"]] / 2)
  )
  
  # do ordering
  if (options[["results_models_order"]] == "marglik") {
    s.fit[["overview"]] <-
      s.fit[["overview"]][order(s.fit[["overview"]][["log(MargLik)"]], decreasing = TRUE),]
  } else if (options[["results_models_order"]] == "posterior") {
    s.fit[["overview"]] <-
      s.fit[["overview"]][order(s.fit[["overview"]][["Post. prob."]], decreasing = TRUE),]
  }
  
  # compute the BF requested
  if (options[["results_models_BF"]] == "inclusion") {
    bf <- s.fit[["overview"]][, 7]
  } else if (options[["results_models_BF"]] == "best") {
    bf <-
      exp(s.fit[["overview"]][["log(MargLik)"]]) / exp(max(s.fit[["overview"]][["log(MargLik)"]]))
  } else if (options[["results_models_BF"]] == "previous") {
    temp_this <-
      exp(s.fit[["overview"]][["log(MargLik)"]])[-length(s.fit[["overview"]][["log(MargLik)"]])]
    temp_prev <- exp(s.fit[["overview"]][["log(MargLik)"]])[-1]
    bf <- c(1, temp_prev / temp_this)
  }
  
  
  
  summary_dependencies <-
    c(
      .RoBMA_dependencies,
      "bayesFactorType",
      "results_CI",
      "results_models",
      "results_models_BF",
      "results_models_order"
    )
  
  
  ### create overview table
  models_summary <-
    createJaspTable(title = gettext("Models Overview"))
  models_summary$position <- 6
  models_summary$dependOn(summary_dependencies)
  
  if (options[["results_models_BF"]] == "inclusion") {
    BF_title <- paste0(
      ifelse(
        options[["bayesFactorType"]] == "BF01",
        gettext("Exclusion"),
        gettext("Inclusion")
      ),
      " ",
      ifelse(
        options[["bayesFactorType"]] == "LogBF10",
        gettext("log(BF)"),
        gettext("BF")
      )
    )
  } else{
    if (options[["bayesFactorType"]] == "BF01") {
      BF_title <- gettext("BF01")
      bf       <- 1 / bf
    } else if (options[["bayesFactorType"]] == "LogBF10") {
      BF_title <- gettext("log(BF10)")
      bf       <- log(bf)
    } else{
      BF_title <- gettext("BF10")
    }
  }
  
  models_summary$addColumnInfo(name = "number",
                               title = "#",
                               type = "integer")
  models_summary$addColumnInfo(
    name = "prior_mu",
    title = gettext("Effect Size"),
    type = "string",
    overtitle = gettext("Prior Distribution")
  )
  models_summary$addColumnInfo(
    name = "prior_tau",
    title = gettext("Heterogeneity"),
    type = "string",
    overtitle = gettext("Prior Distribution")
  )
  models_summary$addColumnInfo(
    name = "prior_omega",
    title = gettext("Publication Bias"),
    type = "string",
    overtitle = gettext("Prior Distribution")
  )
  models_summary$addColumnInfo(name = "priorProb",
                               title = gettext("P(M)"),
                               type = "number")
  models_summary$addColumnInfo(name = "postProb",
                               title = gettext("P(M|data)"),
                               type = "number")
  models_summary$addColumnInfo(name = "marglik",
                               title = gettext("log(MargLik)"),
                               type = "number")
  models_summary$addColumnInfo(name = "BF",
                               title = BF_title,
                               type = "number")
  
  for (i in 1:nrow(s.fit[["overview"]])) {
    temp_row <- list(
      number       = as.numeric(rownames(s.fit[["overview"]]))[i],
      prior_mu     = s.fit[["overview"]][i, "Prior mu"],
      prior_tau    = s.fit[["overview"]][i, "Prior tau"],
      prior_omega  = s.fit[["overview"]][i, "Prior omega"],
      priorProb    = s.fit[["overview"]][i, "Prior prob."],
      postProb     = s.fit[["overview"]][i, "Post. prob."],
      marglik      = s.fit[["overview"]][i, "log(MargLik)"],
      BF           = bf[i]
    )
    
    models_summary$addRows(temp_row)
  }
  
  jaspResults[["main_summary"]][["models_summary"]] <-
    models_summary
  
  return()
}
.RoBMA_models_summary       <- function(jaspResults, options) {
  if (!is.null(jaspResults[["individual_models"]])) {
    return()
  } else{
    individual_models <-
      createJaspContainer(title = gettext("Individual Models Summary"))
    individual_models$position <- 5
    summary_dependencies <-
      c(
        .RoBMA_dependencies,
        "bayesFactorType",
        "results_individual",
        "results_theta",
        "results_individual_single" ,
        "results_individual_single_number"
      )
    individual_models$dependOn(summary_dependencies)
    jaspResults[["individual_models"]] <- individual_models
  }
  
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  # some shared info
  s.fit <- summary(
    fit,
    type     = "individual",
    logBF    = options[["bayesFactorType"]] == "LogBF10",
    BF01     = options[["bayesFactorType"]] == "BF01",
    include_theta = options[["results_theta"]]
  )
  
  BF_title <- paste0(
    ifelse(
      options[["bayesFactorType"]] == "BF01",
      gettext("Exclusion"),
      gettext("Inclusion")
    ),
    " ",
    ifelse(
      options[["bayesFactorType"]] == "LogBF10",
      gettext("log(BF)"),
      gettext("BF")
    )
  )
  
  ### create tables for individual models
  
  # select models to iterate over
  if (options[["results_individual_single"]]) {
    models_i <- options[["results_individual_single_number"]]
    if (models_i < 1 || models_i > length(fit[["models"]])) {
      temp_model  <-
        createJaspContainer(title = gettextf("Model %i", models_i))
      temp_error  <- createJaspTable(title = "")
      temp_error$setError(gettextf("Model %i does not exist. Select one of the models between 1 and %i.", models_i, length(fit[["models"]])))
      temp_model[["temp_error"]]                      <- temp_error
      individual_models[[paste0("model_", models_i)]] <- temp_model
      return()
    }
  } else{
    models_i <- 1:length(s.fit[["overview"]])
  }
  
  
  # do the iteration
  for (i in models_i) {
    temp_model <- createJaspContainer(title = gettextf("Model %i", i))
    individual_models[[paste0("model_", i)]] <- temp_model
    
    ### model priors
    temp_priors <- createJaspTable(title = gettext("Priors"))
    temp_priors$addColumnInfo(name = "prior_mu",
                              title = gettext("Effect Size"),
                              type = "string")
    temp_priors$addColumnInfo(
      name = "prior_tau",
      title = gettext("Heterogeneity"),
      type = "string"
    )
    temp_priors$addColumnInfo(
      name = "prior_omega",
      title = gettext("Publication Bias"),
      type = "string"
    )
    
    temp_row <- list(
      prior_mu     = print(s.fit[["overview"]][[i]][["priors"]][["mu"]], silent = TRUE),
      prior_tau    = print(s.fit[["overview"]][[i]][["priors"]][["tau"]], silent = TRUE),
      prior_omega  = print(s.fit[["overview"]][[i]][["priors"]][["omega"]], silent = TRUE)
    )
    temp_priors$addRows(temp_row)
    
    temp_model[["temp_priors"]] <- temp_priors
    
    
    ### model information
    temp_info <- createJaspTable(title = gettext("Information"))
    
    temp_info$addColumnInfo(name = "priorProb",
                            title = gettext("P(M)"),
                            type = "number")
    temp_info$addColumnInfo(name = "postProb",
                            title = gettext("P(M|data)"),
                            type = "number")
    temp_info$addColumnInfo(name = "marglik",
                            title = gettext("log(MargLik)"),
                            type = "number")
    temp_info$addColumnInfo(name = "BF",
                            title = BF_title,
                            type = "number")
    
    temp_row <- list(
      priorProb    = s.fit[["overview"]][[i]][["prior_prob"]],
      postProb     = s.fit[["overview"]][[i]][["posterior_prob"]],
      marglik      = s.fit[["overview"]][[i]][["marg_lik"]],
      BF           = s.fit[["overview"]][[i]][["BF"]]
    )
    temp_info$addRows(temp_row)
    
    temp_model[["temp_info"]] <- temp_info
    
    
    ### model coeficients
    # estimate table
    temp_coef <- createJaspTable(title = gettext("Model Estimates"))
    temp_coef <-
      .RoBMA_table_fill_coef(temp_coef,
                             s.fit[["overview"]][[i]][["tab"]],
                             s.fit[["overview"]][[i]][["add_info"]],
                             options,
                             individual = TRUE)
    temp_model[["temp_coef"]] <- temp_coef
    
    ### weights and Studies's effects
    if (!is.null(s.fit[["overview"]][[i]][["tab"]])) {
      # weights table
      if (any(grepl("omega", rownames(s.fit[["overview"]][[i]][["tab"]])))) {
        temp_weights <-
          createJaspTable(title = gettextf("Estimated Weights (%s)", "\u03C9"))
        temp_weights <-
          .RoBMA_table_fill_weights(
            temp_weights,
            s.fit[["overview"]][[i]][["tab"]],
            s.fit[["overview"]][[i]][["add_info"]],
            options,
            individual = TRUE
          )
        temp_model[["temp_weights"]] <- temp_weights
      }
      
      # estimated studies table
      if (any(grepl("theta", rownames(s.fit[["overview"]][[i]][["tab"]])))) {
        temp_studies <-
          createJaspTable(title = gettextf("Estimated Studies' Effects (%s)", "\u03B8"))
        temp_studies <-
          .RoBMA_table_fill_studies(
            temp_studies,
            s.fit[["overview"]][[i]][["tab"]],
            s.fit[["overview"]][[i]][["add_info"]],
            options,
            individual = TRUE
          )
        temp_model[["temp_studies"]] <- temp_studies
        
      }
    }
    
  }
  
  return()
}
.RoBMA_plots                <-
  function(jaspResults, options, parameters) {
    # create / access the container
    if (is.null(jaspResults[["plots"]])) {
      plots <- createJaspContainer(title = gettext("Plots"))
      plots$position <- 6
      jaspResults[["plots"]] <- plots
    } else{
      plots <- jaspResults[["plots"]]
    }
    
    if (!is.null(plots[[paste(parameters, collapse = "")]])) {
      return()
    }
    
    # extract the model
    fit    <- jaspResults[["model"]][["object"]]
    temp_s <- summary(fit)
    
    # get overall settings
    dependencies <- c(
      .RoBMA_dependencies,
      "plots_type",
      "plots_priors",
      if (any(parameters %in% "mu"))
        "plots_mu",
      if (any(parameters %in% "tau"))
        "plots_tau",
      if (any(parameters %in% "omega"))
        "plots_omega",
      "plots_omega_function",
      if (any(parameters %in% "theta"))
        c("plots_theta", "plots_theta_show", "plots_theta_order")
    )
    
    if (all(parameters %in% "mu")) {
      title    <- gettext("Effect size")
      position <- 2
    } else if (all(parameters %in% "tau")) {
      title    <- gettext("Heterogeneity")
      position <- 3
    } else if (all(parameters %in% "omega")) {
      title    <-
        if (options[["plots_omega_function"]])
          gettext("Weight function")
      else
        gettext("Weights")
      position <- 5
    } else if (all(parameters %in% "theta")) {
      title    <- gettext("Forest plot")
      position <- 1
    } else if (all(parameters %in% c("mu", "tau"))) {
      title    <- gettext("Effect size vs Heterogeneity")
      position <- 4
    }
    title <-
      gettextf("%s (%s)", title, ifelse(options[["plots_type"]] == "conditional", gettext("Conditional"), gettext("Model Averaged")))
    
    if (all(parameters %in% "theta")) {
      height <-
        250 + ncol(fit[["RoBMA"]][["samples"]][["averaged"]][["theta"]]) * if (options[["plots_theta_show"]] == "both")
          55
      else
        20
      width  <-
        350 + 9 * if (!is.null(fit[["add_info"]][["study_names"]]))
          max(nchar(fit[["add_info"]][["study_names"]]))
      else
        10
      if (options[["plots_theta_show"]] == "both") {
        pars <- c("forest", "theta")
      } else if (options[["plots_theta_show"]] == "observed") {
        pars <- "forest"
      } else if (options[["plots_theta_show"]] == "estimated") {
        pars <- "theta"
      }
    } else{
      height <- 370
      width  <- 500
      pars   <- parameters
    }
    
    
    # plot
    p <- tryCatch(
      plot(
        fit,
        parameter = pars,
        type      = options[["plots_type"]],
        plot_type = "ggplot",
        prior     = options[["plots_priors"]],
        weights   = if (parameters == "omega") {
          if (options[["plots_omega_function"]])
            FALSE
          else
            TRUE
        } else
          FALSE,
        order     = if (parameters == "theta") {
          if (options[["plots_theta_order"]] == "labels")
            NULL
          else
            options[["plots_theta_order"]]
        }
      ),
      error = function(e)
        e
    )
    
    if (any(class(p) %in% "error")) {
      temp_plot <- createJaspPlot(title       = title,
                                  width       = width,
                                  height      = height)
      temp_plot$position <- position
      temp_plot$dependOn(dependencies)
      temp_plot$setError(p[["message"]])
      plots[[paste(parameters, collapse = "")]] <- temp_plot
      return()
    }
    
    if (ggplot2::is.ggplot(p)) {
      temp_plot <- createJaspPlot(title       = title,
                                  width       = width,
                                  height      = height)
      temp_plot$position <- position
      temp_plot$dependOn(dependencies)
      plots[[paste(parameters, collapse = "")]] <- temp_plot
      
      if (all(parameters %in% "theta")) {
        p <- JASPgraphs::themeJasp(p, sides =  "b")
      } else{
        if (!is.null(ggplot2::ggplot_build(p)[["layout"]][["panel_params"]][[1]][["y.sec.labels"]])) {
          p <- JASPgraphs::themeJasp(p, sides =  "blr")
        } else{
          p <- JASPgraphs::themeJasp(p, sides =  "bl")
        }
      }
      
      plots[[paste(parameters, collapse = "")]][["plotObject"]] <- p
      
    } else{
      temp_plots <- createJaspContainer(title = title)
      temp_plots$position <- position
      temp_plots$dependOn(dependencies)
      plots[[paste(parameters, collapse = "")]] <- temp_plots
      
      for (i in 1:length(p)) {
        temp_plot <- createJaspPlot(width       = width,
                                    height      = height)
        temp_plots[[paste(parameters, "_", i, collapse = "")]] <-
          temp_plot
        
        if (all(parameters %in% "theta")) {
          p[[i]] <- JASPgraphs::themeJasp(p[[i]], sides =  "b")
        } else{
          if (!is.null(ggplot2::ggplot_build(p[[i]])[["layout"]][["panel_params"]][[1]][["y.sec.labels"]])) {
            p[[i]] <- JASPgraphs::themeJasp(p[[i]], sides =  "blr")
          } else{
            p[[i]] <- JASPgraphs::themeJasp(p[[i]], sides =  "bl")
          }
        }
        
        temp_plots[[paste(parameters, "_", i, collapse = "")]][["plotObject"]] <-
          p[[i]]
        
      }
    }
    
    
    return()
  }
.RoBMA_individual_plots     <-
  function(jaspResults, options, parameters) {
    # create / access the container
    if (is.null(jaspResults[["plots_individual"]])) {
      plots_individual <-
        createJaspContainer(title = gettext("Individual Models Plots"))
      plots_individual$position <- 7
      jaspResults[["plots_individual"]] <- plots_individual
    } else{
      plots_individual <- jaspResults[["plots_individual"]]
    }
    
    if (!is.null(plots_individual[[paste(parameters, collapse = "")]])) {
      return()
    }
    
    # extract the model
    fit    <- jaspResults[["model"]][["object"]]
    temp_s <- summary(fit)
    
    # get overall settings
    dependencies <- c(
      .RoBMA_dependencies,
      "plots_type_individual_conditional",
      "plots_type_individual_order",
      "plots_type_individual_by",
      if (any(parameters %in% "mu"))
        "plots_individual_mu",
      if (any(parameters %in% "tau"))
        "plots_individual_tau",
      if (any(parameters %in% "omega"))
        "plots_individual_omega"
    )
    
    if (all(parameters == "mu")) {
      title    <- gettext("Effect size")
      position <- 1
    } else if (parameters == "tau") {
      title    <- gettext("Heterogeneity")
      position <- 2
    } else if (parameters == "omega") {
      title    <- gettext("Weights")
      position <- 3
    }
    title <-
      gettextf("%s (%s)", title, ifelse(options[["plots_type_individual_conditional"]], gettext("Conditional Models"), gettext("Models")))
    
    height <-
      250 + 70 * if (options[["plots_type_individual_conditional"]]) {
        temp_s[["overview"]][["Models"]][if (parameters == "mu")
          1
          else if (parameters == "tau")
            2
          else if (parameters == "omega")
            3]
      } else
        temp_s[["add_info"]][["n_models"]]
    width  <- 750
    pars   <- parameters
    
    
    # plot
    if (pars == "omega" &&
        sum(grepl(pars, rownames(temp_s[["averaged"]]))) > 2) {
      temp_plots <- createJaspContainer(title = title)
      temp_plots$position <- position
      temp_plots$dependOn(dependencies)
      plots_individual[[paste(parameters, collapse = "")]] <-
        temp_plots
      
      # nota that this creates a list of ggplot objects
      p <- tryCatch(
        plot(
          fit,
          parameter = pars,
          type      = c("individual", if (options[["plots_type_individual_conditional"]])
            "conditional"),
          plot_type = "ggplot",
          order     = c(
            options[["plots_type_individual_order"]],
            options[["plots_type_individual_by"]]
          )
        ),
        error = function(e)
          e
      )
      
      if (any(class(p) %in% "error")) {
        temp_plot <- createJaspPlot(title       = title,
                                    width       = width,
                                    height      = height)
        temp_plot$position <- position
        temp_plot$dependOn(dependencies)
        temp_plot$setError(p[["message"]])
        plots_individual[[paste(parameters, collapse = "")]] <-
          temp_plot
        return()
      }
      
      for (i in 1:length(p)) {
        temp_plot <- createJaspPlot(title       = "",
                                    width       = width,
                                    height      = height)
        temp_plots[[paste0("plot_", i)]] <- temp_plot
        
        p[[i]] <- JASPgraphs::themeJasp(p[[i]], sides = "b")
        
        temp_plots[[paste0("plot_", i)]][["plotObject"]] <- p[[i]]
      }
      
    } else{
      temp_plot <- createJaspPlot(title       = title,
                                  width       = width,
                                  height      = height)
      temp_plot$position <- position
      temp_plot$dependOn(dependencies)
      plots_individual[[paste(parameters, collapse = "")]] <-
        temp_plot
      
      p <- tryCatch(
        plot(
          fit,
          parameter = pars,
          type      = c("individual", if (options[["plots_type_individual_conditional"]])
            "conditional"),
          plot_type = "ggplot",
          order     = c(
            options[["plots_type_individual_order"]],
            options[["plots_type_individual_by"]]
          )
        ),
        error = function(e)
          e
      )
      
      if (any(class(p) %in% "error")) {
        temp_plot$setError(p[["message"]])
        return()
      }
      
      p <- JASPgraphs::themeJasp(p, sides = "b")
      
      plots_individual[[paste(parameters, collapse = "")]][["plotObject"]] <-
        p
    }
    
    return()
  }
.RoBMA_diagnostics_overview <- function(jaspResults, options) {
  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 8
    diagnostics$dependOn(.RoBMA_dependencies)
    jaspResults[["diagnostics"]] <- diagnostics
  } else{
    diagnostics <- jaspResults[["diagnostics"]]
  }
  
  if (!is.null(diagnostics[["models_diagnostics"]])) {
    return()
  }
  
  
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  
  # some shared info
  s.fit <- summary(
    fit,
    type          = "models",
    diagnostics   = TRUE,
    include_theta = options[["diagnostics_overview_theta"]]
  )
  
  # do ordering
  diagnostics_dependencies <-
    c(.RoBMA_dependencies,
      "diagnostics_overview",
      "diagnostics_overview_theta")
  
  
  ### create overview table
  models_diagnostics <-
    createJaspTable(title = gettext("Models Diagnostics Overview"))
  models_diagnostics$position <- 1
  models_diagnostics$dependOn(diagnostics_dependencies)
  
  
  models_diagnostics$addColumnInfo(name = "number",
                                   title = "#",
                                   type = "integer")
  models_diagnostics$addColumnInfo(
    name = "prior_mu",
    title = gettext("Effect Size"),
    type = "string",
    overtitle = gettext("Prior Distribution")
  )
  models_diagnostics$addColumnInfo(
    name = "prior_tau",
    title = gettext("Heterogeneity"),
    type = "string",
    overtitle = gettext("Prior Distribution")
  )
  models_diagnostics$addColumnInfo(
    name = "prior_omega",
    title = gettext("Publication Bias"),
    type = "string",
    overtitle = gettext("Prior Distribution")
  )
  models_diagnostics$addColumnInfo(name = "error",
                                   title = gettext("max(MCMC error)"),
                                   type = "number")
  models_diagnostics$addColumnInfo(name = "ESS",
                                   title = gettext("min(ESS)"),
                                   type = "integer")
  models_diagnostics$addColumnInfo(name = "Rhat",
                                   title = gettext("max(Rhat)"),
                                   type = "number")
  
  for (i in 1:nrow(s.fit[["diagnostics"]])) {
    temp_row <- list(
      number       = as.numeric(rownames(s.fit[["diagnostics"]]))[i],
      prior_mu     = s.fit[["diagnostics"]][i, "Prior mu"],
      prior_tau    = s.fit[["diagnostics"]][i, "Prior tau"],
      prior_omega  = s.fit[["diagnostics"]][i, "Prior omega"],
      error        = s.fit[["diagnostics"]][i, "max(MCMC error)"],
      ESS          = s.fit[["diagnostics"]][i, "min(ESS)"],
      Rhat         = s.fit[["diagnostics"]][i, "max(Rhat)"]
    )
    
    models_diagnostics$addRows(temp_row)
  }
  
  diagnostics[["models_diagnostics"]] <- models_diagnostics
  
  return()
}
.RoBMA_diagnostics_plots    <- function(jaspResults, options) {
  # create / access the container
  if (is.null(jaspResults[["diagnostics"]])) {
    diagnostics <- createJaspContainer(title = gettext("Diagnostics"))
    diagnostics$position <- 8
    diagnostics$dependOn(.RoBMA_dependencies)
    jaspResults[["diagnostics"]] <- diagnostics
  } else{
    diagnostics <- jaspResults[["diagnostics"]]
  }
  
  
  # extract the model
  fit   <- jaspResults[["model"]][["object"]]
  
  # select models to iterate over
  if (options[["diagnostics_single"]]) {
    models_i <- options[["diagnostics_single_model"]]
    if (models_i < 1 || models_i > length(fit[["models"]])) {
      temp_model  <-
        createJaspContainer(title = gettextf("Model %i", models_i))
      diagnostics[[paste0("model_", models_i)]] <- temp_model
      temp_error  <- createJaspPlot(title = "")
      temp_error$dependOn("diagnostics_single_model", "diagnostics_single")
      temp_error$setError(gettextf("Model %i does not exist. Select one of the models between 1 and %i.", models_i, length(fit[["models"]])))
      temp_model[["temp_error"]] <- temp_error
      return()
    }
  } else{
    models_i <- 1:length(fit[["models"]])
  }
  
  # collect the parameters
  parameters <- c(if (options[["diagnostics_mu"]])
    "mu",
    if (options[["diagnostics_tau"]])
      "tau",
    if (options[["diagnostics_omega"]])
      "omega",
    if (options[["diagnostics_theta"]])
      "theta")
  
  # do the iteration
  for (i in models_i) {
    # create / access container for individual models
    if (is.null(diagnostics[[paste0("model_", i)]])) {
      temp_model <-
        createJaspContainer(title = gettextf("Model %i", i))
      temp_model$position <- i
      temp_model$dependOn(c("diagnostics_single_model", "diagnostics_single"))
      diagnostics[[paste0("model_", i)]] <- temp_model
    } else{
      temp_model <- diagnostics[[paste0("model_", i)]]
    }
    
    no_pars <-
      TRUE # tracker for checking whether any parameter was plotted
    
    for (par in parameters) {
      # create / access container for individual parameters
      if (is.null(temp_model[[par]])) {
        temp_par <-
          createJaspContainer(
            title = if (par == "mu")
              gettext("Effect")
            else if (par == "tau")
              gettext("Heterogeneity")
            else if (par == "omega")
              gettext("Weights")
            else if (par == "theta")
              gettext("Random effects")
          )
        temp_par$position <-
          if (par == "mu")
            1
        else if (par == "tau")
          2
        else if (par == "omega")
          3
        else if (par == "theta")
          4
        temp_par$dependOn(
          c(
            if (par == "mu")
              c("diagnostics_mu", "diagnostics_transformed"),
            if (par == "tau")
              "diagnostics_tau",
            if (par == "omega")
              "diagnostics_omega",
            if (par == "theta")
              "diagnostics_theta"
          )
        )
        temp_model[[par]] <- temp_par
      } else{
        temp_par <- temp_model[[par]]
      }
      
      
      # add traceplots
      if (options[["diagnostics_trace"]]) {
        # create / access container for trace plots
        if (is.null(temp_par[["trace"]])) {
          temp_plots <- createJaspContainer(gettext("Trace plots"))
          temp_plots$position <- 1
          temp_plots$dependOn("diagnostics_trace")
          temp_par[["trace"]] <- temp_plots
        } else{
          temp_plots <- temp_par[["trace"]]
        }
        
        # create plots
        new_plots <- RoBMA::diagnostics(
          fit,
          parameter     = par,
          type          = "chains",
          plot_type     = "ggplot",
          show_models   = i,
          par_transform = options[["diagnostics_transformed"]],
          title         = FALSE
        )
        
        if (is.null(new_plots))
          next
        no_pars <- FALSE
        
        # add them to the container
        if (!ggplot2::is.ggplot(new_plots)) {
          for (pi in 1:length(new_plots)) {
            temp_plot  <- createJaspPlot(width = 400, aspectRatio = .7)
            temp_plots[[paste0("trace_", pi)]] <- temp_plot
            temp_plot[["plotObject"]] <-
              JASPgraphs::themeJasp(new_plots[[pi]])
          }
          
        } else{
          temp_plot  <- createJaspPlot(width = 400, aspectRatio = .7)
          temp_plots[[paste0("trace_", 1)]] <- temp_plot
          temp_plot[["plotObject"]] <- JASPgraphs::themeJasp(new_plots)
        }
        
      }
      
      
      # add autocorrelation plots
      if (options[["diagnostics_autocorrelation"]]) {
        # create / access container for trace plots
        if (is.null(temp_par[["autocor"]])) {
          temp_plots <-
            createJaspContainer(gettext("Average autocorrelations"))
          temp_plots$position <- 2
          temp_plots$dependOn("diagnostics_autocorrelation")
          temp_par[["autocor"]] <- temp_plots
        } else{
          temp_plots <- temp_par[["autocor"]]
        }
        
        # create plots
        new_plots <- RoBMA::diagnostics(
          fit,
          parameter     = par,
          type          = "autocorrelations",
          plot_type     = "ggplot",
          show_models   = i,
          par_transform = options[["diagnostics_transformed"]],
          title         = FALSE
        )
        
        if (is.null(new_plots))
          next
        no_pars <- FALSE
        
        # add them to the container
        if (!ggplot2::is.ggplot(new_plots)) {
          for (pi in 1:length(new_plots)) {
            temp_plot  <- createJaspPlot(width = 400, aspectRatio = .7)
            temp_plots[[paste0("autocor_", pi)]] <- temp_plot
            temp_plot[["plotObject"]] <-
              JASPgraphs::themeJasp(new_plots[[pi]])
          }
          
        } else{
          temp_plot  <- createJaspPlot(width = 400, aspectRatio = .7)
          temp_plots[[paste0("autocor_", 1)]] <- temp_plot
          temp_plot[["plotObject"]] <- JASPgraphs::themeJasp(new_plots)
        }
        
      }
      
      
      # add sample densities plots
      if (options[["diagnostics_samples"]]) {
        # create / access container for trace plots
        if (is.null(temp_par[["samples"]])) {
          temp_plots <-
            createJaspContainer(gettext("Posterior samples densities"))
          temp_plots$position <- 3
          temp_plots$dependOn("diagnostics_samples")
          temp_par[["samples"]] <- temp_plots
        } else{
          temp_plots <- temp_par[["samples"]]
        }
        
        # create plots
        new_plots <- RoBMA::diagnostics(
          fit,
          parameter     = par,
          type          = "densities",
          plot_type     = "ggplot",
          show_models   = i,
          par_transform = options[["diagnostics_transformed"]],
          title         = FALSE
        )
        
        if (is.null(new_plots))
          next
        no_pars <- FALSE
        
        # add them to the container
        if (!ggplot2::is.ggplot(new_plots)) {
          for (pi in 1:length(new_plots)) {
            temp_plot  <- createJaspPlot(width = 400, aspectRatio = .7)
            temp_plots[[paste0("samples_", pi)]] <- temp_plot
            temp_plot[["plotObject"]] <-
              JASPgraphs::themeJasp(new_plots[[pi]])
          }
          
        } else{
          temp_plot  <- createJaspPlot(width = 400, aspectRatio = .7)
          temp_plots[[paste0("samples_", 1)]] <- temp_plot
          temp_plot[["plotObject"]] <- JASPgraphs::themeJasp(new_plots)
        }
        
      }
      
    }
    
    # show error if only one model is selected but doesn't contain any of the diagnostics
    if (no_pars && options[["diagnostics_single_model"]]) {
      temp_error  <- createJaspPlot(title = "")
      temp_error$dependOn(
        c(
          "diagnostics_mu",
          "diagnostics_transformed",
          "diagnostics_tau",
          "diagnostics_omega",
          "diagnostics_theta",
          "diagnostics_trace",
          "diagnostics_autocorrelation",
          "diagnostics_samples"
        )
      )
      temp_error$setError(gettextf("Model %i does not contain any of the selected parameters.", i))
      temp_model[["temp_error"]] <- temp_error
    }
  }
  
  return()
}
.RoBMA_save_model           <- function(jaspResults, options) {
  if (is.null(jaspResults[["model_saved"]])) {
    model_saved <- createJaspState()
    model_saved$dependOn(c(.RoBMA_dependencies, "save_path"))
    jaspResults[["model_saved"]] <- model_saved
    
  }
  
  saveRDS(jaspResults[["model"]][["object"]], file = options[["save_path"]])
  
  model_saved[["object"]] <- TRUE
  
}
