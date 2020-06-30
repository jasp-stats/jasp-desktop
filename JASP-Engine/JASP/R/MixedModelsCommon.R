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


# TODO: Expose priors specification to users in Bxxx?
# TODO: Add 3rd level random effects grouping factors ;) (not that difficult actually)

.mmRunAnalysis   <- function(jaspResults, dataset, options, type){
  
  if (.mmReady(options, type))
    dataset <- .mmReadData(dataset, options, type)
  if (.mmReady(options, type))
    .mmCheckData(dataset, options, type)

 
  # fit the model
  if (.mmReady(options, type)){
    if(type %in% c("LMM", "GLMM")).mmFitModel(jaspResults, dataset, options, type)
    if(type %in% c("BLMM", "BGLMM")).mmFitModelB(jaspResults, dataset, options, type)
  }
  
  
  # create (default) summary tables
  if(type %in% c("LMM", "GLMM")).mmSummaryAnova(jaspResults, dataset, options, type)
  if(type %in% c("BLMM", "BGLMM")).mmSummaryStanova(jaspResults, dataset, options, type)

  
  if (!is.null(jaspResults[["mmModel"]]) &&
      !jaspResults[[ifelse(type %in% c("LMM", "GLMM"), "ANOVAsummary", "STANOVAsummary")]]$getError()) {
    
    
    # show fit statistics
    if (options$fitStats) {
      if(type %in% c("LMM", "GLMM")).mmFitStats(jaspResults, options, type)
      if(type %in% c("BLMM", "BGLMM")).mmFitStatsB(jaspResults, options, type)
    }
    
    
    # show fixed / random effects summary
    if (options$showFE){
      if(type %in% c("LMM", "GLMM")).mmSummaryFE(jaspResults, options, type)
      if(type %in% c("BLMM", "BGLMM")).mmSummaryFEB(jaspResults, options, type)
    }
    if (options$showRE){
      if(type %in% c("LMM", "GLMM")).mmSummaryRE(jaspResults, options, type)
      if(type %in% c("BLMM", "BGLMM")).mmSummaryREB(jaspResults, options, type)
    }

 
    # sampling diagnostics
    if(type %in% c("BLMM", "BGLMM")){
      if (length(options$samplingVariable1) != 0)
        .mmDiagnostics(jaspResults, options, dataset, type)
    }
    
    
    # create plots
    if (length(options$plotsX))
      .mmPlot(jaspResults, dataset, options, type)
    
    
    # marginal means
    if (length(options$marginalMeans) > 0)
      .mmMarginalMeans(jaspResults, dataset, options, type)
    if (length(options$marginalMeans) > 0 &&
        options$marginalMeansContrast &&
        !is.null(jaspResults[["EMMresults"]]))
      .mmContrasts(jaspResults, options, type, what = "Means")
    
    
    # trends
    if (length(options$trendsTrend) > 0 &&
        length(options$trendsVariables) > 0)
      .mmTrends(jaspResults, dataset, options, type)
    if (options$trendsContrast &&
        length(options$trendsTrend) > 0 &&
        length(options$trendsVariables) > 0 &&
        !is.null(jaspResults[["EMTresults"]]))
      .mmContrasts(jaspResults, options, type, what = "Trends")
  }
  
  return()
}

### common mixed-models functions
.mmReadData      <- function(dataset, options, type = "LMM") {
  if (!is.null(dataset)) {
    return(dataset)
  } else{
    if (type %in% c("LMM","BLMM")) {
      return(readDataSetToEnd(
        columns.as.numeric = options$dependentVariable,
        columns = c(
          options$fixedVariables,
          options$randomVariables
        )
      ))
    } else if (type %in% c("GLMM","BGLMM")) {
      if (options$family == "binomial_agg"){
        return(readDataSetToEnd(
          columns.as.numeric = c(options$dependentVariable, options$dependentVariableAggregation),
          columns = c(
            options$fixedVariables,
            options$randomVariables
          )
        ))
      } else  if (options$dependentVariableAggregation == "") {
        return(readDataSetToEnd(
          columns.as.numeric = options$dependentVariable,
          columns = c(
            options$fixedVariables,
            options$randomVariables
          )
        ))
      }  
    }
  }
}
.mmCheckData     <- function(dataset, options, type = "LMM") {
  
  .hasErrors(
    dataset,
    type = 'infinity',
    exitAnalysisIfErrors = TRUE
  )
  
  # the aggregation variable for binomial can have zero variance and be without factor levels
  .hasErrors(
    dataset[,.v(c(options$dependentVariable, options$fixedVariables, options$randomVariables))],
    type = c('variance', 'factorLevels'),
    factorLevels.amount  = "< 2",
    exitAnalysisIfErrors = TRUE,
    custom = .mmCustomChecks
  )

  for(var in unlist(options$fixedEffects)) {
    if(is.factor(dataset[,.v(var)]) || is.character(dataset[,.v(var)])){
      if(length(unique(dataset[,.v(var)])) == nrow(dataset))
        JASP:::.quitAnalysis(gettextf("The categorical fixed effect '%s' must have fewer levels than the overall number of observations.",var))
    }
  }

  for(var in unlist(options$randomVariables)) {
    if(length(unique(dataset[,.v(var)])) == nrow(dataset))
      JASP:::.quitAnalysis(gettextf("The random effects grouping factor '%s' must have fewer levels than the overall number of observations.",var))  
  }  
  
  # check hack-able options
  if (type %in% c("BLMM", "BGLMM")) {
    if (options$iteration - 1 <= options$warmup) {
      JASP:::.quitAnalysis(gettext("The number of iterations must be at least 2 iterations higher than the burnin"))
    }
  }
  
  # check families
  if (type %in% c("GLMM","BGLMM")) {
    family_text <- .mmMessageGLMMtype(options$family, options$link)
    family_text <- substr(family_text, 1, nchar(family_text) - 1)
    
    if (options$family %in% c("Gamma", "inverse.gaussian")) {
      if (any(dataset[, .v(options$dependentVariable)] <= 0))
        JASP:::.quitAnalysis(gettextf("%s requres that the dependent variable is positive.",family_text))
    } else if (options$family %in% c("neg_binomial_2", "poisson")) {
      if (any(dataset[, .v(options$dependentVariable)] < 0 | any(!.is.wholenumber(dataset[, .v(options$dependentVariable)]))))
        JASP:::.quitAnalysis(gettextf("%s requres that the dependent variable is an integer.",family_text))
    } else if (options$family == "binomial") {
      if (any(!dataset[, .v(options$dependentVariable)] %in% c(0, 1)))
        JASP:::.quitAnalysis(gettextf("%s requres that the dependent variable contains only 0 and 1.",family_text))
    } else if (options$family == "binomial_agg") {
      if (any(dataset[, .v(options$dependentVariable)] < 0 | dataset[, .v(options$dependentVariable)] > 1))
        JASP:::.quitAnalysis(gettextf("%s requres that the dependent variable is higher than 0 and lower than 1.",family_text))
      if (any(dataset[, .v(options$dependentVariableAggregation)] < 0) || any(!.is.wholenumber(dataset[, .v(options$dependentVariableAggregation)])))
        JASP:::.quitAnalysis(gettextf("%s requres that the number of trials variable is an integer.",family_text))
      if (any(!.is.wholenumber(dataset[, .v(options$dependentVariable)] * dataset[, .v(options$dependentVariableAggregation)])))
        JASP:::.quitAnalysis(gettextf("%s requres that the dependent variable is proportion of successes out of the number of trials.",family_text))
    } else if (options$family == "betar") {
      if (any(dataset[, .v(options$dependentVariable)] < 0 | dataset[, .v(options$dependentVariable)] > 1))
        JASP:::.quitAnalysis(gettextf("%s requres that the dependent variable is higher than 0 and lower than 1.",family_text))
    }
  }
}
.mmReady         <- function(options, type = "LMM") {
  if (type %in% c("LMM","BLMM")) {
    if (options$dependentVariable       == "" ||
        length(options$randomVariables) == 0  ||
        length(options$fixedEffects)    == 0) {
      return(FALSE)
    }
  } else if (type %in% c("GLMM","BGLMM")) {
    if (options$family == "binomial_agg"){
      if (options$dependentVariable            == "" ||
          options$dependentVariableAggregation == "" ||
          length(options$randomVariables)      == 0  ||
          length(options$fixedEffects)         == 0) {
        return(FALSE)
      }
    }else{
      if (options$dependentVariable       == "" ||
          length(options$randomVariables) == 0  ||
          length(options$fixedEffects)    == 0) {
        return(FALSE)
      }
    }

  }
  return(TRUE)
}
.mmModelFormula  <- function(options, dataset) {
  # fixed effects
  fe_terms  <-
    sapply(options[["fixedEffects"]], function(x)
      paste(.v(unlist(x)), collapse = "*"))
  # simplify the terms
  fe_terms  <- .mmSimplifyTerms(fe_terms)
  # create the FE formula
  fixed_effects <- paste0(fe_terms, collapse = "+")
  
  if (fixed_effects == "")
    fixed_effects <- 1
  
  # random effects
  random_effects <- NULL
  removed_me     <- list()
  removed_te     <- list()
  added_re       <- list()
  for (temp_re in options[["randomEffects"]]) {
    # unlist selected random effects
    temp_vars <- sapply(temp_re$randomComponents, function(x) {
      if (x$randomSlopes) {
        return(.v(unlist(x$value)))
      } else{
        return(NA)
      }
    })
    temp_vars_rem <- sapply(temp_re$randomComponents, function(x) {
      if (x$randomSlopes) {
        return(NA)
      } else{
        return(.v(unlist(x$value)))
      }
    })
    temp_vars     <- temp_vars[!is.na(temp_vars)]
    temp_vars     <-
      sapply(temp_vars, function(x)
        paste(unlist(x), collapse = "*"))
    temp_vars_rem <- temp_vars_rem[!is.na(temp_vars_rem)]
    temp_vars_rem <-
      sapply(temp_vars_rem, function(x)
        paste(unlist(x), collapse = "*"))
    ### test sensibility of random slopes
    # main effect check #1
    # - remove main effects that have only one level of selected variable for the random effect grouping factor (eg only between subject variables)
    # - and associated interactions
    me_to_remove <- NULL
    for (me in temp_vars[!grepl("\\*", temp_vars)]) {
      temp_table <- table(dataset[, c(.v(temp_re$value), me)])
      if (all(apply(temp_table, 1, function(x)
        sum(x > 0)) <= 1)) {
        me_to_remove <- c(me_to_remove, me)
      }
    }
    if (!is.null(me_to_remove)) {
      temp_vars <-
        temp_vars[!temp_vars %in% unique(as.vector(sapply(me_to_remove, function(x)
          temp_vars[grepl(x, temp_vars, fixed = TRUE)])))]
    }
    temp_vars <- na.omit(temp_vars)
    # terms check #2
    # - remove terms that have at maximum one measure across the level of variables (targeted at interactions of between subject variables)
    te_to_remove <- NULL
    for (te in temp_vars) {
      temp_terms <- unlist(strsplit(te, "\\*"))
      if (any(sapply(temp_terms, function(x)
        typeof(dataset[, .v(x)]) == "double")))
        next
      temp_table <-
        table(dataset[, c(.v(temp_re$value), temp_terms)])
      if (all(temp_table <= 1)) {
        te_to_remove <- c(te_to_remove, te)
      }
    }
    if (!is.null(te_to_remove)) {
      te_to_remove <-
        unique(as.vector(sapply(te_to_remove, function(x)
          temp_vars[grepl(x, temp_vars, fixed = TRUE)])))
      temp_vars    <- temp_vars[!temp_vars %in% te_to_remove]
    }
    
    # simplify the formula
    re_added <- .mmAddedRETerms(temp_vars, temp_vars_rem)
    re_terms <- .mmSimplifyTerms(temp_vars)
    re_terms <- paste0(re_terms, collapse = "+")
    
    new_re <-
      paste0(
        "(",
        ifelse(re_terms == "", 1, re_terms),
        ifelse(temp_re$correlation ||
                 re_terms == "", "|", "||"),
        .v(temp_re$value),
        ")"
      )
    
    random_effects <- c(random_effects, new_re)
    removed_me[[temp_re$value]] <- .unv(me_to_remove)
    removed_te[[temp_re$value]] <- .unv(te_to_remove)
    added_re[[temp_re$value]]   <- re_added
  }
  random_effects <- paste0(random_effects, collapse = "+")
  
  model_formula <-
    paste0(.v(options$dependentVariable),
           "~",
           fixed_effects,
           "+",
           random_effects)
  
  return(
    list(
      model_formula = model_formula,
      removed_me    = removed_me,
      removed_te    = removed_te,
      added_re      = added_re
    )
  )
}
.mmSimplifyTerms <- function(terms) {
  if (length(terms) > 1) {
    split_terms  <- sapply(terms, strsplit, "\\*")
    split_terms  <-
      sapply(split_terms, function(x)
        trimws(x, which = c("both")))
    
    terms_to_remove <- rep(NA, length(split_terms))
    for (i in 1:length(terms)) {
      terms_to_remove[i] <-
        any(sapply(split_terms[-i], function(x)
          all(split_terms[[i]] %in% x)))
    }
    terms <- terms[!terms_to_remove]
  }
  return(terms)
}
.mmAddedRETerms  <- function(terms, removed) {
  added <- NULL
  if (length(terms) > 1 && length(removed) >= 1) {
    split_terms  <- sapply(terms, strsplit, "\\*")
    split_terms  <-
      sapply(split_terms, function(x)
        trimws(x, which = c("both")))
    
    split_removed <- sapply(removed, strsplit, "\\*")
    split_removed <-
      sapply(split_removed, function(x)
        trimws(x, which = c("both")))
    
    terms_to_remove <- rep(NA, length(split_terms))
    for (i in 1:length(removed)) {
      if (any(sapply(split_terms, function(x)
        all(split_removed[[i]] %in% x)))) {
        added <- c(added, paste0(.unv(split_removed[[i]]), collapse = "*"))
      }
    }
  }
  return(added)
}
.mmFitModel      <- function(jaspResults, dataset, options, type = "LMM") {
  if (!is.null(jaspResults[["mmModel"]]))
    return()

  mmModel <- createJaspState()
  #maybe you should define some columns here
  jaspResults[["mmModel"]] <- mmModel

  if (options$method == "PB") {
    seed_dependencies <- c("seed", "setSeed")
    JASP:::.setSeedJASP(options)
  } else{
    seed_dependencies <- NULL
  }
  if (type == "LMM") {
    dependencies <- c(.mmDependenciesLMM, seed_dependencies)
  } else if (type == "GLMM") {
    dependencies <- c(.mmDependenciesGLMM, seed_dependencies)
  }
  mmModel$dependOn(dependencies)


  model_formula <- .mmModelFormula(options, dataset)

  if (type == "LMM") {
    model <- tryCatch(
      afex::mixed(
        formula         = as.formula(model_formula$model_formula),
        data            = dataset,
        type            = options$type,
        method          = options$method,
        test_intercept  = if (options$method %in% c("LRT", "PB"))
          options$test_intercept
        else
          FALSE,
        args_test       = list(nsim = options$bootstrap_samples),
        check_contrasts = TRUE
      ),
      error = function(e)
        return(e)
    )
  } else if (type == "GLMM") {
    # needs to be avaluated in the global environment
    glmm_family <<- options$family
    glmm_link   <<- options$link

    # I wish there was a better way to do this
    if (options$family == "binomial_agg") {
      glmm_weight <<- dataset[, .v(options$dependentVariableAggregation)]
      model <- tryCatch(
        afex::mixed(
          formula         = as.formula(model_formula$model_formula),
          data            = dataset,
          type            = options$type,
          method          = options$method,
          test_intercept  = if (options$method %in% c("LRT", "PB"))
            options$test_intercept
          else
            FALSE,
          args_test       = list(nsim = options$bootstrap_samples),
          check_contrasts = TRUE,
          family          = eval(call("binomial", glmm_link)),
          weights         = glmm_weight
        ),
        error = function(e)
          return(e)
      )
    } else{
      model <- tryCatch(
        afex::mixed(
          formula         = as.formula(model_formula$model_formula),
          data            = dataset,
          type            = options$type,
          method          = options$method,
          test_intercept  = if (options$method %in% c("LRT", "PB"))
            options$test_intercept
          else
            FALSE,
          args_test       = list(nsim = options$bootstrap_samples),
          check_contrasts = TRUE,
          #start           = start,
          family          = eval(call(glmm_family, glmm_link))
        ),
        error = function(e)
          return(e)
      )
    }
  }


  object <- list(
    model             = model,
    removed_me        = model_formula$removed_me,
    removed_te        = model_formula$removed_te,
    added_re          = model_formula$added_re
  )

  mmModel$object <- object

  return()
}
.mmSummaryAnova  <- function(jaspResults, dataset, options, type = "LMM") {
    if (!is.null(jaspResults[["ANOVAsummary"]]))
      return()
    
    model <- jaspResults[["mmModel"]]$object$model
    
    ANOVAsummary <- createJaspTable(title = gettext("ANOVA Summary"))
    #defining columns first to give the user something nice to look at
                                            ANOVAsummary$addColumnInfo(name = "effect",     title = gettext("Effect"),             type = "string")
    if (options$method %in% c("S", "KR")) {
                                            ANOVAsummary$addColumnInfo(name = "df",         title = gettext("df"),                 type = "string")
                                            ANOVAsummary$addColumnInfo(name = "stat",       title = gettext("F"),                  type = "number")
    } else if
     (options$method %in% c("PB", "LRT")) {
                                            ANOVAsummary$addColumnInfo(name = "df",         title = gettext("df"),                 type = "integer")
                                            ANOVAsummary$addColumnInfo(name = "stat",       title = gettext("ChiSq"),              type = "number")
    }
                                            ANOVAsummary$addColumnInfo(name = "pval",       title = gettext("p"),                  type = "pvalue")
    if (options$method == "PB")             ANOVAsummary$addColumnInfo(name = "pvalBoot",   title = gettext("p (bootstrap)"),      type = "pvalue")
    if (options$pvalVS) {
                                            ANOVAsummary$addColumnInfo(name = "pvalVS",     title = gettext("VS-MPR"),             type = "number")
      if (options$method == "PB")           ANOVAsummary$addColumnInfo(name = "pvalBootVS", title = gettext("VS-MPR (bootstrap)"), type = "number")

      ANOVAsummary$addFootnote(.mmMessageVovkSellke, symbol = "\u002A", colNames = c("pvalVS", "pvalBootVS"))
    }

    jaspResults[["ANOVAsummary"]] <- ANOVAsummary
    
    ANOVAsummary$position <- 1
    if (type == "LMM") {
      dependencies <- .mmDependenciesLMM
    } else if (type == "GLMM") {
      dependencies <- .mmDependenciesGLMM
    }
    if (options$method == "PB") {
      seed_dependencies <- c("seed", "setSeed")
    } else{
      seed_dependencies <- NULL
    }
    ANOVAsummary$dependOn(c(dependencies, seed_dependencies, "pvalVS"))
    
    # some error managment for GLMMS - and oh boy, they can fail really easily
    if (type %in% c("LMM", "GLMM") && !is.null(model)) {
      if (any(attr(model, "class") %in% c("std::runtime_error", "C++Error", "error"))) {
        if (model$message == "(maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate")
          ANOVAsummary$setError(
            gettext("The optimizer failed to find a solution. Probably due to quasi-separation in the data. Try removing some of the predictors.")
          )

        else if (model$message == "cannot find valid starting values: please specify some")
          # currently no solution to this, it seems to be a problem with synthetic data only.
          # I will try silving it once someone actually has problem with real data.
          ANOVAsummary$setError(gettext("The optimizer failed to find a solution due to invalid starting values. (JASP currently does not support specifying different starting values.)"))

        else if (model$message == "Downdated VtV is not positive definite")
          ANOVAsummary$setError(gettext("The optimizer failed to find a solution. Probably due to scaling issues quasi-separation in the data. Try rescaling or removing some of the predictors."))

        else
          ANOVAsummary$setError(.unv(model$message))

        
        return()
      }
      
    }


    if (is.null(model)) {
      if (options$dependentVariable != "" &&
          length(options$fixedVariables) > 0 &&
          length(options$randomVariables) == 0) {
        ANOVAsummary$addFootnote(.mmMessageMissingRE)
      }
      if (type == "GLMM") {
        if (options$family == "binomial_agg" &&
            options$dependentVariableAggregation == "") {
          ANOVAsummary$addFootnote(.mmMessageMissingAgg)
        }
      }
      return()
    }
    
    
    for (i in 1:nrow(model$anova_table)) {
      if (rownames(model$anova_table)[i] == "(Intercept)") {
        effect_name <- gettext("Intercept")
      } else{
        effect_name <-
          paste(.unv(unlist(strsplit(
            rownames(model$anova_table)[i], ":"
          ))), collapse = " * ")
      }
      
      temp_row <- list(effect = effect_name,
                       df     = afex::nice(model)$df[i])
      
      if (options$method %in% c("S", "KR")) {
        temp_row$stat   = model$anova_table$`F`[i]
        temp_row$pval   = model$anova_table$`Pr(>F)`[i]
      } else if (options$method == "PB") {
        temp_row$stat     = model$anova_table$Chisq[i]
        temp_row$pval     = model$anova_table$`Pr(>Chisq)`[i]
        temp_row$pvalBoot = model$anova_table$`Pr(>PB)`[i]
      } else if (options$method == "LRT") {
        temp_row$stat     = model$anova_table$Chisq[i]
        temp_row$pval     = model$anova_table$`Pr(>Chisq)`[i]
      }
      if (options$pvalVS) {
        temp_row$pvalVS <- JASP:::.VovkSellkeMPR(temp_row$pval)
        if (options$method == "PB") {
          temp_row$pvalBootVS <-
            JASP:::.VovkSellkeMPR(temp_row$pvalBoot)
        }
      }
      
      ANOVAsummary$addRows(temp_row)
    }
    
    # add message about (lack of) random effect grouping factors 
    ANOVAsummary$addFootnote(.mmMessageREgrouping(options$randomVariables))
    
    # add warning messages
    # deal with type II multiple models stuff
    if (is.list(model$full_model)) {
      if (lme4::isSingular(model$full_model[[length(model$full_model)]])) {
        ANOVAsummary$addFootnote(.mmMessageSingularFit, symbol = gettext("Warning:"))
      } else if (!is.null(model$full_model[[length(model$full_model)]]@optinfo$conv$lme4$messages)) {
        ANOVAsummary$addFootnote(.mmMessageNumericalProblems, symbol = gettext("Warning:"))
      }
      if (nrow(dataset) - nrow(model$full_model[[length(model$full_model)]]@frame) != 0) {
        ANOVAsummary$addFootnote(.mmMessageMissingRows(nrow(dataset) - nrow(model$full_model[[length(model$full_model)]]@frame)))
      }
    } else{
      if (lme4::isSingular(model$full_model)) {
        ANOVAsummary$addFootnote(.mmMessageSingularFit, symbol = gettext("Warning:"))
      } else if (!is.null(model$full_model@optinfo$conv$lme4$messages)) {
        ANOVAsummary$addFootnote(.mmMessageNumericalProblems, symbol = gettext("Warning:"))
      }
      if (nrow(dataset) - nrow(model$full_model@frame) != 0) {
        ANOVAsummary$addFootnote(.mmMessageMissingRows(nrow(dataset) - nrow(model$full_model@frame)))
      }
    }
    
    
    removed_me <- jaspResults[["mmModel"]]$object$removed_me
    removed_te <- jaspResults[["mmModel"]]$object$removed_te
    added_re   <- jaspResults[["mmModel"]]$object$added_re
    if (length(removed_me) > 0) {
      for (i in 1:length(removed_me)) {
        ANOVAsummary$addFootnote(.mmMessageOmmitedTerms1(removed_me[[i]], names(removed_me)[i]), symbol = gettext("Warning:"))
      }
    }
    if (length(removed_te) > 0) {
      for (i in 1:length(removed_te)) {
        ANOVAsummary$addFootnote(.mmMessageOmmitedTerms2(removed_te[[i]], names(removed_te)[i]), symbol = gettext("Warning:"))
      }
    }

    if (length(added_re) > 0)
      for (i in 1:length(added_re))
        ANOVAsummary$addFootnote(.mmMessageAddedTerms(added_re[[i]], names(added_re)[i]), symbol = gettext("Warning:"))


    
    ANOVAsummary$addFootnote(.mmMessageANOVAtype(ifelse(options$type == 3, gettext("III"), gettext("II"))))
    if (type == "GLMM")
      ANOVAsummary$addFootnote(.mmMessageGLMMtype(options$family, options$link))

    ANOVAsummary$addFootnote(.mmMessageTermTest(options$method))
    
    
    return()
  }
.mmFitStats      <- function(jaspResults, options, type = "LMM") {
  if (!is.null(jaspResults[["fitStats"]]))
    return()
  
  model <- jaspResults[["mmModel"]]$object$model
  
  fitStats <- createJaspTable(title = gettext("Model summary"))
  fitStats$position <- 2

  if (type == "LMM") {
    dependencies <- .mmDependenciesLMM
  } else if (type == "GLMM") {
    dependencies <- .mmDependenciesGLMM
  }
  if (options$method == "PB") {
    dependencies <- c(dependencies, "seed", "setSeed")
  }


  fitStats$dependOn(c(dependencies, "fitStats"))

  
  if (is.list(model$full_model)) {
    is_REML <-
      lme4::isREML(model$full_model[[length(model$full_model)]])
  } else{
    is_REML <- lme4::isREML(model$full_model)
  }
  
  fitStats$addColumnInfo(name = "deviance",
                         title = gettext("Deviance"),
                         type = "number")
  if (is_REML) {
    fitStats$addColumnInfo(
      name = "devianceREML",
      title = gettext("Deviance (REML)"),
      type = "number"
    )
  }

  fitStats$addColumnInfo(name = "loglik", title = gettext("log Lik."), type = "number")
  fitStats$addColumnInfo(name = "df",     title = gettext("df"),       type = "integer")
  fitStats$addColumnInfo(name = "aic",    title = gettext("AIC"),      type = "number")
  fitStats$addColumnInfo(name = "bic",    title = gettext("BIC"),      type = "number")
  
  jaspResults[["fitStats"]] <- fitStats
  
  
  if (is.list(model$full_model)) {

    temp_row <- list(
      deviance = deviance(model$full_model[[length(model$full_model)]], REML = FALSE),
      loglik   = logLik(model$full_model[[length(model$full_model)]]),
      df       = attr(logLik(model$full_model[[length(model$full_model)]]) , "df"),
      aic      = AIC(model$full_model[[length(model$full_model)]]),
      bic      = BIC(model$full_model[[length(model$full_model)]])
    )
  
    if (is_REML)
      temp_row$devianceREML <- lme4::REMLcrit(model$full_model[[length(model$full_model)]])
    
  }else{

    temp_row <- list(
      deviance = deviance(model$full_model, REML = FALSE),
      loglik   = logLik(model$full_model),
      df       = attr(logLik(model$full_model) , "df"),
      aic      = AIC(model$full_model),
      bic      = BIC(model$full_model)
    )
    
    if (is_REML)
      temp_row$devianceREML <- lme4::REMLcrit(model$full_model)
    
  }
  
  fitStats$addRows(temp_row)
  fitStats$addFootnote(.mmMessageFitType(is_REML))
  
  return()
}
.mmSummaryRE     <- function(jaspResults, options, type = "LMM") {
  if (!is.null(jaspResults[["REsummary"]]))
    return()
  
  model <- jaspResults[["mmModel"]]$object$model
  
  REsummary <-
    createJaspContainer(title = gettext("Variance/Correlation Estimates"))
  
  REsummary$position <- 4
  
  if (type == "LMM") {
    dependencies <- .mmDependenciesLMM
  } else if (type == "GLMM") {
    dependencies <- .mmDependenciesGLMM
  }
  if (options$method == "PB") {
    seed_dependencies <- c("seed", "setSeed")
  } else{
    seed_dependencies <- NULL
  }
  REsummary$dependOn(c(dependencies, seed_dependencies, "showRE"))
  
  # deal with SS type II stuff
  if (is.list(model$full_model)) {
    VarCorr <-
      lme4::VarCorr(model$full_model[[length(model$full_model)]])
  } else{
    VarCorr <- lme4::VarCorr(model$full_model)
  }
  # go over each random effect grouping factor
  for (gi in 1:length(VarCorr)) {
    temp_VarCorr <- VarCorr[[gi]]
    
    # add variance summary
    REvar <-
      createJaspTable(title = gettextf("%s: Variance Estimates",.unv(names(VarCorr)[gi])))
    
    REvar$addColumnInfo(name = "variable",
                        title = gettext("Term"),
                        type = "string")
    REvar$addColumnInfo(name = "std",
                        title = gettext("Std. Deviation"),
                        type = "number")
    REvar$addColumnInfo(name = "var",
                        title = gettext("Variance"),
                        type = "number")
    
    temp_StdDev <- attr(temp_VarCorr, "stddev")
    for (i in 1:length(temp_StdDev)) {
      if (names(temp_StdDev)[i] == "(Intercept)") {
        var_name <- gettext("Intercept")
      } else{
        var_name <-
          paste(.unv(unlist(strsplit(
            names(temp_StdDev)[i], ":"
          ))), collapse = ":")
        var_name <-
          .mmVariableNames(var_name, options$fixedVariables)
      }
      
      temp_row <- list(
        variable = var_name,
        std      = temp_StdDev[i],
        var      = temp_StdDev[i]^2
      )
      
      REvar$addRows(temp_row)
    }
    
    REvar$addFootnote(.mmMessageInterpretability)
    
    REsummary[[paste0("VE", gi)]] <- REvar
    
    
    # add correlation summary
    if (length(temp_StdDev) > 1) {
      temp_Corr <- attr(temp_VarCorr, "correlation")
      REcor <-
        createJaspTable(title = gettextf("%s: Correlation Estimates",.unv(names(VarCorr)[gi])))
      
      # add columns
      REcor$addColumnInfo(name = "variable",
                          title = gettext("Term"),
                          type = "string")
      for (i in 1:nrow(temp_Corr)) {
        if (rownames(temp_Corr)[i] == "(Intercept)") {
          var_name <- gettext("Intercept")
        } else{
          var_name <-
            paste(.unv(unlist(strsplit(
              rownames(temp_Corr)[i], ":"
            ))), collapse = ":")
          var_name <-
            .mmVariableNames(var_name, options$fixedVariables)
        }
        REcor$addColumnInfo(name = paste0("v", i),
                            title = var_name,
                            type = "number")
      }
      
      # fill rows
      for (i in 1:nrow(temp_Corr)) {
        if (rownames(temp_Corr)[i] == "(Intercept)") {
          var_name <- gettext("Intercept")
        } else{
          var_name <-
            paste(.unv(unlist(strsplit(
              rownames(temp_Corr)[i], ":"
            ))), collapse = ":")
          var_name <-
            .mmVariableNames(var_name, options$fixedVariables)
        }
        
        temp_row <- list(variable = var_name)
        for (j in 1:i) {
          temp_row[paste0("v", j)] <- temp_Corr[i, j]
        }
        REcor$addRows(temp_row)
      }
      
      REcor$addFootnote(.mmMessageInterpretability)
      
      REsummary[[paste0("CE", gi)]] <- REcor
      
    }
    
  }
  
  # add residual variance summary
  REres <-
    createJaspTable(title = gettext("Residual Variance Estimates"))
  
  REres$addColumnInfo(name = "std",
                      title = gettext("Std. Deviation"),
                      type = "number")
  REres$addColumnInfo(name = "var",
                      title = gettext("Variance"),
                      type = "number")
  
  if (is.list(model$full_model)) {
    temp_row <-
      list(std      = sigma(model$full_model[[length(model$full_model)]]),
           var      = sqrt(sigma(model$full_model[[length(model$full_model)]])))
  } else{
    temp_row <- list(std      = sigma(model$full_model),
                     var      = sigma(model$full_model)^2)
  }
  
  REres$addRows(temp_row)
  REsummary[[paste0("RES", gi)]] <- REres
  
  
  jaspResults[["REsummary"]] <- REsummary
  return()
}
.mmSummaryFE     <- function(jaspResults, options, type = "LMM") {
  if (!is.null(jaspResults[["FEsummary"]]))
    return()
  
  model <- jaspResults[["mmModel"]]$object$model
  
  if (is.list(model$full_model)) {
    FE_coef <-
      summary(model$full_model[[length(model$full_model)]])$coeff
  } else{
    FE_coef <- summary(model$full_model)$coeff
  }
  
  FEsummary <- createJaspTable(title = gettext("Fixed Effects Estimates"))
  
  FEsummary$position <- 3
  if (type == "LMM")       dependencies <- .mmDependenciesLMM
  else if (type == "GLMM") dependencies <- .mmDependenciesGLMM

  seed_dependencies <- ifelse(options$method == "PB", yes=c("seed", "setSeed"), no=NULL)

  FEsummary$dependOn(c(dependencies, seed_dependencies, "showFE", "pvalVS"))
  
                          FEsummary$addColumnInfo(name = "term",       title = gettext("Term"),       type = "string")
                          FEsummary$addColumnInfo(name = "estimate",   title = gettext("Estimate"),   type = "number")
                          FEsummary$addColumnInfo(name = "se",         title = gettext("SE"),         type = "number")
  if (type == "LMM")      FEsummary$addColumnInfo(name = "df",         title = gettext("df"),         type = "number")
                          FEsummary$addColumnInfo(name = "stat",       title = gettext("t"),          type = "number")
  if (ncol(FE_coef) >= 4) FEsummary$addColumnInfo(name = "pval",       title = gettext("p"),          type = "pvalue")

  if (options$pvalVS) {
                          FEsummary$addColumnInfo(name = "pvalVS",     title = gettext("VS-MPR"),     type = "number")
    FEsummary$addFootnote(.mmMessageVovkSellke, symbol = "\u002A", colNames = "pvalVS")
  }
  
  jaspResults[["FEsummary"]] <- FEsummary

  for (i in 1:nrow(FE_coef)) {
    if (rownames(FE_coef)[i] == "(Intercept)") {
      effect_name <- gettext("Intercept")
    } else{
      effect_name <-
        paste(.unv(unlist(strsplit(
          rownames(FE_coef)[i], ":"
        ))), collapse = ":")
      effect_name <-
        .mmVariableNames(effect_name, options$fixedVariables)
    }
    
    if (type == "LMM") {
      temp_row <- list(
        term     = effect_name,
        estimate = FE_coef[i, 1],
        se       = FE_coef[i, 2],
        df       = FE_coef[i, 3],
        stat     = FE_coef[i, 4],
        pval     = FE_coef[i, 5]
      )
    } else if (type == "GLMM") {
      temp_row <- list(
        term     = effect_name,
        estimate = FE_coef[i, 1],
        se       = FE_coef[i, 2],
        stat     = FE_coef[i, 3]
      )
      if (ncol(FE_coef) >= 4) {
        temp_row$pval <- FE_coef[i, 4]
      }
    }
    
    if (options$pvalVS) {
      temp_row$pvalVS <- JASP:::.VovkSellkeMPR(temp_row$pval)
    }
    
    FEsummary$addRows(temp_row)
  }
  
  # add warning messages
  FEsummary$addFootnote(.mmMessageInterpretability)
  

}
.mmPlot          <- function(jaspResults, dataset, options, type = "LMM") {
    model <- jaspResults[["mmModel"]]$object$model
    
    # automatic size specification will somewhat work unless there is more than 2 variables in panel
    height <- 350
    width  <- 450 * prod(sapply(unlist(options$plotsX), function(x) length(unique(dataset[, .v(x)])) / 2))

    if (length(options$plotsPanel) > 0) {
      width  <-
        width * length(unique(dataset[, .v(unlist(options$plotsPanel)[1])]))
    } else if (length(options$plotsPanel) > 1) {
      height <-
        height * length(unique(dataset[, .v(unlist(options$plotsPanel)[2])]))
    }
    if (options$plotLegendPosition %in% c("bottom", "top")) {
      height <- height + 50
    } else if (options$plotLegendPosition %in% c("left", "right")) {
      width  <- width + 100
    }
    
    plots  <- createJaspPlot(title = gettext("Plot"), width = width, height = height)
    
    plots$position <- 5
    switch(type,
      LMM   = dependencies <- .mmDependenciesLMM,
      GLMM  = dependencies <- .mmDependenciesGLMM,
      BLMM  = dependencies <- .mmDependenciesBLMM,
      BGLMM = dependencies <- .mmDependenciesBGLMM
    )

    plots$dependOn(
      c(
        dependencies,
        "plotsX",
        "plotsTrace",
        "plotsPanel",
        "plotsAgregatedOver",
        "plotsGeom",
        "plotsTrace",
        "plotsPanel",
        "plotsTheme",
        "plotsCIwidth",
        "plotsCImethod",
        "plotAlpha",
        "plotJitterWidth",
        "plotJitterHeight",
        "plotGeomWidth",
        "plotDodge",
        "plotsBackgroundColor",
        "plotRelativeSize",
        "plotRelativeSizeText",
        "plotLegendPosition",
        "plotsMappingColor",
        "plotsMappingShape",
        "plotsMappingLineType",
        "plotsMappingFill",
        "seed",
        "setSeed"
      )
    )

    jaspResults[["plots"]] <- plots
    plots$status <- "running"

    # stop with message if there is no random effects grouping factor selected
    if (length(options$plotsAgregatedOver) == 0) {
      plots$setError(
        gettext("At least one random effects grouping factor needs to be selected in field 'Background data show'.")
      )
      return()
    }
    if (all(
      !c(
        options$plotsMappingColor,
        options$plotsMappingShape,
        options$plotsMappingLineType,
        options$plotsMappingFill
      )
    )) {
      plots$setError(
        gettext("Factor levels need to be distinguished by at least one feature. Please, check one of the 'Distinguish factor levels' options.")
      )
      return()
    }
    
    # select geom
    if (options$plotsGeom %in% c("geom_jitter", "geom_violin", "geom_boxplot", "geom_count")) {
      geom_package <- "ggplot2"
    } else if (options$plotsGeom == "geom_beeswarm") {
      geom_package <- "ggbeeswarm"
    } else if (options$plotsGeom == "geom_boxjitter") {
      geom_package <- "ggpol"
    }
    
    # select mapping
    mapping <-
      c("color", "shape", "linetype", "fill")[c(
        options$plotsMappingColor,
        options$plotsMappingShape,
        options$plotsMappingLineType,
        options$plotsMappingFill
      )]
    if (length(mapping) == 0)
      mapping <- ""
    
    # specify data_arg
    if (options$plotsGeom == "geom_jitter") {
      data_arg <- list(
        position =
          ggplot2::position_jitterdodge(
            jitter.width  = options$plotJitterWidth,
            jitter.height = options$plotJitterHeight,
            dodge.width   = options$plotDodge
          )
      )
    } else if (options$plotsGeom == "geom_violin") {
      data_arg <- list(width = options$plotGeomWidth)
    } else if (options$plotsGeom == "geom_boxplot") {
      data_arg <- list(width = options$plotGeomWidth)
    } else if (options$plotsGeom == "geom_count") {
      data_arg <- list()
    } else if (options$plotsGeom == "geom_beeswarm") {
      data_arg <- list(dodge.width = options$plotDodge)
    } else if (options$plotsGeom == "geom_boxjitter") {
      data_arg <- list(
        width             = options$plotGeomWidth,
        jitter.width      = options$plotJitterWidth,
        jitter.height     = options$plotJitterHeight,
        outlier.intersect = TRUE
      )
    }
    if (options$plotsBackgroundColor != "none" && options$plotsGeom != "geom_jitter" && "color" %in% mapping)
      data_arg$color <- options$plotsBackgroundColor
    
    
    JASP:::.setSeedJASP(options)
    p <- tryCatch(
      afex::afex_plot(
        model,
        dv          = .v(options$dependentVariable),
        x           = .v(unlist(options$plotsX)),
        trace       = if (length(options$plotsTrace) != 0)
          .v(unlist(options$plotsTrace)),
        panel       = if (length(options$plotsPanel) != 0)
          .v(unlist(options$plotsPanel)),
        id          = .v(options$plotsAgregatedOver),
        data_geom   = getFromNamespace(options$plotsGeom, geom_package),
        mapping     = mapping,
        error       = options$plotsCImethod,
        error_level = options$plotsCIwidth,
        data_alpha  = options$plotAlpha,
        data_arg    = if (length(data_arg) != 0)
          data_arg,
        error_arg   = list(
          width = 0,
          size  = .5 * options$plotRelativeSize
        ),
        point_arg   = list(size = 1.5 * options$plotRelativeSize),
        line_arg    = list(size = .5 * options$plotRelativeSize),
        legend_title = paste(.unv(unlist(
          options$plotsTrace
        )), collapse = "\n"),
        dodge       = options$plotDodge
      ),
      error = function(e)
        e
    )
    
    if (class(p) %in% c("simpleError", "error")) {
      plots$setError(p$message)
      return()
    }
    
    # fix names of the variables
    p <-
      p + ggplot2::labs(x = unlist(options$plotsX),
                        y = options$dependentVariable)
    
    # add theme
    if (options$plotsTheme == "JASP") {
      p <-
        JASPgraphs::themeJasp(p, legend.position = options$plotLegendPosition)
    } else if (options$plotsTheme == "theme_bw") {
      p <-
        p + ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
    } else if (options$plotsTheme == "theme_light") {
      p <-
        p + ggplot2::theme_light() + ggplot2::theme(legend.position = "bottom")
    } else if (options$plotsTheme == "theme_minimal") {
      p <-
        p + ggplot2::theme_minimal() + ggplot2::theme(legend.position = "bottom")
    } else if (options$plotsTheme == "theme_apa") {
      p <-
        p + jtools::theme_apa() + ggplot2::theme(legend.position = "bottom")
    } else if (options$plotsTheme == "theme_pubr") {
      p <- p + ggpubr::theme_pubr()
    }
    
    if (options$plotsTheme != "JASP") {
      p <-
        p + ggplot2::theme(
          legend.text  = ggplot2::element_text(size = ggplot2::rel(options$plotRelativeSizeText)),
          legend.title = ggplot2::element_text(size = ggplot2::rel(options$plotRelativeSizeText)),
          axis.text    = ggplot2::element_text(size = ggplot2::rel(options$plotRelativeSizeText)),
          axis.title   = ggplot2::element_text(size = ggplot2::rel(options$plotRelativeSizeText)),
          legend.position = options$plotLegendPosition
        )
      
    }
    
    
    plots$plotObject <- p
    
    if (options$plotsEstimatesTable) {
      plot_data <- afex::afex_plot(
        model,
        x           = .v(unlist(options$plotsX)),
        dv          = .v(options$dependentVariable),
        trace       = if (length(options$plotsTrace) != 0)
          .v(unlist(options$plotsTrace)),
        panel       = if (length(options$plotsPanel) != 0)
          .v(unlist(options$plotsPanel)),
        id          = .v(options$plotsAgregatedOver),
        data_geom   = getFromNamespace(options$plotsGeom, geom_package),
        error       = options$plotsCImethod,
        error_level = options$plotsCIwidth,
        return      = "data"
      )$means
      
      
      EstimatesTable <-
        createJaspTable(title = gettext("Estimated Means and Confidence Intervals"))
      EstimatesTable$position <- 5
      EstimatesTable$dependOn(
        c(
          dependencies,
          "plotsX",
          "plotsTrace",
          "plotsPanel",
          "plotsAgregatedOver",
          "plotsCIwidth",
          "plotsCImethod",
          "seed",
          "setSeed",
          "plotsEstimatesTable"
        )
      )
      
      
      for (v in attr(plot_data, "pri.vars")) {
        EstimatesTable$addColumnInfo(name = v,
                                     title = .unv(v),
                                     type = "string")
      }
      
      for (v in options$marginalMeans) {
        
      }
      
      EstimatesTable$addColumnInfo(name = "mean",
                                   title = gettext("Mean"),
                                   type = "number")
      if (options$plotsCImethod != "none") {
        EstimatesTable$addColumnInfo(
          name = "lowerCI",
          title = gettext("Lower"),
          type = "number",
          overtitle = gettextf("%s%% CI", 100 * options$plotsCIwidth)
        )
        EstimatesTable$addColumnInfo(
          name = "upperCI",
          title = gettext("Upper"),
          type = "number",
          overtitle = gettextf("%s%% CI", 100 * options$plotsCIwidth)
        )
      }

      jaspResults[["EstimatesTable"]] <- EstimatesTable
      
      for (i in 1:nrow(plot_data)) {
        temp_row <- list()
        for (v in attr(plot_data, "pri.vars")) {
          temp_row[v] <- as.character(plot_data[i, v])
        }
        
        temp_row$mean     <- plot_data[i, "y"]
        if (options$plotsCImethod != "none") {
          temp_row$lowerCI  <- plot_data[i, "lower"]
          temp_row$upperCI  <- plot_data[i, "upper"]
        }
        
        EstimatesTable$addRows(temp_row)
      }
      

    }
    
    return()
  }
.mmMarginalMeans <- function(jaspResults, dataset, options, type = "LMM") {
    if (!is.null(jaspResults[["EMMresults"]]))
      return()
    
    model <- jaspResults[["mmModel"]]$object$model
    
    # deal with continuous predictors
    at <- NULL
    for (var in unlist(options$marginalMeans)) {
      if (typeof(dataset[, .v(var)]) == "double") {
        at[[.v(var)]] <-
          c(
            mean(dataset[, .v(var)], na.rm = TRUE) + c(-1, 0, 1) * options$marginalMeansSD *
              sd(dataset[, .v(var)], na.rm = TRUE)
          )
      }
    }
    
    # compute the results
    if (type == "LMM") {
      emmeans::emm_options(pbkrtest.limit = if (options$marginalMeansOverride)
        Inf,
        mmrTest.limit  = if (options$marginalMeansOverride)
          Inf)
    }
    emm <- emmeans::emmeans(
      object  = model,
      specs   = .v(unlist(options$marginalMeans)),
      at      = at,
      options = list(level  = options$marginalMeansCIwidth),
      lmer.df = if (type == "LMM")
        options$marginalMeansDf
      else if (type == "GLMM" &&
               options$family == "gaussian" &&
               options$link == "identity")
        "asymptotic",
      type    = if (type %in% c("GLMM", "BGLMM"))
        if (options$marginalMeansResponse)
          "response"
    )
    
    emm_table  <- as.data.frame(emm)
    if (type %in% c("LMM", "GLMM")) {
      if (options$marginalMeansCompare) {
        emm_test <-
          as.data.frame(emmeans::test(emm, null = options$marginalMeansCompareTo))
      }
    }
    
    EMMsummary <- createJaspTable(title = gettext("Estimated Marginal Means"))
    EMMresults <- createJaspState()
    
    EMMsummary$position <- 7
    if (type == "LMM") {
      dependencies <- .mmDependenciesLMM
    } else if (type == "GLMM") {
      dependencies <- c(.mmDependenciesGLMM, "marginalMeansResponse")
    } else if (type == "BLMM") {
      dependencies <- .mmDependenciesBLMM
    } else if (type == "BGLMM") {
      dependencies <- c(.mmDependenciesGLMM, "marginalMeansResponse")
    }
    if (type %in% c("LMM", "GLMM")) {
      dependencies_add <-
        c(
          "marginalMeans",
          "marginalMeansSD",
          "marginalMeansCompare",
          "marginalMeansCompareTo",
          "marginalMeansCIwidth",
          "pvalVS",
          "marginalMeansContrast"
        )
    } else{
      dependencies_add <-
        c(
          "marginalMeans",
          "marginalMeansSD",
          "marginalMeansCIwidth",
          "marginalMeansContrast"
        )
    }
    if (type == "LMM") {
      dependencies_add <-
        c(dependencies_add,
          "marginalMeansOverride",
          "marginalMeansDf")
    }
    EMMsummary$dependOn(c(dependencies, dependencies_add))
    EMMresults$dependOn(c(dependencies, dependencies_add))
    
    if (options$marginalMeansContrast) {
      EMMsummary$addColumnInfo(name = "number",
                               title = gettext("Row"),
                               type = "integer")
    }
    for (v in unlist(options$marginalMeans)) {
      if (typeof(dataset[, .v(v)]) == "double") {
        EMMsummary$addColumnInfo(name = .v(v),
                                 title = .unv(v),
                                 type = "number")
      } else{
        EMMsummary$addColumnInfo(name = .v(v),
                                 title = .unv(v),
                                 type = "string")
      }
    }
    
    if (type %in% c("LMM", "GLMM")) {
      EMMsummary$addColumnInfo(name = "estimate",
                               title = gettext("Estimate"),
                               type = "number")
      EMMsummary$addColumnInfo(name = "se",
                               title = gettext("SE"),
                               type = "number")
      EMMsummary$addColumnInfo(name = "df",
                               title = gettext("df"),
                               type = "number")
      EMMsummary$addColumnInfo(
        name = "lowerCI",
        title = gettext("Lower"),
        type = "number",
        overtitle = gettextf("%s%% CI", 100 * options$marginalMeansCIwidth)
      )
      EMMsummary$addColumnInfo(
        name = "upperCI",
        title = gettext("Upper"),
        type = "number",
        overtitle = gettextf("%s%% CI", 100 * options$marginalMeansCIwidth)
      )
      if (options$marginalMeansCompare) {
        EMMsummary$addColumnInfo(
          name = "stat",
          title = ifelse(colnames(emm_test)[ncol(emm_test) - 1] == "t.ratio", gettext("t"), gettext("z")),
          type = "number"
        )
        EMMsummary$addColumnInfo(name = "pval",
                                 title = gettext("p"),
                                 type = "pvalue")
        EMMsummary$addFootnote(.mmMessageTestNull(options$marginalMeansCompareTo),
                               symbol = "\u2020", colNames = "pval")
        
        if (options$pvalVS) {
          EMMsummary$addColumnInfo(name = "pvalVS",
                                   title = gettext("VS-MPR"),
                                   type = "number")
          EMMsummary$addFootnote(.mmMessageVovkSellke, symbol = "\u002A", colNames = "pvalVS")
        }
      }
    } else if (type %in% c("BLMM", "BGLMM")) {
      EMMsummary$addColumnInfo(name = "estimate",
                               title = gettext("Median"),
                               type = "number")
      EMMsummary$addColumnInfo(
        name = "lowerCI",
        title = gettext("Lower"),
        type = "number",
        overtitle = gettextf("%s%% HPD", 100 * options$marginalMeansCIwidth)
      )
      EMMsummary$addColumnInfo(
        name = "upperCI",
        title = gettext("Upper"),
        type = "number",
        overtitle = gettextf("%s%% HPD", 100 * options$marginalMeansCIwidth)
      )
    }

    jaspResults[["EMMsummary"]] <- EMMsummary
    
    for (i in 1:nrow(emm_table)) {
      temp_row <- list()
      
      if (options$marginalMeansContrast) {
        temp_row$number <- i
      }
      
      for (v in unlist(options$marginalMeans)) {
        if (typeof(dataset[, .v(v)]) == "double") {
          temp_row[.v(v)] <- emm_table[i, .v(v)]
        } else{
          temp_row[.v(v)] <- as.character(emm_table[i, .v(v)])
        }
      }
      
      if (type %in% c("LMM", "GLMM")) {
        # the estimate is before SE (names change for GLMM)
        temp_row$estimate <-
          emm_table[i, grep("SE", colnames(emm_table)) - 1]
        temp_row$se       <- emm_table[i, "SE"]
        temp_row$df       <- emm_table[i, "df"]
        
        if (options$marginalMeansCompare) {
          temp_row$stat <- emm_test[i, grep("ratio", colnames(emm_test))]
          temp_row$pval <- emm_test[i, "p.value"]
          if (options$pvalVS) {
            temp_row$pvalVS <- JASP:::.VovkSellkeMPR(temp_row$pval)
          }
        }
      } else if (type %in% c("BLMM", "BGLMM")) {
        temp_row$estimate <- emm_table[i, ncol(emm_table) - 2]
      }
      
      temp_row$lowerCI  <- emm_table[i, ncol(emm_table) - 1]
      temp_row$upperCI  <- emm_table[i, ncol(emm_table)]
      
      
      EMMsummary$addRows(temp_row)
    }
    
    
    if (length(emm@misc$avgd.over) != 0) {
      EMMsummary$addFootnote(.mmMessageAveragedOver(.unv(emm@misc$avgd.over)))
    }
    # add warning message
    if (type == "LMM") {
      if (options$marginalMeansDf != attr(emm@dffun, "mesg")) {
        EMMsummary$addFootnote(.mmMessageDFdisabled, symbol = gettext("Warning:"))
      }
    }
    if (type %in% c("GLMM","BGLMM")) {
      EMMsummary$addFootnote(
        ifelse(
          options$marginalMeansResponse,
          .mmMessageResponse,
          .mmMessageNotResponse
        )
      )
    }
    

    
    
    object <- list(emm         = emm,
                   emm_table   = emm_table)
    EMMresults$object <- object
    jaspResults[["EMMresults"]] <- EMMresults
    
    return()
  }
.mmTrends        <- function(jaspResults, dataset, options, type = "LMM") {
    if (!is.null(jaspResults[["contrasts_Trends"]]))
      return()
    
    model <- jaspResults[["mmModel"]]$object$model
    
    # deal with continuous predictors
    at <- NULL
    for (var in unlist(options$trendsVariables)) {
      if (typeof(dataset[, .v(var)]) == "double") {
        at[[.v(var)]] <-
          c(
            mean(dataset[, .v(var)], na.rm = TRUE) + c(-1, 0, 1) * options$trendsSD *
              sd(dataset[, .v(var)], na.rm = TRUE)
          )
      }
    }
    
    # compute the results
    if (type %in% c("LMM")) {
      emmeans::emm_options(pbkrtest.limit = if (options$trendsOverride)
        Inf,
        mmrTest.limit  = if (options$trendsOverride)
          Inf)
    }
    
    # TODO: deal with the emtrends scoping problems
    trends_CI      <<- options$trendsCIwidth
    trends_at      <<- at
    trends_type    <<- if (type == "LMM" || (type == "GLMM" &&
                                             options$family == "gaussian" &&
                                             options$link == "identity"))
      "LMM"
    else
      type
    trends_dataset <<- dataset
    trends_model   <<- model
    trends_df      <<-
      if (type == "LMM")
        options$trendsDf
    else if (type == "GLMM" &&
             options$family == "gaussian" &&
             options$link == "identity")
      "asymptotic"
    
    emm <- emmeans::emtrends(
      object  = trends_model,
      data    = trends_dataset,
      specs   = .v(unlist(options$trendsVariables)),
      var     = .v(unlist(options$trendsTrend)),
      at      = trends_at,
      options = list(level = trends_CI),
      lmer.df = if (trends_type == "LMM")
        trends_df
    )
    emm_table  <- as.data.frame(emm)
    if (type %in% c("LMM", "GLMM")) {
      if (options$trendsCompare) {
        emm_test <-
          as.data.frame(emmeans::test(emm, null = options$trendsCompareTo))
      }
    }
    
    trendsSummary <- createJaspTable(title = gettext("Estimated Trends"))
    EMTresults <- createJaspState()
    
    trendsSummary$position <- 9
    if (type == "LMM") {
      dependencies <- .mmDependenciesLMM
    } else if (type == "GLMM") {
      dependencies <- c(.mmDependenciesGLMM)
    } else if (type == "BLMM") {
      dependencies <- .mmDependenciesBLMM
    } else if (type == "BGLMM") {
      dependencies <- c(.mmDependenciesBGLMM)
    }
    if (type %in% c("LMM", "GLMM")) {
      dependencies_add <-
        c(
          "trendsVariables",
          "trendsTrend",
          "trendsSD",
          "trendsCompare",
          "trendsCompareTo",
          "trendsCIwidth",
          "pvalVS",
          "trendsContrast"
        )
    } else{
      dependencies_add <-
        c(
          "trendsVariables",
          "trendsTrend",
          "trendsSD",
          "trendsCIwidth",
          "trendsContrast"
        )
    }
    if (type == "LMM") {
      dependencies_add <-
        c(dependencies_add, "trendsDf", "trendsOverride")
    }
    trendsSummary$dependOn(c(dependencies, dependencies_add))
    EMTresults$dependOn(c(dependencies, dependencies_add))
    
    if (options$trendsContrast) {
      trendsSummary$addColumnInfo(name = "number",
                                  title = gettext("Row"),
                                  type = "integer")
    }
    
    trends_var_names <-
      colnames(emm_table)[1:(grep(".trend", colnames(emm_table), fixed = TRUE) -
                               1)]
    for (v in trends_var_names) {
      if (typeof(dataset[, .v(v)]) == "double") {
        trendsSummary$addColumnInfo(name = v,
                                    title = .unv(v),
                                    type = "number")
      } else{
        trendsSummary$addColumnInfo(name = v,
                                    title = .unv(v),
                                    type = "string")
      }
    }
    trendsSummary$addColumnInfo(
      name = "slope",
      title = gettextf("%s (slope)",unlist(options$trendsTrend)),
      type = "number"
    )
    if (type %in% c("LMM", "GLMM")) {
      trendsSummary$addColumnInfo(name = "se",
                                  title = gettext("SE"),
                                  type = "number")
      trendsSummary$addColumnInfo(name = "df",
                                  title = gettext("df"),
                                  type = "number")
      trendsSummary$addColumnInfo(
        name = "lowerCI",
        title = gettext("Lower"),
        type = "number",
        overtitle = gettextf("%s%% CI", 100 * options$trendsCIwidth)
      )
      trendsSummary$addColumnInfo(
        name = "upperCI",
        title = gettext("Upper"),
        type = "number",
        overtitle = gettextf("%s%% CI", 100 * options$trendsCIwidth)
      )
      if (options$trendsCompare) {
        trendsSummary$addColumnInfo(
          name = "stat",
          title = ifelse(colnames(emm_test)[ncol(emm_test) - 1] == "t.ratio", gettext("t"), gettext("z")),
          type = "number"
        )
        trendsSummary$addColumnInfo(name = "pval",
                                    title = gettext("p"),
                                    type = "pvalue")
        trendsSummary$addFootnote(.mmMessageTestNull(options$trendsCompareTo), symbol = "\u2020", colNames = "pval")
        
        if (options$pvalVS) {
          trendsSummary$addColumnInfo(name = "pvalVS",
                                      title = gettext("VS-MPR"),
                                      type = "number")
          trendsSummary$addFootnote(.mmMessageVovkSellke, symbol = "\u002A", colNames = "pvalVS")
        }
      }
    } else if (type %in% c("BLMM", "BGLMM")) {
      trendsSummary$addColumnInfo(
        name = "lowerCI",
        title = gettext("Lower"),
        type = "number",
        overtitle = gettextf("%s%% HPD", 100 * options$trendsCIwidth)
      )
      trendsSummary$addColumnInfo(
        name = "upperCI",
        title = gettext("Upper"),
        type = "number",
        overtitle = gettextf("%s%% HPD", 100 * options$trendsCIwidth)
      )
    }

    jaspResults[["trendsSummary"]] <- trendsSummary
    
    
    for (i in 1:nrow(emm_table)) {
      temp_row <- list()
      
      if (options$trendsContrast) {
        temp_row$number <- i
      }
      
      for (vi in 1:length(trends_var_names)) {
        if (typeof(dataset[, .v(trends_var_names[vi])]) == "double") {
          temp_row[trends_var_names[vi]] <- emm_table[i, vi]
        } else{
          temp_row[trends_var_names[vi]] <-
            as.character(emm_table[i, vi])
        }
      }
      temp_row$slope <- emm_table[i, length(trends_var_names) + 1]
      
      if (type %in% c("LMM", "GLMM")) {
        # the estimate is before SE (names change for GLMM)
        temp_row$se       <- emm_table[i, "SE"]
        temp_row$df       <- emm_table[i, "df"]
        
        if (options$trendsCompare) {
          temp_row$stat <- emm_test[i, grep("ratio", colnames(emm_test))]
          temp_row$pval <- emm_test[i, "p.value"]
          if (options$pvalVS) {
            temp_row$pvalVS <- JASP:::.VovkSellkeMPR(temp_row$pval)
          }
        }
      }
      
      temp_row$lowerCI  <- emm_table[i, ncol(emm_table) - 1]
      temp_row$upperCI  <- emm_table[i, ncol(emm_table)]
      
      
      trendsSummary$addRows(temp_row)
    }
    
    
    if (length(emm@misc$avgd.over) != 0) {
      trendsSummary$addFootnote(.mmMessageAveragedOver(.unv(emm@misc$avgd.over)))
    }
    # add warning message
    if (type == "LMM") {
      if (options$trendsDf != attr(emm@dffun, "mesg")) {
        # TODO: for GLMM
        trendsSummary$addFootnote(.mmMessageDFdisabled, symbol = gettext("Warning:"))
      }
    }
    if (type == "GLMM") {
      trendsSummary$addFootnote(.mmMessageNotResponse)
    }


    
    
    object <- list(emm         = emm,
                   emm_table   = emm_table)
    EMTresults$object <- object

    jaspResults[["EMTresults"]]    <- EMTresults
    
    return()
  }
.mmContrasts     <- function(jaspResults, options, type = "LMM", what = "Means") {
    if (what == "Means") {
      if (!is.null(jaspResults[["contrasts_Means"]]))
        return()
      emm       <- jaspResults[["EMMresults"]]$object$emm
      emm_table <- jaspResults[["EMMresults"]]$object$emm_table
    } else if (what == "Trends") {
      if (!is.null(jaspResults[["contrasts_Trends"]]))
        return()
      emm       <- jaspResults[["EMTresults"]]$object$emm
      emm_table <- jaspResults[["EMTresults"]]$object$emm_table
    }
    
    
    EMMCsummary <- createJaspTable(title = gettext("Contrasts"))
    
    EMMCsummary$position <- ifelse(what == "Means", 8, 10)
    if (type == "LMM") {
      dependencies <- .mmDependenciesLMM
    } else if (type == "GLMM") {
      dependencies <-
        c(.mmDependenciesGLMM, if (what == "Means")
          "marginalMeansResponse")
    } else if (type == "BLMM") {
      dependencies <- .mmDependenciesBLMM
    } else if (type == "BGLMM") {
      dependencies <-
        c(.mmDependenciesBGLMM, if (what == "Means")
          "marginalMeansResponse")
    }
    if (what == "Means") {
      if (type %in% c("LMM", "GLMM")) {
        dependencies_add <-
          c(
            "marginalMeans",
            "marginalMeansDf",
            "marginalMeansSD",
            "marginalMeansCompare",
            "marginalMeansCompareTo",
            "marginalMeansContrast",
            "marginalMeansCIwidth",
            "pvalVS",
            "marginalMeansOverride",
            "Contrasts",
            "marginalMeansAdjustment"
          )
      } else{
        dependencies_add <-
          c(
            "marginalMeans",
            "marginalMeansSD",
            "marginalMeansContrast",
            "marginalMeansCIwidth",
            "Contrasts"
          )
      }
    } else if (what == "Trends") {
      if (type %in% c("LMM", "GLMM")) {
        dependencies_add <-
          c(
            "trendsVariables",
            "trendsTrend",
            "trendsDf",
            "trendsSD",
            "trendsCompare",
            "trendsCompareTo",
            "trendsContrast",
            "trendsContrasts",
            "trendsCIwidth",
            "pvalVS",
            "trendsOverride",
            "trendsAdjustment"
          )
      } else{
        dependencies_add <-
          c(
            "trendsVariables",
            "trendsTrend",
            "trendsSD",
            "trendsCIwidth",
            "trendsContrast",
            "trendsContrasts"
          )
      }
    }
    
    EMMCsummary$dependOn(c(dependencies, dependencies_add))
    
    
    if (type %in% c("LMM", "GLMM")) {
      EMMCsummary$addColumnInfo(name = "contrast",
                                title = "",
                                type = "string")
      EMMCsummary$addColumnInfo(name = "estimate",
                                title = gettext("Estimate"),
                                type = "number")
      EMMCsummary$addColumnInfo(name = "se",
                                title = gettext("SE"),
                                type = "number")
      EMMCsummary$addColumnInfo(name = "df",
                                title = gettext("df"),
                                type = "number")
      EMMCsummary$addColumnInfo(name = "stat",
                                title = gettext("z"),
                                type = "number")
      EMMCsummary$addColumnInfo(name = "pval",
                                title = gettext("p"),
                                type = "pvalue")
      if (options$pvalVS) {
        EMMCsummary$addColumnInfo(name = "pvalVS",
                                  title = gettext("VS-MPR"),
                                  type = "number")
        EMMCsummary$addFootnote(.mmMessageVovkSellke, symbol = "\u002A", colNames = "pvalVS")
      }
    } else if (type %in% c("BLMM", "BGLMM")) {
      EMMCsummary$addColumnInfo(name = "contrast",
                                title = "",
                                type = "string")
      EMMCsummary$addColumnInfo(name = "estimate",
                                title = gettext("Estimate"),
                                type = "number")
      EMMCsummary$addColumnInfo(
        name = "lowerCI",
        title = gettext("Lower"),
        type = "number",
        overtitle = gettextf(
          "%s%% HPD",
          100 * if (what == "Means")
            options$marginalMeansCIwidth
          else
            options$trendsCIwidth
        )
      )
      EMMCsummary$addColumnInfo(
        name = "upperCI",
        title = gettext("Upper"),
        type = "number",
        overtitle = gettextf(
          "%s%% HPD",
          100 * if (what == "Means")
            options$marginalMeansCIwidth
          else
            options$trendsCIwidth
        )
      )
    }

    # Columns have been specified, show to user
    jaspResults[[paste0("contrasts_", what)]] <- EMMCsummary
    
    if (what == "Means") {
      selectedContrasts  <- options$Contrasts
      selectedAdjustment <- options$marginalMeansAdjustment
      
      if (type %in% c("GLMM", "BGLMM")) {
        selectedResponse   <- options$marginalMeansResponse
      }
      
      
    } else if (what == "Trends") {
      selectedContrasts  <- options$trendsContrasts
      selectedAdjustment <- options$trendsAdjustment
    }
    
    contrs <- list()
    i      <- 0
    for (cont in selectedContrasts[sapply(selectedContrasts, function(x)
      x$isContrast)]) {
      if (all(cont$values == 0))
        next
      i <- i + 1
      contrs[[cont$name]] <-
        unname(sapply(cont$values, function(x)
          eval(parse(text = x))))
    }
    if (length(contrs) == 0) {
      return()
    }
    
    
    # take care of the scale
    if (type %in% c("LMM", "BLMM") || what == "Trends") {
      emm_contrast <- tryCatch(
        as.data.frame(
          emmeans::contrast(emm, contrs,
                            adjust = if (type %in% c("LMM", "GLMM"))
                              selectedAdjustment)
        ),
        error = function(e)
          e
      )
    } else if (type %in% c("GLMM", "BGLMM")) {
      if (selectedResponse) {
        emm_contrast <- tryCatch(
          as.data.frame(
            emmeans::contrast(
              emmeans::regrid(emm),
              contrs,
              adjust = if (type == "GLMM")
                selectedAdjustment
            )
          ),
          error = function(e)
            e
        )
      } else{
        emm_contrast <- tryCatch(
          as.data.frame(
            emmeans::contrast(emm, contrs,
                              adjust = if (type == "GLMM")
                                selectedAdjustment)
          ),
          error = function(e)
            e
        )
      }
    }
    
    if (length(emm_contrast) == 2) {
      EMMCsummary$setError(emm_contrast$message)
      return()
    }
    
    # fix the title name if there is a t-stats
    if (type %in% c("LMM", "GLMM"))
      if (colnames(emm_contrast)[5] == "t.ratio")
        EMMCsummary$setColumnTitle("stat", gettext("t"))
    if (type %in% c("GLMM", "BGLMM")) {
      if (type == "GLMM") {
        temp_est_name <- colnames(emm_contrast)[ncol(emm_contrast) - 4]
      } else if (type == "BGLMM") {
        temp_est_name <- colnames(emm_contrast)[ncol(emm_contrast) - 2]
      }
      if (temp_est_name == "odds.ratio") {
        EMMCsummary$setColumnTitle("estimate", gettext("Odds Ratio"))
      } else if (temp_est_name == "ratio") {
        EMMCsummary$setColumnTitle("estimate", gettext("Ratio"))
      } else if (temp_est_name == "estimate") {
        EMMCsummary$setColumnTitle("estimate", gettext("Estimate"))
      } else{
        EMMCsummary$setColumnTitle("estimate", temp_est_name)
      }
    }
    
    for (i in 1:nrow(emm_contrast)) {
      if (type %in% c("LMM", "GLMM")) {
        temp_row <- list(
          contrast =  names(contrs)[i],
          estimate =  emm_contrast[i, ncol(emm_contrast) - 4],
          se       =  emm_contrast[i, "SE"],
          df       =  emm_contrast[i, "df"],
          stat     =  emm_contrast[i, ncol(emm_contrast) - 1],
          pval     =  emm_contrast[i, "p.value"]
        )
        if (options$pvalVS) {
          temp_row$pvalVS <- JASP:::.VovkSellkeMPR(temp_row$pval)
        }
        
        EMMCsummary$addFootnote(.messagePvalAdjustment(selectedAdjustment), symbol = "\u2020", colNames = "pval")
        if (options$pvalVS) {
          temp_row$pvalVS <- JASP:::.VovkSellkeMPR(temp_row$pval)
        }
        
      } else if (type %in% c("BLMM", "BGLMM")) {
        temp_row <- list(
          contrast = names(contrs)[i],
          estimate = emm_contrast[i, ncol(emm_contrast) - 2],
          lowerCI  = emm_contrast[i, "lower.HPD"],
          upperCI  = emm_contrast[i, "upper.HPD"]
        )
      }
      
      
      if (type %in% c("GLMM", "BGLMM") && what == "Means") {
        if (!selectedResponse) {
          EMMCsummary$addFootnote(.mmMessageNotResponse)
        } else{
          EMMCsummary$addFootnote(.mmMessageResponse)
        }
      }
      
      
      EMMCsummary$addRows(temp_row)
      
    }
  }


# specific Bayesian
.mmReadDataB      <- function(dataset, options, type = "BLMM") {
  if (!is.null(dataset)) {
    return(dataset)
  } else{
    if (type == "LMM") {
      return(
        readDataSetToEnd(
          columns.as.numeric = options$dependentVariable,
          columns.as.factor  = c(options$fixedVariables, options$randomVariables)
        )
      )
    } else if (type == "GLMM") {
      if (options$dependentVariableAggregation == "") {
        return(readDataSetToEnd(
          columns = c(
            options$dependentVariable,
            options$fixedVariables,
            options$randomVariables
          )
        ))
      } else{
        return(readDataSetToEnd(
          columns = c(
            options$dependentVariable,
            options$fixedVariables,
            options$randomVariables,
            options$dependentVariableAggregation
          )
        ))
      }
    }
  }
}
.mmFitModelB      <- function(jaspResults, dataset, options, type = "BLMM") {
    # hopefully fixing the random errors
    contr.bayes <<- stanova::contr.bayes
    if (!is.null(jaspResults[["mmModel"]]))
      return()
    
    mmModel <- createJaspState()

    
    if (type == "BLMM") {
      dependencies <- .mmDependenciesBLMM
    } else if (type == "BGLMM") {
      dependencies <- .mmDependenciesBGLMM
    }
    mmModel$dependOn(dependencies)
    
    model_formula <- .mmModelFormula(options, dataset)
    
    JASP:::.setSeedJASP(options)
    if (type == "BLMM") {
      model <- stanova::stanova_lmer(
        formula           = as.formula(model_formula$model_formula),
        check_contrasts   = "contr.bayes",
        data              = dataset,
        chains            = options$chains,
        iter              = options$iteration,
        warmup            = options$warmup,
        adapt_delta       = options$adapt_delta,
        control           = list(max_treedepth = options$max_treedepth)
      )
      
    } else if (type == "BGLMM") {
      # needs to be evaluated in the global environment
      glmm_link      <<- options$link
      if (options$family == "neg_binomial_2") {
        glmm_family <<- rstanarm::neg_binomial_2(link = glmm_link)
      } else if (options$family == "betar") {
        glmm_family <<- mgcv::betar(link = glmm_link)
      } else if (options$family != "binomial_agg"){
        temp_family <<- options$family
        glmm_family <<- eval(call(temp_family, glmm_link))
      }
      
      # I wish there was a better way to do this
      if (options$family == "binomial_agg") {
        glmm_weight <<- dataset[, .v(options$dependentVariableAggregation)]
        
        model <- stanova::stanova_glmer(
          formula           = as.formula(model_formula$model_formula),
          check_contrasts   = "contr.bayes",
          data              = dataset,
          chains            = options$chains,
          iter              = options$iteration,
          warmup            = options$warmup,
          adapt_delta       = options$adapt_delta,
          control           = list(max_treedepth = options$max_treedepth),
          weights           = glmm_weight,
          family            = eval(call("binomial", glmm_link))
        )
        
      } else{
        model <- stanova::stanova_glmer(
          formula           = as.formula(model_formula$model_formula),
          check_contrasts   = "contr.bayes",
          data              = dataset,
          chains            = options$chains,
          iter              = options$iteration,
          warmup            = options$warmup,
          adapt_delta       = options$adapt_delta,
          control           = list(max_treedepth = options$max_treedepth),
          family            = glmm_family
        )
        
      }
      
    }
    
    
    object <- list(
      model             = model,
      removed_me        = model_formula$removed_me,
      removed_te        = model_formula$removed_te
    )
    
    mmModel$object <- object
    jaspResults[["mmModel"]] <- mmModel
    
    return()
  }
.mmFitStatsB      <- function(jaspResults, options, type = "BLMM") {
  if (!is.null(jaspResults[["fitStats"]]))
    return()
  
  model <- jaspResults[["mmModel"]]$object$model
  
  fitStats <- createJaspTable(title = gettext("Fit Statistics"))
  fitStats$position <- 4
  
  if (type == "BLMM") {
    dependencies <- .mmDependenciesBLMM
  } else if (type == "BGLMM") {
    dependencies <- .mmDependenciesBGLMM
  }
  

  fitStats$dependOn(c(dependencies, "fitStats"))
  

  fitStats$addColumnInfo(name = "waic",
                         title = gettext("WAIC"),
                         type = "number")
  fitStats$addColumnInfo(name = "waicSE",
                         title = gettext("SE (WAIC)"),
                         type = "number")
  fitStats$addColumnInfo(name = "loo",
                         title = gettext("LOO"),
                         type = "number")
  fitStats$addColumnInfo(name = "looSE",
                         title = gettext("SE (LOO)"),
                         type = "number")
  

  jaspResults[["fitStats"]] <- fitStats
  
  waic <- loo::waic(model)
  loo  <- loo::loo(model)


  n_bad_waic <- sum(waic$pointwise[,2] > 0.4)
  n_bad_loo  <- length(loo::pareto_k_ids(loo, threshold = .7))
  
  
  if(n_bad_waic > 0){
    fitStats$addFootnote(.mmMessageBadWAIC(n_bad_waic), symbol = gettext("Warning:"))   
  }
  if(n_bad_loo > 0){
    fitStats$addFootnote(.mmMessageBadLOO(n_bad_loo), symbol = gettext("Warning:"))    
  }
  
  
  temp_row <- list(
    waic   = waic$estimates["waic", "Estimate"],
    waicSE = waic$estimates["waic", "SE"],
    loo    = loo$estimates["looic", "Estimate"],
    looSE  = loo$estimates["looic", "SE"]
  )
  
  fitStats$addRows(temp_row)

  
  return()
}
.mmSummaryREB     <- function(jaspResults, options, type = "BLMM") {
  if (!is.null(jaspResults[["REsummary"]]))
    return()
  
  model <- jaspResults[["mmModel"]]$object$model
  
  REsummary <- createJaspContainer(title = gettext("Variance/Correlation Estimates"))
  
  REsummary$position <- 4
  
  if (type == "BLMM") {
    dependencies <- .mmDependenciesBLMM
  } else if (type == "BGLMM") {
    dependencies <- .mmDependenciesBGLMM
  }
  REsummary$dependOn(c(dependencies, "showRE", "summaryCI"))
  
  ### keep this if we decide to change things
  #model_summary <- rstan::summary(model$stanfit, probs = c(.5-options$summaryCI/2, .5+options$summaryCI/2))$summary
  #names_summary <- rownames(model_summary)
  #re_names  <- names_summary[grepl("Sigma[", names_summary, fixed = T)]
  #re_groups <- sapply(re_names, function(x){
  #  substr(x,7,regexpr(":", x, fixed = TRUE)[1]-1)
  #})
  #re_summary    <- model_summary[names_summary %in% re_names,]
  #s_summary     <- model_summary[names_summary == "sigma",]
  
  VarCorr <- rstanarm:::VarCorr.stanreg(model)
  # go over each random effect grouping factor
  for (gi in 1:length(VarCorr)) {
    temp_VarCorr <- VarCorr[[gi]]
    
    # add variance summary
    REvar <-
      createJaspTable(title = gettextf("%s: Variance Estimates",.unv(names(VarCorr)[gi])))
    
    REvar$addColumnInfo(name = "variable",
                        title = gettext("Term"),
                        type = "string")
    REvar$addColumnInfo(name = "std",
                        title = gettext("Std. Deviation"),
                        type = "number")
    REvar$addColumnInfo(name = "var",
                        title = gettext("Variance"),
                        type = "number")
    
    temp_StdDev <- attr(temp_VarCorr, "stddev")
    for (i in 1:length(temp_StdDev)) {
      if (names(temp_StdDev)[i] == "(Intercept)") {
        var_name <- gettext("Intercept")
      } else{
        var_name <-
          paste(.unv(unlist(strsplit(
            names(temp_StdDev)[i], ":"
          ))), collapse = ":")
        var_name <-
          .mmVariableNames(var_name, options$fixedVariables)
      }
      
      temp_row <- list(
        variable = var_name,
        std      = temp_StdDev[i],
        var      = temp_StdDev[i]^2
      )
      
      REvar$addRows(temp_row)
    }
    
    REvar$addFootnote(.mmMessageInterpretability)
    
    REsummary[[paste0("VE", gi)]] <- REvar
    
    
    # add correlation summary
    if (length(temp_StdDev) > 1) {
      temp_Corr <- attr(temp_VarCorr, "correlation")
      REcor <-
        createJaspTable(title = gettextf("%s: Correlation Estimates",.unv(names(VarCorr)[gi])))
      
      # add columns
      REcor$addColumnInfo(name = "variable",
                          title = gettext("Term"),
                          type = "string")
      for (i in 1:nrow(temp_Corr)) {
        if (rownames(temp_Corr)[i] == "(Intercept)") {
          var_name <- gettext("Intercept")
        } else{
          var_name <-
            paste(.unv(unlist(strsplit(
              rownames(temp_Corr)[i], ":"
            ))), collapse = ":")
          var_name <-
            .mmVariableNames(var_name, options$fixedVariables)
        }
        REcor$addColumnInfo(name = paste0("v", i),
                            title = var_name,
                            type = "number")
      }
      
      # fill rows
      for (i in 1:nrow(temp_Corr)) {
        if (rownames(temp_Corr)[i] == "(Intercept)") {
          var_name <- gettext("Intercept")
        } else{
          var_name <-
            paste(.unv(unlist(strsplit(
              rownames(temp_Corr)[i], ":"
            ))), collapse = ":")
          var_name <-
            .mmVariableNames(var_name, options$fixedVariables)
        }
        
        temp_row <- list(variable = var_name)
        for (j in 1:i) {
          # ncol(temp_Corr)
          temp_row[paste0("v", j)] <- temp_Corr[i, j]
        }
        REcor$addRows(temp_row)
      }
      
      REcor$addFootnote(.mmMessageInterpretability)
      
      REsummary[[paste0("CE", gi)]] <- REcor
      
    }
    
  }
  
  # add residual variance summary
  REres <-
    createJaspTable(title = gettext("Residual Variance Estimates"))
  
  REres$addColumnInfo(name = "std",   title = gettext("Std. Deviation"), type = "number")
  REres$addColumnInfo(name = "var",   title = gettext("Variance"),       type = "number")

  jaspResults[["REsummary"]] <- REsummary
  
  temp_row <- list(
    std      = rstanarm:::sigma.stanreg(model),
    var      = rstanarm:::sigma.stanreg(model)^2
  )
  
  REres$addRows(temp_row)
  REsummary[["RES"]] <- REres
    
  return()
}
.mmSummaryFEB     <- function(jaspResults, options, type = "BLMM") {
  if (!is.null(jaspResults[["FEsummary"]]))
    return()
  
  model <- jaspResults[["mmModel"]]$object$model
  
  FEsummary <- createJaspTable(title = "Fixed Effects Estimates")
  FEsummary$position <- 3
  if (type == "BLMM") {
    dependencies <- .mmDependenciesBLMM
  } else if (type == "BGLMM") {
    dependencies <- .mmDependenciesBGLMM
  }
  FEsummary$dependOn(c(dependencies, "showFE", "summaryCI"))
  
  FEsummary$addColumnInfo(name = "term",
                          title = "Term",
                          type = "string")
  FEsummary$addColumnInfo(name = "estimate",
                          title = "Estimate",
                          type = "number")
  FEsummary$addColumnInfo(name = "se",
                          title = "SE",
                          type = "number")
  FEsummary$addColumnInfo(
    name = "lowerCI",
    title = gettext("Lower"),
    type = "number",
    overtitle = gettextf("%s%% CI", 100 * options$summaryCI)
  )
  FEsummary$addColumnInfo(
    name = "upperCI",
    title = gettext("Upper"),
    type = "number",
    overtitle = gettextf("%s%% CI", 100 * options$summaryCI)
  )
  FEsummary$addColumnInfo(name = "rhat",
                          title = "R-hat",
                          type = "number")
  FEsummary$addColumnInfo(name = "neff",
                          title = "ESS",
                          type = "number")

  jaspResults[["FEsummary"]] <- FEsummary
  
  model_summary <-
    rstan::summary(model$stanfit,
                   probs = c(.5 - options$summaryCI / 2, .5 + options$summaryCI / 2))$summary
  names_summary <- rownames(model_summary)
  fe_summary    <-
    model_summary[!grepl("b[", names_summary, fixed = T) &
                    !names_summary %in% c("mean_PPD", "log-posterior") &
                    names_summary != "sigma" &
                    !grepl("Sigma[", names_summary, fixed = T), ]
  
  for (i in 1:nrow(fe_summary)) {
    if (rownames(fe_summary)[i] == "(Intercept)") {
      effect_name <- "Intercept"
    } else{
      effect_name <-
        paste(.unv(unlist(strsplit(
          rownames(fe_summary)[i], ":"
        ))), collapse = ":")
      effect_name <-
        .mmVariableNames(effect_name, options$fixedVariables)
    }
    
    temp_row <- list(
      term     = effect_name,
      estimate = fe_summary[i, 1],
      se       = fe_summary[i, 3],
      lowerCI  = fe_summary[i, 4],
      upperCI  = fe_summary[i, 5],
      rhat     = fe_summary[i, 7],
      neff     = fe_summary[i, 6]
    )
    
    FEsummary$addRows(temp_row)
  }
  
  # add warning messages
  FEsummary$addFootnote(.mmMessageInterpretability)
}
.mmSummaryStanova <- function(jaspResults, dataset, options, type = "BLMM") {
    if (!is.null(jaspResults[["STANOVAsummary"]]))
      return()

    model <- jaspResults[["mmModel"]]$object$model
    if (!is.null(model) && !class(jaspResults[["mmModel"]]$object$model) %in% c("simpleError", "error")) {
      model_summary <-
        summary(
          model,
          probs = c(.50 - options$summaryCI / 2, .50, .50 + options$summaryCI / 2),
          diff_intercept = options$show == "deviation"
        )
    } else{
      # dummy object for creating empty summary
      model_summary <-
        list("Model summary" = matrix(NA, nrow = 0, ncol = 0))
    }
    
    STANOVAsummary <- createJaspContainer(title = "")
    jaspResults[["STANOVAsummary"]] <- STANOVAsummary
    
    STANOVAsummary$position <- 1
    if (type == "BLMM") {
      dependencies <- .mmDependenciesBLMM
    } else if (type == "BGLMM") {
      dependencies <- .mmDependenciesBGLMM
    }
    STANOVAsummary$dependOn(c(dependencies, "summaryCI", "show"))
    
    # go over each random effect grouping factor
    for (i in 1:length(model_summary)) {
      temp_summary <- model_summary[[i]]
      
      if (names(model_summary)[i] == "Model summary") {
        var_name   <- gettext("Model summary")
        table_name <- var_name
      } else if (names(model_summary)[i] == "(Intercept)") {
        var_name   <- gettext("Intercept")
        table_name <- var_name
      } else{
        var_name   <-
          paste(.unv(unlist(strsplit(
            names(model_summary)[i], ":"
          ))), collapse = "*")
        if (options$show == "deviation") {
          table_name <-
            gettextf("%s (differences from intercept)",var_name)
        } else if (options$show == "mmeans") {
          if (nrow(temp_summary) == 1) {
            table_name <- gettextf("%s (trend)",var_name)
          } else{
            table_name <- gettextf("%s (marginal means)",var_name)
          }
        }
      }
      
      temp_table <- createJaspTable(title = table_name)
      STANOVAsummary[[paste0("summary_", i)]] <- temp_table
      
      if (var_name != "Intercept" && nrow(temp_summary) > 1) {
        temp_table$addColumnInfo(name = "level",
                                 title = gettext("Level"),
                                 type = "string")
      }
      temp_table$addColumnInfo(name = "estimate",
                               title = gettext("Estimate"),
                               type = "number")
      temp_table$addColumnInfo(name = "se",
                               title = gettext("SE"),
                               type = "number")
      temp_table$addColumnInfo(
        name = "lowerCI",
        title = gettext("Lower"),
        type = "number",
        overtitle = gettextf("%s%% CI", 100 * options$summaryCI)
      )
      temp_table$addColumnInfo(
        name = "upperCI",
        title = gettext("Upper"),
        type = "number",
        overtitle = gettextf("%s%% CI", 100 * options$summaryCI)
      )
      temp_table$addColumnInfo(name = "rhat",
                               title = gettext("R-hat"),
                               type = "number")
      temp_table$addColumnInfo(name = "ess_bulk",
                               title = gettext("ESS (bulk)"),
                               type = "number")
      temp_table$addColumnInfo(name = "ess_tail",
                               title = gettext("ESS (tail)"),
                               type = "number")
      
      if (table_name == "Model summary") {
        if(options$dependentVariable != "" &&
           length(options$fixedVariables) > 0 &&
           length(options$randomVariables) == 0) {
          temp_table$addFootnote(.mmMessageMissingRE)
        }
        if (type == "BGLMM") {
          if (options$family == "binomial_agg" &&
              options$dependentVariableAggregation == "") {
            temp_table$addFootnote(.mmMessageMissingAgg)
          }
        }
        
        if(class(jaspResults[["mmModel"]]$object$model) %in% c("simpleError", "error")) {
          STANOVAsummary$setError("The model could not be estimated. Please, check the options and dataset for errors.")
        }
        return()
      }
      
      for (j in 1:nrow(temp_summary)) {
        temp_row <- list(
          estimate   = temp_summary$Mean[j],
          se         = temp_summary$MAD_SD[j],
          lowerCI    = temp_summary[j, paste0((.50 - options$summaryCI / 2) *
                                                100, "%")],
          upperCI    = temp_summary[j, paste0((.50 + options$summaryCI / 2) *
                                                100, "%")],
          rhat       = temp_summary$rhat[j],
          ess_bulk   = temp_summary$ess_bulk[j],
          ess_tail   = temp_summary$ess_tail[j]
        )
        
        if (var_name != "Intercept" && nrow(temp_summary) > 1) {
          var_name <-
            paste(.unv(unlist(strsplit(
              as.character(temp_summary$Variable[j]), ","
            ))), collapse = ":")
          var_name <- gsub(" ", "", var_name, fixed = TRUE)
          if (grepl(":", names(model_summary)[i], fixed = T)) {
            for (n in unlist(strsplit(.unv(names(
              model_summary
            )[i]), ":"))) {
              var_name <- gsub(n, "", var_name, fixed = TRUE)
            }
          } else{
            var_name <-
              gsub(.unv(names(model_summary)[i]), "", var_name, fixed = TRUE)
          }
          temp_row$level <- var_name
        }
        
        temp_table$addRows(temp_row)
      }
      
      # add message about (lack of) random effects grouping factors
      temp_table$addFootnote(.mmMessageREgrouping(options$randomVariables))
      
      # check model fit
      div_iterations <- rstan::get_num_divergent(model$stanfit)
      low_bmfi       <- rstan::get_low_bfmi_chains(model$stanfit)
      max_treedepth  <- rstan::get_num_max_treedepth(model$stanfit)
      if(any(is.infinite(rstan::summary(model$stanfit)$summary[, "Rhat"]))){
        max_Rhat     <- Inf
      }else{
        max_Rhat     <- max(rstan::summary(model$stanfit)$summary[, "Rhat"])
      }
      min_ESS        <-
        min(rstan::summary(model$stanfit)$summary[, "n_eff"])
      if (div_iterations != 0) {
        temp_table$addFootnote(.mmMessageDivergentIter(div_iterations), symbol = gettext("Warning:"))
      }
      if (length(low_bmfi) != 0) {
        temp_table$addFootnote(.mmMessageLowBMFI(length(low_bmfi)), symbol = gettext("Warning:"))
      }
      if (max_treedepth != 0) {
        temp_table$addFootnote(.mmMessageMaxTreedepth(max_treedepth))
      }
      if (max_Rhat > 1.01) {
        temp_table$addFootnote(.mmMessageMaxRhat(max_Rhat), symbol = gettext("Warning:"))
      }
      if (min_ESS < 100 * options$chains || is.nan(min_ESS)) {
        temp_table$addFootnote(.mmMessageMinESS(min_ESS, 100 * options$chains), symbol = gettext("Warning:"))
      }
      
      removed_me <- jaspResults[["mmModel"]]$object$removed_me
      removed_te <- jaspResults[["mmModel"]]$object$removed_te
      added_re   <- jaspResults[["mmModel"]]$object$added_re
      if (length(removed_me) > 0) {
        for (j in 1:length(removed_me)) {
          temp_table$addFootnote(.mmMessageOmmitedTerms1(removed_me[[j]], names(removed_me)[j]),
                                 symbol = gettext("Warning:"))
        }
      }
      if (length(removed_te) > 0) {
        for (j in 1:length(removed_te)) {
          temp_table$addFootnote(.mmMessageOmmitedTerms2(removed_te[[j]], names(removed_te)[j]),
                                 symbol = gettext("Warning:"))
        }
      }
      if (length(added_re) > 0) {
        for (i in 1:length(added_re)) {
          ANOVAsummary$addFootnote(.mmMessageAddedTerms(added_re[[i]], names(added_re)[i]), symbol = gettext("Warning:"))
        }
      }
      if (nrow(dataset) - length(model$y) != 0) {
        temp_table$addFootnote(.mmMessageMissingRows(nrow(dataset) - length(model$y)))
      }
      if (type == "BGLMM") {
        temp_table$addFootnote(.mmMessageGLMMtype(options$family, options$link))
      }
      
    }
    
  }
.mmDiagnostics    <- function(jaspResults, options, dataset, type = "BLMM") {
    if (!is.null(jaspResults[["diagnosticPlots"]]))
      return()
    
    
    diagnosticPlots <- createJaspContainer(title = gettext("Sampling diagnostics"))
    
    diagnosticPlots$position <- 5
    if (type == "BLMM") {
      dependencies <- .mmDependenciesBLMM
    } else if (type == "BGLMM") {
      dependencies <- .mmDependenciesBGLMM
    }
    diagnosticPlots$dependOn(c(
      dependencies,
      "samplingPlot",
      "samplingVariable1",
      "samplingVariable2"
    ))
    jaspResults[["diagnosticPlots"]] <- diagnosticPlots
    
    
    if (options$samplingPlot == "stan_scat" &&
        length(options$samplingVariable2) == 0) {
      diagnosticPlots[["emptyPlot"]] <- createJaspPlot()
      return()
    }
    
    model     <- jaspResults[["mmModel"]]$object$model
    
    if (options$samplingPlot != "stan_scat") {
      pars <-
        paste0(.v(unlist(options$samplingVariable1)), collapse = ":")
    } else{
      pars <- c(paste0(.v(unlist(
        options$samplingVariable1
      )), collapse = ":"),
      paste0(.v(unlist(
        options$samplingVariable2
      )), collapse = ":"))
    }
    
    plot_data <-
      .mmGetPlotSamples(model = model,
                        pars = pars,
                        options = options)
    
    
    for (i in 1:length(plot_data)) {
      if (names(plot_data)[i] == "Intercept") {
        var_name <- gettext("Intercept")
      } else{
        var_name <- strsplit(as.character(pars), ":")
        var_name <-
          sapply(var_name, function(x)
            paste(.unv(unlist(
              strsplit(x, ",")
            )), collapse = ":"))
        var_name <-
          sapply(var_name, function(x)
            gsub(" ", "", x, fixed = TRUE))
        var_name <-
          sapply(var_name, function(x)
            .mmVariableNames(x, options$fixedVariables))
        var_name <- paste0(var_name, collapse = " by ")
      }
      
      plots  <-
        createJaspPlot(
          title = var_name,
          width = 400,
          height = 300
        )
      
      if (options$samplingPlot == "stan_trace") {
        p <- .rstanPlotTrace(plot_data[[i]])
      } else if (options$samplingPlot == "stan_scat") {
        p <- .rstanPlotScat(plot_data[[i]])
      } else if (options$samplingPlot == "stan_hist") {
        p <- .rstanPlotHist(plot_data[[i]])
      } else if (options$samplingPlot == "stan_dens") {
        p <- .rstanPlotDens(plot_data[[i]])
      } else if (options$samplingPlot == "stan_ac") {
        p <- .rstanPlotAcor(plot_data[[i]])
      }
      
      
      if (options$samplingPlot %in% c("stan_hist", "stan_dens")) {
        p <- JASPgraphs::themeJasp(p, sides = "b")
        p <- p + ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.text.y  = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
        p <- p + ggplot2::labs(x = var_name)
      } else{
        p <- JASPgraphs::themeJasp(p)
      }
      if (options$samplingPlot == "stan_trace") {
        p <- p + ggplot2::theme(plot.margin = ggplot2::margin(r = 10 * (nchar(options$iteration - options$warmup) - 2)))
      }
      plots$plotObject <- p
      
      diagnosticPlots[[names(plot_data)[i]]] <- plots
    }
    
  }

# helper functions
.mmVariableNames  <- function(var_name, variables) {
  for (vn in variables) {
    inf <- regexpr(vn, var_name, fixed = TRUE)
    if (inf[1] != -1) {
      var_name <- paste0(
        substr(var_name, 0, inf[1] - 1),
        substr(var_name, inf[1], inf[1] + attr(inf, "match.length") - 1),
        " (",
        substr(
          var_name,
          inf[1] + attr(inf, "match.length"),
          nchar(var_name)
        )
      )
    }
  }
  var_name <- gsub(":", ") * ", var_name, fixed = TRUE)
  var_name <- paste0(var_name, ")")
  var_name <- gsub(" ()", "", var_name, fixed = TRUE)
  return(var_name)
}
.mmGetPlotSamples <- function(model, pars, options) {
  matrix_diff <-
    stanova::stanova_samples(model,
                             return = "array",
                             diff_intercept = options$show == "deviation")
  
  if (length(pars) == 1) {
    samples <- matrix_diff[[pars]]
    coefs   <- dim(matrix_diff[[pars]])[2]
    
    plot_data <- list()
    
    for (cf in 1:coefs) {
      
      # this is a mess but the stanova::stanova_samples returns an incomplete variable names
      coefs_trend <- attr(samples, "estimate")
      coefs_trend <- gsub("trend ('", "", coefs_trend, fixed = TRUE)
      coefs_trend <- gsub("')", "", coefs_trend, fixed = TRUE)
      coefs_trend <- strsplit(coefs_trend, ",")
      
      coefs_name <-
        paste(.unv(unlist(
          strsplit(dimnames(samples)$Parameter[cf], ",")
        )), collapse = ":")
      coefs_name <- gsub(" ", "", coefs_name, fixed = TRUE)
      coefs_name <-
        .mmVariableNames(coefs_name, options$fixedVariables)
      
      if(length(coefs_trend) > 0){
        for(cft in coefs_trend){
          if(cft %in% strsplit(pars, ":")[[1]] && !grepl(.unv(cft), coefs_name)){
            coefs_name <- paste0(coefs_name, ":", .unv(cft))
          }
        }
      }
      
      plot_data[[dimnames(samples)$Parameter[cf]]] <- list(
        samp = data.frame(
          value     = as.vector(samples[, cf,]),
          parameter = as.factor(rep(coefs_name, length(as.vector(samples[, cf,])))),
          chain     = as.factor(c(unlist(
            sapply(1:dim(samples)[3], function(x)
              rep(x, dim(samples)[1]))
          ))),
          iteration = rep(1:dim(samples)[1], dim(samples)[3])
        ),
        nchains = options$chains,
        nparams = 1,
        warmup  = 0
      )
    }
    
  } else{
    samples1 <- matrix_diff[[pars[1]]]
    samples2 <- matrix_diff[[pars[2]]]
    coefs1   <- dim(matrix_diff[[pars[1]]])[2]
    coefs2   <- dim(matrix_diff[[pars[2]]])[2]
    
    plot_data <- list()
    
    for (cf1 in 1:coefs1) {
      for (cf2 in 1:coefs2) {

        coefs_trend <- attr(samples1, "estimate")
        coefs_trend <- gsub("trend ('", "", coefs_trend, fixed = TRUE)
        coefs_trend <- gsub("')", "", coefs_trend, fixed = TRUE)
        coefs_trend <- strsplit(coefs_trend, ",")
        
        coefs1_name <-
          paste(.unv(unlist(
            strsplit(dimnames(samples1)$Parameter[cf1], ",")
          )), collapse = ":")
        coefs1_name <- gsub(" ", "", coefs1_name, fixed = TRUE)
        coefs1_name <- .mmVariableNames(coefs1_name, options$fixedVariables)
        coefs2_name <-
          paste(.unv(unlist(
            strsplit(dimnames(samples2)$Parameter[cf2], ",")
          )), collapse = ":")
        coefs2_name <- gsub(" ", "", coefs2_name, fixed = TRUE)
        coefs2_name <- .mmVariableNames(coefs2_name, options$fixedVariables)
        
        if(length(coefs_trend) > 0){
          for(cft in coefs_trend){
            if(cft %in% strsplit(pars[[1]], ":")[[1]] && !grepl(.unv(cft), coefs1_name)){
              coefs1_name <- paste0(coefs1_name, ":", .unv(cft))
            }
          }
        }
        if(length(coefs_trend) > 0){
          for(cft in coefs_trend){
            if(cft %in% strsplit(pars[[2]], ":")[[1]] && !grepl(.unv(cft), coefs2_name)){
              coefs2_name <- paste0(coefs2_name, ":", .unv(cft))
            }
          }
        }

        plot_data[[paste0(coefs1_name, ":", coefs2_name)]] <- list(
          samp = data.frame(
            value     = c(as.vector(samples1[, cf1,]),
                          as.vector(samples2[, cf2,])),
            parameter = factor(c(
              rep(coefs1_name, dim(samples1)[1] * dim(samples1)[3]),
              rep(coefs2_name, dim(samples2)[1] * dim(samples2)[3])
            ), levels = c(coefs1_name, coefs2_name)),
            chain     = as.factor(c(
              unlist(sapply(1:dim(samples1)[3], function(x)
                rep(x, dim(samples2)[1]))),
              unlist(sapply(1:dim(samples2)[3], function(x)
                rep(x, dim(samples2)[1])))
            )),
            iteration = c(rep(
              1:dim(samples1)[1], dim(samples1)[3]
            ),
            rep(
              1:dim(samples2)[1], dim(samples2)[3]
            ))
          ),
          nchains = options$chains,
          nparams = 2,
          warmup  = 0
        )
      }
    }
    
  }
  
  return(plot_data)
  
}
# as explained in ?is.integer
.is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
# modified rstan plotting functions
.rstanPlotHist  <- function(plot_data) {
  dots      <- rstan:::.add_aesthetics(list(), c("fill", "color"))
  thm       <- rstan:::rstanvis_hist_theme()
  base      <-
    ggplot2::ggplot(plot_data$samp, ggplot2::aes_string(x = "value"))
  graph <- base + do.call(ggplot2::geom_histogram, dots) +
    ggplot2::xlab("") + thm + ggplot2::xlab(unique(plot_data$samp$parameter))
  
  return(graph)
}
.rstanPlotTrace <- function(plot_data) {
  thm  <- rstan:::rstanvis_theme()
  clrs <-
    rep_len(rstan:::rstanvis_aes_ops("chain_colors"),
            plot_data$nchains)
  base <-
    ggplot2::ggplot(plot_data$samp,
                    ggplot2::aes_string(x = "iteration",
                                        y = "value", color = "chain"))
  
  graph <-
    base + ggplot2::geom_path() + ggplot2::scale_color_manual(values = clrs) +
    ggplot2::labs(x = "", y = levels(plot_data$samp$parameter)) + thm

  graph <- graph + ggplot2::scale_x_continuous(
    breaks = JASPgraphs::getPrettyAxisBreaks(c(1,max(plot_data$samp$iteration))))

  
  graph
}
.rstanPlotDens  <- function(plot_data, separate_chains = TRUE) {
  clrs <-
    rep_len(rstan:::rstanvis_aes_ops("chain_colors"),
            plot_data$nchains)
  thm  <- rstan:::rstanvis_hist_theme()
  base <-
    ggplot2::ggplot(plot_data$samp, ggplot2::aes_string(x = "value")) +
    ggplot2::xlab("")
  
  if (!separate_chains) {
    dots <- rstan:::.add_aesthetics(list(), c("fill", "color"))
    graph <- base + do.call(ggplot2::geom_density, dots) +
      thm
  } else{
    dots <- rstan:::.add_aesthetics(list(), c("color", "alpha"))
    dots$mapping <- ggplot2::aes_string(fill = "chain")
    graph <- base + do.call(ggplot2::geom_density, dots) +
      ggplot2::scale_fill_manual(values = clrs) + thm
  }
  
  graph + ggplot2::xlab(unique(plot_data$samp$parameter))
  
}
.rstanPlotScat  <- function(plot_data) {
  thm  <- rstan:::rstanvis_theme()
  dots <- rstan:::.add_aesthetics(list(), c("fill", "pt_color",
                                            "pt_size", "alpha", "shape"))
  
  p1    <-
    plot_data$samp$parameter == levels(plot_data$samp$parameter)[1]
  p2    <-
    plot_data$samp$parameter == levels(plot_data$samp$parameter)[2]
  val1  <- plot_data$samp[p1, "value"]
  val2  <- plot_data$samp[p2, "value"]
  df    <- data.frame(x = val1, y = val2)
  base  <- ggplot2::ggplot(df, ggplot2::aes_string("x", "y"))
  graph <-
    base + do.call(ggplot2::geom_point, dots) + ggplot2::labs(
      x = levels(plot_data$samp$parameter)[1],
      y = levels(plot_data$samp$parameter)[2]
    ) + thm
  graph
  
}
.rstanPlotAcor  <- function(plot_data, lags = 30) {
  clrs     <-
    rep_len(rstan:::rstanvis_aes_ops("chain_colors"),
            plot_data$nchains)
  thm      <- rstan:::rstanvis_theme()
  dots     <-
    rstan:::.add_aesthetics(list(), c("size", "color", "fill"))
  ac_dat   <-
    rstan:::.ac_plot_data(dat = plot_data$samp,
                          lags = lags,
                          partial = FALSE)
  
  dots$position <- "dodge"
  dots$stat     <- "summary"
  dots$fun.y    <- "mean"
  y_lab         <- gettext("Avg. autocorrelation")
  ac_labs       <- ggplot2::labs(x = "Lag", y = y_lab)
  y_scale       <-
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25))
  base          <-
    ggplot2::ggplot(ac_dat, ggplot2::aes_string(x = "lag", y = "ac"))
  graph         <-
    base + do.call(ggplot2::geom_bar, dots) + y_scale + ac_labs + thm
  
  graph
}


.mmCustomChecks <- list(
 collinCheck = function(dataset){
   cor_mat       <- cor(apply(dataset,2,as.numeric))
   diag(cor_mat) <- 0
   cor_mat[lower.tri(cor_mat)] <- 0
   nearOne <- 1 - abs(cor_mat) < sqrt(.Machine$double.eps)
   if(any(nearOne)){
     var_ind   <- which(nearOne, arr.ind = TRUE)
     var_names <- paste("'", .unv(rownames(cor_mat)[var_ind[,"row"]]),"' and '", .unv(colnames(cor_mat)[var_ind[,"col"]]),"'", sep = "", collapse = ", ")
     return(gettextf("The following variables are a linear combination of each other, please, remove one of them from the analysis: %s", var_names))
   }
 } 
)
.mmDependenciesLMM   <-
  c(
    "dependentVariable",
    "fixedEffects",
    "randomEffects",
    "randomVariables",
    "method",
    "bootstrap_samples",
    "test_intercept",
    "type"
  )
.mmDependenciesGLMM  <- c(.mmDependenciesLMM,
                          "dependentVariableAggregation",
                          "family",
                          "link")
.mmDependenciesBLMM  <-
  c(
    "dependentVariable",
    "fixedEffects",
    "randomEffects",
    "randomVariables",
    "warmup",
    "iteration",
    "adapt_delta",
    "max_treedepth",
    "chains",
    "seed",
    "setSeed"
  )
.mmDependenciesBGLMM <- c(.mmDependenciesBLMM,
                          "dependentVariableAggregation",
                          "family",
                          "link")
# texts and messages
.mmMessageInterpretability <-
  gettext("The intercept corresponds to the (unweighted) grand mean; for each factor with k levels, k - 1 parameters are estimated. Consequently, the estimates cannot be directly mapped to factor levels.")
.mmMessageSingularFit <-
  gettext("Model fit is singular. Specified random effects parameters (random intercepts and random slopes) cannot be estimated from the available data. Carefully reduce the random effects structure, but be aware this may induce unknown risks of anti-conservative results (i.e., p-values might be lower than nominal).")
.mmMessageVovkSellke <-
  gettext("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on a two-sided <em>p</em>-value, the maximum possible odds in favor of H\u2081 over H\u2080 equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37 (Sellke, Bayarri, & Berger, 2001).")
.mmMessageNumericalProblems <-
  gettext("Numerical problems with the maximum-likelihood estimate (e.g., gradients too large). This may indicate that the specified random effects parameters (random intercepts and random slopes) cannot be estimated from the available data. Consider carefully reducing the random effects structure, but be aware this may induce unknown risks of anti-conservative results (i.e., p-values might be lower than nominal).")
.mmMessageDFdisabled <-
  gettext("Estimation of degrees of freedom disabled (i.e., asymptotic results shown), because the number of observations is large. To force estimation, check corresponding option.")
.mmMessageResponse    <- gettext("Results are on the response scale.")
.mmMessageNotResponse <-
  gettext("Results are not on the response scale and might be misleading.")
.mmMessageANOVAtype     <- function(type) {
  gettextf("Type %s Sum of Squares",type)
}
.mmMessageREgrouping    <- function(RE_grouping_factors) {
  sprintf(
    ngettext(
      length(RE_grouping_factors),
      "The following variable is used as a random effects grouping factor: %s.",
      "The following variables are used as random effects grouping factors: %s."
    ),
    paste0("'", RE_grouping_factors, "'", collapse = ", ")
  )
}
.mmMessageMissingRE     <- gettext("This analysis requires at least one random effects grouping factor to run.")
.mmMessageMissingAgg    <- gettext("The 'Binomial (aggregated)' family requires the 'Number of trials' to be specified to run.")
.mmMessageTestNull      <- function(value) {
  gettextf("P-values correspond to test of null hypothesis against %s.", value)
}
.mmMessageAveragedOver  <- function(terms) {
  gettextf("Results are averaged over the levels of: %s.",paste(terms, collapse = ", "))
}
.mmMessageOmmitedTerms1 <- function(terms, grouping) {
  sprintf(
    ngettext(
      length(terms),
      "Factor %s does not vary within the levels of random effects grouping factor '%s'. All random slopes involving %s have been removed for '%s'.",
      "Factors %s do not vary within the levels of random effects grouping factor '%s'. All random slopes involving %s have been removed for '%s'."
    ),
    paste0("'", terms, "'", collapse = ", "),
    grouping,
    paste0("'", terms, "'", collapse = ", "),
    grouping
  )
}
.mmMessageOmmitedTerms2 <- function(terms, grouping) {
  gettextf(
    "Number of observations is not sufficient for estimating %s random slopes for random effects grouping factor '%s'. Consequently, random slopes for %s have been removed for '%s'.",
    paste0("'", terms, "'", collapse = ", "),
    grouping,
    paste0("'", terms, "'", collapse = ", "),
    grouping
  )
}
.mmMessageAddedTerms    <- function(terms, grouping) {
  sprintf(
    ngettext(
      length(terms),
      "Lower order random effects terms need to be specified in presence of the higher order random effects terms. Therefore, the following random effects term was added to the '%s' random effects grouping factor: '%s.'",
      "Lower order random effects terms need to be specified in presence of the higher order random effects terms. Therefore, the following random effects terms were added to the '%s' random effects grouping factor: '%s.'"
    ),
    grouping,
    paste0("'", terms, "'", collapse = ", ")
  )
}
.mmMessageMissingRows   <- function(value) {
  sprintf(
    ngettext(
      value,
      "%i observation was removed due to missing values.",
      "%i observations were removed due to missing values."
    ),
    value
  )
}
.mmMessageGLMMtype      <- function(family, link) {
  family <- switch(family,
  "binomial"         = gettext("binomial"),
  "binomial_agg"     = gettext("binomial"),
  "gaussian"         = gettext("gaussian"),
  "Gamma"            = gettext("gamma"),
  "inverse.gaussian" = gettext("inverse gaussian"),
  "poisson"          = gettext("poisson"),
  "neg_binomial_2"   = gettext("negative binomial"),
  "betar"            = gettext("beta"),
  )
  gettextf("Generalized linear mixed model with %s family and %s link function.",
           family,
           link)
}
.mmMessageTermTest      <- function(method) {
  method <- switch(method,
  "S"   = gettext("Satterthwaite"),
  "KR"  = gettext("Kenward-Roger"),
  "LRT" = gettext("likelihood ratio tests"),
  "PB"  = gettext("parametric bootstrap")
  )
  gettextf("Model terms tested with %s method.",method)
}
.messagePvalAdjustment  <- function(adjustment) {
  if (adjustment == "none") {
    return(gettext("P-values are not adjusted."))
  }
  adjustment <- switch(adjustment,
  "holm"       = gettext("Holm"),
  "hommel"     = gettext("Homel"),
  "hochberg"   = gettext("Hochberg"),
  "mvt"        = gettext("Multivariate-t"),
  "tukey"      = gettext("Turkey"),
  "BH"         = gettext("Benjamini-Hochberg"),
  "BY"         = gettext("Benjamini-Yekutieli"),
  "scheffe"    = gettext("Scheffe"),
  "sidak"      = gettext("Sidak"),
  "dunnettx"   = gettext("Dunnett"),
  "bonferroni" = gettext("Bonferroni")
  )
  return(gettextf("P-values are adjusted using %s adjustment.",adjustment))
}
.mmMessageDivergentIter <- function(iterations) {
  sprintf(
    ngettext(
      iterations,
      "The Hamiltonian Monte Carlo procedure might be invalid -- There was %i divergent transition after warmup. This can be solved by carefully increasing 'Adapt delta' until there are no divergent transitions.",
      "The Hamiltonian Monte Carlo procedure might be invalid -- There were %i divergent transitions after warmup. This can be solved by carefully increasing 'Adapt delta' until there are no divergent transitions."
    ),
    iterations
  )
}
.mmMessageLowBMFI       <- function(nChains) {
  sprintf(
    ngettext(
      nChains,
      "Bayesian Fraction of Missing Information (BFMI) that was too low in %i chain indicating that the posterior distribution was not explored efficiently. Try increasing number of 'Burnin' and 'Iterations'.",
      "Bayesian Fraction of Missing Information (BFMI) that was too low in %i chains indicating that the posterior distribution was not explored efficiently. Try increasing number of 'Burnin' and 'Iterations'."
    ),
    nChains
  )
}
.mmMessageMaxTreedepth  <- function(iterations) {
  sprintf(
    ngettext(
      iterations,
      "The Hamiltonian Monte Carlo procedure might be inefficient -- %i transition exceeded the maximum tree depth. This can be solved by carefully increasing 'Maximum tree depth",
      "The Hamiltonian Monte Carlo procedure might be inefficient -- %i transitions exceeded the maximum tree depth. This can be solved by carefully increasing 'Maximum tree depth"
    ),
    iterations
  )
}
.mmMessageMaxRhat       <- function(Rhat) {
  gettextf(
    "Inference possibly unreliable -- MCMC chains might not have converged; The largest R-hat is %.3f > 1.01. To lower R-hat please increase 'Iterations', or 'Adapt delta' in the Options section.",
    Rhat
  )
}
.mmMessageMinESS        <- function(ESS, treshold) {
  gettextf(
    "Low estimation accuracy -- The smallest Effective Sample Size (ESS) is %.2f < %1.0f. To increase accuracy please increase 'Iterations', or 'Adapt delta' in the Options section.",
    ESS,
    treshold
  )
}
.mmMessageBadWAIC       <- function(n_bad) {
  sprintf(
    ngettext(
      n_bad,
      "WAIC estimate unreliable -- There was %1.0f p_waic estimate larger than 0.4. We recommend using LOO instead.",
      "WAIC estimate unreliable -- There were %1.0f p_waic estimates larger than 0.4. We recommend using LOO instead."
    ),
    n_bad
  )
}
.mmMessageBadLOO        <- function(n_bad) {
  sprintf(
    ngettext(
      n_bad,
      "LOO estimate unreliable -- There was %1.0f observation with the shape parameter (k) of the generalized Pareto distribution higher than > .5.",
      "LOO estimate unreliable -- There were %1.0f observations with the shape parameter (k) of the generalized Pareto distribution higher than > .5."
    ),
    n_bad
  )
}
.mmMessageFitType       <- function(REML) {
  gettextf("The model was fitted using %s.",
           ifelse(REML, gettext("restricted maximum likelihood"), gettext("maximum likelihood")))
}
