#' General Linear Model in JASP
#' 
#' This function was developed for use in JASP. It takes a dataset as input with user 
#' options and returns results (tables, plots, etc)
#'
#' @param jaspResults A JASP object
#' @param dataset dataset supplied by JASP
#' @param options a list of options to pass to JASP
#'
#' @return a table, plot, etc. 
#' @export
linmod<- function(jaspResults, dataset, options) {

  ### check if they have an IV and a DV
  ready <- (options$dependent != "" & length(options$variables)>0)
  
  ### read in the dataset if it's ready
  if (ready){
    dataset = .read_linmod_data(dataset, options)
  
    ### check for categorical/numeric variables
    check.non.number = function(x){
      return.bool = ifelse(is.character(x) | is.factor(x), TRUE, FALSE)
      return.bool
    }
    character = sapply(dataset[,options$variables, drop=F], check.non.number)
    numeric = !character
  }

  ### compute results
  if (is.null(jaspResults[["linmod_results"]]))
    .linmod_compute(jaspResults, dataset, options, ready)

  ### show plots (if user specifies them)
  if (options$model) {

    if (is.null(jaspResults[["linmod_model_plot"]])){
      .linmod_model_plot(jaspResults, options, ready)
    }
  }
  
  if (options$avp) {
    if (is.null(jaspResults[["linmod_avp_plot"]])){
      .linmod_avp_plot(jaspResults, options, ready)
    }
  }
  
  if (options$residuals) {
    if (is.null(jaspResults[["linmod_residual_plot"]])){
      .linmod_residual_plot(jaspResults, options, ready)
    }
  }
  #### show plots (if user specifies them)
  if (options$univariate) {
    if (is.null(jaspResults[["linmod_univariate_plot"]])){
      .linmod_univariate_plot(jaspResults, options, ready, dataset)
    }
  }

  ### show output, depending on results
  if (ready && sum(numeric)>0){

    if (length(options$variables)>1){
      if (options$modinf){
        if (is.null(jaspResults[["linmod_table_modcomp"]])){
          .create_linmod_table_modcomp(jaspResults, options, ready)
        }
      }
    }

    if (options$sl){
      if (is.null(jaspResults[["linmod_table_slopes"]])){
        .create_linmod_table_slopes(jaspResults, options, ready)
      }
    }
  }

  if (ready && sum(character)>0){

    if (length(options$variables)>1){
      if (options$modinf) {
        if (is.null(jaspResults[["linmod_table_modcomp"]])){
          .create_linmod_table_modcomp(jaspResults, options, ready)
        }
      }
    }

    ### check if there's a jasp table already. if not, create it
    if (options$means){
      if (is.null(jaspResults[["linmod_table_means"]])){
      .create_linmod_table_means(jaspResults, options, ready)
      }
    }

    if (options$diff) {
      if (is.null(jaspResults[["linmod_table_differences"]])){
        .create_linmod_table_differences(jaspResults, options, ready)
      }
    }

  }

}

.linmod_model_plot <- function(jaspResults, options, ready) {
  
  ### create plot options
  modelplot <- createJaspPlot(title = "Plot of the Statistical Model",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  modelplot$dependOn(c("variables", "residuals", "model", "dependent", "interactions"))
  
  ### fill the plot object
  jaspResults[["modelplot"]] <- modelplot
  
  if (!ready)
    return()
  
  ### create the flexplot
  model.type = "model"
  
  .create_flexplot_linmod(jaspResults, modelplot, options, model.type)
  
  return()
}

.linmod_univariate_plot <- function(jaspResults, options, ready, dataset) {
  
  ### create plot options
  uniplot <- createJaspPlot(title = "Univariate Plots",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  uniplot$dependOn(c("dependent", "variables", "theme", "univariate"))
  
  ### fill the plot object
  jaspResults[["uniplot"]] <- uniplot
  
  if (!ready)
    return()
  
  ### loop through and plot everything
  all.variables = c(options$dependent, options$variables)
  
  a = theme_it(flexplot::flexplot(flexplot::make.formula(options$dependent, "1"), dataset), options$theme)
  plot.list = list(rep(a, times=length(all.variables)))
  plot.list[[1]] = a
  for (i in 2:length(all.variables)){
    p = theme_it(flexplot::flexplot(flexplot::make.formula(options$variables[i-1], "1"), dataset), options$theme)
    plot.list[[i]] = p
  }
  #save(all.variables, options, dataset, plot.list, file="/Users/fife/Documents/flexplot/jaspresults.Rdata")
  if (length(options$variables)<3){
    nc = length(options$variables) + 1
  } else if ((length(options$variables)+1)/2 == round((length(options$variables)+1)/2)){
    nc = 2
  } else {
    nc = 3
  }
  uniplot$plotObject <- cowplot::plot_grid(plotlist= plot.list, ncol=nc)
  
  return()
}

.linmod_avp_plot <- function(jaspResults, options, ready) {
  
  ### create plot options
  addedplot <- createJaspPlot(title = "Added Variable Plot",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  addedplot$dependOn(c("variables", "residuals", "model", "dependent", "avp", "interactions"))
  
  ### fill the plot object
  jaspResults[["avp"]] <- addedplot
  
  if (!ready)
    return()
  
  ### create the flexplot
  model.type = "added"
  .create_flexplot_linmod(jaspResults, addedplot, options, model.type)
  
  return()
}

.linmod_residual_plot <- function(jaspResults, options, ready) {
  
  ### create plot options
  residualplot <- createJaspPlot(title = "Diagnostic Plots",  width = 800, height = 500)
  
  ### what options should change the flexplot?
  residualplot$dependOn(c("variables", "residuals", "model", "dependent", "interactions"))
  
  ### fill the plot object
  jaspResults[["residualplot"]] <- residualplot
  
  if (!ready)
    return()
  
  ### create the flexplot
  model.type = "residuals"
  .create_flexplot_linmod(jaspResults, residualplot, options, model.type)
  
  return()
}

.create_flexplot_linmod <- function(jaspResults, flexplot, options, model.type) {
  
  linmod_results <- jaspResults[["linmod_results"]]$object
  
  generated.formula = flexplot:::make_flexplot_formula(options$variables, options$dependent, linmod_results$model$model)
  

  if	(options$ghost & length(options$variables)<4){
    ghost=rgb(1,0,0,.4)
  } else {
    ghost = NULL
  }
  
  whiskers = list("quartiles" = "quartiles",
                  "standard errors" = "sterr",
                  "standard deviations", "stdev")
  if (model.type=="model"){
    plot = flexplot::compare.fits(generated.formula, data = linmod_results$model$model, model1 = linmod_results$model,
                      alpha=options$alpha, ghost.line=ghost, jitter=c(options$jitx, options$jity))
  } else if (model.type == "residuals"){
    plot = flexplot::visualize(linmod_results$model, linmod_results, plot=model.type, plots.as.list=TRUE,
                     alpha=options$alpha, jitter=c(options$jitx, options$jity))
    plot = arrange_jasp_plots(plot, options$theme)
  } else if (model.type == "added"){
    
    methods = list("Regression"="lm", 
                   "Quadratic"="quadratic", 
                   "Cubic"="cubic")
    formla = flexplot::make.formula(options$dependent,options$variables)
    plot = flexplot::added.plot(formla, linmod_results$model$model, method=methods[options$linetype], alpha=options$alpha,
                      jitter=c(options$jitx, options$jity))
  }
  
  if (options$theme == "JASP"){
    plot = JASPgraphs::themeJasp(plot)
  } else {
    theme = list("Black and white"="ggplot2::theme_bw()+ ggplot2::theme(text=ggplot2::element_text(size=18))",
                 "Minimal" = "ggplot2::theme_minimal()+ ggplot2::theme(text=ggplot2::element_text(size=18))",
                 "Classic" = "ggplot2::theme_classic()+ ggplot2::theme(text=ggplot2::element_text(size=18))",
                 "Dark" = "ggplot2::theme_dark() + ggplot2::theme(text=ggplot2::element_text(size=18))")
    plot = plot + eval(parse(text=theme[[options$theme]]))
  }  
  
  if (length(options$variables)<4){
    plot = plot + ggplot2::theme(legend.position = "none")      
  }
    
      #+ theme(legend.position = "none")
  #flexplot$addFootnote("message")
  flexplot$plotObject <- plot

  
  return()
}

.linmod_compute = function(jaspResults, dataset, options, ready) {
  
  if (ready){
    ## createJaspState allows these results to be recycled
    linmod_results <- createJaspState()
    jaspResults[["linmod_results"]] <- linmod_results
    linmod_results$dependOn(c("dependent", "variables", "interactions", "linetype"))
    
    ## interactions are stored in a deeply nested list. de-listify them
    predictors = paste0(
      unlist(
        lapply(options$interactions, FUN=function(x) paste0(unlist(x$components), collapse="*"))
      ), 
      collapse=" + ")
    
    # add variables with polynomial terms -------------------------------------
    vars = unlist(lapply(options$interactions, FUN=function(x) unlist(x$components)))
    polys = unlist(lapply(options$interactions, FUN=function(x) unlist(x$polynoms)))
    vars.with.poly = vars[polys]
    # specify degree
    if (options$linetype=="Quadratic" & length(vars.with.poly)>0){
      vars = paste0(add_polynomials(vars.with.poly, dataset, 2), collapse=" + ")
      predictors = paste0(predictors, " + ", vars)
    } else if (options$linetype == "Cubic" &  length(vars.with.poly)>0){
      vars = paste0(add_polynomials(vars.with.poly, dataset, 3), collapse=" + ")
      predictors = paste0(predictors, " + ", vars)
    }
    
    # create formula
    f = paste0(options$dependent, " ~ ", predictors, collapse = "")
    f = as.formula(f)
    
    ### store all the information
    model = lm(f, dataset)
    est = flexplot::estimates(model)
    #save(options, dataset, ready, model, file="/Users/fife/Documents/jaspresults.Rdat")
    est$model = model
    
     
    linmod_results$object = est
    
    return()
  }
}

.create_linmod_table_means <- function(jaspResults, options, ready) {
  linmod_table_means <- createJaspTable(title = "Means of Categorical Variables")
  
  ### which options are required
  linmod_table_means$dependOn(c("dependent", "variables", "ci", "interactions", "means", "diff", "sl", "modinf"))
  
  ### add citation
  linmod_table_means$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  linmod_table_means$addColumnInfo(name = "var",      title = "Variable",   type = "string", combine = TRUE)
  linmod_table_means$addColumnInfo(name = "levels",    title = "Level",       type = "string", combine = TRUE)	
  linmod_table_means$addColumnInfo(name = "est",      title = "Estimate",    type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    linmod_table_means$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    linmod_table_means$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  ### add message about what type of interval was used
  message <- paste0("")
  linmod_table_means$addFootnote(message)  
  linmod_table_means$showSpecifiedColumnsOnly <- TRUE
  
  ### store the table structure
  jaspResults[["linmod_table_means"]] <- linmod_table_means
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  linmod_results <- jaspResults[["linmod_results"]]$object
  
  ### fill the table with those results
  .fill_linmod_table_means(linmod_table_means, linmod_results)
  
  return()
  
}

.create_linmod_table_differences <- function(jaspResults, options, ready) {
  linmod_table_differences <- createJaspTable(title = "Mean Differences Between Groups")
  
  ### which options are required
  linmod_table_differences$dependOn(c("dependent", "variables", "ci", "interactions", "means", "diff", "sl", "modinf"))
  
  ### add citation
  linmod_table_differences$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  linmod_table_differences$addColumnInfo(name = "var",      title = "Variable",   type = "string", combine = TRUE)
  linmod_table_differences$addColumnInfo(name = "comparison",    title = "Comparison",       type = "string", combine = TRUE)	
  linmod_table_differences$addColumnInfo(name = "diff",      title = "Difference",    type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    linmod_table_differences$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    linmod_table_differences$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  linmod_table_differences$addColumnInfo(name = "cohensd",      title = "Cohen's d",    type = "number", format = "dp:2", combine = TRUE)	
  
  ### add message about what type of interval was used
  message <- paste0("")
  linmod_table_differences$addFootnote(message)  
  linmod_table_differences$showSpecifiedColumnsOnly <- TRUE
  ### store the table structure
  jaspResults[["linmod_table_differences"]] <- linmod_table_differences
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  linmod_results <- jaspResults[["linmod_results"]]$object
  
  ### fill the table with those results
  .fill_linmod_table_differences(linmod_table_differences, linmod_results)
  
  return()
}

.create_linmod_table_slopes <- function(jaspResults, options, ready) {
  linmod_table_slopes <- createJaspTable(title = "Regression Slopes and Intercept")
  
  ### which options are required
  linmod_table_slopes$dependOn(c("dependent", "variables", "ci", "interactions", "means", "diff", "sl", "modinf", "linetype"))
  
  ### add citation
  linmod_table_slopes$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  linmod_table_slopes$addColumnInfo(name = "var",      title = "Variables",   type = "string", combine = TRUE)
  linmod_table_slopes$addColumnInfo(name = "val",    title = "Value",       type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    linmod_table_slopes$addColumnInfo(name = "lwr",      title = "Lower",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    linmod_table_slopes$addColumnInfo(name = "upr",      title = "Upper",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  linmod_table_slopes$addColumnInfo(name = "std",    title = "Standardized Slope (β)",       type = "number", format = "dp:2", combine = TRUE)	
  
  if (options$ci){
    linmod_table_slopes$addColumnInfo(name = "slwr",      title = "Lower β",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
    linmod_table_slopes$addColumnInfo(name = "supr",      title = "Upper β",       type = "number", format = "dp:2", combine = TRUE, overtitle = "95% Confidence Interval")
  }
  
  ### add message about what type of interval was used
  message <- gettextf("Confidence intervals computed 95%% Confidence Intervals.")
  message = paste0(message, gettext("\n All estimates are conditional estimates."))
  linmod_table_slopes$addFootnote(message)  
  linmod_table_slopes$showSpecifiedColumnsOnly <- TRUE
  ### store the table structure
  jaspResults[["linmod_table_slopes"]] <- linmod_table_slopes
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  linmod_results <- jaspResults[["linmod_results"]]$object
  
  ### fill the table with those results
  .fill_linmod_table_slopes(linmod_table_slopes, linmod_results)
  
  return()
}

.create_linmod_table_modcomp <- function(jaspResults, options, ready) {
  linmod_table_modcomp <- createJaspTable(title = "Model Comparisons (Estimating the Effect of Removing Terms)")
  
  ### which options are required
  linmod_table_modcomp$dependOn(c("dependent", "variables", "ci", "interactions", "means", "diff", "sl", "modinf", "linetype"))
  
  ### add citation
  linmod_table_modcomp$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  linmod_table_modcomp$addColumnInfo(name = "terms",      title = "Term",   type = "string", combine = TRUE)
  linmod_table_modcomp$addColumnInfo(name = "rsq",    title = "Semi-partial R Squared",       type = "number", format = "dp:2", combine = TRUE)	
  linmod_table_modcomp$addColumnInfo(name = "bayes",      title = "Semi-partial Bayes Factor", type = "number", combine = TRUE)
  linmod_table_modcomp$addColumnInfo(name = "bayesinv",      title = "Inverted Bayes Factor", type = "number", combine = TRUE)
  
  
  message = paste0("Note: Semi-partials indicate the effect of removing that particular term from the model. ",
    "Bayes factors are computed using the BIC.")
  if (length(options$interactions)>0){
    message = paste0(message, "\n 
                     Main effect estimates of r squared and BF have been suppressed because there is an interaction in the model.")
  }
  linmod_table_modcomp$addFootnote(message)  
  linmod_table_modcomp$showSpecifiedColumnsOnly <- TRUE
  
  ### store the table structure
  jaspResults[["linmod_table_modcomp"]] <- linmod_table_modcomp
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  linmod_results <- jaspResults[["linmod_results"]]$object
  
  ### fill the table with those results
  .fill_linmod_table_modcomp(linmod_table_modcomp, linmod_results)
  
  return()
}


.fill_linmod_table_means = function(linmod_table_means, linmod_results){
  

  factors = linmod_results$factor.summary
  
  ### output results
  tabdat = list(
    var = as.character(factors$variables),
    levels = factors$levels,
    est = factors$estimate,
    lwr = factors$lower,
    upr = factors$upper
  )
  linmod_table_means$setData(tabdat)
  
  
  return()
}

.fill_linmod_table_differences = function(linmod_table_differences, linmod_results){
  
  
  diff = linmod_results$difference.matrix
  
  ### output results
  tabdat = list(
    var = as.character(diff$variables),
    comparison = diff$comparison,
    diff = diff$difference,
    lwr = diff$lower,
    upr = diff$upper,
    cohensd = diff$cohens.d
  )
  
  linmod_table_differences$setData(tabdat)
  
  
  return()
}

.fill_linmod_table_slopes = function(linmod_table_slopes, linmod_results){
  
  slopes = linmod_results$numbers.summary
  
  ### output results
  tabdat = list(
    var = as.character(slopes$variables),
    val = slopes$estimate,
    lwr = slopes$lower,
    upr = slopes$upper,
    std = slopes$std.estimate,
    slwr = slopes$std.lower,
    supr = slopes$std.upper
  )
  
  linmod_table_slopes$setData(tabdat)
  
  
  return()
}

.fill_linmod_table_modcomp = function(linmod_table_modcomp, linmod_results){
  
  mc = linmod_results$model.comparison
  save(linmod_table_modcomp, linmod_results, file="/Users/fife/Documents/flexplot/jaspresults.rdata")
  
  ### reformat : to be a times
  term.labels = as.character(mc$all.terms)
  main.effects = main_effects_2_remove(term.labels)
  term.labels = gsub(":", "×", term.labels)
  
  ### output results
  tabdat = list(
    terms = term.labels,
    rsq = mc$rsq,
    bayes = mc$bayes.factor,
    bayesinv = 1/mc$bayes.factor
  )
  
  #### remove main effects where there's an interaction present
  condition.me = term.labels %in% main.effects
  tabdat$rsq[condition.me] = NA
  tabdat$bayes[condition.me] = NA
  tabdat$bayesinv[condition.me] = NA
  
  ### remove stats for main effects
  
  ### remove the main effects for models with interaction terms
  #save(mc, file="/Users/fife/Documents/jaspresults.rdat")
  linmod_table_modcomp$setData(tabdat)
  
  
  return()
}


.check_linmod_error = function(dataset, options){
  
  # check length of variables
  if ((options$dependent == "" & length(options$paneledVars)>0) | (options$dependent == "" & length(options$variables)>0)) .quitAnalysis("You must specify a dependent variable to view a graphic")
  if (options$dependent != "" & length(options$paneledVars)>0) .quitAnalysis("You must have at least one independent variable to do paneling")
  
}

.read_linmod_data = function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    dataset = .readDataSetToEnd(columns=(c(options$dependent, options$variables))) 
    ## variable names in the dataset are encoded. de-encodify them
    names(dataset) = JASP:::.unv(names(dataset))
    return(dataset)
}
