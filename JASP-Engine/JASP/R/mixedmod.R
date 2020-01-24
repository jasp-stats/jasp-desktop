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
mixedmod<- function(jaspResults, dataset, options) {

  ### check if they have an IV and a DV
  ready <- options$dependent != "" && length(options$variables) > 0 && options$rvariables != ""

  ### read in the dataset if it's ready
  if (ready){
    dataset = .read_mixedmod_data(dataset, options)
  
    ### check for categorical/numeric variables
    check.non.number = function(x){
      return.bool = ifelse(is.character(x) | is.factor(x), TRUE, FALSE)
      return.bool
    }
    character = sapply(dataset[,options$variables, drop=F], check.non.number)
    numeric = !character
    
  }
  
  #### compute results
  if (is.null(jaspResults[["mixedmod_results"]]))
    .mixedmod_compute(jaspResults, dataset, options, ready)
  
  #### show plots (if user specifies them)
  if (options$model) {
    if (is.null(jaspResults[["mixedmod_model_plot"]])){
      .mixedmod_model_plot(jaspResults, options, ready)
    }
  }
  
  #### show plots (if user specifies them)
  if (options$univariates) {
    if (is.null(jaspResults[["mixedmod_univariate_plot"]])){
      .mixedmod_univariate_plot(jaspResults, options, ready, dataset)
    }
  }
  
  if (options$residuals) {
    if (is.null(jaspResults[["mixedmod_residual_plot"]])){
      .mixedmod_residual_plot(jaspResults, options, ready)
    }
  }

  ### report fixed effects
  if (options$fixeff){
    if (is.null(jaspResults[["mixedmod_table_fixed"]])){
      .create_mixedmod_fixed(jaspResults, options, ready)
    }
  }
  
  ### report random effects
  if (options$randeff){
    if (is.null(jaspResults[["mixedmod_table_random"]])){
      .create_mixedmod_random(jaspResults, options, ready)
    }
  }

}

# table functions ---------------------------------------------------------
.create_mixedmod_random <- function(jaspResults, options, ready) {
  mixedmod_table_random <- createJaspTable(title = "Variability of Random Effects")
  
  ### which options are required
  mixedmod_table_random$dependOn(c("variables", "dependent", "rvariables", "fixeff", "randeff2", "interactions"))
  
  ### add citation
  mixedmod_table_random$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  mixedmod_table_random$addColumnInfo(name = "source",      title = "Source",   type = "string", combine = TRUE)
  mixedmod_table_random$addColumnInfo(name = "param",    title = "Parameter",       type = "number", format = "dp:2", combine = TRUE)		
  mixedmod_table_random$addColumnInfo(name = "std",    title = "Standard Deviation",       type = "number", format = "dp:2", combine = TRUE)		
  
  ### store the table structure
  jaspResults[["mixedmod_table_random"]] <- mixedmod_table_random
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  mixedmod_results <- jaspResults[["mixedmod_results"]]$object
  
  ### fill the table with those results
  .fill_mixedmod_table_random(mixedmod_table_random, mixedmod_results)
  
  return()
  
}

.fill_mixedmod_table_random = function(mixedmod_table_random, mixedmod_results){
  factors = summary(mixedmod_results, correlation=FALSE)$varcor
  random.name = names(factors)
  estimates = attr(factors[["School"]], "stddev")
  random.name = c(random.name, rep("", times=length(estimates)-1), "Residual")
  ### output results
  tabdat = list(
    source = random.name,
    param = c(names(estimates), ""),
    std = c(estimates, summary(mixedmod_results)$sigma)
  )
  mixedmod_table_random$setData(tabdat)
  
  
  return()
}

.create_mixedmod_fixed <- function(jaspResults, options, ready) {
  mixedmod_table_fixed <- createJaspTable(title = "Fixed Effects")
  
  ### which options are required
  mixedmod_table_fixed$dependOn(c("variables", "dependent", "rvariables", "fixeff", "randeff2", "interactions"))
  
  ### add citation
  mixedmod_table_fixed$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  mixedmod_table_fixed$addColumnInfo(name = "var",      title = "Variable",   type = "string", combine = TRUE)
  mixedmod_table_fixed$addColumnInfo(name = "est",    title = "Estimate",       type = "number", format = "dp:2", combine = TRUE)		
  mixedmod_table_fixed$addColumnInfo(name = "sterr",    title = "Standard Error",       type = "number", format = "dp:2", combine = TRUE)		
  mixedmod_table_fixed$addColumnInfo(name = "t",      title = "t-statistic",    type = "number", format = "dp:2", combine = TRUE)	
  
  ### store the table structure
  jaspResults[["mixedmod_table_fixed"]] <- mixedmod_table_fixed
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  mixedmod_results <- jaspResults[["mixedmod_results"]]$object
  
  ### fill the table with those results
  .fill_mixedmod_table_fixed(mixedmod_table_fixed, mixedmod_results)
  
  return()
  
}

.fill_mixedmod_table_fixed = function(mixedmod_table_fixed, mixedmod_results){
  factors = summary(mixedmod_results, correlation=FALSE)$coefficients

  ### output results
  tabdat = list(
    var = row.names(factors),
    est = factors[,1],
    sterr = factors[,2],
    t = factors[, 3]
  )
  mixedmod_table_fixed$setData(tabdat)
  
  
  return()
}


# plotting functions ------------------------------------------------------
.mixedmod_model_plot <- function(jaspResults, options, ready) {
  
  ### create plot options
  modelplot <- createJaspPlot(title = "Plot of the Statistical Model",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  modelplot$dependOn(c("variables", "model", "dependent", "theme"))
  
  ### fill the plot object
  jaspResults[["modelplot"]] <- modelplot
  
  if (!ready)
    return()
  
  ### create the flexplot
  model.type = "model"
  .create_flexplot(jaspResults, modelplot, options, model.type)
  
  return()
}

.mixedmod_univariate_plot <- function(jaspResults, options, ready, dataset) {
  
  ### create plot options
  uniplot <- createJaspPlot(title = "Univariate Plots",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  uniplot$dependOn(c("dependent", "variables", "theme", "univariates"))
  
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

.mixedmod_residual_plot <- function(jaspResults, options, ready) {
  
  ### create plot options
  residualplot <- createJaspPlot(title = "Diagnostic Plots",  width = 800, height = 500)
  
  ### what options should change the flexplot?
  residualplot$dependOn(c("variables", "residuals", "model", "dependent", "interactions"))
  
  ### fill the plot object
  jaspResults[["residualplot"]] <- residualplot
  
  if (!ready)
    return()
  
  mixedmod_results <- jaspResults[["mixedmod_results"]]$object
  plot = flexplot::visualize(mixedmod_results, plot="residuals", plots.as.list=TRUE,
                   alpha=options$alpha, jitter=c(options$jitx, options$jity))
  plot = arrange_jasp_plots(plot, options$theme)
  residualplot$plotObject <- plot
  
  return()
}



.create_flexplot <- function(jaspResults, flexplot, options, model.type) {
  
  mixedmod_results <- jaspResults[["mixedmod_results"]]$object
  plot = flexplot::visualize(mixedmod_results, plot="model", alpha = options$alpha, sample = options$nsamp,
                   jitter=c(options$jitx, options$jity))
  plot = theme_it(plot, options$theme)

  flexplot$plotObject <- plot

  return()
}



# other functions ---------------------------------------------------------

.mixedmod_compute = function(jaspResults, dataset, options, ready) {
  
  if (ready){
    ## createJaspState allows these results to be recycled
    mixedmod_results <- createJaspState()
    jaspResults[["mixedmod_results"]] <- mixedmod_results
    mixedmod_results$dependOn(c("dependent", "variables", "interactions", "randeff2"))

    # create the mixed model formula ------------------------------------------
    fe = unlist(lapply(options$interactions, FUN=function(x) unlist(x$components)))
    fixed.effects = paste0(
      unlist(
        lapply(options$interactions, FUN=function(x) paste0(unlist(x$components), collapse="*"))
      ), 
      collapse=" + ")
    random.effects = unlist(
        lapply(options$interactions, FUN=function(x) unlist(x$randeff2))
          )
    random.effects = fe[random.effects]
    if (length(random.effects)<1) random.effects = "1"
    random.effects = paste0(random.effects, collapse="+")
    f = paste0(options$dependent, " ~ ", fixed.effects, " + (", random.effects, " | ", options$rvariables, ")", collapse = "")
    #save(f, dataset,options, file="/Users/fife/Documents/flexplot/jaspresults.Rdata")f
    f = as.formula(f)
    #save(f, dataset,options, file="/Users/fife/Documents/flexplot/jaspresults.Rdata")

    # fit the model -----------------------------------------------------------
    mod = lme4::lmer(f, data=dataset)
    #save(options, dataset, mod, f, file="/Users/fife/Documents/flexplot/jaspresults.Rdata")
    mixedmod_results$object = mod
    return()
  }
}

.check_mixedmod_error = function(dataset, options){
  
  # check length of variables
  if ((options$dependent == "" & length(options$paneledVars)>0) | (options$dependent == "" & length(options$variables)>0)) .quitAnalysis("You must specify a dependent variable to view a graphic")
  if (options$dependent != "" & length(options$paneledVars)>0) .quitAnalysis("You must have at least one independent variable to do paneling")
  
}

.read_mixedmod_data = function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    dataset = .readDataSetToEnd(columns=(c(options$dependent, options$variables, options$rvariables))) 
    ## variable names in the dataset are encoded. de-encodify them
    names(dataset) = JASP:::.unv(names(dataset))
    return(dataset)
}
