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
glinmod_jasp<- function(jaspResults, dataset, options) {

  ### check if they have an IV and a DV
  ready <- (options$dependent != "" & length(options$variables)>0)
  
  ### read in the dataset if it's ready
  if (ready){
    dataset = .read_glinmod_data(dataset, options)
    #save(dataset, options, file="/Users/fife/Documents/jaspdata.rdata")
    #### modify dataset to convert to numeric (where applicable)

    dataset = modify_dv(dataset, options$dependent, options$family)
    #.check_glinmod_error()  #### HOW DO YOU HAVE IT THROW AN ERROR IF THE VARIABLE IS NOT NUMBERIC?
  
    ### check for categorical/numeric variables
    check.non.number = function(x){
      return.bool = ifelse(is.character(x) | is.factor(x), TRUE, FALSE)
      return.bool
    }
    character = sapply(dataset[,options$variables, drop=F], check.non.number)
    numeric = !character
    
    #### compute results
    if (is.null(jaspResults[["glinmod_results"]]))
      .glinmod_compute(jaspResults, dataset, options, ready)
    
    
    #### show plots (if user specifies them)
    if (options$model) {
      if (is.null(jaspResults[["glinmod_model_plot"]])){
        .glinmod_model_plot(jaspResults, options, ready, dataset)
      }
    }
    
    #### show plots (if user specifies them)
    if (options$univariates) {
      if (is.null(jaspResults[["glinmod_univariate_plot"]])){
        .glinmod_univariate_plot(jaspResults, options, ready, dataset)
      }
    }
    
    ### report parameter estimates
    if (options$ests){
      if (is.null(jaspResults[["glinmod_table_fixed"]])){
        .create_glinmod_coefs(jaspResults, options, ready)
      }
    }


  }  
}




# Plot the model ----------------------------------------------------------

.glinmod_univariate_plot <- function(jaspResults, options, ready, dataset) {
  
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
  
  a = theme_it(flexplot::flexplot(make.formula(options$dependent, "1"), dataset), options$theme)
  plot.list = list(rep(a, times=length(all.variables)))
  plot.list[[1]] = a
  for (i in 2:length(all.variables)){
    p = theme_it(flexplot::flexplot(make.formula(options$variables[i-1], "1"), dataset), options$theme)
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

.glinmod_model_plot <- function(jaspResults, options, ready, dataset) {
  
  ### create plot options
  modelplot <- createJaspPlot(title = "Plot of the Statistical Model",  width = 900, height = 500)
  
  ### what options should change the flexplot?
  modelplot$dependOn(c("dependent", "variables", "interactions", "family", "ghost", "theme"))
  
  ### fill the plot object
  jaspResults[["modelplot"]] <- modelplot
  
  if (!ready)
    return()
  
  ### create the flexplot
  model.type = "model"
  .create_flexplot_glinmod(jaspResults, modelplot, options, dataset)
  
  return()
}

.create_flexplot_glinmod <- function(jaspResults, modelplot, options, dataset) {
  
  glinmod_results <- jaspResults[["glinmod_results"]]$object 
  generated.formula = make_flexplot_formula(options$variables, options$dependent, dataset)
  
  if	(options$ghost & length(options$variables)<4){
    ghost=rgb(1,0,0,.4)
  } else {
    ghost = NULL
  }
  
  plot = compare.fits(generated.formula, data=dataset, glinmod_results, 
                      alpha = options$alpha, jitter=c(options$jitx, options$jity),
                      ghost.line=ghost) 
  plot = theme_it(plot, options$theme) + theme(legend.position = "none")
  
  modelplot$plotObject <- plot
  
  return()
}


# create tables -----------------------------------------------------------
.create_glinmod_coefs <- function(jaspResults, options, ready) {
  glinmod_table_fixed <- createJaspTable(title = "Parameter Estimates")
  
  ### which options are required
  glinmod_table_fixed$dependOn(c("variables", "dependent", "rvariables", "ests", "interactions"))
  
  ### add citation
  glinmod_table_fixed$addCitation("Fife, D. A. (2019). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3 [Computer software].")
  
  ### build the table structure
  glinmod_table_fixed$addColumnInfo(name = "var",      title = "Variable",   type = "string", combine = TRUE)
  glinmod_table_fixed$addColumnInfo(name = "est",    title = "Estimate",       type = "number", format = "dp:2", combine = TRUE)		
  glinmod_table_fixed$addColumnInfo(name = "sterr",    title = "Standard Error",       type = "number", format = "dp:2", combine = TRUE)		
  glinmod_table_fixed$addColumnInfo(name = "z",      title = "z-statistic",    type = "number", format = "dp:2", combine = TRUE)	
  
  ### store the table structure
  jaspResults[["glinmod_table_fixed"]] <- glinmod_table_fixed
  
  family = list(
    "Normal" = "identity",
    "Logistic" = "logit",
    "Poisson" = "log",
    "Negative binomial" = "logit",
    "Gamma" = "inverse"
  )
  
  message <- paste0("Note: the default link for ", tolower(options$family), " was used (the ", family[[options$family]], " function).")
  glinmod_table_fixed$addFootnote(message)  
  
  ### return the table
  if (!ready) {
    return()
  } 
  
  ### retrieve the already-computed results
  glinmod_results <- jaspResults[["glinmod_results"]]$object
  
  ### fill the table with those results
  .fill_glinmod_table_fixed(glinmod_table_fixed, glinmod_results)
  
  return()
  
}
.fill_glinmod_table_fixed = function(glinmod_table_fixed, glinmod_results){
  
  factors = summary(glinmod_results)$coefficients
  term.labels = gsub(":", "x", row.names(factors))
  
  ### output results
  tabdat = list(
    var = term.labels,
    est = factors[,1],
    sterr = factors[,2],
    z = factors[, 3]
  )
  glinmod_table_fixed$setData(tabdat)
  
  
  return()
}







# Other stuff -------------------------------------------------------------

.glinmod_compute = function(jaspResults, dataset, options, ready) {
  
  if (ready){
    ## createJaspState allows these results to be recycled
    glinmod_results <- createJaspState()
    jaspResults[["glinmod_results"]] <- glinmod_results
    glinmod_results$dependOn(c("dependent", "variables", "interactions", "family", "ghost", "theme"))
    
    ## interactions are stored in a deeply nested list. de-listify them
    predictors = paste0(
      unlist(
        lapply(options$interactions, FUN=function(x) paste0(unlist(x$components), collapse="*"))
      ), 
      collapse=" + ")
    f = paste0(options$dependent, " ~ ", predictors, collapse = "")
    f = as.formula(f)
    
    ## set up generalIZED models
    family = list(
      "Normal" = "gaussian",
      "Logistic" = "binomial",
      "Poisson" = "poisson",
      "Negative binomial" = "quasipoisson",
      "Gamma" = "Gamma"
    )
    
    chosen.family = family[[options$family]]
    #save(chosen.family, options, file="/Users/fife/Documents/flexplot/jaspresults.rdata")
    model = glm(f, dataset, family=chosen.family)
    glinmod_results$object = model
    return()
  }
}



.check_glinmod_error = function(dataset, options){
  
  # check length of variables
  if ((options$dependent == "" & length(options$paneledVars)>0) | (options$dependent == "" & length(options$variables)>0)) .quitAnalysis("You must specify a dependent variable to view a graphic")
  if (options$dependent != "" & length(options$paneledVars)>0) .quitAnalysis("You must have at least one independent variable to do paneling")
  
}

.read_glinmod_data = function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else
    dataset = .readDataSetToEnd(columns=(c(options$dependent, options$variables))) 
    ## variable names in the dataset are encoded. de-encodify them
    names(dataset) = JASP:::.unv(names(dataset))
    return(dataset)
}
