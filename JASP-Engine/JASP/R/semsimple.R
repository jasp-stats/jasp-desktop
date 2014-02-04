
SEMSimple <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
  
  if (is.null(dataset))
  {
    if (perform == "run") {
      dataset <- read.dataset.to.end()
    } else {
      dataset <- read.dataset.header()
    }
  }
  
  
  ### RUN SEM ###
  sem_results <- try(lavaan:::sem(options$model, data = dataset))
  if (!is(sem_results,"try-error"))
  {
    # Fit Measures:
    sem_fitm <- lavaan:::fitMeasures(sem_results)  
    
    # Parameter Estimates:
    sem_parest <- lavaan:::parameterEstimates(sem_results)
    
    # ANOVA:
    sem_anova <- lavaan:::anova(sem_results)
    
    # Modification indices:
    sem_modind <- lavaan:::modificationIndices(sem_results)
    
  } else {  
    sem_fitm <- NULL
    sem_parest <- NULL
    sem_anova <- NULL
    sem_modind <- NULL
  }
  
  ### ANOVA ###
  an0va <- list()
  an0va[["title"]] <- "Chi Square Test Statistic (unscaled)"
  an0va[["cases"]] <- c("Saturated", "Model")
  an0va[["schema"]] <- list(
    fields = list(
      list(id="DF", type="number", format="dp:0"),
      list(id="AIC", type="number", format="dp:1"),
      list(id="BIC", type="number", format="dp:1"),
      list(id="Chisq", type="number", format="dp:3"),
      list(id="Chisq diff", type="number", format="dp:3"),
      list(id="Pr(>Chisq)", type="number", format="dp:3;p:0.001")
    )
  )
  an0va[["data"]] <- list()
  if (!is.null(sem_anova))
  {
    for (i in seq_len(NROW(sem_anova)))
    {
      an0va[["data"]][[i]] <- as.list(sem_anova[i,])
      an0va[["data"]][[i]][is.na(an0va[["data"]][[i]])] <- '.'
    }
  }
  

  ### PARAMETER ESTIMATES ####
  par_estimates <- list()
  par_estimates[["title"]] <- "Parameter Estimates"
  if (NROW(sem_parest) == 0)
  {
    par_estimates[["cases"]] <- NULL
  } else par_estimates[["cases"]] <- seq_len(NROW(sem_parest))
  par_estimates[["schema"]] <- list(
    fields = list(
      list(id="lhs", type="character"),
      list(id="op", type="character"),
      list(id="rhs", type="character"),
      list(id="label", type="character"),
      list(id="est", type="number", format = "dp:3"),
      list(id="se", type="number", format = "dp:3"),
      list(id="z", type="number", format = "dp:3"),
      list(id="pvalue", type="number", format = "dp:3;p:.001"),
      list(id="ci.lower", type="number", format = "dp:3"),
      list(id="ci.upper", type="number", format = "dp:3")
    )
  )
  
  par_estimates[["data"]] <- list()
  if (!is.null(sem_parest))
  {
    for (i in seq_len(NROW(sem_parest)))
    {
      par_estimates[["data"]][[i]] <- as.list(sem_parest[i,])
      par_estimates[["data"]][[i]][is.na(par_estimates[["data"]][[i]])] <- '.'
    }
  }
  
  ### MODIFICATION INDICES ###
  ### PARAMETER ESTIMATES ####
  mod_indices <- list()
  mod_indices[["title"]] <- "Modification Indices"
  if (NROW(sem_modind) == 0)
  {
    mod_indices[["cases"]] <- NULL
  } else mod_indices[["cases"]] <- seq_len(NROW(sem_modind))
  
  mod_indices[["schema"]] <- list(
    fields = list(
      list(id="lhs", type="character"),
      list(id="op", type="character"),
      list(id="rhs", type="character"),
      list(id="mi", type="number", format = "dp:3"),
      list(id="epc", type="number", format = "dp:3"),
      list(id="sepc.lv", type="number", format = "dp:3"),
      list(id="sepc.all", type="number", format = "dp:3"),
      list(id="sepc.nox", type="number", format = "dp:3")
    )
  )
  
  mod_indices[["data"]] <- list()
  if (!is.null(sem_modind))
  {
    for (i in seq_len(NROW(sem_modind)))
    {
      mod_indices[["data"]][[i]] <- as.list(sem_modind[i,])
      mod_indices[["data"]][[i]][is.na(mod_indices[["data"]][[i]])] <- '.'
    }
  }
  
  ## FIT MEASURES ###
  fit_measures <- list()
  fit_measures[["title"]] <- "Fit Measures"
  fit_measures[["cases"]] <- names(sem_fitm)
  fit_measures[["schema"]] <- list(fields = list(
    list(id="Measure", type="number", format="dp:3")))
  fit_measures[["data"]] <- lapply(as.list(unname(sem_fitm)) , function(x) {names(x) <- "Measure";x})
  
  
  ### RETURN ###
  results <- list()
  results[["an0va"]] <- an0va
  results[["par_estimates"]] <- par_estimates
  results[["fit_measures"]] <- fit_measures
  results[["mod_indices"]] <- mod_indices
  
  results
}

