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




MultilevelMetaAnalysis <- function(jaspResults, options, dataset, state=NULL) {
  #options(contrasts = list(unordered = "contr.Treatment", ordered = "contr.poly"))
  options = Filter(function(x) !all(x=="" | is.na(x)), options) # only keep set components
  do.call(multi_meta_analysis, c(list(jaspResults=jaspResults, dataset=dataset, state=state, debug=FALSE), options))
  
  return()
}

multi_meta_analysis <- 
  function(jaspResults, dataset, state, debug=FALSE,
           ES_name,                                 # effect size variable (continuous)
           SE_name,                                 # standard error of effect size (continuous, strictly positive)
           inner_grouping = "1",                    # factor that distinguishes outcomes in a multivariate model
           outer_grouping = NA,                     # factors that specify hierarchy of otherwise independent samples
           fixed_predictor_terms = "1",             # nested list with predictor terms (including potential interaction terms)
           labels_variable,
           method = c("REML", "ML"),
           inner_cov_struct = c("CS", "HCS", "ID", "DIAG", "AR", "HAR"),
           anova_table = TRUE,
           intercept = TRUE,
           coeftest_type = c('z', 't'),
           add_robust_qtest = TRUE,                 
           FE_coef_table = TRUE,
           fit_measures_table = TRUE,
           varComp_params_table = TRUE,
           FE_vcov_table = FALSE,
           funnelPlotAsymmetry = FALSE,
           ranktest_table = FALSE,
           regtest_table = FALSE,
           diagnostics_table = FALSE,
           failsafe_n_table = FALSE,
           funnel_plot = FALSE,
           forest_plot = TRUE,
           residual_dependent_plot = FALSE,
           profile_plot = FALSE,
           emmeans_table = FALSE,
           ...
  ) 
{
  ### Help functions ####
    # unfortunately, inside JASP the requirement to use namespaces causes
    # dplyr- and other operators not to exist... Hence, it is/they are define here
    `%>%` <- dplyr::`%>%`
    # `cout` makes html output and debug easier
    cout <- function(...) paste(capture.output(...), collapse = "\n")

  ### Restore Previous State? ####
    res <- if (!is.null(jaspResults[['res']])) jaspResults[['res']]$object else NULL
    #jaspResults[['bla']] <- createJaspHtml(cout(res), "pre", title='Stored object', position=1)

    
  ### Default jaspResults ####
    
    jaspResults$title = "Multivariate/Multilevel Meta-Analysis"
    
    if (anova_table && is.null(res)) {
      jaspResults[['anova_table']] = createJaspTable( title = "Analysis of Variance Table", position = 20, 
        data.frame(rowname=c("Test of Residual Heterogeneity","Omnibus test of Effects"), 
                   Q=".", df=".", p=".") %>% tibble::column_to_rownames("rowname")
      )
      jaspResults[['anova_table']]$dependOnOptions("anova_table")
    }
    

  ### Verify and standardize input ####
    
    if (missing(ES_name) || missing(SE_name)) {
      return()
    }
    ES_name = head(ES_name, 1)
    SE_name = head(SE_name, 1)
    
    if (!missing(inner_grouping)) 
      inner_grouping = as.character(substitute(inner_grouping))
    
    if (!missing(labels_variable)) 
      labels_variable = as.character(substitute(labels_variable))
    
    
  ### Verify and standardize input ####
    
    if (missing(ES_name) || missing(SE_name)) {
      return()
    }
    ES_name = head(ES_name, 1)
    SE_name = head(SE_name, 1)
    
    if (!missing(inner_grouping)) 
      inner_grouping = as.character(substitute(inner_grouping))
    
    if (!missing(labels_variable)) 
      labels_variable = as.character(substitute(labels_variable))
    
    
  ### Extract data to data frame ####
    
    if (is.null(dataset)) {
      #dataset         <- .readDataSetToEnd(columns.as.numeric=variables)
      #dataset.factors <- .readDataSetToEnd(columns=variables)

      
      dataset_pred = dataset_ESSE  = dataset_grouping = dataset_labels = NULL
      
      if (!missing(fixed_predictor_terms))
        dataset_pred <- .readDataSetToEnd(columns = unique(unlist(fixed_predictor_terms)))
      
      if (!missing(ES_name) && !missing(SE_name))
        dataset_ESSE = .readDataSetToEnd(columns.as.numeric = c(ES_name, SE_name)) 
      
      if (!missing(outer_grouping)) {
        
        if (!missing(inner_grouping))
          dataset_grouping = .readDataSetToEnd(columns.as.factor = c(inner_grouping, outer_grouping))
        else
          dataset_grouping = .readDataSetToEnd(columns.as.factor = outer_grouping)
        
      }
      
      if (!missing(labels_variable)) 
        dataset_labels = .readDataSetToEnd(columns.as.factor = labels_variable)

      #if (debug) jaspResults[['ES data']] = createJaspTable("Debug: ES/V Data", dataset_ESSE)
      #if (debug) jaspResults[['pred data']] = createJaspTable("Debug: Predictors Data", dataset_pred)
      #if (debug) jaspResults[['group data']] = createJaspTable("Debug: Grouping Data", dataset_grouping)
      #if (debug) jaspResults[['label data']] = createJaspTable("Debug: Label Data", dataset_labels)
      
      args = c(list(dataset_ESSE), list(dataset_pred), list(dataset_grouping), list(dataset_labels))
      dataset = do.call(cbind, Filter(Negate(is.null), args))
      colnames(dataset) = .unv(colnames(dataset))
      
    }
    
    if (debug) jaspResults[['raw data']] = createJaspTable("Debug: Raw Data", dataset)
    
  ### Check data ####
    
    #.hasErrors(data = dataset, type = c("observations", "infinity"))
    dataErrors <- 
      c(
        verify_that(dataset[ES_name], is.numeric, is.finite, not.constant),
        verify_that(dataset[SE_name], is.numeric, is.finite, is.positive)
      )
    
    
    
    if (length(dataErrors) > 0) {
      
      message = '<h1 style="background:#FFCCCCff;color:darkred;padding:1em;font-family:monospace" class="warning">Conditions not met: <ul><li> bla </ul></h1>'
      message = gsub("bla", dataErrors %>% paste(names(.), ., collapse="</li>\n<li>"), message)
      jaspResults[["error_message"]] <- createJaspHtml(message, 
            elementType = "div",title = "Data problems", position = 5)
      return()
      
    }
    

  ### Build model ####
    
    if (is.null(res)) {
      
      # construct model formulas
      
      fixed_formula = as.formula(paste(ES_name, "~", paste(lapply(lapply(fixed_predictor_terms,unlist), paste, collapse = ":"), collapse = " + "))) # e.g.: ES ~ x1 + x2 + x2:x3
      random_formula = as.formula(paste(" ~ ", inner_grouping, " | ", paste(outer_grouping, collapse = "/"))) # e.g.: ~ test_type | district / school
      
      if (debug) jaspResults[['predictor_terms']] = createJaspHtml(paste(capture.output(fixed_predictor_terms), collapse="\n"), "pre", title="Predictor terms")
      
      # construct fitting call
      
      call = quote(metafor::rma.mv(yi = yi ~ 1, V = vi, data = dataset))
      
      call$yi = fixed_formula
      call$V = as.name(SE_name)
      
      if (! missing(outer_grouping))    call$random     = random_formula
      if (! missing(labels_variable))   call$slab       = as.name(labels_variable)
      if (! missing(method))            call$method     = method[1]
      if (! missing(inner_cov_struct))  call$struct     = inner_cov_struct[1]
      if (! missing(intercept))         call$intercept  = intercept
      if (! missing(coeftest_type))     call$test       = coeftest_type[1]
      
      # do call fitting call
      
      res <- try(eval(call))
      
      #jaspResults[['output']] = createJaspHtml(paste(c(capture.output(print(call)), capture.output(res)), collapse = "\n"), "pre", title = "Elementary output", position=10)
      
      if (inherits(res, "try-error")) return()
      
      # store the result as a state
      jaspResults[['res']] <- 
        createJaspState(object = res, title = "Meta-analysis fit object",  dependencies = 
              c("ES_name","SE_name","fixed_predictor_terms","outer_grouping","inner_cov_struct", "method",
                "intercept","coeftest_type","labels_variable")
          )
      
      
      if (debug) jaspResults[['call']] = createJaspHtml(paste(capture.output(call), collapse = "\n"), "pre", title="Debug: Call")
      if (debug) jaspResults[['vars']] = createJaspHtml(paste(capture.output({cat("labels: ", labels_variable)}), collapse = "\n"), "pre", title="Debug: Call")
      
    } 

    
    
  ### Set Output Title ####
    jaspResults$title <- capture.output(summary(res))[2]
    
    
   
  ### Prepare Q-tests table output ####

    if (anova_table) {
      
      jaspResults[['anova_table']] <- 
        createJaspTable( title = "Analysis of Variance Table", position = 20, 
          (function(object, add_robust = FALSE, ...){
            
            # Omnibus
            qstat  <- unlist(object[c('QE','QM')])
            df <- c(object$k - object$p, object$m)
            pval <- unlist(object[c('QEp','QMp')])
            name <- c("Test of Residual Heterogeneity","Omnibus test of Fixed Effects")
            qtable <- data.frame(` ` = name, `Q` = qstat, `df` = df, `p`= pval, check.names = FALSE)
            rownames(qtable) = name
            qtable <- qtable[1, , drop=FALSE]
            
            if (add_robust && !is.null(object$mf.g)) {
              rob = metafor::robust(object, object$mf.g$outer)
              
              qstat  <- unlist(rob[c('QE','QM')])
              df <- c(rob$k - rob$p, rob$m)
              pval <- unlist(rob[c('QEp','QMp')])
              name <- c("Robust Test of Residual Heterogeneity","Robust Omnibus test of Fixed Effects")
              qtabler <- data.frame(` ` = name, `Q` = qstat, `df` = df, `p`= pval, check.names = FALSE)
              rownames(qtabler) = name
              qtable <- rbind(qtable, qtabler[-1,, drop = FALSE])
            }
            
            
            cleanUpColNames <- . %>%
              stringr::str_replace_all("Pr\\(.+","p") %>%
              stringr::str_replace_all("Chisq value", "Q") %>%
              stringr::str_replace_all("Df","df")
              
            anova_df = ANOVA.rma(res, data=dataset) 
            heading = attr(anova_df,"heading")
            anova_df = anova_df %>% tibble::rownames_to_column() %>% dplyr::rename(` `=rowname)
            colnames(anova_df) = colnames(anova_df) %>% cleanUpColNames 
            
            structure(rbind(anova_df[,c(1,3,2,4)], qtable), heading = heading)

          })(res, add_robust = FALSE) -> anova_df
        )
      
      jaspResults[['anova_table']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['anova_table']]$dependOnOptions("anova_table")
      
      jaspResults[['anova_table']]$setColTypes(list(` ` = "string", `Q` = "number", `df` = "number", `p` = "number"))
      jaspResults[['anova_table']]$setColFormats(list(` ` = "", `Q` = "sf:4", `df` = "sf:2", `p` = "dp:4"))
      jaspResults[['anova_table']]$addFootnote(message = attr(anova_df,"heading")[2], symbol = "", colNames="Q")
      
  }

  ### Prepare Coefficients Table Output ####

    if (FE_coef_table && is.null(jaspResults[['coef']])) {
      
      jaspResults[['coef']] = createJaspTable("Fixed Effects Coefficients", position = 30,
         (function(object, ...){
           test = if (exists('test')) test else 'z'
           coefdf = coef(summary(object))
           colnames(coefdf) = c('Estimate', 'Standard Error', test, 'p', 'Lower Bound', 'Upper Bound')
           rownames(coefdf) = gsub("intrcpt", "(Intercept)", rownames(coefdf))
           coefdf %>% tibble::rownames_to_column() %>% dplyr::rename(` ` = rowname)
         })(res))
      
      jaspResults[['coef']]$setColFormats(c("", "sf:5", "sf:5", "sf:5", "dp:5","sf:5","sf:5"))
      jaspResults[['coef']]$setColTypes(c("string", rep("number", 6)))
      
      jaspResults[['coef']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['coef']]$dependOnOptions("FE_coef_table")
      
      # Add footnotes to the analysis result
      
      jaspResults[['coef']]$addFootnote(message = "Wald test.", colNames=if (exists('test')) test else 'z')
      
      # Add citation reference list
      
      jaspResults[['coef']]$addCitation("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of
      Statistical Software, 36(3), 1-48. URL: http://www.jstatsoft.org/v36/i03/")
      
      
    }
    
    

  ### Prepare Model fit table output ####
    
    #jaspResults[['fitmeasures']] <- NULL
    if (fit_measures_table && is.null(jaspResults[['fitmeasures']])) {
      clearUpNames <- . %>%
        gsub("logLik","log Likelihood", .) %>%
        gsub(":","", .)

      jaspResults[['fitmeasures']] <- 
        createJaspTable("Fit measures", position = 40,
           (function(object, ...) {
             fitdf = metafor::fitstats(res) %>% as.data.frame 
             rownames(fitdf) = rownames(fitdf) %>% clearUpNames
             fitdf %>% tibble::rownames_to_column() %>% dplyr::rename(` `=rowname)
           })(res)
        )

      jaspResults[['fitmeasures']]$setColFormats(c("", "sf:5"))
      jaspResults[['fitmeasures']]$setColTypes(c("string", "number"))
      
      jaspResults[['fitmeasures']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['fitmeasures']]$dependOnOptions("fit_measures_table")
      
    } 
    
    
    
  ### Prepare Residuals Variance Components Parameters ####
    
    if (debug) jaspResults[['extract']] <-  createJaspHtml(title="myRMAprint(res)", text = paste(capture.output(myRMAprint(res)), collapse="\n"), elementType = "pre")
    
    #jaspResults[['variance_components']] <- NULL
    if (varComp_params_table && is.null(jaspResults[['variance_components']])) {
      
      jaspResults[['variance_components']] <- 
          (function(varComp){
            
            vcTableNamesCleanUp <-   . %>%
              gsub("tau", "&tau;", .) %>%
              gsub("sigma", "&sigma;", .) %>%
              gsub("rho", "&rho;", .) %>%
              gsub("phi", "&phi;", .) %>%
              gsub("gamma", "&gamma;", .) %>%
              gsub("\\^(\\d+)", "<sup>\\1</sup>", .) %>%
              gsub("estim", "Estimate", .) %>%
              gsub("sqrt", "&radic; Estimate", .) %>%
              gsub("k.lvl|nlvls", "Number of levels", .) %>%
              gsub("fixed", "Fixed", .) %>%
              gsub("level", "Level",.) %>%
              gsub("X(.+)", "\\1", .) %>%
              gsub("\\.(\\S+)", "<sub>\\1</sub>", .) %>%
              gsub("factor", "Factor", .)
          
            vctmp = varComp
            tmp = createJaspContainer("Variance Components")
            tmp[['structure']] = createJaspHtml(text = 
                paste("<ul style=list-style:initial;>", 
                      paste("<li>", 
                            c(
                              vctmp$structure$grouping, 
                              paste("Inner covariance structure:", paste(vctmp$structure$inner.cov.struct, collapse = ", "))
                            ),
                            collapse = "</li>"),
                      "</li>
                      </ul>"), 
                title="Covariance Model") 
            for (nm in names(vctmp)[-1]) {
              print(nm)
              tmpTable = vctmp[[nm]] %>% dplyr::select(-dplyr::matches("^X$"))
              names(tmpTable) = names(tmpTable) %>% vcTableNamesCleanUp
              rownames(tmpTable) = rownames(tmpTable) %>% vcTableNamesCleanUp 
              tmpTable = tmpTable %>% tibble::rownames_to_column() %>% dplyr::rename(` `=rowname)
              tmp[[nm]] = createJaspTable(nm, data = tmpTable)
              tmp[[nm]]$setColFormats(c("", rep("sf:5", ncol(tmpTable))))
              tmp[[nm]]$setColTypes(c("string", rep("number", ncol(tmpTable)), `Number of Levels`="integer", Level="string"))
            }
            tmp
          })(extractRMAVarComp(res)) 
      
      
      jaspResults[['variance_components']]$position = 50
      
      jaspResults[['variance_components']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['variance_components']]$dependOnOptions("varComp_params_table")
     
      
    }

      

  ### Prepare Coefficient Covariance Matrix ####

    #jaspResults[['FixedEffectsParameterCovariance']] <- NULL
    if (FE_vcov_table && is.null(jaspResults[['FixedEffectsParameterCovariance']])) {
      
      jaspResults[['FixedEffectsParameterCovariance']] <- 
        createJaspTable("Fixed Effects Estimates Covariance Matrix", position= 60,
          (function(object, ...){
            
            vcovTableNamesCleanUp <- . %>%
              stringr::str_replace_all("intrcpt", "Intercept") %>%
              stringr::str_replace_all("\\[", " [") 
              
            V = vcov(object) 
            rownames(V) = rownames(V) %>% vcovTableNamesCleanUp
            colnames(V) = colnames(V) %>% vcovTableNamesCleanUp
            
            V %>% as.data.frame() %>% tibble::rownames_to_column() %>% dplyr::rename(` `=rowname)
          })(res) -> vcov_df 
        ) #%>% jasp_as_html()

      jaspResults[['FixedEffectsParameterCovariance']]$setColFormats(c("", rep("sf:5", nrow(vcov_df))))
      jaspResults[['FixedEffectsParameterCovariance']]$setColTypes(c("string", rep("number", nrow(vcov_df))))

      jaspResults[['FixedEffectsParameterCovariance']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['FixedEffectsParameterCovariance']]$dependOnOptions("FE_vcov_table")
     
    }
    

  ### Prepare Tests for Funnel plot asymmetry ####
    
    #jaspResults[['funnel_asymmetry_test']] <- NULL
    if (funnelPlotAsymmetry && is.null(jaspResults[['funnel_asymmetry_test']])) {
      
      
      jaspResults[['funnel_asymmetry_test']] <- 
        createJaspTable("Tests for Funnel Plot Asymmetry", position=70,
          (function(object, ...) {
            
            df = data.frame('Statistic' = numeric(), 'p' = numeric())
            
            if (ranktest_table) {
              # Prepare Kendall's Rank Correlation Test for testing Funnel plot asymmetry
              kendall = metafor::ranktest(object)
              df = rbind(df, data.frame('Statistic' = kendall$tau, 'p' = kendall$p))
              rownames(df)[1] = "Kendall's tau"
            }
            
            if (regtest_table && FALSE) {
            # Prepare Egger's test for Funnel plot asymmetry ('test for publication bias')
              egger = purrr::safely(metafor::regtest)(res)
              df = rbind(df, data.frame('Statistic' = NA, 'p' = NA))
              rownames(df)[2] = "Egger's regression test (<em>z</em>)"
            }
            
            df %>% tibble::rownames_to_column() %>% dplyr::rename(` `=rowname)
          })(res)
        ) #%>% jasp_as_html()
      
      jaspResults[['funnel_asymmetry_test']]$setColFormats(c("", "sf:5", "dp:5"))
      jaspResults[['funnel_asymmetry_test']]$setColTypes(c("string", "number", "number"))

      jaspResults[['funnel_asymmetry_test']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['funnel_asymmetry_test']]$dependOnOptions("funnelPlotAsymmetry")
      
      if (regtest_table)
        jaspResults[['funnel_asymmetry_test']]$addFootnote(message = "Egger's test is not yet implemented.", symbol = "(!)", colNames="")
      
      
    }
    
    
    

  ### Prepare Casewise diagnostics table ####
    
    # jaspResults[['diagnostics']] <- NULL
    if (diagnostics_table && is.null(jaspResults[['diagnostics']])) {
      
      jaspResults[['diagnostics']] <- 
        createJaspTable("Case Wise Influence Diagnostics", position = 80, 
          (function(object, ...) {
            
            #source("~/Desktop/Arnoud Multilevel Meta-analysis/quick.influence.R")
            
            qiTableNamesCleanUp <- . %>%
              stringr::str_to_title() %>%
              stringr::str_replace_all(".+pval", "p") %>%
              stringr::str_replace("Hatvalues","Hat Values") %>%
              paste(., Reduce(paste0, purrr::rep_along(., " "), accumulate = TRUE), sep="") # add increasing number of spaces to fool createJaspTable
            
            qi = quick.influence(object)
            colnames(qi) = colnames(quick.influence(res)) %>% qiTableNamesCleanUp 
            rownames(qi) = paste(rownames(qi), ifelse(rowSums(qi[,c(2,4,6)] < .05) > 0, "*", ""))
            
            qi %>% tibble::rownames_to_column() %>% dplyr::rename(` `=rowname)
          })(res)
        ) #%>% jasp_as_html()
      
      jaspResults[['diagnostics']]$setColFormats(c("", "sf:5", "dp:5", "sf:5", "dp:5", "sf:5", "dp:5"))
      jaspResults[['diagnostics']]$setColTypes(c("string", "number", "number", "number", "number", "number", "number"))
      jaspResults[['diagnostics']]$dependOnOptions("diagnostics_table")
      
      jaspResults[['diagnostics']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['diagnostics']]$dependOnOptions("diagnostics_table")
      
      # Markup influential cases in a footnote
      
      jaspResults[['diagnostics']]$addFootnote(message = "Potentially influential.", symbol = "*", colNames="")

  }
    
  ### Prepare Fail-safe N diagnostics table ####

    #jaspResults[['fsn']] <- NULL
    if (failsafe_n_table) {
      
      jaspResults[['fsn']] <- 
        createJaspTable("Fail-safe N", position = 90, 
          (function(object, ...) {
            # we want to call fsn(yi = yi, vi = vi, data = data) with the right arguments:
            rmacall= object$call
            call = quote(metafor::fsn(yi = yi ~ 1, vi = vi, data = dataset))
            call$yi = as.name(all.vars(rmacall$yi)[1]) # deals with both single variable names as well as formulas
            call$vi = as.name(all.vars(rmacall$V)[1]) # deals with both single variable names as well as formulas
            FSN = eval(call)
            df = data.frame(`Fail-safe N` = FSN$fsnum, `Target Significance Level` = FSN$alph, p=FSN$p, check.names = FALSE)
            row.names(df) = paste(FSN$type, "method")
            df
          })(res)
        ) #%>% jasp_as_html()

      jaspResults[['fsn']]$setColFormats(c("", "", "sf:2", "sf:5"))
      jaspResults[['fsn']]$setColTypes(c("integer", "number", "number"))
      
      jaspResults[['fsn']]$addFootnote(message = "Fail-safe-N procedures are controversial.")
      jaspResults[['fsn']]$addFootnote(message = "Rosenthal's method.")
   
      jaspResults[['fsn']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['fsn']]$dependOnOptions("failsafe_n_table")
         
    }
    
    
    
    
  ### Prepare Plot Output ####

    if (is.null(jaspResults[['plots']])) {
      jaspResults[['plots']] <- createJaspContainer("Plots", position = 100)
      jaspResults[['plots']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['plots']]$dependOnOptions(c("forest_plot","funnel_plot","residual_dependent_plot","profile_plot"))
    }
    jaspResultsPlots <- jaspResults[['plots']]
    
  # plot forest plot

    if (forest_plot && is.null(jaspResultsPlots[['forest_plot']])) {
      jaspResultsPlots[['forest_plot']] <-  
        createJaspPlot(function() metafor::forest(res), title = "Forest plot", height=16*nrow(dataset), width=500)
      
      jaspResultsPlots[['forest_plot']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResultsPlots[['forest_plot']]$dependOnOptions("forest_plot")
    }
    
  # plot funnel plot
    
    if (funnel_plot && is.null(jaspResultsPlots[['plot_funnel']])) {
      jaspResultsPlots[['plot_funnel']]  <- 
        createJaspPlot(function() metafor::funnel(res), title = "Funnel plot", aspectRatio = 1 )

      jaspResultsPlots[['plot_funnel']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResultsPlots[['plot_funnel']]$dependOnOptions("funnel_plot")
    }
    
  # plot residuals vs dependent diagnostic plot
    
    #jaspResultsPlots[['plot_residual_dependent']]  <- NULL
    if (residual_dependent_plot && is.null(jaspResultsPlots[['plot_residual_dependent']] )) {
      
      jaspResultsPlots[['plot_residual_dependent']]  <- 
        createJaspPlot(title = "Fitted vs Dependent and Residual vs Dependent", plot = 
          data.frame(Dependent = res$yi, Fitted = fitted(res), Residual = residuals(res)) %>%
          tidyr::gather(what, value, -Dependent) %>%
            ggplot2::ggplot(ggplot2::aes(x = Dependent, y = value)) + ggplot2::geom_point(alpha=.5) + ggplot2::geom_smooth() + 
            ggplot2::labs(title = "") + ggplot2::facet_wrap(~what, scales="free")
        ) 
      
      jaspResultsPlots[['plot_residual_dependent']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResultsPlots[['plot_residual_dependent']]$dependOnOptions("residual_dependent_plot")
      
    }

  # profile plot: diagnostic plot for tau parameter
    
    #jaspResultsPlots[['plot_profile']]  <-  NULL
    if (profile_plot && !is.null(jaspResults[['plots']])) {
      jaspResultsPlots[['plot_profile']]  <-  
        createJaspPlot(function() profile(res), title = "Profile plot for Random Effects parameters") 
      
      jaspResultsPlots[['plot_profile']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResultsPlots[['plot_profile']]$dependOnOptions("profile_plot")
      
    }
    

    
  ### Prepare Fixed Effects EMMEANS ####

    if (debug) jaspResults[['conditionals']] = createJaspHtml(text = paste("\n<b>factors in model</b>: ", paste(names(Filter(Negate(is.numeric), dataset)), collapse=", "), "\n\n<b>variable names</b>: ", paste(colnames(dataset), collapse = ", ")), elementType="pre", title = "Debug: Factors in the model")
    
    #jaspResults[['emmeans']] = NULL
    if (emmeans_table) {
      
      jaspResults[['emmeans']] = createJaspTable("Estimated Marginal Means", position = 45, 
        data = (function(object, ...){
          cout <- function(...) paste(capture.output(...), collapse = "\n")
          cleanUpNames = . %>% 
            stringr::str_replace("emmean", "Marginal Mean") %>%
            stringr::str_replace("SE", "Standard Error") %>%
            stringr::str_replace("lower.CL", "Lower Bound") %>%
            stringr::str_replace("upper.CL", "Upper Bound") 
          
          conditionals = intersect(all.vars(object$call$yi), names(Filter(Negate(is.numeric), dataset))) # names(dataset %>% dplyr::select_if(is.factor))
          ret = data.frame(` `=c(), `Marginal Means`=c(), `Standard Error`=c(), `Lower Bound`=c(), `Upper Bound`=c(), check.names = FALSE)
          if (!is.null(conditionals) && length(conditionals) > 0) {
            myformula = as.formula(paste("~1+",paste(conditionals, collapse="+")))
            #jaspResults[['emmeans_formula']] = createJaspHtml(cout({print(myformula); dataset}),"pre",title="EMMEANS formula")
            ret = try(emmeans.rma(object, myformula, dataset))
            if (!inherits(ret, "try-error"))
              colnames(ret) = colnames(ret) %>% cleanUpNames
            else
              jaspResults[['emmeans_results']] = createJaspHtml(cout({ret}),"pre",title="EMMEANS results")
          }
          ret
        })(res)
      )
      
      jaspResults[['emmeans']]$setColFormats(c("", "sf:5", "sf:5", "sf:5", "sf:5", "sf:5"))
      jaspResults[['emmeans']]$setColTypes(c("string", "number", "number", "number", "number"))
      jaspResults[['emmeans']]$copyDependenciesFromJaspObject(jaspResults[['res']])
      jaspResults[['emmeans']]$dependOnOptions("emmeans_table")

    }

  ### Prepare Fixed Effects Contrasts ####
}
  





{ # utility functions
  verify_that <- function(x, ...) {
    funs = list(...)
    if (length(funs) > 0) {
      
      names(funs) = setdiff(all.vars(match.call()), all.vars(match.call()$x))
      tests = rapply(funs, function(fun) sapply(x, fun), classes = "function", how = 'replace')
      Filter(length, lapply(tests, function(x) names(x)[!x]))
    }
  }
  not.constant <- function(x) stats::var(as.numeric(x)) > 0
  all.finite <- function(x) all(base::is.finite(x))
  is.positive <- function(x) all(x > 0)
  is.negative <- function(x) all(x < 0)
  non.positive <- function(x) all(x <= 0)
  non.negative <- function(x) all(x >= 0)
}

{ # Functions for extracting variance components
  reinterpret.data.frame <- function(object) {
    zz <- textConnection("foo", "w"); on.exit(close(zz))
    write.table(object, sep=",",file = zz)
    close(zz)
    zz = textConnection(foo,"r")
    bok <- read.csv(zz)
    bok
  }
  
 # `%>%` <- magrittr::`%>%`
 # myRMAprint <- deparse(metafor::print.rma.mv) %>%
 #   gsub("\\.print\\.out", "metafor:::.print.out", .) %>%
 #   gsub("invisible\\(\\)", "invisible(list(fit_stats = if(exists('fs')) fs else NA, variance_components= if(!exists('VC')) list() else VC, res_table=res.table))", .) %>%
 #   gsub("\\.pval\\(", "format(", .) %>%
 #   gsub("\\.format\\.btt\\(", "I(", .) %>%
 #   gsub("print\\(vc", "my_counter <- if(!exists('my_counter')){ 1 }else{ my_counter+1}; if(!exists('VC')) VC = list(); VC[[my_counter]] = print(vc", .) %>%
 #   parse(text = .) %>%
 #   eval()

  extractRMAVarComp <- function(object, ...) {
    text_output = capture.output(outp <- myRMAprint(object, digits=18))
    varComp <- rapply(outp, reinterpret.data.frame, classes = "matrix", how="replace")$variance_components

    struct = list(inner.cov.struct = object$struct, grouping = Filter(function(x) grepl("inner|outer",x), text_output))
    structure(c(list(structure = struct), `Estimates ` = varComp), text_output= text_output)
  }
}



{ ##### Help functions to produce anova tables for 'rma.mv' objects and do automatic model selection #####
  
  ## Fix a the metafor limitation that you cannot use step()
  RMA.mv = function(formula, ...) {
      call = rmacall = match.call(); 
      rmacall[[1]] = as.name("rma.mv"); 
      rmacall$mods = formula; 
      rmacall$formula = NULL; 
      fit = eval(rmacall); 
      fit$rmacall = fit$call
      fit$call = call; 
      fit$terms = terms(formula); 
      fit$formula = formula;
      class(fit) = c("RMA_mv", class(fit))
      fit
  }
  extractAIC.RMA_mv = function(object, scale, k = 2, ...) {
      class(object) = class(object)[-1]
      c(sum(hatvalues(object)), AIC(object))
  }
  update.RMA_mv = function(object, formula., ..., evaluate = TRUE) {
      update.default(object, formula., ..., evaluate = evaluate)
  }
  
  ANOVA.rma <- function(object, data = object$model) {
    formula <- as.formula(object$call[[2]])
    # in stats:::anova.lm 'asgn' is extracted as: 
    #    p1=1:object$rank; object$assign[qr.lm(object)$pivot][p1]
    asgn <- attr(model.matrix(formula, data = data), "assign") # coefficient group indicators
    eff <- split(1:sum(!object$coef.na), asgn[!object$coef.na])
    names(eff) <- terms(as.formula(object$call[[2]])) %>% labels %>% c("(Intercept)", .)
    as_anova_row = function(x) {
      if (x$test == 'z') 
        data.frame(Df=x$m, `Chisq value` = x$QM, `Pr(>X^2)` = x$QMp, check.names = FALSE)
      else
        data.frame(Df1 = x$m, Df2 = x$dfs, `F value` = x$QM, `Pr(>F)` = x$QMp, check.names = FALSE)
    }
    lapply(eff, function(btt) anova(object, btt = btt) %>% as_anova_row) %>% 
      do.call(rbind, .) %>%
      structure(heading = c("Analysis of Variance Table\n", paste("Response:", deparse(formula[[2L]]))),
                class = c("anova", "data.frame"))
  }
  
  ANOVA <- function(object, ...) UseMethod("ANOVA")
  get_rank <- function(object, ...) UseMethod("get_rank")
  get_rank.default <- function(object, ...) object$rank
  get_rank.rma <- function(object, ...) sum(!object$coef.na)
  get_assign <- function(object, ...) UseMethod("get_assign")
  get_assign.default <- function(object, ...) {p1=1:object$rank; object$assign[qr.lm(object)$pivot][p1]}
  get_assign.rma <- function(object, ...) {
    formula <- formula(object)
    asgn <- attr(model.matrix(formula, ...), "assign") # coefficient group indicators
    asgn[!object$coef.na]
  }
  terms.rma <- function(object, ...) terms(formula(object)) 
  formula.rma <- function(object, ...) as.formula(object$call$yi)
  
  ANOVA.mipo <- function(object, mids.object, ...) { 
    # ANOVA tables for mice's mipo object returned by pool()
    ffit <- mids.object
    object1 = ffit$analyses[[1]]
    p1 <- 1:get_rank(object1, ...)
    asgn <- get_assign(object1, ...)
    eff <- split(p1, asgn)
    names(eff) <- terms(ffit$analyses[[1]]) %>% labels %>% c("(Intercept)", .)
    as_anova_row <- function(i) {
      #Df1 = length(i)
      #Df2 = mean(object$df[i]) # does this make sense at all???
      #F_value = drop(b %*% Sigma.b.inv %*% b) * Df1 / Df2
      #data.frame(Df1 = Df1, Df2 = Df2, `F value` = F_value, 
      #           `Pr(>F)` = 1-pf(F_value, Df1, Df2), check.names = FALSE)
      res <- wald.test(object, i)
      data.frame(Df1 = res$parameter['Df1'], Df2 = res$parameter['Df2'], `F value` = res$statistic, 
                 `Pr(>F)` = res$p.value, check.names = FALSE)
    }
    lapply(eff, as_anova_row) %>%
      do.call(rbind, .) %>%
      structure(heading = c("Analysis of Variance Table\n", paste("Response:", deparse(formula(object1)[[2]]))), class = c('anova', 'data.frame'))
  }
  wald.test <- function(object, ...) UseMethod("wald.test")
  wald.test.mipo <- function(object, i, useUbar=FALSE) {
    Df1 <- length(i)
    k <- Df1
    m <- object$m
    Qbar <- object$qbar[i]
    Q0 <- structure(rep(0,k), .Names = names(Qbar))
    r1 <- (1 + 1/m) * sum(diag(object$b[i,i] %*% solve(object$ubar[i,i]))) / k
    V <- if (useUbar) object$ubar[i,i] else object$t[i,i]
    w1 <- (1 + r1)^(-1) * t(Q0 -Qbar) %*% solve(V) %*% (Q0 - Qbar) / k
    a <- k * m
    v1 <- if (a > 4) 4 + (a-4)*(1 + (1-2/a)/r1)^2 else 0.5*a*(1+a/r1)^2
    Df2 <- v1
    structure(list(statistic = c(`F`=w1), parameter = c(`Df1`=Df1,`Df2`=Df2), p.value = 1-pf(w1,Df1,Df2),
                   estimate = Qbar, null.value = Q0, alternative = "Not all coefficients equal their null value", 
                   method=sprintf("Wald test on coefficients (using the %s variance)", if (useUbar) 
                     "Within" else "Total"), data.name = deparse(substitute(object))), class="htest")
  }
  
  
  contr.simple <- function(n, contrasts = TRUE, sparse = FALSE) 
  { # define a simple contrasts function (compares each level to the reference level)
      t(MASS::ginv(contr.sum(n, contrasts, sparse)))
  }
  
}
