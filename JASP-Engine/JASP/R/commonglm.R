# Backend for GLM models. Poor man's object-oriented approach
.jaspGlm <- function(dataset, options, perform, type) {
  glmRes <- NULL
  if (perform == "run" && options[["dependent"]] != "") {
    if (!is.null(type) && type == "binomial") {
      # Logistic regression
      f <- .createGlmFormula(options)
      n <- .createNullFormula(options)
      names(dataset) <- .unv(names(dataset))
      if (!is.null(n)) {
        nullModel <- stats::glm(n[["plaintext"]], data = dataset, 
                                family = "binomial")
        glmRes <- stats::glm(f[["plaintext"]], data = dataset, 
                             family = "binomial")
        glmRes[["null.deviance"]] <- nullModel[["deviance"]]
        glmRes[["df.null"]] <- nullModel[["df.residual"]]
        glmRes[["nullModel"]] <- nullModel
      } else {
        glmRes <- stats::glm(f[["plaintext"]], data = dataset, 
                             family = "binomial")
      }
      
    } else {
      .quitAnalysis("GLM type not supported")
    }
  }
  return(glmRes)
}

.createGlmFormula <- function(options) {
  # this function takes options as its inputs and outputs a formula in base64
  f <- NULL
  f.base64 <- NULL

  dependent <- options[["dependent"]]
  if (dependent == "") {
    f <- f.base64 <- 0~1 # mock formula, always works
  }

  modelTerms <- options[["modelTerms"]]
  includeIntercept <- options[["includeIntercept"]]
  if (length(modelTerms) == 0) {
    if (includeIntercept) {
      f <- formula(paste(dependent, "~ 1"))
      f.base64 <- formula(paste(.v(dependent), "~ 1"))
    } else {
      f <- formula(paste(dependent, "~ 0"))
      f.base64 <- formula(paste(.v(dependent), "~ 0"))
    }
  } else {
    if (includeIntercept) {
      t <- t.base64 <- character(0)
    } else {
      t <- t.base64 <- "0"
    }
    for (i in seq_along(modelTerms)) {
      term <- modelTerms[[i]][["components"]]
      if (length(term) == 1) {
        t <- c(t, term)
        t.base64 <- c(t.base64, .v(term))
      } else {
        t <- c(t, paste(unlist(term), collapse = ":"))
        t.base64 <- c(t.base64, paste(.v(unlist(term)), collapse = ":"))
      }
    }
    f <- formula(paste(dependent, "~", paste(t, collapse = "+")))
    f.base64 <- formula(paste0(.v(dependent), "~", paste(t.base64, collapse = "+")))
  }
  list(plaintext = f, base64 = f.base64)
}

.createNullFormula <- function(options) {
  # this function takes options as its inputs and outputs a formula in base64
  f <- NULL
  f.base64 <- NULL
  nuisanceTerms <- NULL
  
  dependent <- options[["dependent"]]
  if (dependent == "") {
    return(NULL)
  }

  modelTerms <- options[["modelTerms"]]  
  includeIntercept <- options[["includeIntercept"]]
  if (length(modelTerms) == 0) {
    return(NULL)
  } else {
    t <- t.base64 <- character(0)
    for (i in seq_along(modelTerms)) {
      nui <- modelTerms[[i]][["isNuisance"]]
      if (!is.null(nui) && nui) {
        term <- modelTerms[[i]][["components"]]
        if (length(term) == 1) {
          t <- c(t, term)
          t.base64 <- c(t.base64, .v(term))
        } else {
          t <- c(t, paste(unlist(term), collapse = ":"))
          t.base64 <- c(t.base64, paste(.v(unlist(term)), collapse = ":"))
        }
      }
    }
    
    if (length(t) == 0) {
      return(NULL)
    }
    
    nuisanceTerms <- t
    
    if (!includeIntercept) {
      t <- c(t, "0") 
      t.base64 <- c(t.base64, "0")
    }
    
    f <- formula(paste(dependent, "~", paste(t, collapse = "+")))
    f.base64 <- formula(paste0(.v(dependent), "~", paste(t.base64, collapse = "+")))
  }
  list(plaintext = f, base64 = f.base64, terms = nuisanceTerms)
}

.glmModelSummary <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial") {
    
    out[["title"]] <- "Model summary"
    
    fields <- list(
      list(name="mod", title="Model", type="string"),
      list(name="dev", title="Deviance", type="number", format="sf:4;dp:3"),
      list(name="aic", title="AIC", type="number", format="dp:3"),
      list(name="bic", title="BIC", type="number", format="dp:3"),
      list(name="dof", title="df", type="integer"),
      list(name="chi", title="\u03A7\u00B2", type="number", format="dp:3"),
      list(name="pvl", title="p", type="number", format="dp:3;p:.001"),
      list(name="fad", title="McFadden R²", type="number", format="sf:4;dp:3"),
      list(name="nag", title="Nagelkerke R²", type="number", format="sf:4;dp:3"),
      list(name="tju", title="Tjur R²", type="number", format="sf:4;dp:3")
    )
    
    out[["schema"]] <- list(fields=fields)
    
    if (perform == "run" && !is.null(glmObj)) {
      lr <- .lrtest(glmObj)
      nuisance <- glmObj[["nullModel"]]
      if (!is.null(nuisance)) {
        terms <- rownames(summary(nuisance)[["coefficients"]])
        terms <- sapply(terms[terms!="(Intercept)"], .formatTerm, glmObj=nuisance)
        footnotes <- .newFootnotes()
        msg <- paste0("Null model contains nuisance parameters: ", 
                      paste(terms, collapse = ", "))
        .addFootnote(footnotes, symbol="<em>Note.</em>", text = msg)
        out[["footnotes"]] <- as.list(footnotes)
      }
      
      rows <- list(
        list(mod = "H\u2080", 
             dev = .clean(glmObj[["null.deviance"]]),
             aic = .clean(.aicNull(glmObj)),
             bic = .clean(.bicNull(glmObj)),
             dof = .clean(glmObj[["df.null"]]),
             chi = .clean(NULL),
             pvl = .clean(NULL),
             fad = .clean(NULL),
             nag = .clean(NULL),
             tju = .clean(NULL)),
        list(mod = "H\u2081", 
             dev = .clean(glmObj[["deviance"]]),
             aic = .clean(glmObj[["aic"]]),
             bic = .clean(.bic(glmObj)),
             dof = .clean(glmObj[["df.residual"]]),
             chi = .clean(lr[["stat"]]),
             pvl = .clean(lr[["pval"]]),
             fad = .clean(.mcFadden(glmObj)),
             nag = .clean(.nagelkerke(glmObj)),
             tju = .clean(.tjur(glmObj)))
      )
      
    } else {
      rows <- list(
        list(mod = "0", dev = ".", fad = .clean(NULL), nag = .clean(NULL), 
             tju = .clean(NULL), aic = "."),
        list(mod = "1", dev = ".", fad = ".", nag = ".", tju = ".", aic = ".")
      )
    }
    
    out[["data"]] <- rows
  } 
  
  return(out)
}

.glmEstimatesTable <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial" && options[["coeffEstimates"]]) {
    
    out[["title"]] <- "Coefficients"
    
    if (options[["coeffCIOR"]]) {
      ciTitle <- paste0(options[["coeffCIInterval"]], "% Confidence interval <br> (odds ratio scale)")
    } else {
      ciTitle <- paste0(options[["coeffCIInterval"]], "% Confidence interval")
    }
    
    if (options[["robustSEOpt"]]) {
      seTitle <- "Robust <br> Standard Error"
    } else {
      seTitle <- "Standard Error"
    }
    # first define all the fields
    fields <- list(
      list(name="param", title = "", type="string"),
      list(name="est", title = "Estimate", type="number", format="dp:3"),
      list(name="se", title = seTitle, type="number", format="dp:3"),
      list(name="std", title = "Standardized\u207A", type="number", format="dp:3"),
      list(name="or", title = "Odds Ratio", type="number", format="sf:4;dp:3"),
      list(name="zval", title = "z", type="number", format="sf:4;dp:3"),
      list(name="pval", title = "p", type="number", format="dp:3;p:.001"),
      list(name="vsmpr", title = "VS-MPR\u002A", type="number", format="sf:4;dp:3"),
      list(name="cilo", title = "Lower bound", type="number", format="dp:3", overTitle=ciTitle),
      list(name="ciup", title = "Upper bound", type="number", format="dp:3", overTitle=ciTitle)
    )
    
    
    # then determine which ones we need
    selectFields <- with(options, c(TRUE, TRUE, TRUE, stdCoeff, oddsRatios, 
                                    TRUE, TRUE, VovkSellkeMPR, coeffCI, coeffCI))
    
    out[["schema"]] <- list(fields=fields[selectFields])
    
    
    footnotes <- .newFootnotes()
    
    if (options[["stdCoeff"]]) {
      .addFootnote(footnotes, symbol = "\u207A", text = "Standardized estimates 
      represent estimates where the continuous predictors are standardized 
      (X-standardization).")
    }
    if (options[["VovkSellkeMPR"]]) {
      .addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
      <em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
      possible odds in favor of H\u2081 over H\u2080 equals
      1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
      (Sellke, Bayarri, & Berger, 2001).")
    }
    
    # Add footnote of predicted level
    if (options[["dependent"]] != "") {
      predVar <- as.character(glmObj[["terms"]])[2]
      predLevel <- levels(glmObj[["data"]][[predVar]])[2]
      msg <- paste0(predVar, " level '", predLevel, "' coded as class 1.")
      .addFootnote(footnotes, symbol="<em>Note.</em>", text = msg)
    }
    
    if (length(footnotes$footnotes) > 0) {
      out[["footnotes"]] <- as.list(footnotes)
    }
    
    if (perform == "run" && !is.null(glmObj)) {
      rows <- list()
      s <- summary(glmObj)[["coefficients"]]
      rn <- rownames(s)
      c <- qnorm(1 - (100 - options[["coeffCIInterval"]]) / 200)
      beta <- .stdEst(glmObj, type = "X") # stand. X continuous vars
      
      # Confidence intervals on the odds ratio scale
      if (options[["coeffCIOR"]]) {
        expon <- function(x) exp(x)
      } else {
        expon <- function(x) x
      }
      
      
      if (length(rn) == 1) {
        s <- unname(s)
        if (options[["robustSEOpt"]]) {
          s[2] <- unname(.glmRobustSE(glmObj)) # new se
          s[3] <- s[1]/s[2] # new z
          s[4] <- 2*pnorm(-abs(s[3])) # new p 
        }
        rows[[1]] <- list(param = .clean(.formatTerm(rn, glmObj)),
                          est = .clean(s[1]),
                          se = .clean(s[2]),
                          std = .clean(as.numeric(beta)),
                          or = .clean(exp(s[1])),
                          zval = .clean(s[3]),
                          pval = .clean(s[4]),
                          vsmpr = .clean(.VovkSellkeMPR(s[4])),
                          cilo = .clean(expon(s[1] - c * s[2])),
                          ciup = .clean(expon(s[1] + c * s[2])))
      } else {
        if (options[["robustSEOpt"]]) {
          s[,2] <- unname(.glmRobustSE(glmObj)) # new se
          s[,3] <- s[,1]/s[,2] # new z
          s[,4] <- 2*pnorm(-abs(s[,3])) # new p 
        }
        for (i in seq_along(rn)) {
          rows[[i]] <- list(param = .clean(.formatTerm(rn[i], glmObj)),
                            est = .clean(s[i,1]),
                            se = .clean(s[i,2]),
                            std = .clean(as.numeric(beta[i])),
                            or = .clean(exp(s[i,1])),
                            zval = .clean(s[i,3]),
                            pval = .clean(s[i,4]),
                            vsmpr = .clean(.VovkSellkeMPR(s[i,4])),
                            cilo = .clean(expon(s[i,1] - c * s[i,2])),
                            ciup = .clean(expon(s[i,1] + c * s[i,2])))
        }
      }
      
    } else {
      rows <- list(
        list(param = ".", est = ".", se = ".", std = ".", or = ".",
             zval = ".", pval = ".", vsmpr = ".", cilo = ".", ciup = ".")
      )
    }
    
    out[["data"]] <- rows
  } 
  return(out)
}

.glmConfusionMatrix <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial") {
    
    out[["title"]] <- "Confusion matrix"

    if (perform == "run" && !is.null(glmObj)) {
      if (options[["confusionMatrixProportions"]]) {
        n <- length(glmObj[["y"]])
        typ <- "number"
        frm <- "sf:4;dp:3"
      } else {
        n <- 1
        typ <- "integer"
        frm <- ""
      }
      
      levs <- levels(glmObj[["model"]][,1])
      fields <- list(
        list(name="obs", title = "Observed", type="string"),
        list(name="pred0", title = levs[1], type=typ, format=frm, overTitle = "Predicted"),
        list(name="pred1", title = levs[2], type=typ, format=frm, overTitle = "Predicted")
      )
      m <- .confusionMatrix(glmObj, cutoff = 0.5)[["matrix"]]
      rows <- list(
        list(obs = levs[1], pred0 = m[1,1]/n, pred1 = m[1,2]/n),
        list(obs = levs[2], pred0 = m[2,1]/n, pred1 = m[2,2]/n)
      )
    } else {
      fields <- list(
        list(name="obs", title = "Observed", type="string"),
        list(name="pred0", title = "0", type="number", format="sf:4;dp:3", overTitle = "Predicted"),
        list(name="pred1", title = "1", type="number", format="dp:3", overTitle = "Predicted")
      )
      rows <- list(
        list(obs = "0", pred0 = ".", pred1 = "."),
        list(obs = "1", pred0 = ".", pred1 = ".")
      )
    }
    out[["schema"]] <- list(fields=fields)
    out[["data"]] <- rows
  }
  
  return(out)
}

.glmPerformanceMetrics <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial") {
      
    # determine which scores we need
    scrNeed  <- with(options, c(AUC, Sens, Spec, Prec, Fmsr, BrierScr, Hmsr))

    if (perform == "run" && !is.null(glmObj) && any(scrNeed)) {  
      out[["title"]] <- "Performance metrics"
      # make fields
      fields <- list(
        list(name="met", title = "", type="string"),
        list(name="val", title = "Value", type="number", format="sf:4;dp:3")
      )
      
      m <- .confusionMatrix(glmObj, cutoff = 0.5)[["metrics"]]
      rows <- list(
        list(met = "AUC", val = m[["AUC"]]),
        list(met = "Sensitivity", val = m[["Sens"]]),
        list(met = "Specificity", val = m[["Spec"]]),
        list(met = "Precision", val = m[["Precision"]]),
        list(met = "F-measure", val = m[["F"]]),
        list(met = "Brier score", val = m[["Brier"]]),
        list(met = "H-measure", val = m[["H"]])
      )
      
      # delete the rows we don't need
      rows <- rows[scrNeed]
      
      out[["schema"]] <- list(fields=fields)
      out[["data"]] <- rows
      
    } else if (any(scrNeed)){
      out[["title"]] <- "Performance metrics"
      # make fields
      fields <- list(
        list(name="met", title = "", type="string"),
        list(name="val", title = "Value", type="number", format="sf:4;dp:3")
      )
      
      rows <- list(
        list(met = "AUC", val = "."),
        list(met = "Sensitivity", val = "."),
        list(met = "Specificity", val = "."),
        list(met = "Precision", val = "."),
        list(met = "F-measure", val = "."),
        list(met = "Brier score", val = "."),
        list(met = "H-measure", val = ".")
      )
      
      # delete the rows we don't need
      rows <- rows[scrNeed]
      
      out[["schema"]] <- list(fields=fields)
      out[["data"]] <- rows
    }
    
    
  }
  
  return(out)
}

.glmEstimatesPlots <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial") {
    
    predictors <- character(0)
    for (term in options[["modelTerms"]]) {
      if (length(term[["components"]]) == 1 && 
          (is.null(term[["isNuisance"]]) || !term[["isNuisance"]])) {
        predictors <- c(predictors, term[["components"]])
      }
    }
    
    if (length(predictors) > 0 && !is.null(glmObj)) {
      plots <- vector("list", length(predictors))
      if (perform == "run") {  
        for (i in seq_along(predictors)) {
          curvePlot <- list()
          gg <- .plotLogCurve(glmObj, predictors[i], options[["showPoints"]])
          plotObj <- .writeImage(width = options[["plotWidth"]],
                                 height = options[["plotHeight"]],
                                 plot = gg,
                                 obj = TRUE)
          curvePlot[["width"]] <- options[["plotWidth"]]
          curvePlot[["height"]] <- options[["plotHeight"]]
          curvePlot[["custom"]] <- list(width = "plotWidth",
                                        height = "plotHeight")
          curvePlot[["title"]] <- predictors[i]
          curvePlot[["data"]] <- plotObj[["png"]]
          curvePlot[["obj"]] <- plotObj[["obj"]]
          curvePlot[["convertible"]] <- TRUE
          curvePlot[["status"]] <- "complete"
          plots[[i]] <- curvePlot
        }
      } else {
        for (i in seq_along(predictors)) {
          curvePlot <- list()
          curvePlot[["width"]] <- options[["plotWidth"]]
          curvePlot[["height"]] <- options[["plotHeight"]]
          curvePlot[["custom"]] <- list(width = "plotWidth",
                                        height = "plotHeight")
          curvePlot[["title"]] <- predictors[i]
          curvePlot[["data"]] <- ""
          curvePlot[["convertible"]] <- FALSE
          curvePlot[["status"]] <- "waiting"
          plots[[i]] <- curvePlot
        }
      }
      out[["collection"]] <- plots
      out[["title"]] <- "Estimates plots"
    }
  }
  
  return(out)
}

.glmPredictorResidualsPlots <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial") {

    predictors <- character(0)
    for (term in options[["modelTerms"]]) {
      if (length(term[["components"]]) == 1 && 
          (is.null(term[["isNuisance"]]) || !term[["isNuisance"]])) {
        predictors <- c(predictors, term[["components"]])
      }
    }
    
    if (length(predictors) > 0 && !is.null(glmObj)) {
      plots <- vector("list", length(predictors))
      if (perform == "run") {  
        for (i in seq_along(predictors)) {
          resPlot <- list()
          gg <- .plotGlmResiduals(glmObj, predictors[i], 
                                  type = options[["residualType"]])
          plotObj <- .writeImage(width = options[["plotWidth"]],
                                 height = options[["plotHeight"]],
                                 plot = gg,
                                 obj = TRUE)
          resPlot[["width"]] <- options[["plotWidth"]]
          resPlot[["height"]] <- options[["plotHeight"]]
          resPlot[["custom"]] <- list(width = "plotWidth",
                                      height = "plotHeight")
          resPlot[["title"]] <- predictors[i]
          resPlot[["data"]] <- plotObj[["png"]]
          resPlot[["obj"]] <- plotObj[["obj"]]
          resPlot[["convertible"]] <- TRUE
          resPlot[["status"]] <- "complete"
          plots[[i]] <- resPlot
        }
      } else {
        for (i in seq_along(predictors)) {
          resPlot <- list()
          resPlot[["width"]] <- options[["plotWidth"]]
          resPlot[["height"]] <- options[["plotHeight"]]
          resPlot[["custom"]] <- list(width = "plotWidth",
                                        height = "plotHeight")
          resPlot[["title"]] <- predictors[i]
          resPlot[["data"]] <- ""
          resPlot[["convertible"]] <- FALSE
          resPlot[["status"]] <- "waiting"
          plots[[i]] <- resPlot
        }
      }
      out[["collection"]] <- plots
      out[["title"]] <- "Predictor - residuals plots"
    }
  }
  
  return(out)
}

.glmPredictedResidualsPlot <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial") {
    if (!is.null(glmObj)) {
      resPlot <- list()
      gg <- .plotGlmResiduals(glmObj, type = options[["residualType"]])
      plotObj <- .writeImage(width = options[["plotWidth"]],
                             height = options[["plotHeight"]],
                             plot = gg,
                             obj = TRUE)
      resPlot[["width"]] <- options[["plotWidth"]]
      resPlot[["height"]] <- options[["plotHeight"]]
      resPlot[["custom"]] <- list(width = "plotWidth",
                                    height = "plotHeight")
      resPlot[["title"]] <- "Predicted - residuals plot"
      resPlot[["data"]] <- plotObj[["png"]]
      resPlot[["obj"]] <- plotObj[["obj"]]
      resPlot[["convertible"]] <- TRUE
      resPlot[["status"]] <- "complete"
      out <- resPlot
    } else {
      resPlot <- list()
      resPlot[["width"]] <- options[["plotWidth"]]
      resPlot[["height"]] <- options[["plotHeight"]]
      resPlot[["custom"]] <- list(width = "plotWidth",
                                    height = "plotHeight")
      resPlot[["title"]] <- "Predicted - residuals plot"
      resPlot[["data"]] <- ""
      resPlot[["convertible"]] <- FALSE
      resPlot[["status"]] <- "waiting"
      out <- resPlot
    }
  }
  return(out)
}

.glmSquaredPearsonResidualsPlot <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial") {
    resPlot <- list()
    if (!is.null(glmObj)) {
      gg <- .plotSquaredPearsonResiduals(glmObj)
      plotObj <- .writeImage(width = options[["plotWidth"]],
                             height = options[["plotHeight"]],
                             plot = gg,
                             obj = TRUE)
      resPlot[["width"]] <- options[["plotWidth"]]
      resPlot[["height"]] <- options[["plotHeight"]]
      resPlot[["custom"]] <- list(width = "plotWidth",
                                    height = "plotHeight")
      resPlot[["title"]] <- "Squared Pearson residuals plot"
      resPlot[["data"]] <- plotObj[["png"]]
      resPlot[["obj"]] <- plotObj[["obj"]]
      resPlot[["convertible"]] <- TRUE
      resPlot[["status"]] <- "complete"
    } else {
      resPlot[["width"]] <- options[["plotWidth"]]
      resPlot[["height"]] <- options[["plotHeight"]]
      resPlot[["custom"]] <- list(width = "plotWidth",
                                    height = "plotHeight")
      resPlot[["title"]] <- "Predicted - residuals plot"
      resPlot[["data"]] <- ""
      resPlot[["convertible"]] <- FALSE
      resPlot[["status"]] <- "waiting"
    }
    out <- resPlot
  }
  return(out)
}

.glmFactorDescriptives <- function(dataset, options, perform, type) {
  # I stole/adapted this function from .anovaDescriptivesTable()!
  out <- NULL
  if (type == "binomial") {
    out <- list()
    out[["title"]] <- "Factor Descriptives"
    
    fields <- list()
    
    if (length(options[["factors"]]) == 0) {
      fields[[1]] <- list(name = "Factor", title = "Factor", type = "string")
    } else {
      for (variable in options[["factors"]]) {
        name <- paste(".", variable, sep = "")  # in case it's "N"
        fields[[length(fields)+1]] <- list(name = name, type = "string", 
                                           title = variable, combine = TRUE)
      }
    }
    
    fields[[length(fields)+1]] <- list(name = "N", type = "number", 
                                       format = "dp:0")
    
    out[["schema"]] <- list(fields = fields)
    
    rows <- list()
    if (perform == "run" && length(options[["factors"]]) > 0) {
      lvls <- list()
      factors <- list()

      for (variable in options[["factors"]]) {
        factor <- dataset[[ .v(variable) ]]
        factors[[length(factors)+1]] <- factor
        lvls[[ variable ]] <- levels(factor)
      }

      cases <- rev(expand.grid(rev(lvls)))
      namez <- unlist(options[["factors"]])
      columnNames <- paste(".", namez, sep="")

      if (length(options[["factors"]]) > 0) {
        for (i in 1:dim(cases)[1]) {
          row <- list()
          for (j in 1:dim(cases)[2]) {
            row[[ columnNames[[j]] ]] <- as.character(cases[i, j])
          }

          sub  <- eval(parse(text=paste("dataset$", .v(namez), " == \"", row, 
                                        "\"", sep="", collapse=" & ")))

          dat <- base::subset(dataset, sub)[[1]]
          N <- base::length(dat)

          row[["N"]] <- N
          
          if(cases[i,dim(cases)[2]] == lvls[[ dim(cases)[2] ]][1]) {
            row[[".isNewGroup"]] <- TRUE
          } else {
            row[[".isNewGroup"]] <- FALSE
          }

          rows[[i]] <- row
        }
      }
    } else if (perform == "run") {
      rows <- list(list(Factor = ".", N = "."))
    }
    
    out[["data"]] <- rows
  }
  return(out)
}

# Helper functions for the above.
.lrtest <- function(glmObj) {
  # likelihood ratio test for model against null model
  chisq <- max(0,glmObj[["null.deviance"]] - glmObj[["deviance"]])
  df <- glmObj[["df.null"]] - glmObj[["df.residual"]]
  if (chisq == 0) {
    p <- NULL
  } else {
    p <- dchisq(chisq, df)
  }
  return(list(stat = chisq, df = df, pval = p))
}

.mcFadden <- function(glmObj) {
  # https://eml.berkeley.edu/reprints/mcfadden/zarembka.pdf
  if (deparse(glmObj[["formula"]][[3]]) %in% c("1", "0")) {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    return(max(c(0,1-(glmObj[["deviance"]]/glmObj[["null.deviance"]]))))
  }
}

.nagelkerke <- function(glmObj) {
  # https://doi.org/10.1093/biomet/78.3.691
  if (deparse(glmObj[["formula"]][[3]]) %in% c("1", "0")) {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    l0 <- -0.5*glmObj[["null.deviance"]]
    lm <- as.numeric(logLik(glmObj))
    n <- length(glmObj[["y"]])
    coxSnell <- 1 - exp(l0 - lm)^(2 / n)
    denom <- 1 - exp(l0)^(2 / n)
    return(max(c(0,coxSnell / denom)))
  }
}

.tjur <- function(glmObj) {
  # http://dx.doi.org/10.1198/tast.2009.08210
  if (deparse(glmObj[["formula"]][[3]]) %in% c("1", "0")) {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    ps <- predict(glmObj, type = "response")
    ys <- glmObj[["y"]]
    return(max(c(0,mean(ps[ys])-mean(ps[-ys]))))
  }
}

.aicNull <- function(glmObj) {
  if (!is.null(glmObj[["nullModel"]])) {
    return(2*length(coef(glmObj[["nullModel"]]))+glmObj[["null.deviance"]])
  } else if (attr(glmObj[["terms"]], "intercept")) {
    return(2+glmObj[["null.deviance"]])
  } else {
    return(glmObj[["null.deviance"]])
  }
}

.bic <- function(glmObj) {
  return(log(length(glmObj[["y"]]))*length(coef(glmObj))+glmObj[["deviance"]])
}

.bicNull <- function(glmObj) {
  if (!is.null(glmObj[["nullModel"]])) {
    return(log(length(glmObj[["y"]]))*length(coef(glmObj[["nullModel"]]))+
           glmObj[["null.deviance"]])
  } else if (attr(glmObj[["terms"]], "intercept")) {
    return(log(length(glmObj[["y"]]))+glmObj[["null.deviance"]])
  } else {
    return(glmObj[["null.deviance"]])
  }
}

.stdEst <- function(glmObj, type = "X") {
  # This function fits a new model with scaled variables and outputs the coeffs:
  # type = "X" : covariates scaled
  # type = "Y" : outcome scaled
  # type = "XY" : covariates and outcomes scaled
  # NB: factors (dummy-coded) will never be scaled. 
  if (attr(glmObj[["terms"]], "intercept")) {
    b <- summary(glmObj)[["coefficients"]][-1,1]
  } else {
    b <- summary(glmObj)[["coefficients"]][,1]
  }
  
  factors <- names(glmObj[["xlevels"]])
  xmod <- glmObj[["model"]][!names(glmObj[["model"]]) %in% factors][,-1]
  xfac <- glmObj[["model"]][names(glmObj[["model"]]) %in% factors]
  ymod <- glmObj[["model"]][1]
  
  if (type == "X") {
    stdDat <- cbind(ymod, scale(xmod), xfac)
  } else if (type == "Y") {
    stdDat <- cbind(scale(ymod), xmod, xfac)
  } else if (type == "XY") {
    stdDat <- cbind(scale(ymod), scale(xmod), xfac)
  } 
  
  names(stdDat) <- names(glmObj[["model"]])
  
  stdMod <- stats::glm(formula = glmObj[["formula"]], data = stdDat, 
                       family = glmObj[["family"]])
  
  return(coef(stdMod))
}

.confusionMatrix <- function(glmObj, cutoff=0.5) {
  cMat <- list()
  pred <- predict(glmObj,type = "response")
  obs <- glmObj$y
  h <- hmeasure::HMeasure(obs, pred, threshold = cutoff)
  m <- matrix(c(h[["metrics"]][["TN"]], h[["metrics"]][["FN"]],
                h[["metrics"]][["FP"]], h[["metrics"]][["TP"]]), 2)
  dimnames(m) <- list("Observed" = c(0, 1), "Predicted" = c(0, 1))
  h[["metrics"]][["Brier"]] <- .brierScore(obs, pred)
  cMat[["matrix"]] <- m
  cMat[["metrics"]] <- h[["metrics"]]
  cMat
}

.brierScore <- function(obs, pred) {
  sum((pred - obs)^2) / length(pred)
}

.formatTerm <- function(term, glmObj) {
  # input: string of model term & glmObj
  vars <- names(glmObj[["model"]][-1])
  
  if (attr(glmObj[["terms"]], "intercept")) {
    vars <- c(vars, "(Intercept)")
  }
  
  # escape special regex characters
  vars <- gsub("(\\W)", "\\\\\\1", vars, perl=TRUE)
  
  # regex patterns
  pat1 <- paste0("\\:","(?=(",paste(vars, collapse = ")|("),"))")
  pat2 <- paste0("(?<=(",paste(vars, collapse = ")|("),"))")
  
  # split up string into components
  spl <- strsplit(term, pat1, perl = TRUE)[[1]]
  spl2 <- lapply(spl, function(t) strsplit(t, pat2, perl = TRUE))
  
  # format and add back together
  col <- lapply(spl2, function(s) {
    if (length(unlist(s)) > 1) {
     return(paste0(paste(unlist(s), collapse = " ("), ")"))
    } else {
      return(unlist(s))
    }
  })
  col2 <- paste(unlist(col), collapse = " * ")
  
  return(col2)
}

.predLevel <- function(glmObj) {
  predVar <- as.character(glmObj[["terms"]])[2]
  return(levels(glmObj[["data"]][[predVar]])[2])  
}

.plotLogCurve <- function(glmObj, var, points = TRUE) {
  # If user wants raw data points, get them from data
  if (points) {
    z <- model.matrix(glmObj)
    factors <- names(glmObj[["xlevels"]])
    if (var %in% factors) {
      vd <- as.factor(glmObj[["data"]][[var]])
    } else {
      vd <- z[,var]
    }
    ggdat <- data.frame(x = vd, y = glmObj$y)
  }
  
  # Calculate ribbon & main line
  ribdat <- .glmLogRegRibbon(glmObj, var)

  # Find predicted level
  predVar <- as.character(glmObj[["terms"]])[2]
  predLevel <- levels(glmObj[["data"]][[predVar]])[2]
  
  # this will become the y-axis title
  ytitle <- substitute(expr = "P("*italic(x)~italic("=")~italic(y)*")",
                       env = list(x = predVar, y = predLevel))

  if (attr(ribdat, "factor")) {
    # the variable is a factor, plot points with errorbars
    p <- ggplot2::ggplot(ribdat, ggplot2::aes(x = x, y = y))
    
    if (points) {
      p <- p + ggplot2::geom_point(
          data = ggdat,
          size = 2,
          position = ggplot2::position_jitter(height = 0.01, width = 0.04),
          color = "dark grey",
          alpha = 0.3
        )
    }
    
    p <- p +
      ggplot2::geom_point(
        data = ribdat,
        mapping = ggplot2::aes(x = x, y = y),
        size = 4
      ) +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes(x = x, ymin = lo, ymax = hi),
        data = ribdat, width = 0.2
      )

  } else {
    # the variable is continuous, plot curve with error ribbon
    p <- ggplot2::ggplot(ribdat, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_ribbon(
        data = ribdat,
        mapping = ggplot2::aes(x = x, ymax = hi, ymin = lo),
        fill = "light grey",
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        data = ribdat,
        mapping = ggplot2::aes(x = x, y = y),
        size = .75, 
        color = "black"
      )
    
    if (points) {
      p <- p + ggplot2::geom_point(
        data = ggdat,
        size = 2,
        position = ggplot2::position_jitter(height = 0.03, width = 0),
        color = "dark grey",
        alpha = 0.3
      )
    }  
  }


  # We've got our plot, time for some theming!
  # First, define custom y and x-axes to draw
  custom_y_axis <- function() {
    d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
    list(
      ggplot2::geom_segment(
        data = d, 
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE, 
        size = 1
      ), 
      ggplot2::scale_y_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1)
      )
    )
  }

  custom_x_axis <- function(ribdat) {
    l <- NULL
    xdat <- ribdat[["x"]]
    if (attr(ribdat, "factor")) {
      l <- list(ggplot2::scale_x_discrete(labels = levels(xdat)))
    } else {
      b <- pretty(xdat)
      d <- data.frame(y = -Inf, yend = -Inf, x = min(b), xend = max(b))
      l <- list(
        ggplot2::geom_segment(
          data = d,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE, 
          size = 1
        ),
        ggplot2::scale_x_continuous(
          breaks = b
        )
      )
    }
  }
  
  # then perform the theme and return the ggplot object
  p + ggplot2::xlab(var) +
    ggplot2::ylab(ytitle) +
    ggplot2::theme_bw() +
    custom_x_axis(ribdat) +
    custom_y_axis() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 18),
      panel.grid.major = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 18, vjust = 0.1),
      axis.title.y = ggplot2::element_text(size = 18, vjust = 0.9),
      axis.text.x = ggplot2::element_text(
        size = 15,
        margin = ggplot2::margin(t = 1, unit = "mm")
      ),
      axis.text.y = ggplot2::element_text(
        size = 15,
        margin = ggplot2::margin(r = 1, unit = "mm")
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent", 
        colour = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", 
        colour = NA
      ),
      legend.background = ggplot2::element_rect(
        fill = "transparent", 
        colour = NA
      ),
      panel.border = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(size = 0.5),
      axis.ticks.length = grid::unit(2.5, "mm"),
      plot.margin = grid::unit(c(0.1, 0.1, 0.6, 0.6), "cm"),
      legend.position = "none"
    )
}

.glmLogRegRibbon <- function(logRes, var, ciInt = 0.95) {
  # This function calculates the estimation & CI datapoints for logreg plot
  mm <- model.matrix(logRes)
  cm <- colMeans(mm)
  fac <- FALSE
  factors <- names(logRes$xlevels)
  if (length(factors) != 0 && var %in% factors) {
    # variable is factor with dummy coded levels
    fac <- TRUE
    levs <- paste0(var, logRes[["xlevels"]][[var]])
    cmNoVar <- cm[!(names(cm) %in% levs)]
    xs <- factor(logRes[["xlevels"]][[var]],
                 levels = logRes[["xlevels"]][[var]])

    # make a predFrame with nlevels rows
    predFrame <- data.frame(matrix(rep(cmNoVar, length(levs)),
                                   nrow = length(levs),
                                   byrow = TRUE),
                            xs,
                            stringsAsFactors = FALSE)
    colnames(predFrame) <- c(names(cmNoVar), var)

    # Fix potential other factors in the dataset
    if (length(factors) > 1) {
      # set factor to first level (default)
      for (f in factors) {
        if (f != var) {
          predFrame[[f]] <- factor(rep(logRes[["xlevels"]][[f]][1],
                                       length(levs)),
                                   levels = logRes[["xlevels"]][[f]])
        }
      }
    }
  } else {
    vd <- pretty(mm[,var])
    xs <- seq(min(vd), max(vd), length.out = 101) # ggplot2 default
    cmNoVar <- cm[names(cm) != var]
    predFrame <- data.frame(matrix(rep(cmNoVar, 101), nrow = 101, byrow = TRUE),
                            xs, stringsAsFactors = FALSE)
    colnames(predFrame) <- c(names(cmNoVar), var)
    if (length(factors) != 0) {
      # set factor to first level (default)
      for (f in factors) {
        predFrame[[f]] <- factor(rep(logRes[["xlevels"]][[f]][1], 101),
                                 levels = logRes[["xlevels"]][[f]])
      }
    }
  }

  pred <- predict(logRes, newdata = predFrame, type = "link", se.fit = TRUE)
  ys <- .invLogit(pred$fit)
  critValue <- qnorm(1 - (1 - ciInt) / 2)
  lo <- .invLogit(pred$fit - critValue * pred$se.fit)
  hi <- .invLogit(pred$fit + critValue * pred$se.fit)
  outFrame <- data.frame(x = xs, y = ys, lo = lo, hi = hi)

  attr(outFrame, "factor") <- fac

  outFrame
}

.invLogit <- function(x) {
  1/(1 + exp(-x))
}

.glmRobustSE <- function(glmObj) {
  # Robust SE courtesy of Dan Gillen (UC Irvine)
  if (is.matrix(glmObj[["x"]])) {
    xmat <- glmObj[["x"]]
  } else {
    mf <- model.frame(glmObj)
    xmat <- model.matrix(terms(glmObj), mf)
  }
  
  umat <- residuals(glmObj, "working") * glmObj[["weights"]] * xmat
  modelv <- summary(glmObj)[["cov.unscaled"]]
  robustCov <- modelv%*%(t(umat)%*%umat)%*%modelv
  dimnames(robustCov) <- dimnames(vcov(glmObj))
  
  ##	Format the model output with p-values and CIs
  s <- summary(glmObj) 
  robustSE <- sqrt(diag(robustCov)) 
  return(robustSE)
}

.plotGlmResiduals <- function(glmObj, var = NULL, type = "deviance") {
  # plots residuals against predicted (var = NULL) or predictor (var = "name")
  if (!is.null(var)) {
    ggdat <- data.frame(resid = residuals(glmObj, type = type),
                        x = glmObj[["data"]][[var]])
  } else {
    ggdat <- data.frame(resid = residuals(glmObj, type = type), 
                        x = predict(glmObj, type = "response"))
    var <- "Predicted Probability"
  }
  
  if (class(ggdat[["x"]]) == "factor") {
    pos <- ggplot2::position_jitter(width = 0.1)
  } else {
    pos <- ggplot2::position_jitter(width = 0)
  }
  
  custom_y_axis <- function(val) {
    d <- data.frame(x = -Inf, xend = -Inf, 
                    y = min(pretty(val)), yend = max(pretty(val)))
    list(
      ggplot2::geom_segment(
        data = d, 
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE, 
        size = 1
      ), 
      ggplot2::scale_y_continuous(
        breaks = pretty(val)
      )
    )
  }
  
  custom_x_axis <- function(val) {
    if (class(val) == "factor") {
      l <- list(ggplot2::scale_x_discrete(labels = levels(val)))
    } else {
      d <- data.frame(y = -Inf, yend = -Inf, 
                      x = min(pretty(val)), xend = max(pretty(val)))
      l <- list(
        ggplot2::geom_segment(
          data = d, 
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE, 
          size = 1
        ), 
        ggplot2::scale_x_continuous(
          breaks = pretty(val)
        )
      )
    }
    return(l)
  }
  
  
  p <- ggplot2::ggplot(data = ggdat, 
                       mapping = ggplot2::aes(x = x, y = resid)) +
    ggplot2::geom_point(position = pos, size = 3, colour="black", fill = "grey", 
                        pch=21) 
  
  p <- p +
    ggplot2::xlab(var) +
    ggplot2::ylab("Residuals") +
    ggplot2::theme_bw() +
    custom_y_axis(ggdat[["resid"]]) +
    custom_x_axis(ggdat[["x"]]) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 18),
      panel.grid.major = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 18, vjust = 0.1),
      axis.title.y = ggplot2::element_text(size = 18, vjust = 0.9),
      axis.text.x = ggplot2::element_text(
        size = 15,
        margin = ggplot2::margin(t = 1, unit = "mm")
      ),
      axis.text.y = ggplot2::element_text(
        size = 15,
        margin = ggplot2::margin(r = 1, unit = "mm")
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent", 
        colour = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", 
        colour = NA
      ),
      legend.background = ggplot2::element_rect(
        fill = "transparent", 
        colour = NA
      ),
      panel.border = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(size = 0.5),
      axis.ticks.length = grid::unit(2.5, "mm"),
      plot.margin = grid::unit(c(0.1, 0.1, 0.6, 0.6), "cm"),
      legend.position = "none"
    )
    
  p
}

.plotSquaredPearsonResiduals <- function(glmObj) {
  # Squared Pearson residuals plot courtesy of Dan Gillen (UC Irvine)
  plotDat <- data.frame("pres" = residuals(glmObj, type = "pearson")^2,
                        "prob" = predict(glmObj, type = "response"))

  custom_y_axis <- function(ydat) {
    b <- pretty(c(ydat,0))
    d <- data.frame(y = 0, yend = max(b), x = -Inf, xend = -Inf)
    l <- list(ggplot2::geom_segment(data = d,
                                    ggplot2::aes(x = x, y = y, xend = xend,
                                                 yend = yend),
                                    inherit.aes = FALSE, size = 1),
              ggplot2::scale_y_continuous(breaks = b))
  }

  custom_x_axis <- function() {
    d <- data.frame(y = -Inf, yend = -Inf, x = 0, xend = 1)
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                      yend = yend),
                               inherit.aes = FALSE, size = 1),
         ggplot2::scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)))
  }




  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = prob, y = pres), data = plotDat) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 1),
                          linetype = 3, size = 1, colour = "grey") +
    ggplot2::geom_smooth(se=FALSE, method = "loess", size = 1.2, 
                         colour = "darkred") +
    ggplot2::geom_point(size = 3, colour="black", fill = "grey", pch=21) +
    custom_y_axis(plotDat[["pres"]]) +
    custom_x_axis() +
    ggplot2::labs(x = "Predicted Probability", y = "Squared Pearson Residual") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 18),
      panel.grid.major = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 18, vjust = 0.1),
      axis.title.y = ggplot2::element_text(size = 18, vjust = 0.9),
      axis.text.x = ggplot2::element_text(size = 15,
                                          margin = ggplot2::margin(t = 1,
                                                                   unit = "mm")),
      axis.text.y = ggplot2::element_text(size = 15,
                                          margin = ggplot2::margin(r = 1,
                                                                   unit = "mm")),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.border = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(size = 0.5),
      axis.ticks.length = grid::unit(2.5, "mm"),
      plot.margin = grid::unit(c(0.1, 0.1, 0.6, 0.6), "cm"),
      legend.position = "none")
  
  return(p)
}
