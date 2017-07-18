# Backend for GLM models. Poor man's object-oriented approach
.jaspGlm <- function(dataset, options, perform, type) {
  glmRes <- NULL
  if (perform == "run" && options[["dependent"]] != "") {
    if (!is.null(type) && type == "binomial") {
      f <- .createGlmFormula(options)
      names(dataset) <- .unv(names(dataset))
      glmRes <- glm(f[["plaintext"]], data = dataset, family = "binomial")
    } else {
      console.log("GLM type not supported")
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
  print(f)
  print(f.base64)
  list(plaintext = f, base64 = f.base64)
}

.glmModelSummary <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial") {
    
    out[["title"]] <- "Model summary"
    
    fields <- list(
      list(name="mod", title = "Model", type="string"),
      list(name="dev", title = "Deviance", type="number", format="sf:4;dp:3"),
      list(name="aic", title = "AIC", type="number", format="dp:3"),
      list(name="bic", title = "BIC", type="number", format="dp:3"),
      list(name="fad", title = "McFadden R²", type="number", format="sf:4;dp:3"),
      list(name="nag", title = "Nagelkerke R²", type="number", format="sf:4;dp:3"),
      list(name="tju", title = "Tjur R²", type="number", format="sf:4;dp:3")
    )
    
    out[["schema"]] <- list(fields=fields)
    
    if (perform == "run" && !is.null(glmObj)) {
      rows <- list(
        list(mod = "0", dev = .clean(glmObj[["null.deviance"]]),
             aic = .clean(.aicNull(glmObj)),
             bic = .clean(.bicNull(glmObj)),
             fad = .clean(NULL),
             nag = .clean(NULL),
             tju = .clean(NULL)),
        list(mod = "1", dev = .clean(glmObj[["deviance"]]),
             aic = .clean(glmObj[["aic"]]),
             bic = .clean(.bic(glmObj)),
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
    
    ciTitle <- paste0(options[["coeffCIInterval"]],"% Confidence interval")
    # first define all the fields
    fields <- list(
      list(name="param", title = "", type="string"),
      list(name="est", title = "Estimate", type="number", format="dp:3"),
      list(name="se", title = "Standard Error", type="number", format="dp:3"),
      list(name="std", title = "Standardized", type="number", format="dp:3"),
      list(name="or", title = "Odds Ratio", type="number", format="sf:4;dp:3"),
      list(name="zval", title = "z", type="number", format="sf:4;dp:3"),
      list(name="pval", title = "p", type="number", format="dp:3;p:.001"),
      list(name="vsmpr", title = "VS-MPR", type="number", format="sf:4;dp:3"),
      list(name="cilo", title = "Lower bound", type="number", format="dp:3", overTitle=ciTitle),
      list(name="ciup", title = "Upper bound", type="number", format="dp:3", overTitle=ciTitle)
    )
    
    # then determine which ones we need
    selectFields <- with(options, c(TRUE, TRUE, TRUE, stdCoeff, oddsRatios, 
                                    TRUE, TRUE, VovkSellkeMPR, coeffCI, coeffCI))
    
    out[["schema"]] <- list(fields=fields[selectFields])
    
    if (perform == "run" && !is.null(glmObj)) {
      rows <- list()
      s <- summary(glmObj)[["coefficients"]]
      rn <- rownames(s)      
      c <- qnorm(1 - (100 - options[["coeffCIInterval"]]) / 200)
      beta <- .stdEst(glmObj, type = "X") # stand. X continuous vars
      
      for (i in seq_along(rn)) {
        rows[[i]] <- list(param = .clean(.formatTerm(rn[i], glmObj)),
                          est = .clean(s[i,1]),
                          se = .clean(s[i,2]),
                          std = .clean(as.numeric(beta[i])),
                          or = .clean(exp(s[i,1])),
                          zval = .clean(s[i,3]),
                          pval = .clean(s[i,4]),
                          vsmpr = .clean(.VovkSellkeMPR(s[i,4])),
                          cilo = .clean(s[i,1]-c*s[i,2]),
                          ciup = .clean(s[i,1]+c*s[i,2]))
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
      levs <- levels(glmObj[["model"]][,1])
      fields <- list(
        list(name="obs", title = "Observed", type="string"),
        list(name="pred0", title = levs[1], type="number", format="sf:4;dp:3", overTitle = "Predicted"),
        list(name="pred1", title = levs[2], type="number", format="dp:3", overTitle = "Predicted")
      )
      m <- .confusionMatrix(glmObj, cutoff = 0.5)[["matrix"]]
      rows <- list(
        list(obs = levs[1], pred0 = m[1,1], pred1 = m[1,2]),
        list(obs = levs[2], pred0 = m[2,1], pred1 = m[2,2])
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
  }
  out[["schema"]] <- list(fields=fields)
  out[["data"]] <- rows
  return(out)
}

.glmPerformanceMetrics <- function(glmObj, options, perform, type) {
  out <- NULL
  type <- glmObj[["family"]][["family"]] # will be null if glmObj == null
  
  if (!is.null(type) && type == "binomial") {
    
  } 
  
  return(out)
}

.glmEstimatesPlots <- function(glmObj, options, perform, type) {
  out <- NULL
  type <- glmObj[["family"]][["family"]] # will be null if glmObj == null
  
  if (!is.null(type) && type == "binomial") {
    
  } 
  
  return(out)
}


# Pseudo-R² and the like
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
    lm <- -0.5*glmObj[["deviance"]]
    p <- mean(glmObj[["y"]])
    n <- length(glmObj[["y"]])
    coxSnell <- 1 - (l0 / lm)^(2 / n)
    return(max(c(0,coxSnell / (1 - (p^p * (1 - p)^(1 - p))))))
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
  if (attr(glmObj[["terms"]], "intercept")) {
    return(2+glmObj[["null.deviance"]])
  } else {
    return(glmObj[["null.deviance"]])
  }
}

.bic <- function(glmObj) {
  return(log(length(glmObj[["y"]]))*length(coef(glmObj))+glmObj[["deviance"]])
}

.bicNull <- function(glmObj) {
  if (attr(glmObj[["terms"]], "intercept")) {
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
  
  print(factors)
  print(glmObj$formula)
  
  if (type == "X") {
    stdDat <- cbind(ymod, scale(xmod), xfac)
  } else if (type == "Y") {
    stdDat <- cbind(scale(ymod), xmod, xfac)
  } else if (type == "XY") {
    stdDat <- cbind(scale(ymod), scale(xmod), xfac)
  } 
  
  names(stdDat) <- names(glmObj[["model"]])
  
  stdMod <- glm(formula = glmObj[["formula"]], data = stdDat, 
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
