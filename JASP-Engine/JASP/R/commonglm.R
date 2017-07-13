# Backend for GLM models. Poor man's object-oriented approach
.jaspGlm <- function(dataset, options, perform, type) {
  glmRes <- NULL
  if (perform == "run" && options[["dependent"]] != "") {
    if (!is.null(type) && type == "binomial") {
      f <- .createGlmFormula(options)
      print(f)
      glmRes <- glm(f$base64, data = dataset, family = "binomial")
      print(glmRes)
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

  list(plaintext = f, base64 = f.base64)
}

.glmModelSummary <- function(glmObj, options, perform, type) {
  out <- NULL
  if (type == "binomial") {
    
    out[["title"]] <- "Model summary"
    
    fields <- list(
      list(name="metric", title = "", type="string"),
      list(name="value", title = "Value", type="number", format="sf:4;dp:3")
    )
    
    out[["schema"]] <- list(fields=fields)
    
    if (perform == "run" && !is.null(glmObj)) {
      rows <- list(
        list(metric = "Model Deviance", value = .clean(glmObj[["deviance"]])),
        list(metric = "Null Deviance", value = .clean(glmObj[["null.deviance"]])),
        list(metric = "McFadden R²", value = .clean(.mcFadden(glmObj))),
        list(metric = "Nagelkerke R²", value = .clean(.nagelkerke(glmObj))),
        list(metric = "Tjur R²", value = .clean(.tjur(glmObj))),
        list(metric = "AIC", value = .clean(glmObj[["AIC"]]))
      )
    } else {
      rows <- list(
        list(metric = "Model Deviance", value = "."),
        list(metric = "Null Deviance", value = "."),
        list(metric = "McFadden R²", value = "."),
        list(metric = "Nagelkerke R²", value = "."),
        list(metric = "Tjur R²", value = "."),
        list(metric = "AIC", value = ".")
      )
    }
    
    out[["data"]] <- rows
  } 
  
  return(out)
}

.glmEstimatesTable <- function(glmObj, options, perform) {
  out <- NULL
  type <- glmObj[["family"]][["family"]] # will be null if glmObj == null
  
  if (!is.null(type) && type == "binomial") {
    
  } 
  
  return(out)
}

.glmConfusionMatrix <- function(glmObj, options, perform) {
  out <- NULL
  type <- glmObj[["family"]][["family"]] # will be null if glmObj == null
  
  if (!is.null(type) && type == "binomial") {
    
  } 
  
  return(out)
}

.glmPerformanceMetrics <- function(glmObj, options, perform) {
  out <- NULL
  type <- glmObj[["family"]][["family"]] # will be null if glmObj == null
  
  if (!is.null(type) && type == "binomial") {
    
  } 
  
  return(out)
}

.glmEstimatesPlots <- function(glmObj, options, perform) {
  out <- NULL
  type <- glmObj[["family"]][["family"]] # will be null if glmObj == null
  
  if (!is.null(type) && type == "binomial") {
    
  } 
  
  return(out)
}


# Pseudo-R² and the like
.mcFadden <- function(glmObj) {
  # https://eml.berkeley.edu/reprints/mcfadden/zarembka.pdf
  if (deparse(glmObj[["formula"]][[3]]) == "1") {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    return(1-(glmObj[["deviance"]]/glmObj[["null.deviance"]]))
  }
}

.nagelkerke <- function(glmObj) {
  # https://doi.org/10.1093/biomet/78.3.691
  if (deparse(glmObj[["formula"]][[3]]) == "1") {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    l0 <- -0.5*glmObj[["null.deviance"]]
    lm <- -0.5*glmObj[["deviance"]]
    p <- mean(glmObj[["y"]])
    n <- length(glmObj[["y"]])
    coxSnell <- 1 - (l0 / lm)^(2 / n)
    return(coxSnell / (1 - (p^p * (1 - p)^(1 - p))))
  }
}

.tjur <- function(glmObj) {
  # http://dx.doi.org/10.1198/tast.2009.08210
  if (deparse(glmObj[["formula"]][[3]]) == "1") {
    # intercept-only model needs fix because of computer precision limits
    return(NULL)
  } else {
    ps <- predict(glmObj, type = "response")
    ys <- glmObj[["y"]]
    return(mean(ps[ys])-mean(ps[-ys]))
  }
}
