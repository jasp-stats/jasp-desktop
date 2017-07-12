# Backend for GLM models. Poor man's object-oriented approach
.jaspGlm <- function(dataset, options, perform, type) {
  glmRes <- NULL
  if (perform == "run" && options[["dependent"]] != "") {
    if (type == "binomial") {
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
      term <- modelTerms[[i]]
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

.glmModelSummary <- function(glmObj, options, perform) {
  if (is.null(glmObj)) {
    return(NULL)
  }
  type <- glmObj[["family"]][["family"]]
  
  if (type == "binomial") {
    return(NULL)
  } else {
    console.log("GLM type not supported")
    return(NULL)
  }
}

.glmEstimatesTable <- function(glmObj, options, perform) {
  if (is.null(glmObj)) {
    return(NULL)
  }
  type <- glmObj[["family"]][["family"]]
  
  if (type == "binomial") {
    return(NULL)
  } else {
    console.log("GLM type not supported")
    return(NULL)
  }
}

.glmConfusionMatrix <- function(glmObj, options, perform) {
  if (is.null(glmObj)) {
    return(NULL)
  }
  type <- glmObj[["family"]][["family"]]
  
  if (type == "binomial") {
    return(NULL)
  } else {
    console.log("GLM type not supported")
    return(NULL)
  }
}

.glmPerformanceMetrics <- function(glmObj, options, perform) {
  if (is.null(glmObj)) {
    return(NULL)
  }
  type <- glmObj[["family"]][["family"]]
  
  if (type == "binomial") {
    return(NULL)
  } else {
    console.log("GLM type not supported")
    return(NULL)
  }
}

.glmEstimatesPlots <- function(glmObj, options, perform) {
  if (is.null(glmObj)) {
    return(NULL)
  }
  type <- glmObj[["family"]][["family"]]
  
  if (type == "binomial") {
    return(NULL)
  } else {
    console.log("GLM type not supported")
    return(NULL)
  }
}
