# 1. Main test function --------
bcor.test <- function(x, y, alternative=c("two.sided", "less", "greater"),
                      method=c("pearson", "kendall", "spearman"), ciValue=0.95,
                      use="pairwise.complete.obs",
                      h0=0, kappa=1, hyperGeoOverFlowThreshold=25, oneThreshold=0.001, var=1) {
  
  if (is.null(method)) {
    result <- .computePearsonCorBf10(NULL, NULL)
    result[["error"]] <- "No method selected"
    return(result)
  }
  
  stat <- tryOrFailWithNA(cor(x, y, use=use, method=method[1]))
  n <- tryOrFailWithNA(
    length(x) - length(
      unique(c(which(is.na(x)), which(is.na(y))))
    )
  )
  
  result <- bcor.testSumStat("n"=n, "stat"=stat, "alternative"=alternative,
                             "method"=method, "ciValue"=ciValue, "h0"=h0, "kappa"=kappa,
                             "hyperGeoOverFlowThreshold"=hyperGeoOverFlowThreshold,
                             "oneThreshold"=oneThreshold, "var"=var)
  result[["call"]] <- match.call()
  return(result)
}

bcor.testSumStat <- function(n, stat, alternative=c("two.sided", "less", "greater"),
                             method=c("pearson", "kendall", "spearman"), ciValue=0.95,
                             h0=0, kappa=1, hyperGeoOverFlowThreshold=25, oneThreshold=0.001, var=1) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  
  if (is.na(stat) | is.na(n) | n <= 0) {
    if (method=="pearson") {
      result <- .computePearsonCorBf10(NaN, NaN)
    } else if (method=="kendall") {
      result <- .computeKendallCorBf10(NaN, NaN)
    }
    
    result[["error"]] <- "Can't compute the correlation"
  }
  
  if (method=="pearson") {
    result <- .computePearsonCorBf10("n"=n, "r"=stat, "h0"=h0, "kappa"=kappa, "ciValue"=ciValue,
                                     "hyperGeoOverFlowThreshold"=hyperGeoOverFlowThreshold,
                                     "oneThreshold" = oneThreshold)
  } else if (method=="kendall") {
    result <- .computeKendallCorBf10("n"=n, "tauObs"=stat, "h0"=h0, "kappa"=kappa, "ciValue"=ciValue,
                                     "oneThreshold"=oneThreshold, "var"=var)
  } else if (method[1]=="spearman") {
    print("NOT YET THIS IS JUST PEARSON AS A PLACEHOLDER")
    result <- .computePearsonCorBf10("n"=NA, "r"=NA, "h0"=h0, "kappa"=kappa, "ciValue"=ciValue,
                                     "oneThreshold" = oneThreshold)
  }
  result[["alternative"]] <- alternative
  return(result)
}

.getSidedObject <- function(bfObject, alternative="two.sided", itemNames=NULL) {
  result <- modifyList(bfObject[[alternative]], bfObject[itemNames])
}


# 2 Compute posteriors --------
.computeCorPosteriorLine <- function(bfObject, method="pearson", alternative="two.sided", minX=-0.985, maxX=0.985) {
  xDomain <- seq(minX, maxX, length.out = 1001)
  if (alternative %in% c("two-sided", "two.sided")) {
    alternative <- "two.sided"
  } else if (alternative %in% c("greater", "right", "positive")) {
    alternative <- "greater"
  } else if (alternative %in% c("less", "left", "negative")) {
    alternative <- "less"
  }
  
  sidedObject <- .getSidedObject("bfObject"=bfObject, "alternative"=alternative,
                                 itemNames=c("error", "h0"))
  
  # Note(Alexander): Don't compute if it's already computed
  #
  if (!is.null(sidedObject[["posteriorLine"]]))
    return(sidedObject)
  
  # Note(Alexander): Don't compute if there's an error
  #
  errorMessage <- sidedObject[["error"]]
  
  if (!is.null(errorMessage)) {
    sidedObject[["posteriorLine"]] <- errorMessage
    return(sidedObject)
  }
  
  # Note(Alexander): Don't compute if it's too peaked
  #
  if (sidedObject[["tooPeaked"]]) {
    sidedObject[["posteriorLine"]] <- "Posterior is too peaked"
    return(sidedObject)
  }
  
  if (method=="pearson") {
    if (isSomeNA(bfObject[["betaA"]], bfObject[["betaB"]])) {
      sidedObject[["posteriorLine"]] <- "Posterior is too peaked"
      return(sidedObject)
    }
    
    # TODO(Alexander): Derive this for shifted h0/Find master student
    #
    sidedObject[["priorAtH0"]] <- .priorRho("rho"=sidedObject[["h0"]], "kappa"=bfObject[["kappa"]], alternative=alternative)
    sidedObject[["priorLine"]] <- .priorRho("rho"=xDomain, "kappa"=bfObject[["kappa"]], alternative=alternative)
    
    subCounter <- 1
    
    while (subCounter <= 3) {
      if (subCounter==1) {
        posteriorAtH0 <- .posteriorRho("bfObject"=bfObject, "rho"=sidedObject[["h0"]], "alternative"=alternative)
        posteriorLine <- .posteriorRho("bfObject"=bfObject, "rho"=xDomain, "alternative"=alternative)
      } else if (subCounter==2) {
        sidedObject[["approximation"]] <- "beta"
        posteriorAtH0 <- .posteriorRhoBetaApprox("rho"=sidedObject[["h0"]], "bfObject"=bfObject,
                                                 alternative=alternative)
        posteriorLine <- .posteriorRhoBetaApprox("rho"=xDomain, "bfObject"=bfObject,
                                                 alternative=alternative)
      } else if (subCounter==3) {
        sidedObject[["approximation"]] <- "fisher"
        posteriorAtH0 <- .posteriorRhoFisherApprox("bfObject"=bfObject, "rho"=sidedResult[["h0"]],
                                                   "alternative"=alternative)
        posteriorLine <- .posteriorRhoFisherApprox("bfObject"=bfObject, "rho"=xDomain,
                                                   "alternative"=alternative)
      }
      
      if (isSomeNA(posteriorLine) | any(posteriorLine < 0) | isSomeInfinite(posteriorLine)) {
        subCounter <- subCounter +1
      } else {
        sidedObject[["posteriorAtH0"]] <- posteriorAtH0
        sidedObject[["posteriorLine"]] <- posteriorLine
        break()
      }
    }
  } else if (method=="kendall") {
    sidedObject[["priorAtH0"]] <- .priorTau("tauPop"=sidedObject[["h0"]], "kappa"=bfObject[["kappa"]],
                                            "alternative"=alternative)
    sidedObject[["priorLine"]] <- .priorTau("tauPop"=xDomain, "kappa"=bfObject[["kappa"]], "alternative"=alternative)
    
    subCounter <- 1
    
    while (subCounter <= 1) {
      if (subCounter==1) {
        posteriorAtH0 <- .posteriorTau("tauPop"=sidedObject[["h0"]], "n"=sidedObject[["n"]],
                                       "tauObs"=sidedObject[["stat"]],"kappa"=bfObject[["kappa"]],
                                       "alternative"=alternative)
        posteriorLine <- .posteriorTau("tauPop"=xDomain, "n"=sidedObject[["n"]], "tauObs"=sidedObject[["stat"]],
                                       "kappa"=bfObject[["kappa"]], "alternative"=alternative)
      } else if (subCounter==2) {
        # TODO(Alexander): Perhaps add a Fisher approximation to this
      }
      
      if (isSomeNA(posteriorLine) | any(posteriorLine < 0) | isSomeInfinite(posteriorLine)) {
        subCounter <- subCounter +1
      } else {
        sidedObject[["posteriorAtH0"]] <- posteriorAtH0
        sidedObject[["posteriorLine"]] <- posteriorLine
        break()
      }
    }
  }
  
  if (isSomeNA(posteriorLine) | any(posteriorLine < 0) | isSomeInfinite(posteriorLine)){
    sidedObject[["posteriorLine"]] <- "Posterior is too peaked"
    return(sidedObject)
  }
  
  sidedObject[["yMax"]] <- max(sidedObject[["priorLine"]], sidedObject[["posteriorLine"]])
  sidedObject[["xDomain"]] <- xDomain
  return(sidedObject)
}

.computeCorSequentialLine <- function(x, y, bfObject, method="pearson") {
  # sidedObject <- .getSidedObject(bfObject, alternative=alternative)
  #
  error <- bfObject[["error"]]
  bf10 <- bfObject[["two.sided"]][["bf"]]
  
  if (bfObject[["two.sided"]][["tooPeaked"]] | bfObject[["greater"]][["tooPeaked"]] |
      bfObject[["less"]][["tooPeaked"]]) {
    error <- "Posterior is too peaked"
  }
  
  if (!is.null(error) | is.na(bf10)) {
    if (is.null(error)) {
      # Note(Alexander): This means that there's no error message
      error <- "Could not compute"
    }
    
    sideError <- list("sequentialLine"=error)
    result <- list("two.sided"=sideError, "greater"=sideError, "less"=sideError)
    return(result)
  }
  
  n <- bfObject[[1]][["n"]]
  
  compN <- 3:n
  nDomain <- c(1:2, compN)
  
  .calculateSequentialCor <- function(i, x, y, method) {
    return(tryOrFailWithNA(cor(x[1:i], y[1:i], use="pairwise.complete.obs", method=method)))
  }
  
  statSeq <- purrr::map_dbl(compN, .calculateSequentialCor, "x"=x, "y"=y, "method"=method)
  
  if (sum(is.na(statSeq)) >= 1) {
    sideError <- list("sequentialLine"="Could not compute")
    result <- list("two.sided"=sideError, "greater"=sideError, "less"=sideError)
  }
  
  h0 <- bfObject[["h0"]]
  kappa <- bfObject[["kappa"]]
  
  methodNumber <- bfObject[["methodNumber"]]
  
  placeHolder <- vector("numeric", length=length(nDomain))
  placeHolder[1] <- placeHolder[2] <- 1
  sideResult <- list("sequentialLine"=placeHolder)
  
  result <- list("two.sided"=sideResult, "greater"=sideResult, "less"=sideResult)
  
  if (method=="pearson") {
    .calculateSequentialBCorPearson <- function(n, r) {
      bfObject <- .computePearsonCorBf10(n, r, "h0"=h0, "kappa"=kappa,
                                         methodNumber=methodNumber)
      list(bfObject[["two.sided"]][["bf"]],
           bfObject[["greater"]][["bf"]],
           bfObject[["less"]][["bf"]]
      )
    }
    
    allBfs <- purrr::map2(compN, statSeq, .calculateSequentialBCorPearson)
    
    for (i in seq_along(allBfs)){
      result[[1]][[1]][i+2] <- allBfs[[i]][[1]]
      result[[2]][[1]][i+2] <- allBfs[[i]][[2]]
      result[[3]][[1]][i+2] <- allBfs[[i]][[3]]
    }
    
  } else if (method=="kendall") {
    .calculateSequentialBCorTau <- function(n, tauObs, alternative) {
      bfObject <- unlist(.bfKendallTauSavageDickey("n"=n, "tauObs"=tauObs, "kappa"=kappa,"h0"=h0, alternative=alternative)[["bf"]])
    }
    
    alternativeItems <- c("two.sided", "greater", "less")
    
    for (i in seq_along(alternativeItems)) {
      alternative <- alternativeItems[i]
      tempResult <- purrr::map2_dbl(compN, statSeq, .calculateSequentialBCorTau, alternative=alternative)
      result[[alternative]][[1]] <- c(1, 1, tempResult)
    }
  } else if (method=="spearman") {
    # TODO(Johnny)
  }
  
  for (j in 1:3) {
    if (isSomeInfinite(result[[j]][[1]])) {
      result[[j]][[1]] <- "Bayes factor hits infinity"
    }
    
    if (sum(is.na(result[[j]][[1]])) >= 1) {
      result[[j]][[1]] <- "Could not compute"
    }
  }
  result[["nDomain"]] <- nDomain
  return(result)
}

.computeCorRobustnessLine <- function(bfObject, method="pearson") {
  error <- bfObject[["error"]]
  bf10 <- bfObject[["two.sided"]][["bf"]]
  
  if (bfObject[["two.sided"]][["tooPeaked"]] | bfObject[["greater"]][["tooPeaked"]] |
      bfObject[["less"]][["tooPeaked"]]) {
    error <- "Posterior is too peaked"
  }
  
  if (!is.null(error) | is.na(bf10)) {
    if (is.null(error)) {
      # Note(Alexander): This means that there's no error message
      error <- "Could not compute"
    }
    
    sideError <- list("robustnessLine"=error)
    result <- list("two.sided"=sideError, "greater"=sideError, "less"=sideError)
    return(result)
  }
  
  n <- bfObject[["two.sided"]][["n"]]
  stat <- bfObject[["two.sided"]][["stat"]]
  h0 <- bfObject[["h0"]]
  methodNumber <- bfObject[["methodNumber"]]
  
  kappas <- .makeKappas(50)
  compKappas <- kappas[3:50]
  
  kappaDomain <- c(kappas[1:2], compKappas)
  
  placeHolder <- c(1, 1, vector("numeric", length=48))
  sideResult <- list("robustnessLine"=placeHolder)
  result <- list("two.sided"=sideResult, "greater"=sideResult, "less"=sideResult)
  
  if (method=="pearson") {
    .calculatePearsonRobustness <- function(kappa) {
      bfObject <- .computePearsonCorBf10("n"=n, "r"=stat, "h0"=h0, "kappa"=kappa, "methodNumber"=methodNumber)
      list(bfObject[["two.sided"]][["bf"]],
           bfObject[["greater"]][["bf"]],
           bfObject[["less"]][["bf"]])
    }
    
    allBfs <- purrr::map(compKappas, .f=.calculatePearsonRobustness)
    
    for (i in seq_len(48)){
      result[[1]][[1]][i+2] <- allBfs[[i]][[1]]
      result[[2]][[1]][i+2] <- allBfs[[i]][[2]]
      result[[3]][[1]][i+2] <- allBfs[[i]][[3]]
    }
  } else if (method=="kendall") {
    .calculateKendallRobustness <- function(kappa, alternative) {
      .bfKendallTauSavageDickey("n"=n, "tauObs"=stat, "kappa" = kappa, "h0"=0, "alternative"=alternative)[["bf"]]
    }
    
    alternativeItems <- c("two.sided", "greater", "less")
    
    for (i in seq_along(alternativeItems)) {
      alternative <- alternativeItems[i]
      tempResult <- purrr::map_dbl(compKappas, ".f"=.calculateKendallRobustness, "alternative"=alternative)
      result[[alternative]][[1]] <- c(1, 1, tempResult)
    }
  }
  
  for (j in 1:3) {
    if (isSomeInfinite(result[[j]][[1]])) {
      result[[j]][[1]] <- "Bayes factor hits infinity"
    }
    
    if (sum(is.na(result[[j]][[1]])) >= 1) {
      result[[j]][[1]] <- "Could not compute"
    } else {
      robustnessMaxBf <- max(result[[j]][[1]])
      robustnessKappaOfMaxBf <- kappaDomain[which.max(result[[j]][[1]])]
      result[[j]] <- modifyList(result[[j]],
                                list("robustnessMaxBf"=robustnessMaxBf,
                                     "robustnessKappaOfMaxBf"=robustnessKappaOfMaxBf))
    }
  }
  result[["kappaDomain"]] <- kappaDomain
  
  return(result)
}

# 1. Priors --------------------
.stretchedBeta <- function(rho, betaA, betaB) {
  result <- 1/2*dbeta((rho+1)/2, betaA, betaB)
  return(result)
}

# Wrapper for all priors on rho
.priorRho <- function(rho, kappa=1, alternative="two.sided") {
  if (alternative == "two.sided") {
    priorLine <- .stretchedBeta("rho"=rho, "betaA"=1/kappa, "betaB"=1/kappa)
  } else if (alternative == "greater") {
    priorLine <- .priorRhoPlus("rho"=rho, "kappa"=kappa)
  } else if (alternative =="less" ) {
    priorLine <- .priorRhoMin("rho"=rho, "kappa"=kappa)
  }
  return(priorLine)
}

.priorRhoPlus <- function(rho, kappa=1) {
  nonNegativeIndex <- rho >=0
  lessThanOneIndex <- rho <=1
  valueIndex <- as.logical(nonNegativeIndex*lessThanOneIndex)
  result <- rho*0
  result[valueIndex] <- 2*.priorRho(rho[valueIndex], kappa)
  return(result)
}

.priorRhoMin <- function(rho, kappa=1) {
  negativeIndex <- rho <=0
  greaterThanMinOneIndex <- rho >= -1
  valueIndex <- as.logical(negativeIndex*greaterThanMinOneIndex)
  result <- rho*0
  result[valueIndex] <- 2*.priorRho(rho[valueIndex], kappa)
  return(result)
}

# 2. Likelihood -------------
# These are the functions used for the likelihood
#
.aFunction <- function(n, r, rho) {
  hyperTerm <- Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=1/2, "z"=(r*rho)^2))
  result <- (1-rho^2)^((n-1)/2)*hyperTerm
  return(result)
}

.bFunction <- function(n, r, rho) {
  hyperTerm <- Re(hypergeo::f15.3.3("A"=n/2, "B"=n/2, "C"=3/2, "z"=(r*rho)^2))
  logTerm <- 2*(lgamma(n/2)-lgamma((n-1)/2))+((n-1)/2)*log(1-rho^2)
  result <- 2*r*rho*exp(logTerm)*hyperTerm
  return(result)
}

.hFunction <- function(n, r, rho) {
  result <- .aFunction("n"=n, "r"=r, rho) + .bFunction("n"=n, "r"=r, rho)
  return(result)
}

.hFunctionCombined <- function(nOri, rOri, nRep, rRep, rho) {
  result <- .hFunction(n=nOri, r=rOri, rho)*.hFunction(n=nRep, r=rRep, rho)
  return(result)
}

.hFunctionCombinedTwoSided <- function(nOri, rOri, nRep, rRep, rho) {
  result <- .aFunction(n=nOri, r=rOri, rho)*.aFunction(n=nRep, r=rRep, rho) +
    .bFunction(n=nOri, r=rOri, rho)*.bFunction(n=nRep, r=rRep, rho)
  return(result)
}

.hJeffreysApprox <- function(n, r, rho) {
  logResult <- ((n-1)/2)*log(1 - rho^2)-(n-3/2)*log(1 - rho*r)
  return(exp(logResult))
  # result <- ((1 - rho^(2))^(0.5*(n - 1)))/((1 - rho*r)^(n - 1 - 0.5))
  # return(result)
}

# 1.1 Explicit marginal likelihood functions
.m0MarginalLikelihood <- function(s, t, n) {
  logTerm <- 2*lgamma(0.5*(n-1))
  result <- 1/4*n^(0.5*(1-2*n))*pi^(1-n)*(s*t)^(1-n)*exp(logTerm)
  return(result)
}

.m1MarginalLikelihoodNoRho <- function(s, t, n, r, rho) {
  return(.m0MarginalLikelihood(s, t, n)*
           (.aFunction(n=n, r=r, rho)+.bFunction(n=n, r=r, rho)))
}


#' Function returns the p value from correlation.
#'
#' @param r numeric in (-1, 1) representing the sample correlation
#' @param n integer representing sample size
#' @param method character, "pearson", "kendall" or "spearman
#'
#' @return list of three p-values
#' @export
#'
#' @examples
.pValueFromCor <- function(n, stat, method=c("pearson","kendall", "spearman")) {
  sidedList <- list("p"=NA)
  result <- list("two.sided"=sidedList,
                 "greater"=sidedList,
                 "less"=sidedList)
  
  if (n <= 2){
    # result[["two.sided"]] <- sidedList
    # # tau < 0
    # result[["less"]] <- sidedList
    # # tau > 0
    # result[["greater"]] <- sidedList
    return(result)
  }
  
  if (method[1] == "pearson"){
    # Use t-distribution based on bivariate normal assumption using r to t transformation
    #
    df <- n - 2
    t <- stat*sqrt(df/(1-stat^2))
    result <- .pValueFromTNew("t"=t, "n1"=n-1, "n2"=0, var.equal=TRUE)
  } else if (method[1] == "kendall"){
    if (n > 2 && n < 50) {
      # Exact sampling distribution
      # tau neq 0
      result[["two.sided"]][["p"]] <- 1 - SuppDists::pKendall("q"=abs(stat), N=n) +
        SuppDists::pKendall("q"=-abs(stat), "N"=n)
      # tau < 0
      result[["less"]][["p"]] <- SuppDists::pKendall("q"=stat, "N"=n)
      # tau > 0
      result[["greater"]][["p"]] <- SuppDists::pKendall("q"=stat, "N"=n, "lower.tail" = FALSE)
    } else if (n >= 50){
      # normal approximation
      #
      someSd <- sqrt(2*(2*n+5)/(9*n*(n-1)))
      
      # tau neq 0
      result[["two.sided"]][["p"]] <- 2 * stats::pnorm(-abs(stat), "sd"=someSd)
      # tau < 0
      result[["less"]][["p"]] <- stats::pnorm(stat, "sd"=someSd)
      # tau > 0
      result[["greater"]][["p"]] <- stats::pnorm(stat, "sd"=someSd, lower.tail = FALSE)
    }
  } else if (method[1] == "spearman"){
    # TODO: Johnny
    # Without code this will print a NULL, if we go through here
  }
  return(result)
}


# TODO(Alexander): Unify ".pValueFromT", and check that this is not used in Alexandra's rewrite or somehwere else
# The important difference is that it now outputs a list
#
#' Function returns the p value from correlation.
#'
#' @param t numeric representing observed t-statistic
#' @param n1 integer representing sample size of the first sample
#' @param n2 integer representing sample size of the second sample, n2=0 implies that it's a one-sample
#' @param var.equal logical, TRUE is equal, cannot have unequal, because we don't have access to s1 and s2
#'
#' @return list of three p-values
#' @export
#'
#' @examples
.pValueFromTNew <- function(t, n1, n2 = 0, var.equal = TRUE) {
  # Function returns the p value from t statistic
  #
  # Args:
  #   t: t value input by user
  #   n1: sample size of group 1
  #   n2: sample size of group 2 (Note the hack by setting n2 = 0)
  #   var.equal: Note: always true: var.equal, we do not have enough info for different
  #              variances. In that case we also need s1 and s2
  #
  # Output:
  #   number in [0, 1] which is the p value
  sidedList <- list("p"=NA)
  result <- list("two.sided"=sidedList,
                 "greater"=sidedList,
                 "less"=sidedList)
  
  if (n2 > 0) {
    # If n2 > 0, then two-sample
    someDf <- n1 + n2 - 2
  } else {
    # If n2 <= 0, then one-sample
    someDf <- n1 - 1
  }
  # mu \neq 0
  result[["two.sided"]][["p"]] <- 2 * stats::pt(-abs(t), "df" = someDf)
  # mu < 0
  result[["less"]][["p"]] <- stats::pt(t, "df" = someDf)
  # mu > 0
  result[["greater"]][["p"]] <- stats::pt(t, "df" = someDf, "lower.tail" = FALSE)
  
  return(result)
}

# 3. Bayes factor
# These are the functions used to compute the Bayes factors
#

# 3.1 Two-sided main Bayes factor ----------------------------------------------
## Suit:
#' Title
#'
#'
#'#' Exact bf10, bfPlus0, bfMin0 based on the exact
#'
#'  for Pearson's correlation based on the exact reduced likelihood, see Ly et al. (2018).
#'
#' @param n integer representing sample size
#' @param r numeric in (-1, 1) sample Pearson correlation r
#' @param kappa numeric > 0 sample
#' @param h0 numeric in (-1, 1) which represents the test point of H0
#' @param methodNumber integer either 1, or 2. Methodnumber 1 uses the exact results of Ly et.al (2018),
#' whereas Methodnumber 2 uses the exact integral based on Jeffreys's approximation to the reduced likelihood.
#' @param hyperGeoOverFlowThreshold integer set to 24. Some experiments show that when log(bf10) > 24, that the
#' one-sided Bayes factors become instable [hypergeo v. 1.2-13]. For instance,
#'
#' myN <- 300
#' myR <- 0.42 # 0.415
#'
#' (bf10 <- .bf10Exact(n=myN, stat=myR))
#' (bfPlus0 <- .bf10Exact(n=myN, stat=myR) + .mPlusExact(n=myN, stat=myR))
#' (bfMin0 <- .bf10Exact(n=myN, stat=myR) + .mPlusExact(n=myN, stat=-myR))
#'
#' Changing myR <- 0.42. Shows that the result is okay
#'
#' @export
#' @return Returns bf10, bfPlus0, bfMin0 and the stretched beta fit of the posterior.
#'

#' Exact BF10 for Pearson's correlation based on the exact reduced likelihood, see Ly et al. (2018).
#'
#' @inheritParams bcor.test
#'
#' @export
#' @return Returns Bayes factor BF10 in favour of the alternative over the null
#'
.bf10Exact <- function(n, r, kappa=1, h0=0, oneThreshold=1e-3) {
  # Note (Alexander): Input check is done at a higher level: .computePearsonCorBf10
  # Maximum it can take is
  #     r=0.993, which works well up to n = 337, but r=0.992 and n=3 fails
  #     r=0.6, which works up to n=3201
  
  checkR <- (1 - abs(r) < oneThreshold)
  
  if (checkR) {
    if (kappa < 2/(n-2)) {
      result <- tryOrFailWithNA(
        exp(lbeta(1/kappa+(n-1)/2, 1/2) - lbeta(1/kappa, 1/2) +
              lgamma(1/kappa) + lgamma(1/kappa -n/2 + 1) - 2*lgamma(1/kappa+1/2))
      )
      return(result)
    } else {
      return(Inf)
    }
  }
  
  logHyperTerm <- tryOrFailWithNA(
    (1/kappa+1-n/2)*log(1-r^2) +
      log(Re(hypergeo::f15.1.1("A"=(1/kappa+1/2), "B"=(1/kappa+1/2), "C"=(1/kappa+n/2), "z"=r^2)))
  )
  
  if (is.na(logHyperTerm))
    return(NaN)
  
  logBetaTerms <- lbeta(1/kappa+(n-1)/2, 1/2)-lbeta(1/kappa, 1/2)
  
  logResult <- logBetaTerms + logHyperTerm
  realResult <- tryOrFailWithNA(exp(Re(logResult)))
  
  if (!is.numeric(realResult))
    return(NaN)
  
  # Failed
  if (realResult < 0)
    return(NaN)
  
  return(realResult)
}


# 2.2 Two-sided secondairy Bayes factor
#' BF10 for Pearson's correlation based on Jeffreys's approximation to the reduced likelihood,
#' see Jeffreys (1961, pp. 289-292).
#'
#' @inheritParams bcor.test
#'
#' @return Returns approximate Bayes factor BF10 in favour of the alternative over the null
.bf10JeffreysIntegrate <- function(n, r, kappa=1, h0=0) {
  # Note(Alexander): Input check is done at a higher level: .computePearsonCorBf10
  
  hyperTerm <- tryOrFailWithNA(Re(hypergeo::f15.3.3(A=(2*n-3)/4, B=(2*n-1)/4, C=(n+2/kappa)/2, z=r^2)))
  
  if (is.na(hyperTerm))
    return(NaN)
  
  logTerm <- lgamma((n+2/kappa-1)/2) - lgamma((n+2/kappa)/2) - lbeta(1/kappa, 1/kappa)
  result <- tryOrFailWithNA(sqrt(pi) * 2^(1-2/kappa) * exp(logTerm) * hyperTerm)
  
  
  if (!is.numeric(result))
    return(NaN)
  
  # Failed
  if (result < 0)
    return(NaN)
  
  return(result)
}

.computeBCorOneSided <- function(bf10, n, r, kappa, methodNumber, betaA, betaB,
                                 hyperGeoOverFlowThreshold=25) {
  failSided <- list("bf"=NA, "tooPeaked"=TRUE)
  result <- list("two.sided"=list("bf"=bf10),
                 "greater"=failSided,
                 "less"=failSided)
  
  if (methodNumber %in% 1:2) {
    if (log(bf10) < hyperGeoOverFlowThreshold) {
      # Exact method doesn't work well for log(bf10) > 25
      subCounter <- 1L
    } else {
      subCounter <- 2L
    }
  } else {
    subCounter <- 3L
  }
  
  while (subCounter <= 3) {
    if (subCounter==1L) {
      # (a) Try exact calculations:
      #
      if (methodNumber==1L) {
        bfPlus0 <- tryOrFailWithNA(bf10 + .mPlusExact(n=n, r=r, kappa))
        bfMin0 <- tryOrFailWithNA(bf10 + .mPlusExact(n=n, r=-r, kappa))
      } else if (methodNumber==2L) {
        bfPlus0 <- tryOrFailWithNA(bf10 + .mPlusJeffreysIntegrate(n=n, r=r, kappa=kappa))
        bfMin0 <- tryOrFailWithNA(bf10 + .mPlusJeffreysIntegrate(n=n, r=-r, kappa=kappa))
      }
    } else if (subCounter==2L) {
      # (b) Compute numerical
      #
      plusSidedIntegrand <- function(x){.hJeffreysApprox(n=n, r=r, x)*.priorRhoPlus(x, kappa=kappa)}
      minSidedIntegrand <- function(x){.hJeffreysApprox(n=n, r=r, x)*.priorRhoMin(x, kappa=kappa)}
      
      bfPlus0 <- tryOrFailWithNA(bf10 + integrate(plusSidedIntegrand, 0, 1)[["value"]])
      bfMin0 <- tryOrFailWithNA(bf10 + integrate(minSidedIntegrand, -1, 0)[["value"]])
    } else if (subCounter==3L) {
      if (!isSomeNA(betaA, betaB)) {
        # Compute bfPlus0 and bfMin0 based on the posterior mass using the found bf10
        #
        tempList <- .computeBCorOneSidedSavageDickey("bf10"=bf10, "betaA"=betaA, "betaB"=betaB,
                                                     "h0"=h0, "kappa"=kappa)
        bfPlus0 <- tempList[["bfPlus0"]]
        bfMin0 <- tempList[["bfMin0"]]
      } else {
        return(result)
      }
    }
    
    if (!isSomeNA(bfPlus0, bfMin0) & isEveryFinite(bfPlus0, bfMin0) &
        bfPlus0 >= 0 & bfMin0 >= 0) {
      # Result seem good, do consistency check
      if (!(bfPlus0 > 1 & bfMin0 > 1) & bfPlus0 > 0 & bfMin0 > 0 | subCounter==3) {
        tempList <- list("greater"=list("bf"=bfPlus0, "tooPeaked"=FALSE),
                         "less"=list("bf"=bfMin0, "tooPeaked"=FALSE))
        result <- modifyList(result, tempList)
        return(result)
      }
    }
    subCounter <- subCounter + 1
  }
  return(result)
}


.computeBCorOneSidedSavageDickey <- function(bf10, betaA, betaB, h0=0, kappa=1) {
  result <- list(bf10=bf10, bfPlus0=NA, bfMin0=NA)
  
  if (is.finite(bf10)) {
    # bf10 is finite, now calculate one-sided stuff
    #
    rightProportion <- NA
    leftProportion <- tryOrFailWithNA(stats::pbeta(1/2, shape1=betaA, shape2=betaB))
    
    if (is.na(leftProportion)) {
      return(result)
    }
    
    if (leftProportion > 0 & leftProportion < 1) {
      bfPlus0 <- 2*bf10*(1-leftProportion)
      bfMin0 <- 2*bf10*leftProportion
    } else if (leftProportion >= 1) {
      bfPlus0 <- 10^(-317)
      bfMin0 <- 2*bf10
    } else {
      rightProportion <- tryOrFailWithNA(stats::pbeta(1/2, "shape1"=betaA, "shape2"=betaB, "lower.tail"=FALSE))
      
      if (is.na(rightProportion)) {
        return(result)
      } else if (rightProportion > 0 & rightProportion < 1) {
        bfPlus0 <- 2*bf10*rightProportion
        bfMin0 <- 2*bf10*(1-rightProportion)
      } else if (rightProportion >= 1) {
        bfPlus0 <- 2*bf10
        bfMin0 <- 10^(-317)
      }
    }
    
    # Note(Alexander): Consistency check
    #
    if (bfPlus0 > 1 & bfMin0 > 1) {
      if (leftProportion > 0.5) {
        # Note(Alexander): The problem is machine precision here,
        # because there aren't enough significant figures in leftProportion to have
        # 2 * bf10 (1-leftProportion) < 1
        #
        #  Alternatively, we can change bf10 to the Savage-Dickey approximation
        #
        bfMin0 <- 2*leftProportion*bf10
        bfPlus0 <- 2*(1-leftProportion)
      } else {
        bfMin0 <- 2*leftProportion
        bfPlus0 <- 2*(1-leftProportion)*bf10
      }
    } else if (bfPlus0 < 0 | bfMin0 < 0) {
      return(result)
    }
    
    #
    result <- list(bf10=bf10, bfPlus0=bfPlus0, bfMin0=bfMin0)
  }
  # return default results
  return(result)
}

# 2.4 The Marsman MH sampler

.logTarget <- function(rho, n, r, kappa=1) {
  # More correctly, with rho=rhoProp, this is upper term of the acceptance ratio.
  # Recall that in the numerator and denominator of the acceptance ratio are given by
  #
  #   likelihood(rhoProp)*prior(rhoProp)*proposal(rhoCurrent | rhoProp)
  #   likelihood(rhoCurrent)*prior(rhoCurrent)*proposal(rhoProp | rhoCurrent)
  #
  # respectively, see Robert (2015). As the proposal is defined on Fisher transform atanh(rho),
  # we have a Jocobian. The Jacobian in the upper part is (1-rhoCurrent^2)^(-1) and in the lower part
  # is (1-rhoProp^2)^(-1).
  # Note that since prior(rhoProp) \propto (1-rhoProp^2)^(alpha-1), we can drop the one due
  # to the Jacobian term of the denominator ending up in the numerator.
  #
  # For the likelihood we use Jeffreys's approximation
  #   rho \propto (1-rho^2)^((n-1)/2)*(1-rho*r)^((3-2*n)/2)
  #
  # The log prior is (alpha-1)*log(1-rho^2) # where alpha=1/kappa
  #
  return((1/kappa+(n-1)/2)*log(1-rho^2)-(2*n-3)/2*log(1-rho*r))
}


.logProposal <- function(rho, n, r, z=NULL) {
  # The proposal is defined on Fisher hyperbolic tangent transform of rho,
  # the Jacobian is absorbed in .logTarget
  if (!is.null(z)) {
    return(-(n-3)/2*(z-atanh(r))^2)
  } else {
    return(-(n-3)/2*(atanh(rho)-atanh(r))^2)
  }
}

#' Fits a beta density to the the posterior samples based on an independent Metropolis sampler, see Tierney (1994).
#'
#' @inheritParams bcor.test
#'
#' @return Returns a list with the beta parameters of a stretched beta distribution on (-1, 1) and the acceptance rate.
#' @export
#'
.marsmanMHSampler <- function(n, r, kappa=1, nIters=50000L) {
  rhoMetropolisChain <- numeric(nIters)
  
  if (n <= 3) {
    std <- 1
  } else {
    std <- 1 / sqrt(n - 3)
  }
  
  zCandidates <- rnorm(nIters, "mean"=atanh(r), "sd"=std)
  rhoCandidates <- tanh(zCandidates)
  logTargetCandidates <- .logTarget("rho"=rhoCandidates, "n"=n, "r"=r, "kappa"=kappa)
  logPropCandidates <- .logProposal("z"=zCandidates, "n"=n, "r"=r)
  acceptMechanism <- runif(nIters)
  candidateAcceptance <- numeric(nIters)
  
  rhoCurrent <- r
  
  for (iter in 1:nIters) {
    zCurrent <- atanh(rhoCurrent)
    
    candidateAcceptance[iter] <- logTargetCandidates[iter]+.logProposal("z"=zCurrent, "n"=n, "r"=r)-
      (.logTarget("rho"=rhoCurrent, "n"=n, "r"=r, "kappa"=kappa)+logPropCandidates[iter])
    
    # Accept candidate and update rhoCurrent for next iteration
    if (log(acceptMechanism[iter]) <= candidateAcceptance[iter])
      rhoCurrent <- rhoCandidates[iter]
    
    rhoMetropolisChain[iter] <- rhoCurrent
  }
  
  acceptanceRate <- length(unique(rhoMetropolisChain))/nIters
  
  metropolisVar <- var(rhoMetropolisChain)/2^2
  metropolisMean <- mean((rhoMetropolisChain+1)/2)
  
  mhFit <- .betaParameterEstimates(metropolisMean, metropolisVar)
  mhFit[["acceptanceRate"]] <- acceptanceRate
  return(mhFit)
}


# 3.2 One-sided preparation ----------------------------------------------------

#' Add this to the exact two-sided Bayes factor to get bfPlus0
#'
#' @inherit .bf10Exact
.mPlusExact <- function(n, r, kappa=1) {
  # Ly et al 2015
  # This is the contribution of one-sided test
  #
  # Note(Alexander): Input check is done at a higher level: .computePearsonCorBf10.
  # In particular the case with n <= 2
  #
  # TODO(Alexander): In hindsight, I'm not so happy with this version, due to instability of 3F2.
  # Try to simplify this expression
  #
  hyperTerm <- Re(hypergeo::genhypergeo(U=c(1, n/2, n/2), L=c(3/2, (2+kappa*(n+1))/(2*kappa)), z=r^2, maxiter=1e5))
  logTerm <- 2*(lgamma(n/2)-lgamma((n-1)/2))-lbeta(1/kappa, 1/kappa)
  result <- 2^((3*kappa-2)/kappa)*kappa*r/(2+(n-1)*kappa)*exp(logTerm)*hyperTerm
  return(result)
}


#' Add this to the semi-exact two-sided Bayes factor to get bfPlus0
#'
#' @inherit .bf10Exact
.mPlusJeffreysIntegrate <- function(n, r, kappa=1, ...) {
  # Ly et al 2015
  # This is the exact result with symmetric beta prior on rho
  # This is the contribution of one-sided test
  #
  # Note (Alexander): Input check is done at a higher level: .computePearsonCorBf10.
  # In particular the case with n <= 2
  #
  #
  hyperTerm <- Re(
    hypergeo::genhypergeo(U=c(1, (2*n-1)/4, (2*n+1)/4),L=c(3/2, (n+1+2/kappa)/2), z=r^2, ...)
  )
  logTerm <- -lbeta(1/kappa, 1/kappa)
  result <- 2^(1-2/kappa)*r*(2*n-3)/(n+2/kappa-1)*exp(logTerm)*hyperTerm
  return(result)
}

#' Compute Bayes factors bf10, bfPlus0, bfMin0 based on the default stretched beta prior on (-1, 1) for
#' Pearson's correlation coefficient rho
#'
#' @param n numeric > 0, number of samples
#' @param r numeric in (-1, 1), the observed Pearson correlation r in the sample
#' @param h0 numeric in (-1, 1), the hypothesised null value
#' @param kappa numeric > 0, the scale of the beta distribution, i.e., beta(1/kappa, 1/kappa)
#' @param ciValue numeric in (0, 1), the credible value
#' @param hyperGeoOverFlowThreshold numeric > 0, the threshold function for which some computations for which the
#' function genhypergeo in hypergeo [version 1.2-13] used for the one-sided Bayes factors lead to some instablish
#' results.
#' @param methodNumber numeric in reference to the computation method used to calculate Bayes factors: (1) Exact
#' results, see Ly et al. (2018) [when log(bf10) > hyperGeoOverFlowThreshold, the one-sided Bayes factors are
#' calculated using a numerical integrator, or a Savage Dickey method redistribution], (2) Semi-exact,
#' see Wagenmakers et al. (2015), (3) Savage-Dickey: First fit a stretched beta to the posterior based on the exact
#' posterior mean and variance, then compute the ratio of prior to posterior at the restriction point, that is, h0
#' (4) First generate posterior samples using Marsman's IMH sampler, which is the used to fit a stretched beta and
#' again the ratio of prior and posterior is used to compute bf10, (5) use Jeffreys's approximation to bf10 based on
#' kappa=1
#' @param oneThreshold numeric > 0, used to determine when abs(r) is considered indistinguishable from one.
#'
#' @return Returns a list with bf10, bfPlus0, bfMin0, whether the result is plottable, the credible interval.
#' @export
#'
#' @examples
.computePearsonCorBf10 <- function(n, r, h0=0, kappa=1, ciValue=0.95, hyperGeoOverFlowThreshold=25,
                                   methodNumber=1L, oneThreshold=1e-3) {
  #
  sidedResult <- list("n"=n, "stat"=r, "bf"=NA, "tooPeaked"=NA,
                      "lowerCi"=NA, "upperCi"=NA, "posteriorMedian"=NA)
  
  result <- list("two.sided"=sidedResult,
                 "less"=sidedResult,
                 "greater"=sidedResult,
                 "kappa"=kappa, "ciValue"=ciValue, "acceptanceRate"=1, "h0"=h0,
                 "methodNumber"=methodNumber, "call"=match.call(), "betaA"=NA, "betaB"=NA
  )
  
  failedSidedResult <- list("n"=n, "stat"=NaN, "bf"=NA, "tooPeaked"=TRUE,
                            "ciValue"=ciValue, "lowerCi"=NA, "upperCi"=NA, "posteriorMedian"=NA)
  
  failedResult <- list("two.sided"=failedSidedResult,
                       "less"=failedSidedResult,
                       "greater"=failedSidedResult,
                       "kappa"=kappa, "ciValue"=ciValue, "acceptanceRate"=1,
                       "methodNumber"=6, "call"=match.call(), "betaA"=NA, "betaB"=NA
  )
  
  # When the prior is trivial (null is alternative) or when the data is predictively matched
  #
  predictiveMatchingList <- list("two.sided"=list("bf"=1, "tooPeaked"=FALSE),
                                 "greater"=list("bf"=1, "tooPeaked"=FALSE),
                                 "less"=list("bf"=1, "tooPeaked"=FALSE),
                                 "methodNumber"=0)
  
  # Information consistent result
  #
  plusSidedInfList <- list("two.sided"=list("bf"=Inf, "tooPeaked"=TRUE, "posteriorMedian"=r),
                           "less"=list("bf"=0, "tooPeaked"=TRUE, "posteriorMedian"=r),
                           "greater"=list("bf"=Inf, "tooPeaked"=TRUE)
  )
  
  minSidedInfList <- list("two.sided"=list("bf"=Inf, "tooPeaked"=TRUE, "posteriorMedian"=r),
                          "less"=list("bf"=Inf, "tooPeaked"=TRUE),
                          "greater"=list("bf"=0, "tooPeaked"=TRUE, "posteriorMedian"=r)
  )
  
  # Note: If log(bf10) then use beta fit for the one sided bfs
  # The 100L is randomlyish chosen based on n=3000, r=-0.3500786
  # hyperGeoOverFlowThreshold <- 100L
  
  checkTypeInput <- !isEveryNumeric(r, kappa, n)
  
  if (checkTypeInput) {
    result <- failedResult
    errorMessage <- "Input error: the sample size n, the summary statistic stat, or kappa are not numeric"
    result[["error"]] <- errorMessage
    return(result)
  }
  
  checkData <- failIfNot(abs(r) <= 1, n >= 0, kappa > 0)
  
  if (!is.null(checkData)) {
    result <- failedResult
    result[["error"]] <- checkData
    return(result)
  }
  
  # Note: Data: OK
  # "No" prior, alternative model is the same as the null model
  # The bound kappa=0.002 is chosen arbitrarily I should choose this based on a trade off
  # between r and n, but it doesn't really matter.
  if (kappa <= 0.002 || n <= 2) {
    result <- modifyList(result, predictiveMatchingList)
    return(result)
  }
  
  checkR <- (1 - abs(r) < oneThreshold) # check whether 1 - |r| < oneThreshold
  
  if (n <= 2 || kappa==0) {
    result <- modifyList(result, predictiveMatchingList)
    return(result)
  } else if (checkR & kappa >= 2/(n-2)) {
    if (r > 0) {
      result <- modifyList(result, plusSidedInfList)
    } else if (r <= 0) {
      result <- modifyList(result, minSidedInfList)
    }
    return(result)
  }
  
  # Note(Alexander): Add stretched beta fitted posterior
  #
  fit <- tryOrFailWithNA(.posteriorBetaParameters(n=n, r=r, kappa=kappa))
  
  # Note(Alexander): Here compute the two sided bf
  #
  while (methodNumber <= 5) {
    # Note: Try all normal methods
    # 1. Exact Ly et al (2018)
    # 2. Exact with Jeffreys's (1961) approximation to the reduced likelihood
    # 3. Savage-Dickey: Via stretched beta fitted to the posterior moments
    # 4. Savage-Dickey: Via Marsman sampler and a stretched beta
    # 5. Jeffreys's approximation to the Bayes factor, only works for h0=0 though
    #     TODO(Alexander): Approximate for h0 \neq 0
    #
    if (methodNumber == 1) {
      bf10 <-  tryOrFailWithNA(.bf10Exact(n=n, r=r, kappa=kappa, h0=h0, oneThreshold=oneThreshold))
    } else if (methodNumber == 2) {
      bf10 <- tryOrFailWithNA(.bf10JeffreysIntegrate(n=n, r=r, kappa=kappa, h0=h0))
    } else if (methodNumber %in% 3:4) {
      if (methodNumber==4) {
        fit <- .marsmanMHSampler(n=n, r=r, kappa=kappa)
        result[["acceptanceRate"]] <- fit[["acceptanceRate"]]
      }
      
      priorAtH0 <- tryOrFailWithNA(.priorRho(rho=h0, kappa=kappa))
      posteriorAtH0 <- tryOrFailWithNA(.stretchedBeta(rho=h0, betaA=fit[["betaA"]], betaB=fit[["betaB"]]))
      
      bf10 <- tryOrFailWithNA(priorAtH0/posteriorAtH0)
    } else if (methodNumber == 5) {
      bf10 <- tryOrFailWithNA(
        ((2*n-3)/pi)^(-0.5)*(1-r^2)^((4-n)/2)
      )
      
      # Note(Alexander): So the methodNumber == 5 is retained
      result[["methodNumber"]] <- 5
      break()
    }
    
    if (!is.na(bf10) & is.finite(bf10)) {
      result[["two.sided"]][["bf"]] <- bf10
      result[["two.sided"]][["tooPeaked"]] <- FALSE
      result[["methodNumber"]] <- methodNumber
      break()
    }
    methodNumber <- methodNumber+1
  }
  
  if (is.na(bf10) || bf10 < 0) {
    # Total failure; it's true
    result <- failedResult
    return(result)
  }
  
  # Note(Alexander): Here compute one-sided bfs
  #
  if (is.infinite(bf10)) {
    if (r >= 0) {
      result <- modifyList(result, plusSidedInfList)
      return(result)
    } else {
      result <- modifyList(result, minSidedInfList)
      return(result)
    }
  } else if (is.finite(bf10)) {
    tempList <- tryOrFailWithNA(
      .computeBCorOneSided("bf10"=bf10, "n"=n, "r"=r, "kappa"=kappa,
                           "methodNumber"=methodNumber, "betaA"=fit[["betaA"]], "betaB"=fit[["betaB"]],
                           "hyperGeoOverFlowThreshold" = hyperGeoOverFlowThreshold)
    )
    
    if (isSomeNA(tempList)) {
      # result <- failedResult
      errorMessage <- "Can't compute one-sided Bayes factors"
      result[["error"]] <- errorMessage
      # return(result)
    }
    
    result <- modifyList(result, tempList)
  }
  
  if (!isSomeNA(fit)) {
    tempList <- .computePearsonCredibleInterval("betaA"=fit[["betaA"]], "betaB"=fit[["betaB"]],
                                                "ciValue"=ciValue)
    
    # Add list $two.sided$ci, $plusSided$ci, $less$ci, ciValue
    result <- modifyList(result, tempList)
  } else {
    tempFailedCiResult <- list("tooPeaked"=TRUE)
    failedCiResult <- list("two.sided"=tempFailedCiResult, "greater"=tempFailedCiResult, "less"=tempFailedCiResult)
    result <- modifyList(result, failedCiResult)
    
    # TODO(Alexander): Consider the case that everything can be computed, except for the cis, then get error.
    # Most of this is covered using Gauss' Vandermonde Identity
    result[["error"]] <- "Can't compute credible intervals"
  }
  return(result)
}


# 4. Posteriors ------------
#


# 4.1 Two-sided
#'
#' @inheritParams .computePearsonCorBf10
#'
#' @return Returns the posterior density
#' @export
#'
#' @examples
.posteriorRho <- function(bfObject, rho, alternative="two.sided") {
  sidedObject <- .getSidedObject(bfObject, "alternative"=alternative, itemNames=c("kappa"))
  
  bf <- sidedObject[["bf"]]
  n <- sidedObject[["n"]]
  r <- sidedObject[["stat"]]
  kappa <- sidedObject[["kappa"]]
  
  if (alternative=="two.sided") {
    return(1/bf*.hFunction("n"=n, "r"=r, rho)*.priorRho(rho, kappa))
  } else if (alternative=="greater") {
    return(1/bf*.hFunction("n"=n, "r"=r, rho)*.priorRhoPlus(rho, kappa))
  } else if (alternative=="less") {
    return(1/bf*.hFunction("n"=n, "r"=r, rho)*.priorRhoMin(rho, kappa))
  }
}

# 4.1 Two-sided
#' ASDF
#'
#' @inheritParams .computePearsonCorBf10
#'
#' @return Returns the posterior density
#' @export
#'
#' @examples
.posteriorRhoStat <- function(n, r, rho, kappa=1, alternative="two.sided") {
  if (alternative=="two.sided") {
    if (!is.na(r)) {
      return(1/.bf10Exact("n"=n, "r"=r, "kappa"=kappa)*.hFunction("n"=n, "r"=r, rho)*.priorRho(rho, kappa))
    } else if (!is.na(r) && r==0) {
      return(1/.bf10JeffreysIntegrate(n=n, "r"=r, kappa)*.hJeffreysApprox(n=n, "r"=r, rho)*.priorRho(rho, kappa))
    }
  } else if (alternative=="greater") {
    return(.posteriorRhoPlusStat("n"=n, "r"=r, "rho"=rho, "kappa"=kappa))
  } else if (alternative=="less") {
    return(.posteriorRhoMinStat("n"=n, "r"=r, "rho"=rho, "kappa"=kappa))
  }
}

.posteriorRhoBetaApprox <- function(bfObject, rho, alternative="two.sided") {
  if (alternative == "two.sided") {
    posteriorLine <- .stretchedBeta(rho, "betaA"=bfObject[["betaA"]], betaB=bfObject[["betaB"]])
  } else if (alternative == "greater") {
    posteriorLine <- .stretchedBeta(rho, "betaA"=bfObject[["betaA"]], betaB=bfObject[["betaB"]]) /
      pbeta("q"=1/2,  "shape1"=bfObject[["betaA"]], "shape2"=bfObject[["betaB"]], "lower.tail"=FALSE)
    posteriorLine[rho < 0] <- 0
  } else if (alternative == "less") {
    posteriorLine <- .stretchedBeta(rho, "betaA"=bfObject[["betaA"]], "betaB"=bfObject[["betaB"]]) /
      pbeta("q"=1/2,  "shape1"=bfObject[["betaA"]], "shape2"=bfObject[["betaB"]], "lower.tail"=TRUE)
    posteriorLine[rho > 0] <- 0
  }
  return(posteriorLine)
}

.posteriorRhoBetaApproxStat <- function(rho, betaA, betaB, alternative="two.sided") {
  if (alternative == "two.sided") {
    posteriorLine <- .stretchedBeta(rho, betaA=betaA, betaB=betaB)
  } else if (alternative == "greater") {
    posteriorLine <- .stretchedBeta(rho, betaA=betaA, betaB=betaB) /
      pbeta(q=1/2,  shape1=betaA, shape2=betaB, lower.tail=FALSE)
    posteriorLine[rho < 0] <- 0
  } else if (alternative == "less") {
    posteriorLine <- .stretchedBeta(rho, betaA=betaA, betaB=betaB) /
      pbeta(q=1/2,  shape1=betaA, shape2=betaB, lower.tail=TRUE)
    posteriorLine[rho > 0] <- 0
  }
  return(posteriorLine)
}

.posteriorRhoPlusStat <- function(n, r, rho, kappa=1) {
  if (!is.na(r)) {
    return(1/.computePearsonCorBf10(n=n, "r"=r, kappa=kappa, methodNumber=1)[["greater"]][["bf"]]
           *.hFunction(n=n, "r"=r, rho)*.priorRhoPlus(rho, kappa))
  } else if (!is.na(r) && r==0) {
    return(1/.computePearsonCorBf10(n=n, "r"=r, kappa=kappa, methodNumber=2)[["greater"]][["bf"]]
           *.hJeffreysApprox(n=n, "r"=r, rho)*.priorRhoPlus(rho, kappa))
  }
}

.posteriorRhoMinStat <- function(n, r, rho, kappa=1) {
  if (!is.na(r)) {
    return(1/.computePearsonCorBf10("n"=n, "r"=r, "kappa"=kappa, methodNumber=1)[["less"]][["bf"]]
           *.hFunction("n"=n, "r"=r, rho)*.priorRhoMin(rho, kappa))
  } else if (!is.na(r) && r==0) {
    return(1/.computePearsonCorBf10(n=n, "r"=r, kappa=kappa, methodNumber=2)[["less"]][["bf"]]
           *.hJeffreysApprox(n=n, "r"=r, rho)*.priorRhoMin(rho, kappa))
  }
}

.posteriorRhoFisherApprox <- function(bfObject, rho, alternative="two.sided") {
  sidedObject <- bfObject[[alternative]]
  n <- sidedObject[["n"]]
  r <- sidedObject[["stat"]]
  
  if (alternative=="two.sided") {
    return(.approximatePosteriorRho("rho"=rho, "n"=n, "r"=r))
  } else if (alternative=="greater") {
    return(.approximatePosteriorRhoPlus("rho"=rho, "n"=n, "r"=r))
  } else if (alternative=="less") {
    return(.approximatePosteriorRhoMin("rho"=rho, "n"=n, "r"=r))
  }
}

.approximatePosteriorRho <- function(rho, n, r) {
  if (n <= 3) {
    std <- 1
  } else {
    std <- 1 / sqrt(n - 3)
  }
  return(1/(1-rho^2)*stats::dnorm(atanh(rho), mean=atanh(r), sd=std))
}

.approximatePosteriorRhoPlus <- function(rho, n, r) {
  if (n <= 3) {
    std <- 1
  } else {
    std <- 1 / sqrt(n - 3)
  }
  result <- (.approximatePosteriorRho(rho, n, r) * (rho > 0)) /
    (stats::pnorm(0, "mean"=atanh(r), "sd"=std, "lower.tail"=FALSE))
  return(result)
}

.approximatePosteriorRhoMin <- function(rho, n, r) {
  if (n <= 3) {
    std <- 1
  } else {
    std <- 1 / sqrt(n - 3)
  }
  result <- (.approximatePosteriorRho(rho, n, r) * (rho<0)) /
    (stats::pnorm(0, "mean"=atanh(r), "sd"=std))
  return(result)
}


# 4.2
.posteriorMean <- function(n, r, kappa=1, old2F1=FALSE, oneThreshold=1e-3) {
  # NEW CODE CAN OFFICIALLY DO .posteriorMean(1219, 0.83)
  #
  checkR <- (1 - abs(r) < oneThreshold)
  
  if (checkR) {
    if (kappa < 2/(n-2)) {
      hypRatio <- exp(
        lgamma(1/kappa+1+n/2)+lgamma(1/kappa+1-n/2) - 2*lgamma(1/kappa+1) -
          (lgamma(1/kappa+n/2) + lgamma(1/kappa-n/2+1) - 2*lgamma(1/kappa+1/2))
      )
    } else {
      # Note(Alexander): Hack due to consistency and posterior mean -> mle, because n grows or even r to 1
      return(r)
    }
  } else {
    logHyperTerm1 <- tryOrFailWithNA(
      # Re(hypergeo::f15.3.3("A"=n/2, "B"=n/2, "C"=(2+(n+2)*kappa)/(2*kappa), "z"=r^2))
      # Re(hypergeo::f15.3.3("A"=n/2, "B"=n/2, "C"=1/kappa+1+n/2, "z"=r^2))
      log(Re(hypergeo::f15.1.1("A"=1+1/kappa, "B"=1+1/kappa, "C"=1/kappa+1+n/2, "z"=r^2)))
    )
    logHyperTerm2 <- tryOrFailWithNA(
      # Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=(2+n*kappa)/(2*kappa), "z"=r^2))
      # Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=1/kappa+n/2, "z"=r^2))
      log(Re(hypergeo::f15.1.1("A"=1/kappa+1/2, "B"=1/kappa+1/2, "C"=1/kappa+n/2, "z"=r^2)))
    )
    hypRatio <- tryOrFailWithNA(exp(logHyperTerm1-logHyperTerm2))
  }
  
  # Note(Alexander): that this is a bit of a hack here, as but correct due to large samples.
  #
  if (is.na(hypRatio)) {
    return(r)
  }
  
  logW <-  2*(lgamma(n/2)-lgamma((n-1)/2))
  result <- (2*kappa*r)/(2+n*kappa)*exp(logW)*hypRatio
  return(result)
  
  # Old code can officially do .posteriorMean(1216, 0.83)
  #
  # hyperTerm1 <- tryOrFailWithNA(
  #   Re(hypergeo::genhypergeo(U=c(n/2, n/2), L=c((2+(n+2)*kappa)/(2*kappa)), z=r^2))
  # )
  # hyperTerm2 <- tryOrFailWithNA(
  #   Re(hypergeo::genhypergeo(U=c((n-1)/2, (n-1)/2), L=c((2+n*kappa)/(2*kappa)), z=r^2))
  # )
}

.posteriorSecondMoment <- function(n, r, kappa=1, oneThreshold=1e-3) {
  # New code can do:.PosteriorSecondMoment(1219, 0.83) n=3 more than old code
  #
  #
  checkR <- (1 - abs(r) < oneThreshold)
  
  if (checkR) {
    if (kappa < 2/(n-2)) {
      logHypTerm1a <- lgamma(1/kappa+1+n/2) + lgamma(1/kappa - n/2 + 2) - 2*lgamma(1/kappa + 3/2)
      logHypTerm1b <- lgamma(n/2+1/kappa+2) + lgamma(1/kappa - n/2 + 1) - 2*lgamma(1/kappa + 3/2)
      logHypTerm2 <- lgamma(1/kappa+n/2) + lgamma(1/kappa-n/2+1) - 2*lgamma(1/kappa+1/2)
      
      hypRatioA <- tryOrFailWithNA(exp(logHypTerm1a-logHypTerm2))
      hypRatioB <- tryOrFailWithNA(exp(logHypTerm1b-logHypTerm2))
    } else {
      # Note(Alexander): Quite the hack here. Note that this is the upper bound (Jensen)
      return(r^2)
    }
  } else {
    hyperTerm1a <- tryOrFailWithNA(
      # Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=(2+(n+2)*kappa)/(2*kappa), "z"=r^2))
      # Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=1/kappa+1+n/2, "z"=r^2))
      log(Re(hypergeo::f15.1.1("A"=(1/kappa+3/2), "B"=(1/kappa+3/2), "C"=(n/2+1/kappa+1), "z"=r^2)))
    )
    hyperTerm1b <- tryOrFailWithNA(
      # Re(hypergeo::f15.3.3("A"=(n+1)/2, "B"=(n+1)/2, "C"=(2+(n+2)*kappa)/(2*kappa)+1, "z"=r^2))
      # Re(hypergeo::f15.3.3("A"=(n+1)/2, "B"=(n+1)/2, "C"=(n/2+1/kappa+2), "z"=r^2))
      Re(hypergeo::f15.1.1("A"=(1/kappa+3/2), "B"=(1/kappa+3/2), "C"=n/2+1/kappa+2, "z"=r^2))
    )
    hyperTerm2 <- tryOrFailWithNA(
      # Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=(2+n*kappa)/(2*kappa), "z"=r^2))
      # Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=1/kappa+n/2, "z"=r^2))
      log(Re(hypergeo::f15.1.1("A"=(1/kappa+1/2), "B"=(1/kappa+1/2), "C"=(1/kappa+n/2), "z"=r^2)))
    )
    
    hypRatioA <- exp(log(1-r^2)+hyperTerm1a-hyperTerm2)
    hypRatioB <- hyperTerm1b/exp(hyperTerm2)
  }
  
  # TODO(Alexander): Add asymptotic approximation here
  #
  result <- tryOrFailWithNA(
    kappa/(n*kappa+2) * (hypRatioA+ kappa*(n-1)^(2)/(2+(n+2)*kappa)*r^2*hypRatioB)
  )
  
  if (is.na(result))
    return(r^2)
  
  return(result)
  
  # OLD CODE CAN DO:.posteriorSecondMoment(1204, 0.83), at 1205 get inf
  #
  # hyperTerm1 <- tryOrFailWithNA(
  #   Re(hypergeo::genhypergeo(U=c(3/2, (n-1)/2, (n-1)/2),
  #                            L=c(1/2, (2+(n+2)*kappa)/(2*kappa)), z=r^2))
  # )
  # hyperTerm2 <- tryOrFailWithNA(
  #   Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=(2+n*kappa)/(2*kappa), "z"=r^2))
  # )
  #
  # result <- kappa/(n*kappa+2)*hyperTerm1/hyperTerm2
  # return(result)
}

.posteriorVariance <- function(n, r, kappa=1, oneThreshold=1e-3) {
  # Posterior mean of the .bf10Exact
  #	That is, (rho+1)/2, thus, on 0,1 scale to estimate a, b in a beta distribution
  #
  # Note(Alexander): Gauss' Vandermonde identity probably catched this case
  #
  #     add safeguard for large n as then hyperTerm1/hyperTerm2 is almost 1
  # 	  and also for logTerm almost being 1
  #
  # 	.posteriorVariance(199, 0.8) yields 6808.702
  #
  #
  result <- tryOrFailWithNA(
    .posteriorSecondMoment("n"=n, "r"=r, "kappa"=kappa, "oneThreshold"=oneThreshold) -
      (.posteriorMean("n"=n, "r"=r, "kappa"=kappa, "oneThreshold"=oneThreshold))^2)
  
  # Asymptotic approximation Based on Fisher
  #
  # if (is.na(result))
  #   result <- tanh(1/sqrt(n-3))^2
  
  return(result)
}

.betaParameterEstimates <- function(someMean, someVar) {
  # someMean \in (0, 1)
  # Note(Alexander): Gauss' Vandermonde identity covers the case that someMean = 1
  #
  someA <- tryOrFailWithNA(someMean*(someMean*(1-someMean)/someVar-1))
  someB <- tryOrFailWithNA((1-someMean)*(someMean*(1-someMean)/someVar-1))
  
  result <- list("betaA"=someA, "betaB"=someB)
  return(result)
}

.posteriorBetaParameters <- function(n, r, kappa=1, oneThreshold=1e-3) {
  # .posteriorBetaParameters
  # Let rho = 2*x - 1 where x \sim beta, thus, x = (rho+1)/2.Hence, someMu.
  # For the variance we have var(rho)/2^2
  #
  someMu <- tryOrFailWithNA((.posteriorMean("n"=n, "r"=r, "kappa"=kappa, "oneThreshold"=oneThreshold)+1)/2, )
  someVar <- tryOrFailWithNA(.posteriorVariance("n"=n, "r"=r, "kappa"=kappa, "oneThreshold"=oneThreshold)/4)
  
  
  if (isSomeNA(someMu, someVar) | isSomeInfinite(someMu, someVar)) {
    # TODO(Alexander): Before doing this try the MH sampler
    return(list(betaA=NA, betaB=NA))
  } else {
    return(.betaParameterEstimates(someMu, someVar))
  }
}

.computePearsonCredibleInterval <- function(betaA, betaB, ciValue, h0=0) {
  # Compute Pearson's correlation credible interval based on a beta fit
  #
  check <- failIfNot(betaA > 0, betaB > 0, ciValue > 0, ciValue < 1, !isSomeNull(betaA, betaB))
  
  failedResult <- list("lowerCi"=NA, "upperCi"=NA, "posteriorMedian"=NA, "ciValue"=ciValue)
  result <- list("two.sided"=failedResult, "greater"=failedResult, "less"=failedResult,
                 "ciValue"=ciValue, "betaA"=betaA, "betaB"=betaB)
  
  if (!is.null(check)) {
    result[["betaA"]] <- NA
    result[["betaB"]] <- NA
    result[["error"]] <- check
    return(result)
  }
  
  typeOne <- 1-ciValue
  excessLevel <- typeOne/2
  
  if (isSomeInfinite(betaA, betaB)) {
    result[["betaA"]] <- NA
    result[["betaB"]] <- NA
    result[["error"]] <- "Can't compute credible intervals"
    return(result)
  } else {
    # Note: Zero one refers to the problem on the (0, 1) rather than on (-1, 1)
    lowerCIZeroOne <- tryOrFailWithNA(qbeta(excessLevel, betaA, betaB))
    medianCIZeroOne <-tryOrFailWithNA(qbeta(1/2, betaA, betaB))
    upperCIZeroOne <- tryOrFailWithNA(qbeta(1-excessLevel, betaA, betaB))
    
    if (isSomeNA(lowerCIZeroOne, medianCIZeroOne, upperCIZeroOne)) {
      return(result)
    } else {
      # Note: This is simply an application of the definition of the stretched beta
      lowerCi <- 2*lowerCIZeroOne-1
      posteriorMedian <- 2*medianCIZeroOne-1
      upperCi <- 2*upperCIZeroOne-1
    }
  }
  
  
  tempList <- list("lowerCi"=lowerCi, "upperCi"=upperCi, "posteriorMedian"=posteriorMedian)
  
  if (abs(upperCi-lowerCi) <= .Machine$double.eps) {
    tempList[["tooPeaked"]] <- TRUE
  }
  
  result[["two.sided"]] <- modifyList(result[["two.sided"]], tempList)
  
  # One sided:
  tempCi <- .computePearsonMinSidedCredibleInterval("betaA"=betaA, "betaB"=betaB, "ciValue"=ciValue)
  tempList <- list("lowerCi"=tempCi[1], "upperCi"=tempCi[3], "posteriorMedian"=tempCi[2])
  
  if (abs(tempCi[3]-tempCi[1]) <= .Machine$double.eps) {
    tempList[["tooPeaked"]] <- TRUE
  }
  result[["less"]] <- modifyList(result[["less"]], tempList)
  
  # The problem is symmetric
  tempCi  <- .computePearsonMinSidedCredibleInterval("betaA"=betaB, "betaB"=betaA, "ciValue"=ciValue)
  tempList <- list("lowerCi"=-tempCi[3], "upperCi"=-tempCi[1], "posteriorMedian"=-tempCi[2])
  
  if (abs(tempCi[3]-tempCi[1]) <= .Machine$double.eps) {
    tempList[["tooPeaked"]] <- TRUE
  }
  result[["greater"]] <- modifyList(result[["greater"]], tempList)
  
  return(result)
}

.computePearsonMinSidedCredibleInterval <- function(betaA, betaB, ciValue) {
  # Compute min sided Pearson's correlation credible interval based on a beta fit
  #
  result <- NA
  typeOne <- 1-ciValue
  excessLevel <- typeOne/2
  
  if (any(is.na(c(betaA, betaB)), is.infinite(c(betaA, betaB)))) {
    return(result)
  } else {
    leftArea <- pbeta(1/2, betaA, betaB)
    lowerCIZeroOne <- tryOrFailWithNA(qbeta(excessLevel*leftArea, betaA, betaB))
    medianCIZeroOne <- tryOrFailWithNA(qbeta(leftArea/2, betaA, betaB))
    upperCIZeroOne <- tryOrFailWithNA(qbeta((1-excessLevel)*leftArea, betaA, betaB))
    
    if (isSomeNA(lowerCIZeroOne, medianCIZeroOne, upperCIZeroOne)) {
      return(result)
    } else {
      lowerCI <- 2*lowerCIZeroOne-1
      medianCI <- 2*medianCIZeroOne-1
      upperCI <- 2*upperCIZeroOne-1
    }
  }
  result <- c(lowerCI, medianCI, upperCI)
  return(result)
}

.makeKappas <- function(n) {
  someKappas <- sin(seq(1.5*pi, 2*pi, length=n))+1
  someKappas[1] <- someKappas[2]/10
  someKappas[n] <- 1
  someKappas <- 2*someKappas
  
  return(someKappas)
}

# 5. Replication TODO(Alexander) Needs revising-------
# These are the functions used to compute replication Bayes factors
#
.bfCorrieRepJosine <- function(nOri, rOri, nRep, rRep, kappa=1, hyperGeoOverFlowThreshold=25) {
  result <- list(combined=list(bf10=NA, bfPlus0=NA, bfMin0=NA))
  
  methodNumber <- 1
  while (methodNumber <= 4 && any(is.na(c(result$combined$bf10,
                                          result$combined$bfPlus0,
                                          result$combined$bfMin0)),
                                  is.infinite(result$combined$bf10))) {
    result <- .bfCorrieRepJosineKernel(nOri=nOri, rOri=rOri, nRep=nRep, rRep=rRep, kappa=kappa, methodNumber=methodNumber, hyperGeoOverFlowThreshold=hyperGeoOverFlowThreshold)
    methodNumber <- methodNumber+1
  }
  
  result[["call"]] <-
    paste0(".bfCorrieRepJosine(nOri=", nOri, ", rOri=", rOri, ", nRep=", nRep, ", rRep=", rRep, ", kappa=", kappa, ", hyperGeoOverFlowThreshold=", hyperGeoOverFlowThreshold, ")")
  
  return(result)
}

.bfCorrieRepJosineKernel <- function(nOri, rOri, nRep, rRep, kappa=1, methodNumber=1, hyperGeoOverFlowThreshold=25) {
  #
  #  Ly, A., Etz, A., Marsman, M., & Wagenmakers, E.--J. (2017) Replication Bayes factors. Manuscript in preparation
  #  Ly, A., Marsman, M., & Wagenmakers, E.-J. (2017) Analytic Posteriors for Pearson’s Correlation Coefficient. Under review
  #  Wagenmakers, E.-J., Verhagen, A. J., & Ly, A. (2016). How to quantify the evidence for the absence of a correlation. Behavior Research Methods, 48, 413-426.
  #
  # Replication BF for the correlation
  #
  # 1:2 are based on the exact reduced likelihood functions
  # 3:4 are based on the beta approximations to the reduced likelihood functions
  #
  #	methodNumber=1: Use exact likelihood Ly, Marsman, Wagenmakers (2017)
  #	methodNumber=2: Use semi-exact result, based on approximation of the likelihood JeffreysExact, see Wagenmakers et al (2015) bathing
  #	methodNumber=3: Savage Dickey beta approximation
  #	methodNumber=4: Marsman's IMH sampler and then Savage Dickey beta approximation
  #
  # Output is a list of the
  #   - original data,
  #   - rep data,
  #   - combined inference
  #   - replication BFs given the original
  #
  #
  
  # TODO: avoid when pass through object
  oriObj <- .computePearsonCorBf10(n=nOri, r=rOri, method=methodNumber, kappa=kappa)
  
  # Default is "NA" list
  result <- list(ori=oriObj, rep=list(NULL),
                 combined=list(n=c(nOri, nRep), r=c(rOri, rRep),
                               repMethodNumber=methodNumber,
                               bf10=NA, bfPlus0=NA, bfMin0=NA,
                               betaA=NA, betaB=NA) ,
                 repGivenOri=list(n=c(nOri, nRep), r=c(rOri, rRep),
                                  bf10=NA, bfPlus0=NA, bfMin0=NA),
                 repMethodNumber=methodNumber)
  
  if (is.infinite(oriObj[["bf10"]])) {
    # No use, too big too great, it's true
    #
    return(result)
  }
  
  # Calculate beta fits of the combined likelihood
  if (kappa==1) {
    #
    # methods 3 and 4 are highly dependent on the beta fits based on kappa = 1
    if (methodNumber %in% 3:4 && any(is.na(c(oriObj[["betaA"]], oriObj[["betaB"]])))) {
      # Total failure, real sad
      return(result)
    }
    
    repObj <- .computePearsonCorBf10(n=nRep, r=rRep, method=methodNumber, kappa=kappa)
    result[["rep"]] <- repObj
    
    if (methodNumber %in% 3:4 && any(is.na(c(repObj[["betaA"]], repObj[["betaB"]])))) {
      # Failed
      return(result)
    }
    
    result[["combined"]][["betaA"]] <- oriObj[["betaA"]]-1+repObj[["betaA"]]
    result[["combined"]][["betaB"]] <- oriObj[["betaB"]]-1+repObj[["betaB"]]
  } else {
    # kappa \neq 1
    
    if (methodNumber %in% 1:3) {
      oriLikelihoodFit <- .posteriorBetaParameters(n=nOri, r=rOri, kappa=1, expand=FALSE)
      repLikelihoodFit <- .posteriorBetaParameters(n=nRep, r=rRep, kappa=1, expand=FALSE)
    }
    
    if (methodNumber==4) {
      oriLikelihoodFit <- .marsmanMHSampler(n=nOri, r=rOri, kappa=1)
      
      if (is.na(oriLikelihoodFit[["betaA"]]) || is.na(oriLikelihoodFit[["betaB"]])) {
        # Total failure, it's sad
        #
        return(result)
      }
      
      repLikelihoodFit <- .marsmanMHSampler(n=nRep, r=rRep, kappa=1)
    }
    
    if (methodNumber %in% 3:4) {
      if (any(is.na(c(oriLikelihoodFit[["betaA"]], oriLikelihoodFit[["betaB"]],
                      repLikelihoodFit[["betaA"]], repLikelihoodFit[["betaB"]])))) {
        # Failure
        return(result)
      }
    }
    # combine here
    result[["combined"]][["betaA"]] <- oriLikelihoodFit[["betaA"]]-1+repLikelihoodFit[["betaA"]]-1+1/kappa
    result[["combined"]][["betaB"]] <- oriLikelihoodFit[["betaB"]]-1+repLikelihoodFit[["betaB"]]-1+1/kappa
    
    
    # Here kappa not 1, but still can see what the original default bfs will do for the rep data
    repObj <- .computePearsonCorBf10(n=nRep, r=rRep, method=methodNumber, kappa=kappa)
    result[["rep"]] <- repObj
  }
  
  if (methodNumber=="exact" || methodNumber==1) {
    twoSidedIntegrand <- function(x){.hFunctionCombinedTwoSided(nOri=nOri, rOri=rOri, nRep=nRep, rRep=rRep, x)*.priorRho(x, kappa=kappa)}
    plusSidedIntegrand <- function(x){.hFunctionCombined(nOri=nOri, rOri=rOri, nRep=nRep, rRep=rRep, x)*.priorRhoPlus(x, kappa=kappa)}
    minSidedIntegrand <- function(x){.hFunctionCombined(nOri=nOri, rOri=rOri, nRep=nRep, rRep=rRep, x)*.priorRhoMin(x, kappa=kappa)}
  } else if (methodNumber=="jeffreysIntegrate" || methodNumber==2) {
    twoSidedIntegrand <- function(x){.hJeffreysApprox(nRep, rRep, x)*.hJeffreysApprox(nOri, rOri, x)*.priorRho(x, kappa=kappa)}
    plusSidedIntegrand <- function(x){.hJeffreysApprox(nRep, rRep, x)*.hJeffreysApprox(nOri, rOri, x)*.priorRhoPlus(x, kappa=kappa)}
    minSidedIntegrand <- function(x){.hJeffreysApprox(nRep, rRep, x)*.hJeffreysApprox(nOri, rOri, x)*.priorRhoMin(x, kappa=kappa)}
  }
  
  
  if (methodNumber %in% 1:2) {
    bf10Combined <- tryOrFailWithNA(integrate(twoSidedIntegrand, -1, 1)[["value"]])
    
    if (is.na(bf10Combined)) {
      # So sad combined bf10 not available
      result[["combined"]][["bf10"]] <- NA
      return(result)
    }
    
    if (is.infinite(bf10Combined)) {
      # So big, totally infinite
      #
      result$combined$bf10 <- Inf
      result$repGivenOri$bf10 <- Inf
      
      if (r >= 0) {
        result$combined$bfPlus0 <- Inf
        result$combined$bfMin0 <- 0
        
        result$repGivenOri$bfPlus0 <- Inf
        result$repGivenOri$bfMin0 <- 0
      } else if (r < 0) {
        result$combined$bfPlus0 <- 0
        result$combined$bfMin0 <- Inf
        
        result$repGivenOri$bfPlus0 <- 0
        result$repGivenOri$bfMin0 <- Inf
      }
      return(result)
    }
    
    if (is.finite(bf10Combined)) {
      # Total winner, real great, it's the best
      
      result$combined$bf10 <- bf10Combined
      result$repGivenOri$bf10 <- bf10Combined/oriObj$bf10
      
      if (log(bf10Combined) > hyperGeoOverFlowThreshold) {
        # So big like my hands, can't handle it need to adjust
        tempList <- .recomputeCorBf10Method3(bf10Combined, a=result$combined$betaA, b=result$combined$betaB,
                                             kappa=kappa, methodNumber=3)
        
        result$combined$bfPlus0 <- tempList$bfPlus0
        result$combined$bfMin0 <- tempList$bfMin0
      } else {
        # No overflow, thus, try numerically integrate
        #
        bfPlus0Combined <- tryOrFailWithNA(integrate(plusSidedIntegrand, 0, 1)[["value"]])
        bfMin0Combined <- tryOrFailWithNA(integrate(minSidedIntegrand, -1, 0)[["value"]])
        
        if (isAnyNA(bfPlus0Combined, bfMin0Combined)) {
          # One sided failed
          return(result)
        }
        
        if ( bfPlus0Combined < 0 || bfMin0Combined < 0) {
          # One sided failed
          return(result)
        }
        
        if (is.infinite(bfPlus0Combined) || is.infinite(bfMin0Combined) ||
            (bfPlus0Combined > 1 && bfMin0Combined > 1) ||
            (bfPlus0Combined < 1 && bfMin0Combined < 1) ) {
          tempList <- .recomputeCorBf10Method3(bf10Combined,
                                               betaA=result[["combined"]][["betaA"]],
                                               betaB=result[["combined"]][["betaB"]], kappa=kappa)
          
          result[["combined"]][["bfPlus0"]] <- tempList[["bfPlus0"]]
          result[["combined"]][["bfMin0"]] <- tempList[["bfMin0"]]
        } else {
          # All good, store numerically calculated one-sided bfs
          
          result[["combined"]][["bfPlus0"]] <- bfPlus0Combined
          result[["combined"]][["bfMin0"]] <- bfMin0Combined
        }
      }
    }
  }
  
  
  if (methodNumber %in% 3:4) {
    # TODO(AlexandeR):
    if (!is.na(result$combined$betaA) && !is.na(result$combined$betaB)) {
      # Use beta fit and Savage-Dickey
      tempList <- .computeCorBf10SavageDickey(betaA=result$combined$betaA, betaB=result$combined$betaB, kappa=kappa,
                                              methodNumber=methodNumber)
      result$combined$bf10 <- tempList$bf10
      result$combined$bfPlus0 <- tempList$bfPlus0
      result$combined$bfMin0 <- tempList$bfMin0
    }
  }
  
  # TODO(Alexander): checks for bf10Combined, bfPlus0Combined, bfMin0Combined for zeroes and infinities
  result$repGivenOri$bf10 <- (result$combined$bf10) / (oriObj$bf10)
  result$repGivenOri$bfPlus0 <- (result$combined$bfPlus0) / (oriObj$bfPlus0)
  result$repGivenOri$bfMin0 <- (result$combined$bfMin0) / (oriObj$bfMin0)
  
  return(result)
}



# 1. Priors for Kendall's Tau -------------
#
.stretchedBetaTau <- function(tauPop, alpha=1, beta=1) {
  logResult <- tryOrFailWithNA((-alpha-beta)*log(2) + (alpha-1)*log(1+sin(pi/2*tauPop))
                               + (beta-1)*log(1-sin(pi/2*tauPop)) + log(cos(pi/2*tauPop) - lbeta(alpha, beta))
  )
  
  if (is.na(logResult))
    result <- tryOrFailWithNA(
      pi * 2^(-alpha-beta)/beta(alpha, beta) * (1+sin(pi/2*tauPop))^(alpha-1) *
        (1-sin(pi/2*tauPop))^(beta-1) * cos(pi/2*tauPop)
    )
  else {
    result <- pi * exp(logResult)
  }
  return(result)
}

.stretchedBetaTauSymmetric <- function(tauPop, alpha=1) {
  result <- ((pi*2^(-2*alpha))/beta(alpha, alpha))  * cos((pi*tauPop)/2)^(2*alpha-1)
  return(result)
}


.priorTau <- function(tauPop, kappa=1, alternative="two.sided") {
  if (alternative == "two.sided") {
    priorLine <- .stretchedBetaTauSymmetric(tauPop, alpha = 1/kappa)
  } else if (alternative == "greater") {
    priorLine <- .priorTauPlus("tauPop"=tauPop, "kappa"=kappa)
  } else if (alternative == "less") {
    priorLine <- .priorTauMin("tauPop"=tauPop, "kappa"=kappa)
  }
  return(priorLine)
}

.priorTauPlus <- function(tauPop, kappa=1) {
  nonNegativeIndex <- tauPop >= 0
  lessThanOneIndex <- tauPop <= 1
  valueIndex <- as.logical(nonNegativeIndex*lessThanOneIndex)
  result <- tauPop*0
  result[valueIndex] <- 2*.priorTau(tauPop[valueIndex], kappa)
  return(result)
}

.priorTauMin <- function(tauPop, kappa=1) {
  negativeIndex <- tauPop <= 0
  greaterThanMinOneIndex <- tauPop >= -1
  valueIndex <- as.logical(negativeIndex*greaterThanMinOneIndex)
  result <- tauPop*0
  result[valueIndex] <- 2*.priorTau(tauPop[valueIndex], kappa)
  return(result)
}

# 2. Bayes factors for Kendall's Tau -------------
# These are the functions used for to compute the Bayes factors
#
.computeKendallCorBf10 <- function(n, tauObs, h0=0, kappa=1, ciValue=0.95, var=1,
                                   methodNumber=1L, oneThreshold=1e-3) {
  #
  sidedResult <- list("n"=n, "stat"=tauObs, "bf"=NA, "tooPeaked"=NA,
                      "lowerCi"=NA, "upperCi"=NA, "posteriorMedian"=NA)
  
  result <- list("two.sided"=sidedResult,
                 "less"=sidedResult,
                 "greater"=sidedResult,
                 "kappa"=kappa, "ciValue"=ciValue, "h0"=h0,
                 "methodNumber"=methodNumber, "var"=var, "call"=match.call()
  )
  
  failedSidedResult <- list("n"=n, "stat"=NaN, "bf"=NA, "tooPeaked"=TRUE,
                            "ciValue"=ciValue, "lowerCi"=NA, "upperCi"=NA, "posteriorMedian"=NA)
  
  failedResult <- list("two.sided"=failedSidedResult,
                       "less"=failedSidedResult,
                       "greater"=failedSidedResult,
                       "kappa"=kappa, "ciValue"=ciValue, "acceptanceRate"=1,
                       "methodNumber"=6, "call"=match.call()
  )
  
  # When the prior is trivial (null is alternative) or when the data is predictively matched
  #
  predictiveMatchingList <- list("two.sided"=list("bf"=1, "tooPeaked"=FALSE),
                                 "greater"=list("bf"=1, "tooPeaked"=FALSE),
                                 "less"=list("bf"=1, "tooPeaked"=FALSE),
                                 "methodNumber"=0)
  
  # Information consistent result
  #
  plusSidedInfList <- list("two.sided"=list("bf"=Inf, "tooPeaked"=TRUE, "posteriorMedian"=tauObs),
                           "less"=list("bf"=0, "tooPeaked"=TRUE, "posteriorMedian"=tauObs),
                           "greater"=list("bf"=Inf, "tooPeaked"=TRUE)
  )
  
  minSidedInfList <- list("two.sided"=list("bf"=Inf, "tooPeaked"=TRUE, "posteriorMedian"=tauObs),
                          "less"=list("bf"=Inf, "tooPeaked"=TRUE),
                          "greater"=list("bf"=0, "tooPeaked"=TRUE, "posteriorMedian"=tauObs)
  )
  
  checkTypeInput <- !isEveryNumeric(tauObs, kappa, n)
  
  if (checkTypeInput) {
    result <- failedResult
    errorMessage <- "Input error: the sample size n, the summary statistic stat, or kappa are not numeric"
    result[["error"]] <- errorMessage
    return(result)
  }
  
  checkData <- failIfNot(abs(tauObs) <= 1, n >= 0, kappa > 0)
  
  if (!is.null(checkData)) {
    result <- failedResult
    result[["error"]] <- checkData
    return(result)
  }
  
  # Note: Data: OK
  # "No" prior, alternative model is the same as the null model
  # The bound kappa=0.002 is chosen arbitrarily I should choose this based on a trade off
  # between r and n, but it doesn't really matter.
  if (kappa <= 0.002 || n <= 2) {
    result <- modifyList(result, predictiveMatchingList)
    return(result)
  }
  
  checkTauObs <- (1 - abs(tauObs) < oneThreshold) # check whether 1 - |r| < oneThreshold
  
  # Information consistent result:
  if (kappa >= 1 & n > 2 & checkTauObs) {
    if (tauObs >= 0) {
      result <- modifyList(result, plusSidedInfList)
      return(result)
    } else if (tauObs < 0) {
      result <- modifyList(result, minSidedInfList)
      return(result)
    }
  }
  
  # TODO(Alexander): Check for information inconsistent kappas
  #
  if (n <= 2 || kappa==0) {
    result <- modifyList(result, predictiveMatchingList)
    return(result)
  } else if (kappa >= 1 && n > 2 && checkTauObs) {
    if (tauObs > 0) {
      result <- modifyList(result, plusSidedInfList)
      return(result)
    } else if (tauObs <= 0) {
      result <- modifyList(result, minSidedInfList)
      return(result)
    }
    result <- modifyList(result, infoConsistentList)
    return(result)
  }
  
  # Note(Alexander): Add stretched beta fitted posterior
  #
  for (alternative in c("two.sided", "greater", "less")) {
    result[[alternative]] <- .bfKendallTauSavageDickey("n"=n, "tauObs"=tauObs, "kappa"=kappa, "var"=var,
                                                       "h0"=h0, "alternative"=alternative)
  }
  
  tempList <- .computeKendallCredibleInterval("n"=n, "tauObs"=tauObs, "kappa"=kappa, "var"=var,
                                              "ciValue"=ciValue)
  result <- modifyList(result, tempList)
  
  return(result)
}

.bfKendallTauSavageDickey <- function(n, tauObs, kappa=1, var=1, h0=0, alternative="two.sided") {
  result <- list("n"=n, "stat"=tauObs, "bf"=NA, "tooPeaked"=TRUE, "lowerCi"=NA, "upperCi"=NA, "posteriorMedian"=NA)
  
  bf <- tryOrFailWithNA(
    .priorTau("tauPop"=h0, "kappa"=kappa, "alternative"=alternative) /
      .posteriorTau("n"=n, "tauObs"=tauObs, "tauPop"=h0, "kappa"=kappa, "var"=var, "alternative"=alternative)
  )
  
  if (is.finite(bf)) {
    result[["bf"]] <- bf
    result[["tooPeaked"]] <- FALSE
  }
  
  return(result)
}

# 3. Posteriors ---------
# These are the functions used for to the posteriors
#
.posteriorTauU <- function(n, Tstar, tauPop, kappa=1, var=1, alternative="two.sided") {
  #
  result <- stats::dnorm("x"=Tstar, "mean"=(1.5*tauPop*sqrt(n)), "sd"=sqrt(var)) *
    .priorTau("tauPop"=tauPop, "kappa"=kappa, "alternative"=alternative )
  return(result)
}

.posteriorTau <- function(n, tauObs, tauPop, kappa=1, var=1, alternative="two.sided") {
  Tstar <- (tauObs * ((n*(n-1))/2))/sqrt(n*(n-1)*(2*n+5)/18)
  lims <- switch(alternative,
                 "two.sided"=c(-1, 1),
                 "greater"=c(0, 1),
                 "less"=c(-1, 0)
  )
  
  logicalCensor <- (tauPop >= lims[1] & tauPop <= lims[2])
  
  integrand <- function(x) {
    .posteriorTauU("n"=n, "Tstar"=Tstar, "tauPop"=x, "kappa"=kappa, "var"=var, "alternative"=alternative)
  }
  normalisingConstant <- try(silent=TRUE, integrate(integrand, lims[1], lims[2])[["value"]])
  
  if (isTryError(normalisingConstant)) {
    result <- NA
  } else {
    # I could also do
    result <- integrand(tauPop)/normalisingConstant
  }
  return(result)
}


.computeKendallCredibleInterval <- function(n, tauObs, kappa=1, var=1, ciValue=0.95, h0=0) {
  # Compute Kendall's correlation credible interval based on a sampling
  #
  check <- failIfNot(ciValue > 0, ciValue < 1, !isSomeNull(n, tauObs))
  
  sidedResult <- list("lowerCi"=NA, "upperCi"=NA, "posteriorMedian"=NA, "ciValue"=NA)
  result <- list("two.sided"=sidedResult, "greater"=sidedResult, "less"=sidedResult, "ciValue"=ciValue)
  
  if (!is.null(check))
    return(result)
  
  for (alternative in c("two.sided", "greater", "less")) {
    result[[alternative]] <- .credibleIntervalKendallTauSingle("n"=n, "tauObs"=tauObs, "kappa"=kappa, "var"=var,
                                                               "alternative"=alternative, "ciValue"=ciValue)
  }
  
  return(result)
}

# Compute credible intervals kendalls tau
.credibleIntervalKendallTauSingle <- function(n, tauObs, kappa=1, var=1, alternative="two.sided", ciValue = 0.95, m=4000) {
  # TODO(Alexander): Interesting use-case: n=800, tauObs=-0.8, m=1000
  lowCI <- (1-ciValue)/2
  upCI <- (1+ciValue)/2
  tauPopDomain <- seq(-1, 1, length.out=(m-1))
  densVals <- .posteriorTau("n"=n, "tauObs"=tauObs, "tauPop"=tauPopDomain, "kappa"=kappa, "var"=var,
                            "alternative"=alternative)
  cdfVals <- cumsum((densVals[1:(m-1)] + densVals[2:m]) * 0.5 * (tauPopDomain[2]-tauPopDomain[1]))
  
  lowerCi <- tauPopDomain[which(cdfVals>=lowCI)[1]]
  upperCi <- tauPopDomain[which(cdfVals>=upCI)[1]]
  posteriorMedian <- tauPopDomain[which(cdfVals>=0.5)[1]]
  
  result <- list("lowerCi"=lowerCi, "upperCi"=upperCi, "posteriorMedian"=posteriorMedian)
  
  if (purrr::some(result, is.na)) {
    result <- list("lowerCi"=NA, "upperCi"=NA, "posteriorMedian"=NA, "tooPeaked"=TRUE,
                   "error"="Can't compute credible intervals")
    return(result)
  }
  
  if (abs(upperCi-lowerCi) <= .Machine$double.eps)
    result[["tooPeaked"]] <- TRUE
  
  return(result)
}


# 0. Helpers ------------
#' Try to evaluate an expression, if not fail with NA (default)
#'
#' @param expr Expression to be evaluated
#' @param value Return value if there is an error, default is NA_real_
#'
#' @return Returns the evaluation of the expression, or value if it doesn't work out
#' @export
#'
#' @examples
#' tryOrFailWithNA(integrate(exp, -Inf, Inf)[["value"]], NA)
#' tryOrFailWithNA(integrate(exp, 0, 3)[["value"]], NA)
tryOrFailWithNA <- function(expr, value=NA_real_) {
  tryCatch(
    error=function(cnd) value,
    expr
  )
}


# TODO(Alexander): Ask Don to catch message of stopifnot
#' Ensure the Truth of R Expressions and returns TRUE if the expressions are not met.
#'
#' This is basically stopifnot{base}, but instead of stopping it returns TRUE. The following descriptions is
#' adapted from stopifnot{base}: If any of the expressions in ... are not all valid, then instead of stopping a TRUE is
#' returned and an error message is printed indicating the first of the elements of ... which were not true.
#'
#' @param ... any number of (logical) R expressions, which should evaluate to TRUE
#'
#' @return Returns TRUE if the provided expressions are not met
#' @export
#'
#' @examples
#'
failIfNot <- function (...) {
  # This is equivalent to
  #
  # tryCatch(error=function(cnd){
  #   return(list("failed"=TRUE, "error"=conditionMessage(cnd)))
  # },
  # stopifnot(...)
  # )
  #
  result <- NULL
  
  ll <- list(...)
  n <- length(ll)
  
  if (n == 0L) {
    return(result)
  }
  
  Dparse <- function(call, cutoff = 60L) {
    ch <- deparse(call, width.cutoff = cutoff)
    if (length(ch) > 1L) {
      paste(ch[1L], "....")
    } else {
      ch
    }
  }
  
  head <- function(x, n = 6L) {
    x[seq_len(if (n < 0L) max(length(x) + n, 0L) else min(n, length(x)))]
  }
  
  abbrev <- function(ae, n = 3L) {
    paste(c(head(ae, n), if (length(ae) > n) "...."), collapse = "\n  ")
  }
  
  mc <- match.call()
  
  for (i in 1L:n) {
    if (!(is.logical(r <- ll[[i]]) && !anyNA(r) && all(r))) {
      cl.i <- mc[[i + 1L]]
      msg <- if (is.call(cl.i) && identical(cl.i[[1]], quote(all.equal)) &&
                 (is.null(ni <- names(cl.i)) || length(cl.i) == 3L ||
                  length(cl.i <- cl.i[!nzchar(ni)]) == 3L)) {
        sprintf(gettext("%s and %s are not equal:\n  %s"),
                Dparse(cl.i[[2]]), Dparse(cl.i[[3]]), abbrev(r))
      } else {
        sprintf(ngettext(length(r), "%s is not TRUE", "%s are not all TRUE"),
                Dparse(cl.i))
      }
      
      result <- msg
      return(result)
    }
  }
  return(result)
}




#' Checks every element using the provided function
#'
#' @param ... objects that need testing for try error
#'
#' @return Returns FALSE if there's a single element that does not check out, and TRUE if all elements check out
#' @export
#'
#' @examples
isEvery <- function(..., func) {
  # TODO: Make these to return at first find
  obj <- list(...)
  return(purrr::every(obj, func))
}

isSomeNA <- function(...) {
  return(isSome(..., "func"=anyNA, recursive=TRUE))
}


isSome <- function(..., func) {
  # TODO: Make these to return at first find
  obj <- list(...)
  return( purrr::some(obj, func) )
}

#' Checks for try errors.
#'
#' @param ... objects that need testing for try error
#'
#' @return Returns TRUE whenever there's a single try-error, FALSE otherwise
#' @export
#'
#' @examples
#' kaas <- try(integrate(exp, -Inf, Inf))
#' isTryError(kaas)
#'
isTryError <- function(...) {
  return(isSome(..., func=function(x){inherits(x, "try-error")}))
}


#' Check for any NULL
#'
#' @param ... objects that need testing for try error
#'
#' @return Returns TRUE if there's a single NULL, returns FALSE if no NULL
#' @export
#'
#' @examples
isSomeNull <- function(...) {
  return(isSome(..., func=is.null))
}


#' Check for whether all are numeric
#'
#' @param ... objects that need testing for being numeric
#'
#' @return Returns TRUE if all objects are numeric, returns FALSE otherwise
#' @export
#'
#' @examples
isEveryNumeric <- function(...) {
  # TODO: Make these to return at first find
  return(isEvery(..., func=is.numeric))
}


#' Check for any NA
#'
#' @param ... objects that need testing for NA
#'
#' @return Returns TRUE if any is NA, or its decendants are NA, returns FALSE otherwise
#' @export
#'
#' @examples
isSomeNA <- function(...) {
  # TODO: Make these to return at first find
  return(isSome(..., func=is.na))
}

isEveryFinite <- function(...) {
  isEvery(..., func=is.finite)
}

isSomeInfinite <- function(...) {
  isSome(..., func=is.infinite)
}

isSomeTrue <- function(...) {
  isSome(..., func=isTRUE)
}

