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
.pValueFromCor <- function(r, n, method=c("pearson","kendall", "spearman")) {
  result <- list()
  
  if (n <= 2){
    result[["two.sided"]] <- NA
    # tau < 0
    result[["less"]] <- NA
    # tau > 0
    result[["greater"]] <- NA
    return(result)
  }
  
  if (method == "pearson"){
    # Use t-distribution based on bivariate normal assumption using r to t transformation
    #
    df <- n - 2
    t <- r*sqrt(df/(1-r^2))
    result <- .pValueFromT("t"=t, "n1"=n-1, "n2"=0, var.equal=TRUE)
  } else if (method == "kendall"){
    if (n > 2 && n < 50) {
      # Exact sampling distribution
      # tau neq 0
      result[["two.sided"]] <- 1 - SuppDists::pKendall("q"=abs(r), N=n) + SuppDists::pKendall("q"=-abs(r), "N"=n)
      # tau < 0
      result[["less"]] <- SuppDists::pKendall("q"=r, "N"=n)
      # tau > 0
      result[["greater"]] <- SuppDists::pKendall("q"=r, "N"=n, "lower.tail" = FALSE)
    } else if (n >= 50){
      # normal approximation
      #
      someSd <- sqrt(2*(2*n+5)/(9*n*(n-1)))
      
      # tau neq 0
      result[["two.sided"]] <- 2 * stats::pnorm(-abs(r), "sd"=someSd)
      # tau < 0
      result[["less"]] <- stats::pnorm(r, "sd"=someSd)
      # tau > 0
      result[["greater"]] <- stats::pnorm(r, "sd"=someSd, lower.tail = FALSE)
    }
  } else if (method == "spearman"){
    # TODO: Johnny
    # Without code this will print a NULL, if we go through here
  }
  return(result)
}

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
.pValueFromT <- function(t, n1, n2 = 0, var.equal = TRUE) {
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
  
  result <- list()
  
  if (n2 > 0) {
    # If n2 > 0, then two-sample
    someDf <- n1 + n2 - 2
  } else {
    # If n2 <= 0, then one-sample
    someDf <- n1 - 1
  }
  
  # mu \neq 0
  result[["two.sided"]] <- 2 * stats::pt(-abs(t), "df" = someDf)
  # mu < 0
  result[["less"]] <- stats::pt(t, "df" = someDf)
  # mu > 0
  result[["greater"]] <- stats::pt(t, "df" = someDf, "lower.tail" = FALSE)
  
  return(result)
}

# These are the functions used to compute the Bayes factors
#

# 2.1 Two-sided main Bayes factor ----------------------------------------------
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
.bf10Exact <- function(n, r, kappa=1, h0=0) {
  # Note (Alexander): Input check is done at a higher level: .computePearsonCorBf10
  # Maximum it can take is r=0.993, which works well up to n = 336, but r=0.992 and n=3 fails
  
  logHyperTerm <- tryOrFailWithNA(log(hypergeo::f15.3.3(A=(n-1)/2, B=(n-1)/2, C=(n+2/kappa)/2, z=r^2)))
  
  if (is.na(logHyperTerm))
    return(NaN)
  
  logResult <- log(2^(1-2/kappa)) + 0.5 * log(pi) - lbeta(1/kappa, 1/kappa) +
    lgamma((n+2/kappa-1)/2) - lgamma((n+2/kappa)/2) + logHyperTerm
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
        # TODO(Alexander): The problem is machine precision here,
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


# 3.0 One-sided preparation ----------------------------------------------------

#' Add this to the exact two-sided Bayes factor to get bfPlus0
#'
#' @inherit .bf10Exact
.mPlusExact <- function(n, r, kappa=1) {
  # Ly et al 2015
  # This is the contribution of one-sided test
  #
  # TODO(Alexander): Input check is done at a higher level: .computePearsonCorBf10.
  # In particular the case with n <= 2
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
  
  # Information consistent result:
  if (kappa >= 1 & n > 2 & checkR) {
    if (r >= 0) {
      result <- modifyList(result, plusSidedInfList)
      return(result)
    } else if (r < 0) {
      result <- modifyList(result, minSidedInfList)
      return(result)
    }
  }
  
  # TODO(Alexander): Check for information inconsistent kappas
  #
  if (n <= 2 || kappa==0) {
    result <- modifyList(result, predictiveMatchingList)
    return(result)
  } else if (kappa >= 1 && n > 2 && checkR) {
    if (r > 0) {
      result <- modifyList(result, plusSidedInfList)
      return(result)
    } else if (r <= 0) {
      result <- modifyList(result, minSidedInfList)
      return(result)
    }
    result <- modifyList(result, infoConsistentList)
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
      bf10 <-  tryOrFailWithNA(.bf10Exact(n=n, r=r, kappa=kappa, h0=h0))
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
  
  # TODO(Alexander): Check if this is really not necessary anymore. This is now moved into
  #     .computeBCorOneSided
  #
  #  that calls
  #
  #     .computeBCorOneSidedSavageDickey
  #
  # and in subCounter 3 this is already checked
  #
  # # Note(Alexander): bfPlus0, bfMin0: CHECK COHERENCE:
  # if (!isSomeNA(bfPlus0, bfMin0)) {
  #   if (bfPlus0 > 1 && bfMin0 > 1 | any(c(bfPlus0, bfMin0) < 0)) {
  #     if (r >= 0) {
  #       # Note: Data: OK,
  #       #  bf10: OK.
  #       #  bfPlus0: OK
  #       #  bfMin0: NOT ok
  #       #
  #       # bfMin0 is bigger than one due to overflow: bfMin0 = 2*bf10 - bfPlus0.
  #       # Example: 2*1.2.... 10^ 24 - 2.... 10^24 = 1... 10^12 (due to round off)
  #       #
  #       bfMin0 <- 10^(-316)
  #       bfPlus0 <- 2*bf10 - bfMin0
  #     } else if (r < 0) {
  #       # Note: Data: OK,
  #       #  bf10: OK.
  #       #  bfPlus0: NOT ok
  #       #  bfMin0: OK
  #       bfPlus0 <- 10^(-316)
  #       bfMin0 <- 2*bf10 - bfPlus0
  #     }
  #   }
  #
  #   result <- modifyList(result, list("greater"=list("bf"=bfPlus0, "tooPeaked"=FALSE),
  #                                     "less"=list("bf"=bfMin0, "tooPeaked"=FALSE))
  #   )
  # } else {
  #   result <- modifyList(result, list("greater"=list("bf"=NA, "tooPeaked"=TRUE),
  #                                     "less"=list("bf"=NA, "tooPeaked"=TRUE))
  #   )
  # }
  
  if (!isSomeNA(fit)) {
    tempList <- .computePearsonCredibleInterval("betaA"=fit[["betaA"]], "betaB"=fit[["betaB"]],
                                                "ciValue"=ciValue)
    
    # Add list $two.sided$ci, $plusSided$ci, $less$ci, ciValue
    result <- modifyList(result, tempList)
  } else {
    # TODO(Alexander): Do we need to get the error everywhere
    tempFailedCiResult <- list("tooPeaked"=TRUE)
    failedCiResult <- list("two.sided"=tempFailedCiResult, "greater"=tempFailedCiResult, "less"=tempFailedCiResult)
    result <- modifyList(result, failedCiResult)
    result[["error"]] <- "Can't compute credible intervals"
  }
  return(result)
}


bcor.test <- function(x, y, alternative=c("two.sided", "less", "greater"),
                      method=c("pearson", "kendall", "spearman"), ciValue=0.95,
                      use="pairwise.complete.obs",
                      h0=0, kappa=1, hyperGeoOverFlowThreshold=25, oneThreshold=0.001) {
  
  if (is.null(method)) {
    result <- .computePearsonCorBf10(NULL, NULL)
    result[["error"]] <- "No method selected"
    return(result)
  }
  
  stat <- tryOrFailWithNA(cor(x, y, use=use, method=method[1]))
  
  if (is.na(stat)) {
    result <- .computePearsonCorBf10(NaN, NaN)
    result[["error"]] <- "Can't compute the correlation"
  }
  
  n <- length(x) - length(unique(c(which(is.na(x)), which(is.na(y)))))
  if (method[1]=="pearson") {
    result <- .computePearsonCorBf10("n"=n, "r"=stat, "h0"=h0, "kappa"=kappa, "ciValue"=ciValue,
                                     "oneThreshold" = oneThreshold)
    result[["alternative"]] <- alternative[1]
    return(result)
  } else if (method[1]=="kendall") {
    result <- .computePearsonCorBf10("n"=n, "r"=stat, "h0"=h0, "kappa"=kappa, "ciValue"=ciValue,
                                     "oneThreshold" = oneThreshold)
    result[["alternative"]] <- alternative[1]
    return(result)
  } else if (method[1]=="spearman") {
    print("NOT YET THIS IS JUST PEARSON AS A PLACEHOLDER")
    result <- .computePearsonCorBf10("n"=NA, "r"=NA, "h0"=h0, "kappa"=kappa, "ciValue"=ciValue,
                                     "oneThreshold" = oneThreshold)
    result[["alternative"]] <- alternative[1]
    return(result)
  }
}

.getSidedObject <- function(bfObject, alternative="two.sided", itemNames=NULL) {
  result <- modifyList(bfObject[[alternative]], bfObject[itemNames])
}

# TODO: Main wrapper function to grab infor for posterior across test=pearson, kendall and alternative = two.sided, greater/plusSided, less/minSided etc
#
.computeCorPosteriorLine <- function(bfObject, test="pearson", alternative="two.sided", minX=-0.99, maxX=0.99) {
  xDomain <- seq(minX, maxX, length.out = 1001)
  if (alternative %in% c("two-sided", "two.sided")) {
    alternative <- "two.sided"
  } else if (alternative %in% c("greater", "right", "positive")) {
    alternative <- "greater"
  } else if (alternative %in% c("less", "left", "negative")) {
    alternative <- "less"
  }
  
  sidedObject <- .getSidedObject(bfObject, alternative=alternative, itemNames=c("error", "h0"))
  
  # Note(Alexander): Don't compute if it's already computed
  #
  if (!is.null(sidedObject[["posteriorLine"]])) {
    return(sidedObject)
  }
  
  # Note(Alexander): Don't compute if there's an error
  #
  if (!is.null(sidedObject[["error"]])) {
    sidedObject[["posteriorLine"]] <- NA
    return(sidedObject)
  }
  
  # Note(Alexander): Don't compute if it's too peaked
  #
  if (sidedObject[["tooPeaked"]]) {
    sidedObject[["posteriorLine"]] <- NA
    sidedObject[["error"]] <- "Posterior is too peaked"
    return(sidedObject)
  }
  
  # TODO(Alexander):
  # # Take tempResult and use the subResult structure to be saved back into bfObject later on
  # subResult <- list(n=bfObject$n, stat=bfObject$stat, bf=NULL,
  #                   savageDickeyTestPoint=savageDTestPoint, savageDickeyPosteriorPoint=NULL, savageDickeyPriorPoint=NULL,
  #                   xDomain=xDomain, priorLine=NULL, posteriorLine=NULL, betaApproximation=FALSE,
  #                   ci=NULL, test=test, xMin=-1, xMax=1, yMin=0, yMax=NULL, alternative=alternative)
  #
  
  if (test=="pearson") {
    if (isSomeNA(bfObject[["betaA"]], bfObject[["betaB"]])) {
      sidedObject[["posteriorLine"]] <- NA
      sidedObject[["error"]] <- "Posterior is too peaked"
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
  } else if (test=="kendall") {
    # TODO(Alexander):
    #
    sidedObject[["priorLine"]] <- .priorTau("tauPop"=xDomain, "kappa"=bfObject[["kappa"]], alternative=alternative)
    tempPosteriorLine <- .posteriorTau("tauPop"=xDomain, "n"=sidedObject[["n"]], tauObs=sidedObject[["stat"]],
                                       kappa=bfObject[["kappa"]], alternative=alternative)
    
    if (isSomeNA(tempPosteriorLine) | any(tempPosteriorLine < 0) | isSomeInfinite(tempPosteriorLine)){
      sidedObject[["error"]] <- "Posterior is too peaked"
      return(sidedObject)
    }
    
    sidedObject[["posteriorLine"]] <- tempPosteriorLine
    sidedObject[["posteriorAtH0"]] <- .posteriorTau("tauPop"=sidedObject[["h0"]], "n"=sidedObject[["n"]],
                                                    "tauObs"=sidedObject[["stat"]], "kappa"=bfObject[["kappa"]],
                                                    alternative=alternative)
    sidedObject[["priorAtH0"]] <- .priorTau("tauPop"=sidedObject[["h0"]], kappa=bfObject[["kappa"]],
                                            alternative=alternative)
  }
  
  sidedObject[["yMax"]] <- max(sidedObject[["priorLine"]], sidedObject[["posteriorLine"]])
  sidedObject[["xDomain"]] <- xDomain
  return(sidedObject)
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
.posteriorMean <- function(n, r, kappa=1, old2F1=FALSE) {
  # NEW CODE CAN OFFICIALLY DO .posteriorMean(1219, 0.83)
  #
  hyperTerm1 <- tryOrFailWithNA(
    Re(hypergeo::f15.3.3("A"=n/2, "B"=n/2, "C"=(2+(n+2)*kappa)/(2*kappa), "z"=r^2))
  )
  hyperTerm2 <- tryOrFailWithNA(
    Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=(2+n*kappa)/(2*kappa), "z"=r^2))
  )
  hypRatio <- tryOrFailWithNA(hyperTerm1/hyperTerm2)
  
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

#log.hyper.term <- log(hypergeo::hypergeo(((n-1)/2), ((n-1)/2), ((n+2/kappa)/2), r^2))


.posteriorSecondMoment <- function(n, r, kappa=1) {
  # New code can do:.PosteriorSecondMoment(1219, 0.83) n=3 more than old code
  #
  #
  hyperTerm1a <- tryOrFailWithNA(
    Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=(2+(n+2)*kappa)/(2*kappa), "z"=r^2))
  )
  hyperTerm1b <- tryOrFailWithNA(
    Re(hypergeo::f15.3.3("A"=(n+1)/2, "B"=(n+1)/2, "C"=(2+(n+2)*kappa)/(2*kappa)+1, "z"=r^2))
  )
  hyperTerm2 <- tryOrFailWithNA(
    Re(hypergeo::f15.3.3("A"=(n-1)/2, "B"=(n-1)/2, "C"=(2+n*kappa)/(2*kappa), "z"=r^2))
  )
  
  hypRatioA <- hyperTerm1a/hyperTerm2
  hypRatioB <- hyperTerm1b/hyperTerm2
  
  # TODO(Alexander): Add asymptotic approximation here
  #
  result <- kappa/(n*kappa+2) *
    (hypRatioA+ kappa*(n-1)^(2)/(2+(n+2)*kappa)*r^2*hypRatioB)
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

.posteriorVariance <- function(n, r, kappa=1) {
  # Posterior mean of the .bf10Exact
  #	That is, (rho+1)/2, thus, on 0,1 scale to estimate a, b in a beta distribution
  #
  # TODO: add safeguard for large n as then hyperTerm1/hyperTerm2 is almost 1
  # 	and also for logTerm almost being 1
  #
  # 	.posteriorVariance(199, 0.8) yields 6808.702
  #
  #
  result <- tryOrFailWithNA(.posteriorSecondMoment(n,r,kappa)-(.posteriorMean(n,r,kappa))^2)
  
  # Asymptotic approximation Based on Fisher
  #
  # if (is.na(result))
  #   result <- tanh(1/sqrt(n-3))^2
  
  return(result)
}

.betaParameterEstimates <- function(someMean, someVar) {
  # someMean \in (0, 1)
  # TODO: Perhaps safeguard against someMean = 0 or someMean = 1
  #
  someA <- tryOrFailWithNA(someMean*(someMean*(1-someMean)/someVar-1))
  someB <- tryOrFailWithNA((1-someMean)*(someMean*(1-someMean)/someVar-1))
  
  result <- list("betaA"=someA, "betaB"=someB)
  return(result)
}

.posteriorBetaParameters <- function(n, r, kappa=1) {
  # .posteriorBetaParameters
  # Let rho = 2*x - 1 where x \sim beta, thus, x = (rho+1)/2.Hence, someMu.
  # For the variance we have var(rho)/2^2
  #
  someMu <- tryOrFailWithNA((.posteriorMean("n"=n, "r"=r, kappa)+1)/2)
  someVar <- tryOrFailWithNA(.posteriorVariance("n"=n, "r"=r, kappa)/4)
  
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
                 "betaA"=betaA, "betaB"=betaB)
  
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


.computeCorSequentialLine <- function(x, y, bfObject, test="pearson", alternative="two.sided", minX=-0.99, maxX=0.99) {
  # sidedObject <- .getSidedObject(bfObject, alternative=alternative)
  #
  error <- bfObject[["error"]]
  bf10 <- bfObject[["two.sided"]][["bf"]]
  
  if (!is.null(error) | is.na(bf10)) {
    if (is.null(error))
      error <- "Could not compute"
    
    sideError <- list("sequentialLine"=error)
    result <- list("two.sided"=sideError, "greater"=sideError, "less"=sideError)
    return(result)
  }
  
  n <- bfObject[[1]][["n"]]
  
  compN <- 3:n
  nDomain <- c(1:2, compN)
  
  .calculateSequentialCor <- function(i, x, y) {
    return(tryOrFailWithNA(cor(x[1:i], y[1:i], use="pairwise.complete.obs", method=test)))
  }
  
  rSeq <- purrr::map_dbl(compN, .calculateSequentialCor, x=x, y=y)
  
  if (sum(is.na(rSeq)) >= 1) {
    sideError <- list("sequentialLine"="Could not compute")
    result <- list("two.sided"=sideError, "greater"=sideError, "less"=sideError)
  }
  
  h0 <- bfObject[["h0"]]
  kappa <- bfObject[["kappa"]]
  methodNumber <- bfObject[["methodNumber"]]
  
  # TODO(Alexander):
  
  if (test=="pearson") {
    .calculateSequentialBCor <- function(n, r) {
      bfObject <- .computePearsonCorBf10(n, r, "h0"=h0, "kappa"=kappa,
                                         methodNumber=methodNumber)
      list(bfObject[["two.sided"]][["bf"]],
           bfObject[["greater"]][["bf"]],
           bfObject[["less"]][["bf"]]
      )
    }
  }
  
  allBfs <- purrr::map2(compN, rSeq, .calculateSequentialBCor)
  
  placeHolder <- vector("numeric", length=length(nDomain))
  placeHolder[1] <- placeHolder[2] <- 1
  sideResult <- list("sequentialLine"=placeHolder)
  
  result <- list("two.sided"=sideResult, "greater"=sideResult, "less"=sideResult)
  
  for (i in seq_along(allBfs)){
    result[[1]][[1]][i+2] <- allBfs[[i]][[1]]
    result[[2]][[1]][i+2] <- allBfs[[i]][[2]]
    result[[3]][[1]][i+2] <- allBfs[[i]][[3]]
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

.computeCorRobustnessLine <- function(bfObject, test="pearson") {
  error <- bfObject[["error"]]
  bf10 <- bfObject[["two.sided"]][["bf"]]
  
  if (!is.null(error) | is.na(bf10)) {
    if (is.null(error))
      error <- "Could not compute"
    
    sideError <- list("robustnessLines"=error)
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
  
  # TODO(Alexander): Different iterations not necessary already covered by the wrapper
  #
  if (test=="pearson") {
    .calculateOneSidedRobustness <- function(kappa) {
      bfObject <- .computePearsonCorBf10("n"=n, "r"=stat, "h0"=h0, "kappa"=kappa, "methodNumber"=methodNumber)
      list(bfObject[["two.sided"]][["bf"]],
           bfObject[["greater"]][["bf"]],
           bfObject[["less"]][["bf"]])
    }
  }
  
  allBfs <- purrr::map(compKappas, .f=.calculateOneSidedRobustness)
  
  placeHolder <- c(1, 1, vector("numeric", length=48))
  sideResult <- list("robustnessLine"=placeHolder)
  result <- list("two.sided"=sideResult, "greater"=sideResult, "less"=sideResult)
  
  for (i in seq_len(48)){
    result[[1]][[1]][i+2] <- allBfs[[i]][[1]]
    result[[2]][[1]][i+2] <- allBfs[[i]][[2]]
    result[[3]][[1]][i+2] <- allBfs[[i]][[3]]
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

