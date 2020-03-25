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

.equivalencePriorandPosterior <- function(jaspResults, dataset, options, equivalenceBayesianTTestResults, ready, paired = FALSE) {
  
  # You only need to create this if it doesn't exist yet
  # if (is.null(equivalencePriorPosteriorContainer))
  
  equivalencePriorPosteriorContainer <- createJaspContainer(title = gettext("Equivalence Prior and Posterior"))
  jaspResults[["equivalencePriorPosteriorContainer"]] <- equivalencePriorPosteriorContainer
  
  equivalencePriorPosteriorContainer$dependOn(c("priorandposterior", "missingValues", "priorWidth",
                                                "effectSizeStandardized", "lowerbound", "upperbound", 
                                                "informative", "informativeCauchyLocation", "informativeCauchyScale",
                                                "informativeNormalMean", "informativeNormalStd", "informativeTLocation", 
                                                "informativeTScale", "informativeTDf", "priorandposteriorAdditionalInfo"))
  
  if (!ready)
    return()
  
  if (paired) {
    variables <- options$pairs
  } else {
    variables <- options$variables
  }
  
  for (variable in variables) {
  
    # Equivalence bounds
    xx <- seq(min(options$lowerbound), max(options$upperbound), length.out = 1000)
    
    if (paired) {
      variable <- paste(variable[[1L]], variable[[2L]], sep = " - ")
    }
    
    title = variable
    
    if (!is.null(equivalencePriorPosteriorContainer[[variable]]))
      next
    
    results <- equivalenceBayesianTTestResults[[variable]]
    
    equivalenceTTestPriorPosterior <- createJaspPlot(title = title, width = 480, height = 320)
    
    if (paired) {
      equivalenceTTestPriorPosterior$dependOn("pairs") # this could be better; by selecting only the relevant pair
    } 
    else {
      equivalenceTTestPriorPosterior$dependOn(optionContainsValue = list("variables" = variable))
    }
    
    if (is.null(results$status)) {
      
      # Needed variables
      delta          <- NULL
      nullInterval   <- c(-Inf, Inf)
      oneSided       <- FALSE
      iterations     <- 10000
      addInformation <- options[["priorandposteriorAdditionalInfo"]]
      t              <- results[["tValue"]]
      n1             <- results[["n1"]]
      n2             <- if (paired) NULL else results[["n2"]]
      r              <- options[["priorWidth"]]
      BF <- results$bfEquivalence / results$bfNonequivalence
      
      # Make an error when BF is either 0 or inf., then no plot possible 
      if (BF == 0 || BF == Inf) {
        equivalenceTTestPriorPosterior$setError("Currently a plot with a Bayes factor of Inf or 0 is not supported")
        return()
      }
      
      if (options[["effectSizeStandardized"]] == "informative") {
        
        # Informative prior
        xlim <- vector("numeric", 2)
        if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
          
          ci99PlusMedian <- .ciPlusMedian_t(t                  = t, 
                                            n1                 = n1, 
                                            n2                 = n2, 
                                            independentSamples = !paired && !is.null(n2),
                                            prior.location     = options[["informativeCauchyLocation"]],
                                            prior.scale        = options[["informativeCauchyScale"]],
                                            prior.df           = 1, 
                                            ci                 = .99, 
                                            oneSided           = oneSided)
          
          priorLower <- .qShiftedT(.15, parameters = c(options[["informativeCauchyLocation"]],
                                                       options[["informativeCauchyScale"]],
                                                       1), oneSided = oneSided)
          
          priorUpper <- .qShiftedT(.85, parameters = c(options[["informativeCauchyLocation"]],
                                                       options[["informativeCauchyScale"]],
                                                       1), oneSided = oneSided)
          
          # Compute 95% credible interval & median
          ci95PlusMedian <- .ciPlusMedian_t(t                  = t, 
                                            n1                 = n1, 
                                            n2                 = n2, 
                                            independentSamples = !paired && !is.null(n2),
                                            prior.location     = options[["informativeCauchyLocation"]],
                                            prior.scale        = options[["informativeCauchyScale"]],
                                            prior.df           = 1, 
                                            ci                 = .95, 
                                            oneSided           = oneSided)
          
          CIlow <- ci95PlusMedian[["ciLower"]]
          CIhigh <- ci95PlusMedian[["ciUpper"]]
          medianPosterior <- ci95PlusMedian[["median"]]
          
        } else if (options[["informativeStandardizedEffectSize"]] == "t") {
          
          ci99PlusMedian <- .ciPlusMedian_t(t                  = t, 
                                            n1                 = n1, 
                                            n2                 = n2, 
                                            independentSamples = !paired && !is.null(n2),
                                            prior.location     = options[["informativeTLocation"]],
                                            prior.scale        = options[["informativeTScale"]],
                                            prior.df           = options[["informativeTDf"]],
                                            ci                 = .99, 
                                            oneSided           = oneSided)
          
          priorLower <- .qShiftedT(.15, parameters = c(options[["informativeTLocation"]],
                                                       options[["informativeTScale"]],
                                                       options[["informativeTDf"]]),
                                   oneSided = oneSided)
          
          priorUpper <- .qShiftedT(.85, parameters = c(options[["informativeTLocation"]],
                                                       options[["informativeTScale"]],
                                                       options[["informativeTDf"]]),
                                   oneSided = oneSided)
          
          # Compute 95% credible interval & median
          ci95PlusMedian <- .ciPlusMedian_t(t                  = t, 
                                            n1                 = n1, 
                                            n2                 = n2, 
                                            independentSamples = !paired && !is.null(n2),
                                            prior.location     = options[["informativeTLocation"]],
                                            prior.scale        = options[["informativeTScale"]],
                                            prior.df           = options[["informativeTDf"]], 
                                            ci                 = .95, 
                                            oneSided           = oneSided)
          
          CIlow <- ci95PlusMedian[["ciLower"]]
          CIhigh <- ci95PlusMedian[["ciUpper"]]
          medianPosterior <- ci95PlusMedian[["median"]]
          
        } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
          
          ci99PlusMedian <- .ciPlusMedian_normal(t                  = t, 
                                                 n1                 = n1, 
                                                 n2                 = n2, 
                                                 independentSamples = !paired && !is.null(n2),
                                                 prior.mean         = options[["informativeNormalMean"]],
                                                 prior.variance     = options[["informativeNormalStd"]]^2,
                                                 ci                 = .99, 
                                                 oneSided           = oneSided)
          
          priorAreaSmaller0 <- pnorm(0, options[["informativeNormalMean"]], options[["informativeNormalStd"]])
          
          lowerp <- 0.15
          upperp <- 0.85
          
          priorLower <- qnorm(lowerp, options[["informativeNormalMean"]], options[["informativeNormalStd"]])
          priorUpper <- qnorm(upperp, options[["informativeNormalMean"]], options[["informativeNormalStd"]])
          
          # Compute 95% credible interval & median
          ci95PlusMedian <- .ciPlusMedian_normal(t                  = t, 
                                                 n1                 = n1, 
                                                 n2                 = n2, 
                                                 independentSamples = !paired && !is.null(n2),
                                                 prior.mean         = options[["informativeNormalMean"]],
                                                 prior.variance     = options[["informativeNormalStd"]]^2,
                                                 ci                 = .95, 
                                                 oneSided           = oneSided)
          
          CIlow <- ci95PlusMedian[["ciLower"]]
          CIhigh <- ci95PlusMedian[["ciUpper"]]
          medianPosterior <- ci95PlusMedian[["median"]]
          
        }
        
        xlim[1] <- min(-2, ci99PlusMedian[["ciLower"]], priorLower)
        xlim[2] <- max(2, ci99PlusMedian[["ciUpper"]], priorUpper)
        xticks <- pretty(xlim)
        
        ylim <- vector("numeric", 2)
        
        ylim[1] <- 0
        dmax1 <- optimize(function(x).dposterior_informative(x, 
                                                             t        = t, 
                                                             n1       = n1, 
                                                             n2       = n2, 
                                                             paired   = paired,
                                                             oneSided = oneSided, 
                                                             options  = options),
                          interval = range(xticks),
                          maximum  = TRUE)$objective
        
        dmax2 <- optimize(function(x).dprior_informative(x, 
                                                         oneSided = oneSided, 
                                                         options  = options),
                          interval = range(xticks),
                          maximum  = TRUE)$objective
        dmax <- max(c(dmax1, dmax2))
        
        xlabels <- formatC(xticks, 1, format = "f")
        
        # Calculate prior and posterior over the whole range
        xxx <- seq(min(xticks), max(xticks), length.out = 1000)
        priorLine <- .dprior_informative(xxx, 
                                         oneSided = oneSided, 
                                         options  = options)
        
        posteriorLine <- .dposterior_informative(xxx, 
                                                 t        = t, 
                                                 n1       = n1, 
                                                 n2       = n2, 
                                                 paired   = paired,
                                                 oneSided = oneSided, 
                                                 options  = options)
        
        # Calculate prior and posterior over the interval range
        priorInterval <- .dprior_informative(xx, 
                                             oneSided = oneSided, 
                                             options  = options)
        
        posteriorInterval <- .dposterior_informative(xx, 
                                                     t        = t, 
                                                     n1       = n1, 
                                                     n2       = n2, 
                                                     paired   = paired,
                                                     oneSided = oneSided, 
                                                     options  = options)
        
        xlim <- c(min(CIlow,range(xticks)[1]), max(range(xticks)[2], CIhigh))
        
      } else {
        
        # Sample from delta posterior
        bfObject <- BayesFactor::meta.ttestBF(t      = t, 
                                              n1     = n1, 
                                              n2     = n2, 
                                              rscale = r)
        
        samples <- BayesFactor::posterior(model      = bfObject, 
                                          iterations = iterations,
                                          index      = 1)
        delta <- samples[,"delta"]
        
        # fit shifted t distribution
        if (is.null(n2) || paired) {
          
          deltaHat <- t * sqrt(1 / n1)
          N <- n1
          df <- N - 1
          sigmaStart <- 1 / N
          
        } else if (!is.null(n2) && !paired) {
          
          deltaHat <- t * sqrt((n1 + n2) / (n1 * n2))
          df <- n1 + n2 - 2
          sigmaStart <- sqrt((n1 * n2) / (n1 + n2))
          
        }
        
        if (sigmaStart < .01)
          sigmaStart <- .01
        
        parameters <- try(silent = TRUE,
                          expr = optim(par = c(deltaHat, sigmaStart, df),
                                       fn =.likelihoodShiftedT, data = delta,
                                       method = "BFGS")$par)
        
        if (isTryError(parameters)) {
          parameters <- try(silent = TRUE,
                            expr = optim(par = c(deltaHat, sigmaStart, df),
                                         fn = .likelihoodShiftedT, data = delta,
                                         method ="Nelder-Mead")$par)
        }
        
        # Set limits plot
        xlim <- vector("numeric", 2)
        
        if (oneSided == FALSE) {
          xlim[1] <- min(-2, quantile(delta, probs = 0.01)[[1]])
          xlim[2] <- max(2, quantile(delta, probs = 0.99)[[1]])
        }
        
        xticks <- pretty(xlim)
        ylim <- vector("numeric", 2)
        ylim[1] <- 0
        
        dmax <- optimize(function(x).dposteriorShiftedT(x, parameters = parameters,
                                                        oneSided = oneSided), interval = range(xticks),
                         maximum = TRUE)$objective
        
        # Calculate position of "nice" tick marks and create labels
        xlabels <- formatC(xticks, 1, format= "f")
        
        # Compute 95% credible interval & median
        CIlow <- quantile(delta, probs = 0.025)[[1]]
        CIhigh <- quantile(delta, probs = 0.975)[[1]]
        medianPosterior <- median(delta)
        
        if (any(is.na(c(CIlow, CIhigh, medianPosterior)))) {
          CIlow <- .qShiftedT(0.025, parameters, oneSided = FALSE)
          CIhigh <- .qShiftedT(0.975, parameters, oneSided = FALSE)
          medianPosterior <- .qShiftedT(0.5, parameters, oneSided = FALSE)
        }
        
        # Calculate prior and posterior over the whole range
        priorLine     <- .dprior(seq(min(xticks), max(xticks),length.out = 1000), r = r, oneSided = oneSided)
        posteriorLine <- .dposteriorShiftedT(x = seq(min(xticks), max(xticks),
                                                     length.out = 1000), parameters = parameters,
                                             oneSided = oneSided)
        
        # Calculate prior and posterior over the interval range
        priorInterval <- .dprior(x        = xx, 
                                 r        = r, 
                                 oneSided = oneSided)
        
        posteriorInterval  <- .dposteriorShiftedT(x          = xx, 
                                                  parameters = parameters,
                                                  oneSided   = oneSided)
        
      }
      
      if ("effectSizeStandardized" %in% names(options) && options$effectSizeStandardized == "informative") {
        heightPriorAtZero <- .dprior_informative(0, oneSided = oneSided, options = options)
        heightPosteriorAtZero <- .dposterior_informative(0, 
                                                         t        = t, 
                                                         n1       = n1, 
                                                         n2       = n2, 
                                                         paired   = paired,
                                                         oneSided = oneSided, 
                                                         options  = options)
      } else {
        heightPriorAtZero <- .dprior(0, r, oneSided = oneSided)
        heightPosteriorAtZero <- .dposteriorShiftedT(0, parameters = parameters, oneSided = oneSided)
        
      }
      
      dfLines <- data.frame(
        x = seq(min(xticks), max(xticks), length.out = 1000L),
        y = c(posteriorLine, priorLine),
        g = factor(rep(c("Posterior", "Prior"), each = 1000L)) # 1000 is apparently a fixed number
      )
      
      dfPoints <- data.frame(
        x = 0.0,
        y = c(heightPosteriorAtZero, heightPriorAtZero),
        g = c("Posterior", "Prior")
      )
      
      CRI <- c(CIlow, CIhigh)
      median <- medianPosterior
      
      
      if (!addInformation) {
        BF <- NULL
        median <- NULL
        CRI <- NULL
      }
      
      plotPriorPosterior <- JASPgraphs::PlotPriorAndPosterior(dfLines, 
                                                              BF           = BF,
                                                              CRI          = CRI,
                                                              median       = median,
                                                              bfType       = "BF10", 
                                                              xName = bquote(paste(.(gettext("Effect size")), ~delta)),
                                                              bfSubscripts = JASPgraphs::parseThis(c("BF[phantom()%in%phantom()%notin%phantom()]",
                                                                                                     "BF[phantom()%notin%phantom()%in%phantom()]")),
                                                              pizzaTxt     = JASPgraphs::parseThis(c("data~'|'~H[phantom()%notin%phantom()]", "data~'|'~H[phantom()%in%phantom()]")))
      
      
      if (options$priorandposteriorAdditionalInfo) { 
        
        plotPriorPosterior$subplots[[4]] <- plotPriorPosterior$subplots[[4]] + ggplot2::geom_ribbon(
          data.frame(x = xx, ymin = 0, ymax = c(priorInterval, posteriorInterval), g = factor(rep(1:2, each = 1000))), # data.frame(x = xx, ymin = 0, ymax = c(dnorm(xx, 0, 1), dnorm(xx, 1, .5)), g = factor(rep(1:2, each=1000))),
          mapping = ggplot2::aes(x = x, ymax = ymax, ymin = ymin, group = g, fill = g),
          inherit.aes = FALSE,
          alpha = .5, show.legend = FALSE) + 
          ggplot2::scale_fill_manual(values = c("grey", "darkgrey"))
        
      } else {
        
        plotPriorPosterior <- plotPriorPosterior + ggplot2::geom_ribbon(
          data.frame(x = xx, ymin = 0, ymax = c(.dposteriorShiftedT(x = xx, parameters = parameters,
                                                                    oneSided = oneSided), .dprior(xx, r = r, oneSided = oneSided)), g = factor(rep(1:2, each = 1000))), # data.frame(x = xx, ymin = 0, ymax = c(dnorm(xx, 0, 1), dnorm(xx, 1, .5)), g = factor(rep(1:2, each=1000))),
          mapping = ggplot2::aes(x = x, ymax = ymax, ymin = ymin, group = g, fill = g),
          inherit.aes = FALSE,
          alpha = .5, show.legend = FALSE) + 
          ggplot2::scale_fill_manual(values = c("grey", "darkgrey"))
      }
      
      if (isTryError(plotPriorPosterior)) {
        equivalenceTTestPriorPosterior$setError(.extractErrorMessage(plotPriorPosterior))
      } else {
        equivalenceTTestPriorPosterior$plotObject <- plotPriorPosterior
      }
      
    } else {
      # place error in plot
      equivalenceTTestPriorPosterior$setError(results$errorFootnotes)
    }
    
    equivalencePriorPosteriorContainer[[variable]] <- equivalenceTTestPriorPosterior
  }
  
  return()
}  

.equivalence_bf_t <- function(t, n1, n2, independentSamples, prior.location, prior.scale, prior.df, options) {
  # BF = density in the range of the posterior / density in the range of the prior
  
  # Step 1: Density in the equivalence range of the prior
  prior <- metaBMA::prior("t", c(location = prior.location, scale = prior.scale, nu = prior.df))
  integralEquivalencePrior <- integrate(prior, lower = options$lowerbound, upper = options$upperbound)
  errorEquivalencePrior <- integralEquivalencePrior$abs.error
  
  integralEquivalencePrior <- integralEquivalencePrior$value
  intergralNonequivalencePrior <- 1 - integralEquivalencePrior 
  
  # Step 2: Density in the equivalence range of the posterior
  upperbound <- .equivalence_cdf_t(x                  = options$upperbound, 
                                   t                  = t, 
                                   n1                 = n1, 
                                   n2                 = n2, 
                                   independentSamples = independentSamples,
                                   prior.location     = prior.location, 
                                   prior.scale        = prior.scale, 
                                   prior.df           = prior.df)
  
  lowerbound <- .equivalence_cdf_t(x                  = options$lowerbound, 
                                   t                  = t, 
                                   n1                 = n1, 
                                   n2                 = n2, 
                                   independentSamples = independentSamples,
                                   prior.location     = prior.location, 
                                   prior.scale        = prior.scale, 
                                   prior.df           = prior.df)
  
  errorEquivalencePosterior <- upperbound$abs.error + lowerbound$abs.error
  integralEquivalencePosterior <- upperbound$value - lowerbound$value
  
  # to prevent numerical integration error (value < error)
  if (integralEquivalencePosterior < 0) 
    integralEquivalencePosterior = 0     
  
  intergralNonequivalencePosterior <- 1 - integralEquivalencePosterior
  
  # Step 3: Calculate BF
  bfEquivalence <- integralEquivalencePosterior / integralEquivalencePrior
  bfNonequivalence <- intergralNonequivalencePosterior / intergralNonequivalencePrior 
  
  return(list(bfEquivalence    = bfEquivalence, 
              bfNonequivalence = bfNonequivalence, 
              errorPrior       = errorEquivalencePrior, 
              errorPosterior   = errorEquivalencePosterior))
}

.equivalence_bf_normal <- function(t, n1, n2, independentSamples, prior.mean, prior.variance, options) {
  # BF_equivalence = density in the range of the posterior / density in the range of the prior
  
  # Step 1: Density in the range of the prior
  prior <- metaBMA::prior("norm", c(mean = prior.mean, sd = sqrt(prior.variance)))
  integralEquivalencePrior <- integrate(prior, lower = options$lowerbound, upper = options$upperbound)
  errorEquivalencePrior <- integralEquivalencePrior$abs.error
  integralEquivalencePrior <- integralEquivalencePrior$value
  
  intergralNonequivalencePrior <- 1 - integralEquivalencePrior 
  
  # Step 2: Density in the equivalence range of the posterior
  upperbound <- .equivalence_cdf_normal(x = options$upperbound, t, n1, n2, independentSamples, 
                                        prior.mean = prior.mean, prior.variance = prior.variance)
  
  lowerbound <- .equivalence_cdf_normal(x = options$lowerbound, t, n1, n2, independentSamples, 
                                        prior.mean = prior.mean, prior.variance = prior.variance)
  
  errorEquivalencePosterior <- upperbound$abs.error + lowerbound$abs.error
  integralEquivalencePosterior <- upperbound$value - lowerbound$value
  
  # to prevent numerical integration error (value < error)
  if (integralEquivalencePosterior < 0) 
    integralEquivalencePosterior = 0
  
  intergralNonequivalencePosterior <- 1 - integralEquivalencePosterior
  
  # Step 3: Calculate BF
  bfEquivalence <- integralEquivalencePosterior / integralEquivalencePrior
  bfNonequivalence <- intergralNonequivalencePosterior / intergralNonequivalencePrior 
  
  return(list(bfEquivalence    = bfEquivalence, 
              bfNonequivalence = bfNonequivalence, 
              errorPrior       = errorEquivalencePrior, 
              errorPosterior   = errorEquivalencePosterior))
}

.equivalence_cdf_normal <- function(x, t, n1, n2 = NULL, independentSamples = FALSE, prior.mean, prior.variance) {
  
  continue <- TRUE
  rel.tol <- .Machine$double.eps^0.25
  subdivisions <- 100L
  maxiter <- 20
  i <- 1
  
  # This is a trick to minimalize numerical integration error
  while(continue && i <= maxiter) {
    r <- try({
      int <- integrate(.posterior_normal, lower = -Inf, upper = x, t = t, n1 = n1, n2 = n2,
                       independentSamples = independentSamples, prior.mean = prior.mean, prior.variance = prior.variance,
                       rel.tol = rel.tol, subdivisions = subdivisions)
    }, silent = TRUE)
    
    if (class(r) == "try-error" && grepl("maximum number of subdivisions", x = r[1])) {
      subdivisions <- 2 * subdivisions
      next
    } else if (class(r) == "try-error") {
      continue = TRUE
    } else {
      continue <- int$value < int$abs.error
    }
    rel.tol <- rel.tol * 1e-3
    i <- i + 1
  }
  
  if (class(r) == "try-error") {
    return(list(value = NULL, abs.error = NULL))
  } else {
    return(list(value = int$value, abs.error = int$abs.error))
  }
}

.equivalence_cdf_t <- function(x, t, n1, n2 = NULL, independentSamples = FALSE, prior.location, prior.scale, prior.df) {
  
  continue <- TRUE
  rel.tol <- .Machine$double.eps^0.25
  subdivisions <- 100L
  maxiter <- 20
  i <- 1
  
  # This is a trick to minimalize numerical integration error
  while(continue && i <= maxiter) {
    r <- try({
      int <- integrate(.posterior_t, lower = -Inf, upper = x, t = t, n1 = n1, n2 = n2,
                       independentSamples = independentSamples, prior.location = prior.location,
                       prior.scale = prior.scale, prior.df = prior.df, rel.tol = rel.tol,
                       subdivisions = subdivisions)
    }, silent = TRUE)
    if (class(r) == "try-error" && grepl("maximum number of subdivisions", x = r[1])) {
      subdivisions <- 2 * subdivisions
      next
    } else if (class(r) == "try-error") {
      continue = TRUE
    } else {
      continue <- int$value < int$abs.error
    }
    rel.tol <- rel.tol * 1e-3
    i <- i + 1
  }
  
  if (class(r) == "try-error") {
    return(list(value = NULL, abs.error = NULL))
  } else {
    return(list(value = int$value, abs.error = int$abs.error))
  }
}

.generalEquivalenceTtestBF <- function(x = NULL, y = NULL, paired = FALSE, options) {
  
  tValue <- unname(t.test(x, y, var.equal = TRUE)$statistic)
  
  n1 <- as.numeric(length(x))
  n2 <- if (paired) 0 else as.numeric(length(y))
  
  if (options[["effectSizeStandardized"]] == "default") {
    
    bfObject <- .equivalence_bf_t(t                  = tValue, 
                                  n1                 = n1, 
                                  n2                 = n2,                         
                                  independentSamples = !paired && !is.null(y),
                                  prior.location     = 0,
                                  prior.scale        = options[["priorWidth"]],
                                  prior.df           = 1, 
                                  options            = options)
    
  } else if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
    
    bfObject <- .equivalence_bf_t(t                  = tValue, 
                                  n1                 = n1,
                                  n2                 = n2,                         
                                  independentSamples = !paired && !is.null(y),
                                  prior.location     = options[["informativeCauchyLocation"]],
                                  prior.scale        = options[["informativeCauchyScale"]],
                                  prior.df           = 1,
                                  options            = options)
    
  } else if (options[["informativeStandardizedEffectSize"]] == "t") {
    
    bfObject <- .equivalence_bf_t(t                  = tValue, 
                                  n1                 = n1, 
                                  n2                 = n2,
                                  independentSamples = !paired && !is.null(y),
                                  prior.location     = options[["informativeTLocation"]],
                                  prior.scale        = options[["informativeTScale"]],
                                  prior.df           = options[["informativeTDf"]], 
                                  options            = options)
    
  } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
    
    bfObject <- .equivalence_bf_normal(t                 = tValue, 
                                       n1                 = n1, 
                                       n2                 = n2,
                                       independentSamples = !paired && !is.null(y),
                                       prior.mean         = options[["informativeNormalMean"]],
                                       prior.variance     = options[["informativeNormalStd"]]^2, 
                                       options            = options)

  }
  
  # if no error  else error
  return(list(bfEquivalence     = bfObject$bfEquivalence, 
              bfNonequivalence  = bfObject$bfNonequivalence, 
              errorPrior        = bfObject$errorPrior, 
              errorPosterior    = bfObject$errorPosterior, 
              tValue            = tValue, 
              n1                = n1, 
              n2                = n2,
              errorBfI1         = bfObject$errorBfI1, 
              errorBfnI1        = bfObject$errorBfnI1, 
              errorBfnII        = bfObject$errorBfInI,
              errorBfInI        = bfObject$errorBfInI))
}

.equivalencePlotSequentialAnalysis <- function(jaspResults, dataset, options, equivalenceBayesianTTestResults, ready, paired = FALSE) {
  
  #if (is.null(equivalenceSequentialContainer))
  equivalenceSequentialContainer <- createJaspContainer(title = gettext("Equivalence Sequential Analysis"))
  
  equivalenceSequentialContainer$dependOn(c("missingValues", "priorWidth",
                                            "effectSizeStandardized", "lowerbound", "upperbound",
                                            "informative", "informativeCauchyLocation", "informativeCauchyScale",
                                            "informativeNormalMean", "informativeNormalStd", "informativeTLocation",
                                            "informativeTScale", "informativeTDf", "plotSequentialAnalysisRobustness"))
  
  jaspResults[["equivalenceSequentialContainer"]] <- equivalenceSequentialContainer
  
  if (!ready)
    return()

  grouping <- options[["groupingVariable"]]
  if (identical(grouping, ""))
      grouping <- NULL
  
  hasGrouping <- !is.null(grouping)
  if (hasGrouping) {
    levels <- levels(dataset[[.v(grouping)]])
    g1 <- levels[1]
    g2 <- levels[2]
    idxG1 <- dataset[[.v(grouping)]] == g1
    idxG2 <- dataset[[.v(grouping)]] == g2
   } else {
    g1 <- NULL
    g2 <- NULL
    group2 <- NULL
    subDataSet <- NULL
   }

  if (paired) {
    variables <- options$pairs
  } else {
    variables <- options$variables
  }

  for (variable in variables) {
    
    if (paired) {
      title <- paste(variable[[1L]], variable[[2L]], sep = " - ")
      
      subDataSet <- dataset[, .v(c(variable[[1L]], variable[[2L]]))]
      subDataSet <- subDataSet[complete.cases(subDataSet), ]
      
      group1 <- subDataSet[[1L]]
      group2 <- subDataSet[[2L]]
      
      #p1 <- .v(variable[[1L]])
      #p2 <- .v(variable[[2L]])
      
      var <- variable
      variable <- title
    } else {
      title <- variable
      idxC <- !is.na(dataset[[.v(variable)]])
    
      if (hasGrouping) {
        group1 <- dataset[idxG1 && idxC, .v(variable)]
        group2 <- dataset[idxG2 && idxC, .v(variable)]
        subDataSet <- dataset[idxC, .v(c(variable, grouping))]
      } else {
        # group 1 is empty [numeric(0)]
        group1 <- dataset[idxC, .v(variable)]   
        group1 <- group1 - options$mu
      }
    }
    
    if (!is.null(equivalenceSequentialContainer[[variable]]))
      next
    
    results <- equivalenceBayesianTTestResults[[variable]]
    
    equivalenceTTestSequential <- createJaspPlot(title = title, width = 480, height = 320)

    equivalenceSequentialContainer[[variable]] <- equivalenceTTestSequential
    
    if (paired) {
      equivalenceTTestSequential$dependOn("pairs")   # optionContainsValue = list(pairs = unname(options$pairs[p1])))  
    } else {
      equivalenceTTestSequential$dependOn(optionContainsValue = list("variables" = variable))
    }

    if (is.null(results$status)) {
      
      if (options[["effectSizeStandardized"]] == "informative") {
        equivalenceTTestSequential$setError(gettext("Sequential analysis robustness check plot currently not supported for informed prior."))
        next
      }
      
      # Make an error when BF is either 0 or inf., then no plot possible
      BF <- results$bfEquivalence / results$bfNonequivalence
      if (BF == 0 || BF == Inf) {
        equivalenceTTestPriorPosterior$setError("Currently a plot with a Bayes factor of Inf or 0 is not supported")
        return()
      }
      
      obj <- try(.plotEquivalenceSequentialBF.ttest(
        x                   = group1,
        y                   = group2,
        oneSided            = FALSE,
        rscale              = options$priorWidth,
        paired              = paired,
        plotDifferentPriors = options[["plotSequentialAnalysisRobustness"]],
        subDataSet          = subDataSet,
        level1              = g1,
        level2              = g2,
        nullInterval        = c(options$lowerbound, options$upperbound),
        options             = options))
      
      if (isTryError(obj)) {
        equivalenceTTestSequential$setError(.extractErrorMessage(obj))
      } else {
        equivalenceTTestSequential$plotObject <- obj
      }
    } else {
      # place error in plot
      equivalenceTTestSequential$setError(results$errorFootnotes)
    }
  }
}

# changed this function
.plotEquivalenceSequentialBF.ttest <- function(x = NULL, y = NULL, paired = FALSE, BF10post, formula = NULL, data = NULL, rscale = 1, oneSided = FALSE,
  plotDifferentPriors = FALSE, BFH1H0 = TRUE, dontPlotData = FALSE, level1 = NULL, level2 = NULL,
  subDataSet = NULL, nullInterval = c(-Inf, Inf), options) {
  
  r <- rscale
  
  if (is.null(y) || paired) {
    BF10  <- vector("numeric", max(length(x), length(y)))
    BF10w <- vector("numeric", max(length(x), length(y)))
    BF10u <- vector("numeric", max(length(x), length(y)))
    
    idData <- 1
    
    if (is.null(y)) {
      ind <- which(x == x[1])
      idData <- sum((ind + 1) - (1:(length(ind))) == 1)
    } else {
      idData <- 1
      for (i in 2:(min(c(length(x), length(y))))) {
        previous  <- c(x[i-1], y[i-1])
        
        if (all(c(x[i], y[i]) == previous)) {
          idData <- idData + 1
        } else if (x[i] == y[i]) {
          idData <- idData + 1
        } else {
          break
        }
      }
    }
    
    BF10[1:idData] <- 1
    BF10w[1:idData] <- 1
    BF10u[1:idData] <- 1
    
    if (idData < length(x)) {
      i <- idData + 1
    } else {
      i <- idData
    }
    
    if (idData < length(y)) {
      j <- idData + 1
    } else {
      j <- idData
    }
    
    k <- idData + 1
    
    while ((i <= length(x) | j <= length(y)) && k <= length(BF10)) {
      bfObject <- .generalEquivalenceTtestBF(x = x[1:i], y = y[1:j], paired = paired, options = options)
      bfEquivalence <- bfObject[["bfEquivalence"]]
      bfNonequivalence <- bfObject[["bfNonequivalence"]]
      
      BF10[k] <- bfEquivalence / bfNonequivalence
      k <- k + 1
      
      if (i < length(x)) {
        i <- i + 1
      }
      
      if (j < length(y)) {
        j <- j + 1
      }
    }
    
    BF10 <- BF10[is.finite(BF10)]
    
    if (plotDifferentPriors) {
      if (idData < length(x)) {
        i <- idData + 1
      } else {
        i <- idData
      }
      
      if (idData < length(y)) {
        j <- idData + 1
      } else {
        j <- idData
      }
      
      k <- idData + 1
      
      while ((i <= length(x) | j <= length(y)) && k <= length(BF10u)) {
        BF <- BayesFactor::ttestBF(x = x[1:i], y = y[1:j], paired = paired, rscale = "ultrawide", nullInterval = nullInterval)
        BF10u[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = FALSE)[1, "bf"] / BayesFactor::extractBF(BF, logbf = FALSE, onlybf = FALSE)[2, "bf"]
        
        k <- k + 1
        
        if (i < length(x)) {
          i <- i + 1
        }
        
        if (j < length(y)) {
          j <- j + 1
        }
      }
      
      BF10u <- BF10u[is.finite(BF10u)]
      
      if (idData < length(x)) {
        i <- idData + 1
      } else {
        i <- idData
      }
      
      if (idData < length(y)) {
        j <- idData + 1
      } else {
        j <- idData
      }
      
      k <- idData + 1
      
      while ((i <= length(x) | j <= length(y)) && k <= length(BF10w)) {
        
        BF <- BayesFactor::ttestBF(x = x[1:i], y = y[1:j], paired = paired, rscale = "wide", nullInterval = nullInterval)
        BF10w[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"] / BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[2, "bf"]
          
        k <- k + 1
        
        if (i < length(x)) {
          i <- i + 1
        }
        
        if (j < length(y)) {
          j <- j + 1
        }
      }
      
      BF10w <- BF10w[is.finite(BF10w)]
    }
    
  } else if (!is.null(y) && !paired) {
    
    idData <- 1
    
    xx <- numeric()
    yy <- numeric()
    
    BF10 <- vector("numeric", nrow(subDataSet))
    BF10w <- vector("numeric", nrow(subDataSet))
    BF10u <- vector("numeric", nrow(subDataSet))
    
    for (i in seq_len(nrow(subDataSet))) {
      if (subDataSet[i, 2] == level1) {
        xx <- c(xx, subDataSet[i, 1])
      } else if (subDataSet[i, 2] == level2) {
        yy <- c(yy, subDataSet[i, 1])
      }
      
      if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {
        
        bfObject <- .generalEquivalenceTtestBF(x = xx, y = yy, paired = paired, options = options)
        bfEquivalence <- bfObject[["bfEquivalence"]]
        bfNonequivalence <- bfObject[["bfNonequivalence"]]
        
        BF10[i] <- bfEquivalence / bfNonequivalence
        
      } else {
        BF10[i] <- 1
      }
    }

    if (plotDifferentPriors) {
      xx <- numeric()
      yy <- numeric()
      
      for (i in seq_len(nrow(subDataSet))) {
        if (subDataSet[i, 2] == level1) {
          xx <- c(xx, subDataSet[i, 1])
        } else if (subDataSet[i, 2] == level2) {
          yy <- c(yy, subDataSet[i, 1])
        }
        
        if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {
          BF <- BayesFactor::ttestBF(x = xx, y = yy, paired = paired, rscale = "ultrawide", nullInterval = nullInterval)
          BF10u[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"] / BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[2, "bf"]
        } else {
          BF10u[i] <- 1
        }
      }
      
      xx <- numeric()
      yy <- numeric()
      
      for (i in seq_len(nrow(subDataSet))) {
        if (subDataSet[i, 2] == level1) {
          xx <- c(xx, subDataSet[i, 1])
        } else if (subDataSet[i, 2] == level2) {
          yy <- c(yy, subDataSet[i, 1])
        }
        
        if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {
          BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= "wide", nullInterval = nullInterval)
          BF10w[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"] / BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[2, "bf"]
        } else {
          BF10w[i] <- 1
        }
      }
    }
  }

  if (plotDifferentPriors) {
    dfLines <- data.frame(
      x = seq_along(BF10),
      y = c(BF10, BF10u, BF10w),
      g = factor(rep(c("user", "ultrawide", "wide"), c(length(BF10), length(BF10u), length(BF10w))),
                 levels = c("user", "wide", "ultrawide"))
    )
  } else {
    dfLines <- data.frame(
      x = seq_along(BF10),
      y = BF10
    )
  }

  BF <- BF10[length(BF10)]
  if (BFH1H0) {
    bftype <- "BF10"
  } else {
    dfLines$y <- 1 / dfLines$y
    bftype <- "BF01"
  }
  dfLines$y <- log(dfLines$y)
  
  plot <- JASPgraphs::PlotRobustnessSequential(
    dfLines         = dfLines,
    xName           = gettext("n"),
    yName           = "BF[phantom()%in%phantom()%notin%phantom()]",
    BF              = BF,
    bfType          = bftype,
    hypothesis      = "equal",
    evidenceLeveltxt = FALSE,
   # evidenceTxt     = JASPgraphs::parseThis("H[phantom()%in%phantom()]"),
    arrowLabel      = JASPgraphs::parseThis(c("H[phantom()%notin%phantom()]", "H[phantom()%in%phantom()]")),
    bfSubscripts    = JASPgraphs::parseThis(c("BF[phantom()%in%phantom()%notin%phantom()]",
                                              "BF[phantom()%notin%phantom()%in%phantom()]")),
    pizzaTxt        = JASPgraphs::parseThis(c("data~'|'~H[phantom()%notin%phantom()]", "data~'|'~H[phantom()%in%phantom()]")))
  
  return(plot)
}

