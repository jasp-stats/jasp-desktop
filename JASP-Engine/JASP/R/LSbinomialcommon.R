#
# Copyright (C) 2019 University of Amsterdam
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

# data load and summary
.readyBinomialLS       <- function(options){
  # are data ready
  if (options[["dataType"]] == "dataCounts"){
    
    ready <- TRUE
    
  } else if (options[["dataType"]] == "dataSequence"){
    
    if (nchar(options[["data_sequence"]]) > 0){
      
      if (length(options[["key_success_Seq"]]) == 0){
        ready <- FALSE
      } else {
        ready <- TRUE
      }
      
    } else {
      ready <- TRUE
    }
    
  } else if (options[["dataType"]] == "dataVariable"){
    
    if (options[["selectedVariable"]] != ""){
      
      if (length(options[["key_success_Var"]]) == 0){
        ready <- FALSE
      } else {
        ready <- TRUE
      }
      
    } else {
      ready <- TRUE
    }
    
  }
  
  # are priors ready
  ready <- c(ready, length(options[["priors"]]) > 0)
  
  return(ready)
}
.readDataBinomialLS    <- function(dataset, options){
  
  data <- list()
  
  if (options[["dataType"]] == "dataCounts"){
    
    data$y <- NULL
    data$nSuccesses <- options[["nSuccesses"]]
    data$nFailures  <- options[["nFailures"]]
    
  } else {
    
    if ((options[["dataType"]]== "dataVariable" && options[["selectedVariable"]] == "") |
        (options[["dataType"]]== "dataSequence" && options[["data_sequence"]] == "")){
      
      data$y <- NULL
      
    } else {
      
      if (options[["dataType"]]== "dataSequence"){
        
        temp_y <- .clean_sequence(options[["data_sequence"]])
        
      } else if (options[["dataType"]] == "dataVariable"){
        
        # this is stupidly written #rework
        if (!is.null(dataset)){
          temp_y <- dataset
        } else {
          temp_y <- .readDataSetToEnd(columns = options[["selectedVariable"]])[,1]
        }
        
      }
      
      data$y <- .cleanDataBinomialLS(temp_y, options)
      
    }
    
    data$nSuccesses <- sum(data$y == 1)
    data$nFailures   <- sum(data$y == 0)
    
  } 
  
  return(data)
  
}
.cleanDataBinomialLS   <- function(x, options){
  
  # doubling the menu allows to store the keys while user switches between different input methods
  if (options[["dataType"]] == "dataSequence"){
    key_success <- options[["key_success_Seq"]]
    key_failure <- options[["key_failure_Seq"]]
  } else {
    key_success <- options[["key_success_Var"]]
    key_failure <- options[["key_failure_Var"]]
  }
  
  x <- na.omit(x)
  x <- as.character(x)
  
  # treat everything else then success as a failure ifonly successes are supplied
  if (length(key_failure) == 0){
    
    temp_ks <- x %in% key_success
    
    x[temp_ks]  <- 1
    x[!temp_ks] <- 0
    
  } else {
    # use only variables specified in successes or failures
    
    x <- x[x %in% c(key_success, key_failure)]
    
    temp_ks <- x %in% key_success
    temp_kf <- x %in% key_failure
    
    x[temp_ks] <- 1
    x[temp_kf] <- 0
    
  }
  
  return(as.numeric(x))
}
.summaryBinomialLS     <- function(jaspResults, data, options, analysis){
  
  if (is.null(jaspResults[["summaryContainer"]])){
    summaryContainer <- createJaspContainer("Data Input")
    summaryContainer$position <- 1
    jaspResults[["summaryContainer"]] <- summaryContainer 
  } else {
    summaryContainer <- jaspResults[["summaryContainer"]]
  }
  
  
  if (options[["introText"]] && is.null(summaryContainer[['summaryText']])){
    
    summaryText <- createJaspHtml()
    summaryText$dependOn(c("introText", "dataType"))
    summaryText$position <- 1
    
    summaryText[['text']] <- .explanatoryTextLS("data", options, analysis)
    
    summaryContainer[['summaryText']] <- summaryText    
  }
  
  
  if (options[["dataSummary"]] && options[["dataType"]] != "dataCounts" && is.null(summaryContainer[['summaryTable']])){
    
    summaryTable <- createJaspTable(title = gettext("Data Summary"))
    
    summaryTable$position <- 2
    summaryTable$dependOn(c("dataSummary", .BinomialLS_data_dependencies))
    
    summaryTable$addColumnInfo(name = "variable",   title = "",                     type = "string")
    summaryTable$addColumnInfo(name = "counts",     title = gettext("Counts"),      type = "integer")
    summaryTable$addColumnInfo(name = "proportion", title = gettext("Proportion"),  type = "number")
    
    summaryTable$setExpectedSize(3)
    
    summaryContainer[["summaryTable"]] <- summaryTable
    
    if (.readyBinomialLS(options)[1]){
      summaryTable$addRows(list(variable   = gettext("Successes"), 
                                counts     = data$nSuccesses, 
                                proportion = ifelse(is.nan(data$nSuccesses / (data$nSuccesses + data$nFailures)), "",
                                                    data$nSuccesses / (data$nSuccesses + data$nFailures))))
      summaryTable$addRows(list(variable   = gettext("Failures"),
                                counts     = data$nFailures, 
                                proportion = ifelse(is.nan(data$nFailures / (data$nSuccesses + data$nFailures)), "",
                                                    data$nFailures / (data$nSuccesses + data$nFailures))))
      summaryTable$addRows(list(variable   = gettext("Total"),
                                counts     = data$nSuccesses + data$nFailures, 
                                proportion = ""))
    }
  }
  
  return()
}

# computational functions
.estimateBinomialLS         <- function(data, prior){
  
  if (prior[["type"]] == "spike"){
    
    output <- list(
      distribution = gettextf("spike at %s", prior[["parPoint_inp"]]),
      mean         = prior[["parPoint"]],
      median       = prior[["parPoint"]],
      mode         = prior[["parPoint"]],
      lCI          = prior[["parPoint"]],
      uCI          = prior[["parPoint"]]
    )
    
    return(output)
    
  } else if (prior[["type"]] == "beta"){
    
    # in order to keep decimals as decimals ifuser fills them that way
    if (!is.na(as.numeric(prior[["parAlpha_inp"]]))){
      text_Alpha <- prior[["parAlpha"]] + data$nSuccesses
    } else {
      text_Alpha <- MASS::fractions(prior[["parAlpha"]] + data$nSuccesses)
    }
    if (!is.na(as.numeric(prior[["parBeta_inp"]]))){
      text_Beta <- prior[["parBeta"]] + data$nFailures
    } else {
      text_Beta <- MASS::fractions(prior[["parBeta"]] + data$nFailures)
    }
    
    output <- list(
      distribution = gettextf("beta (%s, %s)", text_Alpha, text_Beta),
      mean         = (prior[["parAlpha"]] + data$nSuccesses) / (prior[["parAlpha"]] + data$nSuccesses + prior[["parBeta"]] + data$nFailures),
      median       = qbeta(.5,   prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures),
      mode         = .modeBetaLS(prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures),
      lCI          = qbeta(.025, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures),
      uCI          = qbeta(.975, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)
    )
    
    
    return(output)
  }
}
.testBinomialLS             <- function(data, priors){
  
  names     <- rep(NA, length(priors))
  prior     <- rep(NA, length(priors))
  log_lik   <- rep(NA, length(priors))
  
  obs_prop  <- data$nSuccesses / (data$nSuccesses + data$nFailures)
  
  for(i in 1:length(priors)){
    
    temp_prior <- priors[[i]]
    prior[i]   <- temp_prior$PH
    names[i]   <- temp_prior$name
    
    if (data$nSuccesses + data$nFailures > 0){
      
      if (temp_prior[["type"]] == "spike"){
        
        log_lik[i]   <- stats::dbinom(data$nSuccesses, data$nSuccesses + data$nFailures, temp_prior[["parPoint"]], log = TRUE)
        
      } else if (temp_prior[["type"]] == "beta"){
        
        log_lik[i]   <- extraDistr::dbbinom(data$nSuccesses, data$nSuccesses + data$nFailures, 
                                            temp_prior[["parAlpha"]], temp_prior[["parBeta"]], log = TRUE)
        
      }
      
      
    }
    
  }
  
  if (data$nSuccesses + data$nFailures > 0){
    
    PH_log_lik <- log(prior) + log_lik
    norm_const <- log(sum(exp(PH_log_lik))) 
    posterior  <- exp(PH_log_lik - norm_const)
    
  } else {
    
    posterior  <- prior
    
  }
  
  return(data.frame(
    prior       = prior,
    log_lik     = log_lik,
    posterior   = posterior,
    name        = names
  ))
  
}
.predictBinomialLS          <- function(data, prior, options, prop = FALSE){
  
  if (prop) d <- options[["predictionN"]] else d <- 1
  
  if (prior[["type"]] == "spike"){
    
    output <- list(
      distribution = gettextf("binomial (%i, %s)", options[["predictionN"]], prior[["parPoint_inp"]]),
      mean         = prior[["parPoint"]] * options[["predictionN"]] / d,
      median       = qbinom(.5, options[["predictionN"]], prior[["parPoint"]]) / d,
      mode         = .modeBinomialLS(options[["predictionN"]], prior[["parPoint"]], prop = prop),
      lCI          = qbinom(0.025, options[["predictionN"]], prior[["parPoint"]]) / d,
      uCI          = qbinom(0.975, options[["predictionN"]], prior[["parPoint"]]) / d
    )
    
    return(output)
    
  } else if (prior[["type"]] == "beta"){
    
    # in order to keep decimals as decimals ifuser fills them that way
    if (!is.na(as.numeric(prior[["parAlpha_inp"]]))){
      text_Alpha <- prior[["parAlpha"]] + data$nSuccesses
    } else {
      text_Alpha <- MASS::fractions(prior[["parAlpha"]] + data$nSuccesses)
    }
    if (!is.na(as.numeric(prior[["parBeta_inp"]]))){
      text_Beta <- prior[["parBeta"]] + data$nSuccesses
    } else {
      text_Beta <- MASS::fractions(prior[["parBeta"]] + data$nFailures)
    }
    
    output <- list(
      distribution = gettextf("beta-binomial (%i, %s, %s)", options[["predictionN"]], text_Alpha, text_Beta),
      mean         = (prior[["parAlpha"]] + data$nSuccesses) * options[["predictionN"]] / (prior[["parAlpha"]] + data$nSuccesses + prior[["parBeta"]] + data$nFailures) / d,
      median       = .qbetabinomLS(.5, options[["predictionN"]], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) / d,
      mode         = .modeBetaBinomLS(options[["predictionN"]], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures, prop = prop),
      lCI          = .qbetabinomLS(0.025, options[["predictionN"]], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) / d,
      uCI          = .qbetabinomLS(0.975, options[["predictionN"]], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) / d
    )
    
    return(output)
  }
}
.predictBinomialValuesLS    <- function(data, prior, n){
  
  x <- 0:n
  
  if (prior[["type"]] == "spike"){
    y <- dbinom(x, n, prior[["parPoint"]])
  } else if (prior[["type"]] == "beta"){
    y <- sapply(x, function(s)extraDistr::dbbinom(s, n, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures))
  }
  
  return(y)
}
.betaHDILS                  <- function(alpha, beta, coverage){
  
  if (alpha == 1 & beta == 1){
    
    # do central in case that alpha & beta == 1, the interval is weird otherwise
    HDI <- c(.5 - coverage/2, .5 + coverage/2)
    
  } else if (alpha >= 1 & beta >= 1){
    
    HDI <- HDInterval::hdi(qbeta, coverage, shape1 = alpha, shape2 = beta)
    
  } else {
    # new density approach - instead of pdf, use scaled cdf
    # x_density <- seq(0, 1, .00001)
    # y_density <- dbeta(x_density, shape1 = alpha, shape2 = beta)
    # y_density[c(1, length(y_density))] <- 0
    
    den_beta <- .dbetaLS(alpha, beta)
    class(den_beta) <- "density"
    
    HDI <- HDInterval::hdi(den_beta, coverage, allowSplit = T)
    HDI <- round(HDI, 5) # dealing with precission 
    HDI[HDI[,1] <= min(den_beta$x),1] <- 0
    HDI[HDI[,2] >= max(den_beta$x),2] <- 1
    
  }
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.binomialHDILS              <- function(n, theta, coverage){
  
  # this doesn't work in some cases for some reason
  # HDI <- HDInterval::hdi(qbinom, coverage, size = n, prob = theta)
  
  x_density <- 0:n
  y_density <- dbinom(x_density, n, theta)
  y_density <- round(y_density, 10)
  den_binom <- list(
    x = x_density,
    y = y_density
  )
  class(den_binom) <- "density"
  HDI <- HDInterval::hdi(den_binom, coverage, allowSplit = T)
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.betabinomialHDILS          <- function(n, alpha, beta, coverage){
  
  if (alpha == 1 & beta == 1){
    
    HDI <-     x <- c(
      .qbetabinomLS((1 - coverage)/2 + 1e-5,  n, alpha, beta),
      .qbetabinomLS(1 - (1 - coverage)/2,     n, alpha, beta)
    )
    
  } else {
    
    x_density <- 0:n
    y_density <- sapply(x_density,function(s)extraDistr::dbbinom(s, n, alpha, beta))
    y_density <- round(y_density, 10)
    den_beta <- list(
      x = x_density,
      y = y_density
    )
    class(den_beta) <- "density"
    HDI <- HDInterval::hdi(den_beta, coverage, allowSplit = T)
    
  }
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.qbetabinomLS               <- function(p, n, alpha, beta){
  # the rounding is due to numerical imprecission in extraDistr::pbbinom
  return(c(0:n)[match(TRUE, round(sapply(0:n, function(s)extraDistr::pbbinom(s, n, alpha, beta)),10) >= p)])
}
.betaSupportLS              <- function(alpha, beta, successses, failures, BF){
  
  # old way
  # x_seq  <- seq(.001,.999,.001)
  # bf_res <- dbeta(x_seq, alpha + successses, beta + failures)/dbeta(x_seq, alpha, beta)
  
  temp_post  <- .dbetaLS(alpha + successses, beta + failures)
  temp_prior <- .dbetaLS(alpha, beta)
  
  x_seq   <- temp_post$x
  y_post  <- temp_post$y
  y_prior <- temp_prior$y
  
  bf_res  <- y_post/y_prior
  
  TF_seq <- bf_res>BF
  
  support <- .aproximateSupportLS(x_seq, TF_seq)
  
  support$lCI[support$lCI == min(x_seq)] <- 0
  support$uCI[support$uCI == max(x_seq)] <- 1
  
  return(support)
  
}
.modeBetaLS                 <- function(alpha, beta){
  if (alpha == 1 && beta == 1)
    return("[0, 1]")
  else if (alpha < 1 && beta < 1 && alpha == beta)
    return("{0, 1}")
  else if (alpha <= 1 && beta > 1)
    return(0)
  else if (alpha  > 1 && beta <= 1)
    return(1)
  else
    return((alpha-1)/(alpha+beta-2))
}
.modeBinomialLS             <- function(N, p, prop = FALSE){
  if (prop) d <- N else d <- 1
  if (p == 0)
    return(0)
  else if (p == 1)
    return(N / d)
  else if (.is.wholenumber((N + 1)*p))
    return(paste0("{", ((N + 1)*p-1) / d, ", ", ((N + 1)*p) / d,  "}"))
  else
    return(floor((N + 1)*p) / d)
}
.modeBetaBinomLS            <- function(N, alpha, beta, prop = FALSE){
  if (prop) d <- N else d <- 1
  if (alpha == 1 && beta == 1)
    return(paste0("[0, ", N / d,"]"))
  else if (alpha < 1 && beta < 1 && alpha == beta)
    return(paste0("{0, ", N / d,"}"))
  else if (alpha <= 1 && beta > 1)
    return(0)
  else if (alpha  > 1 && beta <= 1)
    return(N / d)
  else {
    temp_d   <- extraDistr::dbbinom(0:N, N, alpha, beta)
    temp_med <- c(0:N)[temp_d == max(temp_d)]
    if (length(temp_med) > 1){
      return(paste0("{", paste(temp_med / d, collapse = ", "), "}"))
    } else {
      return(temp_med / d)
    }
  }
}
.marginalCentralBinomialLS  <- function(density, spikes, coverage, l.bound = 0, u.bound = 1, density_discrete = FALSE){
  
  f(!is.null(density))
  if (!density_discrete)density$y <- density$y/nrow(density)    
  else
    density <- data.frame("y" = NULL, "x" = NULL)
  
  if (length(spikes) != 0){
    for(i in 1:length(spikes)){
      density <- rbind(density[density$x <= spikes[[i]]$x,], spikes[[i]], density[spikes[[i]]$x < density$x,])
    }
  }
  
  cs  <- cumsum(density$y)
  css <- rev(cumsum(rev(density$y)))
  
  lower <- density$x[cs  > (1-coverage)/2]
  lower <- lower[1]
  if (is.na(lower))lower <- l.bound
  
  upper <- density$x[(1-coverage)/2  <  css]
  upper <- upper[length(upper)]
  if (length(upper) == 0)upper <- u.bound
  
  return(cbind.data.frame(x_start = lower, x_end = upper, g = "central", coverage = coverage))
}
.marginalHPDBinomialLS      <- function(density, spikes, coverage, l.bound = 0, u.bound = 1, density_discrete = FALSE){
  
  HDI      <- NULL
  temp.cov <- 0
  
  # spikes have always the highest density - use them first
  if (length(spikes) != 0){
    spikes.df   <- do.call(rbind, spikes)
    spikes.df   <- spikes.df[order(spikes.df$y),]
    
    i        <- 1
    while(temp.cov < coverage & i <= nrow(spikes.df)){
      HDI      <- rbind(HDI, rep(spikes.df$x[i],2))
      temp.cov <- temp.cov + spikes.df$y[i]
      i        <- i + 1
    }
    
    # remove duplicious spikes
    HDI <- HDI[!duplicated(HDI[,1]),]
    HDI <- matrix(as.vector(HDI), ncol = 2)
  }
  
  # add continous density
  if (!is.null(density) & temp.cov < coverage){
    
    # ifwe have only spikes and density, the probability mass of density is 1 - spikes
    sum_dens_prob <- 1 - temp.cov
    # proportion of density needed to finish the coverage
    prop_density  <- (coverage-temp.cov)/sum_dens_prob
    
    # deal with flat density
    if (all(round(density$y,10) == round(density$y[1],10))){
      
      if (density_discrete){
        n.bars  <- u.bound-l.bound+1
        HDI2    <- c((u.bound-l.bound)/2 - prop_density*n.bars/2 + .5, (u.bound-l.bound)/2 + prop_density*n.bars/2 - .5)
        HDI2[1] <- floor(HDI2[1])
        HDI2[2] <- ceiling(HDI2[2])
      } else {
        HDI2 <- c((u.bound-l.bound)/2-(u.bound-l.bound)*prop_density/2, (u.bound-l.bound)/2+(u.bound-l.bound)*prop_density/2)
      }
      
    } else {
      
      den_marginal <- list(
        x = density$x,
        y = density$y
      )
      class(den_marginal) <- "density"
      HDI2 <- HDInterval::hdi(den_marginal, prop_density, allowSplit = T)
      
    }
    
    HDI2 <- matrix(as.vector(HDI2), ncol = 2)
    HDI2[HDI2 >= u.bound - .001] <- u.bound
    HDI2[HDI2 <= l.bound + .001] <- l.bound
    
    # remove spikes covered by density
    if (length(spikes) != 0){
      for(i in nrow(HDI):1){
        if (any(HDI[i,1] >= HDI2[,1] & HDI2[,2] >= HDI[i,1]))HDI <- HDI[-i,]
      }
    }
    
    HDI  <- rbind(HDI, HDI2)
    
  }
  
  HDI <- HDI[order(HDI[,1]),]
  HDI <- matrix(as.vector(HDI), ncol = 2)
  
  return(cbind.data.frame(x_start = HDI[,1], x_end = HDI[,2], g = "HPD", coverage = coverage))
}
.marginalCustomBinomialLS   <- function(density, spikes, lCI, uCI, density_discrete = FALSE){
  
  if (!is.null(density))
    if (!density_discrete)density$y <- density$y/nrow(density)    
    else
      density <- data.frame("y" = NULL, "x" = NULL)
    
    if (length(spikes) != 0){
      for(i in 1:length(spikes)){
        density <- rbind(density[density$x <= spikes[[i]]$x,], spikes[[i]], density[spikes[[i]]$x < density$x,])
      }
    }
    
    coverage <- sum(density$y[density$x >= lCI & density$x <= uCI])
    
    return(cbind.data.frame(x_start = lCI, x_end = uCI, g = "custom", coverage = coverage))
}
.marginalSupportBinomialLS  <- function(data, priors, post_density, post_spikes, BF){
  
  # posterior spikes and density are already computed, we just need to get priors
  prior_spikes   <- list()
  density_i      <- 0
  prior_density  <- NULL
  temp_results   <- .testBinomialLS(data, priors)
  for(i in 1:length(priors)){
    if (priors[[i]]$type == "spike"){
      prior_spikes <- c(
        prior_spikes, 
        list(data.frame(y = priors[[i]]$PH, x = priors[[i]]$parPoint, g = "__marginal"))
      )
    } else if (priors[[i]]$type == "beta"){
      dfLinesPP   <- .dataLinesBinomialLS(data, priors[[i]])
      dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Prior",]
      dfLinesPP$y <- exp(log(dfLinesPP$y)+log(temp_results[i, "prior"]))
      dfLinesPP$g <- priors[[i]]$name
      
      if (density_i == 0){
        prior_density   <- dfLinesPP
      } else {
        prior_density$y <- prior_density$y + dfLinesPP$y
      }
      density_i <- density_i + 1
    }
  }
  
  
  # compute BFs
  bf_spikes <- list()
  if (!is.null(prior_density)){
    bf_density <- data.frame(
      y = exp(log(post_density$y) - log(prior_density$y)),
      x = post_density$x
    )
    bf_density$y[post_density$y == 0] <- 0 # dealing with NaN's due to density aproximation
  } else {
    bf_density <- data.frame(y = NULL, x = NULL)
  }
  if (length(prior_spikes) != 0){
    for(i in 1:length(prior_spikes)){
      bf_spikes[[i]] <- data.frame(
        x = post_spikes[[i]]$x,
        y = post_spikes[[i]]$y / prior_spikes[[i]]$y 
      )
    }
  }
  
  
  if (length(bf_spikes) != 0){
    for(i in 1:length(bf_spikes)){
      bf_density <- rbind(bf_density[bf_density$x <= bf_spikes[[i]]$x,], bf_spikes[[i]], bf_density[bf_spikes[[i]]$x < bf_density$x,])
    }
  }
  
  
  support <- .aproximateSupportLS(bf_density$x, bf_density$y > BF)
  
  support$lCI[support$lCI == .0005] <- 0
  support$uCI[support$uCI == .9995] <- 1
  
  if (nrow(support) > 0){
    lCI      <- support$lCI
    uCI      <- support$uCI
    coverage <- 666 # not implemented
  } else {
    lCI      <- NA
    uCI      <- NA
    coverage <- 0
  }
  
  dat       <- data.frame(x_start = lCI, x_end = uCI, g = "support", coverage = coverage, BF = BF)
  
  return(dat)
}
.dbetaLS                    <- function(alpha, beta){
  
  y <- c(
    pbeta(.001, alpha, beta)*1000,
    dbeta(seq(.0015, .9985, .001), alpha, beta),
    pbeta(.999, alpha, beta, lower.tail = F)*1000
  )
  x <- c(.0005, seq(.0015, .9985, .001), .9995)
  
  return(list(
    x = x,
    y = y
  ))
}
.is.wholenumber             <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# plotting functions
.dataLinesBinomialLS        <- function(data, prior){
  
  x_seq   <- round(seq(.001, .999, .001), 5)
  y_post  <- round((pbeta(x_seq + .001, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) - pbeta(x_seq - .001, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures))*(1/(.001*2)),10)
  y_prior <- round((pbeta(x_seq + .001, prior[["parAlpha"]], prior[["parBeta"]]) - 
                      pbeta(x_seq - .001, prior[["parAlpha"]], prior[["parBeta"]]))
                   *(1/(.001*2)),10)
  
  linesGroup <- c(y_post, y_prior)
  thetaGroup <- c(x_seq, x_seq)
  nameGroup  <- c(rep("Posterior", length(x_seq)), rep("Prior", length(x_seq)))
  
  dat        <- data.frame(x = thetaGroup, y = linesGroup, g = nameGroup)
  return(dat)
}
.dataHPDBinomialLS          <- function(data, prior, coverage, n = NULL, type = c("parameter", "prediction")){
  
  if (type == "parameter"){
    
    if (prior[["type"]] == "spike")
      x <- matrix(prior[["parPoint"]], ncol = 2, nrow = 1)
    else if (prior[["type"]] == "beta")
      x <- .betaHDILS(prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures, coverage)
    
    
  } else if (type == "prediction"){
    
    if (prior[["type"]] == "spike")
      x <- .binomialHDILS(n, prior[["parPoint"]], coverage)
    else if (prior[["type"]] == "beta")
      x <- .betabinomialHDILS(n, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures, coverage)
    
    
  }
  
  dat       <- data.frame(x_start = x[,1], x_end = x[,2], g = "HPD", coverage = coverage)
  return(dat)
}
.dataCentralBinomialLS      <- function(data, prior, coverage, n = NULL, type = c("parameter", "prediction")){
  
  if (type == "parameter"){
    
    if (prior[["type"]] == "spike")
      x <- matrix(prior[["parPoint"]], ncol = 2, nrow = 1)
    else if (prior[["type"]] == "beta")
      x <- qbeta(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)
    
    
  } else if (type == "prediction"){
    # adding  (+ 1e-5) to the first lower bound because the quantile function is not inverse of cumulatiove
    # distribution function and the lower boundary is not part of the interval. Wanted to write custom 
    # quantile function for the lower bound, however, the aproximation in R reusults in inability to fix
    # the borderline cases: CI for BinomialLS distribution with 3 trials, probabily .5 and coverage 75% 
    if (prior[["type"]] == "spike")
      x <- qbinom(c((1 - coverage)/2 + 1e-5, 1 - (1 - coverage)/2), n, prior[["parPoint"]])
    else if (prior[["type"]] == "beta")
      x <- c(
        .qbetabinomLS((1 - coverage)/2 + 1e-5, n, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures),
        .qbetabinomLS(1 - (1 - coverage)/2,     n , prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)
      )
    
    
  }
  
  dat       <- data.frame(x_start = x[1], x_end = x[2], g = "central", coverage = coverage)
  return(dat)
}
.dataCustomBinomialLS       <- function(data, prior, lCI, uCI, n = NULL, type = c("parameter", "prediction")){
  
  if (type == "parameter"){
    
    if (prior[["type"]] == "spike")
      coverage <- ifelse(lCI <= prior[["parPoint"]] & prior[["parPoint"]] <= uCI, 1, 0)
    else if (prior[["type"]] == "beta")
      coverage <- pbeta(uCI, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) -
        pbeta(lCI, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)
    
    
  } else if (type == "prediction"){
    
    if (prior[["type"]] == "spike")
      coverage <- sum(sapply(lCI:uCI, function(s)dbinom(s, n, prior[["parPoint"]])))
    else if (prior[["type"]] == "beta")
      coverage <- sum(sapply(lCI:uCI, function(s)
        extraDistr::dbbinom(s, n, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)))
    
  }
  
  dat       <- data.frame(x_start = lCI, x_end = uCI, g = "custom", coverage = coverage)
  return(dat)
}
.dataSupportBinomialLS      <- function(data, prior, BF){
  
  if (prior[["type"]] == "spike"){
    coverage <- 1
    lCI      <- prior[["parPoint"]]
    uCI      <- prior[["parPoint"]]
  } else if (prior[["type"]] == "beta"){
    
    x        <- .betaSupportLS(prior[["parAlpha"]], prior[["parBeta"]], data$nSuccesses, data$nFailures, BF)
    
    if (nrow(x) > 0){
      lCI      <- x$lCI
      uCI      <- x$uCI
      coverage <- sum(sapply(1:length(lCI),function(i){
        pbeta(uCI[i], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) - 
          pbeta(lCI[i], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)
      }))
    } else {
      lCI      <- NA
      uCI      <- NA
      coverage <- 0
    }
  }
  
  dat       <- data.frame(x_start = lCI, x_end = uCI, g = "support", coverage = coverage, BF = BF)
  return(dat)
}
.dataProportionBinomialLS   <- function(data){
  
  theta <- data$nSuccesses / (data$nSuccesses + data$nFailures)
  dat   <- data.frame(x = theta, y = 0, g = "Sample proportion")
  
  return(dat)
}
.dataHistBinomialLS         <- function(data, prior, n){
  
  x <- 0:n
  
  if (prior[["type"]] == "spike")
    y <- dbinom(x, n, prior[["parPoint"]])
  else if (prior[["type"]] == "beta")
    y <- sapply(x, function(s)extraDistr::dbbinom(s, n, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures))
  
  
  dat <- data.frame(x = x, y = y)
  return(dat)
}
.dataHistBinomialLS2        <- function(data, prior, n){
  
  x <- 0:n
  y <- .predictBinomialValuesLS(data, prior, n)
  
  x_new <- x[sort(rep(1:length(x),2))] + c(-.5, +.5)
  y_new <- y[sort(rep(1:length(x),2))]
  
  dat <- data.frame(x = x_new, y = y_new)
  return(dat)
}
.dataArrowBinomialLS        <- function(prior){
  dat       <- data.frame(x = prior[["parPoint"]], y_start = 0, y_end = 1, g = "Prior = Posterior")
  return(dat)
}
.dataPointEstimateBinomial  <- function(data, prior, N, type = c("parameter", "prediction"), estimate = c("mean", "median", "mode"), prop = FALSE){
  
  if (type == "parameter"){
    l <- .estimateBinomialLS(data, prior)[[estimate]]
    if (prior[["type"]] == "spike"){
      x <- .estimateBinomialLS(data, prior)[[estimate]]
      y <- 1
    } else if (prior[["type"]] == "beta"){
      if (estimate == "mode" && prior[["parAlpha"]] + data$nSuccesses == 1 && prior[["parBeta"]] + data$nFailures == 1){
        x <- NA
        y <- NA
      } else if (estimate == "mode" && prior[["parAlpha"]] + data$nSuccesses < 1 && prior[["parBeta"]] + data$nFailures < 1 &&
                 prior[["parAlpha"]] + data$nSuccesses == prior[["parBeta"]] + data$nFailures){
        x <- c(0, 1)
        y <- .dbetaLS(prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)
        y <- y$y[c(1, length(y$y))]
      } else {
        x <- .estimateBinomialLS(data, prior)[[estimate]]
        y <- dbeta(x, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)
      }
    }
    
  } else if (type == "prediction"){
    options <- list(predictionN = N)
    l <- .predictBinomialLS(data, prior, options, prop)[[estimate]]
    if (prop) d <- N else d <- 1
    
    if (prior[["type"]] == "spike"){
      if (.is.wholenumber((N + 1)*prior[["parPoint"]]) && estimate == "mode")
        x <- c( (N + 1)*prior[["parPoint"]], (N + 1)*prior[["parPoint"]]-1) / d
      else
        x <- .predictBinomialLS(data, prior, options, prop)[[estimate]]
      
      y <- dbinom(x * d, N, prior[["parPoint"]])
    } else if (prior[["type"]] == "beta"){
      
      x <- .predictBinomialLS(data, prior, options, prop)[[estimate]]
      if (estimate == "mode" && !is.numeric(x)){
        if (prior[["parAlpha"]] + data$nSuccesses == 1 && prior[["parBeta"]] + data$nFailures == 1)
          x <- c(NA)
        else if (prior[["parAlpha"]] + data$nSuccesses < 1 && prior[["parBeta"]] + data$nFailures < 1 &&
                 prior[["parAlpha"]] + data$nSuccesses == prior[["parBeta"]] + data$nFailures)
          x <- c(0, N) / d
        else {
          x <- extraDistr::dbbinom(0:N, N, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)
          x <- c(0:N)[x == max(x)] / d
        }
      }
      y <- extraDistr::dbbinom(x * d, N, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures)
    }
    
  }
  
  dat <- data.frame(x = x, y = y, estimate = estimate, l = l)
  return(dat)
}
.dataPointMarginalBinomial  <- function(data, options, all_lines, all_spikes, N, type = c("parameter", "prediction"), type2 = c("Prior", "Posterior"), estimate = c("mean", "median", "mode"), prop = FALSE){
  
  if (is.null(data))
    data <- list(
      "nSuccesses" = 0,
      "nFailures"  = 0
    )
  
  if (estimate == "median"){
    if (type == "prediction" && !is.null(all_lines))all_lines$y <- all_lines$y * length(all_lines$y)
    temp_df <- .marginalCentralBinomialLS(all_lines, all_spikes, coverage = 0)
    if (type == "prediction" && !is.null(all_lines))all_lines$y <- all_lines$y / length(all_lines$y)
    x <- temp_df$x_start
    if (length(all_spikes) > 0){
      spike_y <- sapply(all_spikes, function(s){
        temp_y <- s$y[s$x == x]
        if (length(temp_y) == 0)
          return(NA)
        else
          return(temp_y)
        
      })
      if (length(na.omit(spike_y)) != 0){
        spike_y <- max(spike_y, na.rm = T)
        y       <- spike_y
      } else
        spike_y <- NULL
      
    } else
      spike_y <- NULL
    
    if (is.null(spike_y))
      y <- all_lines$y[all_lines$x == x]
    
    
    return(data.frame(x = x, y = y, estimate = estimate, l = x, spike = !is.null(spike_y)))
    
  } else if (estimate == "mean"){
    
    temp_tests <- .testBinomialLS(data, options[["priors"]])
    
    if (type == "parameter"){
      
      temp_estimates <- sapply(options[["priors"]],function(prior).estimateBinomialLS(data, prior), simplify = F)
      temp_estimates <- do.call(rbind.data.frame, temp_estimates)
      
      x <- sum(temp_tests[,tolower(type2)] * temp_estimates[,"mean"])
      
      if (length(all_spikes) > 0){
        spike_y <- sapply(all_spikes, function(s){
          temp_y <- s$y[s$x == x]
          if (length(temp_y) == 0)
            return(NA)
          else
            return(temp_y)
          
        })
        if (length(na.omit(spike_y)) != 0){
          spike_y <- max(spike_y, na.rm = T)
          y       <- spike_y
        } else
          spike_y <- NULL
        
      } else
        spike_y <- NULL
      
      if (is.null(spike_y)){
        if (any(all_lines$x == x))
          y <- all_lines$y[all_lines$x == x]
        else
          y <- all_lines$y[which.max(all_lines$x > x)]/2 + all_lines$y[which.max(all_lines$x > x)-1]/2
      }
      
      if (length(y) == 0)y <- 0
      return(data.frame(x = x, y = y, estimate = estimate, l = x, spike = !is.null(spike_y)))
      
    } else if (type == "prediction"){
      
      options[["predictionN"]] <- N
      temp_predictions <- sapply(options[["priors"]],function(prior).predictBinomialLS(data, prior, options, prop), simplify = F)
      temp_predictions <- do.call(rbind.data.frame, temp_predictions)
      
      x <- sum(temp_tests[,tolower(type2)] * temp_predictions[,"mean"])
      
      if (any(all_lines$x == x))
        y <- all_lines$y[all_lines$x == x]
      else
        y <- 0
      
      return(data.frame(x = x, y = y, estimate = estimate, l = x, spike = FALSE))
    }
    
  } else if (estimate == "mode"){
    
    if (prop) d <- N else d <- 1
    
    if (length(all_spikes) > 0){
      spike_y <- max(sapply(all_spikes, function(s)s$y))
      y       <- spike_y
      x       <- unique(sapply(all_spikes, function(s)s$x[s$y == spike_y]))
      if (length(x) > 1)
        l <-  paste0("{", paste(x, collapse = ", "), "}")
      else
        l <- x
      
    } else
      spike_y <- NULL
    
    if (is.null(spike_y)){
      if (all(round(all_lines$y,10) == round(all_lines$y[1],10))){
        y <- NA
        x <- NA
        if (prop)
          l <- paste0("[", 0, ", ", 1, "]")
        else {
          if (type == "prediction")
            l <- paste0("[", 0, ", ", N, "]")
          else
            l <- paste0("[", 0, ", ", 1, "]")
        }
      } else {
        y <- all_lines$y[all_lines$y == max(all_lines$y)]
        x <- all_lines$x[all_lines$y == max(all_lines$y)]
        x[x == .0005] <- 0
        x[x == .9995] <- 1
        if (length(x) > 1)
          l <- paste0("{", paste(x, collapse = ", "), "}")          
        else
          l <- x
      }
      
    }
    
    return(data.frame(x = x, y = y, estimate = estimate, l = l, spike = !is.null(spike_y))) 
  }
}

# all settings dependent on data input
.BinomialLS_data_dependencies <- c("dataType",
                                   "nSuccesses", "nFailures",                                 # for Counts
                                   "data_sequence",    "key_success_Seq", "key_failure_Seq",  # for Sequence
                                   "selectedVariable", "key_success_Var", "key_failure_Var",  # for Variable
                                   "priors") 

