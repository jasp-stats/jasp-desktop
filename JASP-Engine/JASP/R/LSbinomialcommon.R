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
  if (options[["dataType"]] == "dataCounts")
    readyData <- TRUE
  else if (options[["dataType"]] == "dataSequence")
    readyData <- length(options[["keySuccessSeq"]]) > 0 || length(options[["keyFailureSeq"]]) > 0
  else if (options[["dataType"]] == "dataVariable")
    readyData <- length(options[["keySuccessVar"]]) > 0 || length(options[["keyFailureVar"]]) > 0
  
  # are priors ready
  readyPriors <- length(options[["priors"]]) > 0
  
  ready <- c("data" = readyData, "priors" = readyPriors)
  
  return(ready)
}
.readDataBinomialLS    <- function(dataset, options){
  
  data <- list()
  
  if (options[["dataType"]] == "dataCounts"){
    
    data$y <- NULL
    data$nSuccesses <- options[["nSuccesses"]]
    data$nFailures  <- options[["nFailures"]]
    
  } else {
    
    if ((options[["dataType"]]== "dataVariable" && options[["selectedVariable"]]  == "") |
        (options[["dataType"]]== "dataSequence" && options[["dataSequenceInput"]] == "")){
      
      data$y <- NULL
      
    } else {
      
      if (options[["dataType"]]== "dataSequence"){
        
        tempY <- .cleanSequence(options[["dataSequenceInput"]])
        
      } else if (options[["dataType"]] == "dataVariable"){
        
        # this is stupidly written #rework
        if (!is.null(dataset)){
          tempY <- dataset
        } else {
          tempY <- .readDataSetToEnd(columns = options[["selectedVariable"]])[,1]
        }
        
      }
      
      data$y <- .cleanDataBinomialLS(tempY, options)
      
    }
    
    data$nSuccesses  <- sum(data[["y"]] == 1)
    data$nFailures   <- sum(data[["y"]] == 0)
    
  } 
  
  return(data)
  
}
.cleanDataBinomialLS   <- function(x, options){
  
  # doubling the menu allows to store the keys while user switches between different input methods
  if (options[["dataType"]] == "dataSequence"){
    keySuccess <- options[["keySuccessSeq"]]
    keyFailure <- options[["keyFailureSeq"]]
  } else {
    keySuccess <- options[["keySuccessVar"]]
    keyFailure <- options[["keyFailureVar"]]
  }
  
  x <- na.omit(x)
  x <- as.character(x)
  
  # treat everything else then success as a failure if only successes are supplied
  if (length(keyFailure) == 0 && length(keySuccess) > 0){
    
    tempKs <- x %in% keySuccess
    
    x[tempKs]  <- 1
    x[!tempKs] <- 0
    
  } else if (length(keySuccess) == 0 && length(keyFailure) > 0){
    
    tempKf <- x %in% keyFailure
    
    x[!tempKf] <- 1
    x[tempKf]  <- 0
    
  } else {
    # use only variables specified in successes or failures
    
    x <- x[x %in% c(keySuccess, keyFailure)]
    
    tempKs <- x %in% keySuccess
    tempKf <- x %in% keyFailure
    
    x[tempKs] <- 1
    x[tempKf] <- 0
    
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
    summaryTable$dependOn(c("dataSummary", .dataDependenciesBinomialLS))
    
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
      distribution = gettextf("spike at %s", prior[["parPointInp"]]),
      mean         = prior[["parPoint"]],
      median       = prior[["parPoint"]],
      mode         = prior[["parPoint"]],
      lCI          = prior[["parPoint"]],
      uCI          = prior[["parPoint"]]
    )
    
    return(output)
    
  } else if (prior[["type"]] == "beta"){
    
    # in order to keep decimals as decimals ifuser fills them that way
    if (!is.na(as.numeric(prior[["parAlphaInp"]]))){
      textAlpha <- prior[["parAlpha"]] + data$nSuccesses
    } else {
      textAlpha <- MASS::fractions(prior[["parAlpha"]] + data$nSuccesses)
    }
    if (!is.na(as.numeric(prior[["parBetaInp"]]))){
      textBeta <- prior[["parBeta"]] + data$nFailures
    } else {
      textBeta <- MASS::fractions(prior[["parBeta"]] + data$nFailures)
    }
    
    output <- list(
      distribution = gettextf("beta (%s, %s)", textAlpha, textBeta),
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
  logLik   <- rep(NA, length(priors))
  
  obsProp  <- data$nSuccesses / (data$nSuccesses + data$nFailures)
  
  for(i in 1:length(priors)){
    
    tempPrior <- priors[[i]]
    prior[i]   <- tempPrior$PH
    names[i]   <- tempPrior$name
    
    if (data$nSuccesses + data$nFailures > 0){
      
      if (tempPrior[["type"]] == "spike"){
        
        logLik[i]   <- stats::dbinom(data$nSuccesses, data$nSuccesses + data$nFailures, tempPrior[["parPoint"]], log = TRUE)
        
      } else if (tempPrior[["type"]] == "beta"){
        
        logLik[i]   <- extraDistr::dbbinom(data$nSuccesses, data$nSuccesses + data$nFailures, 
                                            tempPrior[["parAlpha"]], tempPrior[["parBeta"]], log = TRUE)
        
      }
      
      
    } else {
      logLik[i] <- 0
    }
    
  }
  
  if (data$nSuccesses + data$nFailures > 0){
    
    PHLogLik   <- log(prior) + logLik
    normConst  <- log(sum(exp(PHLogLik))) 
    posterior  <- exp(PHLogLik - normConst)
    
  } else {
    
    posterior  <- prior
    
  }
  
  return(data.frame(
    prior       = prior,
    logLik      = logLik,
    posterior   = posterior,
    name        = names
  ))
  
}
.predictBinomialLS          <- function(data, prior, options, prop = FALSE){
  
  if (prop) d <- options[["predictionN"]] else d <- 1
  
  if (prior[["type"]] == "spike"){
    
    output <- list(
      distribution = gettextf("binomial (%i, %s)", options[["predictionN"]], prior[["parPointInp"]]),
      mean         = prior[["parPoint"]] * options[["predictionN"]] / d,
      median       = qbinom(.5, options[["predictionN"]], prior[["parPoint"]]) / d,
      mode         = .modeBinomialLS(options[["predictionN"]], prior[["parPoint"]], prop = prop),
      lCI          = qbinom(0.025, options[["predictionN"]], prior[["parPoint"]]) / d,
      uCI          = qbinom(0.975, options[["predictionN"]], prior[["parPoint"]]) / d,
      SD           = .computeSdBinomLS(options[["predictionN"]], prior[["parPoint"]]) / d
    )
    
    return(output)
    
  } else if (prior[["type"]] == "beta"){
    
    # in order to keep decimals as decimals ifuser fills them that way
    if (!is.na(as.numeric(prior[["parAlphaInp"]]))){
      textAlpha <- prior[["parAlpha"]] + data$nSuccesses
    } else {
      textAlpha <- MASS::fractions(prior[["parAlpha"]] + data$nSuccesses)
    }
    if (!is.na(as.numeric(prior[["parBetaInp"]]))){
      textBeta <- prior[["parBeta"]] + data$nFailures
    } else {
      textBeta <- MASS::fractions(prior[["parBeta"]] + data$nFailures)
    }
    
    output <- list(
      distribution = gettextf("beta-binomial (%i, %s, %s)", options[["predictionN"]], textAlpha, textBeta),
      mean         = (prior[["parAlpha"]] + data$nSuccesses) * options[["predictionN"]] / (prior[["parAlpha"]] + data$nSuccesses + prior[["parBeta"]] + data$nFailures) / d,
      median       = .qbetabinomLS(.5, options[["predictionN"]], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) / d,
      mode         = .modeBetaBinomLS(options[["predictionN"]], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures, prop = prop),
      lCI          = .qbetabinomLS(0.025, options[["predictionN"]], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) / d,
      uCI          = .qbetabinomLS(0.975, options[["predictionN"]], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) / d,
      SD           = ..computeSdBetaBinomLS(options[["predictionN"]], prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) / d
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
    
    HDI <- hdi.function(qbeta, coverage, shape1 = alpha, shape2 = beta)
    
  } else {
    # new density approach - instead of pdf, use scaled cdf
    # xDensity <- seq(0, 1, .00001)
    # yDensity <- dbeta(xDensity, shape1 = alpha, shape2 = beta)
    # yDensity[c(1, length(yDensity))] <- 0
    
    denBeta <- .dbetaLS(alpha, beta)
    class(denBeta) <- "density"
    
    HDI <- hdi.function(denBeta, coverage, allowSplit = T)
    HDI <- round(HDI, 5) # dealing with precission 
    HDI[HDI[,1] <= min(denBeta$x),1] <- 0
    HDI[HDI[,2] >= max(denBeta$x),2] <- 1
    
  }
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.binomialHDILS              <- function(n, theta, coverage){
  
  # this doesn't work in some cases for some reason
  # HDI <- my_hdi(qbinom, coverage, size = n, prob = theta)
  
  xDensity <- 0:n
  yDensity <- dbinom(xDensity, n, theta)
  yDensity <- round(yDensity, 10)
  denBinom <- list(
    x = xDensity,
    y = yDensity
  )
  class(denBinom) <- "density"
  HDI <- hdi.density(denBinom, coverage, allowSplit = T)
  
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
    
    xDensity <- 0:n
    yDensity <- sapply(xDensity,function(s)extraDistr::dbbinom(s, n, alpha, beta))
    yDensity <- round(yDensity, 10)
    denBeta <- list(
      x = xDensity,
      y = yDensity
    )
    class(denBeta) <- "density"
    HDI <- hdi.density(denBeta, coverage, allowSplit = T)
    
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
  # xSeq  <- seq(.001,.999,.001)
  # bfRes <- dbeta(xSeq, alpha + successses, beta + failures)/dbeta(xSeq, alpha, beta)
  
  tempPost  <- .dbetaLS(alpha + successses, beta + failures)
  tempPrior <- .dbetaLS(alpha, beta)
  
  xSeq   <- tempPost$x
  yPost  <- tempPost$y
  yPrior <- tempPrior$y
  
  bfRes  <- yPost/yPrior
  
  seqTF <- bfRes>BF
  
  support <- .aproximateSupportLS(xSeq, seqTF)
  
  support$lCI[support$lCI == min(xSeq)] <- 0
  support$uCI[support$uCI == max(xSeq)] <- 1
  
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
    tempD   <- extraDistr::dbbinom(0:N, N, alpha, beta)
    tempMed <- c(0:N)[tempD == max(tempD)]
    if (length(tempMed) > 1){
      return(paste0("{", paste(tempMed / d, collapse = ", "), "}"))
    } else {
      return(tempMed / d)
    }
  }
}
.computeSdBinomLS            <- function(N, p, prop = FALSE){
  if (prop) d <- N else d <- 1
  
  sd <- sqrt( N*p*(1-p) )
  
  return(sd / d)
}
..computeSdBetaBinomLS            <- function(N, alpha, beta, prop = FALSE){
  if (prop) d <- N else d <- 1

  sd <- sqrt( (N*alpha*beta*(N+alpha+beta)) / ( (alpha+beta)^2*(alpha+beta+1) ) )
  
  return(sd / d)
}
.marginalCentralBinomialLS  <- function(density, spikes, coverage, l.bound = 0, u.bound = 1, densityDiscrete = FALSE){
  
  if (!is.null(density)){
    if (!densityDiscrete)
      density$y <- density$y/nrow(density)
  } else
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
  
  return(cbind.data.frame(xStart = lower, xEnd = upper, g = "central", coverage = coverage))
}
.marginalHPDBinomialLS      <- function(density, spikes, coverage, l.bound = 0, u.bound = 1, densityDiscrete = FALSE){
  
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
    sumDensProb <- 1 - temp.cov
    # proportion of density needed to finish the coverage
    propDensity  <- (coverage-temp.cov)/sumDensProb
    
    # deal with flat density
    if (all(round(density$y,10) == round(density$y[1],10))){
      
      if (densityDiscrete){
        n.bars  <- u.bound-l.bound+1
        HDI2    <- c((u.bound-l.bound)/2 - propDensity*n.bars/2 + .5, (u.bound-l.bound)/2 + propDensity*n.bars/2 - .5)
        HDI2[1] <- floor(HDI2[1])
        HDI2[2] <- ceiling(HDI2[2])
      } else {
        HDI2 <- c((u.bound-l.bound)/2-(u.bound-l.bound)*propDensity/2, (u.bound-l.bound)/2+(u.bound-l.bound)*propDensity/2)
      }
      
    } else {
      
      denMarginal <- list(
        x = density$x,
        y = density$y
      )
      class(denMarginal) <- "density"
      HDI2 <- hdi.density(denMarginal, propDensity, allowSplit = T)
      
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
  
  return(cbind.data.frame(xStart = HDI[,1], xEnd = HDI[,2], g = "HPD", coverage = coverage))
}
.marginalCustomBinomialLS   <- function(density, spikes, lCI, uCI, densityDiscrete = FALSE){
  
  if (!is.null(density))
    if (!densityDiscrete)density$y <- density$y/nrow(density)    
    else
      density <- data.frame("y" = NULL, "x" = NULL)
    
    if (length(spikes) != 0){
      for(i in 1:length(spikes)){
        density <- rbind(density[density$x <= spikes[[i]]$x,], spikes[[i]], density[spikes[[i]]$x < density$x,])
      }
    }
    
    coverage <- sum(density$y[density$x >= lCI & density$x <= uCI])
    
    return(cbind.data.frame(xStart = lCI, xEnd = uCI, g = "custom", coverage = coverage, parameter = "theta"))
}
.marginalSupportBinomialLS  <- function(data, priors, postDensity, postSpikes, BF){
  
  # posterior spikes and density are already computed, we just need to get priors
  priorSpikes   <- list()
  densityI      <- 0
  priorDensity  <- NULL
  tempResults   <- .testBinomialLS(data, priors)
  for(i in 1:length(priors)){
    if (priors[[i]]$type == "spike"){
      priorSpikes <- c(
        priorSpikes, 
        list(data.frame(y = priors[[i]]$PH, x = priors[[i]]$parPoint, g = "__marginal"))
      )
    } else if (priors[[i]]$type == "beta"){
      dfLinesPP   <- .dataLinesBinomialLS(data, priors[[i]])
      dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Prior",]
      dfLinesPP$y <- exp(log(dfLinesPP$y)+log(tempResults[i, "prior"]))
      dfLinesPP$g <- priors[[i]]$name
      
      if (densityI == 0){
        priorDensity   <- dfLinesPP
      } else {
        priorDensity$y <- priorDensity$y + dfLinesPP$y
      }
      densityI <- densityI + 1
    }
  }
  
  
  # compute BFs
  bfSpikes <- list()
  if (!is.null(priorDensity)){
    bfDensity <- data.frame(
      y = exp(log(postDensity$y) - log(priorDensity$y)),
      x = postDensity$x
    )
    bfDensity$y[postDensity$y == 0] <- 0 # dealing with NaN's due to density aproximation
  } else {
    bfDensity <- data.frame(y = NULL, x = NULL)
  }
  if (length(priorSpikes) != 0){
    for(i in 1:length(priorSpikes)){
      bfSpikes[[i]] <- data.frame(
        x = postSpikes[[i]]$x,
        y = postSpikes[[i]]$y / priorSpikes[[i]]$y 
      )
    }
  }
  
  
  if (length(bfSpikes) != 0){
    for(i in 1:length(bfSpikes)){
      bfDensity <- rbind(bfDensity[bfDensity$x <= bfSpikes[[i]]$x,], bfSpikes[[i]], bfDensity[bfSpikes[[i]]$x < bfDensity$x,])
    }
  }
  
  
  support <- .aproximateSupportLS(bfDensity$x, bfDensity$y > BF)
  
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
  
  dat       <- data.frame(xStart = lCI, xEnd = uCI, g = "support", coverage = coverage, BF = BF)
  
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
  
  xSeq   <- round(seq(.001, .999, .001), 5)
  yPost  <- round((pbeta(xSeq + .001, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures) - pbeta(xSeq - .001, prior[["parAlpha"]] + data$nSuccesses, prior[["parBeta"]] + data$nFailures))*(1/(.001*2)),10)
  yPrior <- round((pbeta(xSeq + .001, prior[["parAlpha"]], prior[["parBeta"]]) - 
                      pbeta(xSeq - .001, prior[["parAlpha"]], prior[["parBeta"]]))
                   *(1/(.001*2)),10)
  
  linesGroup <- c(yPost, yPrior)
  thetaGroup <- c(xSeq, xSeq)
  nameGroup  <- c(rep("Posterior", length(xSeq)), rep("Prior", length(xSeq)))
  
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
  
  dat       <- data.frame(xStart = x[,1], xEnd = x[,2], g = "HPD", coverage = coverage)
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
  
  dat       <- data.frame(xStart = x[1], xEnd = x[2], g = "central", coverage = coverage)
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
  
  dat       <- data.frame(xStart = lCI, xEnd = uCI, g = "custom", coverage = coverage, parameter = "theta")
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
  
  dat       <- data.frame(xStart = lCI, xEnd = uCI, g = "support", coverage = coverage, BF = BF)
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
  
  xNew <- x[sort(rep(1:length(x),2))] + c(-.5, +.5)
  yNew <- y[sort(rep(1:length(x),2))]
  
  dat <- data.frame(x = xNew, y = yNew)
  return(dat)
}
.dataArrowBinomialLS        <- function(prior){
  dat       <- data.frame(x = prior[["parPoint"]], yStart = 0, yEnd = 1, g = "Prior = Posterior")
  return(dat)
}
.estimateDataPointBinomial  <- function(data, prior, N, type = c("parameter", "prediction"), estimate = c("mean", "median", "mode"), prop = FALSE){
  
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
.dataPointMarginalBinomial  <- function(data, options, allLines, allSpikes, N, type = c("parameter", "prediction"), type2 = c("Prior", "Posterior"), estimate = c("mean", "median", "mode"), prop = FALSE){
  
  if (is.null(data))
    data <- list(
      "nSuccesses" = 0,
      "nFailures"  = 0
    )
  
  if (estimate == "median"){
    if (type == "prediction" && !is.null(allLines))allLines$y <- allLines$y * length(allLines$y)
    tempDf <- .marginalCentralBinomialLS(allLines, allSpikes, coverage = 0)
    if (type == "prediction" && !is.null(allLines))allLines$y <- allLines$y / length(allLines$y)
    x <- tempDf$xStart
    if (length(allSpikes) > 0){
      spikeY <- sapply(allSpikes, function(s){
        tempY <- s$y[s$x == x]
        if (length(tempY) == 0)
          return(NA)
        else
          return(tempY)
        
      })
      if (length(na.omit(spikeY)) != 0){
        spikeY <- max(spikeY, na.rm = T)
        y       <- spikeY
      } else
        spikeY <- NULL
      
    } else
      spikeY <- NULL
    
    if (is.null(spikeY))
      y <- allLines$y[allLines$x == x]
    
    
    return(data.frame(x = x, y = y, estimate = estimate, l = x, spike = !is.null(spikeY)))
    
  } else if (estimate == "mean"){
    
    tempTests <- .testBinomialLS(data, options[["priors"]])
    
    if (type == "parameter"){
      
      tempEstimates <- sapply(options[["priors"]],function(prior).estimateBinomialLS(data, prior), simplify = F)
      tempEstimates <- do.call(rbind.data.frame, tempEstimates)
      
      x <- sum(tempTests[,tolower(type2)] * tempEstimates[,"mean"])
      
      if (length(allSpikes) > 0){
        spikeY <- sapply(allSpikes, function(s){
          tempY <- s$y[s$x == x]
          if (length(tempY) == 0)
            return(NA)
          else
            return(tempY)
          
        })
        if (length(na.omit(spikeY)) != 0){
          spikeY <- max(spikeY, na.rm = T)
          y       <- spikeY
        } else
          spikeY <- NULL
        
      } else
        spikeY <- NULL
      
      if (is.null(spikeY)){
        if (any(allLines$x == x))
          y <- allLines$y[allLines$x == x]
        else
          y <- allLines$y[which.max(allLines$x > x)]/2 + allLines$y[which.max(allLines$x > x)-1]/2
      }
      
      if (length(y) == 0)y <- 0
      return(data.frame(x = x, y = y, estimate = estimate, l = x, spike = !is.null(spikeY)))
      
    } else if (type == "prediction"){
      
      options[["predictionN"]] <- N
      tempPredictions <- sapply(options[["priors"]],function(prior).predictBinomialLS(data, prior, options, prop), simplify = F)
      tempPredictions <- do.call(rbind.data.frame, tempPredictions)
      
      x <- sum(tempTests[,tolower(type2)] * tempPredictions[,"mean"])
      
      if (any(allLines$x == x))
        y <- allLines$y[allLines$x == x]
      else
        y <- 0
      
      return(data.frame(x = x, y = y, estimate = estimate, l = x, spike = FALSE))
    }
    
  } else if (estimate == "mode"){
    
    if (prop) d <- N else d <- 1
    
    if (length(allSpikes) > 0){
      spikeY <- max(sapply(allSpikes, function(s)s[["y"]]))
      y       <- spikeY
      x       <- unique(unlist(sapply(allSpikes, function(s)s[["x"]][s[["y"]] == spikeY])))
      if (length(x) > 1)
        l <-  paste0("{", paste(x, collapse = ", "), "}")
      else
        l <- x
      
    } else
      spikeY <- NULL
    
    if (is.null(spikeY)){
      if (all(round(allLines$y,10) == round(allLines$y[1],10))){
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
        y <- allLines$y[allLines$y == max(allLines$y)]
        x <- allLines$x[allLines$y == max(allLines$y)]
        x[x == .0005] <- 0
        x[x == .9995] <- 1
        if (length(x) > 1)
          l <- paste0("{", paste(x, collapse = ", "), "}")          
        else
          l <- x
      }
      
    }
    
    return(data.frame(x = x, y = y, estimate = estimate, l = l, spike = !is.null(spikeY))) 
  }
}

# all settings dependent on data input
.dataDependenciesBinomialLS <- c("dataType",
                                   "nSuccesses", "nFailures",                                 # for Counts
                                   "dataSequenceInput",    "keySuccessSeq", "keyFailureSeq",  # for Sequence
                                   "selectedVariable", "keySuccessVar", "keyFailureVar",  # for Variable
                                   "priors") 

