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

ContingencyTablesBayesian <- function(jaspResults, dataset = NULL, options, ...) {
  dataset <- .crossTabReadData(dataset, options)
  ready <- !(length(options$rows) == 0 || length(options$columns) == 0)
  
  if(ready)
    .crossTabCheckErrors(dataset, options)
  
  # Compute the combinations of rows, columns, layers
  analyses <- .crossTabComputeAnalyses(dataset, options, ready)
  # Tables container - groups per analysis
  .crossTabContainer(jaspResults, options, analyses, ready)
  # Output tables (each calls its own results function)
  .crossTabMain(      jaspResults, dataset, options, analyses, ready)
  .contTabBasBF(      jaspResults, options, analyses, ready)
  .contTabBasLogOdds( jaspResults, options, analyses, ready)
  .contTabBasCramersV(jaspResults, options, analyses, ready)
  
  # Plots
  .contTabBasLogOddsPlot(jaspResults, options, analyses, ready)
  
  return()
}

# Output Tables
.contTabBasBF <- function(jaspResults, options, analyses, ready) {
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["contTabBasBF"]])) 
      next
    
    # Create table
    contTabBasBF <- createJaspTable(title = gettext("Bayesian Contingency Tables Tests"))
    dependList <- c("samplingModel", "hypothesis", "bayesFactorType", "priorConcentration", "setSeed", "seed")
    contTabBasBF$dependOn(dependList)
    contTabBasBF$showSpecifiedColumnsOnly <- TRUE
    contTabBasBF$position <- 2
    
    # Add columns to table
    .crossTabLayersColumns(contTabBasBF, analysis)
    contTabBasBF$addColumnInfo(name = "type[BF]",  title = "",               type = "string")
    contTabBasBF$addColumnInfo(name = "value[BF]", title = gettext("Value"), type = "number")
    contTabBasBF$addColumnInfo(name = "type[N]",   title = "",               type = "string")
    contTabBasBF$addColumnInfo(name = "value[N]",  title = gettext("Value"), type = "integer")
    
    analysisContainer[["contTabBasBF"]] <- contTabBasBF
    analysis <- as.list(analysis)
    # Compute/get groupList
    groupList <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready)
    # Compute/get bfList
    bfList <- .contTabBasComputeBfList(analysisContainer, options, groupList, ready)
    
    res <- try(.contTabBasBFRows(analysisContainer, analysis$rows, groupList, bfList, options, ready))
    .contTabBasSetErrorOrFill(res, contTabBasBF)
  }
}

.contTabBasLogOdds <- function(jaspResults, options, analyses, ready) {
  if(!options$oddsRatio)
    return()
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["contTabBasLogOdds"]])) 
      next
    
    # Create table
    contTabBasLogOdds <- createJaspTable(title = gettext("Log Odds Ratio"))
    dependList <- c("oddsRatio", "oddsRatioCredibleIntervalInterval", "hypothesis", "samplingModel", "priorConcentration", "setSeed", "seed")
    contTabBasLogOdds$dependOn(dependList)
    contTabBasLogOdds$showSpecifiedColumnsOnly <- TRUE
    contTabBasLogOdds$position <- 3
    
    ci.label <- gettextf("%.0f%% Credible Interval", 100*options$oddsRatioCredibleIntervalInterval)
    
    # Add columns to table
    .crossTabLayersColumns(contTabBasLogOdds, analysis)
    #contTabBasLogOdds$addColumnInfo(name = "type[oddsRatio]",  title = "", type = "string")
    contTabBasLogOdds$addColumnInfo(name = "value[oddsRatio]", title = gettext("Log Odds Ratio"), type = "number")
    contTabBasLogOdds$addColumnInfo(name = "low[oddsRatio]",   title = gettext("Lower"),          type = "number", overtitle = ci.label, format = "dp:3")
    contTabBasLogOdds$addColumnInfo(name = "up[oddsRatio]",    title = gettext("Upper"),          type = "number", overtitle = ci.label, format = "dp:3")
    
    analysisContainer[["contTabBasLogOdds"]] <- contTabBasLogOdds
    analysis <- as.list(analysis)
    # Compute/get groupList
    groupList <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready)
    # Compute/get bfList
    bfList <- .contTabBasComputeBfList(analysisContainer, options, groupList, ready)
    
    res <- try(.contTabBasOddsRatioRows(analysisContainer, analysis$rows, groupList, bfList, options, ready))
   
    .contTabBasSetErrorOrFill(res, contTabBasLogOdds)
  }
}

.contTabBasCramersV <- function(jaspResults, options, analyses, ready) {
  if(!options$effectSize)
    return()

  for (i in 1:nrow(analyses)){
    analysis          <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["contTabBasCramersV"]])) 
      next

    # Create table
    contTabBasCramersV <- createJaspTable(title = gettext("Cramer's V"), dependencies="effectSize")
    contTabBasCramersV$showSpecifiedColumnsOnly <- TRUE
    contTabBasCramersV$position <- 4
    
    ci <- gettextf("%.0f%% Credible Interval", 100 * options$effectSizeCredibleIntervalInterval)

    # Add columns to table
    .crossTabLayersColumns(contTabBasCramersV, analysis)
    contTabBasCramersV$addColumnInfo(name = "value[CramerV]", title = gettextf("Cramer's V"), type = "number")
    contTabBasCramersV$addColumnInfo(name = "low[CramerV]",   title = gettextf("Lower"),      type = "number", overtitle = ci, format="dp:3")
    contTabBasCramersV$addColumnInfo(name = "up[CramerV]",    title = gettextf("Upper"),      type = "number", overtitle = ci, format="dp:3")

    analysisContainer[["contTabBasCramersV"]] <- contTabBasCramersV
    analysis <- as.list(analysis)
    # Compute/get groupList
    groupList <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready)
    # Compute/get bfList
    bfList <- .contTabBasComputeBfList(analysisContainer, options, groupList, ready)
    
    res <- try(.contTabBasCramersVRows(analysisContainer, analysis$rows, groupList, bfList, options, ready))
    .contTabBasSetErrorOrFill(res, contTabBasCramersV)
  }
}

# Log Odds Plot
.contTabBasLogOddsPlot <- function(jaspResults, options, analyses, ready) {
  if(!options$plotPosteriorOddsRatio)
    return()
  oddsRatioPlotContainer <- createJaspContainer(gettext("Log Odds Ratio Plots"))
  dependList <- c("plotPosteriorOddsRatio", "hypothesis", "samplingModel",
                  "plotPosteriorOddsRatioAdditionalInfo", "priorConcentration",
                  "counts", "layers", "setSeed", "seed")
  oddsRatioPlotContainer$dependOn(dependList)
  oddsRatioPlotContainer$position <- 2
  .contTablesBayesianCitations(oddsRatioPlotContainer)
  for (i in 1:nrow(analyses)){
    if (!is.null(oddsRatioPlotContainer[[paste0("plots", i)]])) 
      next
    analysisContainer <- jaspResults[[paste0("container", i)]]
    # get groupList
    groupList <- analysisContainer[["groupList"]]$object
    
    group.matrices <- groupList$group.matrices
    groups <- groupList$groups
    for (g in 1:length(group.matrices)) {
      counts.matrix <- group.matrices[[g]]
      if (!is.null(groups))
        group <- groups[[g]]
      else
        group <- NULL
      
      if(!identical(dim(group.matrices[[1]]), as.integer(c(2,2))))
        return()
      
      group[group == ""] <- gettext("Total")
      
      if (length(group) == 0)
        oddsRatioSubContainer <- createJaspContainer()
      else if (length(group) > 0) {
        #Ok, so during the adding of gettext the following looks like it might need some attention, but im afraid to touch it cause i https://www.youtube.com/watch?v=FYhQge4Mnoo what is going on
        layer.levels <- paste(names(group),"=", group)
        layer.levels <- gsub(pattern = " = Total", layer.levels, replacement = "")
        plot.title   <- paste(layer.levels, collapse = "; ")
        oddsRatioSubContainer <- createJaspContainer(plot.title)
      }
      analysis <- analyses[i,]
      title    <- ""
      
      if(length(options$rows) > 0 || length(options$columns) > 0)
        title <- paste0(analysis$rows, " - ", analysis$columns)
      logOddsPlot <- createJaspPlot(title = title, width = 530, height = 400)
      logOddsPlot$position <- 5
      logOddsPlot$dependOn(optionContainsValue = list(rows = analysis$rows, columns = analysis$columns))
      oddsRatioSubContainer[["plot"]] <- logOddsPlot
      oddsRatioPlotContainer[[paste0("plots", i, "sub", g)]] <- oddsRatioSubContainer
      
      if(ready){
        analysisSamples.g <- analysisContainer[["logOddsSamples"]][[g]]
        # Get bfList
        bfList <- analysisContainer[["bfList"]]$object
        bf.results.g <- bfList[[g]]
        p <- try(.contTabBasLogOddsPlotFill(analysisContainer, options, analysisSamples.g,
                                            counts.matrix, bf.results.g, group))
        if(isTryError(p))
          logOddsPlot$setError(.extractErrorMessage(p))
        else
          logOddsPlot$plotObject <- p
      }
    }
  }
  jaspResults[["plots"]] <- oddsRatioPlotContainer
  return()
}

.contTabBasLogOddsPlotFill <- function(analysisContainer, options, analysisSamples.g,
                                       counts.matrix, bf.results.g, group) {
  
  if (options$samplingModel == "hypergeometric")
    stop(gettext("Odds ratio for this model not yet implemented"))
  else {
    BF10 <- bf.results.g$bf10
    if (is.na(BF10))                stop(gettext("The Bayes factor is undefined"))
    else if (is.infinite(1 / BF10)) stop(gettext("The Bayes factor is too small"))
    else if (is.infinite(BF10))     stop(gettext("The Bayes factor is infinite"))
  }

  switch(options$hypothesis,
    groupTwoGreater = {
      oneSided     <- "left"
      bfSubscripts <- c("BF['-'][0]", "BF[0]['-']")
    },
    groupOneGreater = {
      oneSided     <- "right"
      bfSubscripts <- c("BF['+'][0]", "BF[0]['+']")
    },
    groupsNotEqual = {
      oneSided     <- "notABoolean"
      bfSubscripts <- c("BF[1][0]", "BF[0][1]")
    }
  )

  result  <- try(.contTabBasOddsRatioSamples(analysisSamples.g, bf.results.g, options))
  samples <- result$log.odds.ratio.samples
  xlim    <- vector("numeric", 2)
  
  switch(oneSided,
    notABoolean = {
      xlim[1] <- quantile(samples, probs = 0.002)
      xlim[2] <- quantile(samples, probs = 0.998)
    },
    right = {
      xlim[1] <- 0
      xlim[2] <- max(2, quantile(samples, probs = 0.998))
    },
    left = {
      xlim[1] <- min(-2, quantile(samples, probs = 0.002))
      xlim[2] <- 0
    }
  )

  xticks      <- pretty(xlim)
  logOR       <- seq(min(xticks), max(xticks),length.out = 10000)
  dfLines     <- .dposteriorOR(logOR, mean(samples), sd(samples), oneSided)
  ppCri       <- c(result$lower.ci, result$upper.ci)
  CriInterval <- options$oddsRatioCredibleIntervalInterval # todo: need this implemented
  CRItxt      <- paste0(100*CriInterval, "% CI: ")
  median      <- result$median
  medianTxt   <- gettext("median Log OR = ")
  xName       <- gettext("Log Odds Ratio")

  if(options$plotPosteriorOddsRatioAdditionalInfo)
    p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLines, xName = xName, BF = BF10, bfType = "BF10",
                                           CRI = ppCri, median = median, bfSubscripts = bfSubscripts,
                                           CRItxt = CRItxt, medianTxt = medianTxt)
  else 
    p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLines, xName = xName, CRI = ppCri,  
                                           bfSubscripts = bfSubscripts, draqCRItxt = FALSE)

  p$subplots$mainGraph <- p$subplots$mainGraph+ggplot2::theme(legend.position = "none")
  return(p)
}

# Table functions 
.contTabBasSetErrorOrFill <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
  else
    for (level in 1:length(res)) 
      table$addRows(res[[level]])
}

.contTabBasComputeBfList <- function(analysisContainer, options, groupList, ready) {
  if(!is.null(analysisContainer[["bfList"]]))
    return(analysisContainer[["bfList"]]$object)
  bfList <- list()
  grp.mat <- groupList$group.matrices
  if(!ready) return()
  for(g in 1:length(grp.mat)) 
    bfList[[g]]   <- .contTabBasComputeBF(options, grp.mat[[g]], ready)
  analysisContainer[["bfList"]] <- createJaspState(bfList)
  analysisContainer[["bfList"]]$dependOn(c("samplingModel", "priorConcentration", 
                                           "hypothesis", "bayesFactorType", "setSeed", "seed"))
  return(bfList)
}

.contTabBasComputeBF <- function(options, counts.matrix, ready) {
  fixedMargin <- NULL

  switch(options$samplingModel,
    poisson = {
      bfEndLab    <- gettext("Poisson")
      sampleType  <- "poisson"
    },
    jointMultinomial = {
      bfEndLab    <- gettext("Joint multinomial")
      sampleType  <- "jointMulti"
    },
    independentMultinomialRowsFixed = {
      bfEndLab    <- gettext("Independent multinomial")
      sampleType  <- "indepMulti"
      fixedMargin <- "rows"
    },
    independentMultinomialColumnsFixed = {
      bfEndLab    <- gettext("Independent multinomial")
      sampleType  <- "indepMulti"
      fixedMargin <- "cols"
    },
    hypergeometric = {
      bfEndLab    <- gettext("Hypergeometric")
      sampleType  <- "hypergeom"
    }
  )
  
  BF <- try({
    BayesFactor::contingencyTableBF(counts.matrix, sampleType = sampleType, 
                                    priorConcentration = options$priorConcentration, 
                                    fixedMargin = fixedMargin) })

  bf1  <- exp(as.numeric(BF@bayesFactor$bf))
  lbf1 <- as.numeric(BF@bayesFactor$bf)

  rowsOrCols <- c("independentMultinomialRowsFixed", "independentMultinomialColumnsFixed")

  if(options$samplingModel != "hypergeometric"){
    .setSeedJASP(options)
    ch.result <- BayesFactor::posterior(BF, iterations = 10000)
  }


  if(options$hypothesis %in% c("groupOneGreater", "groupTwoGreater")){
    if(options$samplingModel %in% c("poisson", "jointMultinomial")){
      theta      <- as.data.frame(ch.result)
      odds.ratio <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
      logOR      <- log(odds.ratio)

      if(options$hypothesis == "groupOneGreater") prop.consistent <- 1 - mean(logOR < 0)
      else                                        prop.consistent <- mean(logOR < 0)
    
    } else if(options$samplingModel %in% rowsOrCols) {

      theta <- as.data.frame(ch.result[,7:10])

      switch(options$samplingModel,
        independentMultinomialRowsFixed = {
          if(options$hypothesis == "groupOneGreater") prop.consistent <- mean(theta[,1] > theta[,2])
          else                                        prop.consistent <- mean(theta[,1] < theta[,2])
        },
        independentMultinomialColumnsFixed = {
          if(options$hypothesis == "groupOneGreater") prop.consistent <- mean(theta[,1] > theta[,3])
          else                                        prop.consistent <- mean(theta[,1] < theta[,3])
        }
      )
    }
    if(options$samplingModel != "hypergeometric") {
      bf1  <- bf1 * prop.consistent / 0.5
      lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
    }
  }
  
  switch(options$hypothesis,
    groupOneGreater = { bfTitleUniquePart <- "\u208A" }, #On my system these code points seem to be rendered as B+0, B0+ etc. Probably wrong then?
    groupTwoGreater = { bfTitleUniquePart <- "\u208B" },
    groupsNotEqual  = { bfTitleUniquePart <- "\u2081" }) # || options$samplingModel == "hypergeometric" used to be added here, but because it was an if - else if - else if covering all possibilities this was quite pointless. Maybe there is a bug here as well. I don't know about that.

  #I am not adding the following bfTitle's to the translation strings because there is not a lot to translate and it would be neigh incomprehensible if all those unicode chars were %s...
  switch(options$bayesFactorType,
    BF10 = { 
      bfTitle <- paste0("BF", bfTitleUniquePart, "\u2080")
      bfVal   <- bf1
    },
    BF01 = {
      bfTitle <- paste0("BF\u2080", bfTitleUniquePart)
      bfVal   <- 1/bf1
    },
    LogBF10 = {
      bfTitle <- paste0("Log\u2009(\u2009BF", bfTitleUniquePart, "\u2080\u2009)")
      bfVal   <- lbf1
    }
  )

  bfList <- list("bfTitle"      = bfTitle,
                 "bfEndLab"     = bfEndLab,
                 "bfVal"        = bfVal,
                 "bf10"         = bf1)

  if(options$samplingModel != "hypergeometric")
    bfList$post.samples <- ch.result

  return(bfList)
}

.contTabBasBFFootnote <- function(options, counts.matrix, analysisContainer, ready) {
  if(!ready)
    return()

  if (!identical(dim(counts.matrix),as.integer(c(2,2)))){
    if(options$samplingModel == "hypergeometric")
      message <- gettext("Hypergeometric contingency tables test restricted to 2 x 2 tables")
    else
      message <- gettext("Proportion test restricted to 2 x 2 tables")

  } else if(options$samplingModel == "hypergeometric") {
    return()

  } else {

    samplingModels <- c("poisson", "jointMultinomial", "independentMultinomialRowsFixed")
    if(options$samplingModel %in% samplingModels){
      gp1 <- rownames(counts.matrix)[1]
      gp2 <- rownames(counts.matrix)[2]
    } else if(options$samplingModel == "independentMultinomialColumnsFixed") {
      gp1 <- colnames(counts.matrix)[1]
      gp2 <- colnames(counts.matrix)[2]
    }

    switch(options$hypothesis,
      groupOneGreater = { equality <- gettext("is greater than") },
      groupTwoGreater = { equality <- gettext("is less than")    },
      groupsNotEqual  = { equality <- gettext("is not equal to") },
    )

    message <- gettextf("For all tests, the alternative hypothesis specifies that group <em>%1$s</em> %2$s <em>%3$s</em>.", gp1, equality, gp2)
  }

  analysisContainer[["contTabBasBF"]]$addFootnote(message)
}

.contTablesBayesianCitations <- function(object) {
  citationList <- list(
    "Morey, R. D., & Rouder, J. N. (2015). BayesFactor 
    (Version 0.9.11-3)[Computer software].",
    "Jamil, T., Ly, A., Morey, R. D., Love, J., 
    Marsman, M., & Wagenmakers, E.-J. (2017). 
    Default Gunel and Dickey Bayes factors for contingency tables. 
    Behavior Research Methods, 49, 638-652.",
    "Gunel, E., & Dickey, J. (1974). 
    Bayes factors for independence in contingency tables. 
    Biometrika, 61, 545-557.")

  for(citation in citationList)
    object$addCitation(citation)
}

.contTabBasOddsRatioSamples <- function(analysisSamples.g, bf.results.g, options) {
  if(!is.null(analysisSamples.g))
    return(analysisSamples.g$object)
  BF        <- bf.results.g$bfVal
  ch.result <- bf.results.g$post.samples

  if (options$samplingModel == "poisson") {
    lambda             <- as.data.frame(ch.result)
    odds.ratio.samples <- (lambda[,1]*lambda[,4])/(lambda[,2]*lambda[,3])

  } else if (options$samplingModel == "jointMultinomial") {
    theta              <- as.data.frame(ch.result)
    odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])

  } else if (options$samplingModel %in% c("independentMultinomialRowsFixed", "independentMultinomialColumnsFixed")) {
    theta      <- as.data.frame(ch.result[,7:10])
    odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])

  } else
    stop(gettext("Invalid sampling model selected"))

  samples   <- log(odds.ratio.samples)
  sig       <- options$oddsRatioCredibleIntervalInterval
  alpha     <- (1 - sig) / 2
  quantiles <- .crossTabCIPlusMedian(credibleIntervalInterval = sig,
                                           mean = mean(samples), sd = sd(samples),
                                           hypothesis = options$hypothesis)
  median <- quantiles$ci.median
  lower  <- quantiles$ci.lower
  upper  <- quantiles$ci.upper
  #median <- stats::median(samples)
  #lower  <- unname(stats::quantile(samples, p = alpha))
  #upper  <- unname(stats::quantile(samples, p = (1-alpha)))
  samplesList <- list(log.odds.ratio.samples = samples, BF = BF, 
                      median = median, lower.ci = lower, upper.ci = upper)
  logOddsSamples    <- createJaspState(samplesList)
  analysisSamples.g <- logOddsSamples
  return(samplesList)
}

# Table Results 
.contTabBasBFRows <- function(analysisContainer, var.name, groupList, bf.results, options, ready) {
  bf.rows <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups

  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    
    if (!is.null(groups)) group <- groups[[g]]
    else                  group <- NULL

    row <- list()
    row[["type[BF]"]] <- paste(bf.results[[g]]$bfTitle, bf.results[[g]]$bfEndLab)
    row[["type[N]"]]  <- "N"
    
    if (ready) {
      row[["value[BF]"]] <- bf.results[[g]]$bfVal
      row[["value[N]"]]  <- sum(counts.matrix)
    } else 
      row[["value[BF]"]] <- row[["value[N]"]]  <- "."
    
    bf.rows[[length(bf.rows) + 1]] <- .crossTabLayerNames(row, group)
  }

  .contTabBasBFFootnote(options, counts.matrix, analysisContainer, ready) 
  return(bf.rows)
}

.contTabBasOddsRatioRows <- function(analysisContainer, var.name, groupList, bf.results, options, ready) {
  odds.ratio.rows <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups

  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    
    if (!is.null(groups)) group <- groups[[g]]
    else                  group <- NULL
    
    row                       <- .crossTabLayerNames(list(), group)
    row[["type[oddsRatio]"]]  <- gettext("Odds ratio")
    result                    <- list(median = ".", lower  = ".", upper  = ".")

    if (!identical(dim(counts.matrix), as.integer(c(2,2)))) {
      message <- gettext("Odds ratio restricted to 2 x 2 tables")
      analysisContainer[["contTabBasLogOdds"]]$addFootnote(message)

    } else if ( options$samplingModel == "hypergeometric") {
      row[["value[oddsRatio]"]] <- NaN
      row[["low[oddsRatio]"]]   <- row[["up[oddsRatio]"]] <- "" #Really? What order is this evaluated in?

      message <- gettext("Odd ratio for this model not yet implemented")
      analysisContainer[["contTabBasLogOdds"]]$addFootnote(message)

    } else if(ready){
      bf.results.g      <- bf.results[[g]]
      analysisSamples.g <- analysisContainer[["logOddsSamples"]][[g]]
      result            <- try(.contTabBasOddsRatioSamples(analysisSamples.g, bf.results.g, options))
    }
    row[["value[oddsRatio]"]] <- result$median
    row[["low[oddsRatio]"]]   <- result$lower
    row[["up[oddsRatio]"]]    <- result$upper
    
    odds.ratio.rows[[length(odds.ratio.rows) + 1]] <- row
  }
  return(odds.ratio.rows)
}

.contTabBasCramersVRows <- function(analysisContainer, var.name, groupList, bf.results, options, ready) {
  cv.rows <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups

  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
   
    if (!is.null(groups)) group <- groups[[g]]
    else                  group <- NULL
    
    row                         <- .crossTabLayerNames(list(), group)
    row[["type[CramerV]"]]      <- gettext("Cramer's V")

    if ( options$samplingModel == "hypergeometric") {
      row[["value[CramerV]"]] <- NaN
      row[["low[CramerV]"]]   <- row[["up[CramerV]"]] <- ""
      
      message <- gettext("Cramer's V for this model not yet implemented")
      analysisContainer[["contTabCramersV"]]$addFootnote(message)

    } else if(ready){
      result <- try({
        BF <- bf.results[[g]]$bfVal
        ch.result <- bf.results[[g]]$post.samples
        d  <- dim(counts.matrix)
        I  <- d[1]
        J  <- d[2]
        k  <- min(I,J)
        N  <- sum(counts.matrix)
        yr <- rowSums(counts.matrix)
        yc <- colSums(counts.matrix)
        
        if (options$samplingModel == "poisson") {

          lambda    <- as.data.frame(ch.result)
          theta0    <- apply(lambda, 1, function(x) matrix(x,I))
          sumlambda <- apply(lambda, 1, sum)
          chi2 <- apply(theta0, 2, function(data) chisq.test(matrix(data,I))$statistic)
          Phi.Poisson <- sqrt(chi2/(sumlambda*(k-1)))
          Cramer    <- Phi.Poisson

        } else if (options$samplingModel == "jointMultinomial") {

          theta  <- as.data.frame(ch.result)
          theta0 <- apply(theta, 1, function(x) matrix(x,I))
          chi2 <- apply(theta0 * N, 2, function(data) chisq.test(matrix(data,I))$statistic)
          Phi.jointMulti <- sqrt(chi2/(N*(k-1)))
          Cramer <- Phi.jointMulti

        } else if (options$samplingModel %in% c("independentMultinomialRowsFixed", "independentMultinomialColumnsFixed")) {

          index <- grep(pattern = "omega", x = colnames(ch.result))
          theta <- as.data.frame(ch.result[,index])

          if(options$samplingModel == "independentMultinomialRowsFixed") {
            theta0 <- yr * apply(theta, 1, function(x) matrix(x,I))
            chi2   <- apply(theta0, 2, function(data) chisq.test(matrix(data,I))$statistic)

          } else if(options$samplingModel == "independentMultinomialColumnsFixed") {
            theta0 <- yc * apply(theta, 1, function(x) matrix(x,J, byrow=TRUE))
            chi2   <- apply(theta0, 2, function(data) chisq.test(matrix(data,J))$statistic)
          }

          Phi.indepMulti <- sqrt(chi2/(N*(k-1)))
          Cramer         <- Phi.indepMulti

        } else 
          stop(("Invalid sampling model selected"))
        
        CramersV.samples <- Cramer
        CramersV.median  <- stats::median(CramersV.samples)
        sig   <- options$effectSizeCredibleIntervalInterval
        alpha <- (1 - sig) / 2
        lower <- unname(stats::quantile(Cramer, p = alpha))
        upper <- unname(stats::quantile(Cramer, p = (1-alpha)))
        
        list(CramersV.samples = CramersV.samples, 
             BF = BF, CVmedian = CramersV.median, 
             CV.lower.ci = lower, CV.upper.ci = upper)
      })
    } else {
      result <- list("CVmedian"    = ".",
                     "CV.lower.ci" = ".",
                     "CV.upper.ci" = ".")
    }
    row[["value[CramerV]"]] <- result$CVmedian
    row[["low[CramerV]"]]   <- result$CV.lower.ci
    row[["up[CramerV]"]]    <- result$CV.upper.ci
    cv.rows[[length(cv.rows) + 1]] <- row
  }

  return(cv.rows)
}

.dposteriorOR <- function(logOR, mean, sd, oneSided){
  size            <- length(logOR)
  linesPosterior  <- numeric(size) # initializes everything to 0

  switch(oneSided,
    notABoolean = { 
      linesPosterior <- dnorm(logOR, mean, sd) 
    },
    right = {
      idx                 <- logOR >= 0 # the nonzero components
      linesPosterior[idx] <- dnorm(logOR, mean, sd) / pnorm(0, mean, sd, lower.tail = FALSE)
      linesPosterior[1]   <- 0 # gives the sharp truncation line 
    },
    left = {
      idx                  <- logOR <= 0 # the nonzero components
      linesPosterior[idx]  <- dnorm(logOR, mean, sd) / pnorm(0, mean, sd, lower.tail = TRUE)
      linesPosterior[size] <- 0 # gives the sharp truncation line
    }
  )

  # The following data.frame is for the argument dfLines at JASPgraphs::PlotPriorAndPosterior( and I guess it should be gettext'ed
  dat        <- data.frame(x = logOR, y = linesPosterior, g = rep(gettext("Posterior"), size))
  return(dat)
}

#CRI and Median 
.crossTabCIPlusMedian <- function(credibleIntervalInterval = .95, mean, sd, hypothesis = "groupsNotEqual") {
  
  lower <- (1 - credibleIntervalInterval) / 2
  upper <- 1 - lower
  
  switch(hypothesis,
    groupsNotEqual={
      quantiles <- qnorm(c(lower, .5, upper), mean, sd)
    },
    groupOneGreater = {
      rightArea <- pnorm(0, mean, sd, lower.tail = FALSE)
      leftArea  <- 1 - rightArea
      quantiles <- qnorm(leftArea + rightArea * c(lower, .5, upper), mean, sd)
    },
    groupTwoGreater = {
      leftArea  <- pnorm(0, mean, sd)
      quantiles <- qnorm(leftArea * c(lower, .5, upper), mean, sd)
    }
  )

  return(list(ci.lower = quantiles[1], ci.median = quantiles[2], ci.upper = quantiles[3]))
}
