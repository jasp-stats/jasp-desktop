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
    contTabBasBF <- createJaspTable(title = "Bayesian Contingency Tables Tests")
    dependList <- c("samplingModel", "hypothesis", "bayesFactorType", "priorConcentration")
    contTabBasBF$dependOn(dependList)
    contTabBasBF$showSpecifiedColumnsOnly <- TRUE
    contTabBasBF$position <- 2
    
    # Add columns to table
    .crossTabLayersColumns(contTabBasBF, analysis)
    contTabBasBF$addColumnInfo(name = "type[BF]",  title = "",      type = "string")
    contTabBasBF$addColumnInfo(name = "value[BF]", title = "Value", type = "number")
    contTabBasBF$addColumnInfo(name = "type[N]",   title = "",      type = "string")
    contTabBasBF$addColumnInfo(name = "value[N]",  title = "Value", type = "integer")
    
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
    contTabBasLogOdds <- createJaspTable(title = "Log Odds Ratio")
    dependList <- c("oddsRatio", "oddsRatioCredibleIntervalInterval", 
                    "hypothesis", "samplingModel", "priorConcentration")
    contTabBasLogOdds$dependOn(dependList)
    contTabBasLogOdds$showSpecifiedColumnsOnly <- TRUE
    contTabBasLogOdds$position <- 3
    
    ci.label <- paste(100*options$oddsRatioCredibleIntervalInterval, 
                      "% Credible Interval", sep = "")
    
    # Add columns to table
    .crossTabLayersColumns(contTabBasLogOdds, analysis)
    #contTabBasLogOdds$addColumnInfo(name = "type[oddsRatio]",  title = "", type = "string")
    contTabBasLogOdds$addColumnInfo(name = "value[oddsRatio]", title = "Log Odds Ratio", 
                               type = "number")
    contTabBasLogOdds$addColumnInfo(name = "low[oddsRatio]",   title = "Lower", 
                               overtitle = ci.label, type = "number", format = "dp:3")
    contTabBasLogOdds$addColumnInfo(name = "up[oddsRatio]",    title = "Upper", 
                               overtitle = ci.label, type = "number", format = "dp:3")
    
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
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[paste0("container", i)]]
    if (!is.null(analysisContainer[["contTabBasCramersV"]])) 
      next
    
    # Create table
    contTabBasCramersV <- createJaspTable(title = "Cramer's V")
    dependList <- c("effectSize")
    contTabBasCramersV$dependOn(dependList)
    contTabBasCramersV$showSpecifiedColumnsOnly <- TRUE
    contTabBasCramersV$position <- 4
    
    ci <- paste(100 * options$effectSizeCredibleIntervalInterval, "% Credible Interval", sep="")
    
    # Add columns to table
    .crossTabLayersColumns(contTabBasCramersV, analysis)
    contTabBasCramersV$addColumnInfo(name = "value[CramerV]", title = "Cramer's V", 
                                     type = "number")
    contTabBasCramersV$addColumnInfo(name = "low[CramerV]", title = "Lower", 
                                     overtitle = ci, type = "number", format="dp:3")
    contTabBasCramersV$addColumnInfo(name = "up[CramerV]",  title = "Upper", 
                                     overtitle = ci, type = "number", format="dp:3")
    
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
  oddsRatioPlotContainer <- createJaspContainer("Log Odds Ratio Plots")
  dependList <- c("plotPosteriorOddsRatio", "hypothesis", "samplingModel",
                  "plotPosteriorOddsRatioAdditionalInfo", "priorConcentration",
                  "counts", "layers")
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
      
      group[group == ""] <- "Total"
      
      if (length(group) == 0)
        oddsRatioSubContainer <- createJaspContainer()
      else if (length(group) > 0) {
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
    stop("Odds ratio for this model not yet implemented")
  else {
    BF10 <- bf.results.g$bf10
    if (is.na(BF10)) 
      stop("The Bayes factor is undefined")
    else if (is.infinite(1 / BF10)) 
      stop("The Bayes factor is too small")
    else if (is.infinite(BF10)) 
      stop("The Bayes factor is infinite")
  }
  
  if (options$hypothesis == "groupTwoGreater") {
    oneSided     <- "left"
    bfSubscripts <- "BF[1][0]"
  } else if (options$hypothesis == "groupOneGreater") {
    oneSided     <- "right"
    bfSubscripts <- "BF['+'][0]"
  } else {
    oneSided     <- FALSE
    bfSubscripts <- "BF['-'][0]"
  }
  result <- try(.contTabBasOddsRatioSamples(analysisSamples.g, bf.results.g, options))
  samples <- result$log.odds.ratio.samples
  
  xlim <- vector("numeric", 2)
  
  if (oneSided == FALSE) {
    xlim[1] <- quantile(samples, probs = 0.002)
    xlim[2] <- quantile(samples, probs = 0.998)
  } else if (oneSided == "right") {
    xlim[1] <- 0
    xlim[2] <- max(2, quantile(samples, probs = 0.998))
  } else if (oneSided == "left") {
    xlim[1] <- min(-2, quantile(samples, probs = 0.002))
    xlim[2] <- 0
  }    
  xticks  <- pretty(xlim)
  logOR   <- seq(min(xticks), max(xticks),length.out = 10000)
  dfLines <- .dposteriorOR(logOR, mean(samples), sd(samples), oneSided)
  ppCri   <- c(result$lower.ci, result$upper.ci)
  CriInterval <- options$oddsRatioCredibleIntervalInterval # todo: need this implemented
  CRItxt  <- paste0(100*CriInterval, "% CI: ")
  median  <- result$median
  medianTxt <- "median Log OR = "
  xName     <- "Log Odds Ratio"
  if(options$plotPosteriorOddsRatioAdditionalInfo)
    p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLines, xName = xName, BF = 1/BF10,
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
                                           "hypothesis", "bayesFactorType"))
  return(bfList)
}

.contTabBasComputeBF <- function(options, counts.matrix, ready) {
  fixedMargin <- NULL
  if (options$samplingModel == "poisson") {
    bfEndLab    <- "Poisson"
    sampleType  <- "poisson"
  } else if (options$samplingModel == "jointMultinomial"){
    bfEndLab    <- "joint multinomial"
    sampleType  <- "jointMulti"
  } else if (options$samplingModel =="independentMultinomialRowsFixed"){
    bfEndLab    <- "independent multinomial"
    sampleType  <- "indepMulti"
    fixedMargin <- "rows"
  } else if (options$samplingModel =="independentMultinomialColumnsFixed"){
    bfEndLab    <- "independent multinomial"
    sampleType  <- "indepMulti"
    fixedMargin <- "cols"
  } else if (options$samplingModel=="hypergeometric"){
    bfEndLab    <- "hypergeometric"
    sampleType  <- "hypergeom"
  } else
    stop("Invalid sampling model selected")
  BF <- try({
    BayesFactor::contingencyTableBF(counts.matrix, sampleType = sampleType, 
                                    priorConcentration = options$priorConcentration, 
                                    fixedMargin = fixedMargin) })
  bf1  <- exp(as.numeric(BF@bayesFactor$bf))
  lbf1 <- as.numeric(BF@bayesFactor$bf)
  rowsOrCols <- c("independentMultinomialRowsFixed", "independentMultinomialColumnsFixed")
  if(options$samplingModel != "hypergeometric")
    ch.result <- BayesFactor::posterior(BF, iterations = 10000)
  if(options$hypothesis %in% c("groupOneGreater", "groupTwoGreater")){
    if(options$samplingModel %in% c("poisson", "jointMultinomial")){
      theta <- as.data.frame(ch.result)
      odds.ratio <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
      logOR <- log(odds.ratio)
      if(options$hypothesis == "groupOneGreater")
        prop.consistent <- 1 - mean(logOR < 0)
      else 
        prop.consistent <- mean(logOR < 0)
    } else if(options$samplingModel %in% rowsOrCols) {
      theta <- as.data.frame(ch.result[,7:10])
      if(options$samplingModel == "independentMultinomialRowsFixed"){
        if(options$hypothesis == "groupOneGreater")
          prop.consistent <- mean(theta[,1] > theta[,2])
        else 
          prop.consistent <- mean(theta[,1] < theta[,2])
      } else if(options$samplingModel == "independentMultinomialColumnsFixed") {
        if(options$hypothesis == "groupOneGreater")
          prop.consistent <- mean(theta[,1] > theta[,3])
        else 
          prop.consistent <- mean(theta[,1] < theta[,3])
      }
    }
    if(options$samplingModel != "hypergeometric") {
      bf1  <- bf1 * prop.consistent / 0.5
      lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
    }
  }
  
  if(options$hypothesis == "groupOneGreater") {
    if (options$bayesFactorType == "BF10")
      bfTitle <- "BF\u208A\u2080"
    else if (options$bayesFactorType == "BF01")
      bfTitle <- "BF\u2080\u208A"
    else if (options$bayesFactorType == "LogBF10") 
      bfTitle <- "Log\u2009(\u2009BF\u208A\u2080\u2009)"
  } else if(options$hypothesis == "groupTwoGreater"){
    if (options$bayesFactorType == "BF10")
      bfTitle <- "BF\u208B\u2080"
    else if (options$bayesFactorType == "BF01")
      bfTitle <- "BF\u2080\u208B"
    else if (options$bayesFactorType == "LogBF10")
      bfTitle <-	"Log\u2009(\u2009BF\u208B\u2080\u2009)"
  } else if(options$hypothesis == "groupsNotEqual" || 
            options$samplingModel == "hypergeometric") {
    if (options$bayesFactorType == "BF10")
      bfTitle <- "BF\u2081\u2080"
    else if (options$bayesFactorType == "BF01") 
      bfTitle <- "BF\u2080\u2081"
    else if (options$bayesFactorType == "LogBF10") 
      bfTitle <-	"Log\u2009(\u2009BF\u2081\u2080\u2009)"
  }
  
  if (options$bayesFactorType == "BF10")
    bfVal <- bf1
  else if (options$bayesFactorType == "BF01")
    bfVal <- 1/bf1
  else if (options$bayesFactorType == "LogBF10")
    bfVal <- lbf1
  
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
      message <- "Hypergeometric contingency tables test restricted to 2 x 2 tables"
    else
      message <- "Proportion test restricted to 2 x 2 tables"
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
    msgStart <- "For all tests, the alternative hypothesis specifies that group <em>"
    if (options$hypothesis == "groupOneGreater")
      message <- paste0(msgStart, gp1, "</em> is greater than group <em>", gp2, ".</em>")
    else if (options$hypothesis == "groupTwoGreater")
      message <- paste0(msgStart, gp1, "</em> is less than group <em>", gp2, ".</em>")
    else #groupsNotEqual
      message <- paste0(msgStart, gp1, "</em> is not equal to group <em>", gp2, ".</em>")
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
    lambda     <- as.data.frame(ch.result)
    odds.ratio.samples <- (lambda[,1]*lambda[,4])/(lambda[,2]*lambda[,3])
  } else if (options$samplingModel == "jointMultinomial") {
    theta      <- as.data.frame(ch.result)
    odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
  } else if (options$samplingModel %in% c("independentMultinomialRowsFixed", 
                                          "independentMultinomialColumnsFixed")) {
    theta      <- as.data.frame(ch.result[,7:10])
    odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
  } else
    stop("Invalid sampling model selected")
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
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
    row <- list()
    row[["type[BF]"]] <- paste(bf.results[[g]]$bfTitle, bf.results[[g]]$bfEndLab)
    row[["type[N]"]]  <- "N"
    if (ready) {
      row[["value[BF]"]] <- bf.results[[g]]$bfVal
      row[["value[N]"]]  <- sum(counts.matrix)
    } else 
      row[["value[BF]"]] <- row[["value[N]"]]  <- "."
    row <- .crossTabLayerNames(row, group)
    bf.rows[[length(bf.rows) + 1]] <- row
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
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    row <- .crossTabLayerNames(row, group)
    
    row[["type[oddsRatio]"]] <- "Odds ratio"
    result <- list("median" = ".", "lower"  = ".", "upper"  = ".")
    if (!identical(dim(counts.matrix), as.integer(c(2,2)))) {
      message <- "Odds ratio restricted to 2 x 2 tables"
      analysisContainer[["contTabBasLogOdds"]]$addFootnote(message)
    } else if ( options$samplingModel == "hypergeometric") {
      row[["value[oddsRatio]"]] <- NaN
      row[["low[oddsRatio]"]] <- row[["up[oddsRatio]"]] <- ""
      message <- "Odd ratio for this model not yet implemented"
      analysisContainer[["contTabBasLogOdds"]]$addFootnote(message)
    } else if(ready){
      bf.results.g <- bf.results[[g]]
      analysisSamples.g <- analysisContainer[["logOddsSamples"]][[g]]
      result <- try(.contTabBasOddsRatioSamples(analysisSamples.g, 
                                                bf.results.g, options))
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
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    row <- .crossTabLayerNames(row, group)
    
    row[["type[CramerV]"]] <- "Cramer's V"
    if ( options$samplingModel == "hypergeometric") {
      row[["value[CramerV]"]] <- NaN
      row[["low[CramerV]"]] <- row[["up[CramerV]"]] <- ""
      
      message <- "Cramer's V for this model not yet implemented"
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
        } else if (options$samplingModel %in% c("independentMultinomialRowsFixed", 
                                                "independentMultinomialColumnsFixed")) {
          index <- grep(pattern = "omega", x = colnames(ch.result))
          theta <- as.data.frame(ch.result[,index])
          if(options$samplingModel == "independentMultinomialRowsFixed") {
            theta0 <- yr * apply(theta, 1, function(x) matrix(x,I))
            chi2 <- apply(theta0, 2, function(data) chisq.test(matrix(data,I))$statistic)
          } else if(options$samplingModel == "independentMultinomialColumnsFixed") {
            theta0 <- yc * apply(theta, 1, function(x) matrix(x,J, byrow=TRUE))
            chi2 <- apply(theta0, 2, function(data) chisq.test(matrix(data,J))$statistic)
          }
          Phi.indepMulti <- sqrt(chi2/(N*(k-1)))
          Cramer <- Phi.indepMulti
        } else 
          stop("Invalid sampling model selected")
        
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
  size <- length(logOR)
  if(oneSided == FALSE)
    linesPosterior <- dnorm(logOR, mean, sd)
  else {
    linesPosterior <- numeric(size) # initializes everything to 0
    if (oneSided == "right") {
      idx <- logOR >= 0 # the nonzero components
      linesPosterior[idx] <- dnorm(logOR, mean, sd) / pnorm(0, mean, sd, lower.tail = FALSE)
      linesPosterior[1]   <- 0 # gives the sharp truncation line 
    } else if (oneSided == "left") {
      idx <- logOR <= 0 # the nonzero components
      linesPosterior[idx]  <- dnorm(logOR, mean, sd) / pnorm(0, mean, sd, lower.tail = TRUE)
      linesPosterior[size] <- 0 # gives the sharp truncation line
    }
  }
  logORGroup <- logOR
  nameGroup  <- rep("Posterior", size)
  dat        <- data.frame(x = logORGroup, y = linesPosterior, g = nameGroup)
  return(dat)
}

#CRI and Median 
.crossTabCIPlusMedian <- function(credibleIntervalInterval = .95, mean, sd, 
                                        hypothesis = "groupsNotEqual") {
  
  lower <- (1 - credibleIntervalInterval) / 2
  upper <- 1 - lower
  
  if(hypothesis == "groupsNotEqual")
    quantiles <- qnorm(c(lower, .5, upper), mean, sd)
  else if (hypothesis == "groupOneGreater") {
    rightArea <- pnorm(0, mean, sd, lower.tail = FALSE)
    leftArea  <- 1 - rightArea
    quantiles <- qnorm(leftArea + rightArea * c(lower, .5, upper), mean, sd)
  } else if (hypothesis == "groupTwoGreater") {
    leftArea  <- pnorm(0, mean, sd)
    quantiles <- qnorm(leftArea * c(lower, .5, upper), mean, sd)
  }
  return(list(ci.lower = quantiles[1], ci.median = quantiles[2], ci.upper = quantiles[3]))
}
