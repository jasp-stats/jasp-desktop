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

ContingencyTablesBayesian <- function(jaspResults, dataset, options, ...) {
  
  # Read dataset
  dataset <- .contTabBasReadData(dataset, options)
  
  ready <- length(options$rows > 0) && length(options$columns > 0)
  
  # Error checking
  .contTabBasCheckErrors(dataset, options)
  
  # Compute the combinations of rows, columns, layers
  analyses <- .contTabBasComputeAnalyses(dataset, options, ready)
  
  # Tables container
  .contTabBasContainer(jaspResults, options, analyses, ready)
  
  # Output tables (each calls its own results function)
  .contTabBasMain(       jaspResults, dataset, options, analyses, ready)
  .contTabBasBF(         jaspResults, options, analyses, ready)
  .contTabBasLogOdds(    jaspResults, options, analyses, ready)
  .contTabBasCramersV(   jaspResults, options, analyses, ready)
  
  # Plots
  .contTabBasLogOddsPlot(jaspResults, options, analyses, ready)
  
  return()
}

# Preprocessing functions
.contTabBasReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else {
    layer.variables <- c()
    for (layer in options$layers)
      layer.variables <- c(layer.variables, unlist(layer$variables))
    
    counts.var <- options$counts
    if (counts.var == "")
      counts.var <- NULL
    
    factors <- c(unlist(options$rows), unlist(options$columns), layer.variables)
    
    return(.readDataSetToEnd(columns.as.factor  = factors, 
                             columns.as.numeric = counts.var))
  }
}

.contTabBasCheckErrors <- function(dataset, options) {
  .hasErrors(dataset, 
             type = c('negativeValues', 'infinity'), 
             all.target = c(options$counts), 
             exitAnalysisIfErrors = TRUE)
}

# Combinations of rows, columns, layers
.contTabBasComputeAnalyses <- function(dataset, options, ready) {
  rows    <- as.vector(options$rows,    "character")
  columns <- as.vector(options$columns, "character")
  
  if (length(rows) == 0)
    rows <- ""
  if (length(columns) == 0)
    columns <- ""
  
  analyses <- list()
  analyses <- data.frame("columns" = columns, stringsAsFactors = FALSE)
  analyses <- cbind(analyses, "rows" = rep(rows, each = dim(analyses)[1]), 
                    stringsAsFactors = FALSE)
  
  for (layer in options$layers) {
    layer.vars <- as.vector(layer$variables, "character")
    analyses <- cbind(analyses, rep(layer.vars, each = dim(analyses)[1]), 
                      stringsAsFactors = FALSE)
    names(analyses)[dim(analyses)[2]] <- layer$name
  }
  
  analyses <- .dataFrameToRowList(analyses)

  return(analyses)
}

# Container
.contTabBasContainer <- function(jaspResults, options, analyses, ready) {
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    if (is.null(jaspResults[[paste0("tables", i)]])) {
      container <- createJaspContainer()
      container$dependOn(options              = c("layers", "counts"),
                         optionContainsValue  = list(rows     = analysis$rows, 
                                                     columns  = analysis$columns))
      jaspResults[[paste0("tables", i)]] <- container
    }
  }
}

# Output Tables
.contTabBasMain <- function(jaspResults, dataset, options, analyses, ready) {
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
    if (!is.null(analysisContainer[["contTabBasMain"]])) 
      next
    # Create table
    contTabBasMain <- createJaspTable(title = "Bayesian Contingency Tables")
    dependList <- c("countsExpected", "percentagesRow", "percentagesColumn", 
                    "percentagesTotal", "rowOrder", "columnOrder")
    contTabBasMain$dependOn(dependList)
    .contTablesBayesianCitations(contTabBasMain)
    contTabBasMain$showSpecifiedColumnsOnly <- TRUE
    contTabBasMain$position <- 1
    .contTabBasLayersColumns(contTabBasMain, analysis)
    if(analysis$rows == "")
      contTabBasMain$addColumnInfo(name = analysis$rows, title = " ", type = "string", combine = TRUE)
    else 
      contTabBasMain$addColumnInfo(name = analysis$rows, type = "string", combine = TRUE)
    counts.fp <- .contTabBasCountsFloatingPoint(dataset, options)
    .contTabBasMainOvertitle(dataset, options, contTabBasMain, analysis, counts.fp)
    if (counts.fp)
      contTabBasMain$addColumnInfo(name = "total[counts]",   title = "Total", 
                                   type = "number", format = "sf:4;dp:2")
    else
      contTabBasMain$addColumnInfo(name = "total[counts]", title = "Total", type = "integer")
    
    analysisContainer[["contTabBasMain"]] <- contTabBasMain
    # Compute Group List
    .contTabBasComputeGroups(dataset, options, analysisContainer, analysis, ready)
    # Get Group List
    groupList <- analysisContainer[["groupList"]]$object
    res <- try(.contTabBasCountsRows(analysis$rows, groupList$group.matrices, 
                                     groupList$groups, analysisContainer, options, ready))
    
    .contTabBasSetErrorOrFill(res, contTabBasMain)
  }
}

.contTabBasBF <- function(jaspResults, options, analyses, ready) {
  
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
    if (!is.null(analysisContainer[["contTabBasBF"]])) 
      next
    
    # Create table
    contTabBasBF <- createJaspTable(title = "Bayesian Contingency Tables Tests")
    dependList <- c("samplingModel", "hypothesis", "BayesFactorType", "priorConcentration")
    contTabBasBF$dependOn(dependList)
    contTabBasBF$showSpecifiedColumnsOnly <- TRUE
    contTabBasBF$position <- 2
    
    # Add columns to table
    .contTabBasLayersColumns(contTabBasBF, analysis)
    contTabBasBF$addColumnInfo(name = "type[BF]",  title = "",      type = "string")
    contTabBasBF$addColumnInfo(name = "value[BF]", title = "Value", type = "number")
    contTabBasBF$addColumnInfo(name = "type[N]",   title = "",      type = "string")
    contTabBasBF$addColumnInfo(name = "value[N]",  title = "Value", type = "integer")
    
    analysisContainer[["contTabBasBF"]] <- contTabBasBF
    
    # Get Group List
    groupList <- analysisContainer[["groupList"]]$object
    # Compute bfList
    .contTabBasComputeBfList(options, analysisContainer, groupList, ready)
    # Get bfList
    bfList <- analysisContainer[["bfList"]]$object
    
    res <- try(.contTabBasBFRows(analysis$rows, groupList$group.matrices, 
                                 groupList$groups, bfList, 
                                 analysisContainer, options, ready))
    .contTabBasSetErrorOrFill(res, contTabBasBF)
  }
}

.contTabBasLogOdds <- function(jaspResults, options, analyses, ready) {
  if(!options$oddsRatio)
    return()
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
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
    .contTabBasLayersColumns(contTabBasLogOdds, analysis)
    #contTabBasLogOdds$addColumnInfo(name = "type[oddsRatio]",  title = "", type = "string")
    contTabBasLogOdds$addColumnInfo(name = "value[oddsRatio]", title = "Log Odds Ratio", 
                               type = "number")
    contTabBasLogOdds$addColumnInfo(name = "low[oddsRatio]",   title = "Lower", 
                               overtitle = ci.label, type = "number", format = "dp:3")
    contTabBasLogOdds$addColumnInfo(name = "up[oddsRatio]",    title = "Upper", 
                               overtitle = ci.label, type = "number", format = "dp:3")
    
    analysisContainer[["contTabBasLogOdds"]] <- contTabBasLogOdds
    
    # Get Group List
    groupList <- analysisContainer[["groupList"]]$object
    
    # Get bfList
    bfList <- analysisContainer[["bfList"]]$object
    
    res <- try(.contTabBasOddsRatioRows(analysis$rows, groupList$group.matrices, 
                                        groupList$groups, bfList,
                                        analysisContainer, options, ready))
   
    .contTabBasSetErrorOrFill(res, contTabBasLogOdds)
  }
}

.contTabBasCramersV <- function(jaspResults, options, analyses, ready) {
  if(!options$effectSize)
    return()
  for (i in 1:length(analyses)){
    analysis <- analyses[[i]]
    analysisContainer <- jaspResults[[paste0("tables", i)]]
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
    .contTabBasLayersColumns(contTabBasCramersV, analysis)
    contTabBasCramersV$addColumnInfo(name = "value[CramerV]", title = "Cramer's V", 
                                     type = "number")
    contTabBasCramersV$addColumnInfo(name = "low[CramerV]", title = "Lower", 
                                     overtitle = ci, type = "number", format="dp:3")
    contTabBasCramersV$addColumnInfo(name = "up[CramerV]",  title = "Upper", 
                                     overtitle = ci, type = "number", format="dp:3")
    
    
    analysisContainer[["contTabBasCramersV"]] <- contTabBasCramersV
    
    # Get Group List
    groupList <- analysisContainer[["groupList"]]$object
    
    # Get bfList
    bfList <- analysisContainer[["bfList"]]$object
    
    res <- try(.contTabBasCramersVRows(analysis$rows, groupList$group.matrices, 
                                       groupList$groups, bfList,
                                       analysisContainer, options, ready))
    .contTabBasSetErrorOrFill(res, contTabBasCramersV)
  }
}

# Log Odds Plot
.contTabBasLogOddsPlot <- function(jaspResults, options, analyses, ready) {
  if(!options$plotPosteriorOddsRatio)
    return()
  oddsRatioPlotContainer <- createJaspContainer("Log Odds Ratio Plots")
  dependList <- c("plotPosteriorOddsRatio", "hypothesis", "samplingModel",
                  "plotPosteriorOddsRatioAdditionalInfo", "priorConcentration")
  oddsRatioPlotContainer$dependOn(dependList)
  .contTablesBayesianCitations(oddsRatioPlotContainer)
  for (i in 1:length(analyses)){
    if (!is.null(oddsRatioPlotContainer[[paste0("plots", i)]])) 
      next
    analysisContainer <- jaspResults[[paste0("tables", i)]]
    # Get Group List
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
      analysis <- analyses[[i]]
      title <- ""
      
      if(length(options$rows) > 0 || length(options$columns) > 0)
        title <- paste0(analysis$rows, " - ", analysis$columns)
      logOddsPlot <- createJaspPlot(title = title, width = 530, height = 400)
      oddsRatioSubContainer[["plot"]] <- logOddsPlot
      oddsRatioPlotContainer[[paste0("plots", i, "sub", g)]] <- oddsRatioSubContainer
      
      if(ready){
        analysisSamples.g <- analysisContainer[["logOddsSamples"]][[g]]
        # Get bfList
        bfList <- analysisContainer[["bfList"]]$object
        bf.results.g <- bfList[[g]]
        p <- try(.contTabBasLogOddsPlotFill(jaspResults, options, analysisContainer,
                                            analysisSamples.g,
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

.contTabBasLogOddsPlotFill <- function(jaspResults, options, analysisContainer, 
                                       analysisSamples.g,
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
  median  <- result$median
  xName   <- "Log Odds Ratio"
  if(options$plotPosteriorOddsRatioAdditionalInfo)
    p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLines, xName = xName, BF01 = 1/BF10,
                                           CRI = ppCri, median = median, bfSubscripts = bfSubscripts)
  else 
    p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLines, xName = xName, CRI = ppCri,  
                                           bfSubscripts = bfSubscripts, draqCRItxt = FALSE)
  return(p)
}

# Table functions 
.contTabBasLayersColumns <- function(table, analysis) {
  if ((length(analysis)) >= 3)
    for (j in (length(analysis)):3)
      table$addColumnInfo(name = analysis[[j]], type = "string", combine = TRUE)
}

.contTabBasMainOvertitle <- function(dataset, options, table, analysis, counts.fp) {
  
  lvls <- c()
  overTitle <- unlist(analysis$columns)
  
  if(analysis$columns == "") {
    lvls <- c("a", "b")
    for (column.name in lvls) {
      private.name <- base::paste(column.name,"[counts]", sep = "")
      
      if (counts.fp) 
        table$addColumnInfo(name = private.name, title = ".", 
                            overtitle = ".", type = "number", format = "sf:4;dp:2")
      else 
        table$addColumnInfo(name = private.name, title = ".", 
                            overtitle = ".", type = "integer")
    }
  } else {
    if (is.factor(dataset[[ .v(analysis$columns) ]] )) {
      lvls <- base::levels(dataset[[ .v(analysis$columns) ]])
      if (options$columnOrder == "descending")
        lvls <- base::rev(lvls)
    } else {
      lvls <- base::unique(dataset[[ .v(analysis$columns) ]])
      if (options$columnOrder == "descending")
        lvls <- base::rev(lvls, decreasing = TRUE)
    }
    
    for (column.name in lvls) {
      private.name <- base::paste(column.name,"[counts]", sep = "")
      
      if (counts.fp) 
        table$addColumnInfo(name = private.name, title = column.name, 
                            overtitle = overTitle, type = "number", 
                            format = "sf:4;dp:2")
      else 
        table$addColumnInfo(name = private.name, title = column.name, 
                            overtitle = overTitle, type = "integer")
    }
  }
}

.contTabBasSetErrorOrFill <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
  else
    for (level in 1:length(res)) 
      table$addRows(res[[level]])
}

.contTabBasLayerNames <- function(row, group) {
  for (layer in names(group)) {
    level <- group[[layer]]
    if (level == "")
      row[[layer]] <- "Total"
    else 
      row[[layer]] <- level
  }
  return(row)
}

.contTabBasComputeGroups <- function(dataset, options, analysisContainer, analysis, ready) {
  if(!is.null(analysisContainer[["groupList"]]))
    return(analysisContainer[["groupList"]]$object)
  groupsList <- list() #list of groups, group.matrices
  
  counts.var <- options$counts
  if (counts.var == "")
    counts.var <- NULL
  if(ready) {
    all.vars   <- c(unlist(analysis), counts.var)
    subdataset <- subset(dataset, select = .v(all.vars))
  }
  else
    subdataset <- dataset
  # the following creates a 'groups' list
  # a 'group' represents a combinations of the levels from the layers
  # if no layers are specified, groups is null
  if (length(analysis) >= 3) {  # if layers are specified
    
    lvls <- base::levels(subdataset[[ .v(analysis[[3]]) ]])
    
    if (length(lvls) < 2)
      lvls <- ""
    else 
      lvls <- c(lvls, "")  # blank means total
    
    # here we create all combinations of the levels from the layers
    # it is easiest to do this with a data frame
    # at the end we convert this to a list of rows
    
    groups <- data.frame(lvls, stringsAsFactors=FALSE)
    base::names(groups) <- analysis[[3]]
    
    if (length(analysis) >= 4) {
      
      for (j in 4:(length(analysis))) {
        lvls <- base::levels(subdataset[[ .v(analysis[[j]]) ]])
        lvls <- c(lvls, "")  # blank means total
        
        groups <- cbind(rep(lvls, each=dim(groups)[1]), groups, 
                        stringsAsFactors=FALSE)
        names(groups)[1] <- analysis[[j]]
      }
    }
    
    # convert all the combinations to a list of rows
    groups <- .dataFrameToRowList(groups)
    
  } else  # if layers are not specified
    groups <- NULL
  groupsList$groups <- groups
  if (!is.null(counts.var))
    counts <- stats::na.omit(subdataset[[ .v(counts.var) ]])
  grp.mat <- .contTabBasGroupMatrices(subdataset, analysis$rows, 
                                      analysis$columns, groups, 
                                      counts.var, 
                                      options$rowOrder=="descending", 
                                      options$columnOrder=="descending", 
                                      ready)
  groupsList$group.matrices <- grp.mat
  analysisContainer[["groupList"]] <- createJaspState(groupsList)
}

.contTabBasComputeBfList <- function(options, analysisContainer, groupList, ready) {
  if(!is.null(analysisContainer[["bfList"]]))
    return(analysisContainer[["bfList"]]$object)
  bfList <- list()
  grp.mat <- groupList$group.matrices
  for(g in 1:length(grp.mat)) 
    bfList[[g]]   <- .contTabBasComputeBF(options, grp.mat[[g]], ready)
  analysisContainer[["bfList"]] <- createJaspState(bfList)
  bfList$dependOn(c("samplingModel", "priorConcentration", "hypothesis", "BayesFactorType"))
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
    bf1  <- bf1 * prop.consistent / 0.5
    lbf1 <- lbf1 + log(prop.consistent) - log(0.5)
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
  } else if(options$hypothesis == "groupsNotEqual" || options$samplingModel == "hypergeometric") {
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
    "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
    "Jamil, T., Ly, A., Morey, R. D., Love, J., Marsman, M., & Wagenmakers, E.-J. (2017). Default Gunel and Dickey Bayes factors for contingency tables. Behavior Research Methods, 49, 638-652.",
    "Gunel, E., & Dickey, J. (1974). Bayes factors for independence in contingency tables. Biometrika, 61, 545-557.")
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
  } else if (options$samplingModel %in% c("independentMultinomialRowsFixed", "independentMultinomialColumnsFixed")) {
    theta      <- as.data.frame(ch.result[,7:10])
    odds.ratio.samples <- (theta[,1]*theta[,4])/(theta[,2]*theta[,3])
  } else
    stop("Invalid sampling model selected")
  samples   <- log(odds.ratio.samples)
  sig       <- options$oddsRatioCredibleIntervalInterval
  alpha     <- (1 - sig) / 2
  #quantiles <- .credibleIntervalPlusMedian(credibleIntervalInterval = sig,
  #                                         mean = mean(samples), sd = sd(samples),
  #                                         hypothesis = options$hypothesis)
  #median <- quantiles$ci.median
  #lower  <- quantiles$ci.lower
  #upper  <- quantiles$ci.upper
  median <- stats::median(samples)
  lower  <- unname(stats::quantile(samples, p = alpha))
  upper  <- unname(stats::quantile(samples, p = (1-alpha)))
  samplesList <- list(log.odds.ratio.samples = samples, BF = BF, 
                      median = median, lower.ci = lower, upper.ci = upper)
  logOddsSamples    <- createJaspState(samplesList)
  analysisSamples.g <- logOddsSamples
  return(samplesList)
}

.contTabBasCountsFloatingPoint <- function(dataset, options) {
  # whether the counts are float point or not; changes formatting (decimal places)
  if (options$counts != "") {
    counts <- dataset[[ .v(options$counts) ]]
    if(identical(counts, as.integer(counts)) == FALSE)          
      return(TRUE)
  }
  else return(FALSE)
}

# Group matrix 
.contTabBasGroupMatrices <- function(dataset, rows, columns, groups, counts = NULL, 
                                     rowOrderDescending = FALSE, 
                                     columnOrderDescending = FALSE, ready) {
  
  # this creates count matrices for each of the groups
  matrices <- list()
  
  if (is.null(groups)) {
    
    if (!ready) {
      
      row.levels <- c(" .", " . ")
      col.levels <- c(" .", " . ")
      
      if (rows != "")
        row.levels <- base::levels(dataset[[ .v(rows) ]])
      if (columns != "")
        col.levels <- base::levels(dataset[[ .v(columns) ]])
      
      ss.matrix <- base::matrix(0, 
                                nrow  = length(row.levels), 
                                ncol     = length(col.levels), 
                                dimnames = list(row.levels, col.levels))
      
    } else if (is.null(counts)) {
      ss.dataset <- base::subset(dataset, select = .v(c(rows, columns)))
      ss.table   <- base::table(ss.dataset)
      ss.matrix  <- base::matrix(ss.table, nrow = dim(ss.table)[1], 
                                 ncol = dim(ss.table)[2], 
                                 dimnames = dimnames(ss.table))
      
    } else {
      ss.dataset <- base::subset(dataset, select = .v(c(rows, columns, counts)))
      ss.matrix  <- base::tapply(ss.dataset[[ .v(counts) ]], 
                                 list(ss.dataset[[ .v(rows) ]], 
                                      ss.dataset[[ .v(columns) ]]), 
                                 base::sum)
      ss.matrix[is.na(ss.matrix)] <- 0
    }
    
    if (rowOrderDescending)
      ss.matrix <- base::apply(ss.matrix, 2, base::rev)
    else 
      ss.matrix <- ss.matrix
    
    if (columnOrderDescending)
      ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
    else 
      ss.matrix <- ss.matrix
    
    ss.matrix[base::is.na(ss.matrix)] <- 0
    
    matrices[[1]] <- ss.matrix
  } else {
    
    for (group in groups) {
      
      group <- group[group != ""]
      
      if (!ready) { # do nothing
      } else if (length(group) == 0) {
        ss.dataset <- base::subset(dataset, select = .v(c(rows, columns, counts)))
      } else {
        ss.filter.string <- base::paste(.v(names(group)), "==\"", group, "\"", 
                                        sep = "", collapse = "&")
        ss.expression    <- base::parse(text = ss.filter.string)
        ss.dataset	     <- base::subset(dataset, 
                                        select = .v(c(rows, columns, counts)), 
                                        subset = eval(ss.expression))
      }
      
      if (!ready) {
        ss.matrix <- base::matrix(c(0,0,0,0), nrow = 2, ncol = 2)
      } else if (is.null(counts)) {
        ss.table  <- base::table(ss.dataset)
        ss.matrix <- base::matrix(ss.table, 
                                  nrow = dim(ss.table)[1], 
                                  ncol = dim(ss.table)[2], 
                                  dimnames = dimnames(ss.table))
      } else {
        ss.matrix <- base::tapply(ss.dataset[[ .v(counts) ]], 
                                  list(ss.dataset[[ .v(rows) ]], 
                                       ss.dataset[[ .v(columns) ]]), 
                                  base::sum)
      }
      
      ss.matrix[base::is.na(ss.matrix)] <- 0
      
      if (rowOrderDescending)
        ss.matrix <- base::apply(ss.matrix, 2, base::rev)
      else 
        ss.matrix <- ss.matrix
      
      if (columnOrderDescending)
        ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
      else 
        ss.matrix <- ss.matrix
      matrices[[length(matrices) + 1]] <- ss.matrix
    }
  }
  return(matrices)
}

# Table Results 
.contTabBasCountsRows <- function(var.name, group.matrices, groups, 
                                  analysisContainer, options, ready) {
  counts.rows     <- list()
  
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
    
    rows <- list()
    row.count <- list()
    row.expected <- list()
    row.row.proportions <- list()
    row.col.proportions <- list()
    row.total.proportions <- list()
    row.proportions <- list()
    row.count[["type[counts]"]] <- "Count"
    
    for (j in 1:dim(counts.matrix)[[1]]) {
      
      if (ready) {
        
        row <- as.list(counts.matrix[j,])
        names(row) <- base::paste(names(row),"[counts]",	sep = "")
        row[["total[counts]"]] <- base::sum(counts.matrix[j,])
        row <- c(row.count, row)
        
      } else 
        row <- list()
      row <- .contTabBasLayerNames(row, group)
      row[[var.name]] <- dimnames(counts.matrix)[[1]][j]
      if (j == 1)
        row[[".isNewGroup"]] <- TRUE
      rows[[length(rows) + 1]] <- row
    }
    
    if (ready) {
      
      row <- apply(counts.matrix, 2, base::sum)
      row <- as.list(row)
      names(row) <- base::paste(names(row),"[counts]",	sep = "")
      row[["total[counts]"]] <- base::sum(counts.matrix)
      row <- c(row.count, row)
      
    } else 
      row <- list()
    if(var.name != "")
      row[[var.name]] <- "Total"
    
    row[[".isNewGroup"]] <- TRUE
    
    row <- .contTabBasLayerNames(row, group)
    rows[[length(rows) + 1]] <- row
    counts.rows <- c(counts.rows, rows)
  }
  return(counts.rows)
}

.contTabBasBFRows <- function(var.name,  group.matrices, groups, bf.results, 
                              analysisContainer, options, ready) {
  bf.rows <- list()
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
      row[["value[N]"]]  <- base::sum(counts.matrix)
    } else {
      row[["value[BF]"]] <- "."
      row[["value[N]"]]  <- "."
    }
    row <- .contTabBasLayerNames(row, group)
    bf.rows[[length(bf.rows) + 1]] <- row
  }
  .contTabBasBFFootnote(options, counts.matrix, analysisContainer, ready) 
  return(bf.rows)
}

.contTabBasOddsRatioRows <- function(var.name,  group.matrices, groups, bf.results, 
                                     analysisContainer, options, ready) {
  odds.ratio.rows <- list()
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    row <- .contTabBasLayerNames(row, group)
    
    row[["type[oddsRatio]"]] <- "Odds ratio"
    result <- list("median" = ".",
                   "lower"  = ".",
                   "upper"  = ".")
    if (!identical(dim(counts.matrix), as.integer(c(2,2)))) {
      message <- "Odds ratio restricted to 2 x 2 tables"
      analysisContainer[["contTabBasLogOdds"]]$addFootnote(message)
    } else if ( options$samplingModel == "hypergeometric") {
      row[["value[oddsRatio]"]] <- NaN
      row[["low[oddsRatio]"]]   <- ""
      row[["up[oddsRatio]"]]    <- ""
      message <- "Odd ratio for this model not yet implemented"
      analysisContainer[["contTabBasLogOdds"]]$addFootnote(message)
    } else if(ready){
      bf.results.g <- bf.results[[g]]
      analysisSamples.g <- analysisContainer[["logOddsSamples"]][[g]]
      result <- try(.contTabBasOddsRatioSamples(analysisSamples.g, bf.results.g, options))
    }
    row[["value[oddsRatio]"]] <- result$median
    row[["low[oddsRatio]"]]   <- result$lower
    row[["up[oddsRatio]"]]    <- result$upper
    
    odds.ratio.rows[[length(odds.ratio.rows) + 1]] <- row
  }
  return(odds.ratio.rows)
}

.contTabBasCramersVRows <- function(var.name,  group.matrices, groups, bf.results, 
                                    analysisContainer, options, ready) {
  cv.rows <- list()
  for (g in 1:length(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL
    
    row <- list()
    row <- .contTabBasLayerNames(row, group)
    
    row[["type[CramerV]"]] <- "Cramer's V"
    if ( options$samplingModel == "hypergeometric") {
      row[["value[CramerV]"]] <- NaN
      row[["low[CramerV]"]]   <- ""
      row[["up[CramerV]"]]    <- ""
      
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
          chi2      <- apply(theta0, 2, function(data) chisq.test(matrix(data,I))$statistic)
          Phi.Poisson <- sqrt(chi2/(sumlambda*(k-1)))
          Cramer    <- Phi.Poisson
        } else if (options$samplingModel == "jointMultinomial") {
          theta  <- as.data.frame(ch.result)
          theta0 <- apply(theta, 1, function(x) matrix(x,I))
          chi2   <- apply(theta0 * N, 2, function(data) chisq.test(matrix(data,I))$statistic)
          Phi.jointMulti <- sqrt(chi2/(N*(k-1)))
          Cramer <- Phi.jointMulti
        } else if (options$samplingModel %in% c("independentMultinomialRowsFixed", "independentMultinomialColumnsFixed")) {
          index <- grep(pattern = "omega", x = colnames(ch.result))
          theta <- as.data.frame(ch.result[,index])
          if(options$samplingModel == "independentMultinomialRowsFixed") {
            theta0 <- apply(theta, 1, function(x) matrix(x,I))
            chi2   <- apply(theta0 * yr, 2, function(data) chisq.test(matrix(data,I))$statistic)
          } else if(options$samplingModel == "independentMultinomialColumnsFixed") {
            theta0 <- apply(theta, 1, function(x) matrix(x,J, byrow=TRUE))
            chi2   <- apply(theta0 * yc, 2, function(data) chisq.test(matrix(data,J))$statistic)
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
      result <- list("CVmedian" = ".",
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
      linesPosterior[1] <- 0
    } else if (oneSided == "left") {
      idx <- logOR <= 0 # the nonzero components
      linesPosterior[idx] <- dnorm(logOR, mean, sd) / pnorm(0, mean, sd, lower.tail = TRUE)
      linesPosterior[size] <- 0
    }
  }
  logORGroup <- logOR
  nameGroup  <- rep("Posterior", size)
  dat        <- data.frame(x = logORGroup, y = linesPosterior, g = nameGroup)
  return(dat)
}

#CRI and Median 
.credibleIntervalPlusMedian <- function(credibleIntervalInterval = .95, mean, sd, hypothesis = "groupsNotEqual") {
  
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
