.getPairsIndeces <- function(options) {
  pairs <- options[["pairs"]]
  nPairs <- length(pairs)
  
  allIndeces <- seq_len(nPairs)
  
  for (i in seq_along(pairs)) {
    pair <- pairs[[i]]
    if ((pair[1] == "" || pair[2] == "") || pair[1] == pair[2]) {
      allIndeces <- setdiff(allIndeces, i)
    }
  }
  
  return(allIndeces)
}

.getCorTableTitle <- function(methodItems, bayes=TRUE) {
  if (bayes) {
    if (length(methodItems)==1) {
      tabTitle <- switch(methodItems,
                         pearson  = gettext("Bayesian Pearson Correlations"),
                         kendall  = gettext("Bayesian Kendall's Tau Correlations"),
                         spearman = gettext("Bayesian Spearman Correlations")
      )
    } else {
      tabTitle <- gettext("Bayesian Correlation Table")
    }
  } else {
    if (length(methodItems)==1) {
      tabTitle <- switch(methodItems,
                         pearson  = gettext("Pearson Correlations"),
                         kendall  = gettext("Kendall's Tau Correlations"),
                         spearman = gettext("Spearman Correlations")
      )
    } else {
      tabTitle <- gettext("Correlation Table")
    }
  }
  return(tabTitle)
}

.corBayesReadData <- function(dataset, options) {
  allVariables <- unlist(options[["variables"]])
  
  if (options[["missingValues"]] == "excludeListwise") {
    dataset <- .readDataSetToEnd(columns.as.numeric=allVariables, exclude.na.listwise=allVariables)
  } else {
    dataset <- .readDataSetToEnd(columns.as.numeric=allVariables)
  }
  
  return(dataset)
}

# This can be made general for t-tests as well
.getCorPlotItems <- function(options, bayes=TRUE, sumStat=FALSE) {
  
  if (isTRUE(sumStat)) {
    plotItems <- c("plotPriorPosterior", "plotBfRobustness")
  } else {
    plotItems <- c("plotScatter", "plotPriorPosterior", "plotBfSequential", "plotBfRobustness")
    
    if (!options[["plotScatter"]])
      plotItems <- setdiff(plotItems, "plotScatter")
    
    if (!options[["plotBfSequential"]])
      plotItems <- setdiff(plotItems, "plotBfSequential")
  }
  
  if (!options[["plotPriorPosterior"]]) 
    plotItems <- setdiff(plotItems, "plotPriorPosterior")
  if (!options[["plotBfRobustness"]])
    plotItems <- setdiff(plotItems, "plotBfRobustness")
  
  return(plotItems)
}

.getCorMethods <- function(options) {
  methodItems <- c("pearson", "kendall", "spearman")
  
  if (!options[["pearson"]]) 
    methodItems <- setdiff(methodItems, "pearson")
  if (!options[["kendall"]])
    methodItems <- setdiff(methodItems, "kendall")
  if (!options[["spearman"]])
    methodItems <- setdiff(methodItems, "spearman")
  
  return(methodItems)
}

.bCorCitationsList <- list("pearson"=c("Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2016). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Journal of Mathematical Psychology, 72, 19-32.",
                                       "Ly, A., Marsman, M., Wagenmakers, E.-J. (2018). Analytic Posteriors for Pearson's Correlation Coefficient. Statistica Neerlandica, 72(1), 4-13."),
                           "kendall"=c("van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2018). Bayesian Inference for Kendall's Rank Correlation Coefficient. The American Statistician, 72(4), 303-308.", 
                                       "van Doorn, J.B., Ly, A., Marsman, M., & Wagenmakers, E.-J. (2019). Bayesian Estimation of Kendall's tau Using a Latent Normal Approach. Statistics and Probability Letters, 145, 268-272."),
                           "spearman"=c("van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (in press). Bayesian Rank-Based Hypothesis Testing for the Rank Sum Test, the Signed Rank Test, and Spearman's rho. Manuscript submitted for publication")
)

.getCorCitations <- function(methodItems, bayes=TRUE) {
  citations <- NULL
  if (bayes==TRUE) 
    citations <- unlist(.bCorCitationsList[methodItems], use.names=FALSE)
  
  if (is.null(citations)){
    citations <- ""
  }
  return(citations)
}


.corMethodNamesList <- list(pearson = gettext("Pearson's r"), spearman = gettext("Spearman's rho"), kendall = gettext("Kendall's tau B"))
.corOverTitlesList  <- list(pearson = gettext("Pearson"),     spearman = gettext("Spearman"),       kendall = gettext("Kendall"))


# TODO(Alexander):  ADAPTED from Simon
# 
.bCorMarginalDistribution <- function(variable, varName, options, xName = NULL, yName = gettext("Density"), 
                                      coord_flip = FALSE, plotRanks=FALSE) {
  if (length(variable) < 3) 
    return(.displayError(errorMessage = gettext("Plotting not possible: Number of observations is < 3")))
  
  if (any(is.infinite(variable))) 
    return(.displayError(errorMessage = gettext("Plotting not possible: Infinite value(s)")))
  
  if (!options[["pearson"]])
    variable <- rank(variable)
  
  p <- .plotMarginalCor(variable = variable, xName = xName, yName = yName)
  
  if(coord_flip){
    p <- p + ggplot2::coord_flip() +
      ggplot2::theme(axis.ticks.y = ggplot2::element_line(), axis.ticks.x = ggplot2::element_blank(), 
                     axis.text.x = ggplot2::element_blank())
  }
  
  return(p)
}

# TODO(Alexander):  ADAPTED from Simon
# 
.bCorScatter <- function(x, y, options, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL, 
                         drawAxes = TRUE) {
  
  if (!options[["pearson"]]) {
    x <- rank(x)
    y <- rank(y)
  }
  
  # TODO(Alexander): Ask Simon: Why does it return an error if I add drawAxes = drawAxes? 
  # 
  .plotScatter(xVar = x, yVar = y, xBreaks = xBreaks, yBreaks = yBreaks, xName = xName, yName = yName) 
}

# Stolen from Simon's correlation.R: 
# 
#     - .plotScatter
#       .displayError
#       .plotMarginalCor

.corGLegendList <- list(pearson  = gettext("rho"),
                        kendall  = gettext("tau"), 
                        spearman = gettext("rho[s]")
)

.corXNames <- list(pearson  = bquote(paste(.(gettext("Pearson's")), ~rho)),
                   spearman = bquote(paste(.(gettext("Spearman's")), ~rho[s])),
                   kendall  = bquote(paste(.(gettext("Kendall's")), ~tau))
)

.bCorRowNames <- function(options, itemNames, method=c("pearson", "kendall", "spearman")) {
  rowStatName <- list(pearson = gettext("Pearson's r"), kendall = gettext("Kendall's tau"), spearman = gettext("Spearman's rho"))
  
  bfTitle <- .getBfTitle("bfType"=options[["bayesFactorType"]], "alternative"=options[["alternative"]])

  allRowNames <- list(n = gettext("n"), bf = bfTitle, 
                      upperCi = gettextf("Upper %s%% CI", options[["ciValue"]] * 100),
                      lowerCi = gettextf("Lower %s%% CI", options[["ciValue"]] * 100))
  
  if (!is.null(method))
    allRowNames[["stat"]] <- rowStatName[[method[1]]]

  return(allRowNames[itemNames])
}

