.getPairsLength <- function(options) {
  pairs <- options[["pairs"]]
  nPairs <- length(pairs)
  
  if (nPairs!=0) {
    lastPair <- pairs[[nPairs]]
    v2 <- lastPair[2]
    
    if (is.na(v2))
      nPairs <- nPairs -1
  }
  
  return(max(nPairs, 0))
}

.getCorTableTitle <- function(methodItems, bayes=TRUE) {
  if (bayes) {
    if (length(methodItems)==1) {
      tabTitle <- switch(methodItems,
                         "pearson"="Bayesian Pearson Correlation",
                         "kendall"="Bayesian Kendall Correlation",
                         "spearman"="Bayesian Spearman Correlation"
      )
    } else {
      tabTitle <-"Bayesian Correlation Table"
    }
  } else {
    if (length(methodItems)==1) {
      tabTitle <- switch(methodItems,
                         "pearson"="Bayesian Pearson Correlation",
                         "kendall"="Bayesian Kendall Correlation",
                         "spearman"="Bayesian Spearman Correlation"
      )
    } else {
      tabTitle <-"Bayesian Correlation Table"
    }
  }
  return(tabTitle)
}

.corBayesReadData <- function(dataset, options) {
  firstList <- unlist(options[["variables"]])
  secondList <- unlist(options[["pairs"]])
  allVariables <- unique(c(firstList, secondList))
  allVariables <- allVariables[allVariables != ""]
  
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
                                       "Ly, A., Marsman, M., Wagenmakers, E.-J. (2018). Analytic Posteriors for Pearson’s Correlation Coefficient. Statistica Neerlandica, 72(1), 4-13."),
                           "kendall"=c("van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2018). Bayesian Inference for Kendall’s Rank Correlation Coefficient. The American Statistician, 72(4), 303-308.", 
                                       "van Doorn, J.B., Ly, A., Marsman, M., & Wagenmakers, E.-J. (2019). Bayesian Estimation of Kendall’s tau Using a Latent Normal Approach. Statistics and Probability Letters, 145, 268-272."),
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


.corMethodNamesList <- list(pearson="Pearson's r", spearman="Spearman's rho", kendall="Kendall's tau B")
.corOverTitlesList <- list(pearson="Pearson", spearman="Spearman", kendall="Kendall")


# TODO(Alexander):  ADAPTED from Simon
# 
.bCorMarginalDistribution <- function(variable, varName, options, xName = NULL, yName = "Density", 
                                      coord_flip = FALSE, plotRanks=FALSE) {
  if (length(variable) < 3) 
    return(.displayError(errorMessage = "Plotting not possible:\n Number of observations is < 3"))
  
  if (any(is.infinite(variable))) 
    return(.displayError(errorMessage = "Plotting not possible: Infinite value(s)"))
  
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

# # TODO(Alexander): Remove this, since it's stolen from Simon
# # 
# .plotScatter <- function(xVar, yVar, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL, drawAxes = TRUE) {
#   isNumericX <- !(is.factor(xVar) || (is.integer(xVar) && length(unique(xVar)) <= 10))
#   isNumericY <- !(is.factor(yVar) || (is.integer(yVar) && length(unique(yVar)) <= 10))
#   bothNumeric <- isNumericX && isNumericY
#   d <- data.frame(x = xVar, y = yVar)
#   d <- na.omit(d)
#   
#   if (!isNumericX)
#     d$x <- as.factor(d$x)
#   
#   if (!isNumericY)
#     d$y <- as.factor(d$y)
#   
#   if (is.null(xBreaks))
#     xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)
#   
#   fit <- NULL
#   
#   if (bothNumeric) {
#     
#     fit <- lm(y ~ poly(x, 1, raw = TRUE), d)
#     lineObj <- .poly.predDescriptives(fit, line = FALSE, xMin= xBreaks[1], xMax = xBreaks[length(xBreaks)], lwd = lwd)
#     rangeLineObj <- c(lineObj[1], lineObj[length(lineObj)])
#     yLimits <- range(c(pretty(yVar)), rangeLineObj)
#     
#     if (is.null(yBreaks))
#       yBreaks <- JASPgraphs::getPrettyAxisBreaks(yLimits)
#     
#   } else if (is.null(yBreaks)) {
#     
#     yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)
#     
#   }
#   
#   p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
#     JASPgraphs::geom_point()
#   
#   if (bothNumeric) {
#     xr <- range(xBreaks)
#     dfLine <- data.frame(x = xr, y = rangeLineObj)
#     p <- p + ggplot2::geom_line(data = dfLine, ggplot2::aes(x = x, y = y), size = .7, inherit.aes = FALSE)
#   }
#   
#   if(drawAxes){
#     if (isNumericX) {
#       p <- p + ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
#     } else {
#       p <- p + ggplot2::scale_x_discrete(name = xName)
#     }
#     if (isNumericY) {
#       p <- p + ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))
#     } else {
#       p <- p + ggplot2::scale_y_discrete(name = yName)
#     }
#   } else{
#     p <- p + ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, labels = NULL, limits = range(xBreaks))
#     p <- p + ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks, labels = NULL, limits = range(yBreaks))
#   }
#   
#   return(JASPgraphs::themeJasp(p))
# }
# 
# 
# # TODO(Alexander): Remove this, since it's stolen from Simon
# # 
# .displayError <- function(errorMessage=NULL, cexText=1.6, lwdAxis= 1.2, wrap = 20) {
#   if (!is.null(wrap)) errorMessage <- paste(strwrap(errorMessage, wrap), collapse="\n")
#   
#   p <- ggplot2::ggplot(data = data.frame(), ggplot2::aes(x = seq_along(data.frame()),y = summary(data.frame()))) +
#     ggplot2::theme(
#       panel.border = ggplot2::element_blank(),
#       panel.grid.major = ggplot2::element_blank(),
#       panel.grid.minor = ggplot2::element_blank(),
#       panel.background = ggplot2::element_blank(),
#       axis.line = ggplot2::element_blank(),
#       axis.ticks.y = ggplot2::element_blank(),
#       axis.ticks.x = ggplot2::element_blank(),
#       axis.text.y = ggplot2::element_blank(),
#       plot.margin = grid::unit(c(2,1,1,2), "cm"),
#       axis.text.x =ggplot2::element_blank(),
#       axis.title = ggplot2::element_blank()) +
#     ggplot2::annotate("text", x = 0, y = 0, label = errorMessage, size = 5) +
#     ggplot2::xlim(-30, 30) +
#     ggplot2::ylim(-30, 30)
#   return(p)
# }
# 
# # TODO(Alexander): Remove since stolen from Simon
# # 
# .plotMarginalCor <- function(variable, xName = NULL, yName = "Density") {
#   
#   variable <- na.omit(variable)
#   isNumeric <- !(is.factor(variable) || (is.integer(variable) && length(unique(variable)) <= 10))
#   
#   
#   if (isNumeric) {
#     p <- ggplot2::ggplot(data = data.frame(x = variable))
#     h <- hist(variable, plot = FALSE)
#     hdiff <- h$breaks[2L] - h$breaks[1L]
#     xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(variable, h$breaks), min.n = 3)
#     dens <- h$density
#     yBreaks <- c(0, 1.2*max(h$density))
#     
#     p <- p + ggplot2::geom_histogram(
#       mapping  = ggplot2::aes(x = x, y = ..density..),
#       binwidth = hdiff,
#       fill     = "grey",
#       col      = "black",
#       size     = .3,
#       center   = hdiff / 2,
#       stat     = "bin"
#     ) +
#       ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
#   } else {
#     
#     p <- ggplot2::ggplot(data = data.frame(x = factor(variable)))
#     hdiff <- 1L
#     xBreaks <- unique(variable)
#     yBreaks <- c(0, max(table(variable)))
#     p <- p + ggplot2::geom_bar(
#       mapping  = ggplot2::aes(x = x),
#       fill     = "grey",
#       col      = "black",
#       size     = .3,
#       stat     = "count"
#     ) +
#       ggplot2::scale_x_discrete(name = xName, breaks = xBreaks)
#   }
#   
#   yLim <- range(yBreaks)
#   
#   if (isNumeric) {
#     density <- density(variable)
#     p <- p + ggplot2::geom_line(data = data.frame(x = density$x, y = density$y),
#                                 mapping = ggplot2::aes(x = x, y = y), lwd = .7, col = "black")
#   }
#   
#   thm <- ggplot2::theme(
#     axis.ticks.y = ggplot2::element_blank(),
#     axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
#   )
#   p <- p +
#     ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, labels = c("", ""), limits = yLim) +
#     ggplot2::theme()
#   return(JASPgraphs::themeJasp(p) + thm)
#   
# }

# .corGLegendList <- list("pearson"=expression(rho),
#                         "kendall"=expression(tau), 
#                         "spearman"=expression(rho[s])
# )
.corGLegendList <- list("pearson"="rho",
                        "kendall"="tau", 
                        "spearman"="rho[s]"
)


.corXNames <- list("pearson"=expression(paste("Pearson's ", rho)), 
                   "spearman"=expression(paste("Spearman's ", rho)), 
                   "kendall"=expression(paste("Kendall's ", tau))
)



