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

# When making changes to this file always mention @koenderks as a 
# reviewer in the Pull Request

################################################################################
################## Common functions for Bayesian calculations ##################
################################################################################

.jfa.credibleInterval.calculation <- function(options, parentState){
  
  # In calculation of the credible interval, we split the confidence from the
  # original one-sided bound in two so that it becomes two-sided.
  # Example: A 95% credible bound corresponds to the 95th percentile of the 
  # posterior distribution. A 95% credible interval corresponds to the 
  # 2.5th and 97.5th percentiles 2.5 and 97.5 of the posterior distribution.
  
  lowerBoundConfidence 		<- (1 - parentState[["confidence"]]) / 2
  upperBoundConfidence 		<- parentState[["confidence"]] + (1 - parentState[["confidence"]]) / 2
  
  likelihood 				<- parentState[["method"]]
  N 						<- parentState[["N"]]
  n 						<- parentState[["n"]]
  k 						<- parentState[["k"]]
  
  alphaParameterPosterior 	<- parentState[["posterior"]][["description"]]$alpha
  betaParameterPosterior 	<- parentState[["posterior"]][["description"]]$beta
  
  if(likelihood == "poisson"){
    lowerBound <- qgamma(lowerBoundConfidence, shape = alphaParameterPosterior, rate = betaParameterPosterior)
    upperBound <- qgamma(upperBoundConfidence, shape = alphaParameterPosterior, rate = betaParameterPosterior)
  } else if(likelihood == "binomial"){
    lowerBound <- qbeta(lowerBoundConfidence, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior)
    upperBound <- qbeta(upperBoundConfidence, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior)                                        
  } else if(likelihood == "hypergeometric"){
    lowerBound <- jfa:::.qBetaBinom(lowerBoundConfidence, N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior)	/ N						
    upperBound <- jfa:::.qBetaBinom(upperBoundConfidence, N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior) / N                             
  } else if(likelihood == "regression"){
    lowerBound <- (parentState[["popBookvalue"]] - parentState[["lowerBound"]]) / parentState[["popBookvalue"]]
    upperBound <- (parentState[["popBookvalue"]] - parentState[["upperBound"]]) / parentState[["popBookvalue"]]
  }
  
  if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != ""){
    totalLowerBound <- (parentState[["mle"]] + lowerBound * parentState[["unseenValue"]]) / parentState[["populationValue"]]
    totalUpperBound <- (parentState[["mle"]] + upperBound * parentState[["unseenValue"]]) / parentState[["populationValue"]]
    results <- list(lowerBound = totalLowerBound, upperBound = totalUpperBound,
                    unseenLowerBound = lowerBound, unseenUpperBound = upperBound)
    return(results)
  }
  
  results <- list(lowerBound = lowerBound, upperBound = upperBound)
  return(results)
}

.jfa.bayesRegression.calculation <- function(options, sample, prevOptions){
  
  # Bayesian Linear Regression Using the BAS package
  # This will be replaced by the WASEM technique.
  sample <- stats::na.omit(sample)
  
  sample <- sample[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
  colnames(sample) <- c("ist", "soll")
  formula <- soll ~ ist
  
  if(all(sample[,1] == sample[,2])){
    betas <- c(1, 1, 1)
  } else {	
    basResult <- BAS::bas.lm(formula, data = sample)
    if(options[["areaUnderPosterior"]] == "displayCredibleBound"){
      basSummary <- BAS:::confint.coef.bas(coef(basResult), level = prevOptions[["confidence"]] - (1 - prevOptions[["confidence"]]))
    } else {
      basSummary <- BAS:::confint.coef.bas(coef(basResult), level = prevOptions[["confidence"]])
    }
    betas <- as.numeric(basSummary[2, c(1,2, 3)]) 
  }
  
  beta 	<- betas[3]
  betaMin <- betas[1]
  betaMax <- betas[2]	
  n 		<- nrow(sample)
  N 		<- prevOptions[["populationSize"]]
  taints 	<- (sample[["ist"]] - sample[["soll"]])/ sample[["ist"]]
  k 		<- length(which(taints != 0))
  t 		<- sum(taints)
  meanB 	<- mean(sample[["ist"]])
  meanW 	<- mean(sample[["soll"]])
  corBW 	<- cor(sample[["ist"]], sample[["soll"]])
  sdW 	<- sd(sample[["auditValue"]])
  
  pointEstimate <- N * meanW + beta * (prevOptions[["populationValue"]] - N * meanB)
  stDev <- sdW * sqrt(1 - corBW^2) * ( N / sqrt(n)) * sqrt( (N-n) / (N-1) )
  
  lowerBound <- pointEstimate - betaMax * stDev
  upperBound <- pointEstimate - betaMin * stDev
  
  results 						<- list()
  results[["n"]] 				<- as.numeric(n)
  results[["k"]] 				<- as.numeric(k)
  results[["t"]] 				<- as.numeric(t)
  results[["confidence"]] 		<- as.numeric(prevOptions[["confidence"]])
  results[["method"]] 			<- "regression"
  results[["popBookvalue"]] 	<- as.numeric(prevOptions[["populationValue"]])
  results[["pointEstimate"]] 	<- as.numeric(pointEstimate)
  results[["lowerBound"]] 		<- as.numeric(lowerBound)
  results[["upperBound"]] 		<- as.numeric(upperBound)
  results[["materiality"]] 		<- as.numeric(prevOptions[["materiality"]])
  
  return(results)
}

.jfa.bayesCorrelation.calculation <- function(r, n){
  
  # For the calculation of the Bayes factor for a correlation, we use the standard Bayesian test as described in:
  # Wetzels, R., & Wagenmakers, E. J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. Psychonomic bulletin & review, 19(6), 1057-1064.
  
  int <- function(r, n, g){
    (1+g)^((n-2)/2)*(1+(1-r^2)*g)^(-(n-1)/2) * g^(-3/2)*exp(-n/(2*g))
  }
  bf10 <- sqrt((n/2))/gamma(1/2)*integrate(int, lower = 0, upper = Inf, r = r, n = n)$value
  return(1 / bf10)
}

################################################################################
################## Common functions specific to the planning stage #############
################################################################################

.jfa.implicitSample.table <- function(options, parentState, parentContainer, jaspResults, 
                                      ready, positionInContainer){
  
  if(!options[["implicitSampleTable"]]) 
    return()
  
  .jfa.tableNumber.update(jaspResults)
  
  if(is.null(parentContainer[["sampletable"]])){
    
    tableTitle <- gettextf("<b>Table %i.</b> Implicit Sample", jaspResults[["tabNumber"]]$object)
    
    table <- createJaspTable(tableTitle)
    table$position <- positionInContainer
    table$dependOn(options = c("implicitSampleTable", 
                               "planningModel",
                               "priorStatistics"))
    
    table$addColumnInfo(name = 'n', title = gettext("Implicit sample size"), 	type = 'number')
    table$addColumnInfo(name = 'k', title = gettext("Implicit errors"), 		type = 'number')
    
    message <- base::switch(options[["priorConstructionMethod"]], 
                            "none" 		= gettext("The implicit sample is based on no existing information."),
                            "arm" 		= gettextf("The implicit sample is derived from the ARM risk assessments: IR = <b>%1$s</b> and CR = <b>%2$s</b>.", options[["IR"]], options[["CR"]]),
                            "median" 		= gettextf("The implicit sample is derived from equal prior probabilities: <i>p(H%1$s) = p(H%2$s) = 0.5</i>", "\u208B", "\u208A"),
                            "hypotheses" 	= gettextf("The implicit sample is derived from custom prior probabilities: <i>p(H%1$s) = %2$s, p(H%3$s) = %4$s</i>", "\u208B", options[["pHmin"]], "\u208A", 1 - options[["pHmin"]]),
                            "sample" 		= gettextf("The implicit sample is derived from an earlier sample of %1$s observations containing %2$s errors.", options[["sampleN"]], options[["sampleK"]]),
                            "factor" 		= gettextf("The implicit sample is derived from an earlier sample of %1$s observations containing %2$s errors weighted by a factor <i>f = %3$s</i>.", options[["sampleN"]], options[["sampleK"]], options[["factor"]]))
    table$addFootnote(message)
    
    parentContainer[["sampletable"]] <- table
    
    if(!ready || parentContainer$getError()) 
      return()
    
    row <- data.frame(n = parentState[["prior"]][["description"]]$implicitn, 
                      k = parentState[["prior"]][["description"]]$implicitk)
    
    table$addRows(row)
  }
}

.jfa.prior.plot <- function(options, parentOptions, parentState, parentContainer, jaspResults, 
                            ready, positionInContainer){
  
  if(!options[["priorPlot"]]) 
    return()
  
  .jfa.figureNumber.update(jaspResults)
  
  if(is.null(parentContainer[["priorPlot"]])){
    
    title <- ifelse(options[["separateKnownAndUnknownMisstatement"]],
                    yes = gettext("Implied Prior Distribution over Unknown Misstatement"),
                    no = gettext("Implied Prior Distribution"))
    
    figure <- createJaspPlot(plot = NULL, title = title, width = 600, height = 400)
    figure$position <- positionInContainer
    figure$dependOn(options = c("priorPlotLimit", 
                                "priorPlot", 
                                "priorPlotAdditionalInfo", 
                                "priorPlotExpectedPosterior", 
                                "planningModel",
                                "priorAndPosteriorPlotLimit",
                                "shadePrior"))
    
    parentContainer[["priorPlot"]] <- figure
    
    if(!ready || parentContainer$getError()) 
      return()
    
    likelihood 	<- parentState[["likelihood"]]
    materiality <- ifelse(options[["separateKnownAndUnknownMisstatement"]], yes = parentState[["adjustedMateriality"]], no = parentState[["materiality"]])
    expErrors 	<- parentOptions[["expectedErrors"]]
    confidence 	<- parentState[["confidence"]]
    N 			<- parentState[["N"]]
    n 			<- parentState[["sampleSize"]]
    k 			<- parentState[["expectedSampleError"]]
    
    # Determine alpha and beta parameters of the prior and expected posterior distribution
    alphaParameterPrior 	<- parentState[["prior"]][["description"]]$alpha
    betaParameterPrior 		<- parentState[["prior"]][["description"]]$beta
    alphaParameterPosterior <- parentState[["expectedPosterior"]][["description"]]$alpha
    betaParameterPosterior 	<- parentState[["expectedPosterior"]][["description"]]$beta
    
    xseq <- seq(0, options[["priorPlotLimit"]], 0.001)
    
    if(likelihood == "binomial"){
      
      # Create data to show beta prior and expected posterior, materiality, and expected errors
      dPrior 	<- data.frame(x = xseq, 		y = dbeta(xseq, shape1 = alphaParameterPrior, shape2 = betaParameterPrior), type = rep(gettext("Prior"), length(xseq)))
      dPost 	<- data.frame(x = xseq, 		y = dbeta(xseq, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior), type = rep(gettext("Expected\nposterior"), length(xseq)))
      mPrior 	<- data.frame(x = materiality, 	y = dbeta(materiality, shape1 = alphaParameterPrior, shape2 = betaParameterPrior))
      ePrior 	<- data.frame(x = expErrors, 	y = dbeta(expErrors, shape1 = alphaParameterPrior, shape2 = betaParameterPrior))
      
      # Calculate upper bounds for beta prior and posterior
      bPrior 	<- qbeta(confidence, shape1 = alphaParameterPrior, shape2 = betaParameterPrior)
      bPost 	<- qbeta(confidence, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior)
      
    } else if(likelihood == "poisson"){
      
      # Create data to show gamma prior and expected posterior, materiality, and expected errors
      dPrior 	<- data.frame(x = xseq, 		y = dgamma(xseq, shape = alphaParameterPrior, rate = betaParameterPrior), type = rep(gettext("Prior"), length(xseq)))
      dPost 	<- data.frame(x = xseq, 		y = dgamma(xseq, shape = alphaParameterPosterior, rate = betaParameterPosterior), type = rep(gettext("Expected\nposterior"), length(xseq)))
      mPrior 	<- data.frame(x = materiality, 	y = dgamma(materiality, shape = alphaParameterPrior, rate = betaParameterPrior))
      ePrior 	<- data.frame(x = expErrors, 	y = dgamma(expErrors, shape = alphaParameterPrior, rate = betaParameterPrior))
      
      # Calculate upper bounds for gamma prior and posterior
      bPrior 	<- qgamma(confidence, shape = alphaParameterPrior, rate = betaParameterPrior)
      bPost 	<- qgamma(confidence, shape = alphaParameterPosterior, rate = betaParameterPosterior)
      
    } else if(likelihood == "hypergeometric"){
      
      # Create data to show beta-binomial prior and expected posterior, materiality, and expected errors
      popK 	<- ceiling(materiality * N)
      epopK	<- base::switch(options[["expectedErrors"]], "expectedRelative" = ceiling(options[["expectedPercentage"]] * N), "expectedAbsolute" = ceiling(options[["expectedNumber"]] / parentOptions[["populationValue"]] * N))
      xseq 	<- 0:ceiling(options[["priorPlotLimit"]] * N)
      dPrior 	<- data.frame(x = xseq, 		y = jfa:::.dBetaBinom(xseq, N = N - n + k, shape1 = alphaParameterPrior, shape2 = betaParameterPrior), type = rep(gettext("Prior"), length(xseq)))
      dPost 	<- data.frame(x = xseq, 		y = jfa:::.dBetaBinom(xseq, N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior), type = rep(gettext("Expected\nposterior"), length(xseq)))
      mPrior 	<- data.frame(x = popK, 		y = jfa:::.dBetaBinom(popK, N = N - n + k, shape1 = alphaParameterPrior, shape2 = betaParameterPrior))
      ePrior 	<- data.frame(x = epopK, 		y = jfa:::.dBetaBinom(epopK, N = N - n + k, shape1 = alphaParameterPrior, shape2 = betaParameterPrior))
      
      # Calculate upper bounds for beta-binomial prior and posterior
      bPrior 	<- jfa:::.qBetaBinom(confidence, N = N - n + k, shape1 = alphaParameterPrior, shape2 = betaParameterPrior) 
      bPost 	<- jfa:::.qBetaBinom(confidence, N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior)
    }
    
    extra <- base::switch(options[["shadePrior"]],
                          "shadePriorCredibleRegion" = data.frame(x = 0, y = 0, l = "1"),
                          "shadePriorHypotheses" = data.frame(x = c(0, 0), y = c(0, 0), l = c("1", "2")))
    shade <- base::switch(options[["shadePrior"]],
                          "shadePriorCredibleRegion" = rgb(0.5, 0.8, 0.57, .7), # Lightgreen
                          "shadePriorHypotheses" = c(rgb(0, 0.64, 0.80, .6), rgb(0.98, 0.34, 0, .6))) # Blue and orange
    
    dPlot <- dPrior
    guide <- FALSE
    scaleValues <- "dashed"
    
    if(options[["priorPlotExpectedPosterior"]]){  
      dPlot <- rbind(dPlot, dPost)
      dPlot$type <- factor(dPlot$type, levels = levels(dPlot$type)[c(1,2)])
      scaleValues <- c(scaleValues, "dotted")
      guide <- ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1)
    }
    
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
    yBreaks <- c(0, 1.2 * max(dPlot$y))
    
    plot <- ggplot2::ggplot(data = dPlot, mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
      ggplot2::scale_linetype_manual(values = scaleValues, guide = guide) +
      ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, labels = c("", ""), limits = range(yBreaks))
    
    if(likelihood == "hypergeometric"){
      plot <- plot + ggplot2::scale_x_continuous(name = gettextf("Population misstatement (%1$s)", "\u03B8"), breaks = xBreaks, limits = range(xBreaks), labels = xBreaks)
    } else {
      title <- ifelse(options[["separateKnownAndUnknownMisstatement"]], yes = gettextf("Unseen population misstatement (%1$s)", "\u03B8"), no = gettextf("Population misstatement (%1$s)", "\u03B8"))
      plot <- plot + ggplot2::scale_x_continuous(name = title, breaks = xBreaks, limits = range(xBreaks), labels = paste0(xBreaks * 100, "%"))
    }
    
    if(options[["priorPlotAdditionalInfo"]]){
      if(options[["shadePrior"]] != "shadePriorNone"){ 
        plot <- plot + ggplot2::geom_point(data = extra, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = shade)
        
        if(options[["shadePrior"]] == "shadePriorCredibleRegion"){
          if(options[["priorPlotExpectedPosterior"]]){
            plot <- plot + ggplot2::scale_shape_manual(name = "", values = 21, labels = gettextf("%1$s%% Credible \nregion for %2$s", round(confidence * 100, 2), "\u03B8"))
          } else {
            plot <- plot + ggplot2::scale_shape_manual(name = "", values = 21, labels = gettextf("%1$s%% Credible region for %2$s", round(confidence * 100, 2), "\u03B8"))
          }
        } else if(options[["shadePrior"]] == "shadePriorHypotheses"){
          plot <- plot + ggplot2::scale_shape_manual(name = "", values = c(21, 21), labels = c(bquote(paste(.(gettext("Support")), ~H[{"-"}])), bquote(paste(.(gettext("Support")), ~H[{"+"}]))))
        }
        plot <- plot + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = shade, stroke = 2, color = "black")))
        
        if(options[["shadePrior"]] == "shadePriorCredibleRegion"){
          if(likelihood == "binomial")
            plot <- plot + ggplot2::stat_function(fun = dbeta, args = list(shape1 = alphaParameterPrior, shape2 = betaParameterPrior), xlim = c(0, bPrior), geom = "area", fill = shade)
          if(likelihood == "poisson")
            plot <- plot + ggplot2::stat_function(fun = dgamma, args = list(shape = alphaParameterPrior, rate = betaParameterPrior), xlim = c(0, bPrior), geom = "area", fill = shade)
          if(likelihood == "hypergeometric"){
            xseq <- xseq[1:(bPrior + 1)]
            bars <- data.frame(x = xseq, y = jfa:::.dBetaBinom(xseq, N = N - n + k, shape1 = alphaParameterPrior, shape2 = betaParameterPrior))
            plot <- plot + ggplot2::geom_bar(data = bars, stat = "identity", fill = shade)
          }
        } else if(options[["shadePrior"]] == "shadePriorHypotheses"){
          if(likelihood == "binomial")
            plot <- plot + ggplot2::stat_function(fun = dbeta, args = list(shape1 = alphaParameterPrior, shape2 = betaParameterPrior), xlim = c(0, materiality), geom = "area", fill = shade[1]) +
              ggplot2::stat_function(fun = dbeta, args = list(shape1 = alphaParameterPrior, shape2 = betaParameterPrior), xlim = c(materiality, 1), geom = "area", fill = shade[2])
          if(likelihood == "poisson")
            plot <- plot + ggplot2::stat_function(fun = dgamma, args = list(shape = alphaParameterPrior, rate = betaParameterPrior), xlim = c(0, materiality), geom = "area", fill = shade[1]) +
              ggplot2::stat_function(fun = dgamma, args = list(shape = alphaParameterPrior, rate = betaParameterPrior), xlim = c(materiality, 1), geom = "area", fill = shade[2])
          if(likelihood == "hypergeometric"){
            nseq 	<- (N * materiality + 1)
            xseq1 	<- xseq[1:nseq]
            bars1 	<- data.frame(x = xseq1, y = jfa:::.dBetaBinom(xseq1, N = N - n + k, shape1 = alphaParameterPrior, shape2 = betaParameterPrior))
            plot 	<- plot + ggplot2::geom_bar(data = bars1, stat = "identity", fill = shade[1])
            xseq2 	<- xseq[(nseq + 1)]:N
            bars2 	<- data.frame(x = xseq2, y = jfa:::.dBetaBinom(xseq2, N = N - n + k, shape1 = alphaParameterPrior, shape2 = betaParameterPrior))
            plot 	<- plot + ggplot2::geom_bar(data = bars2, stat = "identity", fill = shade[2])
          }
        }
      } 
    }
    
    if(options[["priorPlotAdditionalInfo"]])
      plot <- plot + ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y), data = mPrior, size = 3, shape = 21, stroke = 2, color = "black", fill = rgb(0.9, 0, 0, 1)) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y), data = ePrior, size = 3, shape = 21, stroke = 2, color = "black", fill = "grey")
    
    if(options[["priorPlotAdditionalInfo"]] && options[["priorPlotExpectedPosterior"]])
      plot <- plot + ggplot2::geom_segment(x = 0, xend = bPost, y = max(dPlot$y) * 1.1, yend = max(dPlot$y) * 1.1, 
                                           linetype = 1, size = 1, arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"), angle = 90)) +
      ggplot2::geom_segment(x = bPost, xend = 0, y = max(dPlot$y) * 1.1, yend = max(dPlot$y) * 1.1, 
                            linetype = 1, size = 1, arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"), angle = 90))
    
    jfaTheme <- ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                               axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0)),
                               legend.key.size = ggplot2::unit(2.5, "line"),
                               legend.position = "top")
    
    plot <- JASPgraphs::themeJasp(plot) + jfaTheme
    figure$plotObject <- plot
  }
  
  if(options[["explanatoryText"]]){
    distribution 	<- base::switch(parentOptions[["likelihood"]], "poisson" = gettext("gamma"), "binomial" = gettext("beta"), "hypergeometric" = gettext("beta-binomial"))
    additionalText1 <- gettext("The expected errors (grey dot) receive the highest probability. The red dot represents the materiality.")
    additionalText2 <- gettext("The expected posterior distribution has its upper confidence bound just below materiality.")
    
    figureCaption <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The prior probability distribution <b>(%2$s)</b> on the misstatement in the population. The prior parameters <i>%3$s = %4$s, %5$s = %6$s</i> are derived from the assessments of the inherent and control risk, along with the expected errors. %7$s %8$s",  
                                             jaspResults[["figNumber"]]$object,
                                             distribution,
                                             "\u03B1",
                                             round(parentState[["prior"]][["description"]]$alpha, 3),
                                             "\u03B2",
                                             round(parentState[["prior"]][["description"]]$beta, 3),
                                             ifelse(options[["priorPlotAdditionalInfo"]], yes = additionalText1, no = ""),
                                             ifelse(options[["priorPlotExpectedPosterior"]], yes = additionalText2, no = "")), "p")
    
    figureCaption$position <- positionInContainer + 1
    figureCaption$dependOn(optionsFromObject = parentContainer[["priorPlot"]])
    figureCaption$dependOn(options = "explanatoryText")
    parentContainer[["priorPlotText"]] <- figureCaption
  }
}

################################################################################
################## Common functions specific to the evaluation stage ###########
################################################################################

.jfa.posterior.plot <- function(options, prevOptions, prevState, parentState, parentContainer, jaspResults,
                                positionInContainer){
  
  if(!options[["priorAndPosteriorPlot"]]) 
    return()
  
  .jfa.figureNumber.update(jaspResults)
  
  if(is.null(parentContainer[["priorAndPosteriorPlot"]])){
    
    title <- ifelse(options[["separateKnownAndUnknownMisstatement"]],
                    yes = gettext("Prior and Posterior Distribution over Unknown Misstatement"),
                    no = gettext("Prior and Posterior Distribution"))
    figure <- createJaspPlot(plot = NULL, title = title, width = 600, height = 400)
    figure$position <- positionInContainer
    figure$dependOn(options = c("priorAndPosteriorPlot", 
                                "priorAndPosteriorPlotLimit", 
                                "priorAndPosteriorPlotAdditionalInfo",
                                "priorAndPosteriorPlotExpectedPosterior",
                                "priorPlotLimit",
                                "shadePosterior",
                                "priorAndPosteriorPlotIndicateStatistics"))
    
    parentContainer[["priorAndPosteriorPlot"]] <- figure
    
    if(is.null(parentState) || parentContainer$getError()) 
      return()
    
    likelihood 	<- parentState[["method"]]
    materiality <- ifelse(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != "", 
                          yes = prevState[["adjustedMateriality"]], 
                          no = prevState[["materiality"]])
    expErrors 	<- prevOptions[["expectedErrors"]]
    confidence 	<- parentState[["confidence"]]
    N 			<- prevState[["N"]]
    n 			<- parentState[["n"]]
    k 			<- parentState[["k"]]
    t 			<- parentState[["t"]]
    confBound 	<- parentState[["confBound"]]
    mle 		<- parentState[["mle"]]
    precision 	<- parentState[["precision"]]
    
    
    # Determine alpha and beta parameters of the prior and posterior distribution
    alphaParameterPrior 			<- parentState[["prior"]][["description"]]$alpha
    betaParameterPrior 				<- parentState[["prior"]][["description"]]$beta
    
    alphaParameterExpectedPosterior <- prevState[["expectedPosterior"]][["description"]]$alpha
    betaParameterExpectedPosterior 	<- prevState[["expectedPosterior"]][["description"]]$beta
    
    alphaParameterPosterior 		<- parentState[["posterior"]][["description"]]$alpha
    betaParameterPosterior 			<- parentState[["posterior"]][["description"]]$beta
    
    if(options[["separateKnownAndUnknownMisstatement"]] && options[["monetaryVariable"]] != ""){
      confBound 	<- parentState[["confBoundUnseen"]]
      mle 			<- parentState[["mleUnseen"]]
      precision 	<- parentState[["precisionUnseen"]]
    }
    
    xseq <- seq(0, options[["priorAndPosteriorPlotLimit"]], 0.001)
    
    if(likelihood == "binomial"){
      
      # Create data to show beta prior and posterior and materiality
      dPrior 	<- data.frame(x = xseq, y = dbeta(xseq, shape1 = alphaParameterPrior, shape2 = betaParameterPrior), type = rep(gettext("Prior"), length(xseq)))
      dPoste 	<- data.frame(x = xseq, y = dbeta(xseq, shape1 = alphaParameterExpectedPosterior, shape2 = betaParameterExpectedPosterior), type = rep(gettext("Expected\nposterior"), length(xseq)))
      dPost 	<- data.frame(x = xseq, y = dbeta(x = xseq, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior), type = rep(gettext("Posterior"), length(xseq)))
      mPost 	<- data.frame(x = materiality, y = dbeta(materiality, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior))	
      
      # Calculate upper bound and mle for beta posterior
      bPost 	<- dbeta(confBound, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior)
      ePost 	<- dbeta(mle, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior)
      
    } else if(likelihood == "poisson"){
      
      # Create data to show gamma prior and posterior and materiality
      dPrior 	<- data.frame(x = xseq, y = dgamma(xseq, shape = alphaParameterPrior, rate = betaParameterPrior), type = rep(gettext("Prior"), length(xseq)))
      dPoste 	<- data.frame(x = xseq, y = dgamma(xseq, shape = alphaParameterExpectedPosterior, rate = betaParameterExpectedPosterior), type = rep(gettext("Expected\nposterior"), length(xseq)))
      dPost 	<- data.frame(x = xseq, y = dgamma(xseq, shape = alphaParameterPosterior, rate = betaParameterPosterior), type = rep(gettext("Posterior"), length(xseq)))
      mPost 	<- data.frame(x = materiality, y = dgamma(materiality, shape = alphaParameterPosterior, rate = betaParameterPosterior))
      
      # Calculate upper bound and mle for gamma posterior
      bPost 	<- dgamma(confBound, shape = alphaParameterPosterior, rate = betaParameterPosterior)
      ePost 	<- dgamma(mle, shape = alphaParameterPosterior, rate = betaParameterPosterior)
      
    } else if(likelihood == "hypergeometric"){
      
      # Create data to show beta-binomial prior and posterior and materiality
      xseq 	<- 0:ceiling(options[["priorAndPosteriorPlotLimit"]] * N)
      dPrior 	<- data.frame(x = xseq, y = jfa:::.dBetaBinom(xseq, N = N - n + k, shape1 = alphaParameterPrior, shape2 = betaParameterPrior), type = rep(gettext("Prior"), length(xseq)))
      dPoste 	<- data.frame(x = xseq, y = jfa:::.dBetaBinom(xseq, N = N - n + k, shape1 = alphaParameterExpectedPosterior, shape2 = betaParameterExpectedPosterior), type = rep(gettext("Expected\nposterior"), length(xseq)))
      dPost 	<- data.frame(x = xseq, y = jfa:::.dBetaBinom(xseq, N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior), type = rep(gettext("Posterior"), length(xseq)))
      mPost 	<- data.frame(x = ceiling(materiality * N), y = jfa:::.dBetaBinom(ceiling(materiality * N), N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior))
      
      # Calculate upper bound and mle for beta-binomial posterior
      bPost 	<- jfa:::.dBetaBinom(ceiling(confBound * N), N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior)
      ePost 	<- jfa:::.dBetaBinom(ceiling(mle * N), N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior)
    }
    
    extra <- base::switch(options[["shadePosterior"]],
                          "shadePosteriorCredibleRegion" 	= data.frame(x = 0, y = 0, l = "1"),
                          "shadePosteriorHypotheses" 		= data.frame(x = c(0, 0), y = c(0, 0), l = c("1", "2")))
    
    dPlot <- rbind(dPrior, dPost)
    dPlot$type <- factor(x = dPlot$type, levels = levels(dPlot$type)[c(1,2)])
    
    if(options[["priorAndPosteriorPlotExpectedPosterior"]]){
      dPlot <- rbind(dPlot, dPoste)
      dPlot$type <- factor(x = dPlot$type, levels = levels(dPlot$type)[c(1, 2, 3)])
    }
    
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
    yBreaks <- c(0, 1.2 * max(dPlot$y))
    
    linetypes <- c("dashed", "solid")
    if(options[["priorAndPosteriorPlotExpectedPosterior"]])
      linetypes <- c(linetypes, "dotted")
    
    guide <- ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1)
    
    plot <- ggplot2::ggplot(data = dPlot, mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
      ggplot2::scale_linetype_manual(values = linetypes, guide = guide) +
      ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, labels = c("", ""), limits = c(0, max(yBreaks)))
    
    if(likelihood == "hypergeometric"){
      plot <- plot + ggplot2::scale_x_continuous(name = gettext("Population errors"), breaks = xBreaks, limits = range(xBreaks), labels = xBreaks)
    } else {
      title <- ifelse(options[["separateKnownAndUnknownMisstatement"]], 
                      yes = gettextf("Unseen population misstatement (%1$s)", "\u03B8"),
                      no = gettextf("Population misstatement (%1$s)", "\u03B8"))
      plot <- plot + ggplot2::scale_x_continuous(name = title, breaks = xBreaks, limits = range(xBreaks), labels = paste0(xBreaks * 100, "%"))
    }
    
    if(options[["priorAndPosteriorPlotAdditionalInfo"]]){
      if(options[["shadePosterior"]] != "shadePosteriorNone"){
        
        fill <- base::switch(options[["shadePosterior"]],
                             "shadePosteriorCredibleRegion" = rgb(0.37, 0.42, 0.69, 0.5), # Darkpurple
                             "shadePosteriorHypotheses" = c(rgb(0, 0.64, 0.80, .6), rgb(0.98, 0.34, 0, .6))) # Blue and orange
        
        plot <- plot + ggplot2::geom_point(data = extra, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = fill)
        
        if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){
          plot <- plot + ggplot2::scale_shape_manual(name = "", values = 21, labels = gettextf("%1$s%% Credible \nregion for %2$s", round(confidence * 100, 2), "\u03B8"))
        } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){
          plot <- plot + ggplot2::scale_shape_manual(name = "", values = c(21, 21), labels = c(bquote(paste(.(gettext("Support")), ~H[{"-"}])), 
                                                                                               bquote(paste(.(gettext("Support")), ~H[{"+"}]))))
        }
        
        plot <- plot + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, 
                                                                                         fill = fill, stroke = 2, color = "black")))
        
        if(options[["areaUnderPosterior"]] == "displayCredibleInterval"){
          ci <- .jfa.credibleInterval.calculation(options, parentState)
          functionLimits <- c(ci[["lowerBound"]], ci[["upperBound"]])
          if(options[["separateKnownAndUnknownMisstatement"]])
            functionLimits <- c(ci[["unseenLowerBound"]], ci[["unseenUpperBound"]])
        } else if(options[["areaUnderPosterior"]] == "displayCredibleBound"){
          functionLimits <- c(0, confBound)
          if(options[["separateKnownAndUnknownMisstatement"]])
            functionLimits <- c(0, parentState[["confBoundUnseen"]])
        }                                                                  
        
        if(likelihood == "binomial"){
          if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){
            plot <- plot + ggplot2::stat_function(fun = dbeta, args = list(shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior),
                                                  xlim = functionLimits, geom = "area", fill = fill)
          } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){
            plot <- plot + ggplot2::stat_function(fun = dbeta, args = list(shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior),
                                                  xlim = c(0, materiality), geom = "area", fill = fill[1]) + 
              ggplot2::stat_function(fun = dbeta, args = list(shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior),
                                     xlim = c(materiality, 1), geom = "area", fill = fill[2])
          }
        } else if(likelihood == "poisson"){
          if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){
            plot <- plot + ggplot2::stat_function(fun = dgamma, args = list(shape = alphaParameterPosterior, rate = betaParameterPosterior),
                                                  xlim = functionLimits, geom = "area", fill = fill)
          } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){
            plot <- plot + ggplot2::stat_function(fun = dgamma, args = list(shape = alphaParameterPosterior, rate = betaParameterPosterior),
                                                  xlim = c(0, materiality), geom = "area", fill = fill[1]) +
              ggplot2::stat_function(fun = dgamma, args = list(shape = alphaParameterPosterior, rate = betaParameterPosterior),
                                     xlim = c(materiality, 1), geom = "area", fill = fill[2]) 
          }
        } else if(likelihood == "hypergeometric"){
          if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){
            xseq <- xseq[(ceiling(functionLimits[1] * N) + 1):(ceiling(functionLimits[2] * N) + 1)]
            barData <- data.frame(x = xseq, y = jfa:::.dBetaBinom(xseq, N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior))
            plot <- plot + ggplot2::geom_bar(data = barData, stat = "identity", fill = fill)
          } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){
            nseq <- (N * materiality + 1)
            xseq1 <- xseq[1:nseq]
            bars1 <- data.frame(x = xseq1, y = jfa:::.dBetaBinom(xseq1, N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior))
            plot <- plot + ggplot2::geom_bar(data = bars1, stat = "identity", fill = fill[1])
            xseq2 <- xseq[(nseq + 1):N]
            bars2 <- data.frame(x = xseq2, y = jfa:::.dBetaBinom(x = xseq2, N = N - n + k, shape1 = alphaParameterPosterior, shape2 = betaParameterPosterior))
            plot <- plot + ggplot2::geom_bar(data = bars2, stat = "identity", fill = fill[2])
          }
        }
      }
      
      if(options[["performanceMateriality"]])
        plot <- plot + ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y), data = mPost, size = 3, shape = 21, stroke = 2, color = "black", fill = rgb(0.9, 0, 0, 1))
    }
    
    if(options[["priorAndPosteriorPlotIndicateStatistics"]]){
      
      if(likelihood == "hypergeometric"){
        confBound <- ceiling(confBound *N)
        mle <- ceiling(mle * N)
      }
      
      plot <- plot + ggplot2::geom_segment(x = mle, xend = confBound, y = max(dPlot$y) * 1.1, yend = max(dPlot$y) * 1.1, 
                                           linetype = 1, size = 0.5, color = "black") + 
        ggplot2::geom_segment(x = confBound, xend = confBound, y = max(dPlot$y) * 1.1, yend = bPost, 
                              linetype = "dotted", size = 0.25, color = "black") + 
        ggplot2::geom_segment(x = mle, xend = mle, y = max(dPlot$y) * 1.1, yend = ePost, 
                              linetype = "dotted", size = 0.25, color = "black") + 
        ggplot2::annotate("text", y = max(dPlot$y) * 1.15, x = mle + ((confBound - mle) / 2), 
                          label = gettextf("Precision (%1$s)", paste0(round(precision * 100, 3),"%")), 
                          size = 4, vjust = 0.5, hjust = 0.5, col = "black") +
        ggplot2::annotate("text", y = ePost, x = mle, 
                          label = gettextf("Most likely error (%1$s)", paste0(round(mle * 100, 3), "%")), 
                          size = 4, vjust = 0, hjust = ifelse(mle < (0.2 * options[["priorAndPosteriorPlotLimit"]]), yes = -0.05, no = 1.05), col = "black") +
        ggplot2::annotate("text", y = bPost, x = confBound, 
                          label = gettextf("Upper bound (%1$s)", paste0(round(confBound * 100, 3), "%")), 
                          size = 4, vjust = 0.3, hjust = -0.05, col = "black")
    }
    
    jfaTheme <- ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                               axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0)),
                               legend.key.size = ggplot2::unit(3, "line"),
                               legend.position = "top")
    
    if(options[["priorAndPosteriorPlotAdditionalInfo"]] && options[["priorAndPosteriorPlotExpectedPosterior"]] && options[["shadePosterior"]] == "shadePosteriorHypotheses")
      jfaTheme <- jfaTheme +  ggplot2::theme(legend.text = ggplot2::element_text(size = 12))
    
    plot <- JASPgraphs::themeJasp(plot) + jfaTheme
    
    figure$plotObject <- plot
  }
  
  if(options[["explanatoryText"]]){
    distribution <- base::switch(parentState[["method"]], "poisson" = gettext("gamma"), "binomial" = gettext("beta"), "hypergeometric" = gettext("beta-binomial"))
    additionalText <- gettext("The red dot represents the specified materiality. If the shaded area under the distribution surpasses this point, the estimate of the maximum misstatement exceeds the materiality.")
    figureCaption <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The prior and posterior probability distribution <b>(%2$s)</b> on the misstatement in the population. %3$s",
                                             jaspResults[["figNumber"]]$object,
                                             distribution,
                                             ifelse(options[["priorAndPosteriorPlotAdditionalInfo"]] && options[["performanceMateriality"]],
                                                    yes = additionalText,
                                                    no = "")), "p")
    
    figureCaption$position <- positionInContainer + 1
    figureCaption$dependOn(optionsFromObject = parentContainer[["priorAndPosteriorPlot"]])
    figureCaption$dependOn(options = "explanatoryText")
    parentContainer[["priorAndPosteriorPlotText"]] <- figureCaption
  }
}

################################################################################
################## Common functions not tied to a specific stage ###############
################################################################################

.jfa.distributionStatistics.table <- function(options, parentOptions, parentState, parentContainer, jaspResults, 
                                              ready = NULL, positionInContainer, stage){
  
  if(stage == "planning" && !options[["priorStatistics"]])
    return()
  if(stage == "evaluation" && !options[["priorAndPosteriorStatistics"]])
    return()
  
  .jfa.tableNumber.update(jaspResults)
  
  if(is.null(parentContainer[["priorAndPosteriorStatistics"]])){
    
    title <- base::switch(stage, 
                          "planning" = "Descriptive Statistics for Prior and Expected Posterior Distribution",
                          "evaluation" = "Descriptive Statistics for Prior and Posterior Distribution")
    
    tableTitle <- gettextf("<b>Table %1$i.</b> %2$s",
                           jaspResults[["tabNumber"]]$object,
                           title)
    
    table <- createJaspTable(tableTitle)
    table$position <- positionInContainer
    
    if(stage == "planning")
      table$dependOn(options = c("priorStatistics", 
                                 "planningModel",
                                 "implicitSampleTable",
                                 "bookValueDescriptives"))
    if(stage == "evaluation")
      table$dependOn(options = "priorAndPosteriorStatistics")
    
    table$addColumnInfo(name = 'v', 		title = "", type = 'string')
    table$addColumnInfo(name = 'form', 		title = gettext("Functional form"), type = 'string')
    
    if(options[["performanceMateriality"]]){                                               
      table$addColumnInfo(name = 'hMin', 	title = gettextf("Support %1$s", "H\u208B"), type = 'number')
      table$addColumnInfo(name = 'hPlus', title = gettextf("Support %1$s", "H\u208A"), type = 'number')
      table$addColumnInfo(name = 'odds', 	title = gettextf("Ratio %1$s", "<sup>H\u208B</sup>&frasl;<sub>H\u208A</sub>"), type = 'number')
    }
    
    table$addColumnInfo(name = 'mode', 		title = gettext("Mode"), type = 'number')
    table$addColumnInfo(name = 'bound',		title = gettextf("%1$s%% Upper bound", round(options[["confidence"]] * 100, 2)), type = 'number')
    table$addColumnInfo(name = 'precision', title = gettext("Precision"), type = 'number')
    
    parentContainer[["priorAndPosteriorStatistics"]] <- table
    
    names <- base::switch(stage,
                          "planning" = c(gettext("Prior"), gettext("Expected posterior"), gettext("Expected shift")),
                          "evaluation" = c(gettext("Prior"), gettext("Posterior"), gettext("Shift")))
    
    if(stage == "planning"){
      
      if(!ready || parentContainer$getError()) {
        row <- data.frame(v = names)
        table$addRows(row)
        return()
      }
      
    } else if(stage == "evaluation"){
      
      if(((options[["auditResult"]] == "" || options[["recordNumberVariable"]] == "") && !options[["useSumStats"]]) ||
         (options[["useSumStats"]] && options[["nSumStats"]] == 0) ||
         (parentOptions[["materiality"]] == 0 && options[["performanceMateriality"]]) ||
         parentContainer$getError()) {
        
        row <- data.frame(v = names)
        table$addRows(row)
        return()
      }
    }
    
    if(options[["performanceMateriality"]])
      table$addFootnote(message = gettextf("%1$s: The population misstatement is lower than materiality (%2$s %3$s). %4$s: The population misstatement is equal to, or higher than, materiality (%5$s %6$s).",
                                           "H\u208B",
                                           "\u03B8 <",
                                           round(parentState[["materiality"]], 3),
                                           "H\u208A",
                                           "\u03B8 \u2265",
                                           round(parentState[["materiality"]], 3)))
    
    N 						<- parentState[["N"]]
	prior 					<- parentState[["prior"]]

	if(stage == "planning"){
      
      likelihood 			<- parentState[["likelihood"]]
      n 					<- parentState[["sampleSize"]]
      k 					<- parentState[["expectedSampleError"]]
      posterior 			<- parentState[["expectedPosterior"]]
      
    } else if(stage == "evaluation"){
      
      likelihood 			<- parentState[["method"]]
      n 					<- parentState[["n"]]
      k 					<- parentState[["k"]]
      posterior 			<- parentState[["posterior"]]
      
    }

	alphaParameterPrior 	<- prior[["description"]]$alpha
	betaParameterPrior 		<- prior[["description"]]$beta
    modePrior 				<- prior[["statistics"]]$mode
	boundPrior 				<- prior[["statistics"]]$ub
    alphaParameterPosterior <- posterior[["description"]]$alpha
    betaParameterPosterior 	<- posterior[["description"]]$beta
    modePost 				<- posterior[["statistics"]]$mode
	boundPost 				<- posterior[["statistics"]]$ub
    
    if(likelihood == "poisson"){
      formPrior 	<- paste0("gamma(\u03B1 = ", round(alphaParameterPrior, 3), ", \u03B2 = ", round(betaParameterPrior, 3), ")")
      formPost 	<- paste0("gamma(\u03B1 = ", round(alphaParameterPosterior, 3), ", \u03B2 = ", round(betaParameterPosterior, 3), ")")
    } else if(likelihood == "binomial"){
      formPrior 	<- paste0("beta(\u03B1 = ", round(alphaParameterPrior, 3), ", \u03B2 = ", round(betaParameterPrior, 3), ")")
      formPost 	<- paste0("beta(\u03B1 = ", round(alphaParameterPosterior, 3), ", \u03B2 = ", round(betaParameterPosterior, 3), ")") 
    } else if(likelihood == "hypergeometric"){
      formPrior 	<- paste0("beta-binomial(N = ", N - n + k, ", \u03B1 = ", round(alphaParameterPrior, 3), ", \u03B2 = ", round(betaParameterPrior, 3), ")")
      formPost 	<- paste0("beta-binomial(N = ", N - n + k, ", \u03B1 = ", round(alphaParameterPosterior, 3), ", \u03B2 = ", round(betaParameterPosterior, 3), ")")
    }
    
    shiftInMode  	<- modePost - modePrior
    shiftInBound  	<- boundPost - boundPrior
    precisionPrior  <- prior[["statistics"]]$precision
    precisionPost   <- posterior[["statistics"]]$precision
    
    rows <- data.frame(v = names,
                       form = c(formPrior, formPost, ""),
                       mode = c(modePrior, modePost, shiftInMode),
                       bound = c(boundPrior, boundPost, shiftInBound),
                       precision = c(precisionPrior, precisionPost, NA))
    
    if(options[["performanceMateriality"]]){
	  priorHypotheses 	<- prior[["hypotheses"]]
      postHypotheses 	<- posterior[["hypotheses"]]
	  bf 				<- base::switch(stage, "planning" = postHypotheses[["expectedBf"]], "evaluation" = postHypotheses[["bf"]])
      rows <- cbind(rows, hMin = c(priorHypotheses[["pHmin"]], postHypotheses[["pHmin"]], postHypotheses[["pHmin"]] / priorHypotheses[["pHmin"]]),
                    hPlus = c(priorHypotheses[["pHplus"]], postHypotheses[["pHplus"]], postHypotheses[["pHplus"]] / priorHypotheses[["pHplus"]]),
                    odds = c(priorHypotheses[["oddsHmin"]], postHypotheses[["oddsHmin"]], bf))
    }
    
    table$addRows(rows)
  }
}

################################################################################
################## End functions ###############################################
################################################################################