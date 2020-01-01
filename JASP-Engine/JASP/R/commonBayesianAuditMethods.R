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
################## Functions for Bayes factors #################################
################################################################################

.expectedBF <- function(planningState){

  # For calculation of the Bayes factor, the are below materiality is 
  # regarded to be H1 and the area equal to and above materiality is regarded
  # to be H0.

  if(planningState[["likelihood"]] == "poisson"){

    priorH1 <- diff(pgamma(c(0, planningState[["materiality"]]), 
                           shape = planningState[["prior"]]$aPrior, 
                           rate = planningState[["prior"]]$bPrior))

    priorH0 <- diff(pgamma(c(planningState[["materiality"]], 1), 
                           shape = planningState[["prior"]]$aPrior, 
                           rate = planningState[["prior"]]$bPrior))
    
    priorOdds <- priorH1 / priorH0

    postH1 <- diff(pgamma(c(0, planningState[["materiality"]]), 
                          shape = planningState[["prior"]]$aPrior + 
                                  planningState[["expectedSampleError"]], 
                          rate = planningState[["prior"]]$bPrior + 
                                 planningState[["sampleSize"]]))

    postH0 <- diff(pgamma(c(planningState[["materiality"]], 1), 
                          shape = planningState[["prior"]]$aPrior + 
                                  planningState[["expectedSampleError"]], 
                          rate = planningState[["prior"]]$bPrior + 
                                 planningState[["sampleSize"]]))

    postOdds <- postH1 / postH0

  } else if(planningState[["likelihood"]] == "binomial"){

    priorH1 <- diff(pbeta(c(0, planningState[["materiality"]]), 
                          shape1 = planningState[["prior"]]$aPrior, 
                          shape2 = planningState[["prior"]]$bPrior))

    priorH0 <- diff(pbeta(c(planningState[["materiality"]], 1), 
                          shape1 = planningState[["prior"]]$aPrior, 
                          shape2 = planningState[["prior"]]$bPrior))

    priorOdds <- priorH1 / priorH0

    postH1 <- diff(pbeta(c(0, planningState[["materiality"]]), 
                        shape1 = planningState[["prior"]]$aPrior + 
                                 planningState[["expectedSampleError"]], 
                        shape2 = planningState[["prior"]]$bPrior + 
                                 planningState[["sampleSize"]] - 
                                 planningState[["expectedSampleError"]]))

    postH0 <- diff(pbeta(c(planningState[["materiality"]], 1), 
                         shape1 = planningState[["prior"]]$aPrior + 
                                  planningState[["expectedSampleError"]], 
                         shape2 = planningState[["prior"]]$bPrior + 
                                  planningState[["sampleSize"]] - 
                                  planningState[["expectedSampleError"]]))
                  

    postOdds <- postH1 / postH0
                

  } else if(planningState[["likelihood"]] == "hypergeometric"){

    tolerableE <- 0:floor(planningState[["materiality"]] * 
                          planningState[["N"]])

    intolerableE <- (max(tolerableE) + 1):(planningState[["N"]]  - 
                                           planningState[["sampleSize"]] + 
                                           planningState[["expectedSampleError"]])

    priorH1 <- sum(jfa:::.dBetaBinom(x = tolerableE, 
                                        N = planningState[["N"]] - 
                                            planningState[["sampleSize"]] + 
                                            planningState[["expectedSampleError"]], 
                                        shape1 = planningState[["prior"]]$aPrior, 
                                        shape2 = planningState[["prior"]]$bPrior))

    priorH0 <- sum(jfa:::.dBetaBinom(x = intolerableE, 
                                        N = planningState[["N"]] - 
                                            planningState[["sampleSize"]] + 
                                            planningState[["expectedSampleError"]], 
                                        shape1 = planningState[["prior"]]$aPrior, 
                                        shape2 = planningState[["prior"]]$bPrior))

    priorOdds <- priorH1 / priorH0

    postH1 <- sum(jfa:::.dBetaBinom(x = tolerableE, 
                                      N = planningState[["N"]] - 
                                          planningState[["sampleSize"]] + 
                                          planningState[["expectedSampleError"]], 
                                      shape1 = planningState[["prior"]]$aPrior + 
                                                planningState[["expectedSampleError"]], 
                                      shape2 = planningState[["prior"]]$bPrior + 
                                                planningState[["sampleSize"]] - 
                                                planningState[["expectedSampleError"]]))

    postH0 <- sum(jfa:::.dBetaBinom(x = intolerableE, 
                                      N = planningState[["N"]] - 
                                          planningState[["sampleSize"]] + 
                                          planningState[["expectedSampleError"]], 
                                      shape1 = planningState[["prior"]]$aPrior + 
                                                planningState[["expectedSampleError"]], 
                                      shape2 = planningState[["prior"]]$bPrior + 
                                                planningState[["sampleSize"]] - 
                                                planningState[["expectedSampleError"]]))

    postOdds <- postH1 / postH0

  }

  expectedBF <- round(postOdds / priorOdds, 2)

  if(expectedBF == "NaN") # Bayes factor for gamma(1, 0) distribution is undefined.
    expectedBF <- Inf

  result <- list(expectedBF = expectedBF, 
                 priorOdds = priorOdds, 
                 priorH1 = priorH1, 
                 priorH0 = priorH0,
                 postH1 = postH1,
                 postH0 = postH0,
                 postOdds = postOdds)

  return(result)
}

################################################################################
################## Functions for the planning ##################################
################################################################################

.auditImplicitSampleTable <- function(options, 
                                      planningState, 
                                      planningContainer, 
                                      jaspResults,
                                      ready, 
                                      positionInContainer){

  if(!options[["implicitSampleTable"]]) 
    return()

  .updateTabNumber(jaspResults)

  if(is.null(planningContainer[["sampletable"]])){

    tableTitle <- paste0("<b>Table ", 
                        jaspResults[["tabNumber"]]$object, 
                        ".</b> Implicit Sample")
    
    sampletable <- createJaspTable(tableTitle)
    sampletable$position <- positionInContainer

    sampletable$dependOn(options = c("implicitSampleTable", 
                                    "planningModel",
                                    "priorStatistics"))

    sampletable$addColumnInfo(name = 'implicitn', 
                              title = "Implicit sample size", 
                              type = 'string')
    sampletable$addColumnInfo(name = 'implicitk', 
                              title = "Implicit errors", 
                              type = 'string')

    message <- paste0("Sample sizes shown are implicit sample sizes derived from the ARM risk assessments: IR = <b>", 
                      options[["IR"]], 
                      "</b> and CR = <b>", 
                      options[["CR"]], 
                      "</b>.")

    sampletable$addFootnote(message = message, symbol="<i>Note.</i>")

    planningContainer[["sampletable"]] <- sampletable

    if(!ready || planningContainer$getError()) 
      return()

    row <- data.frame(implicitn = planningState[["prior"]]$nPrior, 
                      implicitk = planningState[["prior"]]$kPrior)

    sampletable$addRows(row)
  }
}

.auditPriorAndPosterStatisticsTable <- function(options, 
                                                planningState, 
                                                planningContainer, 
                                                jaspResults,
                                                ready, 
                                                positionInContainer){

  if(!options[["priorStatistics"]]) 
    return()
    
  .updateTabNumber(jaspResults)

  if(is.null(planningContainer[["priorStatistics"]])){

    tableTitle <- paste0("<b>Table ", 
                        jaspResults[["tabNumber"]]$object, 
                        ".</b> Prior and Posterior Descriptive Statistics")
    
    priorStatisticsTable <- createJaspTable(tableTitle)
    priorStatisticsTable$position <- positionInContainer

    priorStatisticsTable$dependOn(options = c("priorStatistics", 
                                              "planningModel",
                                              "implicitSampleTable"))

    priorStatisticsTable$addColumnInfo(name = 'v', 
                                    title = "", 
                                    type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'form', 
                                      title = "Functional form", 
                                      type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'priorH1', 
                                      title = "p(H\u208B)", 
                                      type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'priorH0', 
                                      title = "p(H\u208A)", 
                                      type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'priorOdds', 
                                      title = "Odds <sup>p(H\u208B)</sup>&frasl;<sub>p(H\u208A)</sub>", 
                                      type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'priorBound', 
                                      title = paste0(options[["confidence"]]*100,"% Credible bound") , 
                                      type = 'string')

    priorStatisticsTable$addFootnote(message = "p(H\u208B): The probability that 
                                                the misstatement is lower than materiality. 
                                                p(H\u208A): The probability that the misstatement 
                                                is equal to, or higher than, materiality.
                                                The odds are to be interpreted in favor of 
                                                tolerable misstatement.", 
                                    symbol="<i>Note.</i>")

    planningContainer[["priorStatistics"]] <- priorStatisticsTable

    if(!ready || planningContainer$getError()) {

        row <- data.frame(v = c("Prior", "Expected posterior", "Shift"))
        priorStatisticsTable$addRows(row)
        return()

    }

    if(planningState[["likelihood"]] == "poisson"){

      priorBound <- round(qgamma(p = options[["confidence"]], 
                                  shape = planningState[["prior"]]$aPrior, 
                                  rate = planningState[["prior"]]$bPrior), 6)

      postBound <- round(qgamma(p = options[["confidence"]], 
                                shape = planningState[["prior"]]$aPrior + 
                                        planningState[["expectedSampleError"]], 
                                rate = planningState[["prior"]]$bPrior + 
                                        planningState[["sampleSize"]]), 6)

      priorForm <- paste0("Gamma(\u03B1 = ", 
                          planningState[["prior"]]$aPrior,
                          ", \u03B2 = ",
                          planningState[["prior"]]$bPrior,
                          ")")

      posteriorForm <- paste0("Gamma(\u03B1 = ", 
                              planningState[["prior"]]$aPrior + 
                              planningState[["expectedSampleError"]],
                              ", \u03B2 = ",
                              planningState[["prior"]]$bPrior +
                              planningState[["sampleSize"]],
                              ")")

    } else if(planningState[["likelihood"]] == "binomial"){

      priorBound <- round(qbeta(p = options[["confidence"]], 
                                shape1 = planningState[["prior"]]$aPrior, 
                                shape2 = planningState[["prior"]]$bPrior), 6)

      postBound <- round(qbeta(p = options[["confidence"]], 
                                shape1 = planningState[["prior"]]$aPrior + 
                                        planningState[["expectedSampleError"]], 
                                shape2 = planningState[["prior"]]$bPrior +
                                        planningState[["sampleSize"]] - 
                                        planningState[["expectedSampleError"]]), 6)

      priorForm <- paste0("Beta(\u03B1 = ", 
                          planningState[["prior"]]$aPrior,
                          ", \u03B2 = ",
                          planningState[["prior"]]$bPrior,
                          ")")

      posteriorForm <- paste0("Beta(\u03B1 = ", 
                              planningState[["prior"]]$aPrior + 
                              planningState[["expectedSampleError"]],
                              ", \u03B2 = ",
                              planningState[["prior"]]$bPrior +
                              planningState[["sampleSize"]] -
                              planningState[["expectedSampleError"]],
                              ")")

    } else if(planningState[["likelihood"]] == "hypergeometric"){

      priorBound <- round(jfa:::.qBetaBinom(p = options[["confidence"]], 
                                  N = planningState[["N"]] - 
                                      planningState[["sampleSize"]] + 
                                      planningState[["expectedSampleError"]], 
                                  shape1 = planningState[["prior"]]$aPrior, 
                                  shape2 = planningState[["prior"]]$bPrior) / 
                      planningState[["N"]], 6)

      postBound <- round(jfa:::.qBetaBinom(p = options[["confidence"]], 
                                      N = planningState[["N"]] - 
                                        planningState[["sampleSize"]] + 
                                        planningState[["expectedSampleError"]], 
                                      shape1 = planningState[["prior"]]$aPrior + 
                                              planningState[["expectedSampleError"]], 
                                      shape2 = planningState[["prior"]]$bPrior +
                                              planningState[["sampleSize"]] - 
                                              planningState[["expectedSampleError"]]) / 
                          planningState[["N"]], 6)

      priorForm <- paste0("Beta-binomial(N = ",
                          planningState[["N"]] - 
                          planningState[["sampleSize"]] + 
                          planningState[["expectedSampleError"]],
                          ", \u03B1 = ", 
                          planningState[["prior"]]$aPrior,
                          ", \u03B2 = ",
                          planningState[["prior"]]$bPrior,
                          ")")

      posteriorForm <- paste0("Beta-binomial(N = ", 
                              planningState[["N"]] - 
                              planningState[["sampleSize"]] + 
                              planningState[["expectedSampleError"]],
                              ", \u03B1 = ",
                              planningState[["prior"]]$aPrior + 
                              planningState[["expectedSampleError"]],
                              ", \u03B2 = ",
                              planningState[["prior"]]$bPrior +
                              planningState[["sampleSize"]] -
                              planningState[["expectedSampleError"]],
                              ")")

    }
    
    if(planningState[["likelihood"]] != "hypergeometric"){

      priorBound  <- paste0(priorBound * 100, "%")
      postBound   <- paste0(postBound * 100, "%")

    } else {

      priorBound  <- ceiling(priorBound * planningState[["N"]])
      postBound   <- ceiling(postBound * planningState[["N"]])

    }
      
    BFresult <- .expectedBF(planningState)

    priorOdds <- round(BFresult[["priorOdds"]], 2)
    priorH1 <- round(BFresult[["priorH1"]], 2)
    priorH0 <- round(BFresult[["priorH0"]], 2)
    postOdds <- round(BFresult[["postOdds"]], 2)
    postH1 <- round(BFresult[["postH1"]], 2)
    postH0 <- round(BFresult[["postH0"]], 2)
    shiftH1 <- round(postH1 / priorH1, 2)
    shiftH0 <- round(postH0 / priorH0, 2)
    shiftOdds <- round(BFresult[["postOdds"]] / BFresult[["priorOdds"]], 2)

    rows <- data.frame(v = c("Prior", "Expected posterior", "Shift"),
                      form = c(priorForm, posteriorForm, ""),
                      priorH1 = c(priorH1, postH1, shiftH1),
                      priorH0 = c(priorH0, postH0, shiftH0),
                      priorOdds = c(priorOdds, postOdds, shiftOdds),
                      priorBound = c(priorBound, postBound, ""))
    
    priorStatisticsTable$addRows(rows)
  }
}

.auditPlanningPlotPrior <- function(options, 
                                    planningOptions, 
                                    planningState, 
                                    planningContainer, 
                                    jaspResults,
                                    ready, 
                                    positionInContainer){

  if(!options[["priorPlot"]]) 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(planningContainer[["priorPlot"]])){

    priorPlot <- createJaspPlot(plot = NULL, 
                                title = "Implied Prior from Risk Assessments", 
                                width = 600, 
                                height = 400)
    priorPlot$position <- positionInContainer

    priorPlot$dependOn(options = c("priorPlotLimit", 
                                   "priorPlot", 
                                   "priorPlotAdditionalInfo", 
                                   "priorPlotExpectedPosterior", 
                                   "planningModel"))

    planningContainer[["priorPlot"]] <- priorPlot

    if(!ready || planningContainer$getError()) 
      return()

    xseq <- seq(0, options[["priorPlotLimit"]], 0.001)

    if(planningState[["likelihood"]] == "binomial"){

      priorData <- data.frame(x = xseq, 
                              y = dbeta(x = xseq, 
                                        shape1 = planningState[["prior"]]$aPrior, 
                                        shape2 = planningState[["prior"]]$bPrior),
                              type = rep("Prior", length(xseq)))

      postData <- data.frame(x = xseq, 
                             y = dbeta(x = xseq, 
                                       shape1 = planningState[["prior"]]$aPrior + 
                                                planningState[["expectedSampleError"]], 
                                       shape2 = planningState[["prior"]]$bPrior + 
                                                planningState[["sampleSize"]] - 
                                                planningState[["expectedSampleError"]]),
                            type = rep("Expected posterior", length(xseq)))

      pdata <- data.frame(x = planningState[["materiality"]], 
                          y = dbeta(planningState[["materiality"]], 
                                    shape1 = planningState[["prior"]]$aPrior, 
                                    shape2 = planningState[["prior"]]$bPrior))

      pdata2 <- data.frame(x = planningOptions[["expectedErrors"]], 
                           y = dbeta(planningOptions[["expectedErrors"]], 
                                     shape1 = planningState[["prior"]]$aPrior, 
                                     shape2 = planningState[["prior"]]$bPrior))

      priorBound <- qbeta(planningState[["confidence"]], 
                          shape1 = planningState[["prior"]]$aPrior, 
                          shape2 = planningState[["prior"]]$bPrior)

      posteriorBound <- qbeta(planningState[["confidence"]], 
                              shape1 = planningState[["prior"]]$aPrior + 
                                       planningState[["expectedSampleError"]], 
                              shape2 = planningState[["prior"]]$bPrior + 
                                       planningState[["sampleSize"]] - 
                                       planningState[["expectedSampleError"]])

    } else if(planningState[["likelihood"]] == "poisson"){

      priorData <- data.frame(x = xseq, 
                              y = dgamma(x = xseq, 
                                         shape = planningState[["prior"]]$aPrior, 
                                         rate = planningState[["prior"]]$bPrior),
                              type = rep("Prior", length(xseq)))

      postData <- data.frame(x = xseq, 
                             y = dgamma(x = xseq, 
                                        shape = planningState[["prior"]]$aPrior + 
                                                planningState[["expectedSampleError"]], 
                                        rate = planningState[["prior"]]$bPrior + 
                                               planningState[["sampleSize"]]),
                             type = rep("Expected posterior", length(xseq)))

      pdata <- data.frame(x = planningState[["materiality"]], 
                          y = dgamma(planningState[["materiality"]], 
                          shape = planningState[["prior"]]$aPrior, 
                          rate = planningState[["prior"]]$bPrior))

      pdata2 <- data.frame(x = planningOptions[["expectedErrors"]], 
                           y = dgamma(planningOptions[["expectedErrors"]], 
                           shape = planningState[["prior"]]$aPrior, 
                           rate = planningState[["prior"]]$bPrior))

      priorBound <- qgamma(planningState[["confidence"]], 
                           shape = planningState[["prior"]]$aPrior, 
                           rate = planningState[["prior"]]$bPrior)

      posteriorBound <- qgamma(planningState[["confidence"]], 
                               shape = planningState[["prior"]]$aPrior + 
                                       planningState[["expectedSampleError"]], 
                               rate = planningState[["prior"]]$bPrior + 
                                      planningState[["sampleSize"]])
    }

    if(planningState[["likelihood"]] == "hypergeometric"){

      xseq <- 0:ceiling(options[["priorPlotLimit"]] * planningState[["N"]])

      priorData <- data.frame(x = xseq, 
                              y = jfa:::.dBetaBinom(x = xseq, 
                                                    N = planningState[["N"]] - 
                                                        planningState[["sampleSize"]] + 
                                                        planningState[["expectedSampleError"]], 
                                                    shape1 = planningState[["prior"]]$aPrior, 
                                                    shape2 = planningState[["prior"]]$bPrior),
                              type = rep("Prior", length(xseq)))

      postData <- data.frame(x = xseq, 
                             y = jfa:::.dBetaBinom(x = xseq, 
                             N = planningState[["N"]] - 
                                 planningState[["sampleSize"]] + 
                                 planningState[["expectedSampleError"]], 
                             shape1 = planningState[["prior"]]$aPrior + 
                                      planningState[["expectedSampleError"]], 
                             shape2 = planningState[["prior"]]$bPrior + 
                                      planningState[["sampleSize"]] - 
                                      planningState[["expectedSampleError"]]),
                             type = rep("Expected posterior", length(xseq)))

      pdata <- data.frame(x = planningState[["materiality"]] * planningState[["N"]], 
                          y = jfa:::.dBetaBinom(ceiling(
                                                  planningState[["materiality"]] * 
                                                  planningState[["N"]]
                                                  ),
                                                N = planningState[["N"]] - 
                                                    planningState[["sampleSize"]] + 
                                                    planningState[["expectedSampleError"]], 
                                                shape1 = planningState[["prior"]]$aPrior, 
                                                shape2 = planningState[["prior"]]$bPrior))

      populationK <- base::switch(options[["expectedErrors"]], 
                                  "expectedRelative" = options[["expectedPercentage"]] * 
                                                       planningState[["N"]], 
                                  "expectedAbsolute" = round(
                                                        options[["expectedNumber"]] / 
                                                        planningOptions[["populationValue"]] * 
                                                        planningState[["N"]], 
                                                        2)) 

      pdata2 <- data.frame(x = populationK, 
                          y = jfa:::.dBetaBinom(populationK, 
                                                N = planningState[["N"]]  - 
                                                    planningState[["sampleSize"]] + 
                                                    planningState[["expectedSampleError"]], 
                                                shape1 = planningState[["prior"]]$aPrior, 
                                                shape2 = planningState[["prior"]]$bPrior))

      priorBound <- jfa:::.qBetaBinom(p = options[["confidence"]], 
                                      N = planningState[["N"]] - 
                                          planningState[["sampleSize"]] + 
                                          planningState[["expectedSampleError"]], 
                                      shape1 = planningState[["prior"]]$aPrior, 
                                      shape2 = planningState[["prior"]]$bPrior) 

      posteriorBound <- jfa:::.qBetaBinom(p = options[["confidence"]], 
                                          N = planningState[["N"]] - 
                                              planningState[["sampleSize"]] + 
                                              planningState[["expectedSampleError"]], 
                                          shape1 = planningState[["prior"]]$aPrior + 
                                                   planningState[["expectedSampleError"]], 
                                          shape2 = planningState[["prior"]]$bPrior + 
                                                   planningState[["sampleSize"]] - 
                                                   planningState[["expectedSampleError"]])
    }

    pdata3 <- data.frame(x = 0, 
                         y = 0, 
                         l = "1")

    if(options[["priorPlotExpectedPosterior"]]){

      plotData <- rbind(priorData, postData)
      plotData$type <- factor(x = plotData$type, 
                              levels = levels(plotData$type)[c(1,2)])
    
    } else {

      plotData <- priorData
    
    }

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
    yBreaks <- c(0, 1.2 * max(plotData$y))

    if(!options[["priorPlotExpectedPosterior"]]){
      scaleValues <- c("dashed")
      guide <- FALSE
    } else {
      scaleValues <- c("dashed", "dotted")
      guide <- ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1)
    }

    p <- ggplot2::ggplot(data = plotData, 
                         mapping = ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y, linetype = type), 
                             lwd = 1) +
          ggplot2::scale_linetype_manual(values = scaleValues, 
                                         guide = guide) +
          ggplot2::scale_y_continuous(name = "Density", 
                                      breaks = yBreaks, 
                                      labels = c("", ""), 
                                      limits = range(yBreaks))

    if(planningState[["likelihood"]] == "hypergeometric"){

      p <- p + ggplot2::scale_x_continuous(name = "Population errors", 
                                           breaks = xBreaks, 
                                           limits = range(xBreaks), 
                                           labels = xBreaks)

    } else {

      p <- p + ggplot2::scale_x_continuous(name = "Probability of misstatement", 
                                           breaks = xBreaks, 
                                           limits = range(xBreaks), 
                                           labels = paste0(xBreaks * 100, "%"))

    }
    
    if(options[["priorPlotAdditionalInfo"]]){

      p <- p + ggplot2::geom_point(data = pdata3, 
                                   mapping = ggplot2::aes(x = x, y = y, shape = l), 
                                   size = 0, 
                                   color = rgb(0, 1, 0.5, 0))

      if(options[["priorPlotExpectedPosterior"]]){

        p <- p + ggplot2::scale_shape_manual(name = "", 
                                             values = 21, 
                                             labels = paste0(
                                                        options[["confidence"]]*100, 
                                                        "% Prior \ncredible region"
                                                      ))
      } else {

        p <- p + ggplot2::scale_shape_manual(name = "", 
                                             values = 21, 
                                             labels = paste0(
                                                        options[["confidence"]]*100, 
                                                        "% Prior credible region"
                                                      ))
      
      }

      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(
                                                  override.aes = list(size = 15, 
                                                                      shape = 22, 
                                                                      fill = rgb(0, 1, 0.5, .7), 
                                                                      stroke = 2, 
                                                                      color = "black")))

      if(planningState[["likelihood"]] == "binomial"){

        p <- p + ggplot2::stat_function(fun = dbeta, 
                                        args = list(
                                                shape1 = planningState[["prior"]]$aPrior, 
                                                shape2 = planningState[["prior"]]$bPrior
                                                ),
                                        xlim = c(0, priorBound),
                                        geom = "area", 
                                        fill = rgb(0, 1, 0.5, .7))

      } else if(planningState[["likelihood"]] == "poisson"){

        p <- p + ggplot2::stat_function(fun = dgamma, 
                                        args = list(
                                                shape = planningState[["prior"]]$aPrior, 
                                                rate = planningState[["prior"]]$bPrior
                                                ),
                                        xlim = c(0, priorBound),
                                        geom = "area", 
                                        fill = rgb(0, 1, 0.5, .7))

      } else if(planningState[["likelihood"]] == "hypergeometric"){

        xseq <- xseq[1:(priorBound + 1)]
        barData <- data.frame(x = xseq, 
                              y = jfa:::.dBetaBinom(x = xseq, 
                                                    N = planningState[["N"]] - 
                                                        planningState[["sampleSize"]] + 
                                                        planningState[["expectedSampleError"]], 
                                                    shape1 = planningState[["prior"]]$aPrior, 
                                                    shape2 = planningState[["prior"]]$bPrior))

        p <- p + ggplot2::geom_bar(data = barData, 
                                   stat="identity", 
                                   fill = rgb(0, 1, 0.5, .7))
      
      }

      p <- p + ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y), 
                                   data = pdata, 
                                   size = 3, 
                                   shape = 21, 
                                   stroke = 2, 
                                   color = "black", 
                                   fill = "red") +
                ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y), 
                                    data = pdata2, 
                                    size = 3, 
                                    shape = 21, 
                                    stroke = 2, 
                                    color = "black", 
                                    fill = "grey")
    }

    if(options[["priorPlotAdditionalInfo"]] && 
        options[["priorPlotExpectedPosterior"]]){

      p <- p + ggplot2::geom_segment(x = 0, 
                                     xend = posteriorBound, 
                                     y = max(plotData$y) * 1.1, 
                                     yend = max(plotData$y) * 1.1, 
                                     linetype = 1, 
                                     size = 1, 
                                     arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"), 
                                                            angle = 90)) +
                ggplot2::geom_segment(x = posteriorBound, 
                                      xend = 0, y = max(plotData$y) * 1.1, 
                                      yend = max(plotData$y) * 1.1, 
                                      linetype = 1, 
                                      size = 1, 
                                      arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"), 
                                                             angle = 90))
    }

    myTheme <- ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                              axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0)),
                              legend.key.size = ggplot2::unit(4, "line"))
  
    p <- JASPgraphs::themeJasp(p, 
                               legend.position = "top") + myTheme

    priorPlot$plotObject <- p
  }

  if(options[["explanatoryText"]]){

    distribution <- base::switch(planningOptions[["likelihood"]], 
                                 "poisson" = "gamma", 
                                 "binomial" = "beta", 
                                 "hypergeometric" = "beta-binomial")

    priorPlotText <- createJaspHtml(paste0("<b>Figure ", 
                                           jaspResults[["figNumber"]]$object,
                                           ".</b> The prior probability distribution <b>(", 
                                           distribution,
                                           ")</b> on the misstatement in the population. The prior parameters 
                                           <i>\u03B1 = ", 
                                           planningState[["prior"]]$aPrior, 
                                           ", \u03B2 = ", 
                                           planningState[["prior"]]$bPrior, 
                                           "</i> are derived from the assessments of the inherent and control risk, along with the expected errors.",  
                                           ifelse(options[["priorPlotAdditionalInfo"]], 
                                           yes = "
                                           The expected errors (grey dot) receive the highest probability. The red dot represents the materiality.", 
                                           no = ""),
                                           ifelse(options[["priorPlotExpectedPosterior"]], 
                                           yes = " 
                                           The expected posterior has its upper confidence bound below materiality. ", 
                                           no = "")), "p")

    priorPlotText$position <- positionInContainer + 1
    priorPlotText$dependOn(optionsFromObject = planningContainer[["priorPlot"]])
    planningContainer[["priorPlotText"]] <- priorPlotText
  }
}

################################################################################
################## Functions for the Evaluation ################################
################################################################################

.auditEvaluationPriorAndPosterior <- function(options,
                                              planningOptions,
                                              evaluationState,
                                              evaluationContainer,
                                              jaspResults,
                                              positionInContainer){



}

################################################################################
################## End functions ###############################################
################################################################################


# The following function will be deprecated
.priorAndPosteriorBayesianAttributes <- function(options, evaluationResult, jaspResults){

  if(options[["estimator"]] == "betaBound"){

    xseq <- seq(0, options[["priorAndPosteriorPlotLimit"]], 0.001)
    d <- data.frame(
        x = rep(xseq, 2),
        y = c(dbeta(x = xseq, shape1 = evaluationResult[["priorA"]], shape2 = evaluationResult[["priorB"]]), dbeta(x = xseq, shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]])),
        type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
    )
    # Reorder factor levels to display in legend
    d$type = factor(d$type,levels(d$type)[c(2,1)])

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
    xLim <- range(xBreaks)
    yBreaks <- c(0, 1.2*max(d$y))
    yLim <- range(yBreaks)

    pointdata <- data.frame(x = jaspResults[["materiality"]]$object, y = dbeta(jaspResults[["materiality"]]$object, evaluationResult[["posteriorA"]], evaluationResult[["posteriorB"]]))

    p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
        ggplot2::scale_linetype_manual(values=c("dashed", "solid"), guide = ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1))

    p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))

    if(options[["priorAndPosteriorPlotAdditionalInfo"]]){
      pdata <- data.frame(x = 0, y = 0, l = "1")
      p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 0.25, 1, 0))
      p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior \ncredible region"))
      p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")), order = 2)

      if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
        p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]]), xlim = c(0, evaluationResult[["bound"]]),
                                        geom = "area", fill = rgb(0, 0.25, 1, .5))
      } else {
        p <- p + ggplot2::stat_function(fun = dbeta, args = list(shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]]), xlim = evaluationResult[["interval"]],
                                        geom = "area", fill = rgb(0, 0.25, 1, .5))
      }
    }

    p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

    thm <- ggplot2::theme(
  		axis.ticks.y = ggplot2::element_blank(),
  		axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
  	)
    p <- p + ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
    	       ggplot2::theme()

    p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm

  } else if(options[["estimator"]] == "betabinomialBound"){

      xseq <- seq(0, jaspResults[["N"]]$object, 1)[1:ceiling(options[["priorAndPosteriorPlotLimit"]] * jaspResults[["N"]]$object)]
      d <- data.frame(
          x = rep(xseq, 2),
          y = c(.dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object - evaluationResult[["n"]], shape1 = evaluationResult[["priorA"]], shape2 = evaluationResult[["priorB"]])[1:ceiling(options[["priorAndPosteriorPlotLimit"]] * jaspResults[["N"]]$object)],
                .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object - evaluationResult[["n"]], shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]])[1:ceiling(options[["priorAndPosteriorPlotLimit"]] * jaspResults[["N"]]$object)]),
          type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
      )

      xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
      xLim <- range(xBreaks)
      yBreaks <- c(0, 1.2*max(d$y))
      yLim <- range(yBreaks)

      pointdata <- data.frame(x = jaspResults[["materiality"]]$object * jaspResults[["N"]]$object, y = .dBetaBinom(ceiling(jaspResults[["materiality"]]$object * jaspResults[["N"]]$object),
                            N = jaspResults[["N"]]$object - evaluationResult[["n"]], shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]]))

      p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
          ggplot2::scale_linetype_manual(values=c("solid", "dashed"), guide = FALSE)

      p <- p + ggplot2::scale_x_continuous(name = "Population errors", breaks = xBreaks, limits = xLim, labels = xBreaks)

      if(options[["priorAndPosteriorPlotAdditionalInfo"]]){
          pdata <- data.frame(x = 0, y = 0, l = "1")
          p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 1, 0.5, 0))
          p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior credible region"))
          p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")))

          df <- data.frame(x = 0:jaspResults[["N"]]$object, y = .dBetaBinom(x = 0:jaspResults[["N"]]$object, N = jaspResults[["N"]]$object - evaluationResult[["n"]], shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]]))
          if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
            lim <- .qBetaBinom(p = options[["confidence"]], N = jaspResults[["N"]]$object - evaluationResult[["n"]], shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]])
            df <- df[1:lim, ]
            p <- p + ggplot2::geom_bar(data = df, stat="identity", fill = rgb(0, 0.25, 1, .5))
          } else {
            lim <- .qBetaBinom(p = c((1 - options[["confidence"]])/2, 1 - (1 - options[["confidence"]])/2), N = jaspResults[["N"]]$object - evaluationResult[["n"]], shape1 = evaluationResult[["posteriorA"]], shape2 = evaluationResult[["posteriorB"]])
            df <- df[lim[1]:lim[2], ]
            p <- p + ggplot2::geom_bar(data = df, stat="identity", fill = rgb(0, 0.25, 1, .5))
          }
      }

      p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

      thm <- ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
      )
      p <- p + ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
                ggplot2::theme()
      p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm
    }
  return(p)
}

# The following function will be deprecated
.BF <- function(options, planningResult, jaspResults){
  priorOdds         <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), planningResult[["priorA"]], planningResult[["priorB"]])) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), planningResult[["priorA"]], planningResult[["priorB"]]))
  posteriorOdds     <- diff(pbeta(c(0, jaspResults[["materiality"]]$object), planningResult[["posteriorA"]], planningResult[["posteriorB"]])) / diff(pbeta(c(jaspResults[["materiality"]]$object, 1), planningResult[["posteriorA"]], planningResult[["posteriorB"]]))
  BF                <- round(posteriorOdds / priorOdds, 2)
  return(BF)
}

# The following function will be deprecated
.BFsamples <- function(options, evaluationResult, jaspResults){
  set.seed(options[["seed"]])
  prior             <- rbeta(n = 1e5, shape1 = evaluationResult[["priorA"]], shape2 = evaluationResult[["priorB"]])
  posterior         <- evaluationResult[["mf"]] * rf(n = 1e5, df1 = evaluationResult[["df1"]], df2 = evaluationResult[["df2"]])
  densprior         <- density(prior)
  priorCDF          <- approxfun(densprior$x, densprior$y, yleft=0, yright=0)
  priorLeft         <- integrate(priorCDF, lower = 0, upper = jaspResults[["materiality"]]$object)$value
  priorRight        <- integrate(priorCDF, lower = jaspResults[["materiality"]]$object, upper = 1)$value
  priorOdds         <- priorLeft / priorRight
  densposterior     <- density(posterior)
  posteriorCDF      <- approxfun(densposterior$x, densposterior$y, yleft=0, yright=0)
  posteriorLeft     <- integrate(posteriorCDF, lower = 0, upper = jaspResults[["materiality"]]$object)$value
  posteriorRight    <- integrate(posteriorCDF, lower = jaspResults[["materiality"]]$object, upper = 1)$value
  posteriorOdds     <- posteriorLeft / posteriorRight
  BF                <- round(posteriorOdds / priorOdds, 2)
  return(BF)
}

# The following function will be deprecated
.priorAndPosteriorCoxAndSnell <- function(options, evaluationResult, jaspResults){

  xseq <- seq(0, options[["priorAndPosteriorPlotLimit"]], 0.001)
  d <- data.frame(
      x = rep(xseq, 2),
      y = c(dbeta(x = xseq, shape1 = evaluationResult[["priorA"]], shape2 = evaluationResult[["priorB"]]), .dCoxAndSnellF(x = xseq, df1 = evaluationResult[["df1"]], df2 = evaluationResult[["df2"]], multiplicationFactor = evaluationResult[["mf"]])),
      type = c(rep("Prior", length(xseq)), rep("Posterior", length(xseq)))
  )
  # Reorder factor levels to display in legend
  d$type = factor(d$type,levels(d$type)[c(2,1)])

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
  xLim <- range(xBreaks)
  yBreaks <- c(0, 1.2*max(d$y))
  yLim <- range(yBreaks)

  pointdata <- data.frame(x = jaspResults[["materiality"]]$object, y = .dCoxAndSnellF(x = jaspResults[["materiality"]]$object, df1 = evaluationResult[["df1"]], df2 = evaluationResult[["df2"]], multiplicationFactor = evaluationResult[["mf"]]))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = y, linetype = type), lwd = 1) +
      ggplot2::scale_linetype_manual(values=c("dashed", "solid"), guide = ggplot2::guide_legend(nrow = 1, byrow = FALSE, title = "", order = 1))

  p <- p + ggplot2::scale_x_continuous(name = "Error percentage", breaks = xBreaks, limits = xLim, labels = paste0(xBreaks * 100, "%"))

  if(options[["priorAndPosteriorPlotAdditionalInfo"]]){
    pdata <- data.frame(x = 0, y = 0, l = "1")
    p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = rgb(0, 0.25, 1, 0))
    p <- p + ggplot2::scale_shape_manual(name = "", values = 21, labels = paste0(options[["confidence"]]*100, "% Posterior \ncredible region"))
    p <- p + ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 15, shape = 22, fill = rgb(0, 0.25, 1, .5), stroke = 2, color = "black")), order = 2)

    if(options[["areaUnderPosterior"]]=="displayCredibleBound"){
      p <- p + ggplot2::stat_function(fun = .dCoxAndSnellF, args = list(df1 = evaluationResult[["df1"]], df2 = evaluationResult[["df2"]], multiplicationFactor = evaluationResult[["mf"]]), xlim = c(0, evaluationResult[["bound"]]),
                                      geom = "area", fill = rgb(0, 0.25, 1, .5))
    } else {
      p <- p + ggplot2::stat_function(fun = .dCoxAndSnellF, args = list(df1 = evaluationResult[["df1"]], df2 = evaluationResult[["df2"]], multiplicationFactor = evaluationResult[["mf"]]), xlim = evaluationResult[["interval"]],
                                      geom = "area", fill = rgb(0, 0.25, 1, .5))
    }
  }

  p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointdata, size = 3, shape = 21, stroke = 2, color = "black", fill = "red")

  thm <- ggplot2::theme(
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
  )
  p <- p + ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, labels = c("", ""), limits = yLim) +
            ggplot2::theme()

  p <- JASPgraphs::themeJasp(p, legend.position = "top") + thm
  return(p)
}

# The following function will be deprecated
.regressionBoundBayesian <- function(dataset, options, total_data_value, jaspResults){

  if(!is.null(jaspResults[["evaluationResult"]]))
    return(jaspResults[["evaluationResult"]]$object)

    ar                      <- 1 - options[["confidence"]]
    ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    alpha                   <- ar / ir / cr

    n                       <- 0
    M                       <- 0
    z                       <- 0
    bound                   <- "."
    interval              <- c(".", ".")
    mle                     <- 0
    mleTable                <- 0

    if(options[["auditResult"]] != "" && options[["sampleFilter"]] != "" && options[["monetaryVariable"]] != ""){
        sample                  <- dataset[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
        n                       <- nrow(sample)

        t                       <- sample[, .v(options[["monetaryVariable"]])] - sample[, .v(options[["auditResult"]])]
        z                       <- t / sample[, .v(options[["monetaryVariable"]])]
        zplus                   <- sort(subset(z, z > 0), decreasing = TRUE)
        M                       <- length(which(t != 0))

        B                       <- total_data_value
        N                       <- jaspResults[["N"]]$object
        b                       <- sample[, .v(options[["monetaryVariable"]])]
        w                       <- sample[, .v(options[["auditResult"]])]

        # Bayesian Linear Regression Using the BAS package
        colnames(sample)        <- c("bookValue", "auditValue")
        formula                 <- auditValue ~ bookValue

        regResult               <- BAS::bas.lm(formula, data=sample)
        coefReg                 <- coef(regResult)  
        beta                    <- coefReg$postmean[2]

        cred.interval           <- BAS:::confint.coef.bas(coefReg, level = options[["confidence"]])  
        betaMaxInterval         <- cred.interval[2, c(1,2)] 

        cred.bound              <- BAS:::confint.coef.bas(coefReg, level = options[["confidence"]] - (1 - options[["confidence"]])) 
        betaMax                 <- cred.bound[2, 2] 

        meanb                   <- mean(b)
        meanw                   <- mean(w)

        mle                     <- N * meanw + beta * (B - N * meanb)
        stand.dev               <- sd(w) * sqrt(1 - cor(b, w)^2) * ( N / sqrt(n)) * sqrt( (N-n) / (N-1) )
        lowerValue              <- mle - betaMax * stand.dev
        lowerValueInterval      <- c(mle + betaMaxInterval[1] * stand.dev, mle - betaMaxInterval[2] * stand.dev)

        if(lowerValue == 0){
          bound                 <- 0
          interval              <- c(0, 0)
        } else {
          bound                 <- (B - lowerValue) / B
          interval              <- (B - lowerValueInterval) / B
        }
        mleTable                <- (B - mle) / B    
    }

    resultList <- list()
    resultList[["n"]]           <- n
    resultList[["k"]]           <- M
    resultList[["z"]]           <- z
    resultList[["IR"]]          <- options[["IR"]]
    resultList[["CR"]]          <- options[["CR"]]
    resultList[["confidence"]]  <- options[["confidence"]]
    resultList[["bound"]]       <- bound
    resultList[["interval"]]    <- interval
    resultList[["alpha"]]       <- alpha
    resultList[["mle"]]         <- mle
    resultList[["mleTable"]]    <- mleTable

    jaspResults[["evaluationResult"]] <- createJaspState(resultList)
    jaspResults[["evaluationResult"]]$dependOn(options = c("IR", "CR", "confidence", "auditResult", "sampleFilter", "materiality", "estimator", "monetaryVariable", "performAudit"))
    return(jaspResults[["evaluationResult"]]$object)
}

# The following function will be deprecated
.priorAndPosteriorPlot <- function(options, evaluationResult, jaspResults, position, evaluationContainer){

  if(!is.null(evaluationContainer[["priorAndPosteriorPlot"]])) return()

  priorAndPosteriorPlot <- createJaspPlot(plot = NULL, title = "Prior and Posterior", width = 600, height = 400)
  priorAndPosteriorPlot$position <- position
  priorAndPosteriorPlot$dependOn(options = c("IR", "CR", "confidence", "priorAndPosteriorPlotLimit", "priorAndPosteriorPlot", "priorAndPosteriorPlotAdditionalInfo", "materialityPercentage", "auditResult",
                                              "expectedErrors", "expectedPercentage", "expectedNumber", "sampleFilter", "planningModel", "materialityValue", "displayCredibleInterval", "performAudit", "estimator"))

  evaluationContainer[["priorAndPosteriorPlot"]] <- priorAndPosteriorPlot

  if(!jaspResults[["runEvaluation"]]$object) return()

  if(options[["variableType"]] == "variableTypeCorrect"){
    p <- .priorAndPosteriorBayesianAttributes(options, evaluationResult, jaspResults)
  } else {
    p <- .priorAndPosteriorCoxAndSnell(options, evaluationResult, jaspResults)
  }

  priorAndPosteriorPlot$plotObject <- p

  if(options[["explanatoryText"]]){
    posteriorName <- ifelse(options[["variableType"]] == "variableTypeCorrect", 
                        yes = ifelse(options[["planningModel"]] == "beta", yes = "<b>(beta)</b>", no = "<b>(beta-binomial)</b>"),
                        no = "<b>(Cox and Snell)</b>")
    figure5 <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The prior and posterior probability distrubution ", posteriorName, " on the percentage of misstatement in the population. 
                                      The red dot represents the specified materiality. If the credible area under the distribution surpasses this point, the 
                                      estimate of the maximum misstatement exceeds the materiality."), "p")
    figure5$position <- position + 1
    figure5$dependOn(optionsFromObject = priorAndPosteriorPlot)
    evaluationContainer[["figure5"]] <- figure5
    jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  }
}
