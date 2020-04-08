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
################## Common functions the evidence ratio #########################
################################################################################

.auditExpectedEvidenceRatio <- function(planningState){

  # For calculation of the evidence ratio the area below materiality is 
  # regarded to be H1 and the area equal to and above materiality is regarded
  # to be H0.

  if(planningState[["likelihood"]] == "poisson"){

    priorH1 <- diff(pgamma(c(0, planningState[["materiality"]]), 
                           shape = planningState[["prior"]]$aPrior, 
                           rate = planningState[["prior"]]$bPrior))

    priorH0 <- diff(pgamma(c(planningState[["materiality"]], 1), 
                           shape = planningState[["prior"]]$aPrior, 
                           rate = planningState[["prior"]]$bPrior))
    
    priorEvidenceRatio <- priorH1 / priorH0

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

    posteriorEvidenceRatio <- postH1 / postH0

  } else if(planningState[["likelihood"]] == "binomial"){

    priorH1 <- diff(pbeta(c(0, planningState[["materiality"]]), 
                          shape1 = planningState[["prior"]]$aPrior, 
                          shape2 = planningState[["prior"]]$bPrior))

    priorH0 <- diff(pbeta(c(planningState[["materiality"]], 1), 
                          shape1 = planningState[["prior"]]$aPrior, 
                          shape2 = planningState[["prior"]]$bPrior))

    priorEvidenceRatio <- priorH1 / priorH0

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
                  

    posteriorEvidenceRatio <- postH1 / postH0
                
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

    priorEvidenceRatio <- priorH1 / priorH0

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

    posteriorEvidenceRatio <- postH1 / postH0

  }

  expectedShift <- round(posteriorEvidenceRatio / priorEvidenceRatio, 2)

  if(expectedShift == "NaN") # Happens when the prior is improper (e.g., Gamma(1, 0))
    expectedShift <- Inf

  result <- list(expectedShift = expectedShift, 
                 priorEvidenceRatio = priorEvidenceRatio, 
                 priorH1 = priorH1, 
                 priorH0 = priorH0,
                 postH1 = postH1,
                 postH0 = postH0,
                 posteriorEvidenceRatio = posteriorEvidenceRatio)

  return(result)
}

.auditEvidenceRatio <- function(planningOptions,
                                evaluationState){

  # For calculation of the evidence ratio, the area below materiality is 
  # regarded to be H1 and the area equal to and above materiality is regarded
  # to be H0.

  if(planningOptions[["likelihood"]] == "poisson"){

    priorH1 <- diff(pgamma(c(0, evaluationState[["materiality"]]), 
                           shape = 1 + evaluationState[["kPrior"]], 
                           rate = evaluationState[["nPrior"]]))

    priorH0 <- diff(pgamma(c(evaluationState[["materiality"]], 1), 
                           shape = 1 + evaluationState[["kPrior"]], 
                           rate = evaluationState[["nPrior"]]))
    
  } else if(planningOptions[["likelihood"]] == "binomial"){

    priorH1 <- diff(pbeta(c(0, evaluationState[["materiality"]]), 
                          shape1 = 1 + evaluationState[["kPrior"]], 
                          shape2 = 1 + evaluationState[["nPrior"]] - 
                                   evaluationState[["kPrior"]]))

    priorH0 <- diff(pbeta(c(evaluationState[["materiality"]], 1), 
                          shape1 = 1 + evaluationState[["kPrior"]], 
                          shape2 = 1 + evaluationState[["nPrior"]] - 
                                   evaluationState[["kPrior"]]))
                

  } else if(planningOptions[["likelihood"]] == "hypergeometric"){

    tolerableE <- 0:floor(evaluationState[["materiality"]] * 
                          evaluationState[["N"]])

    intolerableE <- (max(tolerableE) + 1):(evaluationState[["N"]]  - 
                                           evaluationState[["n"]] + 
                                           evaluationState[["k"]])

    priorH1 <- sum(jfa:::.dBetaBinom(x = tolerableE, 
                                      N = evaluationState[["N"]] - 
                                          evaluationState[["n"]] + 
                                          evaluationState[["k"]], 
                                      shape1 = 1 + evaluationState[["kPrior"]], 
                                      shape2 = 1 + evaluationState[["nPrior"]] - 
                                               evaluationState[["kPrior"]]))

    priorH0 <- sum(jfa:::.dBetaBinom(x = intolerableE, 
                                      N = evaluationState[["N"]] - 
                                          evaluationState[["n"]] + 
                                          evaluationState[["k"]], 
                                      shape1 = 1 + evaluationState[["kPrior"]], 
                                      shape2 = 1 + evaluationState[["nPrior"]] - 
                                               evaluationState[["kPrior"]]))

  } 

  priorEvidenceRatio <- priorH1 / priorH0

  if(evaluationState[["method"]] == "poisson"){

    postH1 <- diff(pgamma(c(0, evaluationState[["materiality"]]), 
                          shape = 1 + evaluationState[["kPrior"]] + 
                                  evaluationState[["t"]], 
                          rate = evaluationState[["nPrior"]] + 
                                 evaluationState[["n"]]))

    postH0 <- diff(pgamma(c(evaluationState[["materiality"]], 1), 
                          shape = 1 + evaluationState[["kPrior"]] + 
                                  evaluationState[["t"]], 
                          rate = evaluationState[["nPrior"]] + 
                                 evaluationState[["n"]]))

  } else if(evaluationState[["method"]] == "binomial"){

    postH1 <- diff(pbeta(c(0, evaluationState[["materiality"]]), 
                    shape1 = 1 + evaluationState[["kPrior"]] + 
                              evaluationState[["t"]], 
                    shape2 = 1 + evaluationState[["nPrior"]] -
                              evaluationState[["kPrior"]] + 
                              evaluationState[["n"]] - 
                              evaluationState[["t"]]))

    postH0 <- diff(pbeta(c(evaluationState[["materiality"]], 1), 
                         shape1 = 1 + evaluationState[["kPrior"]] + 
                                  evaluationState[["t"]], 
                          shape2 = 1 + evaluationState[["nPrior"]] -
                                   evaluationState[["kPrior"]] + 
                                   evaluationState[["n"]] - 
                                   evaluationState[["t"]]))

  } else if(evaluationState[["method"]] == "hypergeometric"){

    postH1 <- sum(jfa:::.dBetaBinom(x = tolerableE, 
                                    N = evaluationState[["N"]] - 
                                        evaluationState[["n"]] + 
                                        evaluationState[["k"]], 
                                    shape1 = 1 + evaluationState[["kPrior"]] + 
                                              evaluationState[["k"]], 
                                    shape2 = 1 + evaluationState[["nPrior"]] -
                                              evaluationState[["kPrior"]] + 
                                              evaluationState[["n"]] - 
                                              evaluationState[["k"]]))

    postH0 <- sum(jfa:::.dBetaBinom(x = intolerableE, 
                                     N = evaluationState[["N"]] - 
                                         evaluationState[["n"]] + 
                                         evaluationState[["k"]], 
                                      shape1 = 1 + evaluationState[["kPrior"]] + 
                                               evaluationState[["k"]], 
                                      shape2 = 1 + evaluationState[["nPrior"]] -
                                               evaluationState[["kPrior"]] + 
                                               evaluationState[["n"]] - 
                                               evaluationState[["k"]]))

  } else if(evaluationState[["method"]] == "coxsnell"){

    pCoxAndSnellF <- ecdf(jfa:::.dCoxAndSnellF(seq(0, 1, length.out = 100000), 
                                               df1 = evaluationState[["df1"]],
                                               df2 = evaluationState[["df2"]],
                                               multiplicationFactor = evaluationState[["multiplicationFactor"]]))
    
    postH1 <- pCoxAndSnellF(evaluationState[["materiality"]])
    postH0 <- pCoxAndSnellF(1) - pCoxAndSnellF(evaluationState[["materiality"]])

  }

  posteriorEvidenceRatio <- postH1 / postH0

  shift <- round(posteriorEvidenceRatio / priorEvidenceRatio, 2)

  if(shift == "NaN") # Happens when the prior is improper (e.g., Gamma(1, 0))
    shift <- Inf

  result <- list(shift = shift, 
                 priorEvidenceRatio = priorEvidenceRatio, 
                 priorH1 = priorH1, 
                 priorH0 = priorH0,
                 postH1 = postH1,
                 postH0 = postH0,
                 posteriorEvidenceRatio = posteriorEvidenceRatio)

  return(result)
}

################################################################################
################## Common functions for the Bayesian planning ##################
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

    tableTitle <- gettextf("<b>Table %i.</b> Implicit Sample", 
                           jaspResults[["tabNumber"]]$object)
    
    sampletable <- createJaspTable(tableTitle)
    sampletable$position <- positionInContainer

    sampletable$dependOn(options = c("implicitSampleTable", 
                                    "planningModel",
                                    "priorStatistics"))

    sampletable$addColumnInfo(name = 'implicitn', 
                              title = gettext("Implicit sample size"), 
                              type = 'string')
    sampletable$addColumnInfo(name = 'implicitk', 
                              title = gettext("Implicit errors"), 
                              type = 'string')

    message <- gettextf("Sample sizes shown are implicit sample sizes derived from the ARM risk assessments: IR = <b>%1$s</b> and CR = <b>%2$s</b>.", options[["IR"]], options[["CR"]])

    sampletable$addFootnote(message)

    planningContainer[["sampletable"]] <- sampletable

    if(!ready || planningContainer$getError()) 
      return()

    row <- data.frame(implicitn = planningState[["prior"]]$nPrior, 
                      implicitk = planningState[["prior"]]$kPrior)

    sampletable$addRows(row)
  }
}

.auditPriorAndExpectedPosteriorStatisticsTable <- function(options, 
                                                           planningState, 
                                                           planningContainer, 
                                                           jaspResults,
                                                           ready, 
                                                           positionInContainer){

  if(!options[["priorStatistics"]]) 
    return()
    
  .updateTabNumber(jaspResults)

  if(is.null(planningContainer[["priorStatistics"]])){

    tableTitle <- gettextf("<b>Table %i.</b> Prior and Expected Posterior Descriptive Statistics",
                           jaspResults[["tabNumber"]]$object)
    
    priorStatisticsTable <- createJaspTable(tableTitle)
    priorStatisticsTable$position <- positionInContainer

    priorStatisticsTable$dependOn(options = c("priorStatistics", 
                                              "planningModel",
                                              "implicitSampleTable"))

    priorStatisticsTable$addColumnInfo(name = 'v', 
                                       title = gettext(""), 
                                       type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'form', 
                                      title = gettext("Functional form"), 
                                      type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'priorH1', 
                                      title = gettextf("Support %1$s", "H\u208B"), 
                                      type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'priorH0', 
                                      title = gettextf("Support %1$s", "H\u208A"), 
                                      type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'priorOdds', 
                                      title = gettextf("Evidence ratio %1$s", "<sup>H\u208B</sup>&frasl;<sub>H\u208A</sub>"), 
                                      type = 'string')
    priorStatisticsTable$addColumnInfo(name = 'priorBound', 
                                      title = gettextf("%1$s%% Credible bound", options[["confidence"]]*100) , 
                                      type = 'string')

    planningContainer[["priorStatistics"]] <- priorStatisticsTable

    if(!ready || planningContainer$getError()) {

        row <- data.frame(v = c(gettext("Prior"), gettext("Expected posterior"), gettext("Expected shift")))
        priorStatisticsTable$addRows(row)
        return()

    }

    priorStatisticsTable$addFootnote(gettextf("%1$s: The population misstatement is lower than materiality (%2$s %3$s). %4$s: The population misstatement is equal to, or higher than, materiality (%5$s %6$s)."
    , "H\u208B"
    , "\u03B8 <"
    , round(planningState[["materiality"]], 4)
    , "H\u208A"
    , "\u03B8 \u2265"
    , round(planningState[["materiality"]], 4)))

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
      
    expResult <- .auditExpectedEvidenceRatio(planningState)

    priorOdds <- round(expResult[["priorEvidenceRatio"]], 2)
    priorH1 <- round(expResult[["priorH1"]], 2)
    priorH0 <- round(expResult[["priorH0"]], 2)
    postOdds <- round(expResult[["posteriorEvidenceRatio"]], 2)
    postH1 <- round(expResult[["postH1"]], 2)
    postH0 <- round(expResult[["postH0"]], 2)
    shiftH1 <- round(postH1 / priorH1, 2)
    shiftH0 <- round(postH0 / priorH0, 2)
    shiftOdds <- round(expResult[["posteriorEvidenceRatio"]] / 
                       expResult[["priorEvidenceRatio"]], 2)

    rows <- data.frame(v = c(gettext("Prior"), gettext("Expected posterior"), gettext("Expected shift")),
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
                                title = gettext("Implied Prior from Risk Assessments"), 
                                width = 600, 
                                height = 400)
    priorPlot$position <- positionInContainer

    priorPlot$dependOn(options = c("priorPlotLimit", 
                                   "priorPlot", 
                                   "priorPlotAdditionalInfo", 
                                   "priorPlotExpectedPosterior", 
                                   "planningModel",
                                   "priorAndPosteriorPlotLimit",
                                   "shadePrior"))

    planningContainer[["priorPlot"]] <- priorPlot

    if(!ready || 
       planningContainer$getError()) 
      return()

    xseq <- seq(0, options[["priorPlotLimit"]], 0.0001)

    if(planningState[["likelihood"]] == "binomial"){

      priorData <- data.frame(x = xseq, 
                              y = dbeta(x = xseq, 
                                        shape1 = planningState[["prior"]]$aPrior, 
                                        shape2 = planningState[["prior"]]$bPrior),
                              type = rep(gettext("Prior"), length(xseq)))

      postData <- data.frame(x = xseq, 
                             y = dbeta(x = xseq, 
                                       shape1 = planningState[["prior"]]$aPrior + 
                                                planningState[["expectedSampleError"]], 
                                       shape2 = planningState[["prior"]]$bPrior + 
                                                planningState[["sampleSize"]] - 
                                                planningState[["expectedSampleError"]]),
                            type = rep(gettext("Expected\nposterior"), length(xseq)))

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
                              type = rep(gettext("Prior"), length(xseq)))

      postData <- data.frame(x = xseq, 
                             y = dgamma(x = xseq, 
                                        shape = planningState[["prior"]]$aPrior + 
                                                planningState[["expectedSampleError"]], 
                                        rate = planningState[["prior"]]$bPrior + 
                                               planningState[["sampleSize"]]),
                             type = rep(gettext("Expected\nposterior"), length(xseq)))

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

    } else if(planningState[["likelihood"]] == "hypergeometric"){

      xseq <- 0:ceiling(options[["priorPlotLimit"]] * planningState[["N"]])

      priorData <- data.frame(x = xseq, 
                              y = jfa:::.dBetaBinom(x = xseq, 
                                                    N = planningState[["N"]] - 
                                                        planningState[["sampleSize"]] + 
                                                        planningState[["expectedSampleError"]], 
                                                    shape1 = planningState[["prior"]]$aPrior, 
                                                    shape2 = planningState[["prior"]]$bPrior),
                              type = rep(gettext("Prior"), length(xseq)))

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
                             type = rep(gettext("Expected\nposterior"), length(xseq)))

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

    if(options[["shadePrior"]] == "shadePriorCredibleRegion"){
      pdata3 <- data.frame(x = 0, y = 0, l = "1")
    } else if(options[["shadePrior"]] == "shadePriorHypotheses"){
      pdata3 <- data.frame(x = c(0, 0), y = c(0, 0), l = c("1", "2"))
    }

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
          ggplot2::scale_y_continuous(name = gettext("Density"), 
                                      breaks = yBreaks, 
                                      labels = c("", ""), 
                                      limits = range(yBreaks))

    if(planningState[["likelihood"]] == "hypergeometric"){

      p <- p + ggplot2::scale_x_continuous(name = gettext("Population errors"), 
                                           breaks = xBreaks, 
                                           limits = range(xBreaks), 
                                           labels = xBreaks)

    } else {

      p <- p + ggplot2::scale_x_continuous(name = gettext("Probability of misstatement"), 
                                           breaks = xBreaks, 
                                           limits = range(xBreaks), 
                                           labels = paste0(xBreaks * 100, "%"))

    }
    
    if(options[["priorPlotAdditionalInfo"]]){

      if(options[["shadePrior"]] != "shadePriorNone"){

        if(options[["shadePrior"]] == "shadePriorCredibleRegion"){

          shadeColors <- rgb(0.5, 0.8, 0.57, .7) # Lightgreen

        } else {

          shadeColors <- c(rgb(0, 0.64, 0.80, .6), rgb(0.98, 0.34, 0, .6)) # Blue and orange 

        }
      
        p <- p + ggplot2::geom_point(data = pdata3, 
                                    mapping = ggplot2::aes(x = x, y = y, shape = l), 
                                    size = 0, 
                                    color = shadeColors)

        if(options[["shadePrior"]] == "shadePriorCredibleRegion"){

          if(options[["priorPlotExpectedPosterior"]]){

            p <- p + ggplot2::scale_shape_manual(name = "", 
                                                values = 21, 
                                                labels = gettextf("%1$s%% Prior \ncredible region",
                                                                  round(options[["confidence"]] * 100, 2)))
          } else {

            p <- p + ggplot2::scale_shape_manual(name = "", 
                                                values = 21, 
                                                labels = gettextf("%1$s%% Prior credible region",
                                                                  round(options[["confidence"]] * 100, 2)))
          
          }

        } else if(options[["shadePrior"]] == "shadePriorHypotheses"){

          p <- p + ggplot2::scale_shape_manual(name = "", 
                                                values = c(21, 21), 
                                                labels = c(bquote(paste(.(gettext("Support")), ~H[{"-"}])), 
                                                           bquote(paste(.(gettext("Support")), ~H[{"+"}]))))

        }

        p <- p + ggplot2::guides(shape = ggplot2::guide_legend(
                                                    override.aes = list(size = 15, 
                                                                        shape = 22, 
                                                                        fill = shadeColors, 
                                                                        stroke = 2, 
                                                                        color = "black")))

        if(planningState[["likelihood"]] == "binomial"){

          if(options[["shadePrior"]] == "shadePriorCredibleRegion"){

            p <- p + ggplot2::stat_function(fun = dbeta, 
                                            args = list(
                                                    shape1 = planningState[["prior"]]$aPrior, 
                                                    shape2 = planningState[["prior"]]$bPrior
                                                    ),
                                            xlim = c(0, priorBound),
                                            geom = "area", 
                                            fill = shadeColors)

          } else if(options[["shadePrior"]] == "shadePriorHypotheses"){

            p <- p + ggplot2::stat_function(fun = dbeta, 
                                  args = list(
                                          shape1 = planningState[["prior"]]$aPrior, 
                                          shape2 = planningState[["prior"]]$bPrior
                                          ),
                                  xlim = c(0, planningState[["materiality"]]),
                                  geom = "area", 
                                  fill = shadeColors[1]) +
                      ggplot2::stat_function(fun = dbeta, 
                                        args = list(
                                                shape1 = planningState[["prior"]]$aPrior, 
                                                shape2 = planningState[["prior"]]$bPrior
                                                ),
                                        xlim = c(planningState[["materiality"]], 1),
                                        geom = "area", 
                                        fill = shadeColors[2])

          }

        } else if(planningState[["likelihood"]] == "poisson"){

          if(options[["shadePrior"]] == "shadePriorCredibleRegion"){

            p <- p + ggplot2::stat_function(fun = dgamma, 
                                            args = list(
                                                    shape = planningState[["prior"]]$aPrior, 
                                                    rate = planningState[["prior"]]$bPrior
                                                    ),
                                            xlim = c(0, priorBound),
                                            geom = "area", 
                                            fill = shadeColors)

          } else if(options[["shadePrior"]] == "shadePriorHypotheses"){

            p <- p + ggplot2::stat_function(fun = dgamma, 
                                            args = list(
                                                    shape = planningState[["prior"]]$aPrior, 
                                                    rate = planningState[["prior"]]$bPrior
                                                    ),
                                            xlim = c(0, planningState[["materiality"]]),
                                            geom = "area", 
                                            fill = shadeColors[1]) +
                    ggplot2::stat_function(fun = dgamma, 
                                            args = list(
                                                    shape = planningState[["prior"]]$aPrior, 
                                                    rate = planningState[["prior"]]$bPrior
                                                    ),
                                            xlim = c(planningState[["materiality"]], 1),
                                            geom = "area", 
                                            fill = shadeColors[2])
            
          }

        } else if(planningState[["likelihood"]] == "hypergeometric"){

          if(options[["shadePrior"]] == "shadePriorCredibleRegion"){

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
                                      fill = shadeColors)

          } else if(options[["shadePrior"]] == "shadePriorHypotheses"){

            nseq <- (planningState[["N"]] * planningState[["materiality"]] + 1)
            xseq1 <- xseq[1:nseq]
            barData <- data.frame(x = xseq1, 
                                  y = jfa:::.dBetaBinom(x = xseq1, 
                                                        N = planningState[["N"]] - 
                                                            planningState[["sampleSize"]] + 
                                                            planningState[["expectedSampleError"]], 
                                                        shape1 = planningState[["prior"]]$aPrior, 
                                                        shape2 = planningState[["prior"]]$bPrior))

            p <- p + ggplot2::geom_bar(data = barData, 
                                      stat = "identity", 
                                      fill = shadeColors[1])

            xseq2 <- xseq[(nseq + 1):planningState[["N"]]]
            barData <- data.frame(x = xseq2, 
                                  y = jfa:::.dBetaBinom(x = xseq2, 
                                                        N = planningState[["N"]] - 
                                                            planningState[["sampleSize"]] + 
                                                            planningState[["expectedSampleError"]], 
                                                        shape1 = planningState[["prior"]]$aPrior, 
                                                        shape2 = planningState[["prior"]]$bPrior))

            p <- p + ggplot2::geom_bar(data = barData, 
                                      stat = "identity", 
                                      fill = shadeColors[2])

          } 
        }
      }

      p <- p + ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y), 
                                   data = pdata, 
                                   size = 3, 
                                   shape = 21, 
                                   stroke = 2, 
                                   color = "black", 
                                   fill = rgb(0.9, 0, 0, 1)) +
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
                              legend.key.size = ggplot2::unit(2.5, "line"))
  
    p <- JASPgraphs::themeJasp(p, 
                               legend.position = "top") + myTheme

    priorPlot$plotObject <- p
  }

  if(options[["explanatoryText"]]){

    distribution <- base::switch(planningOptions[["likelihood"]], 
                                 "poisson" = "gamma", 
                                 "binomial" = "beta", 
                                 "hypergeometric" = "beta-binomial")

    y1 <- gettext("The expected errors (grey dot) receive the highest probability. The red dot represents the materiality.")
    y2 <- gettext("The expected posterior has its upper confidence bound below materiality.")

    priorPlotText <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The prior probability distribution <b>(%2$s)</b> on the misstatement in the population. The prior parameters <i>%3$s = %4$s, %5$s = %6$s</i> are derived from the assessments of the inherent and control risk, along with the expected errors. %7$s %8$s",  
                                           jaspResults[["figNumber"]]$object,
                                           distribution,
                                           "\u03B1",
                                           planningState[["prior"]]$aPrior,
                                           "\u03B2",
                                           planningState[["prior"]]$bPrior,
                                           ifelse(options[["priorPlotAdditionalInfo"]], 
                                           yes = y1,
                                           no = ""),
                                           ifelse(options[["priorPlotExpectedPosterior"]], 
                                           yes = y2,
                                           no = "")), "p")

    priorPlotText$position <- positionInContainer + 1
    priorPlotText$dependOn(optionsFromObject = planningContainer[["priorPlot"]])
    priorPlotText$dependOn(options = "explanatoryText")
    planningContainer[["priorPlotText"]] <- priorPlotText
  }
}

################################################################################
################## Common functions for the Bayesian Evaluation ################
################################################################################

.auditEvaluationPriorAndPosterior <- function(options,
                                              planningOptions,
                                              planningState,
                                              evaluationState,
                                              evaluationContainer,
                                              jaspResults,
                                              positionInContainer){

  if(!options[["priorAndPosteriorPlot"]]) 
    return()

  .updateFigNumber(jaspResults)

  if(is.null(evaluationContainer[["priorAndPosteriorPlot"]])){

    priorAndPosteriorPlot <- createJaspPlot(plot = NULL, 
                                            title = gettext("Prior and Posterior Distribution"), 
                                            width = 600, 
                                            height = 400)
    priorAndPosteriorPlot$position <- positionInContainer

    priorAndPosteriorPlot$dependOn(options = c("priorAndPosteriorPlot", 
                                               "priorAndPosteriorPlotLimit", 
                                               "priorAndPosteriorPlotAdditionalInfo",
                                               "priorAndPosteriorPlotExpectedPosterior",
                                               "priorPlotLimit",
                                               "shadePosterior"))

    evaluationContainer[["priorAndPosteriorPlot"]] <- priorAndPosteriorPlot

    if(is.null(evaluationState) || 
        evaluationContainer$getError()) 
      return()

    xseq <- seq(0, options[["priorAndPosteriorPlotLimit"]], 0.0001)

    if(planningState[["likelihood"]] == "binomial"){

      priorData <- data.frame(x = xseq, 
                              y = dbeta(x = xseq, 
                                        shape1 = 1 + evaluationState[["kPrior"]], 
                                        shape2 = 1 + evaluationState[["nPrior"]] -
                                                 evaluationState[["kPrior"]]),
                              type = rep(gettext("Prior"), length(xseq)))

      expPostData <- data.frame(x = xseq, 
                                y = dbeta(x = xseq, 
                                          shape1 = planningState[["prior"]]$aPrior + 
                                                    planningState[["expectedSampleError"]], 
                                          shape2 = planningState[["prior"]]$bPrior + 
                                                    planningState[["sampleSize"]] - 
                                                    planningState[["expectedSampleError"]]),
                                type = rep(gettext("Expected\nposterior"), length(xseq)))

      postData <- data.frame(x = xseq, 
                             y = dbeta(x = xseq, 
                                       shape1 = 1 + evaluationState[["kPrior"]] +  
                                                evaluationState[["t"]], 
                                       shape2 = 1 + evaluationState[["nPrior"]] -
                                                evaluationState[["kPrior"]] + 
                                                evaluationState[["n"]] - 
                                                evaluationState[["t"]]),
                            type = rep(gettext("Posterior"), length(xseq)))

      pdata <- data.frame(x = evaluationState[["materiality"]], 
                          y = dbeta(evaluationState[["materiality"]], 
                                    shape1 = 1 + evaluationState[["kPrior"]] +  
                                             evaluationState[["t"]], 
                                    shape2 = 1 + evaluationState[["nPrior"]] -
                                             evaluationState[["kPrior"]] + 
                                             evaluationState[["n"]] - 
                                             evaluationState[["t"]]))

    } else if(planningState[["likelihood"]] == "poisson"){

      priorData <- data.frame(x = xseq, 
                              y = dgamma(x = xseq, 
                                         shape = 1 + evaluationState[["kPrior"]], 
                                         rate = evaluationState[["nPrior"]]),
                              type = rep(gettext("Prior"), length(xseq)))

      postData <- data.frame(x = xseq, 
                             y = dgamma(x = xseq, 
                                        shape = 1 + evaluationState[["kPrior"]] + 
                                                evaluationState[["t"]], 
                                        rate = evaluationState[["nPrior"]] + 
                                               evaluationState[["n"]]),
                             type = rep(gettext("Posterior"), length(xseq)))

      expPostData <- data.frame(x = xseq, 
                                y = dgamma(x = xseq, 
                                            shape = planningState[["prior"]]$aPrior + 
                                                    planningState[["expectedSampleError"]], 
                                            rate = planningState[["prior"]]$bPrior + 
                                                  planningState[["sampleSize"]]),
                                type = rep(gettext("Expected\nposterior"), length(xseq)))

      pdata <- data.frame(x = evaluationState[["materiality"]], 
                          y = dgamma(evaluationState[["materiality"]], 
                                    shape = 1 + evaluationState[["kPrior"]] + 
                                            evaluationState[["t"]], 
                                    rate = evaluationState[["nPrior"]] + 
                                           evaluationState[["n"]]))
                                      
    } else if(planningState[["likelihood"]] == "hypergeometric"){

      xseq <- 0:ceiling(options[["priorAndPosteriorPlotLimit"]] * planningOptions[["populationSize"]])

      priorData <- data.frame(x = xseq, 
                              y = jfa:::.dBetaBinom(x = xseq, 
                                                    N = planningOptions[["populationSize"]] - 
                                                        evaluationState[["n"]] + 
                                                        evaluationState[["k"]], 
                                                    shape1 = 1 + evaluationState[["kPrior"]], 
                                                    shape2 = 1 + evaluationState[["nPrior"]] -
                                                             evaluationState[["kPrior"]]),
                              type = rep(gettext("Prior"), length(xseq)))

      expPostData <- data.frame(x = xseq, 
                                y = jfa:::.dBetaBinom(x = xseq, 
                                N = planningState[["N"]] - 
                                    planningState[["sampleSize"]] + 
                                    planningState[["expectedSampleError"]], 
                                shape1 = planningState[["prior"]]$aPrior + 
                                          planningState[["expectedSampleError"]], 
                                shape2 = planningState[["prior"]]$bPrior + 
                                          planningState[["sampleSize"]] - 
                                          planningState[["expectedSampleError"]]),
                                type = rep(gettext("Expected\nposterior"), length(xseq)))

      postData <- data.frame(x = xseq, 
                             y = jfa:::.dBetaBinom(x = xseq, 
                             N = planningOptions[["populationSize"]] - 
                                 evaluationState[["n"]] + 
                                 evaluationState[["k"]], 
                             shape1 = 1 + evaluationState[["kPrior"]] + 
                                      evaluationState[["k"]], 
                             shape2 = 1 + evaluationState[["nPrior"]] -
                                      evaluationState[["kPrior"]] + 
                                      evaluationState[["n"]] - 
                                      evaluationState[["k"]]),
                             type = rep(gettext("Posterior"), length(xseq)))

      pdata <- data.frame(x = evaluationState[["materiality"]] * planningOptions[["populationSize"]], 
                          y = jfa:::.dBetaBinom(ceiling(
                                                  evaluationState[["materiality"]] * 
                                                  planningOptions[["populationSize"]]
                                                  ),
                                                N = planningOptions[["populationSize"]] - 
                                                    evaluationState[["n"]] + 
                                                    evaluationState[["k"]], 
                                                shape1 = 1 + evaluationState[["kPrior"]] + 
                                                         evaluationState[["k"]], 
                                                shape2 = 1 + evaluationState[["nPrior"]] -
                                                         evaluationState[["kPrior"]] + 
                                                         evaluationState[["n"]] - 
                                                         evaluationState[["k"]]))

    }

    if(evaluationState[["method"]] == "coxsnell"){

      postData <- data.frame(x = xseq, 
                             y = jfa:::.dCoxAndSnellF(xseq,
                                  df1 = evaluationState[["df1"]],
                                  df2 = evaluationState[["df2"]],
                                  multiplicationFactor = evaluationState[["multiplicationFactor"]]
                             ),
                             type = rep(gettext("Posterior"), length(xseq)))

      pdata <- data.frame(x = evaluationState[["materiality"]], 
                             y = jfa:::.dCoxAndSnellF(evaluationState[["materiality"]],
                                  df1 = evaluationState[["df1"]],
                                  df2 = evaluationState[["df2"]],
                                  multiplicationFactor = evaluationState[["multiplicationFactor"]]
                             ))

    }

    posteriorBound <- evaluationState[["confBound"]]

    if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){
      pdata3 <- data.frame(x = 0, y = 0, l = "1")
    } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){
      pdata3 <- data.frame(x = c(0, 0), y = c(0, 0), l = c("1", "2"))
    }

    plotData <- rbind(priorData, postData)
    plotData$type <- factor(x = plotData$type, 
                        levels = levels(plotData$type)[c(1,2)])

    if(options[["priorAndPosteriorPlotExpectedPosterior"]]){

      plotData <- rbind(plotData, expPostData)
      plotData$type <- factor(x = plotData$type, 
                        levels = levels(plotData$type)[c(1,2, 3)])

    }

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xseq, min.n = 4)
    yBreaks <- c(0, 1.2 * max(plotData$y))

    # Adjust legend
    if(options[["priorAndPosteriorPlotExpectedPosterior"]]){

      scaleValues <- c("dashed", "solid", "dotted")

    } else {

      scaleValues <- c("dashed", "solid")
    }

    guide <- ggplot2::guide_legend(nrow = 1, 
                                byrow = FALSE, 
                                title = "", 
                                order = 1)

    p <- ggplot2::ggplot(data = plotData, 
                         mapping = ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y, linetype = type), 
                             lwd = 1) +
          ggplot2::scale_linetype_manual(values = scaleValues, 
                                         guide = guide) +
          ggplot2::scale_y_continuous(name = gettext("Density"), 
                                      breaks = yBreaks, 
                                      labels = c("", ""), 
                                      limits = range(yBreaks))

    if(planningOptions[["likelihood"]] == "hypergeometric"){

        p <- p + ggplot2::scale_x_continuous(name = gettext("Population errors"), 
                                            breaks = xBreaks, 
                                            limits = range(xBreaks), 
                                            labels = xBreaks)

      } else {

        p <- p + ggplot2::scale_x_continuous(name = gettext("Probability of misstatement"), 
                                            breaks = xBreaks, 
                                            limits = range(xBreaks), 
                                            labels = paste0(xBreaks * 100, "%"))

    }

    if(options[["priorAndPosteriorPlotAdditionalInfo"]]){

        if(options[["shadePosterior"]] != "shadePosteriorNone"){

          if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){

            shadeColors <- rgb(0.37, 0.42, 0.69, 0.5) # Darkpurple

          } else {

            shadeColors <- c(rgb(0, 0.64, 0.80, .6), rgb(0.98, 0.34, 0, .6)) # Blue and orange 

          }

          p <- p + ggplot2::geom_point(data = pdata3, 
                                      mapping = ggplot2::aes(x = x, y = y, shape = l), 
                                      size = 0, 
                                      color = shadeColors)

          if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){

              p <- p + ggplot2::scale_shape_manual(name = "", 
                                                  values = 21, 
                                                  labels = gettextf("%1$s%% Posterior \ncredible region",
                                                                  round(options[["confidence"]] * 100, 2)))

          } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){

          p <- p + ggplot2::scale_shape_manual(name = "", 
                                                values = c(21, 21), 
                                                labels = c(bquote(paste(.(gettext("Support")), ~H[{"-"}])), 
                                                           bquote(paste(.(gettext("Support")), ~H[{"+"}]))))

          }

          p <- p + ggplot2::guides(shape = ggplot2::guide_legend(
                                                      override.aes = list(size = 15, 
                                                                          shape = 22, 
                                                                          fill = shadeColors, 
                                                                          stroke = 2, 
                                                                          color = "black")))

          if(options[["areaUnderPosterior"]] == "displayCredibleInterval"){

            credibleInterval <- .auditCalculateCredibleInterval(evaluationState)
            functionLimits <- c(credibleInterval[["lowerBound"]], 
                                credibleInterval[["upperBound"]])

          } else if(options[["areaUnderPosterior"]] == "displayCredibleBound"){
            functionLimits <- c(0, posteriorBound)
          }                                                                  

        if(evaluationState[["method"]] == "coxsnell"){

          if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){

            p <- p + ggplot2::stat_function(fun = jfa:::.dCoxAndSnellF, 
                                    args = list(
                                            df1 = evaluationState[["df1"]], 
                                            df2 = evaluationState[["df2"]],
                                            multiplicationFactor = evaluationState[["multiplicationFactor"]] 
                                            ),
                                    xlim = functionLimits,
                                    geom = "area", 
                                    fill = shadeColors)

          } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){

            p <- p + ggplot2::stat_function(fun = jfa:::.dCoxAndSnellF, 
                                    args = list(
                                            df1 = evaluationState[["df1"]], 
                                            df2 = evaluationState[["df2"]],
                                            multiplicationFactor = evaluationState[["multiplicationFactor"]] 
                                            ),
                                    xlim = c(0, evaluationState[["materiality"]]),
                                    geom = "area", 
                                    fill = shadeColors[1]) +
                      ggplot2::stat_function(fun = jfa:::.dCoxAndSnellF, 
                                    args = list(
                                            df1 = evaluationState[["df1"]], 
                                            df2 = evaluationState[["df2"]],
                                            multiplicationFactor = evaluationState[["multiplicationFactor"]] 
                                            ),
                                    xlim = c(evaluationState[["materiality"]], 1),
                                    geom = "area", 
                                    fill = shadeColors[2])

          }

        } else if(evaluationState[["method"]] == "binomial"){

          if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){

            p <- p + ggplot2::stat_function(fun = dbeta, 
                                            args = list(
                                                    shape1 = 1 + evaluationState[["kPrior"]] +
                                                            evaluationState[["t"]], 
                                                    shape2 = 1 + evaluationState[["nPrior"]] - 
                                                            evaluationState[["kPrior"]] +
                                                            evaluationState[["n"]] - 
                                                            evaluationState[["t"]] 
                                                    ),
                                            xlim = functionLimits,
                                            geom = "area", 
                                            fill = shadeColors)

          } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){

            p <- p + ggplot2::stat_function(fun = dbeta, 
                                            args = list(
                                                    shape1 = 1 + evaluationState[["kPrior"]] +
                                                            evaluationState[["t"]], 
                                                    shape2 = 1 + evaluationState[["nPrior"]] - 
                                                            evaluationState[["kPrior"]] +
                                                            evaluationState[["n"]] - 
                                                            evaluationState[["t"]] 
                                                    ),
                                            xlim = c(0, evaluationState[["materiality"]]),
                                            geom = "area", 
                                            fill = shadeColors[1]) + 
                    ggplot2::stat_function(fun = dbeta, 
                                            args = list(
                                                    shape1 = 1 + evaluationState[["kPrior"]] +
                                                            evaluationState[["t"]], 
                                                    shape2 = 1 + evaluationState[["nPrior"]] - 
                                                            evaluationState[["kPrior"]] +
                                                            evaluationState[["n"]] - 
                                                            evaluationState[["t"]] 
                                                    ),
                                            xlim = c(evaluationState[["materiality"]], 1),
                                            geom = "area", 
                                            fill = shadeColors[2])

          }

        } else if(evaluationState[["method"]] == "poisson"){

          if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){

            p <- p + ggplot2::stat_function(fun = dgamma, 
                                            args = list(
                                                    shape = 1 + evaluationState[["kPrior"]] +
                                                            evaluationState[["t"]], 
                                                    rate = evaluationState[["nPrior"]] + 
                                                          evaluationState[["n"]]
                                                    ),
                                            xlim = functionLimits,
                                            geom = "area", 
                                            fill = shadeColors)

          } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){

            p <- p + ggplot2::stat_function(fun = dgamma, 
                                            args = list(
                                                    shape = 1 + evaluationState[["kPrior"]] +
                                                            evaluationState[["t"]], 
                                                    rate = evaluationState[["nPrior"]] + 
                                                          evaluationState[["n"]]
                                                    ),
                                            xlim = c(0, evaluationState[["materiality"]]),
                                            geom = "area", 
                                            fill = shadeColors[1]) +
                      ggplot2::stat_function(fun = dgamma, 
                                            args = list(
                                                    shape = 1 + evaluationState[["kPrior"]] +
                                                            evaluationState[["t"]], 
                                                    rate = evaluationState[["nPrior"]] + 
                                                          evaluationState[["n"]]
                                                    ),
                                            xlim = c(evaluationState[["materiality"]], 1),
                                            geom = "area", 
                                            fill = shadeColors[2]) 

          }

        } else if(evaluationState[["method"]] == "hypergeometric"){

          if(options[["shadePosterior"]] == "shadePosteriorCredibleRegion"){

            xseq <- xseq[(ceiling(functionLimits[1] * planningOptions[["populationSize"]]) + 1):
                        (ceiling(functionLimits[2] * planningOptions[["populationSize"]]) + 1)]
            barData <- data.frame(x = xseq, 
                                  y = jfa:::.dBetaBinom(x = xseq, 
                                                        N = planningOptions[["populationSize"]] - 
                                                            evaluationState[["n"]] + 
                                                            evaluationState[["k"]], 
                                                        shape1 = 1 + evaluationState[["kPrior"]] + 
                                                                evaluationState[["k"]], 
                                                        shape2 = 1 + evaluationState[["nPrior"]] -
                                                                evaluationState[["kPrior"]] + 
                                                                evaluationState[["n"]] - 
                                                                evaluationState[["k"]]))

            p <- p + ggplot2::geom_bar(data = barData, 
                                      stat = "identity", 
                                      fill = shadeColors)

          } else if(options[["shadePosterior"]] == "shadePosteriorHypotheses"){

            nseq <- (planningOptions[["populationSize"]] * planningState[["materiality"]] + 1)
            xseq1 <- xseq[1:nseq]
            barData <- data.frame(x = xseq1, 
                                  y = jfa:::.dBetaBinom(x = xseq1, 
                                                        N = planningOptions[["populationSize"]] - 
                                                            evaluationState[["n"]] + 
                                                            evaluationState[["k"]], 
                                                        shape1 = 1 + evaluationState[["kPrior"]] + 
                                                                evaluationState[["k"]], 
                                                        shape2 = 1 + evaluationState[["nPrior"]] -
                                                                evaluationState[["kPrior"]] + 
                                                                evaluationState[["n"]] - 
                                                                evaluationState[["k"]]))

            p <- p + ggplot2::geom_bar(data = barData, 
                                      stat = "identity", 
                                      fill = shadeColors[1])

            xseq2 <- xseq[(nseq + 1):planningOptions[["populationSize"]]]
            barData <- data.frame(x = xseq2, 
                                  y = jfa:::.dBetaBinom(x = xseq2, 
                                                        N = planningOptions[["populationSize"]] - 
                                                            evaluationState[["n"]] + 
                                                            evaluationState[["k"]], 
                                                        shape1 = 1 + evaluationState[["kPrior"]] + 
                                                                evaluationState[["k"]], 
                                                        shape2 = 1 + evaluationState[["nPrior"]] -
                                                                evaluationState[["kPrior"]] + 
                                                                evaluationState[["n"]] - 
                                                                evaluationState[["k"]]))

            p <- p + ggplot2::geom_bar(data = barData, 
                                      stat = "identity", 
                                      fill = shadeColors[2])

          }
        }
      }

      p <- p + ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y), 
                                   data = pdata, 
                                   size = 3, 
                                   shape = 21, 
                                   stroke = 2, 
                                   color = "black", 
                                   fill = rgb(0.9, 0, 0, 1))
    }

    myTheme <- ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                              axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0)),
                              legend.key.size = ggplot2::unit(3, "line"))
  
    if(options[["priorAndPosteriorPlotAdditionalInfo"]] && 
        options[["priorAndPosteriorPlotExpectedPosterior"]] &&
        options[["shadePosterior"]] == "shadePosteriorHypotheses"){
      myTheme <- myTheme +  ggplot2::theme(legend.text = ggplot2::element_text(size = 12))
    } 

    p <- JASPgraphs::themeJasp(p, 
                               legend.position = "top") + myTheme

    priorAndPosteriorPlot$plotObject <- p
  }

  if(options[["explanatoryText"]]){

    distribution <- base::switch(evaluationState[["method"]], 
                                 "poisson" = gettext("gamma"), 
                                 "binomial" = gettext("beta"), 
                                 "hypergeometric" = gettext("beta-binomial"),
                                 "coxsnell" = gettext("Cox and Snell"))

   y1 <- gettext("The red dot represents the specified materiality. If the credible area under the distribution surpasses this point, the estimate of the maximum misstatement exceeds the materiality.")

    priorAndPosteriorPlotText <- createJaspHtml(gettextf("<b>Figure %1$i.</b> The prior and posterior probability distribution <b>(%2$s)</b> on the misstatement in the population. %3$s",
                                                        jaspResults[["figNumber"]]$object,
                                                        distribution,
                                                        ifelse(options[["priorAndPosteriorPlotAdditionalInfo"]],
                                                        yes = y1,
                                                        no = "")), "p")

    priorAndPosteriorPlotText$position <- positionInContainer + 1
    priorAndPosteriorPlotText$dependOn(optionsFromObject = evaluationContainer[["priorAndPosteriorPlot"]])
    priorAndPosteriorPlotText$dependOn(options = "explanatoryText")
    evaluationContainer[["priorAndPosteriorPlotText"]] <- priorAndPosteriorPlotText
  }
}

.auditPriorAndPosteriorStatisticsTable <- function(options, 
                                                   planningOptions,
                                                   evaluationState, 
                                                   evaluationContainer, 
                                                   jaspResults,
                                                   positionInContainer){

  if(!options[["priorAndPosteriorStatistics"]]) 
    return()
    
  .updateTabNumber(jaspResults)

  if(is.null(evaluationContainer[["priorAndPosteriorStatistics"]])){

    tableTitle <- gettextf("<b>Table %i.</b> Prior and Posterior Descriptive Statistics",
                        jaspResults[["tabNumber"]]$object)
    
    priorAndPosteriorStatisticsTable <- createJaspTable(tableTitle)
    priorAndPosteriorStatisticsTable$position <- positionInContainer

    priorAndPosteriorStatisticsTable$dependOn(options = "priorAndPosteriorStatistics")

    priorAndPosteriorStatisticsTable$addColumnInfo(name = 'v', 
                                                   title = gettext(""), 
                                                   type = 'string')
    priorAndPosteriorStatisticsTable$addColumnInfo(name = 'form', 
                                                   title = gettext("Functional form"), 
                                                   type = 'string')
    priorAndPosteriorStatisticsTable$addColumnInfo(name = 'priorH1', 
                                                   title = gettextf("Support %1$s", "H\u208B"), 
                                                   type = 'string')
    priorAndPosteriorStatisticsTable$addColumnInfo(name = 'priorH0', 
                                                   title = gettextf("Support %1$s", "H\u208A"), 
                                                   type = 'string')
    priorAndPosteriorStatisticsTable$addColumnInfo(name = 'priorOdds', 
                                                   title = gettextf("Evidence ratio %1$s", "<sup>H\u208B</sup>&frasl;<sub>H\u208A</sub>"), 
                                                   type = 'string')
    priorAndPosteriorStatisticsTable$addColumnInfo(name = 'priorBound', 
                                                   title = gettextf("%1$s%% Credible bound", round(options[["confidence"]] * 100, 2)) , 
                                                   type = 'string')
    priorAndPosteriorStatisticsTable$addFootnote(message = gettextf("%1$s: The population misstatement is lower than materiality (%2$s %3$s). %4$s: The population misstatement is equal to, or higher than, materiality (%5$s %6$s).",
                                                                    "H\u208B",
                                                                    "\u03B8 <",
                                                                    round(evaluationState[["materiality"]], 3),
                                                                    "H\u208A",
                                                                    "\u03B8 \u2265",
                                                                    round(evaluationState[["materiality"]], 3)))

    evaluationContainer[["priorAndPosteriorStatistics"]] <- priorAndPosteriorStatisticsTable

    if(options[["auditResult"]] == "" || 
        evaluationContainer$getError()) {

        row <- data.frame(v = c(gettext("Prior"), gettext("Posterior"), gettext("Shift")))
        priorAndPosteriorStatisticsTable$addRows(row)
        return()

    }

    if(planningOptions[["likelihood"]] == "poisson"){

      priorBound <- round(qgamma(p = options[["confidence"]], 
                                  shape = 1 + evaluationState[["kPrior"]], 
                                  rate = evaluationState[["nPrior"]]), 6)

      priorForm <- paste0("Gamma(\u03B1 = ", 
                          round(1 + evaluationState[["kPrior"]], 3),
                          ", \u03B2 = ",
                          round(evaluationState[["nPrior"]], 3),
                          ")")

    } else if(planningOptions[["likelihood"]] == "binomial"){

      priorBound <- round(qbeta(p = options[["confidence"]], 
                                shape1 = 1 + evaluationState[["kPrior"]], 
                                shape2 = 1 + evaluationState[["nPrior"]] -
                                         evaluationState[["kPrior"]]), 6)

      priorForm <- paste0("Beta(\u03B1 = ", 
                          round(1 + evaluationState[["kPrior"]], 3),
                          ", \u03B2 = ",
                          round(1 + evaluationState[["nPrior"]] - 
                                 evaluationState[["kPrior"]], 3),
                          ")")

    } else if(planningOptions[["likelihood"]] == "hypergeometric"){

      priorBound <- round(jfa:::.qBetaBinom(p = options[["confidence"]], 
                                  N = evaluationState[["N"]] - 
                                      evaluationState[["n"]] + 
                                      evaluationState[["k"]], 
                                  shape1 = 1 + evaluationState[["kPrior"]], 
                                  shape2 = 1 + evaluationState[["nPrior"]] -
                                           evaluationState[["kPrior"]]) / 
                                evaluationState[["N"]], 6)

      priorForm <- paste0("Beta-binomial(N = ",
                          evaluationState[["N"]] - 
                          evaluationState[["k"]] + 
                          evaluationState[["k"]],
                          ", \u03B1 = ", 
                          round(1 + evaluationState[["kPrior"]], 3),
                          ", \u03B2 = ",
                          round(1 + evaluationState[["nPrior"]] - 
                                evaluationState[["kPrior"]], 3),
                          ")")

    }

    if(evaluationState[["method"]] == "poisson"){

      postBound <- round(qgamma(p = options[["confidence"]], 
                                shape = 1 + evaluationState[["kPrior"]] +
                                        evaluationState[["t"]], 
                                rate = evaluationState[["nPrior"]] + 
                                       evaluationState[["n"]]), 6)

      posteriorForm <- paste0("Gamma(\u03B1 = ", 
                              round(evaluationState[["kPrior"]] + 
                                    evaluationState[["t"]], 3),
                              ", \u03B2 = ",
                              round(evaluationState[["nPrior"]] +
                                    evaluationState[["n"]], 3),
                              ")")

    } else if(evaluationState[["method"]] == "binomial"){

      postBound <- round(qbeta(p = options[["confidence"]], 
                                shape1 = 1 + evaluationState[["kPrior"]] +
                                          evaluationState[["t"]], 
                                shape2 = 1 + evaluationState[["nPrior"]] -
                                         evaluationState[["kPrior"]] +
                                         evaluationState[["n"]] - 
                                         evaluationState[["t"]]), 6)

      posteriorForm <- paste0("Beta(\u03B1 = ", 
                              round(1 + evaluationState[["kPrior"]] + 
                                    evaluationState[["t"]], 3),
                              ", \u03B2 = ",
                              round(1 + evaluationState[["nPrior"]] +
                                    evaluationState[["n"]] -
                                    evaluationState[["t"]], 3),
                              ")")
      
    } else if(evaluationState[["method"]] == "hypergeometric"){

      postBound <- round(jfa:::.qBetaBinom(p = options[["confidence"]], 
                                      N = evaluationState[["N"]] - 
                                          evaluationState[["n"]] + 
                                          evaluationState[["k"]], 
                                      shape1 = 1 + evaluationState[["kPrior"]] + 
                                               evaluationState[["k"]], 
                                      shape2 = 1 + evaluationState[["nPrior"]] -
                                               evaluationState[["kPrior"]] +
                                               evaluationState[["n"]] - 
                                               evaluationState[["k"]]) / 
                                  evaluationState[["N"]], 6)

      posteriorForm <- paste0("Beta-binomial(N = ", 
                              evaluationState[["N"]] - 
                              evaluationState[["n"]] + 
                              evaluationState[["k"]],
                              ", \u03B1 = ",
                              round(1 + evaluationState[["kPrior"]] + 
                                    evaluationState[["k"]], 3),
                              ", \u03B2 = ",
                              round(1 + evaluationState[["nPrior"]] -
                                    evaluationState[["kPrior"]] +
                                    evaluationState[["n"]] - 
                                    evaluationState[["k"]], 3),
                              ")")   

    } else if(evaluationState[["method"]] == "coxsnell"){

      postBound <- round(evaluationState[["confBound"]], 6)

      posteriorForm <- paste0(round(evaluationState[["multiplicationFactor"]], 5),
                              " \u00D7 F(df1 = ",
                              evaluationState[["df1"]],
                              ", df2 = ",
                              evaluationState[["df2"]],
                              ")")
      
    }
    
    if(evaluationState[["method"]] != "hypergeometric"){

      priorBound  <- paste0(priorBound * 100, "%")
      postBound   <- paste0(postBound * 100, "%")

    } else {

      priorBound  <- ceiling(priorBound * planningOptions[["populationSize"]])
      postBound   <- ceiling(postBound * planningOptions[["populationSize"]])

    }
      
    expResult <- .auditEvidenceRatio(planningOptions,
                                     evaluationState)

    priorOdds <- round(expResult[["priorEvidenceRatio"]], 2)
    priorH1 <- round(expResult[["priorH1"]], 2)
    priorH0 <- round(expResult[["priorH0"]], 2)
    postOdds <- round(expResult[["posteriorEvidenceRatio"]], 2)
    postH1 <- round(expResult[["postH1"]], 2)
    postH0 <- round(expResult[["postH0"]], 2)
    shiftH1 <- round(postH1 / priorH1, 2)
    shiftH0 <- round(postH0 / priorH0, 2)
    shiftOdds <- round(expResult[["posteriorEvidenceRatio"]] / 
                       expResult[["priorEvidenceRatio"]], 2)

    rows <- data.frame(v = c(gettext("Prior"), gettext("Posterior"), gettext("Shift")),
                      form = c(priorForm, posteriorForm, ""),
                      priorH1 = c(priorH1, postH1, shiftH1),
                      priorH0 = c(priorH0, postH0, shiftH0),
                      priorOdds = c(priorOdds, postOdds, shiftOdds),
                      priorBound = c(priorBound, postBound, ""))
    
    priorAndPosteriorStatisticsTable$addRows(rows)
  }
}

.auditCalculateCredibleInterval <- function(evaluationState){

  lowerBoundConfidence <- (1 - evaluationState[["confidence"]]) / 2
  upperBoundConfidence <- evaluationState[["confidence"]] + (1 - evaluationState[["confidence"]]) / 2

  if(evaluationState[["method"]] == "poisson"){

      lowerBound <- round(qgamma(p = lowerBoundConfidence, 
                                shape = 1 + evaluationState[["kPrior"]] +
                                        evaluationState[["t"]], 
                                rate = evaluationState[["nPrior"]] + 
                                       evaluationState[["n"]]), 6)

      upperBound <- round(qgamma(p = upperBoundConfidence, 
                                shape = 1 + evaluationState[["kPrior"]] +
                                        evaluationState[["t"]], 
                                rate = evaluationState[["nPrior"]] + 
                                       evaluationState[["n"]]), 6)

    } else if(evaluationState[["method"]] == "binomial"){

      lowerBound <- round(qbeta(p = lowerBoundConfidence, 
                                shape1 = 1 + evaluationState[["kPrior"]] +
                                          evaluationState[["t"]], 
                                shape2 = 1 + evaluationState[["nPrior"]] -
                                         evaluationState[["kPrior"]] +
                                         evaluationState[["n"]] - 
                                         evaluationState[["t"]]), 6)

      upperBound <- round(qbeta(p = upperBoundConfidence, 
                                shape1 = 1 + evaluationState[["kPrior"]] +
                                          evaluationState[["t"]], 
                                shape2 = 1 + evaluationState[["nPrior"]] -
                                         evaluationState[["kPrior"]] +
                                         evaluationState[["n"]] - 
                                         evaluationState[["t"]]), 6)                                         

      
    } else if(evaluationState[["method"]] == "hypergeometric"){

      lowerBound <- round(jfa:::.qBetaBinom(p = lowerBoundConfidence, 
                                      N = evaluationState[["N"]] - 
                                          evaluationState[["n"]] + 
                                          evaluationState[["k"]], 
                                      shape1 = 1 + evaluationState[["kPrior"]] + 
                                               evaluationState[["k"]], 
                                      shape2 = 1 + evaluationState[["nPrior"]] -
                                               evaluationState[["kPrior"]] +
                                               evaluationState[["n"]] - 
                                               evaluationState[["k"]]) / 
                                  evaluationState[["N"]], 6)

      upperBound <- round(jfa:::.qBetaBinom(p = upperBoundConfidence, 
                                      N = evaluationState[["N"]] - 
                                          evaluationState[["n"]] + 
                                          evaluationState[["k"]], 
                                      shape1 = 1 + evaluationState[["kPrior"]] + 
                                               evaluationState[["k"]], 
                                      shape2 = 1 + evaluationState[["nPrior"]] -
                                               evaluationState[["kPrior"]] +
                                               evaluationState[["n"]] - 
                                               evaluationState[["k"]]) / 
                                  evaluationState[["N"]], 6)                                  

    } else if(evaluationState[["method"]] == "coxsnell"){

      lowerBound <- round(evaluationState[["multiplicationFactor"]] * 
                          qf(p = lowerBoundConfidence,
                             df1 = evaluationState[["df1"]], 
                             df2 = evaluationState[["df2"]]), 6)

      upperBound <- round(evaluationState[["multiplicationFactor"]] * 
                          qf(p = upperBoundConfidence,
                             df1 = evaluationState[["df1"]], 
                             df2 = evaluationState[["df2"]]), 6)
      
    } else if(evaluationState[["method"]] == "regression"){

      lowerBound <- (evaluationState[["popBookvalue"]] - evaluationState[["lowerBound"]]) / 
                     evaluationState[["popBookvalue"]]
      upperBound <- (evaluationState[["popBookvalue"]] - evaluationState[["upperBound"]]) / 
                     evaluationState[["popBookvalue"]]

    }

    results <- list(lowerBound = lowerBound,
                    upperBound = upperBound)
    return(results)
}

.auditBayesianRegression <- function(sample, 
                                     confidence,
                                     options,
                                     planningOptions){

  # Bayesian Linear Regression Using the BAS package
  sample <- stats::na.omit(sample)
  
  sample <- sample[, c(.v(options[["monetaryVariable"]]), .v(options[["auditResult"]]))]
  colnames(sample) <- c("bookValue", "auditValue")
  formula <- auditValue ~ bookValue
  
  if(all(sample[,1] == sample[,2])){
    
    betaStats <- c(1, 1, 1)
    
  } else {
    
    basResult <- BAS::bas.lm(formula, data = sample)
    
    if(options[["areaUnderPosterior"]] == "displayCredibleBound"){

      basSummary <- BAS:::confint.coef.bas(coef(basResult), 
                                           level = planningOptions[["confidence"]] - (1 - planningOptions[["confidence"]]))
    
    } else {

      basSummary <- BAS:::confint.coef.bas(coef(basResult), 
                                           level = planningOptions[["confidence"]])
    
    }
    
    betaStats <- as.numeric(basSummary[2, c(1,2, 3)]) 

  }
  
  beta <- betaStats[3]
  betaMin <- betaStats[1]
  betaMax <- betaStats[2]
  
  n <- nrow(sample)
  N <- planningOptions[["populationSize"]]
  taints <- (sample[["bookValue"]] - sample[["auditValue"]])/ sample[["bookValue"]]
  k <- length(which(taints != 0))
  t <- sum(taints)
  
  meanB <- mean(sample[["bookValue"]])
  meanW <- mean(sample[["auditValue"]])
  
  corBW <- cor(sample[["bookValue"]], 
               sample[["auditValue"]])
  
  sdW <- sd(sample[["auditValue"]])

  pointEstimate <- N * meanW + 
                    beta * (planningOptions[["populationValue"]] - 
                            N * meanB)
  
  stDev <- sdW * sqrt(1 - corBW^2) * ( N / sqrt(n)) * sqrt( (N-n) / (N-1) )
  lowerBound <- pointEstimate - betaMax * stDev
  upperBound <- pointEstimate - betaMin * stDev

  results <- list()
  results[["n"]] <- as.numeric(n)
  results[["k"]] <- as.numeric(k)
  results[["t"]] <- as.numeric(t)
  results[["confidence"]] <- as.numeric(planningOptions[["confidence"]])
  results[["method"]] <- "regression"
  results[["popBookvalue"]] <- as.numeric(planningOptions[["populationValue"]])
  results[["pointEstimate"]] <- as.numeric(pointEstimate)
  results[["lowerBound"]] <- as.numeric(lowerBound)
  results[["upperBound"]] <- as.numeric(upperBound)
  results[["materiality"]] <- as.numeric(planningOptions[["materiality"]])

  return(results)
}

################################################################################
################## End functions ###############################################
################################################################################
