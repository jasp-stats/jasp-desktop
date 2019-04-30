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

TTestBayesianOneSample <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

  if(is.null(options()$BFMaxModels)) options(BFMaxModels = 50000)
  if(is.null(options()$BFpretestIterations)) options(BFpretestIterations = 100)
  if(is.null(options()$BFapproxOptimizer)) options(BFapproxOptimizer = "optim")
  if(is.null(options()$BFapproxLimits)) options(BFapproxLimits = c(-15,15))
  if(is.null(options()$BFprogress)) options(BFprogress = interactive())
  if(is.null(options()$BFfactorsMax)) options(BFfactorsMax = 5)

	options[["wilcoxTest"]] <- FALSE

  all.variables <- unlist(options$variables)

  if (is.null(dataset))
  {
    if (perform == "run") {

      if (options$missingValues == "excludeListwise") {

        dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)

      } else {

        dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
      }

    } else {

      dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
    }
  }

  results <- list()

  meta <- list()

  meta[[1]] <- list(name="ttest", type="table")
  meta[[2]] <- list(name="descriptives", type="object", meta=list(list(name="descriptivesTable", type="table"),
                                                                  list(name = "descriptivesPlots", type = "collection", meta="image")))
  meta[[3]] <- list(name="inferentialPlots", type="collection", meta=list(	name="plotGroups", type="object",
                                                                           meta=list(
                                                                             list(name="PriorPosteriorPlot", type="image"),
                                                                             list(name="BFrobustnessPlot", type="image"),
                                                                             list(name="BFsequentialPlot", type="image")
                                                                           )))

  results[[".meta"]] <- meta
  results[["title"]] <- "Bayesian One Sample T-Test"

  ttest <- list()

  ttest[["title"]] <- "Bayesian One Sample T-Test"

  if (options$effectSizeStandardized == "default") {
    ttest[["citation"]] <- list(
      "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
      "Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")
  } else if (options$effectSizeStandardized == "informative") {
    ttest[["citation"]] <- list(
      "Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2017). Informed Bayesian T-Tests. Manuscript submitted for publication and uploaded to arXiv: https://arxiv.org/abs/1704.02479")
  }


  bf.type <- options$bayesFactorType


  if (bf.type == "BF10") {

    BFH1H0 <- TRUE

    if (options$hypothesis == "notEqualToTestValue") {
      bf.title <- "BF\u2081\u2080"
    }
    if (options$hypothesis == "greaterThanTestValue") {
      bf.title <- "BF\u208A\u2080"
    }
    if (options$hypothesis == "lessThanTestValue") {
      bf.title <- "BF\u208B\u2080"
    }

  } else if (bf.type == "LogBF10") {

    BFH1H0 <- TRUE

    if (options$hypothesis == "notEqualToTestValue") {
      bf.title <- "Log(\u0042\u0046\u2081\u2080)"
    }
    if (options$hypothesis == "greaterThanTestValue") {
      bf.title <- "Log(\u0042\u0046\u208A\u2080)"
    }
    if (options$hypothesis == "lessThanTestValue") {
      bf.title <- "Log(\u0042\u0046\u208B\u2080)"
    }

  } else if (bf.type == "BF01") {

    BFH1H0 <- FALSE

    if (options$hypothesis == "notEqualToTestValue") {
      bf.title <- "BF\u2080\u2081"
    }
    if (options$hypothesis == "greaterThanTestValue") {
      bf.title <- "BF\u2080\u208A"
    }
    if (options$hypothesis == "lessThanTestValue") {
      bf.title <- "BF\u2080\u208B"
    }
  }


  if (options$hypothesis == "notEqualToTestValue") {

    fields <- list(
      list(name="Variable", type="string", title=""),
      list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
      list(name="error", type="number", format="sf:4;dp:3", title="error %"))

  } else {

    fields <- list(
      list(name="Variable", type="string", title=""),
      list(name="BF", type="number", format="sf:4;dp:3", title=bf.title),
      list(name="error", type="number", format="sf:4;dp:3;~", title="error %"))
  }

  ttest[["schema"]] <- list(fields=fields)

  results[["ttest"]] <- ttest

  footnotes <- .newFootnotes()

  if (options$hypothesis == "greaterThanTestValue") {

    note <- "For all tests, the alternative hypothesis specifies that the mean
					is greater than "
    message <- paste0(note, options$testValue, ".")
    .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

  } else if (options$hypothesis == "lessThanTestValue") {

    note <- "For all tests, the alternative hypothesis specifies that the mean
					is less than "
    message <- paste0(note, options$testValue, ".")
    .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

  } else {

    if (options$testValue != 0) {

      message <- paste0("For all tests, the alternative hypothesis specifies that the population mean is different from ", options$testValue,".")
      .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)
    }
  }

  if (options$hypothesis == "notEqualToTestValue") {
    nullInterval <- NULL
    oneSided <- FALSE
  }
  if (options$hypothesis == "greaterThanTestValue") {
    nullInterval <- c(0, Inf)
    oneSided <- "right"
  }
  if (options$hypothesis == "lessThanTestValue") {
    nullInterval <- c(-Inf, 0)
    oneSided <- "left"
  }

  if (options$descriptives || options$descriptivesPlots)
    results[["descriptives"]] <- list(title="Descriptives")

  plotGroups <- list()

  ttest.rows <- list()
  plots.ttest <- list()
  plotTypes <- list()
  plotVariables <- list()
  descriptPlotVariables <- list()
  descriptivesPlots <- list()
  errorFootnotes <- rep("no", length(options$variables))

  state <- .retrieveState()

  diff <- NULL

  if (!is.null(state)) {

    diff <- .diff(options, state$options)

  }

  status <- rep("ok", length(options$variables))
  plottingError <- rep("error", length(options$variables))
  BF10post <- numeric(length(options$variables))

  i <- 1

  for (variable in options[["variables"]])
  {

    if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis) {

      plotGroups[[i]] <- list()
      plotGroups[[i]][["title"]] <- variable
      plotGroups[[i]][["name"]] <- variable
    }

    if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
      (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
      diff$testValue == FALSE && diff$missingValues == FALSE)))) {

      index <- which(state$options$variables == variable)

      if (state$errorFootnotes[index] == "no") {

        ttest.rows[[length(ttest.rows)+1]] <- state$results$ttest$data[[index]]

      } else {

        index2 <- .addFootnote(footnotes, state$errorFootnotes[index])

        errorFootnotes[i] <- state$errorFootnotes[index]

        ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index2)))
      }

      BF10post[i] <- state$BF10post[index]
      status[i] <- state$status[index]
      plottingError[i] <- state$plottingError[index]

    } else {

      ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable, "BF"=".", error=".")
    }


    if (options$descriptivesPlots) {

      if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
         (is.list(diff) && (diff$testValue == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE && diff$missingValues == FALSE &&
                            diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$descriptivesPlots) {

        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists, use it

        index <- which(state$descriptPlotVariables == variable)

        descriptivesPlots[[length(descriptivesPlots)+1]] <- state$descriptivesPlots[[index]]


      } else {

        descriptivesPlot <- list()

        descriptivesPlot[["title"]] <- variable
        descriptivesPlot[["width"]] <- options$plotWidth
        descriptivesPlot[["height"]] <- options$plotHeight
        descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")
        descriptivesPlot[["status"]] <- "waiting"
        descriptivesPlot[["data"]] <- ""

        descriptivesPlots[[length(descriptivesPlots)+1]] <- descriptivesPlot
      }

      descriptPlotVariables[[length(descriptPlotVariables)+1]] <- variable

    }

    if (options$plotPriorAndPosterior){


      if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
          (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                             diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                             diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                             diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                             diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                             diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                             diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
          options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {

        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists, use it

        index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlotAddInfo")

        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]

      } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
                (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                                   diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                                   diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                                   diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                                   diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                                   diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                                   diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
                 !options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {

        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # if the requested plot already exists use it

        index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlot")

        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]

      } else {

        plot <- list()

        plot[["title"]] <- "Prior and Posterior"
        plot[["width"]]  <- 530
        plot[["height"]] <- 400
        plot[["status"]] <- "waiting"

        .plotFunc <- function() {
          .plotPosterior.summarystats.ttest(addInformation = options$plotPriorAndPosteriorAdditionalInf, dontPlotData = TRUE)
        }

        content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)
        plot[["convertible"]] <- TRUE
        plot[["obj"]] <- content[["obj"]]
        plot[["data"]] <- content[["png"]]

        plots.ttest[[length(plots.ttest)+1]] <- plot
      }

      plotGroups[[i]][["PriorPosteriorPlot"]] <- plots.ttest[[length(plots.ttest)]]


      if (options$plotPriorAndPosteriorAdditionalInfo) {

        plotTypes[[length(plotTypes)+1]] <- "posteriorPlotAddInfo"

      } else {

        plotTypes[[length(plotTypes)+1]] <- "posteriorPlot"
      }

      plotVariables[[length(plotVariables)+1]] <- variable
    }

    if (options$plotBayesFactorRobustness){

      if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
          (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                             diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                             diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                             diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                             diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                             diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                             diff$informativeNormalStd == FALSE))) && "robustnessPlotAddInfo" %in% state$plotTypes &&
                             options$plotBayesFactorRobustnessAdditionalInfo) {

        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists, use it
        index <- which(state$plotVariables == variable & state$plotTypes == "robustnessPlotAddInfo")

        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]

      } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
          (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                             diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                             diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                             diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                             diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                             diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                             diff$informativeNormalStd == FALSE))) && "robustnessPlot" %in% state$plotTypes &&
                             !options$plotBayesFactorRobustnessAdditionalInfo) {

        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists, use it
        index <- which(state$plotVariables == variable & state$plotTypes == "robustnessPlot")

        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]

      } else {
        plot <- list()

        plot[["title"]] <- "Bayes Factor Robustness Check"
        plot[["width"]]  <- 530
        plot[["height"]] <- 400
        plot[["status"]] <- "waiting"

        .plotFunc <- function() {
          .plotBF.robustnessCheck.ttest (oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE, additionalInformation=options$plotBayesFactorRobustnessAdditionalInfo)
        }
        content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

        plot[["convertible"]] <- TRUE
        plot[["obj"]] <- content[["obj"]]
        plot[["data"]] <- content[["png"]]

        plots.ttest[[length(plots.ttest)+1]] <- plot
      }

      plotGroups[[i]][["BFrobustnessPlot"]] <- plots.ttest[[length(plots.ttest)]]

      if (options$plotBayesFactorRobustnessAdditionalInfo) {
        plotTypes[[length(plotTypes)+1]] <- "robustnessPlotAddInfo"
      } else {
        plotTypes[[length(plotTypes)+1]] <- "robustnessPlot"
      }

      plotVariables[[length(plotVariables)+1]] <- variable
    }

    if (options$plotSequentialAnalysis){

      if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
          (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                             diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                             diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                             diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                             diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                             diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                             diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
          options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {

        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists, use it

        index <- which(state$plotVariables == variable & state$plotTypes == "sequentialRobustnessPlot")

        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]

      } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
                (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                                   diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                                   diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                                   diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                                   diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                                   diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                                   diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE))) &&
                 !options$plotSequentialAnalysisRobustness && "sequentialPlot" %in% state$plotTypes) {

        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
        # then, if the requested plot already exists use it

        index <- which(state$plotVariables == variable & state$plotTypes == "sequentialPlot")

        plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[index]]

      } else {

        plot <- list()

        plot[["title"]] <- "Sequential Analysis"
        plot[["width"]]  <- 530
        plot[["height"]] <- 400
        plot[["status"]] <- "waiting"

        .plotFunc <- function() {
          .plotSequentialBF.ttest(oneSided= oneSided, BFH1H0= BFH1H0, dontPlotData= TRUE, options = options)
        }
        content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

        plot[["convertible"]] <- TRUE
        plot[["obj"]] <- content[["obj"]]
        plot[["data"]] <- content[["png"]]

        plots.ttest[[length(plots.ttest)+1]] <- plot
      }

      if (options$plotSequentialAnalysisRobustness) {

        plotTypes[[length(plotTypes)+1]] <- "sequentialRobustnessPlot"

      } else {

        plotTypes[[length(plotTypes)+1]] <- "sequentialPlot"
      }

      plotVariables[[length(plotVariables)+1]] <- variable
      plotGroups[[i]][["BFsequentialPlot"]] <- plots.ttest[[length(plots.ttest)]]
    }


    i <- i + 1
  }

  ttest[["data"]] <- ttest.rows
  ttest[["footnotes"]] <- as.list(footnotes)
  results[["ttest"]] <- ttest

  if (options$plotPriorAndPosterior || options$plotBayesFactorRobustness || options$plotSequentialAnalysis)
    results[["inferentialPlots"]] <- list(title=ifelse(length(options[["variables"]]) > 1 ||
                                                         sum(c(options$plotPriorAndPosterior, options$plotBayesFactorRobustness, options$plotSequentialAnalysis)) > 1,
                                                       "Inferential Plots", "Inferential Plot"), collection=plotGroups)

  if (options$descriptivesPlots)
    results[["descriptives"]][["descriptivesPlots"]] <- list(title=ifelse(length(options[["variables"]]) > 1, "Descriptives Plots", "Descriptives Plot"), collection=descriptivesPlots)

  if (options$descriptives) {

    descriptivesComplete <- FALSE

    descriptives <- list()

    descriptives[["title"]] <- "Descriptives"
    descriptives[["cases"]] <- I(options$variables)

    fields <- list(
      list(name="v",    title="",   type="string"),
      list(name="N",    title="N",  type="integer"),
      list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
      list(name="sd",   title="SD", type="number",   format="sf:4;dp:3"),
      list(name="se",   title="SE", type="number",   format="sf:4;dp:3"))

    ## add credible interval values if asked for in plot
    if (options$descriptivesPlots) {
      interval <- 100 * options$descriptivesPlotsCredibleInterval
      title <- paste0(interval, "% Credible Interval")
      fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
                                           format = "sf:4;dp:3", title = "Lower",
                                           overTitle = title)
      fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
                                           format = "sf:4;dp:3", title = "Upper",
                                           overTitle = title)
    }

    descriptives[["schema"]] <- list(fields=fields)
    descriptives.results <- list()

    variables <- options[["variables"]]
    if (length(variables) == 0)
      variables = "."

    for (variable in variables) {

      if (perform == "run" && length(options[["variables"]]) > 0) {

        data <- na.omit(dataset[[ .v(variable) ]])

        if (class(data) != "factor") {

          posteriorSummary <- .posteriorSummaryGroupMean(variable=data, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
          ciLower <- .clean(posteriorSummary$ciLower)
          ciUpper <- .clean(posteriorSummary$ciUpper)

          n    <- .clean(length(data))
          mean <- .clean(mean(data))
          stdDeviation <- .clean(sd(data))
          stdErrorMean <- .clean(sd(data)/sqrt(length(data)))

          result <- list(v=variable, N=n, mean=mean, sd=stdDeviation, se=stdErrorMean, lowerCI = ciLower, upperCI = ciUpper)
        } else {

          n <- .clean(length(data))
          result <- list(v=variable, N=n, mean="", sd="", se="", lowerCI="", upperCI="")
        }

        descriptivesComplete <- TRUE

      } else {

        result <- list(v=variable, N=".", mean=".", sd= ".", se=".", lowerCI=".", upperCI=".")

      }

      descriptives.results[[length(descriptives.results)+1]] <- result
    }

    descriptives[["data"]] <- descriptives.results

    if (descriptivesComplete)
      descriptives[["status"]] <- "complete"

    results[["descriptives"]][["descriptivesTable"]] <- descriptives
  }


  if (perform == "run") {


    i <- 1

    tValue <- rep(NA, length(options[["variables"]]))
    n <- rep(NA, length(options[["variables"]]))

    for (variable in options[["variables"]])
    {

      if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
          (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                             diff$testValue == FALSE && diff$missingValues == FALSE && diff$effectSizeStandardized == FALSE &&
                             diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                             diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                             diff$informativeTScale == FALSE && diff$informativeTDf == FALSE &&
                             diff$informativeNormalMean == FALSE && diff$informativeNormalStd == FALSE)))) {

        index <- which(state$options$variables == variable)

        if (state$errorFootnotes[index] == "no") {

          ttest.rows[[i]] <- state$results$ttest$data[[index]]

        } else {

          index2 <- .addFootnote(footnotes, state$errorFootnotes[index])

          errorFootnotes[i] <- state$errorFootnotes[index]

          ttest.rows[[i]] <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index2)))
        }

        BF10post[i] <- state$BF10post[index]
        tValue[i] <- state$tValue[index]
        n[i] <- state$n[index]
        status[i] <- state$status[index]
        plottingError[i] <- state$plottingError[index]

      } else {

          errors <- .hasErrors(dataset, perform, message = "short", type = c('observations', 'variance', 'infinity'),
                               all.target = variable,
                               observations.amount = '< 2')

          if (!identical(errors, FALSE)) {
              errorMessage <- errors$message
              status[i] <- "error"
          } else {
              errorMessage <- NULL
          }

          if(!is.null(errorMessage)){

              errorFootnotes[i] <- errorMessage

              index <- .addFootnote(footnotes, errorMessage)

              result <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))

              ttest.rows[[i]] <- result

          } else {

              result <- try (silent = TRUE, expr = {

                  variableData <- dataset[[ .v(variable) ]]
                  variableData <- variableData[ ! is.na(variableData) ]

                  variableData <- variableData - options$testValue
                  r <- .generalTtestBF(x = variableData, oneSided = oneSided, options = options)
                  bf.raw <- r[["bf"]]
                  tValue[i] <- r[["tValue"]]
                  n[i] <- r[["n1"]]

                  if (is.na(bf.raw)) {

                      status[i] <- "error"
                      plottingError[i] <- "Plotting is not possible: Bayes factor could not be calculated"
                  } else if(bf.raw == Inf & (options$plotPriorAndPosterior | options$plotBayesFactorRobustness | options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness)){

                      status[i] <- "error"
                      plottingError[i] <- "Plotting is not possible: Bayes factor is infinite"
                  } else if (is.infinite(1 / bf.raw)) {

                      status[i] <- "error"
                      plottingError[i] <- "Plotting is not possible: The Bayes factor is too small"

                  }

                  if (bf.type == "BF01")
                      bf.raw <- 1 / bf.raw

                  BF10post[i] <- bf.raw
                  BF <- .clean(bf.raw)

                  if (options$bayesFactorType == "LogBF10") {

                      BF <- log(BF10post[i])
                      BF <- .clean(BF)
                  }

                  error <- .clean(r[["error"]])

                  list(Variable=variable, BF=BF, error=error)
              })

              if (isTryError(result)) {
                  errorMessage <- .extractErrorMessage(result)
                  errorFootnotes[i] <- errorMessage
                  index <- .addFootnote(footnotes, errorMessage)
                  result <- list(Variable=variable, BF=.clean(NaN), error="", .footnotes=list(BF=list(index)))
                  ttest.rows[[i]] <- result
              }

              ind <- which(variableData == variableData[1])
              idData <- sum((ind+1)-(1:(length(ind))) == 1)

              if(idData > 1 & (options$plotSequentialAnalysis | options$plotSequentialAnalysisRobustness)){

                  #seqFootnote <- paste("Sequential Analysis not possible: The first", idData, "observations are identical")
                  #plotSequentialStatus <- "error"
                  # status[i] <- "sequentialNotPossible"
                  # plottingError[i] <- paste("Sequential Analysis not possible: The first", idData, "observations are identical")

              }

              ttest.rows[[i]] <- result
          }

      }

      i <- i + 1
    }

    ttest[["data"]] <- ttest.rows
    ttest[["footnotes"]] <- as.list(footnotes)
    ttest[["status"]] <- "complete"
    results[["ttest"]] <- ttest

    if ( ! .shouldContinue(callback()))
      return()

    i <- 1
    descriptInd <- 1
    z <- 1

    for (variable in options[["variables"]])
    {

        errors <- .hasErrors(dataset, perform, message = 'short', type = c('infinity','observations','variance'),
                             all.target = variable, observations.amount = "< 2")

        if (!identical(errors, FALSE)) {
            errorMessage <- errors$message
        } else {
            errorMessage <- NULL
        }

      variableData <- dataset[[ .v(variable) ]]
      variableData <- variableData[ ! is.na(variableData) ]
      variableDataDescriptivesPlot <- variableData
      variableData <- variableData - options$testValue

      if (options$descriptivesPlots) {

        if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
            (is.list(diff) && (diff$testValue == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE && diff$missingValues == FALSE &&
                               diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$descriptivesPlots) {

          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists, use it

          index <- which(state$descriptPlotVariables == variable)

          descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]


        } else {

          results[["descriptives"]][["descriptivesPlots"]][["collection"]][[descriptInd]][["status"]] <- "running"

          if ( ! .shouldContinue(callback(results)))
            return()

          plot <- descriptivesPlots[[descriptInd]]

          if (is.null(errorMessage)) {

              p <- try(silent= FALSE, expr= {

                  obj <- .plotGroupMeanBayesOneSampleTtest(variable=variableDataDescriptivesPlot, variableName=variable, testValueOpt=options$testValue,
                                                           descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
                  content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = obj, obj = TRUE)

                  plot[["convertible"]] <- TRUE
                  plot[["obj"]] <- content[["obj"]]
                  plot[["data"]] <- content[["png"]]

              })

              if(isTryError(p)){
                  errorMessage <- .extractErrorMessage(p)
              }

          }

          if (!is.null(errorMessage)) {

              plot[["error"]] <- list(error="badData", errorMessage=errorMessage)

          }

          plot[["status"]] <- "complete"

          descriptivesPlots[[descriptInd]] <- plot

        }

        results[["descriptives"]][["descriptivesPlots"]][["collection"]] <- descriptivesPlots

        descriptInd <- descriptInd + 1

        if ( ! .shouldContinue(callback(results)))
          return()

      }

      if (options$plotPriorAndPosterior) {

        if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
            (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                               diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                               diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                               diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                               diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                               diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                               diff$informativeNormalStd == FALSE))) &&
            options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlotAddInfo" %in% state$plotTypes) {

          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists, use it

          index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlotAddInfo")

          plots.ttest[[z]] <- state$plotsTtest[[index]]

        } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
                  (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                                     diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                                     diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                                     diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                                     diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                                     diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                                     diff$informativeNormalStd == FALSE))) &&
                   !options$plotPriorAndPosteriorAdditionalInfo && "posteriorPlot" %in% state$plotTypes) {

          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # if the requested plot already exists use it

          index <- which(state$plotVariables == variable & state$plotTypes == "posteriorPlot")

          plots.ttest[[z]] <- state$plotsTtest[[index]]

        } else {

          results[["inferentialPlots"]][["collection"]][[i]][["PriorPosteriorPlot"]][["status"]] <- "running"

          if ( ! .shouldContinue(callback(results)))
            return()

          plot <- plots.ttest[[z]]

              if (is.null(errorMessage)) {

                  p <- try(silent= FALSE, expr= {

                      .plotFunc <- function() {
                          .plotPosterior.summarystats.ttest(t = tValue[i], n1 = n[i],
                                                            oneSided = oneSided, BF = BF10post[i], BFH1H0 = BFH1H0,
                                                            rscale = options$priorWidth,
                                                            addInformation = options$plotPriorAndPosteriorAdditionalInfo,
                                                            options = options)
                      }

                      content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

                      plot[["convertible"]] <- TRUE
                      plot[["obj"]] <- content[["obj"]]
                      plot[["data"]] <- content[["png"]]

                  })

                  if(isTryError(p)){
                      errorMessage <- .extractErrorMessage(p)
                  }

              }

          if(!is.null(errorMessage)){
              plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
          }

          plot[["status"]] <- "complete"

          plots.ttest[[z]] <- plot
        }

        plotGroups[[i]][["PriorPosteriorPlot"]] <- plots.ttest[[z]]
        results[["inferentialPlots"]][["collection"]] <- plotGroups

        z <- z + 1

        if ( ! .shouldContinue(callback(results)))
          return()
      }

      if (options$plotBayesFactorRobustness) {

          if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
              (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                                 diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                                 diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                                 diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                                 diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                                 diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                                 diff$informativeNormalStd == FALSE))) && "robustnessPlotAddInfo" %in% state$plotTypes &&
                                 options$plotBayesFactorRobustnessAdditionalInfo) {

            # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
            # then, if the requested plot already exists, use it

            index <- which(state$plotVariables == variable & state$plotTypes == "robustnessPlotAddInfo")

            plots.ttest[[z]] <- state$plotsTtest[[index]]

          } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
            (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                               diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                               diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                               diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                               diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                               diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                               diff$informativeNormalStd == FALSE))) && "robustnessPlot" %in% state$plotTypes &&
                               !options$plotBayesFactorRobustnessAdditionalInfo) {

          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists, use it

          index <- which(state$plotVariables == variable & state$plotTypes == "robustnessPlot")

          plots.ttest[[z]] <- state$plotsTtest[[index]]

        } else {

          results[["inferentialPlots"]][["collection"]][[i]][["BFrobustnessPlot"]][["status"]] <- "running"

          if ( ! .shouldContinue(callback(results)))
            return()

          plot <- plots.ttest[[z]]

          if (options$effectSizeStandardized == "informative") {
            plot[["error"]] <- list(error="badData", errorMessage="Bayes factor robustness check plot currently not supported for informed prior.")
          }

          if (!is.null(errorMessage)) {

              plot[["error"]] <- list(error="badData", errorMessage=errorMessage)

          } else {

            p <- try(silent= FALSE, expr= {

              .plotFunc <- function() {
                .plotBF.robustnessCheck.ttest (x= variableData, oneSided= oneSided, BF10post=BF10post[i], rscale = options$priorWidth, BFH1H0= BFH1H0, additionalInformation=options$plotBayesFactorRobustnessAdditionalInfo)
              }
              content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

              plot[["convertible"]] <- TRUE
              plot[["obj"]] <- content[["obj"]]
              plot[["data"]] <- content[["png"]]

            })

            if (isTryError(p)) {
              errorMessage <- .extractErrorMessage(p)
              plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
            }

          }

          plot[["status"]] <- "complete"

          plots.ttest[[z]] <- plot
        }

        plotGroups[[i]][["BFrobustnessPlot"]] <- plots.ttest[[z]]

        results[["inferentialPlots"]][["collection"]] <- plotGroups

        z <- z + 1

        if ( ! .shouldContinue(callback(results)))
          return()
      }

      if (options$plotSequentialAnalysis) {

        if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
            (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                               diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                               diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                               diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                               diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                               diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                               diff$informativeNormalStd == FALSE))) &&
            options$plotSequentialAnalysisRobustness && "sequentialRobustnessPlot" %in% state$plotTypes) {

          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists, use it

          index <- which(state$plotVariables == variable & state$plotTypes == "sequentialRobustnessPlot")

          plots.ttest[[z]] <- state$plotsTtest[[index]]

        } else if (!is.null(state) && variable %in% state$plotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) ||
                  (is.list(diff) && (diff$priorWidth == FALSE && diff$hypothesis == FALSE && diff$bayesFactorType == FALSE &&
                                     diff$testValue == FALSE && diff$missingValues == FALSE && diff$plotHeight == FALSE &&
                                     diff$plotWidth == FALSE && diff$effectSizeStandardized == FALSE &&
                                     diff$informativeStandardizedEffectSize == FALSE && diff$informativeCauchyLocation == FALSE &&
                                     diff$informativeCauchyScale == FALSE && diff$informativeTLocation == FALSE &&
                                     diff$informativeTScale == FALSE && diff$informativeTDf == FALSE && diff$informativeNormalMean == FALSE &&
                                     diff$informativeNormalStd == FALSE))) &&
                   !options$plotSequentialAnalysisRobustness && "sequentialPlot" %in% state$plotTypes) {

          # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
          # then, if the requested plot already exists use it

          index <- which(state$plotVariables == variable & state$plotTypes == "sequentialPlot")

          plots.ttest[[z]] <- state$plotsTtest[[index]]

        } else {

          results[["inferentialPlots"]][["collection"]][[i]][["BFsequentialPlot"]][["status"]] <- "running"

          if ( ! .shouldContinue(callback(results)))
            return()

          plot <- plots.ttest[[z]]

          if (options$plotSequentialAnalysisRobustness && options$effectSizeStandardized == "informative") {
            plot[["error"]] <- list(error="badData", errorMessage="Sequential analysis robustness check plot currently not supported for informed prior.")
          }

          if (is.null(errorMessage)) {

            p <- try(silent= FALSE, expr= {

              .plotFunc <- function() {
                .plotSequentialBF.ttest (x= variableData, oneSided= oneSided, rscale = options$priorWidth, BFH1H0= BFH1H0, BF10post=BF10post[i],
                                         plotDifferentPriors= options$plotSequentialAnalysisRobustness, options = options)
              }
              content <- .writeImage(width = 530, height = 400, plot = .plotFunc, obj = TRUE)

              plot[["convertible"]] <- TRUE
              plot[["obj"]] <- content[["obj"]]
              plot[["data"]] <- content[["png"]]

            })

            if(isTryError(p)){
                errorMessage <- .extractErrorMessage(p)
            }

          }

          if(!is.null(errorMessage)){

              plot[["error"]] <- list(error="badData", errorMessage=errorMessage)

          }

          plot[["status"]] <- "complete"

          plots.ttest[[z]] <- plot
        }

        plotGroups[[i]][["BFsequentialPlot"]] <- plots.ttest[[z]]

        results[["inferentialPlots"]][["collection"]] <- plotGroups

        z <- z + 1

        if ( ! .shouldContinue(callback(results)))
          return()
      }


      i <- i + 1
    }
  }

  keep <- NULL

  for (plot in plots.ttest)
    keep <- c(keep, plot$data)

  for (plot in descriptivesPlots)
    keep <- c(keep, plot$data)

  if (perform == "init") {

    return(list(results=results, status="inited", state=state, keep=keep))

  } else {

    return(list(results=results, status="complete", state=list(options=options, results=results, plotsTtest=plots.ttest, plotTypes=plotTypes,
                                                               plotVariables=plotVariables, descriptPlotVariables=descriptPlotVariables,
                                                               descriptivesPlots=descriptivesPlots, status=status, plottingError=plottingError,
                                                               BF10post=BF10post, errorFootnotes=errorFootnotes, tValue = tValue, n = n), keep=keep))
  }

}

.oneSidedTtestBFRichard <- function(x=NULL, y=NULL, paired=FALSE, oneSided="right", r= sqrt(2)/2, iterations=10000) {

  # sample from delta posterior
  samples <- BayesFactor::ttestBF(x=x, y=y, paired=paired, posterior=TRUE, iterations=iterations, rscale=r)

  if (is.null(y) || paired) {

    N <- length(x)
    varBeta <- samples[,'sig2'] / (1 * N + 1/samples[,'g'])

    if (paired) {

      meanBeta <- sum(x - y) * varBeta / samples[,'sig2']

    } else {

      meanBeta <- sum(x) * varBeta / samples[,'sig2']
    }

  } else {

    sumN <- length(y) + length(x)
    diffN <- length(y) - length(x)

    varBeta <- samples[,'sig2'] / (sumN/4 + 1/samples[,'g'])

    meanBeta <- varBeta / samples[,'sig2'] * ((sum(x) - sum(y)) + samples[,'mu'] * (diffN)) / 2
  }

  logProbMin <- BayesFactor::logSummaryStats(pnorm(0, meanBeta, sqrt(varBeta), log=TRUE))$logMean

  BF <- BayesFactor::ttestBF(x, y, paired=paired, rscale=r)
  BF10 <- BayesFactor::extractBF(BF, onlybf = TRUE, logbf=TRUE)

  if (oneSided == "right") {

    logProbPlus = pexp(-logProbMin, log=TRUE)
    BFplus1 = log(2) + logProbPlus
    BFplus0 <- BFplus1 + BF10

    return(exp(BFplus0))

  } else if (oneSided == "left") {

    BFmin1 <- log(2) + logProbMin
    BFmin0 <- BFmin1 + BF10
    return(exp(BFmin0))
  }
}

.oneSidedTtestBFRichardAdaptive <- function(x=NULL, y=NULL, paired=FALSE, oneSided="right", r= sqrt(2)/2, nTests=5, nIterations=2000, criterion=.02, nMaxIterations=2050000) {

  variability <- criterion + 1

  while (variability > criterion && nIterations < nMaxIterations) {

    BF <- numeric(nTests)

    for (i in seq_len(nTests)) {

      BF[i] <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, r=r, oneSided = oneSided, iterations = nIterations)

    }

    variability <- sd(abs(log(BF))) / mean(abs(log(BF)))
    nIterations <- 2 * nIterations

  }

  return(mean(BF))

}

.likelihoodShiftedT <- function(par, data) {

  - sum(log( dt((data - par[1]) / par[2], par[3]) / par[2]))

}

.dposteriorShiftedT <- function(x, parameters, oneSided) {

  if (oneSided == FALSE) {

    dt((x - parameters[1]) / parameters[2], parameters[3]) / parameters[2]

  } else if (oneSided == "right") {

    ifelse (x >= 0, (dt((x - parameters[1]) / parameters[2], parameters[3]) / parameters[2]) / pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=FALSE) , 0 )

  } else if (oneSided == "left") {

    ifelse (x <= 0, (dt((x - parameters[1]) / parameters[2], parameters[3]) / parameters[2]) / pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE), 0)

  }

}

.qShiftedT <- function(q, parameters, oneSided) {

  if (oneSided == FALSE) {

    qt(q, df=parameters[3]) * parameters[2] + parameters[1]

  } else if (oneSided == "right") {

    areaSmallerZero <- pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE)

    qt(areaSmallerZero + q * (1 - areaSmallerZero), df=parameters[3]) * parameters[2] + parameters[1]

  } else if (oneSided == "left") {

    areaSmallerZero <- pt((0 - parameters[1]) / parameters[2], parameters[3], lower.tail=TRUE)

    qt(q * areaSmallerZero, df=parameters[3]) * parameters[2] + parameters[1]

  }
}

# pdf cauchy prior
.dprior <- function(x, r, oneSided= oneSided){

  if (oneSided == "right") {

    y <- ifelse(x < 0, 0, 2/(pi*r*(1+(x/r)^2)))
    return(y)
  }

  if (oneSided == "left") {

    y <- ifelse(x > 0, 0, 2/(pi*r*(1+(x/r)^2)))
    return(y)
  }	else {

    return(1/(pi*r*(1+(x/r)^2)))
  }
}

.qt.shiftedT <- function(prob, parameters) {

  qt(prob, parameters[3]) * parameters[2] + parameters[1]

}

.posteriorSummaryGroupMean <- function(variable, descriptivesPlotsCredibleInterval=.95) {

  # Assumes that data are normally distributed
  # Jeffreys prior on mu and sigma: p(mu, sigma) proportional to 1/sigma
  # Compare Gelman et al. "Bayesian Data Analysis" for derivation of marginal posterior distribution of mu (inference for unknown mean and variance of a normal distribution)
  if (is.null(variable)) return(NULL)

  ciLower <- (1 - descriptivesPlotsCredibleInterval) / 2
  ciUpper <- ciLower + descriptivesPlotsCredibleInterval

  df <- length(variable) - 1
  location <- mean(variable)
  scale <- sd(variable) / sqrt(length(variable))

  outTmp <- .qt.shiftedT(c(ciLower, .5, ciUpper), parameters=c(location, scale, df))
  out <- list(ciLower=outTmp[1], median=outTmp[2], ciUpper=outTmp[3])

  return(out)

}


#-------------------------------------------------------------------------------
# GENERAL T-TEST BF FUNCTION
#-------------------------------------------------------------------------------
.generalTtestBF <- function(x = NULL, y = NULL, paired = FALSE, oneSided = FALSE, options) {

  tValue <- unname(t.test(x, y, paired = paired, var.equal = TRUE)$statistic)
  # numeric multiplication is more robust in R
  n1 <- as.numeric(length(x))
  n2 <- ifelse(paired, 0, as.numeric(length(y)))

  if(options[["effectSizeStandardized"]] == "default") {

    ### default zero-centered Cauchy prior
    if (oneSided == FALSE) {
      nullInterval <- c(-Inf, Inf)
    } else if (oneSided == "right") {
      nullInterval <- c(0, Inf)
    } else if (oneSided == "left") {
      nullInterval <- c(-Inf, 0)
    }

    bfObject <- BayesFactor::ttest.tstat(
      t = tValue,
      n1 = n1,
      n2 = n2,
      rscale = options$priorWidth,
      nullInterval = nullInterval)

    bf    <- exp(bfObject$bf)
    error <- 100*bfObject$properror

  } else if (options[["effectSizeStandardized"]] == "informative") {

    ### informed prior ###

    # Note that strictly speaking, in case of the independent samples t-test,
    # for the informed prior n1 corresponds to n2 and n2 to n1 and not vice-versa.
    # However, since in the expression for the Bayes factor they only appear
    # as an "effective" sample size and in the degrees of freedom for which it does
    # not matter whether we swap the two, we retain this order for easier extension
    # of the one-sample case.

    if (options[["informativeStandardizedEffectSize"]] == "cauchy") {
      bfObject <- .bf10_t(t = tValue, n1 = n1, n2 = n2, oneSided = oneSided,
                          independentSamples = ! paired && !is.null(y),
                          prior.location = options[["informativeCauchyLocation"]],
                          prior.scale = options[["informativeCauchyScale"]],
                          prior.df = 1)
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "t") {
      bfObject <- .bf10_t(t = tValue, n1 = n1, n2 = n2, oneSided = oneSided,
                          independentSamples = ! paired && !is.null(y),
                          prior.location = options[["informativeTLocation"]],
                          prior.scale = options[["informativeTScale"]],
                          prior.df = options[["informativeTDf"]])
      bf <- bfObject$bf
      error <- 100*bfObject$error
    } else if (options[["informativeStandardizedEffectSize"]] == "normal") {
      bf <- .bf10_normal(t = tValue, n1 = n1, n2 = n2, oneSided = oneSided,
                         independentSamples = ! paired && !is.null(y),
                         prior.mean = options[["informativeNormalMean"]],
                         prior.variance = options[["informativeNormalStd"]]^2)
      error <- NULL
    }

  }

  return(list(bf = bf, error = error, tValue = tValue, n1 = n1, n2 = n2))
}
