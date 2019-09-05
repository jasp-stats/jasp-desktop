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


ABTestBayesian <- function(jaspResults, dataset, options, ...) {

  ready <- (options$n1 != "" && options$y1 != "" && options$n2 != "" && options$y2 != "")

  ### READ DATA                ###
  if (ready)
    dataset <- .abTestBayesianReadData(dataset, options)

  ## COMPUTE AB OBJECT         ###
  ab_obj <- NULL
  if (ready)
    ab_obj <- try(.calcModel.abTest(dataset = dataset, options = options))

  ### RESULTS - MAIN TABLE     ###
  .abTestBayesianTableMain(jaspResults, ab_obj, options, ready)

  ### DESCRIPTIVES TABLE       ###
  if (options$descriptives)
    .abTestBayesianDescriptivesTable(jaspResults, dataset, options, ready)

  ### PRIOR AND POSTERIOR PLOT ###
  if (options$plotPriorAndPosterior)
    .abTestPlotPriorPosterior(jaspResults, ab_obj, options, ready)

  ### SEQUENTIAL PLOT          ###
  if (options$plotSequentialAnalysis)
    .abTestPlotSequential(jaspResults, ab_obj, ready)

  ### PRIOR PLOT               ###
  if (options$plotPriorOnly)
    .abTestPlotPriorOnly(jaspResults, options)
}


.abTestBayesianReadData <- function(dataset, options) {
  # Get the relevant data
  #
  # Args:
  #   dataset: dataset object
  #   options: a list of user options
  #
  # Return:
  #   The (numeric) columns given as dependent/covariates/wlsWeights

  if (!is.null(dataset))
    return (dataset)

  n1 <- .readDataSetToEnd(columns.as.numeric = c(options$n1), exclude.na.listwise = c(options$n1))
  y1 <- .readDataSetToEnd(columns.as.numeric = c(options$y1), exclude.na.listwise = c(options$y1))
  n2 <- .readDataSetToEnd(columns.as.numeric = c(options$n2), exclude.na.listwise = c(options$n2))
  y2 <- .readDataSetToEnd(columns.as.numeric = c(options$y2), exclude.na.listwise = c(options$y2))

  dataset = list()

  dataset[["n1"]] <- n1[[.v(options$n1)]]
  dataset[["y1"]] <- y1[[.v(options$y1)]]
  dataset[["n2"]] <- n2[[.v(options$n2)]]
  dataset[["y2"]] <- y2[[.v(options$y2)]]

  return(dataset)
}


.abTestBayesianTableMain <- function(jaspResults, ab_obj, options, ready) {

  if (!is.null(jaspResults[["abTestBayesianTable"]]))
    return()

  # Create the main table
  abTestBayesianTable <- createJaspTable(title = "Bayesian A/B Test")
  abTestBayesianTable$dependOn(c("n1", "y1", "n2", "y2", "normal_mu", "normal_sigma", "orEqualTo1Prob", "bayesFactorType",
                                 "orLessThan1Prob", "orGreaterThan1Prob", "orNotEqualTo1Prob", "numSamples", "bayesFactorOrder"))
  abTestBayesianTable$position <- 1

  # abTestBayesianTable$addCitation("JASP Team (2018). JASP (Version 0.9.3) [Computer software].")

  if (options$bayesFactorType == "BF10") {
      bf.title <- "BF<sub>10</sub>"
  } else if (options$bayesFactorType == "BF01") {
      bf.title <- "BF<sub>01</sub>"
  } else if (options$bayesFactorType == "LogBF10") {
      bf.title <- "Log(BF<sub>10</sub>)"
  }

  abTestBayesianTable$addColumnInfo(name = "Models",    title = "Models",    type = "string")
  abTestBayesianTable$addColumnInfo(name = "P(M)",      title = "P(M)",      type = "number", format = "sf:4;dp:3")
  abTestBayesianTable$addColumnInfo(name = "P(M|data)", title = "P(M|data)", type = "number", format = "sf:4;dp:3")
  abTestBayesianTable$addColumnInfo(name = "BF",        title = bf.title,    type = "number")

  jaspResults[["abTestBayesianTable"]] <- abTestBayesianTable

  if (!ready)
    return()

  .abTestBayesianFillTableMain(abTestBayesianTable, ab_obj, options)
}


.abTestBayesianFillTableMain <- function(abTestBayesianTable, ab_obj, options) {

  # Normalize prior probabilities
  sum_logor          <- options$orGreaterThan1Prob + options$orLessThan1Prob + options$orEqualTo1Prob + options$orNotEqualTo1Prob
  orGreaterThan1Prob <- options$orGreaterThan1Prob / sum_logor
  orLessThan1Prob    <- options$orLessThan1Prob / sum_logor
  orEqualTo1Prob     <- options$orEqualTo1Prob / sum_logor
  orNotEqualTo1Prob  <- options$orNotEqualTo1Prob / sum_logor

  if (inherits(ab_obj, "try-error")) {
    errorMessage <- as.character(ab_obj)
    abTestBayesianTable$setError(errorMessage)

    return()
  }

  output.rows <- list()
  rowCount    <- 0

  if (orEqualTo1Prob > 0) {
      rowCount = rowCount + 1
      output.rows[[rowCount]] <- list(
          "Models" = "Log odds ratio = 0",
          "BF" = .clean(1.00),
          "P(M|data)" = ab_obj$post_prob[["H0"]],
          "P(M)" = .clean(orEqualTo1Prob)
      )
  }

  if (orGreaterThan1Prob > 0) {
      rowCount = rowCount + 1
      output.rows[[rowCount]] <- list(
          "Models" = "Log odds ratio > 0",
          "BF" = .clean(ab_obj$bf[["bfplus0"]]),
          "P(M|data)" = ab_obj$post_prob[["H+"]],
          "P(M)" = .clean(orGreaterThan1Prob)
      )
  }

  if (orLessThan1Prob > 0) {
      rowCount = rowCount + 1
      output.rows[[rowCount]] <- list(
          "Models" = "Log odds ratio < 0",
          "BF" = .clean(ab_obj$bf[["bfminus0"]]),
          "P(M|data)" = ab_obj$post_prob[["H-"]],
          "P(M)" = .clean(orLessThan1Prob)
      )
  }

  if (orNotEqualTo1Prob > 0) {
      rowCount = rowCount + 1
      output.rows[[rowCount]] <- list(
          "Models" = "Log odds ratio ≠ 0",
          "BF" = .clean(ab_obj$bf[["bf10"]]),
          "P(M|data)" = ab_obj$post_prob[["H1"]],
          "P(M)" = .clean(orNotEqualTo1Prob)
      )
  }

  if (options$bayesFactorOrder == "bestModelTop") {

      ordered       <- output.rows[order(sapply(output.rows, "[[", "P(M|data)"), decreasing = TRUE)]
      best_model_bf <- ordered[[1]]$BF

      output.rows   <- list()
      for (r in 1:rowCount) {
          ordered[[r]]$BF  <-.clean(ordered[[r]]$BF / best_model_bf)
      }
      output.rows   <- ordered

  }

  for (r in 1:rowCount) {
      if (options$bayesFactorType == "BF01") {
          output.rows[[r]]$BF <- .clean(1 / output.rows[[r]]$BF)
      } else if (options$bayesFactorType == "LogBF10") {
          output.rows[[r]]$BF <- .clean(base::log(output.rows[[r]]$BF))
      }
  }

  abTestBayesianTable$addRows(output.rows)
}


.calcModel.abTest <- function(dataset, options) {
    # Conducts A/B Test
    #
    # Args:
    #   dataset: dataset input by user
    #   options: a list of user options
    #
    # Return:
    #   list containing the ab object

    prior_par <- list(mu_psi = options$normal_mu, sigma_psi = options$normal_sigma, mu_beta = 0, sigma_beta = 1)

    # Normalize prior probabilities
    sum_logor          <- options$orGreaterThan1Prob + options$orLessThan1Prob + options$orEqualTo1Prob + options$orNotEqualTo1Prob
    orGreaterThan1Prob <- options$orGreaterThan1Prob / sum_logor
    orLessThan1Prob    <- options$orLessThan1Prob / sum_logor
    orEqualTo1Prob     <- options$orEqualTo1Prob / sum_logor
    orNotEqualTo1Prob  <- options$orNotEqualTo1Prob / sum_logor

    prior_prob <- c(orNotEqualTo1Prob, orGreaterThan1Prob, orLessThan1Prob, orEqualTo1Prob)
    names(prior_prob) <- c("H1", "H+", "H-", "H0")

    ab <- try(abtest::ab_test(data = dataset, prior_par = prior_par, prior_prob = prior_prob,
                              posterior = TRUE, nsamples = options$numSamples))

    return(ab)
}


.abTestBayesianDescriptivesTable <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["abTestBayesianDescriptivesTable"]]))
    return()

  abTestBayesianDescriptivesTable <- createJaspTable(title = "Descriptives")

  abTestBayesianDescriptivesTable$dependOn(c("n1", "y1", "n2", "y2", "descriptives"))
  abTestBayesianDescriptivesTable$position <- 2

  abTestBayesianDescriptivesTable$addColumnInfo(name = "group",      title = "",           type = "string")
  abTestBayesianDescriptivesTable$addColumnInfo(name = "counts",     title = "Counts",     type = "integer")
  abTestBayesianDescriptivesTable$addColumnInfo(name = "total",      title = "Total",      type = "integer")
  abTestBayesianDescriptivesTable$addColumnInfo(name = "proportion", title = "Proportion", type = "number", format = "sf:4;dp:3")

  jaspResults[["abTestBayesianDescriptivesTable"]] <- abTestBayesianDescriptivesTable

  if (!ready)
    return()

  .abTestBayesianFillDescriptivesTable(abTestBayesianDescriptivesTable, dataset)

  return()
}


.abTestBayesianFillDescriptivesTable <- function(abTestBayesianDescriptivesTable, dataset) {

  output.rows <- list()

  num_rows = length(dataset$y1)
  counts = dataset$y1[num_rows]
  total = dataset$n1[num_rows]
  output.rows[[1]] <- list(group = "Group 1", counts = counts, total = total, proportion = counts / total)

  counts = dataset$y2[num_rows]
  total = dataset$n2[num_rows]
  output.rows[[2]] <- list(group = "Group 2", counts = counts, total = total, proportion = counts / total)

  abTestBayesianDescriptivesTable$addRows(output.rows)
}


.abTestPlotPriorPosterior <- function(jaspResults, ab_obj, options, ready) {

  abTestPriorAndPosteriorPlot <- createJaspPlot(title = "Prior and Posterior",  width = 530, height = 400)
  abTestPriorAndPosteriorPlot$dependOn(c("n1", "y1", "n2", "y2", "normal_mu", "normal_sigma", "plotPosteriorType", "plotPriorAndPosterior"))
  jaspResults[["abTestPriorAndPosteriorPlot"]] <- abTestPriorAndPosteriorPlot

  if (!ready)
    return()

  abTestPriorAndPosteriorPlot$plotObject <- .plotPosterior.abTest(ab_obj, options$plotPosteriorType)
}


.plotPosterior.abTest <- function(ab_obj, posteriorPlotType) {
  # Plot the posterior for different models
  #
  # Args:
  #   ab_obj: ab test object
  #   posteriorPlotType

  what <- ifelse(posteriorPlotType == "LogOddsRatio", "logor",
          ifelse(posteriorPlotType == "OddsRatio",    "or",
          ifelse(posteriorPlotType == "RelativeRisk", "rrisk",
          ifelse(posteriorPlotType == "AbsoluteRisk", "arisk",
          ifelse(posteriorPlotType == "p1&p2",        "p1p2")))))

  plotFunc <- function() {
      abtest::plot_posterior(x = ab_obj, what = what, hypothesis = "H1")
  }

  return(plotFunc)
}


.abTestPlotSequential <- function(jaspResults, ab_obj, ready) {

  abTestSequentialPlot <- createJaspPlot(title = "Sequential Analysis",  width = 530, height = 400)
  abTestSequentialPlot$dependOn(c("n1", "y1", "n2", "y2", "normal_mu", "normal_sigma", "plotSequentialAnalysis"))
  jaspResults[["abTestSequentialPlot"]] <- abTestSequentialPlot

  if (!ready)
    return()

  abTestSequentialPlot$plotObject <- .plotSequentialAnalysis.abTest(ab_obj)
}


.plotSequentialAnalysis.abTest <- function(ab_obj) {
  # Args:
  #   ab_obj: ab test object

  plotFunc <- function() {
      abtest::plot_sequential(x = ab_obj)
  }

  return (plotFunc)
}


.abTestPlotPriorOnly <- function(jaspResults, options) {

  abTestPriorPlot <- createJaspPlot(title = "Prior",  width = 530, height = 400)
  abTestPriorPlot$dependOn(c("normal_mu", "normal_sigma", "plotPriorType", "plotPriorOnly"))
  jaspResults[["abTestPriorPlot"]] <- abTestPriorPlot

  abTestPriorPlot$plotObject <- .plotPrior.abTest(options)
}


.plotPrior.abTest <- function(options) {
  # Plot the prior
  #
  # Args:
  #   options

  prior_par <- list(mu_psi = options$normal_mu, sigma_psi = options$normal_sigma, mu_beta = 0, sigma_beta = 1)

  what <- switch(
      options$plotPriorType,
      "LogOddsRatio" = "logor",
      "OddsRatio" = "or",
      "RelativeRisk" = "rrisk",
      "AbsoluteRisk" = "arisk",
      "p1&p2" = "p1p2",
      "p1" = "p1",
      "p2" = "p2"
  )

  plotFunc <- function() {
      abtest::plot_prior(prior_par = prior_par, what = what, hypothesis = "H1")
  }

  return(plotFunc)
}
