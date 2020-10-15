context("Summary Statistics Bayesian Independent Samples T-Test")

options <- jaspTools::analysisOptions("SummaryStatsTTestBayesianIndependentSamples")
options$tStatistic <- 2.3
options$n1Size <- 20
options$n2Size <- 20
options$plotPriorAndPosterior <- TRUE
options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
options$hypothesis <- "groupTwoGreater"
options$informativeCauchyLocation <- 1
options$effectSizeStandardized <- "informative"
set.seed(1)
results <- jaspTools::run("SummaryStatsTTestBayesianIndependentSamples", "debug.csv", options)


test_that("Prior and Posterior plot matches", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsTTestBayesianIndependentSamples")
})

test_that("Bayesian Independent Samples T-Test table results match", {
  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.08420871, 0.001761993, 20, 20, 0.986483662535044,
                           2.3))
})

options <- jaspTools::analysisOptions("SummaryStatsTTestBayesianIndependentSamples")
options$hypothesis <- "groupTwoGreater"
options$tStatistic <- 2.3
options$n1Size <- 20
options$n2Size <- 20
options$plotPriorAndPosterior <- TRUE
options$plotBayesFactorRobustnessAdditionalInfo <- TRUE
options$plotBayesFactorRobustness <- TRUE
options$plotBayesFactorRobustnessAdditionalInfo <- TRUE
set.seed(1)
results <- jaspTools::run("SummaryStatsTTestBayesianIndependentSamples", "debug.csv", options)

test_that("Bayes Factor Robustness Check plot matches", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_BayesFactorRobustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsTTestBayesianIndependentSamples")
})

test_that("Prior and Posterior plot matches", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior-2", dir="SummaryStatsTTestBayesianIndependentSamples")
})


options <- jasptools::analysisOptions("SummaryStatsTTestBayesianIndependentSamples")
options$hypothesis <- "groupTwoGreater"
options$tStatistic <- 2.3
options$n1Size <- 20
options$n2Size <- 20
options$plotPriorAndPosterior <- TRUE
options$plotBayesFactorRobustnessAdditionalInfo <- TRUE
options$plotBayesFactorRobustness <- TRUE
options$plotBayesFactorRobustnessAdditionalInfo <- TRUE
options$bayesFactorType = "BF01"
options$plotPriorAndPosteriorAdditionalInfo <- TRUE
set.seed(1)
results <- jasptools::run("SummaryStatsTTestBayesianIndependentSamples", "debug.csv", options)


test_that("BF Type Label Switches Correctly Robustness Check", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_BayesFactorRobustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factor-robustness-check-2", dir="SummaryStatsTTestBayesianIndependentSamples")
})

test_that("BF Type Label Switches Correctly Prior Posterior", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior-3", dir="SummaryStatsTTestBayesianIndependentSamples")
})


