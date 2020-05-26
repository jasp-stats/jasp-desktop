context("Summary Statistics Bayesian Paired Samples T-Test")

options <- jasptools::analysisOptions("SummaryStatsTTestBayesianPairedSamples")
options$tStatistic <- 2.3
options$n1Size <- 20
options$plotPriorAndPosterior <- TRUE
options$plotBayesFactorRobustness <- TRUE
options$hypothesis <- "groupTwoGreater"
set.seed(1)
results <- jasptools::run("SummaryStatsTTestBayesianPairedSamples", "debug.csv", options)


test_that("Bayes Factor Robustness Check plot matches", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_BayesFactorRobustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsTTestBayesianPairedSamples")
})

test_that("Bayesian Paired Samples T-Test table results match", {
  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.0821009262649839, 5.19539218807268e-05, 20, 0.983523708768303,
                           2.3))
})

test_that("Default Prior and Posterior plot matches", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsTTestBayesianPairedSamples")
})

options <- jasptools::analysisOptions("SummaryStatsTTestBayesianPairedSamples")
options$tStatistic <- 2.3
options$n1Size <- 20
options$plotPriorAndPosterior <- TRUE
options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
options$hypothesis <- "groupTwoGreater"
options$informativeCauchyLocation <- 1
options$effectSizeStandardized <- "informative"
set.seed(1)
results <- jasptools::run("SummaryStatsTTestBayesianPairedSamples", "debug.csv", options)


test_that("Bayesian Paired Samples T-Test table results match", {
  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.0644349416204716, 0.000247553379975798, 20, 0.983523708768303,
                           2.3))
})

test_that("Informative Cauchy Prior and Posterior plot matches", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior-2", dir="SummaryStatsTTestBayesianPairedSamples")
})