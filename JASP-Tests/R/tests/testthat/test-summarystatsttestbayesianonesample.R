context("Summary Statistics Bayesian One Sample T-Test")

test_that("Main table results match", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsTTestBayesianOneSample")
  options$tStatistic      <- 2.3
  options$n1Size          <- 23
  options$bayesFactorType <- "LogBF10"
  options$hypothesis      <- "greaterThanTestValue"
  results <- jasptools::run("SummaryStatsTTestBayesianOneSample", "debug.csv", options)
  
  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  expect_equal_tables(table, list(1.32450670641619, 3.95778537281638e-05, 23,  0.015654342509863, 2.3))
})

test_that("BF for Informed and Default Prior Matches", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsTTestBayesianOneSample")
  options$tStatistic                        <- 2.3
  options$n1Size                            <- 23
  options$bayesFactorType                   <- "LogBF10"
  options$hypothesis                        <- "greaterThanTestValue"
  options$effectSizeStandardized            <- "informative"
  options$informativeStandardizedEffectSize <- "cauchy"
  results <- jasptools::run("SummaryStatsTTestBayesianOneSample", "debug.csv", options)
  
  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  BF    <- table[[1]]$BF
  expect_equal(BF, 1.32450670641619)
})

options <- jasptools::analysisOptions("SummaryStatsTTestBayesianOneSample")
options$tStatistic <- 6
options$n1Size <- 20
options$plotPriorAndPosterior <- TRUE
options$plotBayesFactorRobustness <- TRUE
options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
set.seed(1)
results <- jasptools::run("SummaryStatsTTestBayesianOneSample", "debug.csv", options)


test_that("Bayes Factor Robustness Check plot matches", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_BayesFactorRobustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsTTestBayesianOneSample")
})

test_that("Bayesian One Sample T-Test table results match", {
  table <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]][["data"]]
  expect_equal_tables(table,
                      list(2428.05675490473, 1.99678979420922e-06, 20, 8.97923547249084e-06,
                           6))
})

test_that("Default Prior and Posterior plot matches", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsTTestBayesianOneSample")
})

options <- jasptools::analysisOptions("SummaryStatsTTestBayesianOneSample")
options$tStatistic <- 6
options$n1Size <- 20
options$plotPriorAndPosterior <- TRUE
options$plotBayesFactorRobustness <- TRUE
options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
options$informativeCauchyLocation <- 1
options$effectSizeStandardized <- "informative"
set.seed(1)
results <- jasptools::run("SummaryStatsTTestBayesianOneSample", "debug.csv", options)

test_that("Informative Cauchy Prior and Posterior plot matches", {
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_priorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior-informative", dir="SummaryStatsTTestBayesianOneSample")
})
