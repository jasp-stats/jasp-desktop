context("Summary Statistics Bayesian Linear Regression")

test_that("Main table results match with one model", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsRegressionLinearBayesian")
  options$sampleSize                    <- 51
  options$numberOfCovariatesNull        <- 0
  options$unadjustedRSquaredNull        <- 0
  options$numberOfCovariatesAlternative <- 4
  options$unadjustedRSquaredAlternative <- 0.82
  options$priorWidth                    <- 0.5
  options$bayesFactorType               <- "LogBF10"
  results <- jasptools::run("SummaryStatsRegressionLinearBayesian", "debug.csv", options)
  
  table <- results[["results"]][["regressionTable"]][["data"]]
  # fill in expected values
  expect_equal_tables(table, list(31.7268858756927, 0.82, 1.40664186301779e-06, 4, 51))
})

test_that("Main table results match with model comparison", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsRegressionLinearBayesian")
  options$sampleSize                    <- 51
  options$numberOfCovariatesNull        <- 2
  options$unadjustedRSquaredNull        <- 0.62
  options$numberOfCovariatesAlternative <- 4
  options$unadjustedRSquaredAlternative <- 0.82
  options$priorWidth                    <- 0.5
  options$bayesFactorType               <- "LogBF10"
  results <- jasptools::run("SummaryStatsRegressionLinearBayesian", "debug.csv", options)
  
  table <- results[["results"]][["regressionTable"]][["data"]]
  # fill in expected values
  expect_equal_tables(table, list(18.7379856112487, 0.62, 2.01408039564443e-06, 2, "Null model",
                                  31.7268858756927, 0.82, 1.40664186301779e-06, 4, "Alternative model"
                                  ))
})