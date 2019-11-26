context("Binomial Test Bayesian")

addFormulaFieldOptions <- function(options) {
  options$priorA    <- list("1",   "T")
  options$priorB    <- list("1",   "T")
  options$testValue <- list("0.5", "T")
  return(options)
}

test_that("Main table results match", {
  options <- jasptools::analysisOptions("BinomialTestBayesian")
  options$variables <- "contBinom"
  options$bayesFactorType <- "BF01"
  options$hypothesis <- "notEqualToTestValue"
  options$priorA <- list("4", "T")
  options$priorB <- list("2", "T")
  options$testValue <- list("0.2", "T")
  results <- jasptools::run("BinomialTestBayesian", "test.csv", options)
  table <- results[["results"]][["binomTable"]][["data"]]
  expect_equal_tables(table,
    list("TRUE", 4.32337507642424e-15, "contBinom", 58, 0, 0.58, 100,
         "FALSE", 3.43240614623212e-05, "contBinom", 42, 1, 0.42, 100)
  )
})

test_that("Prior posterior plots match", {
  options <- jasptools::analysisOptions("BinomialTestBayesian")
  options <- addFormulaFieldOptions(options)
  options$variables <- "contBinom"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  results <- jasptools::run("BinomialTestBayesian", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-1", dir="BinomialTestBayesian")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-2", dir="BinomialTestBayesian")
})

test_that("Sequential analysis plots match", {
  options <- jasptools::analysisOptions("BinomialTestBayesian")
  options <- addFormulaFieldOptions(options)
  options$variables <- "contBinom"
  options$plotSequentialAnalysis <- TRUE
  results <- jasptools::run("BinomialTestBayesian", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-1", dir="BinomialTestBayesian")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-2", dir="BinomialTestBayesian")
})
