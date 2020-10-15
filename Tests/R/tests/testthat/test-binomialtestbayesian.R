context("Binomial Test Bayesian")

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("BinomialTestBayesian")
  options$variables <- "contBinom"
  options$bayesFactorType <- "BF01"
  options$hypothesis <- "notEqualToTestValue"
  options$priorA <- 1
  options$priorB <- 2
  options$testValue <- 0.2
  results <- jaspTools::run("BinomialTestBayesian", "test.csv", options)
  table <- results[["results"]][["binomTable"]][["data"]]
  expect_equal_tables(table,
    list("TRUE", 4.32337507642424e-15, "contBinom", 58, 0, 0.58, 100,
         "FALSE", 3.43240614623212e-05, "contBinom", 42, 1, 0.42, 100)
  )
})

test_that("Prior posterior plots match", {
  options <- jaspTools::analysisOptions("BinomialTestBayesian")
  options$priorA <- 1
  options$priorB <- 1
  options$testValue <- 0.5
  options$variables <- "contBinom"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  results <- jaspTools::run("BinomialTestBayesian", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-1", dir="BinomialTestBayesian")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-2", dir="BinomialTestBayesian")
})

test_that("Sequential analysis plots match - BF10", {
  options <- jaspTools::analysisOptions("BinomialTestBayesian")
  options$priorA <- 1
  options$priorB <- 1
  options$testValue <- 0.5
  options$variables <- "contBinom"
  options$plotSequentialAnalysis <- TRUE
  results <- jaspTools::run("BinomialTestBayesian", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-1", dir="BinomialTestBayesian")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-2", dir="BinomialTestBayesian")
})

test_that("Sequential analysis plots match - BF01", {
  options <- jasptools::analysisOptions("BinomialTestBayesian")
  options$priorA <- 1
  options$priorB <- 1
  options$testValue <- 0.5
  options$variables <- "contBinom"
  options$plotSequentialAnalysis <- TRUE
  options$bayesFactorType <- "BF01"
  results <- jasptools::run("BinomialTestBayesian", "test.csv", options)
  
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-3", dir="BinomialTestBayesian")
  
  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-4", dir="BinomialTestBayesian")
})