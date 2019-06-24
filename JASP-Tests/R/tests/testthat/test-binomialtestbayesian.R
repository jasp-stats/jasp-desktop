context("Binomial Test Bayesian")

test_that("Main table results match", {
  options <- jasptools::analysisOptions("BinomialTestBayesian")
  options$variables <- "contBinom"
  options$bayesFactorType <- "BF01"
  options$hypothesis <- "notEqualToTestValue"
  options$priorA <- 4
  options$priorB <- 2
  options$testValue <- 0.2
  results <- jasptools::run("BinomialTestBayesian", "test.csv", options)
  table <- results[["results"]][["binomTable"]][["data"]]
  expect_equal_tables(table,
    list("TRUE", 4.32337507642424e-15, "contBinom", 58, 0, 0.58, 100,
         "FALSE", 3.43240614623212e-05, "contBinom", 42, 1, 0.42, 100)
  )
})

test_that("Prior posterior plots match", {
  options <- jasptools::analysisOptions("BinomialTestBayesian")
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
  options$variables <- "contBinom"
  options$plotSequentialAnalysis <- TRUE
  jaspResults:::.plotStateStorage$plot_1
  results <- jasptools::run("BinomialTestBayesian", "test.csv", options)

  testpath <- (results[["results"]][["bayesianBinomPlots"]][["collection"]]
  [["bayesianBinomPlots_deeperBayesianBinomPlotscontBinom1"]][["collection"]]
  [["bayesianBinomPlots_deeperBayesianBinomPlotscontBinom1_contBinom1sequential"]][["data"]])
  testPlot <- results[["state"]][["figures"]][[testpath]][["obj"]]
  #print(names(results[["state"]][["figures"]]))
  expect_equal_plots(testPlot, "sequential-analysis-1", dir="BinomialTestBayesian")

  testpath <- (results[["results"]][["bayesianBinomPlots"]][["collection"]]
               [["bayesianBinomPlots_deeperBayesianBinomPlotscontBinom2"]][["collection"]]
               [["bayesianBinomPlots_deeperBayesianBinomPlotscontBinom2_contBinom2sequential"]][["data"]])
  testPlot <- results[["state"]][["figures"]][[testpath]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-2", dir="BinomialTestBayesian")
})
