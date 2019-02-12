context("Bayesian Correlation Pairs")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - errors whilst plotting

test_that("Main table results match", {
  options <- jasptools::analysisOptions("CorrelationBayesianPairs")
  options$pairs <- list(c("contcor1", "contcor2"))
  options$corcoefficient <- "Pearson"
  options$priorWidth <- 2
  options$credibleInterval <- TRUE
  results <- jasptools::run("CorrelationBayesianPairs", "test.csv", options)
  table <- results[["results"]][["correlation"]][["data"]]
  expect_equal_tables(table,
    list("contcor1", "-", "contcor2", 0.657010063712354, 64323420095.6619,
         0.754530668995209, 0.526024457777752)
  )
})

test_that("Scatterplot matches", {
  options <- jasptools::analysisOptions("CorrelationBayesianPairs")
  options$pairs <- list(c("contcor1", "contcor2"))
  options$plotScatter <- TRUE
  results <- jasptools::run("CorrelationBayesianPairs", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "scatterplot", dir="CorrelationBayesianPairs")
})

test_that("Prior posterior plot matches", {
  options <- jasptools::analysisOptions("CorrelationBayesianPairs")
  options$pairs <- list(c("contcor1", "contcor2"))
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  results <- jasptools::run("CorrelationBayesianPairs", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior", dir="CorrelationBayesianPairs")
})

test_that("BF robustness check plot matches", {
  options <- jasptools::analysisOptions("CorrelationBayesianPairs")
  options$pairs <- list(c("contcor1", "contcor2"))
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
  results <- jasptools::run("CorrelationBayesianPairs", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "robustness-check", dir="CorrelationBayesianPairs")
})

test_that("Sequential analysis plot matches", {
  options <- jasptools::analysisOptions("CorrelationBayesianPairs")
  options$pairs <- list(c("contcor1", "contcor2"))
  options$plotSequentialAnalysis <- TRUE
  results <- jasptools::run("CorrelationBayesianPairs", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis", dir="CorrelationBayesianPairs")
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("CorrelationBayesianPairs")

  options$pairs <- list(c("contNormal", "debInf"))
  results <- jasptools::run("CorrelationBayesianPairs", "test.csv", options)
  notes <- unlist(results[["results"]][["correlation"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$pairs <- list(c("contNormal", "debSame"))
  results <- jasptools::run("CorrelationBayesianPairs", "test.csv", options)
  notes <- unlist(results[["results"]][["correlation"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$pairs <- list(c("contNormal", "debMiss99"))
  results <- jasptools::run("CorrelationBayesianPairs", "test.csv", options)
  notes <- unlist(results[["results"]][["correlation"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
