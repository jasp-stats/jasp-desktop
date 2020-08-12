context("Bayesian Correlation")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - errors whilst plotting

test_that("Bayesian Correlation Table results match", {
  options <- jaspTools::analysisOptions("CorrelationBayesian")
  options$variables <- c("contcor1", "contcor2")
  options$kendall <- TRUE
  options$flagSupported <- TRUE
  options$ci <- TRUE
  options$kappa <- 1.5
  set.seed(1)
  results <- jaspTools::run("CorrelationBayesian", "test.csv", options)
  table <- results[["results"]][["corBayesTable"]][["data"]]
  expect_equal_tables(table,
                      list("TRUE", "<unicode>", "Pearson's r", "1. contcor1", "FALSE", "<unicode>",
                           "BF<unicode><unicode>", "", "FALSE", "<unicode>", "Upper 95% CI",
                           "", "FALSE", "<unicode>", "Lower 95% CI", "", "FALSE", "<unicode>",
                           "Kendall's tau", "", "FALSE", "<unicode>", "BF<unicode><unicode>",
                           "", "FALSE", "<unicode>", "Upper 95% CI", "", "FALSE", "<unicode>",
                           "Lower 95% CI", "", 1, "TRUE", 0.657010063712354, "<unicode>",
                           "Pearson's r", "2. contcor2", "FALSE", 71191291327.7135, "<unicode>",
                           "BF<unicode><unicode>", "", "FALSE", 0.753487301516098, "<unicode>",
                           "Upper 95% CI", "", "FALSE", 0.524567068817432, "<unicode>",
                           "Lower 95% CI", "", 1, "FALSE", 0.503030303030303, "<unicode>",
                           "Kendall's tau", "", "FALSE", 78878934747.8062, "<unicode>",
                           "BF<unicode><unicode>", "", "FALSE", 0.621810905452726, "<unicode>",
                           "Upper 95% CI", "", "FALSE", 0.36168084042021, "<unicode>",
                           "Lower 95% CI", ""))
})

# Note(Alexander): The difference in the Kendall's version is quite small. The differences in ci is due to me using
# a larger number of samplers.
# 
# test_that("Main table results match", {
#   options <- jaspTools::analysisOptions("CorrelationBayesian")
#   options$variables <- c("contcor1", "contcor2")
#   options$kendallsTauB <- TRUE
#   options$reportBayesFactors <- TRUE
#   options$flagSupported <- TRUE
#   options$credibleInterval <- TRUE
#   options$priorWidth <- 1.5
#   results <- jaspTools::run("CorrelationBayesian", "test.csv", options)
#   table <- results[["results"]][["correlations"]][["data"]]
#   expect_equal_tables(table,
#     list("Pearson's r", "<unicode>", "", "BF<unicode><unicode>", "<unicode>",
#          "", "Upper 95% CI", "<unicode>", "", "Lower 95% CI", "<unicode>",
#          "", "Kendall's tau", "<unicode>", "", "BF<unicode><unicode>",
#          "<unicode>", "", "Upper 95% CI", "<unicode>", "", "Lower 95% CI",
#          "<unicode>", "", "contcor1", "Pearson's r", 0.657010063712354,
#          "<unicode>", "BF<unicode><unicode>", 71191291327.7147, "<unicode>",
#          "Upper 95% CI", 0.753487301516087, "<unicode>", "Lower 95% CI",
#          0.524567068817447, "<unicode>", "Kendall's tau", 0.503030303030303,
#          "<unicode>", "BF<unicode><unicode>", 78878934747.8062, "<unicode>",
#          "Upper 95% CI", 0.62124248496994, "<unicode>", "Lower 95% CI",
#          0.360721442885771, "<unicode>", "contcor2", "***", "***")
#   )
# })


test_that("Bayesian Correlation Matrix Plot matches", {
  options <- jaspTools::analysisOptions("CorrelationBayesian")
  options$variables <- c("contcor1", "contcor2")
  options$plotMatrix <- TRUE
  options$plotMatrixDensities <- TRUE
  options$plotMatrixPosteriors <- TRUE
  options$kappa <- 1.0
  set.seed(1)
  results <- jaspTools::run("CorrelationBayesian", "test.csv", options)
  plotName <- results[["results"]][["matrixPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayesian-correlation-matrix-plot", dir="CorrelationBayesian")
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("CorrelationBayesian")

  options$variables <- c("contNormal", "debInf")
  results <- jaspTools::run("CorrelationBayesian", "test.csv", options)
  notes <- unlist(results[["results"]][["corBayesTable"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- c("contNormal", "debSame")
  results <- jaspTools::run("CorrelationBayesian", "test.csv", options)
  notes <- unlist(results[["results"]][["corBayesTable"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- c("contNormal", "debMiss99")
  results <- jaspTools::run("CorrelationBayesian", "test.csv", options)
  notes <- unlist(results[["results"]][["corBayesTable"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})


# Bayesian correlation pairs 
test_that("Bayesian Pearson Correlation PAIRWISE table results match", {
  options <- jaspTools::analysisOptions("CorrelationBayesian")
  options$variables <- list("contcor1", "contcor2")
  options$displayPairwise <- TRUE
  options$ci <- TRUE
  options$kappa <- 2
  set.seed(1)
  results <- jaspTools::run("CorrelationBayesian", "test.csv", options)
  table <- results[["results"]][["corBayesTable"]][["data"]]
  expect_equal_tables(table,
                      list(64323420095.6647, 0.526024457777766, 0.657010063712354, 0.754530668995199,
                           "-", "contcor1", "contcor2"))
})

# Pairs plot
options <- jaspTools::analysisOptions("CorrelationBayesian")
options$variables <- list("contcor1", "contcor2")
options$pairs <- list(c("contcor1", "contcor2"))
options$plotPriorPosterior <- TRUE
options$plotBfRobustness <- TRUE
options$plotBfSequential <- TRUE
options$kappa <- 1.0
set.seed(1)
results <- jaspTools::run("CorrelationBayesian", "test.csv", options)

test_that("Bayes Factor Robustness Check plot matches", {
  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_contcor1-contcor2"]][["collection"]][["pairsPlotCollection_contcor1-contcor2_plotBfRobustness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="CorrelationBayesian")
})

test_that("Sequential Analysis plot matches", {
  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_contcor1-contcor2"]][["collection"]][["pairsPlotCollection_contcor1-contcor2_plotBfSequential"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis", dir="CorrelationBayesian")
})

test_that("Prior and Posterior plot matches", {
  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_contcor1-contcor2"]][["collection"]][["pairsPlotCollection_contcor1-contcor2_plotPriorPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior", dir="CorrelationBayesian")
})

test_that("Scatterplot matches", {
  plotName <- results[["results"]][["pairsPlotCollection"]][["collection"]][["pairsPlotCollection_contcor1-contcor2"]][["collection"]][["pairsPlotCollection_contcor1-contcor2_plotScatter"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "scatterplot", dir="CorrelationBayesian")
})