context("Discover Distributions - Poisson")

options <- jasptools::analysisOptions("LDpoisson")
options$.meta <- list(newVariableName = list(containsColumn = TRUE), variable = list(
  containsColumn = TRUE))
options$chiSquare <- TRUE
options$ciInterval <- TRUE
options$ciIntervalInterval <- 0.95
options$ecdf <- TRUE
options$estCDF <- TRUE
options$estPMF <- TRUE
options$explanatoryText <- TRUE
options$highlightDensity <- TRUE
options$highlightProbability <- TRUE
options$histogram <- TRUE
options$max_x <- 8
options$methodMLE <- TRUE
options$min <- 2
options$moments <- TRUE
options$newVariableName <- ""
options$outputEstimates <- TRUE
options$outputSE <- TRUE
options$parsSupportMoments <- TRUE
options$plotCMF <- TRUE
options$ppplot <- TRUE
options$qqplot <- TRUE
options$summary <- TRUE
options$variable <- "Pois100(lambda=1)"
set.seed(1)
results <- jasptools::run("LDpoisson", "Distributions.csv", options)


test_that("Empirical Cumulative Distribution plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_ecdf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "empirical-cumulative-distribution", dir="LDpoisson")
})

test_that("Bar plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bar-plot", dir="LDpoisson")
})

test_that("Observed Moments table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_moments"]][["data"]]
  expect_equal_tables(table,
                      list(0.95, 1, 0.95, 1.0275, 2, 1.93))
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  expect_equal_tables(table,
                      list(4, 0.95, 1, 0, 0, 1, 100, 1.01876336206147, 1.03787878787879,
                           "Pois100(lambda=1)"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.950001225643958, 0.75896753204467, "<unicode>", 0.0974679612003779,
                           1.14103491924325))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDpoisson")
})

test_that("Histogram vs. Theoretical PMF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPMF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "histogram-vs-theoretical-pmf", dir="LDpoisson")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.283426175463762, 5.03794538367432, "Chi-square"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "p-p-plot", dir="LDpoisson")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "q-q-plot", dir="LDpoisson")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCMF"]][["collection"]][["plotCMF_cmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDpoisson")
})

test_that("Probability Mass Plot matches", {
  plotName <- results[["results"]][["plotPMF"]][["collection"]][["plotPMF_pmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "probability-mass-plot", dir="LDpoisson")
})