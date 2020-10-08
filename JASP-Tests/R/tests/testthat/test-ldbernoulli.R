context("Discover Distributions - Bernoulli")

options <- jasptools::analysisOptions("LDbernoulli")
options$.meta <- list(newVariableName = list(containsColumn = TRUE), variable = list(
  containsColumn = TRUE))
options$ciInterval <- TRUE
options$ciIntervalInterval <- 0.95
options$estCDF <- TRUE
options$estPMF <- TRUE
options$explanatoryText <- TRUE
options$histogram <- TRUE
options$max <- 0
options$methodMLE <- TRUE
options$newVariableName <- ""
options$outputEstimates <- TRUE
options$outputSE <- TRUE
options$parsSupportMoments <- TRUE
options$plotCMF <- TRUE
options$ppplot <- TRUE
options$qqplot <- TRUE
options$summary <- TRUE
options$variable <- "Bern10(p=0.5)"
set.seed(1)
results <- jasptools::run("LDbernoulli", "Distributions.csv", options)


test_that("Bar plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bar-plot", dir="LDbernoulli")
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  expect_equal_tables(table,
                      list(6, 0, 0.6, 4, 1, 0.4, 10, "Total", "."))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.60000004149985, 0.296365213169198, "p (0)", 0.154918575405306,
                           0.903634869830503, 0.39999995850015, 0.0963651301694973, "p (1)",
                           0.154918575405306, 0.703634786830802))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDbernoulli")
})

test_that("Histogram vs. Theoretical PMF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPMF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "histogram-vs-theoretical-pmf", dir="LDbernoulli")
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "p-p-plot", dir="LDbernoulli")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCMF"]][["collection"]][["plotCMF_cmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDbernoulli")
})

test_that("Probability Mass Plot matches", {
  plotName <- results[["results"]][["plotPMF"]][["collection"]][["plotPMF_pmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "probability-mass-plot", dir="LDbernoulli")
})