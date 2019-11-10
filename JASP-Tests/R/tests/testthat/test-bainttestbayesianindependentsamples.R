context("Bain Independent Samples T-Test")

options <- jasptools::analysisOptions("BainTTestBayesianIndependentSamples")
options$bayesFactorPlot <- TRUE
options$bayesFactorType <- "BF01"
options$descriptives <- TRUE
options$descriptivesPlots <- TRUE
options$groupingVariable <- "sex"
options$hypothesis <- "allTypes"
options$variables <- list("age")
set.seed(1)
results <- jasptools::run("BainTTestBayesianIndependentSamples", "sesame.csv", options)

test_that("BF age plot matches", {
  plotName <- results[["results"]][["BFplots"]][["collection"]][["BFplots_age"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "BF age", dir="BainTTestBayesianIndependentSamples")
})

test_that("Bain Independent Samples Welch's T-Test table results match", {
  table <- results[["results"]][["bainTable"]][["data"]]
  expect_equal_tables(table,
                      list("", 5.73744721273446, 29.3003302598868, "age", 0.827524901085719,
                           0.144232246572831, 0.0282428523414506, "H0: Equal", "H1: Bigger",
                           "H2: Smaller"))
})

test_that("Descriptives plot matches", {
  plotName <- results[["results"]][["descriptivesPlots"]][["collection"]][["descriptivesPlots_age"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "descriptives age", dir="BainTTestBayesianIndependentSamples")
})

test_that("Descriptive Statistics table results match", {
  table <- results[["results"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
                      list(115, 1, 50.301, 51.4260869565217, 0.568034988550375, 52.551, "age",
                           125, 2, 49.486, 50.632, 0.578823723793182, 51.778, ""))
})