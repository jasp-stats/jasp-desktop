context("Bain Independent Samples T-Test")

options <- jaspTools::analysisOptions("BainTTestBayesianIndependentSamples")
options$bayesFactorPlot <- TRUE
options$bayesFactorType <- "BF01"
options$descriptives <- TRUE
options$descriptivesPlots <- TRUE
options$groupingVariable <- "sex"
options$hypothesis <- "equalBiggerSmaller"
options$variables <- list("age")
set.seed(1)
results <- jaspTools::run("BainTTestBayesianIndependentSamples", "sesame.csv", options)

test_that("Bain Independent Samples Welch's T-Test table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bainTable"]][["data"]]
  expect_equal_tables(table,
                      list("", 5.73744721273446, 29.3003302598868, "age", 0.827524901085719,
                           0.144232246572831, 0.0282428523414506, "H0: Equal", "H1: Bigger",
                           "H2: Smaller"))
})

test_that("BF age plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorPlots"]][["collection"]][["bainContainer_bayesFactorPlots_age"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "BF age", dir="BainTTestBayesianIndependentSamples")
})

test_that("Descriptives age plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["collection"]][["bainContainer_descriptivesPlots_age"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "descriptives age", dir="BainTTestBayesianIndependentSamples")
})

test_that("Descriptive Statistics table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
  expect_equal_tables(table,
                      list(115, 1, 50.3127588370044, 51.4260869565217, 6.0914966178275, 0.568034988550375,
                           52.5394150760391, "age", 125, 2, 49.497526347968, 50.632, 6.47144596695559,
                           0.578823723793182, 51.766473652032, ""))
})