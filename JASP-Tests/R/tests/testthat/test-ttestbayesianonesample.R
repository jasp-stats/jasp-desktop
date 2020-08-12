context("Bayesian One Sample T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - default cauchy and informed prior cauchy/Normal
# - error handling of plots
 
getTtestTable <- function(x) x[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]]
getDescriptivesTable <- function(x) x[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]]

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$effectSizeStandardized <- "default"
  options$defaultStandardizedEffectSize <- "cauchy"
  options$priorWidth <- 0.707
  results <- jaspTools::run("TTestBayesianOneSample", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  expect_equal_tables(table, list(0.508160332089536, "contNormal", 2.80907441042415e-05))
})

test_that("Inferential and descriptives plots match", {
  set.seed(0)
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- FALSE
  
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
  
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- FALSE

  options$descriptives <- TRUE
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.90
  
  results <- jaspTools::run("TTestBayesianOneSample", "test.csv", options)
  
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestBayesianOneSample")
  
  table <- getDescriptivesTable(results)[["data"]]
  expect_equal_tables(table,
    list(100, -0.364486647151235, -0.18874858754, 1.05841360919316, 0.105841360919316,-0.013010527928765, "contNormal"),
    label = "Descriptives table"
  )
  
  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianOneSample")
  
  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  expect_equal_plots(testPlot, "robustness-check", dir="TTestBayesianOneSample")
  
  testPlot <- results[["state"]][["figures"]][[4]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis", dir="TTestBayesianOneSample")
  
})

test_that("Inferential plots with additional info match", {
  set.seed(0)
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contcor1"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- TRUE
  
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- TRUE
  
  results <- jaspTools::run("TTestBayesianOneSample", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-additional", dir="TTestBayesianOneSample")
  
  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "robustness-check-additional", dir="TTestBayesianOneSample")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-additional", dir="TTestBayesianOneSample")
  
})

test_that("Prior and posterior plot custom CI level match", {
  options <- jasptools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contcor1"
  options$plotPriorAndPosterior <- TRUE
  
  options$priorAndPosteriorPlotsCredibleInterval <- 0.8
  
  results  <- jasptools::run("TTestBayesianOneSample", "test.csv", options)
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_contcor1"]][["collection"]][["ttestContainer_inferentialPlots_contcor1_plotPriorAndPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-ci-level-80", dir="TTestBayesianOneSample")
  
  options$priorAndPosteriorPlotsCredibleInterval <- 0.99

  results  <- jasptools::run("TTestBayesianOneSample", "test.csv", options)
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_contcor1"]][["collection"]][["ttestContainer_inferentialPlots_contcor1_plotPriorAndPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-ci-level-99", dir="TTestBayesianOneSample")
  
  options$priorAndPosteriorPlotsCredibleInterval <- 0.999
  
  results  <- jasptools::run("TTestBayesianOneSample", "test.csv", options)
  plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_contcor1"]][["collection"]][["ttestContainer_inferentialPlots_contcor1_plotPriorAndPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-ci-level-99.9", dir="TTestBayesianOneSample")
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("TTestBayesianOneSample")

  options$variables <- "debInf"

  results <- jaspTools::run("TTestBayesianOneSample", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  results <- jaspTools::run("TTestBayesianOneSample", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- "debMiss99"
  results <- jaspTools::run("TTestBayesianOneSample", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
