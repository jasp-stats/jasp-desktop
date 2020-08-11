context("Bayesian Independent Samples T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - informed prior Normal/t distributions
# - error handling of plots

getTtestTable <- function(x) x[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]]
getDescriptivesTable <- function(x) x[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]]

test_that("Main table results match for Student test", {
  set.seed(0)
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$hypothesis <- "groupOneGreater"
  options$effectSizeStandardized <- "informative"
  options$informativeCauchyLocation <- 1
  options$informativeCauchyScale <- 0.5
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  expect_equal_tables(table, list(0.123677493243643, 0.0217437890351034, "contNormal"))
})

test_that("Main table results match for Wilcoxon test", {
  set.seed(0)
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$testStatistic <- "Wilcoxon"
  options$wilcoxonSamplesNumber <- 100
  options$hypothesis <- "groupOneGreater"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  expect_equal_tables(table, list(0.277730185316155, 1290, 1.00447417926979, "contNormal"))
})

test_that("Inferential and descriptives plots match", {
  set.seed(0)
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- FALSE
  
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
  
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- FALSE
  
  options$descriptives <- TRUE
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.90
  
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestBayesianIndependentSamples")
  
  table <- getDescriptivesTable(results)[["data"]]
  expect_equal_tables(table,
    list(58, 0, -0.362903138386961, -0.120135614827586, 1.10575982846952,
         0.145193378675912, 0.122631908731789, "contNormal", 42, 1, -0.541774532654284,
         -0.283499835571428, 0.994612407217046, 0.15347202634745, -0.0252251384885728,
         "contNormal"),
    label = "Descriptives table"
  )

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianIndependentSamples")
  
  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  expect_equal_plots(testPlot, "robustness-check", dir="TTestBayesianIndependentSamples")

  testPlot <- results[["state"]][["figures"]][[4]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis", dir="TTestBayesianIndependentSamples")
  
})

test_that("Inferential plots with additional info match", {
  set.seed(0)
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contcor1"
  options$groupingVariable <- "facGender"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- TRUE
  
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- TRUE

  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-additional", dir="TTestBayesianIndependentSamples")
  
  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "robustness-check-additional", dir="TTestBayesianIndependentSamples")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-additional", dir="TTestBayesianIndependentSamples")
  
  
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")

  options$variables <- c("debInf", "debSame")
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "variance check")
  expect_null(results[["results"]][["errorMessage"]])

  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  msg <- results[["results"]][["errorMessage"]]
  expect_true(any(grepl("levels", msg, ignore.case=TRUE)), label = "1-level factor check")
})

test_that("Analysis handles integer overflow", {

  set.seed(4491)
  dat <- data.frame(dependent_var = rnorm(2e5),
                    grouping      = rep(c(1, 2), each = 1e5))

  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "dependent_var"
  options$groupingVariable <- "grouping"
  results <- jasptools::run("TTestBayesianIndependentSamples", dat, options)

  table <- getTtestTable(results)[["data"]]
  # the error statistic differs between osx <-> windows. if anyone can figure out why i'd be interested (especially because the BF is the same)
  if (identical(.Platform$OS.type, "windows"))
     expect_equal_tables(table, list(0.00511047754408505, 0.309808326755948, "dependent_var"))
  else
     expect_equal_tables(table, list(0.00511047754408505, 0.311958523148014, "dependent_var"))
})

# all combinations of hypotheses and Bayes factor type
options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
options$groupingVariable <- "Rotation"
options$plotBayesFactorRobustness <- TRUE
options$plotPriorAndPosterior <- TRUE
options$plotSequentialAnalysis <- TRUE
options$variables <- list("mean_NEO")

hypotheses <- c("groupsNotEqual", "groupOneGreater", "groupTwoGreater")
bftypes <- c("BF10", "BF01", "LogBF10")

# uncomment this function to generate tables
# makeTables <- function(tables) {
#   for (r in rownames(tables)) for (c in colnames(tables))
#     cat(sprintf("tables[[\"%s\", \"%s\"]] <- %s\n", r, c, tables[[r, c]]))
# }

tables <- matrix(list(), 3, 3, dimnames = list(bftypes, hypotheses))
tables[["BF10", "groupsNotEqual"]]     <- list(0.269544340467379, 0.0322341868191384, "mean_NEO")
tables[["BF10", "groupOneGreater"]]    <- list(0.129051353111555, 0.00903931757819941, "mean_NEO")
tables[["BF10", "groupTwoGreater"]]    <- list(0.4100373278359, 0.0140038714457594, "mean_NEO")
tables[["BF01", "groupsNotEqual"]]     <- list(3.70996474370799, 0.0322341868191384, "mean_NEO")
tables[["BF01", "groupOneGreater"]]    <- list(7.74885327343739, 0.00903931757819941, "mean_NEO")
tables[["BF01", "groupTwoGreater"]]    <- list(2.43880235313651, 0.0140038714457594, "mean_NEO")
tables[["LogBF10", "groupsNotEqual"]]  <- list(-1.31102237353052, 0.0322341868191384, "mean_NEO")
tables[["LogBF10", "groupOneGreater"]] <- list(-2.04754486769964, 0.00903931757819941, "mean_NEO")
tables[["LogBF10", "groupTwoGreater"]] <- list(-0.891507079925797, 0.0140038714457594, "mean_NEO")


# original
tables[["BF01", "groupTwoGreater"]] <- list(2.43880235313651, 0.0140038714457594, "mean_NEO")

set.seed(1)
for (hypo in hypotheses) {

  options$hypothesis <- hypo

  for (bftype in bftypes) {

    options$bayesFactorType <- bftype

    results <- jasptools::run("TTestBayesianIndependentSamples", "Kitchen Rolls", options)
    # run once with this uncommented to generate tables, afterwards call makeTables(tables)
    # tables[[bftype, hypo]] <<- makeTestTable(getTtestTable(results)[["data"]], print = FALSE)

    test_that(sprintf("%s-%s Prior and Posterior plot matches", hypo, bftype), {
      plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO_plotPriorAndPosterior"]][["data"]]
      testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
      expect_equal_plots(testPlot, sprintf("%s-%s-prior-and-posterior", hypo, bftype), dir="TTestBayesianIndependentSamples")
    })

    test_that(sprintf("%s-%s Bayes Factor Robustness Check plot matches", hypo, bftype), {
      plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO_plotRobustness"]][["data"]]
      testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
      expect_equal_plots(testPlot, sprintf("%s-%s-bayes-factor-robustness-check", hypo, bftype), dir="TTestBayesianIndependentSamples")
    })

    test_that(sprintf("%s-%s Sequential Analysis plot matches", hypo, bftype), {
      plotName <- results[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_inferentialPlots"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO"]][["collection"]][["ttestContainer_inferentialPlots_mean_NEO_plotSequential"]][["data"]]
      testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
      expect_equal_plots(testPlot, sprintf("%s-%s-sequential-analysis", hypo, bftype), dir="TTestBayesianIndependentSamples")
    })

    test_that(sprintf("%s-%s Bayesian Independent Samples T-Test table results match", hypo, bftype), {
      table <- getTtestTable(results)[["data"]]
      expect_equal_tables(table, tables[[bftype, hypo]])
    })
  }
}
