context("Independent Samples TTest")

# does not test
# - missing values exclusion
# - error handling of plots

test_that("Main table results match", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$welchs <- TRUE
  options$mannWhitneyU <- TRUE
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  tableStudent       <- results[["results"]][["independentSamplesTTestTableParametric"]][["data"]][[1]]
  tableWelch         <- results[["results"]][["independentSamplesTTestTableParametric"]][["data"]][[2]]
  tableNonParametric <- results[["results"]][["independentSamplesTTestTableNonParametric"]][["data"]]
  expect_equal_tables(tableStudent,
    list(1, 0.1540188, -0.2486351, 0.5566726, 98, 0.152837, -0.2467274, 0.5524015,
         0.1633642, -0.2631059, 0.5898344, 0.4489763, 0.2149041, 0.7601727, "Student", "contNormal")
  )
  expect_equal_tables(tableWelch,
    list(1, 0.1553401, -0.2473238, 0.5580039, 93.41147, 0.1541482, -0.2454262, 0.5537225,
         0.1633642, -0.2561507, 0.5828791, 0.4413265, 0.2112694, 0.7732506, "Welch", "contNormal")
  )
  expect_equal_tables(tableNonParametric,
    list(1, 0.0591133, -0.1724909, 0.2845237, 0.09329842, -0.2693034, 0.553341, 
         0.6175391, 0.0591133, -0.1695779, 0.2817635, 1290, "Mann-Whitney", "contNormal")
  )
})

test_that("Normality table matches", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$normalityTests <- TRUE
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["independentSamplesTTestAssumptionChecksContainer"]][["collection"]][["independentSamplesTTestAssumptionChecksContainer_independentSamplesTTestShapiroWilkTable"]][["data"]]
  expect_equal_tables(table,
    list(0.9335474, "contNormal", 0, 0.003420008,
         0.9725864, "contNormal", 1, 0.4017059)
  )
})

test_that("Equality of variances table matches", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$equalityOfVariancesTests<- TRUE
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["independentSamplesTTestAssumptionChecksContainer"]][["collection"]][["independentSamplesTTestAssumptionChecksContainer_independentSamplesTTestLevenesTable"]][["data"]]
  expect_equal_tables(table,
    list(0.4747607, 1, 0.4924332, "contNormal"))
})

test_that("Descriptives table matches", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptives <- TRUE
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["independentSamplesTTestDescriptivesContainer"]][["collection"]][["independentSamplesTTestDescriptivesContainer_independentSamplesTTestDescriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list(58, 0, -0.1201356, 1.10576, 0.1451934, "contNormal",
         42, 1, -0.2834998, 0.9946124, 0.153472, "contNormal")
  )
})
#
# test_that("Descriptives plot matches", {
#   options <- jasptools::analysisOptions("TTestIndependentSamples")
#   options$variables <- "contNormal"
#   options$groupingVariable <- "contBinom"
#   options$descriptivesPlots <- TRUE
#   results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "descriptives", dir="TTestIndependentSamples")
# })

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("TTestIndependentSamples")

  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  error <- list(results[["results"]][["error"]], results[["results"]][["errorMessage"]])
  expect_true(error[[1]] == TRUE && error[[2]] == "The following problem(s) occurred while running the analysis:<ul><li>Infinity found in debInf</li></ul>")

  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  error <- list(results[["results"]][["error"]], results[["results"]][["errorMessage"]])
  expect_true(error[[1]] == TRUE && error[[2]] == "The following problem(s) occurred while running the analysis:<ul><li>Variance = 0 in debSame after grouping on contBinom</li></ul>")
  
  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  error <- list(results[["results"]][["error"]], results[["results"]][["errorMessage"]])
  expect_true(error[[1]] == TRUE && error[[2]] == "The following problem(s) occurred while running the analysis:<ul><li>Number of observations < 3 in debMiss99 after grouping on contBinom</li></ul>")

  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- jasptools::run("TTestIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  error <- list(results[["results"]][["error"]], results[["results"]][["errorMessage"]])
  expect_true(error[[1]] == TRUE && error[[2]] == "The following problem(s) occurred while running the analysis:<ul><li>Number of factor levels ≠ 2 in debSame</li></ul>")
})
