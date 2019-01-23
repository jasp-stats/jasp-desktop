context("One Sample TTest")

# does not test
# - missing values exclusion
# - error handling of plots

test_that("Main table results match for t-test", {
  options <- jasptools::analysisOptions("TTestOneSample")
  options$variables <- "contGamma"
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jasptools::run("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table,
    list("contGamma", 99, 1.08315413981152e-23, 2.03296079621, 1.32664189226908,
         1.72889718286736, 2.33702440955264, 0, 0, 13.2664189226908,
         6.42284229078859e+20, "Student")
  )
})

test_that("Main table results match for Wilcoxon signed rank", {
  options <- jasptools::analysisOptions("TTestOneSample")
  options$variables <- "contGamma"
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$effSizeConfidenceIntervalCheckbox <- TRUE
  options$students <- FALSE
  options$mannWhitneyU <- TRUE

  results <- jasptools::run("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]

  expect_equal_tables(table,
                      list("contGamma", "", 3.955912e-18, 1.813483, 1,
                           1.553513, 2.158945, 1, 1, 5050, "Wilcoxon"))
})

test_that("Main table results match for Z-test", {
  options <- jasptools::analysisOptions("TTestOneSample")
  options$variables <- "contNormal"
  options$students <- FALSE
  options$zTest <- TRUE
  options$stddev <- 1.5
  options$effectSize <- TRUE
  options$effSizeConfidenceIntervalCheckbox <- TRUE
  results <- jasptools::run("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table,
                      list("contNormal", "", 0.2082746, -0.1887486, -0.1258324,
                          -0.4827432, 0.105246, -0.3218288, 0.07016401, -1.258324,
                          "Z")
                      )
})

test_that("Normality table matches", {
  options <- jasptools::analysisOptions("TTestOneSample")
  options$variables <- "contGamma"
  options$normalityTests <- TRUE
  results <- jasptools::run("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["assumptionChecks"]][["shapiroWilk"]][["data"]]
  expect_equal_tables(table, list("contGamma", 0.876749741598208, 1.32551553117109e-07, "TRUE"))
})

test_that("Descriptives table matches", {
  options <- jasptools::analysisOptions("TTestOneSample")
  options$variables <- "contGamma"
  options$descriptives <- TRUE
  results <- jasptools::run("TTestOneSample", "test.csv", options)
  table <- results[["results"]][["descriptives"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list("contGamma", 100, 2.03296079621, 1.53241112621044, 0.153241112621044,
         "TRUE")
  )
})

test_that("Descriptives plot matches", {
  options <- jasptools::analysisOptions("TTestOneSample")
  options$variables <- "contGamma"
  options$descriptivesPlots <- TRUE
  results <- jasptools::run("TTestOneSample", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestOneSample")
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("TTestOneSample")

  options$variables <- "debInf"
  results <- jasptools::run("TTestOneSample", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  results <- jasptools::run("TTestOneSample", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- "debMiss99"
  results <- jasptools::run("TTestOneSample", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
