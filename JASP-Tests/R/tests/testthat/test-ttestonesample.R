context("One Sample TTest")

# does not test
# - missing values exclusion
# - error handling of plots

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("TTestOneSample")
  options$variables <- "contGamma"
  options$wilcoxonSignedRank <- TRUE
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- JASPTools::run("TTestOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table,
    list("contGamma", 99, 1.08315413981152e-23, 2.03296079621, 1.32664189226908,
         1.72889718286736, 2.33702440955264, 0, 0, 13.2664189226908,
         6.42284229078859e+20)
  )
})

test_that("Normality table matches", {
  options <- JASPTools::analysisOptions("TTestOneSample")
  options$variables <- "contGamma"
  options$normalityTests <- TRUE
  results <- JASPTools::run("TTestOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["assumptionChecks"]][["shapiroWilk"]][["data"]]
  expect_equal_tables(table, list("contGamma", 0.876749741598208, 1.32551553117109e-07, "TRUE"))
})

test_that("Descriptives table matches", {
  options <- JASPTools::analysisOptions("TTestOneSample")
  options$variables <- "contGamma"
  options$descriptives <- TRUE
  results <- JASPTools::run("TTestOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["descriptives"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list("contGamma", 100, 2.03296079621, 1.53241112621044, 0.153241112621044,
         "TRUE")
  )
})

test_that("Descriptives plot matches", {
  options <- JASPTools::analysisOptions("TTestOneSample")
  options$variables <- "contGamma"
  options$descriptivesPlots <- TRUE
  results <- JASPTools::run("TTestOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestOneSample")
})

test_that("Analysis handles errors", {
  options <- JASPTools::analysisOptions("TTestOneSample")

  options$variables <- "debInf"
  results <- JASPTools::run("TTestOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  results <- JASPTools::run("TTestOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- "debMiss99"
  results <- JASPTools::run("TTestOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
