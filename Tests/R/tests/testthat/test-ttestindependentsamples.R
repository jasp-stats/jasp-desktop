context("Independent Samples TTest")

# does not test
# - missing values exclusion
# - error handling of plots

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$welchs <- TRUE
  options$mannWhitneyU <- TRUE
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$VovkSellkeMPR <- TRUE
  options$effSizeConfidenceIntervalCheckbox <- TRUE
  results <- jaspTools::run("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table,
    list("TRUE", 0.760172707980336, 1, 0.15401876311258, 98, -0.244064746209808,
	       0.163364220743842, 0.448976320466698, 0.214904085649005, "Student",
	       0.551319670653115, "contNormal", "FALSE", 0.773250564688269,
	       1, 0.155340050635411, 93.4114683704755, -0.242805192811962,
	       0.163364220743842, 0.441326472332004, 0.211269449004155, "Welch",
	       0.552657418835939, "contNormal", "FALSE", 1290, 1, 0.0591133004926108,
	       "", -0.169577908162339, 0.0932984248674163, 0.617539087467476,
	       "", "Mann-Whitney", 0.281763520076616, "contNormal")
  )
})

test_that("Normality table matches", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$normalityTests <- TRUE
  results <- jaspTools::run("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_ttestNormalTable"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0, 0.933547444665698, 0.00342000811150064, "TRUE",
         "contNormal", 1, 0.972586424088514, 0.401705854633909, "FALSE")
  )
})

test_that("Equality of variances table matches", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$equalityOfVariancesTests<- TRUE
  results <- jaspTools::run("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["AssumptionChecks"]][["collection"]][["AssumptionChecks_equalityVariance"]][["data"]]
  expect_equal_tables(table, list("contNormal", 0.474760708390762, 1, 0.492433247088434))
})

test_that("Descriptives table matches", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptives <- TRUE
  results <- jaspTools::run("TTestIndependentSamples", "test.csv", options)
  table <- results[["results"]][["ttestDescriptives"]][["collection"]][["ttestDescriptives_table"]][["data"]]
  expect_equal_tables(table,
    list("TRUE", 58, 0, -0.120135614827586, 1.10575982846952, 0.145193378675912,
	       "contNormal", "FALSE", 42, 1, -0.283499835571428, 0.994612407217046,
	       0.15347202634745, "contNormal")
  )
})

test_that("Descriptives plot matches", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptivesPlots <- TRUE
  results <- jaspTools::run("TTestIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestIndependentSamples")
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  
  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- jaspTools::run("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")
  
  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- jaspTools::run("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")
  
  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- jaspTools::run("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
  
  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- jaspTools::run("TTestIndependentSamples", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError", label = "1-level factor check")
})

