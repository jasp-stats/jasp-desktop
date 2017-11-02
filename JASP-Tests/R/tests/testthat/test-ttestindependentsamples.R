context("Independent Samples TTest")

# does not test
# - missing values exclusion
# - error handling of plots

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$welchs <- TRUE
  options$mannWhitneyU <- TRUE
  options$meanDifference <- TRUE
  options$effectSize <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- JASPTools::run("TTestIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", "Student", 98, 0.448976320466698, 0.163364220743842,
         0.15401876311258, -0.263105943067512, 0.589834384555196, 0,
         0, 0.214904085649005, 0.760172707980336, 1, "contNormal", "Welch",
         93.4114683704755, 0.441326472332004, 0.163364220743842, 0.155340050635411,
         -0.256150680877671, 0.582879122365355, 0, 0, 0.211269449004155,
         0.773250564688269, 1, "contNormal", "Mann-Whitney", "", 0.617539087467476,
         0.0932984248674163, 0.0591133004926108, -0.269303374938764,
         0.553341045241524, -0.169577908162339, 0.281763520076616, "",
         1290, 1)
  )
})

test_that("Normality table matches", {
  options <- JASPTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$normalityTests <- TRUE
  results <- JASPTools::run("TTestIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["assumptionChecks"]][["shapiroWilk"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0, 0.933547444665698, 0.00342000811150064, "TRUE",
         "contNormal", 1, 0.972586424088514, 0.401705854633909, "FALSE")
  )
})

test_that("Equality of variances table matches", {
  options <- JASPTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$equalityOfVariancesTests<- TRUE
  results <- JASPTools::run("TTestIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["assumptionChecks"]][["levene"]][["data"]]
  expect_equal_tables(table, list("contNormal", 0.474760708390762, 1, 0.492433247088434))
})

test_that("Descriptives table matches", {
  options <- JASPTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptives <- TRUE
  results <- JASPTools::run("TTestIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["descriptives"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0, 58, -0.120135614827586, 1.10575982846952, 0.145193378675912,
         "TRUE", "contNormal", 1, 42, -0.283499835571429, 0.994612407217046,
         0.15347202634745)
  )
})

test_that("Descriptives plot matches", {
  options <- JASPTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptivesPlots <- TRUE
  results <- JASPTools::run("TTestIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestIndependentSamples")
})

test_that("Analysis handles errors", {
  options <- JASPTools::analysisOptions("TTestIndependentSamples")

  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- JASPTools::run("TTestIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- JASPTools::run("TTestIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- JASPTools::run("TTestIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- JASPTools::run("TTestIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_is(results, "expectedError", label="1-level factor check")
})
