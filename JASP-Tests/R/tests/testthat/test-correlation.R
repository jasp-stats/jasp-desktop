context("Correlation")

# does not test
# - hypothesis

test_that("Correlation table results match", {
  options <- jasptools::analysisOptions("Correlation")
  options$variables <- list("contGamma", "contNormal")
  options$kendallsTauB <- TRUE
  options$spearman <- TRUE
  options$confidenceIntervals <- TRUE
  options$confidenceIntervalsInterval <- 0.99
  options$VovkSellkeMPR <- TRUE
  options$reportSignificance <- TRUE
  results <- jasptools::run("Correlation", "test.csv", options)
  table <- results[["results"]][["correlations"]][["data"]]
  expect_equal_tables(table,
    list("Pearson's r", "p-value", "VS-MPR*", "Upper 99% CI", "Lower 99% CI",
         "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
         "Spearman's rho", "p-value", "VS-MPR*", "Upper 99% CI", "Lower 99% CI",
         "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
         "Kendall's tau B", "p-value", "VS-MPR*", "Upper 99% CI", "Lower 99% CI",
         "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
         "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
         "contGamma", "Pearson's r", "p-value", "VS-MPR*", "Upper 99% CI",
         "Lower 99% CI", -0.0592003859505642, 0.558497687623534, 1, 0.199552209614306,
         -0.310235105822658, "Spearman's rho", "p-value", "VS-MPR*",
         "Upper 99% CI", "Lower 99% CI", -0.0341794179417942, 0.73526094223706,
         1, 0.223505684391118, -0.287398829792082, "Kendall's tau B",
         "p-value", "VS-MPR*", "Upper 99% CI", "Lower 99% CI", -0.0266666666666667,
         0.694237192757787, 1, 0.135964685981942, -0.189298019315275,
         "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
         "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
         "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
         "contNormal")
  )
})

test_that("Correlation matrix plot matches", {
  skip("base plots are not supported in regression testing")
  options <- jasptools::analysisOptions("Correlation")
  options$variables <- list("contGamma", "contNormal")
  options$plotCorrelationMatrix <- TRUE
  options$plotDensities <- TRUE
  options$plotStatistics <- TRUE
  results <- jasptools::run("Correlation", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "correlation-matrix", dir="Correlation")
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("Correlation")
  options$variables <- list("contGamma", "debInf")
  results <- jasptools::run("Correlation", "test.csv", options)
  notes <- unlist(results[["results"]][["correlations"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- list("contGamma", "debSame")
  results <- jasptools::run("Correlation", "test.csv", options)
  notes <- unlist(results[["results"]][["correlations"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- list("contGamma", "debMiss99")
  results <- jasptools::run("Correlation", "test.csv", options)
  notes <- unlist(results[["results"]][["correlations"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
