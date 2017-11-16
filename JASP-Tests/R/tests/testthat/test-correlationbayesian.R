context("Bayesian Correlation")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - errors whilst plotting

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("CorrelationBayesian")
  options$variables <- c("contcor1", "contcor2")
  options$kendallsTauB <- TRUE
  options$reportBayesFactors <- TRUE
  options$flagSupported <- TRUE
  options$credibleInterval <- TRUE
  options$priorWidth <- 1.5
  results <- JASPTools::run("CorrelationBayesian", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["correlations"]][["data"]]
  expect_equal_tables(table,
    list("Pearson's r", "<unicode>", "", "BF<unicode><unicode>", "<unicode>",
         "", "Upper 95% CI", "<unicode>", "", "Lower 95% CI", "<unicode>",
         "", "Kendall's tau", "<unicode>", "", "BF<unicode><unicode>",
         "<unicode>", "", "Upper 95% CI", "<unicode>", "", "Lower 95% CI",
         "<unicode>", "", "contcor1", "Pearson's r", 0.657010063712354,
         "<unicode>", "BF<unicode><unicode>", 71191291327.7147, "<unicode>",
         "Upper 95% CI", 0.753487301516087, "<unicode>", "Lower 95% CI",
         0.524567068817447, "<unicode>", "Kendall's tau", 0.503030303030303,
         "<unicode>", "BF<unicode><unicode>", 78878934747.8062, "<unicode>",
         "Upper 95% CI", 0.62124248496994, "<unicode>", "Lower 95% CI",
         0.360721442885771, "<unicode>", "contcor2", "***", "***")
  )
})

test_that("Correlation plot matches", {
  options <- JASPTools::analysisOptions("CorrelationBayesian")
  options$variables <- c("contcor1", "contcor2")
  options$plotCorrelationMatrix <- TRUE
  options$plotDensitiesForVariables <- TRUE
  options$plotPosteriors <- TRUE
  results <- JASPTools::run("CorrelationBayesian", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "correlation", dir="CorrelationBayesian")
})

test_that("Analysis handles errors", {
  options <- JASPTools::analysisOptions("CorrelationBayesian")

  options$variables <- c("contNormal", "debInf")
  results <- JASPTools::run("CorrelationBayesian", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["correlations"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- c("contNormal", "debSame")
  results <- JASPTools::run("CorrelationBayesian", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["correlations"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- c("contNormal", "debMiss99")
  results <- JASPTools::run("CorrelationBayesian", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["correlations"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
