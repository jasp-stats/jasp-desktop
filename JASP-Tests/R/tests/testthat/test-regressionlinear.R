context("Linear Regression")

# does not test
# - stepwise methods (currently gives an error if I set p entry too high)
# - plots handle errors

test_that("Main table results match", {
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$rSquaredChange <- TRUE
  options$residualsDurbinWatson <- TRUE
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["model summary"]][["data"]]
  expect_equal_tables(table,
    list(1, 0.00124876050417603, 1.55940279678998e-06, -0.0102025063175828,
         5.01896242334011, 1.55940279678998e-06, 0.000152821712396024,
         1, 98, 0.990161847660694, 2.22918408630401)
  )
})

test_that("Coefficients table results match", {
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$regressionCoefficientsEstimates <- TRUE
  options$regressionCoefficientsConfidenceIntervals <- TRUE
  options$regressionCoefficientsConfidenceIntervalsInterval <- 0.9
  options$collinearityDiagnostics <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["regression"]][["data"]]
  expect_equal_tables(table,
    list(1, "(Intercept)", -0.105623204281424, 0.176988347288719, "", -0.596780555892316,
         0.552030096201664, 1, -0.107624744624094, -0.103621663938754,
         "", "", "TRUE", "", "contGamma", -0.0408888274744623, 0.0696473684093105,
         -0.0592003859505643, -0.587083595666713, 0.558497687623533,
         1, -0.0416764613484857, -0.0401011936004389, 1, 1)
  )
})

test_that("ANOVA table results match", {
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "debCollin1"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$modelFit <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["anova"]][["data"]]
  expect_equal_tables(table,
    list(1, "Regression", 0.01937103, 1, 0.01937103, 2.931691,
         0.09001915, 1.697314, "TRUE", "", "Residual", 0.6475311,
         98, 0.006607461, "", "", "", "", "Total", 0.6669022,
         99, "", "", "", "")
  )
})

test_that("Coefficients Covariance table results match", {
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- c("contGamma", "contcor1")
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE),
    list(components="contcor1", isNuisance=FALSE)
  )
  options$regressionCoefficientsCovarianceMatrix <- TRUE
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["coefficient covariances"]][["data"]]
  expect_equal_tables(table,
    list(1, "contGamma", "TRUE", 0.00490486111017858, 0.00116294327838645,
         "", "contcor1", "", 0.0112500585702943)
  )
})

test_that("Descriptive table results match", {
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$descriptives <- TRUE
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["descriptives"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 100, -0.18874858754, 1.05841360919316, 0.105841360919316,
         "contGamma", 100, 2.03296079621, 1.53241112621044, 0.153241112621044)
  )
})

test_that("Part and Partial Correlations table results match", {
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- c("debCollin2", "contGamma")
  options$modelTerms <- list(
    list(components="debCollin2", isNuisance=FALSE),
    list(components="contGamma", isNuisance=FALSE)
  )
  options$partAndPartialCorrelations <- TRUE
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["correlations"]][["data"]]
  expect_equal_tables(table,
    list(1, "debCollin2", -0.0198687, -0.01983386, "TRUE",
         "", "contGamma", -0.06171731, -0.06171455)
  )
})

test_that("Collinearity Diagonistic table results match", {
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components="contcor1", isNuisance=FALSE)
  )
  options$collinearityDiagnostics <- TRUE
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["collinearity diagnostics"]][["data"]]
  expect_equal_tables(table,
    list(1, 1, "TRUE", 1.05212452477783, 1, 0.473937737611082, 0.473937737611089,
         "", 2, 0.947875475222171, 1.0535567372186, 0.526062262388918,
         0.526062262388911)
  )
})

test_that("Residuals Statistics table results match", {
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components="contcor1", isNuisance=FALSE)
  )
  options$residualsDurbinWatson <- TRUE
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["residuals statistics"]][["data"]]
  expect_equal_tables(table,
    list("Predicted Value", -0.559288923489434, 0.200246244240391, -0.18874858754,
         0.170438384014894, 100, "Residual", -2.87689451816188, 3.15584820375961,
         9.84238732182341e-18, 1.04460046208093, 100, "Std. Predicted Value",
         -2.17404276678107, 2.28231940844029, 6.76021738588162e-17, 1,
         100, "Std. Residual", -2.75476210882495, 3.10457788320998, 0.000645077391284091,
         1.00643569133304, 100)
  )
})

test_that("Casewise Diagnostics table results match", {
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contOutlier"
  options$modelTerms <- list(
    list(components="contOutlier", isNuisance=FALSE)
  )
  options$residualsCasewiseDiagnostics <- TRUE
  options$residualsCasewiseDiagnosticsType <- "outliersOutside"
  options$residualsCasewiseDiagnosticsOutliersOutside <- 3
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["residuals statistics"]][["data"]]
  expect_equal_tables(table,
    list("Predicted Value", -0.275779454672472, -0.143545494526366, -0.18874858754,
         0.0109677571683762, 100, "Residual", -2.83584638233813, 3.54333175330668,
         1.8570756911418e-17, 1.05835678125479, 100, "Std. Predicted Value",
         -7.93515627638173, 4.12145275644591, 7.10605890536647e-16, 1,
         100, "Std. Residual", -2.6793890904309, 3.34810934796608, 0.00829531757614864,
         1.01553841838244, 100)
  )
})

# test_that("Residuals vs. Dependent plot matches", {
#   options <- jasptools::analysisOptions("RegressionLinear")
#   options$dependent <- "contNormal"
#   options$covariates <- "contGamma"
#   options$modelTerms <- list(
#     list(components="contGamma", isNuisance=FALSE)
#   )
#   options$plotResidualsDependent <- TRUE
#   results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "residuals-dependent", dir="RegressionLinear")
# })
#
# test_that("Residuals vs. Covariates plot matches", {
#   options <- jasptools::analysisOptions("RegressionLinear")
#   options$dependent <- "contNormal"
#   options$covariates <- "contGamma"
#   options$modelTerms <- list(
#     list(components="contGamma", isNuisance=FALSE)
#   )
#   options$plotResidualsCovariates <- TRUE
#   results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "residuals-covariates", dir="RegressionLinear")
# })
#
# test_that("Residuals vs. Predicted plot matches", {
#   options <- jasptools::analysisOptions("RegressionLinear")
#   options$dependent <- "contNormal"
#   options$covariates <- "contGamma"
#   options$modelTerms <- list(
#     list(components="contGamma", isNuisance=FALSE)
#   )
#   options$plotResidualsPredicted <- TRUE
#   results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "residuals-predicted", dir="RegressionLinear")
# })
#
# test_that("Standardized Residuals Histogram matches", {
#   options <- jasptools::analysisOptions("RegressionLinear")
#   options$dependent <- "contNormal"
#   options$covariates <- "contGamma"
#   options$modelTerms <- list(
#     list(components="contGamma", isNuisance=FALSE)
#   )
#   options$plotResidualsHistogram <- TRUE
#   options$plotResidualsHistogramStandardized <- TRUE
#   results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "residuals-histogram", dir="RegressionLinear")
# })
#
# test_that("Q-Q Plot Standardized Residuals matches", {
#   options <- jasptools::analysisOptions("RegressionLinear")
#   options$dependent <- "contNormal"
#   options$covariates <- "contGamma"
#   options$modelTerms <- list(
#     list(components="contGamma", isNuisance=FALSE)
#   )
#   options$plotResidualsQQ <- TRUE
#   results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "residuals-q-q", dir="RegressionLinear")
# })

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("RegressionLinear")

  options$dependent <- "debInf"
  options$covariates <- "contGamma"
  options$modelTerms <- list(list(components="contGamma", isNuisance=FALSE))
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["model summary"]][["error"]][["errorType"]], "badData",
                   label="Inf dependent check")

  options$dependent <- "contNormal"
  options$covariates <- "debInf"
  options$modelTerms <- list(list(components="debInf", isNuisance=FALSE))
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["model summary"]][["error"]][["errorType"]], "badData",
                  label="Inf covariate check")

  options$covariates <- "contGamma"
  options$wlsWeights <- "debInf"
  options$modelTerms <- list(list(components="contGamma", isNuisance=FALSE))
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["model summary"]][["error"]][["errorType"]], "badData",
                  label="Inf wlsWeights check")

  options$dependent <- "debSame"
  options$covariates <- "contGamma"
  options$wlsWeights <- ""
  options$modelTerms <- list(list(components="contGamma", isNuisance=FALSE))
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["model summary"]][["error"]][["errorType"]], "badData",
                  label="No variance dependent check")

  options$dependent <- "contNormal"
  options$covariates <- "debSame"
  options$modelTerms <- list(list(components="debSame", isNuisance=FALSE))
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["model summary"]][["error"]][["errorType"]], "badData",
                  label="No variance covariate check")

  options$dependent <- "contGamma"
  options$covariates <- "contcor1"
  options$wlsWeights <- "contNormal"
  options$modelTerms <- list(list(components="contcor1", isNuisance=FALSE))
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["model summary"]][["error"]][["errorType"]], "badData",
                  label="Negative wlsWeights check")

  options$dependent <- "debNaN"
  options$covariates <- "contcor1"
  options$wlsWeights <- ""
  options$modelTerms <- list(list(components="contcor1", isNuisance=FALSE))
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["model summary"]][["error"]][["errorType"]], "badData",
                  label="Too few obs dependent check")

  options$dependent <- "contGamma"
  options$covariates <- "debNaN"
  options$modelTerms <- list(list(components="debNaN", isNuisance=FALSE))
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["model summary"]][["error"]][["errorType"]], "badData",
                  label="Too few obs covariate check")

  options$dependent <- "contGamma"
  options$covariates <- "contNormal"
  options$wlsWeights <- "debNaN"
  options$modelTerms <- list(list(components="contNormal", isNuisance=FALSE))
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["model summary"]][["error"]][["errorType"]], "badData",
                  label="Too few obs wlsWeights check")
  
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- c("debCollin2", "debCollin3")
  options$modelTerms <- list(
    list(components="debCollin2", isNuisance=FALSE),
    list(components="debCollin3", isNuisance=FALSE)
  )
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  results$results$errorMessage
  expect_equal(results[["results"]], list(title = "error", error = 1, errorMessage = "The following problem(s) occurred while running the analysis:<ul><li>The variance-covariance matrix of the supplied data is not positive-definite. Please check if variables have many missings observations or are collinear</li></ul><ul><li> Note: The following pair(s) of variables is/are perfectly correlated: debCollin2 and debCollin3. Note that if you have specified a weights variable, the correlations are computed for the weighted variables.</li></ul>"))
  
  options <- jasptools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- c("contcor1", "contcor2", "contWide", "contNarrow")
  options$modelTerms <- list(
    list(components="contcor1", isNuisance=FALSE),
    list(components="contcor2", isNuisance=FALSE),
    list(components="contWide", isNuisance=FALSE),
    list(components="contNarrow", isNuisance=FALSE)
  )
  options$wlsWeights <- "contExpon"
  results <- jasptools::run("RegressionLinear", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_equal(results[["results"]], list(title = "error", error = 1, errorMessage = "The following problem(s) occurred while running the analysis:<ul><li>The variance-covariance matrix of the supplied data is not positive-definite. Please check if variables have many missings observations or are collinear</li></ul>"))
})