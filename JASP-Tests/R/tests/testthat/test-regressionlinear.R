context("Linear Regression")

# Some of the sample files used in this file below are distributed under the following license restrictions:
# This data set comes from Field, A. P. (2017). {Discovering Statistics Using IBM SPSS Statistics} (5th ed.). London: Sage. The data set was constructed by Andy Field who therefore owns the copyright. Andy Field generously agreed that we can include the data set in the JASP data library. This data set is also publicly available on the website that accompanies Andy Field`s book, {https://edge.sagepub.com/field5e}. Without Andy Field`s explicit consent, this data set may not be distributed for commercial purposes, this data set may not be edited, and this data set may not be presented without acknowledging its source (i.e., the terms of a CC BY-NC-ND license see https://creativecommons.org/licenses/by-nc-nd/3.0/).

# does not test
# - stepwise methods (currently gives an error if I set p entry too high)
# - plots handle errors

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$rSquaredChange <- TRUE
  options$residualsDurbinWatson <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  expect_equal_tables(table,
                      list(2.22949319138764, -0.124606080290573, "", 0, 0, 0, 4.99355368556631,
                           0, 0, 99, "H<unicode>", "", 2.22918408630401, -0.124437281020818,
                           0.000152821712396024, 0.00124876050417603, 1.55940279678998e-06,
                           1.55940279678998e-06, 5.01896242334011, -0.0102025063175828,
                           1, 98, "H<unicode>", 0.990161847660694)
  )
})

test_that("Coefficients table results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
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
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  expect_equal_tables(table,
                      list("FALSE", 0.105841360919316, -0.364486647151235, "H<unicode>",
                           "(Intercept)", 0.0775993871437191, -1.78331595418435, -0.18874858754,
                           -0.013010527928765, 1.85461222592575, "TRUE", 0.176988347288719,
                           -0.399521419778159, "H<unicode>", "(Intercept)", 0.552030096201664,
                           -0.596780555892316, -0.105623204281424, 0.188275011215311, 1,
                           "FALSE", 0.0696473684093105, 1, -0.156541849851968, "H<unicode>",
                           "contGamma", 0.558497687623533, -0.0592003859505643, -0.587083595666713,
                           1, -0.0408888274744623, 0.0747641949030438, 1)
  )
})

test_that("ANOVA table results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "debCollin1"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$modelFit <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  expect_equal_tables(table,
                      list("TRUE", 2.93169085062604, 0.0193710317063457, 0.0193710317063457,
                           "Regression", 1, "H<unicode>", 0.0900191505810211, 1.69731445510183,
                           "FALSE", 0.00660746057252562, 0.647531136107511, "Residual",
                           98, "H<unicode>", "FALSE", 0.666902167813857, "Total", 99, "H<unicode>"))
})

test_that("Coefficients Covariance table results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- c("contGamma", "contcor1")
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE),
    list(components="contcor1", isNuisance=FALSE)
  )
  options$regressionCoefficientsCovarianceMatrix <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffCovMatrixTable"]][["data"]]
  expect_equal_tables(table,
    list("TRUE", 0.00490486111017858, 0.00116294327838645, "H<unicode>",
         "contGamma", "FALSE", "", 0.0112500585702943, "H<unicode>",
         "contcor1")
    
  )
})

test_that("Descriptive table results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$descriptives <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 100, -0.18874858754, 1.05841360919316, 0.105841360919316,
         "contGamma", 100, 2.03296079621, 1.53241112621044, 0.153241112621044)
  )
})

test_that("Part and Partial Correlations table results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- c("debCollin2", "contGamma")
  options$modelTerms <- list(
    list(components="debCollin2", isNuisance=FALSE),
    list(components="contGamma", isNuisance=FALSE)
  )
  options$partAndPartialCorrelations <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partialCorTable"]][["data"]]
  expect_equal_tables(table,
    list("TRUE", "H<unicode>", "debCollin2", -0.0198338559459939, -0.0198687032851428,
         "FALSE", "H<unicode>", "contGamma", -0.0617145529439823, -0.0617173111983368)
  )
})

test_that("Collinearity Diagonistic table results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components="contcor1", isNuisance=FALSE)
  )
  options$collinearityDiagnostics <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_collinearityTable"]][["data"]]
  expect_equal_tables(table,
    list(0.473937737611082, "TRUE", 0.473937737611089, 1, 1, 1.05212452477783,
         "H<unicode>", 0.526062262388918, "FALSE", 0.526062262388911,
         1.0535567372186, 2, 0.947875475222171, "H<unicode>")
  )
})

test_that("Residuals Statistics table results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components="contcor1", isNuisance=FALSE)
  )
  options$residualsStatistics <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualsTable"]][["data"]]
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
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contOutlier"
  options$modelTerms <- list(
    list(components="contOutlier", isNuisance=FALSE)
  )
  options$residualsCasewiseDiagnostics <- TRUE
  options$residualsCasewiseDiagnosticsType <- "outliersOutside"
  options$residualsCasewiseDiagnosticsOutliersOutside <- 3
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_casewiseTable"]][["data"]]
  expect_equal_tables(table,
                      list(55, 3.34810934796608, 3.356094448, -0.187237305306683, 3.54333175330668,
                           0.0577123260439598, 83, 3.22377600371253, 2.958797116, -0.143545494526366,
                           3.10234261052637, 1.15289593050314)
  )
})

test_that("Residuals vs. Dependent plot matches", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$plotResidualsDependent <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "residuals-dependent", dir="RegressionLinear")
})

test_that("Residuals vs. Covariates plot matches", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$plotResidualsCovariates <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "residuals-covariates", dir="RegressionLinear")
})

test_that("Residuals vs. Predicted plot matches", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$plotResidualsPredicted <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "residuals-predicted", dir="RegressionLinear")
})

test_that("Standardized Residuals Histogram matches", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$plotResidualsHistogram <- TRUE
  options$plotResidualsHistogramStandardized <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "residuals-histogram", dir="RegressionLinear")
})

test_that("Q-Q Plot Standardized Residuals matches", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE)
  )
  options$plotResidualsQQ <- TRUE
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "residuals-q-q", dir="RegressionLinear")
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("RegressionLinear")

  options$dependent <- "debInf"
  options$covariates <- "contGamma"
  options$modelTerms <- list(list(components="contGamma", isNuisance=FALSE))
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Inf dependent check")

  options$dependent <- "contNormal"
  options$covariates <- "debInf"
  options$modelTerms <- list(list(components="debInf", isNuisance=FALSE))
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Inf covariate check")

  options$covariates <- "contGamma"
  options$wlsWeights <- "debInf"
  options$modelTerms <- list(list(components="contGamma", isNuisance=FALSE))
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Inf wlsWeights check")

  options$dependent <- "debSame"
  options$covariates <- "contGamma"
  options$wlsWeights <- ""
  options$modelTerms <- list(list(components="contGamma", isNuisance=FALSE))
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="No variance dependent check")

  options$dependent <- "contNormal"
  options$covariates <- "debSame"
  options$modelTerms <- list(list(components="debSame", isNuisance=FALSE))
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="No variance covariate check")

  options$dependent <- "contGamma"
  options$covariates <- "contcor1"
  options$wlsWeights <- "contNormal"
  options$modelTerms <- list(list(components="contcor1", isNuisance=FALSE))
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Negative wlsWeights check")

  options$dependent <- "debNaN"
  options$covariates <- "contcor1"
  options$wlsWeights <- ""
  options$modelTerms <- list(list(components="contcor1", isNuisance=FALSE))
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Too few obs dependent check")

  options$dependent <- "contGamma"
  options$covariates <- "debNaN"
  options$modelTerms <- list(list(components="debNaN", isNuisance=FALSE))
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Too few obs covariate check")

  options$dependent <- "contGamma"
  options$covariates <- "contNormal"
  options$wlsWeights <- "debNaN"
  options$modelTerms <- list(list(components="contNormal", isNuisance=FALSE))
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label="Too few obs wlsWeights check")
  
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "contNormal"
  options$covariates <- c("debCollin2", "debCollin3")
  options$modelTerms <- list(
    list(components="debCollin2", isNuisance=FALSE),
    list(components="debCollin3", isNuisance=FALSE)
  )
  results <- jaspTools::run("RegressionLinear", "test.csv", options)
  results$results$errorMessage
  expect_identical(results[["status"]], "validationError", label="Perfect correlation check")
})

# Below are the unit tests for Andy Field's book

# Chapter 1
test_that("Fields Book - Chapter 1 results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "sales"
  options$covariates <- "adverts"
  options$modelTerms <- list(
    list(components="adverts", isNuisance=FALSE)
  )
  results <- jaspTools::run("RegressionLinear", dataset = "Album Sales.csv", options)
  output1 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  expect_equal_tables(output1,
                      list(0, 0, 80.698956672563, 0, "H<unicode>", 0.578487741981689, 0.334648067623073,
                           65.9914352978124, 0.331287704328241, "H<unicode>")
  )
  output2 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  expect_equal_tables(output2,
                      list("TRUE", 99.5868714961988, 433687.832532257, 433687.832532257,
                           "Regression", 1, "H<unicode>", 2.94197985216586e-19, "FALSE",
                           4354.86953266536, 862264.167467742, "Residual", 198, "H<unicode>",
                           "FALSE", 1295952, "Total", 199, "H<unicode>")
                      
  )
  output3 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  expect_equal_tables(output3,
                      list("FALSE", 5.70627794978487, "H<unicode>", "(Intercept)", 1.59908972195556e-84,
                           33.8574464300821, 193.2, "TRUE", 7.53657467947199, "H<unicode>",
                           "(Intercept)", 5.9678171979745e-43, 17.7985283125294, 134.139937812074,
                           "FALSE", 0.00963236621523019, "H<unicode>", "adverts", 2.94197985216575e-19,
                           0.578487741981689, 9.97932219623151, 0.0961244859738772)
  )
  
  options$covariates <- c("adverts", "airplay", "attract")
  options$modelTerms <- list(
    list(components="adverts", isNuisance=TRUE),
    list(components="airplay", isNuisance=FALSE),
    list(components="attract", isNuisance=FALSE)
  )
  results <- jaspTools::run("RegressionLinear", dataset = "Album Sales.csv", options)
  output4 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  expect_equal_tables(output4,
                      list("H<unicode>", 0.5784877, 0.3346481, 0.3312877, 65.99144,
                           "H<unicode>", 0.8152715, 0.6646677, 0.659535, 47.08734)
  )
  output5 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  expect_equal_tables(output5,
                      list("FALSE", 99.5868714961988, 433687.832532257, 433687.832532257,
                           "Regression", 1, "H<unicode>", 2.94197985216586e-19, "FALSE",
                           4354.86953266536, 862264.167467742, "Residual", 198, "H<unicode>",
                           "FALSE", 1295952, "Total", 199, "H<unicode>", "TRUE", 129.498273390974,
                           287125.806089999, 861377.418269998, "Regression", 3, "H<unicode>",
                           2.87553505648734e-46, "FALSE", 2217.2172537245, 434574.581730001,
                           "Residual", 196, "H<unicode>", "FALSE", 1295952, "Total", 199,
                           "H<unicode>")
  )
  output6 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_coeffTable"]][["data"]]
  expect_equal_tables(output6,
                      list("FALSE", 7.53657467947199, "H<unicode>", "(Intercept)", 5.9678171979745e-43,
                           17.7985283125294, 134.139937812074, "FALSE", 0.00963236621523019,
                           "H<unicode>", "adverts", 2.94197985216575e-19, 0.578487741981689,
                           9.97932219623151, 0.0961244859738772, "TRUE", 17.3500005649351,
                           "H<unicode>", "(Intercept)", 0.126669773576493, -1.53388804006521,
                           -26.6129583616786, "FALSE", 0.00692301687199933, "H<unicode>",
                           "adverts", 5.05493680123644e-26, 0.510846225434074, 12.2612477656671,
                           0.0848848251534775, "FALSE", 0.277770831682515, "H<unicode>",
                           "airplay", 1.32630717054857e-25, 0.511988143597586, 12.1230337617277,
                           3.36742517051031, "FALSE", 2.43784926487601, "H<unicode>", "attract",
                           9.49212095293172e-06, 0.191683427305029, 4.54758846836075, 11.0863352045519)
  )
})

# Chapter 2
test_that("Fields Book - Chapter 2 results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "sales"
  options$covariates <- c("adverts", "airplay", "attract")
  options$modelTerms <- list(
    list(components="adverts", isNuisance=TRUE),
    list(components="airplay", isNuisance=FALSE),
    list(components="attract", isNuisance=FALSE)
  )
  options$rSquaredChange <- TRUE
  options$regressionCoefficientsConfidenceIntervals <- TRUE
  results <- jaspTools::run("RegressionLinear", dataset = "Album Sales.csv", options)
  output4 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_summaryTable"]][["data"]]
  expect_equal_tables(output4,
                      list("H<unicode>", 0.5784877, 0.3346481, 0.3312877, 65.99144, 0.3346481, 99.58687, 1, 198, 2.94198e-19,
                           "H<unicode>", 0.8152715, 0.6646677, 0.659535, 47.08734, 0.3300196, 96.44738, 2, 196, 6.879395e-30)
  )
  # needs investigating
  # output6 <- results[["results"]][["regression"]][["data"]]
  # expect_equal_tables(output6,
  #                     list(0, "(Intercept)", 134.1399, 7.536575, "", 17.79853, 5.967817e-43, 119.2777, 149.0022, "TRUE",
  #                          "", "adverts", 0.09612449, 0.009632366, 0.5784877, 9.979322, 2.94198e-19, 0.07712929, 0.1151197,
  #                          1, "(Intercept)", -26.61296, 17.35, "", -1.533888, 0.1266698, -60.82961, 7.603693, "TRUE",
  #                          "", "adverts", 0.08488483, 0.006923017, 0.5108462, 12.26125, 5.054937e-26, 0.07123166, 0.09853799,
  #                          "", "airplay", 3.367425, 0.2777708, 0.5119881, 12.12303, 1.326307e-25, 2.819622, 3.915228,
  #                          "", "attract", 11.08634, 2.437849, 0.1916834, 4.547588, 9.492121e-06, 6.278552, 15.89412)
  # )
})

# Chapter 3
test_that("Fields Book - Chapter 3 results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "sales"
  options$covariates <- c("adverts", "airplay", "attract")
  options$modelTerms <- list(
    list(components="adverts", isNuisance=TRUE),
    list(components="airplay", isNuisance=FALSE),
    list(components="attract", isNuisance=FALSE)
  )
  options$plotResidualsPredicted <- TRUE
  options$plotsPartialRegression <- TRUE
  options$plotResidualsHistogram <- TRUE
  options$plotResidualsHistogramStandardized <- TRUE
  options$plotResidualsQQ <- TRUE
  options$residualsCasewiseDiagnostics <- TRUE
  options$residualsCasewiseDiagnosticsType <- "outliersOutside"
  options$residualsCasewiseDiagnosticsOutliersOutside <- 2
  options$regressionCoefficientsConfidenceIntervals <- TRUE
  results <- jaspTools::run("RegressionLinear", dataset = "Album Sales.csv", options)
  figure3 <- results[["state"]][["figures"]][[1]][["obj"]] # Residuals vs. Predicted
  expect_equal_plots(figure3, "field-residuals-predicted", dir="RegressionLinear")
  figure4a <- results[["state"]][["figures"]][[4]][["obj"]] # Partial Plot Adverts
  expect_equal_plots(figure4a, "field-partial-adverts", dir="RegressionLinear")
  figure4b <- results[["state"]][["figures"]][[5]][["obj"]] # Partial Plot Airplay
  expect_equal_plots(figure4b, "field-partial-airplay", dir="RegressionLinear")
  figure4c <- results[["state"]][["figures"]][[6]][["obj"]] # Partial Plot Image
  expect_equal_plots(figure4c, "field-partial-attract", dir="RegressionLinear")
  figure5a <- results[["state"]][["figures"]][[2]][["obj"]] # Standardized Residuals Histogram
  expect_equal_plots(figure5a, "field-residuals-histogram", dir="RegressionLinear")
  figure5b <- results[["state"]][["figures"]][[3]][["obj"]] # Q-Q-Plot
  expect_equal_plots(figure5b, "field-qq", dir="RegressionLinear")
  output1 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_casewiseTable"]][["data"]]
  expect_equal_tables(output1,
                      list(1, 2.177404, 330, 229.9203, 100.0797, 0.05870388, 
                           2, -2.323083, 120, 228.949, -108.949, 0.01088943,
                           10, 2.130289, 300, 200.4662, 99.53375, 0.01775647,
                           47, -2.460996, 40, 154.9698, -114.9698, 0.02411519,
                           52, 2.099446, 190, 92.59734, 97.40266, 0.03315918,
                           55, -2.455913, 190, 304.1231, -114.1231, 0.0404159,
                           61, 2.104079, 300, 201.1897, 98.8103, 0.005948358,
                           68, -2.363549, 70, 180.4156, -110.4156, 0.02228898,
                           100, 2.095399, 250, 152.7133, 97.28666, 0.03136402,
                           164, -2.628814, 120, 241.324, -121.324, 0.07076588,
                           169, 3.093333, 360, 215.8675, 144.1325, 0.050867,
                           200, -2.088044, 110, 207.2061, -97.20606, 0.02513455)
  )
  
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "sales"
  options$covariates <- c("adverts", "airplay", "attract")
  options$modelTerms <- list(
    list(components="adverts", isNuisance=TRUE),
    list(components="airplay", isNuisance=FALSE),
    list(components="attract", isNuisance=FALSE)
  )
  options$residualsCasewiseDiagnostics <- TRUE
  options$residualsCasewiseDiagnosticsType <- "allCases"
  options$regressionCoefficientsBootstrapping <- TRUE
  options$regressionCoefficientsBootstrappingReplicates <- 1000
  set.seed(1) # For Bootstrapping Unit Tests
  options$regressionCoefficientsConfidenceIntervals <- TRUE
  results <- jaspTools::run("RegressionLinear", dataset = "Album Sales.csv", options)
  figure10 <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_casewiseTable"]][["data"]]
  figure10 <- list(figure10[[1]]$cooksD, figure10[[2]]$cooksD, figure10[[3]]$cooksD, figure10[[4]]$cooksD,
                   figure10[[5]]$cooksD, figure10[[6]]$cooksD, figure10[[7]]$cooksD, figure10[[8]]$cooksD,
                   figure10[[9]]$cooksD, figure10[[10]]$cooksD, figure10[[11]]$cooksD, figure10[[12]]$cooksD,
                   figure10[[13]]$cooksD, figure10[[14]]$cooksD, figure10[[15]]$cooksD, figure10[[16]]$cooksD,
                   figure10[[17]]$cooksD, figure10[[18]]$cooksD, figure10[[19]]$cooksD, figure10[[20]]$cooksD,
                   figure10[[21]]$cooksD, figure10[[22]]$cooksD)
  expect_equal_tables(figure10,
                      list(0.05870388, 0.01088943, 0.01140066, 7.166478e-05, 0.0001025423, 
                           0.001377347, 0.00594368, 0.0007230228, 0.0009490373, 0.01775647,
                           0.000822629, 0.01657069, 0.0003049181, 0.0003904011, 0.004041096,
                           0.002207233, 0.0002713418, 0.0001776211, 0.0004687904, 0.008795957,
                           0.0002810147, 0.0002291055)
  )
  # needs investigating
  # output2 <- results[["results"]][["bootstrap.regression"]][["data"]]
  # expect_equal_tables(output2,
  #                     list(0, "(Intercept)", -0.08265759, 134.1399, 8.228444, 118.874, 151.0766, "TRUE", 
  #                          "", "adverts", 0.0002069432, 0.09612449, 0.008855503, 0.07704638, 0.1127528,
  #                          1, "(Intercept)", 1.258855, -26.61296, 15.91686, -54.69175, 8.180507, "TRUE",
  #                          "", "adverts", -6.543211e-05, 0.08488483, 0.007137828, 0.07036585, 0.09968162,
  #                          "", "airplay", 0.0128862, 3.367425, 0.3070985, 2.732326, 3.918479,
  #                          "", "attract", -0.2110928, 11.08634, 2.234141, 6.502079, 15.13605)
  # )
  
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "spai"
  options$covariates <- c("tosca", "obq")
  options$modelTerms <- list(
    list(components="tosca", isNuisance=TRUE),
    list(components="obq", isNuisance=FALSE)
  )
  options$plotResidualsPredicted <- TRUE
  options$plotsPartialRegression <- TRUE
  options$plotResidualsQQ <- TRUE
  results <- jaspTools::run("RegressionLinear", dataset = "SocialAnxietyRegression.csv", options)
  figure11a <- results[["state"]][["figures"]][[1]][["obj"]] # Residuals vs. Predicted
  #expect_equal_plots(figure11a, "?", dir="RegressionLinear") # This command needs to be updated
  figure11b <- results[["state"]][["figures"]][[3]][["obj"]] # Partial Plot Adverts
  #expect_equal_plots(figure11b, "?", dir="RegressionLinear") # This command needs to be updated
  figure11c <- results[["state"]][["figures"]][[4]][["obj"]] # Partial Plot Airplay
  #expect_equal_plots(figure11c, "?", dir="RegressionLinear") # This command needs to be updated
  figure11d <- results[["state"]][["figures"]][[2]][["obj"]] # Q-Q-Plot
  #expect_equal_plots(figure11d, "?", dir="RegressionLinear") # This command needs to be updated
})


# Chapter 4
test_that("Fields Book - Chapter 4 results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "Happiness"
  options$covariates <- c("dummy1", "dummy2")
  options$modelTerms <- list(
    list(components="dummy1", isNuisance=FALSE),
    list(components="dummy2", isNuisance=FALSE)
  )
  options$regressionCoefficientsConfidenceIntervals <- TRUE
  results <- jaspTools::run("RegressionLinear", dataset = "Puppies Dummy.csv", options)
  output1a <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  expect_equal_tables(output1a,
                      list("TRUE", 5.11864406779661, 10.0666666666667, 20.1333333333333,
                           "Regression", 2, "H<unicode>", 0.0246942895382226, "FALSE",
                           1.96666666666667, 23.6, "Residual", 12, "H<unicode>", "FALSE",
                           43.7333333333333, "Total", 14, "H<unicode>")
  )
  # needs investigating
  # output1b <- results[["results"]][["regression"]][["data"]]
  # expect_equal_tables(output1b,
  #                     list(1, "(Intercept)", 2.2, 0.6271629, "", 3.50786, 0.00431889, 0.8335294, 3.566471, "TRUE",
  #                          "", "dummy1", 2.8, 0.8869423, 0.7730207, 3.156913, 0.008268103, 0.8675187, 4.732481,
  #                          "", "dummy2", 1, 0.8869423, 0.2760788, 1.127469, 0.2815839, -0.9324813, 2.932481)
  # )
})


# Chapter 5
test_that("Fields Book - Chapter 5 results match", {
  options <- jaspTools::analysisOptions("RegressionLinear")
  options$dependent <- "Happiness"
  options$covariates <- c("Dummy1", "Dummy2")
  options$modelTerms <- list(
    list(components="Dummy1", isNuisance=FALSE),
    list(components="Dummy2", isNuisance=FALSE)
  )
  options$regressionCoefficientsConfidenceIntervals <- TRUE
  results <- jaspTools::run("RegressionLinear", dataset = "Puppies Contrast.csv", options)
  output1a <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_anovaTable"]][["data"]]
  expect_equal_tables(output1a,
                      list("TRUE", 5.11864406779661, 10.0666666666667, 20.1333333333333,
                           "Regression", 2, "H<unicode>", 0.0246942895382226, "FALSE",
                           1.96666666666667, 23.6, "Residual", 12, "H<unicode>", "FALSE",
                           43.7333333333333, "Total", 14, "H<unicode>")
  )
  # needs investigating
  # output1b <- results[["results"]][["regression"]][["data"]]
  # expect_equal_tables(output1b,
  #                     list(1, "(Intercept)", 3.466667, 0.3620927, "", 9.573976, 5.720565e-07, 2.677734, 4.255599, "TRUE",
  #                          "", "Dummy1", 0.6333333, 0.2560382, 0.5245497, 2.473589, 0.02930022, 0.07547404, 1.191193,
  #                          "", "Dummy2", 0.9, 0.4434712, 0.4303643, 2.029444, 0.06519221, -0.06624065, 1.866241)
  # )
})