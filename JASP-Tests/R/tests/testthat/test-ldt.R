context("Discover Distributions - t distribution")

options <- jasptools::analysisOptions("LDt")
options$.meta <- list(newVariableName = list(containsColumn = TRUE), variable = list(
  containsColumn = TRUE))
options$andersonDarling <- TRUE
options$ciInterval <- TRUE
options$cramerVonMisses <- TRUE
options$ecdf <- TRUE
options$estCDF <- TRUE
options$estPDF <- TRUE
options$highlightDensity <- TRUE
options$highlightProbability <- TRUE
options$histogram <- TRUE
options$kolmogorovSmirnov <- TRUE
options$methodMLE <- TRUE
options$moments <- TRUE
options$newVariableName <- ""
options$outputSE <- TRUE
options$ppplot <- TRUE
options$qqplot <- TRUE
options$summary <- TRUE
options$variable <- "t100(df=1,ncp=0)"
set.seed(1)
results <- jasptools::run("LDt", "Distributions.csv", options)


test_that("Empirical Cumulative Distribution plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_ecdf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "empirical-cumulative-distribution", dir="LDt")
})

test_that("Histogram plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "histogram", dir="LDt")
})

test_that("Observed Moments table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_moments"]][["data"]]
  expect_equal_tables(table,
                      list(-2.21047644001841, 1, -2.21047644001841, 316.765269274516, 2,
                           321.651475366392))
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  expect_equal_tables(table,
                      list(36.2706307848376, -2.21047644001841, -0.238122673749703, -170.103213735222,
                           -1.49213653555073, 0.777614680002567, 100, 17.8875632342448,
                           319.964918459107, "t100(df=1,ncp=0)"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  expect_equal_tables(table,
                      list(1.11314614484469, 0.776931331533359, "df", 0.17154132216885, 1.44936095815601,
                           -0.199023390898578, -0.421397364388024, "ncp", 0.113458193744122,
                           0.0233505825908685))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDt")
})

test_that("Histogram vs. Theoretical PDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "histogram-vs-theoretical-pdf", dir="LDt")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.943712731748752, 0.0527356116236999, "Kolmogorov-Smirnov", 0.936114828474193,
                           0.039590649123452, "Cram<unicode>r-von Mises", 0.951915789105146, 0.280536039088275,
                           "Anderson-Darling"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "p-p-plot", dir="LDt")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "q-q-plot", dir="LDt")
})

test_that("Density Plot matches", {
  plotName <- results[["results"]][["plotPDF"]][["collection"]][["plotPDF_pdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "density-plot", dir="LDt")
})