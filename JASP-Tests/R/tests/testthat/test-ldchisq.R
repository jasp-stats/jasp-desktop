context("Discover Distributions - Chi-square")

options <- jaspTools::analysisOptions("LDchisq")
options$explanatoryText <- TRUE
options$parsSupportMoments <- TRUE
options$plotCDF <- TRUE
options$plotQF <- TRUE
options$methodMLE <- TRUE
options$outputEstimates <- TRUE
options$outputSE <- TRUE
options$ciInterval <- TRUE
options$ciIntervalInterval <- 0.95
options$estPDF <- TRUE
options$qqplot <- TRUE
options$estCDF <- TRUE
options$ppplot <- TRUE
options$kolmogorovSmirnov <- TRUE
options$cramerVonMisses <- TRUE
options$andersonDarling <- TRUE
options$highlightDensity <- TRUE
options$highlightProbability <- TRUE
options$variable <- "Chisq1000(df=5,ncp=0)"
options$moments <- TRUE
options$ecdf <- TRUE
options$newVariableName <- ""
set.seed(1)
results <- jaspTools::run("LDchisq", "Distributions.csv", options)


test_that("Empirical Cumulative Distribution plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_ecdf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "empirical-cumulative-distribution", dir="LDchisq")
})

test_that("Histogram plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "histogram", dir="LDchisq")
})

test_that("Observed Moments table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_moments"]][["data"]]
  expect_equal_tables(table,
                      list(5.06873210797175, 1, 5.06873210797175, 10.4688174303449, 2, 36.1608626127286
                      ))
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  expect_equal_tables(table,
                      list(21.9144146199972, 5.06873210797175, 4.38244055093348, 0.175664757015611,
                           2.68285589535141, 6.70891916895853, 1000, 3.2371741885589, 10.479296727072,
                           "Chisq1000(df=5,ncp=0)"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  expect_equal_tables(table,
                      list(4.8938297545344, 4.47271658799517, "k", 0.214857604456471, 5.31494292107364,
                           0.174944391692288, -0.295081957134747, "<unicode>", 0.239813768280715,
                           0.644970740519323))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDchisq")
})

test_that("Histogram vs. Theoretical PDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "histogram-vs-theoretical-pdf", dir="LDchisq")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.98827693435693, 0.0141404497973602, "Kolmogorov-Smirnov", 0.994063879727108,
                           0.0224779929435535, "Cram<unicode>r-von Mises", 0.999468456836373,
                           0.133316493982989, "Anderson-Darling"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "p-p-plot", dir="LDchisq")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "q-q-plot", dir="LDchisq")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCDF"]][["collection"]][["plotCDF_cdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDchisq")
})

test_that("Density Plot matches", {
  plotName <- results[["results"]][["plotPDF"]][["collection"]][["plotPDF_pdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "density-plot", dir="LDchisq")
})

test_that("Quantile Plot matches", {
  plotName <- results[["results"]][["plotQF"]][["collection"]][["plotQF_qfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "quantile-plot", dir="LDchisq")
})
