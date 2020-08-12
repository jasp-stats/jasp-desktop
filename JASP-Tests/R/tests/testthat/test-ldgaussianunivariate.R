context("Discover Distributions - Normal")

options <- jaspTools::analysisOptions("LDgaussianunivariate")
options$.meta <- list(newVariableName = list(containsColumn = TRUE), variable = list(
  containsColumn = TRUE))
options$andersonDarling <- TRUE
options$ciInterval <- TRUE
options$ciIntervalInterval <- 0.95
options$cramerVonMisses <- TRUE
options$ecdf <- TRUE
options$estCDF <- TRUE
options$estPDF <- TRUE
options$explanatoryText <- TRUE
options$highlightDensity <- TRUE
options$highlightProbability <- TRUE
options$histogram <- TRUE
options$kolmogorovSmirnov <- TRUE
options$methodMLE <- TRUE
options$moments <- TRUE
options$momentsUpTo <- 10
options$newVariableName <- ""
options$outputEstimates <- TRUE
options$outputSE <- TRUE
options$parsSupportMoments <- TRUE
options$plotCDF <- TRUE
options$plotQF <- TRUE
options$ppplot <- TRUE
options$qqplot <- TRUE
options$shapiroWilk <- TRUE
options$summary <- TRUE
options$variable <- "Normal100(mu=0,sigma=1)"
set.seed(1)
results <- jaspTools::run("LDgaussianunivariate", "Distributions.csv", options)


test_that("Empirical Cumulative Distribution plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_ecdf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "empirical-cumulative-distribution", dir="LDgaussianunivariate")
})

test_that("Histogram plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "histogram", dir="LDgaussianunivariate")
})

test_that("Observed Moments table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_moments"]][["data"]]
  expect_equal_tables(table,
                      list(0.130415109675896, 1, 0.130415109675896, 0.82808243717973, 2,
                           0.845090538011506, -0.047697887796715, 3, 0.278503611135073,
                           2.01019716268452, 4, 2.07010899463661, -0.292701501538923, 5,
                           1.02839194368045, 7.13443207941017, 6, 7.4197225223246, -1.68338958372109,
                           7, 4.88136372180672, 29.9772236055153, 8, 31.6228767537321,
                           -9.62585956073692, 9, 25.8569456873756, 137.395432416605, 10,
                           147.769982264367))
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  expect_equal_tables(table,
                      list(2.40161776050478, 0.130415109675896, 0.113909160788544, -2.2146998871775,
                           -0.494242549079377, 0.70433710614055, 100, 0.914574713318793,
                           0.836446906242152, "Normal100(mu=0,sigma=1)"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.130422052279253, -0.0479317525657895, "<unicode>", 0.0909985113256541,
                           0.308775857124295, 0.828072906238923, 0.598550253333547, "<unicode><unicode>",
                           0.117105546181369, 1.0575955591443))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDgaussianunivariate")
})

test_that("Histogram vs. Theoretical PDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "histogram-vs-theoretical-pdf", dir="LDgaussianunivariate")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.99278432447795, 0.0429230630813024, "Kolmogorov-Smirnov", 0.99519059575295,
                           0.0217999835633844, "Cram<unicode>r-von Mises", 0.999444838973091,
                           0.134303182925592, "Anderson-Darling", 0.994211531885932, 0.996119141722174,
                           "Shapiro-Wilk"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "p-p-plot", dir="LDgaussianunivariate")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "q-q-plot", dir="LDgaussianunivariate")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCDF"]][["collection"]][["plotCDF_cdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDgaussianunivariate")
})

test_that("Density Plot matches", {
  plotName <- results[["results"]][["plotPDF"]][["collection"]][["plotPDF_pdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "density-plot", dir="LDgaussianunivariate")
})

test_that("Quantile Plot matches", {
  plotName <- results[["results"]][["plotQF"]][["collection"]][["plotQF_qfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "quantile-plot", dir="LDgaussianunivariate")
})