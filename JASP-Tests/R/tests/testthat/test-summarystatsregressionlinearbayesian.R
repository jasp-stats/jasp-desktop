context("SummaryStatsRegressionLinearBayesian")

options <- jasptools::analysisOptions("SummaryStatsRegressionLinearBayesian")
options$numberOfCovariatesAlternative <- 5
options$numberOfCovariatesNull <- 3
options$plotBayesFactorRobustness <- TRUE
options$sampleSize <- 30
options$unadjustedRSquaredAlternative <- 0.8
options$unadjustedRSquaredNull <- 0.2
set.seed(1)
results <- jasptools::run("SummaryStatsRegressionLinearBayesian", "test.csv", options)


test_that("Bayes Factor Robustness Check plot matches", {
	plotName <- results[["results"]][["inferentialPlots"]][["BFrobustnessPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsRegressionLinearBayesian")
})

test_that("Model Comparison table results match", {
	table <- results[["results"]][["table"]][["data"]]
	expect_equal_tables(table,
		list(3, "Null model", 0.2, 4.40008152727808e-06, 1.3254648491404e-05,
			 "Alternative model", 5, 0.8, 227268.516231018, 1.54506020907333e-08
			))
})