context("SummaryStatsTTestBayesianPairedSamples")

options <- jasptools::analysisOptions("SummaryStatsTTestBayesianPairedSamples")
options$n1Size <- 35
options$plotBayesFactorRobustness <- TRUE
options$plotPriorAndPosterior <- TRUE
options$tStatistic <- 1.3
set.seed(1)
results <- jasptools::run("SummaryStatsTTestBayesianPairedSamples", "test.csv", options)


test_that("Bayesian Paired Samples T-Test table results match", {
	table <- results[["results"]][["table"]][["data"]]
	expect_equal_tables(table,
		list(0.392593651551234, 1.3, 35, 5.20718305010584e-07, 0.202345424721371
			))
})

test_that("Prior and Posterior plot matches", {
	plotName <- results[["results"]][["inferentialPlots"]][["PriorPosteriorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsTTestBayesianPairedSamples")
})

test_that("Bayes Factor Robustness Check plot matches", {
	plotName <- results[["results"]][["inferentialPlots"]][["BFrobustnessPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsTTestBayesianPairedSamples")
})