context("SummaryStatsTTestBayesianOneSample")

options <- jasptools::analysisOptions("SummaryStatsTTestBayesianOneSample")
options$n1Size <- 25
options$plotBayesFactorRobustness <- TRUE
options$plotPriorAndPosterior <- TRUE
options$tStatistic <- 1.8
set.seed(1)
results <- jasptools::run("SummaryStatsTTestBayesianOneSample", "test.csv", options)


test_that("Bayesian One Sample T-Test table results match", {
	table <- results[["results"]][["table"]][["data"]]
	expect_equal_tables(table,
		list(0.853217271320418, 1.8, 25, 6.13707302160764e-05, 0.0844448509229511
			))
})

test_that("Prior and Posterior plot matches", {
	plotName <- results[["results"]][["inferentialPlots"]][["PriorPosteriorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsTTestBayesianOneSample")
})

test_that("Bayes Factor Robustness Check plot matches", {
	plotName <- results[["results"]][["inferentialPlots"]][["BFrobustnessPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsTTestBayesianOneSample")
})