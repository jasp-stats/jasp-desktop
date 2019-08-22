context("SummaryStatsBinomialTestBayesian")

options <- jasptools::analysisOptions("SummaryStatsBinomialTestBayesian")
options$betaPriorParamA <- 2
options$betaPriorParamB <- 3
options$failures <- 45
options$plotPriorAndPosterior <- TRUE
options$successes <- 55
options$testValue <- 0.6
set.seed(1)
results <- jasptools::run("SummaryStatsBinomialTestBayesian", "test.csv", options)


test_that("Prior and Posterior plot matches", {
	plotName <- results[["results"]][["inferentialPlots"]][["PriorPosteriorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsBinomialTestBayesian")
})

test_that("Bayesian Binomial Test table results match", {
	table <- results[["results"]][["table"]][["data"]]
	expect_equal_tables(table,
		list(0.275361608438223, 55, 45, 0.6, 0.309238161634108))
})