context("[Audit] Classical Planning")

options <- jaspTools::analysisOptions("auditClassicalPlanning")
options$.meta <- list()
options$CR <- "High"
options$IR <- "High"
options$decisionPlot <- TRUE
options$expectedErrors <- "expectedRelative"
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.05
options$planningModel <- "binomial"
options$populationSize <- 1000
options$samplingDistribution <- TRUE
options$valuta <- "euroValuta"
set.seed(1)
dataset <- NULL
results <- jaspTools::run("auditClassicalPlanning", dataset, options)

### TEST 1: PLAN USING BINOMIAL DISTRIBUTION

test_that("Test 1: Sample Size Comparison plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 1: sample-size-comparison", dir="auditClassicalPlanning")
})

test_that("Test 1: Implied Binomial Sampling Distribution plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_samplingDistribution"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 1: implied-binomial-sampling-distribution", dir="auditClassicalPlanning")
})

test_that("Test 1: <b>Table 1.</b> Planning Summary results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
	expect_equal_tables(table,
		list("100%", "5%", "100%", 0, "5%", 59))
})

### TEST 2: PLAN USING POISSON DISTRIBUTION

options <- jaspTools::analysisOptions("auditClassicalPlanning")
options$.meta <- list()
options$CR <- "High"
options$IR <- "Medium"
options$decisionPlot <- TRUE
options$expectedErrors <- "expectedRelative"
options$materiality <- "materialityAbsolute"
options$materialityValue <- 70161
options$planningModel <- "Poisson"
options$populationSize <- 3500
options$populationValue <- 1200000
options$samplingDistribution <- TRUE
options$valuta <- "euroValuta"
set.seed(1)
dataset <- NULL
results <- jaspTools::run("auditClassicalPlanning", dataset, options)


test_that("Test 2: Sample Size Comparison plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 2: sample-size-comparison", dir="auditClassicalPlanning")
})

test_that("Test 2: Implied Poisson Sampling Distribution plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_samplingDistribution"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 2: implied-poisson-sampling-distribution", dir="auditClassicalPlanning")
})

test_that("Test 2: <b>Table 1.</b> Planning Summary results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
	expect_equal_tables(table,
		list("100%", "8.33%", "60%", 0, "<unicode><unicode><unicode> 70161",
			 43))
})

### TEST 3: PLAN USING HYPERGEOMETRIC DISTRIBUTION

options <- jaspTools::analysisOptions("auditClassicalPlanning")
options$.meta <- list()
options$CR <- "High"
options$IR <- "High"
options$decisionPlot <- TRUE
options$expectedErrors <- "expectedRelative"
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.05
options$materialityValue <- 70161
options$planningModel <- "hypergeometric"
options$populationSize <- 200
options$populationValue <- 1200000
options$samplingDistribution <- TRUE
options$valuta <- "euroValuta"
set.seed(1)
dataset <- NULL
results <- jaspTools::run("auditClassicalPlanning", dataset, options)


test_that("Test 3: Sample Size Comparison plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 3: sample-size-comparison", dir="auditClassicalPlanning")
})

test_that("Test 3: Implied Hypergeometric Sampling Distribution plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_samplingDistribution"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 3: implied-hypergeometric-sampling-distribution", dir="auditClassicalPlanning")
})

test_that("Test 3: <b>Table 1.</b> Planning Summary results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
	expect_equal_tables(table,
		list("100%", "5%", "100%", 0, "5%", 51))
})