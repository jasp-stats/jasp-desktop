context("[Audit] Bayesian Planning")

### TEST 1: PLAN USING BETA DISTRIBUTION

options <- jaspTools::analysisOptions("auditBayesianPlanning")
options$.meta <- list()
options$CR <- "High"
options$IR <- "High"
options$decisionPlot <- TRUE
options$expectedEvidenceRatio <- TRUE
options$expectedBayesFactor <- TRUE
options$expectedErrors <- "expectedRelative"
options$implicitSampleTable <- TRUE
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.05
options$planningModel <- "binomial"
options$populationSize <- 1000
options$priorPlot <- TRUE
options$shadePrior <- "shadePriorCredibleRegion"
options$priorPlotExpectedPosterior <- TRUE
options$priorStatistics <- TRUE
options$valuta <- "euroValuta"
set.seed(1)
dataset <- NULL
results <- jaspTools::run("auditBayesianPlanning", dataset, options)


test_that("Test 1: Sample Size Comparison plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 1: sample-size-comparison", dir="auditBayesianPlanning")
})

test_that("Test 1: Implied Prior from Risk Assessments plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 1: implied-prior-from-risk-assessments", dir="auditBayesianPlanning")
})

test_that("Test 1: <b>Table 3.</b> Prior and Expected Posterior Descriptive Statistics results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorStatistics"]][["data"]]
	expect_equal_tables(table,
		list("Beta(<unicode><unicode> = 1, <unicode><unicode> = 1)", "95%",
			 0.95, 0.05, 0.05, "Prior", "Beta(<unicode><unicode> = 1, <unicode><unicode> = 59)",
			 "4.9508%", 0.05, 0.95, 19.62, "Expected posterior", "", "",
			 0.05, 19, 372.8, "Expected shift"))
})

test_that("Test 1: <b>Table 2.</b> Implicit Sample results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_sampletable"]][["data"]]
	expect_equal_tables(table,
		list(0, 0))
})

test_that("Test 1: <b>Table 1.</b> Planning Summary results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
	expect_equal_tables(table,
		list("100%", "5%", "100%", 372.8, 19.62, 0, "5%", 58))
})

### TEST 2: PLAN USING GAMMA DISTRIBUTION

options <- jaspTools::analysisOptions("auditBayesianPlanning")
options$.meta <- list()
options$CR <- "Medium"
options$IR <- "High"
options$decisionPlot <- TRUE
options$expectedBayesFactor <- TRUE
options$expectedErrors <- "expectedRelative"
options$expectedEvidenceRatio <- TRUE
options$implicitSampleTable <- TRUE
options$materiality <- "materialityAbsolute"
options$materialityValue <- 70161
options$planningModel <- "Poisson"
options$populationSize <- 3500
options$populationValue <- 1400000
options$priorPlot <- TRUE
options$priorPlotExpectedPosterior <- TRUE
options$priorPlotLimit <- 0.5
options$priorStatistics <- TRUE
options$shadePrior <- "shadePriorCredibleRegion"
options$valuta <- "dollarValuta"
set.seed(1)
dataset <- NULL
results <- jaspTools::run("auditBayesianPlanning", dataset, options)


test_that("Test 2: Sample Size Comparison plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 2: sample-size-comparison", dir="auditBayesianPlanning")
})

test_that("Test 2: Implied Prior from Risk Assessments plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 2: implied-prior-from-risk-assessments", dir="auditBayesianPlanning")
})

test_that("Test 2: <b>Table 3.</b> Prior and Expected Posterior Descriptive Statistics results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorStatistics"]][["data"]]
	expect_equal_tables(table,
		list("Gamma(<unicode><unicode> = 1, <unicode><unicode> = 10)", "29.9573%",
			 0.61, 0.39, 0.65, "Prior", "Gamma(<unicode><unicode> = 1, <unicode><unicode> = 60)",
			 "4.9929%", 0.05, 0.95, 19.22, "Expected posterior", "", "",
			 0.08, 2.44, 29.55, "Expected shift"))
})

test_that("Test 2: <b>Table 2.</b> Implicit Sample results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_sampletable"]][["data"]]
	expect_equal_tables(table,
		list(0, 10))
})

test_that("Test 2: <b>Table 1.</b> Planning Summary results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
	expect_equal_tables(table,
		list("60%", "8.33%", "100%", 29.55, 19.22, 0, "$ 70161", 50))
})

### TEST 3: PLAN USING BETA-BINOMIAL DISTRIBUTION

options <- jaspTools::analysisOptions("auditBayesianPlanning")
options$.meta <- list()
options$CR <- "Medium"
options$IR <- "High"
options$decisionPlot <- TRUE
options$expectedBayesFactor <- TRUE
options$expectedErrors <- "expectedRelative"
options$expectedEvidenceRatio <- TRUE
options$implicitSampleTable <- TRUE
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.1
options$materialityValue <- 70161
options$planningModel <- "hypergeometric"
options$populationSize <- 200
options$populationValue <- 1400000
options$priorPlot <- TRUE
options$priorPlotExpectedPosterior <- TRUE
options$priorPlotLimit <- 0.5
options$priorStatistics <- TRUE
options$shadePrior <- "shadePriorCredibleRegion"
options$valuta <- "dollarValuta"
set.seed(1)
dataset <- NULL
results <- jaspTools::run("auditBayesianPlanning", dataset, options)


test_that("Test 3: Sample Size Comparison plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 3: sample-size-comparison", dir="auditBayesianPlanning")
})

test_that("Test 3: Implied Prior from Risk Assessments plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 3: implied-prior-from-risk-assessments", dir="auditBayesianPlanning")
})

test_that("Test 3: <b>Table 3.</b> Prior and Expected Posterior Descriptive Statistics results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorStatistics"]][["data"]]
	expect_equal_tables(table,
		list("Beta-binomial(N = 177, <unicode><unicode> = 1, <unicode><unicode> = 5)",
			 81, 0.54, 0.46, 0.86, "Prior", "Beta-binomial(N = 177, <unicode><unicode> = 1, <unicode><unicode> = 28)",
			 19, 0.04, 0.96, 25.02, "Expected posterior", "", "", 0.07, 2.09,
			 29.11, "Expected shift"))
})

test_that("Test 3: <b>Table 2.</b> Implicit Sample results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_sampletable"]][["data"]]
	expect_equal_tables(table,
		list(0, 4))
})

test_that("Test 3: <b>Table 1.</b> Planning Summary results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
	expect_equal_tables(table,
		list("60%", "8.33%", "100%", 29.11, 25.02, 0, "10%", 23))
})