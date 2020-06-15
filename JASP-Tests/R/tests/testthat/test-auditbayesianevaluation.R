context("[Audit] Bayesian Evaluation")

### TEST 1: EVALUATION USING SUMMARY STATISTICS

options <- jasptools::analysisOptions("auditBayesianEvaluation")
options$.meta <- list(auditResult = list(containsColumn = TRUE), monetaryVariable = list(
    containsColumn = TRUE), recordNumberVariable = list(containsColumn = TRUE), 
    sampleCounter = list(containsColumn = TRUE))
options$CR <- "Medium"
options$IR <- "High"
options$areaUnderPosterior <- "displayCredibleBound"
options$bayesFactor <- TRUE
options$estimator <- "betaBound"
options$evaluationInformation <- TRUE
options$evidenceRatio <- TRUE
options$expectedErrors <- "expectedRelative"
options$expectedPercentage <- 0.0125
options$kSumStats <- 1
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.05
options$mostLikelyError <- TRUE
options$nSumStats <- 80
options$populationSize <- 3500
options$populationValue <- 1200000
options$priorAndPosteriorPlot <- TRUE
options$priorAndPosteriorStatistics <- TRUE
options$shadePosterior <- "shadePosteriorCredibleRegion"
options$useSumStats <- TRUE
options$variableType <- "variableTypeCorrect"
set.seed(1)
dataset <- NULL
results <- jasptools::run("auditBayesianEvaluation", dataset, options)


test_that("Test 1: Evaluation Information plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationInformation"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 1: evaluation-information", dir="auditBayesianEvaluation")
})

test_that("Test 1: <b>Table 1.</b> Evaluation Summary results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationTable"]][["data"]]
	expect_equal_tables(table,
		list(15.34, "4.996%", 19.07, 1, "5%", "1.247%", 80, 1))
})

test_that("Test 1: Prior and Posterior Distribution plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_priorAndPosteriorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 1: prior-and-posterior-distribution", dir="auditBayesianEvaluation")
})

test_that("Test 1: <b>Table 2.</b> Prior and Posterior Descriptive Statistics results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_priorAndPosteriorStatistics"]][["data"]]
	expect_equal_tables(table,
		list("Beta(<unicode><unicode> = 1.25, <unicode><unicode> = 20.75)",
			 "15.2924%", 0.45, 0.55, 1.24, "Prior", "Beta(<unicode><unicode> = 2.25, <unicode><unicode> = 100)",
			 "4.996%", 0.05, 0.95, 19.07, "Posterior", "", "", 0.11, 1.73,
			 15.34, "Shift"))
})

### TEST 2: EVALUATION USING CORRECT / INCORRECT

options <- jasptools::analysisOptions("auditBayesianEvaluation")
options$.meta <- list(auditResult = list(containsColumn = TRUE), monetaryVariable = list(
    containsColumn = TRUE), recordNumberVariable = list(containsColumn = TRUE), 
    sampleCounter = list(containsColumn = TRUE))
options$CR <- "High"
options$IR <- "High"
options$areaUnderPosterior <- "displayCredibleInterval"
options$auditResult <- "TARGET_Adjusted"
options$bayesFactor <- TRUE
options$estimator <- "betaBound"
options$evaluationInformation <- TRUE
options$evidenceRatio <- TRUE
options$expectedErrors <- "expectedRelative"
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.2
options$mostLikelyError <- TRUE
options$populationSize <- 2000
options$priorAndPosteriorPlot <- TRUE
options$priorAndPosteriorPlotLimit <- 1
options$priorAndPosteriorStatistics <- TRUE
options$recordNumberVariable <- "ID"
options$shadePosterior <- "shadePosteriorCredibleRegion"
options$variableType <- "variableTypeCorrect"
set.seed(1)
results <- jasptools::run("auditBayesianEvaluation", "auditRattle.csv", options)


test_that("Test 2: Evaluation Information plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationInformation"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 2: evaluation-information", dir="auditBayesianEvaluation")
})

test_that("Test 2: <b>Table 1.</b> Evaluation Summary results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationTable"]][["data"]]
	expect_equal_tables(table,
		list(0, 0, 42, "40.035%", "20%", "50.602%", 83, 42, "61.115%"))
})

test_that("Test 2: Prior and Posterior Distribution plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_priorAndPosteriorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 2: prior-and-posterior-distribution", dir="auditBayesianEvaluation")
})

test_that("Test 2: <b>Table 2.</b> Prior and Posterior Descriptive Statistics results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_priorAndPosteriorStatistics"]][["data"]]
	expect_equal_tables(table,
		list("Beta(<unicode><unicode> = 1, <unicode><unicode> = 1)", "95%",
			 0.8, 0.2, 0.25, "Prior", "Beta(<unicode><unicode> = 43, <unicode><unicode> = 42)",
			 "59.4552%", 1, 0, 0, "Posterior", "", "", 1.25, 0, 0, "Shift"
			))
})

### TEST 3: EVALUATION USING AUDIT VALUES

options <- jasptools::analysisOptions("auditBayesianEvaluation")
options$.meta <- list(auditResult = list(containsColumn = TRUE), monetaryVariable = list(
    containsColumn = TRUE), recordNumberVariable = list(containsColumn = TRUE), 
    sampleCounter = list(containsColumn = TRUE))
options$CR <- "Medium"
options$IR <- "High"
options$areaUnderPosterior <- "displayCredibleBound"
options$auditResult <- "auditValue"
options$bayesFactor <- TRUE
options$correlationPlot <- TRUE
options$estimator <- "betaBound"
options$evaluationInformation <- TRUE
options$evidenceRatio <- TRUE
options$expectedErrors <- "expectedRelative"
options$expectedPercentage <- 0.005
options$materiality <- "materialityAbsolute"
options$materialityValue <- 70161
options$monetaryVariable <- "bookValue"
options$mostLikelyError <- TRUE
options$populationSize <- 3500
options$populationValue <- 1400000
options$priorAndPosteriorPlot <- TRUE
options$priorAndPosteriorStatistics <- TRUE
options$recordNumberVariable <- "ID"
options$sampleCounter <- "selectionResult"
options$shadePosterior <- "shadePosteriorHypotheses"
options$variableType <- "variableTypeAuditValues"
set.seed(1)
results <- jasptools::run("auditBayesianEvaluation", "test-auditClassicalWorkflow.csv", options)


test_that("Test 3: Correlation Plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_correlationPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 3: correlation-plot", dir="auditBayesianEvaluation")
})

test_that("Test 3: Evaluation Information plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationInformation"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 3: evaluation-information", dir="auditBayesianEvaluation")
})

test_that("Test 3: <b>Table 1.</b> Evaluation Summary results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationTable"]][["data"]]
	expect_equal_tables(table,
		list(723.75, "3.133%", 672.1, 5, "$ 70161", "$ 17367.918", "$ 43867.51",
			 234, 3))
})

test_that("Test 3: Prior and Posterior Distribution plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_priorAndPosteriorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 3: prior-and-posterior-distribution", dir="auditBayesianEvaluation")
})

test_that("Test 3: <b>Table 2.</b> Prior and Posterior Descriptive Statistics results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_priorAndPosteriorStatistics"]][["data"]]
	expect_equal_tables(table,
		list("Beta(<unicode><unicode> = 1.065, <unicode><unicode> = 13.935)",
			 "20.0224%", 0.52, 0.48, 0.93, "Prior", "Beta(<unicode><unicode> = 4.065, <unicode><unicode> = 245)",
			 "3.1334%", 0, 1, 672.1, "Posterior", "", "", 0, 2.08, 723.75,
			 "Shift"))
})