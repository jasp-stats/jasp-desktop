context("[Audit] Classical Evaluation")

### TEST 1: EVALUATION USING SUMMARY STATISTICS

options <- jasptools::analysisOptions("auditClassicalEvaluation")
options$.meta <- list(auditResult = list(containsColumn = TRUE), monetaryVariable = list(
    containsColumn = TRUE), recordNumberVariable = list(containsColumn = TRUE), 
    sampleCounter = list(containsColumn = TRUE))
options$CR <- "Medium"
options$IR <- "High"
options$correlationPlot <- FALSE
options$estimator <- "stringerBound"
options$estimator2 <- "binomialBound"
options$evaluationInformation <- TRUE
options$kSumStats <- 1
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.05
options$mostLikelyError <- TRUE
options$nSumStats <- 93
options$populationSize <- 3500
options$populationValue <- 1200000
options$useSumStats <- TRUE
options$variableType <- "variableTypeCorrect"
set.seed(1)
dataset <- NULL
results <- jasptools::run("auditClassicalEvaluation", dataset, options)


test_that("Test 1: Evaluation Information plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationInformation"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 1: evaluation-information", dir="auditClassicalEvaluation")
})

test_that("Test 1: <b>Table 1.</b> Evaluation Summary results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationTable"]][["data"]]
	expect_equal_tables(table,
		list("4.354%", 1, "5%", "1.075%", 93, 1))
})

### TEST 2: EVALUATION USING CORRECT / INCORRECT

options <- jasptools::analysisOptions("auditClassicalEvaluation")
options$.meta <- list(auditResult = list(containsColumn = TRUE), monetaryVariable = list(
    containsColumn = TRUE), recordNumberVariable = list(containsColumn = TRUE), 
    sampleCounter = list(containsColumn = TRUE))
options$CR <- "High"
options$IR <- "High"
options$auditResult <- "TARGET_Adjusted"
options$correlationPlot <- FALSE
options$estimator <- "stringerBound"
options$estimator2 <- "poissonBound"
options$evaluationInformation <- TRUE
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.05
options$mostLikelyError <- TRUE
options$populationSize <- 2000
options$recordNumberVariable <- "ID"
options$variableType <- "variableTypeCorrect"
set.seed(1)
results <- jasptools::run("auditClassicalEvaluation", "auditRattle.csv", options)


test_that("Test 2: Evaluation Information plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationInformation"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 2: evaluation-information", dir="auditClassicalEvaluation")
})

test_that("Test 2: <b>Table 1.</b> Evaluation Summary results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationTable"]][["data"]]
	expect_equal_tables(table,
		list("65.451%", 42, "5%", "50.602%", 83, 42))
})

### TEST 3: EVALUATION USING AUDIT VALUES

options <- jasptools::analysisOptions("auditClassicalEvaluation")
options$.meta <- list(auditResult = list(containsColumn = TRUE), monetaryVariable = list(
    containsColumn = TRUE), recordNumberVariable = list(containsColumn = TRUE), 
    sampleCounter = list(containsColumn = TRUE))
options$CR <- "High"
options$IR <- "High"
options$auditResult <- "auditValue"
options$estimator <- "stringerBound"
options$estimator2 <- "binomialBound"
options$evaluationInformation <- TRUE
options$materiality <- "materialityAbsolute"
options$materialityValue <- 70161
options$monetaryVariable <- "bookValue"
options$mostLikelyError <- TRUE
options$populationSize <- 3500
options$populationValue <- 1200000
options$recordNumberVariable <- "ID"
options$sampleCounter <- "selectionResult"
options$variableType <- "variableTypeAuditValues"
set.seed(1)
results <- jasptools::run("auditClassicalEvaluation", "test-auditClassicalWorkflow.csv", options)


test_that("Test 3: Correlation Plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_correlationPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 3: correlation-plot", dir="auditClassicalEvaluation")
})

test_that("Test 3: Evaluation Information plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationInformation"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "Test 3: evaluation-information", dir="auditClassicalEvaluation")
})

test_that("Test 3: <b>Table 1.</b> Evaluation Summary results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationTable"]][["data"]]
	expect_equal_tables(table,
		list("3.173%", 5, "$ 70161", "$ 15384.633", "$ 38073.9", 234, 3))
})
