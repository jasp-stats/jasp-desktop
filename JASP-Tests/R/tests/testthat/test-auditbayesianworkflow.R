context("[Audit] Bayesian Workflow")

options <- jasptools::analysisOptions("auditBayesianWorkflow")
options$.meta <- list(additionalVariables = list(containsColumn = TRUE), auditResult = list(
  containsColumn = TRUE), monetaryVariable = list(containsColumn = TRUE), 
  performAudit = list(list(dataCols = list(containsColumn = TRUE), 
                           extraCol = list(containsColumn = TRUE))), rankingVariable = list(
                             containsColumn = TRUE), recordNumberVariable = list(containsColumn = TRUE), 
  sampleFilter = list(containsColumn = TRUE), variableName = list(
    containsColumn = TRUE))
options$CR <- "Medium"
options$IR <- "High"
options$areaUnderPosterior <- "displayCredibleBound"
options$auditResult <- "auditValue"
options$evidenceRatio <- TRUE
options$bookValueDescriptives <- TRUE
options$bookValueDistribution <- TRUE
options$bayesFactor <- TRUE
options$decisionPlot <- TRUE
options$correlationPlot <- TRUE
options$displaySample <- TRUE
options$estimator <- "betaBound"
options$evaluationChecked <- TRUE
options$evaluationInformation <- TRUE
options$expectedEvidenceRatio <- TRUE
options$expectedBayesFactor <- TRUE
options$expectedErrors <- "expectedRelative"
options$expectedPercentage <- 0.025
options$implicitSampleTable <- TRUE
options$materiality <- "materialityAbsolute"
options$materialityValue <- 70161
options$max <- TRUE
options$min <- TRUE
options$monetaryVariable <- "bookValue"
options$mostLikelyError <- TRUE
options$performAudit <- list(list(colName = "auditResult", dataCols = c("ID", "bookValue"
), extraCol = "selectionResult", filter = "selectionResult > 0", 
rowIndices = c(4, 38, 65, 72, 87, 124, 141, 166, 194, 198, 
               221, 245, 261, 290, 306, 325, 351, 380, 392, 421, 443, 458, 
               466, 488, 506, 540, 561, 590, 595, 634, 644, 657, 684, 701, 
               721, 753, 779, 805, 819, 842, 875, 886, 917, 934, 961, 972, 
               999, 1008, 1047, 1065, 1076, 1099, 1126, 1149, 1168, 1177, 
               1192, 1213, 1233, 1249, 1283, 1294, 1325, 1350, 1368, 1385, 
               1407, 1413, 1451, 1470, 1491, 1505, 1512, 1542, 1547, 1590, 
               1592, 1628, 1645, 1668, 1700, 1721, 1729, 1744, 1777, 1787, 
               1824, 1840, 1851, 1880, 1905, 1915, 1930, 1951, 1972, 2003, 
               2011, 2036, 2052, 2072, 2097, 2111, 2127, 2151, 2172, 2180, 
               2198, 2228, 2248, 2267, 2285, 2310, 2336, 2346, 2365, 2383, 
               2403, 2416, 2444, 2458, 2476, 2516, 2532, 2555, 2563, 2577, 
               2606, 2624, 2636, 2660, 2691, 2707, 2726, 2763, 2773, 2787, 
               2811, 2841, 2860, 2888, 2909, 2928, 2957, 2989, 3006, 3026, 
               3040, 3064, 3085, 3108, 3124, 3149, 3170, 3175, 3207, 3230, 
               3247, 3261, 3283, 3319, 3339, 3352, 3368, 3399, 3416, 3432, 
               3451, 3467, 3499), values = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                             0, 0, 0, 0, 0, 0, 0, 0)))
options$planningModel <- "binomial"
options$priorAndPosteriorPlot <- TRUE
options$priorAndPosteriorPlotExpectedPosterior <- TRUE
options$shadePosterior <- "shadePosteriorCredibleRegion"
options$priorAndPosteriorPlotLimit <- 0.2
options$priorAndPosteriorStatistics <- TRUE
options$priorPlot <- TRUE
options$priorPlotExpectedPosterior <- TRUE
options$shadePrior <- "shadePriorCredibleRegion"
options$priorStatistics <- TRUE
options$recordNumberVariable <- "ID"
options$sampleDescriptives <- TRUE
options$sampleFilter <- "selectionResult"
options$samplingChecked <- TRUE
options$seed <- 5
options$selectionMethod <- "cellSampling"
options$selectionType <- "musSampling"
options$valuta <- "dollarValuta"
options$var <- TRUE
options$variableName <- "auditResult"
options$variableType <- "variableTypeAuditValues"
set.seed(1)
results <- jasptools::run("auditBayesianWorkflow", "test-auditBayesianWorkflow.csv", options)


test_that("Correlation Plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_correlationPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "correlation-plot", dir="auditBayesianWorkflow")
})

test_that("Evaluation Information plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationInformation"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "evaluation-information", dir="auditBayesianWorkflow")
})

test_that("<b>Table 8.</b> Evaluation Summary results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationTable"]][["data"]]
	expect_equal_tables(table,
		list(108.94, "3.518%", 223.72, 3, "$ 70161", "$ 19500.338", "$ 49370.9",
			 169, 1.8))
})

test_that("Prior and Posterior Distribution plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_priorAndPosteriorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "prior-and-posterior-distribution", dir="auditBayesianWorkflow")
})

test_that("<b>Table 9.</b> Prior and Posterior Descriptive Statistics results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_priorAndPosteriorStatistics"]][["data"]]
	expect_equal_tables(table,
		list("Beta(<unicode><unicode> = 2.275, <unicode><unicode> = 50.725)",
			 "9.6012%", 0.33, 0.67, 2.05, "Prior", "Beta(<unicode><unicode> = 4.075, <unicode><unicode> = 219.2)",
			 "3.5184%", 0, 1, 223.72, "Posterior", "", "", 0, 1.49, 108.94,
			 "Shift"))
})

test_that("Sample Size Comparison plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "sample-size-comparison", dir="auditBayesianWorkflow")
})

test_that("Implied Prior from Risk Assessments plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "implied-prior-from-risk-assessments", dir="auditBayesianWorkflow")
})

test_that("<b>Table 4.</b> Prior and Expected Posterior Descriptive Statistics results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorStatistics"]][["data"]]
	expect_equal_tables(table,
		list("Beta(<unicode><unicode> = 2.275, <unicode><unicode> = 50.725)",
			 "9.6012%", 0.33, 0.67, 2.05, "Prior", "Beta(<unicode><unicode> = 6.505, <unicode><unicode> = 215.495)",
			 "4.9974%", 0.05, 0.95, 19.07, "Expected posterior", "", "",
			 0.15, 1.42, 9.28, "Expected shift"))
})

test_that("<b>Table 3.</b> Implicit Sample results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_sampletable"]][["data"]]
	expect_equal_tables(table,
		list(1.275, 51))
})

test_that("<b>Table 2.</b> Planning Summary results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
	expect_equal_tables(table,
		list("60%", "8.33%", "100%", 9.28, 19.07, 4.23, "$ 70161", 169))
})

test_that("<b>Table 1.</b> Book Value Descriptive Statistics results match", {
	table <- results[["results"]][["procedureContainer"]][["collection"]][["procedureContainer_bookValueDescriptives"]][["data"]]
	expect_equal_tables(table,
		list("$ 1403220.82", "$ 400.92", 3500, "$ 1403220.82", "$ 254.38",
			 "$ 364.11", "$ 513.26", "$ 200.78"))
})

test_that("Book Value Distribution plot matches", {
	plotName <- results[["results"]][["procedureContainer"]][["collection"]][["procedureContainer_bookValueDistribution"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "book-value-distribution", dir="auditBayesianWorkflow")
})

test_that("<b>Table 6.</b> Selection Descriptive Statistics results match", {
	table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_sampleDescriptivesTable"]][["data"]]
	expect_equal_tables(table,
		list(1025.03, 474.358106508876, 455.08, 122.84, 902.19, 189.801769316514,
			 169, 36024.7116356791, "bookValue"))
})

test_that("<b>Table 7.</b> Selected Observations results match", {
	table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_sampleTable"]][["data"]]
	expect_equal_tables(table,
		list(1, 71769, 4, 431.87, 1, 57172, 38, 329.3, 1, 74733, 65, 509.36,
			 1, 38968, 72, 635.28, 1, 82532, 87, 540.99, 1, 46093, 124, 243.42,
			 1, 58999, 141, 568.6, 1, 37626, 166, 544.69, 1, 60975, 194,
			 566.94, 1, 37012, 198, 570.3, 1, 46828, 221, 277.91, 1, 48325,
			 245, 270.19, 1, 63859, 261, 411.31, 1, 4913, 290, 464.32, 1,
			 11569, 306, 1025.03, 1, 57791, 325, 376, 1, 21337, 351, 272.84,
			 1, 23831, 380, 769.89, 1, 75887, 392, 739, 1, 25839, 421, 294.92,
			 1, 32859, 443, 305.23, 1, 43525, 458, 222.1, 1, 12836, 466,
			 455.82, 1, 50811, 488, 651.35, 1, 53945, 506, 846.85, 1, 23685,
			 540, 424.56, 1, 61936, 561, 530.25, 1, 63201, 590, 376.79, 1,
			 30750, 595, 338.36, 1, 92541, 634, 244.31, 1, 67362, 644, 590.27,
			 1, 21849, 657, 514.78, 1, 90641, 684, 347.52, 1, 65772, 701,
			 538.94, 1, 18869, 721, 280.47, 1, 66560, 753, 331.95, 1, 82046,
			 779, 569.21, 1, 16926, 805, 200.33, 1, 64972, 819, 321.55, 1,
			 84419, 842, 494.53, 1, 82047, 875, 249.68, 1, 85328, 886, 449.07,
			 1, 63335, 917, 377.3, 1, 24081, 934, 372.59, 1, 70217, 961,
			 492.51, 1, 64369, 972, 382.49, 1, 42875, 999, 553.65, 1, 45429,
			 1008, 489.34, 1, 86438, 1047, 758.85, 1, 36766, 1065, 279.94,
			 1, 78091, 1076, 489.4, 1, 30832, 1099, 300.46, 1, 63552, 1126,
			 645.7, 1, 22843, 1149, 716.26, 1, 50193, 1168, 411.34, 1, 76926,
			 1177, 731.92, 1, 37062, 1192, 603.58, 1, 18133, 1213, 432.95,
			 1, 5324, 1233, 503.95, 1, 57764, 1249, 696.32, 1, 9935, 1283,
			 307.34, 1, 39251, 1294, 605.56, 1, 10518, 1325, 247.19, 1, 24727,
			 1350, 715.09, 1, 8070, 1368, 938.12, 1, 20959, 1385, 251.8,
			 1, 12020, 1407, 465.3, 1, 157, 1413, 501.79, 1, 18684, 1451,
			 309.6, 1, 10305, 1470, 648.7, 1, 85526, 1491, 591.02, 1, 48139,
			 1505, 718.58, 1, 54612, 1512, 238.78, 1, 57174, 1542, 449.24,
			 1, 18858, 1547, 416.94, 1, 97834, 1590, 294.9, 1, 81268, 1592,
			 320.05, 1, 75784, 1628, 364.3, 1, 93238, 1645, 276.08, 1, 39990,
			 1668, 788, 1, 54113, 1700, 367.27, 1, 35982, 1721, 829.17, 1,
			 78779, 1729, 608.02, 1, 14398, 1744, 440.64, 1, 23651, 1777,
			 162.18, 1, 24260, 1787, 453.7, 1, 95824, 1824, 184.68, 1, 76633,
			 1840, 259.95, 1, 11978, 1851, 468.95, 1, 97540, 1880, 354.4,
			 1, 55003, 1905, 774.27, 1, 65841, 1915, 533.44, 1, 3524, 1930,
			 559.99, 1, 27769, 1951, 425.57, 1, 47337, 1972, 330.6, 1, 81413,
			 2003, 354.77, 1, 50165, 2011, 846.39, 1, 18587, 2036, 685.75,
			 1, 50642, 2052, 616.12, 1, 92569, 2072, 517.14, 1, 53186, 2097,
			 642.34, 1, 52300, 2111, 293.58, 1, 76164, 2127, 341.01, 1, 11702,
			 2151, 604.13, 1, 36735, 2172, 500.01, 1, 79968, 2180, 544.51,
			 1, 76862, 2198, 447.29, 1, 98556, 2228, 539.99, 1, 44975, 2248,
			 890.46, 1, 67488, 2267, 454.31, 1, 42337, 2285, 784.74, 1, 96158,
			 2310, 469.93, 1, 38977, 2336, 358.77, 1, 42475, 2346, 876, 1,
			 89719, 2365, 549.26, 1, 48478, 2383, 254.6, 1, 65112, 2403,
			 408.75, 1, 47910, 2416, 654.87, 1, 4902, 2444, 430.53, 1, 74773,
			 2458, 244.58, 1, 6122, 2476, 762.65, 1, 68230, 2516, 137.18,
			 1, 96315, 2532, 239.26, 1, 25955, 2555, 832.6, 1, 45868, 2563,
			 222.34, 1, 1308, 2577, 534.6, 1, 23665, 2606, 336.75, 1, 7964,
			 2624, 548.11, 1, 32760, 2636, 599.3, 1, 23927, 2660, 454.81,
			 1, 91787, 2691, 494.26, 1, 1832, 2707, 141.72, 1, 12075, 2726,
			 461.4, 1, 7218, 2763, 420.53, 1, 52382, 2773, 504.95, 1, 10925,
			 2787, 377.1, 1, 70717, 2811, 219.88, 1, 4480, 2841, 216.25,
			 1, 58155, 2860, 229.59, 1, 82785, 2888, 339.57, 1, 97156, 2909,
			 370.44, 1, 68595, 2928, 548.21, 1, 38924, 2957, 279.13, 1, 83073,
			 2989, 652.79, 1, 2571, 3006, 406.14, 1, 90807, 3026, 595.88,
			 1, 60860, 3040, 555.91, 1, 6080, 3064, 722.45, 1, 99720, 3085,
			 654.23, 1, 87494, 3108, 489.8, 1, 94951, 3124, 849.9, 1, 67152,
			 3149, 453.15, 1, 34382, 3170, 450.3, 1, 53877, 3175, 451.91,
			 1, 74858, 3207, 884.51, 1, 74531, 3230, 844.36, 1, 15366, 3247,
			 455.08, 1, 50380, 3261, 180.01, 1, 25908, 3283, 530.74, 1, 66666,
			 3319, 219.33, 1, 26102, 3339, 531.24, 1, 15806, 3352, 163, 1,
			 15, 3368, 475.92, 1, 10213, 3399, 503.69, 1, 24236, 3416, 309.07,
			 1, 10878, 3432, 122.84, 1, 1561, 3451, 388.21, 1, 21104, 3467,
			 801.5, 1, 86390, 3499, 328.26))
})

test_that("<b>Table 5.</b> Selection Summary results match", {
	table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_selectionInformationTable"]][["data"]]
	expect_equal_tables(table,
		list("$ 8304", "5.71%", 169, "$ 80167"))
})