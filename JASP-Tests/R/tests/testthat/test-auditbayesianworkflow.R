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
		list(2244.4, 510.195917159763, 481.16, 52.89, 2191.51, 244.237380783372,
		     169, 59651.8981719217, "bookValue"))
})

test_that("<b>Table 7.</b> Selected Observations results match", {
	table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_sampleTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 71769, 4, 431.87, 1, 43450, 37, 508.55, 1, 74733, 65, 509.36,
                           1, 38968, 72, 635.28, 1, 82532, 87, 540.99, 1, 46093, 124, 243.42,
                           1, 93820, 140, 845.91, 1, 37626, 166, 544.69, 1, 60975, 194,
                           566.94, 1, 34540, 197, 337.09, 1, 4260, 220, 428.45, 1, 48325,
                           245, 270.19, 1, 63859, 261, 411.31, 1, 4913, 290, 464.32, 1,
                           59363, 305, 542.45, 1, 56338, 324, 465.67, 1, 13682, 350, 441.66,
                           1, 26208, 379, 368.94, 1, 75887, 392, 739, 1, 48687, 420, 471.73,
                           1, 8911, 442, 585.23, 1, 25823, 456, 323.42, 1, 12836, 466,
                           455.82, 1, 71864, 487, 201.07, 1, 53945, 506, 846.85, 1, 84536,
                           539, 295.35, 1, 97331, 560, 227.75, 1, 63201, 590, 376.79, 1,
                           32309, 594, 665.25, 1, 75034, 633, 406.42, 1, 15871, 643, 69.81,
                           1, 85834, 655, 665.15, 1, 86512, 683, 484.48, 1, 65772, 701,
                           538.94, 1, 64893, 719, 234.73, 1, 4261, 752, 214.07, 1, 35085,
                           778, 291.58, 1, 54656, 804, 544.05, 1, 45922, 818, 1225.78,
                           1, 30468, 841, 363.95, 1, 57963, 873, 87.18, 1, 47761, 884,
                           452.95, 1, 39231, 916, 381.7, 1, 84650, 932, 256.54, 1, 89798,
                           959, 208.03, 1, 11929, 971, 850.14, 1, 48630, 996, 678.42, 1,
                           47602, 1006, 203.09, 1, 86438, 1047, 758.85, 1, 99993, 1064,
                           732.09, 1, 57043, 1074, 642.68, 1, 85233, 1098, 686.1, 1, 39852,
                           1124, 278.56, 1, 14979, 1148, 169.77, 1, 64189, 1166, 289.17,
                           1, 54233, 1176, 319, 1, 61808, 1191, 220.35, 1, 12877, 1211,
                           52.89, 1, 57555, 1231, 410.97, 1, 48046, 1248, 539.13, 1, 53993,
                           1282, 744.55, 1, 40156, 1293, 279.08, 1, 10168, 1323, 558.19,
                           1, 52733, 1349, 589.18, 1, 8070, 1368, 938.12, 1, 83336, 1383,
                           656.34, 1, 13472, 1405, 360.05, 1, 25900, 1411, 335.35, 1, 67036,
                           1448, 360.85, 1, 47527, 1468, 299.8, 1, 8019, 1489, 769.5, 1,
                           26904, 1503, 498.97, 1, 75412, 1509, 343.75, 1, 50334, 1540,
                           441.46, 1, 89188, 1545, 305.53, 1, 26926, 1585, 586.31, 1, 97834,
                           1590, 294.9, 1, 52590, 1626, 540.05, 1, 71425, 1642, 228.45,
                           1, 93835, 1665, 457.25, 1, 35289, 1697, 304.04, 1, 29042, 1719,
                           284.1, 1, 93051, 1726, 450.88, 1, 69825, 1741, 745.85, 1, 24364,
                           1773, 460.75, 1, 14140, 1786, 775.99, 1, 41689, 1821, 639.42,
                           1, 8446, 1836, 373.85, 1, 38576, 1849, 484.25, 1, 78011, 1877,
                           583.89, 1, 23139, 1902, 820.33, 1, 26261, 1913, 661.28, 1, 35832,
                           1928, 592.51, 1, 56012, 1949, 581.22, 1, 12380, 1969, 469.55,
                           1, 37408, 2001, 711.79, 1, 2933, 2009, 517.89, 1, 7826, 2034,
                           879.18, 1, 61616, 2050, 627.28, 1, 91323, 2069, 566.32, 1, 23624,
                           2096, 876.04, 1, 11044, 2108, 821.93, 1, 77731, 2123, 307.83,
                           1, 25056, 2149, 344.4, 1, 11006, 2170, 337.57, 1, 81326, 2178,
                           839.21, 1, 48009, 2197, 931.1, 1, 12855, 2225, 481.16, 1, 17423,
                           2244, 598.4, 1, 68502, 2264, 494.87, 1, 77556, 2282, 529.52,
                           1, 93548, 2306, 439.82, 1, 6230, 2333, 349.17, 1, 84318, 2341,
                           292.3, 1, 90643, 2362, 428.34, 1, 31708, 2380, 515.65, 1, 76656,
                           2400, 497.55, 1, 42588, 2412, 340.33, 1, 96959, 2441, 712.32,
                           1, 95755, 2453, 295.58, 1, 63825, 2473, 795.72, 1, 39672, 2514,
                           463.91, 1, 15748, 2529, 423.05, 1, 53789, 2552, 444.43, 1, 22829,
                           2559, 229.41, 1, 99146, 2574, 353.19, 1, 42016, 2603, 515.66,
                           1, 78909, 2622, 559.53, 1, 72195, 2631, 885.24, 1, 39805, 2656,
                           617.63, 1, 65347, 2687, 399.5, 1, 58965, 2704, 790.46, 1, 50938,
                           2723, 579.02, 1, 8983, 2761, 423.66, 1, 98790, 2768, 558.36,
                           1, 8201, 2781, 482.02, 1, 27035, 2808, 597.71, 1, 85267, 2835,
                           294.68, 1, 21836, 2856, 820.86, 1, 77036, 2884, 569.36, 1, 27181,
                           2904, 156.65, 1, 15776, 2924, 791.64, 1, 3514, 2951, 427.68,
                           1, 47090, 2986, 368.51, 1, 77867, 3001, 374.63, 1, 30993, 3021,
                           318.49, 1, 83722, 3036, 569.1, 1, 29378, 3061, 386.56, 1, 52609,
                           3081, 432.09, 1, 91738, 3102, 897.3, 1, 17092, 3121, 506.66,
                           1, 21683, 3145, 475.98, 1, 90226, 3167, 597.9, 1, 61036, 3171,
                           387.67, 1, 41420, 3203, 689.6, 1, 78693, 3226, 538.61, 1, 94620,
                           3243, 886.12, 1, 29389, 3257, 587.66, 1, 47392, 3279, 455.65,
                           1, 73719, 3314, 469.12, 1, 53121, 3333, 287.85, 1, 19410, 3348,
                           891.58, 1, 91263, 3362, 659.14, 1, 99612, 3394, 2244.4, 1, 7883,
                           3411, 220.68, 1, 70852, 3428, 606.14, 1, 33411, 3445, 239.4,
                           1, 56247, 3463, 571.88, 1, 97423, 3494, 827.96))
})

test_that("<b>Table 5.</b> Selection Summary results match", {
	table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_selectionInformationTable"]][["data"]]
	expect_equal_tables(table,
		list("$ 8304", "6.14%", 169, "$ 86224"))
})
