context("[Audit] Classical Workflow")

options <- jasptools::analysisOptions("auditClassicalWorkflow")
options$.meta <- list(additionalVariables = list(containsColumn = TRUE), auditResult = list(
  containsColumn = TRUE), monetaryVariable = list(containsColumn = TRUE), 
  performAudit = list(list(dataCols = list(containsColumn = TRUE), 
                           extraCol = list(containsColumn = TRUE))), rankingVariable = list(
                             containsColumn = TRUE), recordNumberVariable = list(containsColumn = TRUE), 
  sampleFilter = list(containsColumn = TRUE), variableName = list(
    containsColumn = TRUE))
options$CR <- "High"
options$IR <- "High"
options$auditResult <- "auditValue"
options$bookValueDescriptives <- TRUE
options$bookValueDistribution <- TRUE
options$decisionPlot <- TRUE
options$displaySample <- TRUE
options$estimator <- "stringerBound"
options$evaluationChecked <- TRUE
options$evaluationInformation <- TRUE
options$expectedErrors <- "expectedRelative"
options$expectedPercentage <- 0.025
options$materiality <- "materialityAbsolute"
options$materialityValue <- 70161
options$monetaryVariable <- "bookValue"
options$mostLikelyError <- TRUE
options$performAudit <- list(list(colName = "auditResult", dataCols = c("ID", "bookValue"
), extraCol = "selectionResult", filter = "selectionResult > 0", 
rowIndices = c(1, 15, 31, 49, 64, 77, 91, 110, 126, 140, 
               154, 170, 186, 203, 217, 232, 247, 260, 277, 295, 308, 324, 
               340, 355, 368, 383, 397, 413, 429, 444, 456, 470, 486, 501, 
               516, 535, 551, 568, 583, 595, 611, 626, 640, 657, 672, 689, 
               704, 719, 737, 750, 766, 786, 804, 818, 831, 852, 865, 884, 
               897, 912, 929, 943, 956, 971, 986, 1005, 1020, 1038, 1053, 
               1068, 1080, 1093, 1107, 1121, 1138, 1153, 1167, 1180, 1193, 
               1208, 1225, 1238, 1251, 1268, 1284, 1297, 1312, 1324, 1342, 
               1356, 1368, 1382, 1398, 1413, 1428, 1442, 1457, 1474, 1490, 
               1501, 1513, 1525, 1539, 1556, 1572, 1585, 1602, 1618, 1633, 
               1653, 1669, 1685, 1699, 1715, 1731, 1743, 1759, 1778, 1791, 
               1806, 1819, 1832, 1846, 1864, 1879, 1895, 1910, 1922, 1935, 
               1947, 1963, 1977, 1995, 2008, 2020, 2034, 2047, 2061, 2076, 
               2088, 2102, 2112, 2127, 2140, 2153, 2170, 2181, 2197, 2213, 
               2229, 2244, 2260, 2276, 2289, 2303, 2318, 2334, 2347, 2358, 
               2371, 2388, 2398, 2413, 2426, 2441, 2457, 2472, 2486, 2501, 
               2518, 2531, 2548, 2562, 2575, 2588, 2601, 2612, 2626, 2643, 
               2659, 2681, 2697, 2714, 2730, 2744, 2760, 2771, 2788, 2804, 
               2819, 2834, 2853, 2866, 2883, 2901, 2916, 2935, 2952, 2971, 
               2986, 3001, 3014, 3027, 3042, 3057, 3070, 3087, 3102, 3120, 
               3135, 3149, 3162, 3175, 3192, 3207, 3222, 3235, 3249, 3263, 
               3281, 3299, 3314, 3330, 3348, 3362, 3376, 3389, 3400, 3412, 
               3427, 3442, 3460, 3476, 3489), values = c(0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                         0)))
options$planningModel <- "binomial"
options$range <- FALSE
options$recordNumberVariable <- "ID"
options$sampleDescriptives <- TRUE
options$sampleFilter <- "selectionResult"
options$samplingChecked <- TRUE
options$samplingDistribution <- TRUE
options$selectionMethod <- "systematicSampling"
options$selectionType <- "musSampling"
options$valuta <- "dollarValuta"
options$variableName <- "auditResult"
options$variableType <- "variableTypeAuditValues"
set.seed(1)
results <- jasptools::run("auditClassicalWorkflow", "test-auditClassicalWorkflow.csv", options)


test_that("Correlation Plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_correlationPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "correlation-plot", dir="auditClassicalWorkflow")
})

test_that("Evaluation Information plot matches", {
	plotName <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationInformation"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "evaluation-information", dir="auditClassicalWorkflow")
})

test_that("<b>Table 6.</b> Evaluation Summary results match", {
	table <- results[["results"]][["evaluationContainer"]][["collection"]][["evaluationContainer_evaluationTable"]][["data"]]
	expect_equal_tables(table,
		list("3.173%", 5, "$ 70161", "$ 17990.031", "$ 44521.74", 234, 3))
})

test_that("Sample Size Comparison plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "sample-size-comparison", dir="auditClassicalWorkflow")
})

test_that("Implied Binomial Sampling Distribution plot matches", {
	plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_samplingDistribution"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "implied-binomial-sampling-distribution", dir="auditClassicalWorkflow")
})

test_that("<b>Table 2.</b> Planning Summary results match", {
	table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
	expect_equal_tables(table,
		list("100%", "5%", "100%", 6, "$ 70161", 234))
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
	expect_equal_plots(testPlot, "book-value-distribution", dir="auditClassicalWorkflow")
})

test_that("<b>Table 4.</b> Selection Descriptive Statistics results match", {
	table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_sampleDescriptivesTable"]][["data"]]
	expect_equal_tables(table,
		list(491.571709401709, 469.495, 209.675976994018, 234, "bookValue"
			))
})

test_that("<b>Table 5.</b> Selected Observations results match", {
	table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_sampleTable"]][["data"]]
	expect_equal_tables(table,
	                    list(1, 82884, 1, 242.61, 1, 76073, 15, 469.93, 1, 83557, 31, 507.34,
	                         1, 28052, 49, 490.93, 1, 81126, 64, 623.94, 1, 28956, 77, 462.28,
	                         1, 7224, 91, 354.76, 1, 4756, 110, 295.96, 1, 25996, 126, 434.05,
	                         1, 93820, 140, 845.91, 1, 44198, 154, 765.11, 1, 36834, 170,
	                         603.64, 1, 93367, 186, 585.26, 1, 88668, 203, 274.96, 1, 55391,
	                         216, 593.38, 1, 15340, 232, 460.4, 1, 74299, 246, 417.48, 1,
	                         14001, 260, 524.19, 1, 88999, 277, 488.17, 1, 64172, 295, 556.46,
	                         1, 85801, 308, 214.69, 1, 56338, 324, 465.67, 1, 63311, 339,
	                         227.84, 1, 27934, 355, 749.38, 1, 47856, 367, 214.39, 1, 63599,
	                         382, 452.91, 1, 11499, 396, 140.3, 1, 497, 411, 640.18, 1, 73223,
	                         429, 683.8, 1, 32859, 443, 305.23, 1, 87838, 455, 533.69, 1,
	                         25886, 469, 478.13, 1, 89934, 484, 490.9, 1, 82615, 501, 629.43,
	                         1, 31163, 515, 363.46, 1, 71707, 534, 222.87, 1, 6514, 551,
	                         530.06, 1, 80260, 567, 176.39, 1, 35124, 582, 236.8, 1, 32309,
	                         594, 665.25, 1, 7851, 610, 407.09, 1, 97070, 626, 909.19, 1,
	                         42800, 639, 893.41, 1, 85834, 655, 665.15, 1, 36859, 671, 542.31,
	                         1, 59806, 689, 401.22, 1, 25087, 704, 501.51, 1, 23123, 717,
	                         361.01, 1, 60115, 736, 252.19, 1, 85033, 749, 264.85, 1, 8844,
	                         764, 588.93, 1, 52619, 785, 279.44, 1, 91330, 803, 304.58, 1,
	                         45922, 818, 1225.78, 1, 28971, 831, 558.38, 1, 35269, 850, 281.84,
	                         1, 50246, 865, 635.91, 1, 33249, 883, 428.24, 1, 96141, 896,
	                         370.65, 1, 67506, 911, 314.29, 1, 43451, 927, 230.42, 1, 84051,
	                         942, 321.9, 1, 99419, 956, 668.93, 1, 11929, 971, 850.14, 1,
	                         43520, 985, 416.75, 1, 26298, 1001, 339.16, 1, 51851, 1019,
	                         550.07, 1, 11808, 1037, 397.04, 1, 30309, 1052, 368.94, 1, 47500,
	                         1067, 583.33, 1, 98581, 1078, 254.58, 1, 25693, 1092, 360.27,
	                         1, 65066, 1106, 301.97, 1, 96557, 1119, 610.25, 1, 79238, 1136,
	                         642.84, 1, 30188, 1152, 766.24, 1, 32125, 1164, 262.23, 1, 92595,
	                         1179, 719.44, 1, 37062, 1192, 603.58, 1, 36993, 1207, 491.74,
	                         1, 44167, 1223, 288.42, 1, 73288, 1237, 891.39, 1, 38972, 1250,
	                         419.85, 1, 52022, 1266, 380.99, 1, 53993, 1282, 744.55, 1, 64300,
	                         1296, 549.93, 1, 41463, 1309, 382.67, 1, 23305, 1322, 605.12,
	                         1, 754, 1340, 395.82, 1, 83096, 1355, 534.88, 1, 8070, 1368,
	                         938.12, 1, 80163, 1381, 500.43, 1, 25836, 1396, 314.19, 1, 25900,
	                         1411, 335.35, 1, 30544, 1427, 376.15, 1, 43868, 1440, 576.5,
	                         1, 77533, 1456, 648.86, 1, 71354, 1472, 582.52, 1, 8019, 1489,
	                         769.5, 1, 99030, 1499, 539.68, 1, 58, 1510, 412.04, 1, 38880,
	                         1523, 655.94, 1, 72466, 1537, 514.16, 1, 25132, 1553, 355.86,
	                         1, 47936, 1570, 521.42, 1, 77108, 1584, 515.81, 1, 12604, 1600,
	                         973.75, 1, 42859, 1615, 598.07, 1, 66109, 1631, 517.77, 1, 50867,
	                         1650, 350.84, 1, 39990, 1668, 788, 1, 80283, 1683, 433.12, 1,
	                         53855, 1696, 632.64, 1, 44122, 1712, 401, 1, 78779, 1729, 608.02,
	                         1, 69825, 1741, 745.85, 1, 36462, 1756, 926.01, 1, 87258, 1774,
	                         157.68, 1, 67496, 1788, 491.41, 1, 56776, 1803, 1059.41, 1,
	                         64081, 1817, 580.3, 1, 21343, 1829, 490.59, 1, 79061, 1844,
	                         565.64, 1, 21654, 1861, 293.37, 1, 78011, 1877, 583.89, 1, 37757,
	                         1891, 430.09, 1, 11919, 1908, 322.96, 1, 89453, 1920, 271.91,
	                         1, 56641, 1932, 590.46, 1, 52241, 1945, 540.96, 1, 96558, 1961,
	                         265.52, 1, 97309, 1975, 654.59, 1, 42049, 1993, 745.19, 1, 25134,
	                         2006, 641.48, 1, 56402, 2019, 299.12, 1, 72664, 2032, 627.35,
	                         1, 45416, 2045, 381.05, 1, 27138, 2059, 641.17, 1, 6244, 2073,
	                         234.81, 1, 96492, 2087, 1109.32, 1, 77292, 2099, 349.68, 1,
	                         52300, 2111, 293.58, 1, 30454, 2122, 459.42, 1, 49847, 2138,
	                         360.9, 1, 43001, 2150, 607.21, 1, 16092, 2168, 595.35, 1, 51445,
	                         2179, 667.81, 1, 67116, 2194, 408.41, 1, 25564, 2209, 286.6,
	                         1, 86909, 2227, 399.8, 1, 61831, 2242, 508.63, 1, 80386, 2257,
	                         553.69, 1, 54661, 2273, 295.75, 1, 14825, 2287, 364.88, 1, 98955,
	                         2300, 349.99, 1, 62125, 2314, 306.91, 1, 14697, 2330, 220.59,
	                         1, 10137, 2345, 243.26, 1, 1692, 2354, 607.25, 1, 49157, 2369,
	                         911.21, 1, 48478, 2383, 254.6, 1, 85195, 2396, 847.81, 1, 70820,
	                         2410, 189.73, 1, 78163, 2423, 988.75, 1, 36797, 2436, 455.83,
	                         1, 95755, 2453, 295.58, 1, 17201, 2467, 822.73, 1, 27786, 2481,
	                         582.91, 1, 44020, 2497, 554.92, 1, 21808, 2515, 1011.39, 1,
	                         76399, 2528, 469.06, 1, 99279, 2544, 274.91, 1, 16188, 2558,
	                         649.78, 1, 83160, 2572, 473.86, 1, 19507, 2586, 619.56, 1, 99252,
	                         2598, 765.73, 1, 33705, 2610, 670.89, 1, 73965, 2623, 463.01,
	                         1, 23733, 2639, 264.88, 1, 39805, 2656, 617.63, 1, 87501, 2675,
	                         453.12, 1, 5783, 2695, 214.13, 1, 43318, 2709, 388.58, 1, 12075,
	                         2726, 461.4, 1, 35995, 2740, 630.45, 1, 94527, 2757, 686.4,
	                         1, 12486, 2767, 1064.23, 1, 76199, 2783, 236.35, 1, 80560, 2802,
	                         1150.9, 1, 19125, 2815, 634.58, 1, 71290, 2830, 476.61, 1, 45242,
	                         2847, 786.32, 1, 82794, 2863, 274.71, 1, 76649, 2880, 465.22,
	                         1, 57115, 2896, 499.94, 1, 13059, 2913, 583.61, 1, 92044, 2931,
	                         321.68, 1, 1922, 2948, 126.19, 1, 42205, 2967, 186.66, 1, 66411,
	                         2983, 169.56, 1, 29020, 2996, 250.42, 1, 3586, 3011, 748.21,
	                         1, 90807, 3026, 595.88, 1, 28508, 3039, 350.46, 1, 57063, 3052,
	                         277.19, 1, 38896, 3067, 445.42, 1, 16896, 3083, 491.14, 1, 63047,
	                         3098, 202.19, 1, 30109, 3116, 250.38, 1, 15893, 3131, 237.59,
	                         1, 49654, 3146, 485.5, 1, 76014, 3158, 392.18, 1, 18286, 3172,
	                         450.57, 1, 57211, 3188, 378.91, 1, 41420, 3203, 689.6, 1, 41334,
	                         3219, 749.71, 1, 32525, 3232, 432.37, 1, 12365, 3245, 235.54,
	                         1, 69686, 3260, 650.66, 1, 65433, 3277, 137.41, 1, 3037, 3294,
	                         372.64, 1, 5118, 3311, 338.74, 1, 28426, 3325, 448.93, 1, 24989,
	                         3343, 237.38, 1, 86092, 3357, 479.8, 1, 23295, 3372, 606.68,
	                         1, 99193, 3386, 317.27, 1, 92814, 3395, 333.22, 1, 89484, 3407,
	                         558.69, 1, 53133, 3423, 321, 1, 55573, 3437, 554.25, 1, 35945,
	                         3455, 274.93, 1, 59126, 3470, 417.57, 1, 22862, 3485, 392.82
	                    ))
})

test_that("<b>Table 3.</b> Selection Summary results match", {
	table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_selectionInformationTable"]][["data"]]
	expect_equal_tables(table,
		list("$ 5997", "8.2%", 234, "$ 115028"))
})
