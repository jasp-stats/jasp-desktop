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


test_that("Badge: <i>Annotated</i> plot matches", {
  plotName <- results[["results"]][["badgeSection"]][["collection"]][["badgeSection_annotationBadge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "badge-i-annotated-i-", dir="auditClassicalWorkflow")
})

test_that("Badge: <i>Approved</i> plot matches", {
  plotName <- results[["results"]][["badgeSection"]][["collection"]][["badgeSection_resultBadge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "badge-i-approved-i-", dir="auditClassicalWorkflow")
})

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

test_that("Decision Analysis Plot matches", {
  plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "decision-analysis-plot", dir="auditClassicalWorkflow")
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
                      list(500.038076923077, 470.485, 226.061346408586, 234, "bookValue"
                      ))
})

test_that("<b>Table 5.</b> Selected Observations results match", {
  table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_sampleTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 82884, 1, 242.61, 1, 76073, 15, 469.93, 1, 83557, 31, 507.34,
                           1, 28052, 49, 490.93, 1, 81126, 64, 623.94, 1, 28956, 77, 462.28,
                           1, 7224, 91, 354.76, 1, 4756, 110, 295.96, 1, 25996, 126, 434.05,
                           1, 93820, 140, 845.91, 1, 44198, 154, 765.11, 1, 36834, 170,
                           603.64, 1, 93367, 186, 585.26, 1, 88668, 203, 274.96, 1, 47821,
                           217, 52.75, 1, 15340, 232, 460.4, 1, 83880, 247, 697.3, 1, 14001,
                           260, 524.19, 1, 88999, 277, 488.17, 1, 64172, 295, 556.46, 1,
                           85801, 308, 214.69, 1, 56338, 324, 465.67, 1, 92584, 340, 450,
                           1, 27934, 355, 749.38, 1, 65018, 368, 309.25, 1, 48076, 383,
                           175.71, 1, 71803, 397, 624.7, 1, 49031, 413, 377.31, 1, 73223,
                           429, 683.8, 1, 12474, 444, 198.08, 1, 25823, 456, 323.42, 1,
                           37508, 470, 249.83, 1, 79093, 486, 635.85, 1, 82615, 501, 629.43,
                           1, 66583, 516, 879.54, 1, 89333, 535, 362.28, 1, 6514, 551,
                           530.06, 1, 64644, 568, 544.46, 1, 98319, 583, 579.4, 1, 30750,
                           595, 338.36, 1, 27452, 611, 322.19, 1, 97070, 626, 909.19, 1,
                           45342, 640, 408.83, 1, 21849, 657, 514.78, 1, 37946, 672, 412.46,
                           1, 59806, 689, 401.22, 1, 25087, 704, 501.51, 1, 64893, 719,
                           234.73, 1, 80847, 737, 717.5, 1, 74909, 750, 240.84, 1, 30584,
                           766, 128.66, 1, 74, 786, 394.25, 1, 54656, 804, 544.05, 1, 45922,
                           818, 1225.78, 1, 28971, 831, 558.38, 1, 20020, 852, 496.52,
                           1, 50246, 865, 635.91, 1, 47761, 884, 452.95, 1, 36965, 897,
                           247.12, 1, 84412, 912, 270.1, 1, 38549, 929, 336.55, 1, 30725,
                           943, 661.82, 1, 99419, 956, 668.93, 1, 11929, 971, 850.14, 1,
                           35188, 986, 639.54, 1, 31860, 1005, 437.3, 1, 49224, 1020, 275.39,
                           1, 77534, 1038, 589.52, 1, 6407, 1053, 501.45, 1, 37569, 1068,
                           633.59, 1, 5401, 1080, 553.58, 1, 56565, 1093, 397.74, 1, 56209,
                           1107, 441.31, 1, 72856, 1121, 375.16, 1, 38970, 1138, 685.84,
                           1, 4919, 1153, 584.12, 1, 32602, 1167, 159.95, 1, 99765, 1180,
                           536.56, 1, 48330, 1193, 933.99, 1, 32675, 1208, 413.54, 1, 76761,
                           1225, 394.11, 1, 69069, 1238, 440.66, 1, 1756, 1251, 513.56,
                           1, 60785, 1268, 272.74, 1, 6548, 1284, 742.4, 1, 11343, 1297,
                           471.04, 1, 71244, 1312, 352.15, 1, 85806, 1324, 200.7, 1, 71371,
                           1342, 688.71, 1, 68454, 1356, 518.82, 1, 8070, 1368, 938.12,
                           1, 57302, 1382, 524.91, 1, 79116, 1398, 467.72, 1, 157, 1413,
                           501.79, 1, 33462, 1428, 713.04, 1, 24329, 1442, 149.44, 1, 61930,
                           1457, 290.99, 1, 83225, 1474, 221.15, 1, 40983, 1490, 477.78,
                           1, 64874, 1501, 508.59, 1, 60042, 1513, 794.93, 1, 9212, 1525,
                           290.91, 1, 46214, 1539, 415.49, 1, 6242, 1556, 879.02, 1, 26525,
                           1572, 941.47, 1, 26926, 1585, 586.31, 1, 33389, 1602, 128.13,
                           1, 52545, 1618, 504.44, 1, 60678, 1633, 277.26, 1, 49850, 1653,
                           316.94, 1, 49325, 1669, 790.31, 1, 20736, 1685, 407.24, 1, 8437,
                           1699, 194.89, 1, 12080, 1715, 550.83, 1, 9392, 1731, 843.32,
                           1, 52285, 1743, 220.61, 1, 79044, 1759, 422.08, 1, 1604, 1778,
                           776.25, 1, 6154, 1791, 285.42, 1, 55225, 1806, 822.25, 1, 97153,
                           1819, 582.12, 1, 30701, 1832, 1026.71, 1, 82307, 1846, 80.71,
                           1, 35741, 1864, 213.39, 1, 47306, 1879, 457.51, 1, 2885, 1895,
                           285, 1, 40019, 1910, 741.36, 1, 6716, 1922, 909.33, 1, 51924,
                           1935, 294.94, 1, 53306, 1947, 374.17, 1, 75649, 1963, 423.29,
                           1, 67741, 1977, 353.28, 1, 2740, 1995, 714.23, 1, 78718, 2008,
                           339.03, 1, 52311, 2020, 1029.3, 1, 7826, 2034, 879.18, 1, 74167,
                           2047, 329.61, 1, 79899, 2061, 723.34, 1, 72265, 2076, 280.32,
                           1, 64999, 2088, 370.32, 1, 18608, 2102, 1090.25, 1, 37961, 2112,
                           881.96, 1, 76164, 2127, 341.01, 1, 19936, 2140, 776.33, 1, 43231,
                           2153, 308.53, 1, 11006, 2170, 337.57, 1, 32475, 2181, 567.86,
                           1, 48009, 2197, 931.1, 1, 49598, 2213, 420.93, 1, 74495, 2229,
                           327.98, 1, 17423, 2244, 598.4, 1, 94600, 2260, 717.11, 1, 80154,
                           2276, 282.91, 1, 34881, 2289, 1041.99, 1, 56953, 2303, 649.55,
                           1, 11857, 2318, 360.57, 1, 38461, 2334, 588.4, 1, 75616, 2347,
                           623.7, 1, 63287, 2358, 235.61, 1, 67153, 2371, 251.75, 1, 46490,
                           2388, 388.4, 1, 40967, 2398, 398.25, 1, 86415, 2413, 275.15,
                           1, 8332, 2426, 611.8, 1, 96959, 2441, 712.32, 1, 13732, 2457,
                           306.42, 1, 98756, 2472, 104.55, 1, 72681, 2486, 651.26, 1, 82438,
                           2501, 481.47, 1, 27217, 2518, 608.28, 1, 49963, 2531, 455.5,
                           1, 21144, 2548, 509.94, 1, 56996, 2562, 599.29, 1, 23322, 2575,
                           378.66, 1, 81316, 2588, 874.85, 1, 35760, 2601, 646.36, 1, 52259,
                           2612, 611.5, 1, 45843, 2626, 158.96, 1, 88715, 2643, 632.34,
                           1, 24581, 2659, 451.17, 1, 60935, 2681, 312.98, 1, 13291, 2697,
                           802.87, 1, 22730, 2714, 252.55, 1, 81023, 2730, 359.4, 1, 48377,
                           2744, 531.82, 1, 50504, 2760, 661.58, 1, 13329, 2771, 282.36,
                           1, 72425, 2788, 388.88, 1, 72585, 2804, 521.7, 1, 45248, 2819,
                           290.35, 1, 77671, 2834, 271.35, 1, 20047, 2853, 942.65, 1, 1744,
                           2866, 349.28, 1, 59402, 2883, 279.29, 1, 71459, 2901, 151.74,
                           1, 98382, 2916, 269.55, 1, 28145, 2935, 544.54, 1, 86627, 2952,
                           501.81, 1, 6808, 2971, 217.15, 1, 47090, 2986, 368.51, 1, 77867,
                           3001, 374.63, 1, 75272, 3014, 510.93, 1, 49597, 3027, 1315.05,
                           1, 40307, 3042, 410.75, 1, 27813, 3057, 189.71, 1, 32190, 3070,
                           596.3, 1, 87887, 3087, 666.13, 1, 91738, 3102, 897.3, 1, 31532,
                           3120, 469, 1, 79478, 3135, 544.8, 1, 67152, 3149, 453.15, 1,
                           19970, 3162, 325.25, 1, 53877, 3175, 451.91, 1, 31757, 3192,
                           504.24, 1, 74858, 3207, 884.51, 1, 90783, 3222, 543.95, 1, 48095,
                           3235, 852.46, 1, 13083, 3249, 576.79, 1, 94576, 3263, 278.46,
                           1, 59245, 3281, 538.92, 1, 5271, 3299, 477.09, 1, 73719, 3314,
                           469.12, 1, 47872, 3330, 444.67, 1, 19410, 3348, 891.58, 1, 91263,
                           3362, 659.14, 1, 37070, 3376, 279.72, 1, 45061, 3389, 123.5,
                           1, 51100, 3400, 506.76, 1, 92359, 3412, 330.32, 1, 35513, 3427,
                           392.05, 1, 513, 3442, 592.24, 1, 54201, 3460, 401.52, 1, 51209,
                           3476, 323.72, 1, 86008, 3489, 649.08))
})

test_that("<b>Table 3.</b> Selection Summary results match", {
  table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_selectionInformationTable"]][["data"]]
  expect_equal_tables(table,
                      list("$ 5997", "8.34%", 234, "$ 117009"))
})