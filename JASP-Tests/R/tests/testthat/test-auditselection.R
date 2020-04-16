context("[Audit] Selection")

options <- jasptools::analysisOptions("auditSelection")
options$.meta <- list(additionalVariables = list(containsColumn = TRUE), monetaryVariable = list(
  containsColumn = TRUE), rankingVariable = list(containsColumn = TRUE), 
  recordNumberVariable = list(containsColumn = TRUE))
options$displaySample <- TRUE
options$monetaryVariable <- "bookValue"
options$range <- FALSE
options$recordNumberVariable <- "ID"
options$sampleDescriptives <- TRUE
options$sampleSize <- 60
options$selectionMethod <- "randomSampling"
options$selectionType <- "musSampling"
set.seed(1)
results <- jasptools::run("auditSelection", "BuildIt_Monetary.csv", options)


test_that("<b>Table 2.</b> Selection Descriptive Statistics results match", {
  table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_sampleDescriptivesTable"]][["data"]]
  expect_equal_tables(table,
                      list(487.091333333333, 462.62, 198.339879965258, 60, "bookValue"))
})

test_that("<b>Table 3.</b> Selected Observations results match", {
  table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_sampleTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 90260, 2174, 625.98, 1, 68595, 2928, 548.21, 1, 98301, 1627,
                           429.07, 1, 29683, 700, 239.26, 1, 72906, 147, 677.62, 1, 86317,
                           3056, 246.22, 1, 14548, 3118, 204.63, 1, 45416, 2045, 381.05,
                           1, 91955, 1311, 398.96, 1, 12815, 716, 873.43, 1, 99419, 956,
                           668.93, 1, 4838, 530, 701.64, 1, 89447, 1218, 367.23, 1, 71607,
                           1772, 540.71, 1, 86098, 2979, 323.9, 1, 76399, 2528, 469.06,
                           1, 85858, 3354, 349.41, 1, 32500, 662, 118.29, 1, 49057, 2868,
                           543.32, 1, 29132, 969, 320.19, 1, 85801, 308, 214.69, 1, 78043,
                           1858, 664.6, 1, 29378, 3061, 386.56, 1, 86438, 1047, 758.85,
                           1, 39905, 1574, 622.61, 1, 48046, 1248, 539.13, 1, 50725, 1108,
                           1073.76, 1, 72525, 326, 541.93, 1, 63557, 1115, 268.12, 1, 59284,
                           2616, 567.93, 1, 30644, 1366, 476.96, 1, 75319, 3179, 414.35,
                           1, 76073, 15, 469.93, 1, 41420, 3203, 689.6, 1, 84318, 2341,
                           292.3, 1, 61593, 97, 376.3, 1, 58263, 3195, 309.46, 1, 45828,
                           1418, 788.28, 1, 77635, 3304, 346.41, 1, 55567, 1842, 518.98,
                           1, 95755, 2453, 295.58, 1, 41150, 3435, 388.34, 1, 769, 1270,
                           317.21, 1, 45746, 136, 440.72, 1, 14628, 2584, 452.17, 1, 93255,
                           225, 312.5, 1, 71832, 3152, 1001.82, 1, 65768, 463, 480.89,
                           1, 72951, 2560, 341.16, 1, 31163, 515, 363.46, 1, 48921, 1622,
                           480.79, 1, 58329, 1693, 272.29, 1, 29396, 3365, 505.6, 1, 78162,
                           3223, 639.24, 1, 85195, 2396, 847.81, 1, 63825, 2473, 795.72,
                           1, 82902, 1532, 583.95, 1, 71580, 1261, 456.18, 1, 8094, 1409,
                           378.87, 1, 76607, 438, 523.32))
})

test_that("<b>Table 1.</b> Selection Summary results match", {
  table <- results[["results"]][["selectionContainer"]][["collection"]][["selectionContainer_selectionInformationTable"]][["data"]]
  expect_equal_tables(table,
                      list("2.08%", 60, "$ 29226"))
})