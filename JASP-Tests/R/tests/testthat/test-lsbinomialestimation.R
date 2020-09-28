context("Learn Bayes - Binomial Estimation")

skip_on_travis()

### output for all default settings (spike + beta)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- TRUE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsIterative <- TRUE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mean"
  options$plotsIterativeIndividualCI <- FALSE
  options$plotsIterativeIndividualType <- "central"
  options$plotsIterativeInterval <- TRUE
  options$plotsIterativeIntervalLower <- 0.25
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.75
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- FALSE
  options$plotsPosteriorIndividualEstimate <- FALSE
  options$plotsPosteriorIndividualEstimateType <- "mean"
  options$plotsPosteriorIndividualType <- "central"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "stacked"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- TRUE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorIndividualCI <- FALSE
  options$plotsPriorIndividualEstimate <- FALSE
  options$plotsPriorIndividualEstimateType <- "mean"
  options$plotsPriorIndividualType <- "central"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "overlying"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mean"
  options$predictionN <- 1
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "overlying"
  options$predictionTable <- TRUE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(name = "Models Spike", parAlpha = "1", parBeta = "1", 
                              parPoint = "0.5", type = "spike", value = ""), list(name = "Models Beta", 
                                                                                  parAlpha = "1", parBeta = "1", parPoint = "0.5", type = "beta", 
                                                                                  value = "2"))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-default-1", dir="LSbinomialestimation")
  })
  
  test_that("Models Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_Models Spike"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-spike-default-2", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Updating Plot matches", {
    plotName <- results[["results"]][["containerIterative"]][["collection"]][["containerIterative_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-2-default-3", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Interval Updating Plot matches", {
    plotName <- results[["results"]][["containerIterativeInterval"]][["collection"]][["containerIterativeInterval_plotsIterativeInterval"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequantial-interval-plot-3-default-4", dir="LSbinomialestimation")
  })
  
  test_that("Updating Table results match", {
    table <- results[["results"]][["containerIterativeUpdating"]][["collection"]][["containerIterativeUpdating_estimatesSequentialTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("beta (1, 1)", "spike at 0.5", 0, "beta (1, 2)", "spike at 0.5",
                                        1, "beta (2, 2)", "spike at 0.5", 2, "beta (3, 2)", "spike at 0.5",
                                        3, "beta (3, 3)", "spike at 0.5", 4, "beta (3, 4)", "spike at 0.5",
                                        5, "beta (3, 5)", "spike at 0.5", 6, "beta (4, 5)", "spike at 0.5",
                                        7, "beta (5, 5)", "spike at 0.5", 8, "beta (6, 5)", "spike at 0.5",
                                        9, "beta (7, 5)", "spike at 0.5", 10, "beta (8, 5)", "spike at 0.5",
                                        11, "beta (8, 6)", "spike at 0.5", 12, "beta (9, 6)", "spike at 0.5",
                                        13, "beta (10, 6)", "spike at 0.5", 14, "beta (11, 6)", "spike at 0.5",
                                        15, "beta (12, 6)", "spike at 0.5", 16, "beta (12, 7)", "spike at 0.5",
                                        17, "beta (13, 7)", "spike at 0.5", 18, "beta (14, 7)", "spike at 0.5",
                                        19, "beta (15, 7)", "spike at 0.5", 20, "beta (16, 7)", "spike at 0.5",
                                        21, "beta (16, 8)", "spike at 0.5", 22, "beta (17, 8)", "spike at 0.5",
                                        23, "beta (18, 8)", "spike at 0.5", 24, "beta (19, 8)", "spike at 0.5",
                                        25, "beta (20, 8)", "spike at 0.5", 26, "beta (21, 8)", "spike at 0.5",
                                        27, "beta (21, 9)", "spike at 0.5", 28, "beta (21, 10)", "spike at 0.5",
                                        29, "beta (22, 10)", "spike at 0.5", 30, "beta (23, 10)", "spike at 0.5",
                                        31, "beta (24, 10)", "spike at 0.5", 32, "beta (24, 11)", "spike at 0.5",
                                        33, "beta (25, 11)", "spike at 0.5", 34, "beta (26, 11)", "spike at 0.5",
                                        35, "beta (26, 12)", "spike at 0.5", 36, "beta (26, 13)", "spike at 0.5",
                                        37, "beta (26, 14)", "spike at 0.5", 38, "beta (26, 15)", "spike at 0.5",
                                        39, "beta (27, 15)", "spike at 0.5", 40, "beta (28, 15)", "spike at 0.5",
                                        41, "beta (28, 16)", "spike at 0.5", 42, "beta (29, 16)", "spike at 0.5",
                                        43, "beta (30, 16)", "spike at 0.5", 44, "beta (30, 17)", "spike at 0.5",
                                        45, "beta (31, 17)", "spike at 0.5", 46, "beta (31, 18)", "spike at 0.5",
                                        47, "beta (31, 19)", "spike at 0.5", 48, "beta (31, 20)", "spike at 0.5",
                                        49, "beta (32, 20)", "spike at 0.5", 50, "beta (32, 21)", "spike at 0.5",
                                        51, "beta (32, 22)", "spike at 0.5", 52, "beta (33, 22)", "spike at 0.5",
                                        53, "beta (33, 23)", "spike at 0.5", 54, "beta (34, 23)", "spike at 0.5",
                                        55, "beta (34, 24)", "spike at 0.5", 56, "beta (35, 24)", "spike at 0.5",
                                        57, "beta (36, 24)", "spike at 0.5", 58, "beta (37, 24)", "spike at 0.5",
                                        59, "beta (38, 24)", "spike at 0.5", 60, "beta (39, 24)", "spike at 0.5",
                                        61, "beta (40, 24)", "spike at 0.5", 62, "beta (40, 25)", "spike at 0.5",
                                        63, "beta (40, 26)", "spike at 0.5", 64, "beta (40, 27)", "spike at 0.5",
                                        65, "beta (40, 28)", "spike at 0.5", 66, "beta (40, 29)", "spike at 0.5",
                                        67, "beta (41, 29)", "spike at 0.5", 68, "beta (42, 29)", "spike at 0.5",
                                        69, "beta (43, 29)", "spike at 0.5", 70, "beta (43, 30)", "spike at 0.5",
                                        71, "beta (44, 30)", "spike at 0.5", 72, "beta (44, 31)", "spike at 0.5",
                                        73, "beta (45, 31)", "spike at 0.5", 74, "beta (46, 31)", "spike at 0.5",
                                        75, "beta (47, 31)", "spike at 0.5", 76, "beta (47, 32)", "spike at 0.5",
                                        77, "beta (48, 32)", "spike at 0.5", 78, "beta (49, 32)", "spike at 0.5",
                                        79, "beta (50, 32)", "spike at 0.5", 80, "beta (51, 32)", "spike at 0.5",
                                        81, "beta (52, 32)", "spike at 0.5", 82, "beta (53, 32)", "spike at 0.5",
                                        83, "beta (53, 33)", "spike at 0.5", 84, "beta (53, 34)", "spike at 0.5",
                                        85, "beta (53, 35)", "spike at 0.5", 86, "beta (54, 35)", "spike at 0.5",
                                        87, "beta (54, 36)", "spike at 0.5", 88, "beta (54, 37)", "spike at 0.5",
                                        89, "beta (55, 37)", "spike at 0.5", 90, "beta (56, 37)", "spike at 0.5",
                                        91, "beta (56, 38)", "spike at 0.5", 92, "beta (56, 39)", "spike at 0.5",
                                        93, "beta (56, 40)", "spike at 0.5", 94, "beta (56, 41)", "spike at 0.5",
                                        95, "beta (57, 41)", "spike at 0.5", 96, "beta (57, 42)", "spike at 0.5",
                                        97, "beta (58, 42)", "spike at 0.5", 98, "beta (58, 43)", "spike at 0.5",
                                        99, "beta (59, 43)", "spike at 0.5", 100))
  })
  
  test_that("Posterior Plots matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-5-default-5", dir="LSbinomialestimation")
  })
  
  test_that("Prior Plots matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-6-default-6", dir="LSbinomialestimation")
  })
  
  test_that("Predictions Plots matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predictions-plot-7-default-7", dir="LSbinomialestimation")
  })
  
  test_that("Predictions Table results match", {
    table <- results[["results"]][["containerPredictions"]][["collection"]][["containerPredictions_predictionsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Spike", "spike at 0.5", 0.5, "binomial (1, 0.5)", 0.5,
                                        "Models Beta", "beta (59, 43)", 0.57843137254902, "beta-binomial (1, 59, 59)",
                                        0.57843137254902))
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Spike", "spike at 0.5", 0.5, "spike at 0.5", 0.5, "Models Beta",
                                        "beta (59, 43)", 0.57843137254902, "beta (1, 1)", 0.5))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### more options vol. 1 (spike + beta)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- TRUE
  options$plotsIterative <- TRUE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "median"
  options$plotsIterativeIndividualCI <- TRUE
  options$plotsIterativeIndividualType <- "HPD"
  options$plotsIterativeInterval <- TRUE
  options$plotsIterativeIntervalLower <- 0.25
  options$plotsIterativeIntervalType <- "stacked"
  options$plotsIterativeIntervalUpdatingTable <- TRUE
  options$plotsIterativeIntervalUpper <- 0.75
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- TRUE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- TRUE
  options$plotsPosteriorIndividualEstimate <- FALSE
  options$plotsPosteriorIndividualEstimateType <- "mean"
  options$plotsPosteriorIndividualType <- "central"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "individual"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- TRUE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorIndividualCI <- FALSE
  options$plotsPriorIndividualEstimate <- TRUE
  options$plotsPriorIndividualEstimateType <- "mean"
  options$plotsPriorIndividualType <- "central"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "individual"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mode"
  options$predictionN <- 10
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "stacked"
  options$predictionTable <- TRUE
  options$predictionTableEstimate <- "mode"
  options$priors <- list(list(name = "Models Beta", parAlpha = "2", parBeta = "2", 
                              parPoint = "0.5", type = "beta", value = ""), list(name = "Models Point", 
                                                                                 parAlpha = "1", parBeta = "1", parPoint = "0.3", type = "spike", 
                                                                                 value = "2"))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol1-1", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol1-2", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Updating plot matches", {
    plotName <- results[["results"]][["containerIterative"]][["collection"]][["containerIterative_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-2-vol1-3", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Updating Table results match", {
    table <- results[["results"]][["containerIterative"]][["collection"]][["containerIterative_tableIterative"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("[0.094, 0.906]", 0.5, "[0.300, 0.300]", 0.3, 0, "[0.044, 0.772]",
                                        0.38572756813239, "[0.300, 0.300]", 0.3, 1, "[0.147, 0.853]",
                                        0.5, "[0.300, 0.300]", 0.3, 2, "[0.239, 0.895]", 0.578592809309287,
                                        "[0.300, 0.300]", 0.3, 3, "[0.184, 0.816]", 0.5, "[0.300, 0.300]",
                                        0.3, 4, "[0.149, 0.746]", 0.440155204634766, "[0.300, 0.300]",
                                        0.3, 5, "[0.124, 0.685]", 0.39308483281063, "[0.300, 0.300]",
                                        0.3, 6, "[0.182, 0.732]", 0.451694156223663, "[0.300, 0.300]",
                                        0.3, 7, "[0.234, 0.766]", 0.5, "[0.300, 0.300]", 0.3, 8, "[0.281, 0.793]",
                                        0.54049281421956, "[0.300, 0.300]", 0.3, 9, "[0.323, 0.814]",
                                        0.574923338650419, "[0.300, 0.300]", 0.3, 10, "[0.361, 0.832]",
                                        0.604556744392119, "[0.300, 0.300]", 0.3, 11, "[0.328, 0.792]",
                                        0.565167213922496, "[0.300, 0.300]", 0.3, 12, "[0.362, 0.809]",
                                        0.591773498675099, "[0.300, 0.300]", 0.3, 13, "[0.392, 0.823]",
                                        0.61531276594024, "[0.300, 0.300]", 0.3, 14, "[0.420, 0.836]",
                                        0.636286181644356, "[0.300, 0.300]", 0.3, 15, "[0.446, 0.846]",
                                        0.65509143085512, "[0.300, 0.300]", 0.3, 16, "[0.416, 0.816]",
                                        0.622894794821295, "[0.300, 0.300]", 0.3, 17, "[0.439, 0.827]",
                                        0.640565733353753, "[0.300, 0.300]", 0.3, 18, "[0.461, 0.836]",
                                        0.656655004344889, "[0.300, 0.300]", 0.3, 19, "[0.481, 0.845]",
                                        0.671365816585758, "[0.300, 0.300]", 0.3, 20, "[0.500, 0.852]",
                                        0.684868008478614, "[0.300, 0.300]", 0.3, 21, "[0.474, 0.828]",
                                        0.65784657834052, "[0.300, 0.300]", 0.3, 22, "[0.491, 0.835]",
                                        0.670836999809617, "[0.300, 0.300]", 0.3, 23, "[0.508, 0.843]",
                                        0.682877205036544, "[0.300, 0.300]", 0.3, 24, "[0.523, 0.849]",
                                        0.69406775991081, "[0.300, 0.300]", 0.3, 25, "[0.538, 0.855]",
                                        0.704495525209941, "[0.300, 0.300]", 0.3, 26, "[0.551, 0.861]",
                                        0.714235913830798, "[0.300, 0.300]", 0.3, 27, "[0.528, 0.840]",
                                        0.691449298248634, "[0.300, 0.300]", 0.3, 28, "[0.507, 0.820]",
                                        0.670070729920772, "[0.300, 0.300]", 0.3, 29, "[0.520, 0.827]",
                                        0.679967316384646, "[0.300, 0.300]", 0.3, 30, "[0.533, 0.833]",
                                        0.689287516232948, "[0.300, 0.300]", 0.3, 31, "[0.545, 0.838]",
                                        0.698080254263729, "[0.300, 0.300]", 0.3, 32, "[0.526, 0.820]",
                                        0.678871788869583, "[0.300, 0.300]", 0.3, 33, "[0.537, 0.826]",
                                        0.687472547900221, "[0.300, 0.300]", 0.3, 34, "[0.548, 0.831]",
                                        0.695624641542151, "[0.300, 0.300]", 0.3, 35, "[0.531, 0.815]",
                                        0.677942968082248, "[0.300, 0.300]", 0.3, 36, "[0.514, 0.799]",
                                        0.661137560458077, "[0.300, 0.300]", 0.3, 37, "[0.499, 0.783]",
                                        0.64514491836568, "[0.300, 0.300]", 0.3, 38, "[0.484, 0.768]",
                                        0.62990751702158, "[0.300, 0.300]", 0.3, 39, "[0.495, 0.774]",
                                        0.638447436303181, "[0.300, 0.300]", 0.3, 40, "[0.505, 0.780]",
                                        0.646602144702128, "[0.300, 0.300]", 0.3, 41, "[0.492, 0.766]",
                                        0.632340703720261, "[0.300, 0.300]", 0.3, 42, "[0.502, 0.772]",
                                        0.640275248179192, "[0.300, 0.300]", 0.3, 43, "[0.511, 0.777]",
                                        0.647874567128595, "[0.300, 0.300]", 0.3, 44, "[0.498, 0.764]",
                                        0.634471796690132, "[0.300, 0.300]", 0.3, 45, "[0.508, 0.769]",
                                        0.641880701656875, "[0.300, 0.300]", 0.3, 46, "[0.495, 0.757]",
                                        0.629129419469412, "[0.300, 0.300]", 0.3, 47, "[0.484, 0.744]",
                                        0.616874813086074, "[0.300, 0.300]", 0.3, 48, "[0.473, 0.732]",
                                        0.605088428278435, "[0.300, 0.300]", 0.3, 49, "[0.482, 0.738]",
                                        0.612492624209658, "[0.300, 0.300]", 0.3, 50, "[0.471, 0.727]",
                                        0.601220656886222, "[0.300, 0.300]", 0.3, 51, "[0.461, 0.716]",
                                        0.590356033725692, "[0.300, 0.300]", 0.3, 52, "[0.470, 0.721]",
                                        0.597627465725956, "[0.300, 0.300]", 0.3, 53, "[0.460, 0.711]",
                                        0.587204447349039, "[0.300, 0.300]", 0.3, 54, "[0.469, 0.716]",
                                        0.594280619749873, "[0.300, 0.300]", 0.3, 55, "[0.459, 0.706]",
                                        0.584265290819207, "[0.300, 0.300]", 0.3, 56, "[0.467, 0.711]",
                                        0.591155626527085, "[0.300, 0.300]", 0.3, 57, "[0.475, 0.717]",
                                        0.597821293739793, "[0.300, 0.300]", 0.3, 58, "[0.483, 0.722]",
                                        0.604273104107719, "[0.300, 0.300]", 0.3, 59, "[0.490, 0.727]",
                                        0.610521186566894, "[0.300, 0.300]", 0.3, 60, "[0.498, 0.731]",
                                        0.616575040384333, "[0.300, 0.300]", 0.3, 61, "[0.505, 0.736]",
                                        0.622443583334046, "[0.300, 0.300]", 0.3, 62, "[0.496, 0.726]",
                                        0.613060533443989, "[0.300, 0.300]", 0.3, 63, "[0.487, 0.717]",
                                        0.603956150474431, "[0.300, 0.300]", 0.3, 64, "[0.479, 0.708]",
                                        0.595118204160828, "[0.300, 0.300]", 0.3, 65, "[0.471, 0.699]",
                                        0.586535169305878, "[0.300, 0.300]", 0.3, 66, "[0.463, 0.691]",
                                        0.578196175742505, "[0.300, 0.300]", 0.3, 67, "[0.470, 0.695]",
                                        0.584109131212598, "[0.300, 0.300]", 0.3, 68, "[0.477, 0.700]",
                                        0.589858603518289, "[0.300, 0.300]", 0.3, 69, "[0.483, 0.705]",
                                        0.59545128002427, "[0.300, 0.300]", 0.3, 70, "[0.476, 0.696]",
                                        0.58744105050883, "[0.300, 0.300]", 0.3, 71, "[0.482, 0.701]",
                                        0.592917347189705, "[0.300, 0.300]", 0.3, 72, "[0.475, 0.693]",
                                        0.585150168403083, "[0.300, 0.300]", 0.3, 73, "[0.481, 0.697]",
                                        0.590514466487781, "[0.300, 0.300]", 0.3, 74, "[0.487, 0.701]",
                                        0.595741809870873, "[0.300, 0.300]", 0.3, 75, "[0.493, 0.706]",
                                        0.600837377135875, "[0.300, 0.300]", 0.3, 76, "[0.486, 0.698]",
                                        0.593358338512864, "[0.300, 0.300]", 0.3, 77, "[0.492, 0.702]",
                                        0.598357914811768, "[0.300, 0.300]", 0.3, 78, "[0.497, 0.706]",
                                        0.603236048712387, "[0.300, 0.300]", 0.3, 79, "[0.503, 0.710]",
                                        0.607997111862196, "[0.300, 0.300]", 0.3, 80, "[0.508, 0.714]",
                                        0.612645268578687, "[0.300, 0.300]", 0.3, 81, "[0.514, 0.717]",
                                        0.617184487995694, "[0.300, 0.300]", 0.3, 82, "[0.519, 0.721]",
                                        0.621618555365786, "[0.300, 0.300]", 0.3, 83, "[0.512, 0.714]",
                                        0.61450101368092, "[0.300, 0.300]", 0.3, 84, "[0.505, 0.707]",
                                        0.607544611226366, "[0.300, 0.300]", 0.3, 85, "[0.499, 0.700]",
                                        0.600743937631615, "[0.300, 0.300]", 0.3, 86, "[0.504, 0.703]",
                                        0.605163663866717, "[0.300, 0.300]", 0.3, 87, "[0.498, 0.697]",
                                        0.598537968373689, "[0.300, 0.300]", 0.3, 88, "[0.492, 0.690]",
                                        0.592055780320433, "[0.300, 0.300]", 0.3, 89, "[0.497, 0.694]",
                                        0.59642653398455, "[0.300, 0.300]", 0.3, 90, "[0.502, 0.697]",
                                        0.600704624291, "[0.300, 0.300]", 0.3, 91, "[0.496, 0.691]",
                                        0.594403685290299, "[0.300, 0.300]", 0.3, 92, "[0.490, 0.684]",
                                        0.588233553913292, "[0.300, 0.300]", 0.3, 93, "[0.484, 0.678]",
                                        0.582190198964589, "[0.300, 0.300]", 0.3, 94, "[0.479, 0.672]",
                                        0.576269753178822, "[0.300, 0.300]", 0.3, 95, "[0.484, 0.676]",
                                        0.580535426024885, "[0.300, 0.300]", 0.3, 96, "[0.478, 0.670]",
                                        0.574749482467959, "[0.300, 0.300]", 0.3, 97, "[0.483, 0.673]",
                                        0.578945969319675, "[0.300, 0.300]", 0.3, 98, "[0.478, 0.667]",
                                        0.573288632711558, "[0.300, 0.300]", 0.3, 99, "[0.482, 0.671]",
                                        0.577418036585966, "[0.300, 0.300]", 0.3, 100))
  })
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerIterativeInterval"]][["collection"]][["containerIterativeInterval_plotsIterativeInterval"]][["collection"]][["containerIterativeInterval_plotsIterativeInterval_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol1-3", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerIterativeInterval"]][["collection"]][["containerIterativeInterval_plotsIterativeInterval"]][["collection"]][["containerIterativeInterval_plotsIterativeInterval_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol1-4", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Interval Updating Table results match", {
    table <- results[["results"]][["containerIterativeInterval"]][["collection"]][["containerIterativeInterval_tableIterativeInterval"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(0.6875, 1, 0, 0.6875, 1, 1, 0.79296875, 1, 2, 0.79296875, 1, 3,
                                        0.85888671875, 1, 4, 0.85888671875, 1, 5, 0.82427978515625,
                                        1, 6, 0.902145385742188, 1, 7, 0.931344985961914, 1, 8, 0.931344985961914,
                                        1, 9, 0.914138078689575, 1, 10, 0.886176854372025, 1, 11, 0.939186675474048,
                                        1, 12, 0.9187982827425, 1, 13, 0.892293372191489, 1, 14, 0.860784037970006,
                                        1, 15, 0.825040636962513, 1, 16, 0.898004439019132, 1, 17, 0.870018534384144,
                                        1, 18, 0.83844765849426, 1, 19, 0.80368789285103, 1, 20, 0.766201091614107,
                                        1, 21, 0.850554966074297, 1, 22, 0.819545663580518, 1, 23, 0.785948838581055,
                                        1, 24, 0.750140483291595, 1, 25, 0.712541285738125, 1, 26, 0.673599113147189,
                                        1, 27, 0.770954666752371, 1, 28, 0.846405126147307, 1, 29, 0.81896869871178,
                                        1, 30, 0.789444605905065, 1, 31, 0.758075217946548, 1, 32, 0.832934015967973,
                                        1, 33, 0.805984859949065, 1, 34, 0.777221806401602, 1, 35, 0.845534068098425,
                                        1, 36, 0.896768257229844, 1, 37, 0.933364091306859, 1, 38, 0.958371213222553,
                                        1, 39, 0.948646257965095, 1, 40, 0.93744515127369, 1, 41, 0.960547462805447,
                                        1, 42, 0.951585370436516, 1, 43, 0.94127895128334, 1, 44, 0.962649623463581,
                                        1, 45, 0.954377108659419, 1, 46, 0.971266833447028, 1, 47, 0.982378489845163,
                                        1, 48, 0.989462161912837, 1, 49, 0.986584430965322, 1, 50, 0.992031585044148,
                                        1, 51, 0.995374144285945, 1, 52, 0.993981426992656, 1, 53, 0.996524678589675,
                                        1, 54, 0.995458768998839, 1, 55, 0.997390751944553, 1, 56, 0.996576568324053,
                                        1, 57, 0.995558827308007, 1, 58, 0.994300399559007, 1, 59, 0.992760479460712,
                                        1, 60, 0.9908948062538, 1, 61, 0.988655998093453, 1, 62, 0.993021674208965,
                                        1, 63, 0.995792199307395, 1, 64, 0.997510950749263, 1, 65, 0.998554477974045,
                                        1, 66, 0.999175195975642, 1, 67, 0.998910256362271, 1, 68, 0.998574349516895,
                                        1, 69, 0.998152512484225, 1, 70, 0.998922365390762, 1, 71, 0.998598677372228,
                                        1, 72, 0.999186014748229, 1, 73, 0.998938027959835, 1, 74, 0.998626696589875,
                                        1, 75, 0.998239188341338, 1, 76, 0.998956684116105, 1, 77, 0.998657727557176,
                                        1, 78, 0.998287082418425, 1, 79, 0.997831188890701, 1, 80, 0.997274730611342,
                                        1, 81, 0.996600560002496, 1, 82, 0.995789647241391, 1, 83, 0.997374613092944,
                                        1, 84, 0.998388525071318, 1, 85, 0.999025841171644, 1, 86, 0.998763243334461,
                                        1, 87, 0.999255614280026, 1, 88, 0.999558355874413, 1, 89, 0.999431754844686,
                                        1, 90, 0.999274068739883, 1, 91, 0.99956661796114, 1, 92, 0.999744772935317,
                                        1, 93, 0.999851665919272, 1, 94, 0.999914889207583, 1, 95, 0.999887714286677,
                                        1, 96, 0.999935755666939, 1, 97, 0.999915048176409, 1, 98, 0.999951527071306,
                                        1, 99, 0.999935760770006, 1, 100))
  })
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol1-5", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol1-6", dir="LSbinomialestimation")
  })
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol1-7", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol1-8", dir="LSbinomialestimation")
  })
  
  test_that("Predictions plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predictions-plot-11-vol1-9", dir="LSbinomialestimation")
  })
  
  test_that("Predictions table results match", {
    table <- results[["results"]][["containerPredictions"]][["collection"]][["containerPredictions_predictionsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (60, 44)", 0.57843137254902, "beta-binomial (10, 60, 60)",
                                        6, "Models Point", "spike at 0.3", 0.3, "binomial (10, 0.3)",
                                        3))
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (60, 44)", 0.57843137254902, "beta (2, 2)",
                                        0.5, "Models Point", "spike at 0.3", 0.3, "spike at 0.3", 0.3
                                   ))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### more options vol. 2 (spike + beta)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- TRUE
  options$plotsIterative <- TRUE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "median"
  options$plotsIterativeIndividualCI <- TRUE
  options$plotsIterativeIndividualType <- "HPD"
  options$plotsIterativeInterval <- TRUE
  options$plotsIterativeIntervalLower <- 0
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.25
  options$plotsIterativeType <- "stacked"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- TRUE
  options$plotsPosteriorIndividualEstimate <- TRUE
  options$plotsPosteriorIndividualEstimateType <- "mode"
  options$plotsPosteriorIndividualType <- "support"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "individual"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- TRUE
  options$plotsPriorCoverage <- 0.8
  options$plotsPriorIndividualCI <- TRUE
  options$plotsPriorIndividualEstimate <- TRUE
  options$plotsPriorIndividualEstimateType <- "median"
  options$plotsPriorIndividualType <- "HPD"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "individual"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mode"
  options$predictionN <- 10
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "individual"
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mode"
  options$priors <- list(list(name = "Models Beta", parAlpha = "2", parBeta = "2", 
                              parPoint = "0.5", type = "beta", value = ""), list(name = "Models Point", 
                                                                                 parAlpha = "1", parBeta = "1", parPoint = "0.3", type = "spike", 
                                                                                 value = "2"))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerIterative"]][["collection"]][["containerIterative_plotsIterative"]][["collection"]][["containerIterative_plotsIterative_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol2-1", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerIterative"]][["collection"]][["containerIterative_plotsIterative"]][["collection"]][["containerIterative_plotsIterative_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol2-2", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Interval plot matches", {
    plotName <- results[["results"]][["containerIterativeInterval"]][["collection"]][["containerIterativeInterval_plotsIterativeInterval"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-interval-plot-2-vol2-3", dir="LSbinomialestimation")
  })
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol2-4", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol2-5", dir="LSbinomialestimation")
  })
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol2-6", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol2-7", dir="LSbinomialestimation")
  })
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["collection"]][["containerPredictionPlots_plotsPredictions_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol2-8", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["collection"]][["containerPredictionPlots_plotsPredictions_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol2-9", dir="LSbinomialestimation")
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (60, 44)", 0.57843137254902, "beta (2, 2)",
                                        0.5, "Models Point", "spike at 0.3", 0.3, "spike at 0.3", 0.3
                                   ))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### more options vol. 3 (spike + beta)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- TRUE
  options$plotsIterative <- TRUE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mode"
  options$plotsIterativeIndividualCI <- TRUE
  options$plotsIterativeIndividualType <- "support"
  options$plotsIterativeInterval <- FALSE
  options$plotsIterativeIntervalLower <- 0
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.25
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- TRUE
  options$plotsPosteriorIndividualEstimate <- TRUE
  options$plotsPosteriorIndividualEstimateType <- "mode"
  options$plotsPosteriorIndividualType <- "support"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "individual"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- TRUE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- FALSE
  options$plotsPriorCoverage <- 0.8
  options$plotsPriorIndividualCI <- TRUE
  options$plotsPriorIndividualEstimate <- TRUE
  options$plotsPriorIndividualEstimateType <- "median"
  options$plotsPriorIndividualType <- "HPD"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "individual"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mode"
  options$predictionN <- 10
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "individual"
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mode"
  options$priors <- list(list(name = "Models Beta", parAlpha = "2", parBeta = "2", 
                              parPoint = "0.5", type = "beta", value = ""), list(name = "Models Point", 
                                                                                 parAlpha = "1", parBeta = "1", parPoint = "0.3", type = "spike", 
                                                                                 value = "2"))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Sequantial Updating plot matches", {
    plotName <- results[["results"]][["containerIterative"]][["collection"]][["containerIterative_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "titleless-plot-0-vol3-1", dir="LSbinomialestimation")
  })
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["collection"]][["containerPredictionPlots_plotsPredictions_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol3-2", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["collection"]][["containerPredictionPlots_plotsPredictions_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol3-3", dir="LSbinomialestimation")
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (60, 44)", 0.57843137254902, "beta (2, 2)",
                                        0.5, "Models Point", "spike at 0.3", 0.3, "spike at 0.3", 0.3
                                   ))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### more options vol. 4 (spike + beta)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- TRUE
  options$plotsIterative <- FALSE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mode"
  options$plotsIterativeIndividualCI <- TRUE
  options$plotsIterativeIndividualType <- "support"
  options$plotsIterativeInterval <- FALSE
  options$plotsIterativeIntervalLower <- 0
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.25
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- TRUE
  options$plotsPosteriorIndividualEstimate <- TRUE
  options$plotsPosteriorIndividualEstimateType <- "mode"
  options$plotsPosteriorIndividualType <- "support"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "individual"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- TRUE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- TRUE
  options$plotsPredictionEstimateType <- "median"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "HPD"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- FALSE
  options$plotsPriorCoverage <- 0.8
  options$plotsPriorIndividualCI <- TRUE
  options$plotsPriorIndividualEstimate <- TRUE
  options$plotsPriorIndividualEstimateType <- "median"
  options$plotsPriorIndividualType <- "HPD"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "individual"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mode"
  options$predictionN <- 10
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "individual"
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mode"
  options$priors <- list(list(name = "Models Beta", parAlpha = "2", parBeta = "2", 
                              parPoint = "0.5", type = "beta", value = ""), list(name = "Models Point", 
                                                                                 parAlpha = "1", parBeta = "1", parPoint = "0.3", type = "spike", 
                                                                                 value = "2"))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["collection"]][["containerPredictionPlots_plotsPredictions_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol4-1", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["collection"]][["containerPredictionPlots_plotsPredictions_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol4-2", dir="LSbinomialestimation")
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (60, 44)", 0.57843137254902, "beta (2, 2)",
                                        0.5, "Models Point", "spike at 0.3", 0.3, "spike at 0.3", 0.3
                                   ))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### more options vol. 5 (spike + beta)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- TRUE
  options$plotsIterative <- FALSE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mode"
  options$plotsIterativeIndividualCI <- TRUE
  options$plotsIterativeIndividualType <- "support"
  options$plotsIterativeInterval <- FALSE
  options$plotsIterativeIntervalLower <- 0
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.25
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- TRUE
  options$plotsPosteriorIndividualEstimate <- TRUE
  options$plotsPosteriorIndividualEstimateType <- "mode"
  options$plotsPosteriorIndividualType <- "support"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "individual"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- TRUE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- TRUE
  options$plotsPredictionEstimateType <- "median"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "HPD"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- FALSE
  options$plotsPriorCoverage <- 0.8
  options$plotsPriorIndividualCI <- TRUE
  options$plotsPriorIndividualEstimate <- TRUE
  options$plotsPriorIndividualEstimateType <- "median"
  options$plotsPriorIndividualType <- "HPD"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "individual"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mode"
  options$predictionN <- 10
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "individual"
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mode"
  options$priors <- list(list(name = "Models Beta", parAlpha = "2", parBeta = "2", 
                              parPoint = "0.5", type = "beta", value = ""), list(name = "Models Point", 
                                                                                 parAlpha = "1", parBeta = "1", parPoint = "0.3", type = "spike", 
                                                                                 value = "2"))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["collection"]][["containerPredictionPlots_plotsPredictions_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-vol5-1", dir="LSbinomialestimation")
  })
  
  test_that("Models Point plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["collection"]][["containerPredictionPlots_plotsPredictions_Models Point"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-point-vol5-2", dir="LSbinomialestimation")
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (60, 44)", 0.57843137254902, "beta (2, 2)",
                                        0.5, "Models Point", "spike at 0.3", 0.3, "spike at 0.3", 0.3
                                   ))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### more options vol. 6 (spike + beta)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- TRUE
  options$plotsIterative <- FALSE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mode"
  options$plotsIterativeIndividualCI <- TRUE
  options$plotsIterativeIndividualType <- "support"
  options$plotsIterativeInterval <- FALSE
  options$plotsIterativeIntervalLower <- 0
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.25
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- TRUE
  options$plotsPosteriorIndividualEstimate <- TRUE
  options$plotsPosteriorIndividualEstimateType <- "mode"
  options$plotsPosteriorIndividualType <- "support"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "individual"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- TRUE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- TRUE
  options$plotsPredictionEstimateType <- "mode"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "HPD"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- FALSE
  options$plotsPriorCoverage <- 0.8
  options$plotsPriorIndividualCI <- TRUE
  options$plotsPriorIndividualEstimate <- TRUE
  options$plotsPriorIndividualEstimateType <- "median"
  options$plotsPriorIndividualType <- "HPD"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "individual"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mode"
  options$predictionN <- 10
  options$predictionPlotProp <- TRUE
  options$predictionPlotType <- "stacked"
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mode"
  options$priors <- list(list(name = "Models Beta", parAlpha = "2", parBeta = "2", 
                              parPoint = "0.5", type = "beta", value = ""), list(name = "Models Point", 
                                                                                 parAlpha = "1", parBeta = "1", parPoint = "0.3", type = "spike", 
                                                                                 value = "2"))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Prediction plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prediction-plot-0-vol6-1", dir="LSbinomialestimation")
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (60, 44)", 0.57843137254902, "beta (2, 2)",
                                        0.5, "Models Point", "spike at 0.3", 0.3, "spike at 0.3", 0.3
                                   ))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### more options vol. 7 (spike + beta)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- TRUE
  options$plotsIterative <- FALSE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mode"
  options$plotsIterativeIndividualCI <- TRUE
  options$plotsIterativeIndividualType <- "support"
  options$plotsIterativeInterval <- FALSE
  options$plotsIterativeIntervalLower <- 0
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.25
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- TRUE
  options$plotsPosteriorIndividualEstimate <- TRUE
  options$plotsPosteriorIndividualEstimateType <- "mode"
  options$plotsPosteriorIndividualType <- "support"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "individual"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- TRUE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- TRUE
  options$plotsPredictionEstimateType <- "mode"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "HPD"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- FALSE
  options$plotsPriorCoverage <- 0.8
  options$plotsPriorIndividualCI <- TRUE
  options$plotsPriorIndividualEstimate <- TRUE
  options$plotsPriorIndividualEstimateType <- "median"
  options$plotsPriorIndividualType <- "HPD"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "individual"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mode"
  options$predictionN <- 10
  options$predictionPlotProp <- TRUE
  options$predictionPlotType <- "overlying"
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mode"
  options$priors <- list(list(name = "Models Beta", parAlpha = "2", parBeta = "2", 
                              parPoint = "0.5", type = "beta", value = ""), list(name = "Models Point", 
                                                                                 parAlpha = "1", parBeta = "1", parPoint = "0.3", type = "spike", 
                                                                                 value = "2"))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Prediction plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prediction-plot-0-vol7-1", dir="LSbinomialestimation")
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (60, 44)", 0.57843137254902, "beta (2, 2)",
                                        0.5, "Models Point", "spike at 0.3", 0.3, "spike at 0.3", 0.3
                                   ))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### output for all default settings (spike only)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsIterative <- TRUE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mean"
  options$plotsIterativeIndividualCI <- TRUE
  options$plotsIterativeIndividualType <- "central"
  options$plotsIterativeInterval <- TRUE
  options$plotsIterativeIntervalLower <- 0.25
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.75
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- FALSE
  options$plotsPosteriorIndividualEstimate <- FALSE
  options$plotsPosteriorIndividualEstimateType <- "mean"
  options$plotsPosteriorIndividualType <- "central"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "stacked"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- TRUE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorIndividualCI <- FALSE
  options$plotsPriorIndividualEstimate <- FALSE
  options$plotsPriorIndividualEstimateType <- "mean"
  options$plotsPriorIndividualType <- "central"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "overlying"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "median"
  options$predictionN <- 1
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "overlying"
  options$predictionTable <- TRUE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(name = "Models Spike", parAlpha = "1", parBeta = "1", 
                              parPoint = "0.5", type = "spike", value = ""))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Models Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_Models Spike"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-spike", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Updating plot matches", {
    plotName <- results[["results"]][["containerIterative"]][["collection"]][["containerIterative_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-1-spike-1", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Interval Updating  matches", {
    plotName <- results[["results"]][["containerIterativeInterval"]][["collection"]][["containerIterativeInterval_plotsIterativeInterval"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequantial-interval-plot-2-spike-2", dir="LSbinomialestimation")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-3-spike-3", dir="LSbinomialestimation")
  })
  
  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-4-spike-4", dir="LSbinomialestimation")
  })
  
  test_that("Predictions plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prediction-plot-5-spike-5", dir="LSbinomialestimation")
  })
  
  test_that("Predictions Summary table results match", {
    table <- results[["results"]][["containerPredictions"]][["collection"]][["containerPredictions_predictionsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Spike", "spike at 0.5", 0.5, "binomial (1, 0.5)", 0.5
                                   ))
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Spike", "spike at 0.5", 0.5, "spike at 0.5", 0.5))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### output for all default settings (beta only)
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "1"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "0"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsIterative <- TRUE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mean"
  options$plotsIterativeIndividualCI <- TRUE
  options$plotsIterativeIndividualType <- "central"
  options$plotsIterativeInterval <- TRUE
  options$plotsIterativeIntervalLower <- 0.25
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.75
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- FALSE
  options$plotsPosteriorIndividualEstimate <- FALSE
  options$plotsPosteriorIndividualEstimateType <- "mean"
  options$plotsPosteriorIndividualType <- "central"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "stacked"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPrior <- TRUE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorIndividualCI <- FALSE
  options$plotsPriorIndividualEstimate <- FALSE
  options$plotsPriorIndividualEstimateType <- "mean"
  options$plotsPriorIndividualType <- "central"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "overlying"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mode"
  options$predictionN <- 1
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "overlying"
  options$predictionTable <- TRUE
  options$predictionTableEstimate <- "median"
  options$priors <- list(list(name = "Models Beta", parAlpha = "1", parBeta = "1", 
                              parPoint = "0.5", type = "beta", value = ""))
  options$selectedVariable <- "contBinom"
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Models Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_Models Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "models-beta-beta-1", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Updating plot matches", {
    plotName <- results[["results"]][["containerIterative"]][["collection"]][["containerIterative_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-1-beta-2", dir="LSbinomialestimation")
  })
  
  test_that("Sequential Interval Updating plot matches", {
    plotName <- results[["results"]][["containerIterativeInterval"]][["collection"]][["containerIterativeInterval_plotsIterativeInterval"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-interval-plot-2-beta-3", dir="LSbinomialestimation")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-3-beta-4", dir="LSbinomialestimation")
  })
  
  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-4-beta-5", dir="LSbinomialestimation")
  })
  
  test_that("Predictions plot matches", {
    plotName <- results[["results"]][["containerPredictionPlots"]][["collection"]][["containerPredictionPlots_plotsPredictions"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predictions-plot-5-beta-6", dir="LSbinomialestimation")
  })
  
  test_that("Predictions Summary table results match", {
    table <- results[["results"]][["containerPredictions"]][["collection"]][["containerPredictions_predictionsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (59, 43)", 0.578945969319675, "beta-binomial (1, 59, 59)",
                                        1))
  })
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models Beta", "beta (59, 43)", 0.58, "beta (1, 1)", "[0, 1]"
                                   ))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(58, 0.58, "Successes", 42, 0.42, "Failures", 100, "", "Total"
                                   ))
  })
}
### different input types
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataCounts"
  options$dataSequenceInput <- ""
  options$doIterative <- FALSE
  options$introText <- FALSE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- list()
  options$keySuccessSeq <- list()
  options$keySuccessVar <- list()
  options$nFailures <- 6
  options$nSuccesses <- 3
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- FALSE
  options$plotsIterative <- FALSE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mean"
  options$plotsIterativeIndividualCI <- FALSE
  options$plotsIterativeIndividualType <- "central"
  options$plotsIterativeInterval <- FALSE
  options$plotsIterativeIntervalLower <- 0.25
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.75
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- FALSE
  options$plotsPosteriorIndividualEstimate <- FALSE
  options$plotsPosteriorIndividualEstimateType <- "mean"
  options$plotsPosteriorIndividualType <- "central"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "overlying"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- FALSE
  options$plotsPrior <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorIndividualCI <- FALSE
  options$plotsPriorIndividualEstimate <- FALSE
  options$plotsPriorIndividualEstimateType <- "mean"
  options$plotsPriorIndividualType <- "central"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "overlying"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mean"
  options$predictionN <- 1
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "overlying"
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(name = "Models 1", parAlpha = "1", parBeta = "1", parPoint = "0.5", 
                              type = "spike", value = ""), list(name = "Models 2", parAlpha = "1", 
                                                                parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- ""
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
  jasptools::expect_equal_tables(table,
                                 list("Models 1", "spike at 0.5", 0.5, "spike at 0.5", 0.5, "Models 2",
                                      "beta (4, 7)", 0.363636363636364, "beta (1, 1)", 0.5))
}
{
  options <- analysisOptions("LSbinomialestimation")
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataSequence"
  options$dataSequenceInput <- "1;0;0,10,1,1,0 ,1 ,
1
"
  options$doIterative <- FALSE
  options$introText <- FALSE
  options$keyFailureSeq <- "0"
  options$keyFailureVar <- list()
  options$keySuccessSeq <- "1"
  options$keySuccessVar <- list()
  options$nFailures <- 6
  options$nSuccesses <- 3
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- FALSE
  options$plotsIterative <- FALSE
  options$plotsIterativeBF <- 1
  options$plotsIterativeCoverage <- 0.95
  options$plotsIterativeEstimateType <- "mean"
  options$plotsIterativeIndividualCI <- FALSE
  options$plotsIterativeIndividualType <- "central"
  options$plotsIterativeInterval <- FALSE
  options$plotsIterativeIntervalLower <- 0.25
  options$plotsIterativeIntervalType <- "overlying"
  options$plotsIterativeIntervalUpdatingTable <- FALSE
  options$plotsIterativeIntervalUpper <- 0.75
  options$plotsIterativeType <- "overlying"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorIndividualCI <- FALSE
  options$plotsPosteriorIndividualEstimate <- FALSE
  options$plotsPosteriorIndividualEstimateType <- "mean"
  options$plotsPosteriorIndividualType <- "central"
  options$plotsPosteriorLower <- 0.25
  options$plotsPosteriorType <- "overlying"
  options$plotsPosteriorUpper <- 0.75
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionLower <- 0
  options$plotsPredictionType <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- FALSE
  options$plotsPrior <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorIndividualCI <- FALSE
  options$plotsPriorIndividualEstimate <- FALSE
  options$plotsPriorIndividualEstimateType <- "mean"
  options$plotsPriorIndividualType <- "central"
  options$plotsPriorLower <- 0.25
  options$plotsPriorType <- "overlying"
  options$plotsPriorUpper <- 0.75
  options$pointEstimate <- "mean"
  options$predictionN <- 1
  options$predictionPlotProp <- FALSE
  options$predictionPlotType <- "overlying"
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(name = "Models 1", parAlpha = "1", parBeta = "1", parPoint = "0.5", 
                              type = "spike", value = ""), list(name = "Models 2", parAlpha = "1", 
                                                                parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- ""
  set.seed(1)
  results <- run("LSbinomialestimation", "debug", options)
  
  
  test_that("Estimation Summary table results match", {
    table <- results[["results"]][["estimatesContainer"]][["collection"]][["estimatesContainer_estimatesTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Models 1", "spike at 0.5", 0.5, "spike at 0.5", 0.5, "Models 2",
                                        "beta (6, 4)", 0.6, "beta (1, 1)", 0.5))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(5, 0.625, "Successes", 3, 0.375, "Failures", 8, "", "Total"))
  })
}