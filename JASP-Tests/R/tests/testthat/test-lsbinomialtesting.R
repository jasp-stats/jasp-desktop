context("Learn Bayes - Binomial Testing")

skip_on_travis()


# the conditional == individual plots are comprehensivelly tested in estimation part 

### output for all default settings (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "BF10"
  options$bayesFactorTypeSequential <- "BF10"
  options$bfType <- "inclusion"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- ""
  options$bfTypevsNameSequential <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- c("2", "3")
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsBothType <- "conditional"
  options$plotsIterative <- TRUE
  options$plotsIterativeType <- "conditional"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "overlying"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- FALSE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- FALSE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "central"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- FALSE
  options$plotsPosteriorType <- "joint"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "overlying"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- FALSE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- FALSE
  options$plotsPredictionMarginalEstimateType <- "mean"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "central"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- FALSE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- FALSE
  options$plotsPredictionPostMarginalEstimateType <- "mean"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "joint"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "conditional"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- TRUE
  options$plotsPredictiveAccuracy <- TRUE
  options$plotsPredictiveAccuracyType <- "conditional"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "overlying"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- FALSE
  options$plotsPriorMarginalCoverage <- 0.95
  options$plotsPriorMarginalEstimate <- FALSE
  options$plotsPriorMarginalEstimateType <- "mean"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "conditional"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 1
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""), 
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_Hypothesis Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-beta-default-1", dir="LSbinomialtesting")
  })
  
  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_Hypothesis Spike"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-spike-default-2", dir="LSbinomialtesting")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-2-default-3", dir="LSbinomialtesting")
  })
  
  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_plotsPredictionsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-prediction-plot-3-default-4", dir="LSbinomialtesting")
  })
  
  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior_Hypothesis Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-beta-default-5", dir="LSbinomialtesting")
  })
  
  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior_Hypothesis Spike"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-spike-default-6", dir="LSbinomialtesting")
  })
  
  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior_Hypothesis Beta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-beta-default-7", dir="LSbinomialtesting")
  })
  
  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior_Hypothesis Spike"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-spike-default-8", dir="LSbinomialtesting")
  })
  
  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predictive-accuracy-plot-8-default-9", dir="LSbinomialtesting")
  })
  
  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "titleless-plot-9-default-10", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(0.22178657769189, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, 4.50883912997303, "Hypothesis Beta", -4.11087386417331,
                                        0.818473551975932, 0.5))
  })
}
### more options vol. 1 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "BF01"
  options$bayesFactorTypeSequential <- "BF10"
  options$bfType <- "best"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- ""
  options$bfTypevsNameSequential <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- c("2", "3")
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsBothType <- "joint"
  options$plotsIterative <- TRUE
  options$plotsIterativeType <- "joint"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "overlying"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- FALSE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- TRUE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "central"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- FALSE
  options$plotsPosteriorType <- "marginal"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "overlying"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- FALSE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- FALSE
  options$plotsPredictionMarginalEstimateType <- "mean"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "central"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- FALSE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mean"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "marginal"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "marginal"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- TRUE
  options$plotsPredictiveAccuracy <- TRUE
  options$plotsPredictiveAccuracyType <- "joint"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "overlying"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- FALSE
  options$plotsPriorMarginalCoverage <- 0.95
  options$plotsPriorMarginalEstimate <- FALSE
  options$plotsPriorMarginalEstimateType <- "mean"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "marginal"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 1
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""), 
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-spike-vol1-1", dir="LSbinomialtesting")
  })
  
  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-beta-vol1-2", dir="LSbinomialtesting")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-2-vol1-3", dir="LSbinomialtesting")
  })
  
  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_plotsPredictionsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-prediction-plot-3-vol1-4", dir="LSbinomialtesting")
  })
  
  test_that("Prior Prediction matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-prediction-plot-4-vol1-5", dir="LSbinomialtesting")
  })
  
  test_that("Prior and Posterio plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-and-posterior-plot-5-vol1-6", dir="LSbinomialtesting")
  })
  
  test_that("Predictiove Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predictive-accuracy-plot-6-vol1-7", dir="LSbinomialtesting")
  })
  
  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-7-vol1-8", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(4.50883912997304, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, 1, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### more options vol. 2 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "LogBF10"
  options$bayesFactorTypeSequential <- "BF10"
  options$bfType <- "vs"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- "Hypothesis Spike"
  options$bfTypevsNameSequential <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- c("2", "3")
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsBothType <- "joint"
  options$plotsIterative <- TRUE
  options$plotsIterativeType <- "joint"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "overlying"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- FALSE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- TRUE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "central"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- FALSE
  options$plotsPosteriorType <- "marginal"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "overlying"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- TRUE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- TRUE
  options$plotsPredictionMarginalEstimateType <- "mean"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "central"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- FALSE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mean"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "marginal"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "marginal"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- TRUE
  options$plotsPredictiveAccuracy <- TRUE
  options$plotsPredictiveAccuracyType <- "joint"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "overlying"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- TRUE
  options$plotsPriorMarginalCoverage <- 0.95
  options$plotsPriorMarginalEstimate <- TRUE
  options$plotsPriorMarginalEstimateType <- "mean"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "marginal"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 1
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""), 
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-spike-vol2-1", dir="LSbinomialtesting")
  })
  
  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-beta-vol2-2", dir="LSbinomialtesting")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-2-vol2-3", dir="LSbinomialtesting")
  })
  
  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_plotsPredictionsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-prediction-plot-3-vol2-4", dir="LSbinomialtesting")
  })
  
  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-prediction-plot-4-vol2-5", dir="LSbinomialtesting")
  })
  
  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-5-vol2-6", dir="LSbinomialtesting")
  })
  
  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predicitve-accuracy-plot-6-vol2-6", dir="LSbinomialtesting")
  })
  
  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-7-vol2-7", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(0, "Hypothesis Spike", -5.616913585436, 0.181526448024068, 0.5,
                                        1.50603972126269, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### more options vol. 3 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "LogBF10"
  options$bayesFactorTypeSequential <- "BF10"
  options$bfType <- "vs"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- "Hypothesis Spike"
  options$bfTypevsNameSequential <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- c("2", "3")
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsBothType <- "joint"
  options$plotsIterative <- TRUE
  options$plotsIterativeType <- "joint"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "overlying"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- FALSE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- TRUE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "central"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- FALSE
  options$plotsPosteriorType <- "marginal"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "overlying"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- TRUE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- TRUE
  options$plotsPredictionMarginalEstimateType <- "mean"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "HPD"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- FALSE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mean"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "marginal"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "marginal"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- TRUE
  options$plotsPredictiveAccuracy <- TRUE
  options$plotsPredictiveAccuracyType <- "joint"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "overlying"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- TRUE
  options$plotsPriorMarginalCoverage <- 0.95
  options$plotsPriorMarginalEstimate <- TRUE
  options$plotsPriorMarginalEstimateType <- "mean"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "custom"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "marginal"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 1
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""), 
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Hypothesis Spike plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-spike-vol3-1", dir="LSbinomialtesting")
  })
  
  test_that("Hypothesis Beta plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-beta-vol3-2", dir="LSbinomialtesting")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-2-vol3-3", dir="LSbinomialtesting")
  })
  
  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_plotsPredictionsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-prediction-plot-3-vol3-4", dir="LSbinomialtesting")
  })
  
  test_that("Prior-Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-prediction-plot-4-vol3-5", dir="LSbinomialtesting")
  })
  
  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-5-vol3-6", dir="LSbinomialtesting")
  })
  
  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predictive-accuracy-plot-6-vol3-7", dir="LSbinomialtesting")
  })
  
  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-7-vol3-8", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(0, "Hypothesis Spike", -5.616913585436, 0.181526448024068, 0.5,
                                        1.50603972126269, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  }) 
}
### more options vol. 4 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "LogBF10"
  options$bayesFactorTypeSequential <- "BF10"
  options$bfType <- "vs"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- "Hypothesis Spike"
  options$bfTypevsNameSequential <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- c("2", "3")
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsBothType <- "marginal"
  options$plotsIterative <- TRUE
  options$plotsIterativeType <- "marginal"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "overlying"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- TRUE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- TRUE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "support"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- FALSE
  options$plotsPosteriorType <- "marginal"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "overlying"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- TRUE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- TRUE
  options$plotsPredictionMarginalEstimateType <- "median"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "custom"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- FALSE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mode"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "marginal"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "marginal"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- TRUE
  options$plotsPredictiveAccuracy <- TRUE
  options$plotsPredictiveAccuracyType <- "marginal"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "overlying"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- TRUE
  options$plotsPriorMarginalCoverage <- 0.01
  options$plotsPriorMarginalEstimate <- TRUE
  options$plotsPriorMarginalEstimateType <- "median"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "marginal"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 1
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""), 
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Prior and Posterior plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_plot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-and-posterior-plot-0-vol4-1", dir="LSbinomialtesting")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-1-vol4-2", dir="LSbinomialtesting")
  })
  
  test_that("Posterior prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_plotsPredictionsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-prediction-plot-2-vol4-3", dir="LSbinomialtesting")
  })
  
  test_that("Prior prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-prediction-plot-3-vol4-4", dir="LSbinomialtesting")
  })
  
  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-4-vol4-5", dir="LSbinomialtesting")
  })
  
  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predictive-accuracy-plot-5-vol4-6", dir="LSbinomialtesting")
  })
  
  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-6-vol4-7", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(0, "Hypothesis Spike", -5.616913585436, 0.181526448024068, 0.5,
                                        1.50603972126269, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### more options vol. 5 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "LogBF10"
  options$bayesFactorTypeSequential <- "BF01"
  options$bfType <- "vs"
  options$bfTypeSequential <- "vs"
  options$bfTypevsName <- "Hypothesis Beta"
  options$bfTypevsNameSequential <- "Hypothesis Spike"
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- c("2", "3")
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- TRUE
  options$plotsBothType <- "marginal"
  options$plotsIterative <- TRUE
  options$plotsIterativeType <- "BF"
  options$plotsIterativeUpdatingTable <- TRUE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "stacked"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- TRUE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- TRUE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "support"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- TRUE
  options$plotsPosteriorType <- "joint"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "stacked"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- TRUE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- TRUE
  options$plotsPredictionMarginalEstimateType <- "median"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "custom"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- FALSE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mode"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "marginal"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "joint"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- FALSE
  options$plotsPredictiveAccuracy <- FALSE
  options$plotsPredictiveAccuracyType <- "marginal"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "stacked"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- TRUE
  options$plotsPriorMarginalCoverage <- 0.01
  options$plotsPriorMarginalEstimate <- TRUE
  options$plotsPriorMarginalEstimateType <- "median"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "joint"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 1
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- TRUE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""), 
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Prior and Posterio matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_plot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-and-posterior-plot-0-vol5-1", dir="LSbinomialtesting")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-1-vol5-2", dir="LSbinomialtesting")
  })
  
  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-prediction-plot-2-vol5-3", dir="LSbinomialtesting")
  })
  
  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-3-vol5-4", dir="LSbinomialtesting")
  })
  
  test_that("Predictions table results match", {
    table <- results[["results"]][["containerPredictions"]][["collection"]][["containerPredictions_predictionsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Hypothesis Spike", "spike at 0.5", 0.5, "binomial (1, 0.5)",
                                        0.5, 0.181526448024068, "Hypothesis Beta", "beta (21, 41)",
                                        0.338709677419355, "beta-binomial (1, 21, 21)", 0.338709677419355,
                                        0.818473551975932, "Marginal", 0.367988136778075, 0.367988136778075
                                   ))
  })
  
  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-5-vol5-5", dir="LSbinomialtesting")
  })
  
  test_that("Sequentail table results match", {
    table <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_tableIterative"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(1, 1, 1, 1.5, 1, 2, 1.5, 1, 3, 1.875, 1, 4, 1.875, 1, 5, 1.640625,
                                        1, 6, 2.1875, 1, 7, 1.96875, 1, 8, 1.640625, 1, 9, 2.255859375,
                                        1, 10, 1.93359375, 1, 11, 1.571044921875, 1, 12, 2.199462890625,
                                        1, 13, 1.8328857421875, 1, 14, 1.46630859375, 1, 15, 2.0772705078125,
                                        1, 16, 1.6995849609375, 1, 17, 1.34550476074219, 1, 18, 1.92214965820313,
                                        1, 19, 1.55250549316406, 1, 20, 1.21982574462891, 1, 21, 1.75349950790405,
                                        1, 22, 1.40279960632324, 1, 23, 1.09593719244004, 1, 24, 1.58302038908005,
                                        1, 25, 1.25710442662239, 1, 26, 0.977747887372972, 1, 27, 1.41773443669081,
                                        1, 28, 1.11926402896643, 1, 29, 0.867429622448983, 1, 30, 1.26171581447124,
                                        1, 31, 0.991348139941694, 1, 32, 0.7660417445004, 1, 33, 1.11714421072975,
                                        1, 34, 0.874286773614583, 1, 35, 0.673929387994579, 1, 36, 0.984973720915149,
                                        1, 37, 0.768279502313816, 1, 38, 0.590984232549092, 1, 39, 0.865369769089739,
                                        1, 40, 0.673065375958685, 1, 41, 0.516818056539706, 1, 42, 0.757999816258231,
                                        1, 43, 0.588103305717597, 1, 44, 0.450879201050157, 1, 45, 0.66222882654242,
                                        1, 46, 0.512693285065102, 1, 47, 0.392530796377967, 1, 48, 0.577251171144071,
                                        1, 49, 0.446057723156778, 1, 50, 0.34110296476695, 1, 51, 0.502179364795783,
                                        1, 52, 0.387395509985322, 1, 53, 0.295927125683231, 1, 54, 0.436103132585814,
                                        1, 55, 0.335917277802586, 1, 56, 0.256357922533554, 1, 57, 0.378127935736991,
                                        1, 58, 0.290867642874606, 1, 59, 0.22178657769189, 1, 60))
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(-1.50603972126269, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, 0, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### more options vol. 6 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "BF01"
  options$bayesFactorTypeSequential <- "LogBF10"
  options$bfType <- "best"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- "Hypothesis Beta"
  options$bfTypevsNameSequential <- "Hypothesis Spike"
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- c("2", "3")
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- TRUE
  options$plotsBothType <- "marginal"
  options$plotsIterative <- TRUE
  options$plotsIterativeType <- "BF"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "stacked"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- TRUE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- TRUE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "support"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- TRUE
  options$plotsPosteriorType <- "joint"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "stacked"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- TRUE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- TRUE
  options$plotsPredictionMarginalEstimateType <- "median"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "custom"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- TRUE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mode"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "marginal"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "joint"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- FALSE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- TRUE
  options$plotsPredictiveAccuracy <- FALSE
  options$plotsPredictiveAccuracyType <- "marginal"
  options$plotsPrior <- FALSE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "stacked"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- TRUE
  options$plotsPriorMarginalCoverage <- 0.01
  options$plotsPriorMarginalEstimate <- TRUE
  options$plotsPriorMarginalEstimateType <- "median"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "joint"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 10
  options$predictionPostPlotProp <- TRUE
  options$predictionPostPlotTable <- TRUE
  options$predictionTable <- TRUE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""), 
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Prediction Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_plotsPredictionsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "titleless-plot-0-vol6-1", dir="LSbinomialtesting")
  })
  
  test_that("Prediction table results match", {
    table <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_tablePredictions"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(0.0183858383377217, 0, 0.0782486981654427, 0.1, 0.162489928642561,
                                        0.2, 0.218705515050819, 0.3, 0.213656489678812, 0.4, 0.159735167414158,
                                        0.5, 0.0926276354595948, 0.6, 0.0406987911504769, 0.7, 0.0127208338223604,
                                        0.8, 0.00250057277861945, 0.9, 0.000230529499432729, 1))
  })
  
  test_that("Prediction summary table match", {
    table <- results[["results"]][["containerPredictions"]][["collection"]][["containerPredictions_predictionsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("Hypothesis Spike", "spike at 0.5", 0.5, "binomial (10, 0.5)",
                                        5, 0.181526448024068, "Hypothesis Beta", "beta (21, 41)", 0.338709677419355,
                                        "beta-binomial (10, 21, 21)", 3.38709677419355, 0.818473551975932,
                                        "Marginal", 0.367988136778075, 3.67988136778075))
  })
  
  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-3-vol6-2", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(4.50883912997304, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, 1, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### more options vol. 7 (spike + beta)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "BF01"
  options$bayesFactorTypeSequential <- "LogBF10"
  options$bfType <- "best"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- "Hypothesis Beta"
  options$bfTypevsNameSequential <- "Hypothesis Spike"
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- c("2", "3")
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- TRUE
  options$plotsBothType <- "marginal"
  options$plotsIterative <- FALSE
  options$plotsIterativeType <- "BF"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "stacked"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- TRUE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- TRUE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "support"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- TRUE
  options$plotsPosteriorType <- "joint"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "stacked"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- TRUE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- TRUE
  options$plotsPredictionMarginalEstimateType <- "median"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "custom"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- TRUE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- FALSE
  options$plotsPredictionPostMarginalEstimateType <- "mode"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "HPD"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "marginal"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "joint"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- FALSE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- TRUE
  options$plotsPredictiveAccuracy <- FALSE
  options$plotsPredictiveAccuracyType <- "marginal"
  options$plotsPrior <- FALSE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "stacked"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- TRUE
  options$plotsPriorMarginalCoverage <- 0.01
  options$plotsPriorMarginalEstimate <- TRUE
  options$plotsPriorMarginalEstimateType <- "median"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "joint"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 10
  options$predictionPostPlotProp <- TRUE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis Spike", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "spike", value = ""), 
                         list(PH = "1", name = "Hypothesis Beta", parAlpha = "1", 
                              parBeta = "1", parPoint = "0.5", type = "beta", value = "2"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_plotsPredictionsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-prediction-plot-0-vol7-1", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.333333333333333, "Successes", 40, 0.666666666666667, "Failures",
                                        60, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(4.50883912997304, "Hypothesis Spike", -5.616913585436, 0.181526448024068,
                                        0.5, 1, "Hypothesis Beta", -4.11087386417331, 0.818473551975932,
                                        0.5))
  })
}
### output for all default settings (spike only)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "BF10"
  options$bayesFactorTypeSequential <- "BF10"
  options$bfType <- "inclusion"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- ""
  options$bfTypevsNameSequential <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "4"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsBothType <- "joint"
  options$plotsIterative <- TRUE
  options$plotsIterativeType <- "joint"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "overlying"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- FALSE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- FALSE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "central"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- FALSE
  options$plotsPosteriorType <- "marginal"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "stacked"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- FALSE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- FALSE
  options$plotsPredictionMarginalEstimateType <- "mean"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "central"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- TRUE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mean"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "marginal"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "joint"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- TRUE
  options$plotsPredictiveAccuracy <- TRUE
  options$plotsPredictiveAccuracyType <- "conditional"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "overlying"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- FALSE
  options$plotsPriorMarginalCoverage <- 0.95
  options$plotsPriorMarginalEstimate <- FALSE
  options$plotsPriorMarginalEstimateType <- "mean"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "joint"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 1
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis 1", parAlpha = "1", parBeta = "1", 
                              parPoint = "0.5", type = "spike", value = ""))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Hypothesis 1 plot matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "hypothesis-1-spike-1", dir="LSbinomialtesting")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-1-spike-2", dir="LSbinomialtesting")
  })
  
  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_plotsPredictionsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-prediction-plot-2-spike-3", dir="LSbinomialtesting")
  })
  
  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-prediction-plot-3-spike-4", dir="LSbinomialtesting")
  })
  
  test_that("Prior matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-prediction-plot-4-spike-5", dir="LSbinomialtesting")
  })
  
  test_that("Predictive Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predictive-accuracy-plot-5-spike-6", dir="LSbinomialtesting")
  })
  
  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-6-spike-7", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.5, "Successes", 20, 0.5, "Failures", 40, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("NaN", "Hypothesis 1", -2.07648042914739, 1, 1))
  })
}
### output for all default settings (beta only)
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "BF10"
  options$bayesFactorTypeSequential <- "BF10"
  options$bfType <- "inclusion"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- ""
  options$bfTypevsNameSequential <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "4"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- TRUE
  options$plotsBothSampleProportion <- FALSE
  options$plotsBothType <- "marginal"
  options$plotsIterative <- TRUE
  options$plotsIterativeType <- "marginal"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- TRUE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "stacked"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- FALSE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- FALSE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "central"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- FALSE
  options$plotsPosteriorType <- "joint"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "stacked"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- TRUE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- FALSE
  options$plotsPredictionMarginalEstimateType <- "mean"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "HPD"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- TRUE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mean"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "joint"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "marginal"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- TRUE
  options$plotsPredictiveAccuracy <- TRUE
  options$plotsPredictiveAccuracyType <- "marginal"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "overlying"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- FALSE
  options$plotsPriorMarginalCoverage <- 0.95
  options$plotsPriorMarginalEstimate <- TRUE
  options$plotsPriorMarginalEstimateType <- "mean"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "marginal"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 10
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis 1", parAlpha = "1", parBeta = "1", 
                              parPoint = "0.5", type = "beta", value = ""))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Prior and Posterior matches", {
    plotName <- results[["results"]][["containerBoth"]][["collection"]][["containerBoth_plotsBoth"]][["collection"]][["containerBoth_plotsBoth_plotsBoth_plot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-and-posterior-plot-0-beta-1", dir="LSbinomialtesting")
  })
  
  test_that("Posterior plot matches", {
    plotName <- results[["results"]][["containerPlotsPosterior"]][["collection"]][["containerPlotsPosterior_plotsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-plot-1-beta-2", dir="LSbinomialtesting")
  })
  
  test_that("Posterior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPosterior"]][["collection"]][["containerPlotsPredictionPosterior_plotsPredictionsPosterior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "posterior-prediction-plot-2-beta-3", dir="LSbinomialtesting")
  })
  
  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-prediction-plot-3-beta-4", dir="LSbinomialtesting")
  })
  
  test_that("Prior plor matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-4-beta-5", dir="LSbinomialtesting")
  })
  
  test_that("Predictive-Accuracy plot matches", {
    plotName <- results[["results"]][["containerPredictiveAccuracy"]][["collection"]][["containerPredictiveAccuracy_plotsPredAccuracy"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "predictive-accuracy-plot-5-beta-6", dir="LSbinomialtesting")
  })
  
  test_that("Sequential plot matches", {
    plotName <- results[["results"]][["containerSequentialTests"]][["collection"]][["containerSequentialTests_plotsIterative"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "sequential-plot-6-beta-7", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.5, "Successes", 20, 0.5, "Failures", 40, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list("NaN", "Hypothesis 1", -3.71357206670431, 1, 1))
  })
}
### some challenging plots
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "BF10"
  options$bayesFactorTypeSequential <- "BF10"
  options$bfType <- "inclusion"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- ""
  options$bfTypevsNameSequential <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "4"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- FALSE
  options$plotsBothType <- "marginal"
  options$plotsIterative <- FALSE
  options$plotsIterativeType <- "marginal"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "stacked"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- FALSE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- FALSE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "central"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- FALSE
  options$plotsPosteriorType <- "joint"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "stacked"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- TRUE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- TRUE
  options$plotsPredictionMarginalEstimateType <- "median"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "HPD"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- TRUE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mean"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "joint"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "marginal"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- FALSE
  options$plotsPredictiveAccuracy <- FALSE
  options$plotsPredictiveAccuracyType <- "marginal"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "overlying"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- TRUE
  options$plotsPriorMarginalCoverage <- 0.95
  options$plotsPriorMarginalEstimate <- TRUE
  options$plotsPriorMarginalEstimateType <- "mean"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "central"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "marginal"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 10
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis 1", parAlpha = ".1", parBeta = "1", 
                              parPoint = "0.5", type = "beta", value = ""), list(PH = "1", 
                                                                                 name = "Hypothesis 2", parAlpha = "1", parBeta = ".1", parPoint = "0.5", 
                                                                                 type = "beta", value = "2"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-prediction-plot-0-hard-1", dir="LSbinomialtesting")
  })
  
  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-1-hard-2", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.5, "Successes", 20, 0.5, "Failures", 40, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(1, "Hypothesis 1", -5.37126513656346, 0.5, 0.5, 1, "Hypothesis 2",
                                        -5.37126513656346, 0.5, 0.5))
  })
}
{
  options <- analysisOptions("LSbinomialtesting")
  options$bayesFactorType <- "BF10"
  options$bayesFactorTypeSequential <- "BF10"
  options$bfType <- "inclusion"
  options$bfTypeSequential <- "inclusion"
  options$bfTypevsName <- ""
  options$bfTypevsNameSequential <- ""
  options$colorPalette <- "colorblind"
  options$colorPalettePrediction <- "colorblind"
  options$dataSummary <- TRUE
  options$dataType <- "dataVariable"
  options$dataSequenceInput <- ""
  options$introText <- TRUE
  options$keyFailureSeq <- list()
  options$keyFailureVar <- "4"
  options$keySuccessSeq <- list()
  options$keySuccessVar <- "1"
  options$nFailures <- 0
  options$nSuccesses <- 0
  options$plotsBoth <- FALSE
  options$plotsBothSampleProportion <- FALSE
  options$plotsBothType <- "marginal"
  options$plotsIterative <- FALSE
  options$plotsIterativeType <- "marginal"
  options$plotsIterativeUpdatingTable <- FALSE
  options$plotsPosterior <- FALSE
  options$plotsPosteriorBF <- 1
  options$plotsPosteriorCI <- FALSE
  options$plotsPosteriorCoverage <- 0.95
  options$plotsPosteriorEstimate <- FALSE
  options$plotsPosteriorEstimateType <- "mean"
  options$plotsPosteriorJointType <- "stacked"
  options$plotsPosteriorLower <- 0
  options$plotsPosteriorMarginalBF <- 1
  options$plotsPosteriorMarginalCI <- FALSE
  options$plotsPosteriorMarginalCoverage <- 0.95
  options$plotsPosteriorMarginalEstimate <- FALSE
  options$plotsPosteriorMarginalEstimateType <- "mean"
  options$plotsPosteriorMarginalLower <- 0.25
  options$plotsPosteriorMarginalType <- "central"
  options$plotsPosteriorMarginalUpper <- 0.75
  options$plotsPosteriorObserved <- FALSE
  options$plotsPosteriorType <- "joint"
  options$plotsPosteriorTypeCI <- "central"
  options$plotsPosteriorUpper <- 1
  options$plotsPredictionCI <- FALSE
  options$plotsPredictionCoverage <- 0.95
  options$plotsPredictionEstimate <- FALSE
  options$plotsPredictionEstimateType <- "mean"
  options$plotsPredictionJointType <- "stacked"
  options$plotsPredictionLower <- 0
  options$plotsPredictionMarginalCI <- TRUE
  options$plotsPredictionMarginalCoverage <- 0.95
  options$plotsPredictionMarginalEstimate <- TRUE
  options$plotsPredictionMarginalEstimateType <- "median"
  options$plotsPredictionMarginalLower <- 0
  options$plotsPredictionMarginalTypeCI <- "HPD"
  options$plotsPredictionMarginalUpper <- 1
  options$plotsPredictionPostCI <- FALSE
  options$plotsPredictionPostCoverage <- 0.95
  options$plotsPredictionPostEstimate <- FALSE
  options$plotsPredictionPostEstimateType <- "mean"
  options$plotsPredictionPostJointType <- "overlying"
  options$plotsPredictionPostLower <- 0
  options$plotsPredictionPostMarginalCI <- TRUE
  options$plotsPredictionPostMarginalCoverage <- 0.95
  options$plotsPredictionPostMarginalEstimate <- TRUE
  options$plotsPredictionPostMarginalEstimateType <- "mean"
  options$plotsPredictionPostMarginalLower <- 0
  options$plotsPredictionPostMarginalTypeCI <- "central"
  options$plotsPredictionPostMarginalUpper <- 1
  options$plotsPredictionPostType <- "joint"
  options$plotsPredictionPostTypeCI <- "central"
  options$plotsPredictionPostUpper <- 1
  options$plotsPredictionType <- "marginal"
  options$plotsPredictionTypeCI <- "central"
  options$plotsPredictionUpper <- 1
  options$plotsPredictions <- TRUE
  options$plotsPredictionsObserved <- FALSE
  options$plotsPredictionsPost <- FALSE
  options$plotsPredictiveAccuracy <- FALSE
  options$plotsPredictiveAccuracyType <- "marginal"
  options$plotsPrior <- TRUE
  options$plotsPriorCI <- FALSE
  options$plotsPriorCoverage <- 0.95
  options$plotsPriorEstimate <- FALSE
  options$plotsPriorEstimateType <- "mean"
  options$plotsPriorJointType <- "overlying"
  options$plotsPriorLower <- 0.25
  options$plotsPriorMarginalCI <- TRUE
  options$plotsPriorMarginalCoverage <- 0.95
  options$plotsPriorMarginalEstimate <- TRUE
  options$plotsPriorMarginalEstimateType <- "mode"
  options$plotsPriorMarginalLower <- 0.25
  options$plotsPriorMarginalType <- "HPD"
  options$plotsPriorMarginalUpper <- 0.75
  options$plotsPriorType <- "marginal"
  options$plotsPriorTypeCI <- "central"
  options$plotsPriorUpper <- 0.75
  options$predictionN <- 10
  options$predictionPostPlotProp <- FALSE
  options$predictionPostPlotTable <- FALSE
  options$predictionTable <- FALSE
  options$predictionTableEstimate <- "mean"
  options$priors <- list(list(PH = "1", name = "Hypothesis 1", parAlpha = ".1", parBeta = "1", 
                              parPoint = "0.5", type = "beta", value = ""), list(PH = "1", 
                                                                                 name = "Hypothesis 2", parAlpha = "1", parBeta = ".1", parPoint = "0.5", 
                                                                                 type = "beta", value = "2"), list(PH = "1", name = "Hypothesis 3", 
                                                                                                                   parAlpha = "1", parBeta = "1", parPoint = "0.3", type = "spike", 
                                                                                                                   value = "3"), list(PH = "1", name = "Hypothesis 4", parAlpha = "1", 
                                                                                                                                      parBeta = "1", parPoint = "0.5", type = "spike", value = "4"), 
                         list(PH = "1", name = "Hypothesis 5", parAlpha = "1", parBeta = "1", 
                              parPoint = "0.6", type = "spike", value = "5"))
  options$selectedVariable <- "facFive"
  set.seed(1)
  results <- run("LSbinomialtesting", "debug", options)
  
  
  test_that("Prior Prediction plot matches", {
    plotName <- results[["results"]][["containerPlotsPredictionPrior"]][["collection"]][["containerPlotsPredictionPrior_plotsPredictionsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-prediction-plot-0-hard-3", dir="LSbinomialtesting")
  })
  
  test_that("Prior plot matches", {
    plotName <- results[["results"]][["containerPlotsPrior"]][["collection"]][["containerPlotsPrior_plotsPrior"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jasptools::expect_equal_plots(testPlot, "prior-plot-1-hard-4", dir="LSbinomialtesting")
  })
  
  test_that("Data Summary table results match", {
    table <- results[["results"]][["summaryContainer"]][["collection"]][["summaryContainer_summaryTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(20, 0.5, "Successes", 20, 0.5, "Failures", 40, "", "Total"))
  })
  
  test_that("Testing Summary table results match", {
    table <- results[["results"]][["testsContainer"]][["collection"]][["testsContainer_testsTable"]][["data"]]
    jasptools::expect_equal_tables(table,
                                   list(0.0982361798125317, "Hypothesis 1", -5.37126513656346, 0.0239703559049214,
                                        0.2, 0.0982361798125317, "Hypothesis 2", -5.37126513656346,
                                        0.0239703559049214, 0.2, 0.0807053373343048, "Hypothesis 3",
                                        -5.56354817204294, 0.0197773008984337, 0.2, 7.31602582186271,
                                        "Hypothesis 4", -2.07648042914739, 0.646519010917071, 0.2, 1.60038176080074,
                                        "Hypothesis 5", -2.89292031955249, 0.285762976374653, 0.2))
  })
}