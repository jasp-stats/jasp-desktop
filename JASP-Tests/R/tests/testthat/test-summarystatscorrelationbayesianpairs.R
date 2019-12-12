context("SummaryStatsCorrelationBayesianPairs")

options <- jasptools::analysisOptions("SummaryStatsCorrelationBayesianPairs")
  options$pearsonRhoValue <- 0.7
  options$plotBayesFactorRobustness <- TRUE
  options$plotPriorAndPosterior <- TRUE
  options$priorWidth <- 1.2
  options$sampleSize <- 50
  set.seed(1)
  results <- jasptools::run("SummaryStatsCorrelationBayesianPairs", "test.csv", options)
  
  test_that("Bayesian Pearson Correlation table results match", {
    table <- results[["results"]][["table"]][["data"]]
    expect_equal_tables(table,
                        list(973435.048301983, 50, 0.7, 1.53820662839905e-08))
  })		
  
  test_that("Prior and Posterior plot matches", {		
    plotName <- results[["results"]][["inferentialPlots"]][["PriorPosteriorPlot"]][["data"]]		
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]		
    expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsCorrelationBayesianPairs")		
  })		
  
  test_that("Bayes Factor Robustness Check plot matches", {		
    plotName <- results[["results"]][["inferentialPlots"]][["BFrobustnessPlot"]][["data"]]		
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]		
    expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsCorrelationBayesianPairs")
  })