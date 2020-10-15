context("AB Test Bayesian")


test_that("Main table results match", {
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"

  results  <- jaspTools::run("ABTestBayesian", "ab_data.csv", options)
  table    <- results[["results"]][["abTestBayesianTable"]][["data"]]
  refTable <- list("Log odds ratio = 0", 1                , 0.517752872701024, 0.5,
                   "Log odds ratio &gt; 0", 1.23201979244712 , 0.318940893382007, 0.25,
                   "Log odds ratio &lt; 0", 0.630826954431096, 0.163306233916969, 0.25)

  expect_equal_tables(table, refTable)
})


test_that("Descriptives table results match", {
  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$descriptives <- TRUE

  results  <- jaspTools::run("ABTestBayesian", "ab_data.csv", options)
  table    <- results[["results"]][["abTestBayesianDescriptivesTable"]][["data"]]

  refTable <- list("Group 1", 2, 4, 0.500,
                   "Group 2", 3, 4, 0.750)

  expect_equal_tables(table, refTable)
})


test_that("Main table results, log odds neq 0", {
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$orEqualTo1Prob     <- 0.495
  options$orGreaterThan1Prob <- 0.168
  options$orLessThan1Prob    <- 0.168
  options$orNotEqualTo1Prob  <- 0.168

  results  <- jaspTools::run("ABTestBayesian", "ab_data.csv", options)
  table    <- results[["results"]][["abTestBayesianTable"]][["data"]]

  refTable <- list("Log odds ratio = 0"        , 1                , 0.51649969761886 , 0.495495495495495,
                   "Log odds ratio &gt; 0"     , 1.23201979244712 , 0.215969209785004, 0.168168168168168,
                   "Log odds ratio <unicode> 0", 0.895333193589823, 0.15694910382065 , 0.168168168168168,
                   "Log odds ratio &lt; 0"     , 0.630826954431096, 0.110581988775487, 0.168168168168168)

  expect_equal_tables(table, refTable)
})


test_that("Main table results, only 2 hypotheses", {
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$orEqualTo1Prob     <- 0.5
  options$orGreaterThan1Prob <- 0.5
  options$orLessThan1Prob    <- 0
  options$orNotEqualTo1Prob  <- 0

  results  <- jaspTools::run("ABTestBayesian", "ab_data.csv", options)
  table    <- results[["results"]][["abTestBayesianTable"]][["data"]]

  refTable <- list("Log odds ratio &gt; 0", 1                , 0.55197529906147, 0.5,
                   "Log odds ratio = 0"   , 0.811675271883202, 0.44802470093853, 0.5)

  expect_equal_tables(table, refTable)
})


test_that("Prior plot matches", {
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$orEqualTo1Prob     <- 0.5
  options$orGreaterThan1Prob <- 0.5
  options$orLessThan1Prob    <- 0
  options$orNotEqualTo1Prob  <- 0
  options$plotPriorOnly      <- TRUE

  results  <- jaspTools::run("ABTestBayesian", "ab_data.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]

  expect_equal_plots(testPlot, "prioronly", dir="ABTestBayesian")
})


test_that("Posterior plot matches", {
  skip("Needs to be verified.")
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$orEqualTo1Prob     <- 0.5
  options$orGreaterThan1Prob <- 0.5
  options$orLessThan1Prob    <- 0
  options$orNotEqualTo1Prob  <- 0
  options$plotPriorAndPosterior <- TRUE

  results  <- jaspTools::run("ABTestBayesian", "ab_data.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]

  expect_equal_plots(testPlot, "posterior", dir="ABTestBayesian")
})


test_that("Sequential plot matches", {
  skip("This test need to be verified")
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$orEqualTo1Prob     <- 0.5
  options$orGreaterThan1Prob <- 0.5
  options$orLessThan1Prob    <- 0
  options$orNotEqualTo1Prob  <- 0
  options$plotSequentialAnalysis <- TRUE

  results  <- jaspTools::run("ABTestBayesian", "ab_data.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]

  expect_equal_plots(testPlot, "sequential", dir="ABTestBayesian")
})


test_that("plotRobustness plot matches", {
  skip("Have to set a global theme.")
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$orEqualTo1Prob     <- 0.5
  options$orGreaterThan1Prob <- 0.5
  options$orLessThan1Prob    <- 0
  options$orNotEqualTo1Prob  <- 0
  options$plotRobustness     <- TRUE

  results  <- jaspTools::run("ABTestBayesian", "ab_data.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]

  expect_equal_plots(testPlot, "robustness", dir="ABTestBayesian")
})
