context("SummaryStatsABTestBayesian")

options <- jasptools::analysisOptions("SummaryStatsABTestBayesian")
options$.meta <- list()
options$descriptives <- TRUE
options$n1 <- 25
options$n2 <- 27
options$numSamples <- 100
options$setSeed <- TRUE
options$y1 <- 5
options$y2 <- 10
set.seed(1)
results <- jasptools::run("SummaryStatsABTestBayesian", "", options)


test_that("Descriptives table results match", {
  table <- results[["results"]][["abTestBayesianDescriptivesTable"]][["data"]]
  expect_equal_tables(table,
                      list(5, "Group 1", 0.2, 25, 10, "Group 2", 0.37037037037037, 27))
})

test_that("Bayesian A/B Test table results match", {
  table <- results[["results"]][["abTestBayesianTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, "Log odds ratio = 0", 0.5, 0.478678749958352, 1.93830077634521,
                           "Log odds ratio &gt; 0", 0.25, 0.463911696332114, 0.239866731976423,
                           "Log odds ratio &lt; 0", 0.25, 0.0574095537095346))
})