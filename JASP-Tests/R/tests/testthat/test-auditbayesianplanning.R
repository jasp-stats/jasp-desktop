context("[Audit] Bayesian Planning")

options <- jasptools::analysisOptions("auditBayesianPlanning")
options$.meta <- list()
options$CR <- "High"
options$IR <- "High"
options$decisionPlot <- TRUE
options$expectedBF <- TRUE
options$expectedErrors <- "expectedRelative"
options$implicitSampleTable <- TRUE
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.05
options$planningModel <- "binomial"
options$populationSize <- 1000
options$priorPlot <- TRUE
options$priorStatistics <- TRUE
options$valuta <- "euroValuta"
set.seed(1)
results <- jasptools::run("auditBayesianPlanning", "sinoForest.csv", options)


test_that("Badge: <i>Annotated</i> plot matches", {
  plotName <- results[["results"]][["badgeSection"]][["collection"]][["badgeSection_annotationBadge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "badge-i-annotated-i-", dir="auditBayesianPlanning")
})

test_that("Decision Analysis Plot matches", {
  plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "decision-analysis-plot", dir="auditBayesianPlanning")
})

test_that("Implied Prior from Risk Assessments plot matches", {
  plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "implied-prior-from-risk-assessments", dir="auditBayesianPlanning")
})

test_that("<b>Table 3.</b> Prior and Expected Posterior Descriptive Statistics results match", {
  table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_priorStatistics"]][["data"]]
  expect_equal_tables(table,
                      list("Beta(<unicode><unicode> = 1, <unicode><unicode> = 1)", "95%",
                           0.95, 0.05, 0.05, "Prior", "Beta(<unicode><unicode> = 1, <unicode><unicode> = 59)",
                           "4.9508%", 0.05, 0.95, 19.62, "Expected posterior", "", "",
                           0.05, 19, 372.8, "Expected shift"))
})

test_that("<b>Table 2.</b> Implicit Sample results match", {
  table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_sampletable"]][["data"]]
  expect_equal_tables(table,
                      list(0, 0))
})

test_that("<b>Table 1.</b> Planning Summary results match", {
  table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
  expect_equal_tables(table,
                      list("100%", "5%", "100%", 372.8, 0, "5%", 58))
})