context("[Audit] Classical Planning")

options <- jasptools::analysisOptions("auditClassicalPlanning")
options$.meta <- list()
options$CR <- "High"
options$IR <- "High"
options$decisionPlot <- TRUE
options$expectedErrors <- "expectedRelative"
options$materiality <- "materialityRelative"
options$materialityPercentage <- 0.05
options$planningModel <- "binomial"
options$populationSize <- 1000
options$samplingDistribution <- TRUE
options$valuta <- "euroValuta"
set.seed(1)
results <- jasptools::run("auditClassicalPlanning", "sinoForest.csv", options)

test_that("Badge: <i>Annotated</i> plot matches", {
  plotName <- results[["results"]][["badgeSection"]][["collection"]][["badgeSection_annotationBadge"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "badge-i-annotated-i-", dir="auditClassicalPlanning")
})

test_that("Decision Analysis Plot matches", {
  plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_decisionPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "decision-analysis-plot", dir="auditClassicalPlanning")
})

test_that("Implied Binomial Sampling Distribution plot matches", {
  plotName <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_samplingDistribution"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "implied-binomial-sampling-distribution", dir="auditClassicalPlanning")
})

test_that("<b>Table 1.</b> Planning Summary results match", {
  table <- results[["results"]][["planningContainer"]][["collection"]][["planningContainer_summaryTable"]][["data"]]
  expect_equal_tables(table,
                      list("100%", "5%", "100%", 0, "5%", 59))
})