context("Network Analysis")

# does not test
# - error handling
# - bootstrapping
# - plots or graphical options

options <- jaspTools::analysisOptions("NetworkAnalysis")
options$estimator <- "EBICglasso"
options$variables <- c("contNormal", "contcor1", "contcor2")
options$tableCentrality <- TRUE
options$tableClustering <- TRUE
options$tableWeightsMatrix <- TRUE
options$tableLayout <- TRUE
results <- jaspTools::run("NetworkAnalysis", "test.csv", options)

test_that("generalTB table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_generalTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.333333333333333, 3, "2 / 3")
  )
})

test_that("centralityTB table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_centralityTable"]][["data"]]
  expect_equal_tables(table,
                      list(-0.577350269189626, -1.12003079401266, -1.14291478283658, -1.14291478283658,
                           "contNormal", 1.15470053837925, 0.803219560925303, 0.713968266021615,
                           0.713968266021615, "contcor1", -0.577350269189626, 0.316811233087358,
                           0.428946516814968, 0.428946516814968, "contcor2")
  )
})

test_that("clusteringTB table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_clusteringTable"]][["data"]]
  expect_equal_tables(table,
                      list("contcor1", 0, 0, 0, 0, "contcor2", 0, 0, 0, 0, "contNormal",
                           0, 0, 0, 0)
  )
})

test_that("weightmatrixTB table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_weightsTable"]][["data"]]
  expect_equal_tables(table,
                      list("contNormal", 0, 0.0939476582188346, 0, "contcor1", 0.0939476582188346,
                           0, 0.612057902640958, "contcor2", 0, 0.612057902640958, 0)
  )
})


options <- jaspTools::analysisOptions("NetworkAnalysis")
options$bootstrapOnOff <- TRUE
options$numberOfBootstraps <- 2
options$plotCentrality <- TRUE
options$plotClustering <- TRUE
options$plotNetwork <- TRUE
options$showLegend <- "All plots"
options$showVariableNames <- "In nodes"
options$tableCentrality <- TRUE
options$tableClustering <- TRUE
options$tableWeightsMatrix <- TRUE
options$variables <- list("A1", "A2", "A3", "A4", "A5")
estimators <- c("EBICglasso","cor","pcor","IsingFit","IsingSampler","huge","adalasso")
file <- file.path(jaspTools:::.pkgOptions$tests.dir, "networkResults.rds")

# run the code below to create the .rds object
# clearEverythingButData <- function(x) {
#  # a small helper to remove everything but the data from the jaspResults objects to save.
#   if (is.list(x) && !is.null(x[["data"]])) {
#     return(x["data"])
#   } else {
#     x <- Filter(is.list, x)
#     return(lapply(x, clearEverythingButData))
#   }
# }
# 
# results <- vector("list", length(estimators))
# names(results) <- names(estimators)
# for (e in estimators) {
#   options$estimator <- e
#   set.seed(1)
#   results[[e]] <- jaspTools::run(options = options, data = "BFI Network.csv", view = FALSE)["results"]
#   results[[e]] <- clearEverythingButData(results[[e]])
# }
# saveRDS(results, file = file)
storedResults <- readRDS(file)

for (estimator in estimators) {
  
  options$estimator <- estimator
  set.seed(1)
  results <- jaspTools::run("NetworkAnalysis", "BFI Network.csv", options)

  test_that(paste0(estimator, ": Centrality measures per variable table results match"), {
    table    <- results                   [["results"]][["mainContainer"]][["collection"]][["mainContainer_centralityTable"]][["data"]]
    oldTable <- storedResults[[estimator]][["results"]][["mainContainer"]][["collection"]][["mainContainer_centralityTable"]][["data"]]
    expect_equal_tables(table, jaspTools:::collapseTable(oldTable))
  })
  
  test_that(paste0(estimator, ": Clustering measures per variable table results match"), {
    table    <- results                   [["results"]][["mainContainer"]][["collection"]][["mainContainer_clusteringTable"]][["data"]]
    oldTable <- storedResults[[estimator]][["results"]][["mainContainer"]][["collection"]][["mainContainer_clusteringTable"]][["data"]]
    expect_equal_tables(table, jaspTools:::collapseTable(oldTable))
  })
  
  test_that(paste0(estimator, ": Summary of Network table results match"), {
    table    <- results                   [["results"]][["mainContainer"]][["collection"]][["mainContainer_generalTable"]][["data"]]
    oldTable <- storedResults[[estimator]][["results"]][["mainContainer"]][["collection"]][["mainContainer_generalTable"]][["data"]]
    expect_equal_tables(table, jaspTools:::collapseTable(oldTable))
  })
  
  test_that(paste0(estimator, ": Weights matrix table results match"), {
    table    <- results                   [["results"]][["mainContainer"]][["collection"]][["mainContainer_weightsTable"]][["data"]]
    oldTable <- storedResults[[estimator]][["results"]][["mainContainer"]][["collection"]][["mainContainer_weightsTable"]][["data"]]
    expect_equal_tables(table, jaspTools:::collapseTable(oldTable))
  })
  
  test_that(paste0(estimator, ": Bootstrapped edge plot matches"), {
    if (estimator == "IsingSampler")
      skip("Cannot reliably test Bootstrapped edge plot for IsingSampler")
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_bootstrapContainer"]][["collection"]][["mainContainer_bootstrapContainer_EdgeStabilityPlots"]][["collection"]][["mainContainer_bootstrapContainer_EdgeStabilityPlots_Network"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, paste0(estimator, "-bootstrapped-edges"), dir="NetworkAnalysis")
  })
  
  test_that(paste0(estimator, ": Bootstrapped centrality plot matches"), {
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_bootstrapContainer"]][["collection"]][["mainContainer_bootstrapContainer_StatisticsCentralityPlots"]][["collection"]][["mainContainer_bootstrapContainer_StatisticsCentralityPlots_Network"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, paste0(estimator, "-bootstrapped-centrality"), dir="NetworkAnalysis")
  })
  
  test_that(paste0(estimator, ": Centrality Plot matches"), {
    skip("Cannot reliably test Centrality Plots")
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_centralityPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, paste0(estimator, "-centrality-plot"), dir="NetworkAnalysis")
  })
  
  test_that(paste0(estimator, ": Clustering Plot matches"), {
    skip("Cannot reliably test Clustering Plots")
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_clusteringPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, paste0(estimator, "-clustering-plot"), dir="NetworkAnalysis")
  })
  
  test_that(paste0(estimator, ": Network plot matches"), {
    if (estimator == "IsingSampler")
      skip("Cannot reliably test Network plot for IsingSampler")
    plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_networkPlotContainer"]][["collection"]][["mainContainer_plotContainer_networkPlotContainer_Network"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, paste0(estimator, "-network-plot"), dir="NetworkAnalysis")
  })
}
