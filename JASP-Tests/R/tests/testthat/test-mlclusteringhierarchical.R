context("Machine Learning Hierarchical Clustering")

options <- jasptools::analysisOptions("mlClusteringHierarchical")
options$addClusters <- FALSE
options$clusterColumn <- ""
options$clusterEvaluationMetrics <- TRUE
options$dendrogram <- TRUE
options$modelOpt <- "validationOptimized"
options$plot2dCluster <- TRUE
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
                           "Hue", "Dilution", "Proline")
options$seedBox <- TRUE
options$tableClusterInfoBetweenSumSquares <- TRUE
options$tableClusterInfoSilhouette <- TRUE
options$tableClusterInfoTotalSumSquares <- TRUE
options$withinssPlot <- TRUE
set.seed(1)
results <- jasptools::run("mlClusteringHierarchical", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
  expect_equal_tables(table,
                      list("Maximum diameter", 7.00925271860537, "Minimum separation", 1.97770308313792,
                           "Pearson's <unicode><unicode>", 0.681974728064797, "Dunn index",
                           0.282156053224947, "Entropy", 1.39022827667793, "Calinski-Harabasz index",
                           29.9257432119148))
})

test_that("Cluster Information table results match", {
  table <- results[["results"]][["clusterInfoTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 0.362671818083091, 58, 275.316619967016, 2, 0.235220348745693,
                           3, 18.9196435833159, 3, 0, 1, 0, 4, 0.234762654151005, 52, 331.189475268968,
                           5, 0.18495415887279, 55, 365.280404143116, 6, 0.275974136527698,
                           4, 24.9877555644448, 7, 0.290821078307267, 3, 13.5285419737949,
                           8, 0.651863847872169, 2, 1.58232201421642))
})

test_that("Hierarchical Clustering table results match", {
  table <- results[["results"]][["clusteringTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.27, 1238.8, 1569.71, 8, 0.552018790736692, 178))
})

test_that("Dendogram plot matches", {
  plotName <- results[["results"]][["dendrogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "dendogram", dir="mlClusteringHierarchical")
})

test_that("Elbow Method Plot matches", {
  plotName <- results[["results"]][["optimPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "elbow-method-plot", dir="mlClusteringHierarchical")
})

test_that("t-SNE Cluster Plot matches", {
  skip("Does not reproduce on windows <-> osx")
  plotName <- results[["results"]][["plot2dCluster"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "t-sne-cluster-plot", dir="mlClusteringHierarchical")
})