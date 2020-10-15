context("Machine Learning Random Forest Clustering")

options <- jaspTools::analysisOptions("mlClusteringRandomForest")
options$addClusters <- FALSE
options$clusterColumn <- ""
options$clusterEvaluationMetrics <- TRUE
options$importanceTable <- TRUE
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
options$plotClusterMeans <- TRUE
options$showBars <- TRUE
options$oneFigure <- TRUE
set.seed(1)
results <- jaspTools::run("mlClusteringRandomForest", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	expect_equal_tables(table,
		list("Maximum diameter", 11.1799587393255, "Minimum separation", 1.91296244175494,
			 "Pearson's <unicode><unicode>", 0.556526809045383, "Dunn index",
			 0.171106395502703, "Entropy", 1.07850807265, "Calinski-Harabasz index",
			 64.0845132710531))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	expect_equal_tables(table,
		list(1, 0.201146634109287, 0.409774198440774, 57, 267.166873258102,
			 2, 0.605247626150293, 0.0820430005029718, 75, 803.901673729259,
			 3, 0.19360573974042, 0.394537832399485, 46, 257.15091062954
			))
})

test_that("All predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "all-predictors", dir="mlClusteringRandomForest")
})

test_that("Random Forest Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	expect_equal_tables(table,
		list(0.27, 1406.22, 1530.31, 3, 0.422764251361625, 178))
})

test_that("Variable Importance table results match", {
	table <- results[["results"]][["importanceTable"]][["data"]]
	expect_equal_tables(table,
		list(20.9122039237879, "Flavanoids", 16.7564424183852, "Phenols", 16.1286527609624,
			 "Dilution", 14.8508385264296, "Proline", 14.5421300011253, "Color",
			 13.6772809549555, "Hue", 13.4601315432519, "Alcohol", 13.4314873093631,
			 "Proanthocyanins", 11.6570120327778, "Malic", 11.0596207822425,
			 "Alcalinity", 10.9112870469843, "Nonflavanoids", 10.058673972256,
			 "Magnesium", 10.0524690645575, "Ash"))
})

test_that("Elbow Method Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "elbow-method-plot", dir="mlClusteringRandomForest")
})

test_that("t-SNE Cluster Plot matches", {
  skip("Does not reproduce on windows <-> osx")
	plotName <- results[["results"]][["plot2dCluster"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "t-sne-cluster-plot", dir="mlClusteringRandomForest")
})