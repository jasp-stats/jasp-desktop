context("Machine Learning Fuzzy C-Means Clustering")

options <- jaspTools::analysisOptions("mlClusteringFuzzyCMeans")
options$addClusters <- FALSE
options$clusterColumn <- ""
options$clusterEvaluationMetrics <- TRUE
options$modelOpt <- "validationOptimized"
options$plot2dCluster <- TRUE
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
                           "Hue", "Dilution", "Proline")
options$seedBox <- TRUE
options$tableClusterInfoBetweenSumSquares <- TRUE
options$tableClusterInfoCentroids <- TRUE
options$tableClusterInfoSilhouette <- TRUE
options$tableClusterInfoTotalSumSquares <- TRUE
options$withinssPlot <- TRUE
options$plotClusterMeans <- TRUE
options$showBars <- TRUE
options$oneFigure <- TRUE
set.seed(1)
results <- jaspTools::run("mlClusteringFuzzyCMeans", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	expect_equal_tables(table,
		list("Maximum diameter", 8.97000459794371, "Minimum separation", 1.36147055656936,
			 "Pearson's <unicode><unicode>", 0.596539894695789, "Dunn index",
			 0.151780363287826, "Entropy", 1.35544420311396, "Calinski-Harabasz index",
			 54.6693511555767))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	expect_equal_tables(table,
		list(0.839491921541895, 0.26947627616058, 0.413518093019243, 0.681741820639285,
			 1.07942113261275, -0.364934745388105, 0.224461290194488, -0.644852087484508,
			 0.461099060284824, 0.868380225923728, 0.910456569168096, -0.57233062599521,
			 0.595306684857612, 1, 0.267890291791698, 0.316127359005202,
			 57, 317.31879340078, -0.568876164456132, -0.560066319238215,
			 0.24032168538845, 0.157434473040581, -0.47775005380727, -0.243675075852539,
			 -0.264103805511917, 0.15533306892253, -0.332700708665599, -0.110539235903571,
			 -0.0218087376049095, 0.0303723803257744, -0.0469364415727733,
			 2, 0.241293367036458, 0.183509657229118, 43, 285.814463717693,
			 0.132001829894163, 0.815996929070708, -1.01600723441937, -1.11039103934574,
			 -0.350372275297844, 0.885409629513722, 0.150439518111281, 0.480756833547261,
			 -0.109695684301561, -0.903542238777212, -1.10117457564158, 0.706130710336304,
			 -0.696738933739068, 3, 0.265639220078202, 0.309705733697454,
			 50, 314.652375908725, -0.436904186315785, -0.478493424207421,
			 0.268851907708301, 0.223253730126168, -0.333263770138439, -0.230527517713692,
			 -0.202113194707297, 0.0580400694641861, -0.213968345987052,
			 0.0324887902093801, 0.0955354713291075, -0.096851831619987,
			 0.0439010230950389, 4, 0.225177121093643, 0.0125199125557127,
			 28, 266.724605393522))
})

test_that("All predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "all-predictors", dir="mlClusteringFuzzyCMeans")
})

test_that("Fuzzy C-Means Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	expect_equal_tables(table,
		list(0.23, 1288.51, 1453.96, 4, 0.387338190388083, 178))
})

test_that("Elbow Method Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "elbow-method-plot", dir="mlClusteringFuzzyCMeans")
})

test_that("t-SNE Cluster Plot matches", {
  skip("Does not reproduce on windows <-> osx")
	plotName <- results[["results"]][["plot2dCluster"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "t-sne-cluster-plot", dir="mlClusteringFuzzyCMeans")
})