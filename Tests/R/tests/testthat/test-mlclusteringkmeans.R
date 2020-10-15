context("Machine Learning K-Means Clustering")

options <- jaspTools::analysisOptions("mlClusteringKMeans")
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
results <- jaspTools::run("mlClusteringKMeans", "wine.csv", options)


test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	expect_equal_tables(table,
		list("Maximum diameter", 8.97000459794371, "Minimum separation", 1.60349035617523,
			 "Pearson's <unicode><unicode>", 0.622353795934006, "Dunn index",
			 0.178761375054681, "Entropy", 1.47556540322037, "Calinski-Harabasz index",
			 47.2866877681223))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	expect_equal_tables(table,
		list(-0.730833814176345, -0.74707612935895, 0.17606244829255, 0.639563322424828,
			 -0.6995003034381, 0.153720814884363, -0.256477289296812, 0.0855876487085092,
			 -0.528568111653367, 0.714539559517063, 0.624134979374367, -0.602079907880078,
			 0.646893048915268, 1, 0.160931654126512, 0.0984645246472935,
			 26, 176.896648011844, -0.921299089995289, -0.916064607877969,
			 0.536798629417091, 0.0350708567135793, -0.769844680090664, -0.558048304218843,
			 -0.629189664097712, 0.18336923491786, -0.690602880280368, -0.526097546097289,
			 -0.309134678083177, 0.41625724048548, -0.464353646841332, 2,
			 0.247000239101422, 0.142777266079956, 41, 271.503543490686,
			 0.981705546386994, 0.255561956674198, 0.488357488677896, 0.772806160311654,
			 1.26108553315067, -0.394404612616799, 0.265825343740946, -0.82305585865629,
			 0.455044649627823, 0.908509053731672, 0.987969032048833, -0.614556122250903,
			 0.559460603458105, 3, 0.22127006271421, 0.34422711905128, 54,
			 243.220841865851, -0.669007419814319, -0.639735547737523, 0.656470813650849,
			 0.474745282591851, 0.0277279591190561, -0.504722091238357, 0.741707999252534,
			 0.772572814789805, 2.49490412001043, 0.279439717765542, 0.437479639145589,
			 -0.406609677680301, 1.07948205904972, 4, 0.0951516921201966,
			 0.00679859389123221, 8, 104.591079238431, 0.186018402285889,
			 0.985717694037202, -1.18794772162286, -1.29787849848707, -0.378975565101653,
			 0.902425818310064, 0.248509248850859, 0.582061558185055, -0.0504929601123107,
			 -0.985776240838559, -1.23271739808418, 0.714825280978949, -0.747498955315058,
			 5, 0.27564635193766, 0.320348147018739, 49, 302.991452856956
			))
})

test_that("All predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "all-predictors", dir="mlClusteringKMeans")
})

test_that("K-Means Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	expect_equal_tables(table,
		list(0.24, 1229.2, 1436.02, 5, 0.522293104970114, 178))
})

test_that("Elbow Method Plot matches", {
	plotName <- results[["results"]][["optimPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "elbow-method-plot", dir="mlClusteringKMeans")
})

test_that("t-SNE Cluster Plot matches", {
  skip("Does not reproduce on windows <-> osx")
	plotName <- results[["results"]][["plot2dCluster"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "t-sne-cluster-plot", dir="mlClusteringKMeans")
})