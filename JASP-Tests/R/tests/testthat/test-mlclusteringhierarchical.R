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
options$plotClusterMeans <- TRUE
options$showBars <- TRUE
options$oneFigure <- TRUE
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
		list(1, 0.267089006549913, 0.362671818083091, 58, 275.316619967016,
			 2, 0.0183542454122518, 0.235220348745693, 3, 18.9196435833159,
			 3, 0, 0, 1, 0, 4, 0.321292146983255, 0.234762654151005, 52,
			 331.189475268968, 5, 0.354364296156273, 0.18495415887279, 55,
			 365.280404143116, 6, 0.0242410167988376, 0.275974136527698,
			 4, 24.9877555644448, 7, 0.0131242524925759, 0.290821078307267,
			 3, 13.5285419737949, 8, 0.00153503560689417, 0.651863847872169,
			 2, 1.58232201421642))
})

test_that("All predictors plot matches", {
	plotName <- results[["results"]][["clusterMeans"]][["collection"]][["clusterMeans_oneFigure"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	expect_equal_plots(testPlot, "all-predictors", dir="mlClusteringHierarchical")
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

# Ward.D linkage
options <- jasptools::analysisOptions("mlClusteringHierarchical")
options$addClusters <- FALSE
options$clusterColumn <- ""
options$clusterEvaluationMetrics <- TRUE
options$linkage <- "ward.D"
options$modelOpt <- "validationOptimized"
options$predictors <- c("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", "Proline")
options$seedBox <- TRUE
options$tableClusterMeans <- TRUE
set.seed(1)
results <- jasptools::run("mlClusteringHierarchical", "wine.csv", options)

test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	expect_equal_tables(table,
		list("Maximum diameter", 8.97000459794371, "Minimum separation", 1.65382813821881,
			 "Pearson's <unicode><unicode>", 0.576069991420004, "Dunn index",
			 0.184373165048091, "Entropy", 1.6642067641347, "Calinski-Harabasz index",
			 39.017310096389))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	expect_equal_tables(table,
		list(1, 0.242534984865065, 56, 261.487531765994, 2, 0.109109073675993,
			 9, 117.635245012931, 3, 0.230765129303098, 36, 248.797937801422,
			 4, 0.202904096764332, 34, 218.759745022484, 5, 0.123616307170801,
			 23, 133.27612536436, 6, 0.0910704082207109, 20, 98.1869740392464
			))
})

test_that("Cluster Means table results match", {
	table <- results[["results"]][["clusterMeansTable"]][["data"]]
	expect_equal_tables(table,
		list(-0.781843207924736, 0.92528108110726, 0.259388633050896, 0.215037009410226,
			 0.752415021725891, 0.953961965305775, 0.444752190789042, 0.431937572019575,
			 -0.307951562262038, -0.600313943067432, 0.86635481402651, 0.519144294355176,
			 1.19360329436145, "Cluster 1", 0.800021509606689, -0.195110344255247,
			 0.806509041705175, -0.553225269607803, 0.686211357071248, 0.70153100492565,
			 0.925047041258243, 1.90074197012781, -0.552711437686052, -0.291662537479379,
			 0.604330654214653, 1.05254674333779, 0.00810138286916384, "Cluster 2",
			 -0.146542572260378, -0.82982447794628, -0.989694853778986, -0.788552397373411,
			 0.507805196178362, 0.266590155973554, 0.268797435339725, -0.674284826744811,
			 -0.464440878157909, -0.617532099990733, 0.243488671519124, 0.204205628662902,
			 -0.777138085789446, "Cluster 3", 0.299192917546764, -0.120317329484489,
			 0.0287774127101431, 0.178313085214061, -1.190459435479, -1.24951928741746,
			 -0.834245202930723, -0.134293424521656, 0.816264749488125, 0.861878951732851,
			 -1.0157378437394, -1.11512524607075, -0.401366333769037, "Cluster 4",
			 0.712359274776484, -1.16078628111573, 0.144235344247606, -1.03779952171632,
			 0.0833591124064994, -0.209508778429251, 0.643821364725174, -0.712506031006811,
			 -0.12321813836773, 0.854994624710662, -0.512525358967146, -0.0904475233946064,
			 -0.767174266522769, "Cluster 5", 0.765086807112473, 0.530140371524544,
			 0.477441248000354, 1.95657926535688, -1.40168846358879, -1.1015268520218,
			 -1.46759041083875, 0.196634357272983, 0.701028886969743, 0.475246926074739,
			 -0.819863385236042, -0.295092620065656, -0.382313118173636,
			 "Cluster 6"))
})

test_that("Hierarchical Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	expect_equal_tables(table,
		list(0.19, 1234.14, 1482.32, 6, 0.53144565014931, 178))
})

# ward.D2 linkage
options <- jasptools::analysisOptions("mlClusteringHierarchical")
options$addClusters <- FALSE
options$clusterColumn <- ""
options$clusterEvaluationMetrics <- TRUE
options$linkage <- "ward.D"
options$modelOpt <- "validationOptimized"
options$predictors <- c("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", "Proline")
options$seedBox <- TRUE
options$tableClusterMeans <- TRUE
set.seed(1)
results <- jasptools::run("mlClusteringHierarchical", "wine.csv", options)

test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	expect_equal_tables(table,
		list("Maximum diameter", 8.97000459794371, "Minimum separation", 1.65382813821881,
			 "Pearson's <unicode><unicode>", 0.576069991420004, "Dunn index",
			 0.184373165048091, "Entropy", 1.6642067641347, "Calinski-Harabasz index",
			 39.017310096389))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	expect_equal_tables(table,
		list(1, 0.242534984865065, 56, 261.487531765994, 2, 0.109109073675993,
			 9, 117.635245012931, 3, 0.230765129303098, 36, 248.797937801422,
			 4, 0.202904096764332, 34, 218.759745022484, 5, 0.123616307170801,
			 23, 133.27612536436, 6, 0.0910704082207109, 20, 98.1869740392464
			))
})

test_that("Cluster Means table results match", {
	table <- results[["results"]][["clusterMeansTable"]][["data"]]
	expect_equal_tables(table,
		list(-0.781843207924736, 0.92528108110726, 0.259388633050896, 0.215037009410226,
			 0.752415021725891, 0.953961965305775, 0.444752190789042, 0.431937572019575,
			 -0.307951562262038, -0.600313943067432, 0.86635481402651, 0.519144294355176,
			 1.19360329436145, "Cluster 1", 0.800021509606689, -0.195110344255247,
			 0.806509041705175, -0.553225269607803, 0.686211357071248, 0.70153100492565,
			 0.925047041258243, 1.90074197012781, -0.552711437686052, -0.291662537479379,
			 0.604330654214653, 1.05254674333779, 0.00810138286916384, "Cluster 2",
			 -0.146542572260378, -0.82982447794628, -0.989694853778986, -0.788552397373411,
			 0.507805196178362, 0.266590155973554, 0.268797435339725, -0.674284826744811,
			 -0.464440878157909, -0.617532099990733, 0.243488671519124, 0.204205628662902,
			 -0.777138085789446, "Cluster 3", 0.299192917546764, -0.120317329484489,
			 0.0287774127101431, 0.178313085214061, -1.190459435479, -1.24951928741746,
			 -0.834245202930723, -0.134293424521656, 0.816264749488125, 0.861878951732851,
			 -1.0157378437394, -1.11512524607075, -0.401366333769037, "Cluster 4",
			 0.712359274776484, -1.16078628111573, 0.144235344247606, -1.03779952171632,
			 0.0833591124064994, -0.209508778429251, 0.643821364725174, -0.712506031006811,
			 -0.12321813836773, 0.854994624710662, -0.512525358967146, -0.0904475233946064,
			 -0.767174266522769, "Cluster 5", 0.765086807112473, 0.530140371524544,
			 0.477441248000354, 1.95657926535688, -1.40168846358879, -1.1015268520218,
			 -1.46759041083875, 0.196634357272983, 0.701028886969743, 0.475246926074739,
			 -0.819863385236042, -0.295092620065656, -0.382313118173636,
			 "Cluster 6"))
})

test_that("Hierarchical Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	expect_equal_tables(table,
		list(0.19, 1234.14, 1482.32, 6, 0.53144565014931, 178))
})

# Median linkage
options <- jasptools::analysisOptions("mlClusteringHierarchical")
options$addClusters <- FALSE
options$clusterColumn <- ""
options$clusterEvaluationMetrics <- TRUE
options$linkage <- "median"
options$modelOpt <- "validationOptimized"
options$predictors <- c("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", "Proline")
options$seedBox <- TRUE
options$tableClusterMeans <- TRUE
set.seed(1)
results <- jasptools::run("mlClusteringHierarchical", "wine.csv", options)

test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	expect_equal_tables(table,
		list("Maximum diameter", 9.12230549592459, "Minimum separation", 2.21781034559338,
			 "Pearson's <unicode><unicode>", 0.135839544447697, "Dunn index",
			 0.243119499405517, "Entropy", 0.0855285935357971, "Calinski-Harabasz index",
			 3.89216207251532))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	expect_equal_tables(table,
		list(1, 0.988936239883131, 175, 2226.30846207446, 2, 0.0110637601168691,
			 3, 24.9069068127773))
})

test_that("Cluster Means table results match", {
	table <- results[["results"]][["clusterMeansTable"]][["data"]]
	expect_equal_tables(table,
		list(0.0203359861847774, -0.000127726563435059, 0.0318588656427388,
			 0.0121377108983087, 0.0206445936319528, 0.0224702684469892,
			 0.0135586997230953, 0.0204945225572549, -0.012281064408198,
			 -0.00112208213657474, 0.020957401769073, 0.0283807621366269,
			 0.016942478148088, "Cluster 1", -1.18626586077868, 0.00745071620032743,
			 -1.85843382915971, -0.70803313573467, -1.20426796186389, -1.3107656594077,
			 -0.79092415051388, -1.19551381583987, 0.716395423811544, 0.0654547913001873,
			 -1.22251510319592, -1.65554445796991, -0.98831122530514, "Cluster 2"
			))
})

test_that("Hierarchical Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	expect_equal_tables(table,
		list(0.16, 2303.22, 2385.94, 2, 0.0216360847947693, 178))
})

# McQuitty linkage
options <- jasptools::analysisOptions("mlClusteringHierarchical")
options$addClusters <- FALSE
options$clusterColumn <- ""
options$clusterEvaluationMetrics <- TRUE
options$linkage <- "mcquitty"
options$modelOpt <- "validationOptimized"
options$predictors <- c("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", "Proline")
options$seedBox <- TRUE
options$tableClusterMeans <- TRUE
set.seed(1)
results <- jasptools::run("mlClusteringHierarchical", "wine.csv", options)

test_that("Evaluation Metrics table results match", {
	table <- results[["results"]][["clusterEvaluationMetrics"]][["data"]]
	expect_equal_tables(table,
		list("Maximum diameter", 7.58155451648733, "Minimum separation", 1.96727498646141,
			 "Pearson's <unicode><unicode>", 0.57530315948636, "Dunn index",
			 0.259481743774744, "Entropy", 1.38765867427383, "Calinski-Harabasz index",
			 21.5743837216312))
})

test_that("Cluster Information table results match", {
	table <- results[["results"]][["clusterInfoTable"]][["data"]]
	expect_equal_tables(table,
		list(1, 0.582805907580771, 91, 710.160425088166, 2, 0.0155267453101322,
			 3, 18.9196435833159, 3, 0, 1, 0, 4, 0.0889424766631771, 17,
			 108.378151652079, 5, 0.104697120938497, 21, 127.575494592744,
			 6, 0.00751470105220235, 3, 9.15681056802415, 7, 0.189410607507031,
			 39, 230.800538899409, 8, 0.0111024409481889, 3, 13.5285419737949
			))
})

test_that("Cluster Means table results match", {
	table <- results[["results"]][["clusterMeansTable"]][["data"]]
	expect_equal_tables(table,
		list(-0.503914889926213, 0.282685896356031, -0.140716635849476, -0.164417516291793,
			 0.687311992727421, 0.694049939200721, 0.410677692020196, 0.0257880273964818,
			 -0.416068994074122, -0.567643594033477, 0.62147946422719, 0.344212738467797,
			 0.455906232026601, "Cluster 1", 2.49684991646857, -0.575596660516394,
			 2.36983419087516, -0.322690955034978, 1.1995203112192, 1.52246795256413,
			 0.725741605386693, 1.9318601187305, -0.369705432579816, 0.172589989934057,
			 1.18487347508274, 0.569167023534477, 0.0416209288240359, "Cluster 2",
			 -2.6635047091055, -0.776789065158081, -3.66881295268039, -1.34068447718192,
			 -1.11506488141745, -1.4609370522684, 0.404908464715418, -0.82209603260759,
			 -1.24992453322666, -0.657707799478434, -0.503494178267563, -2.04574254504005,
			 -0.720507694992004, "Cluster 3", -0.0918408952456267, -0.357255803618334,
			 -0.285340875796504, -0.215867606873497, -1.16560408798618, -1.05989109721689,
			 -0.695017345361344, 0.244615796699337, 0.287606737295045, 0.0512751326574692,
			 -1.20371717051806, -1.22560714517533, -0.385768848453194, "Cluster 4",
			 0.811428759400718, -1.0354650139831, 0.335544322451154, -0.967872079543363,
			 0.0982311526247688, -0.072209219242327, 0.859074858652677, -0.788755159104707,
			 -0.217105704719492, 0.823053695925409, -0.365016074207286, 0.0516761703199189,
			 -0.80050095729333, "Cluster 5", -0.787012117987655, -0.81784873957475,
			 -1.45747737898399, -0.909331289831523, 0.0821343561532301, -0.229531630810677,
			 0.915324824874264, 3.49554708601568, -0.987350699813364, -0.738059198453836,
			 -0.33838566958031, 2.19402174431909, 0.173934926014321, "Cluster 6",
			 0.688691139020728, 0.26359991583048, 0.366921200836534, 1.20407811036236,
			 -1.28046978362179, -1.2363217210665, -1.18243032566869, -0.0950085219870384,
			 0.941786598996733, 0.988465733376604, -0.853786423745483, -0.623826668621419,
			 -0.387890662055196, "Cluster 7", 0.35086104896678, -1.07652468839977,
			 -0.922868778749699, -0.98841270751243, 0.805148797666505, 0.724890999370647,
			 -0.93675739627355, -0.565371306635395, 2.04119976512408, -0.898761996404641,
			 0.94519983343995, 2.1707263361358, -0.972433545642306, "Cluster 8"
			))
})

test_that("Hierarchical Clustering table results match", {
	table <- results[["results"]][["clusteringTable"]][["data"]]
	expect_equal_tables(table,
		list(0.16, 1426.52, 1757.43, 8, 0.470439110666, 178))
})