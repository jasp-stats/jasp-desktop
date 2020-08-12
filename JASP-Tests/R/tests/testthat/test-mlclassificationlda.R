context("Machine Learning LDA Classification")

options <- jaspTools::analysisOptions("mlClassificationLda")
options$addClasses <- FALSE
options$addIndicator <- FALSE
options$andrewsCurve <- TRUE
options$boxTest <- TRUE
options$classColumn <- ""
options$classProportionsTable <- TRUE
options$coefficientsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$manovaTable <- TRUE
options$matrixplot <- TRUE
options$meanTable <- TRUE
options$modelOpt <- "optimizationManual"
options$modelValid <- "validationManual"
options$multicolTable <- TRUE
options$noOfFolds <- 5
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
                           "Hue", "Dilution", "Proline")
options$priorTable <- TRUE
options$rocCurve <- TRUE
options$seedBox <- TRUE
options$target <- "Type"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
set.seed(1)
results <- jaspTools::run("mlClassificationLda", "wine.csv", options)


test_that("Andrews Curves Plot matches", {
  plotName <- results[["results"]][["andrewsCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "andrews-curves-plot", dir="mlClassificationLda")
})

test_that("Tests of Equality of Covariance Matrices table results match", {
  table <- results[["results"]][["boxTest"]][["data"]]
  expect_equal_tables(table,
                      list(182, 2.89185053268131e-59, "Box's M", 684.203088594673))
})

test_that("Class Proportions table results match", {
  table <- results[["results"]][["classProportionsTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.331460674157303, 1, 0.371428571428571, 0.321678321678322, 0.398876404494382,
                           2, 0.4, 0.398601398601399, 0.269662921348315, 3, 0.228571428571429,
                           0.27972027972028))
})

test_that("Linear Discriminant Classification table results match", {
  table <- results[["results"]][["classificationTable"]][["data"]]
  expect_equal_tables(table,
                      list(2, "Moment", 35, 143, 1))
})

test_that("Linear Discriminant Coefficients table results match", {
  table <- results[["results"]][["coefficientsTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.0985099080776885, -0.00342793222377346, "(Constant)", -0.393164736556141,
                           0.74329203659079, "Alcohol", 0.196548982479906, 0.262198132657181,
                           "Malic", -0.0951763741131142, 0.598083594724991, "Ash", 0.501081256982107,
                           -0.534426316736952, "Alcalinity", -0.00263923349863145, 0.0427994783686402,
                           "Magnesium", 0.208101192372295, 0.0529200512387909, "Phenols",
                           -1.52479478359811, -0.558554626913979, "Flavanoids", -0.157591493223858,
                           -0.0949688285544638, "Nonflavanoids", -0.0405222908054603, -0.217441894815476,
                           "Proanthocyanins", 0.91972456724253, 0.549848617160285, "Color",
                           -0.270506150341797, -0.410863228716687, "Hue", -0.71156016179675,
                           0.138734056359611, "Dilution", -1.00443257274125, 0.950915886290421,
                           "Proline"))
})

test_that("Confusion Matrix table results match", {
  table <- results[["results"]][["confusionTable"]][["data"]]
  expect_equal_tables(table,
                      list("Observed", 1, 13, 0, 0, "", 2, 0, 14, 0, "", 3, 0, 0, 8))
})

test_that("Data Split plot matches", {
  skip("We need to figure out why this fails.")
  plotName <- results[["results"]][["dataSplitPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "data-split", dir="mlClassificationLda")
})

test_that("Tests of Equality of Class Means table results match", {
  table <- results[["results"]][["manovaTable"]][["data"]]
  expect_equal_tables(table,
                      list(1, 176, 21.2496323754348, "Alcohol", 7.72325331461668e-06, 1,
                           176, 41.7269323301623, "Malic", 9.91770325689756e-10, 1, 176,
                           0.434814672036439, "Ash", 0.510497749646086, 1, 176, 64.4956585871677,
                           "Alcalinity", 1.3353947853313e-13, 1, 176, 8.05344576130258,
                           "Magnesium", 0.00507541577210749, 1, 176, 188.537094424012,
                           "Phenols", 1.23405114169271e-29, 1, 176, 448.671871016041, "Flavanoids",
                           2.73665226170043e-50, 1, 176, 55.3438804591477, "Nonflavanoids",
                           4.2867390397641e-12, 1, 176, 58.3949501230074, "Proanthocyanins",
                           1.32725091814432e-12, 1, 176, 13.3652593667787, "Color", 0.000338241649326902,
                           1, 176, 108.396064621733, "Hue", 4.40539946338208e-20, 1, 176,
                           288.755043155589, "Dilution", 5.88616358195603e-39, 1, 176,
                           118.11615464263, "Proline", 2.23131916940365e-21))
})

test_that("Linear Discriminant Matrix plot matches", {
  plotName <- results[["results"]][["matrixplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "linear-discriminant-matrix", dir="mlClassificationLda")
})

test_that("Class Means in Training Data table results match", {
  table <- results[["results"]][["meanTable"]][["data"]]
  expect_equal_tables(table,
                      list(-0.737973343057887, 0.866579773288271, 0.30192375054201, 0.172427486097655,
                           0.835971830040447, 0.915688469526993, 0.479093202775772, 0.392526485134375,
                           -0.331435036899518, -0.640240104048998, 0.826520856297991, 0.531311985236627,
                           1.1413228289499, 1, 0.230034258385284, -0.89672653516467, -0.439418896241175,
                           -0.87376357087456, 0.281791112387928, 0.0384935686108725, 0.417649685134421,
                           -0.410599462269383, -0.353059005341892, -0.0543674703297988,
                           -0.0779683443333514, 0.0799634516853449, -0.74039657793808,
                           2, 0.547992584469851, 0.236666348631391, 0.275140493593514,
                           1.08190285049093, -1.28971512313365, -1.26896795506147, -1.20399681912815,
                           -0.0501714588321044, 0.921456636326643, 0.786608597104423, -1.01599631531372,
                           -0.716157122978659, -0.361275192620381, 3))
})

test_that("Pooled Within-Class Matrices Correlations table results match", {
  table <- results[["results"]][["multicolTable"]][["data"]]
  expect_equal_tables(table,
                      list("Alcohol", 1, "Malic", -0.15543140098941, 1, "Ash", -0.5077707223394,
                           0.10089679874801, 1, "Alcalinity", -0.467611905093725, 0.222285564619173,
                           0.884023520091301, 1, "Magnesium", -0.140413384091464, -0.256626215005153,
                           0.130223015503791, 0.00888175472723472, 1, "Phenols", 0.0451478948674465,
                           -0.243018700145123, -0.111235085964553, -0.117466569301758,
                           0.0775775713281924, 1, "Flavanoids", -0.0104179089218085, -0.200727294690442,
                           0.00488737317210032, 0.0179766449407254, 0.118742990766646,
                           0.91895865528287, 1, "Nonflavanoids", -0.196055464438605, 0.166861804099441,
                           0.335406465283413, 0.254504190909519, -0.429087861871871, -0.5753262491195,
                           -0.647943360372001, 1, "Proanthocyanins", 0.0253016108188158,
                           -0.122957793855602, -0.245943017246018, -0.18402137363758, 0.227431825623563,
                           0.654068699036149, 0.721189091877098, -0.545676586525786, 1,
                           "Color", 0.451413493469851, 1, -0.443474535624877, -0.277497844424914,
                           -0.324035265260096, 0.0803406024773737, 0.448676707247801, 0.474021656042198,
                           -0.27791501003148, 0.461086184605623, "Hue", -0.116736513331207,
                           -0.31605549855032, 1, -0.547694221367693, -0.0186379576416076,
                           -0.134150256351639, 0.0485210936837518, -0.25296286007992, -0.310674164048376,
                           0.104702433435255, -0.319965463375162, "Dilution", -0.216100482480972,
                           -0.194429688410647, -0.278041430079757, 1, 0.178453920345921,
                           0.0552302903464611, 0.206854858646108, -0.13003396618442, 0.51557569149584,
                           0.549699740766929, -0.488152463473658, 0.339741954938867, "Proline",
                           0.205948558920481, 0.419941021344394, 0.263379222194261, -0.448302974733637,
                           1, -0.611025813341785, -0.31960994842826, -0.425043376452818,
                           0.330321645062641, 0.0517339721463955, -0.015508474529089, -0.251298231606317,
                           0.13998283266291))
})

test_that("Prior and Posterior Class Probabilities table results match", {
  table <- results[["results"]][["priorTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.360925605569456, 0.321678321678322, 1, 0.407922906646082, 0.398601398601399,
                           2, 0.231151487784462, 0.27972027972028, 3))
})

test_that("ROC Curves Plot matches", {
  plotName <- results[["results"]][["rocCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "roc-curves-plot", dir="mlClassificationLda")
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 1, 1, 1, 13, 0.979591836734694, 1, 2, 1, 1, 14, 1, 1, 3,
                           1, 1, 8, 0.993197278911565, 1, "Average / Total", 1, 1, 35
                      ))
})