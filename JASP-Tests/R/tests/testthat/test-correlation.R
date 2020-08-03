context("Correlation")

# test general results ----
options <- jasptools::analysisOptions("Correlation")
options$VovkSellkeMPR <- TRUE
options$confidenceIntervals <- TRUE
options$flagSignificant <- TRUE
options$kendallsTauB <- TRUE
options$multivariateShapiro <- TRUE
options$pairwiseShapiro <- TRUE
options$plotCorrelationMatrix <- TRUE
options$plotDensities <- TRUE
options$plotHeatmap <- TRUE
options$plotStatistics <- TRUE
options$sampleSize <- TRUE
options$spearman <- TRUE
options$variables <- list("contNormal", "contGamma", "contcor1", "debMiss30")
set.seed(1)
results <- jasptools::run("Correlation", "debug.csv", options)


test_that("Shapiro-Wilk Test for Multivariate Normality table results match", {
  table <- results[["results"]][["assumptionsContainer"]][["collection"]][["assumptionsContainer_multivariateShapiro"]][["data"]]
  expect_equal_tables(table,
                      list(0.878989128563944, 6.30818027438384e-06))
})

test_that("Shapiro-Wilk Test for Bivariate Normality table results match", {
  table <- results[["results"]][["assumptionsContainer"]][["collection"]][["assumptionsContainer_pairwiseShapiro"]][["data"]]
  expect_equal_tables(table,
                      list(0.901474720121716, 1.65965808553626e-06, "-", "contNormal", "contGamma",
                           0.966220519133756, 0.0114432512312019, "-", "contNormal", "contcor1",
                           0.966282842281815, 0.0561697740909883, "-", "contNormal", "debMiss30",
                           0.873628887299087, 9.84839266547295e-08, "-", "contGamma", "contcor1",
                           0.862918050703853, 1.7604538483754e-06, "-", "contGamma", "debMiss30",
                           0.990143883746405, 0.861444767503725, "-", "contcor1", "debMiss30"
                      ))
})

test_that("Correlation plot matches", {
  plotName <- results[["results"]][["corrPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "correlation-plot", dir="Correlation")
})

test_that("Kendall's tau B heatmap matches", {
  plotName <- results[["results"]][["heatmaps"]][["collection"]][["heatmaps_kendall"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "kendall-s-tau-b", dir="Correlation")
})

test_that("Pearson's r heatmap matches", {
  plotName <- results[["results"]][["heatmaps"]][["collection"]][["heatmaps_pearson"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "pearson-s-r", dir="Correlation")
})

test_that("Spearman's rho heatmap matches", {
  plotName <- results[["results"]][["heatmaps"]][["collection"]][["heatmaps_spearman"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "spearman-s-rho", dir="Correlation")
})

test_that("Correlation Table results match", {
  table <- results[["results"]][["mainTable"]][["data"]]
  expect_equal_tables(table,
                      list("<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", -0.0266666666666667, -0.15041383947394, 0.694237192757787,
                           0.097080506140607, 1, -0.0592003859505642, -0.252680329590477,
                           0.558497687623534, 0.138832075039338, 1, 100, -0.0341794179417942,
                           -0.229059752837501, 0.73526094223706, 0.163335243866025, 1,
                           0.0957575757575758, -0.0373731390706183, 0.15805971278439, 0.22888829058577,
                           1.26165085338952, 0.161031927910319, -0.0365419981231701, 0.109479317429059,
                           0.346490687832583, 1.51909334263147, 100, 0.142838283828383,
                           -0.0551264633902869, 0.156055917429528, 0.329997969616898, 1.26907384634445,
                           -0.142028985507246, -0.302753498566225, 0.0820536231540238,
                           0.0186955275517326, 1.7930869050848, -0.163779936728643, -0.383976976749411,
                           0.175488795918533, 0.0740435803355283, 1.20465290217953, 70,
                           -0.20524888461202, -0.419968595404043, 0.0883143492445961, 0.0312313683562874,
                           1.71644871351761, "contNormal", "", "", "", "", "", "", "",
                           "", "", "", "", "", "", "", "", "", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", -0.128484848484849,
                           -0.265695191109086, 0.0582140897855729, 0.00872549413938944,
                           2.22231002833737, -0.156555303374674, -0.342443190888167, 0.119832226549265,
                           0.041127497099264, 1.44695679291394, 100, -0.183750375037504,
                           -0.3669254548718, 0.0673279518522942, 0.0131420647686214, 2.02506621791795,
                           0.149482401656315, -0.0220394444690113, 0.0672280148907629,
                           0.321004247781641, 2.02696064848969, 0.171798366528544, -0.0658332206699671,
                           0.155001605969274, 0.39098888887008, 1.27306010334954, 70, 0.211162627941562,
                           -0.0250545433406204, 0.0793767652827101, 0.425046791840888,
                           1.82929064467251, "contGamma", "", "", "", "", "", "", "", "",
                           "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                           "", "", "", "", "", "", "", "", "", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", -0.0583850931677019,
                           -0.226151894979294, 0.474719152682399, 0.109381708643891, 1,
                           -0.0904225863977578, -0.318626758463425, 0.456613508199801,
                           0.147689385556226, 1, 70, -0.10261569416499, -0.329641395147143,
                           0.3970672317383, 0.135628607517475, 1, "contcor1", "", "", "",
                           "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                           "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                           "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", "<unicode>", "<unicode>", "<unicode>", "<unicode>",
                           "<unicode>", "debMiss30"))
})

test_that("Correlation Table hypothesis correlated positively match", {
  options <- jasptools::analysisOptions("Correlation")
  options$confidenceIntervals <- TRUE
  options$displayPairwise <- TRUE
  options$hypothesis <- "correlatedPositively"
  options$kendallsTauB <- TRUE
  options$spearman <- TRUE
  options$variables <- list("contNormal", "contGamma", "contExpon")
  set.seed(1)
  results <- jasptools::run("Correlation", "debug.csv", options)
  table <- results[["results"]][["mainTable"]][["data"]]
  expect_equal_tables(table,
                      list(-0.0266666666666667, -0.130518567835555, 0.652881403621106, 1,
                           -0.0592003859505642, -0.22249475793732, 0.720751156188233, 1,
                           "-", -0.0341794179417942, -0.198530526102106, 0.632414289086752,
                           1, "contNormal", "contGamma", 0.0824242424242424, -0.0362831074978719,
                           0.112169382715083, 1, 0.0628084903725342, -0.10374369323152,
                           0.267365049565566, 1, "-", 0.117203720372037, -0.0492249134641657,
                           0.122536070879884, 1, "contNormal", "contExpon", 0.0747474747474747,
                           -0.0315081778525471, 0.135251876148973, 1, 0.062688895089753,
                           -0.103862467527346, 0.267754890741034, 1, "-", 0.125616561656166,
                           -0.0407034694516623, 0.106321760175355, 1, "contGamma", "contExpon"
                      ))
})

test_that("Correlation Table hypothesis correlated negatively match", {
  options <- jasptools::analysisOptions("Correlation")
  options$confidenceIntervals <- TRUE
  options$displayPairwise <- TRUE
  options$hypothesis <- "correlatedNegatively"
  options$kendallsTauB <- TRUE
  options$spearman <- TRUE
  options$variables <- list("contNormal", "contGamma", "contExpon")
  set.seed(1)
  results <- jasptools::run("Correlation", "debug.csv", options)
  table <- results[["results"]][["mainTable"]][["data"]]
  expect_equal_tables(table,
                      list(-0.0266666666666667, -1, 0.347118596378894, 0.0771852345022219,
                           -0.0592003859505642, -1, 0.279248843811767, 0.107324940091343,
                           "-", -0.0341794179417942, -1, 0.36763047111853, 0.132041339755167,
                           "contNormal", "contGamma", 0.0824242424242424, -1, 0.887830617284917,
                           0.201131592346357, 0.0628084903725342, -1, 0.732634950434434,
                           0.225934274977044, "-", 0.117203720372037, -1, 0.877488217988468,
                           0.277299682807625, "contNormal", "contExpon", 0.0747474747474747,
                           -1, 0.864748123851027, 0.181003127347497, 0.062688895089753,
                           -1, 0.732245109258966, 0.225820332876708, "-", 0.125616561656166,
                           -1, 0.893700219352055, 0.285163043562837, "contGamma", "contExpon"
                      ))
})

# test error handling
test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("Correlation")
  options$displayPairwise <- TRUE
  options$variables <- list("contNormal", "debMiss99", "debSame")
  set.seed(1)
  results <- jasptools::run("Correlation", "debug.csv", options)
  table <- results[["results"]][["mainTable"]][["data"]]
  expect_equal_tables(table,
                      list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "NaN", "NaN", "-",
                           "contNormal", "debMiss99", 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                           1, 1, 1, 1, "NaN", "NaN", "-", "contNormal", "debSame", 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "NaN", "NaN", "-", "debMiss99",
                           "debSame"))
})


test_that("Pearson's partial correlation correct", {
  # Validated against Field, A. Discovering Statistics (5th edition). Chapter 8.5
  options <- jasptools::analysisOptions("Correlation")
  options$displayPairwise <- TRUE
  options$variables <- list("Exam", "Anxiety")
  options$conditioningVariables <- list("Revise")
  
  results <- jasptools::run("Correlation", "Exam Anxiety.csv", options)
  table <- results[["results"]][["mainTable"]][["data"]]
  expect_equal_tables(table,
                      list(-0.246665820246124, 0.0124458135120866, "-", "Exam", "Anxiety"
                      ))
})