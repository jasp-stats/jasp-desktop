#context("Reliability Analysis")
#
## does not test
## - missing values exclusion
#
#test_that("Main table results match", {
#  options <- jasptools::analysisOptions("ReliabilityAnalysis")
#  options$variables <- c("contcor1", "contcor2", "contNormal")
#  options$reverseScaledItems <- "contcor2"
#  options$alphaScale <- TRUE
#  options$averageInterItemCor <- TRUE
#  options$confAlpha <- TRUE
#  options$glbScale <- TRUE
#  options$gutmannScale <- TRUE
#  options$mcDonaldScale <- TRUE
#  options$meanScale <- TRUE
#  options$sdScale <- TRUE
#  results <- jasptools::run("ReliabilityAnalysis", "test.csv", options)
#  table <- results[["results"]][["scaleTable"]][["data"]]
#  expect_equal_tables(table,
#    list("scale", -0.757822989578577, -0.0677657928415725, 0.197235966525379,
#         0.622700230679449, -0.175972651899464, -0.02217061461, 0.144515070286093,
#         -1.45211881901153, -0.235388804018903)
#  )
#})
#
#test_that("Item Statistics table matches", {
#  options <- jasptools::analysisOptions("ReliabilityAnalysis")
#  options$variables <- c("contcor1", "contcor2", "contNormal")
#  options$alphaItem <- TRUE
#  options$confAlpha <- TRUE
#  options$gutmannItem <- TRUE
#  options$itemRestCor <- TRUE
#  options$mcDonaldItem <- TRUE
#  options$meanItem <- TRUE
#  options$sdItem <- TRUE
#  results <- jasptools::run("ReliabilityAnalysis", "test.csv", options)
#  table <- results[["results"]][["itemTable"]][["data"]]
#  expect_equal_tables(table,
#    list("contcor1", 0.0618194975467092, 0.0319398198963565, 0.061902485553013,
#         0.560156128034403, 0.05254867287, 1.01183864387684, "contcor2",
#         0.277152727398941, 0.161031927910319, 0.27739448681683, 0.442807451055322,
#         0.06968807084, 1.0041493380131, "contNormal", 0.79299280264282,
#         0.657010063712354, 0.793006727117146, 0.106272823965938, -0.18874858754,
#         1.05841360919316)
#  )
#})
#
#test_that("Reverse scaled items match", {
#  options <- jasptools::analysisOptions("ReliabilityAnalysis")
#  options$variables <- c("Q01", "Q03", "Q04", "Q05", "Q12", "Q16", "Q20", "Q21")
#  options$reverseScaledItems <- "Q03"
#  options$alphaScale <- TRUE
#  options$averageInterItemCor <- TRUE
#  options$confAlpha <- TRUE
#  options$glbScale <- TRUE
#  options$gutmannScale <- TRUE
#  options$mcDonaldScale <- TRUE
#  options$meanScale <- TRUE
#  options$sdScale <- TRUE
#  
#  results <- jasptools::run("ReliabilityAnalysis", "Fear of Statistics.csv", options)
#  table <- results[["results"]][["scaleTable"]][["data"]]
#  expect_equal_tables(table,
#    list("scale", 0.820836210468446, 0.813045844410605, 0.824827780086911,
#         0.708529526625945, 0.368244389782582, 3.08727148969273, 0.393585959365817,
#         0.81017314606981, 0.831119838017159)
#  )
#})
#
#test_that("Analysis handles errors - Infinity", {
#  options <- jasptools::analysisOptions("ReliabilityAnalysis")
#  options$variables <- c("debInf")
#  results <- jasptools::run("ReliabilityAnalysis", "test.csv", options)
#  status <- results[["status"]]
#  expect_identical(status, "validationError")
#})