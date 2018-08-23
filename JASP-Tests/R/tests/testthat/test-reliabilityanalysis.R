context("Reliability Analysis")

# does not test
# - missing values exclusion

test_that("Main table results match", {
  browser()
  options <- jasptools::analysisOptions("ReliabilityAnalysis")
  options$variables <- c("contcor1", "contcor2", "contNormal")
  options$reverseScaledItems <- "contcor2"
  options$alphaScale <- TRUE
  options$averageInterItemCor <- TRUE
  options$confAlpha <- TRUE
  options$glbScale <- TRUE
  options$gutmannScale <- TRUE
  options$mcDonaldScale <- TRUE
  options$meanScale <- TRUE
  options$sdScale <- TRUE
  results <- jasptools::run("ReliabilityAnalysis", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["reliabilityScaleTable"]][["data"]]
  expect_equal_tables(table,
    list(-0.757823, -1.452119, -0.2353888, "Scale", 0.6227002, -0.06776579, -0.02217061461, 0.144515070286093, -0.1759727, 
         0.667932535083157))
})

test_that("Item Statistics table matches", {
  options <- jasptools::analysisOptions("ReliabilityAnalysis")
  options$variables <- c("contcor1", "contcor2", "contNormal")
  options$alphaItem <- TRUE
  options$confAlpha <- TRUE
  options$gutmannItem <- TRUE
  options$itemRestCor <- TRUE
  options$mcDonaldItem <- TRUE
  options$meanItem <- TRUE
  options$sdItem <- TRUE
  results <- jasptools::run("ReliabilityAnalysis", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["reliabilityItemsObj"]][["reliabilityItems"]][["data"]]
  expect_equal_tables(table,
    list("contcor1", 0.0618194975467092, 0.0319398198963565, 0.061902485553013,
         0.560156128034403, 0.05254867287, 1.01183864387684, "contcor2",
         0.277152727398941, 0.161031927910319, 0.27739448681683, 0.442807451055322,
         0.06968807084, 1.0041493380131, "contNormal", 0.79299280264282,
         0.657010063712354, 0.793006727117146, 0.106272823965938, -0.18874858754,
         1.05841360919316)
  )
})
