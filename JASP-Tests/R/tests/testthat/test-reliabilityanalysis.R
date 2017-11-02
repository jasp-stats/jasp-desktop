context("Reliability Analysis")

# does not test
# - missing values exclusion

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("ReliabilityAnalysis")
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
  results <- JASPTools::run("ReliabilityAnalysis", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["reliabilityScale"]][["data"]]
  expect_equal_tables(table,
    list("scale", -0.757822989578577, -0.0677657928415725, 0.667932535083157,
         0.622700230679449, -0.175972651899464, -0.02217061461, 0.144515070286093,
         -1.45211881901153, -0.235388804018903)
  )
})

test_that("Item Statistics table matches", {
  options <- JASPTools::analysisOptions("ReliabilityAnalysis")
  options$variables <- c("contcor1", "contcor2", "contNormal")
  options$alphaItem <- TRUE
  options$confAlpha <- TRUE
  options$gutmannItem <- TRUE
  options$itemRestCor <- TRUE
  options$mcDonaldItem <- TRUE
  options$meanItem <- TRUE
  options$sdItem <- TRUE
  results <- JASPTools::run("ReliabilityAnalysis", "debug.csv", options, view=FALSE, quiet=TRUE)
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
