context("Exploratory Factor Analysis")

# does not test
# - error handling
# - orthogonal rotation
# - Eigen values above / manual
# - contents of screeplot (set.seed does not work)

test_that("Main tables' results match", {
  options <- jasptools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contWide", "contcor1", "facFifty", "contExpon", "contBinom")
  options$factorMethod <- "parallelAnalysis"
  options$highlightText <- 0
  options$rotationMethod <- "oblique"
  options$obliqueSelector <- "oblimin"
  results <- jasptools::run("ExploratoryFactorAnalysis", "test.csv", options)

  table <- results[["results"]][["factorLoadings"]][["data"]]
  expect_equal_tables(table,
    list("contWide", -0.0259163777157326, 0.999328341366095, "contcor1",
         -0.302932576039401, 0.908231854374133, "facFifty", 0.320261312722698,
         0.897432691573134, "contExpon", 0.275659841743814, 0.924011651649776,
         "contBinom", 0.444974142157851, 0.801998012810884),
    label="Factor loadings table"
  )

  table <- results[["results"]][["goodnessOfFit"]][["data"]]
  expect_equal_tables(table, list("Model", 4.94262899224083, 5, 0.422921921501767),
    label="Chi squared table"
  )
})

test_that("Path diagram matches", {
  skip("base plots are not supported in regression testing")
  options <- jasptools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contWide", "contcor1", "facFifty", "contExpon")
  options$incl_pathDiagram <- TRUE
  results <- jasptools::run("ExploratoryFactorAnalysis", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "path-diagram", dir="ExploratoryFactorAnalysis")
})

test_that("Scree plot option creates .png", {
  options <- jasptools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contNormal", "contGamma")
  options$incl_screePlot <- TRUE
  results <- jasptools::run("ExploratoryFactorAnalysis", "test.csv", options)
  expect_match(results[["results"]][["screePlot"]][["data"]], ".*\\.png")
})

test_that("Factor correlation table matches", {
  options <- jasptools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contWide", "contcor1", "facFifty", "contExpon")
  options$incl_correlations <- TRUE
  results <- jasptools::run("ExploratoryFactorAnalysis", "test.csv", options)
  table <- results[["results"]][["factorCorrelations"]][["data"]]
  expect_equal_tables(table, list("RC 1", 1))
})

test_that("Missing values works", {
	options <- jasptools::analysisOptions("ExploratoryFactorAnalysis")
	options$variables <- list("contNormal", "contGamma", "contcor1", "debMiss30")
	options$incl_correlations <- TRUE

	options$missingValues <- "pairwise"
	results <- jasptools::run("ExploratoryFactorAnalysis", "test.csv", options)
	table <- results[["results"]][["goodnessOfFit"]][["data"]][[1]]
	expect_equal_tables(table, list("Model", 1.42781053334818, 2L, 0.489727939944839), label = "pairwise")

	options$missingValues <- "listwise"
	results <- jasptools::run("ExploratoryFactorAnalysis", "test.csv", options)
	table <- results[["results"]][["goodnessOfFit"]][["data"]][[1]]
	expect_equal_tables(table, list("Model", 0.491396758561133, 2L, 0.782158104440787), label = "listwise")
})
