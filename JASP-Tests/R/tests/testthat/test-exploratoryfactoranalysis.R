context("Exploratory Factor Analysis")

# does not test
# - error handling
# - orthogonal rotation
# - Eigen values above / manual
# - contents of screeplot (set.seed does not work)

test_that("Main tables' results match", {
  options <- JASPTools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contWide", "contcor1", "facFifty", "contExpon", "contBinom")
  options$factorMethod <- "parallelAnalysis"
  options$highlightText <- 0
  options$rotationMethod <- "oblique"
  options$obliqueSelector <- "oblimin"
  results <- JASPTools::run("ExploratoryFactorAnalysis", "debug.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")

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
  options <- JASPTools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contWide", "contcor1", "facFifty", "contExpon")
  options$incl_pathDiagram <- TRUE
  results <- JASPTools::run("ExploratoryFactorAnalysis", "debug.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "path-diagram", dir="ExploratoryFactorAnalysis")
})

test_that("Scree plot option creates .png", {
  options <- JASPTools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contNormal", "contGamma")
  options$incl_screePlot <- TRUE
  results <- JASPTools::run("ExploratoryFactorAnalysis", "debug.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")
  expect_match(results[["results"]][["screePlot"]][["data"]], ".*\\.png")
})

test_that("Factor correlation table matches", {
  options <- JASPTools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contWide", "contcor1", "facFifty", "contExpon")
  options$incl_correlations <- TRUE
  results <- JASPTools::run("ExploratoryFactorAnalysis", "debug.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")
  table <- results[["results"]][["factorCorrelations"]][["data"]]
  expect_equal_tables(table, list("RC 1", 1, "RC 2", 0.0433504307183835, 1))
})
