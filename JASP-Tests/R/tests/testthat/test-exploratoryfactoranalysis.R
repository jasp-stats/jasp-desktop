context("Exploratory Factor Analysis")

# does not test
# - error handling
# - orthogonal rotation
# - Eigen values above / manual
# - contents of screeplot (set.seed does not work)

options <- jasptools::analysisOptions("ExploratoryFactorAnalysis")
options$factorMethod <- "manual"
options$fitmethod <- "minres"
options$highlightText <- 0.4
options$incl_correlations <- TRUE
options$incl_fitIndices <- TRUE
options$incl_pathDiagram <- TRUE
options$incl_screePlot <- TRUE
options$incl_structure <- TRUE
options$numberOfFactors <- 2
options$obliqueSelector <- "geominQ"
options$rotationMethod <- "oblique"
options$variables <- list("contWide", "contcor1", "contcor2", "facFifty", "contExpon", 
                          "debCollin1", "debEqual1")
set.seed(1)
results <- jasptools::run("ExploratoryFactorAnalysis", "debug.csv", options)


test_that("Factor Correlations table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cortab"]][["data"]]
  expect_equal_tables(table,
                      list(1, -0.0406671, "Factor 1", -0.0406671, 1, "Factor 2"))
})

test_that("Factor Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigtab"]][["data"]]
  expect_equal_tables(table,
                      list("Factor 1", 0.252207709974464, 1.76545396982125, 0.252207709974464,
                           "Factor 2", 0.439372431717106, 1.31015305219849, 0.187164721742642
                      ))
})

test_that("Chi-squared Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  expect_equal_tables(table,
                      list(4.05152653321549, 8, "Model", 0.85244487039262))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loatab"]][["data"]]
  expect_equal_tables(table,
                      list("", "", 0.951432334368898, "contWide", 0.654092561077089, "",
                           0.57413710933047, "contcor1", 1.00020594814694, "", -0.00255707470903843,
                           "contcor2", "", "", 0.953108905280117, "facFifty", "", 0.997800455330077,
                           0.00428892032601724, "contExpon", "", "", 0.998135387367175,
                           "debCollin1", "", "", 0.958751715702742, "debEqual1"))
})

test_that("Path Diagram plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_path"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "path-diagram", dir = "ExploratoryFactorAnalysis")
})

test_that("Scree plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_scree"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "scree-plot", dir = "ExploratoryFactorAnalysis")
})

test_that("Factor Loadings (Structure Matrix) table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_strtab"]][["data"]]
  expect_equal_tables(table,
                      list("", "", "contWide", 0.651914307711847, "", "contcor1", 1.00118914256335,
                           "", "contcor2", "", "", "facFifty", "", 0.997852981864278, "contExpon",
                           "", "", "debCollin1", "", "", "debEqual1"))
})

test_that("Missing values works", {
  options <- jasptools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contNormal", "contGamma", "contcor1", "debMiss30")
  options$incl_correlations <- TRUE
  
  options$missingValues <- "pairwise"
  results <- jasptools::run("ExploratoryFactorAnalysis", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  expect_equal_tables(table, list("Model", 1.42781053334818, 2L, 0.489727939944839), label = "pairwise")
  
  options$missingValues <- "listwise"
  results <- jasptools::run("ExploratoryFactorAnalysis", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  expect_equal_tables(table, list("Model", 0.491396758561133, 2L, 0.782158104440787), label = "listwise")
})

