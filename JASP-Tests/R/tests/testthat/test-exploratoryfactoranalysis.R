context("Exploratory Factor Analysis")

# does not test
# - error handling
# - orthogonal rotation
# - Eigen values above / manual
# - contents of screeplot (set.seed does not work)

options <- jaspTools::analysisOptions("ExploratoryFactorAnalysis")
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
results <- jaspTools::run("ExploratoryFactorAnalysis", "debug.csv", options)



test_that("Factor Correlations table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cortab"]][["data"]]
  expect_equal_tables(table,
                      list(1, -0.0849326, "Factor 1", -0.0849326, 1, "Factor 2"))
})

test_that("Factor Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigtab"]][["data"]]
  expect_equal_tables(table,
                      list("Factor 1", 0.211560139236826, 1.48092097465778, 0.211560139236826,
                           "Factor 2", 0.36610038604766, 1.08178172767584, 0.154540246810834
                      ))
})

test_that("Additional fit indices table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  expect_equal_tables(table,
                      list(-32.7898349546892, 0, "0 - 0.065", 1.20127892716016))
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
  skip("Scree plot check does not work because some data is simulated (non-deterministic).")
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
  options <- jaspTools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contNormal", "contGamma", "contcor1", "debMiss30")
  options$incl_correlations <- TRUE
  
  options$missingValues <- "pairwise"
  results <- jaspTools::run("ExploratoryFactorAnalysis", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  expect_equal_tables(table, list("Model", 1.42781053334818, 2L, 0.489727939944839), label = "pairwise")
  
  options$missingValues <- "listwise"
  results <- jaspTools::run("ExploratoryFactorAnalysis", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  expect_equal_tables(table, list("Model", 0.491396758561133, 2L, 0.782158104440787), label = "listwise")
})

