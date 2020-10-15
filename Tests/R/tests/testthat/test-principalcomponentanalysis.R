context("Principal Component Analysis")

# does not test
# - error handling
# - oblique rotation
# - Parallel analysis / manual
# - slider

options <- jaspTools::analysisOptions("PrincipalComponentAnalysis")
options$variables <- list("contNormal", "contGamma", "debCollin1", "contcor1", "facFifty")
options$eigenValuesBox <- 0.95
options$orthogonalSelector <- "varimax"
options$incl_pathDiagram <- TRUE
options$incl_screePlot <- TRUE
options$factorMethod <- "eigenValues"
set.seed(1)
results <- jaspTools::run("PrincipalComponentAnalysis", "test.csv", options)


test_that("Chi-squared Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  expect_equal_tables(table,
                      list(56.1723464768203, 1, "Model", 6.63887442169672e-14))
})

test_that("Component Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loatab"]][["data"]]
  expect_equal_tables(table,
                      list(0.709068975944499, -0.055882219913321, 0.494098364850579, "contNormal",
                           -0.198414056307147, -0.730807163622534, 0.426552751857732, "contGamma",
                           -0.154267640888903, 0.766942636295035, 0.388000487607395, "debCollin1",
                           0.613519408318389, 0.258607271436745, 0.556716214776696, "contcor1",
                           -0.560112933829558, 0.0519207989938901, 0.683577731988681, "facFifty"
                      ))
})

test_that("Component Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigtab"]][["data"]]
  expect_equal_tables(table,
                      list("PC1", 0.269220893232411, 1.34610446616205, 0.269220893232411,
                           "PC2", 0.490210889783784, 1.10494998275686, 0.220989996551372
                      ))
})

test_that("Path Diagram plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_path"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "path-diagram", dir = "PrincipalComponentAnalysis")
})

test_that("Scree plot matches", {
  skip("Scree plot check does not work because some data is simulated (non-deterministic).")
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_scree"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "scree-plot", dir = "PrincipalComponentAnalysis")
})
