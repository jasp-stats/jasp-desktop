context("Principal Component Analysis")

# does not test
# - error handling
# - oblique rotation
# - Parallel analysis / manual
# - contents of screeplot (set.seed does not work)
# - slider

test_that("Main tables' results match", {
  options <- jasptools::analysisOptions("PrincipalComponentAnalysis")
  options$variables <- list("contNormal", "contGamma", "debCollin1", "contcor1", "facFifty")
  options$factorMethod <- "eigenValues"
  options$eigenValuesBox <- 0.95
  options$rotationMethod <- "orthogonal"
  options$orthogonalSelector <- "varimax"
  results <- jasptools::run("PrincipalComponentAnalysis", "test.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")

  table <- results[["results"]][["factorLoadings"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0.709068975944499, ".", 0.494098364850579, "contGamma",
         ".", -0.730807163622534, 0.426552751857732, "debCollin1", ".",
         0.766942636295035, 0.388000487607395, "contcor1", 0.613519408318389,
         ".", 0.556716214776696, "facFifty", -0.560112933829558, ".",
         0.683577731988681),
    label="Factor loadings table"
  )

  table <- results[["results"]][["goodnessOfFit"]][["data"]]
  expect_equal_tables(table, list("Model", 56.1723464768203, 1, 6.63887442169672e-14),
    label="Chi squared table"
  )
})

# test_that("Path diagram matches", {
#   options <- jasptools::analysisOptions("PrincipalComponentAnalysis")
#   options$variables <- list("contNormal", "contGamma")
#   options$incl_pathDiagram <- TRUE
#   results <- jasptools::run("PrincipalComponentAnalysis", "test.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "path-diagram", dir="PrincipalComponentAnalysis")
# })

test_that("Scree plot option creates .png", {
  options <- jasptools::analysisOptions("PrincipalComponentAnalysis")
  options$variables <- list("contNormal", "contGamma")
  options$incl_screePlot <- TRUE
  results <- jasptools::run("PrincipalComponentAnalysis", "test.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")
  expect_match(results[["results"]][["screePlot"]][["data"]], ".*\\.png")
})

test_that("Factor correlation table matches", {
  options <- jasptools::analysisOptions("PrincipalComponentAnalysis")
  options$variables <- list("contNormal", "contGamma", "contcor1", "debCollin1")
  options$incl_correlations <- TRUE
  results <- jasptools::run("PrincipalComponentAnalysis", "test.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")
  table <- results[["results"]][["factorCorrelations"]][["data"]]
  expect_equal_tables(table, list("RC 1", 1))
})

test_that("Missing values works", {
	options <- jasptools::analysisOptions("PrincipalComponentAnalysis")
	options$variables <- list("contNormal", "contGamma", "contcor1", "debMiss30")

	options$missingValues <- "pairwise"
	results <- jasptools::run("PrincipalComponentAnalysis", "test.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")
	table <- results[["results"]][["goodnessOfFit"]][["data"]][[1]]
	expect_equal_tables(table, list("Model", 20.7622398603288, 2, 3.10125086457269e-05), label = "pairwise")

	options$missingValues <- "listwise"
	results <- jasptools::run("PrincipalComponentAnalysis", "test.csv", options, view=FALSE, quiet=TRUE, sideEffects="pkgLoading")
	table <- results[["results"]][["goodnessOfFit"]][["data"]][[1]]
	expect_equal_tables(table, list("Model", 13.8130059031587, 2, 0.00100125311189221), label = "listwise")
})
