context("Network Analysis")

# does not test
# - error handling
# - bootstrapping
# - plots or graphical options

options <- jasptools::analysisOptions("NetworkAnalysis")
options$variables <- c("contNormal", "contcor1", "contcor2")
options$tableCentrality <- TRUE
options$tableClustering <- TRUE
options$tableWeightsMatrix <- TRUE
options$tableLayout <- TRUE
results <- jasptools::run("NetworkAnalysis", "test.csv", options)

test_that("generalTB table results match", {
  table <- results[["results"]][["generalTB"]][["data"]]
  expect_equal_tables(table,
    list("Network", 3, "2 / 3", 0.333333333333333)
  )
})

test_that("centralityTB table results match", {
  table <- results[["results"]][["centralityTB"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", -0.577350269189626, -1.12003079401266, -1.14291478283658,
      "contcor1", 1.15470053837925, 0.803219560925303, 0.713968266021615,
      "contcor2", -0.577350269189626, 0.316811233087358, 0.428946516814968)
  )
})

test_that("clusteringTB table results match", {
  table <- results[["results"]][["clusteringTB"]][["data"]]
  expect_equal_tables(table,
    list("contcor1", 0, 0, 0, 0, "contcor2", 0, 0, 0, 0, "contNormal",
      0, 0, 0, 0)
  )
})

test_that("weightmatrixTB table results match", {
  table <- results[["results"]][["weightmatrixTB"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0, 0.0939476582188346, 0, "contcor1", 0.0939476582188346,
     0, 0.612057902640958, "contcor2", 0, 0.612057902640958, 0)
  )
})

test_that("layoutTB table results match", {
  table <- results[["results"]][["layoutTB"]][["data"]]
  expect_equal_tables(table,
    list(1, 1, -1, 0.0473496200043197, -0.258402690078048, -1)
  )
})
