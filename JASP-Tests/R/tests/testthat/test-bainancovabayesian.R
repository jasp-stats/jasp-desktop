context("Bain ANCOVA")

options <- jaspTools::analysisOptions("BainAncovaBayesian")
options$bayesFactorMatrix <- TRUE
options$bayesFactorPlot <- TRUE
options$coefficients <- TRUE
options$covariates <- list("peabody", "prenumb", "postnumb", "funumb")
options$dependent <- "age"
options$descriptivesPlot <- TRUE
options$fixedFactors <- "site"
options$model <- "site1 = site2 = site3 = site4 = site5;site1 < site2 < site3 < site4 < site5;site1 > site2 > site3 > site4 > site5"
set.seed(1)
results <- jaspTools::run("BainAncovaBayesian", "sesame.csv", options)

test_that("Bain ANCOVA table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bainTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.000105084311355787, 0.0123829914149606, 0.00010420005082317,
                           "H1", 0.00832657849517319, 0.987616517988685, 0.00831056793303529,
                           "H2", 4.13483006346205e-09, 4.9059635434339e-07, 4.12825652083404e-09,
                           "H3", "", "", 0.991585227887885, "Hu"))
})

test_that("Bayes Factor Matrix table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  expect_equal_tables(table,
                      list(1, 0.0125382587162262, 25240.6918749608, "H1", 79.7558913588109,
                           1, 2013093.87900059, "H2", 3.96185653291072e-05, 4.96747822062056e-07,
                           1, "H3"))
})

test_that("Posterior Probabilities plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "posterior-probabilities", dir="BainAncovaBayesian")
})

test_that("Adjusted Means plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "adjusted-means", dir="BainAncovaBayesian")
})

test_that("Hypothesis Legend table results match", {
  table <- results[["results"]][["legendTable"]][["data"]]
  expect_equal_tables(table,
                      list("site1 = site2 = site3 = site4 = site5", "H1", "site1 &lt; site2 &lt; site3 &lt; site4 &lt; site5",
                           "H2", "site1 &gt; site2 &gt; site3 &gt; site4 &gt; site5", "H3"
                      ))
})