context("Bain Linear Regression")

options <- jaspTools::analysisOptions("BainRegressionLinearBayesian")
options$bayesFactorMatrix <- TRUE
options$bayesFactorPlot <- TRUE
options$coefficients <- TRUE
options$covariates <- list("peabody", "prenumb", "postnumb", "funumb")
options$dependent <- "age"
options$model <- "peabody < 0 & prenumb > 0;peabody = 0 & postnumb = 0;postnumb > 0 & funumb > 0"
set.seed(1)
results <- jaspTools::run("BainRegressionLinearBayesian", "sesame.csv", options)

test_that("Bain Linear Regression table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bainTable"]][["data"]]
  expect_equal_tables(table,
                      list(1.29950533181896, 0.0304646728142417, 0.0297033594892114, "H1",
                           35.3064243610644, 0.904922937589465, 0.882308879177836, "H2",
                           3.88021871697742, 0.0646123895962929, 0.0629977235382773, "H3",
                           "", "", 0.0249900377946751, "Hu"))
})

test_that("Bayes Factor Matrix table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  expect_equal_tables(table,
                      list(1, 0.0336654885723126, 0.471498933944235, "H1", 29.7040097265193,
                           1, 14.0054089199231, "H2", 2.12089556944422, 0.0714009855561927,
                           1, "H3"))
})

test_that("Posterior Probabilities plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "posterior-probabilities", dir="BainRegressionLinearBayesian")
})

test_that("Coefficients table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_coefficientsTable"]][["data"]]
  expect_equal_tables(table,
                      list(-0.0486909406866819, 0.0683382879873222, 240, 0.0298549436614947,
                           0.00982367365032013, "peabody", 0.0633098838945105, 0.271988758484053,
                           240, 0.0532353849957385, 0.167649321189282, "prenumb", -0.046805449165961,
                           0.130880494459348, 240, 0.045328879771994, 0.0420375226466933,
                           "postnumb", -0.0355734195087462, 0.048962974589695, 240, 0.0215658029344553,
                           0.00669477754047436, "funumb"))
})

test_that("Hypothesis Legend table results match", {
  table <- results[["results"]][["legendTable"]][["data"]]
  expect_equal_tables(table,
                      list("peabody &lt; 0 &amp; prenumb &gt; 0", "H1", "peabody = 0 &amp; postnumb = 0",
                           "H2", "postnumb &gt; 0 &amp; funumb &gt; 0", "H3"))
})