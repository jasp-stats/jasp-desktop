context("Bain ANOVA")

options <- jasptools::analysisOptions("BainAnovaBayesian")
options$bayesFactorMatrix <- TRUE
options$bayesFactorPlot <- TRUE
options$dependent <- "age"
options$descriptives <- TRUE
options$descriptivesPlot <- TRUE
options$fixedFactors <- "site"
options$model <- "site1 = site2 = site3 = site4 = site5;site1 < site2 < site3 < site4 < site5;site1 > site2 > site3 > site4 > site5"
set.seed(1)
results <- jasptools::run("BainAnovaBayesian", "sesame.csv", options)


test_that("Bain ANOVA table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bainTable"]][["data"]]
  expect_equal_tables(table,
                      list(404.653666701287, 0.992827040615495, 0.990397078695981, "H1",
                           2.91811756517224, 0.0070507685056039, 0.00703351162371911, "H2",
                           0.0494067240352438, 0.0001221908789008, 0.000121891814541657,
                           "H3", "", "", 0.00244751786575824, "Hu"))
})

test_that("Bayes Factor Matrix table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  expect_equal_tables(table,
                      list(1, 140.811181054435, 8125.21400571575, "H1", 0.00710170877420183,
                           1, 57.7029035966591, "H2", 0.000123073681418919, 0.0173301504373152,
                           1, "H3"))
})

test_that("Posterior Probabilities plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "posterior-probabilities", dir="BainAnovaBayesian")
})

test_that("Descriptives Plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "descriptives-plot", dir="BainAnovaBayesian")
})

test_that("Descriptive Statistics table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
  expect_equal_tables(table,
                      list(60, 49.8551481684342, 49.8666666666667, 7.462, 0.963300196876966,
                           49.8781851648991, 1, 55, 50.8635343274387, 50.8727272727273,
                           5.699, 0.768510976836514, 50.8819202180158, 2, 64, 52.022763819189,
                           52.03125, 5.679, 0.7098962973394, 52.039736180811, 3, 43, 51.4080569870222,
                           51.4186046511628, 5.774, 0.88059714848365, 51.4291523153033,
                           4, 18, 50.6467186344585, 50.6666666666667, 7.004, 1.65090587135622,
                           50.6866146988748, 5))
})

test_that("Hypothesis Legend table results match", {
  table <- results[["results"]][["legendTable"]][["data"]]
  expect_equal_tables(table,
                      list("site1 = site2 = site3 = site4 = site5", "H1", "site1 &lt; site2 &lt; site3 &lt; site4 &lt; site5",
                           "H2", "site1 &gt; site2 &gt; site3 &gt; site4 &gt; site5", "H3"
                      ))
})