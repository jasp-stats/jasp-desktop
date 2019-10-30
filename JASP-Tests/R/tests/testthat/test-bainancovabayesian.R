context("Bain ANCOVA")

options <- jasptools::analysisOptions("BainAncovaBayesian")
options$bayesFactorMatrix <- TRUE
options$bayesFactorPlot <- TRUE
options$coefficients <- TRUE
options$covariates <- list("peabody", "prenumb", "postnumb", "funumb")
options$dependent <- "age"
options$descriptivesPlot <- TRUE
options$fixedFactors <- "site"
options$model <- "site1 = site2 = site3 = site4 = site5;site1 < site2 < site3 < site4 < site5;site1 > site2 > site3 > site4 > site5"
set.seed(1)
results <- jasptools::run("BainAncovaBayesian", "sesame.csv", options)


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

test_that("Coefficients for Groups plus Covariates table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_coefficientsTable"]][["data"]]
  expect_equal_tables(table,
                      list(-3.6449166395947, 0.523724139533094, 60, 1.06342877018566, -1.5605962500308,
                           "site1", -5.33956324224342, -1.0096639198965, 55, 1.10456615365993,
                           -3.17461358106996, "site2", 1.32707019271716, 5.43992118638981,
                           64, 1.04919668205935, 3.38349568955349, "site3", -0.786388210137594,
                           3.69109729334519, 43, 1.14221568966398, 1.4523545416038, "site4",
                           -3.66569419106477, 1.98186863439215, 18, 1.44070480241248, -0.841912778336311,
                           "site5", 0.00400597434885645, 0.133513110943639, 240, 0.0330375348456078,
                           0.0687595426462478, "peabody", 0.0810741240935815, 0.289726507676895,
                           240, 0.0532276488732942, 0.185400315885238, "prenumb", -0.000747486080512422,
                           0.175557469804055, 240, 0.0449757540521855, 0.0874049918617711,
                           "postnumb", -0.0396517976270246, 0.0407199007107595, 240, 0.0205029842698429,
                           0.000534051541867467, "funumb"))
})

test_that("Descriptives Plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "descriptives-plot", dir="BainAncovaBayesian")
})

test_that("Hypothesis Legend table results match", {
  table <- results[["results"]][["legendTable"]][["data"]]
  expect_equal_tables(table,
                      list("site1 = site2 = site3 = site4 = site5", "H1", "site1 &lt; site2 &lt; site3 &lt; site4 &lt; site5",
                           "H2", "site1 &gt; site2 &gt; site3 &gt; site4 &gt; site5", "H3"
                      ))
})