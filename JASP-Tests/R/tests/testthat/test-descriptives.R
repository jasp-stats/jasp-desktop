context("Descriptives")

# does not test
# - error handling

test_that("Main table results match", {
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$splitby <- "contBinom"
  options$median <- TRUE
  options$mode <- TRUE
  options$sum <- TRUE
  options$variance <- TRUE
  options$range <- TRUE
  options$standardErrorMean <- TRUE
  options$kurtosis <- TRUE
  options$skewness <- TRUE
  options$mode <- TRUE
  options$percentileValuesEqualGroups <- TRUE
  options$percentileValuesEqualGroupsNo <- 5
  options$percentileValuesPercentiles <- TRUE
  options$percentileValuesPercentilesPercentiles <- c(2, 5, 8)
  options$percentileValuesQuartiles <- TRUE
  results <- jasptools::run("Descriptives", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["stats"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0, 58, 0, -0.120135614827586, -0.2223981035, 0,
         -2.336742886, -6.96786566, 3.356094448, -2.336742886, 5.692837334,
         1.10575982846952, 0.145193378675912, 1.22270479825695, 1.89652072756094,
         0.618135836828014, 0.885861572513177, 0.313719932561217, -0.7900939905,
         -0.2223981035, 0.40412953575, -0.8670655236, -0.6057613228,
         0.201713297, 0.5234206222, -2.30423818432, -1.9634857839, -1.45975925544,
         "contNormal", 1, 42, 0, -0.283499835571429, -0.405769511, 0,
         -3.023963827, -11.906993094, 2.179421126, -3.023963827, 5.203384953,
         0.994612407217046, 0.15347202634745, 0.989253840590086, 0.972132667292966,
         0.716632727345669, 0.166587887409046, 0.365360605557062, -0.824972188,
         -0.405769511, 0.47832629925, -1.011718665, -0.5958418376, -0.2081038858,
         0.5510430038, -3.023963827, -1.64658611775, -1.54829717812)
  )
})

test_that("Frequencies table matches", {
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "facGender"
  options$splitby <- "contBinom"
  options$frequencyTables <- TRUE
  results <- jasptools::run("Descriptives", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["tables"]][[1]][["data"]]
  expect_equal_tables(table,
  list(0, "f", 26, 44.8275862068966, 44.8275862068966, 44.8275862068966,
   "TRUE", 0, "m", 32, 55.1724137931034, 55.1724137931034, 100,
   "FALSE", "", "Missing", 0, 0, "", "", "", "Total", 58, 100,
   "", "", 1, "f", 24, 57.1428571428571, 57.1428571428571, 57.1428571428571,
   "TRUE", 1, "m", 18, 42.8571428571429, 42.8571428571429, 100,
   "FALSE", "", "Missing", 0, 0, "", "", "", "Total", 42, 100,
   "", "")
  )
})

test_that("Frequencies table matches with missing values", {
  options <- jasptools::analysisOptions("Descriptives")
  x <- c(rep(NA, 10), rep(1:2, times=10))
  split <- rep(1:2, each=15)
  data <- data.frame(x=as.factor(x), split=as.factor(split))
  options$variables <- "x"
  options$splitby <- "split"
  options$frequencyTables <- TRUE
  results <- jasptools::run("Descriptives", data, options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["tables"]][[1]][["data"]]
  expect_equal_tables(table,
  list(1, 1, 3, 20, 60, 60, "TRUE", 1, 2, 2, 13.3333333333333, 40, 100,
   "FALSE", "", "Missing", 10, 66.6666666666667, "", "", "", "Total",
   15, 100, "", "", 2, 1, 7, 46.6666666666667, 46.6666666666667,
   46.6666666666667, "TRUE", 2, 2, 8, 53.3333333333333, 53.3333333333333,
   100, "FALSE", "", "Missing", 0, 0, "", "", "", "Total", 15,
   100, "", "")
  )
})

# test_that("Distribution plot matches", {
#   options <- jasptools::analysisOptions("Descriptives")
#   options$variables <- "contNormal"
#   options$plotVariables <- TRUE
#   results <- jasptools::run("Descriptives", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "distribution", dir="Descriptives")
# })
#
# test_that("Correlation plot matches", {
#   options <- jasptools::analysisOptions("Descriptives")
#   options$variables <- c("contNormal", "contGamma")
#   options$plotCorrelationMatrix <- TRUE
#   results <- jasptools::run("Descriptives", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "correlation", dir="Descriptives")
# })
#
# test_that("Boxplot matches", {
#   set.seed(0)
#   options <- jasptools::analysisOptions("Descriptives")
#   options$variables <- "contNormal"
#   options$splitby <- "contBinom"
#   options$splitPlotBoxplot <- TRUE
#   options$splitPlotColour <- TRUE
#   options$splitPlotJitter <- TRUE
#   options$splitPlotOutlierLabel <- TRUE
#   options$splitPlotViolin <- TRUE
#   options$splitPlots <- TRUE
#   results <- jasptools::run("Descriptives", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "boxplot", dir="Descriptives")
# })

# Below are the unit tests for Andy Field's book

# Chapter 4
test_that("Fields Book - Chapter 4 results match", {
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "Happiness"
  options$splitby <- "Dose"
  options$mean <- TRUE
  options$standardDeviation <- TRUE
  options$standardErrorMean <- TRUE
  options$minimum <- TRUE
  options$maximum <- TRUE
  results <- jasptools::run("Descriptives", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_04/www/Puppies Dummy.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput2a <- results[["results"]][["stats"]][["data"]]
  expect_equal_tables(tableOutput2a,
                      list("Happiness", 1, 5, 0, 2.2, 4, 1, 1.30384, 0.5830952, "TRUE",
                           "Happiness", 2, 5, 0, 3.2, 5, 2, 1.30384, 0.5830952,
                           "Happiness", 3, 5, 0, 5, 7, 3, 1.581139, 0.7071068)
  )
  # Total row
  options <- jasptools::analysisOptions("Descriptives")
  options$variables <- "Happiness"
  options$mean <- TRUE
  options$standardDeviation <- TRUE
  options$standardErrorMean <- TRUE
  options$minimum <- TRUE
  options$maximum <- TRUE
  results <- jasptools::run("Descriptives", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_04/www/Puppies Dummy.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput2b <- results[["results"]][["stats"]][["data"]]
  expect_equal_tables(tableOutput2b,
                      list("Happiness", 15, 0, 3.466667, 7, 1, 1.76743, 0.4563485)
  )
})