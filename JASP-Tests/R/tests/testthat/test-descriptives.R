context("Descriptives")

# does not test
# - error handling

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("Descriptives")
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
  results <- JASPTools::run("Descriptives", "debug.csv", options, view=FALSE, quiet=TRUE)
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
  options <- JASPTools::analysisOptions("Descriptives")
  options$variables <- "facGender"
  options$splitby <- "contBinom"
  options$frequencyTables <- TRUE
  results <- JASPTools::run("Descriptives", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["tables"]][[1]][["data"]]
  expect_equal_tables(table,
    list(0, "f", 26, 44.8275862068966, 44.8275862068966, 44.8275862068966,
         "TRUE", 0, "m", 32, 55.1724137931034, 55.1724137931034, 100,
         "FALSE", "", "Total", 58, 100, 100, "", 1, "f", 24, 57.1428571428571,
         57.1428571428571, 57.1428571428571, "TRUE", 1, "m", 18, 42.8571428571429,
         42.8571428571429, 100, "FALSE", "", "Total", 42, 100, 100, "")
  )
})

test_that("Distribution plot matches", {
  options <- JASPTools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$plotVariables <- TRUE
  results <- JASPTools::run("Descriptives", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "distribution", dir="Descriptives")
})

test_that("Correlation plot matches", {
  options <- JASPTools::analysisOptions("Descriptives")
  options$variables <- c("contNormal", "contGamma")
  options$plotCorrelationMatrix <- TRUE
  results <- JASPTools::run("Descriptives", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "correlation", dir="Descriptives")
})

test_that("Boxplot matches", {
  set.seed(0)
  options <- JASPTools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$splitby <- "contBinom"
  options$splitPlotBoxplot <- TRUE
  options$splitPlotColour <- TRUE
  options$splitPlotJitter <- TRUE
  options$splitPlotOutlierLabel <- TRUE
  options$splitPlotViolin <- TRUE
  options$splitPlots <- TRUE
  results <- JASPTools::run("Descriptives", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "boxplot", dir="Descriptives")
})
