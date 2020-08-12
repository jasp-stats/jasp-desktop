context("Multinomial Test")

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$VovkSellkeMPR <- TRUE
  options$countProp <- "descProps"
  options$descriptives <- TRUE
  options$factor <- "facOutlier"
  options$hypothesis <- "expectedProbs"
  options$tableWidget <- list(
    list(
      levels = list("f1", "f2", "f3", "totallyridiculoussuperlongfactorme"),
      name = "H1",
      values = list(50, 42, 5, 3)
    )
  )
  results <- jaspTools::run("MultinomialTest",
                            "test.csv", options)
  maintable <- results[["results"]][["chisqTable"]][["data"]]
  desctable <- results[["results"]][["descriptivesTable"]][["data"]]

  expected <- jaspTools:::collapseTable(
      list(list(case = "H\u2080 (a)",
                chisquare = 5.72,
                df = 3,
                p = 0.126056548007017,
                VovkSellkeMPR = 1.40914224189199)))

  expect_equal_tables(maintable, expected)
  expect_equal_tables(desctable,
                      list("f1", 0.49, 0.5,
                           "f2", 0.49, 0.42,
                           "f3", 0.01, 0.05,
                           "totallyridiculoussuperlongfactorme", 0.01, 0.03))
})

test_that("Descriptives plot matches", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$factor <- "facFive"
  options$descriptivesPlot <- TRUE
  results <- jaspTools::run("MultinomialTest", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "descriptives-1", dir="MultinomialTest")
})

test_that("Analysis handles errors - Negative Values", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$factor <- "facExperim"
  options$counts <- "contNormal"
  options$tableWidget <- list(
    list(
      levels = list("control", "experimental"),
      name = "H1",
      values = list(50, 50)
    )
  )
  results <- jaspTools::run("MultinomialTest", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Analysis handles errors - wrong levels", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$factor <- "facExperim"
  options$counts <- "debSame"
  options$tableWidget <- list(
    list(
      levels = list("control", "experimental"),
      name = "H1",
      values = list(50, 50)
    )
  )
  results <- jaspTools::run("MultinomialTest", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Analysis handles errors - Infinities", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$factor <- "facExperim"
  options$counts <- "debInf"
  options$tableWidget <- list(
    list(
      levels = list("control", "experimental"),
      name = "H1",
      values = list(50, 50)
    )
  )
  results <- jaspTools::run("MultinomialTest", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})