context("Bayesian Contingency Tables")

# does not test
# - row/column order (ascending/descending)
# - different hypothesis options
# - bftype (01, 10, log)
# - log odds for different sampling models
# - error handling in plots

test_that("Main table results match", {
  options <- jasptools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "facFifty"
  options$layers <- list(list(
    name = "Layer 1",
    variables = "facGender")
  )
  results <- jasptools::run("ContingencyTablesBayesian", "test.csv", options)
  table <- results[["results"]][["container1"]][["collection"]][["container1_contTabBasMain"]][["data"]]
  expect_equal_tables(table,
    list(320, 495, "Count", 815, "control", "f", "TRUE", 334, 202, "Count",
         536, "experimental", "f", 654, 697, "Count", 1351, "Total",
         "TRUE", "f", 253, 182, "Count", 435, "control", "m", "TRUE",
         494, 270, "Count", 764, "experimental", "m", 747, 452, "Count",
         1199, "Total", "TRUE", "m", 573, 677, "Count", 1250, "control",
         "Total", "TRUE", 828, 472, "Count", 1300, "experimental", "Total",
         1401, 1149, "Count", 2550, "Total", "TRUE", "Total")
  )
})

test_that("Multiple row and column variables give multiple main tables", {
  options <- jasptools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- c("facExperim", "facGender")
  options$columns <- c("contBinom", "facFive")
  results <- jasptools::run("ContingencyTablesBayesian", "test.csv", options)

  pairs <- list(
    c("facExperim", "contBinom"),
    c("facExperim", "facFive"),
    c("facGender", "contBinom"),
    c("facGender", "facFive")
  )

  for (i in 1:4) {
    rows <- results[["results"]][[paste0("container", i)]][["collection"]][[paste0("container", i, "_contTabBasMain")]][["schema"]][["fields"]][[1]][["name"]]
    cols <- results[["results"]][[paste0("container", i)]][["collection"]][[paste0("container", i, "_contTabBasMain")]][["schema"]][["fields"]][[2]][["overTitle"]]
    expect_identical(c(rows, cols), pairs[[i]], label=paste("Table", i))
  }
})

test_that("Bayesian Contingency Tables Tests table results match", {
  options <- jasptools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$priorConcentration <- 1.5

  samplingModels <- c("poisson", "jointMultinomial", "independentMultinomialRowsFixed",
                      "independentMultinomialColumnsFixed", "hypergeometric")
  refTables <- list(
    poisson = list("BF<unicode><unicode> Poisson", "N", 100, 0.523118843924781),
    jointMultinomial = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.440084106793853),
    independentMultinomialRowsFixed = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.35545254779504),
    independentMultinomialColumnsFixed = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.364069579256729),
    hypergeometric = list("BF<unicode><unicode> hypergeometric", "N", 100, 0.269648117146104)
  )

  for (samplingModel in samplingModels) {
    options$samplingModel <- samplingModel
    results <- jasptools::run("ContingencyTablesBayesian", "test.csv", options)
    table <- results[["results"]][["container1"]][["collection"]][["container1_contTabBasBF"]][["data"]]
    expect_equal_tables(table, refTables[[samplingModel]], label=paste("Sampling model", samplingModel))
  }
})

test_that("Bayesian Contingency Tables Tests table results match - different hypotheses", {
  set.seed(0)
  options <- jasptools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$priorConcentration <- 1.5
  
  samplingModels <- c("poisson", "jointMultinomial", "independentMultinomialRowsFixed",
                      "independentMultinomialColumnsFixed")
  hypotheses <- c("groupsNotEqual", "groupOneGreater", "groupTwoGreater")
  
  refTables <- list(
    poisson = list(
      groupsNotEqual  = list("BF<unicode><unicode> Poisson", "N", 100, 0.523118843924781),
      groupOneGreater = list("BF<unicode><unicode> Poisson", "N", 100, 0.224941102887656),             
      groupTwoGreater = list("BF<unicode><unicode> Poisson", "N", 100, 0.818576366973497)),
    jointMultinomial = list(
      groupsNotEqual  = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.440084106793853),
      groupOneGreater = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.181490685641785),             
      groupTwoGreater = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.683978718779006)),
    independentMultinomialRowsFixed = list(
      groupsNotEqual  = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.35545254779504),
      groupOneGreater = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.149361160583476),             
      groupTwoGreater = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.560761939401455)),
    independentMultinomialColumnsFixed = list(
      groupsNotEqual  = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.364069579256729),
      groupOneGreater = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.153564548530488),             
      groupTwoGreater = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.571734867264767))
  )
  
  for (samplingModel in samplingModels) {
    options$samplingModel <- samplingModel
    for(hypothesis in hypotheses) {
      options$hypothesis <- hypothesis
      results <- jasptools::run("ContingencyTablesBayesian", "test.csv", options, view = FALSE)
      table <- results[["results"]][["container1"]][["collection"]][["container1_contTabBasBF"]][["data"]]
      #makeTestTable(table)
      expect_equal_tables(table, refTables[[samplingModel]][[hypothesis]], label=paste("Sampling model", samplingModel, "; hypothesis", hypothesis))
    }
  }
})

test_that("Log Odds Ratio table results match", {
  set.seed(0)
  options <- jasptools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$oddsRatio <- TRUE
  options$oddsRatioCredibleIntervalInterval <- 0.90
  results <- jasptools::run("ContingencyTablesBayesian", "test.csv", options)
  table <- results[["results"]][["container1"]][["collection"]][["container1_contTabBasLogOdds"]][["data"]]
  expect_equal_tables(table,
    list("Odds ratio", -0.325226942981456, -0.981898524010587, 0.337174584207703)
  )
})

test_that("Cramer's V table results match", {
  set.seed(0)
  options <- jasptools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$effectSize <- TRUE
  options$effectSizeCredibleIntervalInterval <- 0.90
  results <- jasptools::run("ContingencyTablesBayesian", "test.csv", options)
  table <- results[["results"]][["container1"]][["collection"]][["container1_contTabBasCramersV"]][["data"]]
  expect_equal_tables(table,
                      list("Cramer's V", 0.0698837782777569, 1.02065076979155e-16, 0.216657039422164)
  )
})

test_that("Log Odds Ratio Plot matches", {
  set.seed(0)
  options <- jasptools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$plotPosteriorOddsRatio <- TRUE
  options$plotPosteriorOddsRatioAdditionalInfo <- TRUE
  results <- jasptools::run("ContingencyTablesBayesian", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "log-odds-ratio", dir="ContingencyTablesBayesian")
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "contNormal"
  results <- jasptools::run("ContingencyTablesBayesian", "test.csv", options)
  errorMsg <- results[["results"]][["errorMessage"]]
  expect_is(errorMsg, "character")
})
