context("Contingency Tables")

# does not test
# - row/column order (ascending/descending)

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "facFifty"
  options$layers <- list(list(
    name = "Layer 1",
    variables = "facGender"
  ))
  options$countsObserved <- TRUE
  options$countsExpected <- TRUE
  options$percentagesRow <- TRUE
  options$percentagesColumn <- TRUE
  options$percentagesTotal <- TRUE
  results <- JASPTools::run("ContingencyTables", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["Counts Table 1"]][["data"]]
  expect_equal_tables(table,
    list("Count", "% of Total", 320, 495, 815, "Expected count", 394.529977794227,
         420.470022205773, 815, " % within row", 0.392638036809816, 0.607361963190184,
         1, " % within column", 0.489296636085627, 0.710186513629842,
         0.603256846780163, " % within row", 0.392638036809816, 0.607361963190184,
         1, " % of Total", 0.236861584011843, 0.36639526276832, 0.603256846780163,
         "control", "f", "Count", "% of Total", 334, 202, 536, "Expected count",
         259.470022205773, 276.529977794227, 536, " % within row", 0.623134328358209,
         0.376865671641791, 1, " % within column", 0.510703363914373,
         0.289813486370158, 0.396743153219837, " % within row", 0.623134328358209,
         0.376865671641791, 1, " % of Total", 0.247224278312361, 0.149518874907476,
         0.396743153219837, "experimental", "f", "Count", "% of Total",
         654, 697, 1351, "Expected count", 654, 697, 1351, " % within row",
         0.484085862324204, 0.515914137675796, 1, " % within column",
         1, 1, 1, " % within row", 0.484085862324204, 0.515914137675796,
         1, 0.484085862324204, 0.515914137675796, 1, "Total", "f", "Count",
         "% of Total", 253, 182, 435, "Expected count", 271.013344453711,
         163.986655546289, 435, " % within row", 0.581609195402299, 0.418390804597701,
         1, " % within column", 0.338688085676037, 0.402654867256637,
         0.362802335279399, " % within row", 0.581609195402299, 0.418390804597701,
         1, " % of Total", 0.211009174311927, 0.151793160967473, 0.3628023352794,
         "control", "m", "Count", "% of Total", 494, 270, 764, "Expected count",
         475.986655546289, 288.013344453711, 764, " % within row", 0.646596858638743,
         0.353403141361257, 1, " % within column", 0.661311914323963,
         0.597345132743363, 0.6371976647206, " % within row", 0.646596858638743,
         0.353403141361257, 1, " % of Total", 0.412010008340284, 0.225187656380317,
         0.6371976647206, "experimental", "m", "Count", "% of Total",
         747, 452, 1199, "Expected count", 747, 452, 1199, " % within row",
         0.62301918265221, 0.37698081734779, 1, " % within column", 1,
         1, 1, " % within row", 0.62301918265221, 0.37698081734779, 1,
         0.62301918265221, 0.37698081734779, 1, "Total", "m", "Count",
         "% of Total", 573, 677, 1250, "Expected count", 686.764705882353,
         563.235294117647, 1250, " % within row", 0.4584, 0.5416, 1,
         " % within column", 0.408993576017131, 0.589208006962576, 0.490196078431373,
         " % within row", 0.4584, 0.5416, 1, " % of Total", 0.224705882352941,
         0.265490196078431, 0.490196078431373, "control", "Total", "Count",
         "% of Total", 828, 472, 1300, "Expected count", 714.235294117647,
         585.764705882353, 1300, " % within row", 0.636923076923077,
         0.363076923076923, 1, " % within column", 0.591006423982869,
         0.410791993037424, 0.509803921568627, " % within row", 0.636923076923077,
         0.363076923076923, 1, " % of Total", 0.324705882352941, 0.185098039215686,
         0.509803921568627, "experimental", "Total", "Count", "% of Total",
         1401, 1149, 2550, "Expected count", 1401, 1149, 2550, " % within row",
         0.549411764705882, 0.450588235294118, 1, " % within column",
         1, 1, 1, " % within row", 0.549411764705882, 0.450588235294118,
         1, 0.549411764705882, 0.450588235294118, 1, "Total", "Total")
  )
})

test_that("Multiple row and column variables give multiple main tables", {
  options <- JASPTools::analysisOptions("ContingencyTables")
  options$rows <- c("facExperim", "facGender")
  options$columns <- c("contBinom", "facFive")
  results <- JASPTools::run("ContingencyTables", "debug.csv", options, view=FALSE, quiet=TRUE)

  pairs <- list(
    c("facExperim", "contBinom"),
    c("facExperim", "facFive"),
    c("facGender", "contBinom"),
    c("facGender", "facFive")
  )

  for (i in 1:4) {
    rows <- results[["results"]][[paste("Counts Table", i)]][["schema"]][["fields"]][[1]][["name"]]
    cols <- results[["results"]][[paste("Counts Table", i)]][["schema"]][["fields"]][[2]][["overTitle"]]
    expect_identical(c(rows, cols), pairs[[i]], label=paste("Table", i))
  }
})

test_that("Chi-Squared test table results match", {
  options <- JASPTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "facFifty"
  options$chiSquared <- TRUE
  options$chiSquaredContinuityCorrection <- TRUE
  options$likelihoodRatio <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- JASPTools::run("ContingencyTables", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["Tests Table 1"]][["data"]]
  expect_equal_tables(table,
    list("N", "", "", "", 2550, "<unicode><unicode>", 82.0397085317219,
         1, 1.33379878452991e-19, 63462127883801120, "<unicode><unicode> continuity correction",
         81.3201582621313, 1, 1.91958529099645e-19, 44468347240355080,
         "Likelihood ratio", 82.4643894680383, 1, 0, "<unicode>")
  )
})

test_that("Nominal table results match", {
  options <- JASPTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$contingencyCoefficient <- TRUE
  options$phiAndCramersV <- TRUE
  results <- JASPTools::run("ContingencyTables", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["Nominal Table 1"]][["data"]]
  expect_equal_tables(table,
    list("Contingency coefficient", 0.0807792391722019, "Phi-coefficient",
         0.0810440898473108, "Cramer's V ", 0.0810440898473108)
  )
})

test_that("Log Odds Ratio table results match", {
  options <- JASPTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$oddsRatio <- TRUE
  options$oddsRatioConfidenceIntervalInterval <- 0.90
  results <- JASPTools::run("ContingencyTables", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["Odds Ratio Table 1"]][["data"]]
  expect_equal_tables(table,
    list("Odds ratio", -0.329205575243527, -0.998167649205055, 0.339756498718001,
         "Fisher's exact test ", -0.325882968750928, -1.07370478788709,
         0.415368461868818)
  )
})

test_that("Ordinal Gamma table results match", {
  options <- JASPTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$gamma <- TRUE
  results <- JASPTools::run("ContingencyTables", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["Ordinal Table 1"]][["data"]]
  expect_equal_tables(table,
    list("Gamma coefficient", -0.163132137030995, 0.197938461395245, -0.551084392520947,
         0.224820118458957)
  )
})

test_that("Kendall's Tau table results match", {
  options <- JASPTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$kendallsTauB <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- JASPTools::run("ContingencyTables", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["Kendalls Table 1"]][["data"]]
  expect_equal_tables(table,
    list("Kendall's Tau-b", -0.0810440898473108, 0.420024632711394, 1,
         -0.806378512498144)
  )
})

test_that("Analysis handles errors", {
  options <- JASPTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "contNormal"
  results <- JASPTools::run("ContingencyTables", "debug.csv", options, view=FALSE, quiet=TRUE)
  errorMsg <- results[["results"]][["Counts Table 1"]][["error"]][["errorMessage"]]
  expect_is(errorMsg, "character")
})
