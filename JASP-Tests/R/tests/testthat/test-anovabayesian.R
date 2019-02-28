context("Bayesian ANOVA")

# does not test
# - descriptives table (code is from regular ANOVA)
# - descriptives plot (code is from regular ANOVA)
# - bftype (01, 10)

initOpts <- function() {
  options <- jasptools::analysisOptions("AnovaBayesian")
  options$sampleMode <- "manual"
  options$fixedSamplesNumber <- 50
  return(options)
}

test_that("Main table results match", {
  set.seed(0)
  options <- initOpts()
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facGender", "facFive")
  options$randomFactors <- "facExperim"
  options$priorFixedEffects <- 0.4
  options$priorRandomEffects <- 1.5
  options$modelTerms <- list(
    list(components="facGender", isNuisance=FALSE),
    list(components="facFive", isNuisance=FALSE),
    list(components="facExperim", isNuisance=TRUE),
    list(components=c("facGender", "facFive"), isNuisance=FALSE)
  )

  refTables <- list(
    nullModelTop = list(2.03396231183419, 4.36594897914025, "facGender", 0.2, 0.521871337014648,
                        0.969106320600043, 1, 1.38052893828542, "Null model (incl. facExperim)",
                        0.2, 0.256578666172056, 0.432501410293758, 0.499288951910149,
                        "facGender + facFive", 0.2, 0.110970634970706, 1.41647769133671,
                        0.21724304954363, 0.236121101672541, "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                        0.2, 0.0557399318870544, 1.83775875304445, 0.213733397143635,
                        0.232085136403676, "facFive", 0.2, 0.0548394299555363, 1.04177436451923),
    bestModelTop = list(1, 4.18686558133661, "facGender", 0.2, 0.511412522868496, 0.510943003733483,
                        1.41493752908138, "Null model (incl. facExperim)", 0.2, 0.261302650581348,
                        2.3021061884882, 0.223966148027088, 0.517421342346211, "facGender + facFive",
                        0.2, 0.114539092799672, 4.15307380646848, 0.111744890960883,
                        0.242446197872535, "facFive", 0.2, 0.0571477366039704, 2.59068727917517,
                        0.108714579053848, 0.235484452504449, "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                        0.2, 0.0555979971465146, 3.03025679917061)
  )

  for (order in c("nullModelTop", "bestModelTop")) {
    options$bayesFactorOrder <- order
    results <- jasptools::run("AnovaBayesian", "test.csv", options)
    table <- results[["results"]][["tableModelComparison"]][["data"]]
    expect_equal_tables(table, refTables[[order]], label=paste("Table with order", order))
  }
})

test_that("Effects table results match", {
  set.seed(0)
  options <- initOpts()
  options$dependent <- "contNormal"
  options$fixedFactors <- list("facFive", "contBinom")
  options$effects <- TRUE
  options$modelTerms <- list(
    list(components="facFive", isNuisance=FALSE),
    list(components="contBinom", isNuisance=FALSE),
    list(components=c("facFive", "contBinom"), isNuisance=FALSE)
  )

  refTables <- list(
    allModels = list(0.101833665496691, "facFive", 0.6, 0.132509592038855, 0.184940107557576,
                     "contBinom", 0.6, 0.217166083167955, 0.0176366361186621, "facFive<unicode><unicode><unicode>contBinom",
                     0.2, 0.0043898036871001),
    matchedModels = list(0.146995523754444, "facFive", 0.4, 0.127601379108464, 0.271032837792626,
                         "contBinom", 0.4, 0.2123137507459, 0.174704371549221, "facFive<unicode><unicode><unicode>contBinom",
                         0.2, 0.00433559523354601)
  )

  for (effectsType in c("allModels", "matchedModels")) {
    options$effectsType <- effectsType
    results <- jasptools::run("AnovaBayesian", "test.csv", options)
    table <- results[["results"]][["tableEffects"]][["data"]]
    expect_equal_tables(table, refTables[[effectsType]], label=paste("Table with effects type", effectsType))
  }
})

test_that("Post-hoc Comparisons table results match", {
  options <- jasptools::analysisOptions("AnovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(
    list(components="facFive", isNuisance=FALSE)
  )
  options$postHocTestsNullControl <- TRUE
  options$postHocTestsVariables <- "facFive"

  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  table <- results[["results"]][["collectionPosthoc"]][["collection"]][["collectionPosthoc_postHoc_facFive"]][["data"]]
  expect_equal_tables(table,
    list(1, 2, 0.312140273352768, 0.099731286607023, 0.319507910772894,
         0.00713501145651816, 1, 3, 0.814810284000699, 0.260338331517332,
         0.319507910772894, 0.00346528957422733, 1, 4, 0.309300726898471,
         0.0988240290518682, 0.319507910772894, 0.00712877021506968,
         1, 5, 0.435141737421767, 0.139031227413716, 0.319507910772894,
         0.0084847992806719, 2, 3, 0.940407207963574, 0.300467542292212,
         0.319507910772894, 0.00368467308971747, 2, 4, 0.309677154169755,
         0.0989443005428739, 0.319507910772894, 0.00712946546406268,
         2, 5, 0.47450139081732, 0.151606948038875, 0.319507910772894,
         0.00859149881710335, 3, 4, 0.743172685201022, 0.23744955199206,
         0.319507910772894, 0.00425332570766848, 3, 5, 0.327698986510047,
         0.10470241854222, 0.319507910772894, 0.00721167205986274, 4,
         5, 0.431922758637939, 0.138002738227673, 0.319507910772894,
         0.00846064111128918)
  )
})

test_that("Analysis handles errors", {
  options <- initOpts()
  options$dependent <- "debInf"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Inf check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "debSame"
  options$modelTerms <- list(list(components="debSame", isNuisance=FALSE))
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "1-level factor check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=TRUE))
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["tableModelComparison"]][["error"]][["type"]], "badData",
                   label="All nuisance check")

  # options$dependent <- "debSame"
  # options$fixedFactors <- "facFive"
  # options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  # results <- jasptools::run("AnovaBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="No variance check")

  options$dependent <- "debMiss99"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Too few obs check")
})
