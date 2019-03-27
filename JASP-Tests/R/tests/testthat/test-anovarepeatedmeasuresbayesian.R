context("Bayesian Repeated Measures ANOVA")

# does not test
# - descriptives table (code from regular ANOVA)
# - descriptives plot (code from regular ANOVA)
# - bftype (01, 10)

initOpts <- function() {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasuresBayesian")
  options$sampleMode <- "manual"
  options$fixedSamplesNumber <- 50
  return(options)
}

test_that("Main table results match", {
  set.seed(0)
  options <- initOpts()
  options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2"), name="RM Factor 1")
  )
  options$betweenSubjectFactors <- "facGender"
  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components="RM Factor 1", isNuisance=FALSE),
    list(components="facGender", isNuisance=FALSE),
    list(components="contcor1", isNuisance=FALSE),
    list(components=c("RM Factor 1", "facGender"), isNuisance=FALSE)
  )
  options$priorCovariates <- 0.3
  options$priorFixedEffects <- 0.8
  options$priorRandomEffects <- 0.8

  refTables <- list(
    nullModelTop = list(3.62496733116162e+23, 5.73145848262438, "RM Factor 1 + facGender",
                        0.1, 0.389062528288327, 1.72433512753681, 3.46448419993762e+23,
                        5.32751662817302, "RM Factor 1", 0.1, 0.37183810470666, 3.74201005842137,
                        8.9411204428442e+22, 0.955353109807862, "RM Factor 1 + facGender + contcor1",
                        0.1, 0.0959637593232787, 16.4218696956561, 6.81640452356858e+22,
                        0.710408526266743, "RM Factor 1 + contcor1", 0.1, 0.0731594890518848,
                        4.65091229160849, 5.48567012520599e+22, 0.5630423677071, "RM Factor 1 + facGender + RM Factor 1<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>facGender",
                        0.1, 0.0588769082115965, 8.02076448534254, 1.03413390502686e+22,
                        0.101014070184426, "RM Factor 1 + facGender + contcor1 + RM Factor 1<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>facGender",
                        0.1, 0.0110992104182495, 4.18450741833095, 1, 9.65957051390276e-24,
                        "Null model (incl. subject)", 0.1, 1.07328561265586e-24, 0.449149895733824,
                        4.33859508915294e-24, "facGender", 0.1, 4.82066121016992e-25,
                        3.63958095837114, 0.199117747695815, 1.92339192443722e-24, "contcor1",
                        0.1, 2.13710213826357e-25, 2.50946236929854, 0.0827555999423614,
                        7.99383553063569e-25, "facGender + contcor1", 0.1, 8.88203947848407e-26,
                        1.53533237565983),
    bestModelTop = list(1, 6.50874645747621, "RM Factor 1 + facGender", 0.1, 0.419682304777031,
                        0.836215528978976, 4.86631035203809, "RM Factor 1", 0.1, 0.350944860492241,
                        1.97072663295012, 0.215841804977564, 0.896471757374706, "RM Factor 1 + facGender + contcor1",
                        0.1, 0.0905849861802185, 6.6431450159699, 0.170299127246734,
                        0.692756111371905, "RM Factor 1 + contcor1", 0.1, 0.071471530224426,
                        2.19671475265757, 0.133248845838306, 0.533112456797527, "RM Factor 1 + facGender + RM Factor 1<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>facGender",
                        0.1, 0.0559221827302997, 2.962819662002, 0.0271494305718735,
                        0.103729124066941, "RM Factor 1 + facGender + contcor1 + RM Factor 1<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>facGender",
                        0.1, 0.0113941355957878, 4.60008794602492, 2.70717632857682e-24,
                        1.02253860091345e-23, "Null model (incl. subject)", 0.1, 1.13615400101494e-24,
                        1.77842089287593, 1.18358526138156e-24, 4.47056811357066e-24,
                        "facGender", 0.1, 4.96729790396738e-25, 2.11365788695685, 5.27151495518567e-25,
                        1.99112539145302e-24, "contcor1", 0.1, 2.21236154605891e-25,
                        2.06808792667175, 2.66915441617695e-25, 1.00817718946824e-24,
                        "facGender + contcor1", 0.1, 1.12019687718693e-25, 13.9884843562539)
  )

  for (order in c("nullModelTop", "bestModelTop")) {
    options$bayesFactorOrder <- order
    results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
    table <- results[["results"]][["tableModelComparison"]][["data"]]
    expect_equal_tables(table, refTables[[order]], label=paste("Table with order", order))
  }
})

test_that("Effects table results match", {
  options <- initOpts()
  options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2"), name="RM Factor 1")
  )
  options$betweenSubjectFactors <- "facGender"
  options$modelTerms <- list(
    list(components="RM Factor 1", isNuisance=FALSE),
    list(components="facGender", isNuisance=FALSE),
    list(components=c("RM Factor 1", "facGender"), isNuisance=FALSE)
  )
  options$effects <- TRUE

  refTables <- list(
    allModels = list(1501199875790165, "RM Factor 1", 0.6, 1, 1.23554633080451, "facGender",
                     0.6, 0.649531010694943, 0.541809634931422, "RM Factor 1<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>facGender",
                     0.2, 0.11929377901802),
    matchedModels = list(7.95624688797966e+23, "RM Factor 1", 0.4, 0.88070622098198, 1.51293623075847,
                         "facGender", 0.4, 0.530237231676923, 0.224981898462208, "RM Factor 1<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>facGender",
                         0.2, 0.11929377901802)
  )

  effectsTypes <- c("allModels", "matchedModels")
  for (effectsType in effectsTypes) {
    options$effectsType <- effectsType
    set.seed(5) # setting seed at start gives aberrant behaviour
    results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
    table <- results[["results"]][["tableEffects"]][["data"]]
    expect_equal_tables(table, refTables[[effectsType]], label=paste("Table with effects type", effectsType))
  }
})

test_that("Post-hoc Comparisons table results match", {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasuresBayesian")
  options$repeatedMeasuresCells <- c("contNormal", "contGamma", "contcor1")
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2", "Level 3"), name="RM Factor 1")
  )
  options$modelTerms <- list(
    list(components="RM Factor 1", isNuisance=FALSE)
  )
  options$postHocTestsNullControl <- TRUE
  options$postHocTestsVariables <- "RM Factor 1"
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  table <- results[["results"]][["collectionPosthoc"]][["collection"]][["collectionPosthoc_postHoc_RM Factor 1"]][["data"]]
  expect_equal_tables(table,
    list("Level 1", "Level 2", 142887114837413104, 83932041568197648, 0.587401051968199,
         2.78226779130567e-20, "Level 1", "Level 3", 0.521697047188026,
         0.30644539432695, 0.587401051968199, 2.74342774324035e-05, "Level 2",
         "Level 3", 82922847973592.7, 48708968131887.4, 0.587401051968199,
         1.07201998003854e-19)
  )
})

test_that("Analysis handles errors", {
  # NOTE: only errors that are not handled in test-anovabayesian or test-ancovabayesian are tested
  
  options <- initOpts()
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2"), name="RM Factor 1")
  )

  options$repeatedMeasuresCells <- c("contNormal", "debInf")
  options$modelTerms <- list(list(components="RM Factor 1", isNuisance=FALSE))
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Inf RM factor check")
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="Inf RM factor check")

  # options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  # options$covariates <- "debInf"
  # options$modelTerms <- list(
  #   list(components="RM Factor 1", isNuisance=FALSE),
  #   list(components="debInf", isNuisance=FALSE)
  # )
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                 label="Inf covariate check")

  # options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  # options$betweenSubjectFactors <- "debSame"
  # options$covariates <- list()
  # options$modelTerms <- list(
  #   list(components="RM Factor 1", isNuisance=FALSE),
  #   list(components="debSame", isNuisance=FALSE)
  # )
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="1-level factor check")

  # options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  # options$betweenSubjectFactors <- list()
  # options$modelTerms <- list(list(components="RM Factor 1", isNuisance=TRUE))
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="All nuisance check")

  # options$repeatedMeasuresCells <- c("contNormal", "debSame")
  # options$modelTerms <- list(list(components="RM Factor 1", isNuisance=FALSE))
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="No variance check")

  # options$repeatedMeasuresCells <- c("contNormal", "debMiss99")
  # options$modelTerms <- list(list(components="RM Factor 1", isNuisance=FALSE))
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                 label="Too few obs check")
})
