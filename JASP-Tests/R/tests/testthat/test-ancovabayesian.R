context("Bayesian ANCOVA")

# does not test
# - descriptives table (code from regular ANOVA)
# - descriptives plot (code from regular ANOVA)
# - bftype (01, 10)

initOpts <- function() {
  options <- jasptools::analysisOptions("AncovaBayesian")
  options$sampleMode <- "manual"
  options$fixedSamplesNumber <- 50
  return(options)
}

test_that("Main table results match", {
  set.seed(0)
  options <- initOpts()
  options$dependent <- "contNormal"
  options$fixedFactors <- "facGender"
  options$randomFactors <- "facFive"
  options$covariates <- "contGamma"
  options$priorCovariates <- 0.3
  options$priorRandomEffects <- 1.2
  options$modelTerms <- list(
    list(components="facGender", isNuisance=FALSE),
    list(components="facFive", isNuisance=TRUE),
    list(components="contGamma", isNuisance=FALSE)
  )

  refTables <- list(
    nullModelTop = list(2.0810402340829, 3.45565802495949, "facGender", 0.25, 0.535291369462089,
                        2.54299550940088, 1, 1.03889716184925, "Null model (incl. facFive)",
                        0.25, 0.257222979496111, 0.554492327691682, 0.499065271632886,
                        "facGender + contGamma", 0.25, 0.142628168636589, 1.52356717742169,
                        0.252144977607616, 0.208067159341743, "contGamma", 0.25, 0.0648574824052112,
                        1.09719511907165),
    bestModelTop = list(1, 3.1778383618232, "facGender", 0.25, 0.514393251442299, 0.512506394916977,
                        1.07403793058239, "Null model (incl. facFive)", 0.25, 0.263629830866315,
                        1.45815184582002, 0.290516285547582, 0.527086446737985, "facGender + contGamma",
                        0.25, 0.14943961671976, 2.72502064669284, 0.141015265593473,
                        0.234631433849418, "contGamma", 0.25, 0.0725373009716262, 7.06028559007371)
  )

  for (order in c("nullModelTop", "bestModelTop")) {
    options$bayesFactorOrder <- order
    results <- jasptools::run("AncovaBayesian", "test.csv", options)
    table <- results[["results"]][["tableModelComparison"]][["data"]]
    expect_equal_tables(table, refTables[[order]], label=paste("Table with order", order))
  }
})

test_that("Effects table results match", {
  set.seed(0)
  options <- initOpts()
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$fixedFactors <- "contBinom"
  options$effects <- TRUE
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE),
    list(components="contBinom", isNuisance=FALSE),
    list(components=c("contGamma", "contBinom"), isNuisance=FALSE)
  )

  refTables <- list(
    allModels = list(0.194541140104503, "contGamma", 0.6, 0.22589337738806, 0.215301593138521,
                     "contBinom", 0.6, 0.244114899538536, 0.139109048927543, "contGamma<unicode><unicode><unicode>contBinom",
                     0.2, 0.033608452273947),
    matchedModels = list(0.2488203279531, "contGamma", 0.4, 0.192486007853074, 0.278924648376759,
                         "contBinom", 0.4, 0.210695472488687, 0.777915623551311, "contGamma<unicode><unicode><unicode>contBinom",
                         0.2, 0.0339196100618932)
  )

  for (effectsType in c("allModels", "matchedModels")) {
    options$effectsType <- effectsType
    results <- jasptools::run("AncovaBayesian", "test.csv", options)
    table <- results[["results"]][["tableEffects"]][["data"]]
    expect_equal_tables(table, refTables[[effectsType]], label=paste("Table with effects type", effectsType))
  }
})

test_that("Post-hoc Comparisons table results match", {
  options <- jasptools::analysisOptions("AncovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(
    list(components="facFive", isNuisance=FALSE)
  )
  options$postHocTestsNullControl <- TRUE
  options$postHocTestsVariables <- "facFive"
  results <- jasptools::run("AncovaBayesian", "test.csv", options)
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
  # NOTE: only errors that are not handled in test-anovabayesian are tested
 
  options <- initOpts()
  # options$dependent <- "debInf"
  # options$fixedFactors <- "facFive"
  # options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  # results <- jasptools::run("AncovaBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="Inf dependent check")

  options$dependent <- "contNormal"
  options$fixedFactors <- list()
  options$covariates <- "debInf"
  options$modelTerms <- list(list(components="debInf", isNuisance=FALSE))
  results <- jasptools::run("AncovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label="Inf covariate check")

  # options$dependent <- "contNormal"
  # options$fixedFactors <- "debSame"
  # options$covariates <- list()
  # options$modelTerms <- list(list(components="debSame", isNuisance=FALSE))
  # results <- jasptools::run("AncovaBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="1-level factor check")

  # options$dependent <- "contNormal"
  # options$fixedFactors <- "facFive"
  # options$modelTerms <- list(list(components="facFive", isNuisance=TRUE))
  # results <- jasptools::run("AncovaBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="All nuisance check")

  # options$dependent <- "debSame"
  # options$fixedFactors <- "facFive"
  # options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  # results <- jasptools::run("AncovaBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="No variance check")

  # options$dependent <- "debMiss99"
  # options$fixedFactors <- "facFive"
  # options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  # results <- jasptools::run("AncovaBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                 label="Too few obs check")
})
