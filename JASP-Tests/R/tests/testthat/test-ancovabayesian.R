context("Bayesian ANCOVA")

# does not test
# - descriptives table (code from regular ANOVA)
# - descriptives plot (code from regular ANOVA)
# - bftype (01, 10)

initOpts <- function() {
  options <- jasptools::analysisOptions("AncovaBayesian")
  options$sampleModeNumAcc <- "manual"
  options$fixedNumAcc <- 50
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
    nullModelTop = list(1, 1.15069866070698, "Null model (incl. facFive)",
                        0.25, 0.277230113474675, "", 1.83701195521646, 3.11340404608183, "facGender", 0.25, 0.509275032798994,
                        29.6656710072444, 0.502295526791326, 0.485338413495893,
                        "facGender + contGamma", 0.25, 0.139251445890181, 9.83211717425022,
                        0.267804268827858, 0.24059264108273, "contGamma", 0.25, 0.0742434078361496,
                        15.885810482332),
    bestModelTop = list(1, 3.52818212286609, "facGender", 0.25, 0.540453997217391, "",
                        0.485506904639836, 1.0672128467728, "Null model (incl. facFive)",
                        0.25, 0.262394147289242, 10.5414217188718, 0.230284849014787,
                        0.426450425012883, "facGender + contGamma", 0.25, 0.124458367148645,
                        14.932174588461, 0.134504488298719, 0.235176246789085, "contGamma",
                        0.25, 0.0726934883447225, 15.235554204653)
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
    allModels = list(0.195147508378382, "contGamma", 0.4, 0.773561964946583, 0.6, 0.226438035053417,
                     0.215922578651091, "contBinom", 0.4, 0.755353263370717, 0.6,
                     0.244646736629283, 0.148059370442441, "contGamma<unicode><unicode><unicode>contBinom",
                     0.8, 0.964306352146872, 0.2, 0.0356936478531281),
    matchedModels = list(0.251463756140853, "contGamma", 0.4, 0.770552715646492, 0.4, 0.193766080181001,
                         0.281631799589681, "contBinom", 0.4, 0.752414848114898, 0.4,
                         0.211903947712595, 0.784742840790344, "contGamma<unicode><unicode><unicode>contBinom",
                         0.2, 0.0454686584162663, 0.2, 0.0356812041725066)
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

  options$dependent <- "contNormal"
  options$fixedFactors <- list()
  options$covariates <- "debInf"
  options$modelTerms <- list(list(components="debInf", isNuisance=FALSE))
  results <- jasptools::run("AncovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label="Inf covariate check")

  options$dependent <- "contNormal"
  options$covariates <- c("debEqual1", "debEqual2")
  options$modelTerms <- list(list(components="debEqual1", isNuisance=FALSE),
                             list(components="debEqual2", isNuisance=FALSE))
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Identical covariates check")
})


options <- initOpts()
options$covariates <- "contcor1"
options$dependent <- "contNormal"
options$fixedFactors <- "facGender"
options$modelTerms <- list(list(components = "facGender", isNuisance = FALSE), list(components = "contcor1", isNuisance = FALSE))
options$plotCredibleInterval <- TRUE
options$plotHorizontalAxis <- "contcor1"
options$plotSeparateLines <- "facGender"
options$singleModelTerms <- list(list(components = "facGender"), list(components = "contcor1"))
set.seed(1)
results <- jasptools::run("AncovaBayesian", "debug.csv", options)


test_that("contcor1 - contNormal plot matches", {
  plotName <- results[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_containerDescriptivesPlots"]][["collection"]][["descriptivesContainer_containerDescriptivesPlots_contcor1 - contNormal"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "contcor1-contnormal", dir="AncovaBayesian")
})

test_that("Model Comparison table results match", {
  table <- results[["results"]][["tableModelComparison"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1.79472415330685, "facGender", 0.25, 0.374312284903617, "",
                           0.798367847232719, 1.27861723994693, "facGender + contcor1",
                           0.25, 0.298838893091261, 12.1608691349882, 0.523794362714111,
                           0.731634131635191, "Null model", 0.25, 0.196062664727153, 2.03265957838496e-05,
                           0.349403860233028, 0.451394642548716, "contcor1", 0.25, 0.130786157277969,
                           0.0026137271628805))
})
