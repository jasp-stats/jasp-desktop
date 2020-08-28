context("Bayesian Repeated Measures ANOVA")

# does not test
# - descriptives table (code from regular ANOVA)
# - descriptives plot (code from regular ANOVA)
# - bftype (01, 10)

initOpts <- function() {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasuresBayesian")
  options$sampleModeNumAcc <- "manual"
  options$fixedNumAcc <- 50
  return(options)
}

test_that("Main table and Effects table results match", {
  set.seed(0)
  options <- initOpts()
  options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2"), name="RM_FACTOR_1")
  )
  options$betweenSubjectFactors <- "facGender"
  options$covariates <- "contcor1"
  options$modelTerms <- list(
    list(components="RM_FACTOR_1", isNuisance=FALSE),
    list(components="facGender", isNuisance=FALSE),
    list(components="contcor1", isNuisance=FALSE),
    list(components=c("RM_FACTOR_1", "facGender"), isNuisance=FALSE)
  )
  options$priorCovariates <- 0.3
  options$priorFixedEffects <- 0.8
  options$priorRandomEffects <- 0.8

  options$effects <- TRUE

  refTablesModelComparison <- list(
    nullModelTop = list(1, 1.2015680635829e-23, "Null model (incl. subject)", 0.1, 1.33507562620322e-24,
                        "", 3.47351082631416e+23, 7.78290269354812, "RM_FACTOR_1 + facGender",
                        0.1, 0.463739964156505, 9.48562551500833, 2.25207823390771e+23,
                        3.86945112370337, "RM_FACTOR_1", 0.1, 0.300669475839298, 39.106368292494,
                        6.71141299730886e+22, 0.885790984668, "RM_FACTOR_1 + contcor1",
                        0.1, 0.0896024391009056, 12.887989220175, 5.74376683392463e+22,
                        0.74747150859863, "RM_FACTOR_1 + facGender + contcor1", 0.1,
                        0.0766836310256722, 15.7870140533192, 4.50189687402267e+22,
                        0.575524732870686, "RM_FACTOR_1 + facGender + RM_FACTOR_1<unicode><unicode><unicode>facGender",
                        0.1, 0.0601037278818813, 12.5735601755857, 6.89156615187532e+21,
                        0.0835758191825203, "RM_FACTOR_1 + facGender + contcor1 + RM_FACTOR_1<unicode><unicode><unicode>facGender",
                        0.1, 0.00920076199573587, 11.0628049253349, 0.41104359340273,
                        4.93896854573074e-24, "facGender", 0.1, 5.4877428285897e-25,
                        7.7632364395313, 0.23874600738579, 2.86869577782693e-24, "contcor1",
                        0.1, 3.18743975314103e-25, 16.2455760987563, 0.0921852605167578,
                        1.10766864970006e-24, "facGender + contcor1", 0.1, 1.23074294411118e-25,
                        13.7488070754201),
    bestModelTop = list(1, 6.76784490026738, "RM_FACTOR_1 + facGender", 0.1, 0.429218129875987,
                        "", 0.812461183408298, 4.81900629644283, "RM_FACTOR_1", 0.1,
                        0.348723069739341, 11.0954981789983, 0.23781134935476, 1.02308586890581,
                        "RM_FACTOR_1 + facGender + contcor1", 0.1, 0.102072942633335,
                        32.3366579475681, 0.192584660287367, 0.810984063552273, "RM_FACTOR_1 + contcor1",
                        0.1, 0.0826608277313462, 19.6531747475726, 0.0565717639157407,
                        0.223973070906526, "RM_FACTOR_1 + facGender + RM_FACTOR_1<unicode><unicode><unicode>facGender",
                        0.1, 0.0242816267117001, 57.5970296697693, 0.0303887520130162,
                        0.118942038756438, "RM_FACTOR_1 + facGender + contcor1 + RM_FACTOR_1<unicode><unicode><unicode>facGender",
                        0.1, 0.013043403308292, 18.7536418861197, 2.8910010651933e-24,
                        1.11678306360458e-23, "Null model (incl. subject)", 0.1, 1.24087007067176e-24,
                        10.0789044176123, 1.2418397001326e-24, 4.79718102327006e-24,
                        "facGender", 0.1, 5.33020113696672e-25, 11.1219692156722, 5.61029992468607e-25,
                        2.16723819754544e-24, "contcor1", 0.1, 2.40804244171715e-25,
                        12.5004991737756, 2.26151580646109e-25, 8.73615226620796e-25,
                        "facGender + contcor1", 0.1, 9.70683585134215e-26, 15.8888961021958)
  )

  refTablesEffects <- list(
    allModels = list(193703209779376, "RM_FACTOR_1", 0.4, 3.44169137633798e-15, 0.6,
                     0.999999999999997, 1.04154401781854, "facGender", 0.4, 0.390271921796097,
                     0.6, 0.609728078203903, 0.212836911165513, "contcor1", 0.5,
                     0.824513164790655, 0.5, 0.175486835209345, 0.297861074572698,
                     "RM_FACTOR_1<unicode><unicode><unicode>facGender", 0.8, 0.930695508904435,
                     0.2, 0.0693044910955647),
    matchedModels = list(4.55863198189131e+23, "RM_FACTOR_1", 0.4, 2.11176285474782e-24,
                         0.4, 0.962674968782349, 1.23159682956966, "facGender", 0.4,
                         0.431383911299063, 0.4, 0.531291057483286, 0.246536467297555,
                         "contcor1", 0.5, 0.80222281997731, 0.5, 0.197777180022688, 0.0702534527768218,
                         "RM_FACTOR_1<unicode><unicode><unicode>facGender", 0.2, 0.531291057483286,
                         0.2, 0.0373250312176498)
  )

  orders       <- c("nullModelTop", "bestModelTop")
  effectsTypes <- c("allModels", "matchedModels")

  for (i in 1:2) {

    order <- orders[i]
    effectsType <- effectsTypes[i]

    options$bayesFactorOrder <- order
    options$effectsType      <- effectsType
    results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)

    table <- results[["results"]][["tableModelComparison"]][["data"]]
    expect_equal_tables(table, refTablesModelComparison[[order]], label=paste("Table with order", order))

    table <- results[["results"]][["tableEffects"]][["data"]]
    expect_equal_tables(table, refTablesEffects[[effectsType]], label=paste("Table with effectsType", effectsType))
  }
})

test_that("Post-hoc Comparisons table results match", {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasuresBayesian")
  options$repeatedMeasuresCells <- c("contNormal", "contGamma", "contcor1")
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2", "Level 3"), name="RM_FACTOR_1")
  )
  options$modelTerms <- list(
    list(components="RM_FACTOR_1", isNuisance=FALSE)
  )
  options$postHocTestsNullControl <- TRUE
  options$postHocTestsVariables <- "RM_FACTOR_1"
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  table <- results[["results"]][["collectionPosthoc"]][["collection"]][["collectionPosthoc_postHoc_RM_FACTOR_1"]][["data"]]
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
    list(levels=c("Level 1", "Level 2"), name="RM_FACTOR_1")
  )

  options$repeatedMeasuresCells <- c("contNormal", "debInf")
  options$modelTerms <- list(list(components="RM_FACTOR_1", isNuisance=FALSE))
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  expect_true(results[["results"]][["error"]], label = "Inf RM factor check")
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="Inf RM factor check")

  # options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  # options$covariates <- "debInf"
  # options$modelTerms <- list(
  #   list(components="RM_FACTOR_1", isNuisance=FALSE),
  #   list(components="debInf", isNuisance=FALSE)
  # )
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                 label="Inf covariate check")

  # options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  # options$betweenSubjectFactors <- "debSame"
  # options$covariates <- list()
  # options$modelTerms <- list(
  #   list(components="RM_FACTOR_1", isNuisance=FALSE),
  #   list(components="debSame", isNuisance=FALSE)
  # )
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="1-level factor check")

  # options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  # options$betweenSubjectFactors <- list()
  # options$modelTerms <- list(list(components="RM_FACTOR_1", isNuisance=TRUE))
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="All nuisance check")

  # options$repeatedMeasuresCells <- c("contNormal", "debSame")
  # options$modelTerms <- list(list(components="RM_FACTOR_1", isNuisance=FALSE))
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="No variance check")

  # options$repeatedMeasuresCells <- c("contNormal", "debMiss99")
  # options$modelTerms <- list(list(components="RM_FACTOR_1", isNuisance=FALSE))
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                 label="Too few obs check")
})

test_that("Analysis fails gracefully if some models error", {

  options <- initOpts()
  options$covariates = list("contNormal")
  options$betweenSubjectFactors = list("contBinom")
  options$effects <- TRUE
  options$modelTerms = list(list(components = list("RM_FACTOR_1"), isNuisance = FALSE),
                            list(components = list("contBinom"), isNuisance = FALSE),
                            list(components = list("contNormal"), isNuisance = FALSE),
                            list(components = list("RM_FACTOR_1", "contBinom"), isNuisance = FALSE))
  options$repeatedMeasuresCells = list("contcor1", "contcor2")
  options$repeatedMeasuresFactors = list(list(levels = list("Level 1", "Level 2"), name = "RM_FACTOR_1"))

  # NOTE: the option below makes BayesFactor return NaN as BF for models with covariates. 
  # It's a nice hack to test how gracefully the analysis recovers when some but not all BFs could be computed.
  # A user can never enter NULL here. This hack exists for BayesFactor version 0.9.12.4.2.
  options$priorCovariates <- NULL

  set.seed(42)
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)

  mainTable <- results[["results"]][["tableModelComparison"]][["data"]]
  effectsTable <- results[["results"]][["tableEffects"]][["data"]]
  
  expect_equal_tables(
    mainTable, 
    list(1, 4.81723072651078, "Null model (incl. subject)", 0.1, 0.546342823039303,
         "", 0.57259147990674, 1.82098643786428, "contBinom", 0.1, 0.3128312455805,
         4.73186300857416, 0.138862578799232, 0.328379305953214, "RM_FACTOR_1",
         0.1, 0.0758665733156897, 7.12638585019384, 0.0858219117860418,
         0.196779369864607, "RM_FACTOR_1 + contBinom", 0.1, 0.046888185563816,
         8.86956002564671, 0.0330766173520163, 0.0736149993547432, "RM_FACTOR_1 + contBinom + RM_FACTOR_1<unicode><unicode><unicode>contBinom",
         0.1, 0.0180711725006913, 17.6010066976745, 1, 1, 1, "NaN", "NaN",
         "contNormal", 0.1, "NaN", "", 1, 1, 1, "NaN", "NaN", "RM_FACTOR_1 + contNormal",
         0.1, "NaN", "", 1, 1, 1, "NaN", "NaN", "contBinom + contNormal",
         0.1, "NaN", "", 1, 1, 1, "NaN", "NaN", "RM_FACTOR_1 + contBinom + contNormal",
         0.1, "NaN", "", 1, 1, 1, "NaN", "NaN", "RM_FACTOR_1 + contBinom + contNormal + RM_FACTOR_1<unicode><unicode><unicode>contBinom",
         0.1, "NaN", ""), 
    label = "Table where some BFs are NaN")
  
  expect_equal_tables(
    effectsTable, 
    list(0.109272332211192, "RM_FACTOR_1", 0.4, 0.859174068619803, 0.6,
         0.140825931380197, 0.404783990575272, "contBinom", 0.4, 0.622209396354992,
         0.6, 0.377790603645008, "NaN", "contNormal", 1, 1, 0, 0, 0.0736149993547432,
         "RM_FACTOR_1<unicode><unicode><unicode>contBinom", 0.8, 0.981928827499309,
         0.2, 0.0180711725006913),
    label = "Table where one inclusion BF is NaN")
  
})

# Single model inference
options <- initOpts()
options$modelTerms <- list(list(components = "RM.Factor.1", isNuisance = FALSE))
options$repeatedMeasuresCells <- c("Stick Insect", "Kangaroo Testicle")
options$repeatedMeasuresFactors <- list(list(levels = c("Level 1", "Level 2"), name = "RM.Factor.1"))
options$singleModelCriTable <- TRUE
options$singleModelEstimates <- TRUE
options$singleModelTerms <- list(list(components = "RM.Factor.1"))
set.seed(1)
results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "Bush Tucker Food.csv", options)

test_that("Single Model Posterior Summary table results match", {
  table <- results[["results"]][["containerSingleModel"]][["collection"]][["containerSingleModel_SMItablePosteriorEstimates"]][["data"]]
  expect_equal_tables(table,
                      list("", 4.58948410870924, 6.16450279524202, 0.745342080929142, 7.70351606555363,
                           "Intercept", "Level 1", 0.732042615149814, 1.73232598619876,
                           0.452456686599393, 2.5434859267573, "RM.Factor.1", "Level 2",
                           -2.5434859267573, -1.73232598619876, 0.452456686599393, -0.732042615149814,
                           ""))
})

test_that("Single Model R² table results match", {
  table <- results[["results"]][["containerSingleModel"]][["collection"]][["containerSingleModel_tableSMICRI"]][["data"]]
  expect_equal_tables(table,
                      list(0.308711456209889, 0.638172359225456, 0.815825511804337, "R<unicode>"
                      ))
})

test_that("Model Comparison table results match", {
  table <- results[["results"]][["tableModelComparison"]][["data"]]
  expect_equal_tables(table,
                      list(1, 75.0836170114885, "RM.Factor.1", 0.5, 0.986856565982542, "",
                           0.0133184846415562, 0.0133184846415562, "Null model (incl. subject)",
                           0.5, 0.0131434340174574, 19.861508543733))
})
