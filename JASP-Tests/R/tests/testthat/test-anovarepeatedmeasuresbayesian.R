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
  options$priorCovariatesEffects <- 0.3
  options$priorFixedEffects <- 0.8
  options$priorRandomEffects <- 0.8

  refTables <- list(
    nullModelTop = list(1, 1.2015680635829e-23, "Null model (incl. subject)", 0.1, 1.33507562620322e-24, "",
                        3.47351082631419e+23, 7.78290269354818, "RM Factor 1 + facGender",
                        0.1, 0.463739964156508, 9.48562551500826, 2.25207823390771e+23,
                        3.86945112370334, "RM Factor 1", 0.1, 0.300669475839298, 39.106368292494,
                        6.71141299730886e+22, 0.885790984667994, "RM Factor 1 + contcor1",
                        0.1, 0.0896024391009056, 12.887989220175, 5.74376683392463e+22,
                        0.74747150859863, "RM Factor 1 + facGender + contcor1", 0.1,
                        0.0766836310256722, 15.7870140533192, 4.50189687402267e+22,
                        0.575524732870682, "RM Factor 1 + facGender + RM Factor 1<unicode><unicode><unicode>facGender",
                        0.1, 0.0601037278818813, 12.5735601755857, 6.89156615187532e+21,
                        0.0835758191825197, "RM Factor 1 + facGender + contcor1 + RM Factor 1<unicode><unicode><unicode>facGender",
                        0.1, 0.00920076199573587, 11.0628049253349, 
                        0.41104359340273, 4.93896854573074e-24, "facGender", 0.1, 5.4877428285897e-25,
                        7.7632364395313, 0.23874600738579, 2.86869577782693e-24, "contcor1",
                        0.1, 3.18743975314103e-25, 16.2455760987563, 0.0921852605167578,
                        1.10766864970006e-24, "facGender + contcor1", 0.1, 1.23074294411118e-25,
                        13.7488070754201),
    bestModelTop = list(1, 6.76784490026738, "RM Factor 1 + facGender", 0.1, 0.429218129875987,
                        "", 0.812461183408298, 4.81900629644283, "RM Factor 1", 0.1,
                        0.348723069739341, 11.0954981789983, 0.23781134935476, 1.02308586890581,
                        "RM Factor 1 + facGender + contcor1", 0.1, 0.102072942633335,
                        32.3366579475681, 0.192584660287367, 0.810984063552273, "RM Factor 1 + contcor1",
                        0.1, 0.0826608277313462, 19.6531747475726, 0.0565717639157407,
                        0.223973070906526, "RM Factor 1 + facGender + RM Factor 1<unicode><unicode><unicode>facGender",
                        0.1, 0.0242816267117001, 57.5970296697693, 0.0303887520130162,
                        0.118942038756438, "RM Factor 1 + facGender + contcor1 + RM Factor 1<unicode><unicode><unicode>facGender",
                        0.1, 0.013043403308292, 18.7536418861197, 2.8910010651933e-24,
                        1.11678306360458e-23, "Null model (incl. subject)", 0.1, 1.24087007067176e-24,
                        10.0789044176123, 1.2418397001326e-24, 4.79718102327006e-24,
                        "facGender", 0.1, 5.33020113696672e-25, 11.1219692156722, 5.61029992468607e-25,
                        2.16723819754544e-24, "contcor1", 0.1, 2.40804244171715e-25,
                        12.5004991737756, 2.26151580646109e-25, 8.73615226620796e-25,
                        "facGender + contcor1", 0.1, 9.70683585134215e-26, 15.8888961021958)
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
    allModels = list(1000799917193443, "RM Factor 1", 0.6, 0.999999999999999, 2.08122183080565,
                     "facGender", 0.6, 0.757389476581779, 0.343504623304608, "RM Factor 1<unicode><unicode><unicode>facGender",
                     0.2, 0.0790846685097493),
    matchedModels = list(1.02806369879075e+24, "RM Factor 1", 0.4, 0.92091533149025, 2.79585896982196,
                         "facGender", 0.4, 0.67830480807203, 0.116591637813293, "RM Factor 1<unicode><unicode><unicode>facGender",
                         0.2, 0.0790846685097493)
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
