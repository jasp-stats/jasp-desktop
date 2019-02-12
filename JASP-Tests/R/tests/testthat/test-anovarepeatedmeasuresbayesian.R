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
    nullModelTop = list("Null model (incl. subject)", 0.1, -23.8744941250848, -22.9202516156455,
                        0, "", "RM Factor 1", 0.1, -0.52191065188766, 0.587649376228934,
                        23.3525834731972, 39.1063682924946, "facGender", 0.1, -24.2606062414806,
                        -23.3063637320413, -0.386112116395769, 7.76323643953116, "RM Factor 1 + facGender",
                        0.1, -0.333725484709657, 0.891141584222282, 23.5407686403752,
                        9.48562764001249, "RM Factor 1 + facGender + RM Factor 1<unicode><unicode><unicode>facGender",
                        0.1, -1.22109858280499, -0.239935999954474, 22.6533955422798,
                        12.5735651534712, "contcor1", 0.1, -24.4965580075927, -23.5423154981534,
                        -0.622063882507853, 16.2455760987562, "RM Factor 1 + contcor1",
                        0.1, -1.04768016045519, -0.0526687357727608, 22.8268139646297,
                        12.8879892201747, "facGender + contcor1", 0.1, -24.9098326377401,
                        -23.9555901283008, -1.03533851265525, 13.7488070754196, "RM Factor 1 + facGender + contcor1",
                        0.1, -1.11529732351552, -0.126405348442144, 22.7591968015693,
                        15.7870140533182, "RM Factor 1 + facGender + RM Factor 1<unicode><unicode><unicode>facGender + contcor1",
                        0.1, -2.03617619580061, -1.0779193502007, 21.8383179292842,
                        11.0628049253349),
    bestModelTop = list("RM Factor 1 + facGender", 0.1, -0.367321960540044, 0.830450364907295,
                        0, "", "RM Factor 1", 0.1, -0.457519307040247, 0.682957515126908,
                        -0.0901973465002028, 11.0954992966344, "RM Factor 1 + facGender + contcor1",
                        0.1, -0.991089350964165, 0.00991210164236568, -0.623767390424122,
                        32.3366583310552, "RM Factor 1 + contcor1", 0.1, -1.08270023626127,
                        -0.0909876647379188, -0.715378275721224, 19.6531753785511, "RM Factor 1 + facGender + RM Factor 1<unicode><unicode><unicode>facGender",
                        0.1, -1.61472220777918, -0.649804181052933, -1.24740024723913,
                        57.5970298850713, "RM Factor 1 + facGender + RM Factor 1<unicode><unicode><unicode>facGender + contcor1",
                        0.1, -1.88460906299348, -0.924664607527325, -1.51728710245343,
                        18.7536425473633, "Null model (incl. subject)", 0.1, -23.9062736764081,
                        -22.9520311669687, -23.538951715868, 10.0789056479773, "facGender",
                        0.1, -24.2732563884872, -23.3190138790478, -23.9059344279471,
                        11.1219703306485, "contcor1", 0.1, -24.6183358489862, -23.6640933395468,
                        -24.2510138884461, 12.5005001657945, "facGender + contcor1",
                        0.1, -25.0129223005788, -24.0586797911395, -24.6456003400388,
                        15.8888968826608)
  )

  for (order in c("nullModelTop", "bestModelTop")) {
    options$bayesFactorOrder <- order
    results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
    table <- results[["results"]][["model comparison"]][["data"]]
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
    allModels = list("RM Factor 1", 0.6, 1, "<unicode>", "facGender", 0.6, 0.757389476581774,
                     2.08122183080559, "RM Factor 1<unicode><unicode><unicode>facGender",
                     0.2, 0.0790846685097528, 0.343504623304625),
    matchedModels = list("RM Factor 1", 0.4, 0.920915331490247, 1.02806370019033e+24, "facGender",
                         0.4, 0.678304808072022, 2.79585896982186, "RM Factor 1<unicode><unicode><unicode>facGender",
                         0.2, 0.0790846685097528, 0.1165916378133)
  )

  effectsTypes <- c("allModels", "matchedModels")
  for (effectsType in effectsTypes) {
    options$effectsType <- effectsType
    set.seed(5) # setting seed at start gives aberrant behaviour
    results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
    table <- results[["results"]][["effects"]][["data"]]
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
  table <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table,
    list("Level 1", "Level 2", 0.587401051968199, 83932041568197648, 17.1549930670883,
         2.78226779130567e-20, "TRUE", "Level 1", "Level 3", 0.587401051968199,
         0.30644539432695, -0.282581621383003, 2.74342774324035e-05,
         "FALSE", "Level 2", "Level 3", 0.587401051968199, 48708968131887.4,
         13.9186742094611, 1.07201998003854e-19, "FALSE")
  )
})

test_that("Analysis handles errors", {
  options <- initOpts()
  options$repeatedMeasuresFactors <- list(
    list(levels=c("Level 1", "Level 2"), name="RM Factor 1")
  )

  options$repeatedMeasuresCells <- c("contNormal", "debInf")
  options$modelTerms <- list(list(components="RM Factor 1", isNuisance=FALSE))
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                   label="Inf RM factor check")

  options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  options$covariates <- "debInf"
  options$modelTerms <- list(
    list(components="RM Factor 1", isNuisance=FALSE),
    list(components="debInf", isNuisance=FALSE)
  )
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                  label="Inf covariate check")

  options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  options$betweenSubjectFactors <- "debSame"
  options$covariates <- list()
  options$modelTerms <- list(
    list(components="RM Factor 1", isNuisance=FALSE),
    list(components="debSame", isNuisance=FALSE)
  )
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                   label="1-level factor check")

  options$repeatedMeasuresCells <- c("contNormal", "contGamma")
  options$betweenSubjectFactors <- list()
  options$modelTerms <- list(list(components="RM Factor 1", isNuisance=TRUE))
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                   label="All nuisance check")

  # options$repeatedMeasuresCells <- c("contNormal", "debSame")
  # options$modelTerms <- list(list(components="RM Factor 1", isNuisance=FALSE))
  # results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="No variance check")

  options$repeatedMeasuresCells <- c("contNormal", "debMiss99")
  options$modelTerms <- list(list(components="RM Factor 1", isNuisance=FALSE))
  results <- jasptools::run("AnovaRepeatedMeasuresBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                  label="Too few obs check")
})
